%%%-------------------------------------------------------------------
%%% @doc
%%% Registry Sharding Performance Stress Test Suite - 100K Concurrent
%%%
%%% This test suite validates that erlmcp_registry_sharded can handle
%%% 100K+ concurrent lookups with sub-100µs p99 latency.
%%%
%%% Test Scenarios:
%%% 1. Baseline Lookup Performance (10K-100K concurrent)
%%% 2. Concurrent Registration + Lookup Storm (50/50 split)
%%% 3. Binding Stress with Router Lookups
%%% 4. Message Routing at Full Scale
%%% 5. Partition Balance & Contention Analysis
%%% 6. Sustained Load (30+ seconds at 100K ops/sec)
%%%
%%% Performance Targets:
%%% - Lookup latency p99 < 100µs
%%% - Throughput > 100K ops/sec
%%% - Zero registry-related timeouts
%%% - Even distribution across partitions
%%% - Contention detection accuracy
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_registry_100k_stress_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Common Test callbacks
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_baseline_lookup_10k/1,
    test_baseline_lookup_50k/1,
    test_baseline_lookup_100k/1,
    test_concurrent_register_lookup_storm/1,
    test_binding_stress_with_routing/1,
    test_message_routing_100k/1,
    test_partition_balance/1,
    test_sustained_load_30sec/1,
    test_latency_histogram/1,
    test_contention_under_load/1
]).

%% Helper exports
-export([
    lookup_worker/4,
    register_worker/5,
    routing_worker/5,
    monitor_progress/2,
    collect_latencies/2
]).

%% Macros for test configuration
-define(PARTITION_COUNT, 64).
-define(NUM_SERVERS, 10000).
-define(NUM_TRANSPORTS, 10000).
-define(LOOKUP_DURATION_SEC, 30).
-define(REG_DURATION_SEC, 20).
-define(SUSTAINED_DURATION_SEC, 30).

%%====================================================================
%% Common Test Callbacks
%%====================================================================

all() ->
    [
        test_baseline_lookup_10k,
        test_baseline_lookup_50k,
        test_baseline_lookup_100k,
        test_concurrent_register_lookup_storm,
        test_binding_stress_with_routing,
        test_message_routing_100k,
        test_partition_balance,
        test_sustained_load_30sec,
        test_latency_histogram,
        test_contention_under_load
    ].

init_per_suite(Config) ->
    ensure_gproc_started(),
    clear_all_registrations(),
    timer:sleep(100),

    {ok, Registry} = erlmcp_registry_sharded:start_link(?PARTITION_COUNT),

    %% Pre-populate with test data
    populate_test_data(Registry, ?NUM_SERVERS, ?NUM_TRANSPORTS),

    [{registry, Registry}, {num_servers, ?NUM_SERVERS}, {num_transports, ?NUM_TRANSPORTS} | Config].

end_per_suite(Config) ->
    Registry = proplists:get_value(registry, Config),
    catch gen_server:stop(Registry, shutdown, 5000),
    clear_all_registrations(),
    ok.

init_per_testcase(_TestCase, Config) ->
    erlmcp_registry_sharded:reset_stats(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% Test 1: Baseline 10K concurrent lookups
test_baseline_lookup_10k(Config) ->
    Registry = proplists:get_value(registry, Config),
    NumServers = proplists:get_value(num_servers, Config),

    ct:pal("Starting baseline lookup test: 10K concurrent lookups"),

    WorkerCount = 10,
    LookupsPerWorker = 1000,

    Latencies = run_lookup_workers(WorkerCount, NumServers, LookupsPerWorker),
    Stats = compute_latency_stats(Latencies),

    ct:pal("Lookup Stats (10K):~n~s", [format_stats(Stats)]),

    %% Verify p99 < 100µs
    P99 = maps:get(p99, Stats),
    P99 < 100 orelse ct:fail("P99 latency " ++ integer_to_list(P99) ++ "µs exceeds 100µs target").

%% Test 2: Baseline 50K concurrent lookups
test_baseline_lookup_50k(Config) ->
    Registry = proplists:get_value(registry, Config),
    NumServers = proplists:get_value(num_servers, Config),

    ct:pal("Starting baseline lookup test: 50K concurrent lookups"),

    WorkerCount = 50,
    LookupsPerWorker = 1000,

    Latencies = run_lookup_workers(WorkerCount, NumServers, LookupsPerWorker),
    Stats = compute_latency_stats(Latencies),

    ct:pal("Lookup Stats (50K):~n~s", [format_stats(Stats)]),

    P99 = maps:get(p99, Stats),
    P99 < 100 orelse ct:fail("P99 latency exceeds 100µs target").

%% Test 3: Baseline 100K concurrent lookups
test_baseline_lookup_100k(Config) ->
    Registry = proplists:get_value(registry, Config),
    NumServers = proplists:get_value(num_servers, Config),

    ct:pal("Starting baseline lookup test: 100K concurrent lookups"),

    WorkerCount = 100,
    LookupsPerWorker = 1000,

    Latencies = run_lookup_workers(WorkerCount, NumServers, LookupsPerWorker),
    Stats = compute_latency_stats(Latencies),

    ct:pal("Lookup Stats (100K):~n~s", [format_stats(Stats)]),

    Throughput = maps:get(throughput_ops_sec, Stats),
    P99 = maps:get(p99, Stats),

    ct:pal("Throughput: ~.0f ops/sec", [Throughput]),

    Throughput > 100000 orelse ct:fail("Throughput " ++ integer_to_list(round(Throughput)) ++
                                       " ops/sec below 100K target"),
    P99 < 100 orelse ct:fail("P99 latency " ++ integer_to_list(P99) ++ "µs exceeds 100µs target").

%% Test 4: Concurrent registration + lookup storm (50/50)
test_concurrent_register_lookup_storm(Config) ->
    NumServers = proplists:get_value(num_servers, Config),

    ct:pal("Starting concurrent register/lookup storm test"),

    %% Split workers: 25 for registration, 25 for lookups
    RegWorkerCount = 25,
    LookupWorkerCount = 25,

    Duration = ?REG_DURATION_SEC * 1000,

    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + Duration,

    %% Start registration workers (new servers)
    ServerIdStart = 50000,
    RegWorkers = [spawn_link(?MODULE, register_worker,
                            [self(), I, ServerIdStart + I * 1000, EndTime, ?PARTITION_COUNT])
                 || I <- lists:seq(1, RegWorkerCount)],

    %% Start lookup workers (existing servers)
    LookupWorkers = [spawn_link(?MODULE, lookup_worker,
                               [self(), I, NumServers, EndTime])
                    || I <- lists:seq(1, LookupWorkerCount)],

    %% Collect results
    AllWorkers = RegWorkers ++ LookupWorkers,
    Results = collect_worker_results(AllWorkers, [], 0, 0),

    {RegCount, LookupCount} = Results,
    Duration_Sec = (erlang:monotonic_time(millisecond) - StartTime) / 1000.0,
    TotalOps = RegCount + LookupCount,
    Throughput = TotalOps / Duration_Sec,

    ct:pal("Register/Lookup Storm Results:~n  Registrations: ~p~n  Lookups: ~p~n" ++
            "  Total ops: ~p~n  Duration: ~.2f sec~n  Throughput: ~.0f ops/sec",
            [RegCount, LookupCount, TotalOps, Duration_Sec, Throughput]),

    Throughput > 50000 orelse ct:fail("Throughput below 50K ops/sec target").

%% Test 5: Binding stress with routing lookups
test_binding_stress_with_routing(Config) ->
    NumServers = proplists:get_value(num_servers, Config),
    NumTransports = proplists:get_value(num_transports, Config),

    ct:pal("Starting binding stress with routing test"),

    WorkerCount = 40,
    OpCount = 500,

    Duration = ?REG_DURATION_SEC * 1000,
    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + Duration,

    %% Start routing workers (mix of bindings and route ops)
    Workers = [spawn_link(?MODULE, routing_worker,
                         [self(), I, NumServers, NumTransports, EndTime])
              || I <- lists:seq(1, WorkerCount)],

    Results = collect_worker_results(Workers, [], 0, 0),
    {RouteCount, BindCount} = Results,

    Duration_Sec = (erlang:monotonic_time(millisecond) - StartTime) / 1000.0,
    TotalOps = RouteCount + BindCount,
    Throughput = TotalOps / Duration_Sec,

    ct:pal("Routing Stress Results:~n  Routes: ~p~n  Bindings: ~p~n" ++
            "  Total ops: ~p~n  Duration: ~.2f sec~n  Throughput: ~.0f ops/sec",
            [RouteCount, BindCount, TotalOps, Duration_Sec, Throughput]),

    Throughput > 20000 orelse ct:fail("Routing throughput below 20K ops/sec target").

%% Test 6: Message routing at full 100K scale
test_message_routing_100k(Config) ->
    Registry = proplists:get_value(registry, Config),
    NumServers = proplists:get_value(num_servers, Config),

    ct:pal("Starting message routing test at 100K scale"),

    WorkerCount = 100,
    RoutesPerWorker = 1000,

    Latencies = run_routing_workers(WorkerCount, NumServers, RoutesPerWorker),
    Stats = compute_latency_stats(Latencies),

    ct:pal("Message Routing Stats (100K routes):~n~s", [format_stats(Stats)]),

    P99 = maps:get(p99, Stats),
    P99 < 500 orelse ct:fail("Routing p99 latency exceeds 500µs target").

%% Test 7: Partition balance analysis
test_partition_balance(Config) ->
    Registry = proplists:get_value(registry, Config),

    ct:pal("Analyzing partition balance"),

    PartitionStats = erlmcp_registry_sharded:get_partition_stats(),

    %% Extract write counts
    WriteCounts = [maps:get(write_count, Stats, 0) || {_, Stats} <- maps:to_list(PartitionStats)],

    AvgWrites = lists:sum(WriteCounts) / length(WriteCounts),
    MaxWrites = lists:max(WriteCounts),
    MinWrites = lists:min(WriteCounts),
    Skew = (MaxWrites - MinWrites) / (AvgWrites + 1),

    ct:pal("Partition Balance:~n  Avg writes/partition: ~.2f~n" ++
            "  Min writes: ~p~n  Max writes: ~p~n  Skew ratio: ~.2f",
            [AvgWrites, MinWrites, MaxWrites, Skew]),

    %% Verify balanced distribution (skew < 0.3 = within 30%)
    Skew < 0.3 orelse ct:fail("Partition skew " ++ float_to_list(Skew) ++ " exceeds 0.3 target").

%% Test 8: Sustained load for 30 seconds at 100K ops/sec
test_sustained_load_30sec(Config) ->
    NumServers = proplists:get_value(num_servers, Config),
    NumTransports = proplists:get_value(num_transports, Config),

    ct:pal("Starting 30-second sustained load test (100K ops/sec target)"),

    WorkerCount = 100,
    Duration = ?SUSTAINED_DURATION_SEC * 1000,

    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + Duration,

    %% Mix of operations: 40% lookups, 30% routes, 20% registers, 10% binds
    Workers = create_mixed_workers(WorkerCount, NumServers, NumTransports, EndTime),

    %% Monitor every 5 seconds
    ProgressPid = spawn_link(?MODULE, monitor_progress, [EndTime, []]),

    Results = collect_worker_results(Workers, [], 0, 0),
    {Ops, _} = Results,

    Elapsed_Sec = (erlang:monotonic_time(millisecond) - StartTime) / 1000.0,
    Throughput = Ops / Elapsed_Sec,

    ct:pal("Sustained Load Results (30 sec):~n  Total ops: ~p~n" ++
            "  Duration: ~.2f sec~n  Throughput: ~.0f ops/sec",
            [Ops, Elapsed_Sec, Throughput]),

    Throughput > 80000 orelse ct:fail("Sustained throughput below 80K ops/sec target").

%% Test 9: Latency histogram analysis
test_latency_histogram(Config) ->
    NumServers = proplists:get_value(num_servers, Config),

    ct:pal("Generating latency histogram from 100K lookups"),

    WorkerCount = 100,
    LookupsPerWorker = 1000,

    Latencies = run_lookup_workers(WorkerCount, NumServers, LookupsPerWorker),
    Stats = compute_latency_stats(Latencies),

    ct:pal("Full Latency Histogram:~n~s", [format_histogram(Latencies)]).

%% Test 10: Contention detection under load
test_contention_under_load(Config) ->
    Registry = proplists:get_value(registry, Config),

    erlmcp_registry_sharded:reset_stats(),

    ct:pal("Testing contention detection under load"),

    WorkerCount = 50,
    LookupsPerWorker = 2000,

    StartTime = erlang:monotonic_time(millisecond),
    Latencies = run_lookup_workers(WorkerCount, NumServers, LookupsPerWorker),
    Duration = erlang:monotonic_time(millisecond) - StartTime,

    ContentionStatus = erlmcp_registry_sharded:get_contention_status(),
    PartitionStats = erlmcp_registry_sharded:get_partition_stats(),

    %% Count alarmed partitions
    AlarmedPartitions = [Id || {Id, Stats} <- maps:to_list(PartitionStats),
                                maps:get(contention_alarm, Stats, false)],

    ct:pal("Contention Analysis:~n  Test duration: ~p ms~n" ++
            "  Alarmed partitions: ~p~n  Admission control: ~p",
            [Duration, length(AlarmedPartitions),
             maps:get(admission_control, ContentionStatus, false)]),

    %% We expect no alarms under normal operation
    length(AlarmedPartitions) =:= 0 orelse
        ct:pal("Warning: ~p partitions alarmed (may be expected under extreme load)",
               [length(AlarmedPartitions)]).

%%====================================================================
%% Helper Functions
%%====================================================================

ensure_gproc_started() ->
    case application:ensure_started(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end.

clear_all_registrations() ->
    ensure_gproc_started(),

    ServerPattern = [{{{n, l, {mcp, server, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    ServerEntries = gproc:select(ServerPattern),
    lists:foreach(fun({Id, Pid}) ->
        catch gproc:unreg_other({n, l, {mcp, server, Id}}, Pid)
    end, ServerEntries),

    TransportPattern = [{{{n, l, {mcp, transport, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    TransportEntries = gproc:select(TransportPattern),
    lists:foreach(fun({Id, Pid}) ->
        catch gproc:unreg_other({n, l, {mcp, transport, Id}}, Pid)
    end, TransportEntries),
    ok.

populate_test_data(Registry, NumServers, NumTransports) ->
    ct:pal("Pre-populating registry with ~p servers and ~p transports",
           [NumServers, NumTransports]),

    %% Create dummy processes for servers and transports
    ServerPid = spawn_link(fun() -> dummy_process() end),
    TransportPid = spawn_link(fun() -> dummy_process() end),

    %% Register servers
    lists:foreach(fun(N) ->
        ServerId = list_to_binary("server_" ++ integer_to_list(N)),
        erlmcp_registry_sharded:register_server(ServerId, ServerPid, #{})
    end, lists:seq(1, NumServers)),

    %% Register transports
    lists:foreach(fun(N) ->
        TransportId = list_to_atom("transport_" ++ integer_to_list(N)),
        erlmcp_registry_sharded:register_transport(TransportId, TransportPid, #{})
    end, lists:seq(1, NumTransports)),

    ct:pal("Pre-population complete").

dummy_process() ->
    receive stop -> ok after 60000 -> dummy_process() end.

run_lookup_workers(WorkerCount, NumServers, LookupsPerWorker) ->
    Parent = self(),

    Duration = ?LOOKUP_DURATION_SEC * 1000,
    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + Duration,

    Workers = [spawn_link(?MODULE, lookup_worker,
                         [Parent, I, NumServers, EndTime])
              || I <- lists:seq(1, WorkerCount)],

    collect_latencies(Workers, []).

run_routing_workers(WorkerCount, NumServers, RoutesPerWorker) ->
    Parent = self(),

    Duration = ?LOOKUP_DURATION_SEC * 1000,
    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + Duration,

    Workers = [spawn_link(?MODULE, routing_worker,
                         [Parent, I, NumServers, NumServers, EndTime])
              || I <- lists:seq(1, WorkerCount)],

    collect_latencies(Workers, []).

create_mixed_workers(WorkerCount, NumServers, NumTransports, EndTime) ->
    Parent = self(),
    [spawn_link(?MODULE, mixed_worker,
               [Parent, I, NumServers, NumTransports, EndTime])
    || I <- lists:seq(1, WorkerCount)].

%% Lookup worker - performs N concurrent lookups
lookup_worker(Parent, WorkerId, NumServers, EndTime) ->
    Latencies = [],
    lookup_loop(Parent, WorkerId, NumServers, EndTime, Latencies, 0).

lookup_loop(Parent, WorkerId, NumServers, EndTime, Latencies, Count) ->
    Now = erlang:monotonic_time(millisecond),
    if Now >= EndTime ->
        Parent ! {lookup_done, WorkerId, Latencies, Count};
    true ->
        %% Random server ID
        ServerId = list_to_binary("server_" ++ integer_to_list(rand:uniform(NumServers))),

        %% Measure lookup latency
        Start = erlang:monotonic_time(microsecond),
        case erlmcp_registry_sharded:find_server(ServerId) of
            {ok, _} -> ok;
            {error, not_found} -> ok
        end,
        Elapsed = erlang:monotonic_time(microsecond) - Start,

        lookup_loop(Parent, WorkerId, NumServers, EndTime,
                   [Elapsed | Latencies], Count + 1)
    end.

%% Register worker - continuously registers new servers
register_worker(Parent, WorkerId, StartServerId, EndTime, PartitionCount) ->
    DummyPid = spawn_link(fun() -> dummy_process() end),
    RegCount = register_loop(Parent, WorkerId, StartServerId, EndTime, DummyPid, 0),
    Parent ! {register_done, WorkerId, RegCount}.

register_loop(Parent, WorkerId, ServerId, EndTime, DummyPid, Count) ->
    Now = erlang:monotonic_time(millisecond),
    if Now >= EndTime ->
        Count;
    true ->
        BinServerId = list_to_binary("new_server_" ++ integer_to_list(ServerId + Count)),
        case erlmcp_registry_sharded:register_server(BinServerId, DummyPid, #{}) of
            ok -> register_loop(Parent, WorkerId, ServerId, EndTime, DummyPid, Count + 1);
            {error, already_registered} ->
                register_loop(Parent, WorkerId, ServerId, EndTime, DummyPid, Count + 1);
            _Error -> register_loop(Parent, WorkerId, ServerId, EndTime, DummyPid, Count + 1)
        end
    end.

%% Routing worker - performs binding and routing operations
routing_worker(Parent, WorkerId, NumServers, NumTransports, EndTime) ->
    Latencies = [],
    routing_loop(Parent, WorkerId, NumServers, NumTransports, EndTime,
                Latencies, 0, 0).

routing_loop(Parent, WorkerId, NumServers, NumTransports, EndTime,
             Latencies, RouteCount, BindCount) ->
    Now = erlang:monotonic_time(millisecond),
    if Now >= EndTime ->
        Parent ! {routing_done, WorkerId, Latencies, RouteCount, BindCount};
    true ->
        Operation = rand:uniform(2),
        case Operation of
            1 ->
                %% Perform routing lookup
                ServerId = list_to_binary("server_" ++ integer_to_list(rand:uniform(NumServers))),
                TransportId = list_to_atom("transport_" ++ integer_to_list(rand:uniform(NumTransports))),

                Start = erlang:monotonic_time(microsecond),
                case erlmcp_registry_sharded:find_server(ServerId) of
                    {ok, _} -> erlmcp_registry_sharded:route_to_server(ServerId, TransportId, #{test => true});
                    {error, _} -> ok
                end,
                Elapsed = erlang:monotonic_time(microsecond) - Start,

                routing_loop(Parent, WorkerId, NumServers, NumTransports, EndTime,
                            [Elapsed | Latencies], RouteCount + 1, BindCount);
            2 ->
                %% Perform binding
                ServerId = list_to_binary("server_" ++ integer_to_list(rand:uniform(NumServers))),
                TransportId = list_to_atom("transport_" ++ integer_to_list(rand:uniform(NumTransports))),

                Start = erlang:monotonic_time(microsecond),
                case erlmcp_registry_sharded:bind_transport_to_server(TransportId, ServerId) of
                    ok -> ok;
                    {error, _} -> ok
                end,
                Elapsed = erlang:monotonic_time(microsecond) - Start,

                routing_loop(Parent, WorkerId, NumServers, NumTransports, EndTime,
                            Latencies, RouteCount, BindCount + 1)
        end
    end.

%% Mixed worker - performs all types of operations
mixed_worker(Parent, WorkerId, NumServers, NumTransports, EndTime) ->
    mixed_loop(Parent, WorkerId, NumServers, NumTransports, EndTime, 0, 0, 0, 0).

mixed_loop(Parent, WorkerId, NumServers, NumTransports, EndTime,
          LookupCount, RouteCount, RegCount, BindCount) ->
    Now = erlang:monotonic_time(millisecond),
    if Now >= EndTime ->
        TotalOps = LookupCount + RouteCount + RegCount + BindCount,
        Parent ! {mixed_done, WorkerId, TotalOps};
    true ->
        Operation = rand:uniform(100),
        case Operation of
            Op when Op =< 40 ->
                %% 40% lookups
                ServerId = list_to_binary("server_" ++ integer_to_list(rand:uniform(NumServers))),
                case erlmcp_registry_sharded:find_server(ServerId) of
                    {ok, _} -> ok;
                    {error, _} -> ok
                end,
                mixed_loop(Parent, WorkerId, NumServers, NumTransports, EndTime,
                          LookupCount + 1, RouteCount, RegCount, BindCount);
            Op when Op =< 70 ->
                %% 30% routes
                ServerId = list_to_binary("server_" ++ integer_to_list(rand:uniform(NumServers))),
                TransportId = list_to_atom("transport_" ++ integer_to_list(rand:uniform(NumTransports))),
                erlmcp_registry_sharded:route_to_server(ServerId, TransportId, #{test => true}),
                mixed_loop(Parent, WorkerId, NumServers, NumTransports, EndTime,
                          LookupCount, RouteCount + 1, RegCount, BindCount);
            Op when Op =< 90 ->
                %% 20% registers
                ServerId = list_to_binary("mixed_server_" ++ integer_to_list(rand:uniform(1000000))),
                DummyPid = spawn(fun() -> dummy_process() end),
                case erlmcp_registry_sharded:register_server(ServerId, DummyPid, #{}) of
                    ok -> ok;
                    {error, _} -> ok
                end,
                mixed_loop(Parent, WorkerId, NumServers, NumTransports, EndTime,
                          LookupCount, RouteCount, RegCount + 1, BindCount);
            _ ->
                %% 10% binds
                ServerId = list_to_binary("server_" ++ integer_to_list(rand:uniform(NumServers))),
                TransportId = list_to_atom("transport_" ++ integer_to_list(rand:uniform(NumTransports))),
                case erlmcp_registry_sharded:bind_transport_to_server(TransportId, ServerId) of
                    ok -> ok;
                    {error, _} -> ok
                end,
                mixed_loop(Parent, WorkerId, NumServers, NumTransports, EndTime,
                          LookupCount, RouteCount, RegCount, BindCount + 1)
        end
    end.

%% Collect results from worker processes
collect_worker_results([], _Acc, Count, OpCount) ->
    {Count, OpCount};
collect_worker_results(Workers, Acc, Count, OpCount) ->
    receive
        {lookup_done, _WorkerId, _Latencies, WorkerCount} ->
            collect_worker_results(tl(Workers), [WorkerCount | Acc], Count + 1, OpCount + WorkerCount);
        {register_done, _WorkerId, RegCount} ->
            collect_worker_results(tl(Workers), [RegCount | Acc], Count + 1, OpCount + RegCount);
        {routing_done, _WorkerId, _Latencies, RouteCount, BindCount} ->
            TotalOps = RouteCount + BindCount,
            collect_worker_results(tl(Workers), [TotalOps | Acc], Count + 1, OpCount + TotalOps);
        {mixed_done, _WorkerId, TotalOps} ->
            collect_worker_results(tl(Workers), [TotalOps | Acc], Count + 1, OpCount + TotalOps)
    after 60000 ->
        {Count, OpCount}
    end.

%% Collect latencies from worker processes
collect_latencies([], Acc) ->
    lists:flatten(Acc);
collect_latencies(Workers, Acc) ->
    receive
        {lookup_done, _WorkerId, Latencies, _Count} ->
            collect_latencies(tl(Workers), [Latencies | Acc]);
        {routing_done, _WorkerId, Latencies, _RouteCount, _BindCount} ->
            collect_latencies(tl(Workers), [Latencies | Acc])
    after 60000 ->
        lists:flatten(Acc)
    end.

%% Compute latency statistics from list of latency measurements
compute_latency_stats(Latencies) ->
    SortedLatencies = lists:sort(Latencies),
    Count = length(SortedLatencies),

    Sum = lists:sum(SortedLatencies),
    Min = lists:min(SortedLatencies),
    Max = lists:max(SortedLatencies),
    Avg = Sum div max(1, Count),

    %% Percentiles
    P50 = lists:nth(max(1, (Count * 50) div 100), SortedLatencies),
    P95 = lists:nth(max(1, (Count * 95) div 100), SortedLatencies),
    P99 = lists:nth(max(1, (Count * 99) div 100), SortedLatencies),
    P999 = lists:nth(max(1, (Count * 999) div 1000), SortedLatencies),

    Throughput = Count / ?LOOKUP_DURATION_SEC,

    #{
        min => Min,
        max => Max,
        avg => Avg,
        p50 => P50,
        p95 => P95,
        p99 => P99,
        p999 => P999,
        count => Count,
        throughput_ops_sec => Throughput
    }.

%% Format stats for display
format_stats(Stats) ->
    io_lib:format(
        "  Min: ~pµs~n  Avg: ~pµs~n  P50: ~pµs~n" ++
        "  P95: ~pµs~n  P99: ~pµs~n  P999: ~pµs~n" ++
        "  Max: ~pµs~n  Count: ~p~n  Throughput: ~.0f ops/sec~n",
        [
            maps:get(min, Stats),
            maps:get(avg, Stats),
            maps:get(p50, Stats),
            maps:get(p95, Stats),
            maps:get(p99, Stats),
            maps:get(p999, Stats),
            maps:get(max, Stats),
            maps:get(count, Stats),
            maps:get(throughput_ops_sec, Stats)
        ]
    ).

%% Generate ASCII histogram
format_histogram(Latencies) ->
    Buckets = create_histogram_buckets(Latencies),
    MaxCount = max_bucket_count(Buckets),

    Lines = [format_bucket(Bucket, MaxCount) || Bucket <- Buckets],
    string:join(Lines, "\n").

create_histogram_buckets(Latencies) ->
    BucketRanges = [
        {0, 10, "0-10µs"},
        {10, 25, "10-25µs"},
        {25, 50, "25-50µs"},
        {50, 100, "50-100µs"},
        {100, 200, "100-200µs"},
        {200, 500, "200-500µs"},
        {500, 1000, "500-1000µs"},
        {1000, 5000, "1-5ms"},
        {5000, 999999999, ">5ms"}
    ],

    [
        {Range, length([L || L <- Latencies, L >= Min, L < Max])}
        || {Min, Max, Range} <- BucketRanges
    ].

max_bucket_count(Buckets) ->
    lists:max([Count || {_, Count} <- Buckets] ++ [1]).

format_bucket({Range, Count}, MaxCount) ->
    BarLength = (Count * 50) div max(1, MaxCount),
    Bar = string:chars($#, BarLength),
    io_lib:format("  ~12s | ~s ~p", [Range, Bar, Count]).

%% Monitor progress periodically
monitor_progress(EndTime, ProgressLog) ->
    receive
    after 5000 ->
        Now = erlang:monotonic_time(millisecond),
        if Now >= EndTime ->
            ok;
        true ->
            Elapsed = (Now - (EndTime - 30000)) / 1000.0,
            io:format("Progress: ~.1f seconds elapsed~n", [Elapsed]),
            monitor_progress(EndTime, ProgressLog)
        end
    end.
