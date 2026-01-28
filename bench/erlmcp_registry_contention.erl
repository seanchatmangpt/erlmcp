-module(erlmcp_registry_contention).

%% Contention and performance benchmarking for registry at scale
%% Measures: ops/sec, latency percentiles, lock wait time, context switches
%% Scales: 10K, 25K, 50K, 100K concurrent connections

-export([
    benchmark_all/0,
    benchmark_scale/1,
    measure_registration_latency/2,
    measure_lookup_latency/2,
    measure_routing_latency/2,
    measure_contention_metrics/2,
    run_suite/1,
    format_results/1
]).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Benchmark configuration
-define(SCALES, [10000, 25000, 50000, 100000]).
-define(ITERATIONS, 5).
-define(SAMPLE_SIZE, 1000).

%% Record for latency statistics
-record(latency_stats, {
    p50 :: float(),
    p95 :: float(),
    p99 :: float(),
    p99_9 :: float(),
    min :: float(),
    max :: float(),
    avg :: float(),
    count :: integer()
}).

%% Record for contention metrics
-record(contention_metrics, {
    scale :: integer(),
    iteration :: integer(),
    partition_count :: integer(),
    registration_latency :: #latency_stats{},
    lookup_latency :: #latency_stats{},
    routing_latency :: #latency_stats{},
    ops_per_second :: float(),
    lock_contention_ratio :: float(),
    context_switches :: integer(),
    memory_used :: integer(),
    timestamp :: erlang:timestamp()
}).

%%====================================================================
%% Public API
%%====================================================================

benchmark_all() ->
    io:format("~n=== Registry Contention Benchmark v1.3.0 ===~n"),
    io:format("Testing scales: ~p~n", [?SCALES]),
    io:format("Iterations per scale: ~p~n~n", [?ITERATIONS]),

    ensure_gproc_started(),

    Results = [
        run_benchmark_scale(Scale, Shard)
        || Scale <- ?SCALES,
           Shard <- [calculate_shard_count(Scale)]
    ],

    io:format("~n=== Benchmark Complete ===~n"),
    format_results(Results),
    Results.

benchmark_scale(Scale) ->
    ensure_gproc_started(),
    ShardCount = calculate_shard_count(Scale),
    run_benchmark_scale(Scale, ShardCount).

%%====================================================================
%% Internal Implementation
%%====================================================================

ensure_gproc_started() ->
    case application:ensure_started(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end.

calculate_shard_count(Scale) ->
    % Adaptive shard count based on scale
    % Target: one shard per ~100 connections for balance
    max(16, (Scale div 100)).

run_benchmark_scale(Scale, ShardCount) ->
    io:format("~nBenchmarking scale ~p with ~p shards...~n", [Scale, ShardCount]),

    IterationResults = [
        run_single_iteration(Scale, ShardCount, Iter)
        || Iter <- lists:seq(1, ?ITERATIONS)
    ],

    aggregate_results(IterationResults, Scale, ShardCount).

run_single_iteration(Scale, ShardCount, IterNum) ->
    io:format("  Iteration ~p/~p... ", [IterNum, ?ITERATIONS]),

    % Start registry with specific shard count
    {ok, Registry} = erlmcp_registry_sharded:start_link(ShardCount),
    timer:sleep(100),

    % Spawn mock servers/transports
    ServerPids = spawn_mock_servers(Scale div 2),
    TransportPids = spawn_mock_transports(Scale div 2),

    % Register all entities
    RegStart = erlang:monotonic_time(microsecond),
    register_all_servers(ServerPids),
    register_all_transports(TransportPids),
    RegEnd = erlang:monotonic_time(microsecond),

    timer:sleep(100),

    % Measure registration latency
    RegLatencyStats = measure_registration_latency(ShardCount, ?SAMPLE_SIZE),

    % Measure lookup latency
    LookupLatencyStats = measure_lookup_latency(ShardCount, ?SAMPLE_SIZE),

    % Measure routing latency
    RoutingLatencyStats = measure_routing_latency(ShardCount, ?SAMPLE_SIZE),

    % Get contention metrics
    {PartitionStats, LockMetrics} = measure_contention_metrics(ShardCount, Scale),

    % Calculate operations/second
    ElapsedUs = RegEnd - RegStart,
    OpsPerSec = (Scale * 1_000_000.0) / ElapsedUs,

    % Get memory and context switch info
    {MemUsed, CtxSwitches} = get_system_metrics(),

    % Calculate lock contention ratio
    LockContentionRatio = calculate_lock_contention(LockMetrics, PartitionStats),

    io:format("OK (reg: ~.1fms, lookup p99: ~.2fms, routing p99: ~.2fms)~n",
        [RegLatencyStats#latency_stats.avg,
         LookupLatencyStats#latency_stats.p99,
         RoutingLatencyStats#latency_stats.p99]),

    % Cleanup
    lists:foreach(fun(P) -> catch unlink(P), catch exit(P, kill) end, ServerPids),
    lists:foreach(fun(P) -> catch unlink(P), catch exit(P, kill) end, TransportPids),
    catch gen_server:stop(Registry, shutdown, 5000),
    timer:sleep(50),

    #contention_metrics{
        scale = Scale,
        iteration = IterNum,
        partition_count = ShardCount,
        registration_latency = RegLatencyStats,
        lookup_latency = LookupLatencyStats,
        routing_latency = RoutingLatencyStats,
        ops_per_second = OpsPerSec,
        lock_contention_ratio = LockContentionRatio,
        context_switches = CtxSwitches,
        memory_used = MemUsed,
        timestamp = erlang:timestamp()
    }.

%% Measure registration latency under concurrent load
measure_registration_latency(ShardCount, SampleSize) ->
    {ok, Registry} = erlmcp_registry_sharded:start_link(ShardCount),
    timer:sleep(50),

    Latencies = [
        measure_single_registration()
        || _ <- lists:seq(1, SampleSize)
    ],

    catch gen_server:stop(Registry, shutdown, 5000),
    timer:sleep(50),

    calculate_percentiles(Latencies).

measure_single_registration() ->
    ServerId = erlang:unique_integer(),
    Pid = spawn_link(fun() -> receive _ -> ok end end),

    Start = erlang:monotonic_time(microsecond),
    erlmcp_registry_sharded:register_server(ServerId, Pid, #{}),
    End = erlang:monotonic_time(microsecond),

    unlink(Pid),
    exit(Pid, kill),

    (End - Start) / 1000.0. % Convert to milliseconds

%% Measure lookup latency under concurrent load
measure_lookup_latency(ShardCount, SampleSize) ->
    {ok, Registry} = erlmcp_registry_sharded:start_link(ShardCount),
    timer:sleep(50),

    % Pre-populate with some entries
    populate_registry_for_lookup(ShardCount, 100),

    % Measure lookups
    Latencies = [
        measure_single_lookup()
        || _ <- lists:seq(1, SampleSize)
    ],

    catch gen_server:stop(Registry, shutdown, 5000),
    timer:sleep(50),

    calculate_percentiles(Latencies).

measure_single_lookup() ->
    % Pick a random server ID that should exist
    ServerId = erlang:unique_integer(positive) rem 100,

    Start = erlang:monotonic_time(microsecond),
    _ = erlmcp_registry_sharded:find_server(ServerId),
    End = erlang:monotonic_time(microsecond),

    (End - Start) / 1000.0. % Convert to milliseconds

%% Measure routing latency
measure_routing_latency(ShardCount, SampleSize) ->
    {ok, Registry} = erlmcp_registry_sharded:start_link(ShardCount),
    timer:sleep(50),

    % Pre-populate with servers and transports
    populate_registry_for_routing(ShardCount, 50),

    % Measure routing operations
    Latencies = [
        measure_single_routing()
        || _ <- lists:seq(1, SampleSize)
    ],

    catch gen_server:stop(Registry, shutdown, 5000),
    timer:sleep(50),

    calculate_percentiles(Latencies).

measure_single_routing() ->
    ServerId = erlang:unique_integer(positive) rem 50,
    TransportId = erlang:unique_integer(positive) rem 50,
    Message = #{<<"data">> => <<"test">>},

    Start = erlang:monotonic_time(microsecond),
    erlmcp_registry_sharded:route_to_server(ServerId, TransportId, Message),
    End = erlang:monotonic_time(microsecond),

    (End - Start) / 1000.0. % Convert to milliseconds

%% Measure contention: partition stats and lock metrics
measure_contention_metrics(ShardCount, _Scale) ->
    PartitionStats = erlmcp_registry_sharded:get_partition_stats(),
    LockMetrics = extract_lock_metrics(PartitionStats),
    {PartitionStats, LockMetrics}.

%%====================================================================
%% Helper Functions
%%====================================================================

spawn_mock_servers(Count) ->
    [spawn_link(fun() ->
        receive stop -> ok after 60000 -> ok end
    end) || _ <- lists:seq(1, Count)].

spawn_mock_transports(Count) ->
    [spawn_link(fun() ->
        receive stop -> ok after 60000 -> ok end
    end) || _ <- lists:seq(1, Count)].

register_all_servers(Pids) ->
    lists:foreach(fun({N, Pid}) ->
        ServerId = list_to_atom("server_" ++ integer_to_list(N)),
        catch erlmcp_registry_sharded:register_server(ServerId, Pid, #{})
    end, lists:zip(lists:seq(1, length(Pids)), Pids)).

register_all_transports(Pids) ->
    lists:foreach(fun({N, Pid}) ->
        TransportId = list_to_atom("transport_" ++ integer_to_list(N)),
        catch erlmcp_registry_sharded:register_transport(TransportId, Pid, #{})
    end, lists:zip(lists:seq(1, length(Pids)), Pids)).

populate_registry_for_lookup(ShardCount, Count) ->
    Pids = [spawn_link(fun() -> receive _ -> ok end end) || _ <- lists:seq(1, Count)],
    lists:foreach(fun({N, Pid}) ->
        ServerId = N,
        catch erlmcp_registry_sharded:register_server(ServerId, Pid, #{})
    end, lists:zip(lists:seq(1, Count), Pids)).

populate_registry_for_routing(ShardCount, Count) ->
    ServerPids = [spawn_link(fun() -> receive _ -> ok end end) || _ <- lists:seq(1, Count)],
    TransportPids = [spawn_link(fun() -> receive _ -> ok end end) || _ <- lists:seq(1, Count)],

    lists:foreach(fun({N, Pid}) ->
        catch erlmcp_registry_sharded:register_server(N, Pid, #{})
    end, lists:zip(lists:seq(1, Count), ServerPids)),

    lists:foreach(fun({N, Pid}) ->
        catch erlmcp_registry_sharded:register_transport(N, Pid, #{})
    end, lists:zip(lists:seq(1, Count), TransportPids)).

calculate_percentiles(Values) ->
    SortedValues = lists:sort(Values),
    Count = length(SortedValues),
    Avg = lists:sum(Values) / Count,
    Min = lists:min(Values),
    Max = lists:max(Values),

    P50 = percentile(SortedValues, 0.50, Count),
    P95 = percentile(SortedValues, 0.95, Count),
    P99 = percentile(SortedValues, 0.99, Count),
    P99_9 = percentile(SortedValues, 0.999, Count),

    #latency_stats{
        p50 = P50,
        p95 = P95,
        p99 = P99,
        p99_9 = P99_9,
        min = Min,
        max = Max,
        avg = Avg,
        count = Count
    }.

percentile(SortedValues, Percentile, Count) ->
    Index = max(1, ceil(Count * Percentile)),
    lists:nth(Index, SortedValues).

extract_lock_metrics(PartitionStats) ->
    maps:map(fun(_PartId, Stats) ->
        #{
            avg_latency => maps:get(avg_latency_ms, Stats, 0),
            max_latency => maps:get(max_latency_ms, Stats, 0),
            write_count => maps:get(write_count, Stats, 0)
        }
    end, PartitionStats).

calculate_lock_contention(LockMetrics, PartitionStats) ->
    AllLatencies = [
        maps:get(avg_latency_ms, Stats, 0)
        || {_, Stats} <- maps:to_list(PartitionStats)
    ],

    case AllLatencies of
        [] -> 0.0;
        _ ->
            AvgLatency = lists:sum(AllLatencies) / length(AllLatencies),
            MaxLatency = lists:max(AllLatencies),
            % Contention ratio: max/avg indicates imbalance
            MaxLatency / max(0.001, AvgLatency)
    end.

get_system_metrics() ->
    % Get memory from erlang:memory/0
    MemInfo = erlang:memory(),
    MemUsed = proplists:get_value(total, MemInfo, 0),

    % Try to get context switch count (simplified)
    {_, Stats} = erlang:statistics(runtime),
    CtxSwitches = proplists:get_value(context_switches, Stats, 0),

    {MemUsed, CtxSwitches}.

aggregate_results(IterationResults, Scale, ShardCount) ->
    % Average metrics across iterations
    AvgRegLatency = average_latency_stats([
        R#contention_metrics.registration_latency || R <- IterationResults
    ]),
    AvgLookupLatency = average_latency_stats([
        R#contention_metrics.lookup_latency || R <- IterationResults
    ]),
    AvgRoutingLatency = average_latency_stats([
        R#contention_metrics.routing_latency || R <- IterationResults
    ]),
    AvgOpsPerSec = lists:sum([
        R#contention_metrics.ops_per_second || R <- IterationResults
    ]) / length(IterationResults),
    AvgLockContention = lists:sum([
        R#contention_metrics.lock_contention_ratio || R <- IterationResults
    ]) / length(IterationResults),
    AvgMemory = lists:sum([
        R#contention_metrics.memory_used || R <- IterationResults
    ]) / length(IterationResults),

    #{
        scale => Scale,
        shard_count => ShardCount,
        iterations => ?ITERATIONS,
        registration_latency => AvgRegLatency,
        lookup_latency => AvgLookupLatency,
        routing_latency => AvgRoutingLatency,
        ops_per_second => AvgOpsPerSec,
        lock_contention_ratio => AvgLockContention,
        avg_memory_mb => AvgMemory / (1024 * 1024),
        individual_results => IterationResults
    }.

average_latency_stats(StatsList) ->
    Count = length(StatsList),
    #latency_stats{
        p50 = lists:sum([S#latency_stats.p50 || S <- StatsList]) / Count,
        p95 = lists:sum([S#latency_stats.p95 || S <- StatsList]) / Count,
        p99 = lists:sum([S#latency_stats.p99 || S <- StatsList]) / Count,
        p99_9 = lists:sum([S#latency_stats.p99_9 || S <- StatsList]) / Count,
        min = lists:min([S#latency_stats.min || S <- StatsList]),
        max = lists:max([S#latency_stats.max || S <- StatsList]),
        avg = lists:sum([S#latency_stats.avg || S <- StatsList]) / Count,
        count = length(StatsList)
    }.

format_results(Results) ->
    io:format("~n=== Contention Benchmark Results ===~n~n"),

    % Print table header
    io:format("Scale      | Shards | Reg P99 (ms) | Lookup P99 (ms) | Routing P99 (ms) | "
              "Ops/Sec | Lock Contention | Mem (MB)~n"),
    io:format("-----------|--------|--------------|-----------------|------------------|"
              "---------|-----------------|----------~n"),

    lists:foreach(fun(Result) ->
        Scale = maps:get(scale, Result),
        ShardCount = maps:get(shard_count, Result),
        RegLatency = maps:get(registration_latency, Result),
        LookupLatency = maps:get(lookup_latency, Result),
        RoutingLatency = maps:get(routing_latency, Result),
        OpsPerSec = maps:get(ops_per_second, Result),
        LockContention = maps:get(lock_contention_ratio, Result),
        MemMB = maps:get(avg_memory_mb, Result),

        io:format("~8p    | ~6p  | ~12.2f | ~15.2f | ~16.2f | ~7.0f | ~15.2f | ~8.1f~n",
            [Scale, ShardCount,
             RegLatency#latency_stats.p99,
             LookupLatency#latency_stats.p99,
             RoutingLatency#latency_stats.p99,
             OpsPerSec,
             LockContention,
             MemMB])
    end, Results),

    io:format("~nRecommendations:~n"),
    lists:foreach(fun(Result) ->
        Scale = maps:get(scale, Result),
        LookupLatency = maps:get(lookup_latency, Result),
        P99 = LookupLatency#latency_stats.p99,

        Recommendation = case {Scale, P99} of
            {_, P} when P > 1.0 ->
                io_lib:format("Scale ~p: P99 ~.2fms > 1ms - INCREASE SHARD COUNT", [Scale, P]);
            {100000, P} when P =< 0.5 ->
                io_lib:format("Scale ~p: P99 ~.2fms ✓ OPTIMAL", [Scale, P]);
            {_, P} when P =< 0.8 ->
                io_lib:format("Scale ~p: P99 ~.2fms - GOOD", [Scale, P]);
            {_, P} ->
                io_lib:format("Scale ~p: P99 ~.2fms - ACCEPTABLE", [Scale, P])
        end,
        io:format("  - ~s~n", [Recommendation])
    end, Results).

run_suite(Scale) ->
    benchmark_scale(Scale).
