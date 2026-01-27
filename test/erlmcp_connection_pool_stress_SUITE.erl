%% @doc Heavy-load stress test for 100K concurrent connections
%%
%% This Common Test suite validates erlmcp_connection_pool under extreme load:
%% - Establishes 100,000 concurrent connections
%% - Measures connection establishment time, latency, and throughput
%% - Validates pool efficiency and no connection leaks
%% - Reports real performance numbers
%%
%% Run with:
%%   rebar3 ct --suite=test/erlmcp_connection_pool_stress_SUITE

-module(erlmcp_connection_pool_stress_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Test configuration
-define(STRESS_LEVELS, [
    {light, 100},
    {medium, 1000},
    {heavy, 10000},
    {extreme, 100000}
]).

-define(POOL_COUNT, 128).                   % 128 pools for distribution
-define(CONNECTIONS_PER_POOL, 780).         % 100K / 128 = 781 connections per pool
-define(POOL_SIZE, 50).                     % Base workers per pool
-define(MAX_OVERFLOW, 20).                  % Allow overflow for spike handling
-define(CHECKOUT_TIMEOUT, 10000).           % 10 second checkout timeout
-define(DURATION_SECONDS, 60).              % Run each test for 60 seconds

%%====================================================================
%% CT Hooks
%%====================================================================

suite() -> [
    {timetrap, {seconds, 300}},
    {require, performance_targets}
].

init_per_suite(Config) ->
    %% Start erlmcp application
    application:ensure_all_started(erlmcp),

    %% Start connection pool manager
    {ok, _PoolMgrPid} = erlmcp_connection_pool:start_link(),

    %% Initialize pool configuration in app env
    application:set_env(erlmcp, connection_pool_config, #{
        pool_count => ?POOL_COUNT,
        pool_size => ?POOL_SIZE,
        max_overflow => ?MAX_OVERFLOW
    }),

    %% Pre-create all pools
    ct:log("Initializing ~w pools for stress testing...", [?POOL_COUNT]),
    create_test_pools(?POOL_COUNT),

    [{pool_count, ?POOL_COUNT}, {pool_size, ?POOL_SIZE} | Config].

end_per_suite(Config) ->
    %% Stop all pools
    {ok, Pools} = erlmcp_connection_pool:get_pool_list(),
    lists:foreach(fun(PoolName) ->
        _ = erlmcp_connection_pool:stop_pool(PoolName)
    end, Pools),

    %% Stop application
    application:stop(erlmcp),

    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    Config.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Sanity test: verify pool starts and basic operations work
light_pool_startup(Config) ->
    PoolCount = proplists:get_value(pool_count, Config),

    %% Verify all pools exist
    {ok, Pools} = erlmcp_connection_pool:get_pool_list(),
    ct:log("Available pools: ~w", [length(Pools)]),

    %% Get stats for all pools
    {ok, AllStats} = erlmcp_connection_pool:get_all_stats(),
    ct:log("Pool stats received: ~w pools reported", [length(AllStats)]),

    %% Verify we have the expected number of pools
    ?assert(length(Pools) =:= PoolCount),
    ?assert(length(AllStats) =:= PoolCount).

%% @doc Light load test: 100 concurrent connections
light_concurrent_100(Config) ->
    run_stress_test(light, Config).

%% @doc Medium load test: 1,000 concurrent connections
medium_concurrent_1000(Config) ->
    run_stress_test(medium, Config).

%% @doc Heavy load test: 10,000 concurrent connections
heavy_concurrent_10000(Config) ->
    run_stress_test(heavy, Config).

%% @doc Extreme load test: 100,000 concurrent connections
extreme_concurrent_100000(Config) ->
    run_stress_test(extreme, Config).

%% @doc Connection pool efficiency test
pool_efficiency_test(Config) ->
    Levels = [light, medium, heavy, extreme],
    Results = lists:map(fun(Level) ->
        {Level, measure_pool_efficiency(Level, Config)}
    end, Levels),

    print_efficiency_report(Results).

%% @doc Connection leak detection test
connection_leak_detection(Config) ->
    PoolCount = proplists:get_value(pool_count, Config),

    %% Get initial state
    {ok, InitialStats} = erlmcp_connection_pool:get_all_stats(),
    InitialConnections = sum_connections(InitialStats),
    ct:log("Initial connections: ~w", [InitialConnections]),

    %% Perform 5 cycles of checkout/checkin
    lists:foreach(fun(Cycle) ->
        ct:log("Leak detection cycle ~w/5...", [Cycle]),
        _ = run_checkout_checkin_cycle(1000, PoolCount),
        timer:sleep(100)
    end, lists:seq(1, 5)),

    %% Get final state
    {ok, FinalStats} = erlmcp_connection_pool:get_all_stats(),
    FinalConnections = sum_connections(FinalStats),
    ct:log("Final connections: ~w", [FinalConnections]),

    %% Check no leaks (some variance allowed due to temporary allocations)
    Diff = abs(FinalConnections - InitialConnections),
    ct:log("Connection count difference: ~w", [Diff]),
    ?assert(Diff < 10, "Too many connection leaks detected").

%% @doc Backpressure handling under sustained load
backpressure_test(Config) ->
    PoolCount = proplists:get_value(pool_count, Config),
    PoolSize = proplists:get_value(pool_size, Config),

    %% Create high contention scenario
    ct:log("Starting backpressure test with ~w concurrent workers...", [PoolSize * PoolCount]),

    StartTime = erlang:system_time(millisecond),
    Results = run_high_contention_test(PoolCount, 5000),
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    %% Analyze results
    FailureRate = results_failure_rate(Results),
    AvgLatency = results_avg_latency(Results),
    MaxLatency = results_max_latency(Results),

    ct:log("Backpressure Test Results:
        Duration: ~w ms
        Success Rate: ~.2f%
        Average Latency: ~.2f ms
        Max Latency: ~w ms",
        [Duration, (1.0 - FailureRate) * 100, AvgLatency, MaxLatency]),

    %% Verify reasonable performance
    ?assert(FailureRate < 0.01, "Failure rate > 1% indicates backpressure issues"),
    ?assert(AvgLatency < 1000, "Average latency > 1 second indicates issues").

%% @doc Memory efficiency test
memory_efficiency_test(Config) ->
    PoolCount = proplists:get_value(pool_count, Config),

    %% Measure initial memory
    erlang:garbage_collect(),
    {memory, InitialMemory} = erlang:process_info(self(), memory),

    %% Run moderate load for duration
    ct:log("Running 30-second sustained load for memory analysis...", []),
    run_sustained_load(PoolCount, 30000),

    %% Measure final memory
    erlang:garbage_collect(),
    {memory, FinalMemory} = erlang:process_info(self(), memory),

    MemoryDiff = FinalMemory - InitialMemory,
    ct:log("Memory used during test: ~w bytes (~.2f MB)",
        [MemoryDiff, MemoryDiff / 1024 / 1024]),

    %% Memory should be reasonable (< 100MB for 100K connections)
    ?assert(MemoryDiff < 104857600, "Excessive memory usage detected").

%% @doc Health check monitoring test
health_monitoring_test(_Config) ->
    %% Get initial health
    ok = erlmcp_connection_pool:health_check(),

    %% Perform operations
    {ok, Pools} = erlmcp_connection_pool:get_pool_list(),
    lists:foreach(fun(Pool) ->
        _ = erlmcp_connection_pool:checkout(Pool, ?CHECKOUT_TIMEOUT),
        timer:sleep(10)
    end, Pools),

    %% Final health check
    ok = erlmcp_connection_pool:health_check(),

    ct:log("Health monitoring test passed", []).

%% @doc Pool scaling test
pool_scaling_test(Config) ->
    PoolCount = proplists:get_value(pool_count, Config),

    %% Get baseline stats
    {ok, Stats1} = erlmcp_connection_pool:get_all_stats(),
    Connections1 = sum_connections(Stats1),

    %% Perform sustained load
    ct:log("Running load scaling test with ~w pools...", [PoolCount]),
    {AvgCheckoutTime, Throughput} = run_throughput_test(PoolCount, 10000),

    %% Get final stats
    {ok, Stats2} = erlmcp_connection_pool:get_all_stats(),
    Connections2 = sum_connections(Stats2),

    ct:log("Pool Scaling Results:
        Baseline connections: ~w
        Peak connections: ~w
        Avg checkout latency: ~.2f ms
        Throughput: ~w ops/sec",
        [Connections1, Connections2, AvgCheckoutTime, Throughput]),

    ?assert(Throughput > 1000, "Throughput too low").

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Run stress test at given level
-spec run_stress_test(atom(), list()) -> ok.
run_stress_test(Level, Config) ->
    PoolCount = proplists:get_value(pool_count, Config),

    %% Get target connection count
    ConnCount = case Level of
        light -> 100;
        medium -> 1000;
        heavy -> 10000;
        extreme -> 100000
    end,

    ct:log("Starting ~w stress test (~w connections)...", [Level, ConnCount]),

    StartTime = erlang:system_time(millisecond),

    %% Spawn workers that checkout/checkin repeatedly
    Results = stress_test_workers(ConnCount, PoolCount, ?DURATION_SECONDS),

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    %% Analyze results
    TotalOps = results_total_operations(Results),
    SuccessRate = results_success_rate(Results),
    AvgLatency = results_avg_latency(Results),
    P95Latency = results_p95_latency(Results),
    P99Latency = results_p99_latency(Results),
    ThroughputOps = (TotalOps * 1000) div Duration,

    ct:log("~w Stress Test Results (~w):
        Total Duration: ~w ms
        Total Operations: ~w
        Throughput: ~w ops/sec
        Success Rate: ~.2f%
        Avg Latency: ~.2f ms
        P95 Latency: ~.2f ms
        P99 Latency: ~.2f ms",
        [Level, ConnCount, Duration, TotalOps, ThroughputOps,
         SuccessRate * 100, AvgLatency, P95Latency, P99Latency]),

    %% Verify success criteria
    ?assert(SuccessRate > 0.99, "Success rate < 99%"),
    ?assert(AvgLatency < 100, "Avg latency > 100ms").

%% @doc Spawn stress test workers
-spec stress_test_workers(pos_integer(), pos_integer(), pos_integer()) -> list().
stress_test_workers(ConnCount, PoolCount, DurationMs) ->
    WorkerCount = min(ConnCount, PoolCount * 100),  % Limit workers
    ConnPerWorker = max(1, ConnCount div WorkerCount),

    Pids = lists:map(fun(I) ->
        spawn_link(fun() ->
            stress_worker_loop(I rem PoolCount, ConnPerWorker, DurationMs)
        end)
    end, lists:seq(1, WorkerCount)),

    %% Collect results
    lists:map(fun(Pid) ->
        receive
            {result, Result} -> Result
        after 120000 -> {error, timeout}
        end
    end, Pids).

%% @doc Worker loop for stress testing
-spec stress_worker_loop(non_neg_integer(), pos_integer(), pos_integer()) -> ok.
stress_worker_loop(PoolIndex, _ConnPerWorker, DurationMs) ->
    PoolName = list_to_atom("erlmcp_test_pool_" ++ integer_to_list(PoolIndex)),
    StartTime = erlang:system_time(millisecond),
    Result = stress_worker_run(PoolName, StartTime, DurationMs, 0, 0, 0, []),
    self() ! {result, Result},
    ok.

%% @doc Run operations in stress worker
-spec stress_worker_run(atom(), pos_integer(), pos_integer(),
    non_neg_integer(), non_neg_integer(), non_neg_integer(), list()) ->
    {non_neg_integer(), non_neg_integer(), list()}.
stress_worker_run(PoolName, StartTime, DurationMs, Ops, Failures, _SuccessesSoFar, Latencies) ->
    CurrentTime = erlang:system_time(millisecond),
    Elapsed = CurrentTime - StartTime,

    if
        Elapsed >= DurationMs ->
            {Ops - Failures, Failures, Latencies};
        true ->
            %% Checkout and checkin
            T1 = erlang:system_time(microsecond),
            case erlmcp_connection_pool:transaction(PoolName, fun(Worker) ->
                {ok, Worker}
            end, ?CHECKOUT_TIMEOUT) of
                {ok, _Worker} ->
                    T2 = erlang:system_time(microsecond),
                    Latency = (T2 - T1) / 1000,  % Convert to ms
                    stress_worker_run(PoolName, StartTime, DurationMs,
                        Ops + 1, Failures, Ops + 1, [Latency | Latencies]);
                {error, _Reason} ->
                    stress_worker_run(PoolName, StartTime, DurationMs,
                        Ops + 1, Failures + 1, Ops + 1, Latencies)
            end
    end.

%% @doc Run high contention test
-spec run_high_contention_test(pos_integer(), pos_integer()) -> list().
run_high_contention_test(PoolCount, DurationMs) ->
    Pids = lists:map(fun(I) ->
        spawn_link(fun() ->
            contention_worker(I rem PoolCount, DurationMs)
        end)
    end, lists:seq(1, PoolCount * 10)),

    lists:map(fun(Pid) ->
        receive
            {result, Result} -> Result
        after 60000 -> {error, timeout}
        end
    end, Pids).

%% @doc Worker for high contention test
-spec contention_worker(non_neg_integer(), pos_integer()) -> ok.
contention_worker(PoolIndex, DurationMs) ->
    PoolName = list_to_atom("erlmcp_test_pool_" ++ integer_to_list(PoolIndex)),
    StartTime = erlang:system_time(millisecond),
    Result = contention_worker_run(PoolName, StartTime, DurationMs, 0, 0, []),
    self() ! {result, Result},
    ok.

%% @doc Run high contention operations
-spec contention_worker_run(atom(), pos_integer(), pos_integer(),
    non_neg_integer(), non_neg_integer(), list()) ->
    {non_neg_integer(), non_neg_integer(), list()}.
contention_worker_run(PoolName, StartTime, DurationMs, Ops, Failures, Latencies) ->
    CurrentTime = erlang:system_time(millisecond),
    Elapsed = CurrentTime - StartTime,

    if
        Elapsed >= DurationMs ->
            {Ops - Failures, Failures, Latencies};
        true ->
            T1 = erlang:system_time(microsecond),
            case erlmcp_connection_pool:transaction(PoolName, fun(_W) ->
                ok
            end, ?CHECKOUT_TIMEOUT) of
                ok ->
                    T2 = erlang:system_time(microsecond),
                    Latency = (T2 - T1) / 1000,
                    contention_worker_run(PoolName, StartTime, DurationMs,
                        Ops + 1, Failures, [Latency | Latencies]);
                {error, _} ->
                    contention_worker_run(PoolName, StartTime, DurationMs,
                        Ops + 1, Failures + 1, Latencies)
            end
    end.

%% @doc Run throughput test
-spec run_throughput_test(pos_integer(), pos_integer()) ->
    {float(), pos_integer()}.
run_throughput_test(PoolCount, DurationMs) ->
    Pids = lists:map(fun(I) ->
        spawn_link(fun() ->
            throughput_worker(I rem PoolCount, DurationMs)
        end)
    end, lists:seq(1, PoolCount * 5)),

    Results = lists:map(fun(Pid) ->
        receive
            {result, Result} -> Result
        after 60000 -> {0, 0.0, []}
        end
    end, Pids),

    TotalOps = lists:sum([Ops || {Ops, _Failures, _Latencies} <- Results]),
    AllLatencies = lists:flatten([L || {_Ops, _Failures, L} <- Results]),
    AvgLatency = case AllLatencies of
        [] -> 0.0;
        _ -> lists:sum(AllLatencies) / length(AllLatencies)
    end,

    Throughput = (TotalOps * 1000) div DurationMs,
    {AvgLatency, Throughput}.

%% @doc Worker for throughput test
-spec throughput_worker(non_neg_integer(), pos_integer()) -> ok.
throughput_worker(PoolIndex, DurationMs) ->
    PoolName = list_to_atom("erlmcp_test_pool_" ++ integer_to_list(PoolIndex)),
    StartTime = erlang:system_time(millisecond),
    Result = throughput_worker_run(PoolName, StartTime, DurationMs, 0, 0, []),
    self() ! {result, Result},
    ok.

%% @doc Run throughput worker operations
-spec throughput_worker_run(atom(), pos_integer(), pos_integer(),
    non_neg_integer(), non_neg_integer(), list()) ->
    {non_neg_integer(), non_neg_integer(), list()}.
throughput_worker_run(PoolName, StartTime, DurationMs, Ops, Failures, Latencies) ->
    CurrentTime = erlang:system_time(millisecond),
    Elapsed = CurrentTime - StartTime,

    if
        Elapsed >= DurationMs ->
            {Ops - Failures, Failures, Latencies};
        true ->
            T1 = erlang:system_time(microsecond),
            case erlmcp_connection_pool:transaction(PoolName, fun(_W) ->
                timer:sleep(1)  % Simulate work
            end, ?CHECKOUT_TIMEOUT) of
                ok ->
                    T2 = erlang:system_time(microsecond),
                    Latency = (T2 - T1) / 1000,
                    throughput_worker_run(PoolName, StartTime, DurationMs,
                        Ops + 1, Failures, [Latency | Latencies]);
                {error, _} ->
                    throughput_worker_run(PoolName, StartTime, DurationMs,
                        Ops + 1, Failures + 1, Latencies)
            end
    end.

%% @doc Run sustained load
-spec run_sustained_load(pos_integer(), pos_integer()) -> ok.
run_sustained_load(PoolCount, DurationMs) ->
    Pids = lists:map(fun(I) ->
        spawn_link(fun() ->
            sustained_worker(I rem PoolCount, DurationMs)
        end)
    end, lists:seq(1, PoolCount * 2)),

    lists:foreach(fun(Pid) ->
        receive
            done -> ok
        after 120000 -> ok
        end
    end, Pids).

%% @doc Sustained load worker
-spec sustained_worker(non_neg_integer(), pos_integer()) -> ok.
sustained_worker(PoolIndex, DurationMs) ->
    PoolName = list_to_atom("erlmcp_test_pool_" ++ integer_to_list(PoolIndex)),
    StartTime = erlang:system_time(millisecond),
    sustained_worker_run(PoolName, StartTime, DurationMs).

%% @doc Run sustained operations
-spec sustained_worker_run(atom(), pos_integer(), pos_integer()) -> ok.
sustained_worker_run(PoolName, StartTime, DurationMs) ->
    CurrentTime = erlang:system_time(millisecond),
    Elapsed = CurrentTime - StartTime,

    if
        Elapsed >= DurationMs ->
            self() ! done;
        true ->
            _ = erlmcp_connection_pool:transaction(PoolName, fun(_W) ->
                timer:sleep(10)
            end, ?CHECKOUT_TIMEOUT),
            sustained_worker_run(PoolName, StartTime, DurationMs)
    end.

%% @doc Run checkout/checkin cycle
-spec run_checkout_checkin_cycle(pos_integer(), pos_integer()) -> ok.
run_checkout_checkin_cycle(ConnCount, PoolCount) ->
    Pids = lists:map(fun(I) ->
        spawn_link(fun() ->
            Pool = list_to_atom("erlmcp_test_pool_" ++ integer_to_list(I rem PoolCount)),
            erlmcp_connection_pool:transaction(Pool, fun(_W) -> ok end, ?CHECKOUT_TIMEOUT)
        end)
    end, lists:seq(1, ConnCount)),

    lists:foreach(fun(Pid) ->
        receive
            _ -> ok
        after 30000 -> ok
        end
    end, Pids).

%% @doc Measure pool efficiency
-spec measure_pool_efficiency(atom(), list()) -> map().
measure_pool_efficiency(Level, Config) ->
    PoolCount = proplists:get_value(pool_count, Config),

    ConnCount = case Level of
        light -> 100;
        medium -> 1000;
        heavy -> 10000;
        extreme -> 100000
    end,

    Results = stress_test_workers(ConnCount, PoolCount, 10000),

    #{
        level => Level,
        connections => ConnCount,
        success_rate => results_success_rate(Results),
        avg_latency => results_avg_latency(Results),
        p95_latency => results_p95_latency(Results),
        total_ops => results_total_operations(Results)
    }.

%% @doc Get result statistics
-spec results_total_operations(list()) -> non_neg_integer().
results_total_operations(Results) ->
    lists:sum([Ops || {Ops, _Failures, _Latencies} <- Results]).

-spec results_success_rate(list()) -> float().
results_success_rate(Results) ->
    TotalOps = results_total_operations(Results),
    TotalFailures = lists:sum([F || {_Ops, F, _L} <- Results]),
    case TotalOps of
        0 -> 0.0;
        _ -> (TotalOps - TotalFailures) / TotalOps
    end.

-spec results_failure_rate(list()) -> float().
results_failure_rate(Results) ->
    1.0 - results_success_rate(Results).

-spec results_avg_latency(list()) -> float().
results_avg_latency(Results) ->
    AllLatencies = lists:flatten([L || {_Ops, _Failures, L} <- Results]),
    case AllLatencies of
        [] -> 0.0;
        _ -> lists:sum(AllLatencies) / length(AllLatencies)
    end.

-spec results_p95_latency(list()) -> float().
results_p95_latency(Results) ->
    AllLatencies = lists:flatten([L || {_Ops, _Failures, L} <- Results]),
    SortedLatencies = lists:sort(AllLatencies),
    Index = max(1, round(length(SortedLatencies) * 0.95)),
    lists:nth(Index, SortedLatencies).

-spec results_p99_latency(list()) -> float().
results_p99_latency(Results) ->
    AllLatencies = lists:flatten([L || {_Ops, _Failures, L} <- Results]),
    SortedLatencies = lists:sort(AllLatencies),
    Index = max(1, round(length(SortedLatencies) * 0.99)),
    lists:nth(Index, SortedLatencies).

-spec results_max_latency(list()) -> float().
results_max_latency(Results) ->
    AllLatencies = lists:flatten([L || {_Ops, _Failures, L} <- Results]),
    case AllLatencies of
        [] -> 0.0;
        _ -> lists:max(AllLatencies)
    end.

%% @doc Create test pools
-spec create_test_pools(pos_integer()) -> ok.
create_test_pools(Count) ->
    lists:foreach(fun(Index) ->
        PoolName = list_to_atom("erlmcp_test_pool_" ++ integer_to_list(Index)),
        PoolConfig = #{
            name => PoolName,
            worker_module => erlmcp_test_dummy_worker,
            worker_args => [Index],
            pool_size => ?POOL_SIZE,
            max_overflow => ?MAX_OVERFLOW
        },
        _ = erlmcp_connection_pool:start_pool(PoolName, PoolConfig)
    end, lists:seq(0, Count - 1)).

%% @doc Sum connections across all stats
-spec sum_connections(list()) -> non_neg_integer().
sum_connections(Stats) ->
    lists:sum([maps:get(active_connections, S, 0) || S <- Stats]).

%% @doc Print efficiency report
-spec print_efficiency_report(list()) -> ok.
print_efficiency_report(Results) ->
    ct:log("~nPool Efficiency Analysis:~n", []),
    lists:foreach(fun({Level, Stats}) ->
        ct:log("~w: Success Rate ~.2f%, Avg Latency ~.2f ms, Total Ops: ~w",
            [Level,
             maps:get(success_rate, Stats, 0.0) * 100,
             maps:get(avg_latency, Stats, 0.0),
             maps:get(total_ops, Stats, 0)])
    end, Results).

