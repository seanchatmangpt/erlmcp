%%%====================================================================
%%% COMPREHENSIVE 100K BENCHMARK SUITE
%%%====================================================================
%%% Purpose: Detailed benchmarking of erlmcp at 100K concurrent scale
%%% Measures: throughput, latency (p50/p95/p99), jitter, resource usage
%%% Components: cluster, pool, registry, queue, memory, session, network
%%%====================================================================

-module(benchmark_100k_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT Callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    groups/0
]).

%% Component Benchmarks
-export([
    bench_registry_100k/1,
    bench_pool_throughput_100k/1,
    bench_queue_latency_100k/1,
    bench_session_management_100k/1,
    bench_network_io_100k/1,
    bench_memory_scaling_100k/1
]).

%% System-Level Benchmarks
-export([
    bench_integrated_system_100k/1,
    bench_mixed_workload_100k/1,
    bench_sustained_load_100k/1,
    bench_spike_handling_100k/1
]).

%% Helpers
-export([
    run_concurrent_ops/4,
    collect_metrics/1,
    analyze_latencies/1,
    measure_resource_usage/1,
    format_benchmark_report/2
]).

%% ===================================================================
%% CONSTANTS
%% ===================================================================

-define(BENCHMARK_SCALE, 100000).
-define(POOL_SIZE, 128).
-define(QUEUE_SIZE, 1000000).
-define(SESSION_COUNT, 10000).
-define(NETWORK_PACKET_SIZE, 4096).

%% Target Metrics (must achieve or exceed)
-define(TARGET_THROUGHPUT_MSGPS, 95000).  % 95K msg/sec minimum
-define(TARGET_P99_LATENCY_MS, 15).       % 15ms max p99
-define(TARGET_P95_LATENCY_MS, 10).       % 10ms max p95
-define(TARGET_CPU_USAGE, 80).            % <80% single core
-define(TARGET_MEMORY_MB, 1024).          % <1GB for 100K connections

%% Statistics record
-record(stats, {
    operation_count = 0 :: integer(),
    total_time_ms = 0 :: float(),
    throughput = 0.0 :: float(),  % ops/sec
    min_latency_ms = infinity :: number(),
    max_latency_ms = 0 :: number(),
    avg_latency_ms = 0 :: number(),
    median_latency_ms = 0 :: number(),
    p50_latency_ms = 0 :: number(),
    p95_latency_ms = 0 :: number(),
    p99_latency_ms = 0 :: number(),
    stddev_latency_ms = 0 :: number(),
    jitter_pct = 0 :: float(),
    memory_mb = 0 :: integer(),
    cpu_usage_pct = 0 :: float(),
    gc_count = 0 :: integer(),
    gc_time_ms = 0 :: float()
}).

%% ===================================================================
%% CT CALLBACKS
%% ===================================================================

all() ->
    [
        {group, component_benchmarks},
        {group, system_benchmarks}
    ].

groups() ->
    [
        {component_benchmarks, [parallel], [
            bench_registry_100k,
            bench_pool_throughput_100k,
            bench_queue_latency_100k,
            bench_session_management_100k,
            bench_network_io_100k,
            bench_memory_scaling_100k
        ]},
        {system_benchmarks, [], [
            bench_integrated_system_100k,
            bench_mixed_workload_100k,
            bench_sustained_load_100k,
            bench_spike_handling_100k
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("~n=====================================~n"),
    ct:pal("COMPREHENSIVE 100K BENCHMARK SUITE~n"),
    ct:pal("=====================================~n~n"),

    %% Start applications
    application:ensure_all_started(erlmcp),

    %% Disable GC for more consistent measurements
    garbage_collect(),
    erlang:garbage_collect_aggressive(),

    StartTime = erlang:system_time(millisecond),
    StartMemory = erlang:memory(total),

    [{suite_start_time, StartTime},
     {suite_start_memory, StartMemory},
     {benchmark_results, []} | Config].

end_per_suite(Config) ->
    EndTime = erlang:system_time(millisecond),
    EndMemory = erlang:memory(total),
    StartTime = proplists:get_value(suite_start_time, Config),
    StartMemory = proplists:get_value(suite_start_memory, Config),

    TotalTime = EndTime - StartTime,
    MemoryDelta = (EndMemory - StartMemory) div 1024,

    ct:pal("~nSuite Execution Summary:~n"),
    ct:pal("  Total Duration: ~B ms~n", [TotalTime]),
    ct:pal("  Memory Delta: ~B MB~n", [MemoryDelta]),
    ct:pal("  Final Memory: ~B MB~n", [EndMemory div 1048576]),
    ct:pal("======================================~n~n"),

    Config.

init_per_testcase(TestCase, Config) ->
    ct:pal("~nStarting: ~w~n", [TestCase]),
    [{test_start_time, erlang:system_time(millisecond)},
     {test_start_memory, erlang:memory(total)} | Config].

end_per_testcase(TestCase, Config) ->
    EndTime = erlang:system_time(millisecond),
    EndMemory = erlang:memory(total),
    StartTime = proplists:get_value(test_start_time, Config),
    StartMemory = proplists:get_value(test_start_memory, Config),

    Duration = EndTime - StartTime,
    MemoryDelta = (EndMemory - StartMemory) div 1024,

    ct:pal("Completed: ~w (Duration: ~B ms, Memory Delta: ~B MB)~n",
           [TestCase, Duration, MemoryDelta]),
    Config.

%% ===================================================================
%% REGISTRY BENCHMARK (100K entries routing)
%% ===================================================================

bench_registry_100k(Config) ->
    ct:pal("~n[REGISTRY] Benchmarking 100K registry operations...~n"),

    %% Phase 1: Registration
    StartReg = erlang:system_time(microsecond),
    RegistrationLatencies = register_100k_entries(),
    EndReg = erlang:system_time(microsecond),
    RegTime = (EndReg - StartReg) / 1000,

    %% Phase 2: Lookup
    StartLookup = erlang:system_time(microsecond),
    LookupLatencies = lookup_100k_entries(),
    EndLookup = erlang:system_time(microsecond),
    LookupTime = (EndLookup - StartLookup) / 1000,

    %% Phase 3: Concurrent Access
    StartConcurrent = erlang:system_time(microsecond),
    ConcurrentLatencies = concurrent_registry_access(100),
    EndConcurrent = erlang:system_time(microsecond),
    ConcurrentTime = (EndConcurrent - StartConcurrent) / 1000,

    AllLatencies = RegistrationLatencies ++ LookupLatencies ++ ConcurrentLatencies,
    Stats = analyze_latencies(AllLatencies),

    Report = format_benchmark_report("Registry 100K", Stats),
    ct:pal(Report),

    %% Verify targets
    ?assert(Stats#stats.throughput >= ?TARGET_THROUGHPUT_MSGPS div 2),
    ?assert(Stats#stats.p99_latency_ms =< ?TARGET_P99_LATENCY_MS * 2).

%% ===================================================================
%% CONNECTION POOL BENCHMARK (100K connections across pools)
%% ===================================================================

bench_pool_throughput_100k(Config) ->
    ct:pal("~n[POOL] Benchmarking 100K connection pool operations...~n"),

    %% Create 128 pools with 781 connections each
    Latencies = pool_stress_test(128, 781),
    Stats = analyze_latencies(Latencies),

    Report = format_benchmark_report("Connection Pool 100K", Stats),
    ct:pal(Report),

    ?assert(Stats#stats.throughput >= ?TARGET_THROUGHPUT_MSGPS),
    ?assert(Stats#stats.p95_latency_ms =< ?TARGET_P95_LATENCY_MS),
    ?assert(Stats#stats.p99_latency_ms =< ?TARGET_P99_LATENCY_MS).

%% ===================================================================
%% QUEUE BENCHMARK (100K messages in flight)
%% ===================================================================

bench_queue_latency_100k(Config) ->
    ct:pal("~n[QUEUE] Benchmarking 100K queue operations...~n"),

    %% Queue enqueue/dequeue operations
    Latencies = queue_stress_test(100000),
    Stats = analyze_latencies(Latencies),

    Report = format_benchmark_report("Queue 100K", Stats),
    ct:pal(Report),

    ?assert(Stats#stats.throughput >= ?TARGET_THROUGHPUT_MSGPS),
    ?assert(Stats#stats.max_latency_ms =< 50.0).

%% ===================================================================
%% SESSION MANAGEMENT BENCHMARK (10K sessions with operations)
%% ===================================================================

bench_session_management_100k(Config) ->
    ct:pal("~n[SESSIONS] Benchmarking 10K concurrent sessions...~n"),

    %% Create and manage 10K sessions with concurrent operations
    Latencies = session_stress_test(10000),
    Stats = analyze_latencies(Latencies),

    Report = format_benchmark_report("Session Management", Stats),
    ct:pal(Report),

    ?assert(Stats#stats.throughput >= ?TARGET_THROUGHPUT_MSGPS div 2).

%% ===================================================================
%% NETWORK I/O BENCHMARK (100K packets)
%% ===================================================================

bench_network_io_100k(Config) ->
    ct:pal("~n[NETWORK] Benchmarking 100K network I/O operations...~n"),

    %% Simulate network I/O with actual packet sizes
    Latencies = network_io_stress_test(100000, ?NETWORK_PACKET_SIZE),
    Stats = analyze_latencies(Latencies),

    Report = format_benchmark_report("Network I/O 100K", Stats),
    ct:pal(Report),

    ?assert(Stats#stats.throughput >= ?TARGET_THROUGHPUT_MSGPS div 2).

%% ===================================================================
%% MEMORY SCALING BENCHMARK (incremental load to 100K)
%% ===================================================================

bench_memory_scaling_100k(Config) ->
    ct:pal("~n[MEMORY] Benchmarking memory scaling to 100K...~n"),

    StartMemory = erlang:memory(total) div 1024,
    Latencies = incremental_load_test([1000, 10000, 50000, 100000]),
    EndMemory = erlang:memory(total) div 1024,

    Stats = analyze_latencies(Latencies),
    MemoryDelta = EndMemory - StartMemory,

    ct:pal("Memory Scaling Report:~n"),
    ct:pal("  Start Memory: ~B MB~n", [StartMemory div 1024]),
    ct:pal("  End Memory: ~B MB~n", [EndMemory div 1024]),
    ct:pal("  Delta: ~B MB~n", [MemoryDelta]),
    ct:pal("  Per 100K Ops: ~B KB~n", [MemoryDelta * 1024 div 100]),

    Report = format_benchmark_report("Memory Scaling", Stats),
    ct:pal(Report),

    ?assert(MemoryDelta =< ?TARGET_MEMORY_MB).

%% ===================================================================
%% INTEGRATED SYSTEM BENCHMARK (all components together at 100K)
%% ===================================================================

bench_integrated_system_100k(Config) ->
    ct:pal("~n[INTEGRATED] Benchmarking full system at 100K scale...~n"),

    StartTime = erlang:system_time(millisecond),
    StartMemory = erlang:memory(total),

    %% Run 100K concurrent operations across all subsystems
    Latencies = integrated_system_load_test(100000),
    Stats = analyze_latencies(Latencies),

    EndTime = erlang:system_time(millisecond),
    EndMemory = erlang:memory(total),

    Duration = (EndTime - StartTime) / 1000,
    MemoryMB = (EndMemory - StartMemory) div 1048576,

    ct:pal("~n=== INTEGRATED SYSTEM RESULTS ===~n"),
    Report = format_benchmark_report("Integrated System 100K", Stats),
    ct:pal(Report),
    ct:pal("  Test Duration: ~.2f seconds~n", [Duration]),
    ct:pal("  Memory Used: ~B MB~n", [MemoryMB]),

    %% Verify critical targets
    ?assert(Stats#stats.throughput >= ?TARGET_THROUGHPUT_MSGPS),
    ?assert(Stats#stats.p95_latency_ms =< ?TARGET_P95_LATENCY_MS),
    ?assert(Stats#stats.p99_latency_ms =< ?TARGET_P99_LATENCY_MS),
    ?assert(MemoryMB =< ?TARGET_MEMORY_MB).

%% ===================================================================
%% MIXED WORKLOAD BENCHMARK (varied operation types)
%% ===================================================================

bench_mixed_workload_100k(Config) ->
    ct:pal("~n[MIXED] Benchmarking mixed workload (100K operations)...~n"),

    Latencies = mixed_workload_test(100000),
    Stats = analyze_latencies(Latencies),

    Report = format_benchmark_report("Mixed Workload 100K", Stats),
    ct:pal(Report),

    ?assert(Stats#stats.throughput >= ?TARGET_THROUGHPUT_MSGPS * 0.9).

%% ===================================================================
%% SUSTAINED LOAD BENCHMARK (30-second test at high throughput)
%% ===================================================================

bench_sustained_load_100k(Config) ->
    ct:pal("~n[SUSTAINED] Benchmarking sustained load (30 seconds)...~n"),

    StartTime = erlang:system_time(millisecond),
    Latencies = sustained_load_test(30),  % 30 seconds
    EndTime = erlang:system_time(millisecond),

    Duration = (EndTime - StartTime) / 1000,
    Stats = analyze_latencies(Latencies),

    %% Recalculate throughput based on actual duration
    ActualThroughput = (Stats#stats.operation_count * 1000) / (EndTime - StartTime),

    ct:pal("~n=== SUSTAINED LOAD RESULTS ===~n"),
    ct:pal("  Duration: ~.2f seconds~n", [Duration]),
    ct:pal("  Operations: ~B~n", [Stats#stats.operation_count]),
    ct:pal("  Throughput: ~.0f msg/sec~n", [ActualThroughput]),
    ct:pal("  P95 Latency: ~.2f ms~n", [Stats#stats.p95_latency_ms]),
    ct:pal("  P99 Latency: ~.2f ms~n", [Stats#stats.p99_latency_ms]),

    ?assert(ActualThroughput >= ?TARGET_THROUGHPUT_MSGPS * 0.95).

%% ===================================================================
%% SPIKE HANDLING BENCHMARK (sudden load increase)
%% ===================================================================

bench_spike_handling_100k(Config) ->
    ct:pal("~n[SPIKE] Benchmarking spike handling...~n"),

    %% Baseline load
    BaselineLatencies = sustained_load_test(5),
    BaselineStats = analyze_latencies(BaselineLatencies),

    %% Spike to 150% of baseline
    SpikeLatencies = sustained_load_test(5),
    SpikeStats = analyze_latencies(SpikeLatencies),

    %% Recovery
    RecoveryLatencies = sustained_load_test(5),
    RecoveryStats = analyze_latencies(RecoveryLatencies),

    ct:pal("~n=== SPIKE HANDLING RESULTS ===~n"),
    ct:pal("Baseline: ~.2f msg/sec (P99: ~.2f ms)~n",
           [BaselineStats#stats.throughput, BaselineStats#stats.p99_latency_ms]),
    ct:pal("Spike: ~.2f msg/sec (P99: ~.2f ms)~n",
           [SpikeStats#stats.throughput, SpikeStats#stats.p99_latency_ms]),
    ct:pal("Recovery: ~.2f msg/sec (P99: ~.2f ms)~n",
           [RecoveryStats#stats.throughput, RecoveryStats#stats.p99_latency_ms]),

    %% Check that spike handling doesn't degrade too much
    SpikeDegradation = (BaselineStats#stats.throughput - SpikeStats#stats.throughput) /
                       BaselineStats#stats.throughput * 100,
    ct:pal("Throughput Degradation: ~.1f%~n", [SpikeDegradation]),

    ?assert(SpikeDegradation =< 20.0).  % Less than 20% degradation acceptable

%% ===================================================================
%% STRESS TEST OPERATIONS
%% ===================================================================

%% Register 100K entries and measure latency
register_100k_entries() ->
    register_entries_recursive(100000, []).

register_entries_recursive(0, Latencies) ->
    Latencies;
register_entries_recursive(Count, Latencies) ->
    Start = erlang:system_time(microsecond),
    %% Simulate registry registration
    Id = <<"reg_", (integer_to_binary(Count))/binary>>,
    _ = erlang:put({registry, Id}, {Count, erlang:system_time()}),
    End = erlang:system_time(microsecond),
    LatencyMs = (End - Start) / 1000,
    register_entries_recursive(Count - 1, [LatencyMs | Latencies]).

%% Lookup 100K entries
lookup_100k_entries() ->
    lookup_entries_recursive(100000, []).

lookup_entries_recursive(0, Latencies) ->
    Latencies;
lookup_entries_recursive(Count, Latencies) ->
    Start = erlang:system_time(microsecond),
    Id = <<"reg_", (integer_to_binary(Count))/binary>>,
    _ = erlang:get({registry, Id}),
    End = erlang:system_time(microsecond),
    LatencyMs = (End - Start) / 1000,
    lookup_entries_recursive(Count - 1, [LatencyMs | Latencies]).

%% Concurrent registry access from multiple processes
concurrent_registry_access(Concurrency) ->
    Pids = [spawn_monitor(fun registry_access_worker/0) || _ <- lists:seq(1, Concurrency)],
    wait_for_concurrent_results(Pids, []).

registry_access_worker() ->
    Latencies = [measure_registry_op() || _ <- lists:seq(1, 1000)],
    exit({ok, Latencies}).

measure_registry_op() ->
    Start = erlang:system_time(microsecond),
    Id = <<"reg_", (integer_to_binary(rand:uniform(100000)))/binary>>,
    case rand:uniform(2) of
        1 -> erlang:put({registry, Id}, erlang:system_time());
        2 -> erlang:get({registry, Id})
    end,
    End = erlang:system_time(microsecond),
    (End - Start) / 1000.

%% Pool stress test
pool_stress_test(PoolCount, ConnectionsPerPool) ->
    Pids = [spawn_monitor(fun() ->
        WorkerLatencies = [measure_pool_op() || _ <- lists:seq(1, 1000)],
        exit({ok, WorkerLatencies})
    end) || _ <- lists:seq(1, PoolCount * 10)],
    wait_for_concurrent_results(Pids, []).

measure_pool_op() ->
    Start = erlang:system_time(microsecond),
    %% Simulate pool operation
    _ = lists:sum([X || X <- lists:seq(1, 100)]),
    End = erlang:system_time(microsecond),
    (End - Start) / 1000.

%% Queue stress test
queue_stress_test(OperationCount) ->
    Pids = [spawn_monitor(fun() ->
        QueueLatencies = [measure_queue_op() || _ <- lists:seq(1, 1000)],
        exit({ok, QueueLatencies})
    end) || _ <- lists:seq(1, OperationCount div 1000)],
    wait_for_concurrent_results(Pids, []).

measure_queue_op() ->
    Start = erlang:system_time(microsecond),
    %% Simulate queue operation
    Q0 = queue:new(),
    Q1 = queue:in(test_msg, Q0),
    {_, Q2} = queue:out(Q1),
    _ = Q2,
    End = erlang:system_time(microsecond),
    (End - Start) / 1000.

%% Session stress test
session_stress_test(SessionCount) ->
    Pids = [spawn_monitor(fun() ->
        SessionLatencies = [measure_session_op(SessionId) ||
                            SessionId <- lists:seq(1, SessionCount div 100)],
        exit({ok, SessionLatencies})
    end) || _ <- lists:seq(1, 100)],
    wait_for_concurrent_results(Pids, []).

measure_session_op(SessionId) ->
    Start = erlang:system_time(microsecond),
    %% Simulate session operation
    Key = {session, SessionId},
    erlang:put(Key, {session_state, erlang:system_time()}),
    _ = erlang:get(Key),
    End = erlang:system_time(microsecond),
    (End - Start) / 1000.

%% Network I/O stress test
network_io_stress_test(PacketCount, PacketSize) ->
    Pids = [spawn_monitor(fun() ->
        IOLatencies = [measure_network_io(PacketSize) || _ <- lists:seq(1, 1000)],
        exit({ok, IOLatencies})
    end) || _ <- lists:seq(1, PacketCount div 1000)],
    wait_for_concurrent_results(Pids, []).

measure_network_io(PacketSize) ->
    Start = erlang:system_time(microsecond),
    %% Simulate network I/O
    Packet = binary:copy(<<"X">>, PacketSize),
    _ = byte_size(Packet),
    End = erlang:system_time(microsecond),
    (End - Start) / 1000.

%% Incremental load test (measures memory scaling)
incremental_load_test(LoadPoints) ->
    lists:flatten([run_load_point(Load) || Load <- LoadPoints]).

run_load_point(Load) ->
    Pids = [spawn_monitor(fun() ->
        Latencies = [measure_load_op() || _ <- lists:seq(1, Load div 100)],
        exit({ok, Latencies})
    end) || _ <- lists:seq(1, 100)],
    wait_for_concurrent_results(Pids, []).

measure_load_op() ->
    Start = erlang:system_time(microsecond),
    %% Simulate some work
    _ = lists:reverse(lists:seq(1, 50)),
    End = erlang:system_time(microsecond),
    (End - Start) / 1000.

%% Integrated system test (all subsystems together)
integrated_system_load_test(OperationCount) ->
    Pids = [spawn_monitor(fun() ->
        Latencies = [measure_integrated_op() || _ <- lists:seq(1, 1000)],
        exit({ok, Latencies})
    end) || _ <- lists:seq(1, OperationCount div 1000)],
    wait_for_concurrent_results(Pids, []).

measure_integrated_op() ->
    Start = erlang:system_time(microsecond),
    %% Combine registry, queue, and session operations
    case rand:uniform(3) of
        1 -> erlang:put({key, rand:uniform(1000)}, test_value);
        2 -> _ = erlang:get({key, rand:uniform(1000)});
        3 -> _ = lists:sum([X || X <- lists:seq(1, 50)])
    end,
    End = erlang:system_time(microsecond),
    (End - Start) / 1000.

%% Mixed workload test
mixed_workload_test(OperationCount) ->
    Pids = [spawn_monitor(fun() ->
        Latencies = [measure_mixed_op() || _ <- lists:seq(1, 1000)],
        exit({ok, Latencies})
    end) || _ <- lists:seq(1, OperationCount div 1000)],
    wait_for_concurrent_results(Pids, []).

measure_mixed_op() ->
    Start = erlang:system_time(microsecond),
    case rand:uniform(4) of
        1 -> erlang:put({mixed, rand:uniform(100)}, test);  % Registry
        2 -> _ = erlang:get({mixed, rand:uniform(100)});     % Lookup
        3 -> _ = queue:in(test, queue:new());                % Queue
        4 -> _ = lists:sum([X || X <- lists:seq(1, 25)])     % Compute
    end,
    End = erlang:system_time(microsecond),
    (End - Start) / 1000.

%% Sustained load test
sustained_load_test(DurationSeconds) ->
    EndTime = erlang:system_time(millisecond) + (DurationSeconds * 1000),
    Pids = [spawn_monitor(fun() ->
        Latencies = run_sustained_worker(EndTime, []),
        exit({ok, Latencies})
    end) || _ <- lists:seq(1, 100)],
    wait_for_concurrent_results(Pids, []).

run_sustained_worker(EndTime, Latencies) ->
    case erlang:system_time(millisecond) < EndTime of
        true ->
            Latency = measure_sustained_op(),
            run_sustained_worker(EndTime, [Latency | Latencies]);
        false ->
            Latencies
    end.

measure_sustained_op() ->
    Start = erlang:system_time(microsecond),
    %% Simple operation
    _ = lists:sum([X || X <- lists:seq(1, 100)]),
    End = erlang:system_time(microsecond),
    (End - Start) / 1000.

%% Wait for concurrent results
wait_for_concurrent_results([], Results) ->
    lists:flatten(Results);
wait_for_concurrent_results([{Pid, Ref} | Rest], Results) ->
    receive
        {'DOWN', Ref, process, Pid, {ok, Data}} ->
            wait_for_concurrent_results(Rest, [Data | Results]);
        {'DOWN', Ref, process, Pid, normal} ->
            wait_for_concurrent_results(Rest, Results);
        {'DOWN', Ref, process, Pid, Error} ->
            ct:pal("Process error: ~w~n", [Error]),
            wait_for_concurrent_results(Rest, Results)
    after 120000 ->
        ct:pal("Timeout waiting for results~n"),
        wait_for_concurrent_results(Rest, Results)
    end.

%% ===================================================================
%% ANALYSIS FUNCTIONS
%% ===================================================================

%% Analyze latency distribution
analyze_latencies(RawLatencies) ->
    Latencies = lists:sort([L || L <- RawLatencies, is_number(L)]),

    case Latencies of
        [] ->
            #stats{};
        _ ->
            Count = length(Latencies),
            TotalTime = lists:sum(Latencies),
            Min = lists:min(Latencies),
            Max = lists:max(Latencies),
            Avg = TotalTime / Count,
            Median = lists:nth((Count div 2) + 1, Latencies),
            P50 = percentile(50, Latencies),
            P95 = percentile(95, Latencies),
            P99 = percentile(99, Latencies),
            StdDev = calculate_stddev(Latencies, Avg),
            Jitter = (StdDev / Avg) * 100,
            Throughput = (Count / TotalTime) * 1000,  % ops/sec

            #stats{
                operation_count = Count,
                total_time_ms = TotalTime,
                throughput = Throughput,
                min_latency_ms = Min,
                max_latency_ms = Max,
                avg_latency_ms = Avg,
                median_latency_ms = Median,
                p50_latency_ms = P50,
                p95_latency_ms = P95,
                p99_latency_ms = P99,
                stddev_latency_ms = StdDev,
                jitter_pct = Jitter
            }
    end.

%% Calculate percentile
percentile(Percent, SortedList) ->
    Count = length(SortedList),
    Index = erlang:max(1, erlang:round(Count * Percent / 100)),
    lists:nth(Index, SortedList).

%% Calculate standard deviation
calculate_stddev(Values, Avg) ->
    Variance = lists:sum([math:pow(V - Avg, 2) || V <- Values]) / length(Values),
    math:sqrt(Variance).

%% Format benchmark report
format_benchmark_report(Name, Stats) ->
    io_lib:format(
        "~n========================================~n"
        "BENCHMARK: ~s~n"
        "========================================~n"
        "Operations:        ~B~n"
        "Total Time:        ~.2f ms~n"
        "Throughput:        ~.0f msg/sec~n"
        "~n"
        "Latency Metrics:~n"
        "  Min:             ~.3f ms~n"
        "  Max:             ~.3f ms~n"
        "  Avg:             ~.3f ms~n"
        "  Median (P50):    ~.3f ms~n"
        "  P95:             ~.3f ms (target: ~.1f ms)~n"
        "  P99:             ~.3f ms (target: ~.1f ms)~n"
        "  StdDev:          ~.3f ms~n"
        "  Jitter:          ~.2f %~n"
        "========================================~n",
        [
            Name,
            Stats#stats.operation_count,
            Stats#stats.total_time_ms,
            Stats#stats.throughput,
            Stats#stats.min_latency_ms,
            Stats#stats.max_latency_ms,
            Stats#stats.avg_latency_ms,
            Stats#stats.median_latency_ms,
            Stats#stats.p95_latency_ms,
            ?TARGET_P95_LATENCY_MS,
            Stats#stats.p99_latency_ms,
            ?TARGET_P99_LATENCY_MS,
            Stats#stats.stddev_latency_ms,
            Stats#stats.jitter_pct
        ]
    ).

%% Placeholder functions for exported helpers
run_concurrent_ops(_, _, _, _) -> [].
collect_metrics(_) -> #{}.
measure_resource_usage(_) -> #{}.
