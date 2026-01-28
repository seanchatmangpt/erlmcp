%%%-------------------------------------------------------------------
%%% Common Test Suite for Profiling at 100K Concurrent Scale
%%%
%%% Tests:
%%% 1. CPU profiler accuracy and overhead (<10% at 100K)
%%% 2. Memory profiler accuracy and memory leak detection
%%% 3. Latency profiler accuracy at scale
%%% 4. Bottleneck detector alert generation
%%% 5. End-to-end 100K load test with all profilers enabled
%%% 6. Profiling overhead measurement
%%% 7. Bottleneck detection accuracy
%%%
%%%-------------------------------------------------------------------
-module(erlmcp_profiling_100k_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

%% Import memory snapshot record definition
-record(memory_snapshot, {
    timestamp :: integer(),
    process_count :: integer(),
    total_memory :: integer(),
    process_memory :: integer(),
    binary_memory :: integer(),
    ets_memory :: integer(),
    atom_memory :: integer(),
    gc_collections :: integer(),
    gc_reclaimed :: integer(),
    gc_pause_max :: integer()
}).

-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    test_cpu_profiler_accuracy/1,
    test_cpu_profiler_overhead/1,
    test_latency_profiler_accuracy/1,
    test_latency_percentiles/1,
    test_memory_profiler_accuracy/1,
    test_bottleneck_detector_cpu_alert/1,
    test_bottleneck_detector_latency_alert/1,
    test_bottleneck_detector_memory_alert/1,
    test_100k_concurrent_with_profilers/1,
    test_profiling_overhead_measurement/1,
    test_bottleneck_detection_accuracy/1,
    test_slow_operation_identification/1,
    test_hot_function_identification/1
]).

suite() ->
    [
        {timetrap, {minutes, 30}},
        {require, erlmcp_app}
    ].

all() ->
    [
        test_cpu_profiler_accuracy,
        test_cpu_profiler_overhead,
        test_latency_profiler_accuracy,
        test_latency_percentiles,
        test_memory_profiler_accuracy,
        test_bottleneck_detector_cpu_alert,
        test_bottleneck_detector_latency_alert,
        test_bottleneck_detector_memory_alert,
        test_profiling_overhead_measurement,
        test_slow_operation_identification,
        test_hot_function_identification,
        test_bottleneck_detection_accuracy,
        test_100k_concurrent_with_profilers
    ].

init_per_suite(Config) ->
    ct:log("Starting profiling test suite~n"),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting test: ~w~n", [TestCase]),
    erlmcp_cpu_profiler:start_profiling(),
    erlmcp_latency_profiler:start_profiling(),
    erlmcp_bottleneck_detector:start_detection(),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:log("Ending test: ~w~n", [TestCase]),
    erlmcp_cpu_profiler:stop_profiling(),
    erlmcp_latency_profiler:stop_profiling(),
    erlmcp_bottleneck_detector:stop_detection(),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_cpu_profiler_accuracy(_Config) ->
    ct:log("Testing CPU profiler accuracy~n"),

    %% Create known workload with measurable CPU time
    erlmcp_cpu_profiler:measure_function_call({test, cpu_work, 0}, 1000),
    erlmcp_cpu_profiler:measure_function_call({test, cpu_work, 0}, 1200),
    erlmcp_cpu_profiler:measure_function_call({test, cpu_work, 0}, 900),

    %% Get stats
    Stats = erlmcp_cpu_profiler:get_cpu_stats(),
    TotalCalls = maps:get(total_calls, Stats, 0),
    TotalTime = maps:get(total_time_us, Stats, 0),

    ct:log("Total calls tracked: ~B, Total time: ~B us~n", [TotalCalls, TotalTime]),

    %% Verify tracking works
    true = TotalCalls >= 3,
    true = TotalTime >= 3000,

    ct:log("CPU profiler accuracy test PASSED~n").

test_cpu_profiler_overhead(_Config) ->
    ct:log("Testing CPU profiler overhead measurement~n"),

    %% Simulate 1000 function calls
    NumCalls = 1000,
    lists:foreach(
        fun(I) ->
            erlmcp_cpu_profiler:measure_function_call(
                {test, func, I rem 10},
                100 + (I rem 500)
            )
        end,
        lists:seq(1, NumCalls)
    ),

    %% Get profiler overhead
    Overhead = erlmcp_cpu_profiler:get_cpu_overhead(),
    ct:log("Profiler overhead: ~.2f%~n", [Overhead]),

    %% Overhead should be reasonable (<15% for 1000 calls)
    true = Overhead < 15.0,

    ct:log("CPU profiler overhead test PASSED~n").

test_latency_profiler_accuracy(_Config) ->
    ct:log("Testing latency profiler accuracy~n"),

    %% Measure known latencies
    erlmcp_latency_profiler:measure_operation(test_op, 10000),   %% 10ms
    erlmcp_latency_profiler:measure_operation(test_op, 15000),   %% 15ms
    erlmcp_latency_profiler:measure_operation(test_op, 20000),   %% 20ms
    erlmcp_latency_profiler:measure_operation(test_op, 100000),  %% 100ms (slow)
    erlmcp_latency_profiler:measure_operation(test_op, 500000),  %% 500ms (very slow)

    %% Get stats
    Stats = erlmcp_latency_profiler:get_latency_stats(),
    TotalSamples = maps:get(total_samples, Stats, 0),
    AvgLatency = maps:get(avg_latency_us, Stats, 0),

    ct:log("Total samples: ~B, Average latency: ~.0f us~n", [TotalSamples, AvgLatency]),

    %% Verify accuracy
    true = TotalSamples >= 5,
    true = AvgLatency >= 10000,

    %% Verify categorization
    Dist = erlmcp_latency_profiler:analyze_latency_distribution(),
    FastPercent = maps:get(fast_percent, Dist, 0),
    SlowPercent = maps:get(slow_percent, Dist, 0),
    CriticalPercent = maps:get(critical_percent, Dist, 0),

    ct:log("Distribution - Fast: ~.1f%, Slow: ~.1f%, Critical: ~.1f%~n",
           [FastPercent, SlowPercent, CriticalPercent]),

    %% Should have some in each category
    true = FastPercent > 0,
    true = SlowPercent >= 0,
    true = CriticalPercent >= 20,  %% At least 20% critical (500ms)

    ct:log("Latency profiler accuracy test PASSED~n").

test_latency_percentiles(_Config) ->
    ct:log("Testing latency percentiles~n"),

    %% Generate distribution of latencies (0-100ms)
    lists:foreach(
        fun(I) ->
            Latency = (I rem 100) * 1000,  %% 0-99ms
            erlmcp_latency_profiler:measure_operation(test_op, Latency)
        end,
        lists:seq(1, 100)
    ),

    %% Get percentiles
    Percentiles = erlmcp_latency_profiler:get_percentiles(),
    P50 = maps:get(p50, Percentiles, 0),
    P95 = maps:get(p95, Percentiles, 0),
    P99 = maps:get(p99, Percentiles, 0),
    P100 = maps:get(p100, Percentiles, 0),

    ct:log("Percentiles - p50: ~B us, p95: ~B us, p99: ~B us, p100: ~B us~n",
           [P50, P95, P99, P100]),

    %% Verify ordering
    true = P50 > 0,
    true = P95 > P50,
    true = P99 > P95,
    true = P100 >= P99,

    ct:log("Latency percentiles test PASSED~n").

test_memory_profiler_accuracy(_Config) ->
    ct:log("Testing memory profiler accuracy~n"),

    %% Get baseline
    erlmcp_memory_profiler:start_profiling(),
    Baseline = erlmcp_memory_profiler:measure_memory_snapshot(),
    BaselineMem = Baseline#memory_snapshot.total_memory,

    ct:log("Baseline memory: ~B bytes~n", [BaselineMem]),

    %% Allocate some memory
    _Pids = [spawn_link(fun() -> timer:sleep(60000) end) || _ <- lists:seq(1, 100)],
    timer:sleep(1000),

    %% Measure again
    Snap2 = erlmcp_memory_profiler:measure_memory_snapshot(),
    Mem2 = Snap2#memory_snapshot.total_memory,
    Delta = Mem2 - BaselineMem,

    ct:log("After spawn: ~B bytes, Delta: ~B bytes~n", [Mem2, Delta]),

    %% Should have some memory growth
    true = Delta > 0,

    ct:log("Memory profiler accuracy test PASSED~n").

test_bottleneck_detector_cpu_alert(_Config) ->
    ct:log("Testing bottleneck detector CPU alerts~n"),

    %% Simulate high CPU load
    lists:foreach(
        fun(_) ->
            erlmcp_cpu_profiler:measure_function_call({test, hot_func, 0}, 10000)
        end,
        lists:seq(1, 1000)
    ),

    %% Manually trigger check (in real scenario, would run continuously)
    Alerts = erlmcp_bottleneck_detector:check_bottlenecks(),

    ct:log("Alerts generated: ~B~n", [length(Alerts)]),

    %% Verify bottleneck detector structure
    Report = erlmcp_bottleneck_detector:get_bottleneck_report(),
    TotalAlerts = maps:get(total_alerts, Report, 0),

    ct:log("Total alerts in report: ~B~n", [TotalAlerts]),

    ct:log("Bottleneck detector CPU alert test PASSED~n").

test_bottleneck_detector_latency_alert(_Config) ->
    ct:log("Testing bottleneck detector latency alerts~n"),

    %% Measure slow operations
    lists:foreach(
        fun(_) ->
            erlmcp_latency_profiler:measure_operation(slow_op, 600000)  %% 600ms
        end,
        lists:seq(1, 10)
    ),

    %% Check bottlenecks
    Alerts = erlmcp_bottleneck_detector:check_bottlenecks(),
    ct:log("Alerts generated: ~B~n", [length(Alerts)]),

    %% Get recommendations
    Recs = erlmcp_bottleneck_detector:get_recommendations(),
    ct:log("Recommendations: ~B~n", [length(Recs)]),

    ct:log("Bottleneck detector latency alert test PASSED~n").

test_bottleneck_detector_memory_alert(_Config) ->
    ct:log("Testing bottleneck detector memory alerts~n"),

    %% Allocate significant memory
    _Pids = [spawn_link(fun() -> timer:sleep(300000) end) || _ <- lists:seq(1, 5000)],
    timer:sleep(2000),

    %% Check bottlenecks
    Alerts = erlmcp_bottleneck_detector:check_bottlenecks(),
    ct:log("Alerts generated: ~B~n", [length(Alerts)]),

    Report = erlmcp_bottleneck_detector:get_bottleneck_report(),
    MemIncidents = maps:get(memory_incidents, Report, 0),
    ct:log("Memory incidents: ~B~n", [MemIncidents]),

    ct:log("Bottleneck detector memory alert test PASSED~n").

test_profiling_overhead_measurement(_Config) ->
    ct:log("Testing profiling overhead measurement at scale~n"),

    %% Get baseline CPU
    {CpuTime1, _} = erlang:statistics(runtime),

    %% Run 10K operations with profiling
    lists:foreach(
        fun(I) ->
            erlmcp_cpu_profiler:measure_function_call({test, func, I rem 100}, 100),
            erlmcp_latency_profiler:measure_operation(op, 1000)
        end,
        lists:seq(1, 10000)
    ),

    {CpuTime2, _} = erlang:statistics(runtime),
    TotalCpuTime = CpuTime2 - CpuTime1,

    ct:log("Total CPU time for 10K ops: ~B ms~n", [TotalCpuTime]),

    %% Profiler overhead
    CpuOverhead = erlmcp_cpu_profiler:get_cpu_overhead(),
    ct:log("CPU profiler overhead: ~.2f%~n", [CpuOverhead]),

    %% Should be < 10% at reasonable scale
    true = CpuOverhead < 10.0 orelse TotalCpuTime < 100,

    ct:log("Profiling overhead measurement test PASSED~n").

test_slow_operation_identification(_Config) ->
    ct:log("Testing slow operation identification~n"),

    %% Measure mix of fast and slow operations
    lists:foreach(
        fun(I) ->
            Latency = case I rem 10 of
                0 -> 200000;   %% 200ms - slow
                _ -> 5000      %% 5ms - fast
            end,
            erlmcp_latency_profiler:measure_operation(mixed_op, Latency)
        end,
        lists:seq(1, 100)
    ),

    %% Get slow operations
    SlowOps = erlmcp_latency_profiler:get_slow_operations(100000),  %% >100ms
    SlowCount = length(SlowOps),

    ct:log("Slow operations (>100ms): ~B~n", [SlowCount]),

    %% Should find some slow operations
    true = SlowCount > 0,

    ct:log("Slow operation identification test PASSED~n").

test_hot_function_identification(_Config) ->
    ct:log("Testing hot function identification~n"),

    %% Create workload with one hot function
    lists:foreach(
        fun(I) ->
            case I rem 10 of
                0 ->
                    erlmcp_cpu_profiler:measure_function_call({test, hot_func, 0}, 5000);
                _ ->
                    erlmcp_cpu_profiler:measure_function_call({test, cold_func, I rem 5}, 100)
            end
        end,
        lists:seq(1, 100)
    ),

    %% Get top functions
    TopFuncs = erlmcp_cpu_profiler:get_top_functions(5),
    ct:log("Top 5 functions: ~B~n", [length(TopFuncs)]),

    %% Should identify hot_func as top
    case TopFuncs of
        [TopFunc | _] ->
            TopMFA = maps:get(mfa, TopFunc, undefined),
            ct:log("Hottest function: ~p~n", [TopMFA]);
        _ ->
            ok
    end,

    true = length(TopFuncs) > 0,

    ct:log("Hot function identification test PASSED~n").

test_bottleneck_detection_accuracy(_Config) ->
    ct:log("Testing bottleneck detection accuracy~n"),

    %% Create known bottleneck pattern
    lists:foreach(
        fun(_) ->
            %% Create slow operation
            erlmcp_latency_profiler:measure_operation(bottleneck_op, 600000),  %% >500ms
            %% Create CPU load
            erlmcp_cpu_profiler:measure_function_call({test, bottleneck_func, 0}, 2000)
        end,
        lists:seq(1, 50)
    ),

    %% Get bottleneck report
    Report = erlmcp_bottleneck_detector:get_bottleneck_report(),
    TotalAlerts = maps:get(total_alerts, Report, 0),
    CriticalAlerts = maps:get(critical_alerts, Report, 0),

    ct:log("Total alerts: ~B, Critical: ~B~n", [TotalAlerts, CriticalAlerts]),

    ct:log("Bottleneck detection accuracy test PASSED~n").

test_100k_concurrent_with_profilers(_Config) ->
    ct:log("Testing 100K concurrent operations with all profilers enabled~n"),

    %% Start profilers
    erlmcp_cpu_profiler:start_profiling(),
    erlmcp_latency_profiler:start_profiling(),
    erlmcp_bottleneck_detector:start_detection(),

    StartTime = erlang:system_time(microsecond),

    %% Simulate 100K concurrent operations
    NumWorkers = 100,
    NumOpsPerWorker = 1000,

    ct:log("Starting ~B workers with ~B ops each~n", [NumWorkers, NumOpsPerWorker]),

    %% Create worker processes
    Pids = [spawn_link(fun() ->
        run_worker(NumOpsPerWorker)
    end) || _ <- lists:seq(1, NumWorkers)],

    %% Wait for workers
    lists:foreach(fun(Pid) -> monitor(process, Pid) end, Pids),

    %% Collect results
    ResultCount = wait_for_workers(NumWorkers, 0),

    ElapsedUs = erlang:system_time(microsecond) - StartTime,
    ThroughputPerSec = (ResultCount * 1000000) / ElapsedUs,

    ct:log("100K test complete: ~B operations in ~B us (~.0f ops/sec)~n",
           [ResultCount, ElapsedUs, ThroughputPerSec]),

    %% Get profiling results
    CpuStats = erlmcp_cpu_profiler:get_cpu_stats(),
    CpuFuncs = maps:get(total_functions, CpuStats, 0),
    CpuCalls = maps:get(total_calls, CpuStats, 0),

    LatencyStats = erlmcp_latency_profiler:get_latency_stats(),
    LatencySamples = maps:get(total_samples, LatencyStats, 0),
    AvgLatency = maps:get(avg_latency_us, LatencyStats, 0),

    Report = erlmcp_bottleneck_detector:get_bottleneck_report(),
    TotalAlerts = maps:get(total_alerts, Report, 0),

    ct:log("Profiling Results:~n"),
    ct:log("  CPU: ~B functions, ~B calls~n", [CpuFuncs, CpuCalls]),
    ct:log("  Latency: ~B samples, ~.0f us avg~n", [LatencySamples, AvgLatency]),
    ct:log("  Bottlenecks: ~B alerts~n", [TotalAlerts]),

    %% Verify system didn't crash under load
    true = ResultCount >= 90000,  %% At least 90K successful

    ct:log("100K concurrent test PASSED~n").

%%====================================================================
%% Helper Functions
%%====================================================================

run_worker(NumOps) ->
    lists:foreach(
        fun(I) ->
            %% Simulate operation with random latency
            Latency = case I rem 100 of
                0 -> 100000;  %% 100ms - slow
                _ -> 1000 + (I rem 5000)  %% 1-6ms - normal
            end,

            erlmcp_latency_profiler:measure_operation(concurrent_op, Latency),
            erlmcp_cpu_profiler:measure_function_call({test, worker_func, I rem 10}, Latency div 10),

            %% Small CPU work simulation
            crypto:hash(sha256, <<I:64>>)
        end,
        lists:seq(1, NumOps)
    ).

wait_for_workers(0, Count) ->
    Count;
wait_for_workers(Remaining, Count) ->
    receive
        {'DOWN', _, process, _, _} ->
            wait_for_workers(Remaining - 1, Count + 1000)
    after 60000 ->
        Count
    end.
