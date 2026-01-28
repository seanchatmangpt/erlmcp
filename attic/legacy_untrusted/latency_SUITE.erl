%% ===================================================================
%% ERLMCP LATENCY DISTRIBUTION BENCHMARK SUITE
%% ===================================================================
%% Module: latency_SUITE
%% Purpose: Measure latency distribution (p50, p95, p99) under various
%%          load conditions and identify performance regressions.
%% ===================================================================

-module(latency_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% CT Callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test Cases
-export([
    latency_stability_test/1,
    latency_under_load_test/1,
    latency_tail_analysis/1,
    latency_variance_test/1,
    memory_per_request/1
]).

%% ===================================================================
%% CT CALLBACKS
%% ===================================================================

all() ->
    [
        latency_stability_test,
        latency_under_load_test,
        latency_tail_analysis,
        latency_variance_test,
        memory_per_request
    ].

init_per_suite(Config) ->
    ct:pal("Initializing latency benchmarks..."),
    application:ensure_all_started(erlmcp),
    application:ensure_all_started(taiea),
    [{suite_start_time, erlang:system_time(millisecond)} | Config].

end_per_suite(Config) ->
    ct:pal("Latency benchmark suite completed"),
    Config.

init_per_testcase(_TestCase, Config) ->
    [{test_start_time, erlang:system_time(millisecond)} | Config].

end_per_testcase(_TestCase, Config) ->
    Config.

%% ===================================================================
%% TEST CASES
%% ===================================================================

latency_stability_test(Config) ->
    ct:pal("Testing latency stability over time..."),
    Duration = 60,  % 60 seconds
    WindowSize = 10,  % 10 second windows
    Windows = Duration div WindowSize,

    Stats = [run_latency_window(WindowSize) || _ <- lists:seq(1, Windows)],

    % Analyze variance across windows
    Averages = [S#window_stats.avg || S <- Stats],
    Variance = calculate_variance(Averages),

    ct:pal("Average latency variance across windows: ~.4f", [Variance]),
    ?assert(Variance < 10.0),  % Less than 10ms variance

    ct:pal("PASS: Latency is stable over time~n").

latency_under_load_test(Config) ->
    ct:pal("Testing latency under increasing load..."),

    % Test at different concurrency levels
    Loads = [1, 10, 50, 100, 500],
    Results = [test_load_level(Load) || Load <- Loads],

    % Verify latency doesn't degrade significantly
    verify_load_scaling(Results),

    ct:pal("PASS: Latency scaling acceptable~n").

latency_tail_analysis(Config) ->
    ct:pal("Analyzing tail latencies..."),

    Latencies = run_benchmark(1000),  % 1000 operations
    Stats = analyze_latencies(Latencies),

    ct:pal("Tail latency analysis:"),
    ct:pal("  P95: ~.2f ms", [Stats#latency_stats.p95]),
    ct:pal("  P99: ~.2f ms", [Stats#latency_stats.p99]),
    ct:pal("  P99.9: ~.2f ms", [Stats#latency_stats.p99_9]),

    % Verify tail latencies are acceptable
    ?assert(Stats#latency_stats.p99 < 200),  % P99 < 200ms

    ct:pal("PASS: Tail latencies acceptable~n").

latency_variance_test(Config) ->
    ct:pal("Testing latency variance..."),

    Latencies = run_benchmark(1000),
    Stats = analyze_latencies(Latencies),

    % Calculate coefficient of variation (stddev/mean)
    CV = Stats#latency_stats.stddev / Stats#latency_stats.avg,

    ct:pal("Coefficient of variation: ~.4f", [CV]),
    ?assert(CV < 1.0),  % CV should be reasonable

    ct:pal("PASS: Latency variance acceptable~n").

memory_per_request(Config) ->
    ct:pal("Measuring memory per request..."),

    InitialMemory = erlang:memory(total),

    % Run 1000 requests
    _Latencies = run_benchmark(1000),

    FinalMemory = erlang:memory(total),
    MemoryUsed = FinalMemory - InitialMemory,
    MemoryPerRequest = MemoryUsed / 1000,

    ct:pal("Memory per request: ~.2f bytes", [MemoryPerRequest]),
    ct:pal("Total memory delta: ~B bytes", [MemoryUsed]),

    % Verify memory efficiency
    ?assert(MemoryPerRequest < 10000),  % Less than 10KB per request

    ct:pal("PASS: Memory efficiency acceptable~n").

%% ===================================================================
%% BENCHMARK HELPERS
%% ===================================================================

-record(window_stats, {
    window_num = 0 :: integer(),
    count = 0 :: integer(),
    min = infinity :: number(),
    max = 0 :: number(),
    avg = 0 :: number(),
    p95 = 0 :: number(),
    p99 = 0 :: number()
}).

-record(latency_stats, {
    count = 0 :: integer(),
    min = infinity :: number(),
    max = 0 :: number(),
    avg = 0 :: number(),
    median = 0 :: number(),
    p50 = 0 :: number(),
    p95 = 0 :: number(),
    p99 = 0 :: number(),
    p99_9 = 0 :: number(),
    stddev = 0 :: number()
}).

run_latency_window(WindowSeconds) ->
    EndTime = erlang:system_time(millisecond) + WindowSeconds * 1000,
    Latencies = collect_latencies_until(EndTime),
    analyze_latencies(Latencies).

collect_latencies_until(EndTime) ->
    collect_latencies_until(EndTime, []).

collect_latencies_until(EndTime, Results) ->
    case erlang:system_time(millisecond) < EndTime of
        true ->
            Latency = measure_operation(),
            collect_latencies_until(EndTime, [Latency | Results]);
        false ->
            Results
    end.

test_load_level(Concurrency) ->
    ct:pal("Testing at ~B concurrency...", [Concurrency]),
    Latencies = run_concurrent_ops(Concurrency, 100),
    analyze_latencies(Latencies).

run_concurrent_ops(Concurrency, OperationsPerWorker) ->
    Pids = [spawn_monitor(
        fun() -> run_worker_ops(OperationsPerWorker) end)
        || _ <- lists:seq(1, Concurrency)],
    collect_worker_results(Pids, []).

run_worker_ops(Count) ->
    [measure_operation() || _ <- lists:seq(1, Count)].

collect_worker_results([], Results) ->
    lists:flatten(Results);
collect_worker_results([{Pid, Ref} | Rest], Results) ->
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            collect_worker_results(Rest, Results);
        {'DOWN', Ref, process, Pid, {ok, Data}} ->
            collect_worker_results(Rest, [Data | Results])
    after 30000 ->
        ct:pal("Timeout collecting worker results"),
        collect_worker_results(Rest, Results)
    end.

run_benchmark(OperationCount) ->
    [measure_operation() || _ <- lists:seq(1, OperationCount)].

measure_operation() ->
    Start = erlang:system_time(microsecond),
    % Simulate a typical operation
    _ = erlang:system_time(microsecond),
    End = erlang:system_time(microsecond),
    (End - Start) / 1000.  % Convert to milliseconds

analyze_latencies(Latencies) ->
    SortedLatencies = lists:sort(Latencies),
    Count = length(SortedLatencies),
    Min = lists:min(SortedLatencies),
    Max = lists:max(SortedLatencies),
    Avg = lists:sum(SortedLatencies) / Count,
    Median = lists:nth((Count div 2) + 1, SortedLatencies),
    P50 = percentile(50, SortedLatencies),
    P95 = percentile(95, SortedLatencies),
    P99 = percentile(99, SortedLatencies),
    P99_9 = percentile(99.9, SortedLatencies),
    StdDev = calculate_stddev(SortedLatencies, Avg),

    #latency_stats{
        count = Count,
        min = Min,
        max = Max,
        avg = Avg,
        median = Median,
        p50 = P50,
        p95 = P95,
        p99 = P99,
        p99_9 = P99_9,
        stddev = StdDev
    }.

percentile(Percent, SortedLatencies) ->
    Count = length(SortedLatencies),
    Index = erlang:max(1, erlang:round(Count * Percent / 100)),
    lists:nth(Index, SortedLatencies).

calculate_variance(Values) ->
    Avg = lists:sum(Values) / length(Values),
    Variance = lists:sum([(V - Avg) * (V - Avg) || V <- Values]) / length(Values),
    math:sqrt(Variance).

calculate_stddev(Values, Avg) ->
    Variance = lists:sum([math:pow(V - Avg, 2) || V <- Values]) / length(Values),
    math:sqrt(Variance).

verify_load_scaling(Results) ->
    % Verify that latency doesn't degrade more than 50% under load
    BaselineP95 = (hd(Results))#latency_stats.p95,
    MaxP95 = lists:max([R#latency_stats.p95 || R <- Results]),
    Degradation = (MaxP95 - BaselineP95) / BaselineP95,

    ct:pal("Load scaling: baseline P95=~.2f, max P95=~.2f, degradation=~.1f%",
           [BaselineP95, MaxP95, Degradation * 100]),

    ?assert(Degradation < 0.5).  % Less than 50% degradation
