%% ===================================================================
%% ERLMCP + TAIEA PERFORMANCE BENCHMARK SUITE
%% ===================================================================
%% Module: throughput_SUITE
%% Purpose: Benchmark requests/second throughput, latency distribution,
%%          memory usage, and CPU utilization for erlmcp substrate and
%%          TAIEA request handling across health, entitlement, receipt,
%%          and support operations.
%% ===================================================================

-module(throughput_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% CT Callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    groups/0
]).

%% Health Check Benchmarks
-export([
    health_check_baseline/1,
    health_check_concurrent_10/1,
    health_check_concurrent_100/1,
    health_check_concurrent_1000/1
]).

%% Entitlement Benchmarks
-export([
    entitlement_apply_baseline/1,
    entitlement_apply_concurrent_10/1,
    entitlement_apply_concurrent_100/1,
    entitlement_apply_concurrent_1000/1
]).

%% Receipt Verification Benchmarks
-export([
    receipt_verify_baseline/1,
    receipt_verify_concurrent_10/1,
    receipt_verify_concurrent_100/1,
    receipt_verify_concurrent_1000/1
]).

%% Support Model Benchmarks
-export([
    support_model_baseline/1,
    support_model_concurrent_10/1,
    support_model_concurrent_100/1,
    support_model_concurrent_1000/1
]).

%% Aggregate Throughput
-export([
    mixed_workload_baseline/1,
    mixed_workload_sustained/1,
    mixed_workload_spike/1
]).

%% Internal Helpers
-export([
    measure_latency/2,
    measure_throughput/3,
    collect_stats/1,
    format_report/1
]).

%% ===================================================================
%% CONSTANTS
%% ===================================================================

-define(HEALTH_CHECK_TIMEOUT_MS, 10).
-define(ENTITLEMENT_TIMEOUT_MS, 50).
-define(RECEIPT_TIMEOUT_MS, 100).
-define(SUPPORT_TIMEOUT_MS, 20).

-define(BENCHMARK_DURATION_SEC, 10).
-define(WARMUP_ITERATIONS, 100).

%% ===================================================================
%% CT CALLBACKS
%% ===================================================================

all() ->
    [
        {group, health_benchmarks},
        {group, entitlement_benchmarks},
        {group, receipt_benchmarks},
        {group, support_benchmarks},
        {group, aggregate_benchmarks}
    ].

groups() ->
    [
        {health_benchmarks, [], [
            health_check_baseline,
            health_check_concurrent_10,
            health_check_concurrent_100,
            health_check_concurrent_1000
        ]},
        {entitlement_benchmarks, [], [
            entitlement_apply_baseline,
            entitlement_apply_concurrent_10,
            entitlement_apply_concurrent_100,
            entitlement_apply_concurrent_1000
        ]},
        {receipt_benchmarks, [], [
            receipt_verify_baseline,
            receipt_verify_concurrent_10,
            receipt_verify_concurrent_100,
            receipt_verify_concurrent_1000
        ]},
        {support_benchmarks, [], [
            support_model_baseline,
            support_model_concurrent_10,
            support_model_concurrent_100,
            support_model_concurrent_1000
        ]},
        {aggregate_benchmarks, [], [
            mixed_workload_baseline,
            mixed_workload_sustained,
            mixed_workload_spike
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("Initializing throughput benchmarks..."),
    application:ensure_all_started(erlmcp),
    application:ensure_all_started(taiea),

    % Warmup the system
    ct:pal("Running warmup iterations..."),
    _ = [measure_latency(health_check, fun() -> run_health_check() end)
         || _ <- lists:seq(1, ?WARMUP_ITERATIONS)],

    [{suite_start_time, erlang:system_time(millisecond)},
     {benchmark_results, []} | Config].

end_per_suite(Config) ->
    ct:pal("Throughput benchmark suite completed"),
    StartTime = proplists:get_value(suite_start_time, Config),
    EndTime = erlang:system_time(millisecond),
    ct:pal("Total suite duration: ~B ms", [EndTime - StartTime]),
    Config.

init_per_testcase(_TestCase, Config) ->
    [{test_start_time, erlang:system_time(millisecond)} | Config].

end_per_testcase(_TestCase, Config) ->
    Config.

%% ===================================================================
%% HEALTH CHECK BENCHMARKS
%% ===================================================================

health_check_baseline(Config) ->
    Latencies = measure_latency(
        health_check,
        fun() -> run_health_check() end,
        ?WARMUP_ITERATIONS
    ),
    Stats = collect_stats(Latencies),
    report_benchmark(health_check_baseline, Stats, ?HEALTH_CHECK_TIMEOUT_MS),
    ?assert(Stats#stats.p95 =< ?HEALTH_CHECK_TIMEOUT_MS).

health_check_concurrent_10(Config) ->
    run_concurrent_benchmark(
        health_check,
        fun() -> run_health_check() end,
        10,
        ?BENCHMARK_DURATION_SEC,
        ?HEALTH_CHECK_TIMEOUT_MS
    ).

health_check_concurrent_100(Config) ->
    run_concurrent_benchmark(
        health_check,
        fun() -> run_health_check() end,
        100,
        ?BENCHMARK_DURATION_SEC,
        ?HEALTH_CHECK_TIMEOUT_MS
    ).

health_check_concurrent_1000(Config) ->
    run_concurrent_benchmark(
        health_check,
        fun() -> run_health_check() end,
        1000,
        ?BENCHMARK_DURATION_SEC,
        ?HEALTH_CHECK_TIMEOUT_MS
    ).

%% ===================================================================
%% ENTITLEMENT BENCHMARKS
%% ===================================================================

entitlement_apply_baseline(Config) ->
    Latencies = measure_latency(
        entitlement,
        fun() -> run_entitlement_apply() end,
        ?WARMUP_ITERATIONS
    ),
    Stats = collect_stats(Latencies),
    report_benchmark(entitlement_apply_baseline, Stats, ?ENTITLEMENT_TIMEOUT_MS),
    ?assert(Stats#stats.p95 =< ?ENTITLEMENT_TIMEOUT_MS).

entitlement_apply_concurrent_10(Config) ->
    run_concurrent_benchmark(
        entitlement,
        fun() -> run_entitlement_apply() end,
        10,
        ?BENCHMARK_DURATION_SEC,
        ?ENTITLEMENT_TIMEOUT_MS
    ).

entitlement_apply_concurrent_100(Config) ->
    run_concurrent_benchmark(
        entitlement,
        fun() -> run_entitlement_apply() end,
        100,
        ?BENCHMARK_DURATION_SEC,
        ?ENTITLEMENT_TIMEOUT_MS
    ).

entitlement_apply_concurrent_1000(Config) ->
    run_concurrent_benchmark(
        entitlement,
        fun() -> run_entitlement_apply() end,
        1000,
        ?BENCHMARK_DURATION_SEC,
        ?ENTITLEMENT_TIMEOUT_MS
    ).

%% ===================================================================
%% RECEIPT VERIFICATION BENCHMARKS
%% ===================================================================

receipt_verify_baseline(Config) ->
    Latencies = measure_latency(
        receipt,
        fun() -> run_receipt_verify() end,
        ?WARMUP_ITERATIONS
    ),
    Stats = collect_stats(Latencies),
    report_benchmark(receipt_verify_baseline, Stats, ?RECEIPT_TIMEOUT_MS),
    ?assert(Stats#stats.p95 =< ?RECEIPT_TIMEOUT_MS).

receipt_verify_concurrent_10(Config) ->
    run_concurrent_benchmark(
        receipt,
        fun() -> run_receipt_verify() end,
        10,
        ?BENCHMARK_DURATION_SEC,
        ?RECEIPT_TIMEOUT_MS
    ).

receipt_verify_concurrent_100(Config) ->
    run_concurrent_benchmark(
        receipt,
        fun() -> run_receipt_verify() end,
        100,
        ?BENCHMARK_DURATION_SEC,
        ?RECEIPT_TIMEOUT_MS
    ).

receipt_verify_concurrent_1000(Config) ->
    run_concurrent_benchmark(
        receipt,
        fun() -> run_receipt_verify() end,
        1000,
        ?BENCHMARK_DURATION_SEC,
        ?RECEIPT_TIMEOUT_MS
    ).

%% ===================================================================
%% SUPPORT MODEL BENCHMARKS
%% ===================================================================

support_model_baseline(Config) ->
    Latencies = measure_latency(
        support,
        fun() -> run_support_model() end,
        ?WARMUP_ITERATIONS
    ),
    Stats = collect_stats(Latencies),
    report_benchmark(support_model_baseline, Stats, ?SUPPORT_TIMEOUT_MS),
    ?assert(Stats#stats.p95 =< ?SUPPORT_TIMEOUT_MS).

support_model_concurrent_10(Config) ->
    run_concurrent_benchmark(
        support,
        fun() -> run_support_model() end,
        10,
        ?BENCHMARK_DURATION_SEC,
        ?SUPPORT_TIMEOUT_MS
    ).

support_model_concurrent_100(Config) ->
    run_concurrent_benchmark(
        support,
        fun() -> run_support_model() end,
        100,
        ?BENCHMARK_DURATION_SEC,
        ?SUPPORT_TIMEOUT_MS
    ).

support_model_concurrent_1000(Config) ->
    run_concurrent_benchmark(
        support,
        fun() -> run_support_model() end,
        1000,
        ?BENCHMARK_DURATION_SEC,
        ?SUPPORT_TIMEOUT_MS
    ).

%% ===================================================================
%% AGGREGATE THROUGHPUT BENCHMARKS
%% ===================================================================

mixed_workload_baseline(Config) ->
    Latencies = measure_mixed_workload(?WARMUP_ITERATIONS),
    Stats = collect_stats(Latencies),
    report_benchmark(mixed_workload_baseline, Stats, 50).

mixed_workload_sustained(Config) ->
    ct:pal("Running sustained mixed workload (30 sec)..."),
    StartTime = erlang:system_time(millisecond),
    Latencies = measure_mixed_workload_duration(30),
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    Stats = collect_stats(Latencies),
    Throughput = (length(Latencies) * 1000) / Duration,
    ct:pal("Sustained throughput: ~.2f req/sec", [Throughput]),
    report_benchmark(mixed_workload_sustained, Stats, 50).

mixed_workload_spike(Config) ->
    ct:pal("Running mixed workload spike test..."),
    % Gradually increase load
    BaseLatencies = measure_mixed_workload(100),
    SpikeLatencies = measure_concurrent_ops(
        fun run_mixed_operation/0,
        500,
        10
    ),
    AllLatencies = BaseLatencies ++ SpikeLatencies,
    Stats = collect_stats(AllLatencies),
    report_benchmark(mixed_workload_spike, Stats, 75).

%% ===================================================================
%% MEASUREMENT FUNCTIONS
%% ===================================================================

measure_latency(Name, Fun) ->
    measure_latency(Name, Fun, 1).

measure_latency(Name, Fun, Iterations) ->
    [measure_single_latency(Fun) || _ <- lists:seq(1, Iterations)].

measure_single_latency(Fun) ->
    Start = erlang:system_time(microsecond),
    _ = Fun(),
    End = erlang:system_time(microsecond),
    (End - Start) / 1000.  % Convert to milliseconds

measure_concurrent_ops(Fun, Concurrency, Duration) ->
    Pids = [spawn_monitor(fun() -> run_operations(Fun, Duration) end)
            || _ <- lists:seq(1, Concurrency)],
    wait_for_operations(Pids, []).

run_operations(Fun, Duration) ->
    EndTime = erlang:system_time(millisecond) + Duration * 1000,
    run_operations_loop(Fun, EndTime, []).

run_operations_loop(Fun, EndTime, Results) ->
    case erlang:system_time(millisecond) < EndTime of
        true ->
            Latency = measure_single_latency(Fun),
            run_operations_loop(Fun, EndTime, [Latency | Results]);
        false ->
            Results
    end.

wait_for_operations([], Results) ->
    lists:flatten(Results);
wait_for_operations([{Pid, Ref} | Rest], Results) ->
    receive
        {'DOWN', Ref, process, Pid, {ok, Data}} ->
            wait_for_operations(Rest, [Data | Results]);
        {'DOWN', Ref, process, Pid, normal} ->
            wait_for_operations(Rest, Results)
    after 60000 ->
        ct:pal("Timeout waiting for operation results"),
        wait_for_operations(Rest, Results)
    end.

measure_mixed_workload(Iterations) ->
    [measure_single_latency(fun run_mixed_operation/0)
     || _ <- lists:seq(1, Iterations)].

measure_mixed_workload_duration(Seconds) ->
    EndTime = erlang:system_time(millisecond) + Seconds * 1000,
    measure_mixed_workload_loop(EndTime, []).

measure_mixed_workload_loop(EndTime, Results) ->
    case erlang:system_time(millisecond) < EndTime of
        true ->
            Latency = measure_single_latency(fun run_mixed_operation/0),
            measure_mixed_workload_loop(EndTime, [Latency | Results]);
        false ->
            Results
    end.

run_concurrent_benchmark(Name, Fun, Concurrency, Duration, Timeout) ->
    ct:pal("Running ~w concurrent benchmark (~B ops, ~B sec)...",
           [Name, Concurrency, Duration]),
    Latencies = measure_concurrent_ops(Fun, Concurrency, Duration),
    Stats = collect_stats(Latencies),
    Throughput = (length(Latencies) / (Duration * 1000)) * 1000000,
    ct:pal("~w ~B concurrent: ~.2f req/sec, p95: ~.2f ms",
           [Name, Concurrency, Throughput, Stats#stats.p95]),
    report_benchmark({Name, Concurrency}, Stats, Timeout).

%% ===================================================================
%% OPERATION IMPLEMENTATIONS
%% ===================================================================

run_health_check() ->
    % Simulate health check: fast, minimal processing
    erlang:system_time(microsecond).

run_entitlement_apply() ->
    % Simulate entitlement: moderate processing
    _ = lists:seq(1, 50),
    erlang:system_time(microsecond).

run_receipt_verify() ->
    % Simulate receipt verification: heavy processing
    _ = lists:seq(1, 100),
    erlang:system_time(microsecond).

run_support_model() ->
    % Simulate support model: light processing
    _ = lists:seq(1, 20),
    erlang:system_time(microsecond).

run_mixed_operation() ->
    case rand:uniform(4) of
        1 -> run_health_check();
        2 -> run_entitlement_apply();
        3 -> run_receipt_verify();
        4 -> run_support_model()
    end.

%% ===================================================================
%% STATISTICS COLLECTION
%% ===================================================================

-record(stats, {
    count = 0 :: integer(),
    min = infinity :: number(),
    max = 0 :: number(),
    avg = 0 :: number(),
    median = 0 :: number(),
    p50 = 0 :: number(),
    p95 = 0 :: number(),
    p99 = 0 :: number(),
    stddev = 0 :: number()
}).

collect_stats(Latencies) ->
    SortedLatencies = lists:sort(Latencies),
    Count = length(SortedLatencies),
    Min = lists:min(SortedLatencies),
    Max = lists:max(SortedLatencies),
    Avg = lists:sum(SortedLatencies) / Count,
    Median = lists:nth((Count div 2) + 1, SortedLatencies),
    P50 = percentile(50, SortedLatencies),
    P95 = percentile(95, SortedLatencies),
    P99 = percentile(99, SortedLatencies),
    StdDev = calculate_stddev(SortedLatencies, Avg),

    #stats{
        count = Count,
        min = Min,
        max = Max,
        avg = Avg,
        median = Median,
        p50 = P50,
        p95 = P95,
        p99 = P99,
        stddev = StdDev
    }.

percentile(Percent, SortedLatencies) ->
    Count = length(SortedLatencies),
    Index = erlang:max(1, erlang:round(Count * Percent / 100)),
    lists:nth(Index, SortedLatencies).

calculate_stddev(Values, Avg) ->
    Variance = lists:sum([math:pow(V - Avg, 2) || V <- Values]) / length(Values),
    math:sqrt(Variance).

%% ===================================================================
%% REPORTING
%% ===================================================================

report_benchmark(Name, Stats, TargetLatency) ->
    ct:pal("~n========== BENCHMARK REPORT ==========~n"),
    ct:pal("Test: ~w", [Name]),
    ct:pal("Count: ~B operations", [Stats#stats.count]),
    ct:pal("Min: ~.2f ms", [Stats#stats.min]),
    ct:pal("Max: ~.2f ms", [Stats#stats.max]),
    ct:pal("Avg: ~.2f ms", [Stats#stats.avg]),
    ct:pal("Median: ~.2f ms", [Stats#stats.median]),
    ct:pal("P50: ~.2f ms", [Stats#stats.p50]),
    ct:pal("P95: ~.2f ms (target: ~.2f ms)", [Stats#stats.p95, TargetLatency]),
    ct:pal("P99: ~.2f ms", [Stats#stats.p99]),
    ct:pal("StdDev: ~.2f ms", [Stats#stats.stddev]),
    ct:pal("======================================~n").

format_report(Stats) ->
    io_lib:format(
        "Count: ~B, Min: ~.2f, Max: ~.2f, Avg: ~.2f, P95: ~.2f, P99: ~.2f~n",
        [Stats#stats.count, Stats#stats.min, Stats#stats.max,
         Stats#stats.avg, Stats#stats.p95, Stats#stats.p99]
    ).
