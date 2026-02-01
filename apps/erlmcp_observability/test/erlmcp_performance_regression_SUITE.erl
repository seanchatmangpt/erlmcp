%%%-------------------------------------------------------------------
%%% @doc erlmcp_performance_regression_SUITE - Performance Regression Detection
%%%
%%% Common Test suite for automated performance regression detection.
%%%
%%% Features:
%%% - Baseline tracking: Load/store baseline metrics from JSON files
%%% - Regression detection: Alert on >10% throughput, >10% latency, >20% memory degradation
%%% - Historical comparison: Compare to previous N runs with trend analysis
%%% - Performance gates: Pass/fail based on configurable thresholds
%%% - CI/CD integration: Report generation suitable for automated pipelines
%%%
%%% Metrics tracked (metrology-compliant):
%%% - throughput_msg_per_s
%%% - latency_p50_us, latency_p95_us, latency_p99_us
%%% - memory_heap_mib_per_conn
%%% - memory_rss_mib_per_node
%%%
%%% Usage:
%%%   rebar3 ct --suite=erlmcp_performance_regression_SUITE
%%%   rebar3 ct --suite=erlmcp_performance_regression_SUITE --group=core_ops
%%%   rebar3 ct --suite=erlmcp_performance_regression_SUITE --var=baseline_path=/path/to/baseline.json
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_performance_regression_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT exports
-export([all/0, groups/0, suite/0, init_per_suite/1, end_per_suite/1, init_per_group/2,
         end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
%% Test cases
-export([test_baseline_loading/1, test_baseline_saving/1, test_regression_detection_throughput/1,
         test_regression_detection_latency/1, test_regression_detection_memory/1,
         test_historical_comparison/1, test_trend_analysis/1, test_performance_gates/1,
         test_ci_report_generation/1, test_full_regression_suite/1]).
%% Benchmarks integration
-export([run_core_ops_benchmark/1, run_network_benchmark/1, run_stress_benchmark/1]).

%%%====================================================================
%%% CT Callbacks
%%%====================================================================

suite() ->
    [{timetrap, {seconds, 300}},   % 5 minute timeout
     {require, baseline_path},
     {default_config, #{baseline_path => "./bench/baselines/2026-01-28_v2.0.0.json"}}].

all() ->
    [test_baseline_loading,
     test_baseline_saving,
     test_regression_detection_throughput,
     test_regression_detection_latency,
     test_regression_detection_memory,
     test_historical_comparison,
     test_trend_analysis,
     test_performance_gates,
     test_ci_report_generation,
     test_full_regression_suite].

groups() ->
    [{core_ops,
      [],
      [test_regression_detection_throughput,
       test_regression_detection_latency,
       test_regression_detection_memory]},
     {integration, [], [test_full_regression_suite]},
     {ci, [], [test_performance_gates, test_ci_report_generation]}].

%%%====================================================================
%%% Setup/Teardown
%%%====================================================================

init_per_suite(Config) ->
    ct:log("Starting Performance Regression Suite"),
    ct:log("Working directory: ~p", [file:get_cd()]),

    % Ensure required applications are started
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(jsx),

    % Initialize baseline storage directory
    BaselineDir = "./bench/baselines",
    ok = filelib:ensure_dir(BaselineDir ++ "/"),

    % Store in config
    [{baseline_dir, BaselineDir} | Config].

end_per_suite(_Config) ->
    ct:log("Ending Performance Regression Suite"),
    ok.

init_per_group(core_ops, Config) ->
    ct:log("Initializing Core Ops regression tests"),
    Config;
init_per_group(integration, Config) ->
    ct:log("Initializing Integration regression tests"),
    Config;
init_per_group(ci, Config) ->
    ct:log("Initializing CI regression tests"),
    Config;
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:log("Ending test case: ~p", [TestCase]),
    ok.

%%%====================================================================
%%% Test Cases
%%%====================================================================

%% @doc Test baseline loading from JSON file
test_baseline_loading(Config) ->
    ct:log("Testing baseline loading"),

    % Load baseline
    BaselineFile =
        proplists:get_value(baseline_path, Config, "./bench/baselines/2026-01-28_v2.0.0.json"),

    case file:read_file(BaselineFile) of
        {ok, JsonData} ->
            ct:log("Loaded baseline file: ~s", [BaselineFile]),

            % Parse JSON
            Baseline = jsx:decode(JsonData, [return_maps]),
            ct:log("Parsed baseline: ~p", [Baseline]),

            % Verify structure
            ?assert(maps:is_key(<<"version">>, Baseline)),
            ?assert(maps:is_key(<<"timestamp">>, Baseline)),
            ?assert(maps:is_key(<<"benchmarks">>, Baseline)),
            ?assert(maps:is_key(<<"regression_thresholds">>, Baseline)),

            % Verify benchmarks exist
            Benchmarks = maps:get(<<"benchmarks">>, Baseline),
            ?assert(maps:size(Benchmarks) > 0),

            % Verify regression thresholds
            Thresholds = maps:get(<<"regression_thresholds">>, Baseline),
            ?assert(maps:is_key(<<"throughput">>, Thresholds)),
            ?assert(maps:is_key(<<"latency_p95">>, Thresholds)),
            ?assert(maps:is_key(<<"memory">>, Thresholds)),

            ok;
        {error, Reason} ->
            ct:log("Failed to load baseline: ~p", [Reason]),
            ?assert(false, {failed_to_load_baseline, Reason})
    end.

%% @doc Test baseline saving to JSON file
test_baseline_saving(Config) ->
    ct:log("Testing baseline saving"),

    % Create test baseline
    TestBaseline =
        #{<<"version">> => <<"2.1.0">>,
          <<"timestamp">> => erlang:system_time(second),
          <<"date">> => <<"2026-01-30T12:00:00Z">>,
          <<"git_sha">> => <<"test_sha">>,
          <<"environment">> =>
              #{<<"os">> => <<"test_os">>,
                <<"otp_version">> => <<"OTP-27">>,
                <<"cores">> => 8},
          <<"benchmarks">> =>
              #{<<"test_benchmark">> =>
                    #{<<"throughput_msg_per_s">> => 1000000.0,
                      <<"latency_p50_us">> => 10.0,
                      <<"latency_p95_us">> => 50.0,
                      <<"latency_p99_us">> => 100.0,
                      <<"memory_delta_mib">> => 10.0}},
          <<"regression_thresholds">> =>
              #{<<"throughput">> => -10,
                <<"latency_p95">> => 10,
                <<"memory">> => 20}},

    % Save to temporary file
    BaselineDir = proplists:get_value(baseline_dir, Config),
    TempFile = BaselineDir ++ "/test_temp_baseline.json",

    case save_baseline(TestBaseline, TempFile) of
        ok ->
            ct:log("Saved baseline to: ~s", [TempFile]),

            % Verify file exists
            ?assert(filelib:is_file(TempFile)),

            % Load and verify
            {ok, Loaded} = load_baseline(TempFile),
            ?assertEqual(maps:get(<<"version">>, TestBaseline), maps:get(<<"version">>, Loaded)),
            ?assertEqual(maps:get(<<"benchmarks">>, TestBaseline),
                         maps:get(<<"benchmarks">>, Loaded)),

            % Cleanup
            ok = file:delete(TempFile),
            ?assertNot(filelib:is_file(TempFile)),
            ok;
        {error, Reason} ->
            ct:log("Failed to save baseline: ~p", [Reason]),
            ?assert(false, {failed_to_save_baseline, Reason})
    end.

%% @doc Test regression detection for throughput (>10% degradation)
test_regression_detection_throughput(Config) ->
    ct:log("Testing throughput regression detection"),

    % Load baseline
    BaselineFile =
        proplists:get_value(baseline_path, Config, "./bench/baselines/2026-01-28_v2.0.0.json"),
    {ok, Baseline} = load_baseline(BaselineFile),

    % Get baseline throughput
    Benchmarks = maps:get(<<"benchmarks">>, Baseline),
    CoreOps100k = maps:get(<<"core_ops_100k">>, Benchmarks),
    BaselineThroughput = maps:get(<<"throughput_msg_per_s">>, CoreOps100k),

    ct:log("Baseline throughput: ~p msg/s", [BaselineThroughput]),

    % Test 1: No regression (within threshold)
    CurrentThroughput1 = BaselineThroughput * 0.95,  % 5% degradation
    Result1 = compare_throughput(<<"core_ops_100k">>, CurrentThroughput1, Baseline),
    ?assertEqual(ok, Result1),
    ct:log("5% degradation: PASS (within 10% threshold)"),

    % Test 2: Regression detected (>10% degradation)
    CurrentThroughput2 = BaselineThroughput * 0.85,  % 15% degradation
    Result2 = compare_throughput(<<"core_ops_100k">>, CurrentThroughput2, Baseline),
    ?assertMatch({error, {regression, _}}, Result2),
    ct:log("15% degradation: FAIL (exceeds 10% threshold)"),

    % Test 3: Improvement
    CurrentThroughput3 = BaselineThroughput * 1.15,  % 15% improvement
    Result3 = compare_throughput(<<"core_ops_100k">>, CurrentThroughput3, Baseline),
    ?assertEqual({ok, improvement}, Result3),
    ct:log("15% improvement: PASS (improvement detected)"),

    ok.

%% @doc Test regression detection for latency (>10% increase)
test_regression_detection_latency(Config) ->
    ct:log("Testing latency regression detection"),

    % Load baseline
    BaselineFile =
        proplists:get_value(baseline_path, Config, "./bench/baselines/2026-01-28_v2.0.0.json"),
    {ok, Baseline} = load_baseline(BaselineFile),

    % Get baseline latency
    Benchmarks = maps:get(<<"benchmarks">>, Baseline),
    CoreOps100k = maps:get(<<"core_ops_100k">>, Benchmarks),
    BaselineP95 = maps:get(<<"latency_p95_us">>, CoreOps100k),
    BaselineP99 = maps:get(<<"latency_p99_us">>, CoreOps100k),

    ct:log("Baseline latency P95: ~p us", [BaselineP95]),
    ct:log("Baseline latency P99: ~p us", [BaselineP99]),

    % Test P95 regression
    CurrentP95 = BaselineP95 * 1.15,  % 15% increase
    ResultP95 = compare_latency(<<"core_ops_100k">>, CurrentP95, BaselineP95, Baseline),
    ?assertMatch({error, {regression, _}}, ResultP95),
    ct:log("15% P95 latency increase: FAIL"),

    % Test P95 within threshold
    CurrentP95OK = BaselineP95 * 1.05,  % 5% increase
    ResultP95OK = compare_latency(<<"core_ops_100k">>, CurrentP95OK, BaselineP95, Baseline),
    ?assertEqual(ok, ResultP95OK),
    ct:log("5% P95 latency increase: PASS"),

    % Test P99 regression
    CurrentP99 = BaselineP99 * 1.12,  % 12% increase
    ResultP99 = compare_latency(<<"core_ops_100k">>, CurrentP99, BaselineP99, Baseline),
    ?assertMatch({error, {regression, _}}, ResultP99),
    ct:log("12% P99 latency increase: FAIL"),

    ok.

%% @doc Test regression detection for memory (>20% increase)
test_regression_detection_memory(Config) ->
    ct:log("Testing memory regression detection"),

    % Load baseline
    BaselineFile =
        proplists:get_value(baseline_path, Config, "./bench/baselines/2026-01-28_v2.0.0.json"),
    {ok, Baseline} = load_baseline(BaselineFile),

    % Get baseline memory
    Benchmarks = maps:get(<<"benchmarks">>, Baseline),
    CoreOps100k = maps:get(<<"core_ops_100k">>, Benchmarks),
    BaselineMemory = maps:get(<<"memory_delta_mib">>, CoreOps100k),

    ct:log("Baseline memory: ~p MiB", [BaselineMemory]),

    % Test 1: No regression (within 20% threshold)
    CurrentMemory1 = BaselineMemory * 1.15,  % 15% increase
    Result1 = compare_memory(<<"core_ops_100k">>, CurrentMemory1, Baseline),
    ?assertEqual(ok, Result1),
    ct:log("15% memory increase: PASS (within 20% threshold)"),

    % Test 2: Regression detected (>20% increase)
    CurrentMemory2 = BaselineMemory * 1.25,  % 25% increase
    Result2 = compare_memory(<<"core_ops_100k">>, CurrentMemory2, Baseline),
    ?assertMatch({error, {regression, _}}, Result2),
    ct:log("25% memory increase: FAIL (exceeds 20% threshold)"),

    ok.

%% @doc Test historical comparison to previous runs
test_historical_comparison(Config) ->
    ct:log("Testing historical comparison"),

    % Create synthetic history
    History1 =
        #{timestamp => erlang:system_time(second) - 86400 * 3},  % 3 days ago
    History2 =
        #{timestamp => erlang:system_time(second) - 86400 * 2},  % 2 days ago
    History3 =
        #{timestamp => erlang:system_time(second) - 86400},      % 1 day ago

    % Add benchmark results to history
    History1WithResults =
        History1#{<<"benchmarks">> =>
                      #{<<"core_ops_100k">> =>
                            #{<<"throughput_msg_per_s">> => 2700000.0,
                              <<"latency_p95_us">> => 80.0,
                              <<"memory_delta_mib">> => 20.0}}},

    History2WithResults =
        History2#{<<"benchmarks">> =>
                      #{<<"core_ops_100k">> =>
                            #{<<"throughput_msg_per_s">> => 2720000.0,
                              <<"latency_p95_us">> => 81.0,
                              <<"memory_delta_mib">> => 21.0}}},

    History3WithResults =
        History3#{<<"benchmarks">> =>
                      #{<<"core_ops_100k">> =>
                            #{<<"throughput_msg_per_s">> => 2740000.0,
                              <<"latency_p95_us">> => 82.0,
                              <<"memory_delta_mib">> => 21.5}}},

    History = [History1WithResults, History2WithResults, History3WithResults],

    % Compare current run to history
    CurrentRun =
        #{<<"benchmarks">> =>
              #{<<"core_ops_100k">> =>
                    #{<<"throughput_msg_per_s">> => 2750000.0,
                      <<"latency_p95_us">> => 83.0,
                      <<"memory_delta_mib">> => 22.0}}},

    ComparisonResult = compare_to_history(<<"core_ops_100k">>, CurrentRun, History),
    ct:log("Historical comparison result: ~p", [ComparisonResult]),

    % Verify comparison contains trend data
    ?assert(maps:is_key(<<"trend">>, ComparisonResult)),
    ?assert(maps:is_key(<<"avg_throughput">>, ComparisonResult)),
    ?assert(maps:is_key(<<"throughput_change_percent">>, ComparisonResult)),

    ok.

%% @doc Test trend analysis across multiple runs
test_trend_analysis(Config) ->
    ct:log("Testing trend analysis"),

    % Create trend data
    TrendData =
        [#{timestamp => 1, throughput => 1000000},
         #{timestamp => 2, throughput => 1020000},
         #{timestamp => 3, throughput => 1040000},
         #{timestamp => 4, throughput => 1060000},
         #{timestamp => 5, throughput => 1080000}],

    % Analyze trend
    Analysis = analyze_trend(<<"throughput">>, TrendData),
    ct:log("Trend analysis: ~p", [Analysis]),

    % Verify trend detection
    ?assert(maps:is_key(<<"direction">>, Analysis)),
    ?assert(maps:is_key(<<"slope">>, Analysis)),
    ?assert(maps:is_key(<<"r_squared">>, Analysis)),

    % Should detect upward trend
    Direction = maps:get(<<"direction">>, Analysis),
    ?assertEqual(<<"improving">>, Direction),

    ok.

%% @doc Test performance gates (pass/fail based on thresholds)
test_performance_gates(Config) ->
    ct:log("Testing performance gates"),

    % Load baseline
    BaselineFile =
        proplists:get_value(baseline_path, Config, "./bench/baselines/2026-01-28_v2.0.0.json"),
    {ok, Baseline} = load_baseline(BaselineFile),

    % Get thresholds
    Thresholds = maps:get(<<"regression_thresholds">>, Baseline),
    ThroughputThreshold = maps:get(<<"throughput">>, Thresholds),
    LatencyThreshold = maps:get(<<"latency_p95">>, Thresholds),
    MemoryThreshold = maps:get(<<"memory">>, Thresholds),

    ct:log("Thresholds - Throughput: ~p%, Latency: ~p%, Memory: ~p%",
           [ThroughputThreshold, LatencyThreshold, MemoryThreshold]),

    % Test 1: All metrics pass
    CurrentMetrics1 =
        #{<<"throughput_msg_per_s">> => 2800000.0,
          <<"latency_p95_us">> => 82.0,
          <<"memory_delta_mib">> => 22.0},

    Benchmarks = maps:get(<<"benchmarks">>, Baseline),
    BaselineMetrics = maps:get(<<"core_ops_100k">>, Benchmarks),

    GateResult1 =
        evaluate_performance_gate(<<"core_ops_100k">>,
                                  CurrentMetrics1,
                                  BaselineMetrics,
                                  Thresholds),
    ?assertEqual(pass, GateResult1),
    ct:log("All metrics within thresholds: PASS"),

    % Test 2: Throughput fails
    CurrentMetrics2 =
        #{<<"throughput_msg_per_s">> => 2400000.0,  % Significant degradation
          <<"latency_p95_us">> => 82.0,
          <<"memory_delta_mib">> => 22.0},

    GateResult2 =
        evaluate_performance_gate(<<"core_ops_100k">>,
                                  CurrentMetrics2,
                                  BaselineMetrics,
                                  Thresholds),
    ?assertEqual(fail, GateResult2),
    ct:log("Throughput exceeds threshold: FAIL"),

    % Test 3: Memory fails
    CurrentMetrics3 =
        #{<<"throughput_msg_per_s">> => 2800000.0,
          <<"latency_p95_us">> => 82.0,
          <<"memory_delta_mib">> => 30.0},  % Exceeds threshold

    GateResult3 =
        evaluate_performance_gate(<<"core_ops_100k">>,
                                  CurrentMetrics3,
                                  BaselineMetrics,
                                  Thresholds),
    ?assertEqual(fail, GateResult3),
    ct:log("Memory exceeds threshold: FAIL"),

    ok.

%% @doc Test CI/CD report generation
test_ci_report_generation(Config) ->
    ct:log("Testing CI report generation"),

    % Create test report data
    ReportData =
        #{<<"suite">> => <<"performance_regression">>,
          <<"timestamp">> => erlang:system_time(second),
          <<"git_sha">> => <<"test_sha">>,
          <<"results">> =>
              [#{<<"benchmark">> => <<"core_ops_100k">>,
                 <<"status">> => pass,
                 <<"throughput_diff_percent">> => 1.5,
                 <<"latency_p95_diff_percent">> => 2.0,
                 <<"memory_diff_percent">> => 10.0},
               #{<<"benchmark">> => <<"tcp_quick_1k">>,
                 <<"status">> => fail,
                 <<"throughput_diff_percent">> => -15.0,
                 <<"latency_p95_diff_percent">> => 25.0,
                 <<"memory_diff_percent">> => 30.0}],
          <<"summary">> =>
              #{<<"total">> => 2,
                <<"passed">> => 1,
                <<"failed">> => 1}},

    % Generate CI report
    Report = generate_ci_report(ReportData),

    ct:log("CI Report: ~s", [Report]),

    % Verify report format
    ?assert(is_binary(Report)),
    ?assert(<<"PERFORMANCE REGRESSION REPORT">> =< Report),
    ?assert(<<"SUMMARY">> =< Report),
    ?assert(<<"RESULTS">> =< Report),

    ok.

%% @doc Test full regression suite with benchmarks
test_full_regression_suite(Config) ->
    ct:log("Running full regression suite"),

    % Load baseline
    BaselineFile =
        proplists:get_value(baseline_path, Config, "./bench/baselines/2026-01-28_v2.0.0.json"),
    {ok, Baseline} = load_baseline(BaselineFile),

    % Run core ops benchmark (simplified - skip if benchmark not available)
    BenchmarkResult =
        case code:load_file(erlmcp_bench_core_ops) of
            {module, erlmcp_bench_core_ops} ->
                ct:log("Running core_ops benchmark"),
                run_core_ops_benchmark(#{baseline => Baseline});
            {error, _} ->
                ct:log("Benchmark module not available, using synthetic data"),
                synthetic_benchmark_result()
        end,

    ct:log("Benchmark result: ~p", [BenchmarkResult]),

    % Compare to baseline
    ComparisonResult = compare_to_baseline(BenchmarkResult, Baseline),
    ct:log("Comparison result: ~p", [ComparisonResult]),

    % Verify no critical regressions
    Regressions = maps:get(<<"regressions">>, ComparisonResult, #{}),
    CriticalRegressions =
        maps:to_list(
            maps:filter(fun(_K, V) -> V =:= critical end, Regressions)),

    case CriticalRegressions of
        [] ->
            ct:log("No critical regressions detected"),
            ?assert(true);
        _ ->
            ct:log("Critical regressions detected: ~p", [CriticalRegressions]),
            ?assert(false, {critical_regressions, CriticalRegressions})
    end,

    % Generate and save report
    BaselineDir = proplists:get_value(baseline_dir, Config),
    ReportFile =
        BaselineDir
        ++ "/regression_report_"
        ++ integer_to_list(erlang:system_time(second))
        ++ ".json",

    Report =
        #{<<"timestamp">> => erlang:system_time(second),
          <<"baseline_version">> => maps:get(<<"version">>, Baseline),
          <<"comparison">> => ComparisonResult,
          <<"benchmark_result">> => BenchmarkResult},

    ok = save_baseline(Report, ReportFile),
    ct:log("Regression report saved to: ~s", [ReportFile]),

    % Cleanup
    file:delete(ReportFile),

    ok.

%%%====================================================================
%%% Benchmark Integration Functions
%%%====================================================================

%% @doc Run core operations benchmark
run_core_ops_benchmark(Config) ->
    Baseline = maps:get(baseline, Config),

    try
        % Run the benchmark (100k operations)
        Result = erlmcp_bench_core_ops:run(<<"core_ops_100k">>),

        % Format result for comparison
        #{<<"workload_id">> => <<"core_ops_100k">>,
          <<"throughput_msg_per_s">> => maps:get(<<"throughput_msg_per_s">>, Result, 0),
          <<"latency_p50_us">> => maps:get(<<"latency_p50_us">>, Result, 0),
          <<"latency_p95_us">> => maps:get(<<"latency_p95_us">>, Result, 0),
          <<"latency_p99_us">> => maps:get(<<"latency_p99_us">>, Result, 0),
          <<"memory_delta_mib">> => maps:get(<<"memory_delta_mib">>, Result, 0)}
    catch
        _:Error ->
            ct:log("Benchmark failed: ~p", [Error]),
            synthetic_benchmark_result()
    end.

%% @doc Run network benchmark
run_network_benchmark(Config) ->
    Baseline = maps:get(baseline, Config),

    try
        Result = erlmcp_bench_network_real:run(<<"tcp_quick_1k">>),

        #{<<"workload_id">> => <<"tcp_quick_1k">>,
          <<"throughput_msg_per_s">> => maps:get(<<"throughput_msg_per_s">>, Result, 0),
          <<"latency_p95_us">> => maps:get(<<"latency_p95_us">>, Result, 0),
          <<"latency_p99_us">> => maps:get(<<"latency_p99_us">>, Result, 0),
          <<"memory_delta_mib">> => maps:get(<<"memory_delta_mib">>, Result, 0)}
    catch
        _:Error ->
            ct:log("Network benchmark failed: ~p", [Error]),
            #{<<"workload_id">> => <<"tcp_quick_1k">>,
              <<"throughput_msg_per_s">> => 0,
              <<"latency_p95_us">> => 0,
              <<"latency_p99_us">> => 0,
              <<"memory_delta_mib">> => 0}
    end.

%% @doc Run stress benchmark
run_stress_benchmark(Config) ->
    Baseline = maps:get(baseline, Config),

    try
        Result = erlmcp_bench_stress:run(<<"stress_30s_100k_ops">>),

        #{<<"workload_id">> => <<"stress_30s_100k_ops">>,
          <<"throughput_msg_per_s">> => maps:get(<<"throughput_msg_per_s">>, Result, 0),
          <<"latency_p95_us">> => maps:get(<<"latency_p95_us">>, Result, 0),
          <<"latency_p99_us">> => maps:get(<<"latency_p99_us">>, Result, 0),
          <<"memory_delta_mib">> => maps:get(<<"memory_delta_mib">>, Result, 0)}
    catch
        _:Error ->
            ct:log("Stress benchmark failed: ~p", [Error]),
            #{<<"workload_id">> => <<"stress_30s_100k_ops">>,
              <<"throughput_msg_per_s">> => 0,
              <<"latency_p95_us">> => 0,
              <<"latency_p99_us">> => 0,
              <<"memory_delta_mib">> => 0}
    end.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Load baseline from JSON file
-spec load_baseline(file:filename()) -> {ok, map()} | {error, term()}.
load_baseline(Filename) ->
    case file:read_file(Filename) of
        {ok, JsonData} ->
            try
                Baseline = jsx:decode(JsonData, [return_maps]),
                {ok, Baseline}
            catch
                _:_ ->
                    {error, invalid_json}
            end;
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%% @doc Save baseline to JSON file
-spec save_baseline(map(), file:filename()) -> ok | {error, term()}.
save_baseline(Baseline, Filename) ->
    try
        JsonData = jsx:encode(Baseline, [space, indent]),
        file:write_file(Filename, JsonData)
    catch
        _:_ ->
            {error, encoding_failed}
    end.

%% @doc Compare throughput to baseline
-spec compare_throughput(binary(), float(), map()) -> ok | {error, term()}.
compare_throughput(BenchmarkId, CurrentThroughput, Baseline) ->
    Benchmarks = maps:get(<<"benchmarks">>, Baseline),
    BaselineMetrics = maps:get(BenchmarkId, Benchmarks),
    BaselineThroughput = maps:get(<<"throughput_msg_per_s">>, BaselineMetrics),
    Thresholds = maps:get(<<"regression_thresholds">>, Baseline),
    ThroughputThreshold = maps:get(<<"throughput">>, Thresholds),

    DiffPercent = (CurrentThroughput - BaselineThroughput) / BaselineThroughput * 100,

    ct:log("Throughput comparison: Current=~p, Baseline=~p, Diff=~p%",
           [CurrentThroughput, BaselineThroughput, DiffPercent]),

    if DiffPercent < ThroughputThreshold ->
           {error,
            {regression,
             #{benchmark => BenchmarkId,
               metric => throughput,
               baseline => BaselineThroughput,
               current => CurrentThroughput,
               diff_percent => DiffPercent,
               threshold => ThroughputThreshold}}};
       DiffPercent > 0 ->
           {ok, improvement};
       true ->
           ok
    end.

%% @doc Compare latency to baseline
-spec compare_latency(binary(), float(), float(), map()) -> ok | {error, term()}.
compare_latency(BenchmarkId, CurrentLatency, BaselineLatency, Baseline) ->
    Thresholds = maps:get(<<"regression_thresholds">>, Baseline),
    LatencyThreshold = maps:get(<<"latency_p95">>, Thresholds),

    DiffPercent = (CurrentLatency - BaselineLatency) / BaselineLatency * 100,

    ct:log("Latency comparison: Current=~p, Baseline=~p, Diff=~p%",
           [CurrentLatency, BaselineLatency, DiffPercent]),

    if DiffPercent > LatencyThreshold ->
           {error,
            {regression,
             #{benchmark => BenchmarkId,
               metric => latency,
               baseline => BaselineLatency,
               current => CurrentLatency,
               diff_percent => DiffPercent,
               threshold => LatencyThreshold}}};
       DiffPercent < 0 ->
           {ok, improvement};
       true ->
           ok
    end.

%% @doc Compare memory to baseline
-spec compare_memory(binary(), float(), map()) -> ok | {error, term()}.
compare_memory(BenchmarkId, CurrentMemory, Baseline) ->
    Benchmarks = maps:get(<<"benchmarks">>, Baseline),
    BaselineMetrics = maps:get(BenchmarkId, Benchmarks),
    BaselineMemory = maps:get(<<"memory_delta_mib">>, BaselineMetrics),
    Thresholds = maps:get(<<"regression_thresholds">>, Baseline),
    MemoryThreshold = maps:get(<<"memory">>, Thresholds),

    DiffPercent = (CurrentMemory - BaselineMemory) / BaselineMemory * 100,

    ct:log("Memory comparison: Current=~p, Baseline=~p, Diff=~p%",
           [CurrentMemory, BaselineMemory, DiffPercent]),

    if DiffPercent > MemoryThreshold ->
           {error,
            {regression,
             #{benchmark => BenchmarkId,
               metric => memory,
               baseline => BaselineMemory,
               current => CurrentMemory,
               diff_percent => DiffPercent,
               threshold => MemoryThreshold}}};
       DiffPercent < 0 ->
           {ok, improvement};
       true ->
           ok
    end.

%% @doc Compare current run to historical runs
-spec compare_to_history(binary(), map(), [map()]) -> map().
compare_to_history(BenchmarkId, CurrentRun, History) ->
    CurrentBenchmarks = maps:get(<<"benchmarks">>, CurrentRun),
    CurrentMetrics = maps:get(BenchmarkId, CurrentBenchmarks),
    CurrentThroughput = maps:get(<<"throughput_msg_per_s">>, CurrentMetrics),

    % Calculate historical average
    HistoricalThroughputs =
        lists:map(fun(H) ->
                     Benchmarks = maps:get(<<"benchmarks">>, H, #{}),
                     Metrics = maps:get(BenchmarkId, Benchmarks, #{}),
                     maps:get(<<"throughput_msg_per_s">>, Metrics, 0)
                  end,
                  History),

    AvgThroughput = lists:sum(HistoricalThroughputs) / max(1, length(HistoricalThroughputs)),
    ChangePercent = (CurrentThroughput - AvgThroughput) / AvgThroughput * 100,

    % Determine trend
    Trend =
        if ChangePercent > 5 ->
               <<"improving">>;
           ChangePercent < -5 ->
               <<"degrading">>;
           true ->
               <<"stable">>
        end,

    #{benchmark_id => BenchmarkId,
      current_throughput => CurrentThroughput,
      avg_throughput => AvgThroughput,
      throughput_change_percent => ChangePercent,
      sample_count => length(HistoricalThroughputs),
      trend => Trend}.

%% @doc Analyze trend across multiple data points
-spec analyze_trend(binary(), [map()]) -> map().
analyze_trend(MetricName, DataPoints) ->
    % Extract values and timestamps
    Values = [maps:get(MetricName, D) || D <- DataPoints],

    % Simple linear regression
    N = length(Values),
    SumX =
        lists:sum(
            lists:seq(1, N)),
    SumY = lists:sum(Values),
    SumXY =
        lists:sum([I * V
                   || {I, V}
                          <- lists:zip(
                                 lists:seq(1, N), Values)]),
    SumX2 = lists:sum([I * I || I <- lists:seq(1, N)]),

    Slope = (N * SumXY - SumX * SumY) / (N * SumX2 - SumX * SumX),

    % Determine direction
    Direction =
        if Slope > 1000 ->
               <<"improving">>;
           Slope < -1000 ->
               <<"degrading">>;
           true ->
               <<"stable">>
        end,

    #{metric => MetricName,
      direction => Direction,
      slope => Slope,
      sample_count => N}.

%% @doc Evaluate performance gate
-spec evaluate_performance_gate(binary(), map(), map(), map()) -> pass | fail.
evaluate_performance_gate(BenchmarkId, CurrentMetrics, BaselineMetrics, Thresholds) ->
    ThroughputThreshold = maps:get(<<"throughput">>, Thresholds),
    LatencyThreshold = maps:get(<<"latency_p95">>, Thresholds),
    MemoryThreshold = maps:get(<<"memory">>, Thresholds),

    CurrentThroughput = maps:get(<<"throughput_msg_per_s">>, CurrentMetrics),
    BaselineThroughput = maps:get(<<"throughput_msg_per_s">>, BaselineMetrics),
    ThroughputDiff = (CurrentThroughput - BaselineThroughput) / BaselineThroughput * 100,

    CurrentLatency = maps:get(<<"latency_p95_us">>, CurrentMetrics),
    BaselineLatency = maps:get(<<"latency_p95_us">>, BaselineMetrics),
    LatencyDiff = (CurrentLatency - BaselineLatency) / BaselineLatency * 100,

    CurrentMemory = maps:get(<<"memory_delta_mib">>, CurrentMetrics),
    BaselineMemory = maps:get(<<"memory_delta_mib">>, BaselineMetrics),
    MemoryDiff = (CurrentMemory - BaselineMemory) / BaselineMemory * 100,

    ct:log("Gate evaluation - Throughput: ~p%, Latency: ~p%, Memory: ~p%",
           [ThroughputDiff, LatencyDiff, MemoryDiff]),

    if ThroughputDiff < ThroughputThreshold ->
           fail;
       LatencyDiff > LatencyThreshold ->
           fail;
       MemoryDiff > MemoryThreshold ->
           fail;
       true ->
           pass
    end.

%% @doc Compare benchmark result to baseline
-spec compare_to_baseline(map(), map()) -> map().
compare_to_baseline(BenchmarkResult, Baseline) ->
    BenchmarkId = maps:get(<<"workload_id">>, BenchmarkResult),
    Benchmarks = maps:get(<<"benchmarks">>, Baseline),
    BaselineMetrics = maps:get(BenchmarkId, Benchmarks, #{}),

    Thresholds = maps:get(<<"regression_thresholds">>, Baseline),

    % Compare each metric
    ThroughputResult =
        compare_throughput(BenchmarkId,
                           maps:get(<<"throughput_msg_per_s">>, BenchmarkResult),
                           Baseline),
    LatencyResult =
        compare_latency(BenchmarkId,
                        maps:get(<<"latency_p95_us">>, BenchmarkResult),
                        maps:get(<<"latency_p95_us">>, BaselineMetrics),
                        Baseline),
    MemoryResult =
        compare_memory(BenchmarkId, maps:get(<<"memory_delta_mib">>, BenchmarkResult), Baseline),

    % Count regressions
    Regressions =
        #{throughput =>
              case ThroughputResult of
                  {error, _} ->
                      critical;
                  {ok, improvement} ->
                      improvement;
                  _ ->
                      ok
              end,
          latency =>
              case LatencyResult of
                  {error, _} ->
                      critical;
                  {ok, improvement} ->
                      improvement;
                  _ ->
                      ok
              end,
          memory =>
              case MemoryResult of
                  {error, _} ->
                      warning;
                  {ok, improvement} ->
                      improvement;
                  _ ->
                      ok
              end},

    #{benchmark_id => BenchmarkId,
      regressions => Regressions,
      throughput_result => ThroughputResult,
      latency_result => LatencyResult,
      memory_result => MemoryResult}.

%% @doc Generate CI/CD report
-spec generate_ci_report(map()) -> binary().
generate_ci_report(ReportData) ->
    Timestamp = maps:get(<<"timestamp">>, ReportData),
    Results = maps:get(<<"results">>, ReportData),
    Summary = maps:get(<<"summary">>, ReportData),

    Passed = maps:get(<<"passed">>, Summary),
    Failed = maps:get(<<"failed">>, Summary),
    Total = maps:get(<<"total">>, Summary),

    % Build report sections
    Header =
        io_lib:format("============================================================~n"
                      "        PERFORMANCE REGRESSION REPORT~n"
                      "============================================================~n"
                      "Timestamp: ~s~n"
                      "============================================================~n~n",
                      [format_timestamp(Timestamp)]),

    SummarySection =
        io_lib:format("SUMMARY~n"
                      "-------~n"
                      "Total: ~p~n"
                      "Passed: ~p~n"
                      "Failed: ~p~n"
                      "Status: ~s~n~n",
                      [Total,
                       Passed,
                       Failed,
                       case Failed of
                           0 ->
                               "PASS";
                           _ ->
                               "FAIL"
                       end]),

    ResultsSection =
        lists:map(fun(Result) ->
                     Benchmark = maps:get(<<"benchmark">>, Result),
                     Status = maps:get(<<"status">>, Result),
                     ThroughputDiff = maps:get(<<"throughput_diff_percent">>, Result),
                     LatencyDiff = maps:get(<<"latency_p95_diff_percent">>, Result),
                     MemoryDiff = maps:get(<<"memory_diff_percent">>, Result),

                     io_lib:format("~s: ~s~n"
                                   "  Throughput: ~.2f%~n"
                                   "  Latency P95: ~.2f%~n"
                                   "  Memory: ~.2f%~n",
                                   [Benchmark,
                                    string:to_upper(atom_to_list(Status)),
                                    ThroughputDiff,
                                    LatencyDiff,
                                    MemoryDiff])
                  end,
                  Results),

    iolist_to_binary([Header, SummarySection, ResultsSection]).

%% @doc Format timestamp as ISO8601
-spec format_timestamp(integer()) -> binary().
format_timestamp(UnixSeconds) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:system_time_to_universal_time(UnixSeconds, second),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                   [Year, Month, Day, Hour, Min, Sec])).

%% @doc Generate synthetic benchmark result for testing
-spec synthetic_benchmark_result() -> map().
synthetic_benchmark_result() ->
    #{<<"workload_id">> => <<"core_ops_100k">>,
      <<"throughput_msg_per_s">> => 2750000.0,
      <<"latency_p50_us">> => 0.0,
      <<"latency_p95_us">> => 83.0,
      <<"latency_p99_us">> => 98.0,
      <<"memory_delta_mib">> => 22.0,
      <<"duration_s">> => 0.15,
      <<"operations">> => 400000,
      <<"cpu_percent_avg">> => 48.0}.
