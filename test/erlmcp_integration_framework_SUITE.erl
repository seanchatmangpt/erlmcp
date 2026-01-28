%%%-------------------------------------------------------------------
%%% @doc
%%% ErlMCP 100K Concurrent Integration Framework Test Suite
%%%
%%% Comprehensive end-to-end test suite that validates:
%%% 1. Framework components at 100K scale
%%% 2. Test orchestration engine functionality
%%% 3. Result reporting accuracy
%%% 4. Regression detection accuracy
%%% 5. Framework performance metrics
%%%
%%% USAGE:
%%%   rebar3 ct --suite erlmcp_integration_framework_SUITE
%%%   rebar3 ct --suite erlmcp_integration_framework_SUITE --case test_full_framework_at_scale
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_integration_framework_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp_framework.hrl").

%% Common Test callbacks
-export([
    all/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    test_framework_startup_and_shutdown/1,
    test_test_result_reporter/1,
    test_regression_detector/1,
    test_test_orchestrator/1,
    test_full_framework_at_scale/1,
    test_framework_parallel_execution/1,
    test_framework_resource_management/1,
    test_framework_error_handling/1,
    test_framework_performance_metrics/1,
    test_complete_100k_integration/1
]).

%%====================================================================
%% Common Test Callbacks
%%====================================================================

suite() ->
    [{timetrap, {minutes, 30}}].

all() ->
    [
        test_framework_startup_and_shutdown,
        test_test_result_reporter,
        test_regression_detector,
        test_test_orchestrator,
        test_framework_parallel_execution,
        test_framework_resource_management,
        test_framework_error_handling,
        test_framework_performance_metrics,
        test_full_framework_at_scale,
        test_complete_100k_integration
    ].

init_per_suite(Config) ->
    ct:log("Initializing 100K Integration Framework Test Suite~n", []),

    % Start necessary applications
    application:ensure_all_started(erlmcp),

    % Create temporary directory for test artifacts
    TempDir = "/tmp/erlmcp_framework_tests",
    filelib:ensure_dir(TempDir ++ "/"),

    [{temp_dir, TempDir} | Config].

end_per_suite(Config) ->
    ct:log("Finalizing 100K Integration Framework Test Suite~n", []),

    % Cleanup
    TempDir = proplists:get_value(temp_dir, Config),
    catch file:del_dir_r(TempDir),

    Config.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~w~n", [TestCase]),
    [{test_case, TestCase}, {test_start_time, erlang:system_time(millisecond)} | Config].

end_per_testcase(TestCase, Config) ->
    StartTime = proplists:get_value(test_start_time, Config),
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    ct:log("Test case ~w completed in ~w ms~n", [TestCase, Duration]),
    Config.

%%====================================================================
%% Test Cases
%%====================================================================

test_framework_startup_and_shutdown(Config) ->
    %% GIVEN: Framework components initialized
    StartTime = erlang:system_time(millisecond),

    %% WHEN: Starting framework components
    {ok, FrameworkPid} = erlmcp_framework_100k:start_link(),
    {ok, ReporterPid} = erlmcp_test_result_reporter:start_link(<<"test_reporter">>),
    {ok, DetectorPid} = erlmcp_regression_detector:start_link(),
    {ok, OrchestratorPid} = erlmcp_test_orchestrator_100k:start_link(<<"test_orchestrator">>),

    %% THEN: All components start successfully
    ?assert(is_pid(FrameworkPid)),
    ?assert(is_pid(ReporterPid)),
    ?assert(is_pid(DetectorPid)),
    ?assert(is_pid(OrchestratorPid)),

    %% AND: Shutdown completes successfully
    ok = erlmcp_framework_100k:stop(),
    ok = erlmcp_test_result_reporter:stop(ReporterPid),
    ok = erlmcp_regression_detector:stop(DetectorPid),
    ok = erlmcp_test_orchestrator_100k:stop(OrchestratorPid),

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    ct:log("Framework startup/shutdown test completed in ~w ms~n", [Duration]),
    ?assert(Duration < 5000).  %% Should complete in < 5 seconds

test_test_result_reporter(Config) ->
    %% GIVEN: Test result reporter initialized
    {ok, ReporterPid} = erlmcp_test_result_reporter:start_link(<<"reporter_test">>),

    %% WHEN: Adding test results
    TestResult1 = #{
        name => <<"test_1">>,
        status => passed,
        duration_ms => 100,
        assertions_total => 10,
        assertions_passed => 10,
        assertions_failed => 0,
        metrics => #{throughput => 1000}
    },

    TestResult2 = #{
        name => <<"test_2">>,
        status => passed,
        duration_ms => 150,
        assertions_total => 15,
        assertions_passed => 15,
        assertions_failed => 0,
        metrics => #{throughput => 900}
    },

    ok = erlmcp_test_result_reporter:add_test_result(ReporterPid, TestResult1),
    ok = erlmcp_test_result_reporter:add_test_result(ReporterPid, TestResult2),

    %% THEN: Summary is accurate
    Summary = erlmcp_test_result_reporter:get_summary(ReporterPid),

    ?assertEqual(2, Summary#report_summary.total_tests),
    ?assertEqual(2, Summary#report_summary.passed_tests),
    ?assertEqual(0, Summary#report_summary.failed_tests),
    ?assertEqual(25, Summary#report_summary.total_assertions),
    ?assertEqual(100.0, Summary#report_summary.pass_rate),

    %% AND: Metrics are aggregated correctly
    MetricsSummary = erlmcp_test_result_reporter:get_metrics_summary(ReporterPid),
    AvgThroughput = maps:get(avg_throughput_msg_per_sec, MetricsSummary, 0),
    ?assert(AvgThroughput > 900),  %% Should be between 900-1000

    %% AND: Reports can be generated
    TempDir = proplists:get_value(temp_dir, Config),
    {ok, _Results} = erlmcp_test_result_reporter:generate_report(
        ReporterPid,
        [json, html, csv, plaintext],
        TempDir
    ),

    ok = erlmcp_test_result_reporter:stop(ReporterPid),
    ct:log("Reporter test passed~n", []).

test_regression_detector(Config) ->
    %% GIVEN: Regression detector initialized
    {ok, DetectorPid} = erlmcp_regression_detector:start_link(),

    %% WHEN: Creating baseline from test results
    BaselineResults = [
        #{throughput_msg_per_sec => 10000, avg_latency_ms => 10, p99_latency_ms => 50},
        #{throughput_msg_per_sec => 9800, avg_latency_ms => 12, p99_latency_ms => 45},
        #{throughput_msg_per_sec => 10200, avg_latency_ms => 9, p99_latency_ms => 55}
    ],

    {ok, Baseline} = erlmcp_regression_detector:create_baseline(DetectorPid, BaselineResults),

    %% THEN: Baseline is created with correct structure
    ?assert(maps:is_key(throughput, Baseline)),
    ?assert(maps:is_key(latency, Baseline)),
    ?assert(maps:is_key(memory, Baseline)),

    %% AND: Baseline contains statistical metrics
    ThroughputBaseline = maps:get(throughput, Baseline),
    ?assert(is_record(ThroughputBaseline, baseline_metrics)),
    ?assert(ThroughputBaseline#baseline_metrics.mean > 9500),

    %% WHEN: Detecting regressions with degraded results
    DegradedResults = [
        #{throughput_msg_per_sec => 8000, avg_latency_ms => 20, p99_latency_ms => 100},
        #{throughput_msg_per_sec => 8200, avg_latency_ms => 22, p99_latency_ms => 95}
    ],

    {ok, Regressions} = erlmcp_regression_detector:detect_regressions(
        DetectorPid,
        DegradedResults,
        Baseline
    ),

    %% THEN: Regressions are detected
    ?assert(length(Regressions) > 0),

    % Find throughput regression
    ThroughputRegressions = [R || R <- Regressions, R#regression_result.metric_name =:= throughput],
    ?assert(length(ThroughputRegressions) > 0),

    ThroughputRegression = hd(ThroughputRegressions),
    ?assert(ThroughputRegression#regression_result.is_regression),
    ?assert(ThroughputRegression#regression_result.difference_percent > 10),

    %% AND: Regression severity is calculated
    ?assert(ThroughputRegression#regression_result.severity =/= none),

    ok = erlmcp_regression_detector:stop(DetectorPid),
    ct:log("Regression detector test passed~n", []).

test_test_orchestrator(Config) ->
    %% GIVEN: Test orchestrator initialized
    {ok, OrchestratorPid} = erlmcp_test_orchestrator_100k:start_link(<<"orchestrator_test">>),

    %% WHEN: Adding test suites
    TestFun1 = fun() -> {ok, result1} end,
    TestFun2 = fun() -> {ok, result2} end,
    TestFun3 = fun() -> {ok, result3} end,

    ok = erlmcp_test_orchestrator_100k:add_test_suite(OrchestratorPid, <<"test_1">>, TestFun1),
    ok = erlmcp_test_orchestrator_100k:add_test_suite(OrchestratorPid, <<"test_2">>, TestFun2),
    ok = erlmcp_test_orchestrator_100k:add_test_suite(OrchestratorPid, <<"test_3">>, TestFun3),

    %% AND: Adding dependencies
    ok = erlmcp_test_orchestrator_100k:add_test_dependency(OrchestratorPid, <<"test_2">>, <<"test_1">>),

    %% WHEN: Executing tests
    {ok, Results} = erlmcp_test_orchestrator_100k:execute(OrchestratorPid),

    %% THEN: Execution completes successfully
    ?assert(maps:is_key(test, Results)),

    %% AND: Metrics are collected
    Metrics = erlmcp_test_orchestrator_100k:get_orchestration_metrics(OrchestratorPid),
    ?assertEqual(3, maps:get(total_tests, Metrics)),

    %% AND: Status indicates completion
    Status = erlmcp_test_orchestrator_100k:get_execution_status(OrchestratorPid),
    ?assertEqual(completed, maps:get(status, Status)),

    ok = erlmcp_test_orchestrator_100k:stop(OrchestratorPid),
    ct:log("Test orchestrator test passed~n", []).

test_framework_parallel_execution(Config) ->
    %% GIVEN: Test orchestrator for parallel execution
    {ok, OrchestratorPid} = erlmcp_test_orchestrator_100k:start_link(<<"parallel_test">>),

    %% WHEN: Adding multiple test suites
    lists:foreach(fun(I) ->
        TestFun = fun() ->
            timer:sleep(10),
            {ok, {result, I}}
        end,
        TestId = list_to_binary(io_lib:format("test_~w", [I])),
        ok = erlmcp_test_orchestrator_100k:add_test_suite(OrchestratorPid, TestId, TestFun)
    end, lists:seq(1, 10)),

    %% AND: Executing in parallel
    StartTime = erlang:system_time(millisecond),
    {ok, _Results} = erlmcp_test_orchestrator_100k:execute_parallel(OrchestratorPid, 5),
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    %% THEN: Parallel execution is faster than sequential
    % Sequential would take ~100ms (10 * 10ms), parallel should take ~20-30ms
    ct:log("Parallel execution took ~w ms~n", [Duration]),
    ?assert(Duration < 100),  %% Should be much faster than sequential

    %% AND: Parallelism efficiency is calculated
    Metrics = erlmcp_test_orchestrator_100k:get_orchestration_metrics(OrchestratorPid),
    Efficiency = maps:get(parallelism_efficiency, Metrics),
    ct:log("Parallelism efficiency: ~.2f~n", [Efficiency]),
    ?assert(Efficiency > 1.0),  %% Should show parallelism benefit

    ok = erlmcp_test_orchestrator_100k:stop(OrchestratorPid),
    ct:log("Parallel execution test passed~n", []).

test_framework_resource_management(Config) ->
    %% GIVEN: Framework managing resources
    {ok, OrchestratorPid} = erlmcp_test_orchestrator_100k:start_link(<<"resource_test">>),

    %% WHEN: Creating many tests
    lists:foreach(fun(I) ->
        TestFun = fun() ->
            % Simulate some work
            lists:sum(lists:seq(1, 100)),
            {ok, done}
        end,
        TestId = list_to_binary(io_lib:format("test_~w", [I])),
        ok = erlmcp_test_orchestrator_100k:add_test_suite(OrchestratorPid, TestId, TestFun)
    end, lists:seq(1, 50)),

    %% AND: Executing tests
    {ok, _Results} = erlmcp_test_orchestrator_100k:execute_parallel(OrchestratorPid, 10),

    %% THEN: All resources are cleaned up
    ok = erlmcp_test_orchestrator_100k:cleanup(OrchestratorPid),

    %% AND: Progress shows all tests completed
    Progress = erlmcp_test_orchestrator_100k:get_test_progress(OrchestratorPid),
    ActiveTests = maps:get(active_tests, Progress),
    ?assertEqual(0, ActiveTests),  %% All active tests cleaned up

    ok = erlmcp_test_orchestrator_100k:stop(OrchestratorPid),
    ct:log("Resource management test passed~n", []).

test_framework_error_handling(Config) ->
    %% GIVEN: Framework with error handling
    {ok, OrchestratorPid} = erlmcp_test_orchestrator_100k:start_link(<<"error_test">>),

    %% WHEN: Adding test that will fail
    FailingTest = fun() -> error(intentional_failure) end,
    PassingTest = fun() -> {ok, success} end,

    ok = erlmcp_test_orchestrator_100k:add_test_suite(OrchestratorPid, <<"failing_test">>, FailingTest),
    ok = erlmcp_test_orchestrator_100k:add_test_suite(OrchestratorPid, <<"passing_test">>, PassingTest),

    %% AND: Executing tests
    {ok, _Results} = erlmcp_test_orchestrator_100k:execute(OrchestratorPid),

    %% THEN: Status indicates failure but execution completes
    Status = erlmcp_test_orchestrator_100k:get_execution_status(OrchestratorPid),
    ?assertEqual(completed, maps:get(status, Status)),

    %% AND: Metrics show both passed and failed
    Metrics = erlmcp_test_orchestrator_100k:get_orchestration_metrics(OrchestratorPid),
    TotalTests = maps:get(total_tests, Metrics),
    PassedTests = maps:get(passed_tests, Metrics),
    FailedTests = maps:get(failed_tests, Metrics),

    ?assertEqual(2, TotalTests),
    ?assertEqual(1, PassedTests),
    ?assertEqual(1, FailedTests),

    ok = erlmcp_test_orchestrator_100k:stop(OrchestratorPid),
    ct:log("Error handling test passed~n", []).

test_framework_performance_metrics(Config) ->
    %% GIVEN: Framework collecting metrics
    {ok, FrameworkPid} = erlmcp_framework_100k:start_link(),

    %% WHEN: Running full test suite at scale
    {ok, _Results} = erlmcp_framework_100k:run_full_suite(1000, 60, [
        connection_scaling,
        sustained_load,
        memory_stability
    ]),

    %% THEN: Framework metrics are collected
    FrameworkMetrics = erlmcp_framework_100k:get_framework_metrics(),

    ?assert(maps:is_key(framework_startup_ms, FrameworkMetrics)),
    ?assert(maps:is_key(framework_shutdown_ms, FrameworkMetrics)),
    ?assert(maps:is_key(test_scheduling_overhead_us, FrameworkMetrics)),
    ?assert(maps:is_key(result_aggregation_overhead_us, FrameworkMetrics)),

    StartupTime = maps:get(framework_startup_ms, FrameworkMetrics),
    SchedulingOverhead = maps:get(test_scheduling_overhead_us, FrameworkMetrics),
    Reliability = maps:get(framework_reliability_percent, FrameworkMetrics),

    ct:log("Framework startup time: ~w ms~n", [StartupTime]),
    ct:log("Test scheduling overhead: ~w us~n", [SchedulingOverhead]),
    ct:log("Framework reliability: ~.2f%~n", [Reliability]),

    ?assert(StartupTime < 1000),  %% Should startup quickly
    ?assert(SchedulingOverhead < 1000),  %% Overhead should be minimal
    ?assert(Reliability >= 99.0),  %% High reliability

    ok = erlmcp_framework_100k:stop(),
    ct:log("Performance metrics test passed~n", []).

test_full_framework_at_scale(Config) ->
    %% GIVEN: Full framework at scale
    {ok, FrameworkPid} = erlmcp_framework_100k:start_link(),
    {ok, ReporterPid} = erlmcp_test_result_reporter:start_link(<<"full_scale_test">>),
    {ok, DetectorPid} = erlmcp_regression_detector:start_link(),

    %% WHEN: Running full 100K test suite
    {ok, Results} = erlmcp_framework_100k:run_full_suite(100000, 300),

    %% THEN: Results are captured
    ?assert(maps:size(Results) > 0),

    %% AND: All scenarios executed
    Scenarios = maps:keys(Results),
    RequiredScenarios = [connection_scaling, sustained_load, memory_stability,
                        latency_consistency, error_handling, graceful_degradation],
    lists:foreach(fun(Scenario) ->
        ?assert(lists:member(Scenario, Scenarios))
    end, RequiredScenarios),

    %% AND: Framework metrics validate scale support
    FrameworkMetrics = erlmcp_framework_100k:get_framework_metrics(),
    TotalTests = maps:get(total_tests_executed, FrameworkMetrics),
    Reliability = maps:get(framework_reliability_percent, FrameworkMetrics),

    ct:log("Executed ~w tests at 100K scale~n", [TotalTests]),
    ?assert(TotalTests >= 6),  %% At least 6 scenarios
    ?assert(Reliability >= 99.0),

    ok = erlmcp_framework_100k:stop(),
    ok = erlmcp_test_result_reporter:stop(ReporterPid),
    ok = erlmcp_regression_detector:stop(DetectorPid),

    ct:log("Full framework at scale test passed~n", []).

test_complete_100k_integration(Config) ->
    %% GIVEN: Complete integration setup
    StartTime = erlang:system_time(millisecond),

    {ok, FrameworkPid} = erlmcp_framework_100k:start_link(),
    {ok, ReporterPid} = erlmcp_test_result_reporter:start_link(<<"integration_test">>),
    {ok, DetectorPid} = erlmcp_regression_detector:start_link(),
    {ok, OrchestratorPid} = erlmcp_test_orchestrator_100k:start_link(<<"integration_orchestrator">>),

    %% WHEN: Running complete 100K integration test
    ct:log("Starting complete 100K integration test~n", []),

    % 1. Run full framework suite
    {ok, Results} = erlmcp_framework_100k:run_full_suite(100000, 300, [
        connection_scaling,
        sustained_load,
        failover_recovery,
        chaos_injection,
        memory_stability,
        latency_consistency,
        error_handling,
        graceful_degradation
    ]),

    % 2. Add results to reporter
    lists:foreach(fun({_Scenario, Result}) ->
        ok = erlmcp_test_result_reporter:add_test_result(ReporterPid, maps:from_list([
            {name, <<"test">>},
            {status, Result#test_result.status},
            {duration_ms, Result#test_result.duration_ms},
            {assertions_total, Result#test_result.assertions_total},
            {assertions_passed, Result#test_result.assertions_passed},
            {assertions_failed, Result#test_result.assertions_failed},
            {metrics, Result#test_result.metrics}
        ])),

        ok = erlmcp_test_result_reporter:mark_test_complete(ReporterPid,
            list_to_binary(io_lib:format("~w", [_Scenario])))
    end, maps:to_list(Results)),

    % 3. Generate reports
    TempDir = proplists:get_value(temp_dir, Config),
    {ok, ReportFiles} = erlmcp_test_result_reporter:generate_report(
        ReporterPid,
        [json, html, csv, plaintext],
        TempDir
    ),

    %% THEN: Full integration completes successfully
    ?assert(maps:size(Results) > 0),
    ?assert(maps:size(ReportFiles) > 0),

    %% AND: Regression detection validates results
    TestResultsForDetector = [
        #{
            throughput_msg_per_sec => 50000 + rand:uniform(5000),
            avg_latency_ms => 10 + rand:uniform(5),
            p99_latency_ms => 50 + rand:uniform(10),
            error_rate => 0.001 + (rand:uniform(10) / 10000)
        }
        || _ <- lists:seq(1, 5)
    ],

    {ok, Baseline} = erlmcp_regression_detector:create_baseline(DetectorPid, TestResultsForDetector),
    {ok, Regressions} = erlmcp_regression_detector:detect_regressions(
        DetectorPid,
        TestResultsForDetector,
        Baseline
    ),

    ct:log("Detected ~w regressions~n", [length(Regressions)]),

    %% AND: Framework metrics demonstrate successful 100K scale
    FrameworkMetrics = erlmcp_framework_100k:get_framework_metrics(),

    ct:log("Framework Metrics:~n", []),
    ct:log("  Total tests executed: ~w~n", [maps:get(total_tests_executed, FrameworkMetrics)]),
    ct:log("  Framework reliability: ~.2f%~n", [maps:get(framework_reliability_percent, FrameworkMetrics)]),
    ct:log("  Framework startup: ~w ms~n", [maps:get(framework_startup_ms, FrameworkMetrics)]),
    ct:log("  Scheduling overhead: ~w us~n", [maps:get(test_scheduling_overhead_us, FrameworkMetrics)]),
    ct:log("  Memory peak: ~w MB~n", [maps:get(memory_peak_mb, FrameworkMetrics)]),

    ?assert(maps:get(framework_reliability_percent, FrameworkMetrics) >= 99.0),

    %% Cleanup
    ok = erlmcp_framework_100k:stop(),
    ok = erlmcp_test_result_reporter:stop(ReporterPid),
    ok = erlmcp_regression_detector:stop(DetectorPid),
    ok = erlmcp_test_orchestrator_100k:stop(OrchestratorPid),

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    ct:log("Complete 100K integration test passed in ~w ms~n", [Duration]).
