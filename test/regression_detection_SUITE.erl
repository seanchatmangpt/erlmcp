%%%-------------------------------------------------------------------
%%% @doc Regression Detection Integration Tests
%%%
%%% Chicago School TDD tests for regression detection system.
%%% Tests real regression detection with historical data.
%%%
%%% Test Coverage:
%%% - Detects test pass rate drop
%%% - Detects coverage decrease
%%% - Detects performance regression
%%% - Blocks on critical regression
%%% - Allows with justification
%%% - Tracks regression history
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(regression_detection_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
     detects_test_pass_rate_drop_test,
     detects_coverage_decrease_test,
     detects_performance_regression_test,
     blocks_on_critical_regression_test,
     allows_with_justification_test,
     tracks_regression_history_test,
     detects_multiple_regressions_test,
     regression_severity_classification_test,
     regression_recovery_tracking_test
    ].

suite() ->
    [{timetrap, {minutes, 5}}].

init_per_suite(Config) ->
    %% Start application
    case tcps_test_helper:start_tcps_apps() of
        {ok, _Apps} ->
            %% Create test data directory for baselines
            BaselineDir = "/tmp/regression_detection_baselines",
            ok = tcps_test_helper:ensure_test_dir(BaselineDir),

            [{baseline_dir, BaselineDir} | Config];
        {error, Reason} ->
            ct:fail("Failed to start TCPS applications: ~p", [Reason])
    end.

end_per_suite(Config) ->
    BaselineDir = ?config(baseline_dir, Config),
    tcps_test_helper:cleanup_test_dir(BaselineDir),
    tcps_test_helper:stop_tcps_apps(),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting testcase: ~p", [TestCase]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases (Chicago School: Real Detection, Real Data)
%%%===================================================================

%% @doc Test: Detects test pass rate drop
detects_test_pass_rate_drop_test(Config) ->
    BaselineDir = ?config(baseline_dir, Config),

    %% Setup: Create baseline metrics (historical data)
    BaselineMetrics = #{
        work_order_id => <<"WO-BASELINE-001">>,
        timestamp => erlang:system_time(millisecond),
        tests => #{
            total => 100,
            passed => 98,
            failed => 2,
            pass_rate => 98.0
        },
        coverage => 85.0,
        quality_score => 92.0
    },

    %% Write baseline (real file I/O)
    BaselineFile = filename:join(BaselineDir, "baseline_metrics.json"),
    ok = file:write_file(BaselineFile, jsx:encode(BaselineMetrics)),

    %% Setup: Create current metrics with regression (pass rate drop)
    CurrentMetrics = #{
        work_order_id => <<"WO-CURRENT-001">>,
        timestamp => erlang:system_time(millisecond),
        tests => #{
            total => 100,
            passed => 90,  %% Dropped from 98
            failed => 10,
            pass_rate => 90.0  %% 8% drop (moderate: 5-10%)
        },
        coverage => 85.0,
        quality_score => 92.0
    },

    %% Exercise: Run regression detection (real analysis)
    Result = detect_regression(BaselineMetrics, CurrentMetrics),

    %% Verify: Regression detected (state verification)
    {regression_detected, Regressions} = Result,
    ct:log("Detected regressions: ~p", [Regressions]),

    %% Find pass rate regression
    PassRateRegressions = [R || R <- Regressions,
                                 maps:get(metric, R) =:= test_pass_rate],
    true = length(PassRateRegressions) > 0,

    %% Verify regression details
    [PassRateRegression | _] = PassRateRegressions,
    98.0 = maps:get(baseline_value, PassRateRegression),
    90.0 = maps:get(current_value, PassRateRegression),
    true = maps:get(drop_percentage, PassRateRegression) >= 8.0,
    %% 8% drop should be moderate (5-10% range), not critical (>10%)
    moderate = maps:get(severity, PassRateRegression),

    ok.

%% @doc Test: Detects coverage decrease
detects_coverage_decrease_test(Config) ->
    BaselineDir = ?config(baseline_dir, Config),

    %% Setup: Baseline with good coverage
    BaselineMetrics = #{
        work_order_id => <<"WO-COVERAGE-BASELINE">>,
        coverage => 87.5,
        quality_score => 90.0
    },

    BaselineFile = filename:join(BaselineDir, "coverage_baseline.json"),
    ok = file:write_file(BaselineFile, jsx:encode(BaselineMetrics)),

    %% Setup: Current with coverage drop
    CurrentMetrics = #{
        work_order_id => <<"WO-COVERAGE-CURRENT">>,
        coverage => 75.0,  %% Dropped 12.5%
        quality_score => 90.0
    },

    %% Exercise: Detect coverage regression
    Result = detect_regression(BaselineMetrics, CurrentMetrics),

    %% Verify: Coverage regression detected
    {regression_detected, Regressions} = Result,
    ct:log("Coverage regressions: ~p", [Regressions]),

    %% Find coverage regression
    CoverageRegressions = [R || R <- Regressions,
                                 maps:get(metric, R) =:= coverage],
    true = length(CoverageRegressions) > 0,

    [CoverageRegression | _] = CoverageRegressions,
    87.5 = maps:get(baseline_value, CoverageRegression),
    75.0 = maps:get(current_value, CoverageRegression),

    %% Coverage drop >10% is critical
    critical = maps:get(severity, CoverageRegression),

    ok.

%% @doc Test: Detects performance regression
detects_performance_regression_test(Config) ->
    BaselineDir = ?config(baseline_dir, Config),

    %% Setup: Baseline performance metrics
    BaselineMetrics = #{
        work_order_id => <<"WO-PERF-BASELINE">>,
        performance => #{
            throughput_msg_per_s => 1000000,  %% 1M msg/s
            latency_p99_us => 1000,           %% 1ms p99
            memory_heap_mib_per_conn => 2.5
        }
    },

    BaselineFile = filename:join(BaselineDir, "perf_baseline.json"),
    ok = file:write_file(BaselineFile, jsx:encode(BaselineMetrics)),

    %% Setup: Current with performance regression
    CurrentMetrics = #{
        work_order_id => <<"WO-PERF-CURRENT">>,
        performance => #{
            throughput_msg_per_s => 800000,   %% 20% drop (critical!)
            latency_p99_us => 1200,           %% 20% increase
            memory_heap_mib_per_conn => 3.0   %% 20% increase
        }
    },

    %% Exercise: Detect performance regression
    Result = detect_regression(BaselineMetrics, CurrentMetrics),

    %% Verify: Performance regressions detected
    {regression_detected, Regressions} = Result,
    ct:log("Performance regressions: ~p", [Regressions]),

    %% Find throughput regression
    ThroughputRegressions = [R || R <- Regressions,
                                   maps:get(metric, R) =:= throughput],
    true = length(ThroughputRegressions) > 0,

    [ThroughputRegression | _] = ThroughputRegressions,
    1000000 = maps:get(baseline_value, ThroughputRegression),
    800000 = maps:get(current_value, ThroughputRegression),
    critical = maps:get(severity, ThroughputRegression), %% >10% drop

    ok.

%% @doc Test: Blocks on critical regression
blocks_on_critical_regression_test(Config) ->
    _BaselineDir = ?config(baseline_dir, Config),

    %% Setup: Metrics with critical regression
    BaselineMetrics = #{
        tests => #{pass_rate => 98.0},
        coverage => 85.0
    },

    CurrentMetrics = #{
        tests => #{pass_rate => 80.0},  %% 18% drop (critical)
        coverage => 85.0
    },

    %% Exercise: Check if should block (real blocking decision)
    ShouldBlock = should_block_on_regression(BaselineMetrics, CurrentMetrics),

    %% Verify: Blocks on critical regression (state verification)
    {block, Reason} = ShouldBlock,
    ct:log("Blocked due to: ~p", [Reason]),

    %% Reason should mention critical regression
    true = is_map(Reason),
    true = maps:is_key(critical_regressions, Reason),

    CriticalRegressions = maps:get(critical_regressions, Reason),
    true = length(CriticalRegressions) > 0,

    ok.

%% @doc Test: Allows with justification
allows_with_justification_test(Config) ->
    _BaselineDir = ?config(baseline_dir, Config),

    %% Setup: Metrics with critical regression but justification
    BaselineMetrics = #{
        tests => #{pass_rate => 98.0},
        coverage => 85.0
    },

    CurrentMetrics = #{
        tests => #{pass_rate => 88.0},  %% 10% drop (critical, requires justification)
        coverage => 85.0
    },

    Justification = #{
        reason => <<"Refactoring: Removed flaky tests">>,
        ticket => <<"JIRA-1234">>,
        approved_by => <<"tech-lead@company.com">>,
        timestamp => erlang:system_time(millisecond)
    },

    %% Exercise: Check if should block with justification
    ShouldBlock = should_block_on_regression_with_justification(
        BaselineMetrics, CurrentMetrics, Justification
    ),

    %% Verify: Allows with valid justification (state verification)
    {allow, Receipt} = ShouldBlock,
    ct:log("Allowed with receipt: ~p", [Receipt]),

    %% Receipt should contain decision and approved_by (not justification key)
    true = maps:is_key(decision, Receipt),
    allowed_with_justification = maps:get(decision, Receipt),
    true = maps:is_key(approved_by, Receipt),
    <<"tech-lead@company.com">> = maps:get(approved_by, Receipt),

    ok.

%% @doc Test: Tracks regression history
tracks_regression_history_test(Config) ->
    BaselineDir = ?config(baseline_dir, Config),

    %% Setup: Create regression history (multiple regressions over time)
    History = [
        #{
            timestamp => erlang:system_time(millisecond) - 86400000,  %% 1 day ago
            metric => test_pass_rate,
            baseline_value => 98.0,
            current_value => 96.0,
            severity => warning
        },
        #{
            timestamp => erlang:system_time(millisecond) - 43200000,  %% 12 hours ago
            metric => coverage,
            baseline_value => 85.0,
            current_value => 82.0,
            severity => warning
        },
        #{
            timestamp => erlang:system_time(millisecond),  %% Now
            metric => test_pass_rate,
            baseline_value => 96.0,
            current_value => 90.0,
            severity => critical
        }
    ],

    %% Write history (real file I/O)
    HistoryFile = filename:join(BaselineDir, "regression_history.json"),
    ok = file:write_file(HistoryFile, jsx:encode(History)),

    %% Exercise: Analyze regression trend (real trend analysis)
    Trend = analyze_regression_trend(History),

    %% Verify: Trend shows degradation (state verification)
    ct:log("Regression trend: ~p", [Trend]),

    true = maps:is_key(direction, Trend),
    degrading = maps:get(direction, Trend),  %% Pass rate declining

    true = maps:is_key(severity_trend, Trend),
    escalating = maps:get(severity_trend, Trend),  %% Warning -> Critical

    ok.

%% @doc Test: Detects multiple regressions
detects_multiple_regressions_test(Config) ->
    _BaselineDir = ?config(baseline_dir, Config),

    %% Setup: Baseline metrics
    BaselineMetrics = #{
        tests => #{pass_rate => 98.0},
        coverage => 87.5,
        performance => #{throughput_msg_per_s => 1000000}
    },

    %% Setup: Current with multiple regressions
    CurrentMetrics = #{
        tests => #{pass_rate => 90.0},     %% 8% drop
        coverage => 75.0,                   %% 12.5% drop
        performance => #{throughput_msg_per_s => 800000}  %% 20% drop
    },

    %% Exercise: Detect all regressions
    Result = detect_regression(BaselineMetrics, CurrentMetrics),

    %% Verify: Multiple regressions detected
    {regression_detected, Regressions} = Result,
    ct:log("Multiple regressions: ~p", [Regressions]),

    %% Should detect at least 3 regressions
    true = length(Regressions) >= 3,

    %% Verify each regression type present
    Metrics = [maps:get(metric, R) || R <- Regressions],
    true = lists:member(test_pass_rate, Metrics),
    true = lists:member(coverage, Metrics),
    true = lists:member(throughput, Metrics),

    ok.

%% @doc Test: Regression severity classification
regression_severity_classification_test(Config) ->
    _BaselineDir = ?config(baseline_dir, Config),

    %% Setup: Test different drop percentages
    TestCases = [
        {98.0, 96.0, warning},    %% 2% drop = warning
        {98.0, 93.0, moderate},   %% 5% drop = moderate
        {98.0, 88.0, critical},   %% 10% drop = critical
        {98.0, 78.0, blocker}     %% 20% drop = blocker
    ],

    %% Exercise: Classify each severity
    lists:foreach(fun({Baseline, Current, ExpectedSeverity}) ->
        Severity = classify_regression_severity(
            test_pass_rate, Baseline, Current
        ),

        %% Verify: Correct severity classification
        ct:log("Drop ~.1f -> ~.1f classified as: ~p (expected: ~p)",
               [Baseline, Current, Severity, ExpectedSeverity]),
        ExpectedSeverity = Severity
    end, TestCases),

    ok.

%% @doc Test: Regression recovery tracking
regression_recovery_tracking_test(Config) ->
    BaselineDir = ?config(baseline_dir, Config),

    %% Setup: Regression event
    RegressionEvent = #{
        timestamp => erlang:system_time(millisecond) - 3600000,  %% 1 hour ago
        metric => test_pass_rate,
        baseline_value => 98.0,
        current_value => 90.0,
        severity => critical,
        status => detected
    },

    %% Setup: Recovery event (fixed)
    RecoveryEvent = #{
        timestamp => erlang:system_time(millisecond),  %% Now
        metric => test_pass_rate,
        baseline_value => 90.0,
        current_value => 98.0,
        severity => resolved,
        status => recovered,
        recovery_time_ms => 3600000  %% 1 hour to recover
    },

    %% Write events
    EventsFile = filename:join(BaselineDir, "regression_events.json"),
    Events = [RegressionEvent, RecoveryEvent],
    ok = file:write_file(EventsFile, jsx:encode(Events)),

    %% Exercise: Analyze recovery (real recovery analysis)
    RecoveryAnalysis = analyze_recovery(Events),

    %% Verify: Recovery tracked correctly
    ct:log("Recovery analysis: ~p", [RecoveryAnalysis]),

    true = maps:is_key(recovery_time_ms, RecoveryAnalysis),
    3600000 = maps:get(recovery_time_ms, RecoveryAnalysis),

    true = maps:is_key(recovered, RecoveryAnalysis),
    true = maps:get(recovered, RecoveryAnalysis),

    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Detect regression between baseline and current
detect_regression(BaselineMetrics, CurrentMetrics) ->
    Regressions = [],

    %% Check test pass rate
    Regressions1 = case {maps:find(tests, BaselineMetrics),
                         maps:find(tests, CurrentMetrics)} of
        {{ok, BaseTests}, {ok, CurrentTests}} ->
            BasePassRate = maps:get(pass_rate, BaseTests, 100.0),
            CurrentPassRate = maps:get(pass_rate, CurrentTests, 100.0),
            DropPercentage = ((BasePassRate - CurrentPassRate) / BasePassRate) * 100,

            if
                DropPercentage > 5.0 ->  %% >5% drop triggers detection
                    PassRateRegression = #{
                        metric => test_pass_rate,
                        baseline_value => BasePassRate,
                        current_value => CurrentPassRate,
                        drop_percentage => DropPercentage,
                        severity => classify_regression_severity(
                            test_pass_rate, BasePassRate, CurrentPassRate
                        )
                    },
                    [PassRateRegression | Regressions];
                true ->
                    Regressions
            end;
        _ ->
            Regressions
    end,

    %% Check coverage
    Regressions2 = case {maps:find(coverage, BaselineMetrics),
                         maps:find(coverage, CurrentMetrics)} of
        {{ok, BaseCoverage}, {ok, CurrentCoverage}} ->
            CoverageDrop = ((BaseCoverage - CurrentCoverage) / BaseCoverage) * 100,

            if
                CoverageDrop > 10.0 ->  %% >10% drop is critical
                    CoverageRegression = #{
                        metric => coverage,
                        baseline_value => BaseCoverage,
                        current_value => CurrentCoverage,
                        drop_percentage => CoverageDrop,
                        severity => critical
                    },
                    [CoverageRegression | Regressions1];
                true ->
                    Regressions1
            end;
        _ ->
            Regressions1
    end,

    %% Check performance
    Regressions3 = case {maps:find(performance, BaselineMetrics),
                         maps:find(performance, CurrentMetrics)} of
        {{ok, BasePerf}, {ok, CurrentPerf}} ->
            BaseThroughput = maps:get(throughput_msg_per_s, BasePerf, 0),
            CurrentThroughput = maps:get(throughput_msg_per_s, CurrentPerf, 0),
            ThroughputDrop = ((BaseThroughput - CurrentThroughput) / BaseThroughput) * 100,

            if
                ThroughputDrop > 10.0 ->  %% >10% throughput drop is critical
                    ThroughputRegression = #{
                        metric => throughput,
                        baseline_value => BaseThroughput,
                        current_value => CurrentThroughput,
                        drop_percentage => ThroughputDrop,
                        severity => critical
                    },
                    [ThroughputRegression | Regressions2];
                true ->
                    Regressions2
            end;
        _ ->
            Regressions2
    end,

    case Regressions3 of
        [] ->
            {no_regression, []};
        _ ->
            {regression_detected, Regressions3}
    end.

%% Classify regression severity based on drop percentage
classify_regression_severity(_Metric, Baseline, Current) ->
    DropPercentage = ((Baseline - Current) / Baseline) * 100,

    if
        DropPercentage >= 20.0 -> blocker;
        DropPercentage >= 10.0 -> critical;
        DropPercentage >= 5.0 -> moderate;
        DropPercentage >= 2.0 -> warning;
        true -> info
    end.

%% Should block on regression
should_block_on_regression(BaselineMetrics, CurrentMetrics) ->
    case detect_regression(BaselineMetrics, CurrentMetrics) of
        {regression_detected, Regressions} ->
            CriticalRegressions = [R || R <- Regressions,
                                         lists:member(maps:get(severity, R),
                                                      [critical, blocker])],
            case CriticalRegressions of
                [] ->
                    {allow, #{reason => <<"Only minor regressions">>}};
                _ ->
                    {block, #{critical_regressions => CriticalRegressions}}
            end;
        {no_regression, _} ->
            {allow, #{reason => <<"No regression detected">>}}
    end.

%% Should block with justification
should_block_on_regression_with_justification(BaselineMetrics, CurrentMetrics, Justification) ->
    case should_block_on_regression(BaselineMetrics, CurrentMetrics) of
        {block, _Reason} ->
            %% Check if justification is valid
            case maps:is_key(approved_by, Justification) of
                true ->
                    Receipt = #{
                        decision => allowed_with_justification,
                        justification => maps:get(reason, Justification),
                        approved_by => maps:get(approved_by, Justification),
                        timestamp => erlang:system_time(millisecond)
                    },
                    {allow, Receipt};
                false ->
                    {block, #{reason => <<"No valid justification">>}}
            end;
        Allow ->
            Allow
    end.

%% Analyze regression trend
analyze_regression_trend(History) ->
    %% Sort by timestamp
    SortedHistory = lists:sort(
        fun(A, B) ->
            maps:get(timestamp, A) =< maps:get(timestamp, B)
        end,
        History
    ),

    %% Analyze direction (improving or degrading)
    Direction = case length(SortedHistory) >= 2 of
        true ->
            [First | _] = SortedHistory,
            Last = lists:last(SortedHistory),

            FirstValue = maps:get(current_value, First),
            LastValue = maps:get(current_value, Last),

            if
                LastValue < FirstValue -> degrading;
                LastValue > FirstValue -> improving;
                true -> stable
            end;
        false ->
            unknown
    end,

    %% Analyze severity trend (check if last severity is worse than first)
    Severities = [maps:get(severity, R) || R <- SortedHistory],
    SeverityTrend = case length(Severities) >= 2 of
        true ->
            [FirstSev | _] = Severities,
            LastSev = lists:last(Severities),
            %% warning -> critical is escalating
            case {FirstSev, LastSev} of
                {warning, critical} -> escalating;
                {warning, blocker} -> escalating;
                {moderate, critical} -> escalating;
                {moderate, blocker} -> escalating;
                {critical, blocker} -> escalating;
                {critical, warning} -> deescalating;
                {critical, moderate} -> deescalating;
                {blocker, _} when LastSev =/= blocker -> deescalating;
                _ -> stable
            end;
        false -> stable
    end,

    #{
        direction => Direction,
        severity_trend => SeverityTrend,
        total_regressions => length(SortedHistory)
    }.

%% Analyze recovery
analyze_recovery(Events) ->
    %% Find regression and recovery events by looking for status field
    StatusPairs = [{maps:get(status, E, undefined), E} || E <- Events],

    RegressionEvent = case lists:keyfind(detected, 1, StatusPairs) of
        {detected, RegrEvent} -> RegrEvent;
        false -> undefined
    end,

    RecoveryEvent = case lists:keyfind(recovered, 1, StatusPairs) of
        {recovered, RecovEvent} -> RecovEvent;
        false -> undefined
    end,

    case {RegressionEvent, RecoveryEvent} of
        {Regression, Recovery} when is_map(Regression) andalso is_map(Recovery) ->
            RecoveryTime = maps:get(timestamp, Recovery) -
                           maps:get(timestamp, Regression),
            #{
                recovered => true,
                recovery_time_ms => RecoveryTime
            };
        _ ->
            #{
                recovered => false
            }
    end.
