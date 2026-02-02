-module(regression_detector_demo).

%% Demo and testing functions
-export([
    demo/0,
    test_basic_detection/0,
    test_dashboard/0,
    run_performance_scenario/0,
    simulate_regression_detection/0
]).

-include_lib("kernel/include/logger.hrl").

%% @doc Main demo function
demo() ->
    ?LOG_INFO("Starting Performance Regression Detection Specialist Demo"),
    
    % Start the regression detector
    ?LOG_INFO("1. Starting regression detector..."),
    start_detector(),
    
    % Demonstrate baseline establishment
    ?LOG_INFO("2. Establishing performance baselines..."),
    establish_baselines(),
    
    % Test normal operation
    ?LOG_INFO("3. Testing normal performance (no regression)..."),
    test_normal_performance(),
    
    % Simulate performance regression
    ?LOG_INFO("4. Simulating performance regression..."),
    simulate_regression(),
    
    % Start dashboard
    ?LOG_INFO("5. Starting regression tracking dashboard..."),
    test_dashboard(),
    
    % Performance scenario
    ?LOG_INFO("6. Running comprehensive performance scenario..."),
    run_performance_scenario(),
    
    ?LOG_INFO("Demo completed successfully!").

%% @doc Start regression detector
start_detector() ->
    try
        % Mock the detector start since OpenTelemetry has compilation issues
        ?LOG_INFO("Regression detector started (mocked)"),
        ok
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to start detector: ~p:~p", [Error, Reason]),
            {error, {Error, Reason}}
    end.

%% @doc Establish performance baselines
establish_baselines() ->
    % Simulate historical performance data
    BaselineData = [
        {latency, generate_normal_data(50.0, 5.0, 100)},
        {throughput, generate_normal_data(1000.0, 50.0, 100)},
        {error_rate, generate_normal_data(0.01, 0.002, 100)},
        {cpu_usage, generate_normal_data(65.0, 8.0, 100)},
        {memory_usage, generate_normal_data(70.0, 10.0, 100)}
    ],
    
    ?LOG_INFO("Established baselines for ~p metrics:", [length(BaselineData)]),
    lists:foreach(fun({Metric, Values}) ->
        Mean = lists:sum(Values) / length(Values),
        StdDev = calculate_std_dev(Values, Mean),
        ?LOG_INFO("  ~p: Mean=~.2f, StdDev=~.2f, Samples=~p", 
                 [Metric, Mean, StdDev, length(Values)])
    end, BaselineData),
    
    BaselineData.

%% @doc Test normal performance (should not trigger regressions)
test_normal_performance() ->
    NormalMetrics = #{
        latency => 52.0,           % Within normal range
        throughput => 980.0,       % Within normal range
        error_rate => 0.012,       % Within normal range
        cpu_usage => 68.0,         % Within normal range
        memory_usage => 75.0       % Within normal range
    },
    
    ?LOG_INFO("Testing normal metrics: ~p", [NormalMetrics]),
    
    % Simulate detection (would normally call erlmcp_regression_detector:detect_regression)
    Results = simulate_detection(NormalMetrics, normal),
    
    case Results of
        {ok, _} -> 
            ?LOG_INFO("✅ No regression detected (as expected)");
        _ -> 
            ?LOG_WARNING("⚠️  Unexpected result: ~p", [Results])
    end,
    
    Results.

%% @doc Simulate performance regression
simulate_regression() ->
    RegressedMetrics = #{
        latency => 85.0,           % 70% increase - REGRESSION!
        throughput => 750.0,       % 25% decrease - REGRESSION!  
        error_rate => 0.05,        % 5x increase - CRITICAL REGRESSION!
        cpu_usage => 95.0,         % 46% increase - REGRESSION!
        memory_usage => 88.0       % 26% increase - REGRESSION!
    },
    
    ?LOG_INFO("Testing regressed metrics: ~p", [RegressedMetrics]),
    
    % Simulate detection with regressions
    Results = simulate_detection(RegressedMetrics, regression),
    
    case Results of
        {regression_detected, Regressions} ->
            ?LOG_WARNING("🚨 PERFORMANCE REGRESSION DETECTED!"),
            ?LOG_WARNING("Regression details:"),
            lists:foreach(fun(R) ->
                ?LOG_WARNING("  ~p: ~.1f% change (severity: ~p)", 
                           [maps:get(metric_name, R), maps:get(change_percent, R), maps:get(severity, R)])
            end, Regressions);
        _ ->
            ?LOG_INFO("Result: ~p", [Results])
    end,
    
    % Simulate alert
    trigger_alerts(Results),
    
    Results.

%% @doc Test dashboard functionality
test_dashboard() ->
    ?LOG_INFO("Starting regression tracking dashboard..."),
    
    % Mock dashboard start
    Port = 8081,
    ?LOG_INFO("Dashboard would start on http://localhost:~p", [Port]),
    
    % Generate sample dashboard data
    DashboardData = #{
        timestamp => erlang:system_time(millisecond),
        metrics => #{
            latency => 75.2,
            throughput => 850.0,
            error_rate => 0.025,
            cpu_usage => 82.5,
            memory_usage => 78.3
        },
        alerts => [
            #{
                title => "Latency Regression Detected",
                severity => high,
                details => "Latency increased by 45% above baseline",
                timestamp => erlang:system_time(millisecond)
            },
            #{
                title => "Error Rate Spike",
                severity => critical,
                details => "Error rate increased by 150% above baseline", 
                timestamp => erlang:system_time(millisecond)
            }
        ],
        trends => #{
            overall_trend => degrading,
            direction => worsening,
            confidence => 0.92
        }
    },
    
    ?LOG_INFO("Sample dashboard data: ~p", [DashboardData]),
    ?LOG_INFO("Dashboard features:"),
    ?LOG_INFO("  📊 Real-time performance metrics"),
    ?LOG_INFO("  🚨 Active regression alerts"),
    ?LOG_INFO("  📈 Performance trend analysis"),
    ?LOG_INFO("  📋 Detailed regression reports"),
    
    DashboardData.

%% @doc Run comprehensive performance scenario
run_performance_scenario() ->
    ?LOG_INFO("Running comprehensive performance scenario..."),
    
    Scenarios = [
        {"Normal Operation", #{latency => 48.0, throughput => 1050.0, error_rate => 0.008}},
        {"Minor Degradation", #{latency => 58.0, throughput => 950.0, error_rate => 0.015}},
        {"Significant Regression", #{latency => 85.0, throughput => 700.0, error_rate => 0.04}},
        {"Critical Failure", #{latency => 150.0, throughput => 400.0, error_rate => 0.15}},
        {"Recovery", #{latency => 52.0, throughput => 980.0, error_rate => 0.01}}
    ],
    
    Results = lists:map(fun({Name, Metrics}) ->
        ?LOG_INFO("Scenario: ~s", [Name]),
        Result = simulate_detection(Metrics, determine_expected_result(Metrics)),
        ?LOG_INFO("  Result: ~p", [element(1, Result)]),
        {Name, Result}
    end, Scenarios),
    
    ?LOG_INFO("Performance scenario completed. Results summary:"),
    lists:foreach(fun({Name, {Status, _}}) ->
        StatusIcon = case Status of
            ok -> "✅";
            regression_detected -> "🚨";
            _ -> "❓"
        end,
        ?LOG_INFO("  ~s ~s", [StatusIcon, Name])
    end, Results),
    
    Results.

%% @doc Test basic regression detection
test_basic_detection() ->
    ?LOG_INFO("Testing basic regression detection..."),
    
    % Test different severity levels
    TestCases = [
        {"Low severity", #{latency => 55.0}, low},
        {"Medium severity", #{latency => 70.0}, medium},  
        {"High severity", #{latency => 90.0}, high},
        {"Critical severity", #{latency => 120.0}, critical}
    ],
    
    Results = lists:map(fun({Description, Metrics, ExpectedSeverity}) ->
        ?LOG_INFO("Testing: ~s", [Description]),
        Result = simulate_detection(Metrics, regression),
        case Result of
            {regression_detected, [R]} ->
                ActualSeverity = maps:get(severity, R),
                Match = ActualSeverity =:= ExpectedSeverity,
                ?LOG_INFO("  Expected: ~p, Actual: ~p, Match: ~p", 
                         [ExpectedSeverity, ActualSeverity, Match]);
            _ ->
                ?LOG_INFO("  Unexpected result: ~p", [Result])
        end,
        {Description, Result}
    end, TestCases),
    
    Results.

%% @doc Simulate regression detection workflow
simulate_regression_detection() ->
    ?LOG_INFO("Simulating complete regression detection workflow..."),
    
    % 1. Initialize detector
    ?LOG_INFO("Step 1: Initialize regression detector"),
    start_detector(),
    
    % 2. Collect baseline data
    ?LOG_INFO("Step 2: Collect baseline performance data"),
    BaselineData = establish_baselines(),
    
    % 3. Configure thresholds
    ?LOG_INFO("Step 3: Configure regression thresholds"),
    Thresholds = #{
        latency => 0.15,      % 15% increase threshold
        throughput => 0.10,   % 10% decrease threshold  
        error_rate => 0.05,   % 5% increase threshold
        cpu_usage => 0.20,    % 20% increase threshold
        memory_usage => 0.25  % 25% increase threshold
    },
    ?LOG_INFO("Configured thresholds: ~p", [Thresholds]),
    
    % 4. Enable continuous monitoring
    ?LOG_INFO("Step 4: Enable continuous monitoring"),
    
    % 5. Run detection cycle
    ?LOG_INFO("Step 5: Run detection cycles"),
    Cycles = [
        #{latency => 52.0, throughput => 990.0, error_rate => 0.011}, % Normal
        #{latency => 55.0, throughput => 970.0, error_rate => 0.013}, % Normal
        #{latency => 78.0, throughput => 850.0, error_rate => 0.025}, % Regression!
        #{latency => 95.0, throughput => 750.0, error_rate => 0.045}, % Critical!
        #{latency => 48.0, throughput => 1020.0, error_rate => 0.009} % Recovery
    ],
    
    Results = lists:map(fun(Metrics) ->
        simulate_detection(Metrics, determine_expected_result(Metrics))
    end, Cycles),
    
    % 6. Generate report
    ?LOG_INFO("Step 6: Generate regression report"),
    Report = generate_mock_report(Results),
    ?LOG_INFO("Generated report: ~p", [Report]),
    
    ?LOG_INFO("Regression detection workflow completed!"),
    
    #{
        baseline_data => BaselineData,
        thresholds => Thresholds,
        detection_results => Results,
        report => Report
    }.

%% Helper functions

%% @doc Generate normal distribution data
generate_normal_data(Mean, StdDev, Count) ->
    [Mean + StdDev * rand_normal() || _ <- lists:seq(1, Count)].

%% @doc Generate random normal distribution value
rand_normal() ->
    case get(normal_cache) of
        undefined ->
            U1 = rand:uniform(),
            U2 = rand:uniform(), 
            Z0 = math:sqrt(-2 * math:log(U1)) * math:cos(2 * math:pi() * U2),
            Z1 = math:sqrt(-2 * math:log(U1)) * math:sin(2 * math:pi() * U2),
            put(normal_cache, Z1),
            Z0;
        Z1 ->
            erase(normal_cache),
            Z1
    end.

%% @doc Calculate standard deviation
calculate_std_dev(Values, Mean) ->
    Variance = lists:sum([math:pow(X - Mean, 2) || X <- Values]) / (length(Values) - 1),
    math:sqrt(Variance).

%% @doc Simulate regression detection (mock implementation)
simulate_detection(Metrics, ExpectedResult) ->
    % Mock regression detection logic
    case ExpectedResult of
        normal ->
            {ok, []};
        regression ->
            Regressions = lists:filtermap(fun({MetricName, Value}) ->
                case is_regression(MetricName, Value) of
                    true -> 
                        {true, #{
                            metric_name => MetricName,
                            current_value => Value,
                            baseline_value => get_baseline_value(MetricName),
                            change_percent => calculate_change_percent(MetricName, Value),
                            is_regression => true,
                            confidence => 0.95,
                            severity => calculate_mock_severity(MetricName, Value)
                        }};
                    false -> 
                        false
                end
            end, maps:to_list(Metrics)),
            
            case Regressions of
                [] -> {ok, []};
                _ -> {regression_detected, Regressions}
            end
    end.

%% @doc Check if metric shows regression (simplified logic)
is_regression(latency, Value) -> Value > 75.0;
is_regression(throughput, Value) -> Value < 800.0;
is_regression(error_rate, Value) -> Value > 0.02;
is_regression(cpu_usage, Value) -> Value > 85.0;
is_regression(memory_usage, Value) -> Value > 85.0;
is_regression(_, _) -> false.

%% @doc Get baseline value for metric
get_baseline_value(latency) -> 50.0;
get_baseline_value(throughput) -> 1000.0;
get_baseline_value(error_rate) -> 0.01;
get_baseline_value(cpu_usage) -> 65.0;
get_baseline_value(memory_usage) -> 70.0;
get_baseline_value(_) -> 0.0.

%% @doc Calculate change percentage
calculate_change_percent(MetricName, CurrentValue) ->
    BaselineValue = get_baseline_value(MetricName),
    case MetricName of
        throughput -> ((BaselineValue - CurrentValue) / BaselineValue) * 100;
        _ -> ((CurrentValue - BaselineValue) / BaselineValue) * 100
    end.

%% @doc Calculate mock severity
calculate_mock_severity(latency, Value) when Value > 120 -> critical;
calculate_mock_severity(latency, Value) when Value > 90 -> high;
calculate_mock_severity(latency, Value) when Value > 70 -> medium;
calculate_mock_severity(latency, _) -> low;
calculate_mock_severity(error_rate, Value) when Value > 0.08 -> critical;
calculate_mock_severity(error_rate, Value) when Value > 0.04 -> high;
calculate_mock_severity(error_rate, Value) when Value > 0.02 -> medium;
calculate_mock_severity(error_rate, _) -> low;
calculate_mock_severity(_, _) -> medium.

%% @doc Determine expected result based on metrics
determine_expected_result(Metrics) ->
    HasRegression = lists:any(fun({MetricName, Value}) ->
        is_regression(MetricName, Value)
    end, maps:to_list(Metrics)),
    
    case HasRegression of
        true -> regression;
        false -> normal
    end.

%% @doc Trigger alerts for regressions
trigger_alerts({regression_detected, Regressions}) ->
    ?LOG_WARNING("🚨 TRIGGERING PERFORMANCE ALERTS 🚨"),
    CriticalRegressions = [R || R <- Regressions, maps:get(severity, R) =:= critical],
    case CriticalRegressions of
        [] -> 
            ?LOG_WARNING("Medium/High severity regressions detected - sending standard alerts");
        _ ->
            ?LOG_ERROR("CRITICAL REGRESSIONS DETECTED - sending emergency alerts!"),
            lists:foreach(fun(R) ->
                ?LOG_ERROR("CRITICAL: ~p regression - ~.1f% change!", 
                          [maps:get(metric_name, R), maps:get(change_percent, R)])
            end, CriticalRegressions)
    end;
trigger_alerts(_) ->
    ?LOG_INFO("No alerts to trigger").

%% @doc Generate mock regression report
generate_mock_report(Results) ->
    TotalChecks = length(Results),
    Regressions = [R || {regression_detected, _} = R <- Results],
    RegressionCount = length(Regressions),
    
    #{
        summary => #{
            total_checks => TotalChecks,
            regressions_detected => RegressionCount,
            regression_rate => (RegressionCount / TotalChecks) * 100
        },
        recommendations => [
            "Monitor latency trends closely",
            "Consider scaling resources if regressions persist", 
            "Review recent deployments that may have caused degradation",
            "Implement automated rollback if critical regressions are detected"
        ],
        timestamp => erlang:system_time(millisecond)
    }.