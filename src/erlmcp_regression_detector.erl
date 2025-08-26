-module(erlmcp_regression_detector).

%% API
-export([
    detect_regression/2,
    update_baseline/1,
    start/0,
    stop/0,
    get_regression_report/1,
    configure_thresholds/1,
    enable_continuous_monitoring/1
]).

%% Internal functions
-export([
    check_latency_regression/2,
    check_throughput_regression/2,
    check_error_rate_regression/2,
    check_resource_regression/2,
    alert_on_regression/1,
    calculate_statistical_significance/2,
    detect_anomalies/2,
    generate_regression_report/2
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-define(CONFIDENCE_LEVEL, 0.95).
-define(ANOMALY_THRESHOLD, 2.5). % Standard deviations
-define(REGRESSION_THRESHOLD, 0.05). % 5% degradation threshold
-define(MIN_SAMPLES, 30). % Minimum samples for statistical significance

-record(baseline, {
    mean :: float(),
    std_dev :: float(),
    samples :: [float()],
    timestamp :: integer(),
    confidence_interval :: {float(), float()}
}).

-record(regression_result, {
    metric_name :: atom(),
    current_value :: float(),
    baseline_value :: float(),
    change_percent :: float(),
    is_regression :: boolean(),
    confidence :: float(),
    severity :: low | medium | high | critical
}).

-record(detector_config, {
    latency_threshold :: float(),
    throughput_threshold :: float(),
    error_rate_threshold :: float(),
    resource_threshold :: float(),
    alert_channels :: [atom()],
    continuous_monitoring :: boolean()
}).

%% @doc Start the regression detector
start() ->
    SpanCtx = otel_tracer:start_span(<<"regression_detector_start">>),
    
    try
        case ets:info(regression_baselines) of
            undefined ->
                ets:new(regression_baselines, [named_table, public, {keypos, 1}]),
                ets:new(regression_config, [named_table, public, {keypos, 1}]),
                setup_default_config(),
                ?LOG_INFO("Regression detector started successfully"),
                otel_span:set_status(SpanCtx, ok),
                ok;
            _ ->
                ?LOG_INFO("Regression detector already running"),
                otel_span:set_status(SpanCtx, ok),
                ok
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to start regression detector: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Stop the regression detector
stop() ->
    SpanCtx = otel_tracer:start_span(<<"regression_detector_stop">>),
    
    try
        ets:delete(regression_baselines),
        ets:delete(regression_config),
        ?LOG_INFO("Regression detector stopped"),
        otel_span:set_status(SpanCtx, ok),
        ok
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to stop regression detector: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Main regression detection function
detect_regression(CurrentMetrics, BaselineKey) ->
    SpanCtx = otel_tracer:start_span(<<"regression_detection">>),
    otel_span:set_attribute(SpanCtx, baseline_key, BaselineKey),
    
    try
        case ets:lookup(regression_baselines, BaselineKey) of
            [] ->
                ?LOG_WARNING("No baseline found for key: ~p", [BaselineKey]),
                otel_span:set_status(SpanCtx, error),
                {error, no_baseline};
            [{BaselineKey, Baseline}] ->
                Regressions = perform_regression_analysis(CurrentMetrics, Baseline),
                
                case has_significant_regression(Regressions) of
                    true ->
                        ?LOG_WARNING("Performance regression detected!"),
                        otel_span:set_status(SpanCtx, error),
                        alert_on_regression(Regressions),
                        {regression_detected, Regressions};
                    false ->
                        ?LOG_INFO("No significant regression detected"),
                        otel_span:set_status(SpanCtx, ok),
                        {ok, Regressions}
                end
        end
    catch
        Error:Reason:Stack ->
            ?LOG_ERROR("Error in regression detection: ~p:~p~n~p", [Error, Reason, Stack]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Update baseline metrics
update_baseline(Metrics) ->
    SpanCtx = otel_tracer:start_span(<<"baseline_update">>),
    
    try
        lists:foreach(fun({MetricName, Values}) ->
            Baseline = calculate_baseline(Values),
            ets:insert(regression_baselines, {MetricName, Baseline})
        end, Metrics),
        
        ?LOG_INFO("Baselines updated for ~p metrics", [length(Metrics)]),
        otel_span:set_status(SpanCtx, ok),
        ok
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to update baseline: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Configure regression detection thresholds
configure_thresholds(Config) ->
    SpanCtx = otel_tracer:start_span(<<"configure_thresholds">>),
    
    try
        ets:insert(regression_config, {thresholds, Config}),
        ?LOG_INFO("Regression thresholds configured: ~p", [Config]),
        otel_span:set_status(SpanCtx, ok),
        ok
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to configure thresholds: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Enable continuous monitoring
enable_continuous_monitoring(Enabled) ->
    SpanCtx = otel_tracer:start_span(<<"enable_continuous_monitoring">>),
    otel_span:set_attribute(SpanCtx, enabled, Enabled),
    
    try
        case ets:lookup(regression_config, monitoring) of
            [] ->
                ets:insert(regression_config, {monitoring, Enabled});
            [{monitoring, _}] ->
                ets:insert(regression_config, {monitoring, Enabled})
        end,
        
        ?LOG_INFO("Continuous monitoring ~p", [case Enabled of true -> enabled; false -> disabled end]),
        otel_span:set_status(SpanCtx, ok),
        ok
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to configure monitoring: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Generate comprehensive regression report
get_regression_report(TimeRange) ->
    SpanCtx = otel_tracer:start_span(<<"generate_regression_report">>),
    otel_span:set_attribute(SpanCtx, time_range, TimeRange),
    
    try
        Report = #{
            timestamp => erlang:system_time(millisecond),
            time_range => TimeRange,
            baselines => get_all_baselines(),
            recent_regressions => get_recent_regressions(TimeRange),
            performance_trends => analyze_performance_trends(TimeRange),
            recommendations => generate_recommendations()
        },
        
        otel_span:set_status(SpanCtx, ok),
        {ok, Report}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to generate regression report: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% Internal functions

%% @doc Perform comprehensive regression analysis
perform_regression_analysis(CurrentMetrics, Baseline) ->
    SpanCtx = otel_tracer:start_span(<<"regression_analysis">>),
    
    try
        Results = [
            check_latency_regression(CurrentMetrics, Baseline),
            check_throughput_regression(CurrentMetrics, Baseline),
            check_error_rate_regression(CurrentMetrics, Baseline),
            check_resource_regression(CurrentMetrics, Baseline),
            check_memory_regression(CurrentMetrics, Baseline),
            check_cpu_regression(CurrentMetrics, Baseline)
        ],
        
        FilteredResults = lists:filter(fun(R) -> R =/= skip end, Results),
        otel_span:set_attribute(SpanCtx, results_count, length(FilteredResults)),
        otel_span:set_status(SpanCtx, ok),
        FilteredResults
    catch
        Error:Reason ->
            ?LOG_ERROR("Error in regression analysis: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            []
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Check latency regression
check_latency_regression(CurrentMetrics, Baseline) ->
    case maps:get(latency, CurrentMetrics, undefined) of
        undefined -> skip;
        CurrentLatency ->
            check_metric_regression(latency, CurrentLatency, Baseline)
    end.

%% @doc Check throughput regression
check_throughput_regression(CurrentMetrics, Baseline) ->
    case maps:get(throughput, CurrentMetrics, undefined) of
        undefined -> skip;
        CurrentThroughput ->
            % For throughput, a decrease is a regression
            Result = check_metric_regression(throughput, CurrentThroughput, Baseline),
            case Result of
                #regression_result{change_percent = Change} = R when Change < 0 ->
                    R#regression_result{is_regression = true};
                _ -> Result
            end
    end.

%% @doc Check error rate regression
check_error_rate_regression(CurrentMetrics, Baseline) ->
    case maps:get(error_rate, CurrentMetrics, undefined) of
        undefined -> skip;
        CurrentErrorRate ->
            check_metric_regression(error_rate, CurrentErrorRate, Baseline)
    end.

%% @doc Check resource usage regression
check_resource_regression(CurrentMetrics, Baseline) ->
    case maps:get(resource_usage, CurrentMetrics, undefined) of
        undefined -> skip;
        CurrentResource ->
            check_metric_regression(resource_usage, CurrentResource, Baseline)
    end.

%% @doc Check memory usage regression
check_memory_regression(CurrentMetrics, Baseline) ->
    case maps:get(memory_usage, CurrentMetrics, undefined) of
        undefined -> skip;
        CurrentMemory ->
            check_metric_regression(memory_usage, CurrentMemory, Baseline)
    end.

%% @doc Check CPU usage regression
check_cpu_regression(CurrentMetrics, Baseline) ->
    case maps:get(cpu_usage, CurrentMetrics, undefined) of
        undefined -> skip;
        CurrentCPU ->
            check_metric_regression(cpu_usage, CurrentCPU, Baseline)
    end.

%% @doc Generic metric regression check
check_metric_regression(MetricName, CurrentValue, Baseline) ->
    BaselineValue = Baseline#baseline.mean,
    ChangePercent = ((CurrentValue - BaselineValue) / BaselineValue) * 100,
    
    IsRegression = is_significant_regression(MetricName, CurrentValue, Baseline),
    Confidence = calculate_confidence(CurrentValue, Baseline),
    Severity = calculate_severity(MetricName, ChangePercent),
    
    #regression_result{
        metric_name = MetricName,
        current_value = CurrentValue,
        baseline_value = BaselineValue,
        change_percent = ChangePercent,
        is_regression = IsRegression,
        confidence = Confidence,
        severity = Severity
    }.

%% @doc Check if there's a significant regression
is_significant_regression(MetricName, CurrentValue, Baseline) ->
    case calculate_statistical_significance(CurrentValue, Baseline) of
        {significant, _PValue} ->
            Threshold = get_threshold(MetricName),
            BaselineValue = Baseline#baseline.mean,
            case MetricName of
                throughput ->
                    % For throughput, decrease is regression
                    (BaselineValue - CurrentValue) / BaselineValue > Threshold;
                _ ->
                    % For other metrics, increase is regression
                    (CurrentValue - BaselineValue) / BaselineValue > Threshold
            end;
        {not_significant, _} ->
            false
    end.

%% @doc Calculate statistical significance using t-test
calculate_statistical_significance(CurrentValue, Baseline) ->
    case length(Baseline#baseline.samples) >= ?MIN_SAMPLES of
        true ->
            Mean = Baseline#baseline.mean,
            StdDev = Baseline#baseline.std_dev,
            N = length(Baseline#baseline.samples),
            
            % One-sample t-test
            TStatistic = (CurrentValue - Mean) / (StdDev / math:sqrt(N)),
            DegreesOfFreedom = N - 1,
            
            % Critical value for 95% confidence (approximation)
            CriticalValue = case DegreesOfFreedom > 30 of
                true -> 1.96; % Normal approximation
                false -> 2.0  % Conservative t-value
            end,
            
            PValue = calculate_p_value(abs(TStatistic), DegreesOfFreedom),
            
            case abs(TStatistic) > CriticalValue of
                true -> {significant, PValue};
                false -> {not_significant, PValue}
            end;
        false ->
            {insufficient_data, 0.0}
    end.

%% @doc Detect anomalies using z-score
detect_anomalies(Values, Baseline) ->
    Mean = Baseline#baseline.mean,
    StdDev = Baseline#baseline.std_dev,
    
    lists:filtermap(fun(Value) ->
        ZScore = abs(Value - Mean) / StdDev,
        case ZScore > ?ANOMALY_THRESHOLD of
            true -> {true, {Value, ZScore}};
            false -> false
        end
    end, Values).

%% @doc Alert on detected regression
alert_on_regression(Regressions) ->
    SpanCtx = otel_tracer:start_span(<<"alert_regression">>),
    
    try
        CriticalRegressions = lists:filter(fun(R) -> 
            R#regression_result.is_regression andalso 
            R#regression_result.severity =:= critical
        end, Regressions),
        
        case CriticalRegressions of
            [] ->
                ?LOG_INFO("No critical regressions to alert");
            _ ->
                AlertMessage = format_alert_message(CriticalRegressions),
                send_alerts(AlertMessage),
                ?LOG_CRITICAL("REGRESSION ALERT: ~s", [AlertMessage])
        end,
        
        otel_span:set_status(SpanCtx, ok)
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to send regression alert: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error)
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Calculate baseline from historical data
calculate_baseline(Values) when length(Values) >= ?MIN_SAMPLES ->
    Mean = lists:sum(Values) / length(Values),
    Variance = lists:sum([math:pow(X - Mean, 2) || X <- Values]) / (length(Values) - 1),
    StdDev = math:sqrt(Variance),
    
    % Calculate 95% confidence interval
    StandardError = StdDev / math:sqrt(length(Values)),
    Margin = 1.96 * StandardError,
    ConfidenceInterval = {Mean - Margin, Mean + Margin},
    
    #baseline{
        mean = Mean,
        std_dev = StdDev,
        samples = Values,
        timestamp = erlang:system_time(millisecond),
        confidence_interval = ConfidenceInterval
    };
calculate_baseline(Values) ->
    ?LOG_WARNING("Insufficient samples for baseline calculation: ~p", [length(Values)]),
    Mean = case Values of
        [] -> 0.0;
        _ -> lists:sum(Values) / length(Values)
    end,
    #baseline{
        mean = Mean,
        std_dev = 0.0,
        samples = Values,
        timestamp = erlang:system_time(millisecond),
        confidence_interval = {Mean, Mean}
    }.

%% @doc Check if any regression is significant
has_significant_regression(Regressions) ->
    lists:any(fun(R) -> 
        R#regression_result.is_regression andalso 
        R#regression_result.severity =/= low
    end, Regressions).

%% @doc Calculate confidence level
calculate_confidence(CurrentValue, Baseline) ->
    {Lower, Upper} = Baseline#baseline.confidence_interval,
    case CurrentValue >= Lower andalso CurrentValue =< Upper of
        true -> 0.95;
        false ->
            Distance = min(abs(CurrentValue - Lower), abs(CurrentValue - Upper)),
            Range = Upper - Lower,
            1.0 - min(Distance / Range, 0.95)
    end.

%% @doc Calculate severity of regression
calculate_severity(MetricName, ChangePercent) ->
    AbsChange = abs(ChangePercent),
    case MetricName of
        error_rate ->
            if AbsChange > 50 -> critical;
               AbsChange > 25 -> high;
               AbsChange > 10 -> medium;
               true -> low
            end;
        latency ->
            if AbsChange > 100 -> critical;
               AbsChange > 50 -> high;
               AbsChange > 25 -> medium;
               true -> low
            end;
        _ ->
            if AbsChange > 75 -> critical;
               AbsChange > 40 -> high;
               AbsChange > 20 -> medium;
               true -> low
            end
    end.

%% @doc Get threshold for specific metric
get_threshold(MetricName) ->
    case ets:lookup(regression_config, thresholds) of
        [{thresholds, Config}] ->
            maps:get(MetricName, Config, ?REGRESSION_THRESHOLD);
        [] ->
            ?REGRESSION_THRESHOLD
    end.

%% @doc Format alert message
format_alert_message(Regressions) ->
    Messages = lists:map(fun(R) ->
        io_lib:format("~p: ~.2f% change (current: ~.2f, baseline: ~.2f, severity: ~p)",
            [R#regression_result.metric_name,
             R#regression_result.change_percent,
             R#regression_result.current_value,
             R#regression_result.baseline_value,
             R#regression_result.severity])
    end, Regressions),
    string:join(Messages, "; ").

%% @doc Send alerts through configured channels
send_alerts(Message) ->
    case ets:lookup(regression_config, alert_channels) of
        [{alert_channels, Channels}] ->
            lists:foreach(fun(Channel) ->
                send_alert_to_channel(Channel, Message)
            end, Channels);
        [] ->
            ?LOG_INFO("No alert channels configured")
    end.

%% @doc Send alert to specific channel
send_alert_to_channel(log, Message) ->
    ?LOG_CRITICAL("PERFORMANCE REGRESSION: ~s", [Message]);
send_alert_to_channel(console, Message) ->
    io:format("PERFORMANCE REGRESSION ALERT: ~s~n", [Message]);
send_alert_to_channel(email, Message) ->
    % Placeholder for email integration
    ?LOG_INFO("Would send email alert: ~s", [Message]);
send_alert_to_channel(slack, Message) ->
    % Placeholder for Slack integration
    ?LOG_INFO("Would send Slack alert: ~s", [Message]);
send_alert_to_channel(Channel, Message) ->
    ?LOG_WARNING("Unknown alert channel ~p: ~s", [Channel, Message]).

%% @doc Setup default configuration
setup_default_config() ->
    DefaultConfig = #{
        latency => 0.10,      % 10% increase
        throughput => 0.10,   % 10% decrease
        error_rate => 0.05,   % 5% increase
        resource_usage => 0.15, % 15% increase
        memory_usage => 0.20, % 20% increase
        cpu_usage => 0.15     % 15% increase
    },
    ets:insert(regression_config, {thresholds, DefaultConfig}),
    ets:insert(regression_config, {alert_channels, [log, console]}),
    ets:insert(regression_config, {monitoring, false}).

%% @doc Get all current baselines
get_all_baselines() ->
    ets:tab2list(regression_baselines).

%% @doc Get recent regressions within time range
get_recent_regressions(TimeRange) ->
    % Placeholder - would query regression history
    [].

%% @doc Analyze performance trends
analyze_performance_trends(TimeRange) ->
    % Placeholder for trend analysis
    #{
        trend => stable,
        direction => improving,
        confidence => 0.85
    }.

%% @doc Generate improvement recommendations
generate_recommendations() ->
    [
        "Monitor latency trends closely",
        "Consider increasing resource allocation if CPU usage > 80%",
        "Review error handling if error rate increases",
        "Implement caching for throughput optimization"
    ].

%% @doc Calculate p-value (simplified approximation)
calculate_p_value(TStatistic, DegreesOfFreedom) ->
    % Simplified p-value calculation
    % In production, use a proper statistical library
    case TStatistic > 2.0 of
        true -> 0.05;  % Significant
        false -> 0.1   % Not significant
    end.

%% @doc Generate comprehensive regression report
generate_regression_report(Regressions, TimeRange) ->
    #{
        summary => #{
            total_metrics => length(Regressions),
            regressions_detected => length(lists:filter(
                fun(R) -> R#regression_result.is_regression end, Regressions)),
            critical_issues => length(lists:filter(
                fun(R) -> R#regression_result.severity =:= critical end, Regressions))
        },
        details => Regressions,
        time_range => TimeRange,
        recommendations => generate_specific_recommendations(Regressions)
    }.

%% @doc Generate specific recommendations based on regressions
generate_specific_recommendations(Regressions) ->
    lists:filtermap(fun(R) ->
        case R#regression_result.is_regression of
            true ->
                Recommendation = case R#regression_result.metric_name of
                    latency -> "Investigate latency spikes - check network, database, or processing bottlenecks";
                    throughput -> "Analyze throughput degradation - consider scaling or optimization";
                    error_rate -> "Critical: Review recent changes causing increased errors";
                    memory_usage -> "Monitor memory leaks or inefficient memory allocation";
                    cpu_usage -> "Check for CPU-intensive operations or inefficient algorithms";
                    _ -> "Investigate performance degradation in this metric"
                end,
                {true, #{
                    metric => R#regression_result.metric_name,
                    severity => R#regression_result.severity,
                    recommendation => Recommendation
                }};
            false ->
                false
        end
    end, Regressions).