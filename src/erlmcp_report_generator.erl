-module(erlmcp_report_generator).
-export([generate_report/2, generate_report/3, generate_summary/1]).

-include_lib("kernel/include/logger.hrl").

%% Report generation with OpenTelemetry tracing
generate_report(Results, Format) ->
    generate_report(Results, Format, #{}).

generate_report(Results, Format, Options) ->
    SpanCtx = otel_tracer:start_span(<<"report_generation">>),
    
    try
        otel_span:set_attributes(SpanCtx, [
            {<<"report.format">>, atom_to_binary(Format)},
            {<<"report.test_count">>, count_tests(Results)},
            {<<"report.options">>, format_options(Options)}
        ]),
        
        ?LOG_INFO("Starting report generation for format: ~p", [Format]),
        
        Report = #{
            summary => generate_summary(Results),
            traces => collect_traces(Results),
            metrics => aggregate_metrics(Results),
            benchmarks => format_benchmarks(Results),
            vulnerabilities => list_vulnerabilities(Results),
            regressions => identify_regressions(Results),
            metadata => generate_metadata(Results),
            graphs => prepare_graph_data(Results)
        },
        
        Output = case Format of
            html -> generate_html(Report, Options);
            json -> jiffy:encode(Report);
            markdown -> generate_markdown(Report, Options);
            pdf -> generate_pdf(Report, Options);
            csv -> generate_csv(Report)
        end,
        
        otel_span:set_attributes(SpanCtx, [
            {<<"report.size_bytes">>, byte_size(Output)},
            {<<"report.generation_status">>, <<"success">>}
        ]),
        
        ?LOG_INFO("Report generated successfully, size: ~p bytes", [byte_size(Output)]),
        {ok, Output}
    catch
        Error:Reason:Stacktrace ->
            otel_span:record_exception(SpanCtx, Error, Reason, Stacktrace),
            otel_span:set_status(SpanCtx, opentelemetry:status(error, "Report generation failed")),
            ?LOG_ERROR("Report generation failed: ~p:~p", [Error, Reason]),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% Generate executive summary
generate_summary(Results) ->
    SpanCtx = otel_tracer:start_span(<<"generate_summary">>),
    
    try
        TestStats = calculate_test_statistics(Results),
        Performance = analyze_performance(Results),
        Quality = assess_quality(Results),
        
        Summary = #{
            timestamp => os:system_time(millisecond),
            test_statistics => TestStats,
            performance_metrics => Performance,
            quality_assessment => Quality,
            overall_status => determine_overall_status(TestStats, Performance, Quality),
            recommendations => generate_recommendations(Results)
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"summary.total_tests">>, maps:get(total, TestStats, 0)},
            {<<"summary.pass_rate">>, maps:get(pass_rate, TestStats, 0.0)},
            {<<"summary.overall_status">>, maps:get(overall_status, Summary)}
        ]),
        
        Summary
    after
        otel_span:end_span(SpanCtx)
    end.

%% Collect and format traces
collect_traces(Results) ->
    SpanCtx = otel_tracer:start_span(<<"collect_traces">>),
    
    try
        Traces = maps:get(traces, Results, []),
        FormattedTraces = lists:map(fun format_trace/1, Traces),
        
        TraceStats = #{
            total_traces => length(FormattedTraces),
            avg_duration => calculate_avg_duration(FormattedTraces),
            slow_traces => identify_slow_traces(FormattedTraces),
            error_traces => filter_error_traces(FormattedTraces)
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"traces.count">>, length(FormattedTraces)},
            {<<"traces.errors">>, length(maps:get(error_traces, TraceStats))}
        ]),
        
        #{
            traces => FormattedTraces,
            statistics => TraceStats,
            flame_graph_data => generate_flame_graph_data(FormattedTraces)
        }
    after
        otel_span:end_span(SpanCtx)
    end.

%% Aggregate performance metrics
aggregate_metrics(Results) ->
    SpanCtx = otel_tracer:start_span(<<"aggregate_metrics">>),
    
    try
        Metrics = maps:get(metrics, Results, []),
        
        Aggregated = #{
            response_times => aggregate_response_times(Metrics),
            throughput => calculate_throughput(Metrics),
            error_rates => calculate_error_rates(Metrics),
            resource_usage => aggregate_resource_usage(Metrics),
            latency_percentiles => calculate_latency_percentiles(Metrics),
            time_series => prepare_time_series_data(Metrics)
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"metrics.count">>, length(Metrics)},
            {<<"metrics.avg_response_time">>, maps:get(avg, maps:get(response_times, Aggregated, #{}), 0)}
        ]),
        
        Aggregated
    after
        otel_span:end_span(SpanCtx)
    end.

%% Format benchmark results
format_benchmarks(Results) ->
    SpanCtx = otel_tracer:start_span(<<"format_benchmarks">>),
    
    try
        Benchmarks = maps:get(benchmarks, Results, []),
        
        FormattedBenchmarks = lists:map(fun(Benchmark) ->
            #{
                name => maps:get(name, Benchmark),
                duration => maps:get(duration, Benchmark),
                iterations => maps:get(iterations, Benchmark, 1),
                throughput => calculate_benchmark_throughput(Benchmark),
                memory_usage => maps:get(memory, Benchmark, 0),
                cpu_usage => maps:get(cpu, Benchmark, 0),
                comparison => compare_with_baseline(Benchmark)
            }
        end, Benchmarks),
        
        otel_span:set_attributes(SpanCtx, [
            {<<"benchmarks.count">>, length(FormattedBenchmarks)}
        ]),
        
        #{
            benchmarks => FormattedBenchmarks,
            summary => summarize_benchmarks(FormattedBenchmarks),
            trends => analyze_benchmark_trends(FormattedBenchmarks)
        }
    after
        otel_span:end_span(SpanCtx)
    end.

%% List security vulnerabilities
list_vulnerabilities(Results) ->
    SpanCtx = otel_tracer:start_span(<<"list_vulnerabilities">>),
    
    try
        SecurityResults = maps:get(security, Results, #{}),
        Vulnerabilities = maps:get(vulnerabilities, SecurityResults, []),
        
        CategorizedVulns = lists:foldl(fun(Vuln, Acc) ->
            Severity = maps:get(severity, Vuln, low),
            SeverityList = maps:get(Severity, Acc, []),
            maps:put(Severity, [Vuln | SeverityList], Acc)
        end, #{}, Vulnerabilities),
        
        otel_span:set_attributes(SpanCtx, [
            {<<"vulnerabilities.total">>, length(Vulnerabilities)},
            {<<"vulnerabilities.critical">>, length(maps:get(critical, CategorizedVulns, []))},
            {<<"vulnerabilities.high">>, length(maps:get(high, CategorizedVulns, []))}
        ]),
        
        #{
            vulnerabilities => Vulnerabilities,
            by_severity => CategorizedVulns,
            risk_score => calculate_risk_score(CategorizedVulns),
            recommendations => security_recommendations(CategorizedVulns)
        }
    after
        otel_span:end_span(SpanCtx)
    end.

%% Identify performance regressions
identify_regressions(Results) ->
    SpanCtx = otel_tracer:start_span(<<"identify_regressions">>),
    
    try
        CurrentMetrics = maps:get(metrics, Results, []),
        BaselineMetrics = maps:get(baseline_metrics, Results, []),
        
        Regressions = case BaselineMetrics of
            [] -> 
                #{regressions => [], status => no_baseline};
            _ ->
                DetectedRegressions = detect_regressions(CurrentMetrics, BaselineMetrics),
                #{
                    regressions => DetectedRegressions,
                    status => case DetectedRegressions of
                        [] -> no_regressions;
                        _ -> regressions_detected
                    end,
                    severity => assess_regression_severity(DetectedRegressions)
                }
        end,
        
        otel_span:set_attributes(SpanCtx, [
            {<<"regressions.count">>, length(maps:get(regressions, Regressions, []))}
        ]),
        
        Regressions
    after
        otel_span:end_span(SpanCtx)
    end.

%% Generate HTML report with interactive elements
generate_html(Report, Options) ->
    SpanCtx = otel_tracer:start_span(<<"generate_html">>),
    
    try
        Template = maps:get(template, Options, default),
        IncludeCharts = maps:get(include_charts, Options, true),
        
        HTML = [
            generate_html_header(Report, IncludeCharts),
            generate_html_summary(maps:get(summary, Report)),
            generate_html_test_results(Report),
            generate_html_performance_section(maps:get(metrics, Report)),
            generate_html_trace_visualization(maps:get(traces, Report)),
            generate_html_benchmarks(maps:get(benchmarks, Report)),
            generate_html_security_section(maps:get(vulnerabilities, Report)),
            generate_html_regressions(maps:get(regressions, Report)),
            generate_html_footer()
        ],
        
        otel_span:set_attributes(SpanCtx, [
            {<<"html.template">>, atom_to_binary(Template)},
            {<<"html.includes_charts">>, IncludeCharts}
        ]),
        
        iolist_to_binary(HTML)
    after
        otel_span:end_span(SpanCtx)
    end.

%% Generate Markdown report
generate_markdown(Report, Options) ->
    SpanCtx = otel_tracer:start_span(<<"generate_markdown">>),
    
    try
        IncludeDetails = maps:get(include_details, Options, true),
        
        Markdown = [
            "# ErlMCP Test Report\n\n",
            generate_markdown_summary(maps:get(summary, Report)),
            "\n## Test Results\n\n",
            generate_markdown_test_results(Report),
            "\n## Performance Metrics\n\n",
            generate_markdown_metrics(maps:get(metrics, Report)),
            "\n## Benchmarks\n\n",
            generate_markdown_benchmarks(maps:get(benchmarks, Report)),
            case IncludeDetails of
                true -> [
                    "\n## Detailed Traces\n\n",
                    generate_markdown_traces(maps:get(traces, Report)),
                    "\n## Security Analysis\n\n",
                    generate_markdown_security(maps:get(vulnerabilities, Report))
                ];
                false -> []
            end,
            "\n## Regressions\n\n",
            generate_markdown_regressions(maps:get(regressions, Report))
        ],
        
        iolist_to_binary(Markdown)
    after
        otel_span:end_span(SpanCtx)
    end.

%% Helper functions for calculations and formatting

calculate_test_statistics(Results) ->
    Tests = maps:get(tests, Results, []),
    Total = length(Tests),
    Passed = length(lists:filter(fun(T) -> maps:get(status, T) =:= passed end, Tests)),
    Failed = length(lists:filter(fun(T) -> maps:get(status, T) =:= failed end, Tests)),
    Skipped = length(lists:filter(fun(T) -> maps:get(status, T) =:= skipped end, Tests)),
    
    #{
        total => Total,
        passed => Passed,
        failed => Failed,
        skipped => Skipped,
        pass_rate => case Total of 0 -> 0.0; _ -> Passed / Total end,
        failure_rate => case Total of 0 -> 0.0; _ -> Failed / Total end
    }.

analyze_performance(Results) ->
    Metrics = maps:get(metrics, Results, []),
    ResponseTimes = [maps:get(response_time, M, 0) || M <- Metrics],
    
    case ResponseTimes of
        [] -> #{avg => 0, min => 0, max => 0, p95 => 0, p99 => 0};
        _ ->
            Sorted = lists:sort(ResponseTimes),
            Length = length(Sorted),
            #{
                avg => lists:sum(ResponseTimes) / Length,
                min => lists:min(ResponseTimes),
                max => lists:max(ResponseTimes),
                p95 => lists:nth(round(Length * 0.95), Sorted),
                p99 => lists:nth(round(Length * 0.99), Sorted)
            }
    end.

assess_quality(Results) ->
    TestStats = calculate_test_statistics(Results),
    SecurityIssues = length(maps:get(vulnerabilities, maps:get(vulnerabilities, Results, #{}), [])),
    CoverageData = maps:get(coverage, Results, #{}),
    
    QualityScore = calculate_quality_score(TestStats, SecurityIssues, CoverageData),
    
    #{
        score => QualityScore,
        grade => quality_grade(QualityScore),
        coverage => maps:get(percentage, CoverageData, 0),
        security_issues => SecurityIssues,
        test_quality => assess_test_quality(TestStats)
    }.

determine_overall_status(TestStats, Performance, Quality) ->
    PassRate = maps:get(pass_rate, TestStats, 0),
    AvgResponseTime = maps:get(avg, Performance, 0),
    QualityScore = maps:get(score, Quality, 0),
    
    if
        PassRate >= 0.95 andalso AvgResponseTime < 100 andalso QualityScore >= 80 ->
            excellent;
        PassRate >= 0.90 andalso AvgResponseTime < 200 andalso QualityScore >= 70 ->
            good;
        PassRate >= 0.80 andalso AvgResponseTime < 500 andalso QualityScore >= 60 ->
            acceptable;
        true ->
            needs_improvement
    end.

generate_recommendations(Results) ->
    Recommendations = [],
    
    % Add performance recommendations
    Performance = analyze_performance(Results),
    PerfRecommendations = case maps:get(avg, Performance, 0) of
        Avg when Avg > 1000 -> [<<"Consider optimizing slow operations">>];
        _ -> []
    end,
    
    % Add test recommendations
    TestStats = calculate_test_statistics(Results),
    TestRecommendations = case maps:get(pass_rate, TestStats, 1.0) of
        Rate when Rate < 0.95 -> [<<"Investigate and fix failing tests">>];
        _ -> []
    end,
    
    % Add security recommendations
    SecurityIssues = maps:get(vulnerabilities, maps:get(vulnerabilities, Results, #{}), []),
    SecurityRecommendations = case length(SecurityIssues) of
        Count when Count > 0 -> [<<"Address security vulnerabilities">>];
        _ -> []
    end,
    
    lists:flatten([Recommendations, PerfRecommendations, TestRecommendations, SecurityRecommendations]).

%% Utility functions
count_tests(Results) ->
    length(maps:get(tests, Results, [])).

format_options(Options) ->
    maps:fold(fun(K, V, Acc) ->
        [{atom_to_binary(K), format_option_value(V)} | Acc]
    end, [], Options).

format_option_value(V) when is_atom(V) -> atom_to_binary(V);
format_option_value(V) when is_binary(V) -> V;
format_option_value(V) when is_integer(V) -> integer_to_binary(V);
format_option_value(V) when is_float(V) -> float_to_binary(V);
format_option_value(V) -> iolist_to_binary(io_lib:format("~p", [V])).

atom_to_binary(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom));
atom_to_binary(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

generate_metadata(Results) ->
    #{
        generated_at => os:system_time(millisecond),
        version => <<"1.0.0">>,
        node => node(),
        system_info => #{
            otp_release => erlang:system_info(otp_release),
            system_version => erlang:system_info(system_version),
            process_count => erlang:system_info(process_count),
            memory => erlang:memory()
        }
    }.

%% Placeholder implementations for remaining helper functions
format_trace(Trace) -> Trace.
calculate_avg_duration(_) -> 0.
identify_slow_traces(Traces) -> lists:sublist(Traces, 5).
filter_error_traces(Traces) -> lists:filter(fun(T) -> maps:get(status, T, ok) =/= ok end, Traces).
generate_flame_graph_data(_) -> #{}.
aggregate_response_times(_) -> #{avg => 0, min => 0, max => 0}.
calculate_throughput(_) -> 0.
calculate_error_rates(_) -> 0.
aggregate_resource_usage(_) -> #{cpu => 0, memory => 0}.
calculate_latency_percentiles(_) -> #{p50 => 0, p95 => 0, p99 => 0}.
prepare_time_series_data(_) -> [].
prepare_graph_data(_) -> #{}.
calculate_benchmark_throughput(_) -> 0.
compare_with_baseline(_) -> no_baseline.
summarize_benchmarks(_) -> #{}.
analyze_benchmark_trends(_) -> #{}.
calculate_risk_score(_) -> 0.
security_recommendations(_) -> [].
detect_regressions(_, _) -> [].
assess_regression_severity(_) -> low.
calculate_quality_score(_, _, _) -> 75.
quality_grade(Score) when Score >= 90 -> excellent;
quality_grade(Score) when Score >= 80 -> good;
quality_grade(Score) when Score >= 70 -> acceptable;
quality_grade(_) -> needs_improvement.
assess_test_quality(_) -> good.

%% HTML generation functions
generate_html_header(_, _) -> <<"<!DOCTYPE html><html><head><title>ErlMCP Test Report</title></head><body>">>.
generate_html_summary(_) -> <<"<h1>Summary</h1>">>.
generate_html_test_results(_) -> <<"<h2>Test Results</h2>">>.
generate_html_performance_section(_) -> <<"<h2>Performance</h2>">>.
generate_html_trace_visualization(_) -> <<"<h2>Traces</h2>">>.
generate_html_benchmarks(_) -> <<"<h2>Benchmarks</h2>">>.
generate_html_security_section(_) -> <<"<h2>Security</h2>">>.
generate_html_regressions(_) -> <<"<h2>Regressions</h2>">>.
generate_html_footer() -> <<"</body></html>">>.

%% Markdown generation functions
generate_markdown_summary(_) -> "## Summary\n".
generate_markdown_test_results(_) -> "Test results summary\n".
generate_markdown_metrics(_) -> "Performance metrics\n".
generate_markdown_benchmarks(_) -> "Benchmark results\n".
generate_markdown_traces(_) -> "Trace details\n".
generate_markdown_security(_) -> "Security analysis\n".
generate_markdown_regressions(_) -> "Regression analysis\n".

%% Additional format generators
generate_pdf(_, _) -> <<"PDF content placeholder">>.
generate_csv(_) -> <<"CSV content placeholder">>.