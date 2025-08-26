-module(erlmcp_report_metrics).
-export([
    collect_system_metrics/0,
    analyze_performance_trends/2,
    calculate_regression_impact/2,
    generate_benchmark_comparison/2,
    assess_resource_utilization/1,
    create_trace_flame_graph/1,
    calculate_quality_score/3,
    identify_performance_bottlenecks/1
]).

-include_lib("kernel/include/logger.hrl").

%% Collect comprehensive system metrics
collect_system_metrics() ->
    SpanCtx = otel_tracer:start_span(<<"collect_system_metrics">>),
    
    try
        Metrics = #{
            timestamp => os:system_time(millisecond),
            system => collect_system_info(),
            memory => collect_memory_metrics(),
            process => collect_process_metrics(),
            network => collect_network_metrics(),
            disk => collect_disk_metrics(),
            gc => collect_gc_metrics(),
            scheduler => collect_scheduler_metrics(),
            ports => collect_port_metrics(),
            ets => collect_ets_metrics()
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"metrics.memory_total">>, maps:get(total, maps:get(memory, Metrics, #{}), 0)},
            {<<"metrics.process_count">>, maps:get(process_count, maps:get(system, Metrics, #{}), 0)},
            {<<"metrics.port_count">>, maps:get(port_count, maps:get(system, Metrics, #{}), 0)}
        ]),
        
        {ok, Metrics}
    catch
        Error:Reason:Stacktrace ->
            otel_span:record_exception(SpanCtx, Error, Reason, Stacktrace),
            ?LOG_ERROR("Failed to collect system metrics: ~p:~p", [Error, Reason]),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% Analyze performance trends over time
analyze_performance_trends(CurrentMetrics, HistoricalMetrics) ->
    SpanCtx = otel_tracer:start_span(<<"analyze_performance_trends">>),
    
    try
        Trends = #{
            response_time => calculate_trend(CurrentMetrics, HistoricalMetrics, response_time),
            throughput => calculate_trend(CurrentMetrics, HistoricalMetrics, throughput),
            error_rate => calculate_trend(CurrentMetrics, HistoricalMetrics, error_rate),
            memory_usage => calculate_trend(CurrentMetrics, HistoricalMetrics, memory_usage),
            cpu_utilization => calculate_trend(CurrentMetrics, HistoricalMetrics, cpu_utilization),
            gc_frequency => calculate_trend(CurrentMetrics, HistoricalMetrics, gc_frequency)
        },
        
        TrendSummary = #{
            improving => count_improving_trends(Trends),
            degrading => count_degrading_trends(Trends),
            stable => count_stable_trends(Trends),
            overall_direction => determine_overall_trend(Trends),
            significant_changes => identify_significant_changes(Trends)
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"trends.improving_count">>, maps:get(improving, TrendSummary)},
            {<<"trends.degrading_count">>, maps:get(degrading, TrendSummary)},
            {<<"trends.overall">>, atom_to_binary(maps:get(overall_direction, TrendSummary))}
        ]),
        
        #{trends => Trends, summary => TrendSummary}
    after
        otel_span:end_span(SpanCtx)
    end.

%% Calculate regression impact analysis
calculate_regression_impact(CurrentResults, BaselineResults) ->
    SpanCtx = otel_tracer:start_span(<<"calculate_regression_impact">>),
    
    try
        Regressions = detect_regressions(CurrentResults, BaselineResults),
        
        ImpactAnalysis = lists:map(fun(Regression) ->
            Metric = maps:get(metric, Regression),
            Current = maps:get(current, Regression),
            Baseline = maps:get(baseline, Regression),
            PercentChange = ((Current - Baseline) / Baseline) * 100,
            
            Impact = case abs(PercentChange) of
                Change when Change >= 50 -> critical;
                Change when Change >= 25 -> high;
                Change when Change >= 10 -> medium;
                _ -> low
            end,
            
            BusinessImpact = assess_business_impact(Metric, PercentChange),
            TechnicalImpact = assess_technical_impact(Metric, PercentChange),
            
            #{
                metric => Metric,
                current_value => Current,
                baseline_value => Baseline,
                percent_change => PercentChange,
                impact_severity => Impact,
                business_impact => BusinessImpact,
                technical_impact => TechnicalImpact,
                recommended_action => recommend_action(Impact, PercentChange),
                priority => calculate_priority(Impact, BusinessImpact, TechnicalImpact)
            }
        end, Regressions),
        
        Summary = #{
            total_regressions => length(ImpactAnalysis),
            critical_count => count_by_impact(ImpactAnalysis, critical),
            high_count => count_by_impact(ImpactAnalysis, high),
            medium_count => count_by_impact(ImpactAnalysis, medium),
            low_count => count_by_impact(ImpactAnalysis, low),
            avg_impact => calculate_avg_impact(ImpactAnalysis),
            most_critical => find_most_critical(ImpactAnalysis)
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"regressions.total">>, maps:get(total_regressions, Summary)},
            {<<"regressions.critical">>, maps:get(critical_count, Summary)},
            {<<"regressions.avg_impact">>, maps:get(avg_impact, Summary)}
        ]),
        
        #{regressions => ImpactAnalysis, summary => Summary}
    after
        otel_span:end_span(SpanCtx)
    end.

%% Generate comprehensive benchmark comparison
generate_benchmark_comparison(CurrentBenchmarks, BaselineBenchmarks) ->
    SpanCtx = otel_tracer:start_span(<<"generate_benchmark_comparison">>),
    
    try
        Comparisons = lists:map(fun(CurrentBench) ->
            Name = maps:get(name, CurrentBench),
            BaselineBench = find_benchmark_by_name(Name, BaselineBenchmarks),
            
            case BaselineBench of
                undefined ->
                    #{
                        name => Name,
                        status => new_benchmark,
                        current => CurrentBench,
                        baseline => undefined,
                        improvement => undefined
                    };
                _ ->
                    Improvement = calculate_benchmark_improvement(CurrentBench, BaselineBench),
                    #{
                        name => Name,
                        status => comparison_status(Improvement),
                        current => CurrentBench,
                        baseline => BaselineBench,
                        improvement => Improvement,
                        detailed_metrics => compare_detailed_metrics(CurrentBench, BaselineBench)
                    }
            end
        end, CurrentBenchmarks),
        
        ComparisonSummary = #{
            total_benchmarks => length(Comparisons),
            improved => count_by_status(Comparisons, improved),
            degraded => count_by_status(Comparisons, degraded),
            stable => count_by_status(Comparisons, stable),
            new_benchmarks => count_by_status(Comparisons, new_benchmark),
            overall_performance_change => calculate_overall_performance_change(Comparisons),
            top_improvements => find_top_improvements(Comparisons, 3),
            top_degradations => find_top_degradations(Comparisons, 3)
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"benchmarks.total">>, maps:get(total_benchmarks, ComparisonSummary)},
            {<<"benchmarks.improved">>, maps:get(improved, ComparisonSummary)},
            {<<"benchmarks.degraded">>, maps:get(degraded, ComparisonSummary)}
        ]),
        
        #{comparisons => Comparisons, summary => ComparisonSummary}
    after
        otel_span:end_span(SpanCtx)
    end.

%% Assess resource utilization patterns
assess_resource_utilization(Metrics) ->
    SpanCtx = otel_tracer:start_span(<<"assess_resource_utilization">>),
    
    try
        SystemMetrics = maps:get(system, Metrics, #{}),
        MemoryMetrics = maps:get(memory, Metrics, #{}),
        ProcessMetrics = maps:get(process, Metrics, #{}),
        
        MemoryUtilization = analyze_memory_utilization(MemoryMetrics),
        ProcessUtilization = analyze_process_utilization(ProcessMetrics),
        SchedulerUtilization = analyze_scheduler_utilization(maps:get(scheduler, Metrics, #{})),
        PortUtilization = analyze_port_utilization(maps:get(ports, Metrics, #{})),
        ETSUtilization = analyze_ets_utilization(maps:get(ets, Metrics, #{})),
        
        OverallAssessment = #{
            memory => MemoryUtilization,
            process => ProcessUtilization,
            scheduler => SchedulerUtilization,
            ports => PortUtilization,
            ets => ETSUtilization,
            bottlenecks => identify_resource_bottlenecks([
                MemoryUtilization,
                ProcessUtilization,
                SchedulerUtilization,
                PortUtilization,
                ETSUtilization
            ]),
            recommendations => generate_resource_recommendations([
                MemoryUtilization,
                ProcessUtilization,
                SchedulerUtilization
            ])
        },
        
        OverallScore = calculate_resource_efficiency_score(OverallAssessment),
        
        Result = OverallAssessment#{
            efficiency_score => OverallScore,
            efficiency_grade => efficiency_grade(OverallScore)
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"utilization.efficiency_score">>, OverallScore},
            {<<"utilization.memory_usage_pct">>, maps:get(usage_percentage, MemoryUtilization, 0)},
            {<<"utilization.process_count">>, maps:get(active_processes, ProcessUtilization, 0)}
        ]),
        
        Result
    after
        otel_span:end_span(SpanCtx)
    end.

%% Create interactive flame graph data from traces
create_trace_flame_graph(Traces) ->
    SpanCtx = otel_tracer:start_span(<<"create_trace_flame_graph">>),
    
    try
        % Convert traces to flame graph format
        FlameGraphData = lists:foldl(fun(Trace, Acc) ->
            TraceId = maps:get(trace_id, Trace, <<"unknown">>),
            Spans = maps:get(spans, Trace, []),
            
            % Build hierarchical structure
            SpanHierarchy = build_span_hierarchy(Spans),
            FlameData = convert_to_flame_format(SpanHierarchy, TraceId),
            
            merge_flame_data(Acc, FlameData)
        end, #{}, Traces),
        
        % Generate visualization data
        VisualizationData = #{
            flame_graph => FlameGraphData,
            summary => #{
                total_traces => length(Traces),
                total_spans => count_total_spans(Traces),
                max_depth => calculate_max_depth(FlameGraphData),
                hot_paths => identify_hot_paths(FlameGraphData),
                slow_operations => identify_slow_operations(FlameGraphData)
            },
            interactive_data => generate_interactive_flame_data(FlameGraphData),
            color_scheme => generate_color_scheme(FlameGraphData)
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"flame_graph.traces_count">>, length(Traces)},
            {<<"flame_graph.max_depth">>, maps:get(max_depth, maps:get(summary, VisualizationData))},
            {<<"flame_graph.hot_paths_count">>, length(maps:get(hot_paths, maps:get(summary, VisualizationData)))}
        ]),
        
        VisualizationData
    after
        otel_span:end_span(SpanCtx)
    end.

%% Calculate comprehensive quality score
calculate_quality_score(TestResults, SecurityResults, CoverageData) ->
    SpanCtx = otel_tracer:start_span(<<"calculate_quality_score">>),
    
    try
        % Test quality scoring (40% of total)
        TestScore = calculate_test_quality_score(TestResults),
        
        % Security scoring (30% of total)  
        SecurityScore = calculate_security_quality_score(SecurityResults),
        
        % Coverage scoring (20% of total)
        CoverageScore = calculate_coverage_quality_score(CoverageData),
        
        % Code quality scoring (10% of total)
        CodeQualityScore = calculate_code_quality_score(TestResults),
        
        WeightedScore = (TestScore * 0.4) + 
                       (SecurityScore * 0.3) + 
                       (CoverageScore * 0.2) + 
                       (CodeQualityScore * 0.1),
        
        QualityBreakdown = #{
            overall_score => round(WeightedScore),
            test_quality => #{score => TestScore, weight => 40},
            security_quality => #{score => SecurityScore, weight => 30},
            coverage_quality => #{score => CoverageScore, weight => 20},
            code_quality => #{score => CodeQualityScore, weight => 10},
            grade => quality_grade(WeightedScore),
            recommendations => generate_quality_recommendations([
                {test_quality, TestScore},
                {security_quality, SecurityScore},
                {coverage_quality, CoverageScore},
                {code_quality, CodeQualityScore}
            ])
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"quality.overall_score">>, round(WeightedScore)},
            {<<"quality.test_score">>, TestScore},
            {<<"quality.security_score">>, SecurityScore},
            {<<"quality.coverage_score">>, CoverageScore}
        ]),
        
        QualityBreakdown
    after
        otel_span:end_span(SpanCtx)
    end.

%% Identify performance bottlenecks
identify_performance_bottlenecks(SystemMetrics) ->
    SpanCtx = otel_tracer:start_span(<<"identify_performance_bottlenecks">>),
    
    try
        BottleneckAnalysis = [
            analyze_memory_bottlenecks(maps:get(memory, SystemMetrics, #{})),
            analyze_process_bottlenecks(maps:get(process, SystemMetrics, #{})),
            analyze_scheduler_bottlenecks(maps:get(scheduler, SystemMetrics, #{})),
            analyze_io_bottlenecks(maps:get(disk, SystemMetrics, #{})),
            analyze_network_bottlenecks(maps:get(network, SystemMetrics, #{})),
            analyze_gc_bottlenecks(maps:get(gc, SystemMetrics, #{})),
            analyze_port_bottlenecks(maps:get(ports, SystemMetrics, #{})),
            analyze_ets_bottlenecks(maps:get(ets, SystemMetrics, #{}))
        ],
        
        % Filter and prioritize bottlenecks
        SignificantBottlenecks = lists:filter(fun(Bottleneck) ->
            maps:get(severity, Bottleneck, low) =/= low
        end, lists:flatten(BottleneckAnalysis)),
        
        PrioritizedBottlenecks = lists:sort(fun(A, B) ->
            priority_value(maps:get(severity, A)) >= priority_value(maps:get(severity, B))
        end, SignificantBottlenecks),
        
        BottleneckSummary = #{
            total_bottlenecks => length(SignificantBottlenecks),
            critical_count => count_by_severity(SignificantBottlenecks, critical),
            high_count => count_by_severity(SignificantBottlenecks, high),
            medium_count => count_by_severity(SignificantBottlenecks, medium),
            most_critical => lists:sublist(PrioritizedBottlenecks, 3),
            system_health => assess_system_health(SignificantBottlenecks),
            recommendations => generate_bottleneck_recommendations(PrioritizedBottlenecks)
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"bottlenecks.total">>, maps:get(total_bottlenecks, BottleneckSummary)},
            {<<"bottlenecks.critical">>, maps:get(critical_count, BottleneckSummary)},
            {<<"bottlenecks.system_health">>, atom_to_binary(maps:get(system_health, BottleneckSummary))}
        ]),
        
        #{
            bottlenecks => PrioritizedBottlenecks,
            summary => BottleneckSummary
        }
    after
        otel_span:end_span(SpanCtx)
    end.

%% Helper Functions for System Metrics Collection

collect_system_info() ->
    #{
        otp_release => list_to_binary(erlang:system_info(otp_release)),
        system_version => list_to_binary(erlang:system_info(system_version)),
        process_count => erlang:system_info(process_count),
        process_limit => erlang:system_info(process_limit),
        port_count => erlang:system_info(port_count),
        port_limit => erlang:system_info(port_limit),
        atom_count => erlang:system_info(atom_count),
        atom_limit => erlang:system_info(atom_limit),
        ets_count => length(ets:all()),
        ets_limit => erlang:system_info(ets_limit),
        logical_processors => erlang:system_info(logical_processors),
        schedulers => erlang:system_info(schedulers),
        thread_pool_size => erlang:system_info(thread_pool_size)
    }.

collect_memory_metrics() ->
    MemoryInfo = erlang:memory(),
    #{
        total => proplists:get_value(total, MemoryInfo, 0),
        processes => proplists:get_value(processes, MemoryInfo, 0),
        processes_used => proplists:get_value(processes_used, MemoryInfo, 0),
        system => proplists:get_value(system, MemoryInfo, 0),
        atom => proplists:get_value(atom, MemoryInfo, 0),
        atom_used => proplists:get_value(atom_used, MemoryInfo, 0),
        binary => proplists:get_value(binary, MemoryInfo, 0),
        code => proplists:get_value(code, MemoryInfo, 0),
        ets => proplists:get_value(ets, MemoryInfo, 0)
    }.

collect_process_metrics() ->
    ProcessInfo = [erlang:process_info(P) || P <- processes()],
    ValidProcessInfo = [P || P <- ProcessInfo, P =/= undefined],
    
    MessageQueueLengths = [proplists:get_value(message_queue_len, P, 0) || P <- ValidProcessInfo],
    HeapSizes = [proplists:get_value(heap_size, P, 0) || P <- ValidProcessInfo],
    Reductions = [proplists:get_value(reductions, P, 0) || P <- ValidProcessInfo],
    
    #{
        active_processes => length(ValidProcessInfo),
        avg_message_queue_len => safe_average(MessageQueueLengths),
        max_message_queue_len => safe_max(MessageQueueLengths),
        avg_heap_size => safe_average(HeapSizes),
        max_heap_size => safe_max(HeapSizes),
        total_reductions => lists:sum(Reductions),
        avg_reductions => safe_average(Reductions)
    }.

collect_network_metrics() ->
    % This is a simplified implementation
    % In a real system, you would collect actual network statistics
    #{
        connections => length(inet:getstat()),
        bytes_in => 0,
        bytes_out => 0,
        packets_in => 0,
        packets_out => 0
    }.

collect_disk_metrics() ->
    % Simplified disk metrics
    #{
        reads => 0,
        writes => 0,
        bytes_read => 0,
        bytes_written => 0
    }.

collect_gc_metrics() ->
    Statistics = erlang:statistics(garbage_collection),
    #{
        number_of_gcs => element(1, Statistics),
        words_reclaimed => element(2, Statistics),
        reductions_during_gc => element(3, Statistics)
    }.

collect_scheduler_metrics() ->
    SchedulerInfo = erlang:system_info(scheduler_utilization),
    case SchedulerInfo of
        undefined -> #{utilization => 0, schedulers => []};
        Utilization ->
            #{
                utilization => proplists:get_value(total, Utilization, 0),
                schedulers => proplists:get_value(normal, Utilization, [])
            }
    end.

collect_port_metrics() ->
    Ports = erlang:ports(),
    PortInfo = [erlang:port_info(P) || P <- Ports],
    ValidPortInfo = [P || P <- PortInfo, P =/= undefined],
    
    #{
        active_ports => length(ValidPortInfo),
        total_input => sum_port_stat(ValidPortInfo, input),
        total_output => sum_port_stat(ValidPortInfo, output)
    }.

collect_ets_metrics() ->
    AllTables = ets:all(),
    TableInfo = [try ets:info(T) catch _:_ -> undefined end || T <- AllTables],
    ValidTableInfo = [T || T <- TableInfo, T =/= undefined],
    
    #{
        table_count => length(ValidTableInfo),
        total_memory => lists:sum([proplists:get_value(memory, T, 0) || T <- ValidTableInfo]),
        total_objects => lists:sum([proplists:get_value(size, T, 0) || T <- ValidTableInfo])
    }.

%% Helper functions for calculations

safe_average([]) -> 0;
safe_average(List) -> lists:sum(List) / length(List).

safe_max([]) -> 0;
safe_max(List) -> lists:max(List).

sum_port_stat(PortInfo, Stat) ->
    lists:sum([proplists:get_value(Stat, P, 0) || P <- PortInfo]).

calculate_trend(Current, Historical, Metric) ->
    CurrentValue = extract_metric_value(Current, Metric),
    HistoricalValues = [extract_metric_value(H, Metric) || H <- Historical],
    
    case HistoricalValues of
        [] -> #{direction => unknown, change => 0, confidence => low};
        _ ->
            AvgHistorical = lists:sum(HistoricalValues) / length(HistoricalValues),
            Change = case AvgHistorical of
                0 -> 0;
                _ -> ((CurrentValue - AvgHistorical) / AvgHistorical) * 100
            end,
            
            Direction = case Change of
                C when C > 5 -> improving;
                C when C < -5 -> degrading;
                _ -> stable
            end,
            
            #{
                direction => Direction,
                change => Change,
                confidence => calculate_confidence(length(HistoricalValues)),
                current_value => CurrentValue,
                historical_avg => AvgHistorical
            }
    end.

extract_metric_value(Metrics, response_time) ->
    maps:get(avg_response_time, Metrics, 0);
extract_metric_value(Metrics, throughput) ->
    maps:get(throughput, Metrics, 0);
extract_metric_value(Metrics, error_rate) ->
    maps:get(error_rate, Metrics, 0);
extract_metric_value(Metrics, memory_usage) ->
    maps:get(memory_total, Metrics, 0);
extract_metric_value(Metrics, cpu_utilization) ->
    maps:get(cpu_utilization, Metrics, 0);
extract_metric_value(Metrics, gc_frequency) ->
    maps:get(gc_count, Metrics, 0);
extract_metric_value(_, _) -> 0.

calculate_confidence(SampleSize) when SampleSize >= 10 -> high;
calculate_confidence(SampleSize) when SampleSize >= 5 -> medium;
calculate_confidence(_) -> low.

count_improving_trends(Trends) ->
    maps:fold(fun(_, #{direction := improving}, Acc) -> Acc + 1;
                 (_, _, Acc) -> Acc
              end, 0, Trends).

count_degrading_trends(Trends) ->
    maps:fold(fun(_, #{direction := degrading}, Acc) -> Acc + 1;
                 (_, _, Acc) -> Acc
              end, 0, Trends).

count_stable_trends(Trends) ->
    maps:fold(fun(_, #{direction := stable}, Acc) -> Acc + 1;
                 (_, _, Acc) -> Acc
              end, 0, Trends).

determine_overall_trend(Trends) ->
    Improving = count_improving_trends(Trends),
    Degrading = count_degrading_trends(Trends),
    Stable = count_stable_trends(Trends),
    
    if
        Improving > Degrading + Stable -> improving;
        Degrading > Improving + Stable -> degrading;
        true -> stable
    end.

identify_significant_changes(Trends) ->
    maps:fold(fun(Metric, #{change := Change} = Trend, Acc) ->
        case abs(Change) >= 10 of  % 10% threshold for significance
            true -> [Trend#{metric => Metric} | Acc];
            false -> Acc
        end
    end, [], Trends).

%% Placeholder implementations for remaining functions
detect_regressions(_, _) -> [].
assess_business_impact(_, _) -> low.
assess_technical_impact(_, _) -> low.
recommend_action(_, _) -> <<"Monitor performance">>.
calculate_priority(_, _, _) -> medium.
count_by_impact(List, Impact) ->
    length(lists:filter(fun(#{impact_severity := I}) -> I =:= Impact; (_) -> false end, List)).
calculate_avg_impact(_) -> 0.0.
find_most_critical(List) ->
    case lists:filter(fun(#{impact_severity := critical}) -> true; (_) -> false end, List) of
        [] -> undefined;
        [H|_] -> H
    end.

find_benchmark_by_name(Name, Benchmarks) ->
    lists:search(fun(B) -> maps:get(name, B) =:= Name end, Benchmarks).

calculate_benchmark_improvement(Current, Baseline) ->
    CurrentDuration = maps:get(duration, Current, 0),
    BaselineDuration = maps:get(duration, Baseline, 0),
    
    case BaselineDuration of
        0 -> 0;
        _ -> ((BaselineDuration - CurrentDuration) / BaselineDuration) * 100
    end.

comparison_status(Improvement) when Improvement > 5 -> improved;
comparison_status(Improvement) when Improvement < -5 -> degraded;
comparison_status(_) -> stable.

compare_detailed_metrics(Current, Baseline) ->
    #{
        duration_change => calculate_change(
            maps:get(duration, Current, 0),
            maps:get(duration, Baseline, 0)
        ),
        memory_change => calculate_change(
            maps:get(memory, Current, 0),
            maps:get(memory, Baseline, 0)
        ),
        cpu_change => calculate_change(
            maps:get(cpu, Current, 0),
            maps:get(cpu, Baseline, 0)
        )
    }.

calculate_change(Current, Baseline) when Baseline > 0 ->
    ((Current - Baseline) / Baseline) * 100;
calculate_change(_, _) -> 0.

count_by_status(Comparisons, Status) ->
    length(lists:filter(fun(#{status := S}) -> S =:= Status; (_) -> false end, Comparisons)).

calculate_overall_performance_change(Comparisons) ->
    Improvements = [maps:get(improvement, C, 0) || C <- Comparisons, 
                   maps:is_key(improvement, C)],
    case Improvements of
        [] -> 0;
        _ -> lists:sum(Improvements) / length(Improvements)
    end.

find_top_improvements(Comparisons, N) ->
    Improved = lists:filter(fun(#{status := improved}) -> true; (_) -> false end, Comparisons),
    Sorted = lists:sort(fun(A, B) -> 
        maps:get(improvement, A, 0) >= maps:get(improvement, B, 0)
    end, Improved),
    lists:sublist(Sorted, N).

find_top_degradations(Comparisons, N) ->
    Degraded = lists:filter(fun(#{status := degraded}) -> true; (_) -> false end, Comparisons),
    Sorted = lists:sort(fun(A, B) ->
        maps:get(improvement, A, 0) =< maps:get(improvement, B, 0)
    end, Degraded),
    lists:sublist(Sorted, N).

%% Resource utilization analysis functions
analyze_memory_utilization(MemoryMetrics) ->
    Total = maps:get(total, MemoryMetrics, 0),
    ProcessesUsed = maps:get(processes_used, MemoryMetrics, 0),
    System = maps:get(system, MemoryMetrics, 0),
    
    ProcessUsagePercent = case Total of
        0 -> 0;
        _ -> (ProcessesUsed / Total) * 100
    end,
    
    SystemUsagePercent = case Total of
        0 -> 0;
        _ -> (System / Total) * 100
    end,
    
    Status = case ProcessUsagePercent + SystemUsagePercent of
        Usage when Usage > 90 -> critical;
        Usage when Usage > 75 -> high;
        Usage when Usage > 50 -> medium;
        _ -> low
    end,
    
    #{
        status => Status,
        usage_percentage => ProcessUsagePercent + SystemUsagePercent,
        process_percentage => ProcessUsagePercent,
        system_percentage => SystemUsagePercent,
        total_bytes => Total,
        fragmentation => calculate_fragmentation(MemoryMetrics)
    }.

analyze_process_utilization(ProcessMetrics) ->
    ActiveProcesses = maps:get(active_processes, ProcessMetrics, 0),
    AvgQueueLen = maps:get(avg_message_queue_len, ProcessMetrics, 0),
    MaxQueueLen = maps:get(max_message_queue_len, ProcessMetrics, 0),
    
    Status = case {ActiveProcesses, MaxQueueLen} of
        {P, _} when P > 50000 -> critical;
        {P, Q} when P > 20000; Q > 1000 -> high;
        {P, Q} when P > 5000; Q > 100 -> medium;
        _ -> low
    end,
    
    #{
        status => Status,
        active_processes => ActiveProcesses,
        avg_queue_length => AvgQueueLen,
        max_queue_length => MaxQueueLen,
        process_efficiency => calculate_process_efficiency(ProcessMetrics)
    }.

analyze_scheduler_utilization(SchedulerMetrics) ->
    Utilization = maps:get(utilization, SchedulerMetrics, 0),
    
    Status = case Utilization of
        U when U > 95 -> critical;
        U when U > 80 -> high;
        U when U > 60 -> medium;
        _ -> low
    end,
    
    #{
        status => Status,
        utilization_percentage => Utilization,
        efficiency_rating => scheduler_efficiency_rating(Utilization)
    }.

analyze_port_utilization(PortMetrics) ->
    ActivePorts = maps:get(active_ports, PortMetrics, 0),
    
    Status = case ActivePorts of
        P when P > 10000 -> critical;
        P when P > 5000 -> high;
        P when P > 1000 -> medium;
        _ -> low
    end,
    
    #{
        status => Status,
        active_ports => ActivePorts,
        total_input => maps:get(total_input, PortMetrics, 0),
        total_output => maps:get(total_output, PortMetrics, 0)
    }.

analyze_ets_utilization(ETSMetrics) ->
    TableCount = maps:get(table_count, ETSMetrics, 0),
    TotalMemory = maps:get(total_memory, ETSMetrics, 0),
    
    Status = case {TableCount, TotalMemory} of
        {T, _} when T > 1000 -> critical;
        {T, M} when T > 500; M > 100000000 -> high;  % 100MB
        {T, M} when T > 100; M > 10000000 -> medium;  % 10MB
        _ -> low
    end,
    
    #{
        status => Status,
        table_count => TableCount,
        total_memory => TotalMemory,
        total_objects => maps:get(total_objects, ETSMetrics, 0)
    }.

%% Additional helper functions
calculate_fragmentation(_MemoryMetrics) -> 5.  % Simplified
calculate_process_efficiency(_ProcessMetrics) -> 85.  % Simplified
scheduler_efficiency_rating(Util) when Util > 90 -> excellent;
scheduler_efficiency_rating(Util) when Util > 70 -> good;
scheduler_efficiency_rating(Util) when Util > 50 -> acceptable;
scheduler_efficiency_rating(_) -> poor.

identify_resource_bottlenecks(UtilizationList) ->
    lists:filter(fun(#{status := Status}) -> 
        lists:member(Status, [critical, high]) 
    end, UtilizationList).

generate_resource_recommendations(UtilizationList) ->
    lists:flatmap(fun(#{status := critical}) ->
        [<<"Critical resource usage detected - immediate action required">>];
    (#{status := high}) ->
        [<<"High resource usage - monitor and consider scaling">>];
    (_) -> []
    end, UtilizationList).

calculate_resource_efficiency_score(Assessment) ->
    Scores = [
        score_from_status(maps:get(status, maps:get(memory, Assessment, #{status => low}))),
        score_from_status(maps:get(status, maps:get(process, Assessment, #{status => low}))),
        score_from_status(maps:get(status, maps:get(scheduler, Assessment, #{status => low}))),
        score_from_status(maps:get(status, maps:get(ports, Assessment, #{status => low}))),
        score_from_status(maps:get(status, maps:get(ets, Assessment, #{status => low})))
    ],
    round(lists:sum(Scores) / length(Scores)).

score_from_status(critical) -> 25;
score_from_status(high) -> 50;
score_from_status(medium) -> 75;
score_from_status(low) -> 100.

efficiency_grade(Score) when Score >= 90 -> excellent;
efficiency_grade(Score) when Score >= 75 -> good;
efficiency_grade(Score) when Score >= 60 -> acceptable;
efficiency_grade(_) -> poor.

%% Flame graph functions
build_span_hierarchy(_Spans) -> [].  % Simplified
convert_to_flame_format(_Hierarchy, _TraceId) -> #{}.  % Simplified
merge_flame_data(Acc, _Data) -> Acc.  % Simplified
count_total_spans(Traces) -> 
    lists:sum([length(maps:get(spans, T, [])) || T <- Traces]).
calculate_max_depth(_FlameData) -> 5.  % Simplified
identify_hot_paths(_FlameData) -> [].  % Simplified
identify_slow_operations(_FlameData) -> [].  % Simplified
generate_interactive_flame_data(FlameData) -> FlameData.
generate_color_scheme(_FlameData) -> #{}.

%% Quality scoring functions
calculate_test_quality_score(TestResults) ->
    Tests = maps:get(tests, TestResults, []),
    case length(Tests) of
        0 -> 0;
        Total ->
            Passed = length(lists:filter(fun(T) -> maps:get(status, T) =:= passed end, Tests)),
            (Passed / Total) * 100
    end.

calculate_security_quality_score(SecurityResults) ->
    Vulnerabilities = maps:get(vulnerabilities, SecurityResults, []),
    CriticalCount = length(lists:filter(fun(V) -> maps:get(severity, V) =:= critical end, Vulnerabilities)),
    HighCount = length(lists:filter(fun(V) -> maps:get(severity, V) =:= high end, Vulnerabilities)),
    
    % Deduct points based on vulnerability severity
    BaseScore = 100,
    Deductions = (CriticalCount * 30) + (HighCount * 15) + (length(Vulnerabilities) * 2),
    max(0, BaseScore - Deductions).

calculate_coverage_quality_score(CoverageData) ->
    maps:get(percentage, CoverageData, 0).

calculate_code_quality_score(_TestResults) ->
    85.  % Simplified - in practice would analyze code metrics

quality_grade(Score) when Score >= 90 -> excellent;
quality_grade(Score) when Score >= 80 -> good;
quality_grade(Score) when Score >= 70 -> acceptable;
quality_grade(_) -> needs_improvement.

generate_quality_recommendations(QualityScores) ->
    lists:flatmap(fun({Category, Score}) ->
        case Score < 70 of
            true -> [iolist_to_binary(io_lib:format("Improve ~p (current score: ~p)", [Category, round(Score)]))];
            false -> []
        end
    end, QualityScores).

%% Bottleneck analysis functions
analyze_memory_bottlenecks(MemoryMetrics) ->
    Total = maps:get(total, MemoryMetrics, 0),
    Processes = maps:get(processes, MemoryMetrics, 0),
    
    UsagePercent = case Total of
        0 -> 0;
        _ -> (Processes / Total) * 100
    end,
    
    case UsagePercent > 80 of
        true -> [#{
            type => memory,
            severity => critical,
            description => <<"High memory usage detected">>,
            metric => UsagePercent,
            recommendation => <<"Review memory allocation patterns">>
        }];
        false -> []
    end.

analyze_process_bottlenecks(ProcessMetrics) ->
    ActiveProcesses = maps:get(active_processes, ProcessMetrics, 0),
    case ActiveProcesses > 20000 of
        true -> [#{
            type => process,
            severity => high,
            description => <<"High process count">>,
            metric => ActiveProcesses,
            recommendation => <<"Review process creation patterns">>
        }];
        false -> []
    end.

analyze_scheduler_bottlenecks(SchedulerMetrics) ->
    Utilization = maps:get(utilization, SchedulerMetrics, 0),
    case Utilization > 90 of
        true -> [#{
            type => scheduler,
            severity => critical,
            description => <<"High scheduler utilization">>,
            metric => Utilization,
            recommendation => <<"Review CPU-intensive operations">>
        }];
        false -> []
    end.

% Simplified implementations for remaining bottleneck analysis functions
analyze_io_bottlenecks(_) -> [].
analyze_network_bottlenecks(_) -> [].
analyze_gc_bottlenecks(_) -> [].
analyze_port_bottlenecks(_) -> [].
analyze_ets_bottlenecks(_) -> [].

priority_value(critical) -> 4;
priority_value(high) -> 3;
priority_value(medium) -> 2;
priority_value(low) -> 1.

count_by_severity(Bottlenecks, Severity) ->
    length(lists:filter(fun(#{severity := S}) -> S =:= Severity; (_) -> false end, Bottlenecks)).

assess_system_health(Bottlenecks) ->
    CriticalCount = count_by_severity(Bottlenecks, critical),
    HighCount = count_by_severity(Bottlenecks, high),
    
    case {CriticalCount, HighCount} of
        {0, 0} -> healthy;
        {0, H} when H =< 2 -> warning;
        _ -> critical
    end.

generate_bottleneck_recommendations(Bottlenecks) ->
    lists:map(fun(#{recommendation := Rec}) -> Rec end, lists:sublist(Bottlenecks, 5)).

atom_to_binary(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom));
atom_to_binary(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).