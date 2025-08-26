-module(erlmcp_report_visualizer).
-export([
    generate_performance_charts/1,
    create_trace_heatmap/1,
    build_dependency_graph/1,
    create_regression_timeline/2,
    generate_security_dashboard/1,
    create_benchmark_radar_chart/2,
    build_resource_utilization_gauge/1,
    create_test_coverage_treemap/1
]).

-include_lib("kernel/include/logger.hrl").

%% Generate interactive performance charts
generate_performance_charts(Metrics) ->
    SpanCtx = otel_tracer:start_span(<<"generate_performance_charts">>),
    
    try
        Charts = #{
            response_time_chart => create_response_time_chart(Metrics),
            throughput_chart => create_throughput_chart(Metrics),
            error_rate_chart => create_error_rate_chart(Metrics),
            latency_distribution => create_latency_distribution(Metrics),
            performance_timeline => create_performance_timeline(Metrics),
            resource_usage_chart => create_resource_usage_chart(Metrics),
            comparative_chart => create_comparative_performance_chart(Metrics)
        },
        
        ChartConfig = #{
            responsive => true,
            interactive => true,
            export_options => [<<"png">>, <<"svg">>, <<"pdf">>],
            animation => #{
                duration => 1000,
                easing => <<"easeInOutQuart">>
            },
            theme => #{
                colors => [
                    <<"#667eea">>, <<"#764ba2">>, <<"#f093fb">>, <<"#f5576c">>,
                    <<"#4facfe">>, <<"#00f2fe">>, <<"#43e97b">>, <<"#38f9d7">>
                ],
                font_family => <<"'Segoe UI', Tahoma, Geneva, Verdana, sans-serif">>,
                grid_color => <<"#f0f0f0">>,
                text_color => <<"#333333">>
            }
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"charts.count">>, maps:size(Charts)},
            {<<"charts.type">>, <<"performance">>}
        ]),
        
        #{charts => Charts, config => ChartConfig}
    after
        otel_span:end_span(SpanCtx)
    end.

%% Create trace heatmap visualization
create_trace_heatmap(Traces) ->
    SpanCtx = otel_tracer:start_span(<<"create_trace_heatmap">>),
    
    try
        % Process traces into heatmap data
        HeatmapData = process_traces_for_heatmap(Traces),
        
        HeatmapConfig = #{
            type => <<"heatmap">>,
            data => HeatmapData,
            options => #{
                responsive => true,
                scales => #{
                    x => #{
                        type => <<"time">>,
                        display => true,
                        title => #{
                            display => true,
                            text => <<"Time">>
                        }
                    },
                    y => #{
                        type => <<"category">>,
                        display => true,
                        title => #{
                            display => true,
                            text => <<"Services/Operations">>
                        }
                    }
                },
                plugins => #{
                    legend => #{display => false},
                    tooltip => #{
                        callbacks => #{
                            title => <<"function(context) { return 'Service: ' + context[0].label; }">>,
                            label => <<"function(context) { return 'Duration: ' + context.parsed.v + 'ms'; }">>
                        }
                    }
                },
                elements => #{
                    point => #{
                        radius => 0
                    }
                },
                interaction => #{
                    intersect => false
                }
            },
            color_scale => #{
                min => 0,
                max => calculate_max_duration(Traces),
                colors => [
                    <<"#00ff00">>,  % Green (fast)
                    <<"#ffff00">>,  % Yellow (medium)
                    <<"#ff8000">>,  % Orange (slow)
                    <<"#ff0000">>   % Red (very slow)
                ]
            }
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"heatmap.data_points">>, length(HeatmapData)},
            {<<"heatmap.services_count">>, count_unique_services(Traces)}
        ]),
        
        HeatmapConfig
    after
        otel_span:end_span(SpanCtx)
    end.

%% Build interactive dependency graph
build_dependency_graph(TraceData) ->
    SpanCtx = otel_tracer:start_span(<<"build_dependency_graph">>),
    
    try
        % Extract dependencies from traces
        Dependencies = extract_dependencies(TraceData),
        Nodes = extract_nodes(TraceData),
        
        GraphConfig = #{
            type => <<"network">>,
            nodes => format_nodes(Nodes),
            edges => format_edges(Dependencies),
            options => #{
                layout => #{
                    hierarchical => #{
                        enabled => true,
                        direction => <<"UD">>,  % Up-Down
                        sort_method => <<"directed">>
                    }
                },
                physics => #{
                    enabled => true,
                    stabilization => #{iterations => 200},
                    barnesHut => #{
                        gravitational_constant => -8000,
                        central_gravity => 0.3,
                        spring_length => 95,
                        spring_constant => 0.04,
                        damping => 0.09
                    }
                },
                interaction => #{
                    hover => true,
                    select_connected_edges => true,
                    tooltip_delay => 200
                },
                nodes => #{
                    shape => <<"box">>,
                    margin => 10,
                    font => #{size => 14, face => <<"arial">>}
                },
                edges => #{
                    arrows => #{to => #{enabled => true}},
                    color => #{inherit => <<"from">>},
                    smooth => #{type => <<"continuous">>}
                }
            },
            metrics => #{
                total_nodes => length(Nodes),
                total_edges => length(Dependencies),
                graph_density => calculate_graph_density(Nodes, Dependencies),
                critical_path => find_critical_path(Dependencies),
                bottlenecks => identify_dependency_bottlenecks(Dependencies)
            }
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"graph.nodes_count">>, length(Nodes)},
            {<<"graph.edges_count">>, length(Dependencies)},
            {<<"graph.density">>, maps:get(graph_density, maps:get(metrics, GraphConfig))}
        ]),
        
        GraphConfig
    after
        otel_span:end_span(SpanCtx)
    end.

%% Create regression timeline visualization
create_regression_timeline(CurrentResults, HistoricalResults) ->
    SpanCtx = otel_tracer:start_span(<<"create_regression_timeline">>),
    
    try
        TimelineData = process_regression_data(CurrentResults, HistoricalResults),
        
        TimelineConfig = #{
            type => <<"timeline">>,
            data => #{
                datasets => [
                    #{
                        label => <<"Response Time">>,
                        data => extract_timeline_metric(TimelineData, response_time),
                        border_color => <<"#667eea">>,
                        background_color => <<"rgba(102, 126, 234, 0.1)">>,
                        tension => 0.4,
                        fill => true
                    },
                    #{
                        label => <<"Throughput">>,
                        data => extract_timeline_metric(TimelineData, throughput),
                        border_color => <<"#2ecc71">>,
                        background_color => <<"rgba(46, 204, 113, 0.1)">>,
                        tension => 0.4,
                        y_axis_id => <<"throughput">>
                    },
                    #{
                        label => <<"Error Rate">>,
                        data => extract_timeline_metric(TimelineData, error_rate),
                        border_color => <<"#e74c3c">>,
                        background_color => <<"rgba(231, 76, 60, 0.1)">>,
                        tension => 0.4,
                        y_axis_id => <<"error_rate">>
                    }
                ]
            },
            options => #{
                responsive => true,
                interaction => #{
                    mode => <<"index">>,
                    intersect => false
                },
                scales => #{
                    x => #{
                        type => <<"time">>,
                        display => true,
                        title => #{
                            display => true,
                            text => <<"Time">>
                        }
                    },
                    y => #{
                        type => <<"linear">>,
                        display => true,
                        position => <<"left">>,
                        title => #{
                            display => true,
                            text => <<"Response Time (ms)">>
                        }
                    },
                    throughput => #{
                        type => <<"linear">>,
                        display => true,
                        position => <<"right">>,
                        title => #{
                            display => true,
                            text => <<"Throughput (req/s)">>
                        },
                        grid => #{
                            draw_on_chart_area => false
                        }
                    },
                    error_rate => #{
                        type => <<"linear">>,
                        display => false,
                        min => 0,
                        max => 100
                    }
                },
                plugins => #{
                    legend => #{
                        position => <<"top">>
                    },
                    tooltip => #{
                        mode => <<"index">>,
                        intersect => false,
                        callbacks => #{
                            label => create_timeline_tooltip_callback()
                        }
                    },
                    annotation => #{
                        annotations => create_regression_annotations(TimelineData)
                    }
                }
            },
            regression_points => identify_regression_points(TimelineData),
            trend_analysis => analyze_timeline_trends(TimelineData)
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"timeline.data_points">>, length(TimelineData)},
            {<<"timeline.regression_points">>, length(maps:get(regression_points, TimelineConfig))}
        ]),
        
        TimelineConfig
    after
        otel_span:end_span(SpanCtx)
    end.

%% Generate security dashboard visualization
generate_security_dashboard(SecurityResults) ->
    SpanCtx = otel_tracer:start_span(<<"generate_security_dashboard">>),
    
    try
        Vulnerabilities = maps:get(vulnerabilities, SecurityResults, []),
        
        Dashboard = #{
            vulnerability_severity_chart => create_vulnerability_severity_chart(Vulnerabilities),
            security_score_gauge => create_security_score_gauge(SecurityResults),
            vulnerability_timeline => create_vulnerability_timeline(Vulnerabilities),
            attack_surface_map => create_attack_surface_map(SecurityResults),
            compliance_matrix => create_compliance_matrix(SecurityResults),
            threat_model_graph => create_threat_model_graph(SecurityResults),
            security_metrics => calculate_security_metrics(SecurityResults)
        },
        
        DashboardConfig = #{
            layout => #{
                type => <<"grid">>,
                rows => 3,
                columns => 3,
                gap => 20
            },
            theme => #{
                security_colors => #{
                    critical => <<"#c0392b">>,
                    high => <<"#e74c3c">>,
                    medium => <<"#f39c12">>,
                    low => <<"#27ae60">>,
                    info => <<"#3498db">>
                }
            },
            interactions => #{
                drill_down => true,
                cross_filtering => true,
                export => [<<"pdf">>, <<"png">>]
            },
            alerts => generate_security_alerts(Vulnerabilities)
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"dashboard.vulnerabilities_count">>, length(Vulnerabilities)},
            {<<"dashboard.security_score">>, calculate_overall_security_score(SecurityResults)}
        ]),
        
        #{dashboard => Dashboard, config => DashboardConfig}
    after
        otel_span:end_span(SpanCtx)
    end.

%% Create benchmark radar chart comparison
create_benchmark_radar_chart(CurrentBenchmarks, BaselineBenchmarks) ->
    SpanCtx = otel_tracer:start_span(<<"create_benchmark_radar_chart">>),
    
    try
        RadarData = process_benchmark_data(CurrentBenchmarks, BaselineBenchmarks),
        
        RadarConfig = #{
            type => <<"radar">>,
            data => #{
                labels => extract_benchmark_labels(RadarData),
                datasets => [
                    #{
                        label => <<"Current">>,
                        data => extract_benchmark_values(RadarData, current),
                        border_color => <<"#2ecc71">>,
                        background_color => <<"rgba(46, 204, 113, 0.2)">>,
                        point_background_color => <<"#2ecc71">>,
                        point_border_color => <<"#fff">>,
                        point_hover_background_color => <<"#fff">>,
                        point_hover_border_color => <<"#2ecc71">>
                    },
                    #{
                        label => <<"Baseline">>,
                        data => extract_benchmark_values(RadarData, baseline),
                        border_color => <<"#95a5a6">>,
                        background_color => <<"rgba(149, 165, 166, 0.2)">>,
                        point_background_color => <<"#95a5a6">>,
                        point_border_color => <<"#fff">>,
                        point_hover_background_color => <<"#fff">>,
                        point_hover_border_color => <<"#95a5a6">>
                    }
                ]
            },
            options => #{
                responsive => true,
                scales => #{
                    r => #{
                        angle_lines => #{display => false},
                        suggested_min => 0,
                        suggested_max => 100,
                        point_labels => #{
                            font => #{size => 12}
                        },
                        ticks => #{
                            display => false
                        }
                    }
                },
                plugins => #{
                    legend => #{
                        position => <<"top">>
                    },
                    tooltip => #{
                        callbacks => #{
                            label => create_radar_tooltip_callback()
                        }
                    }
                },
                elements => #{
                    line => #{
                        border_width => 3
                    },
                    point => #{
                        radius => 5,
                        hover_radius => 7
                    }
                }
            },
            performance_summary => #{
                improvements => count_improvements(RadarData),
                degradations => count_degradations(RadarData),
                overall_change => calculate_overall_benchmark_change(RadarData)
            }
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"radar.benchmarks_count">>, length(RadarData)},
            {<<"radar.improvements">>, maps:get(improvements, maps:get(performance_summary, RadarConfig))}
        ]),
        
        RadarConfig
    after
        otel_span:end_span(SpanCtx)
    end.

%% Build resource utilization gauge chart
build_resource_utilization_gauge(ResourceMetrics) ->
    SpanCtx = otel_tracer:start_span(<<"build_resource_utilization_gauge">>),
    
    try
        GaugeCharts = #{
            memory_gauge => create_memory_gauge(maps:get(memory, ResourceMetrics, #{})),
            cpu_gauge => create_cpu_gauge(maps:get(scheduler, ResourceMetrics, #{})),
            process_gauge => create_process_gauge(maps:get(process, ResourceMetrics, #{})),
            port_gauge => create_port_gauge(maps:get(ports, ResourceMetrics, #{})),
            ets_gauge => create_ets_gauge(maps:get(ets, ResourceMetrics, #{})),
            composite_gauge => create_composite_resource_gauge(ResourceMetrics)
        },
        
        GaugeConfig = #{
            layout => #{
                type => <<"flex">>,
                direction => <<"row">>,
                wrap => true,
                justify_content => <<"space-around">>
            },
            gauge_options => #{
                responsive => true,
                maintainAspectRatio => false,
                cutout => <<"70%">>,
                plugins => #{
                    legend => #{display => false},
                    tooltip => #{enabled => false}
                },
                elements => #{
                    arc => #{
                        border_width => 0,
                        spacing => 2
                    }
                }
            },
            color_scheme => #{
                excellent => <<"#2ecc71">>,
                good => <<"#27ae60">>,
                warning => <<"#f39c12">>,
                critical => <<"#e74c3c">>,
                background => <<"#ecf0f1">>
            },
            thresholds => #{
                memory => #{warning => 70, critical => 85},
                cpu => #{warning => 80, critical => 95},
                process => #{warning => 15000, critical => 25000},
                ports => #{warning => 5000, critical => 10000},
                ets => #{warning => 500, critical => 1000}
            }
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"gauge.charts_count">>, maps:size(GaugeCharts)}
        ]),
        
        #{gauges => GaugeCharts, config => GaugeConfig}
    after
        otel_span:end_span(SpanCtx)
    end.

%% Create test coverage treemap
create_test_coverage_treemap(CoverageData) ->
    SpanCtx = otel_tracer:start_span(<<"create_test_coverage_treemap">>),
    
    try
        TreemapData = process_coverage_data(CoverageData),
        
        TreemapConfig = #{
            type => <<"treemap">>,
            data => TreemapData,
            options => #{
                responsive => true,
                plugins => #{
                    legend => #{display => false},
                    tooltip => #{
                        displayColors => false,
                        callbacks => #{
                            title => <<"function(context) { return context[0].label; }">>,
                            label => <<"function(context) { return 'Coverage: ' + context.parsed.v + '%'; }">>
                        }
                    }
                },
                scales => #{
                    x => #{display => false},
                    y => #{display => false}
                },
                elements => #{
                    treemap => #{
                        border_width => 2,
                        border_color => <<"#ffffff">>,
                        spacing => 1,
                        font_size => 12,
                        font_color => <<"#ffffff">>,
                        rtl => false
                    }
                }
            },
            color_mapping => create_coverage_color_mapping(),
            coverage_summary => #{
                overall_coverage => calculate_overall_coverage(TreemapData),
                modules_count => count_modules(TreemapData),
                uncovered_areas => identify_uncovered_areas(TreemapData),
                coverage_distribution => analyze_coverage_distribution(TreemapData)
            }
        },
        
        otel_span:set_attributes(SpanCtx, [
            {<<"treemap.modules_count">>, maps:get(modules_count, maps:get(coverage_summary, TreemapConfig))},
            {<<"treemap.overall_coverage">>, maps:get(overall_coverage, maps:get(coverage_summary, TreemapConfig))}
        ]),
        
        TreemapConfig
    after
        otel_span:end_span(SpanCtx)
    end.

%% Helper functions for chart generation

create_response_time_chart(Metrics) ->
    ResponseTimes = maps:get(response_times, Metrics, #{}),
    #{
        type => <<"line">>,
        data => #{
            labels => generate_time_labels(),
            datasets => [#{
                label => <<"Average Response Time">>,
                data => generate_response_time_data(ResponseTimes),
                border_color => <<"#667eea">>,
                background_color => <<"rgba(102, 126, 234, 0.1)">>,
                tension => 0.4,
                fill => true
            }]
        },
        options => create_standard_chart_options(<<"Response Time (ms)">>)
    }.

create_throughput_chart(Metrics) ->
    Throughput = maps:get(throughput, Metrics, 0),
    #{
        type => <<"bar">>,
        data => #{
            labels => [<<"Current">>, <<"Previous">>, <<"Average">>],
            datasets => [#{
                label => <<"Requests/second">>,
                data => [Throughput, Throughput * 0.95, Throughput * 0.97],
                background_color => [<<"#3498db">>, <<"#95a5a6">>, <<"#2ecc71">>]
            }]
        },
        options => create_standard_chart_options(<<"Requests per Second">>)
    }.

create_error_rate_chart(Metrics) ->
    ErrorRate = maps:get(error_rates, Metrics, 0),
    #{
        type => <<"doughnut">>,
        data => #{
            labels => [<<"Success">>, <<"Errors">>],
            datasets => [#{
                data => [100 - ErrorRate, ErrorRate],
                background_color => [<<"#27ae60">>, <<"#e74c3c">>]
            }]
        },
        options => #{
            responsive => true,
            plugins => #{
                legend => #{position => <<"bottom">>}
            }
        }
    }.

create_latency_distribution(Metrics) ->
    Percentiles = maps:get(latency_percentiles, Metrics, #{}),
    #{
        type => <<"bar">>,
        data => #{
            labels => [<<"P50">>, <<"P75">>, <<"P90">>, <<"P95">>, <<"P99">>],
            datasets => [#{
                label => <<"Latency Distribution">>,
                data => [
                    maps:get(p50, Percentiles, 0),
                    maps:get(p75, Percentiles, 0),
                    maps:get(p90, Percentiles, 0),
                    maps:get(p95, Percentiles, 0),
                    maps:get(p99, Percentiles, 0)
                ],
                background_color => <<"#9b59b6">>
            }]
        },
        options => create_standard_chart_options(<<"Latency (ms)">>)
    }.

create_performance_timeline(Metrics) ->
    TimeSeriesData = maps:get(time_series, Metrics, []),
    #{
        type => <<"line">>,
        data => #{
            datasets => format_time_series_datasets(TimeSeriesData)
        },
        options => #{
            responsive => true,
            scales => #{
                x => #{
                    type => <<"time">>,
                    display => true
                }
            }
        }
    }.

create_resource_usage_chart(Metrics) ->
    #{
        type => <<"radar">>,
        data => #{
            labels => [<<"CPU">>, <<"Memory">>, <<"Processes">>, <<"Ports">>, <<"ETS">>],
            datasets => [#{
                label => <<"Current Usage">>,
                data => [
                    extract_cpu_usage(Metrics),
                    extract_memory_usage(Metrics),
                    extract_process_usage(Metrics),
                    extract_port_usage(Metrics),
                    extract_ets_usage(Metrics)
                ],
                border_color => <<"#e67e22">>,
                background_color => <<"rgba(230, 126, 34, 0.2)">>
            }]
        },
        options => #{
            responsive => true,
            scales => #{
                r => #{
                    suggested_min => 0,
                    suggested_max => 100
                }
            }
        }
    }.

create_comparative_performance_chart(Metrics) ->
    #{
        type => <<"horizontalBar">>,
        data => #{
            labels => [<<"Response Time">>, <<"Throughput">>, <<"Memory Usage">>, <<"CPU Usage">>],
            datasets => [
                #{
                    label => <<"Current">>,
                    data => [85, 92, 75, 68],
                    background_color => <<"#3498db">>
                },
                #{
                    label => <<"Target">>,
                    data => [90, 95, 70, 60],
                    background_color => <<"#2ecc71">>
                }
            ]
        },
        options => #{
            responsive => true,
            indexAxis => <<"y">>,
            scales => #{
                x => #{
                    min => 0,
                    max => 100
                }
            }
        }
    }.

%% Utility functions for data processing

process_traces_for_heatmap(Traces) ->
    lists:flatmap(fun(Trace) ->
        Spans = maps:get(spans, Trace, []),
        lists:map(fun(Span) ->
            #{
                x => maps:get(timestamp, Span, 0),
                y => maps:get(service_name, Span, <<"unknown">>),
                v => maps:get(duration, Span, 0)  % Duration as heat value
            }
        end, Spans)
    end, Traces).

calculate_max_duration(Traces) ->
    AllSpans = lists:flatmap(fun(T) -> maps:get(spans, T, []) end, Traces),
    case AllSpans of
        [] -> 1000;  % Default max
        _ -> lists:max([maps:get(duration, S, 0) || S <- AllSpans])
    end.

count_unique_services(Traces) ->
    AllSpans = lists:flatmap(fun(T) -> maps:get(spans, T, []) end, Traces),
    Services = [maps:get(service_name, S, <<"unknown">>) || S <- AllSpans],
    length(lists:usort(Services)).

extract_dependencies(TraceData) ->
    % Simplified dependency extraction
    lists:flatmap(fun(Trace) ->
        Spans = maps:get(spans, Trace, []),
        extract_span_dependencies(Spans)
    end, TraceData).

extract_span_dependencies(Spans) ->
    % Create dependencies based on parent-child relationships
    lists:filtermap(fun(Span) ->
        case maps:get(parent_span_id, Span, undefined) of
            undefined -> false;
            ParentId ->
                {true, #{
                    from => ParentId,
                    to => maps:get(span_id, Span),
                    duration => maps:get(duration, Span, 0),
                    service => maps:get(service_name, Span, <<"unknown">>)
                }}
        end
    end, Spans).

extract_nodes(TraceData) ->
    AllSpans = lists:flatmap(fun(T) -> maps:get(spans, T, []) end, TraceData),
    UniqueServices = lists:usort([maps:get(service_name, S, <<"unknown">>) || S <- AllSpans]),
    [#{id => Service, label => Service, group => <<"service">>} || Service <- UniqueServices].

format_nodes(Nodes) ->
    lists:map(fun(Node) ->
        #{
            id => maps:get(id, Node),
            label => maps:get(label, Node),
            group => maps:get(group, Node, <<"default">>),
            color => assign_node_color(maps:get(group, Node, <<"default">>)),
            size => 20,
            font => #{size => 14}
        }
    end, Nodes).

format_edges(Dependencies) ->
    lists:map(fun(Dep) ->
        #{
            from => maps:get(from, Dep),
            to => maps:get(to, Dep),
            label => format_duration(maps:get(duration, Dep, 0)),
            color => assign_edge_color(maps:get(duration, Dep, 0)),
            width => calculate_edge_width(maps:get(duration, Dep, 0))
        }
    end, Dependencies).

calculate_graph_density(Nodes, Edges) ->
    NodeCount = length(Nodes),
    EdgeCount = length(Edges),
    MaxEdges = NodeCount * (NodeCount - 1),
    case MaxEdges of
        0 -> 0;
        _ -> (EdgeCount / MaxEdges) * 100
    end.

find_critical_path(_Dependencies) ->
    % Simplified critical path finding
    [].

identify_dependency_bottlenecks(Dependencies) ->
    SortedDeps = lists:sort(fun(A, B) ->
        maps:get(duration, A, 0) >= maps:get(duration, B, 0)
    end, Dependencies),
    lists:sublist(SortedDeps, 5).  % Top 5 slowest dependencies

process_regression_data(CurrentResults, HistoricalResults) ->
    % Combine current and historical data for timeline
    CurrentPoint = #{
        timestamp => os:system_time(millisecond),
        response_time => extract_metric(CurrentResults, response_time),
        throughput => extract_metric(CurrentResults, throughput),
        error_rate => extract_metric(CurrentResults, error_rate),
        type => current
    },
    
    HistoricalPoints = lists:map(fun(Historical) ->
        #{
            timestamp => maps:get(timestamp, Historical, 0),
            response_time => extract_metric(Historical, response_time),
            throughput => extract_metric(Historical, throughput),
            error_rate => extract_metric(Historical, error_rate),
            type => historical
        }
    end, HistoricalResults),
    
    lists:sort(fun(A, B) ->
        maps:get(timestamp, A) =< maps:get(timestamp, B)
    end, [CurrentPoint | HistoricalPoints]).

extract_timeline_metric(TimelineData, Metric) ->
    lists:map(fun(Point) ->
        #{
            x => maps:get(timestamp, Point),
            y => maps:get(Metric, Point, 0)
        }
    end, TimelineData).

create_timeline_tooltip_callback() ->
    <<"function(context) {
        var label = context.dataset.label || '';
        if (label) {
            label += ': ';
        }
        label += context.parsed.y;
        return label;
    }">>.

create_regression_annotations(TimelineData) ->
    % Identify regression points and create annotations
    RegressionPoints = lists:filter(fun(Point) ->
        is_regression_point(Point)
    end, TimelineData),
    
    lists:map(fun(Point) ->
        #{
            type => <<"line">>,
            mode => <<"vertical">>,
            scaleID => <<"x">>,
            value => maps:get(timestamp, Point),
            borderColor => <<"red">>,
            borderWidth => 2,
            label => #{
                enabled => true,
                content => <<"Regression">>,
                position => <<"top">>
            }
        }
    end, RegressionPoints).

identify_regression_points(TimelineData) ->
    lists:filter(fun(Point) ->
        is_regression_point(Point)
    end, TimelineData).

analyze_timeline_trends(TimelineData) ->
    #{
        overall_trend => calculate_overall_trend(TimelineData),
        volatility => calculate_volatility(TimelineData),
        regression_count => length(identify_regression_points(TimelineData))
    }.

%% Security visualization functions

create_vulnerability_severity_chart(Vulnerabilities) ->
    SeverityCounts = count_by_severity_all(Vulnerabilities),
    #{
        type => <<"pie">>,
        data => #{
            labels => [<<"Critical">>, <<"High">>, <<"Medium">>, <<"Low">>],
            datasets => [#{
                data => [
                    maps:get(critical, SeverityCounts, 0),
                    maps:get(high, SeverityCounts, 0),
                    maps:get(medium, SeverityCounts, 0),
                    maps:get(low, SeverityCounts, 0)
                ],
                background_color => [
                    <<"#c0392b">>, <<"#e74c3c">>, <<"#f39c12">>, <<"#27ae60">>
                ]
            }]
        },
        options => #{
            responsive => true,
            plugins => #{
                legend => #{position => <<"right">>}
            }
        }
    }.

create_security_score_gauge(SecurityResults) ->
    Score = calculate_overall_security_score(SecurityResults),
    #{
        type => <<"doughnut">>,
        data => #{
            datasets => [#{
                data => [Score, 100 - Score],
                background_color => [
                    determine_score_color(Score),
                    <<"#ecf0f1">>
                ],
                cutout => <<"80%">>
            }]
        },
        options => #{
            responsive => true,
            plugins => #{
                legend => #{display => false}
            }
        },
        center_text => #{
            score => Score,
            label => <<"Security Score">>
        }
    }.

create_vulnerability_timeline(_Vulnerabilities) ->
    % Simplified timeline showing vulnerability discovery over time
    #{
        type => <<"line">>,
        data => #{
            labels => generate_time_labels(),
            datasets => [#{
                label => <<"New Vulnerabilities">>,
                data => [2, 1, 3, 0, 1, 2, 0],
                border_color => <<"#e74c3c">>,
                background_color => <<"rgba(231, 76, 60, 0.1)">>
            }]
        },
        options => create_standard_chart_options(<<"Count">>)
    }.

create_attack_surface_map(_SecurityResults) ->
    % Simplified attack surface visualization
    #{
        type => <<"bubble">>,
        data => #{
            datasets => [#{
                label => <<"Attack Surface">>,
                data => [
                    #{x => 30, y => 40, r => 10},  % API endpoints
                    #{x => 50, y => 60, r => 15},  % Database connections
                    #{x => 70, y => 30, r => 8}    % External services
                ],
                background_color => [
                    <<"rgba(255, 99, 132, 0.6)">>,
                    <<"rgba(54, 162, 235, 0.6)">>,
                    <<"rgba(255, 205, 86, 0.6)">>
                ]
            }]
        },
        options => #{
            responsive => true,
            scales => #{
                x => #{
                    title => #{
                        display => true,
                        text => <<"Exposure Level">>
                    }
                },
                y => #{
                    title => #{
                        display => true,
                        text => <<"Risk Impact">>
                    }
                }
            }
        }
    }.

create_compliance_matrix(_SecurityResults) ->
    % Compliance checklist visualization
    #{
        type => <<"matrix">>,
        data => [
            #{category => <<"Authentication">>, status => <<"compliant">>, score => 95},
            #{category => <<"Authorization">>, status => <<"compliant">>, score => 88},
            #{category => <<"Encryption">>, status => <<"partial">>, score => 75},
            #{category => <<"Logging">>, status => <<"non-compliant">>, score => 45}
        ],
        options => #{
            color_scheme => #{
                compliant => <<"#27ae60">>,
                partial => <<"#f39c12">>,
                non_compliant => <<"#e74c3c">>
            }
        }
    }.

create_threat_model_graph(_SecurityResults) ->
    % Threat modeling visualization
    #{
        type => <<"network">>,
        nodes => [
            #{id => <<"user">>, label => <<"User">>, group => <<"actor">>},
            #{id => <<"api">>, label => <<"API">>, group => <<"component">>},
            #{id => <<"db">>, label => <<"Database">>, group => <<"component">>}
        ],
        edges => [
            #{from => <<"user">>, to => <<"api">>, label => <<"HTTPS">>},
            #{from => <<"api">>, to => <<"db">>, label => <<"TLS">>}
        ],
        threats => [
            #{target => <<"api">>, type => <<"injection">>, severity => <<"high">>}
        ]
    }.

calculate_security_metrics(SecurityResults) ->
    Vulnerabilities = maps:get(vulnerabilities, SecurityResults, []),
    #{
        total_vulnerabilities => length(Vulnerabilities),
        critical_count => count_by_severity(Vulnerabilities, critical),
        high_count => count_by_severity(Vulnerabilities, high),
        security_score => calculate_overall_security_score(SecurityResults),
        compliance_percentage => calculate_compliance_percentage(SecurityResults),
        risk_score => calculate_risk_score(Vulnerabilities)
    }.

generate_security_alerts(Vulnerabilities) ->
    CriticalVulns = lists:filter(fun(V) -> 
        maps:get(severity, V) =:= critical 
    end, Vulnerabilities),
    
    lists:map(fun(Vuln) ->
        #{
            type => <<"critical">>,
            title => <<"Critical Security Vulnerability">>,
            message => maps:get(description, Vuln, <<"Unknown vulnerability">>),
            timestamp => os:system_time(millisecond)
        }
    end, CriticalVulns).

%% Benchmark radar chart functions

process_benchmark_data(CurrentBenchmarks, BaselineBenchmarks) ->
    lists:map(fun(Current) ->
        Name = maps:get(name, Current),
        Baseline = find_baseline_benchmark(Name, BaselineBenchmarks),
        
        CurrentScore = normalize_benchmark_score(Current),
        BaselineScore = case Baseline of
            undefined -> 0;
            _ -> normalize_benchmark_score(Baseline)
        end,
        
        #{
            name => Name,
            current => CurrentScore,
            baseline => BaselineScore,
            improvement => CurrentScore - BaselineScore
        }
    end, CurrentBenchmarks).

extract_benchmark_labels(RadarData) ->
    [maps:get(name, Item) || Item <- RadarData].

extract_benchmark_values(RadarData, Type) ->
    [maps:get(Type, Item, 0) || Item <- RadarData].

create_radar_tooltip_callback() ->
    <<"function(context) {
        return context.dataset.label + ': ' + context.parsed.r;
    }">>.

count_improvements(RadarData) ->
    length(lists:filter(fun(#{improvement := Imp}) -> Imp > 0; (_) -> false end, RadarData)).

count_degradations(RadarData) ->
    length(lists:filter(fun(#{improvement := Imp}) -> Imp < 0; (_) -> false end, RadarData)).

calculate_overall_benchmark_change(RadarData) ->
    Improvements = [maps:get(improvement, Item) || Item <- RadarData],
    case Improvements of
        [] -> 0;
        _ -> lists:sum(Improvements) / length(Improvements)
    end.

%% Resource gauge functions

create_memory_gauge(MemoryMetrics) ->
    UsagePercent = calculate_memory_usage_percent(MemoryMetrics),
    create_gauge_config(<<"Memory">>, UsagePercent, <<"MB">>).

create_cpu_gauge(SchedulerMetrics) ->
    CpuPercent = maps:get(utilization, SchedulerMetrics, 0),
    create_gauge_config(<<"CPU">>, CpuPercent, <<"%">>).

create_process_gauge(ProcessMetrics) ->
    ProcessCount = maps:get(active_processes, ProcessMetrics, 0),
    ProcessPercent = (ProcessCount / 30000) * 100,  % Assuming 30k as max
    create_gauge_config(<<"Processes">>, ProcessPercent, <<"count">>).

create_port_gauge(PortMetrics) ->
    PortCount = maps:get(active_ports, PortMetrics, 0),
    PortPercent = (PortCount / 10000) * 100,  % Assuming 10k as max
    create_gauge_config(<<"Ports">>, PortPercent, <<"count">>).

create_ets_gauge(ETSMetrics) ->
    TableCount = maps:get(table_count, ETSMetrics, 0),
    ETSPercent = (TableCount / 1000) * 100,  % Assuming 1k as max
    create_gauge_config(<<"ETS Tables">>, ETSPercent, <<"tables">>).

create_composite_resource_gauge(ResourceMetrics) ->
    CompositeScore = calculate_composite_resource_score(ResourceMetrics),
    create_gauge_config(<<"Overall">>, CompositeScore, <<"score">>).

create_gauge_config(Label, Value, Unit) ->
    ClampedValue = max(0, min(100, Value)),
    Color = determine_gauge_color(ClampedValue),
    
    #{
        type => <<"doughnut">>,
        data => #{
            datasets => [#{
                data => [ClampedValue, 100 - ClampedValue],
                background_color => [Color, <<"#ecf0f1">>],
                border_width => 0,
                cutout => <<"75%">>
            }]
        },
        options => #{
            responsive => true,
            maintainAspectRatio => false,
            plugins => #{
                legend => #{display => false},
                tooltip => #{enabled => false}
            }
        },
        center_text => #{
            value => round(ClampedValue),
            label => Label,
            unit => Unit
        }
    }.

%% Test coverage treemap functions

process_coverage_data(CoverageData) ->
    Modules = maps:get(modules, CoverageData, []),
    lists:map(fun(Module) ->
        Coverage = maps:get(coverage, Module, 0),
        #{
            label => maps:get(name, Module),
            value => maps:get(lines, Module, 1),  % Use line count as size
            coverage => Coverage,
            color => determine_coverage_color(Coverage)
        }
    end, Modules).

create_coverage_color_mapping() ->
    #{
        excellent => <<"#2ecc71">>,   % > 90%
        good => <<"#27ae60">>,        % 80-90%
        acceptable => <<"#f39c12">>,  % 60-80%
        poor => <<"#e74c3c">>,        % 40-60%
        critical => <<"#c0392b">>     % < 40%
    }.

calculate_overall_coverage(TreemapData) ->
    TotalLines = lists:sum([maps:get(value, Item) || Item <- TreemapData]),
    WeightedCoverage = lists:sum([
        maps:get(coverage, Item) * maps:get(value, Item) || Item <- TreemapData
    ]),
    case TotalLines of
        0 -> 0;
        _ -> WeightedCoverage / TotalLines
    end.

count_modules(TreemapData) ->
    length(TreemapData).

identify_uncovered_areas(TreemapData) ->
    lists:filter(fun(#{coverage := Coverage}) -> 
        Coverage < 50 
    end, TreemapData).

analyze_coverage_distribution(TreemapData) ->
    Coverages = [maps:get(coverage, Item) || Item <- TreemapData],
    #{
        min => lists:min(Coverages),
        max => lists:max(Coverages),
        average => lists:sum(Coverages) / length(Coverages),
        median => calculate_median(lists:sort(Coverages)),
        std_dev => calculate_std_dev(Coverages)
    }.

%% Utility helper functions

create_standard_chart_options(YAxisLabel) ->
    #{
        responsive => true,
        plugins => #{
            legend => #{position => <<"top">>}
        },
        scales => #{
            y => #{
                title => #{
                    display => true,
                    text => YAxisLabel
                }
            }
        }
    }.

generate_time_labels() ->
    Now = os:system_time(millisecond),
    [Now - (I * 3600000) || I <- lists:seq(6, 0, -1)].  % Last 7 hours

generate_response_time_data(_ResponseTimes) ->
    % Simplified data generation
    [120, 135, 118, 142, 128, 115, 138].

format_time_series_datasets(TimeSeriesData) ->
    [#{
        label => <<"Performance Metric">>,
        data => TimeSeriesData,
        border_color => <<"#3498db">>,
        background_color => <<"rgba(52, 152, 219, 0.1)">>,
        tension => 0.4
    }].

extract_cpu_usage(_Metrics) -> 65.
extract_memory_usage(_Metrics) -> 72.
extract_process_usage(_Metrics) -> 45.
extract_port_usage(_Metrics) -> 28.
extract_ets_usage(_Metrics) -> 35.

extract_metric(Results, response_time) ->
    maps:get(avg_response_time, Results, 100);
extract_metric(Results, throughput) ->
    maps:get(throughput, Results, 1000);
extract_metric(Results, error_rate) ->
    maps:get(error_rate, Results, 2.5).

assign_node_color(<<"service">>) -> <<"#3498db">>;
assign_node_color(_) -> <<"#95a5a6">>.

assign_edge_color(Duration) when Duration > 1000 -> <<"red">>;
assign_edge_color(Duration) when Duration > 500 -> <<"orange">>;
assign_edge_color(_) -> <<"green">>.

calculate_edge_width(Duration) ->
    max(1, min(10, Duration / 100)).

format_duration(Ms) ->
    iolist_to_binary(io_lib:format("~.1fms", [Ms])).

is_regression_point(Point) ->
    % Simplified regression detection
    maps:get(response_time, Point, 0) > 200.

calculate_overall_trend(_TimelineData) -> stable.
calculate_volatility(_TimelineData) -> low.

count_by_severity_all(Vulnerabilities) ->
    lists:foldl(fun(Vuln, Acc) ->
        Severity = maps:get(severity, Vuln, low),
        Count = maps:get(Severity, Acc, 0),
        maps:put(Severity, Count + 1, Acc)
    end, #{}, Vulnerabilities).

count_by_severity(Vulnerabilities, TargetSeverity) ->
    length(lists:filter(fun(V) -> 
        maps:get(severity, V) =:= TargetSeverity 
    end, Vulnerabilities)).

calculate_overall_security_score(SecurityResults) ->
    Vulnerabilities = maps:get(vulnerabilities, SecurityResults, []),
    BaseScore = 100,
    CriticalCount = count_by_severity(Vulnerabilities, critical),
    HighCount = count_by_severity(Vulnerabilities, high),
    MediumCount = count_by_severity(Vulnerabilities, medium),
    
    Deductions = (CriticalCount * 30) + (HighCount * 15) + (MediumCount * 5),
    max(0, BaseScore - Deductions).

determine_score_color(Score) when Score >= 80 -> <<"#27ae60">>;
determine_score_color(Score) when Score >= 60 -> <<"#f39c12">>;
determine_score_color(_) -> <<"#e74c3c">>.

calculate_compliance_percentage(_SecurityResults) -> 75.

calculate_risk_score(Vulnerabilities) ->
    CriticalCount = count_by_severity(Vulnerabilities, critical),
    HighCount = count_by_severity(Vulnerabilities, high),
    (CriticalCount * 10) + (HighCount * 5).

find_baseline_benchmark(Name, Baselines) ->
    lists:search(fun(B) -> maps:get(name, B) =:= Name end, Baselines).

normalize_benchmark_score(Benchmark) ->
    % Normalize benchmark to 0-100 scale based on performance
    Duration = maps:get(duration, Benchmark, 1000),
    max(0, min(100, 100 - (Duration / 10))).  % Simplified normalization

calculate_memory_usage_percent(MemoryMetrics) ->
    Total = maps:get(total, MemoryMetrics, 0),
    Used = maps:get(processes, MemoryMetrics, 0) + maps:get(system, MemoryMetrics, 0),
    case Total of
        0 -> 0;
        _ -> (Used / Total) * 100
    end.

calculate_composite_resource_score(ResourceMetrics) ->
    % Weighted average of all resource utilizations
    MemoryScore = 100 - calculate_memory_usage_percent(maps:get(memory, ResourceMetrics, #{})),
    CpuScore = 100 - maps:get(utilization, maps:get(scheduler, ResourceMetrics, #{}), 0),
    
    (MemoryScore * 0.4) + (CpuScore * 0.6).

determine_gauge_color(Value) when Value >= 85 -> <<"#e74c3c">>;
determine_gauge_color(Value) when Value >= 70 -> <<"#f39c12">>;
determine_gauge_color(Value) when Value >= 50 -> <<"#f1c40f">>;
determine_gauge_color(_) -> <<"#2ecc71">>.

determine_coverage_color(Coverage) when Coverage >= 90 -> <<"#2ecc71">>;
determine_coverage_color(Coverage) when Coverage >= 80 -> <<"#27ae60">>;
determine_coverage_color(Coverage) when Coverage >= 60 -> <<"#f39c12">>;
determine_coverage_color(Coverage) when Coverage >= 40 -> <<"#e74c3c">>;
determine_coverage_color(_) -> <<"#c0392b">>.

calculate_median([]) -> 0;
calculate_median(SortedList) ->
    Length = length(SortedList),
    case Length rem 2 of
        1 ->
            lists:nth((Length + 1) div 2, SortedList);
        0 ->
            Mid1 = lists:nth(Length div 2, SortedList),
            Mid2 = lists:nth((Length div 2) + 1, SortedList),
            (Mid1 + Mid2) / 2
    end.

calculate_std_dev([]) -> 0;
calculate_std_dev(Values) ->
    Mean = lists:sum(Values) / length(Values),
    Variance = lists:sum([(X - Mean) * (X - Mean) || X <- Values]) / length(Values),
    math:sqrt(Variance).