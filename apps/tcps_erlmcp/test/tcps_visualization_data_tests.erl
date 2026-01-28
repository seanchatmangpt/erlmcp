%%%-----------------------------------------------------------------------------
%%% @doc Tests for TCPS Visualization Data Generation
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_visualization_data_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Descriptions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Kanban Visualization Tests
%%------------------------------------------------------------------------------

kanban_heatmap_chartjs_test() ->
    Result = tcps_visualization_data:kanban_heatmap(#{
        window => '1hour',
        buckets => [security, reliability, cost, compliance],
        format => chartjs
    }),

    ?assert(is_map(Result)),
    ?assert(maps:is_key(labels, Result)),
    ?assert(maps:is_key(datasets, Result)),

    Datasets = maps:get(datasets, Result),
    ?assert(is_list(Datasets)),
    ?assert(length(Datasets) > 0).

kanban_heatmap_d3_test() ->
    Result = tcps_visualization_data:kanban_heatmap(#{
        window => '1hour',
        buckets => [security, reliability],
        format => d3
    }),

    ?assert(is_list(Result)).

kanban_heatmap_recharts_test() ->
    Result = tcps_visualization_data:kanban_heatmap(#{
        window => '1hour',
        buckets => [security, reliability],
        format => recharts
    }),

    ?assert(is_list(Result)),

    case Result of
        [First | _] ->
            ?assert(is_map(First)),
            ?assert(maps:is_key(timestamp, First));
        [] ->
            ok
    end.

kanban_time_series_test() ->
    Result = tcps_visualization_data:kanban_time_series(#{
        window => '5min',
        buckets => [security, reliability],
        format => chartjs
    }),

    ?assert(is_map(Result)),
    ?assert(maps:is_key(labels, Result)),
    ?assert(maps:is_key(datasets, Result)).

kanban_utilization_test() ->
    Result = tcps_visualization_data:kanban_utilization(#{
        buckets => [security, reliability, cost, compliance],
        format => chartjs
    }),

    ?assert(is_map(Result)),
    ?assert(maps:is_key(labels, Result)).

kanban_utilization_d3_test() ->
    Result = tcps_visualization_data:kanban_utilization(#{
        buckets => [security, reliability],
        format => d3
    }),

    ?assert(is_list(Result)),

    case Result of
        [First | _] ->
            ?assert(is_map(First)),
            ?assert(maps:is_key(bucket, First)),
            ?assert(maps:is_key(utilization, First));
        [] ->
            ok
    end.

%%------------------------------------------------------------------------------
%% Quality Gate Visualization Tests
%%------------------------------------------------------------------------------

quality_gate_funnel_test() ->
    Result = tcps_visualization_data:quality_gate_funnel(#{
        gates => [code_coverage, test_pass_rate, security_scan],
        format => chartjs
    }),

    ?assert(is_map(Result)).

quality_gate_funnel_d3_test() ->
    Result = tcps_visualization_data:quality_gate_funnel(#{
        gates => [code_coverage, test_pass_rate],
        format => d3
    }),

    ?assert(is_list(Result)).

quality_gate_scores_test() ->
    Result = tcps_visualization_data:quality_gate_scores(#{
        gates => [code_coverage, test_pass_rate],
        format => chartjs
    }),

    ?assert(is_map(Result)).

quality_gate_trends_test() ->
    Result = tcps_visualization_data:quality_gate_trends(#{
        window => '1day',
        gates => [code_coverage],
        format => chartjs
    }),

    ?assert(is_map(Result)).

%%------------------------------------------------------------------------------
%% Andon Event Visualization Tests
%%------------------------------------------------------------------------------

andon_timeline_test() ->
    Result = tcps_visualization_data:andon_timeline(#{
        window => '1day',
        severity_filter => [high, critical],
        format => d3
    }),

    ?assert(is_list(Result)).

andon_severity_distribution_test() ->
    Result = tcps_visualization_data:andon_severity_distribution(#{
        window => '1day',
        format => chartjs
    }),

    ?assert(is_map(Result)),
    ?assert(maps:is_key(labels, Result)),
    ?assert(maps:is_key(datasets, Result)).

andon_severity_distribution_d3_test() ->
    Result = tcps_visualization_data:andon_severity_distribution(#{
        window => '1day',
        format => d3
    }),

    ?assert(is_list(Result)),

    case Result of
        [First | _] ->
            ?assert(is_map(First)),
            ?assert(maps:is_key(severity, First)),
            ?assert(maps:is_key(count, First));
        [] ->
            ok
    end.

andon_resolution_times_test() ->
    Result = tcps_visualization_data:andon_resolution_times(#{
        window => '1day',
        format => chartjs
    }),

    ?assert(is_map(Result)).

%%------------------------------------------------------------------------------
%% Learning Visualization Tests
%%------------------------------------------------------------------------------

learning_dashboard_test() ->
    Result = tcps_visualization_data:learning_dashboard(#{
        session_id => <<"session-001">>,
        format => chartjs
    }),

    ?assert(is_map(Result)),
    ?assert(maps:is_key(navigation_flow, Result)),
    ?assert(maps:is_key(tutorial_progress, Result)),
    ?assert(maps:is_key(learning_outcomes, Result)),
    ?assert(maps:is_key(session_summary, Result)).

quadrant_navigation_flow_test() ->
    Result = tcps_visualization_data:quadrant_navigation_flow(#{
        window => '1day',
        format => d3
    }),

    ?assert(is_map(Result)),
    ?assert(maps:is_key(nodes, Result)),
    ?assert(maps:is_key(links, Result)),

    Nodes = maps:get(nodes, Result),
    Links = maps:get(links, Result),
    ?assert(is_list(Nodes)),
    ?assert(is_list(Links)).

tutorial_progress_test() ->
    Result = tcps_visualization_data:tutorial_progress(#{
        session_id => <<"session-001">>,
        format => chartjs
    }),

    ?assert(is_map(Result)).

learning_outcomes_test() ->
    Result = tcps_visualization_data:learning_outcomes(#{
        session_id => <<"session-001">>,
        format => chartjs
    }),

    ?assert(is_map(Result)).

%%------------------------------------------------------------------------------
%% MCP Tool Visualization Tests
%%------------------------------------------------------------------------------

tool_usage_analytics_test() ->
    Result = tcps_visualization_data:tool_usage_analytics(#{
        tools => [list_tools, get_prompt, call_tool],
        window => '1hour',
        format => chartjs
    }),

    ?assert(is_map(Result)),
    ?assert(maps:is_key(invocation_frequency, Result)),
    ?assert(maps:is_key(latency_heatmap, Result)),
    ?assert(maps:is_key(error_rates, Result)),
    ?assert(maps:is_key(cache_analytics, Result)).

tool_latency_heatmap_test() ->
    Result = tcps_visualization_data:tool_latency_heatmap(#{
        window => '1hour',
        tools => [list_tools, call_tool],
        format => chartjs
    }),

    ?assert(is_map(Result)).

tool_latency_heatmap_d3_test() ->
    Result = tcps_visualization_data:tool_latency_heatmap(#{
        window => '1hour',
        tools => [list_tools],
        format => d3
    }),

    ?assert(is_list(Result)).

tool_error_rates_test() ->
    Result = tcps_visualization_data:tool_error_rates(#{
        window => '1day',
        format => chartjs
    }),

    ?assert(is_map(Result)).

tool_cache_analytics_test() ->
    Result = tcps_visualization_data:tool_cache_analytics(#{
        window => '1hour',
        format => chartjs
    }),

    ?assert(is_map(Result)),
    ?assert(maps:is_key(labels, Result)),
    ?assert(maps:is_key(datasets, Result)).

tool_cache_analytics_d3_test() ->
    Result = tcps_visualization_data:tool_cache_analytics(#{
        window => '1hour',
        format => d3
    }),

    ?assert(is_list(Result)),
    ?assertEqual(2, length(Result)),

    [First | _] = Result,
    ?assert(is_map(First)),
    ?assert(maps:is_key(metric, First)),
    ?assert(maps:is_key(value, First)).

%%------------------------------------------------------------------------------
%% Export Utility Tests
%%------------------------------------------------------------------------------

export_chartjs_test() ->
    Data = #{test => value},
    Result = tcps_visualization_data:export_chartjs(test_type, Data),

    ?assert(is_map(Result)),
    ?assertEqual(test_type, maps:get(type, Result)),
    ?assertEqual(chartjs, maps:get(format, Result)).

export_d3_test() ->
    Data = [#{value => 1}, #{value => 2}],
    Result = tcps_visualization_data:export_d3(test_type, Data),

    ?assert(is_list(Result)),
    ?assertEqual(Data, Result).

export_recharts_test() ->
    Data = [#{timestamp => 1000, value => 10}],
    Result = tcps_visualization_data:export_recharts(test_type, Data),

    ?assert(is_list(Result)),
    ?assertEqual(Data, Result).

export_json_test() ->
    Data = #{metric => 123},
    Result = tcps_visualization_data:export_json(test_type, Data),

    ?assert(is_map(Result)),
    ?assertEqual(test_type, maps:get(type, Result)),
    ?assertEqual(Data, maps:get(data, Result)).

%%------------------------------------------------------------------------------
%% Format Consistency Tests
%%------------------------------------------------------------------------------

multiple_formats_consistency_test() ->
    Opts = #{
        window => '1hour',
        buckets => [security, reliability]
    },

    %% Get same data in different formats
    ChartJS = tcps_visualization_data:kanban_heatmap(Opts#{format => chartjs}),
    D3 = tcps_visualization_data:kanban_heatmap(Opts#{format => d3}),
    Recharts = tcps_visualization_data:kanban_heatmap(Opts#{format => recharts}),
    JSON = tcps_visualization_data:kanban_heatmap(Opts#{format => json}),

    %% Verify format types
    ?assert(is_map(ChartJS)),
    ?assert(is_list(D3)),
    ?assert(is_list(Recharts)),
    ?assert(is_list(JSON)).

%%------------------------------------------------------------------------------
%% Window Parameter Tests
%%------------------------------------------------------------------------------

different_windows_test() ->
    Windows = ['1min', '5min', '15min', '1hour', '1day'],

    Results = [tcps_visualization_data:kanban_heatmap(#{
        window => Window,
        buckets => [security],
        format => chartjs
    }) || Window <- Windows],

    %% All should return valid data
    [?assert(is_map(R)) || R <- Results],
    [?assert(maps:is_key(labels, R)) || R <- Results].

%%------------------------------------------------------------------------------
%% Bucket Filtering Tests
%%------------------------------------------------------------------------------

bucket_filtering_test() ->
    AllBuckets = tcps_visualization_data:kanban_heatmap(#{
        buckets => [security, reliability, cost, compliance],
        format => chartjs
    }),

    SingleBucket = tcps_visualization_data:kanban_heatmap(#{
        buckets => [security],
        format => chartjs
    }),

    AllDatasets = maps:get(datasets, AllBuckets),
    SingleDatasets = maps:get(datasets, SingleBucket),

    ?assertEqual(4, length(AllDatasets)),
    ?assertEqual(1, length(SingleDatasets)).

%%------------------------------------------------------------------------------
%% Edge Case Tests
%%------------------------------------------------------------------------------

empty_buckets_test() ->
    Result = tcps_visualization_data:kanban_heatmap(#{
        buckets => [],
        format => chartjs
    }),

    ?assert(is_map(Result)).

invalid_format_fallback_test() ->
    Result = tcps_visualization_data:kanban_heatmap(#{
        window => '1hour',
        buckets => [security],
        format => invalid_format
    }),

    %% Should fallback gracefully
    ?assert(is_map(Result) orelse is_list(Result)).

missing_options_test() ->
    %% Should use defaults
    Result = tcps_visualization_data:kanban_heatmap(#{}),

    ?assert(is_map(Result) orelse is_list(Result)).

%%------------------------------------------------------------------------------
%% Data Structure Validation Tests
%%------------------------------------------------------------------------------

chartjs_structure_test() ->
    Result = tcps_visualization_data:kanban_heatmap(#{
        buckets => [security],
        format => chartjs
    }),

    ?assert(maps:is_key(labels, Result)),
    ?assert(maps:is_key(datasets, Result)),

    Labels = maps:get(labels, Result),
    Datasets = maps:get(datasets, Result),

    ?assert(is_list(Labels)),
    ?assert(is_list(Datasets)),

    case Datasets of
        [FirstDataset | _] ->
            ?assert(is_map(FirstDataset)),
            ?assert(maps:is_key(label, FirstDataset)),
            ?assert(maps:is_key(data, FirstDataset));
        [] ->
            ok
    end.

d3_structure_test() ->
    Result = tcps_visualization_data:kanban_utilization(#{
        buckets => [security, reliability],
        format => d3
    }),

    ?assert(is_list(Result)),

    case Result of
        [First | _] ->
            ?assert(is_map(First));
        [] ->
            ok
    end.

recharts_structure_test() ->
    Result = tcps_visualization_data:kanban_time_series(#{
        buckets => [security],
        format => recharts
    }),

    ?assert(is_list(Result)).
