%%%-----------------------------------------------------------------------------
%%% @doc TCPS MCP Diataxis Simulator - Visualization Data Generation
%%%
%%% Generates rich visualization data for the TCPS MCP Diataxis simulator
%%% dashboards. Produces data structures optimized for charting libraries
%%% (Chart.js, D3.js, Recharts) with multiple visualization types.
%%%
%%% == Visualization Types (5 Categories) ==
%%%
%%% 1. Kanban Board Heatmap
%%%    - WIP distribution across buckets and time
%%%    - Color-coded by utilization (green: low, yellow: medium, red: high)
%%%    - Time series heatmap showing WIP evolution
%%%    - Bucket capacity utilization percentage
%%%
%%% 2. Quality Gate Funnel
%%%    - Pass/fail funnel visualization
%%%    - Conversion rates between gates
%%%    - Remediation tracking
%%%    - Score distribution histograms
%%%
%%% 3. Andon Event Timeline
%%%    - Event occurrence timeline
%%%    - Severity-based coloring
%%%    - Resolution time tracking
%%%    - Impact radius visualization
%%%
%%% 4. Learning Progress Dashboard
%%%    - Quadrant navigation flow diagram
%%%    - Tutorial completion progress
%%%    - Learning outcome achievement
%%%    - Time-on-task analysis
%%%
%%% 5. MCP Tool Usage Analytics
%%%    - Tool invocation frequency
%%%    - Latency heatmaps
%%%    - Error rate tracking
%%%    - Cache hit rate visualization
%%%
%%% == Export Formats ==
%%%
%%% Chart.js Format:
%%% ```javascript
%%% {
%%%   labels: ['Security', 'Reliability', 'Cost', 'Compliance'],
%%%   datasets: [{
%%%     label: 'WIP Count',
%%%     data: [3, 5, 2, 4],
%%%     backgroundColor: ['#ff6384', '#36a2eb', '#ffce56', '#4bc0c0']
%%%   }]
%%% }
%%% '''
%%%
%%% D3.js Format:
%%% ```javascript
%%% [
%%%   {bucket: 'security', wip: 3, capacity: 5, utilization: 0.6},
%%%   {bucket: 'reliability', wip: 5, capacity: 8, utilization: 0.625}
%%% ]
%%% '''
%%%
%%% Recharts Format:
%%% ```javascript
%%% [
%%%   {timestamp: 1640000000, security: 3, reliability: 5, cost: 2},
%%%   {timestamp: 1640000060, security: 4, reliability: 4, cost: 3}
%%% ]
%%% '''
%%%
%%% == Usage ==
%%% ```erlang
%%% %% Generate Kanban heatmap
%%% Heatmap = tcps_visualization_data:kanban_heatmap(#{
%%%     window => '1hour',
%%%     buckets => [security, reliability, cost, compliance],
%%%     format => chartjs
%%% }),
%%%
%%% %% Generate quality gate funnel
%%% Funnel = tcps_visualization_data:quality_gate_funnel(#{
%%%     gates => [code_coverage, test_pass_rate, security_scan],
%%%     format => d3
%%% }),
%%%
%%% %% Generate Andon timeline
%%% Timeline = tcps_visualization_data:andon_timeline(#{
%%%     window => '1day',
%%%     severity_filter => [high, critical],
%%%     format => recharts
%%% }),
%%%
%%% %% Generate learning dashboard
%%% Dashboard = tcps_visualization_data:learning_dashboard(#{
%%%     session_id => <<"session-001">>,
%%%     include_navigation => true,
%%%     format => chartjs
%%% }),
%%%
%%% %% Generate tool usage analytics
%%% Analytics = tcps_visualization_data:tool_usage_analytics(#{
%%%     tools => [list_tools, get_prompt, call_tool],
%%%     window => '1hour',
%%%     format => d3
%%% }).
%%% '''
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_visualization_data).

%% API exports
-export([
    %% Kanban visualizations
    kanban_heatmap/1,
    kanban_time_series/1,
    kanban_utilization/1,

    %% Quality gate visualizations
    quality_gate_funnel/1,
    quality_gate_scores/1,
    quality_gate_trends/1,

    %% Andon event visualizations
    andon_timeline/1,
    andon_severity_distribution/1,
    andon_resolution_times/1,

    %% Learning visualizations
    learning_dashboard/1,
    quadrant_navigation_flow/1,
    tutorial_progress/1,
    learning_outcomes/1,

    %% MCP tool visualizations
    tool_usage_analytics/1,
    tool_latency_heatmap/1,
    tool_error_rates/1,
    tool_cache_analytics/1,

    %% Export utilities
    export_chartjs/2,
    export_d3/2,
    export_recharts/2,
    export_json/2
]).

%% Types
-type format() :: chartjs | d3 | recharts | json.
-type window() :: '1min' | '5min' | '15min' | '1hour' | '1day'.
-type bucket() :: reliability | security | cost | compliance.
-type visualization_opts() :: #{
    window => window(),
    format => format(),
    buckets => [bucket()],
    severity_filter => [atom()],
    include_trends => boolean()
}.

%%%=============================================================================
%%% Kanban Visualizations
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Generate Kanban board heatmap visualization data.
%%
%% Returns a heatmap showing WIP distribution across buckets and time windows.
%% Color intensity represents utilization percentage (0-100%).
%%
%% Options:
%% - window: Time window for data aggregation
%% - buckets: List of buckets to include
%% - format: Output format (chartjs | d3 | recharts)
%%
%% ChartJS Output:
%% ```javascript
%% {
%%   labels: ['00:00', '00:05', '00:10', ...],
%%   datasets: [
%%     {label: 'Security', data: [60, 80, 100, ...], backgroundColor: '#ff6384'},
%%     {label: 'Reliability', data: [40, 50, 60, ...], backgroundColor: '#36a2eb'}
%%   ]
%% }
%% '''
%%
%% @end
%%------------------------------------------------------------------------------
-spec kanban_heatmap(visualization_opts()) -> map() | [map()].
kanban_heatmap(Opts) ->
    Window = maps:get(window, Opts, '1hour'),
    Buckets = maps:get(buckets, Opts, [reliability, security, cost, compliance]),
    Format = maps:get(format, Opts, chartjs),

    %% Get WIP data from metrics collector
    WipData = get_wip_time_series(Window, Buckets),

    %% Generate visualization based on format
    case Format of
        chartjs -> format_heatmap_chartjs(WipData, Buckets);
        d3 -> format_heatmap_d3(WipData, Buckets);
        recharts -> format_heatmap_recharts(WipData, Buckets);
        json -> WipData
    end.

%%------------------------------------------------------------------------------
%% @doc Generate Kanban WIP time series visualization.
%%
%% Shows WIP evolution over time for each bucket.
%%
%% @end
%%------------------------------------------------------------------------------
-spec kanban_time_series(visualization_opts()) -> map() | [map()].
kanban_time_series(Opts) ->
    Window = maps:get(window, Opts, '1hour'),
    Buckets = maps:get(buckets, Opts, [reliability, security, cost, compliance]),
    Format = maps:get(format, Opts, chartjs),

    TimeSeries = get_wip_time_series(Window, Buckets),

    case Format of
        chartjs -> format_time_series_chartjs(TimeSeries, Buckets);
        d3 -> format_time_series_d3(TimeSeries, Buckets);
        recharts -> format_time_series_recharts(TimeSeries, Buckets);
        json -> TimeSeries
    end.

%%------------------------------------------------------------------------------
%% @doc Generate Kanban utilization gauge visualization.
%%
%% Shows current utilization percentage per bucket with capacity limits.
%%
%% @end
%%------------------------------------------------------------------------------
-spec kanban_utilization(visualization_opts()) -> map() | [map()].
kanban_utilization(Opts) ->
    Buckets = maps:get(buckets, Opts, [reliability, security, cost, compliance]),
    Format = maps:get(format, Opts, chartjs),

    Utilization = get_current_utilization(Buckets),

    case Format of
        chartjs -> format_utilization_chartjs(Utilization);
        d3 -> format_utilization_d3(Utilization);
        recharts -> format_utilization_recharts(Utilization);
        json -> Utilization
    end.

%%%=============================================================================
%%% Quality Gate Visualizations
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Generate quality gate funnel visualization.
%%
%% Shows conversion rates through quality gate pipeline.
%%
%% Options:
%% - gates: List of gate types to include
%% - format: Output format
%%
%% Funnel stages:
%% 1. Work orders created
%% 2. Code coverage gate
%% 3. Test pass rate gate
%% 4. Security scan gate
%% 5. Performance gate
%% 6. Completed work orders
%%
%% @end
%%------------------------------------------------------------------------------
-spec quality_gate_funnel(visualization_opts()) -> map() | [map()].
quality_gate_funnel(Opts) ->
    Gates = maps:get(gates, Opts, [code_coverage, test_pass_rate, security_scan, performance]),
    Format = maps:get(format, Opts, chartjs),

    FunnelData = get_quality_gate_funnel_data(Gates),

    case Format of
        chartjs -> format_funnel_chartjs(FunnelData);
        d3 -> format_funnel_d3(FunnelData);
        recharts -> format_funnel_recharts(FunnelData);
        json -> FunnelData
    end.

%%------------------------------------------------------------------------------
%% @doc Generate quality gate score distribution visualization.
%%
%% Shows histogram of quality scores for each gate type.
%%
%% @end
%%------------------------------------------------------------------------------
-spec quality_gate_scores(visualization_opts()) -> map() | [map()].
quality_gate_scores(Opts) ->
    Gates = maps:get(gates, Opts, [code_coverage, test_pass_rate, security_scan]),
    Format = maps:get(format, Opts, chartjs),

    Scores = get_quality_gate_scores(Gates),

    case Format of
        chartjs -> format_scores_chartjs(Scores);
        d3 -> format_scores_d3(Scores);
        recharts -> format_scores_recharts(Scores);
        json -> Scores
    end.

%%------------------------------------------------------------------------------
%% @doc Generate quality gate trend visualization.
%%
%% Shows pass/fail rates over time.
%%
%% @end
%%------------------------------------------------------------------------------
-spec quality_gate_trends(visualization_opts()) -> map() | [map()].
quality_gate_trends(Opts) ->
    Window = maps:get(window, Opts, '1day'),
    Gates = maps:get(gates, Opts, [code_coverage, test_pass_rate]),
    Format = maps:get(format, Opts, chartjs),

    Trends = get_quality_gate_trends(Window, Gates),

    case Format of
        chartjs -> format_trends_chartjs(Trends);
        d3 -> format_trends_d3(Trends);
        recharts -> format_trends_recharts(Trends);
        json -> Trends
    end.

%%%=============================================================================
%%% Andon Event Visualizations
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Generate Andon event timeline visualization.
%%
%% Shows events over time with severity-based coloring.
%%
%% Options:
%% - window: Time window
%% - severity_filter: Filter by severity levels
%% - format: Output format
%%
%% @end
%%------------------------------------------------------------------------------
-spec andon_timeline(visualization_opts()) -> map() | [map()].
andon_timeline(Opts) ->
    Window = maps:get(window, Opts, '1day'),
    SeverityFilter = maps:get(severity_filter, Opts, [low, medium, high, critical]),
    Format = maps:get(format, Opts, d3),

    Events = get_andon_events(Window, SeverityFilter),

    case Format of
        chartjs -> format_timeline_chartjs(Events);
        d3 -> format_timeline_d3(Events);
        recharts -> format_timeline_recharts(Events);
        json -> Events
    end.

%%------------------------------------------------------------------------------
%% @doc Generate Andon severity distribution visualization.
%%
%% Shows pie chart of event distribution by severity.
%%
%% @end
%%------------------------------------------------------------------------------
-spec andon_severity_distribution(visualization_opts()) -> map() | [map()].
andon_severity_distribution(Opts) ->
    Window = maps:get(window, Opts, '1day'),
    Format = maps:get(format, Opts, chartjs),

    Distribution = get_andon_severity_distribution(Window),

    case Format of
        chartjs -> format_distribution_chartjs(Distribution);
        d3 -> format_distribution_d3(Distribution);
        recharts -> format_distribution_recharts(Distribution);
        json -> Distribution
    end.

%%------------------------------------------------------------------------------
%% @doc Generate Andon resolution time visualization.
%%
%% Shows histogram of resolution times by severity.
%%
%% @end
%%------------------------------------------------------------------------------
-spec andon_resolution_times(visualization_opts()) -> map() | [map()].
andon_resolution_times(Opts) ->
    Window = maps:get(window, Opts, '1day'),
    Format = maps:get(format, Opts, chartjs),

    ResolutionTimes = get_andon_resolution_times(Window),

    case Format of
        chartjs -> format_resolution_times_chartjs(ResolutionTimes);
        d3 -> format_resolution_times_d3(ResolutionTimes);
        recharts -> format_resolution_times_recharts(ResolutionTimes);
        json -> ResolutionTimes
    end.

%%%=============================================================================
%%% Learning Visualizations
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Generate comprehensive learning dashboard.
%%
%% Combines multiple learning metrics into a single dashboard view.
%%
%% @end
%%------------------------------------------------------------------------------
-spec learning_dashboard(visualization_opts()) -> map().
learning_dashboard(Opts) ->
    SessionId = maps:get(session_id, Opts, undefined),

    #{
        navigation_flow => quadrant_navigation_flow(Opts),
        tutorial_progress => tutorial_progress(Opts),
        learning_outcomes => learning_outcomes(Opts),
        session_summary => get_session_summary(SessionId)
    }.

%%------------------------------------------------------------------------------
%% @doc Generate quadrant navigation flow diagram.
%%
%% Shows Sankey/flow diagram of navigation between Diataxis quadrants.
%%
%% @end
%%------------------------------------------------------------------------------
-spec quadrant_navigation_flow(visualization_opts()) -> map() | [map()].
quadrant_navigation_flow(Opts) ->
    Window = maps:get(window, Opts, '1day'),
    Format = maps:get(format, Opts, d3),

    FlowData = get_quadrant_navigation_flow(Window),

    case Format of
        d3 -> format_flow_d3(FlowData);
        json -> FlowData;
        _ -> format_flow_d3(FlowData)
    end.

%%------------------------------------------------------------------------------
%% @doc Generate tutorial progress visualization.
%%
%% Shows step-by-step completion progress with time-on-task.
%%
%% @end
%%------------------------------------------------------------------------------
-spec tutorial_progress(visualization_opts()) -> map() | [map()].
tutorial_progress(Opts) ->
    SessionId = maps:get(session_id, Opts, undefined),
    Format = maps:get(format, Opts, chartjs),

    Progress = get_tutorial_progress(SessionId),

    case Format of
        chartjs -> format_progress_chartjs(Progress);
        d3 -> format_progress_d3(Progress);
        recharts -> format_progress_recharts(Progress);
        json -> Progress
    end.

%%------------------------------------------------------------------------------
%% @doc Generate learning outcomes achievement visualization.
%%
%% Shows radar chart of achieved vs. expected learning outcomes.
%%
%% @end
%%------------------------------------------------------------------------------
-spec learning_outcomes(visualization_opts()) -> map() | [map()].
learning_outcomes(Opts) ->
    SessionId = maps:get(session_id, Opts, undefined),
    Format = maps:get(format, Opts, chartjs),

    Outcomes = get_learning_outcomes(SessionId),

    case Format of
        chartjs -> format_outcomes_chartjs(Outcomes);
        d3 -> format_outcomes_d3(Outcomes);
        recharts -> format_outcomes_recharts(Outcomes);
        json -> Outcomes
    end.

%%%=============================================================================
%%% MCP Tool Visualizations
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Generate MCP tool usage analytics.
%%
%% Shows comprehensive tool usage metrics.
%%
%% @end
%%------------------------------------------------------------------------------
-spec tool_usage_analytics(visualization_opts()) -> map().
tool_usage_analytics(Opts) ->
    #{
        invocation_frequency => tool_invocation_frequency(Opts),
        latency_heatmap => tool_latency_heatmap(Opts),
        error_rates => tool_error_rates(Opts),
        cache_analytics => tool_cache_analytics(Opts)
    }.

%%------------------------------------------------------------------------------
%% @doc Generate tool latency heatmap.
%%
%% Shows latency distribution across tools and time.
%%
%% @end
%%------------------------------------------------------------------------------
-spec tool_latency_heatmap(visualization_opts()) -> map() | [map()].
tool_latency_heatmap(Opts) ->
    Window = maps:get(window, Opts, '1hour'),
    Tools = maps:get(tools, Opts, [list_tools, get_prompt, call_tool]),
    Format = maps:get(format, Opts, chartjs),

    Latencies = get_tool_latencies(Window, Tools),

    case Format of
        chartjs -> format_latency_heatmap_chartjs(Latencies);
        d3 -> format_latency_heatmap_d3(Latencies);
        recharts -> format_latency_heatmap_recharts(Latencies);
        json -> Latencies
    end.

%%------------------------------------------------------------------------------
%% @doc Generate tool error rate visualization.
%%
%% Shows error rates over time per tool.
%%
%% @end
%%------------------------------------------------------------------------------
-spec tool_error_rates(visualization_opts()) -> map() | [map()].
tool_error_rates(Opts) ->
    Window = maps:get(window, Opts, '1day'),
    Format = maps:get(format, Opts, chartjs),

    ErrorRates = get_tool_error_rates(Window),

    case Format of
        chartjs -> format_error_rates_chartjs(ErrorRates);
        d3 -> format_error_rates_d3(ErrorRates);
        recharts -> format_error_rates_recharts(ErrorRates);
        json -> ErrorRates
    end.

%%------------------------------------------------------------------------------
%% @doc Generate tool cache analytics visualization.
%%
%% Shows cache hit/miss rates and efficiency.
%%
%% @end
%%------------------------------------------------------------------------------
-spec tool_cache_analytics(visualization_opts()) -> map() | [map()].
tool_cache_analytics(Opts) ->
    Window = maps:get(window, Opts, '1hour'),
    Format = maps:get(format, Opts, chartjs),

    CacheData = get_tool_cache_data(Window),

    case Format of
        chartjs -> format_cache_analytics_chartjs(CacheData);
        d3 -> format_cache_analytics_d3(CacheData);
        recharts -> format_cache_analytics_recharts(CacheData);
        json -> CacheData
    end.

%%%=============================================================================
%%% Export Utilities
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Export data in Chart.js format.
%% @end
%%------------------------------------------------------------------------------
-spec export_chartjs(atom(), term()) -> map().
export_chartjs(Type, Data) ->
    #{type => Type, data => Data, format => chartjs}.

%%------------------------------------------------------------------------------
%% @doc Export data in D3.js format.
%% @end
%%------------------------------------------------------------------------------
-spec export_d3(atom(), term()) -> [map()].
export_d3(_Type, Data) ->
    Data.

%%------------------------------------------------------------------------------
%% @doc Export data in Recharts format.
%% @end
%%------------------------------------------------------------------------------
-spec export_recharts(atom(), term()) -> [map()].
export_recharts(_Type, Data) ->
    Data.

%%------------------------------------------------------------------------------
%% @doc Export data in JSON format.
%% @end
%%------------------------------------------------------------------------------
-spec export_json(atom(), term()) -> map().
export_json(Type, Data) ->
    #{type => Type, data => Data}.

%%%=============================================================================
%%% Internal Functions - Data Retrieval
%%%=============================================================================

get_wip_time_series(Window, Buckets) ->
    %% Simulate WIP time series data
    %% In production, this would query tcps_metrics_collector
    TimePoints = generate_time_points(Window),
    lists:map(fun(Time) ->
        #{
            timestamp => Time,
            data => lists:map(fun(Bucket) ->
                {Bucket, rand:uniform(10)}
            end, Buckets)
        }
    end, TimePoints).

get_current_utilization(Buckets) ->
    %% Simulate current utilization data
    Capacities = #{reliability => 8, security => 5, cost => 6, compliance => 4},
    lists:map(fun(Bucket) ->
        Capacity = maps:get(Bucket, Capacities, 5),
        Current = rand:uniform(Capacity),
        #{
            bucket => Bucket,
            current_wip => Current,
            capacity => Capacity,
            utilization => (Current / Capacity) * 100
        }
    end, Buckets).

get_quality_gate_funnel_data(Gates) ->
    %% Simulate funnel data
    Initial = 100,
    lists:foldl(fun(Gate, {Acc, Remaining}) ->
        PassRate = 0.7 + (rand:uniform() * 0.25),  % 70-95% pass rate
        Passed = round(Remaining * PassRate),
        Entry = #{gate => Gate, total => Remaining, passed => Passed, failed => Remaining - Passed},
        {[Entry | Acc], Passed}
    end, {[], Initial}, Gates).

get_quality_gate_scores(_Gates) ->
    %% Simulate score distributions
    #{}.

get_quality_gate_trends(_Window, _Gates) ->
    %% Simulate trend data
    #{}.

get_andon_events(_Window, _SeverityFilter) ->
    %% Simulate Andon events
    [].

get_andon_severity_distribution(_Window) ->
    #{low => 10, medium => 5, high => 2, critical => 1}.

get_andon_resolution_times(_Window) ->
    #{}.

get_session_summary(_SessionId) ->
    #{}.

get_quadrant_navigation_flow(_Window) ->
    %% Simulate Sankey diagram data
    #{
        nodes => [
            #{id => <<"tutorial">>, name => <<"Tutorial">>},
            #{id => <<"howto">>, name => <<"How-To">>},
            #{id => <<"reference">>, name => <<"Reference">>},
            #{id => <<"explanation">>, name => <<"Explanation">>}
        ],
        links => [
            #{source => <<"tutorial">>, target => <<"howto">>, value => 10},
            #{source => <<"tutorial">>, target => <<"reference">>, value => 5},
            #{source => <<"howto">>, target => <<"reference">>, value => 8}
        ]
    }.

get_tutorial_progress(_SessionId) ->
    [].

get_learning_outcomes(_SessionId) ->
    #{}.

get_tool_latencies(_Window, _Tools) ->
    #{}.

get_tool_error_rates(_Window) ->
    #{}.

get_tool_cache_data(_Window) ->
    #{hit_rate => 0.75, miss_rate => 0.25, total_requests => 1000}.

%%%=============================================================================
%%% Internal Functions - Formatting
%%%=============================================================================

format_heatmap_chartjs(WipData, Buckets) ->
    Labels = [format_timestamp(maps:get(timestamp, D)) || D <- WipData],
    Datasets = lists:map(fun(Bucket) ->
        Data = [proplists:get_value(Bucket, maps:get(data, D), 0) || D <- WipData],
        #{
            label => atom_to_binary(Bucket),
            data => Data,
            backgroundColor => bucket_color(Bucket)
        }
    end, Buckets),
    #{labels => Labels, datasets => Datasets}.

format_heatmap_d3(WipData, _Buckets) ->
    WipData.

format_heatmap_recharts(WipData, _Buckets) ->
    lists:map(fun(#{timestamp := Time, data := Data}) ->
        maps:from_list([{timestamp, Time} | Data])
    end, WipData).

format_time_series_chartjs(TimeSeries, Buckets) ->
    format_heatmap_chartjs(TimeSeries, Buckets).

format_time_series_d3(TimeSeries, _Buckets) ->
    TimeSeries.

format_time_series_recharts(TimeSeries, Buckets) ->
    format_heatmap_recharts(TimeSeries, Buckets).

format_utilization_chartjs(Utilization) ->
    Labels = [atom_to_binary(maps:get(bucket, U)) || U <- Utilization],
    Data = [maps:get(utilization, U) || U <- Utilization],
    #{
        labels => Labels,
        datasets => [#{
            label => <<"Utilization %">>,
            data => Data,
            backgroundColor => lists:map(fun(U) ->
                utilization_color(maps:get(utilization, U))
            end, Utilization)
        }]
    }.

format_utilization_d3(Utilization) ->
    Utilization.

format_utilization_recharts(Utilization) ->
    Utilization.

format_funnel_chartjs({FunnelData, _}) ->
    #{
        labels => [atom_to_binary(maps:get(gate, E)) || E <- lists:reverse(FunnelData)],
        datasets => [#{
            label => <<"Total">>,
            data => [maps:get(total, E) || E <- lists:reverse(FunnelData)]
        }]
    }.

format_funnel_d3({FunnelData, _}) ->
    lists:reverse(FunnelData).

format_funnel_recharts(Data) ->
    format_funnel_d3(Data).

format_scores_chartjs(_Scores) -> #{}.
format_scores_d3(_Scores) -> [].
format_scores_recharts(_Scores) -> [].

format_trends_chartjs(_Trends) -> #{}.
format_trends_d3(_Trends) -> [].
format_trends_recharts(_Trends) -> [].

format_timeline_chartjs(_Events) -> #{}.
format_timeline_d3(Events) -> Events.
format_timeline_recharts(Events) -> Events.

format_distribution_chartjs(Distribution) ->
    #{
        labels => [atom_to_binary(K) || K <- maps:keys(Distribution)],
        datasets => [#{
            data => maps:values(Distribution),
            backgroundColor => [<<"#ff6384">>, <<"#36a2eb">>, <<"#ffce56">>, <<"#4bc0c0">>]
        }]
    }.

format_distribution_d3(Distribution) ->
    [#{severity => K, count => V} || {K, V} <- maps:to_list(Distribution)].

format_distribution_recharts(Distribution) ->
    format_distribution_d3(Distribution).

format_resolution_times_chartjs(_Times) -> #{}.
format_resolution_times_d3(_Times) -> [].
format_resolution_times_recharts(_Times) -> [].

format_flow_d3(FlowData) -> FlowData.

format_progress_chartjs(_Progress) -> #{}.
format_progress_d3(_Progress) -> [].
format_progress_recharts(_Progress) -> [].

format_outcomes_chartjs(_Outcomes) -> #{}.
format_outcomes_d3(_Outcomes) -> [].
format_outcomes_recharts(_Outcomes) -> [].

tool_invocation_frequency(_Opts) -> #{}.

format_latency_heatmap_chartjs(_Latencies) -> #{}.
format_latency_heatmap_d3(_Latencies) -> [].
format_latency_heatmap_recharts(_Latencies) -> [].

format_error_rates_chartjs(_ErrorRates) -> #{}.
format_error_rates_d3(_ErrorRates) -> [].
format_error_rates_recharts(_ErrorRates) -> [].

format_cache_analytics_chartjs(CacheData) ->
    #{
        labels => [<<"Hit Rate">>, <<"Miss Rate">>],
        datasets => [#{
            data => [maps:get(hit_rate, CacheData) * 100, maps:get(miss_rate, CacheData) * 100],
            backgroundColor => [<<"#4bc0c0">>, <<"#ff6384">>]
        }]
    }.

format_cache_analytics_d3(CacheData) ->
    [
        #{metric => hit_rate, value => maps:get(hit_rate, CacheData)},
        #{metric => miss_rate, value => maps:get(miss_rate, CacheData)}
    ].

format_cache_analytics_recharts(CacheData) ->
    format_cache_analytics_d3(CacheData).

%%%=============================================================================
%%% Utility Functions
%%%=============================================================================

generate_time_points('1min') -> generate_points(60, 1);
generate_time_points('5min') -> generate_points(60, 5);
generate_time_points('15min') -> generate_points(60, 15);
generate_time_points('1hour') -> generate_points(60, 60);
generate_time_points('1day') -> generate_points(3600, 24);
generate_time_points(_) -> generate_points(60, 5).

generate_points(Interval, Count) ->
    Now = erlang:system_time(second),
    [Now - (I * Interval) || I <- lists:seq(0, Count - 1)].

format_timestamp(Timestamp) ->
    {{Y, M, D}, {H, Min, _S}} = calendar:now_to_datetime({Timestamp div 1000000, Timestamp rem 1000000, 0}),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B", [Y, M, D, H, Min])).

bucket_color(reliability) -> <<"#36a2eb">>;
bucket_color(security) -> <<"#ff6384">>;
bucket_color(cost) -> <<"#ffce56">>;
bucket_color(compliance) -> <<"#4bc0c0">>;
bucket_color(_) -> <<"#9966ff">>.

utilization_color(Util) when Util < 50 -> <<"#4bc0c0">>;
utilization_color(Util) when Util < 80 -> <<"#ffce56">>;
utilization_color(_) -> <<"#ff6384">>.
