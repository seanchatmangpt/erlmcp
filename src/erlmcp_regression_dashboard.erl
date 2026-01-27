-module(erlmcp_regression_dashboard).

%% API
-export([
    start_dashboard/1,
    stop_dashboard/0,
    get_dashboard_data/0,
    update_dashboard/1,
    render_dashboard/0,
    export_dashboard/1,
    get_real_time_metrics/0
]).

%% Internal functions
-export([
    format_dashboard_html/1,
    generate_charts_data/1,
    create_alert_panel/1,
    create_trends_panel/1,
    create_metrics_panel/1,
    dashboard_loop_init/1,
    dashboard_loop/1,
    websocket_handler/2
]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-define(DASHBOARD_PORT, 8080).
-define(REFRESH_INTERVAL, 5000). % 5 seconds
-define(HISTORY_RETENTION, 86400000). % 24 hours in milliseconds

-record(dashboard_state, {
    port :: integer(),
    pid :: pid(),
    last_update :: integer(),
    metrics_history :: [map()],
    active_alerts :: [map()]
}).

%% @doc Start the regression tracking dashboard
start_dashboard(Port) ->
    SpanCtx = otel_tracer:start_span(<<"start_regression_dashboard">>),
    otel_span:set_attribute(SpanCtx, port, Port),
    
    try
        case whereis(regression_dashboard) of
            undefined ->
                Pid = spawn_link(?MODULE, dashboard_loop_init, [Port]),
                register(regression_dashboard, Pid),
                setup_dashboard_tables(),
                start_web_server(Port),
                ?LOG_INFO("Regression dashboard started on port ~p", [Port]),
                otel_span:set_status(SpanCtx, ok),
                {ok, Pid};
            Pid ->
                ?LOG_INFO("Dashboard already running with PID ~p", [Pid]),
                otel_span:set_status(SpanCtx, ok),
                {ok, Pid}
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to start dashboard: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Stop the regression tracking dashboard
stop_dashboard() ->
    SpanCtx = otel_tracer:start_span(<<"stop_regression_dashboard">>),
    
    try
        case whereis(regression_dashboard) of
            undefined ->
                ?LOG_INFO("Dashboard not running"),
                otel_span:set_status(SpanCtx, ok),
                ok;
            Pid ->
                unregister(regression_dashboard),
                exit(Pid, shutdown),
                cleanup_dashboard_tables(),
                ?LOG_INFO("Regression dashboard stopped"),
                otel_span:set_status(SpanCtx, ok),
                ok
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to stop dashboard: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Get current dashboard data
get_dashboard_data() ->
    SpanCtx = otel_tracer:start_span(<<"get_dashboard_data">>),
    
    try
        CurrentMetrics = get_current_metrics(),
        RecentAlerts = get_recent_alerts(),
        PerformanceTrends = get_performance_trends(),
        BaselineStatus = get_baseline_status(),
        
        Data = #{
            timestamp => erlang:system_time(millisecond),
            metrics => CurrentMetrics,
            alerts => RecentAlerts,
            trends => PerformanceTrends,
            baselines => BaselineStatus,
            system_status => get_system_status()
        },
        
        otel_span:set_status(SpanCtx, ok),
        {ok, Data}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to get dashboard data: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Update dashboard with new data
update_dashboard(NewData) ->
    SpanCtx = otel_tracer:start_span(<<"update_dashboard">>),
    
    try
        Timestamp = erlang:system_time(millisecond),
        
        % Store metrics history
        store_metrics_history(NewData, Timestamp),
        
        % Update alerts if any regressions detected
        case maps:get(regressions, NewData, []) of
            [] -> ok;
            Regressions -> update_alerts(Regressions, Timestamp)
        end,
        
        % Notify connected clients
        broadcast_update(NewData),
        
        otel_span:set_status(SpanCtx, ok),
        ok
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to update dashboard: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Render dashboard HTML
render_dashboard() ->
    SpanCtx = otel_tracer:start_span(<<"render_dashboard">>),
    
    try
        {ok, Data} = get_dashboard_data(),
        HTML = format_dashboard_html(Data),
        otel_span:set_status(SpanCtx, ok),
        {ok, HTML}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to render dashboard: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Export dashboard data
export_dashboard(Format) ->
    SpanCtx = otel_tracer:start_span(<<"export_dashboard">>),
    otel_span:set_attribute(SpanCtx, format, Format),
    
    try
        {ok, Data} = get_dashboard_data(),
        ExportedData = case Format of
            json -> jsx:encode(Data);
            csv -> format_csv(Data);
            html -> element(2, render_dashboard())
        end,
        otel_span:set_status(SpanCtx, ok),
        {ok, ExportedData}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to export dashboard: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% @doc Get real-time metrics
get_real_time_metrics() ->
    SpanCtx = otel_tracer:start_span(<<"get_real_time_metrics">>),
    
    try
        Metrics = #{
            latency => get_current_latency(),
            throughput => get_current_throughput(),
            error_rate => get_current_error_rate(),
            cpu_usage => get_current_cpu(),
            memory_usage => get_current_memory(),
            active_connections => get_active_connections()
        },
        otel_span:set_status(SpanCtx, ok),
        {ok, Metrics}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to get real-time metrics: ~p:~p", [Error, Reason]),
            otel_span:set_status(SpanCtx, error),
            {error, {Error, Reason}}
    after
        otel_span:end_span(SpanCtx)
    end.

%% Internal functions

%% @doc Dashboard main loop initialization
dashboard_loop_init(Port) ->
    State = #dashboard_state{
        port = Port,
        pid = self(),
        last_update = erlang:system_time(millisecond),
        metrics_history = [],
        active_alerts = []
    },
    dashboard_loop(State).

%% @doc Dashboard main loop
dashboard_loop(State) ->
    receive
        {update, Data} ->
            NewState = handle_update(Data, State),
            dashboard_loop(NewState);
        {get_data, From} ->
            From ! {dashboard_data, get_dashboard_state(State)},
            dashboard_loop(State);
        stop ->
            ?LOG_INFO("Dashboard loop stopping"),
            ok;
        Msg ->
            ?LOG_WARNING("Unknown message in dashboard loop: ~p", [Msg]),
            dashboard_loop(State)
    after ?REFRESH_INTERVAL ->
        % Auto-refresh
        {ok, Metrics} = get_real_time_metrics(),
        update_dashboard(#{metrics => Metrics}),
        dashboard_loop(State)
    end.

%% @doc Setup dashboard ETS tables
setup_dashboard_tables() ->
    ets:new(dashboard_metrics, [named_table, public, ordered_set]),
    ets:new(dashboard_alerts, [named_table, public, ordered_set]),
    ets:new(dashboard_clients, [named_table, public, set]).

%% @doc Cleanup dashboard ETS tables
cleanup_dashboard_tables() ->
    ets:delete(dashboard_metrics),
    ets:delete(dashboard_alerts),
    ets:delete(dashboard_clients).

%% @doc Format dashboard HTML
format_dashboard_html(Data) ->
    ChartsData = generate_charts_data(Data),
    AlertsPanel = create_alert_panel(Data),
    TrendsPanel = create_trends_panel(Data),
    MetricsPanel = create_metrics_panel(Data),
    
    HTML = [
        "<!DOCTYPE html>",
        "<html>",
        "<head>",
        "    <title>Performance Regression Dashboard</title>",
        "    <meta charset='utf-8'>",
        "    <meta name='viewport' content='width=device-width, initial-scale=1'>",
        "    <script src='https://cdn.plot.ly/plotly-latest.min.js'></script>",
        "    <script src='https://code.jquery.com/jquery-3.6.0.min.js'></script>",
        "    <style>",
        get_dashboard_css(),
        "    </style>",
        "</head>",
        "<body>",
        "    <div class='dashboard-header'>",
        "        <h1>Performance Regression Dashboard</h1>",
        "        <div class='status-indicator ", get_overall_status_class(Data), "'>",
        "            ", get_overall_status(Data), "",
        "        </div>",
        "    </div>",
        "    <div class='dashboard-container'>",
        "        <div class='panel-row'>",
        AlertsPanel,
        "        </div>",
        "        <div class='panel-row'>",
        MetricsPanel,
        TrendsPanel,
        "        </div>",
        "        <div class='panel-row'>",
        "            <div class='panel chart-panel'>",
        "                <h3>Performance Metrics</h3>",
        "                <div id='metrics-chart'></div>",
        "            </div>",
        "        </div>",
        "    </div>",
        "    <script>",
        ChartsData,
        get_dashboard_js(),
        "    </script>",
        "</body>",
        "</html>"
    ],
    lists:flatten(HTML).

%% @doc Generate charts data for dashboard
generate_charts_data(Data) ->
    Metrics = maps:get(metrics, Data, #{}),
    Timestamp = maps:get(timestamp, Data, erlang:system_time(millisecond)),
    
    [
        "var metricsData = [",
        format_metric_trace("Latency (ms)", maps:get(latency, Metrics, 0)),
        ",",
        format_metric_trace("Throughput (req/s)", maps:get(throughput, Metrics, 0)),
        ",",
        format_metric_trace("Error Rate (%)", maps:get(error_rate, Metrics, 0)),
        ",",
        format_metric_trace("CPU Usage (%)", maps:get(cpu_usage, Metrics, 0)),
        ",",
        format_metric_trace("Memory Usage (%)", maps:get(memory_usage, Metrics, 0)),
        "];",
        "var layout = {",
        "    title: 'Real-time Performance Metrics',",
        "    xaxis: { title: 'Time' },",
        "    yaxis: { title: 'Value' },",
        "    showlegend: true",
        "};",
        "Plotly.newPlot('metrics-chart', metricsData, layout);"
    ].

%% @doc Format metric trace for Plotly
format_metric_trace(Name, Value) ->
    [
        "{",
        "x: ['", format_timestamp(erlang:system_time(millisecond)), "'],",
        "y: [", float_to_list(Value, [{decimals, 2}]), "],",
        "name: '", Name, "',",
        "type: 'scatter',",
        "mode: 'lines+markers'",
        "}"
    ].

%% @doc Create alerts panel
create_alert_panel(Data) ->
    Alerts = maps:get(alerts, Data, []),
    ActiveAlerts = lists:filter(fun(Alert) -> maps:get(active, Alert, false) end, Alerts),
    
    [
        "<div class='panel alert-panel'>",
        "    <h3>Active Alerts (", integer_to_list(length(ActiveAlerts)), ")</h3>",
        case ActiveAlerts of
            [] -> 
                "    <div class='no-alerts'>No active performance regressions</div>";
            _ ->
                ["    <div class='alert-list'>"] ++
                [format_alert_item(Alert) || Alert <- ActiveAlerts] ++
                ["    </div>"]
        end,
        "</div>"
    ].

%% @doc Create trends panel
create_trends_panel(Data) ->
    Trends = maps:get(trends, Data, #{}),
    
    [
        "<div class='panel trends-panel'>",
        "    <h3>Performance Trends</h3>",
        "    <div class='trend-item'>",
        "        <span class='trend-label'>Overall Trend:</span>",
        "        <span class='trend-value ", get_trend_class(Trends), "'>",
        maps:get(overall_trend, Trends, "stable"),
        "        </span>",
        "    </div>",
        "    <div class='trend-item'>",
        "        <span class='trend-label'>Direction:</span>",
        "        <span class='trend-value'>", maps:get(direction, Trends, "stable"), "</span>",
        "    </div>",
        "    <div class='trend-item'>",
        "        <span class='trend-label'>Confidence:</span>",
        "        <span class='trend-value'>", format_percentage(maps:get(confidence, Trends, 0.0)), "</span>",
        "    </div>",
        "</div>"
    ].

%% @doc Create metrics panel
create_metrics_panel(Data) ->
    Metrics = maps:get(metrics, Data, #{}),
    
    [
        "<div class='panel metrics-panel'>",
        "    <h3>Current Metrics</h3>",
        "    <div class='metrics-grid'>",
        format_metric_card("Latency", maps:get(latency, Metrics, 0), "ms"),
        format_metric_card("Throughput", maps:get(throughput, Metrics, 0), "req/s"),
        format_metric_card("Error Rate", maps:get(error_rate, Metrics, 0), "%"),
        format_metric_card("CPU Usage", maps:get(cpu_usage, Metrics, 0), "%"),
        format_metric_card("Memory Usage", maps:get(memory_usage, Metrics, 0), "%"),
        "    </div>",
        "</div>"
    ].

%% @doc Format alert item
format_alert_item(Alert) ->
    [
        "<div class='alert-item ", maps:get(severity, Alert, "low"), "'>",
        "    <div class='alert-title'>", maps:get(title, Alert, "Unknown Alert"), "</div>",
        "    <div class='alert-details'>", maps:get(details, Alert, ""), "</div>",
        "    <div class='alert-time'>", format_timestamp(maps:get(timestamp, Alert, 0)), "</div>",
        "</div>"
    ].

%% @doc Format metric card
format_metric_card(Name, Value, Unit) ->
    [
        "<div class='metric-card'>",
        "    <div class='metric-name'>", Name, "</div>",
        "    <div class='metric-value'>", format_number(Value), " <span class='metric-unit'>", Unit, "</span></div>",
        "</div>"
    ].

%% @doc Get dashboard CSS
get_dashboard_css() ->
    "
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; margin: 0; background: #f5f5f5; }
    .dashboard-header { background: #2c3e50; color: white; padding: 1rem; display: flex; justify-content: space-between; align-items: center; }
    .dashboard-header h1 { margin: 0; }
    .status-indicator { padding: 0.5rem 1rem; border-radius: 4px; font-weight: bold; }
    .status-good { background: #27ae60; }
    .status-warning { background: #f39c12; }
    .status-critical { background: #e74c3c; }
    .dashboard-container { padding: 1rem; max-width: 1200px; margin: 0 auto; }
    .panel-row { display: flex; gap: 1rem; margin-bottom: 1rem; }
    .panel { background: white; border-radius: 8px; padding: 1.5rem; box-shadow: 0 2px 4px rgba(0,0,0,0.1); flex: 1; }
    .alert-panel { min-height: 200px; }
    .alert-list { max-height: 300px; overflow-y: auto; }
    .alert-item { padding: 0.75rem; margin-bottom: 0.5rem; border-radius: 4px; border-left: 4px solid; }
    .alert-item.critical { background: #fdf2f2; border-left-color: #e74c3c; }
    .alert-item.high { background: #fef9e7; border-left-color: #f39c12; }
    .alert-item.medium { background: #f0f9f4; border-left-color: #f39c12; }
    .alert-item.low { background: #f8f9fa; border-left-color: #6c757d; }
    .no-alerts { text-align: center; color: #27ae60; padding: 2rem; }
    .metrics-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 1rem; }
    .metric-card { text-align: center; padding: 1rem; background: #f8f9fa; border-radius: 6px; }
    .metric-name { font-size: 0.9rem; color: #6c757d; margin-bottom: 0.5rem; }
    .metric-value { font-size: 1.5rem; font-weight: bold; color: #2c3e50; }
    .metric-unit { font-size: 0.8rem; color: #6c757d; }
    .trend-item { display: flex; justify-content: space-between; padding: 0.5rem 0; border-bottom: 1px solid #eee; }
    .trend-item:last-child { border-bottom: none; }
    .trend-label { color: #6c757d; }
    .trend-value { font-weight: bold; }
    .trend-value.improving { color: #27ae60; }
    .trend-value.degrading { color: #e74c3c; }
    .trend-value.stable { color: #6c757d; }
    .chart-panel { width: 100%; }
    #metrics-chart { width: 100%; height: 400px; }
    ".

%% @doc Get dashboard JavaScript
get_dashboard_js() ->
    "
    // Auto-refresh dashboard
    setInterval(function() {
        location.reload();
    }, 30000); // Refresh every 30 seconds
    
    // Add real-time updates via WebSocket (if available)
    if (typeof WebSocket !== 'undefined') {
        var ws = new WebSocket('ws://localhost:' + window.location.port + '/ws');
        ws.onmessage = function(event) {
            var data = JSON.parse(event.data);
            updateDashboard(data);
        };
    }
    
    function updateDashboard(data) {
        // Update metrics in real-time
        if (data.metrics) {
            updateMetricsDisplay(data.metrics);
        }
        
        // Update charts
        if (data.chartData) {
            Plotly.redraw('metrics-chart');
        }
    }
    
    function updateMetricsDisplay(metrics) {
        // Update metric cards with new values
        $('.metric-card').each(function() {
            var metricName = $(this).find('.metric-name').text().toLowerCase();
            if (metrics[metricName]) {
                $(this).find('.metric-value').text(metrics[metricName]);
            }
        });
    }
    ".

%% Helper functions

%% @doc Store metrics in history
store_metrics_history(Data, Timestamp) ->
    % Clean old data beyond retention period
    Cutoff = Timestamp - ?HISTORY_RETENTION,
    ets:select_delete(dashboard_metrics, [{{{'$1', '_'}, [], [{'<', '$1', Cutoff}]}}]),
    
    % Store new data
    ets:insert(dashboard_metrics, {{Timestamp, Data}}).

%% @doc Update alerts
update_alerts(Regressions, Timestamp) ->
    lists:foreach(fun(Regression) ->
        AlertId = {Timestamp, Regression},
        Alert = #{
            id => AlertId,
            timestamp => Timestamp,
            title => format_regression_title(Regression),
            details => format_regression_details(Regression),
            severity => get_regression_severity(Regression),
            active => true
        },
        ets:insert(dashboard_alerts, {AlertId, Alert})
    end, Regressions).

%% @doc Broadcast update to connected clients
broadcast_update(Data) ->
    % Placeholder for WebSocket broadcast
    ok.

%% @doc Get current metrics (mock implementation)
get_current_metrics() ->
    #{
        latency => 45.2,
        throughput => 1250.0,
        error_rate => 0.02,
        cpu_usage => 68.5,
        memory_usage => 78.3
    }.

get_recent_alerts() -> [].
get_performance_trends() -> #{overall_trend => "stable", direction => "stable", confidence => 0.85}.
get_baseline_status() -> #{}.
get_system_status() -> healthy.

%% @doc Get current latency
get_current_latency() -> 45.2.

%% @doc Get current throughput  
get_current_throughput() -> 1250.0.

%% @doc Get current error rate
get_current_error_rate() -> 0.02.

%% @doc Get current CPU usage
get_current_cpu() -> 68.5.

%% @doc Get current memory usage
get_current_memory() -> 78.3.

%% @doc Get active connections count
get_active_connections() -> 42.

%% @doc Start web server for dashboard
start_web_server(_Port) ->
    % Placeholder for web server startup
    ok.

%% @doc Handle dashboard update
handle_update(Data, State) ->
    NewHistory = [Data | State#dashboard_state.metrics_history],
    State#dashboard_state{
        last_update = erlang:system_time(millisecond),
        metrics_history = lists:sublist(NewHistory, 100) % Keep last 100 updates
    }.

%% @doc Get dashboard state
get_dashboard_state(State) ->
    #{
        last_update => State#dashboard_state.last_update,
        metrics_count => length(State#dashboard_state.metrics_history),
        alerts_count => length(State#dashboard_state.active_alerts)
    }.

%% @doc Format CSV export
format_csv(_Data) ->
    "timestamp,latency,throughput,error_rate,cpu_usage,memory_usage\n".

%% @doc Get overall status
get_overall_status(_Data) -> "HEALTHY".

%% @doc Get overall status CSS class
get_overall_status_class(_Data) -> "status-good".

%% @doc Get trend CSS class
get_trend_class(_Trends) -> "stable".

%% @doc Format timestamp
format_timestamp(Timestamp) ->
    DateTime = calendar:system_time_to_universal_time(Timestamp, millisecond),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", 
        tuple_to_list(DateTime)).

%% @doc Format percentage
format_percentage(Value) ->
    io_lib:format("~.1f%", [Value * 100]).

%% @doc Format number
format_number(Value) when is_float(Value) ->
    io_lib:format("~.1f", [Value]);
format_number(Value) when is_integer(Value) ->
    integer_to_list(Value).

%% @doc Format regression title
format_regression_title(Regression) ->
    io_lib:format("~p Regression Detected", [maps:get(metric_name, Regression, unknown)]).

%% @doc Format regression details  
format_regression_details(Regression) ->
    io_lib:format("Change: ~.1f%", [maps:get(change_percent, Regression, 0.0)]).

%% @doc Get regression severity
get_regression_severity(Regression) ->
    maps:get(severity, Regression, low).

%% @doc WebSocket handler
websocket_handler(init, _Req) ->
    {ok, undefined};
websocket_handler({text, Msg}, State) ->
    ?LOG_INFO("WebSocket message: ~p", [Msg]),
    {ok, State};
websocket_handler(_Frame, State) ->
    {ok, State}.