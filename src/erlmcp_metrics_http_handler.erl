-module(erlmcp_metrics_http_handler).

%% Inets httpd callback module
-export([do/1]).

-include_lib("inets/include/httpd.hrl").

%%====================================================================
%% HTTP Handler for Metrics API
%%====================================================================

-spec do(#mod{}) -> {proceed, list()} | {ok, #mod{}}.
do(ModData) ->
    Method = ModData#mod.method,
    Path = ModData#mod.request_uri,

    case {Method, Path} of
        {"GET", "/metrics"} ->
            handle_metrics_api();
        {"GET", "/"} ->
            handle_dashboard();
        {"GET", "/metrics/dashboard"} ->
            handle_dashboard();
        {_, _} ->
            {proceed, [{response, {404, "Not Found"}}]}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec handle_metrics_api() -> {proceed, list()}.
handle_metrics_api() ->
    try
        Metrics = erlmcp_metrics_server:get_metrics(),
        Json = jsx:encode(metrics_to_json(Metrics)),
        Response = {response, {200, [
            {content_type, "application/json"},
            {cache_control, "no-cache, no-store"},
            {access_control_allow_origin, "*"}
        ], Json}},
        {proceed, [Response]}
    catch
        _:_ ->
            ErrorJson = jsx:encode(#{
                error => <<"server_error">>,
                message => <<"Internal server error">>
            }),
            ErrorResponse = {response, {500, [
                {content_type, "application/json"}
            ], ErrorJson}},
            {proceed, [ErrorResponse]}
    end.

-spec handle_dashboard() -> {proceed, list()}.
handle_dashboard() ->
    Html = dashboard_html(),
    Response = {response, {200, [
        {content_type, "text/html; charset=utf-8"},
        {cache_control, "no-cache, no-store"}
    ], Html}},
    {proceed, [Response]}.

-spec metrics_to_json(map()) -> map().
metrics_to_json(Metrics) ->
    maps:map(fun encode_metric_value/2, Metrics).

-spec encode_metric_value(atom() | binary(), term()) -> term().
encode_metric_value(uptime_human, Value) when is_binary(Value) ->
    Value;
encode_metric_value(latency_stats, LatencyStats) when is_map(LatencyStats) ->
    LatencyStats;
encode_metric_value(nodes, Nodes) when is_list(Nodes) ->
    Nodes;
encode_metric_value(system_metrics, Metrics) when is_map(Metrics) ->
    maps:map(fun encode_metric_value/2, Metrics);
encode_metric_value(memory, Memory) when is_map(Memory) ->
    Memory;
encode_metric_value(_Key, Value) ->
    Value.

-spec dashboard_html() -> iolist().
dashboard_html() ->
    <<"
<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>ErlMCP Monitoring Dashboard</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            background: linear-gradient(135deg, #1e1e2e 0%, #2a2a3e 100%);
            color: #e0e0e0;
            min-height: 100vh;
            padding: 20px;
        }
        .container { max-width: 1600px; margin: 0 auto; }
        .header {
            text-align: center;
            margin-bottom: 40px;
            padding: 20px;
            background: rgba(255, 255, 255, 0.05);
            border-radius: 8px;
            border-left: 4px solid #00d4ff;
        }
        h1 { font-size: 2.5em; margin-bottom: 10px; color: #00d4ff; }
        .status { font-size: 1.1em; color: #4caf50; font-weight: bold; }
        .grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }
        .metric-card {
            background: rgba(255, 255, 255, 0.08);
            border: 1px solid rgba(255, 255, 255, 0.12);
            border-radius: 8px;
            padding: 20px;
            transition: all 0.3s ease;
        }
        .metric-label { font-size: 0.9em; color: #a0a0a0; text-transform: uppercase; letter-spacing: 1px; margin-bottom: 10px; }
        .metric-value { font-size: 2.2em; font-weight: bold; color: #00d4ff; font-family: 'Monaco', monospace; }
        .metric-unit { font-size: 0.7em; color: #707070; margin-left: 5px; }
        .mini-stat { font-size: 0.9em; color: #808080; margin-top: 10px; }
    </style>
</head>
<body>
    <div class=\"container\">
        <div class=\"header\">
            <h1>ErlMCP Real-Time Monitoring</h1>
            <p class=\"status\" id=\"systemStatus\">Connecting...</p>
        </div>
        <div class=\"grid\">
            <div class=\"metric-card\">
                <div class=\"metric-label\">Concurrent Connections</div>
                <div class=\"metric-value\" id=\"concurrentConnections\">0</div>
            </div>
            <div class=\"metric-card\">
                <div class=\"metric-label\">Messages per Second</div>
                <div class=\"metric-value\" id=\"messageRate\">0</div>
            </div>
            <div class=\"metric-card\">
                <div class=\"metric-label\">P50 Latency (ms)</div>
                <div class=\"metric-value\" id=\"p50Latency\">0</div>
            </div>
            <div class=\"metric-card\">
                <div class=\"metric-label\">P99 Latency (ms)</div>
                <div class=\"metric-value\" id=\"p99Latency\">0</div>
            </div>
        </div>
    </div>
    <script>
        const API_ENDPOINT = '/metrics';
        const UPDATE_INTERVAL = 1000;

        function formatNumber(n) {
            return Math.round(n).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
        }

        async function updateDashboard() {
            try {
                const resp = await fetch(API_ENDPOINT);
                if (!resp.ok) throw new Error('HTTP ' + resp.status);
                const metrics = await resp.json();

                document.getElementById('concurrentConnections').textContent =
                    formatNumber(metrics.concurrent_connections);
                document.getElementById('messageRate').textContent =
                    formatNumber(metrics.message_rate_per_sec);

                const ls = metrics.latency_stats || {};
                document.getElementById('p50Latency').textContent = ls.p50 || 0;
                document.getElementById('p99Latency').textContent = ls.p99 || 0;

                document.getElementById('systemStatus').textContent =
                    '✓ Live - ' + new Date().toLocaleTimeString();
            } catch (e) {
                document.getElementById('systemStatus').textContent = '✗ Connection error';
            }
        }

        setInterval(updateDashboard, UPDATE_INTERVAL);
        updateDashboard();
    </script>
</body>
</html>
    ">>.
