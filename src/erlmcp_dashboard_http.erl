-module(erlmcp_dashboard_http).

%% Cowboy HTTP handler for dashboard HTML
-export([init/2]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Cowboy HTTP Handler
%%====================================================================

init(Req, Opts) ->
    Path = cowboy_req:path(Req),
    case Path of
        <<"/">> ->
            serve_dashboard(Req, Opts);
        <<"/metrics/dashboard">> ->
            serve_dashboard(Req, Opts);
        _ ->
            Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>}, <<"Not Found">>, Req),
            {ok, Req2, Opts}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec serve_dashboard(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
serve_dashboard(Req, Opts) ->
    Html = dashboard_html(),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html; charset=utf-8">>}, Html, Req),
    {ok, Req2, Opts}.

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
            * {
                margin: 0;
                padding: 0;
                box-sizing: border-box;
            }

            body {
                font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
                background: linear-gradient(135deg, #1e1e2e 0%, #2a2a3e 100%);
                color: #e0e0e0;
                min-height: 100vh;
                padding: 20px;
            }

            .container {
                max-width: 1600px;
                margin: 0 auto;
            }

            .header {
                text-align: center;
                margin-bottom: 40px;
                padding: 20px;
                background: rgba(255, 255, 255, 0.05);
                border-radius: 8px;
                border-left: 4px solid #00d4ff;
            }

            h1 {
                font-size: 2.5em;
                margin-bottom: 10px;
                color: #00d4ff;
            }

            .status {
                font-size: 1.1em;
                color: #4caf50;
                font-weight: bold;
            }

            .status.error {
                color: #f44336;
            }

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
                backdrop-filter: blur(10px);
                transition: all 0.3s ease;
            }

            .metric-card:hover {
                border-color: rgba(0, 212, 255, 0.3);
                background: rgba(255, 255, 255, 0.12);
                transform: translateY(-2px);
            }

            .metric-label {
                font-size: 0.9em;
                color: #a0a0a0;
                text-transform: uppercase;
                letter-spacing: 1px;
                margin-bottom: 10px;
            }

            .metric-value {
                font-size: 2.2em;
                font-weight: bold;
                color: #00d4ff;
                font-family: 'Monaco', 'Menlo', monospace;
            }

            .metric-value.critical {
                color: #f44336;
            }

            .metric-value.warning {
                color: #ff9800;
            }

            .metric-value.success {
                color: #4caf50;
            }

            .metric-unit {
                font-size: 0.7em;
                color: #707070;
                margin-left: 5px;
            }

            .mini-stat {
                font-size: 0.9em;
                color: #808080;
                margin-top: 10px;
            }

            .section {
                margin-bottom: 30px;
            }

            .section-title {
                font-size: 1.3em;
                color: #00d4ff;
                margin-bottom: 15px;
                padding-left: 10px;
                border-left: 3px solid #00d4ff;
            }

            .latency-section {
                background: rgba(255, 255, 255, 0.08);
                border: 1px solid rgba(255, 255, 255, 0.12);
                border-radius: 8px;
                padding: 20px;
                margin-bottom: 20px;
            }

            .latency-grid {
                display: grid;
                grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
                gap: 15px;
                margin-top: 15px;
            }

            .latency-item {
                background: rgba(0, 212, 255, 0.1);
                border: 1px solid rgba(0, 212, 255, 0.3);
                border-radius: 6px;
                padding: 12px;
                text-align: center;
            }

            .latency-label {
                font-size: 0.85em;
                color: #a0a0a0;
                margin-bottom: 5px;
            }

            .latency-value {
                font-size: 1.8em;
                font-weight: bold;
                color: #00d4ff;
                font-family: 'Monaco', monospace;
            }

            .nodes-grid {
                display: grid;
                grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
                gap: 15px;
            }

            .node-card {
                background: rgba(255, 255, 255, 0.08);
                border: 1px solid rgba(255, 255, 255, 0.12);
                border-radius: 6px;
                padding: 15px;
            }

            .node-name {
                font-size: 1.1em;
                font-weight: bold;
                color: #00d4ff;
                margin-bottom: 10px;
                font-family: 'Monaco', monospace;
            }

            .node-stat {
                display: flex;
                justify-content: space-between;
                padding: 5px 0;
                font-size: 0.9em;
                border-bottom: 1px solid rgba(255, 255, 255, 0.08);
            }

            .node-stat:last-child {
                border-bottom: none;
            }

            .node-stat-label {
                color: #a0a0a0;
            }

            .node-stat-value {
                color: #00d4ff;
                font-family: 'Monaco', monospace;
                font-weight: bold;
            }

            .update-indicator {
                position: fixed;
                top: 20px;
                right: 20px;
                padding: 8px 15px;
                background: rgba(76, 175, 80, 0.2);
                border: 1px solid #4caf50;
                border-radius: 4px;
                font-size: 0.85em;
                color: #4caf50;
            }

            .update-indicator.updating {
                background: rgba(255, 193, 7, 0.2);
                border-color: #ffc107;
                color: #ffc107;
            }

            .update-indicator.error {
                background: rgba(244, 67, 54, 0.2);
                border-color: #f44336;
                color: #f44336;
            }

            .chart-placeholder {
                background: rgba(255, 255, 255, 0.04);
                border: 1px dashed rgba(255, 255, 255, 0.2);
                border-radius: 6px;
                padding: 40px;
                text-align: center;
                color: #707070;
                margin-top: 15px;
            }

            .progress-bar {
                height: 8px;
                background: rgba(255, 255, 255, 0.1);
                border-radius: 4px;
                overflow: hidden;
                margin-top: 10px;
            }

            .progress-fill {
                height: 100%;
                background: linear-gradient(90deg, #00d4ff, #00a8ff);
                transition: width 0.3s ease;
            }

            @keyframes pulse {
                0%, 100% { opacity: 1; }
                50% { opacity: 0.5; }
            }

            .live-indicator {
                display: inline-block;
                width: 8px;
                height: 8px;
                background: #4caf50;
                border-radius: 50%;
                margin-right: 8px;
                animation: pulse 1s infinite;
            }

            .error-percentage {
                display: flex;
                align-items: center;
                justify-content: space-between;
                margin-top: 8px;
            }

            .memory-usage {
                display: flex;
                gap: 15px;
                margin-top: 10px;
            }

            .memory-item {
                flex: 1;
            }

            .memory-label {
                font-size: 0.8em;
                color: #a0a0a0;
                margin-bottom: 4px;
            }

            .memory-value {
                font-size: 1.2em;
                font-weight: bold;
                color: #00d4ff;
                font-family: 'Monaco', monospace;
            }
        </style>
    </head>
    <body>
        <div class=\"update-indicator\" id=\"updateIndicator\">
            <span class=\"live-indicator\"></span>Live
        </div>

        <div class=\"container\">
            <div class=\"header\">
                <h1>🚀 ErlMCP Monitoring Dashboard</h1>
                <p class=\"status\" id=\"systemStatus\">Initializing...</p>
            </div>

            <!-- Main Metrics Grid -->
            <div class=\"grid\">
                <div class=\"metric-card\">
                    <div class=\"metric-label\">Concurrent Connections</div>
                    <div class=\"metric-value\" id=\"concurrentConnections\">0</div>
                    <div class=\"mini-stat\" id=\"connectionTrend\"></div>
                </div>

                <div class=\"metric-card\">
                    <div class=\"metric-label\">Messages per Second</div>
                    <div class=\"metric-value\" id=\"messageRate\">0</div>
                    <div class=\"mini-stat\" id=\"totalMessages\">Total: 0</div>
                </div>

                <div class=\"metric-card\">
                    <div class=\"metric-label\">Error Rate</div>
                    <div class=\"metric-value\" id=\"errorRate\">0</div>
                    <div class=\"error-percentage\">
                        <span id=\"errorPercentage\">0% of total messages</span>
                        <div class=\"progress-bar\">
                            <div class=\"progress-fill\" id=\"errorProgressBar\" style=\"width: 0%\"></div>
                        </div>
                    </div>
                </div>

                <div class=\"metric-card\">
                    <div class=\"metric-label\">System Uptime</div>
                    <div class=\"metric-value\" id=\"uptime\" style=\"font-size: 1.3em;\">0d 0h 0m</div>
                    <div class=\"mini-stat\" id=\"lastUpdate\"></div>
                </div>

                <div class=\"metric-card\">
                    <div class=\"metric-label\">P50 Latency</div>
                    <div class=\"metric-value\" id=\"p50Latency\">0</div>
                    <div class=\"metric-unit\">ms</div>
                </div>

                <div class=\"metric-card\">
                    <div class=\"metric-label\">P99 Latency</div>
                    <div class=\"metric-value\" id=\"p99Latency\">0</div>
                    <div class=\"metric-unit\">ms</div>
                </div>

                <div class=\"metric-card\">
                    <div class=\"metric-label\">Active Processes</div>
                    <div class=\"metric-value\" id=\"processCount\">0</div>
                    <div class=\"mini-stat\" id=\"schedulerCount\"></div>
                </div>

                <div class=\"metric-card\">
                    <div class=\"metric-label\">Memory Usage</div>
                    <div class=\"metric-value\" id=\"memoryUsage\" style=\"font-size: 1.5em;\">0 MB</div>
                    <div class=\"mini-stat\" id=\"memoryPercent\"></div>
                </div>
            </div>

            <!-- Latency Statistics -->
            <div class=\"section\">
                <div class=\"section-title\">Latency Distribution (Last 1K Samples)</div>
                <div class=\"latency-section\">
                    <div class=\"latency-grid\">
                        <div class=\"latency-item\">
                            <div class=\"latency-label\">Min</div>
                            <div class=\"latency-value\" id=\"latencyMin\">0</div>
                        </div>
                        <div class=\"latency-item\">
                            <div class=\"latency-label\">P50</div>
                            <div class=\"latency-value\" id=\"latencyP50\">0</div>
                        </div>
                        <div class=\"latency-item\">
                            <div class=\"latency-label\">P95</div>
                            <div class=\"latency-value\" id=\"latencyP95\">0</div>
                        </div>
                        <div class=\"latency-item\">
                            <div class=\"latency-label\">P99</div>
                            <div class=\"latency-value\" id=\"latencyP99\">0</div>
                        </div>
                        <div class=\"latency-item\">
                            <div class=\"latency-label\">Avg</div>
                            <div class=\"latency-value\" id=\"latencyAvg\">0</div>
                        </div>
                        <div class=\"latency-item\">
                            <div class=\"latency-label\">Max</div>
                            <div class=\"latency-value\" id=\"latencyMax\">0</div>
                        </div>
                    </div>
                </div>
            </div>

            <!-- Node Metrics -->
            <div class=\"section\">
                <div class=\"section-title\">Node Status</div>
                <div class=\"nodes-grid\" id=\"nodesGrid\">
                    <div class=\"node-card\">
                        <div class=\"node-name\">Loading...</div>
                    </div>
                </div>
            </div>

            <!-- Footer -->
            <div class=\"section\" style=\"text-align: center; color: #707070; font-size: 0.9em; margin-top: 40px;\">
                <p>Dashboard updates every 1 second | Data is live from erlmcp metrics server</p>
                <p id=\"fetchStatus\"></p>
            </div>
        </div>

        <script>
            const API_ENDPOINT = '/metrics';
            const UPDATE_INTERVAL = 1000; // 1 second
            let updateTimer = null;
            let previousMetrics = null;

            // Format bytes to MB/GB
            function formatBytes(bytes) {
                if (bytes === 0) return '0 B';
                const k = 1024;
                const sizes = ['B', 'KB', 'MB', 'GB'];
                const i = Math.floor(Math.log(bytes) / Math.log(k));
                return Math.round(bytes / Math.pow(k, i) * 100) / 100 + ' ' + sizes[i];
            }

            // Format large numbers with commas
            function formatNumber(num) {
                return Math.round(num).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
            }

            // Update dashboard with metrics
            function updateDashboard(metrics) {
                try {
                    // Update main metrics
                    document.getElementById('concurrentConnections').textContent = formatNumber(metrics.concurrent_connections);
                    document.getElementById('messageRate').textContent = formatNumber(metrics.message_rate_per_sec);
                    document.getElementById('totalMessages').textContent = 'Total: ' + formatNumber(metrics.total_messages);
                    document.getElementById('errorRate').textContent = formatNumber(metrics.error_rate_per_sec);
                    document.getElementById('uptime').textContent = metrics.uptime_human.toString();

                    // Error percentage
                    const errorPct = Math.round(metrics.error_percentage * 100 * 100) / 100;
                    document.getElementById('errorPercentage').textContent = errorPct.toFixed(2) + '% of total messages';
                    document.getElementById('errorProgressBar').style.width = Math.min(100, errorPct) + '%';

                    // Latency stats
                    const latencyStats = metrics.latency_stats || {};
                    document.getElementById('p50Latency').textContent = latencyStats.p50 || 0;
                    document.getElementById('p99Latency').textContent = latencyStats.p99 || 0;
                    document.getElementById('latencyMin').textContent = latencyStats.min || 0;
                    document.getElementById('latencyP50').textContent = latencyStats.p50 || 0;
                    document.getElementById('latencyP95').textContent = latencyStats.p95 || 0;
                    document.getElementById('latencyP99').textContent = latencyStats.p99 || 0;
                    document.getElementById('latencyAvg').textContent = latencyStats.avg || 0;
                    document.getElementById('latencyMax').textContent = latencyStats.max || 0;

                    // System metrics
                    const systemMetrics = metrics.system_metrics || {};
                    document.getElementById('processCount').textContent = formatNumber(systemMetrics.process_count || 0);
                    const schedulers = systemMetrics.schedulers || 1;
                    const schedulersOnline = systemMetrics.schedulers_online || 1;
                    document.getElementById('schedulerCount').textContent = 'Schedulers: ' + schedulersOnline + '/' + schedulers;

                    // Memory
                    const memoryMap = systemMetrics.memory || {};
                    const totalMemory = memoryMap.total || 0;
                    document.getElementById('memoryUsage').textContent = formatBytes(totalMemory);
                    const heapMemory = memoryMap.heap_used || 0;
                    const heapPct = totalMemory > 0 ? Math.round(heapMemory / totalMemory * 100) : 0;
                    document.getElementById('memoryPercent').textContent = heapPct + '% heap used';

                    // Update node status
                    updateNodeStatus(metrics.nodes || []);

                    // Update last update time
                    const now = new Date();
                    const timeStr = now.toLocaleTimeString();
                    document.getElementById('lastUpdate').textContent = 'Updated: ' + timeStr;

                    // Update status indicator
                    updateStatusIndicator(metrics);

                    previousMetrics = metrics;
                } catch (error) {
                    console.error('Error updating dashboard:', error);
                    setErrorStatus('Dashboard update error: ' + error.message);
                }
            }

            function updateNodeStatus(nodes) {
                const grid = document.getElementById('nodesGrid');
                grid.innerHTML = '';

                if (!nodes || nodes.length === 0) {
                    grid.innerHTML = '<div class=\"node-card\"><div class=\"node-name\">No nodes</div></div>';
                    return;
                }

                nodes.forEach(node => {
                    const card = document.createElement('div');
                    card.className = 'node-card';

                    const nodeName = document.createElement('div');
                    nodeName.className = 'node-name';
                    nodeName.textContent = node.node || 'unknown';

                    let statsHtml = '';

                    if (node.status === 'unreachable') {
                        statsHtml = '<div class=\"node-stat\"><span class=\"node-stat-label\">Status</span><span class=\"node-stat-value\" style=\"color: #f44336;\">Unreachable</span></div>';
                    } else if (node.status === 'error') {
                        statsHtml = '<div class=\"node-stat\"><span class=\"node-stat-label\">Status</span><span class=\"node-stat-value\" style=\"color: #f44336;\">Error</span></div>';
                    } else {
                        const processCount = node.process_count || 0;
                        const memory = node.memory || {};
                        const totalMem = memory.total || 0;

                        statsHtml += '<div class=\"node-stat\"><span class=\"node-stat-label\">Processes</span><span class=\"node-stat-value\">' + formatNumber(processCount) + '</span></div>';
                        statsHtml += '<div class=\"node-stat\"><span class=\"node-stat-label\">Memory</span><span class=\"node-stat-value\">' + formatBytes(totalMem) + '</span></div>';
                    }

                    card.appendChild(nodeName);
                    card.innerHTML += statsHtml;
                    grid.appendChild(card);
                });
            }

            function updateStatusIndicator(metrics) {
                const indicator = document.getElementById('updateIndicator');
                const status = document.getElementById('systemStatus');

                let statusText = '✓ System Healthy';
                let statusClass = '';

                // Check for error rate
                if (metrics.error_percentage > 0.05) {
                    statusText = '⚠ High Error Rate (' + Math.round(metrics.error_percentage * 100 * 100) / 100 + '%)';
                    statusClass = 'error';
                }

                // Check concurrent connections
                if (metrics.concurrent_connections > 100000) {
                    statusText = '✓ 100K+ Concurrent Connections Active';
                    statusClass = '';
                } else if (metrics.concurrent_connections > 50000) {
                    statusText = '✓ 50K+ Concurrent Connections Active';
                    statusClass = '';
                } else if (metrics.concurrent_connections > 10000) {
                    statusText = '✓ 10K+ Concurrent Connections Active';
                    statusClass = '';
                }

                status.textContent = statusText;
                status.className = 'status ' + statusClass;
            }

            function setErrorStatus(message) {
                const indicator = document.getElementById('updateIndicator');
                indicator.textContent = '✕ ' + message;
                indicator.className = 'update-indicator error';
            }

            function setUpdatingStatus() {
                const indicator = document.getElementById('updateIndicator');
                indicator.innerHTML = '<span class=\"live-indicator\"></span>Updating...';
                indicator.className = 'update-indicator updating';
            }

            function setLiveStatus() {
                const indicator = document.getElementById('updateIndicator');
                indicator.innerHTML = '<span class=\"live-indicator\"></span>Live';
                indicator.className = 'update-indicator';
            }

            // Fetch metrics from server
            async function fetchMetrics() {
                setUpdatingStatus();
                try {
                    const response = await fetch(API_ENDPOINT);
                    if (!response.ok) {
                        throw new Error('HTTP ' + response.status);
                    }
                    const metrics = await response.json();
                    updateDashboard(metrics);
                    setLiveStatus();
                    document.getElementById('fetchStatus').textContent = 'Last successful fetch: ' + new Date().toLocaleTimeString();
                } catch (error) {
                    console.error('Error fetching metrics:', error);
                    setErrorStatus('Connection error: ' + error.message);
                    document.getElementById('fetchStatus').textContent = 'Error: ' + error.message;
                }
            }

            // Start periodic updates
            function startUpdates() {
                fetchMetrics(); // Initial fetch
                updateTimer = setInterval(fetchMetrics, UPDATE_INTERVAL);
            }

            // Stop updates
            function stopUpdates() {
                if (updateTimer) {
                    clearInterval(updateTimer);
                    updateTimer = null;
                }
            }

            // Page visibility handling
            document.addEventListener('visibilitychange', () => {
                if (document.hidden) {
                    stopUpdates();
                } else {
                    startUpdates();
                }
            });

            // Initialize on page load
            window.addEventListener('load', startUpdates);

            // Cleanup on page unload
            window.addEventListener('beforeunload', stopUpdates);
        </script>
    </body>
    </html>
    ">>.
