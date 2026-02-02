%%%-------------------------------------------------------------------
%%% @doc
%%% Health Dashboard Server for Transport Monitoring
%%%
%%% Provides HTTP endpoints and WebSocket streaming for real-time
%%% transport health monitoring with visual dashboard.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_health_dashboard).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, stop/0, get_port/0, subscribe/0, unsubscribe/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% HTTP handler callbacks (Cowboy)
-export([init/2, handle/2, terminate/3]).
%% WebSocket handler
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_PORT, 9091).
-define(HEALTH_UPDATE_INTERVAL, 5000). % 5 seconds
-define(MAX_WEBSOCKET_CLIENTS, 100).

-record(state,
        {port :: inet:port_number(),
         listener_name :: atom(),
         listener_pid :: pid() | undefined,
         websocket_clients = [] :: [pid()],
         health_timer :: reference() | undefined}).

-record(ws_state,
        {client_id :: binary(),
         subscribed = true :: boolean()}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the health dashboard with default port
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    Port = application:get_env(erlmcp_observability, health_dashboard_port, ?DEFAULT_PORT),
    start_link(Port).

%% @doc Start the health dashboard with specified port
-spec start_link(inet:port_number()) -> {ok, pid()} | {error, term()}.
start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%% @doc Stop the health dashboard
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Get the current port
-spec get_port() -> {ok, inet:port_number()} | {error, not_started}.
get_port() ->
    gen_server:call(?MODULE, get_port).

%% @doc Subscribe to health updates (via WebSocket)
-spec subscribe() -> ok.
subscribe() ->
    gen_server:cast(?MODULE, {subscribe, self()}).

%% @doc Unsubscribe from health updates
-spec unsubscribe() ->
    gen_server:cast(?MODULE, {unsubscribe, self()}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([inet:port_number()]) -> {ok, #state{}, {continue, start_http}}.
init([Port]) ->
    ?LOG_INFO("Starting health dashboard on port ~p (async)", [Port]),
    State = #state{port = Port, listener_name = listener_name(Port)},
    {ok, State, {continue, start_http}}.

handle_call(get_port, _From, State) ->
    {reply, {ok, State#state.port}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({subscribe, Pid}, State) ->
    erlang:monitor(process, Pid),
    ?LOG_INFO("WebSocket client subscribed: ~p", [Pid]),
    {noreply, State#state{websocket_clients = [Pid | State#state.websocket_clients]}};
handle_cast({unsubscribe, Pid}, State) ->
    ?LOG_INFO("WebSocket client unsubscribed: ~p", [Pid]),
    {noreply, State#state{websocket_clients = lists:delete(Pid, State#state.websocket_clients)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({health_update, HealthData}, State) ->
    % Broadcast health update to all WebSocket clients
    Message = jsx:encode(#{type => <<"health_update">>, data => HealthData}),
    lists:foreach(fun(WsPid) -> WsPid ! {send, Message} end, State#state.websocket_clients),
    {noreply, State};
handle_info(broadcast_health, State) ->
    % Fetch and broadcast current health status
    HealthData = fetch_health_data(),
    lists:foreach(fun(WsPid) -> WsPid ! {send, HealthData} end, State#state.websocket_clients),
    % Reschedule next broadcast
    Timer = erlang:send_after(?HEALTH_UPDATE_INTERVAL, self(), broadcast_health),
    {noreply, State#state{health_timer = Timer}};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % WebSocket client died
    ?LOG_INFO("WebSocket client disconnected: ~p", [Pid]),
    {noreply, State#state{websocket_clients = lists:delete(Pid, State#state.websocket_clients)}};
handle_info(_Info, State) ->
    {noreply, State}.

handle_continue(start_http, State) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", ?MODULE, []},  % Overall health JSON
            {"/health/:transport", ?MODULE, []},  % Per-transport health
            {"/health/stream", ?MODULE, []},  % WebSocket streaming
            {"/health/metrics", ?MODULE, []},  % Prometheus metrics export
            {"/health/dashboard", ?MODULE, []}  % HTML dashboard
        ]}
    ]),

    % Start Cowboy listener
    ListenerPid = case cowboy:start_clear(
        State#state.listener_name,
        [{port, State#state.port}, {max_connections, ?MAX_WEBSOCKET_CLIENTS}],
        #{env => #{dispatch => Dispatch}}
    ) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end,

    % Start periodic health broadcast
    {ok, Timer} = timer:send_interval(?HEALTH_UPDATE_INTERVAL, self(), broadcast_health),

    ?LOG_INFO("Health dashboard started on http://localhost:~p", [State#state.port]),
    {noreply, State#state{listener_pid = ListenerPid, health_timer = Timer}}.

terminate(_Reason, State) ->
    cowboy:stop_listener(State#state.listener_name),
    case State#state.health_timer of
        undefined -> ok;
        Timer -> timer:cancel(Timer)
    end,
    ?LOG_INFO("Health dashboard stopped"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Cowboy HTTP Handler Callbacks
%%%===================================================================

init(Req, _Opts) ->
    {cowboy_rest, Req, #{}}.

handle(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    case {Method, Path} of
        {<<"GET">>, <<"/health">>} ->
            handle_overall_health(Req, State);
        {<<"GET">>, <<"/health/", Transport/binary>>} ->
            handle_transport_health(Req, Transport, State);
        {<<"GET">>, <<"/health/stream">>} ->
            handle_websocket_upgrade(Req, State);
        {<<"GET">>, <<"/health/metrics">>} ->
            handle_prometheus_metrics(Req, State);
        {<<"GET">>, <<"/health/dashboard">>} ->
            handle_dashboard_html(Req, State);
        _ ->
            Req1 = cowboy_req:reply(404, #{}, <<"Not found">>, Req),
            {ok, Req1, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%%===================================================================
%%% WebSocket Handler
%%%===================================================================

websocket_init(State) ->
    ClientId = generate_client_id(),
    ?LOG_INFO("WebSocket health client connected: ~s", [ClientId]),
    % Subscribe to health updates
    gen_server:cast(?MODULE, {subscribe, self()}),
    % Send initial health data
    InitialHealth = fetch_health_data(),
    {ok, InitialHealth, #ws_state{client_id = ClientId}}.

websocket_handle({text, <<>>}, State) ->
    % Ignore empty messages
    {ok, State};
websocket_handle({text, Msg}, State) ->
    try
        Decoded = jsx:decode(Msg, [return_maps]),
        handle_ws_message(Decoded, State)
    catch
        _:_ ->
            Error = jsx:encode(#{type => <<"error">>, message => <<"Invalid JSON">>}),
            {[{text, Error}], State}
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({send, Data}, State) ->
    {[{text, Data}], State};
websocket_info(_Info, State) ->
    {ok, State}.

%%%===================================================================
%%% HTTP Handlers
%%%===================================================================

handle_overall_health(Req, State) ->
    try
        OverallHealth = erlmcp_transport_health:overall_health(),
        Body = jsx:encode(OverallHealth),
        Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req),
        {ok, Req1, State}
    catch
        _:_ ->
            Req1 = cowboy_req:reply(500, #{}, <<"Internal error">>, Req),
            {ok, Req1, State}
    end.

handle_transport_health(Req, Transport, State) ->
    try
        TransportAtom = binary_to_existing_atom(Transport, utf8),
        {ok, Health} = erlmcp_transport_health:get_health_status(TransportAtom),
        Body = jsx:encode(Health),
        Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req),
        {ok, Req1, State}
    catch
        error:badarg ->
            Req1 = cowboy_req:reply(404, #{}, <<"Transport not found">>, Req),
            {ok, Req1, State};
        _:_ ->
            Req1 = cowboy_req:reply(500, #{}, <<"Internal error">>, Req),
            {ok, Req1, State}
    end.

handle_websocket_upgrade(Req, State) ->
    % Upgrade to WebSocket
    {cowboy_websocket, Req, State}.

handle_prometheus_metrics(Req, State) ->
    try
        % Fetch health metrics in Prometheus format
        Metrics = generate_prometheus_metrics(),
        Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, Metrics, Req),
        {ok, Req1, State}
    catch
        _:_ ->
            Req1 = cowboy_req:reply(500, #{}, <<"Internal error">>, Req),
            {ok, Req1, State}
    end.

handle_dashboard_html(Req, State) ->
    HTML = generate_dashboard_html(),
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, HTML, Req),
    {ok, Req1, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
%% Generate unique listener name
-spec listener_name(inet:port_number()) -> atom().
listener_name(Port) ->
    list_to_atom("erlmcp_health_dashboard_" ++ integer_to_list(Port)).

%% @private
%% Generate WebSocket client ID
-spec generate_client_id() -> binary().
generate_client_id() ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    list_to_binary(io_lib:format("client_~8.16.0b~8.16.0b~8.16.0b", [A, B, C])).

%% @private
%% Fetch health data from transport health monitor
-spec fetch_health_data() -> binary().
fetch_health_data() ->
    try
        OverallHealth = erlmcp_transport_health:overall_health(),
        jsx:encode(#{type => <<"health_update">>, data => OverallHealth})
    catch
        _:_ ->
            jsx:encode(#{type => <<"error">>, message => <<"Failed to fetch health data">>})
    end.

%% @private
%% Generate Prometheus metrics format
-spec generate_prometheus_metrics() -> iolist().
generate_prometheus_metrics() ->
    try
        OverallHealth = erlmcp_transport_health:overall_health(),
        #{overall_status := Status,
          healthy_count := Healthy,
          degraded_count := Degraded,
          unhealthy_count := Unhealthy,
          total_transports := Total} = OverallHealth,

        StatusValue = case Status of
            healthy -> 3;
            degraded -> 2;
            unhealthy -> 1;
            unknown -> 0
        end,

        [
            "# HELP erlmcp_transport_health_status Overall transport health status (0=unknown, 1=unhealthy, 2=degraded, 3=healthy)\n",
            "# TYPE erlmcp_transport_health_status gauge\n",
            io_lib:format("erlmcp_transport_health_status ~p\n", [StatusValue]),
            "\n",
            "# HELP erlmcp_transport_healthy_count Number of healthy transports\n",
            "# TYPE erlmcp_transport_healthy_count gauge\n",
            io_lib:format("erlmcp_transport_healthy_count ~p\n", [Healthy]),
            "\n",
            "# HELP erlmcp_transport_degraded_count Number of degraded transports\n",
            "# TYPE erlmcp_transport_degraded_count gauge\n",
            io_lib:format("erlmcp_transport_degraded_count ~p\n", [Degraded]),
            "\n",
            "# HELP erlmcp_transport_unhealthy_count Number of unhealthy transports\n",
            "# TYPE erlmcp_transport_unhealthy_count gauge\n",
            io_lib:format("erlmcp_transport_unhealthy_count ~p\n", [Unhealthy]),
            "\n",
            "# HELP erlmcp_transport_total Total number of transports\n",
            "# TYPE erlmcp_transport_total gauge\n",
            io_lib:format("erlmcp_transport_total ~p\n", [Total])
        ]
    catch
        _:_ ->
            "# Error generating metrics\n"
    end.

%% @private
%% Generate dashboard HTML
-spec generate_dashboard_html() -> binary().
generate_dashboard_html() ->
    <<
        "<!DOCTYPE html>\n"
        "<html lang='en'>\n"
        "<head>\n"
        "    <meta charset='UTF-8'>\n"
        "    <meta name='viewport' content='width=device-width, initial-scale=1.0'>\n"
        "    <title>erlmcp Transport Health Dashboard</title>\n"
        "    <style>\n"
        "        * { margin: 0; padding: 0; box-sizing: border-box; }\n"
        "        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #f5f5f5; }\n"
        "        .container { max-width: 1200px; margin: 0 auto; padding: 20px; }\n"
        "        .header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 30px; border-radius: 10px; margin-bottom: 30px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }\n"
        "        .header h1 { font-size: 32px; margin-bottom: 10px; }\n"
        "        .status-badge { display: inline-block; padding: 8px 16px; border-radius: 20px; font-weight: bold; font-size: 14px; }\n"
        "        .status-healthy { background: #10b981; }\n"
        "        .status-degraded { background: #f59e0b; }\n"
        "        .status-unhealthy { background: #ef4444; }\n"
        "        .status-unknown { background: #6b7280; }\n"
        "        .metrics { display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin-bottom: 30px; }\n"
        "        .metric-card { background: white; padding: 25px; border-radius: 10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }\n"
        "        .metric-card h3 { font-size: 14px; color: #6b7280; margin-bottom: 10px; text-transform: uppercase; letter-spacing: 0.5px; }\n"
        "        .metric-value { font-size: 36px; font-weight: bold; color: #1f2937; }\n"
        "        .transports-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 20px; }\n"
        "        .transport-card { background: white; padding: 20px; border-radius: 10px; border-left: 4px solid #10b981; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }\n"
        "        .transport-card.degraded { border-left-color: #f59e0b; }\n"
        "        .transport-card.unhealthy { border-left-color: #ef4444; }\n"
        "        .transport-card h3 { margin-bottom: 10px; color: #1f2937; }\n"
        "        .transport-info { font-size: 14px; color: #6b7280; }\n"
        "        .last-updated { text-align: center; color: #6b7280; font-size: 14px; margin-top: 30px; }\n"
        "    </style>\n"
        "</head>\n"
        "<body>\n"
        "    <div class='container'>\n"
        "        <div class='header'>\n"
        "            <h1>🔧 erlmcp Transport Health</h1>\n"
        "            <span id='overall-status' class='status-badge status-unknown'>Loading...</span>\n"
        "        </div>\n"
        "        <div class='metrics'>\n"
        "            <div class='metric-card'>\n"
        "                <h3>Healthy</h3>\n"
        "                <div class='metric-value' id='healthy-count' style='color: #10b981;'>-</div>\n"
        "            </div>\n"
        "            <div class='metric-card'>\n"
        "                <h3>Degraded</h3>\n"
        "                <div class='metric-value' id='degraded-count' style='color: #f59e0b;'>-</div>\n"
        "            </div>\n"
        "            <div class='metric-card'>\n"
        "                <h3>Unhealthy</h3>\n"
        "                <div class='metric-value' id='unhealthy-count' style='color: #ef4444;'>-</div>\n"
        "            </div>\n"
        "            <div class='metric-card'>\n"
        "                <h3>Total</h3>\n"
        "                <div class='metric-value' id='total-count'>-</div>\n"
        "            </div>\n"
        "        </div>\n"
        "        <div id='transports-container' class='transports-grid'></div>\n"
        "        <div class='last-updated'>Last updated: <span id='last-updated'>-</span></div>\n"
        "    </div>\n"
        "    <script>\n"
        "        const ws = new URL('/health/stream', window.location.href);\n"
        "        ws.protocol = ws.protocol.replace('http', 'ws');\n"
        "        const socket = new WebSocket(ws.href);\n"
        "        socket.onmessage = (event) => {\n"
        "            const data = JSON.parse(event.data);\n"
        "            if (data.type === 'health_update') {\n"
        "                updateDashboard(data.data);\n"
        "            }\n"
        "        };\n"
        "        function updateDashboard(health) {\n"
        "            document.getElementById('healthy-count').textContent = health.healthy_count;\n"
        "            document.getElementById('degraded-count').textContent = health.degraded_count;\n"
        "            document.getElementById('unhealthy-count').textContent = health.unhealthy_count;\n"
        "            document.getElementById('total-count').textContent = health.total_transports;\n"
        "            const statusBadge = document.getElementById('overall-status');\n"
        "            statusBadge.className = 'status-badge status-' + health.overall_status;\n"
        "            statusBadge.textContent = health.overall_status.toUpperCase();\n"
        "            const container = document.getElementById('transports-container');\n"
        "            container.innerHTML = '';\n"
        "            Object.entries(health.transports || {}).forEach(([id, transport]) => {\n"
        "                const card = document.createElement('div');\n"
        "                card.className = 'transport-card ' + transport.status;\n"
        "                card.innerHTML = `\n"
        "                    <h3>${id}</h3>\n"
        "                    <div class='transport-info'>\n"
        "                        Status: <strong>${transport.status}</strong><br>\n"
        "                        Last Check: ${new Date(transport.last_check).toLocaleString()}\n"
        "                    </div>\n"
        "                `;\n"
        "                container.appendChild(card);\n"
        "            });\n"
        "            document.getElementById('last-updated').textContent = new Date(health.timestamp).toLocaleString();\n"
        "        }\n"
        "    </script>\n"
        "</body>\n"
        "</html>\n"
    >>.

%% @private
%% Handle WebSocket message from client
-spec handle_ws_message(map(), #ws_state{}) -> {ok, #ws_state{}} | {[{text, binary()}], #ws_state{}}.
handle_ws_message(#{<<"type">> := <<"ping">>}, State) ->
    Pong = jsx:encode(#{type => <<"pong">>, timestamp => erlang:system_time(millisecond)}),
    {[{text, Pong}], State};
handle_ws_message(_Msg, State) ->
    {ok, State}.
