%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_dashboard_server - Real-time Metrics Dashboard Server
%%%
%%% Cowboy HTTP server with WebSocket support for real-time metrics.
%%% Serves static dashboard UI and streams metrics updates every 1s.
%%%
%%% WebSocket Protocol:
%%%   Server -> Client: {metrics, #{timestamp, throughput, latency, ...}}
%%%   Client -> Server: {subscribe, [metric_names]} | {unsubscribe, [metric_names]}
%%%
%%% Port: 9090 (configurable via application:get_env/2)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_dashboard_server).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/0,
    get_port/0,
    broadcast_metrics/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% WebSocket handler
-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    port :: inet:port_number(),
    listener_pid :: pid() | undefined,
    websocket_pids = [] :: [pid()],
    metrics_timer :: reference() | undefined
}).

-record(ws_state, {
    subscribed_metrics = all :: all | [binary()],
    client_id :: binary()
}).

-define(DEFAULT_PORT, 9090).
-define(METRICS_INTERVAL, 1000). % Broadcast metrics every 1 second

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the dashboard server with default port
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    Port = application:get_env(erlmcp_observability, dashboard_port, ?DEFAULT_PORT),
    start_link(Port).

%% @doc Start the dashboard server with specified port
-spec start_link(inet:port_number()) -> {ok, pid()} | {error, term()}.
start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%% @doc Stop the dashboard server
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Get the current port the dashboard is listening on
-spec get_port() -> {ok, inet:port_number()} | {error, not_started}.
get_port() ->
    gen_server:call(?MODULE, get_port).

%% @doc Broadcast metrics to all connected WebSocket clients
-spec broadcast_metrics(map()) -> ok.
broadcast_metrics(Metrics) ->
    gen_server:cast(?MODULE, {broadcast_metrics, Metrics}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([inet:port_number()]) -> {ok, #state{}}.
init([Port]) ->
    ?LOG_INFO("Starting dashboard server on port ~p", [Port]),

    % Configure Cowboy routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, erlmcp_observability, "dashboard/index.html"}},
            {"/static/[...]", cowboy_static, {priv_dir, erlmcp_observability, "dashboard/static"}},
            {"/ws", ?MODULE, []},
            {"/api/metrics", erlmcp_dashboard_http_handler, []}
        ]}
    ]),

    % Start Cowboy HTTP listener
    {ok, ListenerPid} = cowboy:start_clear(
        erlmcp_dashboard_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),

    % Start periodic metrics broadcast timer
    {ok, TimerRef} = timer:send_interval(?METRICS_INTERVAL, self(), broadcast_metrics),

    ?LOG_INFO("Dashboard server started successfully on http://localhost:~p", [Port]),

    {ok, #state{
        port = Port,
        listener_pid = ListenerPid,
        metrics_timer = TimerRef
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.

handle_call(get_port, _From, State) ->
    {reply, {ok, State#state.port}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.

handle_cast({broadcast_metrics, Metrics}, State) ->
    % Broadcast to all WebSocket clients
    Message = jsx:encode(#{
        type => <<"metrics">>,
        data => Metrics
    }),
    lists:foreach(fun(WsPid) ->
        WsPid ! {send_metrics, Message}
    end, State#state.websocket_pids),
    {noreply, State};

handle_cast({register_ws, WsPid}, State) ->
    erlang:monitor(process, WsPid),
    {noreply, State#state{websocket_pids = [WsPid | State#state.websocket_pids]}};

handle_cast({unregister_ws, WsPid}, State) ->
    {noreply, State#state{websocket_pids = lists:delete(WsPid, State#state.websocket_pids)}};

handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(broadcast_metrics, State) ->
    % Fetch current metrics from aggregator
    case erlmcp_metrics_aggregator:get_current_metrics() of
        {ok, Metrics} ->
            gen_server:cast(?MODULE, {broadcast_metrics, Metrics});
        {error, Reason} ->
            ?LOG_WARNING("Failed to fetch metrics: ~p", [Reason])
    end,
    {noreply, State};

handle_info({'DOWN', _Ref, process, WsPid, _Reason}, State) ->
    % WebSocket process died, remove from list
    {noreply, State#state{websocket_pids = lists:delete(WsPid, State#state.websocket_pids)}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    % Stop Cowboy listener
    case State#state.listener_pid of
        undefined -> ok;
        _ -> cowboy:stop_listener(erlmcp_dashboard_listener)
    end,

    % Cancel metrics timer
    case State#state.metrics_timer of
        undefined -> ok;
        TimerRef -> timer:cancel(TimerRef)
    end,

    ?LOG_INFO("Dashboard server stopped"),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Cowboy WebSocket Handler Callbacks
%%====================================================================

%% @doc Initialize WebSocket connection
init(Req, _Opts) ->
    ClientId = generate_client_id(),
    ?LOG_INFO("WebSocket connection established: ~s", [ClientId]),
    {cowboy_websocket, Req, #ws_state{client_id = ClientId}}.

%% @doc WebSocket initialization after upgrade
websocket_init(State) ->
    % Register this WebSocket process with the dashboard server
    gen_server:cast(?MODULE, {register_ws, self()}),

    % Send initial connection acknowledgment
    Message = jsx:encode(#{
        type => <<"connected">>,
        client_id => State#ws_state.client_id,
        timestamp => erlang:system_time(millisecond)
    }),
    {[{text, Message}], State}.

%% @doc Handle incoming WebSocket messages from client
websocket_handle({text, Msg}, State) ->
    try
        Decoded = jsx:decode(Msg, [return_maps]),
        handle_client_message(Decoded, State)
    catch
        _:_ ->
            ErrorMsg = jsx:encode(#{
                type => <<"error">>,
                message => <<"Invalid JSON">>
            }),
            {[{text, ErrorMsg}], State}
    end;

websocket_handle(_Frame, State) ->
    {ok, State}.

%% @doc Handle messages sent to the WebSocket process
websocket_info({send_metrics, Message}, State) ->
    % Only send if subscribed
    case State#ws_state.subscribed_metrics of
        all ->
            {[{text, Message}], State};
        [] ->
            {ok, State};
        _SubscribedList ->
            % TODO: Filter metrics based on subscription
            {[{text, Message}], State}
    end;

websocket_info(_Info, State) ->
    {ok, State}.

%% @doc WebSocket termination
terminate(_Reason, _Req, State) ->
    gen_server:cast(?MODULE, {unregister_ws, self()}),
    ?LOG_INFO("WebSocket connection closed: ~s", [State#ws_state.client_id]),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Handle client WebSocket messages
-spec handle_client_message(map(), #ws_state{}) -> {list(), #ws_state{}}.
handle_client_message(#{<<"type">> := <<"subscribe">>, <<"metrics">> := Metrics}, State) ->
    NewState = State#ws_state{subscribed_metrics = Metrics},
    Response = jsx:encode(#{
        type => <<"subscribed">>,
        metrics => Metrics
    }),
    {[{text, Response}], NewState};

handle_client_message(#{<<"type">> := <<"unsubscribe">>}, State) ->
    NewState = State#ws_state{subscribed_metrics = []},
    Response = jsx:encode(#{
        type => <<"unsubscribed">>
    }),
    {[{text, Response}], NewState};

handle_client_message(#{<<"type">> := <<"ping">>}, State) ->
    Response = jsx:encode(#{
        type => <<"pong">>,
        timestamp => erlang:system_time(millisecond)
    }),
    {[{text, Response}], State};

handle_client_message(_Unknown, State) ->
    ErrorMsg = jsx:encode(#{
        type => <<"error">>,
        message => <<"Unknown message type">>
    }),
    {[{text, ErrorMsg}], State}.

%% @doc Generate unique client ID
-spec generate_client_id() -> binary().
generate_client_id() ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    list_to_binary(io_lib:format("client_~8.16.0b~8.16.0b~8.16.0b", [A, B, C])).
