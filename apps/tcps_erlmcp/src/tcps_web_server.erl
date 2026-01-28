%%%-------------------------------------------------------------------
%%% @doc TCPS MCP Diataxis Web Server
%%%
%%% Cowboy-based HTTP server providing the web UI for the TCPS MCP
%%% Diataxis simulator. Supports static file serving, REST API,
%%% and WebSocket connections for real-time updates.
%%%
%%% Features:
%%% - Diataxis quadrant navigation (Tutorial, How-To, Explanation, Reference)
%%% - Interactive TCPS workflow simulator with Kanban visualization
%%% - Real-time metrics dashboard with SSE/WebSocket updates
%%% - MCP tool playground for testing integrations
%%% - Quality gate and Andon alert monitoring
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_web_server).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/0]).
-export([get_server_info/0, broadcast_event/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8088).
-define(DEFAULT_HOST, "0.0.0.0").

-record(state, {
    config :: map(),
    http_listener :: pid() | undefined,
    ws_connections :: #{pid() => map()},
    start_time :: erlang:timestamp()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the web server with configuration
-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

%% @doc Stop the web server
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Get server information
-spec get_server_info() -> map().
get_server_info() ->
    gen_server:call(?SERVER, get_server_info).

%% @doc Broadcast event to all WebSocket connections
-spec broadcast_event(Type :: atom(), Data :: map()) -> ok.
broadcast_event(Type, Data) ->
    gen_server:cast(?SERVER, {broadcast_event, Type, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Config]) ->
    Port = maps:get(port, Config, ?DEFAULT_PORT),
    Host = maps:get(host, Config, ?DEFAULT_HOST),

    ?LOG_INFO("Starting TCPS Web Server on ~s:~p", [Host, Port]),

    %% Initialize ETS table for WebSocket connections
    ets:new(tcps_ws_connections, [set, public, named_table, {write_concurrency, true}]),

    %% Start HTTP listener
    case start_http_listener(Port) of
        {ok, Listener} ->
            ?LOG_INFO("TCPS Web Server started successfully on port ~p", [Port]),
            ?LOG_INFO("Access the simulator at: http://localhost:~p", [Port]),

            State = #state{
                config = Config,
                http_listener = Listener,
                ws_connections = #{},
                start_time = erlang:timestamp()
            },
            {ok, State};
        {error, Reason} ->
            ?LOG_ERROR("Failed to start HTTP listener: ~p", [Reason]),
            {stop, Reason}
    end.

handle_call(get_server_info, _From, State) ->
    Info = #{
        port => maps:get(port, State#state.config, ?DEFAULT_PORT),
        uptime_seconds => calculate_uptime(State#state.start_time),
        ws_connections => maps:size(State#state.ws_connections),
        status => running
    },
    {reply, Info, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({broadcast_event, Type, Data}, State) ->
    Event = #{
        type => Type,
        data => Data,
        timestamp => erlang:system_time(millisecond)
    },
    broadcast_to_websockets(Event),
    {noreply, State};

handle_cast({register_ws_connection, Pid, Metadata}, State) ->
    monitor(process, Pid),
    NewConnections = maps:put(Pid, Metadata, State#state.ws_connections),
    ?LOG_INFO("WebSocket connection registered: ~p", [Pid]),
    {noreply, State#state{ws_connections = NewConnections}};

handle_cast({unregister_ws_connection, Pid}, State) ->
    NewConnections = maps:remove(Pid, State#state.ws_connections),
    ?LOG_INFO("WebSocket connection unregistered: ~p", [Pid]),
    {noreply, State#state{ws_connections = NewConnections}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% WebSocket connection died
    NewConnections = maps:remove(Pid, State#state.ws_connections),
    {noreply, State#state{ws_connections = NewConnections}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    stop_http_listener(State#state.http_listener),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Start Cowboy HTTP listener
start_http_listener(Port) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            %% Static files
            {"/", cowboy_static, {priv_file, erlmcp, "static/index.html"}},
            {"/index.html", cowboy_static, {priv_file, erlmcp, "static/index.html"}},
            {"/css/[...]", cowboy_static, {priv_dir, erlmcp, "static/css"}},
            {"/js/[...]", cowboy_static, {priv_dir, erlmcp, "static/js"}},
            {"/assets/[...]", cowboy_static, {priv_dir, erlmcp, "static/assets"}},

            %% API endpoints
            {"/api/diataxis/quadrants", tcps_api_handler, #{endpoint => diataxis_quadrants}},
            {"/api/diataxis/content/:quadrant", tcps_api_handler, #{endpoint => diataxis_content}},
            {"/api/simulator/status", tcps_api_handler, #{endpoint => simulator_status}},
            {"/api/simulator/workflow", tcps_api_handler, #{endpoint => workflow}},
            {"/api/simulator/workflow/:id", tcps_api_handler, #{endpoint => workflow_detail}},
            {"/api/simulator/kanban", tcps_api_handler, #{endpoint => kanban}},
            {"/api/simulator/metrics", tcps_api_handler, #{endpoint => metrics}},
            {"/api/simulator/andons", tcps_api_handler, #{endpoint => andons}},
            {"/api/simulator/quality-gates", tcps_api_handler, #{endpoint => quality_gates}},
            {"/api/mcp/tools", tcps_api_handler, #{endpoint => mcp_tools}},
            {"/api/mcp/tools/:tool/execute", tcps_api_handler, #{endpoint => mcp_execute}},
            {"/api/health", tcps_api_handler, #{endpoint => health}},

            %% WebSocket endpoint
            {"/ws", tcps_websocket_handler, #{}}
        ]}
    ]),

    cowboy:start_clear(
        tcps_web_http_listener,
        [{port, Port}],
        #{
            env => #{dispatch => Dispatch},
            middlewares => [cowboy_router, cowboy_handler],
            idle_timeout => 60000,
            max_keepalive => 100
        }
    ).

%% @private Stop HTTP listener
stop_http_listener(_Listener) ->
    cowboy:stop_listener(tcps_web_http_listener).

%% @private Calculate uptime in seconds
calculate_uptime(StartTime) ->
    Now = erlang:timestamp(),
    timer:now_diff(Now, StartTime) div 1000000.

%% @private Broadcast event to all WebSocket connections
broadcast_to_websockets(Event) ->
    case ets:tab2list(tcps_ws_connections) of
        [] ->
            ok;
        Connections ->
            Message = jsx:encode(Event),
            lists:foreach(fun({Pid, _Meta}) ->
                case is_process_alive(Pid) of
                    true ->
                        Pid ! {ws_send, Message};
                    false ->
                        ets:delete(tcps_ws_connections, Pid)
                end
            end, Connections)
    end.
