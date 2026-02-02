%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_transport_ws - WebSocket Transport Implementation
%%%
%%% Implements WebSocket transport for CLI communications using gun/cowboy
%%% with bidirectional JSON-RPC 2.0 protocol handling.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_ws).

-behaviour(erlmcp_transport).

%% API
-export([start_link/1, send/1, close/1]).

%% erlmcp_transport callbacks
-export([init/2, send/2, close/1]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(ws_state, {
    transport_id :: binary(),
    session_id :: binary(),
    host :: string(),
    port :: integer(),
    ws_pid :: pid() | undefined,  % WebSocket process
    ws_ref :: reference() | undefined,  % WebSocket reference
    metrics :: map()
}).

-define(SERVER, ?MODULE).
-define(DEFAULT_OPTS, #{
    host => "localhost",
    port => 8080,
    session_id => erlmcp_utils:generate_id(),
    transport_id => erlmcp_utils:generate_id()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start WebSocket transport
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Send data through WebSocket
-spec send(binary()) -> {ok, term()} | {error, term()}.
send(Data) ->
    gen_server:call(?SERVER, {send_data, Data}).

%% @doc Close WebSocket transport
-spec close() -> ok.
close() ->
    gen_server:call(?SERVER, close).

%%====================================================================
%% erlmcp_transport callbacks
%%====================================================================

%% @doc Initialize WebSocket transport
-spec init(atom(), map()) -> {ok, term()} | {error, term()}.
init(TransportType, Opts) when TransportType =:= ws ->
    case start_link(Opts) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc Send data through WebSocket
-spec send(binary(), #ws_state{}) -> {ok, #ws_state{}} | {error, term(), #ws_state{}}.
send(Data, State) ->
    %% Create OTEL span for WebSocket send
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.ws.send">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"websocket">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#ws_state.session_id,
                                            <<"host">> => State#ws_state.host,
                                            <<"port">> => State#ws_state.port
                                        },
                                        undefined),

    %% Send data through WebSocket
    case State#ws_state.ws_pid of
        undefined ->
            %% WebSocket not connected
            erlmcp_otel:record_error(SpanCtx, {websocket_not_connected, State#ws_state.ws_pid}),
            {error, {websocket_not_connected, State#ws_state.ws_pid}, State};
        WSPid ->
            try
                %% Send JSON-RPC request
                JsonRequest = jsx:encode(#{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"method">> => <<"cli.transport.ws">>,
                    <<"params">> => #{<<"data">> => Data},
                    <<"id">> => erlmcp_utils:generate_id()
                }),

                case gun:send(WSPid, JsonRequest) of
                    ok ->
                        %% Update metrics
                        Metrics = update_metrics(State#ws_state.metrics, sent, ok),

                        %% Record send event
                        erlmcp_otel:add_event(SpanCtx, <<"websocket.sent">>, #{
                            <<"data.size">> => size(Data),
                            <<"timestamp">> => erlang:system_time(millisecond)
                        }),

                        {ok, State#ws_state{metrics = Metrics}};
                    {error, Reason} ->
                        %% Record error
                        erlmcp_otel:record_error(SpanCtx, {websocket_send_error, Reason}),

                        %% Update error metrics
                        Metrics = update_metrics(State#ws_state.metrics, sent, {error, Reason}),

                        {error, {websocket_send_failed, Reason}, State#ws_state{metrics = Metrics}}
                end
            catch
                Error:Reason ->
                    %% Record error
                    erlmcp_otel:record_error(SpanCtx, {websocket_send_exception, Error, Reason}),

                    %% Update error metrics
                    Metrics = update_metrics(State#ws_state.metrics, sent, {error, Reason}),

                    {error, {websocket_send_failed, Reason}, State#ws_state{metrics = Metrics}}
            end
    end.

%% @doc Close WebSocket transport
-spec close(#ws_state{}) -> ok.
close(State) ->
    %% Create OTEL span for WebSocket close
    erlmcp_otel:with_span("cli.transport.ws.close", #{
        <<"transport">> => <<"websocket">>,
        <<"session">> => State#ws_state.session_id,
        <<"host">> => State#ws_state.host,
        <<"port">> => State#ws_state.port
    }, fun() ->
        %% Close WebSocket connection
        case State#ws_state.ws_pid of
            undefined ->
                ok;
            WSPid ->
                gun:shutdown(WSPid)
        end,

        %% Record close event
        erlmcp_otel:add_event(undefined, <<"websocket.closed">>, #{
            <<"session">> => State#ws_state.session_id,
            <<"host">> => State#ws_state.host,
            <<"port">> => State#ws_state.port,
            <<"total_messages">> => maps:get("messages.total", State#ws_state.metrics, 0)
        }),

        ok
    end).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the server
-spec init(term()) -> {ok, #ws_state{}} | {stop, term()}.
init(Opts) ->
    %% Merge with default options
    FinalOpts = maps:merge(?DEFAULT_OPTS, Opts),

    %% Create OTEL span for WebSocket transport initialization
    SpanCtx = erlmcp_otel:with_span("cli.transport.ws.init", #{
        <<"transport">> => <<"websocket">>,
        <<"session">> => FinalOpts#{session_id},
        <<"host">> => FinalOpts#{host},
        <<"port">> => FinalOpts#{port}
    }, fun() ->
        ok
    end),

    %% Initialize state
    State = #ws_state{
        transport_id = FinalOpts#{transport_id},
        session_id = FinalOpts#{session_id},
        host = FinalOpts#{host},
        port = FinalOpts#{port},
        ws_pid = undefined,
        ws_ref = undefined,
        metrics = #{
            "messages.sent" => 0,
            "messages.received" => 0,
            "errors.send" => 0,
            "errors.receive" => 0,
            "errors.connection" => 0,
            "session.start" => erlang:system_time(millisecond)
        }
    },

    %% Connect to WebSocket server
    case connect_websocket(State) of
        {ok, WSPid, WSRef} ->
            %% Record initialization metrics
            erlmcp_metrics:record("cli.transport.ws.initialized", 1),

            {ok, State#ws_state{ws_pid = WSPid, ws_ref = WSRef}};
        {error, Reason} ->
            %% Record error
            erlmcp_otel:record_error(SpanCtx, {websocket_connection_failed, Reason}),

            {stop, Reason}
    end.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #ws_state{}) ->
    {reply, term(), #ws_state{}} | {stop, term(), #ws_state{}}.
handle_call({send_data, Data}, _From, State) ->
    %% Create OTEL span for WebSocket send
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.ws.send">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"websocket">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#ws_state.session_id,
                                            <<"host">> => State#ws_state.host,
                                            <<"port">> => State#ws_state.port
                                        },
                                        undefined),

    %% Send data through WebSocket
    case State#ws_state.ws_pid of
        undefined ->
            %% WebSocket not connected
            erlmcp_otel:record_error(SpanCtx, {websocket_not_connected, State#ws_state.ws_pid}),

            %% Update error metrics
            Metrics = maps:update_with("errors.connection", fun(V) -> V + 1 end,
                                       State#ws_state.metrics),

            Reply = {error, {websocket_not_connected, State#ws_state.ws_pid}},
            NewState = State#ws_state{metrics = Metrics};

        WSPid ->
            try
                %% Send JSON-RPC request
                JsonRequest = jsx:encode(#{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"method">> => <<"cli.transport.ws">>,
                    <<"params">> => #{<<"data">> => Data},
                    <<"id">> => erlmcp_utils:generate_id()
                }),

                case gun:send(WSPid, JsonRequest) of
                    ok ->
                        %% Update metrics
                        Metrics = maps:update_with("messages.sent", fun(V) -> V + 1 end,
                                                   State#ws_state.metrics),

                        %% Record send event
                        erlmcp_otel:add_event(SpanCtx, <<"websocket.sent">>, #{
                            <<"data.size">> => size(Data),
                            <<"timestamp">> => erlang:system_time(millisecond)
                        }),

                        Reply = ok,
                        NewState = State#ws_state{metrics = Metrics};

                    {error, Reason} ->
                        %% Record error
                        erlmcp_otel:record_error(SpanCtx, {websocket_send_error, Reason}),

                        %% Update error metrics
                        Metrics = maps:update_with("errors.send", fun(V) -> V + 1 end,
                                                   State#ws_state.metrics),

                        Reply = {error, {websocket_send_failed, Reason}},
                        NewState = State#ws_state{metrics = Metrics}
                end
            catch
                Error:Reason ->
                    %% Record error
                    erlmcp_otel:record_error(SpanCtx, {websocket_send_exception, Error, Reason}),

                    %% Update error metrics
                    Metrics = maps:update_with("errors.send", fun(V) -> V + 1 end,
                                               State#ws_state.metrics),

                    Reply = {error, {websocket_send_failed, Reason}},
                    NewState = State#ws_state{metrics = Metrics}
            end
    end,

    {reply, Reply, NewState};

handle_call(close, _From, State) ->
    %% Create OTEL span for WebSocket close
    erlmcp_otel:with_span("cli.transport.ws.close", #{
        <<"transport">> => <<"websocket">>,
        <<"session">> => State#ws_state.session_id,
        <<"host">> => State#ws_state.host,
        <<"port">> => State#ws_state.port
    }, fun() ->
        ok
    end),

    %% Close WebSocket connection
    case State#ws_state.ws_pid of
        undefined ->
            ok;
        WSPid ->
            gun:shutdown(WSPid)
    end,

    %% Record close event
    erlmcp_otel:add_event(undefined, <<"websocket.closed">>, #{
        <<"session">> => State#ws_state.session_id,
        <<"host">> => State#ws_state.host,
        <<"port">> => State#ws_state.port,
        <<"total_messages">> => maps:get("messages.total", State#ws_state.metrics, 0)
    }),

    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #ws_state{}) -> {noreply, #ws_state{}} | {stop, term(), #ws_state{}}.
handle_cast({received_data, Data}, State) ->
    %% Create OTEL span for data reception
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.ws.receive">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"websocket">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#ws_state.session_id,
                                            <<"host">> => State#ws_state.host,
                                            <<"port">> => State#ws_state.port
                                        },
                                        undefined),

    %% Process received data
    try
        %% Parse JSON-RPC request
        JsonData = erlmcp_json_native:decode(Data),

        %% Forward to JSON-RPC handler
        case erlmcp_cli_json_rpc:handle_json_rpc(Data, #{}, State#ws_state.session_id) of
            {ok, Response} ->
                lager:debug("WebSocket JSON-RPC response: ~p", [Response]);
            {error, Reason} ->
                lager:warning("Failed to handle JSON-RPC: ~p", [Reason])
        end,

        %% Update metrics
        Metrics = maps:update_with("messages.received", fun(V) -> V + 1 end,
                                   State#ws_state.metrics),

        %% Record reception event
        erlmcp_otel:add_event(SpanCtx, <<"websocket.received">>, #{
            <<"data.size">> => size(Data),
            <<"timestamp">> => erlang:system_time(millisecond)
        }),

        {noreply, State#ws_state{metrics = Metrics}}

    catch
        Error:Reason ->
            %% Record error
            erlmcp_otel:record_error(SpanCtx, {websocket_receive_error, Error, Reason}),

            %% Update error metrics
            Metrics = maps:update_with("errors.receive", fun(V) -> V + 1 end,
                                       State#ws_state.metrics),

            {noreply, State#ws_state{metrics = Metrics}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #ws_state{}) -> {noreply, #ws_state{}} | {stop, term(), #ws_state{}}.
handle_info({'DOWN', Ref, process, WSPid, Reason}, State) ->
    %% WebSocket process died
    lager:warning("WebSocket process ~p died: ~p", [WSPid, Reason]),

    %% Check if this is our WebSocket
    case Ref of
        State#ws_state.ws_ref ->
            %% Record death event
            erlmcp_otel:record_error(undefined, {websocket_process_died, WSPid, Reason}),

            %% Update metrics
            Metrics = maps:update_with("errors.connection", fun(V) -> V + 1 end,
                                       State#ws_state.metrics),

            case Reason of
                normal ->
                    %% Expected shutdown
                    {noreply, State#ws_state{ws_pid = undefined, ws_ref = undefined, metrics = Metrics}};
                _ ->
                    Unexpected = true,
                    %% Unexpected shutdown, try to reconnect
                    case connect_websocket(State) of
                        {ok, NewWSPid, NewWSRef} ->
                            {noreply, State#ws_state{ws_pid = NewWSPid, ws_ref = NewWSRef, metrics = Metrics}};
                        {error, ConnectReason} ->
                            lager:error("Failed to reconnect WebSocket: ~p", [ConnectReason]),
                            {noreply, State#ws_state{ws_pid = undefined, ws_ref = undefined, metrics = Metrics}}
                    end
            end;
        _ ->
            {noreply, State}
    end;

handle_info({gun_up, WSPid, ws}, State) ->
    %% WebSocket connection established
    lager:info("WebSocket connection established to ~s:~p", [State#ws_state.host, State#ws_state.port]),

    %% Record connection event
    erlmcp_otel:add_event(undefined, <<"websocket.connected">>, #{
        <<"session">> => State#ws_state.session_id,
        <<"host">> => State#ws_state.host,
        <<"port">> => State#ws_state.port
    }),

    {noreply, State};

handle_info({gun_down, WSPid, ws, Reason}, State) ->
    %% WebSocket connection closed
    lager:warning("WebSocket connection closed: ~p", [Reason]),

    %% Record disconnection event
    erlmcp_otel:record_error(undefined, {websocket_disconnected, Reason}),

    %% Update metrics
    Metrics = maps:update_with("errors.connection", fun(V) -> V + 1 end,
                               State#ws_state.metrics),

    case Reason of
        normal ->
            %% Expected shutdown
            {noreply, State#ws_state{ws_pid = undefined, ws_ref = undefined, metrics = Metrics}};
        _ ->
            Unexpected = true,
            %% Unexpected shutdown, try to reconnect
            case connect_websocket(State) of
                {ok, NewWSPid, NewWSRef} ->
                    {noreply, State#ws_state{ws_pid = NewWSPid, ws_ref = NewWSRef, metrics = Metrics}};
                {error, ConnectReason} ->
                    lager:error("Failed to reconnect WebSocket: ~p", [ConnectReason]),
                    {noreply, State#ws_state{ws_pid = undefined, ws_ref = undefined, metrics = Metrics}}
            end
    end;

handle_info({gun_data, WSPid, fin, Data}, State) ->
    %% WebSocket message received
    self() ! {gun_data, WSPid, Data},
    {noreply, State};

handle_info({gun_data, WSPid, Data}, State) ->
    %% WebSocket message data received
    handle_cast({received_data, Data}, State);

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), #ws_state{}) -> ok.
terminate(_Reason, State) ->
    %% Close WebSocket if connected
    case State#ws_state.ws_pid of
        undefined ->
            ok;
        WSPid ->
            gun:shutdown(WSPid)
    end,

    %% Send final metrics
    erlmcp_metrics:record("cli.transport.ws.terminated", 1),

    ok.

%% @doc Handle code changes
-spec code_change(term(), #ws_state{}, term()) -> {ok, #ws_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Connect to WebSocket server
-spec connect_websocket(#ws_state{}) -> {ok, pid(), reference()} | {error, term()}.
connect_websocket(State) ->
    %% WebSocket URL
    WSURL = "ws://" ++ State#ws_state.host ++ ":" ++ integer_to_list(State#ws_state.port) ++ "/ws",

    %% Connect to WebSocket
    case gun:open(State#ws_state.port, State#ws_state.host) of
        {ok, WSPid} ->
            erlang:monitor(process, WSPid),
            Ref = gun:ws_upgrade(WSPid, WSURL),
            {ok, WSPid, Ref};
        {error, Reason} ->
            {error, {websocket_connect_failed, Reason}}
    end.

%% @doc Update metrics
-spec update_metrics(map(), atom(), term()) -> map().
update_metrics(Metrics, Operation, Result) ->
    Updated = Metrics,

    %% Update operation count
    OperationKey = case Operation of
        sent -> "messages.sent";
        received -> "messages.received"
    end,
    Updated1 = maps:update_with(OperationKey, fun(V) -> V + 1 end, 1, Updated),

    %% Update success/failure count
    ResultKey = case Result of
        ok -> "success";
        {error, _} -> "errors"
    end,
    Updated2 = maps:update_with(ResultKey, fun(V) -> V + 1 end, 1, Updated1),

    %% Update total messages
    Updated3 = maps:update_with("messages.total", fun(V) -> V + 1 end, 1, Updated2),

    Updated3.

%% @doc Make request ID
-spec make_request_id() -> binary().
make_request_id() ->
    Id = crypto:strong_rand_bytes(16),
    base64:encode(Id).