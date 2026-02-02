%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_transport_tcp - TCP Transport Implementation
%%%
%%% Implements TCP transport for CLI communications using gun/ranch
%%% for high-performance HTTP/WebSocket support.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_tcp).

-behaviour(erlmcp_transport).

%% API
-export([start_link/1, send/1, close/1]).

%% erlmcp_transport callbacks
-export([init/2, send/2, close/1]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(tcp_state, {
    transport_id :: binary(),
    session_id :: binary(),
    socket :: port() | undefined,
    host :: string(),
    port :: integer(),
    pid :: pid() | undefined,  % gun process
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

%% @doc Start TCP transport
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Send data through TCP
-spec send(binary()) -> {ok, term()} | {error, term()}.
send(Data) ->
    gen_server:call(?SERVER, {send_data, Data}).

%% @doc Close TCP transport
-spec close() -> ok.
close() ->
    gen_server:call(?SERVER, close).

%%====================================================================
%% erlmcp_transport callbacks
%%====================================================================

%% @doc Initialize TCP transport
-spec init(atom(), map()) -> {ok, term()} | {error, term()}.
init(TransportType, Opts) when TransportType =:= tcp ->
    case start_link(Opts) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc Send data through TCP
-spec send(binary(), #tcp_state{}) -> {ok, #tcp_state{}} | {error, term(), #tcp_state{}}.
send(Data, State) ->
    %% Create OTEL span for TCP send
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.tcp.send">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"tcp">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#tcp_state.session_id,
                                            <<"host">> => State#tcp_state.host,
                                            <<"port">> => State#tcp_state.port
                                        },
                                        undefined),

    %% Send data through gun
    case State#tcp_state.pid of
        undefined ->
            %% Gun process not started
            erlmcp_otel:record_error(SpanCtx, {gun_not_started, State#tcp_state.pid}),
            {error, {gun_not_started, State#tcp_state.pid}, State};
        GunPid ->
            try
                %% Send data through gun
                ok = gun:send(GunPid, Data),

                %% Update metrics
                Metrics = update_metrics(State#tcp_state.metrics, sent, ok),

                %% Record send event
                erlmcp_otel:add_event(SpanCtx, <<"tcp.sent">>, #{
                    <<"data.size">> => size(Data),
                    <<"timestamp">> => erlang:system_time(millisecond)
                }),

                {ok, State#tcp_state{metrics = Metrics}}
            catch
                Error:Reason ->
                    %% Record error
                    erlmcp_otel:record_error(SpanCtx, {tcp_send_error, Error, Reason}),

                    %% Update error metrics
                    Metrics = update_metrics(State#tcp_state.metrics, sent, {error, Reason}),

                    {error, {tcp_send_failed, Reason}, State#tcp_state{metrics = Metrics}}
            end
    end.

%% @doc Close TCP transport
-spec close(#tcp_state{}) -> ok.
close(State) ->
    %% Create OTEL span for TCP close
    erlmcp_otel:with_span("cli.transport.tcp.close", #{
        <<"transport">> => <<"tcp">>,
        <<"session">> => State#tcp_state.session_id,
        <<"host">> => State#tcp_state.host,
        <<"port">> => State#tcp_state.port
    }, fun() ->
        %% Close gun process
        case State#tcp_state.pid of
            undefined ->
                ok;
            GunPid ->
                gun:shutdown(GunPid)
        end,

        %% Record close event
        erlmcp_otel:add_event(undefined, <<"tcp.closed">>, #{
            <<"session">> => State#tcp_state.session_id,
            <<"host">> => State#tcp_state.host,
            <<"port">> => State#tcp_state.port,
            <<"total_messages">> => maps:get("messages.total", State#tcp_state.metrics, 0)
        }),

        ok
    end).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the server
-spec init(term()) -> {ok, #tcp_state{}} | {stop, term()}.
init(Opts) ->
    %% Merge with default options
    FinalOpts = maps:merge(?DEFAULT_OPTS, Opts),

    %% Create OTEL span for TCP transport initialization
    SpanCtx = erlmcp_otel:with_span("cli.transport.tcp.init", #{
        <<"transport">> => <<"tcp">>,
        <<"session">> => FinalOpts#{session_id},
        <<"host">> => FinalOpts#{host},
        <<"port">> => FinalOpts#{port}
    }, fun() ->
        ok
    end),

    %% Initialize state
    State = #tcp_state{
        transport_id = FinalOpts#{transport_id},
        session_id = FinalOpts#{session_id},
        host = FinalOpts#{host},
        port = FinalOpts#{port},
        socket = undefined,
        pid = undefined,
        metrics = #{
            "messages.sent" => 0,
            "messages.received" => 0,
            "errors.send" => 0,
            "errors.receive" => 0,
            "errors.connection" => 0,
            "session.start" => erlang:system_time(millisecond)
        }
    },

    %% Start gun process
    case start_gun_process(State) of
        {ok, GunPid} ->
            %% Record initialization metrics
            erlmcp_metrics:record("cli.transport.tcp.initialized", 1),

            {ok, State#tcp_state{pid = GunPid}};
        {error, Reason} ->
            %% Record error
            erlmcp_otel:record_error(SpanCtx, {gun_start_failed, Reason}),

            {stop, Reason}
    end.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #tcp_state{}) ->
    {reply, term(), #tcp_state{}} | {stop, term(), #tcp_state{}}.
handle_call({send_data, Data}, _From, State) ->
    %% Create OTEL span for TCP send
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.tcp.send">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"tcp">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#tcp_state.session_id,
                                            <<"host">> => State#tcp_state.host,
                                            <<"port">> => State#tcp_state.port
                                        },
                                        undefined),

    %% Send data through gun
    case State#tcp_state.pid of
        undefined ->
            %% Gun process not started
            erlmcp_otel:record_error(SpanCtx, {gun_not_started, State#tcp_state.pid}),

            %% Update error metrics
            Metrics = maps:update_with("errors.connection", fun(V) -> V + 1 end,
                                       State#tcp_state.metrics),

            Reply = {error, {gun_not_started, State#tcp_state.pid}},
            NewState = State#tcp_state{metrics = Metrics};

        GunPid ->
            try
                %% Send data through gun
                ok = gun:send(GunPid, Data),

                %% Update metrics
                Metrics = maps:update_with("messages.sent", fun(V) -> V + 1 end,
                                           State#tcp_state.metrics),

                %% Record send event
                erlmcp_otel:add_event(SpanCtx, <<"tcp.sent">>, #{
                    <<"data.size">> => size(Data),
                    <<"timestamp">> => erlang:system_time(millisecond)
                }),

                Reply = ok,
                NewState = State#tcp_state{metrics = Metrics};

            catch
                Error:Reason ->
                    %% Record error
                    erlmcp_otel:record_error(SpanCtx, {tcp_send_error, Error, Reason}),

                    %% Update error metrics
                    Metrics = maps:update_with("errors.send", fun(V) -> V + 1 end,
                                               State#tcp_state.metrics),

                    Reply = {error, {tcp_send_failed, Reason}},
                    NewState = State#tcp_state{metrics = Metrics}
            end
    end,

    {reply, Reply, NewState};

handle_call(close, _From, State) ->
    %% Create OTEL span for TCP close
    erlmcp_otel:with_span("cli.transport.tcp.close", #{
        <<"transport">> => <<"tcp">>,
        <<"session">> => State#tcp_state.session_id,
        <<"host">> => State#tcp_state.host,
        <<"port">> => State#tcp_state.port
    }, fun() ->
        ok
    end),

    %% Close gun process
    case State#tcp_state.pid of
        undefined ->
            ok;
        GunPid ->
            gun:shutdown(GunPid)
    end,

    %% Record close event
    erlmcp_otel:add_event(undefined, <<"tcp.closed">>, #{
        <<"session">> => State#tcp_state.session_id,
        <<"host">> => State#tcp_state.host,
        <<"port">> => State#tcp_state.port,
        <<"total_messages">> => maps:get("messages.total", State#tcp_state.metrics, 0)
    }),

    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #tcp_state{}) -> {noreply, #tcp_state{}} | {stop, term(), #tcp_state{}}.
handle_cast({received_data, Data}, State) ->
    %% Create OTEL span for data reception
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.tcp.receive">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"tcp">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#tcp_state.session_id,
                                            <<"host">> => State#tcp_state.host,
                                            <<"port">> => State#tcp_state.port
                                        },
                                        undefined),

    %% Process received data
    try
        %% Parse JSON-RPC request
        JsonData = jsx:decode(Data, [{labels, binary}]),

        %% Forward to JSON-RPC handler
        case erlmcp_cli_json_rpc:handle_json_rpc(Data, #{}, State#tcp_state.session_id) of
            {ok, Response} ->
                %% Send response back through gun
                case State#tcp_state.pid of
                    undefined ->
                        lager:warning("Gun process not available for response");
                    GunPid ->
                        gun:send(GunPid, Response)
                end;
            {error, Reason} ->
                %% Log error
                lager:warning("Failed to handle JSON-RPC: ~p", [Reason])
        end,

        %% Update metrics
        Metrics = maps:update_with("messages.received", fun(V) -> V + 1 end,
                                   State#tcp_state.metrics),

        %% Record reception event
        erlmcp_otel:add_event(SpanCtx, <<"tcp.received">>, #{
            <<"data.size">> => size(Data),
            <<"timestamp">> => erlang:system_time(millisecond)
        }),

        {noreply, State#tcp_state{metrics = Metrics}}

    catch
        Error:Reason ->
            %% Record error
            erlmcp_otel:record_error(SpanCtx, {tcp_receive_error, Error, Reason}),

            %% Update error metrics
            Metrics = maps:update_with("errors.receive", fun(V) -> V + 1 end,
                                       State#tcp_state.metrics),

            {noreply, State#tcp_state{metrics = Metrics}}
    end;

handle_cast(Msg, State) ->
    lager:debug("Received cast: ~p", [Msg]),
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #tcp_state{}) -> {noreply, #tcp_state{}} | {stop, term(), #tcp_state{}}.
handle_info({'DOWN', MonitorRef, process, GunPid, Reason}, State) ->
    %% Gun process died
    lager:warning("Gun process ~p died: ~p", [GunPid, Reason]),

    %% Record death event
    erlmcp_otel:record_error(undefined, {gun_process_died, GunPid, Reason}),

    %% Update metrics
    Metrics = maps:update_with("errors.connection", fun(V) -> V + 1 end,
                               State#tcp_state.metrics),

    case Reason of
        normal ->
            %% Expected shutdown
            {noreply, State#tcp_state{pid = undefined, metrics = Metrics}};
        _ ->
            Unexpected = true,
            %% Unexpected shutdown, try to restart
            case start_gun_process(State) of
                {ok, NewGunPid} ->
                    {noreply, State#tcp_state{pid = NewGunPid, metrics = Metrics}};
                {error, RestartReason} ->
                    lager:error("Failed to restart gun process: ~p", [RestartReason]),
                    {noreply, State#tcp_state{pid = undefined, metrics = Metrics}}
            end
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), #tcp_state{}) -> ok.
terminate(_Reason, State) ->
    %% Close gun process if running
    case State#tcp_state.pid of
        undefined ->
            ok;
        GunPid ->
            gun:shutdown(GunPid)
    end,

    %% Send final metrics
    erlmcp_metrics:record("cli.transport.tcp.terminated", 1),

    ok.

%% @doc Handle code changes
-spec code_change(term(), #tcp_state{}, term()) -> {ok, #tcp_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Start gun process
-spec start_gun_process(#tcp_state{}) -> {ok, pid()} | {error, term()}.
start_gun_process(State) ->
    %% Create gun transport options
    TransportOpts = #{
        transport => ranch_tcp,
        protocol => {tcp, []},
        transport_opts => #{
            port => State#tcp_state.port,
            ip => {0, 0, 0, 0},
            backlog => 128,
            nodelay => true,
            reuseaddr => true
        },
        protocol_opts => #{
            env => #{
                dispatch => gun_room:dispatch([
                    {"/", erlmcp_tcp_handler, []}
                ])
            }
        }
    },

    %% Start gun server
    case gun:start(TransportOpts) of
        {ok, GunPid} ->
            %% Set up monitor for gun process
            erlang:monitor(process, GunPid),

            %% Connect to gun process
            case gun:connect(GunPid, State#tcp_state.host, State#tcp_state.port) of
                ok ->
                    lager:info("Connected to ~s:~p", [State#tcp_state.host, State#tcp_state.port]),
                    {ok, GunPid};
                {error, Reason} ->
                    gun:shutdown(GunPid),
                    {error, {connection_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
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