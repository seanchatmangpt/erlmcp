%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_transport_sse - Server-Sent Events Transport Implementation
%%%
%%% Implements SSE (Server-Sent Events) transport for CLI communications
%%% with unidirectional message streaming and JSON-RPC 2.0 support.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_sse).

-behaviour(erlmcp_transport).

%% API
-export([start_link/1, send/1, close/1]).

%% erlmcp_transport callbacks
-export([init/2, send/2, close/1]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(sse_state, {
    transport_id :: binary(),
    session_id :: binary(),
    host :: string(),
    port :: integer(),
    http_pid :: pid() | undefined,  % HTTP server process
    event_id :: integer(),  % Current event ID
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

%% @doc Start SSE transport
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Send data through SSE
-spec send(binary()) -> {ok, term()} | {error, term()}.
send(Data) ->
    gen_server:call(?SERVER, {send_data, Data}).

%% @doc Close SSE transport
-spec close() -> ok.
close() ->
    gen_server:call(?SERVER, close).

%%====================================================================
%% erlmcp_transport callbacks
%%====================================================================

%% @doc Initialize SSE transport
-spec init(atom(), map()) -> {ok, term()} | {error, term()}.
init(TransportType, Opts) when TransportType =:= sse ->
    case start_link(Opts) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc Send data through SSE
-spec send(binary(), #sse_state{}) -> {ok, #sse_state{}} | {error, term(), #sse_state{}}.
send(Data, State) ->
    %% Create OTEL span for SSE send
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.sse.send">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"sse">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#sse_state.session_id,
                                            <<"host">> => State#sse_state.host,
                                            <<"port">> => State#sse_state.port
                                        },
                                        undefined),

    %% Send data as SSE event
    case State#sse_state.http_pid of
        undefined ->
            %% HTTP server not started
            erlmcp_otel:record_error(SpanCtx, {sse_server_not_started, State#sse_state.http_pid}),
            {error, {sse_server_not_started, State#sse_state.http_pid}, State};
        _ ->
            try
                %% Create SSE message
                SSEMessage = create_sse_message(Data, State),

                %% Broadcast SSE message
                broadcast_sse_message(SSEMessage, State),

                %% Update metrics
                Metrics = update_metrics(State#sse_state.metrics, sent, ok),

                %% Record send event
                erlmcp_otel:add_event(SpanCtx, <<"sse.sent">>, #{
                    <<"data.size">> => size(Data),
                    <<"event_id">> => State#sse_state.event_id,
                    <<"timestamp">> => erlang:system_time(millisecond)
                }),

                {ok, State#sse_state{metrics = Metrics}}
            catch
                Error:Reason ->
                    %% Record error
                    erlmcp_otel:record_error(SpanCtx, {sse_send_error, Error, Reason}),

                    %% Update error metrics
                    Metrics = update_metrics(State#sse_state.metrics, sent, {error, Reason}),

                    {error, {sse_send_failed, Reason}, State#sse_state{metrics = Metrics}}
            end
    end.

%% @doc Close SSE transport
-spec close(#sse_state{}) -> ok.
close(State) ->
    %% Create OTEL span for SSE close
    erlmcp_otel:with_span("cli.transport.sse.close", #{
        <<"transport">> => <<"sse">>,
        <<"session">> => State#sse_state.session_id,
        <<"host">> => State#sse_state.host,
        <<"port">> => State#sse_state.port
    }, fun() ->
        %% Close HTTP server
        case State#sse_state.http_pid of
            undefined ->
                ok;
            _ ->
                cowboy:stop(State#sse_state.http_pid)
        end,

        %% Record close event
        erlmcp_otel:add_event(undefined, <<"sse.closed">>, #{
            <<"session">> => State#sse_state.session_id,
            <<"host">> => State#sse_state.host,
            <<"port">> => State#sse_state.port,
            <<"total_messages">> => maps:get("messages.total", State#sse_state.metrics, 0)
        }),

        ok
    end).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the server
-spec init(term()) -> {ok, #sse_state{}} | {stop, term()}.
init(Opts) ->
    %% Merge with default options
    FinalOpts = maps:merge(?DEFAULT_OPTS, Opts),

    %% Create OTEL span for SSE transport initialization
    SpanCtx = erlmcp_otel:with_span("cli.transport.sse.init", #{
        <<"transport">> => <<"sse">>,
        <<"session">> => FinalOpts#{session_id},
        <<"host">> => FinalOpts#{host},
        <<"port">> => FinalOpts#{port}
    }, fun() ->
        ok
    end),

    %% Initialize state
    State = #sse_state{
        transport_id = FinalOpts#{transport_id},
        session_id = FinalOpts#{session_id},
        host = FinalOpts#{host},
        port = FinalOpts#{port},
        http_pid = undefined,
        event_id = 1,
        metrics = #{
            "messages.sent" => 0,
            "messages.received" => 0,
            "errors.send" => 0,
            "errors.receive" => 0,
            "errors.connection" => 0,
            "session.start" => erlang:system_time(millisecond)
        }
    },

    %% Start HTTP server
    case start_http_server(State) of
        {ok, Pid} ->
            %% Record initialization metrics
            erlmcp_metrics:record("cli.transport.sse.initialized", 1),

            {ok, State#sse_state{http_pid = Pid}};
        {error, Reason} ->
            %% Record error
            erlmcp_otel:record_error(SpanCtx, {sse_server_start_failed, Reason}),

            {stop, Reason}
    end.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #sse_state{}) ->
    {reply, term(), #sse_state{}} | {stop, term(), #sse_state{}}.
handle_call({send_data, Data}, _From, State) ->
    %% Create OTEL span for SSE send
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.sse.send">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"sse">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#sse_state.session_id,
                                            <<"host">> => State#sse_state.host,
                                            <<"port">> => State#sse_state.port
                                        },
                                        undefined),

    %% Send data as SSE event
    case State#sse_state.http_pid of
        undefined ->
            %% HTTP server not started
            erlmcp_otel:record_error(SpanCtx, {sse_server_not_started, State#sse_state.http_pid}),

            %% Update error metrics
            Metrics = maps:update_with("errors.connection", fun(V) -> V + 1 end,
                                       State#sse_state.metrics),

            Reply = {error, {sse_server_not_started, State#sse_state.http_pid}},
            NewState = State#sse_state{metrics = Metrics};

        _ ->
            try
                %% Create SSE message
                SSEMessage = create_sse_message(Data, State),

                %% Broadcast SSE message
                broadcast_sse_message(SSEMessage, State),

                %% Update metrics
                Metrics = maps:update_with("messages.sent", fun(V) -> V + 1 end,
                                           State#sse_state.metrics),

                %% Record send event
                erlmcp_otel:add_event(SpanCtx, <<"sse.sent">>, #{
                    <<"data.size">> => size(Data),
                    <<"event_id">> => State#sse_state.event_id,
                    <<"timestamp">> => erlang:system_time(millisecond)
                }),

                Reply = ok,
                NewState = State#sse_state{metrics = Metrics, event_id = State#sse_state.event_id + 1};

            catch
                Error:Reason ->
                    %% Record error
                    erlmcp_otel:record_error(SpanCtx, {sse_send_error, Error, Reason}),

                    %% Update error metrics
                    Metrics = maps:update_with("errors.send", fun(V) -> V + 1 end,
                                               State#sse_state.metrics),

                    Reply = {error, {sse_send_failed, Reason}},
                    NewState = State#sse_state{metrics = Metrics}
            end
    end,

    {reply, Reply, NewState};

handle_call(close, _From, State) ->
    %% Create OTEL span for SSE close
    erlmcp_otel:with_span("cli.transport.sse.close", #{
        <<"transport">> => <<"sse">>,
        <<"session">> => State#sse_state.session_id,
        <<"host">> => State#sse_state.host,
        <<"port">> => State#sse_state.port
    }, fun() ->
        ok
    end),

    %% Close HTTP server
    case State#sse_state.http_pid of
        undefined ->
            ok;
        _ ->
            cowboy:stop(State#sse_state.http_pid)
    end,

    %% Record close event
    erlmcp_otel:add_event(undefined, <<"sse.closed">>, #{
        <<"session">> => State#sse_state.session_id,
        <<"host">> => State#sse_state.host,
        <<"port">> => State#sse_state.port,
        <<"total_messages">> => maps:get("messages.total", State#sse_state.metrics, 0)
    }),

    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #sse_state{}) -> {noreply, #sse_state{}} | {stop, term(), #sse_state{}}.
handle_cast({received_data, Data}, State) ->
    %% Create OTEL span for data reception
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.sse.receive">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"sse">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#sse_state.session_id,
                                            <<"host">> => State#sse_state.host,
                                            <<"port">> => State#sse_state.port
                                        },
                                        undefined),

    %% Process received data
    try
        %% Parse JSON-RPC request
        JsonData = jsx:decode(Data, [{labels, binary}]),

        %% Forward to JSON-RPC handler
        case erlmcp_cli_json_rpc:handle_json_rpc(Data, #{}, State#sse_state.session_id) of
            {ok, Response} ->
                lager:debug("SSE JSON-RPC response: ~p", [Response]);
            {error, Reason} ->
                lager:warning("Failed to handle JSON-RPC: ~p", [Reason])
        end,

        %% Update metrics
        Metrics = maps:update_with("messages.received", fun(V) -> V + 1 end,
                                   State#sse_state.metrics),

        %% Record reception event
        erlmcp_otel:add_event(SpanCtx, <<"sse.received">>, #{
            <<"data.size">> => size(Data),
            <<"timestamp">> => erlang:system_time(millisecond)
        }),

        {noreply, State#sse_state{metrics = Metrics}}

    catch
        Error:Reason ->
            %% Record error
            erlmcp_otel:record_error(SpanCtx, {sse_receive_error, Error, Reason}),

            %% Update error metrics
            Metrics = maps:update_with("errors.receive", fun(V) -> V + 1 end,
                                       State#sse_state.metrics),

            {noreply, State#sse_state{metrics = Metrics}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #sse_state{}) -> {noreply, #sse_state{}} | {stop, term(), #sse_state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), #sse_state{}) -> ok.
terminate(_Reason, State) ->
    %% Close HTTP server if running
    case State#sse_state.http_pid of
        undefined ->
            ok;
        _ ->
            cowboy:stop(State#sse_state.http_pid)
    end,

    %% Send final metrics
    erlmcp_metrics:record("cli.transport.sse.terminated", 1),

    ok.

%% @doc Handle code changes
-spec code_change(term(), #sse_state{}, term()) -> {ok, #sse_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Start HTTP server for SSE
-spec start_http_server(#sse_state{}) -> {ok, pid()} | {error, term()}.
start_http_server(State) ->
    %% Cowboy routing configuration
    Routes = [
        {"/events", erlmcp_sse_handler, []},
        {"/", erlmcp_sse_index_handler, []}
    ],

    %% Cowboy listener options
    TransOpts = #{
        num_acceptors => 5,
        max_connections => 500,
        shutdown => 5000,
        protocol => http,
        ip => {0, 0, 0, 0},
        port => State#sse_state.port
    },

    %% Cowboy options
    CowboyOpts = #{
        env => #{
            dispatch => cowboy_router:compile([
                {'_', Routes}
            ])
        },
        middlewares => [cowboy_router, cowboy_handler]
    },

    %% Start cowboy listener
    case cowboy:start_clear(erlmcp_sse_listener, TransOpts, CowboyOpts) of
        {ok, Pid} ->
            lager:info("SSE server started on ~s:~p", [State#sse_state.host, State#sse_state.port]),
            {ok, Pid};
        {error, Reason} ->
            lager:error("Failed to start SSE server: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc Create SSE message
-spec create_sse_message(binary(), #sse_state{}) -> binary().
create_sse_message(Data, State) ->
    %% Create SSE event format
    EventId = integer_to_list(State#sse_state.event_id),
    SSEMessage = list_to_binary([
        "id: ", EventId, "\n",
        "event: message\n",
        "data: ", Data, "\n",
        "\n"
    ]),
    SSEMessage.

%% @doc Broadcast SSE message
-spec broadcast_sse_message(binary(), #sse_state{}) -> ok.
broadcast_sse_message(SSEMessage, _State) ->
    %% In a real implementation, this would broadcast to connected clients
    %% For now, we'll just log it
    lager:debug("SSE message broadcasted: ~s", [SSEMessage]),
    ok.

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