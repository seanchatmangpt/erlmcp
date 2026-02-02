%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_transport_http - HTTP Transport Implementation
%%%
%%% Implements HTTP transport for CLI communications using cowboy/gun
%%% with proper JSON-RPC 2.0 protocol handling.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_http).

-behaviour(erlmcp_transport).

%% API
-export([start_link/1, send/1, close/1]).

%% erlmcp_transport callbacks
-export([init/2, send/2, close/1]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(http_state, {
    transport_id :: binary(),
    session_id :: binary(),
    host :: string(),
    port :: integer(),
    pid :: pid() | undefined,  % cowboy/httpd process
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

%% @doc Start HTTP transport
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Send data through HTTP
-spec send(binary()) -> {ok, term()} | {error, term()}.
send(Data) ->
    gen_server:call(?SERVER, {send_data, Data}).

%% @doc Close HTTP transport
-spec close() -> ok.
close() ->
    gen_server:call(?SERVER, close).

%%====================================================================
%% erlmcp_transport callbacks
%%====================================================================

%% @doc Initialize HTTP transport
-spec init(atom(), map()) -> {ok, term()} | {error, term()}.
init(TransportType, Opts) when TransportType =:= http ->
    case start_link(Opts) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc Send data through HTTP
-spec send(binary(), #http_state{}) -> {ok, #http_state{}} | {error, term(), #http_state{}}.
send(Data, State) ->
    %% Create OTEL span for HTTP send
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.http.send">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"http">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#http_state.session_id,
                                            <<"host">> => State#http_state.host,
                                            <<"port">> => State#http_state.port
                                        },
                                        undefined),

    %% Send HTTP request
    case State#http_state.pid of
        undefined ->
            %% HTTP server not started
            erlmcp_otel:record_error(SpanCtx, {http_server_not_started, State#http_state.pid}),
            {error, {http_server_not_started, State#http_state.pid}, State};
        _ ->
            try
                %% Create HTTP request
                HttpRequest = create_http_request(Data, State),

                %% Send request
                case httpc:request(HttpRequest) of
                    {ok, {_, _, ResponseBody}} ->
                        %% Parse response
                        JsonResponse = erlmcp_json_native:decode(ResponseBody),

                        %% Update metrics
                        Metrics = update_metrics(State#http_state.metrics, sent, ok),

                        %% Record send event
                        erlmcp_otel:add_event(SpanCtx, <<"http.sent">>, #{
                            <<"data.size">> => size(Data),
                            <<"response.size">> => size(ResponseBody),
                            <<"timestamp">> => erlang:system_time(millisecond)
                        }),

                        {ok, State#http_state{metrics = Metrics}};
                    {error, Reason} ->
                        %% Record error
                        erlmcp_otel:record_error(SpanCtx, {http_send_error, Reason}),

                        %% Update error metrics
                        Metrics = update_metrics(State#http_state.metrics, sent, {error, Reason}),

                        {error, {http_send_failed, Reason}, State#http_state{metrics = Metrics}}
                end
            catch
                Error:Reason ->
                    %% Record error
                    erlmcp_otel:record_error(SpanCtx, {http_send_exception, Error, Reason}),

                    %% Update error metrics
                    Metrics = update_metrics(State#http_state.metrics, sent, {error, Reason}),

                    {error, {http_send_failed, Reason}, State#http_state{metrics = Metrics}}
            end
    end.

%% @doc Close HTTP transport
-spec close(#http_state{}) -> ok.
close(State) ->
    %% Create OTEL span for HTTP close
    erlmcp_otel:with_span("cli.transport.http.close", #{
        <<"transport">> => <<"http">>,
        <<"session">> => State#http_state.session_id,
        <<"host">> => State#http_state.host,
        <<"port">> => State#http_state.port
    }, fun() ->
        %% Close HTTP server
        case State#http_state.pid of
            undefined ->
                ok;
            _ ->
                %% Stop cowboy server
                cowboy:stop(State#http_state.pid)
        end,

        %% Record close event
        erlmcp_otel:add_event(undefined, <<"http.closed">>, #{
            <<"session">> => State#http_state.session_id,
            <<"host">> => State#http_state.host,
            <<"port">> => State#http_state.port,
            <<"total_messages">> => maps:get("messages.total", State#http_state.metrics, 0)
        }),

        ok
    end).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the server
-spec init(term()) -> {ok, #http_state{}} | {stop, term()}.
init(Opts) ->
    %% Merge with default options
    FinalOpts = maps:merge(?DEFAULT_OPTS, Opts),

    %% Create OTEL span for HTTP transport initialization
    SpanCtx = erlmcp_otel:with_span("cli.transport.http.init", #{
        <<"transport">> => <<"http">>,
        <<"session">> => FinalOpts#{session_id},
        <<"host">> => FinalOpts#{host},
        <<"port">> => FinalOpts#{port}
    }, fun() ->
        ok
    end),

    %% Initialize state
    State = #http_state{
        transport_id = FinalOpts#{transport_id},
        session_id = FinalOpts#{session_id},
        host = FinalOpts#{host},
        port = FinalOpts#{port},
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

    %% Start HTTP server
    case start_http_server(State) of
        {ok, Pid} ->
            %% Record initialization metrics
            erlmcp_metrics:record("cli.transport.http.initialized", 1),

            {ok, State#http_state{pid = Pid}};
        {error, Reason} ->
            %% Record error
            erlmcp_otel:record_error(SpanCtx, {http_server_start_failed, Reason}),

            {stop, Reason}
    end.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #http_state{}) ->
    {reply, term(), #http_state{}} | {stop, term(), #http_state{}}.
handle_call({send_data, Data}, _From, State) ->
    %% Create OTEL span for HTTP send
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.http.send">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"http">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#http_state.session_id,
                                            <<"host">> => State#http_state.host,
                                            <<"port">> => State#http_state.port
                                        },
                                        undefined),

    %% Send HTTP request
    case State#http_state.pid of
        undefined ->
            %% HTTP server not started
            erlmcp_otel:record_error(SpanCtx, {http_server_not_started, State#http_state.pid}),

            %% Update error metrics
            Metrics = maps:update_with("errors.connection", fun(V) -> V + 1 end,
                                       State#http_state.metrics),

            Reply = {error, {http_server_not_started, State#http_state.pid}},
            NewState = State#http_state{metrics = Metrics};

        _ ->
            try
                %% Create HTTP request
                HttpRequest = create_http_request(Data, State),

                %% Send request
                case httpc:request(HttpRequest) of
                    {ok, {_, _, ResponseBody}} ->
                        %% Parse response
                        JsonResponse = erlmcp_json_native:decode(ResponseBody),

                        %% Forward to JSON-RPC handler if needed
                        case erlmcp_cli_json_rpc:handle_json_rpc(ResponseBody, #{}, State#http_state.session_id) of
                            {ok, Response} ->
                                lager:debug("HTTP JSON-RPC response: ~p", [Response]);
                            {error, Reason} ->
                                lager:warning("Failed to handle JSON-RPC: ~p", [Reason])
                        end,

                        %% Update metrics
                        Metrics = maps:update_with("messages.sent", fun(V) -> V + 1 end,
                                                   State#http_state.metrics),

                        %% Record send event
                        erlmcp_otel:add_event(SpanCtx, <<"http.sent">>, #{
                            <<"data.size">> => size(Data),
                            <<"response.size">> => size(ResponseBody),
                            <<"timestamp">> => erlang:system_time(millisecond)
                        }),

                        Reply = ok,
                        NewState = State#http_state{metrics = Metrics};

                    {error, Reason} ->
                        %% Record error
                        erlmcp_otel:record_error(SpanCtx, {http_send_error, Reason}),

                        %% Update error metrics
                        Metrics = maps:update_with("errors.send", fun(V) -> V + 1 end,
                                                   State#http_state.metrics),

                        Reply = {error, {http_send_failed, Reason}},
                        NewState = State#http_state{metrics = Metrics}
                end
            catch
                Error:Reason ->
                    %% Record error
                    erlmcp_otel:record_error(SpanCtx, {http_send_exception, Error, Reason}),

                    %% Update error metrics
                    Metrics = maps:update_with("errors.send", fun(V) -> V + 1 end,
                                               State#http_state.metrics),

                    Reply = {error, {http_send_failed, Reason}},
                    NewState = State#http_state{metrics = Metrics}
            end
    end,

    {reply, Reply, NewState};

handle_call(close, _From, State) ->
    %% Create OTEL span for HTTP close
    erlmcp_otel:with_span("cli.transport.http.close", #{
        <<"transport">> => <<"http">>,
        <<"session">> => State#http_state.session_id,
        <<"host">> => State#http_state.host,
        <<"port">> => State#http_state.port
    }, fun() ->
        ok
    end),

    %% Close HTTP server
    case State#http_state.pid of
        undefined ->
            ok;
        _ ->
            cowboy:stop(State#http_state.pid)
    end,

    %% Record close event
    erlmcp_otel:add_event(undefined, <<"http.closed">>, #{
        <<"session">> => State#http_state.session_id,
        <<"host">> => State#http_state.host,
        <<"port">> => State#http_state.port,
        <<"total_messages">> => maps:get("messages.total", State#http_state.metrics, 0)
    }),

    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #http_state{}) -> {noreply, #http_state{}} | {stop, term(), #http_state{}}.
handle_cast({received_data, Data}, State) ->
    %% Create OTEL span for data reception
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.http.receive">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"http">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#http_state.session_id,
                                            <<"host">> => State#http_state.host,
                                            <<"port">> => State#http_state.port
                                        },
                                        undefined),

    %% Process received data
    try
        %% Parse JSON-RPC request
        JsonData = erlmcp_json_native:decode(Data),

        %% Forward to JSON-RPC handler
        case erlmcp_cli_json_rpc:handle_json_rpc(Data, #{}, State#http_state.session_id) of
            {ok, Response} ->
                lager:debug("HTTP JSON-RPC response: ~p", [Response]);
            {error, Reason} ->
                lager:warning("Failed to handle JSON-RPC: ~p", [Reason])
        end,

        %% Update metrics
        Metrics = maps:update_with("messages.received", fun(V) -> V + 1 end,
                                   State#http_state.metrics),

        %% Record reception event
        erlmcp_otel:add_event(SpanCtx, <<"http.received">>, #{
            <<"data.size">> => size(Data),
            <<"timestamp">> => erlang:system_time(millisecond)
        }),

        {noreply, State#http_state{metrics = Metrics}}

    catch
        Error:Reason ->
            %% Record error
            erlmcp_otel:record_error(SpanCtx, {http_receive_error, Error, Reason}),

            %% Update error metrics
            Metrics = maps:update_with("errors.receive", fun(V) -> V + 1 end,
                                       State#http_state.metrics),

            {noreply, State#http_state{metrics = Metrics}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #http_state{}) -> {noreply, #http_state{}} | {stop, term(), #http_state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), #http_state{}) -> ok.
terminate(_Reason, State) ->
    %% Close HTTP server if running
    case State#http_state.pid of
        undefined ->
            ok;
        _ ->
            cowboy:stop(State#http_state.pid)
    end,

    %% Send final metrics
    erlmcp_metrics:record("cli.transport.http.terminated", 1),

    ok.

%% @doc Handle code changes
-spec code_change(term(), #http_state{}, term()) -> {ok, #http_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Start HTTP server
-spec start_http_server(#http_state{}) -> {ok, pid()} | {error, term()}.
start_http_server(State) ->
    %% Cowboy routing configuration
    Routes = [
        {"/", erlmcp_http_handler, []},
        {"/ws", erlmcp_ws_handler, []}
    ],

    %% Cowboy listener options
    TransOpts = #{
        num_acceptors => 10,
        max_connections => 1000,
        shutdown => 5000,
        websocket => true,
        protocol => http,
        ip => {0, 0, 0, 0},
        port => State#http_state.port
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
    case cowboy:start_clear(erlmcp_http_listener, TransOpts, CowboyOpts) of
        {ok, Pid} ->
            lager:info("HTTP server started on ~s:~p", [State#http_state.host, State#http_state.port]),
            {ok, Pid};
        {error, Reason} ->
            lager:error("Failed to start HTTP server: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc Create HTTP request
-spec create_http_request(binary(), #http_state{}) -> {httpc, {string(), list()}}.
create_http_request(Data, State) ->
    %% Prepare JSON-RPC request
    JsonRequest = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"cli.transport.http">>,
        <<"params">> => #{<<"data">> => Data},
        <<"id">> => erlmcp_utils:generate_id()
    }),

    %% HTTP headers
    Headers = [
        {"Content-Type", "application/json"},
        {"User-Agent", "erlmcp-cli/2.1.0"},
        {"X-Session-ID", State#http_state.session_id}
    ],

    %% HTTP request
    Request = {
        post,
        "/api/cli",
        Headers,
        "application/json",
        JsonRequest
    },

    FullUrl = "http://" ++ State#http_state.host ++ ":" ++ integer_to_list(State#http_state.port) ++ "/api/cli",

    {httpc, FullUrl, Request}.

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