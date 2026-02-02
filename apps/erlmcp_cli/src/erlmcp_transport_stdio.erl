%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_transport_stdio - Stdio Transport Implementation
%%%
%%% Implements stdio transport for CLI communications using
%%% process_group_leader for bidirectional communication.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_stdio).

-behaviour(erlmcp_transport).

%% API
-export([start_link/1, send/1, close/1]).

%% erlmcp_transport callbacks
-export([init/2, send/2, close/1]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(stdio_state, {
    transport_id :: binary(),
    session_id :: binary(),
    input_stream :: pid() | undefined,
    output_stream :: pid(),
    metrics :: map()
}).

-define(SERVER, ?MODULE).
-define(DEFAULT_OPTS, #{
    session_id => erlmcp_utils:generate_id(),
    transport_id => erlmcp_utils:generate_id()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start stdio transport
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Send data through stdio
-spec send(binary()) -> {ok, term()} | {error, term()}.
send(Data) ->
    gen_server:call(?SERVER, {send_data, Data}).

%% @doc Close stdio transport
-spec close() -> ok.
close() ->
    gen_server:call(?SERVER, close).

%%====================================================================
%% erlmcp_transport callbacks
%%====================================================================

%% @doc Initialize stdio transport
-spec init(atom(), map()) -> {ok, term()} | {error, term()}.
init(TransportType, Opts) when TransportType =:= stdio ->
    case start_link(Opts) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc Send data through stdio
-spec send(binary(), #stdio_state{}) -> {ok, #stdio_state{}} | {error, term(), #stdio_state{}}.
send(Data, State) ->
    %% Create OTEL span for stdio send
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.stdio.send">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"stdio">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#stdio_state.session_id
                                        },
                                        undefined),

    %% Send data to stdout
    try
        io:format("~s~n", [Data]),

        %% Record success metrics
        Metrics = update_metrics(State#stdio_state.metrics, sent, ok),

        %% Record send event
        erlmcp_otel:add_event(SpanCtx, <<"stdio.sent">>, #{
            <<"data.size">> => size(Data),
            <<"timestamp">> => erlang:system_time(millisecond)
        }),

        {ok, State#stdio_state{metrics = Metrics}}
    catch
        Error:Reason ->
            %% Record error
            erlmcp_otel:record_error(SpanCtx, {stdio_send_error, Error, Reason}),

            %% Update error metrics
            Metrics = update_metrics(State#stdio_state.metrics, sent, {error, Reason}),

            {error, {stdio_send_failed, Reason}, State#stdio_state{metrics = Metrics}}
    end.

%% @doc Close stdio transport
-spec close(#stdio_state{}) -> ok.
close(State) ->
    %% Create OTEL span for stdio close
    erlmcp_otel:with_span("cli.transport.stdio.close", #{
        <<"transport">> => <<"stdio">>,
        <<"session">> => State#stdio_state.session_id
    }, fun() ->
        %% Send close message
        io:format("~n", []),

        %% Record close event
        erlmcp_otel:add_event(undefined, <<"stdio.closed">>, #{
            <<"session">> => State#stdio_state.session_id,
            <<"total_messages">> => maps:get("messages.total", State#stdio_state.metrics, 0)
        }),

        ok
    end).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the server
-spec init(term()) -> {ok, #stdio_state{}} | {stop, term()}.
init(Opts) ->
    %% Merge with default options
    FinalOpts = maps:merge(?DEFAULT_OPTS, Opts),

    %% Create OTEL span for stdio transport initialization
    SpanCtx = erlmcp_otel:with_span("cli.transport.stdio.init", #{
        <<"transport">> => <<"stdio">>,
        <<"session">> => FinalOpts#{session_id}
    }, fun() ->
        ok
    end),

    %% Start input stream reader
    InputPid = case start_input_stream(FinalOpts#{session_id}) of
        {ok, Pid} -> Pid;
        {error, Reason} -> undefined
    end,

    %% Initialize state
    State = #stdio_state{
        transport_id = FinalOpts#{transport_id},
        session_id = FinalOpts#{session_id},
        input_stream = InputPid,
        output_stream = self(),
        metrics = #{
            "messages.sent" => 0,
            "messages.received" => 0,
            "errors.send" => 0,
            "errors.receive" => 0,
            "session.start" => erlang:system_time(millisecond)
        }
    },

    %% Record initialization metrics
    erlmcp_metrics:record("cli.transport.stdio.initialized", 1),

    {ok, State}.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #stdio_state{}) ->
    {reply, term(), #stdio_state{}} | {stop, term(), #stdio_state{}}.
handle_call({send_data, Data}, _From, State) ->
    %% Create OTEL span for stdio send
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.stdio.send">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"stdio">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#stdio_state.session_id
                                        },
                                        undefined),

    %% Send data to stdout
    try
        io:format("~s~n", [Data]),

        %% Update metrics
        Metrics = maps:update_with("messages.sent", fun(V) -> V + 1 end,
                                   State#stdio_state.metrics),

        %% Record send event
        erlmcp_otel:add_event(SpanCtx, <<"stdio.sent">>, #{
            <<"data.size">> => size(Data),
            <<"timestamp">> => erlang:system_time(millisecond)
        }),

        Reply = ok,
        NewState = State#stdio_state{metrics = Metrics};

    catch
        Error:Reason ->
            %% Record error
            erlmcp_otel:record_error(SpanCtx, {stdio_send_error, Error, Reason}),

            %% Update error metrics
            Metrics = maps:update_with("errors.send", fun(V) -> V + 1 end,
                                       State#stdio_state.metrics),

            Reply = {error, {stdio_send_failed, Reason}},
            NewState = State#stdio_state{metrics = Metrics}
    end,

    {reply, Reply, NewState};

handle_call(close, _From, State) ->
    %% Create OTEL span for stdio close
    erlmcp_otel:with_span("cli.transport.stdio.close", #{
        <<"transport">> => <<"stdio">>,
        <<"session">> => State#stdio_state.session_id
    }, fun() ->
        ok
    end),

    %% Send close message
    io:format("~n", []),

    %% Close input stream if running
    case State#stdio_state.input_stream of
        undefined ->
            ok;
        InputPid ->
            gen_server:stop(InputPid)
    end,

    %% Record close event
    erlmcp_otel:add_event(undefined, <<"stdio.closed">>, #{
        <<"session">> => State#stdio_state.session_id,
        <<"total_messages">> => maps:get("messages.total", State#stdio_state.metrics, 0)
    }),

    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #stdio_state{}) -> {noreply, #stdio_state{}} | {stop, term(), #stdio_state{}}.
handle_cast({received_data, Data}, State) ->
    %% Create OTEL span for data reception
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.transport.stdio.receive">>,
                                        make_request_id(),
                                        #{
                                            <<"transport">> => <<"stdio">>,
                                            <<"data.size">> => size(Data),
                                            <<"session">> => State#stdio_state.session_id
                                        },
                                        undefined),

    %% Process received data
    try
        %% Parse JSON-RPC request
        JsonData = jsx:decode(Data, [{labels, binary}]),

        %% Forward to JSON-RPC handler
        case erlmcp_cli_json_rpc:handle_json_rpc(Data, #{}, State#stdio_state.session_id) of
            {ok, Response} ->
                %% Send response back to stdout
                io:format("~s~n", [jsx:encode(Response)]);
            {error, Reason} ->
                %% Log error
                lager:warning("Failed to handle JSON-RPC: ~p", [Reason])
        end,

        %% Update metrics
        Metrics = maps:update_with("messages.received", fun(V) -> V + 1 end,
                                   State#stdio_state.metrics),

        %% Record reception event
        erlmcp_otel:add_event(SpanCtx, <<"stdio.received">>, #{
            <<"data.size">> => size(Data),
            <<"timestamp">> => erlang:system_time(millisecond)
        }),

        {noreply, State#stdio_state{metrics = Metrics}}

    catch
        Error:Reason ->
            %% Record error
            erlmcp_otel:record_error(SpanCtx, {stdio_receive_error, Error, Reason}),

            %% Update error metrics
            Metrics = maps:update_with("errors.receive", fun(V) -> V + 1 end,
                                       State#stdio_state.metrics),

            {noreply, State#stdio_state{metrics = Metrics}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #stdio_state{}) -> {noreply, #stdio_state{}} | {stop, term(), #stdio_state{}}.
handle_info({'EXIT', Pid, Reason}, State) ->
    %% Handle input stream death
    case Pid of
        State#stdio_state.input_stream ->
            lager:warning("Input stream died: ~p", [Reason]),
            {noreply, State#stdio_state{input_stream = undefined}};
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), #stdio_state{}) -> ok.
terminate(_Reason, State) ->
    %% Close input stream if running
    case State#stdio_state.input_stream of
        undefined ->
            ok;
        InputPid ->
            gen_server:stop(InputPid)
    end,

    %% Send final metrics
    erlmcp_metrics:record("cli.transport.stdio.terminated", 1),

    ok.

%% @doc Handle code changes
-spec code_change(term(), #stdio_state{}, term()) -> {ok, #stdio_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Start input stream reader
-spec start_input_stream(map()) -> {ok, pid()} | {error, term()}.
start_input_stream(SessionId) ->
    spawn_link(fun() -> input_reader_loop(SessionId) end).

%% @doc Input reader loop
-spec input_reader_loop(binary()) -> no_return().
input_reader_loop(SessionId) ->
    %% Create OTEL span for input reader
    SpanCtx = erlmcp_otel:with_span("cli.transport.stdio.input_reader", #{
        <<"session">> => SessionId
    }, fun() ->
        ok
    end),

    input_reader_loop(SessionId, SpanCtx).

%% @doc Input reader loop with span
-spec input_reader_loop(binary(), term()) -> no_return().
input_reader_loop(SessionId, SpanCtx) ->
    case io:get_line("") of
        eof ->
            %% End of input
            erlmcp_otel:add_event(SpanCtx, <<"input_reader.eof">>, #{
                <<"session">> => SessionId
            });
        {error, Reason} ->
            %% Input error
            erlmcp_otel:record_error(SpanCtx, {input_error, Reason});
        Line ->
            %% Send data to parent
            case erlang:self() of
                Parent when is_pid(Parent) ->
                    gen_server:cast(Parent, {received_data, Line}),
                    input_reader_loop(SessionId, SpanCtx);
                _ ->
                    %% Parent process died, exit
                    ok
            end
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

    Updated2.

%% @doc Make request ID
-spec make_request_id() -> binary().
make_request_id() ->
    Id = crypto:strong_rand_bytes(16),
    base64:encode(Id).