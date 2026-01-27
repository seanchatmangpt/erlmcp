-module(erlmcp_transport_ws).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-behavior(erlmcp_transport).

%% Transport behavior
-export([
    init/2,
    send/2,
    close/1
]).

%% WebSocket handler exports
-export([
    websocket_handle/2,
    websocket_info/2
]).

%% Internal/test exports
-export([
    validate_utf8/1,
    validate_message_size/1,
    process_messages/2,
    generate_session_id/0
]).

%% Configuration constants
-define(PING_INTERVAL, 30000). %% 30 seconds
-define(IDLE_TIMEOUT, 300000). %% 5 minutes
-define(DEFAULT_MAX_MESSAGE_SIZE, 16777216). %% 16MB
-define(MESSAGE_DELIMITER, <<"\n">>).
-define(FRAGMENT_TIMEOUT, 30000). %% 30 seconds for fragment reassembly

%% WebSocket close codes (RFC 6455)
-define(WS_CLOSE_NORMAL, 1000).
-define(WS_CLOSE_PROTOCOL_ERROR, 1002).
-define(WS_CLOSE_MESSAGE_TOO_BIG, 1009).

-record(state, {
    transport_id :: binary(),
    registry_pid :: pid(),
    connection_info :: map(),
    session_id :: binary(),
    ping_timer :: reference() | undefined,
    fragment_buffer :: binary() | undefined,
    fragment_start_time :: integer() | undefined,
    max_message_size :: integer(),
    strict_delimiter_check :: boolean(),
    validate_utf8 :: boolean()
}).

%%====================================================================
%% Transport Behavior Implementation
%%====================================================================

-spec init(binary(), map()) -> {ok, pid()} | {error, term()}.
init(TransportId, Config) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_ws.init">>),
    try
        Port = maps:get(port, Config, 8080),
        Path = maps:get(path, Config, "/mcp/ws"),
        MaxMessageSize = maps:get(max_message_size, Config, ?DEFAULT_MAX_MESSAGE_SIZE),
        StrictDelimiterCheck = maps:get(strict_delimiter_check, Config, true),
        ValidateUtf8 = maps:get(validate_utf8, Config, true),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"transport_id">> => TransportId,
            <<"port">> => Port,
            <<"path">> => Path,
            <<"max_message_size">> => MaxMessageSize,
            <<"strict_delimiter_check">> => StrictDelimiterCheck,
            <<"validate_utf8">> => ValidateUtf8
        }),

        Dispatch = cowboy_router:compile([
            {'_', [
                {Path, ?MODULE, [TransportId, Config]}
            ]}
        ]),

        {ok, _} = cowboy:start_clear(erlmcp_ws_listener,
            [{port, Port}],
            #{env => #{dispatch => Dispatch}}),

        erlmcp_tracing:set_status(SpanCtx, ok),
        {ok, self()}
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

-spec send(pid(), binary()) -> ok | {error, term()}.
send(Handler, Data) when is_pid(Handler), is_binary(Data) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_ws.send">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"data_size">> => byte_size(Data)
        }),

        Handler ! {send_frame, Data},
        erlmcp_tracing:set_status(SpanCtx, ok),
        ok
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

send(_Handler, _Data) ->
    {error, invalid_arguments}.

-spec close(pid()) -> ok.
close(Handler) when is_pid(Handler) ->
    Handler ! close,
    ok;

close(_) ->
    ok.

%%====================================================================
%% Cowboy WebSocket Handler
%%====================================================================

init(Req, [TransportId, Config], _Opts) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_ws.cowboy_init">>),

    MaxMessageSize = maps:get(max_message_size, Config, ?DEFAULT_MAX_MESSAGE_SIZE),
    StrictDelimiterCheck = maps:get(strict_delimiter_check, Config, true),
    ValidateUtf8 = maps:get(validate_utf8, Config, true),
    SessionId = generate_session_id(),

    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"transport_id">> => TransportId,
        <<"session_id">> => SessionId,
        <<"max_message_size">> => MaxMessageSize
    }),

    {cowboy_websocket, Req, #state{
        transport_id = TransportId,
        registry_pid = erlmcp_registry:get_pid(),
        session_id = SessionId,
        fragment_buffer = undefined,
        fragment_start_time = undefined,
        max_message_size = MaxMessageSize,
        strict_delimiter_check = StrictDelimiterCheck,
        validate_utf8 = ValidateUtf8
    }, #{idle_timeout => ?IDLE_TIMEOUT}}.

websocket_handle({text, Data}, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_ws.handle_text_message">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"data_size">> => byte_size(Data),
            <<"session_id">> => State#state.session_id
        }),

        %% Check message size limit first
        case validate_message_size(Data) of
            {ok, _} ->
                %% Handle fragmented or complete messages
                case handle_text_frame(Data, State) of
                    {ok, NewState} ->
                        erlmcp_tracing:set_status(SpanCtx, ok),
                        {ok, NewState};
                    {error, Reason, NewState} ->
                        erlmcp_tracing:record_error_details(SpanCtx, Reason, Data),
                        close_with_error(Reason, NewState)
                end;
            {error, too_big} ->
                erlmcp_tracing:record_error_details(SpanCtx, message_too_big, Data),
                close_with_error(message_too_big, State)
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {reply, {close, ?WS_CLOSE_PROTOCOL_ERROR, <<"Internal error">>}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

websocket_handle({binary, _Data}, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_ws.handle_binary">>),
    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"session_id">> => State#state.session_id
    }),
    erlmcp_tracing:record_error_details(SpanCtx, binary_frame_not_supported, <<>>),
    erlmcp_tracing:end_span(SpanCtx),
    {reply, {close, ?WS_CLOSE_PROTOCOL_ERROR, <<"Binary frames not supported">>}, State};

websocket_handle(ping, State) ->
    {reply, pong, State};

websocket_handle(pong, State) ->
    {ok, State};

websocket_handle({close, Code, Reason}, State) ->
    logger:info("WebSocket closed by client: code=~p reason=~p", [Code, Reason]),
    {ok, State};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({send_frame, Data}, State) ->
    {reply, {text, Data}, State};

websocket_info(ping, State) ->
    {reply, ping, State};

websocket_info(close, State) ->
    {stop, State};

websocket_info(_Info, State) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Handle text frame with newline delimiter validation
-spec handle_text_frame(binary(), #state{}) -> {ok, #state{}} | {error, atom(), #state{}}.
handle_text_frame(Data, State) ->
    %% Check if this is a fragment continuation
    case State#state.fragment_buffer of
        undefined ->
            %% New message(s)
            process_messages(Data, State);
        Buffer ->
            %% Continue fragment reassembly
            reassemble_fragment(<<Buffer/binary, Data/binary>>, State)
    end.

%% Process potentially multiple newline-delimited messages
-spec process_messages(binary(), #state{}) -> {ok, #state{}} | {error, atom(), #state{}}.
process_messages(Data, State) ->
    case State#state.strict_delimiter_check of
        true ->
            %% Split by newline and validate each message ends with \n
            Lines = binary:split(Data, ?MESSAGE_DELIMITER, [global]),
            process_lines(Lines, State, []);
        false ->
            %% Lenient mode: process all content
            Lines = binary:split(Data, ?MESSAGE_DELIMITER, [global]),
            process_lines(Lines, State, [])
    end.

%% Process individual lines (messages)
-spec process_lines([binary()], #state{}, [binary()]) -> {ok, #state{}} | {error, atom(), #state{}}.
process_lines([], State, _Processed) ->
    {ok, State};

process_lines([<<>>], State, _Processed) ->
    %% Last empty line after final delimiter is normal
    {ok, State};

process_lines([LastLine], State, _Processed) when State#state.strict_delimiter_check ->
    %% Last line without trailing newline in strict mode - buffer it
    NewState = State#state{fragment_buffer = LastLine, fragment_start_time = erlang:monotonic_time()},
    {ok, NewState};

process_lines([Line | Rest], State, Processed) ->
    case process_single_message(Line, State) of
        {ok, NewState} ->
            process_lines(Rest, NewState, [Line | Processed]);
        {error, Reason} ->
            {error, Reason, State}
    end.

%% Process a single message (after delimiter validation)
-spec process_single_message(binary(), #state{}) -> {ok, #state{}} | {error, atom()}.
process_single_message(<<>>, State) ->
    {ok, State};

process_single_message(Message, State) ->
    %% Validate UTF-8
    case State#state.validate_utf8 of
        true ->
            case validate_utf8(Message) of
                ok ->
                    parse_and_route(Message, State);
                {error, invalid_utf8} ->
                    {error, invalid_utf8}
            end;
        false ->
            parse_and_route(Message, State)
    end.

%% Parse and route message to registry
-spec parse_and_route(binary(), #state{}) -> {ok, #state{}} | {error, atom()}.
parse_and_route(Message, State) ->
    case jsx:decode(Message, [return_maps]) of
        {error, _} ->
            logger:warning("JSON parse error in WebSocket message: ~p", [Message]),
            {error, parse_error};
        ParsedMessage ->
            %% Route to registry
            State#state.registry_pid ! {transport_data, State#state.transport_id, ParsedMessage},
            {ok, State}
    end.

%% Reassemble fragmented message
-spec reassemble_fragment(binary(), #state{}) -> {ok, #state{}} | {error, atom(), #state{}}.
reassemble_fragment(BufferedData, State) ->
    case check_fragment_timeout(State) of
        ok ->
            %% Check if we have complete message now
            case binary:match(BufferedData, ?MESSAGE_DELIMITER) of
                nomatch ->
                    %% Still incomplete, keep buffering
                    {ok, State#state{fragment_buffer = BufferedData}};
                _ ->
                    %% Complete message received, process it
                    process_messages(BufferedData, State#state{fragment_buffer = undefined})
            end;
        {error, timeout} ->
            logger:error("WebSocket fragment timeout after ~pms", [?FRAGMENT_TIMEOUT]),
            {error, fragment_timeout, State}
    end.

%% Check if fragment reassembly has timed out
-spec check_fragment_timeout(#state{}) -> ok | {error, timeout}.
check_fragment_timeout(State) ->
    case State#state.fragment_start_time of
        undefined ->
            ok;
        StartTime ->
            Elapsed = erlang:monotonic_time() - StartTime,
            ElapsedMs = erlang:convert_time_unit(Elapsed, native, millisecond),
            case ElapsedMs > ?FRAGMENT_TIMEOUT of
                true -> {error, timeout};
                false -> ok
            end
    end.

%% Validate message size
-spec validate_message_size(binary()) -> {ok, integer()} | {error, too_big}.
validate_message_size(Data) ->
    MaxSize = application:get_env(erlmcp, max_ws_message_size, ?DEFAULT_MAX_MESSAGE_SIZE),
    Size = byte_size(Data),
    case Size =< MaxSize of
        true -> {ok, Size};
        false -> {error, too_big}
    end.

%% Validate UTF-8 encoding
-spec validate_utf8(binary()) -> ok | {error, invalid_utf8}.
validate_utf8(Data) ->
    case unicode:characters_to_list(Data, utf8) of
        {error, _, _} ->
            {error, invalid_utf8};
        {incomplete, _, _} ->
            {error, invalid_utf8};
        _ ->
            ok
    end.

%% Generate unique session ID
-spec generate_session_id() -> binary().
generate_session_id() ->
    Base64 = base64:encode(crypto:strong_rand_bytes(32)),
    Base64.

%% Close connection with error code
-spec close_with_error(atom(), #state{}) -> {reply, {close, integer(), binary()}, #state{}}.
close_with_error(message_too_big, State) ->
    MaxSize = application:get_env(erlmcp, max_ws_message_size, ?DEFAULT_MAX_MESSAGE_SIZE),
    Reason = erlang:iolist_to_binary(
        io_lib:format("Message exceeds maximum size of ~p bytes", [MaxSize])
    ),
    {reply, {close, ?WS_CLOSE_MESSAGE_TOO_BIG, Reason}, State};

close_with_error(invalid_utf8, State) ->
    {reply, {close, ?WS_CLOSE_PROTOCOL_ERROR, <<"Invalid UTF-8 encoding">>}, State};

close_with_error(parse_error, State) ->
    {reply, {close, ?WS_CLOSE_PROTOCOL_ERROR, <<"JSON-RPC parse error">>}, State};

close_with_error(fragment_timeout, State) ->
    {reply, {close, ?WS_CLOSE_PROTOCOL_ERROR, <<"Fragment reassembly timeout">>}, State};

close_with_error(_Reason, State) ->
    {reply, {close, ?WS_CLOSE_PROTOCOL_ERROR, <<"Protocol error">>}, State}.
