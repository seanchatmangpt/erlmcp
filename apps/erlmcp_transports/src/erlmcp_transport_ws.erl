-module(erlmcp_transport_ws).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% Note: This module does NOT implement erlmcp_transport_behavior
%% It is a Cowboy WebSocket handler with its own init/2 interface

%% WebSocket-specific exports (NOT erlmcp_transport_behavior callbacks)
-export([
    init/2,
    send/2,
    close/1
]).

%% WebSocket handler exports
-export([
    init/3,  % Cowboy WebSocket handler callback
    websocket_handle/2,
    websocket_info/2
]).

%% Internal/test exports
-export([
    validate_utf8/1,
    validate_message_size/1,
    process_messages/2,
    generate_session_id/0,
    check_backpressure/1,
    update_buffer_usage/3,
    resume_reading/1,
    handle_ping_frame/2,
    handle_pong_frame/2,
    handle_close_frame/3
]).

%% Configuration constants
-define(PING_INTERVAL, 30000). %% 30 seconds
-define(IDLE_TIMEOUT, 300000). %% 5 minutes
-define(DEFAULT_MAX_MESSAGE_SIZE, 16777216). %% 16MB
-define(MESSAGE_DELIMITER, <<"\n">>).
-define(FRAGMENT_TIMEOUT, 30000). %% 30 seconds for fragment reassembly

%% Backpressure and flow control
-define(DEFAULT_FRAME_BUFFER_SIZE, 102400). %% 100KB default buffer
-define(BUFFER_DRAIN_THRESHOLD, 0.5). %% Resume at 50% of max
-define(BACKPRESSURE_TIMEOUT, 5000). %% 5 second backpressure timeout

%% WebSocket close codes (RFC 6455)
-define(WS_CLOSE_NORMAL, 1000).
-define(WS_CLOSE_GOING_AWAY, 1001).
-define(WS_CLOSE_PROTOCOL_ERROR, 1002).
-define(WS_CLOSE_UNSUPPORTED_DATA, 1003).
-define(WS_CLOSE_MESSAGE_TOO_BIG, 1009).
-define(WS_CLOSE_INTERNAL_ERROR, 1011).

%% Backpressure states
-define(BACKPRESSURE_INACTIVE, inactive).
-define(BACKPRESSURE_ACTIVE, active).

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
    validate_utf8 :: boolean(),
    %% Backpressure and flow control fields
    frame_buffer_size :: integer(),
    frame_buffer_used :: integer(),
    backpressure_state :: atom(),
    backpressure_timer :: reference() | undefined,
    messages_pending :: non_neg_integer(),
    bytes_buffered :: non_neg_integer(),
    %% Statistics
    messages_received = 0 :: non_neg_integer(),
    messages_sent = 0 :: non_neg_integer(),
    bytes_received = 0 :: non_neg_integer(),
    bytes_sent = 0 :: non_neg_integer(),
    ping_count = 0 :: non_neg_integer(),
    pong_count = 0 :: non_neg_integer(),
    connection_start_time :: integer()
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
        MaxConnections = maps:get(max_connections, Config, 1000),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"transport_id">> => TransportId,
            <<"port">> => Port,
            <<"path">> => Path,
            <<"max_message_size">> => MaxMessageSize,
            <<"max_connections">> => MaxConnections,
            <<"strict_delimiter_check">> => StrictDelimiterCheck,
            <<"validate_utf8">> => ValidateUtf8
        }),

        Dispatch = cowboy_router:compile([
            {'_', [
                {Path, ?MODULE, [TransportId, Config]}
            ]}
        ]),

        %% Cowboy listener configuration
        %% Generate unique listener name per transport ID to avoid conflicts
        %% Handle both atom and binary TransportId
        TransportIdBin = case is_binary(TransportId) of
            true -> TransportId;
            false when is_atom(TransportId) -> atom_to_binary(TransportId, utf8);
            false when is_list(TransportId) -> list_to_binary(TransportId)
        end,
        ListenerName = binary_to_atom(<<"erlmcp_ws_", TransportIdBin/binary>>, utf8),

        %% Ranch 2.x options: port, num_acceptors, max_connections, socket_opts
        %% For Cowboy 2.10, pass socket opts separately from ranch opts
        {ok, _} = cowboy:start_clear(ListenerName,
            [{port, Port}, {num_acceptors, 100}],
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
    FrameBufferSize = maps:get(frame_buffer_size, Config, ?DEFAULT_FRAME_BUFFER_SIZE),
    SessionId = generate_session_id(),
    ConnectionStartTime = erlang:system_time(millisecond),

    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"transport_id">> => TransportId,
        <<"session_id">> => SessionId,
        <<"max_message_size">> => MaxMessageSize,
        <<"frame_buffer_size">> => FrameBufferSize
    }),

    %% Collect connection info
    PeerAddr = cowboy_req:peer(Req),
    % Note: cowboy_req:certificates/1 not available in this version
    Certificates = undefined,
    ConnectionInfo = #{
        peer => PeerAddr,
        certificates => Certificates,
        path => cowboy_req:path(Req),
        qs => cowboy_req:qs(Req),
        headers => cowboy_req:headers(Req)
    },

    %% Start ping timer for keepalive
    PingTimer = erlang:send_after(?PING_INTERVAL, self(), send_ping),

    {cowboy_websocket, Req, #state{
        transport_id = TransportId,
        registry_pid = erlmcp_registry:get_pid(),
        session_id = SessionId,
        connection_info = ConnectionInfo,
        ping_timer = PingTimer,
        fragment_buffer = undefined,
        fragment_start_time = undefined,
        max_message_size = MaxMessageSize,
        strict_delimiter_check = StrictDelimiterCheck,
        validate_utf8 = ValidateUtf8,
        frame_buffer_size = FrameBufferSize,
        frame_buffer_used = 0,
        backpressure_state = ?BACKPRESSURE_INACTIVE,
        backpressure_timer = undefined,
        messages_pending = 0,
        bytes_buffered = 0,
        connection_start_time = ConnectionStartTime
    }, #{idle_timeout => ?IDLE_TIMEOUT}}.

websocket_handle({text, Data}, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_ws.handle_text_message">>),
    try
        DataSize = byte_size(Data),
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"data_size">> => DataSize,
            <<"session_id">> => State#state.session_id,
            <<"backpressure_state">> => State#state.backpressure_state
        }),

        %% Update statistics
        NewMessagesReceived = State#state.messages_received + 1,
        NewBytesReceived = State#state.bytes_received + DataSize,

        %% Check for backpressure conditions
        case check_backpressure(State) of
            {ok, NewState1} ->
                %% Check message size limit first
                case validate_message_size(Data) of
                    {ok, _} ->
                        %% Update buffer usage tracking
                        NewState2 = update_buffer_usage(NewState1, DataSize, add),
                        %% Handle fragmented or complete messages
                        case handle_text_frame(Data, NewState2) of
                            {ok, NewState3} ->
                                FinalState = NewState3#state{
                                    messages_received = NewMessagesReceived,
                                    bytes_received = NewBytesReceived
                                },
                                erlmcp_tracing:set_status(SpanCtx, ok),
                                {ok, FinalState};
                            {error, Reason, NewState3} ->
                                erlmcp_tracing:record_error_details(SpanCtx, Reason, Data),
                                close_with_error(Reason, NewState3)
                        end;
                    {error, too_big} ->
                        erlmcp_tracing:record_error_details(SpanCtx, message_too_big, Data),
                        close_with_error(message_too_big, NewState1)
                end;
            {error, backpressure_active, NewState1} ->
                erlmcp_tracing:record_error_details(SpanCtx, backpressure_active, Data),
                close_with_error(backpressure_failed, NewState1)
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
    {reply, {close, ?WS_CLOSE_UNSUPPORTED_DATA, <<"Binary frames not supported">>}, State};

%% Handle ping frames (RFC 6455)
websocket_handle(ping, State) ->
    handle_ping_frame(<<>>, State);

websocket_handle({ping, Payload}, State) when is_binary(Payload) ->
    handle_ping_frame(Payload, State);

%% Handle pong frames (RFC 6455) - response to our pings
websocket_handle(pong, State) ->
    handle_pong_frame(<<>>, State);

websocket_handle({pong, Payload}, State) when is_binary(Payload) ->
    handle_pong_frame(Payload, State);

%% Handle close frames (RFC 6455)
websocket_handle({close, <<>>}, State) ->
    %% Close without status code
    handle_close_frame(undefined, undefined, State);

websocket_handle({close, <<Code:16>>}, State) when Code >= 1000, Code =< 4999 ->
    %% Close with status code only
    handle_close_frame(Code, undefined, State);

websocket_handle({close, <<Code:16, Reason/binary>>}, State) when Code >= 1000, Code =< 4999 ->
    %% Close with status code and reason
    handle_close_frame(Code, Reason, State);

websocket_handle({close, _Payload}, State) ->
    %% Invalid close frame
    logger:warning("Received invalid close frame from client"),
    {reply, {close, ?WS_CLOSE_PROTOCOL_ERROR, <<"Invalid close frame">>}, State};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({send_frame, Data}, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_ws.send_frame">>),
    try
        DataSize = byte_size(Data),
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"data_size">> => DataSize
        }),

        %% Update statistics
        NewMessagesSent = State#state.messages_sent + 1,
        NewBytesSent = State#state.bytes_sent + DataSize,

        %% Update buffer usage on send
        NewState = update_buffer_usage(State, DataSize, subtract),

        %% Check if we should resume reading after backpressure
        ResumeState = resume_reading(NewState),

        FinalState = ResumeState#state{
            messages_sent = NewMessagesSent,
            bytes_sent = NewBytesSent
        },

        {reply, {text, Data}, FinalState}
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            {reply, {text, Data}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

%% Send periodic ping for keepalive
websocket_info(send_ping, State) ->
    %% Send ping frame
    NewPingCount = State#state.ping_count + 1,
    %% Reschedule ping timer
    NewPingTimer = erlang:send_after(?PING_INTERVAL, self(), send_ping),
    {reply, ping, State#state{ping_timer = NewPingTimer, ping_count = NewPingCount}};

%% Resume reading after backpressure timeout
websocket_info(resume_reading, State) ->
    NewState = State#state{
        backpressure_state = ?BACKPRESSURE_INACTIVE,
        backpressure_timer = undefined
    },
    {ok, NewState};

%% Close connection
websocket_info(close, State) ->
    %% Cancel backpressure timer if active
    case State#state.backpressure_timer of
        undefined -> ok;
        TimerRef -> erlang:cancel_timer(TimerRef)
    end,
    %% Cancel ping timer if active
    case State#state.ping_timer of
        undefined -> ok;
        PingTimerRef -> erlang:cancel_timer(PingTimerRef)
    end,
    {stop, State};

websocket_info(_Info, State) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Frame Handling
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
    case jsx:is_json(Message) of
        true ->
            case jsx:decode(Message, [return_maps]) of
                {error, Reason} ->
                    logger:warning("JSON parse error in WebSocket message: ~p, reason: ~p", [Message, Reason]),
                    {error, parse_error};
                ParsedMessage ->
                    %% Route to registry
                    State#state.registry_pid ! {transport_data, State#state.transport_id, ParsedMessage},
                    {ok, State}
            end;
        false ->
            logger:warning("Invalid JSON in WebSocket message: ~p", [Message]),
            {error, parse_error}
    end.

%% Reassemble fragmented message
-spec reassemble_fragment(binary(), #state{}) -> {ok, #state{}} | {error, atom(), #state{}}.
reassemble_fragment(BufferedData, State) ->
    %% Check if buffered data exceeds max message size
    case validate_message_size(BufferedData) of
        {error, too_big} ->
            logger:error("Fragmented message exceeds maximum size"),
            {error, message_too_big, State#state{fragment_buffer = undefined}};
        {ok, _} ->
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
                    {error, fragment_timeout, State#state{fragment_buffer = undefined}}
            end
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

%%====================================================================
%% Internal Functions - Frame Type Handlers
%%====================================================================

%% Handle ping frame (RFC 6455 Section 5.5.2)
-spec handle_ping_frame(binary(), #state{}) -> {reply, pong | {pong, binary()}, #state{}}.
handle_ping_frame(Payload, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_ws.handle_ping">>),
    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"payload_size">> => byte_size(Payload),
        <<"session_id">> => State#state.session_id
    }),

    logger:debug("WebSocket ping received, sending pong response"),

    %% RFC 6455: Respond with pong frame containing same payload
    Reply = case Payload of
        <<>> -> pong;
        _ -> {pong, Payload}
    end,

    erlmcp_tracing:end_span(SpanCtx),
    {reply, Reply, State}.

%% Handle pong frame (RFC 6455 Section 5.5.3)
-spec handle_pong_frame(binary(), #state{}) -> {ok, #state{}}.
handle_pong_frame(Payload, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_ws.handle_pong">>),
    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"payload_size">> => byte_size(Payload),
        <<"session_id">> => State#state.session_id
    }),

    %% Update pong count for statistics
    NewPongCount = State#state.pong_count + 1,

    logger:debug("WebSocket pong received (count: ~p)", [NewPongCount]),

    %% Pong is a response to our ping, no action needed
    erlmcp_tracing:end_span(SpanCtx),
    {ok, State#state{pong_count = NewPongCount}}.

%% Handle close frame (RFC 6455 Section 5.5.1)
-spec handle_close_frame(integer() | undefined, binary() | undefined, #state{}) ->
    {reply, {close, integer(), binary()}, #state{}} | {stop, #state{}}.
handle_close_frame(Code, Reason, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_ws.handle_close">>),
    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"close_code">> => case Code of undefined -> null; C -> C end,
        <<"session_id">> => State#state.session_id
    }),

    %% Log connection statistics
    ConnectionDuration = erlang:system_time(millisecond) - State#state.connection_start_time,
    logger:info("WebSocket closed by client: code=~p reason=~p duration=~pms stats=~p",
        [Code, Reason, ConnectionDuration, #{
            messages_sent => State#state.messages_sent,
            messages_received => State#state.messages_received,
            bytes_sent => State#state.bytes_sent,
            bytes_received => State#state.bytes_received,
            ping_count => State#state.ping_count,
            pong_count => State#state.pong_count
        }]),

    %% Respond with close frame echoing the code (RFC 6455)
    Reply = case {Code, Reason} of
        {undefined, undefined} ->
            {close, ?WS_CLOSE_NORMAL, <<>>};
        {undefined, _} ->
            {close, ?WS_CLOSE_NORMAL, <<>>};
        {CodeVal, undefined} ->
            {close, CodeVal, <<>>};
        {CodeVal, ReasonVal} ->
            {close, CodeVal, ReasonVal}
    end,

    erlmcp_tracing:end_span(SpanCtx),
    {reply, Reply, State}.

%%====================================================================
%% Internal Functions - Validation
%%====================================================================

%% Validate message size
-spec validate_message_size(binary()) -> {ok, integer()} | {error, too_big}.
validate_message_size(Data) ->
    MaxSize = application:get_env(erlmcp, max_ws_message_size, ?DEFAULT_MAX_MESSAGE_SIZE),
    Size = byte_size(Data),
    case Size =< MaxSize of
        true -> {ok, Size};
        false -> {error, too_big}
    end.

%% Validate UTF-8 encoding (RFC 3629)
-spec validate_utf8(binary()) -> ok | {error, invalid_utf8}.
validate_utf8(<<>>) ->
    ok;
validate_utf8(Data) ->
    case unicode:characters_to_list(Data, utf8) of
        {error, _, _} ->
            {error, invalid_utf8};
        {incomplete, _, _} ->
            {error, invalid_utf8};
        _List ->
            ok
    end.

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

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

close_with_error(backpressure_failed, State) ->
    {reply, {close, ?WS_CLOSE_GOING_AWAY, <<"Backpressure limit exceeded">>}, State};

close_with_error(_Reason, State) ->
    {reply, {close, ?WS_CLOSE_INTERNAL_ERROR, <<"Internal error">>}, State}.

%%====================================================================
%% Backpressure Management Functions
%%====================================================================

%% Check if backpressure is active
-spec check_backpressure(#state{}) -> {ok, #state{}} | {error, backpressure_active, #state{}}.
check_backpressure(State) ->
    BytesBuffered = State#state.bytes_buffered,
    MaxBuffer = State#state.frame_buffer_size,

    case State#state.backpressure_state of
        ?BACKPRESSURE_ACTIVE ->
            %% Already in backpressure, reject new messages
            {error, backpressure_active, State};
        ?BACKPRESSURE_INACTIVE ->
            %% Check if we're about to exceed buffer
            case BytesBuffered >= MaxBuffer of
                true ->
                    %% Activate backpressure
                    TimerRef = erlang:send_after(?BACKPRESSURE_TIMEOUT, self(), resume_reading),
                    NewState = State#state{
                        backpressure_state = ?BACKPRESSURE_ACTIVE,
                        backpressure_timer = TimerRef
                    },
                    {error, backpressure_active, NewState};
                false ->
                    {ok, State}
            end
    end.

%% Update buffer usage tracking
-spec update_buffer_usage(#state{}, integer(), add | subtract) -> #state{}.
update_buffer_usage(State, Bytes, add) ->
    NewBytesBuffered = State#state.bytes_buffered + Bytes,
    State#state{bytes_buffered = NewBytesBuffered, messages_pending = State#state.messages_pending + 1};

update_buffer_usage(State, Bytes, subtract) ->
    NewBytesBuffered = max(0, State#state.bytes_buffered - Bytes),
    NewMessagesPending = max(0, State#state.messages_pending - 1),
    State#state{bytes_buffered = NewBytesBuffered, messages_pending = NewMessagesPending}.

%% Resume reading when buffer drains below threshold
-spec resume_reading(#state{}) -> #state{}.
resume_reading(State) ->
    BytesBuffered = State#state.bytes_buffered,
    MaxBuffer = State#state.frame_buffer_size,
    DrainThreshold = trunc(MaxBuffer * ?BUFFER_DRAIN_THRESHOLD),

    case State#state.backpressure_state of
        ?BACKPRESSURE_ACTIVE when BytesBuffered =< DrainThreshold ->
            %% Cancel existing timer if any
            case State#state.backpressure_timer of
                undefined -> ok;
                TimerRef -> erlang:cancel_timer(TimerRef)
            end,
            %% Resume reading
            State#state{
                backpressure_state = ?BACKPRESSURE_INACTIVE,
                backpressure_timer = undefined
            };
        _ ->
            State
    end.
