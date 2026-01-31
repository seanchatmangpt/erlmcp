-module(erlmcp_transport_sse).
-behaviour(erlmcp_transport_behavior).

-include("erlmcp.hrl").

%% Transport behavior callbacks
-export([init/1, send/2, close/1, get_info/1, handle_transport_call/2]).

%% SSE-specific exports (internal)
-export([init_sse/2]).

%% Cowboy handler
-export([
    init/3,
    handle/2,
    terminate/3
]).

-define(PING_INTERVAL, 30000). %% 30 seconds
-define(MAX_RETRIES, 3).
-define(DEFAULT_RETRY_TIMEOUT, 5000). %% 5 seconds (Gap #29)
-define(DEFAULT_MAX_MESSAGE_SIZE, 16777216). %% 16 MB

%% SSE Event Types
-define(EVENT_TYPE_MESSAGE, <<"message">>).
-define(EVENT_TYPE_NOTIFICATION, <<"notification">>).
-define(EVENT_TYPE_ERROR, <<"error">>).
-define(EVENT_TYPE_KEEPALIVE, <<"keepalive">>).
-define(EVENT_TYPE_CLOSE, <<"close">>).

-record(sse_state, {
    transport_id :: binary(),
    client_id :: binary(),
    session_id :: binary(),
    request_ref :: reference() | undefined,
    ping_timer :: reference() | undefined,
    event_number = 0 :: non_neg_integer(),
    last_received_event_id :: binary() | undefined,
    max_message_size :: pos_integer()  % Maximum allowed message size in bytes
}).

%%====================================================================
%% Transport Behavior Implementation
%%====================================================================

%% @doc Initialize transport (starts Cowboy SSE listener)
-spec init(map()) -> {ok, pid()} | {error, term()}.
init(Config) when is_map(Config) ->
    TransportId = maps:get(transport_id, Config, <<"sse_default">>),
    init_sse(TransportId, Config).

%% @doc Get transport information
-spec get_info(pid() | term()) -> #{atom() => term()}.
get_info(_State) ->
    #{
        transport_id => undefined,
        type => sse,
        status => running
    }.

%% @doc Handle transport-specific calls
-spec handle_transport_call(term(), term()) ->
    {reply, term(), term()} | {error, term()}.
handle_transport_call(_Request, State) ->
    {error, unknown_request}.

%%====================================================================
%% SSE-Specific Implementation
%%====================================================================

-spec init_sse(binary(), map()) -> {ok, pid()} | {error, term()}.
init_sse(TransportId, Config) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.init">>),
    try
        Port = maps:get(port, Config, 8081),
        Path = maps:get(path, Config, "/mcp/sse"),
        MaxMessageSize = maps:get(max_message_size, Config, ?DEFAULT_MAX_MESSAGE_SIZE),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"transport_id">> => TransportId,
            <<"port">> => Port,
            <<"path">> => Path,
            <<"max_message_size">> => MaxMessageSize
        }),

        Dispatch = cowboy_router:compile([
            {'_', [
                {Path, erlmcp_transport_sse, [TransportId, Config]},
                {<<Path/binary, "/subscribe">>, erlmcp_transport_sse, [TransportId, Config]}
            ]}
        ]),

        {ok, _} = cowboy:start_clear(erlmcp_sse_listener,
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
send(ClientPid, Data) when is_pid(ClientPid), is_binary(Data) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.send">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"data_size">> => byte_size(Data)
        }),

        ClientPid ! {send_event, ?EVENT_TYPE_MESSAGE, Data},
        erlmcp_tracing:set_status(SpanCtx, ok),
        ok
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

send(_ClientPid, _Data) ->
    {error, invalid_arguments}.

-spec close(pid()) -> ok.
close(ClientPid) when is_pid(ClientPid) ->
    ClientPid ! close,
    ok;

close(_) ->
    ok.

%%====================================================================
%% Cowboy HTTP Handler
%%====================================================================

init(_, Req, [TransportId]) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.cowboy_init">>),

    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"transport_id">> => TransportId
    }),

    {ok, Req, #{
        transport_id => TransportId,
        span_ctx => SpanCtx
    }}.

handle(Req, #{transport_id := TransportId} = State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.handle">>),

    %% Log connection attempt for security audit
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    PeerIP = format_peer_ip(cowboy_req:peer(Req)),
    log_connection_attempt(Method, Path, PeerIP, TransportId),

    try
        %% Validate Origin header (DNS rebinding protection)
        case validate_request_origin(Req, SpanCtx) of
            {error, forbidden} ->
                erlmcp_tracing:record_error_details(SpanCtx, origin_validation_failed, <<"Invalid origin">>),
                %% Log security rejection
                log_security_rejection(<<"origin_forbidden">>, PeerIP, Method, Path),
                ReqForbidden = cowboy_req:reply(403, #{
                    <<"content-type">> => <<"application/json">>
                }, jsx:encode(#{
                    <<"error">> => <<"Forbidden">>,
                    <<"message">> => <<"Origin not allowed">>
                }), Req),
                {ok, ReqForbidden, State};
            {ok, ValidOrigin} ->
                %% Log successful origin validation
                log_security_success(<<"origin_validated">>, PeerIP, ValidOrigin),
                case cowboy_req:method(Req) of
                    <<"GET">> ->
                        handle_sse_stream(Req, TransportId, State);
                    <<"POST">> ->
                        handle_post_request(Req, TransportId, State);
                    <<"DELETE">> ->
                        handle_delete_request(Req, TransportId, State);
                    _ ->
                        log_security_rejection(<<"method_not_allowed">>, PeerIP, Method, Path),
                        ReqReply = cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req),
                        {ok, ReqReply, State}
                end
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            log_security_exception(Class, CaughtReason, PeerIP),
            ReqError = cowboy_req:reply(500, #{}, <<"Internal error">>, Req),
            {ok, ReqError, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

handle_sse_stream(Req, TransportId, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.handle_stream">>),

    %% Validate HTTP headers per MCP spec (Gap #10)
    ReqHeaders = maps:to_list(cowboy_req:headers(Req)),
    case erlmcp_http_header_validator:validate_request_headers(ReqHeaders, get) of
        {error, {StatusCode, Message, Data}} ->
            erlmcp_tracing:record_error_details(SpanCtx, header_validation_failed, Data),
            %% Log header validation failure for security audit
            PeerIP = format_peer_ip(cowboy_req:peer(Req)),
            log_header_validation_failure(PeerIP, <<"GET">>, Message, Data),
            {_StatusCode2, ResponseHeaders, Body} =
                erlmcp_http_header_validator:format_error_response(StatusCode, Message, Data),
            Req2 = cowboy_req:reply(StatusCode, maps:from_list(ResponseHeaders), Body, Req),
            {ok, Req2, State};
        {ok, ValidatedHeaders} ->
            %% Generate unique client ID and session ID
            ClientId = erlang:list_to_binary(erlang:pid_to_list(self())),
            SessionId = generate_session_id(ClientId),

            %% Extract Last-Event-ID header for stream resumption
            LastEventId = cowboy_req:header(<<"last-event-id">>, Req, undefined),

            erlmcp_tracing:set_attributes(SpanCtx, #{
                <<"client_id">> => ClientId,
                <<"session_id">> => SessionId,
                <<"transport_id">> => TransportId,
                <<"protocol_version">> => maps:get(protocol_version, ValidatedHeaders),
                <<"last_event_id">> => case LastEventId of
                    undefined -> <<"undefined">>;
                    _ -> LastEventId
                end
            }),

            %% Set SSE headers with support for resumption
            %% Include MCP-required headers (Gap #10)
            Headers = #{
                <<"content-type">> => <<"text/event-stream">>,
                <<"cache-control">> => <<"no-cache">>,
                <<"connection">> => <<"keep-alive">>,
                <<"x-accel-buffering">> => <<"no">>,
                <<"mcp-protocol-version">> => maps:get(protocol_version, ValidatedHeaders),
                <<"mcp-session-id">> => SessionId
            },

            %% Send initial retry field
            InitialRetry = format_retry_field(get_retry_timeout()),
            Req2 = cowboy_req:stream_reply(200, Headers, Req),
            cowboy_req:stream_body(InitialRetry, nofin, Req2),

            %% Register with registry
            RegistryPid = erlmcp_registry:get_pid(),
            RegistryPid ! {transport_connected, TransportId, #{
                client_id => ClientId,
                session_id => SessionId
            }},

            %% Start ping timer
            {ok, PingRef} = timer:send_interval(?PING_INTERVAL, ping),

            %% Get max message size (default 16MB)
            MaxMessageSize = get_max_message_size(),

            %% Prepare initial state
            SseState = #sse_state{
                transport_id = TransportId,
                client_id = ClientId,
                session_id = SessionId,
                ping_timer = PingRef,
                last_received_event_id = LastEventId,
                max_message_size = MaxMessageSize
            },

            %% If resuming, replay missed events
            case LastEventId of
                undefined ->
                    %% New stream - start fresh
                    sse_event_loop(Req2, #{
                        transport_id => TransportId,
                        client_id => ClientId,
                        session_id => SessionId,
                        registry_pid => RegistryPid,
                        ping_timer => PingRef,
                        span_ctx => SpanCtx,
                        sse_state => SseState
                    }, State);
                _ ->
                    %% Resuming - replay events after Last-Event-ID
                    handle_stream_resumption(Req2, TransportId, SessionId, LastEventId, SpanCtx, SseState, RegistryPid, PingRef, State)
            end
    end.

handle_post_request(Req, TransportId, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.handle_post">>),

    %% Validate HTTP headers per MCP spec (Gap #10)
    ReqHeaders = maps:to_list(cowboy_req:headers(Req)),
    case erlmcp_http_header_validator:validate_request_headers(ReqHeaders, post) of
        {error, {StatusCode, Message, Data}} ->
            erlmcp_tracing:record_error_details(SpanCtx, header_validation_failed, Data),
            %% Log header validation failure for security audit
            PeerIP = format_peer_ip(cowboy_req:peer(Req)),
            log_header_validation_failure(PeerIP, <<"POST">>, Message, Data),
            {_StatusCode2, ResponseHeaders, Body} =
                erlmcp_http_header_validator:format_error_response(StatusCode, Message, Data),
            Req2 = cowboy_req:reply(StatusCode, maps:from_list(ResponseHeaders), Body, Req),
            {ok, Req2, State};
        {ok, _ValidatedHeaders} ->
            %% POST requests deliver messages to the SSE stream
            {ok, Body, Req2} = cowboy_req:read_body(Req),

            %% Validate message size (16MB limit)
            BodySize = byte_size(Body),
            MaxMessageSize = get_max_message_size(),

            case BodySize > MaxMessageSize of
                true ->
                    logger:error("SSE POST message exceeds 16MB limit (~p bytes > ~p bytes)",
                        [BodySize, MaxMessageSize]),
                    %% Send proper JSON-RPC error response
                    ErrorMsg = erlmcp_json_rpc:error_message_too_large(null, MaxMessageSize),
                    Req3 = cowboy_req:reply(413, #{
                        <<"content-type">> => <<"application/json">>
                    }, ErrorMsg, Req2),
                    {ok, Req3, State};
                false ->
                    try
                        case jsx:decode(Body) of
                            {error, _} ->
                                erlmcp_tracing:record_error_details(SpanCtx, parse_error, Body),
                                Req3 = cowboy_req:reply(400, #{}, <<"Invalid JSON">>, Req2),
                                {ok, Req3, State};
                            Message ->
                                erlmcp_tracing:set_attributes(SpanCtx, #{
                                    <<"message_size">> => BodySize
                                }),

                                %% Route to registry
                                RegistryPid = erlmcp_registry:get_pid(),
                                RegistryPid ! {transport_data, TransportId, Message},

                                erlmcp_tracing:set_status(SpanCtx, ok),
                                ReqFinal = cowboy_req:reply(202, #{}, <<"Accepted">>, Req2),
                                {ok, ReqFinal, State}
                        end
                    catch
                        Class:CaughtReason:Stacktrace ->
                            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
                            ReqFinalError = cowboy_req:reply(500, #{}, <<"Internal error">>, Req2),
                            {ok, ReqFinalError, State}
                    after
                        erlmcp_tracing:end_span(SpanCtx)
                    end
            end
    end.

%% @doc Handle DELETE request to close SSE connection
%% Joe Armstrong pattern: Clean up all associated resources gracefully
%% @private
-spec handle_delete_request(term(), binary(), map()) -> {ok, term(), map()}.
handle_delete_request(Req, TransportId, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.handle_delete">>),

    %% Log DELETE request for security audit
    PeerIP = format_peer_ip(cowboy_req:peer(Req)),
    log_delete_request(PeerIP, TransportId),

    try
        %% Validate HTTP headers for DELETE request
        ReqHeaders = maps:to_list(cowboy_req:headers(Req)),
        case erlmcp_http_header_validator:validate_request_headers(ReqHeaders, delete) of
            {error, {StatusCode, Message, Data}} ->
                erlmcp_tracing:record_error_details(SpanCtx, header_validation_failed, Data),
                log_header_validation_failure(PeerIP, <<"DELETE">>, Message, Data),
                {_StatusCode2, ResponseHeaders, Body} =
                    erlmcp_http_header_validator:format_error_response(StatusCode, Message, Data),
                Req2 = cowboy_req:reply(StatusCode, maps:from_list(ResponseHeaders), Body, Req),
                {ok, Req2, State};
            {ok, _ValidatedHeaders} ->
                %% Extract session ID from headers
                SessionId = cowboy_req:header(<<"mcp-session-id">>, Req, undefined),

                erlmcp_tracing:set_attributes(SpanCtx, #{
                    <<"transport_id">> => TransportId,
                    <<"session_id">> => case SessionId of
                        undefined -> <<"undefined">>;
                        _ -> SessionId
                    end
                }),

                case SessionId of
                    undefined ->
                        %% No session to close
                        erlmcp_tracing:record_error_details(SpanCtx, delete_no_session, <<"No session ID provided">>),
                        log_delete_no_session(PeerIP),
                        ReqReply = cowboy_req:reply(404, #{
                            <<"content-type">> => <<"application/json">>
                        }, jsx:encode(#{
                            <<"error">> => <<"Not Found">>,
                            <<"message">> => <<"No active SSE session found">>
                        }), Req),
                        {ok, ReqReply, State};
                    _ ->
                        %% Clean up session and event store
                        case cleanup_sse_session(SessionId, TransportId, SpanCtx) of
                            ok ->
                                erlmcp_tracing:set_status(SpanCtx, ok),
                                %% Log successful session cleanup
                                log_delete_success(PeerIP, SessionId, TransportId),

                                %% Notify registry of disconnection
                                RegistryPid = erlmcp_registry:get_pid(),
                                RegistryPid ! {transport_disconnected, TransportId, normal},

                                %% Return 204 No Content on success
                                ReqReply = cowboy_req:reply(204, #{}, <<>>, Req),
                                {ok, ReqReply, State};
                            {error, Reason} ->
                                erlmcp_tracing:record_error_details(SpanCtx, delete_cleanup_failed, Reason),
                                %% Log cleanup failure
                                log_delete_cleanup_failed(PeerIP, SessionId, TransportId, Reason),

                                %% Notify registry of abnormal disconnection
                                RegistryPid = erlmcp_registry:get_pid(),
                                RegistryPid ! {transport_disconnected, TransportId, {cleanup_failed, Reason}},

                                %% Return 500 Internal Server Error on cleanup failure
                                ReqReply = cowboy_req:reply(500, #{
                                    <<"content-type">> => <<"application/json">>
                                }, jsx:encode(#{
                                    <<"error">> => <<"Internal Server Error">>,
                                    <<"message">> => <<"Failed to cleanup SSE session">>,
                                    <<"reason">> => format_term(Reason)
                                }), Req),
                                {ok, ReqReply, State}
                        end
                end
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            ReqError = cowboy_req:reply(500, #{}, <<"Internal error">>, Req),
            {ok, ReqError, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

%% @doc Clean up SSE session and associated resources
%% Joe Armstrong pattern: Proper resource cleanup with error handling
%% @private
-spec cleanup_sse_session(binary(), binary(), term()) -> ok | {error, term()}.
cleanup_sse_session(SessionId, TransportId, SpanCtx) ->
    try
        %% Clean up event store subscriptions
        case erlmcp_sse_event_store:delete_session(SessionId) of
            ok ->
                logger:info("SSE DELETE: Cleaned up session ~s for transport ~s",
                    [SessionId, TransportId]),
                erlmcp_tracing:set_attributes(SpanCtx, #{
                    <<"session_cleanup">> => <<"success">>
                }),
                ok;
            {error, Reason} ->
                logger:error("SSE DELETE: Failed to cleanup session ~s: ~p",
                    [SessionId, Reason]),
                erlmcp_tracing:record_error_details(SpanCtx, event_store_cleanup_failed, Reason),
                {error, {event_store_cleanup, Reason}}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {cleanup_exception, {Class, CaughtReason}}}
    end.

sse_event_loop(Req, StreamState, State) ->
    receive
        ping ->
            %% Send keep-alive ping comment (SSE standard)
            %% Format: ":\n\n" (colon followed by two newlines)
            PingData = format_sse_keepalive(),
            cowboy_req:stream_body(PingData, nofin, Req),
            sse_event_loop(Req, StreamState, State);

        {send_event, EventType, Data} ->
            %% Get session and event info
            #{sse_state := SseState, session_id := SessionId} = StreamState,
            NewEventNumber = SseState#sse_state.event_number + 1,

            %% Store event for resumability
            {ok, EventId} = erlmcp_sse_event_store:add_event(SessionId, NewEventNumber, Data),

            %% Format as SSE event with event type and event ID
            EventData = format_sse_event_with_id(EventType, EventId, Data),
            cowboy_req:stream_body(EventData, nofin, Req),

            %% Update state with new event number
            UpdatedSseState = SseState#sse_state{event_number = NewEventNumber},
            UpdatedStreamState = StreamState#{sse_state => UpdatedSseState},

            sse_event_loop(Req, UpdatedStreamState, State);

        close ->
            %% Close the stream with retry hint
            CloseData = format_close_event_with_retry(get_retry_timeout()),
            cowboy_req:stream_body(CloseData, fin, Req),
            {ok, Req, State};

        {disconnect, Reason} ->
            %% Handle unexpected disconnection
            ErrorData = format_sse_event(?EVENT_TYPE_ERROR, jsx:encode(#{
                <<"error">> => <<"disconnected">>,
                <<"reason">> => format_disconnect_reason(Reason),
                <<"retry">> => get_retry_timeout()
            })),
            cowboy_req:stream_body(ErrorData, fin, Req),
            {ok, Req, State};

        _ ->
            sse_event_loop(Req, StreamState, State)
    after 300000 ->
        %% 5 minute idle timeout - send retry hint before closing
        cowboy_req:stream_body(format_retry_field(get_retry_timeout()), fin, Req),
        {ok, Req, State}
    end.

terminate(_Reason, _Req, #{ping_timer := PingRef}) ->
    timer:cancel(PingRef),
    ok;

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal Functions - SSE Event Formatting
%%====================================================================

%% @doc Format SSE event with event type
%% SSE format: "event: <type>\ndata: <json>\n\n"
%% Handles multiline data by prefixing each line with "data: "
%% @private
-spec format_sse_event(binary(), binary()) -> binary().
format_sse_event(EventType, Data) ->
    %% Split data by newlines and prefix each line with "data: "
    DataLines = split_data_by_newline(Data),
    FormattedData = format_data_lines(DataLines),
    <<"event: ", EventType/binary, "\n", FormattedData/binary, "\n">>.

%% @doc Format SSE event with event type and ID
%% SSE format: "id: <id>\nevent: <type>\ndata: <json>\n\n"
%% Handles multiline data by prefixing each line with "data: "
%% @private
-spec format_sse_event_with_id(binary(), binary(), binary()) -> binary().
format_sse_event_with_id(EventType, EventId, Data) ->
    %% Split data by newlines and prefix each line with "data: "
    DataLines = split_data_by_newline(Data),
    FormattedData = format_data_lines(DataLines),
    <<"id: ", EventId/binary, "\nevent: ", EventType/binary, "\n", FormattedData/binary, "\n">>.

%% @doc Format SSE keepalive ping comment
%% SSE format: ":\n\n" (colon followed by two newlines)
%% This is the standard SSE keepalive format
%% @private
-spec format_sse_keepalive() -> binary().
format_sse_keepalive() ->
    <<":\n\n">>.

%% @doc Split binary data by newlines for SSE multiline formatting
%% Handles both \n and \r\n line endings
%% @private
-spec split_data_by_newline(binary()) -> [binary()].
split_data_by_newline(Data) ->
    %% First normalize \r\n to \n, then split by \n
    Normalized = binary:replace(Data, <<"\r\n">>, <<"\n">>, [global]),
    binary:split(Normalized, <<"\n">>, [global]).

%% @doc Format data lines for SSE, prefixing each non-empty line with "data: "
%% Empty lines become "data:\n" (which represents an empty data field)
%% @private
-spec format_data_lines([binary()]) -> binary().
format_data_lines([]) ->
    <<"data: ">>;
format_data_lines(Lines) ->
    FormattedLines = [format_data_line(Line) || Line <- Lines],
    binary:list_to_bin(FormattedLines).

%% @doc Format a single data line with "data: " prefix
%% @private
-spec format_data_line(binary()) -> binary().
format_data_line(Line) ->
    <<"data: ", Line/binary, "\n">>.

%%====================================================================
%% Internal Functions - Utility
%%====================================================================

%% @doc Generate session ID from client ID and timestamp
-spec generate_session_id(binary()) -> binary().
generate_session_id(ClientId) ->
    %% Create session ID from client ID and timestamp
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    Random = integer_to_binary(erlang:abs(erlang:system_time(nanosecond) rem 1000000)),
    <<"session_", ClientId/binary, "_", Timestamp/binary, "_", Random/binary>>.

%% @doc Handle stream resumption by replaying missed events
-spec handle_stream_resumption(
    term(),
    binary(),
    binary(),
    binary(),
    term(),
    #sse_state{},
    pid(),
    reference(),
    term()
) -> term().
handle_stream_resumption(Req, TransportId, SessionId, LastEventId, SpanCtx, SseState, RegistryPid, PingRef, State) ->
    %% Replay events after the Last-Event-ID
    case erlmcp_sse_event_store:get_events_since(SessionId, LastEventId) of
        {ok, Events} ->
            erlmcp_tracing:set_attributes(SpanCtx, #{
                <<"events_to_replay">> => length(Events)
            }),

            %% Send resumption notification
            ResumptionNotice = format_sse_event(?EVENT_TYPE_NOTIFICATION, jsx:encode(#{
                <<"type">> => <<"resumption">>,
                <<"from_event_id">> => LastEventId,
                <<"events_count">> => length(Events)
            })),
            cowboy_req:stream_body(ResumptionNotice, nofin, Req),

            %% Replay all stored events
            lists:foreach(
                fun(EventData) ->
                    %% Use message event type for replayed data
                    EventBody = format_sse_event_with_id(?EVENT_TYPE_MESSAGE, <<"replay">>, EventData),
                    cowboy_req:stream_body(EventBody, nofin, Req)
                end,
                Events
            ),

            %% Continue with normal event loop
            LastEventNum = erlmcp_sse_event_store:parse_event_id(LastEventId),
            UpdatedSseState = SseState#sse_state{
                event_number = LastEventNum
            },

            sse_event_loop(Req, #{
                transport_id => TransportId,
                session_id => SessionId,
                registry_pid => RegistryPid,
                ping_timer => PingRef,
                span_ctx => SpanCtx,
                sse_state => UpdatedSseState
            }, State);
        {error, Reason} ->
            erlmcp_tracing:record_error_details(SpanCtx, resumption_error, Reason),
            %% Send error event to client
            ErrorEvent = format_sse_event(?EVENT_TYPE_ERROR, jsx:encode(#{
                <<"error">> => <<"resumption_failed">>,
                <<"message">> => <<"Failed to resume session">>,
                <<"reason">> => format_term(Reason)
            })),
            cowboy_req:stream_body(ErrorEvent, fin, Req),
            {ok, Req, State}
    end.

%% @doc Format disconnect reason for client
-spec format_disconnect_reason(term()) -> binary().
format_disconnect_reason(Reason) when is_binary(Reason) ->
    Reason;
format_disconnect_reason(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_disconnect_reason(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% @doc Format Erlang term to binary
-spec format_term(term()) -> binary().
format_term(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).

%%====================================================================
%% Security and Validation Functions
%%====================================================================

%% @doc Validate Origin header to prevent DNS rebinding attacks
%% @private
-spec validate_request_origin(term(), term()) -> {ok, binary()} | {error, forbidden}.
validate_request_origin(Req, SpanCtx) ->
    Origin = cowboy_req:header(<<"origin">>, Req, undefined),
    AllowedOrigins = get_allowed_origins(),

    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"origin_validation">> => <<"checking">>
    }),

    case erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins) of
        {ok, ValidOrigin} ->
            erlmcp_tracing:set_attributes(SpanCtx, #{
                <<"origin_valid">> => <<"true">>,
                <<"origin">> => ensure_binary(ValidOrigin)
            }),
            {ok, ValidOrigin};
        {error, forbidden} ->
            erlmcp_tracing:set_attributes(SpanCtx, #{
                <<"origin_valid">> => <<"false">>,
                <<"origin">> => case Origin of
                    undefined -> <<"undefined">>;
                    O -> ensure_binary(O)
                end
            }),
            {error, forbidden}
    end.

%% @doc Get allowed origins from configuration or defaults
%% @private
-spec get_allowed_origins() -> [string()].
get_allowed_origins() ->
    case application:get_env(erlmcp, http_security) of
        undefined ->
            erlmcp_origin_validator:get_default_allowed_origins();
        {ok, SecurityConfig} ->
            case proplists:get_value(allowed_origins, SecurityConfig) of
                undefined ->
                    erlmcp_origin_validator:get_default_allowed_origins();
                Origins ->
                    Origins
            end
    end.

%% @doc Ensure value is binary
%% @private
-spec ensure_binary(term()) -> binary().
ensure_binary(B) when is_binary(B) ->
    B;
ensure_binary(S) when is_list(S) ->
    list_to_binary(S);
ensure_binary(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
ensure_binary(X) ->
    iolist_to_binary(io_lib:format("~p", [X])).

%%====================================================================
%% Retry Field Functions (Gap #29 - SSE Retry Field)
%%====================================================================

%% @doc Get retry timeout from configuration or return default
%% Per MCP 2025-11-25 specification: default is 5000ms
%% @private
-spec get_retry_timeout() -> pos_integer().
get_retry_timeout() ->
    case application:get_env(erlmcp, sse) of
        undefined ->
            ?DEFAULT_RETRY_TIMEOUT;
        {ok, SseConfig} ->
            case proplists:get_value(retry_timeout, SseConfig) of
                undefined ->
                    ?DEFAULT_RETRY_TIMEOUT;
                RetryMs when is_integer(RetryMs), RetryMs > 0 ->
                    RetryMs;
                _ ->
                    ?DEFAULT_RETRY_TIMEOUT
            end
    end.

%% @doc Format SSE retry field: "retry: N\n\n"
%% Per SSE specification, retry field tells client to wait N milliseconds
%% before attempting reconnection. (Gap #29)
%% @private
-spec format_retry_field(pos_integer()) -> binary().
format_retry_field(RetryMs) when is_integer(RetryMs), RetryMs > 0 ->
    RetryBin = integer_to_binary(RetryMs),
    <<"retry: ", RetryBin/binary, "\n\n">>.

%% @doc Format SSE close event with retry field (Gap #29)
%% Sends close event with retry hint so client knows to reconnect
%% Format: "event: close\ndata: {...}\nretry: N\n\n"
%% @private
-spec format_close_event_with_retry(pos_integer()) -> binary().
format_close_event_with_retry(RetryMs) when is_integer(RetryMs), RetryMs > 0 ->
    RetryBin = integer_to_binary(RetryMs),
    <<"event: ", ?EVENT_TYPE_CLOSE/binary, "\ndata: {\"status\":\"closed\"}\nretry: ",
      RetryBin/binary, "\n\n">>.

%% @doc Get the maximum allowed message size from configuration.
%% Falls back to default 16MB if not configured.
-spec get_max_message_size() -> pos_integer().
get_max_message_size() ->
    case application:get_env(erlmcp, message_size_limits) of
        {ok, Limits} when is_map(Limits) ->
            maps:get(sse, Limits, ?DEFAULT_MAX_MESSAGE_SIZE);
        _ ->
            ?DEFAULT_MAX_MESSAGE_SIZE
    end.

%%====================================================================
%% Security Logging Functions
%%====================================================================

%% @doc Format peer IP address for logging
%% @private
-spec format_peer_ip({inet:ip_address(), inet:port_number()}) -> binary().
format_peer_ip({{A, B, C, D}, Port}) ->
    iolist_to_binary(io_lib:format("~w.~w.~w.~w:~w", [A, B, C, D, Port]));
format_peer_ip({{A, B, C, D, E, F, G, H}, Port}) ->
    iolist_to_binary(io_lib:format("[~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b]:~w",
        [A, B, C, D, E, F, G, H, Port]));
format_peer_ip(_) ->
    <<"unknown">>.

%% @doc Log connection attempt for security audit
%% @private
-spec log_connection_attempt(binary(), binary(), binary(), binary()) -> ok.
log_connection_attempt(Method, Path, PeerIP, TransportId) ->
    logger:info("SSE connection attempt: ~s ~s from ~s (transport: ~s)",
        [Method, Path, PeerIP, TransportId]),
    ok.

%% @doc Log security rejection
%% @private
-spec log_security_rejection(binary(), binary(), binary(), binary()) -> ok.
log_security_rejection(Reason, PeerIP, Method, Path) ->
    logger:warning("SECURITY REJECTION: ~s - ~s ~s from ~s",
        [Reason, Method, Path, PeerIP]),
    ok.

%% @doc Log successful security validation
%% @private
-spec log_security_success(binary(), binary(), binary()) -> ok.
log_security_success(Event, PeerIP, Origin) ->
    logger:info("SECURITY SUCCESS: ~s from ~s (origin: ~s)",
        [Event, PeerIP, Origin]),
    ok.

%% @doc Log security exception
%% @private
-spec log_security_exception(atom(), term(), binary()) -> ok.
log_security_exception(Class, Reason, PeerIP) ->
    logger:error("SECURITY EXCEPTION: ~p:~p from ~s",
        [Class, Reason, PeerIP]),
    ok.

%% @doc Log header validation failure
%% @private
-spec log_header_validation_failure(binary(), binary(), binary(), term()) -> ok.
log_header_validation_failure(PeerIP, Method, Message, Data) ->
    logger:warning("SECURITY: Header validation failed for ~s from ~s: ~s - ~p",
        [Method, PeerIP, Message, Data]),
    ok.

%% @doc Log DELETE request
%% @private
-spec log_delete_request(binary(), binary()) -> ok.
log_delete_request(PeerIP, TransportId) ->
    logger:info("SSE DELETE request from ~s (transport: ~s)",
        [PeerIP, TransportId]),
    ok.

%% @doc Log DELETE with no session
%% @private
-spec log_delete_no_session(binary()) -> ok.
log_delete_no_session(PeerIP) ->
    logger:warning("SSE DELETE request with no session ID from ~s",
        [PeerIP]),
    ok.

%% @doc Log successful DELETE
%% @private
-spec log_delete_success(binary(), binary(), binary()) -> ok.
log_delete_success(PeerIP, SessionId, TransportId) ->
    logger:info("SSE DELETE success: session ~s closed from ~s (transport: ~s)",
        [SessionId, PeerIP, TransportId]),
    ok.

%% @doc Log DELETE cleanup failure
%% @private
-spec log_delete_cleanup_failed(binary(), binary(), binary(), term()) -> ok.
log_delete_cleanup_failed(PeerIP, SessionId, TransportId, Reason) ->
    logger:error("SSE DELETE cleanup failed: session ~s from ~s (transport: ~s) - reason: ~p",
        [SessionId, PeerIP, TransportId, Reason]),
    ok.
