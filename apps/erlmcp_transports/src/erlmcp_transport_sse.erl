-module(erlmcp_transport_sse).

-include("erlmcp.hrl").

%% Note: This module does NOT implement erlmcp_transport_behavior
%% It is a Cowboy SSE handler with its own init/2 interface

%% SSE-specific exports (NOT erlmcp_transport_behavior callbacks)
-export([
    init/2,
    send/2,
    close/1
]).

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

-spec init(binary(), map()) -> {ok, pid()} | {error, term()}.
init(TransportId, Config) ->
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

    try
        %% Validate Origin header (DNS rebinding protection)
        case validate_request_origin(Req, SpanCtx) of
            {error, forbidden} ->
                erlmcp_tracing:record_error_details(SpanCtx, origin_validation_failed, <<"Invalid origin">>),
                ReqForbidden = cowboy_req:reply(403, #{
                    <<"content-type">> => <<"application/json">>
                }, jsx:encode(#{
                    <<"error">> => <<"Forbidden">>,
                    <<"message">> => <<"Origin not allowed">>
                }), Req),
                {ok, ReqForbidden, State};
            {ok, _ValidOrigin} ->
                case cowboy_req:method(Req) of
                    <<"GET">> ->
                        handle_sse_stream(Req, TransportId, State);
                    <<"POST">> ->
                        handle_post_request(Req, TransportId, State);
                    <<"DELETE">> ->
                        % TODO: Implement DELETE handler
                        ReqReply = cowboy_req:reply(501, #{}, <<"Not implemented">>, Req),
                        {ok, ReqReply, State};
                    _ ->
                        ReqReply = cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req),
                        {ok, ReqReply, State}
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

handle_sse_stream(Req, TransportId, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.handle_stream">>),

    %% Validate HTTP headers per MCP spec (Gap #10)
    ReqHeaders = maps:to_list(cowboy_req:headers(Req)),
    case erlmcp_http_header_validator:validate_request_headers(ReqHeaders, get) of
        {error, {StatusCode, Message, Data}} ->
            erlmcp_tracing:record_error_details(SpanCtx, header_validation_failed, Data),
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
            cowboy_req:stream_body(InitialRetry, Req2),

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

sse_event_loop(Req, StreamState, State) ->
    receive
        ping ->
            %% Send keep-alive ping comment (SSE standard)
            PingData = format_sse_event(?EVENT_TYPE_KEEPALIVE, <<"ping">>),
            cowboy_req:stream_body(PingData, Req),
            sse_event_loop(Req, StreamState, State);

        {send_event, EventType, Data} ->
            %% Get session and event info
            #{sse_state := SseState, session_id := SessionId} = StreamState,
            NewEventNumber = SseState#sse_state.event_number + 1,

            %% Store event for resumability
            {ok, EventId} = erlmcp_sse_event_store:add_event(SessionId, NewEventNumber, Data),

            %% Format as SSE event with event type and event ID
            EventData = format_sse_event_with_id(EventType, EventId, Data),
            cowboy_req:stream_body(EventData, Req),

            %% Update state with new event number
            UpdatedSseState = SseState#sse_state{event_number = NewEventNumber},
            UpdatedStreamState = StreamState#{sse_state => UpdatedSseState},

            sse_event_loop(Req, UpdatedStreamState, State);

        close ->
            %% Close the stream with retry hint
            CloseData = format_close_event_with_retry(get_retry_timeout()),
            cowboy_req:stream_body(CloseData, Req),
            {ok, Req, State};

        {disconnect, Reason} ->
            %% Handle unexpected disconnection
            ErrorData = format_sse_event(?EVENT_TYPE_ERROR, jsx:encode(#{
                <<"error">> => <<"disconnected">>,
                <<"reason">> => format_disconnect_reason(Reason),
                <<"retry">> => get_retry_timeout()
            })),
            cowboy_req:stream_body(ErrorData, Req),
            {ok, Req, State};

        _ ->
            sse_event_loop(Req, StreamState, State)
    after 300000 ->
        %% 5 minute idle timeout - send retry hint before closing
        cowboy_req:stream_body(format_retry_field(get_retry_timeout()), Req),
        {ok, Req, State}
    end.

terminate(_Reason, _Req, #{ping_timer := PingRef}) ->
    timer:cancel(PingRef),
    ok;

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Format SSE event with event type
%% SSE format: "event: <type>\ndata: <json>\n\n"
-spec format_sse_event(binary(), binary()) -> binary().
format_sse_event(EventType, Data) ->
    <<"event: ", EventType/binary, "\ndata: ", Data/binary, "\n\n">>.

%% @doc Format SSE event with event type and ID
%% SSE format: "id: <id>\nevent: <type>\ndata: <json>\n\n"
-spec format_sse_event_with_id(binary(), binary(), binary()) -> binary().
format_sse_event_with_id(EventType, EventId, Data) ->
    <<"id: ", EventId/binary, "\nevent: ", EventType/binary, "\ndata: ", Data/binary, "\n\n">>.

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
            cowboy_req:stream_body(ResumptionNotice, Req),

            %% Replay all stored events
            lists:foreach(
                fun(EventData) ->
                    %% Use message event type for replayed data
                    EventBody = format_sse_event_with_id(?EVENT_TYPE_MESSAGE, <<"replay">>, EventData),
                    cowboy_req:stream_body(EventBody, Req)
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
            cowboy_req:stream_body(ErrorEvent, Req),
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

%% @doc Format SSE retry field: "retry: N\n"
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
