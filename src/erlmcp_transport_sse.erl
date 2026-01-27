-module(erlmcp_transport_sse).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

-behaviour(erlmcp_transport).

%% Transport behavior
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

-record(sse_state, {
    transport_id :: binary(),
    client_id :: binary(),
    session_id :: binary(),
    request_ref :: reference() | undefined,
    ping_timer :: reference() | undefined,
    event_number = 0 :: non_neg_integer(),
    last_received_event_id :: binary() | undefined
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

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"transport_id">> => TransportId,
            <<"port">> => Port,
            <<"path">> => Path
        }),

        Dispatch = cowboy_router:compile([
            {'_', [
                {Path, erlmcp_transport_sse_handler, [TransportId]},
                {<<Path/binary, "/subscribe">>, erlmcp_transport_sse_handler, [TransportId]}
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

        ClientPid ! {send_event, Data},
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
        case cowboy_req:method(Req) of
            <<"GET">> ->
                handle_sse_stream(Req, TransportId, State);
            <<"POST">> ->
                handle_post_request(Req, TransportId, State);
            _ ->
                ReqReply = cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req),
                {ok, ReqReply, State}
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

    %% Generate unique client ID and session ID
    ClientId = erlang:list_to_binary(erlang:pid_to_list(self())),
    SessionId = generate_session_id(ClientId),

    %% Extract Last-Event-ID header for stream resumption
    LastEventId = cowboy_req:header(<<"last-event-id">>, Req, undefined),

    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"client_id">> => ClientId,
        <<"session_id">> => SessionId,
        <<"transport_id">> => TransportId,
        <<"last_event_id">> => case LastEventId of
            undefined -> <<"undefined">>;
            _ -> LastEventId
        end
    }),

    %% Set SSE headers with support for resumption
    Headers = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>,
        <<"x-accel-buffering">> => <<"no">>
    },

    Req2 = cowboy_req:reply(200, Headers, Req),

    %% Register with registry
    RegistryPid = erlmcp_registry:get_pid(),
    RegistryPid ! {transport_connected, TransportId, #{
        client_id => ClientId,
        session_id => SessionId
    }},

    %% Start ping timer
    {ok, PingRef} = timer:send_interval(?PING_INTERVAL, ping),

    %% Prepare initial state
    SseState = #sse_state{
        transport_id = TransportId,
        client_id = ClientId,
        session_id = SessionId,
        ping_timer = PingRef,
        last_received_event_id = LastEventId
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
    end.

handle_post_request(Req, TransportId, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.handle_post">>),

    %% POST requests deliver messages to the SSE stream
    {ok, Body, Req2} = cowboy_req:read_body(Req),

    try
        case jsx:decode(Body) of
            {error, _} ->
                erlmcp_tracing:record_error_details(SpanCtx, parse_error, Body),
                Req3 = cowboy_req:reply(400, #{}, <<"Invalid JSON">>, Req2),
                {ok, Req3, State};
            Message ->
                erlmcp_tracing:set_attributes(SpanCtx, #{
                    <<"message_size">> => byte_size(Body)
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
    end.

sse_event_loop(Req, StreamState, State) ->
    receive
        ping ->
            %% Send keep-alive ping comment
            cowboy_req:stream_body(<<":\n">>, Req),
            sse_event_loop(Req, StreamState, State);

        {send_event, Data} ->
            %% Get session and event info
            #{sse_state := SseState, session_id := SessionId} = StreamState,
            NewEventNumber = SseState#sse_state.event_number + 1,

            %% Store event for resumability
            {ok, EventId} = erlmcp_sse_event_store:add_event(SessionId, NewEventNumber, Data),

            %% Format as SSE event with event ID
            EventData = format_sse_event_with_id(EventId, Data),
            cowboy_req:stream_body(EventData, Req),

            %% Update state with new event number
            UpdatedSseState = SseState#sse_state{event_number = NewEventNumber},
            UpdatedStreamState = StreamState#{sse_state => UpdatedSseState},

            sse_event_loop(Req, UpdatedStreamState, State);

        close ->
            %% Close the stream with retry hint
            CloseData = <<"event: close\ndata: {\"status\":\"closed\"}\nretry: 3000\n\n">>,
            cowboy_req:stream_body(CloseData, Req),
            {ok, Req, State};

        _ ->
            sse_event_loop(Req, StreamState, State)
    after 300000 ->
        %% 5 minute idle timeout - send retry hint before closing
        cowboy_req:stream_body(<<"retry: 3000\n">>, Req),
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

-spec format_sse_event(binary()) -> binary().
format_sse_event(Data) ->
    %% Format: event: message\ndata: {json}\n\n
    <<"event: message\ndata: ", Data/binary, "\n\n">>.

-spec format_sse_event_with_id(binary(), binary()) -> binary().
format_sse_event_with_id(EventId, Data) ->
    %% Format with event ID for resumability:
    %% id: session_abc123_42\ndata: {json}\n\n
    <<"id: ", EventId/binary, "\ndata: ", Data/binary, "\n\n">>.

-spec generate_session_id(binary()) -> binary().
generate_session_id(ClientId) ->
    %% Create session ID from client ID and timestamp
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    Random = integer_to_binary(erlang:abs(erlang:system_time(nanosecond) rem 1000000)),
    <<"session_", ClientId/binary, "_", Timestamp/binary, "_", Random/binary>>.

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

            %% Replay all stored events
            lists:foreach(
                fun(EventData) ->
                    EventBody = format_sse_event(EventData),
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
            cowboy_req:reply(500, #{}, <<"Resume failed">>, Req),
            {ok, Req, State}
    end.

