-module(erlmcp_transport_sse).

-include("erlmcp.hrl").

%% Temporarily disabled until opentelemetry_api is properly configured
%% -include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% Mock tracing macros for now
-define(otel_tracer, undefined).
-define(start_span(Name), undefined).
-define(end_span(Ctx), ok).
-define(set_attributes(Ctx, Attrs), ok).
-define(set_status(Ctx, Status), ok).
-define(record_exception(Ctx, Class, Reason, Stacktrace), ok).
-define(record_error_details(Ctx, Reason, Data), ok).

%% Note: This module does NOT implement erlmcp_transport_behavior
%% It is a Cowboy SSE handler with its own init/2 interface

%% SSE-specific exports (NOT erlmcp_transport_behavior callbacks)
-export([init/2, send/2, close/1]).
%% Cowboy HTTP handler
-export([init/3, handle/2, terminate/3]).

-define(PING_INTERVAL, 30000). %% 30 seconds
-define(DEFAULT_MAX_MESSAGE_SIZE, 16777216). %% 16 MB

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

        erlmcp_tracing:set_attributes(SpanCtx,
                                      #{<<"transport_id">> => TransportId,
                                        <<"port">> => Port,
                                        <<"path">> => Path,
                                        <<"max_message_size">> => MaxMessageSize}),

        Dispatch = cowboy_router:compile([{'_', [{Path, ?MODULE, [TransportId, Config]}]}]),

        %% Cowboy listener configuration
        ListenerOpts = [{port, Port}, {max_connections, 1000}, {connection_type, supervisor}],

        {ok, _} =
            cowboy:start_clear(erlmcp_sse_listener,
                               ListenerOpts,
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
        erlmcp_tracing:set_attributes(SpanCtx, #{<<"data_size">> => byte_size(Data)}),

        ClientPid ! {send_event, <<"message">>, Data},
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

init(_Type, Req, [TransportId]) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.cowboy_init">>),
    erlmcp_tracing:set_attributes(SpanCtx, #{<<"transport_id">> => TransportId}),
    {ok, Req, #{transport_id => TransportId, span_ctx => SpanCtx}}.

handle(Req, #{transport_id := TransportId} = State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.handle">>),
    try
        Method = cowboy_req:method(Req),
        case Method of
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
    ClientId =
        erlang:list_to_binary(
            erlang:pid_to_list(self())),
    SessionId = generate_session_id(ClientId),

    erlmcp_tracing:set_attributes(SpanCtx,
                                  #{<<"client_id">> => ClientId,
                                    <<"session_id">> => SessionId,
                                    <<"transport_id">> => TransportId}),

    %% Set SSE headers
    Headers =
        #{<<"content-type">> => <<"text/event-stream">>,
          <<"cache-control">> => <<"no-cache">>,
          <<"connection">> => <<"keep-alive">>,
          <<"x-accel-buffering">> => <<"no">>},

    Req2 = cowboy_req:stream_reply(200, Headers, Req),

    %% Register with registry
    RegistryPid = erlmcp_registry:get_pid(),
    RegistryPid
    ! {transport_connected, TransportId, #{client_id => ClientId, session_id => SessionId}},

    %% Start ping timer
    {ok, PingRef} = timer:send_interval(?PING_INTERVAL, ping),

    sse_event_loop(Req2,
                   #{transport_id => TransportId,
                     client_id => ClientId,
                     session_id => SessionId,
                     registry_pid => RegistryPid,
                     ping_timer => PingRef,
                     span_ctx => SpanCtx},
                   State).

handle_post_request(Req, TransportId, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.handle_post">>),

    {ok, Body, Req2} = cowboy_req:read_body(Req),

    %% Validate message size
    BodySize = byte_size(Body),
    MaxMessageSize = ?DEFAULT_MAX_MESSAGE_SIZE,

    case BodySize > MaxMessageSize of
        true ->
            logger:error("SSE POST message exceeds 16MB limit (~p bytes > ~p bytes)",
                         [BodySize, MaxMessageSize]),
            Req3 = cowboy_req:reply(413, #{}, <<"Payload too large">>, Req2),
            {ok, Req3, State};
        false ->
            try
                case jsx:decode(Body) of
                    {error, Reason} ->
                        erlmcp_tracing:record_error_details(SpanCtx, parse_error, Body),
                        Req3 = cowboy_req:reply(400, #{}, <<"Invalid JSON">>, Req2),
                        {ok, Req3, State};
                    Message ->
                        erlmcp_tracing:set_attributes(SpanCtx, #{<<"message_size">> => BodySize}),

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
    end.

sse_event_loop(Req, StreamState, State) ->
    receive
        ping ->
            %% Send keep-alive comment
            PingData = <<":\n\n">>,
            cowboy_req:stream_body(PingData, nofin, Req),
            sse_event_loop(Req, StreamState, State);
        {send_event, EventType, Data} ->
            %% Format as SSE event
            EventData = format_sse_event(EventType, Data),
            cowboy_req:stream_body(EventData, nofin, Req),
            sse_event_loop(Req, StreamState, State);
        close ->
            cowboy_req:stream_body(<<>>, fin, Req),
            {ok, Req, State};
        _ ->
            sse_event_loop(Req, StreamState, State)
    after 300000 ->
        %% 5 minute idle timeout
        cowboy_req:stream_body(<<>>, fin, Req),
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

%% @doc Format SSE event: "event: <type>\ndata: <json>\n\n"
%% OTP 28: Use binary:join/2 for efficient newline joining
-spec format_sse_event(binary(), binary()) -> binary().
format_sse_event(EventType, Data) ->
    %% OTP 28: Use erlmcp_binary_utils for efficient join
    Chunks = [<<"event: ">>, EventType, <<"\ndata: ">>, Data, <<"\n\n">>],
    iolist_to_binary(Chunks).

%% @doc Generate session ID
-spec generate_session_id(binary()) -> binary().
generate_session_id(ClientId) ->
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    <<"session_", ClientId/binary, "_", Timestamp/binary>>.
