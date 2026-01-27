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

-define(PING_INTERVAL, 30000). %% 30 seconds
-define(IDLE_TIMEOUT, 300000). %% 5 minutes

-record(state, {
    transport_id :: binary(),
    registry_pid :: pid(),
    connection_info :: map(),
    session_id :: binary(),
    ping_timer :: reference() | undefined
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

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"transport_id">> => TransportId,
            <<"port">> => Port,
            <<"path">> => Path
        }),

        Dispatch = cowboy_router:compile([
            {'_', [
                {Path, ?MODULE, [TransportId]}
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

init(Req, [TransportId], _Opts) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_ws.cowboy_init">>),

    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"transport_id">> => TransportId
    }),

    {cowboy_websocket, Req, #{
        transport_id => TransportId,
        registry_pid => erlmcp_registry:get_pid(),
        span_ctx => SpanCtx
    }}.

websocket_handle({text, Data}, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_ws.handle_message">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"data_size">> => byte_size(Data)
        }),

        TransportId = maps:get(transport_id, State),
        RegistryPid = maps:get(registry_pid, State),

        %% Parse JSON-RPC message
        case jsx:decode(Data) of
            {error, _} ->
                erlmcp_tracing:record_error_details(SpanCtx, parse_error, Data),
                ErrorResp = jsx:encode(#{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"error">> => #{
                        <<"code">> => -32700,
                        <<"message">> => <<"Parse error">>
                    },
                    <<"id">> => null
                }),
                {reply, {text, ErrorResp}, State};
            Message ->
                %% Route to registry
                RegistryPid ! {transport_data, TransportId, Message},
                erlmcp_tracing:set_status(SpanCtx, ok),
                {ok, State}
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {ok, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

websocket_handle({binary, Data}, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_ws.handle_binary">>),
    erlmcp_tracing:set_status(SpanCtx, ok),
    erlmcp_tracing:end_span(SpanCtx),
    {reply, {text, <<"Binary data not supported">>}, State};

websocket_handle(ping, State) ->
    {reply, pong, State};

websocket_handle(pong, State) ->
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
