%%%-------------------------------------------------------------------
%%% @doc TCPS Dashboard SSE (Server-Sent Events) Handler
%%%
%%% Provides real-time updates via Server-Sent Events.
%%% Streams metrics updates, Andon alerts, and work order changes.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_dashboard_sse_handler).

-export([init/2, info/3, terminate/3]).

-include_lib("kernel/include/logger.hrl").

-define(HEARTBEAT_INTERVAL, 30000). %% 30 seconds

%%%===================================================================
%%% Cowboy Loop Handler Callbacks
%%%===================================================================

init(Req, State) ->
    ?LOG_INFO("SSE client connected from ~p", [cowboy_req:peer(Req)]),

    %% Set headers for SSE
    Req2 = cowboy_req:stream_reply(200, #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, Req),

    %% Register with SSE manager
    tcps_sse_manager:register_client(self()),

    %% Subscribe to dashboard events (legacy)
    ok = tcps_dashboard:subscribe_events(self()),

    %% Send initial connection event
    send_event(Req2, <<"connected">>, #{
        timestamp => erlang:timestamp(),
        server_version => <<"1.0.0">>
    }),

    %% Send initial metrics (use aggregator)
    InitialMetrics = tcps_metrics_aggregator:get_all_metrics(),
    send_event(Req2, <<"metrics-update">>, InitialMetrics),

    %% Schedule heartbeat
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),

    {cowboy_loop, Req2, State}.

info({sse_update, Update}, Req, State) ->
    %% Received update from SSE manager
    EventType = maps:get(type, Update, <<"update">>),
    EventData = maps:get(data, Update, #{}),

    send_event(Req, format_event_type(EventType), EventData),

    {ok, Req, State};

info({dashboard_event, Event}, Req, State) ->
    %% Received event from dashboard (legacy path)
    EventType = maps:get(type, Event),
    EventData = maps:get(data, Event),

    send_event(Req, atom_to_binary(EventType, utf8), EventData),

    {ok, Req, State};

info(heartbeat, Req, State) ->
    %% Send heartbeat to keep connection alive
    send_heartbeat(Req),

    %% Schedule next heartbeat
    erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),

    {ok, Req, State};

info(_Msg, Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    %% Unregister from SSE manager
    tcps_sse_manager:unregister_client(self()),

    %% Unsubscribe from dashboard events (legacy)
    tcps_dashboard:unsubscribe_events(self()),

    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Send SSE event
send_event(Req, EventName, Data) ->
    EventId = generate_event_id(),
    JsonData = jsx:encode(Data),

    %% Format SSE message
    Message = [
        <<"id: ">>, EventId, <<"\n">>,
        <<"event: ">>, EventName, <<"\n">>,
        <<"data: ">>, JsonData, <<"\n\n">>
    ],

    cowboy_req:stream_body(Message, nofin, Req).

%% @private Send heartbeat comment
send_heartbeat(Req) ->
    Timestamp = erlang:system_time(millisecond),
    Message = [<<": heartbeat ">>, integer_to_binary(Timestamp), <<"\n\n">>],
    cowboy_req:stream_body(Message, nofin, Req).

%% @private Generate unique event ID
generate_event_id() ->
    integer_to_binary(erlang:unique_integer([positive, monotonic])).

%% @private Format event type (handle atoms and binaries)
format_event_type(EventType) when is_atom(EventType) ->
    atom_to_binary(EventType, utf8);
format_event_type(EventType) when is_binary(EventType) ->
    EventType;
format_event_type(_) ->
    <<"unknown">>.
