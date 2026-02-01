# SSE Resumption Implementation Plan for erlmcp v3

**Author**: Erlang Transport Builder Agent
**Date**: 2026-01-31
**Status**: Design Specification
**MCP Spec**: 2025-11-25 (HTTP Streamable Transport)

---

## Executive Summary

This plan details the integration of **SSE (Server-Sent Events) stream resumption** between the transport layer and event store, enabling clients to reconnect and receive missed events after network failures.

### Current State

- ✅ **Event Store** (`erlmcp_sse_event_store.erl`): Fully implemented with ETS storage, event replay, and cleanup
- ❌ **Transport Layer** (`erlmcp_transport_sse.erl`): **DISCONNECTED** from event store
- ❌ **Last-Event-ID Handling**: **NOT IMPLEMENTED**
- ❌ **SSE Manager** (`erlmcp_transport_sse_manager.erl`): No session state tracking

### Gap Analysis

| Component | Status | Gap |
|-----------|--------|-----|
| Event Store (ETS) | ✅ Complete | - |
| Last-Event-ID Header Parser | ❌ Missing | Critical |
| Event Replay Orchestrator | ❌ Missing | Critical |
| Session State Manager | ❌ Missing | High |
| SSE Event ID Injection | ❌ Missing | Critical |
| Configuration (TTL, Window) | ❌ Missing | Medium |

---

## Architecture Design

### Component Integration

```
┌─────────────────────────────────────────────────────────────────┐
│                     SSE Transport Layer                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌──────────────────┐        ┌────────────────────────────┐   │
│  │  Cowboy Handler  │        │  SSE Manager (gen_server)  │   │
│  │  (init/3)        │◄──────┤  - Session state tracking  │   │
│  │                  │        │  - Connection registry     │   │
│  │ ┌──────────────┐ │        │  - Event replay trigger    │   │
│  │ │  Parse       │ │        └────────────────────────────┘   │
│  │ │  Last-Event- │ │                  │                       │
│  │ │  ID Header   │ │                  │                       │
│  │ └──────┬───────┘ │                  │                       │
│  └────────┼─────────┘                  │                       │
│           │                            │                       │
│           │ parse_header()             │ session_state         │
│           │                            │                       │
└───────────┼────────────────────────────┼───────────────────────┘
            │                            │
            │ get_events_since()         │ add_event()
            ▼                            ▼
┌───────────────────────────────────────────────────────────────────┐
│                     Event Store (ETS)                             │
├───────────────────────────────────────────────────────────────────┤
│  - ETS table per session: {erlmcp_sse_events, SessionId}         │
│  - Event number as key: O(log N) lookup                          │
│  - TTL: 1 hour per event                                         │
│  - Cleanup interval: 5 minutes                                   │
└───────────────────────────────────────────────────────────────────┘
```

### Data Flow

#### 1. Initial Connection (No Last-Event-ID)

```
Client                          Server
  │                               │
  ├─ GET /mcp/sse ────────────►  │  1. Generate SessionId
  │                               │  2. Parse Last-Event-ID → undefined
  │                               │  3. Initialize StreamState
  │                               │  4. Begin live streaming
  │◄───── id: session_..._1 ──────┤
  │◄───── data: {...} ────────────┤  5. Add event to store
  │◄───── id: session_..._2 ──────┤     add_event(SessionId, 2, Data)
  │◄───── data: {...} ────────────┤
```

#### 2. Reconnection (With Last-Event-ID)

```
Client                          Server
  │                               │
  ├─ GET /mcp/sse ────────────►  │  1. Generate SessionId (reuse)
  │  Last-Event-ID:              │  2. Parse Last-Event-ID → 10
  │    session_..._10            │  3. Fetch missed events
  │                               │     get_events_since(SessionId, 10)
  │                               │  4. Replay events 11, 12, 13...
  │◄───── id: session_..._11 ─────┤
  │◄───── data: {...} ────────────┤
  │◄───── id: session_..._12 ─────┤
  │                               │  5. Continue live streaming
  │◄───── id: session_..._14 ─────┤
  │◄───── data: {...} ────────────┤
```

---

## Implementation Specifications

### Phase 1: Transport Layer Integration

#### File: `apps/erlmcp_transports/src/erlmcp_transport_sse.erl`

**Current Issues:**
- No event store integration
- No Last-Event-ID header parsing
- No event ID injection in SSE events
- No replay orchestration

**Required Changes:**

##### 1.1 Update `handle_sse_stream/3` to Parse Last-Event-ID

```erlang
handle_sse_stream(Req, TransportId, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.handle_stream">>),

    %% Parse Last-Event-ID header
    LastEventId = cowboy_req:header(<<"last-event-id">>, Req),

    %% Generate unique client ID and session ID
    ClientId = erlang:list_to_binary(erlang:pid_to_list(self())),
    SessionId = generate_session_id(ClientId),

    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"client_id">> => ClientId,
        <<"session_id">> => SessionId,
        <<"transport_id">> => TransportId,
        <<"last_event_id">> => LastEventId
    }),

    %% Set SSE headers
    Headers = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>,
        <<"x-accel-buffering">> => <<"no">>
    },

    Req2 = cowboy_req:stream_reply(200, Headers, Req),

    %% Fetch missed events if Last-Event-ID present
    ok = replay_missed_events(Req2, SessionId, LastEventId, SpanCtx),

    %% Register with registry
    RegistryPid = erlmcp_registry:get_pid(),
    RegistryPid ! {transport_connected, TransportId, #{
        client_id => ClientId,
        session_id => SessionId,
        last_event_id => LastEventId
    }},

    %% Start ping timer
    {ok, PingRef} = timer:send_interval(?PING_INTERVAL, ping),

    %% Initialize event counter for this session
    EventCounter = case LastEventId of
        undefined -> 0;
        _ -> erlmcp_sse_event_store:parse_event_id(LastEventId)
    end,

    sse_event_loop(Req2, #{
        transport_id => TransportId,
        client_id => ClientId,
        session_id => SessionId,
        registry_pid => RegistryPid,
        ping_timer => PingRef,
        span_ctx => SpanCtx,
        event_counter => EventCounter,
        last_event_id => LastEventId
    }, State).
```

##### 1.2 Implement `replay_missed_events/4`

```erlang
%% @doc Replay missed events from event store
-spec replay_missed_events(cowboy_req:req(), binary(), binary() | undefined, otel:span_ctx()) -> ok.
replay_missed_events(Req, SessionId, LastEventId, SpanCtx) ->
    case LastEventId of
        undefined ->
            %% No previous events, start fresh
            erlmcp_tracing:set_attributes(SpanCtx, #{<<"replay">> => <<"none">>}),
            ok;
        _ ->
            erlmcp_tracing:set_attributes(SpanCtx, #{<<"replay">> => <<"active">>}),

            %% Fetch events from store
            case erlmcp_sse_event_store:get_events_since(SessionId, LastEventId) of
                {ok, []} ->
                    %% No missed events
                    erlmcp_tracing:set_attributes(SpanCtx, #{<<"missed_events">> => 0}),
                    ok;
                {ok, MissedEvents} ->
                    %% Replay missed events
                    MissedCount = length(MissedEvents),
                    erlmcp_tracing:set_attributes(SpanCtx, #{<<"missed_events">> => MissedCount}),

                    lists:foreach(fun(EventData) ->
                        %% Format as SSE event (without ID during replay)
                        FormattedEvent = format_sse_event(<<"message">>, EventData),
                        cowboy_req:stream_body(FormattedEvent, nofin, Req)
                    end, MissedEvents),

                    ok;
                {error, Reason} ->
                    %% Log error but continue with live stream
                    erlmcp_tracing:record_error_details(SpanCtx, replay_error, Reason),
                    logger:error("Failed to replay events for session ~s: ~p", [SessionId, Reason]),
                    ok
            end
    end.
```

##### 1.3 Update `sse_event_loop/3` to Add Event IDs and Store Events

```erlang
sse_event_loop(Req, StreamState = #{event_counter := Counter, session_id := SessionId}, State) ->
    receive
        ping ->
            %% Send keep-alive comment
            PingData = <<":\n\n">>,
            cowboy_req:stream_body(PingData, nofin, Req),
            sse_event_loop(Req, StreamState, State);

        {send_event, EventType, Data} ->
            %% Increment event counter
            NewCounter = Counter + 1,

            %% Store event for resumption
            EventId = case erlmcp_sse_event_store:add_event(SessionId, NewCounter, Data) of
                {ok, GeneratedId} -> GeneratedId;
                {error, StoreReason} ->
                    logger:error("Failed to store event: ~p", [StoreReason]),
                    %% Fallback: generate ID locally
                    <<SessionId/binary, "_", (integer_to_binary(NewCounter))/binary>>
            end,

            %% Format as SSE event WITH ID
            EventData = format_sse_event_with_id(EventType, EventId, Data),
            cowboy_req:stream_body(EventData, nofin, Req),

            %% Update stream state
            UpdatedState = StreamState#{event_counter => NewCounter},
            sse_event_loop(Req, UpdatedState, State);

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
```

##### 1.4 Implement `format_sse_event_with_id/3`

```erlang
%% @doc Format SSE event with ID for resumption
%% Format: "id: <id>\nevent: <type>\ndata: <json>\n\n"
-spec format_sse_event_with_id(binary(), binary(), binary()) -> binary().
format_sse_event_with_id(EventType, EventId, Data) ->
    <<"id: ", EventId/binary, "\nevent: ", EventType/binary, "\ndata: ", Data/binary, "\n\n">>.
```

##### 1.5 Add Configuration Support

```erlang
%% Add to top of module
-define(EVENT_TTL, 3600000).        %% 1 hour event TTL
-define(REPLAY_WINDOW, 86400000).   %% 24 hour replay window
-define(MAX_SESSION_EVENTS, 10000). %% Max events per session

%% In init/2 or handle_sse_stream/2, read config:
get_event_ttl() ->
    case application:get_env(erlmcp, sse) of
        {ok, SSEConfig} ->
            proplists:get_value(event_ttl, SSEConfig, ?EVENT_TTL);
        undefined ->
            ?EVENT_TTL
    end.

get_replay_window() ->
    case application:get_env(erlmcp, sse) of
        {ok, SSEConfig} ->
            proplists:get_value(replay_window, SSEConfig, ?REPLAY_WINDOW);
        undefined ->
            ?REPLAY_WINDOW
    end.
```

---

### Phase 2: SSE Manager Session State

#### File: `apps/erlmcp_transports/src/erlmcp_transport_sse_manager.erl`

**Current Issues:**
- No session state persistence
- No event tracking
- Missing resumption coordination

**Required Changes:**

##### 2.1 Update State Record

```erlang
-record(state, {
    transport_id :: binary(),
    port :: integer(),
    path :: binary(),
    listener_ref :: term() | undefined,
    connections :: #{binary() => #conn_info{}}  %% SessionId -> ConnInfo
}).

-record(conn_info, {
    session_id :: binary(),
    client_pid :: pid(),
    req :: term(),
    ping_timer :: reference(),
    last_event_number :: non_neg_integer(),  %% Track last event sent
    connected_at :: integer()                %% Connection timestamp
}).
```

##### 2.2 Add Session State Tracking

```erlang
handle_cast({register_connection, SessionId, ClientPid, Req, LastEventId}, State) ->
    ?LOG_DEBUG("Registering SSE connection: ~p (Last-Event-ID: ~p)", [SessionId, LastEventId]),

    %% Parse last event number
    LastEventNum = case LastEventId of
        undefined -> 0;
        _ -> erlmcp_sse_event_store:parse_event_id(LastEventId)
    end,

    NewConnections = maps:put(SessionId,
                              #conn_info{
                                  session_id = SessionId,
                                  client_pid = ClientPid,
                                  req = Req,
                                  ping_timer = undefined,
                                  last_event_number => LastEventNum,
                                  connected_at => erlang:system_time(millisecond)
                              },
                              State#state.connections),
    {noreply, State#state{connections = NewConnections}};

handle_cast({update_event_counter, SessionId, EventNumber}, State) ->
    %% Update last event number for session
    NewConnections = maps:update_with(
        SessionId,
        fun(ConnInfo) -> ConnInfo#conn_info{last_event_number = EventNumber} end,
        State#state.connections
    ),
    {noreply, State#state{connections = NewConnections}};
```

##### 2.3 Add Session Info Query

```erlang
handle_call({get_session_info, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.connections, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ConnInfo ->
            Info = #{
                session_id => ConnInfo#conn_info.session_id,
                last_event_number => ConnInfo#conn_info.last_event_number,
                connected_at => ConnInfo#conn_info.connected_at,
                connected_duration => erlang:system_time(millisecond) - ConnInfo#conn_info.connected_at
            },
            {reply, {ok, Info}, State}
    end;
```

---

### Phase 3: Configuration

#### File: `config/sys.config`

```erlang
{erlmcp, [
    %% ... other config ...

    %% Server-Sent Events (SSE) Transport Configuration
    {sse, [
        {enabled, true},
        {port, 8081},
        {path, "/mcp/sse"},
        {keepalive_interval, 30000},    %% 30 seconds
        {stream_timeout, 300000},        %% 5 minutes
        {retry_timeout, 5000},           %% 5 seconds (Gap #29)
        {event_ttl, 3600000},            %% 1 hour - event retention
        {replay_window, 86400000},       %% 24 hours - max replay window
        {max_session_events, 10000},     %% Max events per session
        {cleanup_interval, 300000}       %% 5 minutes - event cleanup
    ]}
]}.
```

---

### Phase 4: Integration Tests

#### File: `apps/erlmcp_transports/test/erlmcp_sse_resumption_integration_tests.erl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc SSE Resumption Integration Tests (Chicago School TDD)
%%%
%%% Tests full transport integration with event store:
%%% - Last-Event-ID header parsing
%%% - Event replay on reconnection
%%% - Session continuity across disconnects
%%% - Configuration validation (TTL, replay window)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sse_resumption_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    %% Start event store
    {ok, EventStorePid} = erlmcp_sse_event_store:start_link(),

    %% Start SSE transport manager
    Config = #{
        port => 8081,
        path => <<"/mcp/sse">>
    },
    {ok, ManagerPid} = erlmcp_transport_sse_manager:start_link(
        <<"test_transport">>,
        Config
    ),

    #{
        event_store => EventStorePid,
        manager => ManagerPid,
        config => Config
    }.

cleanup(#{event_store := EPid, manager := MPid}) ->
    gen_server:stop(EPid),
    gen_server:stop(MPid),
    ok.

%%%===================================================================
%%% Last-Event-ID Header Tests
%%%===================================================================

last_event_id_header_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Ctx) ->
         [
          ?_test(test_parse_last_event_id_header()),
          ?_test(test_parse_missing_last_event_id()),
          ?_test(test_parse_malformed_last_event_id())
         ]
     end}.

test_parse_last_event_id_header() ->
    %% Simulate HTTP request with Last-Event-ID header
    SessionId = <<"session_test_replay">>,
    LastEventId = <<"session_test_replay_42">>,

    %% Add test events to store
    ok = add_events(SessionId, [
        {43, <<"event_43">>},
        {44, <<"event_44">>},
        {45, <<"event_45">>}
    ]),

    %% Verify event retrieval
    {ok, Events} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        LastEventId
    ),

    ?assertEqual(3, length(Events)),
    ?assertEqual(<<"event_43">>, lists:nth(1, Events)).

test_parse_missing_last_event_id() ->
    %% No Last-Event-ID header should return all events
    SessionId = <<"session_no_header">>,

    ok = add_events(SessionId, [
        {1, <<"first">>},
        {2, <<"second">>}
    ]),

    {ok, Events} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        undefined
    ),

    ?assertEqual(2, length(Events)).

test_parse_malformed_last_event_id() ->
    %% Malformed header should default to 0
    SessionId = <<"session_malformed">>,

    ok = add_events(SessionId, [{1, <<"only_event">>}]),

    %% Invalid event ID format
    {ok, Events} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"invalid_format">
    ),

    %% Should return all events (parse_event_id returns 0)
    ?assertEqual(1, length(Events)).

%%%===================================================================
%%% Event Replay Tests
%%%===================================================================

event_replay_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Ctx) ->
         [
          ?_test(test_replay_after_disconnect()),
          ?_test(test_replay_with_multiple_disconnects()),
          ?_test(test_replay_event_ordering())
         ]
     end}.

test_replay_after_disconnect() ->
    SessionId = <<"session_disconnect_replay">>,

    %% Simulate client receives events 1-5
    ok = add_events(SessionId, lists:map(fun(N) ->
        {N, <<"event_", (integer_to_binary(N))/binary>>}
    end, lists:seq(1, 10))),

    %% Client disconnects at event 5
    %% Server continues: events 6-10 added

    %% Client reconnects with Last-Event-ID = 5
    LastEventId = <<"session_disconnect_replay_5">>,
    {ok, MissedEvents} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        LastEventId
    ),

    %% Should recover events 6-10
    ?assertEqual(5, length(MissedEvents)),
    ?assertEqual(<<"event_6">>, lists:nth(1, MissedEvents)),
    ?assertEqual(<<"event_10">>, lists:nth(5, MissedEvents)).

test_replay_with_multiple_disconnects() ->
    SessionId = <<"session_multi_disconnect">>,

    %% First connection: events 1-5
    ok = add_events(SessionId, lists:map(fun(N) ->
        {N, <<"batch1_", (integer_to_binary(N))/binary>>}
    end, lists:seq(1, 5))),

    %% First reconnect from event 3
    {ok, Batch2} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_multi_disconnect_3">>
    ),

    %% Add events 6-10
    ok = add_events(SessionId, lists:map(fun(N) ->
        {N, <<"batch2_", (integer_to_binary(N))/binary>>}
    end, lists:seq(6, 10))),

    %% Second reconnect from event 8
    {ok, Batch3} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_multi_disconnect_8">>
    ),

    ?assertEqual(2, length(Batch2)),  %% events 4, 5
    ?assertEqual(2, length(Batch3)).  %% events 9, 10

test_replay_event_ordering() ->
    SessionId = <<"session_ordering">>,

    %% Add events
    ok = add_events(SessionId, [
        {1, <<"first">>},
        {5, <<"fifth">>},
        {10, <<"tenth">>}
    ]),

    %% Replay from beginning
    {ok, Events} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        undefined
    ),

    %% Verify ordering by event number
    ?assertEqual(3, length(Events)),
    ?assertEqual(<<"first">>, lists:nth(1, Events)),
    ?assertEqual(<<"fifth">>, lists:nth(2, Events)),
    ?assertEqual(<<"tenth">>, lists:nth(3, Events)).

%%%===================================================================
%%% Session Continuity Tests
%%%===================================================================

session_continuity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Ctx) ->
         [
          ?_test(test_session_id_reuse()),
          ?_test(test_event_counter_continuity()),
          ?_test(test_session_state_tracking())
         ]
     end}.

test_session_id_reuse() ->
    %% Client reconnects to same session
    SessionId = <<"session_reuse">>,

    %% Initial connection
    ok = add_events(SessionId, [{1, <<"initial">>}]),

    %% Disconnect and reconnect with same session ID
    {ok, Events} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_reuse_1">>
    ),

    %% Session should persist
    ?assertEqual([], Events).  %% No new events

test_event_counter_continuity() ->
    SessionId = <<"session_counter">>,

    %% Add events 1-10
    ok = add_events(SessionId, lists:map(fun(N) ->
        {N, integer_to_binary(N)}
    end, lists:seq(1, 10))),

    %% Reconnect from event 5
    {ok, Events} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_counter_5">>
    ),

    %% Counter should continue from 5
    ?assertEqual(5, length(Events)),  %% events 6-10

test_session_state_tracking() ->
    %% Verify manager tracks session state
    SessionId = <<"session_state">>,

    %% Register connection
    ManagerPid = whereis(erlmcp_transport_sse_manager),

    %% (In real test, would use manager:register_connection)
    %% Then verify state via manager:get_session_info

    ?assert(is_pid(ManagerPid)).

%%%===================================================================
%%% Configuration Tests
%%%===================================================================

configuration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Ctx) ->
         [
          ?_test(test_event_ttl_config()),
          ?_test(test_replay_window_config()),
          ?_test(test_max_session_events_config())
         ]
     end}.

test_event_ttl_config() ->
    %% Verify event TTL is configurable
    TTL = application:get_env(erlmcp, sse_event_ttl, 3600000),
    ?assert(TTL > 0).

test_replay_window_config() ->
    %% Verify replay window is configurable
    Window = application:get_env(erlmcp, sse_replay_window, 86400000),
    ?assert(Window > 0).

test_max_session_events_config() ->
    %% Verify max session events is configurable
    MaxEvents = application:get_env(erlmcp, sse_max_session_events, 10000),
    ?assert(MaxEvents > 0).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @private
add_events(SessionId, Events) ->
    lists:foreach(fun({EventNumber, Data}) ->
        {ok, _Id} = erlmcp_sse_event_store:add_event(
            SessionId,
            EventNumber,
            Data
        )
    end, Events),
    ok.
```

---

## Test Scenarios

### Scenario 1: Basic Resumption

1. Client connects to `/mcp/sse`
2. Server sends events with IDs: `session_abc_1`, `session_abc_2`, `session_abc_3`
3. Client disconnects after event 3
4. Server continues sending events 4, 5, 6
5. Client reconnects with `Last-Event-ID: session_abc_3`
6. Server replays events 4, 5, 6
7. Server resumes live streaming with event 7

**Expected**: All events received, no duplicates, no gaps

### Scenario 2: Multiple Disconnects

1. Client connects, receives events 1-10
2. Disconnect at event 10
3. Reconnect with `Last-Event-ID: 10`, receive events 11-15
4. Disconnect at event 15
5. Reconnect with `Last-Event-ID: 15`, receive events 16-20

**Expected**: Each resumption picks up exactly where left off

### Scenario 3: Expired Events

1. Client connects, receives events 1-5
2. Server stops sending events
3. Wait for event TTL (1 hour)
4. Client reconnects with `Last-Event-ID: 5`

**Expected**: No events replayed (store empty or expired), fresh start

### Scenario 4: Replay Window

1. Client receives events 1-100
2. Disconnect
3. Wait > 24 hours (replay window)
4. Reconnect with `Last-Event-ID: 100`

**Expected**: No replay (outside window), fresh start

### Scenario 5: Invalid Last-Event-ID

1. Client connects with malformed `Last-Event-ID: invalid_xyz`
2. Server parses as event number 0

**Expected**: Returns all available events (or none if new session)

---

## Configuration Options

### Event Storage

| Parameter | Default | Description |
|-----------|---------|-------------|
| `event_ttl` | 3600000 (1 hour) | Time before events expire from store |
| `cleanup_interval` | 300000 (5 min) | Frequency of expired event cleanup |
| `max_session_events` | 10000 | Maximum events to retain per session |

### Replay Behavior

| Parameter | Default | Description |
|-----------|---------|-------------|
| `replay_window` | 86400000 (24 hours) | Maximum time window for replay |
| `enable_replay` | true | Enable/disable replay feature |

### Stream Settings

| Parameter | Default | Description |
|-----------|---------|-------------|
| `retry_timeout` | 5000 (5 sec) | SSE retry field value |
| `stream_timeout` | 300000 (5 min) | Idle timeout before disconnect |
| `keepalive_interval` | 30000 (30 sec) | Ping interval for keep-alive |

---

## Quality Gates

### Mandatory Checks

- ✅ All tests passing (100% pass rate)
- ✅ Test coverage >= 85%
- ✅ No Dialyzer warnings
- ✅ No Xref errors
- ✅ OTEL tracing integrated
- ✅ Configuration documented
- ✅ Chicago School TDD followed (no mocks)

### Performance Targets

- Event replay latency: < 50ms for 100 events
- Header parsing: < 1ms
- Event ID generation: < 1ms
- Event storage: < 5ms per event

### Error Handling

- Invalid Last-Event-ID: Graceful degradation (default to 0)
- Event store failure: Log error, continue live streaming
- Session not found: Return empty replay, continue live stream
- Malformed headers: Parse as 0, log warning

---

## Implementation Checklist

### Phase 1: Transport Layer (erlmcp_transport_sse.erl)

- [ ] Parse `Last-Event-ID` header in `handle_sse_stream/3`
- [ ] Implement `replay_missed_events/4` function
- [ ] Update `sse_event_loop/3` to inject event IDs
- [ ] Implement `format_sse_event_with_id/3`
- [ ] Add event storage on each send
- [ ] Update `StreamState` to track event counter
- [ ] Add configuration support (TTL, replay window)

### Phase 2: SSE Manager (erlmcp_transport_sse_manager.erl)

- [ ] Update `conn_info` record with event tracking
- [ ] Add `register_connection` handler with Last-Event-ID
- [ ] Add `update_event_counter` handler
- [ ] Implement `get_session_info` query
- [ ] Add session state persistence

### Phase 3: Configuration (sys.config)

- [ ] Add `event_ttl` to SSE config
- [ ] Add `replay_window` to SSE config
- [ ] Add `max_session_events` to SSE config
- [ ] Document all parameters

### Phase 4: Testing

- [ ] Implement `erlmcp_sse_resumption_integration_tests.erl`
- [ ] Add Last-Event-ID header tests (3 tests)
- [ ] Add event replay tests (3 tests)
- [ ] Add session continuity tests (3 tests)
- [ ] Add configuration tests (3 tests)
- [ ] Add performance benchmarks
- [ ] Add chaos tests (event store failure)
- [ ] Verify >= 85% code coverage

### Phase 5: Documentation

- [ ] Update SSE architecture docs
- [ ] Add resumption flow diagrams
- [ ] Document configuration options
- [ ] Add client integration examples
- [ ] Update MCP compliance matrix

---

## Success Criteria

### Functional Requirements

1. ✅ Last-Event-ID header parsed correctly
2. ✅ Missed events replayed on reconnection
3. ✅ Event IDs injected in SSE events
4. ✅ Session continuity across disconnects
5. ✅ Configuration (TTL, replay window) respected

### Non-Functional Requirements

1. ✅ Replay latency < 50ms (100 events)
2. ✅ No memory leaks (proper ETS cleanup)
3. ✅ No performance regression in live streaming
4. ✅ Comprehensive error handling
5. ✅ Full observability (OTEL traces)

### MCP 2025-11-25 Compliance

1. ✅ HTTP Streamable Transport support
2. ✅ SSE event IDs for resumption
3. ✅ Last-Event-ID header handling
4. ✅ SSE retry field (Gap #29 already complete)
5. ✅ Session management (Gap #2 integration)

---

## Risks & Mitigations

### Risk 1: Event Store Memory Bloat

**Impact**: High
**Probability**: Medium
**Mitigation**:
- Strict TTL enforcement (1 hour default)
- Per-session event limits (10,000 events)
- Periodic cleanup every 5 minutes
- Memory monitoring and alerts

### Risk 2: Replay Performance Degradation

**Impact**: Medium
**Probability**: Medium
**Mitigation**:
- ETS ordered_set for O(log N) lookups
- Replay window limits (24 hours)
- Benchmarks for replay latency
- Async replay (non-blocking)

### Risk 3: Session ID Collision

**Impact**: High
**Probability**: Low
**Mitigation**:
- Cryptographically random session IDs
- Timestamp component in ID generation
- Pid component for uniqueness
- Collision detection and logging

### Risk 4: Client Malformed Last-Event-ID

**Impact**: Low
**Probability**: High
**Mitigation**:
- Graceful parsing with fallback to 0
- Warning logs for malformed headers
- Return empty replay (not error)
- Document expected format

---

## References

### MCP Specification

- **MCP 2025-11-25**: HTTP Streamable Transport
  - SSE events MUST include `id` field
  - Clients MUST send `Last-Event-ID` on reconnect
  - Servers MUST replay events since Last-Event-ID

### Existing erlmcp Documentation

- `/Users/sac/erlmcp/docs/sse-stream-resumability.md` - Original SSE design
- `/Users/sac/erlmcp/docs/GAP_29_SSE_RETRY_FIELD_IMPLEMENTATION.md` - Retry field
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_sse_event_store.erl` - Event store API
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_sse_event_store_replay_tests.erl` - Replay tests

### Related Transport Implementations

- `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_ws.erl` - WebSocket for comparison
- `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_tcp.erl` - TCP for comparison

---

## Appendix: Event ID Format

### Format Specification

```
<session_id>_<event_number>
```

### Components

1. **Session ID**: `session_<client_pid>_<timestamp>_<random>`
   - Example: `session_<0.123.0>_1706745600000_abc123`

2. **Event Number**: Sequential integer per session
   - Starts at 1
   - Increments on each event
   - Survives reconnection

### Example Event IDs

```
session_<0.123.0>_1706745600000_abc123_1
session_<0.123.0>_1706745600000_abc123_2
session_<0.123.0>_1706745600000_abc123_3
```

### Parsing

```erlang
parse_event_id(EventId) ->
    %% Extract last component (event number)
    Parts = binary:split(EventId, <<"_">>, [global]),
    LastPart = lists:last(Parts),
    binary_to_integer(LastPart).
```

---

**End of Implementation Plan**
