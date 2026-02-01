# SSE Resumption Integration Summary

**Quick Reference**: Critical integration points for SSE stream resumption in erlmcp v3

---

## Current State: DISCONNECTED

### What Works

- ✅ **Event Store**: Complete ETS-based implementation with replay capability
  - API: `add_event/3`, `get_events_since/2`, `parse_event_id/1`
  - File: `/apps/erlmcp_core/src/erlmcp_sse_event_store.erl`
  - Tests: `/apps/erlmcp_core/test/erlmcp_sse_event_store_replay_tests.erl`

### What's Missing

- ❌ **Transport Layer**: No event store integration
- ❌ **Last-Event-ID Parsing**: Header not read or processed
- ❌ **Event Replay**: No replay orchestration on reconnection
- ❌ **Event ID Injection**: SSE events missing `id:` field
- ❌ **Session State**: No tracking of event counters per session

---

## Integration Points

### Point 1: Parse Last-Event-ID Header

**Location**: `erlmcp_transport_sse.erl:handle_sse_stream/3`

**Current Code** (lines 138-178):
```erlang
handle_sse_stream(Req, TransportId, State) ->
    %% ... current code DOES NOT read Last-Event-ID header
    SessionId = generate_session_id(ClientId),
    %% No replay logic
    sse_event_loop(Req, StreamState, State).
```

**Required Change**:
```erlang
handle_sse_stream(Req, TransportId, State) ->
    %% ADD: Parse Last-Event-ID header
    LastEventId = cowboy_req:header(<<"last-event-id">>, Req),

    %% ADD: Replay missed events
    ok = replay_missed_events(Req, SessionId, LastEventId, SpanCtx),

    %% ADD: Initialize event counter from Last-Event-ID
    EventCounter = case LastEventId of
        undefined -> 0;
        _ -> erlmcp_sse_event_store:parse_event_id(LastEventId)
    end,

    sse_event_loop(Req, StreamState#{event_counter => EventCounter}, State).
```

### Point 2: Implement Replay Function

**Location**: `erlmcp_transport_sse.erl` (new function)

**Signature**:
```erlang
-spec replay_missed_events(
    cowboy_req:req(),
    SessionId :: binary(),
    LastEventId :: binary() | undefined,
    otel:span_ctx()
) -> ok.
```

**Implementation**:
```erlang
replay_missed_events(Req, SessionId, LastEventId, SpanCtx) ->
    case LastEventId of
        undefined ->
            %% No previous events
            ok;
        _ ->
            %% Fetch from event store
            case erlmcp_sse_event_store:get_events_since(SessionId, LastEventId) of
                {ok, MissedEvents} ->
                    %% Stream each missed event
                    lists:foreach(fun(EventData) ->
                        FormattedEvent = format_sse_event(<<"message">>, EventData),
                        cowboy_req:stream_body(FormattedEvent, nofin, Req)
                    end, MissedEvents),
                    ok;
                {error, Reason} ->
                    logger:error("Replay failed: ~p", [Reason]),
                    ok
            end
    end.
```

### Point 3: Inject Event IDs in SSE Events

**Location**: `erlmcp_transport_sse.erl:sse_event_loop/3`

**Current Code** (lines 225-249):
```erlang
sse_event_loop(Req, StreamState, State) ->
    receive
        {send_event, EventType, Data} ->
            %% Current: NO event ID
            EventData = format_sse_event(EventType, Data),
            cowboy_req:stream_body(EventData, nofin, Req),
            sse_event_loop(Req, StreamState, State);
    end.
```

**Required Change**:
```erlang
sse_event_loop(Req, StreamState = #{
    event_counter := Counter,
    session_id := SessionId
}, State) ->
    receive
        {send_event, EventType, Data} ->
            NewCounter = Counter + 1,

            %% ADD: Store event
            {ok, EventId} = erlmcp_sse_event_store:add_event(
                SessionId, NewCounter, Data
            ),

            %% ADD: Format WITH event ID
            EventData = format_sse_event_with_id(EventType, EventId, Data),
            cowboy_req:stream_body(EventData, nofin, Req),

            %% ADD: Update counter
            sse_event_loop(Req, StreamState#{event_counter => NewCounter}, State);
    end.
```

### Point 4: Add format_sse_event_with_id Function

**Location**: `erlmcp_transport_sse.erl` (new function)

**Signature**:
```erlang
-spec format_sse_event_with_id(EventType, EventId, Data) -> binary().
```

**Implementation**:
```erlang
format_sse_event_with_id(EventType, EventId, Data) ->
    <<"id: ", EventId/binary, "\nevent: ", EventType/binary,
      "\ndata: ", Data/binary, "\n\n">>.
```

**Output Format**:
```
id: session_<0.123.0>_1706745600000_abc123_42
event: message
data: {"jsonrpc":"2.0","method":"notification",...}

```

---

## Session State Tracking

### SSE Manager Changes

**Location**: `erlmcp_transport_sse_manager.erl`

**Required State Fields**:
```erlang
-record(conn_info, {
    session_id :: binary(),
    client_pid :: pid(),
    req :: term(),
    ping_timer :: reference(),
    last_event_number :: non_neg_integer(),  %% ADD THIS
    connected_at :: integer()                %% ADD THIS
}).
```

**Required Handlers**:
```erlang
handle_cast({register_connection, SessionId, ClientPid, Req, LastEventId}, State) ->
    LastEventNum = case LastEventId of
        undefined -> 0;
        _ -> erlmcp_sse_event_store:parse_event_id(LastEventId)
    end,

    ConnInfo = #conn_info{
        session_id = SessionId,
        client_pid = ClientPid,
        req = Req,
        ping_timer = undefined,
        last_event_number = LastEventNum,  %% TRACK THIS
        connected_at = erlang:system_time(millisecond)
    },

    {noreply, State#state{connections => maps:put(SessionId, ConnInfo, ...)}};

handle_cast({update_event_counter, SessionId, EventNumber}, State) ->
    %% UPDATE: Track event progress
    NewConnections = maps:update_with(
        SessionId,
        fun(Conn) -> Conn#conn_info{last_event_number = EventNumber} end,
        State#state.connections
    ),
    {noreply, State#state{connections => NewConnections}}.
```

---

## Configuration

### Add to sys.config

```erlang
{erlmcp, [
    {sse, [
        {enabled, true},
        {port, 8081},
        {path, "/mcp/sse"},
        {keepalive_interval, 30000},
        {stream_timeout, 300000},
        {retry_timeout, 5000},

        %% ADD THESE FOR RESUMPTION
        {event_ttl, 3600000},           %% 1 hour
        {replay_window, 86400000},      %% 24 hours
        {max_session_events, 10000},    %% Max events/session
        {cleanup_interval, 300000}      %% 5 minutes
    ]}
]}.
```

---

## Test Scenarios

### Scenario 1: Basic Resumption

**Steps**:
1. Client connects → Server sends event IDs: `_1`, `_2`, `_3`
2. Client disconnects after event 3
3. Server continues → events `_4`, `_5`, `_6`
4. Client reconnects with `Last-Event-ID: session_..._3`
5. Server replays events 4, 5, 6
6. Server resumes with event 7

**Expected**: All events received, no duplicates

### Scenario 2: Multiple Disconnects

**Steps**:
1. Client connects → receives events 1-10
2. Disconnect at 10
3. Reconnect with `Last-Event-ID: 10` → receives 11-15
4. Disconnect at 15
5. Reconnect with `Last-Event-ID: 15` → receives 16-20

**Expected**: Each resumption picks up exactly where left off

### Scenario 3: Expired Events

**Steps**:
1. Client receives events 1-5
2. Wait > event_ttl (1 hour)
3. Reconnect with `Last-Event-ID: 5`

**Expected**: No replay (events expired), fresh start

### Scenario 4: Invalid Last-Event-ID

**Steps**:
1. Client connects with `Last-Event-ID: invalid_xyz`
2. Server parses as event number 0

**Expected**: Returns all available events (graceful degradation)

---

## Files to Modify

### Core Changes

1. **`apps/erlmcp_transports/src/erlmcp_transport_sse.erl`**
   - Parse Last-Event-ID header
   - Implement `replay_missed_events/4`
   - Inject event IDs in SSE events
   - Track event counter in stream state
   - Add `format_sse_event_with_id/3`

2. **`apps/erlmcp_transports/src/erlmcp_transport_sse_manager.erl`**
   - Update `conn_info` record
   - Track last event number
   - Add session info queries

3. **`config/sys.config`**
   - Add resumption configuration options

### New Files

4. **`apps/erlmcp_transports/test/erlmcp_sse_resumption_integration_tests.erl`**
   - Last-Event-ID header tests
   - Event replay tests
   - Session continuity tests
   - Configuration tests

---

## Verification Checklist

### Functional Tests

- [ ] Last-Event-ID header parsed correctly
- [ ] Missed events replayed on reconnection
- [ ] Event IDs present in SSE events
- [ ] Session counter persists across disconnects
- [ ] Expired events not replayed
- [ ] Invalid headers handled gracefully

### Performance Tests

- [ ] Replay latency < 50ms for 100 events
- [ ] Header parsing < 1ms
- [ ] Event ID generation < 1ms
- [ ] Event storage < 5ms per event
- [ ] No memory leaks (ETS cleanup verified)

### Compliance Tests

- [ ] MCP 2025-11-25 HTTP Streamable Transport
- [ ] SSE `id:` field present
- [ ] Last-Event-ID header handling
- [ ] Session management integration
- [ ] SSE retry field (Gap #29)

---

## Quick Start Implementation

### Step 1: Add Event Store Calls (5 min)

```erlang
%% In handle_sse_stream/3
LastEventId = cowboy_req:header(<<"last-event-id">>, Req),

%% In sse_event_loop/3
{ok, EventId} = erlmcp_sse_event_store:add_event(SessionId, Counter, Data),
```

### Step 2: Add Replay Function (10 min)

```erlang
replay_missed_events(Req, SessionId, LastEventId, SpanCtx) ->
    case erlmcp_sse_event_store:get_events_since(SessionId, LastEventId) of
        {ok, Events} -> stream_events(Events);
        {error, _} -> ok
    end.
```

### Step 3: Update Event Format (5 min)

```erlang
format_sse_event_with_id(EventType, EventId, Data) ->
    <<"id: ", EventId/binary, "\nevent: ", EventType/binary,
      "\ndata: ", Data/binary, "\n\n">>.
```

### Step 4: Add Tests (30 min)

```bash
# Create test file
apps/erlmcp_transports/test/erlmcp_sse_resumption_integration_tests.erl

# Run tests
rebar3 eunit --module=erlmcp_sse_resumption_integration_tests
```

### Step 5: Verify (10 min)

```bash
# Compile
rebar3 compile

# Test
rebar3 eunit --module=erlmcp_sse_resumption_integration_tests

# Coverage
rebar3 cover --verbose
```

**Total Estimated Time**: 60 minutes

---

## Success Metrics

| Metric | Target | How to Measure |
|--------|--------|----------------|
| Replay Success Rate | 100% | All reconnection tests pass |
| Replay Latency (p95) | < 50ms | Benchmark 100-event replay |
| Event ID Coverage | 100% | All SSE events have `id:` field |
| Test Coverage | >= 85% | `rebar3 cover` |
| Memory Leaks | 0 | ETS cleanup verified |

---

## References

- **Full Plan**: `/docs/v3/00_sse_resumption_plan.md`
- **Event Store**: `/apps/erlmcp_core/src/erlmcp_sse_event_store.erl`
- **Event Store Tests**: `/apps/erlmcp_core/test/erlmcp_sse_event_store_replay_tests.erl`
- **SSE Architecture**: `/docs/sse-stream-resumability.md`
- **Gap #29 (Retry Field)**: `/docs/GAP_29_SSE_RETRY_FIELD_IMPLEMENTATION.md`

---

**Status**: 📋 Design Complete
**Next Steps**: Implementation (60 min estimate)
**Priority**: P0 - MCP 2025-11-25 Compliance
