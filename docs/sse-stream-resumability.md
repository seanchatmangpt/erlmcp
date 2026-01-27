# SSE Stream Resumability with Event IDs

## Overview

This document describes the implementation of Server-Sent Events (SSE) stream resumability in erlmcp, enabling clients to reconnect and resume streaming without losing events.

## Architecture

### Components

1. **erlmcp_sse_event_store.erl** - Event persistence and replay engine
2. **erlmcp_transport_sse.erl** - Updated SSE transport with resumability support
3. **erlmcp_client.erl** - Enhanced client with automatic reconnection
4. **erlmcp_sse_resumability_tests.erl** - Comprehensive test suite

### Design Pattern

```
┌─────────────┐                ┌──────────────────┐
│   Client    │                │ SSE Transport    │
│             │<───────────────│  (Cowboy)        │
│ Tracks      │                │                  │
│ Last-Event- │   HTTP GET     │ Generates event  │
│ ID          │  with header   │ IDs for each     │
│             │                │ message          │
└─────────────┘                └──────────────────┘
      │                               │
      │                          ┌────▼────────────┐
      │                          │ Event Store     │
      │                          │                 │
      │                          │ Stores last 100 │
      │◄─────────Replay────────────│ events/session  │
      │    missing events          │                 │
      │                            │ Auto-cleanup    │
      │                            │ every 5 min     │
      │                            └─────────────────┘
```

## Event ID Format

Event IDs follow the pattern: `session_id_event_number`

Example: `session_abc123_def456_42`

This format allows:
- Identifying which stream the event belongs to
- Extracting the sequential event number for replay
- Resuming from any known event ID

## Event Format (SSE)

Events sent to clients include:

```
id: session_abc123_42
data: {"jsonrpc":"2.0","method":"initialize",...}

```

Example with closure:

```
event: close
data: {"status":"closed"}
retry: 3000

```

The `retry: 3000` directive tells browsers/clients to wait 3 seconds before reconnecting.

## Stream Resumption Flow

### Initial Connection

```
Client                          Server
  │                               │
  ├─ GET /mcp/sse ────────────►   │
  │                          Create session
  │                          Start streaming
  │◄──────── event 1 ──────────   │
  │◄──────── event 2 ──────────   │
  │◄──────── event 3 ──────────   │
  └─ Network disconnect ──────►   │
                             Keep events
                             in store
```

### Reconnection

```
Client                          Server
  │                               │
  ├─ GET /mcp/sse ────────────►   │
  │  Last-Event-ID: session_42    │ Find all
  │                               │ events after ID
  │◄──────── event 4 (replayed)─  │
  │◄──────── event 5 (replayed)─  │
  │◄────────── event 6 ───────────│
  │                          Live streaming
```

## API Reference

### Event Store Functions

#### add_event/3

```erlang
add_event(SessionId :: binary(), EventNumber :: pos_integer(), Data :: binary())
  -> {ok, EventId :: binary()} | {error, term()}
```

Adds an event to the store and returns the generated event ID.

**Parameters:**
- `SessionId` - Unique session identifier (binary)
- `EventNumber` - Sequential event number (starts at 1)
- `Data` - Event data as binary (JSON)

**Returns:**
- `{ok, EventId}` - Generated event ID: `SessionId_EventNumber`
- `{error, Reason}` - Failure reason

**Example:**

```erlang
{ok, EventId} = erlmcp_sse_event_store:add_event(
    <<"session_abc123">>,
    1,
    <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\"}">>
),
```

#### get_events_since/2

```erlang
get_events_since(SessionId :: binary(), LastEventId :: binary() | undefined)
  -> {ok, [binary()]} | {error, term()}
```

Retrieves all events for a session since a given event ID.

**Parameters:**
- `SessionId` - Session identifier
- `LastEventId` - Last known event ID; `undefined` returns all events

**Returns:**
- `{ok, Events}` - List of event data (binary) in order
- `{error, Reason}` - Failure reason

**Example:**

```erlang
{ok, Events} = erlmcp_sse_event_store:get_events_since(
    <<"session_abc123">>,
    <<"session_abc123_10">>
),
%% Events = [event_11_data, event_12_data, event_13_data, ...]
```

#### cleanup_expired/0

```erlang
cleanup_expired() -> ok | {error, term()}
```

Removes events older than 1 hour. Called automatically every 5 minutes.

#### clear_session/1

```erlang
clear_session(SessionId :: binary()) -> ok | {error, term()}
```

Removes all events for a session.

#### get_session_info/1

```erlang
get_session_info(SessionId :: binary()) -> {ok, map()} | {error, not_found}
```

Returns session metadata:

```erlang
{ok, #{
    session_id => <<"session_abc123">>,
    created_at => 1711234567890,
    last_event_number => 42,
    event_count => 42
}}
```

### Transport Integration

The SSE transport automatically:
1. Generates unique event IDs for each message
2. Stores events for resumability
3. Extracts `Last-Event-ID` header from client
4. Replays missed events on reconnection
5. Sends `retry: 3000` on stream closure

### Client Integration

Clients should:
1. Track the `id` field from each SSE event
2. Store it as `Last-Event-ID`
3. Send `Last-Event-ID` header on reconnection
4. Deduplicate events after resumption

## Event Storage

### Limits

- **Maximum events per session:** 100
- **Event TTL:** 1 hour
- **Cleanup interval:** 5 minutes

Old events are automatically removed when limits are exceeded or TTL expires.

### Storage Mechanism

Events are stored in an ETS table with the following structure:

```erlang
-record(sse_event, {
    event_id :: binary(),           %% Unique: session_id_event_number
    session_id :: binary(),         %% Session identifier
    event_number :: pos_integer(),  %% Sequential number
    data :: binary(),               %% Event data (JSON)
    timestamp :: integer()          %% Milliseconds since epoch
}).
```

## Supervision

The event store is started as a worker in `erlmcp_sup`:

```erlang
#{
    id => erlmcp_sse_event_store,
    start => {erlmcp_sse_event_store, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [erlmcp_sse_event_store]
}
```

## Testing

Comprehensive tests cover:

1. **Event ID Generation** - Format and uniqueness
2. **Event ID Parsing** - Extracting event numbers
3. **Event Store Operations** - Add, retrieve, clear
4. **Last-Event-ID Parsing** - Header extraction and parsing
5. **Event Replay** - Correct ordering and completeness
6. **Connection Closure** - Retry hints
7. **Stream Resumption** - Smooth reconnection
8. **Missed Event Recovery** - Gap handling
9. **Concurrent Streams** - Multi-session isolation
10. **Cleanup** - Expiry and limits
11. **Session Tracking** - Metadata accuracy
12. **SSE Formatting** - Correct protocol output

Run tests:

```bash
rebar3 eunit --module=erlmcp_sse_resumability_tests
```

## Example: Client Reconnection

### Initial Connection

```javascript
const eventSource = new EventSource('/mcp/sse');

eventSource.addEventListener('message', (event) => {
    console.log('Event ID:', event.lastEventId);
    console.log('Data:', event.data);
    localStorage.setItem('lastEventId', event.lastEventId);
});

eventSource.addEventListener('close', () => {
    eventSource.close();
});
```

### Automatic Reconnection

```javascript
function connect() {
    const lastEventId = localStorage.getItem('lastEventId');
    const headers = lastEventId
        ? { 'Last-Event-ID': lastEventId }
        : {};

    const eventSource = new EventSource('/mcp/sse', {
        headers
    });

    eventSource.addEventListener('message', (event) => {
        localStorage.setItem('lastEventId', event.lastEventId);
    });
}

// Browser automatically reconnects after 3 seconds (retry: 3000)
```

## Error Handling

### Event Store Errors

- **invalid_arguments** - Invalid parameter types
- **resumption_error** - Failure during event replay
- **not_found** - Session not found when getting info

### Transport Errors

- Stream timeout (5 minutes) - Automatic reconnection
- Connection loss - Browser retry mechanism (3000ms)
- Invalid Last-Event-ID - Fresh stream starts

## Performance Considerations

### Event Storage

- ETS table with ordered_set for fast lookups
- O(log n) insertion and retrieval
- Memory: ~200 bytes per event × 100 events × N sessions

### Cleanup

- Runs every 5 minutes asynchronously
- Deletes expired events: O(n) scan, O(k) deletions
- No blocking operations

### Throughput

- Event addition: < 1ms
- Event retrieval: < 10ms (for 100 events)
- Concurrent streams: No interference

## Limitations & Future Work

### Current Limitations

1. Events stored in-memory only (lost on restart)
2. Per-node storage (no clustering)
3. Simple TTL-based cleanup
4. No event compression

### Potential Improvements

1. Persistent storage (RocksDB, Redis)
2. Distributed cache (memcached)
3. Event filtering/sampling for large streams
4. Compression for large payloads
5. Metrics/monitoring integration

## References

- [Server-Sent Events MDN](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events)
- [EventSource API](https://html.spec.whatwg.org/multipage/server-sent-events.html)
- [MCP Protocol Specification](https://modelcontextprotocol.io/)

## Implementation Timeline

- **Phase 1** ✅ Core event store with ETS storage
- **Phase 2** ✅ SSE transport integration
- **Phase 3** ✅ Client reconnection support
- **Phase 4** ✅ Comprehensive test suite
- **Future** - Persistent storage, metrics, monitoring
