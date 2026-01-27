# SSE Stream Resumability - Architecture Overview

## System Architecture

### High-Level Component Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                      Client Application                          │
│                                                                   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │              Browser/JavaScript/Erlang                    │   │
│  │                                                            │   │
│  │  • Track Last-Event-ID from SSE stream                   │   │
│  │  • Store in localStorage/process state                   │   │
│  │  • Send Last-Event-ID on reconnect (HTTP header)        │   │
│  │  • Handle auto-reconnection (browser or custom)         │   │
│  └──────────────────────────────────────────────────────────┘   │
└────────┬────────────────────────────────────────────────────────┘
         │
         │ HTTP/SSE
         │
┌────────▼────────────────────────────────────────────────────────┐
│                      erlmcp_transport_sse                        │
│                                                                   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  handle_sse_stream/3                                     │   │
│  │  • Extract Last-Event-ID header                          │   │
│  │  • Create unique SessionId                               │   │
│  │  • Initialize EventNumber counter                        │   │
│  │  • If Last-Event-ID: call handle_stream_resumption      │   │
│  │  • Enter sse_event_loop                                  │   │
│  └──────────────────────────────────────────────────────────┘   │
│                                                                   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  sse_event_loop/3                                        │   │
│  │  • Ping keepalive every 30s                              │   │
│  │  • For each {send_event, Data}:                          │   │
│  │    - Increment EventNumber                               │   │
│  │    - Call erlmcp_sse_event_store:add_event(...)         │   │
│  │    - Get back EventId                                    │   │
│  │    - Format with event ID                                │   │
│  │    - Send to client                                      │   │
│  └──────────────────────────────────────────────────────────┘   │
│                                                                   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  handle_stream_resumption/7                              │   │
│  │  • Call get_events_since(SessionId, LastEventId)         │   │
│  │  • Replay all retrieved events in order                  │   │
│  │  • Continue with sse_event_loop                          │   │
│  └──────────────────────────────────────────────────────────┘   │
└────────┬────────────────────────────────────────────────────────┘
         │
         │ gen_server calls
         │
┌────────▼────────────────────────────────────────────────────────┐
│                erlmcp_sse_event_store (gen_server)              │
│                                                                   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  Public API                                              │   │
│  │  • add_event/3                                           │   │
│  │    Returns: {ok, EventId} or {error, Reason}            │   │
│  │                                                          │   │
│  │  • get_events_since/2                                    │   │
│  │    Returns: {ok, [Events]} or {error, Reason}           │   │
│  │                                                          │   │
│  │  • cleanup_expired/0                                     │   │
│  │    Runs every 5 minutes automatically                   │   │
│  │                                                          │   │
│  │  • clear_session/1                                       │   │
│  │    Removes all events for session                        │   │
│  │                                                          │   │
│  │  • parse_event_id/1                                      │   │
│  │    Extract event number from ID                          │   │
│  └──────────────────────────────────────────────────────────┘   │
│                                                                   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  Internal State (ETS Table)                              │   │
│  │                                                          │   │
│  │  Table: erlmcp_sse_events                                │   │
│  │  Type: ordered_set, public, named                        │   │
│  │                                                          │   │
│  │  Records:                                                │   │
│  │  • sse_event                                             │   │
│  │    - event_id (key)                                      │   │
│  │    - session_id                                          │   │
│  │    - event_number                                        │   │
│  │    - data                                                │   │
│  │    - timestamp                                           │   │
│  │                                                          │   │
│  │  • sse_session                                           │   │
│  │    - session_id (key)                                    │   │
│  │    - created_at                                          │   │
│  │    - last_event_number                                   │   │
│  │    - event_count                                         │   │
│  └──────────────────────────────────────────────────────────┘   │
│                                                                   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  Cleanup Process                                         │   │
│  │  • Timer: every 5 minutes (300,000 ms)                  │   │
│  │  • Find events older than 1 hour                         │   │
│  │  • Delete expired events                                 │   │
│  │  • Async, non-blocking                                   │   │
│  └──────────────────────────────────────────────────────────┘   │
│                                                                   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  Enforcement (per session)                               │   │
│  │  • Max 100 events stored                                 │   │
│  │  • TTL: 1 hour                                           │   │
│  │  • Remove oldest when limit exceeded                     │   │
│  └──────────────────────────────────────────────────────────┘   │
└────────────────────────────────────────────────────────────────┘
```

## Data Flow Sequence

### Initial Stream

```
┌─────┐                      ┌──────────┐              ┌─────────┐
│ CLI │                      │ Transport│              │ Store   │
└──┬──┘                      └────┬─────┘              └────┬────┘
   │                             │                         │
   │ GET /mcp/sse                │                         │
   ├────────────────────────────►│                         │
   │                             │ init                    │
   │                             ├────────────────────────►│
   │                             │ {ok, []}               │
   │                             │◄────────────────────────┤
   │                             │                         │
   │                             │ set event loop         │
   │                             │ event_number = 0       │
   │                             │                         │
   │◄────────200 OK + headers───┤                         │
   │  (text/event-stream)        │                         │
   │                             │                         │
   │ (server sends events)       │                         │
   │                             │ {send_event, Data}     │
   │                             ├────────────────────────►│
   │ id: session_1_1             │ add_event(...)         │
   │ data: {...}                 │ {ok, "session_1_1"}    │
   │                             │◄────────────────────────┤
   │◄── event + event ID ────────┤                         │
   │                             │                         │
```

### Resumption After Disconnect

```
┌─────┐                      ┌──────────┐              ┌─────────┐
│ CLI │                      │ Transport│              │ Store   │
└──┬──┘                      └────┬─────┘              └────┬────┘
   │                             │                         │
   │ [Disconnected]              │                         │
   │ Store: lastEventId = "_2"   │                         │
   │                             │ [Events stored: 1-5]   │
   │                             │                         │
   │ GET /mcp/sse                │                         │
   │ Last-Event-ID: session_1_2  │                         │
   ├────────────────────────────►│                         │
   │                             │ Extract header         │
   │                             │ Get events since _2    │
   │                             ├────────────────────────►│
   │                             │ Match: _3, _4, _5      │
   │                             │ {ok, [data3, ...]}     │
   │                             │◄────────────────────────┤
   │                             │                         │
   │ id: session_1_3 (replay)    │                         │
   │ data: {...}                 │                         │
   │◄── event ─────────────────┤                         │
   │                             │                         │
   │ id: session_1_4 (replay)    │                         │
   │ data: {...}                 │                         │
   │◄── event ─────────────────┤                         │
   │                             │                         │
   │ id: session_1_5 (replay)    │                         │
   │ data: {...}                 │                         │
   │◄── event ─────────────────┤                         │
   │                             │                         │
   │ [Normal streaming resumes]  │                         │
```

## Event ID Structure

```
Event ID Format: session_id_event_number

Examples:
┌───────────────────────────┐
│ session_abc123_1          │  ◄─── First event
│ session_abc123_2          │  ◄─── Second event
│ session_abc123_42         │  ◄─── 42nd event
│ session_xyz_9999999       │  ◄─── Very large number
└───────────────────────────┘

Parsing:
Input:  "session_abc123_def456_42"
Parts:  ["session", "abc123", "def456", "42"]
Extract: last part = 42
Returns: 42 (event number)
```

## Storage Structure

### ETS Table Layout

```
┌──────────────────────────────────────────────────┐
│         erlmcp_sse_events (ordered_set)         │
├──────────────────────────────────────────────────┤
│                                                   │
│ sse_event records:                               │
│ ┌────────────────────────────────────────────┐  │
│ │ event_id: "session_1_1"                    │  │
│ │ session_id: "session_1"                    │  │
│ │ event_number: 1                            │  │
│ │ data: "{...}"                              │  │
│ │ timestamp: 1711234567890                   │  │
│ └────────────────────────────────────────────┘  │
│ ┌────────────────────────────────────────────┐  │
│ │ event_id: "session_1_2"                    │  │
│ │ session_id: "session_1"                    │  │
│ │ event_number: 2                            │  │
│ │ data: "{...}"                              │  │
│ │ timestamp: 1711234568890                   │  │
│ └────────────────────────────────────────────┘  │
│ ... (up to 100 per session)                      │
│                                                   │
│ sse_session records (metadata):                  │
│ ┌────────────────────────────────────────────┐  │
│ │ session_id: "session_1"                    │  │
│ │ created_at: 1711234567890                  │  │
│ │ last_event_number: 42                      │  │
│ │ event_count: 42                            │  │
│ └────────────────────────────────────────────┘  │
│                                                   │
└──────────────────────────────────────────────────┘
```

## State Machine

### Transport State

```
         ┌─────────────┐
         │   START     │
         └──────┬──────┘
                │ init/2
                │
         ┌──────▼──────────┐
         │ WAITING_REQUEST │  Extract headers
         └──────┬──────────┘  Generate session_id
                │
         ┌──────┴──────────┐
         │                 │
    Last-Event-ID       No header
         │                 │
    ┌────▼─────┐      ┌────▼─────┐
    │ RESUMING  │      │ FRESH    │
    └────┬─────┘      └────┬─────┘
         │                 │
    Replay events     event_number = 0
         │                 │
         └────┬────────────┘
              │
         ┌────▼──────────┐
         │  EVENT_LOOP   │  • Send events
         │               │  • Keep-alive
         │               │  • Handle close
         └────┬──────────┘
              │
         ┌────▼──────────┐
         │   CLOSED      │
         │               │  • Send close event
         │               │  • Send retry hint
         └───────────────┘
```

### Event Store State

```
         ┌─────────────┐
         │   START     │
         └──────┬──────┘
                │ init/1
                │
         ┌──────▼──────────────┐
         │  RUNNING             │
         │  ETS table created   │
         │  Cleanup timer set   │
         └──────┬───────────────┘
                │
         ┌──────┴──────────────────────┐
         │                             │
    {add_event}                  {cleanup_timer}
         │                             │
    ┌────▼──────┐              ┌──────▼──────┐
    │ Store     │              │ Cleanup     │
    │ Event     │              │ Expired     │
    └────┬──────┘              └──────┬──────┘
         │                            │
    Update session           Delete old events
    Enforce limits                    │
         │                            │
         └────────────┬───────────────┘
                      │
         ┌────────────▼──────┐
         │  READY            │
         │ (continue loop)   │
         └───────────────────┘
```

## Error Handling Paths

```
┌──────────────────┐
│ Invalid Event ID │
└────────┬─────────┘
         │
         ▼
    ┌─────────────────────┐
    │ parse_event_id/1    │
    │ Returns: 0          │
    │ (default safe value)│
    └─────────────────────┘
         │
         ▼
    Client gets all events
    (safe fallback)

┌──────────────────────┐
│ Session Not Found    │
└────────┬─────────────┘
         │
         ▼
    ┌──────────────────────┐
    │ get_events_since/2   │
    │ Returns: {ok, []}    │
    │ (empty list)         │
    └──────────────────────┘
         │
         ▼
    Fresh stream starts

┌──────────────────────┐
│ Stream Interrupted   │
└────────┬─────────────┘
         │
         ▼
    ┌──────────────────────┐
    │ Browser auto-retry   │
    │ after 3000 ms        │
    │ (retry: 3000)        │
    └──────────────────────┘
         │
         ▼
    Reconnect with
    Last-Event-ID
```

## Performance Characteristics

```
Operation               Time Complexity    Space
─────────────────────────────────────────────────
add_event/3             O(log n)           ~200 bytes
get_events_since/2      O(log n) + O(k)    O(k) result
cleanup_expired/0       O(n)               O(k) deletions
parse_event_id/1        O(m)               O(1) (m=string_len)
─────────────────────────────────────────────────

Memory per Event:
- event_id: 40 bytes
- session_id: 20 bytes
- event_number: 8 bytes
- data: variable (JSON)
- timestamp: 8 bytes
- Overhead: ~100 bytes
Total: ~200 bytes (without data)

Example: 100 events × 200 bytes = 20 KB/session
```

## Supervision Integration

```
┌────────────────────────────────────┐
│         erlmcp_sup                 │
│      (one_for_all strategy)        │
└────────┬───────────────────────────┘
         │
    ┌────┴─────────────────────────────────────┐
    │                                           │
┌───▼──────────┐   ┌─────────────────────────┐
│ Registry     │   │ SSE Event Store (NEW)   │
│ gen_server   │   │ erlmcp_sse_event_store  │
└──────────────┘   │                         │
                   │ • Manage events         │
                   │ • Cleanup timer         │
                   │ • ETS table             │
                   └─────────────────────────┘

Other supervisors:
├── erlmcp_server_sup (simple_one_for_one)
├── erlmcp_transport_sup (simple_one_for_one)
├── erlmcp_client_sup (simple_one_for_one)
└── ...
```

---

**Document Purpose:** Visual architecture reference for SSE resumability implementation

**Last Updated:** 2026-01-27
