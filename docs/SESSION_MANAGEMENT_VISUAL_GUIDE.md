# Session Management Visual Guide

Visual diagrams and illustrations for MCP session management concepts.

---

## 1. Session Lifecycle States

```
                    ┌─────────────────────────────────────────┐
                    │   Client.new(transport_opts)            │
                    └─────────────────┬───────────────────────┘
                                      │
                                      ▼
                    ┌──────────────────────────────────────┐
                    │   pre_initialization                 │
                    │   (Ready for initialize)             │
                    └──────────────┬───────────────────────┘
                                   │
              ┌────────────────────┘
              │ erlmcp_client:initialize(Client, Caps)
              ▼
        ┌──────────────────────────────────────┐
        │   initializing                       │
        │   (Waiting for server response)      │
        └──────────┬───────────────────────────┘
                   │
      ┌────────────┴────────────┐
      │                         │
   SUCCESS                   TIMEOUT/ERROR
      │                         │
      ▼                         ▼
┌──────────────────────┐  ┌──────────────┐
│   initialized        │  │   error      │
│   (Ready for API)    │  │   (Failed)   │
└──────────┬───────────┘  └──────────────┘
           │
      erlmcp_client:stop(Client)
           │
           ▼
     ┌──────────────┐
     │   closed     │
     │   (Cleanup)  │
     └──────────────┘
```

---

## 2. Session State Components

```
Session = {
    ┌─ Connection State ─────────────────────────────────┐
    │ transport: erlmcp_transport_stdio                   │
    │ transport_state: #{fd_in: 0, fd_out: 1}            │
    │ phase: initialized                                 │
    │ last_activity: 1640995300000 (ms since epoch)      │
    └────────────────────────────────────────────────────┘

    ┌─ Capability State ─────────────────────────────────┐
    │ client_capabilities: #mcp_client_capabilities{...} │
    │ server_capabilities: #mcp_server_capabilities{...} │
    └────────────────────────────────────────────────────┘

    ┌─ Request State ────────────────────────────────────┐
    │ request_id: 42                                     │
    │ pending_requests: #{                               │
    │   1 -> {tools_list, pid_a},                        │
    │   2 -> {resources_read, pid_b},                    │
    │   3 -> {tool_call, pid_c}                          │
    │ }                                                  │
    └────────────────────────────────────────────────────┘

    ┌─ Subscription State ───────────────────────────────┐
    │ subscriptions: [<<"weather://nyc">>,               │
    │                 <<"news://tech">>]                 │
    │ notification_handlers: #{...}                      │
    └────────────────────────────────────────────────────┘

    ┌─ Metadata ─────────────────────────────────────────┐
    │ user_id: <<"user_123">>                            │
    │ client_info: #{name => <<"claude">>}               │
    │ created_at: 1640995200000 (ms)                     │
    │ last_accessed: 1640995300000 (ms)                  │
    │ timeout_ms: 1800000 (30 minutes)                   │
    └────────────────────────────────────────────────────┘
}
```

---

## 3. Request Correlation Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                           CLIENT                                │
│                                                                 │
│  send_request(tools/list)                                       │
│  ├─ RequestId = 42                                              │
│  ├─ pending_requests[42] = {tools_list, self()}                 │
│  └─ Send: {id: 42, method: "tools/list"}                        │
└──────────────────────┬──────────────────────────────────────────┘
                       │
                       │ JSON-RPC over transport
                       │
                       ▼
┌──────────────────────────────────────────────────────────────────┐
│                          SERVER                                  │
│                                                                  │
│  Receive: {id: 42, method: "tools/list"}                         │
│  ├─ Get session from store                                       │
│  ├─ Validate capabilities                                        │
│  ├─ Execute tools/list handler                                   │
│  └─ Send: {id: 42, result: [...]}  ← ECHOES RequestId            │
└──────────────────────┬───────────────────────────────────────────┘
                       │
                       │ JSON-RPC over transport
                       │
                       ▼
┌──────────────────────────────────────────────────────────────────┐
│                           CLIENT                                 │
│                                                                  │
│  Receive: {id: 42, result: [...]}                                │
│  ├─ Look up pending_requests[42]                                 │
│  ├─ Found: {tools_list, self()}                                  │
│  ├─ Remove from pending_requests                                 │
│  └─ Send result to caller                                        │
└──────────────────────────────────────────────────────────────────┘
```

---

## 4. Session Persistence Backends

```
SPEED vs DURABILITY Tradeoff

                        Speed (Operations/sec)
                        ↑
                  100M  │
                        │  ETS (in-memory)
                   10M  │  ████████████████
                        │  (1-5 µs, lost on restart)
                    1M  │
                        │
                  100K  │        DETS (disk)
                        │        ████████
                   10K  │        (100-500 µs)
                        │
                    1K  │           Mnesia (distributed)
                        │           ████████
                  100   │           (50-200 µs)
                        │
                   10   │                Redis (external)
                        │                ████
                    1   └─────────────────────────────────────────>
                        None    File    RAM    Network
                        (Lost) (DETS) (Mnesia) (Redis)
                                    Durability
```

**Use Cases:**

```
DEVELOPMENT:     ETS         ETS         ETS
                 Fast        Portable    Simple
                 ────────────────────────────
                 (No persistence needed)

PRODUCTION:      DETS        Mnesia      Redis
SINGLE NODE      File-based  Cluster-    External
                 Persistent  ready
                 ────────────────────────────
                 (1 node)    (3+ nodes)  (Distributed)
```

---

## 5. Session Expiration Timeline

```
Session Created
│
├─ t=0ms
│  created_at: 1640995200000
│  last_accessed: 1640995200000
│  timeout_ms: 1800000 (30 minutes)
│
├─ t=600000ms (10 minutes)
│  [Client accesses resource]
│  last_accessed: 1640995800000 (RESET TIMER)
│
├─ t=1200000ms (20 minutes from creation)
│  [No activity for 10 minutes]
│  (Still not expired: 1200000ms - 1640995800000 < 1800000)
│
├─ t=2400000ms (40 minutes from creation)
│  EXPIRED! (2400000 - 1640995800000 > 1800000)
│  Session removed by cleanup_expired()
│
└─ t=2460000ms (cleanup runs)
   Cleanup process:
   ├─ Scan all sessions
   ├─ Check: (now - last_accessed) > timeout_ms
   ├─ Delete expired sessions
   └─ Log: "Cleanup removed 5 expired sessions"

Cleanup Interval: 60 seconds (default)

Timeline:
  0s   60s  120s 180s 240s
  │    │    │    │    │
  ├────┼────┼────┼────┼────→
  │ #1 │ #2 │ #3 │ #4 │ #5
     ↑      ↑      ↑
     └──────┴──────┴─ Cleanup runs every 60s
```

---

## 6. Multi-Session Isolation

```
Server Process
│
└─ erlmcp_session_manager (gen_server)
   │
   ├─ Session A (User 1)
   │  ├─ subscriptions: [weather://nyc]
   │  ├─ pending_requests: {1 -> tool_call}
   │  └─ capabilities: [resources, tools]
   │  └─ cleanup_timeout: 1800000ms
   │
   ├─ Session B (User 2)
   │  ├─ subscriptions: [weather://sf, news://tech]
   │  ├─ pending_requests: {1 -> tools_list, 2 -> resources_read}
   │  └─ capabilities: [resources, prompts]
   │  └─ cleanup_timeout: 3600000ms  (Different!)
   │
   └─ Session C (User 3)
      ├─ subscriptions: [sports://nba]
      ├─ pending_requests: {1 -> tool_call}
      └─ capabilities: [tools, prompts]
      └─ cleanup_timeout: 300000ms  (Different!)

Isolation Guarantees:
✓ Each session has independent cleanup timer
✓ Cleanup of Session A doesn't affect Session B or C
✓ Subscription to weather://nyc only in Session A
✓ Session C's cleanup at 300s doesn't affect Session A (1800s)
```

---

## 7. Request-Response Ordering with Async

```
Client sends 3 requests (ordering not guaranteed):

  Request 1 (id=1)    Request 2 (id=2)    Request 3 (id=3)
  tools/list          resources/read      tool/call
       │                   │                   │
       └───────┬───────────┼───────────────────┘
               │           │
               ▼           ▼
          Transport Queue (async)
               │
               └─→ [1, 3, 2]  ← Server may process in any order!

Server processes in order: 3, 1, 2

Response 3 (id=3) arrives first
Response 1 (id=1) arrives second
Response 2 (id=2) arrives third

But CLIENT uses request ID to match responses!

┌─────────────────────────────────────────┐
│ pending_requests                        │
├─────────────────────────────────────────┤
│ {1: {tools_list, pid_a},                │
│  2: {resources_read, pid_b},            │
│  3: {tool_call, pid_c}}                 │
└─────────────────────────────────────────┘

Receive Response 3 → Look up 3 → Found → Notify pid_c
Receive Response 1 → Look up 1 → Found → Notify pid_a
Receive Response 2 → Look up 2 → Found → Notify pid_b

ORDER RESTORED!
```

---

## 8. Stateless vs Stateful Architecture

```
STATELESS ARCHITECTURE:

Client                              Server
   │                                  │
   ├─ tools/call                      │
   │  + jwt: "eyJ..."  ───────────→   │ Validate JWT
   │  + user_id: "123"                │ Extract user from JWT
   │  + args: {...}                   │ Execute tool (no lookup)
   │                                  │ No session store
   └─ Response ←───────────────────   │


STATEFUL ARCHITECTURE:

Client                              Server + Session Store
   │                                  │
   ├─ initialize ──────────────────→  │ Create session
   │ ← session_id: "abc123..." ──     │
   │                                  │
   ├─ tools/call                      │
   │  + session_id: "abc123..."  ──→  │ Look up session
   │  + args: {...}                   │ Validate auth (from session)
   │                                  │ Execute tool
   │                                  │ Update session (last_accessed)
   │  ← Response ─────────────────     │
   │                                  │


HYBRID ARCHITECTURE (RECOMMENDED):

Client                              Server + Session Store
   │                                  │
   ├─ initialize ──────────────────→  │ Create session
   │ ← session_id: "abc123..." ──     │ Store capabilities
   │                                  │
   ├─ tools/call                      │
   │  + session_id: "abc123..."  ──→  │ Look up session
   │  + jwt: "eyJ..."                 │ Validate JWT (stateless auth)
   │  + args: {...}                   │ Check capabilities (from session)
   │                                  │ Execute tool (complete in request)
   │  ← Response ─────────────────     │
   │                                  │
   └─ subscribe to resource ──────→   │ Add to session subscriptions
     (Subscription state on server)   │


Trade-offs:

           Stateless    Stateful    Hybrid
           ─────────    ────────    ──────
Payload    Large        Small       Medium
Scaling    Excellent    Poor        Good (if replicated)
Features   Limited      Rich        Rich
Complexity Simple       Complex     Moderate
```

---

## 9. Failover Architecture

```
NORMAL OPERATION:

Primary Node (node1)          Backup Node (node2)        Backup Node (node3)
┌──────────────────────┐      ┌─────────────────┐       ┌─────────────────┐
│ Session A            │      │ Session A       │       │ Session A       │
│ Session B            │ ───→ │ (replica)       │ ←──── │ (replica)       │
│ Session C            │      │ Replication lag │       │ Replication lag │
└──────────────────────┘      │ <100ms          │       │ <100ms          │
        │                     └─────────────────┘       └─────────────────┘
        │                              ▲
        └──────────────────────────────┘
         Periodic replication
         (every 20ms batch)


NODE1 FAILURE (node1 down):

        ❌ node1 (FAILED)             node2 (ALIVE)          node3 (ALIVE)
        ┌──────────────────┐      ┌─────────────────┐       ┌─────────────────┐
        │ Session A (lost) │      │ Session A       │       │ Session A       │
        │ Session B (lost) │  ✗   │ (replica)       │   ✓   │ (replica)       │
        │ Session C (lost) │      │ Complete copy   │       │ Complete copy   │
        └──────────────────┘      └─────────────────┘       └─────────────────┘
                                          ▲
                                          │
                                    PROMOTE node2
                                    to PRIMARY


AFTER FAILOVER:

        ❌ node1 (FAILED)        node2 (PRIMARY) ←──    node3 (BACKUP)
        ┌──────────────────┐      ┌─────────────────┐       ┌─────────────────┐
        │ (offline)        │      │ Session A       │ ───→  │ Session A       │
        │                  │  ✗   │ Session B       │   ✓   │ Session B       │
        │                  │      │ Session C       │       │ Session C       │
        └──────────────────┘      └─────────────────┘       └─────────────────┘
                                   (continues operation
                                    with clients)


RECOVERY (node1 comes back):

        node1 (RECOVERED)       node2 (PRIMARY)          node3 (BACKUP)
        ┌──────────────────┐      ┌─────────────────┐       ┌─────────────────┐
        │ (stale copies)   │      │ Session A       │ ───→  │ Session A       │
        │ (waiting for     │  ←   │ Session B       │   ✓   │ Session B       │
        │  sync)           │      │ Session C       │       │ Session C       │
        └──────────────────┘      └─────────────────┘       └─────────────────┘
                                        │
                                        └─→ Sync sessions to node1
                                           (node1 becomes BACKUP again)
```

---

## 10. Replication Batching

```
Without Batching (High Overhead):

  t=0ms   Session A updated
          ├─ Send immediately
          └─ Network round-trip: ~10ms

  t=10ms  Session B updated
          ├─ Send immediately
          └─ Network round-trip: ~10ms

  t=20ms  Session C updated
          ├─ Send immediately
          └─ Network round-trip: ~10ms

  Total: 30ms (3 round-trips)
  Network efficiency: 1 message per update (POOR)


With Batching (Better Efficiency):

  Batch Window: 20ms

  t=0ms   ┌─ Session A updated
          ├─ Session B updated
          ├─ Session C updated
  t=20ms  └─ Batch flush → [A, B, C]
             ├─ Send 1 batch
             └─ Network round-trip: ~10ms

  Total: 30ms (1 round-trip for 3 updates)
  Network efficiency: 3 messages per round-trip (GOOD)

  Batching Trade-off:
  ├─ Single update latency: 0-20ms (amortized)
  ├─ Batch throughput: 3-100 updates per 20ms
  └─ Overall: 10-100x better throughput
```

---

## 11. Session Storage Capacity

```
ETS (In-Memory):

  Available RAM: 16 GB
  Per Session: ~1 KB
  ────────────────────────
  Capacity: ~16 Million sessions

  Lookup: 1-5 µs  ← Very fast


DETS (Disk):

  Disk Space: 1 TB
  Per Session: ~1 KB
  ────────────────────────
  Capacity: ~1 Billion sessions

  Lookup: 100-500 µs  ← Slower


Mnesia (Distributed 3 Nodes):

  Total RAM: 48 GB (16 GB × 3 nodes)
  Sessions per node: 16 Million
  ────────────────────────────────
  Capacity: 16 Million (sharded)
           + 16M replicas on other nodes

  Lookup: 50-200 µs  ← Local RAM
```

---

## 12. Session Cleanup Timeline

```
Timeline of 1000 Sessions with 1-hour TTL

0h (Session created)
├─ Session 1 created at t=0
├─ Session 2 created at t=1
├─ ...
└─ Session 1000 created at t=1000

30m (Halfway to expiration)
├─ Session 1 accessed at t=30m (timer reset)
├─ Session 2: idle
├─ ...
└─ Session 1000: idle

1h (First cleanup cycle)
├─ Cleanup runs
├─ Session 1: NOT expired (accessed at 30m)
├─ Sessions 2-1000: EXPIRED (1h - 0m > 1h)
├─ Delete Sessions 2-1000
├─ Result: 999 deleted

2h
├─ Session 1: NOT expired (accessed at 30m, 30m ago < 1h)
├─ No sessions to delete

2h 30m
├─ Cleanup runs
├─ Session 1: EXPIRED (2h30m - 30m = 2h > 1h)
├─ Delete Session 1
└─ Result: 1 deleted

Result:
├─ Peak memory: 1000 sessions × 1 KB = 1 MB
├─ Cleanup efficiency: 999 at 1h, 1 at 2h30m
└─ Final: 0 sessions
```

---

## 13. Capability Negotiation Handshake

```
┌──────────────────────────────────────────────────────────────┐
│                    CLIENT                                    │
│                                                               │
│  Initialize Request:                                          │
│  {                                                            │
│    "jsonrpc": "2.0",                                          │
│    "id": 1,                                                   │
│    "method": "initialize",                                    │
│    "params": {                                                │
│      "capabilities": {                                        │
│        "roots": {"enabled": true},                            │
│        "sampling": {"enabled": true}       ← What I support   │
│      }                                                        │
│    }                                                          │
│  }                                                            │
└──────────────────────────┬───────────────────────────────────┘
                           │
                           ▼
┌──────────────────────────────────────────────────────────────┐
│                    SERVER                                    │
│                                                               │
│  Initialize Response:                                         │
│  {                                                            │
│    "jsonrpc": "2.0",                                          │
│    "id": 1,                                                   │
│    "result": {                                                │
│      "capabilities": {                                        │
│        "resources": {"enabled": true},                        │
│        "tools": {"enabled": true},                            │
│        "prompts": {"enabled": true},      ← What I support    │
│        "logging": {"enabled": true}                           │
│      }                                                        │
│    }                                                          │
│  }                                                            │
└──────────────────────────┬───────────────────────────────────┘
                           │
                           ▼
┌──────────────────────────────────────────────────────────────┐
│                    CLIENT                                    │
│                                                               │
│  Session established with:                                    │
│  {                                                            │
│    "capabilities": {                                          │
│      "resources": true,      ← Can call resources/*           │
│      "tools": true,          ← Can call tools/call            │
│      "prompts": true,        ← Can call prompts/get           │
│      "logging": true         ← Can use logging/setLevel       │
│    }                                                          │
│  }                                                            │
│                                                               │
│  Now ready to:                                                │
│  ├─ list_resources()                                          │
│  ├─ list_tools()                                              │
│  ├─ list_prompts()                                            │
│  └─ set_logging_level()                                       │
└──────────────────────────────────────────────────────────────┘
```

---

## 14. Rate Limiting Per Session

```
Session A (VIP User)
┌──────────────────────────────┐
│ rate_limit:                  │
│ ├─ requests_per_second: 100  │
│ └─ burst_size: 500           │
└──────────────────────────────┘
         │
         │ Token bucket
         │   t=0:   100 tokens
         │   t=100: 100 tokens (100 requests consumed)
         │   t=500: 400 tokens (burst available)
         │


Session B (Free Tier)
┌──────────────────────────────┐
│ rate_limit:                  │
│ ├─ requests_per_second: 10   │
│ └─ burst_size: 50            │
└──────────────────────────────┘
         │
         │ Token bucket
         │   t=0:   10 tokens
         │   t=100: 10 tokens (10 requests consumed)
         │   t=500: 40 tokens (burst partial)
         │


Rate Limit Enforcement:

Request arrives:
├─ Session A: Have 500 tokens? YES → Process
├─ Session B: Have 10 tokens? YES → Process
│             Have 50 tokens? NO  → RATE LIMITED
│                                  "Retry after 5000ms"
```

---

## Summary Table

| Concept | Stateless | Stateful | Hybrid |
|---------|-----------|----------|--------|
| **Request size** | Large | Small | Medium |
| **Horizontal scale** | ✓ Excellent | ✗ Hard | ~ Good (replicated) |
| **Auth per request** | ✓ Yes | ~ Cached | ~ JWT + Session |
| **Subscriptions** | ✗ No | ✓ Yes | ✓ Yes |
| **Correlation** | ✗ None | ✓ Yes | ✓ Yes |
| **Memory overhead** | ✗ None | ~ High | ~ Medium |
| **Failure recovery** | ✓ Simple | ~ Complex | ~ Moderate |

---

**For more details, see:**
- `MCP_SESSION_MANAGEMENT_SPECIFICATION.md` - Full specification
- `SESSION_MANAGEMENT_QUICK_REFERENCE.md` - Quick reference
- `SESSION_PERSISTENCE.md` - Persistence details
- `SESSION_REPLICATION_SYSTEM.md` - Replication & failover
