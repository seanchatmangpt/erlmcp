# Session Management Quick Reference

**Quick links to common tasks**

---

## Key Files

| File | Purpose |
|------|---------|
| `/home/user/erlmcp/docs/MCP_SESSION_MANAGEMENT_SPECIFICATION.md` | **Full specification** (1770 lines) |
| `/home/user/erlmcp/docs/SESSION_PERSISTENCE.md` | Session persistence backends |
| `/home/user/erlmcp/docs/SESSION_REPLICATION_SYSTEM.md` | Distributed replication & failover |
| `apps/erlmcp_core/src/erlmcp_session.erl` | Public API |
| `apps/erlmcp_core/src/erlmcp_session_manager.erl` | Session lifecycle manager |
| `apps/erlmcp_core/src/erlmcp_session_backend.erl` | Backend behavior interface |

---

## Quick Start

### Create Session
```erlang
{ok, SessionId} = erlmcp_session:create(#{user_id => <<"user_123">>}, 1800000).
```

### Retrieve Session
```erlang
{ok, SessionData} = erlmcp_session:retrieve(SessionId).
```

### Update Session
```erlang
ok = erlmcp_session:update(SessionId, fun(Session) ->
    Metadata = maps:get(metadata, Session),
    Session#{metadata => Metadata#{counter => 1}}
end).
```

### Delete Session
```erlang
ok = erlmcp_session:delete(SessionId).
```

### List All Sessions
```erlang
Sessions = erlmcp_session:list_sessions().
```

### Set TTL
```erlang
ok = erlmcp_session:set_ttl(SessionId, 3600000).  % 1 hour
```

---

## Initialization Handshake

```erlang
%% Client side
{ok, ClientPid} = erlmcp_client:start_link({stdio, []}),
ClientCaps = #mcp_client_capabilities{
    roots = #mcp_capability{enabled = true},
    sampling = #mcp_capability{enabled = true}
},
{ok, ServerCaps} = erlmcp_client:initialize(ClientPid, ClientCaps),
%% Now ready to use protocol methods
ok = erlmcp_client:list_tools(ClientPid).
```

---

## Request Correlation

| Concept | How it Works |
|---------|--------------|
| **Request ID** | Unique number in each request (1, 2, 3, ...) |
| **Response** | Echoes same request ID |
| **Pending Requests** | Stored in `#{request_id => {method, from_pid}}` |
| **Timeout** | If no response after 5s, clean up pending |

---

## Session Backends

### ETS (Default)
```erlang
{erlmcp_session, [{backend, erlmcp_session_ets}]}.
```
✅ Fastest (1-5 µs)
❌ Lost on restart

### DETS (Persistent)
```erlang
{erlmcp_session, [
    {backend, erlmcp_session_dets},
    {backend_opts, #{file_path => "data/sessions.dets"}}
]}.
```
✅ Survives restart (100-500 µs)
❌ Slower than ETS

### Mnesia (Distributed)
```erlang
{erlmcp_session, [
    {backend, erlmcp_session_mnesia},
    {backend_opts, #{
        nodes => ['node1@host', 'node2@host', 'node3@host'],
        disc_copies => true
    }}
]}.
```
✅ Clustered (50-200 µs)
✅ Auto-failover

---

## Expiration & Cleanup

| Concept | Setting |
|---------|---------|
| **TTL** | `timeout_ms` in session (inactivity-based) |
| **Cleanup Interval** | Default 60s (configurable) |
| **Never Expire** | Set `timeout_ms => infinity` |

```erlang
%% Automatic cleanup every 60 seconds
%% Removes sessions inactive for > timeout_ms
{ok, Count} = erlmcp_session:cleanup_expired().
```

---

## Stateless vs Stateful

### Stateless
- Context in request (JWT token, auth headers)
- No server-side state
- Scales horizontally
- Larger payload

### Stateful
- Server maintains session store
- Requires session lookup per request
- Less scalable (unless replicated)
- Smaller payload

### Hybrid (Recommended)
- Connection state on server (capabilities, subscriptions)
- Auth in request (JWT token)
- Best of both worlds

---

## Multi-Session Isolation

Each session has **independent**:
- ✅ TTL and cleanup
- ✅ Subscriptions
- ✅ Pending requests
- ✅ Metadata

Shared across sessions:
- ✅ Tool implementations
- ✅ Database connections
- ✅ Resource handlers

---

## Failover & Recovery

### Single Node
- Sessions in ETS or DETS
- No failover
- Manual restart recovery

### Clustered (Mnesia)
- Sessions replicated to backup nodes
- Node failure detected in ~5s
- Automatic failover (majority voting)
- Majority vote prevents split-brain

---

## Metrics to Monitor

```erlang
SessionCount = length(erlmcp_session:list_sessions()),
ExpiredCount = erlmcp_session:cleanup_expired(),
AvgSessionAge = calculate_avg_age(),
Memory = erlang:memory(heap_bin).
```

Key metrics:
- Total session count
- Expired sessions per cleanup
- Average session age
- Memory per session (~1 KB)
- Replication lag (if clustered)

---

## Common Patterns

### Pattern 1: Session with Metadata
```erlang
{ok, SessionId} = erlmcp_session:create(#{
    user_id => <<"user_123">>,
    client_ip => <<"192.168.1.1">>,
    request_count => 0,
    last_tool => undefined
}, 3600000).
```

### Pattern 2: Touch Session on Access
```erlang
{ok, SessionData} = erlmcp_session:retrieve(SessionId),
erlmcp_session:update(SessionId, fun(S) -> S end),  % Touch last_accessed
%% Use SessionData...
```

### Pattern 3: Rate Limiting Per Session
```erlang
RateLimit = maps:get(rate_limit, maps:get(metadata, SessionData), #{}),
case check_rate_limit(SessionId, RateLimit) of
    ok -> process_request();
    {error, rate_limited} -> {error, retry_after_ms}
end.
```

### Pattern 4: Batch Operations
```erlang
erlmcp_client:with_batch(Client, fun(_Batch) ->
    ToolsResp = erlmcp_client:list_tools(Client),
    ResourcesResp = erlmcp_client:list_resources(Client),
    {ok, [ToolsResp, ResourcesResp]}
end).
```

---

## Configuration Presets

### Development
```erlang
{erlmcp_core, [
    {session_backend, ets},
    {session_timeout_ms, 300000},      % 5 min
    {cleanup_interval_ms, 60000}       % 1 min
]}.
```

### Single-Node Production
```erlang
{erlmcp_core, [
    {session_backend, dets},
    {session_backend_opts, #{
        file_path => "/var/lib/erlmcp/sessions.dets",
        auto_save => 120000
    }},
    {session_timeout_ms, 3600000},     % 1 hour
    {cleanup_interval_ms, 120000}      % 2 min
]}.
```

### Multi-Node Cluster
```erlang
{erlmcp_core, [
    {session_backend, mnesia},
    {session_backend_opts, #{
        nodes => ['node1@host', 'node2@host', 'node3@host'],
        disc_copies => true
    }},
    {session_timeout_ms, 3600000},
    {cleanup_interval_ms, 120000},
    {cluster_nodes, ['node1@host', 'node2@host', 'node3@host']}
]}.
```

---

## Decision Tree

```
Need persistent sessions?
├─ No (dev/testing)
│  └─ Use ETS ✓
│
├─ Yes, single node?
│  └─ Use DETS ✓
│
└─ Yes, multi-node cluster?
   ├─ Need auto-failover?
   │  └─ Use Mnesia with disc_copies ✓
   │
   └─ Need external storage?
      └─ Implement custom backend (Redis, PostgreSQL) ✓
```

---

## Troubleshooting

### Sessions Lost on Restart
**Problem**: Sessions disappear after restart
**Cause**: Using ETS backend
**Solution**: Switch to DETS or Mnesia

### DETS File Corrupted
**Problem**: `{error, {corrupt, ...}}`
**Solution**:
```erlang
dets:open_file(erlmcp_sessions_dets, [
    {file, "data/erlmcp_sessions.dets"},
    {repair, true}
]).
```

### Mnesia Table Not Loading
**Problem**: `{aborted, {no_exists, ...}}`
**Solution**:
```erlang
mnesia:wait_for_tables([erlmcp_session], 5000).
```

### High Memory Usage
**Problem**: Sessions consuming too much memory
**Causes**:
- TTL too long
- Cleanup interval too long
- Large metadata in sessions

**Solutions**:
- Reduce TTL
- Reduce cleanup interval
- Reduce metadata size

---

## Performance Targets

| Operation | ETS | DETS | Mnesia | Target |
|-----------|-----|------|--------|--------|
| Create | 1-5 µs | 1-5 ms | 1-10 ms | <10 ms |
| Retrieve | 1-5 µs | 100-500 µs | 50-200 µs | <100 µs |
| Update | 1-5 µs | 1-5 ms | 1-10 ms | <10 ms |
| Delete | 1-5 µs | 1-5 ms | 1-10 ms | <10 ms |
| List 10K | 1-5 ms | 100-500 ms | 50-200 ms | <100 ms |

For 100K concurrent sessions: Use Mnesia cluster (sharded).

---

## See Also

- **Full Specification**: `docs/MCP_SESSION_MANAGEMENT_SPECIFICATION.md`
- **Session Persistence**: `docs/SESSION_PERSISTENCE.md`
- **Replication**: `docs/SESSION_REPLICATION_SYSTEM.md`
- **API Reference**: `apps/erlmcp_core/src/erlmcp_session.erl`
- **Examples**: `examples/session_*.erl`
