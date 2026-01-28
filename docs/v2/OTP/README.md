# Erlang/OTP Architecture Documentation

**Version:** v1.3.0 (Bulkhead Design)
**Last Updated:** 2026-01-27

---

## Quick Links

### Start Here
1. **[Supervision Trees](supervision-trees.md)** - Complete reference for erlmcp's supervision architecture
   - 5-tier bulkhead design with visual diagrams
   - Failure scenarios and recovery procedures
   - Capacity planning (max 15,000 concurrent connections)

2. **[Extraction Report](SUPERVISION_EXTRACTION_REPORT.md)** - Summary of what was extracted and how
   - Ground truth data validation
   - JSON schema overview
   - Improvement recommendations

### Reference Data
3. **[supervision.json](../../tools/v2_arch/supervision.json)** - Machine-readable supervision tree
   - All 9 supervisors with complete metadata
   - Child specifications and strategies
   - Tier hierarchy and dependencies

### Automation
4. **[extract_supervision.escript](../../tools/v2_arch/extract_supervision.escript)** - Extraction tool
   - Scans src/*_sup.erl files
   - Generates updated supervision.json
   - Integrates into CI pipeline

---

## Architecture Overview

### 5-Tier Supervision Hierarchy

```
Root (erlmcp_sup)
│
├─ TIER 1: Registry (one_for_one)
│  ├─ erlmcp_registry - gproc message router
│  └─ erlmcp_registry_health_check - health probes
│
├─ TIER 2: Infrastructure (one_for_one)
│  ├─ erlmcp_hot_reload - zero-downtime upgrades
│  ├─ erlmcp_graceful_drain - connection draining
│  ├─ erlmcp_session_manager - HTTP sessions
│  ├─ erlmcp_task_manager - async task queue
│  ├─ erlmcp_resource_subscriptions - resource changes
│  ├─ erlmcp_sse_event_store - event resumability
│  ├─ erlmcp_icon_cache - icon metadata
│  ├─ erlmcp_session_replicator - distributed state
│  └─ erlmcp_session_failover - session migration
│
├─ TIER 3: Servers (simple_one_for_one)
│  └─ [unlimited dynamic server instances]
│
├─ TIER 3.5: Connection Pools (rest_for_one - optional)
│  └─ pool_0..pool_9 (10 pools × ~1,500 connections each)
│
├─ TIER 4: Transports (one_for_one)
│  └─ [dynamically added: stdio, tcp, http]
│
└─ TIER 5: Monitoring (one_for_one, independent)
   ├─ erlmcp_health_monitor - system health
   ├─ erlmcp_recovery_manager - failure recovery
   ├─ erlmcp_metrics_server - metrics aggregation
   └─ erlmcp_metrics_http_sup - metrics dashboard (port 8088)
```

### Key Design Principles

1. **Bulkhead Pattern:** Each tier is an independent failure domain
2. **Ordered Dependencies:** `rest_for_one` at root ensures TIER 1 starts before TIER 2+
3. **Graceful Degradation:** TIER 5 (monitoring) can fail without affecting core
4. **Dynamic Worker Pools:** `simple_one_for_one` for unlimited server instances
5. **Observable Failure:** Health monitor and recovery manager track all components

---

## Supervision Strategies

### rest_for_one
- **Used:** Root supervisor + connection pool supervisor
- **Behavior:** If child N fails, restart child N and all children started after it
- **Why:** Ensures dependencies start in order (TIER 1 before TIER 2, etc.)

### one_for_one
- **Used:** TIER 1-2 infrastructure, TIER 4 transports, TIER 5 monitoring
- **Behavior:** If one child fails, restart only that child
- **Why:** Independent components with no coupling

### simple_one_for_one
- **Used:** Server instances + pool members
- **Behavior:** Optimized one_for_one for unlimited dynamic children
- **Why:** Supports process pools without pre-allocation

---

## Failure Scenarios & Recovery

| Tier | Component | Failure | Recovery Time | User Impact |
|------|-----------|---------|---------------|-------------|
| 1 | Registry | Can't route messages | <1s | Requests fail until recovery |
| 2 | Session manager | New sessions fail | <1s | Session creation blocked |
| 3 | One server | Client disconnected | Immediate | One client must reconnect |
| 4 | TCP transport | Connections lost | <1s | Automatic reconnect |
| 5 | Monitoring | No visibility | <1s | None (core unaffected) |

**Key:** TIER 5 failures are isolated. Monitoring crash does NOT stop protocol layer.

---

## Capacity Planning

### Concurrent Connections

- **With connection pools:** 10 pools × 1,500 connections = **15,000 max**
- **Without pools:** Unlimited (limited by memory + OS file descriptors)
- **Registry throughput:** 553K messages/sec

### Scaling Beyond 15,000

1. Use clustering + gproc distribution
2. Each node handles 15K connections independently
3. Messages route across cluster via gproc keys

### Monitoring Dashboard

- **URL:** `http://localhost:8088/metrics`
- **Updated:** Real-time via metrics_server
- **Data:** Throughput, latency, memory per connection

---

## Common Tasks

### Add a New Supervisor

1. Create `src/erlmcp_new_sup.erl`:
   ```erlang
   -module(erlmcp_new_sup).
   -behaviour(supervisor).

   -export([start_link/0, init/1]).

   start_link() ->
       supervisor:start_link({local, ?MODULE}, ?MODULE, []).

   init([]) ->
       SupFlags = #{
           strategy => one_for_one,
           intensity => 5,
           period => 60
       },
       ChildSpecs = [
           #{
               id => my_worker,
               start => {my_worker, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [my_worker]
           }
       ],
       {ok, {SupFlags, ChildSpecs}}.
   ```

2. Add to appropriate tier in root supervisor:
   ```erlang
   %% In erlmcp_sup init/1
   #{
       id => erlmcp_new_sup,
       start => {erlmcp_new_sup, start_link, []},
       restart => permanent,
       shutdown => infinity,
       type => supervisor,
       modules => [erlmcp_new_sup]
   }
   ```

3. Update documentation in `supervision-trees.md`

4. Run extraction tool:
   ```bash
   ./tools/v2_arch/extract_supervision.escript
   ```

### Start a Server Instance

```erlang
{ok, Pid} = erlmcp_sup:start_server(my_server, #{
    config => value,
    recovery_policy => exponential_backoff
}).
```

### Register with Health Monitor

```erlang
ok = erlmcp_health_monitor:register_component(my_server, Pid).
ok = erlmcp_recovery_manager:register_component(my_server, Pid, RecoveryPolicy).
```

### Monitor System Health

```erlang
% In Erlang shell
erlmcp_health_monitor:status().
% Returns: {ok, {AliveComponents, FailedComponents}}
```

---

## Code References

### Source Files

| File | Lines | Purpose |
|------|-------|---------|
| src/erlmcp_sup.erl | 212 | Root supervisor |
| src/erlmcp_registry_sup.erl | 51 | TIER 1 registry |
| src/erlmcp_infrastructure_sup.erl | 122 | TIER 2 infrastructure |
| src/erlmcp_server_sup.erl | 47 | TIER 3 servers |
| src/erlmcp_transport_sup.erl | 121 | TIER 4 transports |
| src/erlmcp_monitoring_sup.erl | 72 | TIER 5 monitoring |
| src/erlmcp_connection_pool_sup.erl | 67 | Connection pools |
| src/erlmcp_server_pool_sup.erl | 37 | Pool members |
| src/erlmcp_metrics_http_sup.erl | 48 | Metrics HTTP server |

### Key APIs

**Supervisor Management:**
- `erlmcp_sup:start_server(ServerId, Config)` - Start MCP server instance
- `erlmcp_sup:stop_server(ServerId)` - Stop server instance
- `erlmcp_sup:start_transport(TransportId, Type, Config)` - Start transport
- `erlmcp_sup:stop_transport(TransportId)` - Stop transport

**Health & Recovery:**
- `erlmcp_health_monitor:register_component(Id, Pid)` - Register for health checks
- `erlmcp_health_monitor:unregister_component(Id)` - Unregister
- `erlmcp_health_monitor:status()` - Check system health
- `erlmcp_recovery_manager:register_component(Id, Pid, Policy)` - Set recovery policy

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.3.0 | 2026-01-27 | Bulkhead design, 5-tier architecture, connection pools |
| 1.2.0 | 2026-01-20 | Added infrastructure tier (sessions, tasks, resources) |
| 1.1.0 | 2026-01-10 | Initial monitoring supervisor |
| 1.0.0 | 2025-12-01 | First production supervision tree |

---

## Related Documentation

- **[API Reference](../api-reference.md)** - Complete function documentation
- **[Protocol Guide](../protocol.md)** - MCP protocol implementation
- **[Architecture Guide](../architecture.md)** - System design patterns
- **[OTP Patterns](../otp-patterns.md)** - Erlang/OTP best practices

---

## Questions?

- Check the **[Supervision Trees](supervision-trees.md)** guide for detailed explanations
- Review **failure scenarios** section for your specific use case
- Run `erlmcp_health_monitor:status()` to check system health
- Contact the erlmcp team for architectural questions

---

**Last extraction:** 2026-01-27 via `/tools/v2_arch/extract_supervision.escript`

