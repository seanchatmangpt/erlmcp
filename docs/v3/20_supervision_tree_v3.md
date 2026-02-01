# erlmcp v3.0.0 Supervision Tree
## Complete OTP Supervision Hierarchy

**Version**: 3.0.0
**Date**: 2026-01-31
**OTP Requirement**: 28.3.1+

---

## Overview

erlmcp v3.0.0 uses a **3-tier supervision tree** with `one_for_one` strategy throughout for maximum fault isolation. Failures are contained within application boundaries, preventing cascading restarts.

**Supervision Principles**:
- **Bulkhead Pattern**: Each subsystem isolated
- **Let-It-Crash**: Failures trigger automatic restarts
- **No Cascades**: `one_for_one` strategy prevents domino effects
- **Observability Isolation**: Monitoring failures don't affect protocol

---

## Complete Supervision Hierarchy

```
erlmcp (umbrella application)
│
├── TIER 1: CORE FOUNDATION (erlmcp_core_sup)
│   └── Strategy: one_for_one (independent restart)
│       │
│       ├── Routing & Discovery
│       │   ├── erlmcp_registry                [gproc process registry]
│       │   └── erlmcp_health                  [health check aggregator]
│       │
│       ├── Infrastructure
│       │   ├── erlmcp_reload_sup              [hot code reload supervisor]
│       │   ├── erlmcp_session_manager         [session lifecycle]
│       │   ├── erlmcp_hooks                   [Claude Code integration]
│       │   ├── erlmcp_cache                   [multi-level cache: ETS/Mnesia/External]
│       │   ├── erlmcp_cache_warmer_sup        [async cache warming supervisor]
│       │   ├── erlmcp_icon_cache              [icon caching]
│       │   ├── erlmcp_sse_event_store         [SSE event storage]
│       │   ├── erlmcp_resource_subscriptions  [MCP resource subscriptions]
│       │   ├── erlmcp_session_replicator      [session replication]
│       │   ├── erlmcp_session_failover        [failover coordinator]
│       │   └── erlmcp_failover_worker_sup     [failover workers supervisor]
│       │
│       ├── Protection (DoS, Resource Limits)
│       │   ├── erlmcp_circuit_breaker         [failure threshold detection]
│       │   ├── erlmcp_rate_limiter            [rate limiting & throttling]
│       │   ├── erlmcp_connection_limiter      [file descriptor protection]
│       │   ├── erlmcp_connection_monitor      [FD leak detection]
│       │   ├── erlmcp_memory_monitor          [memory garbage collection]
│       │   └── erlmcp_cpu_quota               [CPU-intensive DoS protection]
│       │
│       ├── MCP 2025-11-25 Features
│       │   ├── erlmcp_cancellation            [request cancellation]
│       │   ├── erlmcp_pagination              [cursor-based pagination]
│       │   ├── erlmcp_completion              [argument completion]
│       │   ├── erlmcp_elicitation             [user input elicitation]
│       │   ├── erlmcp_roots_server            [root directory management]
│       │   ├── erlmcp_apps_server             [application lifecycle]
│       │   └── erlmcp_notification_handler_sup [notification processing]
│       │
│       ├── Client Management
│       │   └── erlmcp_client_sup              [dynamic client supervisor]
│       │       └── [simple_one_for_one]
│       │           └── erlmcp_client instances
│       │
│       └── [OPTIONAL: Clustering]
│           └── erlmcp_cluster_sup            [distributed cluster management]
│               ├── erlmcp_node_monitor        [node health tracking]
│               └── erlmcp_split_brain_detector [split-brain resolution]
│
├── TIER 2: PROTOCOL SERVERS (erlmcp_server_sup)
│   └── Strategy: simple_one_for_one (dynamic instances)
│       └── erlmcp_server instances
│           ├── Server 1 (port 3000)
│           ├── Server 2 (port 3001)
│           └── Server N (custom port)
│
├── TIER 3: OBSERVABILITY (erlmcp_observability_sup)
│   └── Strategy: one_for_one (isolated from protocol)
│       │
│       ├── Metrics
│       │   ├── erlmcp_metrics                 [metrics collection]
│       │   ├── erlmcp_metrics_server          [HTTP /metrics endpoint]
│       │   └── erlmcp_metrics_aggregator      [time-series aggregation]
│       │
│       ├── Monitoring
│       │   ├── erlmcp_health_monitor          [component health tracking]
│       │   └── erlmcp_dashboard_server        [WebSocket dashboard]
│       │
│       ├── Resilience
│       │   ├── erlmcp_recovery_manager        [circuit breaker orchestration]
│       │   └── erlmcp_chaos                   [chaos engineering framework]
│       │
│       └── Distributed Tracing
│           ├── erlmcp_otel                    [OpenTelemetry integration]
│           └── erlmcp_tracer                  [span creation]
│
└── TIER 4: TRANSPORTS (erlmcp_transport_sup)
    └── Strategy: one_for_one (transport isolation)
        └── [Dynamic transport instances]
            ├── erlmcp_transport_stdio         [STDIO transport]
            ├── erlmcp_transport_tcp           [TCP transport (ranch)]
            ├── erlmcp_transport_http          [HTTP/2 transport (gun)]
            ├── erlmcp_transport_ws            [WebSocket transport]
            └── erlmcp_transport_sse           [Server-Sent Events]
```

---

## TIER 1: Core Foundation (erlmcp_core_sup)

### Supervisor Configuration

```erlang
#{strategy => one_for_one,
  intensity => 5,
  period => 60}
```

### Child Specifications

**Routing & Discovery** (2 workers):
```erlang
#{id => erlmcp_registry,
  start => {erlmcp_registry, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_registry]}

#{id => erlmcp_health,
  start => {erlmcp_health, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_health]}
```

**Infrastructure** (11 workers + 2 supervisors):
```erlang
%% Hot code reload supervisor
#{id => erlmcp_reload_sup,
  start => {erlmcp_reload_sup, start_link, []},
  restart => permanent,
  shutdown => infinity,
  type => supervisor,
  modules => [erlmcp_reload_sup]}

%% Session management
#{id => erlmcp_session_manager,
  start => {erlmcp_session_manager, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_session_manager]}

%% Claude Code integration hooks
#{id => erlmcp_hooks,
  start => {erlmcp_hooks, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_hooks]}

%% Multi-level cache (L1: ETS, L2: Mnesia, L3: External)
#{id => erlmcp_cache,
  start => {erlmcp_cache, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_cache]}

%% Async cache warming supervisor
#{id => erlmcp_cache_warmer_sup,
  start => {erlmcp_cache_warmer_sup, start_link, []},
  restart => permanent,
  shutdown => infinity,
  type => supervisor,
  modules => [erlmcp_cache_warmer_sup]}

%% Icon cache
#{id => erlmcp_icon_cache,
  start => {erlmcp_icon_cache, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_icon_cache]}

%% SSE event store
#{id => erlmcp_sse_event_store,
  start => {erlmcp_sse_event_store, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_sse_event_store]}

%% Resource subscriptions
#{id => erlmcp_resource_subscriptions,
  start => {erlmcp_resource_subscriptions, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_resource_subscriptions]}

%% Session replication
#{id => erlmcp_session_replicator,
  start => {erlmcp_session_replicator, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_session_replicator]}

%% Session failover
#{id => erlmcp_session_failover,
  start => {erlmcp_session_failover, start_link, [node()]},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_session_failover]}

%% Failover workers supervisor
#{id => erlmcp_failover_worker_sup,
  start => {erlmcp_failover_worker_sup, start_link, []},
  restart => permanent,
  shutdown => infinity,
  type => supervisor,
  modules => [erlmcp_failover_worker_sup]}
```

**Protection** (6 workers):
```erlang
%% Circuit breaker (DoS protection)
#{id => erlmcp_circuit_breaker,
  start => {erlmcp_circuit_breaker, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_circuit_breaker]}

%% Rate limiter
#{id => erlmcp_rate_limiter,
  start => {erlmcp_rate_limiter, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_rate_limiter]}

%% Connection limiter (FD protection)
#{id => erlmcp_connection_limiter,
  start => {erlmcp_connection_limiter, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_connection_limiter]}

%% Connection monitor (leak detection)
#{id => erlmcp_connection_monitor,
  start => {erlmcp_connection_monitor, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_connection_monitor]}

%% Memory monitor
#{id => erlmcp_memory_monitor,
  start => {erlmcp_memory_monitor, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_memory_monitor]}

%% CPU quota (CPU-intensive DoS protection)
#{id => erlmcp_cpu_quota,
  start => {erlmcp_cpu_quota, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_cpu_quota]}
```

**MCP 2025-11-25 Features** (7 workers + 1 supervisor):
```erlang
%% Request cancellation
#{id => erlmcp_cancellation,
  start => {erlmcp_cancellation, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_cancellation]}

%% Cursor-based pagination
#{id => erlmcp_pagination,
  start => {erlmcp_pagination, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_pagination]}

%% Argument completion
#{id => erlmcp_completion,
  start => {erlmcp_completion, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_completion]}

%% User input elicitation
#{id => erlmcp_elicitation,
  start => {erlmcp_elicitation, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_elicitation]}

%% Root directory management
#{id => erlmcp_roots_server,
  start => {erlmcp_roots_server, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_roots_server]}

%% Application lifecycle
#{id => erlmcp_apps_server,
  start => {erlmcp_apps_server, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_apps_server]}

%% Notification handler supervisor
#{id => erlmcp_notification_handler_sup,
  start => {erlmcp_notification_handler_sup, start_link, []},
  restart => permanent,
  shutdown => infinity,
  type => supervisor,
  modules => [erlmcp_notification_handler_sup]}
```

**Client Management** (1 supervisor):
```erlang
%% Client supervisor (dynamic clients)
#{id => erlmcp_client_sup,
  start => {erlmcp_client_sup, start_link, []},
  restart => permanent,
  shutdown => infinity,
  type => supervisor,
  modules => [erlmcp_client_sup]}
```

**[OPTIONAL] Clustering** (1 supervisor + 2 workers):
```erlang
%% Cluster supervisor (if cluster_enabled = true)
#{id => erlmcp_cluster_sup,
  start => {erlmcp_cluster_sup, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => supervisor,
  modules => [erlmcp_cluster_sup]}

%% Node monitor (child of erlmcp_cluster_sup)
#{id => erlmcp_node_monitor,
  start => {erlmcp_node_monitor, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_node_monitor]}

%% Split-brain detector (child of erlmcp_cluster_sup)
#{id => erlmcp_split_brain_detector,
  start => {erlmcp_split_brain_detector, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_split_brain_detector]}
```

---

## TIER 2: Protocol Servers (erlmcp_server_sup)

### Supervisor Configuration

```erlang
#{strategy => simple_one_for_one,
  intensity => 10,
  period => 60}
```

### Dynamic Server Instances

Each MCP server is a separate gen_server process:

```erlang
%% Server instances started dynamically
erlmcp_sup:start_server(my_server, #{
    transport => tcp,
    transport_config => #{port => 3000},
    capabilities => #{
        resources => true,
        tools => true,
        prompts => true
    }
}).
```

**Server Process Structure**:
```
erlmcp_server_sup (simple_one_for_one)
├── erlmcp_server (my_server_1)
│   ├── Transport: TCP port 3000
│   ├── Resources: [resource1, resource2]
│   ├── Tools: [tool1, tool2]
│   └── Prompts: [prompt1, prompt2]
│
├── erlmcp_server (my_server_2)
│   ├── Transport: HTTP port 3001
│   ├── Resources: [resource3]
│   ├── Tools: [tool3]
│   └── Prompts: []
│
└── erlmcp_server (my_server_N)
    └── ... custom configuration
```

---

## TIER 3: Observability (erlmcp_observability_sup)

### Supervisor Configuration

```erlang
#{strategy => one_for_one,
  intensity => 3,
  period => 60}
```

### Child Specifications

**Metrics** (3 workers):
```erlang
%% Metrics collection
#{id => erlmcp_metrics,
  start => {erlmcp_metrics, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_metrics]}

%% HTTP metrics endpoint
#{id => erlmcp_metrics_server,
  start => {erlmcp_metrics_server, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_metrics_server]}

%% Time-series aggregation
#{id => erlmcp_metrics_aggregator,
  start => {erlmcp_metrics_aggregator, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_metrics_aggregator]}
```

**Monitoring** (2 workers):
```erlang
%% Health monitor
#{id => erlmcp_health_monitor,
  start => {erlmcp_health_monitor, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_health_monitor]}

%% Dashboard server
#{id => erlmcp_dashboard_server,
  start => {erlmcp_dashboard_server, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_dashboard_server]}
```

**Resilience** (2 workers):
```erlang
%% Recovery manager
#{id => erlmcp_recovery_manager,
  start => {erlmcp_recovery_manager, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_recovery_manager]}

%% Chaos engineering
#{id => erlmcp_chaos,
  start => {erlmcp_chaos, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_chaos]}
```

**Distributed Tracing** (2 workers):
```erlang
%% OpenTelemetry integration
#{id => erlmcp_otel,
  start => {erlmcp_otel, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_otel]}

%% Span creation
#{id => erlmcp_tracer,
  start => {erlmcp_tracer, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_tracer]}
```

---

## TIER 4: Transports (erlmcp_transport_sup)

### Supervisor Configuration

```erlang
#{strategy => one_for_one,
  intensity => 5,
  period => 60}
```

### Dynamic Transport Instances

Each transport is a separate process:

```erlang
%% Transport instances started dynamically
erlmcp_sup:start_transport(my_tcp, tcp, #{
    port => 3000,
    num_acceptors => 10
}).
```

**Transport Process Structure**:
```
erlmcp_transport_sup (one_for_one)
├── erlmcp_transport_stdio (my_stdio)
│   ├── Type: stdio
│   ├── Buffer: 64KB
│   └── Timeout: infinity
│
├── erlmcp_transport_tcp (my_tcp)
│   ├── Type: tcp
│   ├── Port: 3000
│   ├── Acceptors: 10
│   └── Ranch listener
│
├── erlmcp_transport_http (my_http)
│   ├── Type: http
│   ├── Port: 3001
│   ├── Protocol: HTTP/2
│   └── Gun client
│
├── erlmcp_transport_ws (my_ws)
│   ├── Type: websocket
│   ├── Port: 3002
│   └── Gun WebSocket
│
└── erlmcp_transport_sse (my_sse)
    ├── Type: sse
    ├── Port: 3003
    └── Cowboy SSE
```

---

## Failure Isolation Matrix

| Component | Strategy | Failure Scope | Recovery Time | Impact |
|-----------|----------|---------------|---------------|--------|
| **Registry** | one_for_one | Registry only | ~500ms | New routing fails; existing connections continue |
| **Session Manager** | one_for_one | Session Manager only | ~1s | New sessions fail; existing sessions continue |
| **Cache** | one_for_one | Cache only | ~500ms | Cache miss; requests still served |
| **Circuit Breaker** | one_for_one | Circuit Breaker only | ~500ms | DoS protection temporarily disabled |
| **Rate Limiter** | one_for_one | Rate Limiter only | ~500ms | Rate limiting temporarily disabled |
| **Connection Limiter** | one_for_one | Connection Limiter only | ~500ms | FD protection temporarily disabled |
| **Memory Monitor** | one_for_one | Memory Monitor only | ~500ms | GC hints stop; system continues |
| **CPU Quota** | one_for_one | CPU Quota only | ~500ms | CPU protection temporarily disabled |
| **Completion** | one_for_one | Completion only | ~500ms | Completion feature unavailable |
| **Elicitation** | one_for_one | Elicitation only | ~500ms | Elicitation feature unavailable |
| **Roots Server** | one_for_one | Roots Server only | ~1s | Root management unavailable |
| **Apps Server** | one_for_one | Apps Server only | ~1s | App management unavailable |
| **Server Instance** | simple_one_for_one | That server only | ~2s | In-flight requests lost; clients reconnect |
| **Metrics** | one_for_one | Metrics only | ~500ms | Metrics unavailable; protocol unaffected |
| **Health Monitor** | one_for_one | Health Monitor only | ~500ms | Health checks unavailable; protocol unaffected |
| **Dashboard** | one_for_one | Dashboard only | ~500ms | Dashboard unavailable; protocol unaffected |
| **OTEL** | one_for_one | OTEL only | ~500ms | Tracing unavailable; protocol unaffected |
| **Transport** | one_for_one | That transport only | ~2s | Connections on that transport drop; others unaffected |

**Key Guarantees**:
- ✅ No cascading failures (one_for_one strategy)
- ✅ Observability failures never affect protocol
- ✅ Transport failures are isolated
- ✅ Server failures are isolated (simple_one_for_one)
- ✅ Recovery time < 2s for all components

---

## Startup Sequence

### Application Startup

```
1. erlmcp_app:start/2
   ↓
2. Start erlmcp_sup (TIER 1-3 coordinator)
   ↓
3. Start erlmcp_core_sup (TIER 1: 27 children)
   ↓ 3.1. Routing: erlmcp_registry, erlmcp_health
   ↓ 3.2. Infrastructure: 11 workers + 2 supervisors
   ↓ 3.3. Protection: 6 workers
   ↓ 3.4. MCP Features: 7 workers + 1 supervisor
   ↓ 3.5. Client: erlmcp_client_sup
   ↓ 3.6. [OPTIONAL] Cluster: erlmcp_cluster_sup
   ↓
4. Start erlmcp_server_sup (TIER 2: ready for dynamic servers)
   ↓
5. Start erlmcp_observability_sup (TIER 3: 9 children)
   ↓ 5.1. Metrics: 3 workers
   ↓ 5.2. Monitoring: 2 workers
   ↓ 5.3. Resilience: 2 workers
   ↓ 5.4. Tracing: 2 workers
   ↓
6. Start erlmcp_transports_app
   ↓ Start erlmcp_transport_sup (TIER 4: ready for dynamic transports)
   ↓
7. Start erlmcp_validation_app
   ↓ (No supervisor - stateless validators)
   ↓
8. [OPTIONAL] Start tcps_erlmcp_app
   ↓ Start tcps_erlmcp_sup (quality system)
   ↓
9. System Ready
   ✅ Registry operational
   ✅ Transports can be started
   ✅ Servers/clients can be spawned
   ✅ Metrics collection active
   ✅ Health checks available
```

### Graceful Shutdown

```
1. Application stop signal
   ↓
2. Reverse shutdown order
   ↓ 2.1. Stop tcps_erlmcp_app (if enabled)
   ↓      Flush receipt chain
   ↓      Close dashboard
   ↓ 2.2. Stop erlmcp_validation_app
   ↓ 2.3. Stop erlmcp_transports_app
   ↓      Close all transports
   ↓      Ranch listeners stop
   ↓ 2.4. Stop erlmcp_observability_sup
   ↓      Flush pending metrics
   ↓      Close OTEL exporters
   ↓ 2.5. Stop erlmcp_core_sup
   ↓      Stop all servers
   ↓      Stop all clients
   ↓      Flush sessions to Mnesia
   ↓      Close cache (ETS/Mnesia)
   ↓
3. VM shutdown
```

---

## Supervision Tree Validation

### Validation Checklist

- [ ] All supervisors use `one_for_one` or `simple_one_for_one`
- [ ] No `one_for_all` or `rest_for_one` (except top-level)
- [ ] All critical workers have `restart => permanent`
- [ ] All supervisors have `shutdown => infinity`
- [ ] All workers have `shutdown => 5000` (or appropriate)
- [ ] No circular dependencies between children
- [ ] Observability failures don't affect protocol
- [ ] Transport failures are isolated
- [ ] Server failures are isolated
- [ ] Client failures are isolated

### Testing

Run supervision tree validation:
```bash
# Run supervision tests
rebar3 ct --suite=erlmcp_supervision_tests

# Check tree structure
erlmcp_sup:which_children().
erlmcp_core_sup:which_children().
erlmcp_observability_sup:which_children().
erlmcp_transport_sup:which_children().
```

---

**Document Status**: ✅ Complete
**Related Documents**:
- `10_architecture_design_plan.md` - Overall v3.0.0 architecture
- `30_component_dependency_matrix.md` - Dependency analysis
