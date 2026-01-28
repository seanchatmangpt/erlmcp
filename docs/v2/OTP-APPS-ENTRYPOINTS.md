# OTP Applications & Entrypoints - erlmcp v0.7.0

**Derived from:** `tools/v2_arch/extract_entrypoints.escript`
**Timestamp:** 2026-01-28
**Status:** v0.7.0 runtime structure analysis

---

## Overview

This document describes what actually *runs* at erlmcp runtime. It maps OTP applications to their entry points (start functions and supervisors) and shows the dependency tree.

### Key Files Referenced
- `src/erlmcp.app.src` - Application resource file
- `src/erlmcp_app.erl` - Application callback (entry point)
- `src/erlmcp_sup.erl` - Top-level supervisor
- `tools/v2_arch/entrypoints.json` - Machine-readable metadata

---

## Application Entrypoints

### erlmcp (Main Application)

**Registered Name:** `erlmcp_sup`, `erlmcp_client_sup`, `erlmcp_server_sup`
**Version:** 0.7.0
**Erlang:** OTP 25+

#### Entry Module
```erlang
module:       erlmcp_app
arity:        2
signature:    start(StartType :: application:start_type(), StartArgs :: term())
              -> {ok, pid()} | {error, term()}
file:         src/erlmcp_app.erl:6
```

#### What Happens on Startup
```erlang
%% src/erlmcp_app.erl:start/2
start(_StartType, _StartArgs) ->
    erlmcp_logging:init_session_levels(),
    erlmcp_sup:start_link().
```

**Steps:**
1. **Initialize logging** - Session-level logging configuration (Gap #21: ETS table)
2. **Start supervision tree** - Call `erlmcp_sup:start_link()`

#### Shutdown
```erlang
%% src/erlmcp_app.erl:stop/1
stop(_State) -> ok.
```

No active shutdown cleanup - relies on supervisor cascade.

#### Entry Supervisor
```erlang
supervisor:   erlmcp_sup
strategy:     rest_for_one (if dependency fails, restart dependents only)
restart:      permanent
intensity:    5 restarts per 60 seconds (backoff protection)
file:         src/erlmcp_sup.erl:111
```

---

## Supervision Tree (5-Tier Bulkhead Architecture)

The erlmcp supervision tree uses a **bulkhead pattern** with isolated subsystems at each tier. Failure in one tier restarts only that tier and its dependents.

```
erlmcp_sup (rest_for_one)
│
├─ TIER 1: erlmcp_registry_sup (one_for_one)
│  ├── erlmcp_registry (worker) - Central message routing via gproc
│  └── erlmcp_registry_health_check (worker) - Registry liveness checks
│
├─ TIER 2: erlmcp_infrastructure_sup (one_for_one)
│  ├── erlmcp_hot_reload (worker) - Zero-downtime upgrades
│  ├── erlmcp_graceful_drain (worker) - Connection draining
│  ├── erlmcp_session_manager (worker) - HTTP session tracking
│  ├── erlmcp_task_queue_manager (worker) - Async task coordination
│  └── erlmcp_subscription_manager (worker) - Resource subscriptions
│
├─ TIER 3: erlmcp_server_sup (simple_one_for_one)
│  └── erlmcp_server_new [TEMPLATE] (worker, temporary)
│      - Spawns MCP server instances dynamically via start_child/2
│      - Each server instance: {ServerId, Config} pattern
│
├─ TIER 4: erlmcp_transport_sup (dynamic)
│  └── Various transport implementations (worker, temporary)
│      - stdio, TCP (via ranch), HTTP (via gun/cowboy), WebSocket
│      - Each spawned via start_child(TransportId, Type, Config)
│
└─ TIER 5: erlmcp_monitoring_sup (one_for_one) [OPTIONAL - ISOLATED]
   ├── erlmcp_health_monitor (worker) - System health checks
   ├── erlmcp_recovery_manager (worker) - Failure recovery coordination
   ├── erlmcp_metrics_server (worker) - Metrics collection/aggregation
   ├── erlmcp_metrics_http_sup (supervisor) - HTTP metrics endpoints
   │   └── Cowboy HTTP listeners for Prometheus/OTEL
   └── erlmcp_config_sup (supervisor) - Configuration management
```

### Tier Dependencies & Failure Scenarios

| Tier | Component | Dependencies | Strategy | Failure Impact |
|------|-----------|--------------|----------|---|
| 1 | Registry | None | one_for_one | New messages fail to route; automatic recovery via gproc |
| 2 | Infrastructure | Registry | one_for_one | New sessions/tasks fail; existing connections continue |
| 3 | Servers | Registry + Infrastructure | simple_one_for_one | In-flight requests lost; clients reconnect automatically |
| 4 | Transports | Registry + Servers | (dynamic) | Network connections lost; automatic client retry |
| 5 | Monitoring | (independent) | one_for_one | Lost observability; **NO impact on protocol layer** |

### Bulkhead Isolation

**Why this design:**
- **Registry** (Tier 1) is the foundation - no other component starts until it's healthy
- **Infrastructure** (Tier 2) provides session/task/subscription management
- **Servers** (Tier 3) depend on infrastructure to create new instances
- **Transports** (Tier 4) are I/O handlers that can restart without affecting protocol logic
- **Monitoring** (Tier 5) is completely isolated - a failure here doesn't affect the protocol layer at all

**Effect:** If monitoring subsystem crashes, erlmcp keeps processing requests. If registry crashes, everything waits for recovery. This prevents cascading failures.

---

## Dependencies (18 Total)

erlmcp depends on these OTP applications (from `erlmcp.app.src:applications`):

### Kernel & Language
- `kernel` - Core Erlang runtime
- `stdlib` - Standard library
- `crypto` - Cryptographic functions (SSL/TLS)
- `public_key` - Certificate/key handling
- `ssl` - SSL/TLS protocol
- `inets` - HTTP client (via `gun`)

### Data & Serialization
- `jsx` - JSON encoding/decoding
- `jesse` - JSON Schema validation
- `bbmustache` - Mustache template processing

### Service Infrastructure
- `gproc` (0.9.0) - Global process registry (message routing)
- `gun` (2.0.1) - HTTP/HTTPS client
- `ranch` (2.1.0) - TCP acceptor pool
- `poolboy` (1.5.2) - Generic pooling
- `cowboy` - HTTP/REST server framework
- `jobs` - Job queue management
- `fs` - Filesystem monitoring

### Observability
- `opentelemetry_api` - OTEL API layer
- `opentelemetry` - OTEL instrumentation

---

## Dynamic Process Creation

erlmcp spawns processes dynamically via two main supervisors:

### MCP Servers
```erlang
erlmcp_sup:start_server(ServerId, Config) -> {ok, ServerPid}
```
- **Supervisor:** `erlmcp_server_sup` (simple_one_for_one)
- **Child module:** `erlmcp_server_new`
- **Template args:** `[ServerId, Config]`
- **Restart policy:** temporary (no auto-restart)
- **Lifecycle:** Manual start/stop via `erlmcp_sup:stop_server/1`

### Transports
```erlang
erlmcp_sup:start_transport(TransportId, Type, Config) -> {ok, TransportPid}
```
- **Supervisor:** `erlmcp_transport_sup`
- **Child module:** Determined by `Type` (stdio, tcp, http, websocket)
- **Template args:** `[TransportId, Type, Config]`
- **Restart policy:** temporary (no auto-restart)
- **Lifecycle:** Manual start/stop via `erlmcp_sup:stop_transport/1`

---

## Configuration & Startup Options

### Application Environment (erlmcp.app.src:env)

```erlang
{client_defaults, #{
    timeout => 5000,              % Default request timeout (ms)
    strict_mode => false,         % MCP spec validation
    max_pending_requests => 100   % Queue limit
}}

{server_defaults, #{
    max_subscriptions_per_resource => 1000,  % Subscription limit
    max_progress_tokens => 10000               % Progress tracking limit
}}

{transport_defaults, #{
    tcp => #{
        connect_timeout => 5000,
        keepalive => true,
        nodelay => true           % Nagle off for low-latency
    },
    http => #{
        connect_timeout => 5000,
        request_timeout => 30000,
        max_connections => 100
    }
}}
```

---

## Module Files & Locations

| Module | File | Purpose | Type |
|--------|------|---------|------|
| `erlmcp_app` | `src/erlmcp_app.erl` | Application callback | Behavior |
| `erlmcp_sup` | `src/erlmcp_sup.erl` | Top-level supervisor (5 tiers) | Supervisor |
| `erlmcp_registry_sup` | `src/erlmcp_registry_sup.erl` | Registry subsystem (Tier 1) | Supervisor |
| `erlmcp_infrastructure_sup` | `src/erlmcp_infrastructure_sup.erl` | Infrastructure (Tier 2) | Supervisor |
| `erlmcp_server_sup` | `src/erlmcp_server_sup.erl` | Server instances (Tier 3) | Supervisor |
| `erlmcp_transport_sup` | `src/erlmcp_transport_sup.erl` | Transport layer (Tier 4) | Supervisor |
| `erlmcp_monitoring_sup` | `src/erlmcp_monitoring_sup.erl` | Monitoring (Tier 5, isolated) | Supervisor |
| `erlmcp_registry` | `src/erlmcp_registry.erl` | Message routing via gproc | Worker |
| `erlmcp_health_monitor` | `src/erlmcp_health_monitor.erl` | System health checks | Worker |
| `erlmcp_recovery_manager` | `src/erlmcp_recovery_manager.erl` | Failure recovery | Worker |

---

## Startup Sequence (Step-by-Step)

### 1. Application Start
```
erl -s erlmcp
  → application:start(erlmcp)
    → erlmcp_app:start(normal, [])
```

### 2. Initialization
```
erlmcp_app:start/2
  → erlmcp_logging:init_session_levels()  [ETS table for log levels]
    → erlmcp_sup:start_link()             [Start supervision tree]
```

### 3. Supervision Tree Startup (rest_for_one)
```
erlmcp_sup:init([])
  → erlmcp_registry_sup:start_link()        [TIER 1: Registry]
    → erlmcp_registry:start_link()          [gproc-based routing]
    → erlmcp_registry_health_check:start_link()

  → erlmcp_infrastructure_sup:start_link()  [TIER 2: Infrastructure]
    → erlmcp_hot_reload:start_link()
    → erlmcp_graceful_drain:start_link()
    → erlmcp_session_manager:start_link()
    → erlmcp_task_queue_manager:start_link()
    → erlmcp_subscription_manager:start_link()

  → erlmcp_server_sup:start_link()          [TIER 3: Server instances]
    → [Template ready, no initial children]

  → erlmcp_transport_sup:start_link()       [TIER 4: Transport handlers]
    → [Template ready, no initial children]

  → erlmcp_monitoring_sup:start_link()      [TIER 5: Observability]
    → erlmcp_health_monitor:start_link()
    → erlmcp_recovery_manager:start_link()
    → erlmcp_metrics_server:start_link()
    → erlmcp_metrics_http_sup:start_link()
    → erlmcp_config_sup:start_link()
```

### 4. Ready State
- All workers in Tiers 1-5 are now running
- Registry is accepting registrations
- Infrastructure is ready to manage sessions/tasks
- Supervisors are ready to spawn servers and transports on demand
- Monitoring is collecting system health data
- System listens for `erlmcp_sup:start_server()` and `erlmcp_sup:start_transport()` calls

---

## Registration & Naming

Processes register with OTP's local name registry:

```erlang
{local, erlmcp_sup}                % Top-level supervisor
{local, erlmcp_registry_sup}       % Tier 1
{local, erlmcp_infrastructure_sup} % Tier 2
{local, erlmcp_server_sup}         % Tier 3
{local, erlmcp_transport_sup}      % Tier 4
{local, erlmcp_monitoring_sup}     % Tier 5
```

Additional processes register with **gproc** (global process registry):
- `erlmcp_registry` publishes all server/transport registrations
- Health monitor registers system health checks
- Recovery manager registers failure recovery handlers

---

## Restart Policies

### Permanent vs. Temporary

| Policy | Behavior | Used For |
|--------|----------|----------|
| `permanent` | Auto-restart on crash (up to intensity limit) | Supervisors, core workers |
| `temporary` | **No restart** on crash; can be manually started | Dynamic servers/transports |

**Why temporary for servers/transports:**
- Clients control their lifecycle (manual `start_server`, `start_transport`)
- Clients handle reconnection logic
- Avoids thundering herd if many servers crash simultaneously

---

## Shutdown Cascade

When erlmcp application stops (or node shuts down):

```
erlmcp_sup receives shutdown signal
  ↓ (rest_for_one strategy)
  ├─ Shutdown erlmcp_monitoring_sup [TIER 5, first] (shutdown: infinity)
  ├─ Shutdown erlmcp_transport_sup [TIER 4] (shutdown: infinity)
  ├─ Shutdown erlmcp_server_sup [TIER 3] (shutdown: infinity)
  ├─ Shutdown erlmcp_infrastructure_sup [TIER 2] (shutdown: infinity)
  └─ Shutdown erlmcp_registry_sup [TIER 1, last] (shutdown: infinity)

Each worker receives SIGTERM, has up to infinity ms to clean up,
then forced termination.
```

**Note:** `shutdown: infinity` means processes can take as long as needed to shut down gracefully (e.g., drain connections, flush buffers).

---

## Tools & Debugging

### Generate Updated Metadata
```bash
tools/v2_arch/extract_entrypoints.escript
```
Outputs:
- `tools/v2_arch/entrypoints.json` - Machine-readable metadata
- `tools/v2_arch/entrypoints.txt` - Human-readable summary

### Inspect Live Supervisors
```erlang
(erlmcp@host)1> supervisor:which_children(erlmcp_sup).
[(erlmcp_registry_sup,...), ...]

(erlmcp@host)2> erlmcp_registry:list_servers().
[{server_id, Pid, Config}, ...]
```

### View Supervision Tree
```bash
make observer  % GUI process tree viewer
```

---

## Summary

**erlmcp v0.7.0 starts with:**
- **1 OTP application:** `erlmcp`
- **1 entry module:** `erlmcp_app` (calls `erlmcp_sup:start_link/0`)
- **1 top supervisor:** `erlmcp_sup` (rest_for_one strategy)
- **5 subsystem supervisors:** Registry, Infrastructure, Servers, Transports, Monitoring
- **18 OTP dependencies:** kernel, stdlib, jsx, gproc, gun, ranch, cowboy, opentelemetry, etc.
- **Dynamic process creation:** Servers and transports spawned on-demand by clients

The system uses a **bulkhead isolation pattern** to prevent cascading failures. Monitoring failures (Tier 5) don't affect protocol processing. Registry failures (Tier 1) block all message routing until recovery.

---

**Document generated:** `tools/v2_arch/extract_entrypoints.escript`
**Status:** erlmcp v0.7.0 runtime analysis
