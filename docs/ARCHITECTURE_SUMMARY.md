# erlmcp v2.0.0 Architecture Summary

> **Quick Reference** - Comprehensive architecture details in [architecture.md](architecture.md)

## 30-Second Overview

erlmcp v2.0.0 is a **production-grade umbrella application** implementing the Model Context Protocol (MCP) in Erlang/OTP with:

- **4 Independent OTP Applications** - Core, Transports, Observability, Quality (optional)
- **94 Modules** organized by domain boundaries
- **Fault-Tolerant Supervision** - Bulkhead pattern prevents cascading failures
- **6 Transport Types** - STDIO, TCP, HTTP/2, WebSocket, SSE, GraphQL
- **Production Libraries** - gproc, gun, ranch, poolboy (~770 LOC reduction)
- **Comprehensive Observability** - Metrics, OpenTelemetry, SHA-256 receipts
- **Optional Quality System** - TCPS with 8 quality gates and Jidoka

---

## Applications

### 1. erlmcp_core (14 modules) - REQUIRED

**Foundation layer with no internal dependencies**

- **JSON-RPC 2.0** - Protocol encoding/decoding
- **MCP Client** - Request correlation, response handling
- **MCP Server** - Resource/tool/prompt management
- **Registry** - gproc-based routing with automatic monitoring
- **Sessions** - Lifecycle management, failover, replication
- **Cache** - Multi-level (L1: ETS, L2: Mnesia, L3: External)

**Dependencies:** jsx, jesse, gproc

---

### 2. erlmcp_transports (8 modules) - REQUIRED

**Pluggable network layer**

- **STDIO** - Standard I/O pipes
- **TCP** - ranch acceptor pools
- **HTTP/2** - gun client with multiplexing
- **WebSocket** - gun bidirectional streaming
- **SSE** - Server-Sent Events
- **GraphQL** - Query/mutation/subscription

**Dependencies:** gun, ranch, poolboy, **erlmcp_core**

---

### 3. erlmcp_observability (9 modules) - REQUIRED

**Comprehensive observability (isolated from core)**

- **Metrics** - Throughput, latency, errors, aggregation
- **OpenTelemetry** - Trace context propagation, span injection
- **Receipts** - SHA-256 deterministic audit trails
- **Health** - Component monitoring, recovery manager
- **Chaos** - Resilience testing framework
- **Dashboard** - Real-time WebSocket metrics

**Dependencies:** opentelemetry_*, **erlmcp_core**

---

### 4. tcps_erlmcp (63 modules) - OPTIONAL

**Zero-defect delivery via Toyota Production System**

- **SHACL** - RDF/Turtle ontology validation
- **Quality Gates** - 8 sequential gates with stop-the-line
- **Kanban** - WIP limits, visual management
- **Heijunka** - Production leveling
- **Jidoka** - Built-in quality, Andon events
- **SKU** - Release management with evidence bundles

**Dependencies:** bbmustache, cowboy, jobs, fs, **erlmcp_core**, **erlmcp_observability**

---

## Supervision Tree

```
erlmcp_sup (one_for_one)
├── erlmcp_core_sup (one_for_one)
│   ├── Registry (gproc)
│   ├── Sessions & Tasks
│   ├── Cache & Failover
│   └── Cluster Management
├── erlmcp_server_sup (simple_one_for_one)
│   └── [Dynamic MCP servers]
└── erlmcp_observability_sup (one_for_one)
    ├── Metrics & Aggregation
    ├── OTEL & Receipts
    └── Health & Recovery

erlmcp_transport_sup (one_for_one)
└── [Dynamic transports]

tcps_erlmcp_sup (one_for_one) [OPTIONAL]
├── Quality Gates
├── Kanban & Heijunka
└── Dashboard & Metrics
```

**Strategy:** `one_for_one` throughout - no cascading failures

---

## Communication Patterns

### Registry-Based Routing (gproc)

```erlang
%% Register server
gproc:add_local_name({mcp, server, ServerId})

%% Lookup and route
case gproc:lookup_local_name({mcp, server, ServerId}) of
    undefined -> {error, not_found};
    Pid -> gen_server:cast(Pid, Message)
end
```

**Benefits:**
- O(1) lookups via ETS
- Automatic process monitoring
- Automatic cleanup on death
- Distributed registry support

---

### Message Flow (Cross-App)

```
Client API
    ↓
[erlmcp_core] Encode JSON-RPC
    ↓
[erlmcp_core] Registry lookup
    ↓
[erlmcp_transports] Transport send
    ↓
Network I/O
    ↓
[erlmcp_transports] Transport receive
    ↓
[erlmcp_core] Registry route to server
    ↓
[erlmcp_core] Execute handler
    ↓
[erlmcp_observability] Record metrics (async)
```

**Key Points:**
- No direct process coupling between apps
- Registry-based indirect communication
- Observability as non-blocking side effects

---

## Failure Isolation

| Component Crash | Restart Scope | Recovery Time | Impact |
|-----------------|---------------|---------------|--------|
| Registry | Registry only | ~500ms | New routing fails; existing connections continue |
| Transport | That transport | ~2s | That transport down; others unaffected |
| Metrics | Metrics only | ~500ms | Zero impact on protocol |
| Quality Gate | That gate only | ~1s | Other gates continue |

**Guarantees:**
- Bulkhead pattern - failures contained
- `one_for_one` - no cascading restarts
- Observability isolation - monitoring never affects protocol

---

## Deployment Options

### Minimal (Core + Transports + Observability)

```bash
# 31 modules, ~50MB release
rebar3 as prod release
```

### Full (with TCPS Quality System)

```bash
# 94 modules, ~65MB release
# Includes quality gates, SHACL, Jidoka
rebar3 as prod release
```

---

## Performance (v1.5.0 Benchmarks)

| Metric | Target | Achieved |
|--------|--------|----------|
| Registry Throughput | >500K msg/s | 553K msg/s |
| Queue Operations | >900K ops/s | 971K ops/s |
| Pool Management | >100K ops/s | 149K ops/s |
| Network I/O | >40K msg/s | 43K msg/s |
| Sustained Load | >300K msg/s | 372K msg/s |
| Concurrent Connections | >40K | 40-50K per node |

---

## Key Files

| Component | File | Purpose |
|-----------|------|---------|
| **Core** | `apps/erlmcp_core/src/erlmcp_sup.erl` | Top-level supervisor |
| **Core** | `apps/erlmcp_core/src/erlmcp_registry.erl` | gproc-based routing |
| **Core** | `apps/erlmcp_core/src/erlmcp_client.erl` | MCP client gen_server |
| **Core** | `apps/erlmcp_core/src/erlmcp_server.erl` | MCP server gen_server |
| **Transports** | `apps/erlmcp_transports/src/erlmcp_transport_sup.erl` | Transport supervisor |
| **Transports** | `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl` | TCP transport (ranch) |
| **Transports** | `apps/erlmcp_transports/src/erlmcp_transport_http.erl` | HTTP/2 (gun) |
| **Observability** | `apps/erlmcp_observability/src/erlmcp_metrics.erl` | Metrics collection |
| **Observability** | `apps/erlmcp_observability/src/erlmcp_otel.erl` | OpenTelemetry |
| **TCPS** | `apps/tcps_erlmcp/src/tcps_quality_gates.erl` | 8 quality gates |
| **TCPS** | `apps/tcps_erlmcp/src/tcps_shacl_validator.erl` | SHACL validation |

---

## Quick Commands

```bash
# Build all apps
rebar3 compile

# Test per app
rebar3 eunit --app erlmcp_core
rebar3 eunit --app erlmcp_transports
rebar3 eunit --app erlmcp_observability
rebar3 eunit --app tcps_erlmcp

# Full validation
make check

# Production release
rebar3 as prod release

# Quality gates (if TCPS enabled)
rebar3 tcps check-all-gates --sku=$(git rev-parse --short HEAD)
```

---

## Migration from v1.x

**Breaking Changes:**
- Umbrella structure (not single app)
- Module paths: `src/` → `apps/*/src/`
- TCPS now optional (separate app)

**API Unchanged:**
- ✅ `erlmcp_client`, `erlmcp_server` work as before
- ✅ Transport interface unchanged
- ✅ Configuration format unchanged
- ✅ MCP 1.0 protocol compliance maintained

---

## Documentation

- **[Complete Architecture](architecture.md)** - In-depth design, supervision, communication
- **[API Reference](api-reference.md)** - Client/server/transport APIs
- **[OTP Patterns](otp-patterns.md)** - Erlang/OTP best practices
- **[Protocol Guide](protocol.md)** - MCP implementation details
- **[Quality Gates](quality-enforcement/INDEX.md)** - TCPS zero-defect system

---

## Support

- **GitHub Issues:** https://github.com/banyan-platform/erlmcp/issues
- **Documentation:** https://github.com/banyan-platform/erlmcp/tree/main/docs
- **MCP Spec:** https://modelcontextprotocol.io

---

**Last Updated:** 2026-01-28 | **Version:** 2.0.0
