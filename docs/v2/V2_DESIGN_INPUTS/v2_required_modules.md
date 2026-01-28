# erlmcp v2.0.0 Required Modules

**Minimum Kernel for Functional MCP SDK**

Version: v2.0.0-draft
Status: CANONICAL REFERENCE
Date: 2026-01-27

---

## Purpose

This document defines the **minimum set of modules** required for erlmcp v2.0.0 to function as a production-grade MCP SDK.

**Target**: 60-70 modules (down from 247)
**Savings**: 177-187 modules deleted or separated

---

## Required Modules by Layer

### Layer 1: Supervision (6 modules)

| Module | Type | Purpose | LOC | Required? |
|--------|------|---------|-----|-----------|
| `erlmcp_sup.erl` | supervisor | Top-level supervisor (rest_for_one, 5 tiers) | 211 | ✅ CRITICAL |
| `erlmcp_registry_sup.erl` | supervisor | Registry subsystem supervisor | 80 | ✅ CRITICAL |
| `erlmcp_infrastructure_sup.erl` | supervisor | Infrastructure subsystem (sessions, tasks) | 100 | ✅ CRITICAL |
| `erlmcp_server_sup.erl` | supervisor | Server pool (simple_one_for_one) | 120 | ✅ CRITICAL |
| `erlmcp_transport_sup.erl` | supervisor | Transport pool (simple_one_for_one) | 150 | ✅ CRITICAL |
| `erlmcp_monitoring_sup.erl` | supervisor | Observability subsystem | 100 | ✅ REQUIRED |

**Total**: 6 modules, 761 LOC

**Rationale**: OTP supervision is the architectural foundation. All 6 supervisors are mandatory for bulkhead pattern.

---

### Layer 2: Core Protocol (15 modules)

| Module | Type | Purpose | LOC | Required? |
|--------|------|---------|-----|-----------|
| `erlmcp_client.erl` | gen_server | MCP client implementation | 400 | ✅ CRITICAL |
| `erlmcp_server.erl` | gen_server | MCP server implementation | 600 | ✅ CRITICAL |
| `erlmcp_server_handlers.erl` | module | Resource/tool/prompt handler helpers | 200 | ✅ REQUIRED |
| `erlmcp_json_rpc.erl` | module | JSON-RPC 2.0 encode/decode | 300 | ✅ CRITICAL |
| `erlmcp_message_parser.erl` | module | Length-delimited JSON parser | 150 | ✅ CRITICAL |
| `erlmcp_capabilities.erl` | module | MCP capability negotiation | 180 | ✅ CRITICAL |
| `erlmcp_templates.erl` | module | URI template matching | 120 | ✅ REQUIRED |
| `erlmcp_validation.erl` | module | Input validation (JSON Schema) | 200 | ✅ REQUIRED |
| `erlmcp_error.erl` | module | Error encoding/decoding | 100 | ✅ REQUIRED |
| `erlmcp_completion_context.erl` | module | LLM completion context | 150 | ⚠️ OPTIONAL |
| `erlmcp_progress.erl` | module | Progress token tracking | 100 | ⚠️ OPTIONAL |
| `erlmcp_roots.erl` | module | Filesystem roots capability | 130 | ⚠️ OPTIONAL |
| `erlmcp_subscription_handlers.erl` | gen_server | Resource subscription tracking | 250 | ✅ REQUIRED |
| `erlmcp_resource_list_changed.erl` | module | Resource list change notifications | 80 | ✅ REQUIRED |
| `erlmcp_tool_change_notifier.erl` | module | Tool list change notifications | 80 | ✅ REQUIRED |

**Total**: 15 modules, 3,040 LOC

**Rationale**: These 15 modules implement the MCP protocol specification. 12 are CRITICAL (cannot function without), 3 are OPTIONAL (advanced features).

---

### Layer 3: Transports (9 modules, after consolidation)

| Module | Type | Transport | Library | LOC | Required? |
|--------|------|-----------|---------|-----|-----------|
| `erlmcp_transport.erl` | behavior | Interface definition | N/A | 50 | ✅ CRITICAL |
| `erlmcp_transport_behavior.erl` | behavior | Behavior callbacks | N/A | 60 | ✅ CRITICAL |
| `erlmcp_transport_stdio.erl` | gen_server | stdio pipes (local LLMs) | stdlib | 300 | ✅ CRITICAL |
| `erlmcp_transport_tcp.erl` | ranch protocol | TCP sockets (production) | ranch | 400 | ✅ CRITICAL |
| `erlmcp_transport_http.erl` | gun client | HTTP/2 (web clients) | gun | 500 | ✅ REQUIRED |
| `erlmcp_transport_ws.erl` | cowboy handler | WebSocket (bidirectional web) | cowboy | 400 | ⚠️ OPTIONAL |
| `erlmcp_transport_api.erl` | module | Transport API helpers | N/A | 100 | ✅ REQUIRED |
| `erlmcp_transport_validation.erl` | module | Transport config validation | N/A | 150 | ✅ REQUIRED |
| `erlmcp_transport_http_server.erl` | cowboy handler | HTTP server (SSE) | cowboy | 350 | ⚠️ OPTIONAL |

**Total**: 9 modules, 2,310 LOC

**Rationale**: stdio, TCP, HTTP are mandatory (covers 95% of use cases). WebSocket and HTTP server are optional (advanced features).

**v2 Consolidation**: Delete `erlmcp_transport_http_new.erl`, `erlmcp_transport_http_adapter.erl`, `erlmcp_transport_sse.erl` (3 modules → merged into http.erl).

---

### Layer 4: Registry (2 modules, after gproc migration)

| Module | Type | Purpose | LOC | Required? |
|--------|------|---------|-----|-----------|
| `erlmcp_registry.erl` | gen_server | Process registry (Server ID → Pid) | 100 | ✅ CRITICAL (gproc wrapper) |
| `erlmcp_registry_health_check.erl` | gen_server | Registry health monitoring | 150 | ⚠️ OPTIONAL |

**Total**: 2 modules, 250 LOC (down from 400 LOC custom registry)

**Rationale**: Registry is critical for message routing. v2 migrates to gproc (400 LOC custom → 100 LOC wrapper).

**Deleted**: `erlmcp_registry_sharded.erl` (experimental, 300 LOC).

---

### Layer 5: Infrastructure (12 modules, reviewed)

| Module | Type | Purpose | LOC | Required? |
|--------|------|---------|-----|-----------|
| `erlmcp_session_manager.erl` | gen_statem | Session lifecycle FSM | 350 | ✅ CRITICAL |
| `erlmcp_session_replicator.erl` | gen_server | HA session replication | 400 | ⚠️ OPTIONAL (HA only) |
| `erlmcp_session_failover.erl` | gen_server | Session failover logic | 300 | ⚠️ OPTIONAL (HA only) |
| `erlmcp_task_manager.erl` | gen_server | Async task coordination | 250 | ✅ REQUIRED |
| `erlmcp_queue_bounded.erl` | gen_server | Bounded queue (backpressure) | 250 | ✅ REQUIRED |
| `erlmcp_queue_limits.erl` | module | Queue limit enforcement | 150 | ✅ REQUIRED |
| `erlmcp_backpressure.erl` | module | Backpressure handling | 200 | ✅ REQUIRED |
| `erlmcp_backpressure_signal.erl` | module | Backpressure signaling | 120 | ✅ REQUIRED |
| `erlmcp_circuit_breaker.erl` | gen_server | Circuit breaker pattern | 250 | ⚠️ OPTIONAL (resilience) |
| `erlmcp_graceful_degradation.erl` | module | Graceful degradation | 180 | ⚠️ OPTIONAL (resilience) |
| `erlmcp_graceful_drain.erl` | module | Graceful connection drain | 150 | ⚠️ OPTIONAL (deployment) |
| `erlmcp_recovery_manager.erl` | gen_server | Recovery orchestration | 300 | ⚠️ OPTIONAL (resilience) |

**Total**: 12 modules, 2,900 LOC

**Rationale**: Session manager, task manager, backpressure are REQUIRED (core functionality). HA/resilience modules are OPTIONAL (production features).

**Deleted**: `erlmcp_connection_pool*.erl` (2 modules, 400 LOC → migrate to poolboy).

---

### Layer 6: Observability (15 modules, after consolidation)

| Module | Type | Purpose | LOC | Required? |
|--------|------|---------|-----|-----------|
| `erlmcp_metrics.erl` | gen_server | Core metrics collection | 400 | ✅ REQUIRED |
| `erlmcp_metrics_server.erl` | gen_server | Metrics aggregation | 300 | ✅ REQUIRED |
| `erlmcp_metrics_http.erl` | cowboy handler | Prometheus /metrics endpoint | 250 | ✅ REQUIRED |
| `erlmcp_metrics_http_handler.erl` | cowboy handler | HTTP handler | 150 | ✅ REQUIRED |
| `erlmcp_metrics_http_sup.erl` | supervisor | HTTP supervisor | 80 | ✅ REQUIRED |
| `erlmcp_metrics_http_worker.erl` | gen_server | HTTP worker | 120 | ✅ REQUIRED |
| `erlmcp_health.erl` | module | Health check API | 200 | ✅ REQUIRED |
| `erlmcp_health_monitor.erl` | gen_server | Health monitoring | 250 | ✅ REQUIRED |
| `erlmcp_monitor_config.erl` | module | Monitor configuration | 120 | ✅ REQUIRED |
| `erlmcp_monitor_sup.erl` | supervisor | Monitor supervisor | 100 | ✅ REQUIRED |
| `erlmcp_otel.erl` | gen_server | OTEL integration | 350 | ⚠️ OPTIONAL (OTEL) |
| `erlmcp_tracing.erl` | gen_server | Distributed tracing | 280 | ⚠️ OPTIONAL (OTEL) |
| `erlmcp_trace_propagation.erl` | module | Trace context propagation | 150 | ⚠️ OPTIONAL (OTEL) |
| `erlmcp_memory_profiler.erl` | gen_server | Memory profiling | 220 | ⚠️ OPTIONAL (debugging) |
| `erlmcp_memory_accounting.erl` | gen_server | Memory accounting | 180 | ⚠️ OPTIONAL (debugging) |

**Total**: 15 modules, 3,150 LOC

**Rationale**: Metrics and health checks are REQUIRED (production observability). OTEL and profiling are OPTIONAL (advanced features).

**Deleted**: `erlmcp_simple_metrics.erl`, `erlmcp_simple_monitor.erl`, `erlmcp_simple_trace.erl` (3 modules → merged into canonical modules).

---

### Layer 7: Benchmarks (5 modules, canonical)

| Module | Type | Purpose | Workloads | Required? |
|--------|------|---------|-----------|-----------|
| `erlmcp_bench_core_ops.erl` | benchmark | In-memory ops (registry, queue, pool, session) | 4 workloads | ✅ REQUIRED |
| `erlmcp_bench_network_real.erl` | benchmark | Real sockets (TCP, HTTP) | 7 workloads | ✅ REQUIRED |
| `erlmcp_bench_stress.erl` | benchmark | Sustained load (30s-24hr) | 4 durations | ✅ REQUIRED |
| `erlmcp_bench_chaos.erl` | benchmark | Failure injection | 11 chaos tests | ✅ REQUIRED |
| `erlmcp_bench_integration.erl` | benchmark | MCP e2e workflows | 5 integration tests | ✅ REQUIRED |

**Total**: 5 modules, ~2,000 LOC

**Rationale**: Performance validation is mandatory. All 5 benchmarks are REQUIRED.

**Deleted**: 9 legacy benchmarks (benchmark_100k.erl, latency_SUITE.erl, etc.).

---

### Layer 8: Utilities (6 modules)

| Module | Type | Purpose | LOC | Required? |
|--------|------|---------|-----|-----------|
| `erlmcp_app.erl` | application | OTP application callback | 50 | ✅ CRITICAL |
| `erlmcp_util.erl` | module | Common utilities | 200 | ✅ REQUIRED |
| `erlmcp_config.erl` | gen_server | Configuration management | 250 | ✅ REQUIRED |
| `erlmcp_config_loader.erl` | module | Config file loading | 150 | ✅ REQUIRED |
| `erlmcp_config_schema.erl` | module | Config validation schema | 200 | ✅ REQUIRED |
| `erlmcp_logging.erl` | module | Structured logging | 180 | ✅ REQUIRED |

**Total**: 6 modules, 1,030 LOC

**Rationale**: Application boilerplate and utilities are mandatory.

---

## Module Count Summary

| Layer | Required | Optional | Total | LOC |
|-------|----------|----------|-------|-----|
| **Supervision** | 6 | 0 | 6 | 761 |
| **Core Protocol** | 12 | 3 | 15 | 3,040 |
| **Transports** | 6 | 3 | 9 | 2,310 |
| **Registry** | 1 | 1 | 2 | 250 |
| **Infrastructure** | 6 | 6 | 12 | 2,900 |
| **Observability** | 10 | 5 | 15 | 3,150 |
| **Benchmarks** | 5 | 0 | 5 | 2,000 |
| **Utilities** | 6 | 0 | 6 | 1,030 |
| **TOTAL** | **52** | **18** | **70** | **15,441** |

**Target**: 60-70 modules ✅ ACHIEVED (70 modules)
**LOC**: ~15,441 (down from ~37,000, 58% reduction)

---

## Minimum Viable Kernel (52 Modules)

For **absolute minimum** functionality (stdio only, no HA, no OTEL):

**Required Modules** (52):
- ✅ Supervision (6)
- ✅ Core Protocol (12)
- ✅ Transports: stdio + TCP (4 modules: behavior, stdio, tcp, API)
- ✅ Registry (1: gproc wrapper)
- ✅ Infrastructure (6: session manager, task manager, backpressure)
- ✅ Observability (10: metrics, health, monitoring)
- ✅ Benchmarks (5)
- ✅ Utilities (6)

**Total**: 52 modules, ~11,000 LOC

**Use Case**: Lightweight deployments, embedded systems, LLM integrations (stdio only).

---

## Optional Feature Modules (18 Modules)

**Optional Modules** (18):
- ⚠️ Core Protocol: completion_context, progress, roots (3)
- ⚠️ Transports: WebSocket, HTTP server, SSE (3)
- ⚠️ Registry: health check (1)
- ⚠️ Infrastructure: HA (session replicator, failover), resilience (circuit breaker, graceful degradation, recovery) (6)
- ⚠️ Observability: OTEL (3), profiling (2)

**Total**: 18 modules, ~4,441 LOC

**Use Case**: Production deployments with HA, OTEL, WebSocket support.

---

## Deleted Modules (177 Modules)

**Separated** (85+ modules):
- All `tcps_*.erl` modules → Move to `apps/tcps_erlmcp/`

**Deleted** (37 modules):
- Experimental (12): complex routing, audio, icon caching, sampling, elicitation, etc.
- Legacy CLI (8): marketplace, receipt CLI, plan CLI
- Legacy dashboard (5): regression dashboard, SLA dashboards
- Legacy profiling (4): CPU profiler, latency profiler
- Legacy planning (8): plan loader, SLA monitor

**Deleted** (9 benchmarks):
- Legacy benchmarks: benchmark_100k.erl, latency_SUITE.erl, throughput_SUITE.erl, etc.

**Deleted** (6 duplicates):
- simple_metrics, simple_monitor, simple_trace (3 pairs → merged)
- erlmcp_transport_http_new, erlmcp_transport_http_adapter, erlmcp_transport_sse (3 → merged into http.erl)

**Deleted** (40+ miscellaneous):
- Experimental features with no dependencies

**Total Deleted**: 177 modules

---

## Migration Checklist

### Phase 1: TCPS Separation (CRITICAL)
- [ ] Create `apps/tcps_erlmcp/` OTP application
- [ ] Move 85+ `tcps_*.erl` modules
- [ ] Update rebar.config: `{applications, [erlmcp]}` (optional dep)
- [ ] Verify erlmcp compiles without TCPS

---

### Phase 2: Delete Legacy (HIGH PRIORITY)
- [ ] Delete 37 experimental/deprecated modules
- [ ] Delete 9 legacy benchmarks
- [ ] Delete 6 duplicate modules (simple_*, transport HTTP variants)
- [ ] Run tests: `rebar3 eunit`, `rebar3 ct`

---

### Phase 3: Library Migration (MODERATE PRIORITY)
- [ ] Migrate registry to gproc (400 LOC → 100 LOC wrapper)
- [ ] Migrate connection pool to poolboy (300 LOC → 80 LOC wrapper)
- [ ] Consolidate HTTP transports (3 modules → 1 gun-based module)
- [ ] Update documentation

---

### Phase 4: Validation (FINAL)
- [ ] Run benchmark suite: `bench/run_all_benchmarks.sh`
- [ ] Verify no performance regressions (≤10%)
- [ ] Run integration tests: `rebar3 ct`
- [ ] Update API documentation
- [ ] Release v2.0.0

---

## References

### Source Documents
- **Principles**: [v2_principles.md](./v2_principles.md)
- **Deletions**: [v2_deletions.md](./v2_deletions.md)
- **Risks**: [v2_risks.md](./v2_risks.md)
- **Code Map**: [L4-code-map.md](../C4/L4-code-map.md)
- **Glossary**: [GLOSSARY.md](../GLOSSARY.md)

### Source Code
- **Supervision**: `src/erlmcp_sup.erl:L111-L211`
- **Protocol**: `src/erlmcp_server.erl`, `src/erlmcp_client.erl`
- **Transports**: `src/erlmcp_transport_*.erl`

---

**Document Status**: CANONICAL (v2.0.0-draft)
**Last Updated**: 2026-01-27
**Purpose**: Define minimum kernel for erlmcp v2.0.0 (60-70 modules)
