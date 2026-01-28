# C4 Level 4: Code Map

**erlmcp v2.0.0 Architecture - Module Inventory and Callgraph Clusters**

---

## Purpose

This document provides a comprehensive map of erlmcp's 214 Erlang modules, classified by architecture layer, canonical vs. legacy status, and callgraph relationships.

**Audience**: v2 refactoring architects, code reviewers, technical debt analysis.

---

## Module Statistics

| Category | Module Count | % of Total | v2 Disposition |
|----------|--------------|------------|----------------|
| **TCPS** | 85+ | 40% | SEPARATE (move to tcps_erlmcp package) |
| **Core Protocol** | 15 | 7% | CANONICAL (keep, refactor) |
| **Transports** | 12 | 6% | CANONICAL (keep, library migration) |
| **Registry/Supervision** | 8 | 4% | CANONICAL (migrate to gproc) |
| **Benchmarks** | 14 | 7% | CANONICAL (keep, enhance) |
| **Infrastructure** | 25 | 12% | MIXED (review per module) |
| **Monitoring/Metrics** | 18 | 8% | CANONICAL (keep, consolidate) |
| **Experimental/Legacy** | 37 | 17% | LEGACY (delete or isolate) |

**Total**: 214 modules (as of 2026-01-27)

**Source**: `find src -name "*.erl" | wc -l`

---

## Module Classification

### CANONICAL: Core Protocol (v2 Must-Keep)

**Modules**: 15 modules

| Module | Type | Purpose | LOC | Calls |
|--------|------|---------|-----|-------|
| `erlmcp_client.erl` | gen_server | MCP client implementation | ~400 | json_rpc, transport, registry |
| `erlmcp_server.erl` | gen_server | MCP server implementation | ~600 | json_rpc, capabilities, handlers |
| `erlmcp_server_handlers.erl` | module | Resource/tool/prompt handler helpers | ~200 | server |
| `erlmcp_json_rpc.erl` | module | JSON-RPC 2.0 encode/decode | ~300 | jsx, jesse |
| `erlmcp_message_parser.erl` | module | Length-delimited JSON parser | ~150 | json_rpc |
| `erlmcp_capabilities.erl` | module | MCP capability negotiation | ~180 | server, client |
| `erlmcp_templates.erl` | module | URI template matching | ~120 | server |
| `erlmcp_validation.erl` | module | Input validation (JSON Schema) | ~200 | jesse |
| `erlmcp_error.erl` | module | Error encoding/decoding | ~100 | json_rpc |
| `erlmcp_completion_context.erl` | module | LLM completion context | ~150 | server |
| `erlmcp_progress.erl` | module | Progress token tracking | ~100 | server |
| `erlmcp_roots.erl` | module | Filesystem roots capability | ~130 | server |
| `erlmcp_subscription_handlers.erl` | gen_server | Resource subscription tracking | ~250 | server, registry |
| `erlmcp_resource_list_changed.erl` | module | Resource list change notifications | ~80 | server |
| `erlmcp_tool_change_notifier.erl` | module | Tool list change notifications | ~80 | server |

**LOC Total**: ~3,040 lines (core protocol kernel)

**v2 Action**: KEEP, refactor for clarity, add type specs, enhance tests.

---

### CANONICAL: Transports (v2 Must-Keep)

**Modules**: 12 modules

| Module | Type | Transport | Library | LOC |
|--------|------|-----------|---------|-----|
| `erlmcp_transport.erl` | behavior | Interface | N/A | ~50 |
| `erlmcp_transport_behavior.erl` | behavior | Behavior callbacks | N/A | ~60 |
| `erlmcp_transport_stdio.erl` | gen_server | stdio pipes | stdlib | ~300 |
| `erlmcp_transport_tcp.erl` | ranch protocol | TCP sockets | ranch | ~400 |
| `erlmcp_transport_http.erl` | gun client | HTTP/1.1, HTTP/2 | gun | ~500 |
| `erlmcp_transport_http_new.erl` | gen_server | HTTP refactor | gun | ~450 |
| `erlmcp_transport_http_adapter.erl` | module | HTTP adapter | gun | ~120 |
| `erlmcp_transport_http_server.erl` | cowboy handler | HTTP server | cowboy | ~350 |
| `erlmcp_transport_ws.erl` | cowboy handler | WebSocket | cowboy | ~400 |
| `erlmcp_transport_sse.erl` | gen_server | Server-Sent Events | cowboy | ~300 |
| `erlmcp_transport_api.erl` | module | Transport API helpers | N/A | ~100 |
| `erlmcp_transport_validation.erl` | module | Transport config validation | N/A | ~150 |

**LOC Total**: ~3,180 lines

**v2 Action**: KEEP, complete library migration (gun, ranch, cowboy), consolidate HTTP modules.

---

### CANONICAL: Registry & Supervision (v2 Must-Keep, Migrate)

**Modules**: 8 modules

| Module | Type | Purpose | LOC | v2 Action |
|--------|------|---------|-----|-----------|
| `erlmcp_sup.erl` | supervisor | Top-level supervisor (rest_for_one) | ~211 | KEEP (document bulkhead) |
| `erlmcp_registry.erl` | gen_server | Process registry (Server ID → Pid) | ~400 | MIGRATE to gproc |
| `erlmcp_registry_sup.erl` | supervisor | Registry supervisor | ~80 | KEEP |
| `erlmcp_registry_sharded.erl` | gen_server | Sharded registry (experimental) | ~300 | DELETE (use gproc) |
| `erlmcp_registry_health_check.erl` | gen_server | Registry health monitoring | ~150 | KEEP |
| `erlmcp_server_sup.erl` | supervisor | Server pool (simple_one_for_one) | ~120 | KEEP |
| `erlmcp_transport_sup.erl` | supervisor | Transport pool (simple_one_for_one) | ~150 | KEEP |
| `erlmcp_infrastructure_sup.erl` | supervisor | Infrastructure subsystem | ~100 | KEEP |

**LOC Total**: ~1,511 lines

**v2 Action**: Migrate `erlmcp_registry.erl` to gproc (save ~400 LOC), delete experimental sharding.

---

### CANONICAL: Benchmarks (v2 Must-Keep)

**Modules**: 14 modules

| Module | Type | Purpose | Workloads |
|--------|------|---------|-----------|
| `erlmcp_bench_core_ops.erl` | benchmark | In-memory ops (registry, queue, pool, session) | 4 workloads (1K-1M ops) |
| `erlmcp_bench_network_real.erl` | benchmark | Real sockets (TCP, HTTP) | 7 workloads (100-100K conn) |
| `erlmcp_bench_stress.erl` | benchmark | Sustained load (30s-24hr) | 4 durations |
| `erlmcp_bench_chaos.erl` | benchmark | Failure injection (11 scenarios) | 11 chaos tests |
| `erlmcp_bench_integration.erl` | benchmark | MCP e2e workflows | 5 integration tests |
| `erlmcp_bench_helpers.erl` | module | Benchmark utilities | N/A |
| `erlmcp_bench_helpers_tests.erl` | eunit | Benchmark tests | N/A |
| `benchmark_100k.erl` | benchmark | Legacy 100K benchmark | DEPRECATED |
| `benchmark_100k_SUITE.erl` | ct | Legacy CT suite | DEPRECATED |
| `latency_SUITE.erl` | ct | Latency benchmark suite | DEPRECATED |
| `throughput_SUITE.erl` | ct | Throughput benchmark suite | DEPRECATED |
| `erlmcp_registry_contention.erl` | benchmark | Registry contention test | 1 workload |
| `erlmcp_transport_tcp_4kb.erl` | benchmark | TCP 4KB packet test | 1 workload |
| `test_network_real_bench.erl` | benchmark | Network bench test | 1 workload |

**v2 Action**: KEEP 5 canonical benchmarks (core_ops, network_real, stress, chaos, integration), DELETE 9 legacy benchmarks.

---

### TCPS: Manufacturing System (v2 SEPARATE PACKAGE)

**Modules**: 85+ modules (40% of codebase)

**Categories**:
1. **CLI** (10 modules): `tcps_cli_*.erl`
2. **Dashboard** (8 modules): `tcps_dashboard*.erl`, `tcps_websocket_handler.erl`
3. **Core** (15 modules): `tcps_andon.erl`, `tcps_heijunka.erl`, `tcps_kanban.erl`, etc.
4. **MCP Integration** (5 modules): `tcps_mcp_*.erl`
5. **Documentation** (12 modules): `tcps_diataxis_*.erl`, `tcps_tutorial_*.erl`
6. **Persistence** (8 modules): `tcps_receipt*.erl`, `tcps_persistence.erl`, `tcps_ontology_index.erl`
7. **Quality** (10 modules): `tcps_quality_gates.erl`, `tcps_rebar3_*.erl`
8. **Simulator** (7 modules): `tcps_simulator*.erl`, `tcps_scenario_loader.erl`
9. **Metrics** (5 modules): `tcps_metrics_*.erl`
10. **Utilities** (5 modules): `tcps_config_reference.erl`, `tcps_principles.erl`, etc.

**Total LOC**: ~15,000+ lines (estimated)

**v2 Action**: SEPARATE into `tcps_erlmcp` OTP application
- Rationale: TCPS is a complete manufacturing workflow system orthogonal to MCP protocol
- Benefit: erlmcp v2 focuses on MCP SDK, TCPS becomes optional dependency
- Migration: Create `apps/tcps_erlmcp/` directory, update rebar.config

**Source**: `find src -name "tcps_*.erl" | wc -l` → 85+ modules

---

### MIXED: Infrastructure (Review Per Module)

**Modules**: 25 modules

| Module | Purpose | LOC | v2 Action |
|--------|---------|-----|-----------|
| `erlmcp_session_manager.erl` | Session lifecycle FSM | ~350 | KEEP (canonical) |
| `erlmcp_session_replicator.erl` | HA session replication | ~400 | KEEP (HA feature) |
| `erlmcp_session_failover.erl` | Session failover logic | ~300 | KEEP (HA feature) |
| `erlmcp_task_manager.erl` | Async task coordination | ~250 | KEEP (canonical) |
| `erlmcp_connection_pool.erl` | Connection pooling | ~300 | MIGRATE to poolboy |
| `erlmcp_connection_pool_sup.erl` | Pool supervisor | ~100 | DELETE (poolboy handles) |
| `erlmcp_connection_optimizer.erl` | Connection optimization | ~200 | REVIEW (experimental?) |
| `erlmcp_queue_bounded.erl` | Bounded queue | ~250 | KEEP (backpressure) |
| `erlmcp_queue_limits.erl` | Queue limit enforcement | ~150 | KEEP (backpressure) |
| `erlmcp_queue_optimized.erl` | Optimized queue | ~200 | REVIEW (vs bounded) |
| `erlmcp_queue_benchmark.erl` | Queue benchmark | ~150 | MOVE to bench/ |
| `erlmcp_memory_pool.erl` | Memory pooling | ~200 | REVIEW (experimental?) |
| `erlmcp_memory_optimization.erl` | Memory optimization | ~180 | REVIEW (experimental?) |
| `erlmcp_memory_profiler.erl` | Memory profiling | ~220 | KEEP (observability) |
| `erlmcp_memory_accounting.erl` | Memory accounting | ~180 | KEEP (observability) |
| `erlmcp_backpressure.erl` | Backpressure handling | ~200 | KEEP (canonical) |
| `erlmcp_backpressure_signal.erl` | Backpressure signaling | ~120 | KEEP (canonical) |
| `erlmcp_circuit_breaker.erl` | Circuit breaker pattern | ~250 | KEEP (resilience) |
| `erlmcp_graceful_degradation.erl` | Graceful degradation | ~180 | KEEP (resilience) |
| `erlmcp_graceful_drain.erl` | Graceful connection drain | ~150 | KEEP (deployment) |
| `erlmcp_recovery_manager.erl` | Recovery orchestration | ~300 | KEEP (resilience) |
| `erlmcp_lifecycle_manager.erl` | Application lifecycle | ~200 | KEEP (deployment) |
| `erlmcp_hot_reload.erl` | Hot code reload | ~220 | REVIEW (experimental?) |
| `erlmcp_zero_downtime_upgrade.erl` | Zero-downtime upgrade | ~300 | REVIEW (experimental?) |
| `erlmcp_upgrade.erl` | Upgrade utilities | ~150 | REVIEW (experimental?) |

**v2 Action**: Classify each module as CANONICAL (keep), EXPERIMENTAL (review/delete), or MIGRATE (to library).

---

### CANONICAL: Monitoring & Metrics (Consolidate)

**Modules**: 18 modules

| Module | Purpose | LOC | v2 Action |
|--------|---------|-----|-----------|
| `erlmcp_metrics.erl` | Core metrics collection | ~400 | KEEP (canonical) |
| `erlmcp_metrics_server.erl` | Metrics gen_server | ~300 | KEEP (canonical) |
| `erlmcp_metrics_http.erl` | Prometheus HTTP endpoint | ~250 | KEEP (observability) |
| `erlmcp_metrics_http_handler.erl` | HTTP handler | ~150 | KEEP (observability) |
| `erlmcp_metrics_http_sup.erl` | HTTP supervisor | ~80 | KEEP |
| `erlmcp_metrics_http_worker.erl` | HTTP worker | ~120 | KEEP |
| `erlmcp_simple_metrics.erl` | Simple metrics | ~150 | MERGE into metrics.erl |
| `erlmcp_health.erl` | Health check API | ~200 | KEEP (canonical) |
| `erlmcp_health_monitor.erl` | Health monitoring | ~250 | KEEP (canonical) |
| `erlmcp_monitor_config.erl` | Monitor configuration | ~120 | KEEP |
| `erlmcp_monitor_dashboard.erl` | Dashboard backend | ~300 | REVIEW (vs TCPS dashboard) |
| `erlmcp_monitor_sup.erl` | Monitor supervisor | ~100 | KEEP |
| `erlmcp_simple_monitor.erl` | Simple monitor | ~120 | MERGE into health_monitor |
| `erlmcp_otel.erl` | OTEL integration | ~350 | KEEP (observability) |
| `erlmcp_tracing.erl` | Distributed tracing | ~280 | KEEP (observability) |
| `erlmcp_trace_propagation.erl` | Trace context propagation | ~150 | KEEP (observability) |
| `erlmcp_simple_trace.erl` | Simple tracing | ~100 | MERGE into tracing.erl |
| `erlmcp_structured_logging.erl` | Structured logging | ~180 | KEEP (observability) |

**v2 Action**: Consolidate 3 pairs (simple_metrics → metrics, simple_monitor → health_monitor, simple_trace → tracing). Keep 15 canonical modules.

---

### LEGACY: Experimental & Deprecated (Delete or Isolate)

**Modules**: 37 modules

**Categories**:
1. **Experimental Features** (12 modules):
   - `erlmcp_complex_routing.erl`, `erlmcp_router.erl` (use registry instead)
   - `erlmcp_coordination.erl` (unclear purpose)
   - `erlmcp_sampling*.erl` (2 modules, experimental LLM sampling)
   - `erlmcp_elicitation.erl` (experimental)
   - `erlmcp_audio.erl` (experimental audio support, not in MCP spec)
   - `erlmcp_icon_*.erl` (2 modules, icon caching - niche feature)
   - `erlmcp_message_size.erl` (single-purpose utility)
   - `erlmcp_pagination.erl` (not widely used)
   - `erlmcp_binding.erl`, `erlmcp_localhost_binding.erl` (niche)

2. **Legacy CLI/Tools** (8 modules):
   - `erlmcp_cli_*.erl` (5 modules: bench, chaos, doctor, marketplace, upgrade)
   - `erlmcp_marketplace_copy.erl` (legacy marketplace)
   - `erlmcp_receipt_cli.erl` (moved to TCPS)
   - `erlmcp_plan_cli.erl` (moved to TCPS)

3. **Legacy Dashboard** (5 modules):
   - `erlmcp_dashboard_*.erl` (2 modules, replaced by TCPS dashboard)
   - `erlmcp_regression_dashboard.erl` (experimental)
   - `erlmcp_sla_dashboard_handler.erl` (moved to TCPS)
   - `erlmcp_sla_http_handler.erl` (moved to TCPS)

4. **Legacy Profiling** (4 modules):
   - `erlmcp_cpu_profiler.erl`, `erlmcp_latency_profiler.erl` (use recon instead)
   - `erlmcp_profiling_suite.erl` (experimental)
   - `erlmcp_performance_benchmark.erl` (use bench/ instead)

5. **Legacy Planning** (8 modules):
   - `erlmcp_plan*.erl` (6 modules: plan, loader, sla_monitor, docs_generator, etc.)
   - All moved to TCPS or deprecated

**v2 Action**: DELETE all 37 modules or move to `experimental/` directory with clear warnings.

---

## Callgraph Clusters

### Cluster 1: Core Protocol (Tight Coupling)

```
erlmcp_server.erl
  ├─→ erlmcp_json_rpc.erl
  ├─→ erlmcp_capabilities.erl
  ├─→ erlmcp_server_handlers.erl
  ├─→ erlmcp_validation.erl (jesse)
  ├─→ erlmcp_templates.erl
  ├─→ erlmcp_subscription_handlers.erl
  ├─→ erlmcp_resource_list_changed.erl
  ├─→ erlmcp_tool_change_notifier.erl
  └─→ erlmcp_registry.erl

erlmcp_client.erl
  ├─→ erlmcp_json_rpc.erl
  ├─→ erlmcp_capabilities.erl
  └─→ erlmcp_registry.erl
```

**Cohesion**: HIGH (all modules essential to MCP protocol)
**Coupling**: MEDIUM (via registry for message routing)

---

### Cluster 2: Transports (Moderate Coupling)

```
erlmcp_transport_tcp.erl (ranch)
  ├─→ ranch (external)
  └─→ erlmcp_registry.erl

erlmcp_transport_http.erl (gun)
  ├─→ gun (external)
  └─→ erlmcp_registry.erl

erlmcp_transport_ws.erl (cowboy)
  ├─→ cowboy (external)
  └─→ erlmcp_registry.erl

erlmcp_transport_stdio.erl
  ├─→ stdlib (io)
  └─→ erlmcp_registry.erl
```

**Cohesion**: MEDIUM (each transport independent)
**Coupling**: LOW (only depends on registry for routing)

---

### Cluster 3: Registry & Supervision (Low Coupling)

```
erlmcp_sup.erl
  ├─→ erlmcp_registry_sup.erl
  ├─→ erlmcp_infrastructure_sup.erl
  ├─→ erlmcp_server_sup.erl
  ├─→ erlmcp_transport_sup.erl
  └─→ erlmcp_monitoring_sup.erl

erlmcp_registry.erl
  └─→ gproc (v2 migration)
```

**Cohesion**: HIGH (supervision tree)
**Coupling**: LOW (supervisors are independent)

---

### Cluster 4: TCPS (Isolated)

```
tcps_* (85+ modules)
  └─→ NO dependencies on erlmcp core protocol
  └─→ OPTIONAL dependency on erlmcp for MCP integration
```

**Cohesion**: HIGH (manufacturing workflow)
**Coupling**: ZERO (can be separated cleanly)

---

## Dependency Analysis

### External Dependencies (Production)

| Library | Version | Usage | Modules |
|---------|---------|-------|---------|
| **jsx** | 3.1.0 | JSON encode/decode | json_rpc, all transports |
| **jesse** | 1.8.0 | JSON Schema validation | validation, server |
| **gproc** | 0.9.0 | Process registry (v2 target) | registry (future) |
| **gun** | 2.0.1 | HTTP/2 client | transport_http* |
| **ranch** | 2.1.0 | TCP server | transport_tcp |
| **cowboy** | 2.10.0 | HTTP server, WebSocket | transport_http_server, transport_ws |
| **poolboy** | 1.5.2 | Connection pooling | connection_pool (future) |

**Source**: `rebar.config`

---

### Internal Dependency Graph

**Core → Transports → Registry → Supervision**

```
[Supervision Layer]
  erlmcp_sup.erl

[Registry Layer]
  erlmcp_registry.erl
  erlmcp_registry_sup.erl

[Transport Layer]
  erlmcp_transport_*.erl (12 modules)

[Protocol Layer]
  erlmcp_server.erl
  erlmcp_client.erl
  erlmcp_json_rpc.erl
  erlmcp_capabilities.erl
  erlmcp_validation.erl
  erlmcp_templates.erl
  erlmcp_*_handlers.erl (3 modules)

[Infrastructure Layer]
  erlmcp_session_manager.erl
  erlmcp_task_manager.erl
  erlmcp_subscription_handlers.erl

[Observability Layer]
  erlmcp_metrics*.erl (7 modules)
  erlmcp_health*.erl (2 modules)
  erlmcp_otel.erl
  erlmcp_tracing.erl
```

---

## LOC Summary

| Category | Modules | LOC | % of Total |
|----------|---------|-----|------------|
| **Core Protocol** | 15 | ~3,040 | 8% |
| **Transports** | 12 | ~3,180 | 8% |
| **Registry/Supervision** | 8 | ~1,511 | 4% |
| **Benchmarks (canonical)** | 5 | ~2,000 | 5% |
| **Infrastructure (canonical)** | 15 | ~3,500 | 9% |
| **Monitoring (canonical)** | 15 | ~3,000 | 8% |
| **TCPS** | 85+ | ~15,000 | 40% |
| **Legacy/Experimental** | 37 | ~6,000 | 16% |

**Total**: ~37,231 LOC (estimated)

**v2 Target**: ~15,000 LOC (remove TCPS + legacy, migrate to libraries)
- **Savings**: ~22,000 LOC (59% reduction)

---

## v2 Refactoring Priorities

### Phase 1: Separate TCPS (Critical)

**Action**: Move 85+ TCPS modules to `apps/tcps_erlmcp/`
**Benefit**: Reduce core codebase by 40%, clarify focus on MCP protocol
**Effort**: 2-3 days (rebar3 app structure, update dependencies)

---

### Phase 2: Delete Legacy (High Impact)

**Action**: Remove 37 experimental/deprecated modules
**Benefit**: Reduce codebase by 16%, eliminate tech debt
**Effort**: 1 day (verify no critical dependencies, update docs)

---

### Phase 3: Library Migration (Moderate Impact)

**Action**: Migrate registry to gproc, connection pool to poolboy
**Benefit**: Save ~700 LOC, improve reliability
**Effort**: 3-5 days (careful migration, testing)

---

### Phase 4: Consolidate Observability (Low Impact)

**Action**: Merge simple_* modules into canonical counterparts
**Benefit**: Reduce 18 → 15 modules, cleaner API
**Effort**: 1-2 days

---

## References

### Source Code
- **Module List**: `find src -name "*.erl"`
- **LOC Analysis**: `cloc src/`
- **Dependencies**: `rebar.config`

### Documentation
- **Glossary**: [v2 Glossary](../GLOSSARY.md)
- **C4 Diagrams**: [L1 Context](./L1-context.md), [L2 Containers](./L2-containers.md), [L3 Core](./L3-components-core.md)
- **v2 Design Inputs**: [V2 Principles](../V2_DESIGN_INPUTS/v2_principles.md)

---

**Document Status**: CANONICAL (v2.0.0-draft)
**Last Updated**: 2026-01-27
**Module Count**: 214 (verified 2026-01-27)
