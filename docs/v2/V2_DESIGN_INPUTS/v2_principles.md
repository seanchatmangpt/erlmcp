# erlmcp v2.0.0 Design Principles

**What to Keep vs. Delete - Architectural Philosophy**

Version: v2.0.0-draft
Status: CANONICAL REFERENCE
Date: 2026-01-27

---

## Purpose

This document defines the architectural principles guiding the v2.0.0 redesign of erlmcp. It answers the fundamental question: **"What should erlmcp be in v2?"**

**Target Audience**: Architects, technical leads, refactoring teams.

---

## Core Mission (Unchanged from v1)

**erlmcp is an Erlang/OTP SDK for the Model Context Protocol (MCP).**

**Responsibilities**:
1. ✅ Implement MCP protocol (client + server roles)
2. ✅ Support multiple transports (stdio, TCP, HTTP, WebSocket)
3. ✅ Provide OTP supervision patterns (fault tolerance)
4. ✅ Deliver production-grade performance (40K+ msg/s sustained)
5. ✅ Integrate with Erlang ecosystem (gproc, gun, ranch, poolboy)

**Non-Responsibilities** (v2 Changes):
1. ❌ Manufacturing workflow systems (→ TCPS moved to separate package)
2. ❌ Experimental features (→ Delete or isolate)
3. ❌ Reinventing production libraries (→ Migrate to gproc, gun, ranch, poolboy)

---

## v2 Guiding Principles

### 1. Focus on MCP Protocol (The 80/20 Rule)

**Principle**: erlmcp v2 is a **MCP SDK**, not a general-purpose application framework.

**What This Means**:
- ✅ **KEEP**: Core protocol (JSON-RPC, capabilities, resources, tools, prompts)
- ✅ **KEEP**: Transports (stdio, TCP, HTTP, WebSocket)
- ✅ **KEEP**: OTP supervision (bulkhead pattern, fault isolation)
- ❌ **DELETE**: Manufacturing workflows (TCPS → separate package)
- ❌ **DELETE**: Experimental features not in MCP spec (audio, icon caching, complex routing)

**Impact**:
- **Before v2**: 247 modules, 40% TCPS, 17% experimental
- **After v2**: 60-70 modules, 100% MCP-focused
- **Benefit**: Clearer value proposition, easier onboarding, faster development

**Rationale**: Users adopt erlmcp for MCP protocol, not for manufacturing workflows. TCPS is valuable but orthogonal.

---

### 2. Library Integration Over Reinvention

**Principle**: Use production-grade Erlang libraries instead of custom implementations.

**What This Means**:

| Component | v1 Custom | v2 Library | Benefit |
|-----------|-----------|------------|---------|
| **Registry** | `erlmcp_registry.erl` (400 LOC) | `gproc` | Distributed, automatic monitoring, property storage |
| **HTTP Client** | Custom HTTP (281 LOC) | `gun` | HTTP/2 multiplexing, built-in retry, connection reuse |
| **TCP Server** | Custom TCP (199 LOC) | `ranch` | Accept pool, supervisor integration, connection limits |
| **Connection Pool** | `erlmcp_connection_pool.erl` (300 LOC) | `poolboy` | Battle-tested, queue management, backpressure |

**Total Savings**: ~980 LOC (custom code) → 0 LOC (library usage)

**Rationale**: Production libraries are:
- ✅ More reliable (used by EMQX, RabbitMQ, Cowboy)
- ✅ Better tested (thousands of deployments)
- ✅ Better documented (community support)
- ✅ Maintained externally (security patches, performance improvements)

**Source**: `docs/library-migration-guide.md`

---

### 3. Separate Concerns (TCPS Independence)

**Principle**: TCPS (Toyota Code Production System) should be a **separate OTP application**, not embedded in erlmcp core.

**Architecture**:

```
Before v2:
erlmcp (monolith)
├── MCP protocol (60%)
├── TCPS (40%, 85+ modules)
└── Tightly coupled

After v2:
erlmcp (MCP SDK)
  └── MCP protocol (100%)

tcps_erlmcp (separate app)
  ├── Manufacturing workflows
  └── OPTIONAL dependency on erlmcp for MCP integration
```

**Benefits**:
1. ✅ **Clarity**: erlmcp is MCP SDK, tcps_erlmcp is manufacturing system
2. ✅ **Reusability**: TCPS can be used with non-MCP projects
3. ✅ **Testability**: Each package has isolated test suites
4. ✅ **Deployment**: Deploy erlmcp without TCPS (lighter containers)
5. ✅ **Maintenance**: Independent release cycles

**Migration Path**:
1. Create `apps/tcps_erlmcp/` OTP application
2. Move 85+ `tcps_*.erl` modules
3. Update rebar.config: `{applications, [erlmcp]}` (optional dep)
4. Publish `tcps_erlmcp` as separate hex package

**Rationale**: TCPS is a complete system (CLI, dashboard, receipt chain, simulator). It deserves its own identity.

---

### 4. Delete Legacy Without Remorse

**Principle**: If a module is experimental, unused, or superseded, **delete it in v2**.

**What This Means**:
- ❌ **DELETE**: 37 experimental/deprecated modules (see [v2_deletions.md](./v2_deletions.md))
- ❌ **DELETE**: 9 legacy benchmarks (replaced by 5 canonical benchmarks)
- ❌ **DELETE**: Duplicate "simple" modules (simple_metrics, simple_monitor, simple_trace)

**Process**:
1. Identify module as legacy (not in canonical architecture)
2. Check for dependencies: `grep -r "module_name" src/`
3. If no critical dependencies → DELETE
4. If uncertain → Move to `src/experimental/` with `@deprecated` tag

**Rationale**: Tech debt accumulates. v2 is the opportunity to clean slate.

---

### 5. Performance is a Feature

**Principle**: erlmcp v2 must maintain or exceed v1.5.0 performance baselines.

**Performance Targets** (per node):

| Metric | Target | Workload | Baseline (v1.5.0) |
|--------|--------|----------|-------------------|
| **Throughput** | ≥40K msg/s sustained | `tcp_sustained_10k_1kib` | 43K msg/s |
| **Latency p99** | <50ms | `tcp_sustained_10k_1kib` | 28.4ms |
| **Concurrent Connections** | 40-50K (honest capacity) | `tcp_sustained_50k_1kib` | 40K validated |
| **Memory per Connection** | <0.06 MiB | `core_ops_100k` | 0.048 MiB heap + 0.012 MiB state |
| **Failover SLA** | <2s | `ha_failover_test` | <2s validated |

**Non-Negotiable**:
- ✅ Benchmark suite MUST pass before v2 release
- ✅ No regressions >10% on canonical workloads
- ✅ Memory footprint must not increase

**Rationale**: Performance is why users choose Erlang/OTP. Cannot regress in v2.

---

### 6. OTP Patterns are Sacred

**Principle**: v2 must follow OTP best practices (behaviors, supervision, let-it-crash).

**What This Means**:
- ✅ **KEEP**: Bulkhead supervision pattern (`erlmcp_sup.erl` with rest_for_one)
- ✅ **KEEP**: gen_server for stateful components (client, server, registry)
- ✅ **KEEP**: simple_one_for_one for dynamic workers (transports, servers)
- ✅ **KEEP**: Process-per-connection model (scalability via Erlang processes)
- ❌ **DELETE**: Non-OTP patterns (unsupervised spawns, manual monitors where gproc handles it)

**OTP Supervision Tree** (canonical):
```
erlmcp_sup (rest_for_one)
├─ [Tier 1] Registry (isolated, gproc-based)
├─ [Tier 2] Infrastructure (session manager, task manager)
├─ [Tier 3] Protocol Servers (simple_one_for_one)
├─ [Tier 4] Transports (simple_one_for_one)
└─ [Tier 5] Observability (isolated, does not affect protocol)
```

**Source**: `src/erlmcp_sup.erl:L111-L211`, `docs/otp-patterns.md`

---

### 7. Test Coverage is Mandatory

**Principle**: v2 must maintain ≥80% test coverage with comprehensive integration tests.

**Test Strategy**:

| Test Type | Framework | Coverage Target | Status |
|-----------|-----------|-----------------|--------|
| **Unit Tests** | EUnit | ≥80% | Existing (validate) |
| **Integration Tests** | Common Test | Key workflows | Existing (validate) |
| **Benchmarks** | Custom + CT | Performance baselines | Canonical (5 benchmarks) |
| **Property Tests** | PropEr | Critical algorithms | Partial (expand) |

**Required Tests** (non-negotiable):
1. ✅ MCP protocol compliance (all methods in spec)
2. ✅ Transport integration (stdio, TCP, HTTP, WebSocket)
3. ✅ Supervision restart scenarios (tier 1-5 failures)
4. ✅ Performance benchmarks (no regressions)
5. ✅ Library integration (gproc, gun, ranch, poolboy)

**Source**: `test/*.erl`, `bench/*.erl`

---

### 8. Documentation is First-Class

**Principle**: v2 architecture is documented BEFORE code refactoring begins.

**Documentation Deliverables**:

| Document | Purpose | Status |
|----------|---------|--------|
| [GLOSSARY.md](../GLOSSARY.md) | Canonical terms, metrics | COMPLETE |
| [C4 L1-L4 Diagrams](../C4/) | Architecture layers | COMPLETE (4 diagrams, 1 pending) |
| [v2_principles.md](./v2_principles.md) | Design philosophy | COMPLETE (this doc) |
| [v2_required_modules.md](./v2_required_modules.md) | Minimum kernel | COMPLETE |
| [v2_deletions.md](./v2_deletions.md) | Deletion list | COMPLETE |
| [v2_risks.md](./v2_risks.md) | Identified risks | COMPLETE |

**Benefits**:
- ✅ **Alignment**: All stakeholders agree on v2 scope
- ✅ **Efficiency**: Refactoring follows documented plan
- ✅ **Quality**: Design flaws caught in review, not in code
- ✅ **Onboarding**: New contributors understand v2 vision

**Rationale**: Architecture-first prevents costly rework.

---

## What to Keep (60-70 Canonical Modules)

### Category 1: Core Protocol (15 modules)
- `erlmcp_client.erl`, `erlmcp_server.erl`, `erlmcp_json_rpc.erl`
- `erlmcp_capabilities.erl`, `erlmcp_templates.erl`, `erlmcp_validation.erl`
- Resource/tool/prompt handlers (5 modules)

**Rationale**: Essential MCP protocol implementation.

---

### Category 2: Transports (9 modules, after consolidation)
- `erlmcp_transport_{stdio,tcp,http,ws}.erl`
- Transport API and validation (3 modules)

**Rationale**: Multi-transport support is core value proposition.

**v2 Consolidation**:
- Merge `erlmcp_transport_http*.erl` (3 modules → 1 gun-based module)
- Delete experimental transports (SSE → use HTTP long-polling)

---

### Category 3: Registry & Supervision (6 modules, after gproc migration)
- `erlmcp_sup.erl`, `erlmcp_registry_sup.erl`, `erlmcp_registry.erl` (migrated to gproc)
- `erlmcp_server_sup.erl`, `erlmcp_transport_sup.erl`, `erlmcp_infrastructure_sup.erl`

**Rationale**: OTP supervision is architectural foundation.

**v2 Savings**: Delete `erlmcp_registry_sharded.erl` (experimental), simplify registry to gproc wrapper.

---

### Category 4: Infrastructure (15 modules, reviewed)
- Session management (3 modules: manager, replicator, failover)
- Task management (1 module)
- Backpressure (2 modules)
- Circuit breaker, graceful degradation (3 modules)
- Memory/resource management (3 modules)
- Deployment (3 modules: lifecycle, drain, upgrade)

**Rationale**: Production-grade features users expect.

---

### Category 5: Observability (15 modules, after consolidation)
- Metrics (4 modules: core, server, HTTP endpoint, validator)
- Health checks (2 modules)
- OTEL integration (3 modules: otel, tracing, propagation)
- Logging (2 modules: structured, helpers)
- Monitoring (2 modules: health monitor, config)
- Profiling (2 modules: memory, accounting)

**Rationale**: Observability is critical for production deployments.

**v2 Consolidation**: Merge simple_* modules (3 pairs → 3 canonical modules).

---

### Category 6: Benchmarks (5 modules, canonical)
- `erlmcp_bench_core_ops.erl`, `erlmcp_bench_network_real.erl`, `erlmcp_bench_stress.erl`
- `erlmcp_bench_chaos.erl`, `erlmcp_bench_integration.erl`

**Rationale**: Performance validation is mandatory.

**v2 Cleanup**: Delete 9 legacy benchmarks, keep 5 canonical benchmarks.

---

## What to Delete (122+ Modules)

### Category 1: TCPS (85+ modules → SEPARATE PACKAGE)
- All `tcps_*.erl` modules
- Move to `apps/tcps_erlmcp/` OTP application
- Publish as separate hex package

**Rationale**: TCPS is a complete system orthogonal to MCP protocol.

---

### Category 2: Legacy/Experimental (37 modules → DELETE)
- Experimental features (12 modules): complex routing, audio, icon caching, sampling, elicitation
- Legacy CLI (8 modules): marketplace, receipt CLI, plan CLI
- Legacy dashboard (5 modules): regression dashboard, SLA dashboards
- Legacy profiling (4 modules): CPU profiler, latency profiler
- Legacy planning (8 modules): plan loader, SLA monitor

**Rationale**: Tech debt cleanup, no critical dependencies.

**Source**: [v2_deletions.md](./v2_deletions.md)

---

## What to Migrate (Libraries)

### Migration 1: gproc Registry
**Before**: Custom `erlmcp_registry.erl` (400 LOC)
**After**: gproc wrapper (50 LOC)
**Benefit**: Distributed registry, automatic monitoring, property storage

---

### Migration 2: gun HTTP Client
**Before**: Custom HTTP (281 LOC)
**After**: gun integration (existing, enhance)
**Benefit**: HTTP/2 multiplexing, built-in retry, connection reuse

---

### Migration 3: ranch TCP Server
**Before**: Custom TCP (199 LOC)
**After**: ranch protocol (existing, document)
**Benefit**: Accept pool, supervisor integration, connection limits

---

### Migration 4: poolboy Connection Pooling
**Before**: Custom `erlmcp_connection_pool.erl` (300 LOC)
**After**: poolboy integration (80 LOC wrapper)
**Benefit**: Battle-tested, queue management, backpressure

**Total Savings**: ~1,180 LOC

---

## v2 Success Criteria

### Technical Criteria

1. ✅ **Module Count**: 60-70 modules (from 247)
2. ✅ **LOC**: ~15,000 LOC (from ~37,000)
3. ✅ **Test Coverage**: ≥80% (maintain)
4. ✅ **Performance**: No regressions >10% on canonical workloads
5. ✅ **OTP Compliance**: All modules follow OTP patterns
6. ✅ **Library Integration**: gproc, gun, ranch, poolboy fully integrated
7. ✅ **TCPS Separation**: `tcps_erlmcp` published as separate package

---

### User-Facing Criteria

1. ✅ **Clearer Value**: "erlmcp is a MCP SDK" (not "MCP + manufacturing")
2. ✅ **Easier Onboarding**: 60-70 modules vs. 247 modules
3. ✅ **Better Docs**: C4 diagrams, glossary, design inputs
4. ✅ **Same API**: Core API unchanged (add_resource, add_tool, etc.)
5. ✅ **Better Performance**: Same or better throughput/latency
6. ✅ **TCPS Available**: Optional `tcps_erlmcp` dependency

---

## References

### Source Documents
- **Module Inventory**: [L4-code-map.md](../C4/L4-code-map.md)
- **Glossary**: [GLOSSARY.md](../GLOSSARY.md)
- **Deletions**: [v2_deletions.md](./v2_deletions.md)
- **Required Modules**: [v2_required_modules.md](./v2_required_modules.md)
- **Risks**: [v2_risks.md](./v2_risks.md)

### Source Code
- **Supervision**: `src/erlmcp_sup.erl:L111-L211`
- **Registry**: `src/erlmcp_registry.erl`
- **Library Migration**: `docs/library-migration-guide.md`
- **OTP Patterns**: `docs/otp-patterns.md`

---

**Document Status**: CANONICAL (v2.0.0-draft)
**Last Updated**: 2026-01-27
**Purpose**: Define v2 architectural philosophy (what to keep vs. delete)
