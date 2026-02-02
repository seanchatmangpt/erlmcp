# MCP Master Implementation Plan - erlmcp v2.1.0 → v3.0.0

**Document Version:** 1.0.0
**Created:** 2026-02-01
**Status:** AUTHORITATIVE PLAN
**Target:** 100% MCP Specification 2025-11-25 Compliance

---

## Executive Summary

This master plan consolidates findings from 5 specialized agent analyses to provide the definitive roadmap for achieving full MCP 2025-11-25 specification compliance in erlmcp. The current baseline (v2.1.0) provides **65% compliance** with excellent performance characteristics but requires strategic enhancements across 4 major phases.

**Current State:**
- **Version:** erlmcp v2.1.0
- **Modules:** 164 (Core: 97, Transports: 23, Observability: 31, Validation: 13)
- **Performance:** 553K msg/s registry, P50=5ms, P95=20ms, 40-50K connections/node
- **Compliance:** 65% (MCP 2025-11-25)
- **Test Coverage:** ~75% overall, ~85% core modules

**Target State (v3.0.0):**
- **Compliance:** 95%+ MCP 2025-11-25
- **Performance:** P50=2ms, P95=8ms, 100K req/s throughput
- **Test Coverage:** 85%+ overall, 90%+ core modules
- **Timeline:** 30-38 weeks (7-9 months)
- **Effort:** 1,712 hours across 4 phases

---

## I. CONSOLIDATION ANALYSIS

### A. Cross-Reference Validation

#### 1. Architecture Recommendations (Consistent ✅)

All agent analyses align on:
- **OTP Supervision:** 3-tier one_for_one supervision tree maintained
- **Process Model:** Process-per-connection isolation preserved
- **Chicago School TDD:** Real processes, no mocks, state-based verification
- **Module Organization:** apps/{core,transports,observability,validation}
- **Registry:** gproc-based O(log N) routing

**Validation:** No conflicts detected

---

#### 2. Performance Targets Alignment (Validated ✅)

| Metric | Baseline (v2.1.0) | Phase 1 Target (v2.2.0) | Final Target (v3.0.0) | Source Documents |
|--------|-------------------|-------------------------|------------------------|------------------|
| **P50 Latency** | 5ms | 2-3ms | 2ms | Performance Analysis, Implementation Phasing |
| **P95 Latency** | 20ms | 8-12ms | 8ms | Performance Analysis, Test Strategy |
| **P99 Latency** | 50ms | 20ms | 20ms | Performance Analysis |
| **Throughput** | 10K req/s | 10-50K req/s | 100K req/s | Implementation Phasing |
| **Registry** | 553K msg/s | 553K+ msg/s | 600K+ msg/s | Performance Analysis |
| **Connections** | 40-50K/node | 50K/node | 200K/node | Implementation Phasing |
| **SONA Latency** | N/A | N/A | <0.05ms (hybrid) | Performance Analysis, Implementation Phasing |

**Critical Finding:** SONA <0.05ms requirement cannot be met by erlmcp alone. Hybrid architecture with Rust FFI required (Phase 4).

**Validation:** All documents agree on performance progression. No conflicts.

---

#### 3. Testing Strategy Coverage (Comprehensive ✅)

| Test Category | Current | Target | Gap | Responsible Agent |
|--------------|---------|--------|-----|-------------------|
| **EUnit Tests** | 269 | 350+ | +81 | erlang-test-engineer |
| **CT Suites** | 33 | 60+ | +27 | erlang-test-engineer |
| **Proper Tests** | ~10 | 60+ | +50 | erlang-test-engineer |
| **Compliance Tests** | ~5 | 30+ | +25 | erlang-test-engineer |
| **Benchmarks** | ~15 | 30+ | +15 | erlang-performance |
| **Chaos Tests** | ~5 | 20+ | +15 | erlang-test-engineer |
| **Coverage** | 75% overall | 85% overall | +10% | agent-11-coverage |
| **Core Coverage** | 85% | 90%+ | +5% | agent-11-coverage |

**Critical Gaps:**
- **Sampling Tests:** 0% → 100% (NEW feature in MCP 2025-11-25)
- **Tasks API Tests:** 0% → 100% (NEW experimental feature)
- **Elicitation Tests:** 10% → 100% (NEW experimental feature)
- **Compliance Tests:** 5% → 100% (Required for certification)

**Validation:** Test Strategy document provides detailed roadmap. Gap Analysis identifies missing tests. Aligned.

---

#### 4. Implementation Phasing Dependencies (Resolved)

**Apparent Conflict:**
- **Gap Analysis:** 3 phases, 4-6 weeks (compliance only)
- **Implementation Phasing:** 4 phases, 30-38 weeks (comprehensive)

**Resolution:**
- Gap Analysis focuses on **minimum compliance** (65% → 80%)
- Implementation Phasing targets **full feature set** (65% → 95%+)
- **Adopted Approach:** Use Implementation Phasing 4-phase model with Gap Analysis priorities

**Validated Phasing:**

```
Phase 0: Gap Analysis (✅ COMPLETE)
   ↓
Phase 1: Core Improvements (4-6 weeks)
   ├─ Schema caching (75% latency reduction)
   ├─ jiffy migration (60% JSON overhead reduction)
   ├─ Async tool execution (10x throughput)
   └─ v2.2.0 Release
   ↓
Phase 2: Missing Features (6-8 weeks)
   ├─ Sampling (streaming support)
   ├─ Tasks API (experimental)
   ├─ Completion (experimental)
   ├─ Elicitation (experimental)
   └─ v2.3.0 Release
   ↓
Phase 3: Optimization (8-10 weeks)
   ├─ RuVector intelligence
   ├─ Swarm topology
   ├─ Distributed sessions (Mnesia)
   └─ v2.4.0 Release
   ↓
Phase 4: Advanced Features (10-12 weeks)
   ├─ claude-flow integration
   ├─ SONA routing (Rust FFI)
   ├─ Kubernetes operator
   └─ v3.0.0 Release
```

**Validation:** Dependency graph verified. No circular dependencies. Each phase builds on previous.

---

#### 5. Risk Assessments (Aligned ✅)

**High-Risk Items (All Documents Agree):**

| Risk | Probability | Impact | Mitigation | Source Documents |
|------|-------------|--------|------------|------------------|
| **Rust NIFs Crash** | Medium | Critical | Crash isolation, fallback, extensive testing | All 3 documents |
| **SONA Latency Miss** | Medium | High | Hybrid architecture, profiling, caching layers | Performance, Implementation |
| **OAuth Compliance** | Low | High | Security audit, compliance testing | Gap Analysis, Implementation |
| **Schema Validation** | Low | High | ETS caching, jesse optimization | Performance, Implementation |
| **Performance Regression** | Medium | High | Continuous benchmarking, feature flags | All 3 documents |

**Medium-Risk Items:**
- Distributed coordination (Raft split-brain)
- Mnesia corruption
- Process pooling complexity
- Sampling provider failures

**Validation:** All documents identify same high-risk areas. Mitigation strategies consistent.

---

#### 6. Integration Assumptions (Validated ✅)

**claude-flow Integration:**
- **Transport:** STDIO (all documents agree)
- **Hot Path:** Shared memory (mmap) for SONA <0.05ms
- **Fallback:** erlmcp JSON-RPC for uncached
- **Architecture:** Dual-path (Performance Analysis, Implementation Phasing)

**Existing Infrastructure:**
- **Registry:** gproc O(log N) routing (all documents)
- **Transports:** STDIO, TCP, HTTP, WebSocket, SSE (all documents)
- **Supervision:** 3-tier one_for_one (all documents)
- **Session:** ETS/DETS/Mnesia backends (all documents)

**Validation:** No conflicts. All assumptions verified against existing codebase.

---

## II. MASTER SPECIFICATION COMPLIANCE MATRIX

### Current vs Target Compliance

| Feature | Spec Version | Current Status | Phase | Target Version | Priority |
|---------|-------------|----------------|-------|----------------|----------|
| **CORE PROTOCOL** |
| JSON-RPC 2.0 | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| Protocol Version Negotiation | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| Capability Negotiation | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| Error Codes | 2025-11-25 | ⚠️ 90% (input validation fix needed) | 1 | v2.2.0 | P2 |
| **RESOURCES** |
| Static Resources | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| Resource Templates | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| Resource Subscriptions | 2025-11-25 | ⚠️ 85% (optimization needed) | 1 | v2.2.0 | P1 |
| Resource List Changed | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| **TOOLS** |
| Tool Registration | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| Tool Call (no schema) | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| Tool Call (with schema) | 2025-11-25 | ⚠️ 80% (performance issue) | 1 | v2.2.0 | P0 |
| Schema Validation | 2025-11-25 | ⚠️ 75% (caching needed) | 1 | v2.2.0 | P0 |
| Tool List Changed | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| **PROMPTS** |
| Prompt List/Get | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| Prompt Arguments | 2025-11-25 | ⚠️ 90% (verification needed) | 2 | v2.3.0 | P2 |
| Prompt Templates | 2025-11-25 | ⚠️ 90% (verification needed) | 2 | v2.3.0 | P2 |
| Prompt List Changed | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| **SAMPLING (LLM)** |
| Create Message | 2025-11-25 | ⚠️ 40% (basic only) | 2 | v2.3.0 | P1 |
| Streaming Support | 2025-11-25 | ❌ 0% | 2 | v2.3.0 | P1 |
| Model Preferences | 2025-11-25 | ❌ 0% | 2 | v2.3.0 | P1 |
| System Prompt | 2025-11-25 | ❌ 0% | 2 | v2.3.0 | P1 |
| Temperature/Max Tokens | 2025-11-25 | ❌ 0% | 2 | v2.3.0 | P1 |
| **LOGGING** |
| Log Levels | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| setLevel Notification | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| **COMPLETION** |
| Argument Completion | 2025-11-25 | ⚠️ 60% (basic only) | 2 | v2.3.0 | P1 |
| Resource URI Completion | 2025-11-25 | ⚠️ 60% | 2 | v2.3.0 | P1 |
| Ref Completion | 2025-11-25 | ⚠️ 30% | 2 | v2.3.0 | P1 |
| **ROOTS** |
| Root List | 2025-11-25 | ⚠️ 60% | 2 | v2.3.0 | P2 |
| Root URI Validation | 2025-11-25 | ❌ 0% | 2 | v2.3.0 | P2 |
| **CANCELLATION** |
| Request Cancellation | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| Progress Token Cancellation | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| **PROGRESS** |
| Progress Notifications | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| Progress Tokens | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| **EXPERIMENTAL FEATURES** |
| Tasks API | 2025-11-25 (exp) | ❌ 0% | 2 | v2.3.0 | P0 |
| Elicitation API | 2025-11-25 (exp) | ❌ 10% (error codes only) | 2 | v2.3.0 | P1 |
| **METADATA & UI** |
| Icons (Tools) | 2025-11-25 | ⚠️ 30% (cache exists) | 2 | v2.3.0 | P2 |
| Icons (Resources) | 2025-11-25 | ⚠️ 30% | 2 | v2.3.0 | P2 |
| Icons (Prompts) | 2025-11-25 | ⚠️ 30% | 2 | v2.3.0 | P2 |
| Server Description | 2025-11-25 | ❌ 0% | 2 | v2.3.0 | P3 |
| **SECURITY & AUTH** |
| OAuth 2.0 (basic) | 2025-11-25 | ⚠️ 40% | 1 | v2.2.0 | P0 |
| OpenID Connect Discovery | 2025-11-25 | ❌ 0% | 1 | v2.2.0 | P0 |
| Incremental Scope Consent | 2025-11-25 | ❌ 0% | 1 | v2.2.0 | P0 |
| RFC 9728 Metadata | 2025-11-25 | ❌ 0% | 1 | v2.2.0 | P0 |
| HTTP Origin Validation | 2025-11-25 | ❌ 0% | 1 | v2.2.0 | P0 |
| **TRANSPORTS** |
| STDIO | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| TCP | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| HTTP | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| WebSocket | 2025-11-25 | ✅ 100% | - | v2.1.0 | - |
| SSE | 2025-11-25 | ⚠️ 80% (polling missing) | 1 | v2.2.0 | P1 |
| SSE Polling Streams | 2025-11-25 | ❌ 0% | 1 | v2.2.0 | P1 |
| **SCHEMA & VALIDATION** |
| JSON Schema 2020-12 | 2025-11-25 | ⚠️ 90% (default dialect) | 1 | v2.2.0 | P2 |
| Input Validation Errors | 2025-11-25 | ⚠️ 80% (SEP-1303 fix) | 1 | v2.2.0 | P2 |

**Compliance Score:**
- **Current (v2.1.0):** 65% (42/65 features at ≥80%)
- **Phase 1 Target (v2.2.0):** 75% (+10%)
- **Phase 2 Target (v2.3.0):** 90% (+15%)
- **Final Target (v3.0.0):** 95%+ (+5%)

---

## III. ARCHITECTURE DIAGRAMS

### A. Current Architecture (v2.1.0)

```
┌─────────────────────────────────────────────────────────────┐
│                     erlmcp_sup (one_for_one)                │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌────────────────────────────────────────────────────┐   │
│  │  TIER 1: CORE (erlmcp_core_sup - one_for_one)     │   │
│  ├────────────────────────────────────────────────────┤   │
│  │  • erlmcp_registry (gproc)                         │   │
│  │  • erlmcp_session_manager                          │   │
│  │  • erlmcp_resource_subscriptions                   │   │
│  │  • erlmcp_schema_registry                          │   │
│  │  • erlmcp_cache (L1/L2/L3)                         │   │
│  │  • erlmcp_cancellation                             │   │
│  │  • erlmcp_progress                                 │   │
│  │  • erlmcp_sampling (basic)                         │   │
│  │  • erlmcp_prompt_template                          │   │
│  │  • [11+ other core workers]                        │   │
│  └────────────────────────────────────────────────────┘   │
│                                                             │
│  ┌────────────────────────────────────────────────────┐   │
│  │  TIER 2: PROTOCOL (simple_one_for_one)            │   │
│  ├────────────────────────────────────────────────────┤   │
│  │  • erlmcp_server_sup                               │   │
│  │    └─ [Dynamic erlmcp_server instances]            │   │
│  │  • erlmcp_client_sup                               │   │
│  │    └─ [Dynamic erlmcp_client instances]            │   │
│  └────────────────────────────────────────────────────┘   │
│                                                             │
│  ┌────────────────────────────────────────────────────┐   │
│  │  TIER 3: OBSERVABILITY (one_for_one)              │   │
│  ├────────────────────────────────────────────────────┤   │
│  │  • erlmcp_otel                                     │   │
│  │  • erlmcp_metrics                                  │   │
│  │  • erlmcp_dashboard_server                         │   │
│  │  • erlmcp_health_monitor                           │   │
│  │  • erlmcp_chaos                                    │   │
│  └────────────────────────────────────────────────────┘   │
│                                                             │
└─────────────────────────────────────────────────────────────┘

TRANSPORTS (separate app)
┌─────────────────────────────────────────────────────────────┐
│  erlmcp_transport_sup                                       │
│  ├─ erlmcp_transport_stdio                                  │
│  ├─ erlmcp_transport_tcp                                    │
│  ├─ erlmcp_transport_http                                   │
│  ├─ erlmcp_transport_ws                                     │
│  └─ erlmcp_transport_sse                                    │
└─────────────────────────────────────────────────────────────┘
```

### B. Phase 1 Enhancements (v2.2.0)

```
NEW ADDITIONS TO TIER 1:

┌─────────────────────────────────────────────────────────────┐
│  erlmcp_core_sup (one_for_one)                              │
│  ├─ erlmcp_schema_cache (gen_server) ← NEW                  │
│  │   └─ ETS cache for compiled jesse schemas               │
│  ├─ erlmcp_tool_executor_sup (one_for_one) ← NEW            │
│  │   └─ poolboy pool (10-50 workers)                        │
│  ├─ erlmcp_json_codec (behavior abstraction) ← NEW          │
│  │   ├─ erlmcp_json_codec_jsx (fallback)                   │
│  │   └─ erlmcp_json_codec_jiffy (primary) ← NEW            │
│  └─ erlmcp_auth_oauth_sup (one_for_one) ← NEW               │
│      ├─ erlmcp_auth_oauth (OpenID Connect discovery)        │
│      └─ erlmcp_auth_incremental_scope                       │
└─────────────────────────────────────────────────────────────┘

PERFORMANCE IMPROVEMENTS:
• Schema validation: 5-20ms → 1-5ms (75% reduction)
• JSON encoding: 0.5-2ms → 0.2-0.7ms (60% reduction)
• Tool throughput: 1 concurrent → 10-50 concurrent (10-50x)
```

### C. Phase 2 Additions (v2.3.0)

```
NEW MODULES:

┌─────────────────────────────────────────────────────────────┐
│  erlmcp_core_sup (one_for_one)                              │
│  ├─ erlmcp_task_manager (gen_server) ← NEW                  │
│  │   └─ Async task management (experimental)               │
│  ├─ erlmcp_elicitation (gen_server) ← NEW                   │
│  │   └─ User interaction elicitation                       │
│  ├─ erlmcp_completion (gen_server) ← NEW                    │
│  │   └─ Text completion support                            │
│  ├─ erlmcp_sampling_stream_sup (simple_one_for_one) ← NEW   │
│  │   └─ [Dynamic streaming workers]                         │
│  └─ erlmcp_sse_event_store (gen_server) ← NEW               │
│      └─ SSE polling stream management                       │
└─────────────────────────────────────────────────────────────┘

NEW FEATURES:
• Tasks API (experimental): Full async workflow support
• Elicitation: User interaction with URL mode
• Completion: Argument/resource completion
• Sampling: Streaming support for all LLM providers
• SSE: Polling streams with resumption
```

### D. Phase 3 Optimizations (v2.4.0)

```
DISTRIBUTED & INTELLIGENCE LAYERS:

┌─────────────────────────────────────────────────────────────┐
│  erlmcp_ruvector_sup (one_for_one) ← NEW                    │
│  ├─ erlmcp_ruvector_index (gen_server)                      │
│  │   └─ HNSW vector index (via NIF)                         │
│  ├─ erlmcp_ruvector_embeddings (gen_server + workers)       │
│  │   └─ ONNX embedding generation                           │
│  └─ erlmcp_ruvector_router (gen_server)                     │
│      └─ Semantic request routing                            │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│  erlmcp_swarm_sup (one_for_one) ← NEW                       │
│  ├─ erlmcp_swarm_coordinator (gen_statem)                   │
│  │   └─ Raft consensus (leader/follower/candidate)          │
│  ├─ erlmcp_swarm_router (gen_server)                        │
│  │   └─ Consistent hashing for load distribution            │
│  └─ erlmcp_swarm_gossip (gen_server)                        │
│      └─ Membership protocol, failure detection              │
└─────────────────────────────────────────────────────────────┘

PERFORMANCE TARGETS:
• RuVector overhead: <3ms P95
• Swarm leader election: <2s (5-node cluster)
• Distributed sessions: <100ms replication
• Server pooling: 10x throughput per logical server
```

### E. Phase 4 Advanced (v3.0.0)

```
HYBRID ARCHITECTURE (claude-flow Integration):

┌─────────────────────────────────────────────────────────────┐
│                   claude-flow (Rust)                        │
│  ┌──────────────────────┬───────────────────────────────┐  │
│  │  SONA Router         │  MCP Client (erlmcp)          │  │
│  │  (<0.05ms)           │  (via STDIO)                  │  │
│  │                      │                               │  │
│  │  Shared Memory       │  JSON-RPC over STDIO          │  │
│  │  (mmap)              │  (1-5ms)                      │  │
│  └──────┬───────────────┴─────────┬─────────────────────┘  │
│         │                         │                        │
│         ▼                         ▼                        │
│  ┌─────────────┐         ┌──────────────┐                 │
│  │ Cached Data │         │ erlmcp_server│                 │
│  │ (mmap)      │         │ (full MCP)   │                 │
│  └─────────────┘         └──────────────┘                 │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│  erlmcp_sona_sup (one_for_one) ← NEW                        │
│  ├─ erlmcp_sona_cache (gen_server)                          │
│  │   └─ Shared memory management (Rust FFI)                 │
│  ├─ erlmcp_sona_router_nif (NIF module)                     │
│  │   └─ Rust-based routing (<0.05ms)                        │
│  └─ erlmcp_sona_monitor (gen_server)                        │
│      └─ Rust process health monitoring                      │
└─────────────────────────────────────────────────────────────┘

PERFORMANCE TARGETS:
• SONA path: <0.05ms (cached reads)
• Rust NIF overhead: <0.01ms
• Cache hit rate: >80%
• Fallback latency: 1-5ms (erlmcp MCP)
```

---

## IV. TIMELINE AND RESOURCE ESTIMATES

### A. Phase Breakdown

| Phase | Duration | Effort (Hours) | FTE | Risk | Deliverable |
|-------|----------|----------------|-----|------|-------------|
| **Phase 0** | 2 weeks | 40 | 0.25 | Low | ✅ Gap analysis (DONE) |
| **Phase 1** | 4-6 weeks | 256 | 1.0 | Low-Med | v2.2.0 (Performance) |
| **Phase 2** | 6-8 weeks | 308 | 1.0 | Medium | v2.3.0 (Features) |
| **Phase 3** | 8-10 weeks | 528 | 1.5 | Med-High | v2.4.0 (Optimization) |
| **Phase 4** | 10-12 weeks | 580 | 2.0 | High | v3.0.0 (Advanced) |
| **TOTAL** | **30-38 weeks** | **1,712** | **Avg 1.3** | **Medium** | **95%+ Compliance** |

**Calendar Time:** 7-9 months (with parallelization and overlap)

### B. Team Composition by Phase

#### Phase 1: Core Improvements (1.0 FTE)
- 1 Senior Erlang Engineer (full-time)
- 1 QA Engineer (50%)
- 1 Technical Writer (25%)

#### Phase 2: Missing Features (1.0 FTE)
- 1 Senior Erlang Engineer (full-time)
- 1 QA Engineer (50%)
- 1 Technical Writer (25%)

#### Phase 3: Optimization (1.5 FTE)
- 1 Senior Erlang Engineer (full-time)
- 1 Distributed Systems Engineer (50%)
- 1 Performance Engineer (50%)
- 1 QA Engineer (full-time)

#### Phase 4: Advanced (2.0 FTE)
- 1 Senior Erlang Engineer (full-time)
- 1 Rust Engineer (full-time)
- 1 DevOps Engineer (Kubernetes, 50%)
- 1 QA Engineer (full-time)
- 1 Technical Writer (50%)

### C. Critical Path Analysis

```
Phase 1: Schema Caching → jiffy Migration → Async Tools
   ↓
Phase 2: Tasks API → Sampling → Completion/Elicitation
   ↓           ↓
Phase 3: RuVector ← Swarm (parallel) ← Distributed Sessions
   ↓
Phase 4: Rust FFI → SONA Router → claude-flow Integration
   ↓
v3.0.0 Release
```

**Critical Dependencies:**
1. Schema caching MUST complete before async tools (data dependency)
2. Tasks API MUST complete before advanced workflows (foundation)
3. RuVector MUST complete before SONA routing (semantic layer)
4. Rust FFI infrastructure MUST complete before claude-flow (interface)

**Parallel Opportunities:**
- jiffy migration can run parallel to schema caching (different modules)
- Sampling, Completion, Elicitation can be developed in parallel (independent features)
- RuVector and Swarm can be developed in parallel (different concerns)

---

## V. RISK MANAGEMENT AND MITIGATION

### A. Critical Risks (P0)

#### Risk 1: Rust NIF Stability
- **Probability:** Medium (40%)
- **Impact:** Critical (VM crashes)
- **Mitigation:**
  - Crash isolation via monitoring process
  - Fallback to pure Erlang on NIF failure
  - Extensive stress testing (7-day chaos tests)
  - Feature flags for gradual rollout
- **Contingency:** Defer Rust NIFs to post-v3.0.0 if unstable

#### Risk 2: SONA Latency Target Unmet
- **Probability:** Medium (35%)
- **Impact:** High (performance goal miss)
- **Mitigation:**
  - Profiling at each optimization step
  - Multiple caching layers (L1/L2/L3)
  - Hybrid architecture with shared memory
  - Rust FFI for hot path
- **Contingency:** Document as "best-effort <0.1ms" if <0.05ms impossible

#### Risk 3: OAuth Compliance Gaps
- **Probability:** Low (15%)
- **Impact:** High (security vulnerability)
- **Mitigation:**
  - Security audit by third party
  - Compliance testing against real OAuth providers
  - Reference implementation comparison
  - Regular security updates
- **Contingency:** Partner with OAuth specialist for consultation

#### Risk 4: Performance Regression
- **Probability:** Medium (30%)
- **Impact:** High (user impact)
- **Mitigation:**
  - Continuous benchmarking in CI
  - Performance baselines tracked
  - Feature flags for new optimizations
  - Gradual rollout with monitoring
- **Contingency:** Rollback mechanism for each phase

### B. Medium Risks (P1)

#### Risk 5: Test Coverage Insufficient
- **Probability:** Medium (40%)
- **Impact:** Medium (quality issues)
- **Mitigation:**
  - Dedicated testing phases
  - Chicago School TDD enforcement
  - Coverage gates in CI (80%+)
  - Compliance test suite
- **Contingency:** Extend Phase 2 timeline for test completion

#### Risk 6: Distributed Coordination Complexity
- **Probability:** Medium (35%)
- **Impact:** Medium (cluster stability)
- **Mitigation:**
  - Use battle-tested Raft library
  - Network partition testing
  - Chaos engineering scenarios
  - Gradual cluster rollout
- **Contingency:** Fallback to single-node operation

#### Risk 7: Sampling Provider Integration
- **Probability:** Low (20%)
- **Impact:** Medium (feature incomplete)
- **Mitigation:**
  - Mock providers for testing
  - Provider abstraction layer
  - Graceful degradation on provider failure
  - Circuit breaker pattern
- **Contingency:** Ship with mock provider, add real providers post-release

### C. Low Risks (P2-P3)

- Documentation lag
- UI/UX enhancements (icons)
- Minor spec compliance gaps
- Deprecated feature removal

---

## VI. QUALITY GATES AND ACCEPTANCE CRITERIA

### A. Phase 1 Acceptance (v2.2.0)

**Performance Gates:**
- [ ] Schema validation P95 < 5ms (currently 5-20ms)
- [ ] JSON encoding P95 < 1ms (currently 0.5-2ms)
- [ ] Tool execution concurrency ≥ 10 (currently 1)
- [ ] Subscription fan-out < 10ms for 1000 subscribers

**Functional Gates:**
- [ ] All existing tests pass (269 EUnit, 33 CT)
- [ ] New schema cache tests: 20+ test cases
- [ ] jiffy migration tests: 15+ test cases
- [ ] Async tool tests: 25+ test cases
- [ ] OAuth enhancement tests: 30+ test cases

**Quality Gates:**
- [ ] Compile: errors = 0
- [ ] Dialyzer: warnings = 0
- [ ] Xref: undefined functions = 0
- [ ] Coverage: ≥80% overall, ≥85% core
- [ ] Performance regression: <5%

**Documentation:**
- [ ] Migration guide (jsx → jiffy)
- [ ] OAuth configuration guide
- [ ] Performance tuning guide
- [ ] v2.2.0 release notes

---

### B. Phase 2 Acceptance (v2.3.0)

**Compliance Gates:**
- [ ] Tasks API: 100% spec compliant
- [ ] Sampling: Streaming support for all providers
- [ ] Completion: All completion modes functional
- [ ] Elicitation: URL mode + enhanced enums
- [ ] MCP 2025-11-25: 90% overall compliance

**Functional Gates:**
- [ ] Tasks API tests: 30+ test cases
- [ ] Sampling tests: 40+ test cases
- [ ] Completion tests: 20+ test cases
- [ ] Elicitation tests: 25+ test cases
- [ ] Compliance test suite: 30+ suites

**Quality Gates:**
- [ ] All Phase 1 gates maintained
- [ ] New feature coverage: ≥85%
- [ ] Integration test coverage: 100% for new features
- [ ] Chaos tests: 20+ scenarios

**Documentation:**
- [ ] Tasks API guide
- [ ] Sampling provider guide
- [ ] Completion integration guide
- [ ] Compliance certification report

---

### C. Phase 3 Acceptance (v2.4.0)

**Performance Gates:**
- [ ] RuVector routing overhead: <3ms P95
- [ ] Swarm leader election: <2s (5-node cluster)
- [ ] Distributed session replication: <100ms
- [ ] Server pooling: 10x throughput improvement

**Scalability Gates:**
- [ ] Support 200K concurrent connections/node
- [ ] Support 5-node cluster with automatic failover
- [ ] Support distributed sessions across nodes
- [ ] Support semantic routing at scale

**Quality Gates:**
- [ ] All Phase 1 & 2 gates maintained
- [ ] RuVector tests: 35+ test cases
- [ ] Swarm tests: 40+ test cases
- [ ] Distributed tests: 30+ test cases
- [ ] Performance benchmarks: 10+ new benchmarks

**Documentation:**
- [ ] RuVector integration guide
- [ ] Swarm deployment guide
- [ ] Distributed architecture guide
- [ ] Scaling best practices

---

### D. Phase 4 Acceptance (v3.0.0)

**Performance Gates:**
- [ ] SONA path latency: <0.05ms for cached reads
- [ ] Rust NIF overhead: <0.01ms
- [ ] Cache hit rate: >80% for static resources
- [ ] Overall P50: ≤2ms (from 5ms baseline)

**Integration Gates:**
- [ ] claude-flow integration functional
- [ ] Shared memory IPC stable
- [ ] Rust NIF crash isolation verified
- [ ] Kubernetes operator deploys successfully

**Compliance Gates:**
- [ ] MCP 2025-11-25: ≥95% compliance
- [ ] Security audit: 0 critical vulnerabilities
- [ ] Test coverage: ≥85% overall, ≥90% core
- [ ] All experimental features: ≥80% implementation

**Quality Gates:**
- [ ] All Phase 1-3 gates maintained
- [ ] Rust stability: 7-day chaos test pass
- [ ] SONA tests: 30+ test cases
- [ ] K8s operator tests: 20+ test cases
- [ ] Full compliance test suite: 100% pass

**Documentation:**
- [ ] claude-flow integration guide
- [ ] SONA architecture guide
- [ ] Rust FFI safety guide
- [ ] Kubernetes deployment guide
- [ ] v3.0.0 migration guide
- [ ] Complete API documentation

---

## VII. AGENT ORCHESTRATION STRATEGY

### A. Available Agents (20 Total)

**Core Development (7):**
- erlang-otp-developer
- erlang-transport-builder
- erlang-test-engineer
- erlang-architect
- erlang-researcher
- erlang-performance
- code-reviewer

**Build & Validation (7):**
- build-engineer
- verifier
- agent-01 through agent-05 (compile gates)

**Testing (5):**
- agent-06-test-eunit
- agent-07-test-ct
- agent-08-test-smoke
- agent-09-test-quick
- agent-10-test-proper

**Quality (6):**
- agent-11-coverage
- agent-12-dialyzer
- agent-13-xref
- agent-14-format
- agent-15-benchmark
- agent-16-jidoka

**Workflow (7):**
- erlang-github-ops
- sparc-orchestrator
- plan-designer
- agent-17-poka-yoke
- agent-18-andon
- agent-19-tcps
- agent-20-release

### B. Parallel Execution Workflow (EPIC 9)

**Trigger:** Multi-feature tasks (5+ files, 3+ systems)

**Workflow:**
```
1. FAN-OUT: Spawn all agents in ONE message
   ├─ Task("Research", ..., "erlang-researcher")
   ├─ Task("Architecture", ..., "erlang-architect")
   ├─ Task("Implementation", ..., "erlang-otp-developer")
   ├─ Task("Testing", ..., "erlang-test-engineer")
   ├─ Task("Performance", ..., "erlang-performance")
   └─ Task("Review", ..., "code-reviewer")

2. INDEPENDENT CONSTRUCTION: Each agent works in parallel
   • Unique branches per agent (avoid conflicts)
   • Independent file ownership
   • Asynchronous execution

3. COLLISION DETECTION: Merge coordinator identifies conflicts
   • File-level conflict detection
   • API compatibility checks
   • Test result aggregation

4. CONVERGENCE: Consolidate all work
   • Merge all branches
   • Resolve conflicts
   • Run integration tests

5. REFACTORING: Extract common patterns
   • Identify duplicated code
   • Create shared modules
   • Optimize architecture

6. CLOSURE: Final validation
   • Run all quality gates
   • Generate reports
   • Create release
```

**Expected Speedup:** 2.8x - 4.4x

### C. Phase-Specific Agent Assignment

#### Phase 1 Example: Schema Caching Implementation

```javascript
// ONE MESSAGE - spawn all agents in parallel
Task("Research jesse usage", "Analyze erlmcp_schema_registry.erl", "erlang-researcher")
Task("Design cache architecture", "Design ETS-based schema cache", "erlang-architect")
Task("Implement schema cache", "Create erlmcp_schema_cache.erl", "erlang-otp-developer")
Task("Write EUnit tests", "Create erlmcp_schema_cache_tests.erl", "erlang-test-engineer")
Task("Write CT tests", "Create erlmcp_schema_cache_SUITE.erl", "erlang-test-engineer")
Task("Benchmark schema validation", "Create erlmcp_bench_schema_validation.erl", "erlang-performance")
Task("Review OTP compliance", "Review gen_server implementation", "code-reviewer")
Task("Verify tests", "Run eunit + ct", "verifier")
Task("Check coverage", "Verify ≥85%", "agent-11-coverage")
Task("Run Dialyzer", "Type check", "agent-12-dialyzer")

// Batch todos together
TodoWrite({todos: [
    {content: "Research jesse usage", status: "in_progress", activeForm: "Researching jesse usage"},
    {content: "Design cache architecture", status: "in_progress", activeForm: "Designing cache architecture"},
    {content: "Implement schema cache", status: "pending", activeForm: "Implementing schema cache"},
    {content: "Write EUnit tests", status: "pending", activeForm: "Writing EUnit tests"},
    {content: "Write CT tests", status: "pending", activeForm: "Writing CT tests"},
    {content: "Benchmark performance", status: "pending", activeForm: "Benchmarking performance"},
    {content: "Review code", status: "pending", activeForm: "Reviewing code"},
    {content: "Verify coverage", status: "pending", activeForm: "Verifying coverage"},
    {content: "Run quality gates", status: "pending", activeForm: "Running quality gates"},
    {content: "Generate report", status: "pending", activeForm: "Generating report"}
]})
```

---

## VIII. COMPLIANCE CERTIFICATION CHECKLIST

### A. MCP Specification 2025-11-25

- [ ] **Core Protocol**
  - [ ] JSON-RPC 2.0 compliance
  - [ ] Protocol version negotiation
  - [ ] Capability negotiation
  - [ ] Error code coverage (100%)
  - [ ] Notification formats

- [ ] **Resources**
  - [ ] Static resources
  - [ ] Resource templates
  - [ ] Resource subscriptions
  - [ ] Resource list changed notifications
  - [ ] URI format validation

- [ ] **Tools**
  - [ ] Tool registration
  - [ ] Tool list/call
  - [ ] Schema validation (JSON Schema 2020-12)
  - [ ] Tool list changed notifications
  - [ ] Input validation error handling (SEP-1303)

- [ ] **Prompts**
  - [ ] Prompt list/get
  - [ ] Prompt arguments
  - [ ] Prompt templates
  - [ ] Prompt list changed notifications

- [ ] **Sampling**
  - [ ] Create message (basic + streaming)
  - [ ] Model preferences
  - [ ] System prompt
  - [ ] Temperature/max tokens
  - [ ] Stop sequences
  - [ ] Metadata support

- [ ] **Logging**
  - [ ] Log levels
  - [ ] setLevel notification
  - [ ] Log data types

- [ ] **Completion**
  - [ ] Argument completion
  - [ ] Resource URI completion
  - [ ] Ref completion

- [ ] **Roots**
  - [ ] Root list
  - [ ] Root URI validation

- [ ] **Cancellation**
  - [ ] Request cancellation
  - [ ] Progress token cancellation
  - [ ] Cancellation notifications

- [ ] **Progress**
  - [ ] Progress notifications
  - [ ] Progress token generation

- [ ] **Experimental Features**
  - [ ] Tasks API (create/list/get/cancel/result)
  - [ ] Elicitation API (create/complete)
  - [ ] Icons metadata (tools/resources/prompts)
  - [ ] Server description field

- [ ] **Security**
  - [ ] OAuth 2.0 basic flow
  - [ ] OpenID Connect discovery
  - [ ] Incremental scope consent
  - [ ] RFC 9728 metadata
  - [ ] HTTP Origin validation
  - [ ] Input validation separation

- [ ] **Transports**
  - [ ] STDIO (stderr logging)
  - [ ] TCP
  - [ ] HTTP
  - [ ] WebSocket
  - [ ] SSE (with polling streams)

- [ ] **Schema & Validation**
  - [ ] JSON Schema 2020-12 default
  - [ ] Tool naming guidance compliance
  - [ ] Enhanced enum schemas (SEP-1330)
  - [ ] Default values (SEP-1034)

### B. Testing Coverage

- [ ] **Unit Tests (EUnit)**
  - [ ] 350+ test files
  - [ ] 90%+ coverage for core modules
  - [ ] 85%+ coverage for protocol modules
  - [ ] 100% coverage for public APIs

- [ ] **Integration Tests (CT)**
  - [ ] 60+ test suites
  - [ ] Client-server workflows
  - [ ] Multi-client scenarios
  - [ ] Transport integration
  - [ ] Distributed scenarios

- [ ] **Property Tests (Proper)**
  - [ ] 60+ properties
  - [ ] Protocol invariants
  - [ ] State machine properties
  - [ ] Concurrency properties

- [ ] **Compliance Tests**
  - [ ] 30+ compliance suites
  - [ ] All MCP methods tested
  - [ ] All error codes validated
  - [ ] All notification types verified

- [ ] **Performance Tests**
  - [ ] 30+ benchmarks
  - [ ] Latency regression tests
  - [ ] Throughput regression tests
  - [ ] Memory regression tests

- [ ] **Chaos Tests**
  - [ ] 20+ chaos scenarios
  - [ ] Process crash recovery
  - [ ] Network failure handling
  - [ ] Resource exhaustion handling

### C. Documentation

- [ ] **API Documentation**
  - [ ] 100% public API coverage
  - [ ] Type specifications
  - [ ] Usage examples
  - [ ] Error handling guide

- [ ] **Architecture Documentation**
  - [ ] Supervision tree diagrams
  - [ ] Module interaction diagrams
  - [ ] Data flow diagrams
  - [ ] Deployment architecture

- [ ] **Integration Guides**
  - [ ] claude-flow integration
  - [ ] OAuth configuration
  - [ ] Transport configuration
  - [ ] Performance tuning

- [ ] **Migration Guides**
  - [ ] v2.1.0 → v2.2.0
  - [ ] v2.2.0 → v2.3.0
  - [ ] v2.3.0 → v2.4.0
  - [ ] v2.4.0 → v3.0.0

---

## IX. SUCCESS METRICS

### A. Technical Metrics

| Metric | Baseline (v2.1.0) | Target (v3.0.0) | Measurement |
|--------|-------------------|-----------------|-------------|
| **Performance** |
| P50 Latency | 5ms | 2ms | Continuous benchmarking |
| P95 Latency | 20ms | 8ms | Continuous benchmarking |
| P99 Latency | 50ms | 20ms | Continuous benchmarking |
| Throughput | 10K req/s | 100K req/s | Load testing |
| Registry | 553K msg/s | 600K+ msg/s | Microbenchmark |
| Connections | 40-50K/node | 200K/node | Stress testing |
| SONA Latency | N/A | <0.05ms | Specialized benchmark |
| **Compliance** |
| MCP Spec Coverage | 65% | 95%+ | Compliance test suite |
| Experimental Features | 10% | 80%+ | Feature matrix |
| Security Compliance | 40% | 95%+ | Security audit |
| **Quality** |
| Test Coverage (Overall) | 75% | 85%+ | rebar3 cover |
| Test Coverage (Core) | 85% | 90%+ | rebar3 cover |
| Test Count | 340 | 550+ | Test suite count |
| Dialyzer Warnings | 0 | 0 | rebar3 dialyzer |
| Xref Undefined | 0 | 0 | rebar3 xref |
| **Reliability** |
| MTBF | >1000h | >2000h | Chaos testing |
| MTTR | <10s | <5s | Recovery testing |
| Availability | 99.5% | 99.9% | Production monitoring |

### B. Project Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| **Timeline** |
| Phase 1 Completion | 4-6 weeks | Project tracking |
| Phase 2 Completion | 10-14 weeks cumulative | Project tracking |
| Phase 3 Completion | 18-24 weeks cumulative | Project tracking |
| Phase 4 Completion | 28-36 weeks cumulative | Project tracking |
| **Effort** |
| Total Engineering Hours | 1,712 | Time tracking |
| Average FTE | 1.3 | Resource allocation |
| **Quality** |
| Gate Pass Rate (Phase 1) | 100% | Quality gate tracking |
| Gate Pass Rate (Phase 2) | 100% | Quality gate tracking |
| Gate Pass Rate (Phase 3) | 100% | Quality gate tracking |
| Gate Pass Rate (Phase 4) | 100% | Quality gate tracking |

### C. Business Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| **Adoption** |
| Production Deployments | 100+ | Usage tracking |
| GitHub Stars | 500+ | GitHub metrics |
| Community Contributors | 20+ | GitHub metrics |
| **Ecosystem** |
| Third-Party Integrations | 10+ | Integration catalog |
| Language Bindings | 3+ | SDK count |
| **Support** |
| Issue Response Time | <24h | GitHub analytics |
| Documentation Quality Score | 90%+ | User surveys |
| Community Health Score | 85%+ | GitHub community metrics |

---

## X. RECOMMENDATIONS AND NEXT STEPS

### A. Immediate Actions (Week 1)

1. **Review and Approve Master Plan** (1 day)
   - Stakeholder review
   - Risk acceptance
   - Resource commitment

2. **Establish Project Infrastructure** (2 days)
   - GitHub project board
   - CI/CD pipeline updates
   - Performance monitoring dashboard
   - Documentation repository

3. **Phase 1 Kickoff** (2 days)
   - Team onboarding
   - Environment setup
   - Architecture design review
   - Sprint planning

### B. Short-Term Actions (Month 1)

1. **Phase 1 Execution** (4-6 weeks)
   - Schema caching implementation
   - jiffy migration
   - Async tool execution
   - OAuth enhancements

2. **Continuous Integration** (Ongoing)
   - Performance regression monitoring
   - Coverage tracking
   - Quality gate enforcement

3. **Documentation Updates** (Ongoing)
   - Architecture diagrams
   - API documentation
   - Migration guides

### C. Long-Term Actions (Months 2-9)

1. **Phase 2-4 Execution** (24-32 weeks)
   - Feature completeness (Phase 2)
   - Optimization (Phase 3)
   - Advanced integration (Phase 4)

2. **Compliance Certification** (Ongoing)
   - Compliance test suite
   - Security audits
   - Performance validation

3. **Community Engagement** (Ongoing)
   - Release communications
   - Documentation updates
   - Support channels

### D. Decision Points

**Decision Point 1: End of Phase 1**
- **Criteria:** All Phase 1 gates pass
- **Decision:** Proceed to Phase 2 OR extend Phase 1 for remediation
- **Timeline:** Week 6

**Decision Point 2: End of Phase 2**
- **Criteria:** 90% MCP compliance achieved
- **Decision:** Proceed to Phase 3 OR focus on remaining gaps
- **Timeline:** Week 14

**Decision Point 3: End of Phase 3**
- **Criteria:** Performance targets met
- **Decision:** Proceed to Phase 4 OR optimize further
- **Timeline:** Week 24

**Decision Point 4: End of Phase 4**
- **Criteria:** All acceptance criteria met
- **Decision:** Release v3.0.0 OR address critical gaps
- **Timeline:** Week 36

---

## XI. APPENDICES

### A. Reference Documents

1. **Source Analyses:**
   - MCP_SPEC_PERFORMANCE_ANALYSIS.md (Performance baseline)
   - MCP_2025-11-25_ARCHITECTURE_DESIGN.md (Architecture)
   - MCP_IMPLEMENTATION_PHASING.md (Phasing plan)
   - MCP_TEST_STRATEGY.md (Testing approach)
   - MCP_2025-11-25_COMPLIANCE_GAP_ANALYSIS.md (Gap analysis)

2. **MCP Specification:**
   - MCP 2025-11-25 Official Specification
   - SEP-973, SEP-835, SEP-1303, SEP-1613, SEP-1699, SEP-1730

3. **erlmcp Documentation:**
   - CLAUDE.md (Project requirements)
   - docs/architecture.md
   - docs/protocol.md
   - docs/otp-patterns.md

### B. Glossary

- **Chicago School TDD:** Test-driven development approach using real collaborators, no mocks, state-based verification
- **SONA:** Sub-millisecond latency requirement (<0.05ms) for claude-flow integration
- **RuVector:** Semantic routing and intelligence layer using vector embeddings (HNSW, ONNX)
- **Swarm:** Distributed coordination layer using Raft consensus and consistent hashing
- **P0/P1/P2/P3:** Priority levels (P0 = Critical, P1 = High, P2 = Medium, P3 = Low)
- **FTE:** Full-Time Equivalent
- **MTBF:** Mean Time Between Failures
- **MTTR:** Mean Time To Recovery
- **ETS:** Erlang Term Storage (in-memory key-value store)
- **NIF:** Native Implemented Function (C/Rust extension to Erlang)

### C. Change Log

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2026-02-01 | Code Reviewer Agent | Initial master plan consolidating 5 agent analyses |

---

**END OF MASTER IMPLEMENTATION PLAN**

**Authoritative Status:** This document supersedes all individual agent analyses and serves as the single source of truth for erlmcp MCP 2025-11-25 compliance implementation.

**Approval Required:** Stakeholder sign-off before Phase 1 execution.

**Next Review:** After Phase 1 completion (Week 6).
