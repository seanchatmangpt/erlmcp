# erlmcp 4-Phase Implementation Plan for 100% MCP Spec Support

**Document Version:** 1.0.0
**Created:** 2026-02-02
**Authority:** Plan Designer Agent
**Target:** 100% MCP Specification 2025-11-25 Compliance
**Baseline:** erlmcp v2.1.0 (65% compliance)
**Target:** erlmcp v3.0.0 (95%+ compliance)

---

## Executive Summary

This document provides a detailed, phase-by-phase roadmap to achieve full MCP specification support across four major releases: v2.2, v2.3, v2.4, and v3.0. The plan addresses 23 compliance gaps, implements 8 missing features, and optimizes performance by 75%+ while maintaining backward compatibility.

**Timeline:** 30-38 weeks (7-9 months)
**Effort:** 1,712 engineering hours
**Risk Level:** Medium (High in Phase 4 due to Rust NIFs)
**Current Compliance:** 65% → **Target:** 95%+

**Key Deliverables:**
- v2.2.0: Performance foundation (Schema caching, jiffy migration, async tools)
- v2.3.0: Feature completeness (Tasks API, Sampling, Elicitation, Completion)
- v2.4.0: Scale & optimization (RuVector, Swarm, distributed sessions)
- v3.0.0: Advanced integration (claude-flow, SONA routing, Kubernetes operator)

---

## Table of Contents

1. [Phase Progression Map](#phase-progression-map)
2. [Phase 1: Performance Foundation (v2.2.0)](#phase-1-performance-foundation-v220)
3. [Phase 2: Feature Completeness (v2.3.0)](#phase-2-feature-completeness-v230)
4. [Phase 3: Scale & Optimization (v2.4.0)](#phase-3-scale--optimization-v240)
5. [Phase 4: Advanced Integration (v3.0.0)](#phase-4-advanced-integration-v300)
6. [Gantt Timeline & Critical Path](#gantt-timeline--critical-path)
7. [Resource Allocation](#resource-allocation)
8. [Risk Management Matrix](#risk-management-matrix)
9. [Success Criteria & Quality Gates](#success-criteria--quality-gates)
10. [Migration & Deployment Strategy](#migration--deployment-strategy)

---

## Phase Progression Map

```
Current State: v2.1.0 (65% compliance)
├── 42/65 features at ≥80%
├── 553K msg/s registry, P50=5ms, P95=20ms
├── 164 modules, 84+ test suites
└── Gaps: Sampling (18%), Tasks (0%), Security (26%)

    ↓ Phase 1 (4-6 weeks, 256 hours)

v2.2.0: Performance Foundation (75% compliance)
├── Schema validation: 5-20ms → 1-5ms (75% ↓)
├── JSON codec: jsx → jiffy (60% ↓)
├── Async tool execution (10x throughput)
├── OAuth enhancements (40% → 100%)
└── SSE polling streams (0% → 100%)

    ↓ Phase 2 (6-8 weeks, 308 hours)

v2.3.0: Feature Completeness (90% compliance)
├── Tasks API (0% → 100%) [8 features]
├── Sampling/LLM (18% → 100%) [12 features]
├── Elicitation (1% → 100%) [7 features]
├── Completion (42% → 100%) [5 features]
└── Roots (40% → 100%) [3 features]

    ↓ Phase 3 (8-10 weeks, 528 hours)

v2.4.0: Scale & Optimization (93% compliance)
├── RuVector intelligence (HNSW, ONNX)
├── Swarm topology (Raft, consistent hashing)
├── Distributed sessions (Mnesia)
├── HTTP/2 multiplexing
└── Server pooling (10x throughput)

    ↓ Phase 4 (10-12 weeks, 580 hours)

v3.0.0: Advanced Integration (95%+ compliance)
├── claude-flow integration (STDIO + SONA)
├── SONA routing (<0.05ms, Rust FFI)
├── Advanced caching (predictive, semantic)
├── Kubernetes operator
└── Full compliance certification
```

---

## Phase 1: Performance Foundation (v2.2.0)

**Duration:** 4-6 weeks
**Effort:** 256 hours
**Risk:** Low-Medium
**Target Compliance:** 75% (+10% from baseline)
**FTE:** 1.0 (1 Senior Erlang Engineer, 0.5 QA, 0.25 Tech Writer)

### 1.1 Feature Set

| # | Feature | Spec Section | Current | Target | Priority | Effort |
|---|---------|-------------|---------|--------|----------|--------|
| 1 | Schema Validation Caching | Tools | 75% | 100% | P0 | 80h |
| 2 | JSON Codec Migration (jsx→jiffy) | Core | N/A | 100% | P0 | 48h |
| 3 | Async Tool Execution | Tools | N/A | 100% | P0 | 56h |
| 4 | OAuth 2.0 Enhancements | Security | 40% | 100% | P0 | 32h |
| 5 | SSE Polling Streams | Transports | 0% | 100% | P1 | 24h |
| 6 | Input Validation Errors (SEP-1303) | Schema | 80% | 100% | P2 | 16h |

**Total Features:** 6 major enhancements
**Compliance Impact:** 49/65 features at ≥80%

### 1.2 Module Changes

#### 1.2.1 Schema Validation Caching

**NEW MODULE:** `apps/erlmcp_core/src/erlmcp_schema_cache.erl` (350 lines)

```erlang
-module(erlmcp_schema_cache).
-behaviour(gen_server).
-export([start_link/0, validate/2, preload_schema/2, get_stats/0]).

%% State structure
-record(state, {
    schemas :: ets:tid(),              % ETS: {SchemaId, CompiledSchema}
    compile_stats :: map(),            % Hit/miss statistics
    lru :: queue:queue(),              % LRU eviction queue
    max_size :: pos_integer()          % Max cache entries
}).

%% Lines 1-50: init/1, gen_server callbacks
%% Lines 51-120: validate/2 - Cache lookup + jesse validation
%% Lines 121-180: preload_schema/2 - Pre-compile schemas
%% Lines 181-220: Cache eviction (LRU policy)
%% Lines 221-280: Statistics tracking
%% Lines 281-350: Helper functions
```

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_tool.erl` (Lines 156-203)
- Replace direct jesse:validate/2 calls
- Add erlmcp_schema_cache:validate/2 integration
- Remove inline schema compilation

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_core_sup.erl` (Lines 89-97)
- Add erlmcp_schema_cache child spec
- Permanent restart strategy

**Effort:** 80 hours (40 implementation, 24 tests, 16 benchmarks)

**Performance Impact:**
- Before: 5-20ms per validation
- After: 1-5ms (75% reduction)
- Throughput: 5x for schema-heavy workloads

---

#### 1.2.2 JSON Codec Migration

**NEW MODULE:** `apps/erlmcp_core/src/erlmcp_json_codec.erl` (120 lines)
- Behavior definition for pluggable JSON codecs
- Lines 1-40: Behavior callbacks
- Lines 41-80: Codec selection logic
- Lines 81-120: Performance monitoring

**NEW MODULE:** `apps/erlmcp_core/src/erlmcp_json_codec_jiffy.erl` (180 lines)
- jiffy implementation of codec behavior
- Lines 1-60: encode/1 with error handling
- Lines 61-120: decode/1 with options
- Lines 121-180: Type conversions, null handling

**NEW MODULE:** `apps/erlmcp_core/src/erlmcp_json_codec_jsx.erl` (150 lines)
- jsx fallback implementation
- Same structure as jiffy codec

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_json_rpc.erl` (Lines 34-89)
- Replace jsx:encode → erlmcp_json_codec:encode
- Replace jsx:decode → erlmcp_json_codec:decode
- Add codec configuration support

**MODIFIED:** `rebar.config` (Lines 12-18)
- Add jiffy 1.1.1 dependency
- Keep jsx as fallback dependency

**Effort:** 48 hours (24 implementation, 16 tests, 8 migration)

**Performance Impact:**
- Before: 0.5-2ms encoding/decoding
- After: 0.2-0.7ms (60% reduction)

**Breaking Changes:** NONE (abstraction layer maintains compatibility)

---

#### 1.2.3 Async Tool Execution

**NEW MODULE:** `apps/erlmcp_core/src/erlmcp_tool_executor_sup.erl` (200 lines)
- Supervisor for tool worker pool
- Lines 1-80: Supervisor callbacks
- Lines 81-140: Poolboy configuration
- Lines 141-200: Worker lifecycle management

**NEW MODULE:** `apps/erlmcp_core/src/erlmcp_tool_worker.erl` (280 lines)
- gen_server worker for async tool execution
- Lines 1-60: init/1, worker setup
- Lines 61-150: execute/4 - Tool execution with timeout
- Lines 151-220: Error handling, crash isolation
- Lines 221-280: Monitoring, metrics

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_server.erl` (Lines 567-645)
- Replace synchronous handle_call for tools/call
- Add poolboy:transaction for async execution
- Add timeout enforcement (default: 30s)

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_core_sup.erl` (Lines 98-106)
- Add erlmcp_tool_executor_sup child spec

**Effort:** 56 hours (32 implementation, 24 tests)

**Performance Impact:**
- Before: 1 concurrent tool per server
- After: 10-50 concurrent tools (poolboy size)
- Throughput: 10-50x improvement

**Breaking Changes:** NONE (API remains same, internal execution async)

---

#### 1.2.4 OAuth 2.0 Enhancements

**NEW MODULE:** `apps/erlmcp_core/src/erlmcp_auth_oauth.erl` (400 lines)
- OpenID Connect discovery support
- Lines 1-100: Discovery endpoint parsing
- Lines 101-200: Incremental scope consent
- Lines 201-300: Client metadata handling
- Lines 301-400: RFC 9728 resource metadata

**NEW MODULE:** `apps/erlmcp_core/src/erlmcp_auth_incremental_scope.erl` (250 lines)
- WWW-Authenticate header generation
- Lines 1-100: Scope validation
- Lines 101-200: Token refresh logic
- Lines 201-250: Error responses

**MODIFIED:** `apps/erlmcp_transports/src/erlmcp_transport_http.erl` (Lines 234-298)
- Add HTTP Origin validation
- Add 403 Forbidden on origin mismatch
- Add CORS headers

**Effort:** 32 hours (20 implementation, 12 tests)

**Breaking Changes:**
- **Migration Required:** OAuth configuration format changed
  - Before: `{oauth, #{provider => Provider, client_id => Id}}`
  - After: `{oauth, #{discovery_url => Url, client_id => Id, scopes => [...]}}`
- **Migration Path:** Provided via `docs/OAUTH_MIGRATION_GUIDE.md`

---

#### 1.2.5 SSE Polling Streams

**NEW MODULE:** `apps/erlmcp_transports/src/erlmcp_sse_event_store.erl` (300 lines)
- gen_server for SSE event buffering
- Lines 1-80: Event storage (ETS)
- Lines 81-160: GET polling support
- Lines 161-240: Stream resumption (Last-Event-ID)
- Lines 241-300: Event expiration (TTL)

**MODIFIED:** `apps/erlmcp_transports/src/erlmcp_transport_sse.erl` (Lines 123-234)
- Add polling endpoint handler
- Add stream resumption logic
- Add server-initiated disconnect

**Effort:** 24 hours (16 implementation, 8 tests)

---

### 1.3 Dependencies

**External:**
- jiffy 1.1.1 (NEW)
- poolboy 1.5.2 (already present, expanded usage)

**Internal:**
- erlmcp_schema_cache → jesse, ETS
- erlmcp_json_codec → jiffy/jsx
- erlmcp_tool_executor_sup → poolboy
- erlmcp_auth_oauth → gun (HTTP client)

**Dependency Graph:**
```
jiffy ────→ erlmcp_json_codec_jiffy ────→ erlmcp_json_rpc
                                              ↓
poolboy ───→ erlmcp_tool_executor_sup ──→ erlmcp_server
                                              ↓
jesse ─────→ erlmcp_schema_cache ────────→ erlmcp_tool
```

**No Circular Dependencies:** Validated ✓

---

### 1.4 Testing Coverage Requirements

| Test Type | Target | Details |
|-----------|--------|---------|
| **EUnit** | 90%+ | 81 new test files for new modules |
| **CT** | 27 new suites | Integration tests for async execution, OAuth flows |
| **Proper** | 15 properties | Schema caching invariants, JSON roundtrip |
| **Compliance** | 6 suites | OAuth compliance, SSE spec compliance |
| **Performance** | 10 benchmarks | Schema validation, JSON codec, tool throughput |
| **Chaos** | 8 scenarios | Pool exhaustion, schema cache corruption |

**Chicago School TDD:** All tests use real gen_servers, no mocks

---

### 1.5 Deployment Strategy

**Feature Flags:**
```erlang
%% Enable/disable jiffy codec
{json_codec, jiffy}.  % Options: jiffy | jsx

%% Enable/disable async tools
{async_tools, true}.  % Default: false (gradual rollout)

%% OAuth provider
{oauth_provider, openid}.  % Options: openid | basic
```

**Rollout Plan:**
1. **Week 1-2:** Implement features with feature flags OFF
2. **Week 3:** Enable in test environments
3. **Week 4:** Enable for 10% of production traffic
4. **Week 5:** Enable for 50% of production traffic
5. **Week 6:** Enable for 100%, release v2.2.0

**Rollback:** All features can be disabled via feature flags without code changes

---

### 1.6 Risk Mitigation

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| jiffy NIF crashes | Low (15%) | High | Feature flag, crash isolation, 7-day stress test |
| Schema cache memory leak | Low (10%) | Medium | LRU eviction, monitoring, configurable limits |
| Pool exhaustion (tools) | Medium (30%) | Medium | Configurable overflow, backpressure, metrics |
| OAuth breaking changes | Medium (25%) | High | Migration guide, backward compat mode for 2 releases |
| Performance regression | Low (15%) | High | Continuous benchmarking, rollback via feature flags |

**Critical Mitigations:**
1. **jiffy Stability:** 1000+ EUnit tests, 7-day chaos testing before production
2. **Pool Monitoring:** Grafana dashboard for pool utilization, alerts at 80%
3. **OAuth Migration:** Auto-migration script provided, dual-mode support for 6 months

---

### 1.7 Success Criteria

**Functional:**
- ✅ All 269 existing EUnit tests pass
- ✅ 81 new test files created (90%+ coverage)
- ✅ Zero Dialyzer warnings
- ✅ Zero Xref undefined functions

**Performance:**
- ✅ Schema validation P95 < 5ms (from 20ms)
- ✅ JSON encoding P95 < 1ms (from 2ms)
- ✅ Tool execution: 10+ concurrent per server
- ✅ Performance regression < 5%

**Compliance:**
- ✅ 49/65 features at ≥80% (75% compliance)
- ✅ OAuth 2.0: 100% compliant
- ✅ SSE: 100% compliant

**Quality Gates:**
- ✅ `make check` passes (compile + tests + dialyzer + xref)
- ✅ Coverage ≥80% overall, ≥85% core modules
- ✅ CI/CD pipeline green for 7 consecutive days

---

## Phase 2: Feature Completeness (v2.3.0)

**Duration:** 6-8 weeks
**Effort:** 308 hours
**Risk:** Medium
**Target Compliance:** 90% (+15% from v2.2.0)
**FTE:** 1.0 (1 Senior Erlang Engineer, 0.5 QA, 0.25 Tech Writer)

### 2.1 Feature Set

| # | Feature | Spec Section | Current | Target | Priority | Effort |
|---|---------|-------------|---------|--------|----------|--------|
| 7 | Tasks API (Experimental) | Tasks | 0% | 100% | P0 | 80h |
| 8 | Sampling/LLM (Complete) | Sampling | 18% | 100% | P1 | 84h |
| 9 | Elicitation API | Elicitation | 1% | 100% | P1 | 48h |
| 10 | Completion (Full) | Completion | 42% | 100% | P1 | 32h |
| 11 | Roots (Full) | Roots | 40% | 100% | P2 | 24h |
| 12 | Icons Metadata | Metadata | 30% | 100% | P2 | 20h |
| 13 | Prompt Verification | Prompts | 90% | 100% | P2 | 20h |

**Total Features:** 7 major implementations
**Compliance Impact:** 58/65 features at ≥80%

---

### 2.2 Module Changes

#### 2.2.1 Tasks API (0% → 100%)

**NEW MODULE:** `apps/erlmcp_core/src/erlmcp_task_manager.erl` (500 lines)
```erlang
-module(erlmcp_task_manager).
-behaviour(gen_server).

%% Lines 1-100: Task creation, scheduling
%% Lines 101-200: Task lifecycle (pending → running → completed/failed)
%% Lines 201-300: Task result storage (ETS)
%% Lines 301-400: Task expiration (TTL), cleanup
%% Lines 401-500: Task listing, filtering, cancellation
```

**NEW MODULE:** `apps/erlmcp_core/src/erlmcp_task_worker.erl` (350 lines)
- Task execution worker (gen_server)
- Lines 1-100: Worker initialization
- Lines 101-200: Task execution with timeout
- Lines 201-300: Progress notifications
- Lines 301-350: Error handling, retry logic

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_server.erl` (Lines 789-921)
- Add tasks/create handler
- Add tasks/list handler
- Add tasks/get handler
- Add tasks/result handler
- Add tasks/cancel handler

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_core_sup.erl` (Lines 107-115)
- Add erlmcp_task_manager child spec

**Effort:** 80 hours (48 implementation, 32 tests)

**API Specification:**
```erlang
%% Tasks API
tasks_create(Name, Params) -> {ok, TaskId} | {error, Reason}.
tasks_list(Cursor) -> {ok, #{tasks => [...], nextCursor => ...}}.
tasks_get(TaskId) -> {ok, TaskStatus} | {error, task_not_found}.
tasks_result(TaskId) -> {ok, Result} | {error, not_ready | task_not_found}.
tasks_cancel(TaskId) -> ok | {error, Reason}.
```

**State Transitions:**
```
pending → running → completed
                  → failed
                  → cancelled
```

**Breaking Changes:** NONE (new experimental feature)

---

#### 2.2.2 Sampling/LLM Complete (18% → 100%)

**NEW MODULE:** `apps/erlmcp_core/src/erlmcp_sampling_stream_sup.erl` (180 lines)
- Supervisor for streaming workers
- simple_one_for_one strategy

**NEW MODULE:** `apps/erlmcp_core/src/erlmcp_sampling_stream_worker.erl` (400 lines)
- gen_server for SSE/WebSocket streaming
- Lines 1-100: Stream setup, connection management
- Lines 101-200: Chunk processing, buffering
- Lines 201-300: Token counting, stop sequence detection
- Lines 301-400: Error handling, stream cleanup

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_sampling.erl` (Lines 234-567)
- Add streaming support (create_message_stream)
- Add model preferences handling
- Add system prompt support
- Add temperature/max_tokens/stop_sequences

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_llm_provider_anthropic.erl` (Lines 123-298)
- Add streaming API calls
- Add SSE parsing for Claude API
- Add chunk assembly logic

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_llm_provider_openai.erl` (Lines 112-267)
- Add streaming support (GPT-4 API)
- Add delta handling

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_llm_provider_local.erl` (Lines 89-201)
- Add mock streaming for testing

**Effort:** 84 hours (48 implementation, 24 tests, 12 provider integration)

**Performance Targets:**
- Non-streaming: P95 < 500ms (depends on LLM provider)
- Streaming: First chunk < 200ms, subsequent < 50ms
- Concurrent streams: 100+ per server

**Breaking Changes:** NONE (extends existing API)

---

#### 2.2.3 Elicitation API (1% → 100%)

**NEW MODULE:** `apps/erlmcp_core/src/erlmcp_elicitation.erl` (350 lines)
- gen_server for user interaction elicitation
- Lines 1-100: Elicitation creation
- Lines 101-200: URL mode handling (SEP-1036)
- Lines 201-300: Enhanced enums (SEP-1330)
- Lines 301-350: Default values (SEP-1034)

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_server.erl` (Lines 922-1056)
- Add elicitation/create handler
- Add elicitation response handling

**Effort:** 48 hours (32 implementation, 16 tests)

**API Specification:**
```erlang
%% Elicitation modes
-type elicitation_mode() :: text | number | boolean | enum | multi_enum | url.

%% Create elicitation request
elicitation_create(#{
    type => Mode,
    message => Prompt,
    default => Value,  % Optional
    options => [...]   % For enum/multi_enum
}) -> {ok, ElicitationId}.
```

---

#### 2.2.4 Completion Full (42% → 100%)

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_completion.erl` (Lines 156-345)
- Add ref completion support (ref/tool, ref/resource, ref/prompt)
- Add context-aware completion
- Add fuzzy matching for partial inputs

**Effort:** 32 hours (20 implementation, 12 tests)

---

#### 2.2.5 Roots Full (40% → 100%)

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_roots_server.erl` (Lines 89-201)
- Add file:// URI validation
- Add path canonicalization
- Add security checks (path traversal prevention)

**Effort:** 24 hours (16 implementation, 8 tests)

---

#### 2.2.6 Icons Metadata (30% → 100%)

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_icon_cache.erl` (Lines 123-267)
- Add icon URL validation
- Add icon caching (HTTP caching headers)
- Add icon format validation (PNG, SVG)

**MODIFIED:** Tools, Resources, Prompts modules (3 × 20 lines)
- Add icon field support

**Effort:** 20 hours (12 implementation, 8 tests)

---

### 2.3 Dependencies

**External:** None (use existing libraries)

**Internal:**
- erlmcp_task_manager → ETS, monitoring
- erlmcp_sampling_stream_sup → gun (HTTP/2), cowboy (WebSocket)
- erlmcp_elicitation → Validation logic

---

### 2.4 Testing Coverage Requirements

| Test Type | Target | Details |
|-----------|--------|---------|
| **EUnit** | 90%+ | 95 new test files |
| **CT** | 35 new suites | Tasks lifecycle, sampling streams, elicitation flows |
| **Proper** | 25 properties | Task state machines, sampling invariants |
| **Compliance** | 7 new suites | Full MCP spec compliance for all features |
| **Performance** | 15 benchmarks | Task throughput, sampling latency |
| **Chaos** | 12 scenarios | Task failures, streaming interruptions |

---

### 2.5 Deployment Strategy

**Feature Flags:**
```erlang
{experimental_tasks, true}.  % Default: false
{experimental_elicitation, true}.  % Default: false
{sampling_streaming, true}.  % Default: false
```

**Rollout:**
- Week 1-3: Implement with flags OFF
- Week 4-5: Test environments
- Week 6: Production (opt-in for experimental features)
- Week 7-8: Broader rollout, release v2.3.0

---

### 2.6 Risk Mitigation

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| LLM provider API changes | Medium (40%) | Medium | Provider abstraction, version pinning |
| Streaming complexity | Medium (35%) | Medium | Incremental implementation, extensive testing |
| Task memory leak | Low (20%) | High | TTL expiration, monitoring, stress testing |
| Elicitation security | Low (15%) | High | Input validation, URL sanitization |

---

### 2.7 Success Criteria

**Functional:**
- ✅ Tasks API: 100% spec compliant (8/8 features)
- ✅ Sampling: 100% spec compliant (12/12 features)
- ✅ Elicitation: 100% spec compliant (7/7 features)
- ✅ All tests pass (400+ total test files)

**Performance:**
- ✅ Task creation: P95 < 10ms
- ✅ Sampling first chunk: < 200ms
- ✅ Completion latency: < 50ms

**Compliance:**
- ✅ 58/65 features at ≥80% (90% compliance)
- ✅ 100% MCP method coverage

---

## Phase 3: Scale & Optimization (v2.4.0)

**Duration:** 8-10 weeks
**Effort:** 528 hours
**Risk:** Medium-High
**Target Compliance:** 93% (+3% from v2.3.0)
**FTE:** 1.5 (1 Senior Erlang, 0.5 Distributed Systems, 0.5 Performance, 1 QA)

### 3.1 Feature Set

| # | Feature | Current | Target | Priority | Effort |
|---|---------|---------|--------|----------|--------|
| 14 | RuVector Intelligence | N/A | 100% | P1 | 156h |
| 15 | Swarm Topology | N/A | 100% | P1 | 116h |
| 16 | Distributed Sessions (Mnesia) | N/A | 100% | P1 | 40h |
| 17 | HTTP/2 Multiplexing | 70% | 100% | P2 | 24h |
| 18 | Zero-Copy Binaries | N/A | 100% | P2 | 32h |
| 19 | Server Pooling | N/A | 100% | P1 | 40h |
| 20 | Resource Subscription Optimization | 85% | 100% | P1 | 16h |

**Total Features:** 7 optimization features
**Compliance Impact:** 60/65 features at ≥80%

---

### 3.2 Module Changes

#### 3.2.1 RuVector Intelligence

**NEW MODULES:** (540 lines total)
- `erlmcp_ruvector_sup.erl` (120 lines) - Supervisor
- `erlmcp_ruvector_index.erl` (180 lines) - HNSW index management
- `erlmcp_ruvector_embeddings.erl` (140 lines) - ONNX embedding generation
- `erlmcp_ruvector_router.erl` (100 lines) - Semantic routing logic

**Dependencies:**
- ONNX Runtime (via Rustler NIF)
- hnswlib (via NIF wrapper)

**Effort:** 156 hours (96 implementation, 24 NIF integration, 36 tests)

**Performance Targets:**
- Embedding generation: P95 < 2ms
- HNSW search: P95 < 0.5ms (100K vectors)
- Total overhead: P95 < 3ms

---

#### 3.2.2 Swarm Topology

**NEW MODULES:** (480 lines total)
- `erlmcp_swarm_sup.erl` (100 lines) - Supervisor
- `erlmcp_swarm_coordinator.erl` (200 lines) - Raft consensus (gen_statem)
- `erlmcp_swarm_router.erl` (120 lines) - Consistent hashing
- `erlmcp_swarm_gossip.erl` (60 lines) - Membership protocol

**Effort:** 116 hours (80 implementation, 36 tests)

**Performance Targets:**
- Leader election: < 2s (5-node cluster)
- Routing overhead: < 5ms per request

---

#### 3.2.3 Distributed Sessions (Mnesia)

**MODIFIED:** `apps/erlmcp_core/src/erlmcp_session_backend_mnesia.erl` (Lines 123-298)
- Add disc_copies replication
- Add cross-node session migration
- Add conflict resolution

**Effort:** 40 hours (24 implementation, 16 tests)

**Performance Targets:**
- Session create: P95 < 10ms
- Session restore: P95 < 5ms
- Cross-node replication: < 100ms

---

#### 3.2.4 HTTP/2 Multiplexing

**NEW MODULE:** `apps/erlmcp_transports/src/erlmcp_transport_http_pool_sup.erl` (180 lines)
- Poolboy pool of HTTP/2 connections

**MODIFIED:** `apps/erlmcp_transports/src/erlmcp_transport_http.erl` (Lines 345-489)
- Use pooled connections
- Stream multiplexing support

**Effort:** 24 hours (16 implementation, 8 tests)

---

#### 3.2.5 Server Pooling

**NEW MODULES:**
- `erlmcp_server_pool_sup.erl` (140 lines)
- `erlmcp_server_router.erl` (160 lines)

**Effort:** 40 hours (28 implementation, 12 tests)

**Performance Targets:**
- Throughput: 10x (10 instances vs 1)

---

### 3.3 Dependencies

**External:**
- ONNX Runtime (Rustler NIF)
- hnswlib (NIF wrapper)
- Mnesia (OTP built-in)

**Internal:**
- erlmcp_ruvector_sup → ONNX, HNSW NIFs
- erlmcp_swarm_sup → gen_statem, consistent hashing library

---

### 3.4 Testing Coverage

| Test Type | Target | Details |
|-----------|--------|---------|
| **EUnit** | 85%+ | 60 new test files |
| **CT** | 40 new suites | Distributed scenarios, swarm failover |
| **Proper** | 30 properties | RuVector routing, swarm consensus |
| **Performance** | 25 benchmarks | Throughput, latency under load |
| **Chaos** | 20 scenarios | Network partitions, node failures |

---

### 3.5 Deployment Strategy

**Kubernetes Support:**
- StatefulSet for Mnesia cluster
- Service mesh for routing
- Auto-scaling based on metrics

**Rollout:**
- Week 1-4: Implement RuVector + Swarm
- Week 5-6: Distributed sessions
- Week 7-8: HTTP/2 optimization + testing
- Week 9-10: Performance validation, release v2.4.0

---

### 3.6 Risk Mitigation

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| ONNX NIF crashes | Medium (40%) | High | Crash isolation, fallback to non-semantic routing |
| Raft split-brain | Low (10%) | High | Network partition testing, fencing |
| Mnesia corruption | Low (15%) | High | Regular backups, repair procedures |
| Performance regression | Medium (30%) | High | Continuous benchmarking, feature flags |

---

### 3.7 Success Criteria

**Performance:**
- ✅ RuVector overhead: < 3ms P95
- ✅ Swarm leader election: < 2s
- ✅ Mnesia replication: < 100ms
- ✅ Server pooling: 8-10x throughput

**Scalability:**
- ✅ 5-node cluster operational
- ✅ 200K connections per node
- ✅ Automatic failover working

**Compliance:**
- ✅ 60/65 features at ≥80% (93% compliance)

---

## Phase 4: Advanced Integration (v3.0.0)

**Duration:** 10-12 weeks
**Effort:** 580 hours
**Risk:** High (Rust NIFs, claude-flow integration)
**Target Compliance:** 95%+ (+2%+ from v2.4.0)
**FTE:** 2.0 (1 Senior Erlang, 1 Rust Engineer, 0.5 DevOps, 1 QA, 0.5 Tech Writer)

### 4.1 Feature Set

| # | Feature | Current | Target | Priority | Effort |
|---|---------|---------|--------|----------|--------|
| 21 | claude-flow Integration | N/A | 100% | P0 | 160h |
| 22 | SONA Routing (Rust FFI) | N/A | 100% | P0 | 160h |
| 23 | Advanced Caching | N/A | 100% | P1 | 96h |
| 24 | Kubernetes Operator | N/A | 100% | P1 | 104h |
| 25 | Final Compliance Polish | Various | 100% | P0 | 60h |

**Total Features:** 5 advanced features
**Compliance Impact:** 62/65 features at ≥80%

---

### 4.2 Module Changes

#### 4.2.1 claude-flow Integration

**NEW MODULES:**
- `erlmcp_sona_sup.erl` (140 lines)
- `erlmcp_sona_cache.erl` (280 lines) - Shared memory management
- `erlmcp_sona_router_nif.erl` (NIF wrapper, 120 lines)
- `erlmcp_sona_monitor.erl` (180 lines) - Rust process health

**Rust Crate:** `sona_router` (1500 lines Rust)
- Shared memory protocol
- Sub-millisecond routing
- Cache management

**Effort:** 160 hours (80 Erlang, 60 Rust, 20 integration)

**Performance Targets:**
- SONA path: < 0.05ms (cached reads)
- Cache hit rate: > 80%
- Fallback latency: 1-5ms (erlmcp MCP)

---

#### 4.2.2 SONA Routing (Rust FFI)

**Rust NIFs:**
- `route_request/1` - Zero-copy routing
- `cache_lookup/1` - Sub-microsecond cache lookup
- `cache_write/2` - Cache update

**Safety Protocols:**
1. Async NIFs (separate schedulers)
2. Timeout enforcement (Erlang side)
3. Crash isolation via monitoring
4. Fallback to pure Erlang on failure

**Effort:** 160 hours (40 Erlang, 80 Rust, 40 testing)

**Performance Targets:**
- NIF call overhead: < 0.01ms
- Cache lookup: < 0.02ms
- Total SONA path: < 0.05ms

---

#### 4.2.3 Advanced Caching

**NEW MODULES:**
- `erlmcp_cache_predictor.erl` (200 lines) - Predictive caching
- `erlmcp_semantic_cache.erl` (250 lines) - Semantic similarity caching

**Effort:** 96 hours (64 implementation, 32 tests)

---

#### 4.2.4 Kubernetes Operator

**Rust Operator:** (2000 lines Rust)
- CRD definition
- StatefulSet management
- Auto-scaling logic
- Health monitoring

**Helm Charts:**
- erlmcp deployment
- Mnesia cluster configuration
- Service definitions

**Effort:** 104 hours (60 Rust, 24 Helm, 20 testing)

---

### 4.3 Dependencies

**External:**
- Rustler (Rust NIF framework)
- kube-rs (Kubernetes operator framework)
- POSIX shared memory libraries

**Internal:**
- erlmcp_sona_sup → Rustler NIFs
- claude-flow → STDIO transport

---

### 4.4 Testing Coverage

| Test Type | Target | Details |
|-----------|--------|---------|
| **EUnit** | 90%+ | 45 new test files |
| **CT** | 30 new suites | claude-flow integration, K8s deployment |
| **Proper** | 20 properties | SONA invariants, cache correctness |
| **Stress** | 7-day test | Rust NIF stability, no crashes |
| **Compliance** | 100% | Full MCP spec certification |

---

### 4.5 Deployment Strategy

**Kubernetes:**
```yaml
apiVersion: erlmcp.io/v1
kind: ErlmcpCluster
spec:
  replicas: 5
  version: "3.0.0"
  sessionBackend: mnesia
  sona:
    enabled: true
    cacheSize: 1GB
```

**Rollout:**
- Week 1-4: Rust FFI implementation + testing
- Week 5-7: claude-flow integration
- Week 8-9: Kubernetes operator
- Week 10-11: Final testing + compliance certification
- Week 12: Release v3.0.0

---

### 4.6 Risk Mitigation

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Rust NIF crashes | Medium (35%) | Critical | Crash isolation, 7-day stress test, fallback |
| Shared memory corruption | Low (10%) | High | Checksums, validation, recovery |
| SONA latency not met | Medium (35%) | High | Profiling, caching layers, optimization |
| K8s complexity | Medium (25%) | Medium | Extensive testing, examples, documentation |

**Critical:** Rust stability requires 7-day chaos test with 0 crashes before production

---

### 4.7 Success Criteria

**Performance:**
- ✅ SONA path: < 0.05ms for cached reads
- ✅ Cache hit rate: > 80%
- ✅ Overall P50: ≤ 2ms
- ✅ Overall P95: ≤ 8ms

**Compliance:**
- ✅ 62/65 features at ≥80% (95%+ compliance)
- ✅ Security audit: 0 critical vulnerabilities
- ✅ Full MCP spec certification

**Stability:**
- ✅ Rust NIFs: 7-day test with 0 crashes
- ✅ K8s operator: Deploy 5-node cluster successfully

---

## Gantt Timeline & Critical Path

```
Phase 1: Performance Foundation (v2.2.0)
┌────────────────────────────────────────┐
│ Week 1-2: Schema Cache + jiffy         │ ← Critical Path
│ Week 3-4: Async Tools + OAuth          │ ← Critical Path
│ Week 5-6: SSE Polling + Testing        │
└────────────────────────────────────────┘
    ↓ Release v2.2.0

Phase 2: Feature Completeness (v2.3.0)
┌────────────────────────────────────────┐
│ Week 7-8: Tasks API                    │ ← Critical Path
│ Week 9-10: Sampling Implementation     │ ← Critical Path
│ Week 11-12: Elicitation + Completion   │
│ Week 13-14: Testing + Polish           │
└────────────────────────────────────────┘
    ↓ Release v2.3.0

Phase 3: Scale & Optimization (v2.4.0)
┌────────────────────────────────────────┐
│ Week 15-18: RuVector + Swarm           │ ← Critical Path (parallel)
│ Week 19-20: Distributed Sessions       │
│ Week 21-22: HTTP/2 + Pooling           │
│ Week 23-24: Performance Testing        │
└────────────────────────────────────────┘
    ↓ Release v2.4.0

Phase 4: Advanced Integration (v3.0.0)
┌────────────────────────────────────────┐
│ Week 25-28: Rust FFI + SONA            │ ← Critical Path
│ Week 29-31: claude-flow Integration    │ ← Critical Path
│ Week 32-33: K8s Operator               │
│ Week 34-35: Compliance Certification   │
│ Week 36-38: Final Testing + Release    │
└────────────────────────────────────────┘
    ↓ Release v3.0.0 (95%+ compliance)

CRITICAL PATH:
Week 1-2 → Week 3-4 → Week 7-8 → Week 9-10 → Week 15-18 → Week 25-28 → Week 29-31
(Schema) → (Async)  → (Tasks)  → (Sampling) → (RuVector) → (Rust FFI) → (claude-flow)
```

**Total Duration:** 30-38 weeks (7-9 months)

**Parallelization Opportunities:**
- Phase 1: jiffy migration || schema cache (Week 1-2)
- Phase 2: Elicitation || Completion || Roots (Week 11-12)
- Phase 3: RuVector || Swarm (Week 15-18)
- Phase 4: Advanced caching || K8s operator (Week 32-33)

---

## Resource Allocation

### Team Composition by Phase

**Phase 1 (Weeks 1-6):**
- 1.0 FTE Senior Erlang Engineer
- 0.5 FTE QA Engineer
- 0.25 FTE Technical Writer
- **Total:** 1.75 FTE

**Phase 2 (Weeks 7-14):**
- 1.0 FTE Senior Erlang Engineer
- 0.5 FTE QA Engineer
- 0.25 FTE Technical Writer
- **Total:** 1.75 FTE

**Phase 3 (Weeks 15-24):**
- 1.0 FTE Senior Erlang Engineer
- 0.5 FTE Distributed Systems Engineer
- 0.5 FTE Performance Engineer
- 1.0 FTE QA Engineer
- **Total:** 3.0 FTE

**Phase 4 (Weeks 25-38):**
- 1.0 FTE Senior Erlang Engineer
- 1.0 FTE Rust Engineer
- 0.5 FTE DevOps Engineer (Kubernetes)
- 1.0 FTE QA Engineer
- 0.5 FTE Technical Writer
- **Total:** 4.0 FTE

**Average FTE:** 2.5 FTE across all phases

---

### Budget Allocation

| Phase | Labor (hours) | Infrastructure | External Services | Total |
|-------|--------------|----------------|-------------------|-------|
| Phase 1 | 256h × $150 = $38,400 | $2,000 | $1,000 | $41,400 |
| Phase 2 | 308h × $150 = $46,200 | $2,500 | $1,500 | $50,200 |
| Phase 3 | 528h × $150 = $79,200 | $5,000 | $3,000 | $87,200 |
| Phase 4 | 580h × $150 = $87,000 | $6,000 | $4,000 | $97,000 |
| **Total** | **1,712h = $256,800** | **$15,500** | **$9,500** | **$281,800** |

**Infrastructure:**
- CI/CD runners (GitHub Actions)
- Performance benchmarking servers (32-core)
- Kubernetes test cluster (5 nodes)
- Observability stack (Jaeger + Prometheus + Grafana)

**External Services:**
- Security audit (Phase 4): $3,000
- MCP compliance certification: $4,000
- LLM API credits (testing): $2,500

---

## Risk Management Matrix

### Overall Risk Score

| Phase | Risk Level | Top 3 Risks | Mitigation Effort |
|-------|-----------|-------------|-------------------|
| Phase 1 | Low-Medium | jiffy crashes, pool exhaustion, OAuth breaking | 24h contingency |
| Phase 2 | Medium | LLM provider changes, streaming complexity, task memory leak | 40h contingency |
| Phase 3 | Medium-High | ONNX NIF crashes, Raft split-brain, performance regression | 60h contingency |
| Phase 4 | High | Rust NIF crashes, SONA latency, shared memory corruption | 80h contingency |

**Total Contingency:** 204 hours (12% of total effort)

---

### Risk Heat Map

```
Impact ↑
  │
H │        [Phase 4: Rust NIFs]
I │        [Phase 3: ONNX NIFs]
G │   [Phase 2: Streaming]
H │   [Phase 1: OAuth Breaking]
  │
M │   [Phase 2: Task Leak]
E │   [Phase 3: Raft Split-Brain]
D │   [Phase 1: Pool Exhaustion]
  │
L │   [Phase 1: Schema Cache]
O │   [Phase 2: Elicitation]
W │   [Phase 3: HTTP/2]
  │
  └────────────────────────────────→ Probability
      LOW    MEDIUM    HIGH
```

---

### Mitigation Strategies

**High-Risk Mitigations:**

1. **Rust NIF Stability (Phase 4):**
   - **Before Production:** 7-day chaos test with 0 crashes
   - **Deployment:** Gradual rollout (10% → 50% → 100%)
   - **Fallback:** Feature flag to disable SONA routing
   - **Monitoring:** Real-time crash detection, auto-rollback

2. **Performance Regression (All Phases):**
   - **CI Integration:** Benchmark on every PR
   - **Alerting:** Regression > 10% blocks merge
   - **Rollback:** Feature flags for all optimizations

3. **Breaking Changes (Phase 1 OAuth):**
   - **Migration Guide:** Auto-migration script provided
   - **Dual Mode:** Support old + new config for 2 releases (6 months)
   - **Deprecation Warnings:** Logged on startup

---

## Success Criteria & Quality Gates

### Per-Phase Gates

**Phase 1 (v2.2.0):**
```
├── Compile: errors = 0 ✓
├── EUnit: failures = 0, coverage ≥ 85% ✓
├── CT: failures = 0 ✓
├── Dialyzer: warnings → 0 ✓
├── Xref: undefined = ∅ ✓
├── Performance:
│   ├── Schema validation P95 < 5ms ✓
│   ├── JSON encoding P95 < 1ms ✓
│   └── Tool concurrency ≥ 10 ✓
├── Compliance: 75% (49/65 features) ✓
└── Regression: < 5% ✓
```

**Phase 2 (v2.3.0):**
```
├── All Phase 1 gates maintained ✓
├── New features:
│   ├── Tasks API: 100% spec compliant ✓
│   ├── Sampling: Streaming functional ✓
│   ├── Elicitation: All modes working ✓
│   └── Completion: Full coverage ✓
├── Coverage: ≥ 85% overall, ≥ 90% core ✓
├── Compliance: 90% (58/65 features) ✓
└── Test count: 400+ files ✓
```

**Phase 3 (v2.4.0):**
```
├── All Phase 1-2 gates maintained ✓
├── Performance:
│   ├── RuVector overhead < 3ms P95 ✓
│   ├── Swarm election < 2s ✓
│   ├── Mnesia replication < 100ms ✓
│   └── Server pooling 8-10x throughput ✓
├── Scalability:
│   ├── 5-node cluster operational ✓
│   ├── 200K connections/node ✓
│   └── Automatic failover ✓
├── Compliance: 93% (60/65 features) ✓
└── Chaos: All scenarios pass ✓
```

**Phase 4 (v3.0.0):**
```
├── All Phase 1-3 gates maintained ✓
├── SONA Performance:
│   ├── Cached path < 0.05ms ✓
│   ├── Cache hit rate > 80% ✓
│   ├── NIF overhead < 0.01ms ✓
│   └── Overall P50 ≤ 2ms, P95 ≤ 8ms ✓
├── Stability:
│   ├── Rust NIFs: 7-day test, 0 crashes ✓
│   ├── K8s operator: 5-node deploy success ✓
│   └── Security audit: 0 critical issues ✓
├── Compliance:
│   ├── 95%+ (62/65 features) ✓
│   ├── Full MCP spec certification ✓
│   └── 100% method coverage ✓
├── Coverage: ≥ 85% overall, ≥ 90% core ✓
└── Documentation: 100% API coverage ✓
```

---

### Release Checklist

**v2.2.0 Release:**
- [ ] All quality gates pass
- [ ] Migration guide published
- [ ] Performance report generated
- [ ] Release notes complete
- [ ] Backward compatibility verified
- [ ] CI/CD green for 7 days
- [ ] Stakeholder approval

**v2.3.0 Release:**
- [ ] All quality gates pass
- [ ] Experimental features documented
- [ ] Compliance report (90%)
- [ ] Integration test suite complete
- [ ] Security review passed

**v2.4.0 Release:**
- [ ] All quality gates pass
- [ ] Distributed deployment guide
- [ ] Performance benchmarks published
- [ ] Chaos testing report
- [ ] Scaling best practices documented

**v3.0.0 Release:**
- [ ] All quality gates pass
- [ ] Full MCP compliance certification
- [ ] Rust FFI stability report (7-day test)
- [ ] K8s operator documentation
- [ ] Migration guide (v2.x → v3.0)
- [ ] Security audit report
- [ ] Final API documentation

---

## Migration & Deployment Strategy

### Backward Compatibility

**Breaking Changes Summary:**

| Phase | Breaking Change | Migration Path | Dual-Mode Support |
|-------|----------------|----------------|-------------------|
| Phase 1 | OAuth config format | Auto-migration script | 2 releases (6 months) |
| Phase 2 | None (all additions) | N/A | N/A |
| Phase 3 | None (opt-in features) | N/A | N/A |
| Phase 4 | None (feature flags) | N/A | N/A |

**Only 1 breaking change across all 4 phases**

---

### Deployment Paths

**Path 1: Gradual Upgrade (Recommended)**
```
v2.1.0 → v2.2.0 (4 weeks apart)
       → v2.3.0 (6 weeks apart)
       → v2.4.0 (8 weeks apart)
       → v3.0.0 (10 weeks apart)
```

**Path 2: Fast-Track (High-Risk)**
```
v2.1.0 → v2.3.0 (skip v2.2.0)
       → v3.0.0 (skip v2.4.0)
```
⚠️ **Not Recommended:** Skipping releases increases risk

**Path 3: Conservative (Production)**
```
v2.1.0 → v2.2.0 (test 4 weeks)
       → v2.2.0 (canary deploy 2 weeks)
       → v2.2.0 (full production 2 weeks)
       → v2.3.0 (repeat)
```

---

### Feature Flag Strategy

```erlang
%% Phase 1 Flags
{json_codec, jiffy}.              % Options: jiffy | jsx
{async_tools, true}.              % Default: false
{oauth_provider, openid}.         % Options: openid | basic

%% Phase 2 Flags
{experimental_tasks, true}.       % Default: false
{experimental_elicitation, true}. % Default: false
{sampling_streaming, true}.       % Default: false

%% Phase 3 Flags
{ruvector_enabled, true}.         % Default: false
{swarm_enabled, true}.            % Default: false
{server_pooling, true}.           % Default: false

%% Phase 4 Flags
{sona_enabled, true}.             % Default: false
{advanced_caching, true}.         % Default: false
```

**Rollback:** All features can be disabled via config without code changes

---

### Database Migrations

**Mnesia Schema Migrations (Phase 3):**
```erlang
%% Migration v2.3.0 → v2.4.0
-spec migrate_to_distributed_sessions() -> ok.
migrate_to_distributed_sessions() ->
    %% 1. Copy ETS sessions to Mnesia
    Sessions = erlmcp_session_backend_ets:export_all(),

    %% 2. Create Mnesia table with replication
    mnesia:create_table(session, [
        {disc_copies, [node() | nodes()]},
        {attributes, record_info(fields, session)}
    ]),

    %% 3. Import sessions
    lists:foreach(fun(Session) ->
        mnesia:dirty_write(session, Session)
    end, Sessions),

    %% 4. Switch backend
    application:set_env(erlmcp_core, session_backend, mnesia),
    ok.
```

**Rollback:**
```erlang
%% Rollback to ETS
mnesia:clear_table(session),
application:set_env(erlmcp_core, session_backend, ets).
```

---

## Appendices

### A. Module Inventory

**NEW Modules by Phase:**

**Phase 1:** 6 new modules
- erlmcp_schema_cache.erl
- erlmcp_json_codec.erl
- erlmcp_json_codec_jiffy.erl
- erlmcp_json_codec_jsx.erl
- erlmcp_tool_executor_sup.erl
- erlmcp_tool_worker.erl

**Phase 2:** 7 new modules
- erlmcp_task_manager.erl
- erlmcp_task_worker.erl
- erlmcp_sampling_stream_sup.erl
- erlmcp_sampling_stream_worker.erl
- erlmcp_elicitation.erl
- (+ enhancements to existing modules)

**Phase 3:** 14 new modules
- erlmcp_ruvector_sup.erl
- erlmcp_ruvector_index.erl
- erlmcp_ruvector_embeddings.erl
- erlmcp_ruvector_router.erl
- erlmcp_swarm_sup.erl
- erlmcp_swarm_coordinator.erl
- erlmcp_swarm_router.erl
- erlmcp_swarm_gossip.erl
- erlmcp_server_pool_sup.erl
- erlmcp_server_router.erl
- erlmcp_transport_http_pool_sup.erl
- erlmcp_sse_event_store.erl
- (+ 2 more)

**Phase 4:** 4 new modules + Rust crates
- erlmcp_sona_sup.erl
- erlmcp_sona_cache.erl
- erlmcp_sona_router_nif.erl
- erlmcp_sona_monitor.erl
- sona_router (Rust crate)
- erlmcp-operator (Rust crate)

**Total:** 164 → 195+ modules (19% increase)

---

### B. Performance Baselines vs Targets

| Metric | v2.1.0 Baseline | v2.2.0 | v2.3.0 | v2.4.0 | v3.0.0 Target |
|--------|----------------|--------|--------|--------|---------------|
| **Latency (P50)** | 5ms | 2-3ms | 2ms | 2ms | 2ms |
| **Latency (P95)** | 20ms | 8-12ms | 10ms | 8ms | 8ms |
| **Latency (P99)** | 50ms | 25ms | 22ms | 20ms | 20ms |
| **Throughput** | 10K req/s | 20K | 30K | 80K | 100K req/s |
| **Connections** | 50K/node | 50K | 100K | 150K | 200K/node |
| **SONA Path** | N/A | N/A | N/A | N/A | <0.05ms |
| **Registry** | 553K msg/s | 553K+ | 553K+ | 580K+ | 600K+ msg/s |

---

### C. Compliance Progression

```
v2.1.0: 65% ████████████████████░░░░░░░░░░ 42/65
v2.2.0: 75% ██████████████████████░░░░░░░░ 49/65
v2.3.0: 90% ███████████████████████████░░░ 58/65
v2.4.0: 93% ████████████████████████████░░ 60/65
v3.0.0: 95% ████████████████████████████░░ 62/65
```

**Remaining Gaps (5%):**
- 3 experimental features intentionally partial (edge cases)
- No impact on production deployments

---

### D. Agent Assignment Matrix

**Phase 1 Agents:**
- erlang-researcher: Gap analysis, existing code review
- erlang-architect: Schema cache design, async tools design
- erlang-otp-developer: Implementation (schema cache, tool executor)
- erlang-test-engineer: EUnit, CT, Proper tests
- erlang-performance: jiffy migration, benchmarks
- code-reviewer: Chicago School TDD compliance
- verifier: Run test suites
- agent-11-coverage: Coverage analysis

**Phase 2 Agents:**
- erlang-otp-developer: Tasks API, sampling, elicitation
- erlang-test-engineer: Comprehensive test suites
- erlang-performance: Sampling latency benchmarks
- code-reviewer: API design review

**Phase 3 Agents:**
- erlang-architect: RuVector + Swarm architecture
- erlang-otp-developer: Distributed sessions
- erlang-performance: Throughput optimization
- erlang-test-engineer: Chaos testing

**Phase 4 Agents:**
- erlang-otp-developer: Erlang-side integration
- (External Rust engineer for NIFs)
- erlang-performance: SONA latency optimization
- erlang-github-ops: K8s operator deployment

---

### E. Documentation Deliverables

**Per-Phase Documentation:**

**Phase 1:**
- OAuth Migration Guide
- jiffy Codec Configuration Guide
- Async Tools Performance Tuning
- v2.2.0 Release Notes

**Phase 2:**
- Tasks API Guide
- Sampling Provider Integration Guide
- Elicitation Best Practices
- Completion Integration Guide
- v2.3.0 Release Notes

**Phase 3:**
- RuVector Integration Guide
- Swarm Deployment Guide
- Distributed Session Configuration
- Scaling Best Practices
- v2.4.0 Release Notes

**Phase 4:**
- claude-flow Integration Guide
- SONA Architecture Deep Dive
- Rust FFI Safety Guide
- Kubernetes Operator Guide
- Migration Guide (v2.x → v3.0)
- Full API Documentation
- v3.0.0 Release Notes

---

**END OF 4-PHASE IMPLEMENTATION PLAN**

**Next Steps:**
1. Stakeholder review and approval
2. Resource allocation
3. Phase 1 kickoff (Week 1)

**Document Authority:** This plan supersedes all previous implementation documents and serves as the definitive roadmap for erlmcp v2.1.0 → v3.0.0.

**Last Updated:** 2026-02-02
**Review Cycle:** After each phase completion
**Contact:** plan-designer agent
