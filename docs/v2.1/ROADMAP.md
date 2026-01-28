# erlmcp v2.1 Feature Roadmap

**Status:** PLANNING
**Date:** 2026-01-27
**Based On:** v2.0.0 Implementation Report + v2 Risk Analysis

---

## Executive Summary

v2.1 focuses on **stability, performance, and developer experience** after the major v2.0 umbrella refactoring. This release addresses critical issues from v2.0, completes incomplete features, and adds high-value enhancements that strengthen erlmcp's position as the production-grade Erlang/OTP MCP SDK.

**Timeline:** 6-8 weeks
**Target Release:** March 2026
**Theme:** "Production-Ready Refinement"

---

## Top 5 Critical Risks from v2.0 (Prioritized for v2.1)

Based on `docs/v2/V2_DESIGN_INPUTS/v2_risks.md`:

### 1. R3: Inadequate Test Coverage for Edge Cases (HIGH/HIGH)
**Risk:** Deleting 177 modules exposed untested code paths in remaining 70 modules.
**Impact:** Production failures, data loss
**Mitigation:** Phase 1 addresses with comprehensive test migration

### 2. R1: Hidden Dependencies in Legacy Code (HIGH/MEDIUM)
**Risk:** Runtime crashes from dynamically referenced modules
**Impact:** Service unavailability
**Mitigation:** Phase 1 static analysis + integration tests

### 3. R6: API Compatibility Breaks (MEDIUM/HIGH)
**Risk:** Users relying on "optional" modules face breakage
**Impact:** User frustration, migration effort
**Mitigation:** Phase 1 fixes + Phase 4 deprecation audit

### 4. R4: Library Migration Breaking Changes (MEDIUM/MEDIUM)
**Risk:** gproc/gun/ranch/poolboy API differences break existing code
**Impact:** Refactoring required across callers
**Mitigation:** Phase 2 API wrapper pattern

### 5. R2: Performance Regression After Library Migration (LOW/HIGH)
**Risk:** Throughput drops >10% after library migration
**Impact:** Violates v2 performance targets
**Mitigation:** Phase 2 benchmark validation

---

## Known Issues from V2_IMPLEMENTATION_REPORT.md

### Critical (Block v2.1 Release)
1. ❌ **erlmcp_client:encode_capabilities/1 function_clause**
   - **Location:** `apps/erlmcp_core/src/erlmcp_client.erl:531`
   - **Symptom:** Client initialization fails with map-based capabilities
   - **Root Cause:** Function expects `#mcp_client_capabilities{}` record, receives map
   - **Fix:** Add clause for map format or convert at call site
   - **Priority:** P0 (blocks client usage)
   - **ETA:** 1 hour

### High Priority
2. ⚠️ **Test suite migration incomplete**
   - **Issue:** 73 legacy test suites in `.skip` state
   - **Impact:** Unknown edge case coverage
   - **Fix:** Migrate to umbrella structure (apps/*/test/)
   - **ETA:** 2-3 days

3. ⚠️ **Transport stubs incomplete**
   - **Issue:** WebSocket and SSE transports lack `init/1` callbacks
   - **Impact:** Dialyzer warnings, unusable transports
   - **Fix:** Implement full transport behavior
   - **ETA:** 1-2 days

### Medium Priority
4. **Stub implementations across codebase**
   - **Count:** 32 TODOs found in apps/ (grep analysis)
   - **Notable:**
     - `erlmcp_subscription.erl`: All functions stubbed (5 TODOs)
     - `erlmcp_task.erl`: All functions stubbed (5 TODOs)
     - `tcps_persistence.erl`: 15 stub functions for ontology/backup
   - **Fix:** Implement or mark as experimental
   - **ETA:** 5-7 days

5. **Documentation drift**
   - **Issue:** API docs reference old paths (single-app structure)
   - **Fix:** Regenerate edoc, update README.md
   - **ETA:** 1 day

---

## v2.1 Roadmap: 4 Phases

### Phase 1: Critical Fixes (Week 1-2) **MUST-HAVE**
**Goal:** Make v2.0 stable and production-ready
**Success Criteria:** All P0/P1 issues resolved, 80%+ test coverage

#### 1.1 Fix Client Capability Encoding (1 hour)
- **Task:** Add map format support to `erlmcp_client:encode_capabilities/1`
- **Files:** `apps/erlmcp_core/src/erlmcp_client.erl`
- **Tests:** Add property test for all capability input formats
- **Deliverable:** Client initialization works with maps and records

#### 1.2 Migrate Test Suites to Umbrella Structure (2-3 days)
- **Task:** Move 73 legacy test suites from `.skip` to `apps/*/test/`
- **Structure:**
  ```
  apps/erlmcp_core/test/           (20 suites)
  apps/erlmcp_observability/test/  (8 suites)
  apps/tcps_erlmcp/test/           (40 suites)
  apps/erlmcp_transports/test/     (5 suites)
  ```
- **Coverage Target:** 80%+ line coverage per app
- **Deliverable:** `rebar3 eunit` passes all suites

#### 1.3 Implement WebSocket/SSE Transport Behaviors (1-2 days)
- **Task:** Complete `erlmcp_transport_ws.erl` and `erlmcp_transport_sse.erl`
- **Behavior:** Implement `init/1`, `send/2`, `close/1` callbacks
- **Integration:** Add to transport registry
- **Tests:** Add transport behavior compliance tests
- **Deliverable:** All transports pass Dialyzer

#### 1.4 Static Dependency Analysis (4 hours)
- **Task:** Validate no hidden dependencies from deleted modules
- **Tools:** `rebar3 xref`, `rebar3 dialyzer`, grep for `apply/3`, `spawn/3`
- **Deliverable:** Zero `undef` errors in integration tests

#### 1.5 Integration Test Expansion (1 day)
- **Task:** Add MCP protocol conformance tests
- **Scenarios:**
  - Initialize → tools/list → tools/call → shutdown
  - Multi-transport parallel access (stdio + TCP + HTTP)
  - Supervisor restart recovery
- **Deliverable:** 15+ end-to-end scenarios passing

**Phase 1 Deliverables:**
- [x] All P0/P1 issues resolved
- [x] Test coverage ≥80% (measured)
- [x] Zero Dialyzer warnings
- [x] Zero xref undefined calls
- [x] Integration tests pass (15+ scenarios)

---

### Phase 2: Performance Optimization (Week 3-4) **HIGH-VALUE**
**Goal:** Validate library migration performance, optimize bottlenecks
**Success Criteria:** <10% regression vs. v1.5.0 baseline, identify 2+ optimization wins

#### 2.1 Baseline Benchmark Validation (2 hours)
- **Task:** Run v2.0 benchmarks vs. v1.5.0 baseline
- **Benchmarks:**
  - `erlmcp_bench_core_ops:run(<<"core_ops_100k">>)`
  - `erlmcp_bench_network_real:run(<<"tcp_sustained_10k">>)`
  - `erlmcp_bench_stress:run(<<"stress_30s_100k_ops">>)`
- **Baseline Targets (v1.5.0):**
  - Registry: 553K msg/s
  - Network I/O: 43K msg/s
  - Session: 242K msg/s
- **Deliverable:** Performance comparison report (JSON + markdown)

#### 2.2 Library Migration Performance Tuning (2 days)
- **Focus:** gproc, gun, ranch, poolboy integration
- **Tuning Levers:**
  - gproc pool size (default: 100)
  - gun HTTP/2 max_concurrent_streams (default: 100)
  - ranch max_connections (default: 1024)
  - poolboy pool size (default: 10)
- **Profiling:** `recon:proc_count(memory, 10)`, `recon:proc_window(reductions, 10, 1000)`
- **Target:** <10% regression on all benchmarks
- **Deliverable:** Tuned configuration + performance report

#### 2.3 Message Batching for High-Throughput Scenarios (3 days)
- **Feature:** Batch multiple MCP requests in single JSON-RPC call
- **API:**
  ```erlang
  erlmcp_client:with_batch(Client, fun(Batch) ->
      erlmcp_client:batch_call_tool(Batch, ToolId1, Args1),
      erlmcp_client:batch_call_tool(Batch, ToolId2, Args2),
      erlmcp_client:batch_call_tool(Batch, ToolId3, Args3)
  end).
  ```
- **Protocol:** Use JSON-RPC batch request format (array of requests)
- **Benefit:** Reduce syscall overhead, improve throughput by 2-3x
- **Tests:** Benchmark batch vs. serial requests (10K+ ops)
- **Deliverable:** Batching API + benchmark showing >2x speedup

#### 2.4 Request Pipelining for Latency Reduction (2 days)
- **Feature:** Send multiple requests without waiting for responses (HTTP/2 style)
- **Implementation:** Separate request_id correlation from synchronous reply
- **Benefit:** Reduce latency by overlapping network I/O
- **Tests:** Measure P99 latency improvement (target: 30% reduction)
- **Deliverable:** Pipelining option + latency benchmark

#### 2.5 Connection Pooling Improvements (1 day)
- **Enhancement:** Add per-transport connection pools via poolboy
- **Config:**
  ```erlang
  {erlmcp_transports, [
      {http_pool, #{size => 20, max_overflow => 10}},
      {tcp_pool, #{size => 50, max_overflow => 25}}
  ]}.
  ```
- **Benefit:** Reduce connection overhead, improve concurrent access
- **Tests:** Benchmark 1000 concurrent clients (target: 50K+ req/s)
- **Deliverable:** Pooled transports + concurrency benchmark

**Phase 2 Deliverables:**
- [x] Performance regression <10% vs. v1.5.0
- [x] Batching API with >2x throughput improvement
- [x] Pipelining with 30% latency reduction
- [x] Connection pooling for 50K+ concurrent req/s
- [x] Performance tuning guide (docs/)

---

### Phase 3: New Features (Week 5-6) **NICE-TO-HAVE**
**Goal:** Add high-value capabilities that differentiate erlmcp
**Success Criteria:** 3+ new features shipped, documented, tested

#### 3.1 Distributed Registry with gproc Global Mode (3 days)
- **Feature:** Multi-node MCP server registry
- **Implementation:** Use `gproc:reg({n, g, {mcp_server, ServerId}})` for global names
- **Benefit:** Horizontal scaling, failover across nodes
- **Config:**
  ```erlang
  {erlmcp_core, [
      {registry_mode, distributed}, % local | distributed
      {cluster_nodes, ['erlmcp1@host', 'erlmcp2@host']}
  ]}.
  ```
- **Tests:** Multi-node Common Test suite (3+ nodes)
- **Deliverable:** Distributed registry + clustering guide

#### 3.2 Hot Code Reload Safety (2 days)
- **Feature:** Graceful code upgrades without dropping connections
- **Implementation:**
  - Version-aware supervision trees
  - Suspend message handling during upgrade
  - Resume with state migration
- **API:** `erlmcp:upgrade(NewVersion)`
- **Tests:** Upgrade v2.0 → v2.1 without connection loss
- **Deliverable:** Hot reload support + upgrade guide

#### 3.3 Enhanced Observability with Distributed Tracing (2 days)
- **Feature:** Correlate traces across client/server/transport boundaries
- **Implementation:**
  - OpenTelemetry context propagation via MCP messages
  - Span links between request/response pairs
  - Distributed trace visualization
- **Benefit:** Debug latency issues across multi-service deployments
- **Tests:** Verify span context propagation through 3+ hops
- **Deliverable:** Distributed tracing + example dashboard

#### 3.4 Transport Auto-Discovery (1 day)
- **Feature:** Automatic transport selection based on environment
- **Logic:**
  - stdio if running in subprocess (no TTY)
  - TCP if `MCP_TCP_PORT` env var set
  - HTTP if `MCP_HTTP_URL` env var set
- **API:** `erlmcp_client:start_link(auto)`
- **Tests:** Verify correct transport selection in 4+ scenarios
- **Deliverable:** Auto-discovery + environment config guide

#### 3.5 Client SDK Generation from Server Definitions (3 days)
- **Feature:** Generate Erlang client code from MCP server tool/resource definitions
- **Implementation:**
  - Parse server capability response
  - Generate typed Erlang functions for each tool
  - Type specs from JSON Schema
- **Example:**
  ```erlang
  % Generated from weather server
  weather_client:get_forecast(City) ->
      erlmcp_client:call_tool(Client, <<"get_forecast">>, #{<<"city">> => City}).
  ```
- **Benefit:** Type safety, autocomplete, reduced boilerplate
- **Tests:** Generate client for 3+ example servers
- **Deliverable:** SDK generator + code generation guide

**Phase 3 Deliverables:**
- [x] Distributed registry with multi-node clustering
- [x] Hot code reload without connection loss
- [x] Distributed tracing with span propagation
- [x] Transport auto-discovery from environment
- [x] Client SDK generation from server definitions

---

### Phase 4: Developer Experience (Week 7-8) **POLISH**
**Goal:** Make erlmcp easier to adopt, debug, and maintain
**Success Criteria:** 5+ DX improvements, updated docs, community feedback

#### 4.1 Deprecation Audit and API Stability Contract (1 day)
- **Task:** Identify all public APIs, classify stability
- **Categories:**
  - **Stable:** Won't change in v2.x (e.g., `erlmcp_client:call_tool/3`)
  - **Experimental:** May change (e.g., `erlmcp_subscription:*`)
  - **Deprecated:** Will be removed in v3.0 (none yet)
- **Deliverable:** `docs/API_STABILITY.md` + deprecation warnings

#### 4.2 Migration Guide from v1.x to v2.x (1 day)
- **Content:**
  - Breaking changes (TCPS separation, umbrella structure)
  - Configuration migration (single-app → multi-app)
  - API updates (client capability encoding)
  - Deployment changes (Docker, releases)
- **Examples:** Before/after code snippets
- **Deliverable:** `docs/MIGRATION_v1_to_v2.md`

#### 4.3 Improved Error Messages and Debugging (2 days)
- **Enhancement:** Structured error returns with context
- **Before:** `{error, not_initialized}`
- **After:** `{error, #{reason => not_initialized, phase => pre_initialization, hint => <<"Call initialize/2 first">>}}`
- **Benefit:** Reduce time-to-fix by 50%
- **Deliverable:** Error message improvements + debugging guide

#### 4.4 TCPS Integration Guide (1 day)
- **Content:**
  - When to use TCPS (quality gates, CI/CD, manufacturing workflows)
  - Setting up tcps_erlmcp application
  - Dashboard access, CLI usage
  - Integration with erlmcp_core observability
- **Examples:** CI/CD pipeline with TCPS quality gates
- **Deliverable:** `docs/tcps/INTEGRATION_GUIDE.md`

#### 4.5 Complete Stub Implementations or Mark Experimental (2 days)
- **Task:** Address 32 TODOs found in grep analysis
- **Priority Stubs:**
  - `erlmcp_subscription.erl`: Implement or mark experimental
  - `erlmcp_task.erl`: Implement or mark experimental
  - `tcps_persistence.erl`: Implement critical stubs (backup, verification)
- **Decision:** Implement if <1 day work, otherwise mark experimental
- **Deliverable:** Zero TODOs in stable APIs, experimental modules documented

#### 4.6 Comprehensive Examples and Tutorials (2 days)
- **Content:**
  - "Hello World" MCP server/client (5 minutes)
  - Calculator service with tools (15 minutes)
  - Weather API with resources and streaming (30 minutes)
  - Multi-node distributed setup (1 hour)
  - TCPS manufacturing workflow (1 hour)
- **Format:** Step-by-step markdown + working code in `examples/`
- **Deliverable:** 5+ tutorials + `docs/GETTING_STARTED.md`

#### 4.7 EdDoc API Documentation Regeneration (4 hours)
- **Task:** Generate HTML docs for all public APIs
- **Config:** Update `rebar.config` with edoc settings
- **Output:** `doc/` directory with per-module HTML
- **Publish:** GitHub Pages at `erlmcp.github.io/erlmcp`
- **Deliverable:** Published API docs

**Phase 4 Deliverables:**
- [x] API stability contract documented
- [x] Migration guide (v1 → v2)
- [x] Improved error messages
- [x] TCPS integration guide
- [x] Stub implementations complete or marked experimental
- [x] 5+ tutorials and examples
- [x] Published EdDoc API documentation

---

## v2.1 Feature Priority Matrix

```
                    IMPACT
            LOW         MEDIUM              HIGH
          +-----------------------------------------------+
VALUE     |                                               |
HIGH      | - SDK Gen    | - Hot Reload      | - Test Migration     |
          | - Auto-Disc  | - Dist Tracing    | - Client Fix         |
          |              | - Conn Pool       | - Benchmark Valid    |
          |              |                   | - Integration Tests  |
          +-----------------------------------------------+
MEDIUM    | - TCPS Guide | - Batching        | - WebSocket/SSE      |
          | - Tutorials  | - Pipelining      | - Stub Complete      |
          | - EdDocs     | - Dist Registry   | - Error Messages     |
          +-----------------------------------------------+
LOW       | - Deprecate  | - Migration Guide | - Static Analysis    |
          |              |                   | - API Stability      |
          +-----------------------------------------------+
```

**Legend:**
- **HIGH VALUE/HIGH IMPACT:** Phase 1 (critical fixes)
- **HIGH VALUE/MEDIUM IMPACT:** Phase 2 (performance)
- **MEDIUM VALUE/MEDIUM IMPACT:** Phase 3 (new features)
- **LOW-MEDIUM VALUE/LOW-MEDIUM IMPACT:** Phase 4 (DX)

---

## Top 20 Features Prioritized for v2.1 Sprint

| # | Feature | Phase | Priority | Effort | Impact | Dependencies |
|---|---------|-------|----------|--------|--------|--------------|
| 1 | Fix client capability encoding | 1.1 | P0 | 1h | Critical | None |
| 2 | Migrate test suites | 1.2 | P0 | 2-3d | Critical | None |
| 3 | Implement WebSocket/SSE | 1.3 | P0 | 1-2d | High | None |
| 4 | Static dependency analysis | 1.4 | P0 | 4h | High | None |
| 5 | Integration test expansion | 1.5 | P0 | 1d | High | #2 |
| 6 | Benchmark validation | 2.1 | P1 | 2h | High | #1-5 |
| 7 | Library migration tuning | 2.2 | P1 | 2d | High | #6 |
| 8 | Message batching API | 2.3 | P1 | 3d | High | #7 |
| 9 | Request pipelining | 2.4 | P1 | 2d | Medium | #8 |
| 10 | Connection pooling | 2.5 | P1 | 1d | Medium | #9 |
| 11 | Distributed registry | 3.1 | P2 | 3d | Medium | #1-5 |
| 12 | Hot code reload | 3.2 | P2 | 2d | Medium | #11 |
| 13 | Distributed tracing | 3.3 | P2 | 2d | Medium | #1-5 |
| 14 | Transport auto-discovery | 3.4 | P2 | 1d | Low | #3 |
| 15 | Client SDK generation | 3.5 | P2 | 3d | Medium | #1-5 |
| 16 | Deprecation audit | 4.1 | P3 | 1d | Low | #1-5 |
| 17 | Migration guide (v1→v2) | 4.2 | P3 | 1d | Medium | #16 |
| 18 | Improved error messages | 4.3 | P3 | 2d | Medium | #1-5 |
| 19 | Complete stub implementations | 4.5 | P3 | 2d | Medium | #1-5 |
| 20 | Comprehensive tutorials | 4.6 | P3 | 2d | Medium | #17 |

**Total Effort:** 6-8 weeks (with 2 engineers, can parallelize Phase 2-4)

---

## Success Metrics

### Stability (Phase 1)
- ✅ Zero P0/P1 issues open
- ✅ Test coverage ≥80% (measured with `rebar3 cover`)
- ✅ Zero Dialyzer warnings
- ✅ Zero xref undefined calls
- ✅ 15+ integration test scenarios passing

### Performance (Phase 2)
- ✅ Regression <10% vs. v1.5.0 baseline (all benchmarks)
- ✅ Batching shows >2x throughput improvement
- ✅ Pipelining reduces P99 latency by 30%
- ✅ Connection pooling supports 50K+ concurrent req/s
- ✅ Performance tuning guide published

### Features (Phase 3)
- ✅ 3+ new capabilities shipped (distributed registry, hot reload, tracing)
- ✅ All new features have tests with ≥85% coverage
- ✅ All new features documented with examples

### Developer Experience (Phase 4)
- ✅ API stability contract published
- ✅ Migration guide (v1→v2) complete
- ✅ 5+ tutorials and examples working
- ✅ EdDoc API docs published
- ✅ Stub implementations complete or marked experimental

---

## Risk Mitigation

### Risk: Phase 1 Takes Longer Than Expected (Likelihood: MEDIUM)
**Mitigation:**
- Parallelize tasks within Phase 1 (test migration + transport implementation)
- Cut Phase 4 features if timeline slips (DX is nice-to-have)
- Release v2.1-rc1 after Phase 1 completion

### Risk: Performance Regression >10% After Library Migration (Likelihood: LOW)
**Mitigation:**
- Run benchmarks early in Phase 2 (week 3, day 1)
- If regression detected, roll back library or tune aggressively
- Escalate to erlmcp core team if tuning insufficient

### Risk: Distributed Features (Registry, Tracing) Complex to Test (Likelihood: MEDIUM)
**Mitigation:**
- Use Common Test distributed mode (3+ nodes)
- Validate in staging environment before production
- Document known limitations (e.g., network partitions)

---

## Dependencies and External Factors

### Library Versions
- **gproc:** 0.9.0 (stable)
- **gun:** 2.0.1 (stable)
- **ranch:** 2.1.0 (stable)
- **poolboy:** 1.5.2 (stable)
- **opentelemetry_api:** 1.0.0+ (stable)

### Erlang/OTP
- **Required:** OTP 25+ (maps, records, distributed features)
- **Recommended:** OTP 26+ (JIT improvements)

### Community Feedback
- Gather feedback on v2.0 issues via GitHub discussions
- Prioritize community-requested features for Phase 3

---

## Next Steps

1. **Create Feature Proposals:** See `FEATURE_PROPOSALS.md` for detailed designs
2. **Assign Work:** Distribute top 20 features to erlmcp team
3. **Set Up Tracking:** Create GitHub milestones for v2.1 (Phases 1-4)
4. **Weekly Sync:** Review progress, adjust priorities, unblock issues

---

## References

### Source Documents
- **Risks:** `docs/v2/V2_DESIGN_INPUTS/v2_risks.md`
- **Implementation Report:** `docs/V2_IMPLEMENTATION_REPORT.md`
- **Principles:** `docs/v2/V2_DESIGN_INPUTS/v2_principles.md`
- **Benchmarks:** `bench/erlmcp_bench_*.erl`

### Related Documents
- **Feature Proposals:** `docs/v2.1/FEATURE_PROPOSALS.md` (detailed designs)
- **Effort Estimates:** `docs/v2.1/EFFORT_ESTIMATES.md` (time/resource breakdown)
- **API Stability:** `docs/v2.1/API_STABILITY.md` (to be created in Phase 4)

---

**Document Status:** PLANNING
**Last Updated:** 2026-01-27
**Owner:** erlmcp Core Team
**Next Review:** After v2.0.0 release
