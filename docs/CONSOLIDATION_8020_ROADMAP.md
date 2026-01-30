# erlmcp 80/20 Consolidation Roadmap
## Practical Implementation Plan for Production-Ready Simplification

**Version:** 1.0.0
**Date:** 2026-01-30
**Status:** Executive Summary
**Owner:** Plan Designer Agent

---

## Executive Summary

This roadmap provides a **concrete, phased approach** to consolidate erlmcp from its current complex state (297 modules, 3 apps, 68 broken files) to a streamlined 80/20 architecture that delivers 80% of value with 20% of complexity.

**Current State Assessment:**
- ✅ **Core Strength:** 297 active modules, robust transport layer, observability stack
- ⚠️ **Technical Debt:** 68 broken/backup files, duplicate implementations, legacy migrations
- ✅ **Recent Wins:** MCP 2025-11-25 compliance, GCP integration, TCPS certification
- ⚠️ **Complexity:** Multiple rate limiter versions, redundant validators, scattered test files

**Target State (80/20):**
- 🎯 **50% code reduction:** 297 → ~150 modules (remove duplicates, consolidate patterns)
- 🎯 **Zero broken files:** All 68 .broken files resolved or archived
- 🎯 **Single source of truth:** One implementation per concern
- 🎯 **Maintainable architecture:** Clear separation, documented patterns
- 🎯 **Production-ready:** All quality gates passing, comprehensive tests

**Business Impact:**
- 50% faster onboarding for new developers
- 70% reduction in cognitive load for maintenance
- 40% faster build/test cycles
- Zero production incidents from deprecated code paths

---

## Phase 1: Foundation & Quick Wins (Weeks 1-2)

**Goal:** Eliminate obvious technical debt and establish consolidation patterns.

### 1.1 Broken File Cleanup (Days 1-3)

**Scope:** Resolve all 68 `.broken` and `.bak` files

**Action Items:**
```bash
# Audit all broken files
find . -name "*.broken" -o -name "*.bak" > /tmp/broken_files.txt

# Categorize by action:
# - KEEP: Needed functionality, fix and restore
# - ARCHIVE: Historical reference, move to attic/
# - DELETE: Obsolete, remove entirely
```

**File Categories:**

#### A. Restore & Fix (Priority: HIGH)
- `erlmcp_cache.erl.broken` → **RESTORE** (core caching needed)
- `erlmcp_state_migration.erl.broken` → **MERGE** into existing modules
- `erlmcp_rate_limiter_v2.erl.broken` → **DELETE** (v1 is sufficient)
- `erlmcp_prompt_argument_validator.erl.broken` → **MERGE** into tool validation
- `erlmcp_schema_validator.erl.broken` → **RESTORE** (MCP spec validation)
- `erlmcp_uri_validator.erl.broken` → **MERGE** into transport validation

#### B. Archive to attic/ (Priority: MEDIUM)
- All test files: `test/*_tests.erl.broken` → `attic/tests/legacy/`
- Documentation: `*.md.broken` → `attic/docs/legacy/`

#### C. Delete Entirely (Priority: LOW)
- Duplicate implementations superseded by production code
- Experimental features that didn't work out

**Risk Assessment:**
- 🟢 **LOW RISK:** Files already disabled (`.broken` extension)
- 🟡 **MEDIUM:** Restoring cache may require testing effort
- 🟢 **MITIGATION:** All changes behind feature flags, can rollback

**Validation Metrics:**
- ✅ 0 `.broken` files in `src/` directories
- ✅ All archived files have README explaining why archived
- ✅ Compilation succeeds (0 errors)
- ✅ Test suite passes (100% pass rate)

**Rollback Plan:**
```bash
# If critical functionality breaks
git revert <commit-hash>
# Restore from attic/ if needed
cp attic/src/erlmcp_cache.erl.broken apps/erlmcp_core/src/
```

---

### 1.2 Duplicate Implementation Consolidation (Days 4-7)

**Scope:** Merge duplicate/legacy implementations into single source of truth

#### A. Rate Limiter Consolidation

**Current State:**
- `erlmcp_rate_limiter.erl` (active, working)
- `erlmcp_rate_limiter_v2.erl.broken` (incomplete duplicate)

**Action:**
```erlang
% Keep: erlmcp_rate_limiter.erl
% Delete: erlmcp_rate_limiter_v2.erl.broken
% Document: Why v1 is sufficient for current requirements
```

**Migration Path:**
- No migration needed (v2 never activated)
- Document v1's capabilities and limitations
- Create feature request ticket for future enhancements

#### B. Validator Consolidation

**Current State:**
- `erlmcp_prompt_argument_validator.erl.broken` (prompt validation)
- `erlmcp_schema_validator.erl.broken` (JSON Schema validation)
- `erlmcp_uri_validator.erl.broken` (URI validation)
- `erlmcp_transport_validation.erl` (active, transport-level)

**Consolidated Design:**
```erlang
% Single module: erlmcp_validator.erl
% Exports:
%   - validate_prompt(Prompt) -> ok | {error, Reason}
%   - validate_schema(Data, Schema) -> ok | {error, Reason}
%   - validate_uri(URI) -> ok | {error, Reason}
%   - validate_transport(Msg) -> ok | {error, Reason}

% Internals:
%   - Reuse existing transport_validation logic
%   - Add prompt/schema/URI validation as separate functions
%   - Use jesse for JSON Schema (already a dependency)
```

**Risk Assessment:**
- 🟡 **MEDIUM RISK:** Consolidating validation logic
- 🟢 **MITIGATION:** Extensive test coverage before merging
- 🟢 **MITIGATION:** Feature flags to toggle old/new implementations

**Validation Metrics:**
- ✅ All validation tests pass with consolidated module
- ✅ No regression in transport validation
- ✅ New prompt/schema/URI validation tests added
- ✅ Coverage >= 80% on consolidated module

---

### 1.3 State Migration Simplification (Days 8-10)

**Current State:**
- `erlmcp_state_migration.erl.broken` (standalone migration module)
- Individual `code_change/3` implementations in each gen_server
- STATE_MIGRATION_GUIDE.md documents the approach

**Simplified Design:**

**Option A: Decentralized (RECOMMENDED)**
```erlang
% Keep code_change/3 in each gen_server
% Pros: Each module owns its migration logic
% Pros: No single point of failure
% Cons: Some code duplication

% Standard pattern in each module:
code_change(OldVsn, State, Extra) ->
    logger:info("~p: Code change from ~p", [?MODULE, OldVsn]),
    NewState = migrate_state(OldVsn, State, Extra),
    {ok, NewState}.

migrate_state(v1, #state{version = v1} = State, _Extra) ->
    State#state{version = v2};
migrate_state(v2, #state{version = v2} = State, _Extra) ->
    State#state{version = v3}.
```

**Option B: Centralized (NOT RECOMMENDED)**
```erlang
% Create erlmcp_state_migration.erl with helpers
% Pros: Centralized migration logic
% Cons: Single point of failure
% Cons: Complex dependency graph

% Don't do this - adds complexity for minimal benefit
```

**Decision:** Stick with **Option A (Decentralized)**
- Simpler, more robust
- Follows OTP best practices
- Each gen_server owns its state

**Action:**
- Delete `erlmcp_state_migration.erl.broken`
- Keep existing `code_change/3` implementations
- Document pattern in docs/otp-patterns.md

---

### 1.4 Establish Consolidation Patterns (Days 11-14)

**Goal:** Document patterns for remaining consolidation work

**Pattern Catalog:**

#### Pattern 1: Feature Flag Rollout
```erlang
% In application env:
{erlmcp, [
    {consolidated_cache, true},  % Feature flag
    {consolidated_validator, true}
]}

% In code:
case application:get_env(erlmcp, consolidated_cache, false) of
    true -> erlmcp_cache_v2:get(Key);
    false -> erlmcp_cache_v1:get(Key)
end.
```

#### Pattern 2: Adapter Pattern for Compatibility
```erlang
% Old module -> thin wrapper -> new consolidated module
-module(erlmcp_cache_v1).
-export([get/1, set/2]).

get(Key) ->
    erlmcp_cache:get(Key).  % Delegate to consolidated

set(Key, Value) ->
    erlmcp_cache:set(Key, Value).
```

#### Pattern 3: Telemetry for Migration Monitoring
```erlang
% Track which code paths are used
otel_counter:add(
    'erlmcp.consolidation.code_path',
    1,
    #{module => 'cache', version => 'v2'}
).

% Dashboard shows adoption:
% - cache_v1: 5% (stragglers)
% - cache_v2: 95% (consolidated)
```

**Documentation:**
- Create `docs/CONSOLIDATION_PATTERNS.md`
- Include code examples for each pattern
- Document testing strategies

---

## Phase 2: Core Module Consolidation (Weeks 3-5)

**Goal:** Consolidate core functional areas into single, well-tested modules.

### 2.1 Caching Consolidation (Week 3)

**Current State:**
- `erlmcp_cache.erl.broken` (broken, needs restoration)
- `erlmcp_icon_cache.erl` (specific to icons)
- Potential ad-hoc caching in other modules

**Target Design:**

```erlang
%% Single, general-purpose cache module
-module(erlmcp_cache).
-behaviour(gen_server).

%% API
-export([start_link/0, get/1, set/2, delete/1, clear/0]).
-export([get_stats/0, get_ttl/1, set_ttl/2]).

%% Generic cache interface
-spec get(Key :: term()) -> {ok, Value :: term()} | {error, not_found}.
-spec set(Key :: term(), Value :: term()) -> ok.
-spec delete(Key :: term()) -> ok.
-spec clear() -> ok.

%% Stats and monitoring
-spec get_stats() -> #{hit => integer(), miss => integer()}.

%% TTL management
-spec get_ttl(Key :: term()) -> {ok, Seconds :: integer()} | {error, not_found}.
-spec set_ttl(Key :: term(), Seconds :: integer()) -> ok.

%% Internal state
-record(state, {
    table :: ets:tid(),
    ttl_table :: ets:tid(),
    stats :: #{hit => integer(), miss => integer()}
}).

%% Implementation:
%% - ETS table for cache entries
%% - Separate ETS table for TTL tracking
%% - Built-in stats (hit/miss ratio)
%% - Optional TTL support
%% - O(1) get/set operations
```

**Migration Strategy:**
1. Restore `erlmcp_cache.erl.broken` and fix issues
2. Add TTL support and stats
3. Replace `erlmcp_icon_cache.erl` with generic cache
4. Add telemetry hooks for monitoring

**Risk Assessment:**
- 🟡 **MEDIUM RISK:** Cache is critical path
- 🟢 **MITIGATION:** Parallel deployment (old + new)
- 🟢 **MITIGATION:** Feature flag to toggle
- 🟢 **MITIGATION:** Extensive performance testing

**Validation Metrics:**
- ✅ Cache hit rate >= 90% (baseline)
- ✅ Latency p99 < 1ms (get operation)
- ✅ No memory leaks (24h soak test)
- ✅ Coverage >= 85%
- ✅ Performance tests pass (100K ops/sec)

**Rollout Strategy:**
```bash
# Week 3: Deploy with feature flag off
{erlmcp, [{consolidated_cache, false}]}

# Week 4: Enable for 10% of traffic
{erlmcp, [{consolidated_cache, true}, {cache_rollout_pct, 10}]}

# Week 5: Ramp to 100%
{erlmcp, [{consolidated_cache, true}, {cache_rollout_pct, 100}]}

# Week 6: Remove old code
git rm erlmcp_icon_cache.erl
```

---

### 2.2 Session Management Consolidation (Week 4)

**Current State:**
- `erlmcp_session.erl` (active)
- `erlmcp_session_manager.erl` (active)
- `erlmcp_session_failover.erl` (clustering)
- `erlmcp_session_replicator.erl` (clustering)

**Analysis:**
- `erlmcp_session.erl` - Individual session record/process
- `erlmcp_session_manager.erl` - Session lifecycle management
- `erlmcp_session_failover.erl` - Failover logic (cluster-specific)
- `erlmcp_session_replicator.erl` - Replication logic (cluster-specific)

**Consolidation Decision:**
- ✅ **KEEP:** `erlmcp_session.erl` (core data structure)
- ✅ **KEEP:** `erlmcp_session_manager.erl` (lifecycle management)
- ✅ **KEEP:** `erlmcp_session_failover.erl` (cluster feature)
- ✅ **KEEP:** `erlmcp_session_replicator.erl` (cluster feature)
- 📝 **DOCUMENT:** Clear separation of concerns in module docs

**Action Items:**
1. Add comprehensive module documentation
2. Clarify dependencies between modules
3. Add integration tests for failover/replication
4. Document clustering requirements

**Risk Assessment:**
- 🟢 **LOW RISK:** No code changes, documentation only
- 🟢 **MITIGATION:** Tests verify current behavior

**Validation Metrics:**
- ✅ All session tests pass
- ✅ Clustering tests pass (if applicable)
- ✅ Module docs clearly explain responsibilities

---

### 2.3 Transport Layer Consolidation (Week 5)

**Current State:**
- `erlmcp_transport_stdio.erl` (stdin/stdout)
- `erlmcp_transport_tcp.erl` (TCP sockets)
- `erlmcp_transport_http.erl` (HTTP client)
- `erlmcp_transport_ws.erl` (WebSocket)
- `erlmcp_transport_sse.erl` (Server-Sent Events)
- `erlmcp_transport_http_server.erl` (HTTP server)
- `erlmcp_transport_pool.erl` (connection pooling)
- `erlmcp_transport_adapter.erl` (adapter layer)
- `erlmcp_transport_behavior.erl` (behavior definition)
- `erlmcp_transport_sup.erl` (supervisor)
- `erlmcp_transport_registry.erl` (transport registry)
- `erlmcp_transport_discovery.erl` (transport discovery)
- `erlmcp_transport_health.erl` (health checks)
- `erlmcp_transport_pipeline.erl` (message pipeline)
- `erlmcp_transport_validation.erl` (validation)
- `erlmcp_pool_strategy.erl` (pool strategies)
- `erlmcp_pool_manager.erl` (pool management)
- `erlmcp_security_headers.erl` (HTTP security)
- `erlmcp_http_header_validator.erl` (header validation)
- `erlmcp_origin_validator.erl` (CORS validation)
- `erlmcp_client_transport.erl` (client-side)

**Analysis:** 21 modules - good separation of concerns, some optimization possible

**Consolidation Opportunities:**

#### A. HTTP Module Consolidation
```erlang
% Merge into single HTTP transport module:
% - erlmcp_transport_http.erl (keep)
% - erlmcp_transport_http_server.erl (merge)
% - erlmcp_http_header_validator.erl (merge)
% - erlmcp_security_headers.erl (merge)

% Result: erlmcp_transport_http.erl with client + server
```

#### B. Pool Consolidation
```erlang
% Merge pool management:
% - erlmcp_transport_pool.erl (keep)
% - erlmcp_pool_strategy.erl (merge)
% - erlmcp_pool_manager.erl (merge)

% Result: Single erlmcp_pool.erl module
```

#### C. Validation Consolidation
```erlang
% Already addressed in Phase 1.2
% Single erlmcp_validator.erl for all validation
```

**Risk Assessment:**
- 🟡 **MEDIUM RISK:** Transport is critical infrastructure
- 🟢 **MITIGATION:** Thorough integration testing
- 🟢 **MITIGATION:** Gradual rollout with feature flags
- 🟢 **MITIGATION:** Performance benchmarks before/after

**Validation Metrics:**
- ✅ All transport tests pass
- ✅ No regression in throughput (基准: 43K msg/sec)
- ✅ No regression in latency (基准: p99 < 10ms)
- ✅ Coverage >= 80% on consolidated modules

**Rollout Strategy:**
```bash
# Week 5: Deploy consolidated HTTP transport
{erlmcp, [{consolidated_http, true}]}

# Week 6: Deploy consolidated pool
{erlmcp, [{consolidated_pool, true}]}

# Week 7: Remove deprecated modules
git rm erlmcp_transport_http_server.erl
git rm erlmcp_pool_strategy.erl
git rm erlmcp_pool_manager.erl
```

---

## Phase 3: Observability Simplification (Weeks 6-7)

**Goal:** Streamline observability stack without losing functionality.

### 3.1 Metrics Consolidation

**Current State:**
- `erlmcp_metrics.erl` (core metrics)
- `erlmcp_metrics_server.erl` (metrics server)
- `erlmcp_metrics_aggregator.erl` (aggregation)

**Analysis:** Good separation, keep as-is

**Action Items:**
- Add documentation for custom metrics
- Document aggregation strategy
- Add examples for common use cases

---

### 3.2 Tracing Consolidation

**Current State:**
- `erlmcp_tracing.erl` (core, in erlmcp_core)
- `erlmcp_otel.erl` (OpenTelemetry integration)
- `erlmcp_otel_jaeger.erl` (Jaeger exporter)
- `erlmcp_otel_datadog.erl` (Datadog exporter)
- `erlmcp_otel_honeycomb.erl` (Honeycomb exporter)
- `erlmcp_otel_middleware.erl` (middleware)
- `erlmcp_otel.erl` (duplicate in observability)

**Consolidation Opportunities:**

#### A. Duplicate erlmcp_tracing.erl
```erlang
% Two versions exist:
% - apps/erlmcp_core/src/erlmcp_tracing.erl
% - apps/erlmcp_observability/src/erlmcp_tracing.erl (maybe?)

% Solution: Keep observability version, remove core version
```

#### B. OTEL Exporter Consolidation
```erlang
% Create single exporter with pluggable backends:
-module(erlmcp_otel_exporter).
-export([export/2]).

% Backends: jaeger, datadog, honeycomb, otlp (standard)

export(Spans, #{backend := jaeger} = Config) ->
    erlmcp_otel_jaeger:export(Spans, Config);
export(Spans, #{backend := datadog} = Config) ->
    erlmcp_otel_datadog:export(Spans, Config);
export(Spans, #{backend := otlp} = Config) ->
    opentelemetry_exporter:export(Spans, Config).
```

**Risk Assessment:**
- 🟢 **LOW RISK:** Observability changes are non-blocking
- 🟢 **MITIGATION:** Feature flags for new exporters

**Validation Metrics:**
- ✅ All tracing tests pass
- ✅ No loss of trace data
- ✅ Exporter switching works correctly

---

### 3.3 Dashboard Consolidation

**Current State:**
- `erlmcp_dashboard_server.erl` (HTTP server)
- `erlmcp_dashboard_http_handler.erl` (HTTP handlers)

**Analysis:** Good separation, keep as-is

**Action Items:**
- Document dashboard API
- Add authentication/authorization
- Add rate limiting for dashboard endpoints

---

## Phase 4: Testing & Documentation (Weeks 8-9)

**Goal:** Ensure comprehensive test coverage and documentation.

### 4.1 Test Consolidation

**Current State:**
- 68 broken test files in `test/*.erl.broken`
- Active tests in `apps/*/test/`

**Action Items:**
1. Archive broken tests to `attic/tests/legacy/`
2. Identify which tests are still needed
3. Port needed tests to modern framework
4. Delete obsolete tests
5. Achieve 80%+ coverage target

**Test Categories:**

#### A. Keep & Modernize (Priority: HIGH)
- `erlmcp_json_rpc_proper_tests.erl.broken` → Port to PropEr
- `erlmcp_client_request_id_overflow_tests.erl` → Keep, integrate

#### B. Archive (Priority: LOW)
- Old capability tests (superseded by MCP compliance tests)
- Legacy integration tests (superseded by new suites)

#### C. Delete (Priority: LOW)
- `auto_fix_SUITE.erl.broken` (experimental)
- `regression_detection_SUITE.erl.broken` (experimental)

**Validation Metrics:**
- ✅ Test suite passes (100% pass rate)
- ✅ Coverage >= 80% (overall)
- ✅ No broken test files in `test/` directories

---

### 4.2 Documentation Consolidation

**Current State:**
- 100+ markdown files in `docs/`
- Some duplication (TCPS docs, multiple HOWTOs)
- Some outdated content

**Consolidation Strategy:**

#### A. Create Documentation Hierarchy
```
docs/
├── README.md (index)
├── getting-started/
│   ├── installation.md
│   ├── quick-start.md
│   └── examples.md
├── architecture/
│   ├── overview.md
│   ├── otp-patterns.md
│   └── mcp-protocol.md
├── api/
│   ├── client-api.md
│   ├── server-api.md
│   └── transport-api.md
├── operations/
│   ├── deployment.md
│   ├── monitoring.md
│   └── troubleshooting.md
├── tcps/ (Toyota Production System)
│   ├── overview.md
│   ├── andon-system.md
│   └── work-orders.md
└── attic/ (archived docs)
    ├── legacy-guides.md
    └── old-versions/
```

#### B. Content Audit
- Identify duplicate content
- Merge overlapping docs
- Archive historical docs
- Update cross-references

**Validation Metrics:**
- ✅ No duplicate documentation
- ✅ All docs linked from index
- ✅ All code examples tested
- ✅ API docs generated from specs

---

## Phase 5: Production Rollout (Weeks 10-12)

**Goal:** Deploy consolidated codebase to production with confidence.

### 5.1 Staging Validation (Week 10)

**Pre-Production Checklist:**
```bash
# 1. Full test suite
rebar3 compile
rebar3 eunit
rebar3 ct
rebar3 proper
rebar3 dialyzer
rebar3 xref

# 2. Performance benchmarks
make benchmark-quick
make benchmark-full

# 3. Load testing
make stress-test
make chaos-test

# 4. Documentation generation
rebar3 edoc

# 5. Release build
rebar3 release
```

**Staging Deployment:**
1. Deploy to staging environment
2. Run synthetic load tests
3. Monitor metrics for 24 hours
4. Validate all functionality

**Success Criteria:**
- ✅ All tests pass
- ✅ No performance regression (>10% degradation)
- ✅ No errors in logs
- ✅ Metrics within normal range

---

### 5.2 Production Rollout (Weeks 11-12)

**Rollout Strategy:**

#### A. Canary Deployment (Week 11)
```yaml
# Week 11: 10% canary
production:
  canary_pct: 10
  feature_flags:
    consolidated_cache: true
    consolidated_validator: true
    consolidated_http: true
    consolidated_pool: true

# Monitor:
# - Error rate (target: <0.1%)
# - Latency p99 (target: <10ms)
# - Throughput (target: >40K msg/sec)
# - Memory (target: <2GB per node)
```

#### B. Gradual Rollout (Week 12)
```yaml
# Day 1: 10% (canary)
# Day 2: 25%
# Day 3: 50%
# Day 4: 75%
# Day 5: 100%

# Automated rollback if:
# - Error rate >0.5%
# - Latency p99 >20ms
# - Memory >3GB
# - Any process crash
```

**Monitoring Dashboard:**
```
┌─────────────────────────────────────┐
│ erlmcp Consolidation Dashboard      │
├─────────────────────────────────────┤
│ Canary Status: ✅ PASSING (10%)     │
│ Error Rate: 0.01% ✅                │
│ Latency p99: 8.2ms ✅               │
│ Throughput: 45.2K msg/sec ✅        │
│ Memory: 1.8GB ✅                    │
│                                     │
│ Feature Flags:                      │
│ consolidated_cache: ON ✅           │
│ consolidated_validator: ON ✅        │
│ consolidated_http: ON ✅             │
│ consolidated_pool: ON ✅             │
│                                     │
│ Rollback: Available (0 errors)      │
└─────────────────────────────────────┘
```

**Rollback Plan:**
```bash
# If issues detected:
kubectl rollout undo deployment/erlmcp

# Or via feature flags:
{erlmcp, [
    {consolidated_cache, false},
    {consolidated_validator, false}
]}

# Investigate logs:
kubectl logs -f deployment/erlmcp
```

---

### 5.3 Post-Deployment Validation (Week 12)

**7-Day Soak Test:**
- Monitor all metrics continuously
- Run daily health checks
- Review error logs daily
- Validate performance benchmarks

**Success Criteria:**
- ✅ Zero production incidents
- ✅ All metrics stable for 7 days
- ✅ No customer-reported issues
- ✅ Performance within 10% of baseline

**Post-Mortem:**
- Document lessons learned
- Update runbooks
- Celebrate success 🎉

---

## Risk Register

### Critical Risks (🔴)

#### R1: Cache Consolidation Failure
- **Impact:** HIGH (cache is critical path)
- **Probability:** MEDIUM (complex logic)
- **Mitigation:**
  - Extensive testing before rollout
  - Feature flag for instant rollback
  - Parallel deployment (old + new)
  - Performance benchmarks

#### R2: Transport Regression
- **Impact:** HIGH (transport is core)
- **Probability:** MEDIUM (HTTP consolidation)
- **Mitigation:**
  - Integration test suite
  - Performance benchmarks (43K msg/sec baseline)
  - Gradual rollout (10% → 100%)
  - Real-time monitoring

---

### High Risks (🟡)

#### R3: Test Coverage Drop
- **Impact:** MEDIUM (confidence in changes)
- **Probability:** LOW (careful consolidation)
- **Mitigation:**
  - Coverage threshold (80% minimum)
  - Pre-commit hooks
  - CI/CD enforcement
  - Regular audits

#### R4: Documentation Gaps
- **Impact:** MEDIUM (onboarding/maintenance)
- **Probability:** MEDIUM ( lots of docs to consolidate)
- **Mitigation:**
  - Documentation audit in Phase 4
  - Technical writer review
  - User feedback loop
  - Regular updates

---

### Medium Risks (🟢)

#### R5: Feature Flag Complexity
- **Impact:** LOW (temporary complexity)
- **Probability:** HIGH (multiple flags)
- **Mitigation:**
  - Clear documentation of flags
  - Automated flag cleanup
  - Monitoring dashboard
  - Regular review

#### R6: Rollout Delays
- **Impact:** LOW (schedule slip)
- **Probability:** MEDIUM (unexpected issues)
- **Mitigation:**
  - Buffer time in schedule
  - Early staging deployment
  - Parallel work streams
  - Flexible timeline

---

## Validation Metrics Summary

### Phase 1: Foundation
- [ ] 0 `.broken` files in `src/` directories
- [ ] All archived files documented
- [ ] Compilation succeeds (0 errors)
- [ ] Test suite passes (100%)

### Phase 2: Core Consolidation
- [ ] Cache performance: hit rate >= 90%, p99 < 1ms
- [ ] No memory leaks (24h soak test)
- [ ] Transport throughput: >= 43K msg/sec
- [ ] Transport latency: p99 < 10ms
- [ ] Coverage >= 80% on consolidated modules

### Phase 3: Observability
- [ ] All tracing tests pass
- [ ] No loss of trace data
- [ ] Exporter switching works

### Phase 4: Testing & Docs
- [ ] Test suite passes (100%)
- [ ] Coverage >= 80% (overall)
- [ ] No duplicate documentation
- [ ] All code examples tested

### Phase 5: Production
- [ ] Staging validation passes
- [ ] Canary succeeds (10% traffic)
- [ ] Gradual rollout completes
- [ ] 7-day soak test passes
- [ ] Zero production incidents

---

## Backward Compatibility

### API Compatibility

**Guarantees:**
- ✅ All public APIs remain unchanged
- ✅ No breaking changes to client/server interfaces
- ✅ Transport behavior unchanged from user perspective
- ✅ Configuration files compatible

**Internal Changes:**
- ⚙️ Module consolidation (internal only)
- ⚙️ Feature flags for gradual rollout
- ⚙️ Deprecation warnings for old APIs (6-month notice)

### Migration Guide

**For Users:**
```erlang
% No changes required for basic usage
% All existing code continues to work

% Optional: Adopt consolidated APIs
% Old (still works):
erlmcp_icon_cache:get(IconId).

% New (recommended):
erlmcp_cache:get({icon, IconId}).
```

**For Contributors:**
```erlang
% Update imports:
-old: -include_lib("erlmcp/include/icon_cache.hrl").
+new: -include_lib("erlmcp/include/cache.hrl").

% Update function calls:
-old: erlmcp_prompt_argument_validator:validate(Prompt).
+new: erlmcp_validator:validate_prompt(Prompt).
```

### Deprecation Timeline

**Phase 1 (Weeks 1-2):**
- Mark old modules as `-deprecated()` in specs
- Add compiler warnings for deprecated usage
- Document migration path

**Phase 2-4 (Weeks 3-9):**
- Deprecated modules still functional
- Feature flags allow gradual migration
- Monitor usage of old vs new APIs

**Phase 5 (Weeks 10-12):**
- Remove deprecated modules in v2.2.0
- Provide compatibility shim (optional)
- Archive old code to `attic/`

---

## Success Criteria

### Quantitative Metrics

**Code Reduction:**
- Target: 50% reduction in module count (297 → ~150)
- Target: 30% reduction in lines of code
- Target: 0 `.broken` files in production

**Performance:**
- Target: No regression > 10% in throughput
- Target: No regression > 10% in latency
- Target: Memory usage stable or reduced

**Quality:**
- Target: Test coverage >= 80%
- Target: Dialyzer warnings = 0
- Target: Xref warnings = 0

**Reliability:**
- Target: 100% test pass rate
- Target: Zero production incidents
- Target: 7-day soak test passes

### Qualitative Outcomes

**Developer Experience:**
- Faster onboarding (50% reduction in learning curve)
- Clearer module responsibilities
- Better documentation
- Easier debugging

**Maintainability:**
- Single source of truth for each concern
- Clear separation of concerns
- Comprehensive test coverage
- Well-documented patterns

**Production Readiness:**
- Confidence in deployments
- Easy rollback procedures
- Comprehensive monitoring
- Well-understood failure modes

---

## Timeline Summary

| Phase | Duration | Deliverables | Risk |
|-------|----------|--------------|------|
| Phase 1: Foundation | 2 weeks | Clean codebase, established patterns | Low |
| Phase 2: Core Consolidation | 3 weeks | Unified cache, sessions, transports | Medium |
| Phase 3: Observability | 2 weeks | Streamlined metrics, tracing | Low |
| Phase 4: Testing & Docs | 2 weeks | 80% coverage, consolidated docs | Medium |
| Phase 5: Production Rollout | 3 weeks | Staging validation, canary, full rollout | Medium |

**Total Duration:** 12 weeks

**Critical Path:** Phase 2 (Core Consolidation) → Phase 5 (Production Rollout)

**Parallel Work:** Phase 3 (Observability) can run in parallel with Phase 2

**Buffer Time:** 1 week built into each phase for unexpected issues

---

## Next Steps

1. **Review & Approve Roadmap** (Week 1, Day 1-2)
   - Stakeholder review
   - Risk assessment validation
   - Resource allocation
   - Timeline confirmation

2. **Kickoff Phase 1** (Week 1, Day 3)
   - Create feature branches
   - Set up tracking board
   - Assign tasks
   - Establish daily standups

3. **Execute & Monitor** (Weeks 1-12)
   - Daily progress tracking
   - Weekly risk reviews
   - Bi-weekly stakeholder updates
   - Continuous validation

4. **Celebrate Success** (Week 12)
   - Production deployment complete
   - Metrics validated
   - Documentation published
   - Team retrospective

---

## Appendix A: File Inventory

### Files to Restore & Fix
- `apps/erlmcp_core/src/erlmcp_cache.erl.broken`
- `apps/erlmcp_core/src/erlmcp_schema_validator.erl.broken`

### Files to Consolidate
- `apps/erlmcp_core/src/erlmcp_rate_limiter_v2.erl.broken` → DELETE
- `apps/erlmcp_core/src/erlmcp_prompt_argument_validator.erl.broken` → MERGE
- `apps/erlmcp_core/src/erlmcp_uri_validator.erl.broken` → MERGE
- `apps/erlmcp_core/src/erlmcp_state_migration.erl.broken` → DELETE

### Files to Archive
- All `test/*_tests.erl.broken` → `attic/tests/legacy/`
- All duplicate documentation → `attic/docs/legacy/`

### Files to Keep (No Changes)
- Core transport modules (stdio, tcp, http, ws, sse)
- Observability stack (metrics, tracing, dashboard)
- Session management (session, session_manager, failover, replicator)

---

## Appendix B: Command Reference

### Build & Test
```bash
# Full build and test
rebar3 compile
rebar3 eunit
rebar3 ct
rebar3 proper
rebar3 dialyzer
rebar3 xref

# Quick check (compile + unit tests)
make check

# Full validation (all quality gates)
make validate
```

### Benchmarks
```bash
# Quick benchmarks (< 2 min)
make benchmark-quick

# Full benchmark suite (10-15 min)
make benchmark-full

# Specific benchmark
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).
```

### Release
```bash
# Build release
rebar3 release

# Run release
_build/default/rel/erlmcp/bin/erlmcp console

# Upgrade release
_build/default/rel/erlmcp/bin/erlmcp upgrade <version>
```

---

## Appendix C: Contact & Support

**Project Lead:** [Name]
**Tech Lead:** [Name]
**QA Lead:** [Name]
**DevOps Lead:** [Name]

**Communication:**
- Daily standups: 9:00 AM PST
- Weekly demo: Friday 4:00 PM PST
- Slack: #erlmcp-consolidation
- Issues: https://github.com/erlmcp/erlmcp/issues

**Escalation:**
- Technical blockers: Tech Lead
- Resource conflicts: Project Lead
- Production issues: On-call (PagerDuty)

---

**Document Status:** ✅ Ready for Review
**Last Updated:** 2026-01-30
**Version:** 1.0.0
