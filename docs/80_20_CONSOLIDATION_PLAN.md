# 80/20 Consolidation Plan - erlmcp v2.1.0

**Executive Summary**: Analysis of 106 modules (35,549 LOC) reveals that **21 modules (20%) deliver 80% of production value**, while **~25 modules (bottom 20%) consume 80% of maintenance effort** through incomplete implementations, experimental features, and redundant abstractions.

**Status**: DRAFT - For Technical Review
**Date**: 2026-01-30
**Author**: Plan Designer Agent
**Context**: erlmcp v2.1.0 - Erlang/OTP MCP SDK

---

## Table of Contents

1. [Methodology](#methodology)
2. [The Vital 20%: Keep & Optimize](#the-vital-20-keep-optimize)
3. [The Costly 20%: Remove or Consolidate](#the-costly-20-remove-or-consolidate)
4. [The Middle 60%: Simplify & Standardize](#the-middle-60-simplify-standardize)
5. [Consolidation Roadmap](#consolidation-roadmap)
6. [Risk Assessment](#risk-assessment)

---

## Methodology

**Analysis Criteria**:
- **Value Delivered**: API surface area, direct user impact, protocol compliance
- **Maintenance Burden**: TODO count, complexity, experimental status, duplication
- **Usage Frequency**: Git commit frequency, integration depth, dependency count
- **Code Quality**: Type specs, test coverage, documentation, production readiness

**Metrics Collected**:
- 106 total modules (58 core, 21 transports, 27 observability)
- 35,549 lines of production code
- 52 test files (29,771 lines of tests)
- 24 unimplemented features marked TODO/FIXME
- 14 broken files (.broken extension)
- 18 files with technical debt markers

---

## The Vital 20%: Keep & Optimize

**Total: 21 modules (~8,500 LOC, 80% of production value)**

These modules are the **core value proposition** of erlmcp. They MUST be preserved, optimized, and fully tested.

### Tier 1: Critical Protocol Engine (9 modules, ~4,500 LOC)

**Keep. Optimize. Complete.**

| Module | LOC | Value | Action | Priority |
|--------|-----|-------|--------|----------|
| **erlmcp_client.erl** | 742 | HIGH | Optimize state machine, complete type specs | P0 |
| **erlmcp_server.erl** | 2,040 | HIGH | Split into 3 focused modules (see below) | P0 |
| **erlmcp_json_rpc.erl** | 469 | HIGH | Complete, excellent - add batch RFC compliance | P0 |
| **erlmcp_registry.erl** | 503 | HIGH | Complete, excellent - document gproc patterns | P0 |
| **erlmcp_capabilities.erl** | 1,253 | HIGH | Complete MCP 2025-11-25 compliance - keep | P0 |
| **erlmcp_transport_behavior.erl** | 819 | HIGH | Complete behavior - document patterns | P0 |
| **erlmcp_transport_stdio.erl** | 324 | HIGH | Complete - optimize buffer handling | P1 |
| **erlmcp_transport_tcp.erl** | 867 | HIGH | Complete - validate ranch pooling | P1 |
| **erlmcp_transport_http.erl** | ~300 | HIGH | Complete HTTP/2 via gun - document | P1 |

**Action Items**:
1. **Split erlmcp_server.erl (2,040 LOC)** into:
   - `erlmcp_server_core.erl` (600 LOC) - Resource/tool/prompt management
   - `erlmcp_server_protocol.erl` (500 LOC) - MCP protocol state machine
   - `erlmcp_server_handlers.erl` (400 LOC) - Notification/progress handlers
   - **Rationale**: Current module violates SRP, hard to test, 914 type specs (good but complex)

2. **Add missing type specs** to all public APIs (target: 100% coverage)

3. **Complete test coverage** (target: 90%+ for all Tier 1 modules)

### Tier 2: Essential Infrastructure (12 modules, ~4,000 LOC)

**Keep. Simplify. Standardize.**

| Module | LOC | Value | Action | Priority |
|--------|-----|-------|--------|----------|
| **erlmcp_rate_limiter.erl** | 874 | HIGH | Simplify to single algorithm (remove token bucket) | P0 |
| **erlmcp_auth.erl** | 604 | HIGH | Complete JWT/OAuth2 implementations (currently TODO) | P0 |
| **erlmcp_circuit_breaker.erl** | 685 | MED | Complete, excellent - document usage patterns | P1 |
| **erlmcp_batch.erl** | 485 | MED | Complete - optimize batching strategy | P1 |
| **erlmcp_hooks.erl** | 596 | MED | Complete - validate hook ordering guarantees | P1 |
| **erlmcp_session_manager.erl** | 381 | MED | Complete - add clustering support | P2 |
| **erlmcp_connection_monitor.erl** | 500 | MED | Complete - reduce false positives | P2 |
| **erlmcp_code_reload.erl** | 565 | LOW | Evaluate - consider removing if unused | P3 |
| **erlmcp_logging.erl** | 382 | MED | Complete - integrate with OTEL | P1 |
| **erlmcp_transport_ws.erl** | 724 | HIGH | Complete - document WebSocket lifecycle | P1 |
| **erlmcp_transport_sse.erl** | 639 | MED | Complete - validate event delivery ordering | P1 |
| **erlmcp_pool_manager.erl** | 579 | MED | Complete - document pool strategy selection | P1 |

**Action Items**:
1. **erlmcp_rate_limiter.erl**: Remove unused token bucket algorithm, keep only sliding window
2. **erlmcp_auth.erl**: Implement TODO items (JWT validation, OAuth2 introspection, mTLS)
3. **erlmcp_code_reload.erl**: If unused in 6 months, deprecate (high maintenance, low value)

---

## The Costly 20%: Remove or Consolidate

**Total: ~25 modules (~8,000 LOC, 80% of maintenance burden)**

These modules have **disproportionate maintenance cost** relative to value delivered.

### Category 1: Incomplete Implementations (9 modules, ~4,000 LOC)

**Status: BLOCKED on unimplemented features**

**Action: Complete within 30 days OR remove**

| Module | LOC | TODO Count | Blockers | Recommendation |
|--------|-----|------------|----------|----------------|
| erlmcp_icon_cache.erl | ~300 | 1 | "Implement icon caching with ETS/Mnesia" | **REMOVE** - Low value, edge case |
| erlmcp_resource_subscriptions.erl | ~400 | 4 | "Implement full resource subscription system" | **COMPLETE** - Core MCP feature |
| erlmcp_session_failover.erl | ~350 | 1 | "Implement failover with node monitoring" | **REMOVE** - Clustering premature |
| erlmcp_session_replicator.erl | ~350 | 1 | "Implement session replication with Mnesia" | **REMOVE** - Clustering premature |
| erlmcp_pricing_*.erl (11 modules) | ~4,000 | 6 | "Implement Vault/AWS Secrets Manager API calls" | **EVALUATE** - See below |
| erlmcp_secrets.erl | 486 | 3 | "Implement Vault API call", AWS Secrets Manager | **SIMPLIFY** - Use ENV vars only |
| erlmcp_progress.erl | 340 | 0 | Complete but rarely used | **DEPRECATE** - Move to example |

**Pricing Module Analysis (11 modules in `pricing/`)**:
- **erlmcp_pricing_receipt.erl** (747 LOC) - **KEEP** - Core to TCPS quality system
- **erlmcp_pricing_plan.erl** (479 LOC) - **KEEP** - SKU management
- **erlmcp_sla_monitor.erl** (414 LOC) - **KEEP** - Production monitoring
- **erlmcp_pricing_*.erl** (8 remaining) - **CONSOLIDATE** into single 500 LOC module

**Recommendation**: Consolidate 8 pricing modules into `erlmcp_pricing_engine.erl`:
- Remove CLI, HTTP, loader, validator, state, upgrade, util separate modules
- Single module with clear API: `init/1`, `verify_receipt/2`, `check_sla/2`, `update_sku/2`
- **Estimated savings**: 2,500 LOC → 500 LOC (80% reduction)

### Category 2: Experimental / Unused Features (8 modules, ~2,500 LOC)

**Status: HIGH RISK, LOW VALUE**

**Action: Move to examples/ or remove entirely**

| Module | LOC | Status | Recommendation |
|--------|-----|--------|----------------|
| erlmcp_chaos.erl | 762 | Experimental | **MOVE** to `examples/chaos_test.erl` |
| erlmcp_debugger.erl | 444 | Experimental | **REMOVE** - Use observer_cli/recon instead |
| erlmcp_profiler.erl | 476 | Incomplete (TODO) | **REMOVE** - Use fprof/eprof |
| erlmcp_evidence_path.erl | 479 | TCPS-specific | **MOVE** to tcps_erlmcp app |
| erlmcp_recovery_manager.erl | 582 | Incomplete (TODO) | **REMOVE** - Use supervisors |
| erlmcp_cpu_guard.erl | ~300 | Incomplete | **REMOVE** - Use OS quotas |
| erlmcp_memory_guard.erl | ~300 | Incomplete | **REMOVE** - Use OS limits |
| erlmcp_cpu_quota.erl | 360 | Incomplete | **REMOVE** - Use scheduler quotas |

**Rationale**:
- **Chaos testing**: Valuable but experimental, belongs in examples, not core
- **Debugging/profiling**: Existing OTP tools (observer, debugger, fprof) are superior
- **Resource guards**: OS-level limits are more reliable than application-level

### Category 3: Broken / Dead Code (14 files, ~3,000 LOC)

**Status: UNMAINTAINABLE**

**Action: DELETE IMMEDIATELY**

**All `.broken` files must be removed**:
```
apps/erlmcp_core/src/erlmcp_cache.erl.broken
apps/erlmcp_core/src/erlmcp_prompt_argument_validator.erl.broken
apps/erlmcp_core/src/erlmcp_rate_limiter_v2.erl.broken
apps/erlmcp_core/src/erlmcp_schema_validator.erl.broken
apps/erlmcp_core/src/erlmcp_state_migration.erl.broken
apps/erlmcp_core/src/erlmcp_uri_validator.erl.broken
apps/erlmcp_core/test/erlmcp_cancellation_tests.erl.broken
apps/erlmcp_core/test/erlmcp_code_reload_tests.erl.broken
apps/erlmcp_core/test/erlmcp_connection_limiter_tests.erl.broken
apps/erlmcp_core/test/erlmcp_json_rpc_proper_tests.erl.broken
apps/erlmcp_core/test/erlmcp_message_parser_tests.erl.broken
apps/erlmcp_core/test/erlmcp_progress_tests.erl.broken
apps/erlmcp_transports/src/erlmcp_connection_pool_worker.erl.broken
apps/erlmcp_transports/test/erlmcp_transport_compliance_tests.erl.broken
```

**Action**: `git rm` all `.broken` files - they create technical debt and confusion.

**Question**: Why are they in git?
- **Answer**: Failed experiments preserved for reference
- **Problem**: Creates noise, unclear what's production-ready
- **Solution**: Delete OR move to `attic/` directory with README explaining failure

### Category 4: Redundant Abstractions (6 modules, ~1,500 LOC)

**Status: PREMATURE OPTIMIZATION**

**Action: SIMPLIFY or REMOVE**

| Module | LOC | Redundancy | Recommendation |
|--------|-----|------------|----------------|
| erlmcp_transport_registry.erl | 546 | Duplicate of erlmcp_registry | **CONSOLIDATE** into erlmcp_registry |
| erlmcp_transport_discovery.erl | 588 | Unused feature | **REMOVE** - Use static config |
| erlmcp_transport_pipeline.erl | 389 | Over-engineered | **SIMPLIFY** - Direct function calls |
| erlmcp_transport_validation.erl | 397 | Duplicate of jesse | **REMOVE** - Use jesse directly |
| erlmcp_pool_strategy.erl | ~300 | Unused abstraction | **REMOVE** - Use poolboy directly |
| erlmcp_transport_adapter.erl | ~200 | Unnecessary wrapper | **REMOVE** - Direct gen_server calls |

**Estimated savings**: 1,500 LOC removed, 0 functional loss

---

## The Middle 60%: Simplify & Standardize

**Total: ~60 modules (~19,000 LOC)**

These modules have **moderate value** but need **standardization**.

### Standardization Opportunities

**1. Error Handling Patterns**

**Current State**: Inconsistent error handling across 18 files
```erlang
% Pattern 1: ad-hoc tuples
{error, invalid_id}
{error, overflow}

% Pattern 2: custom records
#refusal{reason = ...}

% Pattern 3: exceptions
exit(normal)
throw(bad_request)
```

**Recommendation**: Standardize on **well-typed error unions**
```erlang
-type error() :: {error, invalid_id | overflow | validation_failed | ...}.

% All functions return:
-spec foo() -> {ok, result()} | {error, error()}.
```

**Action**: Create `erlmcp_errors.hrl` with canonical error types

**2. Type Specification Standards**

**Current State**: 316 type specs in client, 914 in server, 211 in json_rpc
**Goal**: 100% type spec coverage for all public APIs

**Priority**:
1. All Tier 1 modules (P0)
2. All Tier 2 modules (P1)
3. All transport modules (P1)

**3. Documentation Standards**

**Current State**: Incomplete @doc coverage
**Goal**: Every public function has @doc with:
- Purpose (1 sentence)
- Parameters (with types)
- Return value (with types)
- Example usage
- Errors raised

**Example**:
```erlang
%% @doc Register an MCP server with the registry.
%%
%% @param ServerId Unique server identifier (atom or binary)
%% @param ServerPid Server process PID
%% @param Config Server capabilities and options
%%
%% @returns ok | {error, already_registered | invalid_config}
%%
%% @example
%%   {ok, Server} = erlmcp_server:start_link(my_server, #{}),
%%   ok = erlmcp_registry:register_server(my_server, Server,
%%       #{capabilities => #mcp_server_capabilities{}}).
%%
-spec register_server(server_id(), pid(), server_config()) -> ok | {error, term()}.
```

---

## Consolidation Roadmap

### Phase 1: Cleanup (Week 1-2) - IMMEDIATE

**Goal**: Remove dead code, reduce cognitive load

**Actions**:
1. **Delete all `.broken` files** (14 files)
2. **Move experimental modules to `examples/`**:
   - erlmcp_chaos.erl → examples/mcp_chaos_test.erl
   - erlmcp_debugger.erl → DELETE (use recon)
   - erlmcp_profiler.erl → DELETE (use fprof)
3. **Remove redundant abstractions** (6 modules):
   - erlmcp_transport_validation.erl → use jesse
   - erlmcp_pool_strategy.erl → use poolboy
   - erlmcp_transport_adapter.erl → direct calls

**Expected Result**: -3,000 LOC, -20 modules

**Quality Gates**:
- All tests pass
- No xref warnings
- No dialyzer regressions

### Phase 2: Complete Incomplete Features (Week 3-4)

**Goal**: Resolve TODOs or remove features

**Priority 1 (MUST Complete)**:
1. **erlmcp_auth.erl**: Implement JWT/OAuth2/mTLS
2. **erlmcp_resource_subscriptions.erl**: Complete subscription system
3. **erlmcp_secrets.erl**: Simplify to ENV-only, remove Vault/AWS TODOs

**Priority 2 (Complete or Remove)**:
1. **erlmcp_session_failover.erl**: Complete OR remove
2. **erlmcp_session_replicator.erl**: Complete OR remove
3. **erlmcp_icon_cache.erl**: Remove (edge case)

**Expected Result**: 0 TODOs in production code

**Quality Gates**:
- 0 `TODO` markers in production code
- 90%+ test coverage for completed features
- Integration tests pass

### Phase 3: Consolidate & Simplify (Week 5-6)

**Goal**: Reduce LOC while maintaining functionality

**Actions**:
1. **Split erlmcp_server.erl** (2,040 LOC → 3 modules × 500 LOC)
2. **Consolidate pricing modules** (11 modules → 2 modules)
3. **Merge transport registry** into erlmcp_registry
4. **Standardize error handling** (create erlmcp_errors.hrl)

**Expected Result**: -2,500 LOC, -8 modules

**Quality Gates**:
- All tests pass
- No regression in benchmarks
- Documentation updated

### Phase 4: Optimize Core (Week 7-8)

**Goal**: Maximize performance of vital 20%

**Actions**:
1. **Optimize erlmcp_client.erl** state machine
2. **Optimize erlmcp_json_rpc.erl** batch processing
3. **Add type specs** to all Tier 1 APIs
4. **Complete test coverage** (90%+ target)

**Expected Result**: 10-20% performance improvement

**Quality Gates**:
- Benchmarks show <10% regression (or improvement)
- 90%+ test coverage
- 0 dialyzer warnings in Tier 1 modules

### Phase 5: Standardize Documentation (Week 9-10)

**Goal**: Complete, consistent documentation

**Actions**:
1. **Add @doc to all public APIs** (Tier 1 + Tier 2)
2. **Create architecture diagrams** (update architecture.md)
3. **Write integration guides** for common patterns
4. **Document OTP patterns** (supervision trees, process lifecycle)

**Expected Result**: Production-ready documentation

**Quality Gates**:
- 100% @doc coverage for Tier 1 public APIs
- All examples tested and working
- User guide complete

---

## Risk Assessment

### High-Risk Changes

**1. Splitting erlmcp_server.erl**
- **Risk**: Breaking existing integrations
- **Mitigation**: Maintain backward-compatible API wrapper
- **Rollback**: Keep original file for 1 release

**2. Removing experimental features**
- **Risk**: Power users may depend on chaos testing
- **Mitigation**: Move to examples/ with clear deprecation notice
- **Rollback**: Git revert, feature flag

**3. Consolidating pricing modules**
- **Risk**: Breaking TCPS quality system
- **Mitigation**: Extensive testing, slow rollout
- **Rollback**: Branch per release

### Medium-Risk Changes

**1. Standardizing error handling**
- **Risk**: Breaking existing error handling code
- **Mitigation**: Gradual migration, backward-compatible shims
- **Rollback**: Revert erlmcp_errors.hrl

**2. Removing redundant abstractions**
- **Risk**: Tight coupling to removed modules
- **Mitigation**: Search codebase for references, update all
- **Rollback**: Git revert

### Low-Risk Changes

**1. Deleting .broken files**
- **Risk**: Someone wants to revive them
- **Mitigation**: Move to attic/ with README
- **Rollback**: Git revert

**2. Adding type specs**
- **Risk**: Dialyzer reveals existing bugs
- **Mitigation**: Fix bugs as found, document known issues
- **Rollback**: N/A (incremental improvement)

---

## Success Metrics

### Code Quality

**Before**:
- 106 modules, 35,549 LOC
- 24 TODOs
- 14 broken files
- ~60% test coverage
- ~70% type spec coverage

**After** (Target):
- 75 modules, 25,000 LOC (-30%)
- 0 TODOs
- 0 broken files
- 90%+ test coverage
- 100% type spec coverage (Tier 1 + Tier 2)

### Maintenance Burden

**Before**:
- 80% of time spent on bottom 20% of code
- High cognitive load (106 modules to understand)
- Frequent regressions in experimental features

**After** (Target):
- 20% of time on maintenance, 80% on features
- Low cognitive load (75 focused modules)
- Stable, production-ready codebase

### Developer Experience

**Before**:
- Unclear which modules are production-ready
- Inconsistent error handling
- Incomplete documentation
- Experimental features in core

**After** (Target):
- Clear module boundaries (core vs examples)
- Consistent error handling
- Complete documentation
- Experimental features isolated

---

## Next Steps

1. **Review this document** with technical leadership
2. **Prioritize phases** based on team capacity
3. **Create tracking tickets** for each action item
4. **Measure baseline metrics** (LOC, coverage, benchmarks)
5. **Execute Phase 1** (cleanup) - QUICK WIN
6. **Iterate** through remaining phases

---

## Appendix: Module Inventory

### Complete Module List by Category

**Tier 1: Critical (9 modules)**
- erlmcp_client.erl
- erlmcp_server.erl → SPLIT
- erlmcp_json_rpc.erl
- erlmcp_registry.erl
- erlmcp_capabilities.erl
- erlmcp_transport_behavior.erl
- erlmcp_transport_stdio.erl
- erlmcp_transport_tcp.erl
- erlmcp_transport_http.erl

**Tier 2: Essential (12 modules)**
- erlmcp_rate_limiter.erl
- erlmcp_auth.erl
- erlmcp_circuit_breaker.erl
- erlmcp_batch.erl
- erlmcp_hooks.erl
- erlmcp_session_manager.erl
- erlmcp_connection_monitor.erl
- erlmcp_code_reload.erl → EVALUATE
- erlmcp_logging.erl
- erlmcp_transport_ws.erl
- erlmcp_transport_sse.erl
- erlmcp_pool_manager.erl

**Tier 3: Observability (consolidate from 27 → 10 modules)**
- erlmcp_metrics.erl → KEEP
- erlmcp_otel.erl → SIMPLIFY
- erlmcp_health_monitor.erl → KEEP
- erlmcp_audit_log.erl → KEEP
- erlmcp_tracing.erl → KEEP
- erlmcp_dashboard_server.erl → MOVE to examples
- erlmcp_dashboard_http_handler.erl → MOVE to examples
- erlmcp_metrics_server.erl → KEEP
- erlmcp_metrics_aggregator.erl → KEEP
- erlmcp_process_monitor.erl → KEEP

**Tier 4: Remove/Move (25 modules)**
- See "The Costly 20%" section above

---

**Document Version**: 1.0
**Last Updated**: 2026-01-30
**Status**: DRAFT - PENDING REVIEW
**Next Review**: After Phase 1 completion

