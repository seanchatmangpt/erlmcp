# erlmcp Code Consolidation & Cleanup Strategy
## 80/20 Principle - Maximum Impact, Minimum Risk

**Version:** 0.6.0 → 0.7.0
**Date:** 2026-01-30
**Scope:** Full codebase consolidation following recent MCP compliance work
**Philosophy:** Remove cruft, consolidate duplication, improve maintainability without breaking functionality

---

## Executive Summary

**Current State:**
- **189 Erlang modules** across 3 apps (erlmcp_core: 73, erlmcp_transports: 29, erlmcp_observability: 28)
- **214 markdown files** in root directory (documentation debt)
- **47 root-level test scripts** (manual, ad-hoc, not in CI)
- **45 test files** recommended for deletion (37.5% of total tests)
- **6 xref warnings** (undefined functions)
- **3 compilation warnings**

**Expected Outcomes:**
- **Phase 1 (Quick Wins):** Remove 45 test files, 20+ MD files, 1 broken module → Save 3,000+ LOC
- **Phase 2 (Module Consolidation):** Merge 8 modules → Save 1,500+ LOC, improve maintainability
- **Phase 3 (Refactoring):** Split 3 large modules → Improve readability, reduce complexity
- **Phase 4 (Documentation):** Consolidate 214 MD files to 30 essential docs → Reduce cognitive load

**Total Impact:** Remove ~5,000 LOC, eliminate 80+ files, improve code quality without functionality changes

---

## Phase 1: Quick Wins (Remove Dead Code)
**Duration:** 2-3 days
**Risk:** LOW
**Effort:** LOW
**Impact:** HIGH (80% of cleanup benefit)

### 1.1 Delete Root-Level Manual Test Scripts

**Problem:** 47 manual test scripts in root directory, not integrated into CI, superseded by proper test suites

**Tasks:**

| # | Task | Files | LOC Saved | Effort | Risk |
|---|------|-------|-----------|--------|------|
| 1.1.1 | Delete registry manual tests | 2 files | 191 | 15min | NONE |
| 1.1.2 | Delete connection monitor manual tests | 1 file | 93 | 10min | NONE |
| 1.1.3 | Delete progress/pagination manual tests | 2 files | 331 | 15min | NONE |
| 1.1.4 | Delete schema/error manual tests | 2 files | 184 | 15min | NONE |
| 1.1.5 | Delete one-off test scripts | 5 files | 670 | 30min | NONE |
| 1.1.6 | Delete OTEL/TCP/supervision manual tests | 3 files | 126 | 15min | NONE |
| 1.1.7 | Delete metrics/monitor manual tests | 2 files | 200 | 15min | NONE |
| 1.1.8 | Delete stress test variants | 6 files | 450 | 30min | NONE |
| 1.1.9 | Delete batch test duplicates | 8 files | 980 | 45min | NONE |
| 1.1.10 | Delete GCP/marketplace test scripts | 5 files | 425 | 30min | NONE |

**Files to Delete (36 total):**
```bash
# Registry/Connection Tests
test_registry_manual.erl
test_registry_new_functions.erl
test_connection_monitor.erl

# Progress/Pagination
test_progress_manual.erl
test_pagination_manual.erl

# Schema/Error
test_schema_error_fix.erl
test_encode_capabilities.erl

# One-off Scripts
test_error_module.erl
test_codegen.erl
test_marketplace_gen.erl
test_final.erl
test_monitor.erl

# Transport/OTEL
test_simple_otel.erl
test_tcp_transport.erl
test_supervision.erl
test_trace.erl

# Metrics
test_metrics.erl

# Stress Tests (variants)
test_quick_stress.erl
run_stress_test.erl
run_stress_retest.erl
quick_stress_test.erl
run_collapse_test.erl
run_sup_tests.erl

# Batch Tests (duplicates)
test_batch.erl
test_batch4.erl
test_batch4_db_ops.erl
test_batch9_mcp_roundtrip.erl
run_batch14_test.erl
run_batch18_simple.erl
run_batch18_test.erl
test_batch14.sh

# GCP/Marketplace
test_gcp_standalone.erl
test_gcp_gen_server.erl
test_gcp_full_integration.erl
gen_marketplace.erl
test_100k_pooling.erl
```

**Verification Commands:**
```bash
# Before deletion, verify proper tests exist
rebar3 eunit --module=erlmcp_registry_tests
rebar3 eunit --module=erlmcp_connection_monitor_tests
rebar3 eunit --module=erlmcp_progress_tests
rebar3 eunit --module=erlmcp_pagination_tests

# After deletion, verify CI still passes
make check
```

**Dependencies:** None
**Success Criteria:**
- ✅ All 36 files deleted
- ✅ make check passes (0 errors)
- ✅ No references to deleted files in docs/scripts

---

### 1.2 Delete Broken/Deprecated Modules

**Problem:** Module with .broken extension, unused pricing module

**Tasks:**

| # | Task | Files | LOC Saved | Effort | Risk |
|---|------|-------|-----------|--------|------|
| 1.2.1 | Delete erlmcp_pricing_upgrade.erl.broken | 1 file | ~200 | 10min | NONE |
| 1.2.2 | Remove references to broken module | N/A | N/A | 15min | LOW |

**Files to Delete:**
```bash
apps/erlmcp_core/src/pricing/erlmcp_pricing_upgrade.erl.broken
```

**Verification:**
```bash
# Check for references
grep -r "pricing_upgrade" apps/
```

**Dependencies:** None
**Success Criteria:**
- ✅ File deleted
- ✅ No grep matches for "pricing_upgrade"

---

### 1.3 Consolidate Markdown Documentation (Phase 1 - Obvious Deletions)

**Problem:** 214 markdown files in root directory, many are temporary/duplicate/outdated

**Tasks:**

| # | Task | Files | Effort | Risk |
|---|------|-------|--------|------|
| 1.3.1 | Delete AGENT_*_COMPLETION_REPORT.md files | 8 files | 20min | NONE |
| 1.3.2 | Delete batch test result files | 6 files | 15min | NONE |
| 1.3.3 | Delete duplicate audit reports | 5 files | 15min | NONE |
| 1.3.4 | Delete obsolete delivery manifests | 7 files | 20min | NONE |

**Files to Delete (26 total):**
```bash
# Agent Completion Reports (superseded by version control)
AGENT_11_COMPLETION_REPORT.md
AGENT_11_INTEGRATION_DELIVERY.md
AGENT_12_COMPLETION.md
AGENT_15_CACHE_DELIVERABLES.md
AGENT_15_PRODUCTION_READINESS_CERTIFICATION.txt
AGENT_18_COMPLETE.md
AGENT_3_DELIVERY_INDEX.md
AGENT_4_BACKPRESSURE_DELIVERABLES.md

# Batch Test Results (temporary)
BATCH16_RESULTS.txt
BATCH6_AUTH_REPORT.md
BATCH6_RESULTS.md
BATCH_18_SUMMARY.md
batch14_results.txt
test_batch14.sh

# Audit Duplicates
AUDIT_DELIVERABLES.md
AUDIT_FILES_MANIFEST.md
AUDIT_FINDINGS.txt
AUDIT_SUMMARY_TABLE.txt
COMPLETE_AUDIT_MANIFEST.txt

# Obsolete Deliveries
DELIVERABLES.md
DELIVERABLES.txt
DELIVERABLES_ERROR_HANDLING.md
DELIVERABLES_GAP26.md
DELIVERY.txt
DELIVERY_MANIFEST.txt
DELIVERY_REPORT_AUTO_FIX.md
```

**Verification:**
```bash
# Check if any are referenced in critical docs
grep -r "AGENT_11_COMPLETION" docs/ README.md CLAUDE.md
```

**Dependencies:** None
**Success Criteria:**
- ✅ All 26 files deleted
- ✅ No broken links in README.md, CLAUDE.md, docs/

---

### 1.4 Fix Xref Warnings

**Problem:** 6 undefined function warnings reported in code review

**Tasks:**

| # | Task | Description | Effort | Risk |
|---|------|-------------|--------|------|
| 1.4.1 | Run xref to identify warnings | `rebar3 xref` | 5min | NONE |
| 1.4.2 | Fix or document each warning | Add functions or exports | 30min | LOW |
| 1.4.3 | Verify xref clean | `rebar3 xref` passes | 5min | NONE |

**Verification:**
```bash
rebar3 xref
# Expected: 0 warnings
```

**Dependencies:** None
**Success Criteria:**
- ✅ rebar3 xref reports 0 warnings
- ✅ No undefined function references

---

### Phase 1 Summary

**Total Effort:** 6-8 hours (1 day)
**Total Files Deleted:** 62+ files
**Total LOC Saved:** ~3,650 lines
**Risk Level:** NONE to LOW
**Deployment Impact:** NONE (only test/doc files deleted)

**Rollback Plan:**
```bash
# Before Phase 1:
git checkout -b cleanup/phase1-quick-wins
git add .
git commit -m "Checkpoint before Phase 1 cleanup"

# If issues arise:
git reset --hard HEAD~1
```

**Success Criteria:**
- ✅ All specified files deleted
- ✅ `make check` passes (compile + tests)
- ✅ `rebar3 xref` clean
- ✅ No broken references in documentation

---

## Phase 2: Module Consolidation (Merge Similar Functionality)
**Duration:** 3-5 days
**Risk:** MEDIUM
**Effort:** MEDIUM
**Impact:** HIGH (code quality, maintainability)

### 2.1 Consolidate Rate Limiter Modules

**Problem:** Two rate limiter implementations (erlmcp_rate_limiter.erl, erlmcp_rate_limiter_v2.erl)

**Analysis:**
- `erlmcp_rate_limiter.erl`: 845 lines, mature implementation
- `erlmcp_rate_limiter_v2.erl`: 234 lines, experimental

**Tasks:**

| # | Task | Description | Effort | Risk |
|---|------|-------------|--------|------|
| 2.1.1 | Research both implementations | Compare APIs, features, tests | 2hr | NONE |
| 2.1.2 | Identify v2 improvements | What features should be backported? | 1hr | LOW |
| 2.1.3 | Merge improvements into v1 | Backport v2 features to erlmcp_rate_limiter | 4hr | MEDIUM |
| 2.1.4 | Update tests | Ensure all v2 tests work with merged module | 2hr | MEDIUM |
| 2.1.5 | Update references | Find/replace erlmcp_rate_limiter_v2 calls | 1hr | LOW |
| 2.1.6 | Delete v2 module | Remove erlmcp_rate_limiter_v2.erl | 15min | LOW |

**Verification:**
```bash
# Find all references
grep -r "rate_limiter_v2" apps/

# Run all rate limiter tests
rebar3 eunit --module=erlmcp_rate_limiting_tests
rebar3 eunit --module=erlmcp_rate_limit_edge_case_tests

# Integration test
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_integration_SUITE
```

**Dependencies:** Phase 1 complete
**Success Criteria:**
- ✅ Only erlmcp_rate_limiter.erl exists
- ✅ All v2 features preserved
- ✅ All tests pass
- ✅ No references to "rate_limiter_v2"

---

### 2.2 Consolidate Monitor Modules

**Problem:** Multiple monitoring modules with overlapping functionality

**Analysis:**
- `erlmcp_core/src/erlmcp_connection_monitor.erl` - Connection tracking
- `erlmcp_core/src/erlmcp_node_monitor.erl` - Node health monitoring
- `erlmcp_observability/src/erlmcp_process_monitor.erl` - Process monitoring
- `erlmcp_observability/src/erlmcp_health_monitor.erl` (716 lines) - Health checks

**Recommendation:** Create unified monitoring facade

**Tasks:**

| # | Task | Description | Effort | Risk |
|---|------|-------------|--------|------|
| 2.2.1 | Design unified monitor API | Single interface for all monitoring | 2hr | LOW |
| 2.2.2 | Create erlmcp_monitor facade | Delegates to specialized monitors | 3hr | MEDIUM |
| 2.2.3 | Update client code | Use facade instead of direct calls | 2hr | MEDIUM |
| 2.2.4 | Add deprecation warnings | Warn on direct monitor calls | 1hr | LOW |
| 2.2.5 | Update documentation | Document unified API | 1hr | LOW |

**New Module Structure:**
```erlang
% apps/erlmcp_observability/src/erlmcp_monitor.erl
-module(erlmcp_monitor).

% Unified API
-export([
    start_link/0,
    get_health/0,          % Delegates to health_monitor
    list_connections/0,    % Delegates to connection_monitor
    get_node_status/0,     % Delegates to node_monitor
    list_processes/0       % Delegates to process_monitor
]).
```

**Verification:**
```bash
# All monitor tests pass
rebar3 eunit --module=erlmcp_connection_monitor_tests
rebar3 eunit --module=erlmcp_health_monitor_tests

# Integration test
make check
```

**Dependencies:** Phase 1 complete
**Success Criteria:**
- ✅ New erlmcp_monitor module created
- ✅ All existing monitors still functional
- ✅ Client code uses unified API
- ✅ All tests pass

---

### 2.3 Consolidate Guard Modules

**Problem:** CPU and memory guards are separate but conceptually related

**Analysis:**
- `erlmcp_cpu_guard.erl` - CPU quota enforcement
- `erlmcp_memory_guard.erl` - Memory limit enforcement
- Both have similar patterns (gen_server, threshold checking, notifications)

**Recommendation:** Create unified resource guard

**Tasks:**

| # | Task | Description | Effort | Risk |
|---|------|-------------|--------|------|
| 2.3.1 | Create erlmcp_resource_guard | Unified CPU + memory guarding | 4hr | MEDIUM |
| 2.3.2 | Migrate CPU guard logic | Move to resource_guard | 2hr | MEDIUM |
| 2.3.3 | Migrate memory guard logic | Move to resource_guard | 2hr | MEDIUM |
| 2.3.4 | Update supervision tree | Replace 2 guards with 1 | 1hr | HIGH |
| 2.3.5 | Update tests | Consolidate guard tests | 2hr | MEDIUM |
| 2.3.6 | Delete old guard modules | Remove cpu_guard, memory_guard | 15min | LOW |

**New Module Structure:**
```erlang
% apps/erlmcp_core/src/erlmcp_resource_guard.erl
-module(erlmcp_resource_guard).

-behaviour(gen_server).

% API
-export([
    start_link/1,
    check_cpu/0,
    check_memory/0,
    get_status/0
]).

% Config
-record(guard_config, {
    cpu_threshold :: float(),
    memory_threshold :: non_neg_integer(),
    check_interval :: pos_integer()
}).
```

**Verification:**
```bash
# Tests pass
rebar3 eunit --module=erlmcp_memory_guard_tests
rebar3 eunit --module=erlmcp_resource_guard_tests

# Supervisor starts correctly
make console
> observer:start().
% Verify erlmcp_resource_guard appears once
```

**Dependencies:** Phase 1 complete
**Success Criteria:**
- ✅ erlmcp_resource_guard module created
- ✅ Old guard modules deleted
- ✅ Supervision tree updated
- ✅ All tests pass

---

### 2.4 Consolidate Pricing Modules (Optional - Low Priority)

**Problem:** 13 pricing modules in erlmcp_core/src/pricing/ - consider extracting to separate app

**Analysis:**
- Pricing is a distinct domain (TCPS-related)
- 13 modules: pricing_plan, pricing_state, pricing_receipt, sla_monitor, etc.
- May not belong in erlmcp_core

**Recommendation:** Extract to `erlmcp_pricing` app (v0.7.0 or v0.8.0 release)

**Tasks:**

| # | Task | Description | Effort | Risk |
|---|------|-------------|--------|------|
| 2.4.1 | Create erlmcp_pricing app | New OTP application | 2hr | MEDIUM |
| 2.4.2 | Move pricing modules | From core/src/pricing to pricing/src | 2hr | MEDIUM |
| 2.4.3 | Update dependencies | Core depends on pricing (optional) | 1hr | MEDIUM |
| 2.4.4 | Update build config | Add pricing to rebar.config | 30min | LOW |
| 2.4.5 | Update tests | Move pricing tests | 1hr | MEDIUM |
| 2.4.6 | Update documentation | Document pricing as optional | 1hr | LOW |

**Verification:**
```bash
# Build without pricing
rebar3 as minimal compile

# Build with pricing
rebar3 compile

# Tests pass
rebar3 eunit --application=erlmcp_pricing
```

**Dependencies:** Phase 2.1-2.3 complete
**Success Criteria:**
- ✅ erlmcp_pricing app created
- ✅ Core compiles without pricing (minimal profile)
- ✅ Core compiles with pricing (default profile)
- ✅ All tests pass

**Note:** This is OPTIONAL and can be deferred to v0.7.0 or v0.8.0

---

### 2.5 Consolidate Transport Registry and Discovery

**Problem:** Transport registry and discovery have overlapping responsibilities

**Analysis:**
- `erlmcp_transport_registry.erl` - Register/lookup transports
- `erlmcp_transport_discovery.erl` - Discover available transports

**Recommendation:** Merge into transport_registry

**Tasks:**

| # | Task | Description | Effort | Risk |
|---|------|-------------|--------|------|
| 2.5.1 | Add discovery to registry | Merge discovery functions | 2hr | MEDIUM |
| 2.5.2 | Update transport code | Use registry for discovery | 1hr | MEDIUM |
| 2.5.3 | Update tests | Consolidate tests | 1hr | MEDIUM |
| 2.5.4 | Delete discovery module | Remove transport_discovery.erl | 15min | LOW |

**Verification:**
```bash
# Transport tests pass
rebar3 eunit --module=erlmcp_transport_registry_tests
rebar3 ct --suite=apps/erlmcp_transports/test/erlmcp_transport_integration_SUITE
```

**Dependencies:** Phase 1 complete
**Success Criteria:**
- ✅ Discovery logic in registry
- ✅ Discovery module deleted
- ✅ All transport tests pass

---

### Phase 2 Summary

**Total Effort:** 30-40 hours (5 days)
**Total Modules Consolidated:** 8 modules → 3-4 modules
**Total LOC Saved:** ~1,500 lines
**Risk Level:** MEDIUM
**Deployment Impact:** LOW (internal refactoring, API-compatible)

**Incremental Rollout Strategy:**
```bash
# One consolidation per branch
git checkout -b consolidate/rate-limiter
# ... do 2.1 ...
git commit -m "Consolidate rate limiter modules"
# Test, review, merge

git checkout -b consolidate/monitors
# ... do 2.2 ...
git commit -m "Add unified monitor facade"
# Test, review, merge

# Continue for each consolidation
```

**Success Criteria:**
- ✅ All specified modules consolidated
- ✅ `make check` passes (compile + tests + xref + dialyzer)
- ✅ No API breakage (existing code works)
- ✅ Code coverage maintained (≥80%)

---

## Phase 3: Refactoring (Improve Code Structure)
**Duration:** 5-7 days
**Risk:** MEDIUM to HIGH
**Effort:** HIGH
**Impact:** MEDIUM (code quality, maintainability)

### 3.1 Split erlmcp_server.erl (2,040 lines → 500 lines)

**Problem:** erlmcp_server.erl is 2,040 lines (306% over 500-line limit)

**Analysis:**
- Resources management: ~400 lines
- Tools management: ~350 lines
- Prompts management: ~350 lines
- Server lifecycle: ~300 lines
- Message handling: ~300 lines
- Utilities: ~340 lines

**Recommendation:** Split into 5 focused modules

**Tasks:**

| # | Task | Description | Effort | Risk |
|---|------|-------------|--------|------|
| 3.1.1 | Extract resource management | → erlmcp_server_resources.erl | 4hr | HIGH |
| 3.1.2 | Extract tool management | → erlmcp_server_tools.erl | 4hr | HIGH |
| 3.1.3 | Extract prompt management | → erlmcp_server_prompts.erl | 4hr | HIGH |
| 3.1.4 | Extract utilities | → erlmcp_server_util.erl | 2hr | MEDIUM |
| 3.1.5 | Update server to delegate | Slim down to 500 lines | 4hr | HIGH |
| 3.1.6 | Update tests | Split server tests | 4hr | HIGH |
| 3.1.7 | Verify API compatibility | No breaking changes | 2hr | HIGH |

**New Module Structure:**
```
apps/erlmcp_core/src/
├── erlmcp_server.erl              (~500 lines) - Main gen_server, delegates
├── erlmcp_server_resources.erl    (~400 lines) - Resource CRUD
├── erlmcp_server_tools.erl        (~350 lines) - Tool CRUD
├── erlmcp_server_prompts.erl      (~350 lines) - Prompt CRUD
└── erlmcp_server_util.erl         (~200 lines) - Shared utilities

apps/erlmcp_core/test/
├── erlmcp_server_tests.erl        (keep main tests)
├── erlmcp_server_resources_tests.erl
├── erlmcp_server_tools_tests.erl
└── erlmcp_server_prompts_tests.erl
```

**API Stability:**
```erlang
% External API remains unchanged
erlmcp_server:add_resource(Server, Name, Uri, Handler)
erlmcp_server:add_tool(Server, Name, Description, Handler)
erlmcp_server:add_prompt(Server, Name, Description, Handler)

% Internal delegation (hidden from users)
erlmcp_server.erl:
    add_resource(Server, Name, Uri, Handler) ->
        erlmcp_server_resources:add(Server, Name, Uri, Handler).
```

**Verification:**
```bash
# All server tests pass
rebar3 eunit --module=erlmcp_server_tests
rebar3 eunit --module=erlmcp_server_resources_tests
rebar3 eunit --module=erlmcp_server_tools_tests

# Integration tests pass
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_integration_SUITE

# Examples still work
cd examples/calculator && make run
cd examples/weather && make run
```

**Dependencies:** Phase 1 and 2 complete
**Success Criteria:**
- ✅ erlmcp_server.erl ≤ 500 lines
- ✅ 4 new focused modules created
- ✅ All tests pass
- ✅ No API breakage
- ✅ Examples work unchanged

---

### 3.2 Split erlmcp_capabilities.erl (1,253 lines → 400 lines)

**Problem:** erlmcp_capabilities.erl is 1,253 lines (250% over limit)

**Analysis:**
- Capability encoding: ~300 lines
- Capability validation: ~250 lines
- Capability negotiation: ~200 lines
- Capability types: ~300 lines
- Utilities: ~200 lines

**Recommendation:** Split into 3 focused modules

**Tasks:**

| # | Task | Description | Effort | Risk |
|---|------|-------------|--------|------|
| 3.2.1 | Extract capability types | → erlmcp_capability_types.erl | 2hr | MEDIUM |
| 3.2.2 | Extract negotiation logic | → erlmcp_capability_negotiation.erl | 3hr | HIGH |
| 3.2.3 | Slim down main module | Keep only core functionality | 2hr | MEDIUM |
| 3.2.4 | Update tests | Split capability tests | 2hr | MEDIUM |
| 3.2.5 | Verify API compatibility | No breaking changes | 1hr | MEDIUM |

**New Module Structure:**
```
apps/erlmcp_core/src/
├── erlmcp_capabilities.erl             (~400 lines) - Core API
├── erlmcp_capability_types.erl         (~300 lines) - Type definitions
└── erlmcp_capability_negotiation.erl   (~350 lines) - Negotiation logic
```

**Verification:**
```bash
# Capability tests pass
rebar3 eunit --module=erlmcp_capability_negotiation_tests

# Integration tests pass
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_integration_SUITE
```

**Dependencies:** Phase 1 and 2 complete
**Success Criteria:**
- ✅ erlmcp_capabilities.erl ≤ 400 lines
- ✅ 2 new focused modules created
- ✅ All tests pass
- ✅ No API breakage

---

### 3.3 Refactor erlmcp_rate_limiter.erl (818 lines → 600 lines)

**Problem:** erlmcp_rate_limiter.erl is 818 lines after Phase 2 consolidation

**Analysis:**
- Core rate limiting: ~300 lines
- Token bucket algorithm: ~200 lines
- Configuration management: ~150 lines
- Statistics/reporting: ~168 lines

**Recommendation:** Extract statistics to separate module

**Tasks:**

| # | Task | Description | Effort | Risk |
|---|------|-------------|--------|------|
| 3.3.1 | Extract statistics | → erlmcp_rate_limiter_stats.erl | 2hr | MEDIUM |
| 3.3.2 | Simplify config handling | Use application env | 2hr | MEDIUM |
| 3.3.3 | Update tests | Test statistics separately | 1hr | LOW |
| 3.3.4 | Verify performance | No regression | 1hr | MEDIUM |

**New Module Structure:**
```
apps/erlmcp_core/src/
├── erlmcp_rate_limiter.erl        (~600 lines) - Core limiting logic
└── erlmcp_rate_limiter_stats.erl  (~200 lines) - Statistics collection
```

**Verification:**
```bash
# Rate limiter tests pass
rebar3 eunit --module=erlmcp_rate_limiting_tests

# No performance regression
make benchmark-quick
```

**Dependencies:** Phase 2.1 complete (rate_limiter_v2 merged)
**Success Criteria:**
- ✅ erlmcp_rate_limiter.erl ≤ 600 lines
- ✅ Statistics module created
- ✅ All tests pass
- ✅ No performance regression

---

### 3.4 Improve Test Organization

**Problem:** Test files scattered across multiple directories

**Current Structure:**
```
test/                           # Root-level (legacy)
apps/erlmcp_core/test/          # Core tests
apps/erlmcp_transports/test/    # Transport tests
apps/erlmcp_observability/test/ # Observability tests
```

**Recommendation:** Standardize test structure

**Tasks:**

| # | Task | Description | Effort | Risk |
|---|------|-------------|--------|------|
| 3.4.1 | Move root tests to apps | Organize by application | 2hr | LOW |
| 3.4.2 | Standardize test naming | *_tests.erl (EUnit), *_SUITE.erl (CT) | 1hr | LOW |
| 3.4.3 | Update rebar.config | Point to new test locations | 30min | LOW |
| 3.4.4 | Delete empty test/ dir | Remove root test directory | 15min | NONE |

**Verification:**
```bash
# All tests still run
rebar3 eunit
rebar3 ct
make check
```

**Dependencies:** Phase 1 complete (manual tests deleted)
**Success Criteria:**
- ✅ No tests in root test/ directory
- ✅ All tests organized by application
- ✅ Consistent naming (*_tests.erl, *_SUITE.erl)
- ✅ All tests pass

---

### Phase 3 Summary

**Total Effort:** 50-60 hours (7 days)
**Total LOC Refactored:** ~4,000 lines
**Total New Modules:** 10 focused modules
**Risk Level:** MEDIUM to HIGH
**Deployment Impact:** NONE (internal refactoring, API-compatible)

**Incremental Rollout Strategy:**
```bash
# One refactoring per branch
git checkout -b refactor/split-server
# ... do 3.1 ...
# Full test suite before merge

git checkout -b refactor/split-capabilities
# ... do 3.2 ...
# Full test suite before merge

# Continue for each refactoring
```

**Success Criteria:**
- ✅ All large modules split (≤ 500 lines each)
- ✅ `make check` passes (compile + tests + xref + dialyzer)
- ✅ No API breakage
- ✅ Code coverage maintained (≥80%)
- ✅ Examples still work

---

## Phase 4: Documentation and Polish
**Duration:** 3-4 days
**Risk:** LOW
**Effort:** MEDIUM
**Impact:** HIGH (developer experience)

### 4.1 Consolidate Markdown Documentation

**Problem:** 214 markdown files in root directory (after Phase 1, still ~188 remain)

**Analysis by Category:**
- **Architecture/Design:** 15 files → Keep 5 essential
- **Implementation Reports:** 40 files → Archive to docs/archive/
- **Delivery Receipts:** 30 files → Archive to docs/receipts/
- **Test Reports:** 25 files → Delete (superseded by CI)
- **Benchmark Reports:** 12 files → Keep 2 latest in docs/benchmarks/
- **Deployment Guides:** 20 files → Consolidate to 3 files
- **Checklists/Indexes:** 25 files → Consolidate to 5 files
- **Miscellaneous:** 21 files → Review individually

**Recommendation:** Consolidate to 30 essential docs

**Tasks:**

| # | Task | Description | Files | Effort | Risk |
|---|------|-------------|-------|--------|------|
| 4.1.1 | Archive implementation reports | Move to docs/archive/ | 40 | 1hr | NONE |
| 4.1.2 | Archive delivery receipts | Move to docs/receipts/ | 30 | 1hr | NONE |
| 4.1.3 | Delete obsolete test reports | Remove temporary reports | 25 | 30min | NONE |
| 4.1.4 | Consolidate benchmark reports | Keep 2 latest | 10 | 30min | NONE |
| 4.1.5 | Consolidate deployment guides | 20 → 3 files | 20 | 2hr | LOW |
| 4.1.6 | Consolidate checklists | 25 → 5 master checklists | 25 | 2hr | LOW |
| 4.1.7 | Review miscellaneous | Keep/archive/delete | 21 | 1hr | LOW |

**Essential Documentation (30 files):**
```
Root Directory (10 files):
├── README.md                    # Main entry point
├── CLAUDE.md                    # Development guide (keep)
├── CHANGELOG.md                 # Version history
├── CONTRIBUTING.md              # Contribution guidelines
├── LICENSE                      # License file
├── ARCHITECTURE.md              # High-level architecture (NEW - consolidation)
├── DEPLOYMENT.md                # Deployment guide (NEW - consolidation)
├── TESTING.md                   # Test strategy (NEW - consolidation)
├── PERFORMANCE.md               # Benchmark results (NEW - consolidation)
└── MIGRATION.md                 # Migration guide for v0.7.0

docs/ Directory (20 files):
├── architecture/
│   ├── overview.md              # System overview
│   ├── supervision.md           # Supervision trees
│   └── protocols.md             # MCP protocol details
├── guides/
│   ├── quickstart.md            # Getting started
│   ├── client-usage.md          # Client API guide
│   ├── server-usage.md          # Server API guide
│   ├── transport-guide.md       # Transport configuration
│   └── observability-guide.md   # Monitoring/tracing
├── api/
│   ├── client-api.md            # erlmcp_client API
│   ├── server-api.md            # erlmcp_server API
│   ├── transport-api.md         # Transport API
│   └── observability-api.md     # Observability API
├── operations/
│   ├── deployment.md            # Production deployment
│   ├── scaling.md               # Scaling strategies
│   ├── troubleshooting.md       # Common issues
│   └── monitoring.md            # Monitoring setup
└── development/
    ├── contributing.md          # How to contribute
    ├── testing.md               # Test guidelines
    ├── benchmarking.md          # Benchmark guide
    └── releasing.md             # Release process

docs/archive/                    # Historical reports (40+ files)
docs/receipts/                   # Delivery receipts (30+ files)
```

**Verification:**
```bash
# Check for broken links
find docs -name "*.md" -exec grep -l "](/" {} \; | while read f; do
    markdown-link-check "$f"
done

# Verify essential docs exist
test -f README.md && test -f CLAUDE.md && test -f docs/guides/quickstart.md
```

**Dependencies:** Phase 1.3 complete (obvious deletions)
**Success Criteria:**
- ✅ Root directory: ≤ 10 essential .md files
- ✅ docs/ directory: 20 organized files
- ✅ docs/archive/: Historical reports preserved
- ✅ No broken links

---

### 4.2 Improve API Documentation

**Problem:** Code review found 0 @doc comments in key modules

**Tasks:**

| # | Task | Description | Modules | Effort | Risk |
|---|------|-------------|---------|--------|------|
| 4.2.1 | Add module @doc headers | High-level module purpose | 20 | 3hr | NONE |
| 4.2.2 | Add function @doc comments | Public API documentation | 100 | 6hr | NONE |
| 4.2.3 | Generate edoc | `rebar3 edoc` | N/A | 30min | NONE |
| 4.2.4 | Publish docs to docs/edoc/ | HTML documentation | N/A | 30min | NONE |

**Example @doc comments:**
```erlang
%% @doc erlmcp_client implements the MCP client protocol as a gen_server.
%%
%% This module provides a complete MCP client implementation with:
%% - Request/response correlation via request IDs
%% - Capability negotiation with servers
%% - Tool calling and resource access
%% - Async notification handling
%%
%% Example usage:
%% ```
%% {ok, Client} = erlmcp_client:start_link(TransportMod, TransportOpts).
%% {ok, Result} = erlmcp_client:call_tool(Client, "calculator", "add", #{a => 1, b => 2}).
%% '''
-module(erlmcp_client).

%% @doc Start a new MCP client linked to the calling process.
%% @param TransportMod The transport module (e.g., erlmcp_transport_tcp)
%% @param TransportOpts Transport-specific options
%% @returns {ok, Pid} | {error, Reason}
-spec start_link(module(), map()) -> {ok, pid()} | {error, term()}.
start_link(TransportMod, TransportOpts) ->
    ...
```

**Priority Modules (Top 20):**
1. erlmcp_client.erl
2. erlmcp_server.erl
3. erlmcp_registry.erl
4. erlmcp_json_rpc.erl
5. erlmcp_capabilities.erl
6. erlmcp_transport_tcp.erl
7. erlmcp_transport_http.erl
8. erlmcp_transport_stdio.erl
9. erlmcp_cache.erl
10. erlmcp_rate_limiter.erl
11. erlmcp_circuit_breaker.erl
12. erlmcp_session_manager.erl
13. erlmcp_metrics.erl
14. erlmcp_tracing.erl
15. erlmcp_otel.erl
16. erlmcp_pool_manager.erl
17. erlmcp_auth.erl
18. erlmcp_pagination.erl
19. erlmcp_batch.erl
20. erlmcp_notification_handler.erl

**Verification:**
```bash
# Generate edoc
rebar3 edoc

# Check output
ls doc/*.html
# Should see 20+ HTML files

# Check for @doc coverage
grep -r "@doc" apps/*/src/*.erl | wc -l
# Should be ≥ 100 (module headers + functions)
```

**Dependencies:** Phase 3 complete (refactoring done)
**Success Criteria:**
- ✅ All 20 priority modules have @doc headers
- ✅ All public functions have @doc comments
- ✅ `rebar3 edoc` generates HTML docs
- ✅ docs/edoc/ published

---

### 4.3 Standardize Code Formatting

**Problem:** rebar3_format plugin not configured (from code review)

**Tasks:**

| # | Task | Description | Effort | Risk |
|---|------|-------------|--------|------|
| 4.3.1 | Add rebar3_format to rebar.config | Configure plugin | 15min | NONE |
| 4.3.2 | Run initial format | `rebar3 format -w` | 30min | LOW |
| 4.3.3 | Review formatting changes | Manual review | 1hr | LOW |
| 4.3.4 | Commit formatting | Separate commit | 15min | NONE |
| 4.3.5 | Add pre-commit hook | Auto-format on commit | 30min | NONE |
| 4.3.6 | Add CI check | Verify formatting in CI | 30min | NONE |

**rebar.config additions:**
```erlang
{plugins, [
    rebar3_format,
    rebar3_lint,
    rebar3_proper
]}.

{format, [
    {sources, ["apps/*/src/**/*.erl", "apps/*/test/**/*.erl"]},
    {line_length, 100},
    {indent, 4},
    {files, [
        "apps/erlmcp_core/src/*.erl",
        "apps/erlmcp_transports/src/*.erl",
        "apps/erlmcp_observability/src/*.erl"
    ]}
]}.
```

**Pre-commit hook (.git/hooks/pre-commit):**
```bash
#!/bin/bash
set -e

echo "Running rebar3 format..."
rebar3 format --verify || {
    echo "ERROR: Code not formatted. Run: rebar3 format -w"
    exit 1
}

echo "Format check passed."
```

**Verification:**
```bash
# Format codebase
rebar3 format -w

# Verify formatting
rebar3 format --verify
# Expected: No changes needed

# Test pre-commit hook
git commit -m "test" # Should check formatting
```

**Dependencies:** None (can be done anytime)
**Success Criteria:**
- ✅ rebar3_format configured
- ✅ All code formatted
- ✅ Pre-commit hook installed
- ✅ CI checks formatting

---

### 4.4 Add Code Quality Badges

**Problem:** README.md lacks quality badges

**Tasks:**

| # | Task | Description | Effort | Risk |
|---|------|-------------|--------|------|
| 4.4.1 | Add CI status badge | GitHub Actions status | 15min | NONE |
| 4.4.2 | Add coverage badge | Code coverage % | 15min | NONE |
| 4.4.3 | Add Hex.pm version badge | Package version | 15min | NONE |
| 4.4.4 | Add license badge | License type | 15min | NONE |

**README.md additions:**
```markdown
# erlmcp - Erlang MCP SDK

[![CI](https://github.com/user/erlmcp/workflows/CI/badge.svg)](https://github.com/user/erlmcp/actions)
[![Coverage](https://coveralls.io/repos/github/user/erlmcp/badge.svg)](https://coveralls.io/github/user/erlmcp)
[![Hex.pm](https://img.shields.io/hexpm/v/erlmcp.svg)](https://hex.pm/packages/erlmcp)
[![License](https://img.shields.io/github/license/user/erlmcp.svg)](LICENSE)

Erlang/OTP implementation of the Model Context Protocol (MCP).

...
```

**Verification:**
```bash
# Check badges render
cat README.md | grep "badge.svg"
```

**Dependencies:** None
**Success Criteria:**
- ✅ 4+ badges in README.md
- ✅ Badges render correctly on GitHub

---

### 4.5 Create Migration Guide (v0.6.0 → v0.7.0)

**Problem:** Major consolidation requires migration guide

**Tasks:**

| # | Task | Description | Effort | Risk |
|---|------|-------------|--------|------|
| 4.5.1 | Document rate_limiter changes | v2 → v1 migration | 1hr | NONE |
| 4.5.2 | Document guard changes | cpu/memory → resource | 1hr | NONE |
| 4.5.3 | Document monitor changes | Multiple → unified API | 1hr | NONE |
| 4.5.4 | Document server refactoring | Internal changes only | 30min | NONE |
| 4.5.5 | Create upgrade checklist | Step-by-step upgrade | 1hr | NONE |

**MIGRATION.md structure:**
```markdown
# Migration Guide: v0.6.0 → v0.7.0

## Overview

Version 0.7.0 consolidates and refactors erlmcp for improved maintainability.

## Breaking Changes

### 1. Rate Limiter Consolidation

**Changed:**
- `erlmcp_rate_limiter_v2` removed
- All v2 features merged into `erlmcp_rate_limiter`

**Action Required:**
```erlang
% Before (v0.6.0):
erlmcp_rate_limiter_v2:check(Client)

% After (v0.7.0):
erlmcp_rate_limiter:check(Client)
```

### 2. Resource Guard Consolidation

**Changed:**
- `erlmcp_cpu_guard` and `erlmcp_memory_guard` removed
- Unified as `erlmcp_resource_guard`

**Action Required:**
```erlang
% Before (v0.6.0):
erlmcp_cpu_guard:check()
erlmcp_memory_guard:check()

% After (v0.7.0):
erlmcp_resource_guard:check_cpu()
erlmcp_resource_guard:check_memory()
```

### 3. Monitor Facade (Optional)

**Added:**
- New unified `erlmcp_monitor` API

**Action Optional:**
```erlang
% Old way still works:
erlmcp_health_monitor:get_health()

% New unified way:
erlmcp_monitor:get_health()
```

## Non-Breaking Changes

### Server Refactoring

No API changes. Internal refactoring only.

### Capabilities Refactoring

No API changes. Internal refactoring only.

## Upgrade Checklist

- [ ] Update `erlmcp_rate_limiter_v2` → `erlmcp_rate_limiter`
- [ ] Update `erlmcp_cpu_guard`/`erlmcp_memory_guard` → `erlmcp_resource_guard`
- [ ] (Optional) Migrate to unified `erlmcp_monitor` API
- [ ] Update dependencies in rebar.config
- [ ] Run `rebar3 format -w`
- [ ] Run `make check` (all tests pass)
- [ ] Test in staging environment
- [ ] Deploy to production

## Support

Questions? Open an issue: https://github.com/user/erlmcp/issues
```

**Verification:**
```bash
# Check migration guide exists
test -f MIGRATION.md

# Check all sections present
grep -E "Breaking Changes|Upgrade Checklist" MIGRATION.md
```

**Dependencies:** Phase 2 and 3 complete
**Success Criteria:**
- ✅ MIGRATION.md created
- ✅ All breaking changes documented
- ✅ Upgrade checklist provided

---

### Phase 4 Summary

**Total Effort:** 20-30 hours (4 days)
**Total Files Organized:** 188 MD files → 30 essential docs
**Total Documentation Added:** 100+ @doc comments, migration guide
**Risk Level:** LOW
**Deployment Impact:** NONE (documentation only)

**Success Criteria:**
- ✅ Root directory: ≤ 10 .md files
- ✅ docs/ directory: 20 organized files
- ✅ docs/archive/: Historical reports preserved
- ✅ All modules have @doc comments
- ✅ `rebar3 edoc` generates docs
- ✅ Code formatted with rebar3_format
- ✅ README.md has quality badges
- ✅ MIGRATION.md created

---

## Overall Strategy Summary

### Effort Distribution (80/20 Principle)

| Phase | Duration | Files Touched | LOC Impact | Risk | Benefit |
|-------|----------|---------------|------------|------|---------|
| Phase 1 | 1 day | 62 files | -3,650 LOC | LOW | **80%** (Quick wins) |
| Phase 2 | 5 days | 8 modules | -1,500 LOC | MEDIUM | **15%** (Consolidation) |
| Phase 3 | 7 days | 10 modules | Refactor 4K | HIGH | **3%** (Quality) |
| Phase 4 | 4 days | 188 files | Docs only | LOW | **2%** (Polish) |
| **Total** | **17 days** | **268 files** | **-5,150 LOC** | **MEDIUM** | **100%** |

**Key Insight:** Phase 1 (1 day, 6% effort) delivers 80% of the benefit!

---

## Execution Timeline

### Week 1: Foundation
- **Mon-Tue:** Phase 1 (Quick Wins)
- **Wed-Fri:** Phase 2.1-2.2 (Rate limiter + monitors)

### Week 2: Consolidation
- **Mon-Tue:** Phase 2.3 (Guards)
- **Wed-Fri:** Phase 2.5 + Phase 3.1 start (Transport + Server split)

### Week 3: Refactoring
- **Mon-Wed:** Phase 3.1 complete (Server split)
- **Thu-Fri:** Phase 3.2 (Capabilities split)

### Week 4: Polish
- **Mon-Tue:** Phase 3.3-3.4 (Rate limiter + test org)
- **Wed-Fri:** Phase 4 (Documentation)

---

## Risk Mitigation

### High-Risk Activities

1. **Server refactoring (Phase 3.1)** - 2,040 line module
   - **Mitigation:** Incremental extraction, comprehensive tests, API stability tests
   - **Rollback:** Keep old module until new modules verified

2. **Supervision tree changes (Phase 2.3)** - Resource guard consolidation
   - **Mitigation:** Test supervision tree explicitly, observer verification
   - **Rollback:** Keep old guards as deprecated until v0.8.0

3. **Rate limiter merge (Phase 2.1)** - Merging v1 and v2
   - **Mitigation:** Preserve all v2 tests, benchmark performance
   - **Rollback:** Keep v2 module as deprecated

### Risk Reduction Strategies

1. **Incremental Branches:** One phase per branch, full test suite before merge
2. **Feature Flags:** Keep old code paths available during transition
3. **Comprehensive Testing:** `make check` must pass for every commit
4. **Monitoring:** Track metrics in staging before production
5. **Rollback Plan:** Each phase has explicit rollback procedure

---

## Success Criteria

### Phase 1 (Quick Wins)
- ✅ 62+ files deleted
- ✅ `make check` passes
- ✅ `rebar3 xref` clean
- ✅ No broken references

### Phase 2 (Module Consolidation)
- ✅ 8 modules consolidated to 3-4
- ✅ All tests pass
- ✅ No API breakage
- ✅ Code coverage ≥ 80%

### Phase 3 (Refactoring)
- ✅ All large modules ≤ 500 lines
- ✅ All tests pass
- ✅ Examples work unchanged
- ✅ No performance regression

### Phase 4 (Documentation)
- ✅ Root: ≤ 10 .md files
- ✅ docs/: 20 organized files
- ✅ All modules documented
- ✅ MIGRATION.md created

### Overall Success
- ✅ Remove 5,000+ LOC
- ✅ Eliminate 268+ files
- ✅ Improve maintainability
- ✅ No functionality lost
- ✅ No performance regression
- ✅ `make check` passes 100%

---

## Appendix A: Dependency Graph

```
Phase 1 (Quick Wins)
  └── Phase 2 (Consolidation)
        ├── Phase 2.1 (Rate Limiter) ──┐
        ├── Phase 2.2 (Monitors)       │
        ├── Phase 2.3 (Guards)         │
        ├── Phase 2.4 (Pricing) ────────┼─── Phase 3 (Refactoring)
        └── Phase 2.5 (Transport)      │      ├── Phase 3.1 (Server)
                                       │      ├── Phase 3.2 (Capabilities)
                                       │      ├── Phase 3.3 (Rate Limiter)
                                       │      └── Phase 3.4 (Test Org)
                                       │            │
                                       └────────────┴─── Phase 4 (Documentation)
                                                          ├── Phase 4.1 (Markdown)
                                                          ├── Phase 4.2 (API Docs)
                                                          ├── Phase 4.3 (Formatting)
                                                          ├── Phase 4.4 (Badges)
                                                          └── Phase 4.5 (Migration)
```

---

## Appendix B: File Deletion Checklist

### Root-Level Test Scripts (36 files)

```bash
# Delete registry tests
rm test_registry_manual.erl
rm test_registry_new_functions.erl

# Delete connection monitor tests
rm test_connection_monitor.erl

# Delete progress/pagination tests
rm test_progress_manual.erl
rm test_pagination_manual.erl

# Delete schema/error tests
rm test_schema_error_fix.erl
rm test_encode_capabilities.erl

# Delete one-off scripts
rm test_error_module.erl
rm test_codegen.erl
rm test_marketplace_gen.erl
rm test_final.erl
rm test_monitor.erl

# Delete OTEL/TCP/supervision tests
rm test_simple_otel.erl
rm test_tcp_transport.erl
rm test_supervision.erl
rm test_trace.erl

# Delete metrics tests
rm test_metrics.erl

# Delete stress test variants
rm test_quick_stress.erl
rm run_stress_test.erl
rm run_stress_retest.erl
rm quick_stress_test.erl
rm run_collapse_test.erl
rm run_sup_tests.erl

# Delete batch test duplicates
rm test_batch.erl
rm test_batch4.erl
rm test_batch4_db_ops.erl
rm test_batch9_mcp_roundtrip.erl
rm run_batch14_test.erl
rm run_batch18_simple.erl
rm run_batch18_test.erl
rm test_batch14.sh

# Delete GCP/marketplace scripts
rm test_gcp_standalone.erl
rm test_gcp_gen_server.erl
rm test_gcp_full_integration.erl
rm gen_marketplace.erl
rm test_100k_pooling.erl
```

### Broken Modules (1 file)

```bash
rm apps/erlmcp_core/src/pricing/erlmcp_pricing_upgrade.erl.broken
```

### Markdown Files (26 files)

```bash
# Agent completion reports
rm AGENT_11_COMPLETION_REPORT.md
rm AGENT_11_INTEGRATION_DELIVERY.md
rm AGENT_12_COMPLETION.md
rm AGENT_15_CACHE_DELIVERABLES.md
rm AGENT_15_PRODUCTION_READINESS_CERTIFICATION.txt
rm AGENT_18_COMPLETE.md
rm AGENT_3_DELIVERY_INDEX.md
rm AGENT_4_BACKPRESSURE_DELIVERABLES.md

# Batch test results
rm BATCH16_RESULTS.txt
rm BATCH6_AUTH_REPORT.md
rm BATCH6_RESULTS.md
rm BATCH_18_SUMMARY.md
rm batch14_results.txt
rm test_batch14.sh

# Audit duplicates
rm AUDIT_DELIVERABLES.md
rm AUDIT_FILES_MANIFEST.md
rm AUDIT_FINDINGS.txt
rm AUDIT_SUMMARY_TABLE.txt
rm COMPLETE_AUDIT_MANIFEST.txt

# Obsolete deliveries
rm DELIVERABLES.md
rm DELIVERABLES.txt
rm DELIVERABLES_ERROR_HANDLING.md
rm DELIVERABLES_GAP26.md
rm DELIVERY.txt
rm DELIVERY_MANIFEST.txt
rm DELIVERY_REPORT_AUTO_FIX.md
```

**Total Files Deleted: 63**

---

## Appendix C: Quality Gates

Every phase must pass these quality gates before proceeding:

### Compilation Gate
```bash
TERM=dumb rebar3 compile
# Expected: 0 errors, ≤ 3 warnings
```

### Test Gate
```bash
rebar3 eunit
rebar3 ct
# Expected: 100% pass rate, 0 failures
```

### Xref Gate
```bash
rebar3 xref
# Expected: 0 warnings
```

### Dialyzer Gate
```bash
rebar3 dialyzer
# Expected: 0 errors (warnings acceptable)
```

### Coverage Gate
```bash
rebar3 cover
# Expected: ≥ 80% coverage
```

### Format Gate
```bash
rebar3 format --verify
# Expected: No changes needed
```

### Benchmark Gate (if perf-critical code changed)
```bash
make benchmark-quick
# Expected: < 10% regression
```

---

## Appendix D: Communication Plan

### Internal Communication
- **Daily Standup:** Progress update, blockers, next steps
- **Branch Review:** Each phase gets code review before merge
- **Slack Updates:** Post completion of each phase

### External Communication
- **GitHub Issues:** Track each phase as separate issue
- **GitHub Projects:** Kanban board for tracking
- **Release Notes:** Document all changes in CHANGELOG.md
- **Migration Guide:** Publish MIGRATION.md with v0.7.0

---

## Conclusion

This 4-phase consolidation strategy follows the 80/20 principle:

- **Phase 1 (1 day, 6% effort)** delivers **80% of the benefit** through quick wins
- **Phase 2 (5 days, 29% effort)** delivers **15% of the benefit** through consolidation
- **Phase 3 (7 days, 41% effort)** delivers **3% of the benefit** through refactoring
- **Phase 4 (4 days, 24% effort)** delivers **2% of the benefit** through documentation

**Recommendation:** Execute Phase 1 immediately for maximum impact. Phases 2-4 can be scheduled based on team capacity and priorities.

**Questions or concerns?** Contact the erlmcp maintainers or open a discussion on GitHub.

---

**Document Version:** 1.0
**Last Updated:** 2026-01-30
**Author:** Plan Designer Agent
**Status:** PROPOSED (Awaiting Approval)
