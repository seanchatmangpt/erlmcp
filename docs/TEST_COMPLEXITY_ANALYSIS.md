# Test Complexity Analysis Report

**Generated:** 2026-01-30
**Scope:** Analysis of broken/inactive test files in erlmcp
**Purpose:** Identify overly complex tests that could be simplified vs complex but valuable tests

## Executive Summary

Analyzed 5 broken/inactive test files totaling 1,962 lines of code. Key findings:

- **2 tests are OVERLY COMPLEX** and should be simplified/deleted
- **2 tests are COMPLEX BUT VALUABLE** - worth fixing and maintaining
- **1 test module is MISSING** its implementation (dead code)

---

## Detailed Analysis

### 1. erlmcp_schema_validator_tests.erl (562 lines)

**Status:** ❌ OVERLY COMPLEX - DELETE

**Complexity Metrics:**
- 562 total lines (HIGHEST)
- 36 test functions
- 83 assertions
- 40 complex data structures (Schema + Data maps)
- 2 setup/teardown blocks
- Only 3 process calls (low process complexity)

**Why It's Overly Complex:**
1. **Module doesn't exist** - `erlmcp_schema_validator.erl` is missing from codebase
2. **Massive data duplication** - 40+ inline JSON Schema structures hardcoded in tests
3. **Low value** - Tests Jesse library behavior (external dependency), not erlmcp code
4. **Poor test organization** - Monolithic file testing format errors, validation rules, custom validators
5. **Maintenance burden** - Every JSON Schema change requires updating 5+ test functions

**Value vs. Cost:**
- **Value:** LOW - Tests external library (jesse), not core business logic
- **Cost:** HIGH - 562 lines, 40+ hardcoded schemas, complex setup
- **Recommendation:** DELETE or replace with simple smoke tests (50 lines max)

**Simplification Strategy:**
```erlang
%% Replace 562-line file with 50-line smoke test:
%% 1. Test basic validation (5 schemas)
%% 2. Test error formatting (3 error types)
%% 3. Test custom validators (2 cases)
%% Total: 10 test functions, 50 lines, 15 assertions
```

---

### 2. erlmcp_rate_limit_edge_case_tests.erl (420 lines)

**Status:** ✅ COMPLEX BUT VALUABLE - KEEP

**Complexity Metrics:**
- 420 total lines
- 25 test functions
- 30 assertions
- 0 complex data structures (uses simple IDs)
- 2 setup/teardown blocks
- 3 process calls (low process complexity)

**Why It's Valuable:**
1. **Critical functionality** - Rate limiting prevents DoS attacks
2. **Edge case coverage** - Tests floating-point precision, burst capacity, priority queues
3. **Production safety** - Tests real failure modes (token bucket precision, rounding errors)
4. **Well-organized** - Clear test groups (precision, burst, priority, rounding, global, stress)
5. **Good coverage** - 25 test functions cover important edge cases

**Value vs. Cost:**
- **Value:** HIGH - Tests production-critical rate limiter
- **Cost:** MEDIUM - 420 lines, but well-organized and focused
- **Recommendation:** KEEP and fix compilation errors

**Why Not Too Complex:**
- Low data structure complexity (no hardcoded schemas/maps)
- Clear test organization by feature (token bucket, burst, priority)
- Focused on edge cases that matter in production
- Tests real business logic (rate limiting), not external libraries

---

### 3. erlmcp_registry_dist_tests.erl (281 lines)

**Status:** ✅ COMPLEX BUT VALUABLE - KEEP

**Complexity Metrics:**
- 281 total lines
- 15 test functions
- 35 assertions
- 57 process/external calls (HIGHEST)
- 2 setup/teardown blocks
- 3 complex data structures

**Why It's Valuable:**
1. **Distributed systems** - Tests critical gproc integration
2. **Process monitoring** - Tests cleanup on process death (important for no memory leaks)
3. **Real distributed testing** - Tests gproc global registration, not just mocks
4. **Integration coverage** - Tests registry_dist + registry integration
5. **Cluster simulation** - Tests cluster node queries

**Value vs. Cost:**
- **Value:** HIGH - Tests distributed registry (critical for multi-node deployments)
- **Cost:** MEDIUM - 281 lines, 57 process calls (necessary for distributed testing)
- **Recommendation:** KEEP and fix compilation errors

**Why Not Too Complex:**
- Process complexity is NECESSARY (testing distributed systems)
- Well-organized test groups (basic, local global, integration, cluster, monitoring)
- Tests real business logic (distributed registry), not external libraries
- 281 lines is reasonable for distributed systems testing

---

### 4. erlmcp_supervisor_collapse_tests.erl (288 lines)

**Status:** ⚠️ BORDERLINE - SIMPLIFY

**Complexity Metrics:**
- 288 total lines
- 14 test functions
- 22 assertions
- 29 process/external calls (HIGH)
- 2 setup/teardown blocks
- 0 complex data structures

**Analysis:**
1. **Tests zombie prevention** - Important for supervisor hygiene
2. **Tests intensity limits** - Important for restart cascades
3. **Tests dynamic child management** - Important for cluster enable/disable
4. **Tests shutdown cascades** - Important for clean shutdown
5. **Has mock init/1** - Duplicates erlmcp_cluster_sup logic (28 lines)

**Value vs. Cost:**
- **Value:** MEDIUM - Tests supervisor behavior, but some redundancy
- **Cost:** MEDIUM - 288 lines, includes duplicate init/1 logic
- **Recommendation:** SIMPLIFY by removing mock init/1, merge redundant tests

**Simplification Strategy:**
```erlang
%% Remove: init_mock() (lines 246-288, 43 lines)
%% Remove: test_intensity_configuration/0 (duplicate)
%% Remove: test_period_configuration/0 (duplicate)
%% Merge: zombie prevention tests (4 tests → 2 tests)
%% Target: Reduce from 288 lines to ~180 lines
```

---

### 5. erlmcp_capability_negotiation_tests.erl (411 lines)

**Status:** ❌ OVERLY COMPLEX - REFACTOR

**Complexity Metrics:**
- 411 total lines
- 29 test functions (MOST)
- 84 assertions (MOST)
- 0 process calls (low process complexity)
- 0 setup/teardown blocks
- 0 complex data structures (simple record constructions)

**Why It's Overly Complex:**
1. **Test explosion** - 29 test functions for a 1253-line module
2. **Redundant tests** - Multiple tests for same function with slight variations
3. **Low process complexity** - Pure functions, no process setup needed
4. **Over-specified** - Tests implementation details (record fields) vs. behavior
5. **Poor organization** - 29 flat test functions, no test generators

**Value vs. Cost:**
- **Value:** MEDIUM - Tests important capability negotiation logic
- **Cost:** HIGH - 411 lines, 84 assertions, many redundant tests
- **Recommendation:** REFACTOR to reduce by 50%

**Simplification Strategy:**
```erlang
%% Group related tests:
%% 1. Extract client caps (2 tests → 1 test with ?_testliste)
%% 2. Extract server caps (2 tests → 1 test with ?_testliste)
%% 3. Capability to map (2 tests → 1 parametrized test)
%% 4. Protocol validation (2 tests → 1 parametrized test)
%% 5. Merge capability tests (2 tests → 1 parametrized test)
%% 6. Has capability tests (3 tests → 1 parametrized test)
%%
%% Use test generators to reduce duplication:
%% server_caps_test_() ->
%%     {setup, fun setup/0, fun cleanup/1,
%%      [?_test(extract_full), ?_test(extract_minimal), ...]}.
%%
%% Target: Reduce from 411 lines to ~200 lines (50% reduction)
```

---

## Recommendations Summary

### DELETE (1 file)
1. **erlmcp_schema_validator_tests.erl** - Module doesn't exist, tests external library, 562 lines of low-value tests

### REFACTOR (2 files)
1. **erlmcp_supervisor_collapse_tests.erl** - Remove duplicate init/1, merge redundant tests (288 → 180 lines)
2. **erlmcp_capability_negotiation_tests.erl** - Use test generators, parametrize tests (411 → 200 lines)

### KEEP (2 files)
1. **erlmcp_rate_limit_edge_case_tests.erl** - Well-organized, tests critical rate limiter edge cases
2. **erlmcp_registry_dist_tests.erl** - Tests distributed systems, process complexity is necessary

---

## Complexity Heuristics Used

### Signs of Overly Complex Tests
- **>500 lines** for a single test module (schema_validator: 562 lines)
- **>30 test functions** without test generators (capability: 29 tests)
- **Tests external libraries** instead of business logic (schema_validator tests jesse)
- **Module under test doesn't exist** (schema_validator.erl missing)
- **Massive data duplication** (40+ hardcoded schemas in schema_validator)
- **>3 assertions per test** average (capability: 84 assertions / 29 tests = 2.9 avg)

### Signs of Valuable Complexity
- **Tests critical infrastructure** (rate limiter, distributed registry)
- **Tests real failure modes** (token bucket precision, process death cleanup)
- **Process complexity is necessary** (distributed systems require real processes)
- **Well-organized test groups** (clear sections for precision, burst, priority, etc.)
- **Tests business logic** not external libraries
- **Production safety value** (prevents DoS, memory leaks, cascade failures)

---

## Action Plan

### Immediate Actions
1. **Delete** `erlmcp_schema_validator_tests.erl` (562 lines saved)
2. **Fix** `erlmcp_rate_limit_edge_case_tests.erl` (fix compilation, keep as-is)
3. **Fix** `erlmcp_registry_dist_tests.erl` (fix compilation, keep as-is)

### Refactoring Actions
4. **Simplify** `erlmcp_supervisor_collapse_tests.erl` (remove 43-line init/1 mock)
5. **Refactor** `erlmcp_capability_negotiation_tests.erl` (use test generators, reduce by 50%)

### Expected Impact
- **Lines removed:** 562 (delete) + 108 (supervisor) + 211 (capability) = **881 lines removed**
- **Lines kept:** 420 (rate_limit) + 281 (registry_dist) = **701 lines kept**
- **Net reduction:** 1,962 → 881 lines (**55% reduction**)
- **Test coverage:** Maintained or improved (focus on valuable tests)

---

## Test Complexity Best Practices

### DO
- Keep test modules **under 300 lines**
- Use **test generators** for similar tests
- Test **business logic**, not external libraries
- Use **parametrized tests** for data variations
- Keep **assertions per test** under 5
- Use **setup/teardown** for common initialization

### DON'T
- Write **>500-line** test modules
- Hardcode **complex data structures** in tests
- Test **external library behavior** (jesse, gproc, etc.)
- Duplicate **production logic** in test mocks
- Write **>30 flat tests** without organization
- Test **implementation details** (record fields, etc.)

---

## Conclusion

**Key Finding:** 2 of 5 broken tests are overly complex and should be deleted/refactored.

**Recommendation:** Focus on fixing the 2 valuable tests (rate_limit_edge_case, registry_dist) and simplify/delete the other 3 to reduce maintenance burden by 55%.

**Next Steps:**
1. Delete erlmcp_schema_validator_tests.erl (module doesn't exist)
2. Fix and keep erlmcp_rate_limit_edge_case_tests.erl
3. Fix and keep erlmcp_registry_dist_tests.erl
4. Refactor erlmcp_supervisor_collapse_tests.erl (remove duplicate code)
5. Refactor erlmcp_capability_negotiation_tests.erl (use test generators)
