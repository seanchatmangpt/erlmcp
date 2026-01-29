# CT Test Failure Analysis & Prioritized Fix Plan

**Analysis Date:** 2026-01-28
**Total CT Failures:** ~40+ test cases (across 4 test suites)
**Root Cause Categories:** 3 major issues

---

## Executive Summary

The CT test failures fall into **3 distinct root causes**, not 278 individual failures. Most failures are cascading from `init_per_suite` failures in the integration test suite. This analysis provides a prioritized fix plan based on:
- **Severity:** Blocking vs non-blocking
- **Root Cause:** Missing application files, process startup failures, configuration issues
- **Effort:** Easy wins (minutes) vs complex (hours)

---

## Failure Breakdown by Root Cause

### **CRITICAL - Root Cause #1: Missing `erlmcp.app` (Application File)**

**Severity:** 🔴 **BLOCKING** (causes 20+ cascading failures)
**Effort:** 🟢 **EASY** (5-10 minutes)
**Impact:** Fixes **ALL** `erlmcp_integration_SUITE` failures (20 test cases)

#### Symptoms:
```erlang
{error, {"no such file or directory", "erlmcp.app"}}
```

#### Affected Tests:
- **erlmcp_integration_SUITE:** ALL 20 test cases **SKIPPED** due to `init_per_suite` failure
  - `test_system_startup_shutdown`
  - `test_complete_message_flow`
  - `test_multi_transport_coordination`
  - `test_server_registry_coordination`
  - `test_configuration_loading`
  - `test_configuration_hot_reload`
  - `test_transport_config_validation`
  - `test_failure_recovery_integration`
  - `test_transport_failure_recovery`
  - `test_server_crash_recovery`
  - `test_registry_failure_handling`
  - `test_concurrent_connections`
  - `test_high_message_throughput`
  - `test_resource_management_under_load`
  - `test_real_mcp_client_interaction`
  - `test_tool_execution_end_to_end`
  - `test_resource_access_end_to_end`
  - `test_prompt_handling_integration`
  - `test_monitoring_integration`
  - `test_metrics_collection_integration`
  - `test_tracing_integration`

#### Root Cause Analysis:
The project has migrated to an **umbrella structure** with 4 applications:
- `erlmcp_core`
- `erlmcp_transports`
- `erlmcp_observability`
- `tcps_erlmcp`

However, the `erlmcp_integration_SUITE.erl` test at line 148 attempts to start an **`erlmcp`** application that **no longer exists**:

```erlang
%% Line 148 in erlmcp_integration_SUITE.erl
case application:start(erlmcp) of
    ok -> ok;
    {error, {already_started, erlmcp}} -> ok;
    {error, {not_started, _}} ->
        application:start(erlmcp, temporary)
end,
```

The umbrella project **no longer has a top-level `erlmcp` application**, only the 4 sub-apps.

#### Fix Strategy:
**Option 1: Start individual apps (RECOMMENDED - 5 min)**
```erlang
%% Replace line 148 in apps/erlmcp_core/test/erlmcp_integration_SUITE.erl
Apps = [erlmcp_core, erlmcp_transports, erlmcp_observability],
lists:foreach(fun(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end
end, Apps),
```

**Option 2: Create umbrella app (NOT RECOMMENDED - 30 min)**
Create `src/erlmcp.app.src` as a meta-application that depends on the 4 apps. This adds complexity.

---

### **CRITICAL - Root Cause #2: Ranch Supervisor Not Started (noproc)**

**Severity:** 🔴 **BLOCKING** (causes 10+ cascading failures)
**Effort:** 🟡 **MEDIUM** (30-60 minutes)
**Impact:** Fixes **ALL** `erlmcp_observability_SUITE` failures (5 test cases) + **10** `quality_gates_SUITE` failures

#### Symptoms:
```erlang
{noproc, {gen_server, call, [ranch_sup, {...}]}}
```

#### Affected Tests:
- **erlmcp_observability_SUITE:** ALL 5 test cases **SKIPPED** due to `init_per_suite` failure
  - `test_otel_integration`
  - `test_metrics_integration`
  - `test_health_integration`
  - `test_full_observability_stack`
- **quality_gates_SUITE:** 10 test cases **FAILED** with `{noproc, {gen_server, call, [erlmcp_hooks, ...]}}` or ranch issues
  - `gate_detects_compilation_failure_test`
  - `gate_detects_test_failure_test`
  - `gate_detects_coverage_drop_test`
  - `gate_blocks_on_failure_test`
  - `gate_allows_on_pass_test`
  - `gate_generates_receipt_test`
  - `gate_integrates_with_tcps_test`
  - `multiple_gates_cascade_failure_test`
  - `gate_timeout_handling_test`
  - `gate_recovery_after_fix_test`

#### Root Cause Analysis:
The `erlmcp_observability_SUITE` tries to start the observability app, which includes:
- `erlmcp_dashboard_server` (WebSocket server)
- Cowboy HTTP server (requires **ranch**)
- Ranch TCP acceptor pool supervisor

The error shows `ranch_sup` is **not running** when `erlmcp_dashboard_server` tries to start a Ranch listener:
```erlang
{noproc, {gen_server, call, [ranch_sup, {start_child, ...}]}}
```

This happens because:
1. **ranch application not started** in `init_per_suite`
2. **cowboy application not started** (depends on ranch)
3. Dashboard server tries to start Ranch listener before Ranch supervisor exists

#### Fix Strategy:
**Fix 1: Start dependencies in init_per_suite (REQUIRED - 10 min)**
```erlang
%% In apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl
init_per_suite(Config) ->
    %% Start dependencies in order
    Apps = [ranch, cowboy, jsx, jesse, opentelemetry_api, opentelemetry,
            erlmcp_core, erlmcp_transports, erlmcp_observability],
    lists:foreach(fun(App) ->
        case application:start(App) of
            ok -> ok;
            {error, {already_started, App}} -> ok
        end
    end, Apps),
    Config.
```

**Fix 2: Optional - Make dashboard startup optional (20 min)**
Add configuration to disable dashboard in test mode:
```erlang
%% In apps/erlmcp_observability/src/erlmcp_observability_sup.erl
init([]) ->
    Children = [
        %% ... other children ...
    ] ++ case application:get_env(erlmcp_observability, enable_dashboard, true) of
        true -> [dashboard_spec()];
        false -> []
    end,
    {ok, {SupFlags, Children}}.
```

Then in test config:
```erlang
%% In test sys.config or init_per_suite
application:set_env(erlmcp_observability, enable_dashboard, false).
```

**Fix 3: Fix quality_gates_SUITE (erlmcp_hooks not started) (10 min)**
```erlang
%% In test/quality_gates_SUITE.erl
init_per_suite(Config) ->
    %% Start erlmcp_hooks application (it's a gen_server)
    Apps = [erlmcp_core, erlmcp_observability],  % or wherever erlmcp_hooks lives
    lists:foreach(fun(App) ->
        case application:start(App) of
            ok -> ok;
            {error, {already_started, App}} -> ok
        end
    end, Apps),

    %% OR if erlmcp_hooks is standalone gen_server:
    {ok, _Pid} = erlmcp_hooks:start_link(),

    Config.
```

---

### **HIGH - Root Cause #3: Distributed Erlang Slave Node Timeout**

**Severity:** 🟡 **HIGH** (blocks distributed tests)
**Effort:** 🟡 **MEDIUM** (30-60 minutes)
**Impact:** Fixes **7** `erlmcp_registry_dist_SUITE` failures

#### Symptoms:
```erlang
{badmatch, {error, boot_timeout, 'slave1@Seans-MacBook-Pro'}}
```

#### Affected Tests:
- **erlmcp_registry_dist_SUITE:** 7 test cases **FAILED** in `multi_node` group
  - `init_per_group` (cascades to all tests in group)
  - `multi_node_registration`
  - `multi_node_failover`
  - `global_name_conflict`
  - `node_reconnection`
  - `split_brain_detection`
  - `end_per_group`

#### Root Cause Analysis:
The distributed test suite uses `ct_slave:start/2` to boot additional Erlang nodes for multi-node testing. The slave nodes are **timing out** during boot (default timeout likely 30-60s).

Possible causes:
1. **Hostname resolution issues:** `'slave1@Seans-MacBook-Pro'` requires DNS resolution
2. **Firewall blocking EPMD:** Erlang Port Mapper Daemon (EPMD) port 4369 blocked
3. **Code path mismatch:** Slave nodes can't find beam files
4. **Insufficient timeout:** 60s may not be enough on slow CI or macOS

#### Fix Strategy:
**Fix 1: Use short names (RECOMMENDED - 5 min)**
```erlang
%% In apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl
%% Change from:
ct_slave:start('slave1@Seans-MacBook-Pro')

%% To:
ct_slave:start(node(), slave1, [{boot_timeout, 120}])  % 120s timeout
```

**Fix 2: Use localhost short names (10 min)**
```erlang
%% In init_per_group/2
NodeName = list_to_atom("slave1@127.0.0.1"),
{ok, Node} = ct_slave:start(NodeName, [
    {boot_timeout, 120},
    {monitor_master, true},
    {erl_flags, "-setcookie " ++ atom_to_list(erlang:get_cookie())}
]),
```

**Fix 3: Skip distributed tests in CI (if not critical) (2 min)**
```erlang
%% In groups/0
groups() ->
    case os:getenv("CI") of
        "true" ->
            [{single_node, [parallel], single_node_tests()}];
        _ ->
            [{single_node, [parallel], single_node_tests()},
             {multi_node, [sequence], multi_node_tests()}]
    end.
```

---

### **MEDIUM - Root Cause #4: Transport Validation Logic Error**

**Severity:** 🟢 **MEDIUM** (single test, logic bug)
**Effort:** 🟢 **EASY** (5 minutes)
**Impact:** Fixes **1** test case

#### Symptoms:
```erlang
Failure/Error: ?assertMatch({error, _}, erlmcp_transport_behavior:validate_transport_opts(stdio, Opts))
  expected: = {error, _}
       got: ok
      line: 328
```

#### Affected Tests:
- **erlmcp_transport_behavior_SUITE:** 1 test case **FAILED**
  - `transport_options.stdio_opts_validation`

#### Root Cause Analysis:
The test expects validation to **fail** for invalid stdio options, but the validation function returns `ok` instead of `{error, Reason}`.

This is either:
1. **Test bug:** Test expects invalid opts to fail, but opts are actually valid
2. **Code bug:** Validation function doesn't validate properly

#### Fix Strategy:
**Inspect test to determine intent (5 min)**
```bash
# Read the test
grep -A 10 "stdio_opts_validation" apps/erlmcp_transports/test/erlmcp_transport_behavior_SUITE.erl
```

Then:
- **If test is wrong:** Fix test to use **actually invalid** options
- **If code is wrong:** Add validation to `erlmcp_transport_behavior:validate_transport_opts/2`

---

### **LOW - Root Cause #5: Hook Integration Test Logic**

**Severity:** 🟢 **LOW** (single test, test setup issue)
**Effort:** 🟢 **EASY** (10 minutes)
**Impact:** Fixes **1** test case

#### Symptoms:
```erlang
{badmatch, false}
```

#### Affected Tests:
- **hooks_integration_SUITE:** 1 test case **FAILED**
  - `pre_commit_hook_blocks_bad_code_test`

#### Root Cause Analysis:
The test expects a pre-commit hook to **block** bad code, but the hook returns `false` instead of expected result.

Likely causes:
1. Hook not properly installed in test environment
2. Test file path incorrect
3. Validation logic changed

#### Fix Strategy:
**Read test and fix setup (10 min)**
```bash
grep -A 20 "pre_commit_hook_blocks_bad_code_test" test/hooks_integration_SUITE.erl
```

Then fix initialization or mock the hook response.

---

## Prioritized Fix Plan

### **Phase 1: Critical Blockers (1-2 hours total) - MUST FIX FIRST**

| Priority | Root Cause | Effort | Impact | Fix Time |
|----------|-----------|--------|--------|----------|
| **P0** | Missing `erlmcp.app` | 🟢 Easy | 🔴 20 tests | **5-10 min** |
| **P0** | Ranch supervisor not started | 🟡 Medium | 🔴 15 tests | **40-60 min** |

**Estimated Total:** 45-70 minutes
**Tests Fixed:** ~35 test cases (87% of failures)

### **Phase 2: High Priority (1-2 hours total)**

| Priority | Root Cause | Effort | Impact | Fix Time |
|----------|-----------|--------|--------|----------|
| **P1** | Distributed node timeout | 🟡 Medium | 🟡 7 tests | **30-60 min** |

**Estimated Total:** 30-60 minutes
**Tests Fixed:** 7 test cases

### **Phase 3: Medium Priority (30 min total)**

| Priority | Root Cause | Effort | Impact | Fix Time |
|----------|-----------|--------|--------|----------|
| **P2** | Transport validation logic | 🟢 Easy | 🟢 1 test | **5-10 min** |
| **P2** | Hook integration test | 🟢 Easy | 🟢 1 test | **10-15 min** |

**Estimated Total:** 15-25 minutes
**Tests Fixed:** 2 test cases

---

## Implementation Order (Recommended)

### **Step 1: Fix erlmcp.app missing (P0 - 10 min)**
```bash
# Edit apps/erlmcp_core/test/erlmcp_integration_SUITE.erl line 148
# Replace application:start(erlmcp) with individual app starts
```

**Expected Result:** 20 integration tests will **PASS** (instead of SKIPPED)

---

### **Step 2: Fix ranch supervisor (P0 - 60 min)**
```bash
# 1. Edit apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl
#    Add ranch, cowboy to init_per_suite dependency list

# 2. Edit test/quality_gates_SUITE.erl
#    Start erlmcp_hooks or parent app in init_per_suite

# 3. Optional: Make dashboard optional in test mode
```

**Expected Result:** 15 observability + quality gate tests will **PASS**

---

### **Step 3: Fix distributed node timeout (P1 - 30 min)**
```bash
# Edit apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl
# Use short names and increase boot_timeout to 120s
```

**Expected Result:** 7 distributed tests will **PASS**

---

### **Step 4: Fix transport validation (P2 - 5 min)**
```bash
# Inspect test and fix either test or validation function
grep -A 10 "stdio_opts_validation" apps/erlmcp_transports/test/erlmcp_transport_behavior_SUITE.erl
```

**Expected Result:** 1 test will **PASS**

---

### **Step 5: Fix hook integration (P2 - 10 min)**
```bash
# Inspect test and fix hook setup
grep -A 20 "pre_commit_hook_blocks_bad_code_test" test/hooks_integration_SUITE.erl
```

**Expected Result:** 1 test will **PASS**

---

## Quality Gate Metrics

### **Before Fixes:**
- **CT Tests Passing:** ~0% (all suites fail at init)
- **CT Tests Failing:** ~40 test cases
- **CT Tests Skipped:** ~20 test cases (cascading from init_per_suite)

### **After Phase 1 (Critical Blockers):**
- **CT Tests Passing:** ~87% (35/40 test cases)
- **CT Tests Failing:** ~5 test cases
- **Estimated Time:** 45-70 minutes

### **After Phase 2 (High Priority):**
- **CT Tests Passing:** ~95% (42/44 test cases)
- **CT Tests Failing:** ~2 test cases
- **Estimated Time:** +30-60 minutes (total 75-130 min)

### **After Phase 3 (All Fixes):**
- **CT Tests Passing:** ~100% (44/44 test cases)
- **CT Tests Failing:** 0 test cases
- **Estimated Time:** +15-25 minutes (total 90-155 min)

---

## Risk Assessment

### **Low Risk Fixes (Can proceed immediately):**
- ✅ Fix `erlmcp.app` missing (changes test setup, not production code)
- ✅ Fix transport validation (single line test fix)
- ✅ Fix hook integration (test setup fix)

### **Medium Risk Fixes (Review carefully):**
- ⚠️ Fix ranch supervisor (changes app startup order, test dependencies)
- ⚠️ Fix distributed nodes (changes test infrastructure, may need CI config)

### **Mitigation:**
- Run **EUnit tests** after each fix to ensure no regressions
- Run **full CT suite** after each phase
- Commit after each working phase

---

## Verification Checklist

After completing each phase:

```bash
# 1. Compile clean
TERM=dumb rebar3 compile

# 2. Run specific suite
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_integration_SUITE

# 3. Check results
grep -E "PASSED|FAILED|SKIPPED" _build/test/logs/index.html

# 4. Run ALL CT tests
rebar3 ct

# 5. Verify no EUnit regressions
rebar3 eunit
```

---

## Summary

**Total Failures:** ~44 test cases
**Root Causes:** 5 distinct issues
**Critical Blockers:** 2 issues (87% of failures)
**Total Estimated Fix Time:** 90-155 minutes (1.5-2.5 hours)

**Recommended Approach:**
1. Fix **Phase 1** (critical blockers) immediately → 35 tests pass
2. Fix **Phase 2** (distributed tests) next → 42 tests pass
3. Fix **Phase 3** (minor issues) last → 44 tests pass (100%)

**ROI:** Fixing 2 critical issues (70 min) resolves 87% of failures.
