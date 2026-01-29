# CT Test Fix Priority Matrix

**Quick Reference Guide for Test Failures**

---

## Fix Priority Matrix

| Rank | Root Cause | Severity | Effort | Tests Fixed | Fix Time | Files to Edit |
|------|-----------|----------|--------|-------------|----------|---------------|
| **1** | Missing `erlmcp.app` | 🔴 CRITICAL | 🟢 Easy | **20** | 5-10 min | `apps/erlmcp_core/test/erlmcp_integration_SUITE.erl:148` |
| **2** | Ranch supervisor not started | 🔴 CRITICAL | 🟡 Medium | **15** | 40-60 min | `apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl:init_per_suite`<br>`test/quality_gates_SUITE.erl:init_per_suite` |
| **3** | Distributed node timeout | 🟡 HIGH | 🟡 Medium | **7** | 30-60 min | `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl:init_per_group` |
| **4** | Transport validation logic | 🟢 MEDIUM | 🟢 Easy | **1** | 5-10 min | `apps/erlmcp_transports/test/erlmcp_transport_behavior_SUITE.erl:328` |
| **5** | Hook integration test | 🟢 LOW | 🟢 Easy | **1** | 10-15 min | `test/hooks_integration_SUITE.erl:pre_commit_hook_blocks_bad_code_test` |

**Total:** 44 test cases, estimated 90-155 minutes to fix all

---

## Quick Fix Commands

### Priority 1: Missing erlmcp.app (10 min)

**Problem:** `{error, {"no such file or directory", "erlmcp.app"}}`

**Fix:**
```bash
# Edit line 148 in apps/erlmcp_core/test/erlmcp_integration_SUITE.erl
# Replace:
    case application:start(erlmcp) of
        ...
    end,

# With:
    Apps = [erlmcp_core, erlmcp_transports, erlmcp_observability],
    lists:foreach(fun(App) ->
        case application:start(App) of
            ok -> ok;
            {error, {already_started, App}} -> ok
        end
    end, Apps),
```

**Verify:**
```bash
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_integration_SUITE
# Expected: 20 tests PASS (instead of SKIPPED)
```

---

### Priority 2: Ranch Supervisor (60 min)

**Problem:** `{noproc, {gen_server, call, [ranch_sup, ...]}}`

**Fix 1: Observability Suite**
```bash
# Edit apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl
# Add to init_per_suite/1:
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

**Fix 2: Quality Gates Suite**
```bash
# Edit test/quality_gates_SUITE.erl
# Add to init_per_suite/1:
init_per_suite(Config) ->
    %% Start erlmcp_hooks or parent app
    Apps = [erlmcp_core, erlmcp_observability],
    lists:foreach(fun(App) ->
        case application:start(App) of
            ok -> ok;
            {error, {already_started, App}} -> ok
        end
    end, Apps),
    Config.
```

**Verify:**
```bash
rebar3 ct --suite=apps/erlmcp_observability/test/erlmcp_observability_SUITE
rebar3 ct --suite=test/quality_gates_SUITE
# Expected: 15 tests PASS
```

---

### Priority 3: Distributed Node Timeout (30 min)

**Problem:** `{badmatch, {error, boot_timeout, 'slave1@...'}}`

**Fix:**
```bash
# Edit apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl
# In init_per_group/2 for multi_node group:

init_per_group(multi_node, Config) ->
    %% Use short names and increased timeout
    NodeName = list_to_atom("slave1@127.0.0.1"),
    {ok, Node} = ct_slave:start(NodeName, [
        {boot_timeout, 120},  % Increase from default 60s
        {monitor_master, true},
        {erl_flags, "-setcookie " ++ atom_to_list(erlang:get_cookie())}
    ]),
    [{slave_node, Node} | Config];
```

**Alternative (skip in CI if not critical):**
```bash
# Add to groups/0:
groups() ->
    case os:getenv("CI") of
        "true" ->
            [{single_node, [parallel], single_node_tests()}];
        _ ->
            [{single_node, [parallel], single_node_tests()},
             {multi_node, [sequence], multi_node_tests()}]
    end.
```

**Verify:**
```bash
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE
# Expected: 7 tests PASS
```

---

### Priority 4: Transport Validation (5 min)

**Problem:** Test expects `{error, _}` but gets `ok`

**Fix:**
```bash
# Inspect test first
grep -A 10 "stdio_opts_validation" apps/erlmcp_transports/test/erlmcp_transport_behavior_SUITE.erl

# Then either:
# Option 1: Fix test to use actually invalid opts
# Option 2: Fix erlmcp_transport_behavior:validate_transport_opts/2 to validate properly
```

**Verify:**
```bash
rebar3 ct --suite=apps/erlmcp_transports/test/erlmcp_transport_behavior_SUITE
# Expected: 1 test PASS
```

---

### Priority 5: Hook Integration (10 min)

**Problem:** `{badmatch, false}`

**Fix:**
```bash
# Inspect test
grep -A 20 "pre_commit_hook_blocks_bad_code_test" test/hooks_integration_SUITE.erl

# Then fix hook initialization or mock response
```

**Verify:**
```bash
rebar3 ct --suite=test/hooks_integration_SUITE
# Expected: 1 test PASS
```

---

## Execution Plan (Recommended Order)

### Phase 1: Critical Blockers (70 min, 87% of failures)
1. ✅ Fix Priority 1 (10 min) → 20 tests pass
2. ✅ Fix Priority 2 (60 min) → 15 tests pass
3. ✅ **Verify:** `rebar3 ct` should show ~35/44 passing

### Phase 2: High Priority (60 min, +16% of failures)
1. ✅ Fix Priority 3 (30-60 min) → 7 tests pass
2. ✅ **Verify:** `rebar3 ct` should show ~42/44 passing

### Phase 3: Remaining Issues (25 min, +5% of failures)
1. ✅ Fix Priority 4 (5 min) → 1 test pass
2. ✅ Fix Priority 5 (10 min) → 1 test pass
3. ✅ **Verify:** `rebar3 ct` should show 44/44 passing (100%)

---

## Validation After Each Phase

```bash
# 1. Compile clean
TERM=dumb rebar3 compile

# 2. Run all CT tests
rebar3 ct

# 3. Check for regressions in EUnit
rebar3 eunit

# 4. Verify coverage
rebar3 cover --verbose

# 5. Check for errors
echo "✅ Tests passing, ❌ Tests failing"
```

---

## Success Metrics

| Phase | Time | Tests Pass | Pass Rate | ROI |
|-------|------|------------|-----------|-----|
| Before | 0 min | 0/44 | 0% | - |
| Phase 1 | 70 min | 35/44 | 80% | ⭐⭐⭐⭐⭐ |
| Phase 2 | 130 min | 42/44 | 95% | ⭐⭐⭐⭐ |
| Phase 3 | 155 min | 44/44 | 100% | ⭐⭐⭐ |

**Best ROI:** Phase 1 fixes 80% of failures in 70 minutes (1.2 hours)

---

## Risk Mitigation

- ✅ **Low Risk:** Priorities 1, 4, 5 (test setup changes only)
- ⚠️ **Medium Risk:** Priorities 2, 3 (app startup order, infrastructure)

**Mitigation Strategy:**
1. Commit after each working phase
2. Run full test suite after each fix
3. Monitor for EUnit regressions
4. Verify compilation remains clean

---

## Next Steps

1. **Read full analysis:** `docs/CT_TEST_FAILURE_ANALYSIS.md`
2. **Start with Phase 1:** Fix priorities 1 & 2
3. **Validate:** Run `rebar3 ct` after each fix
4. **Document:** Update this file with actual results
5. **Commit:** Create commits after each phase
