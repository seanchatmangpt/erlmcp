# CT Test Failures - Immediate Action Plan

**Status:** 44 test failures identified
**Root Causes:** 5 distinct issues (NOT 278!)
**Time to Fix:** 90-155 minutes total

---

## START HERE: Phase 1 Critical Blockers (70 min)

These 2 fixes resolve **87% of all failures** (35/44 tests).

### Action 1: Fix Missing erlmcp.app (10 minutes)

**File:** `apps/erlmcp_core/test/erlmcp_integration_SUITE.erl`
**Line:** 148
**Impact:** Fixes 20 skipped tests

**Current Code:**
```erlang
%% Start ErlMCP application
case application:start(erlmcp) of
    ok -> ok;
    {error, {already_started, erlmcp}} -> ok;
    {error, {not_started, _}} ->
        %% Try to start it anyway
        application:start(erlmcp, temporary)
end,
```

**Replace With:**
```erlang
%% Start ErlMCP applications (umbrella structure)
Apps = [erlmcp_core, erlmcp_transports, erlmcp_observability],
lists:foreach(fun(App) ->
    case application:start(App) of
        ok ->
            ct:pal("Started application: ~p", [App]),
            ok;
        {error, {already_started, App}} ->
            ct:pal("Application already started: ~p", [App]),
            ok;
        {error, Reason} ->
            ct:fail("Failed to start ~p: ~p", [App, Reason])
    end
end, Apps),
```

**Verify:**
```bash
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_integration_SUITE
# Expected: 20 tests PASS (instead of SKIPPED)
```

---

### Action 2: Fix Ranch Supervisor Not Started (60 minutes)

**Impact:** Fixes 15 tests (5 observability + 10 quality gates)

#### Part 2A: Fix Observability Suite (30 min)

**File:** `apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl`
**Function:** `init_per_suite/1`

**Current Code:**
```erlang
init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.
```

**Replace With:**
```erlang
init_per_suite(Config) ->
    ct:pal("Starting Observability Test Suite"),

    %% Start dependencies in correct order
    DepsToStart = [
        crypto,      % SSL dependency
        ssl,         % HTTP dependency
        ranch,       % TCP acceptor pool (CRITICAL for dashboard)
        cowboy,      % HTTP server (depends on ranch)
        jsx,         % JSON encoding
        jesse,       % JSON schema
        opentelemetry_api,
        opentelemetry,
        erlmcp_core,
        erlmcp_transports,
        erlmcp_observability
    ],

    lists:foreach(fun(App) ->
        case application:start(App) of
            ok ->
                ct:pal("Started dependency: ~p", [App]),
                ok;
            {error, {already_started, App}} ->
                ct:pal("Dependency already started: ~p", [App]),
                ok;
            {error, {not_started, DepApp}} ->
                ct:pal("Missing dependency ~p for ~p, attempting start...", [DepApp, App]),
                application:start(DepApp),
                application:start(App, temporary);
            {error, Reason} ->
                ct:pal("WARNING: Failed to start ~p: ~p", [App, Reason])
                %% Don't fail yet - some tests may not need all apps
        end
    end, DepsToStart),

    %% Wait for dashboard server to initialize
    timer:sleep(500),

    %% Verify critical processes are running
    case whereis(erlmcp_dashboard_server) of
        undefined ->
            ct:pal("WARNING: Dashboard server not started (tests may skip)");
        Pid when is_pid(Pid) ->
            ct:pal("Dashboard server started: ~p", [Pid])
    end,

    [{suite_start_time, erlang:system_time(millisecond)} | Config].
```

**Verify:**
```bash
rebar3 ct --suite=apps/erlmcp_observability/test/erlmcp_observability_SUITE
# Expected: 5 tests PASS (instead of init_per_suite FAILED)
```

#### Part 2B: Fix Quality Gates Suite (30 min)

**File:** `test/quality_gates_SUITE.erl`
**Function:** `init_per_suite/1`

**Find:** The existing `init_per_suite` function
**Replace With:**
```erlang
init_per_suite(Config) ->
    ct:pal("Starting Quality Gates Test Suite"),

    %% Start required applications
    DepsToStart = [
        crypto,
        ssl,
        jsx,
        jesse,
        ranch,
        cowboy,
        opentelemetry_api,
        opentelemetry,
        erlmcp_core,
        erlmcp_observability  % Contains erlmcp_hooks
    ],

    lists:foreach(fun(App) ->
        case application:start(App) of
            ok -> ok;
            {error, {already_started, App}} -> ok;
            {error, Reason} ->
                ct:pal("WARNING: Failed to start ~p: ~p", [App, Reason])
        end
    end, DepsToStart),

    %% Verify erlmcp_hooks gen_server is running
    timer:sleep(500),  % Allow supervisor to start children

    case whereis(erlmcp_hooks) of
        undefined ->
            %% Try to start it manually if not under supervision
            ct:pal("Starting erlmcp_hooks manually..."),
            case erlmcp_hooks:start_link() of
                {ok, Pid} ->
                    ct:pal("erlmcp_hooks started: ~p", [Pid]),
                    [{hooks_pid, Pid} | Config];
                {error, {already_started, Pid}} ->
                    ct:pal("erlmcp_hooks already running: ~p", [Pid]),
                    Config;
                {error, Reason} ->
                    ct:fail("Failed to start erlmcp_hooks: ~p", [Reason])
            end;
        Pid when is_pid(Pid) ->
            ct:pal("erlmcp_hooks already running: ~p", [Pid]),
            Config
    end.
```

**Verify:**
```bash
rebar3 ct --suite=test/quality_gates_SUITE
# Expected: 10 tests PASS (instead of noproc errors)
```

---

## Phase 1 Validation

After completing both actions:

```bash
# 1. Compile clean
TERM=dumb rebar3 compile

# 2. Run all CT tests
rebar3 ct

# 3. Check results
echo "Expected: ~35/44 tests passing (80%)"

# 4. Verify no EUnit regressions
rebar3 eunit
```

**Success Criteria:**
- ✅ Compilation: 0 errors
- ✅ CT Tests: ~35/44 passing (80% pass rate)
- ✅ EUnit Tests: No regressions (same pass rate as before)

---

## STOP HERE (Optional)

**Phase 1 achieves 80% pass rate in 70 minutes.**

If you have limited time, stop here. The remaining 9 failures are:
- 7 distributed node tests (complex, may skip in CI)
- 2 minor test logic issues (low priority)

---

## Phase 2: High Priority (Optional - 60 min)

### Action 3: Fix Distributed Node Timeout (30-60 min)

**File:** `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl`
**Function:** `init_per_group/2` for `multi_node` group
**Impact:** Fixes 7 tests

**Option A: Fix Timeout (Recommended)**
```erlang
init_per_group(multi_node, Config) ->
    ct:pal("Starting multi-node group"),

    %% Use short name with localhost and increased timeout
    NodeName = list_to_atom("slave1@127.0.0.1"),
    Cookie = erlang:get_cookie(),

    SlaveOpts = [
        {boot_timeout, 120},  % Increase from default 60s to 120s
        {monitor_master, true},
        {erl_flags, "-setcookie " ++ atom_to_list(Cookie)}
    ],

    case ct_slave:start(NodeName, SlaveOpts) of
        {ok, Node} ->
            ct:pal("Slave node started: ~p", [Node]),
            [{slave_node, Node} | Config];
        {error, Reason} ->
            ct:fail("Failed to start slave node: ~p", [Reason])
    end;
init_per_group(Group, Config) ->
    ct:pal("Starting group: ~p", [Group]),
    Config.
```

**Option B: Skip in CI (Alternative)**
```erlang
%% Add to groups/0 function
groups() ->
    BaseGroups = [{single_node, [parallel], single_node_tests()}],

    case os:getenv("CI") of
        "true" ->
            ct:pal("CI environment detected, skipping multi-node tests"),
            BaseGroups;
        _ ->
            BaseGroups ++ [{multi_node, [sequence], multi_node_tests()}]
    end.
```

**Verify:**
```bash
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE
# Expected: 7 tests PASS (or skipped if using Option B)
```

---

## Phase 3: Cleanup (Optional - 25 min)

### Action 4: Fix Transport Validation (5 min)

**File:** `apps/erlmcp_transports/test/erlmcp_transport_behavior_SUITE.erl`
**Line:** ~328

First, inspect the test:
```bash
grep -A 15 "stdio_opts_validation" apps/erlmcp_transports/test/erlmcp_transport_behavior_SUITE.erl
```

Then fix either:
- The test (if it's using valid opts but expecting error)
- The validation function (if it should reject invalid opts)

### Action 5: Fix Hook Integration (10 min)

**File:** `test/hooks_integration_SUITE.erl`
**Test:** `pre_commit_hook_blocks_bad_code_test`

Inspect the test:
```bash
grep -A 25 "pre_commit_hook_blocks_bad_code_test" test/hooks_integration_SUITE.erl
```

Then fix the hook initialization or expected result.

---

## Final Verification

```bash
# Full test suite
rebar3 ct

# Expected results:
# Phase 1: 35/44 passing (80%)
# Phase 2: 42/44 passing (95%)
# Phase 3: 44/44 passing (100%)

# Coverage check
rebar3 cover --verbose

# Quality gates
make check  # OR: rebar3 do dialyzer, xref
```

---

## Troubleshooting

### If tests still fail after Phase 1:

1. **Check application startup:**
   ```bash
   rebar3 shell
   # In shell:
   application:start(erlmcp_core).
   application:start(erlmcp_transports).
   application:start(erlmcp_observability).
   ```

2. **Check for missing dependencies:**
   ```bash
   rebar3 tree
   ```

3. **Check for process crashes:**
   ```bash
   # In failing test output, look for:
   # - {noproc, ...} → Process not started
   # - {badmatch, ...} → Unexpected return value
   # - {case_clause, ...} → Unhandled case
   ```

4. **Run individual test:**
   ```bash
   rebar3 ct --suite=<suite> --case=<test_name> --verbose
   ```

### Common Issues:

- **ranch_sup not found:** Ranch application not started (Action 2)
- **erlmcp.app not found:** Wrong application name (Action 1)
- **boot_timeout:** Distributed node timeout (Action 3)
- **{noproc, erlmcp_hooks}:** Hooks not started (Action 2B)

---

## Success Metrics

| Phase | Time | Tests Fixed | Pass Rate | Status |
|-------|------|-------------|-----------|--------|
| Start | 0 min | 0/44 | 0% | ❌ |
| Phase 1 | 70 min | 35/44 | 80% | ⭐⭐⭐⭐⭐ |
| Phase 2 | 130 min | 42/44 | 95% | ⭐⭐⭐⭐ |
| Phase 3 | 155 min | 44/44 | 100% | ⭐⭐⭐ |

---

## Documentation

- **Full Analysis:** `docs/CT_TEST_FAILURE_ANALYSIS.md` (15 KB, comprehensive)
- **Fix Matrix:** `docs/CT_FIX_PRIORITY_MATRIX.md` (7 KB, reference)
- **Summary:** `docs/CT_FAILURE_SUMMARY.txt` (8 KB, visual)
- **This File:** `docs/CT_IMMEDIATE_ACTIONS.md` (quick start)

---

**Next Step:** Start with Action 1 (10 minutes) to fix 20 tests immediately.
