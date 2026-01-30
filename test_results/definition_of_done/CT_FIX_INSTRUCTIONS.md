# Common Test Suite Fix Instructions

**Goal:** Fix all CT failures and achieve 80%+ pass rate

---

## Fix 1: Process Startup Dependencies (erlmcp_integration_SUITE)

**File:** `apps/erlmcp_core/test/erlmcp_integration_SUITE.erl`

**Problem:** Tests fail because `erlmcp_tasks` and `erlmcp_auth` processes are not started

**Current Code (BROKEN):**
```erlang
init_per_suite(Config) ->
    %% Start real erlmcp application (Chicago School: real system)
    application:ensure_all_started(erlmcp),
    Config.
```

**Fixed Code:**
```erlang
init_per_suite(Config) ->
    %% Start real erlmcp application (Chicago School: real system)
    application:ensure_all_started(erlmcp),

    %% Start required processes for integration tests
    {ok, TasksPid} = erlmcp_tasks:start_link(#{
        worker_count => 5,
        task_timeout => 5000
    }),
    {ok, AuthPid} = erlmcp_auth:start_link(#{
        rate_limiter_enabled => true,
        session_timeout => 3600
    }),

    %% Store PIDs in config for cleanup
    [{tasks_pid, TasksPid}, {auth_pid, AuthPid} | Config].

end_per_suite(Config) ->
    %% Clean up processes
    TasksPid = proplists:get_value(tasks_pid, Config),
    AuthPid = proplists:get_value(auth_pid, Config),

    case is_pid(TasksPid) andalso is_process_alive(TasksPid) of
        true -> gen_server:stop(TasksPid);
        false -> ok
    end,

    case is_pid(AuthPid) andalso is_process_alive(AuthPid) of
        true -> gen_server:stop(AuthPid);
        false -> ok
    end,

    application:stop(erlmcp),
    ok.
```

**Expected Result:** 8 tests in erlmcp_integration_SUITE will pass

**Time Estimate:** 30 minutes

---

## Fix 2: API Implementation Mismatch (erlmcp_protocol_checker_SUITE)

**Problem:** Test suite calls functions that don't exist in the implementation

**Option A: Implement Missing Functions (RECOMMENDED)**

**File:** `apps/erlmcp_validation/src/erlmcp_protocol_checker.erl`

The file already has these functions implemented (lines 182-651), but they may not be exported or may have signature mismatches.

**Check 1: Verify exports**
```bash
grep "^-export\|^%% API exports" apps/erlmcp_validation/src/erlmcp_protocol_checker.erl
```

Expected exports should include:
```erlang
-export([
    validate_initialize_request/1,
    validate_initialized_notification/1,
    validate_tools_list/1,
    validate_tools_call/1,
    validate_resources_list/1,
    validate_resources_read/1,
    validate_prompts_list/1,
    validate_prompts_get/1,
    validate_progress/1,
    validate_setLevel/1,
    validate_cancelled/1,
    validate_initialize_sequence/2
]).
```

**Check 2: Verify function signatures**

The test suite expects these signatures:
```erlang
-spec validate_initialize_request(map()) -> validation_result().
-spec validate_initialized_notification(map()) -> validation_result().
-spec validate_tools_list(map()) -> validation_result().
-spec validate_tools_call(map()) -> validation_result().
-spec validate_resources_list(map()) -> validation_result().
-spec validate_resources_read(map()) -> validation_result().
-spec validate_prompts_list(map()) -> validation_result().
-spec validate_progress(map()) -> validation_result().
-spec validate_cancelled(map()) -> validation_result().
-spec validate_initialize_sequence(map(), map()) -> validation_result().
```

**Current Status:** These functions ARE implemented in the file (lines 182-651), so the issue may be:
1. Module not recompiled after changes
2. Function signature mismatch
3. Export list missing these functions

**Fix Steps:**
1. Ensure all functions are exported
2. Recompile: `rebar3 compile`
3. Verify BEAM file: `ls -la _build/test/lib/erlmcp_validation/ebin/erlmcp_protocol_checker.beam`

**Option B: Update Test Suite (NOT RECOMMENDED)**

If the functions don't exist, update the test suite to match the actual API.

**Time Estimate:** 2-4 hours (mostly debugging)

---

## Fix 3: Enable Skipped Tests

**Problem:** 82 tests are skipped across 8 suites

**Step 1: Identify why tests are skipped**

```bash
# Find all .skip files
find apps/*/test -name "*.skip"

# Check for skip conditions in suite files
grep -r "skip\|SKIP\|{skip" apps/*/test/*_SUITE.erl
```

**Step 2: Review each skipped suite**

For each suite:
1. Remove `.skip` extension
2. Check dependencies
3. Run suite individually: `rebar3 ct --suite=SUITE_NAME`
4. Fix any failures
5. Verify pass rate

**Example: Enable erlmcp_observability_SUITE**
```bash
# Check if suite exists
ls apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl

# Run suite
rebar3 ct --suite=erlmcp_observability_SUITE

# Review failures in _build/test/logs/
```

**Expected Outcome:** 82 additional tests will run

**Time Estimate:** 4-8 hours (depends on failures found)

---

## Fix 4: Add Missing Test Cases

**Target:** Achieve 80%+ overall pass rate (142+ passing tests)

**Current:** 62 passing tests

**Gap:** 80 more tests needed

**Strategy:**
1. Prioritize high-value test areas
2. Add tests for skipped suites
3. Fix failing tests first
4. Then add new tests

**High-Priority Test Areas:**
- Transport behavior compliance (critical)
- Error handling robustness (critical)
- Performance validation (important)
- Observability features (important)

**Time Estimate:** 16-24 hours

---

## Verification Steps

After each fix, verify:

```bash
# 1. Clean build
rebar3 clean

# 2. Compile
rebar3 compile

# 3. Run CT
rebar3 ct 2>&1 | tee ct_verification.log

# 4. Check results
grep "Passed\|Failed\|Skipped" ct_verification.log

# 5. Generate report
grep -E "Passed|Failed|Skipped" ct_verification.log | tail -1
```

**Expected Final Result:**
```
Failed 0 tests. Skipped 0 (0, 0) tests. Passed 142+ tests.
```

---

## Quick Reference: Test Commands

```bash
# Run all CT suites
rebar3 ct

# Run specific suite
rebar3 ct --suite=erlmcp_integration_SUITE

# Run with verbose output
rebar3 ct --verbose

# Run and keep shell open
rebar3 ct --shell

# Run failing tests only (after first run)
rebar3 ct --retry

# Run with coverage
rebar3 ct --cover

# View HTML report
open _build/test/logs/index.html
```

---

## Progress Tracking

**Initial State (2026-01-30):**
- Total: 178 tests
- Passed: 62 (34.8%)
- Failed: 34 (19.1%)
- Skipped: 82 (46.1%)
- Status: ❌ CRITICAL FAIL

**Target State:**
- Total: 178+ tests
- Passed: 142+ (80%+)
- Failed: 0 (0%)
- Skipped: 0-10 (acceptable)
- Status: ✅ PASS

**Progress Checklist:**
- [ ] Fix process startup (8 tests)
- [ ] Fix API mismatch (10+ tests)
- [ ] Enable skipped suites (82 tests)
- [ ] Fix additional failures found
- [ ] Add missing test cases
- [ ] Verify 80%+ pass rate
- [ ] Generate final report

---

## Contact and Support

**Files to Monitor:**
- `/Users/sac/erlmcp/test_results/definition_of_done/CT_FAILURE_REPORT.md`
- `/Users/sac/erlmcp/test_results/definition_of_done/CT_FAILURE_SUMMARY.txt`
- `/Users/sac/erlmcp/_build/test/logs/index.html`

**Next Review:** After Fix 1 and Fix 2 are complete

**Estimated Total Time:** 8-16 hours of focused development work

---

**Last Updated:** 2026-01-30
**Status:** Ready for fixes
**Priority:** CRITICAL
