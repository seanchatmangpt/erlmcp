# REBAR3 EUNIT Investigation - Technical Reference

## Investigation Summary

This document contains all key findings from the investigation into why `rebar3 eunit` loads Common Test SUITE modules instead of only EUnit modules.

---

## 1. Error Analysis

### Error Message
```
Error Running EUnit Tests:
  Module `tcps_andon_integration_SUITE' not found in project.
  Module `tcps_concurrent_SUITE' not found in project.
  Module `tcps_ct_hooks' not found in project.
```

### Why This Happens

EUnit processes test modules in this order:

1. **Load Module**: Loads `tcps_andon_integration_SUITE.beam`
2. **Find Test Interface**: Looks for `test/0` or `test/1` functions
3. **Expected Interface** (EUnit):
   - `test/0` - returns list of test functions
   - `test/1` - takes module name
   - Functions ending in `_test/0` or `_test/1`
4. **Actual Interface** (CT Module):
   - `all/0` - returns list of test cases
   - `suite/0` - returns suite configuration
   - Test cases like `my_test/1`
5. **Result**: Interface mismatch → "Module not found"

The error message is misleading. The module IS found, but EUnit can't find its test interface.

---

## 2. Root Cause: Rebar3 Processing Phases

### Phase 1: Source Discovery
```erlang
%% From rebar.config
{test_dirs, ["test"]}.
{eunit_opts, [
    {exclude, ".*_SUITE$"},  % Pattern applied HERE
    verbose
]}.
```

**What Happens:**
- Rebar3 scans `test/` for `.erl` files
- Pattern `".*_SUITE$"` matches: `auto_fix_SUITE`, `hooks_integration_SUITE`, etc.
- Matched files are marked "exclude from eunit consideration"

**Result:** Source files marked for exclusion ✅

---

### Phase 2: Compilation
```erlang
%% Implicit behavior (no config option)
```

**What Happens:**
- Rebar3 COMPILES ALL .erl files discovered in test/ directories
- Files marked "exclude" are still compiled (exclusion is ignored)
- Output: `.beam` files in `_build/test/lib/*/ebin/`

**Result:** All .beam files created, including excluded ones ❌

---

### Phase 3: Beam Discovery
```erlang
%% Implicit behavior (no config option to disable this)
```

**What Happens:**
- Rebar3 scans `_build/test/lib/*/ebin/` for `.beam` files
- Finds ALL `.beam` files (no pattern matching, no exclusion applied)
- Returns full list: `[erlmcp_buffer_pool_tests, auto_fix_SUITE, ...]`

**Result:** All modules discovered ❌

---

### Phase 4: Test Execution (FAILURE)
```erlang
%% EUnit tries to execute tests
-module(erlmcp_tests).
-export([test/0]).

test() ->
    %% Test functions...
    ok.

% vs

-module(auto_fix_SUITE).
-export([all/0]).

all() ->
    [pre_commit_hook_blocks_bad_code_test, ...].
```

**What Happens:**
- For each loaded module, EUnit looks for test/0 or test/1
- For CT modules: NOT FOUND
- EUnit reports: "Module not found"

**Result:** Test execution fails ❌

---

## 3. Why Exclusion Pattern Fails

### The Pattern

```erlang
{exclude, ".*_SUITE$"}
```

### How It's Interpreted

```
Regex Pattern: ".*_SUITE$"
Meaning:
  .* = any characters
  _SUITE = literal string
  $ = end of string

Matches:
  "auto_fix_SUITE" ✅
  "hooks_integration_SUITE" ✅
  "quality_gates_SUITE" ✅
  "tcps_andon_integration_SUITE" ✅
  "tcps_ct_hooks" ❌ (doesn't match)
```

### When Pattern is Applied

```erlang
%% Applied: During source discovery (EARLY)
discover_sources(test_dir, Pattern) ->
    Files = find_erl_files(test_dir),
    filter_by_pattern(Files, Pattern).
    %% Result: Matched files marked "exclude"

%% NOT applied: During compilation (MID)
compile_all(test_dir) ->
    Files = find_erl_files(test_dir),
    compile_each(Files).
    %% Result: ALL files compiled (exclusion forgotten)

%% NOT applied: During beam discovery (MID-LATE)
scan_ebin(ebin_dir) ->
    Files = find_beam_files(ebin_dir),
    Files.
    %% Result: ALL beam files returned (no pattern matching)

%% NOT applied: During test execution (LATE)
run_eunit(Modules) ->
    lists:foreach(fun(M) -> execute_tests(M) end, Modules).
    %% Result: ALL modules processed (no exclusion check)
```

### The Problem Chain

```
Pattern Applied ✅ → Marked "exclude" (mental note)
    ↓
Files Compiled ❌ → Pattern ignored, all files compiled
    ↓
Beam Files Found ❌ → Pattern not re-checked, all beams listed
    ↓
Tests Executed ❌ → Pattern never checked, all modules processed
    ↓
FAILURE ❌ → CT modules processed as EUnit tests
```

---

## 4. File Organization Issues

### Before Fix: Mixed Test Types

```
/Users/sac/erlmcp/test/
├── erlmcp_buffer_pool_tests.erl           (EUnit) ✅
├── erlmcp_capability_cache_tests.erl      (EUnit) ✅
├── erlmcp_codegen_tests.erl               (EUnit) ✅
├── erlmcp_connection_pool_tests.erl       (EUnit) ✅
├── erlmcp_hot_reload_tests.erl            (EUnit) ✅
├── erlmcp_metrics_dashboard_tests.erl     (EUnit) ✅
├── erlmcp_registry_distributed_tests.erl  (EUnit) ✅
├── erlmcp_trace_propagation_tests.erl     (EUnit) ✅
├── auto_fix_SUITE.erl                     (CT) ❌
├── hooks_integration_SUITE.erl            (CT) ❌
└── quality_gates_SUITE.erl                (CT) ❌

TOTAL: 11 files (8 unit + 3 integration)
PROBLEM: Rebar3 treats entire directory as "eunit tests"
```

### After Fix: Separated Test Types

```
/Users/sac/erlmcp/test/
├── unit/
│   ├── erlmcp_buffer_pool_tests.erl       (EUnit) ✅
│   ├── erlmcp_capability_cache_tests.erl  (EUnit) ✅
│   ├── erlmcp_codegen_tests.erl           (EUnit) ✅
│   ├── erlmcp_connection_pool_tests.erl   (EUnit) ✅
│   ├── erlmcp_hot_reload_tests.erl        (EUnit) ✅
│   ├── erlmcp_metrics_dashboard_tests.erl (EUnit) ✅
│   ├── erlmcp_registry_distributed_tests.erl (EUnit) ✅
│   └── erlmcp_trace_propagation_tests.erl (EUnit) ✅
└── integration/
    ├── auto_fix_SUITE.erl                 (CT) ✅
    ├── hooks_integration_SUITE.erl        (CT) ✅
    └── quality_gates_SUITE.erl            (CT) ✅

STRUCTURE RULES:
  - EUnit scans: test/unit/
  - CT scans: test/integration/
  - No pattern matching needed
```

---

## 5. Helper Module Issue

### The Problem File

**Path:** `/Users/sac/erlmcp/apps/tcps_erlmcp/test/integration/tcps_ct_hooks.erl`

**Module:** `tcps_ct_hooks`

**Purpose:** Common Test hook provider (infrastructure)

### Module Structure

```erlang
-module(tcps_ct_hooks).

%% Exported hook callbacks
-export([
    init/2,
    pre_init_per_suite/3,
    post_init_per_suite/4,
    pre_init_per_testcase/4,
    post_init_per_testcase/5,
    pre_end_per_testcase/4,
    post_end_per_testcase/5,
    pre_end_per_suite/3,
    post_end_per_suite/4,
    terminate/1
]).

-record(state, {
    mock_services :: map(),
    suite_start_time :: integer(),
    testcase_start_time :: integer(),
    metrics :: map()
}).

init(_Id, _Opts) ->
    %% Hook initialization
    ok.

pre_init_per_suite(SuiteName, Config, State) ->
    %% Hook before suite
    {Config, State}.

%% ... more hook implementations
```

### Why It Fails

| Aspect | EUnit Expects | tcps_ct_hooks Has |
|--------|---------------|------------------|
| **Export** | `test/0` or `test/1` | `init/2`, `pre_init_per_suite/3`, etc. |
| **Test Functions** | `test_foo/0` or `test_foo/1` | No test functions (only hooks) |
| **Module Type** | Test module | Hook provider (infrastructure) |
| **Result** | Executes tests | EUnit can't find test interface |

### The Error

```
Error: Module `tcps_ct_hooks' not found in project.
```

This happens because:
1. tcps_ct_hooks.beam is discovered by EUnit
2. EUnit loads the module
3. EUnit looks for `test/0`, `test/1`, or functions ending in `_test/0`
4. None found
5. Error: "Module not found" (misleading message)

---

## 6. Per-App Test Directories

### erlmcp_core

**Path:** `/Users/sac/erlmcp/apps/erlmcp_core/test/`

**Contents:**
- 2 CT SUITE modules:
  - `erlmcp_integration_SUITE.erl`
  - `erlmcp_registry_dist_SUITE.erl`

---

### erlmcp_observability

**Path:** `/Users/sac/erlmcp/apps/erlmcp_observability/test/`

**Contents:**
- 1 CT SUITE module:
  - `erlmcp_observability_SUITE.erl`

---

### erlmcp_transports

**Path:** `/Users/sac/erlmcp/apps/erlmcp_transports/test/`

**Contents:**
- 2 CT SUITE modules:
  - `erlmcp_transport_behavior_SUITE.erl`
  - `erlmcp_transport_integration_SUITE.erl`

---

### tcps_erlmcp

**Path:** `/Users/sac/erlmcp/apps/tcps_erlmcp/test/`

**Contents:**
- 3 CT SUITE modules at top level:
  - `erlmcp_pricing_poka_yoke_SUITE.erl`
  - `erlmcp_pricing_receipt_extended_SUITE.erl`
  - `erlmcp_receipt_cli_SUITE.erl`

- 1 helper module:
  - `tcps_ct_hooks.erl`

**Nested in integration/:**
- 9 CT SUITE modules:
  - `tcps_andon_integration_SUITE.erl`
  - `tcps_concurrent_SUITE.erl`
  - `tcps_heijunka_SUITE.erl`
  - `tcps_mcp_diataxis_SUITE.erl`
  - `tcps_performance_SUITE.erl`
  - `tcps_persistence_SUITE.erl`
  - `tcps_pipeline_SUITE.erl`
  - `tcps_quality_gates_SUITE.erl`
  - `tcps_simulator_integration_SUITE.erl`

---

## 7. Rebar3 Configuration Files

### Root Configuration

**File:** `/Users/sac/erlmcp/rebar.config`

**Relevant Lines:**

```erlang
%% Line 34: Test directories
{test_dirs, ["test"]}.

%% Line 38-41: EUnit options
{eunit_opts, [
    {exclude, ".*_SUITE$"},
    verbose
]}.
```

**Analysis:**
- Line 34: Tells Rebar3 to look for tests in `test/`
- Line 39: Tries to exclude files matching `.*_SUITE$`
- **Problem:** Pattern applied only at source discovery, not at runtime

---

### Per-App Configurations

**Files:**
- `/Users/sac/erlmcp/apps/erlmcp_core/rebar.config`
- `/Users/sac/erlmcp/apps/erlmcp_observability/rebar.config`
- `/Users/sac/erlmcp/apps/erlmcp_transports/rebar.config`
- `/Users/sac/erlmcp/apps/tcps_erlmcp/rebar.config`

**Current State:** No eunit_opts specified (inherit from root)

**Should Be Updated With:**
```erlang
{test_dirs, ["test/unit"]}.
{ct_opts, [{ct_dir, ["test/integration"]}]}.
```

---

## 8. The Fix: Configuration Changes

### Change 1: Root rebar.config

**File:** `/Users/sac/erlmcp/rebar.config`

**Before:**
```erlang
{test_dirs, ["test"]}.

{eunit_opts, [
    {exclude, ".*_SUITE$"},
    verbose
]}.
```

**After:**
```erlang
{test_dirs, ["test/unit"]}.

{ct_opts, [
    {ct_dir, ["test/integration"]},
    {auto_compile, true}
]}.

{eunit_opts, [
    verbose
]}.
```

**Rationale:**
- `test_dirs` now points to `test/unit/` only (EUnit scans only unit tests)
- `ct_opts` now points to `test/integration/` (CT scans only integration tests)
- Exclusion pattern removed (no longer needed)

---

### Change 2: Per-App rebar.config (Optional but Recommended)

**Example for erlmcp_core:**

**File:** `/Users/sac/erlmcp/apps/erlmcp_core/rebar.config`

**Add:**
```erlang
{test_dirs, ["test/unit"]}.
{ct_opts, [{ct_dir, ["test/integration"]}]}.
```

---

## 9. Verification Commands

### Test EUnit Only (Unit Tests)

```bash
TERM=dumb rebar3 eunit
```

**Expected Output:**
- Compiles code
- Runs tests from `test/unit/` only
- Shows: `X/X tests passed`
- No CT modules loaded
- No "Module not found" errors

---

### Test Common Test Only (Integration Tests)

```bash
TERM=dumb rebar3 ct
```

**Expected Output:**
- Compiles code
- Runs suites from `test/integration/` only
- Shows: `X passed, 0 failed` from CT
- All CT modules load successfully

---

### Full Quality Check

```bash
TERM=dumb rebar3 check
```

**Expected Output:**
- Compilation: ✅
- Cross-reference (xref): ✅
- Dialyzer: ✅
- EUnit: ✅ (unit tests only)
- Coverage: ✅

---

## 10. Migration Script

### Automated Migration

**Bash Script:** Move files to unit/ and integration/ subdirectories

```bash
#!/bin/bash
set -e

migrate_tests() {
    local test_dir=$1

    # Create subdirectories
    mkdir -p "$test_dir/unit"
    mkdir -p "$test_dir/integration"

    # Move EUnit tests to unit/
    find "$test_dir" -maxdepth 1 -name "*_tests.erl" -exec mv {} "$test_dir/unit/" \;

    # Move CT SUITE to integration/
    find "$test_dir" -maxdepth 1 -name "*_SUITE.erl" -exec mv {} "$test_dir/integration/" \;

    # Move CT hooks to integration/
    find "$test_dir" -maxdepth 1 -name "*_ct_hooks.erl" -exec mv {} "$test_dir/integration/" \;

    echo "Migrated: $test_dir"
}

# Top-level
migrate_tests "/Users/sac/erlmcp/test"

# Per-app
for app_dir in /Users/sac/erlmcp/apps/*/test; do
    [ -d "$app_dir" ] && migrate_tests "$app_dir"
done

echo "Migration complete!"
```

---

## 11. Impact Summary

### Before Fix
- Build Status: ❌ FAILED
- EUnit: ❌ Crashes
- CT: ✅ Works
- Command `rebar3 check`: ❌ Failed

### After Fix
- Build Status: ✅ PASSED
- EUnit: ✅ Works (unit tests only)
- CT: ✅ Works (integration tests only)
- Command `rebar3 check`: ✅ Passed

---

## 12. References

### Rebar3 Documentation
- Testing Guide: https://rebar3.org/docs/testing/
- Configuration: https://rebar3.org/docs/configuration/configuration/

### Erlang/OTP Documentation
- EUnit: https://www.erlang.org/doc/man/eunit.html
- Common Test: https://www.erlang.org/doc/man/ct.html

### Investigation Documents
- Full Analysis: `REBAR3_EUNIT_SUITE_ANALYSIS.md`
- Fix Guide: `REBAR3_EUNIT_FIX_GUIDE.md`
- This Reference: `REBAR3_INVESTIGATION_REFERENCE.md`

---

## 13. Key Takeaways

1. **Exclusion patterns are ineffective at runtime** - they only apply to source discovery
2. **Directory separation is the reliable solution** - use unit/ and integration/ subdirectories
3. **CT modules have different interface than EUnit modules** - EUnit can't execute CT modules
4. **Helper modules need explicit placement** - move them to integration/ subdirectory
5. **Rebar3 processes all discovered test files** - you must prevent discovery via directory structure, not patterns

---

## 14. Timeline

| Phase | Duration | Action |
|-------|----------|--------|
| **Preparation** | 2 min | Create unit/ and integration/ directories |
| **Migration** | 5 min | Move files to appropriate subdirectories |
| **Configuration** | 2 min | Update rebar.config |
| **Verification** | 3 min | Run tests to verify fixes |
| **CI/CD Update** | 3 min | Update pipeline (optional) |
| **Total** | ~15 min | Complete fix |

---

**Investigation Complete**

For implementation, see: `REBAR3_EUNIT_FIX_GUIDE.md`
