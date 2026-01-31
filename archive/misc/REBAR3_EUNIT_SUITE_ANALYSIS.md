# Analysis: rebar3 eunit Incorrectly Loading CT SUITE Modules

## Executive Summary

**Root Cause:** The exclusion pattern in `/Users/sac/erlmcp/rebar.config` line 39 uses an incomplete regex that matches `_SUITE` anywhere in the filename, but the regex is evaluated AFTER EUnit attempts to load modules from the ebin directory. CT SUITE modules are being compiled into beam files, and EUnit is discovering them via direct beam file scanning rather than exclusion rules.

**Impact Level:** CRITICAL
- EUnit fails with: `Module 'tcps_andon_integration_SUITE' not found in project`
- Blocks `rebar3 eunit` and `rebar3 check` commands
- Affects CI/CD pipelines

---

## Problem Analysis

### 1. Why EUnit is Loading *_SUITE Modules

**Root Cause Chain:**

1. **File Organization Problem**
   - CT SUITE modules are stored in `/Users/sac/erlmcp/test/` (top-level)
   - CT SUITE modules are also stored in `/Users/sac/erlmcp/apps/*/test/` (per-app)
   - Rebar.config line 34 specifies: `{test_dirs, ["test"]}`
   - This causes rebar3 to compile ALL `.erl` files in `test/` directories during the test phase

2. **Compilation Process**
   - When `rebar3 eunit` runs:
     - Phase 1: Compile all test code including CT SUITE modules → `.beam` files in `_build/test/lib/*/ebin/`
     - Phase 2: Discover beam files in ebin directories
     - Phase 3: Try to load modules (EUnit uses file discovery, not exclusion rules)
     - Phase 4: EUnit attempts to call test functions on CT modules (which don't exist)

3. **Exclusion Pattern Ineffectiveness**
   - Current pattern in rebar.config (line 39): `{exclude, ".*_SUITE$"}`
   - **Problem:** This regex is meant to exclude from eunit TEST DISCOVERY, not compilation
   - The pattern works at the source discovery level, BUT:
     - Rebar3 still compiles the files (they ARE valid Erlang modules)
     - Compiled beam files end up in ebin/ directories
     - EUnit's module discovery scans ebin/ directly and finds them anyway

4. **Why the Error Message is Misleading**
   ```
   Module `tcps_andon_integration_SUITE' not found in project.
   ```
   - This error occurs when EUnit tries to load a module from a compiled beam file
   - The module IS found (it was compiled), but EUnit expects to find it in the PROJECT's ebin directory
   - The issue is that CT SUITE beam files are in dependency ebin directories (e.g., `_build/test/lib/tcps_erlmcp/ebin/`)
   - EUnit has no way to invoke test functions on CT modules (CT modules don't export test/0, test/1, etc.)

5. **Helper Module Being Treated as Test**
   - File: `/Users/sac/erlmcp/apps/tcps_erlmcp/test/integration/tcps_ct_hooks.erl`
   - Module: `tcps_ct_hooks`
   - This is a **Common Test hook module** (not a test module, not exportable as test)
   - It has NO test functions (no `test/0`, `test/1`, etc.)
   - It has NO `all/0` callback (which is different from CT suites that DO have `all/0`)
   - EUnit tries to load it and fails because:
     - It's not in the project's direct ebin/ (only in app ebin/)
     - It has no test interface

---

## Rebar.config Exclusion Analysis

### Current Configuration (Line 38-41)

```erlang
%% Exclude integration tests from eunit (use CT for integration)
{eunit_opts, [
    {exclude, ".*_SUITE$"},
    verbose
]}.
```

### Why This Doesn't Work

The `{exclude, ...}` pattern is applied **at source discovery time**, NOT at beam loading time:

1. **Source Discovery Phase** (where exclusion applies):
   - Pattern: `.*_SUITE$`
   - Scans: `src/` directories
   - Result: Source files matching `*_SUITE.erl` are excluded from eunit consideration
   - ✅ Works correctly for source files

2. **Compilation Phase** (exclusion not applied):
   - ALL test files are compiled, including `*_SUITE.erl`
   - Result: `*.beam` files for CT modules appear in ebin/
   - ❌ Exclusion doesn't prevent compilation

3. **Test Discovery Phase** (exclusion not enforced):
   - EUnit scans compiled `.beam` files in ebin/ directories
   - Pattern matching is NOT re-applied
   - Result: All `.beam` files (including CT SUITE modules) are considered
   - ❌ Exclusion is ineffective

4. **Test Execution Phase** (failure):
   - EUnit tries to execute test functions on CT modules
   - CT modules don't export test interface
   - Result: "Module not found" error
   - ❌ Failure occurs

---

## File Organization Issues

### Problem 1: Top-Level Test Directory

**Path:** `/Users/sac/erlmcp/test/` contains:
- ✅ EUnit test modules: `erlmcp_*.erl` (ending in `_tests.erl`)
- ❌ CT SUITE modules: `*_SUITE.erl` (3 files)

```
test/
├── erlmcp_buffer_pool_tests.erl           ✅ EUnit
├── erlmcp_capability_cache_tests.erl      ✅ EUnit
├── erlmcp_connection_pool_tests.erl       ✅ EUnit
├── erlmcp_trace_propagation_tests.erl     ✅ EUnit
├── erlmcp_codegen_tests.erl               ✅ EUnit
├── erlmcp_hot_reload_tests.erl            ✅ EUnit
├── erlmcp_metrics_dashboard_tests.erl     ✅ EUnit
├── erlmcp_registry_distributed_tests.erl  ✅ EUnit
├── hooks_integration_SUITE.erl            ❌ CT SUITE
├── quality_gates_SUITE.erl                ❌ CT SUITE
└── auto_fix_SUITE.erl                     ❌ CT SUITE
```

**Issue:** Top-level `test/` directory mixes EUnit and CT modules. Rebar3 doesn't know to separate them.

### Problem 2: Per-App Test Directories

**Paths:** `/Users/sac/erlmcp/apps/*/test/` contains:
- ✅ EUnit test modules
- ❌ CT SUITE modules
- ❌ Helper modules like `tcps_ct_hooks.erl`

Example from tcps_erlmcp:
```
apps/tcps_erlmcp/test/
├── erlmcp_*.erl                           ✅ EUnit modules
├── tcps_*.erl                             ✅ EUnit modules
├── erlmcp_receipt_cli_SUITE.erl           ❌ CT SUITE
├── erlmcp_pricing_receipt_extended_SUITE.erl  ❌ CT SUITE
├── erlmcp_pricing_poka_yoke_SUITE.erl    ❌ CT SUITE
└── integration/
    ├── tcps_ct_hooks.erl                  ❌ CT Helper (NOT a test)
    ├── tcps_andon_integration_SUITE.erl   ❌ CT SUITE
    ├── tcps_concurrent_SUITE.erl          ❌ CT SUITE
    ├── tcps_heijunka_SUITE.erl            ❌ CT SUITE
    └── ... (9 more CT SUITE files)
```

**Issues:**
1. Helper modules (like `tcps_ct_hooks.erl`) are treated as tests
2. CT SUITE modules are in the same directory as EUnit modules
3. Per-app test dirs follow umbrella project structure but get compiled together

---

## The regex Exclusion Pattern Problem

### Current Regex: `".*_SUITE$"`

**Analysis:**

```
Pattern: ".*_SUITE$"
Meaning:
  .* = match any characters
  _SUITE = literal string
  $ = end of string

Expected to match:
  - auto_fix_SUITE ✅
  - hooks_integration_SUITE ✅
  - quality_gates_SUITE ✅
  - tcps_andon_integration_SUITE ✅
```

**BUT:** This pattern is applied ONLY to source files during discovery, not during:
- Compilation (files are compiled anyway)
- Beam file scanning (compiled modules are discovered)
- Test execution (EUnit tries to run them)

### Why Rebar3's Exclusion Fails

From rebar3 source code logic:

```erlang
%% Phase 1: Discover source files
SourceFiles = discover_sources(TestDir, ExcludePattern),  % Regex applied HERE

%% Phase 2: Compile ALL files (including those that matched exclusion)
{ok, CompiledModules} = compile_all(TestDir),  % Exclusion NOT applied

%% Phase 3: Scan beam files
BeamFiles = scan_ebin_dir(EbinDir),  % Direct file listing, no exclusion

%% Phase 4: Try to run tests
run_eunit_tests(BeamFiles),  % Tries to execute on CT modules -> FAILS
```

---

## Recommended Fixes (In Order of Preference)

### FIX 1: Separate CT Tests into Distinct Subdirectory (BEST)

**Action:** Move all CT SUITE modules into a separate directory that Rebar3 won't scan for eunit.

```
test/
├── unit/                                   ← EUnit modules go here
│   ├── erlmcp_buffer_pool_tests.erl
│   ├── erlmcp_capability_cache_tests.erl
│   └── ...
└── integration/                            ← CT modules go here
    ├── hooks_integration_SUITE.erl
    ├── quality_gates_SUITE.erl
    └── auto_fix_SUITE.erl

apps/tcps_erlmcp/test/
├── unit/                                   ← EUnit modules
│   ├── erlmcp_pricing_poka_yoke_tests.erl
│   └── ...
└── integration/                            ← CT modules
    ├── erlmcp_pricing_poka_yoke_SUITE.erl
    ├── tcps_ct_hooks.erl
    └── tcps_andon_integration_SUITE.erl
```

**Rebar.config Changes:**

```erlang
{test_dirs, ["test/unit"]}.

{ct_opts, [
    {ct_dir, ["test/integration", "apps/*/test/integration"]}
]}.

%% EUnit only for unit tests
{eunit_opts, [
    verbose
    %% Remove exclude pattern - no longer needed
]}.
```

**Benefits:**
- Clear separation of concerns
- EUnit runs only unit tests
- CT runs only integration tests
- No regex workarounds needed
- Explicit directory configuration

---

### FIX 2: Update Rebar.config Exclusion Pattern (FALLBACK)

If directory reorganization isn't immediately feasible, update the pattern to be more aggressive:

**Current:**
```erlang
{eunit_opts, [
    {exclude, ".*_SUITE$"},  ← Incomplete pattern
    verbose
]}.
```

**Better (but not a complete fix):**
```erlang
{eunit_opts, [
    {exclude, ".*_SUITE\.erl$"},              ← Match .erl extension
    {exclude, ".*_ct_hooks\.erl$"},           ← Match helper modules
    {exclude, ".*/integration/.*\.erl$"},     ← Match integration dir
    {exclude, ".*/test/integration/.*\.erl$"}, ← Match per-app integration
    verbose
]}.
```

**Limitations:**
- This still doesn't prevent compilation
- Pattern matching happens at discovery time, not beam loading time
- Helper modules might still leak through
- **NOT A COMPLETE FIX** - beam files will still be compiled and discovered

---

### FIX 3: Use Per-App Rebar Configs (HYBRID)

Add `{eunit_opts, [...]}` to each app's `rebar.config` to exclude its own CT modules:

**File:** `/Users/sac/erlmcp/apps/tcps_erlmcp/rebar.config`

```erlang
{eunit_opts, [
    {exclude, ".*_SUITE$"},
    {exclude, ".*_ct_hooks$"},
    verbose
]}.
```

**Benefits:**
- Per-app control
- Explicit per-app exclusions
- Cleaner top-level rebar.config

**Limitations:**
- Requires updating 4 app-level configs
- Still doesn't completely prevent beam file compilation
- Similar to Fix 2 - incomplete solution

---

## Recommended Implementation Path

### Phase 1: Immediate (Today)
Implement **FIX 1** by reorganizing directories:

1. Create `/Users/sac/erlmcp/test/unit/` directory
2. Create `/Users/sac/erlmcp/test/integration/` directory
3. Move `*_tests.erl` files to `test/unit/`
4. Move `*_SUITE.erl` files to `test/integration/`
5. Repeat for all apps in `/Users/sac/erlmcp/apps/*/test/`
6. Update `/Users/sac/erlmcp/rebar.config` with new paths

### Phase 2: Testing
```bash
# Verify EUnit only runs unit tests
rebar3 eunit

# Verify CT only runs integration tests
rebar3 ct

# Verify full suite passes
rebar3 check
```

### Phase 3: CI/CD
Update CI pipeline to run:
- `rebar3 eunit` (unit tests only)
- `rebar3 ct` (integration tests only)
- Separately, not mixed

---

## Technical Details: How Rebar3 Discovers Tests

### EUnit Discovery Algorithm

```
1. Input: {test_dirs, ["test"]}
2. Scan test/ directory for .erl files
3. Apply exclusion patterns to source files
4. Compile .erl → .beam files into _build/test/lib/*/ebin/
5. Scan _build/test/lib/*/ebin/ for .beam files
6. Load each .beam module
7. For each module, check if it has test functions:
   - test/0
   - test/1 (with module name)
   - Function name ending in _test/0
8. Run discovered tests
```

### Why CT Modules Fail

CT modules have:
- `all/0` - Returns list of test case functions
- `init_per_suite/1` - Setup function
- `end_per_suite/1` - Teardown function
- Test functions: `foo_test/1`

They do NOT have:
- `test/0` - EUnit's expected interface
- `test/1` - EUnit's expected interface

When EUnit tries to load a CT module:
```
1. Discovers tcps_andon_integration_SUITE.beam in ebin/
2. Loads module
3. Looks for test/0 or test/1 → NOT FOUND
4. Tries to call test function → FAILS
5. Error: "Module 'tcps_andon_integration_SUITE' not found in project"
```

---

## File Reference Map

### Top-Level Test Directory
- **Location:** `/Users/sac/erlmcp/test/`
- **Files:** 11 total
- **EUnit modules (8):**
  - erlmcp_buffer_pool_tests.erl
  - erlmcp_capability_cache_tests.erl
  - erlmcp_codegen_tests.erl
  - erlmcp_connection_pool_tests.erl
  - erlmcp_hot_reload_tests.erl
  - erlmcp_metrics_dashboard_tests.erl
  - erlmcp_registry_distributed_tests.erl
  - erlmcp_trace_propagation_tests.erl
- **CT SUITE modules (3):**
  - hooks_integration_SUITE.erl
  - quality_gates_SUITE.erl
  - auto_fix_SUITE.erl

### Per-App Test Directories

**erlmcp_core:** `/Users/sac/erlmcp/apps/erlmcp_core/test/`
- CT SUITE modules: 2
  - erlmcp_integration_SUITE.erl
  - erlmcp_registry_dist_SUITE.erl

**erlmcp_observability:** `/Users/sac/erlmcp/apps/erlmcp_observability/test/`
- CT SUITE modules: 1
  - erlmcp_observability_SUITE.erl

**erlmcp_transports:** `/Users/sac/erlmcp/apps/erlmcp_transports/test/`
- CT SUITE modules: 2
  - erlmcp_transport_behavior_SUITE.erl
  - erlmcp_transport_integration_SUITE.erl

**tcps_erlmcp:** `/Users/sac/erlmcp/apps/tcps_erlmcp/test/`
- CT SUITE modules: 3
  - erlmcp_pricing_poka_yoke_SUITE.erl
  - erlmcp_pricing_receipt_extended_SUITE.erl
  - erlmcp_receipt_cli_SUITE.erl
- Helper module:
  - tcps_ct_hooks.erl (Common Test hook, NOT a test module)
- **integration/ subdirectory:** 9 CT SUITE modules + 1 helper

### Helper Modules (Incorrectly Treated as Tests)
- `/Users/sac/erlmcp/apps/tcps_erlmcp/test/integration/tcps_ct_hooks.erl` (CT Hook)

---

## Summary Table

| Issue | Root Cause | Impact | Fix |
|-------|-----------|--------|-----|
| EUnit loading CT SUITE modules | Exclusion pattern applied only at source discovery, not beam loading | EUnit fails with "Module not found" | Separate test/unit/ from test/integration/ |
| Helper module tcps_ct_hooks treated as test | Not in exclude pattern + compiled to beam file | Module loading fails (no test interface) | Move to test/integration/ subdirectory |
| Incomplete regex pattern | `.*_SUITE$` doesn't match all cases or file extensions | Pattern ineffective at runtime | Use directory separation instead |
| Umbrella project complexity | Multiple test/ directories in apps/ | Rebar3 processes all files together | Add per-app ct_opts configuration |

---

## Verification Commands

After implementing fixes:

```bash
# Should only run EUnit tests (no CT modules)
TERM=dumb rebar3 eunit 2>&1 | grep -c "passed"

# Should only run CT tests
TERM=dumb rebar3 ct 2>&1 | grep -c "passed"

# Full check should pass
TERM=dumb rebar3 check
```

---

## References

- **EUnit Documentation:** https://www.erlang.org/doc/man/eunit.html
- **Common Test Documentation:** https://www.erlang.org/doc/man/ct.html
- **Rebar3 Testing Guide:** https://rebar3.org/docs/testing/
- **Rebar.config Options:** https://rebar3.org/docs/configuration/configuration/
