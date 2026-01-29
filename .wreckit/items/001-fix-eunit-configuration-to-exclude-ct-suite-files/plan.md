# Fix EUnit configuration to exclude CT SUITE files Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Reorganize test directory structure to physically separate EUnit unit tests from Common Test integration tests, enabling proper test runner isolation and eliminating module interface conflicts.

**Root Cause:** Rebar3's `{exclude, ".*_SUITE$"}` pattern is applied only during source file discovery, not during beam file loading. CT SUITE modules are compiled into `.beam` files and subsequently discovered by EUnit's beam file scanner, but they lack EUnit's expected `test/0` or `test/1` interface, causing execution failures.

**Manufacturing Solution:** Separate EUnit and CT tests into distinct subdirectories (`test/unit/` and `test/integration/`), then configure rebar3 to use these directories explicitly. This provides physical separation that cannot be bypassed by beam file discovery.

### Quality Gate Requirements

- **Compilation**: 0 errors (MANDATORY)
- **EUnit**: 100% pass rate (MANDATORY)
- **Common Test**: 100% pass rate (if applicable)
- **Coverage**: ≥80% (MANDATORY)
- **Dialyzer**: 0 warnings (MANDATORY)
- **Xref**: 0 undefined function calls (MANDATORY)
- **Performance**: <10% regression from baseline (if applicable)

## Current State

### What Exists Now

**Modules:**
- 84 EUnit test files (`*_tests.erl`) - Total: 26,568 lines of test code
  - Located in: `/Users/sac/erlmcp/test/`, `/Users/sac/erlmcp/apps/*/test/`
  - All use `-include_lib("eunit/include/eunit.hrl")`
  - Export test functions with `_test_()` pattern
  - Implement `setup()` and `cleanup()` fixtures

- 27 CT SUITE files (`*_SUITE.erl`) - Total: 13,559 lines of integration test code
  - Located in: SAME directories as EUnit tests (mixed together)
  - All use `-include_lib("common_test/include/ct.hrl")`
  - Export `all/0`, `groups/0`, `init_per_suite/1`, `end_per_suite/1` callbacks
  - Test functions have `/1` arity (take Config argument)

- 1 CT helper module: `tcps_ct_hooks.erl`
  - Located in: `/Users/sac/erlmcp/apps/tcps_erlmcp/test/integration/`
  - Not a test module (no test functions)
  - Provides CT callback hooks

**Tests:**
- EUnit: 0% pass rate (complete failure - cannot execute due to CT module conflicts)
- Common Test: 100% pass rate (CT runs independently without issues)
- Coverage: BLOCKED (cannot measure without EUnit working)

**Quality:**
- Current gate status: **FAIL** - EUnit quality gate completely broken
- Error: `Module 'tcps_andon_integration_SUITE' not found in project`
- Impact: Blocks 40-80 hours of EUnit test development work

### What's Missing

**Gap:** Physical separation of EUnit and CT test modules
- Current: Both test types mixed in same `test/` directories
- Required: EUnit in `test/unit/`, CT in `test/integration/`
- Gap: 100% of test directories need reorganization

**Root Cause:** Rebar3's EUnit exclusion pattern `{exclude, ".*_SUITE$"}` is ineffective because:
1. Pattern applied at source discovery (Phase 1)
2. All files compiled anyway (Phase 2)
3. Beam files scanned without exclusion (Phase 3)
4. EUnit tries to execute CT modules (Phase 4) → FAIL

**Impact:**
- Blocks ALL EUnit test development (40-80 hours of dependent work)
- Prevents CI/CD quality gate validation
- Cannot measure test coverage
- False "module not found" errors mislead developers

### Key Discoveries from Research

**Discovery 1:** `{exclude, ".*_SUITE$"}` pattern is fundamentally broken
- File: `/Users/sac/erlmcp/rebar.config:39`
- Issue: Exclusion only affects source discovery, not beam loading
- Impact: CT modules compiled and discovered anyway

**Discovery 2:** Test file organization causes conflicts
- Files: 84 `*_tests.erl` and 27 `*_SUITE.erl` mixed in same directories
- Issue: Rebar3 compiles all `.erl` files in `test/` directories
- Impact: No way to exclude CT modules at compilation time

**Discovery 3:** Rebar3 requires physical directory separation
- Documentation: `/Users/sac/erlmcp/REBAR3_EUNIT_SUITE_ANALYSIS.md:205-244`
- Solution: Use `{test_dirs, ["test/unit"]}` for EUnit and `{ct_dir, ["test/integration"]}` for CT
- Benefit: Physical separation cannot be bypassed

**Discovery 4:** OTP patterns to follow
- EUnit test pattern: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_rate_limiting_tests.erl:46-58`
  - Use `setup()` and `cleanup()` fixtures for resource management
  - Group related tests with `_test_()` generator functions
  - Use `?_test()` macro for lazy evaluation
  - Use `?assertEqual()`, `?assertMatch()`, `?assert()` macros

**Discovery 5:** CRITICAL - Research summary proposed solution is INCORRECT
- Research summary suggested: Add `{src_dirs, ["src"]}` to eunit_opts
- **This is WRONG**: All EUnit test files are in `test/` directories, NOT `src/`
- **Impact**: Using `{src_dirs, ["src"]}` would cause EUnit to find ZERO tests
- **Correct solution**: Directory reorganization (as documented in REBAR3_EUNIT_SUITE_ANALYSIS.md)

## Desired End State

### Specification

**Directory Structure:**

```
/Users/sac/erlmcp/
├── test/
│   ├── unit/                    ← EUnit tests only
│   │   ├── erlmcp_buffer_pool_tests.erl
│   │   ├── erlmcp_capability_cache_tests.erl
│   │   ├── erlmcp_connection_pool_tests.erl
│   │   └── ... (all *_tests.erl files)
│   └── integration/             ← CT SUITE tests only
│       ├── auto_fix_SUITE.erl
│       ├── hooks_integration_SUITE.erl
│       └── quality_gates_SUITE.erl

└── apps/
    ├── erlmcp_core/
    │   └── test/
    │       ├── unit/            ← EUnit tests
    │       │   ├── erlmcp_auth_tests.erl
    │       │   ├── erlmcp_batch_tests.erl
    │       │   └── ...
    │       └── integration/     ← CT SUITE tests
    │           ├── erlmcp_integration_SUITE.erl
    │           └── erlmcp_registry_dist_SUITE.erl

    ├── erlmcp_transports/
    │   └── test/
    │       ├── unit/            ← EUnit tests
    │       │   ├── erlmcp_transport_discovery_tests.erl
    │       │   └── ...
    │       └── integration/     ← CT SUITE tests
    │           ├── erlmcp_transport_behavior_SUITE.erl
    │           └── erlmcp_transport_integration_SUITE.erl

    ├── erlmcp_observability/
    │   └── test/
    │       ├── unit/            ← EUnit tests
    │       │   └── ...
    │       └── integration/     ← CT SUITE tests
    │           └── erlmcp_observability_SUITE.erl

    └── tcps_erlmcp/
        └── test/
            ├── unit/            ← EUnit tests
            │   ├── tcps_andon_tests.erl
            │   └── ...
            └── integration/     ← CT SUITE tests + helpers
                ├── tcps_ct_hooks.erl
                ├── tcps_andon_integration_SUITE.erl
                └── ...
```

**Configuration Changes:**

**File:** `/Users/sac/erlmcp/rebar.config`

**Lines 33-41 (BEFORE):**
```erlang
{src_dirs, ["src"]}.
{test_dirs, ["test"]}.
{include_dirs, ["include"]}.

%% Exclude integration tests from eunit (use CT for integration)
{eunit_opts, [
    {exclude, ".*_SUITE$"},
    verbose
]}.
```

**Lines 33-44 (AFTER):**
```erlang
{src_dirs, ["src"]}.
{test_dirs, ["test/unit", "test/integration", "apps/*/test/unit", "apps/*/test/integration"]}.
{include_dirs, ["include"]}.

%% EUnit configuration (no exclude needed - physical separation)
{eunit_opts, [
    {test_dirs, ["test/unit", "apps/*/test/unit", "bench/unit"]},
    verbose
]}.

%% Common Test configuration
{ct_opts, [
    {dir, ["test/integration", "apps/*/test/integration", "tests/integration"]},
    {logdir, "_build/test/logs"},
    {keep_logs, 5}
]}.
```

### Verification

**Automated Verification:**
1. Run `rebar3 compile` → 0 errors
2. Run `rebar3 eunit` → 100% pass rate, all 84 EUnit tests execute
3. Run `rebar3 ct` → 100% pass rate, all 27 CT SUITEs execute
4. Run `rebar3 cover` → Coverage report generated (≥80%)
5. Run `rebar3 dialyzer` → 0 warnings
6. Run `rebar3 xref` → 0 undefined function calls
7. Run `rebar3 check` → All quality gates pass

**Manual Verification:**
1. Verify `test/unit/` directories contain only `*_tests.erl` files
2. Verify `test/integration/` directories contain only `*_SUITE.erl` files
3. Verify EUnit output shows no "module not found" errors
4. Verify EUnit output shows no SUITE module names
5. Verify CT logs show all integration tests executing
6. Measure execution time with `time rebar3 eunit` (should be <30s)

**Metrics to Measure:**
1. **EUnit Test Count:** After fix (target: 84 test modules)
2. **EUnit Execution Time:** After fix (target: <30s)
3. **CT Test Count:** Before vs After (should remain 27 SUITEs)
4. **CT Execution Time:** Before vs After (should remain constant)
5. **Coverage Percentage:** After fix (target ≥80%)
6. **Quality Gate Pass Rate:** After fix (target 100%)

### Manufacturing Output

**Files Modified:**
- `/Users/sac/erlmcp/rebar.config` - Update test_dirs, eunit_opts, and add ct_opts

**Files Moved:**
- 84 `*_tests.erl` files → `test/unit/` subdirectories
- 27 `*_SUITE.erl` files → `test/integration/` subdirectories
- 1 `*_ct_hooks.erl` file → `test/integration/` subdirectory

**Tests:**
- 84 existing EUnit test files (moved, not modified)
- 27 existing CT SUITE files (moved, not modified)
- No new test files created

**Documentation:**
- Manufacturing plan (this file)
- Research summary (already exists)

**Receipts:**
- EUnit execution log showing 100% pass rate
- CT execution log showing 100% pass rate
- Coverage report showing ≥80% coverage
- Quality gate validation report

## What We're NOT Doing

**OUT OF SCOPE ITEMS:**

1. **Modifying test code** - Tests are moved, not modified
   - Reason: Test logic is correct; only file location changes
   - Impact: Zero risk of breaking test behavior

2. **Changing test interfaces** - No API changes to test modules
   - Reason: EUnit and CT interfaces remain unchanged
   - Impact: Zero risk of test framework incompatibility

3. **Creating new tests** - No new test files created
   - Reason: This is a configuration fix, not a testing enhancement
   - Impact: Reduces risk and implementation time

4. **Modifying production code** - No changes to `src/` directories
   - Reason: This is a test infrastructure issue only
   - Impact: Zero risk to production code

5. **Refactoring rebar3 provider plugins** - No custom provider development
   - Reason: Standard rebar3 configuration is sufficient
   - Impact: Uses well-documented, stable rebar3 features

6. **Updating CI/CD pipelines** - Pipeline changes out of scope
   - Reason: Infrastructure change (pipeline will adapt to rebar3 commands)
   - Impact: Pipelines use `rebar3 eunit` and `rebar3 ct` which will work correctly

7. **Documentation updates** - No README or API doc changes needed
   - Reason: Test execution commands remain the same
   - Impact: Zero documentation burden

8. **Performance optimization** - Not optimizing test execution time
   - Reason: Current goal is correctness, not performance
   - Impact: Focus on single objective (fixing EUnit)

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** - Requirements with acceptance criteria
   - Reorganize test directories to separate EUnit and CT
   - Update rebar.config with explicit directory paths
   - Verify 0 compilation errors
   - Verify 100% EUnit pass rate
   - Verify 100% CT pass rate
   - Verify ≥80% coverage

2. **Pseudocode** - Algorithm design BEFORE coding
   ```
   For each directory in ["/Users/sac/erlmcp/test", "/Users/sac/erlmcp/apps/*/test"]:
       Create subdirectory: "unit/"
       Create subdirectory: "integration/"

       For each file in directory:
           If filename matches "*_tests.erl":
               Move file to "unit/" subdirectory
           Else if filename matches "*_SUITE.erl" OR filename contains "_ct_hooks":
               Move file to "integration/" subdirectory

   Update /Users/sac/erlmcp/rebar.config:
       Change {test_dirs, ["test"]} to include both unit and integration subdirectories
       Update eunit_opts to explicitly point to unit/ directories only
       Add ct_opts to explicitly point to integration/ directories only
       Remove {exclude, ".*_SUITE$"} from eunit_opts (no longer needed)

   Run quality gates:
       rebar3 compile (verify 0 errors)
       rebar3 eunit (verify 100% pass)
       rebar3 ct (verify 100% pass)
       rebar3 cover (verify ≥80%)
       rebar3 dialyzer (verify 0 warnings)
       rebar3 xref (verify 0 undefined calls)
   ```

3. **Architecture** - Integration points and dependencies
   - **Integration Point 1:** Rebar3 reads `{test_dirs, [...]}` from rebar.config
   - **Integration Point 2:** EUnit reads `{eunit_opts, [{test_dirs, [...]}]}` to locate test files
   - **Integration Point 3:** CT reads `{ct_opts, [{dir, [...]}]}` to locate SUITE files
   - **Integration Point 4:** Test discovery happens at separate directories (no overlap)
   - **No Changes Required:** Per-app rebar.config files (inherit umbrella config)
   - **No Changes Required:** Test module code (file moves only, no logic changes)

4. **Refinement** - Chicago School TDD (tests FIRST)
   - **Test 1:** Verify all test files are correctly categorized
     ```bash
     # Verify no *_tests.erl in integration/
     find test/integration -name "*_tests.erl" | wc -l
     Expected: 0

     # Verify no *_SUITE.erl in unit/
     find test/unit -name "*_SUITE.erl" | wc -l
     Expected: 0
     ```
   - **Test 2:** Verify compilation succeeds
     ```bash
     rebar3 compile
     Expected: exit code 0, 0 errors
     ```
   - **Test 3:** Verify EUnit runs without errors
     ```bash
     rebar3 eunit 2>&1 | grep -i "not found"
     Expected: 0 matches (empty grep output)
     ```
   - **Test 4:** Verify EUnit executes correct number of tests
     ```bash
     rebar3 eunit --verbose 2>&1 | grep -E "Test passed|Module"
     Expected: All 84 test modules executed
     ```
   - **Test 5:** Verify CT executes correctly
     ```bash
     rebar3 ct 2>&1 | grep -E "passed|failed"
     Expected: All 27 CT SUITE modules executed
     ```

5. **Completion** - All quality gates passing
   - Compilation: 0 errors ✓
   - EUnit: 100% pass rate ✓
   - Common Test: 100% pass rate ✓
   - Coverage: ≥80% ✓
   - Dialyzer: 0 warnings ✓
   - Xref: 0 undefined function calls ✓
   - Performance: <10% regression from baseline ✓

### Implementation Strategy

**High-Level Approach:**

This fix uses **physical directory separation** to prevent EUnit from discovering CT SUITE modules. This is superior to regex-based exclusion because:

1. **Poka-yoke (Mistake-proofing):** Directories are separate, so no configuration error can mix them
2. **Jidoka (Built-in Quality):** Rebar3's directory discovery cannot be bypassed
3. **Andon (Visible Signaling):** Directory structure makes test type immediately obvious
4. **Heijunka (Production Leveling):** Changes are simple file moves, no code changes

**Why This Strategy:**

1. **Regex exclusion is fundamentally broken** - `{exclude, ".*_SUITE$"}` doesn't work because:
   - Applied at source discovery, not beam loading
   - CT modules compiled anyway
   - Beam files scanned without exclusion

2. **Configuration workarounds are incomplete** - Using `{src_dirs, ["src"]}` fails because:
   - EUnit tests are in `test/`, not `src/`
   - Would cause 0 tests to be discovered
   - Not how rebar3 EUnit is designed

3. **Directory separation is the correct solution** because:
   - Physical separation cannot be bypassed
   - Explicit configuration is clear and maintainable
   - Follows rebar3 best practices
   - Zero test code changes required

### Quality Integration

**Pre-commit Hooks:**
- `.claude/hooks/pre-task-validate.sh` - Validates all quality gates before commit

**CI Gates:**
- Compilation gate: `rebar3 compile` must pass
- EUnit gate: `rebar3 eunit` must pass (100% pass rate)
- CT gate: `rebar3 ct` must pass (100% pass rate)
- Coverage gate: `rebar3 cover` must pass (≥80%)
- Dialyzer gate: `rebar3 dialyzer` must pass (0 warnings)
- Xref gate: `rebar3 xref` must pass (0 undefined calls)

**Receipt Generation:**
- EUnit execution log saved to `_build/test/logs/eunit.log`
- CT execution log saved to `_build/test/logs/ct.log`
- Coverage report saved to `_build/test/cover/`
- Quality gate summary saved to `.claude/receipts/`

**Andon Signaling:**
- EUnit failures: Visible in console output immediately
- CT failures: Visible in console output immediately
- Coverage gaps: Visible in cover report
- Dialyzer warnings: Visible in build output
- Xref errors: Visible in build output
- Progress tracking: Each phase has clear success/failure indication

---

## Phases

### Phase 1: Directory Reorganization (2 hours)

#### Overview
Create `unit/` and `integration/` subdirectories in all test directories, then move test files into the appropriate subdirectory based on file type (EUnit vs CT).

#### Specification
Create new directory structure and move files:
- Create `test/unit/` and `test/integration/` in root and all apps
- Move 84 `*_tests.erl` files to `unit/` subdirectories
- Move 27 `*_SUITE.erl` files to `integration/` subdirectories
- Move 1 `*_ct_hooks.erl` file to `integration/` subdirectory
- Zero file content modifications (only moves)

#### Pseudocode
```
DIRECTORIES = [
    "/Users/sac/erlmcp/test",
    "/Users/sac/erlmcp/apps/erlmcp_core/test",
    "/Users/sac/erlmcp/apps/erlmcp_transports/test",
    "/Users/sac/erlmcp/apps/erlmcp_observability/test",
    "/Users/sac/erlmcp/apps/tcps_erlmcp/test"
]

FOR EACH dir IN DIRECTORIES:
    CREATE dir/unit/
    CREATE dir/integration/

    FOR EACH file IN dir:
        IF file.name MATCHES "*_tests.erl":
            MOVE file TO dir/unit/
        ELSE IF file.name MATCHES "*_SUITE.erl":
            MOVE file TO dir/integration/
        ELSE IF file.name CONTAINS "_ct_hooks":
            MOVE file TO dir/integration/
        ELSE:
            ERROR "Unknown file type: {file.name}"
```

#### Architecture
**No Integration Points:** This phase only moves files, no code changes.

**Dependencies:**
- Phase 2 depends on Phase 1 (config changes assume new directory structure)
- No other dependencies

**Supervision Tree:** Not applicable (file system changes only)

#### Changes Required:

##### 1. Directory Creation and File Movement
**Action:** Execute file moves for all test directories

**Root Test Directory:**
```
/Users/sac/erlmcp/test/
├── unit/                              ← NEW
│   ├── erlmcp_buffer_pool_tests.erl
│   ├── erlmcp_capability_cache_tests.erl
│   ├── erlmcp_connection_pool_tests.erl
│   ├── erlmcp_enhanced_api_tests.erl
│   ├── erlmcp_hot_reload_tests.erl
│   ├── erlmcp_metrics_dashboard_tests.erl
│   └── erlmcp_registry_distributed_tests.erl
└── integration/                       ← NEW
    ├── auto_fix_SUITE.erl
    ├── hooks_integration_SUITE.erl
    └── quality_gates_SUITE.erl
```

**App Test Directories:**
```
/Users/sac/erlmcp/apps/erlmcp_core/test/
├── unit/
│   ├── erlmcp_auth_tests.erl
│   ├── erlmcp_batch_tests.erl
│   ├── erlmcp_cache_tests.erl
│   ├── erlmcp_circuit_breaker_tests.erl
│   ├── erlmcp_client_tests.erl
│   ├── erlmcp_code_reload_tests.erl
│   ├── erlmcp_json_rpc_tests.erl
│   ├── erlmcp_message_parser_tests.erl
│   ├── erlmcp_rate_limiting_tests.erl
│   ├── erlmcp_rate_limit_middleware_tests.erl
│   ├── erlmcp_registry_tests.erl
│   ├── erlmcp_resource_tests.erl
│   ├── erlmcp_schema_registry_tests.erl
│   ├── erlmcp_server_tests.erl
│   ├── erlmcp_session_manager_tests.erl
│   ├── erlmcp_session_tests.erl
│   └── erlmcp_tool_tests.erl
└── integration/
    ├── erlmcp_integration_SUITE.erl
    └── erlmcp_registry_dist_SUITE.erl

/Users/sac/erlmcp/apps/erlmcp_transports/test/
├── unit/
│   ├── erlmcp_pool_manager_tests.erl
│   ├── erlmcp_transport_discovery_tests.erl
│   ├── erlmcp_transport_http_tests.erl
│   ├── erlmcp_transport_sse_tests.erl
│   ├── erlmcp_transport_stdio_tests.erl
│   ├── erlmcp_transport_sup_tests.erl
│   ├── erlmcp_transport_tcp_tests.erl
│   └── erlmcp_transport_ws_tests.erl
└── integration/
    ├── erlmcp_transport_behavior_SUITE.erl
    └── erlmcp_transport_integration_SUITE.erl

/Users/sac/erlmcp/apps/erlmcp_observability/test/
├── unit/
│   ├── erlmcp_audit_log_tests.erl
│   ├── erlmcp_chaos_tests.erl
│   ├── erlmcp_debugger_tests.erl
│   ├── erlmcp_dashboard_tests.erl
│   ├── erlmcp_health_monitor_tests.erl
│   ├── erlmcp_memory_analyzer_tests.erl
│   ├── erlmcp_metrics_tests.erl
│   ├── erlmcp_otel_enhanced_tests.erl
│   ├── erlmcp_otel_tests.erl
│   ├── erlmcp_profiler_tests.erl
│   └── erlmcp_recovery_manager_tests.erl
└── integration/
    └── erlmcp_observability_SUITE.erl

/Users/sac/erlmcp/apps/tcps_erlmcp/test/
├── unit/
│   ├── tcps_andon_tests.erl
│   ├── tcps_cli_tests.erl
│   ├── tcps_dashboard_tests.erl
│   ├── tcps_diataxis_explain_tests.erl
│   ├── tcps_diataxis_howto_tests.erl
│   ├── tcps_diataxis_reference_tests.erl
│   ├── tcps_deterministic_tests.erl
│   ├── tcps_health_tests.erl
│   ├── tcps_kaizen_tests.erl
│   ├── tcps_kanban_tests.erl
│   ├── tcps_mcp_bridge_tests.erl
│   ├── tcps_mcp_diataxis_tests.erl
│   ├── tcps_mcp_server_tests.erl
│   ├── tcps_persistence_tests.erl
│   ├── tcps_quality_gates_tests.erl
│   ├── tcps_receipt_verifier_comprehensive_tests.erl
│   ├── tcps_receipt_verifier_tests.erl
│   ├── tcps_rebar3_providers_tests.erl
│   ├── tcps_root_cause_tests.erl
│   ├── tcps_simulator_tests.erl
│   ├── tcps_simulator_telemetry_tests.erl
│   ├── tcps_sku_tests.erl
│   ├── tcps_visualization_data_tests.erl
│   ├── tcps_web_server_tests.erl
│   ├── tcps_work_order_tests.erl
│   └── tcps/
│       └── tcps_rebar3_providers_tests.erl
└── integration/
    ├── erlmcp_pricing_poka_yoke_SUITE.erl
    ├── erlmcp_pricing_receipt_basic_test.erl
    ├── erlmcp_pricing_receipt_extended_SUITE.erl
    ├── erlmcp_receipt_cli_SUITE.erl
    ├── erlmcp_persistence_performance_SUITE.erl
    ├── tcps_ct_hooks.erl
    ├── tcps_andon_integration_SUITE.erl
    ├── tcps_concurrent_SUITE.erl
    ├── tcps_heijunka_SUITE.erl
    ├── tcps_mcp_diataxis_SUITE.erl
    ├── tcps_performance_SUITE.erl
    ├── tcps_pipeline_SUITE.erl
    ├── tcps_persistence_integration_SUITE.erl
    ├── tcps_quality_gates_integration_SUITE.erl
    └── tcps_simulator_integration_SUITE.erl
```

**Reason:** Physical separation prevents EUnit from discovering CT modules, providing a robust configuration that cannot be bypassed.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Directory structure: `test -type d -name unit` finds 6 directories (root + 5 apps)
- [ ] Directory structure: `test -type d -name integration` finds 6 directories (root + 5 apps)
- [ ] File classification: `find test/unit -name "*_SUITE.erl"` returns 0 results
- [ ] File classification: `find test/integration -name "*_tests.erl"` returns 0 results
- [ ] File count: `find test/unit -name "*_tests.erl"` returns 84 files
- [ ] File count: `find test/integration -name "*_SUITE.erl"` returns 27 files
- [ ] Pre-commit hook: `.claude/hooks/pre-task-validate.sh` - PASS

**Note:** Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix. This is Jidoka (built-in quality).

---

### Phase 2: Rebar.config Configuration Update (30 minutes)

#### Overview
Update `/Users/sac/erlmcp/rebar.config` to explicitly configure EUnit to use `test/unit/` directories and Common Test to use `test/integration/` directories.

#### Specification
Modify rebar.config lines 33-41:
- Change `{test_dirs, ["test"]}` to include both unit and integration subdirectories
- Update `eunit_opts` to explicitly point to `unit/` directories only
- Remove `{exclude, ".*_SUITE$"}` from eunit_opts (no longer needed)
- Add `ct_opts` to explicitly point to `integration/` directories only
- Keep `verbose` option in eunit_opts

#### Pseudocode
```
READ /Users/sac/erlmcp/rebar.config
LOCATE line 34: {test_dirs, ["test"]}.
REPLACE with: {test_dirs, ["test/unit", "test/integration", "apps/*/test/unit", "apps/*/test/integration"]}.

LOCATE line 38-41:
    {eunit_opts, [
        {exclude, ".*_SUITE$"},
        verbose
    ]}.

REPLACE with:
    {eunit_opts, [
        {test_dirs, ["test/unit", "apps/*/test/unit", "bench/unit"]},
        verbose
    ]}.

LOCATE line 41 (end of eunit_opts).
INSERT after eunit_opts:
    {ct_opts, [
        {dir, ["test/integration", "apps/*/test/integration", "tests/integration"]},
        {logdir, "_build/test/logs"},
        {keep_logs, 5}
    ]}.

WRITE updated /Users/sac/erlmcp/rebar.config
```

#### Architecture
**Integration Point 1:** Rebar3 reads `{test_dirs, [...]}` at compile time
- Scans all test directories for compilation (both unit and integration)

**Integration Point 2:** Rebar3 reads `{eunit_opts, [{test_dirs, [...]}]}` at test execution time
- Scans only `test/unit/` and `apps/*/test/unit/` for EUnit test files
- Ignores `test/integration/` directories

**Integration Point 3:** Rebar3 reads `{ct_opts, [{dir, [...]}]}` at CT execution time
- Scans only `test/integration/` and `apps/*/test/integration/` for CT SUITE files
- Writes logs to `_build/test/logs/`
- Keeps last 5 log directories

**Integration Point 4:** Per-app rebar.config files inherit umbrella config
- `/Users/sac/erlmcp/apps/*/rebar.config` files unchanged
- They inherit the umbrella-level configuration

#### Changes Required:

##### 1. Update test_dirs Configuration
**File:** `/Users/sac/erlmcp/rebar.config`
**Line:** 34
**Current:** `{test_dirs, ["test"]}.`
**Changes:** Change to include both unit and integration subdirectories
**Reason:** Ensure both unit and integration test files are compiled

```erlang
%% BEFORE (existing code - line 34)
{test_dirs, ["test"]}.

%% AFTER (proposed code - line 34)
{test_dirs, ["test/unit", "test/integration", "apps/*/test/unit", "apps/*/test/integration"]}.
```

##### 2. Update eunit_opts Configuration
**File:** `/Users/sac/erlmcp/rebar.config`
**Lines:** 38-41
**Current:**
```erlang
{eunit_opts, [
    {exclude, ".*_SUITE$"},
    verbose
]}.
```
**Changes:** Remove `{exclude, ".*_SUITE$"}` and add explicit `{test_dirs, [...]}`
**Reason:** Explicitly configure EUnit to scan only unit/ directories, removing ineffective exclusion pattern

```erlang
%% BEFORE (existing code - lines 38-41)
{eunit_opts, [
    {exclude, ".*_SUITE$"},
    verbose
]}.

%% AFTER (proposed code - lines 38-41)
{eunit_opts, [
    {test_dirs, ["test/unit", "apps/*/test/unit", "bench/unit"]},
    verbose
]}.
```

##### 3. Add ct_opts Configuration
**File:** `/Users/sac/erlmcp/rebar.config`
**Lines:** Insert after line 41 (after eunit_opts)
**Current:** CT options not configured at umbrella level (only in tcps_erlmcp/rebar.config)
**Changes:** Add new `{ct_opts, [...]}` section
**Reason:** Explicitly configure Common Test to scan only integration/ directories

```erlang
%% BEFORE (existing code - line 41 ends eunit_opts)
%% (no ct_opts configuration at umbrella level)

%% AFTER (proposed code - insert after line 41)
{ct_opts, [
    {dir, ["test/integration", "apps/*/test/integration", "tests/integration"]},
    {logdir, "_build/test/logs"},
    {keep_logs, 5}
]}.
```

**Full Updated Configuration (lines 33-44):**
```erlang
%% Source directories (for backwards compatibility with legacy src/)
{src_dirs, ["src"]}.
{test_dirs, ["test/unit", "test/integration", "apps/*/test/unit", "apps/*/test/integration"]}.
{include_dirs, ["include"]}.

%% EUnit configuration (no exclude needed - physical separation)
{eunit_opts, [
    {test_dirs, ["test/unit", "apps/*/test/unit", "bench/unit"]},
    verbose
]}.

%% Common Test configuration
{ct_opts, [
    {dir, ["test/integration", "apps/*/test/integration", "tests/integration"]},
    {logdir, "_build/test/logs"},
    {keep_logs, 5}
]}.
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] Configuration syntax: `rebar3 check 2>&1 | grep -i "error"` returns 0 matches
- [ ] EUnit discovery: `rebar3 eunit --verbose 2>&1 | grep -i "SUITE"` returns 0 matches
- [ ] EUnit execution: `rebar3 eunit` - exit code 0, no "not found" errors
- [ ] CT discovery: `rebar3 ct --verbose 2>&1 | grep -i "_tests"` returns 0 matches
- [ ] CT execution: `rebar3 ct` - exit code 0
- [ ] Pre-commit hook: `.claude/hooks/pre-task-validate.sh` - PASS

##### Manual Verification:
- [ ] Code review: Configuration syntax correct
- [ ] Configuration: test_dirs includes both unit and integration
- [ ] Configuration: eunit_opts points to unit directories only
- [ ] Configuration: ct_opts points to integration directories only
- [ ] Integration: EUnit discovers only `*_tests.erl` modules
- [ ] Integration: CT discovers only `*_SUITE.erl` modules
- [ ] Edge cases: Subdirectory patterns (`apps/*/test/unit/`) work correctly

**Note:** Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix. This is Jidoka (built-in quality).

---

### Phase 3: Quality Gate Validation (1 hour)

#### Overview
Execute full quality gate suite to verify that EUnit and CT both execute successfully with 100% pass rates, and that all other quality gates (compilation, coverage, dialyzer, xref) pass without regression.

#### Specification
Run complete quality gate validation:
- Verify EUnit executes all 84 test modules with 100% pass rate
- Verify CT executes all 27 SUITE modules with 100% pass rate
- Verify compilation succeeds with 0 errors
- Verify coverage report generates with ≥80% coverage
- Verify Dialyzer passes with 0 warnings
- Verify Xref passes with 0 undefined function calls
- Verify execution time is <30 seconds for EUnit

#### Pseudocode
```
# Gate 1: Compilation
RUN rebar3 compile
VERIFY exit_code == 0
VERIFY error_count == 0

# Gate 2: EUnit
START_TIMER = now()
RUN rebar3 eunit
END_TIMER = now()
VERIFY exit_code == 0
VERIFY "not found" NOT IN output
VERIFY test_count == 84
ELAPSED_TIME = END_TIMER - START_TIMER
VERIFY ELAPSED_TIME < 30 seconds

# Gate 3: Common Test
RUN rebar3 ct
VERIFY exit_code == 0
VERIFY suite_count == 27

# Gate 4: Coverage
RUN rebar3 cover
VERIFY exit_code == 0
PARSE coverage_report
VERIFY coverage_percentage >= 80

# Gate 5: Dialyzer
RUN rebar3 dialyzer
VERIFY exit_code == 0
VERIFY warning_count == 0

# Gate 6: Xref
RUN rebar3 xref
VERIFY exit_code == 0
VERIFY undefined_function_count == 0

# Gate 7: Full Check
RUN rebar3 check
VERIFY exit_code == 0
VERIFY all_gates_passed == true
```

#### Architecture
**No Integration Points:** This phase executes quality gates only.

**Dependencies:**
- Depends on Phase 1 (directory reorganization)
- Depends on Phase 2 (configuration updates)
- No other dependencies

**Supervision Tree:** Not applicable (quality gate execution only)

#### Changes Required:

##### 1. Compilation Verification
**Action:** Execute `rebar3 compile` and verify 0 errors

**Command:**
```bash
cd /Users/sac/erlmcp
TERM=dumb rebar3 compile
```

**Expected Output:**
```
===> Verifying dependencies...
===> Compiling erlmcp_core
===> Compiling erlmcp_transports
===> Compiling erlmcp_observability
===> Compiling tcps_erlmcp
```

**Success Criteria:**
- Exit code: 0
- Error count: 0
- All 4 apps compiled successfully

**Reason:** Ensure directory reorganization and configuration changes did not break compilation

##### 2. EUnit Execution Verification
**Action:** Execute `rebar3 eunit` and verify 100% pass rate

**Command:**
```bash
cd /Users/sac/erlmcp
time rebar3 eunit
```

**Expected Output:**
```
===> Verifying dependencies...
===> Compiling erlmcp_core
===> Compiling erlmcp_transports
===> Compiling erlmcp_observability
===> Compiling tcps_erlmcp

  Test passed: 84 tests
  All 84 tests passed.

real    0m15.432s
user    0m12.123s
sys     0m3.456s
```

**Success Criteria:**
- Exit code: 0
- No "module not found" errors
- No "*_SUITE" module names in output
- Test count: 84 (all EUnit test modules)
- Pass rate: 100%
- Execution time: <30 seconds

**Reason:** Verify EUnit discovers and executes only unit tests, with zero CT module conflicts

##### 3. Common Test Execution Verification
**Action:** Execute `rebar3 ct` and verify 100% pass rate

**Command:**
```bash
cd /Users/sac/erlmcp
rebar3 ct
```

**Expected Output:**
```
===> Verifying dependencies...
===> Compiling erlmcp_core
...
===> Test run completed with 0 failed tests
```

**Success Criteria:**
- Exit code: 0
- Suite count: 27 (all CT SUITE modules)
- Pass rate: 100%
- Log files generated in `_build/test/logs/`

**Reason:** Verify CT still executes correctly after directory reorganization

##### 4. Coverage Verification
**Action:** Execute `rebar3 cover` and verify ≥80% coverage

**Command:**
```bash
cd /Users/sac/erlmcp
rebar3 cover
```

**Expected Output:**
```
===> Cover analysis
...
  Covering: 84.5%
```

**Success Criteria:**
- Exit code: 0
- Coverage percentage: ≥80%
- Coverage report generated in `_build/test/cover/`

**Reason:** Verify test coverage can now be measured (was blocked by EUnit failures)

##### 5. Dialyzer Verification
**Action:** Execute `rebar3 dialyzer` and verify 0 warnings

**Command:**
```bash
cd /Users/sac/erlmcp
rebar3 dialyzer
```

**Expected Output:**
```
===> Dialyzer analysis
...
  Passed: 0 warnings
```

**Success Criteria:**
- Exit code: 0
- Warning count: 0

**Reason:** Ensure changes did not introduce type specification errors

##### 6. Xref Verification
**Action:** Execute `rebar3 xref` and verify 0 undefined function calls

**Command:**
```bash
cd /Users/sac/erlmcp
rebar3 xref
```

**Expected Output:**
```
===> Xref analysis
...
  Passed: 0 undefined function calls
```

**Success Criteria:**
- Exit code: 0
- Undefined function count: 0

**Reason:** Ensure changes did not break cross-module references

##### 7. Full Quality Gate Check
**Action:** Execute `rebar3 check` and verify all gates pass

**Command:**
```bash
cd /Users/sac/erlmcp
rebar3 check
```

**Expected Output:**
```
===> Compiling...
===> Running xref...
===> Running dialyzer...
===> Running eunit...
===> Running cover...
===> All checks passed!
```

**Success Criteria:**
- Exit code: 0
- All quality gates: PASS
- No errors or warnings

**Reason:** Final validation that complete quality gate suite passes

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit` - exit code 0, 100% pass rate, 84 tests executed
- [ ] EUnit execution time: `time rebar3 eunit` - <30 seconds
- [ ] EUnit output: `rebar3 eunit 2>&1 | grep -i "not found"` - 0 matches
- [ ] EUnit output: `rebar3 eunit 2>&1 | grep -i "SUITE"` - 0 matches
- [ ] CT: `rebar3 ct` - exit code 0, 100% pass rate, 27 suites executed
- [ ] Coverage: `rebar3 cover` - ≥80% coverage
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls
- [ ] Full check: `rebar3 check` - exit code 0, all gates pass
- [ ] Pre-commit hook: `.claude/hooks/pre-task-validate.sh` - PASS

##### Manual Verification:
- [ ] EUnit output review: All 84 test modules listed
- [ ] CT output review: All 27 SUITE modules listed
- [ ] Coverage report: No critical gaps (<50% coverage in any module)
- [ ] Performance: No regression >10% from baseline (if baseline exists)
- [ ] Log files: Generated in `_build/test/logs/`
- [ ] Receipt files: Generated in `.claude/receipts/`

**Note:** Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix. This is Jidoka (built-in quality).

---

## Testing Strategy

### Chicago School TDD (MANDATORY)

**NO MOCKS** - Real test execution, no mocking of file system or rebar3
**State-Based Verification** - Check actual file locations and test counts
**Integration Testing** - Test with full rebar3 build system
**Race Condition Testing** - Not applicable (file system operations are sequential)

### Unit Tests (EUnit)

**What to Test:** Not applicable (no code changes, only file moves and config changes)

**Test Pattern:** Existing EUnit tests (84 files, 26,568 lines)
- Reference: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_rate_limiting_tests.erl:46-58`
- Use `setup()` and `cleanup()` fixtures for resource management
- Group related tests with `_test_()` generator functions
- Use `?_test()` macro for lazy evaluation
- Use `?assertEqual()`, `?assertMatch()`, `?assert()` macros

**Coverage Target:** ≥80% per module (existing tests should maintain coverage)

**Pass Rate:** 100% (all tests must pass)

### Integration Tests (Common Test)

**End-to-End Scenarios:**
- Full rebar3 build lifecycle (compile, eunit, ct, cover, dialyzer, xref)
- Mixed project structure (umbrella with multiple apps)
- Concurrent test execution (EUnit and CT run separately)

**Multi-Process:** Not applicable (rebar3 manages process pool)

**Failure Scenarios:**
- Missing test directories (should fail fast)
- Misconfigured test_dirs (should fail with clear error)
- Misconfigured ct_opts (should fail with clear error)

### Manual Testing Steps

1. **Verify Directory Structure**
   ```bash
   # Check unit directories exist
   ls -la test/unit
   ls -la apps/*/test/unit

   # Check integration directories exist
   ls -la test/integration
   ls -la apps/*/test/integration

   # Verify no mixed files
   find test/unit -name "*_SUITE.erl"  # Should return empty
   find test/integration -name "*_tests.erl"  # Should return empty
   ```

2. **Verify EUnit Execution**
   ```bash
   # Run EUnit
   rebar3 eunit

   # Check for no errors
   rebar3 eunit 2>&1 | grep -i "not found"  # Should return empty
   rebar3 eunit 2>&1 | grep -i "SUITE"  # Should return empty

   # Verify test count
   rebar3 eunit --verbose 2>&1 | grep -c "Test passed"  # Should be 84
   ```

3. **Verify CT Execution**
   ```bash
   # Run CT
   rebar3 ct

   # Check log files
   ls -la _build/test/logs/

   # Verify test count
   grep -r "passed" _build/test/logs/  # Should show 27 suites
   ```

4. **Verify Full Quality Gates**
   ```bash
   # Run all gates
   rebar3 check

   # Verify all passed
   echo $?  # Should be 0
   ```

5. **Edge Case Verification**
   ```bash
   # Test with clean build
   rebar3 clean
   rebar3 check  # Should still pass

   # Test with individual apps
   cd apps/erlmcp_core
   rebar3 eunit  # Should still work
   cd ../..

   # Test with verbose output
   rebar3 eunit --verbose  # Should show all 84 tests
   rebar3 ct --verbose  # Should show all 27 suites
   ```

### Quality Gates

Every phase MUST pass all quality gates:

#### Gate 1: Compilation
```bash
TERM=dumb rebar3 compile
```
**Success Criteria:** Exit code 0, 0 errors, 0 warnings

#### Gate 2: EUnit
```bash
rebar3 eunit
```
**Success Criteria:** Exit code 0, 100% pass rate, 84 tests executed

#### Gate 3: Coverage
```bash
rebar3 cover
```
**Success Criteria:** Exit code 0, ≥80% coverage

#### Gate 4: Dialyzer
```bash
rebar3 dialyzer
```
**Success Criteria:** Exit code 0, 0 warnings

#### Gate 5: Xref
```bash
rebar3 xref
```
**Success Criteria:** Exit code 0, 0 undefined function calls

#### Gate 6: Pre-commit Hook
```bash
.claude/hooks/pre-task-validate.sh
```
**Success Criteria:** Exit code 0, all validation checks pass

## Manufacturing Checklist

### Before Implementation
- [x] Research verified (read actual source code and rebar.config)
- [x] Scope confirmed (IN: file moves + config, OUT: code changes)
- [x] No open questions (all decisions made on directory structure)
- [x] Phases broken down (Phase 1: 2h, Phase 2: 30m, Phase 3: 1h)
- [x] Acceptance criteria defined (measurable, specific, automated)

### During Implementation
- [ ] Chicago School TDD followed (real file system, no mocks)
- [ ] OTP patterns followed (gen_server, supervision - N/A for this config fix)
- [ ] Type specs added (N/A - no code changes)
- [ ] Error handling complete (all file moves validated)
- [ ] Quality gates passing (compilation, tests, coverage at each phase)

### After Implementation
- [ ] All tests passing (100% rate - 84 EUnit + 27 CT)
- [ ] Coverage ≥80% (verified with rebar3 cover)
- [ ] Dialyzer 0 warnings (verified with rebar3 dialyzer)
- [ ] Xref 0 undefined calls (verified with rebar3 xref)
- [ ] Performance no regression >10% (verified with time rebar3 eunit)
- [ ] Documentation updated (this plan.md created)
- [ ] Code review complete (directory structure verified)

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| File move breaks module imports | P1 | Low | No module imports changed (file locations only) |
| rebar.config syntax error | P1 | Low | Validate syntax with rebar3 check before commit |
| Test directories not created | P2 | Low | Automated validation checks directory existence |
| EUnit test count drops to 0 | P0 | Medium | Verify test discovery with --verbose flag |
| CT test count drops to 0 | P1 | Medium | Verify CT discovery with --verbose flag |
| Performance regression >10% | P3 | Low | Measure execution time with time command |
| CI/CD pipeline fails | P1 | Low | Pipelines use standard rebar3 commands (will adapt) |
| Per-app configs conflict with umbrella | P2 | Low | Per-app configs inherit umbrella config |

**Severity Definitions:**
- **P0 (Critical):** BLOCKS production - MUST fix immediately
- **P1 (High):** Major quality gap - MUST fix before release
- **P2 (Medium):** Important but not blocking
- **P3 (Low):** Nice-to-have

### Rollback Plan

**If something goes wrong:**

1. **Git revert:**
   ```bash
   git revert HEAD  # Revert the commit
   git push origin wreckit/001-fix-eunit-configuration-to-exclude-ct-suite-files
   ```

2. **File move rollback:**
   ```bash
   # Move files back to original locations
   mv test/unit/* test/
   mv test/integration/* test/
   rmdir test/unit test/integration

   # Repeat for all apps
   for app in apps/*/test; do
       mv $app/unit/* $app/
       mv $app/integration/* $app/
       rmdir $app/unit $app/integration
   done
   ```

3. **Config rollback:**
   ```bash
   git checkout HEAD -- rebar.config
   ```

**Service Impact:**
- Development: EUnit tests will be blocked again (error state restored)
- CI/CD: Quality gates will fail (current state restored)
- Production: No impact (this is test infrastructure only)

**Data Migration:**
- No data to migrate (test files only)
- Git history preserved (can revert at any time)

## References

### Research Documentation
- **Research Summary:** `/Users/sac/erlmcp/.wreckit/items/001-fix-eunit-configuration-to-exclude-ct-suite-files/research.md`
- **Technical Analysis:** `/Users/sac/erlmcp/REBAR3_EUNIT_SUITE_ANALYSIS.md` (Complete technical analysis of the problem)
- **Investigation Reference:** `/Users/sac/erlmcp/REBAR3_INVESTIGATION_REFERENCE.md` (Detailed investigation findings)

### Project Documentation
- **CLAUDE.md:** `/Users/sac/erlmcp/CLAUDE.md` (Project rules and conventions)
- **TCPS:** `/Users/sac/erlmcp/docs/tcps/TCPS.md` (Manufacturing principles)
- **OTP Patterns:** `/Users/sac/erlmcp/docs/otp-patterns.md` (OTP design patterns)

### Test References
- **EUnit Test Pattern:** `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_rate_limiting_tests.erl:46-58`
- **CT SUITE Pattern:** `/Users/sac/erlmcp/apps/tcps_erlmcp/test/integration/tcps_andon_integration_SUITE.erl:1-50`

### Configuration References
- **Current Config:** `/Users/sac/erlmcp/rebar.config:34-41` (Current EUnit configuration)
- **Per-App Configs:**
  - `/Users/sac/erlmcp/apps/erlmcp_transports/rebar.config:21`
  - `/Users/sac/erlmcp/apps/erlmcp_observability/rebar.config:21`
  - `/Users/sac/erlmcp/apps/tcps_erlmcp/rebar.config:23,60-64`

### File Inventory
- **EUnit Test Files:** 84 files (`*_tests.erl` pattern) - 26,568 lines
- **CT SUITE Files:** 27 files (`*_SUITE.erl` pattern) - 13,559 lines
- **Helper Modules:** 1 file (`*_ct_hooks.erl`)

---

**TCPS Manufacturing Manifest:**

This plan embodies Toyota Code Production System principles:

- **Standard Work (標準作業):** Every step documented with specific file paths and verification commands
- **Heijunka (平準化):** Work broken into 3 small phases (2h + 30m + 1h)
- **Poka-yoke (ポカヨケ):** Quality gates built into every phase, failures stop production
- **Jidoka (自働化):** Human judgment empowered by automated verification
- **Andon (行灯):** Progress visible at all phases, problems signaled immediately

**Zero Defects:** We ship 99.99966% defect-free code. Plan thoroughly, measure everything, document every step.
