# Fix erlmcp_observability_SUITE Ranch/Cowboy dashboard startup failure Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Fix the critical test suite startup failure in `erlmcp_observability_SUITE` that prevents ALL observability integration tests from executing. The root cause is a missing dependency startup sequence: Ranch application supervision tree is not started before Cowboy attempts to create the dashboard listener, causing a `noproc` error when Cowboy tries to call `ranch_sup`.

**WHY this matters:** This is a **P0 (Critical)** blocking issue because it prevents 100% of observability integration tests from running, violating the 99.99966% defect-free delivery standard. Currently, 0 out of 4 test cases can execute (100% failure rate). The fix is straightforward: explicitly start Ranch (and Cowboy) before starting the erlmcp_observability application, following the established OTP pattern used in `erlmcp_transport_integration_SUITE`.

### Quality Gate Requirements

- **Compilation**: 0 errors (MANDATORY)
- **EUnit**: 100% pass rate (MANDATORY - if any EUnit tests exist)
- **Common Test**: 4/4 tests passing (100% pass rate - MANDATORY)
- **Coverage**: ≥80% for erlmcp_observability modules (MANDATORY)
- **Dialyzer**: 0 warnings (MANDATORY)
- **Xref**: 0 undefined function calls (MANDATORY)
- **Performance**: Not applicable (no performance-critical code changed)

## Current State

### What Exists Now

**Modules:**
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl` - Failing test suite (lines 27-30 missing dependency startup)
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_dashboard_server.erl` - Cowboy HTTP/WebSocket server (lines 106-110 call cowboy:start_clear/3)
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability_sup.erl` - Supervisor starting dashboard as child (lines 88-96)
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability_app.erl` - Application callback
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability.app.src` - Application metadata (lines 9-16 missing cowboy/ranch)

**Tests:**
- Current Coverage: 0% (all tests blocked by init_per_suite failure)
- Test Count: 4 test cases
- Pass Rate: 0% (0/4 passing)
- Failure Mode: init_per_suite failure with `{noproc, {gen_server,call, [ranch_sup, ...]}}` error

**Quality:**
- Compilation: ✅ Pass (0 errors)
- EUnit: Not run (blocked by CT failure)
- Common Test: ❌ 100% failure (0/4 tests blocked)
- Coverage: ❌ 0% (no tests executed)
- Dialyzer: Not checked (blocked by test failure)
- Xref: Not checked (blocked by test failure)

### What's Missing

**Gap:** 100% test failure rate vs 0% target. All 4 test cases are blocked from executing because the suite cannot initialize.

**Root Cause:** The test suite's `init_per_suite/1` function (lines 27-30) only calls `application:ensure_all_started(erlmcp_observability)` but does not explicitly start the Ranch application first. When `erlmcp_observability_app` starts its supervision tree, it launches `erlmcp_dashboard_server` which calls `cowboy:start_clear/3` (line 106). Cowboy internally tries to call `ranch_sup` to start the listener, but Ranch's supervisor process is not running, causing a `noproc` error.

**Impact:** BLOCKS ALL integration testing for observability functionality. Cannot verify metrics, health monitoring, OpenTelemetry integration, or full observability stack. This is a **Jidoka** violation - the test suite lacks "built-in quality" with proper dependency validation.

### Key Discoveries from Research

1. **Working Pattern Exists**: `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_integration_SUITE.erl:36-37` shows the correct pattern:
   ```erlang
   {ok, _} = application:ensure_all_started(gproc),
   {ok, _} = application:ensure_all_started(ranch),  %% ✅ Started BEFORE cowboy
   {ok, _} = application:ensure_all_started(gun),
   {ok, _} = application:ensure_all_started(ssl),
   {ok, _} = application:ensure_all_started(erlmcp_transports),
   ```

2. **Dependency Chain is Clear**: gproc → ranch → cowboy → erlmcp_observability. Ranch must be started before Cowboy because Cowboy depends on Ranch's supervisor for listener management.

3. **Application Metadata is Incomplete**: The `erlmcp_observability.app.src` file (lines 9-16) does not list `cowboy` or `ranch` in its `{applications, [...]}` list. This is intentional OTP design (applications should declare only direct dependencies), but it means test suites must start transitive dependencies explicitly.

4. **Dashboard Server Requires Ranch**: The `erlmcp_dashboard_server:init/1` function (lines 106-110) calls `cowboy:start_clear/3` which internally requires Ranch's supervisor to be available. This is the point of failure.

5. **Cleanup is Also Missing**: The `end_per_suite/1` function (lines 32-34) only stops `erlmcp_observability` but does not stop cowboy, ranch, or gproc. This can leave processes running after tests complete.

## Desired End State

### Specification

**Modified File**: `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl`

**init_per_suite/1 Function:**
```erlang
%% @doc Initialize test suite by starting applications in dependency order.
%%
%% CRITICAL: Dependencies must be started BEFORE erlmcp_observability:
%%   1. gproc - Required by erlmcp_core for process registry
%%   2. ranch - REQUIRED before cowboy (cowboy:start_clear/3 needs ranch_sup)
%%   3. cowboy - HTTP server used by erlmcp_dashboard_server
%%   4. erlmcp_observability - Application under test
%%
%% Reference: erlmcp_transport_integration_SUITE.erl:36-37 (same pattern)
%% Failure mode: If ranch is not started, cowboy fails with {noproc, ranch_sup}
init_per_suite(Config) ->
    %% Start dependencies in correct order
    {ok, _} = application:ensure_all_started(gproc),
    ct:log("Dependency gproc started"),

    {ok, _} = application:ensure_all_started(ranch),
    ct:log("Dependency ranch started"),

    {ok, _} = application:ensure_all_started(cowboy),
    ct:log("Dependency cowboy started"),

    %% Start observability application
    {ok, _} = application:ensure_all_started(erlmcp_observability),
    ct:log("Observability application started with all dependencies"),

    %% Verify dashboard server is running (defensive programming)
    {ok, Port} = erlmcp_dashboard_server:get_port(),
    ct:log("Dashboard server listening on port ~p", [Port]),

    [{app_started, true} | Config].
```

**end_per_suite/1 Function:**
```erlang
%% @doc Cleanup test suite by stopping applications in reverse order.
%%
%% CRITICAL: Stop in REVERSE order (LIFO) to avoid crashes:
%%   - erlmcp_observability depends on cowboy
%%   - cowboy depends on ranch
%%   - ranch depends on gproc (indirectly)
%%
%% If stopped in wrong order, applications crash when trying to use stopped dependencies.
end_per_suite(_Config) ->
    %% Stop applications in reverse order
    application:stop(erlmcp_observability),
    ct:log("Application erlmcp_observability stopped"),

    application:stop(cowboy),
    ct:log("Application cowboy stopped"),

    application:stop(ranch),
    ct:log("Application ranch stopped"),

    application:stop(gproc),
    ct:log("Application gproc stopped"),

    ok.
```

### Verification

**Automated Verification Commands:**
```bash
# Run the specific test suite
cd /Users/sac/erlmcp
rebar3 ct --suite=erlmcp_observability_SUITE

# Expected output:
# TEST COMPLETE, 4 tests, 0 skipped, 0 failed

# Verify all tests pass
rebar3 ct --verbose | grep "TEST COMPLETE"

# Check coverage
rebar3 cover --verbose | grep erlmcp_observability

# Run dialyzer
rebar3 dialyzer

# Run xref
rebar3 xref
```

**Manual Verification Steps:**
1. Inspect test logs for "Dashboard server started successfully"
2. Verify no `{noproc, {gen_server,call, [ranch_sup, ...]}}` errors
3. Verify all 4 test cases show "ok" instead of "auto_skipped"
4. Confirm coverage report shows ≥80% for erlmcp_dashboard_server
5. Verify no orphaned processes after test completion

### Manufacturing Output

- **Code Modified**: 1 file
  - `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl` (lines 27-42, including documentation)
- **Tests Created**: 0 files (modifying existing test suite)
- **Documentation Updated**: Code comments added (Poka-yoke documentation)
- **Receipts Generated**:
  - Test execution log showing 4/4 tests passing
  - Coverage report showing ≥80% coverage
  - Dialyzer report showing 0 warnings
  - Xref report showing 0 undefined function calls

## What We're NOT Doing

**EXPLICITLY OUT OF SCOPE:**

1. **Modifying erlmcp_observability.app.src** - Not adding cowboy/ranch to applications list because:
   - OTP best practice: applications should declare only direct dependencies
   - cowboy/ranch are transitive dependencies (used by dashboard_server, not the app)
   - Test suites are responsible for starting transitive dependencies
   - Release management (rebar3/relx) handles transitive dependencies correctly

2. **Making dashboard server optional** - Not disabling dashboard for tests because:
   - The dashboard is part of the observability system and should be tested
   - The fix is simple (start dependencies) - no need to work around it
   - Disabling components would reduce test coverage

3. **Starting full erlmcp_observability app in different way** - Not using application:start/1 instead of ensure_all_started because:
   - ensure_all_started is the correct OTP API for starting with dependencies
   - The working pattern (transport suite) uses ensure_all_started
   - Changing the startup method would be a deviation from established patterns

4. **Adding port configuration for tests** - Not setting dashboard_port to 0 (random) for testing because:
   - Port conflicts are not part of the current failure (the suite doesn't get that far)
   - Can be added in a future improvement if needed
   - Out of scope for this critical bug fix

5. **Adding integration tests for dashboard** - Not adding dashboard-specific tests because:
   - The existing 4 test cases provide sufficient coverage once they can run
   - Additional tests can be added in a separate story
   - Focus is on unblocking existing tests, not expanding test scope

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** - Requirements with acceptance criteria
   - MUST start gproc before ranch (required by erlmcp_core)
   - MUST start ranch before cowboy (cowboy depends on ranch)
   - MUST start cowboy before erlmcp_observability (belt-and-suspenders)
   - MUST verify dashboard server is running after startup
   - MUST stop applications in reverse order in end_per_suite
   - MUST pass all 4 test cases
   - MUST achieve ≥80% code coverage

2. **Pseudocode** - Algorithm design BEFORE coding
   ```
   init_per_suite(Config):
       Start gproc application
       Start ranch application
       Start cowboy application
       Start erlmcp_observability application
       Verify dashboard server is running (get_port)
       Log successful startup
       Return Config

   end_per_suite(_Config):
       Stop erlmcp_observability
       Stop cowboy
       Stop ranch
       Stop gproc
   ```

3. **Architecture** - Integration points and dependencies
   - **Dependency Chain**: gproc → ranch → cowboy → erlmcp_observability
   - **Process Tree**: ranch_sup → cowboy_listener_sup → erlmcp_dashboard_server
   - **Test Isolation**: Each test case starts its own processes (metrics, health_monitor)
   - **Cleanup**: Reverse order shutdown prevents dangling processes
   - **Error Handling**: application:ensure_all_started returns {ok, StartedApps} or {error, Reason}

4. **Refinement** - Chicago School TDD (tests FIRST)
   - No new tests needed (modifying existing suite initialization)
   - Existing tests will unblock and execute successfully
   - Test execution IS the verification

5. **Completion** - All quality gates passing
   - Compilation: 0 errors
   - EUnit: 100% pass rate (if any EUnit tests exist)
   - Common Test: 4/4 tests passing (100%)
   - Coverage: ≥80% for erlmcp_observability modules
   - Dialyzer: 0 warnings
   - Xref: 0 undefined function calls

### Implementation Strategy

**High-Level Approach:**

This fix follows the **Poka-yoke** (mistake-proofing) principle by making the dependency chain explicit in the test suite startup. The approach is:

1. **Add Missing Dependencies** - Explicitly start gproc, ranch, and cowboy before erlmcp_observability
2. **Follow Established Pattern** - Use the exact same pattern as erlmcp_transport_integration_SUITE
3. **Verify Startup** - Add verification that dashboard server is running (defensive programming)
4. **Proper Cleanup** - Stop applications in reverse order to prevent orphaned processes

**WHY This Strategy:**

- **Minimal Change**: Only modifies 6 lines of code (add 4 lines to init_per_suite, modify 4 lines in end_per_suite)
- **Follows OTP Best Practices**: Explicit dependency management in test suites
- **Proven Pattern**: Uses the exact approach from the working transport suite
- **Zero Risk**: No changes to production code, only test code
- **Immediate Value**: Unblocks 100% of observability integration tests

**Alternative Strategies Considered:**

1. **Add cowboy/ranch to applications list in .app.src** - Rejected because:
   - Violates OTP design principle (applications should list direct dependencies only)
   - cowboy/ranch are implementation details of dashboard_server, not app dependencies
   - Would require changing release configuration

2. **Make dashboard optional for tests** - Rejected because:
   - Reduces test coverage (dashboard wouldn't be tested)
   - Adds unnecessary complexity (conditional startup logic)
   - The fix is simpler (just start dependencies)

3. **Use application:start instead of ensure_all_started** - Rejected because:
   - ensure_all_started is the correct OTP API
   - Would require manually starting transitive dependencies
   - Deviates from established patterns in the codebase

### Quality Integration

**Pre-commit Hooks:**
- `.claude/hooks/pre-task-validate.sh` - Validates compilation, basic syntax
- MUST pass before allowing commit

**CI Gates:**
- Compilation: `TERM=dumb rebar3 compile` - 0 errors
- EUnit: `rebar3 eunit` - 100% pass rate
- Common Test: `rebar3 ct --suite=erlmcp_observability_SUITE` - 4/4 passing
- Coverage: `rebar3 cover` - ≥80% coverage
- Dialyzer: `rebar3 dialyzer` - 0 warnings
- Xref: `rebar3 xref` - 0 undefined function calls

**Receipt Generation:**
- Test execution log: `_build/test/logs/erlmcp_observability_SUITE.html`
- Coverage report: `_build/test/cover/index.html`
- Dialyzer log: `_build/test/dialyzer/analysis.log`
- Xref log: `_build/test/xref/refusion_xref.log`

**Andon Signaling:**
- Test failures visible immediately in CI output
- Compilation errors stop the build
- Dialyzer warnings block merge
- Coverage <80% is a quality gate failure

---

## Phases

### Phase 1: Fix init_per_suite Startup Sequence

#### Overview

Add explicit dependency startup to `init_per_suite/1` following the established OTP pattern from `erlmcp_transport_integration_SUITE`. This unblocks all 4 test cases by ensuring Ranch is started before Cowboy attempts to create the dashboard listener.

**Estimated Time**: 30 minutes
**Complexity**: Low (straightforward code addition)
**Risk**: None (only test code, no production changes)

#### Specification

**WHAT we're building:**

Modified `init_per_suite/1` function in `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl`:

- Add `application:ensure_all_started(gproc)` - required by erlmcp_core
- Add `application:ensure_all_started(ranch)` - REQUIRED before cowboy
- Add `application:ensure_all_started(cowboy)` - belt-and-suspenders approach
- Keep existing `application:ensure_all_started(erlmcp_observability)`
- Add verification call to `erlmcp_dashboard_server:get_port()` to confirm startup
- Add logging for successful dependency chain startup

#### Pseudocode

```
init_per_suite(Config):
    try
        % Start dependency chain in order
        {ok, GprocApps} = application:ensure_all_started(gproc)
        log("Dependency gproc started")

        {ok, RanchApps} = application:ensure_all_started(ranch)
        log("Dependency ranch started")

        {ok, CowboyApps} = application:ensure_all_started(cowboy)
        log("Dependency cowboy started")

        % Start application under test
        {ok, ObsApps} = application:ensure_all_started(erlmcp_observability)
        log("Observability application started with all dependencies")

        % Verify dashboard server is running
        {ok, Port} = erlmcp_dashboard_server:get_port()
        log("Dashboard server listening on port ~p", [Port])

        return [{app_started, true} | Config]
    catch
        {error, Reason} ->
            ct:fail("Failed to start dependencies: ~p", [Reason])
    end
```

#### Architecture

**INTEGRATION - Dependency Chain:**

```
gproc (registry)
  ↓
ranch (TCP supervisor)
  ↓
cowboy (HTTP server)
  ↓
erlmcp_observability (application under test)
  ↓
erlmcp_observability_sup (supervisor)
  ↓
erlmcp_dashboard_server (gen_server)
  ↓
cowboy_listener (HTTP listener)
```

**Process Registration:**
- ranch_sup registered locally as ranch_sup
- cowboy_listener_sup registered locally as erlmcp_dashboard_listener
- erlmcp_dashboard_server registered locally as erlmcp_dashboard_server

**Error Handling:**
- application:ensure_all_started returns {ok, StartedApps} or {error, Reason}
- Any failure in the chain causes init_per_suite to fail (CT suite abort)
- Failure is immediately visible (Andon signaling)

#### Changes Required:

##### 1. Test Suite init_per_suite Function

**File**: `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl`
**Current**: Lines 27-30
**Changes**: Add explicit dependency startup before erlmcp_observability
**Reason**: Ranch must be started before Cowboy can create listeners

```erlang
%% BEFORE (existing code - lines 27-30)
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp_observability),
    ct:log("Observability application started"),
    Config.

%% AFTER (proposed code - lines 24-49)
%% @doc Initialize test suite by starting applications in dependency order.
%%
%% CRITICAL: Dependencies must be started BEFORE erlmcp_observability:
%%   1. gproc - Required by erlmcp_core for process registry
%%   2. ranch - REQUIRED before cowboy (cowboy:start_clear/3 needs ranch_sup)
%%   3. cowboy - HTTP server used by erlmcp_dashboard_server
%%   4. erlmcp_observability - Application under test
%%
%% Reference: erlmcp_transport_integration_SUITE.erl:36-37 (same pattern)
%% Failure mode: If ranch is not started, cowboy fails with {noproc, ranch_sup}
init_per_suite(Config) ->
    %% Start dependencies in correct order
    {ok, _} = application:ensure_all_started(gproc),
    ct:log("Dependency gproc started"),

    {ok, _} = application:ensure_all_started(ranch),
    ct:log("Dependency ranch started"),

    {ok, _} = application:ensure_all_started(cowboy),
    ct:log("Dependency cowboy started"),

    %% Start observability application
    {ok, _} = application:ensure_all_started(erlmcp_observability),
    ct:log("Observability application started with all dependencies"),

    %% Verify dashboard server is running (defensive programming)
    {ok, Port} = erlmcp_dashboard_server:get_port(),
    ct:log("Dashboard server listening on port ~p", [Port]),

    [{app_started, true} | Config].
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **Common Test**: `rebar3 ct --suite=erlmcp_observability_SUITE` - 4/4 tests passing
- [ ] **Test Log**: `_build/test/logs/erlmcp_observability_SUITE/suite.log` shows:
  - "Dependency gproc started"
  - "Dependency ranch started"
  - "Dependency cowboy started"
  - "Observability application started with all dependencies"
  - "Dashboard server listening on port 9090"
  - NO `{noproc, {gen_server,call, [ranch_sup, ...]}}` errors
- [ ] **Coverage**: `rebar3 cover` - ≥80% coverage for erlmcp_observability modules
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls
- [ ] **Pre-commit hook**: `.claude/hooks/pre-task-validate.sh` - PASS

##### Manual Verification:
- [ ] **Code Review**: Changes follow established pattern from erlmcp_transport_integration_SUITE:36-37
- [ ] **Test Execution**: All 4 test cases execute (not auto_skipped)
- [ ] **Test Results**: All 4 tests show "ok" status
  - test_metrics_integration: ok
  - test_otel_integration: ok
  - test_health_integration: ok
  - test_full_observability_stack: ok
- [ ] **Dashboard Verification**: get_port() returns {ok, 9090} or {ok, <random_port>}
- [ ] **Process Cleanup**: No orphaned processes after test completion

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix. This is Jidoka (built-in quality).

---

### Phase 2: Fix end_per_suite Dependency Shutdown

#### Overview

Add reverse-order shutdown to `end_per_suite/1` to prevent dangling processes after test execution. This is Kaizen (continuous improvement) - ensuring clean test environment.

**Estimated Time**: 30 minutes
**Complexity**: Low (straightforward code addition)
**Risk**: None (only test code, no production changes)

#### Specification

**WHAT we're building:**

Modified `end_per_suite/1` function in `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl`:

- Add `application:stop(cowboy)` after erlmcp_observability
- Add `application:stop(ranch)` after cowboy
- Add `application:stop(gproc)` after ranch
- Add logging for each application stop

#### Pseudocode

```
end_per_suite(_Config):
    % Stop applications in reverse order (LIFO)
    application:stop(erlmcp_observability)
    log("Application erlmcp_observability stopped")

    application:stop(cowboy)
    log("Application cowboy stopped")

    application:stop(ranch)
    log("Application ranch stopped")

    application:stop(gproc)
    log("Application gproc stopped")

    return ok
```

#### Architecture

**SHUTDOWN ORDER (LIFO - Last In First Out):**

```
erlmcp_observability (stops dashboard_server, metrics, health_monitor, etc.)
  ↓
cowboy (stops HTTP listeners)
  ↓
ranch (stops TCP connection supervisors)
  ↓
gproc (stops process registry)
```

**Why Reverse Order:**
- `erlmcp_observability` depends on `cowboy` (dashboard server)
- `cowboy` depends on `ranch` (listener supervision)
- `ranch` depends on `gproc` (process registry, indirectly)
- Stopping in reverse order ensures dependencies are still available when needed

#### Changes Required:

##### 1. Test Suite end_per_suite Function

**File**: `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl`
**Current**: Lines 32-34
**Changes**: Add reverse-order shutdown for all dependencies
**Reason**: Prevent orphaned processes, clean test environment

```erlang
%% BEFORE (existing code - lines 32-34)
end_per_suite(_Config) ->
    application:stop(erlmcp_observability),
    ok.

%% AFTER (proposed code - lines 51-67)
%% @doc Cleanup test suite by stopping applications in reverse order.
%%
%% CRITICAL: Stop in REVERSE order (LIFO) to avoid crashes:
%%   - erlmcp_observability depends on cowboy
%%   - cowboy depends on ranch
%%   - ranch depends on gproc (indirectly)
%%
%% If stopped in wrong order, applications crash when trying to use stopped dependencies.
end_per_suite(_Config) ->
    %% Stop applications in reverse order
    application:stop(erlmcp_observability),
    ct:log("Application erlmcp_observability stopped"),

    application:stop(cowboy),
    ct:log("Application cowboy stopped"),

    application:stop(ranch),
    ct:log("Application ranch stopped"),

    application:stop(gproc),
    ct:log("Application gproc stopped"),

    ok.
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **Common Test**: `rebar3 ct --suite=erlmcp_observability_SUITE` - 4/4 tests passing (100%)
- [ ] **Coverage**: `rebar3 cover` - ≥80% coverage (no regression from Phase 1)
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings (no regression from Phase 1)
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls (no regression from Phase 1)
- [ ] **Pre-commit hook**: `.claude/hooks/pre-task-validate.sh` - PASS

##### Manual Verification:
- [ ] **Process cleanup**: Verify no ranch/cowboy/gproc processes remain after test execution
  - Check: `erlang:processes()` after test suite completes
  - Expected: No ranch_sup, cowboy_sup, or gproc processes
- [ ] **Log inspection**: All stop logs present ("Application X stopped")
- [ ] **Port cleanup**: Port 9090 no longer listening after test suite completes
- [ ] **Repeatability**: Run test suite twice consecutively - second run succeeds without port conflicts

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix. This is Jidoka (built-in quality).

---

## Testing Strategy

### Chicago School TDD (MANDATORY)

- **NO MOCKS** - Use real processes, real gen_servers, real applications
- **State-Based Verification** - Check actual process state via sys:get_state/1 or gen_server:call/2
- **Integration Testing** - Test with real dependencies (gproc, ranch, cowboy)
- **Race Condition Testing** - Not applicable (no concurrent operations in this fix)

### Unit Tests (EUnit)

**What to Test**: No EUnit tests added in this phase (modifying existing CT suite only)

**Test Pattern**: N/A

**Coverage Target**: ≥80% for erlmcp_observability modules (achieved by running existing CT tests)

**Pass Rate**: 100% (all 4 CT tests must pass)

### Integration Tests (Common Test)

**End-to-End Scenarios:**
1. **test_metrics_integration** - Verify metrics recording and retrieval
2. **test_otel_integration** - Verify OpenTelemetry span creation
3. **test_health_integration** - Verify health monitoring registration
4. **test_full_observability_stack** - Verify all components working together

**Multi-Process**: Each test starts its own processes (erlmcp_metrics, erlmcp_health_monitor)

**Failure Scenarios**: Covered by existing test error handling (bad arguments, missing processes)

### Manual Testing Steps

1. **Run the test suite**:
   ```bash
   cd /Users/sac/erlmcp
   rebar3 ct --suite=erlmcp_observability_SUITE
   ```

2. **Verify test output**:
   - Should see "TEST COMPLETE, 4 tests, 0 skipped, 0 failed"
   - Should NOT see any {noproc} errors
   - Should see "Dashboard server listening on port X" in logs

3. **Check coverage report**:
   ```bash
   rebar3 cover
   open _build/test/cover/index.html
   ```
   - Verify erlmcp_dashboard_server ≥80%
   - Verify erlmcp_observability_app ≥80%
   - Verify erlmcp_observability_sup ≥80%

4. **Run dialyzer**:
   ```bash
   rebar3 dialyzer
   ```
   - Should see 0 warnings
   - Should see "Success: Dialyzer passed successfully"

5. **Run xref**:
   ```bash
   rebar3 xref
   ```
   - Should see 0 undefined function calls
   - Should see "No undefined function calls"

### Quality Gates

Every phase MUST pass ALL gates:

1. **Compilation**: `TERM=dumb rebar3 compile`
   - **Threshold**: 0 errors, 0 warnings
   - **Command**: `rebar3 compile 2>&1 | grep -i "error\|warning"` should return nothing

2. **EUnit**: `rebar3 eunit`
   - **Threshold**: 100% pass rate
   - **Command**: `rebar3 eunit 2>&1 | grep "Test passed"` should show 100%

3. **Common Test**: `rebar3 ct --suite=erlmcp_observability_SUITE`
   - **Threshold**: 4/4 tests passing (100%)
   - **Command**: `rebar3 ct 2>&1 | grep "TEST COMPLETE"` should show "4 tests, 0 skipped, 0 failed"

4. **Coverage**: `rebar3 cover`
   - **Threshold**: ≥80% coverage for erlmcp_observability modules
   - **Command**: `rebar3 cover --verbose 2>&1 | grep erlmcp_observability` should show ≥80.0%

5. **Dialyzer**: `rebar3 dialyzer`
   - **Threshold**: 0 warnings
   - **Command**: `rebar3 dialyzer 2>&1 | grep -i "warning"` should return nothing

6. **Xref**: `rebar3 xref`
   - **Threshold**: 0 undefined function calls
   - **Command**: `rebar3 xref 2>&1 | grep "undefined function"` should return nothing

## Manufacturing Checklist

### Before Implementation
- [x] Research verified (read actual source code - all files verified)
- [x] Scope confirmed (IN/OUT documented - only test suite modified)
- [x] No open questions (all research complete - dependency chain clear)
- [x] Phases broken down (2 phases, ≤1 hour total)
- [x] Acceptance criteria defined (measurable, specific - 4/4 tests passing)

### During Implementation
- [ ] Chicago School TDD followed (tests FIRST - using existing CT tests)
- [ ] OTP patterns followed (gen_server, supervisor, application lifecycle)
- [ ] Type specs preserved (N/A - not adding new functions)
- [ ] Error handling complete (all paths - application:ensure_all_started returns errors)
- [ ] Quality gates passing (compilation, tests, coverage, dialyzer, xref)

### After Implementation
- [ ] All tests passing (100% rate - 4/4 tests)
- [ ] Coverage ≥80% (verified via rebar3 cover)
- [ ] Dialyzer 0 warnings (verified via rebar3 dialyzer)
- [ ] Xref 0 undefined calls (verified via rebar3 xref)
- [ ] Performance no regression >10% (N/A - no perf code changed)
- [ ] Documentation updated (code comments added explaining dependency order)
- [ ] Code review complete (OTP patterns verified)

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **Incomplete dependency chain** - Adding only ranch but missing gproc | P1 | Low | Start gproc explicitly (required by erlmcp_core) |
| **Port conflict** - Dashboard tries to bind to port 9090 which may be in use | P2 | Low | Port 9090 is unlikely to be in use; can make configurable in future if needed |
| **Missing priv files** - Dashboard HTML/JS files not in test build | P2 | Low | Test will fail with clear error message; priv files should be in repository |
| **Application stop order** - end_per_suite may stop apps in wrong order | P2 | Low | Stop applications in reverse order: erlmcp_observability → cowboy → ranch → gproc |
| **Transient Cowboy startup** - Cowboy might start ranch automatically | P3 | Low | Belt-and-suspenders approach - start both explicitly anyway |

**Severity Definitions:**
- **P0 (Critical):** BLOCKS production - MUST fix immediately
- **P1 (High):** Major quality gap - MUST fix before release
- **P2 (Medium):** Important but not blocking
- **P3 (Low):** Nice-to-have

### Rollback Plan

**How to rollback if something goes wrong:**

1. **Git Revert**: Revert the single commit that modified erlmcp_observability_SUITE.erl
   ```bash
   git revert HEAD
   git push origin wreckit/015-fix-erlmcpobservabilitysuite-ranchcowboy-dashboard
   ```

2. **Data Migration**: N/A (no data changes, only test code)

3. **Service Impact**: None (only test code affected, no production services)

4. **Recovery**: Tests will go back to current failing state (0/4 passing), which is the pre-fix baseline

## References

- **Research**: `/Users/sac/erlmcp/.wreckit/items/015-fix-erlmcpobservabilitysuite-ranchcowboy-dashboard/research.md`
- **CLAUDE.md**: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- **TCPS**: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- **OTP Patterns**: `/Users/sac/erlmcp/docs/otp-patterns.md`
- **Working Test Reference**: `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_integration_SUITE.erl:36-37`
- **Failing Test Suite**: `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl:27-30`
- **Dashboard Server**: `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_dashboard_server.erl:92-121`
- **Application Metadata**: `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability.app.src:9-16`

---

**Manufacturing Standard: Zero Defects Delivered**

This plan follows TCPS principles:
- ✅ **Standard Work**: Every step documented, every step measurable
- ✅ **Heijunka**: Small phases (≤1 hour total), independently verifiable
- ✅ **Poka-yoke**: Quality gates built into every phase, documentation prevents mistakes
- ✅ **Andon**: Progress visible (test output), failures signaled immediately
- ✅ **Jidoka**: Stop-the-line quality (all gates must pass before proceeding)

Target: 99.99966% defect-free delivery (3.4 defects per million opportunities)
Current State: 100% test failure (1 defect in 1 opportunity)
Post-Fix State: 0% test failure (0 defects in 4 opportunities)

**Let's build it right the first time.**
