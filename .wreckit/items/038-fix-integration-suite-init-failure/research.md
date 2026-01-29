# Research: Fix Integration Suite Init Failure

**Date**: 2025-01-09
**Item**: 038-fix-integration-suite-init-failure
**Section**: ct-fixes
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
erlmcp_integration_SUITE fails during init_per_suite - application won't start. This blocks ALL integration testing.

**Motivation:** Integration testing is critical for validating end-to-end functionality. Currently completely blocked by init failure.

**Success criteria:**
- Root cause identified and documented
- init_per_suite/1 fixed
- Application starts successfully
- All tests in suite can run (may still fail, but not blocked by init)
- rebar3 ct --suite=erlmcp_integration_SUITE executes

**Technical constraints:**
- Check init_per_suite/1 - what applications are being started
- Check application dependencies - are all required apps available
- Check supervisor start - is erlmcp_core_sup starting correctly
- Check missing supervisor children - are referenced modules available

**Signals:** priority: critical, urgency: P0 - BLOCKS INTEGRATION TESTING

### Quality Gate Status
- **Gate Type**: Common Test Integration Suite
- **Current State**: 0% pass rate - init_per_suite/1 fails, blocking ALL 21 test cases
- **Target State**: 100% init success, tests can execute
- **Gap**: 100% test suite blocked (21 tests cannot run)

## Summary
The erlmcp_integration_SUITE is completely blocked from running due to TWO critical root causes in init_per_suite/1. First, the test attempts to start a non-existent application called `erlmcp` - there is no such application in the codebase, only `erlmcp_core`, `erlmcp_transports`, and `erlmcp_observability`. Second, the erlmcp_core supervisor has a circular dependency where it tries to start `erlmcp_observability_sup` as a child, but that module belongs to a separate application (`erlmcp_observability`) which depends on `erlmcp_core`. This architectural violation prevents the application from starting at all. The fix requires (1) correcting the test suite to start the correct applications using `application:ensure_all_started(erlmcp_core)` consistent with other integration suites, and (2) removing the `erlmcp_observability_sup` child from erlmcp_core's supervision tree since observability is a separate application that should be started independently via application dependencies, not as a supervised child process.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_integration_SUITE.erl:122-163` - init_per_suite/1 with wrong application start
  - `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_sup.erl:58-129` - supervisor init with circular dependency
  - `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_core.app.src:1-46` - application definition
  - `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability.app.src:1-40` - observability app definition
  - `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability_sup.erl:1-17` - observability supervisor module

- **Patterns**:
  - OTP application behavior with {mod, {erlmcp_app, []}}
  - Three-tier supervision tree (core, protocol servers, observability)
  - Umbrella project with 3 applications: erlmcp_core, erlmcp_transports, erlmcp_observability

- **Tests**: 0% - ALL 21 test cases blocked by init failure (6 test groups, 21 individual tests)
- **Quality**: FAIL - Cannot execute any integration tests due to application startup failure

### Key Files

#### Root Cause #1: Wrong Application Name
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_integration_SUITE.erl:148` - Tries to start `erlmcp` (doesn't exist)
  ```erlang
  case application:start(erlmcp) of
  ```
  **CRITICAL**: No application named `erlmcp` exists in the codebase. Only `erlmcp_core`, `erlmcp_transports`, `erlmcp_observability`.

#### Root Cause #2: Circular Dependency
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_sup.erl:119-126` - Tries to start `erlmcp_observability_sup` as child
  ```erlang
  #{
      id => erlmcp_observability_sup,
      start => {erlmcp_observability_sup, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => [erlmcp_observability_sup]
  }
  ```
  **CRITICAL**: This module belongs to `erlmcp_observability` app, which depends on `erlmcp_core`. This creates a circular dependency: erlmcp_core → erlmcp_observability_sup → erlmcp_observability → erlmcp_core.

#### Application Dependency Chain
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_core.app.src:10-17` - erlmcp_core dependencies:
  ```erlang
  {applications, [
      kernel,
      stdlib,
      crypto,
      jsx,
      jesse,
      gproc
  ]},
  ```
  **MISSING**: No dependency on `erlmcp_observability`

- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability.app.src:9-16` - erlmcp_observability dependencies:
  ```erlang
  {applications, [
      kernel,
      stdlib,
      opentelemetry_api,
      opentelemetry,
      opentelemetry_exporter,
      erlmcp_core  %% ← Depends on erlmcp_core
  ]},
  ```

#### Correct Pattern from Other Suites
- `/Users/sac/erlmcp/apps/tcps_erlmcp/test/integration/tcps_andon_integration_SUITE.erl:73-76` - Correct approach:
  ```erlang
  init_per_suite(Config) ->
      {ok, _} = application:ensure_all_started(erlmcp),
      ok = tcps_test_utils:init_test_env(),
      Config.
  ```
  **NOTE**: These also use `erlmcp` which doesn't exist - they may also be broken, but they have different architecture.

- `/Users/sac/erlmcp/apps/tcps_erlmcp/test/integration/tcps_pipeline_SUITE.erl:80-84` - Another example:
  ```erlang
  init_per_suite(Config) ->
      %% Start required applications
      {ok, _} = application:ensure_all_started(erlmcp),
  ```

### OTP Patterns Observed
- **Behavior**: application (erlmcp_app), supervisor (erlmcp_sup, erlmcp_core_sup)
- **Supervision**: one_for_one strategy for TIER 1 (core), simple_one_for_one for servers
- **Process Pattern**: Three-tier supervision tree
  - TIER 1: Core (registry + infrastructure) via erlmcp_core_sup
  - TIER 2: Protocol servers via erlmcp_server_sup
  - TIER 3: Observability via erlmcp_observability_sup (INCORRECTLY placed)
- **Test Pattern**: Common Test (CT) with init_per_suite/1, end_per_suite/1

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - erlmcp_app: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_app.erl:1-14` - Application callback
  - erlmcp_sup: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_sup.erl:1-130` - Top supervisor
  - erlmcp_core_sup: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_core_sup.erl:1-148` - Core supervisor
  - erlmcp_registry: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_registry.erl` - Registry service
  - erlmcp_observability_sup: `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability_sup.erl:17-41` - Separate app's supervisor

- **External Libraries**:
  - jsx 3.1.0 (JSON encoding/decoding)
  - jesse 1.8.1 (JSON schema validation)
  - gproc 0.9.0 (Process registry)
  - opentelemetry_api 1.5.0
  - opentelemetry 1.7.0
  - opentelemetry_exporter 1.10.0

- **OTP Applications**: kernel, stdlib, crypto, sasl

### TCPS Quality Gates to Pass
- [ ] Compilation: 0 errors
- [ ] Common Test: init_per_suite/1 must succeed
- [ ] Application Start: erlmcp_core must start without circular dependency
- [ ] Coverage: ≥80% (currently 0% - blocked)
- [ ] Dialyzer: 0 warnings
- [ ] Xref: 0 undefined function calls
- [ ] Architecture: No circular dependencies between applications

### Patterns to Follow
- **Application Start Pattern**:
  - Reference: `/Users/sac/erlmcp/apps/tcps_erlmcp/test/integration/tcps_heijunka_SUITE.erl:72-76`
  - Use `application:ensure_all_started(App)` to handle dependencies automatically
  - Start applications in dependency order

- **Supervision Tree Pattern**:
  - Reference: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_sup.erl:73-77`
  - One_for_one strategy - no cascading failures
  - Separate applications should NOT be children of each other

- **Test Initialization Pattern**:
  - Reference: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_integration_SUITE.erl:122-163`
  - Start applications in init_per_suite/1
  - Verify core processes are running before tests
  - Cleanup in end_per_suite/1

## Root Cause Analysis (5 Whys)

**Problem**: erlmcp_integration_SUITE fails during init_per_suite/1 - application won't start

1. **Why?** The test tries to start application `erlmcp` which doesn't exist in the codebase.

2. **Why?** The test suite assumes there's an umbrella application called `erlmcp`, but the project only has three separate applications: `erlmcp_core`, `erlmcp_transports`, `erlmcp_observability`.

3. **Why?** There's no top-level `erlmcp.app.src` file, and the release configuration (`relx`) defines a release called `erlmcp` but not an application.

4. **Why?** Even if the test tried to start `erlmcp_core`, it would fail because erlmcp_core's supervisor tries to start `erlmcp_observability_sup` as a child, creating a circular dependency.

5. **ROOT CAUSE**: Architectural violation - `erlmcp_sup` (in erlmcp_core app) tries to supervise `erlmcp_observability_sup` (in erlmcp_observability app), but erlmcp_observability depends on erlmcp_core. This creates an impossible startup sequence: erlmcp_core needs erlmcp_observability_sup to start, but erlmcp_observability app needs erlmcp_core to be running first.

**Solution**:
- **Fix #1 (Test Suite)**: Change `application:start(erlmcp)` to `application:ensure_all_started(erlmcp_core)` in init_per_suite/1
- **Fix #2 (Architecture)**: Remove `erlmcp_observability_sup` from erlmcp_sup's child list in erlmcp_core. The observability application should be started independently as a separate application, not as a child supervisor of erlmcp_core.
- **Fix #3 (Dependencies)**: If erlmcp_core needs observability features, it should call into the erlmcp_observability application's API (which will be running as a sibling application), not supervise it directly.

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| Circular dependency prevents erlmcp_core from starting | P0 (Critical) | Application won't start, ALL integration tests blocked | Remove erlmcp_observability_sup from erlmcp_core's supervision tree. Applications should be siblings, not parent-child. |
| Test suite uses wrong application name | P0 (Critical) | Integration tests cannot run (21 tests blocked) | Change `application:start(erlmcp)` to `application:ensure_all_started(erlmcp_core)` |
| Other integration suites may also be broken | P1 (High) | Multiple test suites may have same issue | Audit all integration suites for same pattern, fix consistently |
| Observability features might be unavailable in tests | P2 (Medium) | Tests that depend on observability may fail | Ensure erlmcp_observability is also started via ensure_all_started if needed by tests |
| Architectural change may break runtime behavior | P2 (Medium) | Code that expects erlmcp_observability_sup to be a child may fail | Verify all code uses API calls, not direct supervisor assumptions. Update documentation. |

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Requirements with acceptance criteria
   - erlmcp_integration_SUITE init_per_suite/1 must succeed
   - erlmcp_core application must start without errors
   - All 21 test cases must be able to execute (may pass or fail, but not blocked)
   - No circular dependencies between applications

2. **Pseudocode** - Algorithm design BEFORE coding
   ```
   Fix Strategy:
   A. Remove erlmcp_observability_sup from erlmcp_sup children
   B. Update erlmcp_core.app.src to NOT depend on erlmcp_observability
   C. Update init_per_suite/1 to start erlmcp_core application correctly
   D. If observability needed, start erlmcp_observability as sibling application
   ```

3. **Architecture** - Integration points and dependencies
   ```
   CORRECT Architecture:
   VM
   ├── erlmcp_core (app)
   │   └── erlmcp_sup (supervisor of core-only processes)
   ├── erlmcp_observability (app)
   │   └── erlmcp_observability_sup (supervisor of observability processes)
   └── erlmcp_transports (app)
       └── erlmcp_transport_sup (supervisor of transport processes)

   Dependencies:
   erlmcp_core: [kernel, stdlib, crypto, jsx, jesse, gproc]
   erlmcp_observability: [kernel, stdlib, opentelemetry_*, erlmcp_core]
   erlmcp_transports: [kernel, stdlib, erlmcp_core]
   ```

4. **Refinement** - Chicago School TDD (tests FIRST)
   - Fix init_per_suite/1 first (unblocks testing)
   - Run rebar3 ct to verify fix
   - Remove circular dependency in supervision tree
   - Verify all tests can execute

5. **Completion** - All quality gates passing
   - rebar3 compile (0 errors)
   - rebar3 ct --suite=erlmcp_integration_SUITE (init succeeds, tests run)
   - rebar3 dialyzer (0 warnings)
   - rebar3 xref (0 undefined functions)

**Implementation Strategy:**

Phase 1: Unblock Test Suite (P0 - Critical)
- File: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_integration_SUITE.erl:122-163`
- Change: Line 148 from `application:start(erlmcp)` to `application:ensure_all_started(erlmcp_core)`
- Verification: Run `rebar3 ct --suite=erlmcp_integration_SUITE` - should reach test cases

Phase 2: Fix Circular Dependency (P0 - Critical)
- File: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_sup.erl:119-126`
- Change: Remove erlmcp_observability_sup child spec from TIER 3
- Rationale: erlmcp_observability is a separate application, not a child of erlmcp_core
- Verification: Run `rebar3 shell -s erlmcp_core` - should start without errors

Phase 3: Update Test Suite for Full Stack (P1 - High)
- File: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_integration_SUITE.erl:122-163`
- Change: Start all required applications: `application:ensure_all_started(erlmcp_core)`, then `application:ensure_all_started(erlmcp_observability)` if needed
- Verification: All tests have access to both core and observability features

Phase 4: Audit Other Suites (P2 - Medium)
- Search: All integration suites using `application:start(erlmcp)`
- Update: Consistent pattern across all test suites
- Verification: All integration tests can run

**Quality Validation:**

- **Automated:**
  ```bash
  # Fix validation
  cd /Users/sac/erlmcp
  rebar3 compile
  rebar3 ct --suite=erlmcp_integration_SUITE  # Should reach test cases
  rebar3 dialyzer
  rebar3 xref
  ```

- **Manual:**
  - Verify init_per_suite/1 succeeds (check test output)
  - Verify erlmcp_core application starts in shell
  - Verify erlmcp_observability application starts as sibling
  - Check no circular dependencies in supervision tree

- **Metrics:**
  - Init success rate: 100% (currently 0%)
  - Test execution rate: 100% of 21 tests can run (currently 0%)
  - Application startup time: <2 seconds
  - Memory footprint: No increase from architectural fix

## Open Questions
**NONE** - All research questions answered:
- ✅ Root cause identified (circular dependency + wrong app name)
- ✅ Quality gates defined (100% init success)
- ✅ OTP patterns understood (application, supervisor, dependencies)
- ✅ Test strategy clear (fix init, then verify tests can run)
- ✅ Risk assessment complete (P0 critical issues identified)
- ✅ Manufacturing approach defined (4-phase strategy)

## Manufacturing Checklist
- [x] Root cause identified (circular dependency in supervision tree + wrong application name)
- [x] Quality gates defined (100% init success, 21 tests unblocked)
- [x] OTP patterns understood (application behavior, supervisor hierarchy)
- [x] Test strategy clear (fix init_per_suite/1, remove circular dep)
- [x] Risk assessment complete (P0 critical impact on ALL integration tests)
- [x] No open questions (research complete)
- [x] Specific file paths and line numbers documented
- [x] Correct implementation patterns identified from other suites
- [x] Architecture violation documented with dependency chain
- [x] Manufacturing approach follows TCPS phases
