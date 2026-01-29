# Research: Fix erlmcp_observability_SUITE Ranch/Cowboy dashboard startup failure

**Date**: 2025-01-29
**Item**: 015-fix-erlmcpobservabilitysuite-ranchcowboy-dashboard
**Section**: ct-fixes
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
Observability suite init_per_suite failing - Ranch not started before Cowboy dashboard

**Motivation:** Cannot test observability functionality if dashboard won't start. Ranch supervision tree must be initialized before Cowboy listener.

**Success criteria:**
- Root cause documented (Ranch not started)
- init_per_suite/1 fixed to start ranch/cowboy
- Dashboard server starts successfully
- Observability tests execute
- rebar3 ct --suite=erlmcp_observability_SUITE runs

**Technical constraints:**
- Must start ranch before cowboy
- Options: start in init_per_suite, make dashboard optional, or start full erlmcp_observability app

**Signals:** priority: critical, urgency: P0 - BLOCKS INTEGRATION TESTING

### Quality Gate Status
- **Gate Type**: Common Test Suite
- **Current State**: 0/4 tests passing (100% failure rate)
- **Target State**: 4/4 tests passing (100% pass rate)
- **Gap**: 100% test failure - ALL tests blocked by init_per_suite failure

## Summary

**Manufacturing Objective:** Fix the critical test suite startup failure in `erlmcp_observability_SUITE` that is preventing ALL observability integration tests from executing. The root cause is a missing dependency startup sequence: Ranch application supervision tree is not started before Cowboy attempts to create the dashboard listener, causing a `noproc` error when Cowboy tries to call `ranch_sup`.

**Technical Approach:** The fix requires explicitly starting Ranch before Cowboy in the test suite's `init_per_suite/1` function. This follows the established pattern used in `erlmcp_transport_integration_SUITE.erl:36-39` where dependencies are started in the correct order: gproc → ranch → gun → ssl → application_under_test. The dashboard server (`erlmcp_dashboard_server.erl:106-110`) calls `cowboy:start_clear/3` which internally requires Ranch's supervisor to be available.

**TCPS Justification:** This is a **Jidoka** violation - the test suite lacks "built-in quality" with proper dependency validation. The `noproc` error indicates a missing Poka-yoke (mistake-proofing) guard that should verify Ranch is started before attempting to use Cowboy. This is a **P0 (Critical)** blocking issue because it prevents 100% of observability integration tests from running, violating the 99.99966% defect-free delivery standard (3.4 defects per million opportunities). The Andon (visible problem signaling) is working correctly - the failure is immediate and visible - but the root cause fix is missing from the startup sequence.

## Current State Analysis

### Existing Implementation

**Files:**
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl:1-87` - Failing test suite
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_dashboard_server.erl:1-297` - Dashboard server using Cowboy
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability_sup.erl:1-139` - Supervisor starting dashboard server
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability_app.erl:1-84` - Application callback
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability.app.src:1-41` - Application metadata

**Patterns:**
- OTP application with supervision tree (one_for_one strategy)
- gen_server behavior for dashboard server
- Cowboy HTTP server with WebSocket support
- Common Test suite for integration testing

**Tests:**
- Current Coverage: 0% (all tests blocked)
- Test Count: 4 test cases
- Pass Rate: 0% (0/4 passing)
- Failure Mode: init_per_suite failure with noproc error

**Quality:**
- Compilation: ✅ Pass (0 errors)
- EUnit: Not run (blocked by CT failure)
- Common Test: ❌ 100% failure (0/4 tests)
- Coverage: 0% (no tests executed)
- Dialyzer: Not checked (blocked by test failure)
- Xref: Not checked (blocked by test failure)

### Key Files

**Test Suite (FAILING):**
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl:27-30` - init_per_suite missing Ranch startup
  ```erlang
  init_per_suite(Config) ->
      {ok, _} = application:ensure_all_started(erlmcp_observability),  %% Missing ranch!
      ct:log("Observability application started"),
      Config.
  ```

**Dashboard Server (Requires Ranch):**
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_dashboard_server.erl:92-121` - init/1 starts Cowboy listener
  ```erlang
  init([Port]) ->
      ?LOG_INFO("Starting dashboard server on port ~p", [Port]),
      Dispatch = cowboy_router:compile([...]),
      {ok, ListenerPid} = cowboy:start_clear(  %% Requires Ranch!
          erlmcp_dashboard_listener,
          [{port, Port}],
          #{env => #{dispatch => Dispatch}}
      ),
      ...
  ```

**Supervisor (Starts Dashboard):**
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability_sup.erl:88-96` - Child spec for dashboard
  ```erlang
  #{
      id => erlmcp_dashboard_server,
      start => {erlmcp_dashboard_server, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [erlmcp_dashboard_server]
  }
  ```

**Application Metadata:**
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability.app.src:9-16` - Applications list (missing cowboy, ranch)
  ```erlang
  {applications, [
      kernel,
      stdlib,
      opentelemetry_api,
      opentelemetry,
      opentelemetry_exporter,
      erlmcp_core
  ]},
  ```

**Root Config:**
- `/Users/sac/erlmcp/rebar.config:49-52` - Dependencies declared correctly
  ```erlang
  {deps, [
      ...
      {ranch, "2.1.0"},
      ...
      {cowboy, "2.10.0"},
      ...
  ]}.
  ```

**Working Pattern (Reference):**
- `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_integration_SUITE.erl:34-44` - Correct dependency startup
  ```erlang
  init_per_suite(Config) ->
      %% Start dependencies
      {ok, _} = application:ensure_all_started(gproc),
      {ok, _} = application:ensure_all_started(ranch),  %% ✅ Started BEFORE cowboy
      {ok, _} = application:ensure_all_started(gun),
      {ok, _} = application:ensure_all_started(ssl),
      %% Start transport application
      {ok, _} = application:ensure_all_started(erlmcp_transports),
      [{app_started, true} | Config].
  ```

### OTP Patterns Observed

**Behavior:**
- `erlmcp_dashboard_server`: gen_server (lines 16-17 in erlmcp_dashboard_server.erl)
- `erlmcp_observability_sup`: supervisor (lines 17-18 in erlmcp_observability_sup.erl)
- `erlmcp_observability_app`: application (lines 10-11 in erlmcp_observability_app.erl)

**Supervision:**
- Strategy: one_for_one (erlmcp_observability_sup.erl:50-54)
- Intensity: 10 restarts per 60 seconds
- Child restart: permanent for all workers
- Dashboard server is a permanent worker under supervision

**Process Pattern:**
- gen_server for dashboard management
- Cowboy listener with WebSocket handler
- Process registry: local registration via {local, ?MODULE}
- Monitoring: WebSocket processes monitored via erlang:monitor/2

**Test Pattern:**
- Chicago School TDD (real processes, no mocks)
- Common Test suite with init_per_suite/end_per_suite
- Tests start individual processes (erlmcp_metrics, erlmcp_health_monitor)
- Tests use real OTP application startup via application:ensure_all_started

## Technical Considerations

### Dependencies

**Internal Modules:**
- `erlmcp_metrics` - Metrics collection server
- `erlmcp_metrics_server` - HTTP metrics endpoint
- `erlmcp_metrics_aggregator` - Time-series aggregation
- `erlmcp_health_monitor` - Component health tracking
- `erlmcp_otel` - OpenTelemetry integration
- `erlmcp_dashboard_server` - Dashboard HTTP/WebSocket server (REQUIRES RANCH)

**External Libraries:**
- `cowboy 2.10.0` - HTTP server (depends on ranch)
- `ranch 2.1.0` - TCP connection supervisor (REQUIRED by cowboy)
- `opentelemetry_api 1.5.0` - OTEL API
- `opentelemetry 1.7.0` - OTEL SDK
- `opentelemetry_exporter 1.10.0` - OTEL exporters
- `jsx 3.1.0` - JSON encoding (via erlmcp_core)

**OTP Applications:**
- kernel, stdlib - OTP core
- crypto - Cryptographic functions
- ssl - TLS/SSL support
- sasl - System architecture support libraries

### TCPS Quality Gates to Pass

- [ ] **Compilation**: 0 errors ✅ CURRENTLY PASSING
- [ ] **EUnit**: 100% pass rate (not run - blocked by CT)
- [ ] **Common Test**: 100% pass rate ❌ CURRENTLY FAILING (0/4)
- [ ] **Coverage**: ≥80% ❌ CURRENTLY 0% (no tests executed)
- [ ] **Dialyzer**: 0 warnings (not checked - blocked by test failure)
- [ ] **Xref**: 0 undefined function calls (not checked - blocked by test failure)
- [ ] **Performance**: <10% regression from baseline (not applicable - no perf code changed)

### Patterns to Follow

**Gen Server Pattern:**
- Reference: `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_dashboard_server.erl:16-194`
- Pattern: gen_server with start_link/0, handle_call/3, handle_cast/2, handle_info/2
- State record: #state{port, listener_pid, websocket_pids, metrics_timer}

**Test Pattern:**
- Reference: `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_integration_SUITE.erl:34-50`
- Pattern: Start dependencies in order → start application → run tests → cleanup
- Critical: Start ranch BEFORE cowboy

**Error Handling:**
- Pattern: Use application:ensure_all_started/1 which returns {ok, StartedApps} or {error, Reason}
- Reference: All CT suites use this pattern for dependency startup
- The current test suite has this pattern but is missing the ranch dependency

**Type Specs:**
- Pattern: Dialyzer specs present in all modules
- Example: `-spec start_link() -> {ok, pid()} | {error, term()}.`
- All gen_server callbacks have proper type specs

## Root Cause Analysis (5 Whys)

**Problem:** erlmcp_observability_SUITE init_per_suite fails with `{noproc, {gen_server,call, [ranch_sup, ...]}}` error, blocking all 4 test cases from executing.

1. **Why?** Cowboy's `start_clear/3` function calls `ranch_sup` to start a listener, but Ranch's supervisor process is not running.
   - **Evidence:** Log lines 23-26 in suite.log show `{noproc, {gen_server,call, [ranch_sup, ...]}}`

2. **Why?** The test suite's `init_per_suite/1` only calls `application:ensure_all_started(erlmcp_observability)` but does not explicitly start the Ranch application first.
   - **Evidence:** erlmcp_observability_SUITE.erl:27-30 shows only one application:start call

3. **Why?** The `erlmcp_observability.app.src` file does not list `cowboy` or `ranch` in its `{applications, [...]}` list, so `application:ensure_all_started(erlmcp_observability)` does not automatically start them.
   - **Evidence:** erlmcp_observability.app.src:9-16 shows applications list missing cowboy and ranch

4. **Why?** The application metadata was designed assuming Cowboy/Ranch would be started by the release or by the test suite explicitly, following OTP best practices where applications should declare only direct dependencies (kernel, stdlib), not transitive dependencies.
   - **Evidence:** Cowboy and Ranch are declared in rebar.config:49-52 as top-level dependencies

5. **ROOT CAUSE:** The test suite startup sequence is incomplete. Unlike `erlmcp_transport_integration_SUITE.erl` which correctly starts gproc → ranch → gun → ssl before starting the application under test, `erlmcp_observability_SUITE` skips the explicit Ranch startup, causing Cowboy to fail when it tries to use Ranch's supervisor.

   **Solution:** Add `application:ensure_all_started(ranch)` to init_per_suite BEFORE starting erlmcp_observability, following the established pattern in erlmcp_transport_integration_SUITE.erl:37.

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Incomplete dependency chain** - Adding only ranch but missing other cowboy dependencies | P1 | Dashboard server may fail with different error | Start cowboy explicitly after ranch (belt-and-suspenders approach) |
| **Port conflict** - Dashboard tries to bind to port 9090 which may be in use | P2 | Tests fail with eaddrinuse error | Use port 0 (random available port) for testing via application:set_env/3 |
| **Missing priv files** - Dashboard HTML/JS files not in test build | P2 | Cowboy static handler fails to serve files | Ensure priv/dashboard directory exists or disable static file serving in tests |
| **WebSocket test timing** - Tests may complete before WebSocket connects | P3 | Intermittent test failures | Add explicit wait/retry logic or remove WebSocket-dependent tests |
| **Application stop order** - end_per_suite may stop apps in wrong order | P2 | Tests leave processes running | Stop applications in reverse order: erlmcp_observability → cowboy → ranch |

**Severity Definitions:**
- **P0 (Critical):** BLOCKS production - MUST fix immediately
- **P1 (High):** Major quality gap - MUST fix before release
- **P2 (Medium):** Important but not blocking
- **P3 (Low):** Nice-to-have

## Recommended Manufacturing Approach

**TCPS Methodology:**

1. **Specification** - Requirements with acceptance criteria
   - MUST start ranch application before cowboy
   - MUST start erlmcp_observability after cowboy
   - MUST use port 0 (random) for testing to avoid conflicts
   - MUST stop applications in reverse order in end_per_suite
   - MUST pass all 4 test cases
   - MUST achieve ≥80% code coverage

2. **Pseudocode** - Algorithm design BEFORE coding
   ```
   init_per_suite(Config):
       Start gproc (required by erlmcp_core)
       Start ranch (required by cowboy)
       Start cowboy (required by dashboard)
       Start erlmcp_observability (starts dashboard server)
       Verify dashboard server is running
       Return Config

   end_per_suite(_Config):
       Stop erlmcp_observability
       Stop cowboy
       Stop ranch
       Stop gproc
   ```

3. **Architecture** - Integration points and dependencies
   - **Dependency Chain:** gproc → ranch → cowboy → erlmcp_observability
   - **Process Tree:** ranch_sup → cowboy_listener_sup → erlmcp_dashboard_server
   - **Test Isolation:** Each test case starts its own processes (metrics, health_monitor)
   - **Cleanup:** Reverse order shutdown prevents dangling processes

4. **Refinement** - Chicago School TDD (tests FIRST)
   - Test 1: Verify ranch starts successfully
   - Test 2: Verify cowboy starts after ranch
   - Test 3: Verify erlmcp_observability starts after cowboy
   - Test 4: Verify dashboard listener is accepting connections
   - Test 5: Verify all observability tests pass

5. **Completion** - All quality gates passing
   - Compilation: 0 errors
   - EUnit: 100% pass rate (if any EUnit tests exist)
   - Common Test: 4/4 tests passing (100%)
   - Coverage: ≥80% for erlmcp_observability modules
   - Dialyzer: 0 warnings
   - Xref: 0 undefined function calls

**Implementation Strategy:**

**Phase 1: Fix init_per_suite (Jidoka - stop-the-line validation)**
1. Add `application:ensure_all_started(gproc)` - required by erlmcp_core
2. Add `application:ensure_all_started(ranch)` - REQUIRED before cowboy
3. Add `application:ensure_all_started(cowboy)` - belt-and-suspenders
4. Keep existing `application:ensure_all_started(erlmcp_observability)`
5. Add verification: `cowboy:info() != undefined`

**Phase 2: Fix end_per_suite (Kaizen - continuous improvement)**
1. Stop applications in reverse order
2. Add explicit stop for cowboy and ranch
3. Verify no processes remain (process_info/1 or observer)

**Phase 3: Add Poka-yoke (mistake-proofing)**
1. Add guard clause: check if ranch is started before attempting cowboy
2. Add test case: verify dependency chain is correct
3. Add documentation comment explaining why order matters

**Phase 4: Optimize for Testing (Heijunka - production leveling)**
1. Set dashboard port to 0 (random) for tests
2. Add application:set_env(erlmcp_observability, dashboard_port, 0)
3. Verify port binding succeeds
4. Extract actual port for debug logging

**Quality Validation:**

**Automated:**
```bash
# Run the specific test suite
rebar3 ct --suite=erlmcp_observability_SUITE

# Verify all tests pass
rebar3 ct --verbose | grep "TEST COMPLETE"

# Check coverage
rebar3 cover --verbose | grep erlmcp_observability

# Run dialyzer
rebar3 dialyzer

# Run xref
rebar3 xref
```

**Manual:**
1. Inspect test logs for "Dashboard server started successfully"
2. Verify no `{noproc, {gen_server,call, [ranch_sup, ...]}}` errors
3. Check that port 9090 (or random port) is listening: `netstat -an | grep LISTEN`
4. Verify all 4 test cases show "ok" instead of "auto_skipped"
5. Confirm coverage report shows ≥80% for erlmcp_dashboard_server

**Metrics:**
- **Test Pass Rate:** Target 100% (4/4), Current 0% (0/4)
- **Coverage:** Target ≥80%, Current 0%
- **Startup Time:** <1 second for init_per_suite
- **Memory:** No memory leaks in dashboard server
- **Process Count:** Dashboard server + 1 listener + N WebSocket clients

## Open Questions
**NONE** - All research questions answered. Root cause identified, solution path clear.

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) - Ranch supervision tree not started before Cowboy
- [x] Quality gates defined (specific thresholds) - 100% test pass rate, ≥80% coverage
- [x] OTP patterns understood (behaviors, supervision) - gen_server, supervisor, application
- [x] Test strategy clear (Chicago School TDD) - Real processes, no mocks, proper startup/shutdown
- [x] Risk assessment complete (severity P0-P3) - 5 risks identified with mitigations
- [x] No open questions (all research complete) - All dependencies mapped, solution designed

**TCPS Compliance:**
- ✅ **Jidoka**: Built-in quality checks added (dependency order verification)
- ✅ **Poka-yoke**: Mistake-proofing with explicit startup sequence
- ✅ **Kaizen**: Metrics tracking (test pass rate, coverage, startup time)
- ✅ **Heijunka**: Phased implementation (init_per_suite → end_per_suite → poka-yoke → optimization)
- ✅ **Andon**: Visible failure signaling (test suite will fail immediately if Ranch not started)

**Zero Defect Delivery:**
- Target: 99.99966% defect-free (3.4 defects per million opportunities)
- Current State: 100% test failure (1 defect in 1 opportunity = 100% defective)
- Post-Fix State: 0% test failure (0 defects in 4 opportunities = 0% defective)
- Manufacturing Standard: **ACHIEVED** after fix implementation
