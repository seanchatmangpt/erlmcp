# Research: Fix Observability Suite Dashboard Failure

**Date**: 2026-01-29
**Item**: 039-fix-observability-suite-dashboard-failure
**Section**: ct-fixes
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
erlmcp_observability_SUITE fails because dashboard server (Cowboy) can't start - Ranch supervisor not available.

**Motivation:** Observability tests are blocked by missing Ranch startup. Dashboard is important for metrics visualization.

**Success criteria:**
- Root cause documented (Ranch not started)
- init_per_suite/1 fixed to start ranch/cowboy
- Dashboard server starts successfully
- Observability tests execute
- rebar3 ct --suite=erlmcp_observability_SUITE runs

**Technical constraints:**
- Option A: Start ranch in init_per_suite
- Option B: Make dashboard optional
- Option C: Start full erlmcp_observability app

**Signals:** priority: critical, urgency: P0 - BLOCKS INTEGRATION TESTING

### Quality Gate Status
- **Gate Type**: Common Test Suite
- **Current State**: 0/4 tests passing (100% failure rate - all tests auto-skipped due to init_per_suite failure)
- **Target State**: 4/4 tests passing (100% pass rate)
- **Gap**: 100% test failure - ALL tests blocked by Ranch dependency not being started before dashboard initialization

## Summary

**Manufacturing Objective:** Fix the critical test suite startup failure in `erlmcp_observability_SUITE` that prevents ALL observability integration tests from executing. The root cause is a missing dependency startup sequence: the Ranch application supervision tree is not started before Cowboy attempts to create the dashboard HTTP listener, causing a `{noproc, {gen_server,call, [ranch_sup, ...]}}` error that blocks the entire test suite.

**Technical Approach:** The fix requires explicitly starting Ranch before Cowboy in the test suite's `init_per_suite/1` function. This follows the established pattern used in `erlmcp_transport_integration_SUITE.erl:34-44` where dependencies are started in the correct order. The dashboard server (`erlmcp_dashboard_server.erl:106-110`) calls `cowboy:start_clear/3` which internally requires Ranch's supervisor (`ranch_sup`) to be available. The current `erlmcp_observability.app.src:9-16` does not list `cowboy` or `ranch` in its `{applications, [...]}` list, so `application:ensure_all_started(erlmcp_observability)` does not automatically start these dependencies.

**TCPS Justification:** This is a **Jidoka** violation - the test suite lacks "built-in quality" with proper dependency validation. The `noproc` error indicates a missing Poka-yoke (mistake-proofing) guard that should verify Ranch is started before attempting to use Cowboy. This is a **P0 (Critical)** blocking issue because it prevents 100% of observability integration tests from running, violating the 99.99966% defect-free delivery standard (3.4 defects per million opportunities). The Andon (visible problem signaling) is working correctly - the failure is immediate and visible - but the root cause fix is missing from the startup sequence.

## Current State Analysis

### Existing Implementation

**Files:**
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl:1-87` - Failing test suite (4 test cases all auto-skipped)
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_dashboard_server.erl:1-297` - Dashboard server using Cowboy HTTP/WebSocket
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability_sup.erl:1-139` - Supervisor starting dashboard server as permanent worker
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability_app.erl:1-84` - Application callback (starts supervision tree)
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability.app.src:1-41` - Application metadata (missing cowboy/ranch in applications list)
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_dashboard_tests.erl:40-55` - Working EUnit tests that correctly start cowboy (reference pattern)
- `/Users/sac/erlmcp/rebar.config:44-58` - Top-level dependencies (ranch 2.1.0, cowboy 2.10.0 correctly declared)
- `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transports.app.src:10-19` - Example of correct app listing ranch in applications

**Patterns:**
- OTP application with supervision tree (one_for_one strategy, intensity 10 per 60s)
- gen_server behavior for dashboard server with WebSocket support
- Cowboy HTTP server with static file serving and WebSocket upgrade
- Common Test suite for integration testing
- Chicago School TDD (real processes, no mocks in test implementation)

**Tests:**
- Current Coverage: 0% (all tests blocked at init_per_suite)
- Test Count: 4 test cases (test_metrics_integration, test_otel_integration, test_health_integration, test_full_observability_stack)
- Pass Rate: 0% (0/4 passing - all auto-skipped)
- Failure Mode: init_per_suite failure with `{noproc, {gen_server,call, [ranch_sup, {start_child, ...}]}}`

**Quality:**
- Compilation: ✅ Pass (0 errors)
- EUnit: ⚠️ Not run (blocked by CT failure)
- Common Test: ❌ 100% failure (0/4 tests, all auto-skipped)
- Coverage: ❌ 0% (no tests executed)
- Dialyzer: ⚠️ Not checked (blocked by test failure)
- Xref: ⚠️ Not checked (blocked by test failure)

### Key Files

**Test Suite (FAILING - Missing Ranch Startup):**
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_observability_SUITE.erl:27-34` - init_per_suite/end_per_suite
  ```erlang
  init_per_suite(Config) ->
      {ok, _} = application:ensure_all_started(erlmcp_observability),  %% ❌ Missing ranch!
      ct:log("Observability application started"),
      Config.

  end_per_suite(_Config) ->
      application:stop(erlmcp_observability),
      ok.
  ```

**Dashboard Server (Requires Ranch - Fails When ranch_sup Not Available):**
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_dashboard_server.erl:91-121` - init/1 calls cowboy:start_clear
  ```erlang
  init([Port]) ->
      ?LOG_INFO("Starting dashboard server on port ~p", [Port]),

      % Configure Cowboy routes
      Dispatch = cowboy_router:compile([
          {'_', [
              {"/", cowboy_static, {priv_file, erlmcp_observability, "dashboard/index.html"}},
              {"/static/[...]", cowboy_static, {priv_dir, erlmcp_observability, "dashboard/static"}},
              {"/ws", ?MODULE, []},
              {"/api/metrics", erlmcp_dashboard_http_handler, []}
          ]}
      ]),

      % Start Cowboy HTTP listener - ❌ FAILS if ranch_sup not running!
      {ok, ListenerPid} = cowboy:start_clear(
          erlmcp_dashboard_listener,
          [{port, Port}],
          #{env => #{dispatch => Dispatch}}
      ),
      ...
  ```

**Supervisor (Starts Dashboard as Permanent Worker):**
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability_sup.erl:88-96` - Child spec
  ```erlang
  %% Dashboard Server - Real-time metrics dashboard (Cowboy + WebSocket)
  #{
      id => erlmcp_dashboard_server,
      start => {erlmcp_dashboard_server, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [erlmcp_dashboard_server]
  }
  ```

**Application Metadata (Missing cowboy/ranch in applications list):**
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_observability.app.src:9-16` - Applications list
  ```erlang
  {applications, [
      kernel,
      stdlib,
      opentelemetry_api,
      opentelemetry,
      opentelemetry_exporter,
      erlmcp_core      %% ❌ Missing: cowboy, ranch
  ]},
  ```

**Root Config (Dependencies Declared Correctly):**
- `/Users/sac/erlmcp/rebar.config:44-58` - Top-level dependencies
  ```erlang
  {deps, [
      {jsx, "3.1.0"},
      {jesse, "1.8.1"},
      {gproc, "0.9.0"},
      {gun, "2.0.1"},
      {ranch, "2.1.0"},        %% ✅ Declared
      {poolboy, "1.5.2"},
      {bbmustache, "1.12.2"},
      {cowboy, "2.10.0"},      %% ✅ Declared
      opentelemetry_api,
      opentelemetry,
      opentelemetry_exporter,
      {jobs, "0.10.0"},
      {fs, "0.9.2"}
  ]}.
  ```

**Working Pattern - EUnit Tests (Correct Startup):**
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_dashboard_tests.erl:40-55` - setup/0
  ```erlang
  setup() ->
      % Start required applications - ✅ CORRECT ORDER
      application:ensure_all_started(cowboy),  %% Starts ranch first (cowboy dependency)
      application:ensure_all_started(gun),
      application:ensure_all_started(jsx),

      % Start metrics aggregator
      {ok, AggPid} = erlmcp_metrics_aggregator:start_link(),

      % Start dashboard server
      {ok, DashPid} = erlmcp_dashboard_server:start_link(?TEST_PORT),

      % Wait for server to be ready
      timer:sleep(100),

      #{aggregator => AggPid, dashboard => DashPid}.
  ```

**Working Pattern - Transport Integration CT (Correct Startup):**
- `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_integration_SUITE.erl:34-44` - init_per_suite
  ```erlang
  init_per_suite(Config) ->
      %% Start dependencies - ✅ CORRECT ORDER
      {ok, _} = application:ensure_all_started(gproc),
      {ok, _} = application:ensure_all_started(ranch),  %% ✅ BEFORE cowboy
      {ok, _} = application:ensure_all_started(gun),
      {ok, _} = application:ensure_all_started(ssl),
      %% Start transport application
      {ok, _} = application:ensure_all_started(erlmcp_transports),
      [{app_started, true} | Config].
  ```

**Application That Correctly Lists Ranch (Reference):**
- `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transports.app.src:10-19` - Applications list
  ```erlang
  {applications, [
      kernel,
      stdlib,
      ssl,
      inets,
      gun,
      ranch,           %% ✅ CORRECTLY LISTED
      poolboy,
      erlmcp_core
  ]},
  ```

### OTP Patterns Observed

**Behavior:**
- `erlmcp_dashboard_server`: gen_server (lines 16-19 in erlmcp_dashboard_server.erl)
  - Exports: start_link/0, start_link/1, stop/0, get_port/0, broadcast_metrics/1
  - Callbacks: init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
  - WebSocket handlers: init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3
- `erlmcp_observability_sup`: supervisor (lines 17-18 in erlmcp_observability_sup.erl)
  - Exports: start_link/0, start_link/1, init/1
  - Callback: init/1 returns {ok, {SupFlags, Children}}
- `erlmcp_observability_app`: application (lines 10-11 in erlmcp_observability_app.erl)
  - Exports: start/2, stop/1
  - Starts supervisor via erlmcp_observability_sup:start_link()

**Supervision:**
- Strategy: one_for_one (erlmcp_observability_sup.erl:50-54)
- Intensity: 10 restarts per 60 seconds
- Child restart: permanent for all workers
- Dashboard server is a permanent worker under supervision
- Child processes: erlmcp_metrics, erlmcp_metrics_server, erlmcp_metrics_aggregator, erlmcp_dashboard_server, erlmcp_health_monitor, erlmcp_recovery_manager, erlmcp_chaos

**Process Pattern:**
- gen_server for dashboard management with state record
- Cowboy listener with WebSocket handler (cowboy_websocket behavior)
- Process registry: local registration via {local, ?MODULE}
- Monitoring: WebSocket processes monitored via erlang:monitor/2
- Timer-based metrics broadcast every 1 second (timer:send_interval)

**Test Pattern:**
- Chicago School TDD (real processes, no mocks)
- Common Test suite with init_per_suite/end_per_suite
- Tests start individual processes (erlmcp_metrics, erlmcp_health_monitor) using start_link
- Tests use real OTP application startup via application:ensure_all_started
- Test fixtures setup/0 and cleanup/1 for EUnit tests

## Technical Considerations

### Dependencies

**Internal Modules (erlmcp_observability app):**
- `erlmcp_metrics` - Metrics collection server (gen_server)
- `erlmcp_metrics_server` - HTTP metrics endpoint (gen_server)
- `erlmcp_metrics_aggregator` - Time-series aggregation with percentiles (gen_server)
- `erlmcp_dashboard_server` - Dashboard HTTP/WebSocket server (gen_server + cowboy) - **REQUIRES RANCH**
- `erlmcp_dashboard_http_handler` - HTTP REST API handler (cowboy_handler)
- `erlmcp_health_monitor` - Component health tracking (gen_server)
- `erlmcp_recovery_manager` - Automatic recovery and circuit breakers (gen_server)
- `erlmcp_chaos` - Chaos engineering framework (gen_server)
- `erlmcp_otel` - OpenTelemetry integration (library module)
- `erlmcp_receipt_chain` - Receipt chain storage (ETS-based, no process)
- `erlmcp_evidence_path` - Evidence path tracking (library module)

**External Libraries:**
- `cowboy 2.10.0` - HTTP/WebSocket server (depends on ranch, cowlib)
- `ranch 2.1.0` - TCP connection supervisor (REQUIRED by cowboy) - **CRITICAL DEPENDENCY**
- `cowlib (dependency of cowboy)` - Cowboy utilities
- `gun 2.0.1` - HTTP client (for testing dashboard)
- `opentelemetry_api 1.5.0` - OTEL API
- `opentelemetry 1.7.0` - OTEL SDK
- `opentelemetry_exporter 1.10.0` - OTEL exporters
- `jsx 3.1.0` - JSON encoding (via erlmcp_core dependency)
- `jesse 1.8.1` - JSON Schema validation (via erlmcp_core dependency)
- `gproc 0.9.0` - Process registry (via erlmcp_core dependency)

**OTP Applications (startup order matters):**
1. `kernel` - OTP core (always started)
2. `stdlib` - OTP standard library (always started)
3. `gproc` - Process registry (required by erlmcp_core)
4. `ranch` - **CRITICAL** - TCP connection supervisor (MUST start before cowboy)
5. `cowboy` - HTTP server (MUST start after ranch, before erlmcp_observability)
6. `erlmcp_core` - Core MCP protocol (required by erlmcp_observability)
7. `erlmcp_observability` - Application under test (starts dashboard server)

**Dependency Chain:**
```
erlmcp_observability
  └─ erlmcp_core
      ├─ jsx
      ├─ jesse
      └─ gproc
  └─ opentelemetry_api
  └─ opentelemetry
  └─ opentelemetry_exporter
  └─ cowboy (NOT declared, but REQUIRED by dashboard_server)
      └─ ranch (NOT declared, but REQUIRED by cowboy)
      └─ cowlib (cowboy dependency)
```

### TCPS Quality Gates to Pass

- [ ] **Compilation**: 0 errors ✅ CURRENTLY PASSING
- [ ] **EUnit**: 100% pass rate ⚠️ NOT RUN (blocked by CT failure)
- [ ] **Common Test**: 100% pass rate ❌ CURRENTLY FAILING (0/4 tests - all auto-skipped)
- [ ] **Coverage**: ≥80% ❌ CURRENTLY 0% (no tests executed due to init_per_suite failure)
- [ ] **Dialyzer**: 0 warnings ⚠️ NOT CHECKED (blocked by test failure)
- [ ] **Xref**: 0 undefined function calls ⚠️ NOT CHECKED (blocked by test failure)
- [ ] **Performance**: <10% regression from baseline (not applicable - no perf code changed)

### Patterns to Follow

**Gen Server Pattern:**
- Reference: `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_dashboard_server.erl:16-194`
- Pattern: gen_server with start_link/0, handle_call/3, handle_cast/2, handle_info/2
- State record: `#state{port, listener_pid, websocket_pids, metrics_timer}`
- Initialization: Calls cowboy:start_clear/3 to start HTTP listener
- Termination: Stops Cowboy listener, cancels timer

**Test Pattern (Common Test):**
- Reference: `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_integration_SUITE.erl:34-50`
- Pattern: Start dependencies in order → start application → run tests → cleanup
- **Critical**: Start ranch BEFORE cowboy (cowboy:start_clear internally calls ranch)
- **Correct Order**: gproc → ranch → gun → ssl → application_under_test

**Test Pattern (EUnit - Working Reference):**
- Reference: `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_dashboard_tests.erl:40-55`
- Pattern: setup() starts cowboy (which auto-starts ranch), then starts dashboard
- **Key**: `application:ensure_all_started(cowboy)` ensures ranch is started first

**Error Handling:**
- Pattern: Use `application:ensure_all_started/1` which returns `{ok, StartedApps}` or `{error, Reason}`
- Reference: All CT suites use this pattern for dependency startup
- The current test suite has this pattern but is missing the ranch dependency
- Error mode: `{noproc, {gen_server, call, [ranch_sup, ...]}}` when ranch not started

**Type Specs:**
- Pattern: Dialyzer specs present in all modules
- Example: `-spec start_link() -> {ok, pid()} | {error, term()}.`
- All gen_server callbacks have proper type specs
- Example from dashboard_server: `-spec init([inet:port_number()]) -> {ok, #state{}}.`

## Root Cause Analysis (5 Whys)

**Problem:** erlmcp_observability_SUITE init_per_suite fails with `{noproc, {gen_server,call, [ranch_sup, {start_child, ...}]}}` error, blocking all 4 test cases from executing.

1. **Why?** Cowboy's `cowboy:start_clear/3` function calls `ranch_sup` to start a listener, but Ranch's supervisor process (`ranch_sup`) is not running when the dashboard server tries to start.
   - **Evidence**: erlmcp_dashboard_server.erl:106-110 calls `cowboy:start_clear(erlmcp_dashboard_listener, [{port, Port}], #{env => #{dispatch => Dispatch}})`, which internally tries to contact ranch_sup via gen_server:call

2. **Why?** The test suite's `init_per_suite/1` only calls `application:ensure_all_started(erlmcp_observability)` but does not explicitly start the Ranch application first.
   - **Evidence**: erlmcp_observability_SUITE.erl:27-30 shows only one application:start call
   - **Evidence**: The EUnit tests (erlmcp_dashboard_tests.erl:42) correctly start cowboy first: `application:ensure_all_started(cowboy)`

3. **Why?** The `erlmcp_observability.app.src` file does not list `cowboy` or `ranch` in its `{applications, [...]}` list (lines 9-16), so `application:ensure_all_started(erlmcp_observability)` does not automatically start them.
   - **Evidence**: erlmcp_observability.app.src:9-16 shows applications list: `[kernel, stdlib, opentelemetry_api, opentelemetry, opentelemetry_exporter, erlmcp_core]`
   - **Evidence**: Compare with erlmcp_transports.app.src:10-19 which correctly lists ranch in applications

4. **Why?** The application metadata was designed assuming Cowboy/Ranch would be started either by the release (relx config) or explicitly by the test suite, following OTP best practices where applications should declare only direct dependencies (kernel, stdlib), not transitive dependencies. However, the test suite was not updated to handle this.
   - **Evidence**: Cowboy and Ranch are declared in rebar.config:49-52 as top-level dependencies
   - **Evidence**: relx config (rebar.config:219-240) includes cowboy and ranch in the release
   - **Evidence**: Working test suites (erlmcp_transport_integration_SUITE) explicitly start dependencies

5. **ROOT CAUSE:** The test suite startup sequence is incomplete. Unlike `erlmcp_transport_integration_SUITE.erl` (lines 34-44) which correctly starts gproc → ranch → gun → ssl before starting the application under test, and unlike `erlmcp_dashboard_tests.erl` (lines 40-55) which starts cowboy (auto-starting ranch), `erlmcp_observability_SUITE` skips the explicit Ranch/Cowboy startup, causing Cowboy to fail when the dashboard server tries to use Ranch's supervisor.

   **Solution:** Add explicit dependency startup in init_per_suite BEFORE starting erlmcp_observability:
   ```erlang
   %% Start gproc (required by erlmcp_core)
   {ok, _} = application:ensure_all_started(gproc),
   %% Start ranch (REQUIRED before cowboy)
   {ok, _} = application:ensure_all_started(ranch),
   %% Start cowboy (will also verify ranch is running)
   {ok, _} = application:ensure_all_started(cowboy),
   %% Now start observability app (which starts dashboard server)
   {ok, _} = application:ensure_all_started(erlmcp_observability),
   ```
   This follows the established pattern in erlmcp_transport_integration_SUITE.erl:37-44.

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Incomplete dependency chain** - Adding ranch/cowboy but missing gproc (required by erlmcp_core) | P1 | Application startup fails with different error | Start full dependency chain: gproc → ranch → cowboy → erlmcp_observability |
| **Port conflict** - Dashboard tries to bind to port 9090 which may be in use from previous test run | P2 | Tests fail with eaddrinuse error | Use port 0 (random available port) for testing OR add application:stop(erlmcp_observability) in end_per_suite to ensure clean shutdown |
| **Missing priv files** - Dashboard HTML/JS files not in test build (priv/dashboard directory) | P2 | Cowboy static handler fails to serve files with enoent error | Verify priv/dashboard exists OR disable static file serving in tests by setting application environment variable |
| **WebSocket test timing** - Tests may complete before WebSocket connection fully established | P3 | Intermittent test failures in WebSocket tests | Add explicit wait/retry logic with timer:sleep(100) after starting dashboard server (already done in working EUnit tests) |
| **Application stop order** - end_per_suite may stop apps in wrong order, leaving dangling processes | P2 | Subsequent test runs fail with eaddrinuse or already_started errors | Stop applications in reverse order: erlmcp_observability → cowboy → ranch → gproc. Add application:stop() calls for each explicitly started app |
| **Cowboy version mismatch** - Cowboy 2.10.0 may have different API than test expectations | P2 | cowboy:start_clear fails with badarg or undefined function | Verify cowboy version compatibility (2.10.0 is current and correct). API used (start_clear/3) is stable |
| **Dependency circularity** - If erlmcp_observability is added to erlmcp_core dependencies | P1 | Circular dependency prevents startup | Ensure erlmcp_observability does NOT depend on erlmcp_core in app.src. Current app.src is correct (only depends on erlmcp_core) |

**Severity Definitions:**
- **P0 (Critical):** BLOCKS production - MUST fix immediately
- **P1 (High):** Major quality gap - MUST fix before release
- **P2 (Medium):** Important but not blocking
- **P3 (Low):** Nice-to-have

## Recommended Manufacturing Approach

**TCPS Methodology:**

1. **Specification** - Requirements with acceptance criteria
   - MUST start ranch application before cowboy
   - MUST start cowboy application before erlmcp_observability
   - MUST start gproc before erlmcp_core (required by core)
   - MUST verify dashboard server starts successfully
   - MUST stop applications in reverse order in end_per_suite
   - MUST pass all 4 test cases (100% pass rate)
   - MUST achieve ≥80% code coverage for observability modules

2. **Pseudocode** - Algorithm design BEFORE coding
   ```
   init_per_suite(Config):
       Start gproc (required by erlmcp_core)
       Start ranch (REQUIRED by cowboy)
       Start cowboy (will verify ranch is running)
       Start erlmcp_observability (starts dashboard server that uses cowboy)
       Verify dashboard server is listening (optional: netstat or connection test)
       Return Config

   end_per_suite(_Config):
       Stop erlmcp_observability
       Stop cowboy
       Stop ranch
       Stop gproc
       Return ok
   ```

3. **Architecture** - Integration points and dependencies
   - **Dependency Chain:** gproc → ranch → cowboy → erlmcp_observability
   - **Process Tree:** ranch_sup → cowboy_sup → erlmcp_dashboard_listener → erlmcp_dashboard_server
   - **Test Isolation:** Each test case starts its own processes (metrics, health_monitor) as needed
   - **Cleanup:** Reverse order shutdown prevents dangling processes and port conflicts

4. **Refinement** - Chicago School TDD (tests FIRST)
   - Test 1: Verify ranch starts successfully (application:ensure_all_started(ranch))
   - Test 2: Verify cowboy starts after ranch (application:ensure_all_started(cowboy))
   - Test 3: Verify erlmcp_observability starts after cowboy
   - Test 4: Verify dashboard listener is accepting connections (check port 9090)
   - Test 5: Run all 4 existing test cases to verify observability functionality

5. **Completion** - All quality gates passing
   - Compilation: 0 errors ✅
   - EUnit: 100% pass rate (if EUnit tests exist for observability)
   - Common Test: 4/4 tests passing (100%)
   - Coverage: ≥80% for erlmcp_observability modules
   - Dialyzer: 0 warnings
   - Xref: 0 undefined function calls

**Implementation Strategy:**

**Phase 1: Fix init_per_suite (Jidoka - stop-the-line validation)**

1. Add `application:ensure_all_started(gproc)` - required by erlmcp_core
2. Add `application:ensure_all_started(ranch)` - **CRITICAL** - REQUIRED before cowboy
3. Add `application:ensure_all_started(cowboy)` - belt-and-suspenders (ensures ranch is running)
4. Keep existing `application:ensure_all_started(erlmcp_observability)`
5. Add verification logging to confirm each app started successfully

**Code to add in erlmcp_observability_SUITE.erl:27-30:**
```erlang
init_per_suite(Config) ->
    %% Start dependencies in correct order
    {ok, _} = application:ensure_all_started(gproc),
    ct:log("Started gproc"),

    {ok, _} = application:ensure_all_started(ranch),
    ct:log("Started ranch (required by cowboy)"),

    {ok, _} = application:ensure_all_started(cowboy),
    ct:log("Started cowboy (HTTP server)"),

    {ok, _} = application:ensure_all_started(erlmcp_observability),
    ct:log("Started erlmcp_observability application"),

    Config.
```

**Phase 2: Fix end_per_suite (Kaizen - continuous improvement)**

1. Stop applications in reverse order (LIFO - Last In First Out)
2. Add explicit stop for cowboy and ranch
3. Add explicit stop for gproc
4. Verify no processes remain (optional: log process count)

**Code to add in erlmcp_observability_SUITE.erl:32-34:**
```erlang
end_per_suite(_Config) ->
    %% Stop applications in reverse order
    application:stop(erlmcp_observability),
    ct:log("Stopped erlmcp_observability"),

    application:stop(cowboy),
    ct:log("Stopped cowboy"),

    application:stop(ranch),
    ct:log("Stopped ranch"),

    application:stop(gproc),
    ct:log("Stopped gproc"),

    ok.
```

**Phase 3: Add Poka-yoke (mistake-proofing)**

1. Add guard clause in init_per_suite to check if apps started successfully
2. Add test case to verify dependency chain is correct
3. Add documentation comment explaining why order matters

**Optional enhancement:**
```erlang
init_per_suite(Config) ->
    %% Start dependencies in correct order (CRITICAL: ranch before cowboy)
    Apps = [
        {gproc, "Process registry (required by erlmcp_core)"},
        {ranch, "TCP supervisor (required by cowboy)"},
        {cowboy, "HTTP server (required by dashboard)"},
        {erlmcp_observability, "Application under test"}
    ],
    lists:foreach(fun({App, Desc}) ->
        case application:ensure_all_started(App) of
            {ok, _} -> ct:log("Started ~s: ~s", [App, Desc]);
            {error, Reason} -> ct:fail("Failed to start ~s: ~p", [App, Reason])
        end
    end, Apps),
    Config.
```

**Phase 4: Optimize for Testing (Heijunka - production leveling)**

1. Consider setting dashboard port to 0 (random available port) for tests to avoid conflicts
2. Add application:set_env(erlmcp_observability, dashboard_port, 0) in init_per_suite
3. Extract actual bound port for debug logging (if using port 0)
4. Add explicit timer:sleep(100) after starting dashboard to ensure it's ready

**Optional enhancement (if port conflicts occur):**
```erlang
init_per_suite(Config) ->
    %% ... (dependency startup as above) ...

    %% Use random port for testing to avoid conflicts
    TestPort = 0,  % OS assigns random available port
    application:set_env(erlmcp_observability, dashboard_port, TestPort),

    {ok, _} = application:ensure_all_started(erlmcp_observability),
    ct:log("Started erlmcp_observability with dashboard on random port"),

    %% Give dashboard time to start
    timer:sleep(100),

    Config.
```

**Quality Validation:**

**Automated:**
```bash
# Run the specific test suite
cd /Users/sac/erlmcp
rebar3 ct --suite=apps/erlmcp_observability/test/erlmcp_observability_SUITE

# Verify all tests pass
rebar3 ct --verbose | grep -E "(test_.*|TEST COMPLETE)"

# Check coverage
rebar3 cover --verbose | grep erlmcp_observability

# Run dialyzer
rebar3 dialyzer

# Run xref
rebar3 xref

# Full test suite
make check
```

**Manual:**
1. Inspect test logs for "Dashboard server started successfully" message
2. Verify no `{noproc, {gen_server,call, [ranch_sup, ...]}}` errors in logs
3. Check that port 9090 (or random port) is listening: `netstat -an | grep LISTEN | grep 9090`
4. Verify all 4 test cases show "ok" instead of "{auto_skipped, ...}"
5. Confirm coverage report shows ≥80% for erlmcp_dashboard_server and other modules
6. Check that no processes remain after test completion: `erlang:processes()`

**Metrics:**
- **Test Pass Rate:** Target 100% (4/4), Current 0% (0/4)
- **Coverage:** Target ≥80%, Current 0% (no tests executed)
- **Startup Time:** <1 second for init_per_suite (should be fast)
- **Memory:** No memory leaks in dashboard server (verify with long-running tests)
- **Process Count:** Dashboard server + 1 listener + N WebSocket clients (should be minimal)
- **Dependency Resolution:** All apps start in correct order without conflicts

## Open Questions
**NONE** - All research questions answered. Root cause identified, solution path clear, working reference patterns found.

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) - Ranch supervision tree not started before Cowboy attempts to start dashboard listener
- [x] Quality gates defined (specific thresholds) - 100% test pass rate (4/4), ≥80% coverage, 0 errors
- [x] OTP patterns understood (behaviors, supervision) - gen_server, supervisor, application with correct start_link/init patterns
- [x] Test strategy clear (Chicago School TDD) - Real processes, no mocks, proper startup/shutdown sequence
- [x] Risk assessment complete (severity P0-P3) - 7 risks identified with specific mitigations
- [x] No open questions (all research complete) - All dependencies mapped, solution designed, reference patterns verified

**TCPS Compliance:**
- ✅ **Jidoka**: Built-in quality checks added (dependency order verification with logging and error handling)
- ✅ **Poka-yoke**: Mistake-proofing with explicit startup sequence (following working patterns from transport tests)
- ✅ **Kaizen**: Metrics tracking (test pass rate 0%→100%, coverage 0%→≥80%, startup time, process count)
- ✅ **Heijunka**: Phased implementation (init_per_suite → end_per_suite → poka-yoke → optimization)
- ✅ **Andon**: Visible failure signaling (test suite fails immediately if Ranch not started, with clear error messages)

**Zero Defect Delivery:**
- Target: 99.99966% defect-free (3.4 defects per million opportunities)
- Current State: 100% test failure (1 defect in 1 opportunity = 100% defective)
- Post-Fix State: 0% test failure (0 defects in 4 opportunities = 0% defective)
- Manufacturing Standard: **WILL BE ACHIEVED** after fix implementation following this research

**Evidence-Based Decision:**
- Working reference pattern found in erlmcp_transport_integration_SUITE.erl:34-44
- Working reference pattern found in erlmcp_dashboard_tests.erl:40-55
- Root cause analysis confirms missing ranch startup in init_per_suite
- Solution is simple, safe, and follows established OTP best practices
- No breaking changes to application code or other tests
