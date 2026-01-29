# Create EUnit test suite for TCP transport (erlmcp_transport_tcp) Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Achieve ≥80% code coverage on `erlmcp_transport_tcp.erl` (613 lines) with 100% test pass rate. The TCP transport is the PRODUCTION transport for long-lived connections, server deployments, and high-throughput scenarios using Ranch for connection management. Currently, despite having a comprehensive 700-line test file, coverage is reported as 0%, indicating either test failures, Ranch 2.x API compatibility issues, or coverage measurement problems.

### Quality Gate Requirements

- **Compilation**: 0 errors (MANDATORY)
- **EUnit**: 100% pass rate (MANDATORY)
- **Coverage**: ≥80% (MANDATORY)
- **Dialyzer**: 0 warnings (MANDATORY)
- **Xref**: 0 undefined function calls (MANDATORY)

## Current State

### What Exists Now

**Implementation Files:**
- `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl` (613 lines)
  - Dual behavior: gen_server + ranch_protocol
  - Client mode: outbound connections with reconnection logic
  - Server mode: Ranch listener for accepting connections
  - Lines 164-196: Dual init paths (protocol handler vs standalone)
  - Lines 238-295: handle_info/2 for TCP messages, errors, reconnection
  - Lines 482-547: Connection management and reconnection

**Test File:**
- `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl` (700 lines) - EXISTS
  - Lines 27-70: Test fixtures (setup_client, setup_server, cleanup)
  - Lines 76-150: Client mode tests (4 tests)
  - Lines 156-212: Server mode tests (3 tests)
  - Lines 214-252: Ranch integration test
  - Lines 258-324: Client-server integration test
  - Lines 330-383: Buffer management tests (5 tests)
  - Lines 389-478: Transport behavior tests (6 tests)
  - Lines 480-549: Reconnection tests (3 tests)
  - Lines 555-605: TCP error handling test
  - Lines 611-656: Concurrency test (5 simultaneous clients)
  - Lines 662-699: Ranch protocol handler test

**Dependencies:**
- ranch 2.1.0 (rebar.config line 49)
- kernel, stdlib (OTP applications)
- erlmcp_core (internal)

### What's Missing

**Gap:** 80 percentage points (0% → 80% coverage)

**Root Cause:** The test suite exists but may have:
1. Ranch 2.x API compatibility issues (ranch:get_status returns 'running' atom, not {ok, listening})
2. Test failures preventing coverage measurement
3. Socket timeout issues (timeouts too short at 100ms)
4. Untested code paths (Ranch protocol handler init, server-side error handling, owner death monitoring)
5. Coverage measurement configuration problems

**Impact:** BLOCKS PRODUCTION DEPLOYMENT - TCP transport is critical for:
- Long-lived connections in production
- Server deployments with Ranch
- High-throughput scenarios
- Multi-client concurrent connections

### Key Discoveries from Research

1. **Test file is comprehensive** (700 lines, 25+ test functions) but coverage is 0%
2. **Ranch 2.x API changes**: Tests handle both 'running' atom and {ok, listening} tuple (lines 234, 467, 674)
3. **Socket timing**: Tests use 100-200ms sleeps which may be too short for reliable connection establishment
4. **Untested paths** (identified from code analysis):
   - Ranch protocol handler init/1 (lines 164-190) - may not be covered
   - Server-side tcp_closed/tcp_error (lines 257-270) - error handling paths
   - Client reconnection exponential backoff (lines 549-554) - calculation logic
   - Owner death monitoring (lines 280-283) - process monitoring
   - Socket process death (lines 285-292) - EXIT signal handling
   - Max reconnection attempts enforcement (lines 483-487)
   - Concurrent Ranch listener restart scenarios

## Desired End State

### Specification

**Objective:** Achieve ≥80% code coverage on `erlmcp_transport_tcp.erl` with 100% test pass rate.

**Requirements:**
1. All existing 25+ test functions must pass
2. Ranch 2.x API compatibility verified
3. Socket timeouts adjusted for reliability (100ms → 500ms where needed)
4. Additional tests for uncovered code paths
5. Real TCP sockets (no mocks - Chicago School TDD)
6. Real Ranch listeners (no mocks)
7. Concurrent connection testing (10+ simultaneous clients)
8. All error paths tested (tcp_closed, tcp_error, owner death, socket death)

**Files:**
- `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl` (MODIFY - fix and enhance)
- `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl` (VERIFY - no changes expected)

### Verification

**Automated:**
```bash
cd /Users/sac/erlmcp/apps/erlmcp_transports
rebar3 compile                              # Must pass: 0 errors
rebar3 eunit --module=erlmcp_transport_tcp_tests  # Must pass: 100%
rebar3 cover --module=erlmcp_transport_tcp        # Must pass: ≥80%
rebar3 dialyzer                             # Should pass: 0 warnings
rebar3 xref                                 # Should pass: 0 undefined
```

**Manual:**
- Review coverage report for missed edge cases
- Verify all error paths tested
- Verify Ranch listener lifecycle coverage (start, status, stop)
- Verify client reconnection logic coverage (backoff, max attempts, timers)
- Verify concurrent connection handling (10+ clients)

**Metrics:**
- Coverage percentage: ≥80%
- Test pass rate: 100%
- Test functions: 35+ (current 25+, target add 10+)
- Compilation warnings: 0
- Dialyzer warnings: 0
- Xref issues: 0
- Concurrent connections tested: 10+ (current 5)

### Manufacturing Output

- **Code**: Test file enhanced with additional tests
- **Tests**: Fixed existing tests, added new tests for uncovered paths
- **Documentation**: Plan and PRD (this file + PRD)
- **Receipts**: Coverage report, test execution log

## What We're NOT Doing

- **NOT modifying implementation code** (`erlmcp_transport_tcp.erl`) - tests should be fixed, not the implementation
- **NOT adding mocks** - Chicago School TDD mandates real TCP sockets and real Ranch listeners
- **NOT changing the test framework** - EUnit is the standard
- **NOT adding Common Test suites** - EUnit is sufficient for this module
- **NOT testing pool functionality** - pool configuration exists in opts but is not implemented in transport code (out of scope for this task)
- **NOT performance testing** - coverage is the objective, not benchmarks
- **NOT integration testing with erlmcp_core** - unit testing transport in isolation

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** - Requirements with acceptance criteria (DEFINED)
2. **Pseudocode** - Algorithm design BEFORE coding (DEFINED)
3. **Architecture** - Integration points and supervision tree (UNDERSTOOD)
4. **Refinement** - Chicago School TDD (tests FIRST) (APPLIED)
5. **Completion** - All quality gates passing (TO BE VERIFIED)

### Implementation Strategy

**Diagnose → Fix → Enhance → Validate**

This four-phase approach minimizes risk by:
1. **Phase 1 (Diagnose)**: Identify current state - what fails, what's uncovered
2. **Phase 2 (Fix)**: Fix immediate issues - Ranch API, socket timeouts, test failures
3. **Phase 3 (Enhance)**: Add tests for uncovered code paths to reach 80%+
4. **Phase 4 (Validate)**: Verify all quality gates pass

**Why this strategy:**
- Minimizes risk by fixing known issues first
- Allows incremental coverage improvements
- Each phase is independently verifiable
- Prevents "big bang" changes that are hard to debug

### Quality Integration

- **Pre-commit Hooks**: None (relying on developer discipline and CI)
- **CI Gates**: GitHub Actions will run compilation, EUnit, coverage, Dialyzer, Xref
- **Receipt Generation**: Coverage reports in `_build/test/cover/`
- **Andon Signaling**: Test failures are visible immediately (EUnit verbose mode)

---

## Phases

### Phase 1: Diagnose Current State

**Estimated Time:** 1 hour
**Priority:** P0 (CRITICAL) - Blocks all other work

#### Overview

Identify current test failures, Ranch API issues, and coverage gaps. This phase diagnoses the ROOT CAUSE of the 0% coverage despite having 700 lines of tests.

#### Specification

**WHAT we're diagnosing:**
- Current test pass/fail status
- Ranch 2.x API compatibility issues
- Socket timeout problems
- Coverage measurement configuration
- Specific uncovered lines and branches

#### Pseudocode

```
1. Run rebar3 compile in apps/erlmcp_transports
   - Document any compilation errors

2. Run rebar3 eunit --module=erlmcp_transport_tcp_tests
   - Document failing tests
   - Document failure reasons

3. Run rebar3 cover --module=erlmcp_transport_tcp
   - Document current coverage percentage
   - Document uncovered lines

4. Analyze failures
   - Identify Ranch API compatibility issues
   - Identify socket timeout issues
   - Identify missing test paths

5. Create diagnosis report with:
   - Current coverage %
   - List of failing tests
   - Root cause of 0% coverage
   - Recommendations for fixes
```

#### Architecture

**DIAGNOSIS TOOLS:**
- rebar3 compile (compilation validation)
- rebar3 eunit (test execution)
- rebar3 cover (coverage measurement)
- Manual code review (identify Ranch API calls)

#### Changes Required:

**None** - This is a diagnostic phase only.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `cd /Users/sac/erlmcp/apps/erlmcp_transports && TERM=dumb rebar3 compile` - DOCUMENT errors
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_tcp_tests` - DOCUMENT failures
- [ ] Coverage: `rebar3 cover --module=erlmcp_transport_tcp` - DOCUMENT current % and uncovered lines
- [ ] Dialyzer: `rebar3 dialyzer` - DOCUMENT warnings (if any)

##### Manual Verification:
- [ ] Diagnosis report created with current state
- [ ] Root cause identified (Ranch API, socket timeouts, or missing tests)
- [ ] Uncovered lines documented
- [ ] Phase 2 requirements defined (what to fix first)

**Note**: This phase produces a DIAGNOSIS REPORT, not code changes. The report guides Phase 2 fixes.

---

### Phase 2: Fix Immediate Issues

**Estimated Time:** 2-4 hours
**Priority:** P0 (CRITICAL) - Blocks coverage improvement

#### Overview

Fix Ranch 2.x API compatibility issues, socket timeout problems, and any test failures that prevent coverage measurement. This phase ensures all existing tests pass before adding new ones.

#### Specification

**WHAT we're fixing:**
1. Ranch 2.x API compatibility (ranch:get_status return value)
2. Socket timeouts (increase from 100ms to 500ms where needed)
3. Any compilation errors in test file
4. Any runtime test failures
5. Coverage measurement configuration

#### Pseudocode

```
1. Fix Ranch API compatibility
   IF ranch:get_status returns 'running' (not {ok, listening}):
     - Update assertions in lines 234, 467, 674
     - Use pattern matching: Status =:= running orelse Status =:= {ok, listening}

2. Fix socket timeouts
   IF connection tests fail with timeout:
     - Increase timer:sleep(100) to timer:sleep(500)
     - Increase gen_tcp:connect timeout from 1000ms to 5000ms
     - Add explicit waits for Ranch listener startup

3. Fix any compilation errors
   IF test file doesn't compile:
     - Fix missing includes
     - Fix undefined functions
     - Fix type spec mismatches

4. Verify all tests pass
   RUN rebar3 eunit --module=erlmcp_transport_tcp_tests
   EXPECT: 100% pass rate (0 failures)

5. Measure baseline coverage
   RUN rebar3 cover --module=erlmcp_transport_tcp
   DOCUMENT: Current coverage % after fixes
```

#### Architecture

**TEST FIX PATTERN:**
- Real TCP sockets on localhost
- Real Ranch listeners (no mocks)
- State-based assertions (verify #state records)
- Process monitoring (is_process_alive, ranch:get_status)
- Longer timeouts for reliability (500ms instead of 100ms)

#### Changes Required:

##### 1. Test File: erlmcp_transport_tcp_tests.erl

**File**: `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl`

**Current Issues (from diagnosis):**
- Lines 234, 467, 674: Ranch status assertions may fail
- Lines with timer:sleep(100) may be too short
- gen_tcp:connect timeouts may be too aggressive

**Changes**: Fix Ranch API compatibility and timeouts

```erlang
%% BEFORE (line 233-234)
Status = ranch:get_status(RanchRef),
?assertEqual({ok, listening}, Status),

%% AFTER (Ranch 2.x compatibility)
Status = ranch:get_status(RanchRef),
?assert(Status =:= running orelse Status =:= {ok, listening}),

%% BEFORE (line 57 - insufficient wait for server startup)
timer:sleep(100),

%% AFTER (wait longer for Ranch listener)
timer:sleep(500),

%% BEFORE (line 238-240 - connect timeout too short)
{ok, ClientSocket} = gen_tcp:connect("localhost", Port,
                                      [binary, {active, false},
                                       {packet, line}], 1000),

%% AFTER (increase timeout)
{ok, ClientSocket} = gen_tcp:connect("localhost", Port,
                                      [binary, {active, false},
                                       {packet, line}], 5000),
```

**Reason**: Ranch 2.x returns 'running' atom instead of {ok, listening} tuple. Socket connections need more time for reliable test execution.

##### 2. Coverage Configuration (if needed)

**File**: `apps/erlmcp_transports/rebar.config` (CREATE if needed)

**Current**: No app-specific rebar.config

**Changes**: Add coverage configuration (if diagnosis shows it's missing)

```erlang
%% Coverage configuration
{cover_enabled, true}.
{cover_opts, [verbose]}.
{cover_export_enabled, true}.
```

**Reason**: Ensure coverage measurement is properly configured for the app.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_tcp_tests` - 100% pass rate (ALL 25+ tests pass)
- [ ] Coverage: `rebar3 cover --module=erlmcp_transport_tcp` - Document current % (target baseline for Phase 3)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 new warnings
- [ ] Xref: `rebar3 xref` - 0 new undefined calls

##### Manual Verification:
- [ ] All existing tests pass (25+ tests)
- [ ] Ranch listener lifecycle works (start, status, stop)
- [ ] Client-server integration works
- [ ] Socket connections reliable (no intermittent failures)
- [ ] Coverage measurement working (not 0% due to measurement error)

**Note**: If coverage is still <80% after this phase, proceed to Phase 3 to add missing tests. If coverage is ≥80%, this task is complete (skip to Phase 4).

---

### Phase 3: Enhance Coverage to ≥80%

**Estimated Time:** 8-12 hours
**Priority:** P1 (HIGH) - Required for production readiness

#### Overview

Add tests for uncovered code paths identified in Phase 1 diagnosis to achieve ≥80% coverage. Focus on Ranch protocol handler lifecycle, server-side error handling, client reconnection logic, owner death monitoring, and concurrent connection edge cases.

#### Specification

**WHAT we're adding:**
1. Ranch protocol handler init/1 path test (lines 164-190)
2. Server-side tcp_closed/tcp_error tests (lines 257-270)
3. Client reconnection backoff calculation test (lines 549-554)
4. Max reconnection attempts enforcement test (lines 483-487)
5. Owner death monitoring test (lines 280-283)
6. Socket process death test (lines 285-292)
7. Concurrent connection test with 10+ clients (enhance existing 5-client test)
8. Ranch listener restart test (restart listener with active connections)
9. Socket edge cases (timeout during connect, close during send)
10. Buffer edge cases (incomplete message, oversized message, empty message)

#### Pseudocode

```
1. Analyze coverage report from Phase 2
   IDENTIFY uncovered lines:
   - Ranch protocol handler init (lines 164-190)
   - Server-side error handling (lines 257-270)
   - Client reconnection logic (lines 549-554)
   - Owner monitoring (lines 280-283)
   - Socket death handling (lines 285-292)

2. For each uncovered path, ADD test:

   IF Ranch protocol handler init is uncovered:
     ADD test_ranch_protocol_handler_init()
     - Start Ranch server
     - Connect raw TCP client
     - Verify ranch:handshake/1 called
     - Verify inet:setopts for active mode
     - Verify owner notified with {transport_connected, HandlerPid}

   IF server-side tcp_closed is uncovered:
     ADD test_server_tcp_closed()
     - Start Ranch server
     - Connect client
     - Get server handler pid
     - Close client socket
     - Verify handler stops with {stop, normal, State}

   IF server-side tcp_error is uncovered:
     ADD test_server_tcp_error()
     - Start Ranch server
     - Connect client
     - Get server handler pid
     - Kill socket process
     - Verify handler stops with {stop, {tcp_error, Reason}, State}

   IF client reconnection backoff is uncovered:
     ADD test_client_reconnection_backoff()
     - Start client with max_reconnect_attempts = 3
     - Prevent server from starting (use unavailable port)
     - Verify exponential backoff: 1s, 2s, 4s (with jitter)
     - Verify max delay cap: 60s
     - Verify stops after 3 attempts

   IF owner death monitoring is uncovered:
     ADD test_owner_death_monitoring()
     - Start client process with Owner = self()
     - Verify monitor(process, Owner) called
     - Kill owner process
     - Verify transport stops with {owner_died, Reason}

   IF socket process death is uncovered:
     ADD test_socket_process_death()
     - Start connected client
     - Get socket pid
     - Kill socket process
     - Verify {'EXIT', Socket, Reason} handled
     - Verify client schedules reconnection

   IF concurrent connections need enhancement:
     ENHANCE multiple_clients_test()
     - Change NumClients from 5 to 10
     - Verify all 10 clients connect
     - Verify all 10 clients can send messages
     - Verify server handles all 10 connections

   IF Ranch listener restart is uncovered:
     ADD test_ranch_listener_restart()
     - Start Ranch server
     - Connect 5 clients
     - Stop Ranch listener
     - Restart Ranch listener on same port
     - Verify new connections accepted
     - Verify old clients disconnected

   IF socket edge cases are uncovered:
     ADD test_socket_timeout_during_connect()
     - Start client with connect_timeout = 1ms
     - Point to non-routable IP (e.g., 10.255.255.1)
     - Verify {error, timeout} or {error, etimedout}
     - Verify reconnection scheduled

     ADD test_socket_close_during_send()
     - Start client connected to server
     - Close server socket
     - Client attempts send
     - Verify {error, not_connected} or {error, {tcp_send_failed, Reason}}
     - Verify reconnection scheduled

3. Run all tests
   EXECUTE rebar3 eunit --module=erlmcp_transport_tcp_tests
   VERIFY: 100% pass rate (all 35+ tests pass)

4. Measure coverage
   EXECUTE rebar3 cover --module=erlmcp_transport_tcp
   VERIFY: ≥80% coverage
```

#### Architecture

**TEST PATTERN:** Chicago School TDD
- Real TCP sockets (gen_tcp:connect to localhost)
- Real Ranch listeners (ranch:start_listener/6)
- State-based assertions (verify #state records)
- Process monitoring (is_process_alive, ranch:get_status)
- Concurrent testing (spawn multiple clients)
- Integration testing (end-to-end client-server)

**NEW TEST FUNCTIONS:**
1. `ranch_protocol_handler_init_test_()` - Test Ranch handshake and init
2. `server_tcp_closed_test_()` - Test server-side connection close
3. `server_tcp_error_test_()` - Test server-side connection error
4. `client_reconnection_backoff_test_()` - Test exponential backoff with jitter
5. `max_reconnection_attempts_enforcement_test_()` - Test max attempts limit
6. `owner_death_monitoring_test_()` - Test owner process death handling
7. `socket_process_death_test_()` - Test socket EXIT signal handling
8. `ranch_listener_restart_test_()` - Test listener restart with active connections
9. `socket_timeout_during_connect_test_()` - Test connection timeout
10. `socket_close_during_send_test_()` - Test send failure after close

#### Changes Required:

##### 1. Test File: erlmcp_transport_tcp_tests.erl (ENHANCE)

**File**: `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl`

**Current**: 700 lines, 25+ test functions

**Changes**: Add 10+ new test functions (lines 700+)

**New Test 1: Ranch Protocol Handler Init**
```erlang
%%====================================================================
%% Ranch Protocol Handler Lifecycle Tests
%%====================================================================

ranch_protocol_handler_init_test_() ->
    {timeout, 10,
     fun() ->
         application:ensure_all_started(ranch),

         %% Start server
         {ServerPid, _} = setup_server(),
         {ok, ServerState} = gen_server:call(ServerPid, get_state),
         Port = ServerState#state.port,
         RanchRef = ServerState#state.ranch_ref,

         %% Connect raw TCP client
         {ok, ClientSocket} = gen_tcp:connect("localhost", Port,
                                               [binary, {active, false},
                                                {packet, line}], 5000),

         %% Wait for Ranch protocol handler to start
         timer:sleep(200),

         %% Verify owner received connection notification
         Handler = receive
             {transport_connected, H} when is_pid(H) -> H
         after 2000 ->
             error("No connection notification received")
         end,

         ?assert(is_pid(Handler)),
         ?assert(erlang:is_process_alive(Handler)),

         %% Verify handler state (should have socket from ranch:handshake)
         {ok, HandlerState} = gen_server:call(Handler, get_state),
         ?assertEqual(server, HandlerState#state.mode),
         ?assertNotEqual(undefined, HandlerState#state.socket),
         ?assertEqual(true, HandlerState#state.connected),

         %% Cleanup
         gen_tcp:close(ClientSocket),
         cleanup(Handler),
         cleanup(ServerPid),
         timer:sleep(100)
     end}.
```

**New Test 2: Server TCP Closed**
```erlang
server_tcp_closed_test_() ->
    {timeout, 10,
     fun() ->
         application:ensure_all_started(ranch),

         %% Start server
         {ServerPid, _} = setup_server(),
         {ok, ServerState} = gen_server:call(ServerPid, get_state),
         Port = ServerState#state.port,

         %% Connect client
         {ok, ClientSocket} = gen_tcp:connect("localhost", Port,
                                               [binary, {active, false},
                                                {packet, line}], 5000),

         %% Wait for handler to start
         Handler = receive
             {transport_connected, H} -> H
         after 2000 ->
             error("No handler started")
         end,

         %% Close client socket to trigger tcp_closed on server
         gen_tcp:close(ClientSocket),

         %% Verify handler stops with normal reason
         timer:sleep(200),
         ?assertNot(erlang:is_process_alive(Handler)),

         %% Cleanup
         cleanup(ServerPid),
         timer:sleep(100)
     end}.
```

**New Test 3: Server TCP Error**
```erlang
server_tcp_error_test_() ->
    {timeout, 10,
     fun() ->
         application:ensure_all_started(ranch),

         %% Start server
         {ServerPid, _} = setup_server(),
         {ok, ServerState} = gen_server:call(ServerPid, get_state),
         Port = ServerState#state.port,

         %% Connect client
         {ok, ClientSocket} = gen_tcp:connect("localhost", Port,
                                               [binary, {active, false},
                                                {packet, line}], 5000),

         %% Wait for handler to start
         Handler = receive
             {transport_connected, H} -> H
         after 2000 ->
             error("No handler started")
         end,

         %% Cause TCP error by aborting connection
         inet:setopts(ClientSocket, [{active, once}]),
         spawn(fun() ->
                   timer:sleep(100),
                   %% Send malformed data to cause error
                   gen_tcp:close(ClientSocket)
               end),

         %% Handler should stop with tcp_error reason
         timer:sleep(300),
         HandlerAlive = erlang:is_process_alive(Handler),
         ?assertEqual(false, HandlerAlive),

         %% Cleanup
         cleanup(ServerPid),
         timer:sleep(100)
     end}.
```

**New Test 4: Client Reconnection Backoff**
```erlang
client_reconnection_backoff_test_() ->
    {timeout, 15,
     fun() ->
         application:ensure_all_started(ranch),

         %% Start client with max 3 reconnect attempts to unavailable port
         Opts = #{
             mode => client,
             host => "localhost",
             port => 39999,  % Unavailable port
             owner => self(),
             max_reconnect_attempts => 3
         },
         {ok, Pid} = erlmcp_transport_tcp:start_client(Opts),

         %% Wait for initial connection attempt + 3 reconnects
         %% Backoff: ~1s, ~2s, ~4s = ~7s total + overhead
         timer:sleep(9000),

         %% Verify max attempts reached
         {ok, State} = gen_server:call(Pid, get_state),
         ?assert(State#state.reconnect_attempts >= 3),
         ?assertEqual(false, State#state.connected),

         %% Cleanup
         cleanup(Pid)
     end}.
```

**New Test 5: Owner Death Monitoring**
```erlang
owner_death_monitoring_test_() ->
    {timeout, 10,
     fun() ->
         application:ensure_all_started(ranch),

         %% Create owner process
         Owner = spawn(fun() ->
                              %% Wait for termination
                              receive
                                  die -> ok
                              end
                      end),

         %% Start client with owner
         Opts = #{
             mode => client,
             host => "localhost",
             port => 9999,  % Unavailable
             owner => Owner,
             transport_id => owner_test_transport
         },
         {ok, Pid} = erlmcp_transport_tcp:start_client(Opts),

         %% Verify client is monitoring owner
         {ok, State} = gen_server:call(Pid, get_state),
         ?assertEqual(Owner, State#state.owner),

         %% Kill owner
         Owner ! die,
         timer:sleep(200),

         %% Verify client stopped due to owner death
         ?assertNot(erlang:is_process_alive(Pid))
     end}.
```

**New Test 6: Socket Process Death**
```erlang
socket_process_death_test_() ->
    {timeout, 10,
     fun() ->
         application:ensure_all_started(ranch),

         %% Start server
         {ServerPid, _} = setup_server(),
         {ok, ServerState} = gen_server:call(ServerPid, get_state),
         Port = ServerState#state.port,

         %% Connect client
         ClientOpts = #{
             mode => client,
             host => "localhost",
             port => Port,
             owner => self()
         },
         {ok, ClientPid} = erlmcp_transport_tcp:start_client(ClientOpts),

         %% Wait for connection
         receive {transport_connected, ClientPid} -> ok
         after 3000 -> error("Connection timeout")
         end,

         %% Get client socket
         {ok, ClientState} = gen_server:call(ClientPid, get_state),
         Socket = ClientState#state.socket,

         %% Simulate socket process death
         %% This is tricky because gen_tcp sockets are ports, not processes
         %% We'll close it and verify the EXIT-like handling
         gen_tcp:close(Socket),

         %% Client should detect disconnection and schedule reconnect
         timer:sleep(300),
         {ok, NewState} = gen_server:call(ClientPid, get_state),
         ?assertEqual(false, NewState#state.connected),
         ?assert(NewState#state.reconnect_attempts > 0),

         %% Cleanup
         cleanup(ClientPid),
         cleanup(ServerPid),
         timer:sleep(100)
     end}.
```

**New Test 7: Ranch Listener Restart**
```erlang
ranch_listener_restart_test_() ->
    {timeout, 15,
     fun() ->
         application:ensure_all_started(ranch),

         %% Start server
         ServerOpts = #{
             mode => server,
             port => 0,
             owner => self(),
             transport_id => restart_test_transport,
             server_id => restart_test_server
         },
         {ok, ServerPid} = erlmcp_transport_tcp:start_server(ServerOpts),
         {ok, ServerState} = gen_server:call(ServerPid, get_state),
         Port = ServerState#state.port,
         RanchRef = ServerState#state.ranch_ref,

         %% Connect 3 clients
         Clients = [begin
             {ok, CPid} = erlmcp_transport_tcp:start_client(#{
                 mode => client,
                 host => "localhost",
                 port => Port,
                 owner => self()
             }),
             receive {transport_connected, CPid} -> ok
             after 3000 -> error({client_timeout, N})
             end,
             CPid
         end || N <- lists:seq(1, 3)],

         %% Stop Ranch listener
         ranch:stop_listener(RanchRef),
         timer:sleep(200),

         %% Verify clients disconnected
         [begin
             receive {transport_disconnected, CPid, _} -> ok
             after 2000 -> error({client_not_disconnected, N})
             end
         end || {CPid, N} <- lists:zip(Clients, lists:seq(1, 3))],

         %% Restart Ranch listener
         {ok, NewServerPid} = erlmcp_transport_tcp:start_server(ServerOpts),
         {ok, NewServerState} = gen_server:call(NewServerPid, get_state),
         ?assertEqual(Port, NewServerState#state.port),

         %% Verify new connections work
         {ok, NewClient} = erlmcp_transport_tcp:start_client(#{
             mode => client,
             host => "localhost",
             port => Port,
             owner => self()
         }),
         receive {transport_connected, NewClient} -> ok
         after 3000 -> error("New client connection timeout")
         end,

         %% Cleanup
         [cleanup(CPid) || CPid <- Clients],
         cleanup(NewClient),
         cleanup(NewServerPid),
         timer:sleep(100)
     end}.
```

**New Test 8: Socket Timeout During Connect**
```erlang
socket_timeout_during_connect_test_() ->
    {timeout, 10,
     fun() ->
         application:ensure_all_started(ranch),

         %% Use non-routable IP to force timeout
         Opts = #{
             mode => client,
             host => "10.255.255.1",  % Non-routable
             port => 9999,
             owner => self(),
             max_reconnect_attempts => 1
         },
         {ok, Pid} = erlmcp_transport_tcp:start_client(Opts),

         %% Wait for connection attempt to timeout
         timer:sleep(6000),

         %% Verify connection failed and reconnection scheduled
         {ok, State} = gen_server:call(Pid, get_state),
         ?assertEqual(false, State#state.connected),
         ?assert(State#state.reconnect_attempts > 0),

         %% Cleanup
         cleanup(Pid)
     end}.
```

**New Test 9: Socket Close During Send**
```erlang
socket_close_during_send_test_() ->
    {timeout, 10,
     fun() ->
         application:ensure_all_started(ranch),

         %% Start server
         {ServerPid, _} = setup_server(),
         {ok, ServerState} = gen_server:call(ServerPid, get_state),
         Port = ServerState#state.port,

         %% Start client
         ClientOpts = #{
             mode => client,
             host => "localhost",
             port => Port,
             owner => self()
         },
         {ok, ClientPid} = erlmcp_transport_tcp:start_client(ClientOpts),

         %% Wait for connection
         receive {transport_connected, ClientPid} -> ok
         after 3000 -> error("Connection timeout")
         end,

         %% Close server to trigger tcp_closed on client
         cleanup(ServerPid),

         %% Wait for client to detect disconnect
         timer:sleep(300),
         receive {transport_disconnected, ClientPid, _} -> ok
         after 2000 -> error("Client did not detect disconnect")
         end,

         %% Try to send - should fail
         Result = gen_server:call(ClientPid, {send, <<"test">>}),
         ?assertEqual({error, not_connected}, Result),

         %% Cleanup
         cleanup(ClientPid),
         timer:sleep(100)
     end}.
```

**Enhancement: Multiple Clients Test (5 → 10)**
```erlang
%% EXISTING TEST (lines 611-656) - MODIFY line 628
%% BEFORE:
%% NumClients = 5,

%% AFTER:
NumClients = 10,
```

**Reason**: These tests cover the critical code paths identified in diagnosis:
- Ranch protocol handler lifecycle (init, handshake, socket setup)
- Server-side error handling (tcp_closed, tcp_error)
- Client reconnection logic (exponential backoff, max attempts, timers)
- Owner death monitoring (process monitoring, shutdown)
- Socket process death (EXIT signal handling)
- Concurrent connection handling (10 simultaneous clients)
- Ranch listener restart (recover from listener stop)
- Socket edge cases (timeout, close during send)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_tcp_tests` - 100% pass rate (ALL 35+ tests pass)
- [ ] Coverage: `rebar3 cover --module=erlmcp_transport_tcp` - ≥80% coverage
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] All Ranch protocol handler paths covered (init, handshake, socket setup)
- [ ] All server-side error paths covered (tcp_closed, tcp_error)
- [ ] All client reconnection paths covered (backoff, max attempts, timers)
- [ ] Owner monitoring path covered (monitor, DOWN message, shutdown)
- [ ] Socket death path covered (EXIT signal, reconnection)
- [ ] Concurrent connections tested (10+ simultaneous clients)
- [ ] Ranch listener restart tested (stop, restart, new connections)
- [ ] Socket edge cases tested (timeout, close during send)
- [ ] Code review: OTP patterns followed correctly
- [ ] Code review: Chicago School TDD applied (real sockets, real Ranch)

**Note**: If coverage is still <80% after this phase, analyze coverage report again and add additional tests for remaining uncovered lines.

---

### Phase 4: Validate All Quality Gates

**Estimated Time:** 1 hour
**Priority:** P0 (CRITICAL) - Final validation before completion

#### Overview

Validate all quality gates pass, generate coverage receipts, and document final metrics. This phase confirms the manufacturing objective (≥80% coverage, 100% pass rate) is achieved.

#### Specification

**WHAT we're validating:**
1. Compilation with 0 errors
2. EUnit with 100% pass rate
3. Coverage with ≥80%
4. Dialyzer with 0 warnings
5. Xref with 0 undefined calls
6. Documentation of final metrics

#### Pseudocode

```
1. Run full quality gate suite
   EXECUTE cd /Users/sac/erlmcp
   RUN rebar3 do compile, xref, dialyzer, eunit --module=erlmcp_transport_tcp_tests, cover

2. Verify each gate
   IF compilation errors: FAIL and fix
   IF test failures: FAIL and fix
   IF coverage < 80%: FAIL and add more tests
   IF dialyzer warnings: FAIL and fix
   IF xref issues: FAIL and fix

3. Generate coverage report
   EXECUTE rebar3 cover --verbose
   SAVE: _build/test/cover/erlmcp_transport_tcp.coverdata

4. Document final metrics
   CREATE coverage_receipt.md with:
   - Coverage percentage
   - Test count (35+ tests)
   - Test pass rate (100%)
   - Uncovered lines (if any <80%)
   - Quality gate results

5. Code review checklist
   VERIFY: OTP patterns followed
   VERIFY: Chicago School TDD applied
   VERIFY: Real TCP sockets (no mocks)
   VERIFY: Real Ranch listeners (no mocks)
   VERIFY: State-based assertions
   VERIFY: Concurrent testing
   VERIFY: All error paths tested
```

#### Architecture

**QUALITY GATES:**
- rebar3 compile (compilation validation)
- rebar3 eunit (test execution)
- rebar3 cover (coverage measurement)
- rebar3 dialyzer (type checking)
- rebar3 xref (cross-reference analysis)

**RECEIPTS:**
- Coverage report: `_build/test/cover/index.html`
- Coverage data: `_build/test/cover/*.coverdata`
- Test log: `_build/test/logs/`

#### Changes Required:

**None** - This is a validation phase only. If any gate fails, return to previous phase to fix.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_tcp_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover --module=erlmcp_transport_tcp` - ≥80% coverage
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Coverage report reviewed and saved
- [ ] Final metrics documented (coverage %, test count, pass rate)
- [ ] Code review complete (OTP patterns, TDD, no mocks)
- [ ] All error paths tested
- [ ] Ranch integration fully tested
- [ ] Concurrent connections tested (10+ clients)
- [ ] Receipts generated (coverage report, test log)

**Deliverables:**
- Coverage receipt (HTML report)
- Test execution receipt (log files)
- Final metrics summary

---

## Testing Strategy

### Chicago School TDD (MANDATORY)

- **NO MOCKS** - Use real TCP sockets, real Ranch listeners, real gen_servers
- **State-Based Verification** - Check #state{} record contents, not just return values
- **Integration Testing** - Test with real dependencies (ranch, gen_tcp, inet)
- **Race Condition Testing** - Concurrent operations (10+ simultaneous clients)

### Unit Tests (EUnit)

**What to Test:**
- All public functions (transport_init, send, close)
- All gen_server callbacks (init, handle_call, handle_info, terminate)
- All Ranch protocol callbacks (start_link/3)
- All error paths (tcp_closed, tcp_error, owner death, socket death)
- All reconnection logic (exponential backoff, max attempts, timers)
- All buffer management (message extraction, framing)

**Test Pattern:**
- Reference: `erlmcp_transport_tcp_tests.erl:1-700` (existing tests)
- Fixtures: setup_client, setup_server, cleanup (lines 27-70)
- Assertions: ?assertEqual, ?assert, ?assertError, ?assertNot
- Timeouts: {timeout, Seconds, fun() -> ... end}

**Coverage Target:** ≥80% per module
**Pass Rate:** 100% (all tests must pass)

### Integration Tests

**End-to-End Scenarios:**
- Client connects to server, sends message, server receives (existing test, lines 258-324)
- Server accepts multiple concurrent connections (enhanced test, 10 clients)
- Client reconnection after server restart (new test)
- Ranch listener restart with active connections (new test)

**Multi-Process:**
- 10+ simultaneous client connections
- Concurrent disconnections
- Concurrent reconnections
- Ranch listener lifecycle (start, stop, restart)

**Failure Scenarios:**
- Connection timeout (new test)
- Socket closure during send (new test)
- Server crash during active connection
- Owner process death (new test)
- Socket process death (new test)

### Manual Testing Steps

1. Start Ranch server: `erlmcp_transport_tcp:start_server(#{mode => server, port => 0})`
2. Start 10 clients connecting to server
3. Verify all clients connect successfully
4. Send messages from all clients
5. Verify server receives all messages
6. Close server
7. Verify all clients detect disconnection
8. Verify clients attempt reconnection
9. Restart server
10. Verify clients reconnect successfully

### Quality Gates

Every phase MUST pass:
1. **Compilation**: `TERM=dumb rebar3 compile` - 0 errors
2. **EUnit**: `rebar3 eunit --module=erlmcp_transport_tcp_tests` - 100% pass rate
3. **Coverage**: `rebar3 cover --module=erlmcp_transport_tcp` - ≥80%
4. **Dialyzer**: `rebar3 dialyzer` - 0 warnings
5. **Xref**: `rebar3 xref` - 0 undefined calls

## Manufacturing Checklist

### Before Implementation
- [x] Research verified (read actual source code - 700-line test file, 613-line implementation)
- [x] Scope confirmed (IN scope: fix tests, add tests for uncovered paths; OUT of scope: modify implementation, add mocks, test pool functionality)
- [x] No open questions (all decisions made - Ranch 2.x API, socket timeouts, coverage gaps identified)
- [x] Phases broken down (4 phases, ≤4 hours each for phases 1, 2, 4; 8-12 hours for phase 3)
- [x] Acceptance criteria defined (measurable, specific - ≥80% coverage, 100% pass rate, 35+ tests)

### During Implementation
- [ ] Chicago School TDD followed (tests FIRST - use existing tests as reference)
- [ ] OTP patterns followed (gen_server, ranch_protocol, supervision)
- [ ] Type specs verified (Dialyzer clean - implementation already has specs)
- [ ] Error handling complete (all paths tested - tcp_closed, tcp_error, owner death, socket death)
- [ ] Quality gates passing (compilation, tests, coverage, dialyzer, xref)

### After Implementation
- [ ] All tests passing (100% pass rate - 35+ tests)
- [ ] Coverage ≥80% (verified with rebar3 cover)
- [ ] Dialyzer 0 warnings (verified with rebar3 dialyzer)
- [ ] Xref 0 undefined calls (verified with rebar3 xref)
- [ ] Performance no regression (N/A - coverage is objective, not performance)
- [ ] Documentation updated (this plan, PRD)
- [ ] Code review complete (OTP patterns verified, Chicago School TDD applied)

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **Test file doesn't compile** | P0 | High | Fix compilation errors immediately in Phase 2 |
| **Tests fail due to Ranch 2.x API changes** | P0 | High | Verify Ranch API compatibility in Phase 1, fix in Phase 2 |
| **Coverage measurement not working** | P0 | Low | Verify with rebar3 cover --verbose in Phase 1 |
| **Ranch protocol handler init path untested** | P1 | Medium | Add test in Phase 3 (ranch_protocol_handler_init_test_) |
| **Client reconnection logic untested** | P1 | Medium | Add test in Phase 3 (client_reconnection_backoff_test_) |
| **Socket error handling untested** | P1 | Medium | Add tests in Phase 3 (server_tcp_closed_test_, server_tcp_error_test_) |
| **Concurrent connection handling untested** | P1 | Low | Enhance existing test in Phase 3 (5 → 10 clients) |
| **Owner death monitoring untested** | P2 | Medium | Add test in Phase 3 (owner_death_monitoring_test_) |
| **Socket process death handling untested** | P2 | Medium | Add test in Phase 3 (socket_process_death_test_) |
| **Ranch listener restart untested** | P2 | Medium | Add test in Phase 3 (ranch_listener_restart_test_) |
| **Socket edge cases untested** | P2 | Low | Add tests in Phase 3 (timeout during connect, close during send) |
| **Coverage still <80% after Phase 3** | P1 | Low | Analyze coverage report, add additional tests as needed |

**Severity Definitions:**
- **P0 (Critical):** BLOCKS testing - MUST fix immediately
- **P1 (High):** Major quality gap - MUST fix before release
- **P2 (Medium):** Important but not blocking
- **P3 (Low):** Nice-to-have

### Rollback Plan

**If something goes wrong:**
- Git revert: `git revert HEAD` to undo test changes
- Data migration: N/A (tests don't modify data)
- Service impact: N/A (tests don't affect production)

**If coverage cannot reach 80%:**
- Document best achievable coverage percentage
- Document remaining uncovered lines
- Assess risk of uncovered code (production impact)
- Escalate to technical lead for decision

## References

- Research: `/Users/sac/erlmcp/.wreckit/items/007-create-eunit-test-suite-for-tcp-transport-erlmcptr/research.md`
- CLAUDE.md: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- TCPS: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- OTP Patterns: `/Users/sac/erlmcp/docs/otp-patterns.md`
- Test Implementation: `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl` (700 lines)
- Module Under Test: `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl` (613 lines)
- Behavior Specification: `apps/erlmcp_transports/src/erlmcp_transport_behavior.erl` (817 lines)
- Ranch 2.x Documentation: https://ninenines.eu/docs/en/ranch/2.1/guide/
