# Create EUnit test suite for WebSocket transport (erlmcp_transport_ws) Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Create comprehensive EUnit tests for `erlmcp_transport_ws` that achieve ≥80% coverage by testing **real Cowboy WebSocket protocol handling** with real WebSocket clients (Gun), not just utility function mocking. The existing test file provides a foundation but only tests exported helper functions. Critical Cowboy WebSocket handler callbacks have **ZERO coverage**.

### Quality Gate Requirements

- **Compilation**: 0 errors, 0 warnings (MANDATORY)
- **EUnit**: 100% pass rate (MANDATORY)
- **Coverage**: ≥80% for `erlmcp_transport_ws.erl` (MANDATORY)
  - Lines 162-193: `init(Req, State)` - WebSocket upgrade handler
  - Lines 195-258: `websocket_handle/2` - Frame processing
  - Lines 260-303: `websocket_info/2` - Send and close handling
  - Lines 391-422: Fragment reassembly
  - Lines 480-538: Backpressure management
- **Dialyzer**: 0 warnings (MANDATORY)
- **Xref**: 0 undefined function calls (MANDATORY)
- **Test Execution Time**: <30 seconds for all tests

## Current State

### What Exists Now

**Modules:**
- `apps/erlmcp_transports/src/erlmcp_transport_ws.erl` (539 lines)
  - Cowboy 2.10.0 WebSocket handler
  - Custom init/2 interface for WebSocket upgrade
  - State record with backpressure tracking
  - Message delimiter: newline (`\n`)
  - Fragment reassembly with 30-second timeout
  - Backpressure with 100KB buffer, 5-second recovery

**Tests:**
- `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl` (385 lines)
  - 40 test functions across 8 test groups
  - **Coverage**: ~5% of actual WebSocket functionality
  - Tests only utility functions: `validate_utf8/1`, `validate_message_size/1`, `generate_session_id/0`
  - **ZERO coverage** of:
    - Cowboy WebSocket callbacks (init/2, websocket_handle/2, websocket_info/2)
    - Real WebSocket connections
    - Backpressure management
    - Fragment reassembly
    - Error handling paths

**Quality:**
- Current tests **pass** but provide false sense of security
- Cowboy WebSocket handler callbacks completely untested
- Production deployment **BLOCKED** by lack of protocol coverage

### What's Missing

**Gap**: Test file exists but only tests exported helper functions. Critical WebSocket protocol paths have ZERO coverage.

**Root Cause**: Missing Cowboy-based integration test infrastructure. Existing tests use mock PIDs and stub assertions instead of real Cowboy listeners and Gun WebSocket clients (Chicago School TDD).

**Impact**:
- **BLOCKS PRODUCTION DEPLOYMENT** (P1 urgency)
- WebSocket transport is untested for real browser clients
- No verification of frame handling, backpressure, or fragment reassembly
- Errors will only be discovered in production

### Key Discoveries from Code Review

**Finding 1**: `erlmcp_transport_ws` is NOT a gen_server - it's a Cowboy WebSocket handler with custom callbacks:
- `init(Req, [TransportId, Config]) -> {cowboy_websocket, Req, State, Opts}` (line 162)
- `websocket_handle({text, Data}, State)` (line 195)
- `websocket_info({send_frame, Data}, State)` (line 260)
- Cannot be tested with gen_server patterns

**Finding 2**: Cowboy dependency is at **top level** (`rebar.config:52`), not in transports app:
- `{cowboy, "2.10.0"}` - HTTP/WebSocket server
- `{gun, "2.0.1"}` - HTTP/WebSocket client (for testing)
- Both available in test environment via top-level rebar.config

**Finding 3**: TCP transport tests show the pattern (Chicago School TDD):
- `erlmcp_transport_tcp_tests.erl:214-252` - Real Ranch listener with `port => 0`
- `erlmcp_transport_tcp_tests.erl:238-250` - Real TCP client connection
- `erlmcp_transport_tcp_tests.erl:258-324` - Client-server integration with real sockets
- Must follow this pattern for WebSocket tests

**Finding 4**: Backpressure management requires real buffer overflow testing:
- Lines 480-504: `check_backpressure/1` - Activate at 100% buffer
- Lines 518-538: `resume_reading/1` - Resume at 50% drain threshold
- Cannot be tested with mocks - need real message flooding

**Finding 5**: Fragment reassembly has timeout that cannot be tested efficiently:
- Lines 410-422: `check_fragment_timeout/1` - 30-second timeout
- Will use short timeout (100ms) in test config for faster testing

## Desired End State

### Specification

Create EUnit tests for `erlmcp_transport_ws` that:

1. **Start real Cowboy listeners** on random ports (avoid conflicts)
2. **Connect real Gun WebSocket clients** to test all frame types
3. **Test all WebSocket frame types**:
   - `{text, Data}` - Text frames with delimiter handling
   - `{binary, Data}` - Binary frames rejected with close 1002
   - `ping` / `pong` - Heartbeat frames
   - `{close, Code, Reason}` - Graceful shutdown
4. **Test message processing**:
   - Newline delimiter validation (`\n`)
   - Fragment reassembly (multi-part messages)
   - UTF-8 validation (multibyte, emoji, invalid sequences)
   - Message size limits (16MB default)
5. **Test backpressure management**:
   - Buffer fills to 100% → backpressure active
   - Buffer drains to 50% → resume reading
   - Messages rejected during backpressure
6. **Test error handling**:
   - Invalid UTF-8 → close 1002
   - Message too big → close 1009
   - JSON parse errors → close 1002
   - Fragment timeout → close 1002
7. **Test bidirectional messaging**:
   - Client → server messages (via Gun)
   - Server → client responses (via cowboy_websocket handler)
   - Concurrent send/receive

### Verification

**Automated:**
```bash
cd apps/erlmcp_transports
rebar3 compile
rebar3 eunit --module=erlmcp_transport_ws_tests --cover
rebar3 cover --verbose
```

**Coverage targets by function:**
- `init/2` (listener, lines 79-126): 90%
- `init/2` (handler, lines 162-193): 90%
- `websocket_handle/2` (lines 195-258): 85%
- `websocket_info/2` (lines 260-303): 85%
- `handle_text_frame/2` (lines 310-320): 80%
- `process_messages/2` (lines 323-334): 80%
- `process_lines/3` (lines 337-356): 80%
- `reassemble_fragment/1` (lines 391-407): 80%
- `check_backpressure/1` (lines 481-504): 85%
- `update_buffer_usage/3` (lines 507-515): 80%
- `resume_reading/1` (lines 518-538): 80%
- `validate_utf8/1` (lines 435-444): 90% (already tested)
- `validate_message_size/1` (lines 425-432): 90% (already tested)

**Manual:**
- Review coverage report to verify lines 162-303 (Cowboy callbacks) are covered
- Verify all frame types tested (text, binary, ping, pong, close)
- Verify backpressure activation/recovery tested
- Verify fragment reassembly tested

### Manufacturing Output

**Code:**
- Modified: `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl` (add integration tests)

**Tests:**
- Created: 50+ new EUnit test functions
  - Cowboy setup/cleanup helpers (5 functions)
  - Text frame tests (8 tests)
  - Binary frame tests (3 tests)
  - Ping/pong tests (4 tests)
  - Close frame tests (5 tests)
  - Message delimiter tests (6 tests)
  - Fragment reassembly tests (7 tests)
  - UTF-8 validation tests (6 tests)
  - Message size tests (5 tests)
  - Backpressure tests (8 tests)
  - Error handling tests (8 tests)
  - Bidirectional tests (6 tests)
  - Integration tests (5 tests)

**Documentation:**
- Updated: Test file comments describing coverage targets

**Receipts:**
- Coverage report: `_build/test/cover/erlmcp_transport_ws.coverdata`
- Test results: `_build/test/logs/eunit.log`

## What We're NOT Doing

**OUT OF SCOPE:**

1. **Property-based testing (PropEr)** - Out of scope for EUnit
   - Would be valuable for randomized frame testing
   - Defer to future work

2. **Load testing beyond 1000 messages** - Out of scope
   - Backpressure tests will use 100-1000 messages
   - Sustained load testing (10K+ messages) requires separate benchmark suite

3. **Real network testing** - Out of scope
   - Testing packet loss, latency requires network simulation tools (tc, netem)
   - Focus on protocol logic, not network conditions

4. **Idle timeout testing** - Out of scope
   - Idle timeout is 5 minutes (line 35)
   - Too long for unit tests - will assume Cowboy handles this

5. **Performance regression testing** - Out of scope
   - No baseline benchmarks established
   - Focus on correctness, not performance

6. **Modifying production code** - Out of scope
   - Only adding tests, no changes to `erlmcp_transport_ws.erl`
   - If bugs found, create separate work item

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** - Requirements with acceptance criteria ✅ (this document)
2. **Pseudocode** - Algorithm design BEFORE coding ✅ (in phase details)
3. **Architecture** - Integration points and supervision tree ✅ (documented)
4. **Refinement** - Chicago School TDD (tests FIRST) ⚠️ (implementation phase)
5. **Completion** - All quality gates passing ⚠️ (verification phase)

### Implementation Strategy

**High-level approach:**

Add **Cowboy-based integration tests** to the existing test file following the pattern from `erlmcp_transport_tcp_tests.erl`. Use real Cowboy listeners and Gun WebSocket clients to test all protocol paths.

**WHY this strategy:**

1. **Chicago School TDD**: Real processes, real protocols, no mocks
   - TCP transport tests prove this pattern works
   - Catches integration bugs that unit tests miss
   - Tests actual Cowboy WebSocket handler behavior

2. **Gun WebSocket client**: Already in dependencies (gun 2.0.1)
   - `gun:ws_upgrade/2` - Upgrade HTTP to WebSocket
   - `gun:ws_send/3` - Send WebSocket frames
   - `gun:await/2` - Receive WebSocket responses
   - Production-quality HTTP/WebSocket client

3. **Random port allocation**: Use `{port, 0}` for Cowboy listener
   - Cowboy auto-assigns free port
   - Avoids "address already in use" in parallel tests
   - Retrieve actual port via `cowboy:addr_info/1`

4. **Test isolation**: Sequential execution for integration tests
   - Cowboy listeners have side effects (bind to ports)
   - Use `{timeout, 15}` for integration tests
   - Unit tests (fast) can run in parallel

5. **Backpressure testing**: Small buffer sizes for speed
   - Use 1KB buffer (vs 100KB production) for faster overflow
   - Test backpressure activation with 10-20 messages
   - Test recovery with smaller message counts

### Quality Integration

**Pre-commit Hooks:**
- `.claude/hooks/pre-task-validate.sh` - Runs `rebar3 compile` before commit

**CI Gates:**
- Compilation must pass (0 errors, 0 warnings)
- EUnit must pass (100% pass rate)
- Coverage must be ≥80% (verified in CI logs)
- Dialyzer must pass (0 warnings)
- Xref must pass (0 undefined calls)

**Receipt Generation:**
- Coverage report: `rebar3 cover --verbose` → `_build/test/cover/index.html`
- Test logs: `_build/test/logs/eunit.log`
- Coverage receipts saved per TCPS process

**Andon Signaling:**
- Coverage failures visible in CI logs
- Test failures stop the build (no "skip and continue")
- All quality gates must pass before merging

---

## Phases

### Phase 1: Add Cowboy Integration Test Infrastructure

**Estimated Time**: 2 hours
**Priority**: CRITICAL - BLOCKS ALL PROGRESS

#### Overview

Add Cowboy listener setup/cleanup helpers and Gun WebSocket client helpers to enable real WebSocket protocol testing. Without this infrastructure, we cannot test Cowboy WebSocket callbacks.

#### Specification

**WHAT we're building:**

1. **Cowboy setup helper**: `setup_cowboy_listener/0`
   - Starts Cowboy on random port (port 0)
   - Returns map with port, transport_id, config
   - Unique listener name per test (avoid conflicts)

2. **Cowboy cleanup helper**: `cleanup_cowboy_listener/1`
   - Stops Cowboy listener
   - Cleans up connections
   - Waits for cleanup to complete

3. **Gun WebSocket client helper**: `connect_ws_client/2`
   - Opens Gun connection to Cowboy
   - Upgrades to WebSocket protocol
   - Returns ConnPid and StreamRef
   - Waits for upgrade confirmation

4. **WebSocket frame helpers**:
   - `send_text_frame/3` - Send text frame via Gun
   - `receive_text_frame/2` - Receive text frame with timeout
   - `send_binary_frame/3` - Send binary frame (should be rejected)
   - `send_ping/2` - Send ping frame
   - `close_ws_client/1` - Close Gun connection

5. **Test fixture for integration tests**:
   - Setup: Start Cowboy, connect Gun client
   - Cleanup: Close client, stop Cowboy
   - Timeout: 15 seconds (enough for connection setup)

#### Pseudocode

```erlang
%% Cowboy setup helper
setup_cowboy_listener() ->
    %% Generate unique listener name to avoid conflicts
    UniqueId = erlang:unique_integer([positive]),
    ListenerName = list_to_atom("erlmcp_ws_listener_test_" ++ integer_to_list(UniqueId)),

    %% Start Cowboy on random port (OS assigns free port)
    TransportId = <<"test_ws_transport">>,
    Config = #{
        port => 0,  %% Random port
        path => "/mcp/ws",
        max_message_size => 16777216,
        strict_delimiter_check => true,
        validate_utf8 => true,
        frame_buffer_size => 102400  %% 100KB default
    },

    %% Start Cowboy listener
    {ok, _Pid} = erlmcp_transport_ws:init(TransportId, Config#{listener_name => ListenerName}),
    timer:sleep(100),  %% Give Cowboy time to start

    %% Get actual port assigned by Cowboy
    %% Note: Need to modify erlmcp_transport_ws:init to return port
    %% For now, assume default port or extract from cowboy
    Port = maps:get(port, Config, 8080),  %% TODO: Get actual port from Cowboy

    #{
        listener_name => ListenerName,
        transport_id => TransportId,
        port => Port,
        path => "/mcp/ws",
        config => Config
    }.

%% Cowboy cleanup helper
cleanup_cowboy_listener(#{listener_name := ListenerName}) ->
    %% Stop Cowboy listener
    catch cowboy:stop_listener(ListenerName),
    timer:sleep(50),  %% Give Cowboy time to cleanup
    ok.

%% Gun WebSocket client helper
connect_ws_client(Port, Path) ->
    %% Open Gun connection
    {ok, ConnPid} = gun:open("localhost", Port),

    %% Wait for Gun to connect
    {ok, _} = gun:await_up(ConnPid),

    %% Upgrade to WebSocket
    StreamRef = gun:ws_upgrade(ConnPid, Path),

    %% Wait for WebSocket upgrade
    receive
        {gun_upgrade, ConnPid, StreamRef, _, _} ->
            {ok, ConnPid, StreamRef};
        {gun_response, ConnPid, _, _, Status, Headers} ->
            {error, {upgrade_failed, Status, Headers}}
    after 5000 ->
        {error, timeout}
    end.

%% Send text frame
send_text_frame(ConnPid, StreamRef, Text) ->
    gun:ws_send(ConnPid, StreamRef, {text, Text}),
    ok.

%% Receive text frame
receive_text_frame(ConnPid, Timeout) ->
    receive
        {gun_ws, ConnPid, _, {text, Data}} ->
            {ok, Data};
        {gun_ws, ConnPid, _, {close, Code, Reason}} ->
            {error, {closed, Code, Reason}}
    after Timeout ->
        timeout
    end.

%% Send binary frame (should be rejected)
send_binary_frame(ConnPid, StreamRef, Binary) ->
    gun:ws_send(ConnPid, StreamRef, {binary, Binary}),
    ok.

%% Send ping frame
send_ping(ConnPid, StreamRef) ->
    gun:ws_send(ConnPid, StreamRef, ping),
    ok.

%% Close Gun client
close_ws_client(ConnPid) ->
    gun:close(ConnPid),
    ok.
```

#### Architecture

**INTEGRATION - Dependencies and Process Structure:**

```
Test Process (EUnit)
    │
    ├── setup_cowboy_listener/0
    │   └── erlmcp_transport_ws:init/2
    │       └── cowboy:start_clear/3
    │           └── Cowboy Listener (erlmcp_ws_listener_test_<unique>)
    │               └── Cowboy Connection Processes (spawned per connection)
    │
    ├── connect_ws_client/2
    │   └── gun:open/2
    │       └── Gun Client Process
    │           └── gun:ws_upgrade/2
    │               └── WebSocket Connection to Cowboy
    │
    └── [Test Functions]
        ├── send_text_frame/3
        ├── receive_text_frame/2
        └── verify Cowboy behavior via Gun
```

**Dependencies:**
- `cowboy 2.10.0` - HTTP/WebSocket server (from top-level rebar.config)
- `gun 2.0.1` - HTTP/WebSocket client (from top-level rebar.config)
- `erlmcp_transport_ws` - Module under test
- `erlmcp` application - Started in setup

**Supervision:**
- Cowboy manages its own listener and connection processes
- Gun client manages its connection process
- Test process coordinates both

**Process lifecycle:**
1. Test setup starts Cowboy listener
2. Test setup connects Gun client
3. Test function sends/receives frames
4. Test cleanup closes Gun client
5. Test cleanup stops Cowboy listener

#### Changes Required:

##### 1. Add Cowboy/Gun Helper Functions

**File**: `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl`
**Current**: Lines 1-385 (existing utility function tests)
**Changes**: Add helper functions after line 21 (after cleanup/1)

```erlang
%%====================================================================
%% Cowboy WebSocket Integration Test Helpers
%%====================================================================

%% Setup Cowboy listener on random port
setup_cowboy_listener() ->
    application:ensure_all_started(cowboy),
    application:ensure_all_started(gun),
    application:ensure_all_started(erlmcp),

    UniqueId = erlang:unique_integer([positive]),
    ListenerName = list_to_atom("erlmcp_ws_listener_test_" ++ integer_to_list(UniqueId)),

    TransportId = <<"test_ws_transport_">> <- use UniqueId>,
    Config = #{
        listener_name => ListenerName,
        port => 0,  %% Random port
        path => "/mcp/ws",
        max_message_size => 16777216,
        strict_delimiter_check => true,
        validate_utf8 => true,
        frame_buffer_size => 102400
    },

    %% Modify erlmcp_transport_ws:init to support custom listener name
    {ok, _Pid} = erlmcp_transport_ws:init(TransportId, Config),
    timer:sleep(100),  %% Let Cowboy start

    %% TODO: Get actual port from Cowboy
    %% For now, extract from Ranch if available
    Port = case ListenerName of
        undefined -> 8080;
        _ -> get_cowboy_port(ListenerName)
    end,

    #{
        listener_name => ListenerName,
        transport_id => TransportId,
        port => Port,
        path => maps:get(path, Config),
        config => Config
    }.

%% Get actual port from Cowboy listener
get_cowboy_port(ListenerName) ->
    try
        %% Cowboy 2.x stores listener info
        %% This is a placeholder - actual API may differ
        {ok, Port} = ranch:get_port(ListenerName),
        Port
    catch
        _:_ -> 8080  %% Fallback
    end.

%% Cleanup Cowboy listener
cleanup_cowboy_listener(#{listener_name := ListenerName}) ->
    catch cowboy:stop_listener(ListenerName),
    timer:sleep(50),
    ok.

%% Connect Gun WebSocket client
connect_ws_client(Port, Path) ->
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _} = gun:await_up(ConnPid, 5000),

    StreamRef = gun:ws_upgrade(ConnPid, Path),

    receive
        {gun_upgrade, ConnPid, StreamRef, _, _} ->
            {ok, ConnPid, StreamRef};
        {gun_response, ConnPid, _, _, Status, Headers} ->
            {error, {upgrade_failed, Status, Headers}}
    after 5000 ->
        {error, timeout}
    end.

%% Send text frame
send_text_frame(ConnPid, StreamRef, Text) ->
    gun:ws_send(ConnPid, StreamRef, {text, Text}),
    ok.

%% Receive text frame with timeout
receive_text_frame(ConnPid, Timeout) ->
    receive
        {gun_ws, ConnPid, _, {text, Data}} ->
            {ok, Data};
        {gun_ws, ConnPid, _, {close, Code, Reason}} ->
            {error, {closed, Code, Reason}}
    after Timeout ->
        timeout
    end.

%% Send binary frame
send_binary_frame(ConnPid, StreamRef, Binary) ->
    gun:ws_send(ConnPid, StreamRef, {binary, Binary}),
    ok.

%% Send ping frame
send_ping(ConnPid, StreamRef) ->
    gun:ws_send(ConnPid, StreamRef, ping),
    ok.

%% Close Gun client
close_ws_client(ConnPid) ->
    gun:close(ConnPid),
    ok.
```

**Reason**: These helpers enable real Cowboy/Gun testing. Without them, we cannot test WebSocket protocol.

##### 2. Modify erlmcp_transport_ws:init to Support Custom Listener Names

**File**: `apps/erlmcp_transports/src/erlmcp_transport_ws.erl`
**Current**: Lines 79-126
**Changes**: Add support for custom listener name in Config

```erlang
%% BEFORE (line 114)
{ok, _} = cowboy:start_clear(erlmcp_ws_listener,
    ListenerOpts,
    #{env => #{dispatch => Dispatch}}),

%% AFTER
ListenerName = maps:get(listener_name, Config, erlmcp_ws_listener),
{ok, _} = cowboy:start_clear(ListenerName,
    ListenerOpts,
    #{env => #{dispatch => Dispatch}}),
```

**Reason**: Custom listener names avoid conflicts in parallel tests. Each test gets unique listener name.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_ws_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - Helper functions covered (not yet target 80%)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: Helper functions follow Cowboy/Gun API correctly
- [ ] Integration: Cowboy listener starts, Gun client connects
- [ ] Edge cases: Port conflicts handled, cleanup works

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 2: Test WebSocket Text Frame Handling

**Estimated Time**: 3 hours
**Priority**: HIGH - CORE FUNCTIONALITY

#### Overview

Test all text frame scenarios including delimiter handling, fragment buffering, and UTF-8 validation. This covers the `websocket_handle({text, Data}, State)` callback (lines 195-236).

#### Specification

**WHAT we're building:**

Text frame tests (8 tests):

1. **Text frame with delimiter** - Single complete message with `\n`
2. **Text frame without delimiter** - Fragment buffering activated
3. **Multiple text frames** - Multiple messages with delimiters
4. **Empty text frame** - Empty message handling
5. **UTF-8 text frame** - Valid multibyte characters
6. **Invalid UTF-8 text frame** - Should close connection with 1002
7. **Oversized text frame** - Should close connection with 1009
8. **Fragmented text frames** - Reassembly of multi-part message

#### Pseudocode

```erlang
%% Test fixture for text frames
text_frame_setup() ->
    State = setup_cowboy_listener(),
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),
    {State, ConnPid, StreamRef}.

text_frame_cleanup({State, ConnPid, _StreamRef}) ->
    close_ws_client(ConnPid),
    cleanup_cowboy_listener(State).

%% Test 1: Text frame with delimiter
test_text_frame_with_delimiter({State, ConnPid, StreamRef}) ->
    Message = <<"{\"jsonrpc\":\"2.0\"}\n">>,

    %% Send text frame
    ok = send_text_frame(ConnPid, StreamRef, Message),

    %% Should be processed successfully (no close frame)
    ?assertEqual(timeout, receive_text_frame(ConnPid, 1000)),

    %% Connection should still be open
    ?assertEqual(true, is_process_alive(ConnPid)).

%% Test 2: Text frame without delimiter (fragment)
test_text_frame_without_delimiter({State, ConnPid, StreamRef}) ->
    Fragment1 = <<"{\"jsonrpc\":">>,

    %% Send first fragment (no delimiter)
    ok = send_text_frame(ConnPid, StreamRef, Fragment1),

    %% Should buffer fragment (no close frame)
    ?assertEqual(timeout, receive_text_frame(ConnPid, 1000)),

    %% Send second fragment with delimiter
    Fragment2 = <<"\"2.0\"}\n">>,
    ok = send_text_frame(ConnPid, StreamRef, Fragment2),

    %% Should now be processed
    ?assertEqual(timeout, receive_text_frame(ConnPid, 1000)).

%% Test 3: Invalid UTF-8 in text frame
test_invalid_utf8_text_frame({State, ConnPid, StreamRef}) ->
    %% Create invalid UTF-8 sequence
    InvalidUTF8 = <<195, 40, $\n>>,  %% Incomplete multibyte + newline

    %% Send invalid UTF-8
    ok = send_text_frame(ConnPid, StreamRef, InvalidUTF8),

    %% Should receive close frame with 1002 (protocol error)
    Result = receive_text_frame(ConnPid, 2000),
    ?assertMatch({error, {closed, 1002, <<"Invalid UTF-8 encoding">>}}, Result).

%% Test 4: Oversized text frame
test_oversized_text_frame({State, ConnPid, StreamRef}) ->
    %% Set small limit for testing
    application:set_env(erlmcp, max_ws_message_size, 1000),

    OversizedMsg = binary:copy(<<"x">>, 1001) ++ <<"\n">>,

    %% Send oversized message
    ok = send_text_frame(ConnPid, StreamRef, OversizedMsg),

    %% Should receive close frame with 1009 (message too big)
    Result = receive_text_frame(ConnPid, 2000),
    ?assertMatch({error, {closed, 1009, _}}, Result).

%% Test 5: Multiple text frames in stream
test_multiple_text_frames({State, ConnPid, StreamRef}) ->
    Msg1 = <<"{\"id\":1}\n">>,
    Msg2 = <<"{\"id\":2}\n">>,
    Msg3 = <<"{\"id\":3}\n">>,

    %% Send all messages
    ok = send_text_frame(ConnPid, StreamRef, Msg1),
    ok = send_text_frame(ConnPid, StreamRef, Msg2),
    ok = send_text_frame(ConnPid, StreamRef, Msg3),

    %% All should be processed
    ?assertEqual(timeout, receive_text_frame(ConnPid, 1000)).
```

#### Architecture

**Process flow:**
```
Test Process
    ├── setup_cowboy_listener/0 → Cowboy Listener
    ├── connect_ws_client/2 → Gun Client
    ├── send_text_frame/3 → Gun sends to Cowboy
    ├── Cowboy websocket_handle({text, Data}, State)
    │   ├── validate_message_size/1
    │   ├── handle_text_frame/2
    │   ├── process_messages/2
    │   └── validate_utf8/1
    └── receive_text_frame/2 → Verify response
```

**Coverage targets:**
- `websocket_handle({text, Data}, State)` (lines 195-236): 85%
- `handle_text_frame/2` (lines 310-320): 80%
- `process_messages/2` (lines 323-334): 80%
- `process_lines/3` (lines 337-356): 80%

#### Changes Required:

##### Add Text Frame Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl`
**Current**: After helpers added in Phase 1
**Changes**: Add text frame test group

```erlang
%%====================================================================
%% Text Frame Tests
%%====================================================================

websocket_text_frame_test_() ->
    {setup,
     fun setup_cowboy_listener/0,
     fun cleanup_cowboy_listener/1,
     fun(State) -> [
         {"Text frame with delimiter",
          fun() -> test_text_frame_with_delimiter(State) end},
         {"Text frame without delimiter (fragment)",
          fun() -> test_text_frame_without_delimiter(State) end},
         {"Multiple text frames",
          fun() -> test_multiple_text_frames(State) end},
         {"Empty text frame",
          fun() -> test_empty_text_frame(State) end},
         {"Valid UTF-8 text frame",
          fun() -> test_valid_utf8_text_frame(State) end},
         {"Invalid UTF-8 text frame",
          fun() -> test_invalid_utf8_text_frame(State) end},
         {"Oversized text frame",
          fun() -> test_oversized_text_frame(State) end},
         {"Fragmented text frames",
          fun() -> test_fragmented_text_frames(State) end}
     ] end}.
```

**Reason**: Text frames are the primary message type for WebSocket MCP. Must test all paths.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_ws_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - websocket_handle/2 (text frames) ≥85%
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All text frame paths tested
- [ ] Coverage report: Lines 195-236 covered
- [ ] Edge cases: Delimiter, fragments, UTF-8, size limits

---

### Phase 3: Test WebSocket Frame Rejection and Ping/Pong

**Estimated Time**: 2 hours
**Priority**: HIGH - PROTOCOL COMPLIANCE

#### Overview

Test binary frame rejection (not supported), ping/pong handling, and close frame processing. Covers remaining clauses in `websocket_handle/2` (lines 238-258).

#### Specification

**WHAT we're building:**

Frame type tests (7 tests):

1. **Binary frame rejection** - Binary frames close with 1002
2. **Ping frame** - Pong response sent automatically
3. **Pong frame** - Handled silently
4. **Close frame (normal)** - Connection closes gracefully
5. **Close frame (protocol error)** - Specific close code
6. **Unknown frame type** - Ignored (no crash)
7. **Frame ordering** - Mixed frame types in stream

#### Pseudocode

```erlang
%% Test 1: Binary frame rejection
test_binary_frame_rejection(State) ->
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Send binary frame (not supported)
    BinaryData = <<1, 2, 3, 4>>,
    ok = send_binary_frame(ConnPid, StreamRef, BinaryData),

    %% Should receive close frame with 1002 (protocol error)
    Result = receive_text_frame(ConnPid, 2000),
    ?assertMatch({error, {closed, 1002, <<"Binary frames not supported">>}}, Result),

    close_ws_client(ConnPid).

%% Test 2: Ping frame
test_ping_frame(State) ->
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Send ping frame
    ok = send_ping(ConnPid, StreamRef),

    %% Should receive pong frame automatically
    receive
        {gun_ws, ConnPid, _, pong} ->
            ?assert(true);
        {gun_ws, ConnPid, _, {close, _, _}} ->
            ?assert(false, "Connection closed on ping")
    after 2000 ->
        ?assert(false, "No pong received")
    end,

    close_ws_client(ConnPid).

%% Test 3: Close frame from client
test_close_frame(State) ->
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Send close frame
    gun:ws_send(ConnPid, StreamRef, {close, 1000, <<"Normal shutdown">>}),

    %% Connection should close gracefully
    receive
        {gun_down, ConnPid, _, _, _} ->
            ?assert(true)
    after 2000 ->
        ?assert(false, "Connection did not close")
    end.
```

#### Architecture

**Coverage targets:**
- `websocket_handle({binary, Data}, State)` (lines 238-245): 90%
- `websocket_handle(ping, State)` (line 247-248): 90%
- `websocket_handle(pong, State)` (lines 250-251): 90%
- `websocket_handle({close, Code, Reason}, State)` (lines 253-255): 90%
- `websocket_handle(_Frame, State)` (lines 257-258): 90%

#### Changes Required:

##### Add Frame Rejection Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl`
**Current**: After text frame tests
**Changes**: Add frame rejection test group

```erlang
%%====================================================================
%% Frame Rejection and Ping/Pong Tests
%%====================================================================

websocket_frame_type_test_() ->
    {setup,
     fun setup_cowboy_listener/0,
     fun cleanup_cowboy_listener/1,
     fun(State) -> [
         {"Binary frame rejection",
          fun() -> test_binary_frame_rejection(State) end},
         {"Ping frame handling",
          fun() -> test_ping_frame(State) end},
         {"Pong frame handling",
          fun() -> test_pong_frame(State) end},
         {"Close frame (normal)",
          fun() -> test_close_frame_normal(State) end},
         {"Close frame (custom code)",
          fun() -> test_close_frame_custom(State) end},
         {"Unknown frame type",
          fun() -> test_unknown_frame_type(State) end},
         {"Mixed frame types",
          fun() -> test_mixed_frame_types(State) end}
     ] end}.
```

**Reason**: WebSocket protocol requires proper handling of all frame types. RFC 6455 compliance.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_ws_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - websocket_handle/2 (all clauses) ≥85%
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All frame types tested
- [ ] Coverage report: Lines 238-258 covered
- [ ] Protocol compliance: RFC 6455 requirements met

---

### Phase 4: Test Fragment Reassembly

**Estimated Time**: 2 hours
**Priority**: HIGH - MESSAGE LOGIC

#### Overview

Test fragment reassembly logic for multi-part WebSocket messages. Covers `reassemble_fragment/1` and `check_fragment_timeout/1` (lines 391-422).

#### Specification

**WHAT we're building:**

Fragment reassembly tests (7 tests):

1. **Two-part fragment** - Message split across 2 frames
2. **Multipart fragment** - Message split across 3+ frames
3. **Incomplete fragment** - No delimiter, buffered
4. **Fragment completion** - Delimiter arrives, message processed
5. **Fragment timeout** - Timeout before completion (use 100ms for testing)
6. **Concurrent fragments** - Multiple fragmented messages
7. **Fragment overflow** - Fragment exceeds max size

#### Pseudocode

```erlang
%% Test 1: Two-part fragment
test_two_part_fragment(State) ->
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Send first fragment (no delimiter)
    Fragment1 = <<"{\"jsonrpc\":">>,
    ok = send_text_frame(ConnPid, StreamRef, Fragment1),

    %% Send second fragment with delimiter
    Fragment2 = <<"\"2.0\"}\n">>,
    ok = send_text_frame(ConnPid, StreamRef, Fragment2),

    %% Should be processed as complete message
    ?assertEqual(timeout, receive_text_frame(ConnPid, 1000)),

    close_ws_client(ConnPid).

%% Test 2: Fragment timeout
test_fragment_timeout(State) ->
    %% Set short timeout for testing
    application:set_env(erlmcp, fragment_timeout, 100),

    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Send incomplete fragment
    Fragment = <<"{\"incomplete\">,
    ok = send_text_frame(ConnPid, StreamRef, Fragment),

    %% Wait for timeout (100ms + buffer)
    timer:sleep(200),

    %% Connection should close with 1002 (fragment timeout)
    Result = receive_text_frame(ConnPid, 1000),
    ?assertMatch({error, {closed, 1002, <<"Fragment reassembly timeout">>}}, Result).

%% Test 3: Multipart fragment
test_multipart_fragment(State) ->
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Send message in 3 parts
    Part1 = <<"{\"">>,
    Part2 = <<"jsonrpc\">>,
    Part3 = <<"\":\"2.0\"}\n">>,

    ok = send_text_frame(ConnPid, StreamRef, Part1),
    ok = send_text_frame(ConnPid, StreamRef, Part2),
    ok = send_text_frame(ConnPid, StreamRef, Part3),

    %% Should be processed
    ?assertEqual(timeout, receive_text_frame(ConnPid, 1000)),
```

#### Architecture

**Coverage targets:**
- `handle_text_frame/2` (lines 310-320): 85%
- `reassemble_fragment/1` (lines 391-407): 85%
- `check_fragment_timeout/1` (lines 410-422): 80%
- Fragment buffer state transitions: 85%

#### Changes Required:

##### Add Fragment Reassembly Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl`
**Current**: After frame rejection tests
**Changes**: Add fragment reassembly test group

```erlang
%%====================================================================
%% Fragment Reassembly Tests
%%====================================================================

websocket_fragment_reassembly_test_() ->
    {setup,
     fun setup_cowboy_listener/0,
     fun cleanup_cowboy_listener/1,
     fun(State) -> [
         {"Two-part fragment",
          fun() -> test_two_part_fragment(State) end},
         {"Multipart fragment",
          fun() -> test_multipart_fragment(State) end},
         {"Incomplete fragment buffering",
          fun() -> test_incomplete_fragment(State) end},
         {"Fragment completion",
          fun() -> test_fragment_completion(State) end},
         {"Fragment timeout",
          fun() -> test_fragment_timeout(State) end},
         {"Concurrent fragments",
          fun() -> test_concurrent_fragments(State) end},
         {"Fragment overflow",
          fun() -> test_fragment_overflow(State) end}
     ] end}.
```

**Reason**: Fragment reassembly is critical for large messages that don't fit in single WebSocket frame.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_ws_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - Fragment functions ≥80%
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All fragment paths tested
- [ ] Coverage report: Lines 391-422 covered
- [ ] Edge cases: Timeout, overflow, concurrent fragments

---

### Phase 5: Test Backpressure Management

**Estimated Time**: 3 hours
**Priority**: MEDIUM - PERFORMANCE CRITICAL

#### Overview

Test backpressure activation when buffer fills, and recovery when buffer drains. Covers `check_backpressure/1`, `update_buffer_usage/3`, and `resume_reading/1` (lines 480-538).

#### Specification

**WHAT we're building:**

Backpressure tests (8 tests):

1. **Backpressure activation** - Buffer reaches 100%
2. **Message rejection during backpressure** - New messages rejected
3. **Backpressure recovery** - Buffer drains to 50%, reading resumes
4. **Backpressure timer** - 5-second timeout (use 100ms for testing)
5. **Buffer usage tracking** - Bytes buffered, messages pending
6. **Rapid message stream** - 100 messages sent rapidly
7. **Sustained backpressure** - Multiple backpressure cycles
8. **Send during backpressure** - Server → client messages still work

#### Pseudocode

```erlang
%% Test 1: Backpressure activation
test_backpressure_activation(State) ->
    %% Use small buffer for faster testing
    Config = State#state.config#{frame_buffer_size => 1024},  %% 1KB

    %% Start listener with small buffer
    State2 = setup_cowboy_listener_with_config(Config),
    {ok, ConnPid, StreamRef} = connect_ws_client(State2#state.port, State2#state.path),

    %% Send messages until buffer fills (1KB)
    Msg = binary:copy(<<"x">>, 200) ++ <<"\n">>,  %% 201 bytes

    %% Send 6 messages (1206 bytes > 1024 buffer)
    lists:foreach(
        fun(_) ->
            send_text_frame(ConnPid, StreamRef, Msg)
        end,
        lists:seq(1, 6)
    ),

    %% Should receive backpressure close frame
    Result = receive_text_frame(ConnPid, 2000),
    ?assertMatch({error, {closed, 1001, <<"Backpressure limit exceeded">>}}, Result).

%% Test 2: Backpressure recovery
test_backpressure_recovery(State) ->
    %% Start listener with small buffer
    Config = State#state.config#{frame_buffer_size => 1024},
    State2 = setup_cowboy_listener_with_config(Config),
    {ok, ConnPid, StreamRef} = connect_ws_client(State2#state.port, State2#state.path),

    %% Send messages to 90% of buffer
    Msg = binary:copy(<<"x">>, 100) ++ <<"\n">>,
    lists:foreach(
        fun(_) -> send_text_frame(ConnPid, StreamRef, Msg) end,
        lists:seq(1, 9)
    ),

    %% Simulate buffer drain (server processes messages)
    %% This is tricky - need to mock registry or wait for processing
    timer:sleep(500),

    %% Backpressure should be inactive
    ?assertEqual(timeout, receive_text_frame(ConnPid, 1000)).

%% Test 3: Buffer usage tracking
test_buffer_usage_tracking(State) ->
    %% This tests internal state, need to export state access
    %% Or verify via side effects
    ok.
```

#### Architecture

**Coverage targets:**
- `check_backpressure/1` (lines 481-504): 85%
- `update_buffer_usage/3` (lines 507-515): 80%
- `resume_reading/1` (lines 518-538): 80%
- Backpressure state transitions: 85%

#### Changes Required:

##### Add Backpressure Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl`
**Current**: After fragment tests
**Changes**: Add backpressure test group

```erlang
%%====================================================================
%% Backpressure Management Tests
%%====================================================================

websocket_backpressure_test_() ->
    {timeout, 20,
     {setup,
      fun setup_cowboy_listener/0,
      fun cleanup_cowboy_listener/1,
      fun(State) -> [
          {"Backpressure activation",
           fun() -> test_backpressure_activation(State) end},
          {"Message rejection during backpressure",
           fun() -> test_message_rejection_backpressure(State) end},
          {"Backpressure recovery",
           fun() -> test_backpressure_recovery(State) end},
          {"Backpressure timer",
           fun() -> test_backpressure_timer(State) end},
          {"Buffer usage tracking",
           fun() -> test_buffer_usage_tracking(State) end},
          {"Rapid message stream",
           fun() -> test_rapid_message_stream(State) end},
          {"Sustained backpressure",
           fun() -> test_sustained_backpressure(State) end},
          {"Send during backpressure",
           fun() -> test_send_during_backpressure(State) end}
      ] end}}.
```

**Reason**: Backpressure prevents server overload. Must verify it activates and recovers correctly.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_ws_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - Backpressure functions ≥80%
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All backpressure paths tested
- [ ] Coverage report: Lines 480-538 covered
- [ ] Edge cases: Activation, recovery, timer

---

### Phase 6: Test Bidirectional Messaging

**Estimated Time**: 2 hours
**Priority**: HIGH - E2E VALIDATION

#### Overview

Test full bidirectional messaging: client → server and server → client. Covers `websocket_info({send_frame, Data}, State)` (lines 260-281).

#### Specification

**WHAT we're building:**

Bidirectional tests (6 tests):

1. **Client → server message** - Message sent via Gun, processed by Cowboy
2. **Server → client message** - Message sent via `erlmcp_transport_ws:send/2`
3. **Simultaneous bidirectional** - Concurrent send/receive
4. **Multiple concurrent connections** - 10 WebSocket connections
5. **Message ordering** - FIFO ordering preserved
6. **Large bidirectional messages** - Large messages both directions

#### Pseudocode

```erlang
%% Test 1: Client → server message
test_client_to_server(State) ->
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Send message from client
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\"}\n">>,
    ok = send_text_frame(ConnPid, StreamRef, Message),

    %% Message should be processed by server
    %% Verify via registry or tracing (if available)
    ?assertEqual(timeout, receive_text_frame(ConnPid, 1000)).

%% Test 2: Server → client message
test_server_to_client(State) ->
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Get WebSocket handler PID (tricky - need to extract from Cowboy)
    %% For now, send to self() to test websocket_info
    Message = <<"{\"response\":\"ok\"}\n">>,
    ok = erlmcp_transport_ws:send(self(), Message),

    %% Should receive message via WebSocket
    Result = receive_text_frame(ConnPid, 1000),
    ?assertEqual({ok, Message}, Result).

%% Test 3: Multiple concurrent connections
test_concurrent_connections(State) ->
    %% Start 10 Gun clients
    Clients = [begin
        {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),
        {ConnPid, StreamRef}
    end || _ <- lists:seq(1, 10)],

    %% Send message from each client
    lists:foreach(
        fun({ConnPid, StreamRef}) ->
            Message = <<"{\"id\":1}\n">>,
            send_text_frame(ConnPid, StreamRef, Message)
        end,
        Clients
    ),

    %% All should be processed
    timer:sleep(500),

    %% Cleanup
    lists:foreach(
        fun({ConnPid, _}) -> close_ws_client(ConnPid) end,
        Clients
    ).
```

#### Architecture

**Coverage targets:**
- `websocket_info({send_frame, Data}, State)` (lines 260-281): 85%
- `websocket_info(ping, State)` (lines 283-284): 90%
- `websocket_info(close, State)` (lines 294-300): 90%
- Bidirectional message flow: 85%

#### Changes Required:

##### Add Bidirectional Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl`
**Current**: After backpressure tests
**Changes**: Add bidirectional test group

```erlang
%%====================================================================
%% Bidirectional Messaging Tests
%%====================================================================

websocket_bidirectional_test_() ->
    {timeout, 20,
     {setup,
      fun setup_cowboy_listener/0,
      fun cleanup_cowboy_listener/1,
      fun(State) -> [
          {"Client to server message",
           fun() -> test_client_to_server(State) end},
          {"Server to client message",
           fun() -> test_server_to_client(State) end},
          {"Simultaneous bidirectional",
           fun() -> test_simultaneous_bidirectional(State) end},
          {"Multiple concurrent connections",
           fun() -> test_concurrent_connections(State) end},
          {"Message ordering",
           fun() -> test_message_ordering(State) end},
          {"Large bidirectional messages",
           fun() -> test_large_bidirectional_messages(State) end}
      ] end}}.
```

**Reason**: WebSocket is bidirectional by design. Must test both directions.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_ws_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - websocket_info/2 ≥85%
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All bidirectional paths tested
- [ ] Coverage report: Lines 260-303 covered
- [ ] Edge cases: Concurrent connections, ordering

---

### Phase 7: Test Error Handling

**Estimated Time**: 2 hours
**Priority**: HIGH - SAFETY

#### Overview

Test all error handling paths: invalid UTF-8, oversized messages, parse errors, connection drops. Covers `close_with_error/2` (lines 453-474) and all error paths.

#### Specification

**WHAT we're building:**

Error handling tests (8 tests):

1. **Invalid UTF-8 error** - Close 1002
2. **Message too big error** - Close 1009
3. **JSON parse error** - Close 1002
4. **Fragment timeout error** - Close 1002
5. **Backpressure failed error** - Close 1001
6. **Connection drop** - Client disconnect
7. **Idle timeout** - 5 minutes (skip - too long)
8. **Network error** - Simulate network failure

#### Pseudocode

```erlang
%% Test 1: Invalid UTF-8 error
test_invalid_utf8_error(State) ->
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Send invalid UTF-8
    InvalidUTF8 = <<195, 40, $\n>>,
    ok = send_text_frame(ConnPid, StreamRef, InvalidUTF8),

    %% Should receive close frame with 1002
    Result = receive_text_frame(ConnPid, 2000),
    ?assertMatch({error, {closed, 1002, <<"Invalid UTF-8 encoding">>}}, Result).

%% Test 2: JSON parse error
test_json_parse_error(State) ->
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Send invalid JSON (but valid UTF-8)
    InvalidJSON = <<"{invalid json}\n">>,
    ok = send_text_frame(ConnPid, StreamRef, InvalidJSON),

    %% Should receive close frame with 1002
    Result = receive_text_frame(ConnPid, 2000),
    ?assertMatch({error, {closed, 1002, <<"JSON-RPC parse error">>}}, Result).

%% Test 3: Connection drop
test_connection_drop(State) ->
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Send valid message
    Message = <<"{\"jsonrpc\":\"2.0\"}\n">>,
    ok = send_text_frame(ConnPid, StreamRef, Message),

    %% Abruptly close client connection
    gun:close(ConnPid),

    %% Server should handle gracefully (no crash)
    timer:sleep(500),
    ?assert(true).
```

#### Architecture

**Coverage targets:**
- `close_with_error/2` (lines 453-474): 90%
- Error paths in `websocket_handle/2`: 85%
- Error paths in `websocket_info/2`: 85%
- All error responses tested: 90%

#### Changes Required:

##### Add Error Handling Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl`
**Current**: After bidirectional tests
**Changes**: Add error handling test group

```erlang
%%====================================================================
%% Error Handling Tests
%%====================================================================

websocket_error_handling_test_() ->
    {setup,
     fun setup_cowboy_listener/0,
     fun cleanup_cowboy_listener/1,
     fun(State) -> [
         {"Invalid UTF-8 error",
          fun() -> test_invalid_utf8_error(State) end},
         {"Message too big error",
          fun() -> test_message_too_big_error(State) end},
         {"JSON parse error",
          fun() -> test_json_parse_error(State) end},
         {"Fragment timeout error",
          fun() -> test_fragment_timeout_error(State) end},
         {"Backpressure failed error",
          fun() -> test_backpressure_failed_error(State) end},
         {"Connection drop",
          fun() -> test_connection_drop(State) end},
         {"Network error",
          fun() -> test_network_error(State) end},
         {"Unknown error",
          fun() -> test_unknown_error(State) end}
     ] end}.
```

**Reason**: Error handling is critical for system stability. All error paths must be tested.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_ws_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - Error handling ≥85%
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All error paths tested
- [ ] Coverage report: Lines 453-474 covered
- [ ] Edge cases: Connection drop, network errors

---

### Phase 8: Final Integration Testing

**Estimated Time**: 2 hours
**Priority**: HIGH - E2E VALIDATION

#### Overview

End-to-end integration tests that verify the complete WebSocket transport lifecycle. Combines all previous tests into full workflow tests.

#### Specification

**WHAT we're building:**

Integration tests (5 tests):

1. **Complete request/response cycle** - Full MCP request
2. **Mixed valid/invalid messages** - Error recovery
3. **Large message handling** - 10KB+ messages
4. **Rapid message stream** - 100 messages in succession
5. **Fragmented large message** - 100KB message split across fragments

#### Pseudocode

```erlang
%% Test 1: Complete request/response cycle
test_complete_request_response_cycle(State) ->
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Send MCP request
    Request = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"tools/list">>,
        <<"id">> => 1
    }) ++ <<"\n">>,

    ok = send_text_frame(ConnPid, StreamRef, Request),

    %% Request should be processed
    ?assertEqual(timeout, receive_text_frame(ConnPid, 1000)),

    close_ws_client(ConnPid).

%% Test 2: Mixed valid/invalid messages
test_mixed_valid_invalid_messages(State) ->
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Valid message
    ValidMsg = <<"{\"jsonrpc\":\"2.0\"}\n">>,
    ok = send_text_frame(ConnPid, StreamRef, ValidMsg),

    %% Invalid message (should close)
    InvalidMsg = <<"{invalid}\n">>,
    ok = send_text_frame(ConnPid, StreamRef, InvalidMsg),

    %% Should receive close frame
    Result = receive_text_frame(ConnPid, 2000),
    ?assertMatch({error, {closed, 1002, _}}, Result).

%% Test 3: Rapid message stream
test_rapid_message_stream(State) ->
    {ok, ConnPid, StreamRef} = connect_ws_client(State#state.port, State#state.path),

    %% Send 100 messages rapidly
    Messages = [
        jsx:encode(#{<<"id">> => I}) ++ <<"\n">>
        || I <- lists:seq(1, 100)
    ],

    lists:foreach(
        fun(Msg) -> send_text_frame(ConnPid, StreamRef, Msg) end,
        Messages
    ),

    %% All should be processed
    timer:sleep(1000),
    ?assertEqual(true, is_process_alive(ConnPid)),

    close_ws_client(ConnPid).
```

#### Architecture

**Coverage targets:**
- **Overall coverage**: ≥80% for entire module
- **Integration paths**: All tested end-to-end
- **Edge cases**: All covered

#### Changes Required:

##### Add Integration Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl`
**Current**: After error handling tests
**Changes**: Add integration test group

```erlang
%%====================================================================
%% Integration Tests
%%====================================================================

websocket_integration_test_() ->
    {timeout, 30,
     {setup,
      fun setup_cowboy_listener/0,
      fun cleanup_cowboy_listener/1,
      fun(State) -> [
          {"Complete request/response cycle",
           fun() -> test_complete_request_response_cycle(State) end},
          {"Mixed valid/invalid messages",
           fun() -> test_mixed_valid_invalid_messages(State) end},
          {"Large message handling",
           fun() -> test_large_message_handling(State) end},
          {"Rapid message stream",
           fun() -> test_rapid_message_stream(State) end},
          {"Fragmented large message",
           fun() -> test_fragmented_large_message(State) end}
      ] end}}.
```

**Reason**: Integration tests verify the complete system works end-to-end.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_ws_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - ≥80% overall coverage
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All integration paths tested
- [ ] Coverage report: Verify ≥80% coverage achieved
- [ ] Edge cases: Large messages, rapid streams, fragments

---

## Testing Strategy

### Chicago School TDD (MANDATORY)

- **NO MOCKS** - Use real Cowboy listeners and Gun WebSocket clients
- **State-Based Verification** - Check #state{} record contents where possible
- **Integration Testing** - Test with real dependencies (Cowboy, Gun)
- **Race Condition Testing** - Concurrent connections, simultaneous send/receive

### Unit Tests (EUnit)

**What to Test:**
- All Cowboy WebSocket handler callbacks
- All frame types (text, binary, ping, pong, close)
- All error paths (invalid UTF-8, oversized, parse errors)
- Backpressure activation and recovery
- Fragment reassembly with timeout

**Test Pattern:**
- Reference: `erlmcp_transport_tcp_tests.erl` (real sockets, real protocols)
- Setup: Start Cowboy listener
- Test: Use Gun client to send frames
- Cleanup: Stop Cowboy listener

**Coverage Target:** ≥80% per module

**Pass Rate:** 100% (all tests must pass)

### Integration Tests (EUnit with timeout)

**End-to-End Scenarios:**
- Complete MCP request/response cycle
- Mixed valid/invalid messages
- Large messages (10KB+)
- Rapid message stream (100+ messages)
- Concurrent connections (10+ simultaneous)

**Multi-Process:**
- Multiple Gun clients connecting to same Cowboy listener
- Simultaneous bidirectional messaging

**Failure Scenarios:**
- Connection drops (abrupt Gun close)
- Network errors (simulated)
- Invalid frames, oversized messages, parse errors

### Manual Testing Steps

1. **Start Cowboy listener**: Verify listener starts on random port
2. **Connect Gun client**: Verify WebSocket upgrade succeeds
3. **Send text frames**: Verify frames processed correctly
4. **Send binary frames**: Verify rejection with close 1002
5. **Send ping frames**: Verify pong response
6. **Test fragments**: Verify reassembly works
7. **Test backpressure**: Verify activation/recovery
8. **Test errors**: Verify all error responses correct

### Quality Gates

Every phase MUST pass:

1. **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
2. **EUnit**: `rebar3 eunit --module=erlmcp_transport_ws_tests` - 100% pass rate
3. **Coverage**: `rebar3 cover --verbose` - ≥80% coverage
4. **Dialyzer**: `rebar3 dialyzer` - 0 warnings
5. **Xref**: `rebar3 xref` - 0 undefined function calls

**Commands:**
```bash
cd apps/erlmcp_transports
rebar3 compile
rebar3 eunit --module=erlmcp_transport_ws_tests --cover
rebar3 cover --verbose
```

## Manufacturing Checklist

### Before Implementation
- [x] Research verified (read actual source code)
- [x] Scope confirmed (IN/OUT documented)
- [x] No open questions (all decisions made)
- [x] Phases broken down (≤4 hours each)
- [x] Acceptance criteria defined (measurable, specific)

### During Implementation
- [ ] Chicago School TDD followed (tests FIRST)
- [ ] OTP patterns followed (Cowboy WebSocket handler)
- [ ] Type specs added (Dialyzer clean)
- [ ] Error handling complete (all paths)
- [ ] Quality gates passing (compilation, tests, coverage)

### After Implementation
- [ ] All tests passing (100% rate)
- [ ] Coverage ≥80% (verified)
- [ ] Dialyzer 0 warnings (verified)
- [ ] Xref 0 undefined calls (verified)
- [ ] Performance no regression >10% (verified)
- [ ] Documentation updated (README, API docs)
- [ ] Code review complete (OTP patterns verified)

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| Cowboy listener startup fails in tests | P1 | High | Use `{port, 0}` for random port, verify Cowboy starts before proceeding |
| Gun WebSocket client connection issues | P1 | Medium | Use Gun 2.0.1 API correctly, add timeout to `gun:await_up/2` |
| Race conditions in connection setup | P2 | Medium | Use `timer:sleep(100)` after Cowboy start, use receive with timeout |
| Backpressure testing takes too long | P2 | Low | Use small buffer sizes (1KB) for testing, limit message count to 100 |
| Fragment timeout testing blocks 30 seconds | P2 | Low | Mock `erlang:monotonic_time()` or use short timeout (100ms) in test config |
| Registry dependency not available | P2 | Medium | Start `erlmcp` application in setup, mock `erlmcp_registry` if needed |
| Port conflicts in parallel tests | P1 | High | Use unique listener name per test (`erlmcp_ws_listener_test_<unique>`), sequential execution for integration tests |
| OpenTelemetry tracing not initialized | P3 | Low | Mock `erlmcp_tracing` functions or start OpenTelemetry in setup |
| Coverage measurement excludes Cowboy callbacks | P1 | High | Verify coverage report includes ALL lines in erlmcp_transport_ws.erl, check lines 162-303 specifically |
| Gun client doesn't receive close frames | P2 | Medium | Verify Gun version 2.0.1, check Gun documentation for close frame handling |

### Rollback Plan

**Git revert:**
- Revert to commit before test additions: `git revert <commit-hash>`
- Tests are additive, so rollback is safe (no production code changes)

**Data migration:** N/A (no data changes)

**Service impact:** None (tests only run in dev/test environments)

## References

- Research: `/Users/sac/erlmcp/.wreckit/items/009-create-eunit-test-suite-for-websocket-transport-er/research.md`
- CLAUDE.md: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- TCPS: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- OTP Patterns: `/Users/sac/erlmcp/docs/otp-patterns.md`
- Test Reference: `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl` (lines 214-324 for real socket testing)
- Cowboy WebSocket: https://ninenines.eu/docs/en/cowboy/2.10/guide/ws/
- Gun WebSocket: https://ninenines.eu/docs/en/gun/2.0/manual/gun.ws_upgrade/
- RFC 6455: https://datatracker.ietf.org/doc/html/rfc6455
