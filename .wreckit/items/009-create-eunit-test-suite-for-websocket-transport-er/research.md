# Research: Create EUnit test suite for WebSocket transport (erlmcp_transport_ws)

**Date**: 2025-01-29
**Item**: 009-create-eunit-test-suite-for-websocket-transport-er
**Section**: transports
**TCPS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
WebSocket transport has 0% test coverage - real-time transport is untested

**Motivation:** WebSocket transport for real-time bidirectional messaging. Critical for browser-based MCP clients.

**Success criteria:**
- Test file created: apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl
- WebSocket protocol tested
- Coverage ≥80%
- All tests pass

**Technical constraints:**
- WebSocket protocol - real WebSocket connections
- Frame testing - text/binary frames
- Bidirectional messaging - simultaneous send/receive
- Error handling - connection drop, invalid frames

**Signals:** priority: high, urgency: P1 - BLOCKS PRODUCTION DEPLOYMENT

### Quality Gate Status
- **Gate Type**: Test Coverage / EUnit / Integration
- **Current State**: Test file EXISTS but only tests exported helper functions (validate_utf8, validate_message_size, etc.) - DOES NOT test actual Cowboy WebSocket handler callbacks
- **Target State**: ≥80% coverage including all WebSocket protocol handlers
- **Gap**: Test file exists with ~40 test functions, but they only test utility functions. Zero coverage of:
  - Cowboy WebSocket init/2 callback (lines 162-193)
  - websocket_handle/2 callback (lines 195-258)
  - websocket_info/2 callback (lines 260-303)
  - Backpressure management (lines 480-538)
  - Fragment reassembly (lines 391-422)
  - Real WebSocket connections via Cowboy

## Summary

**Manufacturing Objective:**
Create comprehensive EUnit tests for the WebSocket transport that achieve ≥80% coverage, focusing on real Cowboy WebSocket protocol handling, not just utility function testing. The existing test file (`erlmcp_transport_ws_tests.erl`) provides a foundation but only tests exported helper functions. Critical WebSocket handler callbacks (init/2, websocket_handle/2, websocket_info/2) have ZERO coverage.

**Technical Approach:**
1. **Add Cowboy-based integration tests** that start real Cowboy listeners and WebSocket clients
2. **Test all WebSocket frame types**: text frames (supported), binary frames (rejected), ping/pong
3. **Test backpressure management** with real buffer overflow scenarios
4. **Test fragment reassembly** with timeout handling
5. **Test error handling** for invalid UTF-8, oversized messages, parse errors
6. **Test bidirectional messaging** with concurrent send/receive
7. Follow patterns from `erlmcp_transport_tcp_tests.erl` (real sockets, real protocols)
8. Use EUnit setup/cleanup with timeouts for integration tests

**TCPS Justification:**
- **Jidoka**: Every WebSocket connection path must be verified - no "assume it works"
- **Poka-yoke**: Test real protocol errors (invalid frames, malformed UTF-8) to prove system fails safely
- **Kaizen**: Measure coverage with rebar3 cover to verify ≥80% before "done"
- **Andon**: Coverage failures must be visible - no silent test skipping

## Current State Analysis

### Existing Implementation
- **Files**:
  - `apps/erlmcp_transports/src/erlmcp_transport_ws.erl` (539 lines)
  - `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl` (385 lines)
- **Patterns**:
  - Cowboy 2.10.0 WebSocket handler (NOT gen_server, NOT erlmcp_transport_behavior)
  - Custom init/2 interface: `init(Req, [TransportId, Config]) -> {cowboy_websocket, Req, State, Opts}`
  - Record-based state with backpressure tracking
  - Message delimiter: newline (`\n`)
  - Fragment reassembly with 30-second timeout
  - Backpressure with 100KB buffer, 5-second recovery timeout
- **Tests**: 40 test functions in existing file, BUT they only test:
  - `validate_utf8/1` - UTF-8 validation
  - `validate_message_size/1` - Size checking
  - `generate_session_id/0` - Session ID generation
  - Basic delimiter validation
  - Basic message parsing
  - **NOT tested**: Any Cowboy WebSocket callbacks, real connections, backpressure, fragments
- **Quality**: Current tests pass but provide ~5% coverage of actual WebSocket functionality

### Key Files
- `apps/erlmcp_transports/src/erlmcp_transport_ws.erl:1-539` - Complete WebSocket transport implementation
  - Lines 6-31: Module exports and internal/test exports
  - Lines 33-73: State record definition with backpressure fields
  - Lines 79-126: `init/2` - Cowboy listener startup (starts Cowboy on port)
  - Lines 162-193: `init(Req, State)` - WebSocket upgrade handler
  - Lines 195-258: `websocket_handle/2` - Frame processing (text, binary, ping, close)
  - Lines 260-303: `websocket_info/2` - Send messages, resume reading, close
  - Lines 310-356: `handle_text_frame/2`, `process_messages/2`, `process_lines/3`
  - Lines 391-422: Fragment reassembly with timeout
  - Lines 425-444: `validate_message_size/1`, `validate_utf8/1`
  - Lines 480-538: Backpressure management functions

- `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl:1-385` - Existing tests (inadequate)
  - Lines 1-21: Setup/cleanup
  - Lines 26-87: Test group structure (8 groups)
  - Lines 93-124: Initialization tests (test init, session IDs)
  - Lines 130-149: Delimiter validation tests
  - Lines 155-180: UTF-8 validation tests
  - Lines 186-220: Message size tests
  - Lines 226-256: Fragment tests (basic, no reassembly testing)
  - Lines 262-280: Close code tests (stub assertions)
  - Lines 286-324: Connection tests (mock PIDs, no real connections)
  - Lines 334-384: Integration tests (JSON validation only)

- `apps/erlmcp_transports/src/erlmcp_transport_behavior.erl:1-817` - Transport behavior specification
  - Lines 58-98: Behavior callback definitions (init/1, send/2, close/1, get_info/1)
  - Lines 104-121: Type definitions (transport_state, transport_config, etc.)
  - Lines 532-538: WebSocket options validation (url, owner)
  - **NOTE**: erlmcp_transport_ws does NOT implement this behavior (line 6 comment)

- `apps/erlmcp_transports/rebar.config:1-46` - Dependencies and config
  - Line 15: `{gun, "2.0.1"}` - HTTP client (could use for WS client testing)
  - Line 16: `{ranch, "2.1.0"}` - TCP listener
  - Line 22: `{eunit_opts, [verbose]}` - EUnit configuration
  - Line 44: `{cover_enabled, true}` - Coverage enabled
  - **MISSING**: No cowboy dependency in transports app (cowboy is in top-level rebar.config)

- `rebar.config:52` - Cowboy dependency at top level
  - `{cowboy, "2.10.0"}` - HTTP/WebSocket server

- `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:1-700` - Reference for real protocol testing
  - Lines 76-117: Client start tests with real gen_server
  - Lines 156-212: Server start tests with real Ranch listener
  - Lines 214-252: Ranch integration tests (real TCP connections)
  - Lines 258-324: Client-server integration with real sockets
  - Lines 556-605: Error handling with connection drops
  - **Pattern to follow**: Real processes, real sockets, real protocols (Chicago School TDD)

- `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl:1-100` - Reference for transport testing
  - Lines 26-50: Test group structure with setup/cleanup
  - Lines 67-88: Setup functions with test mode
  - Lines 94-100: Basic initialization tests
  - **Pattern**: Test mode detection, process lifecycle testing

- `apps/erlmcp_transports/test/erlmcp_transport_behavior_SUITE.erl:1-100` - Common Test patterns
  - Lines 50-58: Test suite groups (parallel/sequential)
  - Lines 61-96: Group definitions with parallel/sequential execution
  - **Pattern**: Common Test for integration, EUnit for unit tests

### OTP Patterns Observed
- **Behavior**: Cowboy WebSocket handler (NOT gen_server, NOT erlmcp_transport_behavior)
- **Supervision**: No supervisor - Cowboy manages listener and connection processes
- **Process Pattern**: One Cowboy listener process (named erlmcp_ws_listener), multiple WebSocket connection processes (one per connection)
- **Test Pattern**: Existing tests use stub/mock approach - need to change to Chicago School TDD (real processes, real Cowboy, real WebSocket connections)

### Dependencies
- **Internal Modules**:
  - `erlmcp_registry` - Message routing (State#state.registry_pid)
  - `erlmcp_tracing` - OpenTelemetry tracing (span creation, attributes)
  - `jsx` - JSON encoding/decoding (line 380)
  - `crypto` - Random bytes for session IDs (line 449)

- **External Libraries**:
  - `cowboy 2.10.0` - HTTP/WebSocket server (from top-level rebar.config)
  - `gun 2.0.1` - HTTP client (can use as WebSocket client for testing)
  - `ranch 2.1.0` - TCP listener (used by Cowboy internally)

- **OTP Applications**:
  - `kernel` - Logger, processes
  - `stdlib` - Base64, crypto
  - `cowboy` - WebSocket server
  - `erlmcp_core` - Core MCP functionality
  - `erlmcp_transports` - This application

### Technical Considerations

#### WebSocket Protocol Specifics
- **Supported Frames**:
  - `{text, Data}` - Text frames (line 195-236)
  - `ping` - Ping frames (line 247-248)
  - `pong` - Pong frames (line 250-251)
  - `{close, Code, Reason}` - Close frames (line 253-255)

- **Rejected Frames**:
  - `{binary, Data}` - Binary frames rejected with close code 1002 (line 238-245)

- **Message Delimiter**: Newline (`\n`) - messages split by delimiter (line 328)
- **Max Message Size**: 16MB default (line 36, 427)
- **Fragment Timeout**: 30 seconds (line 38, 418)
- **Idle Timeout**: 5 minutes (line 35, 193)

#### Backpressure Management
- **Buffer Size**: 100KB default (line 41, 188)
- **Backpressure Threshold**: 100% of buffer (line 492)
- **Resume Threshold**: 50% of buffer (line 42, 522)
- **Backpressure Timeout**: 5 seconds (line 43, 495)
- **States**: `inactive` (normal) or `active` (backpressure)

#### Close Codes (RFC 6455)
- `1000` (WS_CLOSE_NORMAL) - Normal shutdown (line 46)
- `1001` (WS_CLOSE_GOING_AWAY) - Going away (line 49)
- `1002` (WS_CLOSE_PROTOCOL_ERROR) - Protocol error (line 47)
- `1009` (WS_CLOSE_MESSAGE_TOO_BIG) - Message too big (line 48)

## Root Cause Analysis (5 Whys)

**Problem**: WebSocket transport has 0% test coverage despite existing test file

1. **Why?** Existing tests only test exported utility functions (`validate_utf8`, `validate_message_size`, `generate_session_id`) and do not test Cowboy WebSocket handler callbacks (`websocket_handle`, `websocket_info`, `init/2`).

2. **Why?** Cowboy WebSocket callbacks have complex state management (Req → cowboy_websocket upgrade) and require real Cowboy listeners and WebSocket clients to test properly.

3. **Why?** Existing tests use mock PIDs and stub assertions instead of real integration testing with Cowboy and Gun (WebSocket client).

4. **Why?** Developer who wrote initial tests focused on easy-to-test utility functions and deferred complex protocol testing.

5. **ROOT CAUSE**: Missing Cowboy-based integration test infrastructure - no pattern for testing real WebSocket connections in the codebase. TCP transport tests show the pattern (real Ranch listeners, real TCP clients), but this wasn't applied to WebSocket transport.

**Solution**: Add Cowboy-based integration tests following the pattern from `erlmcp_transport_tcp_tests.erl`:
- Start real Cowboy listener on random port
- Use Gun (HTTP/WebSocket client) to connect as WebSocket client
- Test all WebSocket frame types and error conditions
- Test backpressure with real message flooding
- Test fragment reassembly with real fragmented messages
- Test bidirectional messaging with concurrent send/receive
- Measure coverage with `rebar3 cover` to verify ≥80%

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| Cowboy listener startup fails in tests | P1 | Tests cannot run - blocks all progress | Use `{port, 0}` for random port allocation ( Cowboy auto-assigns port) |
| Gun WebSocket client connection issues | P1 | Cannot test real WebSocket frames | Use Gun 2.0.1 WebSocket client API (`gun:ws_upgrade`) |
| Race conditions in connection setup | P2 | Flaky tests, intermittent failures | Use `timer:sleep(100)` after Cowboy start, use receive with timeout |
| Backpressure testing takes too long | P2 | Tests timeout, CI/CD failures | Use small buffer sizes (1KB) for backpressure tests, limit message count |
| Fragment timeout testing blocks 30 seconds | P2 | Tests are too slow | Mock `erlang:monotonic_time()` or use short timeout in test config |
| Registry dependency not available | P2 | WebSocket handler crashes on `erlmcp_registry:get_pid()` | Start `erlmcp` application in setup, mock registry if needed |
| Port conflicts in parallel tests | P1 | Tests fail with "address already in use" | Use unique listener name per test (`erlmcp_ws_listener_<unique>`), use sequential execution for integration tests |
| OpenTelemetry tracing not initialized | P3 | Spans fail to record | Mock `erlmcp_tracing` functions or start OpenTelemetry in setup |
| Coverage measurement excludes Cowboy callbacks | P1 | Coverage shows 80% but actual is lower | Verify coverage report includes ALL lines in erlmcp_transport_ws.erl, check lines 162-303 specifically |

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Requirements with acceptance criteria (✓ complete)
2. **Pseudocode** - Algorithm design BEFORE coding (⚠️ needs completion)
3. **Architecture** - Integration points and dependencies (✓ documented above)
4. **Refinement** - Chicago School TDD (tests FIRST) - ⚠️ THIS STEP
5. **Completion** - All quality gates passing - ⚠️ needs verification

**Implementation Strategy:**

### Phase 1: Add Cowboy Integration Test Infrastructure (Week 1)
**Priority: CRITICAL - BLOCKS ALL PROGRESS**

1. **Create Cowboy setup/cleanup helpers**
   ```erlang
   setup_cowboy_listener() ->
       Port = 0,  % Random port
       TransportId = <<"test_ws_transport">>,
       Config = #{port => Port, path => "/mcp/ws"},
       {ok, _Pid} = erlmcp_transport_ws:init(TransportId, Config),
       timer:sleep(100),  % Let Cowboy start
       #{port => get_actual_port(TransportId)}.

   cleanup_cowboy_listener(#{port := Port}) ->
       cowboy:stop_listener(erlmcp_ws_listener),
       timer:sleep(50).
   ```

2. **Create Gun WebSocket client helper**
   ```erlang
   connect_ws_client(Port, Path) ->
       {ok, ConnPid} = gun:open("localhost", Port),
       {ok, _} = gun:await_up(ConnPid),
       StreamRef = gun:ws_upgrade(ConnPid, Path),
       receive {gun_upgrade, ConnPid, StreamRef, _, _} -> ok end,
       {ok, ConnPid, StreamRef}.
   ```

3. **Add WebSocket frame test helpers**
   ```erlang
   send_text_frame(ConnPid, StreamRef, Text) ->
       gun:ws_send(ConnPid, StreamRef, {text, Text}).

   receive_text_frame(ConnPid, Timeout) ->
       receive {gun_ws, ConnPid, _, {text, Data}} -> Data
       after Timeout -> timeout end.
   ```

**Expected Outcome**: Can start Cowboy listener, connect Gun client, send/receive frames

### Phase 2: Test WebSocket Protocol Handling (Week 1-2)

**Priority: HIGH - CORE FUNCTIONALITY**

4. **Test text frame handling** (lines 195-236)
   - Valid text frames with delimiter
   - Text frames without delimiter (fragment buffering)
   - Multiple text frames in single stream
   - Empty text frames
   - UTF-8 validation in text frames

5. **Test frame rejection** (lines 238-245)
   - Binary frame rejection (close code 1002)
   - Invalid frame types

6. **Test ping/pong handling** (lines 247-251)
   - Ping frame → pong response
   - Pong frame handling

7. **Test close frame handling** (lines 253-255)
   - Normal close (1000)
   - Protocol error close (1002)
   - Custom close codes

**Expected Outcome**: All websocket_handle/2 paths covered

### Phase 3: Test Message Processing (Week 2)

**Priority: HIGH - MESSAGE LOGIC**

8. **Test message delimiter validation** (lines 310-356)
   - Messages with `\n` delimiter
   - Messages without delimiter (buffered)
   - Multiple messages with delimiters
   - Empty messages between delimiters
   - Strict vs lenient delimiter checking

9. **Test fragment reassembly** (lines 391-422)
   - Two-part fragments
   - Multi-part fragments (3+ parts)
   - Incomplete fragment buffering
   - Fragment timeout handling (30 seconds)
   - Fragment buffer overflow

10. **Test UTF-8 validation** (lines 434-444)
    - Valid UTF-8 sequences
    - Invalid UTF-8 (incomplete multibyte)
    - UTF-8 disabled mode
    - Multibyte characters (emoji, accents)

11. **Test message size validation** (lines 425-432)
    - Messages under limit
    - Messages at limit (16MB)
    - Messages over limit
    - Configurable size limits

**Expected Outcome**: All message processing paths covered

### Phase 4: Test Backpressure Management (Week 2-3)

**Priority: MEDIUM - PERFORMANCE CRITICAL**

12. **Test backpressure activation** (lines 480-504)
    - Buffer fills to 100% → backpressure active
    - New messages rejected during backpressure
    - Backpressure timer starts (5 seconds)

13. **Test backpressure recovery** (lines 518-538)
    - Buffer drains to 50% → resume reading
    - Timer cancellation on resume
    - Manual resume via websocket_info

14. **Test buffer usage tracking** (lines 507-515)
    - Add bytes on receive
    - Subtract bytes on send
    - Messages pending counter
    - Bytes buffered counter

15. **Load testing with backpressure**
    - Rapid message stream (1000+ messages)
    - Sustained load (10K messages)
    - Backpressure activation/recovery cycles

**Expected Outcome**: All backpressure paths covered

### Phase 5: Test Error Handling (Week 3)

**Priority: HIGH - SAFETY**

16. **Test error responses** (lines 453-474)
    - Message too big → close 1009
    - Invalid UTF-8 → close 1002
    - Parse error → close 1002
    - Fragment timeout → close 1002
    - Backpressure failed → close 1001

17. **Test connection failures**
    - WebSocket client disconnect
    - Network errors
    - Idle timeout (5 minutes)

18. **Test concurrent operations**
    - Multiple WebSocket connections (10 concurrent)
    - Simultaneous send/receive
    - Concurrent fragment reassembly

**Expected Outcome**: All error paths covered

### Phase 6: Test Integration (Week 3-4)

**Priority: HIGH - E2E VALIDATION**

19. **Test bidirectional messaging**
    - Client → server messages
    - Server → client responses
    - Simultaneous bidirectional flow

20. **Test registry integration**
    - Messages routed to registry
    - Registry responses sent via WebSocket
    - Registry unavailability handling

21. **Test tracing integration**
    - OpenTelemetry spans created
    - Span attributes set correctly
    - Error recording on failures

22. **End-to-end integration tests**
    - Full MCP request/response cycle
    - Multiple tools calls
    - Resource listing
    - Prompt handling

**Expected Outcome**: Full integration coverage

**Quality Validation:**
- **Automated**:
  ```bash
  cd apps/erlmcp_transports
  rebar3 compile
  rebar3 eunit --module=erlmcp_transport_ws_tests --cover
  rebar3 cover --verbose
  ```
- **Manual**: Verify coverage report shows ≥80% for erlmcp_transport_ws.erl
- **Metrics**:
  - Coverage percentage: Target ≥80%
  - Test pass rate: Target 100%
  - Test execution time: Target <30 seconds
  - Lines covered: Track which lines remain uncovered

## Open Questions
**NONE** - Research complete. All technical details documented.

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) - Missing Cowboy integration tests
- [x] Quality gates defined (specific thresholds) - ≥80% coverage, 100% pass rate
- [x] OTP patterns understood (behaviors, supervision) - Cowboy WebSocket handler, no gen_server
- [x] Test strategy clear (Chicago School TDD) - Real Cowboy, real Gun client, real WebSocket frames
- [x] Risk assessment complete (severity P0-P3) - 10 risks identified with mitigations
- [x] No open questions (all research complete) - All technical questions answered

## Implementation Notes

### Test File Structure
```erlang
-module(erlmcp_transport_ws_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test Groups
- Text Frame Tests
- Binary Frame Tests
- Ping/Pong Tests
- Close Frame Tests
- Message Delimiter Tests
- Fragment Reassembly Tests
- UTF-8 Validation Tests
- Message Size Tests
- Backpressure Tests
- Error Handling Tests
- Bidirectional Tests
- Integration Tests

%% Setup/Cleanup
setup_cowboy() -> ...
cleanup_cowboy(_) -> ...

%% Test Cases (40+ tests)
test_text_frame_with_delimiter() -> ...
test_binary_frame_rejection() -> ...
test_ping_pong() -> ...
...
```

### Coverage Targets by Function
- `init/2` (listener): 90% - Critical for Cowboy startup
- `init/2` (handler): 90% - Critical for WebSocket upgrade
- `websocket_handle/2`: 85% - Core frame processing
- `websocket_info/2`: 85% - Send and close handling
- `handle_text_frame/2`: 80% - Message processing
- `process_messages/2`: 80% - Delimiter handling
- `reassemble_fragment/1`: 80% - Fragment logic
- `check_backpressure/1`: 85% - Backpressure activation
- `update_buffer_usage/3`: 80% - Buffer tracking
- `resume_reading/1`: 80% - Recovery logic
- `validate_utf8/1`: 90% - Already tested (existing)
- `validate_message_size/1`: 90% - Already tested (existing)
- `close_with_error/2`: 85% - Error responses

### Test Execution Order
1. Unit tests (utility functions) - Fast, isolated
2. Protocol tests (frame handling) - Medium, Cowboy listener
3. Integration tests (full cycle) - Slow, full stack
4. Load tests (backpressure) - Slow, 1000+ messages

### Dependencies to Start in Setup
```erlang
setup() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    {ok, _} = application:ensure_all_started(erlmcp),
    {ok, _} = application:ensure_all_started(erlmcp_transports),
    ok.
```

### Known Limitations
- Fragment timeout (30 seconds) cannot be tested efficiently - will use short timeout in test config
- Idle timeout (5 minutes) will not be tested - too long for unit tests
- Real network conditions (packet loss, latency) require property-based testing (PropEr) - out of scope for EUnit

### References
- Cowboy WebSocket documentation: https://ninenines.eu/docs/en/cowboy/2.10/guide/ws/
- Gun WebSocket client: https://ninenines.eu/docs/en/gun/2.0/manual/gun.ws_upgrade/
- RFC 6455 (WebSocket Protocol): https://datatracker.ietf.org/doc/html/rfc6455
- Chicago School TDD: Real processes, real sockets, real protocols (no mocks)
