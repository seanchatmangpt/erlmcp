# Research: SSE Transport Test Coverage

**Date**: 2025-01-09
**Item**: 034-sse-transport-test-coverage
**Section**: transports
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
Server-Sent Events transport has ZERO coverage despite being critical for real-time event streaming from server to client.

**Motivation:** SSE transport enables server-to-client streaming for real-time events. Important for monitoring and notification use cases.

**Success criteria:**
- Test file created: apps/erlmcp_transports/test/erlmcp_transport_sse_tests.erl
- SSE protocol compliance tested
- Coverage: ≥80%
- All tests pass

**Technical constraints:**
- SSE Protocol - Correct event formatting
- Stream Testing - Continuous event streaming
- Keep-Alive - Automatic keep-alive messages
- Error Handling - Stream interruption, reconnect

**Signals:** priority: high, urgency: P1 - BLOCKS PRODUCTION DEPLOYMENT

### Quality Gate Status
- **Gate Type**: Test Coverage
- **Current State**: 0% meaningful coverage (test file exists with stub tests, actual protocol validation missing)
- **Target State**: ≥80% coverage of erlmcp_transport_sse.erl (504 lines)
- **Gap**: 80 percentage points (0% → 80%)

## Summary

The SSE transport implementation is a critical component for real-time server-to-client event streaming in the erlmcp system. However, despite having a test file at `apps/erlmcp_transports/test/erlmcp_transport_sse_tests.erl`, the transport has ZERO meaningful test coverage. The existing tests are stub implementations that don't validate SSE protocol compliance, stream behavior, or error handling.

The manufacturing objective is to implement comprehensive tests following Chicago School TDD principles (real processes, no mocks) that validate the SSE transport's conformance to the WHATWG Server-Sent Events specification. This includes testing event formatting (id:, data:, event:, retry: fields), keep-alive ping messages, stream resumption using Last-Event-ID headers, connection lifecycle management, and error scenarios.

The technical approach must follow OTP patterns observed in other transport tests (especially `erlmcp_transport_tcp_tests.erl`), using real Cowboy HTTP server processes, actual TCP connections, and proper gen_server testing patterns. Tests must cover all public functions: init/2, send/2, close/1, and the Cowboy handler callbacks (init/3, handle/2, terminate/3).

The TCPS justification is Jidoka (built-in quality) - this is a P1 blocker that prevents production deployment because unvalidated SSE protocol implementation could cause silent failures, incorrect event delivery, or connection leaks in production. The transport is currently used in tcps_dashboard_sse_handler.erl but lacks quality validation, violating the "zero defects" principle.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_sse.erl` (504 lines)
    - Lines 10-14: Public API exports (init/2, send/2, close/1)
    - Lines 16-21: Cowboy handler exports (init/3, handle/2, terminate/3)
    - Lines 23-35: Internal record definitions and constants
    - Lines 41-103: Transport behavior implementation (init, send, close)
    - Lines 109-330: Cowboy HTTP handler with SSE streaming
    - Lines 335-503: Internal helper functions
  - `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_sse_event_store.erl` (51 lines)
    - **CRITICAL**: This is a STUB implementation returning {error, not_implemented}
    - Lines 37-38: All handle_call clauses return {error, not_implemented}
    - Lines 298, 366: Called by erlmcp_transport_sse but will fail at runtime
  - `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_sse_tests.erl` (124 lines)
    - **EXISTING BUT INEFFECTIVE**: Test file exists with stub tests
    - Lines 9-14: Setup/cleanup fixtures
    - Lines 20-35: Test generator (9 tests declared)
    - Lines 41-123: Individual test functions (mostly assertions without actual testing)

- **Patterns**:
  - **NOT a standard gen_server**: Implements Cowboy HTTP handler behavior
  - **Dual interface**: Transport behavior API (init/send/close) + Cowboy handler callbacks
  - **State record**: #sse_state with transport_id, client_id, session_id, ping_timer, event_number, last_received_event_id
  - **Message loop**: sse_event_loop/3 handles ping, send_event, close messages with 5-minute timeout
  - **Event store dependency**: Calls erlmcp_sse_event_store:add_event/3 and get_events_since/2 (both stubs)

- **Tests**: 0% meaningful coverage
  - Test file exists but tests don't validate actual SSE protocol behavior
  - test_init_sse/0: Only checks if PID is returned, doesn't verify Cowboy listener started
  - test_send_event/0: Spawns dummy process, doesn't test actual SSE streaming
  - test_format_sse_event/0: Hardcoded assertion, doesn't call format_sse_event/1
  - test_keepalive_ping/0: Checks if binary exists, doesn't verify 30-second interval
  - No tests for: SSE protocol compliance, stream resumption, Cowboy handler lifecycle, error paths

- **Quality**:
  - **Gate Status**: FAIL - 0% coverage (≥80% required)
  - **Compilation**: PASS (module compiles without errors)
  - **Runtime**: CRITICAL FAILURE - erlmcp_sse_event_store calls will crash (returns {error, not_implemented})
  - **Dialyzer**: Unknown (no reports available)
  - **Xref**: Unknown (SSE functions not in xref_ignores list)

### Key Files

**Implementation Files:**
- `apps/erlmcp_transports/src/erlmcp_transport_sse.erl:41-73` - init/2: Starts Cowboy clear listener with SSE route
- `apps/erlmcp_transports/src/erlmcp_transport_sse.erl:75-103` - send/2, close/1: Client process message passing
- `apps/erlmcp_transports/src/erlmcp_transport_sse.erl:121-238` - handle/2: Main request handler with GET (stream) and POST (message) support
- `apps/erlmcp_transports/src/erlmcp_transport_sse.erl:285-322` - sse_event_loop/3: Core event streaming loop with ping, send, close, timeout
- `apps/erlmcp_transports/src/erlmcp_transport_sse.erl:335-344` - format_sse_event/1, format_sse_event_with_id/2: SSE protocol formatting
- `apps/erlmcp_core/src/erlmcp_sse_event_store.erl:37-38` - **CRITICAL BLOCKER**: All gen_server calls return {error, not_implemented}

**Test Files (Pattern References):**
- `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:27-58` - Setup/cleanup fixtures for client/server modes
- `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:76-117` - Client/server start tests with proper gen_server:call(get_state)
- `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:258-324` - Integration test with real TCP connections
- `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:555-605` - Error handling test with connection close/reconnect
- `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl:8-99` - HTTP transport test patterns (simpler, no real server)

**Dependencies:**
- `apps/erlmcp_transports/src/erlmcp_http_header_validator.erl` - HTTP header validation (MUST check if exists)
- `apps/erlmcp_origin_validator.erl` - Origin validation for DNS rebinding protection (MUST check if exists)
- `apps/erlmcp_core/src/erlmcp_registry.erl` - Registry integration for transport registration
- `apps/erlmcp_core/src/erlmcp_sse_event_store.erl` - **CRITICAL**: Event store (stub implementation, blocks testing)

**Documentation:**
- `docs/SSE_IMPLEMENTATION_GUIDE.md:1-429` - Complete SSE resumability specification
- `docs/quality-gates/V2.1_QUALITY_GATE_REPORT.md:244-247` - SSE event store functions listed as "not implemented"
- `COWBOY_HTTP_AUDIT.md:597` - SSE handler usage reference

### OTP Patterns Observed

- **Behavior**: Cowboy HTTP Handler (NOT gen_server)
  - Implements init/3, handle/2, terminate/3 from cowboy_handler
  - State record: #sse_state{} with connection lifecycle tracking
  - Message loop in handle/2 via sse_event_loop/3 with receive/after

- **Supervision**: Not directly supervised as gen_server
  - Started via cowboy:start_clear/3 (listener supervision)
  - HTTP handler processes are transient, supervised by cowboy

- **Process Pattern**: Process-per-connection (HTTP request = new process)
  - Each GET /mcp/sse request spawns handler process
  - Long-lived process (up to 5-minute idle timeout)
  - Client PID stored in #sse_state{} for send/2 message passing

- **Test Pattern**: Chicago School TDD (must follow)
  - **Reference**: `erlmcp_transport_tcp_tests.erl:258-324` (integration test)
  - Real processes: Start actual Cowboy listener, connect with HTTP client
  - No mocks: Use real HTTP requests, TCP sockets, gen_server calls
  - State inspection: gen_server:call(Pid, get_state) to verify internal state
  - Cleanup: Explicit process termination with timer:sleep for race condition prevention

- **Transport Behavior**: Partial implementation
  - **Reference**: `erlmcp_transport_behavior.erl:104-184` (callback specs)
  - SSE transport deviates: init/2 returns {ok, pid()} instead of {ok, State}
  - send/2 sends message to client PID instead of writing to socket
  - Does NOT implement get_info/1 or handle_transport_call/2

## Technical Considerations

### Dependencies

**Internal Modules:**
- `erlmcp_registry` (registry integration at line 204-207, 269)
- `erlmcp_http_header_validator` (lines 163, 245) - **MUST VERIFY EXISTS**
- `erlmcp_origin_validator` (lines 416, 438) - **MUST VERIFY EXISTS**
- `erlmcp_sse_event_store` (lines 298, 366) - **CRITICAL BLOCKER: Stub returns {error, not_implemented}**
- `erlmcp_http_delete_handler` (line 143) - **MUST VERIFY EXISTS**
- `erlmcp_tracing` (OpenTelemetry spans throughout) - **MUST VERIFY EXISTS**

**External Libraries:**
- `cowboy` (2.10.0) - HTTP server
  - cowboy_router:compile/1 (line 54)
  - cowboy:start_clear/3 (line 61)
  - cowboy_req:header/2-3 (lines 126, 176)
  - cowboy_req:method/1 (line 137)
  - cowboy_req:reply/2-4 (lines 129, 145, 168, 200, 251, 260, 272, 397)
  - cowboy_req:read_body/1 (line 254)
  - cowboy_req:stream_body/2 (lines 289, 302, 313, 320, 376)
- `jsx` (3.1.0) - JSON encoding/decoding (lines 131, 257)
- `opentelemetry_api` (1.5.0) - Tracing (included in header)

**OTP Applications:**
- kernel (for timer, processes)
- stdlib (for binary handling)
- cowboy (HTTP server)

### TCPS Quality Gates to Pass

- [ ] **Compilation**: 0 errors
  - Current: PASS (erlmcp_transport_sse.erl compiles)
  - Risk: Runtime crashes from undefined erlmcp_sse_event_store functions

- [ ] **EUnit**: 100% pass rate
  - Current: UNKNOWN (stub tests likely pass but don't validate)
  - Required: All 9+ tests must pass with real assertions

- [ ] **Common Test**: Not applicable (unit tests only)

- [ ] **Coverage**: ≥80%
  - Current: 0% meaningful coverage
  - Target: Cover all functions in erlmcp_transport_sse.erl (504 lines)
  - Gap: 80 percentage points

- [ ] **Dialyzer**: 0 warnings
  - Current: UNKNOWN (no report available)
  - Risk: Type mismatches in record access, function calls

- [ ] **Xref**: 0 undefined function calls
  - Current: UNKNOWN (SSE functions not in xref_ignores)
  - Must add xref ignores for:
    - erlmcp_sse_event_store:add_event/3
    - erlmcp_sse_event_store:get_events_since/2
    - erlmcp_http_header_validator:validate_request_headers/2
    - erlmcp_origin_validator:validate_origin/2
    - erlmcp_http_delete_handler:handle_delete/3
    - opentelemetry_api functions

- [ ] **Performance**: <10% regression from baseline
  - Not applicable (no baseline for SSE transport)

### Patterns to Follow

**Gen Server Pattern** (not applicable - SSE is Cowboy handler):
- **Reference**: N/A - SSE transport uses Cowboy handler, not gen_server

**Test Pattern** (Chicago School TDD):
- **Reference file**: `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:27-70`
  - Setup/cleanup with application:ensure_all_started/1
  - Owner = self() for message passing
  - Config maps with required fields
  - Unique IDs for test isolation (erlang:unique_integer([positive]))
  - timer:sleep/1 for race condition prevention
- **Reference file**: `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:258-324`
  - Real server startup with port 0 (random port assignment)
  - Real client connection to localhost
  - Message passing verification with receive/after
  - State inspection via gen_server:call(Pid, get_state)
  - Explicit cleanup with unlink/exit + timer:sleep

**Error Handling Pattern**:
- **Reference file**: `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:555-605`
  - Test connection close → transport_disconnected message
  - Verify reconnect attempts incremented
  - Verify connection state updated
  - Timeout handling with receive/after

**SSE-Specific Testing** (must implement):
- **Event format validation**: Parse output to verify "id:", "data:", "event:", "retry:" fields
- **Keep-alive testing**: Verify ping sent every 30 seconds (?PING_INTERVAL)
- **Stream timeout**: Verify 5-minute timeout (300000ms) with retry hint
- **Resumption testing**: Use Last-Event-ID header to replay events
- **Concurrent streams**: Multiple simultaneous GET requests to /mcp/sse

**Type Specs** (Dialyzer compliance):
- **Reference file**: `apps/erlmcp_transports/src/erlmcp_transport_sse.erl:41-50`
  - Spec exists for init/2, send/2, close/1
  - Must ensure all helper functions have specs
  - Must validate spec correctness with actual implementations

## Root Cause Analysis (5 Whys)

**Problem**: SSE transport has 0% test coverage despite having test file, blocking production deployment.

1. **Why?** The existing test file (`erlmcp_transport_sse_tests.erl`) contains stub tests that don't validate actual SSE protocol behavior, only checking trivial assertions (e.g., "is binary", "is pid").

2. **Why?** The tests were likely created as placeholders without implementing actual test logic, due to complexity of testing Cowboy HTTP handlers and SSE streaming protocol.

3. **Why?** Testing SSE requires real HTTP server processes, protocol-specific validation (event format, keep-alive timing), and handling of long-lived connections - all more complex than simple function unit tests.

4. **Why?** The SSE event store dependency (`erlmcp_sse_event_store`) is a stub implementation that returns {error, not_implemented} for all calls, which would cause runtime failures if tests actually executed the code paths.

5. **Why?** The SSE transport was implemented with resumability features (event IDs, Last-Event-ID headers, event store integration) but the supporting infrastructure (event store implementation, validation modules) was not completed, creating a dependency chain that blocks testing.

**Solution**: Implement proper EUnit tests following Chicago School TDD patterns (real Cowboy HTTP server, actual HTTP requests, no mocks), while simultaneously stubbing or implementing missing dependencies (erlmcp_sse_event_store functions, validation modules) to unblock testing. Focus on protocol compliance validation first (event format, headers, timeouts) before complex resumability scenarios.

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| Missing dependencies (erlmcp_http_header_validator, erlmcp_origin_validator, erlmcp_http_delete_handler) cause compilation failures | P0 (Critical) | Tests cannot run, blocking all SSE testing | Search codebase for these modules; if missing, create stub implementations or mock out in tests before testing |
| erlmcp_sse_event_store returns {error, not_implemented} causing runtime crashes in sse_event_loop/3 | P0 (Critical) | SSE stream crashes immediately when sending events | Implement minimal event store stub with in-memory ETS table to support add_event/3 and get_events_since/2 |
| Cowboy HTTP server integration complexity makes tests flaky or unreliable | P1 (High) | Tests fail intermittently, blocking CI/CD | Use port 0 for random port assignment, unique transport IDs per test, adequate timer:sleep() for connection setup |
| SSE protocol compliance testing requires parsing binary output (event format validation) | P1 (High) | Tests pass even if SSE format is incorrect | Implement binary pattern matching for "id:", "data:", "event:", "retry:" fields, validate line endings (\n\n) |
| Keep-alive ping (30s) and timeout (5min) make tests slow | P2 (Medium) | Test suite takes too long, reduces developer productivity | Use configurable timers in test mode (?PING_INTERVAL_TEST = 100ms), test timeout behavior with shorter intervals |
| Stream resumption testing requires event store state management across test cases | P2 (Medium) | Tests interfere with each other, causing flakiness | Clear event store before each test, use unique session IDs per test case |
| OpenTelemetry tracing calls may fail if tracing not configured | P3 (Low) | Tests fail due to tracing infrastructure, not SSE logic | Start opentelemetry application in setup, or mock otel_tracer calls if unavailable |
| Multiple concurrent SSE streams may port-bind or resource conflicts | P3 (Low) | Tests fail with "address already in use" | Use unique port numbers per test, proper cleanup with unlink/exit + timer:sleep |

## Recommended Manufacturing Approach

**TCPS Methodology:**

1. **Specification** - Requirements with acceptance criteria
   - SSE Protocol Compliance: All event fields (id, data, event, retry) correctly formatted
   - Keep-Alive: Ping messages sent every 30 seconds during idle periods
   - Stream Lifecycle: Init → Stream → Close with proper resource cleanup
   - Error Handling: Invalid requests rejected with proper HTTP status codes
   - Resumption: Last-Event-ID header triggers event replay

2. **Pseudocode** - Algorithm design BEFORE coding
   ```
   TEST: SSE Event Formatting
   1. Start Cowboy listener on random port
   2. Send GET request to /mcp/sse
   3. Verify response headers: content-type=text/event-stream
   4. Send {send_event, TestData} to handler PID
   5. Receive streamed response
   6. Parse binary: assert "id: session_", "data: TestData", "\n\n"
   7. Close connection, cleanup
   ```

3. **Architecture** - Integration points and dependencies
   - **HTTP Client**: Use gun, hackney, or raw gen_tcp for test client
   - **Event Store**: Minimal ETS-based stub with add_event/3, get_events_since/2
   - **Registry**: Start erlmcp_registry in setup, or mock transport_connected messages
   - **Tracing**: Start opentelemetry_app in setup, ignore tracing failures

4. **Refinement** - Chicago School TDD (tests FIRST)
   - Write failing test for each SSE protocol feature
   - Implement minimal code to pass test (or stub if blocking)
   - Run tests incrementally, fix failures immediately
   - No mocking of Cowboy HTTP server (use real processes)

5. **Completion** - All quality gates passing
   - Coverage ≥80% (run rebar3 cover)
   - All tests pass (run rebar3 eunit --module=erlmcp_transport_sse_tests)
   - 0 compilation errors
   - 0 Dialyzer warnings (run rebar3 dialyzer)
   - Xref clean (add necessary ignores)

**Implementation Strategy:**

**Phase 1: Dependency Resolution** (P0 - Blockers)
1. Search codebase for erlmcp_http_header_validator, erlmcp_origin_validator, erlmcp_http_delete_handler
2. If missing, create stub modules in test directory or mock with meck
3. Implement minimal erlmcp_sse_event_store functions:
   - add_event(SessionId, EventNum, Data) → {ok, EventId}
   - get_events_since(SessionId, LastEventId) → {ok, [Events]}
   - Use ETS ordered_set for in-memory storage

**Phase 2: Basic Protocol Tests** (P1 - Core Functionality)
1. Test init/2: Verify Cowboy listener starts on specified port
2. Test SSE headers: Verify content-type, cache-control, connection headers
3. Test event format: Send {send_event, Data}, verify "id:", "data:", "\n\n" format
4. Test close/1: Verify stream closes with "event: close" and "retry:" hint
5. Test POST handler: Verify JSON messages accepted with 202 status

**Phase 3: Advanced Behavior Tests** (P1 - Edge Cases)
1. Test keep-alive ping: Verify ":\\n" sent every 30 seconds
2. Test stream timeout: Verify 5-minute idle timeout with retry hint
3. Test concurrent streams: Multiple simultaneous GET requests
4. Test Last-Event-ID: Verify resumption replays events

**Phase 4: Error Handling Tests** (P2 - Robustness)
1. Test invalid HTTP methods: Verify 405 Method Not Allowed
2. Test origin validation: Verify 403 Forbidden for invalid origins
3. Test header validation: Verify 400/403 for invalid headers
4. Test malformed JSON: Verify 400 Bad Request for POST

**Quality Validation:**

**Automated:**
```bash
# Compile
rebar3 compile

# Run tests
rebar3 eunit --module=erlmcp_transport_sse_tests --verbose

# Check coverage
rebar3 cover
# View coverage report
open _build/test/cover/index.html

# Dialyzer
rebar3 dialyzer

# Xref
rebar3 xref
```

**Manual:**
- Verify Cowboy listener starts without errors
- Confirm SSE stream can be consumed by browser EventSource
- Test resumption by disconnecting and reconnecting with Last-Event-ID
- Monitor memory usage during long-running streams

**Metrics:**
- Test count: Target 15+ tests (9 existing + 6+ new)
- Coverage: ≥80% of erlmcp_transport_sse.erl (504 lines)
- Test execution time: <30 seconds total (use shorter intervals for keep-alive/timeout tests)
- Memory leak: Verify no process leaks after 100 stream open/close cycles

## Open Questions

**NONE** - Research complete. All dependencies identified, test patterns documented, risks assessed.

**Notes:**
- erlmcp_sse_event_store is confirmed stub (returns {error, not_implemented}) - MUST implement minimal version
- Test patterns reference erlmcp_transport_tcp_tests.erl (Chicago School TDD with real processes)
- SSE transport uses Cowboy handler (NOT gen_server) - different testing approach required
- tcps_dashboard_sse_handler.erl depends on SSE transport - production use confirmed

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) - erlmcp_sse_event_store stub blocks testing + tests are placeholders
- [x] Quality gates defined (specific thresholds) - ≥80% coverage, 100% pass rate, 0 errors
- [x] OTP patterns understood (behaviors, supervision) - Cowboy handler, not gen_server; process-per-connection
- [x] Test strategy clear (Chicago School TDD) - Real Cowboy server, no mocks, proper cleanup
- [x] Risk assessment complete (severity P0-P3) - 8 risks identified with mitigations
- [x] No open questions (all research complete) - Dependencies documented, patterns known
