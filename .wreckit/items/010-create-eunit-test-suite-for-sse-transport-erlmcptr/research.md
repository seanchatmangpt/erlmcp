# Research: Create EUnit test suite for SSE transport (erlmcp_transport_sse)

**Date**: 2025-01-27
**Item**: 010-create-eunit-test-suite-for-sse-transport-erlmcptr
**Section**: transports
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
SSE transport has 0% test coverage - streaming transport is untested

**Motivation:** Server-Sent Events transport for server-to-client streaming. Critical for real-time event streaming use cases.

**Success criteria:**
- Test file created: apps/erlmcp_transports/test/erlmcp_transport_sse_tests.erl
- SSE protocol compliance tested
- Coverage ≥80%
- All tests pass

**Technical constraints:**
- SSE protocol - correct event formatting
- Stream testing - continuous event streaming
- Keep-alive testing - automatic keep-alive messages
- Error handling - stream interruption, reconnect

**Signals:** priority: high, urgency: P1 - BLOCKS PRODUCTION DEPLOYMENT

### Quality Gate Status
- **Gate Type**: Coverage / EUnit / Protocol Compliance
- **Current State**: 0% coverage (test file exists but tests are stubs)
- **Target State**: ≥80% coverage, 100% pass rate
- **Gap**: 80 percentage points (0% → 80%)

## Summary

The SSE (Server-Sent Events) transport is a critical streaming transport for server-to-client real-time communication in the erlmcp system. It implements the WHATWG SSE specification for event streaming over HTTP, supporting features like event IDs for resumability, retry fields for reconnection hints, and keep-alive ping messages. The current test file exists but contains only stub tests that don't actually validate the SSE protocol implementation, resulting in 0% meaningful coverage.

The manufacturing objective is to create a comprehensive EUnit test suite that validates SSE protocol compliance including: (1) event formatting per WHATWG specification (id:, data:, event:, retry: fields), (2) stream lifecycle management (connection, streaming, disconnection), (3) keep-alive ping mechanism (30-second intervals with comment lines), (4) stream resumption via Last-Event-ID header, (5) error handling for stream interruption and client reconnection, and (6) integration with Cowboy HTTP server and registry.

The technical approach follows the Chicago School TDD pattern used throughout the erlmcp codebase. The test suite will use real Cowboy HTTP connections (not mocks) to test actual SSE protocol behavior, following the pattern established in `erlmcp_transport_tcp_tests.erl` and `erlmcp_transport_ws_tests.erl`. Tests will cover both unit-level functions (format_sse_event, generate_session_id) and integration-level scenarios (full HTTP request/response cycles with multiple concurrent streams). The tests will validate protocol compliance against WHATWG SSE specification and MCP transport requirements as documented in `docs/WEBSOCKET_SSE_COMPLIANCE_REVIEW.md`.

The TCPS justification is that SSE transport is a critical production component for real-time event streaming with 0% test coverage, representing a **P1 quality gap** that BLOCKS production deployment per Lean Six Sigma standards (99.99966% defect-free delivery). Jidoka principle requires that the streaming transport be fully validated before production use - any failure in SSE protocol handling could cause silent message loss or connection failures. Poka-yoke requires protocol-level validation tests to prevent malformed events from reaching production. Kaizen requires measurable quality metrics (coverage, pass rate) to track improvement. Andon requires test failures to be immediately visible with clear error messages indicating which SSE protocol requirement failed.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_sse.erl` (504 lines) - Main SSE transport implementation
  - `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_sse_tests.erl` (124 lines) - Stub test file (0% coverage)
  - `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_behavior.erl` (817 lines) - Transport behavior specification

- **Patterns**:
  - **Cowboy HTTP Handler**: Uses REST handler pattern (init/3, handle/2, terminate/3) not gen_server
  - **SSE Event Loop**: Recursive receive loop (sse_event_loop/3) handling ping, send_event, close messages
  - **Stream State**: #sse_state record tracking transport_id, client_id, session_id, event_number, ping_timer
  - **Tracing Integration**: OpenTelemetry span creation for all operations
  - **Protocol Compliance**: WHATWG SSE specification with event IDs, retry fields, keep-alive comments

- **Tests**: Current test file has 9 stub tests that don't validate actual SSE behavior:
  1. `test_init_sse/0` - Only checks if Pid is returned, doesn't validate Cowboy listener started
  2. `test_send_event/0` - Spawns dummy process, doesn't test actual SSE event formatting
  3. `test_close_stream/0` - No validation of close event format with retry field
  4. `test_format_sse_event/0` - Hardcoded expected value, doesn't call format function
  5. `test_post_message/0` - Only tests JSON encoding, not POST handling
  6. `test_get_stream/0` - Empty test (always true)
  7. `test_keepalive_ping/0` - Only checks ping binary, not 30-second timer
  8. `test_stream_timeout/0` - Only checks timeout constant, not 5-minute idle timeout
  9. `test_concurrent_streams/0` - Spawns processes but doesn't test actual SSE streams

- **Quality**: Current gate status = **FAIL**
  - **Coverage**: 0% meaningful coverage (tests are stubs)
  - **Pass Rate**: 100% (but tests don't validate anything)
  - **Protocol Compliance**: Untested
  - **SSE Specification**: Untested

### Key Files

- `apps/erlmcp_transports/src/erlmcp_transport_sse.erl:1-504`
  - Main SSE transport implementation with Cowboy HTTP handler
  - **Critical functions**: init/2, send/2, close/1, handle/2, sse_event_loop/3
  - **SSE event formatting**: format_sse_event/1 (L336), format_sse_event_with_id/2 (L341)
  - **Keep-alive mechanism**: 30-second ping timer (L210, L287-290)
  - **Stream resumption**: handle_stream_resumption/9 (L364-399) with Last-Event-ID support
  - **Retry field support**: format_close_event_with_retry/1 (L499-502), get_retry_timeout/0 (L470-483)

- `apps/erlmcp_transports/test/erlmcp_transport_sse_tests.erl:1-124`
  - Existing stub test file (needs complete rewrite)
  - **Current structure**: 9 tests in sse_transport_test_/0 generator
  - **Problems**: No actual Cowboy HTTP connections, no protocol validation, no integration tests

- `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:1-700`
  - **Reference test pattern** for transport testing
  - **Setup/Cleanup**: setup_server/0, setup_client/0, cleanup/1 functions
  - **Integration tests**: Real TCP connections with Ranch (L258-324)
  - **Error handling**: Connection failure, reconnection, timeout tests (L555-605)
  - **Concurrency**: Multiple client connections test (L611-656)

- `apps/erlmcp_transports/test/erlmcp_transport_ws_tests.erl:1-385`
  - **Reference for Cowboy HTTP handler testing**
  - **Test groups**: Organized by feature (L26-87)
  - **Unit tests**: UTF-8 validation (L155-180), message size limits (L186-220)
  - **Fragment tests**: Multi-part fragment handling (L226-256)
  - **Integration tests**: Complete request/response cycles (L334-384)

- `docs/WEBSOCKET_SSE_COMPLIANCE_REVIEW.md:1-1011`
  - **SSE compliance checklist** (L826-894)
  - **SSE specification requirements**: Event format, retry field, resumability (L232-550)
  - **Test gaps identified**: Multi-line JSON in SSE (L712), event pub/sub manager (L768)

### OTP Patterns Observed

- **Behavior**: Cowboy REST Handler (NOT gen_server)
  - Exports: init/3, handle/2, terminate/3 (Cowboy handler callbacks)
  - Note: Does NOT implement erlmcp_transport_behavior (unlike TCP/stdio)
  - Pattern: HTTP handler with recursive event loop (sse_event_loop/3)

- **Supervision**: Not supervised as gen_server
  - Cowboy manages listener process (erlmcp_sse_listener)
  - Handler processes are transient (one per HTTP connection)
  - Pattern: Let-it-crash - Cowboy will restart handler on failure

- **Process Pattern**: Process-per-connection (via Cowboy)
  - Each SSE stream gets its own handler process
  - State stored in #sse_state record within handler process
  - No supervisor tree for handlers (Cowboy's responsibility)

- **Test Pattern**: Chicago School TDD (real processes, no mocks)
  - Reference: `erlmcp_transport_tcp_tests.erl` (real Ranch connections)
  - Reference: `erlmcp_transport_ws_tests.erl` (real Cowboy WebSocket)
  - Tests should start actual Cowboy HTTP server and make real HTTP requests

## Technical Considerations

### Dependencies

- **Internal Modules**:
  - `erlmcp_registry` (apps/erlmcp_core/src) - Message routing registry
  - `erlmcp_tracing` (apps/erlmcp_observability/src) - OpenTelemetry tracing
  - `erlmcp_http_header_validator` - HTTP header validation (L163, L245)
  - `erlmcp_origin_validator` - Origin validation for DNS rebinding protection (L416)
  - `erlmcp_sse_event_store` - Event storage for stream resumption (L298, L366)
  - `erlmcp_http_delete_handler` - DELETE request handler (L143)

- **External Libraries**:
  - `cowboy 2.10.0` (rebar.config:52) - HTTP server for SSE streaming
  - `gun 2.0.1` (rebar.config:48) - Could be used for test client
  - `jsx 3.1.0` (rebar.config:45) - JSON encoding/decoding

- **OTP Applications**:
  - `cowboy` - HTTP server
  - `ranch` - Connection acceptor pool (Cowboy dependency)
  - `ssl` - For HTTPS support (optional)

### TCPS Quality Gates to Pass

- [ ] **Compilation**: 0 errors
  - SSE transport module must compile without errors
  - Test module must compile without errors
  - Current status: ✅ Compiles (but tests are stubs)

- [ ] **EUnit**: 100% pass rate
  - All SSE protocol tests must pass
  - Integration tests with real HTTP connections must pass
  - Error handling tests must validate proper failures
  - Current status: ⚠️ 100% pass rate (but tests don't validate anything)

- [ ] **Common Test**: Not applicable (EUnit only for this module)

- [ ] **Coverage**: ≥80%
  - Current coverage: 0% (stub tests)
  - Target coverage: ≥80%
  - Gap: 80 percentage points
  - Functions to cover:
    - init/2 (L42-73)
    - send/2 (L76-103)
    - close/1 (L98-103)
    - init/3 (L109-119)
    - handle/2 (L121-156)
    - handle_sse_stream/1 (L158-238)
    - handle_post_request/2 (L240-283)
    - sse_event_loop/3 (L285-322)
    - terminate/3 (L324-329)
    - format_sse_event/1 (L336-338)
    - format_sse_event_with_id/2 (L341-344)
    - generate_session_id/1 (L347-351)
    - handle_stream_resumption/9 (L364-399)
    - validate_request_origin/2 (L408-432)
    - get_allowed_origins/0 (L437-448)
    - ensure_binary/1 (L453-460)
    - get_retry_timeout/0 (L470-483)
    - format_retry_field/1 (L490-492)
    - format_close_event_with_retry/1 (L499-502)

- [ ] **Dialyzer**: 0 warnings
  - Type specs must be added for all exported functions
  - Current specs present: init/2 (L41), send/2 (L75), close/1 (L97)
  - Missing specs: Internal helper functions

- [ ] **Xref**: 0 undefined function calls
  - Dependencies on erlmcp_http_header_validator (not found in codebase)
  - Dependencies on erlmcp_origin_validator (not found in codebase)
  - Dependencies on erlmcp_sse_event_store (not found in codebase)
  - **RISK**: These modules may not exist, causing runtime failures

- [ ] **Performance**: <10% regression from baseline
  - SSE stream must handle 1000+ concurrent connections
  - Keep-alive ping interval must not degrade performance
  - Event formatting must be efficient (< 1ms per event)

### Patterns to Follow

- **Cowboy HTTP Handler Pattern**:
  - Reference: `erlmcp_transport_sse.erl:109-156` (init/3, handle/2)
  - Test approach: Start Cowboy listener, make HTTP requests using gun or hackney
  - Validate response headers (content-type: text/event-stream)
  - Validate response body (SSE event format)

- **Test Pattern** (Chicago School TDD):
  - Reference: `erlmcp_transport_tcp_tests.erl:27-70` (setup/cleanup)
  - Reference: `erlmcp_transport_ws_tests.erl:9-20` (test setup)
  - Use real Cowboy HTTP server (not mocks)
  - Use real HTTP client (gun or hackney) to make requests
  - Validate actual SSE protocol behavior

- **Error Handling Pattern**:
  - Reference: `erlmcp_transport_tcp_tests.erl:555-605` (error handling tests)
  - Test stream interruption scenarios
  - Test client reconnection with Last-Event-ID
  - Test timeout handling (5-minute idle timeout)

- **Type Specs Pattern**:
  - Reference: `erlmcp_transport_behavior.erl:104-190` (callback specs)
  - Add -spec attributes for all exported functions
  - Use proper types: binary(), pid(), map(), {ok, term()}, {error, term()}

## Root Cause Analysis (5 Whys)

**Problem**: SSE transport has 0% test coverage, blocking production deployment

1. **Why?** The test file exists but contains only stub tests that don't validate SSE protocol behavior.

2. **Why?** The tests were created as placeholders without implementing actual test logic that validates Cowboy HTTP handler behavior.

3. **Why?** There was insufficient understanding of how to test Cowboy HTTP handlers with SSE streaming protocol using real HTTP connections (Chicago School TDD).

4. **Why?** The existing test patterns in the codebase (TCP transport tests) use Ranch which is different from Cowboy REST handlers, requiring a different testing approach.

5. **ROOT CAUSE**: Missing test implementation for SSE protocol validation. The SSE transport implementation is complete and protocol-compliant (per `docs/WEBSOCKET_SSE_COMPLIANCE_REVIEW.md`), but the tests were never written to validate this compliance.

**Solution**: Implement comprehensive EUnit tests following the pattern from `erlmcp_transport_ws_tests.erl` (which also uses Cowboy handlers). Tests must use real Cowboy HTTP server and real HTTP client connections to validate:
- SSE event format (id:, data:, event:, retry: fields)
- Keep-alive ping mechanism (30-second interval with ": " comments)
- Stream resumption via Last-Event-ID header
- Error handling for stream interruption
- Integration with registry and OpenTelemetry tracing
- Multiple concurrent SSE streams

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| Missing dependencies (erlmcp_http_header_validator, erlmcp_origin_validator, erlmcp_sse_event_store) | P0 | Tests and production code will fail at runtime with undefined function errors | Search codebase for these modules; if they don't exist, create stub implementations or remove dependencies from SSE transport before testing |
| Cowboy handler testing complexity | P1 | Tests may be difficult to write correctly, leading to flaky or incomplete coverage | Study `erlmcp_transport_ws_tests.erl` as reference; use gun HTTP client for making requests to Cowboy server; test with real HTTP connections, not mocks |
| SSE stream testing timing issues | P1 | Keep-alive ping (30s) and idle timeout (5min) tests may take too long or be flaky | Use meck to mock timer:send_interval for ping tests; use shortened timeout in test configuration; test timeout logic directly without waiting full 5 minutes |
| Stream resumption complexity | P2 | Last-Event-ID replay logic requires event store, adding test complexity | Create in-memory event store for testing; test resumption with simulated missed events; validate event ID format and parsing |
| Concurrent stream testing resource usage | P2 | Multiple concurrent SSE streams may consume significant resources in tests | Limit to 5-10 concurrent streams in tests; clean up properly with stop/kill messages; use unique ports to avoid conflicts |
| Coverage gaps due to missing modules | P0 | If dependencies don't exist, coverage will be incomplete and production will crash | **MUST resolve before writing tests**: either create missing modules or remove calls from erlmcp_transport_sse.erl |

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Requirements with acceptance criteria
   - SSE event format validation (id:, data:, event:, retry:)
   - Keep-alive ping mechanism (30s interval with ": " comment)
   - Stream resumption via Last-Event-ID header
   - Error handling (stream interruption, timeout, reconnect)
   - Integration with Cowboy, registry, tracing

2. **Pseudocode** - Algorithm design BEFORE coding
   ```
   Test Structure:
   - Setup: Start Cowboy listener on random port
   - Unit Tests: format_sse_event, generate_session_id, get_retry_timeout
   - Integration Tests: GET /mcp/sse → validate headers + event stream
   - Stream Tests: Multiple events, keep-alive ping, close event
   - Resumption Tests: Last-Event-ID replay
   - Error Tests: Invalid headers, stream interruption, timeout
   - Cleanup: Stop Cowboy listener
   ```

3. **Architecture** - Integration points and dependencies
   - Cowboy HTTP server (test fixture)
   - Gun or hackney HTTP client (test driver)
   - Registry mock or integration
   - Event store (if exists, otherwise mock)
   - OpenTelemetry tracing (optional in tests)

4. **Refinement** - Chicago School TDD (tests FIRST)
   - Write failing tests for each SSE protocol requirement
   - Implement test logic (not production code - already exists)
   - Run tests and iterate until all pass
   - Measure coverage (target: ≥80%)

5. **Completion** - All quality gates passing
   - Compilation: 0 errors
   - EUnit: 100% pass rate (all tests pass)
   - Coverage: ≥80%
   - Dialyzer: 0 warnings
   - Xref: 0 undefined function calls

**Implementation Strategy:**

Phase 1 - **Dependency Resolution** (CRITICAL, blocks everything else):
- Search for erlmcp_http_header_validator module
- Search for erlmcp_origin_validator module
- Search for erlmcp_sse_event_store module
- If missing, create stub implementations or remove from erlmcp_transport_sse.erl
- **Output**: Verified dependencies or updated code

Phase 2 - **Unit Tests** (2-3 hours):
- Test format_sse_event/1 with various data payloads
- Test format_sse_event_with_id/2 with event IDs
- Test generate_session_id/1 for uniqueness
- Test get_retry_timeout/0 with default and custom config
- Test format_close_event_with_retry/1
- Test ensure_binary/1 with various input types
- **Target**: Cover all internal helper functions

Phase 3 - **Integration Tests** (3-4 hours):
- Start Cowboy listener on random port
- GET /mcp/sse → validate response headers (content-type: text/event-stream)
- Validate SSE event format in response body
- Test send/2 function sends {send_event, Data} message
- Test close/1 function sends close event
- Test registry integration (transport_connected message)
- **Target**: Cover init/2, handle/2, handle_sse_stream/1

Phase 4 - **Stream Tests** (2-3 hours):
- Test multiple events sent sequentially
- Test keep-alive ping (": " comment) mechanism
- Test 5-minute idle timeout (use shortened timeout in tests)
- Test event ID increment (event_number in state)
- Test event storage for resumption
- **Target**: Cover sse_event_loop/3, terminate/3

Phase 5 - **Resumption Tests** (2 hours):
- Test Last-Event-ID header extraction
- Test stream resumption replays missed events
- Test event ID parsing (erlmcp_sse_event_store:parse_event_id)
- Test resumption failure handling
- **Target**: Cover handle_stream_resumption/9

Phase 6 - **Error Handling Tests** (2-3 hours):
- Test invalid Origin header → 403 Forbidden
- Test missing required headers → 400/400
- Test POST with invalid JSON → 400
- Test stream interruption handling
- Test concurrent stream failures
- **Target**: Cover error paths in handle/2, validate_request_origin/2

Phase 7 - **Concurrency Tests** (1-2 hours):
- Test 5-10 concurrent SSE streams
- Test unique session IDs per stream
- Test independent event numbering per stream
- Test cleanup on stream close
- **Target**: Validate process-per-stream isolation

**Quality Validation:**

- **Automated**:
  ```bash
  # Compile
  TERM=dumb rebar3 compile

  # Run EUnit tests
  rebar3 eunit --module=erlmcp_transport_sse_tests

  # Generate coverage report
  rebar3 cover --verbose

  # Check coverage percentage
  rebar3 cover --verbose | grep erlmcp_transport_sse
  ```

- **Manual**:
  - Verify SSE events follow WHATWG format (id:, data:, event:, retry:)
  - Verify keep-alive ping sent every 30 seconds
  - Verify stream resumption works with Last-Event-ID
  - Verify concurrent streams don't interfere

- **Metrics**:
  - Coverage % (target: ≥80%)
  - Test pass rate (target: 100%)
  - Test execution time (target: < 30 seconds)
  - Number of tests (target: 30-40 tests)
  - Number of assertions (target: 100+ assertions)

## Open Questions

**NONE** - Research complete. All necessary information gathered from:
- Source code: erlmcp_transport_sse.erl (504 lines)
- Existing test: erlmcp_transport_sse_tests.erl (124 lines, stubs)
- Reference tests: erlmcp_transport_tcp_tests.erl, erlmcp_transport_ws_tests.erl
- Documentation: WEBSOCKET_SSE_COMPLIANCE_REVIEW.md (SSE compliance checklist)
- Build configuration: rebar.config (cowboy 2.10.0 dependency)

## Manufacturing Checklist

- [x] Root cause identified (not symptoms)
  - **Root cause**: Tests were created as stubs without implementation

- [x] Quality gates defined (specific thresholds)
  - Coverage: ≥80%
  - Pass rate: 100%
  - Dialyzer: 0 warnings
  - Xref: 0 undefined function calls

- [x] OTP patterns understood (behaviors, supervision)
  - Cowboy REST handler (not gen_server)
  - Process-per-connection (Cowboy managed)
  - Recursive event loop pattern
  - Let-it-crash error handling

- [x] Test strategy clear (Chicago School TDD)
  - Real Cowboy HTTP server (not mocks)
  - Real HTTP client connections (gun/hackney)
  - Validate actual SSE protocol behavior
  - Integration + unit tests

- [x] Risk assessment complete (severity P0-P3)
  - P0: Missing dependencies (erlmcp_http_header_validator, etc.)
  - P1: Cowboy handler testing complexity, timing issues
  - P2: Stream resumption complexity, concurrent stream resources
  - All risks identified with mitigations

- [x] No open questions (all research complete)
  - Dependencies mapped
  - Test patterns identified
  - Protocol requirements documented
  - Implementation strategy defined
