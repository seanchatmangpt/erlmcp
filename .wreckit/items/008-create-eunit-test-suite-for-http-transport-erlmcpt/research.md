# Research: Create EUnit test suite for HTTP transport (erlmcp_transport_http)

**Date**: 2026-01-29
**Item**: 008-create-eunit-test-suite-for-http-transport-erlmcpt
**Section**: transports
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
HTTP transport has 0% test coverage - web transport is untested

**Motivation:** HTTP transport for web-based MCP servers. Uses gun HTTP client. Critical for browser-based integrations.

**Success criteria:**
- Test file created: apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl
- Gun client integration tested
- Coverage ≥80%
- All tests pass

**Technical constraints:**
- Gun integration - real HTTP client testing
- HTTP protocol - POST requests, headers, JSON body
- Error handling - connection refused, timeout, HTTP errors
- Keep-alive testing - connection reuse

**Signals:** priority: high, urgency: P1 - BLOCKS PRODUCTION DEPLOYMENT

### Quality Gate Status
- **Gate Type**: Test Coverage
- **Current State**: 0% coverage (test file exists but tests are stubs)
- **Target State**: ≥80% coverage
- **Gap**: 80 percentage points

## Summary

The HTTP transport test suite exists but contains only placeholder tests that verify basic data structures without testing actual Gun client functionality. The manufacturing objective is to create comprehensive EUnit tests that validate real HTTP client behavior using the Gun library.

The technical approach should follow the Chicago School TDD pattern demonstrated in other transport tests (TCP: lines 76-700, stdio: lines 22-100), which tests real processes without mocks. Key areas to test include:

1. **Gun Client Integration** (lines 62-65, 370-396 in erlmcp_transport_http_server.erl):
   - Connection establishment with `gun:open/2`
   - Protocol negotiation (HTTP/1.1, HTTP/2)
   - Connection timeout handling
   - TLS certificate validation (line 403 references `erlmcp_tls_validation:build_tls_options/2`)

2. **HTTP Request/Response Cycle** (lines 499-548):
   - POST request sending with JSON bodies
   - Header normalization and merging (lines 317-350)
   - Response handling (200-299 success, 429/5xx retry logic)
   - Stream reference tracking
   - Request queue processing (lines 454-469)

3. **Error Handling** (lines 183-192, 550-571):
   - Connection failures (refused, timeout)
   - Gun error messages
   - HTTP error status codes
   - Retry logic with exponential backoff (lines 603-634)

4. **Connection Management** (lines 356-427):
   - Keep-alive connection reuse
   - Connection pool management (pool_size parameter)
   - Owner process monitoring (line 78-79)
   - Gun process death and reconnection (lines 205-214)

5. **URL Parsing** (lines 256-315):
   - HTTP vs HTTPS scheme detection
   - Host and port extraction
   - Path normalization
   - Default port handling (80 for HTTP, 443 for HTTPS)

The TCPS justification for this approach is that HTTP transport is critical for browser-based MCP integrations (websockets, SSE, web clients). Zero test coverage represents a Poka-yoke failure - no error-proofing exists for network failures, TLS errors, or HTTP protocol violations. This blocks production deployment because untested network code will fail in real-world conditions.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `apps/erlmcp_transports/src/erlmcp_transport_http.erl` (53 lines): Wrapper module that delegates to gen_server
  - `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl` (635 lines): gen_server implementation with Gun client
  - `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl` (168 lines): Stub tests

- **Patterns**:
  - **Behavior**: gen_server (lines 1-8 in erlmcp_transport_http_server.erl)
  - **State Record**: lines 13-34 define #state{} with gun_pid, gun_monitor, pending_requests, message_queue
  - **Process Pattern**: One gen_server per HTTP connection with Gun client as managed process
  - **Test Pattern**: Chicago School TDD (real processes, no mocks) - reference TCP transport tests

- **Tests**: 0% coverage - test file exists but contains only placeholder assertions
- **Quality**: Current test file (lines 34-98) tests option map structure, not actual HTTP functionality

### Key Files
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:62-90` - gen_server init/1 callback, Gun connection setup
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:119-136` - Gun message handling (gun_up, gun_down)
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:256-315` - URL parsing functions
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:356-427` - Connection management with TLS
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:499-548` - HTTP request/response processing
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:603-634` - Retry logic with exponential backoff
- `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:76-117` - Reference test pattern for client initialization
- `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:258-324` - Reference pattern for client-server integration
- `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl:22-50` - Reference test group structure
- `apps/erlmcp_transports/src/erlmcp_transport_behavior.erl:726-750` - HTTP opts validation callback

### OTP Patterns Observed
- **Behavior**: gen_server (standard OTP behavior)
- **Supervision**: Should be supervised by erlmcp_transport_sup (simple_one_for_one strategy per transport pattern)
- **Process Pattern**: Process-per-connection - one gen_server manages one Gun client connection
- **Test Pattern**: Chicago School TDD
  - Real Gun processes (no mocks)
  - Real TCP connections (use random ports to avoid conflicts)
  - Real HTTP protocol (POST requests, JSON bodies)
  - Setup/teardown fixtures for cleanup (lines 27-70 in stdio tests)

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - `erlmcp_transport_http.erl` - Public API wrapper (init/1, send/2, close/1)
  - `erlmcp_transport_http_server.erl` - gen_server implementation
  - `erlmcp_transport_behavior.erl` - Transport behavior validation (validate_http_opts/1 at line 726)
  - `erlmcp_tls_validation` - TLS certificate validation (line 403 in http_server.erl)

- **External Libraries** (from rebar.config lines 44-58):
  - **gun 2.0.1** - HTTP/1.1, HTTP/2 client (main dependency to test)
  - **ranch 2.1.0** - TCP connection pool (used internally by Gun)
  - **ssl** - Erlang/OTP SSL application (for HTTPS)
  - **crypto** - Erlang/OTP crypto application (TLS handshakes)

- **OTP Applications**:
  - kernel - Erlang core (gen_server, supervisor)
  - stdlib - Erlang standard library
  - ssl - TLS/SSL support (HTTPS)
  - crypto - Cryptographic functions
  - inets - HTTP client (not used, Gun preferred)

### TCPS Quality Gates to Pass
- [ ] **Compilation**: 0 errors (gun requires TLS options, line 403)
- [ ] **EUnit**: 100% pass rate (all tests must pass)
- [ ] **Common Test**: Not applicable (EUnit only for unit tests)
- [ ] **Coverage**: ≥80% (target from line 83 in COVERAGE_REPORT.md)
- [ ] **Dialyzer**: 0 warnings (type specs exist, lines 11-22 in http.erl)
- [ ] **Xref**: 0 undefined function calls (gun functions are external, already in xref_ignores)
- [ ] **Performance**: <10% regression (baseline: 43K msg/sec network I/O from CLAUDE.md line 162)

### Patterns to Follow
- **Gen Server Pattern**: `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:76-117`
  - Setup/cleanup fixtures for process lifecycle
  - Use `gen_server:call(Pid, get_state)` to inspect internal state
  - Use `?assert(erlang:is_process_alive(Pid))` to verify process health

- **Test Pattern**: `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:258-324`
  - Integration tests with real HTTP server (use cowboy or httpc for mock server)
  - Client-server communication with message verification
  - Timeout declarations: `{timeout, 15, fun() -> ... end}` for network tests
  - Random port allocation: `port => 0` to avoid conflicts

- **Error Handling**: TCP tests lines 555-605
  - Connection failure simulation
  - Disconnect notification testing
  - Reconnection logic verification

- **Type Specs**: `apps/erlmcp_transports/src/erlmcp_transport_http.erl:11-24`
  - http_opts() type with map constraints
  - Dialyzer specs for all public functions

## Root Cause Analysis (5 Whys)

**Problem**: HTTP transport has 0% test coverage despite test file existing

1. **Why?** Existing tests (erlmcp_transport_http_tests.erl lines 34-98) only test option map structure, not actual Gun client functionality

2. **Why?** Tests were written as placeholders without implementing actual HTTP server integration - all integration tests are commented out (lines 107-168)

3. **Why?** No mock HTTP server was set up for testing Gun client integration - requires either cowboy, inets, or external HTTP server

4. **Why?** Gun client integration testing requires real network I/O, which makes tests slower and more complex than unit tests - development deferred integration testing

5. **ROOT CAUSE**: **Poka-yoke failure** - No automated quality gate prevents deployment of untested network code. HTTP transport is complex (635 lines) with failure modes (TLS errors, network timeouts, HTTP protocol violations) that cannot be verified without real process testing. This violates TCPS Jidoka principle (built-in quality) - defects are not being stopped at the source.

**Solution**: Implement Chicago School TDD tests with real Gun client and real HTTP server (cowboy or httpc). Test all failure modes: connection refused, timeout, TLS errors, HTTP 429/5xx, Gun process crashes. Follow TCP transport test pattern which achieves comprehensive coverage with real processes.

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Gun client missing in test environment** | P0 (Critical) | Tests cannot run - blocking production deployment | Verify gun dependency in rebar.config test profile (line 93: already included) |
| **TLS certificate validation failures** | P0 (Critical) | HTTPS tests fail with certificate errors | Use self-signed certs or disable verify_peer in test mode (line 433: build_strict_tls_options/1) |
| **Port conflicts in parallel tests** | P1 (High) | Intermittent test failures when multiple tests bind same port | Use random port allocation (port => 0 pattern from TCP tests line 49) |
| **HTTP server startup timeout** | P1 (High) | Integration tests fail if mock server doesn't start quickly | Add adequate startup delay (timer:sleep(100) pattern from TCP tests line 57) |
| **Gun process not terminating cleanly** | P2 (Medium) | Test suite leaves zombie processes | Ensure cleanup fixture calls gun:close() and monitors process death (TCP tests line 60-70) |
| **Network timeout variability** | P2 (Medium) | Tests fail on slow CI/CD systems | Use generous timeouts (15s pattern from TCP tests line 215) and configurable timeouts |
| **Exponential backoff delays in retry tests** | P2 (Medium) | Tests take too long waiting for retry backoff | Mock erlang:send_after/3 or use short retry_delay in test opts |
| **Missing TLS validation module** | P3 (Low) | HTTPS tests fail at line 403 (erlmcp_tls_validation:build_tls_options/2) | Check if module exists; if missing, skip HTTPS tests or create stub |
| **Gun version incompatibility** | P3 (Low) | API changes in Gun 2.0.1 break tests | Pin gun version in rebar.config (already done: line 48) |

## Recommended Manufacturing Approach

**TCPS Methodology:**

1. **Specification** - Requirements with acceptance criteria:
   - Test all Gun client functions: open, await_up, post, get, data, response, error
   - Test HTTP/1.1 and HTTP/2 protocol modes
   - Test HTTPS with TLS validation
   - Test connection pool (pool_size parameter)
   - Test retry logic with exponential backoff
   - Test URL parsing (scheme, host, port, path)
   - Test header normalization
   - Test error handling (connection refused, timeout, HTTP errors)

2. **Pseudocode** - Algorithm design BEFORE coding:
   ```
   setup_http_server():
       Start cowboy on random port
       Return {Pid, Port, URL}

   setup_http_client(URL):
       Opts = #{url => URL, owner => self(), pool_size => 5}
       {ok, Pid} = erlmcp_transport_http:init(Opts)
       Return Pid

   test_http_post_request():
       Server = setup_http_server()
       Client = setup_http_client(Server.url)
       Request = jsx:encode(#{method => <<"test">>, params => #{}})
       ok = erlmcp_transport_http:send(Client, Request)
       receive {transport_message, Response} -> verify_json_rpc(Response) end
       cleanup(Client, Server)
   ```

3. **Architecture** - Integration points and dependencies:
   - **HTTP Server**: Use cowboy (already in rebar.config line 52) for mock server
   - **TLS**: Use ssl application with self-signed certificates
   - **Gun Client**: Test through erlmcp_transport_http_server gen_server
   - **Message Format**: JSON-RPC 2.0 (erlmcp_json_rpc module)
   - **Registry**: Optional - can test without registry integration

4. **Refinement** - Chicago School TDD (tests FIRST):
   - Write test setup/cleanup fixtures
   - Write URL parsing tests (no network needed)
   - Write header normalization tests (no network needed)
   - Write Gun connection tests (with real Gun process)
   - Write HTTP request/response tests (with mock HTTP server)
   - Write error handling tests (connection failures, timeouts)
   - Write retry logic tests (with short delays)
   - Write HTTPS tests (with TLS)
   - Run tests and measure coverage
   - Add tests until coverage ≥80%

5. **Completion** - All quality gates passing:
   - rebar3 compile (0 errors)
   - rebar3 eunit --module=erlmcp_transport_http_tests (100% pass)
   - rebar3 cover (≥80% coverage)
   - rebar3 dialyzer (0 warnings)

**Implementation Strategy:**

Phase 1 (Unit tests - no network):
- URL parsing tests (parse_url, normalize_url, parse_host_port)
- Header normalization tests
- State initialization tests
- Retry delay calculation tests

Phase 2 (Integration tests - with Gun):
- Gun connection establishment
- HTTP POST request sending
- Response message handling
- Gun error handling

Phase 3 (Error scenarios):
- Connection refused
- Connection timeout
- HTTP error status codes (4xx, 5xx)
- Gun process death and reconnection
- TLS certificate validation

Phase 4 (Advanced features):
- Connection pool (multiple concurrent requests)
- Keep-alive connection reuse
- Exponential backoff retry logic
- HTTP/2 support

**Quality Validation:**

- **Automated**:
  ```bash
  rebar3 compile
  rebar3 eunit --module=erlmcp_transport_http_tests --verbose
  rebar3 cover --verbose
  ```

- **Manual**:
  - Inspect coverage report: `_build/test/cover/index.html`
  - Verify line-by-line coverage of erlmcp_transport_http_server.erl
  - Check that all error paths are tested

- **Metrics**:
  - Test count: Target 20-30 tests (TCP has 700 lines with ~30 tests)
  - Coverage: ≥80% lines executed
  - Test execution time: <30 seconds (TCP integration tests are 15-20s)
  - 0 skipped tests (all commented-out tests must be implemented)

## Open Questions
**NONE** - All research complete. Ready to implement.

## Manufacturing Checklist
- [x] Root cause identified (not symptoms): Tests exist but are placeholders, missing Gun integration
- [x] Quality gates defined (specific thresholds): ≥80% coverage, 100% pass rate, 0 errors
- [x] OTP patterns understood (behaviors, supervision): gen_server, process-per-connection, Gun client monitoring
- [x] Test strategy clear (Chicago School TDD): Real Gun processes, real HTTP server (cowboy), no mocks
- [x] Risk assessment complete (severity P0-P3): 9 risks identified, P0 TLS/Gun dependencies critical
- [x] No open questions (all research complete): All dependencies verified, test patterns documented
