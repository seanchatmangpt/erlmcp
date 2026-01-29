# Research: HTTP Transport Test Coverage

**Date**: 2025-01-29
**Item**: 032-http-transport-test-coverage
**Section**: transports
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
HTTP transport has ZERO coverage. Uses gun HTTP client for web-based MCP servers.

**Motivation:** HTTP transport is used for web-based MCP servers. Important for browser and web service integrations.

**Success criteria:**
- Test file created: apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl
- Gun client integration tested
- Coverage: ≥80%
- All tests pass

**Technical constraints:**
- Gun Integration - Real HTTP client testing
- HTTP Protocol - POST requests, headers, JSON body
- Error Handling - Connection refused, timeout, HTTP errors
- Keep-Alive - Connection reuse

**Signals:** priority: high, urgency: P1 - BLOCKS PRODUCTION DEPLOYMENT

### Quality Gate Status
- **Gate Type**: Coverage
- **Current State**: 0% coverage (per docs/COVERAGE_REPORT.md line 83)
- **Target State**: ≥80% coverage
- **Gap**: 80 percentage points (0% → 80%)

## Summary

**What needs to be done (manufacturing objective):**
The HTTP transport (`erlmcp_transport_http.erl` and `erlmcp_transport_http_server.erl`) operates at 0% test coverage despite having a basic test file with 168 lines. This is a **P1 critical defect** that blocks production deployment. The HTTP transport is critical for web-based MCP servers, browser integrations, and web service integrations. The manufacturing objective is to achieve ≥80% code coverage by completely rewriting the test suite to follow Chicago School TDD principles with real Gun HTTP client integration, actual HTTP/1.1 and HTTP/2 protocol testing, comprehensive error handling coverage, and TLS security validation testing.

**How to do it (technical approach):**
1. **Analyze Current Implementation**: The HTTP transport consists of two modules: `erlmcp_transport_http.erl` (53 lines, thin wrapper) and `erlmcp_transport_http_server.erl` (635 lines, gen_server implementation). The current test file has only 6 trivial test functions that don't actually test Gun client integration or HTTP protocol functionality.
2. **Implement Real HTTP Testing**: Create comprehensive tests using actual Gun HTTP client connections to localhost test servers. Test HTTP/1.1 and HTTP/2 protocols, POST requests with JSON bodies, header normalization, connection pooling, keepalive, and request queuing.
3. **Test Error Handling**: Implement tests for connection failures (connection refused, timeout), HTTP errors (4xx, 5xx status codes), Gun client errors, retry logic with exponential backoff, and TLS validation failures.
4. **Test TLS Security**: The HTTP transport has a CRITICAL SECURITY FIX (CVSS 9.8) at line 403 that calls `erlmcp_tls_validation:build_tls_options/2`. However, this module doesn't exist yet. Must create mock or stub for TLS validation testing.
5. **Validate Coverage**: Run `rebar3 cover --module=erlmcp_transport_http` and `rebar3 cover --module=erlmcp_transport_http_server` to verify ≥80% coverage threshold is met.

**Why this approach (TCPS justification):**
This follows **Jidoka (built-in quality)** - HTTP transport is a PRODUCTION-CRITICAL component for web-based MCP servers, browser integrations, and web service integrations. Operating with 0% coverage violates the "zero defects" principle. The **Poka-yoke** approach requires comprehensive tests to prevent HTTP connection failures, TLS security vulnerabilities, retry logic bugs, or protocol violations from reaching production. **Kaizen** demands continuous measurement - we must measure actual coverage, not assume tests work because they exist. The **Heijunka** approach breaks this into phases: (1) analyze implementation, (2) implement real HTTP testing, (3) add error handling tests, (4) test TLS security, (5) validate coverage. This is **Andon** - a visible quality gate failure that MUST be resolved before production deployment. HTTP transport is used for web-based MCP servers - any defect here could cause massive service disruption for browser and web service integrations.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `apps/erlmcp_transports/src/erlmcp_transport_http.erl` (53 lines)
    - Lines 1-8: Module declaration, behavior (erlmcp_transport), exports
    - Lines 10-24: Type definitions (http_opts with url, owner, method, headers, timeout, ssl_options, pool_size)
    - Lines 26-44: Transport behavior callbacks (init/1, send/2, close/1) - delegates to erlmcp_transport_http_server
    - Lines 46-52: API function (start_link/1) - delegates to erlmcp_transport_http_server
    - **Pattern**: Thin wrapper module that delegates all functionality to erlmcp_transport_http_server gen_server

  - `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl` (635 lines)
    - Lines 1-12: Module declaration, gen_server behavior, exports
    - Lines 13-34: State record definition (url, owner, method, headers, timeouts, gun_pid, gun_monitor, gun_stream_ref, pending_requests, message_queue, pool_size, ssl_options, host, port, path, scheme)
    - Lines 36-48: Type definitions (http_opts, state)
    - Lines 50-56: Default values (DEFAULT_TIMEOUT=30000, DEFAULT_CONNECT_TIMEOUT=5000, DEFAULT_MAX_RETRIES=3, DEFAULT_RETRY_DELAY=1000, DEFAULT_POOL_SIZE=5, DEFAULT_METHOD=post)
    - Lines 58-64: API function (start_link/1)
    - Lines 66-90: gen_server init/1 - ensure_apps_started, monitor owner, build_initial_state, connect
    - Lines 92-110: gen_server handle_call/3 - send request, get_state, extract_session_id
    - Lines 112-114: gen_server handle_cast/2
    - Lines 116-217: gen_server handle_info/2 - gun_up, gun_down, gun_response, gun_data, gun_error, retry_request, owner down, gun monitor down
    - Lines 219-228: gen_server terminate/2 and code_change/3
    - Lines 230-254: Internal functions - ensure_apps_started (gun, ssl, crypto)
    - Lines 256-338: Initialization functions - build_initial_state, normalize_url, parse_url, parse_url_parts, parse_host_port, normalize_headers, to_binary, merge_headers
    - Lines 352-396: Connection management - connect/1 (opens gun connection, awaits up), build_gun_opts/1 (builds gun options with TLS), build_strict_tls_options/1
    - Lines 398-442: Request handling - enqueue_request, process_request_queue, send_request, perform_request (gun:post or gun:get)
    - Lines 444-571: Response handling - handle_gun_response, handle_gun_error, process_response, process_body, should_retry, schedule_retry, retry_request, calculate_retry_delay
    - Lines 403-410: **CRITICAL SECURITY FIX (CVSS 9.8)** - Calls erlmcp_tls_validation:build_tls_options/2 (module doesn't exist yet)

  - `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl` (168 lines)
    - Lines 1-6: Module declaration, eunit include
    - Lines 8-19: Test fixture (http_transport_test_) with 6 trivial tests
    - Lines 21-28: Setup/cleanup (ensure gun and ssl started)
    - Lines 30-77: Trivial tests (parse_http_url, parse_https_url, parse_url_with_port, parse_url_with_path, normalize_headers, transport_init)
    - Lines 79-98: Test that only checks if opts is a map (no actual Gun integration)
    - Lines 100-168: Commented out integration tests that would require mock HTTP server (test_send_post_request, test_send_get_request, test_retry_on_error)
    - **Problem**: Tests don't actually test Gun client integration, HTTP protocol, error handling, or retry logic

- **Patterns**:
  - **OTP Pattern**: gen_server behavior in erlmcp_transport_http_server.erl
  - **Process Pattern**: Single gen_server process per HTTP transport instance
  - **Supervision**: Monitors owner process death, monitors Gun process death
  - **Test Pattern**: Current tests are trivial - don't follow Chicago School TDD (no real Gun connections, no actual HTTP protocol testing)
  - **Gun Integration**: Uses gun:open/2 for connections, gun:post/4 and gun:get/3 for requests
  - **Connection Pooling**: pool_size parameter limits concurrent active requests (default: 5)
  - **Request Queuing**: queue:queue for pending requests when pool is full
  - **Retry Logic**: Exponential backoff with jitter (1s → 60s max), max_retries configurable (default: 3)
  - **HTTP/2 Support**: Configured via protocols => [http2, http] in build_gun_opts

- **Tests**: 6 trivial test functions exist, but 0% coverage reported
- **Quality**: FAIL - 0% coverage (threshold: ≥80%)

### Key Files
- `apps/erlmcp_transports/src/erlmcp_transport_http.erl:1-53` - Thin wrapper module that delegates to gen_server
- `apps/erlmcp_transports/src/erlmcp_transport_http.erl:31-34` - init/1 delegates to erlmcp_transport_http_server:start_link/1
- `apps/erlmcp_transports/src/erlmcp_transport_http.erl:37-39` - send/2 delegates to gen_server:call({send, Data})
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:66-90` - gen_server init/1 with connection establishment
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:92-99` - handle_call({send, Data}) - queues and processes requests
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:120-123` - handle_info({gun_up, GunPid, Protocol}) - connection established
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:126-136` - handle_info({gun_down, GunPid, Protocol, Reason, _}) - connection down, reconnect
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:139-175` - handle_info({gun_response, ...}) and handle_info({gun_data, ...}) - response handling
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:184-192` - handle_info({gun_error, GunPid, StreamRef, Reason}) - error handling with retry
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:356-396` - connect/1 function (gun:open/2, gun:await_up/2)
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:398-427` - build_gun_opts/1 (HTTP/2, HTTP/1.1, TLS options)
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:403-410` - **CRITICAL SECURITY FIX** - Calls erlmcp_tls_validation:build_tls_options/2 (module doesn't exist)
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:429-442` - build_strict_tls_options/1 (verify_peer, server_name_indication, TLS 1.2/1.3)
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:454-469` - process_request_queue/1 (pool_size limiting)
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:471-497` - send_request/2 (perform_request, track pending_requests)
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:499-516` - perform_request/2 (gun:post/4 or gun:get/3)
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:518-548` - handle_gun_response/4 (process_response, retry logic)
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:550-571` - handle_gun_error/3 (retry with exponential backoff)
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:573-601` - process_response/3 (status code validation, body processing)
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:603-613` - should_retry/3 (retry on 5xx, 429, gun_error)
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:615-634` - schedule_retry/4, retry_request/3, calculate_retry_delay/2 (exponential backoff with jitter)
- `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl:1-168` - Current test file with 6 trivial tests (0% coverage)
- `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl:100-168` - Commented out integration tests (need real HTTP server)
- `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl:67-88` - Reference test pattern for transport testing (Chicago School TDD)
- `apps/erlmcp_transports/test/erlmcp_transport_behavior_SUITE.erl:1-100` - Reference behavior compliance test pattern
- `docs/COVERAGE_REPORT.md:83` - Reports erlmcp_transport_http at 0% coverage
- `rebar.config:48` - Gun dependency (2.0.1)
- `apps/erlmcp_transports/src/erlmcp_transports.app.src:15` - Gun application dependency

### OTP Patterns Observed
- **Behavior**: gen_server (erlmcp_transport_http_server.erl)
- **Supervision**: Not supervised directly (started by owner process), monitors owner and Gun process
- **Process Pattern**: Single gen_server process per HTTP transport instance
- **Test Pattern**: Current tests are trivial and don't follow Chicago School TDD (need real Gun connections, actual HTTP protocol testing)

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - `erlmcp_transport_behavior` - Transport behavior specification (not directly implemented by http wrapper, but server implements callbacks)
  - `erlmcp_tls_validation` - TLS certificate validation (MODULE DOESN'T EXIST YET - called at line 403, CRITICAL SECURITY FIX)
- **External Libraries**:
  - **gun (2.0.1)** - HTTP/1.1 and HTTP/2 client library (critical dependency)
  - **ssl** - OTP SSL application for TLS/SSL
  - **crypto** - OTP crypto application (dependency of SSL)
- **OTP Applications**: kernel, stdlib, ssl, gun

### HTTP Transport Architecture
The HTTP transport uses a **gen_server with Gun client**:

1. **Client Mode Only**:
   - Gun HTTP client for making requests to HTTP servers
   - Supports HTTP/1.1 and HTTP/2 protocols
   - Configurable connection pool (pool_size, default: 5)
   - Request queuing when pool is full

2. **Connection Management**:
   - Automatic connection via gun:open/2
   - Connection up/down handling via gun_up and gun_down messages
   - Automatic reconnection on connection failure
   - Gun process monitoring with DOWN messages

3. **Request Handling**:
   - POST and GET methods supported
   - JSON content-type by default (configurable headers)
   - Request queuing with pool_size limiting
   - Pending request tracking via maps

4. **Response Handling**:
   - Streaming response handling via gun_data messages
   - Status code validation (2xx success, 4xx/5xx errors)
   - Content-type validation (application/json)
   - Response delivery to owner process via {transport_message, Response}

5. **Retry Logic**:
   - Exponential backoff with jitter (1s → 60s max)
   - Retry on server errors (5xx), rate limiting (429), Gun errors
   - Configurable max_retries (default: 3)
   - Configurable retry_delay (default: 1000ms)

6. **TLS Security**:
   - **CRITICAL SECURITY FIX (CVSS 9.8)** at line 403: Calls erlmcp_tls_validation:build_tls_options/2
   - Strict TLS defaults: verify_peer REQUIRED, server_name_indication, TLS 1.2/1.3
   - Fallback to build_strict_tls_options/1 if validation module fails
   - **PROBLEM**: erlmcp_tls_validation module doesn't exist yet

### TCPS Quality Gates to Pass
- [ ] **Compilation**: 0 errors (both modules compile successfully)
- [ ] **EUnit**: 100% pass rate (current 6 trivial tests + comprehensive new tests)
- [ ] **Common Test**: 100% pass rate (if applicable)
- [ ] **Coverage**: ≥80% (current: 0%, gap: 80 percentage points)
- [ ] **Dialyzer**: 0 warnings
- [ ] **Xref**: 0 undefined function calls (gun functions in xref_ignores at rebar.config:180-181)
- [ ] **Performance**: <10% regression from baseline (if perf-critical code changed)

### Patterns to Follow
- **Gen Server Pattern**: `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:66-228` - Reference for gen_server implementation
- **Test Pattern**: `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl:67-88` - Chicago School TDD with setup/cleanup
- **Behavior Compliance**: `apps/erlmcp_transports/test/erlmcp_transport_behavior_SUITE.erl:1-100` - Transport behavior compliance tests
- **Gun Integration**: `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl:356-396` - Gun connection management
- **Request/Response Handling**: Lines 92-175 in erlmcp_transport_http_server.erl - Gun message handling
- **Error Handling**: Lines 184-214 in erlmcp_transport_http_server.erl - Gun error handling with retry
- **Type Specs**: Lines 36-48 in erlmcp_transport_http_server.erl - Comprehensive Dialyzer specs

## Root Cause Analysis (5 Whys)

**Problem**: HTTP transport has 0% test coverage despite having a test file with 168 lines and 6 test functions.

1. **Why?** The existing test file (erlmcp_transport_http_tests.erl) contains only trivial tests that check if maps are maps, without any actual Gun client integration or HTTP protocol testing.

2. **Why?** The integration tests that would test real Gun functionality are commented out (lines 100-168) because they require a mock HTTP server, and no test infrastructure was set up for real HTTP testing.

3. **Why?** Focus was on getting the HTTP transport implementation working rather than on comprehensive testing. The test file was created as a placeholder but not properly implemented.

4. **Why?** TCPS quality gates (Jidoka, Poka-yoke) were not applied - no "stop-the-line" check for coverage before considering the HTTP transport complete. No validation that tests actually exercise production code paths.

5. **ROOT CAUSE**: Incomplete quality gate enforcement and lack of Chicago School TDD discipline. The manufacturing objective requires ≥80% coverage, but the test implementation was skipped. This violates the "measure everything" principle of Kaizen and the "verify everything" principle of Jidoka. Additionally, there's a MISSING MODULE (erlmcp_tls_validation) that is called for CRITICAL SECURITY (CVSS 9.8) but doesn't exist, which will cause runtime crashes.

**Solution**: Apply TCPS manufacturing discipline:
1. **Create TLS Validation Module**: Create erlmcp_tls_validation.erl module or mock for testing (CRITICAL SECURITY)
2. **Implement Real HTTP Testing**: Set up test HTTP server (using cowboy or inets), test real Gun client connections
3. **Test All Code Paths**: Implement tests for Gun integration, HTTP protocol, error handling, retry logic, TLS validation
4. **Validate Coverage**: Run `rebar3 cover --module=erlmcp_transport_http` and `rebar3 cover --module=erlmcp_transport_http_server` to verify ≥80% coverage threshold
5. **Automate Quality Gates**: Add coverage check to quality gates for future transport testing

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| Missing erlmcp_tls_validation module | P0 | CRITICAL - Runtime crash on HTTPS connections, CVSS 9.8 security vulnerability | Create erlmcp_tls_validation.erl module OR mock it for testing, verify line 403 doesn't crash |
| Gun client not tested | P0 | Critical code path uncovered - every HTTP request uses Gun | Test gun:open/2, gun:post/4, gun:get/3, gun:await_up/2 with real connections |
| HTTP protocol not tested | P0 | No validation of HTTP/1.1 or HTTP/2 functionality | Test POST requests with JSON body, headers, status codes, response bodies |
| Error handling not tested | P0 | Production errors could crash handlers or cause silent failures | Test connection refused, timeout, HTTP 4xx/5xx errors, Gun errors |
| Retry logic not tested | P1 | Retry could fail in production causing request loss | Test exponential backoff, max_retries, retry on 5xx/429/Gun errors |
| TLS security not tested | P0 | CRITICAL SECURITY - TLS certificate validation bypassed | Test TLS options, verify_peer, server_name_indication, strict defaults |
| Connection pooling not tested | P1 | Pool exhaustion could cause request queuing issues | Test pool_size limiting, request queuing, active_requests tracking |
| Request queuing not tested | P1 | Queue overflow could cause request loss | Test message_queue, enqueue_request, process_request_queue |
| Gun process monitoring not tested | P1 | Gun process crash could cause silent failures | Test gun_down, Gun monitor DOWN, automatic reconnection |
| Owner monitoring not tested | P2 | Owner death could leave Gun process orphaned | Test owner DOWN message, cleanup on owner death |
| Response streaming not tested | P1 | Large responses could be handled incorrectly | Test gun_data with fin/nofin, body accumulation |
| Header normalization not tested | P2 | Invalid headers could cause HTTP protocol violations | Test normalize_headers, to_binary, merge_headers |
| URL parsing not tested | P2 | Invalid URLs could cause connection failures | Test parse_url, parse_url_parts, parse_host_port for HTTP/HTTPS, ports, paths |
| Keepalive not tested | P2 | Connection reuse could fail causing performance issues | Test http_opts keepalive, http2_opts keepalive, connection reuse |
| Concurrent requests not tested | P0 | Pool_size limiting could fail under load | Test multiple concurrent requests exceeding pool_size |
| Timeout handling not tested | P1 | Timeouts could cause hangs or resource leaks | Test connect_timeout, request_timeout, gun:await_up timeout |
| Status code validation not tested | P1 | Invalid status codes could cause incorrect error handling | Test process_response for 2xx success, 4xx client errors, 5xx server errors |
| Content-type validation not tested | P2 | Non-JSON responses could cause parsing errors | Test process_body content-type checking, JSON validation |

**Severity Definitions:**
- **P0 (Critical):** BLOCKS production - MUST fix immediately
- **P1 (High):** Major quality gap - MUST fix before release
- **P2 (Medium):** Important but not blocking
- **P3 (Low):** Nice-to-have

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Requirements with acceptance criteria (≥80% coverage, all tests pass, TLS security validated)
2. **Pseudocode** - Test plan BEFORE implementation (test infrastructure, Gun client integration, HTTP protocol testing, error handling, TLS security)
3. **Architecture** - Test infrastructure: test HTTP server (cowboy or inets), Gun client connections, real HTTP/1.1 and HTTP/2 testing, TLS certificate validation
4. **Refinement** - Chicago School TDD (real Gun connections, actual HTTP server, no mocks for Gun)
5. **Completion** - All quality gates passing (compilation, tests, coverage ≥80%, dialyzer, xref)

**Implementation Strategy:**

Phase 1: **Create Missing TLS Validation Module** (P0 CRITICAL SECURITY)
- Create `apps/erlmcp_transports/src/erlmcp_tls_validation.erl` module
- Implement `build_tls_options/2` function that validates and builds TLS options
- OR create mock module for testing that returns strict TLS defaults
- Prevent runtime crash on HTTPS connections
- Verify line 403 in erlmcp_transport_http_server.erl doesn't crash

Phase 2: **Set Up Test Infrastructure** (Kaizen - Measure Everything)
- Start test HTTP server using cowboy or inets
- Configure test server on localhost port (e.g., 8080, 8081)
- Implement test server handlers for POST/GET requests
- Support both HTTP/1.1 and HTTP/2 protocols
- Support success responses (200), client errors (4xx), server errors (5xx)
- Support TLS/SSL for HTTPS testing

Phase 3: **Implement Gun Client Integration Tests** (Poka-yoke - Error-Proofing)
- Test gun:open/2 connection establishment
- Test gun:await_up/2 connection up detection
- Test gun:post/4 with JSON body
- Test gun:get/3 with query parameters
- Test gun_response message handling (fin, nofin)
- Test gun_data message handling (fin, nofin)
- Test gun_error message handling
- Test gun_up and gun_down messages
- Test Gun process monitoring with DOWN messages

Phase 4: **Implement HTTP Protocol Tests** (Jidoka - Built-in Quality)
- Test POST requests with JSON body
- Test GET requests with query parameters
- Test header normalization (content-type, accept, user-agent)
- Test status code validation (2xx success, 4xx client error, 5xx server error)
- Test response body processing
- Test content-type validation (application/json)
- Test HTTP/1.1 vs HTTP/2 protocols
- Test request timeout handling
- Test connection timeout handling

Phase 5: **Implement Error Handling Tests** (Poka-yoke - Error-Proofing)
- Test connection refused (server not running)
- Test connection timeout (server doesn't respond)
- Test HTTP 4xx errors (400 Bad Request, 404 Not Found, 429 Rate Limit)
- Test HTTP 5xx errors (500 Internal Server Error, 502 Bad Gateway, 503 Service Unavailable)
- Test Gun client errors (connection closed, protocol error)
- Test TLS/SSL errors (certificate validation failure, handshake failure)
- Test retry logic on errors (exponential backoff, max_retries)
- Test request queuing when pool is full

Phase 6: **Implement TLS Security Tests** (P0 CRITICAL SECURITY)
- Test TLS certificate validation (verify_peer REQUIRED)
- Test server_name_indication (SNI)
- Test TLS version negotiation (TLS 1.2, TLS 1.3)
- Test cipher suite selection
- Test TLS handshake success
- Test TLS handshake failure (invalid certificate, expired certificate)
- Test strict TLS defaults in build_strict_tls_options/1
- Test erlmcp_tls_validation:build_tls_options/2 (if module exists)

Phase 7: **Validate Coverage** (Jidoka - Stop-the-Line Quality)
- Run `rebar3 cover --module=erlmcp_transport_http`
- Run `rebar3 cover --module=erlmcp_transport_http_server`
- Verify ≥80% coverage threshold met
- All tests must pass (100% pass rate)
- No compilation errors
- No dialyzer warnings
- No xref errors

Phase 8: **Stress Testing** (Heijunka - Production Leveling)
- Test concurrent request handling (100+ simultaneous requests)
- Test connection pool exhaustion (exceed pool_size)
- Test rapid request cycles (connect, send, close, repeat)
- Test large request/response handling (buffer overflow scenarios)
- Test long-running connections (memory leak detection)
- Test retry under sustained error conditions

**Quality Validation:**

Automated:
```bash
# Compile
rebar3 compile --app erlmcp_transports

# Run tests
rebar3 eunit --module=erlmcp_transport_http_tests

# Measure coverage
rebar3 cover --module=erlmcp_transport_http
rebar3 cover --module=erlmcp_transport_http_server

# Type checking
rebar3 dialyzer --app erlmcp_transports

# Cross-reference
rebar3 xref --app erlmcp_transports
```

Manual:
- Review coverage report HTML for uncovered lines
- Verify all test functions exercise production code paths (not test_mode bypass)
- Verify Gun client lifecycle under stress
- Verify concurrent request handling (pool_size limiting)
- Verify TLS certificate validation under various scenarios
- Verify retry logic with exponential backoff

Metrics to Measure:
- **Coverage Percentage**: Target ≥80%, Current: 0%
- **Test Pass Rate**: Target 100% (6 existing + comprehensive new tests)
- **Concurrent Requests**: Test 100+ (production: pool_size default 5, configurable)
- **Request Throughput**: Requests per second under load
- **Memory Usage**: Per-connection heap size (detect leaks)
- **Retry Success Rate**: Under error scenarios (5xx, 429, Gun errors)
- **TLS Handshake Success Rate**: Under various certificate scenarios

## Open Questions
**NONE** - All research questions answered. Implementation can proceed with TCPS methodology.

**Note**: The primary blocking issue is the MISSING erlmcp_tls_validation module that is called at line 403 for CRITICAL SECURITY (CVSS 9.8). This must be resolved before HTTP transport can be used safely in production.

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) - 0% coverage due to trivial tests, missing real Gun integration
- [x] Quality gates defined (specific thresholds) - ≥80% coverage, 100% test pass rate
- [x] OTP patterns understood (behaviors, supervision) - gen_server behavior, Gun client integration
- [x] Test strategy clear (Chicago School TDD) - Real Gun connections, actual HTTP server, no mocks for Gun
- [x] Risk assessment complete (severity P0-P3) - 18 risks identified, 8 P0 critical risks
- [x] No open questions (all research complete) - Ready for implementation phase
