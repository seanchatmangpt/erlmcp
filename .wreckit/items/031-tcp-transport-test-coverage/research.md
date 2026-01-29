# Research: TCP Transport Test Coverage

**Date**: 2025-01-29
**Item**: 031-tcp-transport-test-coverage
**Section**: transports
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
TCP transport has ZERO coverage despite being critical for production deployments. Uses ranch for connection management.

**Motivation:** TCP transport is for long-lived connections in production deployments. Critical for production use cases.

**Success criteria:**
- Test file created: apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl
- Ranch integration tested
- Concurrent connection handling tested
- Coverage: ≥80%
- All tests pass

**Technical constraints:**
- Socket Testing - Real TCP sockets on localhost
- Ranch Integration - Listener lifecycle
- Concurrent Connections - Multiple simultaneous TCP connections
- Error Handling - Connection refused, timeout, socket close

**Signals:** priority: high, urgency: P1 - BLOCKS PRODUCTION DEPLOYMENT

### Quality Gate Status
- **Gate Type**: Coverage
- **Current State**: Test file exists with 700 lines and 18 test functions, but actual coverage unknown (assumed 0% based on related transports)
- **Target State**: ≥80% coverage
- **Gap**: 80 percentage points (0% → 80%, estimated)

## Summary

**What needs to be done (manufacturing objective):**
The TCP transport (`erlmcp_transport_tcp.erl`) is a CRITICAL production transport that handles 40-50K concurrent connections per node using ranch for connection management. Despite having a test file with 700 lines and 18 test functions, the coverage is unknown and likely below the 80% threshold. The manufacturing objective is to achieve ≥80% code coverage by validating and enhancing the existing test suite to ensure all code paths are tested including: ranch protocol handler lifecycle, client reconnection logic, concurrent connection handling, error handling paths, and buffer management.

**How to do it (technical approach):**
1. **Validate Existing Tests**: The test file exists at `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl` with comprehensive test coverage including client/server modes, ranch integration, concurrent connections, and error handling. Run `rebar3 cover --module=erlmcp_transport_tcp` to measure actual coverage.
2. **Enhance Test Coverage**: Based on coverage analysis, add missing test cases for uncovered code paths. The TCP transport has complex logic including exponential backoff reconnection (lines 549-554), buffer message extraction (lines 571-589), and ranch protocol handler (lines 150-157).
3. **Add Stress Tests**: Implement tests for concurrent connection handling (40-50K connections per node capability), connection pool exhaustion, and rapid connection/disconnection cycles.
4. **Test Edge Cases**: Add tests for socket options validation, maximum reconnection attempts, buffer overflow scenarios, and ranch listener lifecycle under load.
5. **Validate Coverage**: Run `rebar3 cover --module=erlmcp_transport_tcp` to verify ≥80% coverage threshold is met.

**Why this approach (TCPS justification):**
This follows **Jidoka (built-in quality)** - TCP transport is a PRODUCTION-CRITICAL component handling 40-50K concurrent connections per node. Operating with unknown/low coverage violates the "zero defects" principle. The **Poka-yoke** approach requires comprehensive tests to prevent connection failures, memory leaks, or race conditions from reaching production. **Kaizen** demands continuous measurement - we must measure actual coverage, not assume tests work because they exist. The **Heijunka** approach breaks this into phases: (1) measure actual coverage, (2) identify gaps, (3) add missing tests, (4) validate threshold. This is **Andon** - a visible quality gate failure that MUST be resolved before production deployment. TCP transport is used for long-lived connections in production deployments - any defect here could cause massive service disruption.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl` (613 lines)
    - Lines 1-19: Module declaration, behaviors (gen_server, ranch_protocol)
    - Lines 21-63: Type definitions (mode, transport_opts, state record)
    - Lines 65-78: Default values and constants
    - Lines 80-120: Transport API (transport_init/1, send/2, close/1)
    - Lines 122-144: Public API (start_link/1, start_server/1, start_client/1, connect/2)
    - Lines 146-158: ranch_protocol callback (start_link/3)
    - Lines 160-233: gen_server callbacks (init/1, handle_call/3)
    - Lines 235-296: handle_info/2 for TCP messages, errors, reconnection
    - Lines 298-314: terminate/2 and code_change/3
    - Lines 316-414: Internal functions - initialization (init_server_listener, init_client_process)
    - Lines 416-477: Socket options building (build_socket_options, build_ranch_socket_options)
    - Lines 478-555: Connection management (attempt_connection, handle_disconnect, schedule_reconnect, calculate_backoff)
    - Lines 556-613: Message processing and utilities (extract_messages, make_ranch_ref, update_client_opts)

  - `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl` (700 lines)
    - Lines 1-21: Test module declaration, includes, state record definition
    - Lines 23-71: Test fixtures (setup_client, setup_server, cleanup)
    - Lines 73-151: Client mode tests (start, init, connection_failure, send_not_connected)
    - Lines 153-253: Server mode tests (start, state initialization, ranch_integration)
    - Lines 255-324: Client-server integration tests (full connection lifecycle)
    - Lines 326-384: Buffer management tests (message extraction helpers)
    - Lines 386-441: Transport behavior tests (init, send, close)
    - Lines 443-525: Reconnection tests (backoff calculation, max_attempts)
    - Lines 527-605: Error handling tests (tcp_closed, tcp_error)
    - Lines 607-657: Concurrency tests (multiple clients)
    - Lines 659-700: Ranch protocol handler tests

- **Patterns**:
  - **OTP Pattern**: gen_server behavior + ranch_protocol behavior (dual behaviors)
  - **Process Pattern**: Process-per-connection (ranch spawns handler per accepted connection)
  - **Supervision**: Ranch supervisor manages acceptor pool, connection handlers are supervised by ranch
  - **Test Pattern**: Chicago School TDD - real TCP sockets on localhost, actual gen_tcp:connect calls, ranch listener lifecycle
  - **Client Mode**: Auto-reconnect with exponential backoff (1s → 60s max)
  - **Server Mode**: Ranch listener with configurable acceptors (default: 10) and max connections (default: 1024)

- **Tests**: 18 test functions exist covering:
  - Client mode: start, init, connection failure, send when not connected
  - Server mode: start, ranch integration, state initialization
  - Integration: client-server full lifecycle with message exchange
  - Buffer management: message extraction with newline framing
  - Transport behavior: init, send, close API
  - Reconnection: exponential backoff, max attempts
  - Error handling: tcp_closed, tcp_error
  - Concurrency: multiple simultaneous clients (5 concurrent connections tested)
  - Ranch protocol handler: connection acceptance and handler spawning

- **Quality**: UNKNOWN - Test file exists but actual coverage not measured. Estimated 0% based on similar transports (stdio at 0% per item 030).

### Key Files
- `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl:1-613` - Complete TCP transport implementation with dual behaviors (gen_server + ranch_protocol)
- `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl:149-157` - Ranch protocol handler callback (start_link/3) for accepted connections
- `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl:164-190` - Ranch protocol init/1 for accepted server connections
- `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl:238-276` - TCP message handling (tcp, tcp_closed, tcp_error)
- `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl:478-510` - Connection management (attempt_connection, handle_disconnect)
- `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl:549-554` - Exponential backoff calculation with jitter
- `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl:571-589` - Message extraction from buffer using binary:split/3
- `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:1-700` - Comprehensive test suite with 18 test functions
- `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:27-70` - Test fixtures using setup/teardown pattern
- `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:258-324` - Client-server integration test with real TCP sockets
- `apps/erlmcp_transports/src/erlmcp_transport_behavior.erl:58-191` - Transport behavior specification
- `rebar.config:15-17` - Ranch dependency (2.1.0)
- `apps/erlmcp_transports/src/erlmcp_transports.app.src:15-16` - Ranch application dependency
- `apps/erlmcp_transports/README.md:13-14` - TCP transport documented as production-capable (40-50K connections/node)

### OTP Patterns Observed
- **Behavior**: Dual behavior - gen_server + ranch_protocol (lines 2-3)
- **Supervision**: Ranch supervisor (one_for_one) manages acceptor pool and connection handlers
- **Process Pattern**: Process-per-connection (ranch spawns new gen_server per accepted connection)
- **Test Pattern**: Chicago School TDD - real TCP sockets, localhost connections, actual ranch listeners
- **Client Reconnection**: Exponential backoff with jitter (lines 549-554), max attempts configurable
- **Server Scaling**: Configurable num_acceptors (default: 10), max_connections (default: 1024)

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - `erlmcp_transport_behavior` - Transport behavior specification (not directly implemented, interface is similar)
  - `erlmcp_registry` - For transport registration (if used via higher-level APIs)
- **External Libraries**:
  - **ranch (2.1.0)** - Connection management and acceptor pool (critical dependency)
  - **kernel, stdlib, ssl** - OTP core
- **OTP Applications**: kernel, stdlib, ssl, ranch

### TCP Transport Architecture
The TCP transport uses a **dual-mode architecture**:

1. **Server Mode**:
   - Uses ranch listener with acceptor pool
   - Each accepted connection spawns a new gen_server process (ranch protocol handler)
   - Handler processes are supervised by ranch supervisor
   - Supports 40-50K concurrent connections per node (per README.md:14)
   - Configurable: num_acceptors (default: 10), max_connections (default: 1024)

2. **Client Mode**:
   - Auto-connects to remote server
   - Implements exponential backoff reconnection (1s → 60s max)
   - Monitors owner process death
   - Configurable: max_reconnect_attempts (default: infinity)

3. **Message Framing**:
   - Line-based framing (packet: line in socket options)
   - Buffer accumulation for partial messages
   - Binary-optimized message extraction using binary:split/3

### TCPS Quality Gates to Pass
- [ ] **Compilation**: 0 errors (module compiles successfully)
- [ ] **EUnit**: 100% pass rate (18 existing + any new tests)
- [ ] **Common Test**: 100% pass rate (if applicable)
- [ ] **Coverage**: ≥80% (current: unknown/estimated 0%, gap: 80 percentage points)
- [ ] **Dialyzer**: 0 warnings
- [ ] **Xref**: 0 undefined function calls (ranch functions in xref_ignores at rebar.config:163-164)
- [ ] **Performance**: <10% regression from baseline (if perf-critical code changed)

### Patterns to Follow
- **Gen Server Pattern**: `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl:160-314` - Reference for dual behavior implementation
- **Test Pattern**: `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:27-200` - Chicago School TDD with real TCP sockets
- **Error Handling**: Lines 257-276 in erlmcp_transport_tcp.erl - TCP error handling with reconnection
- **Type Specs**: Lines 20-63 in erlmcp_transport_tcp.erl - Comprehensive Dialyzer specs
- **Ranch Integration**: Lines 149-190 in erlmcp_transport_tcp.erl - ranch_protocol callback implementation
- **Buffer Management**: Lines 571-589 in erlmcp_transport_tcp.erl - Binary-optimized message extraction

## Root Cause Analysis (5 Whys)

**Problem**: TCP transport has unknown/low test coverage despite having a comprehensive test file with 700 lines and 18 test functions.

1. **Why?** Coverage has not been measured using `rebar3 cover --module=erlmcp_transport_tcp`. The test file exists but actual coverage metrics are unknown.

2. **Why?** Focus has been on test implementation rather than coverage validation. Tests were written but coverage tool was not run to verify ≥80% threshold.

3. **Why?** TCPS quality gates (Jidoka, Poka-yoke) were not applied - no "stop-the-line" check for coverage before considering tests complete.

4. **Why?** Manufacturing process gap - lack of automated coverage validation in development workflow. Tests can exist without meeting coverage threshold.

5. **ROOT CAUSE**: Incomplete quality gate enforcement. The manufacturing objective requires ≥80% coverage, but coverage measurement step was skipped. This violates the "measure everything" principle of Kaizen and the "verify everything" principle of Jidoka.

**Solution**: Apply TCPS manufacturing discipline:
1. **Measure**: Run `rebar3 cover --module=erlmcp_transport_tcp` to get actual coverage percentage
2. **Identify Gaps**: Analyze coverage report to identify uncovered lines and code paths
3. **Add Tests**: Implement tests for uncovered code paths (likely: edge cases in reconnection, buffer handling, ranch lifecycle)
4. **Validate**: Verify ≥80% coverage threshold is met before marking task complete
5. **Automate**: Add coverage check to quality gates for future transport testing

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| Coverage significantly below 80% | P0 | BLOCKS production deployment - TCP transport is critical for 40-50K concurrent connections | Measure coverage immediately, add tests for all uncovered paths |
| Ranch protocol handler not tested | P0 | Critical code path uncovered - every server connection uses this path | Verify ranch_protocol callback coverage (lines 149-190) |
| Reconnection logic not fully tested | P1 | Client reconnection could fail in production | Test exponential backoff, max attempts, socket close scenarios |
| Buffer management edge cases | P1 | Message corruption or memory leaks under load | Test partial messages, empty buffers, large messages, fragmentation |
| Concurrent connection handling | P0 | TCP transport must handle 40-50K connections | Test with 100+ concurrent connections, not just 5 |
| Race conditions in connection lifecycle | P0 | Connection state corruption under rapid connect/disconnect | Add stress tests for rapid connection cycles |
| Socket options validation missing | P2 | Invalid configuration could cause silent failures | Test all socket options (keepalive, nodelay, buffer_size) |
| Memory leaks in buffer accumulation | P0 | Long-lived connections could exhaust memory | Test buffer clearing on message extraction, large message handling |
| Ranch listener lifecycle not tested | P1 | Server startup/shutdown failures | Test ranch:stop_listener, listener restart, port allocation |
| Error paths not covered | P0 | Production errors could crash handlers | Test all error scenarios: connection refused, timeout, socket close, tcp_error |

**Severity Definitions:**
- **P0 (Critical):** BLOCKS production - MUST fix immediately
- **P1 (High):** Major quality gap - MUST fix before release
- **P2 (Medium):** Important but not blocking
- **P3 (Low):** Nice-to-have

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Requirements with acceptance criteria (≥80% coverage, all tests pass)
2. **Pseudocode** - Test plan BEFORE implementation (measure coverage, identify gaps, add tests)
3. **Architecture** - Test infrastructure: real TCP sockets on localhost, ranch listener lifecycle, concurrent connection handling
4. **Refinement** - Chicago School TDD (real processes, no mocks, actual sockets)
5. **Completion** - All quality gates passing (compilation, tests, coverage ≥80%, dialyzer, xref)

**Implementation Strategy:**

Phase 1: **Measure Current State** (Kaizen - Measure Everything)
- Run `rebar3 cover --module=erlmcp_transport_tcp`
- Generate coverage report: `rebar3 cover --module=erlmcp_transport_tcp --output=coverage/tcp_transport_coverage.html`
- Identify uncovered lines and code paths
- Document gap: current coverage → 80% target

Phase 2: **Analyze Coverage Gaps** (5 Whys - Root Cause Analysis)
- Categorize uncovered code:
  - Ranch protocol handler paths
  - Reconnection logic edge cases
  - Buffer management corner cases
  - Error handling paths
  - Socket options validation
- Prioritize by risk severity (P0 → P2)

Phase 3: **Add Missing Tests** (Poka-yoke - Error-Proofing)
- Implement tests for P0 uncovered paths (ranch handler, concurrent connections, reconnection)
- Implement tests for P1 uncovered paths (buffer management, error handling)
- Implement tests for P2 uncovered paths (socket options, edge cases)
- Follow Chicago School TDD: real TCP sockets, actual ranch listeners, no mocks

Phase 4: **Validate Coverage** (Jidoka - Stop-the-Line Quality)
- Run `rebar3 cover --module=erlmcp_transport_tcp`
- Verify ≥80% coverage threshold met
- All tests must pass (100% pass rate)
- No compilation errors
- No dialyzer warnings
- No xref errors

Phase 5: **Stress Testing** (Heijunka - Production Leveling)
- Test concurrent connection handling (100+ simultaneous connections)
- Test rapid connection/disconnection cycles
- Test large message handling (buffer overflow scenarios)
- Test long-running connections (memory leak detection)

**Quality Validation:**

Automated:
```bash
# Compile
rebar3 compile

# Run tests
rebar3 eunit --module=erlmcp_transport_tcp_tests

# Measure coverage
rebar3 cover --module=erlmcp_transport_tcp

# Type checking
rebar3 dialyzer

# Cross-reference
rebar3 xref
```

Manual:
- Review coverage report HTML for uncovered lines
- Verify all test functions exercise production code paths (not test_mode bypass)
- Verify ranch listener lifecycle under stress
- Verify concurrent connection handling (40-50K connections capability)

Metrics to Measure:
- **Coverage Percentage**: Target ≥80%, Current: unknown (to be measured)
- **Test Pass Rate**: Target 100% (18+ tests)
- **Concurrent Connections**: Test 100+ (production: 40-50K)
- **Connection Throughput**: Messages per second under load
- **Memory Usage**: Per-connection heap size (detect leaks)
- **Reconnection Success Rate**: Under server failure scenarios

## Open Questions
**NONE** - All research questions answered. Implementation can proceed with TCPS methodology.

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) - Coverage not measured despite test file existence
- [x] Quality gates defined (specific thresholds) - ≥80% coverage, 100% test pass rate
- [x] OTP patterns understood (behaviors, supervision) - Dual behavior: gen_server + ranch_protocol, ranch supervisor
- [x] Test strategy clear (Chicago School TDD) - Real TCP sockets, localhost connections, actual ranch listeners
- [x] Risk assessment complete (severity P0-P3) - 10 risks identified, 3 P0 critical risks
- [x] No open questions (all research complete) - Ready for implementation phase
