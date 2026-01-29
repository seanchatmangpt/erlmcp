# Research: Create comprehensive EUnit test suite for erlmcp_client.erl

**Date**: 2025-01-18
**Item**: 002-create-comprehensive-eunit-test-suite-for-erlmcpcl
**Section**: coverage
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
Core client module has 0% test coverage - completely untested critical path

**Motivation:** This is the MOST CRITICAL test suite - all other client tests depend on patterns established here. Deploying without tests is unacceptable for production.

**Success criteria:**
- ≥80% code coverage achieved
- ≥50 tests created
- All public functions tested
- All handle_info/handle_call/handle_cast clauses tested
- All error paths tested
- All tests pass: rebar3 eunit --module=erlmcp_client_tests
- No race conditions in concurrent request testing
- Integration tests with real stdio transport pass
- Execution time <5 seconds

**Technical constraints:**
- Use Chicago School TDD - real processes, real gen_server, no mocks
- State-Based Verification - check #state{} record contents
- Test with real transports (stdio, tcp)
- Use erlmcp_session_manager_tests.erl as reference (527 lines, 25 tests, all passing)

**In scope:**
- Client lifecycle functions
- Connection management
- Request-response correlation
- Capability negotiation
- Error handling
- State management
**Out of scope:**
- Transport implementation details (separate modules)

**Signals:** priority: critical, urgency: P0 - BLOCKS ALL PRODUCTION WORK

### Quality Gate Status
- **Gate Type**: Coverage / EUnit / Integration
- **Current State**: 0% coverage (438 line module), existing test file has only ~20 basic tests with no real process interaction
- **Target State**: ≥80% coverage, ≥50 tests, 100% pass rate
- **Gap**: 80 percentage points, 30+ additional tests needed, complete rewrite needed

## Summary

The erlmcp_client module is the CRITICAL PATH component for all MCP client operations. It implements a gen_server behavior with 729 lines of complex logic including request-response correlation (via #state.pending_requests map), phase-based initialization (pre_initialization → initializing → initialized), capability negotiation, notification handling, and batch request management. The current test suite (erlmcp_client_tests.erl, 438 lines) provides ONLY superficial API tests that don't actually verify gen_server state transitions, request correlation, or error handling.

**What needs to be done:**
Create a Chicago School TDD test suite that uses REAL gen_server processes (no mocks), verifies state changes through sys:get_status/1 or direct state inspection, tests ALL handle_call/handle_cast/handle_info clauses, validates request ID correlation, tests phase transitions (pre_initialization → initializing → initialized), tests capability negotiation, tests notification routing, tests batch request lifecycle, and tests error paths (transport failures, invalid phases, request ID overflow).

**How to do it:**
Follow the pattern established in erlmcp_session_manager_tests.erl (527 lines, 25 tests, 100% passing) - use {foreach} setup/teardown for isolation, start REAL gen_server processes with start_link/1, use gen_server:call to send requests, verify state with sys:get_status/1 or pattern matching on return values, test concurrent operations with spawned processes, test ALL error conditions explicitly, and use proper assertions with ?assertMatch for complex data structures.

**Why this approach (TCPS justification):**
- **Jidoka (Built-in quality)**: Every test MUST stop on failure - no "assume pass" - verify actual #state{} contents
- **Poka-yoke (Error-proofing)**: Test design eliminates impossible states (e.g., calling list_resources before initialized)
- **Kaizen (Continuous improvement)**: Test coverage metrics drive improvement - measure → fix → measure
- **Heijunka (Leveling)**: Small, incremental test additions - each test is independent and verifiable
- **Andon (Visible signaling)**: Test failures are IMMEDIATELY visible with clear error messages (no silent failures)

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_client.erl` (729 lines) - Main gen_server implementation
  - `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_client_tests.erl` (438 lines) - Inadequate test coverage
  - `/Users/sac/erlmcp/apps/erlmcp_core/include/erlmcp.hrl` (523 lines) - Type definitions and constants

- **Patterns**:
  - gen_server behavior with 6 phases: pre_initialization, initializing, initialized, error, closed
  - Request-response correlation via #state.pending_requests map (request_id → {RequestType, FromPid})
  - Phase enforcement via ?CHECK_INITIALIZED macro
  - Batch request management via #state.batch_requests map (batch_id → [requests])
  - Notification routing via #state.notification_handlers map (method → handler)
  - Subscription tracking via sets:set()
  - Transport abstraction (stdio, tcp, http)

- **Tests**: Current test file has ~20 tests that only test API surface, not actual gen_server behavior:
  - Lines 14-65: Basic lifecycle (start_link, stop) - but only check if process is alive
  - Lines 71-112: Initialization - but skip if no actual server connected
  - Lines 118-161: Resources - but use `?assert(true)` to skip errors
  - Lines 167-200: Tools - but accept both ok and error as success
  - Lines 206-250: Prompts - but no actual state verification
  - Lines 256-282: Batch - but no verification of batch state
  - Lines 288-314: Notifications - but no actual notification delivery
  - Lines 320-344: Sampling - but no handler invocation verification
  - Lines 350-368: Strict mode - but only tests API call succeeds
  - Lines 374-410: Capability encoding - only pure function tests (adequate)

- **Quality**:
  - **Coverage**: Estimated <10% (only API calls tested, no internal state verification)
  - **Pass Rate**: 100% (but tests are not testing anything meaningful)
  - **Critical Gaps**:
    1. No handle_call clause coverage (lines 217-412 have 19 different patterns)
    2. No handle_info clause coverage (lines 418-437 have transport_message and EXIT handling)
    3. No request ID correlation testing (state#state.pending_requests never verified)
    4. No phase transition testing (pre_initialization → initializing → initialized never verified)
    5. No batch request state testing (state#state.batch_requests never verified)
    6. No notification handler testing (no actual delivery verification)
    7. No error path testing (transport failure, invalid phase, request ID overflow)
    8. No concurrent request testing (race conditions in request ID generation)

### Key Files
- `apps/erlmcp_core/src/erlmcp_client.erl:49-66` - State record definition with ALL fields
- `apps/erlmcp_core/src/erlmcp_client.erl:200-215` - init/1 callback with transport initialization
- `apps/erlmcp_core/src/erlmcp_client.erl:217-412` - handle_call/3 callback with 19 request patterns
- `apps/erlmcp_core/src/erlmcp_client.erl:414-416` - handle_cast/2 callback (empty, just {noreply, State})
- `apps/erlmcp_core/src/erlmcp_client.erl:418-437` - handle_info/2 callback with transport_message and EXIT handling
- `apps/erlmcp_core/src/erlmcp_client.erl:469-512` - send_request/4 with P0 SECURITY request ID overflow protection
- `apps/erlmcp_core/src/erlmcp_client.erl:597-628` - handle_response/3 with request correlation
- `apps/erlmcp_core/src/erlmcp_client.erl:652-680` - handle_notification/3 with routing logic
- `apps/erlmcp_core/test/erlmcp_session_manager_tests.erl:1-527` - REFERENCE PATTERN for Chicago School TDD

### OTP Patterns Observed
- **Behavior**: gen_server with 6 required callbacks (init, handle_call, handle_cast, handle_info, terminate, code_change)
- **Supervision**: Designed for one_for_one supervision via erlmcp_sup
- **Process Pattern**: gen_server with synchronous request-response (gen_server:call) and asynchronous notifications (handle_info)
- **Test Pattern**: Current tests are NOT Chicago School TDD - need to follow erlmcp_session_manager_tests.erl pattern

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - `erlmcp_json_rpc.erl` - JSON-RPC encoding/decoding (lines 45-118: encode_request, decode_message)
  - `erlmcp_transport_stdio.erl` - Stdio transport (for integration tests)
  - `erlmcp_transport_tcp.erl` - TCP transport (for integration tests)
  - `erlmcp_transport_http.erl` - HTTP transport (for integration tests)
  - `erlmcp_registry.erl` - Central message routing (optional, for advanced tests)

- **External Libraries**:
  - jsx (3.1.0) - JSON encoding/decoding
  - gproc (0.9.0) - Process registry (used by registry, not directly by client)
  - gun (2.0.1) - HTTP client (for http transport)
  - ranch (2.1.0) - TCP listener (for tcp transport)

- **OTP Applications**:
  - kernel - gen_server, supervisor
  - stdlib - sets, maps, proplists
  - sasl - logger (for error logging)

### TCPS Quality Gates to Pass
- [ ] **Compilation**: 0 errors (current: PASS)
- [ ] **EUnit**: 100% pass rate with ≥50 tests (current: PASS but inadequate)
- [ ] **Common Test**: Not applicable (unit tests only)
- [ ] **Coverage**: ≥80% (current: <10%, GAP: 70+ percentage points)
- [ ] **Dialyzer**: 0 warnings (current: unknown)
- [ ] **Xref**: 0 undefined function calls (current: PASS)
- [ ] **Performance**: <10% regression from baseline (N/A for tests)

### Patterns to Follow
- **Gen Server Pattern**: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_session_manager_tests.erl:14-60`
  - Use {foreach} for isolated tests with setup/cleanup
  - Start REAL gen_server processes with start_link/1
  - Use gen_server:call for synchronous requests
  - Verify state with sys:get_status/1 or pattern matching

- **Test Pattern**: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_session_manager_tests.erl:71-85`
  - Each test function returns a list of ?_assert assertions
  - Use ?assertMatch for complex data structures
  - Test success paths AND error paths explicitly
  - Use proper setup/cleanup to avoid test pollution

- **Error Handling**: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_client.erl:229-231, 242-244`
  - Phase enforcement: {error, {not_initialized, Phase, Message}}
  - Capability validation: {error, capability_not_supported}
  - Request ID overflow: {error, {request_id_overflow, Message}} (P0 SECURITY)

- **Type Specs**: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_client.erl:30-68`
  - Use -spec for all exported functions
  - Use -type for custom types (client(), transport_opts(), client_opts())
  - Use -record for state with field types

## Root Cause Analysis (5 Whys)

**Problem**: erlmcp_client has <10% test coverage despite being critical path for all client operations

1. **Why is coverage so low?**
   - Current tests only verify API calls succeed, not internal state changes
   - Tests skip error conditions with `?assert(true)` or `catch {error, _} -> ?assert(true)`
   - No verification of gen_server state transitions or request correlation

2. **Why do tests skip error conditions?**
   - Tests were written as "smoke tests" to verify API compiles and runs
   - No requirement for state-based verification (Chicago School TDD not enforced)
   - Focus was on getting basic functionality working, not comprehensive testing

3. **Why was Chicago School TDD not enforced?**
   - Early development focused on implementing protocol, not testing methodology
   - Session manager tests (erlmcp_session_manager_tests.erl) demonstrate the correct pattern but weren't followed
   - No quality gate requiring ≥80% coverage before merge

4. **Why was there no coverage quality gate?**
   - Project was in rapid development phase (v0.5.0 → v0.6.0)
   - Coverage requirements defined but not enforced (see CLAUDE.md lines 74-85)
   - Automated validation (claude-md-enforcer.sh) defined but not implemented

5. **ROOT CAUSE**: No automated enforcement of TCPS quality standards during development
   - Quality gates defined in documentation but not enforced by CI/CD
   - Manual review process missed the inadequate test coverage
   - Coverage measurement exists but no threshold enforcement

**Solution**: Implement comprehensive Chicago School TDD test suite with:
- Real gen_server processes (no mocks)
- State-based verification (sys:get_status/1 or pattern matching)
- ALL handle_call/handle_cast/handle_info clauses tested
- ALL phase transitions tested
- ALL error paths tested
- Concurrent request testing
- Integration tests with real transports
- Automated coverage enforcement (≥80% threshold)

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Test flakiness due to timing dependencies** | P0 | Tests fail intermittently, blocking CI/CD | Use {foreach} for isolation, avoid timer:sleep, use synchronous calls (gen_server:call), verify state directly instead of waiting |
| **Missing handle_call clause coverage** | P0 | Untested code paths may crash in production | Explicitly test ALL 19 handle_call patterns (lines 217-412), including error paths |
| **Race condition in concurrent request testing** | P1 | Tests pass in isolation but fail under load | Use spawned processes with message passing verification, test with 10+ concurrent requests, verify no request ID collisions |
| **State verification requires sys:get_status/1 which may fail** | P1 | Tests crash when trying to verify state | Wrap sys:get_status/1 in try/catch, fallback to pattern matching on return values, or use internal state inspection via -compile(export_all) |
| **Transport dependency for integration tests** | P1 | Tests fail if stdio/tcp/http not available | Use test_mode => true in transport options, skip integration tests if transport unavailable, or mock transport at behavior level (not gen_server level) |
| **Request ID overflow not tested** | P2 | P0 SECURITY vulnerability (lines 476-485) | Test with request_id = 16#7FFFFFFF (max 32-bit signed int), verify safe_increment/1 detects overflow |
| **Phase transition logic not tested** | P2 | Invalid state transitions may crash server | Test all phase transitions (pre_initialization → initializing → initialized), test invalid operations in each phase |
| **Batch request state not verified** | P2 | Batch requests may fail silently | Test batch lifecycle (start_batch → add_to_batch → execute_batch/cancel_batch), verify state#state.batch_requests |
| **Notification handler delivery not tested** | P3 | Notifications may not reach handlers | Test notification routing (lines 652-680), spawn handler processes, verify handler receives message |
| **Subscription tracking not tested** | P3 | Resource subscriptions may leak memory | Test subscribe/unsubscribe, verify sets:add_element/del_element, verify no memory leaks |

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Requirements with acceptance criteria (≥80% coverage, ≥50 tests, all clauses tested)
2. **Pseudocode** - Algorithm design BEFORE coding (see test structure below)
3. **Architecture** - Integration points and dependencies (real transports, gen_server behavior)
4. **Refinement** - Chicago School TDD (tests FIRST, verify failures, THEN implement)
5. **Completion** - All quality gates passing (compile, eunit, coverage, dialyzer, xref)

**Implementation Strategy:**

### Phase 1: Foundation (Setup/Teardown and Lifecycle)
```erlang
%% Test structure following erlmcp_session_manager_tests.erl pattern
client_test_() ->
    {foreach,
     fun setup/0,           % Start client, register for cleanup
     fun cleanup/1,         % Stop client, verify cleanup
     [
         fun test_start_link_stdio/1,
         fun test_start_link_tcp/1,
         fun test_start_link_with_options/1,
         fun test_stop_normal/1,
         fun test_stop_crash/1
     ]}.
```

### Phase 2: Initialization and Phase Transitions
```erlang
%% Test ALL phase transitions (pre_initialization → initializing → initialized)
initialization_test_() ->
    {foreach,
     fun setup_client/0,
     fun cleanup_client/1,
     [
         fun test_initialize_pre_initialization_phase/1,
         fun test_initialize_invalid_phase/1,
         fun test_initialized_notification_transitions_to_initialized/1,
         fun test_capabilities_extracted_from_init_response/1,
         fun test_initialize_error_transitions_to_error_phase/1
     ]}.
```

### Phase 3: Request-Response Correlation
```erlang
%% Test request ID generation and correlation
request_correlation_test_() ->
    {foreach,
     fun setup_initialized_client/0,
     fun cleanup_client/1,
     [
         fun test_list_resources_generates_unique_request_id/1,
         fun test_pending_requests_stores_request_info/1,
         fun test_response_removes_from_pending_requests/1,
         fun test_concurrent_requests_no_id_collision/1,
         fun test_request_id_overflow_protection/1  % P0 SECURITY
     ]}.
```

### Phase 4: Capability-Based Operations
```erlang
%% Test ALL capability-based operations (resources, tools, prompts)
capability_operations_test_() ->
    {foreach,
     fun setup_initialized_client/0,
     fun cleanup_client/1,
     [
         fun test_list_resources_with_capability/1,
         fun test_list_resources_without_capability_error/1,
         fun test_read_resource/1,
         fun test_subscribe_to_resource/1,
         fun test_unsubscribe_from_resource/1,
         fun test_list_tools/1,
         fun test_call_tool/1,
         fun test_list_prompts/1,
         fun test_get_prompt/1,
         fun test_get_prompt_with_arguments/1
     ]}.
```

### Phase 5: Notification Handling
```erlang
%% Test notification routing and handler delivery
notification_test_() ->
    {foreach,
     fun setup_initialized_client/0,
     fun cleanup_client/1,
     [
         fun test_set_notification_handler/1,
         fun test_remove_notification_handler/1,
         fun test_resources_updated_notification_routed_to_handler/1,
         fun test_resources_list_changed_notification_routed/1,
         fun test_sampling_create_message_notification/1
     ]}.
```

### Phase 6: Batch Request Management
```erlang
%% Test batch request lifecycle
batch_request_test_() ->
    {foreach,
     fun setup_initialized_client/0,
     fun cleanup_client/1,
     [
         fun test_start_batch_creates_batch_state/1,
         fun test_add_to_batch_increases_request_count/1,
         fun test_execute_batch_sends_all_requests/1,
         fun test_cancel_batch_removes_batch_state/1,
         fun test_with_batch_wrapper/1
     ]}.
```

### Phase 7: Error Handling and Edge Cases
```erlang
%% Test ALL error paths
error_handling_test_() ->
    {foreach,
     fun setup_client/0,
     fun cleanup_client/1,
     [
         fun test_operation_before_initialized_error/1,
         fun test_transport_exit_stops_server/1,
         fun test_invalid_json_response_logged/1,
         fun test_unknown_response_id_logged/1,
         fun test_strict_mode_enforces_capabilities/1,
         fun test_request_id_collision_error/1  % P0 SECURITY
     ]}.
```

### Phase 8: Concurrent Operations
```erlang
%% Test concurrent request handling (race conditions)
concurrent_test_() ->
    {foreach,
     fun setup_initialized_client/0,
     fun cleanup_client/1,
     [
         fun test_concurrent_requests_no_collision/1,
         fun test_concurrent_batch_operations/1,
         fun test_concurrent_notification_handlers/1
     ]}.
```

**Quality Validation:**
- **Automated**:
  ```bash
  # Run all tests
  rebar3 eunit --module=erlmcp_client_tests

  # Generate coverage report
  rebar3 cover --verbose

  # Verify ≥80% coverage
  rebar3 cover --verbose | grep erlmcp_client.erl
  ```

- **Manual**:
  - Verify ALL 19 handle_call patterns are tested (lines 217-412)
  - Verify ALL 2 handle_info patterns are tested (lines 418-437)
  - Verify request ID correlation works under load (100+ concurrent requests)
  - Verify no memory leaks (subscribe/unsubscribe balanced)

- **Metrics**:
  - Coverage: ≥80% (target: 90%)
  - Test count: ≥50 (target: 60)
  - Execution time: <5 seconds (target: <3 seconds)
  - Pass rate: 100% (zero tolerance for failures)

## Open Questions
NONE - all research complete

## Manufacturing Checklist
- [x] Root cause identified (not symptoms): Lack of Chicago School TDD enforcement
- [x] Quality gates defined (specific thresholds): ≥80% coverage, ≥50 tests, 100% pass rate
- [x] OTP patterns understood (behaviors, supervision): gen_server with 6 callbacks, phase-based initialization
- [x] Test strategy clear (Chicago School TDD): Real processes, state verification, no mocks, follow erlmcp_session_manager_tests.erl pattern
- [x] Risk assessment complete (severity P0-P3): 10 risks identified with mitigations
- [x] No open questions (all research complete): Test structure, patterns, and validation strategy defined
