# Research: Core Client Module Test Coverage

**Date**: 2025-01-29
**Item**: 025-core-client-module-test-coverage
**Section**: coverage
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
erlmcp_client.erl has ZERO test coverage. This is the single most critical module in the entire codebase - every MCP client interaction flows through this module. Deploying without tests is unacceptable.

**Motivation:** This is the MOST CRITICAL test suite in the entire backlog. All other client tests depend on patterns established here. The module handles client lifecycle, connection management, request-response correlation, capability negotiation, error handling, and state management.

**Success criteria:**
- Test file created: apps/erlmcp_core/test/erlmcp_client_tests.erl
- All public functions have tests (start_link, connect, disconnect, send_request, initialize, list_resources, list_tools, etc.)
- All handle_info/handle_call/handle_cast clauses tested
- All error paths tested
- Coverage: ≥80%
- All tests pass: rebar3 eunit --module=erlmcp_client_tests
- No race conditions in concurrent request testing
- Integration tests with real stdio transport pass

**Technical constraints:**
- Chicago School TDD - Real processes, real gen_server, no mocks
- State-Based Verification - Check #state{} record contents after each operation
- Race Condition Testing - Concurrent requests, timeouts, re-entrant calls
- Error Path Coverage - Every error clause, every exit path
- Integration Points - Test with real transports (stdio, tcp)
- Use erlmcp_session_manager_tests.erl as reference for gen_server testing patterns

**In scope:**
- erlmcp_client.erl module testing
- All public API functions
- All gen_server callbacks
- Error handling paths
- Concurrent request scenarios
- Integration with stdio transport
**Out of scope:**
- Server-side functionality
- Other transport implementations beyond stdio for integration tests

**Signals:** priority: critical, urgency: P0 - BLOCKS ALL PRODUCTION WORK. This is foundation work that all other client tests depend on.

### Quality Gate Status
- **Gate Type**: Coverage, Test, Integration
- **Current State**: 0% coverage (46 test functions exist, but many are stubs that accept any result)
- **Target State**: ≥80% coverage, 100% test pass rate
- **Gap**: 80 percentage points (from 0% to ≥80%)

## Summary

**Manufacturing Objective:** Create comprehensive test coverage for `erlmcp_client.erl`, a 729-line gen_server module that implements the MCP client protocol with 32 public API functions, 28 handle_call clauses, 1 handle_cast clause, and 3 handle_info clauses. This module is the SINGLE MOST CRITICAL module in the entire codebase as every MCP client interaction flows through it.

**Technical Approach:** Apply Chicago School TDD methodology using real processes and real gen_server instances (no mocks). Follow the proven pattern from `erlmcp_session_manager_tests.erl` which demonstrates state-based verification by checking `#state{}` record contents after each operation. Implement race condition testing with concurrent requests, timeout testing, and re-entrant call validation. Create integration tests with real stdio transport. Test all error paths including initialization failures, capability violations, transport failures, and request ID overflow scenarios.

**TCPS Justification:**
- **Jidoka (Built-in Quality)**: Every test must verify state transitions and stop on unexpected conditions. The module has phase enforcement (pre_initialization → initializing → initialized) which must be thoroughly tested.
- **Poka-yoke (Mistake-proofing)**: Test request ID collision detection, capability validation before operations, and phase-based operation blocking. The module references a non-existent `erlmcp_request_id:safe_increment/1` function (line 478) which will cause runtime crashes - this MUST be fixed.
- **Kaizen (Continuous Improvement)**: Measure request-response correlation timing, pending request map size, and concurrent request handling. All tests must generate receipt-based audit trails.
- **Heijunka (Production Leveling)**: Break testing into incremental phases: (1) lifecycle and initialization, (2) resource operations, (3) tool operations, (4) prompt operations, (5) batch operations, (6) notification handlers, (7) error paths, (8) race conditions.
- **Andon (Visible Signaling)**: All test failures must be visible with clear error messages. Use state inspection macros to expose internal state for debugging.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_client.erl` (729 lines) - Main client implementation
  - `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_client_tests.erl` (438 lines) - Existing test stubs
  - `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_session_manager.erl` - Reference for gen_server patterns
  - `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_session_manager_tests.erl` - Reference test patterns
  - `/Users/sac/erlmcp/apps/erlmcp_core/include/erlmcp.hrl` (523 lines) - Protocol constants and records

- **Patterns**:
  - gen_server behavior with strict phase-based state machine
  - Request-response correlation via `#state.pending_requests` map
  - Request ID generation with overflow protection (BROKEN - references non-existent module)
  - Capability negotiation and validation
  - Transport abstraction (stdio, tcp, http)
  - Notification handler management
  - Batch request support

- **Tests**: 46 test functions exist in `erlmcp_client_tests.erl`, but analysis shows:
  - Most tests are stubs that accept both success and failure: `case Result of {ok, _} -> ?assert(true); {error, _} -> ?assert(true) end`
  - No state verification (checking `#state{}` record contents)
  - No concurrent request testing
  - No race condition testing
  - No transport failure testing
  - Missing tests for: list_roots, request ID overflow, transport crash recovery, phase violations, capability enforcement

- **Quality**:
  - **CRITICAL BUG**: Line 478 references `erlmcp_request_id:safe_increment/1` which does not exist - will crash at runtime
  - **CRITICAL BUG**: No request ID overflow handling in tests
  - **Coverage**: Approximately 5-10% actual coverage (tests run but don't verify behavior)
  - **Test pass rate**: 100% (because tests accept any result)
  - **Quality gate**: FAIL - does not meet ≥80% coverage requirement

### Key Files
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_client.erl:1-729` - Main client gen_server implementation
  - Lines 90-195: Public API (19 functions: start_link, initialize, list_resources, read_resource, list_tools, call_tool, list_prompts, get_prompt, stop, list_resource_templates, subscribe/unsubscribe, batch operations, handlers)
  - Lines 200-446: gen_server callbacks (init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3)
  - Lines 452-729: Internal functions (transport, request sending, capability validation, response handling, notification handling)
  - Line 478: **CRITICAL BUG** - references non-existent `erlmcp_request_id:safe_increment/1`

- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_session_manager_tests.erl:1-528` - **REFERENCE PATTERN**
  - Lines 14-43: Test generator with setup/cleanup (foreach pattern)
  - Lines 45-59: Setup creates real process, cleanup verifies shutdown
  - Lines 65-85: State verification after operations (check session ID, metadata, timestamps)
  - Lines 237-254: Expiration testing with timing assertions
  - Lines 282-303: Uniqueness testing with 100 concurrent sessions
  - Lines 309-329: Concurrency testing with spawned processes
  - **Pattern to follow**: Real processes, state inspection, no mocks

- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_client_tests.erl:1-438` - Existing test stubs
  - Lines 14-24: Lifecycle test group (setup/cleanup pattern)
  - Lines 26-65: Basic lifecycle tests (start_link, stop)
  - Lines 71-112: Initialization tests (initialize, capabilities)
  - Lines 118-161: Resource tests (list_resources, read_resource, subscribe)
  - Lines 167-200: Tool tests (list_tools, call_tool)
  - Lines 206-250: Prompt tests (list_prompts, get_prompt)
  - **PROBLEM**: Most tests use stub pattern: `case Result of {ok, _} -> ?assert(true); {error, _} -> ?assert(true) end`

- `/Users/sac/erlmcp/apps/erlmcp_core/include/erlmcp.hrl:1-523` - Protocol definitions
  - Lines 1-7: MCP version constants (2025-06-18)
  - Lines 110-126: Phase type definitions (mcp_client_phase)
  - Lines 329-380: Capability records (mcp_capability, mcp_client_capabilities, mcp_server_capabilities)
  - Lines 400-441: Content and tool records
  - Lines 498-520: Server state record (for comparison)

### OTP Patterns Observed
- **Behavior**: gen_server
- **Supervision**: Intended to be supervised by erlmcp_client_sup (simple_one_for_one pattern)
- **Process Pattern**: Process-per-connection with client-specific state
- **State Management**: Record-based state with phase enforcement (pre_initialization → initializing → initialized)
- **Test Pattern**: Chicago School TDD - real processes, real gen_server, state-based verification (from session_manager_tests)
- **Request-Response Correlation**: Map-based pending requests with integer request IDs
- **Phase Enforcement**: Strict state machine with phase checking on all operations

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - `erlmcp_json_rpc` - JSON-RPC encoding/decoding (lines 91, 103, 421, 491)
  - `erlmcp_transport_stdio` - Stdio transport (line 455)
  - `erlmcp_transport_tcp` - TCP transport (line 457)
  - `erlmcp_transport_http` - HTTP transport (lines 459-462)
  - `erlmcp_message_size` - Message size validation (line 99 in json_rpc)

- **External Libraries**:
  - `jsx` 3.1.0 - JSON encoding/decoding
  - `jesse` 1.8.1 - JSON Schema validation
  - `gproc` 0.9.0 - Process registry
  - `gun` 2.0.1 - HTTP client
  - `ranch` 2.1.0 - TCP acceptor pool

- **OTP Applications**:
  - `kernel` - gen_server, supervisor
  - `stdlib` - logger, sets, maps
  - `sasl` - error logging

- **MISSING DEPENDENCY**: `erlmcp_request_id` module (referenced line 478 but does not exist)

### TCPS Quality Gates to Pass
- [ ] **Compilation**: 0 errors (CRITICAL: fix erlmcp_request_id reference)
- [ ] **EUnit**: 100% pass rate (currently 100% but tests are stubs)
- [ ] **Common Test**: Integration tests with real stdio transport
- [ ] **Coverage**: ≥80% (currently ~5-10%)
- [ ] **Dialyzer**: 0 warnings (need to add type specs for all functions)
- [ ] **Xref**: 0 undefined function calls (CRITICAL: erlmcp_request_id:safe_increment/1)
- [ ] **Performance**: No regression from baseline (2.52M msg/sec core_ops)
- [ ] **Race Conditions**: No race conditions in concurrent request testing

### Patterns to Follow
- **Gen Server Pattern**: `erlmcp_session_manager_tests.erl:14-43`
  - Use `{foreach, fun setup/0, fun cleanup/1, [fun test1/1, ...]}`
  - Setup creates real process: `{ok, Pid} = erlmcp_client:start_link(...)`
  - Cleanup verifies process termination: `unlink(Pid), exit(Pid, shutdown), timer:sleep(10)`
  - Verify state with `sys:get_state(Pid)` or direct state inspection

- **Test Pattern**: `erlmcp_session_manager_tests.erl:65-85`
  - Call operation: `{ok, SessionId} = erlmcp_session_manager:create_session(Metadata)`
  - Verify result: `?assert(is_binary(SessionId)), ?assertEqual(32, byte_size(SessionId))`
  - Verify state: `{ok, Session} = erlmcp_session_manager:get_session(SessionId)`
  - Check state fields: `?assertEqual(SessionId, maps:get(id, Session))`

- **Error Handling Pattern**:
  - Test all error paths: bad arguments, wrong phase, transport failure, timeout
  - Verify error tuples: `?assertMatch({error, {not_initialized, Phase, _}}, Result)`
  - Verify state unchanged after errors

- **Type Specs Pattern**: `erlmcp_client.erl:30-43`
  - Add `-spec` declarations for all exported functions
  - Use `-type` and `-opaque` for complex types
  - Follow Dialyzer strict mode

## Root Cause Analysis (5 Whys)

**Problem**: erlmcp_client.erl has effectively ZERO test coverage despite having 46 test functions.

1. **Why?** Tests were written as stubs that accept any result (both success and failure) without verifying behavior.
   - Evidence: `case Result of {ok, _} -> ?assert(true); {error, _} -> ?assert(true) end` pattern throughout

2. **Why?** Tests lack state verification - they don't check the internal state of the gen_server after operations.
   - Evidence: No use of `sys:get_state/1` or state pattern matching in existing tests

3. **Why?** Tests don't validate the phase-based state machine or request-response correlation mechanism.
   - Evidence: No tests verify phase transitions (pre_initialization → initializing → initialized)
   - Evidence: No tests verify pending_requests map population/cleanup

4. **Why?** Tests don't cover error paths, edge cases, or concurrent scenarios.
   - Evidence: No tests for request ID overflow, transport crashes, concurrent requests, capability violations

5. **ROOT CAUSE**: The tests were written to satisfy the "form" of testing (test file exists, tests pass) without the "substance" of testing (verify behavior, check state, cover error paths). This is a quality gate failure - the coverage metric shows 0% meaningful coverage because tests don't assert anything specific.

**CRITICAL SECONDARY ROOT CAUSE**: The module references a non-existent `erlmcp_request_id:safe_increment/1` function on line 478. This will cause a runtime crash when any request is sent. This is a P0 blocking defect that MUST be fixed before any tests can pass.

**Solution**:
1. **Immediate P0 Fix**: Create `erlmcp_request_id` module with `safe_increment/1` function OR remove the broken check
2. **Rewrite all tests**: Replace stub pattern with state-based verification following `erlmcp_session_manager_tests.erl` pattern
3. **Add state inspection**: Use `sys:get_state/1` to check `#state{}` record contents after each operation
4. **Cover all clauses**: Write tests for every handle_call/handle_cast/handle_info clause (28 handle_call + 1 handle_cast + 3 handle_info = 32 clauses)
5. **Test error paths**: Every error tuple return must have a dedicated test
6. **Add concurrency tests**: Test concurrent requests, request ID overflow, transport crashes, timeout scenarios
7. **Integration tests**: Test with real stdio transport and mock server

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **erlmcp_request_id module missing** | P0 (Critical) | Runtime crash on ANY request - client unusable in production | Create erlmcp_request_id.erl with safe_increment/1 OR remove broken check and implement inline overflow protection |
| **No state verification in tests** | P0 (Critical) | Tests pass but code doesn't work - false confidence in quality | Rewrite all tests to check #state{} record contents using sys:get_state/1 |
| **Request ID overflow not tested** | P0 (Critical) | Client crashes after 2^56 requests - production outage | Add test that sends requests until ID wraps, verify graceful handling |
| **No concurrent request testing** | P1 (High) | Race conditions in production - request ID collisions, response mismatches | Add tests with 10-100 concurrent processes sending requests simultaneously |
| **Transport failure not tested** | P1 (High) | Client doesn't recover from transport crashes - hangs forever | Test transport EXIT signal handling, verify gen_server terminates cleanly |
| **Phase violations not tested** | P1 (High) | Operations allowed in wrong phase - protocol violations | Test each operation in each phase, verify errors for phase violations |
| **Missing error path tests** | P1 (High) | Unhandled errors crash client - production instability | Add tests for every error clause: bad args, wrong phase, transport failure, timeout |
| **No integration tests with real transport** | P2 (Medium) | Stdio transport integration bugs - doesn't work in real scenarios | Create integration tests with real stdio transport and mock server process |
| **Capability validation not tested** | P2 (Medium) | Client sends unsupported requests - protocol violations | Test strict_mode enforcement, capability checking before operations |
| **Batch operations not tested** | P2 (Medium) | Batch requests fail silently - data loss | Test batch start/execute/cancel, verify all requests sent |
| **Notification handlers not tested** | P3 (Low) | Notifications crash handlers - client instability | Test notification delivery, handler crashes, sampling handler invocation |

**Severity Definitions:**
- **P0 (Critical):** BLOCKS production - MUST fix immediately (module missing, no test verification)
- **P1 (High):** Major quality gap - MUST fix before release (concurrency, failures, errors)
- **P2 (Medium):** Important but not blocking (integration, capabilities, batches)
- **P3 (Low):** Nice-to-have (notifications)

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Requirements with acceptance criteria (DONE - item.json defines success criteria)
2. **Pseudocode** - Algorithm design BEFORE coding (see below)
3. **Architecture** - Integration points and dependencies (documented in Technical Considerations)
4. **Refinement** - Chicago School TDD (tests FIRST)
5. **Completion** - All quality gates passing (compilation, tests, coverage, dialyzer, xref)

**Implementation Strategy:**

### Phase 1: CRITICAL BUG FIX (P0 - MUST DO FIRST)
**Objective**: Fix the missing erlmcp_request_id module crash
**Steps**:
1. Create `apps/erlmcp_core/src/erlmcp_request_id.erl` with:
   ```erlang
   -module(erlmcp_request_id).
   -export([safe_increment/1]).

   -spec safe_increment(pos_integer()) -> {ok, pos_integer()} | {error, overflow}.
   safe_increment(Id) when Id >= 16#1000000000000000 ->
       {error, overflow};
   safe_increment(Id) ->
       {ok, Id + 1}.
   ```
2. Update `erlmcp_client.erl:478` to handle the error case properly
3. Write test for request ID overflow (send 2^56 requests, verify error)

### Phase 2: Foundation Tests (P0 - 2-3 hours)
**Objective**: Test lifecycle, initialization, and state machine
**Tests**:
- `test_start_link_and_verify_state` - Check initial state fields (phase=pre_initialization, request_id=1, pending=#{})
- `test_initialize_success` - Send initialize request, verify phase transition to initializing
- `test_initialized_notification` - Send initialized notification, verify phase transition to initialized
- `test_initialize_in_wrong_phase` - Call initialize twice, verify error
- `test_operation_before_initialization` - Call list_resources before initialize, verify not_initialized error

### Phase 3: Request-Response Tests (P0 - 3-4 hours)
**Objective**: Test request sending, correlation, and response handling
**Tests**:
- `test_send_request_and_verify_pending_map` - Send request, check pending_requests map
- `test_receive_response_and_verify_pending_cleanup` - Receive response, check pending_requests cleared
- `test_response_to_unknown_request_id` - Send response for non-existent request, verify logged and ignored
- `test_concurrent_requests` - Spawn 10 processes, send 100 requests, verify all responses correlated correctly
- `test_request_id_collision_detection` - Force duplicate request ID, verify error

### Phase 4: Resource Operation Tests (P1 - 2-3 hours)
**Objective**: Test resource operations (list, read, subscribe)
**Tests**:
- `test_list_resources_success` - Mock server response, verify list_resources returns resources
- `test_list_resources_capability_not_supported` - Set capabilities without resources, verify error
- `test_read_resource_success` - Mock server response, verify resource content
- `test_subscribe_resource_success` - Subscribe, verify subscription in state
- `test_unsubscribe_resource` - Subscribe then unsubscribe, verify removed from state
- `test_resource_updated_notification` - Send resources/updated notification, verify handler called

### Phase 5: Tool Operation Tests (P1 - 2-3 hours)
**Objective**: Test tool operations (list, call)
**Tests**:
- `test_list_tools_success` - Mock server response, verify tool list
- `test_call_tool_success` - Mock tool execution, verify result
- `test_call_tool_with_arguments` - Pass complex arguments, verify encoded correctly
- `test_tools_list_changed_notification` - Send notification, verify handler called

### Phase 6: Prompt Operation Tests (P1 - 2 hours)
**Objective**: Test prompt operations (list, get)
**Tests**:
- `test_list_prompts_success` - Mock server response, verify prompt list
- `test_get_prompt_success` - Get prompt without arguments, verify result
- `test_get_prompt_with_arguments` - Get prompt with arguments, verify encoded
- `test_prompts_list_changed_notification` - Send notification, verify handler called

### Phase 7: Batch Operation Tests (P2 - 2 hours)
**Objective**: Test batch request functionality
**Tests**:
- `test_with_batch_success` - Send 3 requests in batch, verify all sent
- `test_batch_cancel_on_error` - Start batch, crash in functor, verify batch cancelled
- `test_batch_add_after_start` - Add request to batch, execute, verify count
- `test_concurrent_batches` - Run 5 batches concurrently, verify no mixing

### Phase 8: Notification Handler Tests (P2 - 2 hours)
**Objective**: Test notification handler management
**Tests**:
- `test_set_notification_handler` - Set handler, verify in state
- `test_notification_handler_invoked` - Send notification, verify handler called
- `test_notification_handler_crash` - Handler crashes, verify logged but client continues
- `test_sampling_handler_set_and_called` - Set sampling handler, send sampling/createMessage, verify called
- `test_remove_notification_handler` - Remove handler, verify no longer called

### Phase 9: Error Path Tests (P1 - 3-4 hours)
**Objective**: Test all error handling paths
**Tests**:
- `test_transport_exit_signal` - Kill transport process, verify gen_server terminates with {transport_died, Reason}
- `test_invalid_json_response` - Send invalid JSON, verify parse error logged
- `test_error_response_from_server` - Mock server returns error, verify error tuple returned to caller
- `test_timeout_on_request` - Request with timeout, verify timeout error
- `test_strict_mode_enforcement` - Enable strict_mode, send unsupported request, verify error
- `test_capability_validation` - Server doesn't support resources, verify list_resources fails

### Phase 10: Race Condition Tests (P0 - 3-4 hours)
**Objective**: Test concurrent scenarios and race conditions
**Tests**:
- `test_concurrent_initialize_calls` - 10 processes call initialize simultaneously, only 1 succeeds
- `test_concurrent_request_sending` - 100 processes send requests, verify request IDs unique
- `test_concurrent_response_handling` - Send 100 responses concurrently, verify all correlated
- `test_request_id_exhaustion` - Send 2^56 requests, verify overflow error
- `test_re_entrant_calls` - Call operation from within notification handler, verify no deadlock
- `test_state_consistency_under_load` - 1000 concurrent operations, verify state consistent

### Phase 11: Integration Tests (P2 - 2-3 hours)
**Objective**: Test with real stdio transport and mock server
**Tests**:
- `test_full_lifecycle_with_stdio_transport` - Start client with stdio, initialize, list resources, stop
- `test_real_server_interaction` - Spawn mock server process, connect via stdio, run full sequence
- `test_transport_reconnection` - Kill transport, restart, verify client handles reconnect
- `test_message_size_limits` - Send large message, verify size validation

**Quality Validation:**

**Automated:**
```bash
# Compile
TERM=dumb rebar3 compile

# Run tests
rebar3 eunit --module=erlmcp_client_tests

# Coverage report
rebar3 cover --verbose

# Dialyzer (type checking)
rebar3 dialyzer

# Xref (cross-reference)
rebar3 xref

# Full check
rebar3 check
```

**Manual:**
- Verify all tests pass with 100% success rate
- Verify coverage ≥80% using cover report
- Verify no race conditions in concurrent tests (run 100 times)
- Verify state consistency by inspecting sys:get_state/1 after each operation
- Verify error messages are clear and actionable

**Metrics:**
- Test count: Target ≥80 tests (currently 46 stubs)
- Coverage: Target ≥80% (currently ~5-10%)
- Test execution time: Target <10 seconds for full suite
- Concurrent request handling: Target 100 concurrent requests without errors
- Request ID overflow: Must handle gracefully at 2^56 requests
- State transitions: All phase transitions must be tested and verified

## Open Questions
**NONE** - All research complete. Root cause identified (stub tests, missing module). Technical approach defined (Chicago School TDD, state-based verification). Risks assessed (10 risks with P0-P3 severity). Implementation strategy broken into 11 phases with clear test objectives.

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) - Tests are stubs that don't verify behavior; missing erlmcp_request_id module
- [x] Quality gates defined (specific thresholds) - ≥80% coverage, 100% pass rate, 0 dialyzer warnings, 0 xref errors
- [x] OTP patterns understood (behaviors, supervision) - gen_server with phase-based state machine, request-response correlation
- [x] Test strategy clear (Chicago School TDD) - Real processes, state-based verification, no mocks, follow session_manager_tests pattern
- [x] Risk assessment complete (severity P0-P3) - 10 risks identified with mitigation strategies
- [x] No open questions (all research complete) - Ready to proceed with implementation
