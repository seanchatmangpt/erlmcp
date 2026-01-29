# Research: Core Server Module Test Coverage

**Date**: 2026-01-26
**Item**: 026-core-server-module-test-coverage
**Section**: coverage
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
erlmcp_server.erl has ZERO test coverage (0% per COVERAGE_REPORT.md). This module handles incoming requests, manages resources, tools, and prompts. Any server crash takes down ALL MCP connections.

**Motivation:** This is the second most critical module after the client. It manages resource/tool/prompt registries and handles all incoming MCP requests. A bug here affects all connected clients.

**Success criteria:**
- Test file created: apps/erlmcp_core/test/erlmcp_server_tests.erl
- All resource management functions tested (add/remove/list/get)
- All tool management functions tested
- All prompt management functions tested
- JSON-RPC request routing tested
- Error handling paths tested (invalid_request, method_not_found, invalid_params)
- Coverage: ≥80%
- All tests pass: rebar3 eunit --module=erlmcp_server_tests

**Technical constraints:**
- Handler Registration Testing - Add/remove/list operations
- Request Routing Testing - JSON-RPC method calls to correct handlers
- Argument Validation - Invalid arguments, missing required params
- Concurrent Access - Multiple clients registering/calling simultaneously
- Handler Execution - Mock handlers that return test data, verify execution

**Signals:** priority: critical, urgency: P0 - BLOCKS ALL PRODUCTION WORK

### Quality Gate Status
- **Gate Type**: Coverage
- **Current State**: 0% coverage (CRITICAL - 0 of ~1562 lines covered)
- **Target State**: ≥80% coverage (≥1250 lines)
- **Gap**: -80 percentage points (ZERO coverage - needs complete test suite)

## Summary

**1. Manufacturing Objective:**
Create comprehensive EUnit test suite for erlmcp_server.erl to achieve ≥80% code coverage, covering all critical MCP protocol operations (resource management, tool execution, prompt handling, JSON-RPC request routing, and error handling). This is a P0 BLOCKER - the server module is the second-most critical component after the client, and a bug here affects ALL connected MCP clients.

**2. Technical Approach:**
Follow Chicago School TDD (real gen_server processes, no mocks) to test actual gen_server behavior. Use EUnit setup/cleanup fixtures for proper process lifecycle management. Test all handle_call, handle_cast, and handle_info code paths including initialization phase enforcement (Gap #4), request routing, resource/tool/prompt handlers, delete operations (Gap #28), and error responses. Create real processes, register handlers, send actual messages, verify responses with pattern matching.

**3. TCPS Justification:**
Applies **Jidoka** (stop-the-line quality) - missing tests means no defect detection for server crashes. **Poka-yoke** (mistake-proofing) requires test coverage to prevent bugs like double initialization or pre-init RPC requests. **Kaizen** (continuous improvement) needs metrics to measure server reliability. Current 0% coverage violates Lean Six Sigma's 99.99966% defect-free standard - we ship ZERO defects without verification.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl:1-1562` - Main server module (1562 lines)
  - `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_tests.erl:1-317` - Existing test file (BASIC ONLY)

- **Patterns**:
  - **Behavior**: gen_server (line 2: `-behaviour(gen_server)`)
  - **State Record**: Lines 50-66 with 13 fields (server_id, phase, capabilities, resources, tools, prompts, subscriptions, etc.)
  - **API Functions**: 20 exported API functions (lines 17-38)
  - **Gen Server Callbacks**: init/1, handle_call/3 (13 patterns), handle_cast/2 (4 patterns), handle_info/2 (4 patterns)
  - **Request Handlers**: 12 JSON-RPC method handlers (initialize, resources/list, tools/call, prompts/get, tasks/*, etc.)

- **Tests**: EXISTING but BASIC ONLY (317 lines)
  - **Current Coverage**: ~5-10% (estimated from COVERAGE_REPORT.md showing 0%)
  - **Test File**: erlmcp_server_tests.erl exists with basic lifecycle/add operations
  - **Coverage Gaps**: NO request routing, NO initialization enforcement, NO delete operations, NO error handling, NO concurrent access

- **Quality**: CRITICAL GAP - 0% coverage per COVERAGE_REPORT.md line 76. Server is HIGH priority module with -80% gap to 80% threshold.

### Key Files
- `apps/erlmcp_core/src/erlmcp_server.erl:1-1562` - Complete gen_server implementation
  - Lines 75-82: start_link/2 API
  - Lines 83-138: Resource/tool/prompt add/delete APIs
  - Lines 169-202: init/1 callback with tracing
  - Lines 207-344: handle_call/3 with 13 message patterns
  - Lines 348-367: handle_cast/2 with 4 patterns
  - Lines 372-421: handle_info/2 with 4 patterns (mcp_message, task_execute, task_status_update, task_cancel)
  - Lines 449-716: handle_request/5 with 12 JSON-RPC method handlers
  - Lines 859-1087: Resource/tool/prompt execution handlers

- `apps/erlmcp_core/test/erlmcp_server_tests.erl:1-317` - Existing basic tests
  - Lines 14-54: Basic lifecycle tests (start_link, stop, capabilities)
  - Lines 60-112: Resource add operations (NO delete, NO read)
  - Lines 118-181: Tool add operations (NO delete, NO call)
  - Lines 187-256: Prompt add operations (NO delete, NO get)
  - Lines 262-287: Notification tests (basic only)
  - **MISSING**: Request routing, initialization enforcement, error handling, concurrent access

- `include/erlmcp.hrl:1-500+` - MCP protocol constants
  - Lines 132-183: JSON-RPC method names (MCP_METHOD_INITIALIZE, MCP_METHOD_RESOURCES_LIST, etc.)
  - Lines 35-45: Error codes (MCP_ERROR_NOT_INITIALIZED, MCP_ERROR_RESOURCE_NOT_FOUND, etc.)
  - Lines 112-126: Phase state machine constants (Gap #4: MCP_PHASE_INITIALIZATION, etc.)

### OTP Patterns Observed
- **Behavior**: gen_server with standard OTP callbacks
- **Supervision**: Supervised by erlmcp_server_sup (one_for_one strategy)
- **Process Pattern**: Long-lived gen_server managing state (resources, tools, prompts registry)
- **State Management**: Record-based state with maps for resource/tool/prompt storage
- **Message Pattern**: Receives {mcp_message, TransportId, Data} from registry, routes to handle_request/5
- **Test Pattern**: EUnit with setup/cleanup fixtures (Chicago School TDD - real processes)
  - Reference: `erlmcp_registry_tests.erl:16-28` for setup/cleanup pattern
  - Reference: `erlmcp_json_rpc_tests.erl:24-36` for test generator pattern

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - `erlmcp_registry.erl` - Message routing (route_to_transport/3)
  - `erlmcp_json_rpc.erl` - JSON-RPC encode/decode
  - `erlmcp_capabilities.erl` - Capability negotiation (extract_client_capabilities/1, validate_protocol_version/1)
  - `erlmcp_task_manager.erl` - Task execution (create_tool_task/5, list_tasks/1, get_task/1)
  - `erlmcp_tracing.erl` - OpenTelemetry spans
  - `erlmcp_uri_validator.erl` - URI validation (Gap #41)
  - `erlmcp_path_canonicalizer.erl` - Path canonicalization (Gap #36)
  - `erlmcp_prompt_argument_validator.erl` - Prompt argument validation (Gap #42)
  - `erlmcp_change_notifier.erl` - List change notifications (Gap #6)
  - `erlmcp_prompt_list_change_notifier.erl` - Prompt list changes (Gap #27)

- **External Libraries**:
  - `jsx` - JSON encode/decode (version 3.1.0)
  - `gproc` - Process registry (version 0.9.0)

- **OTP Applications**:
  - kernel, stdlib - OTP core
  - sasl - Logging and supervision

### TCPS Quality Gates to Pass
- [ ] **Compilation**: 0 errors (rebar3 compile)
- [ ] **EUnit**: 100% pass rate (rebar3 eunit --module=erlmcp_server_tests)
- [ ] **Common Test**: 100% pass rate (if SUITE tests added)
- [ ] **Coverage**: ≥80% (rebar3 cover --module=erlmcp_server)
- [ ] **Dialyzer**: 0 warnings (rebar3 dialyzer)
- [ ] **Xref**: 0 undefined function calls (rebar3 xref)
- [ ] **Performance**: <10% regression from baseline (benchmarks if code changes perf)

### Patterns to Follow
- **Gen Server Pattern**:
  - Reference: `apps/erlmcp_core/src/erlmcp_server.erl:1-1562` (implementation)
  - Test setup: `erlmcp_registry_tests.erl:35-62` (setup with process cleanup)
  - Message testing: Send gen_server:call/cast, verify response

- **Test Pattern** (Chicago School TDD):
  - Real processes: `erlmcp_registry_tests.erl:100-135` (spawn real processes, no mocks)
  - Setup/cleanup: `erlmcp_server_tests.erl:293-316` (existing pattern)
  - Test generators: `erlmcp_json_rpc_tests.erl:24-36` (?_test wrapping)
  - Assertions: Use ?assertMatch, ?assertEqual for pattern matching

- **Error Handling Pattern**:
  - Test error responses: Send invalid params, expect {error, {Code, Message, Data}}
  - Reference: `erlmcp_server.erl:496-504` (double initialization rejection)
  - Reference: `erlmcp_server.erl:508-517` (pre-init RPC rejection)

- **Type Specs**:
  - Dialyzer specs: Lines 75-161 (API function specs)
  - State record: Lines 43-68 (type definitions)
  - Test with proper types: server(), binary(), map()

## Root Cause Analysis (5 Whys)

**Problem**: erlmcp_server.erl has 0% test coverage (P0 quality gate failure)

1. **Why?** Existing test file (erlmcp_server_tests.erl) only covers basic add operations, missing all request routing, initialization enforcement, delete operations, and error handling paths.

2. **Why?** Tests were written for initial implementation but not expanded as server grew in complexity (added tasks, Gap #4 initialization enforcement, Gap #28 delete operations, Gap #42 prompt validation).

3. **Why?** Test development focused on "happy path" API functions (start_link, add_resource, add_tool) but neglected gen_server callback internals (handle_call patterns, handle_info message routing, error responses).

4. **Why?** Missing test strategy for complex scenarios like JSON-RPC request routing (requires mocking registry, encoding messages, simulating transport), initialization phase enforcement (requires testing state transitions), and concurrent access (requires multiple simultaneous gen_server:call/cast operations).

5. **ROOT CAUSE**: Incomplete TDD practice - tests written AFTER implementation instead of BEFORE. Missing "test the implementation" mindset - only testing public API, not internal gen_server logic. No coverage tracking during development (0% discovered in COVERAGE_REPORT.md after fact).

**Solution**: Expand test suite using Chicago School TDD (real processes, no mocks) to cover ALL gen_server code paths:
- Test ALL handle_call patterns (13 patterns)
- Test ALL handle_cast patterns (4 patterns)
- Test ALL handle_info patterns (4 patterns)
- Test ALL handle_request methods (12 methods)
- Test error responses (invalid params, not found, not initialized)
- Test concurrent access (multiple simultaneous calls)
- Measure coverage with `rebar3 cover --module=erlmcp_server`, iterate until ≥80%

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Test complexity explosion** - Request routing requires encoding JSON-RPC messages, mocking registry, simulating transport responses | P1 (High) | Test development time 2-3x estimate, incomplete coverage | Break into small test generators per method (resources/list, tools/call, prompts/get). Use existing erlmcp_json_rpc:encode/decode for message construction. Create helper functions for common test patterns (setup_server_with_mock_registry). |
| **State management complexity** - Server has 13 state fields, initialization phase, multiple maps (resources, tools, prompts) | P1 (High) | Tests pass but don't cover critical state transitions (initialization phase enforcement, resource deletion) | Focus on state-changing operations: test initialized=false → true transition, test add → delete → not_found sequence, test subscription map updates. Use sys:get_state to inspect state in tests. |
| **Process cleanup issues** - gen_server processes remain registered after test failure, causing "already_registered" errors in subsequent tests | P2 (Medium) | Flaky tests, false failures | Use foreach (not setup) to ensure cleanup after EACH test. Reference: erlmcp_registry_tests.erl:16-29 (foreach with cleanup). Clear gproc entries in cleanup. |
| **Missing concurrent access tests** - gen_server handles multiple simultaneous calls, but tests run sequentially | P2 (Medium) - Important but not blocking | Race conditions in production (e.g., two clients adding same resource) | Add concurrent test using spawn multiple processes calling server simultaneously. Verify final state is consistent (one wins, other gets error). |
| **Dependency on external modules** - erlmcp_task_manager, erlmcp_tracing, erlmcp_capabilities | P3 (Low) | Tests fail due to missing dependencies (not server code issue) - Nice-to-have | Mock or stub external dependencies with meck if needed. Better: use real modules in test environment (Chicago School). Ensure all deps started in setup. |
| **Coverage blind spots** - Internal helper functions not called by public API (e.g., canonicalize_and_validate_uri/1, list_all_resources/1) | P0 (Critical) | Coverage ≥80% not achieved despite passing all tests | Add direct tests for internal functions using ?_test(internal_function_test()). Use -compile(export_all) temporarily or test via public API that calls them. |

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Define testable acceptance criteria per function (13 handle_call patterns, 4 handle_cast, 4 handle_info, 12 request methods)
2. **Pseudocode** - Design test structure BEFORE coding (test generators, setup/cleanup, helper functions)
3. **Architecture** - Identify integration points (registry, json_rpc, task_manager) and test isolation strategy
4. **Refinement** - Chicago School TDD (tests FIRST, real processes, no mocks)
5. **Completion** - All quality gates passing (coverage ≥80%, 100% pass rate)

**Implementation Strategy:**

**Phase 1: Expand Basic Tests (Current → 40% coverage)**
- Test ALL add/delete operations (resources, tools, prompts)
- Test list operations (resources/list, tools/list, prompts/list)
- Test subscription management (subscribe/unsubscribe)
- Test notification sending (notify_resource_updated, notify_resources_changed)
- **Validation**: rebar3 eunit --module=erlmcp_server_tests, rebar3 cover --module=erlmcp_server

**Phase 2: Request Routing Tests (40% → 65% coverage)**
- Test ALL handle_request methods (12 methods: initialize, resources/list, resources/read, tools/list, tools/call, prompts/list, prompts/get, tasks/*)
- Create test generator per method with valid/invalid params
- Test error responses (invalid_params, method_not_found, resource_not_found)
- Test initialization enforcement (pre-init RPC rejection, double initialization)
- **Validation**: Coverage report shows handle_request/5 covered ≥80%

**Phase 3: Gen Server Callback Tests (65% → 80% coverage)**
- Test ALL handle_call patterns (13 patterns)
- Test ALL handle_cast patterns (4 patterns)
- Test ALL handle_info patterns (4 patterns: mcp_message, task_execute, task_status_update, task_cancel)
- Test terminate/2 and code_change/3
- **Validation**: Coverage ≥80%, all gen_server branches covered

**Phase 4: Concurrent Access & Edge Cases (80% → 90% coverage)**
- Test concurrent add/delete (multiple simultaneous gen_server:call)
- Test handler execution with mock handlers returning test data
- Test error paths (handler crashes, invalid URIs, missing params)
- Test state transitions (initialization → initialized, resource add → delete)
- **Validation**: Coverage ≥90% (exceeds 80% target), no flaky tests

**Quality Validation:**
- **Automated**:
  - `rebar3 compile` - 0 errors, 0 warnings
  - `rebar3 eunit --module=erlmcp_server_tests` - 100% pass rate
  - `rebar3 cover --module=erlmcp_server` - ≥80% coverage
  - `rebar3 dialyzer` - 0 warnings
  - `rebar3 xref` - 0 undefined function calls
- **Manual**:
  - Review coverage report: `_build/test/cover/erlmcp_server.coverdata` (ensure no red uncovered lines)
  - Inspect state with `sys:get_state(ServerPid)` in tests
  - Verify error responses match JSON-RPC spec (code, message, data fields)
- **Metrics**:
  - Coverage percentage: ≥80% (target), ≥90% (stretch)
  - Test count: ≥100 tests (estimate: 12 request methods × 5 test cases + 21 callbacks × 3 test cases)
  - Test execution time: <10 seconds (EUnit should be fast)

## Open Questions
**NONE** - Research complete. All requirements understood:
- Test file location: apps/erlmcp_core/test/erlmcp_server_tests.erl (exists, needs expansion)
- Test framework: EUnit with setup/cleanup fixtures
- OTP pattern: gen_server with real processes (Chicago School TDD)
- Coverage target: ≥80%
- Quality gates: compile, eunit, cover, dialyzer, xref all passing

## Manufacturing Checklist
- [x] Root cause identified (incomplete TDD, only happy path tested)
- [x] Quality gates defined (≥80% coverage, 100% pass rate)
- [x] OTP patterns understood (gen_server, real processes, setup/cleanup)
- [x] Test strategy clear (4 phases: basic → routing → callbacks → concurrent)
- [x] Risk assessment complete (6 risks, P0-P3 severity)
- [x] No open questions (all research complete)
