# Research: Create comprehensive EUnit test suite for erlmcp_server.erl

**Date**: 2026-01-29
**Item**: 003-create-comprehensive-eunit-test-suite-for-erlmcpse
**Section**: coverage
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
Core server module has 0% test coverage - server is completely untested

**Motivation:** Server must be bulletproof - any server crash takes down ALL MCP connections. Critical for production reliability.

**Success criteria:**
- ≥80% code coverage achieved
- ≥60 tests created
- All resource management functions tested
- All tool management functions tested
- All prompt management functions tested
- JSON-RPC request routing tested
- Error handling paths tested
- All tests pass: rebar3 eunit --module=erlmcp_server_tests

**Technical constraints:**
- Handler registration testing required
- Request routing testing for JSON-RPC
- Argument validation testing
- Concurrent access testing
- Mock handlers for execution verification

**Signals:** priority: critical, urgency: P0 - BLOCKS ALL PRODUCTION WORK

### Quality Gate Status
- **Gate Type**: Coverage
- **Current State**: 0% code coverage (only 13 basic tests exist)
- **Target State**: ≥80% code coverage, ≥60 tests
- **Gap**: 80 percentage points, 47 additional tests minimum

## Summary

This research phase identifies the critical quality gap in erlmcp_server.erl test coverage. The server is a 1562-line gen_server that handles JSON-RPC messages, manages resources/tools/prompts, and coordinates with the registry for message routing. Current test coverage is approximately 5% (13 basic lifecycle tests only).

The manufacturing objective is to achieve ≥80% coverage by testing ALL code paths: every handle_call clause, every handle_request branch, every handle_cast clause, every handle_info branch, error handling paths, and helper functions. This requires ≥60 comprehensive tests following Chicago School TDD principles (real processes, no mocks where possible).

The technical approach uses EUnit test generators with setup/cleanup fixtures, Chicago School patterns (spawn real gen_server instances), comprehensive message routing tests (via registry integration), handler execution verification (using function references and meck for side effects), concurrent access testing (multiple clients), and error path testing (invalid requests, missing handlers, crashed handlers).

TCPS justification applies Jidoka (built-in quality - every defect stops the test suite), Poka-yoke (mistake-proofing via compile-time type specs and runtime validation), Kaizen (continuous improvement - coverage metrics drive quality), Heijunka (production leveling - small test batches, incremental coverage), and Andon (visible problem signaling - all failures reported with context).

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl:1-1562` - Main server gen_server (1562 lines)
  - `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_tests.erl:1-317` - Existing test file (317 lines, 13 tests)
  - `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server_sup.erl:1-47` - Supervisor (simple_one_for_one)
  - `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_registry.erl:1-400+` - Registry for message routing
  - `/Users/sac/erlmcp/include/erlmcp.hrl:1-1000+` - Protocol constants and type definitions

- **Patterns**:
  - **gen_server behavior**: Standard OTP gen_server with callbacks (init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3)
  - **Supervision**: simple_one_for_one strategy via erlmcp_server_sup
  - **State management**: #state{} record with server_id, phase, capabilities, resources, tools, prompts maps
  - **Message routing**: Registry-based routing via erlmcp_registry:route_to_server/3 and erlmcp_registry:route_to_transport/3
  - **OpenTelemetry tracing**: erlmcp_tracing:start_server_span/2, erlmcp_tracing:set_attributes/2, erlmcp_tracing:end_span/1

- **Tests**: Current coverage ~5% (13 basic tests in erlmcp_server_tests.erl)
  - `server_lifecycle_test_()`: 3 tests (start_link, stop, with_capabilities)
  - `resource_test_()`: 3 tests (add_resource, add_resource_template, resource_handler)
  - `tool_test_()`: 3 tests (add_tool, add_tool_with_schema, tool_handler)
  - `prompt_test_()`: 3 tests (add_prompt, add_prompt_with_args, prompt_handler)
  - `notification_test_()`: 2 tests (notify_resource_updated, notify_resources_changed)

- **Quality**: FAILING - Current test coverage is ~5%, far below 80% threshold. Critical code paths untested:
  - JSON-RPC request routing (handle_request/5 with 16 method clauses)
  - Resource/tool/prompt deletion operations
  - Subscription/unsubscription handling
  - Error handling paths (invalid URIs, missing handlers, validation failures)
  - Concurrent access scenarios
  - Handler execution with real data
  - Registry integration (message routing)
  - Initialization state machine (Gap #4: strict phase enforcement)

### Key Files
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl:77-163` - API functions (start_link/2, add_resource/3, add_tool/3, etc.)
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl:169-202` - init/1 callback with tracing
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl:207-344` - handle_call/3 with 14 clauses (add_resource, add_tool, add_prompt, delete_*, subscribe_*, etc.)
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl:348-367` - handle_cast/2 with 4 clauses (report_progress, notify_resource_updated, notify_resources_changed)
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl:372-421` - handle_info/2 with 5 clauses (mcp_message routing, task messages)
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl:449-716` - handle_request/5 with 16 method clauses (initialize, resources/list, tools/call, prompts/get, tasks/*, etc.)
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl:859-1084` - Resource/tool/prompt execution handlers
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_tests.erl:1-317` - Existing test patterns (setup/cleanup fixtures, test generators)
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_client_tests.erl:14-100` - Reference test patterns (lifecycle tests, initialization tests)

### OTP Patterns Observed
- **Behavior**: gen_server (standard OTP behavior)
- **Supervision**: simple_one_for_one (dynamic server instances, restart = temporary)
- **Process Pattern**: Process-per-connection (one server per transport connection)
- **Registry Pattern**: gproc-based registry for server/transport discovery and routing
- **Test Pattern**: EUnit test generators with setup/cleanup fixtures (Chicago School TDD - real processes)

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - `erlmcp_registry` - Message routing and transport/server discovery
  - `erlmcp_json_rpc` - JSON-RPC encode/decode (line 726-744 for response/error encoding)
  - `erlmcp_task_manager` - Task lifecycle management (line 603-608, 179)
  - `erlmcp_tracing` - OpenTelemetry span management (line 171-202, 373-406)
  - `erlmcp_uri_validator` - URI validation on registration (line 209, 228)
  - `erlmcp_capabilities` - Capability negotiation (line 461-471)
  - `erlmcp_change_notifier` - List change notifications (line 272-273, 284-285, 297-298)
  - `erlmcp_prompt_argument_validator` - Prompt argument validation (line 1082-1084)
  - `erlmcp_path_canonicalizer` - Resource path security (line 1556)
  - `erlmcp_progress` - Progress token generation (line 935, 942)
  - `erlmcp_audio` - Audio content validation (line 1274-1290)

- **External Libraries**:
  - `jsx` - JSON encode/decode (version 3.1.0 in rebar.config)
  - `gproc` - Process registry (version 0.9.0)
  - `meck` - Mocking library for tests (version 0.9.2, test profile only)
  - `proper` - Property-based testing (version 1.4.0, test profile only)

- **OTP Applications**: kernel, stdlib, sasl (for error logging)

### TCPS Quality Gates to Pass
- [ ] **Compilation**: 0 errors (rebar3 compile)
- [ ] **EUnit**: 100% pass rate (rebar3 eunit --module=erlmcp_server_tests)
- [ ] **Common Test**: N/A (unit tests only)
- [ ] **Coverage**: ≥80% (rebar3 cover --module=erlmcp_server)
- [ ] **Dialyzer**: 0 warnings (rebar3 dialyzer)
- [ ] **Xref**: 0 undefined function calls (rebar3 xref)
- [ ] **Performance**: N/A (not perf-critical code path)

### Patterns to Follow
- **Gen Server Pattern**: Standard gen_server callbacks with state management (lines 169-435)
- **Test Pattern**: EUnit test generators with setup/cleanup (erlmcp_server_tests.erl:14-56, erlmcp_client_tests.erl:14-100)
- **Error Handling**: Return `{error, Reason}` tuples, use safe wrapper functions (lines 766-812)
- **Type Specs**: Dialyzer specs for all exported functions (lines 75-163, 44-68 for state record)

## Root Cause Analysis (5 Whys)

**Problem**: Core server module has 0% test coverage, creating catastrophic risk of server crashes in production

1. **Why is coverage 0%?**
   Only 13 basic lifecycle tests exist; all critical code paths (JSON-RPC routing, handler execution, error handling) are untested.

2. **Why are critical paths untested?**
   Tests were written for basic API surface only (add_resource, add_tool, add_prompt) without message routing or execution verification.

3. **Why weren't message routing tests written?**
   Registry integration requires complex test setup (mocking erlmcp_registry, simulating transport messages), which wasn't prioritized.

4. **Why wasn't this prioritized?**
   Initial development focused on feature completion over test coverage; quality gate enforcement was deferred.

5. **ROOT CAUSE**:
   **Missing Poka-yoke (mistake-proofing) in development workflow** - No automated quality gate preventing commits with <80% coverage. TCPS requires "fail-closed" behavior: compilation/tests/coverage gates must BLOCK low-quality code from entering main branch.

**Solution**:
Implement comprehensive test suite covering ALL handle_call, handle_cast, handle_info, and handle_request clauses with ≥60 tests. Configure pre-commit hooks and CI/CD to enforce ≥80% coverage gate (fail-closed). This is Jidoka in action: quality is built-in, not inspected in.

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Incomplete coverage of error paths** | P0 (Critical) | Production crashes on edge cases | Test ALL error return paths in handle_call/handle_request (missing params, invalid URIs, not_found errors) |
| **Missing concurrent access tests** | P0 (Critical) | Race conditions in production | Test multiple simultaneous add_resource/add_tool calls, verify state consistency |
| **Handler execution not verified** | P0 (Critical) | Broken handlers silently fail | Use meck to mock handler side effects, verify handlers are called with correct arguments |
| **Registry integration not tested** | P1 (High) | Message routing failures in production | Mock erlmcp_registry route calls, verify correct JSON-RPC responses |
| **Tracing overhead interferes with tests** | P2 (Medium) | Flaky tests due to timing issues | Use meck to stub erlmcp_tracing functions, disable span recording in tests |
| **State machine edge cases untested** | P1 (High) | Initialization phase violations | Test double-initialize rejection, pre-initialization RPC rejection, phase transitions |
| **Resource template URI validation untested** | P2 (Medium) | Invalid template URIs crash server | Test URI template validation errors (malformed templates, missing placeholders) |
| **Task lifecycle not tested** | P1 (High) | Task execution failures cascade | Test task_create, task_get, task_cancel with invalid task IDs |
| **Delete operations not tested** | P2 (Medium) | Orphaned resources after delete | Test delete_resource/tool/prompt with valid/invalid IDs, verify removal from state |
| **Progress token generation conflicts** | P3 (Low) | Token collisions under load | Test concurrent tool calls generate unique progress tokens (use Proper for property testing) |

**Severity Definitions:**
- **P0 (Critical):** BLOCKS production - MUST fix before any release
- **P1 (High):** Major quality gap - MUST fix before release
- **P2 (Medium):** Important but not blocking
- **P3 (Low):** Nice-to-have

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Requirements with acceptance criteria (≥80% coverage, ≥60 tests, all code paths)
2. **Pseudocode** - Test structure design BEFORE coding (organize tests by functionality: lifecycle, resources, tools, prompts, requests, error handling, concurrency)
3. **Architecture** - Integration points identified (registry, json_rpc, tracing, validators)
4. **Refinement** - Chicago School TDD (write tests FIRST, run to fail, implement to pass)
5. **Completion** - All quality gates passing (compile, eunit, coverage, dialyzer, xref)

**Implementation Strategy:**

**Phase 1: Foundation (Heijunka - small batches)**
- Add setup/cleanup fixtures for server and registry initialization
- Test basic gen_server lifecycle (init, terminate, code_change)
- Test state initialization (capabilities, phase, empty maps)

**Phase 2: Resource Management (Kaizen - measure and improve)**
- Test add_resource with valid/invalid URIs (line 207-224)
- Test add_resource_template with URI templates (line 226-243)
- Test delete_resource (line 303-311)
- Test resource subscription/unsubscription (line 333-341)
- Test resource handler execution (line 859-916)
- Verify URI validation errors are returned correctly

**Phase 3: Tool Management (Poka-yoke - error-proofing)**
- Test add_tool with valid schemas (line 245-252)
- Test add_tool_with_schema (line 254-262)
- Test delete_tool (line 313-321)
- Test tool handler execution (line 918-987)
- Test progress token generation (line 935, 942)
- Test tool description validation (Gap #40, line 1487-1519)

**Phase 4: Prompt Management (Jidoka - stop on defect)**
- Test add_prompt with/without arguments (line 264-286)
- Test add_prompt_with_args_and_schema (line 288-299)
- Test delete_prompt (line 323-331)
- Test prompt handler execution (line 989-1070)
- Test prompt argument validation (line 1074-1084)

**Phase 5: JSON-RPC Request Routing (Andon - visible failures)**
- Test initialize request (line 449-492)
- Test double-initialize rejection (line 496-504)
- Test pre-initialization RPC rejection (line 508-517)
- Test resources/list (line 520-523)
- Test resources/read (line 525-551)
- Test tools/list (line 553-562)
- Test tools/call (line 564-594)
- Test prompts/list (line 693-702)
- Test prompts/get (line 704-712)
- Test task operations (line 596-665)
- Test invalid method errors (line 714-716)

**Phase 6: Error Handling (Poka-yoke - prevent errors)**
- Test missing parameter errors (missing URI, tool name, prompt name)
- Test not_found errors (resource/tool/prompt doesn't exist)
- Test validation errors (invalid URI format, malformed schemas)
- Test handler crashes (catch Class:Reason:Stacktrace, return internal error)
- Test safe wrapper functions (catch exceptions in send_*_safe, line 766-812)

**Phase 7: Concurrent Access (Heijunka - leveled production)**
- Test multiple concurrent add_resource calls
- Test multiple concurrent tool calls
- Test race conditions in subscribe/unsubscribe
- Verify state consistency under concurrent load

**Phase 8: Registry Integration (Andon - signal problems)**
- Mock erlmcp_registry:route_to_transport/3
- Verify JSON-RPC responses are routed correctly
- Test transport ID correlation
- Test broadcast notifications

**Quality Validation:**
- **Automated**:
  ```bash
  rebar3 compile                          # 0 errors
  rebar3 eunit --module=erlmcp_server_tests  # 100% pass
  rebar3 cover --module=erlmcp_server      # ≥80% coverage
  rebar3 dialyzer                          # 0 warnings
  rebar3 xref                              # 0 undefined
  ```
- **Manual**: Review coverage report to identify untested lines, ensure all handle_call/handle_cast/handle_info/handle_request clauses are covered
- **Metrics**:
  - Coverage percentage (target ≥80%)
  - Test count (target ≥60)
  - Test execution time (< 5 seconds for unit tests)
  - Memory usage (no leaks in test processes)

## Open Questions
**NONE** - All research complete. Test strategy is clear, OTP patterns understood, dependencies identified, risks assessed with severity (P0-P3).

## Manufacturing Checklist
- [x] Root cause identified (missing Poka-yoke quality gate, not just lack of tests)
- [x] Quality gates defined (≥80% coverage, ≥60 tests, 100% pass rate)
- [x] OTP patterns understood (gen_server, simple_one_for_one supervision, registry routing)
- [x] Test strategy clear (Chicago School TDD with real processes, meck for side effects)
- [x] Risk assessment complete (P0: incomplete error path coverage, P1: concurrent access)
- [x] No open questions (all research complete, ready for implementation)
