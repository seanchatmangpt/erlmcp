# Research: Create EUnit test suite for OTEL middleware (erlmcp_otel_middleware)

**Date**: 2025-01-21
**Item**: 012-create-eunit-test-suite-for-otel-middleware-erlmcp
**Section**: observability
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
OTEL middleware has 0% test coverage - automatic tracing is untested

**Motivation:** Automatic tracing middleware ensures all MCP calls are traced without manual instrumentation. Critical for production observability.

**Success criteria:**
- Test file created: apps/erlmcp_observability/test/erlmcp_otel_middleware_tests.erl
- Automatic tracing verified
- Coverage ≥80%
- All tests pass

**Technical constraints:**
- Automatic tracing - verify all operations create spans
- Context propagation - trace context through transport layers
- Performance testing - minimal overhead from middleware
- Error handling - middleware failures don't break transport

**Signals:** priority: high, urgency: P1 - BLOCKS PRODUCTION MONITORING

### Quality Gate Status
- **Gate Type**: Coverage, EUnit
- **Current State**: 0% test coverage for erlmcp_otel_middleware.erl (318 lines)
- **Target State**: ≥80% code coverage, 100% test pass rate
- **Gap**: 80 percentage points (0% → 80%)

## Summary

The OTEL middleware (`erlmcp_otel_middleware.erl`) is a critical observability component that provides automatic tracing for all transport layer operations in erlmcp. Currently, this module has **zero test coverage** despite being responsible for production monitoring infrastructure. The middleware automatically creates spans for transport operations (tcp, http, stdio, websocket), handler executions, and RPC calls with proper context propagation.

To achieve the required ≥80% coverage, we need to create comprehensive EUnit tests following the **Chicago School TDD** pattern used throughout erlmcp: real processes, no mocks, and explicit setup/cleanup. The test suite must verify all 8 exported functions: `trace_transport/4`, `trace_handler/3`, `trace_handler/4`, `annotate_request/2`, `annotate_response/2`, `record_transport_error/3`, `wrap_rpc_call/3`, and `wrap_rpc_response/3`. Tests must validate automatic span creation, event emission, attribute setting, error handling, and cross-process context propagation.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `apps/erlmcp_observability/src/erlmcp_otel_middleware.erl` (318 lines)
  - `apps/erlmcp_observability/src/erlmcp_otel.erl` (1005 lines - core OTEL functions)
  - `apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl` (Jaeger exporter)
  - `apps/erlmcp_observability/src/erlmcp_otel_datadog.erl` (Datadog exporter)
  - `apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl` (Honeycomb exporter)

- **Patterns**:
  - Pure functional middleware (no gen_server behavior)
  - Wraps user functions with OTEL span lifecycle
  - Delegates to `erlmcp_otel` for span operations
  - Uses process dictionary for context storage
  - Graceful degradation (returns `ok` when no active span)

- **Tests**:
  - `apps/erlmcp_observability/test/erlmcp_otel_tests.erl` (61 lines - basic OTEL tests)
  - `apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl` (444 lines - includes 2 middleware tests)
  - **No dedicated test file for `erlmcp_otel_middleware`**

- **Quality**:
  - Current coverage: **0%** for erlmcp_otel_middleware.erl
  - EUnit tests exist for core OTEL functions
  - Enhanced tests include `test_middleware_transport/0` and `test_middleware_handler/0` but these are minimal
  - No tests for `annotate_request/2`, `annotate_response/2`, `record_transport_error/3`
  - No tests for `wrap_rpc_call/3`, `wrap_rpc_response/3`
  - No performance/overhead testing

### Key Files
- `apps/erlmcp_observability/src/erlmcp_otel_middleware.erl:52-60` - Public API exports (8 functions)
- `apps/erlmcp_observability/src/erlmcp_otel_middleware.erl:85-123` - `trace_transport/4` implementation (39 lines)
- `apps/erlmcp_observability/src/erlmcp_otel_middleware.erl:132-183` - `trace_handler/4` implementation (52 lines)
- `apps/erlmcp_observability/src/erlmcp_otel_middleware.erl:187-202` - `annotate_request/2` implementation (16 lines)
- `apps/erlmcp_observability/src/erlmcp_otel_middleware.erl:206-217` - `annotate_response/2` implementation (12 lines)
- `apps/erlmcp_observability/src/erlmcp_otel_middleware.erl:221-230` - `record_transport_error/3` implementation (10 lines)
- `apps/erlmcp_observability/src/erlmcp_otel_middleware.erl:234-255` - `wrap_rpc_call/3` implementation (22 lines)
- `apps/erlmcp_observability/src/erlmcp_otel_middleware.erl:259-278` - `wrap_rpc_response/3` implementation (20 lines)
- `apps/erlmcp_observability/src/erlmcp_otel_middleware.erl:286-318` - Private helper functions (classify_method, classify_result, classify_response, is_success_response)
- `apps/erlmcp_observability/src/erlmcp_otel.erl:199-260` - Span lifecycle functions (start_span, end_span, with_span)
- `apps/erlmcp_observability/src/erlmcp_otel.erl:790-824` - `inject_rpc_span/3` implementation
- `apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl:238-262` - Existing middleware test patterns (minimal)
- `apps/erlmcp_core/test/erlmcp_registry_tests.erl:16-29` - EUnit test fixture pattern (foreach with setup/cleanup)
- `apps/erlmcp_core/test/erlmcp_client_tests.erl:1-150` - EUnit test structure example

### OTP Patterns Observed
- **Behavior**: None (pure functional module, no gen_server)
- **Supervision**: Not applicable (stateless middleware)
- **Process Pattern**: Uses process dictionary via `erlmcp_otel:get_current_context()`
- **Test Pattern**: Chicago School TDD - real processes, setup/cleanup fixtures, foreach isolation

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - `erlmcp_otel` (line 99, 143, 188, 207, 222, 237, 265) - Core OTEL functions
  - `erlmcp.hrl` (line 49) - Common records and macros

- **External Libraries**:
  - `opentelemetry_api` (1.5.0) - OTEL API (rebar.config:15)
  - `opentelemetry` (1.7.0) - OTEL SDK (rebar.config:16)
  - `opentelemetry_exporter` (1.10.0) - OTLP exporter (rebar.config:17)

- **OTP Applications**:
  - `kernel` - Process dictionary, logger
  - `stdlib` - Maps, lists, binary operations
  - `crypto` - Random ID generation (via erlmcp_otel)

### TCPS Quality Gates to Pass
- [ ] **Compilation**: 0 errors
- [ ] **EUnit**: 100% pass rate (all tests must pass)
- [ ] **Common Test**: Not required (unit tests only)
- [ ] **Coverage**: ≥80% for erlmcp_otel_middleware.erl
- [ ] **Dialyzer**: 0 new warnings (respect existing xref_ignores)
- [ ] **Xref**: 0 undefined function calls
- [ ] **Performance**: <10% regression from baseline (middleware overhead must be minimal)

### Patterns to Follow
- **Gen Server Pattern**: Not applicable (no gen_server)
- **Test Pattern**:
  - `apps/erlmcp_core/test/erlmcp_registry_tests.erl:16-29` - foreach with setup/cleanup fixtures
  - `apps/erlmcp_core/test/erlmcp_client_tests.erl:14-24` - setup/closure test structure
  - Use `?_test` macro for individual assertions
  - Use `?assert` and `?assertEqual` for assertions
  - Test both success and error paths
  - Test with and without active span context
- **Error Handling**:
  - Middleware functions return `ok` when no active span (graceful degradation)
  - `wrap_rpc_call/3` returns `{error, {wrap_failed, Reason}}` on exception
  - `wrap_rpc_response/3` catches exceptions and returns `ok`
- **Type Specs**:
  - `transport_type() = tcp | http | stdio | websocket`
  - `operation_name() = binary()`
  - `handler_fun() = fun(() -> term())`
  - `middleware_opts() = map()`

## Root Cause Analysis (5 Whys)

**Problem**: erlmcp_otel_middleware has 0% test coverage despite being critical for production observability

1. **Why?** No dedicated test file exists for erlmcp_otel_middleware module
2. **Why?** Tests were added to erlmcp_otel_enhanced_tests.erl instead (only 2 tests for middleware)
3. **Why?** Enhanced test suite focuses on erlmcp_otel core functions, not middleware specifics
4. **Why?** Middleware was added later as convenience wrapper around erlmcp_otel functions
5. **Why?** Root cause: Gap in test coverage planning - middleware treated as "trivial wrapper" despite having 8 exported functions with complex logic (classification, error handling, context propagation)

**Solution**: Create dedicated EUnit test suite `erlmcp_otel_middleware_tests.erl` with comprehensive coverage of all 8 exported functions, testing:
- All exported functions (trace_transport, trace_handler, annotate_request, annotate_response, record_transport_error, wrap_rpc_call, wrap_rpc_response)
- Span creation and lifecycle (via erlmcp_otel)
- Event emission (operation_started, operation_completed, request_received, processing_started, etc.)
- Attribute setting (transport.type, rpc.method, request.id, etc.)
- Error handling (exceptions, no active context)
- Helper functions (classify_method, classify_result, classify_response, is_success_response)
- Context propagation (cross-process trace_ctx)
- Performance (measure overhead of middleware wrapping)

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Test flakiness due to timing issues** | P1 | Unreliable CI/CD, false negatives | Use synchronous assertions, avoid timers, test deterministic behavior only |
| **Missing edge case coverage** | P1 | Production failures in rare scenarios | Test error paths (undefined context, exceptions, invalid input), test all classify_* functions |
| **Dependency on erlmcp_otel internals** | P2 | Tests break when erlmcp_otel changes | Treat erlmcp_otel as opaque dependency, mock only at process dictionary level |
| **Performance overhead not measured** | P2 | Unexpected production latency degradation | Add timing tests to measure <10μs overhead per call, verify <10% regression |
| **Incomplete context propagation testing** | P1 | Traces break across transport boundaries | Test trace_ctx serialization/deserialization, test baggage propagation |
| **Span context collision in concurrent tests** | P1 | Test interference, false failures | Use foreach for isolation, clear process dictionary in setup/cleanup |
| **No real OpenTelemetry library in test env** | P2 | Tests don't validate actual span export | Test behavior with mock erlmcp_otel, integration tests cover real exporter |

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Define test requirements with acceptance criteria (≥80% coverage, all 8 functions tested)
2. **Pseudocode** - Design test structure before implementation (setup fixtures, test cases, assertions)
3. **Architecture** - Identify integration points (erlmcp_otel, process dictionary, span context)
4. **Refinement** - Chicago School TDD (write tests FIRST, then verify they fail, then implementation if needed)
5. **Completion** - All quality gates passing (compile, eunit, coverage ≥80%, dialyzer, xref)

**Implementation Strategy:**

1. **Phase 1: Test Infrastructure (Setup)**
   - Create `erlmcp_otel_middleware_tests.erl`
   - Define setup/cleanup fixtures for OTEL initialization
   - Clear process dictionary between tests (foreach isolation)
   - Add helper functions for span context validation

2. **Phase 2: Core Function Tests (trace_transport, trace_handler)**
   - Test span creation with various transport types (tcp, http, stdio, websocket)
   - Test span events (operation_started, operation_completed, request_received, etc.)
   - Test span attributes (transport.type, operation, rpc.method, request_id)
   - Test error handling (exceptions in wrapped functions)
   - Test behavior without active span context

3. **Phase 3: Annotation Tests (annotate_request, annotate_response)**
   - Test request annotation (method, params, param_count)
   - Test response annotation (type, success flag)
   - Test behavior without active span context
   - Test with various request/response formats

4. **Phase 4: Error Recording Tests (record_transport_error)**
   - Test error recording with stacktrace
   - Test error attributes (error.type, error.message, error.stacktrace)
   - Test behavior without active span context
   - Test with various transport types

5. **Phase 5: RPC Wrapper Tests (wrap_rpc_call, wrap_rpc_response)**
   - Test RPC span injection (client side)
   - Test trace context propagation (headers)
   - Test baggage propagation (request_id, method)
   - Test response wrapping and span completion
   - Test error handling (wrap_failed, exceptions)

6. **Phase 6: Helper Function Tests (classify_*, is_success_response)**
   - Test classify_method with all method types (tools/, resources/, prompts/, initialize, notifications/)
   - Test classify_result with various result types (success, error, content, list, map, unknown)
   - Test classify_response with various response formats (result, error, notification)
   - Test is_success_response with success and error responses

7. **Phase 7: Integration & Performance**
   - Test end-to-end flow (wrap_rpc_call → trace_handler → wrap_rpc_response)
   - Test cross-process trace propagation (trace_ctx serialization)
   - Test middleware overhead (measure timing, verify <10μs per call)
   - Test concurrent execution (multiple processes with shared context)

**Quality Validation:**
- **Automated**:
  ```bash
  rebar3 compile --app erlmcp_observability
  rebar3 eunit --app erlmcp_observability
  rebar3 cover --app erlmcp_observability
  rebar3 dialyzer --app erlmcp_observability
  rebar3 xref --app erlmcp_observability
  ```
- **Manual**:
  - Review coverage report to verify ≥80% for erlmcp_otel_middleware.erl
  - Inspect span exports in Jaeger/OTLP collector (if available)
  - Verify all test pass with verbose output
- **Metrics**:
  - Coverage percentage (target: ≥80%)
  - Test pass rate (target: 100%)
  - Number of test cases (target: 20-30 tests for 8 functions)
  - Middleware overhead (target: <10μs per call, <10% regression)

## Open Questions
**NONE** - Research complete, all questions answered:
- ✅ Module structure and exports identified (8 functions)
- ✅ Dependencies documented (erlmcp_otel, opentelemetry_api)
- ✅ Test patterns understood (Chicago School TDD, foreach fixtures)
- ✅ Coverage target specified (≥80%)
- ✅ Quality gates defined (compile, eunit, cover, dialyzer, xref)
- ✅ Risk assessment complete (7 risks with mitigations)
- ✅ Implementation strategy defined (7-phase approach)

## Manufacturing Checklist
- [x] Root cause identified (gap in test coverage planning, middleware treated as trivial wrapper)
- [x] Quality gates defined (≥80% coverage, 100% pass rate, <10% regression)
- [x] OTP patterns understood (no gen_server, stateless middleware, process dictionary context)
- [x] Test strategy clear (Chicago School TDD, foreach fixtures, real processes)
- [x] Risk assessment complete (7 risks: P1=3, P2=4)
- [x] No open questions (all research complete)
