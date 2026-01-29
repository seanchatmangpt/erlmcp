# Research: Create EUnit test suite for OpenTelemetry core (erlmcp_otel)

**Date**: 2026-01-29
**Item**: 011-create-eunit-test-suite-for-opentelemetry-core-erl
**Section**: observability
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
OpenTelemetry core has 0% test coverage - observability is untested

**Motivation:** OpenTelemetry integration for distributed tracing is critical for production observability and debugging.

**Success criteria:**
- Test file created: apps/erlmcp_observability/test/erlmcp_otel_tests.erl
- W3C trace context compliance verified
- Coverage ≥80%
- All tests pass

**Technical constraints:**
- W3C trace context - traceparent header format
- Span lifecycle - start/end span hierarchy
- Attribute testing - key-value pairs
- Context propagation - in-process and cross-process

**Signals:** priority: high, urgency: P1 - BLOCKS PRODUCTION MONITORING

### Quality Gate Status
- **Gate Type**: Coverage / Test Quality / W3C Compliance
- **Current State**: Basic tests exist (erlmcp_otel_tests.erl: 61 lines, erlmcp_otel_enhanced_tests.erl: 444 lines) but coverage gaps remain
- **Target State**: ≥80% code coverage, 100% W3C compliance
- **Gap**: Missing comprehensive edge case coverage, W3C validation incomplete, some exported functions untested

## Summary

This research task requires creating a comprehensive EUnit test suite for the OpenTelemetry core module (`erlmcp_otel`). The module provides distributed tracing infrastructure for ErlMCP with 1,005 lines of code implementing span management, W3C trace context propagation, error recording, baggage handling, and sampling strategies.

The **manufacturing objective** is to achieve 80% test coverage with 100% W3C Trace Context specification compliance, ensuring production observability is fully validated before deployment.

The **technical approach** follows Chicago School TDD principles: real processes, no mocks, comprehensive edge case coverage, and W3C standard validation. Tests must verify span lifecycle, parent-child relationships, context propagation across processes, error recording, baggage propagation, sampling strategies, and W3C traceparent header format compliance (version-trace_id-span_id-flags).

The **TCPS justification** applies Jidoka (stop-the-line quality checks), Poka-yoke (error-proofing through type specs and guards), and Andon (visible test failures) principles. OpenTelemetry is critical infrastructure - untested observability means flying blind in production, violating Lean Six Sigma zero-defect principles.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `apps/erlmcp_observability/src/erlmcp_otel.erl:1-1005` - Core OpenTelemetry module (1,005 lines)
  - `apps/erlmcp_observability/src/erlmcp_otel_middleware.erl:1-318` - Middleware for automatic tracing
  - `apps/erlmcp_observability/test/erlmcp_otel_tests.erl:1-61` - Basic EUnit tests (61 lines)
  - `apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl:1-444` - Enhanced test suite (444 lines)

- **Patterns**: Functional module with process dictionary for context storage, map-based span context, W3C traceparent format compliance

- **Tests**:
  - Basic file: 6 test functions covering init, start_span, with_span, lifecycle, nested spans, error handling
  - Enhanced file: 15 test functions in setup/cleanup pattern covering trace context, baggage, RPC injection, span linking, sampling, middleware, exporters, e2e flows, multi-process traces

- **Quality**:
  - Basic tests pass but provide minimal coverage
  - Enhanced tests provide better coverage but still miss edge cases
  - No explicit W3C traceparent format validation tests
  - Missing tests for some exported functions

### Key Files
- `apps/erlmcp_observability/src/erlmcp_otel.erl:58-104` - Public API exports (4 export sections: 13 + 9 + 4 + 5 = 31 exported functions)
- `apps/erlmcp_observability/src/erlmcp_otel.erl:163-196` - init/1 implementation with comprehensive error handling
- `apps/erlmcp_observability/src/erlmcp_otel.erl:199-259` - start_span/3 with parent context detection and span creation
- `apps/erlmcp_observability/src/erlmcp_otel.erl:262-285` - end_span/1 with duration calculation and status handling
- `apps/erlmcp_observability/src/erlmcp_otel.erl:288-305` - with_span/4 with automatic error recording
- `apps/erlmcp_observability/src/erlmcp_otel.erl:388-405` - propagate_context/1 for W3C traceparent format
- `apps/erlmcp_observability/src/erlmcp_otel.erl:408-435` - restore_context/1 parsing traceparent headers
- `apps/erlmcp_observability/src/erlmcp_otel.erl:620-630` - ID generation (make_trace_id/0, make_span_id/0)
- `apps/erlmcp_observability/src/erlmcp_otel.erl:664-703` - W3C traceparent format/parse functions
- `apps/erlmcp_observability/test/erlmcp_otel_tests.erl:1-61` - Basic test patterns to follow
- `apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl:32-68` - Setup/cleanup pattern reference
- `docs/opentelemetry-architecture.md:53-56` - W3C Trace Context specification reference

### OTP Patterns Observed
- **Behavior**: Functional module (no gen_server behavior) - uses process dictionary (`erlang:get/put`) for context storage
- **Supervision**: Not a process - stateless functional API with process-local context
- **Process Pattern**: Context propagation via process dictionary and explicit context passing
- **Test Pattern**: Chicago School TDD - setup/cleanup fixtures, real processes (spawn in multi-process tests), no mocks, actual OpenTelemetry library integration

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - `erlmcp_otel_middleware` - Automatic transport/handler tracing
  - `erlmcp_otel_jaeger` - Jaeger exporter
  - `erlmcp_otel_datadog` - Datadog exporter
  - `erlmcp_otel_honeycomb` - Honeycomb exporter

- **External Libraries**:
  - `opentelemetry_api:1.5.0` - OpenTelemetry API
  - `opentelemetry:1.7.0` - OpenTelemetry SDK
  - `opentelemetry_exporter:1.10.0` - Exporter implementations

- **OTP Applications**: kernel, stdlib, crypto (for ID generation)

### TCPS Quality Gates to Pass
- [ ] **Compilation**: 0 errors (rebar3 compile)
- [ ] **EUnit**: 100% pass rate (rebar3 eunit --module=erlmcp_otel_tests)
- [ ] **Common Test**: 100% pass rate (if CT suite exists)
- [ ] **Coverage**: ≥80% (rebar3 cover)
- [ ] **Dialyzer**: 0 warnings (rebar3 dialyzer)
- [ ] **Xref**: 0 undefined function calls (rebar3 xref)
- [ ] **Performance**: <10% regression from baseline (if performance-critical)

### Patterns to Follow
- **Gen Server Pattern**: N/A (functional module, not a gen_server)
- **Test Pattern**:
  - Basic: `erlmcp_otel_tests.erl:8-37` - Simple test_() functions
  - Enhanced: `erlmcp_otel_enhanced_tests.erl:32-68` - setup/cleanup with otel_test_() generator
- **Error Handling**: `erlmcp_otel.erl:191-196` - try/catch with error_logger error messages
- **Type Specs**: `erlmcp_otel.erl:110-157` - Comprehensive -type() definitions for otel_config(), span_context(), otel_event(), otel_error()

## Root Cause Analysis (5 Whys)

**Problem**: OpenTelemetry core has insufficient test coverage for production deployment

1. **Why?** Existing tests (erlmcp_otel_tests.erl, erlmcp_otel_enhanced_tests.erl) don't cover all exported functions and edge cases
2. **Why?** Tests were written incrementally during development without comprehensive coverage planning
3. **Why?** No formal test requirements specification or W3C compliance checklist during initial implementation
4. **Why?** Focus was on implementing OpenTelemetry features first, testing second (violates Chicago School TDD)
5. **ROOT CAUSE**: Missing TCPS quality gate enforcement during initial development - Jidoka (stop-the-line testing) not applied, allowing untested code into codebase

**Solution**: Apply TCPS manufacturing discipline - write comprehensive test suite following Chicago School TDD (tests FIRST), validate W3C compliance with explicit tests, enforce 80% coverage quality gate before considering work complete. Use Poka-yoke principles to make test failures visible (Andon) and impossible to ignore.

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **W3C traceparent format violations** | P0 (Critical) | Distributed tracing breaks in production, cross-service correlation fails | Implement explicit W3C validation tests verifying version (00), trace_id (32 hex chars), span_id (16 hex chars), flags (2 hex chars) format |
| **Missing coverage on error paths** | P1 (High) | Errors not recorded in traces, debugging impossible | Test all error recording scenarios (throw/error/exit classes), stacktrace formatting, status propagation |
| **Context propagation failures** | P1 (High) | Traces break across processes/nodes, parent-child relationships lost | Multi-process tests with spawn, verify trace_id/span_id inheritance, baggage propagation |
| **Sampling strategy bugs** | P1 (High) | Wrong spans sampled, performance impact or missing critical traces | Test all sampling strategies (always_on/off, trace_id_ratio, parent_based, tail_sample), verify determinism |
| **Race conditions in context storage** | P2 (Medium) | Corrupted span data in concurrent scenarios | Concurrent test with multiple processes accessing context, verify process dictionary isolation |
| **Memory leaks from unclosed spans** | P2 (Medium) | Memory exhaustion in long-running systems | Test span lifecycle, verify context cleanup, check for orphaned process dictionary entries |
| **Invalid ID generation** | P2 (Medium) | Trace ID collisions, trace corruption | Verify make_trace_id/0 returns 128-bit random, make_span_id/0 returns 64-bit random, check uniqueness |
| **Baggage propagation failures** | P2 (Medium) | Request correlation data lost across boundaries | Test baggage set/get, propagation to child spans, cross-process baggage transfer |

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Define test requirements with acceptance criteria
2. **Pseudocode** - Design test cases BEFORE implementation
3. **Architecture** - Identify test fixtures, setup/teardown patterns
4. **Refinement** - Write tests FIRST (Chicago School TDD)
5. **Completion** - All quality gates passing (80% coverage, W3C compliance)

**Implementation Strategy:**

### Phase 1: Core API Testing (Foundation)
- Test suite initialization (init/1 with valid/invalid configs)
- Span lifecycle (start_span/2,3 → end_span/1)
- Context management (get_current_context/0, set_current_context/1)
- ID generation (make_trace_id/0, make_span_id/0) with uniqueness validation

### Phase 2: W3C Trace Context Compliance (Critical)
- **Traceparent format validation**:
  - Valid format: `00-{32-char-hex-trace_id}-{16-char-hex-span_id}-{2-char-hex-flags}`
  - Invalid format detection (wrong version, invalid lengths, non-hex characters)
  - format_traceparent/3 output verification
  - parse_traceparent/1 error handling
- **Context propagation**:
  - propagate_context/1 generates correct headers
  - restore_context/1 reconstructs span context
  - Round-trip preservation (propagate → restore → verify equality)
- **Cross-process propagation**:
  - Spawn child process with trace context
  - Verify trace_id continuity
  - Verify baggage propagation

### Phase 3: Span Hierarchy and Attributes (Functional)
- **Parent-child relationships**:
  - Child span inherits parent trace_id
  - parent_span_id correctly set
  - Nested spans (3+ levels)
  - Sibling spans (same parent, different span_ids)
- **Attribute management**:
  - add_attributes/2 merges correctly
  - System attributes added (service.name, service.version, mcp.version, etc.)
  - Custom attributes preserved
  - Attribute type validation (binary keys)
- **Event tracking**:
  - add_event/2,3 creates events with timestamps
  - Events list grows correctly
  - Event attributes preserved

### Phase 4: Error Recording and Status (Quality)
- **Error recording**:
  - record_error/2,3 captures all error classes (throw/error/exit)
  - Error attributes set (error=true, error.type, error.message, error.stacktrace)
  - Error events added to span
  - Status updated to error
- **Stacktrace formatting**:
  - format_error_message/1 handles various term types
  - format_stacktrace/1 produces readable output
  - Large stacktraces handled (no truncation bugs)

### Phase 5: Baggage and Correlation (Integration)
- **Baggage API**:
  - set_baggage/2 stores in current context
  - get_baggage/1 retrieves values
  - get_all_baggage/0 returns complete map
- **Propagation**:
  - Baggage inherits to child spans
  - propagate_baggage/2 converts types (atom → binary)
  - Cross-process baggage transfer verified

### Phase 6: Sampling Strategies (Performance)
- **Head-based sampling**:
  - always_on returns true (100% sampling)
  - always_off returns false (0% sampling)
  - trace_id_ratio deterministic (same trace_id = same decision)
  - parent_based respects parent trace_flags
- **Tail-based sampling**:
  - High latency spans sampled
  - Error spans sampled
  - Configurable thresholds (latency, error_rate)

### Phase 7: RPC Integration (Middleware)
- **RPC span injection**:
  - inject_rpc_span/3,4 creates client spans
  - RPC attributes set (rpc.method, rpc.request_id, rpc.system, rpc.service)
  - Parameter sanitization (sensitive keys redacted)
  - Request sent event added
- **Span linking**:
  - link_span/2 adds link attributes
  - Span linked event added
  - Multiple links supported

### Phase 8: Edge Cases and Error Handling (Robustness)
- **Invalid inputs**:
  - start_span with invalid names/attributes
  - end_span with invalid context
  - add_attributes with non-map attributes
  - Empty/malformed traceparent headers
- **Boundary conditions**:
  - Empty attribute maps
  - Very long attribute values (stress test)
  - Deep nesting (10+ levels)
  - Concurrent access (race condition test)

### Phase 9: Performance and Resource Management (Production)
- **Memory**:
  - Context cleanup (no process dictionary leaks)
  - Large event lists (100+ events)
  - Many baggage items (50+ keys)
- **Performance**:
  - Span creation overhead (< 100μs)
  - Context propagation overhead
  - Sampling decision performance

**Quality Validation:**

```bash
# Automated validation
cd /Users/sac/erlmcp
rebar3 compile
rebar3 eunit --module=erlmcp_otel_tests --verbose
rebar3 cover -v apps/erlmcp_observability --cover_export_coverage=html
rebar3 dialyzer -r apps/erlmcp_observability
rebar3 xref

# Manual verification
# Check coverage report: _build/test/cover/index.html
# Verify ≥80% line coverage for erlmcp_otel.erl
# Run multi-process tests to verify context propagation
# Validate W3C compliance by inspecting traceparent format

# Metrics to track
# - Test pass rate: 100%
# - Coverage percentage: ≥80%
# - W3C traceparent format tests: 100% pass
# - Test execution time: < 5 seconds
# - Memory leaks: 0 bytes leaked
```

**Manual Verification Checklist:**
- [ ] All tests pass (100% success rate)
- [ ] Coverage ≥80% (verified in cover report)
- [ ] W3C traceparent format validated
- [ ] Multi-process context propagation works
- [ ] No memory leaks in process dictionary
- [ ] Error paths fully tested
- [ ] Sampling strategies verified
- [ ] Edge cases covered

**Metrics to Collect:**
- Test execution time (baseline)
- Coverage percentage per function
- Number of test cases (target: 50-100 tests)
- W3C compliance test count (target: 10+ tests)
- Edge case coverage percentage

## Open Questions
**NONE** - All aspects of the OpenTelemetry testing requirements have been thoroughly researched and documented. The manufacturing approach is clear, test patterns are established, and quality gates are defined.

## Manufacturing Checklist
- [x] Root cause identified (missing TCPS quality gate enforcement during development)
- [x] Quality gates defined (≥80% coverage, 100% W3C compliance, 0 errors)
- [x] OTP patterns understood (functional module, process dictionary context, no gen_server)
- [x] Test strategy clear (Chicago School TDD, setup/cleanup fixtures, real processes)
- [x] Risk assessment complete (8 risks identified, P0-P2 severity)
- [x] No open questions (research complete)
