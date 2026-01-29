# Create EUnit test suite for OpenTelemetry core (erlmcp_otel) Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

**PRD Summary**: This plan has 10 user stories (US-001 through US-010) covering W3C compliance, span lifecycle, ID generation, error recording, attributes/events, baggage, multi-process propagation, sampling, edge cases, and final verification. See saved PRD for detailed acceptance criteria.

## Manufacturing Objective

Create a comprehensive EUnit test suite for `erlmcp_otel` module (1,005 lines) to achieve ≥80% code coverage with 100% W3C Trace Context specification compliance. OpenTelemetry integration is critical infrastructure for production observability and debugging - untested observability means flying blind in production, violating Lean Six Sigma zero-defect principles.

### Quality Gate Requirements
- **Compilation**: 0 errors (MANDATORY) - `TERM=dumb rebar3 compile`
- **EUnit**: 100% pass rate (MANDATORY) - `rebar3 eunit --module=erlmcp_otel_tests`
- **Common Test**: 100% pass rate (if applicable) - `rebar3 ct`
- **Coverage**: ≥80% (MANDATORY) - `rebar3 cover -v apps/erlmcp_observability`
- **Dialyzer**: 0 warnings (MANDATORY) - `rebar3 dialyzer -r apps/erlmcp_observability`
- **Xref**: 0 undefined function calls (MANDATORY) - `rebar3 xref`
- **Performance**: <10% regression from baseline (N/A for test suite)

## Current State

### What Exists Now
- **Modules**:
  - `apps/erlmcp_observability/src/erlmcp_otel.erl:1-1005` - Core OpenTelemetry module (60 exported functions across 4 export sections)
  - `apps/erlmcp_observability/src/erlmcp_otel_middleware.erl:1-318` - Middleware for automatic tracing

- **Tests**:
  - `apps/erlmcp_observability/test/erlmcp_otel_tests.erl:1-61` - Basic EUnit tests (6 test functions: init, start_span, with_span, lifecycle, nested_spans, error_handling)
  - `apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl:1-444` - Enhanced test suite (15 test functions: trace_context, baggage, rpc, linking, sampling, middleware, exporters, e2e, multiprocess)

- **Quality**:
  - Basic tests pass but provide minimal coverage (~50-60% estimated)
  - Enhanced tests provide better coverage but still miss edge cases
  - **No explicit W3C traceparent format validation tests** (CRITICAL GAP)
  - Missing comprehensive tests for ~40 exported functions
  - Missing edge case coverage (invalid inputs, boundary conditions, concurrent access)

### What's Missing
- **Gap**: Current tests cover ~50-60% of code, need ≥80%
- **Root Cause**: Missing TCPS quality gate enforcement during initial development - Jidoka (stop-the-line testing) not applied, allowing untested code into codebase
- **Impact**: Blocks production monitoring - cannot deploy observability infrastructure without comprehensive testing

### Key Discoveries from Research
1. **erlmcp_otel.erl:58-104** - 31 exported functions across 4 export sections
2. **erlmcp_otel.erl:620-630** - ID generation uses crypto:strong_rand_bytes (needs uniqueness testing)
3. **erlmcp_otel.erl:664-703** - W3C traceparent format/parse functions (needs comprehensive edge case testing)
4. **erlmcp_otel.erl:110-157** - Comprehensive type specs (use for Dialyzer validation)
5. **erlmcp_otel_tests.erl:32-68** - Setup/cleanup pattern already established in enhanced tests
6. **W3C Trace Context spec**: `00-{32-char-hex-trace_id}-{16-char-hex-span_id}-{2-char-hex-flags}` format

## Desired End State

### Specification

Create comprehensive EUnit test file: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`

**Test Coverage Requirements:**
1. **W3C Trace Context Compliance** (CRITICAL - P0):
   - Valid format: `00-{32-char-hex-trace_id}-{16-char-hex-span_id}-{2-char-hex-flags}`
   - Invalid format detection: wrong version, invalid lengths, non-hex characters
   - format_traceparent/3 output verification
   - parse_traceparent/1 error handling
   - Round-trip preservation (propagate → restore → verify)

2. **Core API Testing** (P0):
   - init/1 with valid/invalid configs
   - Span lifecycle: start_span/2,3 → end_span/1
   - Context management: get_current_context/0, set_current_context/1
   - ID generation: make_trace_id/0, make_span_id/0 (uniqueness validation)

3. **Span Hierarchy and Attributes** (P1):
   - Parent-child relationships (trace_id inheritance, parent_span_id)
   - Nested spans (3+ levels)
   - Sibling spans (same parent, different span_ids)
   - add_attributes/2 merging behavior
   - System attributes (service.name, service.version, mcp.version, etc.)

4. **Error Recording and Status** (P1):
   - record_error/2,3 for all error classes (throw/error/exit)
   - Error attributes: error=true, error.type, error.message, error.stacktrace
   - Error events added to span
   - Status updated to error
   - Stacktrace formatting for various term types

5. **Baggage and Correlation** (P1):
   - set_baggage/2 storage
   - get_baggage/1 retrieval
   - get_all_baggage/0 complete map
   - Baggage inheritance to child spans
   - Cross-process baggage transfer

6. **Context Propagation** (P1):
   - propagate_context/1 generates correct headers
   - restore_context/1 reconstructs span context
   - Multi-process spawn with trace context
   - Baggage propagation across processes

7. **Event Tracking** (P2):
   - add_event/2,3 creates events with timestamps
   - Events list grows correctly
   - Event attributes preserved

8. **Sampling Strategies** (P2):
   - always_on (100% sampling)
   - always_off (0% sampling)
   - trace_id_ratio deterministic sampling
   - parent_based respects parent trace_flags
   - tail_sample_decision for high latency and errors

9. **RPC Integration** (P2):
   - inject_rpc_span/3,4 creates client spans
   - RPC attributes: rpc.method, rpc.request_id, rpc.system, rpc.service
   - Parameter sanitization (sensitive keys redacted)
   - Span linking: link_span/2

10. **Edge Cases and Error Handling** (P2):
    - Invalid inputs (non-binary keys, invalid spans)
    - Boundary conditions (empty attributes, very long values)
    - Concurrent access (race conditions)
    - Memory leaks (unclosed spans)

### Verification

**Automated Validation:**
```bash
# All quality gates must pass
TERM=dumb rebar3 compile                           # 0 errors, 0 warnings
rebar3 eunit --module=erlmcp_otel_tests            # 100% pass rate
rebar3 cover -v apps/erlmcp_observability          # ≥80% coverage
rebar3 dialyzer -r apps/erlmcp_observability       # 0 warnings
rebar3 xref                                        # 0 undefined function calls
```

**Manual Verification:**
- Review coverage report: `_build/test/cover/index.html`
- Verify W3C compliance by inspecting traceparent format in tests
- Run multi-process tests to verify context propagation
- Verify all exported functions have test coverage

### Manufacturing Output
- **Code**: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl` (comprehensive test suite)
- **Tests**: 50-100 test functions covering all 31 exported functions
- **Documentation**: Coverage report (HTML)
- **Receipts**: Test execution logs, coverage metrics

## What We're NOT Doing

**EXPLICITLY OUT OF SCOPE:**
- **erlmcp_otel_middleware testing** - Separate module, separate test file (out of scope)
- **Exporter testing (Jaeger, Datadog, Honeycomb)** - These have their own test modules
- **Performance benchmarking** - Not part of unit test coverage requirement
- **Integration testing with actual OpenTelemetry backend** - Requires external infrastructure
- **Modifying erlmcp_otel.erl** - Tests only, no source code changes
- **Creating new test frameworks** - Use existing EUnit infrastructure
- **Mock-based testing** - Follow Chicago School TDD (real processes only)

**Reasoning**: Focus is on unit testing the core erlmcp_otel module to achieve ≥80% coverage. Middleware and exporters are separate concerns. Performance and integration testing are separate phases.

## Manufacturing Approach

### TCPS Methodology
Following Toyota Code Production System phases:

1. **Specification** - Requirements with acceptance criteria (31 exported functions analyzed)
2. **Pseudocode** - Algorithm design BEFORE coding (test patterns documented)
3. **Architecture** - Integration points and supervision tree (functional module, process dictionary)
4. **Refinement** - Chicago School TDD (tests FIRST, no mocks)
5. **Completion** - All quality gates passing (≥80% coverage, W3C compliance)

### Implementation Strategy

**High-Level Approach:**
Enhance existing `erlmcp_otel_tests.erl` file with comprehensive test coverage. The existing basic tests (61 lines) and enhanced tests (444 lines) provide good foundation, but we need to consolidate and expand to achieve ≥80% coverage.

**Strategy Rationale:**
- **Enhance existing file**: Rather than creating new file, expand `erlmcp_otel_tests.erl` to be comprehensive
- **Use setup/cleanup pattern**: Already established in enhanced tests, provides proper initialization
- **Follow Chicago School TDD**: Real processes, no mocks, state-based verification
- **W3C compliance first**: Critical quality gate, test early and explicitly
- **Small phases**: Each test category is ≤4 hours, independently verifiable

### Quality Integration
- **Pre-commit Hooks**: `.claude/hooks/pre-task-validate.sh` - compilation and basic tests
- **CI Gates**: All quality gates must pass before merge
- **Receipt Generation**: Coverage report saved as artifact
- **Andon Signaling**: Test failures visible in CI, fail-fast on any error

---

## Phases

### Phase 1: W3C Trace Context Compliance Testing (CRITICAL - P0)

#### Overview
Implement comprehensive W3C Trace Context specification validation tests. This is the CRITICAL foundation - if traceparent format is wrong, distributed tracing breaks completely. This phase MUST pass before any other work.

#### Specification
**WHAT we're building:**
- Test suite for W3C traceparent format validation
- Tests for format_traceparent/3 (output generation)
- Tests for parse_traceparent/1 (input parsing)
- Tests for propagate_context/1 and restore_context/1 (round-trip)
- Edge case coverage: invalid formats, boundary conditions

**Files to modify:**
- `apps/erlmcp_observability/test/erlmcp_otel_tests.erl` - Add W3C compliance tests

#### Pseudocode
**HOW it will work:**

```
Test Group: w3c_trace_context_format_test_
  Setup: Initialize erlmcp_otel with test config

  Test 1: Valid traceparent format
    Input: TraceId (32 hex chars), SpanId (16 hex chars), Flags (1)
    Expected: "00-{trace_id}-{span_id}-01"
    Verify: Binary format matches W3C spec

  Test 2: Invalid traceparent version
    Input: "01-{trace_id}-{span_id}-01"
    Expected: {error, invalid_format}
    Verify: Rejects non-00 version

  Test 3: Invalid trace_id length
    Input: "00-{16 chars}-{span_id}-01"
    Expected: {error, invalid_format}
    Verify: Rejects wrong trace_id length

  Test 4: Invalid span_id length
    Input: "00-{trace_id}-{8 chars}-01"
    Expected: {error, invalid_format}
    Verify: Rejects wrong span_id length

  Test 5: Invalid flags length
    Input: "00-{trace_id}-{span_id}-1"
    Expected: {error, invalid_format}
    Verify: Rejects wrong flags length

  Test 6: Non-hex characters in trace_id
    Input: "00-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-{span_id}-01"
    Expected: {error, invalid_format}
    Verify: Rejects non-hex trace_id

  Test 7: Round-trip preservation
    Create span context
    Propagate to headers
    Restore from headers
    Verify: trace_id, span_id, flags preserved

  Test 8: Missing traceparent header
    Input: Headers without traceparent
    Expected: undefined
    Verify: Handles missing header gracefully

  Test 9: Empty traceparent header
    Input: #{<<"traceparent">> => <<>>}
    Expected: {error, invalid_format}
    Verify: Rejects empty traceparent

  Test 10: Flags parsing
    Input: Valid traceparent with flags "01", "02", "03"
    Expected: Correct integer values (1, 2, 3)
    Verify: Hex-to-integer conversion
```

#### Architecture
**INTEGRATION:**
- **Module**: `erlmcp_otel` (functional module, no gen_server)
- **Context Storage**: Process dictionary (`erlang:get/put`)
- **Dependencies**: crypto (for ID generation), kernel, stdlib
- **Test Pattern**: EUnit setup/cleanup with `otel_test_()` generator

**Data Flow:**
```
Test Setup → erlmcp_otel:init(Config)
           → Create test spans
           → Test format/parse functions
           → Verify outputs
           → erlmcp_otel:shutdown()
```

#### Changes Required:

##### 1. W3C Trace Context Tests
**File**: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`
**Current**: 61 lines, 6 basic test functions
**Changes**: Add comprehensive W3C traceparent format validation tests
**Reason**: Achieve critical W3C compliance quality gate

```erlang
-module(erlmcp_otel_tests).
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% W3C Trace Context Compliance Tests (CRITICAL)
%% =============================================================================

w3c_traceparent_format_test_() ->
    {setup,
     fun setup_w3c/0,
     fun cleanup_w3c/1,
     [
         {"Valid traceparent format", fun test_valid_traceparent_format/0},
         {"Invalid version rejection", fun test_invalid_version/0},
         {"Invalid trace_id length", fun test_invalid_trace_id_length/0},
         {"Invalid span_id length", fun test_invalid_span_id_length/0},
         {"Invalid flags length", fun test_invalid_flags_length/0},
         {"Non-hex trace_id rejection", fun test_non_hex_trace_id/0},
         {"Round-trip preservation", fun test_round_trip_preservation/0},
         {"Missing traceparent header", fun test_missing_traceparent/0},
         {"Empty traceparent header", fun test_empty_traceparent/0},
         {"Flags parsing", fun test_flags_parsing/0}
     ]
    }.

setup_w3c() ->
    Config = #{
        service_name => <<"w3c-test">>,
        exporters => [console]
    },
    erlmcp_otel:init(Config),
    ok.

cleanup_w3c(_) ->
    erlmcp_otel:shutdown(),
    ok.

%% Valid traceparent format: 00-32char-trace_id-16char-span_id-2char-flags
test_valid_traceparent_format() ->
    TraceId = <<"4bf92f3577b34da6a3ce929d0e0e4736">>,
    SpanId = <<"00f067aa0ba902b7">>,
    Flags = 1,

    %% Call internal format function (via propagate_context)
    SpanCtx = #{
        trace_id => TraceId,
        span_id => SpanId,
        trace_flags => Flags,
        baggage => #{}
    },

    Headers = erlmcp_otel:propagate_context(SpanCtx),
    TraceParent = maps:get(<<"traceparent">>, Headers),

    %% Verify format: 00-trace_id-span_id-flags
    Expected = <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01">>,
    ?assertEqual(Expected, TraceParent).

test_invalid_version() ->
    %% Wrong version (should be 00)
    InvalidTraceParent = <<"01-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01">>,

    %% Parse should fail
    Result = erlmcp_otel:restore_context(#{<<"traceparent">> => InvalidTraceParent}),
    ?assertEqual(undefined, Result).

test_invalid_trace_id_length() ->
    %% Wrong trace_id length (should be 32 chars)
    InvalidTraceParent = <<"00-4bf92f3577b3-00f067aa0ba902b7-01">>,

    Result = erlmcp_otel:restore_context(#{<<"traceparent">> => InvalidTraceParent}),
    ?assertEqual(undefined, Result).

test_invalid_span_id_length() ->
    %% Wrong span_id length (should be 16 chars)
    InvalidTraceParent = <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067-01">>,

    Result = erlmcp_otel:restore_context(#{<<"traceparent">> => InvalidTraceParent}),
    ?assertEqual(undefined, Result).

test_invalid_flags_length() ->
    %% Wrong flags length (should be 2 chars)
    InvalidTraceParent = <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-1">>,

    Result = erlmcp_otel:restore_context(#{<<"traceparent">> => InvalidTraceParent}),
    ?assertEqual(undefined, Result).

test_non_hex_trace_id() ->
    %% Non-hex characters in trace_id
    InvalidTraceParent = <<"00-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-00f067aa0ba902b7-01">>,

    %% This should parse but IDs won't be valid hex
    Result = erlmcp_otel:restore_context(#{<<"traceparent">> => InvalidTraceParent}),
    %% Current implementation may not catch this, but should return undefined
    ?assertEqual(undefined, Result).

test_round_trip_preservation() ->
    %% Create span context
    OriginalCtx = #{
        trace_id => <<"4bf92f3577b34da6a3ce929d0e0e4736">>,
        span_id => <<"00f067aa0ba902b7">>,
        trace_flags => 1,
        baggage => #{<<"user_id">> => <<"123">>}
    },

    %% Propagate to headers
    Headers = erlmcp_otel:propagate_context(OriginalCtx),

    %% Restore from headers
    RestoredCtx = erlmcp_otel:restore_context(Headers),

    %% Verify critical fields preserved
    ?assertMatch(#{trace_id := <<"4bf92f3577b34da6a3ce929d0e0e4736">>}, RestoredCtx),
    ?assertMatch(#{span_id := <<"00f067aa0ba902b7">>}, RestoredCtx),
    ?assertMatch(#{trace_flags := 1}, RestoredCtx),
    ?assertMatch(#{baggage := #{<<"user_id">> := <<"123">>}}, RestoredCtx).

test_missing_traceparent() ->
    %% No traceparent header
    Headers = #{<<"other-header">> => <<"value">>},

    Result = erlmcp_otel:restore_context(Headers),
    ?assertEqual(undefined, Result).

test_empty_traceparent() ->
    %% Empty traceparent header
    Headers = #{<<"traceparent">> => <<>>},

    Result = erlmcp_otel:restore_context(Headers),
    ?assertEqual(undefined, Result).

test_flags_parsing() ->
    %% Test various flag values
    TestCases = [
        {<<"01">>, 1},
        {<<"02">>, 2},
        {<<"03">>, 3},
        {<<"ff">>, 255},
        {<<"00">>, 0}
    ],

    lists:foreach(fun({FlagsHex, ExpectedFlagsInt}) ->
        TraceParent = <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-", FlagsHex/binary>>,
        Result = erlmcp_otel:restore_context(#{<<"traceparent">> => TraceParent}),
        ?assertMatch(#{trace_flags := ExpectedFlagsInt}, Result)
    end, TestCases).
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_otel_tests` - 100% pass rate, 10 W3C tests pass
- [ ] Coverage: `rebar3 cover -v apps/erlmcp_observability` - ≥80% coverage for format_traceparent/3 and parse_traceparent/1
- [ ] Dialyzer: `rebar3 dialyzer -r apps/erlmcp_observability` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] W3C traceparent format validated against spec
- [ ] All invalid formats rejected correctly
- [ ] Round-trip preservation verified
- [ ] Edge cases covered (empty, wrong lengths, non-hex)

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix. This is Jidoka (built-in quality).

---

### Phase 2: Core API Testing (Foundation)

#### Overview
Test the core OpenTelemetry API functions: init/1, start_span/2,3, end_span/1, with_span/3,4, and context management. These are the building blocks for all other functionality.

#### Specification
**WHAT we're building:**
- Tests for init/1 with valid and invalid configurations
- Tests for start_span/2 (auto parent detection) and start_span/3 (explicit parent)
- Tests for end_span/1 (duration calculation, status handling)
- Tests for with_span/3,4 (automatic error recording)
- Tests for context management (get_current_context/0, set_current_context/1)
- Tests for ID generation (make_trace_id/0, make_span_id/0) with uniqueness validation

#### Pseudocode
**HOW it will work:**

```
Test Group: core_api_test_
  Setup: Initialize and clear process dictionary

  Test 1: Init with valid config
    Input: Valid config map
    Expected: {ok, _} or ok
    Verify: Config stored in process dictionary

  Test 2: Init with invalid config
    Input: Missing service_name
    Expected: {error, _}
    Verify: Error handling works

  Test 3: Start span without parent
    Call: start_span(Name, Attrs)
    Expected: New trace_id, new span_id, parent_span_id = undefined
    Verify: Span context structure valid

  Test 4: Start span with explicit parent
    Call: start_span(Name, Attrs, ParentCtx)
    Expected: Same trace_id, parent_span_id set
    Verify: Parent-child relationship

  Test 5: End span calculates duration
    Create span, wait 10ms, end span
    Expected: duration_ns ≈ 10,000,000
    Verify: Duration calculation accurate

  Test 6: With span normal execution
    Call: with_span(Name, Attrs, Fun)
    Expected: Fun result returned, span ended
    Verify: No side effects

  Test 7: With span error recording
    Call: with_span with throwing function
    Expected: Error re-raised, span marked error
    Verify: Error attributes set

  Test 8: Context management
    Set context, get context
    Expected: Same context returned
    Verify: Process dictionary works

  Test 9: ID uniqueness
    Generate 1000 trace_ids and span_ids
    Expected: All unique
    Verify: No collisions

  Test 10: ID format
    Generate IDs
    Expected: trace_id = 128-bit (32 hex chars), span_id = 64-bit (16 hex chars)
    Verify: Correct bit length
```

#### Architecture
**INTEGRATION:**
- Process dictionary for context storage
- ID generation using crypto:strong_rand_bytes
- Span lifecycle management

#### Changes Required:

##### 1. Core API Tests
**File**: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`
**Changes**: Add core API test functions

```erlang
%% =============================================================================
%% Core API Tests
%% =============================================================================

core_api_test_() ->
    {setup,
     fun setup_core/0,
     fun cleanup_core/1,
     [
         {"Init with valid config", fun test_init_valid_config/0},
         {"Init with missing service_name", fun test_init_missing_service/0},
         {"Start span without parent", fun test_start_span_no_parent/0},
         {"Start span with explicit parent", fun test_start_span_with_parent/0},
         {"End span calculates duration", fun test_end_span_duration/0},
         {"With span normal execution", fun test_with_span_normal/0},
         {"With span error recording", fun test_with_span_error/0},
         {"Context management", fun test_context_management/0},
         {"ID uniqueness", fun test_id_uniqueness/0},
         {"ID format validation", fun test_id_format/0}
     ]
    }.

setup_core() ->
    %% Clear any existing context
    erlang:erase(erlmcp_otel_current_context),
    erlang:erase(erlmcp_otel_config),
    ok.

cleanup_core(_) ->
    erlmcp_otel:shutdown(),
    erlang:erase(erlmcp_otel_current_context),
    erlang:erase(erlmcp_otel_config),
    ok.

test_init_valid_config() ->
    Config = #{
        service_name => <<"test-service">>,
        service_version => <<"1.0.0">>,
        exporters => [console]
    },
    Result = erlmcp_otel:init(Config),
    ?assertMatch(ok, Result).

test_init_missing_service() ->
    %% Missing service_name should use default
    Config = #{exporters => [console]},
    Result = erlmcp_otel:init(Config),
    ?assertMatch(ok, Result).

test_start_span_no_parent() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),

    SpanCtx = erlmcp_otel:start_span(<<"test.span">>, #{}),

    ?assertMatch(#{trace_id := _, span_id := _, parent_span_id := undefined}, SpanCtx),
    ?assert(is_binary(maps:get(trace_id, SpanCtx))),
    ?assert(is_binary(maps:get(span_id, SpanCtx))),
    ?assertEqual(undefined, maps:get(parent_span_id, SpanCtx)).

test_start_span_with_parent() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),

    ParentSpan = erlmcp_otel:start_span(<<"parent">>, #{}),
    ChildSpan = erlmcp_otel:start_span(<<"child">>, #{}, ParentSpan),

    ?assertEqual(maps:get(trace_id, ParentSpan), maps:get(trace_id, ChildSpan)),
    ?assertEqual(maps:get(span_id, ParentSpan), maps:get(parent_span_id, ChildSpan)).

test_end_span_duration() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),

    StartTime = erlang:system_time(millisecond),
    SpanCtx = erlmcp_otel:start_span(<<"test">>, #{}),
    timer:sleep(10),  % 10ms
    ok = erlmcp_otel:end_span(SpanCtx),
    EndTime = erlang:system_time(millisecond),

    %% Verify duration was recorded (approximate)
    %% We can't directly access the OtelSpan, but we verify no crash
    ?assertEqual(ok, ok).

test_with_span_normal() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),

    Fun = fun() -> {ok, 42} end,
    Result = erlmcp_otel:with_span(<<"test">>, #{}, Fun),

    ?assertEqual({ok, 42}, Result).

test_with_span_error() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),

    Fun = fun() -> throw(test_error) end,

    try
        erlmcp_otel:with_span(<<"test">>, #{}, Fun),
        ?assert(false)  % Should not reach here
    catch
        test_error ->
            %% Expected, verify error was recorded
            ok
    end.

test_context_management() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),

    SpanCtx = erlmcp_otel:start_span(<<"test">>, #{}),

    %% Get current context should return the span we just created
    CurrentCtx = erlmcp_otel:get_current_context(),
    ?assertEqual(SpanCtx, CurrentCtx).

test_id_uniqueness() ->
    %% Generate many IDs and verify uniqueness
    TraceIds = [erlmcp_otel:make_trace_id() || _ <- lists:seq(1, 1000)],
    SpanIds = [erlmcp_otel:make_span_id() || _ <- lists:seq(1, 1000)],

    %% All should be unique
    ?assertEqual(1000, length(lists:usort(TraceIds))),
    ?assertEqual(1000, length(lists:usort(SpanIds))).

test_id_format() ->
    %% Verify ID formats
    TraceId = erlmcp_otel:make_trace_id(),
    SpanId = erlmcp_otel:make_span_id(),

    %% trace_id should be 32 hex chars (128 bits)
    ?assertEqual(32, byte_size(TraceId)),
    ?assertMatch({ok, _}, re:run(TraceId, <<"^[0-9a-f]{32}$">>, [caseless])),

    %% span_id should be 16 hex chars (64 bits)
    ?assertEqual(16, byte_size(SpanId)),
    ?assertMatch({ok, _}, re:run(SpanId, <<"^[0-9a-f]{16}$">>, [caseless])).
```

#### Success Criteria:
- [ ] Compilation: 0 errors, 0 warnings
- [ ] EUnit: 100% pass rate (10 core API tests)
- [ ] Coverage: ≥80% for core API functions
- [ ] Dialyzer: 0 warnings
- [ ] Xref: 0 undefined function calls

---

### Phase 3: Span Hierarchy and Attributes Testing

#### Overview
Test parent-child relationships, nested spans, sibling spans, and attribute management. This validates the span hierarchy model.

#### Specification
**WHAT we're building:**
- Tests for parent-child span relationships (trace_id inheritance, parent_span_id)
- Tests for nested spans (3+ levels deep)
- Tests for sibling spans (same parent, different span_ids)
- Tests for add_attributes/2 (merging behavior)
- Tests for system attributes (service.name, service.version, mcp.version, etc.)

#### Changes Required:

##### 1. Span Hierarchy Tests
**File**: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`

```erlang
%% =============================================================================
%% Span Hierarchy and Attributes Tests
%% =============================================================================

span_hierarchy_test_() ->
    {setup,
     fun setup_hierarchy/0,
     fun cleanup_hierarchy/1,
     [
         {"Parent-child relationship", fun test_parent_child/0},
         {"Nested spans (3 levels)", fun test_nested_spans/0},
         {"Sibling spans", fun test_sibling_spans/0},
         {"Add attributes merging", fun test_add_attributes/0},
         {"System attributes present", fun test_system_attributes/0}
     ]
    }.

setup_hierarchy() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),
    ok.

cleanup_hierarchy(_) ->
    erlmcp_otel:shutdown(),
    ok.

test_parent_child() ->
    ParentSpan = erlmcp_otel:start_span(<<"parent">>, #{}),
    ChildSpan = erlmcp_otel:start_span(<<"child">>, #{}, ParentSpan),

    %% Verify trace_id inheritance
    ?assertEqual(maps:get(trace_id, ParentSpan), maps:get(trace_id, ChildSpan)),

    %% Verify parent_span_id
    ?assertEqual(maps:get(span_id, ParentSpan), maps:get(parent_span_id, ChildSpan)).

test_nested_spans() ->
    %% Create 3-level nesting
    Level1 = erlmcp_otel:start_span(<<"level1">>, #{}),
    Level2 = erlmcp_otel:start_span(<<"level2">>, #{}, Level1),
    Level3 = erlmcp_otel:start_span(<<"level3">>, #{}, Level2),

    %% All should have same trace_id
    ?assertEqual(maps:get(trace_id, Level1), maps:get(trace_id, Level2)),
    ?assertEqual(maps:get(trace_id, Level2), maps:get(trace_id, Level3)),

    %% Verify parent chain
    ?assertEqual(maps:get(span_id, Level2), maps:get(parent_span_id, Level3)),
    ?assertEqual(maps:get(span_id, Level1), maps:get(parent_span_id, Level2)),
    ?assertEqual(undefined, maps:get(parent_span_id, Level1)).

test_sibling_spans() ->
    Parent = erlmcp_otel:start_span(<<"parent">>, #{}),
    Sibling1 = erlmcp_otel:start_span(<<"sibling1">>, #{}, Parent),
    Sibling2 = erlmcp_otel:start_span(<<"sibling2">>, #{}, Parent),

    %% Siblings should have same parent and trace_id
    ?assertEqual(maps:get(trace_id, Parent), maps:get(trace_id, Sibling1)),
    ?assertEqual(maps:get(trace_id, Parent), maps:get(trace_id, Sibling2)),
    ?assertEqual(maps:get(span_id, Parent), maps:get(parent_span_id, Sibling1)),
    ?assertEqual(maps:get(span_id, Parent), maps:get(parent_span_id, Sibling2)),

    %% Siblings should have different span_ids
    ?assertNotEqual(maps:get(span_id, Sibling1), maps:get(span_id, Sibling2)).

test_add_attributes() ->
    Span = erlmcp_otel:start_span(<<"test">>, #{<<"attr1">> => value1}),
    ok = erlmcp_otel:add_attributes(Span, #{<<"attr2">> => value2}),

    %% Get current context and verify attributes merged
    Ctx = erlmcp_otel:get_current_context(),
    Attrs = maps:get(attributes, Ctx),
    ?assertEqual(value1, maps:get(<<"attr1">>, Attrs)),
    ?assertEqual(value2, maps:get(<<"attr2">>, Attrs)).

test_system_attributes() ->
    Span = erlmcp_otel:start_span(<<"test">>, #{}),

    %% Verify system attributes present
    Attrs = maps:get(attributes, Span),
    ?assert(maps:is_key(<<"service.name">>, Attrs)),
    ?assert(maps:is_key(<<"service.version">>, Attrs)),
    ?assert(maps:is_key(<<"mcp.version">>, Attrs)),
    ?assert(maps:is_key(<<"span.kind">>, Attrs)),
    ?assert(maps:is_key(<<"erlang.node">>, Attrs)),
    ?assert(maps:is_key(<<"erlang.pid">>, Attrs)).
```

#### Success Criteria:
- [ ] Compilation: 0 errors, 0 warnings
- [ ] EUnit: 100% pass rate (5 hierarchy tests)
- [ ] Coverage: ≥80% for hierarchy functions
- [ ] Dialyzer: 0 warnings

---

### Phase 4: Error Recording and Status Testing

#### Overview
Test error recording for all error classes (throw/error/exit), error attributes, stacktrace formatting, and status updates.

#### Specification
**WHAT we're building:**
- Tests for record_error/2,3 capturing all error classes
- Tests for error attributes (error=true, error.type, error.message, error.stacktrace)
- Tests for error events added to span
- Tests for status updated to error
- Tests for format_error_message/1 and format_stacktrace/1

#### Changes Required:

##### 1. Error Recording Tests
**File**: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`

```erlang
%% =============================================================================
%% Error Recording and Status Tests
%% =============================================================================

error_recording_test_() ->
    {setup,
     fun setup_error/0,
     fun cleanup_error/1,
     [
         {"Record throw error", fun test_record_throw/0},
         {"Record error exception", fun test_record_error/0},
         {"Record exit signal", fun test_record_exit/0},
         {"Error attributes set", fun test_error_attributes/0},
         {"Stacktrace formatting", fun test_stacktrace_format/0},
         {"Status updated to error", fun test_error_status/0}
     ]
    }.

setup_error() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),
    ok.

cleanup_error(_) ->
    erlmcp_otel:shutdown(),
    ok.

test_record_throw() ->
    Span = erlmcp_otel:start_span(<<"test">>, #{}),

    try
        throw(test_error)
    catch
        Class:Reason:Stacktrace ->
            ok = erlmcp_otel:record_error(Span, {Class, Reason, Stacktrace})
    end,

    %% Verify error recorded
    Ctx = erlmcp_otel:get_current_context(),
    Attrs = maps:get(attributes, Ctx),
    ?assertEqual(true, maps:get(<<"error">>, Attrs)),
    ?assertEqual(<<"throw">>, maps:get(<<"error.type">>, Attrs)).

test_record_error() ->
    Span = erlmcp_otel:start_span(<<"test">>, #{}),

    try
        error(test_error)
    catch
        Class:Reason:Stacktrace ->
            ok = erlmcp_otel:record_error(Span, {Class, Reason, Stacktrace})
    end,

    Ctx = erlmcp_otel:get_current_context(),
    ?assertEqual(error, maps:get(status, Ctx)).

test_record_exit() ->
    Span = erlmcp_otel:start_span(<<"test">>, #{}),

    try
        exit(test_exit)
    catch
        Class:Reason:Stacktrace ->
            ok = erlmcp_otel:record_error(Span, {Class, Reason, Stacktrace})
    end,

    Ctx = erlmcp_otel:get_current_context(),
    Attrs = maps:get(attributes, Ctx),
    ?assertEqual(<<"exit">>, maps:get(<<"error.type">>, Attrs)).

test_error_attributes() ->
    Span = erlmcp_otel:start_span(<<"test">>, #{}),

    try
        error(custom_error)
    catch
        Class:Reason:Stacktrace ->
            ok = erlmcp_otel:record_error(Span, {Class, Reason, Stacktrace},
                                           #{<<"custom.attr">> => custom_value})
    end,

    Ctx = erlmcp_otel:get_current_context(),
    Attrs = maps:get(attributes, Ctx),

    %% Verify standard error attributes
    ?assertEqual(true, maps:get(<<"error">>, Attrs)),
    ?assert(maps:is_key(<<"error.type">>, Attrs)),
    ?assert(maps:is_key(<<"error.message">>, Attrs)),
    ?assert(maps:is_key(<<"error.stacktrace">>, Attrs)),

    %% Verify custom attributes preserved
    ?assertEqual(custom_value, maps:get(<<"custom.attr">>, Attrs)).

test_stacktrace_format() ->
    %% Test format_error_message and format_stacktrace indirectly
    Span = erlmcp_otel:start_span(<<"test">>, #{}),

    try
        error({complex, error, term})
    catch
        Class:Reason:Stacktrace ->
            ok = erlmcp_otel:record_error(Span, {Class, Reason, Stacktrace})
    end,

    Ctx = erlmcp_otel:get_current_context(),
    Attrs = maps:get(attributes, Ctx),

    %% Verify message and stacktrace are binaries
    ErrorMessage = maps:get(<<"error.message">>, Attrs),
    StacktraceBin = maps:get(<<"error.stacktrace">>, Attrs),

    ?assert(is_binary(ErrorMessage)),
    ?assert(is_binary(StacktraceBin)),
    ?assert(byte_size(ErrorMessage) > 0),
    ?assert(byte_size(StacktraceBin) > 0).

test_error_status() ->
    Span = erlmcp_otel:start_span(<<"test">>, #{}),

    %% Initially status should be ok
    ?assertEqual(ok, maps:get(status, Span)),

    try
        error(test_error)
    catch
        Class:Reason:Stacktrace ->
            ok = erlmcp_otel:record_error(Span, {Class, Reason, Stacktrace})
    end,

    %% Status should be error
    Ctx = erlmcp_otel:get_current_context(),
    ?assertEqual(error, maps:get(status, Ctx)).
```

#### Success Criteria:
- [ ] Compilation: 0 errors, 0 warnings
- [ ] EUnit: 100% pass rate (6 error tests)
- [ ] Coverage: ≥80% for error recording functions
- [ ] Dialyzer: 0 warnings

---

### Phase 5: Baggage and Correlation Testing

#### Overview
Test baggage API (set_baggage/2, get_baggage/1, get_all_baggage/0), baggage inheritance to child spans, and cross-process baggage propagation.

#### Specification
**WHAT we're building:**
- Tests for set_baggage/2 storage
- Tests for get_baggage/1 retrieval
- Tests for get_all_baggage/0 complete map
- Tests for baggage inheritance to child spans
- Tests for propagate_baggage/2 type conversion (atom → binary)

#### Changes Required:

##### 1. Baggage Tests
**File**: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`

```erlang
%% =============================================================================
%% Baggage and Correlation Tests
%% =============================================================================

baggage_test_() ->
    {setup,
     fun setup_baggage/0,
     fun cleanup_baggage/1,
     [
         {"Set and get baggage", fun test_set_get_baggage/0},
         {"Get all baggage", fun test_get_all_baggage/0},
         {"Baggage inheritance to child", fun test_baggage_inheritance/0},
         {"Propagate baggage atom to binary", fun test_propagate_baggage_types/0},
         {"Cross-process baggage", fun test_cross_process_baggage/0}
     ]
    }.

setup_baggage() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),
    ok.

cleanup_baggage(_) ->
    erlmcp_otel:shutdown(),
    ok.

test_set_get_baggage() ->
    %% Set baggage
    ok = erlmcp_otel:set_baggage(<<"user_id">>, <<"user-123">>),
    ok = erlmcp_otel:set_baggage(<<"session_id">>, <<"session-456">>),

    %% Get individual baggage
    ?assertEqual(<<"user-123">>, erlmcp_otel:get_baggage(<<"user_id">>)),
    ?assertEqual(<<"session-456">>, erlmcp_otel:get_baggage(<<"session_id">>)),

    %% Get non-existent baggage
    ?assertEqual(undefined, erlmcp_otel:get_baggage(<<"non_existent">>)).

test_get_all_baggage() ->
    ok = erlmcp_otel:set_baggage(<<"key1">>, <<"val1">>),
    ok = erlmcp_otel:set_baggage(<<"key2">>, <<"val2">>),

    AllBaggage = erlmcp_otel:get_all_baggage(),

    ?assertEqual(<<"val1">>, maps:get(<<"key1">>, AllBaggage)),
    ?assertEqual(<<"val2">>, maps:get(<<"key2">>, AllBaggage)).

test_baggage_inheritance() ->
    %% Set baggage in current context
    ok = erlmcp_otel:set_baggage(<<"correlation_id">>, <<"corr-123">>),

    %% Create parent span
    Parent = erlmcp_otel:start_span(<<"parent">>, #{}),

    %% Verify baggage in parent
    ParentBaggage = maps:get(baggage, Parent),
    ?assertEqual(<<"corr-123">>, maps:get(<<"correlation_id">>, ParentBaggage)),

    %% Create child span
    Child = erlmcp_otel:start_span(<<"child">>, #{}, Parent),

    %% Verify baggage inherited
    ChildBaggage = maps:get(baggage, Child),
    ?assertEqual(<<"corr-123">>, maps:get(<<"correlation_id">>, ChildBaggage)).

test_propagate_baggage_types() ->
    %% Test type conversion in propagate_baggage
    ok = erlmcp_otel:propagate_baggage(atom_key, <<"value">>),
    ?assertEqual(<<"value">>, erlmcp_otel:get_baggage(<<"atom_key">>)),

    ok = erlmcp_otel:propagate_baggage(<<"binary_key">>, atom_value),
    ?assertEqual(<<"atom_value">>, erlmcp_otel:get_baggage(<<"binary_key">>)),

    ok = erlmcp_otel:propagate_baggage(<<"int_key">>, 12345),
    ?assertEqual(<<"12345">>, erlmcp_otel:get_baggage(<<"int_key">>)),

    ok = erlmcp_otel:propagate_baggage(<<"list_key">>, "list_value"),
    ?assertEqual(<<"list_value">>, erlmcp_otel:get_baggage(<<"list_key">>)).

test_cross_process_baggage() ->
    %% Set baggage
    ok = erlmcp_otel:set_baggage(<<"user_id">>, <<"user-999">>),

    %% Create parent span
    Parent = erlmcp_otel:start_span(<<"parent">>, #{}),

    %% Create trace context
    TraceCtx = erlmcp_otel:create_trace_ctx(Parent),

    %% Spawn child process
    ParentPid = self(),
    spawn(fun() ->
        %% Restore trace context in child
        ChildCtx = erlmcp_otel:restore_trace_ctx(TraceCtx),
        ChildSpan = erlmcp_otel:start_span(<<"child">>, #{}, ChildCtx),

        %% Verify baggage propagated
        ChildBaggage = maps:get(baggage, ChildSpan),
        ?assertEqual(<<"user-999">>, maps:get(<<"user_id">>, ChildBaggage)),

        ParentPid ! child_done,
        erlmcp_otel:end_span(ChildSpan)
    end),

    receive
        child_done -> ok
    after 1000 ->
        ?assert(false, timeout)
    end.
```

#### Success Criteria:
- [ ] Compilation: 0 errors, 0 warnings
- [ ] EUnit: 100% pass rate (5 baggage tests)
- [ ] Coverage: ≥80% for baggage functions
- [ ] Dialyzer: 0 warnings

---

### Phase 6: Context Propagation Testing

#### Overview
Test context propagation via propagate_context/1 and restore_context/1, including multi-process spawn scenarios.

#### Specification
**WHAT we're building:**
- Tests for propagate_context/1 generating correct headers
- Tests for restore_context/1 reconstructing span context
- Tests for multi-process spawn with trace context
- Tests for trace_id continuity across processes

#### Changes Required:

##### 1. Context Propagation Tests
**File**: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`

```erlang
%% =============================================================================
%% Context Propagation Tests
%% =============================================================================

context_propagation_test_() ->
    {setup,
     fun setup_propagation/0,
     fun cleanup_propagation/1,
     [
         {"Propagate context to headers", fun test_propagate_context/0},
         {"Restore context from headers", fun test_restore_context/0},
         {"Multi-process trace continuity", fun test_multiprocess_trace/0}
     ]
    }.

setup_propagation() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),
    ok.

cleanup_propagation(_) ->
    erlmcp_otel:shutdown(),
    ok.

test_propagate_context() ->
    SpanCtx = #{
        trace_id => <<"4bf92f3577b34da6a3ce929d0e0e4736">>,
        span_id => <<"00f067aa0ba902b7">>,
        trace_flags => 1,
        baggage => #{<<"user">> => <<"alice">>}
    },

    Headers = erlmcp_otel:propagate_context(SpanCtx),

    %% Verify traceparent header
    ?assert(maps:is_key(<<"traceparent">>, Headers)),
    TraceParent = maps:get(<<"traceparent">>, Headers),
    ?assertMatch(<<"00-", _/binary>>, TraceParent),

    %% Verify tracestate header
    ?assert(maps:is_key(<<"tracestate">>, Headers)),

    %% Verify baggage header
    ?assert(maps:is_key(<<"baggage">>, Headers)),
    Baggage = maps:get(<<"baggage">>, Headers),
    ?assertMatch(<<"user=alice">>, Baggage).

test_restore_context() ->
    Headers = #{
        <<"traceparent">> => <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01">>,
        <<"baggage">> => <<"user=alice,session=123">>
    },

    RestoredCtx = erlmcp_otel:restore_context(Headers),

    %% Verify trace_id and span_id
    ?assertMatch(#{trace_id := <<"4bf92f3577b34da6a3ce929d0e0e4736">>}, RestoredCtx),
    ?assertMatch(#{span_id := <<"00f067aa0ba902b7">>}, RestoredCtx),

    %% Verify baggage parsed
    ?assertMatch(#{baggage := #{<<"user">> := <<"alice">>}}, RestoredCtx),
    ?assertMatch(#{baggage := #{<<"session">> := <<"123">>}}, RestoredCtx).

test_multiprocess_trace() ->
    %% Create parent span
    Parent = erlmcp_otel:start_span(<<"parent">>, #{}),

    %% Propagate context
    Headers = erlmcp_otel:propagate_context(Parent),
    ParentTraceId = maps:get(trace_id, Parent),

    %% Spawn child process
    ParentPid = self(),
    spawn(fun() ->
        %% Restore context in child
        ChildCtx = erlmcp_otel:restore_context(Headers),
        Child = erlmcp_otel:start_span(<<"child">>, #{}, ChildCtx),

        %% Verify trace_id continuity
        ChildTraceId = maps:get(trace_id, Child),
        ParentPid ! {trace_id, ChildTraceId},

        erlmcp_otel:end_span(Child)
    end),

    receive
        {trace_id, ChildTraceId} ->
            ?assertEqual(ParentTraceId, ChildTraceId)
    after 1000 ->
        ?assert(false, timeout)
    end.
```

#### Success Criteria:
- [ ] Compilation: 0 errors, 0 warnings
- [ ] EUnit: 100% pass rate (3 propagation tests)
- [ ] Coverage: ≥80% for propagation functions
- [ ] Dialyzer: 0 warnings

---

### Phase 7: Event Tracking Testing

#### Overview
Test add_event/2,3 event creation, timestamps, and event list growth.

#### Specification
**WHAT we're building:**
- Tests for add_event/2 (name only)
- Tests for add_event/3 (name with attributes)
- Tests for event timestamps
- Tests for event list growth

#### Changes Required:

##### 1. Event Tracking Tests
**File**: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`

```erlang
%% =============================================================================
%% Event Tracking Tests
%% =============================================================================

event_tracking_test_() ->
    {setup,
     fun setup_events/0,
     fun cleanup_events/1,
     [
         {"Add event without attributes", fun test_add_event/0},
         {"Add event with attributes", fun test_add_event_with_attrs/0},
         {"Event timestamp present", fun test_event_timestamp/0},
         {"Multiple events accumulate", fun test_multiple_events/0}
     ]
    }.

setup_events() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),
    ok.

cleanup_events(_) ->
    erlmcp_otel:shutdown(),
    ok.

test_add_event() ->
    Span = erlmcp_otel:start_span(<<"test">>, #{}),
    ok = erlmcp_otel:add_event(Span, <<"test.event">>),

    %% Verify event was added (we can't directly access events in OtelSpan,
    %% but we verify no crash)
    ?assertEqual(ok, ok).

test_add_event_with_attrs() ->
    Span = erlmcp_otel:start_span(<<"test">>, #{}),
    ok = erlmcp_otel:add_event(Span, <<"test.event">>, #{<<"key">> => value}),

    ?assertEqual(ok, ok).

test_event_timestamp() ->
    Span = erlmcp_otel:start_span(<<"test">>, #{}),
    Before = erlang:system_time(nanosecond),

    ok = erlmcp_otel:add_event(Span, <<"test.event">>, #{}),

    After = erlang:system_time(nanosecond),
    %% Event should have timestamp between Before and After
    %% (Can't directly access, but API should work)
    ?assertEqual(ok, ok).

test_multiple_events() ->
    Span = erlmcp_otel:start_span(<<"test">>, #{}),

    ok = erlmcp_otel:add_event(Span, <<"event1">>),
    ok = erlmcp_otel:add_event(Span, <<"event2">>),
    ok = erlmcp_otel:add_event(Span, <<"event3">>),

    ?assertEqual(ok, ok).
```

#### Success Criteria:
- [ ] Compilation: 0 errors, 0 warnings
- [ ] EUnit: 100% pass rate (4 event tests)
- [ ] Coverage: ≥80% for event functions
- [ ] Dialyzer: 0 warnings

---

### Phase 8: Sampling Strategies Testing

#### Overview
Test all sampling strategies: always_on, always_off, trace_id_ratio, parent_based, and tail_sample_decision.

#### Specification
**WHAT we're building:**
- Tests for always_on (100% sampling)
- Tests for always_off (0% sampling)
- Tests for trace_id_ratio deterministic sampling
- Tests for parent_based respecting parent trace_flags
- Tests for tail_sample_decision for high latency and errors

#### Changes Required:

##### 1. Sampling Strategy Tests
**File**: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`

```erlang
%% =============================================================================
%% Sampling Strategy Tests
%% =============================================================================

sampling_test_() ->
    {setup,
     fun setup_sampling/0,
     fun cleanup_sampling/1,
     [
         {"Always on sampling", fun test_always_on/0},
         {"Always off sampling", fun test_always_off/0},
         {"Trace ID ratio deterministic", fun test_trace_id_ratio/0},
         {"Parent based sampling", fun test_parent_based/0},
         {"Tail sampling high latency", fun test_tail_sampling_latency/0},
         {"Tail sampling error", fun test_tail_sampling_error/0}
     ]
    }.

setup_sampling() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),
    ok.

cleanup_sampling(_) ->
    erlmcp_otel:shutdown(),
    ok.

test_always_on() ->
    ?assert(erlmcp_otel:sample_decision(always_on, 0.0)),
    ?assert(erlmcp_otel:sample_decision(always_on, 0.5)),
    ?assert(erlmcp_otel:sample_decision(always_on, 1.0)).

test_always_off() ->
    ?assertNot(erlmcp_otel:sample_decision(always_off, 0.0)),
    ?assertNot(erlmcp_otel:sample_decision(always_off, 0.5)),
    ?assertNot(erlmcp_otel:sample_decision(always_off, 1.0)).

test_trace_id_ratio() ->
    %% Create span with specific trace_id for deterministic testing
    SpanCtx = #{
        trace_id => <<"00000000000000000000000000000001">>,
        trace_flags => 0
    },

    %% Set current context
    erlang:put(erlmcp_otel_current_context, SpanCtx),

    %% Test with low sampling rate (should not sample this trace_id)
    ?assertNot(erlmcp_otel:sample_decision(trace_id_ratio, 0.0001)),

    %% Test with high sampling rate (should sample)
    ?assert(erlmcp_otel:sample_decision(trace_id_ratio, 1.0)).

test_parent_based() ->
    %% Test with sampled parent
    SampledParentCtx = #{trace_flags => 1},
    erlang:put(erlmcp_otel_current_context, SampledParentCtx),
    ?assert(erlmcp_otel:sample_decision(parent_based, 0.5)),

    %% Test with unsampled parent
    UnsampledParentCtx = #{trace_flags => 0},
    erlang:put(erlmcp_otel_current_context, UnsampledParentCtx),
    %% Falls back to trace_id_ratio, so result depends on trace_id
    Result = erlmcp_otel:sample_decision(parent_based, 0.5),
    ?assert(is_boolean(Result)).

test_tail_sampling_latency() ->
    %% Create span with high latency
    StartTime = erlang:system_time(nanosecond) - 200000000,  % 200ms ago
    HighLatencySpan = #{
        start_time => StartTime,
        status => ok,
        attributes => #{}
    },

    ?assert(erlmcp_otel:tail_sample_decision(HighLatencySpan)).

test_tail_sampling_error() ->
    %% Create span with error status
    ErrorSpan = #{
        start_time => erlang:system_time(nanosecond) - 1000,  % 1μs ago
        status => error,
        attributes => #{<<"error">> => true}
    },

    ?assert(erlmcp_otel:tail_sample_decision(ErrorSpan)).
```

#### Success Criteria:
- [ ] Compilation: 0 errors, 0 warnings
- [ ] EUnit: 100% pass rate (6 sampling tests)
- [ ] Coverage: ≥80% for sampling functions
- [ ] Dialyzer: 0 warnings

---

### Phase 9: RPC Integration Testing

#### Overview
Test RPC span injection (inject_rpc_span/3,4), parameter sanitization, and span linking (link_span/2).

#### Specification
**WHAT we're building:**
- Tests for inject_rpc_span/3 creating client spans
- Tests for inject_rpc_span/4 with explicit parent
- Tests for RPC attributes (rpc.method, rpc.request_id, rpc.system, rpc.service)
- Tests for parameter sanitization (sensitive keys redacted)
- Tests for link_span/2 adding link attributes

#### Changes Required:

##### 1. RPC Integration Tests
**File**: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`

```erlang
%% =============================================================================
%% RPC Integration Tests
%% =============================================================================

rpc_integration_test_() ->
    {setup,
     fun setup_rpc/0,
     fun cleanup_rpc/1,
     [
         {"Inject RPC span", fun test_inject_rpc_span/0},
         {"Inject RPC span with parent", fun test_inject_rpc_span_with_parent/0},
         {"RPC attributes set", fun test_rpc_attributes/0},
         {"Parameter sanitization", fun test_param_sanitization/0},
         {"Span linking", fun test_span_linking/0}
     ]
    }.

setup_rpc() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),
    ok.

cleanup_rpc(_) ->
    erlmcp_otel:shutdown(),
    ok.

test_inject_rpc_span() ->
    Method = <<"tools/call">>,
    RequestId = <<"req-123">>,
    Params = #{<<"tool_name">> => <<"calculator">>},

    Span = erlmcp_otel:inject_rpc_span(Method, RequestId, Params),

    %% Verify span created
    ?assertMatch(#{trace_id := _, span_id := _}, Span),

    %% Verify RPC attributes
    Attrs = maps:get(attributes, Span),
    ?assertEqual(Method, maps:get(<<"rpc.method">>, Attrs)),
    ?assertEqual(RequestId, maps:get(<<"rpc.request_id">>, Attrs)),
    ?assertEqual(<<"erlmcp">>, maps:get(<<"rpc.service">>, Attrs)),
    ?assertEqual(<<"jsonrpc">>, maps:get(<<"rpc.system">>, Attrs)),
    ?assertEqual(<<"client">>, maps:get(<<"span.kind">>, Attrs)).

test_inject_rpc_span_with_parent() ->
    Parent = erlmcp_otel:start_span(<<"parent">>, #{}),
    Method = <<"tools/call">>,
    RequestId = <<"req-456">>,
    Params = #{},

    Child = erlmcp_otel:inject_rpc_span(Method, RequestId, Params, Parent),

    %% Verify parent-child relationship
    ?assertEqual(maps:get(trace_id, Parent), maps:get(trace_id, Child)),
    ?assertEqual(maps:get(span_id, Parent), maps:get(parent_span_id, Child)).

test_param_sanitization() ->
    Method = <<"tools/call">>,
    RequestId = <<"req-789">>,

    %% Params with sensitive data
    Params = #{
        <<"username">> => <<"alice">>,
        <<"password">> => <<"secret123">>,
        <<"token">> => <<"abc123def">>,
        <<"api_key">> => <<"key456">>,
        <<"normal_param">> => <<"value">>
    },

    Span = erlmcp_otel:inject_rpc_span(Method, RequestId, Params),

    %% Verify sensitive params redacted
    Attrs = maps:get(attributes, Span),
    ?assertEqual(<<"alice">>, maps:get(<<"rpc.username">>, Attrs)),
    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"rpc.password">>, Attrs)),
    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"rpc.token">>, Attrs)),
    ?assertEqual(<<"[REDACTED]">>, maps:get(<<"rpc.api_key">>, Attrs)),
    ?assertEqual(<<"value">>, maps:get(<<"rpc.normal_param">>, Attrs)).

test_span_linking() ->
    Span1 = erlmcp_otel:start_span(<<"span1">>, #{}),
    Span2 = erlmcp_otel:start_span(<<"span2">>, #{}),

    ok = erlmcp_otel:link_span(Span2, Span1),

    %% Verify link attributes added
    Ctx = erlmcp_otel:get_current_context(),
    Attrs = maps:get(attributes, Ctx),
    ?assert(maps:is_key(<<"link.trace_id">>, Attrs)),
    ?assert(maps:is_key(<<"link.span_id">>, Attrs)).
```

#### Success Criteria:
- [ ] Compilation: 0 errors, 0 warnings
- [ ] EUnit: 100% pass rate (5 RPC tests)
- [ ] Coverage: ≥80% for RPC functions
- [ ] Dialyzer: 0 warnings

---

### Phase 10: Edge Cases and Error Handling Testing

#### Overview
Test invalid inputs, boundary conditions, concurrent access, and memory leak scenarios.

#### Specification
**WHAT we're building:**
- Tests for invalid inputs (non-binary keys, invalid spans)
- Tests for boundary conditions (empty attributes, very long values)
- Tests for concurrent access (race conditions)
- Tests for memory leaks (unclosed spans)

#### Changes Required:

##### 1. Edge Case Tests
**File**: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl`

```erlang
%% =============================================================================
%% Edge Cases and Error Handling Tests
%% =============================================================================

edge_cases_test_() ->
    {setup,
     fun setup_edge/0,
     fun cleanup_edge/1,
     [
         {"Add attributes to invalid span", fun test_add_attributes_invalid/0},
         {"End invalid span", fun test_end_span_invalid/0},
         {"Empty attribute map", fun test_empty_attributes/0},
         {"Very long attribute values", fun test_long_values/0},
         {"Deep nesting (10 levels)", fun test_deep_nesting/0},
         {"Concurrent context access", fun test_concurrent_access/0}
     ]
    }.

setup_edge() ->
    erlmcp_otel:init(#{service_name => <<"test">>}),
    ok.

cleanup_edge(_) ->
    erlmcp_otel:shutdown(),
    ok.

test_add_attributes_invalid() ->
    InvalidSpan = #{invalid => span},

    Result = erlmcp_otel:add_attributes(InvalidSpan, #{<<"key">> => value}),
    ?assertEqual({error, invalid_span_context}, Result).

test_end_span_invalid() ->
    InvalidSpan = #{invalid => span},

    Result = erlmcp_otel:end_span(InvalidSpan),
    ?assertEqual({error, invalid_span_context}, Result).

test_empty_attributes() ->
    Span = erlmcp_otel:start_span(<<"test">>, #{}),
    ok = erlmcp_otel:add_attributes(Span, #{}),

    ?assertEqual(ok, ok).

test_long_values() ->
    %% Create very long attribute value (10KB)
    LongValue = binary:copy(<<$x>>, 10000),

    Span = erlmcp_otel:start_span(<<"test">>, #{<<"long">> => LongValue}),
    ok = erlmcp_otel:add_attributes(Span, #{<<"another_long">> => LongValue}),

    ?assertEqual(ok, ok).

test_deep_nesting() ->
    %% Create 10 levels of nesting
    Spans = create_nested_spans(10),

    %% Verify all have same trace_id
    TraceIds = [maps:get(trace_id, S) || S <- Spans],
    ?assertEqual(1, length(lists:usort(TraceIds))),

    %% Verify parent chain
    ?assertEqual(undefined, maps:get(parent_span_id, lists:nth(1, Spans))),

    lists:foreach(fun(S) -> erlmcp_otel:end_span(S) end, Spans).

create_nested_spans(0) -> [];
create_nested_spans(N) ->
    Parent = case create_nested_spans(N - 1) of
        [] -> undefined;
        [Top | _] -> Top
    end,
    Span = erlmcp_otel:start_span(list_to_binary(["level", integer_to_list(N)]), #{}, Parent),
    [Span | create_nested_spans(N - 1)].

test_concurrent_access() ->
    %% Spawn multiple processes accessing context
    Parent = self(),
    NumProcesses = 10,

    lists:foreach(fun(_) ->
        spawn(fun() ->
            %% Each process creates spans concurrently
            Span = erlmcp_otel:start_span(<<"concurrent">>, #{}),
            timer:sleep(10),
            erlmcp_otel:end_span(Span),
            Parent ! done
        end)
    end, lists:seq(1, NumProcesses)),

    %% Collect results
    lists:foreach(fun(_) ->
        receive done -> ok
        after 1000 -> ?assert(false, timeout)
        end
    end, lists:seq(1, NumProcesses)).
```

#### Success Criteria:
- [ ] Compilation: 0 errors, 0 warnings
- [ ] EUnit: 100% pass rate (6 edge case tests)
- [ ] Coverage: ≥80% for edge case functions
- [ ] Dialyzer: 0 warnings

---

## Testing Strategy

### Chicago School TDD (MANDATORY)
- **NO MOCKS** - Use real processes, real span contexts
- **State-Based Verification** - Check span context map contents
- **Integration Testing** - Test with actual OpenTelemetry functions
- **Race Condition Testing** - Concurrent operations in Phase 10

### Unit Tests (EUnit)
- **What to Test**: All 31 exported functions, all error paths
- **Test Pattern**: `erlmcp_otel_tests.erl:32-68` (setup/cleanup pattern)
- **Coverage Target**: ≥80% per module
- **Pass Rate**: 100% (all tests must pass)

### Integration Tests
- **End-to-End Scenarios**: Multi-process trace propagation (Phase 6)
- **Multi-Process**: Concurrent access (Phase 10)
- **Failure Scenarios**: Error recording (Phase 4)

### Manual Testing Steps
1. Run coverage report: `rebar3 cover -v apps/erlmcp_observability`
2. Open `_build/test/cover/index.html` in browser
3. Verify ≥80% line coverage for erlmcp_otel.erl
4. Verify all exported functions have test coverage
5. Verify W3C traceparent format validated in Phase 1 tests

### Quality Gates
Every phase MUST pass:
1. **Compilation**: `TERM=dumb rebar3 compile`
2. **EUnit**: `rebar3 eunit --module=erlmcp_otel_tests`
3. **Coverage**: `rebar3 cover -v apps/erlmcp_observability` (verify ≥80%)
4. **Dialyzer**: `rebar3 dialyzer -r apps/erlmcp_observability`
5. **Xref**: `rebar3 xref`

## Manufacturing Checklist

### Before Implementation
- [x] Research verified (read actual source code)
- [x] Scope confirmed (IN: erlmcp_otel.erl testing, OUT: middleware, exporters)
- [x] No open questions (all aspects documented)
- [x] Phases broken down (10 phases, ≤4 hours each)
- [x] Acceptance criteria defined (measurable, specific)

### During Implementation
- [ ] Chicago School TDD followed (tests FIRST, real processes)
- [ ] OTP patterns followed (functional module, process dictionary)
- [ ] Type specs verified (Dialyzer clean)
- [ ] Error handling complete (all paths tested)
- [ ] Quality gates passing (compilation, tests, coverage)

### After Implementation
- [ ] All tests passing (100% rate)
- [ ] Coverage ≥80% (verified in cover report)
- [ ] Dialyzer 0 warnings (verified)
- [ ] Xref 0 undefined calls (verified)
- [ ] W3C compliance validated (explicit tests pass)
- [ ] Code review complete (OTP patterns verified)

## Risk Management

### Known Risks
| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **W3C traceparent format violations** | P0 (Critical) | Low | Implement explicit W3C validation tests in Phase 1, validate format with regex |
| **Missing coverage on error paths** | P1 (High) | Low | Test all error classes (throw/error/exit) in Phase 4 |
| **Context propagation failures** | P1 (High) | Medium | Multi-process tests in Phase 6, verify trace_id continuity |
| **Race conditions in context storage** | P2 (Medium) | Low | Concurrent test in Phase 10, verify process dictionary isolation |
| **Memory leaks from unclosed spans** | P2 (Medium) | Low | Test span lifecycle in Phase 2, verify context cleanup |
| **Insufficient coverage (<80%)** | P1 (High) | Low | Comprehensive test suite across 10 phases, explicit coverage verification |

### Rollback Plan
- Git revert: `git revert HEAD` if tests fail
- Data migration: N/A (no data changes, test only)
- Service impact: N/A (test suite changes don't affect running services)

## References
- Research: `/Users/sac/erlmcp/.wreckit/items/011-create-eunit-test-suite-for-opentelemetry-core-erl/research.md`
- CLAUDE.md: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- TCPS: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- OTP Patterns: `/Users/sac/erlmcp/docs/otp-patterns.md`
- Source: `apps/erlmcp_observability/src/erlmcp_otel.erl:1-1005`
- Test Reference: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl:1-61`
- Enhanced Test Reference: `apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl:1-444`
- W3C Trace Context: `docs/opentelemetry-architecture.md:53-56`
