# Research: OTEL Exporters Test Coverage

**Date**: 2026-01-29
**Item**: 037-otel-exporters-test-coverage
**Section**: observability
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
OpenTelemetry exporters have ZERO coverage. Production telemetry depends on these exporters working correctly.

**Motivation:** Exporters send telemetry data to backends. If they fail, all observability data is lost.

**Success criteria:**
- 3 test files created (one per exporter: jaeger, datadog, honeycomb)
- Protocol compliance verified for each backend
- Coverage: ≥80% per exporter
- All tests pass

**Technical constraints:**
- Protocol Compliance - Correct backend format
- Batch Testing - Multiple spans in one export
- Error Handling - Backend unavailable, retry logic
- Performance - Export overhead minimal

**Signals:** priority: high, urgency: P1 - BLOCKS PRODUCTION MONITORING

### Quality Gate Status
- **Gate Type**: Test Coverage
- **Current State**: 0% coverage for exporters (jaeger, datadog, honeycomb)
- **Target State**: ≥80% coverage per exporter module
- **Gap**: 80 percentage points (complete absence of tests)

## Summary

**Manufacturing Objective:**
Create comprehensive test suites for three OpenTelemetry exporters (Jaeger, Datadog, Honeycomb) that verify protocol compliance, error handling, batch processing, and performance. These exporters are critical production infrastructure—when they fail, all observability data is lost, creating a complete system blackout.

**Technical Approach:**
Apply Chicago School TDD principles to create three EUnit test modules (`erlmcp_otel_jaeger_tests.erl`, `erlmcp_otel_datadog_tests.erl`, `erlmcp_otel_honeycomb_tests.erl`). Use `meck` to mock `httpc:request` for HTTP protocol verification. Test with real processes (actual gen_server state, real queue operations) and verify protocol encoding (OTLP JSON format). Each test suite must validate init/1, export_spans/2, shutdown/1, format_span/1, encode_batch/1, and send_batch/2 functions.

**TCPS Justification:**
**Jidoka**: Every exporter failure must be visible—tests verify error logging and return values. **Poka-yoke**: Type specs and contracts prevent invalid configurations (e.g., missing endpoint, invalid API keys). **Kaizen**: Metrics collection enables root cause analysis of export failures. **Heijunka**: Incremental test phases—first init/shutdown, then single span, then batch, then error handling. **Andon**: Test failures are visible with clear error messages showing protocol violations.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl` (286 lines)
  - `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_datadog.erl` (233 lines)
  - `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl` (253 lines)
  - `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_tests.erl` (61 lines - basic OTEL tests)
  - `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl` (444 lines - integration tests)

- **Patterns**:
  - Jaeger: Queue-based batching with `httpc:request` for HTTP POST (lines 196-205)
  - Datadog: Tag enrichment with `env`, `service`, `version` metadata (lines 132-144)
  - Honeycomb: Sampling with `crypto:hash(md5, TraceId)` for deterministic sampling (line 192)

- **Tests**: 0% coverage for exporter modules. Only basic init/shutdown tests exist in `erlmcp_otel_enhanced_tests.erl:313-360` (no protocol validation, no error paths, no batch testing).

- **Quality**: FAIL - Exporters are production-deployed with ZERO test coverage. This violates TCPS zero-defect mandate (99.99966% defect-free delivery).

### Key Files

- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:75-89` - `init/1`: Config validation (missing endpoint returns error)
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:92-112` - `export_spans/2`: Queue management with batch flush on max_queue_size
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:132-155` - `format_span/1`: OTLP JSON structure with traceId, spanId, parentSpanId, timestamps
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:158-177` - `encode_batch/1`: Wraps spans in resourceSpans → scopeSpans → spans structure
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:179-205` - `send_batch/2`: HTTP POST with content-type headers (http_protobuf or http_json)

- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_datadog.erl:120-129` - `format_span/1`: Datadog attribute naming conventions
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_datadog.erl:132-144` - `add_datadog_tags/2`: Merges env, service, version tags with custom tags
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_datadog.erl:209-232` - `send_batch/2`: Handles HTTP 200 AND 202 (Datadog accepts both)

- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl:81-84` - `export_spans/2`: Applies sampling before queuing
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl:122-145` - `format_span/1`: Honeycomb event format (trace.trace_id, trace.span_id, timestamp, duration_ms)
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl:161-171` - `calculate_sample_rate/1`: Higher sample rate for errors (1) and slow requests (2)
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl:188-193` - `should_sample/2`: Deterministic MD5 hash of trace_id

- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_metrics_tests.erl:8-49` - Test pattern to follow: setup/cleanup with gen_server:start_link and gen_server:stop
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl:313-327` - Existing (inadequate) Jaeger test pattern

### OTP Patterns Observed
- **Behavior**: Functional modules (NOT gen_server). Exporters are stateful maps (no process, just data structure).
- **Supervision**: No supervision required (stateless functional modules).
- **Process Pattern**: Queue-based batching with `queue:queue()` data structure. Timer references for batch flush.
- **Test Pattern**: Chicago School TDD - use `meck` to mock `httpc:request` for HTTP protocol validation. No fake backends needed—verify protocol encoding and error handling.

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - `erlmcp_otel.erl` - Main OTEL integration (may invoke exporters)
  - `erlmcp_otel_middleware.erl` - Middleware that uses exporters
- **External Libraries**:
  - `opentelemetry_exporter 1.10.0` - Official OpenTelemetry Erlang exporter (reference implementation)
  - `jsx` - JSON encoding for OTLP protocol (line 177 in jaeger, line 166 in datadog, line 234 in honeycomb)
  - `httpc` (inets) - HTTP client for backend communication
  - `crypto` - MD5 hashing for Honeycomb sampling
- **OTP Applications**:
  - `kernel` - Timer management (erlang:send_after, erlang:cancel_timer)
  - `stdlib` - queue operations (queue:new, queue:in, queue:len, queue:to_list, queue:is_empty)
  - `inets` - httpc for HTTP requests

### TCPS Quality Gates to Pass
- [ ] **Compilation**: 0 errors (exporters already compile)
- [ ] **EUnit**: 100% pass rate (all new tests must pass)
- [ ] **Common Test**: Not required (EUnit sufficient for unit testing)
- [ ] **Coverage**: ≥80% per exporter module (currently 0%)
- [ ] **Dialyzer**: 0 warnings (exporters already have type specs)
- [ ] **Xref**: 0 undefined function calls (httpc is external)
- [ ] **Performance**: Export overhead <10ms per batch (verify with timer:tc/3)

### Patterns to Follow

**Test Module Pattern**: `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_metrics_tests.erl:1-50`
```erlang
-module(erlmcp_otel_jaeger_tests).
-include_lib("eunit/include/eunit.hrl").

%% Fixture setup/cleanup
setup() ->
    meck:new(httpc, [unstick]),
    ok.

cleanup(_) ->
    meck:unload(httpc),
    ok.

%% Test generator
jaeger_exporter_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"Initialize exporter with valid config", fun test_init_valid/0},
            {"Initialize exporter with missing endpoint", fun test_init_missing_endpoint/0},
            {"Export single span", fun test_export_single_span/0},
            {"Export batch spans triggers flush", fun test_export_batch_flush/0},
            {"Format span to OTLP JSON", fun test_format_span/0},
            {"Send batch HTTP 200 success", fun test_send_batch_success/0},
            {"Send batch HTTP error returns error", fun test_send_batch_http_error/0},
            {"Shutdown cancels timer and flushes queue", fun test_shutdown_flushes/0}
        ]
    }.
```

**Mock Pattern**: Use `meck:expect(httpc, request, fun(...) -> ... end)` to simulate HTTP responses.

**Protocol Validation**: Decode JSON with `jsx:decode/1` and verify OTLP structure:
```erlang
?assertMatch(#{<<"resourceSpans">> := [_]}, jsx:decode(Payload)).
```

**State Inspection**: Access internal state via `erlmcp_otel_jaeger:format_span/1` and `erlmcp_otel_jaeger:encode_batch/1` (exported as internal API).

**Error Handling Pattern**:
```erlang
test_send_batch_http_error() ->
    meck:expect(httpc, request, fun(_, _, _, _) ->
        {ok, {{_, 500, _}, _, <<"Internal Server Error">>}}
    end),
    Config = #{endpoint => <<"http://localhost:4318/v1/traces">>, ...},
    Span = #{trace_id => <<"123">>, span_id => <<"456">>, ...},
    Result = erlmcp_otel_jaeger:send_batch([Span], Config),
    ?assertMatch({error, {http_error, 500}}, Result).
```

## Root Cause Analysis (5 Whys)

**Problem**: OTEL exporters have ZERO test coverage despite being production-deployed.

1. **Why?** Tests were not created when exporter modules were implemented.
2. **Why?** Focus was on implementing OTEL integration features, not comprehensive testing.
3. **Why?** No TCPS quality gate enforcement during initial development (pre-TCMS adoption).
4. **Why?** Missing test coverage requirement in item 037 (created 2026-01-29).
5. **ROOT CAUSE**: Quality debt from pre-TCMS era—exporters deployed to production without test coverage validation.

**Solution**: Create comprehensive EUnit test suites following Chicago School TDD. Verify protocol compliance with mocked HTTP. Test error paths (backend unavailable, HTTP errors, timeout). Validate batch processing (queue overflow, timer flush). Use `rebar3 cover` to verify ≥80% coverage.

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Exporter silently fails to send telemetry** | P0 (Critical) | Complete observability blackout—no traces, no metrics, impossible to debug production issues | Mock httpc to verify all error paths return `{error, Reason}`. Test HTTP 500, connection refused, timeout. Validate error_logger:error_msg calls. |
| **Protocol format mismatch (OTLP spec violation)** | P0 (Critical) | Backend rejects telemetry—data loss, corrupted traces | Decode JSON payload and verify OTLP spec compliance. Test with real span data (trace_id, span_id, timestamps, attributes). |
| **Batch overflow causes memory leak** | P1 (High) | Queue grows unbounded, OOM under load | Test max_queue_size trigger. Verify queue:new() after flush. Test with 10K spans. |
| **Honeycomb sampling discards all traces** | P1 (High) | Sampling logic bug—sample_rate=10 means 0 samples sent | Test should_sample/2 with deterministic trace_ids. Verify MD5 hash sampling logic (sample_rate=1 should always sample). |
| **Timer leak in shutdown** | P2 (Medium) | Timer references not cancelled—resource leak | Verify erlang:cancel_timer(Timer) called in shutdown/1. Test with multiple init/shutdown cycles. |
| **Datadog API key exposed in logs** | P3 (Low) | Security issue—API keys logged in error messages | Verify error_logger calls don't include sensitive headers. Test with api_key in config. |

## Recommended Manufacturing Approach

**TCPS Methodology:**

1. **Specification** - Requirements with acceptance criteria
   - Each exporter must have ≥80% coverage
   - Protocol compliance verified (OTLP JSON structure)
   - Error handling tested (HTTP errors, connection failures)
   - Batch processing validated (queue overflow, timer flush)

2. **Pseudocode** - Algorithm design BEFORE coding
   ```
   For each exporter (jaeger, datadog, honeycomb):
     Create test module
     Add meck setup/cleanup fixtures
     Test init/1: valid config → {ok, State}, missing endpoint → {error, missing_endpoint}
     Test export_spans/2: single span → queued, batch → flush
     Test format_span/1: verify OTLP JSON structure
     Test send_batch/2: mock httpc responses (200, 500, timeout)
     Test shutdown/1: verify timer cancelled, queue flushed
     Run rebar3 cover to verify ≥80%
   ```

3. **Architecture** - Integration points and dependencies
   - Exporters depend on httpc (inets) - mock with meck
   - Exporters use jsx for JSON - verify encoding
   - Exporters use queue (stdlib) - test queue operations
   - No gen_server - state is just a map (no process)

4. **Refinement** - Chicago School TDD (tests FIRST)
   - Write test before implementation
   - Test fails initially (red)
   - Implement minimal code to pass
   - Refactor for clarity
   - Test passes (green)

5. **Completion** - All quality gates passing
   - rebar3 compile → 0 errors
   - rebar3 eunit --module=erlmcp_otel_jaeger_tests → 100% pass
   - rebar3 cover --module=erlmcp_otel_jaeger → ≥80%
   - rebar3 dialyzer → 0 warnings
   - Manual: verify test output shows protocol validation

**Implementation Strategy:**

**Phase 1: Jaeger Exporter Tests (P0 - Critical)**
- Create `erlmcp_otel_jaeger_tests.erl`
- Test init/1, export_spans/2, shutdown/1
- Mock httpc for HTTP protocol validation
- Verify OTLP JSON encoding (resourceSpans structure)
- Test error paths (HTTP 500, connection timeout)
- Run rebar3 cover to verify ≥80%

**Phase 2: Datadog Exporter Tests (P0 - Critical)**
- Create `erlmcp_otel_datadog_tests.erl`
- Test tag enrichment (env, service, version)
- Test HTTP 200 AND 202 (Datadog-specific)
- Test API key header injection
- Verify protocol compliance
- Run rebar3 cover to verify ≥80%

**Phase 3: Honeycomb Exporter Tests (P0 - Critical)**
- Create `erlmcp_otel_honeycomb_tests.erl`
- Test sampling logic (should_sample/2)
- Test deterministic sampling (MD5 hash)
- Test Honeycomb event format (trace.trace_id, duration_ms)
- Test ISO 8601 timestamp formatting
- Run rebar3 cover to verify ≥80%

**Quality Validation:**

**Automated:**
```bash
cd /Users/sac/erlmcp/apps/erlmcp_observability
rebar3 compile
rebar3 eunit --module=erlmcp_otel_jaeger_tests
rebar3 eunit --module=erlmcp_otel_datadog_tests
rebar3 eunit --module=erlmcp_otel_honeycomb_tests
rebar3 cover --module=erlmcp_otel_jaeger
rebar3 cover --module=erlmcp_otel_datadog
rebar3 cover --module=erlmcp_otel_honeycomb
```

**Manual:**
- Verify test output shows "All tests passed"
- Verify coverage report shows ≥80% for each exporter
- Verify protocol validation tests decode JSON and check OTLP structure
- Verify error handling tests mock httpc with 500, timeout, connection refused

**Metrics:**
- Coverage percentage per module (target: ≥80%)
- Test count per module (target: ≥10 tests per exporter)
- Execution time (target: <100ms per test suite)

## Open Questions
**NONE** - Research complete. All exporter modules analyzed. Test patterns documented. Protocol requirements understood. Mock strategy (meck for httpc) validated. Success criteria quantified (≥80% coverage, 100% pass rate).

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) - Pre-TCMS quality debt, no tests created during implementation
- [x] Quality gates defined (specific thresholds) - ≥80% coverage, 100% pass rate, 0 errors, protocol compliance
- [x] OTP patterns understood (behaviors, supervision) - Functional modules (no gen_server), queue-based batching, state maps
- [x] Test strategy clear (Chicago School TDD) - Mock httpc with meck, verify protocol encoding, test error paths
- [x] Risk assessment complete (severity P0-P3) - P0: silent failures, protocol violations; P1: batch overflow, sampling bugs; P2: timer leaks; P3: API key exposure
- [x] No open questions (all research complete) - All code analyzed, dependencies documented, test patterns validated
