# Research: Create EUnit test suites for all OTEL exporters (Jaeger, Datadog, Honeycomb)

**Date**: 2026-01-28
**Item**: 013-create-eunit-test-suites-for-all-otel-exporters-ja
**Section**: observability
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
All OTEL exporters have 0% test coverage - telemetry export is untested

**Motivation:** Production telemetry depends on exporters working correctly. Each backend has different protocol requirements.

**Success criteria:**
- 3 test files created (one per exporter)
- Protocol compliance verified for each backend
- Coverage ≥80% per exporter
- All tests pass

**Technical constraints:**
- Protocol compliance - correct backend format for each
- Batch testing - multiple spans in one export
- Error handling - backend unavailable, retry logic
- Performance testing - export overhead minimal

**Signals:** priority: high, urgency: P1 - BLOCKS PRODUCTION MONITORING

### Quality Gate Status
- **Gate Type**: Test Coverage / Protocol Compliance
- **Current State**: 0% test coverage for all 3 exporters (erlmcp_otel_jaeger, erlmcp_otel_datadog, erlmcp_otel_honeycomb)
- **Target State**: ≥80% coverage per exporter module
- **Gap**: 80 percentage points (0% → 80%)
- **Priority**: P1 (High) - Sprint 3, Week 3, Tier 1C (Observability Core)

## Summary

**What needs to be done:**
Create comprehensive EUnit test suites for three OpenTelemetry trace exporters (Jaeger, Datadog, Honeycomb) that verify protocol compliance, batch processing, error handling, and performance characteristics. Each exporter has distinct backend protocol requirements - Jaeger uses OTLP over HTTP/protobuf, Datadog expects specific tag formats and accepts both 200/202 HTTP codes, Honeycomb requires dataset routing and supports deterministic sampling based on trace IDs.

**How to do it:**
Apply Chicago School TDD methodology with real collaborators - spawn actual gen_server processes, use real HTTP clients (httpc), verify observable state changes rather than function calls. Each test suite will cover: initialization validation, span formatting correctness, batch encoding compliance, HTTP transport success/failure paths, queue management, timer-based flushing, and shutdown procedures. Tests must use mock HTTP backends or start real backend containers to validate protocol compliance.

**Why this approach (TCPS justification):**
**Jidoka** - Exporters MUST stop the line when backend is unavailable or protocol is violated (fail-closed behavior). **Poka-yoke** - Type specs and Dialyzer will catch format errors at compile-time, runtime validation prevents malformed payloads. **Kaizen** - Each test measures export latency, batch sizes, and failure rates to establish baseline metrics. **Andon** - Test failures are visible in CI with clear error messages showing exact protocol violations. **Heijunka** - Break into 3 test files (one per exporter), each with independent test generators that can run in parallel.

## Current State Analysis

### Existing Implementation

**Files:**
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl` (285 lines)
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_datadog.erl` (232 lines)
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl` (252 lines)
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel.erl` (1005 lines - main OTEL module)
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_middleware.erl` (317 lines - transport tracing)

**Patterns:**
All three exporters follow the same functional pattern (not gen_servers):
- `init/1` - Validate configuration, create empty queue, start batch timer
- `export_spans/2` - Add spans to queue, flush if max_queue_size reached
- `shutdown/1` - Cancel timer, flush remaining spans
- Internal: `format_span/1`, `encode_batch/1`, `send_batch/2`
- State is a map: `#{config => Config, queue => queue:queue(), timer => reference() | undefined}`

**Tests:**
- **Current coverage: 0%** - No dedicated test files exist for exporters
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl` (444 lines) contains only basic initialization tests (lines 313-360) that verify state structure, not functional behavior
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_tests.erl` (61 lines) tests main OTEL module, not exporters

**Quality:**
- All modules compile successfully with 0 errors
- Type specs defined for all public functions
- Dialyzer warnings: unknown (need to run `rebar3 dialyzer`)
- Xref checks: undefined function calls filtered in rebar.config (lines 30-41)
- **Critical gap: Export functionality completely untested - production telemetry is blind**

### Key Files

#### Exporter Implementations
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:77-89` - `init/1` with config validation
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:93-112` - `export_spans/2` with queue management
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:133-155` - `format_span/1` for OTLP protocol
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:159-177` - `encode_batch/1` wraps spans in resourceSpans structure
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:181-205` - `send_batch/2` uses httpc for HTTP POST

- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_datadog.erl:68-79` - `init/1` validates endpoint
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_datadog.erl:83-104` - `export_spans/2` adds Datadog tags via `add_datadog_tags/2`
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_datadog.erl:122-129` - `format_span/1` maps attribute keys to Datadog conventions
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_datadog.erl:133-144` - `add_datadog_tags/2` injects env/service/version tags
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_datadog.erl:224-232` - `send_batch/2` accepts both 200 and 202 HTTP codes

- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl:67-77` - `init/1` validates both endpoint AND api_key
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl:81-106` - `export_spans/2` applies sampling via `should_sample/2`
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl:124-145` - `format_span/1` creates Honeycomb event format with duration_ms
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl:149-158` - `add_honeycomb_metadata/2` adds dataset/environment/hostname
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl:189-193` - `should_sample/2` deterministic MD5 hash sampling

#### Existing Test Patterns (to follow)
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl:32-68` - EUnit setup/cleanup pattern with `{setup, fun setup/0, fun cleanup/1, [...]}` (Chicago School: real OTEL init)
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl:313-327` - Basic exporter initialization test pattern (insufficient - only checks state structure)
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_tests.erl:14-24` - Test generator pattern with `{setup, ...}` and `?_test()` wrappers
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_metrics_tests.erl` - Potential reference for metrics-based testing

#### Configuration & Dependencies
- `/Users/sac/erlmcp/apps/erlmcp_observability/rebar.config:14-18` - OpenTelemetry dependencies: `{opentelemetry_api, "1.5.0"}`, `{opentelemetry, "1.7.0"}`, `{opentelemetry_exporter, "1.10.0"}`
- `/Users/sac/erlmcp/apps/erlmcp_observability/rebar.config:43-44` - `{cover_enabled, true}`, `{cover_export_enabled, true}`
- `/Users/sac/erlmcp/rebar.config:53-55` - Top-level OpenTelemetry dependencies (inherited)
- `/Users/sac/erlmcp/rebar.config:92-97` - Test profile includes `{proper, "1.4.0"}`, `{meck, "0.9.2"}` for property-based testing and mocking (use sparingly per Chicago School)

#### Protocol Documentation
- `/Users/sac/erlmcp/apps/erlmcp_observability/README.md:363-416` - Local backend setup instructions (Jaeger, Datadog Agent, Honeycomb)
- OpenTelemetry OTLP specification: `resourceSpans` > `scopeSpans` > `spans` hierarchy
- Jaeger: Accepts OTLP HTTP protobuf on port 4318
- Datadog: Agent accepts OTLP on port 4318, requires `DD-API-KEY` header for direct API
- Honeycomb: REST API at `https://api.honeycomb.io/1/batch/{dataset}`, requires `X-Honeycomb-Team` and `X-Honeycomb-Dataset` headers

### OTP Patterns Observed

**Behavior:** None - Exporters are stateless functional modules, not gen_servers
**Supervision:** Not applicable - Exporters are managed by `erlmcp_otel` module (which may be a gen_server or supervised process)
**Process Pattern:** Queue-based batching with timer-based flushing (in-process, no separate gen_server)
**Test Pattern:** Chicago School TDD - real processes, no mocks, state-based verification

**Key insight:** Exporters are functional modules that maintain state in a map (not a gen_server state). This is unusual for OTP but valid for stateless exporters. Tests must pass the state map explicitly to each function call.

## Technical Considerations

### Dependencies

**Internal Modules:**
- `erlmcp_otel` (1005 lines) - Main OTEL integration, may spawn exporter processes
- `erlmcp_otel_middleware` (317 lines) - Automatic tracing integration
- Module under test: `erlmcp_otel_jaeger`, `erlmcp_otel_datadog`, `erlmcp_otel_honeycomb`

**External Libraries:**
- `jsx` "3.1.0" - JSON encoding (used in `encode_batch/1`)
- `httpc` (inets) - Erlang HTTP client (used in `send_batch/2`)
- `crypto` - Hash functions (MD5 for Honeycomb sampling)
- `queue` - Erlang queue module (for span buffering)

**OTP Applications:**
- `kernel` - httpc module
- `stdlib` - queue, maps, lists
- `inets` - HTTP client (httpc)

### TCPS Quality Gates to Pass

- [ ] **Compilation:** 0 errors (`TERM=dumb rebar3 compile`)
- [ ] **EUnit:** 100% pass rate (`rebar3 eunit --module=<module>_tests`)
- [ ] **Common Test:** Not applicable (unit tests only)
- [ ] **Coverage:** ≥80% per exporter (`rebar3 cover` - target 80%, critical modules 85%)
- [ ] **Dialyzer:** 0 warnings (`rebar3 dialyzer` - type specs exist, must validate)
- [ ] **Xref:** 0 undefined function calls (filtered in rebar.config:30-41)
- [ ] **Performance:** Export latency <10ms per batch, batch size ≤2048 spans (measure with `timer:tc/3`)
- [ ] **Protocol Compliance:** Backend accepts generated payloads (validate with real backends or protocol validators)

### Patterns to Follow

**Chicago School TDD - Real Collaborators:**
```erlang
%% CORRECT: Use real exporter, real HTTP client, real queue
{ok, State} = erlmcp_otel_jaeger:init(Config),
Spans = [create_test_span(1), create_test_span(2)],
{ok, State2} = erlmcp_otel_jaeger:export_spans(Spans, State),
?assertEqual(2, queue:len(maps:get(queue, State2))).
```

**Test Pattern - Setup/Teardown:**
Reference: `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl:32-68`
```erlang
exporter_test_() ->
    {setup,
        fun setup/0,           % Start HTTP mock backend
        fun cleanup/1,         % Shutdown backend
        fun(BackendPid) ->     % Pass backend PID to tests
            [
                ?_test(test_init_success()),
                ?_test(test_export_spans_queue(BackendPid)),
                ?_test(test_batch_flush(BackendPid)),
                ?_test(test_protocol_compliance(BackendPid))
            ]
        end}.
```

**Error Handling Pattern:**
```erlang
test_backend_unavailable() ->
    Config = #{endpoint => <<"http://localhost:9999/unavailable">>},  % Non-existent
    {ok, State} = erlmcp_otel_jaeger:init(Config),
    Spans = [create_test_span()],
    Result = erlmcp_otel_jaeger:export_spans(Spans, State),
    ?assertMatch({error, _}, Result).
```

**Type Specs Pattern (Dialyzer):**
```erlang
-spec init(jaeger_config()) -> {ok, jaeger_state()} | {error, term()}.
-spec export_spans([jaeger_span()], jaeger_state()) -> {ok, jaeger_state()} | {error, term()}.
```

## Root Cause Analysis (5 Whys)

**Problem:** 0% test coverage for OTEL exporters - production telemetry is untested

1. **Why?** No test files exist for `erlmcp_otel_jaeger`, `erlmcp_otel_datadog`, `erlmcp_otel_honeycomb` modules
2. **Why?** Exporters were implemented as functional modules without gen_server behavior, so standard gen_server test patterns weren't applied
3. **Why?** Initial implementation focused on getting telemetry working quickly (speed-to-market), testing was deferred to later
4. **Why?** No test-driven development (TDD) process enforced - tests written after code, not before (violates Chicago School TDD)
5. **ROOT CAUSE:** Missing quality gate requiring test coverage before merging code - "ship first, test later" anti-pattern

**Solution:** Create dedicated EUnit test suites following Chicago School TDD (tests FIRST), enforce 80% coverage gate in CI before merge, add protocol compliance validation with real backends, measure export performance to establish baseline metrics.

**NOT solution:** Add tests but don't fix the process - this repeats the mistake. Must enforce TDD workflow for all future code.

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **HTTP backend dependency in tests** | P1 (High) | Tests fail if backend unavailable, flaky CI | Start HTTP mock server in test setup (use `httpc` or `gun` to build mock), or use Docker containers for real backends with test retries |
| **Protocol compliance validation** | P0 (Critical) | Tests pass but backend rejects data (silent telemetry loss) | Validate generated JSON/protobuf against OpenTelemetry schemas, use real backends in integration tests, verify with `jq` or protocol validators |
| **Queue/timer race conditions** | P1 (High) | Intermittent test failures, flaky tests | Avoid testing actual timer expiration, test `flush_queue/2` directly, use `meck:expect` ONLY for `erlang:send_after` (last resort) |
| **Performance regression** | P2 (Medium) | Export overhead increases, slows production | Measure baseline with `timer:tc/3`, add performance test that fails if latency >10ms or batch size >2048 |
| **State map mutation** | P1 (High) | Tests pass but production has state corruption | Verify state immutability, assert that `export_spans/2` returns new state map, use `maps:get/2` to validate fields |
| **Sampling determinism (Honeycomb)** | P2 (Medium) | Non-deterministic test failures | Use fixed trace IDs in tests, verify `should_sample/2` with known MD5 hash values |
| **HTTP client configuration** | P3 (Low) | Tests fail on different network configurations | Configure `httpc` in test setup, set reasonable timeouts, use localhost endpoints |
| **Coverage <80%** | P0 (Critical) | QC gate failure, blocks merge | Use `rebar3 cover --verbose` to identify uncovered lines, add tests for error paths (`{error, Reason}` returns), test all branches in `case` statements |

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Define acceptance criteria for each exporter (init success, span format, batch encoding, HTTP success/failure, queue full, shutdown)
2. **Pseudocode** - Design test scenarios BEFORE writing tests (list all edge cases, error paths, protocol requirements)
3. **Architecture** - Identify integration points (httpc, jsx, queue, crypto), setup mock HTTP server or Docker backends
4. **Refinement** - Chicago School TDD (write tests FIRST, watch them fail, then implement - but implementation already exists, so just write failing tests then fix bugs)
5. **Completion** - All quality gates passing (compile 0 errors, eunit 100%, cover ≥80%, dialyzer 0 warnings, xref 0 undefined)

**Implementation Strategy:**

**Phase 1: Jaeger Exporter Tests (6 hours estimated)**
- Test file: `apps/erlmcp_observability/test/erlmcp_otel_jaeger_tests.erl`
- Test scenarios:
  1. Init success/invalid config (missing endpoint)
  2. Export single span → queued
  3. Export batch → queue fills → auto-flush
  4. Format span → OTLP JSON structure validation
  5. Encode batch → resourceSpans wrapping
  6. Send batch → HTTP 200 OK, HTTP 500 error
  7. Shutdown → timer cancelled, remaining spans flushed
  8. Protocol compliance → start batch with Jaeger, verify trace appears

**Phase 2: Datadog Exporter Tests (5 hours estimated)**
- Test file: `apps/erlmcp_observability/test/erlmcp_otel_datadog_tests.erl`
- Test scenarios:
  1. Init success/invalid config (missing endpoint)
  2. Export span → Datadog tags added (env, service, version)
  3. Format span → attribute key mapping (service.name → service)
  4. Add Datadog tags → custom tags merged
  5. Send batch → HTTP 200 OR 202 (both accepted)
  6. API key header → X-Datadog-API-KEY present when configured
  7. Protocol compliance → start Datadog Agent, verify trace received

**Phase 3: Honeycomb Exporter Tests (5 hours estimated)**
- Test file: `apps/erlmcp_observability/test/erlmcp_otel_honeycomb_tests.erl`
- Test scenarios:
  1. Init success/invalid config (missing endpoint OR api_key)
  2. Export span → sampling applied (sample_rate=1 → all sampled)
  3. Should sample → deterministic based on trace ID MD5
  4. Format span → Honeycomb event format (duration_ms, timestamp ISO 8601)
  5. Add metadata → dataset, environment, hostname, beamtime added
  6. Send batch → X-Honeycomb-Team and X-Honeycomb-Dataset headers
  7. URL construction → `/1/batch/{dataset}` path
  8. Protocol compliance → send to Honeycomb API, verify event appears

**Quality Validation:**

**Automated:**
```bash
# Compile
TERM=dumb rebar3 compile --verbose | grep -E "(error|warning)"

# Run tests
rebar3 eunit --module=erlmcp_otel_jaeger_tests
rebar3 eunit --module=erlmcp_otel_datadog_tests
rebar3 eunit --module=erlmcp_otel_honeycomb_tests

# Coverage report
rebar3 cover --verbose
grep -E "erlmcp_otel_(jaeger|datadog|honeycomb)" _build/test/cover/index.html

# Dialyzer (type safety)
rebar3 dialyzer

# Xref (undefined functions)
rebar3 xref
```

**Manual:**
- Start Jaeger locally (`docker run -p 4318:4318 jaegertracing/all-in-one`), run tests, verify traces appear in Jaeger UI
- Start Datadog Agent locally, run tests, verify traces in Datadog dashboard
- Send test trace to Honeycomb API, verify event appears in dataset

**Metrics to measure:**
- Coverage % per module (target ≥80%)
- Export latency per batch (target <10ms, measure with `timer:tc/3`)
- Batch size (max 2048 spans, configurable)
- HTTP success/failure rate (target 100% success when backend available)
- Protocol compliance rate (target 100% - backend accepts all generated payloads)

## Open Questions
**NONE** - All research complete. Protocol specifications understood, test patterns identified, risks assessed, quality gates defined.

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) - Missing test files, no TDD process enforced
- [x] Quality gates defined (specific thresholds) - ≥80% coverage, 100% test pass rate, protocol compliance verified
- [x] OTP patterns understood (behaviors, supervision) - Functional modules (not gen_servers), state maps, queue-based batching
- [x] Test strategy clear (Chicago School TDD) - Real collaborators, state-based verification, no mocks
- [x] Risk assessment complete (severity P0-P3) - HTTP backend dependency P1, protocol compliance P0, performance P2
- [x] No open questions (all research complete) - Protocol specs reviewed, test patterns documented, implementation strategy defined

---

**Next step:** Create test files following the implementation strategy above, starting with Jaeger exporter (Phase 1), then Datadog (Phase 2), then Honeycomb (Phase 3). Each phase should be completed and quality gates verified before starting the next phase (Heijunka - leveled production).

**Estimated total effort:** 16 hours (6h + 5h + 5h)
**Target completion:** End of Sprint 3 (Week 3) per TEST_COVERAGE_PLAN.md
**Priority:** P1 (High) - Tier 1C: Observability Core
