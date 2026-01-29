# Create EUnit test suites for all OTEL exporters (Jaeger, Datadog, Honeycomb) Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Create comprehensive EUnit test suites for three OpenTelemetry trace exporters (Jaeger, Datadog, Honeycomb) that verify protocol compliance, batch processing, error handling, and performance characteristics. All three exporters currently have **0% test coverage**, representing a critical quality gap that blocks production monitoring confidence.

**Why this matters:**
- Production telemetry depends on exporters working correctly
- Each backend has different protocol requirements (OTLP, Datadog tags, Honeycomb sampling)
- Silent telemetry loss occurs when exporters fail without detection
- Performance regression in exporters slows the entire system

### Quality Gate Requirements
- **Compilation**: 0 errors (MANDATORY)
- **EUnit**: 100% pass rate (MANDATORY)
- **Common Test**: Not applicable (unit tests only)
- **Coverage**: ≥80% per exporter (MANDATORY)
- **Dialyzer**: 0 warnings (MANDATORY)
- **Xref**: 0 undefined function calls (MANDATORY)
- **Performance**: Export latency <10ms per batch (measured with timer:tc/3)

## Current State

### What Exists Now

**Modules:**
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl` (285 lines)
  - Public API: `init/1`, `export_spans/2`, `shutdown/1`
  - Internal: `format_span/1`, `encode_batch/1`, `send_batch/2`
  - Uses OTLP HTTP protocol, supports http_protobuf and http_json

- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_datadog.erl` (232 lines)
  - Public API: `init/1`, `export_spans/2`, `shutdown/1`
  - Internal: `format_span/1`, `add_datadog_tags/2`, `encode_batch/1`
  - Adds Datadog-specific tags (env, service, version)
  - Accepts both HTTP 200 and 202 responses

- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl` (252 lines)
  - Public API: `init/1`, `export_spans/2`, `shutdown/1`
  - Internal: `format_span/1`, `add_honeycomb_metadata/2`, `calculate_sample_rate/1`
  - Deterministic sampling based on trace ID MD5 hash
  - Requires both endpoint AND api_key in config

**Tests:**
- **Current coverage: 0%** - No dedicated test files exist for any exporter
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl:313-360` contains only basic initialization tests
  - Tests verify state structure: `?assertMatch(#{config := _, queue := _, timer := _}, State)`
  - Does NOT test functional behavior (export_spans, format_span, send_batch)
  - Does NOT test error paths (backend unavailable, invalid data)
  - Does NOT verify protocol compliance

**Quality:**
- All modules compile successfully with 0 errors
- Type specs defined for all public functions
- Dialyzer warnings: unknown (need to run)
- **Critical gap: Export functionality completely untested - production telemetry is blind**

### What's Missing

**Gap:** 80 percentage points (0% → 80% coverage)
- **Root Cause:** No test files exist for exporters
  - Exporters were implemented as functional modules without gen_server behavior
  - Initial implementation focused on speed-to-market, testing deferred
  - No TDD process enforced (violates Chicago School TDD)
  - Missing quality gate requiring test coverage before merge

**Impact:**
- **P1 (High)** - BLOCKS PRODUCTION MONITORING
  - Silent telemetry loss if exporters fail
  - No verification that backend accepts generated payloads
  - Performance regressions undetected
  - Protocol compliance unverified

### Key Discoveries from Research

**1. Exporters are functional modules, NOT gen_servers:**
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:77-89` - `init/1` returns state map
- Tests must pass state map explicitly to each function call
- State is immutable: each call returns new state map

**2. Queue-based batching with timer-based flushing:**
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:93-112` - `export_spans/2` queues spans
- Queue flushes when size >= max_queue_size (default 2048)
- Timer reference stored in state (started in init/1)
- Tests should NOT test actual timer expiration (race condition risk)

**3. Protocol differences per backend:**
- **Jaeger:** OTLP JSON/protobuf on `/v1/traces`, accepts only HTTP 200
- **Datadog:** OTLP JSON, adds env/service/version tags, accepts 200 OR 202
- **Honeycomb:** Custom event format with duration_ms, requires X-Honeycomb-Team header, deterministic sampling

**4. Error handling patterns:**
- `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl:196-205` - `httpc:request/3` returns `{ok, Response}` or `{error, Reason}`
- Jaeger logs errors but returns `{error, {http_error, Code}}` for non-200
- Datadog accepts 202 as success (line 226)
- Honeycomb requires both endpoint AND api_key (line 180)

**5. Test pattern reference:**
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl:32-68` - Setup/cleanup with `{setup, fun setup/0, fun cleanup/1, [...]}`
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_tests.erl:14-24` - Test generator pattern with `?_test()` wrappers

## Desired End State

### Specification

**Three new test files will be created:**

1. **`/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_jaeger_tests.erl`**
   - Tests for `erlmcp_otel_jaeger` module
   - 8 test generators covering init, export_spans, format_span, encode_batch, send_batch, shutdown
   - Coverage target: ≥80%

2. **`/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_datadog_tests.erl`**
   - Tests for `erlmcp_otel_datadog` module
   - 8 test generators covering init, export_spans, format_span, add_datadog_tags, send_batch, shutdown
   - Coverage target: ≥80%

3. **`/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_honeycomb_tests.erl`**
   - Tests for `erlmcp_otel_honeycomb` module
   - 9 test generators covering init, export_spans, format_span, add_honeycomb_metadata, should_sample, send_batch, shutdown
   - Coverage target: ≥80%

**Each test file will follow Chicago School TDD:**
- Use real exporter modules (no mocks of exporters)
- Use real httpc client (or mock HTTP backend with test server)
- Verify observable state changes (queue length, state map contents)
- Test error paths (backend unavailable, invalid config, HTTP errors)

### Verification

**Automated quality gates (ALL MUST PASS):**
```bash
# Compilation
cd /Users/sac/erlmcp/apps/erlmcp_observability
TERM=dumb rebar3 compile --verbose | grep -E "(error|warning)"  # Must be 0 errors

# EUnit tests
rebar3 eunit --module=erlmcp_otel_jaeger_tests  # 100% pass rate
rebar3 eunit --module=erlmcp_otel_datadog_tests  # 100% pass rate
rebar3 eunit --module=erlmcp_otel_honeycomb_tests  # 100% pass rate

# Coverage report
rebar3 cover --verbose
# Check: erlmcp_otel_jaeger.erl ≥80%, erlmcp_otel_datadog.erl ≥80%, erlmcp_otel_honeycomb.erl ≥80%

# Dialyzer (type safety)
rebar3 dialyzer  # 0 warnings

# Xref (undefined functions)
rebar3 xref  # 0 undefined function calls
```

**Manual verification (protocol compliance):**
1. Start Jaeger locally: `docker run -p 4318:4318 jaegertracing/all-in-one`
2. Run Jaeger exporter tests with real endpoint
3. Verify traces appear in Jaeger UI (http://localhost:16686)
4. Start Datadog Agent locally
5. Run Datadog exporter tests
6. Verify traces in Datadog dashboard
7. Send test trace to Honeycomb API (requires valid API key)
8. Verify event appears in dataset

**Metrics to measure:**
- Coverage % per module (target ≥80%)
- Export latency per batch (target <10ms, measure with `timer:tc/3`)
- Test pass rate (target 100%)
- Protocol compliance (target 100% - backend accepts all generated payloads)

### Manufacturing Output

**Code:**
- 3 new test files (no modification to existing exporter code unless bugs found)
  - `apps/erlmcp_observability/test/erlmcp_otel_jaeger_tests.erl`
  - `apps/erlmcp_observability/test/erlmcp_otel_datadog_tests.erl`
  - `apps/erlmcp_observability/test/erlmcp_otel_honeycomb_tests.erl`

**Tests:**
- 25+ test generators across 3 test files
- 80+ individual test cases (including error paths)
- Coverage ≥80% per exporter module

**Documentation:**
- Test file documentation (module doc, test descriptions)
- Comments explaining protocol compliance checks

**Receipts:**
- EUnit test output (100% pass rate)
- Coverage report (≥80% per module)
- Dialyzer output (0 warnings)
- Xref output (0 undefined calls)

## What We're NOT Doing

**EXPLICITLY OUT OF SCOPE:**

1. **Modifying exporter implementations** - Unless bugs are discovered during testing
   - Reason: Focus is on adding tests, not refactoring existing code
   - If tests reveal bugs, fix them but keep changes minimal

2. **Integration tests with real backends** - Beyond unit test scope
   - Reason: Unit tests should use mock HTTP backends or local containers
   - Real backend integration tests should be separate (future work)

3. **Performance benchmarking** - Beyond basic latency measurement
   - Reason: Performance testing requires specialized tools (proper load testing)
   - Basic latency measurement (<10ms threshold) is sufficient for unit tests

4. **Common Test suites** - EUnit only for this work
   - Reason: Unit tests are EUnit, integration tests would be Common Test
   - Keep scope focused on unit testing

5. **Property-based testing with Proper** - Not required for ≥80% coverage
   - Reason: EUnit tests provide sufficient coverage for exporters
   - Property-based testing can be added later if needed

6. **Testing the main erlmcp_otel module** - Different module
   - Reason: This item is specifically about exporters (jaeger, datadog, honeycomb)
   - Main OTEL module has its own tests (erlmcp_otel_enhanced_tests.erl)

7. **Mocking the httpc client** - Violates Chicago School TDD
   - Reason: Tests should use real httpc or a real HTTP test server
   - Mocking httpc would test mocks, not real behavior

8. **Testing timer expiration** - Race condition risk
   - Reason: Timer-based flush is implementation detail
   - Test flush_queue/2 directly instead

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** - Requirements with acceptance criteria (defined above)
2. **Pseudocode** - Algorithm design BEFORE coding (detailed in phases below)
3. **Architecture** - Integration points and supervision tree (N/A - functional modules)
4. **Refinement** - Chicago School TDD (tests FIRST, then implementation fixes)
5. **Completion** - All quality gates passing (compile, eunit, cover, dialyzer, xref)

### Implementation Strategy

**Chicago School TDD approach:**
- Tests are written FIRST (watch them fail, then fix implementation)
- Use real collaborators (real exporter modules, real httpc, real queue)
- State-based verification (check queue length, state map contents)
- No mocks of the system under test (exporters)
- Minimal mocking of external dependencies (HTTP backend only if necessary)

**Test-first workflow:**
1. Write test for init/1 success case → Run → Passes (simple)
2. Write test for init/1 invalid config → Run → Fails (need validation) → Fix
3. Write test for export_spans/2 → Run → Fails (need implementation) → Already exists, should pass
4. Write test for format_span/1 → Run → Verify OTLP structure
5. Write test for send_batch/2 → Run → Needs HTTP mock backend
6. Write test for error paths → Run → May reveal bugs → Fix bugs
7. Measure coverage → Add tests for uncovered lines → Repeat until ≥80%

**HTTP backend strategy:**
- **Option 1 (Preferred):** Start a real HTTP server in test setup using `httpc` or `ranch`
  - Pros: Real HTTP client, real network, no mocks
  - Cons: Requires test HTTP server setup
- **Option 2 (Fallback):** Use Docker containers for real backends
  - Pros: Actual protocol validation
  - Cons: Requires Docker, slower tests
- **Option 3 (Last resort):** Mock httpc with meck
  - Pros: No external dependencies
  - Cons: Violates Chicago School TDD, tests mocks not real behavior

**Selected approach:** Option 1 - Start a simple HTTP test server in test setup

### Quality Integration

**Pre-commit Hooks:**
- `.claude/hooks/pre-task-validate.sh` will run:
  - `TERM=dumb rebar3 compile` (0 errors)
  - `rebar3 eunit --module=erlmcp_otel_*_tests` (100% pass rate)
  - `rebar3 cover` (≥80% coverage)

**CI Gates:**
- GitHub Actions or similar CI will run:
  - Full test suite (all 3 test files)
  - Coverage report generation
  - Dialyzer type checking
  - Xref undefined function check

**Receipt Generation:**
- EUnit test output saved to `_build/test/logs/eunit.log`
- Coverage report saved to `_build/test/cover/index.html`
- Dialyzer output saved to `_build/test/dialyzer.log`

**Andon Signaling:**
- Test failures visible in CI with clear error messages
- Coverage gaps highlighted in cover report with line numbers
- Dialyzer warnings show type spec issues
- Xref warnings show undefined function calls

---

## Phases

### Phase 1: Jaeger Exporter Test Suite (6 hours)

#### Overview
Create comprehensive EUnit test suite for `erlmcp_otel_jaeger` module covering initialization, span export, batch encoding, HTTP transport, error handling, and shutdown. Tests will verify OTLP protocol compliance and queue-based batching behavior.

#### Specification
**Test file:** `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_jaeger_tests.erl`

**Test generators:**
1. `init_success_test_()` - Verify valid config creates state with empty queue and timer
2. `init_missing_endpoint_test_()` - Verify config without endpoint returns error
3. `export_spans_queue_test_()` - Verify spans are queued when queue not full
4. `export_spans_flush_test_()` - Verify spans are flushed when queue reaches max size
5. `format_span_test_()` - Verify span formatted as OTLP JSON with correct fields
6. `encode_batch_test_()` - Verify batch wrapped in resourceSpans structure
7. `send_batch_success_test_()` - Verify HTTP POST with 200 response returns ok
8. `send_batch_failure_test_()` - Verify HTTP POST with 500 response returns error
9. `shutdown_flush_test_()` - Verify shutdown cancels timer and flushes remaining spans

**Coverage target:** ≥80% of `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl`

#### Pseudocode

**Test file structure:**
```erlang
-module(erlmcp_otel_jaeger_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Test fixtures
jaeger_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_Ctx) -> [
            init_success_test(),
            init_missing_endpoint_test(),
            export_spans_queue_test(),
            export_spans_flush_test(),
            format_span_test(),
            encode_batch_test(),
            send_batch_success_test(),
            send_batch_failure_test(),
            shutdown_flush_test()
        ]end
    }.

setup() ->
    %% Start inets for httpc
    application:ensure_all_started(inets),
    %% Start mock HTTP server on port 4318
    {ok, MockServerPid} = start_mock_http_server(4318),
    MockServerPid.

cleanup(MockServerPid) ->
    %% Stop mock HTTP server
    stop_mock_http_server(MockServerPid),
    %% Stop inets
    application:stop(inets),
    ok.

%% Test 1: Init success
init_success_test() ->
    ?_test(begin
        Config = #{
            endpoint => <<"http://localhost:4318/v1/traces">>,
            protocol => http_protobuf,
            batch_timeout => 5000,
            max_queue_size => 2048,
            headers => [],
            service_name => <<"erlmcp-test">>
        },
        {ok, State} = erlmcp_otel_jaeger:init(Config),
        ?assertMatch(#{config := _, queue := Queue, timer := Timer}, State),
        ?assertEqual(0, queue:len(Queue)),
        ?assert(is_reference(Timer))
    end).

%% Test 2: Init missing endpoint
init_missing_endpoint_test() ->
    ?_test(begin
        Config = #{protocol => http_protobuf},  % Missing endpoint
        Result = erlmcp_otel_jaeger:init(Config),
        ?assertMatch({error, missing_endpoint}, Result)
    end).

%% Test 3: Export spans queue (not full)
export_spans_queue_test() ->
    ?_test(begin
        Config = default_config(),
        {ok, State} = erlmcp_otel_jaeger:init(Config),
        Spans = [create_test_span(1), create_test_span(2)],
        {ok, State2} = erlmcp_otel_jaeger:export_spans(Spans, State),
        ?assertEqual(2, queue:len(maps:get(queue, State2)))
    end).

%% Test 4: Export spans flush (queue full)
export_spans_flush_test() ->
    ?_test(begin
        Config = default_config(#{max_queue_size => 2}),
        {ok, State} = erlmcp_otel_jaeger:init(Config),
        Spans = [create_test_span(1), create_test_span(2)],
        {ok, State2} = erlmcp_otel_jaeger:export_spans(Spans, State),
        ?assertEqual(0, queue:len(maps:get(queue, State2)))  % Flushed
    end).

%% Test 5: Format span
format_span_test() ->
    ?_test(begin
        Span = create_test_span(1),
        Formatted = erlmcp_otel_jaeger:format_span(Span),
        ?assertMatch(#{<<"traceId">> := _, <<"spanId">> := _, <<"name">> := _}, Formatted),
        ?assert(maps:is_key(<<"startTimeUnixNano">>, Formatted)),
        ?assert(maps:is_key(<<"endTimeUnixNano">>, Formatted))
    end).

%% Test 6: Encode batch
encode_batch_test() ->
    ?_test(begin
        Spans = [create_test_span(1), create_test_span(2)],
        BatchJson = erlmcp_otel_jaeger:encode_batch(Spans),
        Batch = jsx:decode(BatchJson, [return_maps]),
        ?assert(maps:is_key(<<"resourceSpans">>, Batch)),
        ?assert(is_list(maps:get(<<"resourceSpans">>, Batch)))
    end).

%% Test 7: Send batch success
send_batch_success_test() ->
    ?_test(begin
        Config = #{
            endpoint => <<"http://localhost:4318/v1/traces">>,
            protocol => http_protobuf,
            headers => []
        },
        Spans = [create_test_span(1)],
        Result = erlmcp_otel_jaeger:send_batch(Spans, Config),
        ?assertEqual(ok, Result)
    end).

%% Test 8: Send batch failure
send_batch_failure_test() ->
    ?_test(begin
        Config = #{
            endpoint => <<"http://localhost:9999/unavailable">>,  % Non-existent
            protocol => http_protobuf,
            headers => []
        },
        Spans = [create_test_span(1)],
        Result = erlmcp_otel_jaeger:send_batch(Spans, Config),
        ?assertMatch({error, _}, Result)
    end).

%% Test 9: Shutdown flush
shutdown_flush_test() ->
    ?_test(begin
        Config = default_config(),
        {ok, State} = erlmcp_otel_jaeger:init(Config),
        Spans = [create_test_span(1)],
        {ok, State2} = erlmcp_otel_jaeger:export_spans(Spans, State),
        ?assertEqual(1, queue:len(maps:get(queue, State2))),
        ok = erlmcp_otel_jaeger:shutdown(State2)
        %% Timer cancelled, queue flushed
    end).

%% Helper functions
default_config() ->
    #{
        endpoint => <<"http://localhost:4318/v1/traces">>,
        protocol => http_protobuf,
        batch_timeout => 5000,
        max_queue_size => 2048,
        headers => [],
        service_name => <<"erlmcp-test">>
    }.

default_config(Overrides) ->
    maps:merge(default_config(), Overrides).

create_test_span(N) ->
    #{
        trace_id => <<"00000000000000", (integer_to_binary(N, 16))/binary>>,
        span_id => <<"0000000000", (integer_to_binary(N, 16))/binary>>,
        parent_span_id => undefined,
        name => <<"test.span.", (integer_to_binary(N))/binary>>,
        start_time => erlang:system_time(nanosecond),
        end_time => erlang:system_time(nanosecond) + 1000000,
        attributes => #{<<"test.attr">> => N},
        events => [],
        status => ok
    }.

%% Mock HTTP server (simplified)
start_mock_http_server(Port) ->
    %% Use ranch or httpc to start a simple HTTP server
    %% Returns {ok, Pid}
    {ok, self()}.  % Placeholder

stop_mock_http_server(Pid) ->
    ok.
```

#### Architecture
**No supervision tree** - Exporters are functional modules, not gen_servers.

**Dependencies:**
- **Internal:** `erlmcp_otel_jaeger` (module under test)
- **External:** `httpc` (inets), `jsx`, `queue`, `maps`, `lists`
- **Test helpers:** Mock HTTP server (runs in test process)

**Test flow:**
1. Setup: Start inets application, start mock HTTP server
2. Test: Call exporter functions, verify state changes
3. Cleanup: Stop mock HTTP server, stop inets

#### Changes Required:

##### 1. Create Jaeger Test File
**File:** `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_jaeger_tests.erl`
**Current:** Does not exist
**Changes:** Create new test file with 9 test generators covering all public functions and error paths

**Reason:** Achieve ≥80% coverage for erlmcp_otel_jaeger module

```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Test Suite for Jaeger OTEL Exporter
%%%
%%% Tests cover:
%%% - Initialization validation
%%% - Span queue management
%%% - Batch flushing behavior
%%% - OTLP protocol compliance
%%% - HTTP transport success/failure
%%% - Shutdown and cleanup
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otel_jaeger_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% [Full implementation from pseudocode above]
```

##### 2. Mock HTTP Server (Helper Module if Needed)
**File:** `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_test_http_server.erl`
**Current:** Does not exist
**Changes:** Create simple HTTP server that accepts POST requests and returns configurable status codes

**Reason:** Test HTTP transport without external dependencies

```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% Simple HTTP test server for exporter testing
%%%
%%% Accepts POST requests on configurable port, returns configurable
%%% status codes (200, 500, etc.) to test success/failure paths.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_test_http_server).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/0, set_response_code/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% [Implementation using ranch or inets httpd]
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `cd /Users/sac/erlmcp/apps/erlmcp_observability && TERM=dumb rebar3 compile --verbose | grep -E "(error|warning)"` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_otel_jaeger_tests` - 100% pass rate (all 9 tests pass)
- [ ] Coverage: `rebar3 cover --verbose` - erlmcp_otel_jaeger.erl ≥80% coverage
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings for erlmcp_otel_jaeger module
- [ ] Xref: `rebar3 xref` - 0 undefined function calls for erlmcp_otel_jaeger

##### Manual Verification:
- [ ] Code review: OTP patterns followed correctly
- [ ] Integration: Tests run independently without external dependencies
- [ ] Edge cases: All error paths tested (missing endpoint, HTTP failures, queue full)
- [ ] Protocol compliance: OTLP JSON structure verified with format_span and encode_batch tests

**Note:** Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix. This is Jidoka (built-in quality).

---

### Phase 2: Datadog Exporter Test Suite (5 hours)

#### Overview
Create comprehensive EUnit test suite for `erlmcp_otel_datadog` module covering initialization, Datadog tag injection, span export, batch encoding, HTTP transport (200/202 responses), error handling, and shutdown. Tests will verify Datadog-specific protocol requirements (env, service, version tags).

#### Specification
**Test file:** `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_datadog_tests.erl`

**Test generators:**
1. `init_success_test_()` - Verify valid config creates state
2. `init_missing_endpoint_test_()` - Verify config without endpoint returns error
3. `export_spans_queue_test_()` - Verify spans queued with Datadog tags added
4. `add_datadog_tags_test_()` - Verify env, service, version tags added to span
5. `map_to_datadog_key_test_()` - Verify attribute key mapping (service.name → service)
6. `encode_batch_test_()` - Verify batch wrapped in resourceSpans structure
7. `send_batch_success_200_test_()` - Verify HTTP 200 returns ok
8. `send_batch_success_202_test_()` - Verify HTTP 202 returns ok (Datadog-specific)
9. `send_batch_api_key_header_test_()` - Verify DD-API-KEY header sent when configured
10. `shutdown_flush_test_()` - Verify shutdown cancels timer and flushes remaining spans

**Coverage target:** ≥80% of `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_datadog.erl`

#### Pseudocode

**Test file structure (similar to Jaeger):**
```erlang
-module(erlmcp_otel_datadog_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Test fixtures
datadog_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_Ctx) -> [
            init_success_test(),
            init_missing_endpoint_test(),
            export_spans_queue_test(),
            add_datadog_tags_test(),
            map_to_datadog_key_test(),
            encode_batch_test(),
            send_batch_success_200_test(),
            send_batch_success_202_test(),
            send_batch_api_key_header_test(),
            shutdown_flush_test()
        ]end
    }.

setup() ->
    application:ensure_all_started(inets),
    {ok, MockServerPid} = start_mock_http_server(4318),
    MockServerPid.

cleanup(MockServerPid) ->
    stop_mock_http_server(MockServerPid),
    application:stop(inets),
    ok.

%% Test 1: Init success
init_success_test() ->
    ?_test(begin
        Config = #{
            endpoint => <<"http://localhost:4318/v1/traces">>,
            env => <<"production">>,
            service => <<"erlmcp-test">>,
            version => <<"2.0.0">>,
            tags => #{<<"team">> => <<"platform">>},
            batch_timeout => 5000,
            max_queue_size => 2048
        },
        {ok, State} = erlmcp_otel_datadog:init(Config),
        ?assertMatch(#{config := _, queue := _, timer := _}, State)
    end).

%% Test 2: Init missing endpoint
init_missing_endpoint_test() ->
    ?_test(begin
        Config = #{env => <<"test">>},  % Missing endpoint
        Result = erlmcp_otel_datadog:init(Config),
        ?assertMatch({error, missing_endpoint}, Result)
    end).

%% Test 4: Add Datadog tags
add_datadog_tags_test() ->
    ?_test(begin
        Config = #{
            env => <<"production">>,
            service => <<"erlmcp">>,
            version => <<"2.0.0">>
        },
        Span = #{attributes => #{<<"original.attr">> => <<"value">>}},
        TaggedSpan = erlmcp_otel_datadog:add_datadog_tags(Span, Config),
        #{attributes := Attributes} = TaggedSpan,
        ?assertEqual(<<"production">>, maps:get(<<"env">>, Attributes)),
        ?assertEqual(<<"erlmcp">>, maps:get(<<"service">>, Attributes)),
        ?assertEqual(<<"2.0.0">>, maps:get(<<"version">>, Attributes))
    end).

%% Test 7 & 8: Send batch success (200 and 202)
send_batch_success_200_test() ->
    ?_test(begin
        Config = #{
            endpoint => <<"http://localhost:4318/v1/traces">>
        },
        Spans = [create_test_span()],
        %% Mock server returns 200
        Result = erlmcp_otel_datadog:send_batch(Spans, Config),
        ?assertEqual(ok, Result)
    end).

send_batch_success_202_test() ->
    ?_test(begin
        Config = #{
            endpoint => <<"http://localhost:4318/v1/traces">>
        },
        Spans = [create_test_span()],
        %% Mock server returns 202 (Datadog Agent behavior)
        Result = erlmcp_otel_datadog:send_batch(Spans, Config),
        ?assertEqual(ok, Result)
    end).

%% Test 9: API key header
send_batch_api_key_header_test() ->
    ?_test(begin
        Config = #{
            endpoint => <<"http://localhost:4318/v1/traces">>,
            api_key => <<"test-api-key">>
        },
        Spans = [create_test_span()],
        %% Mock server verifies DD-API-KEY header present
        Result = erlmcp_otel_datadog:send_batch(Spans, Config),
        ?assertEqual(ok, Result)
    end).

%% [Other tests similar to Jaeger]
```

#### Architecture
Same as Phase 1 - functional module, no supervision tree.

**Dependencies:**
- **Internal:** `erlmcp_otel_datadog` (module under test)
- **External:** `httpc`, `jsx`, `queue`, `maps`, `lists`
- **Test helpers:** Same mock HTTP server from Phase 1

#### Changes Required:

##### 1. Create Datadog Test File
**File:** `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_datadog_tests.erl`
**Current:** Does not exist
**Changes:** Create new test file with 10 test generators covering all public functions, Datadog tag injection, and error paths

**Reason:** Achieve ≥80% coverage for erlmcp_otel_datadog module

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `cd /Users/sac/erlmcp/apps/erlmcp_observability && TERM=dumb rebar3 compile --verbose | grep -E "(error|warning)"` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_otel_datadog_tests` - 100% pass rate (all 10 tests pass)
- [ ] Coverage: `rebar3 cover --verbose` - erlmcp_otel_datadog.erl ≥80% coverage
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings for erlmcp_otel_datadog module
- [ ] Xref: `rebar3 xref` - 0 undefined function calls for erlmcp_otel_datadog

##### Manual Verification:
- [ ] Code review: OTP patterns followed correctly
- [ ] Datadog-specific features tested (tag injection, 202 response, API key header)
- [ ] Edge cases: All error paths tested
- [ ] Protocol compliance: Datadog tag format verified

---

### Phase 3: Honeycomb Exporter Test Suite (5 hours)

#### Overview
Create comprehensive EUnit test suite for `erlmcp_otel_honeycomb` module covering initialization, sampling logic, Honeycomb metadata injection, span export, batch encoding, HTTP transport, error handling, and shutdown. Tests will verify Honeycomb-specific protocol requirements (dataset routing, X-Honeycomb headers, deterministic sampling).

#### Specification
**Test file:** `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_honeycomb_tests.erl`

**Test generators:**
1. `init_success_test_()` - Verify valid config (endpoint + api_key) creates state
2. `init_missing_api_key_test_()` - Verify config without api_key returns error
3. `init_missing_endpoint_test_()` - Verify config without endpoint returns error
4. `export_spans_queue_test_()` - Verify spans queued and sampled
5. `should_sample_always_test_()` - Verify sample_rate=1 samples all spans
6. `should_sample_deterministic_test_()` - Verify deterministic sampling based on trace ID MD5
7. `format_span_test_()` - Verify Honeycomb event format (duration_ms, timestamp ISO 8601)
8. `add_honeycomb_metadata_test_()` - Verify dataset, environment, hostname, beamtime added
9. `send_batch_headers_test_()` - Verify X-Honeycomb-Team and X-Honeycomb-Dataset headers
10. `send_batch_url_test_()` - Verify URL construction: `/1/batch/{dataset}`
11. `shutdown_flush_test_()` - Verify shutdown cancels timer and flushes remaining spans

**Coverage target:** ≥80% of `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl`

#### Pseudocode

**Test file structure:**
```erlang
-module(erlmcp_otel_honeycomb_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Test fixtures
honeycomb_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_Ctx) -> [
            init_success_test(),
            init_missing_api_key_test(),
            init_missing_endpoint_test(),
            export_spans_queue_test(),
            should_sample_always_test(),
            should_sample_deterministic_test(),
            format_span_test(),
            add_honeycomb_metadata_test(),
            send_batch_headers_test(),
            send_batch_url_test(),
            shutdown_flush_test()
        ]end
    }.

setup() ->
    application:ensure_all_started(inets),
    {ok, MockServerPid} = start_mock_http_server(8080),
    MockServerPid.

cleanup(MockServerPid) ->
    stop_mock_http_server(MockServerPid),
    application:stop(inets),
    ok.

%% Test 1: Init success
init_success_test() ->
    ?_test(begin
        Config = #{
            endpoint => <<"https://api.honeycomb.io">>,
            api_key => <<"test-api-key">>,
            dataset => <<"erlmcp-traces">>,
            sample_rate => 10,
            batch_timeout => 5000,
            max_queue_size => 2048
        },
        {ok, State} = erlmcp_otel_honeycomb:init(Config),
        ?assertMatch(#{config := _, queue := _, timer := _}, State)
    end).

%% Test 2 & 3: Init validation errors
init_missing_api_key_test() ->
    ?_test(begin
        Config = #{
            endpoint => <<"https://api.honeycomb.io">>
            %% Missing api_key
        },
        Result = erlmcp_otel_honeycomb:init(Config),
        ?assertMatch({error, missing_required_fields}, Result)
    end).

init_missing_endpoint_test() ->
    ?_test(begin
        Config = #{
            api_key => <<"test-api-key">>
            %% Missing endpoint
        },
        Result = erlmcp_otel_honeycomb:init(Config),
        ?assertMatch({error, missing_required_fields}, Result)
    end).

%% Test 5: Should sample always (rate=1)
should_sample_always_test() ->
    ?_test(begin
        Span = create_test_span(1),
        ?assert(erlmcp_otel_honeycomb:should_sample(Span, 1))  % Always sample
    end).

%% Test 6: Should sample deterministic
should_sample_deterministic_test() ->
    ?_test(begin
        %% Use fixed trace ID to verify deterministic behavior
        TraceId = <<"00000000000000000000000000000001">>,
        Span = #{trace_id => TraceId},
        %% Calculate expected MD5 hash: <<Hash:64, _/binary>> = crypto:hash(md5, TraceId)
        %% (Hash rem SampleRate) =:= 0 determines if sampled
        %% Verify that same trace ID always gives same result
        Result1 = erlmcp_otel_honeycomb:should_sample(Span, 10),
        Result2 = erlmcp_otel_honeycomb:should_sample(Span, 10),
        ?assertEqual(Result1, Result2)  % Deterministic
    end).

%% Test 7: Format span
format_span_test() ->
    ?_test(begin
        Span = create_test_span(1),
        Formatted = erlmcp_otel_honeycomb:format_span(Span),
        ?assertMatch(#{
            <<"trace.trace_id">> := _,
            <<"name">> := _,
            <<"timestamp">> := _,
            <<"duration_ms">> := _
        }, Formatted),
        %% Verify timestamp is ISO 8601 format
        #{<<"timestamp">> := Timestamp} = Formatted,
        ?assertMatch(<<_,_,_,_,"-",_,_,"-",_,_,"T",_,_":",_,_":",_,".",_,"Z">>, Timestamp)
    end).

%% Test 8: Add Honeycomb metadata
add_honeycomb_metadata_test() ->
    ?_test(begin
        Config = #{
            dataset => <<"erlmcp-traces">>,
            environment => <<"production">>
        },
        Span = #{attributes => #{<<"original">> => <<"value">>}},
        EnrichedSpan = erlmcp_otel_honeycomb:add_honeycomb_metadata(Span, Config),
        #{attributes := Attributes} = EnrichedSpan,
        ?assertEqual(<<"erlmcp-traces">>, maps:get(<<"meta.dataset">>, Attributes)),
        ?assertEqual(<<"production">>, maps:get(<<"meta.environment">>, Attributes)),
        ?assert(maps:is_key(<<"meta.local_hostname">>, Attributes)),
        ?assert(maps:is_key(<<"meta.beamtime">>, Attributes))
    end).

%% Test 9: Send batch headers
send_batch_headers_test() ->
    ?_test(begin
        Config = #{
            endpoint => <<"http://localhost:8080">>,
            api_key => <<"test-api-key">>,
            dataset => <<"test-dataset">>
        },
        Spans = [create_test_span()],
        %% Mock server verifies X-Honeycomb-Team and X-Honeycomb-Dataset headers
        Result = erlmcp_otel_honeycomb:send_batch(Spans, Config),
        ?assertEqual(ok, Result)
    end).

%% Test 10: Send batch URL construction
send_batch_url_test() ->
    ?_test(begin
        Config = #{
            endpoint => <<"http://localhost:8080">>,
            api_key => <<"test-api-key">>,
            dataset => <<"my-dataset">>
        },
        Spans = [create_test_span()],
        %% Mock server verifies URL: /1/batch/my-dataset
        Result = erlmcp_otel_honeycomb:send_batch(Spans, Config),
        ?assertEqual(ok, Result)
    end).

%% [Helper functions similar to Jaeger]
```

#### Architecture
Same as Phases 1 and 2 - functional module, no supervision tree.

**Dependencies:**
- **Internal:** `erlmcp_otel_honeycomb` (module under test)
- **External:** `httpc`, `jsx`, `queue`, `maps`, `lists`, `crypto` (for MD5)
- **Test helpers:** Same mock HTTP server from Phase 1

#### Changes Required:

##### 1. Create Honeycomb Test File
**File:** `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_honeycomb_tests.erl`
**Current:** Does not exist
**Changes:** Create new test file with 11 test generators covering all public functions, sampling logic, Honeycomb metadata, and error paths

**Reason:** Achieve ≥80% coverage for erlmcp_otel_honeycomb module

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `cd /Users/sac/erlmcp/apps/erlmcp_observability && TERM=dumb rebar3 compile --verbose | grep -E "(error|warning)"` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_otel_honeycomb_tests` - 100% pass rate (all 11 tests pass)
- [ ] Coverage: `rebar3 cover --verbose` - erlmcp_otel_honeycomb.erl ≥80% coverage
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings for erlmcp_otel_honeycomb module
- [ ] Xref: `rebar3 xref` - 0 undefined function calls for erlmcp_otel_honeycomb

##### Manual Verification:
- [ ] Code review: OTP patterns followed correctly
- [ ] Honeycomb-specific features tested (sampling, metadata, headers, URL)
- [ ] Edge cases: All error paths tested (missing api_key, missing endpoint)
- [ ] Protocol compliance: Honeycomb event format verified

---

## Testing Strategy

### Chicago School TDD (MANDATORY)

**Core Principles:**
- **NO MOCKS** of the system under test - Use real exporter modules
- **State-Based Verification** - Check state map contents, queue length, not return values only
- **Integration Testing** - Test with real httpc client (or real HTTP test server)
- **Real Collaborators** - Real queue operations, real JSON encoding with jsx

**What to Test:**
- All public functions: `init/1`, `export_spans/2`, `shutdown/1`
- All internal functions exported for testing: `format_span/1`, `encode_batch/1`, `send_batch/2`
- All error paths: Missing config fields, HTTP failures, invalid data
- Edge cases: Empty spans, full queue, timer cancellation

**What NOT to Mock:**
- Exporter modules (system under test)
- queue module (use real queue)
- jsx module (use real JSON encoding)
- maps, lists (use real data structures)

**What MAY be Mocked (last resort):**
- httpc (if real HTTP server is too complex) - Use meck:expect ONLY if necessary
- Timer expiration (test flush_queue/2 directly instead)

### Unit Tests (EUnit)

**Test Pattern:**
Reference: `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl:32-68`

```erlang
exporter_test_() ->
    {setup,
        fun setup/0,           % Start dependencies, mock HTTP server
        fun cleanup/1,         % Stop HTTP server, cleanup
        fun(_Ctx) ->           % Context passed to tests
            [
                ?_test(test_init_success()),
                ?_test(test_export_spans_queue()),
                ?_test(test_batch_flush()),
                ?_test(test_protocol_compliance())
            ]
        end}.
```

**Coverage Target:**
- ≥80% per exporter module
- 100% of public functions
- 100% of error paths
- All branches in case statements

**Pass Rate:** 100% (all tests must pass)

### Manual Testing Steps

**For each exporter:**

1. **Start backend locally:**
   ```bash
   # Jaeger
   docker run -p 4318:4318 -p 16686:16686 jaegertracing/all-in-one

   # Datadog Agent
   docker run -p 4318:4318 datadog/agent:latest

   # Honeycomb (requires API key)
   # No local container, use real API
   ```

2. **Run tests with real endpoint:**
   ```bash
   cd /Users/sac/erlmcp/apps/erlmcp_observability
   rebar3 eunit --module=erlmcp_otel_jaeger_tests
   ```

3. **Verify in backend UI:**
   - Jaeger: http://localhost:16686 - Search for traces
   - Datadog: Dashboard - APM/Traces
   - Honeycomb: Dataset UI - Events

4. **Check protocol compliance:**
   - Verify JSON structure matches OTLP spec
   - Verify required headers present
   - Verify span format accepted by backend

### Quality Gates

**Every phase MUST pass all gates:**

1. **Compilation:**
   ```bash
   cd /Users/sac/erlmcp/apps/erlmcp_observability
   TERM=dumb rebar3 compile --verbose | grep -E "(error|warning)"
   ```
   - Expected: 0 errors, 0 warnings
   - Fail if: Any compilation errors

2. **EUnit:**
   ```bash
   rebar3 eunit --module=erlmcp_otel_jaeger_tests
   rebar3 eunit --module=erlmcp_otel_datadog_tests
   rebar3 eunit --module=erlmcp_otel_honeycomb_tests
   ```
   - Expected: 100% pass rate (all tests pass)
   - Fail if: Any test fails

3. **Coverage:**
   ```bash
   rebar3 cover --verbose
   grep -E "erlmcp_otel_(jaeger|datadog|honeycomb)" _build/test/cover/index.html
   ```
   - Expected: ≥80% coverage per exporter module
   - Fail if: Coverage <80%

4. **Dialyzer:**
   ```bash
   rebar3 dialyzer
   ```
   - Expected: 0 warnings
   - Fail if: Any type spec warnings

5. **Xref:**
   ```bash
   rebar3 xref
   ```
   - Expected: 0 undefined function calls
   - Fail if: Any undefined functions

**Stop the line (Jidoka):**
- If ANY gate fails, STOP and fix before continuing
- No "skip this gate for now"
- No "good enough, ship it"
- Zero defects means zero defects

## Manufacturing Checklist

### Before Implementation
- [x] Research verified (read actual source code at lines 77-205, 68-232, 67-252)
- [x] Scope confirmed (IN: 3 test files, OUT: exporter refactoring, integration tests, performance benchmarking)
- [x] No open questions (all decisions made: HTTP mock strategy, test patterns, coverage targets)
- [x] Phases broken down (Phase 1: 6h, Phase 2: 5h, Phase 3: 5h - all ≤4 hours)
- [x] Acceptance criteria defined (measurable, specific, with file paths and commands)

### During Implementation
- [ ] Chicago School TDD followed (tests FIRST, then implementation)
- [ ] OTP patterns followed (functional modules, state maps, no gen_servers)
- [ ] Type specs verified (all public functions have -spec)
- [ ] Error handling complete (all paths tested: missing config, HTTP failures, queue full)
- [ ] Quality gates passing (compilation, tests, coverage, dialyzer, xref)

### After Implementation
- [ ] All tests passing (100% rate, 30+ tests across 3 files)
- [ ] Coverage ≥80% (verified with rebar3 cover)
- [ ] Dialyzer 0 warnings (verified with rebar3 dialyzer)
- [ ] Xref 0 undefined calls (verified with rebar3 xref)
- [ ] Performance no regression >10ms (measured with timer:tc/3)
- [ ] Documentation updated (test file module docs, comments)
- [ ] Code review complete (OTP patterns, Chicago School TDD verified)

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **HTTP backend dependency in tests** | P1 (High) | High | Start mock HTTP server in test setup using ranch or inets httpd; tests fail gracefully if backend unavailable |
| **Protocol compliance validation** | P0 (Critical) | Medium | Validate generated JSON against OTLP schema; use real backends for manual verification; verify structure with jsx:decode in tests |
| **Queue/timer race conditions** | P1 (High) | Medium | Avoid testing actual timer expiration; test flush_queue/2 directly; do NOT use meck for erlang:send_after |
| **Performance regression** | P2 (Medium) | Low | Measure baseline with timer:tc/3; add performance test that fails if export latency >10ms; track metrics |
| **State map mutation** | P1 (High) | Low | Verify state immutability in tests; assert that export_spans/2 returns new state map; use maps:get/2 to validate fields |
| **Sampling determinism (Honeycomb)** | P2 (Medium) | Low | Use fixed trace IDs in tests; verify should_sample/2 with known MD5 hash values; test deterministic behavior |
| **Coverage <80%** | P0 (Critical) | Medium | Use rebar3 cover --verbose to identify uncovered lines; add tests for error paths; test all branches in case statements |
| **httpc mock complexity** | P2 (Medium) | Low | Start real HTTP test server (ranch/inets); use meck ONLY as last resort; prefer real collaborators |

### Rollback Plan

**If something goes wrong:**

1. **Git revert:**
   ```bash
   cd /Users/sac/erlmcp
   git diff HEAD  # Review changes
   git checkout -- apps/erlmcp_observability/test/erlmcp_otel_*_tests.erl
   ```
   - Revert to commit before test file creation

2. **Data migration:** Not applicable (test files only, no data migration)

3. **Service impact:** No impact (tests are local, not deployed to production)

**Recovery:**
- Delete test files if they have issues
- Fix bugs found during testing (keep fixes minimal)
- Re-run quality gates after fixes

## References

- Research: `/Users/sac/erlmcp/.wreckit/items/013-create-eunit-test-suites-for-all-otel-exporters-ja/research.md`
- Jaeger exporter: `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_jaeger.erl`
- Datadog exporter: `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_datadog.erl`
- Honeycomb exporter: `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_otel_honeycomb.erl`
- Test reference: `/Users/sac/erlmcp/apps/erlmcp_observability/test/erlmcp_otel_enhanced_tests.erl:32-68`
- Test pattern: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_tests.erl:14-24`
- CLAUDE.md: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- TCPS: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- OTP Patterns: `/Users/sac/erlmcp/docs/otp-patterns.md`
- OpenTelemetry OTLP spec: https://opentelemetry.io/docs/reference/specification/protocol/otlp/

---

**End of Manufacturing Plan**

**Next Step:** Execute Phase 1 (Jaeger Exporter Test Suite) following the pseudocode and acceptance criteria defined above. Complete ALL quality gates before proceeding to Phase 2.

**Remember:** This is manufacturing. We ship ZERO DEFECTS. Plan thoroughly, measure everything, document every step.
