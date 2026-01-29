# Create EUnit test suite for HTTP transport (erlmcp_transport_http) Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Create a comprehensive EUnit test suite for the HTTP transport module (erlmcp_transport_http and erlmcp_transport_http_server) that achieves ≥80% code coverage and validates all Gun client integration points. The HTTP transport is critical for web-based MCP servers and browser integrations, using the Gun HTTP client library for HTTP/1.1 and HTTP/2 connections.

### Quality Gate Requirements

- **Compilation**: 0 errors (MANDATORY)
- **EUnit**: 100% pass rate (MANDATORY)
- **Common Test**: Not applicable (EUnit only for unit tests)
- **Coverage**: ≥80% (MANDATORY)
- **Dialyzer**: 0 warnings (MANDATORY)
- **Xref**: 0 undefined function calls (MANDATORY)
- **Performance**: <10% regression from baseline (baseline: 43K msg/sec network I/O from CLAUDE.md line 162)

## Current State

### What Exists Now

**Modules:**
- `apps/erlmcp_transports/src/erlmcp_transport_http.erl` (53 lines) - Public API wrapper module
- `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl` (635 lines) - gen_server implementation
- `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl` (168 lines) - Stub test file

**Current Implementation:**
- **Behavior**: gen_server with process-per-connection pattern (lines 1-8 in http_server.erl)
- **State Record**: #state{} with 13 fields (lines 13-34)
- **Gun Integration**: gun:open/2, gun:await_up/2, gun:post/4, gun:get/3 (lines 370-516)
- **URL Parsing**: parse_url/1, parse_host_port/2, normalize_url/1 (lines 256-315)
- **Header Normalization**: normalize_headers/1, merge_headers/2 (lines 317-350)
- **Connection Management**: connect/1, build_gun_opts/1 (lines 356-427)
- **Retry Logic**: Exponential backoff with should_retry/3 (lines 603-634)
- **TLS Support**: HTTPS with certificate validation (lines 398-442)

**Tests:**
- **Coverage**: 0% - test file exists but contains only placeholder tests
- **Test Count**: 6 stub tests (lines 34-98) that only test option map structure
- **Integration Tests**: All commented out (lines 107-168) - require mock HTTP server
- **Quality Gap**: 80 percentage points below target (0% vs 80% target)

### What's Missing

**Gap:** 80 percentage points coverage deficiency (0% actual vs 80% target)

**Root Cause:** (from 5 Whys analysis)
1. Tests exist but are placeholders testing only option map structure
2. No mock HTTP server implemented for Gun client integration
3. Gun client integration requires real network I/O (complex, slow)
4. Development deferred integration testing in favor of unit tests
5. **Poka-yoke failure**: No automated quality gate prevents deployment of untested network code

**Impact:**
- **BLOCKS PRODUCTION DEPLOYMENT**: Untested network code will fail in real-world conditions
- **High Risk**: Network failures, TLS errors, HTTP protocol violations not tested
- **Jidoka Violation**: Defects not stopped at source - no built-in quality

### Key Discoveries from Research

1. **Test File Structure**: File exists at `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl` with 168 lines but contains only stub tests (verified by reading lines 34-98)

2. **Gun Client Integration**: HTTP transport uses Gun 2.0.1 (confirmed in rebar.config line 48) - must test gun:open/2, gun:await_up/2, gun:post/4, gun:get/3, gun:close/1

3. **TLS Validation Dependency**: erlmcp_transport_http_server.erl line 403 calls `erlmcp_tls_validation:build_tls_options/2` but this module does NOT exist (glob search returned no files) - must skip HTTPS tests or create stub

4. **Chicago School TDD Pattern**: TCP transport tests (lines 76-117, 258-324) demonstrate the pattern:
   - Real processes, no mocks
   - State inspection via gen_server:call(Pid, get_state)
   - Setup/cleanup fixtures for process lifecycle
   - Random port allocation (port => 0) to avoid conflicts
   - Timeout declarations for network tests ({timeout, 15, fun() -> ... end})

5. **Mock HTTP Server Available**: Cowboy 2.10.0 is in dependencies (rebar.config line 52) - can be used for integration tests

## Desired End State

### Specification

**Test File:** `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl`

**Test Coverage Goals:**
- **erlmcp_transport_http.erl**: 100% coverage (only 53 lines, simple wrapper)
- **erlmcp_transport_http_server.erl**: ≥80% coverage (635 lines, complex Gun integration)

**Test Categories (Chicago School TDD):**

1. **Unit Tests (No Network):**
   - URL parsing: parse_url/1, parse_host_port/2, normalize_url/1 (lines 287-315)
   - Header normalization: normalize_headers/1, merge_headers/2, to_binary/1 (lines 317-350)
   - State initialization: build_initial_state/1 (lines 256-279)
   - Retry delay calculation: calculate_retry_delay/2 (lines 628-634)

2. **Integration Tests (Gun Client):**
   - Connection establishment: connect/1, gun:open/2, gun:await_up/2 (lines 356-396)
   - HTTP POST requests: perform_request/2, gun:post/4 (lines 499-516)
   - HTTP GET requests: gun:get/3 (lines 500-510)
   - Response handling: handle_gun_response/5 (lines 518-548)
   - Gun error handling: handle_gun_error/3 (lines 550-571)

3. **Error Scenario Tests:**
   - Connection refused (no server listening)
   - Connection timeout (gun:await_up/2 timeout)
   - HTTP error status codes (4xx, 5xx)
   - Gun process death and reconnection (lines 205-214)
   - Owner process death (lines 199-202)

4. **Advanced Feature Tests:**
   - Connection pool (pool_size parameter, lines 454-469)
   - Keep-alive connection reuse
   - Exponential backoff retry logic (lines 603-634)
   - HTTP/2 protocol negotiation (gun:open protocols option)

### Verification

**Automated Quality Gates:**
```bash
# Compilation - 0 errors
TERM=dumb rebar3 compile

# EUnit - 100% pass rate
rebar3 eunit --module=erlmcp_transport_http_tests --verbose

# Coverage - ≥80%
rebar3 cover --verbose
# Then open: _build/test/cover/index.html
# Verify: erlmcp_transport_http_server.erl ≥80%

# Dialyzer - 0 warnings
rebar3 dialyzer

# Xref - 0 undefined function calls
rebar3 xref
```

**Manual Verification:**
- Inspect coverage report line-by-line for erlmcp_transport_http_server.erl
- Verify all error paths are tested (gun_error, connection failures, timeouts)
- Verify all public functions have tests
- Verify OTP patterns followed correctly (gen_server, supervision)

**Metrics:**
- **Test Count**: Target 25-35 tests (TCP has 700 lines with ~30 tests)
- **Coverage**: ≥80% lines executed
- **Test Execution Time**: <30 seconds (TCP integration tests are 15-20s)
- **Pass Rate**: 100% (0 skipped tests)

### Manufacturing Output

**Code:**
- Modified: `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl` (replace stub tests with comprehensive test suite)

**Tests:**
- Created: Complete EUnit test suite with 25-35 test functions
- Test Groups: URL parsing tests, Gun integration tests, error handling tests, retry logic tests

**Documentation:**
- Receipts: Coverage report (`_build/test/cover/index.html`)
- Test execution log showing 100% pass rate

## What We're NOT Doing

**Explicitly OUT OF SCOPE:**

1. **Modifying HTTP transport implementation** - Only writing tests, NOT changing src/ files
   - **Reason**: Scope is test suite creation only (success criterion line 83: "Test file created")

2. **Creating production HTTP server** - Only mock HTTP server for testing
   - **Reason**: Testing focus, not production feature development

3. **Performance benchmarking** - Only functional testing, not load testing
   - **Reason**: Performance is separate concern (success criterion doesn't mention performance)

4. **TLS certificate generation** - Skip HTTPS tests if erlmcp_tls_validation module missing
   - **Reason**: TLS validation module doesn't exist (verified by glob search), creating it is out of scope

5. **Common Test suite** - EUnit only, no CT suites
   - **Reason**: Success criterion specifies EUnit test suite (line 83)

6. **Property-based testing** - No QuickCheck/PropEr tests
   - **Reason**: Success criterion specifies EUnit only

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** ✅ (Complete)
   - Requirements documented with acceptance criteria
   - Test categories defined (unit, integration, error, advanced)
   - Quality gates identified (compile, EUnit, coverage, dialyzer, xref)

2. **Pseudocode** (Phase 1-4)
   - Algorithm design BEFORE coding
   - Test fixture structure documented
   - Mock HTTP server protocol defined

3. **Architecture** (Verified)
   - Integration points: Gun client, Cowboy mock server, gen_server
   - Supervision: gen_server under erlmcp_transport_sup
   - Process structure: One gen_server per HTTP connection

4. **Refinement** (Implementation)
   - Chicago School TDD (tests FIRST)
   - Real Gun processes (no mocks)
   - Real HTTP server (cowboy)
   - Real network I/O (localhost connections)

5. **Completion** (Quality Gates)
   - All automated gates passing (compile, test, coverage, dialyzer, xref)
   - Manual verification complete (coverage report inspection)

### Implementation Strategy

**Why This Strategy?**

The HTTP transport is a gen_server wrapper around the Gun HTTP client library. Following TCPS Jidoka principle (built-in quality), we must test REAL Gun processes and REAL HTTP connections because:

1. **Gun Client Complexity**: 635 lines of code handle connection pooling, retry logic, HTTP protocol, TLS
2. **Failure Modes**: Network failures, timeouts, TLS errors, HTTP errors cannot be mocked
3. **Reference Pattern**: TCP transport tests achieve comprehensive coverage with real processes

**Four-Phase Approach (Heijunka - Production Leveling):**

**Phase 1: Unit Tests (No Network) - ≤2 hours**
- URL parsing functions (no network needed)
- Header normalization (no network needed)
- State initialization (no network needed)
- Retry delay calculation (no network needed)
- **Quality Gate**: All unit tests passing, coverage ~20%

**Phase 2: Gun Integration Tests - ≤3 hours**
- Gun connection establishment (real Gun process)
- HTTP POST/GET requests (real HTTP protocol)
- Response handling (real Gun messages)
- Mock HTTP server (cowboy on random port)
- **Quality Gate**: All integration tests passing, coverage ~60%

**Phase 3: Error Scenario Tests - ≤2 hours**
- Connection refused (no server listening)
- Connection timeout (short timeout)
- HTTP error codes (4xx, 5xx from mock server)
- Gun process death (kill Gun pid)
- Owner process death (kill owner)
- **Quality Gate**: All error tests passing, coverage ~75%

**Phase 4: Advanced Features & Coverage Polish - ≤2 hours**
- Connection pool (multiple concurrent requests)
- Exponential backoff (short delays)
- Keep-alive connection reuse
- Add tests until coverage ≥80%
- **Quality Gate**: Coverage ≥80%, all tests passing

**Total Estimated Time**: 9 hours (within single workday)

### Quality Integration

**Pre-commit Hooks:**
- `.claude/hooks/pre-task-validate.sh` - Runs compilation, EUnit, coverage check
- Fails if coverage <80% or any test fails

**CI Gates:**
- GitHub Actions workflow: compile → EUnit → coverage → dialyzer → xref
- Must pass ALL gates before merge

**Receipt Generation:**
- Coverage report: `_build/test/cover/index.html`
- Test execution log: `_build/test/logs/eunit.log`
- Dialyzer report: `_build/test/dialyzer/results`

**Andon Signaling (Visual Management):**
- Test progress visible in console output
- Failures signaled immediately (test fails = stop the line)
- Coverage percentage displayed after each test run

---

## Phases

### Phase 1: Unit Tests (No Network)

#### Overview

Create unit tests for pure functions that don't require network I/O. This establishes test infrastructure and achieves ~20% coverage.

#### Specification

**Test File**: `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl`

**Test Categories:**
1. **URL Parsing Tests**: parse_url/1, parse_host_port/2, normalize_url/1
2. **Header Normalization Tests**: normalize_headers/1, merge_headers/2, to_binary/1
3. **State Initialization Tests**: build_initial_state/1
4. **Retry Delay Calculation Tests**: calculate_retry_delay/2

#### Pseudocode

```erlang
%% Test fixture setup
setup() ->
    application:ensure_all_started(gun),
    application:ensure_all_started(ssl),
    ok.

cleanup(_) ->
    ok.

%% URL parsing tests
parse_http_url_test() ->
    URL = "http://localhost:8080/mcp",
    {Scheme, Host, Port, Path} = parse_url(URL),
    ?assertEqual(http, Scheme),
    ?assertEqual("localhost", Host),
    ?assertEqual(8080, Port),
    ?assertEqual("/mcp", Path).

parse_https_url_test() ->
    URL = "https://api.example.com/v1/mcp",
    {Scheme, Host, Port, Path} = parse_url(URL),
    ?assertEqual(https, Scheme),
    ?assertEqual("api.example.com", Host),
    ?assertEqual(443, Port),
    ?assertEqual("/v1/mcp", Path).

parse_url_default_port_test() ->
    URL = "http://example.com/api",
    {Scheme, Host, Port, Path} = parse_url(URL),
    ?assertEqual(http, Scheme),
    ?assertEqual("example.com", Host),
    ?assertEqual(80, Port),  % Default HTTP port
    ?assertEqual("/api", Path).

parse_url_with_path_test() ->
    URL = "http://localhost:8080/api/v2/endpoint",
    {Scheme, Host, Port, Path} = parse_url(URL),
    ?assertEqual("/api/v2/endpoint", Path).

normalize_headers_test() ->
    Headers = [{"Content-Type", "application/json"}, {<<"Accept">>, <<"text/plain">>}],
    Normalized = normalize_headers(Headers),
    ?assertEqual(true, is_list(Normalized)),
    ?assertEqual(4, length(Normalized)),  % 2 user + 2 default
    ?assertEqual({<<"content-type">>, <<"application/json">>},
                 lists:keyfind(<<"content-type">>, 1, Normalized)).

merge_headers_test() ->
    Defaults = [{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}],
    User = [{<<"b">>, <<"3">>}, {<<"c">>, <<"4">>}],
    Merged = merge_headers(Defaults, User),
    ?assertEqual(3, length(Merged)),
    ?assertEqual({<<"b">>, <<"3">>}, lists:keyfind(<<"b">>, 1, Merged)),
    ?assertEqual({<<"c">>, <<"4">>}, lists:keyfind(<<"c">>, 1, Merged)).

to_binary_test() ->
    ?assertEqual(<<"hello">>, to_binary("hello")),
    ?assertEqual(<<"hello">>, to_binary(<<"hello">>)),
    ?assertEqual(<<"hello">>, to_binary(hello)),
    ?assertEqual(<<"123">>, to_binary(123)).

retry_delay_calculation_test() ->
    %% Exponential backoff: BaseDelay * (2 ^ (Attempts - 1))
    State = #state{retry_delay = 1000},
    ?assertEqual(1000 + _Jitter, calculate_retry_delay(1, State)),
    ?assertEqual(2000 + _Jitter, calculate_retry_delay(2, State)),
    ?assertEqual(4000 + _Jitter, calculate_retry_delay(3, State)).
```

#### Architecture

**Integration Points:**
- **Gun Application**: Started in setup/0 (line 23 in existing test file)
- **SSL Application**: Started in setup/0 for HTTPS support
- **No Network**: All tests are pure functions, no Gun processes created

**Dependencies:**
- **Internal**: erlmcp_transport_http_server (for #state{} record)
- **External**: gun (application start only), ssl (application start only)

#### Changes Required:

##### 1. Test File Structure

**File**: `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl`
**Current**: Lines 1-168 (stub tests)
**Changes**: Replace with comprehensive test structure

```erlang
-module(erlmcp_transport_http_tests).
-include_lib("eunit/include/eunit.hrl").

%% Include state record definition
-include("src/erlmcp_transport_http_server.hrl").  % Or inline the record

%%====================================================================
%% Test Fixtures
%%====================================================================

http_transport_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"URL Parsing", fun test_url_parsing/0},
      {"Header Normalization", fun test_header_normalization/0},
      {"State Initialization", fun test_state_initialization/0},
      {"Retry Delay Calculation", fun test_retry_delay/0}
     ]}.

setup() ->
    application:ensure_all_started(gun),
    application:ensure_all_started(ssl),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Unit Tests (No Network)
%%====================================================================

%% URL parsing tests
parse_http_url_test() ->
    %% Test code here
    ok.

%% (Add all unit tests from pseudocode)
```

**Reason**: Establish test infrastructure and achieve baseline coverage without network complexity.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **EUnit**: `rebar3 eunit --module=erlmcp_transport_http_tests` - 100% pass rate
- [ ] **Coverage**: `rebar3 cover` - ≥20% coverage (baseline for unit tests)
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: OTP patterns followed correctly
- [ ] Test structure: Matches Chicago School TDD pattern
- [ ] Unit tests: All pure functions tested

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix. This is Jidoka (built-in quality).

---

### Phase 2: Gun Integration Tests

#### Overview

Create integration tests with real Gun client and real HTTP server (cowboy). Tests HTTP POST/GET requests, response handling, and connection lifecycle.

#### Specification

**Test Categories:**
1. **Gun Connection Tests**: gun:open/2, gun:await_up/2
2. **HTTP POST Tests**: gun:post/4, JSON body sending
3. **HTTP GET Tests**: gun:get/3, query parameters
4. **Response Handling Tests**: gun_response, gun_data messages
5. **Mock HTTP Server**: Cowboy on random port

#### Pseudocode

```erlang
%% Setup mock HTTP server with cowboy
setup_http_server() ->
    %% Start cowboy on random port
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/mcp", mcp_handler, []},
            {"/api", api_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(mock_http, [{port, 0}], #{
        env => #{dispatch => Dispatch}
    }),
    %% Get assigned port
    Port = ranch:get_port(mock_http),
    URL = "http://localhost:" ++ integer_to_list(Port) ++ "/mcp",
    {Pid, Port, URL}.

cleanup_http_server({Pid, Port, _URL}) ->
    cowboy:stop_listener(mock_http).

%% Gun connection test
gun_connection_test() ->
    {_, _, URL} = setup_http_server(),
    Opts = #{url => URL, owner => self()},
    {ok, Pid} = erlmcp_transport_http:init(Opts),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    {ok, State} = gen_server:call(Pid, get_state),
    ?assertEqual(undefined, State#state.gun_pid),  % Not connected yet
    cleanup_http_server(...).

%% HTTP POST request test
http_post_request_test() ->
    {_, _, URL} = setup_http_server(),
    Opts = #{url => URL, owner => self(), method => post},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Wait for gun_up
    receive {gun_up, _Pid, _Protocol} -> ok after 5000 -> error(timeout) end,

    %% Send POST request
    Request = jsx:encode(#{method => <<"test">>, params => #{}}),
    ok = erlmcp_transport_http:send(Pid, Request),

    %% Verify response
    receive {transport_message, Response} ->
        ?assertEqual(#{jsonrpc => <<"2.0">>, result => ok}, jsx:decode(Response))
    after 5000 ->
        error(timeout)
    end,

    erlmcp_transport_http:close(Pid),
    cleanup_http_server(...).

%% HTTP GET request test
http_get_request_test() ->
    %% Similar to POST but using GET method
    ok.
```

#### Architecture

**Integration Points:**
- **Gun Client**: Real Gun process (gun:open/2)
- **Cowboy Server**: Mock HTTP server on random port
- **gen_server**: erlmcp_transport_http_server process
- **Protocol**: Real HTTP/1.1 and HTTP/2

**Process Structure:**
```
Test Process
    |
    +-- Cowboy Server (mock HTTP)
    |
    +-- erlmcp_transport_http_server (gen_server)
            |
            +-- Gun Client (HTTP client)
```

#### Changes Required:

##### 1. Mock HTTP Server Handler

**File**: Create temporary handler in test file or use cowboy loop handler
**Current**: Does not exist
**Changes**: Add simple cowboy handler for testing

```erlang
%% Simple cowboy handler that echoes JSON
init(Req, State) ->
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),

    {ok, Req2} = case {Method, HasBody} of
        {<<"POST">>, true} ->
            {ok, Body, Req1} = cowboy_req:read_body(Req),
            %% Echo back the body
            Response = jsx:encode(#{jsonrpc => <<"2.0">>, result => ok}),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1);
        {<<"GET">>, false} ->
            Response = jsx:encode(#{jsonrpc => <<"2.0">>, result => get_ok}),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req)
    end,
    {ok, Req2, State}.
```

**Reason**: Provides real HTTP endpoint for Gun client integration testing.

##### 2. Gun Integration Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl`
**Current**: Only unit tests from Phase 1
**Changes**: Add integration test group

```erlang
gun_integration_test_() ->
    {setup,
     fun setup_http_server/0,
     fun cleanup_http_server/1,
     [
      {"Gun connection establishment", fun test_gun_connection/0},
      {"HTTP POST request", fun test_http_post/0},
      {"HTTP GET request", fun test_http_get/0},
      {"Response handling", fun test_response_handling/0}
     ]}.

setup_http_server() ->
    %% Start cowboy, return {Pid, Port, URL}
    ok.

%% (Add integration tests from pseudocode)
```

**Reason**: Test real Gun client behavior with real HTTP protocol.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **EUnit**: `rebar3 eunit --module=erlmcp_transport_http_tests` - 100% pass rate
- [ ] **Coverage**: `rebar3 cover` - ≥60% coverage (unit + integration)
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Integration: Gun client connects successfully
- [ ] Protocol: Real HTTP POST/GET requests working
- [ ] Async: Response messages delivered to owner process

**Note**: Integration tests use real network I/O and may take 5-10 seconds.

---

### Phase 3: Error Scenario Tests

#### Overview

Test error handling paths: connection failures, timeouts, HTTP errors, process crashes. Critical for Poka-yoke (mistake-proofing).

#### Specification

**Test Categories:**
1. **Connection Refused**: No server listening on target port
2. **Connection Timeout**: gun:await_up/2 timeout
3. **HTTP Error Codes**: 4xx, 5xx from mock server
4. **Gun Process Death**: Kill Gun pid, verify reconnection
5. **Owner Process Death**: Kill owner, verify gen_server terminates

#### Pseudocode

```erlang
%% Connection refused test
connection_refused_test() ->
    %% Try to connect to port that's not listening
    URL = "http://localhost:9999/no-server",
    Opts = #{url => URL, owner => self(), connect_timeout => 1000},
    case erlmcp_transport_http:init(Opts) of
        {error, {connect_failed, _}} ->
            ?assert(true);
        {ok, Pid} ->
            %% Might connect but fail on first request
            Request = <<"{}">>,
            Result = erlmcp_transport_http:send(Pid, Request),
            ?assertEqual({error, _}, Result),
            erlmcp_transport_http:close(Pid)
    end.

%% Connection timeout test
connection_timeout_test() ->
    %% Use very short timeout
    URL = "http://localhost:9999/timeout",
    Opts = #{url => URL, owner => self(), connect_timeout => 1},
    case erlmcp_transport_http:init(Opts) of
        {error, connect_timeout} ->
            ?assert(true);
        {error, {connect_failed, timeout}} ->
            ?assert(true);
        Other ->
            error({unexpected_result, Other})
    end.

%% HTTP error 500 test
http_error_500_test() ->
    {_, _, URL} = setup_http_server(),
    Opts = #{url => URL, owner => self()},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Configure server to return 500
    %% (or use /error endpoint that returns 500)
    Request = jsx:encode(#{method => <<"error">>}),
    Result = erlmcp_transport_http:send(Pid, Request),

    %% Should retry and eventually fail
    ?assertEqual({error, max_retries_exceeded}, Result),

    erlmcp_transport_http:close(Pid),
    cleanup_http_server(...).

%% Gun process death test
gun_process_death_test() ->
    {_, _, URL} = setup_http_server(),
    Opts = #{url => URL, owner => self()},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Wait for connection
    {ok, State} = gen_server:call(Pid, get_state),
    GunPid = State#state.gun_pid,
    receive {gun_up, GunPid, _} -> ok after 5000 -> error(timeout) end,

    %% Kill Gun process
    exit(GunPid, kill),

    %% Verify reconnection attempt
    timer:sleep(100),
    {ok, NewState} = gen_server:call(Pid, get_state),
    ?assertEqual(undefined, NewState#state.gun_pid),  % Should be reconnecting

    erlmcp_transport_http:close(Pid),
    cleanup_http_server(...).

%% Owner process death test
owner_death_test() ->
    {_, _, URL} = setup_http_server(),
    Owner = spawn_link(fun() -> receive stop -> ok end end),
    Opts = #{url => URL, owner => Owner},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Kill owner
    exit(Owner, kill),

    %% Verify gen_server terminates
    timer:sleep(100),
    ?assertEqual(false, erlang:is_process_alive(Pid)),

    cleanup_http_server(...).
```

#### Architecture

**Error Injection:**
- **No Server**: Try to connect to non-listening port
- **Short Timeout**: Use 1ms connect_timeout to trigger timeout
- **Mock Server Errors**: Configure cowboy handler to return 500, 429
- **Process Kill**: exit(Pid, kill) to simulate crashes

**Recovery Verification:**
- **Reconnection**: Gun process death should trigger reconnect
- **Retry Logic**: HTTP 500 should trigger retry with exponential backoff
- **Cleanup**: Owner death should terminate gen_server

#### Changes Required:

##### 1. Error Handler in Mock Server

**File**: Mock HTTP server handler
**Current**: Only success responses
**Changes**: Add error endpoints

```erlang
init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    {ok, Req2} = case {Method, Path} of
        {<<"POST">>, <<"/mcp">>} ->
            %% Success response
            Response = jsx:encode(#{jsonrpc => <<"2.0">>, result => ok}),
            cowboy_req:reply(200, #{}, Response, Req);
        {<<"POST">>, <<"/error">>} ->
            %% Return 500 error
            cowboy_req:reply(500, #{}, <<"Internal Server Error">>, Req);
        {<<"POST">>, <<"/rate-limited">>} ->
            %% Return 429 rate limit
            cowboy_req:reply(429, #{}, <<"Too Many Requests">>, Req)
    end,
    {ok, Req2, State}.
```

**Reason**: Enables testing of HTTP error handling and retry logic.

##### 2. Error Scenario Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl`
**Current**: Unit and integration tests from Phases 1-2
**Changes**: Add error scenario test group

```erlang
error_scenario_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Connection refused", fun test_connection_refused/0},
      {"Connection timeout", fun test_connection_timeout/0},
      {"HTTP 500 error", fun test_http_error_500/0},
      {"HTTP 429 rate limit", fun test_http_rate_limit/0},
      {"Gun process death", fun test_gun_death/0},
      {"Owner process death", fun test_owner_death/0}
     ]}.
```

**Reason**: Achieve Poka-yoke - all error paths tested, no untested failure modes.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **EUnit**: `rebar3 eunit --module=erlmcp_transport_http_tests` - 100% pass rate
- [ ] **Coverage**: `rebar3 cover` - ≥75% coverage (unit + integration + error)
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Error paths: All handle_* clauses tested
- [ ] Recovery: Reconnection logic verified
- [ ] Retry: Exponential backoff working

**Note**: Error tests should take <5 seconds total.

---

### Phase 4: Advanced Features & Coverage Polish

#### Overview

Test advanced features (connection pool, keep-alive, exponential backoff) and add additional tests until coverage reaches ≥80%.

#### Specification

**Test Categories:**
1. **Connection Pool**: Multiple concurrent requests (pool_size parameter)
2. **Keep-Alive**: Connection reuse across multiple requests
3. **Exponential Backoff**: Retry delay calculation verification
4. **Coverage Polish**: Add tests for uncovered lines until ≥80%

#### Pseudocode

```erlang
%% Connection pool test
connection_pool_test() ->
    {_, _, URL} = setup_http_server(),
    Opts = #{url => URL, owner => self(), pool_size => 5},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Send multiple concurrent requests
    Requests = [spawn(fun() ->
        Request = jsx:encode(#{method => <<"test">>, id => I}),
        ok = erlmcp_transport_http:send(Pid, Request)
    end) || I <- lists:seq(1, 10)],

    %% Wait for all to complete
    timer:sleep(1000),

    %% Verify pool didn't exceed limit
    {ok, State} = gen_server:call(Pid, get_state),
    ?assert(State#state.active_requests =< 5),

    erlmcp_transport_http:close(Pid),
    cleanup_http_server(...).

%% Keep-alive connection test
keep_alive_test() ->
    {_, _, URL} = setup_http_server(),
    Opts = #{url => URL, owner => self()},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Wait for connection
    {ok, State1} = gen_server:call(Pid, get_state),
    GunPid1 = State1#state.gun_pid,
    receive {gun_up, GunPid1, _} -> ok after 5000 -> error(timeout) end,

    %% Send request
    Request1 = jsx:encode(#{method => <<"test">>}),
    ok = erlmcp_transport_http:send(Pid, Request1),
    receive {transport_message, _} -> ok after 5000 -> error(timeout) end,

    %% Verify same Gun connection used
    {ok, State2} = gen_server:call(Pid, get_state),
    ?assertEqual(GunPid1, State2#state.gun_pid),

    %% Send another request
    Request2 = jsx:encode(#{method => <<"test2">>}),
    ok = erlmcp_transport_http:send(Pid, Request2),
    receive {transport_message, _} -> ok after 5000 -> error(timeout) end,

    %% Verify still same connection
    {ok, State3} = gen_server:call(Pid, get_state),
    ?assertEqual(GunPid1, State3#state.gun_pid),

    erlmcp_transport_http:close(Pid),
    cleanup_http_server(...).

%% Exponential backoff test
exponential_backoff_test() ->
    %% Mock erlang:send_after/3 to capture delays
    %% Or use short retry_delay and measure time
    Opts = #{
        url => "http://localhost:9999/retry",
        owner => self(),
        retry_delay => 100,  % Short delay for testing
        max_retries => 3
    },
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Trigger retry (server returns 500)
    Request = jsx:encode(#{method => <<"error">>}),
    StartTime = erlang:monotonic_time(millisecond),
    Result = erlmcp_transport_http:send(Pid, Request),
    EndTime = erlang:monotonic_time(millisecond),

    ?assertEqual({error, max_retries_exceeded}, Result),

    %% Verify exponential backoff: 100 + 200 + 400 = 700ms (approx)
    Elapsed = EndTime - StartTime,
    ?assert(Elapsed >= 700 andalso Elapsed < 1500),

    erlmcp_transport_http:close(Pid).
```

#### Architecture

**Advanced Features:**
- **Connection Pool**: pool_size limits concurrent requests (lines 454-469)
- **Keep-Alive**: Gun connection reused across requests (http_opts keepalive)
- **Exponential Backoff**: Delay = BaseDelay * 2^(Attempts-1) (line 631)

**Coverage Analysis:**
- Run `rebar3 cover` after Phase 3
- Identify uncovered lines in erlmcp_transport_http_server.erl
- Write tests to cover remaining lines
- Repeat until ≥80%

#### Changes Required:

##### 1. Advanced Feature Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_http_tests.erl`
**Current**: Unit, integration, error tests from Phases 1-3
**Changes**: Add advanced feature test group

```erlang
advanced_features_test_() ->
    {setup,
     fun setup_http_server/0,
     fun cleanup_http_server/1,
     [
      {"Connection pool", fun test_connection_pool/0},
      {"Keep-alive connection", fun test_keep_alive/0},
      {"Exponential backoff", fun test_exponential_backoff/0}
     ]}.
```

**Reason**: Test complex features that are critical for production use.

##### 2. Coverage Polish

**Process:**
1. Run `rebar3 cover`
2. Open `_build/test/cover/index.html`
3. Find uncovered lines in erlmcp_transport_http_server.erl
4. Write targeted tests for uncovered lines
5. Repeat until ≥80%

**Example:**
```
Uncovered: Line 343 - merge_headers/2 user map creation
Test:     test_merge_headers_user_map_priority()
```

**Reason**: Achieve ≥80% coverage target (success criterion).

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **EUnit**: `rebar3 eunit --module=erlmcp_transport_http_tests` - 100% pass rate
- [ ] **Coverage**: `rebar3 cover` - **≥80% coverage** (MANDATORY GATE)
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Coverage report: Inspect line-by-line coverage
- [ ] All error paths: Verified tested
- [ ] All public functions: Verified tested
- [ ] Edge cases: Boundary conditions tested

**Final Quality Gate**: ALL gates must pass. If coverage <80%, add more tests.

---

## Testing Strategy

### Chicago School TDD (MANDATORY)

**Core Principles:**
- **NO MOCKS** - Use real Gun processes, real Cowboy server, real network I/O
- **State-Based Verification** - Check #state{} record contents via get_state call
- **Integration Testing** - Test with real dependencies (Gun, Cowboy, gen_server)
- **Race Condition Testing** - Concurrent operations, process death scenarios

**Why This Matters:**
- Gun client has complex state machine (connecting → connected → request → response)
- Mocks wouldn't catch real-world failures (timeouts, protocol violations)
- TCP transport reference tests achieve comprehensive coverage with real processes

### Unit Tests (EUnit)

**What to Test:**
- All pure functions (URL parsing, header normalization, state building)
- All error paths (connection failures, timeouts, HTTP errors)
- All public functions (init/1, send/2, close/1)
- All gen_server callbacks (init/1, handle_call/3, handle_info/2)

**Test Pattern:** Reference `apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl:76-117`
```erlang
test_function_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(begin normal_case end),
         ?_test(begin error_case end),
         ?_test(begin edge_case end)
     ]end}.
```

**Coverage Target:** ≥80% per module (mandatory gate)

**Pass Rate:** 100% (all tests must pass, 0 skipped)

### Integration Tests (Common Test)

**Not Applicable** - EUnit only for this task (success criterion specifies EUnit test suite)

### Manual Testing Steps

1. **Inspect Coverage Report:**
   ```bash
   rebar3 cover
   open _build/test/cover/index.html
   ```
   - Verify erlmcp_transport_http_server.erl ≥80%
   - Check that all handle_info clauses are covered
   - Check that all error paths are covered

2. **Verify Test Isolation:**
   - Run tests multiple times: `for i in {1..10}; do rebar3 eunit; done`
   - Should pass 100% of the time (no race conditions)

3. **Verify Cleanup:**
   - Run tests, check for zombie processes: `erl -sname observer -hidden`
   - No Gun processes should remain after tests complete

4. **Verify Error Messages:**
   - Check that error logs are meaningful
   - Verify that failure modes are clear

### Quality Gates

**Every Phase MUST Pass:**

1. **Compilation**: `TERM=dumb rebar3 compile`
   - 0 errors (mandatory)
   - 0 warnings (mandatory)

2. **EUnit**: `rebar3 eunit --module=erlmcp_transport_http_tests --verbose`
   - 100% pass rate (mandatory)
   - 0 skipped tests (all commented-out tests must be implemented)

3. **Coverage**: `rebar3 cover --verbose`
   - ≥80% coverage (mandatory gate)
   - Measure lines executed, not just function coverage

4. **Dialyzer**: `rebar3 dialyzer`
   - 0 warnings (mandatory)
   - All type specs present

5. **Xref**: `rebar3 xref`
   - 0 undefined function calls (mandatory)
   - All Gun functions are external (already in xref_ignores)

**Stop the Line (Jidoka):**
- If ANY gate fails, STOP and fix
- Do NOT proceed to next phase
- Do NOT commit failing code

## Manufacturing Checklist

### Before Implementation

- [x] **Research Verified**: Read actual source code (erlmcp_transport_http.erl, erlmcp_transport_http_server.erl, existing tests)
- [x] **Scope Confirmed**: IN = test suite creation only, OUT = implementation changes, Common Test, performance testing
- [x] **No Open Questions**: All dependencies verified (Gun, Cowboy, TLS validation), test patterns documented
- [x] **Phases Broken Down**: 4 phases, each ≤3 hours (total 9 hours)
- [x] **Acceptance Criteria Defined**: Measurable, specific, with file paths and quality gates

### During Implementation

**Phase 1 (Unit Tests):**
- [ ] Chicago School TDD followed (tests FIRST)
- [ ] OTP patterns followed (test fixtures, setup/cleanup)
- [ ] Pure functions tested (URL parsing, headers, state)
- [ ] Quality gates passing (compile, EUnit, coverage ≥20%)

**Phase 2 (Gun Integration):**
- [ ] Real Gun processes used (no mocks)
- [ ] Real HTTP server (Cowboy) on random port
- [ ] State inspection via get_state call
- [ ] Quality gates passing (coverage ≥60%)

**Phase 3 (Error Scenarios):**
- [ ] All error paths tested
- [ ] Process death scenarios tested
- [ ] Recovery logic verified
- [ ] Quality gates passing (coverage ≥75%)

**Phase 4 (Advanced Features):**
- [ ] Connection pool tested
- [ ] Keep-alive verified
- [ ] Exponential backoff tested
- [ ] Coverage ≥80% (MANDATORY GATE)

### After Implementation

- [ ] **All Tests Passing**: 100% pass rate (verified by EUnit)
- [ ] **Coverage ≥80%**: Verified by coverage report
- [ ] **Dialyzer 0 Warnings**: Verified by dialyzer
- [ ] **Xref 0 Undefined Calls**: Verified by xref
- [ ] **Performance No Regression >10%**: Tests complete in <30 seconds
- [ ] **Documentation Updated**: Test comments explain what is being tested
- [ ] **Code Review Complete**: OTP patterns verified, Chicago School TDD followed

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **Gun client missing in test environment** | P0 (Critical) | Low | Gun is in rebar.config deps (line 48), verify with `application:ensure_all_started(gun)` |
| **TLS certificate validation failures** | P0 (Critical) | High | erlmcp_tls_validation module doesn't exist (verified by glob). Skip HTTPS tests or create stub module with build_tls_options/2 |
| **Port conflicts in parallel tests** | P1 (High) | Medium | Use random port allocation (cowboy start with `port => 0`, get port with `ranch:get_port/1`) |
| **HTTP server startup timeout** | P1 (High) | Low | Add startup delay (timer:sleep(100)) after cowboy:start_clear |
| **Gun process not terminating cleanly** | P2 (Medium) | Medium | Ensure cleanup fixture calls gun:close() and demonitors process death |
| **Network timeout variability** | P2 (Medium) | Low | Use generous timeouts (15s for integration tests) and configurable timeouts in opts |
| **Exponential backoff delays in retry tests** | P2 (Medium) | Low | Use short retry_delay (100ms) in test opts, measure time to verify backoff |
| **Coverage target not achievable** | P3 (Low) | Low | TCP transport achieves >80% with similar complexity. Add targeted tests for uncovered lines. |
| **Cowboy handler complexity** | P3 (Low) | Low | Use simple cowboy loop handler that returns JSON responses. Reference cowboy documentation. |

### Rollback Plan

**If Something Goes Wrong:**

**Git Revert:**
```bash
git checkout main  # Return to base branch
git branch -D wreckit/008-create-eunit-test-suite-for-http-transport-erlmcpt  # Delete work branch
```

**Data Migration:** Not applicable (no database changes)

**Service Impact:** No service impact (test suite only, no production code changes)

**Recovery Strategy:**
1. If tests fail to compile: Fix compilation errors before commit
2. If coverage <80%: Add more tests in Phase 4 until target met
3. If Dialyzer warnings: Add type specs or fix type errors
4. If tests are flaky: Add synchronization or increase timeouts

## References

- **Research**: `/Users/sac/erlmcp/.wreckit/items/008-create-eunit-test-suite-for-http-transport-erlmcpt/research.md`
- **CLAUDE.md**: `/Users/sac/erlmcp/CLAUDE.md` (project rules, line 162: 43K msg/sec baseline)
- **TCPS**: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- **Test Reference**: `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl`
  - Lines 76-117: Client initialization test pattern
  - Lines 258-324: Client-server integration test pattern
- **HTTP Transport Implementation**: `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_http_server.erl`
  - Lines 13-34: State record definition
  - Lines 62-90: gen_server init/1 callback
  - Lines 119-136: Gun message handling
  - Lines 256-315: URL parsing functions
  - Lines 356-427: Connection management
  - Lines 499-548: HTTP request/response processing
  - Lines 603-634: Retry logic with exponential backoff

---

## Appendix: Test Coverage Targets

### Line-by-Line Coverage Goals (erlmcp_transport_http_server.erl)

**Initialization (lines 62-90):**
- [ ] init/1 callback (line 70)
- [ ] ensure_apps_started/0 (line 234)
- [ ] build_initial_state/1 (line 256)
- [ ] Owner monitoring (line 78)

**URL Parsing (lines 256-315):**
- [ ] normalize_url/1 (line 281)
- [ ] parse_url/1 (line 287)
- [ ] parse_url_parts/2 (line 295)
- [ ] parse_host_port/2 (line 307)

**Header Normalization (lines 317-350):**
- [ ] normalize_headers/1 (line 317)
- [ ] to_binary/1 (line 333)
- [ ] merge_headers/2 (line 339)

**Connection Management (lines 356-427):**
- [ ] connect/1 (line 356)
- [ ] build_gun_opts/1 (line 398)
- [ ] build_strict_tls_options/1 (line 429)

**Request Handling (lines 448-516):**
- [ ] enqueue_request/2 (line 448)
- [ ] process_request_queue/1 (line 454)
- [ ] send_request/2 (line 471)
- [ ] perform_request/2 (line 499)

**Response Handling (lines 518-601):**
- [ ] handle_gun_response/5 (line 518)
- [ ] handle_gun_error/3 (line 550)
- [ ] process_response/3 (line 573)
- [ ] process_body/2 (line 584)

**Retry Logic (lines 603-634):**
- [ ] should_retry/3 (line 603)
- [ ] schedule_retry/4 (line 615)
- [ ] retry_request/3 (line 623)
- [ ] calculate_retry_delay/2 (line 628)

**Gen Server Callbacks:**
- [ ] handle_call/3 (line 92)
- [ ] handle_info/2 (line 116)
- [ ] terminate/2 (line 219)

**Estimated Testable Lines:** 635 total, target ≥507 lines (80%)
