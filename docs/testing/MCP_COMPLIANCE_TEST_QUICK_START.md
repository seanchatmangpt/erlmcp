# MCP 2025-11-25 Compliance Testing - Quick Start Guide

**Version:** 1.0.0
**Date:** 2026-01-30
**Status:** Ready for Implementation

---

## Overview

This quick start guide provides immediate next steps for implementing MCP 2025-11-25 specification compliance tests in erlmcp. It complements the comprehensive [MCP_2025_11_25_COMPLIANCE_TEST_PLAN.md](./MCP_2025_11_25_COMPLIANCE_TEST_PLAN.md).

---

## Quick Summary

**Current State:**
- 38 existing test files
- 65 core modules to test
- Gaps in MCP 2025-11-25 compliance coverage

**Target State:**
- 300+ test cases
- ≥80% overall coverage (≥85% for core)
- 100% MCP 2025-11-25 spec compliance
- Chicago School TDD (no mocks, real processes)

---

## Installation and Setup

### 1. Verify Build Environment

```bash
cd /home/user/erlmcp

# Verify rebar3 is installed
which rebar3 || echo "Install rebar3 first"

# Verify project compiles
make compile

# Verify existing tests run
make test-core
```

### 2. Install Test Dependencies

Dependencies are already in `rebar.config`:
```erlang
{deps, [
    {jsx, "3.1.0"},           % JSON encoding
    {jesse, "1.8.1"},         % JSON Schema validation
    {proper, "1.4.0"},        % Property-based testing
    {meck, "0.9.2"}           % ONLY for external I/O mocking (NOT for gen_servers)
]}.
```

---

## Priority 1: Critical Tests (Week 1-2)

### Test Suite 1: Server Initialization (P0 Security)

**File:** `apps/erlmcp_core/test/erlmcp_server_initialization_tests.erl`

**Critical Tests:**
```erlang
-module(erlmcp_server_initialization_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% P0: MUST reject non-initialize requests before initialization
reject_non_initialize_before_init_test() ->
    %% Setup: Start server (Chicago School: real gen_server)
    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),

    %% Exercise: Try to call resources/list BEFORE initialize
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"resources/list">>,
        <<"params">> => #{}
    },

    %% Verify: MUST return ?MCP_ERROR_NOT_INITIALIZED (-32005)
    Response = send_request(Server, Request),
    ?assert(maps:is_key(<<"error">>, Response)),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(?MCP_ERROR_NOT_INITIALIZED, maps:get(<<"code">>, Error)),

    %% Cleanup
    erlmcp_server:stop(Server).

%% P0: MUST reject double initialize
reject_double_initialize_test() ->
    %% Setup
    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),

    %% Exercise: Initialize once (should succeed)
    InitReq = build_initialize_request(),
    {ok, _Response1} = send_request(Server, InitReq),

    %% Exercise: Initialize again (should fail)
    Response2 = send_request(Server, InitReq),

    %% Verify: MUST return error (not crash)
    ?assert(maps:is_key(<<"error">>, Response2)),

    %% Cleanup
    erlmcp_server:stop(Server).
```

**Run:**
```bash
rebar3 eunit --module=erlmcp_server_initialization_tests
```

---

### Test Suite 2: State Machine (P0 Security)

**File:** `apps/erlmcp_core/test/erlmcp_server_state_machine_tests.erl`

**Critical Tests:**
```erlang
%% P0: Validate state machine phases
phase_transition_init_to_initialized_test() ->
    %% Setup
    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),

    %% Verify: Initial phase is INITIALIZATION
    ?assertEqual(?MCP_PHASE_INITIALIZATION, get_server_phase(Server)),

    %% Exercise: Send initialize
    InitReq = build_initialize_request(),
    {ok, _Response} = send_request(Server, InitReq),

    %% Verify: Phase transitioned to INITIALIZED
    ?assertEqual(?MCP_PHASE_INITIALIZED, get_server_phase(Server)),

    %% Cleanup
    erlmcp_server:stop(Server).
```

---

### Test Suite 3: JSON-RPC Compliance

**File:** `apps/erlmcp_core/test/erlmcp_json_rpc_compliance_tests.erl`

**Critical Tests:**
```erlang
%% Validate error codes match spec
mcp_error_not_initialized_test() ->
    %% Exercise: Create NOT_INITIALIZED error
    Error = erlmcp_json_rpc:encode_error(1, ?MCP_ERROR_NOT_INITIALIZED, <<"Not initialized">>),

    %% Verify: Error structure
    Decoded = jsx:decode(Error, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(1, maps:get(<<"id">>, Decoded)),
    ?assert(maps:is_key(<<"error">>, Decoded)),

    ErrorObj = maps:get(<<"error">>, Decoded),
    ?assertEqual(-32005, maps:get(<<"code">>, ErrorObj)),
    ?assertEqual(<<"Not initialized">>, maps:get(<<"message">>, ErrorObj)).
```

---

## Running Tests

### Run Single Test Module

```bash
# Run specific test module
rebar3 eunit --module=erlmcp_server_initialization_tests

# Run with verbose output
rebar3 eunit --module=erlmcp_server_initialization_tests --verbose
```

### Run All Tests

```bash
# Run all EUnit tests
make test-core

# Run all Common Test suites
rebar3 ct

# Run property tests
rebar3 proper --numtests=1000
```

### Run with Coverage

```bash
# Generate coverage
rebar3 eunit --cover
rebar3 cover --verbose

# View HTML report
open _build/test/cover/index.html
```

---

## Quality Gates Checklist

Before marking any test suite as complete:

```bash
# 1. Compilation (MUST pass)
✅ make compile
# Expected: 0 errors

# 2. Tests (MUST pass 100%)
✅ rebar3 eunit --module=<module>_tests
# Expected: X/X passed (0 failures)

# 3. Coverage (MUST be ≥80%)
✅ rebar3 cover --verbose
# Expected: ≥80% overall, ≥85% for core modules

# 4. Dialyzer (SHOULD have 0 warnings)
✅ rebar3 dialyzer
# Expected: 0 warnings

# 5. Xref (SHOULD have 0 issues)
✅ rebar3 xref
# Expected: 0 issues

# 6. Format (MUST be formatted)
✅ rebar3 format --verify
# Expected: All files formatted
```

**Verification Report Template:**
```
✅ Tests: 25/25 passed (EUnit: 25)
✅ Quality: Compile clean, tests compile clean
✅ Coverage: 90% for erlmcp_server initialization logic
✅ Chicago School TDD: Real gen_servers ✅, No mocks ✅, State-based ✅
✅ Edge Cases: double initialize, pre-init rejection, protocol version validation

Ready for review: erlmcp_server_initialization_tests.erl
```

---

## Test Helper Functions

Use these helpers in all test modules:

```erlang
%% Build initialize request
build_initialize_request() ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{
            <<"protocolVersion">> => <<"2025-11-25">>,
            <<"capabilities">> => #{},
            <<"clientInfo">> => #{
                <<"name">> => <<"test-client">>,
                <<"version">> => <<"1.0.0">>
            }
        }
    }.

%% Send request to server (Chicago School: real message passing)
send_request(Server, Request) ->
    RequestBinary = jsx:encode(Request),
    gen_server:call(Server, {handle_message, RequestBinary}).

%% Get server phase (via API, not internal inspection)
get_server_phase(Server) ->
    {ok, State} = gen_server:call(Server, get_state),
    State#state.phase.

%% Assert error code
assert_error_code(Response, ExpectedCode) ->
    ?assert(maps:is_key(<<"error">>, Response)),
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(ExpectedCode, maps:get(<<"code">>, Error)).
```

---

## Test Implementation Workflow

### Step-by-Step Process

**1. Read Specification**
- Read relevant section in MCP_2025_11_25_COMPLIANCE_TEST_PLAN.md
- Understand feature requirements
- Identify edge cases

**2. Write Test (TDD Red Phase)**
```erlang
feature_test() ->
    %% Setup: Start real gen_server
    {ok, Server} = erlmcp_server:start_link(...),

    %% Exercise: Call API
    Result = erlmcp_server:operation(Server, Args),

    %% Verify: Check observable state (Chicago School)
    ?assertEqual(Expected, Result),

    %% Cleanup
    erlmcp_server:stop(Server).
```

**3. Run Test (Expect Failure)**
```bash
rebar3 eunit --module=<module>_tests
# Expected: Test fails (feature not implemented)
```

**4. Implement Feature (TDD Green Phase)**
- Implement minimum code to pass test
- Don't over-engineer

**5. Run Test (Expect Success)**
```bash
rebar3 eunit --module=<module>_tests
# Expected: Test passes
```

**6. Refactor (TDD Refactor Phase)**
- Clean up code
- Keep tests green

**7. Verify Quality Gates**
```bash
make compile          # 0 errors
rebar3 cover          # Check coverage
rebar3 dialyzer       # 0 warnings
```

**8. Repeat for Next Test**

---

## Chicago School TDD Reminders

### DO (✅)
```erlang
%% ✅ Use real gen_server
{ok, Server} = erlmcp_server:start_link(...),

%% ✅ Assert on observable state
Result = erlmcp_server:get_resource(Server, Uri),
?assertEqual({ok, Content}, Result),

%% ✅ Use real processes for transports
{ok, Transport} = erlmcp_transport_tcp:start_link(...),
```

### DON'T (❌)
```erlang
%% ❌ Don't use meck for gen_servers
meck:new(erlmcp_server),
meck:expect(erlmcp_server, get_resource, fun(...) -> ... end),

%% ❌ Don't verify internal method calls
?assert(meck:called(erlmcp_server, internal_function, ['_'])),

%% ❌ Don't test implementation details
?assertEqual(some_internal_state, Server#state.internal_field),
```

---

## Common Pitfalls and Solutions

### Pitfall 1: Tests Depend on Order
**Problem:** Tests fail when run in different order
**Solution:** Make tests independent - each test has own setup/cleanup

### Pitfall 2: Flaky Tests
**Problem:** Tests pass sometimes, fail other times
**Solution:** Add timer:sleep/1 for async operations, or use synchronous calls

### Pitfall 3: Low Coverage
**Problem:** Coverage below 80%
**Solution:** Add tests for error paths and edge cases

### Pitfall 4: Slow Tests
**Problem:** Test suite takes >1 minute
**Solution:** Use setup_all for expensive operations, avoid sleeps

### Pitfall 5: Mocking Gen Servers
**Problem:** Using meck for gen_servers
**Solution:** Start real gen_servers (Chicago School TDD)

---

## Test Execution Schedule

### Week 1 (Foundation)
```bash
# Monday-Tuesday: Setup and initialization tests
rebar3 eunit --module=erlmcp_server_initialization_tests

# Wednesday-Thursday: State machine tests
rebar3 eunit --module=erlmcp_server_state_machine_tests

# Friday: JSON-RPC compliance tests
rebar3 eunit --module=erlmcp_json_rpc_compliance_tests

# Target: 60% coverage
rebar3 cover --verbose
```

### Week 2 (Core - Part 1)
```bash
# Monday-Tuesday: Tool tests
rebar3 eunit --module=erlmcp_server_tools_tests

# Wednesday-Thursday: Resource tests
rebar3 eunit --module=erlmcp_server_resources_tests

# Friday: Capability negotiation tests
rebar3 eunit --module=erlmcp_capabilities_negotiation_tests

# Target: 75% coverage
rebar3 cover --verbose
```

### Week 3 (Core - Part 2)
```bash
# Monday-Tuesday: Prompt tests
rebar3 eunit --module=erlmcp_server_prompts_tests

# Wednesday-Thursday: Notification handler tests
rebar3 eunit --module=erlmcp_notification_handler_integration_tests

# Friday: Progress tracking tests
rebar3 eunit --module=erlmcp_progress_tracking_tests

# Target: 80% coverage
rebar3 cover --verbose
```

### Week 4 (Advanced)
```bash
# Monday: URI validation tests
rebar3 eunit --module=erlmcp_uri_validator_tests

# Tuesday: Content type tests
rebar3 eunit --module=erlmcp_content_type_validation_tests

# Wednesday: Error code tests
rebar3 eunit --module=erlmcp_error_code_compliance_tests

# Thursday-Friday: Integration tests
rebar3 ct --suite=erlmcp_integration_compliance_SUITE

# Target: 85% coverage
rebar3 cover --verbose
```

---

## Resources

**Documentation:**
- [Full Test Plan](./MCP_2025_11_25_COMPLIANCE_TEST_PLAN.md) - Comprehensive 300+ test plan
- [MCP Protocol](../protocol.md) - Protocol specification
- [Testing Strategy](./MCP_SERVER_TESTING_STRATEGY.md) - Original server testing strategy
- [Edge Cases](../mcp-edge-case-testing-strategy.md) - Edge case testing

**Code References:**
- Server: `apps/erlmcp_core/src/erlmcp_server.erl`
- Client: `apps/erlmcp_core/src/erlmcp_client.erl`
- JSON-RPC: `apps/erlmcp_core/src/erlmcp_json_rpc.erl`
- Capabilities: `apps/erlmcp_core/src/erlmcp_capabilities.erl`

**Existing Tests (Examples):**
- `apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
- `apps/erlmcp_core/test/erlmcp_client_tests.erl`
- `apps/erlmcp_core/test/erlmcp_resource_tests.erl`

**External Resources:**
- EUnit: http://erlang.org/doc/apps/eunit/chapter.html
- Common Test: http://erlang.org/doc/apps/common_test/chapter.html
- Proper: https://proper-testing.github.io/
- Chicago School TDD: https://martinfowler.com/articles/mocksArentStubs.html

---

## Getting Help

**Test Failures:**
```bash
# Run with verbose output
rebar3 eunit --module=<module>_tests --verbose

# Check for specific test
rebar3 eunit --test=<test_name>
```

**Coverage Issues:**
```bash
# Generate detailed coverage report
rebar3 cover --verbose

# View in browser
open _build/test/cover/index.html
```

**Dialyzer Warnings:**
```bash
# Run dialyzer
rebar3 dialyzer

# Build PLT if needed
rebar3 dialyzer --build-plt
```

---

## Next Steps

1. **Verify environment:**
   ```bash
   make compile && make test-core
   ```

2. **Start with P0 tests:**
   - Create `erlmcp_server_initialization_tests.erl`
   - Implement 25 tests from test plan
   - Run and verify: `rebar3 eunit --module=erlmcp_server_initialization_tests`

3. **Continue with state machine:**
   - Create `erlmcp_server_state_machine_tests.erl`
   - Implement 20 tests
   - Target: 60% coverage by end of Week 1

4. **Follow the schedule:**
   - Week 1: Foundation (60% coverage)
   - Week 2-3: Core (80% coverage)
   - Week 4: Advanced (85% coverage)

---

**Document Version:** 1.0.0
**Last Updated:** 2026-01-30
**Status:** Ready for Implementation

**START TESTING!**
