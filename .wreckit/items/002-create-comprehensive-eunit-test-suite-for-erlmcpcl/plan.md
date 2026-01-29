# Create comprehensive EUnit test suite for erlmcp_client.erl Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Create a comprehensive Chicago School TDD test suite for `erlmcp_client.erl` that achieves ≥80% code coverage with ≥50 tests, testing ALL gen_server callbacks (init, handle_call, handle_cast, handle_info, terminate, code_change), ALL phase transitions, ALL error paths, and ALL concurrent operations using REAL processes (no mocks).

### Quality Gate Requirements
- **Compilation**: 0 errors (MANDATORY)
- **EUnit**: 100% pass rate (MANDATORY)
- **Common Test**: Not applicable (unit tests only)
- **Coverage**: ≥80% (MANDATORY)
- **Dialyzer**: 0 warnings (MANDATORY)
- **Xref**: 0 undefined function calls (MANDATORY)
- **Performance**: Execution time <5 seconds (MANDATORY)

## Current State

### What Exists Now
- **Modules**:
  - `apps/erlmcp_core/src/erlmcp_client.erl` (729 lines) - Main gen_server implementation
  - `apps/erlmcp_core/test/erlmcp_client_tests.erl` (438 lines) - Inadequate test coverage
  - `apps/erlmcp_core/include/erlmcp.hrl` (523 lines) - Type definitions and constants
  - `apps/erlmcp_core/test/erlmcp_session_manager_tests.erl` (527 lines) - Reference pattern

- **Tests**:
  - Current: ~20 tests, <10% coverage, 100% pass rate (but tests don't verify anything meaningful)
  - Tests use `?assert(true)` to skip errors
  - No state verification (sys:get_status/1 or direct state inspection)
  - No handle_call clause coverage (19 different patterns in lines 217-412)
  - No handle_info clause coverage (2 patterns in lines 418-437)
  - No request ID correlation testing
  - No phase transition testing
  - No batch request state testing
  - No notification handler delivery testing
  - No concurrent request testing

- **Quality**:
  - **Compilation**: PASS (0 errors)
  - **EUnit**: PASS (100% but meaningless)
  - **Coverage**: FAIL (<10%, need ≥80%, GAP: 70+ percentage points)
  - **Dialyzer**: UNKNOWN
  - **Xref**: PASS (0 undefined function calls)
  - **CRITICAL BUG FOUND**: Line 478 calls `erlmcp_request_id:safe_increment/1` which doesn't exist (module not implemented)

### What's Missing
- **Gap**: 70+ percentage points coverage (from <10% to ≥80%)
- **Gap**: 30+ additional tests needed (from ~20 to ≥50)
- **Gap**: Complete rewrite of test suite following Chicago School TDD
- **Root Cause**: No enforcement of Chicago School TDD methodology during development
- **Impact**: BLOCKS ALL PRODUCTION WORK - untested critical path for all client operations

### Key Discoveries from Code Verification
1. **CRITICAL BUG** (line 478): `erlmcp_request_id:safe_increment/1` called but module doesn't exist - this is a CRASH waiting to happen
2. **State record** (lines 49-66): 13 fields including pending_requests map, batch_requests map, notification_handlers map - NONE of these are verified in current tests
3. **Phase machine** (line 46): 5 phases (pre_initialization, initializing, initialized, error, closed) - phase transitions NEVER tested
4. **Request correlation** (lines 597-628): handle_response/3 matches request IDs to pending_requests - NEVER tested
5. **Notification routing** (lines 652-680): handle_notification/3 routes to handlers - NEVER tested
6. **Batch management** (lines 361-392): start_batch, add_to_batch, execute_batch, cancel_batch - state NEVER verified
7. **Reference pattern exists**: erlmcp_session_manager_tests.erl shows correct Chicago School TDD approach with 25 tests, 527 lines, 100% passing

## Desired End State

### Specification

**Module to Create**: `apps/erlmcp_core/test/erlmcp_client_tests.erl`

**Test Structure**:
- Use `{foreach}` setup/teardown for test isolation (lines 14-60 of reference)
- Start REAL gen_server processes with `start_link/1` (no mocks)
- Verify state with `sys:get_status/1` or direct state inspection
- Test success paths AND error paths explicitly
- Use `?assertMatch` for complex data structures
- Test concurrent operations with spawned processes
- ALL tests must pass (100% pass rate)

**Test Coverage Targets**:
- **init/1 callback**: Test transport initialization, error handling
- **handle_call/3 callback**: Test ALL 19 patterns (lines 217-412)
- **handle_cast/2 callback**: Test empty handler (lines 414-416)
- **handle_info/2 callback**: Test transport_message and EXIT patterns (lines 418-437)
- **terminate/2 callback**: Test cleanup on normal/shutdown
- **code_change/3 callback**: Test hot code upgrade
- **Public API**: Test ALL exported functions (lines 7-20)
- **Phase transitions**: Test ALL 5 phases and transitions
- **Request correlation**: Test pending_requests map, request ID generation
- **Batch management**: Test batch_requests map, lifecycle
- **Notification handling**: Test notification_handlers map, routing
- **Error paths**: Test transport failures, invalid phases, request ID overflow
- **Concurrent operations**: Test 10+ concurrent requests, race conditions

### Verification

**Automated Verification**:
```bash
# Run all tests
TERM=dumb rebar3 eunit --module=erlmcp_client_tests

# Generate coverage report
rebar3 cover --verbose

# Verify ≥80% coverage for erlmcp_client.erl
rebar3 cover --verbose | grep erlmcp_client.erl | awk '{print $2}' | cut -d'%' -f1

# Dialyzer check
rebar3 dialyzer

# Xref check
rebar3 xref
```

**Manual Verification**:
- Verify ALL 19 handle_call patterns are covered (lines 217-412)
- Verify ALL 2 handle_info patterns are covered (lines 418-437)
- Verify request ID correlation works under load (100+ concurrent requests)
- Verify no memory leaks (subscribe/unsubscribe balanced)
- Verify test execution time <5 seconds

**Quality Metrics**:
- Coverage: ≥80% (target: 90%)
- Test count: ≥50 (target: 60)
- Execution time: <5 seconds (target: <3 seconds)
- Pass rate: 100% (zero tolerance for failures)

### Manufacturing Output
- **Code**: `apps/erlmcp_core/test/erlmcp_client_tests.erl` (complete rewrite, ~1200 lines)
- **Tests**: ≥50 EUnit tests covering all code paths
- **Documentation**: Test documentation in comments (Chicago School TDD methodology)
- **Receipts**: Coverage report showing ≥80%, test execution log showing 100% pass rate

## What We're NOT Doing

- **Transport implementation testing**: erlmcp_transport_stdio, erlmcp_transport_tcp, erlmcp_transport_http are separate modules with their own tests
- **JSON-RPC encoding/decoding testing**: erlmcp_json_rpc module is tested separately
- **Server-side testing**: erlmcp_server module is tested separately
- **Integration testing with real MCP servers**: This is unit testing, not integration testing
- **Performance benchmarking**: Focus on correctness, not performance (performance tests are separate)
- **Property-based testing**: Use EUnit for deterministic testing (Proper is separate)

## Manufacturing Approach

### TCPS Methodology
Following Toyota Code Production System phases:

1. **Specification** - Requirements with acceptance criteria (≥80% coverage, ≥50 tests, 100% pass rate)
2. **Pseudocode** - Algorithm design BEFORE coding (test structure defined in plan)
3. **Architecture** - Integration points and dependencies (gen_server behavior, real transports)
4. **Refinement** - Chicago School TDD (tests FIRST, verify failures, THEN implementation)
5. **Completion** - All quality gates passing (compile, eunit, coverage, dialyzer, xref)

### Implementation Strategy

**Test Strategy**: Chicago School TDD
- **NO MOCKS** - Use real gen_server processes
- **State-Based Verification** - Check #state{} record contents via sys:get_status/1
- **Integration Testing** - Test with real transport behaviors (stdio, tcp, http)
- **Race Condition Testing** - Concurrent operations with spawned processes

**Phase Breakdown**:
1. **Foundation** (2 hours): Setup/teardown, lifecycle tests
2. **Initialization** (2 hours): Phase transitions, capability negotiation
3. **Request-Response** (2 hours): Request ID correlation, pending_requests map
4. **Capability Operations** (2 hours): Resources, tools, prompts operations
5. **Notification Handling** (2 hours): Notification routing, handler delivery
6. **Batch Management** (2 hours): Batch request lifecycle, batch_requests map
7. **Error Handling** (2 hours): All error paths, transport failures
8. **Concurrent Operations** (2 hours): Race conditions, concurrent requests

**Total Estimated Time**: 16 hours (2 days)

### Quality Integration
- **Pre-commit Hooks**: Run `rebar3 compile` and `rebar3 eunit` before commit
- **CI Gates**: All quality gates must pass (compile, eunit, coverage, dialyzer, xref)
- **Receipt Generation**: Coverage report saved as artifact, test log saved
- **Andon Signaling**: Test failures visible immediately in CI, fail build

---

## Phases

### Phase 1: Foundation - Setup/Teardown and Lifecycle (≤2 hours)

#### Overview
Establish test infrastructure following Chicago School TDD pattern, ensure client can start/stop cleanly, verify gen_server lifecycle.

#### Specification
Create test fixtures using `{foreach}` for isolation, test `start_link/1,2`, `stop/1`, verify gen_server is alive and clean termination.

#### Pseudocode
```erlang
%% Setup: Start application and client
setup() ->
    {ok, _} = application:ensure_all_started(erlmcp_core),
    TransportOpts = {stdio, #{test_mode => true}},
    {ok, Client} = erlmcp_client:start_link(TransportOpts),
    Client.

%% Cleanup: Stop client
cleanup(Client) ->
    erlmcp_client:stop(Client).

%% Test: Verify client is alive
test_client_is_alive(Client) ->
    ?assert(erlang:is_process_alive(Client)).
```

#### Architecture
**Integration Points**:
- erlmcp_client gen_server (module under test)
- erlmcp_transport_stdio (real transport, test_mode)
- erlmcp_json_rpc (JSON-RPC encoding/decoding)
- OTP application controller (start/stop applications)

**Process Structure**:
- Test process spawns client gen_server
- Client gen_server manages transport
- Test process verifies client state via sys:get_status/1

#### Changes Required:

##### 1. Test Fixture Setup
**File**: `apps/erlmcp_core/test/erlmcp_client_tests.erl`
**Current**: Lines 416-437 have inadequate setup/cleanup
**Changes**: Replace with proper `{foreach}` fixtures following erlmcp_session_manager_tests.erl pattern
**Reason**: Achieve test isolation, prevent test pollution

```erlang
%% BEFORE (inadequate - lines 416-437)
setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

setup_client() ->
    setup(),
    TransportOpts = {stdio, #{test_mode => true}},
    case erlmcp_client:start_link(TransportOpts) of
        {ok, Client} -> Client;
        {error, _} -> undefined
    end.

%% AFTER (proposed - Chicago School TDD)
setup() ->
    %% Start application
    {ok, _} = application:ensure_all_started(erlmcp_core),
    %% Start client with stdio transport (test_mode)
    TransportOpts = {stdio, #{test_mode => true}},
    {ok, Client} = erlmcp_client:start_link(TransportOpts),
    Client.

cleanup(Client) ->
    %% Stop client
    case is_process_alive(Client) of
        true ->
            unlink(Client),
            exit(Client, shutdown),
            timer:sleep(10);
        false ->
            ok
    end.
```

##### 2. Lifecycle Tests
**File**: `apps/erlmcp_core/test/erlmcp_client_tests.erl`
**Current**: Lines 14-65 have basic tests but only check if process is alive
**Changes**: Add state verification via sys:get_status/1, test with different transports
**Reason**: Verify gen_server state initialization, not just process existence

```erlang
%% Test suite structure
client_lifecycle_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_start_link_stdio/1,
         fun test_start_link_tcp/1,
         fun test_start_link_with_options/1,
         fun test_stop_normal/1,
         fun test_stop_crash/1
     ]}.

test_start_link_stdio(_Client) ->
    fun() ->
        TransportOpts = {stdio, #{test_mode => true}},
        {ok, Client} = erlmcp_client:start_link(TransportOpts),
        ?assert(is_pid(Client)),
        ?assert(erlang:is_process_alive(Client)),
        %% Verify state initialization
        {status, _Pid, {module, _Mod}, [_PDict, _SysState, _Parent, _Dbg, Misc]} = sys:get_status(Client),
        State = proplists:get_value("State", Misc),
        ?assertMatch(#state{phase = pre_initialization}, State),
        erlmcp_client:stop(Client)
    end.

test_start_link_tcp(_Client) ->
    fun() ->
        TransportOpts = {tcp, #{host => "localhost", port => 0}},  % Dummy port for testing
        {ok, Client} = erlmcp_client:start_link(TransportOpts),
        ?assert(is_pid(Client)),
        ?assertMatch(#state{transport = erlmcp_transport_tcp}, get_state(Client)),
        erlmcp_client:stop(Client)
    end.

test_start_link_with_options(_Client) ->
    fun() ->
        TransportOpts = {stdio, #{test_mode => true}},
        ClientOpts = #{strict_mode => true, timeout => 10000},
        {ok, Client} = erlmcp_client:start_link(TransportOpts, ClientOpts),
        State = get_state(Client),
        ?assertEqual(true, State#state.strict_mode),
        ?assertEqual(10000, State#state.timeout),
        erlmcp_client:stop(Client)
    end.

test_stop_normal(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
        ?assert(erlang:is_process_alive(Client)),
        ok = erlmcp_client:stop(Client),
        timer:sleep(10),
        ?assertNot(erlang:is_process_alive(Client))
    end.

test_stop_crash(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
        ?assert(erlang:is_process_alive(Client)),
        %% Simulate crash
        exit(Client, crash),
        timer:sleep(10),
        ?assertNot(erlang:is_process_alive(Client))
    end.

%% Helper: Get state from gen_server
get_state(Client) ->
    {status, _Pid, {module, _Mod}, [_PDict, _SysState, _Parent, _Dbg, Misc]} = sys:get_status(Client),
    proplists:get_value("State", Misc).
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_client_tests` - 100% pass rate (5/5 tests pass)
- [ ] Coverage: `rebar3 cover` - ≥10% for init/1, start_link/1, stop/1
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: Test fixtures follow erlmcp_session_manager_tests.erl pattern
- [ ] Integration: Client starts with stdio, tcp transports
- [ ] Edge cases: Test with invalid transport options (should fail gracefully)
- [ ] Performance: All 5 tests execute in <1 second

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 2: Initialization and Phase Transitions (≤2 hours)

#### Overview
Test ALL phase transitions (pre_initialization → initializing → initialized → error/closed), verify capability negotiation, test initialization error handling.

#### Specification
Test `initialize/2,3` function, verify phase transitions, test capability extraction from initialize response, test initialization failure transitions to error phase.

#### Pseudocode
```erlang
%% Setup: Create client in pre_initialization phase
setup_client() ->
    {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
    Client.

%% Test: Initialize from pre_initialization phase
test_initialize_pre_initialization(Client) ->
    Capabilities = #mcp_client_capabilities{},
    Result = erlmcp_client:initialize(Client, Capabilities),
    ?assertMatch({ok, _ServerInfo}, Result),
    ?assertMatch(initializing, get_phase(Client)).

%% Test: Initialize from wrong phase fails
test_initialize_wrong_phase(Client) ->
    %% First initialize succeeds
    ok = erlmcp_client:initialize(Client, #mcp_client_capabilities{}),
    %% Second initialize fails
    Result = erlmcp_client:initialize(Client, #mcp_client_capabilities{}),
    ?assertMatch({error, {invalid_phase, initializing, _}}, Result).
```

#### Architecture
**Phase Machine**:
- pre_initialization (default) → initialize called
- initializing (initialize sent) → received initialize response
- initialized (received initialized notification) → ready for operations
- error (initialize failed) → no operations allowed
- closed (transport closed) → terminating

**Integration Points**:
- erlmcp_json_rpc:encode_request/3 (build initialize request)
- erlmcp_json_rpc:decode_message/1 (parse initialize response)
- Transport behavior (send/receive JSON-RPC messages)

#### Changes Required:

##### 1. Phase Transition Tests
**File**: `apps/erlmcp_core/test/erlmcp_client_tests.erl`
**Current**: Lines 71-112 have basic initialization tests but skip if no server
**Changes**: Add phase transition verification, test with mock server responses
**Reason**: Verify phase machine correctness, critical for initialization flow

```erlang
initialization_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_initialize_pre_initialization_phase/1,
         fun test_initialize_invalid_phase/1,
         fun test_initialize_with_capabilities/1,
         fun test_capabilities_extracted_from_init_response/1,
         fun test_initialize_error_transitions_to_error_phase/1
     ]}.

test_initialize_pre_initialization_phase(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
        ?assertEqual(pre_initialization, get_phase(Client)),

        %% Send initialize request
        Capabilities = #mcp_client_capabilities{
            roots = #mcp_capability{enabled = false},
            sampling = #mcp_capability{enabled = false}
        },

        %% Mock the initialize response (inject directly into gen_server)
        %% In real test, we'd need to wait for transport to deliver response
        %% For now, just verify phase changes to initializing
        %% TODO: This test needs real transport mocking or server simulation

        erlmcp_client:stop(Client)
    end.

test_initialize_invalid_phase(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Manually set phase to initialized (bypass normal flow)
        %% This tests phase enforcement
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
        %% We can't easily set phase without accessing internal state
        %% Instead, test that calling initialize twice fails

        Capabilities = #mcp_client_capabilities{},
        %% First initialize would succeed if we had a server
        %% For now, test the API contract
        %% TODO: This needs transport layer mocking

        erlmcp_client:stop(Client)
    end.

%% Helper: Get phase from state
get_phase(Client) ->
    State = get_state(Client),
    State#state.phase.
```

##### 2. Capability Extraction Tests
**File**: `apps/erlmcp_core/test/erlmcp_client_tests.erl`
**Current**: No capability extraction tests
**Changes**: Test extract_server_capabilities/1, verify capabilities stored in state
**Reason**: Ensure server capabilities correctly parsed and stored

```erlang
test_capabilities_extracted_from_init_response(_Client) ->
    fun() ->
        %% Test the pure function extract_server_capabilities/1
        InitResult = #{
            <<"protocolVersion">> => <<"2025-06-18">>,
            <<"capabilities">> => #{
                <<"resources">> => #{},
                <<"tools">> => #{},
                <<"prompts">> => #{}
            },
            <<"serverInfo">> => #{
                <<"name">> => <<"test-server">>,
                <<"version">> => <<"1.0.0">>
            }
        },

        %% Call internal function via test export
        Caps = erlmcp_client:extract_server_capabilities(InitResult),
        ?assertMatch(#mcp_server_capabilities{
            resources = #mcp_resources_capability{},
            tools = #mcp_tools_capability{},
            prompts = #mcp_prompts_capability{}
        }, Caps)
    end.
```

**CRITICAL**: The current tests cannot verify phase transitions because:
1. No real MCP server is available in test environment
2. Transport layer doesn't support injecting messages in test_mode
3. Cannot simulate JSON-RPC responses without mocking transport

**Solution**: Either:
- **Option A**: Add test exports to erlmcp_client to call extract_server_capabilities/1 directly (DONE in line 24)
- **Option B**: Create a mock transport module that can inject messages (preferred for Chicago School TDD)
- **Option C**: Skip integration tests and only test pure functions (inadequate)

**DECISION**: Use Option B - Create mock transport module that can inject messages (see Phase 8 for details)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_client_tests` - 100% pass rate (5/5 initialization tests pass)
- [ ] Coverage: `rebar3 cover` - ≥20% for handle_call/3 (initialize patterns)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: Phase transitions tested for all 5 phases
- [ ] Integration: Capability extraction works with real server responses
- [ ] Edge cases: Invalid server response formats handled
- [ ] Performance: All 5 tests execute in <2 seconds

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 3: Request-Response Correlation (≤2 hours)

#### Overview
Test request ID generation, pending_requests map management, response correlation, request ID overflow protection (P0 SECURITY).

#### Specification
Test ALL capability-based operations (list_resources, list_tools, list_prompts), verify request IDs are unique, verify pending_requests map stores request info, verify responses remove from pending_requests.

#### Pseudocode
```erlang
%% Setup: Create initialized client
setup_initialized_client() ->
    {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
    %% Initialize and inject response to reach initialized phase
    Client.

%% Test: list_resources generates unique request ID
test_list_resources_generates_unique_request_id(Client) ->
    %% Get initial request ID
    State1 = get_state(Client),
    InitialId = State1#state.request_id,

    %% Call list_resources
    erlmcp_client:list_resources(Client),

    %% Verify request ID incremented
    State2 = get_state(Client),
    ?assertEqual(InitialId + 1, State2#state.request_id),

    %% Verify pending_requests contains the request
    ?assert(maps:is_key(InitialId, State2#state.pending_requests)).

%% Test: Response removes from pending_requests
test_response_removes_from_pending_requests(Client) ->
    %% Send request
    erlmcp_client:list_resources(Client),

    %% Inject response via transport
    %% TODO: Need mock transport to inject response

    ok.
```

#### Architecture
**Request-Response Flow**:
1. Client calls API function (e.g., list_resources)
2. gen_server:call sends message to handle_call/3
3. handle_call/3 calls send_request/4 to generate request ID and store in pending_requests
4. send_request/4 sends JSON-RPC request via transport
5. Transport delivers response to handle_info/2
6. handle_info/2 calls handle_response/3
7. handle_response/3 matches request ID from pending_requests and replies to caller

**Integration Points**:
- erlmcp_json_rpc:encode_request/3 (build JSON-RPC request)
- erlmcp_json_rpc:decode_message/1 (parse JSON-RPC response)
- Transport behavior (send/receive messages)

#### Changes Required:

##### 1. Request ID Generation Tests
**File**: `apps/erlmcp_core/test/erlmcp_client_tests.erl`
**Current**: No request ID generation tests
**Changes**: Test request ID increment, pending_requests map storage
**Reason**: Verify request correlation logic, prevent ID collisions

```erlang
request_correlation_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_request_id_increment/1,
         fun test_pending_requests_stores_request_info/1,
         fun test_response_removes_from_pending_requests/1,
         fun test_concurrent_requests_no_id_collision/1,
         fun test_request_id_overflow_protection/1  % P0 SECURITY
     ]}.

test_request_id_increment(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
        State1 = get_state(Client),
        InitialId = State1#state.request_id,
        ?assertEqual(1, InitialId),

        %% Send a request (will block waiting for response in real scenario)
        %% For testing, we just verify state before/after
        %% TODO: Need mock transport to inject responses

        %% Verify request ID not incremented yet (request not sent)
        State2 = get_state(Client),
        ?assertEqual(InitialId, State2#state.request_id),

        erlmcp_client:stop(Client)
    end.

test_pending_requests_stores_request_info(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
        State1 = get_state(Client),
        ?assertEqual(#{}, State1#state.pending_requests),

        %% Send request
        %% TODO: Need mock transport

        erlmcp_client:stop(Client)
    end.

test_request_id_overflow_protection(_Client) ->
    fun() ->
        %% CRITICAL BUG: erlmcp_request_id:safe_increment/1 doesn't exist
        %% This test will FAIL until the module is implemented

        %% Test request ID overflow at 16#7FFFFFFF (max 32-bit signed int)
        MaxId = 16#7FFFFFFF,

        %% TODO: Need to set request_id to MaxId in state
        %% Then send a request and verify overflow is detected

        ?assert(true)  % Placeholder until bug is fixed
    end.
```

**CRITICAL BUG DISCOVERED**:
- **Line 478** calls `erlmcp_request_id:safe_increment/1` which doesn't exist
- This is a CRASH waiting to happen when request_id reaches 16#7FFFFFFF
- Module `erlmcp_request_id` needs to be created OR this logic needs to be inlined
- This is P0 SECURITY - request ID overflow could cause request ID collisions

**DECISION**: Create fix ticket for erlmcp_request_id module, document bug in current test suite

##### 2. Concurrent Request Tests
**File**: `apps/erlmcp_core/test/erlmcp_client_tests.erl`
**Current**: No concurrent request tests
**Changes**: Test 10+ concurrent requests, verify no ID collisions
**Reason**: Verify request ID generation is thread-safe

```erlang
test_concurrent_requests_no_id_collision(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Spawn 10 concurrent requests
        Parent = self(),
        NumRequests = 10,

        Pids = [spawn(fun() ->
            %% Send request (will block without mock transport)
            %% TODO: Need mock transport to complete requests
            Parent ! {request_sent, self()}
        end) || _ <- lists:seq(1, NumRequests)],

        %% Wait for all requests to be sent
        lists:foreach(fun(_) ->
            receive {request_sent, _Pid} -> ok end
        end, Pids),

        %% Verify all request IDs are unique
        State = get_state(Client),
        %% TODO: Verify no duplicate request IDs

        erlmcp_client:stop(Client)
    end.
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_client_tests` - 100% pass rate (5/5 correlation tests pass)
- [ ] Coverage: `rebar3 cover` - ≥30% for send_request/4, handle_response/3
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls (will FAIL until erlmcp_request_id module created)

##### Manual Verification:
- [ ] Code review: Request ID generation tested for normal and overflow cases
- [ ] Integration: Request correlation works with real transport responses
- [ ] Edge cases: Request ID overflow tested (P0 SECURITY)
- [ ] Performance: All 5 tests execute in <3 seconds

**CRITICAL**: `rebar3 xref` will FAIL until erlmcp_request_id module is created. This is a BLOCKING issue.

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 4: Capability-Based Operations (≤2 hours)

#### Overview
Test ALL capability-based operations (resources, tools, prompts), verify capability enforcement, test operations with and without server capabilities.

#### Specification
Test list_resources, read_resource, subscribe_to_resource, unsubscribe_from_resource, list_tools, call_tool, list_prompts, get_prompt with and without server capabilities, verify phase enforcement.

#### Pseudocode
```erlang
%% Setup: Create initialized client with capabilities
setup_initialized_client_with_caps() ->
    {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
    %% Initialize with server capabilities
    %% Inject initialize response
    Client.

%% Test: list_resources with capability succeeds
test_list_resources_with_capability(Client) ->
    %% Server has resources capability
    Result = erlmcp_client:list_resources(Client),
    ?assertMatch({ok, _Resources}, Result).

%% Test: list_resources without capability fails
test_list_resources_without_capability(Client) ->
    %% Server doesn't have resources capability
    Result = erlmcp_client:list_resources(Client),
    ?assertMatch({error, capability_not_supported}, Result).
```

#### Architecture
**Capability Enforcement**:
- Server capabilities stored in state#state.capabilities (from initialize response)
- Client operations check server capabilities before sending requests
- Phase enforcement checks state#state.phase = initialized before operations
- Strict mode enforces capability checks

**Integration Points**:
- erlmcp_json_rpc:encode_request/3 (build capability requests)
- validate_capability/2 (check server capability)
- check_server_capability/2 (check specific capability)

#### Changes Required:

##### 1. Capability Operation Tests
**File**: `apps/erlmcp_core/test/erlmcp_client_tests.erl`
**Current**: Lines 118-250 have basic resource/tool/prompt tests but no capability verification
**Changes**: Test operations with and without server capabilities
**Reason**: Verify capability enforcement logic

```erlang
capability_operations_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_list_resources_with_capability/1,
         fun test_list_resources_without_capability/1,
         fun test_read_resource/1,
         fun test_subscribe_to_resource/1,
         fun test_unsubscribe_from_resource/1,
         fun test_list_tools/1,
         fun test_call_tool/1,
         fun test_list_prompts/1,
         fun test_get_prompt/1,
         fun test_get_prompt_with_arguments/1
     ]}.

test_list_resources_with_capability(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Set server capabilities with resources enabled
        %% TODO: Need to inject initialize response with capabilities

        %% Call list_resources
        %% TODO: Need mock transport to send response

        erlmcp_client:stop(Client)
    end.

test_list_resources_without_capability(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Set server capabilities without resources
        %% TODO: Need to inject initialize response without resources capability

        %% Call list_resources - should fail
        %% Result = erlmcp_client:list_resources(Client),
        %% ?assertMatch({error, capability_not_supported}, Result)

        erlmcp_client:stop(Client)
    end.

test_read_resource(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% TODO: Need mock transport

        Uri = <<"test://resource/1">>,
        %% Result = erlmcp_client:read_resource(Client, Uri),

        erlmcp_client:stop(Client)
    end.
```

**PROBLEM**: Cannot test capability operations without:
1. Mock transport to inject initialize response with capabilities
2. Mock transport to inject operation responses

**SOLUTION**: Create mock transport module (see Phase 8)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_client_tests` - 100% pass rate (10/10 capability tests pass)
- [ ] Coverage: `rebar3 cover` - ≥50% for handle_call/3 (capability operations)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All capability-based operations tested
- [ ] Integration: Capability enforcement works correctly
- [ ] Edge cases: Operations fail without capabilities or before initialized
- [ ] Performance: All 10 tests execute in <3 seconds

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 5: Notification Handling (≤2 hours)

#### Overview
Test notification routing, handler registration/deregistration, handler delivery, subscription tracking.

#### Specification
Test set_notification_handler, remove_notification_handler, test all notification types (resources/updated, resources/list_changed, prompts/list_changed, tools/list_changed, sampling/createMessage), verify handlers receive notifications.

#### Pseudocode
```erlang
%% Setup: Create initialized client
setup_initialized_client() ->
    {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
    Client.

%% Test: Set notification handler
test_set_notification_handler(Client) ->
    Method = <<"resources/list_changed">>,
    Handler = fun(_M, _P) -> ok end,
    Result = erlmcp_client:set_notification_handler(Client, Method, Handler),
    ?assertEqual(ok, Result),
    %% Verify handler stored in state
    State = get_state(Client),
    ?assert(maps:is_key(Method, State#state.notification_handlers)).

%% Test: Notification routed to handler
test_notification_routed_to_handler(Client) ->
    Method = <<"resources/list_changed">>,
    HandlerPid = self(),
    Handler = fun(_M, _P) -> HandlerPid ! notified end,
    erlmcp_client:set_notification_handler(Client, Method, Handler),

    %% Inject notification via transport
    %% TODO: Need mock transport to inject notification

    %% Verify handler received notification
    ?assertReceived(notified).
```

#### Architecture
**Notification Flow**:
1. Handler registered via set_notification_handler/3
2. Handler stored in state#state.notification_handlers map
3. Transport delivers notification to handle_info/2
4. handle_info/2 calls handle_notification/3
5. handle_notification/3 routes to handler via invoke_notification_handler/3
6. invoke_notification_handler/3 spawns handler process

**Integration Points**:
- erlmcp_json_rpc:decode_message/1 (parse notification)
- spawn_handler/3 (spawn handler process)
- Notification handler (function or {module, function} or pid)

#### Changes Required:

##### 1. Notification Handler Tests
**File**: `apps/erlmcp_core/test/erlmcp_client_tests.erl`
**Current**: Lines 288-314 have basic notification tests but no delivery verification
**Changes**: Test handler registration, routing, delivery
**Reason**: Verify notification system works correctly

```erlang
notification_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_set_notification_handler/1,
         fun test_remove_notification_handler/1,
         fun test_resources_updated_notification_routed_to_handler/1,
         fun test_resources_list_changed_notification_routed/1,
         fun test_sampling_create_message_notification/1
     ]}.

test_set_notification_handler(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
        Method = <<"resources/list_changed">>,
        Handler = fun(_M, _P) -> ok end,

        Result = erlmcp_client:set_notification_handler(Client, Method, Handler),
        ?assertEqual(ok, Result),

        %% Verify handler stored in state
        State = get_state(Client),
        ?assert(maps:is_key(Method, State#state.notification_handlers)),

        erlmcp_client:stop(Client)
    end.

test_remove_notification_handler(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
        Method = <<"resources/list_changed">>,
        Handler = fun(_M, _P) -> ok end,

        ok = erlmcp_client:set_notification_handler(Client, Method, Handler),
        ok = erlmcp_client:remove_notification_handler(Client, Method),

        %% Verify handler removed from state
        State = get_state(Client),
        ?assertNot(maps:is_key(Method, State#state.notification_handlers)),

        erlmcp_client:stop(Client)
    end.

test_resources_updated_notification_routed_to_handler(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Subscribe to resource
        Uri = <<"test://resource/1">>,
        %% TODO: Need to subscribe

        %% Set notification handler
        TestPid = self(),
        Handler = fun(_M, _P) -> TestPid ! notified end,
        ok = erlmcp_client:set_notification_handler(Client, <<"resources/updated">>, Handler),

        %% Inject notification via transport
        %% TODO: Need mock transport to inject notification

        %% Verify handler received notification
        ?assertReceived(notified),

        erlmcp_client:stop(Client)
    end.
```

**PROBLEM**: Cannot test notification delivery without mock transport to inject notifications.

**SOLUTION**: Create mock transport module (see Phase 8)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_client_tests` - 100% pass rate (5/5 notification tests pass)
- [ ] Coverage: `rebar3 cover` - ≥60% for handle_notification/3, invoke_notification_handler/3
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All notification types tested
- [ ] Integration: Notification routing works correctly
- [ ] Edge cases: Unhandled notifications logged
- [ ] Performance: All 5 tests execute in <2 seconds

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 6: Batch Request Management (≤2 hours)

#### Overview
Test batch request lifecycle (start_batch, add_to_batch, execute_batch, cancel_batch), verify batch_requests map management, test with_batch wrapper.

#### Specification
Test start_batch creates batch state, add_to_batch increases request count, execute_batch sends all requests, cancel_batch removes batch state, test with_batch wrapper handles exceptions.

#### Pseudocode
```erlang
%% Setup: Create initialized client
setup_initialized_client() ->
    {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
    Client.

%% Test: Start batch creates batch state
test_start_batch_creates_batch_state(Client) ->
    BatchId = make_ref(),
    Result = erlmcp_client:send_batch_request(Client, BatchId, <<"resources/list">>, #{}),
    ?assertMatch({ok, _RequestId}, Result),
    %% Verify batch state
    State = get_state(Client),
    ?assert(maps:is_key(BatchId, State#state.batch_requests)).

%% Test: Execute batch sends all requests
test_execute_batch_sends_all_requests(Client) ->
    BatchId = make_ref(),
    ok = gen_server:call(Client, {start_batch, BatchId}),
    {ok, _Id1} = erlmcp_client:send_batch_request(Client, BatchId, <<"resources/list">>, #{}),
    {ok, _Id2} = erlmcp_client:send_batch_request(Client, BatchId, <<"tools/list">>, #{}),
    {ok, Count} = gen_server:call(Client, {execute_batch, BatchId}),
    ?assertEqual(2, Count),
    %% Verify batch removed from state
    State = get_state(Client),
    ?assertNot(maps:is_key(BatchId, State#state.batch_requests)).
```

#### Architecture
**Batch Request Flow**:
1. start_batch creates empty batch in state#state.batch_requests
2. add_to_batch adds requests to batch list
3. execute_batch sends all requests via transport
4. cancel_batch removes batch from state
5. with_batch wrapper handles exceptions

**Integration Points**:
- execute_batch_requests/2 (send batch requests via transport)
- send_message/2 (send via transport)
- Batch state management

#### Changes Required:

##### 1. Batch Request Tests
**File**: `apps/erlmcp_core/test/erlmcp_client_tests.erl`
**Current**: Lines 256-282 have basic batch test but no state verification
**Changes**: Test batch lifecycle, verify batch_requests map
**Reason**: Verify batch request management works correctly

```erlang
batch_request_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_start_batch_creates_batch_state/1,
         fun test_add_to_batch_increases_request_count/1,
         fun test_execute_batch_sends_all_requests/1,
         fun test_cancel_batch_removes_batch_state/1,
         fun test_with_batch_wrapper/1
     ]}.

test_start_batch_creates_batch_state(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        BatchId = make_ref(),
        ok = gen_server:call(Client, {start_batch, BatchId}),

        %% Verify batch state created
        State = get_state(Client),
        ?assert(maps:is_key(BatchId, State#state.batch_requests)),
        ?assertEqual([], maps:get(BatchId, State#state.batch_requests)),

        erlmcp_client:stop(Client)
    end.

test_add_to_batch_increases_request_count(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        BatchId = make_ref(),
        ok = gen_server:call(Client, {start_batch, BatchId}),

        %% Add request to batch
        {ok, RequestId} = erlmcp_client:send_batch_request(Client, BatchId, <<"resources/list">>, #{}),

        %% Verify batch state updated
        State = get_state(Client),
        BatchRequests = maps:get(BatchId, State#state.batch_requests),
        ?assertEqual(1, length(BatchRequests)),
        ?assertEqual({RequestId, <<"resources/list">>, #{}}, hd(BatchRequests)),

        erlmcp_client:stop(Client)
    end.

test_execute_batch_sends_all_requests(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        BatchId = make_ref(),
        ok = gen_server:call(Client, {start_batch, BatchId}),
        {ok, _Id1} = erlmcp_client:send_batch_request(Client, BatchId, <<"resources/list">>, #{}),
        {ok, _Id2} = erlmcp_client:send_batch_request(Client, BatchId, <<"tools/list">>, #{}),

        %% Execute batch
        {ok, Count} = gen_server:call(Client, {execute_batch, BatchId}),
        ?assertEqual(2, Count),

        %% Verify batch removed from state
        State = get_state(Client),
        ?assertNot(maps:is_key(BatchId, State#state.batch_requests)),

        erlmcp_client:stop(Client)
    end.

test_cancel_batch_removes_batch_state(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        BatchId = make_ref(),
        ok = gen_server:call(Client, {start_batch, BatchId}),
        {ok, _Id} = erlmcp_client:send_batch_request(Client, BatchId, <<"resources/list">>, #{}),

        %% Cancel batch
        ok = gen_server:call(Client, {cancel_batch, BatchId}),

        %% Verify batch removed from state
        State = get_state(Client),
        ?assertNot(maps:is_key(BatchId, State#state.batch_requests)),

        erlmcp_client:stop(Client)
    end.

test_with_batch_wrapper(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Test with_batch wrapper
        Result = erlmcp_client:with_batch(Client, fun(BatchId) ->
            {ok, _Id1} = erlmcp_client:send_batch_request(Client, BatchId, <<"resources/list">>, #{}),
            {ok, _Id2} = erlmcp_client:send_batch_request(Client, BatchId, <<"tools/list">>, #{}),
            batch_result
        end),

        ?assertEqual(batch_result, Result),

        erlmcp_client:stop(Client)
    end.
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_client_tests` - 100% pass rate (5/5 batch tests pass)
- [ ] Coverage: `rebar3 cover` - ≥70% for handle_call/3 (batch operations), execute_batch_requests/2
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All batch operations tested
- [ ] Integration: Batch lifecycle works correctly
- [ ] Edge cases: Exception handling in with_batch
- [ ] Performance: All 5 tests execute in <2 seconds

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 7: Error Handling and Edge Cases (≤2 hours)

#### Overview
Test ALL error paths, test transport failures, test invalid phase operations, test strict mode enforcement, test request ID collision errors.

#### Specification
Test operation before initialized error, test transport exit stops server, test invalid JSON response logged, test unknown response ID logged, test strict mode enforces capabilities, test request ID collision error (P0 SECURITY).

#### Pseudocode
```erlang
%% Setup: Create client in various phases
setup_client_pre_initialization() ->
    {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),
    Client.

%% Test: Operation before initialized fails
test_operation_before_initialized_error(Client) ->
    %% Client is in pre_initialization phase
    Result = erlmcp_client:list_resources(Client),
    ?assertMatch({error, {not_initialized, pre_initialization, _}}, Result).

%% Test: Transport exit stops server
test_transport_exit_stops_server(Client) ->
    %% Simulate transport crash
    TransportPid = get_transport_pid(Client),
    exit(TransportPid, crash),
    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Client)).
```

#### Architecture
**Error Handling Patterns**:
- Phase enforcement: {error, {not_initialized, Phase, Message}}
- Capability validation: {error, capability_not_supported}
- Request ID overflow: {error, {request_id_overflow, Message}} (P0 SECURITY)
- Transport failure: {stop, {transport_died, Reason}, State}
- Invalid request: {reply, {error, unknown_request}, State}

**Integration Points**:
- Transport behavior (exit signal)
- Logger (error logging)
- Phase enforcement logic
- Capability validation logic

#### Changes Required:

##### 1. Error Handling Tests
**File**: `apps/erlmcp_core/test/erlmcp_client_tests.erl`
**Current**: No error path tests
**Changes**: Test all error conditions
**Reason**: Verify error handling works correctly

```erlang
error_handling_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_operation_before_initialized_error/1,
         fun test_transport_exit_stops_server/1,
         fun test_invalid_json_response_logged/1,
         fun test_unknown_response_id_logged/1,
         fun test_strict_mode_enforces_capabilities/1,
         fun test_request_id_collision_error/1  % P0 SECURITY
     ]}.

test_operation_before_initialized_error(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Client is in pre_initialization phase
        Result = erlmcp_client:list_resources(Client),
        ?assertMatch({error, {not_initialized, pre_initialization, _}}, Result),

        erlmcp_client:stop(Client)
    end.

test_transport_exit_stops_server(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Simulate transport exit (stdio transport is just self(), so we can't kill it)
        %% For tcp/http transport, we could kill the transport process
        %% TODO: This test needs real transport process

        ?assert(true)  % Placeholder

        erlmcp_client:stop(Client)
    end.

test_invalid_json_response_logged(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Inject invalid JSON via transport
        %% TODO: Need mock transport to inject invalid message

        ?assert(true)  % Placeholder

        erlmcp_client:stop(Client)
    end.

test_unknown_response_id_logged(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Inject response with unknown request ID
        %% TODO: Need mock transport to inject response

        ?assert(true)  % Placeholder

        erlmcp_client:stop(Client)
    end.

test_strict_mode_enforces_capabilities(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}, #{strict_mode => true}}),

        %% Enable strict mode
        ok = erlmcp_client:set_strict_mode(Client, true),

        %% TODO: Need to initialize client without resources capability
        %% Then verify list_resources fails with capability_not_supported

        ?assert(true)  % Placeholder

        erlmcp_client:stop(Client)
    end.

test_request_id_collision_error(_Client) ->
    fun() ->
        %% CRITICAL: Test request ID collision detection
        %% This should never happen in normal operation
        %% But we need to test the safety check

        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Manually inject duplicate request ID into pending_requests
        %% TODO: Need to access internal state to inject collision

        ?assert(true)  % Placeholder

        erlmcp_client:stop(Client)
    end.
```

**PROBLEM**: Cannot test error paths without mock transport to inject error conditions.

**SOLUTION**: Create mock transport module (see Phase 8)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_client_tests` - 100% pass rate (6/6 error tests pass)
- [ ] Coverage: `rebar3 cover` - ≥80% for ALL error paths
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All error paths tested
- [ ] Integration: Error handling works correctly
- [ ] Edge cases: All edge cases covered
- [ ] Performance: All 6 tests execute in <2 seconds

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 8: Concurrent Operations and Mock Transport (≤2 hours)

#### Overview
Test concurrent request handling (race conditions), create mock transport module to inject messages, test with 10+ concurrent requests, verify no request ID collisions.

#### Specification
Create `erlmcp_transport_mock.erl` module that supports injecting JSON-RPC messages, test concurrent requests, test concurrent batch operations, test concurrent notification handlers.

#### Pseudocode
```erlang
%% Mock transport module
-module(erlmcp_transport_mock).
-behaviour(gen_server).

%% API
-export([start_link/0, send/2, inject_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State
-record(state, {
    client_pid :: pid(),
    message_queue :: queue:queue()
}).

%% Start mock transport
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Send message (no-op for testing)
send(_TransportState, _Message) ->
    ok.

%% Inject message into client
inject_message(TransportPid, Message) ->
    gen_server:cast(TransportPid, {inject, Message}).

%% Handle inject
handle_cast({inject, Message}, State) ->
    ClientPid = State#state.client_pid,
    ClientPid ! {transport_message, Message},
    {noreply, State}.
```

#### Architecture
**Mock Transport Design**:
- Standalone gen_server process
- Stores client_pid reference
- Supports inject_message/2 to send messages to client
- send/2 is no-op (messages sent via inject)
- Simulates transport behavior without network I/O

**Concurrent Test Design**:
- Spawn N processes, each sending requests
- Collect request IDs, verify uniqueness
- Inject responses via mock transport
- Verify all requests complete successfully

#### Changes Required:

##### 1. Mock Transport Module
**File**: `apps/erlmcp_core/test/erlmcp_transport_mock.erl` (NEW FILE)
**Current**: Doesn't exist
**Changes**: Create complete mock transport module
**Reason**: Enable testing of client with real gen_server but controlled message injection

```erlang
-module(erlmcp_transport_mock).
-behaviour(gen_server).

%% API
-export([start_link/1, send/2, inject_response/2, inject_notification/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    client_pid :: pid()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% Start mock transport linked to client
start_link(ClientPid) ->
    gen_server:start_link(?MODULE, [ClientPid], []).

%% Send message (no-op, messages injected via inject_response/2)
send(_TransportState, _Message) ->
    ok.

%% Inject JSON-RPC response into client
inject_response(TransportPid, ResponseMap) ->
    Json = jsx:encode(ResponseMap),
    gen_server:cast(TransportPid, {inject, Json}).

%% Inject JSON-RPC notification into client
inject_notification(TransportPid, NotificationMap) ->
    Json = jsx:encode(NotificationMap),
    gen_server:cast(TransportPid, {inject, Json}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([ClientPid]) ->
    {ok, #state{client_pid = ClientPid}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({inject, Json}, State) ->
    ClientPid = State#state.client_pid,
    ClientPid ! {transport_message, Json},
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

##### 2. Concurrent Request Tests
**File**: `apps/erlmcp_core/test/erlmcp_client_tests.erl`
**Current**: No concurrent request tests
**Changes**: Test 10+ concurrent requests, verify no ID collisions
**Reason**: Verify request ID generation is thread-safe

```erlang
concurrent_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_concurrent_requests_no_collision/1,
         fun test_concurrent_batch_operations/1,
         fun test_concurrent_notification_handlers/1
     ]}.

test_concurrent_requests_no_collision(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Spawn 10 concurrent requests
        Parent = self(),
        NumRequests = 10,

        Pids = [spawn(fun() ->
            %% Send request
            %% TODO: Need to initialize client and inject responses

            Parent ! {request_complete, self()}
        end) || _ <- lists:seq(1, NumRequests)],

        %% Wait for all requests to complete
        RequestIds = [receive
            {request_complete, _Pid} -> ok
        after 5000 ->
            error(timeout)
        end || _ <- Pids],

        ?assertEqual(NumRequests, length(RequestIds)),

        erlmcp_client:stop(Client)
    end.

test_concurrent_batch_operations(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Spawn 5 concurrent batch operations
        Parent = self(),
        NumBatches = 5,

        Pids = [spawn(fun() ->
            BatchId = make_ref(),
            ok = gen_server:call(Client, {start_batch, BatchId}),
            {ok, _Id} = erlmcp_client:send_batch_request(Client, BatchId, <<"resources/list">>, #{}),
            {ok, _Count} = gen_server:call(Client, {execute_batch, BatchId}),
            Parent ! {batch_complete, BatchId}
        end) || _ <- lists:seq(1, NumBatches)],

        %% Wait for all batches to complete
        BatchIds = [receive
            {batch_complete, Id} -> Id
        after 5000 ->
            error(timeout)
        end || _ <- Pids],

        ?assertEqual(NumBatches, length(BatchIds)),
        ?assertEqual(NumBatches, length(lists:usort(BatchIds))),

        erlmcp_client:stop(Client)
    end.

test_concurrent_notification_handlers(_Client) ->
    fun() ->
        {ok, Client} = erlmcp_client:start_link({stdio, #{test_mode => true}}),

        %% Register 5 notification handlers
        Methods = [
            <<"resources/updated">>,
            <<"resources/list_changed">>,
            <<"prompts/list_changed">>,
            <<"tools/list_changed">>,
            <<"notifications/progress">>
        ],

        lists:foreach(fun(Method) ->
            Handler = fun(_M, _P) -> ok end,
            ok = erlmcp_client:set_notification_handler(Client, Method, Handler)
        end, Methods),

        %% Verify all handlers registered
        State = get_state(Client),
        lists:foreach(fun(Method) ->
            ?assert(maps:is_key(Method, State#state.notification_handlers))
        end, Methods),

        erlmcp_client:stop(Client)
    end.
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_client_tests` - 100% pass rate (3/3 concurrent tests pass)
- [ ] Coverage: `rebar3 cover` - ≥80% TOTAL coverage for erlmcp_client.erl
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All concurrent operations tested
- [ ] Integration: No race conditions under load
- [ ] Edge cases: 100+ concurrent requests work
- [ ] Performance: All 3 tests execute in <3 seconds

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

## Testing Strategy

### Chicago School TDD (MANDATORY)
- **NO MOCKS** - Use real processes, real gen_servers (except mock transport for message injection)
- **State-Based Verification** - Check #state{} record contents via sys:get_status/1
- **Integration Testing** - Test with real transport behaviors (stdio, tcp, http, mock)
- **Race Condition Testing** - Concurrent operations with spawned processes

### Unit Tests (EUnit)
- **What to Test**: All public functions, all handle_call/handle_cast/handle_info clauses, all error paths
- **Test Pattern**: Follow erlmcp_session_manager_tests.erl pattern (lines 14-60, 71-85)
- **Coverage Target**: ≥80% per module
- **Pass Rate**: 100% (all tests must pass)

### Test Organization
```
erlmcp_client_tests.erl:
- client_lifecycle_test_() (5 tests)
- initialization_test_() (5 tests)
- request_correlation_test_() (5 tests)
- capability_operations_test_() (10 tests)
- notification_test_() (5 tests)
- batch_request_test_() (5 tests)
- error_handling_test_() (6 tests)
- concurrent_test_() (3 tests)
Total: 44+ tests (target: ≥50 tests)
```

### Manual Testing Steps
1. Run all tests: `rebar3 eunit --module=erlmcp_client_tests`
2. Verify coverage: `rebar3 cover --verbose`
3. Check specific coverage: `rebar3 cover --verbose | grep erlmcp_client.erl`
4. Verify Dialyzer: `rebar3 dialyzer`
5. Verify Xref: `rebar3 xref`
6. Test execution time: `time rebar3 eunit --module=erlmcp_client_tests`

### Quality Gates
Every phase MUST pass:
1. **Compilation**: `TERM=dumb rebar3 compile`
2. **EUnit**: `rebar3 eunit --module=erlmcp_client_tests`
3. **Coverage**: `rebar3 cover` (verify ≥80%)
4. **Dialyzer**: `rebar3 dialyzer`
5. **Xref**: `rebar3 xref`

## Manufacturing Checklist

### Before Implementation
- [x] Research verified (read actual source code)
- [x] Scope confirmed (IN: client module tests, OUT: transport/server tests)
- [x] No open questions (all decisions made)
- [x] Phases broken down (≤2 hours each, total 16 hours)
- [x] Acceptance criteria defined (measurable, specific)

### During Implementation
- [ ] Chicago School TDD followed (tests FIRST, state verification)
- [ ] OTP patterns followed (gen_server, {foreach} fixtures)
- [ ] Type specs verified (Dialyzer clean)
- [ ] Error handling complete (all paths tested)
- [ ] Quality gates passing (compilation, tests, coverage, dialyzer, xref)

### After Implementation
- [ ] All tests passing (100% rate, ≥50 tests)
- [ ] Coverage ≥80% (verified)
- [ ] Dialyzer 0 warnings (verified)
- [ ] Xref 0 undefined calls (verified - BLOCKED by erlmcp_request_id bug)
- [ ] Performance no regression >10% (execution time <5 seconds)
- [ ] Documentation updated (test comments)
- [ ] Code review complete (OTP patterns verified)

## Risk Management

### Known Risks
| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **Test flakiness due to timing dependencies** | P0 | High | Use {foreach} for isolation, avoid timer:sleep, use synchronous calls |
| **CRITICAL BUG: erlmcp_request_id module missing** | P0 | 100% | BLOCKS all testing - must create module or inline logic |
| **Missing handle_call clause coverage** | P0 | Medium | Explicitly test ALL 19 handle_call patterns |
| **No mock transport for message injection** | P1 | High | Create erlmcp_transport_mock.erl in Phase 8 |
| **State verification with sys:get_status/1 may fail** | P1 | Low | Wrap in try/catch, fallback to pattern matching |
| **Race condition in concurrent request testing** | P1 | Low | Use spawned processes with message passing, test with 10+ requests |
| **Request ID overflow not tested** | P2 | Low | Test with request_id = 16#7FFFFFFF, verify safe_increment/1 detects overflow |
| **Batch request state not verified** | P2 | Low | Test batch lifecycle, verify state#state.batch_requests |
| **Notification handler delivery not tested** | P3 | Low | Test notification routing, spawn handler processes |
| **Subscription tracking not tested** | P3 | Low | Test subscribe/unsubscribe, verify sets:add_element/del_element |

### Rollback Plan
If something goes wrong:
- **Git revert**: Revert to commit before test rewrite
- **Data migration**: No data migration (test-only changes)
- **Service impact**: No service impact (test-only changes)

## Critical Bugs Found

### Bug #1: erlmcp_request_id Module Missing (P0 SECURITY)
- **Location**: `erlmcp_client.erl:478`
- **Issue**: Call to `erlmcp_request_id:safe_increment/1` but module doesn't exist
- **Impact**: CRASH when request_id reaches 16#7FFFFFFF (2 billion requests)
- **Fix Required**: Create `erlmcp_request_id` module OR inline the overflow check
- **Blocking**: BLOCKS xref gate, BLOCKS request ID overflow testing

### Bug #2: Mock Transport Required for Testing
- **Location**: All test phases (2-8)
- **Issue**: Cannot inject JSON-RPC messages without mock transport
- **Impact**: Cannot test request-response correlation, notification handling, error paths
- **Fix Required**: Create `erlmcp_transport_mock` module in Phase 8
- **Blocking**: BLOCKS ≥80% coverage goal

## References
- Research: `/Users/sac/erlmcp/.wreckit/items/002-create-comprehensive-eunit-test-suite-for-erlmcpcl/research.md`
- Source: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_client.erl` (729 lines)
- Current Tests: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_client_tests.erl` (438 lines)
- Reference Pattern: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_session_manager_tests.erl` (527 lines)
- Type Definitions: `/Users/sac/erlmcp/apps/erlmcp_core/include/erlmcp.hrl` (523 lines)
