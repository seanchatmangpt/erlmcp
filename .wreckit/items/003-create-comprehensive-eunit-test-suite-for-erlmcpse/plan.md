# Create comprehensive EUnit test suite for erlmcp_server.erl Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Create a comprehensive EUnit test suite for `erlmcp_server.erl` to achieve ≥80% code coverage with ≥60 tests, covering ALL gen_server callbacks, ALL JSON-RPC request handlers, ALL error paths, and ALL resource/tool/prompt management operations. The server is a 1562-line gen_server that handles MCP protocol messages, manages resources/tools/prompts, and coordinates with the registry for message routing. Current test coverage is approximately 5% (only 13 basic lifecycle tests exist).

### Quality Gate Requirements

- **Compilation**: 0 errors (MANDATORY) - `TERM=dumb rebar3 compile`
- **EUnit**: 100% pass rate (MANDATORY) - `rebar3 eunit --module=erlmcp_server_tests`
- **Coverage**: ≥80% (MANDATORY) - `rebar3 cover --module=erlmcp_server`
- **Dialyzer**: 0 warnings (MANDATORY) - `rebar3 dialyzer`
- **Xref**: 0 undefined function calls (MANDATORY) - `rebar3 xref`
- **Performance**: No regression >10% from baseline

## Current State

### What Exists Now

**Modules:**
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl:1-1562` - Main server gen_server (1562 lines)
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_tests.erl:1-317` - Existing test file (317 lines, 13 tests only)
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server_sup.erl:1-47` - Supervisor (simple_one_for_one)
- `/Users/sac/erlmcp/include/erlmcp.hrl:1-523` - Protocol constants and type definitions

**Tests:**
- Coverage: ~5% (13 basic tests)
- Test counts: 13 total tests across 5 test generators
  - `server_lifecycle_test_()`: 3 tests (start_link, stop, with_capabilities)
  - `resource_test_()`: 3 tests (add_resource, add_resource_template, resource_handler)
  - `tool_test_()`: 3 tests (add_tool, add_tool_with_schema, tool_handler)
  - `prompt_test_()`: 3 tests (add_prompt, add_prompt_with_args, prompt_handler)
  - `notification_test_()`: 2 tests (notify_resource_updated, notify_resources_changed)

**Quality:**
- **FAILING** - Current test coverage is ~5%, far below 80% threshold
- **FAILING** - Critical code paths untested:
  - JSON-RPC request routing (handle_request/5 with 16 method clauses at lines 449-716)
  - Resource/tool/prompt deletion operations (lines 303-331)
  - Subscription/unsubscription handling (lines 333-341)
  - Error handling paths (invalid URIs, missing handlers, validation failures)
  - Concurrent access scenarios
  - Handler execution with real data
  - Registry integration (message routing via erlmcp_registry)
  - Initialization state machine (phase enforcement, double-initialize rejection)
  - Task operations (create, list, get, result, cancel)

### What's Missing

**Gap:** 75 percentage points of coverage (from 5% to 80%), requiring ≥47 additional comprehensive tests

**Root Cause:** Missing Poka-yoke (mistake-proofing) in development workflow - No automated quality gate preventing commits with <80% coverage. Initial development focused on feature completion over test coverage; quality gate enforcement was deferred.

**Impact:**
- **BLOCKS ALL PRODUCTION WORK** (Priority: P0, Urgency: P0)
- Any server crash takes down ALL MCP connections
- Server is completely untested for critical paths (JSON-RPC routing, error handling)
- Catastrophic risk of production crashes due to untested code paths

### Key Discoveries from Research

**Finding 1: OTP gen_server Pattern**
- Standard gen_server with callbacks (init/1 at line 170, handle_call/3 at line 207, handle_cast/2 at line 348, handle_info/2 at line 372, terminate/2 at line 428)
- State record with phase tracking (initialization → initialized → disconnected → closed) at line 50
- 14 handle_call clauses (add_resource, add_tool, add_prompt, delete_*, subscribe_*, etc.)
- 4 handle_cast clauses (report_progress, notify_resource_updated, notify_resources_changed)
- 5 handle_info clauses (mcp_message routing, task messages)

**Finding 2: JSON-RPC Request Routing**
- handle_request/5 function at line 441 with 16 method clauses covering:
  - initialize (line 449), double-initialize rejection (line 496), pre-init RPC rejection (line 508)
  - resources/list (line 520), resources/read (line 525), resources/templates/list (line 667)
  - resources/subscribe (line 672), resources/unsubscribe (line 682)
  - tools/list (line 553), tools/call (line 564)
  - prompts/list (line 693), prompts/get (line 704)
  - tasks/* (line 596-665: create, list, get, result, cancel)
  - Invalid method error (line 714)

**Finding 3: Handler Execution Pattern**
- Resource handler execution at line 859 (handle_read_resource/4)
- Tool handler execution at line 918 (handle_tool_call/4) with progress token generation at line 935
- Prompt handler execution at line 989 (handle_get_prompt/4) with argument validation at line 1074
- All handlers use try/catch for error handling and call safe wrapper functions

**Finding 4: Registry Integration**
- Message routing via erlmcp_registry:route_to_transport/3 at lines 729, 738, 744, 751, 760
- Uses gproc for process registration and discovery
- Server-to-transport binding via register_server/3 and register_transport/3

**Finding 5: Tracing Integration**
- OpenTelemetry tracing via erlmcp_tracing module at lines 171-202, 373-406
- Span creation for server operations (init, handle_mcp_message, handle_resources_read, etc.)
- Exception recording and error details logging

## Desired End State

### Specification

**What we're building:**

A comprehensive EUnit test suite for `erlmcp_server.erl` that covers:

1. **All gen_server callbacks:**
   - init/1: State initialization, task manager registration, notifier startup
   - handle_call/3: All 14 clauses (add_resource, add_tool, add_prompt, delete_*, subscribe_*)
   - handle_cast/2: All 4 clauses (report_progress, notify_resource_updated, notify_resources_changed)
   - handle_info/2: All 5 clauses (mcp_message routing, task_execute, task_status_update, task_cancel)
   - terminate/2: Task manager cleanup
   - code_change/3: State migration

2. **All JSON-RPC request handlers (handle_request/5):**
   - Initialize flow (normal, double-initialize, pre-init rejection)
   - Resources operations (list, read, templates/list, subscribe, unsubscribe)
   - Tools operations (list, call with progress token)
   - Prompts operations (list, get with argument validation)
   - Tasks operations (create, list, get, result, cancel)
   - Invalid method error handling

3. **All error paths:**
   - Missing parameters (uri, name, arguments, taskId)
   - Not found errors (resource, tool, prompt, task)
   - Validation errors (invalid URI format, malformed schemas, argument validation)
   - Handler crashes (caught and returned as internal errors)
   - Safe wrapper failures (send_*_safe functions catch exceptions)

4. **All resource/tool/prompt management operations:**
   - Add operations (with URI validation, schema validation)
   - Delete operations (with not_found handling)
   - Subscription/unsubscription (with state consistency)
   - Handler execution (with side effect verification using meck)

5. **Concurrent access scenarios:**
   - Multiple simultaneous add_resource/add_tool calls
   - Race conditions in subscribe/unsubscribe
   - State consistency under concurrent load

6. **Registry integration:**
   - Mock erlmcp_registry:route_to_transport/3 calls
   - Verify JSON-RPC responses are routed correctly
   - Test transport ID correlation

### Verification

**Automated verification:**
```bash
# Compilation
TERM=dumb rebar3 compile  # 0 errors, 0 warnings

# EUnit tests
rebar3 eunit --module=erlmcp_server_tests  # 100% pass rate, ≥60 tests

# Coverage
rebar3 cover --module=erlmcp_server  # ≥80% coverage

# Dialyzer
rebar3 dialyzer  # 0 warnings

# Xref
rebar3 xref  # 0 undefined function calls
```

**Manual verification:**
- Review coverage report to identify untested lines
- Ensure all handle_call/handle_cast/handle_info/handle_request clauses are covered
- Verify all error paths are tested
- Verify handler execution is verified (not just called)

**Metrics:**
- Coverage percentage: ≥80% (target)
- Test count: ≥60 tests (target)
- Test execution time: <5 seconds for unit tests
- Memory usage: No leaks in test processes

### Manufacturing Output

**Code:**
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_tests.erl` - Modified/expanded (from 317 lines to ~2000+ lines)

**Tests:**
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_tests.erl` - Comprehensive test suite with ≥60 tests

**Documentation:**
- None (no new documentation required for test expansion)

**Receipts:**
- Coverage report: `_build/test/cover/erlmcp_server.COVER.html`
- EUnit log: `_build/test/logs/eunit.log`

## What We're NOT Doing

**Explicitly OUT OF SCOPE:**

1. **Common Test suites** - This item is for EUnit unit tests only, not integration tests
2. **Property-based testing with Proper** - Out of scope for this item (can be added later)
3. **Performance/benchmark tests** - Out of scope (requires separate performance testing framework)
4. **Fuzz testing** - Out of scope (requires external fuzzing tools)
5. **Regression test suite** - Out of scope (this is unit tests only)
6. **Documentation updates** - No API documentation changes required
7. **Refactoring erlmcp_server.erl** - We are testing existing code, not modifying it

**Reason for scope limitation:** Focus on achieving ≥80% code coverage for the existing server module using EUnit. Additional test types (CT, Proper, performance) are separate work items that can be addressed after core unit test coverage is achieved.

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** - Requirements with acceptance criteria (≥80% coverage, ≥60 tests, all code paths)
2. **Pseudocode** - Test structure design BEFORE coding (organize tests by functionality: lifecycle, resources, tools, prompts, requests, error handling, concurrency)
3. **Architecture** - Integration points identified (registry, json_rpc, tracing, validators, task_manager)
4. **Refinement** - Chicago School TDD (write tests FIRST, run to fail, implement test infrastructure to pass)
5. **Completion** - All quality gates passing (compile, eunit, coverage, dialyzer, xref)

### Implementation Strategy

**High-level approach:**

The test suite will use **Chicago School TDD** principles:
- **NO MOCKS** for gen_servers - Use real processes, real gen_servers
- **State-based verification** - Check #state{} record contents via sys:get_state/1
- **Integration testing** - Test with real dependencies (registry, task_manager, etc.)
- **Race condition testing** - Concurrent operations with multiple processes
- **meck for side effects** - Mock external module calls (erlmcp_registry, erlmcp_tracing, erlmcp_json_rpc) only when necessary

**Test organization:**
```
erlmcp_server_tests.erl
├── Setup/Cleanup fixtures
├── Lifecycle Tests (init, terminate, code_change)
├── Resource Management Tests (add, delete, subscribe, execute)
├── Tool Management Tests (add, delete, call with progress)
├── Prompt Management Tests (add, delete, get with validation)
├── JSON-RPC Request Routing Tests (initialize, resources, tools, prompts, tasks)
├── Error Handling Tests (missing params, not_found, validation failures)
├── Concurrent Access Tests (multiple clients, race conditions)
└── Registry Integration Tests (message routing, transport correlation)
```

**Why this strategy:**
1. **Chicago School TDD** provides realistic testing (real processes, not mocked artifacts)
2. **State-based verification** ensures internal state correctness (not just return values)
3. **meck for side effects** allows verification of external calls without modifying production code
4. **Organized by functionality** makes tests maintainable and understandable
5. **Small, incremental phases** align with Heijunka (production leveling)

### Quality Integration

**Pre-commit Hooks:**
- `.claude/hooks/pre-task-validate.sh` - Runs compilation, EUnit, and coverage checks before allowing commits

**CI Gates:**
- Compilation must pass (0 errors)
- EUnit must pass (100% pass rate)
- Coverage must be ≥80%
- Dialyzer must pass (0 warnings)
- Xref must pass (0 undefined calls)

**Receipt Generation:**
- Coverage HTML report: `_build/test/cover/erlmcp_server.COVER.html`
- EUnit log: `_build/test/logs/eunit.log`

**Andon Signaling:**
- All failures reported with context (test name, line number, assertion details)
- Coverage report shows uncovered lines (red highlighting in HTML)
- CI/CD pipeline fails immediately if any gate fails (Jidoka - stop the line)

---

## Phases

### Phase 1: Foundation - Lifecycle and State Management (≤4 hours)

#### Overview

Establish test infrastructure and verify basic gen_server lifecycle, state initialization, and phase transitions. This phase creates the setup/cleanup fixtures that all subsequent tests will use.

#### Specification

**WHAT we're building:**

1. **Setup/cleanup fixtures:**
   - `setup/0` - Start erlmcp_core application and erlmcp_registry
   - `cleanup/1` - Stop test processes and verify cleanup
   - `setup_server/0` - Start a test server instance with default capabilities
   - `cleanup_server/1` - Stop server and verify cleanup

2. **Lifecycle tests:**
   - `test_init/0` - Verify state initialization (server_id, phase=initialization, capabilities, empty maps)
   - `test_terminate/0` - Verify task manager cleanup and logging
   - `test_code_change/0` - Verify state migration (old state → new state)

3. **Phase management tests:**
   - `test_initialization_phase/0` - Verify server starts in initialization phase
   - `test_initialized_phase_transition/0` - Verify phase transitions to initialized after initialize request
   - `test_phase_state_consistency/0` - Verify phase field is immutable via handle_call (only changes via handle_info)

#### Pseudocode

**Setup/cleanup fixture design:**
```erlang
setup() ->
    % Start required applications
    ok = application:ensure_all_started(erlmcp_core),
    % Start registry if not already started
    {ok, _RegistryPid} = erlmcp_registry:start_link(),
    % Stub tracing to avoid OpenTelemetry overhead in tests
    ok = meck:new(erlmcp_tracing, [passthrough]),
    ok = meck:expect(erlmcp_tracing, start_server_span, fun(_, _) -> {span, fake} end),
    ok = meck:expect(erlmcp_tracing, set_attributes, fun(_, _) -> ok end),
    ok = meck:expect(erlmcp_tracing, set_status, fun(_, _) -> ok end),
    ok = meck:expect(erlmcp_tracing, end_span, fun(_) -> ok end),
    ok = meck:expect(erlmcp_tracing, record_exception, fun(_, _, _, _) -> ok end),
    ok = meck:expect(erlmcp_tracing, record_error_details, fun(_, _, _) -> ok end),
    ok = meck:expect(erlmcp_tracing, log, fun(_, _) -> ok end),
    {registry_started}.

cleanup({registry_started}) ->
    % Unstub tracing
    meck:unload(erlmcp_tracing),
    % Stop registry
    ok = gen_server:stop(erlmcp_registry),
    % Stop application
    application:stop(erlmcp_core),
    ok.

setup_server() ->
    {registry_started} = setup(),
    ServerId = test_server_lifecycle,
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{}
    },
    {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities),
    {ServerPid, ServerId, Capabilities}.

cleanup_server({ServerPid, _ServerId, _Capabilities}) ->
    ok = erlmcp_server:stop(ServerPid),
    timer:sleep(100),  % Allow async cleanup
    cleanup({registry_started}).
```

**Test design:**
```erlang
test_init() ->
    % Verify state via sys:get_state
    State = sys:get_state(ServerPid),
    ?assertEqual(test_server_lifecycle, State#state.server_id),
    ?assertEqual(initialization, State#state.phase),
    ?assertEqual(false, State#state.initialized),
    ?assertEqual(#{}, State#state.resources),
    ?assertEqual(#{}, State#state.tools),
    ?assertEqual(#{}, State#state.prompts).

test_terminate() ->
    % Verify task manager is called during terminate
    meck:new(erlmcp_task_manager, [passthrough]),
    ok = meck:expect(erlmcp_task_manager, unregister_server, fun(_) -> ok end),
    erlmcp_server:stop(ServerPid),
    ?assert(meck:called(erlmcp_task_manager, unregister_server,['_'])),
    meck:unload(erlmcp_task_manager).
```

#### Architecture

**INTEGRATION - Supervision tree:**
```
erlmcp_registry (gen_server)
└── erlmcp_server (gen_server, supervised by erlmcp_server_sup)
    └── Uses: erlmcp_task_manager (for task operations)
    └── Uses: erlmcp_change_notifier (for list change notifications)
    └── Uses: erlmcp_tracing (for OpenTelemetry spans)
```

**Process structure:**
- Test process spawns erlmcp_server gen_server
- erlmcp_server registers with erlmcp_registry (via erlmcp_task_manager:register_server/2)
- Test process verifies state via sys:get_state/1
- Test process stops server via erlmcp_server:stop/1

**Dependencies:**
- erlmcp_registry - Message routing (mocked via meck)
- erlmcp_task_manager - Task lifecycle (mocked via meck)
- erlmcp_tracing - OpenTelemetry spans (mocked via meck)
- erlmcp_change_notifier - List change notifications (mocked via meck)

#### Changes Required:

##### 1. Test file expansion
**File:** `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_tests.erl`
**Current:** 317 lines, 13 basic tests
**Changes:** Add comprehensive test generators for lifecycle, resources, tools, prompts, requests, error handling, concurrency
**Reason:** Achieve ≥80% coverage, ≥60 tests

**New test sections:**
```erlang
%%====================================================================
%% Lifecycle Tests
%%====================================================================

lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(test_init_state()),
         ?_test(test_terminate_cleanup()),
         ?_test(test_code_change_migration()),
         ?_test(test_initialization_phase()),
         ?_test(test_initialized_phase_transition()),
         ?_test(test_phase_state_consistency())
     ]end}.

test_init_state() ->
    {_ServerPid, ServerId, _Capabilities} = setup_server(),
    State = sys:get_state(_ServerPid),
    ?assertEqual(ServerId, State#state.server_id),
    ?assertEqual(initialization, State#state.phase),
    ?assertEqual(false, State#state.initialized),
    ?assertEqual(#{}, State#state.resources),
    ?assertEqual(#{}, State#state.tools),
    ?assertEqual(#{}, State#state.prompts),
    ?assert(is_pid(State#state.notifier_pid)),
    cleanup_server({_ServerPid, ServerId, _Capabilities}).
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_server_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - Lifecycle paths covered (init/1, terminate/2, code_change/3)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: OTP patterns followed correctly (gen_server lifecycle)
- [ ] Integration: Works with erlmcp_registry and erlmcp_task_manager
- [ ] Edge cases: State initialization verified (not just return values)

**Note:** Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix. This is Jidoka (built-in quality).

---

### Phase 2: Resource Management (≤4 hours)

#### Overview

Test all resource management operations: add_resource, add_resource_template, delete_resource, subscribe_resource, unsubscribe_resource, and resource handler execution.

#### Specification

**WHAT we're building:**

1. **Add resource tests:**
   - `test_add_resource_success/0` - Add valid resource, verify state update and list change notification
   - `test_add_resource_invalid_uri/0` - Add resource with invalid URI, verify error response
   - `test_add_resource_duplicate/0` - Add duplicate resource, verify overwrites existing

2. **Add resource template tests:**
   - `test_add_resource_template_success/0` - Add valid template, verify state update
   - `test_add_resource_template_invalid_uri/0` - Add template with invalid URI format, verify error

3. **Delete resource tests:**
   - `test_delete_resource_success/0` - Delete existing resource, verify removal from state
   - `test_delete_resource_not_found/0` - Delete non-existent resource, verify {error, not_found}

4. **Subscription tests:**
   - `test_subscribe_resource_success/0` - Subscribe to resource, verify subscription added
   - `test_unsubscribe_resource_success/0` - Unsubscribe from resource, verify removal
   - `test_notify_subscribers/0` - Notify resource updated, verify subscribers receive notification

5. **Resource handler execution tests:**
   - `test_handle_read_resource_success/0` - Read existing resource, verify handler called and content returned
   - `test_handle_read_resource_not_found/0` - Read non-existent resource, verify error response
   - `test_handle_read_resource_handler_crash/0` - Handler crashes, verify internal error returned

#### Pseudocode

**Test design:**
```erlang
test_add_resource_success() ->
    {ServerPid, _ServerId, _Capabilities} = setup_server(),
    Uri = <<"test://resource/1">>,
    Resource = #mcp_resource{
        uri = Uri,
        name = <<"Test Resource">>,
        description = <<"A test resource">>,
        mime_type = <<"text/plain">>
    },
    Handler = fun(_Uri) -> <<"resource content">> end,

    % Add resource
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    % Verify state update
    State = sys:get_state(ServerPid),
    ?assert(maps:is_key(Uri, State#state.resources)),
    {{StoredResource, _StoredHandler}, _} = {maps:get(Uri, State#state.resources), Uri},
    ?assertEqual(Uri, StoredResource#mcp_resource.uri),

    cleanup_server({ServerPid, _ServerId, _Capabilities}).

test_add_resource_invalid_uri() ->
    {ServerPid, _ServerId, _Capabilities} = setup_server(),
    % Mock URI validator to return error
    meck:new(erlmcp_uri_validator, [passthrough]),
    ok = meck:expect(erlmcp_uri_validator, validate_resource_uri_on_registration,
        fun(<<"invalid://">>) -> {error, {invalid_uri_scheme, <<"invalid://">>}} end),

    Uri = <<"invalid://">>,
    Handler = fun(_Uri) -> <<"content">> end,

    % Attempt to add invalid resource
    Result = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    % Verify error response
    ?assertMatch({error, {-32602, _, #{<<"error_type">> := _}}}, Result),

    meck:unload(erlmcp_uri_validator),
    cleanup_server({ServerPid, _ServerId, _Capabilities}).
```

#### Architecture

**INTEGRATION - Handler verification:**
- Use meck to mock erlmcp_uri_validator for URI validation tests
- Use real fun handlers for execution tests (Chicago School)
- Use meck to mock erlmcp_registry:route_to_transport/3 for response verification

**Dependencies:**
- erlmcp_uri_validator - URI validation (mocked via meck)
- erlmcp_registry - Response routing (mocked via meck)
- erlmcp_change_notifier - List change notifications (mocked via meck)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_server_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - Resource management paths covered (handle_call clauses 207-243, 303-311, 333-341)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: Handler execution verified (not just state updates)
- [ ] Integration: URI validation errors tested
- [ ] Edge cases: Handler crashes, invalid URIs, duplicate resources

---

### Phase 3: Tool Management (≤4 hours)

#### Overview

Test all tool management operations: add_tool, add_tool_with_schema, delete_tool, and tool handler execution with progress token tracking.

#### Specification

**WHAT we're building:**

1. **Add tool tests:**
   - `test_add_tool_success/0` - Add valid tool, verify state update and list change notification
   - `test_add_tool_with_schema_success/0` - Add tool with schema, verify schema stored
   - `test_delete_tool_success/0` - Delete existing tool, verify removal from state
   - `test_delete_tool_not_found/0` - Delete non-existent tool, verify {error, not_found}

2. **Tool handler execution tests:**
   - `test_handle_tool_call_success/0` - Call existing tool, verify handler executed and progress token generated
   - `test_handle_tool_call_not_found/0` - Call non-existent tool, verify error response
   - `test_handle_tool_call_handler_crash/0` - Handler crashes, verify internal error returned and progress cleanup
   - `test_progress_token_generation/0` - Verify unique progress tokens generated for concurrent calls

3. **Tool description validation tests:**
   - `test_validate_tool_description_success/0` - Valid description passes validation
   - `test_validate_tool_description_too_long/0` - Description exceeds max length, verify error

#### Pseudocode

**Test design:**
```erlang
test_handle_tool_call_success() ->
    {ServerPid, _ServerId, _Capabilities} = setup_server(),
    ToolName = <<"test_tool">>,
    Tool = #mcp_tool{
        name = ToolName,
        description = <<"A test tool">>,
        input_schema = #{type => <<"object">>}
    },
    Handler = fun(Args) ->
        Input = maps:get(<<"input">>, Args, <<"">>),
        <<"Processed: ", Input/binary>>
    end,

    % Add tool
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    % Mock registry to capture response
    meck:new(erlmcp_registry, [passthrough]),
    meck:expect(erlmcp_registry, route_to_transport, fun(_, _, _) -> ok end),

    % Mock progress tracking
    meck:new(erlmcp_progress, [passthrough]),
    meck:expect(erlmcp_progress, generate_token, fun() -> <<"token_123">> end),
    meck:expect(erlmcp_progress, track_tool_call, fun(_, _, _) -> ok end),
    meck:expect(erlmcp_progress, cleanup_completed, fun(_) -> ok end),

    % Send tool call request via handle_info
    Request = #json_rpc_request{
        id = 1,
        method = <<"tools/call">>,
        params = #{name => ToolName, arguments => #{input => <<"test">>}}
    },
    {ok, RequestJson} = erlmcp_json_rpc:encode_request(Request),
    ServerPid ! {mcp_message, test_transport, RequestJson},
    timer:sleep(100),

    % Verify handler called
    ?assert(meck:called(erlmcp_progress, track_tool_call, [<<"token_123">>, ToolName, _])),

    % Verify response sent via registry
    ?assert(meck:called(erlmcp_registry, route_to_transport, [test_transport, _, _])),

    meck:unload(erlmcp_progress),
    meck:unload(erlmcp_registry),
    cleanup_server({ServerPid, _ServerId, _Capabilities}).
```

#### Architecture

**INTEGRATION - Progress tracking:**
- Use meck to mock erlmcp_progress for progress token generation and tracking
- Use meck to mock erlmcp_registry:route_to_transport/3 for response verification
- Use real fun handlers for execution tests (Chicago School)

**Dependencies:**
- erlmcp_progress - Progress token generation (mocked via meck)
- erlmcp_registry - Response routing (mocked via meck)
- erlmcp_change_notifier - List change notifications (mocked via meck)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_server_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - Tool management paths covered (handle_call clauses 245-262, 313-321)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: Tool handler execution verified (not just state updates)
- [ ] Integration: Progress token tracking tested
- [ ] Edge cases: Handler crashes, progress token cleanup, tool description validation

---

### Phase 4: Prompt Management (≤4 hours)

#### Overview

Test all prompt management operations: add_prompt, add_prompt_with_args, add_prompt_with_args_and_schema, delete_prompt, and prompt handler execution with argument validation.

#### Specification

**WHAT we're building:**

1. **Add prompt tests:**
   - `test_add_prompt_success/0` - Add valid prompt, verify state update and notification
   - `test_add_prompt_with_args_success/0` - Add prompt with arguments, verify arguments stored
   - `test_add_prompt_with_args_and_schema_success/0` - Add prompt with args and schema, verify schema stored
   - `test_delete_prompt_success/0` - Delete existing prompt, verify removal from state
   - `test_delete_prompt_not_found/0` - Delete non-existent prompt, verify {error, not_found}

2. **Prompt handler execution tests:**
   - `test_handle_get_prompt_success/0` - Get existing prompt, verify handler executed and messages returned
   - `test_handle_get_prompt_not_found/0` - Get non-existent prompt, verify error response
   - `test_handle_get_prompt_handler_crash/0` - Handler crashes, verify internal error returned

3. **Prompt argument validation tests:**
   - `test_validate_prompt_arguments_success/0` - Valid arguments pass validation
   - `test_validate_prompt_arguments_missing_required/0` - Missing required argument, verify error
   - `test_validate_prompt_arguments_schema_failure/0` - Arguments fail schema validation, verify error

#### Pseudocode

**Test design:**
```erlang
test_handle_get_prompt_success() ->
    {ServerPid, _ServerId, _Capabilities} = setup_server(),
    PromptName = <<"test_prompt">>,
    Prompt = #mcp_prompt{
        name = PromptName,
        description = <<"A test prompt">>,
        arguments = [
            #mcp_prompt_argument{
                name = <<"topic">>,
                description = <<"Topic for the prompt">>,
                required = true
            }
        ]
    },
    Handler = fun(Args) ->
        Topic = maps:get(<<"topic">>, Args, <<"default">>),
        [#{
            role => <<"user">>,
            content => #mcp_content{
                type = <<"text">>,
                text = <<"Topic: ", Topic/binary>>
            }
        }]
    end,

    % Add prompt
    ok = erlmcp_server:add_prompt(ServerPid, PromptName, Handler),

    % Mock registry to capture response
    meck:new(erlmcp_registry, [passthrough]),
    meck:expect(erlmcp_registry, route_to_transport, fun(_, _, _) -> ok end),

    % Mock argument validator
    meck:new(erlmcp_prompt_argument_validator, [passthrough]),
    meck:expect(erlmcp_prompt_argument_validator, validate_prompt_arguments,
        fun(_, _, _) -> ok end),

    % Send prompt get request via handle_info
    Request = #json_rpc_request{
        id = 1,
        method = <<"prompts/get">>,
        params = #{name => PromptName, arguments => #{topic => <<"test">>}}
    },
    {ok, RequestJson} = erlmcp_json_rpc:encode_request(Request),
    ServerPid ! {mcp_message, test_transport, RequestJson},
    timer:sleep(100),

    % Verify argument validator called
    ?assert(meck:called(erlmcp_prompt_argument_validator, validate_prompt_arguments, [_, _, _])),

    % Verify response sent via registry
    ?assert(meck:called(erlmcp_registry, route_to_transport, [test_transport, _, _])),

    meck:unload(erlmcp_prompt_argument_validator),
    meck:unload(erlmcp_registry),
    cleanup_server({ServerPid, _ServerId, _Capabilities}).
```

#### Architecture

**INTEGRATION - Argument validation:**
- Use meck to mock erlmcp_prompt_argument_validator for argument validation tests
- Use meck to mock erlmcp_registry:route_to_transport/3 for response verification
- Use real fun handlers for execution tests (Chicago School)

**Dependencies:**
- erlmcp_prompt_argument_validator - Argument validation (mocked via meck)
- erlmcp_registry - Response routing (mocked via meck)
- erlmcp_prompt_list_change_notifier - Prompt added notifications (mocked via meck)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_server_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - Prompt management paths covered (handle_call clauses 264-299, 323-331)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: Prompt handler execution verified (not just state updates)
- [ ] Integration: Argument validation tested
- [ ] Edge cases: Handler crashes, missing required arguments, schema validation failures

---

### Phase 5: JSON-RPC Request Routing (≤4 hours)

#### Overview

Test all JSON-RPC request handlers in handle_request/5, covering initialize flow, resources operations, tools operations, prompts operations, and tasks operations.

#### Specification

**WHAT we're building:**

1. **Initialize tests:**
   - `test_initialize_success/0` - Initialize with valid capabilities, verify phase transition and response
   - `test_initialize_double_init/0` - Attempt double initialize, verify error response
   - `test_pre_init_rpc_rejection/0` - Call RPC before initialize, verify error response

2. **Resources operations tests:**
   - `test_resources_list_empty/0` - List resources with no resources, verify empty list
   - `test_resources_list_with_items/0` - List resources with items, verify all resources returned
   - `test_resources_read_success/0` - Read existing resource, verify content returned
   - `test_resources_read_missing_uri/0` - Read with missing URI parameter, verify error
   - `test_resources_read_not_found/0` - Read non-existent resource, verify error
   - `test_resources_templates_list/0` - List resource templates, verify templates returned
   - `test_resources_subscribe_success/0` - Subscribe to resource, verify subscription added
   - `test_resources_unsubscribe_success/0` - Unsubscribe from resource, verify removal

3. **Tools operations tests:**
   - `test_tools_list_empty/0` - List tools with no tools, verify empty list
   - `test_tools_list_with_items/0` - List tools with items, verify all tools returned
   - `test_tools_call_success/0` - Call existing tool, verify result returned
   - `test_tools_call_missing_name/0` - Call with missing tool name, verify error

4. **Prompts operations tests:**
   - `test_prompts_list_empty/0` - List prompts with no prompts, verify empty list
   - `test_prompts_list_with_items/0` - List prompts with items, verify all prompts returned
   - `test_prompts_get_success/0` - Get existing prompt, verify messages returned
   - `test_prompts_get_missing_name/0` - Get with missing prompt name, verify error

5. **Tasks operations tests:**
   - `test_tasks_create_success/0` - Create task, verify task summary returned
   - `test_tasks_list/0` - List tasks, verify all tasks returned
   - `test_tasks_get_success/0` - Get existing task, verify task details returned
   - `test_tasks_get_not_found/0` - Get non-existent task, verify error
   - `test_tasks_result_completed/0` - Get result for completed task, verify result returned
   - `test_tasks_cancel_success/0` - Cancel existing task, verify cancellation

6. **Invalid method tests:**
   - `test_invalid_method/0` - Call unknown method, verify method not found error

#### Pseudocode

**Test design:**
```erlang
test_initialize_success() ->
    {ServerPid, _ServerId, Capabilities} = setup_server(),

    % Mock registry to capture response
    meck:new(erlmcp_registry, [passthrough]),
    meck:expect(erlmcp_registry, route_to_transport, fun(_, _, _) -> ok end),

    % Mock capabilities extraction
    meck:new(erlmcp_capabilities, [passthrough]),
    meck:expect(erlmcp_capabilities, extract_client_capabilities, fun(_) -> #mcp_client_capabilities{} end),
    meck:expect(erlmcp_capabilities, validate_protocol_version, fun(_) -> ok end),
    meck:expect(erlmcp_capabilities, capability_to_map, fun(_) -> #{} end),

    % Send initialize request via handle_info
    Request = #json_rpc_request{
        id = 1,
        method = <<"initialize">>,
        params = #{
            protocolVersion => <<"2025-11-25">>,
            capabilities => #{},
            clientInfo => #{name => <<"test_client">>, version => <<"1.0.0">>}
        }
    },
    {ok, RequestJson} = erlmcp_json_rpc:encode_request(Request),
    ServerPid ! {mcp_message, test_transport, RequestJson},
    timer:sleep(100),

    % Verify phase transition
    State = sys:get_state(ServerPid),
    ?assertEqual(true, State#state.initialized),
    ?assertEqual(initialized, State#state.phase),

    % Verify response sent via registry
    ?assert(meck:called(erlmcp_registry, route_to_transport, [test_transport, _, _])),

    meck:unload(erlmcp_capabilities),
    meck:unload(erlmcp_registry),
    cleanup_server({ServerPid, _ServerId, Capabilities}).

test_initialize_double_init() ->
    {ServerPid, _ServerId, Capabilities} = setup_server(),

    % Mock registry
    meck:new(erlmcp_registry, [passthrough]),
    meck:expect(erlmcp_registry, route_to_transport, fun(_, _, _) -> ok end),
    meck:new(erlmcp_capabilities, [passthrough]),
    meck:expect(erlmcp_capabilities, extract_client_capabilities, fun(_) -> #mcp_client_capabilities{} end),
    meck:expect(erlmcp_capabilities, validate_protocol_version, fun(_) -> ok end),
    meck:expect(erlmcp_capabilities, capability_to_map, fun(_) -> #{} end),

    % First initialize
    Request1 = #json_rpc_request{
        id = 1,
        method = <<"initialize">>,
        params = #{protocolVersion => <<"2025-11-25">>, capabilities => #{}}
    },
    {ok, RequestJson1} = erlmcp_json_rpc:encode_request(Request1),
    ServerPid ! {mcp_message, test_transport, RequestJson1},
    timer:sleep(100),

    % Second initialize (should fail)
    Request2 = #json_rpc_request{
        id = 2,
        method = <<"initialize">>,
        params = #{protocolVersion => <<"2025-11-25">>, capabilities => #{}}
    },
    {ok, RequestJson2} = erlmcp_json_rpc:encode_request(Request2),
    ServerPid ! {mcp_message, test_transport, RequestJson2},
    timer:sleep(100),

    % Verify error response for double initialize
    ?assert(meck:called(erlmcp_registry, route_to_transport, [test_transport, _, error_json])),

    meck:unload(erlmcp_capabilities),
    meck:unload(erlmcp_registry),
    cleanup_server({ServerPid, _ServerId, Capabilities}).
```

#### Architecture

**INTEGRATION - Request routing:**
- Use meck to mock erlmcp_json_rpc:encode_response/2 and encode_error_response/4
- Use meck to mock erlmcp_registry:route_to_transport/3 for response verification
- Use meck to mock erlmcp_capabilities for capability negotiation
- Send requests via handle_info({mcp_message, TransportId, Data})

**Dependencies:**
- erlmcp_json_rpc - JSON-RPC encoding (mocked via meck)
- erlmcp_registry - Response routing (mocked via meck)
- erlmcp_capabilities - Capability negotiation (mocked via meck)
- erlmcp_task_manager - Task operations (mocked via meck)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_server_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - JSON-RPC request paths covered (handle_request/5 clauses 449-716)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All 16 method clauses tested
- [ ] Integration: Initialize flow tested (normal, double-init, pre-init rejection)
- [ ] Edge cases: Missing parameters, not found errors, invalid methods

---

### Phase 6: Error Handling (≤4 hours)

#### Overview

Test all error handling paths, including missing parameters, not found errors, validation errors, handler crashes, and safe wrapper failures.

#### Specification

**WHAT we're building:**

1. **Missing parameter tests:**
   - `test_resources_read_missing_uri/0` - resources/read with missing URI parameter
   - `test_tools_call_missing_name/0` - tools/call with missing name parameter
   - `test_prompts_get_missing_name/0` - prompts/get with missing name parameter
   - `test_tasks_get_missing_task_id/0` - tasks/get with missing taskId parameter

2. **Not found error tests:**
   - `test_resources_read_not_found/0` - Read non-existent resource
   - `test_tools_call_not_found/0` - Call non-existent tool
   - `test_prompts_get_not_found/0` - Get non-existent prompt
   - `test_delete_resource_not_found/0` - Delete non-existent resource
   - `test_delete_tool_not_found/0` - Delete non-existent tool
   - `test_delete_prompt_not_found/0` - Delete non-existent prompt

3. **Validation error tests:**
   - `test_add_resource_invalid_uri/0` - Add resource with invalid URI format
   - `test_add_resource_template_invalid_uri/0` - Add template with invalid URI template
   - `test_prompt_argument_validation_failed/0` - Prompt arguments fail validation

4. **Handler crash tests:**
   - `test_resource_handler_crash/0` - Resource handler crashes, verify internal error
   - `test_tool_handler_crash/0` - Tool handler crashes, verify internal error and progress cleanup
   - `test_prompt_handler_crash/0` - Prompt handler crashes, verify internal error

5. **Safe wrapper failure tests:**
   - `test_send_response_safe_failure/0` - Registry send fails, verify catch and warning logged
   - `test_send_error_safe_failure/0` - Registry error send fails, verify catch and warning logged
   - `test_send_notification_safe_failure/0` - Registry notification send fails, verify catch and warning logged

#### Pseudocode

**Test design:**
```erlang
test_resource_handler_crash() ->
    {ServerPid, _ServerId, _Capabilities} = setup_server(),
    Uri = <<"test://crash">>,
    Handler = fun(_Uri) -> erlang:error(crash_intentional) end,

    % Add crashing handler
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    % Mock registry to capture error response
    meck:new(erlmcp_registry, [passthrough]),
    meck:expect(erlmcp_registry, route_to_transport, fun(_, _, _) -> ok end),

    % Send resources/read request
    Request = #json_rpc_request{
        id = 1,
        method = <<"resources/read">>,
        params = #{uri => Uri}
    },
    {ok, RequestJson} = erlmcp_json_rpc:encode_request(Request),
    ServerPid ! {mcp_message, test_transport, RequestJson},
    timer:sleep(100),

    % Verify internal error response sent
    ?assert(meck:called(erlmcp_registry, route_to_transport, [test_transport, _, error_json])),

    meck:unload(erlmcp_registry),
    cleanup_server({ServerPid, _ServerId, _Capabilities}).
```

#### Architecture

**INTEGRATION - Error path verification:**
- Use meck to mock erlmcp_uri_validator to return validation errors
- Use meck to mock erlmcp_registry:route_to_transport/3 to verify error responses
- Use fun handlers that crash intentionally to test error handling

**Dependencies:**
- erlmcp_uri_validator - URI validation (mocked via meck)
- erlmcp_registry - Response routing (mocked via meck)
- erlmcp_prompt_argument_validator - Argument validation (mocked via meck)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_server_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - Error handling paths covered (all catch clauses, error returns)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: All error paths tested (catch clauses, error returns)
- [ ] Integration: Error responses properly formatted and routed
- [ ] Edge cases: Handler crashes, validation failures, missing parameters

---

### Phase 7: Concurrent Access (≤4 hours)

#### Overview

Test concurrent access scenarios, including multiple simultaneous add operations, race conditions in subscribe/unsubscribe, and state consistency under concurrent load.

#### Specification

**WHAT we're building:**

1. **Concurrent add operations tests:**
   - `test_concurrent_add_resources/0` - Multiple processes add resources simultaneously
   - `test_concurrent_add_tools/0` - Multiple processes add tools simultaneously
   - `test_concurrent_add_prompts/0` - Multiple processes add prompts simultaneously

2. **Race condition tests:**
   - `test_concurrent_subscribe_unsubscribe/0` - Multiple processes subscribe/unsubscribe same resource
   - `test_concurrent_delete_read/0` - One process deletes resource while another reads it

3. **State consistency tests:**
   - `test_state_consistency_under_load/0` - Verify no corrupted state after 100 concurrent operations

#### Pseudocode

**Test design:**
```erlang
test_concurrent_add_resources() ->
    {ServerPid, _ServerId, _Capabilities} = setup_server(),

    % Spawn 10 processes adding resources concurrently
    NumProcesses = 10,
    Parent = self(),

    Pids = [spawn(fun() ->
        Uri = <<"test://concurrent/", (integer_to_binary(N))/binary>>,
        Handler = fun(_) -> <<"content">> end,
        Result = erlmcp_server:add_resource(ServerPid, Uri, Handler),
        Parent ! {result, N, Result}
    end) || N <- lists:seq(1, NumProcesses)],

    % Wait for all processes to complete
    Results = [receive {result, N, Result} -> {N, Result} end || _ <- Pids],

    % Verify all operations succeeded
    ?assertEqual(NumProcesses, length([R || {_, R} <- Results, R =:= ok])),

    % Verify state consistency
    State = sys:get_state(ServerPid),
    ?assertEqual(NumProcesses, maps:size(State#state.resources)),

    cleanup_server({ServerPid, _ServerId, _Capabilities}).
```

#### Architecture

**INTEGRATION - Concurrent operations:**
- Spawn multiple processes to perform concurrent operations
- Use erlang:spawn/1 and self() messaging for coordination
- Verify state consistency via sys:get_state/1 after all operations complete

**Dependencies:**
- None (tests use pure Erlang concurrency primitives)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_server_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - Concurrent access paths covered (gen_server serialization)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: Concurrent operations tested with state consistency verification
- [ ] Integration: No race conditions detected (all tests pass consistently)
- [ ] Edge cases: Simultaneous add/delete/subscribe operations

---

### Phase 8: Registry Integration (≤4 hours)

#### Overview

Test registry integration, verifying that JSON-RPC responses are routed correctly to transports via erlmcp_registry:route_to_transport/3.

#### Specification

**WHAT we're building:**

1. **Response routing tests:**
   - `test_response_routed_to_transport/0` - Verify response routed to correct transport
   - `test_error_routed_to_transport/0` - Verify error response routed to correct transport
   - `test_notification_broadcast/0` - Verify broadcast notification sent to all transports

2. **Transport ID correlation tests:**
   - `test_transport_id_correlation/0` - Verify transport ID is preserved in routing

3. **Registry failure tests:**
   - `test_registry_send_failure/0` - Registry send fails, verify safe wrapper catches exception

#### Pseudocode

**Test design:**
```erlang
test_response_routed_to_transport() ->
    {ServerPid, _ServerId, Capabilities} = setup_server(),

    % Mock registry to capture route_to_transport calls
    meck:new(erlmcp_registry, [passthrough]),
    TransportId = test_transport_123,

    % Send initialize request
    Request = #json_rpc_request{
        id = 1,
        method = <<"initialize">>,
        params = #{protocolVersion => <<"2025-11-25">>, capabilities => #{}}
    },
    {ok, RequestJson} = erlmcp_json_rpc:encode_request(Request),
    ServerPid ! {mcp_message, TransportId, RequestJson},
    timer:sleep(100),

    % Verify route_to_transport called with correct transport ID
    ?assert(meck:called(erlmcp_registry, route_to_transport, [TransportId, _, _])),

    meck:unload(erlmcp_registry),
    cleanup_server({ServerPid, _ServerId, Capabilities}).
```

#### Architecture

**INTEGRATION - Registry routing:**
- Use meck to mock erlmcp_registry:route_to_transport/3
- Verify transport ID and response payload are correct
- Test safe wrapper functions (send_response_safe, send_error_safe, send_notification_safe)

**Dependencies:**
- erlmcp_registry - Response routing (mocked via meck)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_server_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - Registry integration paths covered (send_*_via_registry, send_*_safe)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: Registry routing verified for all response types
- [ ] Integration: Transport ID correlation tested
- [ ] Edge cases: Registry send failures, safe wrapper error handling

---

## Testing Strategy

### Chicago School TDD (MANDATORY)

**NO MOCKS** for gen_servers:
- Use real processes, real gen_servers
- State-based verification (check #state{} record contents via sys:get_state/1)
- Integration testing (test with real dependencies)
- Race condition testing (concurrent operations)

**meck for side effects:**
- Mock external module calls (erlmcp_registry, erlmcp_tracing, erlmcp_json_rpc)
- Verify handlers are called with correct arguments
- Verify side effects (responses sent, notifications broadcast)

### Unit Tests (EUnit)

**What to Test:**
- All public functions (add_resource, add_tool, add_prompt, delete_*, subscribe_*)
- All gen_server callbacks (init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3)
- All JSON-RPC request handlers (handle_request/5 with 16 method clauses)
- All error paths (missing params, not_found, validation failures, handler crashes)
- All helper functions (find_resource, encode_resource, normalize_tool_result, etc.)

**Test Pattern:**
- Reference: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_client_tests.erl:14-100`
- Use EUnit test generators with setup/cleanup fixtures
- Use ?_test macro for individual test cases
- Use ?assertEqual, ?assertMatch, ?assert for assertions

**Coverage Target:**
- ≥80% per module (erlmcp_server.erl)
- 100% pass rate (all tests must pass)

### Manual Testing Steps

1. **Verification of test execution:**
   - Run `rebar3 eunit --module=erlmcp_server_tests`
   - Verify all tests pass (100% pass rate)
   - Verify test count ≥60

2. **Coverage analysis:**
   - Run `rebar3 cover --module=erlmcp_server`
   - Open `_build/test/cover/erlmcp_server.COVER.html`
   - Verify coverage ≥80%
   - Identify uncovered lines and add tests if needed

3. **Integration verification:**
   - Verify tests pass with real erlmcp_registry
   - Verify tests pass with real erlmcp_task_manager
   - Verify no process leaks (all test processes cleaned up)

### Quality Gates

Every phase MUST pass:
1. **Compilation:** `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
2. **EUnit:** `rebar3 eunit --module=erlmcp_server_tests` - 100% pass rate
3. **Coverage:** `rebar3 cover --module=erlmcp_server` - ≥80% coverage
4. **Dialyzer:** `rebar3 dialyzer` - 0 warnings
5. **Xref:** `rebar3 xref` - 0 undefined function calls

## Manufacturing Checklist

### Before Implementation
- [ ] Research verified (read actual source code)
- [ ] Scope confirmed (IN/OUT documented)
- [ ] No open questions (all decisions made)
- [ ] Phases broken down (≤4 hours each)
- [ ] Acceptance criteria defined (measurable, specific)

### During Implementation
- [ ] Chicago School TDD followed (tests FIRST)
- [ ] OTP patterns followed (gen_server, supervisor)
- [ ] Type specs added (Dialyzer clean)
- [ ] Error handling complete (all paths)
- [ ] Quality gates passing (compilation, tests, coverage)

### After Implementation
- [ ] All tests passing (100% rate)
- [ ] Coverage ≥80% (verified)
- [ ] Dialyzer 0 warnings (verified)
- [ ] Xref 0 undefined calls (verified)
- [ ] Performance no regression >10% (verified)
- [ ] Documentation updated (README, API docs)
- [ ] Code review complete (OTP patterns verified)

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **Incomplete coverage of error paths** | P0 (Critical) | High | Test ALL error return paths in handle_call/handle_request (missing params, invalid URIs, not_found errors) |
| **Missing concurrent access tests** | P0 (Critical) | High | Test multiple simultaneous add_resource/add_tool calls, verify state consistency |
| **Handler execution not verified** | P0 (Critical) | High | Use meck to mock handler side effects, verify handlers are called with correct arguments |
| **Registry integration not tested** | P1 (High) | Medium | Mock erlmcp_registry route calls, verify correct JSON-RPC responses |
| **Tracing overhead interferes with tests** | P2 (Medium) | Low | Use meck to stub erlmcp_tracing functions, disable span recording in tests |
| **State machine edge cases untested** | P1 (High) | Medium | Test double-initialize rejection, pre-initialization RPC rejection, phase transitions |
| **Resource template URI validation untested** | P2 (Medium) | Low | Test URI template validation errors (malformed templates, missing placeholders) |
| **Task lifecycle not tested** | P1 (High) | Medium | Test task_create, task_get, task_cancel with invalid task IDs |

**Severity Definitions:**
- **P0 (Critical):** BLOCKS production - MUST fix before any release
- **P1 (High):** Major quality gap - MUST fix before release
- **P2 (Medium):** Important but not blocking
- **P3 (Low):** Nice-to-have

### Rollback Plan

**Git revert:** If comprehensive test suite breaks existing functionality, revert to commit before test expansion:
```bash
git revert <commit-hash>
```

**Data migration:** Not applicable (no data migration for test changes)

**Service impact:** Tests run in isolation, no impact on running services. If tests fail in CI/CD, block merge until fixed.

## References

- **Research:** `/Users/sac/erlmcp/.wreckit/items/003-create-comprehensive-eunit-test-suite-for-erlmcpse/research.md`
- **PRD:** Saved via `mcp__wreckit__save_prd` - 14 user stories with measurable acceptance criteria
- **CLAUDE.md:** `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- **TCPS:** `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- **OTP Patterns:** `/Users/sac/erlmcp/docs/otp-patterns.md`
- **Test Reference:** `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_client_tests.erl:14-100`
- **Source Code:** `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl:1-1562`
- **Existing Tests:** `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_tests.erl:1-317`
- **Registry:** `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_registry.erl:1-100+`
- **Protocol Constants:** `/Users/sac/erlmcp/include/erlmcp.hrl:1-523`

---

**TCPS MANUFACTURING PLAN COMPLETE**

**Next Step:** Begin Phase 1 implementation following user stories US-001 through US-014.

**Execution Order:** All Priority 1 (P0) stories first, then Priority 2 (P1) stories.
- US-001: Test infrastructure (P0)
- US-002: Lifecycle tests (P0)
- US-003: Resource management (P0)
- US-004: Tool management (P0)
- US-005: Prompt management (P0)
- US-006: JSON-RPC initialize flow (P0)
- US-007: JSON-RPC resources operations (P0)
- US-008: JSON-RPC tools operations (P0)
- US-009: JSON-RPC prompts operations (P0)
- US-011: Error handling paths (P0)
- US-012: Concurrent access (P0)
- US-014: Final verification (P0)
- US-010: Tasks operations (P1)
- US-013: Notification and registry integration (P1)

**Zero Defects:** 99.99966% defect-free delivery (3.4 defects per million)
