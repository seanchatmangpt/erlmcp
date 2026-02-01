# Testing Workflows - Interactive Step-by-Step Guides

**Version:** 2.1.0
**Last Updated:** 2026-01-31
**Status:** Production-Ready

---

## Overview

This document provides step-by-step visual workflows for common testing scenarios in erlmcp. Each workflow includes decision trees, flowcharts, and actionable steps to guide you through testing tasks.

---

## Workflow 1: Writing Your First Test

### Decision Tree: When to Write Tests

```mermaid
graph TB
    Start[Need to Add Feature?] --> A{Is Test Already Written?}
    A -->|Yes| B[Update Test]
    A -->|No| C{New Feature?}

    C -->|Yes| D[Write Test First]
    C -->|No| E{Bug Fix?}

    E -->|Yes| F[Write Failing Test]
    E -->|No| G[Refactoring?]

    G -->|Yes| H[Tests Exist]
    H --> I[Run Tests First]
    I --> J[Verify Pass]
    J --> K[Refactor Code]

    D --> L[TDD Red-Green-Refactor]
    F --> M[Write Fix]
    M --> N[Test Passes]

    L --> Done[✅ Production-Ready]
    K --> Done
    N --> Done

    style Done fill:#51cf66
```

### TDD Red-Green-Refactor Cycle

```mermaid
stateDiagram-v2
    [*] --> Red: Write failing test
    Red --> VerifyFail: Run test
    VerifyFail --> Green: Test fails ✓
    VerifyFail --> Red: Test passes ✗

    Green --> WriteCode: Write minimal code
    WriteCode --> VerifyPass: Run test
    VerifyPass --> Green: Test fails ✗
    VerifyPass --> Refactor: Test passes ✓

    Refactor --> CheckChicago: Refactor code
    CheckChicago -->|Real collaborators| NextTest: No mocks
    CheckChicago -->|Has mocks| RemoveMocks: Remove mocks

    RemoveMocks --> WriteCode

    NextTest --> Coverage: Check coverage
    Coverage -->|≥80%| Done: ✅ Complete
    Coverage -->|<80%| AddTests: Add more tests

    AddTests --> Red

    note right of Red
        Write test FIRST
        Test MUST fail
        Understand requirement
    end note

    note right of Green
        Minimal implementation
        Make test pass
        No extra code
    end note

    note right of Refactor
        Improve design
        Remove duplication
        Keep tests green
    end note
```

### Step-by-Step Guide

```mermaid
graph LR
    subgraph "Step 1: Write Test"
        A1[Identify feature]
        A2[Write test function]
        A3[Run test]
        A4[Verify it fails]
    end

    subgraph "Step 2: Make Pass"
        B1[Write minimal code]
        B2[Run test]
        B3[Verify it passes]
    end

    subgraph "Step 3: Verify Quality"
        C1[Check coverage ≥80%]
        C2[Verify no mocks]
        C3[Check assertions]
    end

    subgraph "Step 4: Refactor"
        D1[Improve design]
        D2[Remove duplication]
        D3[Rerun tests]
    end

    A1 --> A2 --> A3 --> A4
    A4 --> B1
    B1 --> B2 --> B3
    B3 --> C1
    C1 -->|Low| AddMore[Add tests]
    C1 -->|OK| C2
    C2 -->|Mocks| RemoveMocks[Refactor]
    C2 -->|Clean| C3
    C3 --> D1
    D1 --> D2 --> D3
    D3 --> Complete[✅ Complete]

    AddMore -.-> A2
    RemoveMocks -.-> B1

    style Complete fill:#51cf66
```

### Practical Example

**Scenario**: Add a `count_tools/1` function to `erlmcp_server`

**Step 1: Write Test First**
```erlang
% File: apps/erlmcp_core/test/erlmcp_server_tests.erl

count_tools_test_() ->
    {setup,
     fun() -> {ok, Pid} = erlmcp_server:start_link(), Pid end,
     fun(Pid) -> ok = erlmcp_server:stop(Pid) end,
     fun(Pid) ->
         [
          ?_test(count_zero_tools(Pid)),
          ?_test(count_multiple_tools(Pid))
         ]
     end}.

count_zero_tools(Pid) ->
    % Exercise: Call new function
    {ok, Count} = erlmcp_server:count_tools(Pid),

    % Verify: Should be 0 (state-based)
    ?assertEqual(0, Count).

count_multiple_tools(Pid) ->
    % Setup: Add tools
    ok = erlmcp_server:add_tool(Pid, #{name => <<"tool1">>}),
    ok = erlmcp_server:add_tool(Pid, #{name => <<"tool2">>}),
    ok = erlmcp_server:add_tool(Pid, #{name => <<"tool3">>}),

    % Exercise: Count tools
    {ok, Count} = erlmcp_server:count_tools(Pid),

    % Verify: Should be 3
    ?assertEqual(3, Count).
```

**Step 2: Run Test (Verify it Fails)**
```bash
rebar3 eunit --module=erlmcp_server_tests
# Expected: undefined function count_tools/1
```

**Step 3: Write Minimal Code**
```erlang
% File: apps/erlmcp_core/src/erlmcp_server.erl

% Add to handle_call
handle_call({count_tools}, _From, State) ->
    Count = maps:size(State#state.tools),
    {reply, {ok, Count}, State};
```

**Step 4: Run Test (Verify it Passes)**
```bash
rebar3 eunit --module=erlmcp_server_tests
# Expected: All tests passed
```

**Step 5: Check Coverage**
```bash
rebar3 cover --verbose
# Expected: count_tools/1 coverage = 100%
```

**Step 6: Verify Chicago School**
```bash
# Check: No mocks? ✓
# Check: Real gen_server? ✓
# Check: State-based assertion? ✓
```

**Step 7: Refactor (if needed)**
```erlang
% Refactor for clarity
handle_call({count_tools}, _From, State) ->
    Tools = State#state.tools,
    Count = maps:size(Tools),
    {reply, {ok, Count}, State}.
```

---

## Workflow 2: Debugging Failing Tests

### Diagnostic Flowchart

```mermaid
graph TB
    Start[Test Failure] --> CheckOutput{Read Error Message}

    CheckOutput -->|Syntax error| Syntax[Fix syntax]
    CheckOutput -->|Compilation error| Compile[Fix compilation]
    CheckOutput -->|Runtime error| Runtime[Debug runtime]
    CheckOutput -->|Assertion failure| Assertion[Check assertion]

    Syntax --> Run1[Run test again]
    Compile --> Run1
    Runtime --> Investigate[Investigate state]
    Assertion --> Investigate

    Investigate --> Debug{Debugging Strategy}

    Debug -->|Add logging| Log[Add ct:log statements]
    Debug -->|Inspect state| Inspect[Use sys:get_status]
    Debug -->|Trace messages| Trace[Use erlang:trace]
    Debug -->|Run verbose| Verbose[Run with --verbose]

    Log --> Run2[Run test again]
    Inspect --> Run2
    Trace --> Run2
    Verbose --> Run2

    Run1 --> Fixed{Fixed?}
    Run2 --> Fixed

    Fixed -->|Yes| Success[✅ Test passes]
    Fixed -->|No| RootCause[Identify root cause]

    RootCause --> FixCode[Fix code or test]
    FixCode --> Run1

    style Success fill:#51cf66
```

### Common Failure Patterns

```mermaid
graph TB
    subgraph "Timeout Failures"
        T1[Increase timeout]
        T2[Check for deadlock]
        T3[Verify async operations]
    end

    subgraph "Assertion Failures"
        A1[Check expected value]
        A2[Verify test setup]
        A3[Inspect actual state]
    end

    subgraph "Process Failures"
        P1[Check supervision]
        P2[Verify process started]
        P3[Monitor process crashes]
    end

    subgraph "Race Conditions"
        R1[Add timer:sleep]
        R2[Use sync operations]
        R3[Verify ordering]
    end

    T1 --> Fix[Apply fix]
    T2 --> Fix
    T3 --> Fix
    A1 --> Fix
    A2 --> Fix
    A3 --> Fix
    P1 --> Fix
    P2 --> Fix
    P3 --> Fix
    R1 --> Fix
    R2 --> Fix
    R3 --> Fix

    Fix --> Retry[Retry test]

    style Retry fill:#51cf66
```

### Debugging Techniques

**Technique 1: Verbose Output**
```bash
# Run with verbose logging
rebar3 eunit --module=erlmcp_server_tests --verbose

# Enable debug logging
rebar3 shell
# In shell:
application:ensure_all_started(erlmcp).
logger:set_primary_config(level, all).
eunit:test(erlmcp_server_tests, [verbose]).
```

**Technique 2: Inspect Process State**
```erlang
debug_state_test() ->
    {ok, Pid} = erlmcp_server:start_link(),

    % Inspect internal state
    {status, Pid, {module, erlmcp_server}, Status} =
        sys:get_status(Pid),

    % Log state for debugging
    ct:log("Server state: ~p", [Status]),

    ?assertEqual(expected, get_state_value(Pid)),

    erlmcp_server:stop(Pid).
```

**Technique 3: Message Tracing**
```erlang
trace_messages_test() ->
    {ok, Pid} = erlmcp_server:start_link(),

    % Trace all messages to/from Pid
    erlang:trace(Pid, true, [send, 'receive']),

    % Exercise system
    ok = erlmcp_server:add_tool(Pid, #{name => <<"test">>}),

    % Allow time for messages
    timer:sleep(100),

    % Check trace output in logs
    ct:log("Trace output: ~p", [erlang:trace_info(Pid, flags)]),

    erlmcp_server:stop(Pid).
```

**Technique 4: Step-by-Step Execution**
```erlang
step_by_step_test() ->
    {ok, Pid} = erlmcp_server:start_link(),

    % Step 1: Verify server started
    ct:log("Step 1: Server started"),
    ?assert(is_process_alive(Pid)),

    % Step 2: Add tool
    ct:log("Step 2: Adding tool"),
    ok = erlmcp_server:add_tool(Pid, #{name => <<"tool1">>}),
    ct:log("Tool added successfully"),

    % Step 3: Verify tool count
    ct:log("Step 3: Checking tool count"),
    {ok, Count} = erlmcp_server:count_tools(Pid),
    ct:log("Tool count: ~p", [Count]),
    ?assertEqual(1, Count),

    erlmcp_server:stop(Pid).
```

---

## Workflow 3: Adding Coverage to Untested Modules

### Gap Analysis Workflow

```mermaid
graph TB
    Start[Untested Module] --> Analyze[Analyze module]

    Analyze --> Categorize{Module Type?}

    Categorize -->|Gen_server| Server[Gen_server pattern]
    Categorize -->|Utility| Util[Utility functions]
    Categorize -->|Transport| Transp[Transport behavior]
    Categorize -->|Protocol| Proto[Protocol codec]

    Server --> Identify[Identify test scenarios]
    Util --> Identify
    Transp --> Identify
    Proto --> Identify

    Identify --> Prioritize{Priority}

    Prioritize -->|P0| Critical[Critical path]
    Prioritize -->|P1| High[High value]
    Prioritize -->|P2| Medium[Medium priority]

    Critical --> Plan[Plan test cases]
    High --> Plan
    Medium --> Plan

    Plan --> Write[Write tests]
    Write --> Run[Run tests]
    Run --> Cover[Check coverage]

    Cover -->|≥80%| Done[✅ Complete]
    Cover -->|<80%| AddMore[Add more tests]

    AddMore --> Identify

    style Done fill:#51cf66
```

### Coverage Improvement Strategy

```mermaid
graph LR
    subgraph "Phase 1: Quick Wins"
        A1[Exported functions<br/>Easy to test]
        A2[Public API<br/>Happy paths]
    end

    subgraph "Phase 2: Edge Cases"
        B1[Error handling<br/>Error returns]
        B2[Boundary conditions<br/>Empty, nil, max]
    end

    subgraph "Phase 3: Complex Scenarios"
        C1[State transitions<br/>Lifecycle]
        C2[Concurrency<br/>Race conditions]
    end

    A1 --> Quick[Fast coverage boost]
    A2 --> Quick

    Quick --> B1
    B1 --> Medium[Medium effort]
    B2 --> Medium

    Medium --> C1
    C1 --> C2
    C2 --> Complete[Complete coverage]

    style Quick fill:#51cf66
    style Medium fill:#ffd43b
    style Complete fill:#51cf66
```

### Step-by-Step Coverage Improvement

**Example**: Add tests to `erlmcp_hooks` (0% coverage, 597 LOC)

**Step 1: Analyze Module**
```bash
# Check module structure
grep "^[a-z].*(" apps/erlmcp_core/src/erlmcp_hooks.erl | head -20

# Output:
# add_hook(Name, Handler)
# remove_hook(Name)
# execute_hook(Name, Args)
# list_hooks()
```

**Step 2: Identify Test Scenarios**
```erlang
% Test scenarios for erlmcp_hooks

%% Basic operations (Phase 1: Quick Wins)
% ✓ Add hook
% ✓ Remove hook
% ✓ Execute hook
% ✓ List hooks

%% Edge cases (Phase 2)
% ✓ Add duplicate hook
% ✓ Remove non-existent hook
% ✓ Execute non-existent hook
% ✓ Hook returns error

%% Complex scenarios (Phase 3)
% ✓ Multiple hooks for same event
% ✓ Hook execution order
% ✓ Hook failure handling
```

**Step 3: Write Tests (Phased Approach)**

**Phase 1: Quick Wins (30% coverage)**
```erlang
-module(erlmcp_hooks_tests).
-include_lib("eunit/include/eunit.hrl").

hooks_basic_test_() ->
    {setup,
     fun() -> {ok, Pid} = erlmcp_hooks:start_link(), Pid end,
     fun(Pid) -> ok = erlmcp_hooks:stop(Pid) end,
     fun(Pid) ->
         [
          ?_test(add_hook(Pid)),
          ?_test(execute_hook(Pid)),
          ?_test(list_hooks(Pid))
         ]
     end}.

add_hook(Pid) ->
    Handler = fun(_) -> ok end,
    ok = erlmcp_hooks:add_hook(Pid, <<"test_hook">>, Handler).

execute_hook(Pid) ->
    Handler = fun(Args) -> {ok, Args} end,
    ok = erlmcp_hooks:add_hook(Pid, <<"echo">>, Handler),

    {ok, Result} = erlmcp_hooks:execute_hook(Pid, <<"echo">>, #{msg => hello}),
    ?assertEqual(#{msg => hello}, Result).

list_hooks(Pid) ->
    Handler = fun(_) -> ok end,
    ok = erlmcp_hooks:add_hook(Pid, <<"hook1">>, Handler),
    ok = erlmcp_hooks:add_hook(Pid, <<"hook2">>, Handler),

    {ok, Hooks} = erlmcp_hooks:list_hooks(Pid),
    ?assertEqual(2, length(Hooks)).
```

**Phase 2: Edge Cases (60% coverage)**
```erlang
hooks_edge_cases_test_() ->
    {setup,
     fun() -> {ok, Pid} = erlmcp_hooks:start_link(), Pid end,
     fun(Pid) -> ok = erlmcp_hooks:stop(Pid) end,
     fun(Pid) ->
         [
          ?_test(add_duplicate_hook(Pid)),
          ?_test(remove_nonexistent_hook(Pid)),
          ?_test(execute_nonexistent_hook(Pid)),
          ?_test(hook_returns_error(Pid))
         ]
     end}.

add_duplicate_hook(Pid) ->
    Handler = fun(_) -> ok end,
    ok = erlmcp_hooks:add_hook(Pid, <<"dup">>, Handler),
    % Adding same hook again should fail or replace
    Result = erlmcp_hooks:add_hook(Pid, <<"dup">>, Handler),
    ?assertEqual({error, exists}, Result).

remove_nonexistent_hook(Pid) ->
    Result = erlmcp_hooks:remove_hook(Pid, <<"nonexistent">>),
    ?assertEqual({error, not_found}, Result).

execute_nonexistent_hook(Pid) ->
    Result = erlmcp_hooks:execute_hook(Pid, <<"nonexistent">>, #{}),
    ?assertEqual({error, not_found}, Result).

hook_returns_error(Pid) ->
    Handler = fun(_) -> {error, failed} end,
    ok = erlmcp_hooks:add_hook(Pid, <<"failing">>, Handler),

    Result = erlmcp_hooks:execute_hook(Pid, <<"failing">>, #{}),
    ?assertEqual({error, failed}, Result).
```

**Phase 3: Complex Scenarios (80%+ coverage)**
```erlang
hooks_complex_test_() ->
    {setup,
     fun() -> {ok, Pid} = erlmcp_hooks:start_link(), Pid end,
     fun(Pid) -> ok = erlmcp_hooks:stop(Pid) end,
     fun(Pid) ->
         [
          ?_test(multiple_hooks_execution(Pid)),
          ?_test(hook_execution_order(Pid)),
          ?_test(hook_failure_handling(Pid))
         ]
     end}.

multiple_hooks_execution(Pid) ->
    % Register multiple hooks for same event
    H1 = fun(Args) -> maps:put(h1, true, Args) end,
    H2 = fun(Args) -> maps:put(h2, true, Args) end,
    H3 = fun(Args) -> maps:put(h3, true, Args) end,

    ok = erlmcp_hooks:add_hook(Pid, <<"multi">>, H1),
    ok = erlmcp_hooks:add_hook(Pid, <<"multi">>, H2),
    ok = erlmcp_hooks:add_hook(Pid, <<"multi">>, H3),

    % Execute and verify all hooks ran
    {ok, Result} = erlmcp_hooks:execute_hook(Pid, <<"multi">>, #{}),
    ?assertEqual(true, maps:get(h1, Result)),
    ?assertEqual(true, maps:get(h2, Result)),
    ?assertEqual(true, maps:get(h3, Result)).

hook_execution_order(Pid) ->
    % Verify hooks execute in registration order
    OrderRef = make_ref(),
    PidSelf = self(),

    H1 = fun(_) -> PidSelf ! {hook, 1}, ok end,
    H2 = fun(_) -> PidSelf ! {hook, 2}, ok end,
    H3 = fun(_) -> PidSelf ! {hook, 3}, ok end,

    ok = erlmcp_hooks:add_hook(Pid, <<"order">>, H1),
    ok = erlmcp_hooks:add_hook(Pid, <<"order">>, H2),
    ok = erlmcp_hooks:add_hook(Pid, <<"order">>, H3),

    erlmcp_hooks:execute_hook(Pid, <<"order">>, #{}),

    % Verify execution order
    receive {hook, 1} -> ok end,
    receive {hook, 2} -> ok end,
    receive {hook, 3} -> ok end.

hook_failure_handling(Pid) ->
    % Register 3 hooks, middle one fails
    H1 = fun(_) -> {ok, h1} end,
    H2 = fun(_) -> {error, fail} end,
    H3 = fun(_) -> {ok, h3} end,

    ok = erlmcp_hooks:add_hook(Pid, <<"fail">>, H1),
    ok = erlmcp_hooks:add_hook(Pid, <<"fail">>, H2),
    ok = erlmcp_hooks:add_hook(Pid, <<"fail">>, H3),

    % Execute: should handle failure gracefully
    Result = erlmcp_hooks:execute_hook(Pid, <<"fail">>, #{}),

    % Verify: Either all execute or stop on failure
    ?assertMatch({ok, _} orelse {error, _}, Result).
```

**Step 4: Check Coverage**
```bash
# Run tests
rebar3 eunit --module=erlmcp_hooks_tests

# Check coverage
rebar3 cover --verbose | grep erlmcp_hooks

# Expected: 80%+ coverage
```

---

## Workflow 4: Setting Up Integration Tests

### Integration Test Decision Tree

```mermaid
graph TB
    Start[Need Integration Test?] --> A{Involves Multiple<br/>Processes?}

    A -->|No| Unit[Unit test sufficient]
    A -->|Yes| B{Requires Supervision<br/>Tree?}

    B -->|No| Unit
    B -->|Yes| C{Network<br/>Coordination?}

    C -->|No| Local[Local integration test]
    C -->|Yes| D{Distributed<br/>Nodes?}

    D -->|No| Local
    D -->|Yes| Distributed[Distributed test]

    Local --> Setup[Setup Common Test]
    Distributed --> Setup

    Setup --> Write[Write test cases]
    Write --> Run[Run with rebar3 ct]
    Run --> Verify[Verify multi-process<br/>coordination]

    Unit --> Simple[Use EUnit]

    style Setup fill:#51cf66
    style Distributed fill:#ffd43b
```

### Common Test Setup Flow

```mermaid
sequenceDiagram
    participant Dev as Developer
    participant File as SUITE File
    participant CT as Common Test
    participant App as Application

    Dev->>File: Create my_module_SUITE.erl
    Dev->>File: Define all/0
    Dev->>File: Implement init_per_suite/1
    Dev->>File: Implement test cases

    Dev->>CT: rebar3 ct --suite=my_module_SUITE
    CT->>File: Call init_per_suite/1
    File->>App: application:start()
    App-->>File: {ok, Pids}

    CT->>File: Call test_case_1/1
    File->>App: Exercise system
    App-->>File: Observable behavior

    CT->>File: Call end_per_suite/1
    File->>App: application:stop()

    CT-->>Dev: Test results
```

### Step-by-Step Integration Test Setup

**Scenario**: Test client-server communication with registry

**Step 1: Create SUITE File**
```erlang
% File: apps/erlmcp_core/test/erlmcp_client_server_SUITE.erl

-module(erlmcp_client_server_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%% Test cases
all() -> [
    client_server_communication,
    registry_coordination,
    supervised_restart
].

%% Suite setup
init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp).

%% Test case setup
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.
```

**Step 2: Write First Test Case**
```erlang
client_server_communication(Config) ->
    % Setup: Start registry
    {ok, Registry} = erlmcp_registry:start_link(),

    % Setup: Start server
    {ok, Server} = erlmcp_server:start_link(
        #{name => <<"test_server">>}
    ),

    % Setup: Start client
    {ok, Client} = erlmcp_client:start_link(
        #{server_name => <<"test_server">>}
    ),

    % Exercise: Send request via client
    Request = #{jsonrpc => <<"2.0">>, id => 1, method => <<"ping">>},
    {ok, Response} = erlmcp_client:send_request(Client, Request),

    % Verify: Response received
    ?assertEqual(<<"2.0">>, maps:get(jsonrpc, Response)),
    ?assertEqual(1, maps:get(id, Response)),
    ?assert(maps:is_key(result, Response)),

    % Teardown
    ok = erlmcp_client:stop(Client),
    ok = erlmcp_server:stop(Server),
    ok = erlmcp_registry:stop(Registry).
```

**Step 3: Write Registry Coordination Test**
```erlang
registry_coordination(Config) ->
    % Setup: Start registry
    {ok, Registry} = erlmcp_registry:start_link(),

    % Setup: Start multiple servers
    {ok, Server1} = erlmcp_server:start_link(#{name => <<"s1">>}),
    {ok, Server2} = erlmcp_server:start_link(#{name => <<"s2">>}),

    % Exercise: Verify both registered
    {ok, Pid1} = erlmcp_registry:whereis_name({mcp, server, <<"s1">>}),
    {ok, Pid2} = erlmcp_registry:whereis_name({mcp, server, <<"s2">>}),

    ?assertEqual(Server1, Pid1),
    ?assertEqual(Server2, Pid2),

    % Exercise: Client discovers servers
    {ok, Servers} = erlmcp_registry:list_names({mcp, server}),
    ?assertEqual(2, length(Servers)),

    % Teardown
    ok = erlmcp_server:stop(Server1),
    ok = erlmcp_server:stop(Server2),
    ok = erlmcp_registry:stop(Registry).
```

**Step 4: Write Supervision Test**
```erlang
supervised_restart(Config) ->
    % Setup: Start application (starts supervision tree)
    {ok, SupPid} = erlmcp_core_sup:start_link(),

    % Get initial server PID
    {children, Children} = supervisor:which_children(SupPid),
    {erlmcp_server, InitialPid, _, _} =
        lists:keyfind(erlmcp_server, 1, Children),

    % Exercise: Kill server
    exit(InitialPid, kill),
    timer:sleep(100), % Allow supervisor to restart

    % Verify: Server restarted
    {children, NewChildren} = supervisor:which_children(SupPid),
    {erlmcp_server, NewPid, _, _} =
        lists:keyfind(erlmcp_server, 1, NewChildren),

    ?assertNotEqual(InitialPid, NewPid),
    ?assert(is_process_alive(NewPid)),

    % Teardown
    erlmcp_core_sup:stop(SupPid).
```

**Step 5: Run Integration Tests**
```bash
# Run specific suite
rebar3 ct --suite=erlmcp_client_server_SUITE

# Run with verbose output
rebar3 ct --suite=erlmcp_client_server_SUITE --verbose

# Check results
ls -l _build/test/logs/
```

---

## Workflow 5: Running Tests in CI/CD

### CI/CD Pipeline Flow

```mermaid
graph TB
    Trigger[Push/PR Trigger] --> Setup[Setup Environment<br/>Erlang/OTP, Deps]

    Setup --> Compile[Compile Code<br/>TERM=dumb rebar3 compile]

    Compile -->|errors = 0| Test[Run Tests<br/>EUnit, CT, Proper]
    Compile -->|errors > 0| Fail1[❌ Fail Build]

    Test -->|100% pass| Cover[Check Coverage<br/>rebar3 cover]
    Test -->|failures > 0| Fail2[❌ Fail Build]

    Cover -->|≥80%| Bench[Run Benchmarks<br/>make benchmark-strict]
    Cover -->|<80%| Fail3[❌ Fail Build]

    Bench -->|<10% regression| Dialyzer[Run Dialyzer<br/>Optional]
    Bench -->|≥10% regression| Fail4[❌ Fail Build]

    Dialyzer -->|warnings = 0| Success[✅ Build Passes]
    Dialyzer -->|warnings > 0| Warn[⚠️ Warnings]

    Success --> Report[Generate Reports]
    Warn --> Report

    Report --> Upload[Upload Artifacts]
    Upload --> Comment[PR Comment]
    Comment --> Merge[Merge Allowed]

    style Success fill:#51cf66
    style Merge fill:#51cf66
    style Fail1 fill:#ff6b6b
    style Fail2 fill:#ff6b6b
    style Fail3 fill:#ff6b6b
    style Fail4 fill:#ff6b6b
```

### GitHub Actions Workflow

```mermaid
sequenceDiagram
    participant Push as Developer Push
    participant GH as GitHub Actions
    participant Runner as Test Runner
    participant Artifacts as Artifact Storage
    participant PR as Pull Request

    Push->>GH: Trigger workflow
    GH->>Runner: Allocate runner

    Runner->>Runner: Setup Erlang/OTP
    Runner->>Runner: Install dependencies
    Runner->>Runner: Compile code

    Runner->>Runner: Run EUnit
    Runner->>Runner: Run Common Test
    Runner->>Runner: Run Proper

    Runner->>Runner: Generate coverage
    Runner->>Runner: Run benchmarks

    Runner->>Artifacts: Upload coverage report
    Runner->>Artifacts: Upload test results

    Runner->>GH: Report status

    alt Pull Request
        GH->>PR: Post coverage comment
        PR->>Push: Notification sent
    end

    GH->>Runner: Cleanup
    Runner-->>GH: Build complete
```

### Setting Up CI/CD

**Step 1: Create GitHub Actions Workflow**
```yaml
# File: .github/workflows/test.yml

name: Test Suite

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        otp: [25, 26, 27]

    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlang-solutions/erlang-otp-actions@v1
        with:
          otp-version: ${{ matrix.otp }}

      - name: Fetch dependencies
        run: rebar3 get-deps

      - name: Compile
        run: TERM=dumb rebar3 compile

      - name: Run tests
        run: make test-strict

      - name: Check coverage
        run: make coverage-strict

      - name: Run benchmarks
        run: make benchmark-strict

      - name: Upload coverage
        uses: actions/upload-artifact@v3
        with:
          name: coverage-${{ matrix.otp }}
          path: _build/test/cover/

      - name: Upload test results
        uses: actions/upload-artifact@v3
        with:
          name: test-results-${{ matrix.otp }}
          path: _build/test/logs/
```

**Step 2: Test Locally Before Push**
```bash
# Run same tests as CI
make quality-strict

# Verify coverage
rebar3 cover --verbose

# Verify no regressions
make benchmark-strict
```

**Step 3: Push and Monitor**
```bash
# Commit and push
git add .
git commit -m "Add test coverage for module X"
git push origin feature/add-tests

# Monitor GitHub Actions
# Open: https://github.com/user/repo/actions
```

**Step 4: Review Artifacts**
```bash
# Download coverage report from GitHub Actions
# Open index.html
open _build/test/cover/index.html
```

---

## Summary

**Workflow Coverage:**

- ✅ **Writing First Test**: TDD red-green-refactor cycle
- ✅ **Debugging Failures**: Diagnostic flowchart and techniques
- ✅ **Adding Coverage**: Phased approach (quick wins → edge cases → complex)
- ✅ **Integration Tests**: Multi-process Common Test setup
- ✅ **CI/CD Pipeline**: GitHub Actions automation

**Key Takeaways:**

1. **Always write tests first** (TDD)
2. **Use real collaborators** (Chicago School)
3. **Test observable behavior** (state-based)
4. **Add coverage in phases** (30% → 60% → 80%+)
5. **Debug with logging and tracing**
6. **Automate in CI/CD**

---

**Related Documentation:**
- [README](README.md) - Testing overview
- [TDD Strategy](tdd-strategy.md) - Chicago School methodology
- [Test Patterns](TEST_PATTERNS_LIBRARY.md) - Comprehensive patterns
- [Testing Architecture](TESTING_ARCHITECTURE.md) - System design

**Last Updated:** 2026-01-31
**Maintained by:** erlang-test-engineer agent
**Version:** 2.1.0
