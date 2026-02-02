# erlmcp-flow Test Quick Reference Card

## Chicago School TDD at a Glance

| Principle | What to Do | What NOT to Do |
|-----------|------------|----------------|
| **Real Processes** | Use actual gen_servers, supervisors | ❌ Mock gen_server calls with meck |
| **State-Based Verification** | Assert on `get_state()`, `get_status()` | ❌ Verify internal method calls |
| **Real Collaborators** | Test components together | ❌ Mock/stub dependencies |
| **Observable Behavior** | Test what system does (outputs) | ❌ Test how it does it (internals) |
| **Real Faults** | `exit(Pid, kill)` for crashes | ❌ Mock crash behavior |

---

## Test File Naming Convention

```
apps/erlmcp_core/test/
├── erlmcp_flow_<module>_tests.erl       # EUnit (unit tests)
├── erlmcp_flow_<feature>_SUITE.erl      # Common Test (integration)
└── erlmcp_flow_proper_tests.erl         # Proper (property tests)
```

---

## EUnit Test Template (Chicago School)

```erlang
-module(erlmcp_flow_<module>_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Setup/Teardown (Real Processes)
<module>_test_() ->
    {setup,
     fun() ->
         %% Setup: Start real gen_server
         application:ensure_all_started(erlmcp_core),
         {ok, Pid} = erlmcp_flow_<module>:start_link(test, #{}),
         Pid
     end,
     fun(Pid) ->
         %% Teardown: Stop gen_server
         erlmcp_flow_<module>:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(test_happy_path(Pid)),
          ?_test(test_error_case(Pid)),
          ?_test(test_edge_case(Pid))
         ]
     end}.

%%% Test (Observable State)
test_happy_path(Pid) ->
    %% Exercise: Call API
    ok = erlmcp_flow_<module>:do_something(Pid, Args),

    %% Verify: Observable state changed
    {ok, State} = erlmcp_flow_<module>:get_state(Pid),
    ?assertEqual(expected_state, State).
```

---

## Common Test Template (Integration)

```erlang
-module(erlmcp_flow_<feature>_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [test_multi_process_scenario].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp_core),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_core),
    ok.

test_multi_process_scenario(_Config) ->
    %% Setup: Start real processes
    {ok, P1} = erlmcp_flow_agent:start_link(agent1, #{}),
    {ok, P2} = erlmcp_flow_agent:start_link(agent2, #{}),

    %% Exercise: Real interaction
    ok = erlmcp_flow_agent:assign_task(P1, Task),

    %% Verify: Observable behavior
    {ok, working} = erlmcp_flow_agent:get_state(P1),

    %% Cleanup
    erlmcp_flow_agent:stop(P1),
    erlmcp_flow_agent:stop(P2).
```

---

## Property Test Template (Proper)

```erlang
-module(erlmcp_flow_proper_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

proper_test_() ->
    {timeout, 300, [
        {"Invariant Name", ?_assert(proper:quickcheck(prop_invariant(), [{numtests, 100}]))}
    ]}.

prop_invariant() ->
    ?FORALL({Input1, Input2}, {generator1(), generator2()},
        begin
            %% Setup: Real system
            {ok, Pid} = erlmcp_flow_<module>:start_link(test, #{}),

            %% Exercise
            ok = erlmcp_flow_<module>:do_something(Pid, Input1, Input2),

            %% Verify: Invariant (observable state)
            {ok, Result} = erlmcp_flow_<module>:get_result(Pid),

            %% Cleanup
            erlmcp_flow_<module>:stop(Pid),

            %% Invariant
            check_invariant(Result)
        end).
```

---

## Chaos Testing Template

```erlang
test_chaos_scenario(_Config) ->
    %% Setup: Real system
    {ok, SwarmPid} = erlmcp_flow_swarm:start_swarm(test, #{}),
    Agents = [start_agent(N) || N <- lists:seq(1, 10)],

    %% Chaos: Real fault injection (NO MOCKING)
    AgentsToKill = lists:sublist(shuffle(Agents), 3),
    [exit(A, kill) || A <- AgentsToKill],  %% Real process death

    timer:sleep(500),

    %% Verify: Observable recovery
    {ok, Status} = erlmcp_flow_swarm:get_swarm_status(SwarmPid),
    #{agents := Alive} = Status,
    7 = length(Alive),  %% 10 - 3 killed

    %% Cleanup
    [catch stop_agent(A) || A <- Agents],
    erlmcp_flow_swarm:stop_swarm(SwarmPid).
```

---

## Running Tests

```bash
# Single EUnit test
rebar3 eunit --module=erlmcp_flow_agent_tests

# Single Common Test suite
rebar3 ct --suite=test/erlmcp_flow_chaos_SUITE

# Property tests (Proper)
rebar3 proper -c --module=erlmcp_flow_proper_tests

# All tests
rebar3 do eunit, ct, proper -c

# Coverage
rebar3 cover --verbose

# Full quality gate
make check
```

---

## Verification Checklist

Before marking task complete:

- [ ] ✅ Compile: `TERM=dumb rebar3 compile` (0 errors)
- [ ] ✅ Tests: `rebar3 eunit` (0 failures)
- [ ] ✅ Integration: `rebar3 ct` (all passed)
- [ ] ✅ Properties: `rebar3 proper -c` (100 cases passed)
- [ ] ✅ Coverage: `rebar3 cover` (≥80% overall, ≥85% core)
- [ ] ✅ Dialyzer: `rebar3 dialyzer` (0 warnings)
- [ ] ✅ Format: `rebar3 format --verify` (clean)
- [ ] ✅ Chicago School: Real processes ✅, No mocks ✅, Observable state ✅

---

## Common Patterns

### Pattern: State Transition Test

```erlang
test_state_machine(Pid) ->
    %% idle → working
    ok = assign_task(Pid, Task),
    {ok, working} = get_state(Pid),

    %% working → idle
    timer:sleep(100),
    {ok, idle} = get_state(Pid).
```

### Pattern: Concurrent Operations

```erlang
test_concurrent(Pid) ->
    %% Spawn 100 real processes (no mocking)
    Pids = [spawn(fun() -> submit_task(Pid, task(N)) end) || N <- lists:seq(1, 100)],

    %% Wait for all
    [begin Ref = monitor(process, P), receive {'DOWN', Ref, _, _, _} -> ok end end || P <- Pids],

    %% Verify observable state
    {ok, 100} = get_queue_depth(Pid).
```

### Pattern: Failure Recovery

```erlang
test_recovery(Pid) ->
    %% Assign task
    ok = assign_task(Pid, Task),

    %% Kill process (real crash, no mock)
    exit(Pid, kill),
    timer:sleep(100),

    %% Verify supervisor restarted (observable recovery)
    NewPid = whereis(agent_name),
    ?assert(is_pid(NewPid)),
    {ok, idle} = get_state(NewPid).
```

---

## Debugging Failed Tests

### Test Fails: "Process not alive"

**Cause**: Agent crashed during test

**Fix**: Check logs for crash reason, verify supervisor config

```bash
ct:log("Agent state before crash: ~p", [get_state(Pid)])
```

### Test Fails: "Timeout waiting for state change"

**Cause**: Async operation too slow

**Fix**: Increase wait time or poll state

```erlang
wait_for_state(Pid, ExpectedState, Timeout) ->
    wait_for_state(Pid, ExpectedState, Timeout, erlang:monotonic_time(millisecond)).

wait_for_state(Pid, ExpectedState, Timeout, StartTime) ->
    case get_state(Pid) of
        {ok, ExpectedState} -> ok;
        _ ->
            case erlang:monotonic_time(millisecond) - StartTime > Timeout of
                true -> {error, timeout};
                false ->
                    timer:sleep(50),
                    wait_for_state(Pid, ExpectedState, Timeout, StartTime)
            end
    end.
```

### Property Test Shrinking

When property test fails, Proper will shrink to minimal failing case:

```
Failed: After 42 tests.
Shrinking (3 times):
{NumNodes, NumDown} = {5, 3}
```

Debug with minimal case:

```erlang
prop_test_debug() ->
    NumNodes = 5,
    NumDown = 3,
    %% Run minimal failing case manually with ct:log
    ...
```

---

## Anti-Patterns (Don't Do This!)

### ❌ Mocking gen_server

```erlang
%% BAD (London School)
meck:expect(agent, get_state, fun(_) -> {ok, working} end)
```

### ✅ Real gen_server

```erlang
%% GOOD (Chicago School)
{ok, Pid} = erlmcp_flow_agent:start_link(test, #{}),
{ok, working} = erlmcp_flow_agent:get_state(Pid)
```

---

### ❌ Verifying internal calls

```erlang
%% BAD (London School)
?assertMatch({call, handle_call, [_, _, _]}, meck:history(agent))
```

### ✅ Verifying observable state

```erlang
%% GOOD (Chicago School)
ok = assign_task(Pid, Task),
{ok, working} = get_state(Pid)  %% Verify state, not calls
```

---

### ❌ Mocking collaborators

```erlang
%% BAD (London School)
meck:expect(swarm, add_agent, fun(_, _) -> ok end)
```

### ✅ Real collaborators

```erlang
%% GOOD (Chicago School)
{ok, SwarmPid} = erlmcp_flow_swarm:start_swarm(test, #{}),
{ok, AgentPid} = erlmcp_flow_agent:start_link(agent, #{}),
ok = erlmcp_flow_swarm:add_agent(SwarmPid, AgentPid)  %% Real interaction
```

---

## Coverage Targets

| Module Type | Minimum | Target | Priority |
|-------------|---------|--------|----------|
| Agent (core) | 85% | 90% | High |
| Swarm (core) | 85% | 90% | High |
| Consensus (Raft) | 80% | 85% | Medium |
| Consensus (Byzantine) | 75% | 80% | Medium |
| Consensus (Gossip) | 75% | 80% | Medium |
| Task management | 85% | 90% | High |

---

## Chicago School TDD Summary

**Core Principle**: Test what the system does (outputs, state), not how it does it (internals).

**Three Rules**:
1. **Use real processes** (spawn actual gen_servers, no mocks)
2. **Verify observable state** (get_state, get_status, not internal calls)
3. **Test together** (integrate components whenever possible)

**Remember**: If you're mocking, you're not doing Chicago School TDD!

---

**Quick Reference Version**: 1.0.0
**Last Updated**: 2026-02-01
**For**: erlmcp-flow testing
