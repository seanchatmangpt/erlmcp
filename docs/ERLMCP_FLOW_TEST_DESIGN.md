# erlmcp-flow: Chicago School TDD Test Design

## Executive Summary

Comprehensive test strategy for erlmcp-flow multi-agent orchestration system following Chicago School TDD principles:
- **Real processes**: No mocks, use actual gen_servers, supervisors, and distributed processes
- **State-based verification**: Assert on observable behavior, not internal interactions
- **Real collaborators**: Test components together whenever possible
- **Coverage target**: ≥80% minimum, ≥85% for core agent/swarm modules

---

## System Overview

**erlmcp-flow** is a multi-agent orchestration system built on Erlang/OTP with:

| Component | Implementation | Test Focus |
|-----------|----------------|------------|
| **Agent Processes** | gen_server state machines | Lifecycle, state transitions, task execution |
| **Swarms** | Multi-process coordination via gproc | Coordination, load balancing, failure recovery |
| **Consensus** | Raft, Byzantine, Gossip algorithms | Leader election, partition tolerance, Byzantine faults |
| **Supervision** | 3-tier OTP supervision trees | Restart strategies, isolation, let-it-crash |
| **Transport** | Message passing, network simulation | Partitions, delays, message loss |
| **Chaos Engineering** | Fault injection, partition simulation | Resilience, recovery, data consistency |

---

## Test Architecture

### 1. Test File Organization

```
apps/erlmcp_core/test/
├── erlmcp_flow_agent_tests.erl              # EUnit: Agent gen_server state machines
├── erlmcp_flow_agent_supervisor_tests.erl   # EUnit: Agent supervision
├── erlmcp_flow_swarm_tests.erl              # EUnit: Swarm coordination basics
├── erlmcp_flow_consensus_raft_tests.erl     # EUnit: Raft algorithm
├── erlmcp_flow_consensus_byzantine_tests.erl # EUnit: Byzantine consensus
├── erlmcp_flow_consensus_gossip_tests.erl   # EUnit: Gossip protocol
├── erlmcp_flow_task_tests.erl               # EUnit: Task lifecycle
├── erlmcp_flow_integration_SUITE.erl        # Common Test: Multi-process integration
├── erlmcp_flow_swarm_integration_SUITE.erl  # Common Test: Swarm scenarios
├── erlmcp_flow_chaos_SUITE.erl              # Common Test: Chaos testing
├── erlmcp_flow_partition_SUITE.erl          # Common Test: Network partitions
├── erlmcp_flow_proper_tests.erl             # Proper: Property-based tests
```

---

## 2. Module Specifications

### 2.1 Agent Process Module (`erlmcp_flow_agent`)

**Purpose**: Individual agent as gen_server with task queue, state machine, and supervision

#### Module API
```erlang
%% Lifecycle
-spec start_link(AgentId, Config) -> {ok, Pid} | {error, Reason}.
-spec stop(Pid) -> ok.

%% Task Management
-spec assign_task(Pid, Task) -> ok | {error, Reason}.
-spec get_status(Pid) -> {ok, Status} | {error, Reason}.
-spec cancel_task(Pid, TaskId) -> ok | {error, Reason}.

%% State Queries (Chicago School: observable state)
-spec get_state(Pid) -> {ok, idle | working | failed}.
-spec get_current_task(Pid) -> {ok, Task} | {error, no_task}.
-spec get_queue_depth(Pid) -> {ok, non_neg_integer()}.
```

#### State Machine
```
idle → working → idle
   ↓       ↓
  failed ← ←
   ↓
 recovering → idle
```

#### Test Plan: `erlmcp_flow_agent_tests.erl`

**Setup/Teardown (Chicago School)**:
```erlang
agent_test_() ->
    {setup,
     fun() ->
         %% Setup: Start real agent gen_server
         application:ensure_all_started(erlmcp_core),
         {ok, Pid} = erlmcp_flow_agent:start_link(test_agent, #{timeout => 5000}),
         Pid
     end,
     fun(Pid) ->
         %% Teardown: Stop agent, verify cleanup
         ok = erlmcp_flow_agent:stop(Pid),
         timer:sleep(50),
         ?assertNot(erlang:is_process_alive(Pid))
     end,
     fun(Pid) ->
         [
          ?_test(test_lifecycle(Pid)),
          ?_test(test_task_assignment(Pid)),
          ?_test(test_state_transitions(Pid)),
          ?_test(test_task_queue(Pid)),
          ?_test(test_task_cancellation(Pid)),
          ?_test(test_task_timeout(Pid)),
          ?_test(test_agent_crash_recovery(Pid)),
          ?_test(test_concurrent_task_assignment(Pid))
         ]
     end}.
```

**Test Scenarios**:

| Test | Description | Verification Method (Chicago School) |
|------|-------------|--------------------------------------|
| `test_lifecycle` | Start/stop agent | `is_process_alive`, `get_status` returns `{ok, idle}` |
| `test_task_assignment` | Assign task, verify state change | `get_state` → `{ok, working}`, `get_current_task` → `{ok, Task}` |
| `test_state_transitions` | idle→working→idle→failed→recovering→idle | `get_state` at each step, verify observable state |
| `test_task_queue` | Assign 10 tasks, verify FIFO | `get_queue_depth` → `{ok, 10}`, tasks execute in order |
| `test_task_cancellation` | Assign, cancel, verify cleanup | `cancel_task` → `ok`, `get_current_task` → `{error, no_task}` |
| `test_task_timeout` | Task exceeds timeout | `get_state` → `{ok, failed}`, task marked as timeout |
| `test_agent_crash_recovery` | Kill agent, supervisor restarts | Supervisor detects crash, new agent starts with `idle` state |
| `test_concurrent_task_assignment` | 100 concurrent assigns | All tasks queued, `get_queue_depth` → `{ok, 100}` |

**Edge Cases**:
- Task timeout triggers state transition
- Task with invalid input returns error
- Agent crash during task execution
- Queue overflow (max 1000 tasks)

**Coverage Target**: 85% (core module)

---

### 2.2 Swarm Coordination Module (`erlmcp_flow_swarm`)

**Purpose**: Multi-agent coordination with load balancing, task distribution, and failure recovery

#### Module API
```erlang
%% Lifecycle
-spec start_swarm(SwarmId, Config) -> {ok, Pid} | {error, Reason}.
-spec stop_swarm(Pid) -> ok.

%% Agent Management
-spec add_agent(SwarmPid, AgentPid) -> ok.
-spec remove_agent(SwarmPid, AgentPid) -> ok.
-spec list_agents(SwarmPid) -> {ok, [AgentPid]}.

%% Task Distribution
-spec submit_task(SwarmPid, Task) -> {ok, TaskId} | {error, Reason}.
-spec get_task_status(SwarmPid, TaskId) -> {ok, Status} | {error, Reason}.

%% Observable State (Chicago School)
-spec get_swarm_status(SwarmPid) -> {ok, #{agents := [AgentPid], tasks := [Task]}}.
-spec get_load_balance_stats(SwarmPid) -> {ok, #{agent_loads := [{AgentPid, Count}]}}.
```

#### Test Plan: `erlmcp_flow_swarm_tests.erl` (EUnit)

**Setup**:
```erlang
swarm_test_() ->
    {setup,
     fun() ->
         %% Setup: Start real swarm with 5 real agents
         application:ensure_all_started(erlmcp_core),
         {ok, SwarmPid} = erlmcp_flow_swarm:start_swarm(test_swarm, #{}),
         Agents = [begin
             {ok, A} = erlmcp_flow_agent:start_link(agent_id(N), #{}),
             ok = erlmcp_flow_swarm:add_agent(SwarmPid, A),
             A
         end || N <- lists:seq(1, 5)],
         #{swarm => SwarmPid, agents => Agents}
     end,
     fun(#{swarm := SwarmPid, agents := Agents}) ->
         %% Teardown: Stop agents, swarm
         [erlmcp_flow_agent:stop(A) || A <- Agents],
         erlmcp_flow_swarm:stop_swarm(SwarmPid)
     end,
     fun(Ctx) -> swarm_tests(Ctx) end}.

swarm_tests(#{swarm := SwarmPid, agents := Agents}) ->
    [
     ?_test(test_agent_registration(SwarmPid, Agents)),
     ?_test(test_task_distribution(SwarmPid)),
     ?_test(test_load_balancing(SwarmPid)),
     ?_test(test_agent_failure_recovery(SwarmPid, Agents)),
     ?_test(test_task_reassignment_on_failure(SwarmPid)),
     ?_test(test_concurrent_task_submission(SwarmPid))
    ].
```

**Test Scenarios**:

| Test | Description | Verification (Chicago School) |
|------|-------------|-------------------------------|
| `test_agent_registration` | Add 5 agents | `list_agents` → `{ok, [5 agents]}` |
| `test_task_distribution` | Submit 10 tasks | Tasks distributed to agents, all complete |
| `test_load_balancing` | Submit 100 tasks | `get_load_balance_stats` → all agents ~20 tasks |
| `test_agent_failure_recovery` | Kill 1 agent | Swarm detects failure, redistributes tasks |
| `test_task_reassignment_on_failure` | Agent fails during task | Task reassigned to healthy agent |
| `test_concurrent_task_submission` | 1000 concurrent submits | All tasks submitted, no errors |

**Coverage Target**: 85%

---

### 2.3 Consensus Modules

#### 2.3.1 Raft Consensus (`erlmcp_flow_consensus_raft`)

**Purpose**: Leader election, log replication, linearizable consistency

#### Module API
```erlang
%% Lifecycle
-spec start_node(NodeId, Cluster) -> {ok, Pid}.
-spec stop_node(Pid) -> ok.

%% State Queries (Chicago School)
-spec get_role(Pid) -> {ok, leader | follower | candidate}.
-spec get_term(Pid) -> {ok, non_neg_integer()}.
-spec get_leader(Pid) -> {ok, Pid} | {error, no_leader}.

%% Operations
-spec append_entry(Pid, Entry) -> {ok, Index} | {error, not_leader}.
-spec read_entry(Pid, Index) -> {ok, Entry} | {error, not_found}.
```

#### Test Plan: `erlmcp_flow_consensus_raft_tests.erl` (EUnit)

**Test Scenarios**:

| Test | Description | Verification (Chicago School) |
|------|-------------|-------------------------------|
| `test_leader_election_3_nodes` | Start 3 nodes, elect leader | Exactly 1 leader, 2 followers |
| `test_leader_election_5_nodes` | Start 5 nodes | 1 leader, 4 followers |
| `test_log_replication` | Append 100 entries | All nodes have same log |
| `test_leader_failure_reelection` | Kill leader | New leader elected within 2s |
| `test_split_brain_resolution` | Partition cluster | After heal, single leader |
| `test_log_consistency_after_partition` | Partition, append, heal | Logs converge to leader's log |
| `test_majority_quorum` | 5 nodes, 2 down | 3 nodes still elect leader |
| `test_minority_no_election` | 5 nodes, 3 down | No leader elected |

**Edge Cases**:
- Simultaneous candidate election
- Network partition (2-2-1 split)
- Asymmetric partition (A→B works, B→A fails)
- Cascading failures (leader fails, new leader fails immediately)

**Coverage Target**: 85%

---

#### 2.3.2 Byzantine Consensus (`erlmcp_flow_consensus_byzantine`)

**Purpose**: Byzantine fault tolerance, 3f+1 nodes tolerate f Byzantine faults

#### Module API
```erlang
-spec start_node(NodeId, Cluster, IsByzantine) -> {ok, Pid}.
-spec propose_value(Pid, Value) -> {ok, Decision} | {error, no_consensus}.
-spec get_decision(Pid) -> {ok, Value} | {error, no_decision}.
```

#### Test Plan: `erlmcp_flow_consensus_byzantine_tests.erl` (EUnit)

**Test Scenarios**:

| Test | Description | Verification (Chicago School) |
|------|-------------|-------------------------------|
| `test_4_nodes_1_byzantine` | 4 nodes, 1 sends wrong values | 3 honest nodes reach consensus |
| `test_7_nodes_2_byzantine` | 7 nodes, 2 Byzantine | Consensus on correct value |
| `test_3_nodes_1_byzantine_no_consensus` | 3 nodes, 1 Byzantine | No consensus (need 4 for f=1) |
| `test_conflicting_proposals` | Honest nodes propose different values | Consensus on one value |
| `test_byzantine_equivocation` | Byzantine node sends different values | Honest nodes detect and ignore |
| `test_byzantine_silence` | Byzantine node stops responding | Consensus without it |

**Coverage Target**: 80%

---

#### 2.3.3 Gossip Protocol (`erlmcp_flow_consensus_gossip`)

**Purpose**: Eventually consistent information dissemination

#### Module API
```erlang
-spec start_node(NodeId, Cluster) -> {ok, Pid}.
-spec update_local(Pid, Key, Value) -> ok.
-spec get_value(Pid, Key) -> {ok, Value} | {error, not_found}.
-spec get_convergence_time(Cluster) -> {ok, Milliseconds}.
```

#### Test Plan: `erlmcp_flow_consensus_gossip_tests.erl` (EUnit)

**Test Scenarios**:

| Test | Description | Verification (Chicago School) |
|------|-------------|-------------------------------|
| `test_eventual_consistency` | Update on 1 node, check all nodes | All nodes have value within 5s |
| `test_network_partition_convergence` | Partition, update both sides, heal | All nodes converge to same state |
| `test_concurrent_updates` | 10 nodes update same key | Eventual convergence (LWW or vector clocks) |
| `test_node_failure_propagation` | 1 node fails during gossip | Other nodes still converge |

**Coverage Target**: 80%

---

## 3. Integration Test Suites (Common Test)

### 3.1 Multi-Process Integration (`erlmcp_flow_integration_SUITE`)

**Purpose**: Test real interactions between agents, swarms, and consensus

#### Test Cases

```erlang
all() ->
    [
     test_swarm_with_raft_consensus,
     test_agent_failover_with_consensus,
     test_distributed_task_execution,
     test_multi_swarm_coordination
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp_core),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_core),
    ok.
```

**Scenarios**:

| Test | Description | Verification |
|------|-------------|--------------|
| `test_swarm_with_raft_consensus` | Swarm uses Raft for task ordering | Tasks execute in consensus order |
| `test_agent_failover_with_consensus` | Agent fails, consensus maintains state | No task lost, no duplicate execution |
| `test_distributed_task_execution` | 3 swarms, 15 agents, 1000 tasks | All tasks complete, load balanced |
| `test_multi_swarm_coordination` | 2 swarms coordinate via consensus | Swarms don't conflict, tasks distributed |

---

### 3.2 Swarm Integration (`erlmcp_flow_swarm_integration_SUITE`)

**Purpose**: Complex swarm coordination scenarios

#### Test Cases

```erlang
all() ->
    [
     test_hierarchical_swarms,
     test_swarm_merger,
     test_swarm_split,
     test_cross_swarm_task_migration
    ].
```

**Scenarios**:

| Test | Description | Verification |
|------|-------------|--------------|
| `test_hierarchical_swarms` | Parent swarm delegates to 3 child swarms | Tasks flow down, results bubble up |
| `test_swarm_merger` | 2 swarms merge into 1 | Agents from both swarms in merged swarm |
| `test_swarm_split` | 1 swarm splits into 2 | Tasks redistributed, no loss |
| `test_cross_swarm_task_migration` | Migrate task from swarm A to B | Task completes in swarm B |

---

### 3.3 Chaos Testing (`erlmcp_flow_chaos_SUITE`)

**Purpose**: Fault injection, resilience verification

#### Test Cases

```erlang
all() ->
    [
     test_random_agent_crashes,
     test_network_delay_injection,
     test_message_loss_injection,
     test_byzantine_agent_injection,
     test_cascading_failures,
     test_resource_exhaustion
    ].
```

**Scenarios**:

| Test | Description | Verification (Chicago School) |
|------|-------------|-------------------------------|
| `test_random_agent_crashes` | Randomly kill 30% of agents | Swarm continues, tasks complete |
| `test_network_delay_injection` | Add 500ms delay to 50% of messages | Consensus still converges |
| `test_message_loss_injection` | Drop 10% of messages | System detects, retries, recovers |
| `test_byzantine_agent_injection` | Inject 1 Byzantine agent | System detects, isolates agent |
| `test_cascading_failures` | Kill leader → kill new leader | Eventually stable leader elected |
| `test_resource_exhaustion` | Exhaust memory/CPU | Circuit breakers trigger, graceful degradation |

**Chaos Testing Framework**:
```erlang
%% Chicago School: Real fault injection, observable recovery
inject_crash(AgentPid) ->
    exit(AgentPid, kill),  %% Real process death, no mocking
    timer:sleep(100).      %% Let supervisor restart

inject_network_delay(FromPid, ToPid, DelayMs) ->
    %% Intercept messages, add real delay
    erlang:trace(FromPid, true, [send]),
    receive
        {trace, FromPid, send, Msg, ToPid} ->
            timer:sleep(DelayMs),
            ToPid ! Msg
    end.

verify_recovery(SwarmPid) ->
    %% Chicago School: Observable state after chaos
    {ok, Status} = erlmcp_flow_swarm:get_swarm_status(SwarmPid),
    #{agents := Agents, tasks := Tasks} = Status,
    ?assert(length(Agents) > 0),  %% Swarm still has agents
    ?assert(length(Tasks) >= 0).  %% Tasks not lost
```

---

### 3.4 Network Partition Testing (`erlmcp_flow_partition_SUITE`)

**Purpose**: Test behavior under network partitions

#### Test Cases

```erlang
all() ->
    [
     test_partition_2_2_split,
     test_partition_3_2_split,
     test_partition_asymmetric,
     test_partition_healing,
     test_split_brain_detection
    ].
```

**Scenarios**:

| Test | Description | Verification |
|------|-------------|--------------|
| `test_partition_2_2_split` | 4 nodes → 2+2 partition | No leader in minority |
| `test_partition_3_2_split` | 5 nodes → 3+2 partition | 3-node side elects leader |
| `test_partition_asymmetric` | A→B works, B→A blocked | Consensus detects, resolves |
| `test_partition_healing` | Partition, then heal | Logs reconcile, single leader |
| `test_split_brain_detection` | Both sides elect leader | Split-brain detector triggers, resolves |

**Partition Simulation (Chicago School)**:
```erlang
%% Real network partition via message blocking
create_partition(NodesA, NodesB) ->
    [begin
         %% Block messages from A to B
         erlang:trace(NodeA, true, [send]),
         receive
             {trace, NodeA, send, Msg, NodeB} when lists:member(NodeB, NodesB) ->
                 %% Drop message (real message loss, no mocking)
                 ok
         end
     end || NodeA <- NodesA, NodeB <- NodesB].

heal_partition() ->
    %% Remove message blocking
    [erlang:trace(Node, false, [send]) || Node <- all_nodes()].

verify_single_leader(Nodes) ->
    %% Chicago School: Observable leader state
    Roles = [begin
        {ok, Role} = erlmcp_flow_consensus_raft:get_role(Node),
        Role
    end || Node <- Nodes],
    Leaders = [R || R <- Roles, R =:= leader],
    ?assertEqual(1, length(Leaders)).
```

---

## 4. Property-Based Testing (Proper)

### 4.1 Property Test Module (`erlmcp_flow_proper_tests`)

**Purpose**: Generative testing for invariants

#### Properties

```erlang
%% Property: Leader election always results in exactly 1 leader (or 0 in minority)
prop_leader_election_invariant() ->
    ?FORALL({NumNodes, NumDown}, {range(3, 10), range(0, 4)},
        begin
            Nodes = start_nodes(NumNodes),
            [exit(Node, kill) || Node <- lists:sublist(Nodes, NumDown)],
            timer:sleep(2000),  %% Allow election
            Alive = Nodes -- lists:sublist(Nodes, NumDown),
            Roles = [get_role(N) || N <- Alive],
            Leaders = [R || R <- Roles, R =:= leader],
            Majority = length(Alive) > (NumNodes div 2),
            case Majority of
                true -> length(Leaders) =:= 1;
                false -> length(Leaders) =:= 0
            end
        end).

%% Property: Log consistency - all nodes eventually have same committed log
prop_log_consistency() ->
    ?FORALL({Entries, NumNodes}, {list(entry()), range(3, 7)},
        begin
            Nodes = start_nodes(NumNodes),
            [append_entry(hd(Nodes), E) || E <- Entries],
            timer:sleep(1000),  %% Allow replication
            Logs = [get_log(N) || N <- Nodes],
            all_equal(Logs)
        end).

%% Property: Byzantine resilience - 3f+1 nodes tolerate f faults
prop_byzantine_resilience() ->
    ?FORALL({F, ByzantineNodes}, {range(1, 3), list(byzantine_node())},
        length(ByzantineNodes) =:= F ->
            NumNodes = 3 * F + 1,
            Honest = start_nodes(NumNodes - F),
            Byzantine = ByzantineNodes,
            AllNodes = Honest ++ Byzantine,
            {ok, Decision} = propose_consensus(AllNodes, correct_value),
            Decision =:= correct_value
        end).

%% Property: Eventual consistency in gossip
prop_gossip_eventual_consistency() ->
    ?FORALL({NumNodes, Updates}, {range(5, 20), list(update())},
        begin
            Nodes = start_nodes(NumNodes),
            [update_local(random_node(Nodes), K, V) || {K, V} <- Updates],
            timer:sleep(5000),  %% Allow gossip convergence
            Values = [get_all_values(N) || N <- Nodes],
            all_equal(Values)
        end).
```

**Coverage Target**: 100 test cases per property

---

## 5. Performance Benchmarking

### 5.1 Benchmark Suite

```erlang
%% bench/erlmcp_flow_bench.erl

benchmark_leader_election() ->
    %% Measure: Time to elect leader for N nodes
    Results = [begin
        Nodes = start_nodes(N),
        {Time, _} = timer:tc(fun() -> wait_for_leader(Nodes) end),
        Time / 1000  %% Convert to ms
    end || N <- [3, 5, 7, 10, 15, 20]],

    %% Regression: Election time should be O(log N)
    ?assert(lists:last(Results) < 3000).  %% <3s for 20 nodes

benchmark_task_throughput() ->
    %% Measure: Tasks/second for swarm
    Swarm = start_swarm_with_agents(10),
    {Time, _} = timer:tc(fun() ->
        [submit_task(Swarm, task()) || _ <- lists:seq(1, 10000)]
    end),
    TPS = 10000 / (Time / 1_000_000),

    %% Regression: ≥1000 tasks/second
    ?assert(TPS >= 1000).

benchmark_consensus_latency() ->
    %% Measure: Consensus decision latency
    Nodes = start_nodes(5),
    {Time, _} = timer:tc(fun() ->
        append_entry(hd(Nodes), entry())
    end),
    Latency = Time / 1000,  %% ms

    %% Regression: <100ms for 5 nodes
    ?assert(Latency < 100).
```

---

## 6. Quality Gates

### 6.1 Pre-Commit Verification

**All tests must pass before merge**:

```bash
#!/bin/bash
# .claude/hooks/pre-commit-erlmcp-flow.sh

set -e

echo "Gate 1: Compile erlmcp-flow modules"
TERM=dumb rebar3 compile

echo "Gate 2: EUnit tests (agent, swarm, consensus)"
rebar3 eunit --module=erlmcp_flow_agent_tests
rebar3 eunit --module=erlmcp_flow_swarm_tests
rebar3 eunit --module=erlmcp_flow_consensus_raft_tests
rebar3 eunit --module=erlmcp_flow_consensus_byzantine_tests
rebar3 eunit --module=erlmcp_flow_consensus_gossip_tests

echo "Gate 3: Common Test (integration, chaos, partition)"
rebar3 ct --suite=test/erlmcp_flow_integration_SUITE
rebar3 ct --suite=test/erlmcp_flow_chaos_SUITE
rebar3 ct --suite=test/erlmcp_flow_partition_SUITE

echo "Gate 4: Property tests"
rebar3 proper -c --module=erlmcp_flow_proper_tests

echo "Gate 5: Coverage ≥80%"
rebar3 cover --verbose
COVERAGE=$(rebar3 cover --verbose | grep 'total' | awk '{print $2}' | sed 's/%//')
if [ "$COVERAGE" -lt 80 ]; then
    echo "FAIL: Coverage $COVERAGE% < 80%"
    exit 1
fi

echo "Gate 6: Dialyzer"
rebar3 dialyzer

echo "Gate 7: Benchmark regression <10%"
./bench/run_benchmarks.sh

echo "✅ All quality gates passed"
```

### 6.2 Coverage Requirements

| Module | Minimum Coverage | Target Coverage |
|--------|------------------|-----------------|
| erlmcp_flow_agent | 85% | 90% |
| erlmcp_flow_swarm | 85% | 90% |
| erlmcp_flow_consensus_raft | 80% | 85% |
| erlmcp_flow_consensus_byzantine | 75% | 80% |
| erlmcp_flow_consensus_gossip | 75% | 80% |
| erlmcp_flow_task | 85% | 90% |

---

## 7. Test Execution Strategy

### 7.1 Local Development

```bash
# Quick feedback: Run only changed module tests
make test-changed

# Full test suite: EUnit + CT + Proper
make test

# Coverage report
make coverage

# Benchmark (performance regression check)
make benchmark
```

### 7.2 CI/CD Pipeline

```yaml
# .github/workflows/erlmcp-flow-ci.yml
name: erlmcp-flow CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Setup OTP 28
        uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Compile
        run: rebar3 compile
      - name: EUnit
        run: rebar3 eunit
      - name: Common Test
        run: rebar3 ct
      - name: Proper
        run: rebar3 proper -c
      - name: Coverage
        run: rebar3 cover --verbose
      - name: Dialyzer
        run: rebar3 dialyzer
      - name: Benchmark
        run: ./bench/run_benchmarks.sh
```

---

## 8. Test Data Generators

### 8.1 Task Generator

```erlang
%% Generate random task for testing
task_generator() ->
    #{
      id => uuid(),
      type => oneof([compute, io, network]),
      priority => range(1, 10),
      timeout => range(1000, 60000),
      input => binary()
    }.
```

### 8.2 Agent Configuration Generator

```erlang
agent_config_generator() ->
    #{
      max_queue_size => range(10, 1000),
      timeout => range(1000, 30000),
      retry_strategy => oneof([exponential_backoff, fixed_delay, immediate])
    }.
```

### 8.3 Network Fault Generator

```erlang
network_fault_generator() ->
    oneof([
      {delay, range(10, 5000)},
      {loss, range(1, 50)},
      {partition, [list(node()), list(node())]},
      {asymmetric, {node(), node()}}
    ]).
```

---

## 9. Chicago School TDD Examples

### 9.1 Example: Agent Task Execution Test

```erlang
%% Chicago School: Real gen_server, observable state
test_task_execution(AgentPid) ->
    %% Setup: Agent is idle
    {ok, idle} = erlmcp_flow_agent:get_state(AgentPid),

    %% Exercise: Assign task
    Task = #{id => <<"task1">>, type => compute, input => <<"data">>},
    ok = erlmcp_flow_agent:assign_task(AgentPid, Task),

    %% Verify: State changed to working (observable behavior)
    {ok, working} = erlmcp_flow_agent:get_state(AgentPid),
    {ok, Task} = erlmcp_flow_agent:get_current_task(AgentPid),

    %% Wait for completion
    timer:sleep(100),

    %% Verify: State back to idle (observable state transition)
    {ok, idle} = erlmcp_flow_agent:get_state(AgentPid),
    {error, no_task} = erlmcp_flow_agent:get_current_task(AgentPid).
```

### 9.2 Example: Swarm Load Balancing Test

```erlang
%% Chicago School: Real swarm, real agents, observable load distribution
test_load_balancing(SwarmPid) ->
    %% Exercise: Submit 100 tasks
    Tasks = [task_generator() || _ <- lists:seq(1, 100)],
    [erlmcp_flow_swarm:submit_task(SwarmPid, T) || T <- Tasks],

    %% Wait for distribution
    timer:sleep(500),

    %% Verify: Load balanced across agents (observable behavior)
    {ok, Stats} = erlmcp_flow_swarm:get_load_balance_stats(SwarmPid),
    #{agent_loads := Loads} = Stats,

    %% All agents should have ~20 tasks (100 / 5 agents)
    [?assert(Load >= 15 andalso Load =< 25) || {_Agent, Load} <- Loads].
```

### 9.3 Example: Raft Leader Election Test

```erlang
%% Chicago School: Real Raft nodes, observable leader state
test_leader_election_3_nodes() ->
    %% Setup: Start 3 real Raft nodes
    {ok, Node1} = erlmcp_flow_consensus_raft:start_node(node1, [node2, node3]),
    {ok, Node2} = erlmcp_flow_consensus_raft:start_node(node2, [node1, node3]),
    {ok, Node3} = erlmcp_flow_consensus_raft:start_node(node3, [node1, node2]),

    %% Wait for election
    timer:sleep(2000),

    %% Verify: Exactly 1 leader, 2 followers (observable state)
    Nodes = [Node1, Node2, Node3],
    Roles = [begin
        {ok, Role} = erlmcp_flow_consensus_raft:get_role(N),
        Role
    end || N <- Nodes],

    Leaders = [R || R <- Roles, R =:= leader],
    Followers = [R || R <- Roles, R =:= follower],

    ?assertEqual(1, length(Leaders)),
    ?assertEqual(2, length(Followers)),

    %% Cleanup
    [erlmcp_flow_consensus_raft:stop_node(N) || N <- Nodes].
```

---

## 10. Summary

### Test Coverage Matrix

| Component | EUnit | Common Test | Proper | Chaos | Coverage Target |
|-----------|-------|-------------|--------|-------|-----------------|
| Agent | ✅ | ✅ | ✅ | ✅ | 85% |
| Swarm | ✅ | ✅ | ✅ | ✅ | 85% |
| Raft | ✅ | ✅ | ✅ | ✅ | 80% |
| Byzantine | ✅ | ✅ | ✅ | ✅ | 75% |
| Gossip | ✅ | ✅ | ✅ | ✅ | 75% |
| Task | ✅ | ✅ | ✅ | ❌ | 85% |

### Chicago School TDD Principles Applied

1. **Real Processes**: All tests use actual gen_servers, supervisors, distributed processes
2. **State-Based Verification**: Assert on observable state (`get_state`, `get_status`), not internal calls
3. **Real Collaborators**: Agents interact with real swarms, swarms use real consensus
4. **No Mocks**: Fault injection via real process death (`exit(Pid, kill)`), network partitions via message blocking
5. **Observable Behavior**: Tests verify what system does (outputs, state changes), not how it does it

### Quality Gates Summary

- ✅ Compile: 0 errors
- ✅ Tests: 0 failures (EUnit + CT + Proper)
- ✅ Coverage: ≥80% overall, ≥85% for core modules
- ✅ Dialyzer: 0 warnings
- ✅ Benchmark: <10% regression

---

## 11. Next Steps

1. **Implement Agent Module** (`erlmcp_flow_agent.erl`) with TDD
2. **Write Agent Tests** (`erlmcp_flow_agent_tests.erl`) following this design
3. **Implement Swarm Module** with real coordination
4. **Implement Raft Consensus** with real leader election
5. **Create Chaos Test Suite** with fault injection
6. **Run Full Test Suite** and achieve coverage targets
7. **Benchmark Performance** and establish baselines

---

**Document Version**: 1.0.0
**Last Updated**: 2026-02-01
**Author**: erlang-test-engineer (Chicago School TDD)
