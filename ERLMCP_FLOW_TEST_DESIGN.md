# erlmcp-flow Test Design Document

**Version**: 1.0.0
**Date**: 2026-02-02
**Phase**: Week 4 - Agent Framework Integration Testing

---

## Overview

This document specifies the integration test strategy for erlmcp-flow Week 4 Day 2, focusing on 6 critical end-to-end scenarios that validate agent lifecycle, task management, swarm coordination, and fault tolerance.

## Test Objectives

### Primary Goals
1. Validate complete task lifecycle: creation → assignment → execution → completion
2. Verify agent crash recovery and task reassignment
3. Test swarm coordinator election and failover
4. Validate task timeout handling and requeue mechanisms
5. Ensure multi-swarm isolation and resource boundaries
6. Verify leader change during active task execution

### Quality Gates
- **Pass Rate**: 100% (6/6 tests must pass)
- **Execution Time**: < 120 seconds total
- **Coverage**: Integration paths for agent, swarm, registry modules
- **Failure Mode**: Stop-the-line on any failure

---

## Week 4 Test Matrix

Based on the 12-Week Roadmap, Week 4 corresponds to:
- **Phase**: Agent Framework (Tasks 18-33)
- **Modules Under Test**:
  - `erlmcp_flow_agent.erl` (Task 19-22: Agent types)
  - `erlmcp_flow_swarm.erl` (Task 35: Swarm coordinator - early preview)
  - `erlmcp_flow_registry.erl` (Task 5: Foundation)
  - `erlmcp_flow_task_queue.erl` (Task 24: Task management)
  - `erlmcp_flow_task_lifecycle.erl` (Task 25: Lifecycle)

---

## Test Suite Specification

### Test Suite: `erlmcp_flow_integration_SUITE`

**Location**: `apps/erlmcp_flow/test/erlmcp_flow_integration_SUITE.erl`

**Test Cases**: 6 integration scenarios

---

## Test Case 1: Task Creation → Assignment → Execution → Completion

### Objective
Validate the complete happy-path task lifecycle from submission to completion.

### Setup
1. Start `erlmcp_flow_registry` process
2. Start `erlmcp_flow_swarm` coordinator with SwarmId = `<<"swarm-001">>`
3. Start 3 agents: `agent-001`, `agent-002`, `agent-003`
4. Register all agents with swarm

### Test Sequence
```erlang
1. Submit task: #{id => <<"task-001">>, type => <<"compute">>,
                  input => #{operation => add, operands => [1, 2]}}
2. Wait for swarm to assign task to agent
3. Agent transitions: idle → assigned → executing → done
4. Verify task result: {ok, 3}
5. Verify swarm stats: tasks_completed = 1
```

### Acceptance Criteria
- ✅ Task assigned to agent within 100ms
- ✅ Agent state transitions correct
- ✅ Task result matches expected output
- ✅ Swarm metrics updated correctly
- ✅ Agent returns to `idle` state after completion

### Timeout
10 seconds

---

## Test Case 2: Agent Crash Recovery

### Objective
Verify that tasks are reassigned when an agent crashes during execution.

### Setup
1. Start registry, swarm, and 3 agents
2. Submit task with 5s execution time

### Test Sequence
```erlang
1. Assign task to agent-001
2. Verify agent-001 state = executing
3. Kill agent-001 process: exit(AgentPid, kill)
4. Wait for swarm health check (10s heartbeat interval)
5. Swarm detects missed heartbeats (3 consecutive)
6. Task requeued and assigned to agent-002
7. Agent-002 completes task successfully
```

### Acceptance Criteria
- ✅ Swarm detects agent crash within 30s (3 × 10s heartbeat)
- ✅ Task requeued automatically
- ✅ Task reassigned to healthy agent
- ✅ Task completes successfully on agent-002
- ✅ Swarm metrics: agents_removed = 1

### Timeout
60 seconds

---

## Test Case 3: Swarm Coordinator Election

### Objective
Validate that a new swarm coordinator is elected when the leader crashes.

### Setup
1. Start 3 swarm coordinators in a cluster
2. Enable Raft consensus mode
3. Wait for leader election

### Test Sequence
```erlang
1. Identify current leader via get_status/1
2. Submit 5 tasks to leader
3. Kill leader process
4. Wait for new election (<2s per Raft spec)
5. Verify new leader elected
6. Submit 5 more tasks to new leader
7. Verify all 10 tasks eventually complete
```

### Acceptance Criteria
- ✅ Initial leader election within 2s
- ✅ New leader election after crash within 2s
- ✅ Tasks submitted before crash are not lost
- ✅ New leader accepts new tasks
- ✅ Exactly 1 leader at any time (election safety)

### Timeout
30 seconds

---

## Test Case 4: Task Timeout + Requeue

### Objective
Verify that tasks that exceed timeout are terminated and requeued.

### Setup
1. Start registry, swarm, and 2 agents
2. Configure task timeout = 2s

### Test Sequence
```erlang
1. Submit task: #{id => <<"timeout-task">>, timeout => 2000,
                  action => fun() -> timer:sleep(10000) end}
2. Task assigned to agent-001
3. Wait 2s for timeout
4. Verify task killed and requeued
5. Task reassigned to agent-002 (retry #1)
6. Agent-002 also times out
7. After 3 retries, task marked as failed
```

### Acceptance Criteria
- ✅ Task terminated after 2s timeout
- ✅ Task requeued automatically
- ✅ Max 3 retry attempts
- ✅ Task status = `failed` after 3 retries
- ✅ Swarm metrics: tasks_failed = 1

### Timeout
15 seconds

---

## Test Case 5: Multi-Swarm Isolation

### Objective
Verify that multiple swarms operate independently with isolated resources.

### Setup
1. Start registry
2. Start swarm-001 with agents: agent-A1, agent-A2
3. Start swarm-002 with agents: agent-B1, agent-B2

### Test Sequence
```erlang
1. Submit 10 tasks to swarm-001
2. Submit 10 tasks to swarm-002
3. Verify swarm-001 agents only process swarm-001 tasks
4. Verify swarm-002 agents only process swarm-002 tasks
5. Kill agent-A1 (swarm-001 agent)
6. Verify swarm-002 unaffected
7. Verify swarm-001 reassigns tasks to agent-A2
```

### Acceptance Criteria
- ✅ Agent isolation: swarm-001 agents never receive swarm-002 tasks
- ✅ Task isolation: 10 tasks each swarm
- ✅ Crash isolation: swarm-001 crash doesn't affect swarm-002
- ✅ Independent metrics per swarm
- ✅ Registry tracks agents per swarm

### Timeout
20 seconds

---

## Test Case 6: Leader Change During Task Execution

### Objective
Verify that in-progress tasks survive a leader failover in Raft mode.

### Setup
1. Start 3-node Raft cluster
2. Wait for leader election
3. Start 5 agents registered to cluster

### Test Sequence
```erlang
1. Submit 20 tasks to leader (each task takes 5s)
2. Wait for 10 tasks to be assigned (agents executing)
3. Kill current leader process
4. New leader elected within 2s
5. Verify 10 in-progress tasks continue execution
6. Verify 10 remaining queued tasks assigned by new leader
7. All 20 tasks complete successfully
```

### Acceptance Criteria
- ✅ Leader failover within 2s
- ✅ In-progress tasks (executing state) survive failover
- ✅ Queued tasks reassigned by new leader
- ✅ No task loss (all 20 complete)
- ✅ Task results correct

### Timeout
45 seconds

---

## Test Infrastructure Requirements

### Dependencies
- OTP 28+ (required for compilation)
- `gproc` 0.9.0 (agent registry)
- `erlmcp_core` (base infrastructure)

### Helper Modules
- `erlmcp_flow_test_helpers.erl`: Common setup/teardown functions
- `erlmcp_flow_test_tasks.erl`: Sample task implementations

### Common Setup
```erlang
init_per_suite(Config) ->
    application:ensure_all_started(gproc),
    application:ensure_all_started(erlmcp_flow),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_flow),
    application:stop(gproc),
    ok.

init_per_testcase(TestCase, Config) ->
    % Start fresh registry for each test
    {ok, RegPid} = erlmcp_flow_registry:start_link(),
    [{registry_pid, RegPid} | Config].

end_per_testcase(_TestCase, Config) ->
    RegPid = ?config(registry_pid, Config),
    gen_server:stop(RegPid),
    ok.
```

---

## Execution Plan

### Command
```bash
rebar3 ct --suite=apps/erlmcp_flow/test/erlmcp_flow_integration_SUITE
```

### Expected Output
```
Testing erlmcp_flow.erlmcp_flow_integration_SUITE:
  TEST CASE 1: task_lifecycle_complete_flow .......... OK (3.2s)
  TEST CASE 2: agent_crash_recovery ................. OK (18.5s)
  TEST CASE 3: swarm_coordinator_election ........... OK (12.1s)
  TEST CASE 4: task_timeout_and_requeue ............. OK (8.7s)
  TEST CASE 5: multi_swarm_isolation ................ OK (14.3s)
  TEST CASE 6: leader_change_during_execution ....... OK (22.4s)

All 6 tests passed (79.2s)
```

### Failure Scenarios
If any test fails:
1. **STOP THE LINE** (Jidoka principle)
2. Capture failure logs
3. Save CT logs to `_build/test/logs/`
4. Generate failure report with stack trace
5. Trigger Andon signal for team investigation

---

## Success Criteria Summary

| Metric | Target | Measured |
|--------|--------|----------|
| Pass Rate | 100% | TBD |
| Execution Time | < 120s | TBD |
| Agent Crash Recovery | < 30s | TBD |
| Leader Election | < 2s | TBD |
| Task Timeout Detection | < 2s | TBD |
| Multi-Swarm Isolation | 100% | TBD |

---

## Known Issues & Blockers

### OTP Version Requirement
- **Required**: OTP 28+
- **Current**: OTP 25 (on some systems)
- **Impact**: Compilation fails
- **Resolution**: Install OTP 28+ or use custom OTP from `~/.erlmcp/otp-28.3.1/`

### Raft Integration
- **Status**: Partial implementation (Task 51-57 in Week 7-8)
- **Impact**: Test cases 3 and 6 require Raft module
- **Workaround**: Mock Raft behavior for Week 4 testing

---

## References

- **Roadmap**: `ERLMCP_FLOW_12_WEEK_ROADMAP.md` - Tasks 18-33 (Week 3-4)
- **Implementation Plan**: `erlmcp-flow-implementation-plan.md`
- **Agent Design**: `erlmcp_flow_agent_design.md`
- **CLAUDE.md**: Quality gates and OTP design patterns

---

**Document Status**: READY FOR EXECUTION
**Approval**: Week 4 Day 2 Test Execution
**Next Step**: Execute `rebar3 ct --suite=erlmcp_flow_integration_SUITE`
