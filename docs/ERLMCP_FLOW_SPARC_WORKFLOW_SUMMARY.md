# erlmcp-flow SPARC Workflow Design Summary

**Version**: 1.0.0
**Date**: 2026-02-02
**Status**: Design Complete, Ready for Implementation

---

## Overview

The erlmcp-flow SPARC Workflow is a comprehensive orchestration system that implements the SPARC methodology (Specification → Pseudocode → Architecture → Refinement → Completion) using Erlang/OTP state machines with:

- 5-phase workflow coordination
- Byzantine-fault-tolerant consensus (Raft/PBFT)
- Adaptive routing with performance feedback
- Comprehensive error recovery (task failure, Byzantine faults, network partitions)
- Receipt generation for audit trails

---

## Key Design Decisions

### 1. State Machine Architecture

All components use `gen_statem` with:
- `callback_mode`: `[handle_event_function, state_enter_calls]`
- Priority message handling (OTP 28 feature)
- State entry actions for setup/teardown
- Clear state transition paths

### 2. 5-Phase SPARC Workflow

```
idle → specification → pseudocode → architecture → refinement → completion → idle
```

Each phase has a dedicated FSM:
- **Specification**: Spec Parser FSM (parse, validate)
- **Pseudocode**: Planner FSM (analyze, assign, route)
- **Architecture**: Topology & Consensus design
- **Refinement**: Executor FSM (dispatch, execute, monitor)
- **Completion**: Receipt Generator (collect, sign, store)

### 3. Error Recovery Strategy

Three primary error scenarios with dedicated recovery:

| Error Type | Detection | Recovery Strategy | Fallback |
|------------|-----------|-------------------|----------|
| Task Failure | Task timeout or explicit failure | Replan with alternative agent | Escalate to user |
| Byzantine Fault | Message signature mismatch | Switch Raft → PBFT, exclude bad actor | Halt if quorum lost |
| Network Partition | Heartbeat timeout | Halt writes, wait for heal with exponential backoff | Manual intervention |

### 4. Consensus Protocol Selection

**Raft** (Low latency, trusted environment):
- Internal agents only
- 150-300ms consensus latency
- Simple leader election
- Used when Byzantine risk is low

**PBFT** (High fault tolerance, untrusted environment):
- External agents or mixed trust
- 300-500ms consensus latency
- Tolerates f Byzantine faults with 3f+1 nodes
- Used when Byzantine risk is high

### 5. Topology Selection

**Mesh** (≤10 agents):
- Full connectivity (O(N²) edges)
- Low latency, high redundancy
- Best for small teams

**Hierarchical** (>10 agents):
- Tree structure (O(N) edges)
- Coordinator nodes for routing
- Scalability over redundancy

### 6. Adaptive Routing

Performance-based routing adjustments:
```erlang
health_score = 0.4 * latency_factor + 0.4 * success_factor + 0.2 * throughput_factor
```

Routes are adjusted dynamically based on:
- Latency (p99 < 1000ms target)
- Failure rate (< 5% target)
- Throughput (> 100 tasks/s target)

Agents with health < 0.5 are avoided, tasks re-routed to healthier agents.

---

## Module Structure

```
erlmcp_flow/
├── erlmcp_flow.erl                      % Public API
├── erlmcp_flow_app.erl                  % Application callback
├── erlmcp_flow_sup.erl                  % Top-level supervisor
│
├── erlmcp_flow_sparc_orchestrator.erl   % Main coordinator (gen_statem)
├── erlmcp_flow_spec_parser.erl          % Input parser (gen_statem)
├── erlmcp_flow_planner.erl              % Task planner (gen_statem)
├── erlmcp_flow_executor.erl             % Task executor (gen_statem)
├── erlmcp_flow_monitor.erl              % Monitoring (gen_statem)
├── erlmcp_flow_receipt.erl              % Receipt server (gen_server)
├── erlmcp_flow_error_recovery.erl       % Error recovery (gen_statem)
│
├── erlmcp_flow_registry.erl             % Agent registry (gen_server + gproc)
├── erlmcp_flow_router.erl               % Routing logic (pure)
├── erlmcp_flow_topology.erl             % Topology builder (pure)
├── erlmcp_flow_consensus.erl            % Consensus interface (behavior)
├── erlmcp_flow_raft.erl                 % Raft implementation
├── erlmcp_flow_pbft.erl                 % PBFT implementation
│
└── Supervision trees (6 supervisors)
```

**Total**: 14 modules + 6 supervisors = 20 files

---

## Supervision Tree

```
erlmcp_flow_sup (one_for_all)
├── erlmcp_flow_registry (gen_server)
├── erlmcp_flow_sparc_sup (one_for_one)
│   ├── erlmcp_flow_sparc_orchestrator_sup (simple_one_for_one)
│   ├── erlmcp_flow_spec_parser_sup (simple_one_for_one)
│   ├── erlmcp_flow_planner_sup (simple_one_for_one)
│   ├── erlmcp_flow_executor_sup (simple_one_for_one)
│   └── erlmcp_flow_monitor_sup (simple_one_for_one)
├── erlmcp_flow_receipt_server (gen_server)
└── erlmcp_flow_error_recovery_sup (simple_one_for_one)
```

**Strategy**: `one_for_all` at top for consistent state, `simple_one_for_one` for dynamic workers

---

## API Usage

```erlang
%% Start a workflow
{ok, WorkflowId} = erlmcp_flow:start_workflow(
    <<"erlang-otp-developer, erlang-test-engineer parallel 2 timeout 300s">>
).

%% Get status
{ok, Status} = erlmcp_flow:get_workflow_status(WorkflowId).
%% Status = #{
%%     phase := specification | pseudocode | architecture | refinement | completion,
%%     progress := float(),  % 0.0 - 1.0
%%     tasks := #{task_id() => task_state()}
%% }

%% Cancel workflow
ok = erlmcp_flow:cancel_workflow(WorkflowId).

%% Get receipt (after completion)
{ok, Receipt} = erlmcp_flow:get_receipt(WorkflowId).
%% Receipt = #{
%%     workflow_id := binary(),
%%     phases := [phase_result()],
%%     tasks := [task_result()],
%%     metrics := #{latency_ms, throughput, success_rate},
%%     signature := binary()  % HMAC-SHA256
%% }
```

---

## Data Flow Example

**Input**: `"erlang-otp-developer, erlang-test-engineer parallel 2 timeout 300s"`

**Phase 1 - Specification**:
```erlang
SpecResult = #{
    agents => [
        #{role => 'erlang-otp-developer', count => 1},
        #{role => 'erlang-test-engineer', count => 1}
    ],
    parallelism => 2,
    timeout_ms => 300000,
    topology => mesh,  % Only 2 agents
    consensus => raft  % Internal agents
}
```

**Phase 2 - Pseudocode (Planning)**:
```erlang
PlanResult = #{
    assignments => [
        #{task_id => <<"task-1">>, agent_pid => <0.123.0>, role => 'erlang-otp-developer'},
        #{task_id => <<"task-2">>, agent_pid => <0.124.0>, role => 'erlang-test-engineer'}
    ],
    routing_table => #{
        <<"task-1">> => #{agent_pid => <0.123.0>, route => direct},
        <<"task-2">> => #{agent_pid => <0.124.0>, route => direct}
    },
    consensus => #{protocol => raft, quorum_size => 2, timeout_ms => 200}
}
```

**Phase 3 - Architecture**:
```erlang
DesignResult = #{
    topology => #{
        type => mesh,
        edges => [{<0.123.0>, <0.124.0>}]  % Full connectivity
    },
    consensus_state => #{
        protocol => raft,
        leader => <0.123.0>,
        followers => [<0.124.0>],
        current_term => 1
    }
}
```

**Phase 4 - Refinement (Execution)**:
```erlang
ExecutionResult = #{
    completed => [
        #{task_id => <<"task-1">>, result => {ok, code}, latency_ms => 1500},
        #{task_id => <<"task-2">>, result => {ok, tests}, latency_ms => 2300}
    ],
    failed => [],
    metrics => #{
        total_latency_ms => 2300,  % Parallel execution
        throughput => 0.87,  % tasks/s
        success_rate => 1.0
    }
}
```

**Phase 5 - Completion (Receipt)**:
```erlang
Receipt = #{
    workflow_id => <<"wf-uuid-1234">>,
    user_input => <<"erlang-otp-developer, erlang-test-engineer parallel 2 timeout 300s">>,
    phases => [
        #{phase => specification, latency_ms => 5},
        #{phase => pseudocode, latency_ms => 15},
        #{phase => architecture, latency_ms => 10},
        #{phase => refinement, latency_ms => 2300},
        #{phase => completion, latency_ms => 3}
    ],
    tasks => [...],
    total_metrics => #{
        workflow_latency_ms => 2333,
        task_count => 2,
        success_count => 2,
        failure_count => 0
    },
    signature => <<HMAC-SHA256>>
}
```

---

## Performance Targets

| Metric | Target | Threshold |
|--------|--------|-----------|
| Parse latency | <10ms | Reject >50ms |
| Plan latency | <50ms | Reject >200ms |
| Routing lookup | <100μs | Reject >1ms |
| Task dispatch | <5ms | Reject >20ms |
| Raft consensus | <200ms | Reject >500ms |
| PBFT consensus | <500ms | Reject >1000ms |
| Byzantine detection | <1s | Reject >5s |
| Partition heal | <2s RTO | Reject >10s |
| Workflow throughput | 50K tasks/s | Accept >40K tasks/s |
| Memory per workflow | <1KB | Reject >10KB |

---

## Testing Strategy

### Chicago TDD Approach

**Red → Green → Refactor**, test first, black-box only

### Test Coverage

- **EUnit**: Unit tests for each module (80%+ coverage)
- **Common Test**: Integration tests for workflows
- **PropEr**: Property tests for consensus protocols

### Test Suites

```erlang
%% Unit tests
erlmcp_flow_sparc_orchestrator_tests.erl
erlmcp_flow_spec_parser_tests.erl
erlmcp_flow_planner_tests.erl
erlmcp_flow_executor_tests.erl
erlmcp_flow_monitor_tests.erl
erlmcp_flow_receipt_tests.erl
erlmcp_flow_error_recovery_tests.erl
erlmcp_flow_raft_tests.erl
erlmcp_flow_pbft_tests.erl

%% Integration tests
erlmcp_flow_sparc_workflow_SUITE.erl
erlmcp_flow_consensus_SUITE.erl
erlmcp_flow_error_recovery_SUITE.erl
erlmcp_flow_partition_SUITE.erl

%% Property tests
erlmcp_flow_proper_tests.erl
```

---

## Quality Gates

| Gate | Pass Criteria | Enforcement |
|------|---------------|-------------|
| Compile | 0 errors | Pre-commit hook |
| EUnit | 0 failures | Pre-commit hook |
| Common Test | 0 failures | CI/CD |
| Coverage | ≥ 80% | CI/CD |
| Dialyzer | 0 warnings | CI/CD |
| Xref | 0 undefined | CI/CD |
| Format | rebar3 format compliant | Pre-commit hook |
| Benchmarks | <10% regression | Manual review |

---

## Implementation Timeline

| Phase | Duration | Deliverables |
|-------|----------|--------------|
| **Specification** | 1 day | Requirements, constraints, edge cases |
| **Architecture** | 1 day | State machines, supervision trees, data flow |
| **Implementation** | 3 days | All modules with EUnit tests (Chicago TDD) |
| **Integration** | 1 day | Common Test suites, end-to-end tests |
| **Quality** | 1 day | Dialyzer, xref, coverage ≥80%, benchmarks |
| **Documentation** | 1 day | API docs, examples, diagrams |
| **Total** | **8 days** | Production-ready SPARC workflow |

---

## Next Steps

1. Implement core modules (orchestrator, parser, planner, executor, monitor, receipt)
2. Implement consensus protocols (Raft, PBFT)
3. Implement error recovery state machine
4. Write comprehensive test suites (EUnit + CT + PropEr)
5. Run quality gates (make check)
6. Benchmark performance
7. Create API documentation
8. Generate final receipt

---

## Related Documents

- **Specification**: `/home/user/erlmcp/docs/ERLMCP_FLOW_SPARC_WORKFLOW_SPECIFICATION.md`
- **Architecture**: `/home/user/erlmcp/docs/ERLMCP_FLOW_SPARC_WORKFLOW_ARCHITECTURE.md`
- **Implementation Guide**: (To be created)
- **API Reference**: (To be created)
- **Examples**: (To be created)

---

## Key Architectural Principles

1. **Process-per-Connection**: Each workflow gets its own gen_statem orchestrator
2. **Let-it-crash**: Supervision trees isolate failures
3. **No Blocking in init/1**: All async operations via cast/info
4. **State Entry Actions**: Setup/teardown in state_enter_calls
5. **Priority Messages**: OTP 28 priority for health checks, cancel signals
6. **Idempotent Operations**: Cloud-determinism requirement
7. **Black-Box Testing**: Test observable behavior, not implementation
8. **Chicago TDD**: Test first, red → green → refactor

---

**End of Summary**
