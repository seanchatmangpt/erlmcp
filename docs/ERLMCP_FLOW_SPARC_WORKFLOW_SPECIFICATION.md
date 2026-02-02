# erlmcp-flow SPARC Workflow Specification v1.0.0

**Document Version**: 1.0.0
**Status**: Specification Phase
**Date**: 2026-02-02
**Repository**: /home/user/erlmcp

---

## Executive Summary

**erlmcp-flow SPARC Workflow** is an autonomous orchestration framework that implements the SPARC methodology (Specification → Pseudocode → Architecture → Refinement → Completion) using Erlang/OTP state machines, Byzantine-fault-tolerant consensus, and adaptive routing.

**Key Features**:
- 5-phase SPARC workflow orchestration with gen_statem state machines
- Task parser with validation and agent assignment
- Hierarchical vs mesh topology selection
- Raft consensus for distributed coordination
- Byzantine fault detection and recovery
- Network partition healing
- Receipt chain for audit trail
- Performance metrics and monitoring

---

## 1. SPECIFICATION PHASE

### 1.1 System Requirements

#### Functional Requirements

| ID | Requirement | Acceptance Criteria |
|----|-------------|---------------------|
| FR-1.1 | User Task Input | Parse task string → extract agents, parallelism, timeout |
| FR-1.2 | Spec Validation | Validate agent existence, capability match, resource constraints |
| FR-1.3 | Agent Assignment | Map tasks to available agents with load balancing |
| FR-1.4 | Consensus Selection | Choose Raft/PBFT based on Byzantine risk assessment |
| FR-1.5 | Topology Selection | Select mesh/hierarchical based on agent count & task complexity |
| FR-1.6 | Routing Rules | Pattern-based routing with O(log N) lookup |
| FR-1.7 | Adaptive Routing | Adjust routes based on latency, failure rate, throughput |
| FR-1.8 | Task Execution | Dispatch tasks with timeout, monitor progress, handle failure |
| FR-1.9 | Error Recovery | task_failed → replan, Byzantine → switch_consensus, partition → wait_heal |
| FR-1.10 | Receipt Generation | Audit log with task_id, agent_id, timestamp, result, metrics |

#### Non-Functional Requirements

| ID | Requirement | Target | Threshold |
|----|------------|--------|-----------|
| NFR-1.1 | Parse Latency | <10ms | Reject >50ms |
| NFR-1.2 | Routing Latency | <100μs | Reject >1ms |
| NFR-1.3 | Task Throughput | 50K tasks/s | Accept >40K tasks/s |
| NFR-1.4 | Consensus Latency | <200ms (Raft) | Reject >500ms |
| NFR-1.5 | Byzantine Detection | <1s | Reject >5s |
| NFR-1.6 | Partition Healing | <2s RTO | Reject >10s |
| NFR-1.7 | Memory per Task | <1KB | Reject >10KB |
| NFR-1.8 | Code Coverage | ≥80% | Reject <75% |

#### Constraints

| Constraint | Rationale |
|-----------|-----------|
| OTP 28.3.1 STRICT | JIT compiler, priority messages, hibernation |
| gen_statem only | State machine semantics for SPARC phases |
| Chicago TDD | All code must have tests (test ⊢ impl) |
| No mocks/fakes | Black-box testing only |
| Supervised spawn | ∀proc. supervised(proc) = true |
| Idempotent ops | Cloud-determinism requirement |
| Merge-only Git | No rebasing, preserve history |

#### Edge Cases & Error Scenarios

| Edge Case | Handling Strategy |
|-----------|-------------------|
| Task parse failure | Return {error, invalid_spec} with detailed reason |
| Agent unavailable | Fallback to alternative agent with similar capability |
| Consensus split-brain | Leader election with randomized timeout (150-300ms) |
| Byzantine agent | Detect via message signature verification, exclude from quorum |
| Network partition | Quorum loss → halt writes, wait for partition heal |
| Task timeout | Cancel execution, trigger replan with increased timeout |
| Cascading failure | Supervision isolation, circuit breaker per agent |
| Memory leak | LRU eviction for task history, periodic GC |

---

## 2. SPARC WORKFLOW STATE MACHINES

### 2.1 Main Orchestrator FSM

**Module**: `erlmcp_flow_sparc_orchestrator.erl`
**Behavior**: `gen_statem`
**Callback Mode**: `[handle_event_function, state_enter_calls]`

#### States

```erlang
-type orchestrator_state() ::
    idle |              % Waiting for task input
    specification |     % Phase 1: Parse & validate spec
    pseudocode |        % Phase 2: Plan agent assignments
    architecture |      % Phase 3: Design topology & routing
    refinement |        % Phase 4: Execute tasks with monitoring
    completion |        % Phase 5: Generate receipt & audit
    error.              % Error state with recovery options
```

#### State Transitions

```
idle --[user_input]--> specification
specification --[spec_valid]--> pseudocode
specification --[spec_invalid]--> error
pseudocode --[plan_complete]--> architecture
pseudocode --[plan_failed]--> error
architecture --[design_complete]--> refinement
architecture --[design_failed]--> error
refinement --[execution_complete]--> completion
refinement --[execution_failed]--> error
completion --[receipt_generated]--> idle
error --[replan]--> specification
error --[cancel]--> idle
```

#### State Data

```erlang
-record(orchestrator_data, {
    workflow_id :: binary(),              % UUID
    user_input :: binary(),               % Original task string
    spec :: spec_result() | undefined,    % Phase 1 output
    plan :: plan_result() | undefined,    % Phase 2 output
    design :: design_result() | undefined,% Phase 3 output
    execution :: execution_result() | undefined,  % Phase 4 output
    receipt :: receipt_result() | undefined,      % Phase 5 output
    error_reason :: term() | undefined,
    retry_count = 0 :: non_neg_integer(),
    max_retries = 3 :: pos_integer(),
    created_at :: integer(),
    timeout_ms = 300000 :: pos_integer(), % 5 minutes default
    metrics :: metrics_state()
}).
```

#### Events

```erlang
%% API calls
{call, From, {start_workflow, UserInput}}
{call, From, {cancel_workflow}}
{call, From, {get_status}}
{call, From, {get_receipt}}

%% Internal events
{cast, {phase_complete, Phase, Result}}
{cast, {phase_failed, Phase, Reason}}
{cast, {error_recovered, NewPhase}}

%% Timeout events
{info, phase_timeout}
{info, workflow_timeout}
```

---

### 2.2 Spec Parser FSM

**Module**: `erlmcp_flow_spec_parser.erl`
**Behavior**: `gen_statem`
**Callback Mode**: `[handle_event_function, state_enter_calls]`

#### States

```erlang
-type parser_state() ::
    idle |          % Waiting for input
    parsing |       % Tokenizing and parsing task string
    validating |    % Validating agents, capabilities, constraints
    complete |      % Parse complete with spec_result
    error.          % Parse failed with error details
```

#### State Data

```erlang
-record(parser_data, {
    parser_id :: binary(),
    input :: binary(),
    tokens :: [token()] | undefined,
    agents :: [agent_spec()] | undefined,
    parallelism :: pos_integer() | undefined,
    timeout_ms :: pos_integer() | undefined,
    capabilities :: [capability()] | undefined,
    validation_errors :: [error_detail()] | undefined,
    result :: spec_result() | undefined
}).

-type spec_result() :: #{
    agents := [#{
        role := atom(),
        count := pos_integer(),
        capabilities := [capability()]
    }],
    parallelism := pos_integer(),
    timeout_ms := pos_integer(),
    topology := mesh | hierarchical,
    consensus := raft | pbft
}.
```

#### Parsing Rules

```erlang
%% Task grammar (EBNF-like)
Task ::= AgentList ["parallel" Number] ["timeout" Duration]
AgentList ::= Agent ("," Agent)*
Agent ::= AgentRole ["(" Capability ("," Capability)* ")"]
AgentRole ::= atom()
Capability ::= atom()
Duration ::= Number "ms" | Number "s" | Number "m"

%% Examples:
%% "erlang-otp-developer, erlang-test-engineer parallel 2 timeout 300s"
%% "sparc-orchestrator, plan-designer(research, design), erlang-architect"
%% "agent-06-test-eunit, agent-07-test-ct parallel 10 timeout 600s"
```

---

### 2.3 Planner FSM

**Module**: `erlmcp_flow_planner.erl`
**Behavior**: `gen_statem`
**Callback Mode**: `[handle_event_function, state_enter_calls]`

#### States

```erlang
-type planner_state() ::
    idle |          % Waiting for spec
    analyzing |     % Analyze task dependencies and agent availability
    assigning |     % Assign tasks to specific agent instances
    routing |       % Create routing rules and paths
    complete |      % Plan complete with plan_result
    error.          % Planning failed
```

#### State Data

```erlang
-record(planner_data, {
    planner_id :: binary(),
    spec :: spec_result(),
    agent_pool :: #{agent_role() => [pid()]},  % Available agents
    assignments :: [task_assignment()] | undefined,
    dependencies :: digraph:graph() | undefined,  % Task dependency DAG
    routing_table :: #{task_id() => route()} | undefined,
    consensus_state :: consensus_config() | undefined,
    result :: plan_result() | undefined
}).

-type plan_result() :: #{
    assignments := [#{
        task_id := binary(),
        agent_pid := pid(),
        dependencies := [task_id()],
        priority := high | normal | low
    }],
    routing_table := #{task_id() => route()},
    consensus := #{
        protocol := raft | pbft,
        quorum_size := pos_integer(),
        timeout_ms := pos_integer()
    },
    topology := #{
        type := mesh | hierarchical,
        edges := [{pid(), pid()}]
    }
}.
```

#### Consensus Selection

```erlang
%% Raft: Use when Byzantine risk is low
%% - Internal agents only
%% - Trusted environment
%% - Lower latency required (150-300ms)

%% PBFT: Use when Byzantine risk is high
%% - External agents
%% - Untrusted environment
%% - Higher fault tolerance required
%% - Can tolerate f Byzantine faults with 3f+1 nodes
```

#### Topology Selection

```erlang
%% Mesh: Use when agent count is low (<10)
%% - Full connectivity
%% - O(N²) edges
%% - Low latency, high redundancy

%% Hierarchical: Use when agent count is high (≥10)
%% - Tree structure with coordinator nodes
%% - O(N) edges
%% - Scalability over redundancy
```

---

### 2.4 Executor FSM

**Module**: `erlmcp_flow_executor.erl`
**Behavior**: `gen_statem`
**Callback Mode**: `[handle_event_function, state_enter_calls]`

#### States

```erlang
-type executor_state() ::
    idle |          % Waiting for plan
    dispatching |   % Sending tasks to agents
    executing |     % Tasks running, monitoring progress
    monitoring |    % Adaptive monitoring with performance feedback
    complete |      % Execution complete with results
    error.          % Execution failed
```

#### State Data

```erlang
-record(executor_data, {
    executor_id :: binary(),
    plan :: plan_result(),
    task_states :: #{task_id() => task_state()},
    monitors :: #{task_id() => reference()},
    timeouts :: #{task_id() => reference()},
    performance_metrics :: #{agent_pid() => metrics()},
    routing_adjustments :: [routing_adjustment()],
    result :: execution_result() | undefined
}).

-type task_state() ::
    pending |       % Not yet dispatched
    dispatched |    % Sent to agent
    running |       % Agent confirmed receipt, executing
    complete |      % Success
    failed |        % Failed with reason
    timeout.        % Timeout exceeded

-type execution_result() :: #{
    completed := [#{
        task_id := binary(),
        result := term(),
        latency_ms := non_neg_integer()
    }],
    failed := [#{
        task_id := binary(),
        reason := term()
    }],
    metrics := #{
        total_latency_ms := non_neg_integer(),
        throughput := float(),  % tasks/s
        success_rate := float()
    }
}.
```

#### Adaptive Routing

```erlang
%% Performance feedback loop
%% 1. Collect metrics: latency, failure_rate, throughput
%% 2. Calculate agent health scores
%% 3. Adjust routing weights based on health
%% 4. Re-route tasks from unhealthy agents
%% 5. Update routing table for future tasks

-record(agent_health, {
    agent_pid :: pid(),
    latency_p99_ms :: float(),
    failure_rate :: float(),      % 0.0 - 1.0
    throughput :: float(),         % tasks/s
    health_score :: float(),       % 0.0 - 1.0
    last_updated :: integer()
}).

%% Health score formula:
%% health = (0.4 * latency_factor) + (0.4 * success_factor) + (0.2 * throughput_factor)
%% latency_factor = max(0, 1 - (latency_p99 / threshold))
%% success_factor = 1 - failure_rate
%% throughput_factor = min(1, throughput / target_throughput)
```

---

### 2.5 Monitor FSM

**Module**: `erlmcp_flow_monitor.erl`
**Behavior**: `gen_statem`
**Callback Mode**: `[handle_event_function, state_enter_calls]`

#### States

```erlang
-type monitor_state() ::
    observing |     % Collecting metrics and events
    alerting |      % Threshold breach detected
    recovering |    % Triggering recovery actions
    complete.       % Monitoring session complete
```

#### State Data

```erlang
-record(monitor_data, {
    monitor_id :: binary(),
    workflow_id :: binary(),
    subscriptions :: #{event_type() => [pid()]},
    metrics_buffer :: queue:queue(),  % Ring buffer for metrics
    alert_rules :: [alert_rule()],
    active_alerts :: [active_alert()],
    recovery_actions :: [recovery_action()]
}).

-type alert_rule() :: #{
    rule_id := binary(),
    condition := fun((metrics()) -> boolean()),
    severity := critical | warning | info,
    action := fun(() -> ok)
}.

-type recovery_action() ::
    {replan, task_id()} |
    {switch_consensus, raft | pbft} |
    {wait_partition_heal, timeout_ms()} |
    {circuit_break, agent_pid()} |
    {scale_up, agent_role()}.
```

---

### 2.6 Receipt Generator (gen_server)

**Module**: `erlmcp_flow_receipt.erl`
**Behavior**: `gen_server`

#### State Data

```erlang
-record(receipt_state, {
    receipts :: #{workflow_id() => receipt()},
    audit_log :: [audit_entry()],
    retention_days = 30 :: pos_integer()
}).

-type receipt() :: #{
    workflow_id := binary(),
    user_input := binary(),
    phases := [#{
        phase := atom(),
        started_at := integer(),
        completed_at := integer(),
        result := term()
    }],
    tasks := [#{
        task_id := binary(),
        agent_role := atom(),
        agent_pid := pid(),
        status := complete | failed | timeout,
        result := term() | undefined,
        error := term() | undefined,
        metrics := #{
            latency_ms := non_neg_integer(),
            cpu_time_us := non_neg_integer(),
            memory_bytes := non_neg_integer()
        }
    }],
    total_metrics := #{
        workflow_latency_ms := non_neg_integer(),
        task_count := pos_integer(),
        success_count := non_neg_integer(),
        failure_count := non_neg_integer(),
        throughput := float()
    },
    consensus_metrics := #{
        protocol := raft | pbft,
        rounds := pos_integer(),
        consensus_latency_ms := non_neg_integer()
    },
    created_at := integer(),
    signature := binary()  % HMAC-SHA256
}.
```

---

### 2.7 Error Recovery FSM

**Module**: `erlmcp_flow_error_recovery.erl`
**Behavior**: `gen_statem`
**Callback Mode**: `[handle_event_function, state_enter_calls]`

#### States

```erlang
-type recovery_state() ::
    monitoring |            % Normal operation, watching for errors
    task_failed |           % Individual task failure detected
    byzantine_detected |    % Byzantine behavior detected
    partition_detected |    % Network partition detected
    recovering |            % Executing recovery action
    recovered |             % Recovery successful
    failed.                 % Recovery failed, escalate
```

#### State Transitions

```
monitoring --[task_failure]--> task_failed --[replan]--> recovering
monitoring --[byzantine_detected]--> byzantine_detected --[switch_consensus]--> recovering
monitoring --[partition_detected]--> partition_detected --[wait_heal]--> recovering
recovering --[recovery_success]--> recovered --[resume]--> monitoring
recovering --[recovery_failure]--> failed
```

#### State Data

```erlang
-record(recovery_data, {
    recovery_id :: binary(),
    error_type :: task_failed | byzantine | partition,
    error_context :: map(),
    recovery_strategy :: atom(),
    retry_count = 0 :: non_neg_integer(),
    max_retries = 3 :: pos_integer(),
    backoff_ms = 1000 :: pos_integer(),
    recovery_actions :: [recovery_action_result()]
}).
```

#### Recovery Strategies

```erlang
%% Task Failure → Replan
%% 1. Mark task as failed
%% 2. Remove failed agent from pool temporarily
%% 3. Re-run planner with updated agent pool
%% 4. Re-dispatch task to alternative agent
%% 5. If no alternatives, escalate to user

%% Byzantine Detection → Switch Consensus
%% 1. Verify Byzantine behavior (message signature mismatch)
%% 2. Exclude Byzantine agent from quorum
%% 3. If Raft, switch to PBFT for higher tolerance
%% 4. Reconfigure consensus with remaining honest nodes
%% 5. Resume execution with new consensus

%% Network Partition → Wait for Heal
%% 1. Detect partition via heartbeat timeout
%% 2. Halt writes to prevent split-brain
%% 3. Continue reads from majority partition
%% 4. Poll for partition heal (exponential backoff)
%% 5. Reconcile state after heal using vector clocks
%% 6. Resume normal operation
```

---

## 3. SUPERVISION TREE

```
erlmcp_flow_sup (one_for_all)
├── erlmcp_flow_registry (gen_server)
├── erlmcp_flow_sparc_sup (one_for_one)
│   ├── erlmcp_flow_sparc_orchestrator_sup (simple_one_for_one)
│   │   └── erlmcp_flow_sparc_orchestrator (gen_statem) [dynamic]
│   ├── erlmcp_flow_spec_parser_sup (simple_one_for_one)
│   │   └── erlmcp_flow_spec_parser (gen_statem) [dynamic]
│   ├── erlmcp_flow_planner_sup (simple_one_for_one)
│   │   └── erlmcp_flow_planner (gen_statem) [dynamic]
│   ├── erlmcp_flow_executor_sup (simple_one_for_one)
│   │   └── erlmcp_flow_executor (gen_statem) [dynamic]
│   └── erlmcp_flow_monitor_sup (simple_one_for_one)
│       └── erlmcp_flow_monitor (gen_statem) [dynamic]
├── erlmcp_flow_receipt_server (gen_server)
└── erlmcp_flow_error_recovery_sup (simple_one_for_one)
    └── erlmcp_flow_error_recovery (gen_statem) [dynamic]
```

**Supervision Strategy**: `one_for_all` for top-level to ensure consistent state after crashes

**Child Restart**: `transient` - restart only if abnormal termination

**Shutdown Timeout**: `5000ms` per child

---

## 4. API SPECIFICATION

```erlang
%% Start a SPARC workflow
-spec start_workflow(UserInput :: binary()) ->
    {ok, WorkflowId :: binary()} | {error, Reason :: term()}.

%% Start a SPARC workflow with options
-spec start_workflow(UserInput :: binary(), Options :: map()) ->
    {ok, WorkflowId :: binary()} | {error, Reason :: term()}.

%% Get workflow status
-spec get_workflow_status(WorkflowId :: binary()) ->
    {ok, Status :: map()} | {error, not_found}.

%% Cancel a running workflow
-spec cancel_workflow(WorkflowId :: binary()) ->
    ok | {error, Reason :: term()}.

%% Get workflow receipt
-spec get_receipt(WorkflowId :: binary()) ->
    {ok, Receipt :: receipt()} | {error, not_found}.

%% List all workflows
-spec list_workflows() ->
    {ok, [WorkflowId :: binary()]}.

%% Cleanup completed workflows older than N days
-spec cleanup_old_workflows(Days :: pos_integer()) ->
    {ok, Deleted :: non_neg_integer()}.
```

---

## 5. PERFORMANCE TARGETS

| Metric | Target | Measurement |
|--------|--------|-------------|
| Parse latency | <10ms | p99 |
| Plan latency | <50ms | p99 |
| Routing lookup | <100μs | p99 |
| Task dispatch | <5ms | p99 |
| Consensus round (Raft) | <200ms | p99 |
| Consensus round (PBFT) | <500ms | p99 |
| Byzantine detection | <1s | p99 |
| Partition heal | <2s RTO | p99 |
| Workflow throughput | 50K tasks/s | sustained |
| Memory per workflow | <1KB | average |
| Receipt generation | <20ms | p99 |

---

## 6. TESTING STRATEGY (Chicago TDD)

### 6.1 Unit Tests (EUnit)

```erlang
%% Parser tests
parser_valid_task_test() -> ...
parser_invalid_syntax_test() -> ...
parser_missing_agent_test() -> ...

%% Planner tests
planner_simple_assignment_test() -> ...
planner_dependency_dag_test() -> ...
planner_topology_selection_test() -> ...

%% Executor tests
executor_dispatch_test() -> ...
executor_timeout_test() -> ...
executor_adaptive_routing_test() -> ...

%% Monitor tests
monitor_alert_trigger_test() -> ...
monitor_recovery_action_test() -> ...

%% Receipt tests
receipt_generation_test() -> ...
receipt_signature_test() -> ...
receipt_retrieval_test() -> ...

%% Error recovery tests
recovery_task_failed_test() -> ...
recovery_byzantine_test() -> ...
recovery_partition_test() -> ...
```

### 6.2 Integration Tests (Common Test)

```erlang
%% End-to-end SPARC workflow
sparc_workflow_simple_SUITE.erl
sparc_workflow_parallel_SUITE.erl
sparc_workflow_hierarchical_SUITE.erl

%% Consensus protocols
consensus_raft_SUITE.erl
consensus_pbft_SUITE.erl
consensus_partition_SUITE.erl

%% Error scenarios
error_recovery_SUITE.erl
byzantine_fault_SUITE.erl
network_partition_SUITE.erl
```

### 6.3 Property Tests (PropEr)

```erlang
%% Parser properties
prop_parser_roundtrip() -> ...  % parse(format(spec)) = spec
prop_parser_never_crashes() -> ...

%% Planner properties
prop_planner_valid_dag() -> ...  % No cycles in dependency graph
prop_planner_all_assigned() -> ...  % Every task has an agent

%% Consensus properties
prop_raft_eventually_elects_leader() -> ...
prop_pbft_tolerates_f_byzantine() -> ...
```

---

## 7. QUALITY GATES

| Gate | Pass Criteria | Enforcement |
|------|---------------|-------------|
| Compile | 0 errors | Pre-commit hook |
| Tests | 0 failures | Pre-commit hook |
| Coverage | ≥ 80% | CI/CD |
| Dialyzer | 0 warnings | CI/CD |
| Xref | 0 undefined | CI/CD |
| Format | rebar3 format compliant | Pre-commit hook |
| Benchmarks | <10% regression | Manual review |

---

## 8. DOCUMENTATION

- `ERLMCP_FLOW_SPARC_WORKFLOW_SPECIFICATION.md` (this document)
- `ERLMCP_FLOW_SPARC_WORKFLOW_ARCHITECTURE.md` (detailed design)
- `ERLMCP_FLOW_SPARC_WORKFLOW_IMPLEMENTATION_GUIDE.md` (TDD guide)
- `ERLMCP_FLOW_SPARC_WORKFLOW_API_REFERENCE.md` (API docs)
- `ERLMCP_FLOW_SPARC_WORKFLOW_EXAMPLES.md` (usage examples)

---

## 9. TIMELINE

| Phase | Duration | Deliverables |
|-------|----------|--------------|
| Specification | 1 day | This document |
| Architecture | 1 day | Architecture document, supervision trees |
| Implementation | 3 days | All modules with tests (Chicago TDD) |
| Integration | 1 day | Common Test suites, end-to-end tests |
| Quality | 1 day | Dialyzer, xref, coverage, benchmarks |
| Documentation | 1 day | API reference, examples |
| **Total** | **8 days** | Production-ready erlmcp-flow SPARC workflow |

---

**End of Specification**
