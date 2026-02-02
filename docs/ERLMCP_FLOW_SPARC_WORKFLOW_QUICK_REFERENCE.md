# erlmcp-flow SPARC Workflow Quick Reference

**Version**: 1.0.0
**Date**: 2026-02-02

---

## Quick Navigation

| Section | Description |
|---------|-------------|
| [State Machines](#state-machines) | All FSM states and transitions |
| [API Cheat Sheet](#api-cheat-sheet) | Public API reference |
| [Module Index](#module-index) | Module responsibilities |
| [Error Recovery](#error-recovery) | Recovery strategies |
| [Performance Targets](#performance-targets) | Metrics and thresholds |
| [Testing](#testing) | Test structure and commands |

---

## State Machines

### Orchestrator States

```erlang
idle → specification → pseudocode → architecture → refinement → completion → idle
                                                                        ↓
                                                                      error
```

**Transitions**:
- `start_workflow` → specification
- `spec_valid` → pseudocode
- `plan_complete` → architecture
- `design_complete` → refinement
- `execution_complete` → completion
- `receipt_generated` → idle
- `spec_invalid | plan_failed | execution_failed` → error
- `replan` → specification

### Parser States

```erlang
idle → parsing → validating → complete
                    ↓
                  error
```

### Planner States

```erlang
idle → analyzing → assigning → routing → complete
                                  ↓
                                error
```

### Executor States

```erlang
idle → dispatching → executing → monitoring → complete
                                      ↓
                                    error
```

### Monitor States

```erlang
observing → alerting → recovering → complete
              ↓            ↓
              └────────────┘ (loop on alert persist)
```

### Error Recovery States

```erlang
monitoring → {task_failed | byzantine_detected | partition_detected}
                ↓              ↓                      ↓
             replan    switch_consensus         wait_heal
                └──────────┬───────────────────────┘
                           ↓
                      recovering
                           ↓
                    ┌──────┴──────┐
                    ↓             ↓
                recovered       failed
```

---

## API Cheat Sheet

```erlang
%% Start workflow
{ok, WorkflowId} = erlmcp_flow:start_workflow(
    <<"erlang-otp-developer, erlang-test-engineer parallel 2 timeout 300s">>
).

%% Start with options
{ok, WorkflowId} = erlmcp_flow:start_workflow(Input, #{
    topology => mesh,
    consensus => raft,
    timeout_ms => 600000
}).

%% Get status
{ok, #{phase := Phase, progress := Progress}} =
    erlmcp_flow:get_workflow_status(WorkflowId).

%% Cancel workflow
ok = erlmcp_flow:cancel_workflow(WorkflowId).

%% Get receipt
{ok, Receipt} = erlmcp_flow:get_receipt(WorkflowId).

%% List all workflows
{ok, WorkflowIds} = erlmcp_flow:list_workflows().

%% Cleanup old workflows
{ok, DeletedCount} = erlmcp_flow:cleanup_old_workflows(30).  % 30 days
```

---

## Module Index

| Module | Type | Responsibility |
|--------|------|----------------|
| `erlmcp_flow` | API | Public API facade |
| `erlmcp_flow_app` | application | OTP application callback |
| `erlmcp_flow_sup` | supervisor | Top-level supervision |
| `erlmcp_flow_sparc_orchestrator` | gen_statem | Main workflow coordinator |
| `erlmcp_flow_spec_parser` | gen_statem | Parse and validate input |
| `erlmcp_flow_planner` | gen_statem | Task planning and assignment |
| `erlmcp_flow_executor` | gen_statem | Task execution and monitoring |
| `erlmcp_flow_monitor` | gen_statem | Metrics and alerting |
| `erlmcp_flow_receipt` | gen_server | Receipt generation and storage |
| `erlmcp_flow_error_recovery` | gen_statem | Error recovery orchestration |
| `erlmcp_flow_registry` | gen_server | Agent registry (gproc wrapper) |
| `erlmcp_flow_router` | pure | Routing algorithms |
| `erlmcp_flow_topology` | pure | Topology selection logic |
| `erlmcp_flow_consensus` | behaviour | Consensus protocol interface |
| `erlmcp_flow_raft` | gen_statem | Raft consensus implementation |
| `erlmcp_flow_pbft` | gen_statem | PBFT consensus implementation |

**Supervisors**:
- `erlmcp_flow_sparc_sup`
- `erlmcp_flow_sparc_orchestrator_sup`
- `erlmcp_flow_spec_parser_sup`
- `erlmcp_flow_planner_sup`
- `erlmcp_flow_executor_sup`
- `erlmcp_flow_monitor_sup`
- `erlmcp_flow_error_recovery_sup`

---

## Error Recovery

### Task Failure → Replan

```erlang
%% 1. Mark task failed
%% 2. Suspend failed agent (60s)
%% 3. Re-run planner with exclusion
%% 4. Re-dispatch to alternative agent
%% 5. Continue monitoring

erlmcp_flow_registry:suspend_agent(AgentPid, 60000).
{ok, NewPlan} = erlmcp_flow_planner:replan(Plan, [{exclude_agent, AgentPid}]).
```

### Byzantine Fault → Switch Consensus

```erlang
%% 1. Verify Byzantine behavior
%% 2. Exclude from quorum
%% 3. Switch Raft → PBFT
%% 4. Reconfigure consensus
%% 5. Resume execution

case verify_byzantine(AgentPid, Evidence) of
    true ->
        NewPeers = lists:delete(AgentPid, Peers),
        {ok, PBFT} = erlmcp_flow_pbft:start_link(#{replicas => NewPeers}),
        ok = erlmcp_flow_pbft:import_state(PBFT, OldState)
end.
```

### Network Partition → Wait for Heal

```erlang
%% 1. Halt writes (prevent split-brain)
%% 2. Continue reads from majority
%% 3. Poll for heal (exponential backoff)
%% 4. Reconcile with vector clocks
%% 5. Resume writes

erlmcp_flow_consensus:halt_writes(ConsensusPid).
Ref = start_partition_heal_poller(#{
    initial_delay_ms => 1000,
    max_delay_ms => 60000,
    backoff_factor => 2.0
}).
```

---

## Performance Targets

| Metric | Target | Threshold | Measurement |
|--------|--------|-----------|-------------|
| Parse latency | <10ms | 50ms | p99 |
| Plan latency | <50ms | 200ms | p99 |
| Routing lookup | <100μs | 1ms | p99 |
| Task dispatch | <5ms | 20ms | p99 |
| Raft consensus | <200ms | 500ms | p99 |
| PBFT consensus | <500ms | 1000ms | p99 |
| Byzantine detection | <1s | 5s | p99 |
| Partition heal (RTO) | <2s | 10s | p99 |
| Workflow throughput | 50K tasks/s | 40K tasks/s | sustained |
| Memory per workflow | <1KB | 10KB | average |
| Receipt generation | <20ms | 100ms | p99 |

---

## Testing

### Unit Tests (EUnit)

```bash
# Run all EUnit tests
rebar3 eunit --application erlmcp_flow

# Run specific module tests
rebar3 eunit --module erlmcp_flow_sparc_orchestrator_tests
rebar3 eunit --module erlmcp_flow_spec_parser_tests
rebar3 eunit --module erlmcp_flow_planner_tests
rebar3 eunit --module erlmcp_flow_executor_tests
rebar3 eunit --module erlmcp_flow_monitor_tests
rebar3 eunit --module erlmcp_flow_receipt_tests
rebar3 eunit --module erlmcp_flow_error_recovery_tests
rebar3 eunit --module erlmcp_flow_raft_tests
rebar3 eunit --module erlmcp_flow_pbft_tests
```

### Integration Tests (Common Test)

```bash
# Run all CT suites
rebar3 ct --suite apps/erlmcp_flow/test/*_SUITE

# Run specific suite
rebar3 ct --suite apps/erlmcp_flow/test/erlmcp_flow_sparc_workflow_SUITE
rebar3 ct --suite apps/erlmcp_flow/test/erlmcp_flow_consensus_SUITE
rebar3 ct --suite apps/erlmcp_flow/test/erlmcp_flow_error_recovery_SUITE
```

### Property Tests (PropEr)

```bash
# Run property tests
rebar3 proper --module erlmcp_flow_proper_tests
```

### Quality Gates

```bash
# Full quality check (compile + xref + dialyzer + tests)
make check

# Quick verification (compile + eunit)
make verify-fast

# Coverage report
rebar3 cover --verbose

# Dialyzer
rebar3 dialyzer

# Xref
rebar3 xref

# Format check
rebar3 format --verify
```

---

## gen_statem Template

```erlang
-module(erlmcp_flow_MODULE).
-behaviour(gen_statem).

%% API
-export([start_link/1, stop/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).

%% State names
-export([state1/3, state2/3]).

-record(data, {
    id :: binary(),
    % ... state data fields
}).

%%====================================================================
%% API
%%====================================================================

start_link(Args) ->
    gen_statem:start_link(?MODULE, Args, []).

stop(Pid) ->
    gen_statem:stop(Pid).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

init(Args) ->
    %% NO BLOCKING OPERATIONS HERE (Armstrong principle)
    Data = #data{id = maps:get(id, Args)},
    {ok, initial_state, Data}.

callback_mode() ->
    [handle_event_function, state_enter_calls].

%% State enter calls
handle_event(enter, OldState, State, Data) ->
    logger:debug("~p: ~p -> ~p", [?MODULE, OldState, State]),
    %% State entry actions
    {keep_state_and_data, []};

%% Priority messages (OTP 28)
handle_event({call, From}, {health_check}, _State, Data) ->
    Health = #{state => _State, id => Data#data.id},
    {keep_state_and_data, [{reply, From, {ok, Health}}]};

%% Normal events
handle_event(EventType, EventContent, State, Data) ->
    %% Handle events based on State
    {keep_state_and_data, []}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
```

---

## Common Patterns

### Async Init

```erlang
init(Args) ->
    %% NO blocking - schedule async
    self() ! init_async,
    {ok, initializing, #data{args = Args}};

handle_event(info, init_async, initializing, Data) ->
    %% Do expensive init here
    {ok, Resource} = expensive_operation(),
    {next_state, ready, Data#data{resource = Resource}}.
```

### State Timeout

```erlang
handle_event(enter, _OldState, waiting, Data) ->
    {keep_state_and_data, [{state_timeout, 5000, timeout_event}]};

handle_event(state_timeout, timeout_event, waiting, Data) ->
    %% Handle timeout
    {next_state, error, Data#data{reason = timeout}}.
```

### Monitoring

```erlang
handle_event(enter, _OldState, executing, Data) ->
    MonRef = erlang:monitor(process, Data#data.agent_pid),
    {keep_state, Data#data{monitor_ref = MonRef}};

handle_event(info, {'DOWN', MonRef, process, Pid, Reason}, executing, Data) ->
    %% Handle process down
    {next_state, error, Data#data{error = {agent_crashed, Reason}}}.
```

### Registry Lookup (gproc)

```erlang
%% Register
gproc:reg({p, l, {agent_role, 'erlang-otp-developer'}}).

%% Lookup
case gproc:lookup_pids({p, l, {agent_role, Role}}) of
    [] -> {error, not_found};
    [Pid|_] -> {ok, Pid}
end.
```

---

## Data Structures

### Spec Result

```erlang
-type spec_result() :: #{
    agents := [#{
        role := atom(),
        count := pos_integer()
    }],
    parallelism := pos_integer(),
    timeout_ms := pos_integer(),
    topology := mesh | hierarchical,
    consensus := raft | pbft
}.
```

### Plan Result

```erlang
-type plan_result() :: #{
    assignments := [#{
        task_id := binary(),
        agent_pid := pid(),
        dependencies := [task_id()]
    }],
    routing_table := #{task_id() => route()},
    consensus := #{
        protocol := raft | pbft,
        quorum_size := pos_integer()
    },
    topology := #{
        type := mesh | hierarchical,
        edges := [{pid(), pid()}]
    }
}.
```

### Execution Result

```erlang
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
        throughput := float(),
        success_rate := float()
    }
}.
```

### Receipt

```erlang
-type receipt() :: #{
    workflow_id := binary(),
    user_input := binary(),
    phases := [phase_result()],
    tasks := [task_result()],
    total_metrics := workflow_metrics(),
    consensus_metrics := consensus_metrics(),
    created_at := integer(),
    signature := binary()  % HMAC-SHA256
}.
```

---

## Consensus Selection Logic

```erlang
select_consensus_protocol(Agents, Environment) ->
    RiskScore = assess_risk(Agents, Environment),
    case RiskScore of
        Score when Score < 0.3 ->
            %% Low risk: use Raft
            {raft, #{
                election_timeout_ms => random(150, 300),
                heartbeat_interval_ms => 50
            }};
        Score when Score >= 0.3 ->
            %% High risk: use PBFT
            F = calculate_f(length(Agents)),  % Byzantine faults tolerated
            {pbft, #{
                f => F,
                replicas => 3 * F + 1,
                view_change_timeout_ms => 1000
            }}
    end.

assess_risk(Agents, Environment) ->
    ExternalAgentRatio = count_external(Agents) / length(Agents),
    UntrustedEnv = case Environment of
        untrusted -> 1.0;
        mixed -> 0.5;
        trusted -> 0.0
    end,
    0.6 * ExternalAgentRatio + 0.4 * UntrustedEnv.
```

---

## Topology Selection Logic

```erlang
select_topology(AgentCount, TaskComplexity) ->
    case {AgentCount, TaskComplexity} of
        {N, _} when N =< 10 ->
            %% Small team: use mesh
            {mesh, build_mesh_topology(N)};
        {N, low} when N > 10 ->
            %% Large team, simple tasks: use hierarchical
            {hierarchical, build_hierarchical_topology(N, 2)};  % 2 levels
        {N, _} when N > 10 ->
            %% Large team, complex tasks: use hierarchical with 3 levels
            {hierarchical, build_hierarchical_topology(N, 3)}
    end.
```

---

## Helpful Commands

```bash
# Start Erlang shell with erlmcp_flow
make console

# Run observer GUI (visualize processes)
make observer

# Quick compile + test
make verify-fast

# Full quality check
make check

# Generate coverage report
rebar3 cover --verbose
rebar3 covertool generate

# Run benchmarks
rebar3 bench

# Format code
rebar3 format

# Clean build artifacts
rebar3 clean
```

---

**End of Quick Reference**
