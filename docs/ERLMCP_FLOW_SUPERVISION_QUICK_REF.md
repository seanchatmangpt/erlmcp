# erlmcp-flow Supervision Tree - Quick Reference

**Visual Guide**: OTP Supervision Hierarchy & Restart Strategies
**Version**: 1.0.0
**Date**: 2026-02-02

---

## Complete Supervision Tree

```
┌─────────────────────────────────────────────────────────────────────────┐
│                       erlmcp_flow APPLICATION                           │
│                                                                         │
│  erlmcp_flow_sup (one_for_all) ◄────────────────── TIER 1              │
│  ├── erlmcp_flow_registry (gen_server)          [permanent, 5000ms]    │
│  │   └── gproc-based routing: O(log N) agent lookup                    │
│  │                                                                      │
│  ├── erlmcp_flow_consensus_sup (one_for_one)    [permanent, infinity]  │
│  │   ├── erlmcp_flow_raft (gen_server)          [permanent, 5000ms]    │
│  │   │   └── Leader election: 150-300ms timeout                        │
│  │   ├── erlmcp_flow_byzantine (gen_server)     [permanent, 5000ms]    │
│  │   │   └── PBFT: 2f+1 quorum, 500ms view change                     │
│  │   ├── erlmcp_flow_gossip (gen_server)        [permanent, 5000ms]    │
│  │   │   └── Push-pull: fanout=3, round=1s                            │
│  │   ├── erlmcp_flow_election (gen_server)      [permanent, 5000ms]    │
│  │   │   └── Randomized timeout: 150-300ms                            │
│  │   └── erlmcp_flow_log_store (gen_server)     [permanent, 10000ms]   │
│  │       └── Mnesia/DETS backend, snapshot at 10K entries             │
│  │                                                                      │
│  └── erlmcp_flow_core_sup (one_for_one)        [permanent, infinity]   │
│      ├── erlmcp_flow_swarm_sup (simple_one_for_one) [perm, infinity]   │
│      │   └── erlmcp_flow_swarm (gen_server)   [transient, 5000ms] ◄── TIER 3
│      │       └── Dynamic swarm instances (mesh/hierarchical/ring/star) │
│      │                                                                  │
│      ├── erlmcp_flow_agent_sup (simple_one_for_one) [perm, infinity]   │
│      │   └── erlmcp_flow_agent (gen_server)   [temporary, 2000ms] ◄── TIER 3
│      │       └── Dynamic agent instances (worker/specialist/scout)     │
│      │                                                                  │
│      ├── erlmcp_flow_request_tracker (gen_server)  [perm, 5000ms]      │
│      │   └── ETS-backed: UUID → pending request                        │
│      │                                                                  │
│      ├── erlmcp_flow_task_queue (gen_server)       [perm, 5000ms]      │
│      │   └── Priority queue: high → normal → low                       │
│      │                                                                  │
│      ├── erlmcp_flow_load_balancer (gen_server)    [perm, 5000ms]      │
│      │   └── Strategies: round_robin, least_conn, weighted             │
│      │                                                                  │
│      └── erlmcp_flow_observer_sup (one_for_one)    [perm, infinity]    │
│          ├── erlmcp_flow_otel_tracer (gen_server)  [perm, 5000ms]      │
│          ├── erlmcp_flow_metrics (gen_server)      [perm, 5000ms]      │
│          ├── erlmcp_flow_health (gen_server)       [perm, 5000ms]      │
│          ├── erlmcp_flow_chaos (gen_server)        [perm, 5000ms]      │
│          └── erlmcp_flow_circuit_breaker (gen_server) [perm, 5000ms]   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘

LEGEND:
  [restart, shutdown]  = Child spec parameters
  permanent  = Always restart
  transient  = Restart on abnormal exit
  temporary  = Never restart
  5000ms     = 5 second graceful shutdown
  infinity   = Wait for all children (supervisors only)
```

---

## Restart Strategy Matrix

| Supervisor | Strategy | Intensity | Period | Rationale |
|-----------|----------|-----------|--------|-----------|
| **erlmcp_flow_sup** | one_for_all | 3 | 60s | Registry + consensus must restart together |
| **erlmcp_flow_consensus_sup** | one_for_one | 5 | 60s | Each protocol fails independently |
| **erlmcp_flow_core_sup** | one_for_one | 5 | 60s | Isolated subsystem failures |
| **erlmcp_flow_swarm_sup** | simple_one_for_one | 5 | 60s | Dynamic swarm instances |
| **erlmcp_flow_agent_sup** | simple_one_for_one | 10 | 60s | Tolerant for exploratory tasks |
| **erlmcp_flow_observer_sup** | one_for_one | 10 | 60s | Non-critical observability |

---

## Child Spec Reference

### Registry (TIER 1)

```erlang
#{id => erlmcp_flow_registry,
  start => {erlmcp_flow_registry, start_link, []},
  restart => permanent,    % Always restart (critical)
  shutdown => 5000,         % 5s graceful shutdown
  type => worker,
  modules => [erlmcp_flow_registry]}
```

**Failure Impact**: All agent lookups fail → triggers one_for_all restart
**Recovery Time**: ~200ms (registry init) + ~300ms (consensus re-election)

### Consensus Workers (TIER 2)

```erlang
%% Raft
#{id => erlmcp_flow_raft,
  start => {erlmcp_flow_raft, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_flow_raft]}

%% Byzantine (PBFT)
#{id => erlmcp_flow_byzantine,
  start => {erlmcp_flow_byzantine, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_flow_byzantine]}

%% Log Store
#{id => erlmcp_flow_log_store,
  start => {erlmcp_flow_log_store, start_link, []},
  restart => permanent,
  shutdown => 10000,  % 10s to flush writes
  type => worker,
  modules => [erlmcp_flow_log_store]}
```

**Failure Impact**: Leader election required → swarms pause operations
**Recovery Time**: ~150-300ms (Raft), ~500ms (Byzantine view change)

### Swarm Template (TIER 3)

```erlang
%% simple_one_for_one template
#{id => erlmcp_flow_swarm,
  start => {erlmcp_flow_swarm, start_link, []},
  restart => transient,     % Restart on abnormal exit only
  shutdown => 5000,          % 5s to flush pending messages
  type => worker,
  modules => [erlmcp_flow_swarm]}

%% API: Start swarm
erlmcp_flow_swarm_sup:start_child(SwarmId, Topology, Config).
```

**Failure Impact**: Swarm coordinator crashes → state recovery from Raft log
**Recovery Time**: ~200ms (supervisor restart + state recovery)

### Agent Template (TIER 3)

```erlang
%% simple_one_for_one template
#{id => erlmcp_flow_agent,
  start => {erlmcp_flow_agent, start_link, []},
  restart => temporary,      % NEVER restart (swarm requeues task)
  shutdown => 2000,           % 2s to cancel pending ops
  type => worker,
  modules => [erlmcp_flow_agent]}

%% API: Start agent
erlmcp_flow_agent_sup:start_child(AgentId, Role, Config).
```

**Failure Impact**: Agent crashes → swarm requeues task to different agent
**Recovery Time**: <50ms (task requeue + agent selection)

---

## Failure Scenarios & Recovery

### Scenario 1: Agent Crash

```
┌──────────────────────────────────────────────────────────────┐
│ AGENT CRASH                                                  │
├──────────────────────────────────────────────────────────────┤
│ Trigger:  Agent gen_server crashes (OOM, exception)         │
│ Restart:  temporary → NO automatic restart                   │
│ Action:   Swarm requeues task to different agent            │
│ Time:     <50ms (task requeue + agent selection)            │
│ Impact:   Single task delayed, no cascade                    │
└──────────────────────────────────────────────────────────────┘

Timeline:
  T+0ms:    Agent crashes (exit signal)
  T+10ms:   Swarm receives {'DOWN', MonitorRef, process, Pid, Reason}
  T+20ms:   Task requeued to task queue (retry count++)
  T+30ms:   Load balancer selects different agent
  T+40ms:   New agent receives task, execution resumes
  T+50ms:   Recovery complete

Code:
  handle_info({'DOWN', _Ref, process, AgentPid, Reason}, State) ->
      Task = find_pending_task(AgentPid, State),
      NewState = requeue_task(Task, State),
      {noreply, select_new_agent(Task, NewState)}.
```

### Scenario 2: Swarm Coordinator Crash

```
┌──────────────────────────────────────────────────────────────┐
│ SWARM COORDINATOR CRASH                                      │
├──────────────────────────────────────────────────────────────┤
│ Trigger:  Swarm gen_server crashes (state inconsistency)    │
│ Restart:  transient → restart on abnormal exit              │
│ Action:   Supervisor restarts, recover from Raft log        │
│ Time:     ~200ms (restart + state recovery)                  │
│ Impact:   Brief pause in task distribution                   │
└──────────────────────────────────────────────────────────────┘

Timeline:
  T+0ms:    Swarm crashes (exit signal)
  T+50ms:   Supervisor detects exit, initiates restart
  T+100ms:  New swarm process spawned, init/1 called
  T+120ms:  State recovered from Raft log
  T+150ms:  Agent list recovered from gproc registry
  T+180ms:  Task queue recovered from ETS
  T+200ms:  Resume task distribution

Code:
  init({SwarmId, Topology, Config}) ->
      {ok, CommittedState} = erlmcp_flow_raft:read_latest_commit(SwarmId),
      AgentPids = gproc:lookup_values({n, l, {agent, '_', SwarmId}}),
      TaskQueue = erlmcp_flow_task_queue:get_queue(SwarmId),
      State = #state{agents = AgentPids, task_queue = TaskQueue, ...},
      {ok, State}.
```

### Scenario 3: Byzantine Failure

```
┌──────────────────────────────────────────────────────────────┐
│ BYZANTINE FAILURE (Consensus)                                │
├──────────────────────────────────────────────────────────────┤
│ Trigger:  Byzantine node sends conflicting messages         │
│ Restart:  N/A (consensus protocol handles)                  │
│ Action:   PBFT view change, elect new primary               │
│ Time:     ~500ms (PBFT view change timeout)                 │
│ Impact:   All swarms pause until consensus restored          │
└──────────────────────────────────────────────────────────────┘

Timeline:
  T+0ms:    Byzantine behavior detected (conflicting pre-prepare)
  T+50ms:   Backup broadcasts VIEW-CHANGE to all replicas
  T+200ms:  2f+1 replicas agree on view change
  T+350ms:  New primary (view+1 mod N) broadcasts NEW-VIEW
  T+500ms:  Consensus stabilizes, swarms resume

Code:
  handle_info({consensus_event, byzantine_detected, Node}, State) ->
      NewState = pause_agents(State),
      erlmcp_flow_byzantine:trigger_view_change(Node),
      {noreply, NewState}.
```

### Scenario 4: Network Partition

```
┌──────────────────────────────────────────────────────────────┐
│ NETWORK PARTITION                                            │
├──────────────────────────────────────────────────────────────┤
│ Trigger:  Network link failure between nodes                │
│ Restart:  N/A (wait for partition healing)                  │
│ Action:   Quorum check, pause minority partition            │
│ Time:     ~1-3s (gossip detection + leader election)        │
│ Impact:   Minority partition pauses, majority continues      │
└──────────────────────────────────────────────────────────────┘

Timeline:
  T+0s:     Network partition occurs
  T+1s:     Gossip detects missing heartbeats (1 round)
  T+2s:     Gossip confirms partition (2 consecutive rounds)
  T+3s:     Raft detects quorum loss in minority partition
  T+3.5s:   Minority swarms pause operations
  [Partition heals]
  T+5s:     Gossip propagates state across partitions
  T+5.3s:   Raft re-elects leader in majority partition
  T+5.5s:   Minority syncs from majority leader
  T+6s:     All swarms resume

Code:
  handle_info({consensus_event, network_partition, Nodes}, State) ->
      case erlmcp_flow_raft:check_quorum() of
          true  -> {noreply, State};  % Majority: continue
          false -> {noreply, pause_agents(State)}  % Minority: pause
      end.
```

### Scenario 5: Cascading Failure Prevention

```
┌──────────────────────────────────────────────────────────────┐
│ CASCADING FAILURE (Circuit Breaker)                         │
├──────────────────────────────────────────────────────────────┤
│ Trigger:  Multiple agent crashes (5 consecutive)            │
│ Restart:  N/A (circuit breaker prevents)                    │
│ Action:   Open circuit breaker, pause for 60s               │
│ Time:     ~60s cooldown period                               │
│ Impact:   Task distribution paused to prevent cascade        │
└──────────────────────────────────────────────────────────────┘

Timeline:
  T+0s:     Agent 1 crashes
  T+1s:     Agent 2 crashes
  T+2s:     Agent 3 crashes
  T+3s:     Agent 4 crashes
  T+4s:     Agent 5 crashes → threshold reached
  T+4.1s:   Circuit breaker opens, pause task distribution
  T+64s:    Circuit breaker reset timer fires
  T+64.1s:  Transition to half-open (allow 1 test task)
  T+65s:    Test task succeeds → close circuit breaker
  T+65.1s:  Resume normal operations

Code:
  handle_info({agent_crash, _, _}, State = #state{consecutive_crashes = N})
      when N >= 5 ->
      NewState = State#state{circuit_breaker = open},
      erlang:send_after(60000, self(), reset_circuit_breaker),
      {noreply, NewState}.
```

---

## Resilience Limits

### Backpressure Thresholds

```
Task Queue:
  Max Size:     10,000 tasks per swarm
  On Overflow:  {error, queue_full}
  Priority:     high (100) → normal (50) → low (10)

Agent Pool:
  Min Size:     5 agents per role
  Max Size:     100 agents per role
  Idle Timeout: 30s (terminate idle agents)

Connection Limits:
  Max Connections:  10,000 per node
  On Overflow:      Circuit breaker opens
```

### Circuit Breaker Config

```erlang
-record(circuit_breaker, {
    state = closed :: closed | open | half_open,
    failure_count = 0 :: non_neg_integer(),
    threshold = 5 :: non_neg_integer(),      % 5 consecutive failures
    timeout = 60000 :: non_neg_integer(),    % 60s cooldown
    last_failure :: non_neg_integer()        % Timestamp
}).
```

### Supervisor Intensity Limits

```
erlmcp_flow_sup:            3 restarts / 60s (conservative)
erlmcp_flow_consensus_sup:  5 restarts / 60s (standard)
erlmcp_flow_core_sup:       5 restarts / 60s (standard)
erlmcp_flow_swarm_sup:      5 restarts / 60s (standard)
erlmcp_flow_agent_sup:     10 restarts / 60s (tolerant)
erlmcp_flow_observer_sup:  10 restarts / 60s (tolerant)
```

---

## Integration Points

### With erlmcp Core

```erlang
%% Registry integration (gproc)
gproc:reg({n, l, {agent, Role, AgentId}}, Pid).
gproc:lookup_values({n, l, {agent, worker, '_'}}).

%% Transport integration
handle_info({transport_data, TransportId, Data}, State) ->
    Request = erlmcp_json_rpc:decode(Data),
    {ok, AgentPid} = erlmcp_flow_load_balancer:select_agent(Request),
    gen_server:cast(AgentPid, {execute_task, Request}),
    {noreply, State}.

%% Observability integration
SpanCtx = erlmcp_otel:start_span(<<"agent.task.execute">>, Attrs),
Result = execute_task_impl(Task),
erlmcp_otel:end_span(SpanCtx).
```

---

## Testing Checklist

- [ ] **Unit Tests** (EUnit):
  - [ ] Agent crash triggers task requeue
  - [ ] Swarm coordinator crash recovers state from Raft log
  - [ ] Byzantine failure triggers swarm pause
  - [ ] Network partition triggers quorum check
  - [ ] Circuit breaker opens after 5 consecutive crashes

- [ ] **Chaos Tests** (Common Test):
  - [ ] Kill Raft leader → new leader elected <300ms
  - [ ] Kill Byzantine primary → view change <500ms
  - [ ] Network partition → minority pauses, majority continues
  - [ ] Agent pool exhaustion → backpressure activates
  - [ ] Cascading failures → circuit breaker prevents cascade

- [ ] **Property Tests** (Proper):
  - [ ] ∀agent_crash. ∃task. requeue(task)
  - [ ] ∀consensus_failure. ∃leader. elected_within(300ms)
  - [ ] ∀network_partition. ∃quorum. majority_continues()
  - [ ] ∀circuit_open. ∀request. rejected(request)

---

## Quick Commands

```bash
# Start erlmcp-flow application
erlmcp_flow:start().

# Start a swarm
{ok, SwarmPid} = erlmcp_flow_swarm_sup:start_child(
    <<"swarm-1">>, mesh, #{}).

# Start an agent
{ok, AgentPid} = erlmcp_flow_agent_sup:start_child(
    <<"agent-1">>, worker, #{}).

# Check supervision tree
observer:start().
%% Applications → erlmcp_flow → Supervision Tree

# Inject chaos (testing)
erlmcp_flow_chaos:inject_latency(AgentPid, 500).
erlmcp_flow_chaos:kill_process(AgentPid).
erlmcp_flow_chaos:trigger_partition([Node1, Node2]).

# Check consensus state
erlmcp_flow_raft:get_leader().
erlmcp_flow_byzantine:is_stable().
erlmcp_flow_gossip:get_peers().

# Check circuit breaker
erlmcp_flow_circuit_breaker:get_state().
```

---

**Version**: 1.0.0
**Author**: Erlang Architect Agent
**Date**: 2026-02-02
**Status**: Quick Reference Complete
