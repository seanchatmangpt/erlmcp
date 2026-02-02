# erlmcp_flow MVP Architecture v0.1.0

**Date**: 2026-02-02
**Status**: Production (MVP)
**Version**: 0.1.0
**Implementation Roadmap**: [ERLMCP_FLOW_SPARC_WORKFLOW_IMPLEMENTATION_ROADMAP.md](ERLMCP_FLOW_SPARC_WORKFLOW_IMPLEMENTATION_ROADMAP.md)

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Architecture Overview](#architecture-overview)
3. [Supervision Tree](#supervision-tree)
4. [API Reference](#api-reference)
5. [Known Limitations](#known-limitations)
6. [Future Roadmap](#future-roadmap)
7. [Examples](#examples)

---

## Quick Start

### Starting the erlmcp_flow Application

```erlang
%% In your Erlang shell:
1> application:start(erlmcp_flow).
ok

%% Or in your application supervisor:
{ok, Pid} = erlmcp_flow_sup:start_link().
```

### Spawning a Swarm of Agents

```erlang
%% Register agent in the flow registry
AgentId = <<"agent-worker-1">>,
Capabilities = [<<"execute">>, <<"monitor">>, <<"report">>],
Role = worker,

ok = erlmcp_flow_registry:register_agent(AgentId, self(), Capabilities).

%% Query agents by capability
Agents = erlmcp_flow_registry:find_agents_by_capability(<<"execute">>).

%% Get agent load (task queue length)
Load = erlmcp_flow_registry:get_agent_load(AgentId).
```

### Submitting Tasks

```erlang
%% Create and submit a task to an agent
Task = #{
    id => <<"task-001">>,
    type => <<"analyze">>,
    priority => high,
    params => #{input => <<"data">>},
    timeout => 5000
},

%% Assign task (cast - asynchronous)
ok = erlmcp_flow_agent:assign_task(AgentPid, Task).

%% Or get agent by ID from registry
{ok, AgentPid} = erlmcp_flow_registry:find_agent(AgentId),
ok = erlmcp_flow_agent:assign_task(AgentPid, Task).
```

### Monitoring Task Completion

```erlang
%% Get agent state (includes executing task, queue length, stats)
{ok, AgentState} = erlmcp_flow_agent:get_state(AgentPid).

%% Get agent metrics (performance statistics)
{ok, Metrics} = erlmcp_flow_agent:get_metrics(AgentPid).

%% Cancel running task
ok = erlmcp_flow_agent:cancel_task(AgentPid).
```

### Running the Complete Demo

```erlang
%% Basic demo with defaults (3 agents, 10 tasks)
erlmcp_flow_demo:start().

%% Custom configuration
erlmcp_flow_demo:start(#{
    agent_count => 5,
    task_count => 20,
    timeout_ms => 15000,
    log_level => debug
}).
```

---

## Architecture Overview

### System Components

erlmcp_flow is a lightweight agent orchestration framework built on OTP 28+ with:

- **Process-per-Agent**: Each agent runs in isolated gen_server
- **gproc Registry**: O(log N) agent discovery and capability-based routing
- **Load Balancing**: Per-agent task queue with priority handling
- **Error Recovery**: Exponential backoff with configurable retry strategies
- **Health Monitoring**: Heartbeat-based failure detection
- **Non-blocking Init**: Async agent connection via `{continue, connect}`

### Data Flow

```
┌────────────────┐
│  Task Submitter│
└────────┬────────┘
         │ assign_task
         ▼
┌────────────────────────┐
│  erlmcp_flow_agent     │
│  (gen_server)          │
│                        │
│  State Machine:        │
│  idle→assigned→        │
│  executing→done        │
└────────┬───────────────┘
         │ task_complete
         │ / task_failed
         ▼
┌────────────────────────┐
│  Coordinator /         │
│  Monitor               │
│  (listens on messages) │
└────────────────────────┘
         │
         ▼
┌────────────────────────┐
│  Registry              │
│  (gproc-backed)        │
│  - Agent lookup        │
│  - Load tracking       │
│  - Heartbeat status    │
└────────────────────────┘
```

### Task Lifecycle

```
1. QUEUED
   └─ Task submitted to agent, waiting for execution

2. EXECUTING
   └─ Task running on agent
   └─ Timeout timer started
   └─ Heartbeat monitoring enabled

3. DONE
   ├─ COMPLETED: Task finished successfully
   │  └─ Result stored
   │  └─ Stats updated
   │  └─ Next task in queue executed
   │
   └─ FAILED: Task encountered error
      ├─ Retry eligible?
      │  ├─ YES: Schedule retry with exponential backoff
      │  └─ NO: Report failure, process next task
      └─ Max retries exhausted

4. CANCELLED
   └─ Task cancelled by coordinator
   └─ Agent returns to idle
```

---

## Supervision Tree

### Tier 1: Top-Level Supervisor (one_for_all)

```
erlmcp_flow_sup
│
├─ erlmcp_flow_registry
│  └─ Worker (permanent)
│     └─ Manages gproc registrations, heartbeats
│
├─ erlmcp_flow_q_learning
│  └─ Worker (permanent)
│     └─ Learns agent performance patterns
│
├─ erlmcp_flow_circuit_breaker
│  └─ Worker (permanent)
│     └─ Prevents cascading failures
│
├─ erlmcp_flow_correlation_tracker
│  └─ Worker (permanent)
│     └─ Tracks request correlation IDs
│
├─ erlmcp_flow_byzantine
│  └─ Worker (permanent)
│     └─ Detects Byzantine failures
│
├─ erlmcp_flow_failure_detector
│  └─ Worker (permanent)
│     └─ Monitors agent health
│
└─ erlmcp_flow_router
   └─ Worker (permanent)
      └─ Routes tasks based on load/capabilities
```

### Tier 2: Agent Supervisor (simple_one_for_one)

```
erlmcp_flow_agent_sup
│
├─ erlmcp_flow_agent[1]
│  └─ gen_server (worker, permanent)
│     └─ Role: coordinator
│     └─ Capabilities: [orchestrate, schedule, route]
│
├─ erlmcp_flow_agent[2]
│  └─ gen_server (worker, permanent)
│     └─ Role: specialist
│     └─ Capabilities: [analyze, optimize, refactor]
│
└─ erlmcp_flow_agent[N]
   └─ gen_server (worker, permanent)
      └─ Role: worker
      └─ Capabilities: [execute, monitor, report]
```

### Restart Strategies

| Component | Strategy | Intensity | Period | Restart |
|-----------|----------|-----------|--------|---------|
| erlmcp_flow_sup | one_for_all | 5 | 60s | permanent |
| erlmcp_flow_agent_sup | simple_one_for_one | - | - | permanent |
| Individual agents | (managed by sup) | - | - | permanent |

**Semantics**:
- `one_for_all`: If any core component crashes, restart everything
- `simple_one_for_one`: Dynamically spawn agents, isolate failures
- Intensity: 5 crashes per 60 seconds before supervisor crashes

---

## API Reference

### erlmcp_flow_registry

**Agent Registration**

```erlang
-spec register_agent(AgentId, Pid, Capabilities) -> ok | {error, term()}
    when AgentId :: binary(),
         Pid :: pid(),
         Capabilities :: [binary()].

register_agent(AgentId, Pid, Capabilities).
%% Registers agent in gproc registry with capabilities
%% Returns: ok | {error, already_registered}
```

**Agent Discovery**

```erlang
-spec find_agent(AgentId) -> {ok, pid()} | {error, not_found}
    when AgentId :: binary().

find_agent(AgentId).
%% Lookup agent by ID
%% Returns: {ok, Pid} | {error, not_found}

-spec find_agents_by_capability(Capability) -> [pid()]
    when Capability :: binary().

find_agents_by_capability(Capability).
%% Find all agents with given capability
%% Returns: [Pid, ...]

-spec query_agents(Capabilities) -> [binary()]
    when Capabilities :: [binary()].

query_agents(Capabilities).
%% Find agents matching ALL capabilities (AND logic)
%% Returns: [AgentId, ...]
```

**Load Management**

```erlang
-spec get_agent_load(AgentId) -> non_neg_integer()
    when AgentId :: binary().

get_agent_load(AgentId).
%% Get current task queue length
%% Returns: Load (0..N)

-spec increment_load(AgentId) -> non_neg_integer()
    when AgentId :: binary().

increment_load(AgentId).
%% Increment task count (called on task assignment)

-spec decrement_load(AgentId) -> non_neg_integer()
    when AgentId :: binary().

decrement_load(AgentId).
%% Decrement task count (called on task completion)
```

**Heartbeat Management**

```erlang
-spec update_heartbeat(AgentId) -> ok
    when AgentId :: binary().

update_heartbeat(AgentId).
%% Update agent's last heartbeat timestamp

-spec get_last_heartbeat(AgentId) -> {ok, erlang:timestamp()} | {error, not_found}
    when AgentId :: binary().

get_last_heartbeat(AgentId).
%% Get last heartbeat time
%% Returns: {ok, Timestamp} | {error, not_found}
```

### erlmcp_flow_agent

**Task Management**

```erlang
-spec assign_task(Pid, Task) -> ok
    when Pid :: pid(),
         Task :: map().

assign_task(Pid, Task).
%% Assign task to agent (async cast)
%% Task map: #{id, type, priority, params, timeout, ...}

-spec cancel_task(Pid) -> ok | {error, no_task_executing}
    when Pid :: pid().

cancel_task(Pid).
%% Cancel currently executing task

-spec get_state(Pid) -> {ok, map()} | {error, term()}
    when Pid :: pid().

get_state(Pid).
%% Get agent state machine state
%% Returns: #{id, role, status, queue_length, executing_task, stats, ...}

-spec get_metrics(Pid) -> {ok, map()} | {error, term()}
    when Pid :: pid().

get_metrics(Pid).
%% Get agent performance metrics
%% Returns: #{tasks_completed, tasks_failed, success_rate, avg_latency, ...}
```

**Agent Messages (Internal)**

```erlang
%% Agent → Coordinator
{agent_ready, AgentId :: binary(), Role :: atom(), Pid :: pid()}
{agent_heartbeat, AgentId :: binary(), Status :: atom(), Pid :: pid()}
{task_complete, TaskId :: binary(), Result :: term(), AgentId :: binary()}
{task_failed, TaskId :: binary(), Reason :: term(), AgentId :: binary()}
{agent_terminating, AgentId :: binary(), Reason :: term()}
{request_failover_tasks, FailedAgentId :: binary(), RequesterPid :: pid()}

%% Coordinator → Agent
{assign_task, Task :: map()}
{cancel_task, TaskId :: binary()}
{pause_agent}
{resume_agent}
{update_config, NewConfig :: map()}
```

### Utility Functions

**Demo & Testing**

```erlang
-spec erlmcp_flow_demo:start() -> ok | {error, term()}.
erlmcp_flow_demo:start().
%% Run MVP demo with default config (3 agents, 10 tasks)

-spec erlmcp_flow_demo:start(Config :: map()) -> ok | {error, term()}.
erlmcp_flow_demo:start(#{
    agent_count => 5,
    task_count => 20,
    timeout_ms => 15000,
    log_level => debug
}).
%% Run demo with custom configuration
```

---

## Known Limitations

### v0.1.0 (MVP)

#### 1. No Byzantine Fault Tolerance
- **Current**: Simple crash detection via heartbeats
- **Limitation**: Cannot detect malicious/incorrect agents
- **Workaround**: Use external validation layer
- **Future**: v0.2+ will add Byzantine consensus (PBFT)

#### 2. No Gossip Protocol
- **Current**: Centralized registry (gproc)
- **Limitation**: No peer-to-peer agent discovery
- **Workaround**: Registry acts as single source of truth
- **Future**: v0.2+ will add gossip for large swarms (100K+ agents)

#### 3. No Persistent State
- **Current**: In-memory ETS tables only
- **Limitation**: Agent state lost on restart
- **Workaround**: Application stores task receipts externally
- **Future**: v0.3+ will add Mnesia/persistent session backends

#### 4. Single-Node Only
- **Current**: Local gproc registry
- **Limitation**: Cannot span multiple Erlang nodes
- **Workaround**: Run separate erlmcp_flow instance per node + external coordination
- **Future**: v0.2+ distributed registry (gproc sync)

#### 5. No Task Priority Preemption
- **Current**: Priority affects scheduling order only
- **Limitation**: Running task won't preempt for higher-priority task
- **Workaround**: Set timeouts appropriately
- **Future**: v0.3+ soft preemption via {continue, ...}

#### 6. Limited Monitoring
- **Current**: Basic heartbeat + task completion metrics
- **Limitation**: No OTEL tracing, no Prometheus metrics
- **Workaround**: Application logs all messages
- **Future**: v0.2+ OpenTelemetry integration

#### 7. No Consensus for Distributed Decisions
- **Current**: Registry-based routing only
- **Limitation**: Cannot perform consensus-based task assignment
- **Workaround**: External orchestrator makes decisions
- **Future**: v0.2+ Raft consensus (Phase 5 of roadmap)

#### 8. Fixed Retry Strategy
- **Current**: Exponential backoff with fixed parameters
- **Limitation**: Cannot adapt retry strategy based on failure type
- **Workaround**: Configure retry parameters at init time
- **Future**: v0.2+ adaptive retry via Q-learning

#### 9. No Service Mesh Integration
- **Current**: No mTLS, no circuit breakers
- **Limitation**: Unsafe for untrusted networks
- **Workaround**: Run in trusted network only
- **Future**: v0.3+ mTLS + circuit breaker patterns

#### 10. Limited Error Context
- **Current**: Basic error tuples
- **Limitation**: Hard to debug complex failure scenarios
- **Workaround**: Enable debug logging, read agent metrics
- **Future**: v0.2+ correlation tracking + request tracing

### Constraints

| Aspect | Limit | Notes |
|--------|-------|-------|
| Max agents per node | 10K | ETS hash table performance |
| Max task queue per agent | 1M | Memory per agent |
| Task timeout minimum | 100ms | Erlang timer resolution |
| Heartbeat interval | >= 1s | Network polling overhead |
| Concurrent tasks | 1 per agent | Sequential task processing |
| Task message size | < 128MB | Erlang message limit |
| Registry lookup time | O(log N) | gproc performance |

---

## Future Roadmap

### v0.2.0 (Q1 2026) - Enhanced Consensus & Monitoring

**Focus**: Multi-agent coordination and observability

#### Phase 1: Consensus Protocols (Week 5-6)
- [ ] Raft consensus for task assignment
- [ ] PBFT for Byzantine fault tolerance
- [ ] Consensus behavior abstraction
- [ ] Tests: Raft election, log replication, PBFT view change

#### Phase 2: Distributed Registry (Week 7)
- [ ] gproc sync for multi-node deployments
- [ ] Distributed agent discovery
- [ ] Cross-node heartbeat detection
- [ ] Tests: Multi-node failover, split-brain recovery

#### Phase 3: Observability (Week 8)
- [ ] OpenTelemetry integration
- [ ] Prometheus metrics export
- [ ] Distributed tracing (trace context propagation)
- [ ] Request correlation (UUID tracking)
- [ ] Tests: Tracing end-to-end workflows

#### Phase 4: Enhanced Error Recovery (Week 9)
- [ ] Adaptive retry with Q-learning
- [ ] Circuit breaker patterns
- [ ] Error classification (transient vs. permanent)
- [ ] Automatic failover with fallback agents
- [ ] Tests: All recovery scenarios, property-based tests

#### Performance Targets
- Registry: 553K ops/sec (maintained)
- Consensus: <200ms p99 (Raft), <500ms p99 (PBFT)
- Routing: <100μs p99 lookup
- Task throughput: 50K tasks/sec

### v0.3.0 (Q2 2026) - Persistence & Clustering

**Focus**: Production readiness for 100K agents

#### Phase 1: Session Persistence (Week 10-11)
- [ ] Mnesia backend for agent state
- [ ] Durable session recovery
- [ ] State replication across cluster
- [ ] Receipt chain for audit trail

#### Phase 2: Large-Scale Clustering (Week 12)
- [ ] Gossip protocol for 100K+ agents
- [ ] Hierarchical agent topologies
- [ ] Geo-distributed routing

#### Phase 3: Security & Compliance (Week 13)
- [ ] mTLS agent communication
- [ ] RBAC for task assignment
- [ ] Compliance audit logging
- [ ] GDPR-compliant state deletion

### v0.4.0 (Q3 2026) - Advanced Orchestration

**Focus**: Complex workflow support

- [ ] DAG-based task dependencies
- [ ] Parallel workflow execution
- [ ] Human-in-the-loop decision points
- [ ] Machine learning-based routing optimization

### v0.5.0 (Q4 2026) - Production Hardening

**Focus**: Operational excellence

- [ ] Automated performance tuning
- [ ] Self-healing capabilities
- [ ] Canary deployments
- [ ] A/B testing framework

### Long-Term Vision (2027+)

**Agent Mesh**: Federated erlmcp_flow networks spanning multiple cloud providers
**AGI Integration**: Direct integration with Claude API for autonomous agent orchestration
**Semantic Routing**: LLM-based intelligent task-to-agent assignment

---

## Examples

### Example 1: Basic Agent Registration

```erlang
%% Start erlmcp_flow
application:start(erlmcp_flow).

%% Create and register 3 agents
Agents = [
    {<<"agent-1">>, worker, [<<"execute">>, <<"monitor">>]},
    {<<"agent-2">>, specialist, [<<"analyze">>, <<"optimize">>]},
    {<<"agent-3">>, coordinator, [<<"schedule">>, <<"route">>]}
],

lists:foreach(fun({AgentId, Role, Capabilities}) ->
    ok = erlmcp_flow_registry:register_agent(
        AgentId,
        self(),  % In real code: spawn_agent_process(Role)
        Capabilities
    ),
    io:format("Registered ~s (~w)~n", [AgentId, Role])
end, Agents).

%% Query agents by capability
WorkerAgents = erlmcp_flow_registry:find_agents_by_capability(<<"execute">>),
io:format("Found ~w agents with 'execute' capability~n", [length(WorkerAgents)]).
```

### Example 2: Task Submission & Monitoring

```erlang
%% Get agent
{ok, AgentPid} = erlmcp_flow_registry:find_agent(<<"agent-1">>),

%% Create task
Task = #{
    id => <<"task-001">>,
    type => <<"process">>,
    priority => high,
    params => #{data => <<"input">>},
    timeout => 5000
},

%% Submit task (async)
ok = erlmcp_flow_agent:assign_task(AgentPid, Task),

%% Monitor execution
receive
    {task_complete, <<"task-001">>, Result} ->
        io:format("Task completed: ~p~n", [Result]);
    {task_failed, <<"task-001">>, Reason} ->
        io:format("Task failed: ~p~n", [Reason])
after
    6000 ->
        io:format("Task timeout~n")
end.
```

### Example 3: Full Demo

See `/home/user/erlmcp/examples/erlmcp_flow_demo.erl` for complete working example:

```bash
# Run from Erlang shell
erlmcp_flow_demo:start().

# Or with custom config
erlmcp_flow_demo:start(#{
    agent_count => 10,
    task_count => 100,
    timeout_ms => 30000
}).
```

---

## Debugging & Troubleshooting

### Enable Debug Logging

```erlang
logger:set_primary_config(level, debug).
erlmcp_flow_demo:start(#{log_level => debug}).
```

### Check Agent Health

```erlang
%% Get all agents
AllAgents = erlmcp_flow_registry:find_agents_by_capability(<<"any">>).

%% Check heartbeat
{ok, LastHeartbeat} = erlmcp_flow_registry:get_last_heartbeat(AgentId),
TimeSinceHeartbeat = erlang:system_time(millisecond) - LastHeartbeat,
io:format("Last heartbeat: ~w ms ago~n", [TimeSinceHeartbeat]).

%% Get agent metrics
{ok, Metrics} = erlmcp_flow_agent:get_metrics(AgentPid),
io:format("Agent metrics: ~p~n", [Metrics]).
```

### Monitor Load Distribution

```erlang
%% Get load for all agents
Loads = maps:from_list([
    {AgentId, erlmcp_flow_registry:get_agent_load(AgentId)}
    || AgentId <- AllAgentIds
]),

maps:foreach(fun(Id, Load) ->
    io:format("~s: ~w tasks queued~n", [Id, Load])
end, Loads).
```

---

## Related Documentation

- **Implementation Details**: [ERLMCP_FLOW_SPARC_WORKFLOW_IMPLEMENTATION_ROADMAP.md](ERLMCP_FLOW_SPARC_WORKFLOW_IMPLEMENTATION_ROADMAP.md)
- **Agent Design**: [erlmcp_flow_agent_design.md](erlmcp_flow_agent_design.md)
- **Core System**: [../README.md](../README.md)
- **Running Examples**: [../examples/README.md](../examples/README.md)

---

## Contributing

For implementation details, testing strategy, and quality gates, see the [ERLMCP_FLOW_SPARC_WORKFLOW_IMPLEMENTATION_ROADMAP.md](ERLMCP_FLOW_SPARC_WORKFLOW_IMPLEMENTATION_ROADMAP.md).

**Development Environment**:
- Erlang/OTP 28+
- rebar3 3.21+
- Chicago TDD workflow
- All tests must pass: `make check`

---

**Status**: ✅ MVP Complete (v0.1.0)
**Last Updated**: 2026-02-02
**Next Milestone**: v0.2.0 (Consensus & Monitoring)
