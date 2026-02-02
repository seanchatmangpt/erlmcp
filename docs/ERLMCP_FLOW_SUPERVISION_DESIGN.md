# erlmcp-flow Supervision Tree Design v1.0.0

**Design Document**: OTP-Compliant 3-Tier Supervision Hierarchy
**Author**: Erlang Architect Agent
**Date**: 2026-02-02
**Status**: Architecture Design Phase

---

## Executive Summary

This document defines the supervision tree architecture for **erlmcp-flow**, an autonomous agent orchestration framework built on Erlang/OTP. The design follows the proven 3-tier supervision pattern from erlmcp core, with specialized strategies for agent swarms, consensus protocols, and Byzantine fault tolerance.

**Key Architectural Principles**:
- **Bulkhead Pattern**: Failures isolated within supervision boundaries
- **Let-It-Crash**: Supervised processes restart independently
- **Process-per-Agent**: Each agent is a separate gen_server (O(1) isolation)
- **Registry-Based Routing**: gproc for O(log N) agent lookup
- **Consensus-Aware**: Raft leader election with Byzantine fault tolerance

---

## 3-Tier Supervision Hierarchy

### TIER 1: Application Supervisor (one_for_all)

**Strategy**: `one_for_all` - Critical dependencies restart together
**Rationale**: Registry failure requires consensus restart; consensus failure requires swarm reconfiguration
**Recovery Time**: ~500-1000ms for full subsystem restart

```erlang
erlmcp_flow_sup (one_for_all)
├── erlmcp_flow_registry (gen_server)         % gproc-based routing
├── erlmcp_flow_consensus_sup (one_for_one)   % Consensus protocols
│   ├── erlmcp_flow_raft (gen_server)         % Raft leader
│   ├── erlmcp_flow_byzantine (gen_server)     % PBFT for high-security
│   └── erlmcp_flow_gossip (gen_server)       % State propagation
└── erlmcp_flow_core_sup (one_for_one)        % Core infrastructure
    ├── erlmcp_flow_swarm_sup (simple_one_for_one)  % Dynamic swarms
    ├── erlmcp_flow_agent_sup (simple_one_for_one)  % Dynamic agents
    └── erlmcp_flow_observer_sup (one_for_one)      % Isolated observability
```

**Child Spec: erlmcp_flow_sup**

```erlang
-module(erlmcp_flow_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,    % Critical: registry + consensus must restart together
        intensity => 3,              % Max 3 restarts (conservative for consensus)
        period => 60                 % Within 60 seconds
    },

    ChildSpecs = [
        %% ================================================================
        %% REGISTRY: Agent routing and process discovery (gproc-based)
        %% Critical: Must start FIRST - all other components depend on it
        %% Failure Impact: All agent lookups fail → triggers one_for_all restart
        %% Recovery: ~200ms registry initialization + ~300ms consensus re-election
        %% ================================================================
        #{id => erlmcp_flow_registry,
          start => {erlmcp_flow_registry, start_link, []},
          restart => permanent,      % Always restart
          shutdown => 5000,           % 5s graceful shutdown
          type => worker,
          modules => [erlmcp_flow_registry]},

        %% ================================================================
        %% CONSENSUS SUPERVISOR: Manages Raft/Byzantine/Gossip protocols
        %% Strategy: one_for_one (each protocol fails independently)
        %% Critical: Starts SECOND - depends on registry for leader election
        %% Failure Impact: Leader election required → swarm pauses operations
        %% Recovery: ~150-300ms randomized election timeout
        %% ================================================================
        #{id => erlmcp_flow_consensus_sup,
          start => {erlmcp_flow_consensus_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,       % Supervisor: wait for all children
          type => supervisor,
          modules => [erlmcp_flow_consensus_sup]},

        %% ================================================================
        %% CORE SUPERVISOR: Swarms, agents, and observability
        %% Strategy: one_for_one (isolated failures)
        %% Critical: Starts LAST - depends on registry + consensus
        %% Failure Impact: Individual subsystem failures (no cascade)
        %% Recovery: Per-component restart (agent: <50ms, swarm: <200ms)
        %% ================================================================
        #{id => erlmcp_flow_core_sup,
          start => {erlmcp_flow_core_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor,
          modules => [erlmcp_flow_core_sup]}
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Failure Mode Analysis (TIER 1)**:

| Component Crash | Restart Scope | Recovery Time | Impact |
|----------------|---------------|---------------|--------|
| **Registry** | ALL (one_for_all) | ~500ms | All agent routing fails; consensus re-elects leader |
| **Consensus Sup** | Consensus only | ~300ms | Leader election timeout; swarm operations pause |
| **Core Sup** | Core children | Per-component | Isolated failures (agent: <50ms, swarm: <200ms) |

---

### TIER 2: Service Supervisors (simple_one_for_one)

**Strategy**: `simple_one_for_one` - Dynamic worker pools
**Rationale**: Agents and swarms are spawned dynamically per-connection/per-task
**Recovery Time**: <50ms per agent, <200ms per swarm

#### Consensus Supervisor (one_for_one)

```erlang
-module(erlmcp_flow_consensus_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,    % Each consensus protocol fails independently
        intensity => 5,              % Max 5 restarts
        period => 60                 % Within 60 seconds
    },

    ChildSpecs = [
        %% ================================================================
        %% RAFT CONSENSUS: Leader election and log replication (CFT)
        %% States: follower, candidate, leader
        %% Quorum: (N+1)/2 for commit
        %% Timeout: 150-300ms randomized election timeout
        %% Failure: Triggers new election; swarm pauses until leader elected
        %% ================================================================
        #{id => erlmcp_flow_raft,
          start => {erlmcp_flow_raft, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_raft]},

        %% ================================================================
        %% BYZANTINE CONSENSUS: PBFT for high-security scenarios (BFT)
        %% Quorum: 2f+1 replicas (tolerates f Byzantine failures)
        %% Phases: pre-prepare → prepare → commit
        %% Timeout: 500ms per phase
        %% Failure: Triggers view change; swarm pauses until new primary elected
        %% ================================================================
        #{id => erlmcp_flow_byzantine,
          start => {erlmcp_flow_byzantine, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_byzantine]},

        %% ================================================================
        %% GOSSIP PROTOCOL: State propagation and failure detection
        %% Strategy: Push-pull gossip with exponential decay
        %% Fanout: 3 peers per round
        %% Round: Every 1s
        %% Failure: Non-critical; state may be stale for 1-3 rounds
        %% ================================================================
        #{id => erlmcp_flow_gossip,
          start => {erlmcp_flow_gossip, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_gossip]},

        %% ================================================================
        %% LEADER ELECTION: Randomized timeout for leader election
        %% Timeout: 150-300ms (randomized to prevent split votes)
        %% Quorum: (N+1)/2 votes required
        %% Failure: Re-election triggered; operations pause <300ms
        %% ================================================================
        #{id => erlmcp_flow_election,
          start => {erlmcp_flow_election, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_election]},

        %% ================================================================
        %% LOG STORE: Persistent Raft log (Mnesia/DETS backend)
        %% Durability: fsync after each commit
        %% Compaction: Snapshot at 10K entries
        %% Failure: Restart from last snapshot; replay log
        %% ================================================================
        #{id => erlmcp_flow_log_store,
          start => {erlmcp_flow_log_store, start_link, []},
          restart => permanent,
          shutdown => 10000,  % 10s to flush pending writes
          type => worker,
          modules => [erlmcp_flow_log_store]}
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Failure Mode Analysis (Consensus)**:

| Component Crash | Restart Scope | Recovery Time | Impact |
|----------------|---------------|---------------|--------|
| **Raft Leader** | Raft only | ~150-300ms | New leader election; operations pause |
| **Byzantine Primary** | Byzantine only | ~500ms | View change; operations pause |
| **Gossip** | Gossip only | <100ms | State may be stale for 1-3 rounds |
| **Election** | Election only | ~200ms | Re-election triggered |
| **Log Store** | Log only | ~500ms | Replay from snapshot |

#### Core Supervisor (one_for_one)

```erlang
-module(erlmcp_flow_core_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,    % Each subsystem fails independently
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% ================================================================
        %% SWARM SUPERVISOR: Dynamic swarm instances (simple_one_for_one)
        %% Each swarm is a separate gen_server coordinating multiple agents
        %% Restart: permanent (swarm coordinator must survive crashes)
        %% ================================================================
        #{id => erlmcp_flow_swarm_sup,
          start => {erlmcp_flow_swarm_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor,
          modules => [erlmcp_flow_swarm_sup]},

        %% ================================================================
        %% AGENT SUPERVISOR: Dynamic agent instances (simple_one_for_one)
        %% Each agent is a separate gen_server handling requests
        %% Restart: temporary (agents are ephemeral per-task)
        %% ================================================================
        #{id => erlmcp_flow_agent_sup,
          start => {erlmcp_flow_agent_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor,
          modules => [erlmcp_flow_agent_sup]},

        %% ================================================================
        %% REQUEST TRACKER: Request correlation (UUID → pending request)
        %% Critical: Must survive restarts to maintain request state
        %% ETS-based: named_table with {read_concurrency, true}
        %% ================================================================
        #{id => erlmcp_flow_request_tracker,
          start => {erlmcp_flow_request_tracker, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_request_tracker]},

        %% ================================================================
        %% TASK QUEUE: Agent task distribution with priority scheduling
        %% Strategy: Priority queue (high → normal → low)
        %% Persistence: ETS with periodic DETS flush
        %% ================================================================
        #{id => erlmcp_flow_task_queue,
          start => {erlmcp_flow_task_queue, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_task_queue]},

        %% ================================================================
        %% LOAD BALANCER: Agent selection (round_robin, least_conn, weighted)
        %% Strategies:
        %%   - round_robin: O(1) selection
        %%   - least_conn: O(N) search for min connections
        %%   - weighted: O(log N) binary search by weight
        %% ================================================================
        #{id => erlmcp_flow_load_balancer,
          start => {erlmcp_flow_load_balancer, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_load_balancer]},

        %% ================================================================
        %% OBSERVER SUPERVISOR: Isolated observability (one_for_one)
        %% Critical: Failures do NOT affect agent/swarm operations
        %% ================================================================
        #{id => erlmcp_flow_observer_sup,
          start => {erlmcp_flow_observer_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor,
          modules => [erlmcp_flow_observer_sup]}
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

#### Swarm Supervisor (simple_one_for_one)

```erlang
-module(erlmcp_flow_swarm_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,  % Dynamic swarm instances
        intensity => 5,
        period => 60
    },

    %% Template child spec for swarm coordinator instances
    ChildSpecs = [
        #{id => erlmcp_flow_swarm,
          start => {erlmcp_flow_swarm, start_link, []},  % Args appended by start_child
          restart => transient,  % Restart on abnormal exit (not normal shutdown)
          shutdown => 5000,       % 5s graceful shutdown (flush pending messages)
          type => worker,
          modules => [erlmcp_flow_swarm]}
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% API: Start a swarm with topology configuration
start_child(SwarmId, Topology, Config) ->
    supervisor:start_child(?MODULE, [SwarmId, Topology, Config]).
```

**Swarm Restart Strategy**: `transient`
- **Normal Exit** (shutdown): Do NOT restart (swarm completed task)
- **Abnormal Exit** (crash): Restart (preserve swarm state)
- **Justification**: Swarms are task-bound; crash indicates incomplete work

#### Agent Supervisor (simple_one_for_one)

```erlang
-module(erlmcp_flow_agent_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,  % Dynamic agent instances
        intensity => 10,                  % Agents may crash more frequently (exploratory tasks)
        period => 60
    },

    %% Template child spec for agent instances
    ChildSpecs = [
        #{id => erlmcp_flow_agent,
          start => {erlmcp_flow_agent, start_link, []},  % Args: [AgentId, Role, Config]
          restart => temporary,  % Do NOT restart on exit (let parent swarm decide)
          shutdown => 2000,       % 2s graceful shutdown (cancel pending operations)
          type => worker,
          modules => [erlmcp_flow_agent]}
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% API: Start an agent with role and configuration
start_child(AgentId, Role, Config) ->
    supervisor:start_child(?MODULE, [AgentId, Role, Config]).
```

**Agent Restart Strategy**: `temporary`
- **Normal Exit**: Do NOT restart (agent completed task)
- **Abnormal Exit** (crash): Do NOT restart (let swarm requeue task)
- **Justification**: Agents are ephemeral; crashes trigger task requeue by parent swarm

---

### TIER 3: Isolated Workers

#### Agent Worker (gen_server)

**States**: `active | paused | failed`
**Lifecycle**: spawn → init → active → task_execution → shutdown
**Crash Handling**: Swarm requeues task to different agent

```erlang
-module(erlmcp_flow_agent).
-behaviour(gen_server).

-record(state, {
    id :: binary(),              % UUID
    role :: worker | specialist | scout,
    status :: active | paused | failed,
    pending_requests :: maps:map(),  % request_id => {timestamp, request}
    metrics :: metrics_state(),
    config :: agent_config()
}).

%% API
start_link(AgentId, Role, Config) ->
    gen_server:start_link(?MODULE, {AgentId, Role, Config}, []).

%% gen_server callbacks
init({AgentId, Role, Config}) ->
    %% Register with gproc
    gproc:reg({n, l, {agent, Role, AgentId}}),

    State = #state{
        id = AgentId,
        role = Role,
        status = active,
        pending_requests = #{},
        metrics = init_metrics(),
        config = Config
    },

    {ok, State}.

handle_call({execute_task, Task}, From, State) ->
    %% Execute task and track request
    RequestId = uuid(),
    NewState = State#state{
        pending_requests = maps:put(RequestId, {erlang:monotonic_time(), Task}, State#state.pending_requests)
    },

    %% Execute asynchronously (avoid blocking init)
    spawn_link(fun() ->
        Result = execute_task_impl(Task, State),
        gen_server:reply(From, Result)
    end),

    {noreply, NewState};

handle_call({pause, Reason}, _From, State) ->
    %% Byzantine failure detected by swarm → pause agent
    NewState = State#state{status = paused},
    {reply, ok, NewState};

handle_call(resume, _From, State = #state{status = paused}) ->
    %% Consensus restored → resume agent
    NewState = State#state{status = active},
    {reply, ok, NewState}.

handle_info({'EXIT', _Pid, Reason}, State) ->
    %% Task worker crashed → mark as failed, let swarm requeue
    logger:warning("Agent task worker crashed: ~p", [Reason]),
    NewState = State#state{status = failed},
    {noreply, NewState}.

terminate(_Reason, State) ->
    %% Cleanup: unregister from gproc, cancel pending requests
    gproc:unreg({n, l, {agent, State#state.role, State#state.id}}),
    ok.
```

**Restart Policy**: `temporary` (no automatic restart)
- Agent crashes trigger task requeue by parent swarm
- Swarm coordinator decides whether to spawn new agent or retry on different agent

#### Swarm Coordinator (gen_server)

**Topology**: `mesh | hierarchical | ring | star`
**Consensus**: Integrated with Raft/Byzantine for leader election
**Crash Handling**: Consensus layer triggers leader re-election

```erlang
-module(erlmcp_flow_swarm).
-behaviour(gen_server).

-record(state, {
    id :: binary(),
    topology :: mesh | hierarchical | ring | star,
    agents :: [pid()],           % Connected agent PIDs
    leader :: pid() | undefined, % Current Raft leader
    task_queue :: queue:queue(),
    consensus_state :: consensus_state(),
    config :: swarm_config()
}).

init({SwarmId, Topology, Config}) ->
    %% Register with gproc
    gproc:reg({n, l, {swarm, SwarmId}}),

    %% Subscribe to consensus events
    erlmcp_flow_raft:subscribe(),

    State = #state{
        id = SwarmId,
        topology = Topology,
        agents = [],
        leader = undefined,
        task_queue = queue:new(),
        consensus_state = init_consensus(),
        config = Config
    },

    {ok, State}.

handle_info({consensus_event, leader_elected, LeaderPid}, State) ->
    %% Raft elected new leader → resume task distribution
    logger:info("New leader elected: ~p", [LeaderPid]),
    NewState = State#state{leader = LeaderPid},
    {noreply, resume_task_distribution(NewState)};

handle_info({consensus_event, leader_lost, _OldLeader}, State) ->
    %% Leader lost → pause task distribution until new leader elected
    logger:warning("Leader lost, pausing task distribution"),
    NewState = State#state{leader = undefined},
    {noreply, pause_agents(NewState)};

handle_info({consensus_event, network_partition, Nodes}, State) ->
    %% Network partition detected → wait for quorum restoration
    logger:warning("Network partition detected: ~p", [Nodes]),
    {noreply, wait_for_quorum(State)};

handle_info({agent_crash, AgentPid, Task}, State) ->
    %% Agent crashed → requeue task to different agent
    logger:warning("Agent ~p crashed, requeuing task", [AgentPid]),
    NewState = requeue_task(Task, State),
    {noreply, NewState}.

%% Pause agents on consensus failure
pause_agents(State) ->
    [gen_server:call(AgentPid, {pause, consensus_lost}) || AgentPid <- State#state.agents],
    State.

%% Resume agents after consensus restoration
resume_task_distribution(State) ->
    [gen_server:call(AgentPid, resume) || AgentPid <- State#state.agents],
    distribute_tasks(State).
```

**Restart Policy**: `transient` (restart on abnormal exit)
- Swarm coordinator crashes trigger state recovery from Raft log
- Agent references restored from registry (gproc)

---

## Failure Scenarios and Recovery Strategies

### Scenario 1: Agent Crash

**Trigger**: Agent gen_server crashes (e.g., OOM, unhandled exception)
**Detection**: Parent swarm receives `{'DOWN', MonitorRef, process, Pid, Reason}`
**Action**:
1. Swarm coordinator receives agent crash notification
2. Task requeued to task queue with retry count
3. Load balancer selects different agent for task
4. New agent spawned if pool below minimum threshold
5. Original agent NOT restarted (temporary restart strategy)

**Recovery Time**: <50ms (task requeue + agent selection)

```erlang
handle_info({'DOWN', _MonitorRef, process, AgentPid, Reason}, State) ->
    %% Find task associated with crashed agent
    Task = find_pending_task(AgentPid, State),

    %% Requeue task with incremented retry count
    NewState = requeue_task(Task, State),

    %% Remove agent from pool
    NewAgents = lists:delete(AgentPid, State#state.agents),

    %% Optionally spawn replacement agent
    NewState2 = case length(NewAgents) < min_pool_size(State) of
        true ->
            {ok, NewAgent} = spawn_agent(State#state.config),
            NewState#state{agents = [NewAgent | NewAgents]};
        false ->
            NewState#state{agents = NewAgents}
    end,

    {noreply, NewState2}.
```

### Scenario 2: Swarm Coordinator Crash

**Trigger**: Swarm gen_server crashes (e.g., state inconsistency)
**Detection**: Parent supervisor (erlmcp_flow_swarm_sup) detects exit
**Action**:
1. Supervisor restarts swarm coordinator (transient restart strategy)
2. New coordinator process recovers state from:
   - Raft log (committed task history)
   - Agent registry (gproc lookup of active agents)
   - Task queue (ETS-backed persistent queue)
3. Resume task distribution after state recovery

**Recovery Time**: ~200ms (supervisor restart + state recovery)

```erlang
%% Swarm state recovery after crash
init({SwarmId, Topology, Config}) ->
    %% Recover from Raft log
    {ok, CommittedState} = erlmcp_flow_raft:read_latest_commit(SwarmId),

    %% Recover agent list from registry
    AgentPids = gproc:lookup_values({n, l, {agent, '_', SwarmId}}),

    %% Recover task queue from ETS
    TaskQueue = erlmcp_flow_task_queue:get_queue(SwarmId),

    State = #state{
        id = SwarmId,
        topology = Topology,
        agents = AgentPids,
        task_queue = TaskQueue,
        consensus_state = CommittedState,
        config = Config
    },

    {ok, State}.
```

### Scenario 3: Byzantine Failure (Consensus)

**Trigger**: Byzantine node sends conflicting messages (malicious or buggy)
**Detection**: PBFT consensus detects conflicting pre-prepare messages
**Action**:
1. Consensus layer detects Byzantine behavior (2f+1 replicas disagree)
2. Trigger view change protocol
3. All swarms pause task distribution (`pause_agents/1`)
4. New primary elected (view + 1 mod N)
5. Swarms resume after consensus stabilization

**Recovery Time**: ~500ms (PBFT view change timeout)

```erlang
handle_info({consensus_event, byzantine_detected, Node}, State) ->
    logger:error("Byzantine failure detected on node ~p", [Node]),

    %% Pause all agents to prevent inconsistent state
    NewState = pause_agents(State),

    %% Trigger view change in consensus layer
    erlmcp_flow_byzantine:trigger_view_change(Node),

    %% Set timeout for view change completion
    erlang:send_after(500, self(), {check_consensus_restored}),

    {noreply, NewState#state{consensus_state = view_changing}};

handle_info({check_consensus_restored}, State) ->
    case erlmcp_flow_byzantine:is_stable() of
        true ->
            logger:info("Consensus restored after Byzantine failure"),
            NewState = resume_task_distribution(State),
            {noreply, NewState};
        false ->
            %% Retry after another 500ms
            erlang:send_after(500, self(), {check_consensus_restored}),
            {noreply, State}
    end.
```

### Scenario 4: Network Partition

**Trigger**: Network link failure between consensus nodes
**Detection**: Gossip protocol detects missing heartbeats
**Action**:
1. Gossip detects partition via missing heartbeats (3 consecutive rounds)
2. Raft/PBFT unable to achieve quorum → operations pause
3. All swarms wait for quorum restoration (`wait_for_quorum/1`)
4. Partition heals → leader re-election → swarms resume

**Recovery Time**: ~1-3s (gossip detection + leader election)

```erlang
handle_info({consensus_event, network_partition, Nodes}, State) ->
    logger:warning("Network partition detected: ~p", [Nodes]),

    %% Check if quorum is still achievable
    QuorumAvailable = erlmcp_flow_raft:check_quorum(),

    case QuorumAvailable of
        true ->
            %% This partition has quorum → continue operations
            logger:info("Quorum maintained in this partition"),
            {noreply, State};
        false ->
            %% This partition lacks quorum → pause operations
            logger:warning("Quorum lost, pausing operations"),
            NewState = pause_agents(State),

            %% Poll for quorum restoration every 1s
            erlang:send_after(1000, self(), {check_quorum_restored}),
            {noreply, NewState#state{consensus_state = waiting_quorum}}
    end;

handle_info({check_quorum_restored}, State) ->
    case erlmcp_flow_raft:check_quorum() of
        true ->
            logger:info("Quorum restored, resuming operations"),
            NewState = resume_task_distribution(State),
            {noreply, NewState};
        false ->
            %% Retry after another 1s
            erlang:send_after(1000, self(), {check_quorum_restored}),
            {noreply, State}
    end.
```

### Scenario 5: Cascading Failure Prevention

**Trigger**: Multiple agent crashes in rapid succession
**Detection**: Supervisor intensity/period threshold exceeded
**Action**:
1. `erlmcp_flow_agent_sup` detects intensity=10 restarts within period=60s
2. Supervisor terminates itself (prevents cascade)
3. Parent `erlmcp_flow_core_sup` receives exit signal
4. Core supervisor restarts agent supervisor with clean state
5. Circuit breaker prevents further task distribution for 60s

**Recovery Time**: ~1s (supervisor restart + circuit breaker cooldown)

```erlang
%% Circuit breaker in swarm coordinator
handle_info({agent_crash, _AgentPid, _Task}, State = #state{consecutive_crashes = N})
    when N >= 5 ->
    logger:error("Circuit breaker triggered: ~p consecutive agent crashes", [N]),

    %% Open circuit breaker
    NewState = State#state{
        circuit_breaker = open,
        consecutive_crashes = 0
    },

    %% Schedule circuit breaker reset after 60s
    erlang:send_after(60000, self(), reset_circuit_breaker),

    {noreply, NewState};

handle_info(reset_circuit_breaker, State) ->
    logger:info("Circuit breaker reset, resuming task distribution"),
    NewState = State#state{circuit_breaker = closed},
    {noreply, resume_task_distribution(NewState)}.
```

---

## Resilience Limits and Cascade Guards

### Supervisor Intensity Limits

| Supervisor | Intensity | Period | Rationale |
|-----------|-----------|--------|-----------|
| **erlmcp_flow_sup** | 3 | 60s | Conservative for consensus (prevents split-brain) |
| **erlmcp_flow_consensus_sup** | 5 | 60s | Standard for gen_server workers |
| **erlmcp_flow_core_sup** | 5 | 60s | Standard for independent subsystems |
| **erlmcp_flow_swarm_sup** | 5 | 60s | Standard for dynamic swarms |
| **erlmcp_flow_agent_sup** | 10 | 60s | Tolerant for exploratory agent tasks |
| **erlmcp_flow_observer_sup** | 10 | 60s | Tolerant for non-critical observability |

### Circuit Breaker Thresholds

```erlang
-record(circuit_breaker, {
    state :: closed | open | half_open,
    failure_count :: non_neg_integer(),
    threshold :: non_neg_integer(),      % Default: 5 failures
    timeout :: non_neg_integer(),        % Default: 60s
    last_failure :: non_neg_integer()    % Timestamp
}).

%% Circuit breaker logic
check_circuit_breaker(State) ->
    CB = State#state.circuit_breaker,
    Now = erlang:monotonic_time(second),

    case CB#circuit_breaker.state of
        open ->
            %% Check if timeout expired
            ElapsedTime = Now - CB#circuit_breaker.last_failure,
            if ElapsedTime >= CB#circuit_breaker.timeout ->
                %% Transition to half-open (allow 1 request)
                {half_open, CB#circuit_breaker{state = half_open}};
            true ->
                %% Still open, reject request
                {open, CB}
            end;

        half_open ->
            %% Allow 1 request to test circuit
            {half_open, CB};

        closed ->
            %% Normal operation
            {closed, CB}
    end.
```

### Backpressure Mechanisms

**Task Queue Limits**:
- Max queue size: 10,000 tasks per swarm
- On overflow: Reject new tasks with `{error, queue_full}`
- Priority scheduling: High → Normal → Low

**Agent Pool Limits**:
- Min pool size: 5 agents per role
- Max pool size: 100 agents per role
- Idle timeout: 30s (agents with no tasks terminate)

**Connection Limits**:
- Max connections per node: 10,000
- On overflow: Circuit breaker opens, reject new connections

---

## Child Spec Summary

### TIER 1: Application Supervisor

```erlang
%% erlmcp_flow_sup (one_for_all)
#{strategy => one_for_all, intensity => 3, period => 60}

Children:
  1. erlmcp_flow_registry       (permanent, 5000ms, worker)
  2. erlmcp_flow_consensus_sup  (permanent, infinity, supervisor)
  3. erlmcp_flow_core_sup       (permanent, infinity, supervisor)
```

### TIER 2: Consensus Supervisor

```erlang
%% erlmcp_flow_consensus_sup (one_for_one)
#{strategy => one_for_one, intensity => 5, period => 60}

Children:
  1. erlmcp_flow_raft           (permanent, 5000ms, worker)
  2. erlmcp_flow_byzantine      (permanent, 5000ms, worker)
  3. erlmcp_flow_gossip         (permanent, 5000ms, worker)
  4. erlmcp_flow_election       (permanent, 5000ms, worker)
  5. erlmcp_flow_log_store      (permanent, 10000ms, worker)
```

### TIER 2: Core Supervisor

```erlang
%% erlmcp_flow_core_sup (one_for_one)
#{strategy => one_for_one, intensity => 5, period => 60}

Children:
  1. erlmcp_flow_swarm_sup          (permanent, infinity, supervisor)
  2. erlmcp_flow_agent_sup          (permanent, infinity, supervisor)
  3. erlmcp_flow_request_tracker    (permanent, 5000ms, worker)
  4. erlmcp_flow_task_queue         (permanent, 5000ms, worker)
  5. erlmcp_flow_load_balancer      (permanent, 5000ms, worker)
  6. erlmcp_flow_observer_sup       (permanent, infinity, supervisor)
```

### TIER 3: Dynamic Workers

```erlang
%% erlmcp_flow_swarm_sup (simple_one_for_one)
#{strategy => simple_one_for_one, intensity => 5, period => 60}

Template:
  erlmcp_flow_swarm (transient, 5000ms, worker)

%% erlmcp_flow_agent_sup (simple_one_for_one)
#{strategy => simple_one_for_one, intensity => 10, period => 60}

Template:
  erlmcp_flow_agent (temporary, 2000ms, worker)
```

---

## Consensus-Aware Restart Strategies

### Raft Leader Election

**Trigger**: Current leader crashes or becomes unresponsive
**Action**:
1. Followers detect leader timeout (150-300ms randomized)
2. Follower transitions to candidate, increments term
3. Candidate requests votes from peers (RequestVote RPC)
4. Quorum (N+1)/2 votes required to become leader
5. New leader broadcasts heartbeat (AppendEntries with empty log)

**Swarm Behavior**:
- **During Election**: Pause task distribution, queue new tasks
- **After Election**: Resume task distribution with new leader
- **Timeout**: 300ms max (randomized election timeout prevents split votes)

### Byzantine View Change

**Trigger**: PBFT primary suspected of Byzantine behavior
**Action**:
1. Backup detects conflicting pre-prepare messages
2. Backup broadcasts VIEW-CHANGE to all replicas
3. 2f+1 replicas agree on view change
4. New primary (view + 1 mod N) broadcasts NEW-VIEW
5. Consensus stabilizes after 3-phase commit

**Swarm Behavior**:
- **During View Change**: Pause task distribution, block agent operations
- **After Stabilization**: Resume task distribution with new primary
- **Timeout**: 500ms per phase (1.5s total for view change)

### Network Partition Healing

**Trigger**: Gossip protocol detects partition via missing heartbeats
**Action**:
1. Partition detected after 3 consecutive gossip rounds (3s)
2. Each partition elects leader independently (split-brain)
3. Partition heals, gossip propagates state
4. Raft/PBFT detect conflicting logs
5. Conflict resolution via term/view number comparison

**Swarm Behavior**:
- **During Partition**: Majority partition continues, minority pauses
- **After Healing**: Minority partition syncs from majority leader
- **Timeout**: 1-3s for gossip detection + leader election

---

## Integration with erlmcp Core

### Registry Integration (gproc)

```erlang
%% Agent registration
-spec register_agent(AgentId, Role, Pid) -> ok | {error, Reason}.
register_agent(AgentId, Role, Pid) ->
    gproc:reg({n, l, {agent, Role, AgentId}}, Pid).

%% Swarm registration
-spec register_swarm(SwarmId, Pid) -> ok | {error, Reason}.
register_swarm(SwarmId, Pid) ->
    gproc:reg({n, l, {swarm, SwarmId}}, Pid).

%% Agent lookup (O(log N) via ETS)
-spec find_agents_by_role(Role) -> [pid()].
find_agents_by_role(Role) ->
    gproc:lookup_values({n, l, {agent, Role, '_'}}).
```

### Transport Integration

erlmcp-flow agents can handle transport requests via existing transport layer:

```erlang
%% Handle stdio request from Claude Code
handle_info({transport_data, TransportId, Data}, State) ->
    %% Decode JSON-RPC request
    Request = erlmcp_json_rpc:decode(Data),

    %% Route to appropriate agent via load balancer
    {ok, AgentPid} = erlmcp_flow_load_balancer:select_agent(Request),

    %% Forward request to agent
    gen_server:cast(AgentPid, {execute_task, Request}),

    {noreply, State}.
```

### Observability Integration

erlmcp-flow inherits observability from `erlmcp_observability_sup`:

```erlang
%% OTEL tracing for agent tasks
handle_call({execute_task, Task}, From, State) ->
    %% Start OTEL span
    SpanCtx = erlmcp_otel:start_span(
        <<"agent.task.execute">>,
        #{agent_id => State#state.id, task_id => Task#task.id}
    ),

    Result = execute_task_impl(Task),

    %% End OTEL span
    erlmcp_otel:end_span(SpanCtx),

    {reply, Result, State}.
```

---

## Testing Strategy

### Unit Tests (EUnit)

```erlang
%% Test: Agent crash triggers task requeue
agent_crash_requeue_test() ->
    %% Setup
    {ok, SwarmPid} = erlmcp_flow_swarm:start_link(<<"swarm-1">>, mesh, #{}),
    {ok, AgentPid} = erlmcp_flow_agent:start_link(<<"agent-1">>, worker, #{}),

    %% Assign task to agent
    Task = #{id => <<"task-1">>, action => <<"test">>},
    erlmcp_flow_swarm:assign_task(SwarmPid, AgentPid, Task),

    %% Kill agent
    exit(AgentPid, kill),

    %% Verify task requeued
    timer:sleep(100),
    TaskQueue = erlmcp_flow_swarm:get_task_queue(SwarmPid),
    ?assertMatch([#{id := <<"task-1">>}], TaskQueue).

%% Test: Byzantine failure triggers swarm pause
byzantine_failure_pause_test() ->
    {ok, SwarmPid} = erlmcp_flow_swarm:start_link(<<"swarm-1">>, mesh, #{}),

    %% Inject Byzantine failure
    erlmcp_flow_byzantine:inject_failure(byzantine_node),

    %% Verify swarm paused
    timer:sleep(100),
    State = erlmcp_flow_swarm:get_state(SwarmPid),
    ?assertEqual(paused, State#state.status).
```

### Chaos Engineering Tests

```erlang
%% Test: Kill leader → new leader elected within 300ms
raft_leader_election_test() ->
    %% Setup 5-node cluster
    Nodes = [spawn_raft_node(I) || I <- lists:seq(1, 5)],

    %% Wait for leader election
    timer:sleep(500),
    Leader = erlmcp_flow_raft:get_leader(),

    %% Kill leader
    exit(Leader, kill),

    %% Measure election time
    Start = erlang:monotonic_time(millisecond),
    erlmcp_flow_raft:wait_for_leader(),
    ElectionTime = erlang:monotonic_time(millisecond) - Start,

    %% Verify election within 300ms
    ?assert(ElectionTime < 300).
```

---

## Summary: Architecture Invariants

| Invariant | Definition |
|-----------|------------|
| **Process-per-Agent** | ∀agent ∈ Agents. ∃!gen_server. handles(agent) |
| **Registry Routing** | gproc : Name × Pid → Route. O(log N) |
| **Consensus Quorum** | ∀commit ∈ Log. votes(commit) ≥ (N+1)/2 |
| **Failure Isolation** | ∀crash(agent). ¬cascade(swarm) |
| **Byzantine Tolerance** | ∀failures ≤ f. ∃!primary. 2f+1 replicas agree |
| **Task Requeue** | ∀crash(agent). ∃task. requeue(task, swarm) |
| **Circuit Breaker** | ∀failures ≥ 5. pause(swarm, 60s) |
| **Observability Isolation** | ∀crash(observer). ¬affect(agents) |

---

## Next Steps: Implementation Plan

1. **Phase 1**: Implement TIER 1 (erlmcp_flow_sup + registry + consensus_sup) - **Week 1**
2. **Phase 2**: Implement TIER 2 (swarm_sup + agent_sup + core_sup) - **Week 2**
3. **Phase 3**: Implement TIER 3 (agent gen_server + swarm coordinator) - **Week 3**
4. **Phase 4**: Implement consensus protocols (Raft + Byzantine + Gossip) - **Week 4**
5. **Phase 5**: Testing + chaos engineering + documentation - **Week 5**

**Estimated Timeline**: 5 weeks (23 days)
**Module Count**: ~60 modules
**Test Count**: ~250 EUnit + 85 Common Test
**Coverage Target**: ≥80%

---

**Document Version**: 1.0.0
**Author**: Erlang Architect Agent
**Date**: 2026-02-02
**Status**: Architecture Design Complete → Ready for Implementation
