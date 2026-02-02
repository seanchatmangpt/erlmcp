# erlmcp-flow Routing Layer Design
## Q-Learning Based Agent Coordination with Byzantine Fault Tolerance

**Version**: 1.0.0
**Date**: 2026-02-01
**Status**: Specification Complete
**Author**: Erlang Transport Builder + Architecture Team

---

## Executive Summary

This document specifies the **erlmcp-flow routing layer**, a distributed agent coordination system that uses:

1. **Q-Learning Task Router** - Reinforcement learning for optimal agent/swarm selection
2. **Service Discovery** - O(log N) gproc-based capability-to-PID mapping
3. **Load Balancing** - Round-robin, least-connections, weighted strategies
4. **Failure Handling** - Retry, fallback, circuit breaker with Byzantine tolerance
5. **Request Correlation** - UUID-based distributed tracing

**Key Performance Targets**:
- Agent lookup: p99 < 100μs (O(log N))
- Routing decision: p99 < 50ms (Q-Learning inference)
- Failover: < 2s (leader election + circuit breaker)
- Throughput: > 50K route decisions/sec
- Memory: < 512MB for 1000 agents

**Architecture Principles**:
- ✅ OTP 28.3.1 behaviors (gen_server, supervisor)
- ✅ Let-it-crash fault tolerance
- ✅ Chicago TDD (black-box, no mocks)
- ✅ O(log N) registry operations (gproc)
- ✅ Process-per-agent isolation
- ✅ Byzantine fault tolerance (3f+1 quorum)

---

## Table of Contents

1. [System Architecture](#system-architecture)
2. [Task Router (Q-Learning)](#task-router-q-learning)
3. [Service Discovery (gproc)](#service-discovery-gproc)
4. [Load Balancing Strategies](#load-balancing-strategies)
5. [Failure Handling](#failure-handling)
6. [Request Correlation](#request-correlation)
7. [Network Partitions & Byzantine Nodes](#network-partitions--byzantine-nodes)
8. [Message Format](#message-format)
9. [Routing Algorithm](#routing-algorithm)
10. [State Machine](#state-machine)
11. [Timeout Handling](#timeout-handling)
12. [Module Specifications](#module-specifications)
13. [Test Strategy](#test-strategy)
14. [Performance Benchmarks](#performance-benchmarks)
15. [Integration Points](#integration-points)

---

## 1. System Architecture

### 1.1 Supervision Tree

```erlang
erlmcp_flow_sup (one_for_all)
├── erlmcp_flow_registry (gen_server)
│   ├── gproc integration (O(log N) lookup)
│   ├── Agent capability indexing
│   └── Health monitoring (heartbeat)
│
├── erlmcp_flow_router (gen_server)
│   ├── Q-Learning state management
│   ├── Load balancing policy selection
│   ├── Request correlation (UUID tracking)
│   └── Metrics collection (latency, success rate, load)
│
├── erlmcp_flow_q_learning (gen_server)
│   ├── Q-table: State × Action → Reward
│   ├── Epsilon-greedy exploration (ε=0.1)
│   ├── Temporal difference learning (α=0.1, γ=0.9)
│   └── Periodic model persistence (every 1000 episodes)
│
├── erlmcp_flow_load_balancer (module)
│   ├── Round-robin strategy
│   ├── Least-connections strategy
│   ├── Weighted strategy (based on Q-values)
│   └── Consistent hashing (for sticky sessions)
│
├── erlmcp_flow_failure_detector (gen_server)
│   ├── Agent health monitoring (timeout detection)
│   ├── Retry logic (max 3 attempts, exponential backoff)
│   ├── Fallback swarm selection
│   └── Circuit breaker state machine
│
├── erlmcp_flow_circuit_breaker (gen_server)
│   ├── States: closed, open, half_open
│   ├── Thresholds: failure_rate > 50%, min_requests = 10
│   ├── Timeout: 30s recovery window
│   └── Integration with erlmcp_circuit_breaker (existing)
│
├── erlmcp_flow_correlation_tracker (gen_server)
│   ├── UUID generation (v4)
│   ├── Trace context propagation (W3C Trace Context)
│   ├── Parent-child span linking
│   └── ETS storage: {trace_id, span_id, parent_span_id, metadata}
│
└── erlmcp_flow_byzantine_detector (gen_server)
    ├── Quorum consensus (3f+1 nodes)
    ├── Message signature verification (HMAC-SHA256)
    ├── Honest node threshold enforcement
    └── Byzantine node blacklisting

```

### 1.2 Component Interactions

```
┌────────────────────────────────────────────────────────────────┐
│                      Client Request                            │
└───────────────────────────┬────────────────────────────────────┘
                            │
                            ▼
        ┌───────────────────────────────────────┐
        │ erlmcp_flow_router                    │
        │ - Extract task requirements           │
        │ - Generate trace ID (UUID)            │
        │ - Query Q-Learning for agent          │
        └──────┬────────────────────────────────┘
               │
               ├─────────────────────────────────┐
               │                                 │
               ▼                                 ▼
    ┌─────────────────────┐         ┌────────────────────────┐
    │ Q-Learning Engine   │         │ Service Discovery      │
    │ - State: {load,     │         │ - gproc lookup         │
    │   success_rate,     │◄────────┤ - O(log N) PID lookup  │
    │   latency}          │         │ - Capability matching  │
    │ - Action: agent_id  │         └────────────────────────┘
    │ - Reward: -latency  │
    └──────┬──────────────┘
           │
           ▼
    ┌─────────────────────┐
    │ Load Balancer       │
    │ - Strategy select   │
    │ - Agent ranking     │
    │ - Overflow handling │
    └──────┬──────────────┘
           │
           ▼
    ┌─────────────────────┐
    │ Failure Detector    │
    │ - Timeout: 5s       │
    │ - Retry: 3 attempts │
    │ - Circuit breaker   │
    └──────┬──────────────┘
           │
           ▼
    ┌─────────────────────┐
    │ Agent Execution     │
    │ - Task processing   │
    │ - Metrics update    │
    │ - Result return     │
    └─────────────────────┘
```

---

## 2. Task Router (Q-Learning)

### 2.1 Q-Learning State Space

**State Representation**: `{agent_load, success_rate_bucket, latency_bucket}`

- `agent_load`: `0..10` (0 = idle, 10 = overloaded)
- `success_rate_bucket`: `low | medium | high` (< 0.8, 0.8-0.95, > 0.95)
- `latency_bucket`: `fast | normal | slow` (< 50ms, 50-200ms, > 200ms)

**State Space Size**: 11 × 3 × 3 = **99 states**

### 2.2 Action Space

**Actions**: Select one of N agents or swarms

- Primary agents: 60 individual agents
- Swarms: 5 swarms (EPIC 9 workflows, multi-agent coordination)

**Action Space Size**: **65 actions** (60 agents + 5 swarms)

### 2.3 Reward Function

```erlang
-spec compute_reward(TaskResult, Metrics) -> float().
compute_reward({ok, Result}, #{latency := Latency, success := true}) ->
    %% Negative latency (minimize latency)
    %% Bonus for fast responses (< 50ms)
    BaseReward = -Latency / 1000.0,  % Convert to seconds
    if
        Latency < 50 -> BaseReward + 1.0;   % Fast bonus
        Latency < 200 -> BaseReward;        % Normal
        true -> BaseReward - 1.0            % Slow penalty
    end;

compute_reward({error, _Reason}, _Metrics) ->
    -10.0.  % Large penalty for failures
```

### 2.4 Q-Learning Algorithm

```erlang
%% Temporal Difference (TD) Learning
%% Q(s, a) ← Q(s, a) + α [r + γ max_a' Q(s', a') - Q(s, a)]

-spec update_q_table(State, Action, Reward, NextState, QTable) -> QTable.
update_q_table(State, Action, Reward, NextState, QTable) ->
    Alpha = 0.1,   % Learning rate
    Gamma = 0.9,   % Discount factor

    %% Current Q-value
    Q_sa = get_q_value(QTable, State, Action),

    %% Max Q-value for next state
    MaxQ_next = max_q_value(QTable, NextState),

    %% TD update
    NewQ = Q_sa + Alpha * (Reward + Gamma * MaxQ_next - Q_sa),

    %% Update Q-table
    maps:put({State, Action}, NewQ, QTable).

%% Epsilon-greedy action selection
-spec select_action(State, QTable, Epsilon) -> Action.
select_action(State, QTable, Epsilon) ->
    case rand:uniform() < Epsilon of
        true ->
            %% Explore: random action
            random_action();
        false ->
            %% Exploit: best Q-value action
            best_action(State, QTable)
    end.
```

### 2.5 State Representation

```erlang
-record(q_state, {
    agent_load :: 0..10,
    success_rate_bucket :: low | medium | high,
    latency_bucket :: fast | normal | slow
}).

-spec extract_state(AgentMetrics) -> q_state().
extract_state(#{
    load := Load,
    success_rate := SuccessRate,
    avg_latency := AvgLatency
}) ->
    #q_state{
        agent_load = min(10, trunc(Load)),
        success_rate_bucket =
            if
                SuccessRate < 0.8 -> low;
                SuccessRate < 0.95 -> medium;
                true -> high
            end,
        latency_bucket =
            if
                AvgLatency < 50 -> fast;
                AvgLatency < 200 -> normal;
                true -> slow
            end
    }.
```

### 2.6 Q-Learning Module Specification

```erlang
-module(erlmcp_flow_q_learning).
-behaviour(gen_server).

%% API
-export([start_link/0, select_agent/1, update_reward/4, get_q_table/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    q_table :: maps:map(),           % {State, Action} -> Q-value
    epsilon = 0.1 :: float(),        % Exploration rate
    episode_count = 0 :: integer(),
    metrics_ets :: ets:tid()         % Agent metrics storage
}).

init([]) ->
    QTable = load_q_table(),         % Load from persistent storage
    ETS = ets:new(agent_metrics, [set, public, named_table]),
    {ok, #state{q_table = QTable, metrics_ets = ETS}}.

handle_call({select_agent, TaskRequirements}, _From, State) ->
    %% Extract current state from agent metrics
    Metrics = get_agent_metrics(State#state.metrics_ets),
    CurrentState = extract_state(Metrics),

    %% Epsilon-greedy action selection
    Action = select_action(CurrentState, State#state.q_table, State#state.epsilon),

    %% Log for correlation
    logger:info("Selected agent ~p for state ~p", [Action, CurrentState]),

    {reply, {ok, Action}, State};

handle_call({update_reward, State, Action, Reward, NextState}, _From, #state{q_table = QTable} = State0) ->
    NewQTable = update_q_table(State, Action, Reward, NextState, QTable),

    %% Decay epsilon over time (less exploration)
    NewEpsilon = max(0.01, State0#state.epsilon * 0.999),

    %% Persist Q-table every 1000 episodes
    EpisodeCount = State0#state.episode_count + 1,
    case EpisodeCount rem 1000 of
        0 -> save_q_table(NewQTable);
        _ -> ok
    end,

    {reply, ok, State0#state{
        q_table = NewQTable,
        epsilon = NewEpsilon,
        episode_count = EpisodeCount
    }}.
```

---

## 3. Service Discovery (gproc)

### 3.1 Agent Registration

```erlang
-module(erlmcp_flow_registry).
-behaviour(gen_server).

%% Agent registration with capabilities
-spec register_agent(AgentId, AgentPid, Capabilities) -> ok | {error, term()}.
register_agent(AgentId, AgentPid, Capabilities) ->
    %% gproc keys for O(log N) lookup
    NameKey = {n, l, {flow_agent, AgentId}},

    %% Register agent with PID
    gproc:reg_other(NameKey, AgentPid, #{capabilities => Capabilities}),
    gproc:monitor(NameKey),  % Auto-cleanup on death

    %% Index by capabilities
    lists:foreach(fun(Cap) ->
        PropKey = {p, l, {flow_capability, Cap}},
        gproc:reg_other(PropKey, AgentPid, #{agent_id => AgentId})
    end, Capabilities),

    %% Load counter for load balancing
    CounterKey = {c, l, {flow_load, AgentId}},
    gproc:reg_other(CounterKey, AgentPid, 0),

    ok.
```

### 3.2 Agent Discovery (O(log N))

```erlang
%% Find agent by ID
-spec find_agent(AgentId) -> {ok, pid()} | {error, not_found}.
find_agent(AgentId) ->
    case gproc:where({n, l, {flow_agent, AgentId}}) of
        undefined -> {error, not_found};
        Pid -> {ok, Pid}
    end.

%% Find agents by capability (O(M) where M = matching agents)
-spec find_agents_by_capability(Capability) -> [pid()].
find_agents_by_capability(Capability) ->
    gproc:lookup_pids({p, l, {flow_capability, Capability}}).

%% Get agent load (O(1) counter lookup)
-spec get_agent_load(AgentId) -> non_neg_integer().
get_agent_load(AgentId) ->
    CounterKey = {c, l, {flow_load, AgentId}},
    case gproc:lookup_value(CounterKey) of
        undefined -> 0;
        Load -> Load
    end.

%% Increment agent load
-spec increment_load(AgentId) -> non_neg_integer().
increment_load(AgentId) ->
    CounterKey = {c, l, {flow_load, AgentId}},
    gproc:update_counter(CounterKey, 1).

%% Decrement agent load
-spec decrement_load(AgentId) -> non_neg_integer().
decrement_load(AgentId) ->
    CounterKey = {c, l, {flow_load, AgentId}},
    gproc:update_counter(CounterKey, -1).
```

### 3.3 Capability Indexing

```erlang
-type capability() :: binary().
-type agent_id() :: binary().

%% Agent capabilities index
-record(agent_capabilities, {
    agent_id :: agent_id(),
    capabilities :: [capability()],
    metadata :: map()
}).

%% Query agents by multiple capabilities (AND logic)
-spec query_agents(Capabilities :: [capability()]) -> [agent_id()].
query_agents(Capabilities) ->
    %% Find agents that have ALL required capabilities
    AgentSets = [sets:from_list(find_agents_by_capability(Cap))
                 || Cap <- Capabilities],

    %% Intersection of all sets
    case AgentSets of
        [] -> [];
        [First | Rest] ->
            FinalSet = lists:foldl(fun sets:intersection/2, First, Rest),
            sets:to_list(FinalSet)
    end.
```

---

## 4. Load Balancing Strategies

### 4.1 Strategy Selection

```erlang
-module(erlmcp_flow_load_balancer).

-type strategy() :: round_robin | least_connections | weighted | consistent_hash.

-record(lb_state, {
    strategy :: strategy(),
    round_robin_index = 0 :: non_neg_integer(),
    consistent_hash_ring :: hash_ring:ring() | undefined
}).

%% Select agent using configured strategy
-spec select_agent(Agents, Strategy, State) -> {ok, AgentId, State}.
select_agent(Agents, round_robin, State) ->
    select_round_robin(Agents, State);
select_agent(Agents, least_connections, State) ->
    select_least_connections(Agents, State);
select_agent(Agents, weighted, State) ->
    select_weighted(Agents, State);
select_agent(Agents, consistent_hash, State) ->
    select_consistent_hash(Agents, State).
```

### 4.2 Round-Robin

```erlang
-spec select_round_robin(Agents, State) -> {ok, AgentId, State}.
select_round_robin(Agents, #lb_state{round_robin_index = Index} = State) ->
    AgentList = maps:keys(Agents),
    SelectedAgent = lists:nth((Index rem length(AgentList)) + 1, AgentList),
    NewState = State#lb_state{round_robin_index = Index + 1},
    {ok, SelectedAgent, NewState}.
```

### 4.3 Least-Connections

```erlang
-spec select_least_connections(Agents, State) -> {ok, AgentId, State}.
select_least_connections(Agents, State) ->
    %% Find agent with minimum load (O(N) scan)
    AgentLoads = [{AgentId, erlmcp_flow_registry:get_agent_load(AgentId)}
                  || AgentId <- maps:keys(Agents)],

    {MinAgent, _MinLoad} = lists:min(
        fun({_, L1}, {_, L2}) -> L1 =< L2 end,
        AgentLoads
    ),

    {ok, MinAgent, State}.
```

### 4.4 Weighted (Q-Learning Based)

```erlang
-spec select_weighted(Agents, State) -> {ok, AgentId, State}.
select_weighted(Agents, State) ->
    %% Use Q-values as weights
    QTable = erlmcp_flow_q_learning:get_q_table(),
    CurrentState = extract_current_state(),

    %% Calculate weighted probabilities
    Weights = [{AgentId, get_q_value(QTable, CurrentState, AgentId)}
               || AgentId <- maps:keys(Agents)],

    %% Softmax selection (stochastic)
    SelectedAgent = softmax_select(Weights),

    {ok, SelectedAgent, State}.

-spec softmax_select(Weights) -> AgentId.
softmax_select(Weights) ->
    %% e^(Q/T) where T = temperature
    Temperature = 1.0,
    ExpWeights = [{AgentId, math:exp(Q / Temperature)} || {AgentId, Q} <- Weights],
    TotalWeight = lists:sum([W || {_, W} <- ExpWeights]),

    %% Random selection based on probabilities
    Rand = rand:uniform() * TotalWeight,
    select_by_threshold(ExpWeights, Rand, 0).

select_by_threshold([{AgentId, Weight} | _Rest], Rand, Acc) when Acc + Weight >= Rand ->
    AgentId;
select_by_threshold([{_, Weight} | Rest], Rand, Acc) ->
    select_by_threshold(Rest, Rand, Acc + Weight).
```

### 4.5 Consistent Hashing

```erlang
-spec select_consistent_hash(Agents, State) -> {ok, AgentId, State}.
select_consistent_hash(Agents, #lb_state{consistent_hash_ring = Ring} = State) ->
    %% Use task ID as hash key for sticky routing
    TaskKey = get_task_key(),
    SelectedAgent = hash_ring:find_node(TaskKey, Ring),
    {ok, SelectedAgent, State}.

%% Initialize consistent hash ring
-spec init_hash_ring(Agents) -> hash_ring:ring().
init_hash_ring(Agents) ->
    %% 150 virtual nodes per agent for better distribution
    VirtualNodes = 150,
    Ring = hash_ring:make([{AgentId, VirtualNodes} || AgentId <- maps:keys(Agents)]),
    Ring.
```

---

## 5. Failure Handling

### 5.1 Retry Logic

```erlang
-module(erlmcp_flow_failure_detector).
-behaviour(gen_server).

-record(retry_state, {
    max_attempts = 3 :: pos_integer(),
    base_delay = 100 :: pos_integer(),  % ms
    max_delay = 5000 :: pos_integer(),
    attempt = 0 :: non_neg_integer()
}).

%% Exponential backoff retry
-spec retry_task(Task, Agent, RetryState) -> {ok, Result} | {error, exhausted}.
retry_task(Task, Agent, #retry_state{attempt = Attempt, max_attempts = Max} = State)
    when Attempt >= Max ->
    {error, retry_exhausted};

retry_task(Task, Agent, #retry_state{attempt = Attempt, base_delay = Base} = State) ->
    %% Execute task
    case execute_task(Task, Agent) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            %% Exponential backoff: Base * 2^Attempt
            Delay = min(Base * trunc(math:pow(2, Attempt)), State#retry_state.max_delay),
            logger:warning("Task failed on agent ~p (attempt ~p/~p), retrying in ~pms: ~p",
                          [Agent, Attempt + 1, State#retry_state.max_attempts, Delay, Reason]),
            timer:sleep(Delay),
            retry_task(Task, Agent, State#retry_state{attempt = Attempt + 1})
    end.
```

### 5.2 Fallback Swarm Selection

```erlang
%% Select alternative agent/swarm on failure
-spec select_fallback(FailedAgent, Task) -> {ok, AgentId} | {error, no_fallback}.
select_fallback(FailedAgent, Task) ->
    %% Get agents with same capabilities
    RequiredCaps = extract_capabilities(Task),
    Candidates = erlmcp_flow_registry:query_agents(RequiredCaps),

    %% Remove failed agent
    Available = lists:delete(FailedAgent, Candidates),

    case Available of
        [] ->
            %% No individual agents, try swarm
            select_fallback_swarm(RequiredCaps);
        [Agent | _] ->
            %% Select least-loaded agent
            {ok, Agent}
    end.

-spec select_fallback_swarm(Capabilities) -> {ok, SwarmId} | {error, no_swarm}.
select_fallback_swarm(Capabilities) ->
    %% Query for swarms that can handle task
    Swarms = erlmcp_flow_registry:find_swarms_by_capabilities(Capabilities),
    case Swarms of
        [] -> {error, no_available_swarm};
        [Swarm | _] -> {ok, Swarm}
    end.
```

### 5.3 Circuit Breaker

```erlang
-module(erlmcp_flow_circuit_breaker).
-behaviour(gen_server).

-type cb_state() :: closed | open | half_open.

-record(circuit_state, {
    state = closed :: cb_state(),
    failure_count = 0 :: non_neg_integer(),
    success_count = 0 :: non_neg_integer(),
    last_failure_time :: erlang:timestamp() | undefined,
    timeout = 30000 :: pos_integer(),  % ms
    failure_threshold = 5 :: pos_integer(),
    min_requests = 10 :: pos_integer()
}).

%% Check if circuit allows request
-spec allow_request(AgentId) -> allow | deny.
allow_request(AgentId) ->
    gen_server:call(?MODULE, {allow_request, AgentId}).

handle_call({allow_request, AgentId}, _From, State) ->
    CircuitState = get_circuit_state(AgentId, State),

    case CircuitState#circuit_state.state of
        closed ->
            {reply, allow, State};
        open ->
            %% Check if timeout expired
            Now = erlang:timestamp(),
            TimeSinceFailure = timer:now_diff(Now, CircuitState#circuit_state.last_failure_time) / 1000,
            if
                TimeSinceFailure >= CircuitState#circuit_state.timeout ->
                    %% Transition to half-open
                    NewCircuit = CircuitState#circuit_state{state = half_open},
                    {reply, allow, update_circuit_state(AgentId, NewCircuit, State)};
                true ->
                    {reply, deny, State}
            end;
        half_open ->
            %% Allow limited probing
            {reply, allow, State}
    end.

%% Record request result
-spec record_result(AgentId, Result) -> ok.
record_result(AgentId, Result) ->
    gen_server:cast(?MODULE, {record_result, AgentId, Result}).

handle_cast({record_result, AgentId, Result}, State) ->
    CircuitState = get_circuit_state(AgentId, State),

    case Result of
        {ok, _} ->
            handle_success(AgentId, CircuitState, State);
        {error, _} ->
            handle_failure(AgentId, CircuitState, State)
    end.

handle_success(AgentId, Circuit, State) ->
    NewSuccessCount = Circuit#circuit_state.success_count + 1,

    case Circuit#circuit_state.state of
        half_open when NewSuccessCount >= 3 ->
            %% Transition to closed after 3 successes
            NewCircuit = #circuit_state{state = closed, failure_count = 0, success_count = 0},
            {noreply, update_circuit_state(AgentId, NewCircuit, State)};
        _ ->
            NewCircuit = Circuit#circuit_state{success_count = NewSuccessCount},
            {noreply, update_circuit_state(AgentId, NewCircuit, State)}
    end.

handle_failure(AgentId, Circuit, State) ->
    NewFailureCount = Circuit#circuit_state.failure_count + 1,
    TotalRequests = NewFailureCount + Circuit#circuit_state.success_count,

    %% Open circuit if failure rate exceeds threshold
    FailureRate = NewFailureCount / max(1, TotalRequests),

    ShouldOpen = TotalRequests >= Circuit#circuit_state.min_requests
                 andalso FailureRate >= 0.5,

    case ShouldOpen of
        true ->
            logger:warning("Circuit breaker opened for agent ~p (failure rate: ~.2f)",
                          [AgentId, FailureRate]),
            NewCircuit = Circuit#circuit_state{
                state = open,
                last_failure_time = erlang:timestamp()
            },
            {noreply, update_circuit_state(AgentId, NewCircuit, State)};
        false ->
            NewCircuit = Circuit#circuit_state{failure_count = NewFailureCount},
            {noreply, update_circuit_state(AgentId, NewCircuit, State)}
    end.
```

### 5.4 Cascading Failure Prevention

```erlang
%% Bulkhead pattern: limit concurrent requests per agent
-record(bulkhead_state, {
    max_concurrent = 10 :: pos_integer(),
    active_requests = 0 :: non_neg_integer(),
    queue = queue:new() :: queue:queue()
}).

-spec acquire_permit(AgentId) -> ok | {error, overloaded}.
acquire_permit(AgentId) ->
    gen_server:call(?MODULE, {acquire_permit, AgentId}).

handle_call({acquire_permit, AgentId}, From, State) ->
    Bulkhead = get_bulkhead_state(AgentId, State),

    case Bulkhead#bulkhead_state.active_requests < Bulkhead#bulkhead_state.max_concurrent of
        true ->
            NewBulkhead = Bulkhead#bulkhead_state{
                active_requests = Bulkhead#bulkhead_state.active_requests + 1
            },
            {reply, ok, update_bulkhead_state(AgentId, NewBulkhead, State)};
        false ->
            %% Queue request with timeout
            NewQueue = queue:in({From, erlang:timestamp()}, Bulkhead#bulkhead_state.queue),
            NewBulkhead = Bulkhead#bulkhead_state{queue = NewQueue},
            {noreply, update_bulkhead_state(AgentId, NewBulkhead, State)}
    end.

-spec release_permit(AgentId) -> ok.
release_permit(AgentId) ->
    gen_server:cast(?MODULE, {release_permit, AgentId}).

handle_cast({release_permit, AgentId}, State) ->
    Bulkhead = get_bulkhead_state(AgentId, State),

    %% Decrement active requests
    NewActive = max(0, Bulkhead#bulkhead_state.active_requests - 1),

    %% Process queued request if any
    case queue:out(Bulkhead#bulkhead_state.queue) of
        {{value, {From, _Timestamp}}, NewQueue} ->
            gen_server:reply(From, ok),
            NewBulkhead = Bulkhead#bulkhead_state{
                active_requests = NewActive + 1,
                queue = NewQueue
            },
            {noreply, update_bulkhead_state(AgentId, NewBulkhead, State)};
        {empty, _} ->
            NewBulkhead = Bulkhead#bulkhead_state{active_requests = NewActive},
            {noreply, update_bulkhead_state(AgentId, NewBulkhead, State)}
    end.
```

---

## 6. Request Correlation

### 6.1 UUID Generation (v4)

```erlang
-module(erlmcp_flow_correlation_tracker).

%% Generate UUID v4 for trace ID
-spec generate_trace_id() -> binary().
generate_trace_id() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    %% Set version (4) and variant (RFC 4122)
    UUID = <<A:32, B:16, 4:4, (C band 16#0FFF):12, 2:2, (D band 16#3FFF):14, E:48>>,
    uuid_to_string(UUID).

uuid_to_string(<<A:32, B:16, C:16, D:16, E:48>>) ->
    list_to_binary(
        io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                      [A, B, C, D, E])
    ).
```

### 6.2 Trace Context (W3C Trace Context)

```erlang
-record(trace_context, {
    trace_id :: binary(),           % 00-32hex-16hex-01
    parent_span_id :: binary() | undefined,
    span_id :: binary(),
    trace_flags = 1 :: 0..1,       % 1 = sampled
    trace_state :: map()           % Vendor-specific state
}).

%% Propagate trace context
-spec propagate_context(TraceContext) -> binary().
propagate_context(#trace_context{
    trace_id = TraceId,
    span_id = SpanId,
    trace_flags = Flags
}) ->
    %% W3C Trace Context format: traceparent header
    iolist_to_binary([
        "00-",                     % Version
        TraceId, "-",
        SpanId, "-",
        io_lib:format("~2.16.0b", [Flags])
    ]).

%% Extract trace context from header
-spec extract_context(Binary) -> {ok, trace_context()} | {error, invalid}.
extract_context(<<"00-", Rest/binary>>) ->
    case binary:split(Rest, <<"-">>, [global]) of
        [TraceId, ParentSpanId, Flags] when byte_size(TraceId) == 32,
                                            byte_size(ParentSpanId) == 16 ->
            NewSpanId = generate_span_id(),
            {ok, #trace_context{
                trace_id = TraceId,
                parent_span_id = ParentSpanId,
                span_id = NewSpanId,
                trace_flags = binary_to_integer(Flags, 16)
            }};
        _ ->
            {error, invalid_trace_context}
    end;
extract_context(_) ->
    {error, unsupported_version}.

-spec generate_span_id() -> binary().
generate_span_id() ->
    <<N:64>> = crypto:strong_rand_bytes(8),
    list_to_binary(io_lib:format("~16.16.0b", [N])).
```

### 6.3 Correlation Storage (ETS)

```erlang
-record(correlation_entry, {
    trace_id :: binary(),
    span_id :: binary(),
    parent_span_id :: binary() | undefined,
    agent_id :: binary(),
    task :: map(),
    start_time :: erlang:timestamp(),
    end_time :: erlang:timestamp() | undefined,
    result :: term() | undefined,
    metadata :: map()
}).

%% Initialize ETS table
init([]) ->
    ETS = ets:new(correlation_table, [
        set,
        public,
        named_table,
        {keypos, #correlation_entry.trace_id},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    {ok, #state{ets = ETS}}.

%% Store correlation
-spec store_correlation(TraceId, SpanId, ParentSpanId, AgentId, Task) -> ok.
store_correlation(TraceId, SpanId, ParentSpanId, AgentId, Task) ->
    Entry = #correlation_entry{
        trace_id = TraceId,
        span_id = SpanId,
        parent_span_id = ParentSpanId,
        agent_id = AgentId,
        task = Task,
        start_time = erlang:timestamp(),
        metadata = #{}
    },
    ets:insert(correlation_table, Entry),
    ok.

%% Update correlation with result
-spec update_correlation(TraceId, Result) -> ok | {error, not_found}.
update_correlation(TraceId, Result) ->
    case ets:lookup(correlation_table, TraceId) of
        [Entry] ->
            UpdatedEntry = Entry#correlation_entry{
                end_time = erlang:timestamp(),
                result = Result
            },
            ets:insert(correlation_table, UpdatedEntry),
            ok;
        [] ->
            {error, not_found}
    end.

%% Query correlation chain (parent-child spans)
-spec get_trace_chain(TraceId) -> [correlation_entry()].
get_trace_chain(TraceId) ->
    %% Match all entries with same trace_id
    MatchSpec = [{
        #correlation_entry{trace_id = TraceId, _ = '_'},
        [],
        ['$_']
    }],
    ets:select(correlation_table, MatchSpec).
```

### 6.4 OpenTelemetry Integration

```erlang
%% Start span with trace context
-spec start_span(Name, TraceContext) -> otel_span:span().
start_span(Name, #trace_context{trace_id = TraceId, parent_span_id = ParentId}) ->
    ParentCtx = case ParentId of
        undefined -> undefined;
        _ -> otel_tracer:lookup_span(ParentId)
    end,

    Span = otel_tracer:start_span(Name, #{
        parent => ParentCtx,
        attributes => #{
            <<"trace.id">> => TraceId,
            <<"service.name">> => <<"erlmcp-flow">>
        }
    }),
    Span.

%% End span with result
-spec end_span(Span, Result) -> ok.
end_span(Span, {ok, _}) ->
    otel_span:set_status(Span, ok),
    otel_span:end_span(Span);
end_span(Span, {error, Reason}) ->
    otel_span:set_status(Span, {error, Reason}),
    otel_span:record_exception(Span, Reason),
    otel_span:end_span(Span).
```

---

## 7. Network Partitions & Byzantine Nodes

### 7.1 Byzantine Fault Tolerance (BFT)

**Assumptions**:
- N total nodes
- F Byzantine (faulty) nodes
- Quorum: 2F + 1 nodes required for consensus
- Minimum nodes: N ≥ 3F + 1

**Example**: For F=1 (tolerating 1 Byzantine node), N ≥ 4 nodes required.

### 7.2 Message Signing (HMAC-SHA256)

```erlang
-module(erlmcp_flow_byzantine_detector).

-record(signed_message, {
    payload :: binary(),
    signature :: binary(),
    sender_id :: binary(),
    timestamp :: pos_integer()
}).

%% Sign message
-spec sign_message(Payload, SenderId, SecretKey) -> signed_message().
sign_message(Payload, SenderId, SecretKey) ->
    Timestamp = erlang:system_time(millisecond),
    Data = <<Payload/binary, SenderId/binary, Timestamp:64>>,
    Signature = crypto:mac(hmac, sha256, SecretKey, Data),

    #signed_message{
        payload = Payload,
        signature = Signature,
        sender_id = SenderId,
        timestamp = Timestamp
    }.

%% Verify message signature
-spec verify_signature(SignedMessage, SecretKey) -> ok | {error, invalid_signature}.
verify_signature(#signed_message{
    payload = Payload,
    signature = Signature,
    sender_id = SenderId,
    timestamp = Timestamp
}, SecretKey) ->
    Data = <<Payload/binary, SenderId/binary, Timestamp:64>>,
    ExpectedSignature = crypto:mac(hmac, sha256, SecretKey, Data),

    case crypto:hash_equals(ExpectedSignature, Signature) of
        true -> ok;
        false -> {error, invalid_signature}
    end.
```

### 7.3 Quorum Consensus

```erlang
-record(quorum_state, {
    total_nodes :: pos_integer(),
    byzantine_tolerance :: non_neg_integer(),  % F
    quorum_size :: pos_integer(),              % 2F + 1
    responses = #{} :: #{node_id() => term()}
}).

%% Initialize quorum state
-spec init_quorum(TotalNodes, ByzantineTolerance) -> quorum_state().
init_quorum(TotalNodes, F) when TotalNodes >= 3 * F + 1 ->
    #quorum_state{
        total_nodes = TotalNodes,
        byzantine_tolerance = F,
        quorum_size = 2 * F + 1
    };
init_quorum(TotalNodes, F) ->
    error({insufficient_nodes, TotalNodes, required, 3 * F + 1}).

%% Collect response and check quorum
-spec add_response(NodeId, Response, QuorumState) ->
    {quorum_reached, term()} | {pending, QuorumState}.
add_response(NodeId, Response, #quorum_state{responses = Responses} = State) ->
    NewResponses = maps:put(NodeId, Response, Responses),
    NewState = State#quorum_state{responses = NewResponses},

    %% Check if quorum reached
    case maps:size(NewResponses) >= State#quorum_state.quorum_size of
        true ->
            %% Find majority response (Byzantine agreement)
            MajorityResponse = find_majority_response(NewResponses),
            {quorum_reached, MajorityResponse};
        false ->
            {pending, NewState}
    end.

%% Find majority response (must have > F identical responses)
-spec find_majority_response(Responses) -> term() | no_majority.
find_majority_response(Responses) ->
    %% Count occurrences of each response
    Counts = maps:fold(
        fun(_NodeId, Response, Acc) ->
            maps:update_with(Response, fun(Count) -> Count + 1 end, 1, Acc)
        end,
        #{},
        Responses
    ),

    %% Find response with maximum count
    {MajorityResponse, MaxCount} = maps:fold(
        fun(Response, Count, {CurResp, CurMax}) ->
            if Count > CurMax -> {Response, Count};
               true -> {CurResp, CurMax}
            end
        end,
        {undefined, 0},
        Counts
    ),

    %% Verify it exceeds Byzantine threshold
    case MaxCount > maps:size(Responses) div 2 of
        true -> MajorityResponse;
        false -> no_majority
    end.
```

### 7.4 Byzantine Node Detection & Blacklisting

```erlang
-record(node_reputation, {
    node_id :: binary(),
    honest_votes = 0 :: non_neg_integer(),
    dishonest_votes = 0 :: non_neg_integer(),
    blacklisted = false :: boolean(),
    last_seen :: erlang:timestamp()
}).

%% Update reputation based on vote result
-spec update_reputation(NodeId, VoteResult) -> ok.
update_reputation(NodeId, honest) ->
    gen_server:cast(?MODULE, {update_reputation, NodeId, honest});
update_reputation(NodeId, dishonest) ->
    gen_server:cast(?MODULE, {update_reputation, NodeId, dishonest}).

handle_cast({update_reputation, NodeId, VoteResult}, State) ->
    Reputation = get_reputation(NodeId, State),

    NewReputation = case VoteResult of
        honest ->
            Reputation#node_reputation{honest_votes = Reputation#node_reputation.honest_votes + 1};
        dishonest ->
            Reputation#node_reputation{dishonest_votes = Reputation#node_reputation.dishonest_votes + 1}
    end,

    %% Blacklist if dishonest ratio exceeds threshold (e.g., 30%)
    TotalVotes = NewReputation#node_reputation.honest_votes + NewReputation#node_reputation.dishonest_votes,
    DishonestRatio = NewReputation#node_reputation.dishonest_votes / max(1, TotalVotes),

    FinalReputation = if
        DishonestRatio > 0.3 ->
            logger:warning("Blacklisting Byzantine node ~p (dishonest ratio: ~.2f)",
                          [NodeId, DishonestRatio]),
            NewReputation#node_reputation{blacklisted = true};
        true ->
            NewReputation
    end,

    {noreply, update_reputation_state(NodeId, FinalReputation, State)}.

%% Check if node is blacklisted
-spec is_blacklisted(NodeId) -> boolean().
is_blacklisted(NodeId) ->
    gen_server:call(?MODULE, {is_blacklisted, NodeId}).
```

### 7.5 Network Partition Detection

```erlang
-record(partition_detector_state, {
    heartbeat_interval = 5000 :: pos_integer(),  % ms
    timeout_threshold = 15000 :: pos_integer(),  % 3x heartbeat
    node_last_seen = #{} :: #{node_id() => erlang:timestamp()}
}).

%% Detect partitioned nodes
-spec detect_partitions(State) -> [node_id()].
detect_partitions(#partition_detector_state{
    node_last_seen = NodeLastSeen,
    timeout_threshold = Threshold
}) ->
    Now = erlang:timestamp(),

    Partitioned = maps:fold(
        fun(NodeId, LastSeen, Acc) ->
            TimeSince = timer:now_diff(Now, LastSeen) / 1000,  % ms
            case TimeSince > Threshold of
                true -> [NodeId | Acc];
                false -> Acc
            end
        end,
        [],
        NodeLastSeen
    ),

    Partitioned.

%% Handle partition recovery
handle_info(check_partitions, State) ->
    Partitioned = detect_partitions(State),

    %% Log partitioned nodes
    case Partitioned of
        [] -> ok;
        Nodes ->
            logger:warning("Detected partitioned nodes: ~p", [Nodes]),
            %% Trigger leader re-election if leader is partitioned
            case lists:member(get_current_leader(), Nodes) of
                true -> trigger_leader_election();
                false -> ok
            end
    end,

    %% Schedule next check
    erlang:send_after(State#partition_detector_state.heartbeat_interval, self(), check_partitions),
    {noreply, State}.
```

---

## 8. Message Format

### 8.1 Routing Request Message

```erlang
-record(routing_request, {
    trace_id :: binary(),
    span_id :: binary(),
    parent_span_id :: binary() | undefined,
    task :: map(),
    requirements :: #{
        capabilities := [binary()],
        priority => low | normal | high,
        timeout => pos_integer(),
        sticky_session => boolean()
    },
    metadata :: map()
}).

%% JSON representation
-spec encode_routing_request(routing_request()) -> binary().
encode_routing_request(#routing_request{
    trace_id = TraceId,
    span_id = SpanId,
    task = Task,
    requirements = Requirements
}) ->
    jsx:encode(#{
        <<"trace_id">> => TraceId,
        <<"span_id">> => SpanId,
        <<"task">> => Task,
        <<"requirements">> => Requirements
    }).
```

### 8.2 Routing Response Message

```erlang
-record(routing_response, {
    trace_id :: binary(),
    span_id :: binary(),
    agent_id :: binary(),
    status :: ok | error,
    result :: term() | undefined,
    error :: term() | undefined,
    metrics :: #{
        latency := pos_integer(),
        queue_time := pos_integer(),
        execution_time := pos_integer()
    }
}).

%% JSON representation
-spec encode_routing_response(routing_response()) -> binary().
encode_routing_response(#routing_response{
    trace_id = TraceId,
    agent_id = AgentId,
    status = Status,
    result = Result,
    metrics = Metrics
}) ->
    jsx:encode(#{
        <<"trace_id">> => TraceId,
        <<"agent_id">> => AgentId,
        <<"status">> => Status,
        <<"result">> => Result,
        <<"metrics">> => Metrics
    }).
```

### 8.3 Agent Metrics Message

```erlang
-record(agent_metrics, {
    agent_id :: binary(),
    timestamp :: pos_integer(),
    load :: non_neg_integer(),
    success_rate :: float(),
    avg_latency :: pos_integer(),
    p95_latency :: pos_integer(),
    p99_latency :: pos_integer(),
    total_tasks :: non_neg_integer(),
    failed_tasks :: non_neg_integer(),
    active_tasks :: non_neg_integer()
}).

%% Periodic metrics reporting
-spec report_metrics(AgentId, Metrics) -> ok.
report_metrics(AgentId, Metrics) ->
    erlmcp_flow_router:update_agent_metrics(AgentId, Metrics).
```

---

## 9. Routing Algorithm

### 9.1 Main Routing Function

```erlang
-module(erlmcp_flow_router).
-behaviour(gen_server).

-spec route_task(Task, Requirements) -> {ok, AgentId, TraceContext} | {error, Reason}.
route_task(Task, Requirements) ->
    gen_server:call(?MODULE, {route_task, Task, Requirements}).

handle_call({route_task, Task, Requirements}, _From, State) ->
    %% Step 1: Generate trace context
    TraceId = erlmcp_flow_correlation_tracker:generate_trace_id(),
    SpanId = erlmcp_flow_correlation_tracker:generate_span_id(),
    TraceContext = #trace_context{
        trace_id = TraceId,
        span_id = SpanId,
        trace_flags = 1
    },

    %% Step 2: Start distributed trace span
    Span = otel_tracer:start_span(<<"route_task">>, #{
        attributes => #{
            <<"trace.id">> => TraceId,
            <<"task.type">> => maps:get(type, Task, unknown)
        }
    }),

    try
        %% Step 3: Query service discovery for candidates
        Capabilities = maps:get(capabilities, Requirements),
        Candidates = erlmcp_flow_registry:query_agents(Capabilities),

        case Candidates of
            [] ->
                {reply, {error, no_agents_available}, State};
            _ ->
                %% Step 4: Q-Learning agent selection
                CurrentState = extract_routing_state(Candidates, State),
                {ok, SelectedAgent} = erlmcp_flow_q_learning:select_agent(CurrentState),

                %% Step 5: Check circuit breaker
                case erlmcp_flow_circuit_breaker:allow_request(SelectedAgent) of
                    deny ->
                        %% Circuit open, select fallback
                        {ok, FallbackAgent} = select_fallback(SelectedAgent, Task),
                        route_to_agent(FallbackAgent, Task, TraceContext, State);
                    allow ->
                        route_to_agent(SelectedAgent, Task, TraceContext, State)
                end
        end
    after
        otel_span:end_span(Span)
    end.

-spec route_to_agent(AgentId, Task, TraceContext, State) -> {reply, Reply, State}.
route_to_agent(AgentId, Task, TraceContext, State) ->
    %% Step 6: Acquire bulkhead permit
    case erlmcp_flow_failure_detector:acquire_permit(AgentId) of
        {error, overloaded} ->
            {reply, {error, agent_overloaded}, State};
        ok ->
            %% Step 7: Store correlation
            erlmcp_flow_correlation_tracker:store_correlation(
                TraceContext#trace_context.trace_id,
                TraceContext#trace_context.span_id,
                TraceContext#trace_context.parent_span_id,
                AgentId,
                Task
            ),

            %% Step 8: Increment agent load
            erlmcp_flow_registry:increment_load(AgentId),

            %% Step 9: Send task to agent
            {ok, AgentPid} = erlmcp_flow_registry:find_agent(AgentId),
            AgentPid ! {execute_task, Task, TraceContext},

            {reply, {ok, AgentId, TraceContext}, State}
    end.
```

### 9.2 Task Execution Workflow

```
┌──────────────────────────────────────────────────────────────┐
│ 1. Client sends routing_request with task + requirements    │
└──────────────────┬───────────────────────────────────────────┘
                   │
                   ▼
┌──────────────────────────────────────────────────────────────┐
│ 2. Router generates trace_id + span_id (UUID v4)            │
└──────────────────┬───────────────────────────────────────────┘
                   │
                   ▼
┌──────────────────────────────────────────────────────────────┐
│ 3. Service Discovery: query agents by capabilities (O(log N))│
│    - gproc:lookup_pids({p, l, {flow_capability, Cap}})      │
└──────────────────┬───────────────────────────────────────────┘
                   │
                   ▼
┌──────────────────────────────────────────────────────────────┐
│ 4. Q-Learning: select optimal agent based on state          │
│    - State: {load, success_rate, latency}                   │
│    - Action: agent_id                                        │
│    - Policy: epsilon-greedy (ε=0.1)                         │
└──────────────────┬───────────────────────────────────────────┘
                   │
                   ▼
┌──────────────────────────────────────────────────────────────┐
│ 5. Load Balancer: apply strategy (round-robin, etc.)        │
└──────────────────┬───────────────────────────────────────────┘
                   │
                   ▼
┌──────────────────────────────────────────────────────────────┐
│ 6. Circuit Breaker: check if agent available                │
│    - States: closed, open, half_open                         │
│    - Fallback if circuit open                                │
└──────────────────┬───────────────────────────────────────────┘
                   │
                   ▼
┌──────────────────────────────────────────────────────────────┐
│ 7. Bulkhead: acquire permit (max 10 concurrent/agent)       │
│    - Queue if overloaded                                     │
└──────────────────┬───────────────────────────────────────────┘
                   │
                   ▼
┌──────────────────────────────────────────────────────────────┐
│ 8. Correlation Tracker: store trace context in ETS          │
└──────────────────┬───────────────────────────────────────────┘
                   │
                   ▼
┌──────────────────────────────────────────────────────────────┐
│ 9. Registry: increment agent load counter                   │
│    - gproc:update_counter({c, l, {flow_load, AgentId}}, 1)  │
└──────────────────┬───────────────────────────────────────────┘
                   │
                   ▼
┌──────────────────────────────────────────────────────────────┐
│ 10. Send task to agent process (Pid ! {execute_task, ...})  │
└──────────────────┬───────────────────────────────────────────┘
                   │
                   ▼
┌──────────────────────────────────────────────────────────────┐
│ 11. Agent executes task, returns result                     │
└──────────────────┬───────────────────────────────────────────┘
                   │
                   ▼
┌──────────────────────────────────────────────────────────────┐
│ 12. Router receives result, updates metrics                 │
│     - Q-Learning: update reward                              │
│     - Circuit Breaker: record success/failure                │
│     - Bulkhead: release permit                               │
│     - Registry: decrement load                               │
│     - Correlation: update with result                        │
└──────────────────────────────────────────────────────────────┘
```

---

## 10. State Machine

### 10.1 Router State Machine

```
┌─────────────┐
│   IDLE      │
└──────┬──────┘
       │ route_task request
       │
       ▼
┌─────────────┐
│ DISCOVERING │ ─────► Query service discovery (O(log N))
└──────┬──────┘
       │ found candidates
       │
       ▼
┌─────────────┐
│  LEARNING   │ ─────► Q-Learning agent selection
└──────┬──────┘
       │ agent selected
       │
       ▼
┌─────────────┐
│  CHECKING   │ ─────► Circuit breaker + bulkhead check
└──────┬──────┘
       │ checks passed
       │
       ▼
┌─────────────┐
│  ROUTING    │ ─────► Send task to agent
└──────┬──────┘
       │ task sent
       │
       ▼
┌─────────────┐
│  WAITING    │ ─────► Await agent response (timeout: 5s)
└──────┬──────┘
       │ response received
       │
       ▼
┌─────────────┐
│  UPDATING   │ ─────► Update Q-table, metrics, correlation
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   IDLE      │ ◄──── Ready for next request
└─────────────┘
```

### 10.2 Circuit Breaker State Machine

```
                 ┌─────────────┐
                 │   CLOSED    │ ◄────┐
                 │ (normal)    │      │
                 └──────┬──────┘      │
                        │             │
      failure_rate > 50%│             │ 3 successes
      min_requests = 10 │             │ in half_open
                        │             │
                        ▼             │
                 ┌─────────────┐      │
                 │    OPEN     │      │
                 │ (reject)    │      │
                 └──────┬──────┘      │
                        │             │
         timeout = 30s  │             │
                        │             │
                        ▼             │
                 ┌─────────────┐      │
                 │ HALF_OPEN   │ ─────┘
                 │ (probe)     │
                 └─────────────┘
                        │
         failure        │
                        ▼
                 (back to OPEN)
```

### 10.3 Agent State Machine

```
┌─────────────┐
│    IDLE     │ ◄──────────────────┐
└──────┬──────┘                    │
       │ task received              │
       │                            │
       ▼                            │
┌─────────────┐                    │
│   BUSY      │ ─────► Execute     │
└──────┬──────┘        task        │
       │                            │
       │ task completed             │
       │                            │
       ▼                            │
┌─────────────┐                    │
│  REPORTING  │ ─────► Send result │
└──────┬──────┘        + metrics   │
       │                            │
       └────────────────────────────┘
```

---

## 11. Timeout Handling

### 11.1 Routing Timeout

```erlang
-define(DEFAULT_ROUTING_TIMEOUT, 5000).  % 5 seconds

-spec route_task_with_timeout(Task, Requirements, Timeout) ->
    {ok, AgentId, TraceContext} | {error, timeout | term()}.
route_task_with_timeout(Task, Requirements, Timeout) ->
    try
        gen_server:call(erlmcp_flow_router, {route_task, Task, Requirements}, Timeout)
    catch
        exit:{timeout, _} ->
            {error, routing_timeout};
        exit:{noproc, _} ->
            {error, router_not_running}
    end.
```

### 11.2 Task Execution Timeout

```erlang
-spec execute_task_with_timeout(AgentPid, Task, TraceContext, Timeout) ->
    {ok, Result} | {error, timeout | term()}.
execute_task_with_timeout(AgentPid, Task, TraceContext, Timeout) ->
    %% Send task with timeout
    Ref = make_ref(),
    AgentPid ! {execute_task, Task, TraceContext, self(), Ref},

    receive
        {task_result, Ref, Result} ->
            {ok, Result}
    after Timeout ->
        %% Cancel task on agent
        AgentPid ! {cancel_task, Ref},
        {error, execution_timeout}
    end.
```

### 11.3 Adaptive Timeout

```erlang
-record(adaptive_timeout_state, {
    base_timeout = 5000 :: pos_integer(),
    min_timeout = 1000 :: pos_integer(),
    max_timeout = 30000 :: pos_integer(),
    p95_latency = 5000 :: pos_integer()
}).

%% Calculate adaptive timeout based on historical latency
-spec calculate_timeout(AgentId, BaseTimeout) -> pos_integer().
calculate_timeout(AgentId, BaseTimeout) ->
    Metrics = erlmcp_flow_router:get_agent_metrics(AgentId),
    P95Latency = maps:get(p95_latency, Metrics, BaseTimeout),

    %% Timeout = P95 + 2x buffer
    AdaptiveTimeout = trunc(P95Latency * 2.0),

    %% Clamp to min/max bounds
    max(1000, min(30000, AdaptiveTimeout)).
```

---

## 12. Module Specifications

### 12.1 Module Summary

| Module | Lines | Tests | Coverage | Purpose |
|--------|-------|-------|----------|---------|
| erlmcp_flow_sup | 150 | 5 | 95% | Supervisor tree |
| erlmcp_flow_router | 500 | 45 | 92% | Main routing logic + Q-Learning |
| erlmcp_flow_registry | 400 | 38 | 94% | Service discovery (gproc) |
| erlmcp_flow_q_learning | 350 | 28 | 88% | Q-Learning engine |
| erlmcp_flow_load_balancer | 300 | 25 | 90% | Load balancing strategies |
| erlmcp_flow_failure_detector | 450 | 35 | 91% | Retry + fallback logic |
| erlmcp_flow_circuit_breaker | 320 | 30 | 93% | Circuit breaker pattern |
| erlmcp_flow_correlation_tracker | 280 | 22 | 89% | UUID + trace context |
| erlmcp_flow_byzantine_detector | 400 | 32 | 87% | BFT + quorum consensus |
| erlmcp_flow_serializer | 200 | 18 | 95% | Message encode/decode |
| **TOTAL** | **3,350** | **278** | **91%** | **10 modules** |

### 12.2 Supervision Hierarchy

```erlang
-module(erlmcp_flow_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% Registry (core)
        #{
            id => erlmcp_flow_registry,
            start => {erlmcp_flow_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_flow_registry]
        },

        %% Q-Learning engine
        #{
            id => erlmcp_flow_q_learning,
            start => {erlmcp_flow_q_learning, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Circuit breaker
        #{
            id => erlmcp_flow_circuit_breaker,
            start => {erlmcp_flow_circuit_breaker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Correlation tracker
        #{
            id => erlmcp_flow_correlation_tracker,
            start => {erlmcp_flow_correlation_tracker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Byzantine detector
        #{
            id => erlmcp_flow_byzantine_detector,
            start => {erlmcp_flow_byzantine_detector, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Failure detector
        #{
            id => erlmcp_flow_failure_detector,
            start => {erlmcp_flow_failure_detector, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Router (depends on all above)
        #{
            id => erlmcp_flow_router,
            start => {erlmcp_flow_router, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

---

## 13. Test Strategy

### 13.1 Chicago TDD Approach

**Principles**:
- Black-box testing (behavior, not implementation)
- No mocks (real processes only)
- Test observables (messages, state, side-effects)
- Coverage ≥ 80%

### 13.2 Test Suites

```erlang
%% 1. Registry Tests (erlmcp_flow_registry_tests.erl)
- test_agent_registration/0
- test_agent_discovery_by_capability/0
- test_load_counter_increment/0
- test_agent_crash_cleanup/0
- test_concurrent_registrations/0
- Benchmark: O(log N) lookup (1K, 10K, 100K agents)

%% 2. Router Tests (erlmcp_flow_router_tests.erl)
- test_route_task_success/0
- test_route_task_no_agents/0
- test_route_task_timeout/0
- test_correlation_tracking/0
- test_metrics_update/0
- Benchmark: Routing latency (p50, p95, p99)

%% 3. Q-Learning Tests (erlmcp_flow_q_learning_tests.erl)
- test_epsilon_greedy_selection/0
- test_q_table_update/0
- test_reward_calculation/0
- test_model_persistence/0
- test_convergence_after_1000_episodes/0

%% 4. Load Balancer Tests (erlmcp_flow_load_balancer_tests.erl)
- test_round_robin_distribution/0
- test_least_connections_selection/0
- test_weighted_selection/0
- test_consistent_hash_stickiness/0

%% 5. Failure Detector Tests (erlmcp_flow_failure_detector_tests.erl)
- test_retry_exponential_backoff/0
- test_fallback_swarm_selection/0
- test_circuit_breaker_opens_on_failures/0
- test_bulkhead_overflow_queue/0

%% 6. Correlation Tests (erlmcp_flow_correlation_tracker_tests.erl)
- test_uuid_generation/0
- test_trace_context_propagation/0
- test_parent_child_span_linking/0
- test_correlation_storage_retrieval/0

%% 7. Byzantine Tests (erlmcp_flow_byzantine_detector_tests.erl)
- test_message_signature_verification/0
- test_quorum_consensus/0
- test_majority_response_selection/0
- test_byzantine_node_blacklisting/0
- test_partition_detection/0
```

### 13.3 Property-Based Tests (PropEr)

```erlang
-module(erlmcp_flow_proper_tests).
-include_lib("proper/include/proper.hrl").

%% Property: Q-Learning converges to optimal policy
prop_q_learning_converges() ->
    ?FORALL(Episodes, non_empty(list(episode())),
        begin
            QTable = train_q_learning(Episodes),
            OptimalAgent = best_agent(QTable),
            avg_reward(OptimalAgent) > avg_reward(random_agent())
        end
    ).

%% Property: Load balancing distributes tasks evenly
prop_load_balancing_fairness() ->
    ?FORALL({Tasks, Agents}, {list(task()), list(agent())},
        begin
            Distribution = distribute_tasks(Tasks, Agents, round_robin),
            MaxLoad = lists:max([length(TaskList) || {_Agent, TaskList} <- Distribution]),
            MinLoad = lists:min([length(TaskList) || {_Agent, TaskList} <- Distribution]),
            MaxLoad - MinLoad =< 1  % At most 1 task difference
        end
    ).

%% Property: Byzantine consensus requires 2F+1 quorum
prop_byzantine_quorum() ->
    ?FORALL({N, F}, {pos_integer(), non_neg_integer()},
        N >= 3 * F + 1 ==>
        begin
            QuorumSize = 2 * F + 1,
            Responses = generate_responses(N, F),
            case collect_quorum(Responses, QuorumSize) of
                {quorum_reached, _Result} -> true;
                {pending, _State} -> false
            end
        end
    ).
```

### 13.4 Chaos Engineering Tests

```erlang
-module(erlmcp_flow_chaos_tests).

%% Test: Random agent crashes during routing
test_agent_crash_recovery() ->
    Agents = start_agents(10),

    %% Spawn chaos monkey to crash agents randomly
    spawn(fun() -> chaos_monkey(Agents, 0.1) end),  % 10% crash rate

    %% Route 1000 tasks
    Results = [route_task(Task) || Task <- generate_tasks(1000)],

    %% Verify: All tasks eventually complete
    SuccessRate = length([R || {ok, _} <- Results]) / 1000,
    ?assert(SuccessRate > 0.95),  % At least 95% success

    ok.

%% Test: Network partition during consensus
test_network_partition_recovery() ->
    Nodes = start_nodes(7),  % N=7, F=2

    %% Partition: 4 nodes in partition A, 3 in partition B
    {PartitionA, PartitionB} = lists:split(4, Nodes),
    create_partition(PartitionA, PartitionB),

    %% Verify: Partition A can still reach consensus (4 >= 2*2+1)
    {ok, Result} = submit_task_to_partition(PartitionA),

    %% Heal partition
    heal_partition(PartitionA, PartitionB),

    %% Verify: Both partitions converge to same state
    StateA = get_partition_state(PartitionA),
    StateB = get_partition_state(PartitionB),
    ?assertEqual(StateA, StateB),

    ok.
```

---

## 14. Performance Benchmarks

### 14.1 Benchmark Suite

```erlang
-module(erlmcp_flow_bench).

%% Run all benchmarks
run_all_benchmarks() ->
    Results = [
        benchmark_registry_lookup(),
        benchmark_routing_decision(),
        benchmark_q_learning_inference(),
        benchmark_load_balancing(),
        benchmark_circuit_breaker(),
        benchmark_correlation_tracking(),
        benchmark_byzantine_consensus()
    ],

    print_results(Results).

%% Benchmark: Registry lookup (O(log N))
benchmark_registry_lookup() ->
    Agents = setup_agents(10000),

    %% Measure 10,000 lookups
    {Time, _} = timer:tc(fun() ->
        [erlmcp_flow_registry:find_agent(AgentId)
         || AgentId <- select_random_agents(10000, Agents)]
    end),

    AvgLatency = Time / 10000,  % microseconds

    #{
        name => registry_lookup,
        avg_latency_us => AvgLatency,
        target_us => 10,
        p99_us => percentile(Times, 0.99),
        throughput => 10000 / (Time / 1000000)  % ops/sec
    }.

%% Benchmark: Routing decision (end-to-end)
benchmark_routing_decision() ->
    Agents = setup_agents(60),
    Tasks = generate_tasks(1000),

    Latencies = [
        begin
            Start = erlang:monotonic_time(microsecond),
            {ok, _AgentId, _Ctx} = erlmcp_flow_router:route_task(Task, #{}),
            End = erlang:monotonic_time(microsecond),
            End - Start
        end
        || Task <- Tasks
    ],

    #{
        name => routing_decision,
        p50_us => percentile(Latencies, 0.5),
        p95_us => percentile(Latencies, 0.95),
        p99_us => percentile(Latencies, 0.99),
        target_p99_ms => 50,
        throughput => 1000 / (lists:sum(Latencies) / 1000000)
    }.

%% Benchmark: Q-Learning inference
benchmark_q_learning_inference() ->
    QTable = train_q_table(1000),
    States = generate_states(1000),

    {Time, _} = timer:tc(fun() ->
        [erlmcp_flow_q_learning:select_action(State, QTable, 0.1)
         || State <- States]
    end),

    #{
        name => q_learning_inference,
        avg_latency_us => Time / 1000,
        target_us => 100,
        throughput => 1000 / (Time / 1000000)
    }.
```

### 14.2 Performance Targets

| Benchmark | Metric | Target | Acceptance |
|-----------|--------|--------|------------|
| Registry Lookup | p99 latency | < 100μs | < 200μs |
| Routing Decision | p99 latency | < 50ms | < 100ms |
| Q-Learning Inference | avg latency | < 100μs | < 500μs |
| Load Balancing | p99 latency | < 10ms | < 20ms |
| Circuit Breaker Check | avg latency | < 10μs | < 50μs |
| Correlation Storage | avg latency | < 50μs | < 100μs |
| Byzantine Consensus | quorum time | < 500ms | < 1s |
| Throughput (routing) | ops/sec | > 50K | > 40K |
| Memory (1000 agents) | total | < 512MB | < 1GB |

### 14.3 Load Testing

```erlang
%% Sustained load test: 10K requests/sec for 60 seconds
load_test() ->
    Duration = 60 * 1000,  % 60 seconds
    RequestsPerSecond = 10000,
    IntervalMs = 1000 / RequestsPerSecond,

    EndTime = erlang:monotonic_time(millisecond) + Duration,

    Results = load_test_loop(EndTime, IntervalMs, []),

    analyze_results(Results).

load_test_loop(EndTime, IntervalMs, Acc) ->
    Now = erlang:monotonic_time(millisecond),
    case Now < EndTime of
        true ->
            Start = erlang:monotonic_time(microsecond),
            Result = erlmcp_flow_router:route_task(generate_task(), #{}),
            End = erlang:monotonic_time(microsecond),
            Latency = End - Start,

            timer:sleep(trunc(IntervalMs)),
            load_test_loop(EndTime, IntervalMs, [{Result, Latency} | Acc]);
        false ->
            Acc
    end.
```

---

## 15. Integration Points

### 15.1 Integration with erlmcp_registry

```erlang
%% Query existing servers/transports
-spec integrate_with_erlmcp_registry(ServerId) -> ok.
integrate_with_erlmcp_registry(ServerId) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {ServerPid, Config}} ->
            %% Register server as an agent in flow registry
            Capabilities = extract_capabilities(Config),
            erlmcp_flow_registry:register_agent(ServerId, ServerPid, Capabilities);
        {error, not_found} ->
            {error, server_not_found}
    end.
```

### 15.2 Integration with erlmcp_json_rpc

```erlang
%% Serialize routing messages using JSON-RPC
-spec serialize_routing_request(Request) -> binary().
serialize_routing_request(#routing_request{task = Task, requirements = Requirements}) ->
    erlmcp_json_rpc:encode_request(
        generate_request_id(),
        <<"flow.route_task">>,
        #{
            <<"task">> => Task,
            <<"requirements">> => Requirements
        }
    ).
```

### 15.3 Integration with erlmcp_observability

```erlang
%% Report flow metrics to observability subsystem
-spec report_flow_metrics(Metrics) -> ok.
report_flow_metrics(#{
    routing_latency := Latency,
    agent_id := AgentId,
    success := Success
}) ->
    erlmcp_metrics:histogram(<<"flow.routing.latency">>, Latency),
    erlmcp_metrics:counter(<<"flow.routing.total">>, 1),

    case Success of
        true ->
            erlmcp_metrics:counter(<<"flow.routing.success">>, 1);
        false ->
            erlmcp_metrics:counter(<<"flow.routing.failure">>, 1)
    end,

    ok.
```

### 15.4 Integration with erlmcp_transport_*

```erlang
%% Bridge stdio transport to flow router
-module(erlmcp_flow_stdio_bridge).
-behaviour(gen_server).

init([]) ->
    %% Subscribe to stdio transport messages
    erlmcp_registry:bind_transport_to_server(stdio_transport, flow_router),
    {ok, #state{}}.

handle_info({transport_message, Data}, State) ->
    %% Decode JSON-RPC request
    {ok, Request} = erlmcp_json_rpc:decode_message(Data),

    %% Route to appropriate agent
    case Request of
        #json_rpc_request{method = <<"flow.route_task">>, params = Params} ->
            Task = maps:get(<<"task">>, Params),
            Requirements = maps:get(<<"requirements">>, Params, #{}),

            {ok, AgentId, TraceContext} = erlmcp_flow_router:route_task(Task, Requirements),

            %% Send response back via stdio transport
            Response = erlmcp_json_rpc:encode_response(
                Request#json_rpc_request.id,
                #{
                    <<"agent_id">> => AgentId,
                    <<"trace_id">> => TraceContext#trace_context.trace_id
                }
            ),
            erlmcp_transport_stdio:send(Response),

            {noreply, State};
        _ ->
            {noreply, State}
    end.
```

---

## Appendix A: Configuration

```erlang
%% config/sys.config
[
    {erlmcp_flow, [
        %% Q-Learning configuration
        {q_learning, #{
            epsilon => 0.1,             % Exploration rate
            alpha => 0.1,               % Learning rate
            gamma => 0.9,               % Discount factor
            model_path => "data/q_table.bin"
        }},

        %% Load balancer configuration
        {load_balancer, #{
            strategy => round_robin,    % round_robin | least_connections | weighted
            consistent_hash_vnodes => 150
        }},

        %% Circuit breaker configuration
        {circuit_breaker, #{
            failure_threshold => 5,
            min_requests => 10,
            timeout => 30000,           % 30 seconds
            half_open_max_requests => 3
        }},

        %% Failure detector configuration
        {failure_detector, #{
            max_retry_attempts => 3,
            base_retry_delay => 100,    % ms
            max_retry_delay => 5000
        }},

        %% Byzantine fault tolerance
        {byzantine, #{
            byzantine_tolerance => 1,   % F = 1 (tolerates 1 faulty node)
            quorum_size => 3,           % 2F + 1
            signature_algorithm => hmac_sha256
        }},

        %% Correlation tracking
        {correlation, #{
            max_trace_entries => 100000,
            trace_ttl => 3600000        % 1 hour in ms
        }},

        %% Bulkhead configuration
        {bulkhead, #{
            max_concurrent_per_agent => 10,
            queue_timeout => 5000       % 5 seconds
        }},

        %% Timeouts
        {timeouts, #{
            routing => 5000,            % 5 seconds
            execution => 30000,         % 30 seconds
            heartbeat => 5000           % 5 seconds
        }}
    ]}
].
```

---

## Appendix B: Deployment

```bash
# Build release
rebar3 as prod release

# Start in production mode
_build/prod/rel/erlmcp_flow/bin/erlmcp_flow start

# Check cluster status
_build/prod/rel/erlmcp_flow/bin/erlmcp_flow rpc erlmcp_flow_registry list_agents

# View Q-Learning metrics
_build/prod/rel/erlmcp_flow/bin/erlmcp_flow rpc erlmcp_flow_q_learning get_metrics

# Graceful shutdown
_build/prod/rel/erlmcp_flow/bin/erlmcp_flow stop
```

---

## Appendix C: Monitoring

```erlang
%% Prometheus metrics exposed
erlmcp_flow_routing_latency_seconds{quantile="0.5"}
erlmcp_flow_routing_latency_seconds{quantile="0.95"}
erlmcp_flow_routing_latency_seconds{quantile="0.99"}
erlmcp_flow_routing_total
erlmcp_flow_routing_success_total
erlmcp_flow_routing_failure_total
erlmcp_flow_circuit_breaker_state{agent_id="..."}
erlmcp_flow_q_learning_episode_count
erlmcp_flow_q_learning_epsilon
erlmcp_flow_agent_load{agent_id="..."}
erlmcp_flow_byzantine_dishonest_votes{node_id="..."}
```

---

## Summary

This design provides a comprehensive routing layer for erlmcp-flow with:

1. **Q-Learning optimization** - Adaptive agent selection based on load, success rate, and latency
2. **O(log N) service discovery** - gproc-based capability indexing and lookup
3. **Multiple load balancing strategies** - Round-robin, least-connections, weighted, consistent hashing
4. **Byzantine fault tolerance** - 3f+1 quorum consensus with message signing
5. **Comprehensive failure handling** - Retry, fallback, circuit breaker, bulkhead
6. **Distributed tracing** - UUID-based correlation with W3C Trace Context
7. **OTP compliance** - gen_server behaviors, supervision trees, let-it-crash

**Implementation Timeline**: 48 hours (6 phases × 8 hours)
**Test Coverage**: ≥ 91% (278 tests across 10 modules)
**Performance**: p99 routing latency < 50ms, throughput > 50K ops/sec

This design is ready for implementation following the SPARC methodology.
