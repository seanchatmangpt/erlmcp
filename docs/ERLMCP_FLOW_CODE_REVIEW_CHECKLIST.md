# erlmcp-flow Code Review Checklist

**Version:** 1.0.0
**Date:** 2026-02-02
**Status:** MANDATORY
**Extends:** [ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md](./ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md)

---

## Executive Summary

This checklist ensures erlmcp-flow code meets the highest quality standards for:
1. **Agent coordination** - Routing, registry, load balancing
2. **Performance** - 500K msg/s, <50ms p99 latency, zero task loss
3. **OTP compliance** - gen_server, supervision, let-it-crash
4. **Chicago TDD** - Real processes, no mocks, black-box testing
5. **Byzantine fault tolerance** - 3f+1 quorum, consensus safety

**Use this checklist for every erlmcp-flow code review BEFORE approval.**

---

## 1. erlmcp-flow Specific Checklist

### 1.1 Module Naming & Structure

**Rule:** All erlmcp-flow modules MUST follow `erlmcp_flow_*` naming convention.

```erlang
% ✅ CORRECT: Proper naming
-module(erlmcp_flow_registry).
-module(erlmcp_flow_router).
-module(erlmcp_flow_q_learning).
-module(erlmcp_flow_backpressure).

% ❌ VIOLATION: Wrong naming
-module(flow_registry).        % Missing erlmcp_ prefix
-module(erlmcp_registry_flow). % Wrong suffix
-module(registry).             % Too generic
```

**Checklist:**
- [ ] Module name starts with `erlmcp_flow_`
- [ ] Module name describes single responsibility
- [ ] Module location: `apps/erlmcp_flow/src/*.erl`
- [ ] Test module: `apps/erlmcp_flow/test/*_tests.erl` or `*_SUITE.erl`

---

### 1.2 Function Naming Convention

**Rule:** Use `verb_noun` pattern for clarity.

```erlang
% ✅ CORRECT: Clear verb_noun pattern
-export([
    register_agent/3,      % register + agent
    lookup_agent/1,        % lookup + agent
    route_message/2,       % route + message
    balance_load/2,        % balance + load
    detect_failure/1,      % detect + failure
    update_q_table/3       % update + q_table
]).

% ❌ VIOLATION: Unclear naming
-export([
    agent/3,               % What action? Too generic
    msg/2,                 % What about message?
    handle/1,              % Handle what?
    process/2              % Process what?
]).

% ✅ CORRECT: Specific query functions
-export([
    is_agent_healthy/1,
    has_capability/2,
    can_handle_task/2
]).

% ✅ CORRECT: Setters/getters
-export([
    get_agent_load/1,
    set_routing_policy/2,
    get_q_value/2
]).
```

**Checklist:**
- [ ] Functions use clear verb_noun pattern
- [ ] Boolean predicates start with `is_`, `has_`, `can_`
- [ ] Getters/setters clearly named
- [ ] No generic names like `handle/1`, `process/2`

---

### 1.3 Routing Layer Patterns

**Rule:** All routing operations MUST go through `erlmcp_flow_router`.

```erlang
% ✅ CORRECT: Routing through router
route_task(TaskId, Task, Options) ->
    % 1. Lookup available agents
    Agents = erlmcp_flow_registry:lookup_by_capability(
        maps:get(capability, Options)
    ),

    % 2. Select best agent using Q-learning
    AgentId = erlmcp_flow_q_learning:select_action(
        current_state(),
        Agents
    ),

    % 3. Route with load balancing
    case erlmcp_flow_load_balancer:route(AgentId, Task, Options) of
        {ok, CorrelationId} ->
            erlmcp_flow_correlation_tracker:track(TaskId, CorrelationId),
            {ok, AgentId, CorrelationId};
        {error, backpressure} ->
            % Retry with backoff
            erlmcp_flow_router:retry_with_backoff(TaskId, Task, Options);
        {error, agent_unavailable} ->
            % Fallback to alternate agent
            erlmcp_flow_router:fallback_routing(TaskId, Task, Options)
    end.

% ❌ VIOLATION: Direct agent messaging bypasses routing
send_task_directly(AgentPid, Task) ->
    AgentPid ! {task, Task}.  % WRONG: No load balancing, no correlation, no failure handling
```

**Checklist:**
- [ ] All task routing goes through `erlmcp_flow_router`
- [ ] No direct `Pid ! Message` to agents (use router)
- [ ] Load balancing applied to all routes
- [ ] Correlation IDs tracked for all requests
- [ ] Failure handling (retry, fallback, circuit breaker)

---

### 1.4 Registry Operations (gproc)

**Rule:** Use gproc for O(log N) agent lookups. Never use process dictionary or ETS for agent registry.

```erlang
% ✅ CORRECT: gproc-based registry
register_agent(AgentId, Pid, Metadata) ->
    % Register name: O(log N) lookup
    gproc:reg({n, l, {flow_agent, AgentId}}, Pid),

    % Index by type: O(log N) property lookup
    Type = maps:get(type, Metadata),
    gproc:reg({p, l, {flow_agent_type, Type}}, Pid),

    % Index by capabilities: O(M) where M = capabilities count
    Capabilities = maps:get(capabilities, Metadata, []),
    [gproc:reg({p, l, {flow_capability, Cap}}, Pid) || Cap <- Capabilities],

    % Load counter for load balancing
    gproc:reg({c, l, {flow_agent_load, AgentId}}, 0),

    ok.

lookup_agent(AgentId) ->
    % O(log N) lookup
    case gproc:where({n, l, {flow_agent, AgentId}}) of
        undefined -> {error, not_found};
        Pid -> {ok, Pid}
    end.

lookup_by_capability(Capability) ->
    % Find all agents with capability
    gproc:lookup_pids({p, l, {flow_capability, Capability}}).

% ❌ VIOLATION: Using process dictionary
register_agent_bad(AgentId, Pid) ->
    put({agent, AgentId}, Pid).  % WRONG: Not O(log N), not cleaned on crash

% ❌ VIOLATION: Using ETS directly
register_agent_bad2(AgentId, Pid) ->
    ets:insert(agents_table, {AgentId, Pid}).  % WRONG: Manual cleanup required
```

**Checklist:**
- [ ] All agent registration uses gproc
- [ ] Agent names: `{n, l, {flow_agent, AgentId}}`
- [ ] Type indexing: `{p, l, {flow_agent_type, Type}}`
- [ ] Capability indexing: `{p, l, {flow_capability, Cap}}`
- [ ] Load counters: `{c, l, {flow_agent_load, AgentId}}`
- [ ] No process dictionary usage for registry
- [ ] No direct ETS manipulation for registry

---

### 1.5 Agent Lifecycle Management

**Rule:** All agents supervised by `erlmcp_flow_agent_sup` (simple_one_for_one).

```erlang
% ✅ CORRECT: Supervised agent start
start_agent(AgentId, Config) ->
    supervisor:start_child(
        erlmcp_flow_agent_sup,
        [AgentId, Config]
    ).

% erlmcp_flow_agent.erl
-module(erlmcp_flow_agent).
-behaviour(gen_server).

init([AgentId, Config]) ->
    % Fast init - no blocking
    self() ! initialize_async,
    {ok, #state{
        agent_id = AgentId,
        config = Config,
        initialized = false
    }}.

handle_info(initialize_async, State = #state{agent_id = AgentId, config = Config}) ->
    % Register with flow registry
    ok = erlmcp_flow_registry:register_agent(
        AgentId,
        self(),
        Config
    ),

    % Subscribe to relevant topics
    Topics = maps:get(topics, Config, []),
    [erlmcp_flow_router:subscribe_topic(Topic) || Topic <- Topics],

    {noreply, State#state{initialized = true}};

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    % Handle monitored process death
    % Let supervisor restart this agent
    {stop, dependency_died, State}.

terminate(_Reason, State = #state{agent_id = AgentId}) ->
    % Graceful cleanup
    erlmcp_flow_registry:unregister_agent(AgentId),
    ok.

% ❌ VIOLATION: Unsupervised agent
start_agent_bad(AgentId, Config) ->
    spawn(fun() -> agent_loop(AgentId, Config) end).  % WRONG: No supervision
```

**Checklist:**
- [ ] All agents started via `supervisor:start_child`
- [ ] Agents supervised by `erlmcp_flow_agent_sup`
- [ ] `init/1` never blocks (async initialization)
- [ ] Agent registers with `erlmcp_flow_registry` in `handle_info`
- [ ] `terminate/2` unregisters from registry
- [ ] No unsupervised `spawn` calls

---

### 1.6 Q-Learning Integration

**Rule:** Q-Learning MUST be used for agent selection in routing.

```erlang
% ✅ CORRECT: Q-Learning based routing
route_with_q_learning(State, AvailableAgents, Task) ->
    % Get current Q-values for all agents
    QValues = [
        {Agent, erlmcp_flow_q_learning:get_q_value(State, Agent)}
        || Agent <- AvailableAgents
    ],

    % Epsilon-greedy exploration
    Epsilon = 0.1,
    case rand:uniform() < Epsilon of
        true ->
            % Explore: random agent
            lists:nth(rand:uniform(length(AvailableAgents)), AvailableAgents);
        false ->
            % Exploit: best agent by Q-value
            {BestAgent, _MaxQ} = lists:max(
                fun({_, Q1}, {_, Q2}) -> Q1 >= Q2 end,
                QValues
            ),
            BestAgent
    end.

% Update Q-value after task completion
update_q_learning(State, Agent, Outcome) ->
    Reward = case Outcome of
        {success, Latency} when Latency < 50 -> 1.0;      % Fast success
        {success, Latency} when Latency < 100 -> 0.5;     % Acceptable
        {success, _} -> 0.0;                              % Slow
        {error, _} -> -1.0                                % Failure
    end,

    erlmcp_flow_q_learning:update(State, Agent, Reward).

% ❌ VIOLATION: No learning, static routing
route_static(AvailableAgents, _Task) ->
    % WRONG: Always picks first agent, no learning
    hd(AvailableAgents).
```

**Checklist:**
- [ ] Q-Learning used for agent selection
- [ ] Epsilon-greedy exploration (ε=0.1)
- [ ] Q-values updated based on task outcomes
- [ ] Reward function considers latency + success
- [ ] Q-table persisted periodically
- [ ] No hardcoded agent selection

---

### 1.7 Load Balancing Policies

**Rule:** Support multiple load balancing strategies.

```erlang
% ✅ CORRECT: Multiple strategies
-type lb_strategy() :: round_robin | least_connections | weighted | consistent_hash.

balance_load(Agents, Task, Strategy) ->
    case Strategy of
        round_robin ->
            % Simple round-robin
            Index = erlang:phash2(erlang:monotonic_time(), length(Agents)),
            lists:nth(Index + 1, Agents);

        least_connections ->
            % Pick agent with lowest load
            LoadsWithAgents = [
                {erlmcp_flow_registry:get_agent_load(A), A}
                || A <- Agents
            ],
            {_MinLoad, Agent} = lists:min(LoadsWithAgents),
            Agent;

        weighted ->
            % Weighted by Q-values
            select_weighted_by_q(Agents);

        consistent_hash ->
            % Consistent hashing for sticky sessions
            TaskHash = erlang:phash2(maps:get(task_id, Task)),
            AgentHash = TaskHash rem length(Agents),
            lists:nth(AgentHash + 1, Agents)
    end.

% ❌ VIOLATION: Single hardcoded strategy
balance_load_bad(Agents, _Task) ->
    hd(Agents).  % WRONG: Always first agent
```

**Checklist:**
- [ ] Multiple load balancing strategies supported
- [ ] `round_robin` implemented
- [ ] `least_connections` uses load counters
- [ ] `weighted` uses Q-values
- [ ] `consistent_hash` for sticky sessions
- [ ] Strategy configurable per task type

---

### 1.8 Failure Handling & Circuit Breaker

**Rule:** All agent communication MUST have retry, fallback, and circuit breaker.

```erlang
% ✅ CORRECT: Comprehensive failure handling
route_with_failure_handling(Task, Options) ->
    MaxRetries = maps:get(max_retries, Options, 3),
    route_with_retry(Task, MaxRetries, Options).

route_with_retry(_Task, 0, _Options) ->
    {error, max_retries_exceeded};

route_with_retry(Task, RetriesLeft, Options) ->
    AgentId = select_agent(Task, Options),

    % Check circuit breaker
    case erlmcp_flow_circuit_breaker:is_open(AgentId) of
        true ->
            % Circuit open, try fallback
            FallbackAgent = select_fallback_agent(Task, Options),
            route_to_agent(FallbackAgent, Task, Options);
        false ->
            % Circuit closed, attempt routing
            case route_to_agent(AgentId, Task, Options) of
                {ok, Result} ->
                    erlmcp_flow_circuit_breaker:record_success(AgentId),
                    {ok, Result};

                {error, timeout} ->
                    erlmcp_flow_circuit_breaker:record_failure(AgentId),
                    % Exponential backoff
                    Backoff = (MaxRetries - RetriesLeft + 1) * 100,
                    timer:sleep(Backoff),
                    route_with_retry(Task, RetriesLeft - 1, Options);

                {error, agent_unavailable} ->
                    erlmcp_flow_circuit_breaker:record_failure(AgentId),
                    % Try different agent immediately
                    route_with_retry(Task, RetriesLeft - 1, Options)
            end
    end.

% ❌ VIOLATION: No retry or failure handling
route_no_retry(Task, AgentId) ->
    AgentId ! {task, Task},
    receive
        {result, Result} -> {ok, Result}
    after 5000 ->
        {error, timeout}  % WRONG: No retry, no fallback
    end.
```

**Checklist:**
- [ ] Max 3 retries with exponential backoff
- [ ] Circuit breaker integration
- [ ] Fallback agent selection on failure
- [ ] Timeout handling (default 5000ms)
- [ ] Failure metrics recorded
- [ ] No synchronous blocking without timeout

---

### 1.9 Correlation ID Tracking

**Rule:** All routed tasks MUST have correlation IDs for distributed tracing.

```erlang
% ✅ CORRECT: Correlation ID tracking
route_with_correlation(Task, Options) ->
    % Generate or extract correlation ID
    CorrelationId = case maps:get(correlation_id, Task, undefined) of
        undefined -> uuid:v4();
        CId -> CId
    end,

    % Track in correlation tracker
    TaskId = maps:get(task_id, Task),
    erlmcp_flow_correlation_tracker:track(TaskId, CorrelationId, #{
        start_time => erlang:monotonic_time(microsecond),
        task => Task,
        options => Options
    }),

    % Route with correlation context
    Result = route_to_agent(
        select_agent(Task, Options),
        Task#{correlation_id => CorrelationId},
        Options
    ),

    % Update correlation tracker
    erlmcp_flow_correlation_tracker:complete(CorrelationId, #{
        end_time => erlang:monotonic_time(microsecond),
        result => Result
    }),

    Result.

% ❌ VIOLATION: No correlation tracking
route_no_correlation(Task, AgentId) ->
    AgentId ! {task, Task}.  % WRONG: Lost distributed context
```

**Checklist:**
- [ ] Correlation ID (UUID v4) generated for each task
- [ ] Correlation ID tracked in `erlmcp_flow_correlation_tracker`
- [ ] Start/end times recorded
- [ ] Task metadata captured
- [ ] Result status tracked
- [ ] OpenTelemetry integration for tracing

---

### 1.10 Message Format Standardization

**Rule:** All inter-agent messages MUST follow erlmcp-flow message format.

```erlang
% ✅ CORRECT: Standard message format
-record(flow_message, {
    correlation_id :: binary(),
    task_id :: binary(),
    source_agent_id :: binary(),
    target_agent_id :: binary(),
    method :: binary(),
    params :: map(),
    timestamp :: integer(),
    priority :: low | normal | high | critical,
    timeout :: timeout()
}).

create_message(TaskId, Method, Params, Options) ->
    #flow_message{
        correlation_id = maps:get(correlation_id, Options, uuid:v4()),
        task_id = TaskId,
        source_agent_id = maps:get(source, Options),
        target_agent_id = maps:get(target, Options),
        method = Method,
        params = Params,
        timestamp = erlang:system_time(millisecond),
        priority = maps:get(priority, Options, normal),
        timeout = maps:get(timeout, Options, 5000)
    }.

% ❌ VIOLATION: Inconsistent message format
send_task_bad(AgentId, Task) ->
    % WRONG: Unstructured message
    AgentId ! {do_something, Task, erlang:now()}.
```

**Checklist:**
- [ ] All messages use `#flow_message{}` record
- [ ] Correlation ID present
- [ ] Task ID present
- [ ] Source/target agent IDs present
- [ ] Timestamp in milliseconds
- [ ] Priority level set
- [ ] Timeout configured

---

### 1.11 Performance Benchmarks

**Rule:** All routing components MUST meet performance targets.

```erlang
% ✅ CORRECT: Benchmark integration
-ifdef(BENCHMARK).

bench_registry_lookup() ->
    % Target: p99 < 100μs
    NumLookups = 100000,
    Agents = setup_agents(1000),

    Latencies = [
        begin
            Start = erlang:monotonic_time(microsecond),
            {ok, _} = erlmcp_flow_registry:lookup_agent(
                lists:nth(rand:uniform(1000), Agents)
            ),
            erlang:monotonic_time(microsecond) - Start
        end
        || _ <- lists:seq(1, NumLookups)
    ],

    P99 = percentile(Latencies, 0.99),
    ?assert(P99 < 100),  % p99 < 100μs

    cleanup_agents(Agents).

bench_routing_throughput() ->
    % Target: > 50K route decisions/sec
    NumRoutes = 100000,
    Agents = setup_agents(60),

    Start = erlang:monotonic_time(second),

    [route_task(create_test_task(I)) || I <- lists:seq(1, NumRoutes)],

    Duration = erlang:monotonic_time(second) - Start,
    Throughput = NumRoutes / Duration,

    ?assert(Throughput > 50000),  % > 50K routes/sec

    cleanup_agents(Agents).

-endif.

% ❌ VIOLATION: No benchmarks
% Missing performance validation
```

**Checklist:**
- [ ] Registry lookup benchmark: p99 < 100μs
- [ ] Routing decision benchmark: p99 < 50ms
- [ ] Throughput benchmark: > 50K routes/sec
- [ ] Memory benchmark: < 512MB for 1000 agents
- [ ] Failover benchmark: < 2s recovery
- [ ] Benchmarks run in CI/CD

---

### 1.12 Byzantine Fault Tolerance

**Rule:** Consensus operations MUST use 3f+1 quorum for Byzantine tolerance.

```erlang
% ✅ CORRECT: Byzantine quorum
consensus_commit(Operation, Cluster) ->
    ClusterSize = length(Cluster),
    ByzantineQuorum = (ClusterSize div 3) * 2 + 1,  % 3f+1 where f = ClusterSize/3

    % Submit to all nodes
    Responses = [
        submit_to_node(Node, Operation)
        || Node <- Cluster
    ],

    % Count successful commits
    SuccessCount = length([R || R <- Responses, R =:= {ok, committed}]),

    if
        SuccessCount >= ByzantineQuorum ->
            {ok, committed};
        true ->
            {error, insufficient_quorum}
    end.

% ❌ VIOLATION: Simple majority (not Byzantine safe)
consensus_commit_bad(Operation, Cluster) ->
    Responses = [submit_to_node(N, Operation) || N <- Cluster],
    SuccessCount = length([R || R <- Responses, R =:= {ok, committed}]),

    % WRONG: Simple majority not Byzantine tolerant
    if
        SuccessCount > length(Cluster) div 2 ->
            {ok, committed};
        true ->
            {error, insufficient_quorum}
    end.
```

**Checklist:**
- [ ] Byzantine quorum: 3f+1 where f = max Byzantine nodes
- [ ] All consensus operations use quorum
- [ ] Leader election uses Byzantine Paxos
- [ ] State machine replication verified
- [ ] No simple majority consensus

---

## 2. OTP Compliance (erlmcp-flow Specific)

Refer to [ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md](./ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md) for:
- gen_server/gen_statem implementation
- Supervision tree design
- Message handling
- Error handling (let-it-crash)
- Monitoring vs linking

**Additional erlmcp-flow requirements:**

### 2.1 erlmcp-flow Supervision

```erlang
% ✅ CORRECT: 3-tier supervision
% TIER 1: Root (one_for_all)
erlmcp_flow_sup → {registry, router, q_learning, failure_detector}

% TIER 2: Agent supervisor (simple_one_for_one)
erlmcp_flow_agent_sup → erlmcp_flow_agent (per-agent isolation)

% TIER 3: Isolated subsystems
erlmcp_flow_observability_sup → {metrics, tracing, circuit_breaker}
```

**Checklist:**
- [ ] 3-tier supervision properly configured
- [ ] Agents isolated via `simple_one_for_one`
- [ ] Registry, router, Q-learning under root supervisor
- [ ] Observability components isolated

---

## 3. Chicago TDD Compliance (erlmcp-flow Specific)

### 3.1 Real Agent Processes

**Rule:** Tests MUST use real `erlmcp_flow_agent` processes, not mocks.

```erlang
% ✅ CORRECT: Real agent testing
test_routing_to_real_agents() ->
    % Start real agents
    {ok, Agent1} = erlmcp_flow_agent_sup:start_child([agent1, #{type => <<"test">>}]),
    {ok, Agent2} = erlmcp_flow_agent_sup:start_child([agent2, #{type => <<"test">>}]),

    % Test routing
    Task = #{task_id => <<"task1">>, method => <<"execute">>},
    {ok, AgentId} = erlmcp_flow_router:route_task(Task),

    % Verify through observable behavior
    ?assert(lists:member(AgentId, [agent1, agent2])),

    % Cleanup
    erlmcp_flow_agent:stop(Agent1),
    erlmcp_flow_agent:stop(Agent2).

% ❌ VIOLATION: Mock usage
test_routing_with_mock() ->
    meck:new(erlmcp_flow_registry),
    meck:expect(erlmcp_flow_registry, lookup_agent, fun(_) -> {ok, self()} end),
    % WRONG: Not testing real registry behavior
    ...
    meck:unload(erlmcp_flow_registry).
```

**Checklist:**
- [ ] All tests use real `erlmcp_flow_agent` processes
- [ ] No meck, mock, stub frameworks
- [ ] Real `erlmcp_flow_registry` with gproc
- [ ] Real `erlmcp_flow_router` with Q-learning
- [ ] Proper cleanup after tests

---

### 3.2 Observable Behavior Only

**Rule:** Test routing outcomes, not internal Q-table state.

```erlang
% ✅ CORRECT: Observable behavior
test_q_learning_improves_routing() ->
    Agents = setup_test_agents(5),

    % Simulate 100 tasks with varying agent performance
    Results = [
        begin
            Task = create_test_task(I),
            {ok, AgentId} = erlmcp_flow_router:route_task(Task),
            {ok, Latency} = execute_and_measure(AgentId, Task),
            {AgentId, Latency}
        end
        || I <- lists:seq(1, 100)
    ],

    % Verify: Later tasks routed to faster agents
    FirstHalfAvgLatency = avg_latency(lists:sublist(Results, 1, 50)),
    SecondHalfAvgLatency = avg_latency(lists:sublist(Results, 51, 50)),

    ?assert(SecondHalfAvgLatency < FirstHalfAvgLatency * 0.8),  % 20% improvement

    cleanup_agents(Agents).

% ❌ VIOLATION: State inspection
test_q_table_state() ->
    % WRONG: Testing internal Q-table state
    {status, _, _, [_, _, _, _, Misc]} = sys:get_status(erlmcp_flow_q_learning),
    State = proplists:get_value(state, lists:last(Misc)),
    QTable = State#state.q_table,
    ?assert(maps:size(QTable) > 0).  % Testing implementation detail
```

**Checklist:**
- [ ] Tests measure routing latency improvements
- [ ] Tests verify agent selection correctness
- [ ] Tests check failure recovery
- [ ] No `sys:get_status` inspection
- [ ] No Q-table state inspection

---

## 4. Performance Quality Gates

### 4.1 Mandatory Performance Targets

| Metric | Target | Gate | Enforcement |
|--------|--------|------|-------------|
| Registry lookup (p99) | < 100μs | BLOCKING | Benchmark |
| Routing decision (p99) | < 50ms | BLOCKING | Benchmark |
| Task throughput | > 50K/sec | BLOCKING | Benchmark |
| Failover time | < 2s | BLOCKING | Chaos test |
| Memory (1000 agents) | < 512MB | BLOCKING | Benchmark |
| Message latency (p99) | < 50ms | BLOCKING | Integration test |
| Zero task loss | 100% | BLOCKING | Chaos test |

**Enforcement:**

```bash
# Run performance benchmarks
rebar3 eunit --module erlmcp_flow_bench

# Expected output:
# ✅ Registry lookup p99: 95μs (target: <100μs)
# ✅ Routing decision p99: 48ms (target: <50ms)
# ✅ Throughput: 52K routes/sec (target: >50K/sec)
# ✅ Failover: 1.8s (target: <2s)
# ✅ Memory: 487MB for 1000 agents (target: <512MB)
# ✅ Zero task loss: 100% (target: 100%)
```

**Checklist:**
- [ ] All performance benchmarks pass
- [ ] p99 latencies under target
- [ ] Throughput meets target
- [ ] Memory within bounds
- [ ] Zero task loss verified
- [ ] Benchmarks run in CI/CD

---

## 5. Anti-Patterns (BLOCKING)

### 5.1 Routing Deadlocks

```erlang
% ❌ VIOLATION: Circular routing dependency
route_to_agent(AgentId, Task) ->
    gen_server:call(AgentId, {execute, Task}),  % Blocks
    route_result_back().  % DEADLOCK if agent tries to route during execution

% ✅ CORRECT: Async routing
route_to_agent(AgentId, Task) ->
    gen_server:cast(AgentId, {execute, Task}).  % Non-blocking
```

### 5.2 Unbounded Agent Spawn

```erlang
% ❌ VIOLATION: Unbounded agent creation
handle_call({create_agent, Config}, _From, State) ->
    {ok, Pid} = spawn_agent(Config),  % No limit!
    {reply, {ok, Pid}, State}.

% ✅ CORRECT: Bounded agent pool
handle_call({create_agent, Config}, _From, State = #state{agent_count = Count}) ->
    if
        Count >= ?MAX_AGENTS ->
            {reply, {error, max_agents_reached}, State};
        true ->
            {ok, Pid} = spawn_agent(Config),
            {reply, {ok, Pid}, State#state{agent_count = Count + 1}}
    end.
```

### 5.3 State Leaks in Registry

```erlang
% ❌ VIOLATION: Manual cleanup on agent death
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % WRONG: Manual cleanup error-prone
    NewAgents = maps:filter(fun(_, P) -> P =/= Pid end, State#state.agents),
    {noreply, State#state{agents = NewAgents}}.

% ✅ CORRECT: gproc auto-cleanup
% NO CODE NEEDED - gproc automatically removes dead processes
```

**Checklist:**
- [ ] No synchronous routing calls (use cast)
- [ ] Agent pool size bounded
- [ ] gproc used for automatic cleanup
- [ ] No manual process tracking
- [ ] No circular routing dependencies

---

## 6. Code Review Approval Checklist

**Before approving ANY erlmcp-flow PR:**

### 6.1 Module Structure
- [ ] Module named `erlmcp_flow_*`
- [ ] Functions use `verb_noun` pattern
- [ ] Module location: `apps/erlmcp_flow/src/`
- [ ] Tests in `apps/erlmcp_flow/test/`

### 6.2 Routing Layer
- [ ] All routing through `erlmcp_flow_router`
- [ ] gproc used for registry (O(log N))
- [ ] Load balancing strategy applied
- [ ] Correlation IDs tracked
- [ ] Failure handling (retry, fallback, circuit breaker)

### 6.3 OTP Compliance
- [ ] gen_server/supervisor behaviors
- [ ] Supervision tree properly configured
- [ ] init/1 never blocks
- [ ] terminate/2 implements cleanup
- [ ] All processes supervised

### 6.4 Q-Learning
- [ ] Q-Learning used for agent selection
- [ ] Epsilon-greedy exploration (ε=0.1)
- [ ] Q-values updated after task completion
- [ ] Reward function considers latency + success
- [ ] Q-table persisted periodically

### 6.5 Performance
- [ ] Benchmarks pass all targets
- [ ] Registry lookup p99 < 100μs
- [ ] Routing decision p99 < 50ms
- [ ] Throughput > 50K routes/sec
- [ ] Memory < 512MB for 1000 agents
- [ ] Zero task loss in chaos tests

### 6.6 Testing
- [ ] Chicago TDD (real processes, no mocks)
- [ ] Observable behavior tested
- [ ] Coverage ≥ 85%
- [ ] Property-based tests for invariants
- [ ] Chaos engineering tests for reliability

### 6.7 Documentation
- [ ] Module @doc present
- [ ] All exported functions have -spec
- [ ] Type definitions exported
- [ ] Examples provided
- [ ] Performance characteristics documented

---

## 7. Final Approval Gate

**DO NOT APPROVE unless ALL of the following are TRUE:**

```bash
# Gate 1: Compilation
cd apps/erlmcp_flow && TERM=dumb rebar3 compile
# Expected: 0 errors

# Gate 2: Tests
rebar3 eunit --app erlmcp_flow
rebar3 ct --dir apps/erlmcp_flow/test
# Expected: 0 failures

# Gate 3: Coverage
rebar3 cover --verbose
# Expected: erlmcp_flow coverage ≥ 85%

# Gate 4: Dialyzer
rebar3 dialyzer
# Expected: 0 type errors

# Gate 5: Xref
rebar3 xref
# Expected: 0 undefined functions

# Gate 6: Performance Benchmarks
rebar3 eunit --module erlmcp_flow_bench
# Expected: All targets met

# Gate 7: Chaos Tests
rebar3 ct --suite erlmcp_flow_chaos_SUITE
# Expected: Zero task loss, failover < 2s
```

**If ANY gate fails → BLOCK merge.**

---

## 8. References

### Internal Documentation
- [ERLMCP_FLOW_QUALITY_STANDARDS.md](./ERLMCP_FLOW_QUALITY_STANDARDS.md)
- [ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md](./ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md)
- [ERLMCP_FLOW_TEST_DESIGN.md](./ERLMCP_FLOW_TEST_DESIGN.md)
- [ERLMCP_FLOW_ROUTING_LAYER_DESIGN.md](./ERLMCP_FLOW_ROUTING_LAYER_DESIGN.md)

### External References
- [Growing Object-Oriented Software, Guided by Tests](http://www.growing-object-oriented-software.com/) - Chicago TDD
- [Erlang OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
- [gproc Documentation](https://github.com/uwiger/gproc)
- [Byzantine Fault Tolerance](https://en.wikipedia.org/wiki/Byzantine_fault) - Consensus safety

---

**Document Version:** 1.0.0
**Last Updated:** 2026-02-02
**Maintained By:** erlmcp-flow Core Team
**Review Cycle:** Quarterly

**CODE LIKE A JOE ARMSTRONG AGI SWARM!!!**
