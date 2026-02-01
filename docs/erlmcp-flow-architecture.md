# erlmcp-flow: Agent Coordination Transport Architecture

**Version**: 1.0.0
**Date**: 2026-02-01
**Status**: Design Phase

---

## Executive Summary

erlmcp-flow extends the erlmcp transport layer to enable efficient routing and coordination of 60+ agents with O(log N) registry lookups, agent-to-agent messaging (direct, broadcast, gossip), and integration with existing erlmcp transports (stdio, TCP, HTTP).

**Core Design Principles**:
- **O(log N) routing** via gproc registry (already used in erlmcp)
- **Process-per-agent** following erlmcp's process-per-connection pattern
- **Behavior-driven** extending `-behaviour(erlmcp_transport)`
- **Supervised isolation** following 3-tier supervision pattern
- **Flow control** with backpressure and message serialization

---

## 1. Architecture Overview

### 1.1 Component Hierarchy

```
erlmcp_flow_sup (one_for_all)
├── erlmcp_flow_registry (gen_server + gproc)
│   ├── Agent registration: O(log N) lookup
│   ├── Capability indexing: {agent_type, capabilities}
│   └── Health monitoring: automatic cleanup on crash
│
├── erlmcp_flow_router (gen_server)
│   ├── Direct messaging: point-to-point
│   ├── Broadcast: topic-based pub/sub
│   ├── Gossip: eventual consistency
│   └── Backpressure: flow control
│
├── erlmcp_flow_agent_sup (simple_one_for_one)
│   └── erlmcp_flow_agent (gen_server)
│       ├── Implements erlmcp_transport behavior
│       ├── Agent-specific logic
│       └── Message handling
│
└── erlmcp_flow_transport_bridge (gen_server)
    ├── stdio integration
    ├── TCP integration
    └── HTTP integration
```

### 1.2 Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| **gproc for registry** | Already used in erlmcp, O(log N) lookup, auto-cleanup |
| **gen_server per agent** | Process isolation, crash independence, let-it-crash |
| **Behavior extension** | Consistent with erlmcp_transport pattern |
| **Topic-based pub/sub** | Efficient broadcast without N² message passing |
| **Binary serialization** | Use existing JSON-RPC infrastructure |

---

## 2. Registry Design: O(log N) Agent Lookups

### 2.1 Agent Registration Schema

```erlang
%% gproc key structure for O(log N) lookup
-type agent_id() :: binary().
-type agent_type() :: binary().  % e.g., <<"erlang-otp-developer">>
-type capability() :: binary().  % e.g., <<"gen_server">>, <<"supervisor">>

%% Registration keys
{n, l, {flow_agent, AgentId}}              % Name lookup
{p, l, {flow_agent_type, AgentType}}       % Type-based indexing
{p, l, {flow_capability, Capability}}      % Capability-based indexing
{c, l, {flow_agent_load, AgentId}}         % Load counter for balancing

%% Example registrations
gproc:reg({n, l, {flow_agent, <<"agent-001">>}}, AgentPid)
gproc:reg({p, l, {flow_agent_type, <<"erlang-otp-developer">>}}, AgentPid)
gproc:reg({p, l, {flow_capability, <<"gen_server">>}}, AgentPid)
gproc:reg({c, l, {flow_agent_load, <<"agent-001">>}}, 0)
```

### 2.2 Lookup Operations

```erlang
%% O(log N) direct lookup by agent ID
find_agent(AgentId) ->
    case gproc:where({n, l, {flow_agent, AgentId}}) of
        undefined -> {error, not_found};
        Pid -> {ok, Pid}
    end.

%% O(M log N) lookup by type (M = agents of type)
find_agents_by_type(AgentType) ->
    gproc:lookup_pids({p, l, {flow_agent_type, AgentType}}).

%% O(C log N) lookup by capability (C = agents with capability)
find_agents_by_capability(Capability) ->
    gproc:lookup_pids({p, l, {flow_capability, Capability}}).

%% Load-balanced selection
find_least_loaded_agent(AgentType) ->
    Agents = find_agents_by_type(AgentType),
    lists:min([{gproc:get_value({c, l, {flow_agent_load, AgentId}}), Pid}
               || Pid <- Agents, AgentId <- [agent_id_from_pid(Pid)]]).
```

### 2.3 Performance Analysis

| Operation | Complexity | Implementation |
|-----------|-----------|----------------|
| Register agent | O(log N) | gproc:reg/2 |
| Unregister agent | O(log N) | gproc:unreg/1 |
| Find by ID | O(log N) | gproc:where/1 |
| Find by type | O(M log N) | gproc:lookup_pids/1 + filter |
| Find by capability | O(C log N) | gproc:lookup_pids/1 + filter |
| Update load counter | O(log N) | gproc:update_counter/2 |

**Baseline**: erlmcp_registry achieves 553K msg/s with gproc.

---

## 3. Agent-to-Agent Messaging Patterns

### 3.1 Direct Messaging (Point-to-Point)

```erlang
%% Send message directly to specific agent
-spec send_direct(agent_id(), term()) -> ok | {error, term()}.
send_direct(AgentId, Message) ->
    case find_agent(AgentId) of
        {ok, Pid} ->
            Pid ! {flow_message, direct, self(), Message},
            ok;
        {error, not_found} ->
            {error, agent_not_found}
    end.
```

**Use case**: Task assignment to specific agent (e.g., "agent-17-poka-yoke")

### 3.2 Broadcast Messaging (Pub/Sub)

```erlang
%% Topic-based pub/sub using gproc properties
-spec subscribe_topic(topic()) -> ok.
subscribe_topic(Topic) ->
    gproc:reg({p, l, {flow_topic, Topic}}).

-spec broadcast(topic(), term()) -> ok.
broadcast(Topic, Message) ->
    gproc:send({p, l, {flow_topic, Topic}}, {flow_message, broadcast, Topic, Message}),
    ok.

%% Example: Broadcast to all build agents
broadcast(<<"build">>, {compile_request, Files}).
```

**Use case**: Notify all agents of type (e.g., broadcast compile errors to all build agents)

### 3.3 Gossip Messaging (Eventual Consistency)

```erlang
%% Epidemic-style gossip for state propagation
-spec gossip(term(), gossip_opts()) -> ok.
gossip(State, Opts) ->
    FanOut = maps:get(fan_out, Opts, 3),
    AllAgents = gproc:lookup_pids({p, l, {flow_agent_type, '_'}}),
    RandomAgents = random_sample(AllAgents, FanOut),

    lists:foreach(fun(Pid) ->
        Pid ! {flow_message, gossip, self(), State}
    end, RandomAgents),
    ok.
```

**Use case**: Propagate global state (e.g., system health, circuit breaker status)

### 3.4 Message Routing Table

| Pattern | Latency | Throughput | Use Case |
|---------|---------|------------|----------|
| Direct | O(1) after O(log N) lookup | High | Task assignment |
| Broadcast | O(M) where M = subscribers | Medium | Notifications |
| Gossip | O(F) where F = fan-out | Low | State sync |

---

## 4. Transport Integration

### 4.1 erlmcp_transport Behavior Extension

```erlang
-module(erlmcp_flow_transport).
-behaviour(erlmcp_transport).

%% Standard transport callbacks
-export([init/2, send/2, close/1]).

%% Flow-specific extensions
-export([route_to_agent/2, broadcast_to_type/2, gossip_state/2]).

-record(state, {
    transport_id :: atom(),
    agent_id :: agent_id(),
    agent_type :: agent_type(),
    capabilities :: [capability()],
    buffer = <<>> :: binary(),
    pending_tasks = #{} :: #{task_id() => task()},
    connected = true :: boolean(),
    flow_control :: flow_control_state()
}).

%% Initialize flow transport
init(TransportId, Config) ->
    AgentId = maps:get(agent_id, Config),
    AgentType = maps:get(agent_type, Config),
    Capabilities = maps:get(capabilities, Config, []),

    %% Register with flow registry
    erlmcp_flow_registry:register_agent(AgentId, self(), #{
        type => AgentType,
        capabilities => Capabilities,
        transport_id => TransportId
    }),

    %% Initialize flow control
    FlowControl = init_flow_control(Config),

    {ok, #state{
        transport_id = TransportId,
        agent_id = AgentId,
        agent_type = AgentType,
        capabilities = Capabilities,
        flow_control = FlowControl
    }}.

%% Send message to another agent
send(Data, #state{agent_id = AgentId} = State) ->
    case erlmcp_flow_router:route(AgentId, Data, State#state.flow_control) of
        {ok, NewFlowControl} ->
            {ok, State#state{flow_control = NewFlowControl}};
        {error, backpressure} ->
            {error, {backpressure, retry_after, 100}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Close transport
close(#state{agent_id = AgentId}) ->
    erlmcp_flow_registry:unregister_agent(AgentId),
    ok.
```

### 4.2 Integration with Existing Transports

```erlang
%% Bridge to stdio transport
-module(erlmcp_flow_stdio_bridge).
-behaviour(gen_server).

init([FlowTransportId]) ->
    %% Start underlying stdio transport
    {ok, StdioPid} = erlmcp_transport_stdio:start_link(self(), #{
        transport_id => stdio_for_flow
    }),

    %% Start flow transport
    {ok, FlowPid} = erlmcp_flow_transport:start_link(FlowTransportId, #{
        agent_id => generate_agent_id(),
        agent_type => <<"flow-coordinator">>,
        bridge_transport => StdioPid
    }),

    {ok, #{stdio => StdioPid, flow => FlowPid}}.

%% Route stdio messages to flow transport
handle_info({transport_message, Data}, #{stdio := StdioPid, flow := FlowPid} = State) ->
    %% Decode and route to appropriate agent
    case decode_flow_message(Data) of
        {ok, {TargetAgent, Message}} ->
            erlmcp_flow_router:send_direct(TargetAgent, Message);
        {error, Reason} ->
            logger:warning("Failed to decode flow message: ~p", [Reason])
    end,
    {noreply, State}.
```

### 4.3 Transport Bridging Table

| Source Transport | Target Transport | Bridge Module | Latency Overhead |
|-----------------|------------------|---------------|------------------|
| stdio → flow | stdio → agent | erlmcp_flow_stdio_bridge | ~50μs |
| TCP → flow | TCP → agent | erlmcp_flow_tcp_bridge | ~100μs |
| HTTP → flow | HTTP → agent | erlmcp_flow_http_bridge | ~200μs |
| flow → stdio | agent → stdio | erlmcp_flow_stdio_bridge | ~50μs |

---

## 5. Message Serialization

### 5.1 Flow Message Format

```erlang
%% Extend JSON-RPC 2.0 with flow-specific fields
-type flow_message() :: #{
    jsonrpc := <<"2.0">>,
    id := json_rpc_id(),
    method := binary(),
    params := #{
        %% Standard MCP fields
        _ => _,

        %% Flow-specific fields
        flow => #{
            source_agent := agent_id(),
            target_agent := agent_id() | [agent_id()],
            routing := direct | broadcast | gossip,
            priority := high | normal | low,
            deadline := pos_integer(),  % milliseconds
            trace_id := binary()  % distributed tracing
        }
    }
}.

%% Example: Direct task assignment
#{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,
    <<"method">> => <<"flow/assign_task">>,
    <<"params">> => #{
        <<"task">> => <<"Implement gen_server">>,
        <<"files">> => [<<"src/my_server.erl">>],
        <<"flow">> => #{
            <<"source_agent">> => <<"coordinator-001">>,
            <<"target_agent">> => <<"agent-erlang-otp-developer-01">>,
            <<"routing">> => <<"direct">>,
            <<"priority">> => <<"high">>,
            <<"deadline">> => 30000,
            <<"trace_id">> => <<"trace-abc123">>
        }
    }
}
```

### 5.2 Serialization Pipeline

```erlang
%% Encode flow message
encode_flow_message(Message) ->
    %% 1. Validate message structure
    ok = validate_flow_message(Message),

    %% 2. Add trace ID if missing
    MessageWithTrace = ensure_trace_id(Message),

    %% 3. Serialize using jsx (already used in erlmcp)
    Binary = jsx:encode(MessageWithTrace),

    %% 4. Validate size (16MB limit)
    case byte_size(Binary) =< 16777216 of
        true -> {ok, Binary};
        false -> {error, message_too_large}
    end.

%% Decode flow message
decode_flow_message(Binary) ->
    %% 1. Parse JSON
    case jsx:decode(Binary, [return_maps]) of
        Data when is_map(Data) ->
            %% 2. Extract flow routing info
            case maps:get(<<"params">>, Data, #{}) of
                #{<<"flow">> := FlowInfo} ->
                    {ok, {maps:get(<<"target_agent">>, FlowInfo), Data}};
                _ ->
                    {error, missing_flow_info}
            end;
        _ ->
            {error, invalid_json}
    end.
```

---

## 6. Flow Control & Backpressure

### 6.1 Token Bucket Algorithm

```erlang
-record(flow_control_state, {
    tokens :: non_neg_integer(),
    max_tokens :: pos_integer(),
    refill_rate :: pos_integer(),  % tokens per second
    last_refill :: integer(),  % monotonic time
    pending_queue = queue:new() :: queue:queue()
}).

%% Initialize flow control
init_flow_control(Config) ->
    MaxTokens = maps:get(max_tokens, Config, 1000),
    RefillRate = maps:get(refill_rate, Config, 100),

    #flow_control_state{
        tokens = MaxTokens,
        max_tokens = MaxTokens,
        refill_rate = RefillRate,
        last_refill = erlang:monotonic_time(millisecond)
    }.

%% Consume tokens for sending
consume_tokens(MessageSize, #flow_control_state{} = State) ->
    %% Refill tokens based on elapsed time
    NewState = refill_tokens(State),

    %% Calculate tokens needed (1 token per 1KB)
    TokensNeeded = (MessageSize div 1024) + 1,

    case NewState#flow_control_state.tokens >= TokensNeeded of
        true ->
            %% Consume tokens
            {ok, NewState#flow_control_state{
                tokens = NewState#flow_control_state.tokens - TokensNeeded
            }};
        false ->
            %% Not enough tokens - backpressure
            {error, backpressure}
    end.

%% Refill tokens
refill_tokens(#flow_control_state{} = State) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - State#flow_control_state.last_refill,

    TokensToAdd = (Elapsed * State#flow_control_state.refill_rate) div 1000,
    NewTokens = min(State#flow_control_state.tokens + TokensToAdd,
                    State#flow_control_state.max_tokens),

    State#flow_control_state{
        tokens = NewTokens,
        last_refill = Now
    }.
```

### 6.2 Backpressure Strategies

| Strategy | Trigger | Action | Recovery |
|----------|---------|--------|----------|
| **Token exhaustion** | tokens < needed | Return {error, backpressure} | Auto-refill at rate |
| **Queue depth** | queue > 10000 msgs | Drop lowest priority | Process queue |
| **Memory limit** | heap > 80% | Circuit breaker open | GC + wait |
| **Agent overload** | load counter > 100 | Reject new tasks | Complete tasks |

---

## 7. Performance Targets & Benchmarks

### 7.1 Latency Targets

| Operation | Target (p50) | Target (p95) | Target (p99) |
|-----------|--------------|--------------|--------------|
| Agent lookup | 10μs | 50μs | 100μs |
| Direct message | 100μs | 500μs | 1ms |
| Broadcast (10 agents) | 500μs | 2ms | 5ms |
| Broadcast (60 agents) | 2ms | 10ms | 20ms |
| Gossip propagation | 5ms | 20ms | 50ms |

### 7.2 Throughput Targets

| Metric | Target | Baseline (erlmcp) |
|--------|--------|-------------------|
| Registry lookups/sec | 500K | 553K |
| Direct messages/sec | 100K | N/A |
| Broadcasts/sec | 10K | N/A |
| Gossip messages/sec | 5K | N/A |

### 7.3 Benchmark Suite

```erlang
%% Benchmark agent registration
bench_agent_registration() ->
    %% Register 60 agents
    Agents = [register_agent(I) || I <- lists:seq(1, 60)],

    %% Measure lookup performance
    {Time, _} = timer:tc(fun() ->
        [find_agent(AgentId) || {AgentId, _} <- Agents]
    end),

    AvgLookupTime = Time div 60,
    io:format("Average lookup time: ~pμs~n", [AvgLookupTime]).

%% Benchmark direct messaging
bench_direct_messaging() ->
    Agents = setup_agents(60),
    Messages = 100000,

    {Time, _} = timer:tc(fun() ->
        [send_direct(random_agent(Agents), {task, I}) || I <- lists:seq(1, Messages)]
    end),

    Throughput = (Messages * 1000000) div Time,
    io:format("Direct message throughput: ~p msg/sec~n", [Throughput]).

%% Benchmark broadcast
bench_broadcast() ->
    Agents = setup_agents(60),
    Broadcasts = 10000,

    {Time, _} = timer:tc(fun() ->
        [broadcast(<<"build">>, {compile, I}) || I <- lists:seq(1, Broadcasts)]
    end),

    Throughput = (Broadcasts * 1000000) div Time,
    io:format("Broadcast throughput: ~p broadcast/sec~n", [Throughput]).
```

---

## 8. Implementation Plan

### 8.1 Module Structure

```
apps/erlmcp_flow/
├── src/
│   ├── erlmcp_flow_sup.erl               % Top-level supervisor
│   ├── erlmcp_flow_registry.erl          % Agent registry (gproc wrapper)
│   ├── erlmcp_flow_router.erl            % Message routing logic
│   ├── erlmcp_flow_transport.erl         % Transport behavior implementation
│   ├── erlmcp_flow_agent.erl             % Generic agent gen_server
│   ├── erlmcp_flow_agent_sup.erl         % Agent supervisor
│   ├── erlmcp_flow_stdio_bridge.erl      % stdio integration
│   ├── erlmcp_flow_tcp_bridge.erl        % TCP integration
│   ├── erlmcp_flow_http_bridge.erl       % HTTP integration
│   ├── erlmcp_flow_serializer.erl        % Message encoding/decoding
│   └── erlmcp_flow_backpressure.erl      % Flow control
│
├── test/
│   ├── erlmcp_flow_registry_tests.erl
│   ├── erlmcp_flow_router_tests.erl
│   ├── erlmcp_flow_transport_tests.erl
│   ├── erlmcp_flow_backpressure_tests.erl
│   └── erlmcp_flow_bench.erl             % Benchmark suite
│
└── include/
    └── erlmcp_flow.hrl                   % Type definitions
```

### 8.2 Development Phases

| Phase | Deliverable | Tests | Effort |
|-------|-------------|-------|--------|
| 1. Registry | erlmcp_flow_registry.erl | O(log N) lookup tests | 8h |
| 2. Routing | erlmcp_flow_router.erl | Direct/broadcast/gossip tests | 12h |
| 3. Transport | erlmcp_flow_transport.erl | Behavior compliance tests | 10h |
| 4. Bridges | stdio/tcp/http bridges | Integration tests | 8h |
| 5. Flow Control | erlmcp_flow_backpressure.erl | Backpressure tests | 6h |
| 6. Benchmarks | erlmcp_flow_bench.erl | Performance validation | 4h |
| **Total** | | **Coverage ≥ 82%** | **48h** |

---

## 9. Quality Gates

### 9.1 Compilation
```bash
TERM=dumb rebar3 compile
# Expected: 0 errors
```

### 9.2 Tests
```bash
rebar3 eunit --app erlmcp_flow
rebar3 ct --suite test/erlmcp_flow_*
# Expected: 0 failures, coverage ≥ 82%
```

### 9.3 Performance
```bash
rebar3 eunit --module erlmcp_flow_bench
# Expected:
# - Agent lookup: p50 < 10μs, p95 < 50μs
# - Direct messaging: > 100K msg/sec
# - Broadcast (60 agents): p95 < 10ms
```

### 9.4 Integration
```bash
# Test with real erlmcp transports
rebar3 ct --suite test/erlmcp_flow_integration_SUITE
# Expected: All transports bridge successfully
```

---

## 10. Monitoring & Observability

### 10.1 Metrics

```erlang
%% Integrate with erlmcp_observability
-spec report_metrics() -> ok.
report_metrics() ->
    erlmcp_metrics:gauge(<<"flow.agents.registered">>,
                         length(erlmcp_flow_registry:list_agents())),

    erlmcp_metrics:gauge(<<"flow.messages.pending">>,
                         erlmcp_flow_router:pending_count()),

    erlmcp_metrics:histogram(<<"flow.latency.lookup">>,
                            measure_lookup_latency()),

    erlmcp_metrics:counter(<<"flow.messages.sent">>,
                          erlmcp_flow_router:message_count()).
```

### 10.2 Tracing

```erlang
%% OpenTelemetry integration
-spec trace_message(flow_message()) -> ok.
trace_message(Message) ->
    TraceId = maps:get([<<"params">>, <<"flow">>, <<"trace_id">>], Message),

    otel_tracer:with_span(<<"flow.route_message">>, #{
        <<"trace.id">> => TraceId,
        <<"agent.source">> => maps:get([<<"params">>, <<"flow">>, <<"source_agent">>], Message),
        <<"agent.target">> => maps:get([<<"params">>, <<"flow">>, <<"target_agent">>], Message)
    }, fun() ->
        erlmcp_flow_router:route_message(Message)
    end).
```

---

## 11. Failure Modes & Recovery

### 11.1 Agent Crash

**Scenario**: Agent process crashes during task execution

**Detection**: gproc automatically cleans up registrations

**Recovery**:
1. Supervisor restarts agent (simple_one_for_one)
2. Agent re-registers with same ID
3. Router retries pending messages
4. Tasks marked as failed after 3 retries

### 11.2 Registry Partition

**Scenario**: gproc registry becomes partitioned (distributed mode)

**Detection**: Monitor gproc health via erlmcp_flow_registry

**Recovery**:
1. Detect partition via gproc:info()
2. Trigger re-registration of local agents
3. Gossip protocol propagates state
4. Resolve conflicts via timestamp ordering

### 11.3 Backpressure Overflow

**Scenario**: Message rate exceeds token refill rate

**Detection**: Token bucket empty, queue depth > threshold

**Recovery**:
1. Return {error, backpressure} to sender
2. Sender backs off exponentially
3. Drop lowest priority messages if queue > 10K
4. Emit circuit breaker alert

---

## 12. Migration from Existing Systems

### 12.1 Compatibility with erlmcp

**No breaking changes**:
- erlmcp_flow is a separate application
- Existing transports continue to work
- Optional adoption per agent

**Integration points**:
- Uses erlmcp_registry for server/transport lookup
- Uses erlmcp_json_rpc for serialization
- Uses erlmcp_observability for metrics

### 12.2 Adoption Path

```erlang
%% Step 1: Add erlmcp_flow dependency
{deps, [
    {erlmcp_flow, {git, "https://github.com/seanchatmangpt/erlmcp.git", {branch, "flow"}}}
]}.

%% Step 2: Start erlmcp_flow application
application:start(erlmcp_flow).

%% Step 3: Register agents
erlmcp_flow_registry:register_agent(<<"agent-001">>, self(), #{
    type => <<"erlang-otp-developer">>,
    capabilities => [<<"gen_server">>, <<"supervisor">>]
}).

%% Step 4: Route tasks
erlmcp_flow_router:send_direct(<<"agent-001">>, {task, <<"Build gen_server">>}).
```

---

## 13. Future Enhancements

### 13.1 Distributed Registry (v2.0)

- Multi-node gproc with global scope
- Consistent hashing for agent placement
- Cross-cluster routing

### 13.2 Advanced Routing (v2.1)

- Content-based routing
- Complex event processing
- Workflow orchestration

### 13.3 Machine Learning Integration (v3.0)

- Agent capability learning
- Load prediction
- Adaptive routing

---

## 14. Conclusion

erlmcp-flow provides efficient agent coordination with:

✅ **O(log N) routing** via gproc
✅ **60+ agent support** with linear scaling
✅ **Direct/broadcast/gossip** messaging patterns
✅ **Transport integration** with stdio, TCP, HTTP
✅ **Flow control** with backpressure
✅ **OTP compliance** following erlmcp patterns

**Performance**: Targets 100K+ direct messages/sec, <10ms broadcast latency (p95)
**Reliability**: Let-it-crash supervision, auto-recovery, monitoring
**Compatibility**: Zero breaking changes, optional adoption

**Next Steps**: Implement Phase 1 (Registry) with Chicago TDD, benchmark against targets.
