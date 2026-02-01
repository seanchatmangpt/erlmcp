# erlmcp-flow Implementation Plan

**Version**: 1.0.0
**Date**: 2026-02-01
**Methodology**: Chicago TDD + SPARC
**Target**: 48 hours implementation

---

## Phase 1: Registry Implementation (8h)

### 1.1 Create Module Structure

**File**: `apps/erlmcp_flow/src/erlmcp_flow_registry.erl`

```erlang
-module(erlmcp_flow_registry).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    register_agent/3,
    unregister_agent/1,
    find_agent/1,
    find_agents_by_type/1,
    find_agents_by_capability/1,
    find_least_loaded_agent/1,
    list_agents/0,
    update_agent_load/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    agents = #{} :: #{agent_id() => agent_info()}
}).

-type agent_id() :: binary().
-type agent_type() :: binary().
-type capability() :: binary().
-type agent_info() :: #{
    pid := pid(),
    type := agent_type(),
    capabilities := [capability()],
    config := map()
}.
```

### 1.2 Test Plan (TDD)

**File**: `apps/erlmcp_flow/test/erlmcp_flow_registry_tests.erl`

```erlang
-module(erlmcp_flow_registry_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test: Register agent with O(log N) lookup
register_agent_test() ->
    {ok, _Pid} = erlmcp_flow_registry:start_link(),

    AgentId = <<"agent-001">>,
    AgentPid = spawn(fun() -> timer:sleep(infinity) end),

    ok = erlmcp_flow_registry:register_agent(AgentId, AgentPid, #{
        type => <<"erlang-otp-developer">>,
        capabilities => [<<"gen_server">>, <<"supervisor">>]
    }),

    {ok, AgentPid} = erlmcp_flow_registry:find_agent(AgentId),

    exit(AgentPid, kill),
    erlmcp_flow_registry:stop().

%% Test: Find agents by type
find_by_type_test() ->
    {ok, _Pid} = erlmcp_flow_registry:start_link(),

    %% Register 3 agents of same type
    [erlmcp_flow_registry:register_agent(
        list_to_binary("agent-" ++ integer_to_list(I)),
        spawn(fun() -> timer:sleep(infinity) end),
        #{type => <<"erlang-test-engineer">>, capabilities => []}
    ) || I <- lists:seq(1, 3)],

    Agents = erlmcp_flow_registry:find_agents_by_type(<<"erlang-test-engineer">>),
    ?assertEqual(3, length(Agents)),

    erlmcp_flow_registry:stop().

%% Test: gproc auto-cleanup on agent crash
gproc_cleanup_test() ->
    {ok, _Pid} = erlmcp_flow_registry:start_link(),

    AgentId = <<"agent-crash">>,
    AgentPid = spawn(fun() -> timer:sleep(100), exit(crash) end),

    ok = erlmcp_flow_registry:register_agent(AgentId, AgentPid, #{
        type => <<"test">>,
        capabilities => []
    }),

    {ok, AgentPid} = erlmcp_flow_registry:find_agent(AgentId),

    %% Wait for crash
    timer:sleep(200),

    %% gproc should have cleaned up
    {error, not_found} = erlmcp_flow_registry:find_agent(AgentId),

    erlmcp_flow_registry:stop().

%% Benchmark: O(log N) lookup performance
benchmark_lookup_test() ->
    {ok, _Pid} = erlmcp_flow_registry:start_link(),

    %% Register 1000 agents
    Agents = [begin
        AgentId = list_to_binary("agent-" ++ integer_to_list(I)),
        Pid = spawn(fun() -> timer:sleep(infinity) end),
        erlmcp_flow_registry:register_agent(AgentId, Pid, #{
            type => <<"test">>,
            capabilities => []
        }),
        AgentId
    end || I <- lists:seq(1, 1000)],

    %% Measure lookup time
    {Time, _} = timer:tc(fun() ->
        [erlmcp_flow_registry:find_agent(AgentId) || AgentId <- Agents]
    end),

    AvgLookup = Time div 1000,
    ?assert(AvgLookup < 50),  % < 50μs per lookup

    erlmcp_flow_registry:stop().
```

### 1.3 Implementation Checklist

- [ ] Create `erlmcp_flow_registry.erl` with gen_server skeleton
- [ ] Implement `register_agent/3` with gproc integration
- [ ] Implement `find_agent/1` with O(log N) lookup
- [ ] Implement `find_agents_by_type/1` with gproc properties
- [ ] Implement `find_agents_by_capability/1`
- [ ] Implement `find_least_loaded_agent/1` with load counter
- [ ] Write comprehensive EUnit tests
- [ ] Benchmark lookup performance (target: <10μs p50)
- [ ] Verify gproc auto-cleanup on crash
- [ ] Coverage ≥ 82%

**Estimated Time**: 8 hours

---

## Phase 2: Router Implementation (12h)

### 2.1 Create Router Module

**File**: `apps/erlmcp_flow/src/erlmcp_flow_router.erl`

```erlang
-module(erlmcp_flow_router).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    send_direct/2,
    broadcast/2,
    gossip/2,
    subscribe_topic/1,
    unsubscribe_topic/1,
    pending_count/0,
    message_count/0
]).

-record(state, {
    pending_messages = #{} :: #{reference() => pending_message()},
    message_count = 0 :: non_neg_integer(),
    gossip_state = #{} :: map()
}).

-type pending_message() :: #{
    from := pid(),
    to := agent_id(),
    message := term(),
    timestamp := integer(),
    retries := non_neg_integer()
}.
```

### 2.2 Test Plan

**File**: `apps/erlmcp_flow/test/erlmcp_flow_router_tests.erl`

```erlang
-module(erlmcp_flow_router_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test: Direct message routing
direct_message_test() ->
    {ok, _RegPid} = erlmcp_flow_registry:start_link(),
    {ok, _RouterPid} = erlmcp_flow_router:start_link(),

    %% Setup receiver agent
    Receiver = spawn(fun() ->
        receive
            {flow_message, direct, _From, Message} ->
                self() ! {received, Message}
        end
    end),

    erlmcp_flow_registry:register_agent(<<"receiver">>, Receiver, #{
        type => <<"test">>,
        capabilities => []
    }),

    %% Send direct message
    ok = erlmcp_flow_router:send_direct(<<"receiver">>, {hello, world}),

    %% Verify receipt
    receive
        {received, {hello, world}} -> ok
    after 1000 ->
        ?assert(false)
    end.

%% Test: Broadcast to topic
broadcast_test() ->
    {ok, _RegPid} = erlmcp_flow_registry:start_link(),
    {ok, _RouterPid} = erlmcp_flow_router:start_link(),

    %% Setup 3 subscribers
    Subscribers = [spawn(fun() ->
        erlmcp_flow_router:subscribe_topic(<<"build">>),
        receive
            {flow_message, broadcast, <<"build">>, Msg} ->
                self() ! {received, Msg}
        end
    end) || _ <- lists:seq(1, 3)],

    %% Broadcast message
    ok = erlmcp_flow_router:broadcast(<<"build">>, {compile, <<"src/test.erl">>}),

    %% Verify all received
    [receive
        {received, {compile, <<"src/test.erl">>}} -> ok
    after 1000 ->
        ?assert(false)
    end || _ <- Subscribers].

%% Benchmark: Direct message throughput
benchmark_direct_throughput_test() ->
    {ok, _RegPid} = erlmcp_flow_registry:start_link(),
    {ok, _RouterPid} = erlmcp_flow_router:start_link(),

    %% Setup receiver
    Receiver = spawn(fun Loop() ->
        receive
            {flow_message, direct, _From, _Msg} -> Loop()
        end
    end),

    erlmcp_flow_registry:register_agent(<<"receiver">>, Receiver, #{
        type => <<"test">>,
        capabilities => []
    }),

    %% Send 100K messages
    Messages = 100000,
    {Time, _} = timer:tc(fun() ->
        [erlmcp_flow_router:send_direct(<<"receiver">>, {msg, I}) || I <- lists:seq(1, Messages)]
    end),

    Throughput = (Messages * 1000000) div Time,
    io:format("Direct message throughput: ~p msg/sec~n", [Throughput]),
    ?assert(Throughput > 100000).  % > 100K msg/sec
```

### 2.3 Implementation Checklist

- [ ] Create `erlmcp_flow_router.erl` with gen_server
- [ ] Implement `send_direct/2` with registry lookup
- [ ] Implement `broadcast/2` with gproc pub/sub
- [ ] Implement `gossip/2` with random sampling
- [ ] Implement `subscribe_topic/1` and `unsubscribe_topic/1`
- [ ] Add message retry logic (3 attempts)
- [ ] Add pending message tracking
- [ ] Write EUnit tests for all routing patterns
- [ ] Benchmark direct messaging (target: >100K msg/sec)
- [ ] Benchmark broadcast latency (target: <10ms p95 for 60 agents)
- [ ] Coverage ≥ 82%

**Estimated Time**: 12 hours

---

## Phase 3: Transport Behavior Implementation (10h)

### 3.1 Create Transport Module

**File**: `apps/erlmcp_flow/src/erlmcp_flow_transport.erl`

```erlang
-module(erlmcp_flow_transport).
-behaviour(erlmcp_transport).
-behaviour(gen_server).

%% erlmcp_transport callbacks
-export([init/2, send/2, close/1, get_info/1, handle_transport_call/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    transport_id :: atom(),
    agent_id :: agent_id(),
    agent_type :: agent_type(),
    capabilities :: [capability()],
    buffer = <<>> :: binary(),
    pending_tasks = #{} :: #{task_id() => task()},
    flow_control :: flow_control_state(),
    connected = true :: boolean()
}).
```

### 3.2 Test Plan

**File**: `apps/erlmcp_flow/test/erlmcp_flow_transport_tests.erl`

```erlang
-module(erlmcp_flow_transport_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test: Transport initialization
init_transport_test() ->
    {ok, State} = erlmcp_flow_transport:init(test_transport, #{
        agent_id => <<"agent-001">>,
        agent_type => <<"erlang-otp-developer">>,
        capabilities => [<<"gen_server">>]
    }),

    %% Verify agent registered
    {ok, _Pid} = erlmcp_flow_registry:find_agent(<<"agent-001">>),

    erlmcp_flow_transport:close(State).

%% Test: Send message through transport
send_message_test() ->
    {ok, State} = erlmcp_flow_transport:init(test_transport, #{
        agent_id => <<"sender">>,
        agent_type => <<"test">>,
        capabilities => []
    }),

    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"flow/task">>,
        <<"params">> => #{
            <<"task">> => <<"test">>,
            <<"flow">> => #{
                <<"source_agent">> => <<"sender">>,
                <<"target_agent">> => <<"receiver">>,
                <<"routing">> => <<"direct">>
            }
        }
    }),

    {ok, NewState} = erlmcp_flow_transport:send(Message, State),

    erlmcp_flow_transport:close(NewState).

%% Test: get_info compliance
get_info_test() ->
    {ok, State} = erlmcp_flow_transport:init(test_transport, #{
        agent_id => <<"agent-001">>,
        agent_type => <<"test">>,
        capabilities => []
    }),

    Info = erlmcp_flow_transport:get_info(State),

    ?assertEqual(flow, maps:get(type, Info)),
    ?assertEqual(connected, maps:get(status, Info)),

    erlmcp_flow_transport:close(State).
```

### 3.3 Implementation Checklist

- [ ] Create `erlmcp_flow_transport.erl` with dual behaviors
- [ ] Implement `init/2` with agent registration
- [ ] Implement `send/2` with flow control integration
- [ ] Implement `close/1` with cleanup
- [ ] Implement `get_info/1` for observability
- [ ] Implement `handle_transport_call/2` for custom calls
- [ ] Add message serialization/deserialization
- [ ] Write EUnit tests for behavior compliance
- [ ] Test integration with erlmcp_flow_registry
- [ ] Coverage ≥ 82%

**Estimated Time**: 10 hours

---

## Phase 4: Transport Bridges (8h)

### 4.1 Create stdio Bridge

**File**: `apps/erlmcp_flow/src/erlmcp_flow_stdio_bridge.erl`

```erlang
-module(erlmcp_flow_stdio_bridge).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    stdio_transport :: pid(),
    flow_transport :: pid(),
    agent_id :: agent_id()
}).

init([Config]) ->
    %% Start stdio transport
    {ok, StdioPid} = erlmcp_transport_stdio:start_link(self(), #{
        transport_id => stdio_for_flow
    }),

    %% Start flow transport
    {ok, FlowPid} = erlmcp_flow_transport:start_link(flow_transport, #{
        agent_id => maps:get(agent_id, Config, generate_agent_id()),
        agent_type => maps:get(agent_type, Config, <<"flow-coordinator">>),
        capabilities => maps:get(capabilities, Config, []),
        bridge_transport => StdioPid
    }),

    {ok, #state{
        stdio_transport = StdioPid,
        flow_transport = FlowPid,
        agent_id = maps:get(agent_id, Config)
    }}.

%% Route stdio messages to flow
handle_info({transport_message, Data}, #state{flow_transport = FlowPid} = State) ->
    case decode_flow_message(Data) of
        {ok, {TargetAgent, Message}} ->
            erlmcp_flow_router:send_direct(TargetAgent, Message);
        {error, Reason} ->
            logger:warning("Failed to decode flow message: ~p", [Reason])
    end,
    {noreply, State}.
```

### 4.2 Test Plan

**File**: `apps/erlmcp_flow/test/erlmcp_flow_stdio_bridge_tests.erl`

```erlang
-module(erlmcp_flow_stdio_bridge_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test: stdio to flow message routing
stdio_to_flow_test() ->
    {ok, BridgePid} = erlmcp_flow_stdio_bridge:start_link(#{
        agent_id => <<"bridge-agent">>,
        agent_type => <<"coordinator">>,
        capabilities => []
    }),

    %% Setup receiver
    Receiver = spawn(fun() ->
        receive
            {flow_message, direct, _From, Msg} ->
                self() ! {received, Msg}
        end
    end),

    erlmcp_flow_registry:register_agent(<<"receiver">>, Receiver, #{
        type => <<"test">>,
        capabilities => []
    }),

    %% Send message through bridge
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"flow/task">>,
        <<"params">> => #{
            <<"flow">> => #{
                <<"target_agent">> => <<"receiver">>
            }
        }
    }),

    BridgePid ! {transport_message, Message},

    %% Verify receipt
    receive
        {received, _Msg} -> ok
    after 1000 ->
        ?assert(false)
    end.
```

### 4.3 Implementation Checklist

- [ ] Create `erlmcp_flow_stdio_bridge.erl`
- [ ] Create `erlmcp_flow_tcp_bridge.erl`
- [ ] Create `erlmcp_flow_http_bridge.erl`
- [ ] Implement bidirectional message routing
- [ ] Add error handling for malformed messages
- [ ] Write integration tests with real transports
- [ ] Test message encoding/decoding
- [ ] Benchmark bridge overhead (target: <50μs)
- [ ] Coverage ≥ 82%

**Estimated Time**: 8 hours

---

## Phase 5: Flow Control & Backpressure (6h)

### 5.1 Create Backpressure Module

**File**: `apps/erlmcp_flow/src/erlmcp_flow_backpressure.erl`

```erlang
-module(erlmcp_flow_backpressure).
-export([
    init_flow_control/1,
    consume_tokens/2,
    refill_tokens/1,
    queue_message/2,
    dequeue_message/1,
    is_overloaded/1
]).

-record(flow_control_state, {
    tokens :: non_neg_integer(),
    max_tokens :: pos_integer(),
    refill_rate :: pos_integer(),
    last_refill :: integer(),
    pending_queue = queue:new() :: queue:queue(),
    max_queue_size = 10000 :: pos_integer()
}).

init_flow_control(Config) ->
    MaxTokens = maps:get(max_tokens, Config, 1000),
    RefillRate = maps:get(refill_rate, Config, 100),

    #flow_control_state{
        tokens = MaxTokens,
        max_tokens = MaxTokens,
        refill_rate = RefillRate,
        last_refill = erlang:monotonic_time(millisecond)
    }.

consume_tokens(MessageSize, State) ->
    NewState = refill_tokens(State),
    TokensNeeded = (MessageSize div 1024) + 1,

    case NewState#flow_control_state.tokens >= TokensNeeded of
        true ->
            {ok, NewState#flow_control_state{
                tokens = NewState#flow_control_state.tokens - TokensNeeded
            }};
        false ->
            {error, backpressure}
    end.
```

### 5.2 Test Plan

**File**: `apps/erlmcp_flow/test/erlmcp_flow_backpressure_tests.erl`

```erlang
-module(erlmcp_flow_backpressure_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test: Token consumption
consume_tokens_test() ->
    State = erlmcp_flow_backpressure:init_flow_control(#{
        max_tokens => 100,
        refill_rate => 10
    }),

    %% Consume 10 tokens (10KB message)
    {ok, NewState} = erlmcp_flow_backpressure:consume_tokens(10240, State),

    %% Verify tokens decreased
    ?assert(NewState#flow_control_state.tokens < State#flow_control_state.tokens).

%% Test: Backpressure when tokens exhausted
backpressure_test() ->
    State = erlmcp_flow_backpressure:init_flow_control(#{
        max_tokens => 10,
        refill_rate => 1
    }),

    %% Try to consume more than available
    Result = erlmcp_flow_backpressure:consume_tokens(20480, State),

    ?assertEqual({error, backpressure}, Result).

%% Test: Token refill over time
refill_test() ->
    State = erlmcp_flow_backpressure:init_flow_control(#{
        max_tokens => 100,
        refill_rate => 100
    }),

    %% Consume tokens
    {ok, DepletedState} = erlmcp_flow_backpressure:consume_tokens(50240, State),

    %% Wait 1 second
    timer:sleep(1000),

    %% Refill
    RefilledState = erlmcp_flow_backpressure:refill_tokens(DepletedState),

    %% Verify tokens increased
    ?assert(RefilledState#flow_control_state.tokens > DepletedState#flow_control_state.tokens).
```

### 5.3 Implementation Checklist

- [ ] Create `erlmcp_flow_backpressure.erl`
- [ ] Implement token bucket algorithm
- [ ] Implement message queueing
- [ ] Implement priority-based dropping
- [ ] Add circuit breaker integration
- [ ] Write EUnit tests for all scenarios
- [ ] Test under load conditions
- [ ] Coverage ≥ 82%

**Estimated Time**: 6 hours

---

## Phase 6: Benchmarking (4h)

### 6.1 Create Benchmark Suite

**File**: `apps/erlmcp_flow/test/erlmcp_flow_bench.erl`

```erlang
-module(erlmcp_flow_bench).
-export([run_all/0, bench_lookup/0, bench_direct/0, bench_broadcast/0, bench_gossip/0]).

run_all() ->
    bench_lookup(),
    bench_direct(),
    bench_broadcast(),
    bench_gossip().

bench_lookup() ->
    io:format("~n=== Agent Lookup Benchmark ===~n"),

    %% Setup
    {ok, _Pid} = erlmcp_flow_registry:start_link(),
    Agents = setup_agents(1000),

    %% Measure lookup latency
    {Time, _} = timer:tc(fun() ->
        [erlmcp_flow_registry:find_agent(AgentId) || {AgentId, _} <- Agents]
    end),

    AvgLatency = Time div 1000,
    P50 = measure_percentile(Agents, 50),
    P95 = measure_percentile(Agents, 95),
    P99 = measure_percentile(Agents, 99),

    io:format("Average: ~pμs, p50: ~pμs, p95: ~pμs, p99: ~pμs~n",
              [AvgLatency, P50, P95, P99]),

    %% Verify targets
    case P50 < 10 andalso P95 < 50 andalso P99 < 100 of
        true -> io:format("✅ PASS: Lookup performance meets targets~n");
        false -> io:format("❌ FAIL: Lookup performance below targets~n")
    end.

bench_direct() ->
    io:format("~n=== Direct Messaging Benchmark ===~n"),

    %% Setup
    {ok, _RegPid} = erlmcp_flow_registry:start_link(),
    {ok, _RouterPid} = erlmcp_flow_router:start_link(),

    Receiver = spawn(fun Loop() ->
        receive
            {flow_message, direct, _From, _Msg} -> Loop()
        end
    end),

    erlmcp_flow_registry:register_agent(<<"receiver">>, Receiver, #{
        type => <<"test">>,
        capabilities => []
    }),

    %% Send 100K messages
    Messages = 100000,
    {Time, _} = timer:tc(fun() ->
        [erlmcp_flow_router:send_direct(<<"receiver">>, {msg, I}) || I <- lists:seq(1, Messages)]
    end),

    Throughput = (Messages * 1000000) div Time,

    io:format("Throughput: ~p msg/sec~n", [Throughput]),

    %% Verify target
    case Throughput > 100000 of
        true -> io:format("✅ PASS: Direct messaging meets target (>100K msg/sec)~n");
        false -> io:format("❌ FAIL: Direct messaging below target~n")
    end.

bench_broadcast() ->
    io:format("~n=== Broadcast Messaging Benchmark ===~n"),

    %% Setup 60 agents
    {ok, _RegPid} = erlmcp_flow_registry:start_link(),
    {ok, _RouterPid} = erlmcp_flow_router:start_link(),

    Subscribers = [spawn(fun() ->
        erlmcp_flow_router:subscribe_topic(<<"build">>),
        receive
            {flow_message, broadcast, <<"build">>, _Msg} -> ok
        end
    end) || _ <- lists:seq(1, 60)],

    %% Broadcast 1000 messages
    Broadcasts = 1000,
    {Time, _} = timer:tc(fun() ->
        [erlmcp_flow_router:broadcast(<<"build">>, {compile, I}) || I <- lists:seq(1, Broadcasts)]
    end),

    AvgLatency = Time div Broadcasts,

    io:format("Average latency: ~pμs for 60 agents~n", [AvgLatency]),

    %% Verify target (p95 < 10ms = 10000μs)
    case AvgLatency < 10000 of
        true -> io:format("✅ PASS: Broadcast latency meets target (<10ms)~n");
        false -> io:format("❌ FAIL: Broadcast latency above target~n")
    end.
```

### 6.2 Implementation Checklist

- [ ] Create `erlmcp_flow_bench.erl`
- [ ] Implement lookup latency benchmark (p50/p95/p99)
- [ ] Implement direct messaging throughput benchmark
- [ ] Implement broadcast latency benchmark
- [ ] Implement gossip propagation benchmark
- [ ] Add percentile calculations
- [ ] Generate performance report
- [ ] Compare against targets
- [ ] Document results

**Estimated Time**: 4 hours

---

## Quality Gates

### Compilation Gate
```bash
cd apps/erlmcp_flow
TERM=dumb rebar3 compile
# Expected: 0 errors
```

### Test Gate
```bash
rebar3 eunit --app erlmcp_flow
rebar3 ct --dir apps/erlmcp_flow/test
# Expected: 0 failures, coverage ≥ 82%
```

### Performance Gate
```bash
rebar3 eunit --module erlmcp_flow_bench
# Expected:
# - Agent lookup: p50 < 10μs, p95 < 50μs, p99 < 100μs
# - Direct messaging: > 100K msg/sec
# - Broadcast (60 agents): avg latency < 10ms
```

### Integration Gate
```bash
rebar3 ct --suite test/erlmcp_flow_integration_SUITE
# Expected: All transports (stdio, TCP, HTTP) integrate successfully
```

---

## Total Effort Estimate

| Phase | Hours |
|-------|-------|
| 1. Registry | 8 |
| 2. Router | 12 |
| 3. Transport | 10 |
| 4. Bridges | 8 |
| 5. Flow Control | 6 |
| 6. Benchmarks | 4 |
| **Total** | **48** |

---

## Next Steps

1. **Spawn all agents** for parallel implementation (per CLAUDE.md rules)
2. **Create work orders** for each phase
3. **Execute Chicago TDD** for each module
4. **Benchmark continuously** against targets
5. **Document results** in architecture.md

**Expected completion**: 1 week (6 days × 8h/day)
