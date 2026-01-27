# ErlMCP 100x Architecture - Implementation Guide

**Companion to**: `ARCHITECTURE_100X_DESIGN.md`

---

## 1. IMPLEMENTATION OVERVIEW

### Phase-by-Phase Execution

This guide provides practical steps for implementing each phase of the 100x architecture.

**Key Principle**: Each phase is independently testable and can be merged without waiting for other phases.

---

## 2. PHASE 1: SHARD INFRASTRUCTURE (Week 1-2)

### 2.1 Overview

Create the foundation: 64 independent shard managers that handle message routing locally.

### 2.2 Deliverables

1. `src/erlmcp_shard_manager.erl` - Shard process
2. `src/erlmcp_shard_sup.erl` - Shard supervisor
3. `src/erlmcp_shard_registry.erl` - Partitioned ETS registry
4. `test/erlmcp_shard_manager_tests.erl` - Unit tests
5. `test/erlmcp_shard_integration_tests.erl` - Integration tests

### 2.3 Implementation Steps

#### Step 1: Create erlmcp_shard_manager.erl

```erlang
%% Handle these features:
%% 1. Register/unregister connections in local map
%% 2. Local ETS lookup for connection routing
%% 3. Direct erlang:send (fast path)
%% 4. Monitor process crashes
%% 5. Collect stats (queue depth, connection count, latency)

%% Key difference from registry:
%% - Local connections map (only ~234 connections)
%% - No gen_server:cast for routing (use erlang:send directly)
%% - No locking/contention (single shard processes)

-record(state, {
    shard_id :: 0..63,
    connections = #{} :: #{binary() => pid()},
    queue_depth = 0 :: non_neg_integer(),
    last_stats_update :: integer()
}).
```

**Testing Strategy**:
- Create shard with 100 test connections
- Route 10K messages through it
- Measure latency: Should be <100 μs (currently ~100 μs via registry)
- Verify no message loss

#### Step 2: Create erlmcp_shard_sup.erl

```erlang
%% Supervise all 64 shard managers
%% - Simple_one_for_one strategy (independent restarts)
%% - Max restart intensity: 5 per 60 seconds
%% - Shutdown timeout: 5 seconds

%% Key difference:
%% - Shard crash ONLY restarts that shard
%% - Doesn't affect other 63 shards
```

**Testing Strategy**:
- Start supervisor
- Verify all 64 shards start
- Kill one shard, verify it restarts (others unaffected)
- Kill supervisor, verify all 64 restart

#### Step 3: Create erlmcp_shard_registry.erl

```erlang
%% Manage 64 independent ETS tables
%% - Each table: {ClientId => {Pid, Timestamp}}
%% - Create all tables in init_shards()
%% - Routing: hash(ClientId) → shard_id
%% - Lookup: <1ms (direct ETS lookup, no gen_server)

%% Table settings:
%% - public (readers don't block writers)
%% - set (O(1) lookup)
%% - {read_concurrency, true}
%% - {write_concurrency, true}
%% - {decentralized_counters, true}
```

**Testing Strategy**:
- Create all 64 ETS tables
- Insert 10K random records
- Lookup 10K times, measure latency: Should be <1 μs
- Verify distribution across shards: std dev <5%

### 2.4 Backward Compatibility Layer

**Keep old erlmcp_registry.erl but redirect to new shards**:

```erlang
%% Old API
erlmcp_registry:route_to_server(ServerId, TransportId, Message)

%% New implementation
  ShardId = erlmcp_shard_registry:get_shard_id(ServerId),
  ShardPid = erlmcp_shard_sup:get_shard(ShardId),
  erlmcp_shard_manager:route_message(ShardPid, ServerId, Message).
```

**Benefits**:
- No changes needed to clients/servers
- Old code still works
- Can deprecate later in v1.0

### 2.5 Testing Checklist

- [ ] All 64 shards start successfully
- [ ] Connection registration works per shard
- [ ] ETS lookup latency <1 μs
- [ ] Message routing works through shards
- [ ] Process crashes handled correctly
- [ ] No memory leaks (run for 1 hour with steady load)
- [ ] Integration tests pass
- [ ] Backward compat tests pass (old API still works)

---

## 3. PHASE 2: BACKPRESSURE CONTROL (Week 2-3)

### 3.1 Overview

Prevent queue buildup and system collapse under extreme load.

### 3.2 Deliverables

1. `src/erlmcp_token_bucket.erl` - Per-connection rate limiting
2. `src/erlmcp_admission_control.erl` - Per-shard queue depth monitoring
3. `src/erlmcp_circuit_breaker.erl` - Global system health control
4. `test/erlmcp_backpressure_tests.erl` - Load tests with backpressure

### 3.3 Implementation Steps

#### Step 1: erlmcp_token_bucket.erl

```erlang
%% Token bucket algorithm:
%% - Capacity: 1,000 tokens (burst allowance)
%% - Refill rate: 10,000 tokens/sec (configurable)
%% - Refill: elapsed_time * refill_rate

%% Operations:
%% - consume(N): Try to consume N tokens, return {ok, remaining} or {error, rate_limited}
%% - get_state(): Return current state for monitoring

%% Key design:
%% - Refill happens on demand (in consume call)
%% - No periodic timer needed
%% - Accurate and efficient
```

**Testing Strategy**:
- Create token bucket with rate=1000 tokens/sec
- Verify: Can consume 1,000 tokens immediately (burst)
- Verify: After 100ms, can consume ~100 more tokens
- Verify: Rate limiting kicks in when tokens exhausted

#### Step 2: erlmcp_admission_control.erl

```erlang
%% Per-shard queue depth monitoring
%% - Threshold: 10,000 messages per shard
%% - When exceeded: Reject new messages with {error, shard_overloaded}
%% - Per-shard independent (one overloaded shard doesn't kill others)

%% Operations:
%% - check_admission(ShardId): Check if shard can accept more messages
%% - update_queue_depth(ShardId, Delta): Update queue depth
%% - get_shard_stats(ShardId): Get queue depth + latency + throughput
```

**Testing Strategy**:
- Flood one shard with messages
- Verify: Queue depth grows to threshold
- Verify: Further messages rejected
- Verify: Other shards unaffected
- Verify: Backpressure signal propagates to senders

#### Step 3: erlmcp_circuit_breaker.erl

```erlang
%% System-level health monitoring
%% - States: closed (ok), half_open (testing), open (overloaded)
%% - Threshold: 50 failures to open circuit
%% - Recovery: 10 successes to close circuit
%% - Timeout: 30 seconds between state changes

%% Operations:
%% - get_state(): Return current state
%% - allow_test_request(): During half_open, allow limited traffic
%% - record_success(): Count success
%% - record_failure(): Count failure

%% Integration:
%% - Shard admission control records failures
%% - When circuit opens, reject ALL new requests system-wide
%% - Gradual recovery in half_open state
```

**Testing Strategy**:
- Close circuit (fail 50 times)
- Verify state is "open"
- Verify new requests rejected
- Succeed 10 times
- Verify state returns to "closed"

### 3.4 Integration

**Shard manager handles backpressure**:

```erlang
handle_cast({route, ClientId, Message}, State) ->
    %% Check per-connection backpressure
    case erlmcp_token_bucket:consume(ClientId, 1) of
        {ok, _} ->
            %% Check per-shard admission
            case erlmcp_admission_control:check_admission(State#state.shard_id) of
                ok ->
                    %% Check global circuit breaker
                    case erlmcp_circuit_breaker:get_state() of
                        closed ->
                            %% All clear, route message
                            erlang:send(TargetPid, Message);
                        _ ->
                            %% System overloaded, drop message (backpressure)
                            erlmcp_circuit_breaker:record_failure()
                    end;
                {error, shard_overloaded} ->
                    erlmcp_circuit_breaker:record_failure()
            end;
        {error, rate_limited} ->
            %% Rate limited
            ok
    end,
    {noreply, State}.
```

### 3.5 Testing Checklist

- [ ] Token bucket rate limiting works
- [ ] Admission control rejects overloaded shards
- [ ] Circuit breaker state machine works correctly
- [ ] Backpressure prevents queue explosion
- [ ] Load test: 10x normal load with backpressure
- [ ] Recovery test: System recovers after overload
- [ ] Integration test: All three layers working together

---

## 4. PHASE 3: MEMORY OPTIMIZATION (Week 3-4)

### 4.1 Overview

Reduce per-connection memory from 1-2MB to 100-200KB.

### 4.2 Key Optimizations

#### Optimization 1: Process Heap Tuning

```erlang
%% Spawn all connection processes with optimized settings
spawn_opt(Fun, [
    {min_heap_size, 1024},          % 1KB minimum (was default ~2KB)
    {min_bin_vheap_size, 256},      % 256B binary heap (was ~1KB)
    {fullsweep_after, 20},          % Full GC every 20 minor GCs (tune based on profiling)
    {max_heap_size, #{
        size => 512 * 1024,         % Soft limit: 512KB
        error_logger => true,       % Log when exceeded
        kill => true                % Kill if persistent (runaway leak detection)
    }}
]).
```

**Impact**: ~50% reduction in process heap (100-200KB → 50-100KB)

#### Optimization 2: Lazy State Initialization

```erlang
%% Don't allocate all capabilities upfront
-record(connection_state, {
    id :: binary(),
    capabilities = undefined :: undefined | #mcp_capabilities{},
    resources = undefined :: undefined | ets:tid(),  % Reference to shared table
    tools = undefined :: undefined | ets:tid()
}).

%% Fetch capabilities on demand
get_capability(State, Name) ->
    case State#state.capabilities of
        undefined ->
            %% Lazy load from cache
            {ok, Caps} = erlmcp_capability_cache:get(default),
            NewState = State#state{capabilities = Caps},
            {get_capability(NewState, Name), NewState};
        Caps ->
            {maps:get(Name, Caps, undefined), State}
    end.
```

**Impact**: ~30% reduction in per-connection state (50KB → 35KB)

#### Optimization 3: Off-Heap Message Queue

```erlang
%% For connections with deep message queues (>10 messages)
%% Move old messages to ETS temporary table (off-heap)

handle_message_queue(Pid, MessageQueue) when length(MessageQueue) > 10 ->
    %% Move old messages to off-heap storage
    OffHeapTable = erlmcp_message_store:create_temp_table(Pid),

    {OldMessages, RecentMessages} = lists:split(length(MessageQueue) - 5, MessageQueue),

    lists:foreach(fun(Msg) ->
        ets:insert(OffHeapTable, {Pid, erlang:now(), Msg})
    end, OldMessages),

    %% Keep only 5 recent messages in mailbox
    RecentMessages.
```

**Impact**: ~20% reduction for connections with high queue depth

#### Optimization 4: Shared Metadata Cache

```erlang
%% Share common metadata across connections
%% Instead of: Each connection stores full server capabilities (50KB each)
%% Use: Shared ETS table with reference counting

erlmcp_capability_cache:get_or_create(Key) ->
    case ets:lookup(erlmcp_capability_cache, Key) of
        [{Key, Caps, RefCount}] ->
            ets:update_element(erlmcp_capability_cache, Key, {3, RefCount + 1}),
            {ok, Caps};
        [] ->
            %% Create and cache
            Caps = compute_capabilities(),
            ets:insert(erlmcp_capability_cache, {Key, Caps, 1}),
            {ok, Caps}
    end.
```

**Impact**: ~50% reduction for connections sharing metadata

### 4.3 Profiling Strategy

**Before & After Measurement**:

```erlang
%% Baseline: Current architecture
erl -s erlmcp_app start
%% Open 100 connections, measure memory per connection
erlang:memory(system).  % System memory
process_memory(Client1).  % Per-connection memory

%% After Phase 1-3: Optimized
erl -s erlmcp_app start (with optimizations)
%% Same test
%% Should see 5-10x reduction
```

**Target Metrics**:
- Process heap: <30 KB (was 100-200 KB)
- Total per connection: <200 KB (was 1-2 MB)

### 4.4 Testing Checklist

- [ ] Heap size optimization measured
- [ ] Lazy initialization verified (state grows on demand)
- [ ] Off-heap storage working (no memory in heap)
- [ ] Shared metadata cache reduces duplication
- [ ] Profiling: <200KB per connection achieved
- [ ] No memory leaks under sustained load (1 hour test)
- [ ] GC behavior acceptable (<10ms pause)

---

## 5. PHASE 4: FAST PATH IMPLEMENTATION (Week 4-5)

### 5.1 Overview

Reduce message routing latency from 100-700 μs to 10-50 μs.

### 5.2 Current Slow Path

```
1. gen_server:call(registry, lookup)    → 2-5 μs
2. ets:lookup                           → 0.5 μs
3. gen_server:cast(target, Message)     → 2-5 μs (serialization!)
4. Message queue enqueue                → 0.2 μs
5. Target gen_server:handle_cast        → 1-10 μs
─────────────────────────────────────────────────────
Total:                                  → 6-20 μs (local)
Under load (scheduler delay):           → 100-700 μs (bad!)
```

### 5.3 New Fast Path

```
1. ShardId = erlang:phash2(ClientId, 64)     → 0.1 μs (pure computation)
2. ShardPid = get_shard_manager(ShardId)     → 0.1 μs (array lookup)
3. erlang:send(TargetPid, Message)           → 2-5 μs (direct send, no gen_server!)
4. Message queue enqueue                     → 0.2 μs
─────────────────────────────────────────────────────
Total:                                        → 2-6 μs (10-100x faster!)
Under load (no scheduler delay):             → 5-15 μs (predictable!)
```

### 5.4 Implementation

#### Replace gen_server:cast with erlang:send

```erlang
%% OLD (slow)
gen_server:cast(TargetPid, {route, ClientId, Message})

%% NEW (fast)
erlang:send(TargetPid, Message)

%% Key: Shard manager MUST have local routing table
%% So we don't need gen_server:call to lookup target
```

#### Optimize Handler

```erlang
%% OLD erlmcp_client handle_cast
handle_cast({route, ClientId, Message}, State) ->
    %% Complex state updates, logging, etc
    {noreply, NewState}.

%% NEW erlmcp_client handle_info (direct message)
handle_info(Message, State) ->
    %% Fast path: assume message is valid
    %% Minimal processing
    {noreply, State}.
```

#### Use `erlang:send` with `noconnect`

```erlang
%% Avoid remote node connection overhead
erlang:send(TargetPid, Message, [noconnect])

%% If target on local node: Instant
%% If target on remote node: Silently drop (local mode)
```

### 5.5 Batch Message Processing

```erlang
%% For high-throughput scenarios, batch messages together
batch_and_send(Messages, BatchSize, Timeout) ->
    receive
        Message ->
            NewBatch = [Message | CurrentBatch],
            case length(NewBatch) >= BatchSize of
                true ->
                    send_batch(NewBatch),
                    batch_and_send([], BatchSize, Timeout);
                false ->
                    batch_and_send(NewBatch, BatchSize, Timeout)
            after Timeout ->
                case CurrentBatch of
                    [] -> batch_and_send([], BatchSize, Timeout);
                    _ -> send_batch(CurrentBatch), batch_and_send([], BatchSize, Timeout)
                end
            end
    end.

send_batch(Messages) ->
    %% Send 100 messages in single operation
    %% Reduces context switches
    lists:foreach(fun erlang:send/1, Messages).
```

### 5.6 Testing Checklist

- [ ] Direct erlang:send working correctly
- [ ] Fast path latency <15 μs measured
- [ ] Batch processing reduces context switches
- [ ] Throughput improvement verified (5K → 8K msg/sec)
- [ ] No message loss during fast path migration
- [ ] Integration test: Old and new paths coexist

---

## 6. PHASE 5: SUPERVISION ISOLATION (Week 5-6)

### 6.1 Overview

Ensure failures don't cascade across all 15K connections.

### 6.2 New Supervision Tree

```erlang
%% OLD
erlmcp_sup (one_for_all)  ← If ANY component dies, restart everything
├── erlmcp_registry
├── erlmcp_client_sup
└── erlmcp_transport_sup

%% NEW
erlmcp_sup (one_for_one)  ← Components restart independently
├── erlmcp_shard_sup (simple_one_for_one)
│   ├── Shard 0  ← Crash here only restarts THIS shard
│   ├── Shard 1
│   └── Shard 63
├── erlmcp_control_plane_sup (rest_for_one)
│   ├── Health Monitor
│   ├── Recovery Manager
│   ├── Metrics Aggregator
│   └── Circuit Breaker
├── erlmcp_client_sup (simple_one_for_one)
│   └── Clients (legacy API, kept for compat)
└── erlmcp_transport_sup (simple_one_for_one)
    └── Transports
```

### 6.3 Implementation

#### erlmcp_sup.erl (updated)

```erlang
init([]) ->
    SupFlags = #{
        strategy => one_for_one,    % Changed from one_for_all
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% Shard infrastructure (isolated fault domain)
        #{
            id => erlmcp_shard_sup,
            start => {erlmcp_shard_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor
        },

        %% Control plane (can fail independently)
        #{
            id => erlmcp_control_plane_sup,
            start => {erlmcp_control_plane_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor
        },

        %% Legacy components (backward compat)
        #{
            id => erlmcp_client_sup,
            start => {erlmcp_client_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

#### erlmcp_shard_sup.erl (new)

```erlang
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,  % Each shard independent
        intensity => 5,
        period => 60
    },

    ChildSpec = #{
        id => erlmcp_shard_manager,
        start => {erlmcp_shard_manager, start_link, [?SHARD_ID]},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },

    ShardManagers = [
        {shard, erlmcp_shard_manager, [I]} || I <- lists:seq(0, 63)
    ],

    {ok, {SupFlags, [ChildSpec]}}.
```

### 6.4 Failure Scenarios

**Scenario 1: One Shard Fails**
```
1. Shard 47 crashes (e.g., 234 connections lose service)
2. Supervisor restarts Shard 47 (<2 seconds)
3. Other 63 shards unaffected (14,756 connections still ok)
4. Recovery Manager reconnects Shard 47 clients
5. Impact: <1% of connections affected, 2-5 second downtime
```

**Scenario 2: Control Plane Component Fails**
```
1. Health Monitor crashes
2. erlmcp_control_plane_sup restarts it
3. Data path (shards) unaffected
4. Monitoring temporarily offline (non-critical)
5. Impact: No impact on message routing
```

**Scenario 3: Multiple Shards Fail**
```
1. Shard 10, 20, 30 all crash (e.g., cascading error)
2. Each restarts independently (no single point of failure)
3. Other shards continue operating
4. Circuit breaker activates (too many failures)
5. System gracefully degrades (backpressure kicks in)
```

### 6.5 Testing Checklist

- [ ] Single shard failure doesn't affect others
- [ ] Shard restart is fast (<5 seconds)
- [ ] Multiple shard failures handled gracefully
- [ ] Control plane failure doesn't affect data path
- [ ] Recovery Manager reconnects failed clients
- [ ] Integration test: Chaos engineering with random kills

---

## 7. PHASE 6: TESTING & VALIDATION (Week 6-8)

### 7.1 Performance Benchmarks

#### Setup

```erlang
%% Load test framework
run_load_test(ConnectionCount, DurationSeconds, MsgRatePerSec) ->
    %% Create ConnectionCount client processes
    %% Each sends MsgRatePerSec messages
    %% For DurationSeconds
    %% Measure: throughput, latency (p50/p95/p99), memory, GC pauses
```

#### Test Cases

```erlang
%% Test 1: Baseline (current architecture)
test_baseline() ->
    run_load_test(150, 60, 33).  % Current: 150 conns, 5K msg/sec

%% Test 2: Sharding benefit
test_shard_benefit() ->
    run_load_test(1000, 60, 33).  % 1K conns, 33K msg/sec

%% Test 3: Scale to 5K
test_5k_connections() ->
    run_load_test(5000, 300, 33).  % 5K conns, 167K msg/sec, 5 min

%% Test 4: Scale to 10K
test_10k_connections() ->
    run_load_test(10000, 300, 33).  % 10K conns, 333K msg/sec, 5 min

%% Test 5: Full scale (target)
test_15k_connections() ->
    run_load_test(15000, 600, 33).  % 15K conns, 500K msg/sec, 10 min

%% Test 6: Burst test
test_burst_load() ->
    %% Ramp: 0 → 15K connections over 60 seconds
    %% Sustain: 15K connections for 120 seconds
    %% Measure: Connection establishment rate, memory growth
    run_burst_test(15000, 60, 120).

%% Test 7: Latency under load
test_latency_percentiles() ->
    %% Measure p50, p95, p99 latency at each load level
    run_load_test(15000, 300, 33),
    collect_latency_percentiles().
```

#### Expected Results

| Test | Throughput | p95 Latency | Memory | Status |
|------|-----------|-------------|--------|--------|
| Baseline (150c) | 5K msg/s | 5-8ms | 300 MB | ✓ Reference |
| 1K connections | 33K msg/s | 5ms | 100 MB | ✓ Excellent |
| 5K connections | 167K msg/s | 5ms | 500 MB | ✓ Excellent |
| 10K connections | 333K msg/s | 5-10ms | 1.2 GB | ✓ Good |
| 15K connections | 500K msg/s | 10-15ms | 2.2 GB | ✓ Target |

### 7.2 Failure Mode Testing

```erlang
%% Test 1: Shard failure during load
test_shard_failure_recovery() ->
    Start load test (15K connections)
    After 30 seconds: Kill Shard 47
    Measure: Impact, recovery time, data loss
    Expected: <1% connections affected, recovery <5s, no loss

%% Test 2: Cascading failures
test_cascading_failures() ->
    Start load test (15K connections)
    Every 10 seconds: Kill random shard
    Measure: System stability, circuit breaker activation
    Expected: System remains available, graceful degradation

%% Test 3: Memory pressure
test_memory_pressure() ->
    Start load test (15K connections)
    Increase message rate gradually
    Monitor: Memory growth, GC pauses, backpressure activation
    Expected: Memory stays <3GB, GC pauses <20ms
```

### 7.3 Regression Detection

```erlang
%% Create baseline from Phase 1
baseline = run_all_benchmarks().
save_baseline("v1_baseline", baseline).

%% After Phase 6, verify no regression
current = run_all_benchmarks().
regression_check(baseline, current) ->
    check(throughput, <0.95 * baseline_throughput),  % Alert if <5% regression
    check(p95_latency, >1.2 * baseline_latency),     % Alert if >20% regression
    check(memory, >1.1 * baseline_memory).           % Alert if >10% growth
```

### 7.4 Production Readiness Checklist

- [ ] Throughput ≥500K msg/sec at 15K connections
- [ ] p95 latency <50ms under peak load
- [ ] Memory <250KB per connection (avg)
- [ ] 99.9%+ availability (graceful degradation)
- [ ] Zero message loss under backpressure
- [ ] MTTR <30 seconds for single shard failure
- [ ] Backward compatibility 100% (old API still works)
- [ ] Dialyzer zero warnings
- [ ] Test coverage ≥85% for new code
- [ ] Documentation complete (deployment guide, runbooks)

---

## 8. DEPLOYMENT & ROLLOUT

### 8.1 Blue-Green Deployment

```
BLUE (current architecture)      GREEN (new architecture)
├── 15K connections            ├── 15K connections
├── 5K msg/sec                 ├── 500K msg/sec
├── Stable, proven             ├── New, needs validation
└── All traffic here            └── No traffic yet

STEP 1: Test GREEN thoroughly (load tests, chaos engineering)
STEP 2: Route 10% of traffic to GREEN
STEP 3: Monitor for 1-2 days (no issues)
STEP 4: Route 50% of traffic to GREEN
STEP 5: Monitor for 1-2 days (no issues)
STEP 6: Route 100% of traffic to GREEN
STEP 7: Keep BLUE as rollback for 1 week
```

### 8.2 Feature Flags

```erlang
%% Environment variable to control which architecture
application:get_env(erlmcp, architecture_version, current).
%% Values: 'current', '100x'

%% In routing code:
route_message(ClientId, Message) ->
    case application:get_env(erlmcp, architecture_version) of
        current ->
            erlmcp_registry:route_to_server(ClientId, Message);  % Old path
        '100x' ->
            erlmcp_shard_manager:route_message(ClientId, Message)  % New path
    end.
```

### 8.3 Monitoring & Observability

```erlang
%% Metrics to track (OpenTelemetry)
erlmcp_metrics:counter('erlmcp.messages.total', #{
    architecture => current | '100x',
    shard_id => 0..63,
    status => success | dropped | rate_limited
}),

erlmcp_metrics:histogram('erlmcp.latency_ms', Latency, #{
    architecture => current | '100x',
    percentile => p50 | p95 | p99
}),

erlmcp_metrics:gauge('erlmcp.connections', ConnectionCount, #{
    architecture => current | '100x',
    shard_id => 0..63
}),

erlmcp_metrics:gauge('erlmcp.memory_bytes', Memory, #{
    architecture => current | '100x'
}).
```

---

## 9. QUICK REFERENCE: FILE LOCATIONS

### New Files to Create

- `src/erlmcp_shard_manager.erl` (500 lines)
- `src/erlmcp_shard_sup.erl` (100 lines)
- `src/erlmcp_shard_registry.erl` (200 lines)
- `src/erlmcp_token_bucket.erl` (150 lines)
- `src/erlmcp_admission_control.erl` (150 lines)
- `src/erlmcp_circuit_breaker.erl` (180 lines)
- `src/erlmcp_message_store.erl` (optional, off-heap messages)
- `test/erlmcp_shard_manager_tests.erl` (300 lines)
- `test/erlmcp_backpressure_tests.erl` (250 lines)
- `test/erlmcp_chaos_tests.erl` (200 lines)

### Files to Update

- `src/erlmcp_sup.erl` - Update supervision tree
- `src/erlmcp_registry.erl` - Redirect to new shards (backward compat)
- `src/erlmcp_client.erl` - Optional: use fast path
- `src/erlmcp_server.erl` - Optional: use fast path
- `rebar.config` - Add new modules
- `config/sys.config` - Add shard count, backpressure params

---

## 10. ESTIMATED EFFORT

| Phase | Duration | Effort (hours) | Status |
|-------|----------|---|---------|
| 1. Shard Infrastructure | Week 1-2 | 40 | Design complete |
| 2. Backpressure Control | Week 2-3 | 30 | Design complete |
| 3. Memory Optimization | Week 3-4 | 25 | Design complete |
| 4. Fast Path | Week 4-5 | 20 | Design complete |
| 5. Supervision Isolation | Week 5-6 | 20 | Design complete |
| 6. Testing & Validation | Week 6-8 | 30 | Design complete |
| **TOTAL** | **4-5 weeks** | **~165 hours** | **Ready** |

---

## 11. SUCCESS CRITERIA

✓ 100x improvement in concurrent connections (150 → 15,000)
✓ 100x improvement in throughput (5K → 500K msg/sec)
✓ 5-10x improvement in memory efficiency (<200KB per connection)
✓ Consistent <50ms p95 latency at peak load
✓ 99.9%+ availability with graceful degradation
✓ Full backward compatibility (no API breaks)
✓ Production-ready documentation and runbooks

---

**Next Steps**: Begin Phase 1 implementation following this guide.

*For detailed design rationale, see: `/Users/sac/erlmcp/docs/ARCHITECTURE_100X_DESIGN.md`*
