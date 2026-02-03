# Distributed Memory Synchronization - erlmcp v3 Design

## Architecture Overview

The erlmcp v3 distributed memory system provides cross-agent memory sharing with configurable consistency guarantees, automatic failover, and intelligent recovery mechanisms.

## Table of Contents

1. [Memory Architecture](#memory-architecture)
2. [Persistence Strategies](#persistence-strategies)
3. [Consistency Guarantees](#consistency-guarantees)
4. [Cache Coordination](#cache-coordination)
5. [Eviction Policies](#eviction-policies)
6. [Memory Limits](#memory-limits)
7. [Monitoring Instrumentation](#monitoring-instrumentation)
8. [Recovery Procedures](#recovery-procedures)

---

## Memory Architecture

### 4-Tier Hierarchy

```
┌─────────────────────────────────────────────────────────────────┐
│                         L0: Process Dictionary                  │
│                    (Fastest, Process-Local, ~100ns)            │
├─────────────────────────────────────────────────────────────────┤
│                     L1: ETS (In-Memory)                         │
│                   (Fast, Node-Local, ~1μs)                     │
├─────────────────────────────────────────────────────────────────┤
│                    L2: Mnesia (Replicated)                      │
│              (Moderate, Cluster-Wide, ~10-100μs)                │
├─────────────────────────────────────────────────────────────────┤
│                    L3: External (Redis)                         │
│                  (Slow, Persistent, ~1-5ms)                     │
└─────────────────────────────────────────────────────────────────┘
```

### Memory Domains

| Domain | Purpose | Default Backend | Max Size |
|--------|---------|-----------------|----------|
| `session` | Per-session context | Mnesia | 100MB |
| `shared` | Cross-agent data | Mnesia | 200MB |
| `registry` | Service discovery | ETS | 50MB |
| `cache` | Computed results | ETS | 300MB |
| `ephemeral` | Temporary data | ETS | 50MB |

### Key Modules

- `erlmcp_distributed_memory` - Main memory manager with CRDT synchronization
- `erlmcp_cache_coordinator` - Multi-node cache invalidation
- `erlmcp_persistence_selector` - Automatic backend selection
- `erlmcp_eviction_policy` - LRU/LFU/TTL eviction strategies
- `erlmcp_memory_recovery` - Node failure recovery
- `erlmcp_cache` - 3-tier cache (existing)
- `erlmcp_memory_monitor` - Memory pressure monitoring (existing)
- `erlmcp_memory_guard` - Per-process memory limits (existing)

---

## Persistence Strategies

### Selection Algorithm

```
┌──────────────────────────────────────────────────────────────────┐
│                     Data Size < 100MB?                          │
└──────────────────────────────┬─────────────────────────────────┘
                               │ Yes
                ┌──────────────┴──────────────┐
                │   Read Ratio > 90%?         │
                └──────────────┬──────────────┘
                               │ Yes                    │ No
                    ┌──────────┴──────────┐          │
                    │       ETS           │          │
                    └─────────────────────┘          │
                                                        │
┌───────────────────────────────────────────────────────────────┐
│                    Durability Required?                         │
├─────────────────────────┬─────────────────────────────────────┤
│         None           │         High                           │
│           │            │            │                           │
│          ETS           │    ┌────────┴────────┐                │
│                        │    │  Cluster Topology │                │
│                        │    ├─────────┬────────┤                │
│                        │    │Single   │ Multi  │                │
│                        │    │         │        │                │
│                        │    │  DETS   │Mnesia  │                │
│                        │    └─────────┴────────┘                │
└─────────────────────────────────────────────────────────────────┘
```

### Backend Comparison

| Feature | ETS | DETS | Mnesia |
|---------|-----|------|--------|
| Latency (read) | ~1μs | ~50μs | ~10μs |
| Latency (write) | ~2μs | ~100μs | ~200μs |
| Throughput | 1M ops/s | 50K ops/s | 20K ops/s |
| Durability | None | Disk | Disk + Replicated |
| Cluster Support | No | No | Yes |
| Transaction Support | No | No | Yes |
| Memory Efficiency | High | Medium | Medium |

### Usage Examples

```erlang
%% Auto-select based on workload
Strategy = erlmcp_persistence_selector:recommend_strategy(#{
    data_size => 1024 * 1024,  % 1MB
    read_ratio => 0.95,
    durability_requirement => moderate,
    cluster_topology => multi_node
}).

%% Get recommendations for specific use cases
SessionStrategy = erlmcp_persistence_selector:recommend_for_session(),
CacheStrategy = erlmcp_persistence_selector:recommend_for_cache(),
RegistryStrategy = erlmcp_persistence_selector:recommend_for_registry().

%% Benchmark strategies
Benchmarks = erlmcp_persistence_selector:benchmark_strategies(Characteristics).
```

---

## Consistency Guarantees

### Consistency Levels

| Level | Description | Latency | Use Case |
|-------|-------------|---------|----------|
| `eventual` | Async propagation, no wait | Fastest | Cache, ephemeral data |
| `bounded_stale` | Async with time window | Fast | Session state |
| `strong` | Synchronous replication | Slowest | Critical shared state |

### Vector Clock Synchronization

```erlang
%% Each write increments the node's counter
%% Version = #{node1() => 5, node2() => 3, node3() => 7}

%% Comparison rules:
%% - V1 dominates V2 if all counters >= and at least one >
%% - Concurrent if neither dominates
%% - Equal if all counters match

%% Conflict resolution: Last-Writer-Wins (LWW) with timestamps
```

### Consistency Trade-offs

```
┌─────────────────────────────────────────────────────────────────┐
│                   Consistency vs Latency                        │
├─────────────────────────────────────────────────────────────────┤
│ Strong                                                        │
│   ├───────────────────────────────────────────────────────┐   │
│   │ Consistent: 100%                                      │   │
│   │ Latency: 200-500μs                                    │   │
│   │ Use: Leader election, coordination state             │   │
│   └───────────────────────────────────────────────────────┘   │
│                      ↓                                          │
│ Bounded Stale                                                 │
│   ├───────────────────────────────────────────────────────┐   │
│   │ Consistent: >95% (within time window)                 │   │
│   │ Latency: 50-100μs                                     │   │
│   │ Use: Session state, user preferences                 │   │
│   └───────────────────────────────────────────────────────┘   │
│                      ↓                                          │
│ Eventual                                                      │
│   ├───────────────────────────────────────────────────────┐   │
│   │ Consistent: Eventually (seconds)                      │   │
│   │ Latency: 10-50μs                                      │   │
│   │ Use: Cache, telemetry, metrics                        │   │
│   └───────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

### API Examples

```erlang
%% Set consistency level
erlmcp_distributed_memory:set_consistency(strong).

%% Put with specific consistency
erlmcp_distributed_memory:put(shared, key, value, #{consistency => eventual}).

%% Get with local-only (no remote fetch)
erlmcp_distributed_memory:get(cache, key, #{local_only => true}).
```

---

## Cache Coordination

### Invalidation Protocols

```erlang
%% Synchronous invalidation (wait for all acks)
erlmcp_cache_coordinator:invalidate(Key).

%% Asynchronous invalidation (fire and forget)
erlmcp_cache_coordinator:broadcast_invalidation(Key).

%% Local-only invalidation
erlmcp_cache_coordinator:invalidate_local(Key).

%% Tag-based invalidation
erlmcp_cache_coordinator:invalidate_tag(<<"user_sessions">>).

%% Pattern-based invalidation
erlmcp_cache_coordinator:invalidate_pattern("session:*").
```

### Invalidation Flow

```
Writer                    Coordinator                  Nodes
  │                            │                       │
  │ invalidate(Key)            │                       │
  │───────────────────────────>│                       │
  │                            │ broadcast to all      │
  │                            ├──────────────────────>│
  │                            │<──────────────────────│ ack
  │                            ├──────────────────────>│
  │                            │<──────────────────────│ ack
  │                            │                       │
  │ ok                         │                       │
  │<───────────────────────────│                       │
```

### Coherence Levels

| Level | Description | Behavior |
|-------|-------------|----------|
| `strong` | Wait for all nodes | Synchronous broadcast, track acks |
| `eventual` | Best effort | Async broadcast, no tracking |
| `relaxed` | Optional remote | Only invalidate if requested |

---

## Eviction Policies

### Policy Comparison

| Policy | Time Complexity | Space Complexity | Best For |
|--------|----------------|------------------|----------|
| LRU | O(1) | O(n) | Temporal locality |
| LFU | O(1) | O(n) | Hot data |
| TTL | O(1) | O(1) | Time-sensitive data |
| FIFO | O(1) | O(1) | Sequential access |
| ARC | O(1) | O(c) | Adaptive workloads |
| Random | O(1) | O(1) | Uniform access |

### LRU Implementation

```erlang
%% Track access with timestamps
#{key => value,
  last_accessed => erlang:monotonic_time(microsecond),
  access_count => 0}.

%% Eviction: sort by last_accessed, remove oldest
```

### LFU Implementation

```erlang
%% Track access frequency
#{key => value,
  access_count => 0,
  last_accessed => timestamp}.

%% Eviction: sort by access_count, remove least frequent
```

### ARC Implementation

```erlang
%% Adaptive Replacement Cache
%% Maintains two lists: T1 (recency) and T2 (frequency)
%% Dynamically adjusts size based on workload

-record(arc_state,
        {t1 = queue:new(),  % Recent, unique
         t2 = queue:new(),  % Frequent, unique
         b1 = queue:new(),  % Ghost entries from T1
         b2 = queue:new(),  % Ghost entries from T2
         p = 0,             % Target size for T1
         c => 10000}).       % Total cache size
```

### Policy Selection

```erlang
%% Recommend policy based on access pattern
Characteristics = #{
    access_pattern => temporal,  % temporal | sequential | uniform
    data_size => 1024 * 1024,
    performance => critical
},
Policy = erlmcp_eviction_policy:recommend_policy(Characteristics).

%% Benchmark policies
Benchmarks = erlmcp_eviction_policy:benchmark_policies(Table, 10000).
```

---

## Memory Limits

### Per-Node Limits

```erlang
%% Default limits
#{
    max_bytes => 1024 * 1024 * 1024,  % 1GB
    max_entries => 100000,
    warn_percent => 0.80,
    domain_limits => #{
        session => #{max_bytes => 100 * 1024 * 1024},
        shared => #{max_bytes => 200 * 1024 * 1024},
        registry => #{max_bytes => 50 * 1024 * 1024},
        cache => #{max_bytes => 300 * 1024 * 1024},
        ephemeral => #{max_bytes => 50 * 1024 * 1024}
    }
}.
```

### Cluster-Wide Limits

```erlang
%% Get cluster memory usage
{ok, ClusterMemory} = erlmcp_distributed_memory:get_cluster_memory().

%% ClusterMemory = #{
%%     nodes => 3,
%%     total_memory_bytes => 3 * 1024 * 1024 * 1024,
%%     node_data => [
%%         {node1@host, #{memory_total => ..., processes => ...}},
%%         {node2@host, #{memory_total => ..., processes => ...}},
%%         {node3@host, #{memory_total => ..., processes => ...}}
%%     ]
%% }.
```

### Limit Enforcement

```erlang
%% Memory pressure levels:
%% - low: < 60% of limit
%% - medium: 60-80% of limit
%% - high: 80-90% of limit
%% - critical: > 90% of limit

%% Actions:
%% - medium: Log warning
%% - high: Trigger GC, start eviction
%% - critical: Aggressive eviction, reject writes
```

### Per-Process Limits

```erlang
%% Memory guard (OTP 28+)
erlmcp_memory_guard:enable_context_guard().  % 100MB heap
erlmcp_memory_guard:enable_tool_guard().     % 50MB heap
erlmcp_memory_guard:enable_transport_guard(). % 30MB heap

%% Check usage
{Heap, BinHeap} = erlmcp_memory_guard:get_memory_usage().

%% Validate
{ok, Percent} = erlmcp_memory_guard:validate_memory(context).
```

---

## Monitoring Instrumentation

### Metrics Collected

```erlang
%% Memory stats
#{total => ...,
  processes => ...,
  system => ...,
  ets => ...,
  used_percent => ...}.

%% Cache stats
#{hits => ...,
  misses => ...,
  hit_rate => ...,
  evictions => ...,
  l1_size => ...,
  l2_size => ...}.

%% Sync stats
#{syncs_completed => ...,
  syncs_failed => ...,
  bytes_synced => ...,
  last_sync_time => ...}.
```

### OTEL Integration

```erlang
%% Memory metrics
-define(MEMORY_USED, 'erlmcp.memory.used').
-define(MEMORY_LIMIT, 'erlmcp.memory.limit').
-define(MEMORY_USAGE_RATIO, 'erlmcp.memory.usage_ratio').

%% Cache metrics
-define(CACHE_HITS, 'erlmcp.cache.hits').
-define(CACHE_MISSES, 'erlmcp.cache.misses').
-define(CACHE_HIT_RATE, 'erlmcp.cache.hit_rate').
-define(CACHE_EVICTIONS, 'erlmcp.cache.evictions').

%% Sync metrics
-define(SYNC_DURATION, 'erlmcp.sync.duration').
-define(SYNC_BYTES, 'erlmcp.sync.bytes').
-define(SYNC_ERRORS, 'erlmcp.sync.errors').
```

### Alerting

```erlang
%% Memory pressure alert
#{metric => memory_usage,
  threshold => 0.90,
  window => 60000,  % 1 minute
  action => trigger_eviction}.

%% Cache hit rate alert
#{metric => cache_hit_rate,
  threshold => 0.50,
  window => 300000,  % 5 minutes
  action => alert_operator}.
```

---

## Recovery Procedures

### Recovery Modes

| Mode | Description | Duration | Data Loss |
|------|-------------|----------|-----------|
| `full` | Complete reconstruction | Slowest | None |
| `delta` | Incremental from checkpoint | Medium | Minimal |
| `peer` | Pull from single peer | Fast | Possible |
| `snapshot` | Local checkpoint restore | Fastest | Since checkpoint |
| `auto` | Automatic selection | Varies | Varies |

### Recovery Flow

```
┌─────────────────────────────────────────────────────────────────┐
│ 1. Detection                                                   │
│    - Node monitor detects DOWN                                 │
│    - Partition detected via lack of sync messages              │
│    - Manual trigger via API                                     │
├─────────────────────────────────────────────────────────────────┤
│ 2. Assessment                                                  │
│    - Check domain consistency                                  │
│    - Identify lost data                                       │
│    - Determine recovery mode                                   │
├─────────────────────────────────────────────────────────────────┤
│ 3. Reconstruction                                              │
│    - Pull state from peers / checkpoint                        │
│    - Apply CRDT merge                                         │
│    - Resolve conflicts                                        │
├─────────────────────────────────────────────────────────────────┤
│ 4. Validation                                                  │
│    - Verify checksums                                         │
│    - Check vector clock consistency                           │
│    - Validate domain integrity                                │
├─────────────────────────────────────────────────────────────────┤
│ 5. Reintegration                                               │
│    - Join process groups                                      │
│    - Announce presence to cluster                             │
│    - Accept incoming syncs                                    │
└─────────────────────────────────────────────────────────────────┘
```

### Checkpoint Management

```erlang
%% Create checkpoint
{ok, Checkpoint} = erlmcp_memory_recovery:create_checkpoint().

%% List checkpoints
Checkpoints = erlmcp_memory_recovery:list_checkpoints().

%% Restore checkpoint
{ok, Result} = erlmcp_memory_recovery:restore_checkpoint(CheckpointId).

%% Delete old checkpoint
ok = erlmcp_memory_recovery:delete_checkpoint(CheckpointId).
```

### Node Recovery

```erlang
%% Recover specific node
{ok, Status} = erlmcp_memory_recovery:recover_node(DownNode).

%% Recover specific domain
{ok, Result} = erlmcp_memory_recovery:recover_domain(session, DownNode).

%% Trigger automatic recovery
{ok, Pid} = erlmcp_memory_recovery:trigger_recovery().

%% Get recovery status
Status = erlmcp_memory_recovery:get_recovery_status().
%% Status = #{status => in_progress, progress => 45, ...}
```

### Partition Healing

```erlang
%% Detect and heal partition
{ok, Result} = erlmcp_memory_recovery:heal_partition().

%% Result = #{
%%     nodes => 3,
%%     merged => true,
%%     conflicts_resolved => 2
%% }.
```

---

## Integration Examples

### Starting the Memory System

```erlang
%% In supervisor
{erlmcp_distributed_memory, #{
    consistency => eventual,
    replication_factor => 2,
    memory_limit => #{
        max_bytes => 1024 * 1024 * 1024,
        domain_limits => #{
            session => #{max_bytes => 100 * 1024 * 1024}
        }
    }
}}.
```

### Using Cross-Agent Memory

```erlang
%% Agent 1: Store shared data
ok = erlmcp_distributed_memory:put(shared, analysis_results,
    #{model => "gpt-4", tokens => 1500},
    #{consistency => strong}).

%% Agent 2: Read shared data
{ok, Results} = erlmcp_distributed_memory:get(shared, analysis_results).

%% Agent 3: Subscribe to updates
ok = erlmcp_distributed_memory:subscribe(shared, self()).

%% Receive update
receive {memory_update, shared, Key, Value} ->
    io:format("Updated: ~p = ~p~n", [Key, Value])
end.
```

### Cache Coordination

```erlang
%% Subscribe to cache invalidations
ok = erlmcp_cache_coordinator:subscribe_invalidation(self()).

%% When data changes, invalidate across cluster
ok = erlmcp_cache_coordinator:broadcast_invalidation(user_cache_key).

%% Handle invalidation
receive {cache_invalidated, Key} ->
    %% Refresh data from source
    NewData = fetch_from_source(Key),
    erlmcp_cache:put(Key, NewData)
end.
```

---

## Performance Characteristics

### Expected Latency (p50)

| Operation | Local | Remote (Strong) | Remote (Eventual) |
|-----------|-------|-----------------|-------------------|
| Get | 1-5μs | 50-200μs | 10-50μs |
| Put | 2-10μs | 200-500μs | 10-50μs |
| Delete | 2-5μs | 100-300μs | 10-50μs |
| Sync | N/A | 10-100ms | 5-50ms |

### Throughput

| Backend | Reads/sec | Writes/sec | Total |
|---------|-----------|------------|-------|
| ETS | 1M+ | 500K+ | 1M+ |
| DETS | 50K | 25K | 50K |
| Mnesia (ram) | 100K | 20K | 100K |
| Mnesia (disc) | 50K | 10K | 50K |

### Memory Overhead

| Backend | Per Entry | Total (100K entries) |
|---------|-----------|----------------------|
| ETS | ~100 bytes | ~10 MB |
| DETS | ~150 bytes | ~15 MB + disk |
| Mnesia | ~200 bytes | ~20 MB + log |

---

## Configuration

### sys.config Example

```erlang
{erlmcp_core, [
    {memory_monitoring, #{
        enabled => true,
        check_interval => 30000,
        memory_threshold => 0.80,
        binary_threshold => 50000000,
        auto_gc => true
    }},
    {memory_guard, [
        {context, [
            {max_heap_size, 100_000_000},
            {max_bin_vheap_size, 50_000_000}
        ]},
        {tool, [
            {max_heap_size, 50_000_000},
            {max_bin_vheap_size, 25_000_000}
        ]}
    ]},
    {distributed_memory, #{
        consistency => eventual,
        replication_factor => 2,
        checkpoint_interval => 300000,
        auto_select => true
    }},
    {cache_coordinator, #{
        coherence_level => eventual,
        invalidation_timeout => 5000
    }},
    {persistence_selector, #{
        auto_select => true,
        evaluation_interval => 300000
    }}
]}.
```

---

## Migration Guide

### From v2 to v3

1. **Backup existing state**
   ```erlang
   {ok, Checkpoint} = erlmcp_memory_recovery:create_checkpoint().
   ```

2. **Install v3 modules**
   ```bash
   rebar3 compile
   rebar3 upgrade
   ```

3. **Configure persistence selector**
   ```erlang
   %% Auto-select optimal backends
   erlmcp_persistence_selector:auto_select(true).
   ```

4. **Run validation**
   ```erlang
   {ok, Validation} = erlmcp_memory_recovery:verify_state().
   ```

5. **Enable new features**
   ```erlang
   %% Enable distributed memory
   {ok, _} = erlmcp_distributed_memory:start_link([]).

   %% Enable cache coordination
   {ok, _} = erlmcp_cache_coordinator:start_link([]).
   ```

---

## Troubleshooting

### High Memory Usage

```erlang
%% Check memory stats
Stats = erlmcp_memory_monitor:get_memory_stats().

%% Force GC
{ok, _} = erlmcp_distributed_memory:force_gc().

%% Compact memory
{ok, _} = erlmcp_distributed_memory:compact().

%% Check domain stats
lists:foreach(fun(D) ->
    io:format("~p: ~p~n", [D, erlmcp_distributed_memory:get_domain_stats(D)])
end, [session, shared, registry, cache, ephemeral]).
```

### Sync Failures

```erlang
%% Check sync status
Status = erlmcp_distributed_memory:sync_status().

%% Manually trigger sync
ok = erlmcp_distributed_memory:sync(all).

%% Check cluster nodes
Nodes = pg:get_members(erlmcp_memory, memory_nodes).
```

### Recovery Issues

```erlang
%% Check recovery status
Status = erlmcp_memory_recovery:get_recovery_status().

%% List checkpoints
Checkpoints = erlmcp_memory_recovery:list_checkpoints().

%% Restore from last checkpoint
case Checkpoints of
    [Latest | _] ->
        {ok, _} = erlmcp_memory_recovery:restore_checkpoint(
            maps:get(id, Latest));
    [] ->
        {error, no_checkpoints}
end.
```

---

## References

- **Original cache**: `apps/erlmcp_core/src/erlmcp_cache.erl`
- **Session backends**: `apps/erlmcp_core/src/erlmcp_session_*`
- **Memory monitor**: `apps/erlmcp_core/src/erlmcp_memory_monitor.erl`
- **Memory guard**: `apps/erlmcp_core/src/erlmcp_memory_guard.erl`
- **Registry**: `apps/erlmcp_core/src/erlmcp_registry.erl`
