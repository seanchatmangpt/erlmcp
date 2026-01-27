# ErlMCP 100K Concurrent Connections - Implementation Roadmap

**Date:** 2026-01-27
**Status:** Ready for Implementation
**Timeline:** 12 weeks to production
**Owner:** Engineering Team

---

## Phase 1: Foundation & Stabilization (Weeks 1-2)

### Objective
Prevent system from breaking under stress. Ensure production stability without architectural changes.

### Phase 1.1: Message Queue Bounding (3 days)

**Ticket:** `feat: Implement per-connection message queue limits`

**Description:**
Current system has unbounded message queues per connection. Under stress, queues grow to 100K+ messages, consuming memory and causing GC pressure.

**Implementation:**

```erlang
%% erlmcp_backpressure.erl (enhance existing)
-define(DEFAULT_PER_CONN_LIMIT, 10000).  % Max messages per connection
-define(DEFAULT_GLOBAL_LIMIT, 500000).   % Max messages globally
-define(BACKPRESSURE_THRESHOLD, 0.8).    % Alert at 80% full

%% When adding message to queue:
handle_message_arrival(ConnPid, Message, State) ->
    case check_queue_limits(ConnPid, State) of
        ok ->
            queue_message(ConnPid, Message),
            State;
        {error, per_connection_full} ->
            respond_with_503(ConnPid, <<"Connection queue full">>),
            State;
        {error, global_full} ->
            respond_with_503(ConnPid, <<"System overloaded">>),
            State
    end.

check_queue_limits(ConnPid, State) ->
    PerConnDepth = erlmcp_queue_monitor:get_depth(ConnPid),
    GlobalDepth = erlmcp_queue_monitor:get_global_depth(),

    case {PerConnDepth > ?DEFAULT_PER_CONN_LIMIT,
          GlobalDepth > ?DEFAULT_GLOBAL_LIMIT} of
        {true, _} -> {error, per_connection_full};
        {_, true} -> {error, global_full};
        {false, false} -> ok
    end.
```

**Testing:**
- Unit test: Queue accepts up to limit, rejects above
- Integration test: 503 response when queue full
- Load test: Verify backpressure under 10K msg/sec bombing
- Verify: Queue never exceeds limit

**Acceptance Criteria:**
- ✓ Queue never grows beyond per-connection limit
- ✓ Global queue monitored and enforced
- ✓ 503 response returned when limits hit
- ✓ No queue overflow crash

**Estimated Effort:** 3 days
**Code Files:**
- `src/erlmcp_backpressure.erl` (enhance)
- `src/erlmcp_queue_monitor.erl` (new)
- `test/erlmcp_backpressure_tests.erl` (new)

### Phase 1.2: Connection Limiting (2 days)

**Ticket:** `feat: Implement adaptive connection limiting`

**Description:**
Without connection limits, system accepts connections until resources exhausted. Add hard and soft limits.

**Implementation:**

```erlang
%% erlmcp_connection_limiter.erl (new)
-define(SOFT_LIMIT, 150).      % Yellow alert threshold
-define(HARD_LIMIT, 200).      % Reject new connections
-define(ACCEPT_RATE_LIMIT, 100).  % Max accepts per second

start_connection(ClientAddr, ClientPort) ->
    case check_limits() of
        ok -> accept_connection(ClientAddr, ClientPort);
        {error, soft_limit} ->
            send_alert(yellow, "150 concurrent connections reached"),
            accept_connection(ClientAddr, ClientPort);  % Still accept
        {error, hard_limit} ->
            send_alert(red, "200 concurrent connections reached"),
            reject_with_503(ClientAddr)  % Reject new
    end.

check_limits() ->
    Active = erlmcp_registry:count_active_connections(),
    case Active of
        N when N >= ?HARD_LIMIT -> {error, hard_limit};
        N when N >= ?SOFT_LIMIT -> {error, soft_limit};
        _ -> ok
    end.
```

**Configuration (sys.config):**
```erlang
{erlmcp, [
    {connection_soft_limit, 150},
    {connection_hard_limit, 200},
    {accept_rate_limit_per_sec, 100}
]}
```

**Testing:**
- Unit test: Limits enforced at correct threshold
- Integration test: 503 rejection at hard limit
- Load test: Verify exactly 200 connections accepted, 201st rejected
- Verify: Soft limit triggers alert without rejection

**Acceptance Criteria:**
- ✓ Exactly 200 connections max
- ✓ 201st connection rejected with 503
- ✓ Yellow alert at 150 (soft limit)
- ✓ Red alert at 200+ (hard limit)

**Estimated Effort:** 2 days
**Code Files:**
- `src/erlmcp_connection_limiter.erl` (new)
- `test/erlmcp_connection_limiter_tests.erl` (new)
- Update: `sys.config`

### Phase 1.3: Prometheus Alerting Rules (5 days)

**Ticket:** `ops: Implement 4-tier alerting for production monitoring`

**Description:**
Add Prometheus alert rules for production visibility. 4 tiers: Green (ok), Yellow (warning), Red (critical), Emergency (auto-response).

**Implementation:**

**Level 1: Green Zone (Normal)**
```yaml
# No alerts
# Error rate: < 0.05%
# p95 Latency: < 100ms
# Connections: < 150
```

**Level 2: Yellow Zone (Warning)**
```yaml
- alert: ErlMCP_YellowAlert_ErrorRate
  expr: rate(mcp_errors_total[5m]) > 0.0005
  for: 2m
  annotations:
    severity: warning
    summary: "ErlMCP error rate {{ $value }}: investigate trends"
    action: "Page on-call, begin investigation"

- alert: ErlMCP_YellowAlert_Latency
  expr: histogram_quantile(0.95, rate(mcp_latency_ms_bucket[5m])) > 100
  for: 2m
  annotations:
    severity: warning
    summary: "ErlMCP p95 latency {{ $value }}ms: monitor scaling"
    action: "Check load, prepare scaling trigger"

- alert: ErlMCP_YellowAlert_Connections
  expr: mcp_connections_active >= 150
  for: 1m
  annotations:
    severity: warning
    summary: "{{ $value }} connections (soft limit): monitor growth"
    action: "Watch for spike toward hard limit"

- alert: ErlMCP_YellowAlert_Memory
  expr: container_memory_usage_bytes > 300000000
  for: 2m
  annotations:
    severity: warning
    summary: "Memory {{ $value }}MB: GC pressure increasing"
    action: "Monitor if approaching 350MB"
```

**Level 3: Red Zone (Critical)**
```yaml
- alert: ErlMCP_RedAlert_ErrorRate
  expr: rate(mcp_errors_total[5m]) > 0.005
  for: 30s
  annotations:
    severity: critical
    summary: "ERROR RATE CRITICAL {{ $value }}: IMMEDIATE ACTION"
    action: "Page entire team. Begin scaling immediately. Consider load shedding."

- alert: ErlMCP_RedAlert_Latency
  expr: histogram_quantile(0.95, rate(mcp_latency_ms_bucket[5m])) > 200
  for: 1m
  annotations:
    severity: critical
    summary: "LATENCY CRITICAL {{ $value }}ms: SLA BREACH"
    action: "Scale up, shed non-critical load"

- alert: ErlMCP_RedAlert_Connections
  expr: mcp_connections_active >= 200
  for: 30s
  annotations:
    severity: critical
    summary: "AT HARD LIMIT: {{ $value }} connections"
    action: "Reject new connections. Scale if possible."

- alert: ErlMCP_RedAlert_Memory
  expr: container_memory_usage_bytes > 350000000
  for: 1m
  annotations:
    severity: critical
    summary: "MEMORY CRITICAL {{ $value }}MB: OOM RISK"
    action: "Monitor closely. GC pause times increasing. May need restart."
```

**Level 4: Emergency (Auto-Response)**
```yaml
- alert: ErlMCP_Emergency_ErrorRate
  expr: rate(mcp_errors_total[5m]) > 0.01
  for: 10s
  annotations:
    severity: critical
    summary: "🚨 EMERGENCY: Error rate {{ $value }}% TOTAL FAILURE"
    action: "Auto-trigger: Graceful shutdown, failover to backup"

- alert: ErlMCP_Emergency_Memory
  expr: container_memory_usage_bytes > 450000000
  for: 10s
  annotations:
    severity: critical
    summary: "🚨 EMERGENCY: Memory {{ $value }}MB near OOM KILL"
    action: "Auto-trigger: Immediate graceful shutdown"

- alert: ErlMCP_Emergency_QueueOverflow
  expr: mcp_queue_depth_total > 100000
  for: 10s
  annotations:
    severity: critical
    summary: "🚨 EMERGENCY: Queue overflow {{ $value }} messages"
    action: "Auto-trigger: Shed non-critical load, reject new"
```

**Integration with Pager:**
```yaml
# In Alertmanager config:
routing:
  - match:
      severity: warning
    receiver: 'slack'
    continue: false
  - match:
      severity: critical
    receiver: 'pagerduty'
    continue: false
```

**Testing:**
- Unit test: Alert thresholds calculated correctly
- Integration test: Trigger each alert, verify notification
- Production test: Send test alert to pager before going live
- Verify: All 12 alerts firing correctly at specified thresholds

**Acceptance Criteria:**
- ✓ All 4-tier alerts implemented
- ✓ Thresholds match benchmarking data
- ✓ Pager integration working
- ✓ Team trained on alert meanings

**Estimated Effort:** 5 days (1 day implementation + 4 days testing/validation)
**Code Files:**
- `config/prometheus_alerts.yml` (new)
- `docs/ALERTING_RUNBOOK.md` (new)
- `test/alerting_integration_test.erl` (new)

### Phase 1 Validation & Deployment

**Pre-production Testing:**
```bash
# Test 1: Verify connection limit enforced
# Expected: Exactly 200 connections accepted, 201st rejected
make test-connection-limit

# Test 2: Verify queue bounding
# Expected: Queue never exceeds 10K per connection, returns 503
make test-queue-bounding

# Test 3: Verify alerts fire at correct thresholds
# Expected: Yellow at 0.05%, Red at 0.5%, Emergency at 1%
make test-alerting

# Test 4: Run baseline test (should still pass)
# Expected: 25 conn, 2,500 msg/sec, p95 < 100ms
make test-baseline
```

**Deployment:**
```
1. Deploy Phase 1.1: Queue bounding
2. Run baseline test (verify no regression)
3. Deploy Phase 1.2: Connection limiting
4. Run connection flood test (verify limits enforced)
5. Deploy Phase 1.3: Alerting
6. Test alert firing in staging
7. Go live with all three changes simultaneously
```

**Rollback Plan:**
- Each component independently removable
- Config-driven (can disable limits in sys.config)
- No database migrations or state changes

**Post-deployment Monitoring (Week 3):**
- Daily check: Are we hitting connection limits? (if yes, need to scale)
- Daily check: Are alerts firing appropriately?
- Daily check: No queue buildup outside limits?
- Weekly review: Any unexpected failures or regressions?

---

## Phase 2: Capacity Increase (Weeks 3-6)

### Objective
Increase safe operating capacity from 200 connections to 5-15K connections.

### Phase 2.1: Partition Count Increase (1 day)

**Ticket:** `perf: Increase registry partitions from 16 to 64`

**Description:**
Simple config change to spread registry lookups across more partitions.

**Implementation:**

```erlang
%% In sys.config:
{erlmcp, [
    {registry_partition_count, 64}  % Was: 16
]}

%% In erlmcp_registry_sharded.erl:
-define(DEFAULT_PARTITION_COUNT, 64).  % Changed from 16
```

**Analysis:**
- 16 partitions × 31 conn/partition (at failure point) = 496 connections → breaks
- 64 partitions × 31 conn/partition = 1,984 connections (theoretical)
- Expected safe point: ~64 partitions × 100 conn/partition = 6,400 connections

**Testing:**
- Unit test: Partition count correctly set to 64
- Load test: Connection flood to 500 conn (was breaking point, should now be comfortable)
- Load test: Verify no regressions at baseline (25 conn, 2,500 msg/sec)
- Benchmark: Measure lookup latency per partition

**Acceptance Criteria:**
- ✓ Registry correctly initializes 64 partitions
- ✓ Connection flood to 500 conn succeeds without errors
- ✓ Lookup latency unchanged or improved
- ✓ No performance regression

**Estimated Effort:** 1 day (mostly testing)
**Code Files:**
- Update: `src/erlmcp_registry_sharded.erl`
- Update: `sys.config`

### Phase 2.2: Local Lookup Cache (5 days)

**Ticket:** `perf: Implement L1 cache for registry lookups`

**Description:**
Add server-side cache of recent registry lookups to reduce ETS contention. 100ms TTL for eventual consistency.

**Implementation:**

```erlang
%% erlmcp_lookup_cache.erl (new)
-module(erlmcp_lookup_cache).

-define(CACHE_TTL_MS, 100).      % 100ms TTL
-define(MAX_CACHE_ENTRIES, 10000).  % Limit cache size

-record(cache_entry, {
    key :: term(),
    value :: term(),
    expires_at :: integer()  % timestamp in milliseconds
}).

init() ->
    ets:new(lookup_cache, [
        set,
        {keypos, #cache_entry.key},
        named_table,
        {write_concurrency, true},
        {read_concurrency, true}
    ]).

%% Lookup with fallback to registry
lookup(Key, RegistryFunc) ->
    case cache_get(Key) of
        {ok, Value} ->
            {cache_hit, Value};
        expired_or_missing ->
            Value = RegistryFunc(Key),
            cache_put(Key, Value),
            {registry_lookup, Value}
    end.

cache_get(Key) ->
    case ets:lookup(lookup_cache, Key) of
        [] -> expired_or_missing;
        [{cache_entry, _, Value, ExpiresAt}] ->
            Now = erlang:system_time(millisecond),
            case Now < ExpiresAt of
                true -> {ok, Value};
                false ->
                    ets:delete(lookup_cache, Key),
                    expired_or_missing
            end
    end.

cache_put(Key, Value) ->
    ExpiresAt = erlang:system_time(millisecond) + ?CACHE_TTL_MS,
    case ets:info(lookup_cache, size) of
        Size when Size > ?MAX_CACHE_ENTRIES ->
            %% Evict oldest entries
            evict_oldest(Size - (?MAX_CACHE_ENTRIES div 2));
        _ -> ok
    end,
    ets:insert(lookup_cache, #cache_entry{
        key = Key,
        value = Value,
        expires_at = ExpiresAt
    }).

%% Simple eviction: delete all expired
evict_oldest(Count) ->
    Now = erlang:system_time(millisecond),
    Expired = ets:select(lookup_cache, [{
        #cache_entry{expires_at = '$1', _='_'},
        [{'<', '$1', Now}],
        ['$_']
    }]),
    lists:foreach(fun(Entry) ->
        ets:delete(lookup_cache, Entry#cache_entry.key)
    end, lists:sublist(Expired, Count)).
```

**Integration into registry:**

```erlang
%% In erlmcp_registry.erl find_server/1:
find_server(ServerId) ->
    erlmcp_lookup_cache:lookup({server, ServerId}, fun(Key) ->
        %% Actual registry lookup (fallback)
        PartitionId = partition_for_server(ServerId),
        Table = partition_table(PartitionId),
        case ets:lookup(Table, Key) of
            [{_, ServerPid, _}] -> ServerPid;
            [] -> undefined
        end
    end).
```

**Metrics to track:**
```
mcp_lookup_cache_hits          % Cache hit count
mcp_lookup_cache_misses        % Cache miss count
mcp_lookup_cache_hit_rate      % Hit rate % (target: 90%+)
mcp_lookup_latency_cached_ms   % With cache (target: <0.1ms)
mcp_lookup_latency_uncached_ms % Without cache (target: <1ms)
```

**Testing:**
- Unit test: Cache expiry works correctly
- Unit test: LRU eviction works
- Load test: Verify 90%+ cache hit rate
- Load test: Measure lookup latency improvement
- Chaos test: Invalidate cache, verify fallback works

**Acceptance Criteria:**
- ✓ Cache hit rate 90%+ at steady state
- ✓ Lookup latency reduced 10-100x (1µs → 100ns)
- ✓ Cache correctly evicts expired entries
- ✓ Registry lookups work correctly as fallback

**Estimated Effort:** 5 days
**Code Files:**
- `src/erlmcp_lookup_cache.erl` (new)
- Update: `src/erlmcp_registry.erl`
- `test/erlmcp_lookup_cache_tests.erl` (new)

### Phase 2.3: Message Batching (5 days)

**Ticket:** `perf: Implement message batching to reduce context switches`

**Description:**
Instead of processing messages one at a time, batch 10-100 messages and process together. Reduces context switches and improves throughput.

**Implementation:**

```erlang
%% erlmcp_message_batch.erl (new)
-module(erlmcp_message_batch).

-define(BATCH_SIZE, 50).           % Process in batches of 50
-define(BATCH_TIMEOUT_MS, 10).     % Or timeout after 10ms

-record(batch_state, {
    queue = [] :: [term()],        % Queued messages
    timer_ref :: reference() | undefined,
    batch_size_limit :: pos_integer()
}).

init() ->
    #batch_state{batch_size_limit = ?BATCH_SIZE}.

add_message(Message, BatchState) ->
    NewQueue = [Message | BatchState#batch_state.queue],
    case length(NewQueue) of
        N when N >= ?BATCH_SIZE ->
            %% Batch full, process immediately
            {process, lists:reverse(NewQueue), init()};
        _ ->
            %% Start timer if first message
            NewState = case BatchState#batch_state.timer_ref of
                undefined ->
                    TimerRef = erlang:send_after(?BATCH_TIMEOUT_MS, self(), {batch_timeout}),
                    BatchState#batch_state{
                        queue = NewQueue,
                        timer_ref = TimerRef
                    };
                _ ->
                    BatchState#batch_state{queue = NewQueue}
            end,
            {queue, NewState}
    end.

handle_timeout(BatchState) ->
    case BatchState#batch_state.queue of
        [] -> {no_action, init()};
        Queue ->
            {process, lists:reverse(Queue), init()}
    end.

%% Usage in erlmcp_server.erl:
handle_info({transport_data, Data}, State) ->
    Messages = erlmcp_json_rpc:decode_batch(Data),
    case erlmcp_message_batch:add_messages(Messages, State#state.batch_state) of
        {process, Batch, NewBatchState} ->
            process_batch(Batch, State#state{batch_state = NewBatchState});
        {queue, NewBatchState} ->
            {noreply, State#state{batch_state = NewBatchState}}
    end;

handle_info({batch_timeout}, State) ->
    case erlmcp_message_batch:handle_timeout(State#state.batch_state) of
        {no_action, NewBatchState} ->
            {noreply, State#state{batch_state = NewBatchState}};
        {process, Batch, NewBatchState} ->
            process_batch(Batch, State#state{batch_state = NewBatchState})
    end.

process_batch(Batch, State) ->
    %% Process all messages in batch
    Results = [handle_single_message(Msg, State) || Msg <- Batch],
    %% Send all responses together
    send_batch_responses(Results, State).
```

**Benefits:**
```
Before Batching:
  50 messages → 50 separate handle_info calls
  Context switches: 50
  Latency: Average message waits ~25 positions in queue

After Batching (size 50):
  50 messages → 1 handle_info call (+ timeout)
  Context switches: 1-2
  Latency: Average message waits ~25ms (batch timeout)
  Throughput: 10x better CPU utilization
```

**Testing:**
- Unit test: Batch formation and timeout
- Load test: Measure context switch reduction
- Load test: Verify latency doesn't degrade (batching adds latency)
- Benchmark: Throughput improvement measurement

**Acceptance Criteria:**
- ✓ Throughput increases 1.5-2x
- ✓ p50 latency acceptable (slight increase ok for higher throughput)
- ✓ p95 latency stays under 200ms
- ✓ Batches properly timeout and process

**Estimated Effort:** 5 days
**Code Files:**
- `src/erlmcp_message_batch.erl` (new)
- Update: `src/erlmcp_server.erl`
- `test/erlmcp_message_batch_tests.erl` (new)

### Phase 2.4: GC Tuning (3 days)

**Ticket:** `perf: Optimize Erlang VM GC settings for 5-15K connections`

**Description:**
Tune Erlang VM garbage collection settings to reduce pause times and memory fragmentation.

**Implementation:**

**Current vm.args:**
```
+K true                    % Kernel polling
+A 128                     % Async thread pool
```

**Updated vm.args:**
```
+K true                    % Kernel polling
+A 128                     % Async thread pool
+hms 128                   % Min heap size 128MB
+hmsx 256                  % Max heap size 256MB
+hts 32                    % Heap table size
+fnl 10000                 % Force fullsweep after 10K minor GCs
+sbt db                    % Scheduler bind type (default)
+sfp true                  % Set fraction of processes true
+swct very_long            % Sleep when working counter threshold
```

**Configuration (sys.config):**
```erlang
{erlang, [
    {hipe_compiler, false},           % Disable HiPE (not beneficial for this workload)
    {fullsweep_after, 0},             % More frequent minor GCs
    {min_heap_size, 4096},            % Initial heap size (1024 words)
    {max_heap_size, 50000000},        % Max per process ~50MB (prevents runaway)
    {process_limit, 1000000}          % Allow many processes
]}
```

**Analysis:**

```
Current GC Behavior (baseline):
  Min heap: 233 words (~1.8 KB)
  Max heap: unlimited
  GC strategy: Generational (young/old)
  Major GC: Every 65000 minor GCs or memory pressure

  At 500 conn, 25K msg/sec:
    Memory pressure: ~410 MB
    Full GC: Every 5 seconds
    Pause time: 200-500ms
    Impact: ~1% of CPU time in GC

Optimized GC Behavior:
  Min heap: 4096 words (~32 KB)
  Max heap: 50MB per process
  GC strategy: More frequent minor GCs
  Major GC: Every 10K minor GCs (less frequent)

  Expected at 5-15K conn:
    Memory pressure: Distributed across more processes
    Full GC: Less frequent (only when needed)
    Pause time: 30-100ms (rare)
    Impact: <0.5% of CPU time in GC
```

**Testing:**
- Unit test: Verify VM args applied
- Load test: Baseline (25 conn) - verify no regression
- Load test: Extended (250 conn, 10 min) - verify stable memory/GC
- Benchmark: GC pause time measurement
- Benchmark: Memory fragmentation analysis

**Acceptance Criteria:**
- ✓ GC pause time reduced (target: <100ms p99)
- ✓ Baseline performance unchanged or improved
- ✓ Memory usage stable over long runs
- ✓ No process crashes due to max_heap_size

**Estimated Effort:** 3 days
**Code Files:**
- Update: `vm.args`
- Update: `sys.config`
- `test/erlmcp_gc_tuning_tests.erl` (new)

### Phase 2 Validation & Deployment

**End-of-Phase Testing:**
```bash
# Load test: 500 connections, 50 msg/sec each (25K msg/sec)
make test-connection-flood
# Expected: p95 < 150ms, error < 0.1%, CPU < 60%

# Baseline regression test: 25 conn, 2,500 msg/sec
make test-baseline
# Expected: p95 < 100ms, error < 0.01%

# GC stress test: 5,000 connections ramping (if achievable)
make test-gc-stress
# Expected: Stable memory, GC pause < 100ms
```

**Performance Metrics (post-Phase 2):**
- Concurrent connections: 500+ sustained
- Throughput: 10,000+ msg/sec safe (was 5,000)
- p95 Latency: <150ms
- Error rate: <0.1%
- CPU: ~60-70% (was 65-75% before phase 2)
- Memory: ~350MB (stable, was growing)

---

## Phase 3: Full Scaling to 100K (Weeks 7-12)

### Objective
Implement hierarchical sharding and multi-node clustering to reach 100K concurrent connections.

### Phase 3.1: Hierarchical Registry (4 weeks)

**Ticket:** `arch: Implement hierarchical sharded registry for 100K+ scale`

**Description:**
Current flat registry with 64 partitions can sustain 5-15K connections. For 100K, need hierarchical registry:
- Local partitions (100-256) for fast lookup
- Global gossip sync for consistency
- Sticky routing (client stays on same server)

**Architecture:**

```
Hierarchical Registry:
  Server
    ├── erlmcp_registry_local (100 partitions)
    │   ├── Partition 0-99: Local lookup cache
    │   └── Much faster than global registry
    │
    ├── erlmcp_registry_global (gossip sync)
    │   ├── Sync with peers periodically (100ms)
    │   ├── Lazy replication (eventual consistency)
    │   └── Fallback for missing entries
    │
    └── erlmcp_routing (sticky sessions)
        ├── Route new connection to local partition
        ├── Route existing to sticky server
        └── Handle migration on failure

Performance:
  Local hit (99% of requests): <100ns (memory lookup)
  Global miss (1% of requests): <10ms (TCP to peer)
  Effective latency: <1µs average (vs 5-10µs current)
```

**Implementation Details:**

1. **Local Registry Module** (1 week)
```erlang
%% erlmcp_registry_local.erl (new)
-module(erlmcp_registry_local).

%% Local registry: fast in-memory lookup
%% Replaces ETS-based global registry for local operations

register_server_local(ServerId, ServerPid) ->
    PartitionId = compute_local_partition(ServerId, 100),
    Table = local_partition_table(PartitionId),
    ets:insert(Table, {{server, ServerId}, ServerPid, #{timestamp => now()}}),
    ok.

find_server_local(ServerId) ->
    PartitionId = compute_local_partition(ServerId, 100),
    Table = local_partition_table(PartitionId),
    case ets:lookup(Table, {server, ServerId}) of
        [{_, ServerPid, _}] -> {ok, ServerPid};
        [] -> {error, not_found}
    end.

%% Initialize 100 local partitions
init_local_partitions() ->
    [create_partition(I) || I <- lists:seq(0, 99)].

create_partition(PartitionId) ->
    TableName = list_to_atom("erlmcp_partition_local_" ++ integer_to_list(PartitionId)),
    ets:new(TableName, [
        {name, TableName},
        {type, set},
        {read_concurrency, true},
        {write_concurrency, true},
        {memory, compressed}  % Save memory
    ]).
```

2. **Global Gossip Module** (2 weeks)
```erlang
%% erlmcp_registry_gossip.erl (new)
-module(erlmcp_registry_gossip).

%% Gossip protocol for cross-node consistency
%% 100ms heartbeat, lazy replication

gossip_sync_interval() -> 100.  % milliseconds

periodic_sync() ->
    Peers = erlmcp_clustering:get_peer_nodes(),
    LocalState = erlmcp_registry_local:get_all_entries(),
    [sync_with_peer(Peer, LocalState) || Peer <- Peers].

sync_with_peer(Peer, LocalState) ->
    rpc:call(Peer, erlmcp_registry_gossip, merge_state, [LocalState]).

merge_state(RemoteState) ->
    %% Merge remote state with local
    %% Conflict resolution: latest timestamp wins
    erlmcp_registry_local:merge_entries(RemoteState).
```

3. **Sticky Routing Module** (1 week)
```erlang
%% erlmcp_routing.erl (new)
-module(erlmcp_routing).

%% Route connections to servers
%% Prefer local, fallback to sticky, fallback to global

route_to_server(ServerId) ->
    case erlmcp_registry_local:find_server(ServerId) of
        {ok, ServerPid} ->
            {local, ServerPid};
        {error, not_found} ->
            %% Check global registry (gossip-synced)
            case erlmcp_registry_gossip:find_server(ServerId) of
                {ok, ServerPid} ->
                    {global, ServerPid};
                {error, not_found} ->
                    {error, not_found}
            end
    end.

%% On new connection: assign to local partition
assign_new_connection(ClientId) ->
    PartitionId = erlmcp_registry_local:compute_partition(ClientId, 100),
    {local, PartitionId}.
```

**Testing Plan:**
- Unit: Local registry CRUD operations
- Unit: Gossip sync/merge logic
- Integration: Sticky routing under load
- Load: 50K connections, verify local hits
- Chaos: Partition a cluster, verify fallback works
- Verify: No data loss during partition healing

**Acceptance Criteria:**
- ✓ 99%+ local hits (measured via metrics)
- ✓ Cross-node sync within 100ms
- ✓ Graceful fallback on missing entries
- ✓ Can sustain 50K connections on single node

**Estimated Effort:** 4 weeks
**Code Files:**
- `src/erlmcp_registry_local.erl` (new)
- `src/erlmcp_registry_gossip.erl` (new)
- `src/erlmcp_routing.erl` (new)
- Update: `src/erlmcp_registry.erl` (delegation layer)
- `test/erlmcp_registry_hierarchical_tests.erl` (new)

### Phase 3.2: Multi-Node Erlang Clustering (3 weeks)

**Ticket:** `arch: Implement Erlang clustering for horizontal scaling`

**Description:**
Add distributed Erlang support to run erlmcp across multiple nodes with shared state.

**Architecture:**

```
Cluster:
  ┌─────────────────────────────────────────────┐
  │  Load Balancer (Nginx, round-robin)         │
  └──────┬──────────────┬──────────────┬────────┘
         │              │              │
    Node 1          Node 2          Node 3        Node 4
  (25K conn)    (25K conn)      (25K conn)    (25K conn)
    ┌──────┐      ┌──────┐       ┌──────┐      ┌──────┐
    │Erl VM│      │Erl VM│       │Erl VM│      │Erl VM│
    │16GB  │      │16GB  │       │16GB  │      │16GB  │
    └──────┘      └──────┘       └──────┘      └──────┘

  Total: 100K connections
  Per node: 25K (well below safety limit of 50K per hierarchical)
  Redundancy: Any 3 nodes sustain 75K connections
```

**Implementation:**

1. **Clustering Module** (1.5 weeks)
```erlang
%% erlmcp_clustering.erl (new)
-module(erlmcp_clustering).

init_cluster(NodeName, ClusterNodes) ->
    %% Set distributed Erlang name
    erlang:set_node(NodeName),
    %% Connect to peers
    [net_kernel:connect_node(Node) || Node <- ClusterNodes],
    %% Monitor cluster health
    erlang:send_after(5000, self(), {cluster_health_check}).

get_peer_nodes() ->
    nodes() -- [node()].

is_connected(Node) ->
    lists:member(Node, nodes(connected)).

handle_node_down(Node) ->
    %% Node failed: redistribute its connections
    logger:warning("Node ~p down, redistributing connections", [Node]),
    migrate_connections_from_node(Node).
```

2. **Load Balancer Configuration** (1 week)

**Nginx config:**
```nginx
upstream erlmcp_cluster {
    least_conn;  % Round-robin by connection count
    server node1.erlmcp:5000;
    server node2.erlmcp:5000;
    server node3.erlmcp:5000;
    server node4.erlmcp:5000;

    # Health check
    check interval=3000 rise=2 fall=5 timeout=1000 type=tcp;
}

server {
    listen 80;
    location / {
        proxy_pass http://erlmcp_cluster;
        proxy_http_version 1.1;
        proxy_set_header Connection "";
        proxy_set_header X-Forwarded-For $remote_addr;
    }
}
```

3. **Graceful Shutdown** (0.5 weeks)
```erlang
%% Drain existing connections before shutdown
handle_shutdown_signal() ->
    logger:info("Shutdown signal received, draining connections"),
    stop_accepting_new_connections(),
    wait_for_existing_connections(30000),  % 30s timeout
    graceful_exit().
```

**Testing:**
- Integration: Connect 2 nodes, verify state sync
- Load: 4 nodes, 25K conn each = 100K total
- Chaos: Kill node 1, verify 75K connections on remaining 3
- Chaos: Partition network, verify eventual consistency
- Chaos: Rejoin partition, verify re-sync

**Acceptance Criteria:**
- ✓ Cluster initializes correctly
- ✓ 4 nodes sustain 100K connections
- ✓ State syncs across nodes
- ✓ Handles node failures gracefully

**Estimated Effort:** 3 weeks
**Code Files:**
- `src/erlmcp_clustering.erl` (new)
- `config/nginx_erlmcp.conf` (new)
- `scripts/deploy_cluster.sh` (new)
- `test/erlmcp_clustering_tests.erl` (new)

### Phase 3.3: Integration & Production Testing (2 weeks)

**Ticket:** `qa: End-to-end testing and production readiness for 100K scaling`

**Activities:**

1. **End-to-End Test Suite** (1 week)
```
Test 1: Baseline + all phases
  Expected: p95 < 100ms, error <0.01%

Test 2: Connection flood (5,000 ramp)
  Expected: Sustain 5,000 connections, p95 < 100ms

Test 3: Message bombing (50K msg/sec)
  Expected: Queue bounded, no crash, recovery < 2 min

Test 4: Cluster resilience (100K on 4 nodes, 1 node fails)
  Expected: 3 nodes sustain 75K, client reconnect works

Test 5: Chaos engineering (random failures, latency injection)
  Expected: System stable, no data loss

Test 6: Capacity scaling (add 5th node to 100K cluster)
  Expected: Smooth redistribution, no connection loss
```

2. **Performance Benchmarking** (3 days)
```
Measure at 100K connections, 50K msg/sec:
  - Throughput: ✓ Consistent 50K msg/sec
  - p50 Latency: ✓ < 20ms
  - p95 Latency: ✓ < 100ms
  - p99 Latency: ✓ < 150ms
  - Error Rate: ✓ < 0.05%
  - CPU per node: ✓ < 60%
  - Memory per node: ✓ < 250MB
  - Cross-node sync: ✓ < 100ms
  - Connection success rate: ✓ > 99.99%
```

3. **Documentation** (2 days)
```
- Scaling playbook (how to deploy 4-node cluster)
- Operations guide (monitoring, alerts, troubleshooting)
- Failure scenarios (what to do if node dies, network partition)
- Capacity planning (how to scale beyond 100K)
- Runbooks (incident response procedures)
```

**Acceptance Criteria:**
- ✓ All end-to-end tests pass
- ✓ Performance benchmarks meet targets
- ✓ Documentation complete and reviewed
- ✓ Team trained and comfortable with operations

**Estimated Effort:** 2 weeks
**Code Files:**
- `test/erlmcp_e2e_100k_test.erl` (new)
- `docs/SCALING_PLAYBOOK.md` (new)
- `docs/OPERATIONS_GUIDE.md` (new)

---

## Implementation Timeline (Gantt Chart)

```
Week  1-2:    [Phase 1: Foundation & Stabilization]
              ├─ Queue bounding (days 1-3)
              ├─ Connection limiting (days 4-5)
              └─ Alerting (days 6-10) [overlaps week 2]

Week  3-6:    [Phase 2: Capacity Increase]
              ├─ Partitions 16→64 (day 1)
              ├─ Lookup cache (days 2-6)
              ├─ Message batching (days 7-11) [overlaps week 4]
              └─ GC tuning (days 12-14) [overlaps week 4]

Week  7-12:   [Phase 3: Full Scaling to 100K]
              ├─ Hierarchical registry (weeks 7-10)
              ├─ Multi-node clustering (weeks 9-11) [overlaps]
              └─ Integration testing (weeks 11-12)
```

**Critical Path:** Phase 1 → Phase 2 → Phase 3
**Can Run in Parallel:** Phase 2.3 + 2.4 can run during Phase 2.1-2.2

---

## Risk Mitigation

### Risk 1: Breaking Changes in Phase 1

**Risk:** Queue bounding / connection limiting break existing clients

**Mitigation:**
- Queue limits are only triggered under extreme stress
- Connection limits documented in release notes
- Gradual rollout (canary 5% → 25% → 100%)
- Rollback procedure tested before deployment

**Acceptance:** 2 weeks to detect issues in canary

### Risk 2: Hierarchical Registry Consistency

**Risk:** Gossip sync loses data or causes inconsistency

**Mitigation:**
- Extensive testing with chaos engineering
- Write-ahead logging for state changes
- Conflict resolution strategy (timestamps)
- Monitoring of sync delays

**Acceptance:** 100% test coverage of sync logic

### Risk 3: Multi-Node Operational Complexity

**Risk:** Operators struggle with 4-node cluster management

**Mitigation:**
- Extensive documentation and runbooks
- Team training exercises
- Automated health checks and alerts
- Graceful degradation (works with node failures)

**Acceptance:** Dry-run exercises before production

---

## Success Criteria

### Phase 1 Success
- [ ] Queue bounding prevents overflow
- [ ] Connection limiting enforces 200 max
- [ ] Alerts firing appropriately
- [ ] No production incidents related to overload
- [ ] System stability improves

### Phase 2 Success
- [ ] Throughput increases 5,000 → 10,000 msg/sec
- [ ] Concurrent connections increase 200 → 5,000
- [ ] p95 latency < 150ms at 5,000 connections
- [ ] Error rate < 0.1% at all load levels
- [ ] No performance regressions

### Phase 3 Success
- [ ] 100K concurrent connections sustained
- [ ] p95 latency < 100ms across all 4 nodes
- [ ] Error rate < 0.05%
- [ ] CPU utilization < 60% per node
- [ ] Memory utilization < 250MB per node
- [ ] Node failover transparent to clients
- [ ] Recovery from partition < 2 minutes
- [ ] Production deployment stable for 30 days

---

## Estimated Resource Allocation

### Development Team
- **Backend Engineer #1:** Full-time on Phase 1-3 (all phases)
- **Backend Engineer #2:** Phase 2 & 3 (partway through phase 1)
- **DevOps Engineer:** Phase 1 alerting, Phase 3 clustering
- **QA/Load Tester:** Ongoing throughout all phases

### Timeline
- **Phase 1:** 1 backend engineer, 1 devops, 1 QA = 2 weeks
- **Phase 2:** 2 backend engineers, 1 QA = 4 weeks
- **Phase 3:** 2 backend engineers, 1 devops, 1 QA = 6 weeks
- **Total:** 12 weeks (3 engineers × 12 weeks = 36 engineer-weeks)

### Budget (Estimated Infrastructure)
- **Phase 1-2:** Existing infrastructure (single node)
- **Phase 3:** 4 nodes (3 × current cost) = ~$15-30K/month additional
- **Load testing infrastructure:** +$5K one-time

---

**Roadmap Status:** READY FOR EXECUTION
**Confidence Level:** HIGH
**Next Steps:** Kickoff Phase 1, Week of 2026-02-03
