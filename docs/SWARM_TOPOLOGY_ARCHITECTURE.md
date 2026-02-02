# erlmcp Swarm Topology Architecture Design
## Integration with Claude-Flow Multi-Agent Orchestration

**Date:** 2026-02-01
**Version:** 1.0.0
**Author:** Erlang Architect Agent
**Status:** Architecture Design Proposal

---

## Executive Summary

This document presents an optimal swarm topology for erlmcp that integrates claude-flow's multi-agent orchestration capabilities. The design balances:

- **Process isolation** (Erlang strength) vs **connection pooling** (resource efficiency)
- **Hierarchical coordination** (queen-led) vs **peer-to-peer mesh** (fault tolerance)
- **Strong consistency** (Raft) vs **eventual consistency** (Gossip)
- **Distributed session management** vs **local-first optimization**

**Recommendation:** **Hybrid Adaptive Topology** combining hierarchical coordination for control plane with mesh topology for data plane, using Gossip for eventual consistency and local-first session management.

---

## 1. Topology Analysis: Process-per-Connection vs Connection Pooling

### 1.1 Current Model: Process-per-Connection

```
┌─────────────────────────────────────────────────────┐
│         erlmcp Current Architecture                  │
├─────────────────────────────────────────────────────┤
│                                                      │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐          │
│  │ Client 1 │  │ Client 2 │  │ Client N │          │
│  │  (gen_   │  │  (gen_   │  │  (gen_   │          │
│  │  server) │  │  server) │  │  server) │          │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘          │
│       │             │             │                 │
│       └─────────────┼─────────────┘                 │
│                     ▼                               │
│          ┌──────────────────────┐                   │
│          │   Registry (gproc)   │                   │
│          │   553K msg/s         │                   │
│          └──────────┬───────────┘                   │
│                     ▼                               │
│          ┌──────────────────────┐                   │
│          │  Transport Layer     │                   │
│          │  (stdio/tcp/http)    │                   │
│          └──────────────────────┘                   │
└─────────────────────────────────────────────────────┘

Current Metrics:
- 40-50K concurrent connections per node
- 553K msg/s registry throughput
- Isolated failures (one process crash = one client)
```

**Strengths:**
- ✅ Perfect fault isolation (crash = 1 connection lost)
- ✅ No shared state between connections
- ✅ Supervision tree naturally handles restarts
- ✅ Let-it-crash philosophy works perfectly
- ✅ Already achieving 40-50K connections/node

**Weaknesses:**
- ⚠️ Memory overhead: ~2KB per process = 100MB for 50K connections
- ⚠️ Context switching overhead at 50K+ processes
- ⚠️ GC pressure increases with process count

### 1.2 Alternative: Connection Pooling Model

```
┌─────────────────────────────────────────────────────┐
│         Connection Pooling Model                     │
├─────────────────────────────────────────────────────┤
│                                                      │
│  ┌──────────────────────────────────────────┐       │
│  │  Worker Pool (poolboy)                   │       │
│  │  ┌────────┐ ┌────────┐ ┌────────┐        │       │
│  │  │Worker 1│ │Worker 2│ │Worker N│        │       │
│  │  └───┬────┘ └───┬────┘ └───┬────┘        │       │
│  └──────┼──────────┼──────────┼─────────────┘       │
│         │          │          │                     │
│         └──────────┼──────────┘                     │
│                    ▼                                │
│         ┌──────────────────────┐                    │
│         │  Multiplexed Queue   │                    │
│         │  (1 process handles  │                    │
│         │   N connections)     │                    │
│         └──────────────────────┘                    │
└─────────────────────────────────────────────────────┘

Pooling Metrics (estimated):
- 100K+ connections per node (2x improvement)
- Higher throughput per worker
- Shared failure domain (1 worker crash = N connections)
```

**Strengths:**
- ✅ Lower memory footprint (~50% reduction)
- ✅ Better CPU utilization (fewer context switches)
- ✅ Higher connection density per node

**Weaknesses:**
- ❌ Shared failure domain (crash affects multiple connections)
- ❌ Breaks let-it-crash isolation
- ❌ Requires connection multiplexing logic
- ❌ HTTP/2 already provides stream multiplexing

### 1.3 Decision: Hybrid Approach

**Recommendation:** **Adaptive pooling based on transport type**

```erlang
%% Child spec with dynamic strategy selection
#{id => connection_handler,
  start => {erlmcp_connection_handler, start_link, [Config]},
  restart => permanent,
  type => worker,
  strategy => case Transport of
                stdio -> process_per_connection,  % Low volume
                tcp -> process_per_connection,    % Good isolation
                http -> pooled,                   % HTTP/2 streams
                ws -> process_per_connection,     % Stateful
                sse -> pooled                     % One-way streams
              end}
```

**Rationale:**
- **HTTP/2 transport**: Use pooling (gun already multiplexes streams)
- **WebSocket/TCP/STDIO**: Keep process-per-connection (stateful, need isolation)
- **SSE**: Use pooling (unidirectional, no state per connection)

---

## 2. Topology Choice: Hierarchical vs Mesh

### 2.1 Option A: Queen-Led Hierarchical Topology

**Supervision Tree:**
```
erlmcp_swarm_sup (one_for_all)
├── erlmcp_queen_coordinator (gen_server)
│   └── Responsibilities:
│       - Leader election
│       - Task assignment
│       - Worker health monitoring
│       - Load balancing decisions
│
├── erlmcp_worker_pool_sup (simple_one_for_one)
│   └── [Dynamic worker instances]
│       └── Each worker:
│           - Registers with queen
│           - Receives task assignments
│           - Reports metrics
│           - Handles failover
│
└── erlmcp_election_sup (one_for_one)
    ├── erlmcp_raft_server (gen_statem)
    └── erlmcp_election_monitor
```

**Child Spec:**
```erlang
#{id => erlmcp_queen_coordinator,
  start => {erlmcp_queen_coordinator, start_link, [#{
    election_algorithm => raft,
    heartbeat_interval => 5000,
    worker_timeout => 15000,
    load_balancing_strategy => least_connections
  }]},
  restart => permanent,
  shutdown => 10000,
  type => worker,
  modules => [erlmcp_queen_coordinator]}
```

**Strengths:**
- ✅ Clear authority for task assignment
- ✅ Centralized load balancing
- ✅ Simple debugging (query queen for state)
- ✅ Natural fit for claude-flow's orchestration

**Weaknesses:**
- ❌ Single point of failure (requires leader election)
- ❌ Coordination overhead (all decisions go through queen)
- ❌ Scalability ceiling (queen becomes bottleneck)
- ❌ Network partitions cause split-brain risk

### 2.2 Option B: Peer-to-Peer Mesh Topology

**Supervision Tree:**
```
erlmcp_mesh_sup (one_for_one)
├── erlmcp_gossip_server (gen_server)
│   └── Responsibilities:
│       - Propagate cluster membership
│       - Spread load information
│       - Anti-entropy (eventual consistency)
│       - Failure detection
│
├── erlmcp_local_registry (gen_server)
│   └── Responsibilities:
│       - Local process registry
│       - Cache remote lookups
│       - Route to nearest node
│
└── erlmcp_connection_pool_sup (simple_one_for_one)
    └── [Connection workers]
```

**Child Spec:**
```erlang
#{id => erlmcp_gossip_server,
  start => {erlmcp_gossip_server, start_link, [#{
    seed_nodes => [node1@host, node2@host],
    gossip_interval => 1000,
    convergence_time => 5000,
    failure_threshold => 3
  }]},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_gossip_server]}
```

**Strengths:**
- ✅ No single point of failure
- ✅ Horizontal scalability (add nodes freely)
- ✅ Network partition tolerant (eventually heals)
- ✅ Low coordination overhead

**Weaknesses:**
- ❌ Eventual consistency (not immediate)
- ❌ Complex debugging (distributed state)
- ❌ Convergence time (1-5 seconds typical)
- ❌ Requires conflict resolution strategies

### 2.3 Option C: Hybrid Adaptive Topology (RECOMMENDED)

**Architecture:**
- **Control Plane**: Hierarchical (Raft consensus for coordination)
- **Data Plane**: Mesh (Gossip for routing, direct P2P for data)

**Supervision Tree:**
```
erlmcp_hybrid_swarm_sup (one_for_one)
├── erlmcp_control_plane_sup (rest_for_one)
│   ├── erlmcp_coordinator (gen_server + Raft)
│   ├── erlmcp_health_monitor (gen_server)
│   └── erlmcp_task_scheduler (gen_server)
│
├── erlmcp_data_plane_sup (one_for_one)
│   ├── erlmcp_gossip_protocol (gen_server)
│   ├── erlmcp_routing_cache (gen_server)
│   └── erlmcp_p2p_connection_sup (simple_one_for_one)
│
└── erlmcp_registry_sync_sup (one_for_one)
    ├── erlmcp_registry_local (gen_server)
    └── erlmcp_registry_dist (gen_server)
```

**Why Hybrid?**
1. **Control decisions** (task assignment, rebalancing) need **strong consistency** → Raft
2. **Data routing** (request forwarding) needs **low latency** → Direct P2P mesh
3. **Cluster membership** can tolerate **eventual consistency** → Gossip
4. **Best of both worlds**: Authority when needed, autonomy for performance

---

## 3. Consensus Mechanisms: Raft vs Gossip

### 3.1 Raft (Strong Consistency)

**Implementation (gen_statem):**
```erlang
-module(erlmcp_raft_server).
-behaviour(gen_statem).

-record(raft_state, {
    role :: follower | candidate | leader,
    term :: non_neg_integer(),
    voted_for :: atom() | undefined,
    log :: [term()],
    commit_index :: non_neg_integer(),
    peers :: [atom()]
}).

%% States: follower → candidate → leader
callback_mode() -> state_functions.

follower({call, From}, request_vote, Data) ->
    %% Grant vote if term is higher
    {next_state, follower, Data, [{reply, From, {vote_granted, Data#raft_state.term}}]};
follower(timeout, election_timeout, Data) ->
    %% Become candidate and start election
    {next_state, candidate, increment_term(Data)}.

candidate(enter, _OldState, Data) ->
    %% Request votes from peers
    request_votes(Data#raft_state.peers),
    {keep_state_and_data, [{state_timeout, 150 + rand:uniform(150), election_timeout}]};
candidate({call, From}, vote_result, Data) ->
    %% Count votes, become leader if majority
    case has_majority_votes(Data) of
        true -> {next_state, leader, Data};
        false -> keep_state_and_data
    end.

leader(enter, _OldState, Data) ->
    %% Start sending heartbeats
    {keep_state_and_data, [{{timeout, heartbeat}, 50, send_heartbeat}]}.
```

**Use Case:** Control plane coordination, leader election

**Strengths:**
- ✅ Strong consistency guarantees
- ✅ Single source of truth (leader)
- ✅ Well-understood algorithm
- ✅ Fault tolerant (requires N/2 + 1 nodes)

**Weaknesses:**
- ❌ Leader election overhead (100-300ms typical)
- ❌ Write bottleneck (all writes go through leader)
- ❌ Network partition sensitivity
- ❌ Complexity (5 core states, log replication)

### 3.2 Gossip (Eventual Consistency)

**Implementation:**
```erlang
-module(erlmcp_gossip_server).
-behaviour(gen_server).

-record(gossip_state, {
    node_id :: atom(),
    peers :: [atom()],
    vector_clock :: map(),
    data :: map(),
    gossip_interval = 1000 :: pos_integer()
}).

init([Config]) ->
    {ok, #gossip_state{
        node_id = node(),
        peers = maps:get(seed_nodes, Config, []),
        vector_clock = #{node() => 0},
        data = #{}
    }, {continue, start_gossip}}.

handle_continue(start_gossip, State) ->
    Timer = erlang:send_after(State#gossip_state.gossip_interval, self(), gossip_tick),
    {noreply, State#{timer => Timer}}.

handle_info(gossip_tick, State) ->
    %% Select random peer
    Peer = select_random_peer(State#gossip_state.peers),

    %% Send our state
    gen_server:cast({erlmcp_gossip_server, Peer}, {gossip, State#gossip_state.data, State#gossip_state.vector_clock}),

    %% Schedule next gossip
    Timer = erlang:send_after(State#gossip_state.gossip_interval, self(), gossip_tick),
    {noreply, State#{timer => Timer}}.

handle_cast({gossip, RemoteData, RemoteVC}, State) ->
    %% Merge data using vector clocks
    MergedData = merge_data(State#gossip_state.data, RemoteData, State#gossip_state.vector_clock, RemoteVC),
    MergedVC = merge_vector_clocks(State#gossip_state.vector_clock, RemoteVC),

    {noreply, State#gossip_state{data = MergedData, vector_clock = MergedVC}}.
```

**Use Case:** Cluster membership, routing tables, metrics aggregation

**Strengths:**
- ✅ No leader required
- ✅ Partition tolerant
- ✅ Scales horizontally
- ✅ Simple implementation

**Weaknesses:**
- ❌ Eventual consistency only
- ❌ Convergence time (1-5 seconds)
- ❌ Conflict resolution needed
- ❌ Bandwidth overhead (redundant messages)

### 3.3 Decision Matrix

| Use Case | Mechanism | Rationale |
|----------|-----------|-----------|
| **Leader election** | Raft | Need strong consistency, single authority |
| **Task assignment** | Raft | Coordinator must have authoritative view |
| **Cluster membership** | Gossip | Eventual consistency acceptable, partition tolerant |
| **Routing tables** | Gossip | Low latency lookup more important than consistency |
| **Metrics aggregation** | Gossip | Approximate values sufficient |
| **Session replication** | Raft | Data integrity critical |

---

## 4. Session Management Across Distributed Nodes

### 4.1 Current Model: Local ETS + Mnesia

**Performance:**
- Read: ~5μs (ETS)
- Write: ~1ms (Mnesia sync)
- Replication: Manual (erlmcp_session_replicator)

**Strengths:**
- ✅ Extremely fast reads (ETS in-memory)
- ✅ Durable writes (Mnesia persistence)
- ✅ Simple API

**Weaknesses:**
- ❌ No automatic distribution
- ❌ Session affinity required (sticky routing)
- ❌ Node failure = session loss (until Mnesia sync)

### 4.2 Consistent Hashing + Replication (RECOMMENDED)

**Implementation:**
```erlang
-module(erlmcp_consistent_hash).

-export([add_node/1, remove_node/1, find_nodes/2]).

-record(ring_state, {
    vnodes :: [{pos_integer(), atom()}],  % Virtual nodes
    replicas = 3 :: pos_integer()         % Replication factor
}).

%% Add node to ring with virtual nodes (for load balancing)
add_node(Node) ->
    VNodes = [{hash({Node, I}), Node} || I <- lists:seq(1, 160)],
    gen_server:call(?MODULE, {add_vnodes, VNodes}).

%% Find N nodes responsible for key
find_nodes(Key, N) ->
    Hash = hash(Key),
    gen_server:call(?MODULE, {find_nodes, Hash, N}).

%% Internal: Find N successor nodes on ring
find_successors(Hash, N, Ring) ->
    Sorted = lists:sort(Ring#ring_state.vnodes),
    Successors = successors_from(Hash, Sorted, N, []),
    % Deduplicate physical nodes
    unique_nodes(Successors).
```

**Child Spec:**
```erlang
#{id => erlmcp_consistent_hash_manager,
  start => {erlmcp_consistent_hash_manager, start_link, [#{
    replication_factor => 3,      % Store on 3 nodes
    virtual_nodes => 160,         % Virtual nodes per physical node
    read_repair => true,          % Fix inconsistencies on read
    hinted_handoff => true        % Queue writes during failures
  }]},
  restart => permanent,
  shutdown => 5000,
  type => worker}
```

**Session Routing:**
```erlang
%% Store session on RF=3 nodes
create_session(Metadata) ->
    SessionId = generate_session_id(),
    Nodes = erlmcp_consistent_hash:find_nodes(SessionId, 3),

    %% Parallel write to all replica nodes
    Refs = [rpc:async_call(Node, erlmcp_session_manager, create_local_session,
                           [SessionId, Metadata]) || Node <- Nodes],

    %% Wait for W=2 acks (quorum)
    case wait_for_quorum(Refs, 2, 5000) of
        ok -> {ok, SessionId};
        {error, timeout} -> {error, insufficient_replicas}
    end.

%% Read from any replica (R=1)
get_session(SessionId) ->
    [PrimaryNode | _] = erlmcp_consistent_hash:find_nodes(SessionId, 1),

    case rpc:call(PrimaryNode, erlmcp_session_manager, get_local_session, [SessionId]) of
        {ok, Session} -> {ok, Session};
        {error, not_found} ->
            %% Read repair: Try other replicas
            read_from_replicas(SessionId)
    end.
```

**Strengths:**
- ✅ Horizontal scalability (add nodes, automatic rebalancing)
- ✅ Fault tolerance (RF replicas survive node failures)
- ✅ Lower bandwidth than full replication
- ✅ Proven at scale (Riak, Cassandra, DynamoDB)

---

## 5. Registry Routing Optimization

### Current Performance: 553K msg/s

### Optimization Strategies

#### 5.1 Sharded Registry (Horizontal Scaling)

```erlang
%% Shard registry by key prefix
-define(NUM_SHARDS, 16).

shard_for_key(Key) ->
    erlang:phash2(Key, ?NUM_SHARDS).

register_server(ServerId, ServerPid, Config) ->
    Shard = shard_for_key(ServerId),
    ShardPid = erlang:whereis(list_to_atom("erlmcp_registry_shard_" ++ integer_to_list(Shard))),
    gen_server:call(ShardPid, {register, ServerId, ServerPid, Config}).

%% Supervision tree for sharded registry
erlmcp_registry_shard_sup (simple_one_for_one)
└── [16 shard instances]
    ├── erlmcp_registry_shard_0
    ├── erlmcp_registry_shard_1
    ├── ...
    └── erlmcp_registry_shard_15
```

**Expected Performance:**
- 553K msg/s × 16 shards = **8.8M msg/s theoretical**
- Actual: ~6-7M msg/s (due to coordination overhead)

#### 5.2 Three-Tier Lookup Architecture

```
┌─────────────────────────────────────────────────────┐
│         Three-Tier Lookup Architecture               │
├─────────────────────────────────────────────────────┤
│                                                      │
│  ┌──────────────────────────────────────────┐       │
│  │  L1: Process Dictionary (process-local)  │       │
│  │  Latency: ~100ns                         │       │
│  └──────────────────────────────────────────┘       │
│                    │ Miss                            │
│                    ▼                                │
│  ┌──────────────────────────────────────────┐       │
│  │  L2: ETS Cache (node-local)              │       │
│  │  Latency: ~5μs                           │       │
│  └──────────────────────────────────────────┘       │
│                    │ Miss                            │
│                    ▼                                │
│  ┌──────────────────────────────────────────┐       │
│  │  L3: gproc Registry (authoritative)      │       │
│  │  Latency: ~10μs                          │       │
│  └──────────────────────────────────────────┘       │
└─────────────────────────────────────────────────────┘
```

---

## 6. Fault Tolerance and Resilience Patterns

### 6.1 Supervision Strategy Matrix

| Component | Strategy | Max Restarts | Period | Rationale |
|-----------|----------|--------------|--------|-----------|
| **Swarm Coordinator** | `permanent` | 5 | 60s | Critical - restart always |
| **Worker Pool** | `simple_one_for_one` | ∞ | N/A | Dynamic workers |
| **Gossip Server** | `permanent` | 3 | 60s | Self-healing via peers |
| **Registry Shard** | `permanent` | 5 | 60s | Recoverable from peers |
| **Connection Handler** | `temporary` | 0 | N/A | Don't restart (client reconnects) |

### 6.2 Circuit Breaker for Remote Calls

```erlang
-module(erlmcp_circuit_breaker).

-record(breaker_state, {
    state :: closed | open | half_open,
    failures = 0 :: non_neg_integer(),
    success_threshold = 3 :: pos_integer(),
    failure_threshold = 5 :: pos_integer(),
    timeout = 60000 :: pos_integer()  % Time in open state
}).

%% Wrap remote calls with circuit breaker
call_with_breaker(Fun) ->
    case get_breaker_state() of
        closed ->
            try Fun() of
                Result ->
                    reset_failures(),
                    Result
            catch
                _:_ ->
                    increment_failures(),
                    case get_failures() >= failure_threshold() of
                        true ->
                            open_breaker(),
                            {error, circuit_open};
                        false ->
                            {error, call_failed}
                    end
            end;
        open ->
            case time_since_open() > timeout() of
                true ->
                    half_open_breaker(),
                    call_with_breaker(Fun);  % Retry
                false ->
                    {error, circuit_open}
            end;
        half_open ->
            try Fun() of
                Result ->
                    close_breaker(),
                    Result
            catch
                _:_ ->
                    open_breaker(),
                    {error, circuit_open}
            end
    end.
```

### 6.3 Bulkhead Pattern (Failure Isolation)

**Supervision Tree:**
```erlang
erlmcp_bulkhead_sup (one_for_one)
├── erlmcp_http_bulkhead_sup (one_for_all)
│   └── [HTTP connection pool]
├── erlmcp_tcp_bulkhead_sup (one_for_all)
│   └── [TCP connection pool]
└── erlmcp_ws_bulkhead_sup (one_for_all)
    └── [WebSocket connection pool]
```

---

## 7. Final Architecture Recommendation

### Complete Hybrid Topology - Supervision Tree

```
erlmcp_swarm_sup (one_for_one)
├── erlmcp_control_plane_sup (rest_for_one)
│   ├── erlmcp_raft_coordinator (gen_statem)
│   │   - Leader election (Raft)
│   │   - Task assignment
│   │   - Rebalancing decisions
│   │
│   ├── erlmcp_health_monitor (gen_server)
│   │   - Node health checks
│   │   - Failure detection
│   │
│   └── erlmcp_task_scheduler (gen_server)
│       - Work queue management
│       - Priority scheduling
│
├── erlmcp_data_plane_sup (one_for_one)
│   ├── erlmcp_gossip_protocol (gen_server)
│   │   - Cluster membership
│   │   - Routing table propagation
│   │   - Anti-entropy
│   │
│   ├── erlmcp_registry_shard_sup (simple_one_for_one)
│   │   └── [16 registry shards]
│   │       - Partitioned key space
│   │       - O(1) local lookups
│   │
│   └── erlmcp_consistent_hash_manager (gen_server)
│       - Ring management
│       - Virtual nodes
│       - Rebalancing
│
├── erlmcp_session_tier_sup (one_for_one)
│   ├── erlmcp_hot_session_cache (gen_server + ETS)
│   │   - Active sessions (<1 min)
│   │   - ~5μs access time
│   │
│   ├── erlmcp_warm_session_store (gen_server + Mnesia)
│   │   - Recent sessions (1-5 min)
│   │   - ~1ms access time
│   │
│   └── erlmcp_cold_session_archive (gen_server + file)
│       - Inactive sessions (>5 min)
│       - ~10ms access time
│
├── erlmcp_transport_bulkhead_sup (one_for_one)
│   ├── erlmcp_http_pool_sup (one_for_all)
│   │   └── [HTTP/2 connection pool]
│   │
│   ├── erlmcp_tcp_pool_sup (one_for_all)
│   │   └── [TCP connection pool]
│   │
│   └── erlmcp_ws_pool_sup (one_for_all)
│       └── [WebSocket connection pool]
│
└── erlmcp_resilience_sup (one_for_one)
    ├── erlmcp_circuit_breaker_registry (gen_server)
    │   - Track circuit states
    │   - Auto-recovery
    │
    └── erlmcp_failure_detector (gen_server)
        - φ-accrual failure detection
        - Adaptive timeouts
```

### Key Design Decisions

#### ✅ RECOMMENDATIONS

1. **Connection Model:** Adaptive
   - HTTP/2: Use pooling (gun multiplexing)
   - TCP/WebSocket: Process-per-connection (isolation)

2. **Topology:** Hybrid
   - Control plane: Hierarchical + Raft (strong consistency)
   - Data plane: Mesh + Gossip (low latency)

3. **Consensus:** Mixed
   - Leader election: Raft (CP)
   - Routing tables: Gossip (AP)
   - Session data: Consistent hashing + quorum writes

4. **Session Storage:** Tiered
   - Hot: ETS (active sessions)
   - Warm: Mnesia (recent)
   - Cold: Disk (archived)

5. **Registry:** Sharded + Cached
   - 16 shards → ~8M msg/s capacity
   - 3-tier caching (process dict → ETS → gproc)

6. **Fault Tolerance:** Defense in depth
   - Bulkheads (per-transport isolation)
   - Circuit breakers (remote call protection)
   - Adaptive failure detection

---

## 8. Performance Projections

### Current vs Optimized

| Metric | Current | Optimized | Improvement |
|--------|---------|-----------|-------------|
| **Registry throughput** | 553K msg/s | 8M msg/s | 14.5x |
| **Connections/node** | 40-50K | 100K+ | 2-2.5x |
| **Session lookup latency** | 10μs | 100ns-5μs | 2-100x |
| **Node failure recovery** | Manual | <5s automatic | ∞ |
| **Cross-node routing** | N/A | <1ms | New capability |
| **Horizontal scalability** | Limited | Linear | ∞ |

### Expected System Capacity

**10-node cluster:**
- 1M concurrent connections (100K per node)
- 80M msg/s registry throughput (8M × 10 nodes)
- 99.99% availability (tolerates 2 node failures with RF=3)
- <5s failover time (Raft election + gossip convergence)

---

## 9. Migration Path

### Phase 1: Add Sharding (Week 1-2)
- Implement sharded registry
- No breaking changes
- 10-15x throughput improvement

### Phase 2: Add Gossip (Week 3-4)
- Implement gossip protocol for membership
- Enable multi-node awareness
- Prepare for distribution

### Phase 3: Distributed Sessions (Week 5-6)
- Implement consistent hashing
- Add session replication
- Enable cross-node routing

### Phase 4: Raft Coordination (Week 7-8)
- Implement Raft for leader election
- Add task scheduling
- Complete hybrid topology

### Phase 5: Claude-Flow Integration (Week 9-10)
- Integrate topology optimization
- Add orchestration hooks
- Enable adaptive rebalancing

---

## 10. Tradeoffs Summary

| Decision | Pros | Cons | Mitigation |
|----------|------|------|------------|
| **Hybrid topology** | Best of both worlds | Added complexity | Gradual rollout |
| **Sharded registry** | 10-15x throughput | Coordination overhead | Consistent hashing |
| **Gossip protocol** | Partition tolerant | Eventual consistency | Read repair |
| **Raft coordination** | Strong consistency | Leader bottleneck | Delegate data plane |
| **Tiered sessions** | Memory efficiency | Access time variance | Async warming |
| **Bulkhead pattern** | Fault isolation | Resource overhead | Dynamic pool sizing |

---

## Conclusion

The **Hybrid Adaptive Topology** provides the optimal balance for erlmcp's swarm architecture:

1. **Scalability:** Linear scaling to 100+ nodes
2. **Fault tolerance:** Survives multiple node failures
3. **Performance:** 10-100x improvements in key metrics
4. **Simplicity:** Gradual migration path, OTP-compliant
5. **Integration:** Native claude-flow orchestration support

This architecture maintains erlmcp's core strengths (process isolation, supervision trees, let-it-crash) while adding distributed capabilities that enable true swarm behavior.

**Next Steps:**
1. Review architecture with stakeholders
2. Prototype sharded registry (Phase 1)
3. Benchmark performance improvements
4. Begin gradual rollout

---

**Document Status:** Ready for review
**Next Review:** After Phase 1 prototype completion
