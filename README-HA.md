# High Availability and Clustering Guide for erlmcp v3

## Executive Summary

erlmcp v3 is designed for **distributed Erlang clustering** with partition tolerance, automatic failover, and cluster-churn safety. This guide covers:

- Distributed Erlang node clustering and network topology
- gproc/pg-based service discovery and registry routing
- Partition detection and split-brain prevention
- Failover mechanisms and recovery narratives
- Cluster monitoring and health checks

**Key Principle**: As stated in the CLAUDE.md constitution: *"overlay DNS lies; retries + convergence. Partitions expected. Recovery narratives required. No stable node assumptions."*

---

## Part 1: Distributed Erlang Clustering Fundamentals

### 1.1 Cluster Architecture Overview

erlmcp v3 clusters are built on Erlang/OTP distribution, where nodes communicate via:

```
┌─────────────────────────────────────────────────────┐
│  Global Load Balancer (HAProxy/Kubernetes Ingress) │
└────────┬──────────────┬──────────────┬──────────────┘
         │              │              │
    ┌────▼────┐    ┌───▼─────┐   ┌──▼──────┐
    │Node 1   │    │Node 2    │   │Node 3   │
    │(Primary)│    │(Replica) │   │(Replica)│
    └──┬──────┘    └───┬──────┘   └─────┬───┘
       │               │               │
       └───────────────┼───────────────┘
                       │
          ┌────────────┴──────────────┐
          │   Distributed ETS/Mnesia  │
          │  (Session + State Tables) │
          └────────────────────────────┘
```

### 1.2 Node Naming and Connection

Nodes are identified by names like `erlmcp@hostname`. Cluster formation happens through:

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_cluster.erl` (lines 73-89)

```erlang
%% Connect to cluster nodes
connect(Nodes) ->
    gen_server:call(?SERVER, {connect, Nodes}, ?DEFAULT_CONNECT_TIMEOUT).

%% Disconnect from a node
disconnect(Node) ->
    gen_server:call(?SERVER, {disconnect, Node}, 5000).
```

**Key behavior**:
- Async connection attempts (OTP 26+)
- Heartbeat monitoring with configurable intervals (default: 10s)
- Reconnection attempts with exponential backoff
- Per-node health tracking: `healthy | degraded | unhealthy`

### 1.3 Node Status Tracking

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_node_monitor.erl` (lines 1-78)

The `erlmcp_node_monitor` gen_server tracks node states:

```erlang
-record(node_monitor_state, {
    monitored_nodes = #{} :: #{node() => #{status => up | down, last_seen => integer()}},
    check_interval = 5000 :: pos_integer(),
    check_ref :: reference() | undefined
}).
```

**Responsibilities**:
1. **Subscription to node events**: Uses `net_kernel:monitor_nodes(true, [nodedown_reason])`
2. **Periodic node checks**: Validates connectivity on 5-second intervals
3. **Last-seen tracking**: Records last successful contact with each node
4. **Automatic reconnection**: Triggers on node-down events

---

## Part 2: Service Discovery and Registry Routing

### 2.1 gproc-Based Service Discovery

erlmcp v3 uses **gproc** for distributed service registry. This enables:
- Global process registration without central registry
- Process group membership for failover
- Zero-copy name lookups via scalable locks

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_registry.erl` (lines 48-69)

```erlang
%% Register server globally (distributed)
register_server(global, ServerId, ServerPid, Config) ->
    ConfigWithPid = Config#{pid => ServerPid},
    erlmcp_registry_dist:register_global(server, ServerId, ServerPid, ConfigWithPid).

%% Register transport locally (node-scoped)
register_transport(local, TransportId, TransportPid, Config) ->
    gen_server:call(?MODULE, {register_transport, TransportId, TransportPid, Config}).
```

### 2.2 Registry Backends: Global vs. pg

erlmcp v3 supports **two registry backends**:

#### Backend 1: Global Registry (`erlmcp_registry_distributed.erl`)

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_registry_distributed.erl` (lines 1-100)

Uses Erlang's native `global` module + `pg` (process groups):

```erlang
%% Register entity with global name
register(Type, Id, Pid, Config) ->
    GlobalName = make_global_name(Type, Id),
    case global:register_name(GlobalName, Pid) of
        yes ->
            %% Join appropriate groups
            Groups = determine_groups(Type, Config),
            lists:foreach(fun(Group) -> ok = join_group(Group, Pid) end, Groups),
            ok;
        no ->
            {error, {already_registered, ExistingPid}}
    end.
```

**Process Groups** (defined at line 44-50):
- `mcp_all_servers`: All server processes across cluster
- `mcp_all_transports`: All transport processes
- `mcp_tool_servers`: Tool-providing servers
- `mcp_resource_servers`: Resource-providing servers
- `mcp_prompt_servers`: Prompt-providing servers

**Advantages**:
- No external dependencies
- Native clustering support
- Automatic conflict resolution
- Built-in failover via pg

**Trade-offs**:
- Slower than local gproc (2-3x overhead)
- Global locks may impact scalability
- No property/counter support

#### Backend 2: Distribution Registry (`erlmcp_distribution_registry.erl`)

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_distribution_registry.erl` (lines 1-100)

Version-aware registry with OTP 26-28 optimization:

```erlang
%% OTP 26-28 compatible registration
register(Type, Id, Pid, Config) when is_atom(Type), is_pid(Pid), is_map(Config) ->
    gen_server:call(?MODULE, {register, Type, Id, Pid, Config}, 5000).

%% Uses features based on OTP version:
%% OTP 26+: Process iterators for efficient enumeration
%% OTP 27+: Priority messages for better scheduling
%% OTP 28+: Distributed tracing integration
```

### 2.3 Request Routing via Registry

Requests are routed through the registry to registered servers:

```erlang
%% File: erlmcp_registry.erl
route_to_server(ServerId, TransportId, Message) ->
    case find_server(ServerId) of
        {ok, {_Node, ServerPid, _Config}} ->
            ServerPid ! {message, TransportId, Message};
        {error, not_found} ->
            {error, server_not_found}
    end.
```

---

## Part 3: Partition Tolerance and Split-Brain Prevention

### 3.1 Partition Detection

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_split_brain_detector.erl` (lines 1-100)

The split-brain detector monitors for network partitions:

```erlang
-record(split_brain_state, {
    strategy = winner_takes_all :: winner_takes_all | oldest_node | configured_master,
    master_node :: node() | undefined,
    partition_detected = false :: boolean(),
    last_check :: integer() | undefined,
    check_interval = 30000 :: pos_integer()  % 30 seconds
}).
```

**Partition Detection Strategies**:

#### 1. **Winner-Takes-All** (default)
- The partition with the majority of nodes continues
- Minority partition stops accepting writes
- Resolution: automatic when partition heals

#### 2. **Oldest-Node**
- The node with the earliest start time leads
- Provides deterministic leadership
- Use case: even-numbered clusters

#### 3. **Configured Master**
- Pre-configured master node is authoritative
- Minority partitions reject writes
- Use case: 2-node deployments

### 3.2 Overlay DNS and DNS Caching Behavior

**Principle** (from CLAUDE.md): *"Overlay DNS lies; retries + convergence"*

**Implications for erlmcp clusters**:

1. **Don't trust DNS for node discovery**
   ```erlang
   %% WRONG: Assuming DNS is stable
   Nodes = ["node1@erlmcp-0.erlmcp.default.svc.cluster.local",
            "node2@erlmcp-1.erlmcp.default.svc.cluster.local"],

   %% CORRECT: Combined with retries and health checks
   nodes_to_connect(Config) ->
       BaseNodes = maps:get(cluster_nodes, Config, []),
       HealthyNodes = filter_by_health(BaseNodes),
       %% Only trust nodes we've recently pinged
       filter_responsive_nodes(HealthyNodes).
   ```

2. **Implement retries with exponential backoff**
   ```erlang
   %% File: erlmcp_cluster.erl (lines 38-41)
   -define(DEFAULT_CONNECT_TIMEOUT, 5000).
   -define(MAX_RECONNECT_ATTEMPTS, 3).
   -define(HEARTBEAT_INTERVAL, 10000).
   ```

3. **Let the system converge**
   - Don't force immediate cluster formation
   - Wait for health checks to stabilize
   - Eventual consistency for distributed state

### 3.3 Recovery Narratives

When a partition occurs and heals, erlmcp follows this recovery sequence:

**1. Partition Detected** (split_brain_detector.erl)
- Node detects other partition has majority
- Stops accepting write operations
- Logs partition event with timestamp

**2. Minority Partition Behavior**
- Continues read-only operations
- Caches data for session continuity
- Tracks pending mutations

**3. Partition Heals**
- Nodes rejoin cluster
- Perform state reconciliation
- Catch up on missed writes (eventual consistency)

**4. Resumption**
- Minority partition resumes writes
- Sessions restored from backup region
- Metrics logged for post-incident analysis

---

## Part 4: Cluster-Churn Safety

### 4.1 Handling Node Joins

**Assumption**: Nodes can join at any time (no bootstrapping requirement)

```erlang
%% File: erlmcp_cluster.erl
%% When a new node connects:
handle_info({nodeup, Node, _Info}, State) ->
    %% 1. Add to monitored nodes
    NewNodes = maps:put(Node, #node_state{...}, State#state.nodes),

    %% 2. Request replication data
    erlmcp_registry:sync_from_node(Node),
    erlmcp_session_replicator:sync_sessions(Node),

    %% 3. Stabilize cluster
    {noreply, State#state{nodes = NewNodes}}.
```

### 4.2 Handling Node Departures

**Assumption**: Nodes can leave (crash, network partition, maintenance)

```erlang
%% File: erlmcp_node_monitor.erl (lines 50-51)
ok = net_kernel:monitor_nodes(true, [nodedown_reason]).

%% Automatic cleanup on node down:
%% 1. Remove from registry
%% 2. Fail over sessions
%% 3. Rebalance load
```

### 4.3 Cluster-Churn Resilience Pattern

All processes follow the **let-it-crash** pattern:

```erlang
%% File: erlmcp_core_sup.erl
%% Supervision tree structure (from CLAUDE.md):
%% Tier 1: one_for_all (core + registry)
%% Tier 2: per-connection simple_one_for_one
%% Tier 3: isolated observability

%% Result: If one node crashes, others continue serving traffic
%% No single point of failure for the cluster
```

---

## Part 5: Failover Mechanisms

### 5.1 Health Check and Failover Manager

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_failover_manager.erl` (lines 1-80)

```erlang
-record(state, {
    primary_nodes :: [node()],
    secondary_nodes :: [node()],
    tertiary_nodes :: [node()],
    health_check_interval :: pos_integer(),
    failover_threshold :: pos_integer(),  % 3 consecutive failures
    current_active :: 'primary' | 'secondary' | 'tertiary'
}).

-define(HEALTH_CHECK_INTERVAL, 5000).  % 5 seconds
-define(FAILOVER_THRESHOLD, 3).         % Fail after 3 checks
-define(FAILOVER_TIMEOUT, 10000).       % 10 seconds
```

### 5.2 Failover Decision Logic

**Trigger**: 3 consecutive failed health checks (15 seconds)

**Decision process**:
1. Check health of current region
2. If unhealthy, evaluate secondaries
3. Fail over to secondary with lowest latency
4. Update global load balancer

**Session Handling during Failover**:

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_session_ha.erl` (lines 1-80)

```erlang
-record(session_info, {
    session_id :: binary(),
    primary_node :: node(),
    secondary_nodes :: [node()],      % Replicas for failover
    session_data :: map(),
    last_updated :: integer(),
    replication_status :: 'synced' | 'pending' | 'failed' | 'syncing'
}).

%% Failover a session to new primary
failover_session(SessionId, NewPrimaryNode) ->
    gen_server:call(?MODULE, {failover_session, SessionId, NewPrimaryNode}).
```

**Session Replication**:
- Asynchronous replication to 2 secondary nodes (configurable)
- RTO (Recovery Time Objective): < 1 second
- RPO (Recovery Point Objective): < 5 seconds

### 5.3 Multi-Region Failover

For multi-region deployments:

```yaml
# Regions with priorities
primary:    us-east-1     (100% traffic)
secondary:  eu-west-1     (0% traffic, standby)
tertiary:   ap-southeast-1 (0% traffic, cold backup)

# Failover sequence
primary down?
  └─> secondary up?
       └─> yes: shift 100% traffic, replicate from secondary
       └─> no: check tertiary, shift based on health
```

---

## Part 6: Network Topology Considerations

### 6.1 Inter-Node Communication

**File**: `/home/user/erlmcp/cluster/cluster-architecture.md` (lines 574-599)

Erlang distribution uses configurable port ranges:

```erlang
% config/vm.config
{kernel, [
    {inet_dist_listen_min, 9100},
    {inet_dist_listen_max, 9109},        % 10-node cluster
    {net_ticktime, 60},                  % 60 seconds between ticks
    {dist_auto_connect, once},           % Auto-connect once
    {scheduler_width, 2}
]}.
```

**TCP/Network Optimization** (sysctl):
```bash
# For high-performance networking
net.core.rmem_max = 134217728           # 128 MB RX buffer
net.core.wmem_max = 134217728           # 128 MB TX buffer
net.ipv4.tcp_congestion_control = bbr   # BBR congestion control
net.core.somaxconn = 65535              # Max listen queue
```

### 6.2 Latency-Based Routing

For multi-region deployments, consider latency:

```yaml
regions:
  - name: "us-east-1"
    latency_ms: 0      (primary region)
  - name: "eu-west-1"
    latency_ms: 50     (secondary)
  - name: "ap-southeast-1"
    latency_ms: 150    (tertiary)
```

**Routing Decision**:
- Route requests to lowest-latency healthy node
- Update weights based on observed latency
- Failover if latency exceeds threshold

---

## Part 7: Cluster Deployment and Configuration

### 7.1 Static Cluster Configuration

**File**: `config/erlmcp-cluster.config` (or `.env`)

```erlang
[
  {erlmcp_core, [
    %% Cluster nodes to connect to
    {cluster_nodes, ['erlmcp@node-1', 'erlmcp@node-2', 'erlmcp@node-3']},

    %% Health check intervals
    {node_check_interval, 5000},          % 5 seconds
    {split_brain_check_interval, 30000},  % 30 seconds

    %% Partition handling
    {split_brain_strategy, winner_takes_all},

    %% Session replication
    {replication_factor, 2},              % 1 primary + 1 replica
    {replication_timeout, 5000},          % 5 seconds

    %% Failover thresholds
    {failover_threshold, 3},              % 3 consecutive failures
    {health_check_timeout, 2000}          % 2 seconds per check
  ]}
].
```

### 7.2 Dynamic Cluster Join (Kubernetes)

For Kubernetes deployments, use StatefulSet with headless service:

```yaml
# manifests/erlmcp-cluster.yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: erlmcp-cluster
spec:
  serviceName: erlmcp-cluster-headless
  replicas: 3
  selector:
    matchLabels:
      app: erlmcp
  template:
    metadata:
      labels:
        app: erlmcp
    spec:
      containers:
      - name: erlmcp
        image: erlmcp:latest
        env:
        - name: ERLANG_NODENAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        - name: ERLANG_CLUSTER_NODES
          value: "erlmcp-0,erlmcp-1,erlmcp-2"
        ports:
        - containerPort: 8080
          name: api
        - containerPort: 9100
          name: dist-min
        - containerPort: 9109
          name: dist-max

---
apiVersion: v1
kind: Service
metadata:
  name: erlmcp-cluster-headless
spec:
  clusterIP: None
  selector:
    app: erlmcp
  ports:
  - port: 9100
    targetPort: 9100
    name: dist
```

---

## Part 8: Monitoring and Observability

### 8.1 Cluster Health Metrics

Key metrics to monitor:

| Metric | Healthy | Warning | Critical |
|--------|---------|---------|----------|
| `erlmcp_cluster_nodes_up` | 3/3 | 2/3 | <2/3 |
| `erlmcp_node_latency_ms` | <50 | 50-100 | >100 |
| `erlmcp_session_replication_lag_ms` | <100 | 100-500 | >500 |
| `erlmcp_partition_detected` | false | - | true |
| `erlmcp_failover_time_ms` | <1000 | - | >3000 |

### 8.2 Logging and Tracing

**Cluster events are logged to**:
- Node up/down: `erlmcp_node_monitor`
- Partition detection: `erlmcp_split_brain_detector`
- Failover events: `erlmcp_failover_manager`
- Session replication: `erlmcp_session_ha`

**Example log entries**:
```
[notice] erlmcp_split_brain_detector: Partition detected at node erlmcp@node-2
[notice] erlmcp_failover_manager: Initiating failover to secondary region
[error] erlmcp_session_replicator: Session replication timeout for session_id=abc123
[info] erlmcp_cluster: Node erlmcp@node-1 reconnected after partition
```

### 8.3 Health Check Endpoints

**`GET /health`**
- Returns 200 if node is healthy and cluster is quorate
- Returns 503 if partition detected and node is in minority
- Returns 503 if unable to connect to any peer

**`GET /ready`**
- Returns 200 if all sessions have completed replication
- Used by Kubernetes readiness probes

---

## Part 9: Disaster Recovery Procedures

### 9.1 Complete Cluster Failure

**Scenario**: All nodes down simultaneously

**Recovery**:
1. Start primary node: `erlmcp start`
2. Node detects cluster is not quorate (no peer nodes)
3. Node enters "standalone" mode (local operations only)
4. Start secondary nodes
5. Nodes detect peers, join cluster
6. Run reconciliation: `erlmcp admin reconcile_state`

### 9.2 Minority Partition Recovery

**Scenario**: Network partition, minority partition becomes isolated

**Automatic recovery**:
1. Minority partition detects it's in minority
2. Stops accepting writes (logs to change queue)
3. Waits for partition to heal
4. When rejoined: replays change queue, catches up to primary
5. Resumes accepting writes

**Manual intervention** (if partition lasts >1 hour):
```bash
# Force minority partition to become primary (dangerous!)
erlmcp admin force_leader

# Sync state from healthy region
erlmcp admin sync_from_peer erlmcp@primary-node
```

### 9.3 Data Corruption or Inconsistency

**Detection**:
- Hash mismatch during session replication
- Timestamp anomalies in distributed state

**Recovery**:
```bash
# Verify cluster state
erlmcp admin verify_state

# Rebuild state from authoritative region
erlmcp admin rebuild_from_region primary

# Validate sessions after rebuild
erlmcp admin validate_sessions
```

---

## Part 10: Testing and Validation

### 10.1 Chaos Engineering Tests

**Docker-only execution** (as per CLAUDE.md):

```bash
# Test node failure
docker compose exec erlmcp-build \
  rebar3 eunit tests=erlmcp_cluster_SUITE

# Test partition tolerance
docker compose exec erlmcp-ct \
  ct_run -suite erlmcp_partition_SUITE

# Test failover under load
docker compose exec erlmcp-bench \
  rebar3 bench -scope failover
```

### 10.2 Validation Checklist

Before production deployment:

- [ ] **Cluster formation**: Nodes join without manual intervention
- [ ] **Partition handling**: Minority partition stops writes within 15s
- [ ] **Failover**: Session migration completes within 1s
- [ ] **Recovery**: Healed partition catches up within 30s
- [ ] **Churn safety**: Nodes can join/leave without cluster instability
- [ ] **DNS resilience**: Works despite DNS delays/timeouts
- [ ] **Replication lag**: Monitored and < 5 seconds
- [ ] **Split-brain prevention**: Prevents write conflicts

---

## Part 11: Configuration Reference

### Environment Variables

```bash
# Cluster composition
ERLMCP_CLUSTER_NODES=erlmcp@node-1,erlmcp@node-2,erlmcp@node-3

# Health check tuning
ERLMCP_NODE_CHECK_INTERVAL=5000        # ms
ERLMCP_HEALTH_CHECK_TIMEOUT=2000       # ms
ERLMCP_FAILOVER_THRESHOLD=3            # consecutive failures

# Partition handling
ERLMCP_SPLIT_BRAIN_STRATEGY=winner_takes_all
ERLMCP_MASTER_NODE=undefined           # For 'configured_master' strategy

# Session replication
ERLMCP_REPLICATION_FACTOR=2
ERLMCP_REPLICATION_TIMEOUT=5000        # ms
ERLMCP_SESSION_CLEANUP_INTERVAL=300000 # 5 minutes

# Network tuning
ERLMCP_DIST_AUTO_CONNECT=once
ERLMCP_NET_TICKTIME=60                 # seconds
```

---

## Part 12: Troubleshooting Guide

### Problem: Nodes Not Forming Cluster

**Symptoms**:
- `erlmcp_cluster: connect timeout` in logs
- Nodes see each other as `down`

**Solution**:
```bash
# 1. Verify DNS resolution
nslookup erlmcp-1.erlmcp.default.svc.cluster.local

# 2. Check network connectivity
docker exec erlmcp-node-1 nc -zv erlmcp-node-2 9100

# 3. Verify node names match cluster config
docker logs erlmcp-node-1 | grep "erlmcp@"

# 4. Restart with fresh cluster
docker compose down && docker compose up -d
```

### Problem: Partition Detected But Not Healing

**Symptoms**:
- `erlmcp_split_brain_detector: Partition detected` persists
- Nodes won't rejoin despite being connected

**Solution**:
```bash
# 1. Check network policy rules
kubectl describe networkpolicies

# 2. Check if minority partition
erlmcp admin get_cluster_status

# 3. Force reconciliation (last resort)
erlmcp admin force_leader
erlmcp admin reconcile_state
```

### Problem: High Session Replication Lag

**Symptoms**:
- `erlmcp_session_replicator: Replication lag > 5s` in metrics
- Failover loses recent session updates

**Solution**:
```bash
# 1. Check network latency
docker exec erlmcp-node-1 ping -c 3 erlmcp-node-2

# 2. Increase replication timeout
export ERLMCP_REPLICATION_TIMEOUT=10000

# 3. Reduce session update frequency (application-level)
```

---

## Conclusion

erlmcp v3's clustering system is built on **proven Erlang/OTP patterns** with partition awareness, cluster-churn safety, and deterministic failover. By following the principles in this guide—especially "overlay DNS lies; retries + convergence"—you can deploy erlmcp clusters that are resilient to network issues, node failures, and datacenter outages.

**Key Takeaways**:

1. **Trust nothing**: DNS can lie, nodes can crash, partitions happen
2. **Converge eventually**: Let the system reach consistency rather than forcing it
3. **Monitor everything**: Cluster health is the foundation for HA
4. **Test chaos**: Failures happen in production; validate your recovery procedures
5. **Document decisions**: Recovery narratives explain how your system recovers from failure

For additional information, see:
- `/home/user/erlmcp/cluster/cluster-architecture.md` - Infrastructure architecture
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_cluster.erl` - Cluster implementation
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_split_brain_detector.erl` - Partition handling
