# OTP 26-28 Distribution Improvements for MCP Clustering

## Overview

This document describes the implementation of OTP 26-28 distribution improvements for multi-node MCP deployment in erlmcp, featuring enhanced connection setup, improved distribution flags, and better distributed tracing.

## Table of Contents

- [OTP Version Features](#otp-version-features)
- [Architecture](#architecture)
- [Components](#components)
- [Session Affinity Patterns](#session-affinity-patterns)
- [Distributed Tracing](#distributed-tracing)
- [Cluster Monitoring](#cluster-monitoring)
- [Configuration](#configuration)
- [Usage Examples](#usage-examples)
- [Performance Considerations](#performance-considerations)
- [Troubleshooting](#troubleshooting)

## OTP Version Features

### OTP 26: Improved Connection Setup

- **Async connection attempts**: Non-blocking node connections
- **Better error handling**: Detailed connection failure reasons
- **Connection pooling**: Efficient connection reuse

```erlang
%% OTP 26: Async connection setup
connect(Nodes) ->
    lists:foreach(fun(Node) ->
        net_kernel:connect_node(Node)  % Returns immediately, continues in background
    end, Nodes).
```

### OTP 27: Distribution Flag Improvements

- **Enhanced node monitoring**: `{node_type, all}` flag
- **Node down reasons**: `{nodedown_reason, true}` for detailed failure info
- **Tagged monitors**: Monitor with custom tags for easier identification

```erlang
%% OTP 27: Enhanced monitoring
net_kernel:monitor_nodes(true, [node_type, nodedown_reason]),
%% Handle events:
%% {nodeup, Node, InfoList}
%% {nodedown, Node, [{reason, Reason}]}
```

### OTP 28: Better Distributed Tracing

- **Native distributed tracing**: Built-in trace correlation
- **Priority queues**: Urgent control signals
- **Tagged process monitoring**: `{tag, Tag}` option in monitor/2

```erlang
%% OTP 28: Tagged monitoring
Ref = erlang:monitor(process, Pid, [{tag, {tool, ToolName}}]),
%% DOWN message: {'DOWN', {tool, ToolName}, process, Pid, Reason}
```

## Architecture

### 3-Tier Clustering Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   TIER 1: Orchestration                      │
│  ┌────────────────┐  ┌──────────────┐  ┌─────────────────┐ │
│  │ erlmcp_cluster │  │ session_aff  │  │ distr_tracer    │ │
│  │ (Node Mgmt)    │  │ (Routing)    │  │ (Tracing)       │ │
│  └────────────────┘  └──────────────┘  └─────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                   TIER 2: Coordination                       │
│  ┌────────────────┐  ┌──────────────┐  ┌─────────────────┐ │
│  │ registry_dist  │  │ node_monitor │  │ split_brain     │ │
│  │ (gproc global) │  │ (Health)     │  │ (Resolution)    │ │
│  └────────────────┘  └──────────────┘  └─────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                   TIER 3: Observability                       │
│  ┌────────────────┐  ┌──────────────┐  ┌─────────────────┐ │
│  │ cluster_monitor│  │ session_rep  │  │ otel_integration ││
│  │ (Metrics)      │  │ (Backup)     │  │ (Spans)         │ │
│  └────────────────┘  └──────────────┘  └─────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

### Data Flow

```
Client Request
    │
    ▼
[Session Affinity] → Check session location
    │                           │
    │                           ├── Local → Process directly
    │                           └── Remote → Route to session node
    ▼
[Distributed Tracer] → Inject trace ID
    │
    ▼
[erlmcp_cluster] → Distribute call across cluster
    │
    ▼
[Target Node] → Execute with trace correlation
    │
    ▼
[Cluster Monitor] → Track health and performance
```

## Components

### 1. erlmcp_cluster

**Purpose**: Core cluster management with OTP 26-28 features

**Key Functions**:
- `connect/1` - Connect to MCP nodes (OTP 26 async)
- `distribute_call/4,5` - RPC with timeout (OTP 27 flags)
- `cluster_status/0` - Node health tracking (OTP 28 tracing)
- `get_node_health/1` - Individual node health

**State Record**:
```erlang
-record(state,
        {nodes :: #{node() => #node_state{}},
         replication_factor :: pos_integer(),
         heartbeat_ref :: reference(),
         heartbeat_interval :: pos_integer()}).
```

**Usage**:
```erlang
%% Start cluster
{ok, _Pid} = erlmcp_cluster:start_link(#{
    heartbeat_interval => 10000,
    replication_factor => 2,
    nodes => [node1@host, node2@host]
}).

%% Connect to nodes
ok = erlmcp_cluster:connect([node1@host, node2@host]).

%% Distribute call
Result = erlmcp_cluster:distribute_call(
    node1@host,
    erlmcp_session_backend,
    fetch,
    [SessionId],
    5000
).

%% Get cluster status
Status = erlmcp_cluster:cluster_status().
%% [{node1@host, up, healthy}, {node2@host, up, healthy}]
```

### 2. erlmcp_session_affinity

**Purpose**: Session routing and migration for distributed sessions

**Key Functions**:
- `route_request/2` - Find node hosting session
- `migrate_session/3` - Move session between nodes
- `backup_session/2` - Replicate for failover
- `handle_failover/2` - Restore from backup

**Architecture**:
```erlang
%% Session location via gproc
Key = {n, g, {erlmcp_session_location, SessionId}},
gproc:reg_or_update(Key, SessionNode).
```

**Usage**:
```erlang
%% Route request to session node
{ok, SessionNode} = erlmcp_session_affinity:route_request(
    SessionId,
    Request
).

%% Migrate session (load balancing)
{ok, TargetNode} = erlmcp_session_affinity:migrate_session(
    SessionId,
    node2@host,
    [node3@host]  % Backup nodes
).

%% Handle failover on node crash
{ok, NewNode} = erlmcp_session_affinity:handle_failover(
    SessionId,
    FailedNode
).
```

### 3. erlmcp_distributed_tracer

**Purpose**: Trace correlation across cluster nodes (OTP 28)

**Key Functions**:
- `inject_trace_id/2` - Add request ID to message
- `extract_trace_id/1` - Get trace ID from message
- `correlate_traces/1` - Aggregate traces from all nodes
- `propagate_span/3` - Pass span context across nodes

**Trace Context**:
```erlang
-record(trace_ctx,
        {trace_id :: binary(),
         span_id :: binary(),
         parent_span_id :: binary() | undefined,
         node :: node(),
         timestamp :: integer()}).
```

**Usage**:
```erlang
%% Inject trace ID into cross-node message
{TraceCtx, TraceMsg} = erlmcp_distributed_tracer:inject_trace_id(
    Message,
    #{trace_id => <<"12345...">>}
).

%% Extract trace ID from incoming message
{TraceId, OriginalMsg} = erlmcp_distributed_tracer:extract_trace_id(
    IncomingMsg
).

%% Correlate traces from all nodes
{ok, ClusterTrace} = erlmcp_distributed_tracer:correlate_traces(
    TraceId
).
%% #{trace_id => ..., spans => [...], summary => ...}
```

### 4. erlmcp_cluster_monitor

**Purpose**: Cluster health monitoring and split-brain detection

**Key Functions**:
- `check_node_health/1` - Ping + process count
- `detect_partition/0` - Identify network split
- `handle_split_brain/2` - Majority-based resolution
- `get_cluster_metrics/0` - Health statistics

**Partition Detection**:
```erlang
%% Detect isolated groups
{ok, PartitionInfo} = erlmcp_cluster_monitor:detect_partition().
%% #{isolated_groups => [[node1, node2], [node3]],
%%    majority_group => [node1, node2],
%%    minority_groups => [[node3]],
%%    resolution_strategy => majority}
```

**Usage**:
```erlang
%% Check node health
{ok, healthy, #{process_count => 1234}} =
    erlmcp_cluster_monitor:check_node_health(node1@host).

%% Get cluster metrics
{ok, Metrics} = erlmcp_cluster_monitor:get_cluster_metrics().
%% #{total_nodes => 3,
%%    healthy_nodes => 2,
%%    degraded_nodes => 1,
%%    unhealthy_nodes => 0,
%%    partition_detected => false,
%%    last_check_time => 1699999999}
```

## Session Affinity Patterns

### Pattern 1: Route to Session Node

```erlang
%% Route request to node hosting session
route_request_to_session(SessionId, Request) ->
    {ok, SessionNode} = erlmcp_session_affinity:route_request(
        SessionId,
        Request
    ),
    case SessionNode of
        node() when SessionNode =:= node() ->
            %% Session is local, process directly
            handle_request_locally(Request);
        RemoteNode ->
            %% Forward to session node
            rpc:call(RemoteNode, ?MODULE, handle_request_locally, [Request])
    end.
```

### Pattern 2: Session Migration

```erlang
%% Migrate session for load balancing
migrate_session_for_balance(SessionId) ->
    %% Find least loaded node
    Nodes = erlmcp_cluster:cluster_nodes(),
    TargetNode = select_least_loaded_node(Nodes),

    %% Migrate session
    {ok, TargetNode} = erlmcp_session_affinity:migrate_session(
        SessionId,
        TargetNode,
        select_backup_nodes(TargetNode, Nodes)
    ).
```

### Pattern 3: Session Backup

```erlang
%% Backup session on replica nodes
backup_session(SessionId, Session) ->
    ok = erlmcp_session_affinity:backup_session(
        SessionId,
        Session
    ),
    %% Session is now replicated to backup nodes via erlmcp_session_replicator
    ok.
```

### Pattern 4: Failover Handling

```erlang
%% Automatic failover on node failure
handle_node_down(FailedNode) ->
    %% Find all sessions on failed node
    Sessions = get_sessions_on_node(FailedNode),

    %% Migrate each session to backup
    lists:foreach(fun(SessionId) ->
        {ok, NewNode} = erlmcp_session_affinity:handle_failover(
            SessionId,
            FailedNode
        ),
        logger:info("Session ~p migrated to ~p", [SessionId, NewNode])
    end, Sessions).
```

## Distributed Tracing

### Trace Propagation Flow

```
┌──────────────────────────────────────────────────────────────┐
│ Client Request (Trace ID: A)                                 │
└──────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌──────────────────────────────────────────────────────────────┐
│ Node 1: Inject Trace ID                                      │
│ - Create span A-1                                            │
│ - Inject trace ID into message                               │
│ - Route to Node 2                                            │
└──────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌──────────────────────────────────────────────────────────────┐
│ Node 2: Extract Trace ID, Create Child Span                  │
│ - Extract trace ID A                                         │
│ - Create span A-2 (parent: A-1)                              │
│ - Process request                                            │
└──────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌──────────────────────────────────────────────────────────────┐
│ Aggregate Traces                                             │
│ - Collect spans from Node 1 and Node 2                       │
│ - Build complete trace tree:                                 │
│   A-1 (Node 1)                                               │
│     └─ A-2 (Node 2)                                          │
└──────────────────────────────────────────────────────────────┘
```

### Integration with erlmcp_otel

```erlang
%% Create distributed span with erlmcp_otel integration
execute_distributed_call(Node, Module, Fun, Args) ->
    %% Start local span
    {ok, SpanCtx} = erlmcp_otel:start_span(
        <<"distributed_call">>,
        #{<<"target.node">> => atom_to_binary(Node)}
    ),

    %% Inject trace context
    TraceCtx = #{
        trace_id => erlmcp_otel:get_current_trace_id(),
        span_id => erlmcp_otel:get_current_span_id()
    },

    %% Make RPC call with trace propagation
    Result = rpc:call(Node, Module, Fun, Args, 5000),

    %% End span
    erlmcp_otel:end_span(SpanCtx),

    Result.
```

## Cluster Monitoring

### Health Check Flow

```
┌──────────────────────────────────────────────────────────────┐
│ Heartbeat Interval (10s)                                     │
└──────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌──────────────────────────────────────────────────────────────┐
│ Ping All Nodes                                               │
│ - net_adm:ping(Node)                                         │
│ - Get process count                                          │
│ - Calculate health score                                     │
└──────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌──────────────────────────────────────────────────────────────┐
│ Update Node States                                           │
│ - healthy: 0 failed pings                                    │
│ - degraded: 1-2 failed pings                                 │
│ - unhealthy: 3+ failed pings                                 │
└──────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌──────────────────────────────────────────────────────────────┐
│ Check for Partition                                           │
│ - Build connectivity graph                                   │
│ - Find connected components                                  │
│ - Detect isolated groups                                     │
└──────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌──────────────────────────────────────────────────────────────┐
│ Alert if Partition Detected                                   │
│ - Trigger split-brain resolution                             │
│ - Log partition details                                      │
└──────────────────────────────────────────────────────────────┘
```

### Split-Brain Resolution Strategies

#### 1. Majority Strategy (Default)

```erlang
%% Use majority group as surviving cluster
handle_split_brain(PartitionInfo, majority) ->
    #{majority_group := MajorityGroup} = PartitionInfo,
    %% Isolate minority nodes
    %% Continue with majority group
    {ok, MajorityGroup}.
```

#### 2. Oldest Node Strategy

```erlang
%% Find oldest node in cluster
handle_split_brain(PartitionInfo, oldest_node) ->
    #{isolated_groups := Groups} = PartitionInfo,
    %% Find group with oldest node
    OldestNode = find_oldest_node_across_groups(Groups),
    %% Use group containing oldest node
    SurvivingGroup = find_group_with_node(Groups, OldestNode),
    {ok, SurvivingGroup}.
```

#### 3. Configured Master Strategy

```erlang
%% Use pre-configured master node
handle_split_brain(PartitionInfo, configured_master) ->
    {ok, MasterNode} = application:get_env(erlmcp_core, master_node),
    #{isolated_groups := Groups} = PartitionInfo,
    %% Use group containing master node
    case find_group_with_node(Groups, MasterNode) of
        {ok, SurvivingGroup} ->
            {ok, SurvivingGroup};
        error ->
            {error, master_not_available}
    end.
```

## Configuration

### sys.config

```erlang
{erlmcp_core, [
    %% Enable clustering
    {cluster_enabled, true},

    %% Cluster nodes
    {cluster_nodes, [
        'erlmcp1@localhost',
        'erlmcp2@localhost',
        'erlmcp3@localhost'
    ]},

    %% Replication factor for sessions
    {replication_factor, 2},

    %% Split-brain strategy: winner_takes_all | oldest_node | configured_master
    {split_brain_strategy, winner_takes_all},

    %% Master node (if using configured_master strategy)
    {master_node, 'erlmcp1@localhost'},

    %% Heartbeat interval (ms)
    {cluster_heartbeat_interval, 10000},

    %% Alert thresholds
    {process_count_threshold, 1000},
    {failed_ping_threshold, 3},
    {partition_threshold, 3}
]}.
```

### Distributed Cookie

```bash
# Set same cookie on all nodes
export ERLANG_COOKIE="erlmcp_cluster_cookie"

# Start nodes with cookie
erl -setcookie erlmcp_cluster_cookie -name erlmcp1@localhost
```

## Usage Examples

### Example 1: Basic Cluster Setup

```erlang
%% 1. Start cluster on node1
%% On terminal 1:
$ erl -sname erlmcp1 -setcookie erlmcp
1> application:ensure_all_started(erlmcp_core).
{ok, [erlmcp_core]}

%% 2. Start cluster on node2
%% On terminal 2:
$ erl -sname erlmcp2 -setcookie erlmcp
1> application:ensure_all_started(erlmcp_core).
{ok, [erlmcp_core]}

%% 3. Connect nodes
2> erlmcp_cluster:connect(['erlmcp1@localhost']).
ok

%% 4. Check cluster status
2> erlmcp_cluster:cluster_status().
[{erlmcp1@localhost, up, healthy},
 {erlmcp2@localhost, up, healthy}]
```

### Example 2: Session Affinity

```erlang
%% Create session on node1
SessionId = <<"session-123">>,
Session = #{
    id => SessionId,
    created_at => erlang:system_time(millisecond),
    last_accessed => erlang:system_time(millisecond),
    timeout_ms => 3600000,
    metadata => #{}
},
ok = erlmcp_session_backend:store(SessionId, Session).

%% On node2, route request to session (which is on node1)
{ok, SessionNode} = erlmcp_session_affinity:route_request(
    SessionId,
    #{action => get_data}
).
%% Returns: {ok, 'erlmcp1@localhost'}
```

### Example 3: Distributed Tracing

```erlang
%% Start distributed tracing
ok = erlmcp_distributed_tracer:start_link().

%% Create trace context
TraceId = erlmcp_distributed_tracer:generate_trace_id(),
{TraceCtx, TraceMsg} = erlmcp_distributed_tracer:inject_trace_id(
    #{tool => <<"calculator">>, action => <<"add">>},
    TraceId
).

%% Send to remote node
rpc:call('erlmcp2@localhost', erlmcp_server, handle_request, [TraceMsg]).

%% Correlate traces
{ok, ClusterTrace} = erlmcp_distributed_tracer:correlate_traces(TraceId).
```

### Example 4: Split-Brain Recovery

```erlang
%% Detect partition
{ok, PartitionInfo} = erlmcp_cluster_monitor:detect_partition().
%% #{isolated_groups => [['erlmcp1@localhost'], ['erlmcp2@localhost', 'erlmcp3@localhost']],
%%    majority_group => ['erlmcp2@localhost', 'erlmcp3@localhost'],
%%    ...}

%% Resolve with majority strategy
{ok, SurvivingNodes} = erlmcp_cluster_monitor:handle_split_brain(
    PartitionInfo,
    majority
).
%% ['erlmcp2@localhost', 'erlmcp3@localhost']
```

## Performance Considerations

### 1. Network Latency

- **RPC calls**: Add 1-5ms per hop (LAN), 50-200ms (WAN)
- **Heartbeat overhead**: 1KB per node per 10s interval
- **Replication bandwidth**: Session size × replication factor

**Optimization**:
- Use local node preference when possible
- Batch heartbeat messages
- Compress session data before replication

### 2. Memory Usage

Per node (approximate):
- Cluster state: 1KB per node
- Session affinity: 500 bytes per session
- Trace buffer: 2KB per active trace
- Health history: 100 entries × 200 bytes = 20KB

**Example**: 3 nodes, 1000 sessions, 100 active traces
- Total: ~3KB + 500KB + 200KB + 20KB = ~723KB

### 3. Process Count

Cluster adds ~10 processes per node:
- 1 × erlmcp_cluster
- 1 × erlmcp_session_affinity
- 1 × erlmcp_distributed_tracer
- 1 × erlmcp_cluster_monitor
- 1 × erlmcp_registry_dist
- 1 × erlmcp_node_monitor
- 1 × erlmcp_split_brain_detector
- 3 × supporting processes

### 4. Scalability Limits

Recommended limits:
- **Max nodes**: 10 (practical), 50 (theoretical)
- **Max sessions**: 10K per node (with replication)
- **Max trace buffer**: 1000 concurrent traces
- **Heartbeat interval**: 5-30 seconds

## Troubleshooting

### Issue 1: Nodes Won't Connect

**Symptoms**: `net_adm:ping(Node)` returns `pang`

**Solutions**:
1. Check cookie: `erlang:get_cookie()`
2. Check hostname resolution: `inet:gethostbyname(hostname)`
3. Check EPMD: `epmd -names`
4. Check firewall: Allow ports 4369 (EPMD) + 25672+

```bash
# Debug connection
$ epmd -names
epmd: up and running on port 4369 with data:
name erlmcp1 at port 25672
name erlmcp2 at port 25673

# Test ping from Erlang
net_adm:ping('erlmcp2@localhost').  % Should return pong
```

### Issue 2: Split-Brain False Positives

**Symptoms**: Partition detected when network is fine

**Solutions**:
1. Increase `failed_ping_threshold` (default: 3)
2. Increase `heartbeat_interval` (default: 10s)
3. Check network latency: Use `ping` command

```erlang
%% Adjust thresholds
erlmcp_cluster_monitor:set_alert_threshold(failed_ping_count, 5).
```

### Issue 3: High Memory Usage

**Symptoms**: Memory grows continuously

**Solutions**:
1. Clear trace buffer periodically
2. Limit health history size
3. Clean up expired sessions

```erlang
%% Clear trace buffer
ets:delete_all_objects(erlmcp_distributed_traces).

%% Reduce health history
application:set_env(erlmcp_core, max_health_history, 50).
```

### Issue 4: Session Migration Fails

**Symptoms**: `{error, migration_failed}`

**Solutions**:
1. Check target node health
2. Verify replication factor
3. Check session size (large sessions timeout)

```erlang
%% Check node health first
{ok, Health, _} = erlmcp_cluster_monitor:check_node_health(TargetNode),
case Health of
    healthy -> Proceed;
    _ -> Abort and retry later
end.
```

### Issue 5: Trace Correlation Missing Spans

**Symptoms**: Some nodes not appearing in trace

**Solutions**:
1. Check `erlmcp_distributed_tracer` is running on all nodes
2. Verify trace ID propagation in messages
3. Check ETS table sizes

```erlang
%% Check tracer status
whereis(erlmcp_distributed_tracer).

%% Check trace table size
ets:info(erlmcp_distributed_traces, size).
```

## References

- [Erlang/OTP Distribution](https://www.erlang.org/doc/reference_manual/distributed.html)
- [gproc Documentation](https://github.com/uwiger/gproc)
- [OpenTelemetry Erlang](https://github.com/open-telemetry/opentelemetry-erlang)
- [erlmcp OTP Patterns](./otp-patterns.md)
- [Session Replication](./SESSION_PERSISTENCE.md)

---

**Version**: 2.1.0
**Last Updated**: 2026-02-01
**Author**: erlmcp development team
