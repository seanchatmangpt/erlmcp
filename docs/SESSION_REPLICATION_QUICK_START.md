# Session Replication Quick Start Guide

## 5-Minute Setup

### 1. Start the Cluster

```erlang
%% On node 1 (primary)
{ok, _} = erlmcp_session_replicator:start_link().
ok = erlmcp_session_replicator:start_cluster([node2@host, node3@host, node4@host]).

%% Verify cluster is up
Status = erlmcp_session_replicator:get_replication_status().
```

### 2. Store and Retrieve Sessions

```erlang
%% Store a session (1-day expiry = 86400000 ms)
SessionData = #{user_id => <<"alice">>, role => admin},
ok = erlmcp_session_replicator:store_session(
    <<"session_123">>,
    SessionData,
    86400000
).

%% Retrieve later
{ok, Data} = erlmcp_session_replicator:get_session(<<"session_123">>).

%% Delete when done
ok = erlmcp_session_replicator:delete_session(<<"session_123">>).
```

### 3. Monitor Cluster Health

```erlang
%% Check replication status
Status = erlmcp_session_replicator:get_replication_status(),
io:format("Active sessions: ~w~n", [maps:get(active_sessions, Status)]),
io:format("Pending replications: ~w~n", [maps:get(pending_replications, Status)]).

%% Check performance metrics
Metrics = erlmcp_session_replicator:get_metrics(),
io:format("Replication lag: ~w ms~n", [maps:get(replication_lag_ms, Metrics)]).
```

---

## Common Tasks

### Load 10K Sessions for Testing

```erlang
load_sessions(Count) ->
    lists:foreach(fun(Id) ->
        SessionId = erlang:integer_to_binary(Id),
        SessionData = #{
            user_id => SessionId,
            timestamp => erlang:system_time(millisecond)
        },
        erlmcp_session_replicator:store_session(SessionId, SessionData, 3600000)
    end, lists:seq(1, Count)).

%% Run it
load_sessions(10000).
```

### Get Session Count

```erlang
{ok, AllSessions} = erlmcp_session_replicator:get_all_sessions(),
length(AllSessions).
```

### Manually Flush Replications

```erlang
%% Call promote_to_primary to flush pending queue
ok = erlmcp_session_replicator:promote_to_primary().
```

### Simulate Node Failure

```erlang
%% Manually handle failure of node2
ok = erlmcp_session_replicator:trigger_failover(node2@host).

%% Failover manager will automatically:
%% 1. Detect node2 is down
%% 2. Promote secondary to primary
%% 3. Resume session operations
```

---

## Performance Expectations

| Operation | Latency | Throughput |
|-----------|---------|-----------|
| Get session (local) | 1-10 μs | N/A |
| Store session | 1-100 ms | 10K-20K/sec |
| Replication (p99) | 20-100 ms | Batched |
| Failover | 3-5 sec | N/A |

---

## Configuration Quick Reference

```erlang
%% config/sys.config
{session_replication, [
    {enabled, true},
    {replica_nodes, []},           % Add: [node2, node3, node4]
    {batch_size, 100},             % Replication batch size
    {batch_timeout_ms, 20},        % Flush every 20ms
    {replication_rpc_timeout_ms, 5000},
    {health_check_interval_ms, 5000},  % Check nodes every 5s
    {failure_threshold, 3},        % Fail after 3 failed checks
    {storage_mode, ets},           % ets|mnesia|redis
    {cleanup_interval_ms, 30000},  % Clean expired sessions
    {enable_compression, false},
    {max_session_size_bytes, 65536}
]}
```

---

## Testing

```bash
# Run full load test
rebar3 ct --suite=erlmcp_session_replication_load_test_SUITE

# Run specific test
rebar3 ct --suite=erlmcp_session_replication_load_test_SUITE --case test_11_stress_100k_with_ops

# Get detailed output
rebar3 ct --suite=erlmcp_session_replication_load_test_SUITE --verbose
```

---

## Troubleshooting

### Replication Not Working
```erlang
%% Check status
Status = erlmcp_session_replicator:get_replication_status(),
maps:get(pending_replications, Status).  % Should be 0 if caught up

%% Check replica nodes
maps:get(replica_nodes, Status).  % Should list all replicas
```

### High Latency
```erlang
%% Check lag
Metrics = erlmcp_session_replicator:get_metrics(),
maps:get(replication_lag_ms, Metrics).  % Should be <100ms

%% If high:
%% 1. Increase batch_size in config
%% 2. Check network latency: ping replica_node
%% 3. Check logs for RPC failures
```

### Sessions Lost After Restart
```erlang
%% Expected behavior: ETS data lost on restart
%% To prevent:
%% 1. Configure mnesia backend: {storage_mode, mnesia}
%% 2. Or use redis: {storage_mode, redis}
%% 3. Or reload from database on startup
```

---

## API Summary

```erlang
%% Start
erlmcp_session_replicator:start_link()
erlmcp_session_replicator:start_cluster(ReplicaNodes)

%% CRUD
erlmcp_session_replicator:store_session(SessionId, Data, TtlMs)
erlmcp_session_replicator:get_session(SessionId)
erlmcp_session_replicator:delete_session(SessionId)
erlmcp_session_replicator:get_all_sessions()

%% Status
erlmcp_session_replicator:get_replication_status()
erlmcp_session_replicator:get_metrics()

%% Failover
erlmcp_session_replicator:promote_to_primary()
erlmcp_session_replicator:trigger_failover(FailedNode)
```

---

**For full documentation, see: docs/SESSION_REPLICATION_SYSTEM.md**
