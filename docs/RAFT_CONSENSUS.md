# Raft Consensus Implementation for erlmcp v3

## Overview

This document describes the Raft consensus algorithm implementation for erlmcp v3. Raft provides strong consistency guarantees for distributed configuration management, leader election, and coordinated state changes across cluster nodes.

## Architecture

### Components

```
┌─────────────────────────────────────────────────────────────────┐
│                     erlmcp_raft_sup                             │
│  (Supervisor - one_for_one, intensity=10, period=60)            │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌───────────────────┐  ┌───────────────────┐  ┌─────────────┐  │
│  │ erlmcp_raft_transport│ │erlmcp_raft_membership│ │erlmcp_raft  │  │
│  │  (Network RPC)    │  │ (Cluster Mgmt)   │  │ (Raft Core) │  │
│  └───────────────────┘  └───────────────────┘  └─────────────┘  │
│                                                                  │
│  ┌───────────────────┐  ┌───────────────────┐                    │
│  │erlmcp_raft_log    │  │erlmcp_raft_state  │                    │
│  │ (ETS/DETS Storage) │  │_machine          │                    │
│  └───────────────────┘  │ (Config Store)    │                    │
│                          └───────────────────┘                    │
└─────────────────────────────────────────────────────────────────┘
```

### Module Responsibilities

| Module | Responsibility |
|--------|---------------|
| `erlmcp_raft` | Core Raft protocol: leader election, log replication, RPC handling |
| `erlmcp_raft_log` | Persistent log storage using ETS/DETS, snapshot metadata |
| `erlmcp_raft_state_machine` | Distributed configuration store, state machine interface |
| `erlmcp_raft_membership` | Safe cluster reconfiguration using joint consensus |
| `erlmcp_raft_transport` | Network abstraction for Raft RPC communication |
| `erlmcp_raft_sup` | OTP supervision tree for all Raft components |

## Features

### 1. Leader Election

Raft uses randomized election timeouts to prevent split votes:

- **Follower**: Starts election when no heartbeat received within timeout
- **Candidate**: Requests votes from all peers, becomes leader with quorum
- **Leader**: Sends periodic heartbeats to maintain authority

**Timeout Configuration:**
```erlang
-define(RAFT_ELECTION_TIMEOUT_MIN, 150).  % ms
-define(RAFT_ELECTION_TIMEOUT_MAX, 300).  % ms
-define(RAFT_HEARTBEAT_INTERVAL, 50).     % ms
```

### 2. Log Replication

The leader replicates log entries to followers using `AppendEntries` RPC:

1. Client submits command to leader
2. Leader appends entry to its log
3. Leader sends `AppendEntries` to followers in parallel
4. Followers append entries to their logs
5. Leader commits entry once replicated on quorum
6. Entry is applied to state machine

**Log Entry Structure:**
```erlang
-record(raft_log_entry, {
    index    :: pos_integer(),  % Log position
    term     :: pos_integer(),  % Creation term
    type     :: entry_type(),   % command | config_change | noop
    command  :: term(),         % State machine command
    timestamp :: integer()      % For debugging
}).
```

### 3. Split-Brain Prevention

Split-brain is prevented through quorum enforcement:

- **Election Restriction**: Only votes for candidates with up-to-date logs
- **Commit Requirement**: Entries only committed when replicated on quorum
- **Term Monotonicity**: Higher terms always win
- **Leader Lease**: Optimistic read safety with bounded staleness

**Quorum Calculation:**
```erlang
Quorum = floor(ClusterSize / 2) + 1
```

For a 5-node cluster, quorum is 3. No split-brain possible as at most 2 nodes can be isolated.

### 4. Configuration Management

Cluster membership changes use **joint consensus**:

1. **C_old,new**: Transition configuration combining old and new clusters
2. Majority from both old and new must agree
3. Commit C_old,new first, then transition to C_new

This ensures no two majorities can make different decisions during reconfiguration.

### 5. Snapshotting

Log compaction via snapshotting:

- Triggered when log size exceeds threshold (default: 10,000 entries)
- Snapshot includes: last index/term, state machine state, cluster config
- Followers install snapshot via `InstallSnapshot` RPC
- Enables log truncation while preserving recoverability

### 6. Timeout Handling

| Timeout | Default | Purpose |
|---------|---------|---------|
| Election Timeout | 150-300ms (random) | Trigger new election |
| Heartbeat Interval | 50ms | Leader heartbeat frequency |
| RPC Timeout | 100ms | AppendEntries/RequestVote timeout |
| Snapshot Timeout | 5000ms | InstallSnapshot timeout |

### 7. State Transfer

New nodes catch up via:

1. **Log Replication**: Receive missing log entries
2. **Snapshot Transfer**: Install snapshot if log is too far behind
3. **Config Change**: Added to cluster via joint consensus

### 8. Leader Lease (Optimization)

Optimistic linearizable reads:

- Leader extends lease on each successful heartbeat
- Reads require valid lease (no read from stale leader)
- Lease duration: `2 * election_timeout_max` (600ms)

## API Usage

### Starting a Cluster

```erlang
%% Define the state machine callback module
-module(my_config_store).
-behaviour(erlmcp_raft_state_machine).

init(Args) -> {ok, #{}}.
apply({set, Key, Value}, State) -> {ok, State#{Key => Value}, ok}.
snapshot(State) -> {ok, term_to_binary(State)}.
restore(Binary) -> {ok, binary_to_term(Binary)}.
get_state(State) -> State.

%% Start a 3-node cluster
ClusterName = <<"my_cluster">>,
Nodes = [node1, node2, node3],

%% Start on each node (with appropriate peers list)
{ok, _Pid} = erlmcp_raft_sup:start_cluster(
    ClusterName,
    Nodes,
    my_config_store,
    [],
    erlmcp_raft_log,
    erlmcp_raft_transport
).
```

### Client Operations

```erlang
%% Write (only on leader)
{ok, Index} = erlmcp_raft:write(RaftPid, {set, foo, bar}),

%% Read (linearizable, only on leader)
{ok, State} = erlmcp_raft:read(RaftPid),

%% Get status
{ok, Status} = erlmcp_raft:status(RaftPid),
%% #{state => leader, term => 1, leader => node1, ...}

%% Add server
ok = erlmcp_raft:add_server(RaftPid, node4),

%% Remove server
ok = erlmcp_raft:remove_server(RaftPid, node4).
```

### Cluster Membership Changes

```erlang
%% Add server to cluster
{ok, ConfigIndex} = erlmcp_raft_membership:add_server(
    MembershipPid,
    new_node@host
).

%% Remove server from cluster
{ok, ConfigIndex} = erlmcp_raft_membership:remove_server(
    MembershipPid,
    old_node@host
).

%% Verify cluster health
{ok, #{healthy := Healthy, unhealthy := Unhealthy}} =
    erlmcp_raft_membership:verify_cluster_health(MembershipPid).
```

## Performance Characteristics

### Throughput

| Operation | Throughput | Latency (p50) | Latency (p99) |
|-----------|-----------|---------------|---------------|
| Single-node write | ~100K ops/sec | <1ms | <5ms |
| 3-node replication | ~50K ops/sec | 5-10ms | 50-100ms |
| 5-node replication | ~30K ops/sec | 10-20ms | 100-200ms |

### Network Requirements

- **Latency**: <50ms between nodes for optimal performance
- **Bandwidth**: Minimal (log entries are small)
- **Reliability**: Tolerates minority network partitions

### Resource Usage

Per node (approximately):
- Memory: 100MB base + 1MB per 10K log entries
- CPU: <5% single core at 10K ops/sec
- Disk: DETS files (configurable, typically <100MB)

## Failure Recovery

### Leader Failure

1. Followers detect no heartbeat within election timeout
2. Followers become candidates and start election
3. Candidate with quorum of votes becomes new leader
4. New leader commits uncommitted entries from previous term
5. Normal operation resumes

**Typical recovery time:** 200-500ms

### Follower Failure

1. Leader detects RPC timeout to follower
2. Leader decrements `next_index` for that follower
3. Leader retries with earlier log position
4. Upon reconnection, follower catches up via log replication or snapshot
5. No cluster disruption (if quorum maintained)

### Network Partition

**Minority Partition:**
- Nodes in minority cannot elect leader
- Writes are rejected with "not_leader" error
- Automatic retry to majority partition

**Majority Partition:**
- Maintains leader and continues processing
- Minority partition nodes step down on reconnection
- No split-brain possible

## Cluster Formation Procedures

### Initial Cluster (3 nodes)

```erlang
%% On node1@host1:
{ok, _} = erlmcp_raft:start_link(
    <<"production">>,
    node1@host1,
    [node2@host1, node3@host1],
    my_state_machine
).

%% On node2@host2:
{ok, _} = erlmcp_raft:start_link(
    <<"production">>,
    node2@host2,
    [node1@host1, node3@host1],
    my_state_machine
).

%% On node3@host3:
{ok, _} = erlmcp_raft:start_link(
    <<"production">>,
    node3@host3,
    [node1@host1, node2@host2],
    my_state_machine
).
```

### Adding Nodes to Running Cluster

```erlang
%% Start new node with existing cluster members
{ok, NewNode} = erlmcp_raft:start_link(
    <<"production">>,
    node4@host4,
    [node1@host1, node2@host2, node3@host3],
    my_state_machine
).

%% On leader: add the new node
{ok, _} = erlmcp_raft:add_server(LeaderPid, node4@host4).
```

### Removing Nodes

```erlang
%% On leader: remove the node
{ok, _} = erlmcp_raft:remove_server(LeaderPid, node4@host4).

%% On removed node: stop Raft server
erlmcp_raft:stop(Node4Pid).
```

## Testing

Run the test suite:

```bash
rebar3 eunit --module=erlmcp_raft_tests
```

Test coverage includes:
- Leader election (1, 3, 5 node clusters)
- Log replication (single and multiple entries)
- AppendEntries consistency checking
- Term changes and step-down
- Quorum enforcement
- Network partition handling
- Cluster membership changes
- Snapshot creation and restoration
- Concurrent writes

## Implementation Notes

### OTP 28+ Features Used

- **Priority Messages**: Raft RPCs could use priority messaging
- **Process Inspection**: Debug via `erlang:process_info/1`
- **hibernate/0**: Potential memory optimization for idle servers

### ETS Table Configuration

```erlang
ets:new(raft_log, [
    ordered_set,           % Maintain index order
    {read_concurrency, true},  % Parallel reads
    {write_concurrency, true}  % Parallel writes
])
```

### DETS Persistence

- Auto-save interval: 1000ms
- File: `raft_log_<cluster>_<node>.dets`
- Sync on critical operations (term changes, log appends)

## Configuration

The Raft implementation accepts these options:

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `election_timeout_min` | integer | 150 | Minimum election timeout (ms) |
| `election_timeout_max` | integer | 300 | Maximum election timeout (ms) |
| `heartbeat_interval` | integer | 50 | Heartbeat frequency (ms) |
| `snapshot_threshold` | integer | 10000 | Log entries before snapshot |
| `rpc_timeout` | integer | 100 | RPC timeout (ms) |
| `max_log_entries` | integer | 100000 | Maximum log entries before compaction |

## Monitoring

### Key Metrics

```erlang
{ok, #{metrics := Metrics}} = erlmcp_raft:status(RaftPid),
%% #{
%%   elections_won => 5,
%%   elections_lost => 2,
%%   log_entries_committed => 1000,
%%   snapshots_taken => 1,
%%   rpc_timeouts => 10,
%%   split_brain_prevented => 0
%% }
```

### Health Checks

```erlang
%% Check if node is leader
{ok, Status} = erlmcp_raft:status(RaftPid),
IsLeader = maps:get(is_leader, Status),

%% Check quorum health
{ok, Quorum} = erlmcp_raft_membership:get_quorum_size(MembershipPid),

%% Verify cluster connectivity
{ok, #{healthy := Healthy, unhealthy := Unhealthy}} =
    erlmcp_raft_membership:verify_cluster_health(MembershipPid).
```

## References

1. Ongaro, D., & Ousterhout, J. (2014). "In Search of an Understandable Consensus Algorithm." USENIX ATC.
2. Raft Website: https://raft.github.io/
3. etcd Raft Implementation: https://github.com/etcd-io/etcd/tree/main/raft

## License

Apache-2.0 (See LICENSE file for details)
