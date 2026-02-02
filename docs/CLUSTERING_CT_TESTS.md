# Clustering Common Test Suite Documentation

## Overview

`erlmcp_clustering_SUITE` provides comprehensive integration testing for erlmcp clustering functionality using Common Test (CT). This suite validates distributed Erlang features across OTP 26-28.

## Test Structure

### Test Groups

The suite is organized into 6 test groups with sequential execution:

```erlang
all() ->
    [{group, node_connection},
     {group, session_affinity},
     {group, distributed_tracing},
     {group, split_brain},
     {group, cluster_monitoring},
     {group, distributed_tools}].
```

### Group 1: Node Connection Tests

Tests for OTP 26-28 node connection features.

#### `node_connection_sync/1`
- **Purpose**: Test synchronous node connection
- **Chicago School TDD**: Real node connection, state verification
- **Validates**:
  - Node connects via `erlmcp_cluster:connect/1`
  - Ping returns `pong`
  - Node appears in cluster status

#### `node_connection_async/1`
- **Purpose**: Test OTP 26 async node connection
- **Validates**:
  - Multiple nodes connect asynchronously
  - All nodes become reachable
  - Cluster nodes list contains all nodes

#### `node_disconnection/1`
- **Purpose**: Test node disconnection
- **Validates**:
  - `disconnect/1` removes node
  - `net_adm:ping/1` returns `pang`
  - Clean disconnection

#### `node_reconnection/1`
- **Purpose**: Test node reconnection after disconnection
- **Validates**:
  - Disconnect → Reconnect cycle
  - Node becomes reachable again
  - Cluster recovers

#### `cluster_status/1`
- **Purpose**: Test cluster status reporting
- **Validates**:
  - Status format: `[{node(), node_status(), node_health()}]`
  - All nodes included
  - Status types: `up | down | connecting`
  - Health types: `healthy | degraded | unhealthy`

### Group 2: Session Affinity Tests

Tests for distributed session routing.

#### `session_registration/1`
- **Purpose**: Test session registration on local node
- **Chicago School TDD**: Real session storage, gproc registry
- **Validates**:
  - Session registers via `erlmcp_session_affinity`
  - Session node returns correctly
  - Routing finds session

#### `session_routing_local/1`
- **Purpose**: Test local session routing
- **Validates**:
  - Request routes to local node
  - Affinity map lookup succeeds
  - Correct node returned

#### `session_routing_remote/1`
- **Purpose**: Test remote session routing (multi-node)
- **Chicago School TDD**: Real RPC, distributed gproc
- **Validates**:
  - Session on remote node
  - Request routes to correct node
  - Cross-node routing works

#### `session_migration/1`
- **Purpose**: Test session migration between nodes
- **Validates**:
  - `migrate_session/3` moves session
  - Session exists on target node
  - Backup nodes configured

#### `session_failover/1`
- **Purpose**: Test session failover on node failure
- **Chicago School TDD**: Real node disconnection, failover
- **Validates**:
  - Node failure triggers failover
  - Session restores on available node
  - Recovery succeeds

#### `session_invalidation/1`
- **Purpose**: Test session invalidation
- **Validates**:
  - `invalidate_session/1` removes session
  - gproc registry cleanup
  - Subsequent lookups fail

### Group 3: Distributed Tracing Tests

Tests for OTP 28 distributed tracing features.

#### `trace_id_generation/1`
- **Purpose**: Test trace ID generation
- **Validates**:
  - Trace IDs are binary
  - IDs are unique (crypto:strong_rand_bytes)
  - Different modules generate IDs

#### `trace_injection_extraction/1`
- **Purpose**: Test trace context injection/extraction
- **Validates**:
  - `inject_trace_id/2` wraps message
  - Format: `{mcp_trace, TraceCtx, Message}`
  - `extract_trace_id/1` unwraps correctly

#### `trace_correlation_single_node/1`
- **Purpose**: Test trace correlation on single node
- **Validates**:
  - Spans propagate
  - `correlate_traces/1` aggregates spans
  - Trace structure valid

#### `trace_correlation_multi_node/1`
- **Purpose**: Test distributed trace correlation
- **Chicago School TDD**: Real RPC across nodes
- **Validates**:
  - Spans propagate to remote nodes
  - `aggregate_trace_spans/1` collects spans
  - Multi-node correlation works

#### `trace_aggregation/1`
- **Purpose**: Test trace aggregation and summary
- **Validates**:
  - `get_cluster_trace/1` returns complete trace
  - Summary includes: span_count, nodes, duration, status
  - Multiple nodes in trace

#### `trace_propagation/1`
- **Purpose**: Test trace propagation via RPC
- **Validates**:
  - Trace context survives RPC calls
  - Remote node receives trace
  - Correlation finds propagated trace

### Group 4: Split-Brain Detection Tests

Tests for partition detection and resolution.

#### `partition_detection_two_nodes/1`
- **Purpose**: Test partition detection with 2 nodes
- **Chicago School TDD**: Real network partition
- **Validates**:
  - No partition initially
  - Disconnect triggers detection
  - Partition info structure

#### `partition_detection_three_nodes/1`
- **Purpose**: Test partition detection with 3 nodes
- **Validates**:
  - Partial partition detection
  - Isolated groups identified
  - Majority group determined

#### `split_brain_majority_resolution/1`
- **Purpose**: Test majority-based split-brain resolution
- **Validates**:
  - `handle_split_brain/2` with `majority` strategy
  - Majority group survives
  - Minority group isolated

#### `split_brain_oldest_node_resolution/1`
- **Purpose**: Test oldest-node split-brain resolution
- **Validates**:
  - `handle_split_brain/2` with `oldest_node` strategy
  - Single oldest node survives
  - Startup time comparison

#### `cluster_reconnection/1`
- **Purpose**: Test cluster recovery after partition
- **Validates**:
  - Node disconnect → reconnect
  - Cluster status updates
  - Health monitoring resumes

### Group 5: Cluster Monitoring Tests

Tests for health monitoring and metrics.

#### `health_check_single_node/1`
- **Purpose**: Test health check for local node
- **Validates**:
  - `check_node_health/1` returns health status
  - Health info includes: status, process_count, last_check
  - Status types: `healthy | degraded | unhealthy`

#### `health_check_multi_node/1`
- **Purpose**: Test health check for remote nodes
- **Chicago School TDD**: Real RPC health checks
- **Validates**:
  - Remote node health retrieved
  - RPC health check succeeds
  - Process count from remote node

#### `health_metrics/1`
- **Purpose**: Test cluster-wide metrics
- **Validates**:
  - `get_cluster_metrics/0` returns metrics
  - Metrics include: total_nodes, healthy_nodes, degraded_nodes, unhealthy_nodes
  - Partition detection flag
  - Last check timestamp

#### `health_history/1`
- **Purpose**: Test health history tracking
- **Validates**:
  - `get_health_history/0` returns history list
  - History entries: `[{timestamp(), metrics()}]`
  - Bounded history (max 100 entries)

#### `alert_thresholds/1`
- **Purpose**: Test alert threshold configuration
- **Validates**:
  - `set_alert_threshold/2` configures thresholds
  - Thresholds: process_count, failed_ping_count, partition_size
  - Health checks respect thresholds

### Group 6: Distributed Tool Invocation Tests

Tests for distributed tool execution.

#### `tool_invocation_local/1`
- **Purpose**: Test local tool invocation via cluster
- **Validates**:
  - `distribute_call/4` executes on local node
  - Result returned correctly
  - Timeout handling

#### `tool_invocation_remote/1`
- **Purpose**: Test remote tool invocation
- **Chicago School TDD**: Real RPC to remote node
- **Validates**:
  - `distribute_call/4` executes on remote node
  - Result returned from remote node
  - Network transparency

#### `tool_invocation_with_tracing/1`
- **Purpose**: Test tool invocation with distributed tracing
- **Validates**:
  - Trace context propagates with call
  - `erlmcp_otel_current_context` preserved
  - Trace correlation includes tool call

#### `tool_invocation_failover/1`
- **Purpose**: Test tool invocation failover on node failure
- **Chicago School TDD**: Real node failure, error handling
- **Validates**:
  - Disconnected node raises `{node_down, Node}`
  - Reconnection restores invocation
  - Error recovery

## Setup and Teardown

### Suite Initialization

```erlang
init_per_suite(Config) ->
    %% Start distributed Erlang
    net_kernel:start([erlmcp_cluster_test@localhost, shortnames]),

    %% Set cookie
    erlang:set_cookie(node(), erlmcp_cluster_cookie),

    %% Start required applications
    application:ensure_all_started(gproc),
    Config.
```

### Group Initialization

```erlang
init_per_group(node_connection, Config) ->
    %% Start 2 slave nodes
    {ok, Nodes} = start_slave_nodes(2, Config),
    [{slave_nodes, Nodes} | Config].
```

### Test Case Initialization

```erlang
init_per_testcase(_TestCase, Config) ->
    %% Start clustering modules on all nodes
    {ok, _} = erlmcp_cluster:start_link(#{nodes => Nodes}),
    {ok, _} = erlmcp_session_affinity:start_link(),
    {ok, _} = erlmcp_distributed_tracer:start_link(),
    {ok, _} = erlmcp_cluster_monitor:start_link(),

    %% Wait for node connections
    timer:sleep(500),
    Config.
```

## Chicago School TDD Compliance

### Real Collaborators

- **Real Nodes**: Uses `ct_slave` to start actual Erlang nodes (no mocks)
- **Real RPC**: Uses `rpc:call/4` for cross-node communication
- **Real gproc**: Uses distributed gproc for session registry
- **Real Networks**: Creates actual network partitions

### State-Based Verification

- **Cluster Status**: Verifies `erlmcp_cluster:cluster_status()` returns correct state
- **Session Location**: Verifies `erlmcp_session_affinity:get_session_node/1` returns actual node
- **Health Metrics**: Verifies `erlmcp_cluster_monitor:get_cluster_metrics()` returns accurate counts
- **Trace Correlation**: Verifies `erlmcp_distributed_tracer:correlate_traces/1` aggregates actual spans

### No Mocks

- **No meck**: All tests use real gen_servers
- **No stubs**: All modules start real processes
- **No fakes**: All network operations are real

## Running the Tests

### Run All Clustering Tests

```bash
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_clustering_SUITE.erl
```

### Run Specific Test Group

```bash
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_clustering_SUITE.erl \
         --group=node_connection
```

### Run Specific Test Case

```bash
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_clustering_SUITE.erl \
         --case=session_routing_remote
```

### With Verbose Output

```bash
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_clustering_SUITE.erl \
         --verbose
```

## Coverage Analysis

### Generate Coverage Report

```bash
rebar3 cover --verbose
```

### Coverage Targets

- **Core Modules**: 85%+ (erlmcp_cluster, erlmcp_session_affinity)
- **Monitoring**: 85%+ (erlmcp_cluster_monitor)
- **Tracing**: 85%+ (erlmcp_distributed_tracer)

## Dependencies

### Required Applications

- **gproc**: Distributed process registry
- **kernel**: Distributed Erlang support
- **stdlib**: Standard library

### Required Modules

- `erlmcp_cluster`: Node management and RPC distribution
- `erlmcp_session_affinity`: Session routing and failover
- `erlmcp_session_backend`: Session storage
- `erlmcp_session_replicator`: Session replication
- `erlmcp_distributed_tracer`: Distributed tracing
- `erlmcp_cluster_monitor`: Health monitoring and split-brain detection

## OTP Version Compatibility

### OTP 26
- Async node connection (`net_kernel:connect_node/1`)
- Node monitoring improvements

### OTP 27
- Distribution flag improvements
- Better `monitor_nodes` options

### OTP 28
- Improved distributed tracing
- Better trace correlation

## Known Limitations

1. **ct_slave Deprecation**: Uses deprecated `ct_slave` (will migrate to `peer` in OTP 29)
2. **Timing Dependencies**: Some tests use `timer:sleep/1` for node synchronization
3. **Network Requirements**: Tests require local hostname resolution
4. **Cookie Management**: Uses fixed test cookie (not production-ready)

## Future Improvements

1. **Migrate to peer**: Replace `ct_slave` with OTP 29 `peer` module
2. **Property-Based Tests**: Add Proper tests for cluster invariants
3. **Performance Tests**: Add benchmarks for cluster operations
4. **Chaos Engineering**: Add more chaos tests for failure scenarios
5. **Scalability Tests**: Test with larger node counts (10+ nodes)

## Troubleshooting

### Slave Node Start Failures

**Symptom**: Tests skip with `{slave_start_failed, Reason}`

**Solutions**:
1. Check hostname resolution: `hostname -f`
2. Check EPMD: `epmd -names`
3. Check firewall: Ensure ports 4369-4370 are open
4. Check disk space: Slave nodes need temp directory

### Connection Timeouts

**Symptom**: Tests timeout during node connection

**Solutions**:
1. Increase timeout in `start_slave_nodes/3`
2. Check network latency between nodes
3. Verify cookie matches: `erlang:get_cookie()`

### Partition Detection Flakiness

**Symptom**: Partition tests intermittently fail

**Solutions**:
1. Increase `timer:sleep/1` delays
2. Check node monitoring is working
3. Verify `net_kernel:monitor_nodes/2` is called

## Related Documentation

- `docs/otp-patterns.md`: OTP patterns for distributed systems
- `docs/DISTRIBUTION_IMPROVEMENTS_OTP27.md`: OTP 27 distribution features
- `apps/erlmcp_core/src/erlmcp_cluster.erl`: Cluster module documentation
- `apps/erlmcp_core/src/erlmcp_session_affinity.erl`: Session affinity documentation

## References

- [Common Test User's Guide](https://www.erlang.org/doc/apps/common_test/users_guide.html)
- [Distributed Erlang](https://www.erlang.org/doc/reference_manual/distributed.html)
- [OTP 26 Release Notes](https://www.erlang.org/doc/Otp26ErlangReleaseNotes)
- [OTP 27 Release Notes](https://www.erlang.org/doc/Otp27ErlangReleaseNotes)
- [OTP 28 Release Notes](https://www.erlang.org/doc/Otp28ErlangReleaseNotes)
