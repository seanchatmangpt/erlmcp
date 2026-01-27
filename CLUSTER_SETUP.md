# ERLMCP Cluster: 100K Concurrent Connections

This guide explains how to set up and run a 4-node erlmcp cluster capable of handling 100,000 concurrent connections (25K per node).

## Quick Start

### Local Cluster (Bare Metal)

```bash
# 1. Build erlmcp
rebar3 compile

# 2. Start cluster (4 nodes, automatic networking)
./scripts/start-cluster.sh start

# 3. Verify cluster is running
./scripts/start-cluster.sh status

# 4. Run benchmark
./scripts/run-cluster-benchmark.sh

# 5. Stop cluster
./scripts/start-cluster.sh stop
```

### Docker Cluster

```bash
# 1. Start Docker cluster (includes HAProxy load balancer)
./scripts/docker-cluster.sh start

# 2. Verify cluster health
./scripts/docker-cluster.sh status

# 3. View logs
./scripts/docker-cluster.sh logs

# 4. Access load balancer: http://localhost:8000
# 5. View HAProxy stats: http://localhost:9001/stats

# 6. Stop cluster
./scripts/docker-cluster.sh stop
```

## Architecture

### Cluster Layout

```
┌─────────────────────────────────────────────────┐
│              erlmcp 4-Node Cluster              │
│                  (100K concurrent)              │
├─────────────────────────────────────────────────┤
│                  HAProxy LB                     │
│              (Load Balancer, Cookie)            │
│         Sticky Sessions (roundrobin)            │
├──────────┬──────────┬──────────┬────────────────┤
│ erlmcp1  │ erlmcp2  │ erlmcp3  │    erlmcp4     │
│ Primary  │ Replica  │ Replica  │    Replica     │
│ 25K conn │ 25K conn │ 25K conn │   25K conn     │
│ Node 1   │ Node 2   │ Node 3   │   Node 4       │
└──────────┴──────────┴──────────┴────────────────┘
```

### Cluster Configuration

**File**: `config/cluster.config`

Key settings for 100K scale:

- **Connection Limits**: 25,000 per node, 100,000 global
- **TCP Settings**: 2MB send/recv buffers, backlog 2048
- **Cluster Sync**: Lazy (1sec interval) for performance
- **Message Routing**: Sticky routing keeps client on same node

### VM Arguments

**File**: `config/vm-cluster.args`

Optimized for:

- Process count: 262,144 per node
- Scheduler threads: Auto-detect (1 per CPU core)
- Garbage collection: Fullsweep every 1000 collections
- Memory overhead tuning: +MBsbct 32
- Async I/O: 64 threads

## Node Startup Details

### Local Cluster Startup

Each node starts with:

```bash
erl \
  -setcookie erlmcp_cluster \
  -sname erlmcp1 \
  -config config/cluster \
  -args @config/vm-cluster.args \
  -pa _build/default/lib/*/ebin \
  -erlmcp cluster enabled true \
  -erlmcp cluster known_nodes '[erlmcp1@localhost,erlmcp2@localhost,erlmcp3@localhost,erlmcp4@localhost]' \
  -erlmcp cluster node_role primary
```

**Ports**:
- erlmcp1: 9101 (EPMD), 9201 (node)
- erlmcp2: 9102 (EPMD), 9202 (node)
- erlmcp3: 9103 (EPMD), 9203 (node)
- erlmcp4: 9104 (EPMD), 9204 (node)

### Docker Cluster Startup

Using docker-compose with fixed IP addresses:

```
erlmcp1: 172.25.0.2
erlmcp2: 172.25.0.3
erlmcp3: 172.25.0.4
erlmcp4: 172.25.0.5
haproxy: 172.25.0.10
```

## Monitoring the Cluster

### Cluster Status

```bash
# Check cluster status
./scripts/start-cluster.sh status

# View node logs
tail -f logs/cluster/erlmcp1.log
tail -f logs/cluster/erlmcp2.log
tail -f logs/cluster/erlmcp3.log
tail -f logs/cluster/erlmcp4.log
```

### Real-time Metrics

Connect to cluster and query metrics:

```erlang
erl -setcookie erlmcp_cluster -sname monitor

% In Erlang shell:
rpc:call('erlmcp1@localhost', erlmcp_cluster_monitor, get_cluster_status, []).

% Response:
% {ok, #{
%    nodes => [erlmcp2@localhost, erlmcp3@localhost, erlmcp4@localhost],
%    total_connections => 100000,
%    total_messages => 50000000,
%    uptime_seconds => 300,
%    node_details => #{
%      erlmcp1@localhost => #{connections => 25000, messages => 12500000},
%      erlmcp2@localhost => #{connections => 25000, messages => 12500000},
%      erlmcp3@localhost => #{connections => 25000, messages => 12500000},
%      erlmcp4@localhost => #{connections => 25000, messages => 12500000}
%    }
% }}
```

### Docker Compose Status

```bash
docker-compose -f docker-compose.cluster.yml ps
docker-compose -f docker-compose.cluster.yml logs -f erlmcp1
docker-compose -f docker-compose.cluster.yml logs -f haproxy
```

## Load Testing

### Connection Scaling Test

Progressive test from 100 to 100K connections:

```bash
erl -setcookie erlmcp_cluster -sname scale_test -noshell \
  -pa _build/default/lib/*/ebin \
  -eval "load_generator:generate_load(100000, 60)."
```

Expected output:
```
PHASE 1: Ramping up connections...
  Spawning 25000 connections to erlmcp1...
  Spawning 25000 connections to erlmcp2...
  Spawning 25000 connections to erlmcp3...
  Spawning 25000 connections to erlmcp4...
Connections established: 100000
Time to establish: 12500ms
Rate: 8000 conn/sec

PHASE 2: Sending load for 60 seconds...
  [5s] Connections: 100000, Throughput: 125000 msg/sec, P99 Latency: 45ms
  [10s] Connections: 100000, Throughput: 125000 msg/sec, P99 Latency: 42ms
  [15s] Connections: 100000, Throughput: 125000 msg/sec, P99 Latency: 48ms
  ...

PHASE 4: Results
Test Duration: 60 seconds
Total Connections: 100000
Connections per Node: 25000
Average Throughput: 125000 messages/sec
Peak Throughput: 130000 messages/sec
```

### Full Benchmark Suite

```bash
./scripts/run-cluster-benchmark.sh
```

This runs:
1. **Connection Scaling**: 100 → 1K → 10K → 25K → 50K → 75K → 100K
2. **Sustained Load**: 100K connections for 5 minutes
3. **Latency Distribution**: P50/P95/P99 measurements
4. **Message Throughput**: Max messages/second at 100K
5. **Optional Node Failure**: Shutdown erlmcp4 and measure recovery

Results saved to:
- `logs/benchmark/results-TIMESTAMP.log` (detailed log)
- `logs/benchmark/detailed-TIMESTAMP.csv` (metrics for graphing)

## Common Test Suite

Run automated cluster tests:

```bash
# Build tests
rebar3 as test compile

# Run cluster stress tests
rebar3 as test ct --suite=erlmcp_cluster_stress_SUITE

# Run specific test
rebar3 as test ct --suite=erlmcp_cluster_stress_SUITE --group=connection_scaling

# View test results
cat _build/test/logs/ct_run.*/erlmcp_cluster_stress_SUITE.logs
```

Test groups:
- `cluster_formation`: Verify 4 nodes connect
- `connection_scaling`: Test 100 → 100K connections
- `performance`: Throughput and latency at 100K
- `reliability`: Node failure and recovery

## Performance Targets

### Success Criteria ✓

| Metric | Target | Notes |
|--------|--------|-------|
| **Connections** | 100,000 concurrent | 25K per node sustained |
| **Throughput** | 125,000 msg/sec | At 100K connections |
| **P50 Latency** | <10ms | Half of requests faster |
| **P95 Latency** | <50ms | 95% of requests faster |
| **P99 Latency** | <100ms | Tail latency target |
| **Failure Detection** | <5 sec | Node down detected |
| **Recovery Time** | <10 sec | Cluster converges |
| **Memory per node** | ~2GB | At 25K connections |
| **CPU per node** | <50% | Headroom for GC |

## Capacity Planning

### Single Node (erlmcp1)
- **Max connections**: 25,000 (configurable in `cluster.config`)
- **Memory**: ~500MB baseline + ~80KB per connection = ~2GB at 25K
- **CPU**: ~30% per core at full load
- **Throughput**: ~30,000 messages/sec per node

### Full Cluster (4 nodes)
- **Total connections**: 100,000
- **Total memory**: ~8GB (2GB × 4 nodes)
- **Total throughput**: ~125,000 messages/sec
- **Network**: Inter-node sync 1sec, minimal overhead

## Tuning Recommendations

### For Higher Throughput (>100K msg/sec)

1. **Message Size**: Smaller messages = higher throughput
   ```erlang
   % In config/cluster.config:
   {transport_defaults, #{
       tcp => #{send_buf_size => 4194304}  % 4MB instead of 2MB
   }}
   ```

2. **Connection Batch Size**: Increase ramp rate
   ```bash
   ERLMCP_RAMP_RATE=2000 ./scripts/start-cluster.sh
   ```

3. **GC Tuning**: Adjust fullsweep interval
   ```bash
   # In config/vm-cluster.args:
   -env ERL_FULLSWEEP_AFTER 500  # More frequent GC for lower latency
   ```

### For Lower Latency (<50ms P99)

1. **Disable Lazy Sync**: Use eager sync
   ```erlang
   {cluster, [{sync_strategy, eager}]}
   ```

2. **Increase Buffer Sizes**: Reduce backpressure
   ```erlang
   {connection_limits, #{frame_buffer_size => 262144}}  % 256KB
   ```

3. **Pin Schedulers**: CPU affinity
   ```bash
   +B t  % Tight binding
   ```

### For Better Stability

1. **Enable Monitoring**: Increase health check frequency
   ```erlang
   {cluster, [{heartbeat_interval, 1000}]}  % 1 sec
   ```

2. **Connection Pooling**: Enable connection reuse
   ```erlang
   {transport_defaults, #{tcp => #{pool_size => 100}}}
   ```

3. **Rate Limiting**: Prevent overload
   ```erlang
   {rate_limiting, #{enabled => true, max_messages_per_sec => 100}}
   ```

## Troubleshooting

### Nodes not connecting

```bash
# Check EPMD (Erlang Port Mapper Daemon)
epmd -names

# Expected:
# epmd: up and running on port 4369 with names:
#    erlmcp1          9201
#    erlmcp2          9202
#    erlmcp3          9203
#    erlmcp4          9204

# If missing, restart EPMD:
pkill epmd
epmd -daemon
```

### High CPU usage

- Reduce message rate: lower `ERLMCP_RAMP_RATE`
- Enable sampling: set `trace_sample_rate` to 0.1
- Check GC: `+MTop off` in vm.args

### Memory growth

- Reduce connection count
- Enable message batching: `{sync_strategy, lazy}`
- Check for memory leaks with: `rpc:call(Node, erlang, memory, [allocated])`

### Connection limit reached

- Add more nodes (cluster is designed to scale)
- Increase per-node limit in `cluster.config`:
  ```erlang
  {connection_limits, #{tcp_max_connections => 50000}}
  ```
- Note: Each connection ~80KB, so 50K × 80KB = 4GB

## Production Deployment

### Recommended Setup

1. **Separate nodes**: Run each node on its own physical/virtual machine
2. **Load balancer**: HAProxy or NGINX as reverse proxy
3. **Monitoring**: Prometheus + Grafana for metrics
4. **Logging**: ELK Stack for centralized logs
5. **Auto-scaling**: Kubernetes for dynamic node addition/removal

### Security

- Use `verify_peer` for TLS (enabled by default)
- Restrict allowed origins in `http_security`
- Enable rate limiting to prevent DoS
- Monitor for DDoS patterns

### Backup & Recovery

- Enable state replication: `{enable_state_replication => true}`
- Configure persistent storage for critical state
- Regular node snapshots for disaster recovery
- Document node recovery procedures

## References

- **Architecture**: See `docs/architecture.md`
- **OTP Patterns**: See `docs/otp-patterns.md`
- **Cluster Config**: `config/cluster.config`
- **VM Arguments**: `config/vm-cluster.args`
- **Monitor Module**: `src/erlmcp_cluster_monitor.erl`
- **Stress Tests**: `test/erlmcp_cluster_stress_SUITE.erl`
