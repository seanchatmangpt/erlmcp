# Running Benchmarks

## Quick Start

```bash
# 1. Initialize Swarm cluster
cd /Users/sac/erlmcp/swarm
./scripts/init_swarm.sh

# 2. Deploy services
./scripts/deploy.sh

# 3. Wait for services to stabilize (2-3 minutes)
sleep 120

# 4. Run benchmark suite
./scripts/run_benchmarks.sh

# 5. View results
open test-results/benchmark_report.html
```

## Test Scenarios

### 1. Baseline Performance Test (baseline_test.sh)

**Purpose**: Establish performance baseline under normal load

**Duration**: 5 minutes (300 seconds)

**Configuration**:
- Client Type: normal
- Clients: 25
- Message Rate: 100 msg/sec per client
- Total Load: ~2,500 msg/sec

**Metrics Collected**:
- p50, p95, p99 latency
- Throughput (msg/sec)
- Error rate
- Connection stability

**Expected Results**:
- p50 Latency: 10-50ms
- p95 Latency: 50-200ms
- p99 Latency: 100-500ms
- Error Rate: <0.01%
- Throughput: 2,000-3,000 msg/sec

### 2. Connection Flood Test (stress_connection_flood.sh)

**Purpose**: Test connection handling under rapid connection establishment

**Duration**: 10 minutes (600 seconds)

**Phases**:
1. Ramp-up (5 min): 0 → 500 connections (5 new connections/sec)
2. Peak load (3 min): 500 sustained connections
3. Cool-down (2 min): 500 → 25 connections

**Metrics Collected**:
- Peak concurrent connections
- Connection establishment latency
- Error rate during ramp-up
- Recovery time

**Expected Results**:
- Handle 500+ concurrent connections
- Connection errors: <1%
- Recovery time: <30 seconds

### 3. Message Bombing Test (stress_message_bombing.sh)

**Purpose**: Test message throughput capacity

**Duration**: 10 minutes (600 seconds)

**Configuration**:
- Client Type: message-bomber
- Clients: 20
- Message Rate: 10,000 msg/sec per client
- Total Load: ~200,000 msg/sec

**Metrics Collected**:
- Peak throughput
- Latency under extreme load
- Error rate
- CPU/Memory impact

**Expected Results**:
- Peak Throughput: 150,000+ msg/sec
- p50 Latency: 5-50ms
- p95 Latency: 50-500ms
- Error Rate: <5%
- Server CPU: <80%

## Running Individual Scenarios

```bash
# Run baseline test only
cd /Users/sac/erlmcp/swarm/scenarios
bash baseline_test.sh

# Run connection flood test
bash stress_connection_flood.sh

# Run message bombing test
bash stress_message_bombing.sh
```

## Environment Variables

### Global Settings

```bash
# Load balancer address
export LOAD_BALANCER="localhost:5555"

# Prometheus endpoint
export PROMETHEUS_URL="http://localhost:9091"

# Test results directory
export RESULTS_DIR="./test-results"

# Test duration (seconds)
export DURATION=300
```

### Client Configuration

```bash
# Client profile: high-load, normal, low-load, slow-client, message-bomber
export CLIENT_TYPE=normal

# Target servers address
export TARGET_SERVERS=localhost:5555

# Number of simulated clients
export CLIENT_COUNT=25

# Messages per second per client
export MESSAGE_RATE=100

# Test duration in seconds
export DURATION_SECONDS=3600

# Enable metrics export
export METRICS_ENABLED=true

# Metrics port
export METRICS_PORT=8888
```

## Monitoring During Tests

### Prometheus Dashboard

1. Open http://localhost:9091
2. Query key metrics:
   - `rate(mcp_client_requests_total[5m])` - Request rate
   - `mcp_client_connections_active` - Active connections
   - `histogram_quantile(0.95, rate(mcp_client_request_duration_ms_bucket[5m]))` - p95 latency
   - `rate(mcp_client_messages_errors_total[5m])` - Error rate

### Grafana Dashboards

1. Open http://localhost:3000 (user: admin, password: admin)
2. Navigate to Dashboards → MCP Performance
3. View real-time metrics during tests

### Docker Service Logs

```bash
# View erlmcp server logs
docker service logs erlmcp-swarm_erlmcp-server -f

# View client simulator logs
docker service logs erlmcp-swarm_mcp-client-high-load -f

# View load balancer logs
docker service logs erlmcp-swarm_traefik -f

# View Prometheus logs
docker service logs erlmcp-swarm_prometheus -f
```

## Test Results Analysis

### Results Directory Structure

```
test-results/
├── benchmark_report.json          # JSON report
├── benchmark_report.html          # HTML report
├── baseline_test/
│   ├── metrics.json              # Raw metrics
│   └── connections.jsonl         # Connection data
├── connection_flood/
│   ├── connections.jsonl         # Connection timeline
│   ├── error_rate.jsonl          # Error rate timeline
│   ├── peak_connections.jsonl    # Peak data
│   └── peak_latency_p95.jsonl   # Latency at peak
└── message_bombing/
    ├── metrics.jsonl             # Aggregated metrics
    └── [hourly] latency_p95.jsonl # Detailed timeline
```

### Analyzing Results

```bash
# View JSON report
cat test-results/benchmark_report.json | jq .

# Extract latency percentiles
cat test-results/baseline_test/metrics.json | \
    jq '.data.result[] | .metric.quantile, .value'

# Find peak metrics
tail -n 1 test-results/connection_flood/peak_connections.jsonl | jq .

# Calculate statistics
python3 << 'EOF'
import json
from pathlib import Path

results = Path("test-results/message_bombing/metrics.jsonl")
max_throughput = 0

for line in results.read_text().strip().split('\n'):
    data = json.loads(line)
    if isinstance(data.get('req_rate'), (int, float)):
        max_throughput = max(max_throughput, data['req_rate'])

print(f"Max Throughput: {max_throughput:.2f} req/sec")
EOF
```

## Troubleshooting Tests

### Services Not Starting

```bash
# Check service status
docker service ls
docker service ps erlmcp-swarm_erlmcp-server

# View service logs
docker service logs erlmcp-swarm_erlmcp-server

# Check resource availability
docker node ls
docker node inspect <node-id>
```

### Metrics Not Collecting

```bash
# Check Prometheus is running
curl http://localhost:9091/-/healthy

# Query Prometheus directly
curl 'http://localhost:9091/api/v1/query?query=up'

# Check client metrics endpoint
curl http://localhost:8888/metrics
```

### High Error Rates

```bash
# Check server health
curl http://load-balancer:8080/health

# View server logs
docker service logs erlmcp-swarm_erlmcp-server | tail -100

# Check network connectivity
docker exec <container-id> ping erlmcp-server
```

### Performance Lower Than Expected

1. **Check Resource Limits**:
   ```bash
   docker stats erlmcp-swarm_erlmcp-server
   ```

2. **Check Network**:
   ```bash
   docker exec <container-id> iperf3 -c <target>
   ```

3. **Check Load Distribution**:
   ```bash
   docker service ps erlmcp-swarm_erlmcp-server | wc -l
   ```

## Scaling Tests

### Increase Server Capacity

```bash
# Scale to 12 servers
docker service scale erlmcp-swarm_erlmcp-server=12

# Monitor impact
watch -n 2 'docker service ps erlmcp-swarm_erlmcp-server'
```

### Increase Client Load

```bash
# Modify client count
export CLIENT_COUNT=100
export MESSAGE_RATE=500

# Re-run benchmark
./scripts/run_benchmarks.sh
```

### Increase Duration

```bash
# Run longer baseline test
export DURATION=1800  # 30 minutes
bash scenarios/baseline_test.sh
```

## Cleanup

```bash
# Remove all services (but keep volumes)
docker stack rm erlmcp-swarm

# Wait for cleanup
sleep 30

# Remove volumes if needed
docker volume rm erlmcp-data erlmcp-logs prometheus-data grafana-data test-results

# Leave swarm
docker swarm leave --force
