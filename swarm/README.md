# Docker Swarm Testing Infrastructure for erlmcp

Complete Docker Swarm-based testing environment for MCP (Model Context Protocol) clients and servers. Provides benchmarking, stress testing, and performance validation infrastructure with integrated monitoring via Prometheus and Grafana.

## Features

### Infrastructure

- **Docker Swarm Orchestration**: Production-ready container orchestration
- **Horizontal Scaling**: 5-10 MCP server replicas with automatic load balancing
- **Load Balancing**: Traefik with intelligent routing and health checks
- **Overlay Networks**: Isolated communication channels for MCP and monitoring
- **Volume Management**: Persistent storage for logs, metrics, and test results
- **Health Monitoring**: Automatic container restart and health checks

### Client Simulation

- **50-100+ Simulated Clients**: Configurable concurrent MCP clients
- **Multiple Profiles**: High-load, normal, low-load, slow-client, message-bomber
- **Realistic Patterns**: Tool calls, resource reads, subscriptions
- **Error Injection**: Network failures, timeouts, malformed messages
- **Metrics Export**: Prometheus-compatible client metrics

### Benchmarking

- **10+ Test Scenarios**:
  - Baseline performance testing
  - Connection flood testing
  - Message bombing (high throughput)
  - Slow client attacks
  - Network partition simulation
  - Service failure recovery
  - Mixed workload testing
  - Long-running stability

- **Comprehensive Metrics**:
  - Latency (p50, p95, p99, max)
  - Throughput (msg/sec, req/sec)
  - Connection counts and establishment time
  - Error rates by category
  - Resource utilization (CPU, memory, network)
  - Service health status

### Monitoring

- **Prometheus**: Time-series metrics collection and querying
- **Grafana**: Real-time dashboard visualization
- **Alert Rules**: Pre-configured alerts for performance degradation
- **Node Exporter**: Host-level system metrics

## Directory Structure

```
swarm/
├── docker/                     # Docker configuration files
│   ├── docker-compose.swarm.yml    # Main service definitions
│   ├── Dockerfile.erlmcp-swarm     # Optimized erlmcp server image
│   ├── prometheus.yml              # Prometheus scrape config
│   ├── prometheus-alerts.yml       # Alert rules
│   ├── traefik.yml                 # Load balancer config
│   └── grafana/                    # Grafana provisioning
│       ├── provisioning/
│       │   └── datasources/
│       │       └── prometheus.yml
│       └── dashboards/
│           └── mcp-performance.json
├── clients/                    # MCP client simulator
│   ├── Dockerfile.go          # Go client container
│   ├── go.mod                 # Go module definition
│   └── cmd/main.go            # Client simulator source
├── scenarios/                 # Test scenario scripts
│   ├── baseline_test.sh       # Baseline performance
│   ├── stress_connection_flood.sh
│   ├── stress_message_bombing.sh
│   └── [more scenarios]
├── scripts/                   # Orchestration scripts
│   ├── init_swarm.sh          # Initialize Docker Swarm
│   ├── deploy.sh              # Deploy services
│   ├── run_benchmarks.sh      # Run full benchmark suite
│   ├── monitor.sh             # Monitor service health
│   └── cleanup.sh             # Clean up resources
├── docs/                      # Documentation
│   ├── SETUP_GUIDE.md         # Installation and setup
│   ├── RUNNING_BENCHMARKS.md  # How to run tests
│   ├── INTERPRETING_RESULTS.md
│   ├── PERFORMANCE_TUNING.md
│   └── TROUBLESHOOTING.md
├── test-results/              # Generated test results
│   ├── baseline_test/
│   ├── connection_flood/
│   ├── message_bombing/
│   ├── benchmark_report.json
│   └── benchmark_report.html
└── README.md                  # This file
```

## Quick Start

### 1. Initialize Swarm Cluster

```bash
cd /Users/sac/erlmcp/swarm
chmod +x scripts/*.sh scenarios/*.sh

# Initialize Docker Swarm on this machine
./scripts/init_swarm.sh
```

### 2. Deploy Services

```bash
# Deploy erlmcp servers, clients, monitoring stack
./scripts/deploy.sh

# Wait for services to stabilize (2-3 minutes)
sleep 120

# Verify services are running
docker service ls
```

### 3. Run Benchmark Suite

```bash
# Run complete test suite (all scenarios sequentially)
./scripts/run_benchmarks.sh

# Or run individual tests
cd scenarios
bash baseline_test.sh
bash stress_connection_flood.sh
bash stress_message_bombing.sh
```

### 4. View Results

```bash
# Web UI dashboards
open http://localhost:3000          # Grafana
open http://localhost:9091          # Prometheus
open http://localhost:8081          # Traefik

# Generated reports
open test-results/benchmark_report.html

# JSON data
cat test-results/benchmark_report.json | jq .
```

## Service Endpoints

| Service | Endpoint | Purpose |
|---------|----------|---------|
| Load Balancer (Traefik) | http://localhost:80 | MCP HTTP gateway |
| MCP WebSocket | ws://localhost:5555/mcp | WebSocket protocol |
| Prometheus | http://localhost:9091 | Metrics database |
| Grafana | http://localhost:3000 | Dashboards (user: admin, password: admin) |
| Traefik Dashboard | http://localhost:8081 | Load balancer UI |
| Test Controller | http://localhost:8888 | Test orchestration API |

## Key Metrics

### Performance Metrics

```prometheus
# Request rate
rate(mcp_client_requests_total[5m])

# Latency percentiles
histogram_quantile(0.95, rate(mcp_client_request_duration_ms_bucket[1m]))

# Active connections
mcp_client_connections_active

# Error rate
rate(mcp_client_messages_errors_total[5m])

# Throughput
rate(mcp_client_messages_sent_total[5m])
```

### Resource Metrics

```prometheus
# CPU usage
rate(container_cpu_usage_seconds_total[5m])

# Memory usage
container_memory_usage_bytes

# Network I/O
rate(container_network_transmit_bytes_total[5m])
```

## Client Profiles

### High-Load
- **Clients**: 50
- **Message Rate**: 1,000 msg/sec per client
- **Total Load**: ~50,000 msg/sec
- **Use Case**: Maximum sustained throughput

### Normal (Default)
- **Clients**: 25
- **Message Rate**: 100 msg/sec per client
- **Total Load**: ~2,500 msg/sec
- **Use Case**: Typical production workload

### Low-Load
- **Clients**: 10
- **Message Rate**: 10 msg/sec per client
- **Total Load**: ~100 msg/sec
- **Use Case**: Minimal load baseline

### Slow-Client
- **Clients**: 5
- **Message Rate**: 1 msg/sec per client
- **Total Load**: ~5 msg/sec
- **Use Case**: Slow network simulation

### Message-Bomber
- **Clients**: 10-20
- **Message Rate**: 10,000 msg/sec per client
- **Total Load**: ~100,000-200,000 msg/sec
- **Use Case**: Stress testing, capacity limits

## Test Scenarios

### Baseline Performance (baseline_test.sh)
- **Duration**: 5 minutes
- **Phases**: Warmup (30s) → Steady state (4m) → Analysis
- **Expected p95 Latency**: 50-200ms
- **Expected Throughput**: 2,000-3,000 msg/sec

### Connection Flood (stress_connection_flood.sh)
- **Duration**: 10 minutes
- **Phases**: Ramp-up (5m, 0→500 connections) → Peak (3m) → Cool-down (2m)
- **Peak Connections**: 500+
- **Expected Error Rate**: <1%

### Message Bombing (stress_message_bombing.sh)
- **Duration**: 10 minutes
- **Message Rate**: 10,000 msg/sec per client × 20 clients
- **Total Load**: ~200,000 msg/sec
- **Expected Peak Throughput**: 150,000+ msg/sec

### Slow Client Attack (stress_slow_client.sh)
- **Duration**: 15 minutes
- **Client Count**: 100+ slow clients
- **Expected**: Latency increase, connection saturation

### Network Partition (stress_network_partition.sh)
- **Duration**: 10 minutes
- **Failure Type**: Simulate network split, packet loss
- **Recovery**: Automatic reconnection

### Service Failure (recovery_test.sh)
- **Duration**: 10 minutes
- **Failure**: Kill one server instance
- **Expected**: Traffic redistribution, recovery

## Monitoring During Tests

### Real-Time Dashboard

```bash
# View current metrics
curl http://localhost:9091/api/v1/query?query=rate\(mcp_client_requests_total\[5m\]\)

# Watch server status
watch -n 2 'docker service ps erlmcp-swarm_erlmcp-server'

# Monitor service logs
docker service logs erlmcp-swarm_erlmcp-server -f
```

### Grafana Dashboards

1. Open http://localhost:3000
2. Login: admin / admin
3. Navigate to **Dashboards** → **MCP Swarm Performance**
4. View real-time metrics and historical trends

## Results Analysis

Test results are saved in `test-results/` directory:

```
test-results/
├── benchmark_report.json          # Structured test results
├── benchmark_report.html          # HTML report (charts, summaries)
├── baseline_test/
│   ├── metrics.json              # Raw Prometheus metrics
│   ├── connections.jsonl         # Connection timeline
│   └── analysis.txt              # Calculated statistics
├── connection_flood/
│   ├── connections.jsonl
│   ├── error_rate.jsonl
│   └── recovery_time.txt
└── message_bombing/
    ├── metrics.jsonl
    └── statistics.json
```

### Analyzing Results

```bash
# View summary
jq '.summary' test-results/benchmark_report.json

# Extract latency data
jq '.scenarios.baseline_test' test-results/benchmark_report.json

# Generate custom analysis
python3 << 'EOF'
import json
from pathlib import Path

results = Path("test-results")
for test_dir in results.iterdir():
    if test_dir.is_dir():
        print(f"\n{test_dir.name}:")
        for metric_file in test_dir.glob("*.jsonl"):
            print(f"  - {metric_file.name}")
EOF
```

## Troubleshooting

### Services Not Starting

```bash
# Check service status
docker service ls
docker service ps erlmcp-swarm_erlmcp-server

# View logs
docker service logs erlmcp-swarm_erlmcp-server
```

### Metrics Not Collected

```bash
# Check Prometheus
curl http://localhost:9091/-/healthy

# Check client metrics
curl http://localhost:8888/metrics
```

### Performance Issues

```bash
# Monitor resource usage
docker stats erlmcp-swarm_erlmcp-server

# Check network connectivity
docker exec <container-id> ping erlmcp-server

# Scale up servers
docker service scale erlmcp-swarm_erlmcp-server=12
```

See [TROUBLESHOOTING.md](docs/TROUBLESHOOTING.md) for detailed solutions.

## Scaling

### Increase Server Capacity

```bash
# Scale to 12 servers
docker service scale erlmcp-swarm_erlmcp-server=12

# Scale to 16 servers
docker service scale erlmcp-swarm_erlmcp-server=16
```

### Increase Client Load

```bash
# Modify and re-run with more clients
export CLIENT_COUNT=200
export MESSAGE_RATE=500
./scripts/run_benchmarks.sh
```

### Increase Test Duration

```bash
export DURATION=1800  # 30 minutes
bash scenarios/baseline_test.sh
```

## Cleanup

```bash
# Remove all services (keep volumes)
docker stack rm erlmcp-swarm
sleep 30

# Remove volumes
docker volume rm erlmcp-data erlmcp-logs prometheus-data grafana-data

# Leave swarm
docker swarm leave --force
```

## Performance Tuning

See [PERFORMANCE_TUNING.md](docs/PERFORMANCE_TUNING.md) for optimization strategies:

- Resource limits and reservations
- Network optimization
- Message batching
- Connection pooling
- Load balancer configuration

## Documentation

- [SETUP_GUIDE.md](docs/SETUP_GUIDE.md) - Installation and prerequisites
- [RUNNING_BENCHMARKS.md](docs/RUNNING_BENCHMARKS.md) - How to run tests
- [INTERPRETING_RESULTS.md](docs/INTERPRETING_RESULTS.md) - Understanding metrics
- [PERFORMANCE_TUNING.md](docs/PERFORMANCE_TUNING.md) - Optimization guide
- [TROUBLESHOOTING.md](docs/TROUBLESHOOTING.md) - Common issues

## Architecture Notes

### Load Balancing

Traefik acts as the frontend load balancer:
- **Layer 4 (TCP/WebSocket)**: Port 5555 routes to erlmcp-server instances
- **Layer 7 (HTTP)**: Port 80 with path-based routing
- **Health Checks**: Every 10 seconds with 3-second timeout
- **Metrics**: Exported to Prometheus on port 9090

### Networking

Two overlay networks isolate traffic:
- **mcp-network**: MCP client-server communication (VXLAN ID 4096)
- **monitoring**: Prometheus and Grafana (VXLAN ID 4097)

Service-to-service DNS: `<service-name>` (e.g., `prometheus:9090`)

### Metrics Collection

Prometheus scrapes metrics every 10-15 seconds from:
- **erlmcp-server** (9100): Application metrics
- **traefik** (8080): Load balancer metrics
- **node-exporter** (9100): Host-level metrics

Data retention: 7 days by default

## Resource Requirements

| Component | CPU | Memory | Notes |
|-----------|-----|--------|-------|
| Manager Node | 2+ | 4GB | Runs Prometheus, Grafana, Traefik |
| Worker Nodes | 2+ each | 4GB each | Runs erlmcp servers and clients |
| Total (3 nodes) | 8+ cores | 16GB+ | Recommended minimum |

## Support

For issues or questions:
1. Check [TROUBLESHOOTING.md](docs/TROUBLESHOOTING.md)
2. Review service logs: `docker service logs <service-name>`
3. Check Prometheus metrics: http://localhost:9091

## Future Enhancements

- [ ] Kubernetes support (from Docker Swarm)
- [ ] Advanced scenario scripting
- [ ] Machine learning-based performance prediction
- [ ] Chaos engineering integration
- [ ] Multi-region federation
- [ ] Custom client behavior plugins
- [ ] Integration with CI/CD pipelines
