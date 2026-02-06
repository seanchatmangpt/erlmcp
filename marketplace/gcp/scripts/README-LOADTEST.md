# erlmcp Load Testing - Docker-Only Constitution

Comprehensive load testing suite for erlmcp MCP services with Docker-only execution model.

## Overview

This load testing suite provides:
- **hey**: Modern HTTP load generator (primary tool)
- **ab**: Apache Bench (fallback/compatibility)
- **JSON-RPC 2.0**: Native MCP protocol testing
- **Multiple profiles**: baseline, stress, spike, soak, capacity
- **Evidence-based**: Structured metrics and reports
- **Docker-native**: No host execution required

## Quick Start

### Option 1: Docker-based execution (RECOMMENDED)

Build the load test container:
```bash
docker build -f marketplace/gcp/scripts/Dockerfile.loadtest -t erlmcp-loadtest .
```

Run baseline load test:
```bash
docker run --rm --network host \
  -v /tmp/loadtest-results:/results \
  erlmcp-loadtest --profile baseline --url http://localhost:8080
```

### Option 2: Docker Compose

Add to your docker-compose.yml or use existing service:
```bash
docker compose run --rm \
  -e TARGET_URL=http://erlmcp:8080 \
  -e PROFILE=stress \
  erlmcp-loadtest
```

### Option 3: Direct execution (requires hey installed)

For development/testing only:
```bash
./marketplace/gcp/scripts/load-test.sh --profile baseline
```

## Load Test Profiles

### Baseline
**Purpose**: Quick performance check
- Duration: 60 seconds
- Requests: 1,000
- Concurrency: 10
- Use case: CI/CD gate, smoke testing

```bash
docker run --rm --network host erlmcp-loadtest --profile baseline
```

### Stress
**Purpose**: High concurrency stress testing
- Duration: 300 seconds (5 min)
- Requests: 50,000
- Concurrency: 100
- Use case: Capacity planning, bottleneck identification

```bash
docker run --rm --network host erlmcp-loadtest --profile stress
```

### Spike
**Purpose**: Sudden traffic spike simulation
- Duration: 30 seconds
- Requests: 10,000
- Concurrency: 200
- Use case: Auto-scaling validation, failover testing

```bash
docker run --rm --network host erlmcp-loadtest --profile spike
```

### Soak
**Purpose**: Long-running stability test
- Duration: 3,600 seconds (1 hour)
- Requests: 100,000
- Concurrency: 50
- RPS: 25 (rate limited)
- Use case: Memory leak detection, stability validation

```bash
docker run --rm --network host erlmcp-loadtest --profile soak
```

### Capacity
**Purpose**: Maximum capacity determination
- Duration: 120 seconds
- Requests: 100,000
- Concurrency: 500
- Use case: Maximum throughput discovery, SLA planning

```bash
docker run --rm --network host erlmcp-loadtest --profile capacity
```

## Custom Configuration

Override any parameter:
```bash
docker run --rm --network host erlmcp-loadtest \
  --profile custom \
  --duration 180 \
  --requests 25000 \
  --concurrency 75 \
  --rps 100
```

## Testing GCP Marketplace Deployments

Automatically resolve GCP instance IP:
```bash
docker run --rm --network host \
  -v ~/.config/gcloud:/root/.config/gcloud:ro \
  -e GCP_PROJECT_ID=my-project \
  -e GCP_INSTANCE_NAME=erlmcp-prod \
  -e GCP_ZONE=us-central1-a \
  erlmcp-loadtest --profile stress
```

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `TARGET_URL` | Target endpoint | `http://localhost:8080` |
| `PROFILE` | Load test profile | `baseline` |
| `DURATION` | Test duration (seconds) | Profile-specific |
| `REQUESTS` | Total requests | Profile-specific |
| `CONCURRENCY` | Concurrent workers | Profile-specific |
| `RPS` | Rate limit (req/sec) | `0` (unlimited) |
| `TIMEOUT` | Request timeout (seconds) | `30` |
| `MAX_ERROR_RATE` | Max acceptable error rate | `0.01` (1%) |
| `MAX_P95_LATENCY` | Max p95 latency (ms) | `1000` |
| `MAX_P99_LATENCY` | Max p99 latency (ms) | `5000` |
| `RESULTS_DIR` | Output directory | `/tmp/erlmcp-load-test-*` |

## Test Scenarios

The load test script executes multiple scenarios:

1. **Health Check Load Test**: Tests `/health` endpoint
2. **Root Endpoint**: Tests main entry point
3. **MCP Initialize**: JSON-RPC `initialize` method
4. **MCP List Tools**: JSON-RPC `tools/list` method
5. **MCP Ping**: Simple JSON-RPC `ping` method
6. **Progressive Load**: Ramping concurrency (1→200)
7. **Apache Bench**: Fallback compatibility test

## Output Artifacts

All tests generate structured evidence:

```
/results/
├── test-01-health.json           # Health endpoint results
├── test-02-root.json             # Root endpoint results
├── test-03-mcp-initialize.json   # MCP initialize test
├── test-03-mcp-initialize-payload.json
├── test-04-mcp-list-tools.json   # MCP list tools test
├── test-04-mcp-list-tools-payload.json
├── test-05-mcp-ping.json         # MCP ping test
├── test-05-mcp-ping-payload.json
├── progressive-c*.json           # Progressive load tests
├── ab-test.txt                   # Apache Bench results
├── ab-gnuplot.tsv               # AB gnuplot data
├── summary.json                  # Aggregated metrics
└── LOAD_TEST_REPORT.md          # Human-readable report
```

## Quality Thresholds

Tests automatically validate against quality thresholds:

- **Error Rate**: ≤ 1% (configurable via `MAX_ERROR_RATE`)
- **P95 Latency**: ≤ 1000ms (configurable via `MAX_P95_LATENCY`)
- **P99 Latency**: ≤ 5000ms (configurable via `MAX_P99_LATENCY`)

Exit code `0` = all thresholds passed
Exit code `1` = threshold violations detected

## CI/CD Integration

### GitHub Actions

```yaml
- name: Load Test
  run: |
    docker build -f marketplace/gcp/scripts/Dockerfile.loadtest -t erlmcp-loadtest .
    docker run --rm --network host \
      -v ${{ github.workspace }}/results:/results \
      erlmcp-loadtest --profile baseline

- name: Upload Results
  uses: actions/upload-artifact@v3
  with:
    name: load-test-results
    path: results/
```

### Cloud Build (GCP)

```yaml
steps:
  - name: 'gcr.io/cloud-builders/docker'
    args:
      - 'build'
      - '-f'
      - 'marketplace/gcp/scripts/Dockerfile.loadtest'
      - '-t'
      - 'erlmcp-loadtest'
      - '.'

  - name: 'erlmcp-loadtest'
    args:
      - '--profile'
      - 'stress'
      - '--url'
      - 'http://erlmcp-service:8080'
```

## MCP JSON-RPC 2.0 Testing

The script natively supports MCP protocol testing with proper JSON-RPC 2.0 payloads:

### Initialize Request
```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "initialize",
  "params": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "roots": { "listChanged": true },
      "sampling": {}
    },
    "clientInfo": {
      "name": "erlmcp-load-test",
      "version": "3.0.0"
    }
  }
}
```

### Tools List Request
```json
{
  "jsonrpc": "2.0",
  "id": "1234567890",
  "method": "tools/list",
  "params": {}
}
```

## Docker-Only Constitution Compliance

✅ **All execution via Docker**
- No host binaries required (except Docker)
- Containerized hey, ab, curl, jq
- Reproducible environment

✅ **Evidence-based testing**
- Structured JSON metrics
- Markdown reports
- Timestamped artifacts

✅ **Quality gates**
- Automated threshold validation
- Exit code based on success/failure
- CI/CD integration ready

✅ **Security**
- Read-only mounts where applicable
- No privileged containers
- Minimal attack surface

## Troubleshooting

### hey not found
```bash
# Script automatically falls back to Docker-based hey
docker run --rm williamyeh/hey:latest -n 100 -c 10 http://target:8080
```

### Connection refused
```bash
# Verify target is running
curl http://localhost:8080/health

# Check network mode
docker run --rm --network host erlmcp-loadtest --profile baseline
```

### Permission denied
```bash
# Ensure script is executable
chmod +x marketplace/gcp/scripts/load-test.sh
```

### GCP instance not found
```bash
# Verify gcloud auth and project
gcloud auth list
gcloud config get-value project

# Mount gcloud config
docker run --rm --network host \
  -v ~/.config/gcloud:/root/.config/gcloud:ro \
  erlmcp-loadtest --profile baseline
```

## Advanced Usage

### Custom MCP Methods

Edit script to add custom JSON-RPC methods:
```bash
generate_custom_payload() {
    cat <<EOF
{
  "jsonrpc": "2.0",
  "id": "$(date +%s%N)",
  "method": "custom/method",
  "params": {"key": "value"}
}
EOF
}
```

### Multi-endpoint Testing

Test multiple endpoints:
```bash
for endpoint in /api/v1 /api/v2 /health; do
  docker run --rm --network host \
    erlmcp-loadtest --url http://localhost:8080${endpoint}
done
```

### Distributed Load Testing

Run multiple containers for distributed load:
```bash
for i in {1..5}; do
  docker run --rm --network host -d \
    --name loadtest-$i \
    erlmcp-loadtest --profile stress &
done
```

## Constitution Compliance Checklist

- [x] Docker-only execution
- [x] No host command execution (hey via Docker)
- [x] Structured evidence artifacts
- [x] Quality threshold validation
- [x] Reproducible test scenarios
- [x] Security: no privileged containers
- [x] Security: read-only mounts
- [x] Observability: structured metrics
- [x] Exit codes for CI/CD gates
- [x] GCP Marketplace deployment support

## References

- [hey documentation](https://github.com/rakyll/hey)
- [Apache Bench documentation](https://httpd.apache.org/docs/2.4/programs/ab.html)
- [MCP Protocol Specification](https://modelcontextprotocol.io/)
- [erlmcp Constitution](../../../CLAUDE.md)

---

**Generated for erlmcp v3.0.0**
**Docker-Only Constitution Compliant**
