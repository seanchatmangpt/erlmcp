# erlmcp Load Testing Suite

Comprehensive k6 load testing suite for erlmcp (Erlang/OTP MCP SDK). All tests execute inside Docker containers per the DOCKER-ONLY CONSTITUTION.

## Test Suite Overview

| Test | Purpose | Duration | Peak RPS | SLA Thresholds |
|------|---------|----------|----------|----------------|
| **ramp-up** | Gradual load increase | 8 min | 1000 RPS | P95 < 200ms, < 1% errors |
| **sustained** | Constant load stability | 6 min | 500 RPS | P95 < 150ms, < 0.5% errors |
| **spike** | Sudden traffic burst | 5 min | 5000 RPS | P95 < 1000ms, < 5% errors |
| **failover** | Node failure resilience | 6 min | 200 RPS | P95 < 500ms, < 10% errors |

## Quick Start

```bash
# Run all tests
docker compose -f tests/load/docker-compose.load.yml run --rm erlmcp-load bash -c run-tests.sh all

# Run specific test
docker compose run --rm erlmcp-load bash -c run-tests.sh ramp-up

# Generate HTML reports
GENERATE_HTML=yes ./tests/load/run-tests.sh all
```

## Test Files

```
tests/load/
├── lib/
│   └── common.js              # Shared utilities
├── ramp-up-test.js            # Ramp-up to 1000 RPS
├── sustained-load-test.js     # 5min sustained at 500 RPS
├── spike-test.js              # Spike to 5000 RPS
├── failover-test.js           # HA and failover testing
├── run-tests.sh               # Execution wrapper
└── reports/                   # Test output directory
    ├── json/                  # JSON results
    └── html/                  # HTML reports
```

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `BASE_URL` | `http://erlmcp:8080` | Target service URL |
| `K6_IMAGE` | `grafana/k6:latest` | k6 Docker image |
| `REPORT_DIR` | `./reports` | Report output directory |
| `PROMETHEUS_PUSH` | - | Prometheus pushgateway URL |
| `GENERATE_HTML` | `no` | Generate HTML reports |

## Usage Examples

```bash
# Health check before testing
curl http://localhost:8080/health

# Run ramp-up test against local instance
BASE_URL=http://localhost:8080 ./tests/load/run-tests.sh ramp-up

# Run sustained test with HTML report
BASE_URL=http://localhost:8080 GENERATE_HTML=yes ./tests/load/run-tests.sh sustained

# Run spike test
BASE_URL=http://localhost:8080 ./tests/load/run-tests.sh spike

# Run failover test
BASE_URL=http://localhost:8080 ./tests/load/run-tests.sh failover

# Run all tests
./tests/load/run-tests.sh all

# Quick smoke test (1 minute)
./tests/load/run-tests.sh quick
```

## Test Scenarios

### 1. Ramp-up Test
Gradually increases load from 0 to 1000 RPS over 5 minutes, holds for 2 minutes, then cools down. Validates:
- Linear scalability
- Resource allocation under increasing load
- No bottlenecks during ramp-up

### 2. Sustained Load Test
Maintains constant 500 RPS for 5 minutes with mixed workload (60% reads, 30% writes, 10% health). Validates:
- Memory stability (no leaks)
- Connection pool efficiency
- Steady-state performance

### 3. Spike Test
Rapidly increases from 100 to 5000 RPS, holds, then recovers. Two spike cycles. Validates:
- Auto-scaling triggers
- Circuit breaker functionality
- Graceful degradation

### 4. Failover Test
Runs 200 RPS load while simulating node failures. Validates:
- Zero data loss
- Automatic failover
- Recovery to baseline < 30s

## SLA Thresholds

### Baseline (normal operation)
- P95 latency: < 100ms
- P99 latency: < 200ms
- Error rate: < 0.5%

### High Load (sustained/spike)
- P95 latency: < 500ms
- P99 latency: < 1000ms
- Error rate: < 5%

### Failover
- P95 latency: < 1000ms
- Recovery time: < 30s
- Error rate: < 10%

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | All tests passed, SLAs met |
| 1 | One or more SLA failures |
| 2 | Test execution error |
| 3 | System not ready (health check failed) |

## Reports

### JSON Reports
Located in `tests/load/reports/json/`. Include:
- Request timings
- Error rates
- Custom metrics
- Threshold violations

### HTML Reports
Located in `tests/load/reports/html/`. Provide:
- Visual graphs
- Timeline view
- Metric breakdown
- Comparison views

## CI/CD Integration

```yaml
# GitHub Actions example
- name: Run load tests
  run: |
    docker compose up -d erlmcp
    ./tests/load/run-tests.sh all
    docker compose down

# GitLab CI example
load_test:
  script:
    - docker compose -f docker-compose.yml build
    - docker compose -f docker-compose.yml up -d
    - ./tests/load/run-tests.sh all
    - docker compose -f docker-compose.yml down
  artifacts:
    paths:
      - tests/load/reports/
```

## Requirements

- Docker and Docker Compose
- Target service running and healthy
- 4GB RAM minimum for test execution
- Network connectivity to target

## Docker-only Constitution

All test execution MUST run via Docker. Direct k6 execution is forbidden.

```bash
# FORBIDDEN
k6 run tests/load/ramp-up-test.js

# CORRECT
docker run --rm -v $(pwd):/tests grafana/k6:latest run /tests/ramp-up-test.js
```
