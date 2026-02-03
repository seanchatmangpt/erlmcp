# Docker Quick Start Guide for erlmcp v3.0.0

Get erlmcp running in under 5 minutes using Docker. No Erlang/OTP installation required.

## Prerequisites

- Docker 24.0 or later
- Docker Compose v2.27 or later
- 4GB RAM available
- 10GB disk space available

### Verify Docker Installation

```bash
docker --version
# Docker version 24.0.0 or later required

docker compose version
# Docker Compose version v2.27.0 or later required
```

---

## 5-Minute Quick Start

### Step 1: Clone Repository (30 seconds)

```bash
git clone https://github.com/banyan-platform/erlmcp.git
cd erlmcp
```

### Step 2: Start Runtime (1 minute)

```bash
# Start production runtime
docker compose --profile runtime up -d

# Wait for health check (45 seconds)
docker compose ps
```

**Expected Output:**
```
NAME          IMAGE          STATUS          PORTS
erlmcp        erlmcp:3.0.0   Up 45 seconds   0.0.0.0:8080->8080/tcp, 0.0.0.0:9090->9090/tcp, 0.0.0.0:9100->9100/tcp
```

### Step 3: Verify Health (30 seconds)

```bash
# Check HTTP health endpoint
curl http://localhost:8080/health

# Expected response:
# {"status":"healthy","version":"3.0.0","uptime":"1m","checks":{"registry":"ok","session":"ok","monitoring":"ok"}}
```

### Step 4: Run Tests (2 minutes)

```bash
# Compile via quality lane
docker compose run --rm erlmcp-build rebar3 compile

# Run unit tests
docker compose run --rm erlmcp-unit rebar3 eunit

# Run integration tests
docker compose run --rm erlmcp-ct rebar3 ct
```

---

## What's Running?

| Port | Service | Purpose |
|------|---------|---------|
| 8080 | HTTP API | JSON-RPC over HTTP endpoint |
| 9090 | Health | Health check endpoint |
| 9100 | Metrics | Prometheus metrics scraping |

### Test the HTTP API

```bash
# MCP initialize request
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {
      "protocolVersion": "2025-11-25",
      "capabilities": {},
      "clientInfo": {
        "name": "erlmcp-test",
        "version": "1.0.0"
      }
    }
  }'
```

---

## Docker Profiles

erlmcp provides multiple Docker Compose profiles for different use cases.

### Available Profiles

| Profile | Use Case | Services |
|---------|----------|----------|
| `runtime` | Production runtime | erlmcp (minimal image) |
| `dev` | Local development | erlmcp-dev (with debug tools) |
| `build` | Compilation | erlmcp-build (full build environment) |
| `unit` | Unit tests | erlmcp-unit (EUnit) |
| `ct` | Integration tests | erlmcp-ct (Common Test) |
| `check` | Quality analysis | erlmcp-check (Dialyzer, Xref) |
| `bench` | Benchmarks | erlmcp-bench (performance tests) |
| `monitoring` | Observability | Prometheus, Grafana, Alertmanager |

### Using Profiles

```bash
# Single profile
docker compose --profile runtime up -d

# Multiple profiles
docker compose --profile runtime --profile monitoring up -d

# Development with monitoring
docker compose --profile dev --profile monitoring up -d
```

---

## Monitoring Setup

### Start Monitoring Stack (1 minute)

```bash
# Start monitoring services
docker compose --profile monitoring up -d

# Access dashboards
# Grafana: http://localhost:3000 (admin/admin)
# Prometheus: http://localhost:9090
# Alertmanager: http://localhost:9093
```

### Pre-Configured Dashboards

Grafana includes pre-configured dashboards:

- **erlmcp Overview**: System health, request rate, error rate
- **Performance Metrics**: Latency (P50, P95, P99), throughput
- **Resource Usage**: CPU, memory, network, disk
- **Transport Metrics**: Per-transport statistics

### Query Metrics

```bash
# Query Prometheus directly
curl -s http://localhost:9090/api/v1/query?query=up | jq

# Get request rate
curl -s 'http://localhost:9090/api/v1/query?query=rate(http_requests_total[5m])' | jq
```

---

## Development Workflow

### Full Development Cycle

```bash
# 1. Start development container
docker compose --profile dev up -d

# 2. Attach to development shell
docker attach erlmcp-dev

# 3. In the container, work with erlmcp
erlmcp start
erlmcp test-100k
erlmcp benchmark

# 4. Run tests from host
docker compose run --rm erlmcp-unit rebar3 eunit
docker compose run --rm erlmcp-ct rebar3 ct

# 5. Run quality gates
docker compose run --rm erlmcp-check make check
```

### Interactive Development

```bash
# Start development shell
docker compose run --rm erlmcp-dev /bin/bash

# Inside container:
cd /workspace
rebar3 compile
rebar3 eunit
make console
```

---

## Troubleshooting

### Container Won't Start

**Problem**: Container exits immediately

```bash
# Check logs
docker logs erlmcp

# Common issues:
# - Port 8080 already in use
# - Insufficient memory
# - Permission denied on volumes
```

**Solution**:

```bash
# Check port availability
lsof -i :8080

# Change ports in .env
echo "ERLMCP_PORT=8081" > .env
docker compose --profile runtime up -d
```

### Health Check Failing

**Problem**: Health endpoint returns 503

```bash
# Check health status
docker exec erlmcp /opt/erlmcp/bin/healthcheck.sh

# Check application logs
docker logs erlmcp --tail 100
```

**Solution**:

```bash
# Restart container
docker compose restart erlmcp

# If still failing, check logs for specific error
docker logs erlmcp --tail 500 | grep -i error
```

### Out of Memory

**Problem**: Container OOM killed

```bash
# Check OOM events
docker inspect erlmcp | jq '.[0].State.OOMKilled'

# Check memory usage
docker stats erlmcp
```

**Solution**:

```bash
# Increase memory limit in .env
echo "ERLMCP_MEMORY_LIMIT=8G" >> .env
docker compose --profile runtime up -d
```

### Slow Build Times

**Problem**: First build takes 20+ minutes

```bash
# Check if Hex cache is working
docker volume inspect erlmcp_hex-cache
```

**Solution**:

```bash
# Use persistent Hex cache
docker compose --profile build run --rm erlmcp-build \
  sh -c "rebar3 get-deps && rebar3 compile"

# Subsequent builds will use cache
```

### Can't Access Grafana

**Problem**: Grafana dashboard won't load

```bash
# Check Grafana status
docker ps | grep grafana

# Check Grafana logs
docker logs erlmcp-grafana
```

**Solution**:

```bash
# Reset Grafana admin password
echo "GF_SECURITY_ADMIN_PASSWORD=newpassword" >> .env
docker compose --profile monitoring up -d grafana
```

---

## Advanced Configuration

### Environment Variables

Create a `.env` file in the project root:

```bash
# Application ports
ERLMCP_PORT=8080
ERL_DIST_PORT=9100
METRICS_PORT=9100
HEALTH_PORT=9090

# Resource limits
ERLMCP_CPU_LIMIT=2.0
ERLMCP_MEMORY_LIMIT=4G

# Logging
ERLMCP_LOG_LEVEL=info

# Erlang configuration
ERLANG_COOKIE=your-secure-cookie
ERLMCP_NODE_NAME=erlmcp@localhost

# Grafana
GRAFANA_ADMIN_USER=admin
GRAFANA_ADMIN_PASSWORD=admin

# Prometheus
PROMETHEUS_PORT=9090
PROMETHEUS_RETENTION=15d
```

### Custom Configuration

```bash
# Mount custom sys.config
docker compose --profile runtime up -d \
  -v $(pwd)/config/custom.config:/opt/erlmcp/etc/sys.config:ro

# Mount custom vm.args
docker compose --profile runtime up -d \
  -v $(pwd)/vm.args:/opt/erlmcp/releases/3.0.0/vm.args:ro
```

### Production Secrets

```bash
# Create secrets file
cat > .env.secrets << EOF
ERLANG_COOKIE=$(openssl rand -base64 32)
DB_PASSWORD=$(openssl rand -base64 16)
REDIS_PASSWORD=$(openssl rand -base64 16)
EOF

# Source secrets
source .env.secrets

# Deploy with secrets
docker compose --profile runtime up -d
```

---

## Cleanup

### Stop Services

```bash
# Stop runtime
docker compose --profile runtime down

# Stop monitoring
docker compose --profile monitoring down

# Stop all
docker compose down
```

### Remove Volumes

```bash
# Remove all volumes (WARNING: deletes data)
docker compose down -v

# Remove specific volumes
docker volume rm erlmcp_erlmcp-data erlmcp_erlmcp-logs
```

### Clean Build Artifacts

```bash
# Remove build cache
docker volume rm erlmcp_erlmcp-build-cache

# Remove Hex cache
docker volume rm erlmcp_hex-cache

# Rebuild from scratch
docker compose build --no-cache
```

---

## Next Steps

- [Development vs Production](DEVELOPMENT_VS_PRODUCTION.md) - Choose your configuration
- [Production Deployment](DOCKER_DEPLOYMENT_CHECKLIST.md) - Production readiness
- [CLI Reference](docs/CLI_REFERENCE.md) - Command-line tools
- [Architecture](docs/architecture.md) - System design

---

## Support

- **Issues**: [GitHub Issues](https://github.com/banyan-platform/erlmcp/issues)
- **Documentation**: [Full Documentation](README.md#documentation)
- **Discussions**: [GitHub Discussions](https://github.com/banyan-platform/erlmcp/discussions)
