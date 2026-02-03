# erlmcp Health Endpoints - Complete Implementation Guide

## Overview

The erlmcp observability subsystem provides comprehensive health check endpoints for Docker Swarm, Kubernetes, and load balancer integration. These endpoints return JSON status information for system monitoring and orchestration.

**Server Port**: 8080 (configurable via `ERLMCP_HEALTH_PORT` env var)
**Module**: `erlmcp_health_http`
**Server**: `erlmcp_health_http_server`

---

## Endpoints

### 1. GET /health - Comprehensive Health Check

Returns detailed system health status including system checks, memory, ETS tables, and processes.

**URL**: `http://localhost:8080/health`

**Success Response** (200 OK):
```json
{
  "status": "healthy",
  "version": "3.0.0",
  "uptime": 3600,
  "node": "erlmcp@127.0.0.1",
  "timestamp": "2026-02-02T23:00:00Z",
  "checks": {
    "system": {
      "status": "healthy",
      "details": "healthy"
    },
    "memory": {
      "status": "healthy",
      "details": "healthy"
    },
    "ets_tables": {
      "status": "healthy",
      "details": "healthy"
    },
    "processes": {
      "status": "healthy",
      "details": "healthy"
    }
  },
  "metrics": {
    "memory_total": 52428800,
    "memory_processes": 20971520,
    "memory_system": 31457280,
    "memory_ets": 1048576,
    "memory_atom": 524288,
    "memory_binary": 2097152,
    "memory_code": 4194304,
    "process_count": 250,
    "process_limit": 262144,
    "ets_count": 42,
    "atom_count": 8542,
    "atom_limit": 1048576,
    "port_count": 12,
    "port_limit": 65536,
    "scheduler_count": 8,
    "scheduler_online": 8,
    "run_queue": 0,
    "gc_count": 1523,
    "words_reclaimed": 10485760,
    "io_input": 2048,
    "io_output": 4096,
    "reductions": 1234567890,
    "context_switches": 987654321
  }
}
```

**Unhealthy Response** (503 Service Unavailable):
```json
{
  "status": "unhealthy",
  "version": "3.0.0",
  "uptime": 3600,
  "node": "erlmcp@127.0.0.1",
  "timestamp": "2026-02-02T23:00:00Z",
  "checks": {
    "system": {
      "status": "healthy",
      "details": "healthy"
    },
    "memory": {
      "status": "unhealthy",
      "details": "unhealthy"
    },
    "ets_tables": {
      "status": "healthy",
      "details": "healthy"
    },
    "processes": {
      "status": "degraded",
      "details": "degraded"
    }
  },
  "metrics": { ... }
}
```

---

### 2. GET /ready - Readiness Probe

Returns whether the service is ready to accept connections and serve traffic.

**URL**: `http://localhost:8080/ready`

**Success Response** (200 OK):
```json
{
  "ready": true,
  "node": "erlmcp@127.0.0.1",
  "timestamp": "2026-02-02T23:00:00Z",
  "checks": {
    "registry": "ready",
    "session_manager": "ready",
    "http_server": "ready",
    "observability": "ready",
    "transports": "ready"
  }
}
```

**Not Ready Response** (503 Service Unavailable):
```json
{
  "ready": false,
  "node": "erlmcp@127.0.0.1",
  "timestamp": "2026-02-02T23:00:00Z",
  "checks": {
    "registry": "ready",
    "session_manager": "not_ready",
    "http_server": "ready",
    "observability": "ready",
    "transports": "ready"
  }
}
```

---

### 3. GET /live - Liveness Probe

Returns whether the Erlang VM process is alive and responding.

**URL**: `http://localhost:8080/live`

**Response** (200 OK):
```json
{
  "alive": true,
  "node": "erlmcp@127.0.0.1",
  "uptime": 3600,
  "timestamp": "2026-02-02T23:00:00Z"
}
```

---

### 4. GET /metrics - Prometheus Metrics

Returns system metrics in Prometheus text format.

**URL**: `http://localhost:8080/metrics`

**Response** (200 OK, text/plain):
```
# HELP erlmcp_process_count Current number of processes
# TYPE erlmcp_process_count gauge
erlmcp_process_count 250

# HELP erlmcp_process_limit Maximum number of processes
# TYPE erlmcp_process_limit gauge
erlmcp_process_limit 262144

# HELP erlmcp_memory_bytes Total memory in bytes
# TYPE erlmcp_memory_bytes gauge
erlmcp_memory_bytes 52428800

# HELP erlmcp_memory_processes_bytes Memory used by processes
# TYPE erlmcp_memory_processes_bytes gauge
erlmcp_memory_processes_bytes 20971520

# HELP erlmcp_memory_system_bytes Memory used by system
# TYPE erlmcp_memory_system_bytes gauge
erlmcp_memory_system_bytes 31457280

# HELP erlmcp_ets_count Number of ETS tables
# TYPE erlmcp_ets_count gauge
erlmcp_ets_count 42

# HELP erlmcp_atom_count Number of atoms
# TYPE erlmcp_atom_count gauge
erlmcp_atom_count 8542

# HELP erlmcp_port_count Number of ports
# TYPE erlmcp_port_count gauge
erlmcp_port_count 12

# HELP erlmcp_scheduler_count Number of schedulers
# TYPE erlmcp_scheduler_count gauge
erlmcp_scheduler_count 8

# HELP erlmcp_run_queue_length Run queue length
# TYPE erlmcp_run_queue_length gauge
erlmcp_run_queue_length 0

# HELP erlmcp_uptime_seconds Uptime in seconds
# TYPE erlmcp_uptime_seconds gauge
erlmcp_uptime_seconds 3600
```

---

## Health Check Details

### System Status Checks

The `/health` endpoint performs comprehensive system checks:

1. **Application Status**
   - Verifies `erlmcp_core` application is running
   - Checks application list visibility

2. **Node Status**
   - Pings local node via `net_adm:ping/1`
   - Verifies distributed Erlang connectivity

3. **Scheduler Status**
   - Checks run queue length (< 1000 is healthy)
   - Monitors scheduler utilization

### Memory Status Checks

Memory health is determined by thresholds:

- **Healthy**: Process memory < 80% of total, ETS memory < 80% of total
- **Degraded**: Process memory 80-90% or ETS memory 80-90%
- **Unhealthy**: Process memory > 90% or ETS memory > 90%

**Metrics Collected**:
- Total memory
- Process memory
- System memory
- ETS memory
- Atom memory
- Binary memory
- Code memory

### ETS Table Checks

ETS table health verifies:

1. **Critical Tables Present**:
   - `erlmcp_registry`
   - `erlmcp_session_table`
   - `erlmcp_receipt_chain`
   - `erlmcp_audit_log`

2. **Table Count**:
   - Healthy: Table count ≤ 10,000
   - Degraded: Table count > 10,000
   - Unhealthy: Critical tables missing

### Process Status Checks

Process health is determined by:

- **Healthy**: Process count < 80% of limit
- **Degraded**: Process count 80-90% of limit
- **Unhealthy**: Process count > 90% of limit

---

## Readiness Checks

The `/ready` endpoint verifies critical services are initialized:

| Service | Check | Description |
|---------|-------|-------------|
| `registry` | `whereis(erlmcp_registry)` | Process registry available |
| `session_manager` | `whereis(erlmcp_session_manager)` | Session management ready |
| `http_server` | `whereis(erlmcp_health_http_server)` | Health HTTP server running |
| `observability` | Monitor, metrics, recovery manager | Observability stack ready |
| `transports` | `whereis(erlmcp_transport_sup)` | Transport supervisor alive |

All services must return `"ready"` for overall readiness to be `true`.

---

## Integration Examples

### Docker HEALTHCHECK

Add to your `Dockerfile`:

```dockerfile
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
  CMD curl -f http://localhost:8080/health || exit 1
```

Or with wget:

```dockerfile
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
  CMD wget -q --spider http://localhost:8080/health || exit 1
```

### Docker Compose

```yaml
services:
  erlmcp:
    image: erlmcp:3.0.0
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/health"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s
```

### Kubernetes Probes

Add to your deployment YAML:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp
spec:
  template:
    spec:
      containers:
      - name: erlmcp
        image: erlmcp:3.0.0
        ports:
        - containerPort: 8080
          name: health
        livenessProbe:
          httpGet:
            path: /live
            port: health
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: health
          initialDelaySeconds: 10
          periodSeconds: 5
        startupProbe:
          httpGet:
            path: /ready
            port: health
          initialDelaySeconds: 5
          periodSeconds: 5
          failureThreshold: 30
```

### Prometheus Scraping

Configure Prometheus:

```yaml
scrape_configs:
  - job_name: 'erlmcp'
    metrics_path: '/metrics'
    static_configs:
      - targets: ['localhost:8080']
```

---

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ERLMCP_HEALTH_PORT` | 8080 | Health server port |

### Application Configuration

```erlang
{erlmcp_observability, [
  {health_port, 8080}
]}.
```

---

## Module Files

### `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_health_http.erl`

Main health check handler module:
- Cowboy handler behaviour
- 4 endpoints: `/health`, `/ready`, `/live`, `/metrics`
- Comprehensive system, memory, ETS, and process checks
- JSON response formatting with OTP 27+ `json` module
- Fallback JSON encoder for compatibility

**Key Functions**:
- `handle_health/1` - Comprehensive health check
- `handle_ready/1` - Readiness probe
- `handle_live/1` - Liveness probe
- `handle_metrics/1` - Prometheus metrics
- `check_system_status/0` - System health verification
- `check_memory_status/0` - Memory health analysis
- `check_ets_tables/0` - ETS table validation
- `check_process_status/0` - Process utilization check

### `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_health_http_server.erl`

Health HTTP server (gen_server):
- Starts Cowboy HTTP listener on port 8080
- Async listener startup (doesn't block supervisor)
- Routes to `erlmcp_health_http` handler
- Configurable via environment or app config

**Key Functions**:
- `start_link/0` - Start with default port
- `start_link/1` - Start with custom port
- `stop/0` - Stop the server
- `get_port/0` - Get current port

---

## Testing

### Manual Testing

```bash
# Test health endpoint
curl http://localhost:8080/health | jq

# Test readiness endpoint
curl http://localhost:8080/ready | jq

# Test liveness endpoint
curl http://localhost:8080/live | jq

# Test metrics endpoint
curl http://localhost:8080/metrics

# Test with wget
wget -qO- http://localhost:8080/health | jq
```

### Automated Testing

```bash
# Test all endpoints
#!/bin/bash
set -e

echo "Testing /health..."
curl -f http://localhost:8080/health | jq .status | grep -q healthy

echo "Testing /ready..."
curl -f http://localhost:8080/ready | jq .ready | grep -q true

echo "Testing /live..."
curl -f http://localhost:8080/live | jq .alive | grep -q true

echo "Testing /metrics..."
curl -f http://localhost:8080/metrics | grep -q erlmcp_process_count

echo "All health checks passed!"
```

---

## Troubleshooting

### Port Already in Use

```bash
# Check what's using port 8080
lsof -i :8080

# Use different port
ERLMCP_HEALTH_PORT=9090 docker compose up
```

### Health Checks Failing

1. **Check logs**:
   ```bash
   docker logs erlmcp
   ```

2. **Verify application started**:
   ```bash
   curl http://localhost:8080/health | jq
   ```

3. **Check individual services**:
   ```bash
   curl http://localhost:8080/ready | jq .checks
   ```

### Metrics Not Available

- Ensure `/metrics` endpoint is accessible
- Check Prometheus scraping configuration
- Verify server port and firewall rules

---

## Performance Considerations

- **Lightweight checks**: Health endpoints use O(1) lookups where possible
- **No blocking operations**: All checks are non-blocking
- **Async listener startup**: Server doesn't block supervisor tree
- **Minimal overhead**: Checks add < 1ms to response time

---

## Security

- **No authentication**: Health endpoints are for internal use only
- **Bind to localhost**: In production, use firewall rules or network policies
- **Rate limiting**: Consider rate limiting for public deployments
- **TLS**: Use nginx/HAProxy reverse proxy for TLS termination

---

## Future Enhancements

Potential improvements:
1. **Authentication**: Optional API key authentication
2. **Detailed metrics**: Per-process memory breakdown
3. **Historical data**: Time-series health history
4. **Alert integration**: Webhook alerts on health changes
5. **Custom checks**: Plugin system for custom health checks

---

## References

- **Module**: `erlmcp_health_http`
- **Server**: `erlmcp_health_http_server`
- **Supervisor**: `erlmcp_observability_sup`
- **Port**: 8080 (configurable)
- **Format**: JSON (health/ready/live), text/plain (metrics)
- **Standards**: Docker HEALTHCHECK, Kubernetes probes, Prometheus metrics
