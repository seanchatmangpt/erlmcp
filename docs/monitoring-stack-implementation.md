# erlmcp v3 Monitoring Stack - Implementation Summary

## Overview

This document summarizes the complete monitoring stack implementation for erlmcp v3. The monitoring stack provides comprehensive observability including metrics collection, alerting, visualization, and log aggregation.

## Components

### 1. Prometheus - Metrics Collection and Storage

**Location:** `/Users/sac/erlmcp/docker/prometheus.yml`

**Features:**
- Scrapes metrics every 15 seconds from all targets
- 15-day data retention with 10GB size limit
- Remote write support for long-term storage
- Alert evaluation every 15 seconds
- Admin API and lifecycle management enabled

**Scrape Targets:**
- `erlmcp-core` - Main application metrics (port 9090)
- `otel-collector` - OpenTelemetry metrics (port 8888)
- `prometheus` - Self-monitoring
- `alertmanager` - Alertmanager metrics
- `node-exporter` - System metrics (CPU, memory, disk, network)
- `cadvisor` - Container metrics
- `grafana` - Grafana metrics

### 2. Alertmanager - Alert Routing and Notification

**Location:** `/Users/sac/erlmcp/docker/alertmanager.yml`

**Features:**
- Intelligent alert grouping and deduplication
- Multiple receivers: critical, warning, security, ops, performance
- Inhibition rules to prevent alert storms
- Email and webhook notifications
- 5-minute resolve timeout

**Alert Categories:**
- **Critical:** Immediate escalation (5s wait, 5m repeat)
- **Warning:** Team notifications (30s wait, 15m repeat)
- **Security:** Dedicated security team (10s wait, 10m repeat)
- **Infrastructure:** Ops team (30s wait, 30m repeat)
- **Performance:** Dev team (30s wait, 15m repeat)

### 3. Grafana - Visualization Dashboards

**Location:** `/Users/sac/erlmcp/docker/grafana/`

**Dashboards:**
1. **erlmcp-v3-metrics-dashboard.json** - Main metrics dashboard
   - Health status
   - Request rate
   - Error rate percentage
   - Latency percentiles (p50, p95, p99)
   - Memory usage breakdown
   - CPU usage
   - Connections by transport
   - Queue metrics
   - Security metrics
   - Process count and GC metrics

2. **erlmcp-enterprise-dashboard.json** - Enterprise overview
   - Request rate by method
   - Response time distribution
   - Active connections
   - Memory usage
   - Error rate by type
   - Process count

3. **performance-dashboard.json** - Performance monitoring
   - System overview
   - Error rate tracking
   - Request latency percentiles
   - Requests per second
   - Active connections
   - Memory and CPU usage
   - Queue metrics
   - Throughput tracking
   - Error breakdown
   - Response time heatmap

4. **cost-dashboard.json** - Cost optimization
   - Total cost overview
   - Cost by component
   - Resource utilization cost
   - Cost efficiency metrics
   - Optimization opportunities
   - Cost forecasting

### 4. Node Exporter - System Metrics

**Image:** `prom/node-exporter:v1.7.0`

**Metrics:**
- CPU usage per core
- Memory usage (total, available, buffers, cached)
- Disk I/O and filesystem usage
- Network interface statistics
- System load average
- Process count

### 5. cAdvisor - Container Metrics

**Image:** `gcr.io/cadvisor/cadvisor:v0.47.2`

**Metrics:**
- Container CPU usage
- Container memory usage
- Network I/O per container
- Disk I/O per container
- Container start/stop events

### 6. Loki - Log Aggregation

**Image:** `grafana/loki:2.9.2`

**Features:**
- Efficient log storage with boltdb-shipper
- Full-text search capabilities
- Label-based log querying
- 24-hour index periods

### 7. Promtail - Log Collection

**Image:** `grafana/promtail:2.9.2`

**Sources:**
- erlmcp application logs (`/var/log/erlmcp/*.log`)
- Docker container logs
- System logs (`/var/log/syslog`)

## Critical Metrics

The following critical metrics are collected for all erlmcp components:

### Request Metrics
- `erlmcp_requests_total` - Total request counter (by method, endpoint)
- `erlmcp_responses_total` - Total response counter (by status code)
- `erlmcp_request_duration_seconds` - Histogram of request latencies
- `erlmcp_request_rate` - Calculated rate from requests_total

### Error Metrics
- `erlmcp_errors_total` - Total error counter (by error_type)
- `erlmcp_error_rate` - Calculated error rate percentage
- `erlmcp_5xx_ratio` - 5xx response ratio
- `erlmcp_4xx_ratio` - 4xx response ratio

### Latency Metrics
- `erlmcp:p50_latency` - 50th percentile latency
- `erlmcp:p95_latency` - 95th percentile latency
- `erlmcp:p99_latency` - 99th percentile latency
- `erlmcp:avg_latency` - Average request duration

### Connection Metrics
- `erlmcp_connections_total` - Active connections (by transport)
- `erlmcp:connections_active` - Calculated active connections
- `erlmcp:connection_rate` - Connection creation rate
- `erlmcp:connection_close_rate` - Connection close rate

### Memory Metrics
- `erlmcp_memory_bytes` - Memory breakdown (total, processes, system, atom, binary)
- `erlmcp:memory_total_gb` - Total memory in GB
- `erlmcp:memory_usage_percent` - Memory utilization percentage

### CPU Metrics
- `erlmcp_cpu_usage_percent` - CPU usage percentage
- `erlmcp_reductions_total` - Total Erlang reductions
- `erlmcp_context_switches_total` - Total context switches

### Session Metrics
- `erlmcp_active_sessions` - Current active sessions
- `erlmcp_sessions_created_total` - Session creation counter
- `erlmcp_sessions_closed_total` - Session close counter
- `erlmcp_session_duration_seconds` - Session duration histogram

### Security Metrics
- `erlmcp_auth_attempts_total` - Authentication attempts
- `erlmcp_auth_failures_total` - Authentication failures
- `erlmcp_security_events_total` - Security events counter
- `erlmcp_rate_limit_exceeded_total` - Rate limit violations

### Queue Metrics
- `erlmcp_queue_size` - Current queue size
- `erlmcp_queue_wait_seconds` - Queue wait time histogram

### Process Metrics
- `erlmcp_process_count` - Number of Erlang processes
- `erlmcp_gc_count` - Garbage collection count
- `erlmcp_gc_words_reclaimed` - Words reclaimed by GC
- `erlmcp_run_queue_length` - Run queue length

## Alert Rules

### Critical Alerts

1. **ErlmcpDown** - Instance unreachable (30s)
2. **HealthCheckFailing** - Health check failed (1m)
3. **HighErrorRate** - Error rate > 5% (5m)
4. **CriticalLatency** - P95 latency > 2s (2m)
5. **CriticalMemoryUsage** - Memory > 7GB (2m)
6. **CriticalCPUUsage** - CPU > 95% (2m)
7. **CriticalQueueBacklog** - Queue > 20000 items (1m)
8. **HighAuthFailureRate** - Auth failures > 10/s (2m)

### Warning Alerts

1. **ErrorSpike** - Error rate increased 5x (1m)
2. **HighLatencyP99** - P99 latency > 1s (5m)
3. **HighMemoryUsage** - Memory > 4GB (10m)
4. **HighCPUUsage** - CPU > 80% (10m)
5. **ConnectionLimitReached** - Connections > 90% of limit (5m)
6. **HighConnectionRate** - Connection rate > 1000/s (5m)
7. **SessionErrors** - Session error rate > 1/s (5m)
8. **HighSessionCount** - Sessions > 100000 (10m)
9. **QueueBacklog** - Queue > 5000 items (5m)
10. **TransportErrorRate** - Transport errors > 5/s (5m)

## Recording Rules

Pre-computed metrics for dashboard performance:

- `erlmcp:request_rate` - Request rate (5m window)
- `erlmcp:error_rate` - Error rate percentage (5m window)
- `erlmcp:p50_latency` - 50th percentile latency
- `erlmcp:p95_latency` - 95th percentile latency
- `erlmcp:p99_latency` - 99th percentile latency
- `erlmcp:connections_active` - Active connections
- `erlmcp:memory_total_gb` - Memory in GB
- `erlmcp:cpu_usage_percent` - CPU percentage
- `erlmcp:auth_success_rate` - Authentication success rate
- `erlmcp:sla_uptime` - SLA uptime (30d)
- `erlmcp:sla_error_budget` - Remaining error budget

## Usage

### Start Monitoring Stack

```bash
# Start all monitoring services
docker compose --profile monitoring up -d

# Start individual service
docker compose --profile monitoring up prometheus
docker compose --profile monitoring up grafana
docker compose --profile monitoring up alertmanager
```

### Access Points

- **Prometheus:** http://localhost:9090
- **Grafana:** http://localhost:3000 (admin/admin)
- **Alertmanager:** http://localhost:9093
- **Node Exporter:** http://localhost:9100/metrics
- **cAdvisor:** http://localhost:8080
- **Loki:** http://localhost:3100

### Metrics Endpoint

The erlmcp application exposes metrics at:
```
http://localhost:9090/metrics
```

## Configuration Files

| File | Description |
|------|-------------|
| `docker/prometheus.yml` | Prometheus configuration |
| `docker/alert_rules.yml` | Alert rules |
| `docker/recording_rules.yml` | Recording rules |
| `docker/alertmanager.yml` | Alertmanager configuration |
| `docker/loki-config.yml` | Loki configuration |
| `docker/promtail-config.yml` | Promtail configuration |
| `docker/grafana/provisioning/datasources/prometheus.yml` | Grafana datasources |
| `docker/grafana/provisioning/dashboards/erlmcp.yml` | Grafana dashboards |
| `docker/grafana/dashboards/*.json` | Dashboard definitions |

## Implementation Status

- [x] Prometheus configuration with all scrape targets
- [x] Alertmanager with routing and inhibition rules
- [x] Grafana with provisioning and dashboards
- [x] Node Exporter for system metrics
- [x] cAdvisor for container metrics
- [x] Loki for log aggregation
- [x] Promtail for log collection
- [x] Critical alert rules
- [x] Recording rules for pre-computation
- [x] Metrics collector module (`erlmcp_metrics_collector.erl`)
- [x] Prometheus exporter module
- [x] Docker Compose integration
- [x] Volume persistence configuration

## Next Steps

1. Configure external notification channels (Slack, PagerDuty)
2. Set up long-term metrics storage (Cortex, Mimir, or remote write)
3. Configure log retention policies
4. Set up Grafana user authentication
5. Customize alert thresholds based on SLAs
6. Add synthetic monitoring checks
7. Integrate with SLO tracking
