# erlmcp v3 Observability Setup Guide
# =====================================
# Complete guide for Prometheus and OpenTelemetry integration

## Overview

This setup provides comprehensive observability for erlmcp with:

1. **Prometheus** - Metrics collection and storage
2. **OpenTelemetry Collector** - Unified telemetry pipeline
3. **Jaeger** - Distributed tracing
4. **Grafana** - Visualization dashboards
5. **Alertmanager** - Alert routing and notifications

## Quick Start

### 1. Start the Observability Stack

```bash
# From the config/docker directory
cd /Users/sac/erlmcp/config/docker

# Start all observability services
docker compose -f observability-stack.yml up -d

# Check service status
docker compose -f observability-stack.yml ps
```

### 2. Access Dashboards

| Service | URL | Credentials |
|---------|-----|-------------|
| Prometheus | http://localhost:9090 | - |
| Grafana | http://localhost:3000 | admin/admin |
| Alertmanager | http://localhost:9093 | - |
| Jaeger UI | http://localhost:16686 | - |
| OTel Collector Health | http://localhost:8888/health | - |

### 3. Configure erlmcp to Export Metrics

Add to your `vm.args` or sys.config:

```erlang
%% Enable metrics endpoint on port 9100
[
  {erlmcp_observability, [
    {prometheus_port, 9100},
    {prometheus_path, "/metrics"},
    {otel_endpoint, "http://otel-collector:4317"},
    {enable_tracing, true},
    {enable_metrics, true}
  ]}
].
```

## Configuration Files

### Prometheus Configuration

**File**: `/Users/sac/erlmcp/config/docker/prometheus.yml`

Scrape configurations for:
- erlmcp core metrics (port 9100)
- Transport metrics
- Session metrics
- Cluster metrics
- OTel collector metrics

### OpenTelemetry Collector Configuration

**File**: `/Users/sac/erlmcp/config/docker/otel-collector.yaml`

Receivers:
- OTLP gRPC: `0.0.0.0:4317`
- OTLP HTTP: `0.0.0.0:4318`
- Prometheus scraping

Exporters:
- Prometheus Remote Write
- Jaeger (traces)
- Logging (debug)

### Alert Rules

**File**: `/Users/sac/erlmcp/config/docker/erlmcp_alerts.yml`

Alert categories:
- Availability (service down, health checks)
- Performance (latency, throughput)
- Memory (usage, growth, ETS)
- Process count
- GC time
- Connections
- Sessions
- Cluster health

### Recording Rules

**File**: `/Users/sac/erlmcp/config/docker/erlmcp_recording_rules.yml`

Pre-computed queries:
- Request rates
- Error rates
- Latency percentiles (p50, p95, p99, p999)
- Memory ratios
- Scheduler utilization

## Metrics Reference

### Standard Erlang VM Metrics

| Metric | Type | Description |
|--------|------|-------------|
| `erlmcp_memory_total_bytes` | gauge | Total memory allocated |
| `erlmcp_memory_system_bytes` | gauge | Memory used by system |
| `erlmcp_memory_processes_bytes` | gauge | Memory used by processes |
| `erlmcp_memory_ets_bytes` | gauge | Memory used by ETS |
| `erlmcp_process_count` | gauge | Current process count |
| `erlmcp_process_limit` | gauge | Maximum processes |
| `erlmcp_reductions_total` | counter | Total reductions |
| `erlmcp_gc_count_total` | counter | GC collections |
| `erlmcp_gc_ms_total` | counter | GC time (ms) |
| `erlmcp_scheduler_utilization` | gauge | Scheduler utilization (0-1) |

### Application Metrics

| Metric | Type | Labels | Description |
|--------|------|--------|-------------|
| `erlmcp_requests_total` | counter | method, status | Total requests |
| `erlmcp_request_duration_seconds` | histogram | method | Request latency |
| `erlmcp_tool_calls_total` | counter | tool_name, status | Tool invocations |
| `erlmcp_transport_connections_active` | gauge | transport_type | Active connections |
| `erlmcp_transport_messages_total` | counter | transport_type, direction | Messages processed |
| `erlmcp_sessions_active` | gauge | - | Active sessions |
| `erlmcp_cluster_nodes` | gauge | - | Cluster node count |

### OpenTelemetry Metrics

| Metric | Type | Description |
|--------|------|-------------|
| `process_runtime_uptime_seconds` | gauge | Process uptime |
| `process_cpu_seconds_total` | counter | Total CPU time |
| `process_open_fds` | gauge | Open file descriptors |
| `runtime_erlang_erts_version` | gauge | Erlang/OTP version |

## Query Examples

### PromQL Queries for Dashboard

```promql
# Request rate per second
rate(erlmcp_requests_total[5m])

# P95 latency
histogram_quantile(0.95, rate(erlmcp_request_duration_seconds_bucket[5m]))

# Error rate
rate(erlmcp_requests_total{status=~"5.."}[5m]) / rate(erlmcp_requests_total[5m])

# Memory usage percentage
erlmcp_memory_total_bytes / erlmcp_memory_available_bytes * 100

# Active connections
sum(erlmcp_transport_connections_active) by (transport_type)

# Scheduler utilization
avg(erlmcp_scheduler_utilization) by (instance)

# GC time percentage
rate(erlmcp_gc_ms_total[5m]) / 1000 / 5 * 100

# Session duration P99
histogram_quantile(0.99, rate(erlmcp_session_duration_seconds_bucket[5m]))
```

## OpenTelemetry Integration

### Sending Metrics from erlmcp

```erlang
%% Initialize OpenTelemetry
ok = erlmcp_otel:init(#{
    service_name => <<"erlmcp-server">>,
    exporters => [otlp],
    endpoint => <<"http://otel-collector:4317">>,
    sampling => always_on
}).

%% Record a metric
erlmcp_otel:record_metric(<<"erlmcp.request.duration">>, DurationMs, #{
    <<"method">> => Method,
    <<"status">> => Status
}).

%% Create a span
SpanCtx = erlmcp_otel:start_span(<<"mcp.request">>, #{
    <<"method">> => Method,
    <<"request_id">> => RequestId
}),
try
    handle_request(Request),
    erlmcp_otel:end_span(SpanCtx)
catch
    Error:Reason ->
        erlmcp_otel:record_error(SpanCtx, {Error, Reason}),
        erlmcp_otel:end_span(SpanCtx)
end.
```

### Trace Context Propagation

```erlang
%% Inject trace context into outgoing request
TraceHeaders = erlmcp_otel:propagate_context(SpanCtx),
httpc:request(post, {"http://backend/api", [{"traceparent", TraceHeaders}], "application/json", Body}, []).

%% Extract trace context from incoming request
case cowboy_req:header(<<"traceparent">>, Req) of
    undefined ->
        SpanCtx = erlmcp_otel:start_span(<<"mcp.request">>, Attributes);
    TraceParent ->
        Headers = #{<<"traceparent">> => TraceParent},
        case erlmcp_otel:restore_context(Headers) of
            undefined -> SpanCtx = erlmcp_otel:start_span(<<"mcp.request">>, Attributes);
            ParentCtx -> SpanCtx = erlmcp_otel:start_span(<<"mcp.request">>, Attributes, ParentCtx)
        end
end.
```

## Grafana Dashboards

### Import Dashboards

1. Navigate to http://localhost:3000
2. Go to Dashboards -> Import
3. Upload the JSON files from `config/grafana/dashboards/`

### Dashboard Templates

- **erlmcp Overview** - System health, request rate, errors
- **Erlang VM** - Memory, processes, GC, scheduler
- **Transport** - Connection metrics, message throughput
- **Sessions** - Session lifecycle, duration
- **Cluster** - Node health, partition detection

## Troubleshooting

### Metrics Not Appearing

```bash
# Check Prometheus targets
curl http://localhost:9090/api/v1/targets | jq

# Check scrape config
docker exec erlmcp-prometheus cat /etc/prometheus/prometheus.yml

# View scrape logs
docker logs erlmcp-prometheus --tail 100
```

### OTel Collector Issues

```bash
# Check collector health
curl http://localhost:8888/health

# View collector metrics
curl http://localhost:8888/metrics

# Check collector logs
docker logs erlmcp-otel-collector --tail 100
```

### Alerts Firing

```bash
# View active alerts
curl http://localhost:9090/api/v1/alerts | jq

# Check Alertmanager status
curl http://localhost:9093/api/v2/status | jq
```

## File Locations

| File | Path |
|------|------|
| Prometheus Config | `/Users/sac/erlmcp/config/docker/prometheus.yml` |
| OTEL Collector Config | `/Users/sac/erlmcp/config/docker/otel-collector.yaml` |
| Alert Rules | `/Users/sac/erlmcp/config/docker/erlmcp_alerts.yml` |
| Recording Rules | `/Users/sac/erlmcp/config/docker/erlmcp_recording_rules.yml` |
| Alertmanager Config | `/Users/sac/erlmcp/config/docker/alertmanager.yml` |
| Observability Stack | `/Users/sac/erlmcp/config/docker/observability-stack.yml` |
| Metrics Reference | `/Users/sac/erlmcp/config/docker/ERLMCP_METRICS_REFERENCE.md` |

## Next Steps

1. Configure environment variables in `.env`
2. Customize alert thresholds in `erlmcp_alerts.yml`
3. Create Grafana dashboards for your use case
4. Set up notification channels (Slack, PagerDuty, etc.)
5. Review and tune recording rules for performance

## References

- [Prometheus Documentation](https://prometheus.io/docs/)
- [OpenTelemetry Erlang](https://opentelemetry.io/docs/instrumentation/erlang/)
- [Jaeger Documentation](https://www.jaegertracing.io/docs/)
- [Grafana Documentation](https://grafana.com/docs/)
