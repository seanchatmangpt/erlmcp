# erlmcp v3 Enterprise Metrics Reference

This document provides a comprehensive reference for all metrics collected by the enterprise monitoring stack for erlmcp v3.

## Table of Contents

1. [Core System Metrics](#core-system-metrics)
2. [Application Metrics](#application-metrics)
3. [Transport Metrics](#transport-metrics)
4. [Session Metrics](#session-metrics)
5. [Registry Metrics](#registry-metrics)
6. [Security Metrics](#security-metrics)
7. [Business Metrics](#business-metrics)
8. [SLA Metrics](#sla-metrics)
9. [Custom Metrics](#custom-metrics)

## Metric Naming Conventions

### General Guidelines

- Use lowercase letters and underscores
- Group related metrics with prefixes
- Include units in metric names
- Use consistent labeling

### Naming Hierarchy

```
erlmcp_{component}_{metric_type}_{metric_name}_{unit}
```

Examples:
- `erlmcp_core_requests_total`
- `erlmcp_transports_connections_active`
- `erlmcp_sessions_duration_seconds`

## Core System Metrics

### System Health

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_system_health_status` | Gauge | Overall system health status | `status` (healthy, degraded, unhealthy) | 1 |
| `erlmcp_system_uptime` | Gauge | System uptime in seconds | - | seconds |
| `erlmcp_system_version` | Gauge | erlmcp version | `version` | - |

### Resource Utilization

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_system_cpu_usage` | Gauge | CPU usage percentage | `core` | % |
| `erlmcp_system_memory_bytes` | Gauge | Memory usage | `type` (total, used, free) | bytes |
| `erlmcp_system_disk_bytes` | Gauge | Disk usage | `mountpoint` | bytes |
| `erlmcp_system_process_count` | Gauge | Number of running processes | - | count |

### Load Metrics

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_system_load_1min` | Gauge | 1-minute load average | - | load |
| `erlmcp_system_load_5min` | Gauge | 5-minute load average | - | load |
| `erlmcp_system_load_15min` | Gauge | 15-minute load average | - | load |

## Application Metrics

### Request Metrics

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_requests_total` | Counter | Total number of requests | `method`, `endpoint`, `status`, `component` | count |
| `erlmcp_request_duration_seconds` | Histogram | Request duration | `endpoint`, `status`, `component` | seconds |
| `erlmcp_request_size_bytes` | Histogram | Request size | `endpoint`, `component` | bytes |
| `erlmcp_response_size_bytes` | Histogram | Response size | `endpoint`, `status`, `component` | bytes |

### Error Metrics

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_errors_total` | Counter | Total number of errors | `error_type`, `component`, `endpoint` | count |
| `erlmcp_error_rate` | Gauge | Error rate percentage | `component` | % |
| `erlmcp_error_messages` | Counter | Error messages count | `error_code`, `component` | count |

### Performance Metrics

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_throughput_rps` | Gauge | Requests per second | `component` | req/s |
| `erlmcp_p50_latency` | Gauge | 50th percentile latency | `component` | seconds |
| `erlmcp_p95_latency` | Gauge | 95th percentile latency | `component` | seconds |
| `erlmcp_p99_latency` | Gauge | 99th percentile latency | `component` | seconds |
| `erlmcp_apdex_score` | Gauge | Apdex satisfaction score | `component` | score |

## Transport Metrics

### Connection Metrics

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_connections_active` | Gauge | Active connections | `transport_type`, `state` | count |
| `erlmcp_connections_total` | Counter | Total connections created | `transport_type` | count |
| `erlmcp_connections_closed` | Counter | Total connections closed | `transport_type`, `reason` | count |
| `erlmcp_connection_errors` | Counter | Connection errors | `transport_type`, `error_type` | count |

### Transport-Specific Metrics

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_tcp_connections` | Gauge | TCP connections | `state` | count |
| `erlmcp_http_requests` | Counter | HTTP requests | `method`, `path` | count |
| `erlmcp_websocket_messages` | Counter | WebSocket messages | `direction` (in, out) | count |
| `erlmcp_sse_connections` | Gauge | SSE connections | `state` | count |

## Session Metrics

### Session Lifecycle

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_sessions_active` | Gauge | Active sessions | `session_type` | count |
| `erlmcp_sessions_created` | Counter | Sessions created | `session_type` | count |
| `erlmcp_sessions_closed` | Counter | Sessions closed | `session_type`, `reason` | count |
| `erlmcp_sessions_expired` | Counter | Sessions expired | `session_type` | count |

### Session Duration

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_session_duration_seconds` | Histogram | Session duration | `session_type` | seconds |
| `erlmcp_session_max_duration_seconds` | Gauge | Maximum session duration | - | seconds |
| `erlmcp_session_avg_duration_seconds` | Gauge | Average session duration | `session_type` | seconds |

### Session Utilization

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_session_utilization` | Gauge | Session utilization percentage | `session_type` | % |
| `erlmcp_session_pool_size` | Gauge | Session pool size | `pool_name` | count |
| `erlmcp_session_queue_depth` | Gauge | Session queue depth | `queue_name` | count |

## Registry Metrics

### Resource Operations

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_resources_registered` | Counter | Resources registered | `resource_type` | count |
| `erlmcp_resources_unregistered` | Counter | Resources unregistered | `resource_type` | count |
| `erlmcp_resources_accessed` | Counter | Resources accessed | `resource_type`, `access_type` | count |
| `erlmcp_resources_errors` | Counter | Resource operation errors | `operation`, `error_type` | count |

### Registry Performance

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_registry_operations_total` | Counter | Total registry operations | `operation` | count |
| `erlmcp_registry_operation_duration_seconds` | Histogram | Registry operation duration | `operation` | seconds |
| `erlmcp_registry_lookup_time_ms` | Gauge | Registry lookup time | - | milliseconds |
| `erlmcp_registry_cache_hit_ratio` | Gauge | Cache hit ratio | - | ratio |

### Subscription Metrics

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_subscriptions_active` | Gauge | Active subscriptions | `subscription_type` | count |
| `erlmcp_subscriptions_created` | Counter | Subscriptions created | `subscription_type` | count |
| `erlmcp_subscriptions_cancelled` | Counter | Subscriptions cancelled | `subscription_type` | count |
| `erlmcp_subscription_events` | Counter | Subscription events | `event_type` | count |

## Security Metrics

### Authentication Metrics

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_auth_attempts` | Counter | Authentication attempts | `result` (success, failure) | count |
| `erlmcp_auth_success_rate` | Gauge | Authentication success rate | - | % |
| `erlmcp_auth_failures` | Counter | Authentication failures | `error_type` | count |
| `erlmcp_brute_force_attempts` | Counter | Brute force attempts | `source_ip` | count |

### Authorization Metrics

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_authorization_checks` | Counter | Authorization checks | `result` (allowed, denied) | count |
| `erlmcp_unauthorized_attempts` | Counter | Unauthorized access attempts | `endpoint` | count |
| `erlmcp_permission_violations` | Counter | Permission violations | `permission_type` | count |

### Security Events

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_security_events` | Counter | Security events | `event_type`, `severity` | count |
| `erlmcp_intrusion_attempts` | Counter | Intrusion attempts | `attack_type` | count |
| `erlmcp_suspicious_activity` | Counter | Suspicious activity | `activity_type` | count |
| `erlmcp_policy_violations` | Counter | Policy violations | `policy_type` | count |

## Business Metrics

### API Usage

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_api_quota_usage` | Gauge | API quota usage | `quota_type` | percentage |
| `erlmcp_api_quota_limit` | Gauge | API quota limit | `quota_type` | count |
| `erlmcp_api_requests` | Counter | API requests | `endpoint`, `tier` | count |
| `erlmcp_api_cost` | Counter | API cost | `currency` | currency |

### Tool Execution

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_tool_executions` | Counter | Tool executions | `tool_name`, `status` | count |
| `erlmcp_tool_execution_time` | Histogram | Tool execution time | `tool_name` | seconds |
| `erlmcp_tool_errors` | Counter | Tool execution errors | `tool_name`, `error_type` | count |
| `erlmcp_tool_usage_rate` | Gauge | Tool usage rate | `tool_name` | req/s |

### Prompt Execution

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_prompt_executions` | Counter | Prompt executions | `prompt_type`, `status` | count |
| `erlmcp_prompt_execution_time` | Histogram | Prompt execution time | `prompt_type` | seconds |
| `erlmcp_prompt_tokens_used` | Counter | Tokens used in prompts | `model_type` | tokens |
| `erlmcp_prompt_costs` | Counter | Prompt execution costs | `currency` | currency |

## SLA Metrics

### Uptime Metrics

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_uptime_percentage` | Gauge | System uptime percentage | `time_period` | % |
| `erlmcp_downtime_seconds` | Counter | Total downtime | `reason` | seconds |
| `erlmcp_incident_count` | Counter | Number of incidents | `severity` | count |
| `erlmcp_incident_resolution_time` | Histogram | Incident resolution time | `severity` | seconds |

### Performance SLAs

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_response_time_p99` | Gauge | P99 response time | `endpoint` | seconds |
| `erlmcp_response_time_p95` | Gauge | P95 response time | `endpoint` | seconds |
| `erlmcp_response_time_p50` | Gauge | P50 response time | `endpoint` | seconds |
| `erlmcp_sla_compliance` | Gauge | SLA compliance percentage | `sla_type` | % |

### Financial Metrics

| Metric Name | Type | Description | Labels | Unit |
|-------------|------|-------------|--------|------|
| `erlmcp_sla_penalties` | Counter | SLA penalties incurred | `currency` | currency |
| `erlmcp_sla_credits` | Counter | SLA credits earned | `currency` | currency |
| `erlmcp_incident_costs` | Counter | Incident resolution costs | `currency` | currency |
| `erlmcp_monitoring_costs` | Gauge | Monitoring costs | `cost_type` | currency |

## Custom Metrics

### Adding Custom Metrics

#### Via Erlang Code

```erlang
% Create a counter
erlmcp_metrics:create_counter(<<"my_custom_metric">>, "Custom metric description").

% Increment counter
erlmcp_metrics:inc(<<"my_custom_metric">>, 1).

% Create a gauge
erlmcp_metrics:create_gauge(<<"my_custom_gauge">>, "Custom gauge description").

% Set gauge value
erlmcp_metrics:gauge(<<"my_custom_gauge">>, 100).

% Record histogram
erlmcp_metrics:observe(<<"my_custom_histogram">>, 42.5).
```

#### Via HTTP API

```bash
# Increment counter
curl -X POST http://erlmcp:8080/metrics/custom \
  -d 'name=custom_counter&value=1'

# Set gauge
curl -X PUT http://erlmcp:8080/metrics/custom \
  -d 'name=custom_gauge&value=42'

# Record histogram
curl -X POST http://erlmcp:8080/metrics/custom \
  -d 'name=custom_histogram&value=25'
```

### Custom Metric Types

#### Counters

- Monotonically increasing values
- Reset only on restart
- Example: `erlmcp_custom_events_total`

#### Gauges

- Instantaneous values
- Can go up or down
- Example: `erlmcp_custom_current_value`

#### Histograms

- Distribution of values
- Buckles for percentiles
- Example: `erlmcp_custom_request_duration`

#### Summaries

- Similar to histograms
- Calculated on client side
- Example: `erlmcp_custom_summary`

## Metric Labels

### Common Labels

| Label | Description | Values |
|-------|-------------|--------|
| `component` | Component name | core, transports, sessions, registry |
| `status` | HTTP status code | 200, 400, 500, etc. |
| `method` | HTTP method | GET, POST, PUT, DELETE |
| `endpoint` | API endpoint | /api/v1/resources, /health, etc. |
| `environment` | Environment name | development, staging, production |
| `cluster` | Cluster identifier | prod-us-east-1, staging-eu-west-1 |
| `region` | Geographic region | us-east-1, eu-west-2, etc. |

### Component-Specific Labels

| Component | Labels |
|-----------|--------|
| **Core** | `operation`, `error_type` |
| **Transports** | `transport_type`, `connection_state` |
| **Sessions** | `session_type`, `session_status` |
| **Registry** | `resource_type`, `operation` |
| **Security** | `auth_type`, `permission_type` |

## Metric Export Formats

### Prometheus Format

```
# HELP erlmcp_requests_total Total number of requests
# TYPE erlmcp_requests_total counter
erlmcp_requests_total{method="GET",endpoint="/api/v1",status="200"} 12345
erlmcp_requests_total{method="POST",endpoint="/api/v1/resources",status="201"} 6789
```

### OpenTelemetry Format

```json
{
  "resource_metrics": [
    {
      "resource": {
        "attributes": [
          {
            "key": "service.name",
            "value": "erlmcp"
          }
        ]
      },
      "scope_metrics": [
        {
          "scope": {
            "name": "erlmcp",
            "version": "3.0.0"
          },
          "metrics": [
            {
              "name": "erlmcp_requests_total",
              "type": "COUNTER",
              "data": {
                "sum": {
                  "value": 12345
                }
              },
              "attributes": [
                {
                  "key": "method",
                  "value": "GET"
                }
              ]
            }
          ]
        }
      ]
    }
  ]
}
```

### JSON Format

```json
{
  "metrics": [
    {
      "name": "erlmcp_requests_total",
      "type": "counter",
      "value": 12345,
      "labels": {
        "method": "GET",
        "endpoint": "/api/v1",
        "status": "200"
      },
      "timestamp": 1634567890123
    }
  ]
}
```

## Metric Collection Intervals

| Metric Type | Interval | Reason |
|-------------|----------|--------|
| System Metrics | 15s | Real-time system monitoring |
| Request Metrics | 30s | Application performance |
| Business Metrics | 5m | Business activity tracking |
| SLA Metrics | 1m | SLA compliance monitoring |
| Custom Metrics | 1m | Flexible collection |

## Metric Retention

| Metric Type | Retention | Purpose |
|-------------|-----------|---------|
| System Metrics | 30 days | Historical analysis |
| Application Metrics | 7 days | Troubleshooting |
| Business Metrics | 90 days | Reporting |
| SLA Metrics | 365 days | Compliance |
| Custom Metrics | 30 days | Analysis |

## Metric Quality Standards

### Accuracy

- Metrics should be precise and accurate
- Use appropriate data types
- Validate metric values

### Completeness

- All important metrics should be collected
- No gaps in data collection
- Comprehensive coverage

### Consistency

- Metric names should follow conventions
- Labels should be consistent
- Units should be standardized

### Timeliness

- Metrics should be collected at appropriate intervals
- Data should be available for real-time monitoring
- Alerts should trigger based on current data

## Metric Troubleshooting

### Common Issues

#### Missing Metrics

1. Check if metric collection is enabled
2. Verify metric exporter is running
3. Check for permission issues

#### Incorrect Values

1. Validate metric calculation logic
2. Check label values
3. Verify data sources

#### High Cardinality

1. Reduce label dimensions
2. Use consistent naming
3. Implement sampling

### Debug Commands

```bash
# Check Prometheus metrics
curl -G "http://localhost:9090/api/v1/query" --data-urlencode 'query=up'

# List all metrics
curl -G "http://localhost:9090/api/v1/label/__name__/values"

# Get metric metadata
curl -G "http://localhost:9090/api/v1/metadata" --data-urlencode 'metric=erlmcp_requests_total'

# Check Grafana data sources
curl -H "Authorization: Bearer $GRAFANA_TOKEN" \
     "http://localhost:3000/api/datasources"
```

## Conclusion

This comprehensive metrics reference provides everything needed to effectively monitor and manage erlmcp v3 deployments at enterprise scale. Follow the guidelines and best practices to ensure optimal observability and system health.

Remember to:
- Monitor regularly
- Set appropriate alert thresholds
- Use metrics for capacity planning
- Continuously improve monitoring coverage