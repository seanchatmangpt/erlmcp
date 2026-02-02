# MCP Metrics Taxonomy Reference
**Version:** 1.0.0
**Date:** 2026-02-02
**Purpose:** Quick reference for MCP observability metrics

---

## Metric Naming Convention

**Format:** `erlmcp.mcp.<category>.<operation>.<metric_type>`

**Metric Types:**
- `latency_us` - Histogram (microseconds)
- `count` - Counter
- `active` - Gauge
- `errors` - Counter

---

## Resource Metrics (8)

| Metric | Type | Labels | Percentiles | Purpose |
|--------|------|--------|-------------|---------|
| `erlmcp.mcp.resources.list.latency_us` | Histogram | `server_id` | P50, P95, P99 | List operation latency |
| `erlmcp.mcp.resources.list.count` | Counter | `server_id` | - | Total list requests |
| `erlmcp.mcp.resources.read.latency_us` | Histogram | `server_id`, `uri`, `cache_hit` | P50, P95, P99 | Read latency |
| `erlmcp.mcp.resources.read.count` | Counter | `server_id`, `uri` | - | Reads per resource |
| `erlmcp.mcp.resources.subscribe.active` | Gauge | `server_id`, `uri` | - | Active subscriptions |
| `erlmcp.mcp.resources.notify.latency_us` | Histogram | `server_id`, `uri` | P50, P95, P99 | Notification latency |
| `erlmcp.mcp.resources.notify.fan_out` | Histogram | `server_id`, `uri` | P50, P95, P99 | Subscribers per update |
| `erlmcp.mcp.resources.template.match_time_us` | Histogram | `server_id`, `pattern` | P50, P95, P99 | URI template matching |

**SLOs:**
- `resources.list.latency_us`: P95 < 50ms
- `resources.read.latency_us`: P95 < 200ms
- `resources.notify.latency_us`: P95 < 50ms

---

## Tool Metrics (8)

| Metric | Type | Labels | Percentiles | Purpose |
|--------|------|--------|-------------|---------|
| `erlmcp.mcp.tools.list.latency_us` | Histogram | `server_id` | P50, P95, P99 | List tools latency |
| `erlmcp.mcp.tools.call.latency_us` | Histogram | `server_id`, `tool_name` | P50, P95, P99 | Total tool call |
| `erlmcp.mcp.tools.call.validation_time_us` | Histogram | `server_id`, `tool_name` | P50, P95, P99 | Schema validation |
| `erlmcp.mcp.tools.call.execution_time_us` | Histogram | `server_id`, `tool_name` | P50, P95, P99 | Handler execution |
| `erlmcp.mcp.tools.call.count` | Counter | `server_id`, `tool_name` | - | Tool invocations |
| `erlmcp.mcp.tools.call.errors` | Counter | `server_id`, `tool_name`, `error_type` | - | Tool failures |
| `erlmcp.mcp.tools.schema_cache.hits` | Counter | `server_id` | - | Cache hits |
| `erlmcp.mcp.tools.schema_cache.misses` | Counter | `server_id` | - | Cache misses |

**SLOs:**
- `tools.call.latency_us`: P95 < 500ms
- `tools.call.errors`: Rate < 5%

---

## Prompt Metrics (3)

| Metric | Type | Labels | Percentiles | Purpose |
|--------|------|--------|-------------|---------|
| `erlmcp.mcp.prompts.list.latency_us` | Histogram | `server_id` | P50, P95, P99 | List prompts |
| `erlmcp.mcp.prompts.get.latency_us` | Histogram | `server_id`, `prompt_name` | P50, P95, P99 | Get prompt |
| `erlmcp.mcp.prompts.get.count` | Counter | `server_id`, `prompt_name` | - | Prompt retrievals |

**SLOs:**
- `prompts.get.latency_us`: P95 < 100ms

---

## Session Metrics (4)

| Metric | Type | Labels | Percentiles | Purpose |
|--------|------|--------|-------------|---------|
| `erlmcp.mcp.session.active` | Gauge | `server_id` | - | Active sessions |
| `erlmcp.mcp.session.duration_s` | Histogram | `server_id`, `transport_type` | P50, P95, P99 | Session lifetime |
| `erlmcp.mcp.session.init_latency_us` | Histogram | `server_id` | P50, P95, P99 | Initialize time |
| `erlmcp.mcp.session.phase` | Gauge | `server_id`, `phase` | - | Sessions by phase |

**SLOs:**
- `session.init_latency_us`: P95 < 100ms

---

## Transport Metrics (5)

| Metric | Type | Labels | Percentiles | Purpose |
|--------|------|--------|-------------|---------|
| `erlmcp.mcp.transport.send.latency_us` | Histogram | `server_id`, `transport_type` | P50, P95, P99 | Send latency |
| `erlmcp.mcp.transport.receive.latency_us` | Histogram | `server_id`, `transport_type` | P50, P95, P99 | Receive latency |
| `erlmcp.mcp.transport.message_size_bytes` | Histogram | `server_id`, `transport_type`, `direction` | P50, P95, P99 | Message size |
| `erlmcp.mcp.jsonrpc.encode_latency_us` | Histogram | `server_id`, `method` | P50, P95, P99 | Encoding time |
| `erlmcp.mcp.jsonrpc.decode_latency_us` | Histogram | `server_id`, `method` | P50, P95, P99 | Decoding time |

---

## Derived Metrics (Calculated)

| Metric | Formula | Purpose |
|--------|---------|---------|
| `erlmcp.mcp.tools.call.success_rate` | `(count - errors) / count` | Reliability |
| `erlmcp.mcp.resources.cache_hit_ratio` | `cache_hits / (hits + misses)` | Cache efficiency |
| `erlmcp.mcp.session.churn_rate` | `new_sessions / time_window` | Stability |
| `erlmcp.mcp.transport.throughput_ops` | `operations / time_window` | Network load |

---

## Instrumentation API

```erlang
%% Record resource list operation
erlmcp_mcp_metrics:record_resource_list(ServerId, LatencyUs).

%% Record resource read operation
erlmcp_mcp_metrics:record_resource_read(ServerId, Uri, LatencyUs, CacheHit).

%% Record tool call operation
erlmcp_mcp_metrics:record_tool_call(ServerId, ToolName, ValidationUs, ExecutionUs).

%% Record subscription update
erlmcp_mcp_metrics:record_subscription_update(ServerId, Uri, SubscriberCount).

%% Record session initialization
erlmcp_mcp_metrics:record_session_init(ServerId, LatencyUs).

%% Record transport operation
erlmcp_mcp_metrics:record_transport_send(ServerId, TransportType, LatencyUs).
```

---

## Query Examples (Prometheus)

### Resource List P95 Latency
```promql
histogram_quantile(0.95,
  rate(erlmcp_mcp_resources_list_latency_us_bucket[5m])
)
```

### Tool Call Success Rate
```promql
(
  rate(erlmcp_mcp_tools_call_count[5m]) -
  rate(erlmcp_mcp_tools_call_errors[5m])
) / rate(erlmcp_mcp_tools_call_count[5m])
```

### Cache Hit Ratio
```promql
rate(erlmcp_mcp_tools_schema_cache_hits[5m]) /
(
  rate(erlmcp_mcp_tools_schema_cache_hits[5m]) +
  rate(erlmcp_mcp_tools_schema_cache_misses[5m])
)
```

---

## Alert Thresholds

| Alert | Metric | Threshold | Duration | Severity |
|-------|--------|-----------|----------|----------|
| High List Latency | `resources.list.latency_us` | P95 > 50ms | 5 min | Warning |
| High Read Latency | `resources.read.latency_us` | P95 > 200ms | 5 min | Warning |
| High Tool Latency | `tools.call.latency_us` | P95 > 500ms | 5 min | Critical |
| Tool Error Rate | `tools.call.errors` | Rate > 5% | 5 min | Critical |
| Init Failure | `session.init.errors` | Rate > 10% | 2 min | Critical |

---

**Total Metrics:** 28 base + 4 derived = 32 MCP-specific metrics
