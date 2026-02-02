# Transport Health Checks for erlmcp

## Overview

The erlmcp transport health monitoring system provides comprehensive real-time health checks for all transport types (STDIO, TCP, HTTP, WebSocket, SSE) with OpenTelemetry integration and a visual dashboard.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                erlmcp_transport_health                       │
│              (gen_server Health Monitor)                      │
│  - Periodic health checks (default: 30s)                     │
│  - Caches health results (TTL: 5s)                           │
│  - Tracks metrics history (100 samples)                      │
└────────┬────────────────────────────────────────────────────┘
         │
         ├─────────────────────────────────────────────────┐
         │                                                 │
         ▼                                                 ▼
┌─────────────────────┐                         ┌──────────────────────┐
│  Per-Transport      │                         │  erlmcp_transport    │
│  Health Checks      │                         │  telemetry (OTEL)    │
│                     │                         │                      │
│ • STDIO: Process    │                         │ • Metrics recording  │
│   alive, queue      │                         │ • Histograms         │
│   depth             │                         │ • Counters           │
│                     │                         │ • Gauges             │
│ • TCP: Connection   │                         │                      │
│   count, SSL        │                         └──────────────────────┘
│   status            │
│                     │                         ┌──────────────────────┐
│ • HTTP: Gun client  │                         │  erlmcp_health       │
│   status, req queue │                         │  dashboard           │
│                     │                         │                      │
│ • WebSocket: Cowboy │                         │ • HTTP: /health      │
│   connections,      │                         │   /health/:transport │
│   ping/pong         │                         │ • WebSocket: /health  │
│                     │                         │   /stream            │
│ • SSE: Active       │                         │ • HTML: /health/     │
│   streams, reconnect│                         │   dashboard          │
│   count             │                         │                      │
└─────────────────────┘                         └──────────────────────┘
```

## Health Check API

### Check Single Transport

```erlang
%% Check health of a specific transport
{ok, Status} = erlmcp_transport_health:check_health(my_tcp_transport).

%% Status values: healthy | degraded | unhealthy | unknown
```

### Get Overall Health

```erlang
%% Aggregate health from all transports
Overall = erlmcp_transport_health:overall_health().

%% Returns:
%% #{
%%   overall_status => healthy | degraded | unhealthy | unknown,
%%   total_transports => N,
%%   healthy_count => N,
%%   degraded_count => N,
%%   unhealthy_count => N,
%%   timestamp => Millis,
%%   transports => #{
%%     transport_id => #{
%%       status => healthy,
%%       last_check => Millis
%%     }
%%   }
%% }.
```

### Get Detailed Health Status

```erlang
%% Get detailed health information for a transport
{ok, Health} = erlmcp_transport_health:get_health_status(my_transport).

%% Returns:
%% #{
%%   transport_id => my_transport,
%%   status => healthy,
%%   metrics => #{
%%     timestamp => Millis,
%%     connection_status => up | down | unknown,
%%     latency_ms => 12.5,
%%     error_rate => 0.01,  % 1%
%%     throughput => 1000,  % msg/sec
%%     last_error => undefined
%%   },
%%   last_check => Millis,
%%   consecutive_failures => 0,
%%   last_healthy => Millis
%% }.
```

### Register/Unregister Transport

```erlang
%% Register transport for monitoring
ok = erlmcp_transport_health:register_transport(
    my_tcp_transport,
    TransportPid,
    #{type => tcp, host => "localhost", port => 9999}
).

%% Unregister transport
ok = erlmcp_transport_health:unregister_transport(my_tcp_transport).
```

### Update Metrics

```erlang
%% Update specific metric
ok = erlmcp_transport_health:update_metrics(
    my_transport,
    error_rate,
    0.05  % 5% error rate
).

%% Reset all metrics to defaults
ok = erlmcp_transport_health:reset_metrics(my_transport).
```

### Threshold Configuration

```erlang
%% Set health check threshold
ok = erlmcp_transport_health:set_threshold(
    my_transport,
    max_latency,
    100  % 100ms threshold
).
```

### Health History

```erlang
%% Get health check history
{ok, History} = erlmcp_transport_health:get_health_history(my_transport).

%% Returns list of metrics:
%% [
%%   #{timestamp => ..., latency_ms => ..., error_rate => ...},
%%   #{timestamp => ..., latency_ms => ..., error_rate => ...},
%%   ...
%% ]
```

## Per-Transport Metrics

### STDIO Transport

```erlang
%% Health indicators:
- Process alive (is_process_alive/1)
- Message queue depth (process_info(queue_len))
- Buffer status (standard_io availability)
- Encoding errors (UTF-8 validation)
```

### TCP Transport

```erlang
%% Health indicators:
- Active connection count
- SSL/TLS certificate validity
- Socket buffer stats (recv/send via inet:getopts/2)
- Connection error rate
- Listener socket status

%% Example health report:
#{
  status => healthy,
  metrics => #{
    active_connections => 42,
    ssl_enabled => true,
    ssl_cert_valid => true,
    recv_buffer => 65536,
    send_buffer => 65536,
    error_rate => 0.001
  }
}
```

### HTTP Transport

```erlang
%% Health indicators:
- Gun connection pool status
- Active request count
- Response time (p50, p95, p99)
- HTTP error rate (4xx, 5xx)
- Keep-alive connection health

%% Example health report:
#{
  status => degraded,
  metrics => #{
    pool_size => 10,
    active_requests => 8,
    response_time_p50 => 10.2,
    response_time_p95 => 45.8,
    response_time_p99 => 120.5,
    error_4xx_rate => 0.02,
    error_5xx_rate => 0.01
  }
}
```

### WebSocket Transport

```erlang
%% Health indicators:
- Cowboy connection count
- WebSocket frame statistics
- Ping/pong round-trip time
- Message fragmentation rate
- Connection upgrade success rate

%% Example health report:
#{
  status => healthy,
  metrics => #{
    active_connections => 15,
    frames_sent => 1000,
    frames_received => 950,
    ping_rtt_avg => 5.2,
    fragmentation_rate => 0.001,
    upgrade_success_rate => 0.98
  }
}
```

### SSE Transport

```erlang
%% Health indicators:
- Active stream count
- Event queue depth
- Client reconnection rate
- Last-event-id gaps
- Keep-alive delivery success

%% Example health report:
#{
  status => healthy,
  metrics => #{
    active_streams => 25,
    event_queue_depth => 100,
    reconnection_rate => 0.01,
    last_event_id_gaps => 0,
    keepalive_success_rate => 0.99
  }
}
```

## OpenTelemetry Integration

### Initialization

```erlang
%% Initialize transport telemetry
ok = erlmcp_transport_telemetry:init().
```

### Recording Metrics

```erlang
%% Record transport metrics
ok = erlmcp_transport_telemetry:record_transport_metrics(
    tcp,
    messages_sent,
    1000
).

%% Record health check result
ok = erlmcp_transport_telemetry:record_health_check(
    tcp,
    <<"tcp_transport_1">>,
    healthy,
    5.5  % latency in ms
).

%% Record connection event
ok = erlmcp_transport_telemetry:record_connection_event(
    tcp,
    <<"tcp_transport_1">>,
    connected
).

%% Record error
ok = erlmcp_transport_telemetry:record_error(
    tcp,
    <<"tcp_transport_1">>,
    econnrefused
).

%% Record latency
ok = erlmcp_transport_telemetry:record_latency(
    tcp,
    <<"tcp_transport_1">>,
    12.5
).
```

### OTEL Metrics

The following metrics are exported to OpenTelemetry:

- `erlmcp.transport.uptime` (gauge) - Transport uptime in seconds
- `erlmcp.transport.throughput` (counter) - Messages per second
- `erlmcp.transport.error_rate` (gauge) - Error rate percentage
- `erlmcp.transport.latency` (histogram) - Operation latency in ms
- `erlmcp.transport.connections` (counter) - Active connections

**Attributes**:
- `transport_type`: stdio | tcp | http | websocket | sse
- `transport_id`: Transport identifier
- `node`: Erlang node name

## Health Dashboard

### Starting the Dashboard

```erlang
%% Start with default port (9091)
{ok, Pid} = erlmcp_health_dashboard:start_link().

%% Start with custom port
{ok, Pid} = erlmcp_health_dashboard:start_link(9192).
```

### HTTP Endpoints

#### Overall Health
```bash
GET /health

Response:
{
  "overall_status": "healthy",
  "total_transports": 5,
  "healthy_count": 4,
  "degraded_count": 1,
  "unhealthy_count": 0,
  "timestamp": 1704067200000,
  "transports": {
    "stdio_transport": {
      "status": "healthy",
      "last_check": 1704067200000
    },
    "tcp_transport": {
      "status": "degraded",
      "last_check": 1704067200000
    }
  }
}
```

#### Per-Transport Health
```bash
GET /health/:transport

Example:
GET /health/tcp_transport

Response:
{
  "transport_id": "tcp_transport",
  "status": "degraded",
  "metrics": {
    "latency_ms": 125.5,
    "error_rate": 0.05
  }
}
```

#### Prometheus Metrics
```bash
GET /health/metrics

Response:
# HELP erlmcp_transport_health_status Overall transport health status
# TYPE erlmcp_transport_health_status gauge
erlmcp_transport_health_status 3

# HELP erlmcp_transport_healthy_count Number of healthy transports
# TYPE erlmcp_transport_healthy_count gauge
erlmcp_transport_healthy_count 4

# HELP erlmcp_transport_degraded_count Number of degraded transports
# TYPE erlmcp_transport_degraded_count gauge
erlmcp_transport_degraded_count 1
```

#### Visual Dashboard
```bash
GET /health/dashboard

Returns: HTML dashboard with real-time health visualization
```

### WebSocket Streaming

```javascript
// Connect to health update stream
const ws = new WebSocket('ws://localhost:9091/health/stream');

// Receive real-time health updates
ws.onmessage = (event) => {
  const data = JSON.parse(event.data);
  if (data.type === 'health_update') {
    updateDashboard(data.data);
  }
};

// Health update format:
{
  "type": "health_update",
  "data": {
    "overall_status": "healthy",
    "healthy_count": 4,
    "degraded_count": 1,
    "transports": { ... }
  }
}
```

## Supervision Tree

The health monitor and dashboard are integrated into the observability supervision tree:

```erlang
%% erlmcp_observability_sup
{ok, {
    {one_for_one, 10, 10},
    [
        %% Health monitor (permanent - critical)
        {erlmcp_transport_health,
            {erlmcp_transport_health, start_link, []},
            permanent, 5000, worker, [erlmcp_transport_health]},

        %% Health dashboard (transient - can restart)
        {erlmcp_health_dashboard,
            {erlmcp_health_dashboard, start_link, []},
            transient, 5000, worker, [erlmcp_health_dashboard]},

        %% Transport telemetry (permanent)
        {erlmcp_transport_telemetry,
            {erlmcp_transport_telemetry, start_link, []},
            permanent, 5000, worker, [erlmcp_transport_telemetry]}
    ]
}}.
```

## Configuration

### Application Environment

```erlang
%% sys.config
{erlmcp_observability, [
    {health_dashboard_port, 9091},
    {health_check_interval, 30000},  % 30 seconds
    {metrics_retention_period, 300000},  % 5 minutes
    {max_metrics_history, 100}
]}.

%% Transport-specific health thresholds
{erlmcp_transports, [
    {max_latency_ms, 100},
    {max_error_rate, 0.05},  % 5%
    {min_throughput, 100}  % msg/sec
]}.
```

## Alert Thresholds

### Status Determination

```erlang
%% Healthy: All metrics within thresholds
- Latency < max_latency_ms
- Error rate < max_error_rate
- Throughput >= min_throughput
- Connection status = up

%% Degraded: Some metrics degraded but operational
- Latency < max_latency_ms * 2
- Error rate < max_error_rate * 2
- Connection status = up

%% Unhealthy: Critical metrics failing
- Latency >= max_latency_ms * 2 OR
- Error rate >= max_error_rate * 2 OR
- Connection status = down
```

### Custom Thresholds

```erlang
%% Set custom threshold per transport
ok = erlmcp_transport_health:set_threshold(
    my_tcp_transport,
    max_latency,
    200  % 200ms threshold for this transport
).
```

## Troubleshooting

### Common Issues

#### Health Monitor Not Starting

```erlang
%% Check if gproc is running
application:which_applications().
%% Should include gproc
```

#### Dashboard Not Accessible

```bash
# Check if port is in use
netstat -an | grep 9091

# Check Cowboy listener status
 cowboy:listeners().
```

#### OTEL Metrics Not Recording

```erlang
%% Verify OpenTelemetry is started
application:which_applications().
%% Should include opentelemetry

%% Check OTEL configuration
application:get_all_env(opentelemetry).
```

### Debugging

```erlang
%% Enable debug logging
logger:set_application_level(erlmcp_transport_health, debug).

%% Check health monitor state
sys:get_status(erlmcp_transport_health).

%% View registered transports
erlmcp_transport_health:overall_health().
```

## Performance

### Overhead

- **Memory**: ~1KB per transport (health cache + metrics)
- **CPU**: ~0.1ms per health check (cached)
- **Network**: Dashboard WebSocket ~100KB/hour (1 client)

### Optimization

```erlang
%% Reduce check interval for production
{health_check_interval, 60000}.  % 60 seconds

%% Limit metrics history
{max_metrics_history, 50}.  % 50 samples (from 100)

%% Disable dashboard if not needed
%% Don't start erlmcp_health_dashboard
```

## Examples

### Basic Usage

```erlang
%% 1. Start health monitor
{ok, _Pid} = erlmcp_transport_health:start_link(#{}).

%% 2. Register transports
ok = erlmcp_transport_health:register_transport(
    my_stdio,
    StdioPid,
    #{type => stdio}
).

%% 3. Check health
{ok, healthy} = erlmcp_transport_health:check_health(my_stdio).

%% 4. Get overall status
Overall = erlmcp_transport_health:overall_health().
```

### With Dashboard

```erlang
%% 1. Start health monitor and dashboard
{ok, _HealthPid} = erlmcp_transport_health:start_link(#{}),
{ok, _DashboardPid} = erlmcp_health_dashboard:start_link().

%% 2. Access dashboard
%% Open: http://localhost:9091/health/dashboard

%% 3. Subscribe to real-time updates (WebSocket)
%% JavaScript:
%% const ws = new WebSocket('ws://localhost:9091/health/stream');
```

### With OTEL

```erlang
%% 1. Initialize telemetry
ok = erlmcp_transport_telemetry:init().

%% 2. Record metrics during operation
ok = erlmcp_transport_telemetry:record_health_check(
    tcp,
    <<"tcp_1">>,
    healthy,
    5.5
).

%% 3. Metrics are exported to OpenTelemetry
%% View in Jaeger/Prometheus/Zipkin
```

## See Also

- [OTP Patterns](otp-patterns.md) - gen_server and supervision patterns
- [Observability](../erlmcp_observability/) - OTEL and monitoring
- [Transport Implementation](../erlmcp_transports/) - Transport layer details
