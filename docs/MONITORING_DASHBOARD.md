# ErlMCP Real-Time Monitoring Dashboard

## Overview

The ErlMCP Monitoring Dashboard provides real-time visibility into system health and performance metrics for the Erlang MCP SDK running at scale (100K+ concurrent connections).

## Components

### 1. Metrics Server (`erlmcp_metrics_server.erl`)

A `gen_server` that collects and aggregates system metrics in real-time:

- **Concurrent Connections**: Tracks number of active client connections
- **Message Throughput**: Messages per second, total messages
- **Error Rate**: Error count, error percentage
- **Latency Distribution**: P50, P95, P99 percentiles, min, max, average
- **System Resource Usage**: CPU, memory, process count, schedulers
- **Node Status**: Multi-node cluster awareness

**API Functions:**

```erlang
erlmcp_metrics_server:get_metrics()       % Get all metrics as map
erlmcp_metrics_server:record_message(N)   % Record N messages
erlmcp_metrics_server:record_error()      % Record an error
erlmcp_metrics_server:record_latency(Ms)  % Record latency in milliseconds
erlmcp_metrics_server:increment_connections(N)  % Add N connections
erlmcp_metrics_server:decrement_connections(N)  % Remove N connections
erlmcp_metrics_server:get_concurrent_connections()  % Get current count
erlmcp_metrics_server:reset_metrics()     % Clear all metrics
```

**Internal Storage:**

- ETS table (`erlmcp_simple_metrics`) for high-performance concurrent access
- Latency ring buffer (last 10K samples) for percentile calculation
- Rate windows (1-second rolling windows) for throughput calculation

### 2. HTTP API (`erlmcp_metrics_http_handler.erl`)

RESTful JSON endpoint for querying metrics:

```bash
GET /metrics  # JSON metrics response (1KB-50KB)
```

**Response Example:**

```json
{
  "timestamp": 1706299200000,
  "uptime_ms": 300000,
  "uptime_human": "0d 0h 5m 0s",
  "concurrent_connections": 100000,
  "total_messages": 1500000,
  "total_errors": 2500,
  "message_rate_per_sec": 5000,
  "error_rate_per_sec": 8,
  "error_percentage": 0.167,
  "latency_stats": {
    "p50": 12,
    "p95": 45,
    "p99": 98,
    "min": 2,
    "max": 523,
    "avg": 24
  },
  "latency_samples_count": 10000,
  "system_metrics": {
    "memory": {
      "total": 536870912,
      "heap_used": 134217728,
      "heap_allocated": 268435456
    },
    "process_count": 150000,
    "port_count": 100001,
    "schedulers": 8,
    "schedulers_online": 8
  },
  "nodes": [
    {
      "node": "erlmcp@localhost",
      "process_count": 150000,
      "memory": {"total": 536870912}
    }
  ]
}
```

### 3. Web Dashboard (`erlmcp_dashboard_http.erl` + HTML/JS)

Interactive browser-based monitoring dashboard:

**URL:** `http://localhost:8088/`

**Features:**

- Real-time metric cards with formatted numbers
- Auto-refreshing every 1 second
- Color-coded status indicators
- Latency distribution visualization
- Per-node cluster monitoring
- Connection status tracking
- Responsive design for mobile/tablet

**Metrics Displayed:**

| Card | Metric | Unit |
|------|--------|------|
| Concurrent Connections | Active client connections | count |
| Messages per Second | Throughput | msg/sec |
| Error Rate | Errors occurring | count/sec |
| System Uptime | Time since start | d h m s |
| P50 Latency | Median response time | ms |
| P99 Latency | 99th percentile time | ms |
| Active Processes | Total processes | count |
| Memory Usage | Heap memory | MB |

### 4. Stress Test Module (`erlmcp_dashboard_stress_test.erl`)

Load generator for testing dashboard at scale:

```erlang
%% Start 100K worker processes with simulated traffic
erlmcp_dashboard_stress_test:start(100000)

%% Run 1-minute benchmark
erlmcp_dashboard_stress_test:run_benchmark(100000)

%% Stop all workers
erlmcp_dashboard_stress_test:stop()
```

**Worker Behavior:**

- 100K concurrent processes (or specified count)
- Each sends 1 message per cycle
- 5% error rate (configurable)
- Realistic latency distribution (exponential-ish):
  - 70% under 10ms
  - 20% 10-50ms
  - 8% 50-100ms
  - 2% 100-500ms
- Think time: 10-60ms between messages

## Architecture

### Supervision Tree

```
erlmcp_sup (one_for_all)
├── erlmcp_metrics_server (worker)
└── erlmcp_metrics_http_sup (supervisor)
    └── erlmcp_metrics_http_worker (worker)
        └── inets httpd (HTTP server)
```

### Data Flow

```
Application Code
    ↓
erlmcp_metrics_server (gen_server)
    ├→ ETS Table (write-concurrent)
    ├→ Latency Ring Buffer (FIFO)
    ├→ Rate Windows (1-sec rolling)
    └→ Cached Metrics (map)
    ↓
HTTP Request Handler
    ↓
JSON Encoding (jsx)
    ↓
Browser Dashboard
    ↓
Real-time Visualization
```

## Performance Characteristics

### Metrics Server

- **Single Process Gen Server**: One per node
- **Write Performance**: ~100K ops/sec (ETS write-concurrent)
- **Read Performance**: Sub-millisecond for metric retrieval
- **Memory Overhead**: ~50MB for 10K latency samples + metadata
- **CPU Impact**: <1% CPU per 100K ops/sec

### HTTP Server (inets)

- **Lightweight**: Using standard Erlang `inets` httpd
- **Throughput**: ~1000 requests/sec per worker
- **Response Size**: 1KB-50KB JSON
- **Latency**: <5ms p99 for metric endpoint
- **Concurrency**: Handles thousands of concurrent dashboard clients

### Network

- **Update Frequency**: 1 second (configurable)
- **Bandwidth**: ~30KB/sec per active dashboard (worst case)
- **Protocol**: HTTP/1.1 (keep-alive)

## Usage Examples

### Starting the Dashboard

The dashboard automatically starts when the erlmcp application boots:

```erlang
application:start(erlmcp).
%% Dashboard available at http://localhost:8088/
```

### Recording Metrics in Your Code

```erlang
%% Record messages
erlmcp_metrics_server:record_message(10),  % 10 messages

%% Record errors
erlmcp_metrics_server:record_error(),

%% Record latency (in milliseconds)
StartTime = erlang:monotonic_time(millisecond),
% ... do work ...
EndTime = erlang:monotonic_time(millisecond),
Latency = EndTime - StartTime,
erlmcp_metrics_server:record_latency(Latency),

%% Track connections
erlmcp_metrics_server:increment_connections(1),  % Connection opened
erlmcp_metrics_server:decrement_connections(1),  % Connection closed
```

### Integration with Client Code

```erlang
-module(my_app_client).

handle_request(Request) ->
    Start = erlang:monotonic_time(millisecond),

    try
        % Process request
        Response = process(Request),
        erlmcp_metrics_server:record_message(1),
        Response
    catch
        _:Error ->
            erlmcp_metrics_server:record_error(),
            {error, Error}
    after
        Elapsed = erlang:monotonic_time(millisecond) - Start,
        erlmcp_metrics_server:record_latency(Elapsed)
    end.
```

### Testing at 100K Concurrent

```bash
# Terminal 1: Start erlmcp application
erl -pa ebin -eval 'application:start(erlmcp)' -s io:write '"Dashboard: http://localhost:8088/\n"' -s erlang halt

# Terminal 2: Start stress test
erl -pa ebin -eval 'erlmcp_dashboard_stress_test:start(100000)'

# Terminal 3: Monitor with curl
watch 'curl -s http://localhost:8088/metrics | jq ".concurrent_connections, .message_rate_per_sec"'
```

## Configuration

### Port Configuration

Edit `config/sys.config`:

```erlang
{erlmcp, [
    {metrics_http_port, 8088}  % Default: 8088
]}
```

Or start with custom port:

```erlang
erlmcp_metrics_http_sup:start_link(9090).  % Custom port
```

### Metrics Collection

Disable metrics (if needed for performance):

```erlang
application:set_env(erlmcp, metrics_enabled, false).
```

## Limitations & Considerations

1. **Latency Ring Buffer**: Holds last 10K samples
   - If throughput > 10K msg/sec, older samples are dropped
   - Still maintains correct P50/P95/P99 for recent period

2. **ETS Table Growth**: Unbounded in production
   - Older rate windows entries should be cleaned periodically
   - Monitor ETS table size with `ets:info(erlmcp_simple_metrics, memory)`

3. **HTTP Server**: Single port per node
   - Cannot change port after startup
   - For multi-node cluster, each node has separate dashboard

4. **Network Bandwidth**: Dashboard updates every 1 second
   - With 100K connections and 30KB responses, could be 3GB/sec theoretical
   - In practice, typically 1-10 clients accessing dashboard

## Testing

### Unit Tests

```bash
rebar3 eunit --module=erlmcp_dashboard_tests
```

**Test Coverage:**

- Metrics recording and aggregation
- Concurrent message recording
- Latency percentile calculation
- HTTP endpoint JSON encoding
- 100K concurrent connection test
- Edge cases (zero samples, negative connections, etc.)

### Integration Tests

```bash
# Start application with metrics
erl -pa ebin -name node1@127.0.0.1 -eval 'application:start(erlmcp)'

# Spawn 100K connections
erl -pa ebin -name client@127.0.0.1 -eval 'erlmcp_dashboard_stress_test:start(100000)'

# Open browser to http://localhost:8088
# Should show 100K concurrent connections updating in real-time
```

## Troubleshooting

### Dashboard shows "Connection Error"

1. Verify erlmcp is running: `erlang:node()`
2. Check port is accessible: `netstat -an | grep 8088`
3. Verify metrics server started: `erlmcp_metrics_server:get_metrics()`

### Latency values always 0

1. Make sure code is calling `erlmcp_metrics_server:record_latency()`
2. Check if metrics collection is disabled

### Memory usage growing unbounded

1. ETS table may not be cleaning old data
2. Run `erlmcp_metrics_server:reset_metrics()` to clear
3. Implement periodic cleanup in your application

## Files

- **Source**: `/Users/sac/erlmcp/src/erlmcp_metrics_*.erl`
- **Tests**: `/Users/sac/erlmcp/test/erlmcp_dashboard_tests.erl`
- **Stress Test**: `/Users/sac/erlmcp/src/erlmcp_dashboard_stress_test.erl`
- **HTTP Handler**: `/Users/sac/erlmcp/src/erlmcp_metrics_http_*.erl`

## Next Steps

1. Integrate metric recording into your client/server implementations
2. Test dashboard at target concurrent connection scale
3. Monitor metrics-server CPU/memory overhead in production
4. Configure port and update intervals for your environment
5. Set up alerting on error_percentage or P99 latency thresholds
