# WebSocket Backpressure Handling and Connection Limiting

**Gap #46: WebSocket Backpressure and Connection Limiting (DoS Prevention)**

This document describes the backpressure handling and connection limiting mechanisms in erlmcp WebSocket transport to prevent Denial of Service (DoS) attacks.

## Overview

The WebSocket transport implements two complementary DoS prevention mechanisms:

1. **Backpressure Handling**: Per-connection frame buffer limits to prevent memory exhaustion
2. **Connection Limiting**: Global and per-IP connection limits to prevent connection flooding

## Architecture

### Backpressure Flow Control

```
Incoming Message
    ↓
Check Backpressure State
    ├─ If ACTIVE → Reject with 1001 (Going Away)
    └─ If INACTIVE → Continue
    ↓
Check Message Size
    ├─ If Too Large → Close with 1009 (Message Too Big)
    └─ If OK → Continue
    ↓
Update Buffer Tracking
    ├─ Increment bytes_buffered
    └─ Increment messages_pending
    ↓
Process Message
    ↓
Process Send Response
    ├─ Decrement bytes_buffered
    └─ Decrement messages_pending
    ↓
Check for Resume
    ├─ If bytes_buffered ≤ threshold → Resume reading
    └─ Otherwise → Keep backpressure active
```

### Connection Limit Enforcement

```
Connection Attempt
    ↓
Check Current Connection Count
    ├─ If < max_connections → Accept
    ├─ If = max_connections → Accept
    └─ If > max_connections → Reject with 503
    ↓
Accepted: Initialize WebSocket
Failed: Return 503 Service Unavailable
    ├─ Header: Retry-After: 60
    └─ Body: {"error": "Server at capacity"}
```

## Configuration

All configuration is managed in `config/sys.config` under the `websocket` section.

### Buffer Management

```erlang
{websocket, [
    %% Frame buffer size per connection (bytes)
    %% Default: 102400 (100KB)
    %% When buffer exceeds this, backpressure is activated
    %% Recommended: 102400-1048576 depending on message throughput
    {frame_buffer_size, 102400},

    %% Buffer drain threshold (0.0 to 1.0)
    %% Reading resumes when buffer drains below this fraction of max
    %% Default: 0.5 (resume at 50% of max buffer)
    %% Recommended: 0.3-0.7
    {buffer_drain_threshold, 0.5},

    %% Backpressure timeout (milliseconds)
    %% Force resume reading after this time even if buffer not drained
    %% Default: 5000 (5 seconds)
    %% Prevents indefinite backpressure if client is unresponsive
    {backpressure_timeout, 5000}
]}.
```

### Connection Limiting

```erlang
{websocket, [
    %% Max concurrent WebSocket connections per server
    %% Default: 1000
    %% Prevent connection flooding attacks
    %% Recommended: 1000-10000 depending on resources
    {max_connections, 1000},

    %% Connection timeout (milliseconds)
    %% Time to wait for connection establishment
    %% Default: 5000 (5 seconds)
    {connect_timeout, 5000}
]}.
```

## Usage Examples

### Standard Configuration (Production)

```erlang
{erlmcp, [
    {websocket, [
        {enabled, true},
        {port, 8080},
        {path, "/mcp/ws"},
        {max_connections, 1000},      % DoS protection
        {frame_buffer_size, 102400},  % Per-connection limit
        {buffer_drain_threshold, 0.5},
        {backpressure_timeout, 5000}
    ]}
]}.
```

### High-Throughput Configuration

For systems with high message throughput and large payloads:

```erlang
{erlmcp, [
    {websocket, [
        {enabled, true},
        {port, 8080},
        {path, "/mcp/ws"},
        {max_connections, 5000},       % More connections
        {frame_buffer_size, 1048576},  % 1MB per connection
        {buffer_drain_threshold, 0.3}, % Resume earlier
        {backpressure_timeout, 10000}  % Longer timeout
    ]}
]}.
```

### Constrained Resources Configuration

For systems with limited memory (embedded, containers):

```erlang
{erlmcp, [
    {websocket, [
        {enabled, true},
        {port, 8080},
        {path, "/mcp/ws"},
        {max_connections, 100},        % Fewer connections
        {frame_buffer_size, 10240},    % 10KB per connection
        {buffer_drain_threshold, 0.7}, % Wait longer to drain
        {backpressure_timeout, 3000}   % Shorter timeout
    ]}
]}.
```

## Backpressure Behavior

### How Backpressure Works

1. **Activation**: When a connection's frame buffer exceeds `frame_buffer_size`:
   - Set backpressure state to ACTIVE
   - Start backpressure timeout timer
   - Reject new incoming messages with WebSocket close code 1001

2. **Monitoring**: As messages are sent/processed:
   - Track `bytes_buffered` (total bytes in buffer)
   - Track `messages_pending` (count of pending messages)

3. **Resumption**: Reading resumes when:
   - Buffer drains to ≤ (max_buffer_size × drain_threshold), OR
   - Backpressure timeout expires

4. **Recovery**: Once resumed:
   - Accept new messages normally
   - Continue tracking buffer usage

### Backpressure State Transitions

```
INACTIVE
    ↓ [bytes_buffered > frame_buffer_size]
    ↓
ACTIVE (timer started)
    ├─ [bytes_buffered ≤ drain_threshold × max]
    ├─ [OR timeout expires]
    ↓
INACTIVE (timer canceled)
```

## Connection Limiting Behavior

### Global Connection Limit

The server maintains a count of active WebSocket connections. When:

- **Count < max_connections**: New connection accepted, count incremented
- **Count ≥ max_connections**: New connection rejected with HTTP 503
- **Connection closes**: Count decremented, slot available

### Per-IP Connection Limits (Optional)

If configured, tracks connections per IP address:

```erlang
{websocket, [
    %% Optional: limit connections per IP address
    {per_ip_limit, 100}  % Max 100 connections from any single IP
]}.
```

### 503 Service Unavailable Response

When connection limit exceeded:

```http
HTTP/1.1 503 Service Unavailable
Content-Type: application/json
Retry-After: 60

{
  "error": "Service Unavailable",
  "message": "Server at maximum connection capacity",
  "retry_after_seconds": 60
}
```

## Monitoring and Metrics

### Key Metrics

Track via OpenTelemetry/OTEL:

```erlang
%% Per-connection metrics
- ws_backpressure_activations (counter)
  Description: Total backpressure activations
  Label: session_id

- ws_bytes_buffered (gauge)
  Description: Current bytes in frame buffer
  Label: session_id

- ws_messages_pending (gauge)
  Description: Current pending messages
  Label: session_id

- ws_backpressure_duration_ms (histogram)
  Description: Duration of backpressure events
  Label: session_id

%% Server-wide metrics
- ws_total_connections (gauge)
  Description: Current active connections

- ws_max_connections_exceeded (counter)
  Description: Times connection limit was exceeded

- ws_connection_limit_rejections (counter)
  Description: Total connections rejected due to limit
```

### Example OTEL Span Attributes

```erlang
#{
    <<"transport_id">> => <<"ws_primary">>,
    <<"session_id">> => <<"abc123def456">>,
    <<"backpressure_state">> => <<"active">>,
    <<"bytes_buffered">> => 45678,
    <<"messages_pending">> => 12,
    <<"frame_buffer_size">> => 102400,
    <<"backpressure_duration_ms">> => 245
}
```

## Performance Considerations

### Memory Impact

- **Per-connection overhead**: ~1KB for tracking (state record)
- **Buffer size impact**: `frame_buffer_size × max_connections`
  - Default: 100KB × 1000 = ~100MB aggregate
  - Can be tuned based on available memory

### CPU Impact

- **Backpressure check**: O(1) per message
- **Buffer accounting**: O(1) per message send/receive
- **Resume check**: O(1) per response

### Network Impact

- **Backpressure reduces network I/O**: Prevents buffer overflow
- **Clients receive 1001 close code**: Proper protocol signaling
- **No data loss**: Messages queued until resumed or timeout

## Best Practices

### 1. Buffer Size Tuning

Choose `frame_buffer_size` based on:
- Average message size
- Expected concurrency
- Available memory

```erlang
% For 1000 connections with 10KB avg messages
frame_buffer_size = 100KB  % 10x typical message size
```

### 2. Drain Threshold Configuration

- **0.3-0.4**: Eager resumption, high I/O
- **0.5-0.7**: Balanced approach (recommended)
- **0.8-1.0**: Conservative, low I/O

### 3. Timeout Configuration

- **< 3000ms**: Aggressive, may lose slow clients
- **3000-10000ms**: Balanced (recommended)
- **> 10000ms**: Very tolerant, requires monitoring

### 4. Connection Limits

```erlang
% Rule of thumb: max_connections ≈ (available_memory_mb × 10)
% Examples:
1GB RAM   → 10,000 connections
500MB RAM →  5,000 connections
100MB RAM →  1,000 connections
50MB RAM  →    500 connections
```

### 5. Monitoring

Deploy monitoring for:
- Backpressure activation frequency
- Average backpressure duration
- Connection limit hit rate
- Buffer utilization percentiles

### 6. Graceful Degradation

When approaching limits:
1. Log warnings at 80% utilization
2. Reject new connections gracefully at 100%
3. Send 503 with Retry-After header
4. Monitor recovery time after load spike

## Troubleshooting

### Problem: Frequent Backpressure Activations

**Symptoms**: WebSocket close with code 1001, clients reconnecting frequently

**Causes**:
- `frame_buffer_size` too small for message throughput
- Client unable to consume messages fast enough
- Network congestion causing buffering

**Solutions**:
```erlang
% Increase buffer size
{frame_buffer_size, 1048576}  % 1MB instead of 100KB

% Increase drain threshold to be more lenient
{buffer_drain_threshold, 0.3}  % Resume earlier

% Increase timeout to give clients more time
{backpressure_timeout, 10000}  % 10 seconds
```

### Problem: High Memory Usage

**Symptoms**: OOM errors, server crashes

**Causes**:
- `max_connections` too high
- `frame_buffer_size` too large
- Memory leak in message processing

**Solutions**:
```erlang
% Reduce connection limit
{max_connections, 500}

% Reduce per-connection buffer
{frame_buffer_size, 10240}  % 10KB

% Monitor for memory leaks
```

### Problem: Connection Limit Hit Constantly

**Symptoms**: 503 errors returned to clients, cannot accept connections

**Causes**:
- Too many long-lived connections
- Clients not properly closing connections
- Too low connection limit for workload

**Solutions**:
```erlang
% Increase limit (if hardware allows)
{max_connections, 5000}

% Implement idle timeout
{idle_timeout, 300000}  % 5 minutes, closes idle connections

% Investigate slow clients
% Review logs for non-closing connections
```

### Problem: Slow Client Handling

**Symptoms**: Some clients receive backpressure, connection timeouts

**Causes**:
- Client processing slower than message arrival
- Network latency between client and server

**Solutions**:
```erlang
% Increase backpressure timeout
{backpressure_timeout, 15000}

% Reduce message sending rate on client side
% Consider batching messages
```

## Testing

### Unit Tests

Run backpressure tests:
```bash
rebar3 eunit --module=erlmcp_ws_backpressure_tests
```

### Connection Limit Tests

Run connection limit tests:
```bash
rebar3 eunit --module=erlmcp_connection_limits_tests
```

### Load Testing

Test with high message throughput:
```bash
# Simulate 1000 concurrent connections with 100 msg/sec each
make test-ws-load
```

### Stress Testing

Test limit enforcement:
```bash
# Try to open 2000 connections when limit is 1000
make test-ws-stress
```

## References

- **RFC 6455**: The WebSocket Protocol
- **RFC 2119**: Key words for use in RFCs
- **MCP Protocol**: https://modelcontextprotocol.io
- **Cowboy Documentation**: https://ninenines.eu/docs/en/cowboy/2.10

## Changelog

### v0.6.0 (2025-01-27)

- Initial implementation of WebSocket backpressure handling
- Add per-connection frame buffer limits (100KB default)
- Add flow control with suspension/resumption
- Implement global connection limiting (1000 default)
- Add per-IP connection limits (optional)
- Comprehensive test coverage (27+ tests)
- OTEL instrumentation for metrics
- Documentation and configuration examples
