# Connection State Machine - Transport Layer

## Overview

The connection state machine (`erlmcp_connection_fsm`) is a production-ready `gen_statem` implementation managing the complete lifecycle of transport connections in erlmcp. It provides robust connection management with circuit breaker pattern, automatic reconnection, flow control, health monitoring, and connection pooling.

## Architecture

### State Diagram

```
    disconnected
          |
          | {connect, Opts}
          v
    connecting
          |
          | timeout/error         | success
          v                        v
    reconnecting              connected
          |                        |
          | max_attempts           | {ready}
          v                        v
    failed                    ready
                               |
                               | {disconnect}
                               v
                        disconnecting
                               |
                               | cleanup_complete
                               v
                           closed
```

### State Descriptions

| State | Description | Valid Events |
|-------|-------------|--------------|
| **disconnected** | Initial state, not connected | `{connect, Opts}` |
| **connecting** | Actively attempting to establish connection | `timeout`, `{connected, Socket}`, `{error, Reason}` |
| **connected** | Connection established, not ready for data | `ready`, `{data, Data}`, `{error, Reason}` |
| **ready** | Connection ready for data transfer | `{send, Data}`, `{keepalive, timeout}`, `{health_check, Result}` |
| **reconnecting** | Attempting to reconnect after failure | `reconnect`, `{circuit_breaker, closed}` |
| **disconnecting** | Gracefully shutting down | `disconnect_complete` |
| **failed** | Connection failed, will not retry | `reset_circuit_breaker` |
| **closed** | Connection closed permanently | `{connect, Opts}` |

## Features

### 1. Circuit Breaker Pattern

Prevents cascading failures by temporarily rejecting connection attempts after repeated failures.

**States**:
- **closed**: Normal operation, requests allowed
- **open**: Failing, requests rejected
- **half_open**: Testing if service has recovered

**Configuration**:
```erlang
#circuit_breaker{
    state = closed,
    threshold = 5,              %% Failures before opening
    timeout = 60000,            %% ms to stay open
    half_open_max_calls = 3     %% Test calls in half-open
}
```

**Usage**:
```erlang
%% Check circuit breaker state
{ok, healthy, Info} = erlmcp_connection_fsm:get_health(Pid),
#{circuit_breaker := #{state := CBState}} = Info,

%% Reset circuit breaker
ok = erlmcp_connection_fsm:reset_circuit_breaker(Pid)
```

### 2. Automatic Reconnection

Exponential backoff with jitter for reconnection attempts.

**Backoff Strategies**:
- `exponential`: `delay = initial * multiplier^attempts`
- `linear`: `delay = initial * (attempts + 1)`
- `fixed`: `delay = initial`
- `jitter`: Random delay within range
- `decorrelated_jitter`: Correlated jitter for better distribution

**Configuration**:
```erlang
#backoff_config{
    strategy = exponential,
    initial_delay = 1000,       %% 1 second
    max_delay = 60000,          %% 60 seconds
    multiplier = 2.0,
    jitter = 0.1,               %% 10% jitter
    max_attempts = infinity     %% or specific number
}
```

### 3. Flow Control & Backpressure

Window-based flow control to prevent overwhelming connections.

**States**:
- `normal`: Normal operation
- `throttled`: Approaching limit
- `backpressure`: At high watermark, slow down
- `paused`: At pause threshold, stop sending

**Configuration**:
```erlang
#flow_control{
    window_size = 65536,        %% bytes
    high_watermark = 0.8,       %% 80%
    low_watermark = 0.5,        %% 50%
    pause_threshold = 0.9,      %% 90%
    resume_threshold = 0.4      %% 40%
}
```

**Usage**:
```erlang
%% Send data (handles backpressure automatically)
case erlmcp_connection_fsm:send(Pid, Data) of
    ok -> ok;
    {error, backpressure} -> handle_backpressure();
    {error, buffer_full} -> handle_buffer_full()
end
```

### 4. Health Monitoring

Periodic health checks with consecutive failure/success tracking.

**Configuration**:
```erlang
#health_config{
    enabled = true,
    interval = 30000,           %% 30 seconds
    timeout = 5000,             %% 5 seconds
    failure_threshold = 3,      %% Failures before unhealthy
    success_threshold = 2       %% Successes before healthy
}
```

**Usage**:
```erlang
{ok, Status, Info} = erlmcp_connection_fsm:get_health(Pid),
%% Status: healthy | unhealthy | unknown
%% Info includes consecutive_failures, consecutive_successes, circuit_breaker
```

### 5. Keep-Alive

Configurable keep-alive with missed keep-alive detection.

**Configuration**:
```erlang
#keepalive_config{
    enabled = true,
    interval = 30000,           %% 30 seconds
    timeout = 10000,            %% 10 seconds
    max_missed = 3              %% Trigger reconnect after 3 misses
}
```

### 6. Connection Pooling

Integration with poolboy for efficient connection management.

**Configuration**:
```erlang
#pool_config{
    size = 10,                  %% Pool size
    max_overflow = 5,           %% Overflow connections
    strategy = round_robin,     %% or least_loaded, random
    lease_timeout = 30000,      %% 30 seconds
    idle_timeout = 60000        %% 60 seconds
}
```

**Usage**:
```erlang
%% Start pool
Config = #pool_config{
    name = my_pool,
    transport_type = tcp,
    host = "localhost",
    port = 9999,
    size = 10,
    max_overflow = 5
},
{ok, PoolPid} = erlmcp_connection_pool:start_link(Config),

%% Checkout connection
{ok, ConnPid, MonRef} = erlmcp_connection_pool:checkout(my_pool, []),

%% Use connection
erlmcp_connection_fsm:send(ConnPid, Data),

%% Checkin connection
ok = erlmcp_connection_pool:checkin(my_pool, ConnPid),

%% Or use with_connection (automatic checkout/checkin)
{ok, Result} = erlmcp_connection_pool:with_connection(
    my_pool,
    [],
    fun(ConnPid) ->
        erlmcp_connection_fsm:send(ConnPid, Data)
    end
)
```

### 7. Metrics Collection

Comprehensive connection metrics for monitoring.

**Metrics**:
```erlang
#conn_metrics{
    connect_count = 0,          %% Total connections
    disconnect_count = 0,       %% Total disconnections
    bytes_sent = 0,             %% Bytes sent
    bytes_received = 0,         %% Bytes received
    messages_sent = 0,          %% Messages sent
    messages_received = 0,      %% Messages received
    connection_errors = 0,      %% Connection errors
    last_connect_time,          %% Unix timestamp
    last_disconnect_time,       %% Unix timestamp
    uptime_ms = 0,              %% Connection uptime
    latency_ms = 0              %% Connection latency
}
```

**Usage**:
```erlang
{ok, Metrics} = erlmcp_connection_fsm:get_metrics(Pid),
%% Access metrics: Metrics#conn_metrics.bytes_sent
```

### 8. Multiplexing Support

Parent-child connection relationships for multiplexed protocols (HTTP/2).

**Configuration**:
```erlang
ConnOpts = #{
    multiplexed = true,
    multiplex_id = make_ref(),
    parent_connection = ParentPid
}
```

## Transport Types

| Transport | Description | Use Case |
|-----------|-------------|----------|
| **stdio** | Standard input/output | Local MCP communication |
| **tcp** | TCP socket | Direct TCP connections |
| **http** | HTTP/1.1 | HTTP-based transport |
| **http2** | HTTP/2 with multiplexing | High-performance HTTP |
| **ws** | WebSocket | Full-duplex messaging |
| **sse** | Server-Sent Events | Server-to-client streaming |
| **tls** | TLS over TCP | Secure connections |

## API Reference

### Starting Connection FSM

```erlang
%% Start with name
{ok, Pid} = erlmcp_connection_fsm:start_link(
    my_connection,
    tcp,
    "localhost",
    9999
)
```

### Connection Management

```erlang
%% Connect
ok = erlmcp_connection_fsm:connect(Pid, #{timeout => 5000}),

%% Disconnect
ok = erlmcp_connection_fsm:disconnect(Pid),

%% Send data
ok = erlmcp_connection_fsm:send(Pid, <<"data">>),

%% Get state
{ok, State, Info} = erlmcp_connection_fsm:get_state(Pid),

%% Get metrics
{ok, Metrics} = erlmcp_connection_fsm:get_metrics(Pid),

%% Get health
{ok, Health, Info} = erlmcp_connection_fsm:get_health(Pid)
```

### Configuration

```erlang
%% Update configuration
ok = erlmcp_connection_fsm:update_config(Pid, #{
    backoff_config => #backoff_config{strategy = linear},
    circuit_breaker => #circuit_breaker{threshold = 10},
    flow_control => #flow_control{window_size = 131072}
})
```

### Circuit Breaker

```erlang
%% Reset circuit breaker
ok = erlmcp_connection_fsm:reset_circuit_breaker(Pid)
```

### Connection Leasing

```erlang
%% Lease connection
{ok, LeaseRef} = erlmcp_connection_fsm:lease_connection(Pid, 30000),

%% Release connection
ok = erlmcp_connection_fsm:release_connection(Pid)
```

## Error Handling

### Connection Errors

Connection errors trigger automatic reconnection with exponential backoff:

```erlang
%% These errors trigger reconnection:
%% - econnrefused
%% - econnreset
%% - timeout
%% - {tcp_closed, Socket}
%% - {tcp_error, Socket, Reason}
```

### Circuit Breaker

After threshold failures, circuit opens:

```erlang
%% 1. Failures accumulate
%% 2. Circuit opens at threshold
%% 3. Requests rejected for timeout period
%% 4. Half-open state tests recovery
%% 5. Success closes circuit, failure reopens
```

### Max Attempts

Exceeded max attempts moves state to `failed`:

```erlang
%% 1. Reconnect attempts accumulate
%% 2. At max_attempts, state -> failed
%% 3. Manual reset required
ok = erlmcp_connection_fsm:reset_circuit_breaker(Pid)
```

## Best Practices

### 1. Connection Pooling

Always use connection pools for production:

```erlang
%% Good: Use pool
{ok, Result} = erlmcp_connection_pool:with_connection(
    my_pool,
    [],
    fun(ConnPid) -> use_connection(ConnPid) end
),

%% Bad: Single connection
{ok, Pid} = erlmcp_connection_fsm:start_link(...),
%% Single point of failure
```

### 2. Circuit Breaker Thresholds

Set thresholds based on traffic:

```erlang
%% High traffic: Lower threshold
#circuit_breaker{threshold = 3, timeout = 30000},

%% Low traffic: Higher threshold
#circuit_breaker{threshold = 10, timeout = 60000}
```

### 3. Flow Control Windows

Match window size to transport:

```erlang
%% TCP: 64KB default
#flow_control{window_size = 65536},

%% HTTP/2: Larger window
#flow_control{window_size = 65536 * 4}
```

### 4. Backoff Configuration

Use exponential backoff with jitter:

```erlang
#backoff_config{
    strategy = exponential,
    initial_delay = 1000,
    max_delay = 60000,
    multiplier = 2.0,
    jitter = 0.1
}
```

### 5. Health Monitoring

Monitor health regularly:

```erlang
#health_config{
    enabled = true,
    interval = 30000,
    failure_threshold = 3
}
```

### 6. Metrics Collection

Track metrics for observability:

```erlang
{ok, Metrics} = erlmcp_connection_fsm:get_metrics(Pid),
%% Emit to OTEL/metrics system
erlmcp_otel:record_metrics(connection, Metrics)
```

## Integration with OTEL

```erlang
%% Trace connection lifecycle
{ok, SpanCtx} = erlmcp_otel:start_span(<<"connection.connect">>, #{
    <<"transport.type">> => <<"tcp">>,
    <<"host">> => <<"localhost">>,
    <<"port">> => 9999
}),

try
    ok = erlmcp_connection_fsm:connect(Pid, #{}),
    erlmcp_otel:end_span(SpanCtx)
catch
    _:Error ->
        erlmcp_otel:record_error(SpanCtx, Error),
        erlmcp_otel:end_span(SpanCtx)
end
```

## Supervision

The connection FSM should be supervised by `erlmcp_transport_sup`:

```erlang
%% In supervisor
ChildSpec = #{
    id => connection_fsm,
    start => {erlmcp_connection_fsm, start_link, [Name, Type, Host, Port]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [erlmcp_connection_fsm]
}
```

## Testing

See `erlmcp_connection_fsm_tests.erl` for comprehensive tests:

```bash
%% Run tests
rebar3 eunit --module=erlmcp_connection_fsm_tests

%% Run with coverage
rebar3 cover --verbose
```

## Performance Considerations

1. **Connection Pool Size**: Match to expected load
   - Low load: 5-10 connections
   - Medium load: 10-50 connections
   - High load: 50-200 connections

2. **Backoff Delays**: Prevent thundering herd
   - Use jitter: 10-20%
   - Exponential: 2x multiplier
   - Max delay: 30-60 seconds

3. **Flow Control**: Prevent buffer bloat
   - Window: 64KB-256KB
   - High watermark: 80-90%
   - Low watermark: 40-50%

4. **Health Checks**: Balance overhead and responsiveness
   - Interval: 30-60 seconds
   - Timeout: 5-10 seconds
   - Threshold: 3-5 failures

## Troubleshooting

### Connection Stuck in Reconnecting

```erlang
%% Check circuit breaker
{ok, _Health, Info} = erlmcp_connection_fsm:get_health(Pid),
#{circuit_breaker := #{state := CBState}} = Info,

case CBState of
    open -> %% Reset circuit breaker
        erlmcp_connection_fsm:reset_circuit_breaker(Pid);
    closed -> %% Check backoff config
        ok
end
```

### High Memory Usage

```erlang
%% Check send buffer
{ok, _State, Info} = erlmcp_connection_fsm:get_state(Pid),
BufferSize = maps:get(send_buffer_size, Info, 0),

case BufferSize > 1048576 of  %% 1MB
    true -> %% Reduce buffer size
        erlmcp_connection_fsm:update_config(Pid, #{
            flow_control => #flow_control{
                max_buffer_size = 524288  %% 512KB
            }
        });
    false -> ok
end
```

### Slow Reconnection

```erlang
%% Check backoff configuration
{ok, _State, Info} = erlmcp_connection_fsm:get_state(Pid),
BackoffConfig = maps:get(backoff_config, Info),

%% Reduce initial delay for faster recovery
NewBackoff = BackoffConfig#backoff_config{
    initial_delay = 500,
    max_delay = 10000
},
erlmcp_connection_fsm:update_config(Pid, #{
    backoff_config => NewBackoff
})
```

## References

- [OTP Design Principles - gen_statem](https://erlang.org/doc/man/gen_statem.html)
- [Circuit Breaker Pattern](https://martinfowler.com/bliki/CircuitBreaker.html)
- [Flow Control in TCP](https://en.wikipedia.org/wiki/Flow_control_(data))
- [Connection Pooling](https://en.wikipedia.org/wiki/Connection_pool)
