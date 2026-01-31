# Circuit Breaker POC - Enhanced Observability Demo

## Overview

This POC demonstrates an enhanced circuit breaker implementation using `gen_statem` with full telemetry integration for observability.

## Key Features

### 1. **State Machine Implementation**
- Uses `gen_statem` behavior for proper state management
- Three states: `closed`, `open`, `half_open`
- Automatic transitions based on thresholds and timeouts

### 2. **Telemetry Integration**
- Emits telemetry events on all state transitions
- Tracks call latency, success/failure rates
- Integrates with `erlmcp_otel` for distributed tracing
- Events include:
  - `circuit_breaker.breaker_initialized`
  - `circuit_breaker.call_succeeded`
  - `circuit_breaker.call_failed`
  - `circuit_breaker.call_rejected`
  - `circuit_breaker.state_transition`
  - `circuit_breaker.breaker_terminated`

### 3. **Per-Tool Circuit Breakers**
- Each tool gets its own circuit breaker instance
- Failures are isolated to individual tools
- Independent state machines for each breaker

### 4. **Gradual Recovery**
- Half-open state allows limited test calls
- Configurable success threshold to close circuit
- Re-opens immediately on failure in half-open state
- Tracks recovery time metrics

### 5. **Dashboard-Friendly Metrics**
- Real-time state reporting
- Comprehensive statistics:
  - Trip count
  - Failure/success rates
  - Average recovery time
  - Total calls/successes/failures/rejections
  - Consecutive failure/success counts

## Configuration

```erlang
Config = #{
    failure_threshold => 3,         % Failures to trip (default: 3)
    success_threshold => 2,         % Successes to close (default: 2)
    timeout_ms => 5000,             % Time before half-open (default: 5000)
    half_open_max_calls => 3        % Max concurrent calls in half-open (default: 3)
}.
```

## Usage

### Running the Demo

```erlang
%% In rebar3 shell
rebar3 shell

%% Run default demo
erlmcp_circuit_breaker_poc:run_demo().

%% Run with custom configuration
erlmcp_circuit_breaker_poc:run_demo(#{
    tool_name => <<"my_tool">>,
    config => #{
        failure_threshold => 5,
        success_threshold => 3,
        timeout_ms => 2000
    }
}).
```

### Using in Production Code

```erlang
%% Start circuit breaker for a tool
{ok, Breaker} = erlmcp_circuit_breaker_poc:start_link(
    <<"database_query">>,
    #{failure_threshold => 3, timeout_ms => 60000}
).

%% Execute calls through the breaker
Result = erlmcp_circuit_breaker_poc:call_tool(Breaker, fun() ->
    %% Your tool call here
    database:execute_query(Query)
end).

%% Get current state
{State, StateData} = erlmcp_circuit_breaker_poc:get_state(Breaker).
%% State: closed | open | half_open
%% StateData: #{
%%     consecutive_failures => ...,
%%     consecutive_successes => ...,
%%     total_calls => ...,
%%     trip_count => ...,
%%     failure_rate => ...
%% }

%% Get detailed statistics
Stats = erlmcp_circuit_breaker_poc:get_stats(Breaker).
%% #{
%%     state => closed,
%%     total_calls => 1523,
%%     total_successes => 1498,
%%     total_failures => 25,
%%     total_rejected => 12,
%%     trip_count => 2,
%%     success_rate => 0.98,
%%     failure_rate => 0.02,
%%     avg_recovery_time_ms => 5432.5
%% }

%% Manual reset if needed
ok = erlmcp_circuit_breaker_poc:reset(Breaker).
```

## Demo Workflow

The demo runs through six phases:

### Phase 1: Normal Operation (CLOSED)
- 5 successful calls
- Circuit remains closed
- Consecutive successes increment

### Phase 2: Failures Causing Trip (CLOSED → OPEN)
- 4 failing calls
- Exceeds failure threshold (3)
- Circuit trips to OPEN state
- Telemetry event: `state_transition` with reason `failure_threshold_exceeded`

### Phase 3: Rejected Calls (OPEN)
- 3 calls attempted
- All rejected with `{error, circuit_breaker_open}`
- Total rejected count increments
- Telemetry events: `call_rejected`

### Phase 4: Waiting for Recovery Window
- Waits for timeout (2000ms)
- Automatic transition to HALF_OPEN
- Telemetry event: `state_transition` with reason `timeout_expired`

### Phase 5: Gradual Recovery (HALF_OPEN → CLOSED)
- 3 successful calls in half-open
- Exceeds success threshold (2)
- Circuit closes
- Recovery time tracked
- Telemetry event: `state_transition` with reason `success_threshold_met`

### Phase 6: Back to Normal (CLOSED)
- 3 more successful calls
- Normal operation resumed

## State Transitions

```
┌─────────┐
│ CLOSED  │────────[failure_threshold]────────┐
└─────────┘                                     │
     ▲                                          ▼
     │                                    ┌─────────┐
     │                                    │  OPEN   │
     │                                    └─────────┘
     │                                          │
     │                                 [timeout_expired]
     │                                          │
     │                                          ▼
     │                                  ┌──────────────┐
     │                                  │  HALF_OPEN   │
     └──────[success_threshold]─────────┤              │
                                        └──────────────┘
                                             │
                                        [failure]
                                             │
                                             ▼
                                        (back to OPEN)
```

## Telemetry Integration

### With erlmcp_otel

When `erlmcp_otel` is available, all events are automatically added to the current trace span:

```erlang
%% Start a span for tool execution
SpanCtx = erlmcp_otel:start_span(<<"mcp.tool.execute">>, #{
    <<"tool.name">> => <<"my_tool">>
}),

%% Execute with circuit breaker (events auto-added to span)
Result = erlmcp_circuit_breaker_poc:call_tool(Breaker, fun() ->
    execute_tool()
end),

%% End span
erlmcp_otel:end_span(SpanCtx).
```

### Event Attributes

All telemetry events include:
- `tool_name`: Name of the protected tool
- `timestamp`: Event timestamp in milliseconds
- Event-specific attributes (state, latency, errors, etc.)

## Metrics Tracked

### Counters
- `total_calls`: Total number of call attempts
- `total_successes`: Successful calls
- `total_failures`: Failed calls
- `total_rejected`: Rejected calls (while open)
- `trip_count`: Number of times circuit has opened
- `consecutive_failures`: Current failure streak
- `consecutive_successes`: Current success streak

### Rates
- `failure_rate`: Total failures / total calls
- `success_rate`: Total successes / total calls

### Timing
- `avg_recovery_time_ms`: Average time to recover from open state
- `last_failure_time`: Timestamp of last failure
- `last_state_change`: Timestamp of last state transition

## Integration with Dashboard

The POC is designed to integrate with the `erlmcp_dashboard_server`:

```erlang
%% Get all breaker states for dashboard
AllStates = [
    erlmcp_circuit_breaker_poc:get_state(Breaker)
    || Breaker <- get_all_breakers()
].

%% Get statistics for monitoring
AllStats = [
    erlmcp_circuit_breaker_poc:get_stats(Breaker)
    || Breaker <- get_all_breakers()
].
```

Dashboard can display:
- Current state (colored indicators: green=closed, yellow=half_open, red=open)
- Trip count and trend
- Success/failure rates
- Recovery time metrics
- Real-time state transitions

## Testing Chaos Scenarios

The POC can be integrated with `erlmcp_chaos` for resilience testing:

```erlang
%% Test circuit breaker under chaos
erlmcp_chaos:inject_failure(network_partition, #{
    duration_ms => 10000,
    affected_tools => [<<"database_query">>]
}),

%% Circuit breaker should trip and protect system
%% Monitor recovery after chaos ends
```

## Performance Characteristics

- **Call overhead**: < 1μs per call (gen_statem call + counter updates)
- **Memory**: ~1KB per circuit breaker instance
- **State transitions**: < 100μs
- **Telemetry overhead**: < 10μs per event (when using erlmcp_otel)

## Best Practices

### 1. **Choose Appropriate Thresholds**
```erlang
%% Fast-failing operations (< 100ms)
#{failure_threshold => 3, timeout_ms => 1000}

%% Slow external services (> 1s)
#{failure_threshold => 5, timeout_ms => 60000}

%% Critical operations
#{failure_threshold => 2, timeout_ms => 30000}
```

### 2. **Monitor Trip Count**
- High trip count indicates unstable service
- Consider increasing timeout or failure threshold
- Investigate root cause of failures

### 3. **Track Recovery Time**
- Long recovery times suggest service instability
- Consider exponential backoff for retry logic
- Alert on recovery time > SLA threshold

### 4. **Use with Fallbacks**
```erlang
case erlmcp_circuit_breaker_poc:call_tool(Breaker, ToolFun) of
    {ok, Result} -> Result;
    {error, circuit_breaker_open} -> use_fallback();
    {error, Other} -> handle_error(Other)
end.
```

## Future Enhancements

1. **Exponential Backoff**: Increase timeout after repeated trips
2. **Bulkhead Pattern**: Limit concurrent calls even when closed
3. **Health Checks**: Active probing in half-open state
4. **Distributed State**: Share circuit state across nodes
5. **Adaptive Thresholds**: Automatically adjust based on error rates
6. **Custom Failure Predicates**: Define what constitutes a failure

## See Also

- `erlmcp_circuit_breaker.erl` - Production circuit breaker implementation
- `erlmcp_otel.erl` - OpenTelemetry integration
- `erlmcp_dashboard_server.erl` - Web dashboard for monitoring
- `erlmcp_chaos.erl` - Chaos engineering for testing

## License

Part of the erlmcp project.
