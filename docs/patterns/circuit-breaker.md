# Circuit Breaker Pattern in erlmcp

## Overview

The circuit breaker pattern provides fault tolerance and resilience by preventing cascading failures in distributed systems. When a service or operation experiences repeated failures, the circuit breaker "trips" to an open state, failing fast instead of waiting for timeouts.

## State Machine

```
       ┌─────────┐
       │ CLOSED  │◄─────────────────┐
       │ (Normal)│                  │
       └────┬────┘                  │
            │                       │
   Failures │                       │ Success
   >= Threshold                     │ Threshold
            │                       │
            ▼                       │
       ┌─────────┐                  │
       │  OPEN   │                  │
       │(Failing)│                  │
       └────┬────┘                  │
            │                       │
   After    │                       │
   Timeout  │                       │
            ▼                       │
       ┌─────────┐                  │
       │HALF_OPEN│──────────────────┘
       │(Testing)│
       └─────────┘
            │
   Any      │
   Failure  │
            ▼
       (Back to OPEN)
```

### States

#### CLOSED (Normal Operation)
- All calls are allowed through
- Failures are tracked
- Transitions to OPEN when failure threshold is exceeded

#### OPEN (Failing Fast)
- All calls are immediately rejected with `{error, circuit_breaker_open}`
- No actual execution happens (prevents resource exhaustion)
- After timeout period, transitions to HALF_OPEN

#### HALF_OPEN (Testing Recovery)
- Limited calls are allowed through to test if system recovered
- Success transitions back to CLOSED
- Any failure immediately transitions back to OPEN

## Configuration

```erlang
Config = #{
    %% Consecutive failures to trip breaker (default: 5)
    failure_threshold => 5,

    %% Consecutive successes in half_open to close (default: 2)
    success_threshold => 2,

    %% Time in ms before attempting half_open (default: 60000)
    timeout => 60000,

    %% Rolling window size for failure rate (default: 10)
    window_size => 10,

    %% Failure rate threshold 0.0-1.0 (default: 0.5 = 50%)
    failure_rate_threshold => 0.5,

    %% Auto-reset timeout (default: infinity)
    reset_timeout => infinity
}.
```

## Basic Usage

### 1. Register Circuit Breaker

```erlang
%% Start circuit breaker manager
{ok, _Pid} = erlmcp_circuit_breaker:start_link(),

%% Register a breaker for a tool
Config = #{
    failure_threshold => 5,
    timeout => 30000  % 30 seconds
},
ok = erlmcp_circuit_breaker:register_breaker(my_tool, Config).
```

### 2. Execute Calls Through Breaker

```erlang
%% Simple call
Result = erlmcp_circuit_breaker:call(my_tool, fun() ->
    %% Your operation here
    external_service:call(Request)
end),

case Result of
    {ok, Response} ->
        handle_success(Response);
    {error, circuit_breaker_open} ->
        handle_breaker_open();
    {error, Reason} ->
        handle_error(Reason)
end.
```

### 3. Call with Fallback

```erlang
Result = erlmcp_circuit_breaker:call_with_fallback(
    my_tool,
    fun() ->
        %% Primary operation
        expensive_service:call(Request)
    end,
    fun() ->
        %% Fallback when circuit is open
        {ok, cached_response()}
    end
),

handle_result(Result).
```

## Integration with erlmcp_server

### Per-Tool Circuit Breakers

```erlang
-module(my_server).
-behaviour(gen_server).

init([ServerId]) ->
    %% Register circuit breakers for each tool
    Tools = [weather_tool, calculation_tool, database_tool],
    lists:foreach(fun(Tool) ->
        Config = tool_circuit_config(Tool),
        erlmcp_circuit_breaker:register_breaker(Tool, Config)
    end, Tools),

    State = #state{server_id = ServerId},
    {ok, State}.

%% Tool handler with circuit breaker
handle_call({call_tool, ToolName, Params}, _From, State) ->
    Result = erlmcp_circuit_breaker:call_with_fallback(
        ToolName,
        fun() -> execute_tool(ToolName, Params) end,
        fun() -> tool_fallback(ToolName, Params) end
    ),
    {reply, Result, State}.

execute_tool(weather_tool, #{city := City}) ->
    %% External API call
    case weather_api:get_forecast(City) of
        {ok, Forecast} -> {ok, Forecast};
        {error, _} = Error -> Error
    end;
execute_tool(ToolName, Params) ->
    %% Other tools
    tool_impl:call(ToolName, Params).

tool_fallback(weather_tool, #{city := City}) ->
    %% Return cached data when circuit is open
    {ok, cached_weather:get(City)};
tool_fallback(_Tool, _Params) ->
    {error, service_unavailable}.

tool_circuit_config(weather_tool) ->
    #{failure_threshold => 3, timeout => 30000};
tool_circuit_config(database_tool) ->
    #{failure_threshold => 5, timeout => 60000};
tool_circuit_config(_) ->
    #{}.  % Use defaults
```

### Per-Transport Circuit Breakers

```erlang
%% Protect HTTP transports
register_transport_breaker(TransportId, http) ->
    Config = #{
        failure_threshold => 5,
        timeout => 60000,
        failure_rate_threshold => 0.7  % 70% failure rate
    },
    erlmcp_circuit_breaker:register_breaker({transport, TransportId}, Config);

register_transport_breaker(_TransportId, _Type) ->
    ok.

%% Use in transport send
send_with_breaker(TransportId, Data) ->
    erlmcp_circuit_breaker:call(
        {transport, TransportId},
        fun() ->
            transport_impl:send(TransportId, Data)
        end
    ).
```

## Health Monitor Integration

Circuit breaker automatically reports to `erlmcp_health_monitor`:

```erlang
%% Register with health monitoring
erlmcp_circuit_breaker:register_breaker(
    my_service,
    Config,
    whereis(erlmcp_health_monitor)
),

%% Health monitor receives automatic notifications:
%% - Circuit state changes (open/closed)
%% - Degradation events
%% - Failure statistics
```

## Monitoring and Operations

### Check Breaker State

```erlang
%% Get current state
State = erlmcp_circuit_breaker:get_state(my_tool),
%% Returns: closed | open | half_open | not_found

%% Get all states
AllStates = erlmcp_circuit_breaker:get_all_states(),
%% Returns: #{my_tool => closed, other_tool => open, ...}
```

### Statistics and Metrics

```erlang
{ok, Stats} = erlmcp_circuit_breaker:get_stats(my_tool),

#{
    name => my_tool,
    state => open,
    consecutive_failures => 5,
    consecutive_successes => 0,
    total_calls => 127,
    total_successes => 122,
    total_failures => 5,
    total_rejected => 0,
    failure_rate => 0.5,        % 50% in rolling window
    success_rate => 0.96,       % 96% overall
    last_failure_time => {{2026,1,27},{22,15,30}},
    last_state_change => {{2026,1,27},{22,15,31}},
    config => #{...}
} = Stats.

%% Get all statistics
AllStats = erlmcp_circuit_breaker:get_all_stats().
```

### Manual Operations

```erlang
%% Reset a breaker (close it immediately)
ok = erlmcp_circuit_breaker:reset(my_tool),

%% Reset all breakers
ok = erlmcp_circuit_breaker:reset_all(),

%% Force breaker open (maintenance mode)
ok = erlmcp_circuit_breaker:force_open(my_tool),

%% Force breaker closed (restore service)
ok = erlmcp_circuit_breaker:force_close(my_tool).
```

## Failure Detection Strategies

### 1. Consecutive Failures

Trips after N consecutive failures:

```erlang
Config = #{
    failure_threshold => 5  % 5 consecutive failures
}.
```

**Best for:** Persistent failures, service outages.

### 2. Failure Rate in Rolling Window

Trips when failure rate exceeds threshold in recent calls:

```erlang
Config = #{
    failure_threshold => 100,         % Don't trip on consecutive
    window_size => 20,                % Look at last 20 calls
    failure_rate_threshold => 0.5     % Trip at 50% failure rate
}.
```

**Best for:** Intermittent failures, degraded performance.

### 3. Combined Strategy

Both checks are evaluated (OR condition):

```erlang
Config = #{
    failure_threshold => 5,           % 5 consecutive OR
    failure_rate_threshold => 0.7,    % 70% in window
    window_size => 10
}.
```

**Best for:** Production systems - catches both persistent and intermittent failures.

## Bulkhead Pattern (Resource Isolation)

Circuit breakers support resource quotas to prevent one failing service from exhausting shared resources:

```erlang
%% Future feature - currently in breaker record
-record(breaker, {
    ...
    resource_quotas = #{
        max_connections => 10,
        max_memory_mb => 100,
        max_queue_size => 1000
    }
}).
```

## Best Practices

### 1. Choose Appropriate Thresholds

```erlang
%% Critical services - trip fast
critical_tool_config() ->
    #{failure_threshold => 3, timeout => 30000}.

%% Resilient services - be lenient
resilient_tool_config() ->
    #{failure_threshold => 10, timeout => 120000}.

%% External APIs - aggressive
external_api_config() ->
    #{
        failure_threshold => 5,
        timeout => 60000,
        failure_rate_threshold => 0.8  % 80% failure rate
    }.
```

### 2. Always Provide Fallbacks

```erlang
%% Good: Fallback to cache
erlmcp_circuit_breaker:call_with_fallback(
    api_tool,
    fun() -> external_api:call() end,
    fun() -> {ok, cache:get()} end
).

%% Bad: No fallback - caller gets error
erlmcp_circuit_breaker:call(
    api_tool,
    fun() -> external_api:call() end
).
```

### 3. Log State Changes

Circuit breaker automatically logs at INFO/WARNING levels:

```
INFO: Registered circuit breaker: my_tool with config: ...
WARNING: Circuit breaker my_tool tripping to OPEN state (failures: 5)
INFO: Circuit breaker my_tool transitioning to HALF_OPEN state
INFO: Circuit breaker my_tool closing (successes: 2)
```

### 4. Monitor in Production

```erlang
%% Periodic health check
check_circuit_health() ->
    AllStates = erlmcp_circuit_breaker:get_all_states(),
    OpenBreakers = maps:filter(fun(_K, V) -> V =:= open end, AllStates),

    case maps:size(OpenBreakers) > 0 of
        true ->
            alert("Circuit breakers open: ~p", [maps:keys(OpenBreakers)]);
        false ->
            ok
    end.
```

### 5. Test Recovery Scenarios

```erlang
%% Integration test
recovery_test() ->
    Config = #{failure_threshold => 2, timeout => 100},
    erlmcp_circuit_breaker:register_breaker(test, Config),

    %% Simulate failures
    erlmcp_circuit_breaker:call(test, fun() -> {error, fail1} end),
    erlmcp_circuit_breaker:call(test, fun() -> {error, fail2} end),

    ?assertEqual(open, erlmcp_circuit_breaker:get_state(test)),

    %% Wait for recovery
    timer:sleep(150),

    %% Test recovery
    erlmcp_circuit_breaker:call(test, fun() -> {ok, success} end),
    erlmcp_circuit_breaker:call(test, fun() -> {ok, success} end),

    ?assertEqual(closed, erlmcp_circuit_breaker:get_state(test)).
```

## Performance Characteristics

### Memory

- Per-breaker overhead: ~500 bytes
- Rolling window: ~200 bytes per call (up to window_size)
- 100 breakers with window_size=10: ~70KB total

### CPU

- Call through breaker: ~5µs overhead
- State transition: ~10µs
- Statistics calculation: ~15µs

### Latency

- **CLOSED state**: 5µs overhead
- **OPEN state**: 1µs (immediate rejection)
- **HALF_OPEN state**: 5µs overhead

## Comparison with Other Patterns

| Pattern | Use Case | Overhead | Recovery |
|---------|----------|----------|----------|
| **Circuit Breaker** | Service failures | Low | Automatic |
| **Retry** | Transient failures | High | Immediate |
| **Timeout** | Hanging calls | None | Per-call |
| **Rate Limiting** | Overload protection | Low | Continuous |
| **Bulkhead** | Resource isolation | Medium | N/A |

**Circuit breaker is best for:** Protecting against cascading failures from dependent services.

## References

- Martin Fowler: [CircuitBreaker](https://martinfowler.com/bliki/CircuitBreaker.html)
- Release It! by Michael Nygard
- [Hystrix Design](https://github.com/Netflix/Hystrix/wiki) (Netflix)
- erlmcp docs: `docs/otp-patterns.md`

## See Also

- `erlmcp_health_monitor` - System health monitoring
- `erlmcp_batch` - Request batching and backpressure
- `docs/architecture.md` - Overall system architecture
