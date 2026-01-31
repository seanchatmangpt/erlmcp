# Performance Validator - Performance Baseline Validation

## Purpose and Scope

The `erlmcp_performance_validator` module validates that MCP implementations meet defined performance targets. It measures:

- **Latency** - Request-response timing (p50, p95, p99)
- **Throughput** - Requests per second under load
- **Memory** - Per-connection and per-node memory usage
- **Connection Setup** - Time to establish new connections
- **Concurrent Connections** - Ability to handle multiple simultaneous connections

## Architecture

```
erlmcp_performance_validator (gen_server)
├── Latency Measurement      - Percentile timing (p50/p95/p99)
├── Throughput Measurement   - Requests/second under load
├── Memory Measurement       - Per-connection and per-node
├── Connection Setup         - New connection establishment
├── Concurrent Handling      - Multiple simultaneous connections
└── Sustained Load           - Long-term stability
```

## Performance Targets

| Metric | Target | Scope | Notes |
|--------|--------|-------|-------|
| P50 Latency | < 5ms | Per request | Median request time |
| P95 Latency | < 20ms | Per request | 95th percentile |
| P99 Latency | < 50ms | Per request | 99th percentile |
| Throughput | > 1000 req/s | Sustained | Minimum sustained |
| Memory/Connection | < 100KB | Per connection | Heap memory |
| Connection Setup | < 100ms | New connection | TCP handshake included |
| Concurrent Connections | 10K | Simultaneous | Active connections |
| Sustained Load | 60M ops/30s | 30-second test | No degradation |

## API Reference

### Server Management

#### start_link/0

Start the performance validator gen_server.

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
```

**Example:**
```erlang
{ok, Pid} = erlmcp_performance_validator:start_link().
```

#### run/1

Run full performance validation for a transport type.

```erlang
-spec run(transport_type()) -> {ok, performance_report()}.
```

**Parameters:**
- `Transport` - Transport atom: `stdio`, `tcp`, `http`, `websocket`

**Returns:**
- `{ok, Report}` - Performance report with all metrics

**Example:**
```erlang
{ok, Report} = erlmcp_performance_validator:run(tcp).
% Returns: #{
%   transport => tcp,
%   timestamp => ...,
%   latency => #{p50_us => 3500, p95_us => 18000, p99_us => 45000},
%   throughput => #{requests_per_second => 1523},
%   memory => #{bytes_per_connection => 87500},
%   connection_setup => #{avg_setup_time_us => 85000},
%   concurrent_connections => #{max_connections => 10000},
%   overall_passed => true
% }
```

#### run/2

Run performance validation with custom options.

```erlang
-spec run(transport_type(), map()) -> {ok, performance_report()}.
```

**Options:**
```erlang
#{
    samples => 100,              % Number of samples for latency
    requests => 1000,            % Total requests for throughput
    timeout => 30000,            % Timeout in milliseconds
    connections => 1000,         % Number of concurrent connections
    duration => 30000            % Sustained load duration
}
```

**Example:**
```erlang
{ok, Report} = erlmcp_performance_validator:run(tcp, #{
    samples => 500,
    requests => 5000,
    timeout => 60000
}).
```

### Latency Measurement

#### measure_latency/2

Measure request-response latency for a transport.

```erlang
-spec measure_latency(transport_type(), pos_integer()) -> {ok, latency_result()}.
```

**Parameters:**
- `Transport` - Transport type
- `Samples` - Number of samples to collect

**Returns:**
- `{ok, Result}` - Map with percentile latencies

**Example:**
```erlang
{ok, Result} = erlmcp_performance_validator:measure_latency(tcp, 100).
% Returns: #{
%   transport => tcp,
%   samples => 100,
%   p50_us => 3500,       % Median latency
%   p75_us => 7200,       % 75th percentile
%   p90_us => 12500,      % 90th percentile
%   p95_us => 18000,      % 95th percentile
%   p99_us => 45000,      % 99th percentile
%   max_us => 89000,      % Maximum latency
%   avg_us => 6200        % Average latency
% }
```

#### validate_latency_targets/1

Check if latency meets targets.

```erlang
-spec validate_latency_targets(latency_result()) -> {ok, boolean(), map()}.
```

**Example:**
```erlang
{ok, Passed, Details} = erlmcp_performance_validator:validate_latency_targets(
    #{p50_us => 3500, p95_us => 18000, p99_us => 45000}
),
% Returns: {ok, true, #{p50_passed => true, p95_passed => true, p99_passed => true}}
```

### Throughput Measurement

#### measure_throughput/2

Measure sustained throughput for a transport.

```erlang
-spec measure_throughput(transport_type(), pos_integer()) -> {ok, throughput_result()}.
```

**Parameters:**
- `Transport` - Transport type
- `TotalRequests` - Total number of requests to send

**Returns:**
- `{ok, Result}` - Throughput metrics

**Example:**
```erlang
{ok, Result} = erlmcp_performance_validator:measure_throughput(tcp, 10000).
% Returns: #{
%   transport => tcp,
%   total_requests => 10000,
%   duration_ms => 6567,
%   requests_per_second => 1523.4,
%   avg_latency_us => 6567,
%   errors => 0
% }
```

#### validate_throughput_targets/1

Check if throughput meets targets.

```erlang
-spec validate_throughput_targets(throughput_result()) -> {ok, boolean(), map()}.
```

**Example:**
```erlang
{ok, Passed, Details} = erlmcp_performance_validator:validate_throughput_targets(
    #{requests_per_second => 1523.4}
),
% Returns: {ok, true, #{target => 1000.0, actual => 1523.4, passed => true}}
```

### Memory Measurement

#### measure_memory/1

Measure per-connection memory usage.

```erlang
-spec measure_memory(transport_type()) -> {ok, memory_result()}.
```

**Parameters:**
- `Transport` - Transport type

**Returns:**
- `{ok, Result}` - Memory usage metrics

**Example:**
```erlang
{ok, Result} = erlmcp_performance_validator:measure_memory(tcp).
% Returns: #{
%   transport => tcp,
%   connections => 10,
%   heap_per_connection_bytes => 87500,
%   total_heap_bytes => 875000,
%   rss_bytes => 1250000
% }
```

#### validate_memory_targets/1

Check if memory usage meets targets.

```erlang
-spec validate_memory_targets(memory_result()) -> {ok, boolean(), map()}.
```

**Example:**
```erlang
{ok, Passed, Details} = erlmcp_performance_validator:validate_memory_targets(
    #{heap_per_connection_bytes => 87500}
),
% Returns: {ok, true, #{target => 100000, actual => 87500, passed => true}}
```

### Connection Setup Measurement

#### measure_connection_setup/1

Measure time to establish new connections.

```erlang
-spec measure_connection_setup(transport_type()) -> {ok, connection_setup_result()}.
```

**Parameters:**
- `Transport` - Transport type

**Returns:**
- `{ok, Result}` - Connection setup metrics

**Example:**
```erlang
{ok, Result} = erlmcp_performance_validator:measure_connection_setup(tcp).
% Returns: #{
%   transport => tcp,
%   samples => 50,
%   avg_setup_time_us => 85000,
%   p50_setup_time_us => 72000,
%   p95_setup_time_us => 125000,
%   max_setup_time_us => 230000
% }
```

### Concurrent Connection Test

#### test_concurrent_connections/2

Test ability to handle concurrent connections.

```erlang
-spec test_concurrent_connections(transport_type(), pos_integer()) -> {ok, concurrent_result()}.
```

**Parameters:**
- `Transport` - Transport type
- `NumConnections` - Number of concurrent connections

**Returns:**
- `{ok, Result}` - Concurrent connection test results

**Example:**
```erlang
{ok, Result} = erlmcp_performance_validator:test_concurrent_connections(tcp, 1000).
% Returns: #{
%   transport => tcp,
%   requested_connections => 1000,
%   successful_connections => 997,
%   failed_connections => 3,
%   avg_setup_time_us => 92000,
%   total_memory_bytes => 87500000
% }
```

## Usage Examples

### Running Full Performance Validation

```erlang
%% Start the validator
{ok, _} = application:ensure_all_started(erlmcp_validation),
{ok, _} = erlmcp_performance_validator:start_link(),

%% Run validation for each transport
Transports = [stdio, tcp, http, websocket],

lists:foreach(fun(Transport) ->
    io:format("Testing ~p transport...~n", [Transport]),

    case erlmcp_performance_validator:run(Transport) of
        {ok, #{overall_passed := true} = Report} ->
            io:format("  ~p: PASSED~n", [Transport]),
            print_latency(maps:get(latency, Report)),
            print_throughput(maps:get(throughput, Report));
        {ok, #{overall_passed := false} = Report} ->
            io:format("  ~p: FAILED~n", [Transport]),
            print_failures(Report);
        {error, Reason} ->
            io:format("  ~p: ERROR - ~p~n", [Transport, Reason])
    end
end, Transports).

print_latency(#{p50_us := P50, p95_us := P95, p99_us := P99}) ->
    io:format("    Latency: p50=~pus p95=~pus p99=~pus~n", [P50, P95, P99]).

print_throughput(#{requests_per_second := RPS}) ->
    io:format("    Throughput: ~.1f req/s~n", [RPS]).
```

### Custom Performance Test

```erlang
%% Run custom test with specific options
Options = #{
    samples => 500,        % 500 latency samples
    requests => 10000,     % 10K requests for throughput
    timeout => 60000,      % 60 second timeout
    connections => 5000    % Test with 5K concurrent connections
},

{ok, Report} = erlmcp_performance_validator:run(tcp, Options),

%% Check specific metrics
#{latency := Latency, throughput := Throughput} = Report,

%% Verify against custom thresholds
TargetP50 = 10000,  % 10ms p50
TargetRPS = 2000,   % 2000 req/s

P50Passed = maps:get(p50_us, Latency) < TargetP50,
RPSPassed = maps:get(requests_per_second, Throughput) > TargetRPS,

if
    P50Passed andalso RPSPassed ->
        io:format("Performance test PASSED~n");
    true ->
        io:format("Performance test FAILED~n")
end.
```

### Benchmarking Before/After Changes

```erlang
%% Helper to capture baseline
capture_baseline(Transport) ->
    {ok, Report} = erlmcp_performance_validator:run(Transport),
    #{
        latency => maps:get(latency, Report),
        throughput => maps:get(throughput, Report),
        memory => maps:get(memory, Report)
    }.

%% Use before and after code changes
compare_performance(Before, After) ->
    CompareLatency = fun(Key) ->
        B = maps:get(Key, Before),
        A = maps:get(Key, After),
        ((A - B) / B) * 100
    end,

    P50Change = CompareLatency(p50_us),
    P95Change = CompareLatency(p95_us),
    RPSChange = (maps:get(requests_per_second, maps:get(throughput, After)) -
                 maps:get(requests_per_second, maps:get(throughput, Before))) /
                maps:get(requests_per_second, maps:get(throughput, Before)) * 100,

    io:format("Latency Changes: p50: ~.1f%%, p95: ~.1f%%~n", [P50Change, P95Change]),
    io:format("Throughput Change: ~.1f%%~n", [RPSChange]).
```

### Continuous Monitoring

```erlang
%% Run periodic performance checks
start_monitoring(Transport, IntervalMs) ->
    spawn(fun() ->
        monitoring_loop(Transport, IntervalMs, erlang:timestamp())
    end).

monitoring_loop(Transport, IntervalMs, LastTime) ->
    {ok, Report} = erlmcp_performance_validator:run(Transport),

    %% Log metrics
    #{latency := #{p95_us := P95}, throughput := #{requests_per_second := RPS}} = Report,
    logger:info("Performance: p95=~pus, rps=~.1f", [P95, RPS]),

    %% Alert on degradation
    P95 > 50000 * 1000 andalso logger:warning("High p95 latency: ~pus", [P95]),

    timer:sleep(IntervalMs),
    monitoring_loop(Transport, IntervalMs, erlang:timestamp()).
```

## Testing Guidance

### Unit Tests

```erlang
%% Test latency measurement structure
latency_measurement_test() ->
    {ok, Result} = erlmcp_performance_validator:measure_latency(stdio, 10),

    ?assert(is_map(Result)),
    ?assert(maps:is_key(p50_us, Result)),
    ?assert(maps:is_key(p95_us, Result)),
    ?assert(maps:is_key(p99_us, Result)),
    ?assert(maps:get(samples, Result) =< 10).

%% Test throughput measurement structure
throughput_measurement_test() ->
    {ok, Result} = erlmcp_performance_validator:measure_throughput(stdio, 100),

    ?assert(is_map(Result)),
    ?assert(maps:is_key(requests_per_second, Result)),
    ?assert(maps:get(total_requests, Result) =< 100).
```

### Performance Regression Tests

```erlang
%% Property: Throughput should not degrade more than 10%
prop_throughput_no_regression() ->
    ?FORALL({Transport, Requests},
        {oneof([stdio, tcp]), choose(100, 1000)},
        begin
            {ok, Result} = erlmcp_performance_validator:measure_throughput(
                Transport, Requests
            ),
            RPS = maps:get(requests_per_second, Result),
            RPS > 500  % Minimum threshold
        end).
```

## Troubleshooting

### Common Performance Issues

#### High Latency

**Symptom:** p95/p99 latencies exceed targets

**Diagnosis:**
```erlang
{ok, Result} = erlmcp_performance_validator:measure_latency(tcp, 100),
%% Check p99 >> p50 indicates outliers
p99_outlier = maps:get(p99_us, Result) / maps:get(p50_us, Result) > 10.
```

**Solutions:**
1. Check for blocking operations in handlers
2. Verify connection pool size
3. Monitor system resources (CPU, memory)
4. Check for GC pauses

#### Low Throughput

**Symptom:** Requests per second below target

**Diagnosis:**
```erlang
{ok, Result} = erlmcp_performance_validator:measure_throughput(tcp, 1000),
%% Check error count
Errors = maps:get(errors, Result),
Errors > 0 andalso logger:warning("~p errors detected", [Errors]).
```

**Solutions:**
1. Increase worker pool size
2. Optimize request processing
3. Check for bottlenecks (serialization, I/O)
4. Consider batching

#### High Memory Usage

**Symptom:** Per-connection memory exceeds target

**Diagnosis:**
```erlang
{ok, Result} = erlmcp_performance_validator:measure_memory(tcp),
PerConn = maps:get(heap_per_connection_bytes, Result),
PerConn > 100000 andalso logger:warning("High memory: ~p bytes", [PerConn]).
```

**Solutions:**
1. Check for memory leaks in connections
2. Optimize message buffering
3. Use binary references for large data
4. Implement connection backpressure

#### Connection Setup Failures

**Symptom:** Concurrent connection test shows failures

**Diagnosis:**
```erlang
{ok, Result} = erlmcp_performance_validator:test_concurrent_connections(tcp, 1000),
Failed = maps:get(failed_connections, Result),
Failed > 0 andalso logger:warning("~p connections failed", [Failed]).
```

**Solutions:**
1. Increase acceptor pool size
2. Check file descriptor limits
3. Verify system ulimit settings
4. Check for resource exhaustion

## Performance Optimization Tips

### Latency Reduction

1. **Minimize blocking**: Use async casts for non-critical work
2. **Pool connections**: Reuse connections when possible
3. **Optimize serialization**: Use efficient JSON encoding
4. **Cache frequently accessed data**

### Throughput Improvement

1. **Scale workers**: Match worker count to CPU cores
2. **Batch operations**: Group similar requests
3. **Parallelize**: Use parallel map for independent work
4. **Profile**: Use fprof to find hotspots

### Memory Optimization

1. **Use binaries**: For strings and buffers
2. **Limit buffers**: Set max message sizes
3. **Backpressure**: Slow producers when consumers lag
4. **GC tuning**: Adjust garbage collection settings

## Related Documentation

- [MCP_SPEC_VALIDATION.md](MCP_SPEC_VALIDATION.md) - Validation overview
- [Benchmarks](../bench/README.md) - Benchmark suite
- [Performance Tests](../apps/erlmcp_validation/test/erlmcp_performance_validator_tests.erl)
