# ErlMCP Load Testing Framework

A comprehensive load generation and performance testing framework for ErlMCP that creates realistic traffic patterns, exposes system limits, and provides detailed tracing and analysis.

## Features

### 🚀 Traffic Patterns
- **Constant Rate**: Steady N req/sec load
- **Burst Patterns**: Spike to 10x base rate
- **Gradual Ramp-up**: 0 to max rate progression
- **Sine Wave**: Periodic load variation
- **Random Walk**: Unpredictable load changes
- **Poisson Distribution**: Realistic arrival patterns

### 📊 Message Patterns
- **Small Messages**: < 1KB payloads
- **Medium Messages**: 1KB - 1MB payloads
- **Large Messages**: 1MB - 100MB payloads
- **Mixed Distribution**: Realistic size variation
- **Binary vs Text**: Protocol-appropriate formats

### 🔗 Connection Patterns
- **Long-lived**: Persistent connections
- **Short-lived**: Connect per request
- **Connection Pooling**: Shared connection pools
- **Reconnection**: Automatic recovery patterns

### 💼 Workload Scenarios
- **Read-heavy**: 90% read operations
- **Write-heavy**: 90% write operations
- **Balanced**: 50/50 read/write mix
- **Batch Operations**: Bulk processing
- **Stream Processing**: Real-time data flows

### 🔍 Protocol Support
- **HTTP**: RESTful APIs
- **WebSocket**: Real-time bidirectional
- **TCP**: Raw socket communication
- **MCP**: Message Control Protocol
- **Generic**: Extensible protocol handler

### 📈 Comprehensive Metrics
- **Throughput**: Requests per second
- **Latency**: P50, P95, P99 percentiles
- **Success Rate**: Error percentages
- **Resource Usage**: CPU, memory, network
- **System Limits**: Bottleneck detection

## Quick Start

### Prerequisites

```bash
# Ensure Erlang/OTP and required dependencies
rebar3 deps
```

### Basic Usage

```bash
# Run all tests with defaults
./test/run_load_tests.sh

# Run specific test suite
./test/run_load_tests.sh scenarios

# Custom configuration
./test/run_load_tests.sh -d 60 -r 100 --no-trace limits
```

### Programmatic Usage

```erlang
%% Start load generator
{ok, _Pid} = erlmcp_load_generator:start_link().

%% Basic load generation
Config = #{
    pattern => constant,
    rate => 50,
    duration => 30000,
    message_size => medium,
    protocol => http,
    trace_every_request => true
},

{ok, GeneratorId} = erlmcp_load_generator:generate_traffic(Config).

%% Get metrics
{ok, Metrics} = erlmcp_load_generator:get_metrics(GeneratorId).
```

## Test Suites

### Unit Tests (`unit`)
Core functionality validation:
- Pattern generation algorithms
- Message size handling
- Connection management
- Protocol handlers
- Metrics collection
- Error handling
- Tracing integration

```bash
./test/run_load_tests.sh unit
```

### Scenario Tests (`scenarios`)
Predefined realistic workloads:
- API baseline performance
- Mobile app patterns
- Peak hour simulation
- WebSocket streaming
- MCP protocol validation
- Mixed workloads

```bash
./test/run_load_tests.sh scenarios
```

### System Limits (`limits`)
Performance boundary detection:
- Increasing load progression
- Breaking point identification
- Stability thresholds
- Resource constraints
- Degradation patterns

```bash
./test/run_load_tests.sh limits
```

### Protocol Comparison (`protocols`)
Multi-protocol benchmarking:
- HTTP vs WebSocket vs TCP vs MCP
- Latency characteristics
- Throughput comparison
- Reliability analysis
- Use case recommendations

```bash
./test/run_load_tests.sh protocols
```

### MCP Stress Testing (`mcp-stress`)
MCP server validation:
- Progressive stress phases
- Server health assessment
- Recovery testing
- Performance degradation
- Error pattern analysis

```bash
./test/run_load_tests.sh mcp-stress
```

### Capacity Planning (`capacity`)
Deployment sizing guidance:
- User load simulation
- SLA compliance testing
- Infrastructure recommendations
- Scaling factor analysis
- Resource requirements

```bash
./test/run_load_tests.sh capacity
```

## Configuration Options

### Load Patterns

```erlang
%% Constant load - steady rate
#{
    pattern => constant,
    rate => 100,              % req/sec
    duration => 60000         % milliseconds
}

%% Burst pattern - traffic spikes
#{
    pattern => burst,
    rate => 50,               % base rate
    duration => 120000        % total duration
}

%% Ramp-up pattern - gradual increase
#{
    pattern => ramp_up,
    rate => 200,              % max rate
    duration => 180000        % ramp duration
}

%% Sine wave - periodic variation
#{
    pattern => sine_wave,
    rate => 80,               % base rate
    duration => 240000        % wave duration
}

%% Random walk - unpredictable changes
#{
    pattern => random_walk,
    rate => 60,               % starting rate
    duration => 150000        % walk duration
}

%% Poisson distribution - realistic arrivals
#{
    pattern => poisson,
    rate => 75,               % lambda (avg rate)
    duration => 90000         % duration
}
```

### Message Configuration

```erlang
%% Message sizes
#{
    message_size => small,    % < 1KB
    message_size => medium,   % 1KB - 1MB
    message_size => large,    % 1MB - 100MB
    message_size => mixed     % realistic distribution
}
```

### Connection Management

```erlang
%% Connection types
#{
    connection_type => long_lived,    % persistent
    connection_type => short_lived,   % per-request
    connection_type => pooled,        % shared pool
    max_connections => 20             % pool size
}
```

### Protocol Settings

```erlang
%% Protocol configuration
#{
    protocol => http,
    target_host => "localhost",
    target_port => 8080,
    workload => balanced,           % read_heavy | write_heavy | balanced
    batch_size => 1,               % for batch operations
    trace_every_request => true     % detailed tracing
}
```

## Predefined Scenarios

### API Baseline
Standard API performance validation
- 20 req/sec constant load
- 30 second duration
- Small messages, balanced workload
- Full tracing enabled

### Burst Traffic
Traffic spike simulation
- 30 req/sec base, 300 req/sec peaks
- 60 second duration
- Medium messages, read-heavy
- Connection pooling

### Mobile Patterns
Mobile app usage simulation
- Random walk pattern
- 40 req/sec average
- Small messages, read-heavy
- Short-lived connections

### Peak Hours
Peak usage time simulation
- Sine wave pattern
- 80 req/sec base rate
- 180 second duration
- Mixed message sizes

### WebSocket Streaming
Real-time streaming test
- 50 req/sec constant
- Medium messages
- Stream workload
- Long-lived connections

### MCP Protocol Test
MCP server validation
- Ramp-up to 50 req/sec
- 45 second duration
- Medium messages
- Connection pooling

## Metrics and Analysis

### Core Metrics

```erlang
#{
    generator_id => <<"gen_123">>,
    pattern => constant,
    duration_ms => 30000,
    requests_sent => 1500,
    responses_received => 1485,
    errors => 15,
    success_rate => 0.99,
    throughput_rps => 49.5,
    avg_latency_us => 125000,      % 125ms
    p50_latency_us => 100000,      % 100ms
    p95_latency_us => 250000,      % 250ms
    p99_latency_us => 500000       % 500ms
}
```

### System Analysis

The framework provides comprehensive system analysis:

- **Performance Limits**: Maximum stable throughput
- **Bottleneck Detection**: Resource constraints
- **Degradation Patterns**: Performance decline analysis
- **Error Classification**: Failure mode identification
- **Recovery Characteristics**: System resilience

### OpenTelemetry Integration

All load generation includes distributed tracing:

```erlang
%% Automatic span creation for each request
SpanCtx = otel_tracer:start_span(<<"load_request">>),
otel_span:set_attributes(SpanCtx, [
    {<<"request_id">>, RequestId},
    {<<"generator_id">>, GeneratorId},
    {<<"operation">>, Operation},
    {<<"message_size">>, MessageSize}
]).
```

## Advanced Usage

### Custom Protocol Handler

```erlang
%% Add custom protocol support
send_custom_request(Connection, Operation, Message, RequestId) ->
    % Implement custom protocol logic
    StartTime = erlang:system_time(microsecond),

    % Send request using custom protocol
    Result = my_protocol:send_request(Connection, Message),

    EndTime = erlang:system_time(microsecond),
    Latency = EndTime - StartTime,

    case Result of
        {ok, Response} ->
            {ok, #{response => Response, latency => Latency}};
        {error, Reason} ->
            {error, Reason}
    end.
```

### Custom Load Pattern

```erlang
%% Implement custom load pattern
generate_custom_load(GeneratorId, Config, SpanCtx, Parent, ConnMgr, Metrics) ->
    % Custom pattern logic
    Rate = calculate_custom_rate(),
    IntervalMs = 1000 / Rate,

    % Generate requests according to custom pattern
    NewMetrics = send_request(GeneratorId, Config, SpanCtx, ConnMgr, Metrics),
    timer:sleep(round(IntervalMs)),

    % Continue pattern...
    generate_custom_load(GeneratorId, Config, SpanCtx, Parent, ConnMgr, NewMetrics).
```

### Real-time Monitoring

```erlang
%% Monitor load generation in real-time
{ok, GeneratorId} = erlmcp_load_generator:generate_traffic(Config),

% Monitor every 5 seconds
monitor_loop(GeneratorId) ->
    timer:sleep(5000),
    {ok, Metrics} = erlmcp_load_generator:get_metrics(GeneratorId),

    CurrentThroughput = maps:get(throughput_rps, Metrics),
    SuccessRate = maps:get(success_rate, Metrics),

    io:format("Current: ~.1f req/sec, ~.1f% success~n",
              [CurrentThroughput, SuccessRate * 100]),

    monitor_loop(GeneratorId).
```

## Performance Tuning

### High-Rate Testing

For high-rate testing (>1000 req/sec):

```erlang
Config = #{
    pattern => constant,
    rate => 2000,
    duration => 60000,
    message_size => small,
    protocol => tcp,
    connection_type => pooled,
    max_connections => 100,
    trace_every_request => false  % Reduce overhead
}.
```

### Memory Optimization

```erlang
%% Limit stored latency samples
#{
    % Automatically limits to last 1000 measurements
    % to prevent memory growth
}
```

### Connection Pooling

```erlang
Config = #{
    connection_type => pooled,
    max_connections => 50,     % Tune based on target system
    % Pool connections across requests
}.
```

## Troubleshooting

### Common Issues

1. **High Memory Usage**
   - Disable request tracing: `trace_every_request => false`
   - Use smaller message sizes
   - Reduce test duration

2. **Connection Failures**
   - Increase connection pool size
   - Use connection pooling instead of short-lived
   - Check target system limits

3. **Low Throughput**
   - Verify target system capacity
   - Check network bandwidth
   - Use multiple generators

4. **High Error Rates**
   - Reduce load rate
   - Check target system health
   - Verify protocol implementation

### Debugging

```bash
# Enable verbose logging
export ERLANG_LOG_LEVEL=debug

# Check system resources
./test/run_load_tests.sh limits

# Test single protocol
./test/run_load_tests.sh protocols
```

## Results and Reporting

### Output Files

```
test/load_test_results/
├── scenario_results.json      # Individual scenario results
├── scenario_summary.txt       # Scenario test summary
├── limits_test.json          # System limits data
├── limits_report.txt         # Limits analysis
├── protocol_comparison.json  # Protocol benchmark data
├── protocol_report.txt       # Protocol analysis
├── mcp_stress_test.json      # MCP stress results
├── capacity_planning.json    # Capacity analysis
├── load_test_report.html     # Comprehensive HTML report
└── test_summary.txt          # Quick text summary
```

### HTML Report

The framework generates a comprehensive HTML report with:
- Test configuration summary
- Performance metrics visualization
- Protocol comparison charts
- System limits analysis
- Capacity recommendations
- Raw data downloads

### Integration

Results can be integrated with monitoring systems:

```bash
# Export metrics to monitoring system
jq '.throughput_rps' scenario_results.json | monitoring_system_push
```

## Best Practices

### Test Planning

1. **Start Small**: Begin with low rates and short durations
2. **Incremental Testing**: Gradually increase load to find limits
3. **Realistic Patterns**: Use traffic patterns that match production
4. **Baseline First**: Establish baseline performance before optimization
5. **Monitor Resources**: Track system resource usage during tests

### Production Testing

1. **Staging Environment**: Test in production-like environment
2. **Gradual Rollout**: Slowly increase production load testing
3. **Error Budgets**: Plan for acceptable error rates during testing
4. **Rollback Plans**: Have procedures to quickly stop load generation
5. **Coordination**: Communicate with operations team

### Result Analysis

1. **Trend Analysis**: Look for patterns over time
2. **Correlation**: Connect performance to system changes
3. **Percentile Focus**: P95/P99 latencies often more important than averages
4. **Error Classification**: Understand failure modes
5. **Capacity Planning**: Plan for 2-3x expected peak load

## Contributing

To extend the load testing framework:

1. Add new traffic patterns in `generate_*_load` functions
2. Implement protocol handlers in `send_*_request` functions
3. Create scenario templates in `get_scenario_config`
4. Add metrics in `calculate_metrics` function
5. Extend test runner with new test suites

## License

Part of the ErlMCP project. See main project LICENSE file.
