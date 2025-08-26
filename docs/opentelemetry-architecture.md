# OpenTelemetry Architecture for ErlMCP

## Overview

The OpenTelemetry implementation for ErlMCP provides comprehensive distributed tracing, metrics collection, and observability across all protocol operations. This architecture ensures complete visibility into system behavior through spans, traces, and performance metrics.

## Core Architecture Components

### 1. Core OpenTelemetry Module (`erlmcp_otel.erl`)

The main OpenTelemetry infrastructure module providing:
- **Span Management**: Creation, lifecycle, and context management
- **Trace Context Propagation**: Cross-process and cross-service tracing
- **Error Recording**: Automatic error capture with stack traces
- **Baggage Management**: Request correlation across operations
- **Exporter Configuration**: Multiple backend support (Jaeger, Zipkin, Prometheus)

### 2. Integration Layer (`erlmcp_otel_integration.erl`)

Integration utilities for existing ErlMCP components:
- **Transport Tracing**: Automatic instrumentation of all transport operations
- **JSON-RPC Tracing**: Request/response lifecycle tracking
- **Method Execution**: Tool and prompt execution monitoring
- **Registry Operations**: Resource registration and lookup tracing

### 3. Test Suite (`erlmcp_otel_SUITE.erl`)

Comprehensive test coverage validating:
- Span creation and lifecycle management
- Parent-child span relationships
- Context propagation mechanisms
- Error recording and status management
- Integration with ErlMCP components
- Performance metrics collection

## Distributed Tracing Strategy

### Span Hierarchy

```
Transport Receive Span
├── JSON-RPC Request Span
│   ├── Registry Lookup Span
│   ├── Method Execution Span
│   │   ├── Tool Call Span
│   │   └── Resource Access Span
│   └── JSON-RPC Response Span
└── Transport Send Span
```

### Trace Context Propagation

The system implements W3C Trace Context specification:
- **traceparent**: Version-trace_id-span_id-flags format
- **tracestate**: Additional vendor-specific trace information
- **baggage**: Cross-cutting correlation data

### Span Attributes

All spans include standardized attributes:

#### Common Attributes
- `service.name`: Service identifier
- `service.version`: Application version
- `mcp.version`: Protocol version
- `mcp.protocol_version`: Specific protocol version
- `erlang.node`: Node identifier
- `erlang.pid`: Process identifier

#### Transport Attributes
- `transport.type`: stdio, tcp, http, websocket
- `transport.operation`: send, receive, connect, disconnect
- `message.id`: Request/response correlation ID
- `message.size`: Message payload size

#### RPC Attributes
- `rpc.system`: "jsonrpc"
- `rpc.service`: "mcp"
- `rpc.method`: MCP method name
- `rpc.request_id`: Unique request identifier
- `rpc.response.success`: Success/failure indicator

#### Method Attributes
- `method.name`: Called method name
- `method.execution.phase`: execute, validate, prepare
- `tool.name`: Tool identifier (for tool calls)
- `resource.uri`: Resource identifier (for resource operations)

#### Error Attributes
- `error`: Boolean error indicator
- `error.type`: Exception class (error, throw, exit)
- `error.message`: Human-readable error description
- `error.stacktrace`: Full stack trace

#### Performance Attributes
- `duration_ns`: Operation duration in nanoseconds
- `process.memory`: Memory usage
- `process.message_queue_len`: Message queue length
- `process.reductions`: CPU reductions count

## Sampling Strategies

### Always On (Testing)
- All spans are sampled and recorded
- Used for development and testing environments
- Provides complete trace coverage

### Trace ID Ratio (Production)
- Configurable sampling rate (0.0 to 1.0)
- Based on trace ID hash for consistency
- Balances observability with performance

### Parent-Based
- Child spans follow parent sampling decision
- Maintains trace completeness
- Reduces sampling decision overhead

## Exporter Configuration

### Jaeger
```erlang
JaegerConfig = #{
    endpoint => <<"http://jaeger:14268/api/traces">>,
    service_name => <<"erlmcp-server">>,
    tags => #{
        <<"environment">> => <<"production">>,
        <<"version">> => <<"1.0.0">>
    }
}
```

### Zipkin
```erlang
ZipkinConfig = #{
    endpoint => <<"http://zipkin:9411/api/v2/spans">>,
    service_name => <<"erlmcp-server">>
}
```

### Prometheus (Metrics)
```erlang
PrometheusConfig = #{
    port => 9090,
    endpoint => <<"/metrics">>,
    namespace => <<"erlmcp">>,
    metrics => [duration, throughput, error_rate]
}
```

### OTLP (OpenTelemetry Protocol)
```erlang
OtlpConfig = #{
    endpoint => <<"http://otel-collector:4317">>,
    protocol => grpc,
    compression => gzip,
    headers => #{
        <<"x-api-key">> => <<"your-api-key">>
    }
}
```

## Usage Examples

### Basic Initialization

```erlang
%% Initialize OpenTelemetry
Config = #{
    service_name => <<"erlmcp-server">>,
    service_version => <<"0.5.0">>,
    exporters => [jaeger, prometheus],
    sampling => trace_id_ratio,
    sampling_rate => 0.1,
    resource_attributes => #{
        <<"environment">> => <<"production">>,
        <<"datacenter">> => <<"us-east-1">>
    }
},

ok = erlmcp_otel:init(Config).
```

### Manual Span Management

```erlang
%% Create parent span
ParentSpan = erlmcp_otel:start_span(<<"mcp.tools.call">>, #{
    <<"tool.name">> => <<"calculator">>,
    <<"tool.operation">> => <<"add">>
}),

%% Create child span
ChildSpan = erlmcp_otel:start_span(<<"calculator.execute">>, #{
    <<"operation">> => <<"add">>,
    <<"args.count">> => 2
}, ParentSpan),

try
    Result = perform_calculation(Args),
    erlmcp_otel:add_attributes(ChildSpan, #{
        <<"result.value">> => Result,
        <<"execution.success">> => true
    }),
    erlmcp_otel:end_span(ChildSpan),
    erlmcp_otel:end_span(ParentSpan),
    Result
catch
    Class:Reason:Stacktrace ->
        erlmcp_otel:record_error(ChildSpan, {Class, Reason, Stacktrace}),
        erlmcp_otel:record_error(ParentSpan, {Class, Reason, Stacktrace}),
        erlmcp_otel:end_span(ChildSpan),
        erlmcp_otel:end_span(ParentSpan),
        erlang:raise(Class, Reason, Stacktrace)
end.
```

### Automatic Span Management

```erlang
%% Execute function within span
Result = erlmcp_otel:with_span(
    <<"resource.read">>,
    #{
        <<"resource.uri">> => Uri,
        <<"resource.type">> => Type
    },
    fun() ->
        %% Baggage is automatically inherited
        UserId = erlmcp_otel:get_baggage(<<"user.id">>),
        RequestId = erlmcp_otel:get_baggage(<<"request.id">>),
        
        %% Perform resource operation
        read_resource(Uri, Type, UserId, RequestId)
    end
).
```

### Context Propagation

```erlang
%% Extract context for HTTP headers
Headers = erlmcp_otel:propagate_context(CurrentSpan),
%% Headers = #{
%%     <<"traceparent">> => <<"00-trace_id-span_id-01">>,
%%     <<"baggage">> => <<"user.id=123,request.id=abc">>
%% }

%% Send HTTP request with tracing headers
HttpResponse = http_client:request(Url, Headers, Body),

%% On receiving side, restore context
IncomingCtx = erlmcp_otel:restore_context(RequestHeaders),
ChildSpan = erlmcp_otel:start_span(<<"handle.request">>, #{}, IncomingCtx).
```

### Transport Integration

```erlang
%% Instrument transport module
InstrumentedOpts = erlmcp_otel_integration:instrument_transport(
    erlmcp_transport_stdio,
    #{owner => self(), test_mode => false}
),

%% Transport operations are automatically traced
{ok, Transport} = erlmcp_transport_stdio:init(InstrumentedOpts),
ok = erlmcp_transport_stdio:send(Transport, JsonMessage).
```

## Performance Considerations

### Span Creation Overhead
- Minimal CPU impact (~10-50 microseconds per span)
- Memory usage: ~1-2KB per active span
- Batch processing reduces I/O overhead

### Sampling Impact
- 10% sampling reduces overhead by 90%
- No impact on trace completeness for sampled traces
- Parent-based sampling maintains trace integrity

### Export Batching
- Default batch size: 512 spans
- Default batch timeout: 5 seconds
- Configurable based on throughput requirements

### Resource Usage
- Memory: ~1MB per 1000 active spans
- CPU: <1% overhead with 10% sampling
- Network: ~100 bytes per exported span

## Monitoring and Alerting

### Key Metrics
- **Span Export Rate**: Spans exported per second
- **Trace Completeness**: Percentage of complete traces
- **Error Rate**: Percentage of spans with errors
- **Latency Distribution**: P50, P95, P99 latencies

### Alerting Thresholds
- Export failures > 1%
- Trace drops > 0.1%
- Memory usage > 100MB
- Export latency > 10 seconds

### Health Checks
```erlang
%% Check OpenTelemetry health
case erlmcp_otel:get_tracer_provider() of
    undefined -> {error, not_initialized};
    Provider -> 
        ExportRate = get_export_rate(Provider),
        MemoryUsage = get_memory_usage(Provider),
        case {ExportRate > 0, MemoryUsage < 104857600} of
            {true, true} -> {ok, healthy};
            _ -> {warning, performance_degraded}
        end
end.
```

## Integration with ErlMCP Components

### Server Integration
- All method calls automatically traced
- Capability negotiation spans
- Subscription management tracing
- Resource update notifications

### Client Integration  
- Request/response correlation
- Timeout and retry tracing
- Connection pool monitoring
- Circuit breaker state changes

### Transport Integration
- Message framing spans
- Connection lifecycle events
- Error and reconnection handling
- Bandwidth and latency metrics

### Registry Integration
- Resource registration/deregistration
- Lookup operations and cache hits
- Update propagation
- Conflict resolution

## Troubleshooting

### Common Issues

1. **Missing Spans**: Check sampling configuration and parent context
2. **High Memory Usage**: Adjust batch size and export frequency
3. **Export Failures**: Verify exporter endpoints and authentication
4. **Context Loss**: Ensure proper context propagation in async operations

### Debug Configuration
```erlang
DebugConfig = #{
    service_name => <<"erlmcp-debug">>,
    exporters => [console],
    sampling => always_on,
    debug_mode => true,
    log_level => debug
}.
```

### Verification Commands
```erlang
%% Check current configuration
Config = erlang:get(erlmcp_otel_config),

%% Inspect active spans
ActiveSpans = erlang:get(erlmcp_otel_active_spans),

%% Validate context propagation
Headers = erlmcp_otel:propagate_context(erlang:get(erlmcp_otel_current_context)),
RestoredCtx = erlmcp_otel:restore_context(Headers),
TraceId = maps:get(trace_id, RestoredCtx).
```

## Future Enhancements

### Planned Features
- Automatic metric generation from spans
- Custom span processors for business logic
- Integration with Erlang's built-in tracing
- Dynamic sampling adjustment
- Span correlation with logs

### Performance Optimizations
- SIMD-accelerated trace ID generation
- Memory-mapped span storage
- Compressed batch exports
- Adaptive batching algorithms

### Extended Integrations
- Database query tracing
- External service call tracing
- File system operation tracing
- Network socket monitoring

This OpenTelemetry implementation provides comprehensive observability for ErlMCP, enabling detailed monitoring, debugging, and performance analysis of all protocol operations.