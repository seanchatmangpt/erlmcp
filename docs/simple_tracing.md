# ErlMCP Simple Tracing

A lightweight tracing implementation using only Erlang's logger, without OpenTelemetry dependencies.

## Features

- **Simple API**: Start traces, add spans, automatic timing
- **Logger Integration**: All trace data goes through Erlang logger with metadata
- **JSON Export**: Export traces as JSON for external analysis
- **Convenience Functions**: Pre-built wrappers for common operations
- **Error Handling**: Automatic error capture and reporting
- **Performance**: Fast and lightweight, no external dependencies beyond jsx

## API Overview

### Core Functions

```erlang
%% Start a new trace
TraceId = erlmcp_simple_trace:start_trace(<<"trace_name">>).
TraceId = erlmcp_simple_trace:start_trace(<<"trace_name">>, #{metadata => value}).

%% Add spans to trace  
SpanId = erlmcp_simple_trace:add_span(<<"operation">>, #{attributes}).

%% End trace (automatically calculates durations)
erlmcp_simple_trace:end_trace(TraceId).

%% Get current trace context
Context = erlmcp_simple_trace:current_trace().

%% Export as JSON
JsonBinary = erlmcp_simple_trace:format_trace_json(Context).
```

### Convenience Functions

```erlang
%% Trace transport operations
Result = erlmcp_simple_trace:trace_transport_operation(
    <<"send_message">>,
    stdio,
    fun() ->
        %% Your transport code here
        {ok, sent}
    end
).

%% Trace server operations  
Result = erlmcp_simple_trace:trace_server_operation(
    <<"handle_request">>,
    server_id,
    fun() ->
        %% Your server code here
        {ok, processed}
    end
).

%% Trace registry operations
Result = erlmcp_simple_trace:trace_registry_operation(
    <<"lookup_tool">>,
    fun() ->
        %% Your registry code here
        {ok, found}
    end
).

%% Trace message processing
Result = erlmcp_simple_trace:trace_message_processing(
    <<"tools/list">>,
    <<"msg_123">>,
    1024,
    fun() ->
        %% Your message processing code here
        {ok, [tool1, tool2]}
    end
).
```

## Usage Examples

### Basic Tracing

```erlang
%% Start trace
TraceId = erlmcp_simple_trace:start_trace(<<"request_handler">>, #{
    user => <<"client_123">>,
    version => <<"1.0">>
}),

%% Add operation spans
SpanId1 = erlmcp_simple_trace:add_span(<<"validate_input">>, #{
    component => validation,
    input_size => 256
}),

timer:sleep(5), % Simulate work

SpanId2 = erlmcp_simple_trace:add_span(<<"process_data">>, #{
    component => processor,
    algorithm => <<"v2">>
}),

timer:sleep(10), % Simulate work

%% End trace (logs complete summary)
erlmcp_simple_trace:end_trace(TraceId).
```

### Auto-tracing Operations

```erlang
%% Automatically creates trace if none exists
Result = erlmcp_simple_trace:trace_transport_operation(
    <<"send_request">>,
    http,
    fun() ->
        %% Transport implementation
        httpc:request(get, {"http://example.com", []}, [], [])
    end
).
```

### Error Handling

```erlang
%% Errors are automatically captured in traces
try
    erlmcp_simple_trace:trace_server_operation(
        <<"risky_operation">>,
        worker_server,
        fun() ->
            error(database_connection_failed)
        end
    )
catch
    error:database_connection_failed ->
        %% Error details are automatically added to trace
        ok
end.
```

## JSON Export Format

The JSON export includes:

```json
{
  "trace_id": "a1b2c3d4...",
  "start_time": 1234567890123456,
  "end_time": 1234567890123500,
  "metadata": {
    "trace_name": "request_handler",
    "user": "client_123"
  },
  "spans": [
    {
      "span_id": "e5f6g7h8...",
      "parent_id": null,
      "operation_name": "validate_input",
      "start_time": 1234567890123460,
      "end_time": 1234567890123465,
      "duration_us": 5000,
      "status": "ok",
      "attributes": {
        "component": "validation",
        "input_size": 256
      },
      "events": []
    }
  ]
}
```

## Logger Integration

All trace activity is logged with structured metadata:

```erlang
%% Trace start
?LOG_INFO("Trace started", #{
    trace_id => TraceId,
    trace_name => Name,
    timestamp => StartTime,
    metadata => Metadata
})

%% Span operations
?LOG_DEBUG("Span started", #{
    trace_id => TraceId,
    span_id => SpanId,
    parent_span_id => ParentSpanId,
    operation_name => OperationName,
    attributes => Attributes
})

%% Trace completion
?LOG_INFO("Trace completed", #{
    trace_id => TraceId,
    total_duration_us => TotalDuration,
    span_count => SpanCount,
    error_count => ErrorCount,
    trace_json => TraceJson
})
```

## Performance Characteristics

- **Low Overhead**: Uses process dictionary for context storage
- **Fast JSON Export**: Optimized JSON generation with jsx
- **Memory Efficient**: Automatic cleanup on trace end
- **Concurrent Safe**: Each process has its own trace context

## Integration with ErlMCP

The simple tracer integrates seamlessly with ErlMCP components:

- **Transport Layer**: Trace message send/receive operations
- **Server Processes**: Trace request handling
- **Registry Operations**: Trace tool lookups and registrations
- **JSON-RPC**: Trace method calls and responses

## Migration from OpenTelemetry

This simple tracer provides a drop-in replacement for basic OpenTelemetry functionality without the complexity:

| OpenTelemetry | Simple Trace |
|---------------|--------------|
| `otel_tracer:start_span()` | `add_span()` |
| `otel_span:end_span()` | Automatic on `end_trace()` |
| `otel_span:set_attributes()` | Pass to `add_span()` |
| Complex setup | Just start using |

## Testing

Run the included tests:

```bash
# Basic functionality test
erl -pa _build/default/lib/erlmcp/ebin -pa _build/default/lib/jsx/ebin \
    -noshell -eval "
TraceId = erlmcp_simple_trace:start_trace(<<\"test\">>),
SpanId = erlmcp_simple_trace:add_span(<<\"op\">>, #{test => true}),
erlmcp_simple_trace:end_trace(TraceId),
io:format(\"Success!~n\"),
halt()."
```

## Configuration

Configure logger level to see trace output:

```erlang
%% In sys.config
[{logger, [
    {level, info}, % Set to debug for span-level details
    {handlers, [
        {default, logger_std_h, #{
            formatter => {logger_formatter, #{
                depth => 10,
                chars_limit => 2048,
                single_line => false
            }}
        }}
    ]}
]}].
```