# OpenTelemetry Setup for erlmcp

## Overview

erlmcp now includes OpenTelemetry integration for distributed tracing and observability. This document describes the setup and configuration.

## Dependencies

The following OpenTelemetry packages are included (latest stable versions as of October 2025):

```erlang
{opentelemetry_api, "1.5.0"},
{opentelemetry, "1.7.0"},
{opentelemetry_exporter, "1.10.0"}
```

## Configuration

OpenTelemetry is configured in `config/sys.config`:

```erlang
{opentelemetry, [
    {span_processor, batch},
    {traces_exporter, otlp}
]},

{opentelemetry_exporter, [
    {otlp_protocol, http_protobuf},
    {otlp_endpoint, "http://localhost:4318"}
]}
```

### Configuration Options

#### Span Processor
- `batch` - Batches spans before exporting (recommended for production)
- `simple` - Exports spans immediately (useful for development)

#### Traces Exporter
- `otlp` - Export via OpenTelemetry Protocol
- `none` - Disable exporting (prevents memory overflows if collector not ready)

#### OTLP Protocol
- `http_protobuf` - HTTP/1.1 with protobuf encoding (default)
- `grpc` - gRPC transport

#### Endpoint
Default: `http://localhost:4318`

For production, point to your OpenTelemetry collector or APM backend:
- Jaeger: `http://localhost:4318`
- Honeycomb: `https://api.honeycomb.io:443`
- Custom collector: Your collector URL

## Usage in Code

### Including the Header

```erlang
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
```

**Important**: Use `opentelemetry_api/include/otel_tracer.hrl`, not `opentelemetry/include/otel_tracer.hrl`.

### Using erlmcp_tracing Module

The `erlmcp_tracing` module provides convenient wrappers:

```erlang
% Start a span
SpanCtx = erlmcp_tracing:start_span(<<"operation_name">>),

% Add attributes
erlmcp_tracing:set_attributes(SpanCtx, #{
    <<"key">> => <<"value">>,
    <<"count">> => 42
}),

% End span
erlmcp_tracing:end_span(SpanCtx).
```

### Convenience Function

```erlang
% Automatic span management
Result = erlmcp_tracing:with_span(
    <<"my_operation">>,
    #{<<"user_id">> => UserId},
    fun() ->
        % Your code here
        do_work()
    end
).
```

### Transport-Specific Spans

```erlang
% Transport span
SpanCtx = erlmcp_tracing:start_transport_span(TransportId, Type, #{
    <<"message.size">> => Size
}),

% Server span
SpanCtx = erlmcp_tracing:start_server_span(ServerId, #{
    <<"method">> => Method
}),

% Registry span
SpanCtx = erlmcp_tracing:start_registry_span(Operation).
```

## Running with OpenTelemetry

### Local Development

1. Start an OpenTelemetry collector or Jaeger:

```bash
# Using Docker
docker run -d --name jaeger \
  -p 4318:4318 \
  -p 16686:16686 \
  jaegertracing/all-in-one:latest
```

2. Start erlmcp:

```bash
rebar3 shell
```

3. View traces at http://localhost:16686

### Environment Variables

You can override configuration with environment variables:

```bash
export OTEL_EXPORTER_OTLP_ENDPOINT=https://api.example.com:4318
export OTEL_EXPORTER_OTLP_PROTOCOL=http_protobuf
export OTEL_EXPORTER_OTLP_HEADERS="x-api-key=your-key"

rebar3 shell
```

## Verification

To verify OpenTelemetry is working:

1. Compilation should succeed without errors
2. Applications start without crashes
3. Spans are exported to your collector
4. No `undef` errors related to `otel_tracer`

## Troubleshooting

### Include File Not Found

**Error**: `can't find include lib "opentelemetry/include/otel_tracer.hrl"`

**Solution**: Use `opentelemetry_api/include/otel_tracer.hrl` instead.

### Exporter Initialization Failed

**Error**: `OTLP exporter failed to initialize with exception error:{badmatch, {error, inets_not_started}}`

**Solution**: Ensure `inets` is in the applications list in `erlmcp.app.src`:

```erlang
{applications, [
    kernel,
    stdlib,
    inets,  % Required for HTTP exporter
    ...
    opentelemetry_api,
    opentelemetry
]},
```

### No Spans Appearing

1. Check collector is running: `curl http://localhost:4318/v1/traces`
2. Check exporter is not disabled: `traces_exporter` should not be `none`
3. Check application started: `application:which_applications()` should list `opentelemetry`

## References

- [OpenTelemetry Erlang SDK](https://github.com/open-telemetry/opentelemetry-erlang)
- [OpenTelemetry Documentation](https://opentelemetry.io/docs/languages/erlang/)
- [Hex Packages](https://hex.pm/packages/opentelemetry/versions)
- [OTLP Specification](https://opentelemetry.io/docs/specs/otlp/)

## Version History

- **v0.6.0** (2026-01-27): Initial OpenTelemetry integration
  - Added opentelemetry_api 1.5.0
  - Added opentelemetry 1.7.0
  - Added opentelemetry_exporter 1.10.0
  - Fixed include paths in all source files
  - Configured OTLP HTTP exporter
