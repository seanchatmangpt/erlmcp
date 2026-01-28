# erlmcp_observability - Observability & Metrics

**Version:** 2.0.0
**Application:** erlmcp_observability
**Modules:** 9

Production observability for erlmcp - OpenTelemetry traces, Prometheus metrics, deterministic receipt chains, and health monitoring.

## Overview

erlmcp_observability provides comprehensive monitoring:
- **OpenTelemetry** - Distributed tracing with OTLP export to Jaeger/Honeycomb/Datadog
- **Metrics** - Performance counters, histograms, Prometheus export
- **Receipt Chains** - SHA-256 hash chains for deterministic build verification
- **Health Checks** - Process monitoring, registry health, transport status
- **Dashboards** - Real-time observability UI

## Observability Modules (9 total)

### Metrics
- **erlmcp_metrics.erl** - Metrics collection and export (Prometheus-compatible)
- **erlmcp_routing_metrics.erl** - Registry routing and throughput metrics

### OpenTelemetry
- **erlmcp_otel.erl** - OpenTelemetry integration (OTLP exporter)
- **erlmcp_tracer.erl** - Span creation, context propagation, sampling

### Receipt Chains
- **erlmcp_receipt_chain.erl** - Deterministic SHA-256 hash chains
- **erlmcp_receipt_storage.erl** - Receipt persistence (file/ETS backends)
- **erlmcp_receipt_validator.erl** - Chain verification and integrity checks

### Application
- **erlmcp_observability_app.erl** - OTP application callback
- **erlmcp_observability_sup.erl** - Observability supervisor

## Dependencies

| Library | Version | Purpose |
|---------|---------|---------|
| **opentelemetry_api** | 1.5.0 | OpenTelemetry API |
| **opentelemetry** | 1.7.0 | OpenTelemetry SDK |
| **opentelemetry_exporter** | 1.10.0 | OTLP HTTP/gRPC exporter |
| **erlmcp_core** | 2.0.0 | Core MCP protocol |

## Usage Examples

### OpenTelemetry Tracing

```erlang
%% Configure OTEL exporter
application:set_env(erlmcp_observability, otel_defaults, #{
    service_name => <<"my-mcp-server">>,
    exporter => {otlp, #{
        endpoint => "http://localhost:4318",  % Jaeger OTLP endpoint
        protocol => http_protobuf
    }},
    sampling_rate => 0.1  % Sample 10% of traces
}),

%% Start observability
application:ensure_all_started(erlmcp_observability),

%% Automatic tracing (via erlmcp_otel)
{ok, Client} = erlmcp_client:call_tool(Client, <<"greet">>, Args),
%% → Creates span "erlmcp.tool.call" with attributes:
%%   - tool.name = "greet"
%%   - tool.args = <JSON>
%%   - latency_us = <duration>

%% Manual span creation
erlmcp_tracer:with_span(<<"custom_operation">>, fun() ->
    %% Your code here
    erlmcp_tracer:add_event(<<"checkpoint_reached">>, #{step => 1}),
    {ok, result}
end).
```

**OTEL Exporters:**
- **Jaeger:** `http://localhost:4318` (OTLP HTTP)
- **Honeycomb:** `https://api.honeycomb.io` (requires API key)
- **Datadog:** `http://localhost:4318` (Datadog Agent OTLP)

### Metrics Collection

```erlang
%% Counter - increment on events
erlmcp_metrics:inc(request_count, 1),
erlmcp_metrics:inc({request_count, tool_call}, 1),

%% Histogram - latency tracking
StartTime = erlang:monotonic_time(microsecond),
%% ... do work ...
Duration = erlang:monotonic_time(microsecond) - StartTime,
erlmcp_metrics:histogram(tool_latency_us, Duration),

%% Gauge - current state
erlmcp_metrics:gauge(active_connections, 42).
```

**Prometheus Export:**

Metrics exposed at `http://localhost:9090/metrics`:

```prometheus
# TYPE erlmcp_request_count counter
erlmcp_request_count{method="tool_call"} 1234

# TYPE erlmcp_tool_latency_us histogram
erlmcp_tool_latency_us_bucket{le="1000"} 450
erlmcp_tool_latency_us_bucket{le="5000"} 980
erlmcp_tool_latency_us_sum 45230
erlmcp_tool_latency_us_count 1000
```

### Receipt Chains (Deterministic Builds)

```erlang
%% Initialize receipt chain
{ok, ChainPid} = erlmcp_receipt_chain:start_link(#{
    algorithm => sha256,
    storage => file,
    chain_file => "/var/lib/erlmcp/receipts.dat"
}),

%% Append build receipt
{ok, ReceiptHash} = erlmcp_receipt_chain:append(#{
    operation => compile,
    modules => 94,
    timestamp => erlang:system_time(millisecond),
    erlang_version => <<"OTP-25">>,
    rebar3_version => <<"3.22.1">>,
    git_sha => <<"a1b2c3d4">>
}),

%% ReceiptHash = <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>

%% Verify chain integrity
{ok, valid} = erlmcp_receipt_validator:verify_chain(ChainPid),

%% Export for auditing
{ok, ChainData} = erlmcp_receipt_chain:export("/tmp/receipts_export.json").
```

**Use Cases:**
- **Reproducible Builds** - Verify compilation determinism
- **Audit Trails** - Immutable operation history
- **Compliance** - TCPS quality gates require receipt chains
- **Debug** - Trace deployment artifacts to source builds

### Health Monitoring

```erlang
%% Check system health
Health = erlmcp_health:check_all(),
%%
%% #{
%%   registry => ok,
%%   transports => #{tcp => ok, http => ok},
%%   metrics => ok,
%%   otel_exporter => ok,
%%   receipt_chain => ok
%% }

%% Health check endpoint (HTTP)
%% GET http://localhost:9091/health
%% → 200 OK {"status": "healthy", "checks": {...}}
```

## Configuration

Default config in `erlmcp_observability.app.src`:

```erlang
{otel_defaults, #{
    service_name => <<"erlmcp">>,
    exporter => {otlp, #{
        endpoint => "http://localhost:4318",
        protocol => http_protobuf
    }},
    sampling_rate => 1.0  % 100% sampling (reduce in prod)
}},
{metrics_defaults, #{
    interval => 60000,     % Scrape interval (ms)
    backend => simple      % or prometheus
}},
{receipt_defaults, #{
    hash_algorithm => sha256,
    storage_backend => file,
    verification_on_read => false
}}
```

Override in `sys.config`:

```erlang
[
    {erlmcp_observability, [
        {otel_defaults, #{
            service_name => <<"prod-mcp-server">>,
            exporter => {otlp, #{
                endpoint => "https://api.honeycomb.io",
                headers => [{"x-honeycomb-team", "YOUR_API_KEY"}]
            }},
            sampling_rate => 0.01  % 1% sampling in production
        }}
    ]}
].
```

## Build & Test

```bash
# Compile
rebar3 compile --app erlmcp_observability

# Unit tests
rebar3 eunit --app erlmcp_observability

# Integration tests (requires OTEL collector)
rebar3 ct --app erlmcp_observability

# Type checking
rebar3 dialyzer --app erlmcp_observability
```

## Performance Impact

**Benchmarked overhead (v1.5.0):**

| Feature | Overhead | Notes |
|---------|----------|-------|
| **OTEL Tracing (sampled)** | 12 μs/span | Negligible at 1% sampling |
| **OTEL Tracing (100%)** | 89 μs/span | Reduces throughput ~8% |
| **Metrics (counter)** | 0.4 μs/op | ETS-based, lock-free |
| **Receipt Chains** | 230 μs/append | File I/O bottleneck |

**Recommendations:**
- Use sampling (1-10%) in production for OTEL
- Async receipt chain appends (non-blocking)
- Prometheus scrape interval: 60s minimum

## Integration Guide

### With Jaeger (OTEL Backend)

```bash
# Run Jaeger with OTLP
docker run -d --name jaeger \
  -p 4318:4318 \
  -p 16686:16686 \
  jaegertracing/all-in-one:latest
```

Configure erlmcp:

```erlang
{otel_defaults, #{
    endpoint => "http://localhost:4318"
}}.
```

View traces at `http://localhost:16686`.

### With Prometheus

```yaml
# prometheus.yml
scrape_configs:
  - job_name: 'erlmcp'
    static_configs:
      - targets: ['localhost:9090']
```

```bash
prometheus --config.file=prometheus.yml
```

View metrics at `http://localhost:9090`.

## Migration from v0.5.x

v2.0.0 moves observability into separate app. Update your code:

```erlang
% Before (v0.5.x - optional module)
-optional_application([opentelemetry]).

% After (v2.0.0 - explicit dependency)
{applications, [erlmcp_core, erlmcp_observability]}.
```

Observability is now **required** for production deployments (TCPS mandates receipt chains).

## See Also

- [erlmcp_core](../erlmcp_core/README.md) - Core MCP protocol
- [tcps_erlmcp](../tcps_erlmcp/README.md) - TCPS quality gates use receipts
- [Architecture](../../docs/architecture.md) - System design
- [OpenTelemetry Docs](https://opentelemetry.io/docs/erlang/) - Official OTEL Erlang guide
