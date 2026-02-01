# Monitoring Architecture Guide

**Version:** 2.1.0  
**Last Updated:** January 31, 2026

## Overview

erlmcp provides comprehensive monitoring architecture following Toyota Production System principles: Andon (visible alerts), Poka-Yoke (mistake-proofing), Jidoka (built-in quality), and Kaizen (continuous improvement).

## System Monitoring Topology

```mermaid
flowchart TB
    subgraph DataPlane["Data Plane (erlmcp Cluster)"]
        subgraph Nodes["Node 1, 2, 3, ..."]
            N1[erlmcp_server<br/>MCP Protocol]
            N2[erlmcp_transport<br/>TCP/HTTP/WS/SSE]
            N3[erlmcp_registry<br/>gproc routing]
            N4[erlmcp_session<br/>State management]
        end
        
        subgraph Observability["Observability Stack"]
            O1[erlmcp_metrics<br/>Collection]
            O2[erlmcp_tracing<br/>Distributed tracing]
            O3[erlmcp_health_monitor<br/>Andon system]
            O4[erlmcp_chaos<br/>Failure injection]
        end
    end
    
    subgraph ControlPlane["Control Plane (Monitoring Stack)"]
        subgraph Collection["Metrics Collection"]
            M1[erlmcp_metrics_aggregator<br/>Time-series windowing]
            M2[erlmcp_metrology_validator<br/>Unit validation]
        end
        
        subgraph Export["Export Layer"]
            E1[erlmcp_metrics_server<br/>HTTP /metrics]
            E2[erlmcp_otel<br/>OpenTelemetry]
            E3[erlmcp_dashboard_server<br/>Web UI]
        end
        
        subgraph Alerting["Alerting Layer"]
            A1[Andon Alerts<br/>Real-time]
            A2[Prometheus<br/>Rule evaluation]
            A3[Datadog<br/>Smart alerts]
        end
    end
    
    subgraph Backends["Observability Backends"]
        B1[(Prometheus)]
        B2[(Datadog)]
        B3[(Jaeger)]
        B4[(Grafana)]
    end
    
    N1 --> O1
    N2 --> O1
    N3 --> O1
    N4 --> O1
    
    O1 --> M1
    O2 --> E2
    O3 --> E3
    O4 --> O3
    
    M1 --> M2
    M2 --> E1
    M2 --> E2
    
    E1 --> B1
    E2 --> B2
    E2 --> B3
    E3 --> B4
    
    E3 --> A1
    B1 --> A2
    B2 --> A3
    
    style DataPlane fill:#e1f5fe
    style Observability fill:#fff3e0
    style ControlPlane fill:#f3e5f5
    style Backends fill:#eceff1
```

## Monitoring Layers

### Layer 1: Instrumentation (Data Collection)

```mermaid
graph TD
    subgraph "Instrumentation Points"
        A[erlmcp_server<br/>Tool calls<br/>Resource ops<br/>Prompt lists]
        B[erlmcp_client<br/>Requests<br/>Responses<br/>Correlation]
        C[erlmcp_transport<br/>Send/Receive<br/>Connections<br/>Bytes]
        D[erlmcp_registry<br/>Lookups<br/>Registrations<br/>Routing]
        E[erlmcp_session<br/>Lifecycle<br/>State changes<br/>Failover]
    end
    
    subgraph "Metric Types"
        F[Counters<br/>Monotonic events]
        G[Histograms<br/>Distributions]
        G[Gauges<br/>Point-in-time]
    end
    
    subgraph "Trace Spans"
        H[Request span<br/>Parent context]
        I[Operation spans<br/>Child spans]
        J[Backend spans<br/>External calls]
    end
    
    A --> F
    A --> G
    A --> H
    B --> F
    B --> G
    B --> I
    C --> F
    C --> G
    C --> J
    
    style A fill:#E1F5FE
    style B fill:#E1F5FE
    style C fill:#E1F5FE
    style D fill:#E1F5FE
    style E fill:#E1F5FE
    style F fill:#C8E6C9
    style G fill:#FFF9C4
    style H fill:#F3E5F5
    style I fill:#F3E5F5
    style J fill:#F3E5F5
```

### Layer 2: Aggregation (Time-Series Processing)

```mermaid
flowchart LR
    subgraph "Aggregation Pipeline"
        A[Raw Metrics<br/>Microsecond precision] --> B[1-Second Window<br/>Real-time]
        B --> C[5-Second Window<br/>Standard monitoring]
        C --> D[1-Minute Window<br/>Alerting]
        D --> E[5-Minute Window<br/>Trend analysis]
        E --> F[1-Hour Window<br/>Capacity planning]
    end
    
    subgraph "Computations"
        G[Sum<br/>Counters]
        H[Percentiles<br/>p50/p95/p99/p999]
        I[Rates<br/>ops/sec]
        J[Averages<br/>CPU/Memory]
    end
    
    B --> G
    B --> H
    C --> I
    D --> J
    
    style A fill:#E1F5FE
    style B fill:#C8E6C9
    style C fill:#A5D6A7
    style D fill:#81C784
    style E fill:#66BB6A
    style F fill:#4CAF50
    style G fill:#FFF9C4
    style H fill:#FFCC80
    style I fill:#FFEBEE
    style J fill:#F3E5F5
```

### Layer 3: Validation (Metrology Compliance)

```mermaid
graph TB
    subgraph "Validation Pipeline"
        A[Incoming Metric] --> B{Required<br/>Fields Present?}
        B -->|No| C[Reject]
        B -->|Yes| D{Canonical<br/>Units?}
        D -->|No| C
        D -->|Yes| E{Valid<br/>Scope?}
        E -->|No| C
        E -->|Yes| F{Precision<br/>Declared?}
        F -->|No| C
        F -->|Yes| G[Accept Metric]
    end
    
    subgraph "Enforcement Actions"
        H[Log Violation]
        I[Increment Counter]
        J[Alert if Threshold]
    end
    
    C --> H
    H --> I
    I --> J
    
    style A fill:#E1F5FE
    style B fill:#FFF9C4
    style D fill:#FFF9C4
    style E fill:#FFF9C4
    style F fill:#FFF9C4
    style C fill:#FFCDD2
    style G fill:#C8E6C9
    style H fill:#FFEBEE
    style I fill:#F3E5F5
    style J fill:#FFCDD2
```

### Layer 4: Export (Observability Backends)

```mermaid
flowchart TB
    subgraph "Export Destinations"
        A[erlmcp_metrics_server<br/>HTTP /metrics] --> B[Prometheus<br/>Scrape]
        C[erlmcp_otel<br/>OTLP] --> D[Datadog<br/>APM]
        C --> E[Jaeger<br/>Tracing]
        C --> F[Honeycomb<br/>Analytics]
        G[erlmcp_dashboard_server<br/>Web UI] --> H[Grafana<br/>Dashboards]
    end
    
    subgraph "Data Formats"
        I[Prometheus<br/>Text format]
        J[OTLP<br/>Protobuf]
        K[JSON<br/>HTTP API]
        L[HTML<br/>Dashboard]
    end
    
    B --> I
    D --> J
    E --> J
    F --> J
    H --> K
    G --> L
    
    style A fill:#E1F5FE
    style C fill:#E1F5FE
    style G fill:#E1F5FE
    style B fill:#C8E6C9
    style D fill:#A5D6A7
    style E fill:#81C784
    style F fill:#66BB6A
    style H fill:#4CAF50
    style I fill:#FFF9C4
    style J fill:#FFCC80
    style K fill:#FFEBEE
    style L fill:#F3E5F5
```

## Alerting Architecture (Andon System)

### Andon Alert Flow

```mermaid
sequenceDiagram
    participant M as erlmcp_metrics
    participant A as erlmcp_metrics_aggregator
    participant H as erlmcp_health_monitor
    participant D as erlmcp_dashboard_server
    participant O as Operator

    M->>A: Record metric (latency_p99_us = 15ms)
    A->>A: Aggregate 5-second window
    A->>H: Health check (every 30s)
    H->>H: Compare to threshold (10ms)
    H->>H: Threshold exceeded!
    H->>D: Trigger alert
    D->>D: Update /andon endpoint
    D->>O: Visual indicator (red light)
    H->>H: Log incident
    H->>H: Send notification
    
    Note over O: Operator investigates<br/>and resolves issue
    
    O->>H: Issue resolved
    H->>D: Clear alert
    D->>O: Visual indicator (green)
```

### Alert Hierarchy

```mermaid
graph TD
    subgraph "Alert Severity Levels"
        CRITICAL["Critical (P0)<br/>System down<br/>Immediate action<br/>Page on-call"]
        HIGH["High (P1)<br/>Degraded performance<br/>Action within 5min<br/>Send notification"]
        MEDIUM["Medium (P2)<br/>Elevated metrics<br/>Investigate within 1hr<br/>Create ticket"]
        LOW["Low (P3)<br/>Trend detected<br/>Review within 24hr<br/>Log only"]
    end
    
    CRITICAL --> HIGH
    HIGH --> MEDIUM
    MEDIUM --> LOW
    
    style CRITICAL fill:#FFCDD2
    style HIGH fill:#FFCC80
    style MEDIUM fill:#FFF9C4
    style LOW fill:#C8E6C9
```

### Alert Configuration

```erlang
{erlmcp_health_monitor, [
    {check_interval, 30000},  % 30 seconds
    
    {thresholds, #{
        % Critical thresholds
        latency_p99_us_critical => 50000000,  % 50 seconds
        error_rate_5m_critical => 0.10,  % 10%
        memory_heap_mib_per_conn_critical => 200,
        
        % Warning thresholds
        latency_p99_us_warning => 10000000,  % 10 seconds
        error_rate_5m_warning => 0.05,  % 5%
        memory_heap_mib_per_conn_warning => 100,
        
        % Info thresholds
        throughput_msg_per_s_info => 400000  % Below baseline
    }},
    
    {alert_handlers, [
        {erlmcp_dashboard_server, alert},
        {logger, warning},
        {otel, trace},
        {webhook, "https://alerts.example.com/webhook"}
    ]}
]}.
```

## Distributed Tracing Architecture

### Trace Propagation

```mermaid
flowchart LR
    subgraph "Trace Context Flow"
        A[Client Request<br/>trace_id: abc123] --> B[erlmcp_server<br/>span: request]
        B --> C[erlmcp_json_rpc<br/>span: decode]
        C --> D[erlmcp_registry<br/>span: lookup]
        D --> E[erlmcp_tool<br/>span: execute]
        E --> F[External API<br/>span: http_request]
        F --> G[erlmcp_json_rpc<br/>span: encode]
        G --> H[Response<br/>trace_id: abc123]
    end
    
    subgraph "Span Relationships"
        I[Root Span<br/>request]
        J[Child Spans<br/>decode, lookup,<br/>execute, encode]
        K[Nested Span<br/>http_request]
    end
    
    I --> J
    J --> K
    
    style A fill:#E1F5FE
    style H fill:#C8E6C9
    style I fill:#F3E5F5
    style J fill:#FFF9C4
    style K fill:#FFCC80
```

### Trace Sampling Strategy

```mermaid
pie title "Trace Sampling Distribution (Production)"
    "Sampled (10%)" : 10
    "Dropped (90%)" : 90
```

**Configuration:**
```erlang
% Production: 10% sampling
{sampler, #{
    type => trace_id_ratio_based,
    ratio => 0.1
}}.

% Development: 100% sampling
{sampler, #{
    type => always_on
}}.

% Staging: 50% sampling
{sampler, #{
    type => trace_id_ratio_based,
    ratio => 0.5
}}.
```

## Dashboard Architecture

### Dashboard Components

```mermaid
graph TB
    subgraph "Dashboard UI"
        A[erlmcp_dashboard_server<br/>HTTP :8080]
        B[Metrics Panel<br/>Throughput/Latency]
        C[Health Panel<br/>System status]
        D[Alert Panel<br/>Andon alerts]
        E[Topology Panel<br/>Process tree]
        F[Trace Panel<br/>Distributed traces]
    end
    
    subgraph "Data Sources"
        G[erlmcp_metrics_aggregator<br/>Real-time metrics]
        H[erlmcp_health_monitor<br/>Health checks]
        I[erlmcp_chaos<br/>Failure injection]
        J[erlmcp_tracing<br/>Trace spans]
    end
    
    A --> B
    A --> C
    A --> D
    A --> E
    A --> F
    
    B --> G
    C --> H
    D --> H
    E --> I
    F --> J
    
    style A fill:#E1F5FE
    style B fill:#C8E6C9
    style C fill:#A5D6A7
    style D fill:#FFCC80
    style E fill:#FFF9C4
    style F fill:#F3E5F5
    style G fill:#E8EAF6
    style H fill:#E8EAF6
    style I fill:#E8EAF6
    style J fill:#E8EAF6
```

### Dashboard Panels

**1. Metrics Panel (Real-time)**
```mermaid
graph LR
    subgraph "Metrics Panel"
        A["Throughput<br/>553K msg/s"]
        B["Latency p99<br/>5.2ms"]
        C["Connections<br/>42K active"]
        D["Memory<br/>1.8GB / 8GB"]
        E["CPU<br/>67% avg"]
        F["Error Rate<br/>0.02%"]
    end
    
    style A fill:#C8E6C9
    style B fill:#C8E6C9
    style C fill:#A5D6A7
    style D fill:#81C784
    style E fill:#66BB6A
    style F fill:#FFCDD2
```

**2. Health Panel (Status)**
```mermaid
graph TB
    subgraph "Health Status"
        A["Node 1<br/>Healthy ✅"]
        B["Node 2<br/>Healthy ✅"]
        C["Node 3<br/>Degraded ⚠️"]
        D["Connections<br/>42K / 50K"]
        E["Memory<br/>Normal"]
        F["CPU<br/>Normal"]
    end
    
    style A fill:#C8E6C9
    style B fill:#C8E6C9
    style C fill:#FFF9C4
    style D fill:#A5D6A7
    style E fill:#C8E6C9
    style F fill:#C8E6C9
```

**3. Alert Panel (Andon)**
```mermaid
graph TB
    subgraph "Active Alerts"
        A["High Latency<br/>Node 3<br/>p99: 15ms<br/>Started: 2m ago"]
        B["Memory Warning<br/>Node 3<br/>180MB / 200MB<br/>Started: 5m ago"]
    end
    
    style A fill:#FFCDD2
    style B fill:#FFF9C4
```

## Chaos Engineering Integration

### Chaos-to-Monitoring Loop

```mermaid
flowchart TB
    subgraph "Chaos Experiment"
        A[Define Scenario] --> B[Execute Chaos]
        B --> C[Inject Failure]
    end
    
    subgraph "Monitoring Response"
        C --> D[erlmcp_health_monitor<br/>Detects degradation]
        D --> E[Trigger Alert]
        E --> F[erlmcp_recovery_manager<br/>Track recovery]
        F --> G[Measure Recovery Time]
    end
    
    subgraph "Learning"
        G --> H{Recovery < 5s?}
        H -->|Yes| I[Record Success]
        H -->|No| J[Investigate Failure]
        J --> K[Improve Resilience]
        K --> A
    end
    
    style A fill:#E1F5FE
    style B fill:#FFF9C4
    style C fill:#FFCDD2
    style D fill:#FFEBEE
    style E fill:#F3E5F5
    style F fill:#E8EAF6
    style G fill:#C8E6C9
    style H fill:#FFF3E0
    style I fill:#A5D6A7
    style J fill:#FFCC80
    style K fill:#81C784
```

## Performance Considerations

### Monitoring Overhead

| Component | CPU Overhead | Memory Overhead | Network Overhead |
|-----------|--------------|-----------------|------------------|
| Metrics Collection | 1-2% | 10-20MB | Minimal |
| Tracing (10% sample) | 2-3% | 50-100MB | 1-2KB/s |
| Health Monitoring | <1% | 5-10MB | Minimal |
| Dashboard Server | <1% | 5-15MB | 100-200KB/s |
| **Total** | **4-6%** | **70-145MB** | **~200KB/s** |

### Optimization Strategies

```mermaid
graph TB
    subgraph "Optimization Techniques"
        A[Sampling<br/>Reduce trace volume]
        B[Aggregation<br/>Longer windows]
        C[Filtering<br/>Drop unneeded labels]
        D[Batching<br/>Export in bulk]
        E[Compression<br/>Reduce network]
    end
    
    subgraph "Trade-offs"
        F[Less detail<br/>More efficiency]
        M[Balanced<br/>Recommended]
        D[More detail<br/>More overhead]
    end
    
    A --> F
    B --> F
    M -.-> A
    M -.-> B
    D -.-> M
    
    style A fill:#C8E6C9
    style B fill:#C8E6C9
    style C fill:#C8E6C9
    style D fill:#C8E6C9
    style E fill:#C8E6C9
    style F fill:#FFF9C4
    style M fill:#A5D6A7
    style D fill:#FFCDD2
```

## Best Practices

### 1. Set Meaningful Thresholds

```erlang
% Based on baseline measurements
{thresholds, #{
    latency_p99_us => 10000000,  % 10x baseline (5.2ms * 10)
    error_rate_5m => 0.05,  % 5% (baseline: 0.02%)
    memory_heap_mib_per_conn => 100  % 2x baseline (45KB * 2)
}}.
```

### 2. Use Alert Hierarchy

```erlang
% Critical: Page on-call
% High: Send notification
% Medium: Create ticket
% Low: Log only
```

### 3. Monitor Observability Overhead

```erlang
% Track metrics system performance
erlmcp_metrics:histogram(<<"otel_span_duration_us">>, SpanDuration).
erlmcp_metrics:gauge(<<"metrics_buffer_size">>, BufferSize).
erlmcp_metrics:counter(<<"metrics_export_total">>).
```

### 4. Validate All Metrics

```erlang
% Ensure metrology compliance
case erlmcp_metrology_validator:validate_metric(Metric) of
    {ok, _} -> record_metric(Metric);
    {error, Violation} -> logger:warning("Metric violation: ~p", [Violation])
end.
```

### 5. Use Distributed Context

```erlang
% Propagate trace context across nodes
?WITH_SPAN(<<"cross_node_operation">>, #{},
    % Context automatically propagated
    rpc:call(Node, erlmcp_server, handle_request, [Request])
).
```

## References

- [Observability Overview](README.md)
- [Metrics Collection](../performance/METRICS_COLLECTION.md)
- [Dashboard Guide](dashboard.md)
- [Chaos Engineering](chaos.md)
- [Toyota Production System](../TCPS_HEALTH_SUMMARY.md)

---

**Last Updated:** January 31, 2026  
**Version:** 2.1.0
