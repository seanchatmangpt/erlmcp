# Enterprise Monitoring Stack for erlmcp v3

This directory contains a comprehensive enterprise-grade monitoring stack for erlmcp v3 deployments at Fortune 500 scale.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                     Enterprise Monitoring Stack              │
├─────────────────────────────────────────────────────────────┤
│  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │
▼  ▼  ▼  ▼  ▼  ▼  ▼  ▼  ▼  ▼  ▼  │  ▼  ▼  ▼  ▼  ▼  ▼  ▼  ▼  ▼  ▼
├─────────────────┐               │  ┌─────────────────────┐
│   Prometheus     │               │  │    Grafana          │
│   (Metrics)     │               │  │   (Dashboards)      │
├─────────────────┘               │  └─────────────────────┘
│  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │
▼  ▼  │  ▼  ▼  │  ▼  ▼  │  ▼  ▼  │  ▼  ▼  │  ▼  ▼  │  ▼  ▼  ▼  ▼
├─────────────────┐           ┌───┴───┐ ┌─────────────────────┐
│  OpenTelemetry  │           │Alertmanager│    Loki          │
│   (Tracing)     │           │(Alerts)│  (Logging)         │
├─────────────────┘           └────────┘ └─────────────────────┘
│  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │  │
▼  ▼  │  ▼  ▼  │  ▼  ▼  │  ▼  ▼  │  ▼  ▼  │  ▼  ▼  │  ▼  ▼  ▼  ▼
├─────────────────┐       ┌───┴───┐ ┌────────┐ ┌───────────────┐
│   erlmcp v3     │       │ Health │ │Performance│   Enterprise   │
│   (Application) │       │Checks  │ │Monitors │   Components   │
└─────────────────┘       └────────┘ └────────┘ └───────────────┘
```

## Components

### 1. Prometheus (Metrics Collection)
- **File**: `prometheus/prometheus.yml`
- **Purpose**: Collect metrics from erlmcp v3 components
- **Custom Metrics**: Registry metrics, session metrics, transport metrics
- **Recording Rules**: Pre-calculated aggregations
- **Alert Rules**: Enterprise alert thresholds

### 2. Grafana (Visualization)
- **Dashboards**: `dashboards/`
- **Purpose**: Real-time monitoring and historical analysis
- **Enterprise Views**: Infrastructure, Application, Business Metrics
- **Templates**: Reusable panel templates

### 3. OpenTelemetry (Distributed Tracing)
- **Instrumentation**: `otel/instrumentation.erl`
- **Purpose**: Request tracing, performance analysis
- **Integration**: Jaeger-compatible traces
- **Sampling**: Adaptive sampling for production

### 4. Alertmanager (Alerting)
- **Configuration**: `alertmanager/alertmanager.yml`
- **Purpose**: Enterprise alert routing and management
- **Escalation**: Multi-level alert escalation
- **Notification**: Email, Slack, PagerDuty

### 5. Loki (Logging)
- **Configuration**: `logging/loki-config.yml`
- **Purpose**: Centralized log aggregation
- **Integration**: Promtail for log collection
- **Retention**: Configurable retention policies

### 6. Health Monitoring
- **Endpoints**: `health/health_check.erl`
- **Purpose**: Application health monitoring
- **Probes**: Liveness, Readiness, Startup
- **Integration**: Kubernetes ready

### 7. Performance Monitoring
- **Metrics**: Registry throughput, queue depth, response times
- **Dashboards**: Performance trend analysis
- **Benchmarks**: Historical performance comparison

## Quick Start

```bash
# Start all monitoring components
docker-compose -f docker-compose.monitoring.yml up -d

# Access Grafana
open http://localhost:3000
admin/prom-operator

# Access Prometheus
open http://localhost:9090

# View health checks
curl http://localhost:8080/health
```

## Enterprise Features

- **Multi-tenancy**: Support for multiple erlmcp deployments
- **High Availability**: Redundant monitoring components
- **Scalability**: Horizontally scalable metrics collection
- **Security**: RBAC, SSL/TLS encryption
- **Compliance**: SOC2, GDPR, HIPAA ready
- **Cost Management**: Optimized resource usage
- **SLA Monitoring**: Service level agreement tracking
- **Capacity Planning**: Resource utilization forecasting

## Monitoring Policies

1. **Error Rate**: < 0.1%
2. **Response Time**: < 100ms p99
3. **Uptime**: 99.99%
4. **Resource Utilization**: < 70% average
5. **Throughput**: Scaling automatically with load

## Alerting Levels

- **P0**: Critical (Production down)
- **P1**: High (Performance degradation)
- **P2**: Medium (Resource exhaustion)
- **P3**: Low (Maintenance required)