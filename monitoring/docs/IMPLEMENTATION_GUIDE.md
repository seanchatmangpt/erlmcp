# Enterprise Monitoring Stack Implementation Guide for erlmcp v3

This guide provides comprehensive instructions for implementing and managing the enterprise-grade monitoring stack for erlmcp v3 deployments.

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Prerequisites](#prerequisites)
3. [Deployment](#deployment)
4. [Configuration](#configuration)
5. [Customization](#customization)
6. [Integration](#integration)
7. [Monitoring](#monitoring)
8. [Troubleshooting](#troubleshooting)
9. [Best Practices](#best-practices)
10. [Maintenance](#maintenance)

## Architecture Overview

The monitoring stack consists of the following components:

### Core Components

1. **Prometheus** - Metrics collection and aggregation
   - Pulls metrics from erlmcp v3 components
   - Stores metrics in time-series database
   - Provides alerting rules

2. **Grafana** - Visualization and dashboards
   - Real-time monitoring dashboards
   - Alerting and notification
   - Enterprise reporting

3. **Alertmanager** - Alert routing and escalation
   - Manages alerts from Prometheus
   - Routes to appropriate channels
   - Implements escalation policies

4. **OpenTelemetry Collector** - Distributed tracing
   - Collects traces from erlmcp applications
   - Aggregates and exports to Jaeger
   - Provides metrics collection

5. **Loki** - Log aggregation
   - Collects logs from all components
   - Provides log querying capabilities
   - Integrates with Grafana

6. **Promtail** - Log collection agent
   - Ships logs to Loki
   - Parses and enriches logs
   - Handles log rotation

### Optional Components

- **Jaeger** - Distributed tracing backend
- **Elasticsearch** - Log storage and analysis
- **Kibana** - Log visualization
- **Thanos** - Long-term metrics storage
- **VictoriaMetrics** - High-performance metrics storage

## Prerequisites

### System Requirements

- **Docker** and **Docker Compose** v2.1.0 or higher
- **4GB RAM** minimum (8GB recommended)
- **2 CPU cores** minimum (4 cores recommended)
- **20GB** disk space
- **Network access** to required services

### Software Dependencies

- Erlang/OTP 28.3.1
- erlmcp v3.0.0 or higher
- Prometheus v2.45.0
- Grafana v10.2.0
- OpenTelemetry Collector v0.87.0

## Deployment

### Quick Start

1. **Clone the repository**
```bash
git clone https://github.com/your-org/erlmcp.git
cd erlmcp/monitoring
```

2. **Make the deployment script executable**
```bash
chmod +x scripts/deploy-monitoring.sh
```

3. **Deploy the monitoring stack**
```bash
./scripts/deploy-monitoring.sh
```

4. **Verify deployment**
```bash
./scripts/deploy-monitoring.sh verify
```

### Deployment Script Options

```bash
# Start monitoring stack
./scripts/deploy-monitoring.sh start

# Stop monitoring stack
./scripts/deploy-monitoring.sh stop

# Restart services
./scripts/deploy-monitoring.sh restart

# View logs
./scripts/deploy-monitoring.sh logs

# Clean up (removes all data)
./scripts/deploy-monitoring.sh clean
```

### Manual Deployment

1. **Create directories**
```bash
mkdir -p prometheus/data prometheus/rules prometheus/recording_rules
mkdir -p alertmanager/data
mkdir -p grafana/data grafana/dashboards grafana/datasources
mkdir -p loki/data
mkdir -p otel/data
```

2. **Copy configuration files**
```bash
cp prometheus/prometheus.yml prometheus/
cp prometheus/rules/*.yml prometheus/rules/
cp prometheus/recording_rules/*.yml prometheus/recording_rules/
cp alertmanager/alertmanager.yml alertmanager/
cp alertmanager/templates/*.tmpl alertmanager/templates/
cp grafana/datasources/prometheus.yml grafana/datasources/
```

3. **Start services with Docker Compose**
```bash
docker-compose -f docker/docker-compose.monitoring.yml up -d
```

4. **Wait for services to be ready**
```bash
./scripts/wait-for-services.sh
```

## Configuration

### Prometheus Configuration

The main configuration is in `prometheus/prometheus.yml`. Key sections:

- **Global configuration** - Default values and time intervals
- **Scrape configs** - Targets and metrics collection
- **Alerting rules** - Alert conditions and thresholds
- **Remote write** - Long-term storage integration

#### Custom Metrics

To add custom metrics:

1. **Define the metric in Prometheus**
```yaml
scrape_configs:
  - job_name: 'erlmcp_custom'
    metrics_path: '/metrics/custom'
    static_configs:
      - targets: ['erlmcp:8086']
```

2. **Create recording rules**
```yaml
groups:
  - name: erlmcp-custom-metrics
    rules:
      - record: erlmcp_custom_metric:sum
        expr: sum(erlmcp_custom_metric)
```

3. **Update alerts**
```yaml
- alert: high_custom_metric
  expr: erlmcp_custom_metric > 100
  for: 5m
  labels:
    severity: warning
  annotations:
    summary: "High custom metric detected"
```

### Grafana Configuration

#### Data Sources

Data sources are configured in `grafana/datasources/prometheus.yml`:

- **Prometheus** - Metrics endpoint
- **Loki** - Log query endpoint
- **Jaeger** - Tracing endpoint

#### Dashboards

Existing dashboards:

1. **Overview Dashboard** (`erlmcp-overview`)
   - System health overview
   - Request rates and errors
   - Resource utilization
   - Service status

2. **Performance Dashboard** (`erlmcp-performance`)
   - Latency metrics
   - Throughput analysis
   - Queue depth
   - Error rates

3. **SLA Dashboard** (`erlmcp-sla`)
   - SLA compliance tracking
   - Uptime monitoring
   - Response time SLAs
   - Financial impact

#### Custom Dashboards

To create custom dashboards:

1. **Export the dashboard JSON**
```json
{
  "dashboard": {
    "title": "Custom Dashboard",
    "panels": [...]
  }
}
```

2. **Place in `grafana/dashboards/`**

3. **Deploy with Grafana provisioning**
```yaml
apiVersion: 1
datasources:
  - name: Prometheus
    type: prometheus
    url: http://prometheus:9090
    access: proxy
dashboards:
  - name: custom-dashboard
    orgId: 1
    folder: General
    type: file
    options:
      path: /etc/grafana/provisioning/dashboards/custom-dashboard.json
```

### Alertmanager Configuration

Alertmanager routing is configured in `alertmanager/alertmanager.yml`:

#### Alert Priorities

- **P0** - Critical (immediate attention)
- **P1** - High (within 1 hour)
- **P2** - Medium (within 4 hours)
- **P3** - Low (within 24 hours)

#### Notification Channels

Configure multiple channels:

- **Slack** - Team notifications
- **PagerDuty** - Critical alerts
- **Email** - General notifications
- **SMS** - Urgent alerts

#### Escalation Policies

Define escalation paths based on priority and duration.

### OpenTelemetry Configuration

The collector configuration is in `otel/collector-config.yaml`:

#### Receivers

- **OTLP** - Modern protocol for metrics and traces
- **Prometheus** - Pull metrics from endpoints
- **Jaeger** - Legacy trace collection

#### Processors

- **Batch** - Batch metrics for efficiency
- **Memory** - Add resource attributes
- **Resource** - Enrich metrics with labels

#### Exporters

- **Prometheus** - Export to Prometheus
- **Jaeger** - Export traces
- **Loki** - Export logs
- **OTLP** - Export to backend

## Integration

### erlmcp Application Integration

1. **Add OpenTelemetry instrumentation**
```erlang
% In your application
-module(erlmcp_app).
-behaviour(application).

start(_Type, _Args) ->
    erlmcp_otel_instrumentation:start_link(),
    erlmcp_metrics:start_link(),
    erlmcp_sup:start_link().
```

2. **Instrument requests**
```erlang
% In your request handler
trace_request(Req, fun() ->
    % Your business logic here
    erlmcp_metrics:record_counter("requests_total", 1, #{}),
    process_request(Req)
end).
```

3. **Log with structured format**
```erlang
logger:info("Request processed", #{
    request_id => ReqId,
    status => Status,
    duration_ms => Duration
}).
```

### Kubernetes Integration

#### Deploy with Kubernetes

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp-monitoring
spec:
  replicas: 1
  selector:
    matchLabels:
      app: erlmcp-monitoring
  template:
    metadata:
      labels:
        app: erlmcp-monitoring
    spec:
      containers:
      - name: monitoring-stack
        image: erlmcp/monitoring:v3.0.0
        ports:
        - containerPort: 9090
        - containerPort: 3000
        - containerPort: 3100
```

#### Health Checks

```yaml
livenessProbe:
  httpGet:
    path: /health
    port: 8080
  initialDelaySeconds: 30
  periodSeconds: 10
```

### CI/CD Integration

#### GitHub Actions Example

```yaml
name: Deploy Monitoring Stack
on:
  push:
    paths:
      - 'monitoring/**'

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    - name: Deploy monitoring
      run: |
        chmod +x monitoring/scripts/deploy-monitoring.sh
        ./monitoring/scripts/deploy-monitoring.sh
```

## Monitoring

### Key Metrics to Track

#### System Metrics
- CPU usage
- Memory usage
- Disk I/O
- Network traffic

#### Application Metrics
- Request rate
- Response time
- Error rate
- Resource utilization

#### Business Metrics
- API quota usage
- Authentication success rate
- Session metrics
- Resource registry activity

### Alert Thresholds

| Metric | Warning | Critical | SLA |
|--------|---------|----------|-----|
| Error Rate | > 0.1% | > 1% | < 0.1% |
| P99 Latency | > 100ms | > 500ms | < 100ms |
| CPU Usage | > 70% | > 90% | < 70% |
| Memory Usage | > 70% | > 90% | < 70% |
| Uptime | - | < 99.9% | 99.9% |

### Monitoring Dashboards

1. **Overview Dashboard**
   - Real-time system status
   - Service health indicators
   - Quick troubleshooting view

2. **Performance Dashboard**
   - Detailed performance metrics
   - Historical trends
   - Bottleneck identification

3. **SLA Dashboard**
   - SLA compliance tracking
   - Financial impact
   - Incident management

### Log Analysis

1. **Log Levels**
   - ERROR - Critical issues
   - WARN - Important warnings
   - INFO - Normal operations
   - DEBUG - Detailed information

2. **Log Patterns**
   - Structured logging with JSON
   - Correlation IDs for tracing
   - Contextual information

## Troubleshooting

### Common Issues

#### Prometheus Not Scraping

1. **Check firewall rules**
```bash
curl -f http://localhost:9090/api/v1/targets
```

2. **Verify targets are reachable**
```bash
curl -f http://erlmcp:8080/metrics
```

3. **Check Prometheus logs**
```bash
docker logs erlmcp-prometheus
```

#### Grafana Not Loading Dashboards

1. **Check data source configuration**
```bash
curl -f http://localhost:3000/api/datasources
```

2. **Verify dashboard provisioning**
```bash
curl -f http://localhost:3000/api/dashboards
```

#### Loki Not Collecting Logs

1. **Check Promtail configuration**
```bash
docker logs erlmcp-promtail
```

2. **Verify log file permissions**
```bash
ls -la /var/log/erlmcp/
```

### Debugging Commands

```bash
# Check all services
docker-compose -f docker/docker-compose.monitoring.yml ps

# View logs
docker-compose -f docker/docker-compose.monitoring.yml logs -f prometheus

# Check connectivity
nc -z localhost 9090
nc -z localhost 3000
nc -z localhost 3100

# Test Prometheus queries
curl -G "http://localhost:9090/api/v1/query" --data-urlencode 'query=up'
```

### Performance Tuning

#### Prometheus Performance

1. **Increase retention time**
```yaml
storage:
  tsdb:
    retention.time: 30d
```

2. **Adjust scrape intervals**
```yaml
scrape_configs:
  - job_name: 'erlmcp'
    scrape_interval: 30s
```

3. **Enable compression**
```yaml
remote_write:
  queue_config:
    max_shards: 20
```

#### Grafana Performance

1. **Increase cache timeout**
```yaml
metrics:
  max_data_points: 1000
  time_interval: 30s
```

2. **Use efficient queries**
```sql
# Use recording rules instead of complex queries
sum(rate(requests_total[5m]))
```

## Best Practices

### Monitoring Best Practices

1. **Collect metrics at appropriate intervals**
   - Critical metrics: 15 seconds
   - Regular metrics: 30 seconds
   - Historical metrics: 5 minutes

2. **Use meaningful metric names**
   - Use consistent naming conventions
   - Include units in metric names
   - Use hierarchical names

3. **Set appropriate retention policies**
   - Short-term metrics: 7 days
   - Medium-term metrics: 30 days
   - Long-term metrics: 365 days

### Security Best Practices

1. **Secure access to monitoring data**
   - Use authentication and authorization
   - Encrypt sensitive data
   - Limit access based on roles

2. **Monitor security events**
   - Track authentication failures
   - Monitor unauthorized access
   - Alert on suspicious activities

3. **Regular security audits**
   - Review access logs
   - Check for unauthorized configuration changes
   - Monitor for vulnerabilities

### Cost Optimization

1. **Right-size infrastructure**
   - Monitor resource usage
   - Scale based on demand
   - Use spot instances for non-critical workloads

2. **Optimize storage**
   - Use appropriate retention policies
   - Enable compression
   - Archive old data

3. **Optimize queries**
   - Use recording rules
   - Avoid expensive queries
   - Use caching where possible

## Maintenance

### Regular Maintenance Tasks

1. **Daily Tasks**
   - Review alerts
   - Check system health
   - Review logs

2. **Weekly Tasks**
   - Update dashboards
   - Review alert thresholds
   - Optimize queries

3. **Monthly Tasks**
   - Review retention policies
   - Update alert policies
   - Capacity planning

### Backup and Recovery

1. **Backup Configuration**
```bash
# Backup Prometheus configuration
docker cp erlmcp-prometheus:/etc/prometheus/prometheus.yml ./backup/

# Backup Grafana dashboards
docker cp erlmcp-grafana:/var/lib/grafana ./backup/
```

2. **Backup Data**
```bash
# Backup Prometheus data
docker cp erlmcp-prometheus:/prometheus ./backup/

# Backup Loki data
docker cp erlmcp-loki:/loki ./backup/
```

### Updates and Upgrades

1. **Update Monitoring Stack**
```bash
# Pull latest images
docker-compose -f docker/docker-compose.monitoring.yml pull

# Restart services
docker-compose -f docker/docker-compose.monitoring.yml up -d
```

2. **Erlang/OTP Upgrades**
```bash
# Backup current installation
export ERLMCP_VERSION=3.0.0
./scripts/backup.sh

# Upgrade OTP
./scripts/upgrade-otp.sh 28.3.1
```

## Support

### Documentation

- [Prometheus Documentation](https://prometheus.io/docs/)
- [Grafana Documentation](https://grafana.com/docs/)
- [OpenTelemetry Documentation](https://opentelemetry.io/docs/)
- [Loki Documentation](https://grafana.com/docs/loki/)

### Community Support

- [erlmcp GitHub Issues](https://github.com/your-org/erlmcp/issues)
- [Prometheus Slack](https://slack.io/)
- [Grafana Community](https://community.grafana.com/)
- [OpenTelemetry Slack](https://cloud-native.slack.com/)

### Professional Support

For enterprise support, contact:
- Email: support@erlmcp.com
- Phone: +1-800-ERLMCP
- Support Portal: https://support.erlmcp.com

## Conclusion

This enterprise monitoring stack provides comprehensive observability for erlmcp v3 deployments at Fortune 500 scale. With proper implementation and maintenance, it enables proactive monitoring, rapid incident response, and data-driven decision making.

Follow the best practices outlined in this guide to maximize the value of your monitoring investment and ensure the reliability and performance of your erlmcp deployments.