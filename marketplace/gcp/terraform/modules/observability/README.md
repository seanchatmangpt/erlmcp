# GCP Observability Module for erlmcp

**Version:** 3.0.0
**Updated:** 2026-02
**Status:** Production-Ready

Comprehensive Google Cloud Operations Suite (formerly Stackdriver) integration for erlmcp, implementing enterprise-grade observability with SLIs, SLOs, error budgets, and multi-burn-rate alerting.

## Overview

This Terraform module provides a complete observability solution following Google's Site Reliability Engineering (SRE) best practices:

- **Cloud Monitoring** - Custom metrics, dashboards, and alerting
- **Cloud Logging** - Structured logging, log-based metrics, and exports
- **Cloud Trace** - Distributed tracing for request flows
- **Cloud Profiler** - Continuous profiling for performance optimization
- **Error Reporting** - Automated error aggregation and notification
- **SLO Management** - Service-level objectives with error budgets

## Features

### Core Capabilities

- ✅ **SLI/SLO Implementation** - Request-based and distribution-based SLIs
- ✅ **Error Budget Tracking** - Multi-window burn rate alerts (1h, 6h)
- ✅ **Log-Based Metrics** - Extract metrics from structured logs
- ✅ **Multi-Severity Alerting** - Critical, warning, and info alerts with appropriate routing
- ✅ **Comprehensive Dashboards** - Operations, SLO, Erlang VM, performance, and security views
- ✅ **Notification Channel Management** - Email, PagerDuty, Slack, and webhook integrations
- ✅ **Uptime Monitoring** - Multi-region health checks
- ✅ **Log Export** - BigQuery, Cloud Storage, and Pub/Sub sinks

### Observability Signals

#### Metrics
- HTTP request rate and latency (P50, P95, P99)
- Error rates by type and severity
- Erlang VM metrics (processes, memory, supervisors)
- Connection metrics by transport (stdio, tcp, http, ws, sse)
- Resource utilization (CPU, memory, disk, network)

#### Logs
- Structured JSON logging
- Automatic error extraction
- Connection lifecycle events
- Supervisor restart tracking
- Request/response correlation

#### Traces
- OpenTelemetry integration
- End-to-end request tracing
- Cross-service correlation

## Architecture

```
observability/
├── main.tf                    # Core monitoring configuration
├── variables.tf               # Input variables
├── outputs.tf                 # Module outputs
├── providers.tf               # Provider configuration
├── README.md                  # This file
├── alert-policies/            # Alert policies submodule
│   ├── main.tf
│   ├── variables.tf
│   └── outputs.tf
└── dashboards/                # Dashboards submodule
    ├── main.tf
    ├── variables.tf
    └── outputs.tf
```

## Usage

### Basic Example

```hcl
module "observability" {
  source = "./modules/observability"

  project_id  = "my-gcp-project"
  environment = "production"
  service_name = "erlmcp-service"
  team_label   = "platform"

  notification_channels = {
    email = {
      enabled = true
      address = "ops-team@example.com"
    }
    slack = {
      enabled      = true
      channel_name = "#alerts"
      auth_token   = var.slack_token
    }
    pagerduty = {
      enabled     = true
      service_key = var.pagerduty_service_key
      auth_token  = var.pagerduty_token
    }
    webhook = {
      enabled    = false
      url        = ""
      auth_token = ""
    }
  }

  # SLO Configuration
  create_slos              = true
  enable_slo_alerts        = true
  slo_availability_goal    = 0.999  # 99.9%
  slo_latency_goal         = 0.95   # 95% of requests < threshold
  slo_latency_threshold_ms = 500    # 500ms
  slo_rolling_period_days  = 30

  # Alert Configuration
  enable_error_rate_alert    = true
  enable_latency_alert       = true
  enable_memory_alert        = true
  enable_cpu_alert           = true
  enable_health_check_alert  = true

  # Dashboard Configuration
  create_performance_dashboard = true
  create_erlang_dashboard      = true
  create_security_dashboard    = true
  create_slo_dashboard         = true

  # Log Export
  log_export_bigquery_enabled    = true
  log_export_bigquery_destination = "bigquery.googleapis.com/projects/my-project/datasets/erlmcp_logs"
  log_export_storage_enabled     = true
  log_export_storage_destination = "storage.googleapis.com/my-logs-bucket"
}
```

### Advanced Example with Custom Thresholds

```hcl
module "observability" {
  source = "./modules/observability"

  project_id  = "production-project"
  environment = "production"

  # Custom alert thresholds
  error_rate_alert_threshold = 20      # 20 errors/sec
  latency_alert_threshold    = 2.0     # 2 seconds
  memory_alert_threshold     = 4294967296  # 4GB
  cpu_alert_threshold        = 0.85    # 85%

  # Uptime check configuration
  create_uptime_check         = true
  uptime_check_path           = "/health"
  uptime_check_port           = 8080
  uptime_check_use_ssl        = true
  uptime_check_validate_ssl   = true
  uptime_check_regions        = ["USA", "EUROPE", "ASIA_PACIFIC"]

  # Log exclusions to reduce costs
  exclude_health_check_logs   = true
  exclude_successful_get_logs = true

  # Advanced features
  enable_anomaly_detection = true
  enable_supervisor_restart_alert = true
  enable_connection_error_alert   = true
}
```

## SLO Configuration

### Multi-Burn-Rate Alerting

The module implements multi-window, multi-burn-rate alerting as recommended by Google SRE:

| Window | Burn Rate | Alert Threshold | Error Budget Consumed |
|--------|-----------|-----------------|----------------------|
| 1 hour | Fast      | 14.4x          | 2% in 1 hour        |
| 6 hour | Slow      | 6x             | 5% in 6 hours       |

**Alert Logic:**
- **CRITICAL**: Fast burn (1h window) AND slow burn (6h window) both above threshold
- **WARNING**: Slow burn (6h window) above threshold
- **Auto-resolution**: When burn rate returns to sustainable levels

### SLI Types

#### Availability SLI (Request-Based)
```hcl
good_requests / total_requests

# Good requests: HTTP status != 5xx
# Target: 99.9% (0.1% error budget)
```

#### Latency SLI (Distribution-Based)
```hcl
requests_below_threshold / total_requests

# Threshold: 500ms (configurable)
# Target: 95% of requests below threshold
```

## Alert Policies

| Alert | Severity | Description | Default Threshold |
|-------|----------|-------------|-------------------|
| High Error Rate | CRITICAL | 5xx errors above threshold | 10 errors/sec |
| High Latency P95 | WARNING | P95 latency above threshold | 1 second |
| High Memory | CRITICAL | Memory usage above limit | 2GB |
| High CPU | WARNING | CPU utilization sustained high | 80% |
| Health Check Failure | CRITICAL | Multi-region uptime check failing | <50% success |
| Supervisor Restarts | CRITICAL | Erlang supervisor restart rate high | 5 restarts/sec |
| Connection Errors | WARNING | Connection error rate elevated | 10 errors/sec |
| SLO Burn Rate | CRITICAL | Error budget consumption rate | See table above |

## Dashboards

### 1. Main Operations Dashboard
**Purpose:** Real-time operational overview
**Widgets:**
- Request rate and error rate scorecards
- P95/P99 latency scorecards
- Active connections
- Request rate by status code
- Latency percentiles (P50, P95, P99)
- CPU and memory utilization
- Erlang process count
- Connection breakdown by transport
- Recent error logs

### 2. SLO & Error Budget Dashboard
**Purpose:** Track SLO compliance and error budget consumption
**Widgets:**
- Availability SLO compliance gauge
- Latency SLO compliance gauge
- Error budget remaining charts
- Multi-window burn rate analysis
- Good vs. total requests comparison

### 3. Erlang VM Dashboard
**Purpose:** Deep dive into Erlang VM internals
**Widgets:**
- Process count and trends
- Memory breakdown by type
- Supervisor restart analysis
- Connection distribution by transport
- Per-supervisor restart rates

### 4. Performance Dashboard
**Purpose:** Performance analysis and optimization
**Widgets:**
- Latency distribution heatmap
- Endpoint-level P95 latency
- Request volume by method
- Top endpoints by traffic
- Throughput analysis

### 5. Security Dashboard
**Purpose:** Security monitoring and audit
**Widgets:**
- 4xx error rate
- Authentication errors
- Connection errors
- HTTP status distribution
- Security-related log events

## Log-Based Metrics

The module creates the following log-based metrics:

| Metric | Type | Description |
|--------|------|-------------|
| `erlmcp_error_count` | DELTA INT64 | Error log count by severity |
| `erlmcp_request_latency` | DELTA DISTRIBUTION | Request latency from logs |
| `erlmcp_connection_errors` | DELTA INT64 | Connection errors by type |
| `erlmcp_supervisor_restarts` | DELTA INT64 | Supervisor restarts by reason |

### Required Log Format

Ensure your erlmcp application emits structured JSON logs:

```json
{
  "severity": "ERROR",
  "labels": {
    "application": "erlmcp",
    "service": "mcp_handler"
  },
  "jsonPayload": {
    "type": "request_complete",
    "method": "POST",
    "endpoint": "/api/v1/execute",
    "latency_ms": 245,
    "status": 200
  }
}
```

## Notification Channels

### Supported Channels

1. **Email** - Basic email notifications
2. **PagerDuty** - Incident management integration
3. **Slack** - Team collaboration notifications
4. **Webhook** - Custom webhook integration

### Severity-Based Routing

- **CRITICAL** → PagerDuty + Email + Webhook
- **WARNING** → Slack + Email
- **INFO** → Slack only

## Best Practices

### 1. SLO Management
- Start with conservative SLO targets (99% availability)
- Monitor error budget burn rate before increasing targets
- Review SLO compliance monthly
- Adjust thresholds based on user experience data

### 2. Alert Fatigue Prevention
- Use multi-window alerts to reduce false positives
- Set appropriate auto-close durations
- Implement notification rate limits
- Route by severity to appropriate channels

### 3. Cost Optimization
- Enable log exclusions for noisy logs (health checks, successful GETs)
- Use sampling for high-volume traces
- Configure appropriate log retention periods
- Export to Cloud Storage for long-term archival

### 4. Operational Excellence
- Document runbooks for each alert
- Include troubleshooting steps in alert documentation
- Review and update dashboards monthly
- Conduct SLO reviews with stakeholders

## Outputs

| Output | Description |
|--------|-------------|
| `monitoring_service_id` | Service ID for SLO references |
| `availability_slo_id` | Availability SLO ID |
| `latency_slo_id` | Latency SLO ID |
| `notification_channel_ids` | Map of notification channel IDs |
| `alert_policy_ids` | Map of alert policy IDs |
| `dashboard_urls` | Direct URLs to all dashboards |
| `log_sink_writer_identities` | Service accounts for log sinks |

## Requirements

| Name | Version |
|------|---------|
| terraform | >= 1.5.0 |
| google | >= 6.0.0 |

## Permissions Required

The service account running Terraform needs:

```
roles/monitoring.editor
roles/logging.configWriter
roles/iam.serviceAccountUser (for log sink service accounts)
```

## Migration from v2.x

The v3.0 update includes breaking changes:

1. **Module structure** - Alert policies and dashboards are now submodules
2. **Dashboard layout** - Migrated from gridLayout to mosaicLayout
3. **SLO configuration** - New request-based and distribution-based SLIs
4. **Provider version** - Requires google provider >= 6.0.0

See [MIGRATION.md](./MIGRATION.md) for detailed migration guide.

## Troubleshooting

### Common Issues

#### 1. Log-based metrics not populating
**Solution:** Ensure your application emits logs with the correct format and labels:
```json
{
  "labels": {
    "application": "erlmcp"
  }
}
```

#### 2. SLO compliance showing 0%
**Solution:** Verify that custom metrics are being ingested:
```bash
gcloud monitoring metrics list --filter="metric.type:custom.googleapis.com/erlmcp"
```

#### 3. Alerts not triggering
**Solution:** Check notification channel configuration and test channels:
```bash
gcloud alpha monitoring channels describe CHANNEL_ID
```

## Support

- **Documentation:** https://docs.erlmcp.dev/observability
- **Issues:** https://github.com/erlmcp/erlmcp/issues
- **Runbooks:** https://docs.erlmcp.dev/runbooks

## License

Apache 2.0

## Contributors

Platform Team @ erlmcp
