# Changelog

All notable changes to the GCP Observability module will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [3.0.0] - 2026-02-06

### Added

#### Core Observability
- **Cloud Trace integration** - Added cloudtrace.googleapis.com API enablement for distributed tracing
- **Cloud Profiler integration** - Added cloudprofiler.googleapis.com API for continuous profiling
- **Error Reporting** - Added clouderrorreporting.googleapis.com API for automated error aggregation
- **Environment-based naming** - All resources now include environment suffix for multi-environment support

#### SLO Management
- **Multi-burn-rate alerting** - Implemented 1-hour and 6-hour burn rate windows
- **Request-based SLI** - Availability SLO using good/total request ratio
- **Distribution-based SLI** - Latency SLO using distribution cuts
- **Error budget tracking** - Automatic error budget alerts with configurable thresholds
- **SLO dashboard** - Dedicated dashboard for SLO compliance and burn rate visualization
- **User labels** - Added environment, team, and service labels to monitoring resources

#### Log-Based Metrics
- **Error count metric** - Extract error counts from logs with severity and service labels
- **Request latency metric** - Distribution metric from structured log events
- **Connection errors metric** - Track connection failures by transport type
- **Supervisor restarts metric** - Monitor Erlang supervisor restart patterns
- **Exponential bucketing** - Optimized latency distribution bucketing

#### Alert Policies
- **Multi-window error rate alerts** - 5-minute window with rate-based detection
- **P95 latency alerts** - Separate P95 tracking with endpoint breakdown
- **Severity classification** - CRITICAL, WARNING, INFO severity levels
- **Comprehensive documentation** - Detailed runbooks and troubleshooting steps in each alert
- **Notification rate limiting** - Prevent alert fatigue with configurable periods
- **Auto-close configuration** - Automatic incident closure when conditions resolve
- **Supervisor restart alerts** - Track Erlang supervisor instability
- **Connection error alerts** - Monitor transport-level connection issues
- **Anomaly detection alerts** - ML-based anomaly detection (optional)
- **Severity-based routing** - Route alerts to appropriate channels by severity

#### Dashboards (Modern Mosaic Layout)
- **Main Operations Dashboard** - Real-time operational overview with scorecards and charts
- **SLO Dashboard** - Error budget tracking, burn rate analysis, and compliance gauges
- **Erlang VM Dashboard** - VM internals, process counts, memory breakdown, supervisor analysis
- **Performance Dashboard** - Latency heatmaps, endpoint analysis, throughput metrics
- **Security Dashboard** - Security events, 4xx errors, authentication failures
- **Heatmap visualization** - Latency distribution heatmaps for performance analysis
- **Pie/Donut charts** - Connection and request distribution visualizations
- **Threshold indicators** - Visual thresholds on scorecards and charts
- **Log panels** - Integrated log viewing in dashboards

#### Log Management
- **Pub/Sub export** - Real-time error log streaming to Pub/Sub
- **Partitioned BigQuery tables** - Cost-optimized BigQuery exports
- **Structured filtering** - Support for multi-resource-type log collection
- **Writer identity tracking** - Automatic service account management for sinks
- **Environment-based naming** - Log sinks named by environment

#### Notification Channels
- **Webhook token auth** - Support for authenticated webhook notifications
- **Force delete protection** - Prevent accidental notification channel deletion
- **Sensitive labels** - Proper handling of tokens and keys as sensitive data
- **Severity routing** - Separate critical, warning, and info notification channels

#### Module Outputs
- **Comprehensive outputs** - All resource IDs, URLs, and service accounts exposed
- **Dashboard URLs** - Direct links to Cloud Console dashboards
- **Writer identities** - Log sink service accounts for IAM configuration
- **Enabled APIs list** - Track which APIs are enabled

### Changed

#### Breaking Changes
- **Provider version** - Updated minimum google provider from `>= 5.0.0` to `>= 6.0.0`
- **Module structure** - Alert policies and dashboards are now submodules (not inline files)
- **Dashboard layout** - Migrated from `gridLayout` to `mosaicLayout` (Google's modern layout)
- **SLO implementation** - Changed from basic_sli to request_based_sli with better accuracy
- **Notification channels** - Changed from simple lists to severity-based routing

#### Improvements
- **Variable validation** - Added validation rules for SLO goals, environment, rolling periods
- **Resource naming** - Consistent naming with environment suffix
- **Documentation** - Inline documentation in alert policies with runbook links
- **Aggregation periods** - Optimized alignment periods for different metric types
- **Chart types** - Modern chart types (STACKED_AREA, HEATMAP, DONUT)

### Deprecated
- **gridLayout** - Dashboard gridLayout deprecated in favor of mosaicLayout
- **basic_sli** - Basic SLI configuration deprecated for request_based_sli

### Removed
- **Inline submodules** - Removed alert-policies.tf and dashboards.tf from parent module
- **Basic dashboard templates** - Removed simplistic dashboard JSON templates
- **local_file resources** - Removed local file generation for dashboard templates

### Fixed
- **SLO uptime check reference** - Fixed conditional reference to uptime check in availability SLO
- **Module source paths** - Corrected module source from `.tf` files to proper subdirectories
- **Notification channel conditionals** - Fixed compact() usage for conditional channel creation
- **Log filter syntax** - Improved log filter syntax for multi-resource queries

### Security
- **Sensitive variables** - Marked notification channel credentials as sensitive
- **Least privilege** - Log sink writer identities follow least privilege principle
- **Audit logging** - Security dashboard includes audit event tracking
- **Zero-trust patterns** - Explicit resource filtering and validation

## [2.0.0] - 2025-11-15

### Added
- Initial comprehensive observability module
- Basic SLO configuration
- Alert policies for error rate, latency, memory, CPU
- Simple dashboards with gridLayout
- Notification channel support
- Uptime checks

## [1.0.0] - 2025-08-01

### Added
- Initial release
- Basic monitoring and logging setup
- Custom metric definitions

---

## Upgrade Guide

### From 2.x to 3.0

1. **Update provider version** in your root module:
```hcl
terraform {
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 6.0.0"
    }
  }
}
```

2. **Update module variables** - Add new required variables:
```hcl
module "observability" {
  # ... existing config ...

  environment = "production"  # NEW
  team_label  = "platform"    # NEW

  # Update SLO config
  enable_slo_alerts = true    # NEW

  # Add new dashboard flags
  create_slo_dashboard = true  # NEW
}
```

3. **Review alert routing** - Configure severity-based notification channels:
```hcl
# Critical alerts go to PagerDuty + Email
# Warning alerts go to Slack + Email
# Info alerts go to Slack only
```

4. **Import existing resources** (if needed):
```bash
# Import monitoring service
terraform import module.observability.google_monitoring_service.erlmcp \
  projects/PROJECT_ID/services/SERVICE_ID

# Import existing SLOs
terraform import module.observability.google_monitoring_slo.availability[0] \
  projects/PROJECT_ID/services/SERVICE_ID/serviceLevelObjectives/SLO_ID
```

5. **Run terraform plan** to review changes:
```bash
terraform plan
```

6. **Apply incrementally** to avoid downtime:
```bash
# Apply dashboard changes first (no impact)
terraform apply -target=module.observability.module.dashboards

# Then apply alerts
terraform apply -target=module.observability.module.alert_policies

# Finally apply remaining changes
terraform apply
```

### Breaking Changes Detail

#### Module Structure
**Before:**
```hcl
module "alert_policies" {
  source = "./alert-policies.tf"  # ❌ Wrong
}
```

**After:**
```hcl
module "alert_policies" {
  source = "./alert-policies"  # ✅ Correct subdirectory
}
```

#### Dashboard Layout
**Before:**
```json
{
  "gridLayout": {
    "columns": "2",
    "widgets": [...]
  }
}
```

**After:**
```json
{
  "mosaicLayout": {
    "columns": 12,
    "tiles": [...]
  }
}
```

#### SLO Configuration
**Before:**
```hcl
basic_sli {
  method = {
    uptime_check_ids = [...]
  }
}
```

**After:**
```hcl
request_based_sli {
  good_total_ratio {
    total_service_filter = "..."
    good_service_filter  = "..."
  }
}
```

## Support

For questions or issues during upgrade:
- Open an issue: https://github.com/erlmcp/erlmcp/issues
- Check documentation: https://docs.erlmcp.dev/observability/migration
- Contact platform team: platform@example.com
