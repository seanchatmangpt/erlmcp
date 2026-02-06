# ============================================================================
# Observability Module for erlmcp
# Google Cloud Operations Suite (Monitoring, Logging, Alerting, Tracing)
# Updated: 2026-02 - Latest Google Cloud Observability Best Practices
# ============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 6.0.0"
    }
  }
}

# ============================================================================
# Enable Required APIs
# ============================================================================
resource "google_project_service" "monitoring" {
  project            = var.project_id
  service            = "monitoring.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "logging" {
  project            = var.project_id
  service            = "logging.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "trace" {
  project            = var.project_id
  service            = "cloudtrace.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "profiler" {
  project            = var.project_id
  service            = "cloudprofiler.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "error_reporting" {
  project            = var.project_id
  service            = "clouderrorreporting.googleapis.com"
  disable_on_destroy = false
}

# ============================================================================
# Custom Metrics
# ============================================================================

# HTTP request latency metric
resource "google_monitoring_metric_descriptor" "http_latency" {
  project       = var.project_id
  metric_kind   = "GAUGE"
  value_type    = "DOUBLE"
  unit          = "s"
  display_name  = "erlmcp HTTP Request Latency"
  type          = "custom.googleapis.com/erlmcp/http/latency"
  description   = "HTTP request latency in seconds"
  labels        = ["method", "endpoint", "status"]
}

# HTTP request count metric
resource "google_monitoring_metric_descriptor" "http_requests" {
  project       = var.project_id
  metric_kind   = "CUMULATIVE"
  value_type    = "INT64"
  unit          = "1"
  display_name  = "erlmcp HTTP Request Count"
  type          = "custom.googleapis.com/erlmcp/http/requests"
  description   = "HTTP request count"
  labels        = ["method", "endpoint", "status"]
}

# Active connections metric
resource "google_monitoring_metric_descriptor" "connections" {
  project       = var.project_id
  metric_kind   = "GAUGE"
  value_type    = "INT64"
  unit          = "1"
  display_name  = "erlmcp Active Connections"
  type          = "custom.googleapis.com/erlmcp/connections/active"
  description   = "Number of active connections"
  labels        = ["transport"]
}

# Erlang process count metric
resource "google_monitoring_metric_descriptor" "processes" {
  project       = var.project_id
  metric_kind   = "GAUGE"
  value_type    = "INT64"
  unit          = "1"
  display_name  = "erlmcp Erlang Process Count"
  type          = "custom.googleapis.com/erlmcp/erlang/processes"
  description   = "Number of Erlang processes"
}

# Memory usage metric
resource "google_monitoring_metric_descriptor" "memory" {
  project       = var.project_id
  metric_kind   = "GAUGE"
  value_type    = "DOUBLE"
  unit          = "By"
  display_name  = "erlmcp Memory Usage"
  type          = "custom.googleapis.com/erlmcp/memory/usage"
  description   = "Memory usage in bytes"
  labels        = ["type"]
}

# ============================================================================
# Notification Channels
# ============================================================================

# Email notification channel
resource "google_monitoring_notification_channel" "email" {
  count       = var.notification_channels.email.enabled ? 1 : 0
  project     = var.project_id
  display_name = "Email Notifications"
  type        = "email"
  labels = {
    email_address = var.notification_channels.email.address
  }
  force_delete = false
}

# PagerDuty notification channel
resource "google_monitoring_notification_channel" "pagerduty" {
  count       = var.notification_channels.pagerduty.enabled ? 1 : 0
  project     = var.project_id
  display_name = "PagerDuty Notifications"
  type        = "pagerduty"
  labels = {
    pagerduty_service_key = var.notification_channels.pagerduty.service_key
  }
  sensitive_labels {
    auth_token = var.notification_channels.pagerduty.auth_token
  }
  force_delete = false
}

# Slack notification channel
resource "google_monitoring_notification_channel" "slack" {
  count       = var.notification_channels.slack.enabled ? 1 : 0
  project     = var.project_id
  display_name = "Slack Notifications"
  type        = "slack"
  labels = {
    channel_name = var.notification_channels.slack.channel_name
    auth_token   = var.notification_channels.slack.auth_token
  }
  force_delete = false
}

# Webhook notification channel
resource "google_monitoring_notification_channel" "webhook" {
  count       = var.notification_channels.webhook.enabled ? 1 : 0
  project     = var.project_id
  display_name = "Webhook Notifications"
  type        = "webhook_token_auth"
  labels = {
    url = var.notification_channels.webhook.url
  }
  sensitive_labels {
    auth_token = var.notification_channels.webhook.auth_token
  }
  force_delete = false
}

# ============================================================================
# Local Values for Notification Channel Routing
# ============================================================================
locals {
  # Critical alerts go to PagerDuty and email
  critical_notification_channels = compact([
    var.notification_channels.email.enabled ? google_monitoring_notification_channel.email[0].id : "",
    var.notification_channels.pagerduty.enabled ? google_monitoring_notification_channel.pagerduty[0].id : "",
    var.notification_channels.webhook.enabled ? google_monitoring_notification_channel.webhook[0].id : "",
  ])

  # Warning alerts go to Slack and email
  warning_notification_channels = compact([
    var.notification_channels.email.enabled ? google_monitoring_notification_channel.email[0].id : "",
    var.notification_channels.slack.enabled ? google_monitoring_notification_channel.slack[0].id : "",
  ])

  # Info alerts go to Slack only
  info_notification_channels = compact([
    var.notification_channels.slack.enabled ? google_monitoring_notification_channel.slack[0].id : "",
  ])

  all_notification_channels = compact([
    var.notification_channels.email.enabled ? google_monitoring_notification_channel.email[0].id : "",
    var.notification_channels.pagerduty.enabled ? google_monitoring_notification_channel.pagerduty[0].id : "",
    var.notification_channels.slack.enabled ? google_monitoring_notification_channel.slack[0].id : "",
    var.notification_channels.webhook.enabled ? google_monitoring_notification_channel.webhook[0].id : "",
  ])
}

# ============================================================================
# Alert Policies Module
# ============================================================================
module "alert_policies" {
  source = "./alert-policies"

  project_id                 = var.project_id
  environment                = var.environment
  notification_channels      = local.all_notification_channels
  critical_channels          = local.critical_notification_channels
  warning_channels           = local.warning_notification_channels

  enable_error_rate_alert    = var.enable_error_rate_alert
  enable_latency_alert       = var.enable_latency_alert
  enable_memory_alert        = var.enable_memory_alert
  enable_cpu_alert           = var.enable_cpu_alert
  enable_health_check_alert  = var.enable_health_check_alert
  enable_process_count_alert = var.enable_process_count_alert

  error_rate_alert_threshold = var.error_rate_alert_threshold
  latency_alert_threshold    = var.latency_alert_threshold
  memory_alert_threshold     = var.memory_alert_threshold
  cpu_alert_threshold        = var.cpu_alert_threshold

  depends_on = [
    google_project_service.monitoring,
    google_logging_metric.error_count,
    google_logging_metric.request_latency,
  ]
}

# ============================================================================
# Dashboards Module
# ============================================================================
module "dashboards" {
  source = "./dashboards"

  project_id                     = var.project_id
  environment                    = var.environment
  create_performance_dashboard   = var.create_performance_dashboard
  create_erlang_dashboard        = var.create_erlang_dashboard
  create_security_dashboard      = var.create_security_dashboard
  create_slo_dashboard           = var.create_slo_dashboard

  service_name = google_monitoring_service.erlmcp.service_id

  depends_on = [
    google_project_service.monitoring,
    google_monitoring_service.erlmcp,
  ]
}

# ============================================================================
# Uptime Check
# ============================================================================
resource "google_monitoring_uptime_check_config" "erlmcp" {
  count          = var.create_uptime_check ? 1 : 0
  project        = var.project_id
  display_name   = "erlmcp Health Check"
  http_check {
    path         = var.uptime_check_path
    port         = var.uptime_check_port
    request_method = var.uptime_check_method
    use_ssl      = var.uptime_check_use_ssl
    validate_ssl = var.uptime_check_validate_ssl
  }
  monitored_resource {
    type = var.uptime_check_resource_type
    labels = {
      project_id  = var.project_id
      host        = var.uptime_check_host
    }
  }
  timeout         = var.uptime_check_timeout
  period          = var.uptime_check_period
  selected_regions = var.uptime_check_regions
  content_matchers {
    content = var.uptime_check_content_matcher
    matcher = "CONTAINS_STRING"
  }
}

# ============================================================================
# Service Monitoring with Advanced SLIs/SLOs
# ============================================================================
resource "google_monitoring_service" "erlmcp" {
  project      = var.project_id
  service_id   = var.service_name
  display_name = "erlmcp MCP Service"

  user_labels = {
    environment = var.environment
    team        = var.team_label
    service     = "erlmcp"
  }
}

# Availability SLO with request-based SLI
resource "google_monitoring_slo" "availability" {
  count       = var.create_slos ? 1 : 0
  project     = var.project_id
  service     = google_monitoring_service.erlmcp.service_id
  slo_id      = "availability-slo"
  display_name = "erlmcp Availability SLO (99.9%)"

  # Request-based SLI using custom metric
  request_based_sli {
    good_total_ratio {
      total_service_filter = join(" AND ", [
        "metric.type=\"custom.googleapis.com/erlmcp/http/requests\"",
        "resource.type=\"gce_instance\""
      ])

      good_service_filter = join(" AND ", [
        "metric.type=\"custom.googleapis.com/erlmcp/http/requests\"",
        "resource.type=\"gce_instance\"",
        "metric.label.status!=\"5xx\""
      ])
    }
  }

  goal                = var.slo_availability_goal
  rolling_period_days = var.slo_rolling_period_days

  display_name = "Availability SLO - ${var.slo_availability_goal * 100}%"
}

# Latency SLO with distribution cut
resource "google_monitoring_slo" "latency" {
  count       = var.create_slos ? 1 : 0
  project     = var.project_id
  service     = google_monitoring_service.erlmcp.service_id
  slo_id      = "latency-slo"
  display_name = "erlmcp Latency SLO (95th percentile < ${var.slo_latency_threshold_ms}ms)"

  # Distribution-based SLI for latency
  request_based_sli {
    distribution_cut {
      distribution_filter = join(" AND ", [
        "metric.type=\"custom.googleapis.com/erlmcp/http/latency\"",
        "resource.type=\"gce_instance\""
      ])

      range {
        min = 0
        max = var.slo_latency_threshold_ms / 1000.0  # Convert to seconds
      }
    }
  }

  goal                = var.slo_latency_goal
  rolling_period_days = var.slo_rolling_period_days
}

# Error Budget-based Alert Policy for Availability SLO
resource "google_monitoring_alert_policy" "slo_burn_rate_availability" {
  count        = var.create_slos && var.enable_slo_alerts ? 1 : 0
  project      = var.project_id
  display_name = "erlmcp Availability SLO Burn Rate Alert"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Fast burn rate (1 hour)"
    condition_threshold {
      filter = join(" AND ", [
        "select_slo_burn_rate(\"${google_monitoring_slo.availability[0].id}\", 3600)"
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 14.4  # Fast burn rate threshold
      duration        = "0s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_NEXT_OLDER"
      }
    }
  }

  conditions {
    display_name = "Slow burn rate (6 hours)"
    condition_threshold {
      filter = join(" AND ", [
        "select_slo_burn_rate(\"${google_monitoring_slo.availability[0].id}\", 21600)"
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 6  # Slow burn rate threshold
      duration        = "0s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_NEXT_OLDER"
      }
    }
  }

  notification_channels = local.critical_notification_channels

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## SLO Burn Rate Alert - Availability

      The error budget for availability is being consumed at an elevated rate.

      **Current Status:**
      - Fast burn rate (1h): Consuming error budget 14.4x faster than sustainable
      - Slow burn rate (6h): Consuming error budget 6x faster than sustainable

      **Error Budget Impact:**
      At this rate, the monthly error budget will be exhausted in days or hours.

      **Immediate Actions:**
      1. Check recent deployments and rollback if necessary
      2. Review error logs for 5xx responses
      3. Check infrastructure health and autoscaling
      4. Verify database and downstream service health

      **Runbook:** https://docs.erlmcp.dev/runbooks/slo-burn-rate
    EOT
  }

  alert_strategy {
    notification_rate_limit {
      period = "3600s"  # 1 hour
    }
    auto_close = "604800s"  # 7 days
  }
}

# Error Budget-based Alert Policy for Latency SLO
resource "google_monitoring_alert_policy" "slo_burn_rate_latency" {
  count        = var.create_slos && var.enable_slo_alerts ? 1 : 0
  project      = var.project_id
  display_name = "erlmcp Latency SLO Burn Rate Alert"
  combiner     = "AND"
  enabled      = true

  conditions {
    display_name = "Latency SLO fast burn"
    condition_threshold {
      filter = join(" AND ", [
        "select_slo_burn_rate(\"${google_monitoring_slo.latency[0].id}\", 3600)"
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 10
      duration        = "0s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_NEXT_OLDER"
      }
    }
  }

  notification_channels = local.warning_notification_channels

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## SLO Burn Rate Alert - Latency

      Request latency is exceeding SLO targets at an elevated rate.

      **Actions:**
      1. Check CPU and memory utilization
      2. Review slow query logs
      3. Check for network issues
      4. Review cache hit rates

      **Runbook:** https://docs.erlmcp.dev/runbooks/latency-slo
    EOT
  }

  alert_strategy {
    auto_close = "86400s"  # 24 hours
  }
}

# ============================================================================
# Log-Based Metrics
# ============================================================================

# Error log metric
resource "google_logging_metric" "error_count" {
  project = var.project_id
  name    = "erlmcp_error_count"
  filter  = join(" AND ", [
    "resource.type=\"gce_instance\"",
    "severity>=ERROR",
    "labels.application=\"erlmcp\""
  ])

  metric_descriptor {
    metric_kind = "DELTA"
    value_type  = "INT64"
    unit        = "1"
    labels {
      key         = "severity"
      value_type  = "STRING"
      description = "Log severity level"
    }
    labels {
      key         = "service_name"
      value_type  = "STRING"
      description = "Service name"
    }
    display_name = "erlmcp Error Count"
  }

  label_extractors = {
    "severity"     = "EXTRACT(severity)"
    "service_name" = "EXTRACT(labels.service)"
  }
}

# MCP request latency from logs
resource "google_logging_metric" "request_latency" {
  project = var.project_id
  name    = "erlmcp_request_latency"
  filter  = join(" AND ", [
    "resource.type=\"gce_instance\"",
    "jsonPayload.type=\"request_complete\"",
    "labels.application=\"erlmcp\""
  ])

  metric_descriptor {
    metric_kind = "DELTA"
    value_type  = "DISTRIBUTION"
    unit        = "ms"
    labels {
      key         = "method"
      value_type  = "STRING"
      description = "HTTP method"
    }
    labels {
      key         = "endpoint"
      value_type  = "STRING"
      description = "API endpoint"
    }
    display_name = "erlmcp Request Latency"
  }

  value_extractor = "EXTRACT(jsonPayload.latency_ms)"

  label_extractors = {
    "method"   = "EXTRACT(jsonPayload.method)"
    "endpoint" = "EXTRACT(jsonPayload.endpoint)"
  }

  bucket_options {
    exponential_buckets {
      num_finite_buckets = 64
      growth_factor      = 2
      scale              = 0.01
    }
  }
}

# Connection errors metric
resource "google_logging_metric" "connection_errors" {
  project = var.project_id
  name    = "erlmcp_connection_errors"
  filter  = join(" AND ", [
    "resource.type=\"gce_instance\"",
    "severity>=WARNING",
    "jsonPayload.event=\"connection_error\"",
    "labels.application=\"erlmcp\""
  ])

  metric_descriptor {
    metric_kind = "DELTA"
    value_type  = "INT64"
    unit        = "1"
    labels {
      key         = "error_type"
      value_type  = "STRING"
      description = "Type of connection error"
    }
    labels {
      key         = "transport"
      value_type  = "STRING"
      description = "Transport type (stdio, tcp, http, ws, sse)"
    }
    display_name = "erlmcp Connection Errors"
  }

  label_extractors = {
    "error_type" = "EXTRACT(jsonPayload.error_type)"
    "transport"  = "EXTRACT(jsonPayload.transport)"
  }
}

# Supervisor restart metric
resource "google_logging_metric" "supervisor_restarts" {
  project = var.project_id
  name    = "erlmcp_supervisor_restarts"
  filter  = join(" AND ", [
    "resource.type=\"gce_instance\"",
    "jsonPayload.event=\"supervisor_restart\"",
    "labels.application=\"erlmcp\""
  ])

  metric_descriptor {
    metric_kind = "DELTA"
    value_type  = "INT64"
    unit        = "1"
    labels {
      key         = "supervisor"
      value_type  = "STRING"
      description = "Supervisor name"
    }
    labels {
      key         = "reason"
      value_type  = "STRING"
      description = "Restart reason"
    }
    display_name = "erlmcp Supervisor Restarts"
  }

  label_extractors = {
    "supervisor" = "EXTRACT(jsonPayload.supervisor)"
    "reason"     = "EXTRACT(jsonPayload.reason)"
  }
}

# ============================================================================
# Log Sinks for Export (Structured Logging)
# ============================================================================

# Export logs to BigQuery for analysis
resource "google_logging_project_sink" "bigquery" {
  count       = var.log_export_bigquery_enabled ? 1 : 0
  project     = var.project_id
  name        = "erlmcp-logs-bigquery-${var.environment}"
  destination = var.log_export_bigquery_destination

  # Structured logging filter
  filter = coalesce(var.log_export_filter, join(" OR ", [
    "resource.type=\"gce_instance\"",
    "resource.type=\"k8s_container\"",
    "resource.type=\"cloud_run_revision\"",
    "labels.application=\"erlmcp\""
  ]))

  unique_writer_identity = true
  bigquery_options {
    use_partitioned_tables = true
  }
}

# Export logs to Cloud Storage for archival
resource "google_logging_project_sink" "storage" {
  count       = var.log_export_storage_enabled ? 1 : 0
  project     = var.project_id
  name        = "erlmcp-logs-storage-${var.environment}"
  destination = var.log_export_storage_destination

  filter = coalesce(var.log_export_filter, join(" OR ", [
    "resource.type=\"gce_instance\"",
    "resource.type=\"k8s_container\"",
    "resource.type=\"cloud_run_revision\"",
    "labels.application=\"erlmcp\""
  ]))

  unique_writer_identity = true
}

# Export error logs to Pub/Sub for real-time alerting
resource "google_logging_project_sink" "pubsub_errors" {
  count       = var.log_export_pubsub_enabled ? 1 : 0
  project     = var.project_id
  name        = "erlmcp-errors-pubsub-${var.environment}"
  destination = "pubsub.googleapis.com/${var.log_export_pubsub_topic}"

  filter = join(" AND ", [
    "severity>=ERROR",
    "labels.application=\"erlmcp\""
  ])

  unique_writer_identity = true
}

# ============================================================================
# Log Exclusions
# ============================================================================

# Exclude health check logs
resource "google_logging_project_exclusion" "health_check" {
  count       = var.exclude_health_check_logs ? 1 : 0
  project     = var.project_id
  name        = "exclude-health-check-logs"
  description = "Exclude health check request logs"
  filter      = var.health_check_log_filter
  disabled    = false
}

# Exclude successful GET requests
resource "google_logging_project_exclusion" "successful_get" {
  count       = var.exclude_successful_get_logs ? 1 : 0
  project     = var.project_id
  name        = "exclude-successful-get-logs"
  description = "Exclude successful GET request logs"
  filter      = var.successful_get_log_filter
  disabled    = false
}
