# ============================================================================
# Observability Module for erlmcp
# Google Cloud Operations Suite (Monitoring, Logging, Alerting)
# ============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.0.0"
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
# Alert Policies
# ============================================================================
module "alert_policies" {
  source = "./alert-policies.tf"
}

# ============================================================================
# Dashboards
# ============================================================================
module "dashboards" {
  source = "./dashboards.tf"
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
# Service Monitoring
# ============================================================================
resource "google_monitoring_service" "erlmcp" {
  project     = var.project_id
  service_id  = "erlmcp-service"
  display_name = "erlmcp"
  basic_service = {
    service_type = "GENERIC"
  }
}

resource "google_monitoring_slo" "availability" {
  count       = var.create_slos ? 1 : 0
  project     = var.project_id
  service     = google_monitoring_service.erlmcp.service_id
  slo_id      = "availability-slo"
  display_name = "erlmcp Availability SLO"

  # SLO based on uptime check
  basic_sli {
    method = {
      uptime_check_ids = [
        var.create_uptime_check ? google_monitoring_uptime_check_config.erlmcp[0].id : ""
      ]
    }
  }

  goal               = var.slo_availability_goal
  rolling_period_days = var.slo_rolling_period_days
}

resource "google_monitoring_slo" "latency" {
  count       = var.create_slos ? 1 : 0
  project     = var.project_id
  service     = google_monitoring_service.erlmcp.service_id
  slo_id      = "latency-slo"
  display_name = "erlmcp Latency SLO"

  # SLO based on request latency
  basic_sli {
    latency {
      threshold = var.slo_latency_threshold_ms
    }
  }

  goal               = var.slo_latency_goal
  rolling_period_days = var.slo_rolling_period_days
}

# ============================================================================
# Log Sinks for Export
# ============================================================================

# Export logs to BigQuery
resource "google_logging_project_sink" "bigquery" {
  count     = var.log_export_bigquery_enabled ? 1 : 0
  project   = var.project_id
  name      = "erlmcp-logs-bigquery"
  destination = var.log_export_bigquery_destination
  filter    = var.log_export_filter

  unique_writer_identity = true
}

# Export logs to Cloud Storage
resource "google_logging_project_sink" "storage" {
  count     = var.log_export_storage_enabled ? 1 : 0
  project   = var.project_id
  name      = "erlmcp-logs-storage"
  destination = var.log_export_storage_destination
  filter    = var.log_export_filter

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
