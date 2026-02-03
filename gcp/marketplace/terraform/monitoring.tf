# ============================================================================
# Cloud Operations Suite Configuration for erlmcp
# ============================================================================
# Comprehensive observability: Logging, Monitoring, Tracing, Alerting
#
# DOCKER-ONLY CONSTITUTION: Observability enables evidence-based operations
# ============================================================================

# ============================================================================
# Monitoring Dashboard
# ============================================================================

resource "google_monitoring_dashboard" "erlmcp" {
  project        = var.project_id
  dashboard_json = jsonencode({
    displayName = "erlmcp - ${var.environment}"
    gridLayout = {
      columns = 12
      widgets = concat(
        # Overview section
        [
          {
            title = "Service Overview"
            text = {
              content = "# erlmcp MCP SDK\n**Environment:** ${var.environment}\n**Version:** ${var.erlmcp_version}"
              format  = "MARKDOWN"
            }
          }
        ],
        # Request metrics
        [
          {
            title = "Request Rate"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/erlmcp/request_total\" resource.type=\"gce_instance\""
                      aggregation = {
                        alignmentPeriod    = "60s"
                        perSeriesAligner   = "ALIGN_RATE"
                        crossSeriesReducer = "REDUCE_SUM"
                        groupByFields      = ["resource.label.instance_id"]
                      }
                    }
                  }
                  plotType = "LINE"
                }
              ]
              yAxis = {
                label = "req/s"
                scale = "LINEAR"
              }
            }
          },
          {
            title = "Request Latency (P99)"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/erlmcp/request_latency\" resource.type=\"gce_instance\""
                      aggregation = {
                        alignmentPeriod    = "60s"
                        perSeriesAligner   = "ALIGN_PERCENTILE_99"
                        crossSeriesReducer = "REDUCE_MAX"
                      }
                    }
                  }
                  plotType = "LINE"
                }
              ]
              yAxis = {
                label = "ms"
                scale = "LINEAR"
              }
            }
          },
          {
            title = "Error Rate"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/erlmcp/error_total\" resource.type=\"gce_instance\""
                      aggregation = {
                        alignmentPeriod    = "60s"
                        perSeriesAligner   = "ALIGN_RATE"
                        crossSeriesReducer = "REDUCE_SUM"
                      }
                    }
                  }
                  plotType = "LINE"
                }
              ]
              yAxis = {
                label = "errors/s"
                scale = "LINEAR"
              }
            }
          }
        ],
        # Erlang VM metrics
        [
          {
            title = "Erlang Process Count"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/erlmcp/vm_process_count\" resource.type=\"gce_instance\""
                      aggregation = {
                        alignmentPeriod    = "60s"
                        perSeriesAligner   = "ALIGN_MEAN"
                        crossSeriesReducer = "REDUCE_SUM"
                        groupByFields      = ["resource.label.instance_id"]
                      }
                    }
                  }
                  plotType = "LINE"
                }
              ]
            }
          },
          {
            title = "Erlang Memory Usage"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/erlmcp/vm_memory_bytes\" resource.type=\"gce_instance\""
                      aggregation = {
                        alignmentPeriod    = "60s"
                        perSeriesAligner   = "ALIGN_MEAN"
                        crossSeriesReducer = "REDUCE_SUM"
                        groupByFields      = ["metric.label.type"]
                      }
                    }
                  }
                  plotType = "STACKED_AREA"
                }
              ]
              yAxis = {
                label = "bytes"
                scale = "LINEAR"
              }
            }
          },
          {
            title = "Scheduler Utilization"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/erlmcp/vm_scheduler_utilization\" resource.type=\"gce_instance\""
                      aggregation = {
                        alignmentPeriod    = "60s"
                        perSeriesAligner   = "ALIGN_MEAN"
                        crossSeriesReducer = "REDUCE_MEAN"
                      }
                    }
                  }
                  plotType = "LINE"
                }
              ]
              yAxis = {
                label = "%"
                scale = "LINEAR"
              }
            }
          }
        ],
        # Session metrics
        [
          {
            title = "Active Sessions"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"custom.googleapis.com/erlmcp/active_sessions\" resource.type=\"gce_instance\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_MEAN"
                  }
                }
              }
              sparkChartView = {
                sparkChartType = "SPARK_LINE"
              }
            }
          },
          {
            title = "Connection Queue"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"custom.googleapis.com/erlmcp/connection_queue_size\" resource.type=\"gce_instance\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_MAX"
                  }
                }
              }
              sparkChartView = {
                sparkChartType = "SPARK_LINE"
              }
              thresholds = [
                {
                  value     = 100
                  color     = "YELLOW"
                  direction = "ABOVE"
                }
              ]
            }
          }
        ]
      )
    }
    labels = local.common_labels
  })
}

# ============================================================================
# Notification Channels
# ============================================================================

resource "google_monitoring_notification_channel" "email" {
  count = var.alert_email != "" ? 1 : 0

  display_name = "erlmcp ${var.environment} Email"
  project      = var.project_id
  type         = "email"

  labels = {
    email_address = var.alert_email
  }
}

resource "google_monitoring_notification_channel" "slack" {
  count = var.slack_webhook_url != "" ? 1 : 0

  display_name = "erlmcp ${var.environment} Slack"
  project      = var.project_id
  type         = "slack"

  labels = {
    channel_name = var.slack_channel
  }

  sensitive_labels {
    auth_token = var.slack_webhook_url
  }
}

resource "google_monitoring_notification_channel" "pagerduty" {
  count = var.pagerduty_service_key != "" ? 1 : 0

  display_name = "erlmcp ${var.environment} PagerDuty"
  project      = var.project_id
  type         = "pagerduty"

  sensitive_labels {
    service_key = var.pagerduty_service_key
  }
}

locals {
  notification_channels = compact([
    var.alert_email != "" ? google_monitoring_notification_channel.email[0].name : "",
    var.slack_webhook_url != "" ? google_monitoring_notification_channel.slack[0].name : "",
    var.pagerduty_service_key != "" ? google_monitoring_notification_channel.pagerduty[0].name : ""
  ])
}

# ============================================================================
# Alert Policies
# ============================================================================

resource "google_monitoring_alert_policy" "high_error_rate" {
  display_name = "erlmcp ${var.environment} - High Error Rate"
  project      = var.project_id
  combiner     = "OR"

  conditions {
    display_name = "Error rate > 5%"

    condition_threshold {
      filter          = "metric.type=\"custom.googleapis.com/erlmcp/error_total\" resource.type=\"gce_instance\""
      duration        = "300s"
      comparison      = "COMPARISON_GT"
      threshold_value = 0.05

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = local.notification_channels

  alert_strategy {
    auto_close = "604800s"  # 7 days
  }

  documentation {
    content   = "High error rate detected in erlmcp ${var.environment}. Check logs for details."
    mime_type = "text/markdown"
  }

  user_labels = local.common_labels
}

resource "google_monitoring_alert_policy" "high_latency" {
  display_name = "erlmcp ${var.environment} - High Latency"
  project      = var.project_id
  combiner     = "OR"

  conditions {
    display_name = "P99 latency > 2s"

    condition_threshold {
      filter          = "metric.type=\"custom.googleapis.com/erlmcp/request_latency\" resource.type=\"gce_instance\""
      duration        = "300s"
      comparison      = "COMPARISON_GT"
      threshold_value = 2000

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_PERCENTILE_99"
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = local.notification_channels

  documentation {
    content   = "High latency detected in erlmcp ${var.environment}. Check resource utilization."
    mime_type = "text/markdown"
  }

  user_labels = local.common_labels
}

resource "google_monitoring_alert_policy" "instance_down" {
  display_name = "erlmcp ${var.environment} - Instance Down"
  project      = var.project_id
  combiner     = "OR"

  conditions {
    display_name = "Uptime check failed"

    condition_threshold {
      filter          = "metric.type=\"monitoring.googleapis.com/uptime_check/check_passed\" resource.type=\"uptime_url\""
      duration        = "60s"
      comparison      = "COMPARISON_LT"
      threshold_value = 1

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_NEXT_OLDER"
        cross_series_reducer = "REDUCE_COUNT_FALSE"
      }

      trigger {
        count = 2
      }
    }
  }

  notification_channels = local.notification_channels

  alert_strategy {
    auto_close = "604800s"
  }

  documentation {
    content   = "erlmcp instance is down in ${var.environment}. Check instance health."
    mime_type = "text/markdown"
  }

  user_labels = local.common_labels
}

resource "google_monitoring_alert_policy" "high_memory" {
  display_name = "erlmcp ${var.environment} - High Memory"
  project      = var.project_id
  combiner     = "OR"

  conditions {
    display_name = "Memory > 85%"

    condition_threshold {
      filter          = "metric.type=\"custom.googleapis.com/erlmcp/vm_memory_bytes\" resource.type=\"gce_instance\" metric.label.type=\"total\""
      duration        = "300s"
      comparison      = "COMPARISON_GT"
      threshold_value = 0.85

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = local.notification_channels

  documentation {
    content   = "High memory usage in erlmcp ${var.environment}. Consider scaling or investigating memory leaks."
    mime_type = "text/markdown"
  }

  user_labels = local.common_labels
}

# ============================================================================
# Uptime Checks
# ============================================================================

resource "google_monitoring_uptime_check_config" "health" {
  count = local.deploy_gce || local.deploy_cloud_run ? 1 : 0

  display_name = "erlmcp ${var.environment} Health Check"
  project      = var.project_id
  timeout      = "10s"
  period       = "60s"

  http_check {
    path         = "/health"
    port         = 8080
    use_ssl      = var.gce_domain != "" || local.deploy_cloud_run
    validate_ssl = true
  }

  monitored_resource {
    type = "uptime_url"
    labels = {
      project_id = var.project_id
      host       = local.deploy_cloud_run ? trimsuffix(trimprefix(google_cloud_run_v2_service.erlmcp[0].uri, "https://"), "/") : (var.gce_domain != "" ? var.gce_domain : google_compute_global_address.erlmcp[0].address)
    }
  }

  content_matchers {
    content = "healthy"
    matcher = "CONTAINS_STRING"
  }
}

# ============================================================================
# Log-based Metrics
# ============================================================================

resource "google_logging_metric" "error_count" {
  name    = "erlmcp/${var.environment}/error_count"
  project = var.project_id

  filter = <<-EOT
    resource.type="gce_instance" OR resource.type="cloud_run_revision" OR resource.type="k8s_container"
    logName=~"projects/${var.project_id}/logs/erlmcp"
    severity>=ERROR
  EOT

  metric_descriptor {
    metric_kind = "DELTA"
    value_type  = "INT64"
    unit        = "1"
    labels {
      key         = "severity"
      value_type  = "STRING"
      description = "Log severity level"
    }
  }

  label_extractors = {
    "severity" = "EXTRACT(severity)"
  }
}

resource "google_logging_metric" "request_latency" {
  name    = "erlmcp/${var.environment}/request_latency"
  project = var.project_id

  filter = <<-EOT
    resource.type="gce_instance" OR resource.type="cloud_run_revision" OR resource.type="k8s_container"
    logName=~"projects/${var.project_id}/logs/erlmcp"
    jsonPayload.latency_ms:*
  EOT

  metric_descriptor {
    metric_kind = "DELTA"
    value_type  = "DISTRIBUTION"
    unit        = "ms"
  }

  value_extractor = "EXTRACT(jsonPayload.latency_ms)"

  bucket_options {
    exponential_buckets {
      num_finite_buckets = 64
      growth_factor      = 2
      scale              = 0.01
    }
  }
}

# ============================================================================
# Log Sinks (optional export)
# ============================================================================

resource "google_logging_project_sink" "bigquery" {
  count = var.enable_log_export_bigquery ? 1 : 0

  name        = "erlmcp-${var.environment}-bigquery"
  project     = var.project_id
  destination = "bigquery.googleapis.com/projects/${var.project_id}/datasets/${var.log_export_dataset}"

  filter = <<-EOT
    resource.type="gce_instance" OR resource.type="cloud_run_revision" OR resource.type="k8s_container"
    logName=~"projects/${var.project_id}/logs/erlmcp"
  EOT

  unique_writer_identity = true

  bigquery_options {
    use_partitioned_tables = true
  }
}

resource "google_logging_project_sink" "gcs" {
  count = var.enable_log_export_gcs ? 1 : 0

  name        = "erlmcp-${var.environment}-gcs"
  project     = var.project_id
  destination = "storage.googleapis.com/${var.log_export_bucket}"

  filter = <<-EOT
    resource.type="gce_instance" OR resource.type="cloud_run_revision" OR resource.type="k8s_container"
    logName=~"projects/${var.project_id}/logs/erlmcp"
  EOT

  unique_writer_identity = true
}

# ============================================================================
# Variables
# ============================================================================

variable "alert_email" {
  description = "Email for alert notifications"
  type        = string
  default     = ""
}

variable "slack_webhook_url" {
  description = "Slack webhook URL for notifications"
  type        = string
  default     = ""
  sensitive   = true
}

variable "slack_channel" {
  description = "Slack channel for notifications"
  type        = string
  default     = "#alerts"
}

variable "pagerduty_service_key" {
  description = "PagerDuty service key for notifications"
  type        = string
  default     = ""
  sensitive   = true
}

variable "enable_log_export_bigquery" {
  description = "Export logs to BigQuery"
  type        = bool
  default     = false
}

variable "log_export_dataset" {
  description = "BigQuery dataset for log export"
  type        = string
  default     = "erlmcp_logs"
}

variable "enable_log_export_gcs" {
  description = "Export logs to Cloud Storage"
  type        = bool
  default     = false
}

variable "log_export_bucket" {
  description = "GCS bucket for log export"
  type        = string
  default     = ""
}

# ============================================================================
# Outputs
# ============================================================================

output "dashboard_url" {
  description = "Monitoring dashboard URL"
  value       = "https://console.cloud.google.com/monitoring/dashboards/builder/${google_monitoring_dashboard.erlmcp.id}?project=${var.project_id}"
}
