# ==============================================================================
# ERLMCP Cloud Monitoring and Alerting
# ==============================================================================
# Comprehensive observability for erlmcp on GCP.
#
# Includes:
# - Uptime checks
# - Alert policies
# - Dashboard
# - Log-based metrics
# - SLO monitoring
# ==============================================================================

# ==============================================================================
# Notification Channels
# ==============================================================================

resource "google_monitoring_notification_channel" "email" {
  for_each = toset(var.alert_email_addresses)

  display_name = "Email: ${each.value}"
  type         = "email"

  labels = {
    email_address = each.value
  }

  enabled = true
}

resource "google_monitoring_notification_channel" "slack" {
  count = var.slack_webhook_url != "" ? 1 : 0

  display_name = "Slack: ${var.app_name}"
  type         = "slack"

  labels = {
    channel_name = var.slack_channel
  }

  sensitive_labels {
    auth_token = var.slack_webhook_url
  }

  enabled = true
}

resource "google_monitoring_notification_channel" "pagerduty" {
  count = var.pagerduty_service_key != "" ? 1 : 0

  display_name = "PagerDuty: ${var.app_name}"
  type         = "pagerduty"

  labels = {
    service_key = var.pagerduty_service_key
  }

  enabled = true
}

# ==============================================================================
# Uptime Checks
# ==============================================================================

resource "google_monitoring_uptime_check_config" "erlmcp_health" {
  display_name = "${var.app_name}-health-check"
  timeout      = "10s"
  period       = "60s"

  http_check {
    path           = "/health"
    port           = 443
    use_ssl        = true
    validate_ssl   = true
    request_method = "GET"

    accepted_response_status_codes {
      status_class = "STATUS_CLASS_2XX"
    }
  }

  monitored_resource {
    type = "uptime_url"
    labels = {
      project_id = var.project_id
      host       = google_cloud_run_v2_service.erlmcp.uri
    }
  }

  content_matchers {
    content = "ok"
    matcher = "CONTAINS_STRING"
  }

  checker_type = "STATIC_IP_CHECKERS"

  selected_regions = [
    "USA",
    "EUROPE",
    "ASIA_PACIFIC"
  ]
}

resource "google_monitoring_uptime_check_config" "erlmcp_ready" {
  display_name = "${var.app_name}-readiness-check"
  timeout      = "10s"
  period       = "300s"

  http_check {
    path           = "/ready"
    port           = 443
    use_ssl        = true
    validate_ssl   = true
    request_method = "GET"

    accepted_response_status_codes {
      status_class = "STATUS_CLASS_2XX"
    }
  }

  monitored_resource {
    type = "uptime_url"
    labels = {
      project_id = var.project_id
      host       = google_cloud_run_v2_service.erlmcp.uri
    }
  }

  checker_type = "STATIC_IP_CHECKERS"

  selected_regions = ["USA"]
}

# ==============================================================================
# Alert Policies
# ==============================================================================

# Alert: Service Down (Uptime Check Failed)
resource "google_monitoring_alert_policy" "uptime_check_failed" {
  display_name = "${var.app_name}: Service Down"
  combiner     = "OR"

  conditions {
    display_name = "Uptime Check Failed"

    condition_threshold {
      filter          = "resource.type = \"uptime_url\" AND metric.type = \"monitoring.googleapis.com/uptime_check/check_passed\" AND metric.labels.check_id = \"${google_monitoring_uptime_check_config.erlmcp_health.uptime_check_id}\""
      comparison      = "COMPARISON_LT"
      threshold_value = 1
      duration        = "300s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_NEXT_OLDER"
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = concat(
    [for ch in google_monitoring_notification_channel.email : ch.id],
    var.pagerduty_service_key != "" ? [google_monitoring_notification_channel.pagerduty[0].id] : [],
    var.slack_webhook_url != "" ? [google_monitoring_notification_channel.slack[0].id] : []
  )

  alert_strategy {
    auto_close = "604800s" # 7 days
  }

  documentation {
    content   = <<-EOT
      ## Service Down Alert

      The erlmcp service health check has failed.

      ### Troubleshooting Steps:
      1. Check Cloud Run service status: `gcloud run services describe ${var.app_name}-service --region ${var.region}`
      2. View recent logs: `gcloud logging read "resource.type=cloud_run_revision AND resource.labels.service_name=${var.app_name}-service" --limit 50`
      3. Check for deployment issues in Cloud Build history
      4. Verify database connectivity if applicable

      ### Runbook Link:
      https://github.com/seanchatmangpt/erlmcp/blob/main/docs/runbooks/service-down.md
    EOT
    mime_type = "text/markdown"
  }

  enabled = true
}

# Alert: High Error Rate
resource "google_monitoring_alert_policy" "high_error_rate" {
  display_name = "${var.app_name}: High Error Rate"
  combiner     = "OR"

  conditions {
    display_name = "Cloud Run 5xx Error Rate > 1%"

    condition_threshold {
      filter = <<-EOT
        resource.type = "cloud_run_revision"
        AND resource.labels.service_name = "${google_cloud_run_v2_service.erlmcp.name}"
        AND metric.type = "run.googleapis.com/request_count"
        AND metric.labels.response_code_class = "5xx"
      EOT

      comparison      = "COMPARISON_GT"
      threshold_value = 0.01 # 1%
      duration        = "300s"

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

  notification_channels = [for ch in google_monitoring_notification_channel.email : ch.id]

  alert_strategy {
    auto_close = "86400s" # 1 day
  }

  documentation {
    content   = "High error rate detected on erlmcp Cloud Run service. Check application logs for details."
    mime_type = "text/markdown"
  }

  enabled = true
}

# Alert: High Latency (P99 > 1s)
resource "google_monitoring_alert_policy" "high_latency" {
  display_name = "${var.app_name}: High Latency"
  combiner     = "OR"

  conditions {
    display_name = "P99 Latency > 1000ms"

    condition_threshold {
      filter = <<-EOT
        resource.type = "cloud_run_revision"
        AND resource.labels.service_name = "${google_cloud_run_v2_service.erlmcp.name}"
        AND metric.type = "run.googleapis.com/request_latencies"
      EOT

      comparison      = "COMPARISON_GT"
      threshold_value = 1000 # 1000ms
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_PERCENTILE_99"
        cross_series_reducer = "REDUCE_MEAN"
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = [for ch in google_monitoring_notification_channel.email : ch.id]

  alert_strategy {
    auto_close = "86400s"
  }

  documentation {
    content   = "P99 latency is above 1 second. Investigate potential performance issues."
    mime_type = "text/markdown"
  }

  enabled = true
}

# Alert: High Memory Usage
resource "google_monitoring_alert_policy" "high_memory" {
  display_name = "${var.app_name}: High Memory Usage"
  combiner     = "OR"

  conditions {
    display_name = "Memory Utilization > 85%"

    condition_threshold {
      filter = <<-EOT
        resource.type = "cloud_run_revision"
        AND resource.labels.service_name = "${google_cloud_run_v2_service.erlmcp.name}"
        AND metric.type = "run.googleapis.com/container/memory/utilizations"
      EOT

      comparison      = "COMPARISON_GT"
      threshold_value = 0.85 # 85%
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_MEAN"
        cross_series_reducer = "REDUCE_MEAN"
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = [for ch in google_monitoring_notification_channel.email : ch.id]

  alert_strategy {
    auto_close = "86400s"
  }

  documentation {
    content   = "Memory utilization is above 85%. BEAM may need more memory or there could be a memory leak."
    mime_type = "text/markdown"
  }

  enabled = true
}

# Alert: Database CPU High (if Cloud SQL enabled)
resource "google_monitoring_alert_policy" "db_high_cpu" {
  count = var.enable_cloud_sql ? 1 : 0

  display_name = "${var.app_name}: Database High CPU"
  combiner     = "OR"

  conditions {
    display_name = "Cloud SQL CPU > 80%"

    condition_threshold {
      filter = <<-EOT
        resource.type = "cloudsql_database"
        AND resource.labels.database_id = "${var.project_id}:${google_sql_database_instance.erlmcp[0].name}"
        AND metric.type = "cloudsql.googleapis.com/database/cpu/utilization"
      EOT

      comparison      = "COMPARISON_GT"
      threshold_value = 0.80 # 80%
      duration        = "300s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = [for ch in google_monitoring_notification_channel.email : ch.id]

  enabled = true
}

# ==============================================================================
# Log-Based Metrics
# ==============================================================================

# Count of Erlang crashes
resource "google_logging_metric" "erlang_crashes" {
  name   = "${var.app_name}/erlang_crashes"
  filter = <<-EOT
    resource.type = "cloud_run_revision"
    AND resource.labels.service_name = "${google_cloud_run_v2_service.erlmcp.name}"
    AND textPayload =~ "CRASH REPORT"
  EOT

  metric_descriptor {
    metric_kind = "DELTA"
    value_type  = "INT64"
    unit        = "1"

    labels {
      key         = "severity"
      value_type  = "STRING"
      description = "Log severity"
    }
  }

  label_extractors = {
    "severity" = "EXTRACT(severity)"
  }
}

# Count of supervisor restarts
resource "google_logging_metric" "supervisor_restarts" {
  name   = "${var.app_name}/supervisor_restarts"
  filter = <<-EOT
    resource.type = "cloud_run_revision"
    AND resource.labels.service_name = "${google_cloud_run_v2_service.erlmcp.name}"
    AND textPayload =~ "supervisor.*restart"
  EOT

  metric_descriptor {
    metric_kind = "DELTA"
    value_type  = "INT64"
    unit        = "1"
  }
}

# ==============================================================================
# Dashboard
# ==============================================================================

resource "google_monitoring_dashboard" "erlmcp" {
  dashboard_json = jsonencode({
    displayName = "${var.app_name} Dashboard"
    gridLayout = {
      columns = 2
      widgets = [
        {
          title = "Request Rate"
          xyChart = {
            dataSets = [{
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "resource.type = \"cloud_run_revision\" AND resource.labels.service_name = \"${google_cloud_run_v2_service.erlmcp.name}\" AND metric.type = \"run.googleapis.com/request_count\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_RATE"
                  }
                }
              }
              plotType = "LINE"
            }]
          }
        },
        {
          title = "Latency (P50, P95, P99)"
          xyChart = {
            dataSets = [
              {
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type = \"cloud_run_revision\" AND resource.labels.service_name = \"${google_cloud_run_v2_service.erlmcp.name}\" AND metric.type = \"run.googleapis.com/request_latencies\""
                    aggregation = {
                      alignmentPeriod  = "60s"
                      perSeriesAligner = "ALIGN_PERCENTILE_50"
                    }
                  }
                }
                plotType = "LINE"
              },
              {
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type = \"cloud_run_revision\" AND resource.labels.service_name = \"${google_cloud_run_v2_service.erlmcp.name}\" AND metric.type = \"run.googleapis.com/request_latencies\""
                    aggregation = {
                      alignmentPeriod  = "60s"
                      perSeriesAligner = "ALIGN_PERCENTILE_95"
                    }
                  }
                }
                plotType = "LINE"
              },
              {
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type = \"cloud_run_revision\" AND resource.labels.service_name = \"${google_cloud_run_v2_service.erlmcp.name}\" AND metric.type = \"run.googleapis.com/request_latencies\""
                    aggregation = {
                      alignmentPeriod  = "60s"
                      perSeriesAligner = "ALIGN_PERCENTILE_99"
                    }
                  }
                }
                plotType = "LINE"
              }
            ]
          }
        },
        {
          title = "Instance Count"
          xyChart = {
            dataSets = [{
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "resource.type = \"cloud_run_revision\" AND resource.labels.service_name = \"${google_cloud_run_v2_service.erlmcp.name}\" AND metric.type = \"run.googleapis.com/container/instance_count\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_MEAN"
                  }
                }
              }
              plotType = "LINE"
            }]
          }
        },
        {
          title = "Memory Utilization"
          xyChart = {
            dataSets = [{
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "resource.type = \"cloud_run_revision\" AND resource.labels.service_name = \"${google_cloud_run_v2_service.erlmcp.name}\" AND metric.type = \"run.googleapis.com/container/memory/utilizations\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_MEAN"
                  }
                }
              }
              plotType = "LINE"
            }]
          }
        },
        {
          title = "Error Rate by Response Code"
          xyChart = {
            dataSets = [{
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "resource.type = \"cloud_run_revision\" AND resource.labels.service_name = \"${google_cloud_run_v2_service.erlmcp.name}\" AND metric.type = \"run.googleapis.com/request_count\" AND metric.labels.response_code_class != \"2xx\""
                  aggregation = {
                    alignmentPeriod    = "60s"
                    perSeriesAligner   = "ALIGN_RATE"
                    groupByFields      = ["metric.labels.response_code_class"]
                    crossSeriesReducer = "REDUCE_SUM"
                  }
                }
              }
              plotType = "STACKED_BAR"
            }]
          }
        },
        {
          title = "Uptime Check"
          scorecard = {
            timeSeriesQuery = {
              timeSeriesFilter = {
                filter = "resource.type = \"uptime_url\" AND metric.type = \"monitoring.googleapis.com/uptime_check/check_passed\" AND metric.labels.check_id = \"${google_monitoring_uptime_check_config.erlmcp_health.uptime_check_id}\""
              }
            }
            sparkChartView = {
              sparkChartType = "SPARK_LINE"
            }
          }
        }
      ]
    }
  })
}
