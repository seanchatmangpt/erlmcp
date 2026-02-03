# ============================================================================
# Alert Policies for erlmcp
# ============================================================================

# Alert: High Error Rate
resource "google_monitoring_alert_policy" "high_error_rate" {
  project       = var.project_id
  display_name  = "erlmcp High Error Rate Alert"
  combiner      = "OR"
  enabled       = var.enable_error_rate_alert

  conditions {
    display_name = "Error rate above threshold"
    condition_threshold {
      filter          = "resource.type=\"gce_instance\" metric.type=\"custom.googleapis.com/erlmcp/http/requests\" metric.label.status=\"5xx\""
      comparison      = "COMPARISON_GT"
      duration        = var.error_rate_alert_duration
      threshold_value = var.error_rate_alert_threshold

      aggregations {
        alignment_period     = var.error_rate_alert_alignment_period
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
      }
    }
  }

  notification_channels = compact([
    var.notification_channels.email.enabled ? google_monitoring_notification_channel.email[0].id : "",
    var.notification_channels.pagerduty.enabled ? google_monitoring_notification_channel.pagerduty[0].id : "",
    var.notification_channels.slack.enabled ? google_monitoring_notification_channel.slack[0].id : "",
  ])

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## High Error Rate Alert

      The erlmcp service is experiencing an elevated error rate.

      **Troubleshooting Steps:**
      1. Check the error logs in Cloud Logging
      2. Verify database connectivity
      3. Check resource utilization (CPU, memory)
      4. Review recent deployments

      **Runbook:** https://docs.erlmcp.dev/runbooks/high-error-rate
    EOT
  }

  alert_strategy {
    notification_rate_limit {
      period = var.error_rate_alert_notification_period
    }
    auto_close = var.error_rate_alert_auto_close
  }
}

# Alert: High Latency
resource "google_monitoring_alert_policy" "high_latency" {
  project       = var.project_id
  display_name  = "erlmcp High Latency Alert"
  combiner      = "OR"
  enabled       = var.enable_latency_alert

  conditions {
    display_name = "P99 latency above threshold"
    condition_threshold {
      filter          = "resource.type=\"gce_instance\" metric.type=\"custom.googleapis.com/erlmcp/http/latency\" metric.label.percentile=\"99\""
      comparison      = "COMPARISON_GT"
      duration        = var.latency_alert_duration
      threshold_value = var.latency_alert_threshold

      aggregations {
        alignment_period     = var.latency_alert_alignment_period
        per_series_aligner   = "ALIGN_PERCENTILE_99"
        cross_series_reducer = "REDUCE_MEAN"
      }
    }
  }

  notification_channels = compact([
    var.notification_channels.email.enabled ? google_monitoring_notification_channel.email[0].id : "",
    var.notification_channels.slack.enabled ? google_monitoring_notification_channel.slack[0].id : "",
  ])

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## High Latency Alert

      The erlmcp service is experiencing slow response times.

      **Troubleshooting Steps:**
      1. Check CPU and memory utilization
      2. Review database query performance
      3. Check network latency
      4. Review cache hit rates

      **Runbook:** https://docs.erlmcp.dev/runbooks/high-latency
    EOT
  }

  alert_strategy {
    notification_rate_limit {
      period = var.latency_alert_notification_period
    }
  }
}

# Alert: High Memory Usage
resource "google_monitoring_alert_policy" "high_memory" {
  project       = var.project_id
  display_name  = "erlmcp High Memory Usage Alert"
  combiner      = "OR"
  enabled       = var.enable_memory_alert

  conditions {
    display_name = "Memory usage above threshold"
    condition_threshold {
      filter          = "resource.type=\"gce_instance\" metric.type=\"custom.googleapis.com/erlmcp/memory/usage\" metric.label.type=\"total\""
      comparison      = "COMPARISON_GT"
      duration        = var.memory_alert_duration
      threshold_value = var.memory_alert_threshold

      aggregations {
        alignment_period   = var.memory_alert_alignment_period
        per_series_aligner = "ALIGN_MEAN"
      }
    }
  }

  notification_channels = compact([
    var.notification_channels.email.enabled ? google_monitoring_notification_channel.email[0].id : "",
    var.notification_channels.pagerduty.enabled ? google_monitoring_notification_channel.pagerduty[0].id : "",
  ])

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## High Memory Usage Alert

      The erlmcp service is consuming excessive memory.

      **Troubleshooting Steps:**
      1. Check for memory leaks
      2. Review Erlang process count
      3. Check ETS table sizes
      4. Review garbage collection stats

      **Runbook:** https://docs.erlmcp.dev/runbooks/high-memory
    EOT
  }

  alert_strategy {
    auto_close = var.memory_alert_auto_close
  }
}

# Alert: High CPU Usage
resource "google_monitoring_alert_policy" "high_cpu" {
  project       = var.project_id
  display_name  = "erlmcp High CPU Usage Alert"
  combiner      = "OR"
  enabled       = var.enable_cpu_alert

  conditions {
    display_name = "CPU utilization above threshold"
    condition_threshold {
      filter          = "resource.type=\"gce_instance\" metric.type=\"compute.googleapis.com/instance/cpu/utilization\""
      comparison      = "COMPARISON_GT"
      duration        = var.cpu_alert_duration
      threshold_value = var.cpu_alert_threshold

      aggregations {
        alignment_period   = var.cpu_alert_alignment_period
        per_series_aligner = "ALIGN_MEAN"
      }
    }
  }

  notification_channels = compact([
    var.notification_channels.email.enabled ? google_monitoring_notification_channel.email[0].id : "",
  ])
}

# Alert: Health Check Failure
resource "google_monitoring_alert_policy" "health_check" {
  project       = var.project_id
  display_name  = "erlmcp Health Check Failure Alert"
  combiner      = "OR"
  enabled       = var.enable_health_check_alert

  conditions {
    display_name = "Health check failing"
    condition_threshold {
      filter          = "resource.type=\"uptime_url\" metric.type=\"uptime.googleapis.com/check/request_count\" metric.label.\"response_code\">=400"
      comparison      = "COMPARISON_GT"
      duration        = var.health_check_alert_duration
      threshold_value = 0

      aggregations {
        alignment_period     = var.health_check_alert_alignment_period
        per_series_aligner   = "ALIGN_COUNT"
        cross_series_reducer = "REDUCE_SUM"
      }
    }
  }

  notification_channels = compact([
    var.notification_channels.email.enabled ? google_monitoring_notification_channel.email[0].id : "",
    var.notification_channels.pagerduty.enabled ? google_monitoring_notification_channel.pagerduty[0].id : "",
  ])

  documentation {
    content   = "erlmcp health check is failing. Immediate investigation required."
    mime_type = "text/markdown"
  }
}

# Alert: Low Process Count
resource "google_monitoring_alert_policy" "low_process_count" {
  project       = var.project_id
  display_name  = "erlmcp Low Process Count Alert"
  combiner      = "OR"
  enabled       = var.enable_process_count_alert

  conditions {
    display_name = "Process count below threshold"
    condition_threshold {
      filter          = "resource.type=\"gce_instance\" metric.type=\"custom.googleapis.com/erlmcp/erlang/processes\""
      comparison      = "COMPARISON_LT"
      duration        = var.process_count_alert_duration
      threshold_value = var.process_count_alert_threshold

      aggregations {
        alignment_period   = var.process_count_alert_alignment_period
        per_series_aligner = "ALIGN_MIN"
      }
    }
  }

  documentation {
    content   = "Erlang process count is abnormally low. The VM may be crashing."
    mime_type = "text/markdown"
  }
}
