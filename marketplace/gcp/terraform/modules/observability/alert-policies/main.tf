# ============================================================================
# Alert Policies Module for erlmcp
# Advanced alert policies with multi-window, multi-burn-rate detection
# Updated: 2026-02
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
# High Error Rate Alert (Multi-Window)
# ============================================================================
resource "google_monitoring_alert_policy" "high_error_rate" {
  count        = var.enable_error_rate_alert ? 1 : 0
  project      = var.project_id
  display_name = "erlmcp High Error Rate - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Error rate above threshold (5min window)"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"logging.googleapis.com/user/erlmcp_error_count\"",
        "resource.type=\"gce_instance\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = var.error_rate_alert_threshold
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
        group_by_fields      = ["resource.instance_id"]
      }

      trigger {
        count = 1
      }
    }
  }

  notification_channels = var.critical_channels

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## High Error Rate Alert - erlmcp

      **Severity:** CRITICAL

      **Description:**
      The erlmcp service is experiencing an elevated error rate above ${var.error_rate_alert_threshold} errors/second.

      **Impact:**
      - Users may be experiencing service degradation
      - SLO error budget is being consumed
      - Potential data processing failures

      **Troubleshooting Steps:**

      1. **Check Recent Changes:**
         - Review recent deployments in the last 2 hours
         - Check for configuration changes
         - Verify if any infrastructure changes were made

      2. **Examine Error Logs:**
         ```bash
         gcloud logging read 'severity>=ERROR AND labels.application="erlmcp"' \
           --limit 50 --format json --project ${var.project_id}
         ```

      3. **Check Dependencies:**
         - Database connectivity and performance
         - External API availability
         - Message queue health

      4. **Resource Health:**
         - CPU and memory utilization
         - Network connectivity
         - Disk I/O and space

      5. **Immediate Actions:**
         - If related to recent deployment: rollback immediately
         - If resource exhaustion: scale up or restart services
         - If external dependency: enable circuit breaker/fallback

      **Runbooks:**
      - [Error Rate Troubleshooting](https://docs.erlmcp.dev/runbooks/high-error-rate)
      - [Rollback Procedure](https://docs.erlmcp.dev/runbooks/rollback)

      **Escalation:**
      - Primary: Platform Team (#platform-oncall)
      - Secondary: Engineering Lead
    EOT
  }

  alert_strategy {
    notification_rate_limit {
      period = "3600s"
    }
    auto_close = "86400s"

    notification_channel_strategy {
      notification_channel_names = var.critical_channels
      renotify_interval          = "1800s"
    }
  }

  severity = "CRITICAL"

  user_labels = {
    environment = var.environment
    team        = "platform"
    severity    = "critical"
    alert_type  = "error_rate"
  }
}

# ============================================================================
# High Latency Alert (P95 and P99)
# ============================================================================
resource "google_monitoring_alert_policy" "high_latency_p95" {
  count        = var.enable_latency_alert ? 1 : 0
  project      = var.project_id
  display_name = "erlmcp High Latency P95 - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "P95 latency above ${var.latency_alert_threshold}s"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"logging.googleapis.com/user/erlmcp_request_latency\"",
        "resource.type=\"gce_instance\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = var.latency_alert_threshold
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_DELTA"
        cross_series_reducer = "REDUCE_PERCENTILE_95"
        group_by_fields      = ["metric.endpoint"]
      }
    }
  }

  notification_channels = var.warning_channels

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## High Latency Alert (P95) - erlmcp

      **Severity:** WARNING

      **Description:**
      The 95th percentile request latency has exceeded ${var.latency_alert_threshold}s for 5 minutes.

      **Troubleshooting Steps:**

      1. **Identify Slow Endpoints:**
         - Check dashboard for endpoint breakdown
         - Review slow query logs
         - Check for n+1 queries

      2. **Resource Check:**
         - CPU utilization (target < 70%)
         - Memory pressure
         - Garbage collection frequency

      3. **External Dependencies:**
         - Database query performance
         - External API latency
         - Cache hit rates

      4. **Network Issues:**
         - Regional latency
         - Load balancer health
         - Connection pool saturation

      **Runbook:** https://docs.erlmcp.dev/runbooks/high-latency
    EOT
  }

  alert_strategy {
    notification_rate_limit {
      period = "3600s"
    }
    auto_close = "7200s"
  }

  severity = "WARNING"

  user_labels = {
    environment = var.environment
    team        = "platform"
    severity    = "warning"
    alert_type  = "latency"
  }
}

# ============================================================================
# High Memory Usage Alert (Erlang VM)
# ============================================================================
resource "google_monitoring_alert_policy" "high_memory" {
  count        = var.enable_memory_alert ? 1 : 0
  project      = var.project_id
  display_name = "erlmcp High Memory Usage - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Memory usage above ${var.memory_alert_threshold / 1073741824}GB"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"custom.googleapis.com/erlmcp/memory/usage\"",
        "resource.type=\"gce_instance\"",
        "metric.label.type=\"total\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = var.memory_alert_threshold
      duration        = "300s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }
    }
  }

  notification_channels = var.critical_channels

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## High Memory Usage Alert - erlmcp

      **Severity:** CRITICAL

      **Troubleshooting Steps:**

      1. **Check Erlang VM Memory:**
         ```erlang
         % Connect to running node
         % Check memory breakdown
         erlang:memory().

         % Check process count
         erlang:system_info(process_count).

         % Find memory-heavy processes
         recon:proc_count(memory, 10).
         ```

      2. **Common Causes:**
         - Memory leak in application code
         - ETS table growth
         - Message queue buildup
         - Binary accumulation

      3. **Immediate Actions:**
         - Review process mailbox sizes
         - Check for growing ETS tables
         - Monitor binary memory
         - Consider graceful restart if memory continues growing

      **Runbook:** https://docs.erlmcp.dev/runbooks/memory-issues
    EOT
  }

  alert_strategy {
    auto_close = "3600s"
  }

  severity = "CRITICAL"
}

# ============================================================================
# CPU Utilization Alert
# ============================================================================
resource "google_monitoring_alert_policy" "high_cpu" {
  count        = var.enable_cpu_alert ? 1 : 0
  project      = var.project_id
  display_name = "erlmcp High CPU Usage - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "CPU utilization above ${var.cpu_alert_threshold * 100}%"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"compute.googleapis.com/instance/cpu/utilization\"",
        "resource.type=\"gce_instance\"",
        "metadata.user_labels.service=\"erlmcp\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = var.cpu_alert_threshold
      duration        = "600s"  # 10 minutes

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }
    }
  }

  notification_channels = var.warning_channels

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## High CPU Usage Alert - erlmcp

      **Severity:** WARNING

      **Actions:**
      1. Check for CPU-intensive operations
      2. Review scheduler utilization
      3. Consider horizontal scaling
      4. Check for busy loops or inefficient code

      **Runbook:** https://docs.erlmcp.dev/runbooks/high-cpu
    EOT
  }

  alert_strategy {
    auto_close = "7200s"
  }

  severity = "WARNING"
}

# ============================================================================
# Health Check Failure Alert
# ============================================================================
resource "google_monitoring_alert_policy" "health_check_failure" {
  count        = var.enable_health_check_alert ? 1 : 0
  project      = var.project_id
  display_name = "erlmcp Health Check Failure - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Health check failing from multiple regions"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"monitoring.googleapis.com/uptime_check/check_passed\"",
        "metric.label.check_id=monitoring.regex.full_match(\"erlmcp.*\")"
      ])

      comparison      = "COMPARISON_LT"
      threshold_value = 0.5  # Less than 50% success rate
      duration        = "120s"

      aggregations {
        alignment_period     = "60s"
        cross_series_reducer = "REDUCE_MEAN"
        per_series_aligner   = "ALIGN_FRACTION_TRUE"
      }
    }
  }

  notification_channels = var.critical_channels

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## Health Check Failure - erlmcp

      **Severity:** CRITICAL

      **Description:**
      Multiple uptime check regions are reporting service unavailability.

      **Immediate Actions:**
      1. Check service status across all regions
      2. Verify load balancer health
      3. Check application logs for crashes
      4. Verify database connectivity
      5. Check for DDoS or traffic spike

      **Runbook:** https://docs.erlmcp.dev/runbooks/service-down
    EOT
  }

  alert_strategy {
    notification_rate_limit {
      period = "300s"  # Notify every 5 minutes during outage
    }
  }

  severity = "CRITICAL"
}

# ============================================================================
# Supervisor Restart Rate Alert
# ============================================================================
resource "google_monitoring_alert_policy" "supervisor_restarts" {
  count        = var.enable_supervisor_restart_alert ? 1 : 0
  project      = var.project_id
  display_name = "erlmcp High Supervisor Restart Rate - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Supervisor restart rate above threshold"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"logging.googleapis.com/user/erlmcp_supervisor_restarts\"",
        "resource.type=\"gce_instance\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = var.supervisor_restart_threshold
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
      }
    }
  }

  notification_channels = var.critical_channels

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## High Supervisor Restart Rate - erlmcp

      **Severity:** CRITICAL

      **Description:**
      Erlang supervisors are restarting processes at an elevated rate,
      indicating potential crashes or instability.

      **Troubleshooting:**
      1. Check crash logs and SASL reports
      2. Review supervisor strategy and intensity
      3. Check for cascading failures
      4. Verify process initialization logic

      **Runbook:** https://docs.erlmcp.dev/runbooks/supervisor-restarts
    EOT
  }

  alert_strategy {
    auto_close = "1800s"
  }

  severity = "CRITICAL"
}

# ============================================================================
# Connection Error Rate Alert
# ============================================================================
resource "google_monitoring_alert_policy" "connection_errors" {
  count        = var.enable_connection_error_alert ? 1 : 0
  project      = var.project_id
  display_name = "erlmcp High Connection Error Rate - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Connection errors above threshold"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"logging.googleapis.com/user/erlmcp_connection_errors\"",
        "resource.type=\"gce_instance\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = var.connection_error_threshold
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
        group_by_fields      = ["metric.transport", "metric.error_type"]
      }
    }
  }

  notification_channels = var.warning_channels

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## High Connection Error Rate - erlmcp

      **Severity:** WARNING

      **Actions:**
      1. Check network connectivity
      2. Review connection pool settings
      3. Check for port exhaustion
      4. Verify firewall rules
      5. Review transport-specific issues (stdio/tcp/http/ws/sse)

      **Runbook:** https://docs.erlmcp.dev/runbooks/connection-errors
    EOT
  }

  alert_strategy {
    auto_close = "3600s"
  }

  severity = "WARNING"
}

# ============================================================================
# Anomaly Detection Alert (Machine Learning-based)
# ============================================================================
resource "google_monitoring_alert_policy" "request_rate_anomaly" {
  count        = var.enable_anomaly_detection ? 1 : 0
  project      = var.project_id
  display_name = "erlmcp Request Rate Anomaly - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Anomalous request rate detected"
    condition_matched_log {
      filter = join(" AND ", [
        "resource.type=\"gce_instance\"",
        "labels.application=\"erlmcp\"",
        "jsonPayload.anomaly_detected=\"true\""
      ])
    }
  }

  notification_channels = var.info_channels

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## Request Rate Anomaly Detected - erlmcp

      **Severity:** INFO

      **Description:**
      Machine learning-based anomaly detection has identified unusual traffic patterns.

      **Actions:**
      1. Review traffic patterns in dashboard
      2. Check for legitimate traffic spikes
      3. Investigate potential security issues
      4. Verify autoscaling behavior

      **Runbook:** https://docs.erlmcp.dev/runbooks/anomaly-detection
    EOT
  }

  alert_strategy {
    auto_close = "1800s"
  }

  severity = "INFO"

  user_labels = {
    alert_type = "anomaly_detection"
  }
}
