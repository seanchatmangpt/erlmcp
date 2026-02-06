# ============================================================================
# Alert Policies Module Outputs
# ============================================================================

output "alert_policy_ids" {
  description = "Map of all alert policy IDs"
  value = {
    high_error_rate        = var.enable_error_rate_alert ? google_monitoring_alert_policy.high_error_rate[0].id : null
    high_latency_p95       = var.enable_latency_alert ? google_monitoring_alert_policy.high_latency_p95[0].id : null
    high_memory            = var.enable_memory_alert ? google_monitoring_alert_policy.high_memory[0].id : null
    high_cpu               = var.enable_cpu_alert ? google_monitoring_alert_policy.high_cpu[0].id : null
    health_check_failure   = var.enable_health_check_alert ? google_monitoring_alert_policy.health_check_failure[0].id : null
    supervisor_restarts    = var.enable_supervisor_restart_alert ? google_monitoring_alert_policy.supervisor_restarts[0].id : null
    connection_errors      = var.enable_connection_error_alert ? google_monitoring_alert_policy.connection_errors[0].id : null
    request_rate_anomaly   = var.enable_anomaly_detection ? google_monitoring_alert_policy.request_rate_anomaly[0].id : null
  }
}

output "critical_alert_policy_ids" {
  description = "IDs of critical severity alert policies"
  value = compact([
    var.enable_error_rate_alert ? google_monitoring_alert_policy.high_error_rate[0].id : "",
    var.enable_memory_alert ? google_monitoring_alert_policy.high_memory[0].id : "",
    var.enable_health_check_alert ? google_monitoring_alert_policy.health_check_failure[0].id : "",
    var.enable_supervisor_restart_alert ? google_monitoring_alert_policy.supervisor_restarts[0].id : "",
  ])
}

output "warning_alert_policy_ids" {
  description = "IDs of warning severity alert policies"
  value = compact([
    var.enable_latency_alert ? google_monitoring_alert_policy.high_latency_p95[0].id : "",
    var.enable_cpu_alert ? google_monitoring_alert_policy.high_cpu[0].id : "",
    var.enable_connection_error_alert ? google_monitoring_alert_policy.connection_errors[0].id : "",
  ])
}

output "alert_policy_names" {
  description = "Display names of all alert policies"
  value = {
    high_error_rate        = var.enable_error_rate_alert ? google_monitoring_alert_policy.high_error_rate[0].display_name : null
    high_latency_p95       = var.enable_latency_alert ? google_monitoring_alert_policy.high_latency_p95[0].display_name : null
    high_memory            = var.enable_memory_alert ? google_monitoring_alert_policy.high_memory[0].display_name : null
    high_cpu               = var.enable_cpu_alert ? google_monitoring_alert_policy.high_cpu[0].display_name : null
    health_check_failure   = var.enable_health_check_alert ? google_monitoring_alert_policy.health_check_failure[0].display_name : null
    supervisor_restarts    = var.enable_supervisor_restart_alert ? google_monitoring_alert_policy.supervisor_restarts[0].display_name : null
    connection_errors      = var.enable_connection_error_alert ? google_monitoring_alert_policy.connection_errors[0].display_name : null
    request_rate_anomaly   = var.enable_anomaly_detection ? google_monitoring_alert_policy.request_rate_anomaly[0].display_name : null
  }
}
