# ============================================================================
# Observability Module Outputs
# ============================================================================

output "monitoring_service_id" {
  description = "The ID of the monitoring service"
  value       = google_monitoring_service.erlmcp.service_id
}

output "monitoring_service_name" {
  description = "The full name of the monitoring service"
  value       = google_monitoring_service.erlmcp.name
}

output "availability_slo_id" {
  description = "The ID of the availability SLO"
  value       = var.create_slos ? google_monitoring_slo.availability[0].id : null
}

output "latency_slo_id" {
  description = "The ID of the latency SLO"
  value       = var.create_slos ? google_monitoring_slo.latency[0].id : null
}

output "uptime_check_id" {
  description = "The ID of the uptime check configuration"
  value       = var.create_uptime_check ? google_monitoring_uptime_check_config.erlmcp[0].id : null
}

output "notification_channel_ids" {
  description = "Map of notification channel IDs"
  value = {
    email     = var.notification_channels.email.enabled ? google_monitoring_notification_channel.email[0].id : null
    pagerduty = var.notification_channels.pagerduty.enabled ? google_monitoring_notification_channel.pagerduty[0].id : null
    slack     = var.notification_channels.slack.enabled ? google_monitoring_notification_channel.slack[0].id : null
    webhook   = var.notification_channels.webhook.enabled ? google_monitoring_notification_channel.webhook[0].id : null
  }
}

output "log_based_metric_names" {
  description = "Names of log-based metrics created"
  value = {
    error_count         = google_logging_metric.error_count.name
    request_latency     = google_logging_metric.request_latency.name
    connection_errors   = google_logging_metric.connection_errors.name
    supervisor_restarts = google_logging_metric.supervisor_restarts.name
  }
}

output "log_sink_writer_identities" {
  description = "Service account emails for log sinks (grant permissions to these)"
  value = {
    bigquery = var.log_export_bigquery_enabled ? google_logging_project_sink.bigquery[0].writer_identity : null
    storage  = var.log_export_storage_enabled ? google_logging_project_sink.storage[0].writer_identity : null
    pubsub   = var.log_export_pubsub_enabled ? google_logging_project_sink.pubsub_errors[0].writer_identity : null
  }
}

output "dashboard_urls" {
  description = "URLs to access monitoring dashboards"
  value = {
    main        = "https://console.cloud.google.com/monitoring/dashboards/custom/${module.dashboards.main_dashboard_id}?project=${var.project_id}"
    performance = var.create_performance_dashboard ? "https://console.cloud.google.com/monitoring/dashboards/custom/${module.dashboards.performance_dashboard_id}?project=${var.project_id}" : null
    erlang      = var.create_erlang_dashboard ? "https://console.cloud.google.com/monitoring/dashboards/custom/${module.dashboards.erlang_dashboard_id}?project=${var.project_id}" : null
    security    = var.create_security_dashboard ? "https://console.cloud.google.com/monitoring/dashboards/custom/${module.dashboards.security_dashboard_id}?project=${var.project_id}" : null
    slo         = var.create_slo_dashboard ? "https://console.cloud.google.com/monitoring/dashboards/custom/${module.dashboards.slo_dashboard_id}?project=${var.project_id}" : null
  }
}

output "alert_policy_ids" {
  description = "IDs of created alert policies"
  value       = module.alert_policies.alert_policy_ids
}

output "slo_burn_rate_alert_ids" {
  description = "IDs of SLO burn rate alert policies"
  value = {
    availability = var.create_slos && var.enable_slo_alerts ? google_monitoring_alert_policy.slo_burn_rate_availability[0].id : null
    latency      = var.create_slos && var.enable_slo_alerts ? google_monitoring_alert_policy.slo_burn_rate_latency[0].id : null
  }
}

output "enabled_apis" {
  description = "List of enabled Google Cloud APIs"
  value = [
    google_project_service.monitoring.service,
    google_project_service.logging.service,
    google_project_service.trace.service,
    google_project_service.profiler.service,
    google_project_service.error_reporting.service,
  ]
}
