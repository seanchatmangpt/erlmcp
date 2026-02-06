# ============================================================================
# Dashboards Module Outputs
# ============================================================================

output "main_dashboard_id" {
  description = "The ID of the main operations dashboard"
  value       = google_monitoring_dashboard.main.id
}

output "main_dashboard_name" {
  description = "The name of the main operations dashboard"
  value       = google_monitoring_dashboard.main.id
}

output "slo_dashboard_id" {
  description = "The ID of the SLO dashboard"
  value       = var.create_slo_dashboard ? google_monitoring_dashboard.slo[0].id : null
}

output "erlang_dashboard_id" {
  description = "The ID of the Erlang VM dashboard"
  value       = var.create_erlang_dashboard ? google_monitoring_dashboard.erlang[0].id : null
}

output "performance_dashboard_id" {
  description = "The ID of the performance dashboard"
  value       = var.create_performance_dashboard ? google_monitoring_dashboard.performance[0].id : null
}

output "security_dashboard_id" {
  description = "The ID of the security dashboard"
  value       = var.create_security_dashboard ? google_monitoring_dashboard.security[0].id : null
}

output "all_dashboard_ids" {
  description = "Map of all dashboard IDs"
  value = {
    main        = google_monitoring_dashboard.main.id
    slo         = var.create_slo_dashboard ? google_monitoring_dashboard.slo[0].id : null
    erlang      = var.create_erlang_dashboard ? google_monitoring_dashboard.erlang[0].id : null
    performance = var.create_performance_dashboard ? google_monitoring_dashboard.performance[0].id : null
    security    = var.create_security_dashboard ? google_monitoring_dashboard.security[0].id : null
  }
}
