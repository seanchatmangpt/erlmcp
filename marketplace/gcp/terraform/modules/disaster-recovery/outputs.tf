# Disaster Recovery Module Outputs

output "dr_configuration" {
  description = "Disaster recovery configuration details"
  value = {
    enabled       = var.enable_disaster_recovery
    failover_type = var.failover_type
    rpo_minutes   = var.recovery_point_objective_minutes
    rto_minutes   = var.recovery_time_objective_minutes
  }
}

output "regions" {
  description = "DR region configuration"
  value = {
    primary          = var.primary_region
    secondary        = var.secondary_region
    multi_region     = var.multi_region_location
  }
}

output "storage_primary_bucket" {
  description = "Primary storage bucket name"
  value       = google_storage_bucket.primary.name
}

output "storage_secondary_bucket" {
  description = "Secondary storage bucket name"
  value       = google_storage_bucket.secondary.name
}

output "storage_multi_region_bucket" {
  description = "Multi-region storage bucket name"
  value       = google_storage_bucket.multi_region.name
}

output "storage_primary_bucket_url" {
  description = "Primary storage bucket URL"
  value       = google_storage_bucket.primary.url
}

output "storage_secondary_bucket_url" {
  description = "Secondary storage bucket URL"
  value       = google_storage_bucket.secondary.url
}

output "database_primary_instance" {
  description = "Primary database instance name"
  value       = google_sql_database_instance.primary.name
}

output "database_primary_connection_name" {
  description = "Primary database connection name"
  value       = google_sql_database_instance.primary.connection_name
  sensitive   = true
}

output "database_primary_ip_address" {
  description = "Primary database IP address"
  value       = google_sql_database_instance.primary.private_ip_address
  sensitive   = true
}

output "database_secondary_instance" {
  description = "Secondary database replica instance name"
  value       = var.enable_disaster_recovery ? google_sql_database_instance.secondary_replica[0].name : null
}

output "database_secondary_connection_name" {
  description = "Secondary database connection name"
  value       = var.enable_disaster_recovery ? google_sql_database_instance.secondary_replica[0].connection_name : null
  sensitive   = true
}

output "spanner_instance" {
  description = "Spanner instance name"
  value       = var.enable_spanner ? google_spanner_instance.multi_region[0].name : null
}

output "spanner_database" {
  description = "Spanner database name"
  value       = var.enable_spanner ? google_spanner_database.database[0].name : null
}

output "gke_primary_cluster_name" {
  description = "Primary GKE cluster name"
  value       = google_container_cluster.primary.name
}

output "gke_primary_cluster_endpoint" {
  description = "Primary GKE cluster endpoint"
  value       = google_container_cluster.primary.endpoint
  sensitive   = true
}

output "gke_primary_cluster_ca_certificate" {
  description = "Primary GKE cluster CA certificate"
  value       = google_container_cluster.primary.master_auth[0].cluster_ca_certificate
  sensitive   = true
}

output "gke_secondary_cluster_name" {
  description = "Secondary GKE cluster name"
  value       = var.enable_disaster_recovery ? google_container_cluster.secondary[0].name : null
}

output "gke_secondary_cluster_endpoint" {
  description = "Secondary GKE cluster endpoint"
  value       = var.enable_disaster_recovery ? google_container_cluster.secondary[0].endpoint : null
  sensitive   = true
}

output "gke_secondary_cluster_ca_certificate" {
  description = "Secondary GKE cluster CA certificate"
  value       = var.enable_disaster_recovery ? google_container_cluster.secondary[0].master_auth[0].cluster_ca_certificate : null
  sensitive   = true
}

output "global_ip_address" {
  description = "Global load balancer IP address"
  value       = google_compute_global_address.default.address
}

output "global_ip_name" {
  description = "Global load balancer IP name"
  value       = google_compute_global_address.default.name
}

output "health_check_id" {
  description = "Health check ID"
  value       = google_compute_health_check.default.id
}

output "backend_service_primary_id" {
  description = "Primary backend service ID"
  value       = google_compute_backend_service.primary.id
}

output "backend_service_secondary_id" {
  description = "Secondary backend service ID"
  value       = var.enable_disaster_recovery ? google_compute_backend_service.secondary[0].id : null
}

output "redis_primary_instance" {
  description = "Primary Redis instance name"
  value       = google_redis_instance.primary.name
}

output "redis_primary_host" {
  description = "Primary Redis host"
  value       = google_redis_instance.primary.host
  sensitive   = true
}

output "redis_primary_port" {
  description = "Primary Redis port"
  value       = google_redis_instance.primary.port
}

output "redis_primary_auth_string" {
  description = "Primary Redis auth string"
  value       = google_redis_instance.primary.auth_string
  sensitive   = true
}

output "redis_secondary_instance" {
  description = "Secondary Redis instance name"
  value       = var.enable_disaster_recovery ? google_redis_instance.secondary[0].name : null
}

output "redis_secondary_host" {
  description = "Secondary Redis host"
  value       = var.enable_disaster_recovery ? google_redis_instance.secondary[0].host : null
  sensitive   = true
}

output "redis_secondary_port" {
  description = "Secondary Redis port"
  value       = var.enable_disaster_recovery ? google_redis_instance.secondary[0].port : null
}

output "pubsub_dr_events_topic" {
  description = "DR events Pub/Sub topic ID"
  value       = google_pubsub_topic.dr_events.id
}

output "pubsub_dr_events_subscription" {
  description = "DR events subscription ID"
  value       = google_pubsub_subscription.dr_events.id
}

output "pubsub_dr_dlq_topic" {
  description = "DR dead letter queue topic ID"
  value       = google_pubsub_topic.dr_dlq.id
}

output "monitoring_alert_primary_down" {
  description = "Primary region down alert policy name"
  value       = google_monitoring_alert_policy.primary_region_down.name
}

output "monitoring_alert_replication_lag" {
  description = "Replication lag alert policy name"
  value       = var.enable_disaster_recovery ? google_monitoring_alert_policy.replication_lag[0].name : null
}

output "logging_sink_dr_audit" {
  description = "DR audit logging sink name"
  value       = google_logging_project_sink.dr_audit.name
}

output "snapshot_schedule_policy" {
  description = "Snapshot schedule policy name"
  value       = google_compute_resource_policy.snapshot_schedule.name
}

output "failover_function" {
  description = "Failover handler function name"
  value       = var.failover_type == "automatic" ? google_cloudfunctions2_function.failover_handler[0].name : null
}

output "failover_test_job" {
  description = "Failover test scheduler job name"
  value       = var.enable_failover_testing ? google_cloud_scheduler_job.failover_test[0].name : null
}

output "dr_endpoints" {
  description = "All disaster recovery endpoints"
  value = {
    global_ip        = google_compute_global_address.default.address
    primary_region   = var.primary_region
    secondary_region = var.secondary_region
    primary_sql      = google_sql_database_instance.primary.connection_name
    secondary_sql    = var.enable_disaster_recovery ? google_sql_database_instance.secondary_replica[0].connection_name : null
    primary_gke      = google_container_cluster.primary.endpoint
    secondary_gke    = var.enable_disaster_recovery ? google_container_cluster.secondary[0].endpoint : null
    primary_redis    = google_redis_instance.primary.host
    secondary_redis  = var.enable_disaster_recovery ? google_redis_instance.secondary[0].host : null
  }
  sensitive = true
}

output "dr_summary" {
  description = "Disaster recovery deployment summary"
  value = {
    environment                = var.environment
    dr_enabled                 = var.enable_disaster_recovery
    failover_type              = var.failover_type
    rpo_minutes                = var.recovery_point_objective_minutes
    rto_minutes                = var.recovery_time_objective_minutes
    primary_region             = var.primary_region
    secondary_region           = var.secondary_region
    spanner_enabled            = var.enable_spanner
    failover_testing_enabled   = var.enable_failover_testing
    backup_retention_days      = var.backup_retention_days
    snapshot_retention_days    = var.snapshot_retention_days
  }
}
