# Disaster Recovery Module Variables

variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "environment" {
  description = "Environment name (prod, staging, dev)"
  type        = string
}

variable "primary_region" {
  description = "Primary GCP region"
  type        = string
  default     = "us-central1"
}

variable "secondary_region" {
  description = "Secondary GCP region for DR"
  type        = string
  default     = "us-east1"
}

variable "multi_region_location" {
  description = "Multi-region location for critical data (US, EU, ASIA)"
  type        = string
  default     = "US"
}

variable "enable_disaster_recovery" {
  description = "Enable disaster recovery features"
  type        = bool
  default     = true
}

variable "failover_type" {
  description = "Failover type: manual or automatic"
  type        = string
  default     = "automatic"
  validation {
    condition     = contains(["manual", "automatic"], var.failover_type)
    error_message = "Failover type must be either 'manual' or 'automatic'."
  }
}

variable "recovery_point_objective_minutes" {
  description = "Maximum acceptable data loss in minutes (RPO)"
  type        = number
  default     = 5
}

variable "recovery_time_objective_minutes" {
  description = "Maximum acceptable downtime in minutes (RTO)"
  type        = number
  default     = 15
}

variable "backup_retention_days" {
  description = "Number of days to retain backups"
  type        = number
  default     = 30
}

variable "snapshot_retention_days" {
  description = "Number of days to retain disk snapshots"
  type        = number
  default     = 14
}

variable "database_version" {
  description = "Cloud SQL database version"
  type        = string
  default     = "POSTGRES_15"
}

variable "database_tier" {
  description = "Cloud SQL tier"
  type        = string
  default     = "db-custom-4-16384"
}

variable "database_disk_size_gb" {
  description = "Database disk size in GB"
  type        = number
  default     = 100
}

variable "enable_spanner" {
  description = "Enable Cloud Spanner for multi-region database"
  type        = bool
  default     = false
}

variable "spanner_config" {
  description = "Cloud Spanner configuration"
  type        = string
  default     = "nam-eur-asia1"
}

variable "spanner_nodes" {
  description = "Number of Cloud Spanner nodes"
  type        = number
  default     = 1
}

variable "vpc_network_id" {
  description = "VPC network ID"
  type        = string
}

variable "primary_subnet_id" {
  description = "Primary subnet ID"
  type        = string
}

variable "secondary_subnet_id" {
  description = "Secondary subnet ID"
  type        = string
}

variable "node_machine_type" {
  description = "GKE node machine type"
  type        = string
  default     = "n2-standard-4"
}

variable "node_count_per_zone" {
  description = "Number of nodes per zone in primary cluster"
  type        = number
  default     = 1
}

variable "min_node_count" {
  description = "Minimum node count for primary cluster autoscaling"
  type        = number
  default     = 1
}

variable "max_node_count" {
  description = "Maximum node count for primary cluster autoscaling"
  type        = number
  default     = 10
}

variable "secondary_node_count_per_zone" {
  description = "Number of nodes per zone in secondary cluster"
  type        = number
  default     = 1
}

variable "secondary_min_node_count" {
  description = "Minimum node count for secondary cluster autoscaling"
  type        = number
  default     = 0
}

variable "secondary_max_node_count" {
  description = "Maximum node count for secondary cluster autoscaling"
  type        = number
  default     = 5
}

variable "redis_memory_gb" {
  description = "Redis memory size in GB"
  type        = number
  default     = 5
}

variable "redis_primary_ip_range" {
  description = "Reserved IP range for primary Redis instance"
  type        = string
  default     = "10.10.0.0/29"
}

variable "redis_secondary_ip_range" {
  description = "Reserved IP range for secondary Redis instance"
  type        = string
  default     = "10.11.0.0/29"
}

variable "notification_channels" {
  description = "List of notification channel IDs for alerts"
  type        = list(string)
  default     = []
}

variable "enable_failover_testing" {
  description = "Enable automated failover testing"
  type        = bool
  default     = false
}

variable "failover_test_schedule" {
  description = "Cron schedule for failover tests"
  type        = string
  default     = "0 2 * * 0"
}

variable "failover_test_webhook_url" {
  description = "Webhook URL for failover test notifications"
  type        = string
  default     = ""
}

variable "failover_function_source_object" {
  description = "GCS object path for failover function source"
  type        = string
  default     = "functions/failover-handler.zip"
}

variable "deletion_protection" {
  description = "Enable deletion protection for critical resources"
  type        = bool
  default     = true
}

variable "labels" {
  description = "Additional labels to apply to resources"
  type        = map(string)
  default     = {}
}
