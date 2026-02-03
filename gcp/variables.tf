# ==============================================================================
# ERLMCP GCP Infrastructure Variables
# ==============================================================================
# All configurable parameters for the GCP infrastructure.
# Override in terraform.{env}.tfvars files for different environments.
# ==============================================================================

# ==============================================================================
# Project Configuration
# ==============================================================================

variable "project_id" {
  description = "GCP Project ID"
  type        = string
}

variable "region" {
  description = "GCP Region"
  type        = string
  default     = "us-central1"
}

variable "environment" {
  description = "Environment name (dev, staging, production)"
  type        = string
  default     = "production"

  validation {
    condition     = contains(["dev", "staging", "production"], var.environment)
    error_message = "Environment must be one of: dev, staging, production."
  }
}

variable "billing_account" {
  description = "GCP Billing Account ID"
  type        = string
  sensitive   = true
  default     = ""
}

variable "app_name" {
  description = "Application name (used in resource naming)"
  type        = string
  default     = "erlmcp"
}

variable "app_version" {
  description = "Application version to deploy"
  type        = string
  default     = "latest"
}

# ==============================================================================
# Service Account Configuration
# ==============================================================================

variable "service_account_name" {
  description = "Service account name for deployments"
  type        = string
  default     = "erlmcp-deploy"
}

# ==============================================================================
# GCP APIs to Enable
# ==============================================================================

variable "allowed_services" {
  description = "GCP services to enable"
  type        = list(string)
  default = [
    "compute.googleapis.com",
    "cloudrun.googleapis.com",
    "artifactregistry.googleapis.com",
    "secretmanager.googleapis.com",
    "cloudkms.googleapis.com",
    "monitoring.googleapis.com",
    "logging.googleapis.com",
    "cloudtrace.googleapis.com",
    "cloudbuild.googleapis.com",
    "sqladmin.googleapis.com",
    "servicenetworking.googleapis.com",
    "vpcaccess.googleapis.com",
    "dns.googleapis.com",
  ]
}

# ==============================================================================
# KMS Configuration
# ==============================================================================

variable "kms_key_rotation_period" {
  description = "KMS key rotation period"
  type        = string
  default     = "7776000s" # 90 days
}

# ==============================================================================
# Cloud Run Configuration
# ==============================================================================

variable "cloud_run_ingress" {
  description = "Cloud Run ingress setting"
  type        = string
  default     = "INGRESS_TRAFFIC_ALL"

  validation {
    condition     = contains(["INGRESS_TRAFFIC_ALL", "INGRESS_TRAFFIC_INTERNAL_ONLY", "INGRESS_TRAFFIC_INTERNAL_LOAD_BALANCER"], var.cloud_run_ingress)
    error_message = "Invalid Cloud Run ingress setting."
  }
}

variable "cloud_run_min_instances" {
  description = "Minimum number of Cloud Run instances"
  type        = number
  default     = 0
}

variable "cloud_run_max_instances" {
  description = "Maximum number of Cloud Run instances"
  type        = number
  default     = 100
}

variable "cloud_run_cpu" {
  description = "Cloud Run CPU allocation"
  type        = string
  default     = "2"
}

variable "cloud_run_memory" {
  description = "Cloud Run memory allocation"
  type        = string
  default     = "2Gi"
}

variable "cloud_run_timeout" {
  description = "Cloud Run request timeout in seconds"
  type        = number
  default     = 300
}

variable "cloud_run_allow_unauthenticated" {
  description = "Allow unauthenticated access to Cloud Run"
  type        = bool
  default     = true
}

variable "cloud_run_invoker_service_accounts" {
  description = "Service accounts allowed to invoke Cloud Run"
  type        = list(string)
  default     = []
}

variable "cloud_run_custom_domain" {
  description = "Custom domain for Cloud Run (leave empty to skip)"
  type        = string
  default     = ""
}

# ==============================================================================
# Secrets Configuration
# ==============================================================================

variable "erlang_cookie" {
  description = "Erlang distribution cookie (leave empty to auto-generate)"
  type        = string
  sensitive   = true
  default     = ""
}

variable "database_url" {
  description = "Database connection URL (leave empty to use Cloud SQL)"
  type        = string
  sensitive   = true
  default     = ""
}

# ==============================================================================
# VPC Configuration
# ==============================================================================

variable "create_vpc" {
  description = "Create a new VPC (false to use existing)"
  type        = bool
  default     = true
}

variable "existing_vpc_name" {
  description = "Name of existing VPC to use (if create_vpc is false)"
  type        = string
  default     = "default"
}

variable "vpc_primary_cidr" {
  description = "Primary subnet CIDR"
  type        = string
  default     = "10.0.0.0/20"
}

variable "vpc_serverless_cidr" {
  description = "Serverless subnet CIDR"
  type        = string
  default     = "10.0.16.0/24"
}

variable "vpc_connector_cidr" {
  description = "VPC connector CIDR"
  type        = string
  default     = "10.8.0.0/28"
}

variable "vpc_connector_enabled" {
  description = "Enable VPC connector for Cloud Run"
  type        = bool
  default     = true
}

variable "vpc_connector_max_instances" {
  description = "Max instances for VPC connector"
  type        = number
  default     = 10
}

variable "vpc_connector_machine_type" {
  description = "Machine type for VPC connector"
  type        = string
  default     = "e2-micro"
}

variable "enable_nat" {
  description = "Enable Cloud NAT for outbound internet access"
  type        = bool
  default     = true
}

variable "enable_strict_firewall" {
  description = "Enable strict firewall rules (deny-all default)"
  type        = bool
  default     = true
}

variable "enable_private_services" {
  description = "Enable private service connection (for Cloud SQL)"
  type        = bool
  default     = true
}

variable "create_private_dns" {
  description = "Create private DNS zone"
  type        = bool
  default     = true
}

# ==============================================================================
# Cloud SQL Configuration
# ==============================================================================

variable "enable_cloud_sql" {
  description = "Enable Cloud SQL PostgreSQL instance"
  type        = bool
  default     = true
}

variable "cloud_sql_version" {
  description = "Cloud SQL PostgreSQL version"
  type        = string
  default     = "POSTGRES_16"
}

variable "cloud_sql_tier" {
  description = "Cloud SQL machine type"
  type        = string
  default     = "db-custom-2-8192" # 2 vCPU, 8GB RAM
}

variable "cloud_sql_ha" {
  description = "Enable high availability for Cloud SQL"
  type        = bool
  default     = true
}

variable "cloud_sql_disk_size" {
  description = "Cloud SQL disk size in GB"
  type        = number
  default     = 20
}

variable "cloud_sql_pitr" {
  description = "Enable point-in-time recovery"
  type        = bool
  default     = true
}

variable "cloud_sql_backup_retention" {
  description = "Number of backups to retain"
  type        = number
  default     = 7
}

variable "cloud_sql_max_connections" {
  description = "Maximum database connections"
  type        = string
  default     = "200"
}

variable "cloud_sql_authorized_networks" {
  description = "Authorized networks for Cloud SQL"
  type = list(object({
    name = string
    cidr = string
  }))
  default = []
}

variable "cloud_sql_database_name" {
  description = "Database name"
  type        = string
  default     = "erlmcp"
}

variable "cloud_sql_user" {
  description = "Database user"
  type        = string
  default     = "erlmcp"
}

variable "cloud_sql_password" {
  description = "Database password (leave empty to auto-generate)"
  type        = string
  sensitive   = true
  default     = ""
}

variable "cloud_sql_read_replicas" {
  description = "Number of read replicas"
  type        = number
  default     = 0
}

variable "cloud_sql_replica_tier" {
  description = "Cloud SQL replica machine type (empty = same as primary)"
  type        = string
  default     = ""
}

variable "create_cloudsql_proxy_key" {
  description = "Create service account key for Cloud SQL Proxy"
  type        = bool
  default     = false
}

# ==============================================================================
# Monitoring Configuration
# ==============================================================================

variable "alert_email_addresses" {
  description = "Email addresses for alert notifications"
  type        = list(string)
  default     = []
}

variable "slack_webhook_url" {
  description = "Slack webhook URL for alerts"
  type        = string
  sensitive   = true
  default     = ""
}

variable "slack_channel" {
  description = "Slack channel for alerts"
  type        = string
  default     = "#erlmcp-alerts"
}

variable "pagerduty_service_key" {
  description = "PagerDuty service integration key"
  type        = string
  sensitive   = true
  default     = ""
}
