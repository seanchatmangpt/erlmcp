# ============================================================================
# Complete Deployment Variables
# erlmcp v3 - Multi-platform GCP deployment
# ============================================================================

# ============================================================================
# Project Configuration
# ============================================================================
variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "region" {
  type        = string
  description = "Primary GCP region"
  default     = "us-central1"
}

variable "secondary_region" {
  type        = string
  description = "Secondary GCP region for multi-region deployment"
  default     = "us-east1"
}

variable "environment" {
  type        = string
  description = "Environment name (development, staging, production)"
  default     = "production"

  validation {
    condition     = contains(["development", "staging", "production"], var.environment)
    error_message = "Environment must be development, staging, or production."
  }
}

variable "image_tag" {
  type        = string
  description = "erlmcp container image tag"
  default     = "latest"
}

# ============================================================================
# Network Configuration
# ============================================================================
variable "network_name" {
  type        = string
  description = "VPC network name"
  default     = "erlmcp-vpc"
}

variable "primary_subnet_cidr" {
  type        = string
  description = "Primary subnet CIDR range"
  default     = "10.0.0.0/24"
}

variable "secondary_subnet_cidr" {
  type        = string
  description = "Secondary subnet CIDR range"
  default     = "10.10.0.0/24"
}

variable "gke_pods_cidr" {
  type        = string
  description = "GKE pods IP range"
  default     = "10.1.0.0/16"
}

variable "gke_services_cidr" {
  type        = string
  description = "GKE services IP range"
  default     = "10.2.0.0/16"
}

variable "ssh_source_ranges" {
  type        = list(string)
  description = "Allowed SSH source IP ranges (empty = use IAP tunnel)"
  default     = []
}

# ============================================================================
# Security Configuration
# ============================================================================
variable "allow_public_access" {
  type        = bool
  description = "Allow public access to services"
  default     = false
}

variable "enable_tls" {
  type        = bool
  description = "Enable TLS for services"
  default     = true
}

variable "enable_cloud_armor" {
  type        = bool
  description = "Enable Cloud Armor DDoS protection"
  default     = true
}

variable "rate_limit_threshold" {
  type        = number
  description = "Cloud Armor rate limit threshold (requests per minute)"
  default     = 1000
}

variable "enable_binary_authorization" {
  type        = bool
  description = "Enable Binary Authorization for container images"
  default     = true
}

# ============================================================================
# GKE Configuration
# ============================================================================
variable "cluster_name_prefix" {
  type        = string
  description = "GKE cluster name prefix"
  default     = "erlmcp-cluster"
}

variable "kubernetes_version" {
  type        = string
  description = "Kubernetes version"
  default     = "1.30"
}

variable "release_channel" {
  type        = string
  description = "GKE release channel"
  default     = "REGULAR"

  validation {
    condition     = contains(["RAPID", "REGULAR", "STABLE"], var.release_channel)
    error_message = "Release channel must be RAPID, REGULAR, or STABLE."
  }
}

variable "gke_private_endpoint" {
  type        = bool
  description = "Enable GKE private endpoint"
  default     = true
}

variable "gke_master_cidr" {
  type        = string
  description = "GKE master CIDR block"
  default     = "172.16.0.0/28"
}

variable "gke_master_global_access" {
  type        = bool
  description = "Enable global access to GKE master"
  default     = false
}

variable "gke_machine_type" {
  type        = string
  description = "GKE node machine type"
  default     = "n2-standard-4"
}

variable "gke_disk_size_gb" {
  type        = number
  description = "GKE node disk size in GB"
  default     = 100
}

variable "gke_min_nodes" {
  type        = number
  description = "Minimum number of GKE nodes"
  default     = 1
}

variable "gke_max_nodes" {
  type        = number
  description = "Maximum number of GKE nodes"
  default     = 10
}

variable "enable_spot_nodes" {
  type        = bool
  description = "Enable spot (preemptible) node pool"
  default     = true
}

variable "gke_spot_machine_type" {
  type        = string
  description = "Spot node machine type"
  default     = "n2-standard-2"
}

variable "gke_spot_min_nodes" {
  type        = number
  description = "Minimum spot nodes"
  default     = 0
}

variable "gke_spot_max_nodes" {
  type        = number
  description = "Maximum spot nodes"
  default     = 5
}

variable "deploy_erlmcp_to_gke" {
  type        = bool
  description = "Deploy erlmcp Helm chart to GKE"
  default     = true
}

variable "kubernetes_namespace" {
  type        = string
  description = "Kubernetes namespace for erlmcp deployment"
  default     = "erlmcp"
}

# ============================================================================
# Cloud Run Configuration
# ============================================================================
variable "cloud_run_service_prefix" {
  type        = string
  description = "Cloud Run service name prefix"
  default     = "erlmcp-service"
}

variable "cloud_run_cpu" {
  type        = string
  description = "Cloud Run CPU allocation"
  default     = "2"
}

variable "cloud_run_memory" {
  type        = string
  description = "Cloud Run memory allocation"
  default     = "2Gi"
}

variable "cloud_run_cpu_idle" {
  type        = bool
  description = "CPU allocation during idle (Gen2)"
  default     = true
}

variable "cloud_run_timeout" {
  type        = number
  description = "Cloud Run request timeout in seconds"
  default     = 300
}

variable "cloud_run_min_instances" {
  type        = number
  description = "Minimum Cloud Run instances"
  default     = 1
}

variable "cloud_run_max_instances" {
  type        = number
  description = "Maximum Cloud Run instances"
  default     = 100
}

variable "cloud_run_max_concurrency" {
  type        = number
  description = "Maximum concurrent requests per instance"
  default     = 80
}

variable "cloud_run_vpc_connector" {
  type        = string
  description = "VPC connector for Cloud Run"
  default     = ""
}

variable "cloud_run_vpc_egress" {
  type        = string
  description = "VPC egress setting (all-traffic or private-ranges-only)"
  default     = "private-ranges-only"
}

variable "cloud_run_network_interfaces" {
  type = list(object({
    network    = string
    subnetwork = string
    tags       = list(string)
  }))
  description = "Direct VPC network interfaces (Gen2)"
  default     = []
}

variable "cloud_run_session_affinity" {
  type        = bool
  description = "Enable session affinity"
  default     = true
}

variable "cloud_run_custom_domain" {
  type        = string
  description = "Custom domain for Cloud Run service"
  default     = ""
}

variable "cloud_run_invoker_accounts" {
  type        = list(string)
  description = "Service accounts allowed to invoke Cloud Run"
  default     = []
}

# ============================================================================
# Compute Engine Configuration
# ============================================================================
variable "gce_instance_prefix" {
  type        = string
  description = "GCE instance name prefix"
  default     = "erlmcp-vm"
}

variable "gce_zone" {
  type        = string
  description = "GCE instance zone"
  default     = "us-central1-a"
}

variable "gce_instance_count" {
  type        = number
  description = "Number of GCE instances"
  default     = 1
}

variable "gce_machine_type" {
  type        = string
  description = "GCE machine type"
  default     = "n2-standard-4"
}

variable "gce_source_image" {
  type        = string
  description = "GCE source image"
  default     = "projects/ubuntu-os-cloud/global/images/family/ubuntu-2204-lts"
}

variable "gce_disk_type" {
  type        = string
  description = "GCE disk type"
  default     = "pd-ssd"
}

variable "gce_disk_size_gb" {
  type        = number
  description = "GCE disk size in GB"
  default     = 100
}

variable "gce_http_source_ranges" {
  type        = list(string)
  description = "Allowed HTTP source ranges for GCE"
  default     = ["0.0.0.0/0"]
}

# ============================================================================
# Load Balancer Configuration
# ============================================================================
variable "create_global_load_balancer" {
  type        = bool
  description = "Create global load balancer for multi-platform routing"
  default     = true
}

variable "load_balancer_domain" {
  type        = string
  description = "Domain name for load balancer"
  default     = "erlmcp.example.com"
}

variable "ssl_certificate_ids" {
  type        = list(string)
  description = "SSL certificate resource IDs for HTTPS load balancer"
  default     = []
}

# ============================================================================
# Cloud SQL Configuration
# ============================================================================
variable "enable_cloud_sql" {
  type        = bool
  description = "Enable Cloud SQL PostgreSQL"
  default     = true
}

variable "cloud_sql_database_version" {
  type        = string
  description = "Cloud SQL database version"
  default     = "POSTGRES_15"
}

variable "cloud_sql_tier" {
  type        = string
  description = "Cloud SQL instance tier"
  default     = "db-custom-2-7680"
}

variable "cloud_sql_availability_type" {
  type        = string
  description = "Cloud SQL availability type (ZONAL or REGIONAL)"
  default     = "REGIONAL"

  validation {
    condition     = contains(["ZONAL", "REGIONAL"], var.cloud_sql_availability_type)
    error_message = "Availability type must be ZONAL or REGIONAL."
  }
}

variable "cloud_sql_disk_size_gb" {
  type        = number
  description = "Cloud SQL disk size in GB"
  default     = 100
}

# ============================================================================
# Observability Configuration
# ============================================================================
variable "notification_channels" {
  type = list(object({
    type         = string
    display_name = string
    labels       = map(string)
  }))
  description = "Notification channels for alerts"
  default     = []
}

variable "create_uptime_check" {
  type        = bool
  description = "Create uptime monitoring check"
  default     = true
}

variable "uptime_check_host" {
  type        = string
  description = "Host for uptime check (uses Cloud Run URL if empty)"
  default     = ""
}

variable "create_slos" {
  type        = bool
  description = "Create Service Level Objectives"
  default     = true
}

variable "slo_availability_goal" {
  type        = number
  description = "Availability SLO goal (0-1)"
  default     = 0.999
}

variable "slo_latency_threshold" {
  type        = number
  description = "Latency SLO threshold in milliseconds"
  default     = 500
}

variable "slo_latency_goal" {
  type        = number
  description = "Latency SLO goal (0-1)"
  default     = 0.95
}

variable "alert_error_rate_threshold" {
  type        = number
  description = "Error rate alert threshold (0-1)"
  default     = 0.05
}

variable "alert_latency_threshold" {
  type        = number
  description = "Latency alert threshold in milliseconds"
  default     = 1000
}

variable "alert_memory_threshold" {
  type        = number
  description = "Memory usage alert threshold (0-1)"
  default     = 0.9
}

variable "alert_cpu_threshold" {
  type        = number
  description = "CPU usage alert threshold (0-1)"
  default     = 0.8
}

variable "enable_cost_dashboard" {
  type        = bool
  description = "Create cost monitoring dashboard"
  default     = true
}

variable "enable_log_export" {
  type        = bool
  description = "Enable log export to Cloud Storage"
  default     = true
}

variable "log_export_bucket" {
  type        = string
  description = "Cloud Storage bucket for log export"
  default     = ""
}

variable "enable_log_bigquery_export" {
  type        = bool
  description = "Enable log export to BigQuery"
  default     = true
}

variable "log_bigquery_dataset" {
  type        = string
  description = "BigQuery dataset for log export"
  default     = "erlmcp_logs"
}

variable "trace_sampling_rate" {
  type        = number
  description = "Trace sampling rate (0-1)"
  default     = 0.1

  validation {
    condition     = var.trace_sampling_rate >= 0 && var.trace_sampling_rate <= 1
    error_message = "Trace sampling rate must be between 0 and 1."
  }
}

variable "log_level" {
  type        = string
  description = "Application log level"
  default     = "info"

  validation {
    condition     = contains(["debug", "info", "warning", "error"], var.log_level)
    error_message = "Log level must be debug, info, warning, or error."
  }
}

# ============================================================================
# Billing Configuration
# ============================================================================
variable "enable_budget_alerts" {
  type        = bool
  description = "Enable budget alerts"
  default     = true
}

variable "monthly_budget_amount" {
  type        = number
  description = "Monthly budget amount in USD"
  default     = 5000
}
