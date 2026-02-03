# ============================================================================
# Variables for erlmcp GCP Marketplace Deployment
# ============================================================================

# ============================================================================
# General Configuration
# ============================================================================

variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "region" {
  description = "GCP region"
  type        = string
  default     = "us-central1"
}

variable "environment" {
  description = "Environment name (dev, staging, production)"
  type        = string
  default     = "production"
  validation {
    condition     = contains(["dev", "staging", "production"], var.environment)
    error_message = "Environment must be dev, staging, or production."
  }
}

variable "erlmcp_version" {
  description = "erlmcp version"
  type        = string
  default     = "0.1.0"
}

variable "log_level" {
  description = "Logging level"
  type        = string
  default     = "info"
}

variable "deployment_types" {
  description = "Deployment types to enable (gce, cloud_run, gke)"
  type        = list(string)
  default     = ["cloud_run"]
}

# ============================================================================
# Network Configuration
# ============================================================================

variable "subnet_cidr" {
  description = "CIDR for main subnet"
  type        = string
  default     = "10.0.0.0/20"
}

variable "gke_pods_cidr" {
  description = "Secondary CIDR for GKE pods"
  type        = string
  default     = "10.4.0.0/14"
}

variable "gke_services_cidr" {
  description = "Secondary CIDR for GKE services"
  type        = string
  default     = "10.8.0.0/20"
}

variable "vpc_connector_cidr" {
  description = "CIDR for VPC connector (Cloud Run)"
  type        = string
  default     = "10.9.0.0/28"
}

variable "enable_iap" {
  description = "Enable Identity-Aware Proxy for SSH"
  type        = bool
  default     = true
}

variable "enable_os_login" {
  description = "Enable OS Login for VMs"
  type        = bool
  default     = true
}

# ============================================================================
# Secret Manager Configuration
# ============================================================================

variable "erlmcp_cookie" {
  description = "Erlang distribution cookie (generated if empty)"
  type        = string
  default     = ""
  sensitive   = true
}

variable "db_password" {
  description = "Database password (generated if empty)"
  type        = string
  default     = ""
  sensitive   = true
}

# ============================================================================
# Cloud Run Configuration
# ============================================================================

variable "cloud_run_image" {
  description = "Container image for Cloud Run (uses Artifact Registry if empty)"
  type        = string
  default     = ""
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
  description = "CPU allocation for Cloud Run"
  type        = string
  default     = "2"
}

variable "cloud_run_memory" {
  description = "Memory allocation for Cloud Run"
  type        = string
  default     = "2Gi"
}

variable "cloud_run_timeout" {
  description = "Request timeout in seconds"
  type        = number
  default     = 300
}

variable "cloud_run_cpu_idle" {
  description = "Keep CPU allocated when idle"
  type        = bool
  default     = true
}

variable "cloud_run_ingress" {
  description = "Ingress setting for Cloud Run"
  type        = string
  default     = "INGRESS_TRAFFIC_ALL"
}

variable "cloud_run_allow_unauthenticated" {
  description = "Allow unauthenticated access to Cloud Run"
  type        = bool
  default     = false
}

variable "cloud_run_vpc_connector" {
  description = "Existing VPC connector name (creates new if empty)"
  type        = string
  default     = ""
}

variable "cloud_run_custom_domain" {
  description = "Custom domain for Cloud Run"
  type        = string
  default     = ""
}

variable "cloud_run_secrets" {
  description = "Secrets to inject into Cloud Run"
  type = list(object({
    secret_id = string
    env_var   = string
    version   = string
  }))
  default = []
}

# ============================================================================
# Compute Engine Configuration
# ============================================================================

variable "gce_image_family" {
  description = "Image family for GCE instances"
  type        = string
  default     = "erlmcp"
}

variable "gce_image_project" {
  description = "Project containing GCE image (uses current project if empty)"
  type        = string
  default     = ""
}

variable "gce_machine_type" {
  description = "Machine type for GCE instances"
  type        = string
  default     = "e2-standard-4"
}

variable "gce_disk_size" {
  description = "Boot disk size in GB"
  type        = number
  default     = 50
}

variable "gce_preemptible" {
  description = "Use preemptible instances"
  type        = bool
  default     = false
}

variable "gce_assign_public_ip" {
  description = "Assign public IPs to GCE instances"
  type        = bool
  default     = false
}

variable "gce_min_instances" {
  description = "Minimum number of GCE instances"
  type        = number
  default     = 2
}

variable "gce_max_instances" {
  description = "Maximum number of GCE instances"
  type        = number
  default     = 10
}

variable "gce_max_surge" {
  description = "Max surge during rolling update"
  type        = number
  default     = 3
}

variable "gce_cpu_target" {
  description = "Target CPU utilization for autoscaling"
  type        = number
  default     = 0.7
}

variable "gce_custom_metrics" {
  description = "Enable custom metrics autoscaling"
  type        = bool
  default     = false
}

variable "gce_requests_target" {
  description = "Target requests per second for autoscaling"
  type        = number
  default     = 1000
}

variable "gce_domain" {
  description = "Domain for GCE load balancer"
  type        = string
  default     = ""
}

variable "gce_dns_zone" {
  description = "Cloud DNS managed zone name"
  type        = string
  default     = ""
}

variable "gce_secrets" {
  description = "Secrets to inject into GCE instances"
  type = list(object({
    secret_id = string
    env_var   = string
  }))
  default = []
}

# ============================================================================
# GKE Configuration
# ============================================================================

variable "gke_autopilot" {
  description = "Use GKE Autopilot mode"
  type        = bool
  default     = true
}

variable "gke_regional" {
  description = "Use regional GKE cluster"
  type        = bool
  default     = true
}

variable "gke_release_channel" {
  description = "GKE release channel"
  type        = string
  default     = "REGULAR"
}

variable "gke_private_endpoint" {
  description = "Use private endpoint for GKE master"
  type        = bool
  default     = false
}

variable "gke_master_cidr" {
  description = "CIDR for GKE master"
  type        = string
  default     = "172.16.0.0/28"
}

variable "gke_authorized_networks" {
  description = "Authorized networks for GKE master"
  type = list(object({
    cidr = string
    name = string
  }))
  default = []
}

variable "gke_machine_type" {
  description = "Machine type for GKE nodes (standard mode only)"
  type        = string
  default     = "e2-standard-4"
}

variable "gke_disk_size" {
  description = "Disk size for GKE nodes in GB"
  type        = number
  default     = 100
}

variable "gke_min_nodes" {
  description = "Minimum nodes per zone (standard mode only)"
  type        = number
  default     = 1
}

variable "gke_max_nodes" {
  description = "Maximum nodes per zone (standard mode only)"
  type        = number
  default     = 5
}

variable "gke_binary_auth" {
  description = "Enable Binary Authorization"
  type        = bool
  default     = false
}

variable "gke_deletion_protection" {
  description = "Enable deletion protection"
  type        = bool
  default     = true
}

variable "gke_image" {
  description = "Container image for GKE (uses Artifact Registry if empty)"
  type        = string
  default     = ""
}

variable "gke_replicas" {
  description = "Initial number of replicas"
  type        = number
  default     = 3
}

variable "gke_min_replicas" {
  description = "Minimum replicas for HPA"
  type        = number
  default     = 2
}

variable "gke_max_replicas" {
  description = "Maximum replicas for HPA"
  type        = number
  default     = 20
}

variable "gke_cpu_request" {
  description = "CPU request per pod"
  type        = string
  default     = "500m"
}

variable "gke_cpu_limit" {
  description = "CPU limit per pod"
  type        = string
  default     = "2000m"
}

variable "gke_memory_request" {
  description = "Memory request per pod"
  type        = string
  default     = "512Mi"
}

variable "gke_memory_limit" {
  description = "Memory limit per pod"
  type        = string
  default     = "2Gi"
}

variable "gke_cpu_target" {
  description = "Target CPU utilization for HPA"
  type        = number
  default     = 70
}

variable "gke_memory_target" {
  description = "Target memory utilization for HPA"
  type        = number
  default     = 80
}

variable "gke_erlang_schedulers" {
  description = "Number of Erlang schedulers"
  type        = number
  default     = 4
}

variable "gke_async_threads" {
  description = "Number of async threads"
  type        = number
  default     = 64
}

variable "gke_enable_gateway" {
  description = "Enable Gateway API ingress"
  type        = bool
  default     = false
}

variable "gke_certificate_map" {
  description = "Certificate Map name for Gateway"
  type        = string
  default     = ""
}

# ============================================================================
# Data Services Configuration
# ============================================================================

variable "enable_cloud_sql" {
  description = "Enable Cloud SQL PostgreSQL"
  type        = bool
  default     = false
}

variable "cloud_sql_version" {
  description = "Cloud SQL PostgreSQL version"
  type        = string
  default     = "POSTGRES_16"
}

variable "cloud_sql_tier" {
  description = "Cloud SQL machine tier"
  type        = string
  default     = "db-custom-2-4096"
}

variable "cloud_sql_disk_size" {
  description = "Cloud SQL disk size in GB"
  type        = number
  default     = 20
}

variable "cloud_sql_ha" {
  description = "Enable high availability"
  type        = bool
  default     = false
}

variable "cloud_sql_deletion_protection" {
  description = "Enable deletion protection"
  type        = bool
  default     = true
}

variable "cloud_sql_authorized_networks" {
  description = "Authorized networks for Cloud SQL"
  type = list(object({
    name = string
    cidr = string
  }))
  default = []
}

variable "enable_memorystore" {
  description = "Enable Memorystore Redis"
  type        = bool
  default     = false
}

variable "memorystore_tier" {
  description = "Memorystore tier"
  type        = string
  default     = "BASIC"
}

variable "memorystore_size_gb" {
  description = "Memorystore size in GB"
  type        = number
  default     = 1
}

variable "memorystore_version" {
  description = "Memorystore Redis version"
  type        = string
  default     = "REDIS_7_0"
}
