# ============================================================================
# Cloud Run v2 Module Variables - Gen2 Execution Environment
# Enterprise-grade serverless deployment for erlmcp
# ============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.40.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = ">= 5.40.0"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.6"
    }
  }
}

# ============================================================================
# Core Configuration
# ============================================================================

variable "project_id" {
  type        = string
  description = "GCP project ID"

  validation {
    condition     = length(var.project_id) > 0
    error_message = "Project ID must not be empty"
  }
}

variable "region" {
  type        = string
  description = "GCP region for Cloud Run service"
  default     = "us-central1"

  validation {
    condition     = can(regex("^[a-z]+-[a-z]+[0-9]$", var.region))
    error_message = "Region must be a valid GCP region format"
  }
}

variable "service_name" {
  type        = string
  description = "Cloud Run service name (will be suffixed with random string)"
  default     = "erlmcp"

  validation {
    condition     = can(regex("^[a-z][a-z0-9-]*[a-z0-9]$", var.service_name))
    error_message = "Service name must start with letter, contain only lowercase letters, numbers, and hyphens"
  }
}

# ============================================================================
# Container Configuration
# ============================================================================

variable "image_repository" {
  type        = string
  description = "Container image repository (Artifact Registry or GCR)"
  default     = "us-central1-docker.pkg.dev"
}

variable "image_name" {
  type        = string
  description = "Container image name"
  default     = "erlmcp/erlmcp"
}

variable "image_tag" {
  type        = string
  description = "Container image tag"
  default     = "3.0.0"
}

variable "container_port" {
  type        = number
  description = "Container port (HTTP/1 or HTTP/2)"
  default     = 8080

  validation {
    condition     = var.container_port > 0 && var.container_port <= 65535
    error_message = "Container port must be between 1 and 65535"
  }
}

# ============================================================================
# Resource Allocation (Gen2 Execution Environment)
# ============================================================================

variable "cpu" {
  type        = string
  description = "CPU allocation (0.08, 1, 2, 4, 6, 8)"
  default     = "1"

  validation {
    condition     = contains(["0.08", "1", "2", "4", "6", "8"], var.cpu)
    error_message = "CPU must be one of: 0.08, 1, 2, 4, 6, 8"
  }
}

variable "memory" {
  type        = string
  description = "Memory allocation (e.g., '512Mi', '1Gi', '2Gi', '4Gi', '8Gi', '16Gi', '32Gi')"
  default     = "512Mi"

  validation {
    condition     = can(regex("^[0-9]+(Mi|Gi)$", var.memory))
    error_message = "Memory must be in format: number + Mi or Gi (e.g., '512Mi', '2Gi')"
  }
}

variable "cpu_idle" {
  type        = bool
  description = "Allocate CPU only during request processing (Gen2 feature)"
  default     = true
}

variable "startup_cpu_boost" {
  type        = bool
  description = "Enable startup CPU boost for faster cold starts (Gen2 feature)"
  default     = true
}

variable "timeout" {
  type        = number
  description = "Request timeout in seconds (max 3600 for Gen2)"
  default     = 300

  validation {
    condition     = var.timeout >= 1 && var.timeout <= 3600
    error_message = "Timeout must be between 1 and 3600 seconds"
  }
}

variable "max_instance_request_concurrency" {
  type        = number
  description = "Maximum concurrent requests per instance"
  default     = 80

  validation {
    condition     = var.max_instance_request_concurrency >= 1 && var.max_instance_request_concurrency <= 1000
    error_message = "Concurrency must be between 1 and 1000"
  }
}

# ============================================================================
# Scaling Configuration
# ============================================================================

variable "min_instances" {
  type        = number
  description = "Minimum number of instances (0 = scale to zero)"
  default     = 0

  validation {
    condition     = var.min_instances >= 0
    error_message = "Minimum instances must be >= 0"
  }
}

variable "max_instances" {
  type        = number
  description = "Maximum number of instances"
  default     = 100

  validation {
    condition     = var.max_instances >= 1 && var.max_instances <= 1000
    error_message = "Maximum instances must be between 1 and 1000"
  }
}

variable "max_instance_count_per_region" {
  type        = number
  description = "Maximum instances per region (for multi-region deployments)"
  default     = null
}

# ============================================================================
# Networking Configuration (Gen2 VPC Features)
# ============================================================================

variable "ingress_setting" {
  type        = string
  description = "Ingress traffic control"
  default     = "INGRESS_TRAFFIC_ALL"

  validation {
    condition = contains([
      "INGRESS_TRAFFIC_ALL",
      "INGRESS_TRAFFIC_INTERNAL_ONLY",
      "INGRESS_TRAFFIC_INTERNAL_LOAD_BALANCER"
    ], var.ingress_setting)
    error_message = "Invalid ingress setting"
  }
}

variable "vpc_connector_name" {
  type        = string
  description = "VPC Serverless Connector name for private networking (Gen2)"
  default     = ""
}

variable "vpc_egress" {
  type        = string
  description = "VPC egress setting: ALL_TRAFFIC or PRIVATE_RANGES_ONLY"
  default     = "PRIVATE_RANGES_ONLY"

  validation {
    condition     = contains(["ALL_TRAFFIC", "PRIVATE_RANGES_ONLY"], var.vpc_egress)
    error_message = "VPC egress must be ALL_TRAFFIC or PRIVATE_RANGES_ONLY"
  }
}

variable "network_interfaces" {
  type = list(object({
    network    = string
    subnetwork = string
    tags       = list(string)
  }))
  description = "Direct VPC egress network interfaces (Gen2 feature)"
  default     = []
}

variable "cloud_sql_instances" {
  type        = list(string)
  description = "Cloud SQL instance connection names (project:region:instance)"
  default     = []
}

# ============================================================================
# Session Affinity (Gen2 Feature)
# ============================================================================

variable "session_affinity" {
  type        = bool
  description = "Enable session affinity for sticky sessions"
  default     = false
}

# ============================================================================
# Health Checks
# ============================================================================

variable "health_check_path" {
  type        = string
  description = "Health check HTTP path"
  default     = "/health"
}

variable "startup_probe_initial_delay" {
  type        = number
  description = "Startup probe initial delay in seconds"
  default     = 0
}

variable "startup_probe_timeout" {
  type        = number
  description = "Startup probe timeout in seconds"
  default     = 3
}

variable "startup_probe_period" {
  type        = number
  description = "Startup probe period in seconds"
  default     = 10
}

variable "startup_probe_failure_threshold" {
  type        = number
  description = "Startup probe failure threshold"
  default     = 3
}

variable "liveness_probe_initial_delay" {
  type        = number
  description = "Liveness probe initial delay in seconds"
  default     = 30
}

variable "liveness_probe_timeout" {
  type        = number
  description = "Liveness probe timeout in seconds"
  default     = 5
}

variable "liveness_probe_period" {
  type        = number
  description = "Liveness probe period in seconds"
  default     = 10
}

variable "liveness_probe_failure_threshold" {
  type        = number
  description = "Liveness probe failure threshold"
  default     = 3
}

# ============================================================================
# Environment Variables
# ============================================================================

variable "container_env" {
  type = list(object({
    name  = string
    value = string
  }))
  description = "Environment variables"
  default     = []
}

variable "container_secrets" {
  type = list(object({
    name           = string
    secret_name    = string
    secret_version = string
  }))
  description = "Secret Manager environment variables"
  default     = []
}

# ============================================================================
# Volume Mounts (Secret Volumes)
# ============================================================================

variable "secret_volumes" {
  type = list(object({
    name        = string
    mount_path  = string
    secret_name = string
    items = list(object({
      key  = string
      path = string
      mode = string
    }))
  }))
  description = "Secret Manager volume mounts"
  default     = []
}

# ============================================================================
# IAM Configuration
# ============================================================================

variable "create_service_account" {
  type        = bool
  description = "Create a new service account"
  default     = true
}

variable "service_account_email" {
  type        = string
  description = "Existing service account email (if not creating new one)"
  default     = ""
}

variable "service_account_roles" {
  type        = list(string)
  description = "IAM roles to grant to the service account"
  default = [
    "roles/secretmanager.secretAccessor",
    "roles/logging.logWriter",
    "roles/monitoring.metricWriter",
    "roles/cloudtrace.agent",
    "roles/cloudprofiler.agent"
  ]
}

variable "allow_public_access" {
  type        = bool
  description = "Allow unauthenticated public access (WARNING: security risk)"
  default     = false
}

variable "invoker_service_accounts" {
  type        = list(string)
  description = "Service accounts allowed to invoke the service"
  default     = []
}

# ============================================================================
# Security Configuration
# ============================================================================

variable "binary_authorization_policy" {
  type        = string
  description = "Binary Authorization policy name (empty = disabled)"
  default     = ""
}

variable "encryption_key" {
  type        = string
  description = "Cloud KMS encryption key for CMEK (empty = Google-managed)"
  default     = ""
}

variable "execution_environment" {
  type        = string
  description = "Execution environment: EXECUTION_ENVIRONMENT_GEN2 (recommended)"
  default     = "EXECUTION_ENVIRONMENT_GEN2"

  validation {
    condition     = contains(["EXECUTION_ENVIRONMENT_GEN1", "EXECUTION_ENVIRONMENT_GEN2"], var.execution_environment)
    error_message = "Execution environment must be GEN1 or GEN2"
  }
}

# ============================================================================
# Observability Configuration
# ============================================================================

variable "enable_cloud_logging" {
  type        = bool
  description = "Enable Cloud Logging"
  default     = true
}

variable "enable_cloud_monitoring" {
  type        = bool
  description = "Enable Cloud Monitoring"
  default     = true
}

variable "enable_cloud_trace" {
  type        = bool
  description = "Enable Cloud Trace"
  default     = true
}

# ============================================================================
# Artifact Registry
# ============================================================================

variable "create_repository" {
  type        = bool
  description = "Create Artifact Registry repository"
  default     = false
}

variable "repository_id" {
  type        = string
  description = "Artifact Registry repository ID"
  default     = "erlmcp"
}

# ============================================================================
# Labels and Metadata
# ============================================================================

variable "labels" {
  type        = map(string)
  description = "Resource labels"
  default = {
    app        = "erlmcp"
    managed-by = "terraform"
    deployment = "cloud-run-gen2"
  }
}

variable "annotations" {
  type        = map(string)
  description = "Additional annotations"
  default     = {}
}

# ============================================================================
# Domain Mapping
# ============================================================================

variable "custom_domain" {
  type        = string
  description = "Custom domain name (empty = use Cloud Run URL)"
  default     = ""
}

variable "create_domain_mapping" {
  type        = bool
  description = "Create Cloud Run domain mapping"
  default     = false
}
