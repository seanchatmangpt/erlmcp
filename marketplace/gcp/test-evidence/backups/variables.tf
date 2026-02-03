# ============================================================================
# Cloud Run Module Variables
# ============================================================================

variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "region" {
  type        = string
  description = "GCP region for Cloud Run service"
  default     = "us-central1"

  validation {
    condition     = can(regex("^[a-z0-9-]+$", var.region))
    error_message = "Region must contain only lowercase letters, numbers, and hyphens."
  }
}

variable "service_name" {
  type        = string
  description = "Cloud Run service name"
  default     = "erlmcp"
}

variable "image_repository" {
  type        = string
  description = "Container image repository (Artifact Registry)"
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

# ============================================================================
# Container Configuration
# ============================================================================
variable "cpu" {
  type        = string
  description = "CPU allocation (e.g., '1', '2', '500m')"
  default     = "1"
}

variable "memory" {
  type        = string
  description = "Memory allocation (e.g., '512Mi', '1Gi', '2Gi')"
  default     = "512Mi"
}

variable "cpu_idle" {
  type        = bool
  description = "Enable CPU throttling when idle"
  default     = true
}

variable "cpu_throttling" {
  type        = bool
  description = "Enable CPU throttling"
  default     = true
}

variable "timeout_seconds" {
  type        = number
  description = "Request timeout in seconds"
  default     = 300

  validation {
    condition     = var.timeout_seconds >= 1 && var.timeout_seconds <= 3600
    error_message = "Timeout must be between 1 and 3600 seconds."
  }
}

variable "execution_environment" {
  type        = string
  description = "Execution environment (EXECUTION_ENVIRONMENT_GEN1 or EXECUTION_ENVIRONMENT_GEN2)"
  default     = "EXECUTION_ENVIRONMENT_GEN2"

  validation {
    condition     = contains(["EXECUTION_ENVIRONMENT_GEN1", "EXECUTION_ENVIRONMENT_GEN2"], var.execution_environment)
    error_message = "Execution environment must be GEN1 or GEN2."
  }
}

variable "container_ports" {
  type = list(object({
    name           = string
    container_port = number
    protocol       = string
  }))
  description = "Container ports to expose"
  default = [
    {
      name           = "http1"
      container_port = 8080
      protocol       = "HTTP"
    },
    {
      name           = "metrics"
      container_port = 9100
      protocol       = "TCP"
    }
  ]
}

# ============================================================================
# Environment Variables
# ============================================================================
variable "container_env" {
  type = list(object({
    name  = string
    value = string
  }))
  description = "Environment variables for the container"
  default = [
    { name = "ERLMCP_ENV", value = "production" },
    { name = "ERLMCP_VERSION", value = "3.0.0" },
    { name = "ERL_AFLAGS", value = "-proto_dist inet_tls" },
    { name = "ERL_DIST_PORT", value = "9100" },
  ]
}

variable "container_secrets" {
  type = list(object({
    name          = string
    secret_name   = string
    secret_version = string
  }))
  description = "Secret references to inject as environment variables"
  default = [
    { name = "ERLANG_COOKIE", secret_name = "erlmcp-erlang-cookie", secret_version = "latest" },
    { name = "DB_PASSWORD", secret_name = "erlmcp-db-password", secret_version = "latest" },
    { name = "REDIS_PASSWORD", secret_name = "erlmcp-redis-password", secret_version = "latest" },
  ]
}

# ============================================================================
# Scaling Configuration
# ============================================================================
variable "min_instances" {
  type        = number
  description = "Minimum number of instances"
  default     = 0

  validation {
    condition     = var.min_instances >= 0
    error_message = "Minimum instances must be >= 0."
  }
}

variable "max_instances" {
  type        = number
  description = "Maximum number of instances (0 = unlimited)"
  default     = 10

  validation {
    condition     = var.max_instances == 0 || var.max_instances >= var.min_instances
    error_message = "Max instances must be >= min instances or 0 for unlimited."
  }
}

# ============================================================================
# Health Check Configuration
# ============================================================================
variable "health_check_path" {
  type        = string
  description = "Health check endpoint path"
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
  default     = 30
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
# Networking Configuration
# ============================================================================
variable "ingress_setting" {
  type        = string
  description = "Ingress setting (INGRESS_TRAFFIC_ALL, INGRESS_TRAFFIC_INTERNAL_ONLY, INGRESS_TRAFFIC_INTERNAL_LOAD_BALANCER)"
  default     = "INGRESS_TRAFFIC_ALL"

  validation {
    condition     = contains(["INGRESS_TRAFFIC_ALL", "INGRESS_TRAFFIC_INTERNAL_ONLY", "INGRESS_TRAFFIC_INTERNAL_LOAD_BALANCER"], var.ingress_setting)
    error_message = "Ingress setting must be INGRESS_TRAFFIC_ALL, INGRESS_TRAFFIC_INTERNAL_ONLY, or INGRESS_TRAFFIC_INTERNAL_LOAD_BALANCER."
  }
}

variable "allow_public_access" {
  type        = bool
  description = "Allow unauthenticated access to the service"
  default     = false
}

variable "invoker_service_account" {
  type        = string
  description = "Service account to invoke the service (when public_access = false)"
  default     = ""
}

variable "session_affinity" {
  type        = bool
  description = "Enable session affinity"
  default     = false
}

variable "custom_domain" {
  type        = string
  description = "Custom domain mapping (leave empty to disable)"
  default     = ""
}

# ============================================================================
# Secret Volumes
# ============================================================================
variable "secret_volumes" {
  type = list(object({
    name        = string
    mount_path  = string
    secret_name = string
    items       = list(object({
      key  = string
      path = string
      mode = string
    }))
  }))
  description = "Secrets to mount as volumes"
  default = [
    {
      name        = "erlmcp-tls"
      mount_path  = "/etc/tls"
      secret_name = "erlmcp-tls-cert"
      items       = [
        { key = "tls.crt", path = "cert.pem", mode = "0400" },
        { key = "tls.key", path = "key.pem", mode = "0400" }
      ]
    }
  ]
}

# ============================================================================
# Service Account
# ============================================================================
variable "create_service_account" {
  type        = bool
  description = "Create a dedicated service account for the service"
  default     = true
}

variable "service_account_email" {
  type        = string
  description = "Service account email (if not creating one)"
  default     = ""
}

# ============================================================================
# Artifact Registry
# ============================================================================
variable "create_repository" {
  type        = bool
  description = "Create Artifact Registry repository"
  default     = true
}

variable "immutable_tags" {
  type        = bool
  description = "Use immutable tags for container images"
  default     = true
}

variable "kms_key_name" {
  type        = string
  description = "KMS key for encrypting images"
  default     = ""
}

# ============================================================================
# Other Configuration
# ============================================================================
variable "deletion_protection" {
  type        = bool
  description = "Enable deletion protection"
  default     = false
}

variable "resource_labels" {
  type        = map(string)
  description = "Resource labels"
  default = {
    managed-by = "terraform"
    app        = "erlmcp"
  }
}
