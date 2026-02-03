# ============================================================================
# Cloud Run Deployment Variables
# ============================================================================

variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "region" {
  type        = string
  description = "GCP region"
  default     = "us-central1"
}

variable "service_name" {
  type        = string
  description = "Cloud Run service name"
  default     = "erlmcp"
}

# ============================================================================
# Container Configuration
# ============================================================================
variable "image_repository" {
  type        = string
  description = "Container image repository"
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

variable "cpu" {
  type        = string
  description = "CPU allocation"
  default     = "1"
}

variable "memory" {
  type        = string
  description = "Memory allocation"
  default     = "512Mi"
}

variable "cpu_idle" {
  type        = bool
  description = "Enable CPU throttling when idle"
  default     = true
}

variable "timeout_seconds" {
  type        = number
  description = "Request timeout"
  default     = 300
}

# ============================================================================
# Scaling Configuration
# ============================================================================
variable "min_instances" {
  type        = number
  description = "Minimum instances"
  default     = 0
}

variable "max_instances" {
  type        = number
  description = "Maximum instances"
  default     = 10
}

# ============================================================================
# Health Check Configuration
# ============================================================================
variable "health_check_path" {
  type        = string
  description = "Health check endpoint path"
  default     = "/health"
}

# ============================================================================
# Networking Configuration
# ============================================================================
variable "ingress_setting" {
  type        = string
  description = "Ingress setting"
  default     = "INGRESS_TRAFFIC_ALL"
}

variable "allow_public_access" {
  type        = bool
  description = "Allow unauthenticated access"
  default     = false
}

variable "invoker_service_account" {
  type        = string
  description = "Service account to invoke the service (when not public)"
  default     = ""
}

variable "custom_domain" {
  type        = string
  description = "Custom domain (leave empty to disable)"
  default     = ""
}

variable "enable_tls" {
  type        = bool
  description = "Enable TLS configuration"
  default     = false
}

# ============================================================================
# Environment Configuration
# ============================================================================
variable "environment" {
  type        = string
  description = "Application environment"
  default     = "production"
}

variable "container_env" {
  type = list(object({
    name  = string
    value = string
  }))
  description = "Additional environment variables"
  default = [
    { name = "ERL_AFLAGS", value = "-proto_dist inet_tls" },
    { name = "ERL_DIST_PORT", value = "9100" },
  ]
}

# ============================================================================
# Observability Configuration
# ============================================================================
variable "notification_channels" {
  type = object({
    email = object({
      enabled = bool
      address = string
    })
    slack = object({
      enabled     = bool
      channel_name = string
      auth_token   = string
    })
  })
  description = "Notification channel configurations"
  default = {
    email = {
      enabled = false
      address = ""
    }
    slack = {
      enabled     = false
      channel_name = ""
      auth_token   = ""
    }
  }
  sensitive = true
}

variable "create_uptime_check" {
  type        = bool
  description = "Create uptime check"
  default     = true
}

variable "create_slos" {
  type        = bool
  description = "Create SLOs"
  default     = true
}

variable "enable_error_rate_alert" {
  type        = bool
  description = "Enable high error rate alert"
  default     = true
}

variable "enable_latency_alert" {
  type        = bool
  description = "Enable high latency alert"
  default     = true
}

variable "enable_memory_alert" {
  type        = bool
  description = "Enable high memory alert"
  default     = true
}

# ============================================================================
# Labels
# ============================================================================
variable "labels" {
  type        = map(string)
  description = "Resource labels"
  default = {
    app        = "erlmcp"
    managed-by = "terraform"
  }
}
