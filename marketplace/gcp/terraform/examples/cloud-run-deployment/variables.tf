# ============================================================================
# Cloud Run Gen2 Full Deployment Variables
# Enterprise-grade configuration with all Gen2 features
# ============================================================================

# ============================================================================
# Core Configuration
# ============================================================================

variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "region" {
  type        = string
  description = "GCP region for Cloud Run deployment"
  default     = "us-central1"
}

variable "service_name" {
  type        = string
  description = "Cloud Run service name (will be suffixed with random string)"
  default     = "erlmcp"
}

variable "environment" {
  type        = string
  description = "Deployment environment (dev, staging, production)"
  default     = "production"

  validation {
    condition     = contains(["dev", "staging", "production"], var.environment)
    error_message = "Environment must be dev, staging, or production"
  }
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
  description = "Container image name (e.g., 'erlmcp/erlmcp')"
  default     = "erlmcp/erlmcp"
}

variable "image_tag" {
  type        = string
  description = "Container image tag"
  default     = "3.0.0"
}

variable "create_repository" {
  type        = bool
  description = "Create Artifact Registry repository"
  default     = false
}

# ============================================================================
# Resource Configuration (Gen2 Features)
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
  description = "Memory allocation (e.g., '512Mi', '1Gi', '2Gi')"
  default     = "512Mi"

  validation {
    condition     = can(regex("^[0-9]+(Mi|Gi)$", var.memory))
    error_message = "Memory must be in format: number + Mi or Gi"
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

variable "timeout_seconds" {
  type        = number
  description = "Request timeout in seconds (1-3600)"
  default     = 300

  validation {
    condition     = var.timeout_seconds >= 1 && var.timeout_seconds <= 3600
    error_message = "Timeout must be between 1 and 3600 seconds"
  }
}

# ============================================================================
# Scaling Configuration
# ============================================================================

variable "min_instances" {
  type        = number
  description = "Minimum instances (0 = scale to zero, 1+ = warm start)"
  default     = 0

  validation {
    condition     = var.min_instances >= 0
    error_message = "Minimum instances must be >= 0"
  }
}

variable "max_instances" {
  type        = number
  description = "Maximum instances"
  default     = 10

  validation {
    condition     = var.max_instances >= 1 && var.max_instances <= 1000
    error_message = "Maximum instances must be between 1 and 1000"
  }
}

variable "max_concurrency" {
  type        = number
  description = "Maximum concurrent requests per instance"
  default     = 80

  validation {
    condition     = var.max_concurrency >= 1 && var.max_concurrency <= 1000
    error_message = "Concurrency must be between 1 and 1000"
  }
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
# Health Check Configuration
# ============================================================================

variable "health_check_path" {
  type        = string
  description = "Health check endpoint path"
  default     = "/health"
}

# ============================================================================
# Environment Variables
# ============================================================================

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

variable "log_level" {
  type        = string
  description = "Application log level"
  default     = "info"

  validation {
    condition     = contains(["debug", "info", "warning", "error"], var.log_level)
    error_message = "Log level must be debug, info, warning, or error"
  }
}

variable "enable_metrics" {
  type        = bool
  description = "Enable metrics collection"
  default     = true
}

variable "enable_tracing" {
  type        = bool
  description = "Enable distributed tracing"
  default     = true
}

# ============================================================================
# Security Configuration
# ============================================================================

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

variable "session_affinity" {
  type        = bool
  description = "Enable session affinity for sticky sessions (Gen2 feature)"
  default     = false
}

variable "enable_tls" {
  type        = bool
  description = "Enable TLS configuration with certificate volumes"
  default     = false
}

# ============================================================================
# Domain Configuration
# ============================================================================

variable "custom_domain" {
  type        = string
  description = "Custom domain name (empty = use Cloud Run URL)"
  default     = ""
}

variable "create_domain_mapping" {
  type        = bool
  description = "Create Cloud Run domain mapping"
  default     = true
}

# ============================================================================
# Observability Configuration
# ============================================================================

variable "enable_observability" {
  type        = bool
  description = "Enable observability module (monitoring, alerts, dashboards)"
  default     = true
}

variable "notification_channels" {
  type = object({
    email = object({
      enabled = bool
      address = string
    })
    slack = object({
      enabled      = bool
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
      enabled      = false
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
  description = "Create SLOs (Service Level Objectives)"
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
