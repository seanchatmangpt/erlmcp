# Variable definitions for Kubernetes Infrastructure

# Environment variables
variable "environment" {
  type        = string
  description = "Environment name (dev, staging, production)"
  default     = "production"
}

variable "region" {
  type        = string
  description = "AWS region"
  default     = "us-west-2"
}

variable "cluster_name" {
  type        = string
  description = "EKS cluster name"
  default     = "erlmcp-prod"
}

variable "domain" {
  type        = string
  description = "Domain name for erlmcp"
  default     = "example.com"
}

# Kubernetes configuration
variable "kubeconfig_path" {
  type        = string
  description = "Path to kubeconfig file"
  default     = "~/.kube/config"
}

variable "kube_context" {
  type        = string
  description = "Kubernetes context name"
  default     = "minikube"
}

# Application configuration
variable "log_level" {
  type        = string
  description = "Log level for erlmcp"
  default     = "info"
  validation {
    condition     = contains(["debug", "info", "warn", "error"], var.log_level)
    error_message = "Log level must be one of: debug, info, warn, error"
  }
}

variable "metrics_enabled" {
  type        = bool
  description = "Enable metrics collection"
  default     = true
}

variable "tracing_enabled" {
  type        = bool
  description = "Enable distributed tracing"
  default     = true
}

# Deployment configuration
variable "min_replicas" {
  type        = number
  description = "Minimum number of replicas"
  default     = 3
}

variable "max_replicas" {
  type        = number
  description = "Maximum number of replicas"
  default     = 10
}

variable "target_cpu_utilization" {
  type        = number
  description = "Target CPU utilization percentage"
  default     = 70
}

variable "target_memory_utilization" {
  type        = number
  description = "Target memory utilization percentage"
  default     = 80
}

# Secrets configuration
variable "database_url" {
  type        = string
  description = "Database connection URL"
  sensitive   = true
  default     = "postgresql://user:password@db:5432/erlmcp"
}

variable "redis_url" {
  type        = string
  description = "Redis connection URL"
  sensitive   = true
  default     = "redis://redis:6379"
}

variable "oauth_secret" {
  type        = string
  description = "OAuth secret"
  sensitive   = true
  default     = ""
}

variable "encryption_key" {
  type        = string
  description = "Encryption key"
  sensitive   = true
  default     = ""
}

variable "api_key" {
  type        = string
  description = "API key"
  sensitive   = true
  default     = ""
}

# Feature flags
variable "enable_canary" {
  type        = bool
  description = "Enable canary deployments"
  default     = true
}

variable "enable_blue_green" {
  type        = bool
  description = "Enable blue-green deployments"
  default     = true
}

variable "enable_service_mesh" {
  type        = bool
  description = "Enable service mesh (Istio)"
  default     = false
}

variable "enable_autoscaling" {
  type        = bool
  description = "Enable autoscaling"
  default     = true
}

variable "enable_pod_security" {
  type        = bool
  description = "Enable Pod Security Policies"
  default     = true
}

# Monitoring configuration
variable "monitoring_enabled" {
  type        = bool
  description = "Enable monitoring"
  default     = true
}

variable "tracing_sample_rate" {
  type        = number
  description = "Tracing sample rate (0.0 to 1.0)"
  default     = 0.1
  validation {
    condition     = var.tracing_sample_rate >= 0 && var.tracing_sample_rate <= 1
    error_message = "Tracing sample rate must be between 0.0 and 1.0"
  }
}

# Security configuration
variable "image_pull_policy" {
  type        = string
  description = "Image pull policy"
  default     = "IfNotPresent"
  validation {
    condition     = contains(["Always", "Never", "IfNotPresent"], var.image_pull_policy)
    error_message = "Image pull policy must be one of: Always, Never, IfNotPresent"
  }
}

variable "image_pull_secrets" {
  type        = list(string)
  description = "Image pull secrets"
  default     = ["regcred"]
}

variable "pod_security_context" {
  type = object({
    run_as_user  = number
    run_as_group = number
    fs_group     = number
  })
  default = {
    run_as_user  = 1000
    run_as_group = 1000
    fs_group     = 1000
  }
}

variable "container_security_context" {
  type = object({
    allow_privilege_escalation = bool
    read_only_root_filesystem   = bool
    run_as_non_root             = bool
  })
  default = {
    allow_privilege_escalation = false
    read_only_root_filesystem   = true
    run_as_non_root             = true
  }
}

# Resource limits
variable "resource_requests" {
  type = object({
    cpu    = string
    memory = string
  })
  default = {
    cpu    = "250m"
    memory = "256Mi"
  }
}

variable "resource_limits" {
  type = object({
    cpu    = string
    memory = string
  })
  default = {
    cpu    = "500m"
    memory = "512Mi"
  }
}

# Health check configuration
variable "liveness_probe" {
  type = object({
    initial_delay_seconds = number
    period_seconds        = number
    timeout_seconds       = number
    failure_threshold     = number
    path                  = string
  })
  default = {
    initial_delay_seconds = 30
    period_seconds        = 10
    timeout_seconds       = 5
    failure_threshold     = 3
    path                  = "/health"
  }
}

variable "readiness_probe" {
  type = object({
    initial_delay_seconds = number
    period_seconds        = number
    timeout_seconds       = number
    failure_threshold     = number
    path                  = string
  })
  default = {
    initial_delay_seconds = 5
    period_seconds        = 5
    timeout_seconds       = 3
    failure_threshold     = 3
    path                  = "/ready"
  }
}

# Environment-specific variables
variable "env_specific" {
  type = map(object({
    replicas    = number
    cpu         = string
    memory      = string
    log_level   = string
  }))

  default = {
    dev = {
      replicas    = 1
      cpu         = "100m"
      memory      = "128Mi"
      log_level   = "debug"
    }
    staging = {
      replicas    = 2
      cpu         = "250m"
      memory      = "256Mi"
      log_level   = "info"
    }
    production = {
      replicas    = 3
      cpu         = "500m"
      memory      = "512Mi"
      log_level   = "info"
    }
  }
}