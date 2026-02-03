# ErlMCP v3 Terraform Variables

variable "environment" {
  description = "Deployment environment (dev, staging, prod)"
  type        = string
  default     = "staging"
  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be one of: dev, staging, prod"
  }
}

variable "aws_region" {
  description = "AWS region for deployment"
  type        = string
  default     = "us-east-1"
}

variable "aws_account_id" {
  description = "AWS account ID"
  type        = string
}

variable "iam_admin_arn" {
  description = "ARN of the IAM admin user"
  type        = string
  sensitive   = true
}

variable "image_registry" {
  description = "Container image registry"
  type        = string
  default     = "ghcr.io/erlmcp"
}

variable "image_tag" {
  description = "Container image tag"
  type        = string
  default     = "latest"
}

variable "rds_password" {
  description = "RDS database password"
  type        = string
  sensitive   = true
}

variable "enable_monitoring" {
  description = "Enable monitoring stack"
  type        = bool
  default     = true
}

variable "backup_retention_period" {
  description = "Backup retention period in days"
  type        = number
  default     = 7
}

variable "node_count" {
  description = "Number of worker nodes"
  type        = number
  default     = 3
}

variable "node_instance_type" {
  description = "EC2 instance type for worker nodes"
  type        = string
  default     = "m6i.large"
}

variable "enable_auto_scaling" {
  description = "Enable auto-scaling for worker nodes"
  type        = bool
  default     = true
}

variable "min_nodes" {
  description = "Minimum number of worker nodes"
  type        = number
  default     = 2
}

variable "max_nodes" {
  description = "Maximum number of worker nodes"
  type        = number
  default     = 10
}

variable "canary_node_count" {
  description = "Number of canary worker nodes"
  type        = number
  default     = 1
}

variable "enable_canary" {
  description = "Enable canary deployment strategy"
  type        = bool
  default     = true
}

variable "enable_feature_flags" {
  description = "Enable feature flag service"
  type        = bool
  default     = true
}

variable "feature_flag_storage" {
  description = "Feature flag storage backend"
  type        = string
  default     = "redis"
  validation {
    condition     = contains(["redis", "database"], var.feature_flag_storage)
    error_message = "Feature flag storage must be either 'redis' or 'database'"
  }
}

variable "enable_metrics" {
  description = "Enable metrics collection"
  type        = bool
  default     = true
}

variable "tracing_enabled" {
  description = "Enable distributed tracing"
  type        = bool
  default     = true
}

variable "log_level" {
  description = "Application log level"
  type        = string
  default     = "info"
  validation {
    condition     = contains(["debug", "info", "warn", "error"], var.log_level)
    error_message = "Log level must be one of: debug, info, warn, error"
  }
}

variable "enable_cors" {
  description = "Enable CORS support"
  type        = bool
  default     = true
}

variable "cors_origins" {
  description = "CORS origins"
  type        = list(string)
  default     = ["*"]
}

variable "max_connections" {
  description = "Maximum number of connections"
  type        = number
  default     = 10000
}

variable "connection_timeout" {
  description = "Connection timeout in seconds"
  type        = number
  default     = 30
}

variable "request_timeout" {
  description = "Request timeout in seconds"
  type        = number
  default     = 60
}

variable "enable_rate_limiting" {
  description = "Enable rate limiting"
  type        = bool
  default     = true
}

variable "rate_limit_requests" {
  description = "Rate limit requests per minute"
  type        = number
  default     = 1000
}

variable "enable_authentication" {
  description = "Enable authentication"
  type        = bool
  default     = true
}

variable "auth_provider" {
  description = "Authentication provider"
  type        = string
  default     = "jwt"
  validation {
    condition     = contains(["jwt", "oauth", "ldap"], var.auth_provider)
    error_message = "Auth provider must be one of: jwt, oauth, ldap"
  }
}

variable "jwt_secret" {
  description = "JWT secret key"
  type        = string
  sensitive   = true
}

variable "oauth_client_id" {
  description = "OAuth client ID"
  type        = string
  sensitive   = true
}

variable "oauth_client_secret" {
  description = "OAuth client secret"
  type        = string
  sensitive   = true
}

variable "enable_encryption" {
  description = "Enable encryption for data at rest"
  type        = bool
  default     = true
}

variable "encryption_key" {
  description = "Encryption key for data at rest"
  type        = string
  sensitive   = true
}

variable "enable_compliance" {
  description = "Enable compliance monitoring"
  type        = bool
  default     = true
}

variable "compliance_standards" {
  description = "List of compliance standards to enforce"
  type        = list(string)
  default     = ["ISO27001", "SOC2", "PCI-DSS"]
}

variable "enable_auditing" {
  description = "Enable audit logging"
  type        = bool
  default     = true
}

variable "audit_log_retention" {
  description = "Audit log retention period in days"
  type        = number
  default     = 365
}

variable "enable_backup" {
  description = "Enable automatic backups"
  type        = bool
  default     = true
}

variable "backup_frequency" {
  description = "Backup frequency (daily, weekly, monthly)"
  type        = string
  default     = "daily"
  validation {
    condition     = contains(["daily", "weekly", "monthly"], var.backup_frequency)
    error_message = "Backup frequency must be one of: daily, weekly, monthly"
  }
}