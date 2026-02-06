# ============================================================================
# Secret Manager Module Variables - V3 Security Architecture
# Zero-Trust Configuration with Least-Privilege Access
# ============================================================================

variable "project_id" {
  type        = string
  description = "GCP project ID"

  validation {
    condition     = can(regex("^[a-z][a-z0-9-]{4,28}[a-z0-9]$", var.project_id))
    error_message = "Project ID must be 6-30 characters, lowercase letters, digits, and hyphens."
  }
}

variable "labels" {
  type        = map(string)
  description = "Labels to apply to all secrets"
  default = {
    app         = "erlmcp"
    managed-by  = "terraform"
    environment = "production"
  }
}

# ============================================================================
# Secret Values
# ============================================================================
variable "secrets" {
  type = object({
    erlang_cookie    = string
    db_password      = string
    redis_password   = string
    tls_cert         = string
    tls_key          = string
    ca_bundle        = string
    jwt_private_key  = string
    jwt_public_key   = string
    grafana_password = string
    backup_key       = string
    otel_ca_cert     = string
  })
  description = "Secret values (empty strings will generate random values where applicable)"
  default = {
    erlang_cookie    = ""
    db_password      = ""
    redis_password   = ""
    tls_cert         = ""
    tls_key          = ""
    ca_bundle        = ""
    jwt_private_key  = ""
    jwt_public_key   = ""
    grafana_password = ""
    backup_key       = ""
    otel_ca_cert     = ""
  }
  sensitive = true
}

variable "create_secret_versions" {
  type        = bool
  description = "Create initial secret versions (set to false if managing externally)"
  default     = false
}

# ============================================================================
# IAM Configuration - Least-Privilege Access by Workload Type
# Zero-Trust: Each workload type has minimal required access
# ============================================================================

# Cluster Runtime Access
variable "cluster_service_accounts" {
  type        = list(string)
  description = "Service accounts for Erlang cluster nodes (Erlang cookie access)"
  default     = []

  validation {
    condition     = alltrue([for sa in var.cluster_service_accounts : can(regex("^(serviceAccount:|user:|group:)", sa))])
    error_message = "Service accounts must have prefix: serviceAccount:, user:, or group:"
  }
}

# Database Access
variable "database_service_accounts" {
  type        = list(string)
  description = "Service accounts for services requiring database access"
  default     = []

  validation {
    condition     = alltrue([for sa in var.database_service_accounts : can(regex("^(serviceAccount:|user:|group:)", sa))])
    error_message = "Service accounts must have prefix: serviceAccount:, user:, or group:"
  }
}

# Cache Access
variable "cache_service_accounts" {
  type        = list(string)
  description = "Service accounts for services requiring cache access"
  default     = []

  validation {
    condition     = alltrue([for sa in var.cache_service_accounts : can(regex("^(serviceAccount:|user:|group:)", sa))])
    error_message = "Service accounts must have prefix: serviceAccount:, user:, or group:"
  }
}

# TLS Termination
variable "tls_service_accounts" {
  type        = list(string)
  description = "Service accounts for TLS termination (load balancers, API gateways)"
  default     = []

  validation {
    condition     = alltrue([for sa in var.tls_service_accounts : can(regex("^(serviceAccount:|user:|group:)", sa))])
    error_message = "Service accounts must have prefix: serviceAccount:, user:, or group:"
  }
}

# Authentication Services
variable "auth_service_accounts" {
  type        = list(string)
  description = "Service accounts for authentication services (JWT signing)"
  default     = []

  validation {
    condition     = alltrue([for sa in var.auth_service_accounts : can(regex("^(serviceAccount:|user:|group:)", sa))])
    error_message = "Service accounts must have prefix: serviceAccount:, user:, or group:"
  }
}

# Application Services (General)
variable "app_service_accounts" {
  type        = list(string)
  description = "Service accounts for general application services (JWT verification, CA bundle)"
  default     = []

  validation {
    condition     = alltrue([for sa in var.app_service_accounts : can(regex("^(serviceAccount:|user:|group:)", sa))])
    error_message = "Service accounts must have prefix: serviceAccount:, user:, or group:"
  }
}

# Monitoring Services
variable "monitoring_service_accounts" {
  type        = list(string)
  description = "Service accounts for monitoring services (Grafana)"
  default     = []

  validation {
    condition     = alltrue([for sa in var.monitoring_service_accounts : can(regex("^(serviceAccount:|user:|group:)", sa))])
    error_message = "Service accounts must have prefix: serviceAccount:, user:, or group:"
  }
}

# Backup Services
variable "backup_service_accounts" {
  type        = list(string)
  description = "Service accounts for backup services (backup encryption key)"
  default     = []

  validation {
    condition     = alltrue([for sa in var.backup_service_accounts : can(regex("^(serviceAccount:|user:|group:)", sa))])
    error_message = "Service accounts must have prefix: serviceAccount:, user:, or group:"
  }
}

# Observability Services
variable "observability_service_accounts" {
  type        = list(string)
  description = "Service accounts for observability services (OpenTelemetry)"
  default     = []

  validation {
    condition     = alltrue([for sa in var.observability_service_accounts : can(regex("^(serviceAccount:|user:|group:)", sa))])
    error_message = "Service accounts must have prefix: serviceAccount:, user:, or group:"
  }
}

# Secret Viewers (Metadata Only)
variable "secret_viewers" {
  type        = list(string)
  description = "Principals that can view secret metadata (but NOT values) - Security/Audit teams"
  default     = []

  validation {
    condition     = alltrue([for viewer in var.secret_viewers : can(regex("^(serviceAccount:|user:|group:)", viewer))])
    error_message = "Viewers must have prefix: serviceAccount:, user:, or group:"
  }
}

# Secret Administrators
variable "secret_administrators" {
  type        = list(string)
  description = "Principals with full secret management access - Security team only"
  default     = []

  validation {
    condition     = alltrue([for admin in var.secret_administrators : can(regex("^(serviceAccount:|user:|group:)", admin))])
    error_message = "Administrators must have prefix: serviceAccount:, user:, or group:"
  }
}

# ============================================================================
# Encryption Configuration - CMEK
# ============================================================================
variable "enable_cmek" {
  type        = bool
  description = "Enable Customer-Managed Encryption Keys for secrets"
  default     = true
}

variable "kms_location" {
  type        = string
  description = "Location for KMS key ring (e.g., us-central1)"
  default     = "us-central1"
}

variable "kms_key_rotation_period" {
  type        = string
  description = "KMS key rotation period (default: 90 days)"
  default     = "7776000s"
}

# ============================================================================
# Replication Configuration
# ============================================================================
variable "secret_replication_locations" {
  type        = list(string)
  description = "Locations for user-managed replication of sensitive secrets"
  default     = ["us-central1", "us-east1"]

  validation {
    condition     = length(var.secret_replication_locations) >= 1
    error_message = "At least one replication location must be specified."
  }
}

# ============================================================================
# Automatic Rotation Configuration
# ============================================================================
variable "enable_rotation" {
  type        = bool
  description = "Enable automatic secret rotation for applicable secrets"
  default     = true
}

variable "rotation_periods" {
  type = object({
    critical = string  # 30 days for cluster secrets
    high     = string  # 90 days for database/cache passwords
    crypto   = string  # 365 days for cryptographic keys
  })
  description = "Rotation periods by secret classification"
  default = {
    critical = "2592000s"   # 30 days
    high     = "7776000s"   # 90 days
    crypto   = "31536000s"  # 365 days
  }

  validation {
    condition = (
      can(regex("^[0-9]+s$", var.rotation_periods.critical)) &&
      can(regex("^[0-9]+s$", var.rotation_periods.high)) &&
      can(regex("^[0-9]+s$", var.rotation_periods.crypto))
    )
    error_message = "Rotation periods must be specified in seconds (e.g., '2592000s')."
  }
}

variable "enable_rotation_notifications" {
  type        = bool
  description = "Enable Pub/Sub notifications for secret rotation events"
  default     = true
}

# ============================================================================
# Audit and Compliance Configuration
# ============================================================================
variable "enable_audit_logging" {
  type        = bool
  description = "Enable comprehensive audit logging for all secret operations"
  default     = true
}

variable "enable_vpc_service_controls" {
  type        = bool
  description = "Enable VPC Service Controls for secret access"
  default     = false
}
