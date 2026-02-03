# ============================================================================
# Secret Manager Module Variables
# ============================================================================

variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "labels" {
  type        = map(string)
  description = "Labels to apply to all secrets"
  default = {
    app = "erlmcp"
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
# IAM Configuration
# ============================================================================
variable "secret_accessors" {
  type        = list(string)
  description = "Principals that can access secret values"
  default     = []
}

variable "secret_viewers" {
  type        = list(string)
  description = "Principals that can view secret metadata (but not values)"
  default     = []
}

# ============================================================================
# Automatic Rotation (optional)
# ============================================================================
variable "enable_rotation" {
  type        = bool
  description = "Enable automatic secret rotation"
  default     = false
}

variable "rotation_period" {
  type        = string
  description = "Rotation period (e.g., 7776000s for 90 days)"
  default     = "7776000s"
}
