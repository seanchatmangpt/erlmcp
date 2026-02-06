# ============================================================================
# Dashboards Module Variables
# ============================================================================

variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "environment" {
  type        = string
  description = "Environment name (production, staging, development)"
  default     = "production"
}

variable "service_name" {
  type        = string
  description = "Service name for SLO references"
  default     = "erlmcp-service"
}

variable "create_performance_dashboard" {
  type        = bool
  description = "Create performance analysis dashboard"
  default     = true
}

variable "create_erlang_dashboard" {
  type        = bool
  description = "Create Erlang VM internals dashboard"
  default     = true
}

variable "create_security_dashboard" {
  type        = bool
  description = "Create security and audit dashboard"
  default     = true
}

variable "create_slo_dashboard" {
  type        = bool
  description = "Create SLO and error budget dashboard"
  default     = true
}
