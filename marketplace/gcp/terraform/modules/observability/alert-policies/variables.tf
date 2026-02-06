# ============================================================================
# Alert Policies Module Variables
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

variable "notification_channels" {
  type        = list(string)
  description = "List of notification channel IDs"
  default     = []
}

variable "critical_channels" {
  type        = list(string)
  description = "Notification channels for critical alerts"
  default     = []
}

variable "warning_channels" {
  type        = list(string)
  description = "Notification channels for warning alerts"
  default     = []
}

variable "info_channels" {
  type        = list(string)
  description = "Notification channels for informational alerts"
  default     = []
}

# ============================================================================
# Error Rate Alert Configuration
# ============================================================================
variable "enable_error_rate_alert" {
  type        = bool
  description = "Enable high error rate alert"
  default     = true
}

variable "error_rate_alert_threshold" {
  type        = number
  description = "Error rate threshold (errors per second)"
  default     = 10
}

# ============================================================================
# Latency Alert Configuration
# ============================================================================
variable "enable_latency_alert" {
  type        = bool
  description = "Enable high latency alert"
  default     = true
}

variable "latency_alert_threshold" {
  type        = number
  description = "Latency threshold in seconds for P95"
  default     = 1.0
}

# ============================================================================
# Memory Alert Configuration
# ============================================================================
variable "enable_memory_alert" {
  type        = bool
  description = "Enable high memory alert"
  default     = true
}

variable "memory_alert_threshold" {
  type        = number
  description = "Memory threshold in bytes"
  default     = 2147483648  # 2GB
}

# ============================================================================
# CPU Alert Configuration
# ============================================================================
variable "enable_cpu_alert" {
  type        = bool
  description = "Enable high CPU alert"
  default     = true
}

variable "cpu_alert_threshold" {
  type        = number
  description = "CPU utilization threshold (0.0 - 1.0)"
  default     = 0.8
}

# ============================================================================
# Health Check Alert Configuration
# ============================================================================
variable "enable_health_check_alert" {
  type        = bool
  description = "Enable health check failure alert"
  default     = true
}

# ============================================================================
# Supervisor Restart Alert Configuration
# ============================================================================
variable "enable_supervisor_restart_alert" {
  type        = bool
  description = "Enable supervisor restart rate alert"
  default     = true
}

variable "supervisor_restart_threshold" {
  type        = number
  description = "Supervisor restart rate threshold (restarts per second)"
  default     = 5
}

# ============================================================================
# Connection Error Alert Configuration
# ============================================================================
variable "enable_connection_error_alert" {
  type        = bool
  description = "Enable connection error rate alert"
  default     = true
}

variable "connection_error_threshold" {
  type        = number
  description = "Connection error rate threshold (errors per second)"
  default     = 10
}

# ============================================================================
# Anomaly Detection Configuration
# ============================================================================
variable "enable_anomaly_detection" {
  type        = bool
  description = "Enable ML-based anomaly detection alerts"
  default     = false
}
