# ============================================================================
# Observability Module Variables
# ============================================================================

variable "project_id" {
  type        = string
  description = "GCP project ID"
}

# ============================================================================
# Notification Channels
# ============================================================================
variable "notification_channels" {
  type = object({
    email = object({
      enabled = bool
      address = string
    })
    pagerduty = object({
      enabled    = bool
      service_key = string
      auth_token  = string
    })
    slack = object({
      enabled     = bool
      channel_name = string
      auth_token   = string
    })
    webhook = object({
      enabled    = bool
      url        = string
      auth_token = string
    })
  })
  description = "Notification channel configurations"
  default = {
    email = {
      enabled = false
      address = ""
    }
    pagerduty = {
      enabled    = false
      service_key = ""
      auth_token  = ""
    }
    slack = {
      enabled     = false
      channel_name = ""
      auth_token   = ""
    }
    webhook = {
      enabled    = false
      url        = ""
      auth_token = ""
    }
  }
  sensitive = true
}

# ============================================================================
# Uptime Check Configuration
# ============================================================================
variable "create_uptime_check" {
  type        = bool
  description = "Create uptime check"
  default     = true
}

variable "uptime_check_path" {
  type        = string
  description = "Health check endpoint path"
  default     = "/health"
}

variable "uptime_check_port" {
  type        = number
  description = "Health check port"
  default     = 8080
}

variable "uptime_check_method" {
  type        = string
  description = "HTTP method for health check"
  default     = "GET"
}

variable "uptime_check_use_ssl" {
  type        = bool
  description = "Use HTTPS for health check"
  default     = false
}

variable "uptime_check_validate_ssl" {
  type        = bool
  description = "Validate SSL certificate"
  default     = false
}

variable "uptime_check_host" {
  type        = string
  description = "Hostname for uptime check"
  default     = ""
}

variable "uptime_check_resource_type" {
  type        = string
  description = "Monitored resource type"
  default     = "uptime_url"
}

variable "uptime_check_timeout" {
  type        = string
  description = "Timeout for uptime check"
  default     = "10s"
}

variable "uptime_check_period" {
  type        = string
  description = "Check interval"
  default     = "60s"
}

variable "uptime_check_regions" {
  type        = list(string)
  description = "Regions to run uptime checks from"
  default     = ["USA", "EUROPE", "ASIA_PACIFIC"]
}

variable "uptime_check_content_matcher" {
  type        = string
  description = "Expected content in response"
  default     = "OK"
}

# ============================================================================
# SLO Configuration
# ============================================================================
variable "create_slos" {
  type        = bool
  description = "Create SLOs"
  default     = true
}

variable "slo_availability_goal" {
  type        = number
  description = "Availability SLO goal (0.0 - 1.0)"
  default     = 0.995

  validation {
    condition     = var.slo_availability_goal > 0 && var.slo_availability_goal <= 1
    error_message = "Availability goal must be between 0 and 1."
  }
}

variable "slo_latency_goal" {
  type        = number
  description = "Latency SLO goal (0.0 - 1.0)"
  default     = 0.95

  validation {
    condition     = var.slo_latency_goal > 0 && var.slo_latency_goal <= 1
    error_message = "Latency goal must be between 0 and 1."
  }
}

variable "slo_latency_threshold_ms" {
  type        = number
  description = "Latency threshold in milliseconds"
  default     = 500
}

variable "slo_rolling_period_days" {
  type        = number
  description = "Rolling period for SLO calculation"
  default     = 30
}

# ============================================================================
# Alert Policy Configuration
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

variable "error_rate_alert_duration" {
  type        = string
  description = "Alert duration threshold"
  default     = "300s"
}

variable "error_rate_alert_alignment_period" {
  type        = string
  description = "Alignment period for aggregation"
  default     = "60s"
}

variable "error_rate_alert_notification_period" {
  type        = string
  description = "Minimum time between notifications"
  default     = "3600s"
}

variable "error_rate_alert_auto_close" {
  type        = bool
  description = "Auto-close incident when condition no longer met"
  default     = true
}

variable "enable_latency_alert" {
  type        = bool
  description = "Enable high latency alert"
  default     = true
}

variable "latency_alert_threshold" {
  type        = number
  description = "Latency threshold in seconds"
  default     = 1.0
}

variable "latency_alert_duration" {
  type        = string
  description = "Alert duration threshold"
  default     = "300s"
}

variable "latency_alert_alignment_period" {
  type        = string
  description = "Alignment period for aggregation"
  default     = "60s"
}

variable "latency_alert_notification_period" {
  type        = string
  description = "Minimum time between notifications"
  default     = "3600s"
}

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

variable "memory_alert_duration" {
  type        = string
  description = "Alert duration threshold"
  default     = "300s"
}

variable "memory_alert_alignment_period" {
  type        = string
  description = "Alignment period for aggregation"
  default     = "60s"
}

variable "memory_alert_auto_close" {
  type        = bool
  description = "Auto-close incident when condition no longer met"
  default     = true
}

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

variable "cpu_alert_duration" {
  type        = string
  description = "Alert duration threshold"
  default     = "300s"
}

variable "cpu_alert_alignment_period" {
  type        = string
  description = "Alignment period for aggregation"
  default     = "60s"
}

variable "enable_health_check_alert" {
  type        = bool
  description = "Enable health check failure alert"
  default     = true
}

variable "health_check_alert_duration" {
  type        = string
  description = "Alert duration threshold"
  default     = "60s"
}

variable "health_check_alert_alignment_period" {
  type        = string
  description = "Alignment period for aggregation"
  default     = "60s"
}

variable "enable_process_count_alert" {
  type        = bool
  description = "Enable low process count alert"
  default     = true
}

variable "process_count_alert_threshold" {
  type        = number
  description = "Minimum process count threshold"
  default     = 100
}

variable "process_count_alert_duration" {
  type        = string
  description = "Alert duration threshold"
  default     = "300s"
}

variable "process_count_alert_alignment_period" {
  type        = string
  description = "Alignment period for aggregation"
  default     = "60s"
}

# ============================================================================
# Dashboard Configuration
# ============================================================================
variable "create_performance_dashboard" {
  type        = bool
  description = "Create performance dashboard"
  default     = true
}

variable "create_erlang_dashboard" {
  type        = bool
  description = "Create Erlang VM dashboard"
  default     = true
}

variable "create_security_dashboard" {
  type        = bool
  description = "Create security dashboard"
  default     = true
}

# ============================================================================
# Log Export Configuration
# ============================================================================
variable "log_export_bigquery_enabled" {
  type        = bool
  description = "Export logs to BigQuery"
  default     = false
}

variable "log_export_bigquery_destination" {
  type        = string
  description = "BigQuery dataset destination"
  default     = ""
}

variable "log_export_storage_enabled" {
  type        = bool
  description = "Export logs to Cloud Storage"
  default     = false
}

variable "log_export_storage_destination" {
  type        = string
  description = "Cloud Storage bucket destination"
  default     = ""
}

variable "log_export_filter" {
  type        = string
  description = "Filter for log export"
  default     = ""
}

# ============================================================================
# Log Exclusion Configuration
# ============================================================================
variable "exclude_health_check_logs" {
  type        = bool
  description = "Exclude health check request logs"
  default     = true
}

variable "health_check_log_filter" {
  type        = string
  description = "Filter for health check logs"
  default     = "resource.type=\"http_load_balancer\" request_path=\"/health\""
}

variable "exclude_successful_get_logs" {
  type        = bool
  description = "Exclude successful GET request logs"
  default     = true
}

variable "successful_get_log_filter" {
  type        = string
  description = "Filter for successful GET logs"
  default     = "resource.type=\"gce_instance\" logName=\"requests\" httpRequest.requestMethod=\"GET\" status<400"
}
