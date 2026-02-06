# ============================================================================
# Example Variables
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

variable "pagerduty_service_key" {
  type        = string
  description = "PagerDuty service integration key"
  default     = ""
  sensitive   = true
}

variable "pagerduty_auth_token" {
  type        = string
  description = "PagerDuty authentication token"
  default     = ""
  sensitive   = true
}

variable "slack_token" {
  type        = string
  description = "Slack bot authentication token"
  default     = ""
  sensitive   = true
}

variable "webhook_token" {
  type        = string
  description = "Webhook authentication token"
  default     = ""
  sensitive   = true
}
