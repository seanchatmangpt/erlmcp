variable "project_id" {
  description = "GCP Project ID"
  type        = string
}

variable "region" {
  description = "GCP Region"
  type        = string
  default     = "us-central1"
}

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "production"
}

variable "billing_account" {
  description = "GCP Billing Account ID"
  type        = string
  sensitive   = true
}

variable "app_name" {
  description = "Application name"
  type        = string
  default     = "erlmcp-tai"
}

variable "service_account_name" {
  description = "Service account name"
  type        = string
  default     = "erlmcp-deploy"
}

variable "allowed_services" {
  description = "GCP services to enable"
  type        = list(string)
  default = [
    "compute.googleapis.com",
    "cloudrun.googleapis.com",
    "artifactregistry.googleapis.com",
    "secretmanager.googleapis.com",
    "cloudkms.googleapis.com",
    "monitoring.googleapis.com",
    "logging.googleapis.com",
    "cloudtrace.googleapis.com",
    "cloudbuild.googleapis.com",
  ]
}

variable "kms_key_rotation_period" {
  description = "KMS key rotation period"
  type        = string
  default     = "7776000s" # 90 days
}
