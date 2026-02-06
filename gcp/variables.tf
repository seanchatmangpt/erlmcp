# ErlMCP GCP Infrastructure - Variables
# Variable definitions for GCP Terraform configuration
# Updated: 2026-02-06

# ============================================================================
# Core Project Configuration
# ============================================================================

variable "project_id" {
  description = "GCP Project ID where resources will be created"
  type        = string

  validation {
    condition     = can(regex("^[a-z][a-z0-9-]{4,28}[a-z0-9]$", var.project_id))
    error_message = "Project ID must be 6-30 characters, start with a letter, and contain only lowercase letters, numbers, and hyphens."
  }
}

variable "region" {
  description = "GCP Region for resource deployment"
  type        = string
  default     = "us-central1"

  validation {
    condition = contains([
      "us-central1", "us-east1", "us-east4", "us-west1", "us-west2", "us-west3", "us-west4",
      "europe-west1", "europe-west2", "europe-west3", "europe-west4", "europe-west6",
      "asia-east1", "asia-east2", "asia-northeast1", "asia-northeast2", "asia-northeast3",
      "asia-south1", "asia-southeast1", "asia-southeast2",
      "australia-southeast1", "australia-southeast2",
      "northamerica-northeast1", "northamerica-northeast2",
      "southamerica-east1", "southamerica-west1"
    ], var.region)
    error_message = "Region must be a valid GCP region."
  }
}

variable "environment" {
  description = "Environment name (dev, staging, production)"
  type        = string
  default     = "production"

  validation {
    condition     = contains(["dev", "development", "staging", "production", "prod"], var.environment)
    error_message = "Environment must be one of: dev, development, staging, production, prod."
  }
}

variable "billing_account" {
  description = "GCP Billing Account ID for budget alerts and cost tracking"
  type        = string
  sensitive   = true
  default     = ""

  validation {
    condition     = var.billing_account == "" || can(regex("^[A-F0-9]{6}-[A-F0-9]{6}-[A-F0-9]{6}$", var.billing_account))
    error_message = "Billing account must be in format XXXXXX-XXXXXX-XXXXXX or empty string."
  }
}

# ============================================================================
# Application Configuration
# ============================================================================

variable "app_name" {
  description = "Application name used for resource naming and tagging"
  type        = string
  default     = "erlmcp-tai"

  validation {
    condition     = can(regex("^[a-z][a-z0-9-_]{2,29}$", var.app_name))
    error_message = "App name must be 3-30 characters, start with a letter, and contain only lowercase letters, numbers, hyphens, and underscores."
  }
}

variable "service_account_name" {
  description = "Service account name for deployment and CI/CD operations"
  type        = string
  default     = "erlmcp-deploy"

  validation {
    condition     = can(regex("^[a-z][a-z0-9-]{4,28}[a-z0-9]$", var.service_account_name))
    error_message = "Service account name must be 6-30 characters, start with a letter, and contain only lowercase letters, numbers, and hyphens."
  }
}

# ============================================================================
# API and Service Configuration
# ============================================================================

variable "allowed_services" {
  description = "List of GCP APIs/services to enable in the project"
  type        = list(string)
  default = [
    "compute.googleapis.com",
    "container.googleapis.com",
    "cloudrun.googleapis.com",
    "artifactregistry.googleapis.com",
    "secretmanager.googleapis.com",
    "cloudkms.googleapis.com",
    "monitoring.googleapis.com",
    "logging.googleapis.com",
    "cloudtrace.googleapis.com",
    "cloudbuild.googleapis.com",
    "cloudfunctions.googleapis.com",
    "cloudscheduler.googleapis.com",
    "pubsub.googleapis.com",
    "storage-api.googleapis.com",
    "storage-component.googleapis.com",
    "sqladmin.googleapis.com",
    "redis.googleapis.com",
    "firestore.googleapis.com",
    "cloudresourcemanager.googleapis.com",
    "servicenetworking.googleapis.com",
    "vpcaccess.googleapis.com",
    "cloudasset.googleapis.com",
    "cloudapis.googleapis.com",
    "serviceusage.googleapis.com",
    "billingbudgets.googleapis.com"
  ]

  validation {
    condition     = alltrue([for s in var.allowed_services : can(regex("\\.googleapis\\.com$", s))])
    error_message = "All services must end with .googleapis.com"
  }
}

# ============================================================================
# Security and Encryption Configuration
# ============================================================================

variable "kms_key_rotation_period" {
  description = "KMS key rotation period in seconds (recommended: 90 days = 7776000s)"
  type        = string
  default     = "7776000s"

  validation {
    condition     = can(regex("^[0-9]+s$", var.kms_key_rotation_period))
    error_message = "KMS key rotation period must be a number followed by 's' (e.g., 7776000s)."
  }
}

variable "artifact_retention_count" {
  description = "Number of recent artifact versions to retain in Artifact Registry"
  type        = number
  default     = 10

  validation {
    condition     = var.artifact_retention_count >= 3 && var.artifact_retention_count <= 100
    error_message = "Artifact retention count must be between 3 and 100."
  }
}

# ============================================================================
# Logging and Monitoring Configuration
# ============================================================================

variable "log_retention_days" {
  description = "Number of days to retain logs in Cloud Logging"
  type        = number
  default     = 30

  validation {
    condition     = var.log_retention_days >= 1 && var.log_retention_days <= 3650
    error_message = "Log retention must be between 1 and 3650 days."
  }
}

variable "security_log_retention_days" {
  description = "Number of days to retain security logs (should be longer than general logs)"
  type        = number
  default     = 365

  validation {
    condition     = var.security_log_retention_days >= 30 && var.security_log_retention_days <= 3650
    error_message = "Security log retention must be between 30 and 3650 days."
  }
}

variable "alert_email_addresses" {
  description = "List of email addresses to receive monitoring alerts and notifications"
  type        = list(string)
  default     = []

  validation {
    condition     = alltrue([for email in var.alert_email_addresses : can(regex("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", email))])
    error_message = "All email addresses must be valid."
  }
}

# ============================================================================
# Budget and Cost Management
# ============================================================================

variable "monthly_budget_limit" {
  description = "Monthly budget limit in USD for cost alerts"
  type        = number
  default     = 1000

  validation {
    condition     = var.monthly_budget_limit >= 0
    error_message = "Monthly budget limit must be a non-negative number."
  }
}

# ============================================================================
# Backup and Disaster Recovery
# ============================================================================

variable "enable_cross_region_backup" {
  description = "Enable cross-region backup for critical resources"
  type        = bool
  default     = false
}

variable "backup_retention_days" {
  description = "Number of days to retain backups"
  type        = number
  default     = 30

  validation {
    condition     = var.backup_retention_days >= 7 && var.backup_retention_days <= 365
    error_message = "Backup retention must be between 7 and 365 days."
  }
}

# ============================================================================
# Network Configuration
# ============================================================================

variable "enable_private_google_access" {
  description = "Enable Private Google Access for VPC subnets"
  type        = bool
  default     = true
}

variable "enable_flow_logs" {
  description = "Enable VPC Flow Logs for network monitoring"
  type        = bool
  default     = true
}

# ============================================================================
# Feature Flags
# ============================================================================

variable "enable_workload_identity" {
  description = "Enable Workload Identity for GKE clusters"
  type        = bool
  default     = true
}

variable "enable_binary_authorization" {
  description = "Enable Binary Authorization for container image validation"
  type        = bool
  default     = true
}

variable "enable_vulnerability_scanning" {
  description = "Enable vulnerability scanning for container images"
  type        = bool
  default     = true
}

variable "enable_ddos_protection" {
  description = "Enable Cloud Armor DDoS protection"
  type        = bool
  default     = false
}

variable "enable_cdn" {
  description = "Enable Cloud CDN for content delivery"
  type        = bool
  default     = false
}

# ============================================================================
# Compliance and Governance
# ============================================================================

variable "enable_data_residency" {
  description = "Enable data residency controls to keep data in specific regions"
  type        = bool
  default     = false
}

variable "compliance_framework" {
  description = "Compliance framework to adhere to (none, sox, hipaa, pci-dss, gdpr)"
  type        = string
  default     = "sox"

  validation {
    condition     = contains(["none", "sox", "hipaa", "pci-dss", "gdpr", "iso27001"], var.compliance_framework)
    error_message = "Compliance framework must be one of: none, sox, hipaa, pci-dss, gdpr, iso27001."
  }
}

variable "enable_audit_logs" {
  description = "Enable comprehensive audit logging for compliance"
  type        = bool
  default     = true
}

# ============================================================================
# Tags and Labels
# ============================================================================

variable "additional_labels" {
  description = "Additional labels to apply to all resources"
  type        = map(string)
  default     = {}

  validation {
    condition     = alltrue([for k, v in var.additional_labels : can(regex("^[a-z][a-z0-9_-]{0,62}$", k))])
    error_message = "Label keys must start with a letter and contain only lowercase letters, numbers, underscores, and hyphens (max 63 chars)."
  }
}

# ============================================================================
# Advanced Configuration
# ============================================================================

variable "enable_organization_policies" {
  description = "Enable organization policy constraints for enhanced security"
  type        = bool
  default     = false
}

variable "enable_vpc_service_controls" {
  description = "Enable VPC Service Controls for data exfiltration prevention"
  type        = bool
  default     = false
}

variable "terraform_state_encryption" {
  description = "Enable customer-managed encryption for Terraform state"
  type        = bool
  default     = true
}

variable "max_retention_days" {
  description = "Maximum retention period for any resource (compliance requirement)"
  type        = number
  default     = 2555

  validation {
    condition     = var.max_retention_days >= 365 && var.max_retention_days <= 3650
    error_message = "Maximum retention must be between 365 and 3650 days (1-10 years)."
  }
}
