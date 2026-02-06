# marketplace/gcp/terraform/modules/iam/main.tf
# GCP IAM Module: Service Accounts, Workload Identity, Least Privilege
# Zero-trust, auditability, least privilege enforcement

terraform {
  required_version = ">= 1.5.0"
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
  }
}

# Variables
variable "project_id" {
  description = "GCP Project ID"
  type        = string
}

variable "environment" {
  description = "Environment (dev/staging/prod)"
  type        = string
}

variable "region" {
  description = "GCP Region"
  type        = string
}

variable "cluster_name" {
  description = "GKE Cluster Name"
  type        = string
}

variable "namespace" {
  description = "Kubernetes namespace for erlmcp services"
  type        = string
  default     = "erlmcp"
}

# Locals
locals {
  service_account_prefix = "erlmcp-${var.environment}"

  service_accounts = {
    server = {
      id          = "${local.service_account_prefix}-server"
      description = "Service account for erlmcp server workloads"
      roles = [
        "roles/logging.logWriter",
        "roles/monitoring.metricWriter",
        "roles/cloudtrace.agent",
      ]
    }

    client = {
      id          = "${local.service_account_prefix}-client"
      description = "Service account for erlmcp client workloads"
      roles = [
        "roles/logging.logWriter",
        "roles/monitoring.metricWriter",
      ]
    }

    registry = {
      id          = "${local.service_account_prefix}-registry"
      description = "Service account for erlmcp registry service"
      roles = [
        "roles/logging.logWriter",
        "roles/monitoring.metricWriter",
        "roles/cloudtrace.agent",
      ]
    }

    transport = {
      id          = "${local.service_account_prefix}-transport"
      description = "Service account for erlmcp transport layer"
      roles = [
        "roles/logging.logWriter",
        "roles/monitoring.metricWriter",
        "roles/cloudtrace.agent",
      ]
    }

    observability = {
      id          = "${local.service_account_prefix}-observability"
      description = "Service account for observability components"
      roles = [
        "roles/logging.logWriter",
        "roles/logging.viewer",
        "roles/monitoring.metricWriter",
        "roles/monitoring.viewer",
        "roles/cloudtrace.agent",
        "roles/cloudtrace.user",
      ]
    }

    operator = {
      id          = "${local.service_account_prefix}-operator"
      description = "Service account for erlmcp operators"
      roles = [
        "roles/logging.logWriter",
        "roles/monitoring.metricWriter",
      ]
    }
  }

  k8s_service_accounts = {
    server        = "erlmcp-server"
    client        = "erlmcp-client"
    registry      = "erlmcp-registry"
    transport     = "erlmcp-transport"
    observability = "erlmcp-observability"
    operator      = "erlmcp-operator"
  }
}

# Service Accounts
resource "google_service_account" "erlmcp" {
  for_each = local.service_accounts

  project      = var.project_id
  account_id   = each.value.id
  display_name = each.value.description
  description  = "${each.value.description} (${var.environment})"
}

# Project-level IAM Bindings (Least Privilege)
resource "google_project_iam_member" "service_account_roles" {
  for_each = merge([
    for sa_key, sa_config in local.service_accounts : {
      for role in sa_config.roles :
      "${sa_key}-${replace(role, "/", "-")}" => {
        service_account = google_service_account.erlmcp[sa_key].email
        role            = role
      }
    }
  ]...)

  project = var.project_id
  role    = each.value.role
  member  = "serviceAccount:${each.value.service_account}"
}

# Workload Identity Bindings
resource "google_service_account_iam_member" "workload_identity" {
  for_each = local.service_accounts

  service_account_id = google_service_account.erlmcp[each.key].name
  role               = "roles/iam.workloadIdentityUser"
  member             = "serviceAccount:${var.project_id}.svc.id.goog[${var.namespace}/${local.k8s_service_accounts[each.key]}]"
}

# Custom IAM Role for erlmcp Operations (Ultra Least Privilege)
resource "google_project_iam_custom_role" "erlmcp_minimal" {
  project     = var.project_id
  role_id     = "${replace(local.service_account_prefix, "-", "_")}_minimal"
  title       = "erlmcp Minimal Operations Role (${var.environment})"
  description = "Minimal permissions for erlmcp MCP operations"

  permissions = [
    "logging.logEntries.create",
    "monitoring.timeSeries.create",
    "cloudtrace.traces.patch",
  ]
}

# Secret Manager Access (if secrets enabled)
resource "google_project_iam_custom_role" "erlmcp_secrets" {
  project     = var.project_id
  role_id     = "${replace(local.service_account_prefix, "-", "_")}_secrets"
  title       = "erlmcp Secrets Reader (${var.environment})"
  description = "Read-only access to erlmcp secrets"

  permissions = [
    "secretmanager.versions.access",
    "secretmanager.versions.get",
  ]
}

# Service Account for CI/CD (separate from runtime)
resource "google_service_account" "erlmcp_ci" {
  project      = var.project_id
  account_id   = "${local.service_account_prefix}-ci"
  display_name = "erlmcp CI/CD Service Account"
  description  = "Service account for erlmcp CI/CD pipelines (${var.environment})"
}

# CI/CD Service Account Permissions (Container Registry, Artifact Registry)
resource "google_project_iam_member" "ci_roles" {
  for_each = toset([
    "roles/artifactregistry.writer",
    "roles/storage.objectAdmin", # For GCS artifacts
    "roles/logging.logWriter",
  ])

  project = var.project_id
  role    = each.key
  member  = "serviceAccount:${google_service_account.erlmcp_ci.email}"
}

# Service Account Keys (Avoid if possible, prefer Workload Identity)
# Uncomment only if absolutely necessary for legacy integrations
# resource "google_service_account_key" "erlmcp_ci_key" {
#   service_account_id = google_service_account.erlmcp_ci.name
#
#   # Rotation reminder
#   lifecycle {
#     ignore_changes = [private_key]
#   }
# }

# Audit Config for Service Account Usage
resource "google_project_iam_audit_config" "service_accounts" {
  project = var.project_id
  service = "iam.googleapis.com"

  audit_log_config {
    log_type = "ADMIN_READ"
  }

  audit_log_config {
    log_type = "DATA_READ"
  }

  audit_log_config {
    log_type = "DATA_WRITE"
  }
}

# Outputs
output "service_accounts" {
  description = "Map of service account emails"
  value = {
    for key, sa in google_service_account.erlmcp :
    key => sa.email
  }
}

output "service_account_ids" {
  description = "Map of service account unique IDs"
  value = {
    for key, sa in google_service_account.erlmcp :
    key => sa.unique_id
  }
}

output "workload_identity_annotations" {
  description = "Kubernetes service account annotations for Workload Identity"
  value = {
    for key, sa in google_service_account.erlmcp :
    local.k8s_service_accounts[key] => {
      "iam.gke.io/gcp-service-account" = sa.email
    }
  }
}

output "ci_service_account_email" {
  description = "CI/CD service account email"
  value       = google_service_account.erlmcp_ci.email
}

# Security Hardening: Service Account Impersonation Prevention
resource "google_service_account_iam_binding" "prevent_impersonation" {
  for_each = local.service_accounts

  service_account_id = google_service_account.erlmcp[each.key].name
  role               = "roles/iam.serviceAccountTokenCreator"

  # Explicitly deny - only allow specific authorized principals
  members = []

  condition {
    title       = "Deny all token creation"
    description = "Prevent service account impersonation"
    expression  = "false"
  }
}

# Resource Labels for Governance
locals {
  common_labels = {
    environment  = var.environment
    managed_by   = "terraform"
    system       = "erlmcp"
    security     = "least-privilege"
    compliance   = "zero-trust"
  }
}

resource "google_project_iam_member" "service_account_viewer" {
  project = var.project_id
  role    = "roles/iam.serviceAccountViewer"
  member  = "serviceAccount:${google_service_account.erlmcp["observability"].email}"
}

# Conditional Access Bindings (Example: Production only)
resource "google_project_iam_member" "production_only" {
  count = var.environment == "prod" ? 1 : 0

  project = var.project_id
  role    = "roles/cloudkms.cryptoKeyDecrypter"
  member  = "serviceAccount:${google_service_account.erlmcp["server"].email}"

  condition {
    title       = "Production encryption access"
    description = "Allow KMS decryption in production only"
    expression  = "resource.type == 'cloudkms.googleapis.com/CryptoKey'"
  }
}

# Service Account Usage Metrics
resource "google_monitoring_alert_policy" "service_account_usage" {
  project      = var.project_id
  display_name = "erlmcp Service Account Anomalous Usage (${var.environment})"
  combiner     = "OR"

  conditions {
    display_name = "High API call rate"

    condition_threshold {
      filter          = "resource.type = \"service_account\" AND resource.labels.unique_id = one_of(${join(",", [for sa in google_service_account.erlmcp : sa.unique_id])})"
      duration        = "300s"
      comparison      = "COMPARISON_GT"
      threshold_value = 1000

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_RATE"
      }
    }
  }

  notification_channels = []

  alert_strategy {
    auto_close = "1800s"
  }
}
