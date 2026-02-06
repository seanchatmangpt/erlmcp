# ErlMCP GCP Infrastructure - Outputs
# Output values from GCP Terraform configuration
# Updated: 2026-02-06

# ============================================================================
# Project Information
# ============================================================================

output "project_id" {
  description = "GCP Project ID where resources are deployed"
  value       = var.project_id
}

output "region" {
  description = "Primary GCP Region for resource deployment"
  value       = var.region
}

output "environment" {
  description = "Environment name (dev, staging, production)"
  value       = var.environment
}

# ============================================================================
# Service Account Information
# ============================================================================

output "service_account_email" {
  description = "Email address of the deployment service account"
  value       = google_service_account.erlmcp_deploy.email
}

output "service_account_id" {
  description = "Unique ID of the deployment service account"
  value       = google_service_account.erlmcp_deploy.unique_id
}

output "service_account_name" {
  description = "Full resource name of the deployment service account"
  value       = google_service_account.erlmcp_deploy.name
}

output "service_account_key_id" {
  description = "ID of the service account key (for CI/CD)"
  value       = google_service_account_key.erlmcp_deploy_key.name
  sensitive   = true
}

output "service_account_key_private" {
  description = "Private key data for service account (base64 encoded)"
  value       = google_service_account_key.erlmcp_deploy_key.private_key
  sensitive   = true
}

# ============================================================================
# Artifact Registry
# ============================================================================

output "artifact_registry_repo_url" {
  description = "Full URL for Artifact Registry Docker repository"
  value       = "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp.repository_id}"
}

output "artifact_registry_repo_name" {
  description = "Name of the Artifact Registry repository"
  value       = google_artifact_registry_repository.erlmcp.repository_id
}

output "artifact_registry_repo_id" {
  description = "Full resource ID of the Artifact Registry repository"
  value       = google_artifact_registry_repository.erlmcp.id
}

output "artifact_registry_location" {
  description = "Location of the Artifact Registry repository"
  value       = google_artifact_registry_repository.erlmcp.location
}

output "docker_push_command" {
  description = "Sample Docker push command for the Artifact Registry"
  value       = "docker push ${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp.repository_id}/IMAGE_NAME:TAG"
}

# ============================================================================
# Cloud KMS Encryption
# ============================================================================

output "kms_keyring_id" {
  description = "ID of the Cloud KMS key ring"
  value       = google_kms_key_ring.erlmcp.id
}

output "kms_keyring_name" {
  description = "Name of the Cloud KMS key ring"
  value       = google_kms_key_ring.erlmcp.name
}

output "kms_crypto_key_app_id" {
  description = "ID of the application encryption key"
  value       = google_kms_crypto_key.erlmcp_app.id
}

output "kms_crypto_key_app_name" {
  description = "Name of the application encryption key"
  value       = google_kms_crypto_key.erlmcp_app.name
}

output "kms_crypto_key_db_id" {
  description = "ID of the database encryption key"
  value       = google_kms_crypto_key.erlmcp_db.id
}

output "kms_crypto_key_db_name" {
  description = "Name of the database encryption key"
  value       = google_kms_crypto_key.erlmcp_db.name
}

# ============================================================================
# Secret Manager
# ============================================================================

output "database_url_secret_id" {
  description = "Secret Manager secret ID for database URL"
  value       = google_secret_manager_secret.database_url.id
}

output "database_url_secret_name" {
  description = "Secret Manager secret name for database URL"
  value       = google_secret_manager_secret.database_url.secret_id
}

output "api_keys_secret_id" {
  description = "Secret Manager secret ID for API keys"
  value       = google_secret_manager_secret.api_keys.id
}

output "api_keys_secret_name" {
  description = "Secret Manager secret name for API keys"
  value       = google_secret_manager_secret.api_keys.secret_id
}

output "deployment_creds_secret_id" {
  description = "Secret Manager secret ID for deployment credentials"
  value       = google_secret_manager_secret.deployment_credentials.id
}

output "deployment_creds_secret_name" {
  description = "Secret Manager secret name for deployment credentials"
  value       = google_secret_manager_secret.deployment_credentials.secret_id
}

# ============================================================================
# Cloud Storage
# ============================================================================

output "terraform_state_bucket_name" {
  description = "Name of the Terraform state storage bucket"
  value       = google_storage_bucket.terraform_state.name
}

output "terraform_state_bucket_url" {
  description = "GCS URL for Terraform state bucket"
  value       = "gs://${google_storage_bucket.terraform_state.name}"
}

output "terraform_state_bucket_self_link" {
  description = "Self-link for Terraform state bucket"
  value       = google_storage_bucket.terraform_state.self_link
}

output "security_logs_bucket_name" {
  description = "Name of the security logs storage bucket"
  value       = google_storage_bucket.security_logs.name
}

output "security_logs_bucket_url" {
  description = "GCS URL for security logs bucket"
  value       = "gs://${google_storage_bucket.security_logs.name}"
}

# ============================================================================
# Logging and Monitoring
# ============================================================================

output "log_bucket_id" {
  description = "ID of the centralized logging bucket"
  value       = google_logging_project_bucket_config.erlmcp_logs.id
}

output "log_bucket_name" {
  description = "Name of the centralized logging bucket"
  value       = google_logging_project_bucket_config.erlmcp_logs.bucket_id
}

output "security_log_sink_name" {
  description = "Name of the security log sink"
  value       = google_logging_project_sink.security_sink.name
}

output "security_log_sink_writer_identity" {
  description = "Writer identity for the security log sink"
  value       = google_logging_project_sink.security_sink.writer_identity
}

output "notification_channel_ids" {
  description = "IDs of the monitoring notification channels"
  value       = [for channel in google_monitoring_notification_channel.email : channel.id]
}

# ============================================================================
# Budget and Billing
# ============================================================================

output "budget_name" {
  description = "Name of the billing budget alert"
  value       = var.billing_account != "" ? google_billing_budget.erlmcp_budget[0].display_name : null
}

output "budget_id" {
  description = "ID of the billing budget alert"
  value       = var.billing_account != "" ? google_billing_budget.erlmcp_budget[0].name : null
}

# ============================================================================
# API Services
# ============================================================================

output "enabled_apis" {
  description = "List of enabled GCP APIs and services"
  value       = var.allowed_services
}

output "enabled_api_count" {
  description = "Number of enabled GCP APIs"
  value       = length(var.allowed_services)
}

# ============================================================================
# Configuration Details
# ============================================================================

output "kms_rotation_period_days" {
  description = "KMS key rotation period in days"
  value       = tonumber(replace(var.kms_key_rotation_period, "s", "")) / 86400
}

output "artifact_retention_count" {
  description = "Number of artifact versions to retain"
  value       = var.artifact_retention_count
}

output "log_retention_days" {
  description = "Log retention period in days"
  value       = var.log_retention_days
}

output "security_log_retention_days" {
  description = "Security log retention period in days"
  value       = var.security_log_retention_days
}

# ============================================================================
# Deployment Instructions
# ============================================================================

output "deployment_instructions" {
  description = "Step-by-step deployment and configuration instructions"
  value = {
    step1 = "Authenticate: gcloud auth application-default login"
    step2 = "Set project: gcloud config set project ${var.project_id}"
    step3 = "Initialize Terraform: terraform init -backend-config='bucket=${google_storage_bucket.terraform_state.name}' -backend-config='prefix=${var.environment}/terraform.tfstate'"
    step4 = "Review changes: terraform plan -var-file=project.tfvars"
    step5 = "Apply configuration: terraform apply -var-file=project.tfvars"
    step6 = "Authenticate Docker: gcloud auth configure-docker ${var.region}-docker.pkg.dev"
    step7 = "Store secrets: Use 'gcloud secrets versions add' to populate secret values"
    step8 = "Configure GitHub: Add service account key to GitHub secrets as GCP_SA_KEY"
  }
}

output "gcloud_commands" {
  description = "Useful gcloud commands for this deployment"
  value = {
    view_secrets       = "gcloud secrets list --project=${var.project_id}"
    view_service_accounts = "gcloud iam service-accounts list --project=${var.project_id}"
    view_artifact_repos  = "gcloud artifacts repositories list --project=${var.project_id} --location=${var.region}"
    view_kms_keys       = "gcloud kms keys list --keyring=${google_kms_key_ring.erlmcp.name} --location=${var.region}"
    view_logs           = "gcloud logging read 'severity>=ERROR' --limit=50 --project=${var.project_id}"
    view_buckets        = "gcloud storage ls --project=${var.project_id}"
  }
}

# ============================================================================
# Security Configuration Summary
# ============================================================================

output "security_summary" {
  description = "Summary of security configurations"
  value = {
    encryption_at_rest    = "Customer-managed keys (CMEK) via Cloud KMS"
    encryption_in_transit = "TLS 1.2+ enforced"
    secret_rotation      = "Automatic rotation enabled (${var.kms_key_rotation_period})"
    audit_logging        = var.enable_audit_logs ? "Enabled" : "Disabled"
    vulnerability_scan   = var.enable_vulnerability_scanning ? "Enabled" : "Disabled"
    binary_authorization = var.enable_binary_authorization ? "Enabled" : "Disabled"
    compliance_framework = var.compliance_framework
  }
}

# ============================================================================
# Quick Reference URLs
# ============================================================================

output "console_urls" {
  description = "Google Cloud Console URLs for key resources"
  value = {
    project           = "https://console.cloud.google.com/home/dashboard?project=${var.project_id}"
    artifact_registry = "https://console.cloud.google.com/artifacts?project=${var.project_id}"
    secrets          = "https://console.cloud.google.com/security/secret-manager?project=${var.project_id}"
    kms              = "https://console.cloud.google.com/security/kms?project=${var.project_id}"
    service_accounts = "https://console.cloud.google.com/iam-admin/serviceaccounts?project=${var.project_id}"
    logs             = "https://console.cloud.google.com/logs?project=${var.project_id}"
    monitoring       = "https://console.cloud.google.com/monitoring?project=${var.project_id}"
    storage          = "https://console.cloud.google.com/storage/browser?project=${var.project_id}"
    billing          = "https://console.cloud.google.com/billing?project=${var.project_id}"
  }
}

# ============================================================================
# Terraform Backend Configuration
# ============================================================================

output "terraform_backend_config" {
  description = "Configuration for migrating to remote backend"
  value = {
    backend = "gcs"
    config = {
      bucket = google_storage_bucket.terraform_state.name
      prefix = "${var.environment}/terraform.tfstate"
    }
    initialization_command = "terraform init -backend-config='bucket=${google_storage_bucket.terraform_state.name}' -backend-config='prefix=${var.environment}/terraform.tfstate' -migrate-state"
  }
}

# ============================================================================
# Resource Summary
# ============================================================================

output "resource_summary" {
  description = "Summary of created resources"
  value = {
    service_accounts     = 1
    artifact_repositories = 1
    kms_keyrings         = 1
    kms_crypto_keys      = 2
    secrets              = 3
    storage_buckets      = 2
    log_buckets          = 1
    log_sinks            = 1
    notification_channels = length(google_monitoring_notification_channel.email)
    budgets              = var.billing_account != "" ? 1 : 0
    enabled_apis         = length(var.allowed_services)
  }
}
