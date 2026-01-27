output "project_id" {
  description = "GCP Project ID"
  value       = var.project_id
}

output "region" {
  description = "GCP Region"
  value       = var.region
}

output "service_account_email" {
  description = "Service account email address"
  value       = google_service_account.erlmcp_deploy.email
}

output "service_account_id" {
  description = "Service account ID"
  value       = google_service_account.erlmcp_deploy.unique_id
}

output "artifact_registry_repo_url" {
  description = "Artifact Registry repository URL"
  value       = "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp.repository_id}"
}

output "artifact_registry_repo_name" {
  description = "Artifact Registry repository name"
  value       = google_artifact_registry_repository.erlmcp.repository_id
}

output "kms_keyring_id" {
  description = "KMS key ring ID"
  value       = google_kms_key_ring.erlmcp.id
}

output "kms_crypto_key_id" {
  description = "KMS crypto key ID"
  value       = google_kms_crypto_key.erlmcp.id
}

output "database_url_secret_id" {
  description = "Secret Manager database URL secret ID"
  value       = google_secret_manager_secret.database_url.id
}

output "api_keys_secret_id" {
  description = "Secret Manager API keys secret ID"
  value       = google_secret_manager_secret.api_keys.id
}

output "deployment_creds_secret_id" {
  description = "Secret Manager deployment credentials secret ID"
  value       = google_secret_manager_secret.deployment_credentials.id
}

output "terraform_state_bucket_name" {
  description = "Terraform state bucket name"
  value       = google_storage_bucket.terraform_state.name
}

output "terraform_state_bucket_url" {
  description = "Terraform state bucket URL"
  value       = "gs://${google_storage_bucket.terraform_state.name}"
}

output "enabled_apis" {
  description = "List of enabled GCP APIs"
  value       = var.allowed_services
}

output "deployment_instructions" {
  description = "Next steps for deployment"
  value = {
    step1 = "Initialize Terraform: terraform init -backend-config='bucket=${google_storage_bucket.terraform_state.name}' -backend-config='prefix=prod'"
    step2 = "Review changes: terraform plan -var-file=project.tfvars"
    step3 = "Apply configuration: terraform apply -var-file=project.tfvars"
    step4 = "Store secrets in Secret Manager"
    step5 = "Configure GitHub secrets for CI/CD"
  }
}
