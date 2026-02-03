# ==============================================================================
# ERLMCP GCP Infrastructure Outputs
# ==============================================================================
# All outputs from the infrastructure provisioning.
# Use these values for configuration of applications and CI/CD pipelines.
# ==============================================================================

# ==============================================================================
# Project Outputs
# ==============================================================================

output "project_id" {
  description = "GCP Project ID"
  value       = var.project_id
}

output "region" {
  description = "GCP Region"
  value       = var.region
}

output "environment" {
  description = "Deployment environment"
  value       = var.environment
}

# ==============================================================================
# Service Account Outputs
# ==============================================================================

output "service_account_email" {
  description = "Service account email address"
  value       = google_service_account.erlmcp_deploy.email
}

output "service_account_id" {
  description = "Service account ID"
  value       = google_service_account.erlmcp_deploy.unique_id
}

# ==============================================================================
# Artifact Registry Outputs
# ==============================================================================

output "artifact_registry_repo_url" {
  description = "Artifact Registry repository URL"
  value       = "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp.repository_id}"
}

output "artifact_registry_repo_name" {
  description = "Artifact Registry repository name"
  value       = google_artifact_registry_repository.erlmcp.repository_id
}

output "docker_push_command" {
  description = "Command to push Docker images"
  value       = "docker push ${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp.repository_id}/erlmcp:TAG"
}

# ==============================================================================
# KMS Outputs
# ==============================================================================

output "kms_keyring_id" {
  description = "KMS key ring ID"
  value       = google_kms_key_ring.erlmcp.id
}

output "kms_crypto_key_id" {
  description = "KMS crypto key ID"
  value       = google_kms_crypto_key.erlmcp.id
}

# ==============================================================================
# Secret Manager Outputs
# ==============================================================================

output "database_url_secret_id" {
  description = "Secret Manager database URL secret ID"
  value       = google_secret_manager_secret.database_url.id
}

output "erlang_cookie_secret_id" {
  description = "Secret Manager Erlang cookie secret ID"
  value       = google_secret_manager_secret.erlang_cookie.id
}

output "api_keys_secret_id" {
  description = "Secret Manager API keys secret ID"
  value       = google_secret_manager_secret.api_keys.id
}

output "deployment_creds_secret_id" {
  description = "Secret Manager deployment credentials secret ID"
  value       = google_secret_manager_secret.deployment_credentials.id
}

# ==============================================================================
# Cloud Run Outputs
# ==============================================================================

output "cloud_run_service_url" {
  description = "Cloud Run service URL"
  value       = google_cloud_run_v2_service.erlmcp.uri
}

output "cloud_run_service_name" {
  description = "Cloud Run service name"
  value       = google_cloud_run_v2_service.erlmcp.name
}

output "cloud_run_latest_revision" {
  description = "Latest Cloud Run revision"
  value       = google_cloud_run_v2_service.erlmcp.latest_ready_revision
}

output "cloud_run_custom_domain" {
  description = "Cloud Run custom domain (if configured)"
  value       = var.cloud_run_custom_domain != "" ? var.cloud_run_custom_domain : null
}

# ==============================================================================
# VPC Outputs
# ==============================================================================

output "vpc_network_id" {
  description = "VPC network ID"
  value       = var.create_vpc ? google_compute_network.erlmcp[0].id : null
}

output "vpc_network_name" {
  description = "VPC network name"
  value       = var.create_vpc ? google_compute_network.erlmcp[0].name : var.existing_vpc_name
}

output "vpc_primary_subnet_id" {
  description = "Primary subnet ID"
  value       = var.create_vpc ? google_compute_subnetwork.erlmcp_primary[0].id : null
}

output "vpc_connector_id" {
  description = "VPC Access Connector ID"
  value       = var.vpc_connector_enabled ? google_vpc_access_connector.erlmcp[0].id : null
}

# ==============================================================================
# Cloud SQL Outputs
# ==============================================================================

output "cloud_sql_instance_name" {
  description = "Cloud SQL instance name"
  value       = var.enable_cloud_sql ? google_sql_database_instance.erlmcp[0].name : null
}

output "cloud_sql_connection_name" {
  description = "Cloud SQL connection name (for Cloud SQL Proxy)"
  value       = var.enable_cloud_sql ? google_sql_database_instance.erlmcp[0].connection_name : null
}

output "cloud_sql_private_ip" {
  description = "Cloud SQL private IP address"
  value       = var.enable_cloud_sql ? google_sql_database_instance.erlmcp[0].private_ip_address : null
  sensitive   = true
}

output "cloud_sql_database_name" {
  description = "Database name"
  value       = var.enable_cloud_sql ? google_sql_database.erlmcp[0].name : null
}

output "cloud_sql_proxy_service_account" {
  description = "Cloud SQL Proxy service account email"
  value       = var.enable_cloud_sql ? google_service_account.cloudsql_proxy[0].email : null
}

# ==============================================================================
# Monitoring Outputs
# ==============================================================================

output "dashboard_url" {
  description = "Cloud Monitoring dashboard URL"
  value       = "https://console.cloud.google.com/monitoring/dashboards/builder/${google_monitoring_dashboard.erlmcp.id}?project=${var.project_id}"
}

output "uptime_check_id" {
  description = "Uptime check ID"
  value       = google_monitoring_uptime_check_config.erlmcp_health.uptime_check_id
}

# ==============================================================================
# Terraform State Outputs
# ==============================================================================

output "terraform_state_bucket_name" {
  description = "Terraform state bucket name"
  value       = google_storage_bucket.terraform_state.name
}

output "terraform_state_bucket_url" {
  description = "Terraform state bucket URL"
  value       = "gs://${google_storage_bucket.terraform_state.name}"
}

# ==============================================================================
# Enabled APIs
# ==============================================================================

output "enabled_apis" {
  description = "List of enabled GCP APIs"
  value       = var.allowed_services
}

# ==============================================================================
# Connection Strings (for applications)
# ==============================================================================

output "connection_info" {
  description = "Connection information for applications"
  value = {
    cloud_run_url = google_cloud_run_v2_service.erlmcp.uri
    database = var.enable_cloud_sql ? {
      host     = google_sql_database_instance.erlmcp[0].private_ip_address
      database = google_sql_database.erlmcp[0].name
      user     = google_sql_user.erlmcp[0].name
    } : null
  }
  sensitive = true
}

# ==============================================================================
# Deployment Instructions
# ==============================================================================

output "deployment_instructions" {
  description = "Next steps for deployment"
  value = {
    step1 = "Initialize Terraform: terraform init -backend-config='bucket=${google_storage_bucket.terraform_state.name}' -backend-config='prefix=${var.environment}'"
    step2 = "Review changes: terraform plan -var-file=terraform.${var.environment}.tfvars"
    step3 = "Apply configuration: terraform apply -var-file=terraform.${var.environment}.tfvars"
    step4 = "Build and push Docker image: docker build -t ${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp.repository_id}/erlmcp:latest . && docker push ${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp.repository_id}/erlmcp:latest"
    step5 = "Deploy to Cloud Run: gcloud run deploy ${google_cloud_run_v2_service.erlmcp.name} --image ${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp.repository_id}/erlmcp:latest --region ${var.region}"
  }
}

# ==============================================================================
# CI/CD Configuration
# ==============================================================================

output "cicd_config" {
  description = "Configuration values for CI/CD pipelines"
  value = {
    project_id              = var.project_id
    region                  = var.region
    artifact_registry       = "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp.repository_id}"
    cloud_run_service       = google_cloud_run_v2_service.erlmcp.name
    service_account         = google_service_account.erlmcp_deploy.email
    database_secret         = google_secret_manager_secret.database_url.secret_id
    erlang_cookie_secret    = google_secret_manager_secret.erlang_cookie.secret_id
  }
}
