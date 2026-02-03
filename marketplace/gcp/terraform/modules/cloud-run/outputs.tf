# ============================================================================
# Cloud Run Module Outputs
# ============================================================================

output "service_name" {
  description = "Cloud Run service name"
  value       = google_cloud_run_v2_service.erlmcp.name
}

output "service_id" {
  description = "Cloud Run service ID"
  value       = google_cloud_run_v2_service.erlmcp.id
}

output "service_url" {
  description = "Cloud Run service URL"
  value       = google_cloud_run_v2_service.erlmcp.uri
}

output "service_status" {
  description = "Cloud Run service status"
  value       = google_cloud_run_v2_service.erlmcp.latest_ready_revision
}

output "location" {
  description = "Service location"
  value       = google_cloud_run_v2_service.erlmcp.location
}

output "service_account_email" {
  description = "Service account email"
  value       = var.create_service_account ? google_service_account.erlmcp[0].email : var.service_account_email
}

output "repository_id" {
  description = "Artifact Registry repository ID"
  value       = var.create_repository ? google_artifact_registry_repository.erlmcp[0].repository_id : null
}

output "repository_url" {
  description = "Artifact Registry repository URL"
  value = var.create_repository ? format(
    "%s-%s.artifactregistry.appspot.com/%s",
    var.region,
    var.project_id,
    google_artifact_registry_repository.erlmcp[0].repository_id
  ) : null
}

output "container_image" {
  description = "Full container image URL"
  value       = "${var.image_repository}/${var.project_id}/${var.image_name}:${var.image_tag}"
}

output "health_check_url" {
  description = "Health check URL"
  value       = "${google_cloud_run_v2_service.erlmcp.uri}${var.health_check_path}"
}
