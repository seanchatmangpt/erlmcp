output "repository_id" {
  description = "The ID of the primary repository"
  value       = google_artifact_registry_repository.main.repository_id
}

output "repository_name" {
  description = "The full resource name of the primary repository"
  value       = google_artifact_registry_repository.main.name
}

output "repository_location" {
  description = "The location of the primary repository"
  value       = google_artifact_registry_repository.main.location
}

output "repository_url" {
  description = "The URL of the primary repository"
  value       = "${google_artifact_registry_repository.main.location}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.main.repository_id}"
}

output "replica_repositories" {
  description = "Map of replica repository resources by location"
  value = {
    for location, repo in google_artifact_registry_repository.replicas : location => {
      id       = repo.id
      name     = repo.name
      location = repo.location
      url      = "${repo.location}-docker.pkg.dev/${var.project_id}/${repo.repository_id}"
    }
  }
}

output "replica_locations" {
  description = "List of replica locations"
  value       = [for repo in google_artifact_registry_repository.replicas : repo.location]
}

output "all_repository_urls" {
  description = "List of all repository URLs (primary + replicas)"
  value = concat(
    ["${google_artifact_registry_repository.main.location}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.main.repository_id}"],
    [for repo in google_artifact_registry_repository.replicas : "${repo.location}-docker.pkg.dev/${var.project_id}/${repo.repository_id}"]
  )
}

output "kms_key_name" {
  description = "KMS key used for encryption"
  value       = google_artifact_registry_repository.main.kms_key_name
}

output "format" {
  description = "Repository format"
  value       = google_artifact_registry_repository.main.format
}

output "mode" {
  description = "Repository mode"
  value       = google_artifact_registry_repository.main.mode
}

output "create_time" {
  description = "The time the primary repository was created"
  value       = google_artifact_registry_repository.main.create_time
}

output "update_time" {
  description = "The time the primary repository was last updated"
  value       = google_artifact_registry_repository.main.update_time
}
