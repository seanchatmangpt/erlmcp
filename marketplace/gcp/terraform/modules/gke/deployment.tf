# ============================================================================
# Helm Deployment for erlmcp on GKE
# ============================================================================

resource "helm_release" "erlmcp" {
  count           = var.deploy_helm_chart ? 1 : 0
  name            = var.helm_release_name
  repository      = var.helm_chart_path
  chart           = var.helm_chart_path
  namespace       = var.helm_namespace
  create_namespace = true
  timeout         = 600
  cleanup_on_fail = false

  # GCP-specific values
  dynamic "set" {
    for_each = {
      "global.cloud.provider"    = "gcp"
      "global.cloud.region"      = var.region
      "global.cloud.gcpServiceAccount" = google_service_account.erlmcp[0].email
      "erlmcp.image.repository"  = "${var.image_repository}/${var.project_id}/${var.image_name}"
      "erlmcp.image.tag"         = var.image_tag
      "erlmcp.replicaCount"      = var.node_pools["primary"].min_count
      "erlmcp.resources.requests.cpu" = "500m"
      "erlmcp.resources.requests.memory" = "1Gi"
      "erlmcp.resources.limits.cpu" = "2000m"
      "erlmcp.resources.limits.memory" = "2Gi"
    }
    content {
      name  = set.key
      value = tostring(set.value)
    }
  }

  # Merge custom values
  values = [
    templatefile("${path.module}/values-gcp.yaml.tpl", {
      project_id     = var.project_id
      region         = var.region
      cluster_name   = var.cluster_name
      image_registry = var.image_repository
      image_name     = var.image_name
      image_tag      = var.image_tag
    })
  ]

  # Wait for resources to be ready
  wait = true

  # Maximum attempts for Helm operations
  max_history = 10

  # Reuse values
  reuse_values = false

  # Recreate pods if needed
  force_update = false

  # Atomic upgrade
  atomic = false

  # Dependency update
  dependency_update = false

  # Post-renderer (if needed)
  postrender {
    binary_path = "${path.module}/scripts/post-render.sh"
  }
}

# ============================================================================
# Service Account for Workload Identity
# ============================================================================
resource "google_service_account" "erlmcp" {
  count        = var.deploy_helm_chart ? 1 : 0
  project      = var.project_id
  account_id   = "erlmcp-ksa"
  display_name = "erlmcp Kubernetes Service Account"
  description  = "Service account for erlmcp pods with Workload Identity"
}

resource "google_service_account_iam_member" "workload_identity" {
  count              = var.deploy_helm_chart ? 1 : 0
  service_account_id = google_service_account.erlmcp[0].name
  role               = "roles/iam.workloadIdentityUser"
  member             = "serviceAccount:${var.project_id}.svc.id.goog[${var.helm_namespace}/${var.helm_release_name}]"
}

# ============================================================================
# IAM Bindings for Secret Manager Access
# ============================================================================
resource "google_project_iam_member" "secret_accessor" {
  count   = var.deploy_helm_chart ? 1 : 0
  project = var.project_id
  role    = "roles/secretmanager.secretAccessor"
  member  = "serviceAccount:${google_service_account.erlmcp[0].email}"
}

resource "google_project_iam_member" "secret_viewer" {
  count   = var.deploy_helm_chart ? 1 : 0
  project = var.project_id
  role    = "roles/secretmanager.viewer"
  member  = "serviceAccount:${google_service_account.erlmcp[0].email}"
}

# ============================================================================
# IAM Bindings for Monitoring and Logging
# ============================================================================
resource "google_project_iam_member" "logging_writer" {
  count   = var.deploy_helm_chart ? 1 : 0
  project = var.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.erlmcp[0].email}"
}

resource "google_project_iam_member" "monitoring_metric_writer" {
  count   = var.deploy_helm_chart ? 1 : 0
  project = var.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.erlmcp_sa[0].email}"
}

# ============================================================================
# External Secret Manager CSI Driver (optional)
# ============================================================================
resource "google_storage_bucket_iam_member" "bucket_accessor" {
  count  = var.enable_csi_secrets ? 1 : 0
  bucket = var.csi_bucket_name
  role   = "roles/storage.objectViewer"
  member = "serviceAccount:${google_service_account.erlmcp[0].email}"
}
