# ============================================================================
# Cloud Run Deployment for erlmcp
# ============================================================================
# Serverless container deployment with auto-scaling
#
# DOCKER-ONLY CONSTITUTION: Container images built via Docker quality gates
# ============================================================================

# ============================================================================
# Cloud Run Service
# ============================================================================

resource "google_cloud_run_v2_service" "erlmcp" {
  count = local.deploy_cloud_run ? 1 : 0

  name     = "${local.name_prefix}-service"
  location = var.region
  project  = var.project_id

  ingress = var.cloud_run_ingress

  template {
    service_account = google_service_account.erlmcp.email

    scaling {
      min_instance_count = var.cloud_run_min_instances
      max_instance_count = var.cloud_run_max_instances
    }

    timeout = "${var.cloud_run_timeout}s"

    containers {
      image = var.cloud_run_image != "" ? var.cloud_run_image : "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp[0].repository_id}/erlmcp:${var.erlmcp_version}"

      resources {
        limits = {
          cpu    = var.cloud_run_cpu
          memory = var.cloud_run_memory
        }
        cpu_idle          = var.cloud_run_cpu_idle
        startup_cpu_boost = true
      }

      # HTTP port
      ports {
        name           = "http1"
        container_port = 8080
      }

      # Environment variables
      env {
        name  = "ERLMCP_ENVIRONMENT"
        value = var.environment
      }
      env {
        name  = "ERLMCP_LOG_LEVEL"
        value = var.log_level
      }
      env {
        name  = "ERLMCP_HTTP_PORT"
        value = "8080"
      }
      env {
        name  = "ERLMCP_METRICS_PORT"
        value = "9568"
      }
      env {
        name  = "GCP_PROJECT_ID"
        value = var.project_id
      }
      env {
        name  = "GCP_REGION"
        value = var.region
      }

      # Secrets from Secret Manager
      dynamic "env" {
        for_each = var.cloud_run_secrets
        content {
          name = env.value.env_var
          value_source {
            secret_key_ref {
              secret  = env.value.secret_id
              version = env.value.version
            }
          }
        }
      }

      # Database configuration (if enabled)
      dynamic "env" {
        for_each = var.enable_cloud_sql ? [1] : []
        content {
          name = "DATABASE_URL"
          value_source {
            secret_key_ref {
              secret  = google_secret_manager_secret.db_url[0].secret_id
              version = "latest"
            }
          }
        }
      }

      # Redis configuration (if enabled)
      dynamic "env" {
        for_each = var.enable_memorystore ? [1] : []
        content {
          name  = "REDIS_HOST"
          value = google_redis_instance.main[0].host
        }
      }
      dynamic "env" {
        for_each = var.enable_memorystore ? [1] : []
        content {
          name  = "REDIS_PORT"
          value = tostring(google_redis_instance.main[0].port)
        }
      }

      # Volume mounts
      dynamic "volume_mounts" {
        for_each = var.enable_cloud_sql ? [1] : []
        content {
          name       = "cloudsql"
          mount_path = "/cloudsql"
        }
      }

      # Startup probe
      startup_probe {
        http_get {
          path = "/health"
          port = 8080
        }
        initial_delay_seconds = 5
        period_seconds        = 10
        failure_threshold     = 3
        timeout_seconds       = 5
      }

      # Liveness probe
      liveness_probe {
        http_get {
          path = "/health"
          port = 8080
        }
        period_seconds    = 30
        failure_threshold = 3
        timeout_seconds   = 5
      }
    }

    # Cloud SQL connection (if enabled)
    dynamic "volumes" {
      for_each = var.enable_cloud_sql ? [1] : []
      content {
        name = "cloudsql"
        cloud_sql_instance {
          instances = [google_sql_database_instance.main[0].connection_name]
        }
      }
    }

    # VPC connector for private networking
    dynamic "vpc_access" {
      for_each = var.cloud_run_vpc_connector != "" ? [1] : []
      content {
        connector = var.cloud_run_vpc_connector
        egress    = "PRIVATE_RANGES_ONLY"
      }
    }

    labels = local.common_labels
  }

  traffic {
    type    = "TRAFFIC_TARGET_ALLOCATION_TYPE_LATEST"
    percent = 100
  }

  labels = local.common_labels

  depends_on = [
    google_project_service.required_apis,
    google_secret_manager_secret_iam_member.cloud_run_access
  ]
}

# ============================================================================
# VPC Connector (for private networking)
# ============================================================================

resource "google_vpc_access_connector" "main" {
  count = local.deploy_cloud_run && var.cloud_run_vpc_connector == "" ? 1 : 0

  name          = "${local.name_prefix}-connector"
  project       = var.project_id
  region        = var.region
  network       = google_compute_network.main.id
  ip_cidr_range = var.vpc_connector_cidr

  min_instances = 2
  max_instances = 10

  depends_on = [google_project_service.required_apis]
}

# ============================================================================
# IAM - Public Access (if enabled)
# ============================================================================

resource "google_cloud_run_v2_service_iam_member" "public_access" {
  count = local.deploy_cloud_run && var.cloud_run_allow_unauthenticated ? 1 : 0

  location = google_cloud_run_v2_service.erlmcp[0].location
  name     = google_cloud_run_v2_service.erlmcp[0].name
  project  = var.project_id
  role     = "roles/run.invoker"
  member   = "allUsers"
}

# ============================================================================
# IAM - Secret Manager Access
# ============================================================================

resource "google_secret_manager_secret_iam_member" "cloud_run_access" {
  for_each = local.deploy_cloud_run ? {
    for secret in var.cloud_run_secrets : secret.secret_id => secret
  } : {}

  project   = var.project_id
  secret_id = each.value.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.erlmcp.email}"
}

# ============================================================================
# Custom Domain Mapping (optional)
# ============================================================================

resource "google_cloud_run_domain_mapping" "custom_domain" {
  count = local.deploy_cloud_run && var.cloud_run_custom_domain != "" ? 1 : 0

  location = var.region
  name     = var.cloud_run_custom_domain
  project  = var.project_id

  metadata {
    namespace = var.project_id
    labels    = local.common_labels
  }

  spec {
    route_name = google_cloud_run_v2_service.erlmcp[0].name
  }
}

# ============================================================================
# Cloud Run Outputs
# ============================================================================

output "cloud_run_url" {
  description = "Cloud Run service URL"
  value       = local.deploy_cloud_run ? google_cloud_run_v2_service.erlmcp[0].uri : null
}

output "cloud_run_service_name" {
  description = "Cloud Run service name"
  value       = local.deploy_cloud_run ? google_cloud_run_v2_service.erlmcp[0].name : null
}

output "vpc_connector_name" {
  description = "VPC connector name"
  value       = local.deploy_cloud_run && var.cloud_run_vpc_connector == "" ? google_vpc_access_connector.main[0].name : null
}
