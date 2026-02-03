# ==============================================================================
# ERLMCP Cloud Run Service
# ==============================================================================
# Serverless container deployment for erlmcp MCP server.
# Supports: HTTP/SSE transports, autoscaling, VPC connectivity.
#
# Features:
# - Automatic scaling (0 to max instances)
# - VPC connector for private networking
# - Secret Manager integration
# - Custom domain mapping
# - Health checks and traffic splitting
# ==============================================================================

# Cloud Run Service
resource "google_cloud_run_v2_service" "erlmcp" {
  name     = "${var.app_name}-service"
  location = var.region
  ingress  = var.cloud_run_ingress

  template {
    service_account = google_service_account.erlmcp_deploy.email

    scaling {
      min_instance_count = var.cloud_run_min_instances
      max_instance_count = var.cloud_run_max_instances
    }

    vpc_access {
      connector = var.vpc_connector_enabled ? google_vpc_access_connector.erlmcp[0].id : null
      egress    = var.vpc_connector_enabled ? "PRIVATE_RANGES_ONLY" : null
    }

    containers {
      image = "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp.repository_id}/erlmcp:${var.app_version}"

      ports {
        container_port = 8080
      }

      # Resource limits
      resources {
        limits = {
          cpu    = var.cloud_run_cpu
          memory = var.cloud_run_memory
        }
        cpu_idle          = true
        startup_cpu_boost = true
      }

      # Environment variables
      env {
        name  = "ERLMCP_PROFILE"
        value = var.environment
      }

      env {
        name  = "ERLMCP_PORT"
        value = "8080"
      }

      env {
        name  = "ERLMCP_METRICS_PORT"
        value = "9090"
      }

      env {
        name  = "GCP_PROJECT"
        value = var.project_id
      }

      env {
        name  = "GCP_REGION"
        value = var.region
      }

      # Database URL from Secret Manager
      env {
        name = "DATABASE_URL"
        value_source {
          secret_key_ref {
            secret  = google_secret_manager_secret.database_url.secret_id
            version = "latest"
          }
        }
      }

      # Erlang cookie from Secret Manager
      env {
        name = "ERLANG_COOKIE"
        value_source {
          secret_key_ref {
            secret  = google_secret_manager_secret.erlang_cookie.secret_id
            version = "latest"
          }
        }
      }

      # Startup probe
      startup_probe {
        http_get {
          path = "/health"
          port = 8080
        }
        initial_delay_seconds = 5
        period_seconds        = 5
        failure_threshold     = 30
        timeout_seconds       = 3
      }

      # Liveness probe
      liveness_probe {
        http_get {
          path = "/health"
          port = 8080
        }
        period_seconds    = 10
        failure_threshold = 3
        timeout_seconds   = 5
      }
    }

    # Execution environment
    execution_environment = "EXECUTION_ENVIRONMENT_GEN2"
    timeout               = "${var.cloud_run_timeout}s"

    labels = local.common_labels
  }

  traffic {
    type    = "TRAFFIC_TARGET_ALLOCATION_TYPE_LATEST"
    percent = 100
  }

  labels = local.common_labels

  depends_on = [
    google_project_service.required_apis,
    google_secret_manager_secret_version.database_url,
    google_secret_manager_secret_version.erlang_cookie,
  ]
}

# Erlang cookie secret
resource "google_secret_manager_secret" "erlang_cookie" {
  secret_id = "erlmcp-erlang-cookie"

  replication {
    auto {}
  }

  labels = local.common_labels

  depends_on = [google_project_service.required_apis]
}

# Erlang cookie secret version (auto-generated)
resource "google_secret_manager_secret_version" "erlang_cookie" {
  secret      = google_secret_manager_secret.erlang_cookie.id
  secret_data = var.erlang_cookie != "" ? var.erlang_cookie : random_password.erlang_cookie.result
}

# Random Erlang cookie if not provided
resource "random_password" "erlang_cookie" {
  length  = 32
  special = false
}

# Database URL secret version (placeholder - update manually or via CI)
resource "google_secret_manager_secret_version" "database_url" {
  secret      = google_secret_manager_secret.database_url.id
  secret_data = var.database_url != "" ? var.database_url : "postgresql://erlmcp:erlmcp@localhost:5432/erlmcp"

  lifecycle {
    ignore_changes = [secret_data]
  }
}

# IAM: Allow unauthenticated access (public API)
resource "google_cloud_run_v2_service_iam_member" "public_access" {
  count = var.cloud_run_allow_unauthenticated ? 1 : 0

  project  = var.project_id
  location = var.region
  name     = google_cloud_run_v2_service.erlmcp.name
  role     = "roles/run.invoker"
  member   = "allUsers"
}

# IAM: Allow specific service accounts
resource "google_cloud_run_v2_service_iam_member" "service_account_invoker" {
  for_each = toset(var.cloud_run_invoker_service_accounts)

  project  = var.project_id
  location = var.region
  name     = google_cloud_run_v2_service.erlmcp.name
  role     = "roles/run.invoker"
  member   = "serviceAccount:${each.value}"
}

# Custom domain mapping (optional)
resource "google_cloud_run_domain_mapping" "erlmcp" {
  count = var.cloud_run_custom_domain != "" ? 1 : 0

  location = var.region
  name     = var.cloud_run_custom_domain

  metadata {
    namespace = var.project_id
    labels    = local.common_labels
  }

  spec {
    route_name = google_cloud_run_v2_service.erlmcp.name
  }

  depends_on = [google_cloud_run_v2_service.erlmcp]
}

# ==============================================================================
# Cloud Run Jobs (for batch processing)
# ==============================================================================

resource "google_cloud_run_v2_job" "erlmcp_migration" {
  name     = "${var.app_name}-migration"
  location = var.region

  template {
    template {
      service_account = google_service_account.erlmcp_deploy.email

      vpc_access {
        connector = var.vpc_connector_enabled ? google_vpc_access_connector.erlmcp[0].id : null
        egress    = var.vpc_connector_enabled ? "PRIVATE_RANGES_ONLY" : null
      }

      containers {
        image = "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp.repository_id}/erlmcp:${var.app_version}"

        command = ["/opt/erlmcp/bin/erlmcp"]
        args    = ["migrate"]

        resources {
          limits = {
            cpu    = "1"
            memory = "512Mi"
          }
        }

        env {
          name = "DATABASE_URL"
          value_source {
            secret_key_ref {
              secret  = google_secret_manager_secret.database_url.secret_id
              version = "latest"
            }
          }
        }
      }

      timeout     = "600s"
      max_retries = 3
    }
  }

  labels = local.common_labels

  depends_on = [google_project_service.required_apis]
}
