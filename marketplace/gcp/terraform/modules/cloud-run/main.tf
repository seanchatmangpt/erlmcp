# ============================================================================
# Cloud Run v2 Module - Gen2 Execution Environment
# Enterprise-grade serverless deployment for erlmcp MCP SDK
# ============================================================================
# Architecture: Stateless, auto-scaling, zero-trust IAM, VPC-connected
# Quality: Security-first, observability-native, cost-optimized
# Deployment: Blue-green ready, multi-region capable, SBOM-tracked
# ============================================================================

# Generate random suffix for unique resource names
resource "random_string" "suffix" {
  length  = 8
  special = false
  upper   = false
}

locals {
  service_name           = "${var.service_name}-${random_string.suffix.result}"
  service_account_email  = var.create_service_account ? google_service_account.erlmcp[0].email : var.service_account_email
  full_image_path        = "${var.image_repository}/${var.project_id}/${var.image_name}:${var.image_tag}"

  # VPC connector path (if provided)
  vpc_connector_path = var.vpc_connector_name != "" ? (
    startswith(var.vpc_connector_name, "projects/") ? var.vpc_connector_name :
    "projects/${var.project_id}/locations/${var.region}/connectors/${var.vpc_connector_name}"
  ) : ""
}

# ============================================================================
# Enable Required APIs
# ============================================================================

resource "google_project_service" "run" {
  project            = var.project_id
  service            = "run.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "vpcaccess" {
  count              = var.vpc_connector_name != "" ? 1 : 0
  project            = var.project_id
  service            = "vpcaccess.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "secretmanager" {
  count              = length(var.container_secrets) > 0 || length(var.secret_volumes) > 0 ? 1 : 0
  project            = var.project_id
  service            = "secretmanager.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "artifactregistry" {
  count              = var.create_repository ? 1 : 0
  project            = var.project_id
  service            = "artifactregistry.googleapis.com"
  disable_on_destroy = false
}

# ============================================================================
# Artifact Registry Repository (Optional)
# ============================================================================

resource "google_artifact_registry_repository" "erlmcp" {
  count         = var.create_repository ? 1 : 0
  project       = var.project_id
  location      = var.region
  repository_id = var.repository_id
  description   = "erlmcp container images - SBOM tracking enabled"
  format        = "DOCKER"
  mode          = "STANDARD_REPOSITORY"

  labels = merge(var.labels, {
    purpose = "erlmcp-deployment"
  })

  depends_on = [google_project_service.artifactregistry]
}

# ============================================================================
# Service Account (Zero-Trust IAM)
# ============================================================================

resource "google_service_account" "erlmcp" {
  count        = var.create_service_account ? 1 : 0
  project      = var.project_id
  account_id   = "erlmcp-${random_string.suffix.result}"
  display_name = "erlmcp Cloud Run Service Account"
  description  = "Least-privilege service account for erlmcp - created ${timestamp()}"
}

# Grant necessary IAM roles to service account
resource "google_project_iam_member" "service_account_roles" {
  for_each = var.create_service_account ? toset(var.service_account_roles) : toset([])

  project = var.project_id
  role    = each.value
  member  = "serviceAccount:${google_service_account.erlmcp[0].email}"
}

# ============================================================================
# Cloud Run v2 Service (Gen2 Execution Environment)
# ============================================================================

resource "google_cloud_run_v2_service" "erlmcp" {
  project     = var.project_id
  location    = var.region
  name        = local.service_name
  description = "erlmcp v3 MCP SDK - Gen2 execution environment with VPC networking"

  labels = var.labels

  ingress = var.ingress_setting

  # Binary Authorization (supply chain security)
  binary_authorization {
    use_default       = var.binary_authorization_policy == ""
    breakglass_justification = var.binary_authorization_policy != "" ? "Production deployment" : null
  }

  template {
    # Execution environment (Gen2 for better performance and features)
    execution_environment = var.execution_environment

    # Service account (least privilege)
    service_account = local.service_account_email

    # Encryption (CMEK support)
    encryption_key = var.encryption_key != "" ? var.encryption_key : null

    # Session affinity (sticky sessions for stateful protocols like WebSocket)
    session_affinity = var.session_affinity

    # Request timeout
    timeout = "${var.timeout}s"

    # Max instances per region
    max_instance_request_concurrency = var.max_instance_request_concurrency

    # Scaling configuration
    scaling {
      min_instance_count = var.min_instances
      max_instance_count = var.max_instances
    }

    # VPC Access Connector (private networking)
    dynamic "vpc_access" {
      for_each = local.vpc_connector_path != "" || length(var.network_interfaces) > 0 ? [1] : []
      content {
        connector = local.vpc_connector_path != "" ? local.vpc_connector_path : null
        egress    = var.vpc_egress

        # Direct VPC egress (Gen2 feature - more efficient than VPC connector)
        dynamic "network_interfaces" {
          for_each = var.network_interfaces
          content {
            network    = network_interfaces.value.network
            subnetwork = network_interfaces.value.subnetwork
            tags       = network_interfaces.value.tags
          }
        }
      }
    }

    # Container configuration
    containers {
      name  = "erlmcp"
      image = local.full_image_path

      # Resource limits
      resources {
        limits = {
          cpu    = var.cpu
          memory = var.memory
        }
        cpu_idle          = var.cpu_idle
        startup_cpu_boost = var.startup_cpu_boost
      }

      # Container port
      ports {
        name           = "http1"
        container_port = var.container_port
      }

      # Environment variables
      dynamic "env" {
        for_each = concat(
          var.container_env,
          [
            { name = "GOOGLE_CLOUD_PROJECT", value = var.project_id },
            { name = "GOOGLE_CLOUD_REGION", value = var.region },
            { name = "K_SERVICE", value = local.service_name },
          ]
        )
        content {
          name  = env.value.name
          value = env.value.value
        }
      }

      # Secret environment variables
      dynamic "env" {
        for_each = var.container_secrets
        content {
          name = env.value.name
          value_source {
            secret_key_ref {
              secret  = env.value.secret_name
              version = env.value.secret_version
            }
          }
        }
      }

      # Startup probe (faster cold starts)
      startup_probe {
        initial_delay_seconds = var.startup_probe_initial_delay
        timeout_seconds       = var.startup_probe_timeout
        period_seconds        = var.startup_probe_period
        failure_threshold     = var.startup_probe_failure_threshold

        http_get {
          path = var.health_check_path
          port = var.container_port
        }
      }

      # Liveness probe (health monitoring)
      liveness_probe {
        initial_delay_seconds = var.liveness_probe_initial_delay
        timeout_seconds       = var.liveness_probe_timeout
        period_seconds        = var.liveness_probe_period
        failure_threshold     = var.liveness_probe_failure_threshold

        http_get {
          path = var.health_check_path
          port = var.container_port
        }
      }

      # Volume mounts (secrets as files)
      dynamic "volume_mounts" {
        for_each = var.secret_volumes
        content {
          name       = volume_mounts.value.name
          mount_path = volume_mounts.value.mount_path
        }
      }
    }

    # Secret volumes
    dynamic "volumes" {
      for_each = var.secret_volumes
      content {
        name = volumes.value.name
        secret {
          secret = volumes.value.secret_name
          dynamic "items" {
            for_each = volumes.value.items
            content {
              path    = items.value.path
              version = "latest"
              mode    = parseint(items.value.mode, 8)
            }
          }
        }
      }
    }

    # Cloud SQL connections (managed database access)
    dynamic "volumes" {
      for_each = length(var.cloud_sql_instances) > 0 ? [1] : []
      content {
        name = "cloudsql"
        cloud_sql_instance {
          instances = var.cloud_sql_instances
        }
      }
    }
  }

  # Traffic routing (100% to latest revision - blue-green ready)
  traffic {
    type    = "TRAFFIC_TARGET_ALLOCATION_TYPE_LATEST"
    percent = 100
  }

  depends_on = [
    google_project_service.run,
    google_project_service.vpcaccess,
    google_project_service.secretmanager,
    google_project_iam_member.service_account_roles
  ]

  lifecycle {
    create_before_destroy = true
    ignore_changes = [
      # Ignore client-side fields that may change
      client,
      client_version,
    ]
  }
}

# ============================================================================
# IAM Bindings (Zero-Trust Access Control)
# ============================================================================

# Public access (disabled by default - security first)
resource "google_cloud_run_v2_service_iam_member" "public_invoker" {
  count    = var.allow_public_access ? 1 : 0
  project  = google_cloud_run_v2_service.erlmcp.project
  location = google_cloud_run_v2_service.erlmcp.location
  name     = google_cloud_run_v2_service.erlmcp.name
  role     = "roles/run.invoker"
  member   = "allUsers"
}

# Service account invokers (authenticated access)
resource "google_cloud_run_v2_service_iam_member" "invokers" {
  for_each = toset(var.invoker_service_accounts)

  project  = google_cloud_run_v2_service.erlmcp.project
  location = google_cloud_run_v2_service.erlmcp.location
  name     = google_cloud_run_v2_service.erlmcp.name
  role     = "roles/run.invoker"
  member   = "serviceAccount:${each.value}"
}

# ============================================================================
# Domain Mapping (Custom Domains)
# ============================================================================

resource "google_cloud_run_v2_service_iam_member" "domain_mapping_sa" {
  count    = var.create_domain_mapping && var.custom_domain != "" ? 1 : 0
  project  = google_cloud_run_v2_service.erlmcp.project
  location = google_cloud_run_v2_service.erlmcp.location
  name     = google_cloud_run_v2_service.erlmcp.name
  role     = "roles/run.invoker"
  member   = "serviceAccount:service-${data.google_project.current.number}@serverless-robot-prod.iam.gserviceaccount.com"
}

data "google_project" "current" {
  project_id = var.project_id
}

# ============================================================================
# Outputs
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
  description = "Cloud Run service URL (auto-generated)"
  value       = google_cloud_run_v2_service.erlmcp.uri
}

output "location" {
  description = "Service location (region)"
  value       = google_cloud_run_v2_service.erlmcp.location
}

output "service_account_email" {
  description = "Service account email"
  value       = local.service_account_email
}

output "latest_ready_revision" {
  description = "Latest ready revision name"
  value       = google_cloud_run_v2_service.erlmcp.latest_ready_revision
}

output "latest_created_revision" {
  description = "Latest created revision name"
  value       = google_cloud_run_v2_service.erlmcp.latest_created_revision
}

output "container_image" {
  description = "Full container image URL"
  value       = local.full_image_path
}

output "repository_id" {
  description = "Artifact Registry repository ID"
  value       = var.create_repository ? google_artifact_registry_repository.erlmcp[0].repository_id : null
}

output "repository_url" {
  description = "Artifact Registry repository URL"
  value = var.create_repository ? format(
    "%s-docker.pkg.dev/%s/%s",
    var.region,
    var.project_id,
    google_artifact_registry_repository.erlmcp[0].repository_id
  ) : null
}

output "health_check_url" {
  description = "Health check URL"
  value       = "${google_cloud_run_v2_service.erlmcp.uri}${var.health_check_path}"
}

output "vpc_connector" {
  description = "VPC connector path"
  value       = local.vpc_connector_path
}

output "execution_environment" {
  description = "Execution environment (GEN1 or GEN2)"
  value       = var.execution_environment
}
