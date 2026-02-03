# ============================================================================
# Cloud Run Deployment Example for erlmcp
# Serverless deployment with auto-scaling
# ============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.0.0"
    }
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
}

# ============================================================================
# Secret Manager Module
# ============================================================================
module "secrets" {
  source = "../../modules/secret-manager"

  project_id = var.project_id
  labels     = var.labels
  secret_accessors = [
    "serviceAccount:${module.cloud_run.service_account_email}",
  ]
}

# ============================================================================
# Cloud Run Module
# ============================================================================
module "cloud_run" {
  source = "../../modules/cloud-run"

  project_id = var.project_id
  region     = var.region

  service_name = var.service_name

  # Container image
  image_repository = var.image_repository
  image_name       = var.image_name
  image_tag        = var.image_tag

  # Resource configuration
  cpu         = var.cpu
  memory      = var.memory
  cpu_idle    = var.cpu_idle
  timeout     = var.timeout_seconds

  # Scaling
  min_instances = var.min_instances
  max_instances = var.max_instances

  # Health checks
  health_check_path = var.health_check_path

  # Networking
  ingress_setting      = var.ingress_setting
  allow_public_access  = var.allow_public_access
  invoker_service_account = var.invoker_service_account

  # Environment variables
  container_env = concat(var.container_env, [
    { name = "ERLMCP_ENV", value = var.environment },
    { name = "ERLMCP_VERSION", value = var.image_tag },
  ])

  # Secret references
  container_secrets = [
    { name = "ERLANG_COOKIE", secret_name = "erlmcp-erlang-cookie", secret_version = "latest" },
    { name = "DB_PASSWORD", secret_name = "erlmcp-db-password", secret_version = "latest" },
    { name = "REDIS_PASSWORD", secret_name = "erlmcp-redis-password", secret_version = "latest" },
  ]

  # Secret volumes
  secret_volumes = var.enable_tls ? [
    {
      name        = "erlmcp-tls"
      mount_path  = "/etc/tls"
      secret_name = "erlmcp-tls-cert"
      items       = [
        { key = "tls.crt", path = "cert.pem", mode = "0400" },
        { key = "tls.key", path = "key.pem", mode = "0400" }
      ]
    }
  ] : []

  labels = var.labels
}

# ============================================================================
# Observability Module
# ============================================================================
module "observability" {
  source = "../../modules/observability"

  project_id = var.project_id

  # Notification channels
  notification_channels = var.notification_channels

  # Uptime check
  create_uptime_check   = var.create_uptime_check
  uptime_check_path     = var.health_check_path
  uptime_check_port     = 8080
  uptime_check_use_ssl  = var.enable_tls
  uptime_check_host     = var.custom_domain != "" ? var.custom_domain : ""

  # SLOs
  create_slos = var.create_slos

  # Alert policies
  enable_error_rate_alert    = var.enable_error_rate_alert
  enable_latency_alert       = var.enable_latency_alert
  enable_memory_alert        = var.enable_memory_alert

  # Dashboards
  create_performance_dashboard = true
  create_erlang_dashboard    = true
  create_security_dashboard  = false
}

# ============================================================================
# Cloud Run Domain Mapping (optional)
# ============================================================================
resource "google_cloud_run_domain_mapping" "custom_domain" {
  count    = var.custom_domain != "" ? 1 : 0
  project  = var.project_id
  location = var.region
  name     = var.custom_domain

  metadata {
    namespace = "-"
  }

  spec {
    route_name = module.cloud_run.service_name
  }
}

# ============================================================================
# Outputs
# ============================================================================
output "service_name" {
  description = "Cloud Run service name"
  value       = module.cloud_run.service_name
}

output "service_url" {
  description = "Cloud Run service URL"
  value       = module.cloud_run.service_url
}

output "health_check_url" {
  description = "Health check URL"
  value       = module.cloud_run.health_check_url
}

output "custom_domain_url" {
  description = "Custom domain URL (if configured)"
  value       = var.custom_domain != "" ? "https://${var.custom_domain}" : null
}

output "container_image" {
  description = "Deployed container image"
  value       = module.cloud_run.container_image
}

output "service_account_email" {
  description = "Service account email"
  value       = module.cloud_run.service_account_email
}

output "secret_names" {
  description = "Created secret names"
  value = [
    "erlmcp-erlang-cookie",
    "erlmcp-db-password",
    "erlmcp-redis-password",
  ]
}
