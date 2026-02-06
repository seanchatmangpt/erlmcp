# ============================================================================
# erlmcp Cloud Run Gen2 Full Deployment Example
# Complete serverless stack with secrets, observability, and domain mapping
# ============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.40.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = ">= 5.40.0"
    }
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
}

provider "google-beta" {
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

  # Grant access to the Cloud Run service account
  secret_accessors = [
    "serviceAccount:${module.cloud_run.service_account_email}",
  ]
}

# ============================================================================
# Cloud Run Gen2 Module
# ============================================================================

module "cloud_run" {
  source = "../../modules/cloud-run"

  # Project and location
  project_id = var.project_id
  region     = var.region

  # Service configuration
  service_name = var.service_name

  # Container image
  image_repository = var.image_repository
  image_name       = var.image_name
  image_tag        = var.image_tag
  container_port   = 8080

  # Resource configuration (Gen2 features)
  cpu               = var.cpu
  memory            = var.memory
  cpu_idle          = var.cpu_idle
  startup_cpu_boost = var.startup_cpu_boost
  timeout           = var.timeout_seconds

  # Scaling configuration
  min_instances                    = var.min_instances
  max_instances                    = var.max_instances
  max_instance_request_concurrency = var.max_concurrency

  # Networking (Gen2 VPC support)
  ingress_setting    = var.ingress_setting
  vpc_connector_name = var.vpc_connector_name
  vpc_egress         = var.vpc_egress
  network_interfaces = var.network_interfaces

  # Cloud SQL connections (if using Cloud SQL)
  cloud_sql_instances = var.cloud_sql_instances

  # Health checks
  health_check_path = var.health_check_path

  # Startup probe (faster cold starts with Gen2)
  startup_probe_initial_delay     = 0
  startup_probe_timeout           = 3
  startup_probe_period            = 10
  startup_probe_failure_threshold = 3

  # Liveness probe
  liveness_probe_initial_delay     = 30
  liveness_probe_timeout           = 5
  liveness_probe_period            = 10
  liveness_probe_failure_threshold = 3

  # Environment variables
  container_env = concat(
    var.container_env,
    [
      { name = "ERLMCP_ENV", value = var.environment },
      { name = "ERLMCP_VERSION", value = var.image_tag },
      { name = "ERL_AFLAGS", value = "-proto_dist inet_tls" },
      { name = "ERL_DIST_PORT", value = "9100" },
      { name = "ERLMCP_TRANSPORT", value = "stdio" },
      { name = "LOG_LEVEL", value = var.log_level },
      { name = "ENABLE_METRICS", value = tostring(var.enable_metrics) },
      { name = "ENABLE_TRACING", value = tostring(var.enable_tracing) }
    ]
  )

  # Secret references (from Secret Manager)
  container_secrets = [
    { name = "ERLANG_COOKIE", secret_name = "erlmcp-erlang-cookie", secret_version = "latest" },
    { name = "DB_PASSWORD", secret_name = "erlmcp-db-password", secret_version = "latest" },
    { name = "REDIS_PASSWORD", secret_name = "erlmcp-redis-password", secret_version = "latest" },
    { name = "JWT_SECRET", secret_name = "erlmcp-jwt-secret", secret_version = "latest" }
  ]

  # Secret volumes (TLS certificates if enabled)
  secret_volumes = var.enable_tls ? [
    {
      name        = "erlmcp-tls"
      mount_path  = "/etc/tls"
      secret_name = "erlmcp-tls-cert"
      items = [
        { key = "tls.crt", path = "cert.pem", mode = "0400" },
        { key = "tls.key", path = "key.pem", mode = "0400" },
        { key = "ca.crt", path = "ca.pem", mode = "0400" }
      ]
    }
  ] : []

  # IAM configuration
  create_service_account = true
  service_account_roles = [
    "roles/secretmanager.secretAccessor",
    "roles/logging.logWriter",
    "roles/monitoring.metricWriter",
    "roles/cloudtrace.agent",
    "roles/cloudprofiler.agent",
    "roles/cloudsql.client"
  ]
  allow_public_access      = var.allow_public_access
  invoker_service_accounts = var.invoker_service_accounts

  # Security (Gen2 execution environment)
  execution_environment       = "EXECUTION_ENVIRONMENT_GEN2"
  binary_authorization_policy = var.binary_authorization_policy
  encryption_key              = var.encryption_key
  session_affinity            = var.session_affinity

  # Domain mapping
  custom_domain         = var.custom_domain
  create_domain_mapping = var.custom_domain != ""

  # Artifact Registry
  create_repository = var.create_repository
  repository_id     = "erlmcp"

  # Labels
  labels = merge(var.labels, {
    app         = "erlmcp"
    environment = var.environment
    managed-by  = "terraform"
    deployment  = "cloud-run-gen2"
    version     = var.image_tag
  })

  # Annotations
  annotations = {
    "marketplace.cloud.google.com/deployment-type" = "cloudrun-gen2"
    "run.googleapis.com/description"               = "erlmcp v3 MCP SDK - Gen2 deployment"
  }
}

# ============================================================================
# Observability Module (optional)
# ============================================================================

module "observability" {
  count  = var.enable_observability ? 1 : 0
  source = "../../modules/observability"

  project_id = var.project_id

  # Notification channels
  notification_channels = var.notification_channels

  # Uptime check
  create_uptime_check  = var.create_uptime_check
  uptime_check_path    = var.health_check_path
  uptime_check_port    = 8080
  uptime_check_use_ssl = var.enable_tls
  uptime_check_host    = var.custom_domain != "" ? var.custom_domain : ""

  # SLOs
  create_slos = var.create_slos

  # Alert policies
  enable_error_rate_alert = var.enable_error_rate_alert
  enable_latency_alert    = var.enable_latency_alert
  enable_memory_alert     = var.enable_memory_alert

  # Dashboards
  create_performance_dashboard = true
  create_erlang_dashboard      = true
  create_security_dashboard    = false
}

# ============================================================================
# Cloud Run Domain Mapping (if custom domain specified)
# ============================================================================

resource "google_cloud_run_domain_mapping" "custom_domain" {
  count    = var.custom_domain != "" && var.create_domain_mapping ? 1 : 0
  project  = var.project_id
  location = var.region
  name     = var.custom_domain

  metadata {
    namespace = var.project_id
    labels    = var.labels
  }

  spec {
    route_name = module.cloud_run.service_name
  }

  depends_on = [module.cloud_run]
}

# ============================================================================
# Outputs
# ============================================================================

output "service_name" {
  description = "Cloud Run service name"
  value       = module.cloud_run.service_name
}

output "service_url" {
  description = "Cloud Run service URL (auto-generated)"
  value       = module.cloud_run.service_url
}

output "service_id" {
  description = "Cloud Run service ID"
  value       = module.cloud_run.service_id
}

output "health_check_url" {
  description = "Health check endpoint URL"
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

output "latest_ready_revision" {
  description = "Latest ready revision name"
  value       = module.cloud_run.latest_ready_revision
}

output "latest_created_revision" {
  description = "Latest created revision name"
  value       = module.cloud_run.latest_created_revision
}

output "execution_environment" {
  description = "Execution environment (GEN2)"
  value       = module.cloud_run.execution_environment
}

output "is_gen2" {
  description = "Using Gen2 execution environment"
  value       = module.cloud_run.is_gen2
}

output "vpc_connector" {
  description = "VPC connector (if configured)"
  value       = module.cloud_run.vpc_connector
}

output "repository_url" {
  description = "Artifact Registry repository URL (if created)"
  value       = module.cloud_run.repository_url
}

output "deployment_metadata" {
  description = "Deployment metadata for CI/CD and automation"
  value       = module.cloud_run.deployment_metadata
}

output "service_configuration" {
  description = "Complete service configuration for auditing"
  value       = module.cloud_run.service_configuration
}
