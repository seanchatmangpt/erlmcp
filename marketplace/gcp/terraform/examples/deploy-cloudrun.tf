# ============================================================================
# erlmcp Cloud Run Gen2 Deployment Example
# Serverless deployment with VPC networking and enterprise features
# ============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.40.0"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.6"
    }
  }
}

# Configure the provider
provider "google" {
  project = var.project_id
  region  = var.region
}

# ============================================================================
# Variables
# ============================================================================

variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "region" {
  type        = string
  default     = "us-central1"
  description = "GCP region for Cloud Run deployment"
}

variable "service_name" {
  type        = string
  default     = "erlmcp"
  description = "Cloud Run service name"
}

variable "container_image" {
  type        = string
  description = "Container image (e.g., 'us-central1-docker.pkg.dev/PROJECT/erlmcp/erlmcp:3.0.0')"
}

variable "environment" {
  type        = string
  default     = "production"
  description = "Deployment environment"
}

variable "enable_vpc_networking" {
  type        = bool
  default     = false
  description = "Enable VPC networking with VPC connector"
}

variable "vpc_connector_name" {
  type        = string
  default     = ""
  description = "VPC Serverless Connector name (if enable_vpc_networking = true)"
}

variable "min_instances" {
  type        = number
  default     = 1
  description = "Minimum instances (0 = scale to zero, 1+ = warm start)"
}

variable "max_instances" {
  type        = number
  default     = 10
  description = "Maximum instances"
}

variable "cpu" {
  type        = string
  default     = "1"
  description = "CPU allocation (0.08, 1, 2, 4, 6, 8)"
}

variable "memory" {
  type        = string
  default     = "512Mi"
  description = "Memory allocation"
}

variable "allow_public_access" {
  type        = bool
  default     = false
  description = "Allow public unauthenticated access (WARNING: security risk)"
}

# ============================================================================
# Cloud Run Gen2 Module
# ============================================================================

module "erlmcp_cloudrun" {
  source = "../modules/cloud-run"

  # Project and region
  project_id = var.project_id
  region     = var.region

  # Service configuration
  service_name = var.service_name

  # Container image (parse from full path or construct)
  image_repository = split("/", var.container_image)[0]
  image_name       = join("/", slice(split("/", var.container_image), 2, length(split("/", var.container_image)) - 1))
  image_tag        = split(":", var.container_image)[1]

  # Resources (Gen2 features)
  cpu               = var.cpu
  memory            = var.memory
  cpu_idle          = true
  startup_cpu_boost = true
  timeout           = 300

  # Scaling
  min_instances                    = var.min_instances
  max_instances                    = var.max_instances
  max_instance_request_concurrency = 80

  # Networking (VPC connector for private resources)
  ingress_setting   = var.allow_public_access ? "INGRESS_TRAFFIC_ALL" : "INGRESS_TRAFFIC_INTERNAL_LOAD_BALANCER"
  vpc_connector_name = var.enable_vpc_networking ? var.vpc_connector_name : ""
  vpc_egress        = "PRIVATE_RANGES_ONLY"

  # Health checks
  health_check_path = "/health"

  # Startup probe (faster cold starts)
  startup_probe_initial_delay   = 0
  startup_probe_timeout         = 3
  startup_probe_period          = 10
  startup_probe_failure_threshold = 3

  # Liveness probe
  liveness_probe_initial_delay   = 30
  liveness_probe_timeout         = 5
  liveness_probe_period          = 10
  liveness_probe_failure_threshold = 3

  # Environment variables
  container_env = [
    { name = "ERLMCP_ENV", value = var.environment },
    { name = "ERLMCP_VERSION", value = split(":", var.container_image)[1] },
    { name = "ERL_AFLAGS", value = "-proto_dist inet_tls" },
    { name = "ERL_DIST_PORT", value = "9100" },
    { name = "ERLMCP_TRANSPORT", value = "stdio" },
    { name = "LOG_LEVEL", value = "info" },
    { name = "ENABLE_METRICS", value = "true" },
    { name = "ENABLE_TRACING", value = "true" }
  ]

  # Secret environment variables (from Secret Manager)
  container_secrets = [
    { name = "ERLANG_COOKIE", secret_name = "erlmcp-erlang-cookie", secret_version = "latest" },
    { name = "DB_PASSWORD", secret_name = "erlmcp-db-password", secret_version = "latest" },
    { name = "REDIS_PASSWORD", secret_name = "erlmcp-redis-password", secret_version = "latest" }
  ]

  # Secret volumes (TLS certificates)
  secret_volumes = []

  # IAM configuration (zero-trust by default)
  create_service_account = true
  service_account_roles = [
    "roles/secretmanager.secretAccessor",
    "roles/logging.logWriter",
    "roles/monitoring.metricWriter",
    "roles/cloudtrace.agent",
    "roles/cloudprofiler.agent"
  ]
  allow_public_access = var.allow_public_access

  # Security (Gen2 execution environment)
  execution_environment      = "EXECUTION_ENVIRONMENT_GEN2"
  binary_authorization_policy = ""
  encryption_key             = ""
  session_affinity           = false

  # Labels
  labels = {
    app          = "erlmcp"
    environment  = var.environment
    managed-by   = "terraform"
    deployment   = "cloud-run-gen2"
    version      = split(":", var.container_image)[1]
  }

  # Annotations
  annotations = {
    "marketplace.cloud.google.com/deployment-type" = "cloudrun-gen2"
  }
}

# ============================================================================
# Outputs
# ============================================================================

output "service_name" {
  description = "Cloud Run service name"
  value       = module.erlmcp_cloudrun.service_name
}

output "service_url" {
  description = "Cloud Run service URL"
  value       = module.erlmcp_cloudrun.service_url
}

output "service_id" {
  description = "Cloud Run service ID"
  value       = module.erlmcp_cloudrun.service_id
}

output "location" {
  description = "Service location"
  value       = module.erlmcp_cloudrun.location
}

output "service_account_email" {
  description = "Service account email"
  value       = module.erlmcp_cloudrun.service_account_email
}

output "health_check_url" {
  description = "Health check URL"
  value       = module.erlmcp_cloudrun.health_check_url
}

output "container_image" {
  description = "Deployed container image"
  value       = module.erlmcp_cloudrun.container_image
}

output "revision_name" {
  description = "Latest ready revision"
  value       = module.erlmcp_cloudrun.latest_ready_revision
}

output "execution_environment" {
  description = "Execution environment (Gen2)"
  value       = module.erlmcp_cloudrun.execution_environment
}

output "is_gen2" {
  description = "Using Gen2 execution environment"
  value       = module.erlmcp_cloudrun.is_gen2
}

output "deployment_metadata" {
  description = "Deployment metadata for CI/CD"
  value       = module.erlmcp_cloudrun.deployment_metadata
}

# ============================================================================
# Usage Instructions
# ============================================================================

# To deploy this configuration:
#
# 1. Initialize Terraform:
#    docker compose run erlmcp-build terraform -chdir=/workspace/marketplace/gcp/terraform/examples init
#
# 2. Plan the deployment:
#    docker compose run erlmcp-build terraform -chdir=/workspace/marketplace/gcp/terraform/examples plan \
#      -var="project_id=YOUR_PROJECT_ID" \
#      -var="container_image=us-central1-docker.pkg.dev/YOUR_PROJECT/erlmcp/erlmcp:3.0.0"
#
# 3. Apply the deployment:
#    docker compose run erlmcp-build terraform -chdir=/workspace/marketplace/gcp/terraform/examples apply \
#      -var="project_id=YOUR_PROJECT_ID" \
#      -var="container_image=us-central1-docker.pkg.dev/YOUR_PROJECT/erlmcp/erlmcp:3.0.0"
#
# 4. With VPC networking:
#    docker compose run erlmcp-build terraform -chdir=/workspace/marketplace/gcp/terraform/examples apply \
#      -var="project_id=YOUR_PROJECT_ID" \
#      -var="container_image=us-central1-docker.pkg.dev/YOUR_PROJECT/erlmcp/erlmcp:3.0.0" \
#      -var="enable_vpc_networking=true" \
#      -var="vpc_connector_name=erlmcp-vpc-connector"
#
# 5. Destroy the deployment:
#    docker compose run erlmcp-build terraform -chdir=/workspace/marketplace/gcp/terraform/examples destroy \
#      -var="project_id=YOUR_PROJECT_ID" \
#      -var="container_image=us-central1-docker.pkg.dev/YOUR_PROJECT/erlmcp/erlmcp:3.0.0"
