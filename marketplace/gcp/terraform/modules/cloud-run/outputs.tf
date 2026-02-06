# ============================================================================
# Cloud Run v2 Module Outputs
# Gen2 Execution Environment with Enterprise Features
# ============================================================================

output "service_name" {
  description = "Cloud Run v2 service name (with random suffix)"
  value       = google_cloud_run_v2_service.erlmcp.name
}

output "service_id" {
  description = "Cloud Run v2 service fully qualified ID"
  value       = google_cloud_run_v2_service.erlmcp.id
}

output "service_url" {
  description = "Cloud Run service URL (auto-generated .run.app domain)"
  value       = google_cloud_run_v2_service.erlmcp.uri
}

output "location" {
  description = "Service deployment region"
  value       = google_cloud_run_v2_service.erlmcp.location
}

output "project_id" {
  description = "GCP project ID"
  value       = google_cloud_run_v2_service.erlmcp.project
}

# ============================================================================
# Revision Information
# ============================================================================

output "latest_ready_revision" {
  description = "Latest ready revision name (ready to serve traffic)"
  value       = google_cloud_run_v2_service.erlmcp.latest_ready_revision
}

output "latest_created_revision" {
  description = "Latest created revision name (may not be ready yet)"
  value       = google_cloud_run_v2_service.erlmcp.latest_created_revision
}

output "revision_name" {
  description = "Active revision name (alias for latest_ready_revision)"
  value       = google_cloud_run_v2_service.erlmcp.latest_ready_revision
}

# ============================================================================
# IAM and Service Account
# ============================================================================

output "service_account_email" {
  description = "Service account email used by the Cloud Run service"
  value       = var.create_service_account ? google_service_account.erlmcp[0].email : var.service_account_email
}

output "service_account_name" {
  description = "Service account name"
  value       = var.create_service_account ? google_service_account.erlmcp[0].name : null
}

output "service_account_unique_id" {
  description = "Service account unique ID"
  value       = var.create_service_account ? google_service_account.erlmcp[0].unique_id : null
}

# ============================================================================
# Container Image Information
# ============================================================================

output "container_image" {
  description = "Full container image URL deployed to Cloud Run"
  value       = "${var.image_repository}/${var.project_id}/${var.image_name}:${var.image_tag}"
}

output "image_repository" {
  description = "Container image repository"
  value       = var.image_repository
}

output "image_name" {
  description = "Container image name"
  value       = var.image_name
}

output "image_tag" {
  description = "Container image tag"
  value       = var.image_tag
}

# ============================================================================
# Artifact Registry (if created)
# ============================================================================

output "repository_id" {
  description = "Artifact Registry repository ID (if created)"
  value       = var.create_repository ? google_artifact_registry_repository.erlmcp[0].repository_id : null
}

output "repository_url" {
  description = "Artifact Registry repository URL (if created)"
  value = var.create_repository ? format(
    "%s-docker.pkg.dev/%s/%s",
    var.region,
    var.project_id,
    google_artifact_registry_repository.erlmcp[0].repository_id
  ) : null
}

output "repository_name" {
  description = "Artifact Registry repository full name (if created)"
  value       = var.create_repository ? google_artifact_registry_repository.erlmcp[0].name : null
}

# ============================================================================
# Health Check and Monitoring
# ============================================================================

output "health_check_url" {
  description = "Full health check endpoint URL"
  value       = "${google_cloud_run_v2_service.erlmcp.uri}${var.health_check_path}"
}

output "health_check_path" {
  description = "Health check endpoint path"
  value       = var.health_check_path
}

# ============================================================================
# Networking Information
# ============================================================================

output "vpc_connector" {
  description = "VPC connector path (if configured)"
  value       = var.vpc_connector_name != "" ? (startswith(var.vpc_connector_name, "projects/") ? var.vpc_connector_name : "projects/${var.project_id}/locations/${var.region}/connectors/${var.vpc_connector_name}") : null
}

output "ingress_setting" {
  description = "Ingress traffic setting"
  value       = var.ingress_setting
}

output "vpc_egress" {
  description = "VPC egress setting"
  value       = var.vpc_egress
}

# ============================================================================
# Execution Environment
# ============================================================================

output "execution_environment" {
  description = "Execution environment (GEN1 or GEN2)"
  value       = var.execution_environment
}

output "is_gen2" {
  description = "Boolean flag indicating if using Gen2 execution environment"
  value       = var.execution_environment == "EXECUTION_ENVIRONMENT_GEN2"
}

# ============================================================================
# Scaling Configuration
# ============================================================================

output "min_instances" {
  description = "Minimum number of instances"
  value       = var.min_instances
}

output "max_instances" {
  description = "Maximum number of instances"
  value       = var.max_instances
}

output "max_concurrency" {
  description = "Maximum concurrent requests per instance"
  value       = var.max_instance_request_concurrency
}

# ============================================================================
# Resource Configuration
# ============================================================================

output "cpu_allocation" {
  description = "CPU allocation"
  value       = var.cpu
}

output "memory_allocation" {
  description = "Memory allocation"
  value       = var.memory
}

output "cpu_idle" {
  description = "CPU idle throttling enabled"
  value       = var.cpu_idle
}

output "startup_cpu_boost" {
  description = "Startup CPU boost enabled (Gen2 feature)"
  value       = var.startup_cpu_boost
}

# ============================================================================
# Security Configuration
# ============================================================================

output "allow_public_access" {
  description = "Whether public unauthenticated access is allowed"
  value       = var.allow_public_access
}

output "binary_authorization_enabled" {
  description = "Whether binary authorization is enabled"
  value       = var.binary_authorization_policy != ""
}

output "encryption_key" {
  description = "Customer-managed encryption key (if configured)"
  value       = var.encryption_key != "" ? var.encryption_key : null
  sensitive   = true
}

output "session_affinity_enabled" {
  description = "Whether session affinity is enabled"
  value       = var.session_affinity
}

# ============================================================================
# Domain Configuration
# ============================================================================

output "custom_domain" {
  description = "Custom domain name (if configured)"
  value       = var.custom_domain != "" ? var.custom_domain : null
}

output "custom_domain_url" {
  description = "Custom domain URL (if configured)"
  value       = var.custom_domain != "" ? "https://${var.custom_domain}" : null
}

# ============================================================================
# Labels and Metadata
# ============================================================================

output "labels" {
  description = "Resource labels"
  value       = var.labels
}

output "service_labels" {
  description = "Service labels as assigned"
  value       = google_cloud_run_v2_service.erlmcp.labels
}

# ============================================================================
# Status Information
# ============================================================================

output "service_status" {
  description = "Service status and conditions"
  value = {
    latest_ready_revision   = google_cloud_run_v2_service.erlmcp.latest_ready_revision
    latest_created_revision = google_cloud_run_v2_service.erlmcp.latest_created_revision
    uri                     = google_cloud_run_v2_service.erlmcp.uri
    observed_generation     = google_cloud_run_v2_service.erlmcp.observed_generation
    terminal_condition      = google_cloud_run_v2_service.erlmcp.terminal_condition
  }
}

# ============================================================================
# Deployment Metadata (for automation and CI/CD)
# ============================================================================

output "deployment_metadata" {
  description = "Deployment metadata for CI/CD and automation"
  value = {
    service_name            = google_cloud_run_v2_service.erlmcp.name
    project_id              = var.project_id
    region                  = var.region
    image                   = "${var.image_repository}/${var.project_id}/${var.image_name}:${var.image_tag}"
    service_url             = google_cloud_run_v2_service.erlmcp.uri
    health_check_url        = "${google_cloud_run_v2_service.erlmcp.uri}${var.health_check_path}"
    revision                = google_cloud_run_v2_service.erlmcp.latest_ready_revision
    execution_environment   = var.execution_environment
    min_instances           = var.min_instances
    max_instances           = var.max_instances
    service_account         = var.create_service_account ? google_service_account.erlmcp[0].email : var.service_account_email
  }
}

# ============================================================================
# Complete Service Configuration (for debugging and auditing)
# ============================================================================

output "service_configuration" {
  description = "Complete service configuration for auditing"
  value = {
    # Service identity
    name     = google_cloud_run_v2_service.erlmcp.name
    id       = google_cloud_run_v2_service.erlmcp.id
    project  = var.project_id
    location = var.region

    # Container
    image      = "${var.image_repository}/${var.project_id}/${var.image_name}:${var.image_tag}"
    port       = var.container_port

    # Resources
    cpu                = var.cpu
    memory             = var.memory
    cpu_idle           = var.cpu_idle
    startup_cpu_boost  = var.startup_cpu_boost
    timeout            = var.timeout

    # Scaling
    min_instances      = var.min_instances
    max_instances      = var.max_instances
    max_concurrency    = var.max_instance_request_concurrency

    # Networking
    ingress            = var.ingress_setting
    vpc_connector      = var.vpc_connector_name
    vpc_egress         = var.vpc_egress

    # Security
    execution_env      = var.execution_environment
    session_affinity   = var.session_affinity
    public_access      = var.allow_public_access

    # Observability
    health_check       = var.health_check_path

    # Metadata
    labels             = var.labels
  }
}
