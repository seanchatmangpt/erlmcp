# ============================================================================
# GKE Deployment Example for erlmcp
# Complete GKE cluster with Helm chart deployment
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
# VPC Module
# ============================================================================
module "vpc" {
  source = "../../modules/vpc"

  project_id  = var.project_id
  region      = var.region
  network_name = var.network_name
  subnets     = var.subnets
}

# ============================================================================
# Secret Manager Module
# ============================================================================
module "secrets" {
  source = "../../modules/secret-manager"

  project_id = var.project_id
  labels     = var.labels
  secret_accessors = [
    "serviceAccount:${module.gke.service_account_email}",
  ]
}

# ============================================================================
# GKE Module
# ============================================================================
module "gke" {
  source = "../../modules/gke"

  project_id = var.project_id
  region     = var.region

  cluster_name = var.cluster_name
  network      = module.vpc.network_name
  subnet       = module.vpc.subnet_names[0]

  kubernetes_version = var.kubernetes_version
  release_channel    = var.release_channel

  # Private cluster configuration
  enable_private_endpoint = var.enable_private_endpoint
  enable_private_nodes    = var.enable_private_nodes
  master_ipv4_cidr_block  = var.master_ipv4_cidr_block
  master_global_access    = var.master_global_access

  # Node pool configuration
  node_pools = {
    primary = {
      version               = ""
      machine_type          = var.machine_type
      image_type            = "COS_CONTAINERD"
      disk_size_gb          = var.disk_size_gb
      disk_type             = var.disk_type
      min_count             = var.min_nodes
      max_count             = var.max_nodes
      location_policy       = "BALANCED"
      spot                  = false
      preemptible           = false
      enable_secure_boot    = true
      enable_integrity_monitoring = true
      enable_gvnic          = true
      pod_cidr              = ""
      taints                = []
      reservation_affinity  = ""
    }
    spot = var.enable_spot_nodes ? {
      version               = ""
      machine_type          = var.spot_machine_type
      image_type            = "COS_CONTAINERD"
      disk_size_gb          = var.spot_disk_size_gb
      disk_type             = "pd-standard"
      min_count             = var.spot_min_nodes
      max_count             = var.spot_max_nodes
      location_policy       = "ANY"
      spot                  = true
      preemptible           = true
      enable_secure_boot    = false
      enable_integrity_monitoring = false
      enable_gvnic          = false
      pod_cidr              = ""
      taints                = []
      reservation_affinity  = ""
    } : null
  }
  create_spot_node_pool = var.enable_spot_nodes

  # Workload Identity
  create_custom_node_pools = true

  # Network policy
  enable_network_policy = true
  network_policy_provider = "CALICO"

  # Shielded nodes
  enable_secure_boot          = var.enable_secure_boot
  enable_integrity_monitoring = var.enable_integrity_monitoring

  # Logging and monitoring
  enable_managed_prometheus = var.enable_managed_prometheus

  # Helm deployment
  deploy_helm_chart    = var.deploy_helm_chart
  helm_release_name    = var.helm_release_name
  helm_namespace       = var.helm_namespace
  image_repository     = var.image_repository
  image_name           = var.image_name
  image_tag            = var.image_tag

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
  create_uptime_check = var.create_uptime_check
  uptime_check_host   = var.uptime_check_host

  # SLOs
  create_slos = var.create_slos

  # Alert policies
  enable_error_rate_alert    = var.enable_error_rate_alert
  enable_latency_alert       = var.enable_latency_alert
  enable_memory_alert        = var.enable_memory_alert
  enable_cpu_alert           = var.enable_cpu_alert
  enable_health_check_alert  = var.enable_health_check_alert
  enable_process_count_alert = var.enable_process_count_alert

  # Dashboards
  create_performance_dashboard = true
  create_erlang_dashboard    = true
  create_security_dashboard  = true

  # Log export
  log_export_storage_enabled     = var.log_export_storage_enabled
  log_export_storage_destination = var.log_export_storage_destination
}

# ============================================================================
# Outputs
# ============================================================================
output "cluster_name" {
  description = "GKE cluster name"
  value       = module.gke.cluster_name
}

output "cluster_endpoint" {
  description = "GKE cluster endpoint"
  value       = module.gke.cluster_endpoint
  sensitive   = true
}

output "kubectl_config_command" {
  description = "Command to configure kubectl"
  value       = module.gke.kubectl_config_command
}

output "service_url" {
  description = "erlmcp service URL"
  value       = "http://${module.gke.cluster_endpoint}"
}

output "secret_names" {
  description = "Created secret names"
  value = [
    "erlmcp-erlang-cookie",
    "erlmcp-db-password",
    "erlmcp-redis-password",
    "erlmcp-tls-cert",
    "erlmcp-tls-key",
    "erlmcp-ca-bundle",
    "erlmcp-jwt-private-key",
    "erlmcp-jwt-public-key",
    "erlmcp-grafana-password",
    "erlmcp-backup-key",
    "erlmcp-otel-ca-cert"
  ]
}
