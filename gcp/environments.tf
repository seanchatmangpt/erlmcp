# Multi-Environment Configuration for ErlMCP
# Manages development, staging, and production environments
#
# This file defines environment-specific resource configurations.
# Use with: terraform apply -var-file=environments/dev.tfvars
#
# Note: Provider and terraform blocks are in main.tf

# ============================================================================
# Environment-specific Local Variables
# ============================================================================

locals {
  environment_config = {
    dev = {
      gke_cluster_name    = "erlmcp-dev-cluster"
      cloud_sql_name      = "erlmcp-dev"
      redis_memory_gb     = 1
      backup_frequency    = "WEEKLY"
      enable_failover     = false
      replica_regions     = []
      enable_dr           = false
      alert_threshold_err = 0.10  # 10% error rate
      alert_threshold_lat = 5000  # 5 second latency
      min_nodes           = 1
      max_nodes           = 5
      preemptible         = true
      deletion_protection = false
    }
    staging = {
      gke_cluster_name    = "erlmcp-staging-cluster"
      cloud_sql_name      = "erlmcp-staging"
      redis_memory_gb     = 4
      backup_frequency    = "DAILY"
      enable_failover     = true
      replica_regions     = ["us-east1"]
      enable_dr           = true
      alert_threshold_err = 0.05  # 5% error rate
      alert_threshold_lat = 2000  # 2 second latency
      min_nodes           = 2
      max_nodes           = 10
      preemptible         = true
      deletion_protection = false
    }
    production = {
      gke_cluster_name    = "erlmcp-prod-cluster"
      cloud_sql_name      = "erlmcp-prod"
      redis_memory_gb     = 16
      backup_frequency    = "HOURLY"
      enable_failover     = true
      replica_regions     = ["us-east1"]
      enable_dr           = true
      alert_threshold_err = 0.001  # 0.1% error rate (nine-nines target)
      alert_threshold_lat = 500    # 500ms latency
      min_nodes           = 3
      max_nodes           = 20
      preemptible         = false
      deletion_protection = true
    }
  }

  # Select environment config based on var.environment
  env = lookup(local.environment_config, var.environment, local.environment_config["dev"])

  common_labels = {
    environment = var.environment
    managed_by  = "terraform"
    application = "erlmcp"
    version     = "3.0.0"
  }

  # CIDR ranges for environments
  cidr_ranges = {
    dev        = "10.0.0.0/16"
    staging    = "10.10.0.0/16"
    production = "10.20.0.0/16"
  }

  # Pod and service CIDR ranges for GKE
  pod_cidr_ranges = {
    dev        = "10.100.0.0/16"
    staging    = "10.110.0.0/16"
    production = "10.120.0.0/16"
  }

  service_cidr_ranges = {
    dev        = "10.200.0.0/20"
    staging    = "10.210.0.0/20"
    production = "10.220.0.0/20"
  }
}

# ============================================================================
# VPC Network and Subnets (Environment-specific)
# ============================================================================

resource "google_compute_network" "vpc" {
  name                    = "erlmcp-${var.environment}-vpc"
  auto_create_subnetworks = false
  description             = "VPC for ErlMCP ${var.environment} environment"

  depends_on = [google_project_service.required_apis]
}

resource "google_compute_subnetwork" "subnet" {
  name          = "erlmcp-${var.environment}-subnet"
  ip_cidr_range = lookup(local.cidr_ranges, var.environment, local.cidr_ranges["dev"])
  region        = var.region
  network       = google_compute_network.vpc.id

  private_ip_google_access = true

  # Secondary IP ranges for GKE pods and services
  secondary_ip_range {
    range_name    = "pods"
    ip_cidr_range = lookup(local.pod_cidr_ranges, var.environment, local.pod_cidr_ranges["dev"])
  }

  secondary_ip_range {
    range_name    = "services"
    ip_cidr_range = lookup(local.service_cidr_ranges, var.environment, local.service_cidr_ranges["dev"])
  }

  log_config {
    aggregation_interval = "INTERVAL_5_SEC"
    flow_sampling        = 0.5
    metadata             = "INCLUDE_ALL_METADATA"
  }

  depends_on = [google_compute_network.vpc]
}

# ============================================================================
# Cloud Router and NAT (for private GKE nodes)
# ============================================================================

resource "google_compute_router" "router" {
  name    = "erlmcp-${var.environment}-router"
  region  = var.region
  network = google_compute_network.vpc.id

  depends_on = [google_compute_network.vpc]
}

resource "google_compute_router_nat" "nat" {
  name                               = "erlmcp-${var.environment}-nat"
  router                             = google_compute_router.router.name
  region                             = google_compute_router.router.region
  nat_ip_allocate_option             = "AUTO_ONLY"
  source_subnetwork_ip_ranges_to_nat = "ALL_SUBNETWORKS_ALL_IP_RANGES"

  log_config {
    enable = true
    filter = var.environment == "production" ? "ERRORS_ONLY" : "ALL"
  }

  depends_on = [google_compute_router.router]
}

# ============================================================================
# Cloud Memorystore (Redis) - Environment-specific
# ============================================================================

resource "google_redis_instance" "cache" {
  name           = "erlmcp-${var.environment}-cache"
  memory_size_gb = local.env.redis_memory_gb
  tier           = var.environment == "production" ? "STANDARD_HA" : "BASIC"
  region         = var.region
  redis_version  = "REDIS_7_0"

  authorized_network = google_compute_network.vpc.id

  redis_configs = {
    "maxmemory-policy" = var.environment == "production" ? "allkeys-lru" : "noeviction"
  }

  labels = local.common_labels

  depends_on = [google_project_service.required_apis]
}

# ============================================================================
# Environment-specific Outputs
# ============================================================================

output "vpc_network_name" {
  description = "VPC network name"
  value       = google_compute_network.vpc.name
}

output "vpc_network_id" {
  description = "VPC network ID"
  value       = google_compute_network.vpc.id
}

output "subnet_name" {
  description = "Subnet name"
  value       = google_compute_subnetwork.subnet.name
}

output "subnet_id" {
  description = "Subnet ID"
  value       = google_compute_subnetwork.subnet.id
}

output "redis_host" {
  description = "Redis instance host"
  value       = google_redis_instance.cache.host
}

output "redis_port" {
  description = "Redis instance port"
  value       = google_redis_instance.cache.port
}

output "environment_config" {
  description = "Environment-specific configuration"
  value       = local.env
}
