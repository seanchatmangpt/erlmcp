# ============================================================================
# erlmcp GCP AI Marketplace - Main Terraform Configuration
# ============================================================================
# Multi-deployment architecture supporting:
#   - Compute Engine (VM-based with Packer images)
#   - Cloud Run (Serverless containers)
#   - GKE (Kubernetes clusters)
#
# DOCKER-ONLY CONSTITUTION: All artifacts built via Docker quality gates
# ============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.0.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = ">= 5.0.0"
    }
  }

  backend "gcs" {
    # Configure via backend config file or CLI flags
    # bucket = "erlmcp-terraform-state"
    # prefix = "marketplace/production"
  }
}

# ============================================================================
# Provider Configuration
# ============================================================================

provider "google" {
  project = var.project_id
  region  = var.region
}

provider "google-beta" {
  project = var.project_id
  region  = var.region
}

# ============================================================================
# Local Values
# ============================================================================

locals {
  name_prefix = "erlmcp-${var.environment}"

  common_labels = {
    "app"                  = "erlmcp"
    "environment"          = var.environment
    "managed-by"           = "terraform"
    "marketplace-solution" = "erlmcp-mcp-sdk"
    "version"              = var.erlmcp_version
  }

  # Deployment type flags
  deploy_gce      = contains(var.deployment_types, "gce")
  deploy_cloud_run = contains(var.deployment_types, "cloud_run")
  deploy_gke      = contains(var.deployment_types, "gke")
}

# ============================================================================
# Enable Required APIs
# ============================================================================

resource "google_project_service" "required_apis" {
  for_each = toset([
    "compute.googleapis.com",
    "container.googleapis.com",
    "run.googleapis.com",
    "secretmanager.googleapis.com",
    "cloudresourcemanager.googleapis.com",
    "iam.googleapis.com",
    "logging.googleapis.com",
    "monitoring.googleapis.com",
    "cloudtrace.googleapis.com",
    "clouderrorreporting.googleapis.com",
    "servicenetworking.googleapis.com",
    "sqladmin.googleapis.com",
    "redis.googleapis.com",
    "artifactregistry.googleapis.com",
  ])

  project            = var.project_id
  service            = each.value
  disable_on_destroy = false
}

# ============================================================================
# VPC Network
# ============================================================================

resource "google_compute_network" "main" {
  name                    = "${local.name_prefix}-vpc"
  project                 = var.project_id
  auto_create_subnetworks = false
  routing_mode            = "REGIONAL"

  depends_on = [google_project_service.required_apis]
}

resource "google_compute_subnetwork" "main" {
  name          = "${local.name_prefix}-subnet"
  project       = var.project_id
  region        = var.region
  network       = google_compute_network.main.id
  ip_cidr_range = var.subnet_cidr

  private_ip_google_access = true

  log_config {
    aggregation_interval = "INTERVAL_5_SEC"
    flow_sampling        = 0.5
    metadata             = "INCLUDE_ALL_METADATA"
  }

  # Secondary ranges for GKE
  dynamic "secondary_ip_range" {
    for_each = local.deploy_gke ? [1] : []
    content {
      range_name    = "gke-pods"
      ip_cidr_range = var.gke_pods_cidr
    }
  }

  dynamic "secondary_ip_range" {
    for_each = local.deploy_gke ? [1] : []
    content {
      range_name    = "gke-services"
      ip_cidr_range = var.gke_services_cidr
    }
  }
}

# ============================================================================
# Cloud NAT (for private instances)
# ============================================================================

resource "google_compute_router" "main" {
  name    = "${local.name_prefix}-router"
  project = var.project_id
  region  = var.region
  network = google_compute_network.main.id
}

resource "google_compute_router_nat" "main" {
  name                               = "${local.name_prefix}-nat"
  project                            = var.project_id
  region                             = var.region
  router                             = google_compute_router.main.name
  nat_ip_allocate_option             = "AUTO_ONLY"
  source_subnetwork_ip_ranges_to_nat = "ALL_SUBNETWORKS_ALL_IP_RANGES"

  log_config {
    enable = true
    filter = "ERRORS_ONLY"
  }
}

# ============================================================================
# Firewall Rules
# ============================================================================

resource "google_compute_firewall" "allow_health_check" {
  name    = "${local.name_prefix}-allow-health-check"
  project = var.project_id
  network = google_compute_network.main.name

  allow {
    protocol = "tcp"
    ports    = ["8080", "9568"]
  }

  # GCP health check ranges
  source_ranges = [
    "130.211.0.0/22",
    "35.191.0.0/16"
  ]

  target_tags = ["erlmcp"]
}

resource "google_compute_firewall" "allow_internal" {
  name    = "${local.name_prefix}-allow-internal"
  project = var.project_id
  network = google_compute_network.main.name

  allow {
    protocol = "tcp"
    ports    = ["0-65535"]
  }
  allow {
    protocol = "udp"
    ports    = ["0-65535"]
  }
  allow {
    protocol = "icmp"
  }

  source_ranges = [var.subnet_cidr]
}

resource "google_compute_firewall" "allow_erlang_dist" {
  name    = "${local.name_prefix}-allow-erlang-dist"
  project = var.project_id
  network = google_compute_network.main.name

  allow {
    protocol = "tcp"
    ports    = ["9100-9200"]  # Erlang distribution ports
  }

  source_tags = ["erlmcp"]
  target_tags = ["erlmcp"]
}

resource "google_compute_firewall" "allow_iap" {
  count   = var.enable_iap ? 1 : 0
  name    = "${local.name_prefix}-allow-iap"
  project = var.project_id
  network = google_compute_network.main.name

  allow {
    protocol = "tcp"
    ports    = ["22"]
  }

  # IAP's IP range
  source_ranges = ["35.235.240.0/20"]
  target_tags   = ["erlmcp"]
}

# ============================================================================
# Service Account
# ============================================================================

resource "google_service_account" "erlmcp" {
  account_id   = "${local.name_prefix}-sa"
  display_name = "erlmcp Service Account"
  project      = var.project_id
}

resource "google_project_iam_member" "erlmcp_roles" {
  for_each = toset([
    "roles/logging.logWriter",
    "roles/monitoring.metricWriter",
    "roles/cloudtrace.agent",
    "roles/secretmanager.secretAccessor",
    "roles/storage.objectViewer",
  ])

  project = var.project_id
  role    = each.value
  member  = "serviceAccount:${google_service_account.erlmcp.email}"
}

# ============================================================================
# Artifact Registry (for container images)
# ============================================================================

resource "google_artifact_registry_repository" "erlmcp" {
  count = local.deploy_cloud_run || local.deploy_gke ? 1 : 0

  location      = var.region
  repository_id = "${local.name_prefix}-repo"
  description   = "erlmcp container images"
  format        = "DOCKER"
  project       = var.project_id

  labels = local.common_labels

  depends_on = [google_project_service.required_apis]
}

# ============================================================================
# Outputs
# ============================================================================

output "vpc_network" {
  description = "VPC network name"
  value       = google_compute_network.main.name
}

output "vpc_subnet" {
  description = "VPC subnet name"
  value       = google_compute_subnetwork.main.name
}

output "service_account_email" {
  description = "erlmcp service account email"
  value       = google_service_account.erlmcp.email
}

output "artifact_registry_url" {
  description = "Artifact Registry URL for container images"
  value       = local.deploy_cloud_run || local.deploy_gke ? "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp[0].repository_id}" : null
}
