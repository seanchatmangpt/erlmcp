# Multi-Environment Configuration for ErlMCP
# Manages development, staging, and production environments in separate GCP projects
#
# This file defines shared infrastructure patterns applied across all environments,
# with environment-specific parameters stored in terraform.tfvars

terraform {
  required_version = ">= 1.0"
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = "~> 5.0"
    }
  }
}

# ============================================================================
# Variables - Environment-specific configuration
# ============================================================================

variable "environment" {
  description = "Environment name (dev, staging, production)"
  type        = string
  validation {
    condition     = contains(["dev", "staging", "production"], var.environment)
    error_message = "Environment must be dev, staging, or production."
  }
}

variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "region" {
  description = "GCP region"
  type        = string
  default     = "us-central1"
}

variable "gke_zones" {
  description = "GKE cluster zones"
  type        = list(string)
}

variable "gke_cluster_name" {
  description = "GKE cluster name"
  type        = string
}

variable "gke_node_pool_size" {
  description = "Number of nodes in GKE cluster"
  type        = number
}

variable "gke_machine_type" {
  description = "GKE node machine type"
  type        = string
}

variable "firestore_enabled" {
  description = "Enable Firestore database"
  type        = bool
  default     = true
}

variable "cloud_sql_tier" {
  description = "Cloud SQL instance tier (db-f1-micro, db-g1-small, db-custom-*)"
  type        = string
}

variable "backup_retention_days" {
  description = "Database backup retention in days"
  type        = number
}

variable "enable_monitoring" {
  description = "Enable comprehensive monitoring"
  type        = bool
  default     = true
}

# ============================================================================
# Locals - Derived values and common settings
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
    }
    production = {
      gke_cluster_name    = "erlmcp-prod-cluster"
      cloud_sql_name      = "erlmcp-prod"
      redis_memory_gb     = 16
      backup_frequency    = "HOURLY"
      enable_failover     = true
      replica_regions     = ["us-east1"]
      enable_dr           = true
      alert_threshold_err = 0.001  # 0.1% error rate
      alert_threshold_lat = 500    # 500ms latency
    }
  }

  env = local.environment_config[var.environment]

  common_labels = {
    environment = var.environment
    managed_by  = "terraform"
    application = "erlmcp"
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
# Enable Required APIs
# ============================================================================

resource "google_project_service" "required_apis" {
  for_each = toset([
    "container.googleapis.com",
    "compute.googleapis.com",
    "sqladmin.googleapis.com",
    "redis.googleapis.com",
    "firestore.googleapis.com",
    "cloudkms.googleapis.com",
    "secretmanager.googleapis.com",
    "artifactregistry.googleapis.com",
    "cloudbuild.googleapis.com",
    "monitoring.googleapis.com",
    "logging.googleapis.com",
    "cloudresourcemanager.googleapis.com",
    "servicenetworking.googleapis.com",
    "vpcaccess.googleapis.com"
  ])

  service            = each.value
  disable_on_destroy = false

  timeouts {
    create = "20m"
    update = "20m"
  }
}

# ============================================================================
# VPC Network and Subnets
# ============================================================================

resource "google_compute_network" "vpc" {
  name                    = "erlmcp-${var.environment}-vpc"
  auto_create_subnetworks = false
  description             = "VPC for ErlMCP ${var.environment} environment"

  depends_on = [google_project_service.required_apis]
}

resource "google_compute_subnetwork" "subnet" {
  name          = "erlmcp-${var.environment}-subnet"
  ip_cidr_range = var.environment == "production" ? "10.20.0.0/16" : (
    var.environment == "staging" ? "10.10.0.0/16" : "10.0.0.0/16"
  )
  region  = var.region
  network = google_compute_network.vpc.id

  private_ip_google_access = true

  log_config {
    aggregation_interval = "INTERVAL_5_SEC"
    flow_logs_enabled    = true
    metadata             = "INCLUDE_ALL_METADATA"
  }

  depends_on = [google_compute_network.vpc]
}

# ============================================================================
# GKE Cluster
# ============================================================================

resource "google_container_cluster" "primary" {
  name       = local.env.gke_cluster_name
  location   = var.gke_zones[0]
  network    = google_compute_network.vpc.id
  subnetwork = google_compute_subnetwork.subnet.id

  # Cluster configuration
  initial_node_count       = var.gke_node_pool_size
  remove_default_node_pool = true

  # Security features
  enable_shielded_nodes = true
  enable_ip_allocation_policy = true

  # Network configuration
  cluster_secondary_range_name = "pods"
  ip_allocation_policy {
    cluster_secondary_range_name  = "pods"
    services_secondary_range_name = "services"
  }

  # Logging and monitoring
  logging_config {
    enable_components = ["SYSTEM_COMPONENTS", "WORKLOADS"]
  }

  monitoring_config {
    enable_components = ["SYSTEM_COMPONENTS", "WORKLOADS"]
  }

  # Resource labels
  resource_labels = local.common_labels

  depends_on = [
    google_project_service.required_apis,
    google_compute_subnetwork.subnet
  ]
}

# Separate node pool for better control
resource "google_container_node_pool" "primary_nodes" {
  name           = "primary-node-pool"
  cluster        = google_container_cluster.primary.id
  node_count     = var.gke_node_pool_size
  initial_node_count = var.gke_node_pool_size

  autoscaling {
    min_node_count = var.environment == "production" ? 3 : 1
    max_node_count = var.environment == "production" ? 20 : 10
  }

  management {
    auto_repair  = true
    auto_upgrade = true
  }

  node_config {
    preemptible  = var.environment != "production"
    machine_type = var.gke_machine_type
    disk_size_gb = 50

    oauth_scopes = [
      "https://www.googleapis.com/auth/cloud-platform"
    ]

    shielded_instance_config {
      enable_secure_boot          = true
      enable_integrity_monitoring = true
    }

    workload_metadata_config {
      mode = "GKE_METADATA"
    }

    labels = local.common_labels
  }
}

# ============================================================================
# Cloud SQL Instance
# ============================================================================

resource "google_sql_database_instance" "primary" {
  name                = local.env.cloud_sql_name
  database_version    = "POSTGRES_14"
  region              = var.region
  deletion_protection = var.environment == "production"

  settings {
    tier                        = var.cloud_sql_tier
    availability_type           = var.environment == "production" ? "REGIONAL" : "ZONAL"
    backup_configuration {
      enabled                        = true
      backup_retention_settings {
        retained_backups = var.backup_retention_days
        retention_unit   = "COUNT"
      }
      binary_log_enabled          = false
      point_in_time_recovery_enabled = true
      transaction_log_retention_days = 7
      backup_frequency_unit       = "COUNT"
    }

    ip_configuration {
      require_ssl = true

      private_network = google_compute_network.vpc.id
      ipv4_enabled    = var.environment != "production"

      authorized_networks {
        name  = "Office"
        value = "0.0.0.0/0"  # Restrict in production!
      }
    }

    insights_config {
      query_insights_enabled  = true
      query_string_length    = 1024
      record_application_tags = true
    }

    database_flags {
      name  = "max_connections"
      value = var.environment == "production" ? "200" : "100"
    }

    database_flags {
      name  = "log_statement"
      value = var.environment == "production" ? "ERROR" : "ALL"
    }

    user_labels = local.common_labels
  }

  depends_on = [google_project_service.required_apis]
}

# Cloud SQL database
resource "google_sql_database" "database" {
  name     = "erlmcp_${var.environment}"
  instance = google_sql_database_instance.primary.name
}

# Cloud SQL user
resource "google_sql_user" "erlmcp_user" {
  name     = "erlmcp-sa"
  instance = google_sql_database_instance.primary.name
  password = random_password.db_password.result
}

# Generate secure password
resource "random_password" "db_password" {
  length  = 32
  special = true
}

# ============================================================================
# Cloud SQL Replica (for staging and production)
# ============================================================================

resource "google_sql_database_instance" "replica" {
  count = contains(local.env.replica_regions, "us-east1") ? 1 : 0

  name                 = "${local.env.cloud_sql_name}-replica-us-east1"
  database_version     = google_sql_database_instance.primary.database_version
  region               = "us-east1"
  master_instance_name = google_sql_database_instance.primary.name

  replica_configuration {
    kind                    = "SQLSERVER_REPLICA"
    mysql_replica_configuration {
      client_key             = null
      client_certificate     = null
      ca_certificate         = null
      username               = null
      password               = null
    }
  }

  deletion_protection = var.environment == "production"

  depends_on = [google_sql_database_instance.primary]
}

# ============================================================================
# Cloud Memorystore (Redis)
# ============================================================================

resource "google_redis_instance" "cache" {
  name           = "erlmcp-${var.environment}-cache"
  memory_size_gb = local.env.redis_memory_gb
  tier           = var.environment == "production" ? "STANDARD_HA" : "BASIC"
  region         = var.region
  redis_version  = "7.0"

  location_id       = var.gke_zones[0]
  authorized_network = google_compute_network.vpc.id

  redis_configs = {
    "maxmemory-policy" = var.environment == "production" ? "allkeys-lru" : "noeviction"
  }

  labels = local.common_labels

  depends_on = [google_project_service.required_apis]
}

# ============================================================================
# Firestore Database
# ============================================================================

resource "google_firestore_database" "firestore" {
  count           = var.firestore_enabled ? 1 : 0
  name            = "erlmcp-${var.environment}"
  location_id     = var.region
  type            = "FIRESTORE_NATIVE"
  delete_protection_state = var.environment == "production" ? "DELETE_PROTECTION_ENABLED" : "DELETE_PROTECTION_DISABLED"
  backup_config {
    enabled = true
  }

  depends_on = [google_project_service.required_apis]
}

# ============================================================================
# Cloud KMS Keyring and Crypto Key
# ============================================================================

resource "google_kms_key_ring" "keyring" {
  name     = "erlmcp-${var.environment}-keyring"
  location = var.region

  depends_on = [google_project_service.required_apis]
}

resource "google_kms_crypto_key" "app_key" {
  name            = "erlmcp-${var.environment}-app-key"
  key_ring        = google_kms_key_ring.keyring.id
  rotation_period = "7776000s"  # 90 days
  purpose         = "ENCRYPT_DECRYPT"

  lifecycle {
    prevent_destroy = var.environment == "production"
  }
}

# ============================================================================
# Secret Manager Secrets
# ============================================================================

resource "google_secret_manager_secret" "db_password" {
  secret_id = "erlmcp-${var.environment}-db-password"

  replication {
    auto {}
  }

  depends_on = [google_project_service.required_apis]
}

resource "google_secret_manager_secret_version" "db_password" {
  secret      = google_secret_manager_secret.db_password.id
  secret_data = random_password.db_password.result
}

resource "google_secret_manager_secret" "jwt_secret" {
  secret_id = "erlmcp-${var.environment}-jwt-secret"

  replication {
    auto {}
  }

  depends_on = [google_project_service.required_apis]
}

resource "google_secret_manager_secret_version" "jwt_secret" {
  secret      = google_secret_manager_secret.jwt_secret.id
  secret_data = random_password.jwt_secret.result
}

resource "random_password" "jwt_secret" {
  length  = 64
  special = true
}

# ============================================================================
# Service Account for ErlMCP Application
# ============================================================================

resource "google_service_account" "erlmcp_app" {
  account_id   = "erlmcp-${var.environment}-app"
  display_name = "ErlMCP ${var.environment} Application Service Account"
  description  = "Service account for ErlMCP application in ${var.environment}"

  depends_on = [google_project_service.required_apis]
}

# Grant necessary roles to service account
resource "google_project_iam_member" "erlmcp_app_roles" {
  for_each = toset([
    "roles/cloudsql.client",
    "roles/datastore.user",
    "roles/storage.objectViewer",
    "roles/secretmanager.secretAccessor",
    "roles/cloudkms.cryptoKeyEncrypterDecrypter",
    "roles/monitoring.metricWriter",
    "roles/logging.logWriter"
  ])

  project = var.project_id
  role    = each.value
  member  = "serviceAccount:${google_service_account.erlmcp_app.email}"
}

# ============================================================================
# Service Account for CI/CD
# ============================================================================

resource "google_service_account" "cicd" {
  account_id   = "erlmcp-${var.environment}-cicd"
  display_name = "ErlMCP ${var.environment} CI/CD Service Account"
  description  = "Service account for CI/CD pipeline in ${var.environment}"

  depends_on = [google_project_service.required_apis]
}

# Grant CI/CD roles
resource "google_project_iam_member" "cicd_roles" {
  for_each = toset([
    "roles/container.developer",
    "roles/artifactregistry.writer",
    "roles/cloudkms.cryptoKeyEncrypterDecrypter",
    "roles/secretmanager.secretAccessor",
    "roles/cloudbuild.builds.editor",
    "roles/iam.serviceAccountUser"
  ])

  project = var.project_id
  role    = each.value
  member  = "serviceAccount:${google_service_account.cicd.email}"
}

# ============================================================================
# Artifact Registry Repository
# ============================================================================

resource "google_artifact_registry_repository" "docker" {
  location      = var.region
  repository_id = "erlmcp-${var.environment}-repo"
  format        = "DOCKER"
  description   = "Docker repository for ErlMCP ${var.environment} environment"
  labels        = local.common_labels

  depends_on = [google_project_service.required_apis]
}

# ============================================================================
# Monitoring (Cloud Monitoring)
# ============================================================================

resource "google_monitoring_uptime_check_config" "health_check" {
  count = var.enable_monitoring ? 1 : 0

  display_name = "erlmcp-${var.environment}-health-check"
  timeout      = "10s"
  period       = "60s"

  http_check {
    port = 8080
    path = "/health"
  }

  monitored_resource {
    type = "gce_instance"
    labels = {
      instance_id = google_container_cluster.primary.id
      zone        = var.gke_zones[0]
    }
  }

  selected_regions = ["USA", "EUROPE", "ASIA_PACIFIC"]

  depends_on = [google_project_service.required_apis]
}

# ============================================================================
# Cloud NAT and Cloud Router
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
# Outputs
# ============================================================================

output "gke_cluster_name" {
  description = "GKE cluster name"
  value       = google_container_cluster.primary.name
}

output "gke_cluster_host" {
  description = "GKE cluster host endpoint"
  value       = google_container_cluster.primary.endpoint
  sensitive   = true
}

output "sql_instance_name" {
  description = "Cloud SQL instance name"
  value       = google_sql_database_instance.primary.name
}

output "sql_instance_connection_name" {
  description = "Cloud SQL instance connection string"
  value       = google_sql_database_instance.primary.connection_name
}

output "redis_host" {
  description = "Redis instance host"
  value       = google_redis_instance.cache.host
}

output "redis_port" {
  description = "Redis instance port"
  value       = google_redis_instance.cache.port
}

output "artifact_registry_url" {
  description = "Artifact Registry repository URL"
  value       = "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.docker.repository_id}"
}

output "service_account_email" {
  description = "ErlMCP application service account email"
  value       = google_service_account.erlmcp_app.email
}

output "cicd_service_account_email" {
  description = "CI/CD service account email"
  value       = google_service_account.cicd.email
}

output "firestore_database_name" {
  description = "Firestore database name"
  value       = var.firestore_enabled ? google_firestore_database.firestore[0].name : null
}
