# ErlMCP Multi-Environment Configuration
# Manages development, staging, and production environments in GCP
# Updated: 2026-02-06

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 6.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = "~> 6.0"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.6"
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
  description = "GCP project ID for this environment"
  type        = string

  validation {
    condition     = can(regex("^[a-z][a-z0-9-]{4,28}[a-z0-9]$", var.project_id))
    error_message = "Project ID must be 6-30 characters, start with a letter, and contain only lowercase letters, numbers, and hyphens."
  }
}

variable "region" {
  description = "Primary GCP region"
  type        = string
  default     = "us-central1"
}

variable "gke_zones" {
  description = "GKE cluster zones for node distribution"
  type        = list(string)
}

variable "gke_cluster_name" {
  description = "Name of the GKE cluster"
  type        = string
}

variable "gke_node_pool_size" {
  description = "Initial number of nodes in GKE cluster"
  type        = number
}

variable "gke_machine_type" {
  description = "Machine type for GKE nodes"
  type        = string
}

variable "firestore_enabled" {
  description = "Enable Firestore database"
  type        = bool
  default     = true
}

variable "cloud_sql_tier" {
  description = "Cloud SQL instance tier"
  type        = string
}

variable "backup_retention_days" {
  description = "Database backup retention in days"
  type        = number
}

variable "enable_monitoring" {
  description = "Enable comprehensive monitoring and alerting"
  type        = bool
  default     = true
}

# ============================================================================
# Locals - Derived values and environment-specific settings
# ============================================================================

locals {
  environment_config = {
    dev = {
      gke_cluster_name       = "erlmcp-dev-cluster"
      cloud_sql_name         = "erlmcp-dev-db"
      redis_memory_gb        = 1
      redis_tier             = "BASIC"
      backup_frequency       = "DAILY"
      enable_failover        = false
      enable_ha              = false
      replica_regions        = []
      enable_dr              = false
      alert_threshold_err    = 0.10 # 10% error rate
      alert_threshold_lat    = 5000 # 5 second latency
      gke_autoscaling_min    = 1
      gke_autoscaling_max    = 5
      sql_availability       = "ZONAL"
      sql_disk_size          = 10
      enable_deletion_protection = false
      enable_cmek            = false
    }
    staging = {
      gke_cluster_name       = "erlmcp-staging-cluster"
      cloud_sql_name         = "erlmcp-staging-db"
      redis_memory_gb        = 4
      redis_tier             = "STANDARD_HA"
      backup_frequency       = "DAILY"
      enable_failover        = true
      enable_ha              = true
      replica_regions        = ["us-east1"]
      enable_dr              = true
      alert_threshold_err    = 0.05 # 5% error rate
      alert_threshold_lat    = 2000 # 2 second latency
      gke_autoscaling_min    = 2
      gke_autoscaling_max    = 10
      sql_availability       = "REGIONAL"
      sql_disk_size          = 50
      enable_deletion_protection = true
      enable_cmek            = true
    }
    production = {
      gke_cluster_name       = "erlmcp-prod-cluster"
      cloud_sql_name         = "erlmcp-prod-db"
      redis_memory_gb        = 16
      redis_tier             = "STANDARD_HA"
      backup_frequency       = "CONTINUOUS"
      enable_failover        = true
      enable_ha              = true
      replica_regions        = ["us-east1", "europe-west1"]
      enable_dr              = true
      alert_threshold_err    = 0.001 # 0.1% error rate
      alert_threshold_lat    = 500   # 500ms latency
      gke_autoscaling_min    = 3
      gke_autoscaling_max    = 20
      sql_availability       = "REGIONAL"
      sql_disk_size          = 100
      enable_deletion_protection = true
      enable_cmek            = true
    }
  }

  env = local.environment_config[var.environment]

  common_labels = {
    environment = var.environment
    managed_by  = "terraform"
    application = "erlmcp"
    cost_center = "engineering"
    team        = "platform"
  }
}

# ============================================================================
# Provider Configuration
# ============================================================================

provider "google" {
  project = var.project_id
  region  = var.region

  default_labels = local.common_labels
}

provider "google-beta" {
  project = var.project_id
  region  = var.region

  default_labels = local.common_labels
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
    "cloudtrace.googleapis.com",
    "cloudprofiler.googleapis.com",
    "cloudresourcemanager.googleapis.com",
    "servicenetworking.googleapis.com",
    "vpcaccess.googleapis.com",
    "dns.googleapis.com",
    "certificatemanager.googleapis.com",
    "binaryauthorization.googleapis.com",
    "containerscanning.googleapis.com",
    "cloudasset.googleapis.com"
  ])

  project            = var.project_id
  service            = each.value
  disable_on_destroy = false

  timeouts {
    create = "30m"
    update = "30m"
  }
}

# ============================================================================
# VPC Network and Subnets
# ============================================================================

resource "google_compute_network" "vpc" {
  provider                = google-beta
  project                 = var.project_id
  name                    = "erlmcp-${var.environment}-vpc"
  auto_create_subnetworks = false
  description             = "VPC for ErlMCP ${var.environment} environment"
  routing_mode            = "REGIONAL"

  depends_on = [google_project_service.required_apis]
}

resource "google_compute_subnetwork" "subnet" {
  provider      = google-beta
  project       = var.project_id
  name          = "erlmcp-${var.environment}-subnet"
  ip_cidr_range = var.environment == "production" ? "10.20.0.0/20" : (
    var.environment == "staging" ? "10.10.0.0/20" : "10.0.0.0/20"
  )
  region  = var.region
  network = google_compute_network.vpc.id

  private_ip_google_access = true

  secondary_ip_range {
    range_name    = "gke-pods"
    ip_cidr_range = var.environment == "production" ? "10.24.0.0/14" : (
      var.environment == "staging" ? "10.14.0.0/14" : "10.4.0.0/14"
    )
  }

  secondary_ip_range {
    range_name    = "gke-services"
    ip_cidr_range = var.environment == "production" ? "10.28.0.0/20" : (
      var.environment == "staging" ? "10.18.0.0/20" : "10.8.0.0/20"
    )
  }

  log_config {
    aggregation_interval = "INTERVAL_5_SEC"
    flow_sampling        = var.environment == "production" ? 1.0 : 0.5
    metadata             = "INCLUDE_ALL_METADATA"
  }

  depends_on = [google_compute_network.vpc]
}

# ============================================================================
# Cloud NAT and Cloud Router
# ============================================================================

resource "google_compute_router" "router" {
  project = var.project_id
  name    = "erlmcp-${var.environment}-router"
  region  = var.region
  network = google_compute_network.vpc.id

  bgp {
    asn = 64514
  }

  depends_on = [google_compute_network.vpc]
}

resource "google_compute_router_nat" "nat" {
  project                            = var.project_id
  name                               = "erlmcp-${var.environment}-nat"
  router                             = google_compute_router.router.name
  region                             = google_compute_router.router.region
  nat_ip_allocate_option             = "AUTO_ONLY"
  source_subnetwork_ip_ranges_to_nat = "ALL_SUBNETWORKS_ALL_IP_RANGES"

  min_ports_per_vm = var.environment == "production" ? 128 : 64

  log_config {
    enable = true
    filter = var.environment == "production" ? "ERRORS_ONLY" : "ALL"
  }

  depends_on = [google_compute_router.router]
}

# ============================================================================
# Cloud KMS Key Ring and Crypto Keys
# ============================================================================

resource "google_kms_key_ring" "keyring" {
  project  = var.project_id
  name     = "erlmcp-${var.environment}-keyring"
  location = var.region

  depends_on = [google_project_service.required_apis]
}

resource "google_kms_crypto_key" "app_key" {
  name            = "erlmcp-${var.environment}-app-key"
  key_ring        = google_kms_key_ring.keyring.id
  rotation_period = "7776000s" # 90 days
  purpose         = "ENCRYPT_DECRYPT"

  version_template {
    algorithm        = "GOOGLE_SYMMETRIC_ENCRYPTION"
    protection_level = local.env.enable_cmek && var.environment == "production" ? "HSM" : "SOFTWARE"
  }

  lifecycle {
    prevent_destroy = true
  }
}

resource "google_kms_crypto_key" "database_key" {
  name            = "erlmcp-${var.environment}-database-key"
  key_ring        = google_kms_key_ring.keyring.id
  rotation_period = "7776000s" # 90 days
  purpose         = "ENCRYPT_DECRYPT"

  version_template {
    algorithm        = "GOOGLE_SYMMETRIC_ENCRYPTION"
    protection_level = local.env.enable_cmek && var.environment == "production" ? "HSM" : "SOFTWARE"
  }

  lifecycle {
    prevent_destroy = true
  }
}

# ============================================================================
# GKE Cluster with Latest Features
# ============================================================================

resource "google_container_cluster" "primary" {
  provider = google-beta
  project  = var.project_id
  name     = local.env.gke_cluster_name
  location = var.region

  network    = google_compute_network.vpc.id
  subnetwork = google_compute_subnetwork.subnet.id

  # Use release channel for automatic updates
  release_channel {
    channel = var.environment == "production" ? "REGULAR" : "RAPID"
  }

  # Enable Autopilot for production (optional)
  # enable_autopilot = var.environment == "production"

  # Cluster configuration
  initial_node_count       = 1
  remove_default_node_pool = true

  # Workload Identity
  workload_identity_config {
    workload_pool = "${var.project_id}.svc.id.goog"
  }

  # Binary Authorization
  binary_authorization {
    evaluation_mode = var.environment == "production" ? "PROJECT_SINGLETON_POLICY_ENFORCE" : "DISABLED"
  }

  # Security features
  enable_shielded_nodes = true

  # Network Policy
  network_policy {
    enabled  = true
    provider = "PROVIDER_UNSPECIFIED"
  }

  # IP allocation policy
  ip_allocation_policy {
    cluster_secondary_range_name  = "gke-pods"
    services_secondary_range_name = "gke-services"
  }

  # Logging and monitoring (Cloud Operations)
  logging_config {
    enable_components = ["SYSTEM_COMPONENTS", "WORKLOADS"]
  }

  monitoring_config {
    enable_components = ["SYSTEM_COMPONENTS", "WORKLOADS"]

    managed_prometheus {
      enabled = var.enable_monitoring
    }
  }

  # Maintenance window
  maintenance_policy {
    daily_maintenance_window {
      start_time = "03:00"
    }
  }

  # Addons
  addons_config {
    http_load_balancing {
      disabled = false
    }
    horizontal_pod_autoscaling {
      disabled = false
    }
    network_policy_config {
      disabled = false
    }
    gcp_filestore_csi_driver_config {
      enabled = true
    }
    gcs_fuse_csi_driver_config {
      enabled = true
    }
  }

  # Security posture
  security_posture_config {
    mode               = var.environment == "production" ? "ENTERPRISE" : "BASIC"
    vulnerability_mode = var.environment == "production" ? "VULNERABILITY_ENTERPRISE" : "VULNERABILITY_BASIC"
  }

  # Private cluster configuration
  private_cluster_config {
    enable_private_nodes    = true
    enable_private_endpoint = false
    master_ipv4_cidr_block  = var.environment == "production" ? "172.16.0.0/28" : (
      var.environment == "staging" ? "172.16.1.0/28" : "172.16.2.0/28"
    )
  }

  # Master authorized networks
  master_authorized_networks_config {
    cidr_blocks {
      cidr_block   = "0.0.0.0/0"
      display_name = "All networks (update for production)"
    }
  }

  # Resource labels
  resource_labels = local.common_labels

  # Database encryption
  database_encryption {
    state    = local.env.enable_cmek ? "ENCRYPTED" : "DECRYPTED"
    key_name = local.env.enable_cmek ? google_kms_crypto_key.app_key.id : null
  }

  depends_on = [
    google_project_service.required_apis,
    google_compute_subnetwork.subnet
  ]
}

# Separate node pool for better control
resource "google_container_node_pool" "primary_nodes" {
  provider = google-beta
  project  = var.project_id
  name     = "primary-node-pool"
  cluster  = google_container_cluster.primary.id
  location = google_container_cluster.primary.location

  initial_node_count = var.gke_node_pool_size

  autoscaling {
    min_node_count       = local.env.gke_autoscaling_min
    max_node_count       = local.env.gke_autoscaling_max
    location_policy      = "BALANCED"
    total_min_node_count = local.env.gke_autoscaling_min
    total_max_node_count = local.env.gke_autoscaling_max
  }

  management {
    auto_repair  = true
    auto_upgrade = true
  }

  upgrade_settings {
    max_surge       = var.environment == "production" ? 1 : 2
    max_unavailable = 0
    strategy        = "SURGE"
  }

  node_config {
    preemptible  = var.environment == "dev"
    spot         = var.environment == "dev"
    machine_type = var.gke_machine_type
    disk_size_gb = var.environment == "production" ? 100 : 50
    disk_type    = "pd-balanced"
    image_type   = "COS_CONTAINERD"

    oauth_scopes = [
      "https://www.googleapis.com/auth/cloud-platform"
    ]

    # Shielded instance config
    shielded_instance_config {
      enable_secure_boot          = true
      enable_integrity_monitoring = true
    }

    # Workload metadata config
    workload_metadata_config {
      mode = "GKE_METADATA"
    }

    # Node taints for specific workloads
    # taint {
    #   key    = "workload"
    #   value  = "erlmcp"
    #   effect = "NO_SCHEDULE"
    # }

    labels = merge(local.common_labels, {
      node_pool = "primary"
    })

    tags = ["gke-node", "erlmcp-${var.environment}"]

    metadata = {
      disable-legacy-endpoints = "true"
    }
  }
}

# ============================================================================
# Cloud SQL PostgreSQL Instance
# ============================================================================

# Private VPC Connection for Cloud SQL
resource "google_compute_global_address" "private_ip_address" {
  project       = var.project_id
  name          = "erlmcp-${var.environment}-sql-ip"
  purpose       = "VPC_PEERING"
  address_type  = "INTERNAL"
  prefix_length = 16
  network       = google_compute_network.vpc.id

  depends_on = [google_compute_network.vpc]
}

resource "google_service_networking_connection" "private_vpc_connection" {
  network                 = google_compute_network.vpc.id
  service                 = "servicenetworking.googleapis.com"
  reserved_peering_ranges = [google_compute_global_address.private_ip_address.name]

  depends_on = [google_project_service.required_apis]
}

resource "google_sql_database_instance" "primary" {
  provider            = google-beta
  project             = var.project_id
  name                = local.env.cloud_sql_name
  database_version    = "POSTGRES_16"
  region              = var.region
  deletion_protection = local.env.enable_deletion_protection

  settings {
    tier                        = var.cloud_sql_tier
    availability_type           = local.env.sql_availability
    disk_size                   = local.env.sql_disk_size
    disk_type                   = "PD_SSD"
    disk_autoresize             = true
    disk_autoresize_limit       = var.environment == "production" ? 500 : 100
    edition                     = var.environment == "production" ? "ENTERPRISE" : "ENTERPRISE"

    backup_configuration {
      enabled                        = true
      start_time                     = "02:00"
      point_in_time_recovery_enabled = true
      transaction_log_retention_days = var.environment == "production" ? 7 : 3

      backup_retention_settings {
        retained_backups = var.backup_retention_days
        retention_unit   = "COUNT"
      }
    }

    ip_configuration {
      ipv4_enabled                                  = false
      private_network                               = google_compute_network.vpc.id
      enable_private_path_for_google_cloud_services = true
      require_ssl                                   = true
    }

    insights_config {
      query_insights_enabled  = true
      query_plans_per_minute  = 5
      query_string_length     = 1024
      record_application_tags = true
    }

    database_flags {
      name  = "max_connections"
      value = var.environment == "production" ? "200" : "100"
    }

    database_flags {
      name  = "log_checkpoints"
      value = "on"
    }

    database_flags {
      name  = "log_connections"
      value = "on"
    }

    database_flags {
      name  = "log_disconnections"
      value = "on"
    }

    database_flags {
      name  = "log_lock_waits"
      value = "on"
    }

    database_flags {
      name  = "log_min_duration_statement"
      value = var.environment == "production" ? "1000" : "5000"
    }

    maintenance_window {
      day          = 7 # Sunday
      hour         = 3
      update_track = var.environment == "production" ? "stable" : "canary"
    }

    user_labels = local.common_labels
  }

  encryption_key_name = local.env.enable_cmek ? google_kms_crypto_key.database_key.id : null

  depends_on = [
    google_project_service.required_apis,
    google_service_networking_connection.private_vpc_connection
  ]
}

# Cloud SQL database
resource "google_sql_database" "database" {
  project  = var.project_id
  name     = "erlmcp_${var.environment}"
  instance = google_sql_database_instance.primary.name
  charset  = "UTF8"
  collation = "en_US.UTF8"
}

# Generate secure database password
resource "random_password" "db_password" {
  length  = 32
  special = true
}

# Cloud SQL user
resource "google_sql_user" "erlmcp_user" {
  project  = var.project_id
  name     = "erlmcp-sa"
  instance = google_sql_database_instance.primary.name
  password = random_password.db_password.result
}

# ============================================================================
# Cloud Memorystore (Redis)
# ============================================================================

resource "google_redis_instance" "cache" {
  provider           = google-beta
  project            = var.project_id
  name               = "erlmcp-${var.environment}-cache"
  tier               = local.env.redis_tier
  memory_size_gb     = local.env.redis_memory_gb
  region             = var.region
  redis_version      = "REDIS_7_2"
  replica_count      = local.env.enable_ha ? 1 : 0
  read_replicas_mode = local.env.enable_ha ? "READ_REPLICAS_ENABLED" : "READ_REPLICAS_DISABLED"

  location_id             = var.gke_zones[0]
  alternative_location_id = local.env.enable_ha && length(var.gke_zones) > 1 ? var.gke_zones[1] : null
  authorized_network      = google_compute_network.vpc.id

  redis_configs = {
    "maxmemory-policy"      = "allkeys-lru"
    "notify-keyspace-events" = "Ex"
  }

  maintenance_policy {
    weekly_maintenance_window {
      day = "SUNDAY"
      start_time {
        hours   = 3
        minutes = 0
      }
    }
  }

  customer_managed_key = local.env.enable_cmek ? google_kms_crypto_key.app_key.id : null

  labels = local.common_labels

  depends_on = [
    google_project_service.required_apis,
    google_compute_network.vpc
  ]
}

# ============================================================================
# Firestore Database
# ============================================================================

resource "google_firestore_database" "firestore" {
  provider    = google-beta
  count       = var.firestore_enabled ? 1 : 0
  project     = var.project_id
  name        = "(default)"
  location_id = var.region
  type        = "FIRESTORE_NATIVE"

  concurrency_mode            = "OPTIMISTIC"
  app_engine_integration_mode = "DISABLED"
  point_in_time_recovery_enablement = var.environment == "production" ? "POINT_IN_TIME_RECOVERY_ENABLED" : "POINT_IN_TIME_RECOVERY_DISABLED"
  delete_protection_state     = local.env.enable_deletion_protection ? "DELETE_PROTECTION_ENABLED" : "DELETE_PROTECTION_DISABLED"

  depends_on = [google_project_service.required_apis]
}

# ============================================================================
# Secret Manager Secrets
# ============================================================================

resource "google_secret_manager_secret" "db_password" {
  project   = var.project_id
  secret_id = "erlmcp-${var.environment}-db-password"

  replication {
    auto {}
  }

  rotation {
    next_rotation_time = timeadd(timestamp(), "2160h") # 90 days
    rotation_period    = "7776000s" # 90 days
  }

  labels = local.common_labels

  depends_on = [google_project_service.required_apis]
}

resource "google_secret_manager_secret_version" "db_password" {
  secret      = google_secret_manager_secret.db_password.id
  secret_data = random_password.db_password.result
}

resource "google_secret_manager_secret" "jwt_secret" {
  project   = var.project_id
  secret_id = "erlmcp-${var.environment}-jwt-secret"

  replication {
    auto {}
  }

  rotation {
    next_rotation_time = timeadd(timestamp(), "2160h") # 90 days
    rotation_period    = "7776000s" # 90 days
  }

  labels = local.common_labels

  depends_on = [google_project_service.required_apis]
}

resource "random_password" "jwt_secret" {
  length  = 64
  special = true
}

resource "google_secret_manager_secret_version" "jwt_secret" {
  secret      = google_secret_manager_secret.jwt_secret.id
  secret_data = random_password.jwt_secret.result
}

# ============================================================================
# Service Account for ErlMCP Application
# ============================================================================

resource "google_service_account" "erlmcp_app" {
  project      = var.project_id
  account_id   = "erlmcp-${var.environment}-app"
  display_name = "ErlMCP ${var.environment} Application Service Account"
  description  = "Service account for ErlMCP application workloads in ${var.environment}"

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
    "roles/logging.logWriter",
    "roles/cloudtrace.agent"
  ])

  project = var.project_id
  role    = each.value
  member  = "serviceAccount:${google_service_account.erlmcp_app.email}"
}

# ============================================================================
# Service Account for CI/CD
# ============================================================================

resource "google_service_account" "cicd" {
  project      = var.project_id
  account_id   = "erlmcp-${var.environment}-cicd"
  display_name = "ErlMCP ${var.environment} CI/CD Service Account"
  description  = "Service account for CI/CD pipeline deployments in ${var.environment}"

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
    "roles/iam.serviceAccountUser",
    "roles/storage.objectAdmin"
  ])

  project = var.project_id
  role    = each.value
  member  = "serviceAccount:${google_service_account.cicd.email}"
}

# ============================================================================
# Artifact Registry Repository
# ============================================================================

resource "google_artifact_registry_repository" "docker" {
  provider      = google-beta
  project       = var.project_id
  location      = var.region
  repository_id = "erlmcp-${var.environment}-repo"
  format        = "DOCKER"
  description   = "Docker repository for ErlMCP ${var.environment} environment"
  mode          = "STANDARD_REPOSITORY"

  docker_config {
    immutable_tags = var.environment == "production"
  }

  cleanup_policies {
    id     = "delete-untagged"
    action = "DELETE"
    condition {
      tag_state  = "UNTAGGED"
      older_than = "2592000s" # 30 days
    }
  }

  cleanup_policies {
    id     = "keep-recent"
    action = "KEEP"
    most_recent_versions {
      keep_count = var.environment == "production" ? 20 : 10
    }
  }

  labels = local.common_labels

  depends_on = [google_project_service.required_apis]
}

# ============================================================================
# Monitoring and Alerting
# ============================================================================

resource "google_monitoring_uptime_check_config" "health_check" {
  count = var.enable_monitoring ? 1 : 0

  display_name = "erlmcp-${var.environment}-health-check"
  timeout      = "10s"
  period       = "60s"

  http_check {
    port         = 8080
    path         = "/health"
    use_ssl      = true
    validate_ssl = true
  }

  monitored_resource {
    type = "uptime_url"
    labels = {
      project_id = var.project_id
      host       = "erlmcp-${var.environment}.example.com"
    }
  }

  selected_regions = var.environment == "production" ? [
    "USA", "EUROPE", "ASIA_PACIFIC"
  ] : ["USA"]

  depends_on = [google_project_service.required_apis]
}

# ============================================================================
# Outputs
# ============================================================================

output "gke_cluster_name" {
  description = "GKE cluster name"
  value       = google_container_cluster.primary.name
}

output "gke_cluster_endpoint" {
  description = "GKE cluster endpoint"
  value       = google_container_cluster.primary.endpoint
  sensitive   = true
}

output "gke_cluster_ca_certificate" {
  description = "GKE cluster CA certificate"
  value       = google_container_cluster.primary.master_auth[0].cluster_ca_certificate
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

output "sql_instance_ip_address" {
  description = "Cloud SQL private IP address"
  value       = google_sql_database_instance.primary.private_ip_address
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

output "vpc_network_name" {
  description = "VPC network name"
  value       = google_compute_network.vpc.name
}

output "vpc_subnet_name" {
  description = "VPC subnet name"
  value       = google_compute_subnetwork.subnet.name
}
