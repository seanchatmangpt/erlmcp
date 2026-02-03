# ============================================================================
# Secret Manager & Data Services for erlmcp
# ============================================================================
# Secure secrets management and data layer infrastructure
#
# DOCKER-ONLY CONSTITUTION: Secrets externalized, never in container images
# ============================================================================

# ============================================================================
# Secret Manager Secrets
# ============================================================================

resource "google_secret_manager_secret" "erlmcp_cookie" {
  secret_id = "${local.name_prefix}-cookie"
  project   = var.project_id

  labels = local.common_labels

  replication {
    auto {}
  }

  depends_on = [google_project_service.required_apis]
}

resource "google_secret_manager_secret_version" "erlmcp_cookie" {
  secret      = google_secret_manager_secret.erlmcp_cookie.id
  secret_data = var.erlmcp_cookie != "" ? var.erlmcp_cookie : random_password.erlmcp_cookie.result
}

resource "random_password" "erlmcp_cookie" {
  length  = 64
  special = false
}

resource "google_secret_manager_secret" "db_password" {
  count     = var.enable_cloud_sql ? 1 : 0
  secret_id = "${local.name_prefix}-db-password"
  project   = var.project_id

  labels = local.common_labels

  replication {
    auto {}
  }

  depends_on = [google_project_service.required_apis]
}

resource "google_secret_manager_secret_version" "db_password" {
  count       = var.enable_cloud_sql ? 1 : 0
  secret      = google_secret_manager_secret.db_password[0].id
  secret_data = var.db_password != "" ? var.db_password : random_password.db_password[0].result
}

resource "random_password" "db_password" {
  count   = var.enable_cloud_sql ? 1 : 0
  length  = 32
  special = true
}

resource "google_secret_manager_secret" "db_url" {
  count     = var.enable_cloud_sql ? 1 : 0
  secret_id = "${local.name_prefix}-db-url"
  project   = var.project_id

  labels = local.common_labels

  replication {
    auto {}
  }

  depends_on = [google_project_service.required_apis]
}

resource "google_secret_manager_secret_version" "db_url" {
  count  = var.enable_cloud_sql ? 1 : 0
  secret = google_secret_manager_secret.db_url[0].id
  secret_data = format(
    "postgresql://%s:%s@%s/%s",
    google_sql_user.erlmcp[0].name,
    google_secret_manager_secret_version.db_password[0].secret_data,
    google_sql_database_instance.main[0].private_ip_address,
    google_sql_database.erlmcp[0].name
  )
}

# ============================================================================
# Service Account IAM for Secrets
# ============================================================================

resource "google_secret_manager_secret_iam_member" "erlmcp_cookie_access" {
  project   = var.project_id
  secret_id = google_secret_manager_secret.erlmcp_cookie.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.erlmcp.email}"
}

resource "google_secret_manager_secret_iam_member" "db_password_access" {
  count     = var.enable_cloud_sql ? 1 : 0
  project   = var.project_id
  secret_id = google_secret_manager_secret.db_password[0].secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.erlmcp.email}"
}

resource "google_secret_manager_secret_iam_member" "db_url_access" {
  count     = var.enable_cloud_sql ? 1 : 0
  project   = var.project_id
  secret_id = google_secret_manager_secret.db_url[0].secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.erlmcp.email}"
}

# ============================================================================
# Cloud SQL Instance
# ============================================================================

resource "google_sql_database_instance" "main" {
  count = var.enable_cloud_sql ? 1 : 0

  name             = "${local.name_prefix}-postgres"
  project          = var.project_id
  region           = var.region
  database_version = var.cloud_sql_version

  settings {
    tier              = var.cloud_sql_tier
    availability_type = var.cloud_sql_ha ? "REGIONAL" : "ZONAL"
    disk_size         = var.cloud_sql_disk_size
    disk_type         = "PD_SSD"
    disk_autoresize   = true

    backup_configuration {
      enabled                        = true
      binary_log_enabled             = false
      start_time                     = "03:00"
      point_in_time_recovery_enabled = var.cloud_sql_ha
      transaction_log_retention_days = 7
      backup_retention_settings {
        retained_backups = 14
        retention_unit   = "COUNT"
      }
    }

    ip_configuration {
      ipv4_enabled                                  = false
      private_network                               = google_compute_network.main.id
      enable_private_path_for_google_cloud_services = true

      dynamic "authorized_networks" {
        for_each = var.cloud_sql_authorized_networks
        content {
          name  = authorized_networks.value.name
          value = authorized_networks.value.cidr
        }
      }
    }

    maintenance_window {
      day          = 7  # Sunday
      hour         = 3
      update_track = "stable"
    }

    insights_config {
      query_insights_enabled  = true
      query_plans_per_minute  = 5
      query_string_length     = 1024
      record_application_tags = true
      record_client_address   = true
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
      value = "1000"
    }
    database_flags {
      name  = "cloudsql.enable_pg_bigm"
      value = "on"
    }

    user_labels = local.common_labels
  }

  deletion_protection = var.cloud_sql_deletion_protection

  depends_on = [
    google_project_service.required_apis,
    google_service_networking_connection.private_vpc_connection
  ]
}

resource "google_sql_database" "erlmcp" {
  count = var.enable_cloud_sql ? 1 : 0

  name     = "erlmcp"
  project  = var.project_id
  instance = google_sql_database_instance.main[0].name
}

resource "google_sql_user" "erlmcp" {
  count = var.enable_cloud_sql ? 1 : 0

  name     = "erlmcp"
  project  = var.project_id
  instance = google_sql_database_instance.main[0].name
  password = google_secret_manager_secret_version.db_password[0].secret_data
}

# ============================================================================
# Private Service Connection for Cloud SQL
# ============================================================================

resource "google_compute_global_address" "private_ip_range" {
  count = var.enable_cloud_sql || var.enable_memorystore ? 1 : 0

  name          = "${local.name_prefix}-private-ip"
  project       = var.project_id
  purpose       = "VPC_PEERING"
  address_type  = "INTERNAL"
  prefix_length = 16
  network       = google_compute_network.main.id
}

resource "google_service_networking_connection" "private_vpc_connection" {
  count = var.enable_cloud_sql || var.enable_memorystore ? 1 : 0

  network                 = google_compute_network.main.id
  service                 = "servicenetworking.googleapis.com"
  reserved_peering_ranges = [google_compute_global_address.private_ip_range[0].name]

  depends_on = [google_project_service.required_apis]
}

# ============================================================================
# Memorystore (Redis)
# ============================================================================

resource "google_redis_instance" "main" {
  count = var.enable_memorystore ? 1 : 0

  name               = "${local.name_prefix}-redis"
  project            = var.project_id
  region             = var.region
  tier               = var.memorystore_tier
  memory_size_gb     = var.memorystore_size_gb
  redis_version      = var.memorystore_version
  authorized_network = google_compute_network.main.id
  connect_mode       = "PRIVATE_SERVICE_ACCESS"

  redis_configs = {
    "maxmemory-policy" = "allkeys-lru"
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

  labels = local.common_labels

  depends_on = [google_service_networking_connection.private_vpc_connection]
}

# ============================================================================
# Outputs
# ============================================================================

output "erlmcp_cookie_secret_name" {
  description = "Secret Manager secret name for erlmcp cookie"
  value       = google_secret_manager_secret.erlmcp_cookie.secret_id
}

output "cloud_sql_instance_name" {
  description = "Cloud SQL instance name"
  value       = var.enable_cloud_sql ? google_sql_database_instance.main[0].name : null
}

output "cloud_sql_connection_name" {
  description = "Cloud SQL connection name"
  value       = var.enable_cloud_sql ? google_sql_database_instance.main[0].connection_name : null
}

output "cloud_sql_private_ip" {
  description = "Cloud SQL private IP"
  value       = var.enable_cloud_sql ? google_sql_database_instance.main[0].private_ip_address : null
}

output "memorystore_host" {
  description = "Memorystore Redis host"
  value       = var.enable_memorystore ? google_redis_instance.main[0].host : null
}

output "memorystore_port" {
  description = "Memorystore Redis port"
  value       = var.enable_memorystore ? google_redis_instance.main[0].port : null
}
