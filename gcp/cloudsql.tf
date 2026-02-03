# ==============================================================================
# ERLMCP Cloud SQL PostgreSQL
# ==============================================================================
# Managed PostgreSQL database for erlmcp state persistence.
#
# Features:
# - High availability (regional)
# - Automatic backups with point-in-time recovery
# - Private IP only (no public access)
# - Encryption at rest (CMEK optional)
# - Read replicas for scaling reads
# ==============================================================================

# Cloud SQL Instance
resource "google_sql_database_instance" "erlmcp" {
  count = var.enable_cloud_sql ? 1 : 0

  name                = "${var.app_name}-db-${random_id.db_suffix.hex}"
  database_version    = var.cloud_sql_version
  region              = var.region
  deletion_protection = var.environment == "production" ? true : false

  settings {
    tier              = var.cloud_sql_tier
    availability_type = var.cloud_sql_ha ? "REGIONAL" : "ZONAL"
    disk_size         = var.cloud_sql_disk_size
    disk_type         = "PD_SSD"
    disk_autoresize   = true

    # Backup configuration
    backup_configuration {
      enabled                        = true
      binary_log_enabled             = false
      start_time                     = "03:00"
      point_in_time_recovery_enabled = var.cloud_sql_pitr
      transaction_log_retention_days = 7

      backup_retention_settings {
        retained_backups = var.cloud_sql_backup_retention
        retention_unit   = "COUNT"
      }
    }

    # IP configuration (private only)
    ip_configuration {
      ipv4_enabled                                  = false
      private_network                               = var.create_vpc ? google_compute_network.erlmcp[0].id : "projects/${var.project_id}/global/networks/${var.existing_vpc_name}"
      enable_private_path_for_google_cloud_services = true

      # Allow Cloud Run VPC connector
      dynamic "authorized_networks" {
        for_each = var.cloud_sql_authorized_networks
        content {
          name  = authorized_networks.value.name
          value = authorized_networks.value.cidr
        }
      }
    }

    # Maintenance window
    maintenance_window {
      day          = 7 # Sunday
      hour         = 4
      update_track = "stable"
    }

    # Database flags for PostgreSQL optimization
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
      name  = "log_temp_files"
      value = "0"
    }

    database_flags {
      name  = "max_connections"
      value = var.cloud_sql_max_connections
    }

    # Insights for query performance
    insights_config {
      query_insights_enabled  = true
      query_plans_per_minute  = 5
      query_string_length     = 1024
      record_application_tags = true
      record_client_address   = true
    }

    user_labels = local.common_labels
  }

  depends_on = [
    google_project_service.required_apis,
    google_service_networking_connection.private_vpc_connection,
  ]

  lifecycle {
    prevent_destroy = false
  }
}

# Random suffix for unique instance names
resource "random_id" "db_suffix" {
  byte_length = 4
}

# Database
resource "google_sql_database" "erlmcp" {
  count = var.enable_cloud_sql ? 1 : 0

  name     = var.cloud_sql_database_name
  instance = google_sql_database_instance.erlmcp[0].name
  charset  = "UTF8"
}

# Database user
resource "google_sql_user" "erlmcp" {
  count = var.enable_cloud_sql ? 1 : 0

  name     = var.cloud_sql_user
  instance = google_sql_database_instance.erlmcp[0].name
  password = var.cloud_sql_password != "" ? var.cloud_sql_password : random_password.db_password.result

  deletion_policy = "ABANDON"
}

# Random password if not provided
resource "random_password" "db_password" {
  length  = 32
  special = true
}

# Store database URL in Secret Manager
resource "google_secret_manager_secret_version" "database_url_generated" {
  count = var.enable_cloud_sql ? 1 : 0

  secret = google_secret_manager_secret.database_url.id
  secret_data = format(
    "postgresql://%s:%s@%s/%s",
    google_sql_user.erlmcp[0].name,
    google_sql_user.erlmcp[0].password,
    google_sql_database_instance.erlmcp[0].private_ip_address,
    google_sql_database.erlmcp[0].name
  )
}

# ==============================================================================
# Read Replica (for production scaling)
# ==============================================================================

resource "google_sql_database_instance" "erlmcp_replica" {
  count = var.enable_cloud_sql && var.cloud_sql_read_replicas > 0 ? var.cloud_sql_read_replicas : 0

  name                 = "${var.app_name}-db-replica-${count.index}-${random_id.db_suffix.hex}"
  database_version     = var.cloud_sql_version
  region               = var.region
  master_instance_name = google_sql_database_instance.erlmcp[0].name

  replica_configuration {
    failover_target = false
  }

  settings {
    tier            = var.cloud_sql_replica_tier != "" ? var.cloud_sql_replica_tier : var.cloud_sql_tier
    disk_autoresize = true

    ip_configuration {
      ipv4_enabled    = false
      private_network = var.create_vpc ? google_compute_network.erlmcp[0].id : "projects/${var.project_id}/global/networks/${var.existing_vpc_name}"
    }

    user_labels = merge(local.common_labels, {
      role = "replica"
    })
  }

  deletion_protection = false
}

# ==============================================================================
# Cloud SQL Proxy (for local development / debugging)
# ==============================================================================

# Service account for Cloud SQL Proxy
resource "google_service_account" "cloudsql_proxy" {
  count = var.enable_cloud_sql ? 1 : 0

  account_id   = "${var.app_name}-cloudsql-proxy"
  display_name = "Cloud SQL Proxy Service Account"
}

resource "google_project_iam_member" "cloudsql_client" {
  count = var.enable_cloud_sql ? 1 : 0

  project = var.project_id
  role    = "roles/cloudsql.client"
  member  = "serviceAccount:${google_service_account.cloudsql_proxy[0].email}"
}

# Key for Cloud SQL Proxy (for local development)
resource "google_service_account_key" "cloudsql_proxy" {
  count = var.enable_cloud_sql && var.create_cloudsql_proxy_key ? 1 : 0

  service_account_id = google_service_account.cloudsql_proxy[0].name
}

# Store proxy key in Secret Manager
resource "google_secret_manager_secret" "cloudsql_proxy_key" {
  count = var.enable_cloud_sql && var.create_cloudsql_proxy_key ? 1 : 0

  secret_id = "erlmcp-cloudsql-proxy-key"

  replication {
    auto {}
  }

  labels = local.common_labels
}

resource "google_secret_manager_secret_version" "cloudsql_proxy_key" {
  count = var.enable_cloud_sql && var.create_cloudsql_proxy_key ? 1 : 0

  secret      = google_secret_manager_secret.cloudsql_proxy_key[0].id
  secret_data = base64decode(google_service_account_key.cloudsql_proxy[0].private_key)
}
