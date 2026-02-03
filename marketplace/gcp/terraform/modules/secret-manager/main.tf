# ============================================================================
# Secret Manager Module for erlmcp
# Google Secret Manager for secure secret storage
# ============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.0.0"
    }
    random = {
      source  = "hashicorp/random"
      version = ">= 3.0.0"
    }
  }
}

# ============================================================================
# Enable Secret Manager API
# ============================================================================
resource "google_project_service" "secret_manager" {
  project            = var.project_id
  service            = "secretmanager.googleapis.com"
  disable_on_destroy = false
}

# ============================================================================
# Secret Manager Secrets
# All secrets are created but values must be set externally or via automation
# ============================================================================

# 1. Erlang Cookie - Critical for cluster security
resource "google_secret_manager_secret" "erlang_cookie" {
  project     = var.project_id
  secret_id   = "erlmcp-erlang-cookie"
  replication {
    automatic {}
  }
  labels = var.labels
  depends_on = [google_project_service.secret_manager]
}

resource "google_secret_manager_secret_version" "erlang_cookie" {
  count       = var.create_secret_versions ? 1 : 0
  secret      = google_secret_manager_secret.erlang_cookie.id
  secret_data = var.secrets.erlang_cookie != "" ? var.secrets.erlang_cookie : random_password.erlang_cookie[0].result
}

# Random password for erlang cookie if not provided
resource "random_password" "erlang_cookie" {
  count   = var.create_secret_versions && var.secrets.erlang_cookie == "" ? 1 : 0
  length  = 64
  special = false
}

# 2. Database Password
resource "google_secret_manager_secret" "db_password" {
  project     = var.project_id
  secret_id   = "erlmcp-db-password"
  replication {
    automatic {}
  }
  labels = var.labels
}

resource "google_secret_manager_secret_version" "db_password" {
  count       = var.create_secret_versions ? 1 : 0
  secret      = google_secret_manager_secret.db_password.id
  secret_data = var.secrets.db_password != "" ? var.secrets.db_password : random_password.db_password[0].result
}

resource "random_password" "db_password" {
  count   = var.create_secret_versions && var.secrets.db_password == "" ? 1 : 0
  length  = 32
  special = true
}

# 3. Redis Password
resource "google_secret_manager_secret" "redis_password" {
  project     = var.project_id
  secret_id   = "erlmcp-redis-password"
  replication {
    automatic {}
  }
  labels = var.labels
}

resource "google_secret_manager_secret_version" "redis_password" {
  count       = var.create_secret_versions ? 1 : 0
  secret      = google_secret_manager_secret.redis_password.id
  secret_data = var.secrets.redis_password != "" ? var.secrets.redis_password : random_password.redis_password[0].result
}

resource "random_password" "redis_password" {
  count   = var.create_secret_versions && var.secrets.redis_password == "" ? 1 : 0
  length  = 32
  special = false
}

# 4. TLS Certificate
resource "google_secret_manager_secret" "tls_cert" {
  project     = var.project_id
  secret_id   = "erlmcp-tls-cert"
  replication {
    automatic {}
  }
  labels = var.labels
}

resource "google_secret_manager_secret_version" "tls_cert" {
  count       = var.create_secret_versions ? 1 : 0
  secret      = google_secret_manager_secret.tls_cert.id
  secret_data = var.secrets.tls_cert
}

# 5. TLS Private Key
resource "google_secret_manager_secret" "tls_key" {
  project     = var.project_id
  secret_id   = "erlmcp-tls-key"
  replication {
    automatic {}
  }
  labels = var.labels
}

resource "google_secret_manager_secret_version" "tls_key" {
  count       = var.create_secret_versions ? 1 : 0
  secret      = google_secret_manager_secret.tls_key.id
  secret_data = var.secrets.tls_key
}

# 6. CA Bundle
resource "google_secret_manager_secret" "ca_bundle" {
  project     = var.project_id
  secret_id   = "erlmcp-ca-bundle"
  replication {
    automatic {}
  }
  labels = var.labels
}

resource "google_secret_manager_secret_version" "ca_bundle" {
  count       = var.create_secret_versions ? 1 : 0
  secret      = google_secret_manager_secret.ca_bundle.id
  secret_data = var.secrets.ca_bundle
}

# 7. JWT Private Key
resource "google_secret_manager_secret" "jwt_private_key" {
  project     = var.project_id
  secret_id   = "erlmcp-jwt-private-key"
  replication {
    automatic {}
  }
  labels = var.labels
}

resource "google_secret_manager_secret_version" "jwt_private_key" {
  count       = var.create_secret_versions ? 1 : 0
  secret      = google_secret_manager_secret.jwt_private_key.id
  secret_data = var.secrets.jwt_private_key
}

# 8. JWT Public Key
resource "google_secret_manager_secret" "jwt_public_key" {
  project     = var.project_id
  secret_id   = "erlmcp-jwt-public-key"
  replication {
    automatic {}
  }
  labels = var.labels
}

resource "google_secret_manager_secret_version" "jwt_public_key" {
  count       = var.create_secret_versions ? 1 : 0
  secret      = google_secret_manager_secret.jwt_public_key.id
  secret_data = var.secrets.jwt_public_key
}

# 9. Grafana Password
resource "google_secret_manager_secret" "grafana_password" {
  project     = var.project_id
  secret_id   = "erlmcp-grafana-password"
  replication {
    automatic {}
  }
  labels = var.labels
}

resource "google_secret_manager_secret_version" "grafana_password" {
  count       = var.create_secret_versions ? 1 : 0
  secret      = google_secret_manager_secret.grafana_password.id
  secret_data = var.secrets.grafana_password != "" ? var.secrets.grafana_password : random_password.grafana_password[0].result
}

resource "random_password" "grafana_password" {
  count   = var.create_secret_versions && var.secrets.grafana_password == "" ? 1 : 0
  length  = 32
  special = true
}

# 10. Backup Encryption Key
resource "google_secret_manager_secret" "backup_key" {
  project     = var.project_id
  secret_id   = "erlmcp-backup-key"
  replication {
    automatic {}
  }
  labels = var.labels
}

resource "google_secret_manager_secret_version" "backup_key" {
  count       = var.create_secret_versions ? 1 : 0
  secret      = google_secret_manager_secret.backup_key.id
  secret_data = var.secrets.backup_key != "" ? var.secrets.backup_key : random_password.backup_key[0].result
}

resource "random_password" "backup_key" {
  count   = var.create_secret_versions && var.secrets.backup_key == "" ? 1 : 0
  length  = 64
  special = false
}

# 11. OpenTelemetry CA Certificate
resource "google_secret_manager_secret" "otel_ca_cert" {
  project     = var.project_id
  secret_id   = "erlmcp-otel-ca-cert"
  replication {
    automatic {}
  }
  labels = var.labels
}

resource "google_secret_manager_secret_version" "otel_ca_cert" {
  count       = var.create_secret_versions ? 1 : 0
  secret      = google_secret_manager_secret.otel_ca_cert.id
  secret_data = var.secrets.otel_ca_cert
}

# ============================================================================
# IAM Bindings for Secret Access
# ============================================================================

resource "google_secret_manager_secret_iam_member" "erlang_cookie_accessor" {
  for_each = var.secret_accessors
  secret_id = google_secret_manager_secret.erlang_cookie.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value
}

resource "google_secret_manager_secret_iam_member" "db_password_accessor" {
  for_each = var.secret_accessors
  secret_id = google_secret_manager_secret.db_password.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value
}

resource "google_secret_manager_secret_iam_member" "redis_password_accessor" {
  for_each = var.secret_accessors
  secret_id = google_secret_manager_secret.redis_password.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value
}

resource "google_secret_manager_secret_iam_member" "all_secrets_viewer" {
  for_each = var.secret_viewers
  project   = var.project_id
  role      = "roles/secretmanager.viewer"
  member    = each.value
}
