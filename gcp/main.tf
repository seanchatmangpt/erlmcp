# ErlMCP GCP Infrastructure - Main Configuration
# Terraform configuration for base GCP resources
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

  # Backend configuration for remote state
  # Uncomment and configure after initial bootstrap
  # backend "gcs" {
  #   bucket  = "erlmcp-terraform-state"
  #   prefix  = "base/terraform.tfstate"
  # }
}

# ============================================================================
# Provider Configuration
# ============================================================================

provider "google" {
  project = var.project_id
  region  = var.region

  default_labels = {
    environment  = var.environment
    managed_by   = "terraform"
    application  = var.app_name
    cost_center  = "engineering"
    compliance   = "sox-compliant"
  }
}

provider "google-beta" {
  project = var.project_id
  region  = var.region

  default_labels = {
    environment  = var.environment
    managed_by   = "terraform"
    application  = var.app_name
    cost_center  = "engineering"
    compliance   = "sox-compliant"
  }
}

# ============================================================================
# Enable Required GCP APIs
# ============================================================================

resource "google_project_service" "required_apis" {
  for_each = toset(var.allowed_services)

  project            = var.project_id
  service            = each.value
  disable_on_destroy = false

  timeouts {
    create = "30m"
    update = "30m"
  }
}

# ============================================================================
# Service Account for Deployment
# ============================================================================

resource "google_service_account" "erlmcp_deploy" {
  project      = var.project_id
  account_id   = var.service_account_name
  display_name = "ErlMCP Deployment Service Account"
  description  = "Service account for CI/CD deployment to GCP services"

  depends_on = [google_project_service.required_apis]
}

# Service Account Key (for CI/CD - store securely)
resource "google_service_account_key" "erlmcp_deploy_key" {
  service_account_id = google_service_account.erlmcp_deploy.name

  lifecycle {
    create_before_destroy = true
  }
}

# ============================================================================
# IAM Roles for Deployment Service Account
# ============================================================================

# Cloud Run Admin
resource "google_project_iam_member" "cloudrun_admin" {
  project = var.project_id
  role    = "roles/run.admin"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# Service Account User
resource "google_project_iam_member" "service_account_user" {
  project = var.project_id
  role    = "roles/iam.serviceAccountUser"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# Artifact Registry Writer
resource "google_project_iam_member" "artifact_registry_writer" {
  project = var.project_id
  role    = "roles/artifactregistry.writer"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# Secret Manager Accessor
resource "google_project_iam_member" "secret_accessor" {
  project = var.project_id
  role    = "roles/secretmanager.secretAccessor"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# Cloud KMS Crypto Operator
resource "google_project_iam_member" "kms_crypto_operator" {
  project = var.project_id
  role    = "roles/cloudkms.cryptoKeyEncrypterDecrypter"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# Cloud Build Editor
resource "google_project_iam_member" "cloudbuild_editor" {
  project = var.project_id
  role    = "roles/cloudbuild.builds.editor"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# Logging Writer
resource "google_project_iam_member" "logging_writer" {
  project = var.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# Monitoring Metric Writer
resource "google_project_iam_member" "monitoring_writer" {
  project = var.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# ============================================================================
# Artifact Registry Repository
# ============================================================================

resource "google_artifact_registry_repository" "erlmcp" {
  provider = google-beta

  project       = var.project_id
  location      = var.region
  repository_id = "${replace(var.app_name, "_", "-")}-repo"
  description   = "Docker container repository for ErlMCP - managed by Terraform"
  format        = "DOCKER"
  mode          = "STANDARD_REPOSITORY"

  docker_config {
    immutable_tags = var.environment == "production" ? true : false
  }

  cleanup_policies {
    id     = "delete-untagged"
    action = "DELETE"
    condition {
      tag_state = "UNTAGGED"
      older_than = "2592000s" # 30 days
    }
  }

  cleanup_policies {
    id     = "keep-recent-versions"
    action = "KEEP"
    most_recent_versions {
      keep_count = var.artifact_retention_count
    }
  }

  labels = {
    environment = var.environment
    managed_by  = "terraform"
    application = var.app_name
  }

  depends_on = [google_project_service.required_apis]
}

# Grant deployment SA access to repository
resource "google_artifact_registry_repository_iam_member" "deploy_writer" {
  project    = google_artifact_registry_repository.erlmcp.project
  location   = google_artifact_registry_repository.erlmcp.location
  repository = google_artifact_registry_repository.erlmcp.name
  role       = "roles/artifactregistry.writer"
  member     = "serviceAccount:${google_service_account.erlmcp_deploy.email}"
}

# ============================================================================
# Cloud KMS Key Ring and Crypto Keys
# ============================================================================

resource "google_kms_key_ring" "erlmcp" {
  project  = var.project_id
  name     = "${replace(var.app_name, "_", "-")}-keyring"
  location = var.region

  depends_on = [google_project_service.required_apis]
}

# Application Encryption Key
resource "google_kms_crypto_key" "erlmcp_app" {
  name            = "${replace(var.app_name, "_", "-")}-app-key"
  key_ring        = google_kms_key_ring.erlmcp.id
  rotation_period = var.kms_key_rotation_period
  purpose         = "ENCRYPT_DECRYPT"

  version_template {
    algorithm        = "GOOGLE_SYMMETRIC_ENCRYPTION"
    protection_level = var.environment == "production" ? "HSM" : "SOFTWARE"
  }

  lifecycle {
    prevent_destroy = true
  }

  depends_on = [google_kms_key_ring.erlmcp]
}

# Database Encryption Key
resource "google_kms_crypto_key" "erlmcp_db" {
  name            = "${replace(var.app_name, "_", "-")}-db-key"
  key_ring        = google_kms_key_ring.erlmcp.id
  rotation_period = var.kms_key_rotation_period
  purpose         = "ENCRYPT_DECRYPT"

  version_template {
    algorithm        = "GOOGLE_SYMMETRIC_ENCRYPTION"
    protection_level = var.environment == "production" ? "HSM" : "SOFTWARE"
  }

  lifecycle {
    prevent_destroy = true
  }

  depends_on = [google_kms_key_ring.erlmcp]
}

# Grant service account access to encryption keys
resource "google_kms_crypto_key_iam_member" "app_key_user" {
  crypto_key_id = google_kms_crypto_key.erlmcp_app.id
  role          = "roles/cloudkms.cryptoKeyEncrypterDecrypter"
  member        = "serviceAccount:${google_service_account.erlmcp_deploy.email}"
}

resource "google_kms_crypto_key_iam_member" "db_key_user" {
  crypto_key_id = google_kms_crypto_key.erlmcp_db.id
  role          = "roles/cloudkms.cryptoKeyEncrypterDecrypter"
  member        = "serviceAccount:${google_service_account.erlmcp_deploy.email}"
}

# ============================================================================
# Secret Manager Secrets
# ============================================================================

# Database URL Secret
resource "google_secret_manager_secret" "database_url" {
  project   = var.project_id
  secret_id = "erlmcp-database-url-${var.environment}"

  labels = {
    environment = var.environment
    managed_by  = "terraform"
    type        = "database"
  }

  replication {
    auto {}
  }

  rotation {
    next_rotation_time = timeadd(timestamp(), "2160h") # 90 days
    rotation_period    = "7776000s" # 90 days
  }

  depends_on = [google_project_service.required_apis]
}

resource "google_secret_manager_secret_iam_member" "database_url_accessor" {
  project   = google_secret_manager_secret.database_url.project
  secret_id = google_secret_manager_secret.database_url.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.erlmcp_deploy.email}"
}

# API Keys Secret
resource "google_secret_manager_secret" "api_keys" {
  project   = var.project_id
  secret_id = "erlmcp-api-keys-${var.environment}"

  labels = {
    environment = var.environment
    managed_by  = "terraform"
    type        = "api-credentials"
  }

  replication {
    auto {}
  }

  rotation {
    next_rotation_time = timeadd(timestamp(), "2160h") # 90 days
    rotation_period    = "7776000s" # 90 days
  }

  depends_on = [google_project_service.required_apis]
}

resource "google_secret_manager_secret_iam_member" "api_keys_accessor" {
  project   = google_secret_manager_secret.api_keys.project
  secret_id = google_secret_manager_secret.api_keys.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.erlmcp_deploy.email}"
}

# Deployment Credentials Secret
resource "google_secret_manager_secret" "deployment_credentials" {
  project   = var.project_id
  secret_id = "erlmcp-deployment-creds-${var.environment}"

  labels = {
    environment = var.environment
    managed_by  = "terraform"
    type        = "deployment"
  }

  replication {
    auto {}
  }

  rotation {
    next_rotation_time = timeadd(timestamp(), "720h") # 30 days
    rotation_period    = "2592000s" # 30 days
  }

  depends_on = [google_project_service.required_apis]
}

resource "google_secret_manager_secret_iam_member" "deployment_creds_accessor" {
  project   = google_secret_manager_secret.deployment_credentials.project
  secret_id = google_secret_manager_secret.deployment_credentials.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.erlmcp_deploy.email}"
}

# ============================================================================
# Cloud Storage Bucket for Terraform State
# ============================================================================

resource "google_storage_bucket" "terraform_state" {
  project       = var.project_id
  name          = "${var.project_id}-terraform-state"
  location      = var.region
  force_destroy = false

  uniform_bucket_level_access = true

  public_access_prevention = "enforced"

  versioning {
    enabled = true
  }

  encryption {
    default_kms_key_name = google_kms_crypto_key.erlmcp_app.id
  }

  soft_delete_policy {
    retention_duration_seconds = 604800 # 7 days
  }

  lifecycle_rule {
    condition {
      age                   = 90
      num_newer_versions    = 3
      with_state            = "ARCHIVED"
    }
    action {
      type = "Delete"
    }
  }

  lifecycle_rule {
    condition {
      days_since_noncurrent_time = 30
    }
    action {
      type = "Delete"
    }
  }

  labels = {
    environment = var.environment
    managed_by  = "terraform"
    purpose     = "terraform-state"
  }

  lifecycle {
    prevent_destroy = true
  }

  depends_on = [
    google_project_service.required_apis,
    google_kms_crypto_key.erlmcp_app
  ]
}

# Grant deployment SA access to state bucket
resource "google_storage_bucket_iam_member" "state_bucket_admin" {
  bucket = google_storage_bucket.terraform_state.name
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:${google_service_account.erlmcp_deploy.email}"
}

# Enable audit logging for state bucket
resource "google_storage_bucket_iam_binding" "state_bucket_audit" {
  bucket = google_storage_bucket.terraform_state.name
  role   = "roles/storage.objectViewer"

  members = [
    "serviceAccount:${google_service_account.erlmcp_deploy.email}",
  ]

  condition {
    title       = "audit_logging_required"
    description = "Require audit logging for state bucket access"
    expression  = "request.auth.claims.email_verified == true"
  }
}

# ============================================================================
# Cloud Logging and Monitoring
# ============================================================================

# Log bucket for centralized logging
resource "google_logging_project_bucket_config" "erlmcp_logs" {
  project        = var.project_id
  location       = var.region
  retention_days = var.log_retention_days
  bucket_id      = "${var.app_name}-${var.environment}-logs"

  cmek_settings {
    kms_key_name = google_kms_crypto_key.erlmcp_app.id
  }

  depends_on = [google_project_service.required_apis]
}

# Log sink for security events
resource "google_logging_project_sink" "security_sink" {
  name        = "${var.app_name}-security-logs"
  destination = "storage.googleapis.com/${google_storage_bucket.security_logs.name}"
  filter      = "severity >= ERROR OR protoPayload.methodName =~ \".*iam.*\" OR protoPayload.methodName =~ \".*kms.*\""

  unique_writer_identity = true

  depends_on = [google_storage_bucket.security_logs]
}

# Security logs bucket
resource "google_storage_bucket" "security_logs" {
  project       = var.project_id
  name          = "${var.project_id}-security-logs"
  location      = var.region
  force_destroy = false

  uniform_bucket_level_access = true
  public_access_prevention    = "enforced"

  versioning {
    enabled = true
  }

  encryption {
    default_kms_key_name = google_kms_crypto_key.erlmcp_app.id
  }

  lifecycle_rule {
    condition {
      age = var.security_log_retention_days
    }
    action {
      type = "Delete"
    }
  }

  labels = {
    environment = var.environment
    managed_by  = "terraform"
    purpose     = "security-logs"
  }

  lifecycle {
    prevent_destroy = true
  }

  depends_on = [google_project_service.required_apis]
}

# Grant log sink permission to write to bucket
resource "google_storage_bucket_iam_member" "security_log_writer" {
  bucket = google_storage_bucket.security_logs.name
  role   = "roles/storage.objectCreator"
  member = google_logging_project_sink.security_sink.writer_identity
}

# ============================================================================
# Notification Channel for Alerts
# ============================================================================

resource "google_monitoring_notification_channel" "email" {
  count        = length(var.alert_email_addresses)
  display_name = "Email Notification - ${var.alert_email_addresses[count.index]}"
  type         = "email"

  labels = {
    email_address = var.alert_email_addresses[count.index]
  }

  enabled = true

  depends_on = [google_project_service.required_apis]
}

# ============================================================================
# Budget Alert
# ============================================================================

resource "google_billing_budget" "erlmcp_budget" {
  count = var.billing_account != "" ? 1 : 0

  billing_account = var.billing_account
  display_name    = "ErlMCP ${var.environment} Budget"

  budget_filter {
    projects = ["projects/${var.project_id}"]

    labels = {
      environment = var.environment
      application = var.app_name
    }
  }

  amount {
    specified_amount {
      currency_code = "USD"
      units         = var.monthly_budget_limit
    }
  }

  threshold_rules {
    threshold_percent = 0.5
    spend_basis       = "CURRENT_SPEND"
  }

  threshold_rules {
    threshold_percent = 0.75
    spend_basis       = "CURRENT_SPEND"
  }

  threshold_rules {
    threshold_percent = 0.9
    spend_basis       = "CURRENT_SPEND"
  }

  threshold_rules {
    threshold_percent = 1.0
    spend_basis       = "CURRENT_SPEND"
  }

  all_updates_rule {
    monitoring_notification_channels = [
      for channel in google_monitoring_notification_channel.email : channel.id
    ]
    disable_default_iam_recipients = false
  }
}
