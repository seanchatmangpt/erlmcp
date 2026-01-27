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

  # Backend configuration (uncomment and configure for remote state)
  # backend "gcs" {
  #   bucket  = "erlmcp-terraform-state"
  #   prefix  = "prod"
  # }
}

provider "google" {
  project = var.project_id
  region  = var.region
}

provider "google-beta" {
  project = var.project_id
  region  = var.region
}

# Enable required GCP APIs
resource "google_project_service" "required_apis" {
  for_each = toset(var.allowed_services)

  service            = each.value
  disable_on_destroy = false
}

# Service Account for deployment
resource "google_service_account" "erlmcp_deploy" {
  account_id   = var.service_account_name
  display_name = "ErlMCP Deployment Service Account"
  description  = "Service account for CI/CD deployment to Cloud Run"

  depends_on = [google_project_service.required_apis]
}

# IAM Role: Cloud Run Admin
resource "google_project_iam_member" "cloudrun_admin" {
  project = var.project_id
  role    = "roles/run.admin"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# IAM Role: Service Account User (for Cloud Run to use this account)
resource "google_project_iam_member" "service_account_user" {
  project = var.project_id
  role    = "roles/iam.serviceAccountUser"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# IAM Role: Artifact Registry Writer
resource "google_project_iam_member" "artifact_registry_writer" {
  project = var.project_id
  role    = "roles/artifactregistry.writer"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# IAM Role: Secret Manager Secret Accessor
resource "google_project_iam_member" "secret_accessor" {
  project = var.project_id
  role    = "roles/secretmanager.secretAccessor"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# IAM Role: Cloud KMS CryptoKey Encrypter/Decrypter
resource "google_project_iam_member" "kms_crypto_operator" {
  project = var.project_id
  role    = "roles/cloudkms.cryptoKeyEncrypterDecrypter"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# IAM Role: Cloud Build Editor
resource "google_project_iam_member" "cloudbuild_editor" {
  project = var.project_id
  role    = "roles/cloudbuild.builds.editor"
  member  = "serviceAccount:${google_service_account.erlmcp_deploy.email}"

  depends_on = [google_service_account.erlmcp_deploy]
}

# Artifact Registry Repository
resource "google_artifact_registry_repository" "erlmcp" {
  location      = var.region
  repository_id = "${replace(var.app_name, "_", "-")}-repo"
  description   = "Docker container repository for ErlMCP"
  format        = "DOCKER"

  depends_on = [google_project_service.required_apis]
}

# Cloud KMS Keyring
resource "google_kms_key_ring" "erlmcp" {
  name     = "${replace(var.app_name, "_", "-")}-keyring"
  location = var.region

  depends_on = [google_project_service.required_apis]
}

# Cloud KMS Crypto Key
resource "google_kms_crypto_key" "erlmcp" {
  name            = "${replace(var.app_name, "_", "-")}-key"
  key_ring        = google_kms_key_ring.erlmcp.id
  rotation_period = var.kms_key_rotation_period
  purpose         = "ENCRYPT_DECRYPT"

  lifecycle {
    prevent_destroy = true
  }
}

# Secret Manager: Database URL
resource "google_secret_manager_secret" "database_url" {
  secret_id = "erlmcp-database-url"

  replication {
    automatic = true
  }

  depends_on = [google_project_service.required_apis]
}

# Grant service account access to database URL secret
resource "google_secret_manager_secret_iam_member" "database_url_accessor" {
  secret_id = google_secret_manager_secret.database_url.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.erlmcp_deploy.email}"
}

# Secret Manager: API Keys
resource "google_secret_manager_secret" "api_keys" {
  secret_id = "erlmcp-api-keys"

  replication {
    automatic = true
  }

  depends_on = [google_project_service.required_apis]
}

# Grant service account access to API keys secret
resource "google_secret_manager_secret_iam_member" "api_keys_accessor" {
  secret_id = google_secret_manager_secret.api_keys.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.erlmcp_deploy.email}"
}

# Secret Manager: Deployment Credentials
resource "google_secret_manager_secret" "deployment_credentials" {
  secret_id = "erlmcp-deployment-creds"

  replication {
    automatic = true
  }

  depends_on = [google_project_service.required_apis]
}

# Grant service account access to deployment credentials
resource "google_secret_manager_secret_iam_member" "deployment_creds_accessor" {
  secret_id = google_secret_manager_secret.deployment_credentials.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.erlmcp_deploy.email}"
}

# Cloud Storage bucket for Terraform state (optional but recommended)
resource "google_storage_bucket" "terraform_state" {
  name          = "${var.project_id}-terraform-state"
  location      = var.region
  force_destroy = false

  versioning {
    enabled = true
  }

  uniform_bucket_level_access = true

  lifecycle {
    prevent_destroy = true
  }

  depends_on = [google_project_service.required_apis]
}

# Block public access to state bucket
resource "google_storage_bucket_iam_member" "state_bucket_only_sa" {
  bucket = google_storage_bucket.terraform_state.name
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:${google_service_account.erlmcp_deploy.email}"
}
