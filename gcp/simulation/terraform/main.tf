# ============================================================================
# GCP Simulation - Terraform Local Configuration
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION
#
# This Terraform configuration uses local providers and the simulation
# Docker Compose stack to emulate GCP resources for development/testing.
#
# It creates "fake" resources that point to local Docker containers:
#   - MinIO (GCS simulation)
#   - PostgreSQL (Cloud SQL simulation)
#   - Vault (Secret Manager/KMS simulation)
#   - Local Docker Registry (Artifact Registry simulation)
# ============================================================================

terraform {
  required_version = ">= 1.0"

  # Use local backend for simulation (no remote state)
  backend "local" {
    path = "terraform.tfstate"
  }

  required_providers {
    # MinIO provider for Cloud Storage simulation
    minio = {
      source  = "aminueza/minio"
      version = "~> 2.0"
    }

    # PostgreSQL provider for Cloud SQL simulation
    postgresql = {
      source  = "cyrilgdn/postgresql"
      version = "~> 1.21"
    }

    # Vault provider for Secret Manager/KMS simulation
    vault = {
      source  = "hashicorp/vault"
      version = "~> 3.0"
    }

    # Docker provider for container management
    docker = {
      source  = "kreuzwerker/docker"
      version = "~> 3.0"
    }

    # Random provider for generating secrets
    random = {
      source  = "hashicorp/random"
      version = "~> 3.0"
    }

    # Local provider for file operations
    local = {
      source  = "hashicorp/local"
      version = "~> 2.0"
    }
  }
}

# ============================================================================
# Variables
# ============================================================================

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "simulation"
}

variable "minio_endpoint" {
  description = "MinIO endpoint URL"
  type        = string
  default     = "localhost:9000"
}

variable "minio_access_key" {
  description = "MinIO access key"
  type        = string
  default     = "erlmcp_storage"
}

variable "minio_secret_key" {
  description = "MinIO secret key"
  type        = string
  default     = "erlmcp_storage_secret"
  sensitive   = true
}

variable "postgres_host" {
  description = "PostgreSQL host"
  type        = string
  default     = "localhost"
}

variable "postgres_port" {
  description = "PostgreSQL port"
  type        = number
  default     = 5432
}

variable "postgres_user" {
  description = "PostgreSQL user"
  type        = string
  default     = "erlmcp"
}

variable "postgres_password" {
  description = "PostgreSQL password"
  type        = string
  default     = "erlmcp_secret"
  sensitive   = true
}

variable "vault_address" {
  description = "Vault server address"
  type        = string
  default     = "http://localhost:8200"
}

variable "vault_token" {
  description = "Vault root token"
  type        = string
  default     = "erlmcp_vault_token"
  sensitive   = true
}

# ============================================================================
# Locals
# ============================================================================

locals {
  common_labels = {
    environment = var.environment
    managed_by  = "terraform"
    application = "erlmcp"
    simulation  = "true"
  }

  buckets = [
    "erlmcp-terraform-state",
    "erlmcp-artifacts",
    "erlmcp-backups",
    "erlmcp-logs",
    "erlmcp-uploads"
  ]

  databases = [
    "erlmcp_dev",
    "erlmcp_test",
    "erlmcp_staging"
  ]

  secrets = {
    "database-url" = "postgresql://${var.postgres_user}:${var.postgres_password}@${var.postgres_host}:${var.postgres_port}/erlmcp_dev"
    "api-key"      = random_password.api_key.result
    "jwt-secret"   = random_password.jwt_secret.result
  }
}

# ============================================================================
# Provider Configuration
# ============================================================================

provider "minio" {
  minio_server   = var.minio_endpoint
  minio_user     = var.minio_access_key
  minio_password = var.minio_secret_key
  minio_ssl      = false
}

provider "postgresql" {
  host     = var.postgres_host
  port     = var.postgres_port
  username = var.postgres_user
  password = var.postgres_password
  database = "erlmcp_dev"
  sslmode  = "disable"
}

provider "vault" {
  address = var.vault_address
  token   = var.vault_token
}

provider "docker" {
  host = "unix:///var/run/docker.sock"
}

# ============================================================================
# Random Secrets Generation
# ============================================================================

resource "random_password" "api_key" {
  length  = 32
  special = false
}

resource "random_password" "jwt_secret" {
  length  = 64
  special = true
}

resource "random_password" "encryption_key" {
  length  = 32
  special = false
}

# ============================================================================
# Cloud Storage Simulation (MinIO Buckets)
# ============================================================================

resource "minio_s3_bucket" "buckets" {
  for_each = toset(local.buckets)

  bucket = each.value
  acl    = "private"
}

# Bucket lifecycle policies
resource "minio_s3_bucket_versioning" "terraform_state" {
  bucket = minio_s3_bucket.buckets["erlmcp-terraform-state"].bucket

  versioning_configuration {
    status = "Enabled"
  }
}

# ============================================================================
# Cloud SQL Simulation (PostgreSQL Databases)
# ============================================================================

resource "postgresql_database" "databases" {
  for_each = toset(local.databases)

  name              = each.value
  owner             = var.postgres_user
  encoding          = "UTF8"
  lc_collate        = "en_US.UTF-8"
  lc_ctype          = "en_US.UTF-8"
  connection_limit  = 100
  allow_connections = true
}

resource "postgresql_schema" "erlmcp" {
  for_each = toset(local.databases)

  name     = "erlmcp"
  database = each.value
  owner    = var.postgres_user

  depends_on = [postgresql_database.databases]
}

# ============================================================================
# Secret Manager Simulation (Vault Secrets)
# ============================================================================

resource "vault_mount" "kv" {
  path        = "erlmcp"
  type        = "kv-v2"
  description = "ErlMCP secrets (simulates GCP Secret Manager)"
}

resource "vault_kv_secret_v2" "secrets" {
  for_each = local.secrets

  mount = vault_mount.kv.path
  name  = each.key
  data_json = jsonencode({
    value = each.value
  })

  depends_on = [vault_mount.kv]
}

# ============================================================================
# Cloud KMS Simulation (Vault Transit)
# ============================================================================

resource "vault_mount" "transit" {
  path        = "transit"
  type        = "transit"
  description = "ErlMCP encryption keys (simulates GCP Cloud KMS)"
}

resource "vault_transit_secret_backend_key" "app_key" {
  backend          = vault_mount.transit.path
  name             = "erlmcp-app-key"
  type             = "aes256-gcm96"
  deletion_allowed = true
  exportable       = false
  allow_plaintext_backup = false

  depends_on = [vault_mount.transit]
}

# ============================================================================
# Local Configuration Files
# ============================================================================

resource "local_file" "env_file" {
  filename = "${path.module}/../.env.simulation"
  content  = <<-EOT
    # GCP Simulation Environment Variables
    # Generated by Terraform - DO NOT EDIT MANUALLY

    # Environment
    ERLMCP_ENV=simulation
    GCP_PROJECT=erlmcp-local

    # Cloud SQL (PostgreSQL)
    DATABASE_URL=postgresql://${var.postgres_user}:${var.postgres_password}@${var.postgres_host}:${var.postgres_port}/erlmcp_dev
    POSTGRES_HOST=${var.postgres_host}
    POSTGRES_PORT=${var.postgres_port}
    POSTGRES_USER=${var.postgres_user}
    POSTGRES_PASSWORD=${var.postgres_password}

    # Cloud Storage (MinIO)
    STORAGE_ENDPOINT=http://${var.minio_endpoint}
    STORAGE_ACCESS_KEY=${var.minio_access_key}
    STORAGE_SECRET_KEY=${var.minio_secret_key}
    STORAGE_BUCKET=erlmcp-artifacts

    # Secret Manager (Vault)
    VAULT_ADDR=${var.vault_address}
    VAULT_TOKEN=${var.vault_token}

    # Pub/Sub Emulator
    PUBSUB_EMULATOR_HOST=localhost:8085

    # Firestore Emulator
    FIRESTORE_EMULATOR_HOST=localhost:8086

    # Artifact Registry (Local)
    DOCKER_REGISTRY=localhost:5000

    # Monitoring
    PROMETHEUS_URL=http://localhost:9090
    GRAFANA_URL=http://localhost:3000

    # Generated Secrets
    API_KEY=${random_password.api_key.result}
    JWT_SECRET=${random_password.jwt_secret.result}
    ENCRYPTION_KEY=${random_password.encryption_key.result}
  EOT

  file_permission = "0600"
}

resource "local_file" "gcp_service_mapping" {
  filename = "${path.module}/../config/gcp-service-mapping.json"
  content = jsonencode({
    project_id  = "erlmcp-local"
    environment = var.environment
    services = {
      cloudsql = {
        local_endpoint = "${var.postgres_host}:${var.postgres_port}"
        databases      = local.databases
        type           = "postgresql"
      }
      cloudstorage = {
        local_endpoint = "http://${var.minio_endpoint}"
        buckets        = local.buckets
        type           = "minio"
      }
      secretmanager = {
        local_endpoint = var.vault_address
        secrets        = keys(local.secrets)
        type           = "vault"
      }
      cloudkms = {
        local_endpoint = var.vault_address
        keys           = ["erlmcp-app-key"]
        type           = "vault-transit"
      }
      pubsub = {
        local_endpoint = "localhost:8085"
        type           = "emulator"
      }
      firestore = {
        local_endpoint = "localhost:8086"
        type           = "emulator"
      }
      artifactregistry = {
        local_endpoint = "localhost:5000"
        type           = "registry"
      }
      monitoring = {
        prometheus = "http://localhost:9090"
        grafana    = "http://localhost:3000"
        loki       = "http://localhost:3100"
        type       = "prometheus-stack"
      }
    }
  })
}

# ============================================================================
# Outputs
# ============================================================================

output "environment" {
  description = "Environment name"
  value       = var.environment
}

output "database_url" {
  description = "PostgreSQL connection string"
  value       = "postgresql://${var.postgres_user}:****@${var.postgres_host}:${var.postgres_port}/erlmcp_dev"
}

output "storage_endpoint" {
  description = "MinIO/GCS endpoint"
  value       = "http://${var.minio_endpoint}"
}

output "vault_address" {
  description = "Vault/Secret Manager address"
  value       = var.vault_address
}

output "buckets" {
  description = "Created storage buckets"
  value       = [for b in minio_s3_bucket.buckets : b.bucket]
}

output "databases" {
  description = "Created databases"
  value       = [for d in postgresql_database.databases : d.name]
}

output "secrets" {
  description = "Created secrets"
  value       = keys(local.secrets)
}

output "env_file" {
  description = "Path to generated environment file"
  value       = local_file.env_file.filename
}

output "gcp_mapping_file" {
  description = "Path to GCP service mapping file"
  value       = local_file.gcp_service_mapping.filename
}
