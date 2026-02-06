# Complete example: Multi-region Docker registry with CMEK encryption and cleanup policies

terraform {
  required_version = ">= 1.5"
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.0"
    }
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
}

# KMS Key Ring for encryption
resource "google_kms_key_ring" "artifact_registry" {
  name     = "artifact-registry-keyring"
  location = var.region
}

# KMS Crypto Key for CMEK
resource "google_kms_crypto_key" "artifact_registry" {
  name     = "artifact-registry-key"
  key_ring = google_kms_key_ring.artifact_registry.id
  purpose  = "ENCRYPT_DECRYPT"

  lifecycle {
    prevent_destroy = true
  }

  version_template {
    algorithm        = "GOOGLE_SYMMETRIC_ENCRYPTION"
    protection_level = "SOFTWARE"
  }

  rotation_period = "7776000s" # 90 days
}

# Production Docker Registry with multi-region replication
module "production_docker_registry" {
  source = "../../"

  project_id    = var.project_id
  location      = var.region
  repository_id = "production-docker"
  description   = "Production Docker images with multi-region replication"
  format        = "DOCKER"

  # Multi-region replication for high availability
  replica_locations = [
    "us-east1",
    "europe-west1",
    "asia-southeast1"
  ]

  # Customer-managed encryption key
  kms_key_name = google_kms_crypto_key.artifact_registry.id

  # Immutable tags for production
  docker_immutable_tags = true

  # Comprehensive cleanup policies
  cleanup_policies = [
    # Delete untagged images after 7 days
    {
      id     = "delete-untagged"
      action = "DELETE"
      condition = {
        tag_state  = "UNTAGGED"
        older_than = "604800s" # 7 days
      }
      most_recent_versions = null
    },
    # Delete development/feature images after 14 days
    {
      id     = "delete-dev-images"
      action = "DELETE"
      condition = {
        tag_prefixes = ["dev-", "feature-", "test-"]
        older_than   = "1209600s" # 14 days
      }
      most_recent_versions = null
    },
    # Delete staging images after 30 days
    {
      id     = "delete-staging-images"
      action = "DELETE"
      condition = {
        tag_prefixes = ["staging-"]
        older_than   = "2592000s" # 30 days
      }
      most_recent_versions = null
    },
    # Keep production images indefinitely but only last 100 versions
    {
      id     = "keep-production-versions"
      action = "KEEP"
      condition = {
        tag_prefixes = ["prod-", "release-", "v"]
      }
      most_recent_versions = {
        keep_count = 100
      }
    },
    # Keep at least 20 most recent versions of any image
    {
      id     = "keep-recent-any"
      action = "KEEP"
      condition = null
      most_recent_versions = {
        keep_count = 20
      }
    }
  ]

  # IAM bindings
  iam_bindings = {
    "roles/artifactregistry.reader" = [
      "serviceAccount:gke-nodes@${var.project_id}.iam.gserviceaccount.com",
      "serviceAccount:ci-cd@${var.project_id}.iam.gserviceaccount.com",
      "group:developers@example.com"
    ]
    "roles/artifactregistry.writer" = [
      "serviceAccount:ci-cd@${var.project_id}.iam.gserviceaccount.com"
    ]
    "roles/artifactregistry.admin" = [
      "group:platform-team@example.com"
    ]
  }

  labels = {
    environment = "production"
    managed-by  = "terraform"
    team        = "platform"
    cost-center = "engineering"
  }
}

# Development Docker Registry (single region, no CMEK)
module "development_docker_registry" {
  source = "../../"

  project_id    = var.project_id
  location      = var.region
  repository_id = "development-docker"
  description   = "Development Docker images"
  format        = "DOCKER"

  # Aggressive cleanup for dev environment
  cleanup_policies = [
    {
      id     = "delete-old-untagged"
      action = "DELETE"
      condition = {
        tag_state  = "UNTAGGED"
        older_than = "86400s" # 1 day
      }
      most_recent_versions = null
    },
    {
      id     = "keep-recent-only"
      action = "KEEP"
      condition = null
      most_recent_versions = {
        keep_count = 5
      }
    }
  ]

  iam_bindings = {
    "roles/artifactregistry.writer" = [
      "group:developers@example.com"
    ]
  }

  labels = {
    environment = "development"
    managed-by  = "terraform"
  }
}

# Maven Repository
module "maven_registry" {
  source = "../../"

  project_id    = var.project_id
  location      = var.region
  repository_id = "maven-releases"
  description   = "Maven release artifacts"
  format        = "MAVEN"

  maven_allow_snapshot_overwrites = false
  maven_version_policy            = "RELEASE"

  replica_locations = ["us-east1"]

  cleanup_policies = [
    {
      id     = "keep-releases"
      action = "KEEP"
      condition = null
      most_recent_versions = {
        keep_count = 50
      }
    }
  ]

  labels = {
    environment = "production"
    artifact-type = "maven"
  }
}

# Docker Hub Proxy (Remote Repository)
module "dockerhub_proxy" {
  source = "../../"

  project_id    = var.project_id
  location      = var.region
  repository_id = "dockerhub-proxy"
  description   = "Docker Hub proxy for faster pulls and cost reduction"
  format        = "DOCKER"
  mode          = "REMOTE_REPOSITORY"

  remote_repository_config = {
    description = "Proxy to Docker Hub"
    docker_repository = {
      public_repository = "DOCKER_HUB"
    }
  }

  labels = {
    proxy = "true"
    upstream = "dockerhub"
  }
}

# NPM Registry
module "npm_registry" {
  source = "../../"

  project_id    = var.project_id
  location      = var.region
  repository_id = "npm-packages"
  description   = "Private NPM packages"
  format        = "NPM"

  kms_key_name = google_kms_crypto_key.artifact_registry.id

  cleanup_policies = [
    {
      id     = "delete-old-versions"
      action = "DELETE"
      condition = {
        older_than = "7776000s" # 90 days
      }
      most_recent_versions = null
    },
    {
      id     = "keep-recent"
      action = "KEEP"
      condition = null
      most_recent_versions = {
        keep_count = 20
      }
    }
  ]

  labels = {
    artifact-type = "npm"
  }
}

# Outputs
output "production_docker_url" {
  value = module.production_docker_registry.repository_url
}

output "production_replicas" {
  value = module.production_docker_registry.all_repository_urls
}

output "development_docker_url" {
  value = module.development_docker_registry.repository_url
}

output "maven_url" {
  value = module.maven_registry.repository_url
}

output "dockerhub_proxy_url" {
  value = module.dockerhub_proxy.repository_url
}

output "npm_url" {
  value = module.npm_registry.repository_url
}
