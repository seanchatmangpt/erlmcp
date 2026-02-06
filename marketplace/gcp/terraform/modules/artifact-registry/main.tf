terraform {
  required_version = ">= 1.5"
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.0"
    }
  }
}

# Primary Artifact Registry Repository
resource "google_artifact_registry_repository" "main" {
  location      = var.location
  repository_id = var.repository_id
  description   = var.description
  format        = var.format
  project       = var.project_id

  mode = var.mode

  # Customer-Managed Encryption Key
  kms_key_name = var.kms_key_name

  # Cleanup Policies
  dynamic "cleanup_policies" {
    for_each = var.cleanup_policies
    content {
      id     = cleanup_policies.value.id
      action = cleanup_policies.value.action

      dynamic "condition" {
        for_each = cleanup_policies.value.condition != null ? [cleanup_policies.value.condition] : []
        content {
          tag_state             = lookup(condition.value, "tag_state", null)
          tag_prefixes          = lookup(condition.value, "tag_prefixes", null)
          version_name_prefixes = lookup(condition.value, "version_name_prefixes", null)
          package_name_prefixes = lookup(condition.value, "package_name_prefixes", null)
          older_than            = lookup(condition.value, "older_than", null)
          newer_than            = lookup(condition.value, "newer_than", null)
        }
      }

      dynamic "most_recent_versions" {
        for_each = cleanup_policies.value.most_recent_versions != null ? [cleanup_policies.value.most_recent_versions] : []
        content {
          package_name_prefixes = lookup(most_recent_versions.value, "package_name_prefixes", null)
          keep_count            = lookup(most_recent_versions.value, "keep_count", null)
        }
      }
    }
  }

  # Docker Configuration
  dynamic "docker_config" {
    for_each = var.format == "DOCKER" && var.docker_immutable_tags ? [1] : []
    content {
      immutable_tags = var.docker_immutable_tags
    }
  }

  # Maven Configuration
  dynamic "maven_config" {
    for_each = var.format == "MAVEN" && var.maven_allow_snapshot_overwrites != null ? [1] : []
    content {
      allow_snapshot_overwrites = var.maven_allow_snapshot_overwrites
      version_policy            = var.maven_version_policy
    }
  }

  # Virtual Repository Configuration
  dynamic "virtual_repository_config" {
    for_each = var.mode == "VIRTUAL_REPOSITORY" && length(var.upstream_repositories) > 0 ? [1] : []
    content {
      dynamic "upstream_policies" {
        for_each = var.upstream_repositories
        content {
          id         = upstream_policies.value.id
          repository = upstream_policies.value.repository
          priority   = upstream_policies.value.priority
        }
      }
    }
  }

  # Remote Repository Configuration
  dynamic "remote_repository_config" {
    for_each = var.mode == "REMOTE_REPOSITORY" && var.remote_repository_config != null ? [1] : []
    content {
      description = var.remote_repository_config.description

      dynamic "docker_repository" {
        for_each = var.format == "DOCKER" && var.remote_repository_config.docker_repository != null ? [1] : []
        content {
          public_repository = var.remote_repository_config.docker_repository.public_repository

          dynamic "custom_repository" {
            for_each = var.remote_repository_config.docker_repository.custom_repository != null ? [1] : []
            content {
              uri = var.remote_repository_config.docker_repository.custom_repository.uri
            }
          }
        }
      }

      dynamic "maven_repository" {
        for_each = var.format == "MAVEN" && var.remote_repository_config.maven_repository != null ? [1] : []
        content {
          public_repository = var.remote_repository_config.maven_repository.public_repository

          dynamic "custom_repository" {
            for_each = var.remote_repository_config.maven_repository.custom_repository != null ? [1] : []
            content {
              uri = var.remote_repository_config.maven_repository.custom_repository.uri
            }
          }
        }
      }

      dynamic "npm_repository" {
        for_each = var.format == "NPM" && var.remote_repository_config.npm_repository != null ? [1] : []
        content {
          public_repository = var.remote_repository_config.npm_repository.public_repository

          dynamic "custom_repository" {
            for_each = var.remote_repository_config.npm_repository.custom_repository != null ? [1] : []
            content {
              uri = var.remote_repository_config.npm_repository.custom_repository.uri
            }
          }
        }
      }

      dynamic "python_repository" {
        for_each = var.format == "PYTHON" && var.remote_repository_config.python_repository != null ? [1] : []
        content {
          public_repository = var.remote_repository_config.python_repository.public_repository

          dynamic "custom_repository" {
            for_each = var.remote_repository_config.python_repository.custom_repository != null ? [1] : []
            content {
              uri = var.remote_repository_config.python_repository.custom_repository.uri
            }
          }
        }
      }

      dynamic "apt_repository" {
        for_each = var.format == "APT" && var.remote_repository_config.apt_repository != null ? [1] : []
        content {
          public_repository {
            repository_base = var.remote_repository_config.apt_repository.public_repository.repository_base
            repository_path = var.remote_repository_config.apt_repository.public_repository.repository_path
          }
        }
      }

      dynamic "yum_repository" {
        for_each = var.format == "YUM" && var.remote_repository_config.yum_repository != null ? [1] : []
        content {
          public_repository {
            repository_base = var.remote_repository_config.yum_repository.public_repository.repository_base
            repository_path = var.remote_repository_config.yum_repository.public_repository.repository_path
          }
        }
      }

      dynamic "upstream_credentials" {
        for_each = var.remote_repository_config.upstream_credentials != null ? [1] : []
        content {
          dynamic "username_password_credentials" {
            for_each = var.remote_repository_config.upstream_credentials.username_password_credentials != null ? [1] : []
            content {
              username                = var.remote_repository_config.upstream_credentials.username_password_credentials.username
              password_secret_version = var.remote_repository_config.upstream_credentials.username_password_credentials.password_secret_version
            }
          }
        }
      }
    }
  }

  labels = var.labels
}

# Multi-Region Replication
resource "google_artifact_registry_repository" "replicas" {
  for_each = toset(var.replica_locations)

  location      = each.key
  repository_id = var.repository_id
  description   = "${var.description} - Replica in ${each.key}"
  format        = var.format
  project       = var.project_id

  mode = var.mode

  # Use same encryption key (must be in same region or multi-region)
  kms_key_name = var.kms_key_name

  # Replicate cleanup policies
  dynamic "cleanup_policies" {
    for_each = var.cleanup_policies
    content {
      id     = cleanup_policies.value.id
      action = cleanup_policies.value.action

      dynamic "condition" {
        for_each = cleanup_policies.value.condition != null ? [cleanup_policies.value.condition] : []
        content {
          tag_state             = lookup(condition.value, "tag_state", null)
          tag_prefixes          = lookup(condition.value, "tag_prefixes", null)
          version_name_prefixes = lookup(condition.value, "version_name_prefixes", null)
          package_name_prefixes = lookup(condition.value, "package_name_prefixes", null)
          older_than            = lookup(condition.value, "older_than", null)
          newer_than            = lookup(condition.value, "newer_than", null)
        }
      }

      dynamic "most_recent_versions" {
        for_each = cleanup_policies.value.most_recent_versions != null ? [cleanup_policies.value.most_recent_versions] : []
        content {
          package_name_prefixes = lookup(most_recent_versions.value, "package_name_prefixes", null)
          keep_count            = lookup(most_recent_versions.value, "keep_count", null)
        }
      }
    }
  }

  dynamic "docker_config" {
    for_each = var.format == "DOCKER" && var.docker_immutable_tags ? [1] : []
    content {
      immutable_tags = var.docker_immutable_tags
    }
  }

  labels = merge(var.labels, {
    replica = "true"
    primary-location = var.location
  })
}

# IAM Bindings
resource "google_artifact_registry_repository_iam_binding" "bindings" {
  for_each = var.iam_bindings

  project    = var.project_id
  location   = google_artifact_registry_repository.main.location
  repository = google_artifact_registry_repository.main.name
  role       = each.key
  members    = each.value
}

# IAM Bindings for Replicas
resource "google_artifact_registry_repository_iam_binding" "replica_bindings" {
  for_each = {
    for pair in flatten([
      for location, repo in google_artifact_registry_repository.replicas : [
        for role, members in var.iam_bindings : {
          key      = "${location}-${role}"
          location = location
          repo     = repo.name
          role     = role
          members  = members
        }
      ]
    ]) : pair.key => pair
  }

  project    = var.project_id
  location   = each.value.location
  repository = each.value.repo
  role       = each.value.role
  members    = each.value.members
}

# KMS Crypto Key IAM for Artifact Registry Service Account
data "google_project" "project" {
  project_id = var.project_id
}

resource "google_kms_crypto_key_iam_member" "artifact_registry" {
  count = var.kms_key_name != null ? 1 : 0

  crypto_key_id = var.kms_key_name
  role          = "roles/cloudkms.cryptoKeyEncrypterDecrypter"
  member        = "serviceAccount:service-${data.google_project.project.number}@gcp-sa-artifactregistry.iam.gserviceaccount.com"
}
