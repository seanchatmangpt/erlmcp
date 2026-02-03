# ============================================================================
# GCP Simulation - Terraform Version Constraints
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION
#
# This file documents the minimum required versions for all providers
# used in the GCP simulation stack.
# ============================================================================

terraform {
  required_version = ">= 1.0.0, < 2.0.0"

  required_providers {
    minio = {
      source  = "aminueza/minio"
      version = ">= 2.0.0, < 3.0.0"
    }

    postgresql = {
      source  = "cyrilgdn/postgresql"
      version = ">= 1.21.0, < 2.0.0"
    }

    vault = {
      source  = "hashicorp/vault"
      version = ">= 3.0.0, < 5.0.0"
    }

    docker = {
      source  = "kreuzwerker/docker"
      version = ">= 3.0.0, < 4.0.0"
    }

    random = {
      source  = "hashicorp/random"
      version = ">= 3.0.0, < 4.0.0"
    }

    local = {
      source  = "hashicorp/local"
      version = ">= 2.0.0, < 3.0.0"
    }
  }
}
