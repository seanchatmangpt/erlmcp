# ==============================================================================
# Terraform Remote State Backend (GCS)
# ==============================================================================
# This file configures remote state storage in Google Cloud Storage.
# Remote state is essential for:
#   - Team collaboration (shared state)
#   - State locking (prevent concurrent modifications)
#   - State versioning (rollback capability)
#   - Secure storage (encrypted at rest)
#
# SETUP INSTRUCTIONS:
# 1. First, bootstrap the state bucket by running terraform with local state:
#    terraform init (without backend block)
#    terraform apply -target=google_storage_bucket.terraform_state
#
# 2. Then enable this backend and migrate state:
#    terraform init -migrate-state
#
# 3. Verify state was migrated successfully
# ==============================================================================

terraform {
  backend "gcs" {
    # Bucket name format: ${project_id}-terraform-state
    # This bucket is created by the google_storage_bucket.terraform_state resource
    bucket = "taiea-v1-terraform-state"

    # State file path prefix (allows multiple environments in same bucket)
    prefix = "erlmcp/prod"

    # Enable state locking (prevents concurrent modifications)
    # Requires Object Admin permission on the bucket

    # Encryption (GCS encrypts at rest by default, but you can use CMEK)
    # encryption_key = "projects/PROJECT_ID/locations/LOCATION/keyRings/KEYRING/cryptoKeys/KEY"
  }
}

# ==============================================================================
# Backend Configuration for Different Environments
# ==============================================================================
# For multiple environments, use different state prefixes:
#
# Development:
#   terraform init -backend-config="prefix=erlmcp/dev"
#
# Staging:
#   terraform init -backend-config="prefix=erlmcp/staging"
#
# Production:
#   terraform init -backend-config="prefix=erlmcp/prod"
#
# Or use separate .tfbackend files:
#   terraform init -backend-config=backend.dev.tfbackend
# ==============================================================================
