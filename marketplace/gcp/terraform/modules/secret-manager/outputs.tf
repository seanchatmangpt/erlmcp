# ============================================================================
# Secret Manager Module Outputs
# ============================================================================

# ============================================================================
# Secret IDs and Names
# ============================================================================
output "secret_ids" {
  description = "Map of secret names to their full resource IDs"
  value = {
    erlang_cookie    = google_secret_manager_secret.erlang_cookie.id
    db_password      = google_secret_manager_secret.db_password.id
    redis_password   = google_secret_manager_secret.redis_password.id
    tls_cert         = google_secret_manager_secret.tls_cert.id
    tls_key          = google_secret_manager_secret.tls_key.id
    ca_bundle        = google_secret_manager_secret.ca_bundle.id
    jwt_private_key  = google_secret_manager_secret.jwt_private_key.id
    jwt_public_key   = google_secret_manager_secret.jwt_public_key.id
    grafana_password = google_secret_manager_secret.grafana_password.id
    backup_key       = google_secret_manager_secret.backup_key.id
    otel_ca_cert     = google_secret_manager_secret.otel_ca_cert.id
  }
}

output "secret_names" {
  description = "Map of secret logical names to their secret_id"
  value = {
    erlang_cookie    = google_secret_manager_secret.erlang_cookie.secret_id
    db_password      = google_secret_manager_secret.db_password.secret_id
    redis_password   = google_secret_manager_secret.redis_password.secret_id
    tls_cert         = google_secret_manager_secret.tls_cert.secret_id
    tls_key          = google_secret_manager_secret.tls_key.secret_id
    ca_bundle        = google_secret_manager_secret.ca_bundle.secret_id
    jwt_private_key  = google_secret_manager_secret.jwt_private_key.secret_id
    jwt_public_key   = google_secret_manager_secret.jwt_public_key.secret_id
    grafana_password = google_secret_manager_secret.grafana_password.secret_id
    backup_key       = google_secret_manager_secret.backup_key.secret_id
    otel_ca_cert     = google_secret_manager_secret.otel_ca_cert.secret_id
  }
}

# ============================================================================
# Secret Version Resource Names (for GKE/Cloud Run mounting)
# ============================================================================
output "secret_version_names" {
  description = "Map of secret names to their latest version resource names (for mounting)"
  value = {
    erlang_cookie    = "${google_secret_manager_secret.erlang_cookie.id}/versions/latest"
    db_password      = "${google_secret_manager_secret.db_password.id}/versions/latest"
    redis_password   = "${google_secret_manager_secret.redis_password.id}/versions/latest"
    tls_cert         = "${google_secret_manager_secret.tls_cert.id}/versions/latest"
    tls_key          = "${google_secret_manager_secret.tls_key.id}/versions/latest"
    ca_bundle        = "${google_secret_manager_secret.ca_bundle.id}/versions/latest"
    jwt_private_key  = "${google_secret_manager_secret.jwt_private_key.id}/versions/latest"
    jwt_public_key   = "${google_secret_manager_secret.jwt_public_key.id}/versions/latest"
    grafana_password = "${google_secret_manager_secret.grafana_password.id}/versions/latest"
    backup_key       = "${google_secret_manager_secret.backup_key.id}/versions/latest"
    otel_ca_cert     = "${google_secret_manager_secret.otel_ca_cert.id}/versions/latest"
  }
}

# ============================================================================
# Encryption Configuration
# ============================================================================
output "encryption_config" {
  description = "Encryption configuration details"
  value = {
    cmek_enabled   = var.enable_cmek
    kms_key_id     = var.enable_cmek ? google_kms_crypto_key.secret_encryption[0].id : null
    kms_key_ring   = var.enable_cmek ? google_kms_key_ring.secret_manager[0].id : null
    kms_location   = var.kms_location
  }
}

# ============================================================================
# Rotation Configuration
# ============================================================================
output "rotation_config" {
  description = "Rotation configuration details"
  value = {
    rotation_enabled          = var.enable_rotation
    rotation_notifications    = var.enable_rotation_notifications
    rotation_topic           = var.enable_rotation_notifications ? google_pubsub_topic.secret_rotation[0].id : null
    rotation_periods = {
      critical = var.rotation_periods.critical
      high     = var.rotation_periods.high
      crypto   = var.rotation_periods.crypto
    }
  }
}

# ============================================================================
# Security and Compliance Metadata
# ============================================================================
output "security_metadata" {
  description = "Security and compliance configuration summary"
  value = {
    audit_logging_enabled      = var.enable_audit_logging
    vpc_service_controls       = var.enable_vpc_service_controls
    replication_locations      = var.secret_replication_locations
    total_secrets              = 11
    critical_secrets           = 5  # erlang_cookie, tls_key, jwt_private_key, backup_key, db_password
    high_secrets               = 4  # db_password, redis_password, ca_bundle, grafana_password
    medium_secrets             = 2  # jwt_public_key, otel_ca_cert
  }
}

# ============================================================================
# Secret Classifications
# ============================================================================
output "secret_classifications" {
  description = "Secret classifications and data classes"
  value = {
    critical = {
      erlang_cookie   = "cluster-auth"
      tls_key         = "private-key"
      jwt_private_key = "jwt-signing-key"
      backup_key      = "backup-encryption"
      tls_cert        = "tls-cert"
    }
    high = {
      db_password      = "database-auth"
      redis_password   = "cache-auth"
      grafana_password = "monitoring-auth"
      ca_bundle        = "ca-cert"
      otel_ca_cert     = "observability-cert"
    }
    medium = {
      jwt_public_key = "jwt-public-key"
    }
  }
}

# ============================================================================
# IAM Summary
# ============================================================================
output "iam_summary" {
  description = "Summary of IAM access configurations"
  value = {
    cluster_accounts       = length(var.cluster_service_accounts)
    database_accounts      = length(var.database_service_accounts)
    cache_accounts         = length(var.cache_service_accounts)
    tls_accounts           = length(var.tls_service_accounts)
    auth_accounts          = length(var.auth_service_accounts)
    app_accounts           = length(var.app_service_accounts)
    monitoring_accounts    = length(var.monitoring_service_accounts)
    backup_accounts        = length(var.backup_service_accounts)
    observability_accounts = length(var.observability_service_accounts)
    viewers                = length(var.secret_viewers)
    administrators         = length(var.secret_administrators)
  }
}

# ============================================================================
# GKE SecretProviderClass Configuration
# ============================================================================
output "gke_secret_provider_class_spec" {
  description = "Configuration for GKE Workload Identity Secret Store CSI Driver"
  value = {
    provider = "gcp"
    parameters = {
      secrets = join("|", [
        "objectName=${google_secret_manager_secret.erlang_cookie.secret_id}|objectType=secret|objectVersion=latest",
        "objectName=${google_secret_manager_secret.db_password.secret_id}|objectType=secret|objectVersion=latest",
        "objectName=${google_secret_manager_secret.redis_password.secret_id}|objectType=secret|objectVersion=latest",
        "objectName=${google_secret_manager_secret.tls_cert.secret_id}|objectType=secret|objectVersion=latest",
        "objectName=${google_secret_manager_secret.tls_key.secret_id}|objectType=secret|objectVersion=latest",
        "objectName=${google_secret_manager_secret.ca_bundle.secret_id}|objectType=secret|objectVersion=latest",
        "objectName=${google_secret_manager_secret.jwt_private_key.secret_id}|objectType=secret|objectVersion=latest",
        "objectName=${google_secret_manager_secret.jwt_public_key.secret_id}|objectType=secret|objectVersion=latest",
      ])
    }
  }
}

# ============================================================================
# Cloud Run Secret Configuration
# ============================================================================
output "cloud_run_secret_env_vars" {
  description = "Cloud Run environment variable configuration for secrets"
  value = [
    {
      name = "ERLANG_COOKIE"
      value_source = {
        secret_key_ref = {
          secret  = google_secret_manager_secret.erlang_cookie.secret_id
          version = "latest"
        }
      }
    },
    {
      name = "DB_PASSWORD"
      value_source = {
        secret_key_ref = {
          secret  = google_secret_manager_secret.db_password.secret_id
          version = "latest"
        }
      }
    },
    {
      name = "REDIS_PASSWORD"
      value_source = {
        secret_key_ref = {
          secret  = google_secret_manager_secret.redis_password.secret_id
          version = "latest"
        }
      }
    },
  ]
}

# ============================================================================
# Secret Access Instructions
# ============================================================================
output "access_instructions" {
  description = "Instructions for accessing secrets from different environments"
  value = {
    gcloud_command = "gcloud secrets versions access latest --secret=<SECRET_ID> --project=${var.project_id}"
    terraform_data = "data.google_secret_manager_secret_version.<name>.secret_data"
    gke_mount_path = "/secrets/<secret-name>"
    cloud_run_env  = "Mounted as environment variable"
  }
}
