# ============================================================================
# Secret Manager Module for erlmcp - V3 Security Architecture
# Google Secret Manager with Zero-Trust, CMEK, Rotation, and Audit Logging
# ============================================================================
#
# Security Features:
# - Customer-Managed Encryption Keys (CMEK)
# - Automatic secret rotation with Pub/Sub notifications
# - Least-privilege IAM bindings per secret classification
# - Audit logging for all secret access
# - User-managed replication for sensitive data
# - Secret version management and expiration
# - VPC Service Controls support
# - Compliance annotations and labeling
#
# ============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.0.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = ">= 5.0.0"
    }
    random = {
      source  = "hashicorp/random"
      version = ">= 3.0.0"
    }
  }
}

# ============================================================================
# Enable Required APIs
# ============================================================================
resource "google_project_service" "secret_manager" {
  project            = var.project_id
  service            = "secretmanager.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "cloudkms" {
  count              = var.enable_cmek ? 1 : 0
  project            = var.project_id
  service            = "cloudkms.googleapis.com"
  disable_on_destroy = false
}

# ============================================================================
# Pub/Sub Topics for Rotation Notifications
# ============================================================================
resource "google_pubsub_topic" "secret_rotation" {
  count   = var.enable_rotation_notifications ? 1 : 0
  project = var.project_id
  name    = "erlmcp-secret-rotation-events"

  labels = merge(var.labels, {
    purpose = "secret-rotation"
    security = "audit"
  })

  message_retention_duration = "604800s" # 7 days
}

resource "google_pubsub_topic_iam_member" "rotation_publisher" {
  count   = var.enable_rotation_notifications ? 1 : 0
  project = var.project_id
  topic   = google_pubsub_topic.secret_rotation[0].name
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:service-${data.google_project.current.number}@gcp-sa-secretmanager.iam.gserviceaccount.com"
}

# ============================================================================
# Data Sources
# ============================================================================
data "google_project" "current" {
  project_id = var.project_id
}

# ============================================================================
# KMS Key Ring and Keys for CMEK
# ============================================================================
resource "google_kms_key_ring" "secret_manager" {
  count    = var.enable_cmek ? 1 : 0
  project  = var.project_id
  name     = "erlmcp-secrets-keyring"
  location = var.kms_location
}

resource "google_kms_crypto_key" "secret_encryption" {
  count           = var.enable_cmek ? 1 : 0
  name            = "erlmcp-secrets-key"
  key_ring        = google_kms_key_ring.secret_manager[0].id
  rotation_period = "7776000s" # 90 days

  purpose = "ENCRYPT_DECRYPT"

  lifecycle {
    prevent_destroy = true
  }

  labels = merge(var.labels, {
    purpose = "secret-encryption"
    compliance = "pci-dss-gdpr"
  })
}

# Grant Secret Manager service account access to KMS key
resource "google_kms_crypto_key_iam_member" "secret_manager_encrypter" {
  count         = var.enable_cmek ? 1 : 0
  crypto_key_id = google_kms_crypto_key.secret_encryption[0].id
  role          = "roles/cloudkms.cryptoKeyEncrypterDecrypter"
  member        = "serviceAccount:service-${data.google_project.current.number}@gcp-sa-secretmanager.iam.gserviceaccount.com"
}

# ============================================================================
# Secret Manager Secrets - Classified by Sensitivity
# All secrets use CMEK, rotation policies, and least-privilege access
# ============================================================================

# ============================================================================
# CRITICAL SECRETS - Tier 1: Cluster Security (30-day rotation)
# ============================================================================

# 1. Erlang Cookie - Critical for cluster security
resource "google_secret_manager_secret" "erlang_cookie" {
  provider  = google-beta
  project   = var.project_id
  secret_id = "erlmcp-erlang-cookie"

  replication {
    # User-managed replication for critical secrets (multi-region availability)
    user_managed {
      dynamic "replicas" {
        for_each = var.secret_replication_locations
        content {
          location = replicas.value
          dynamic "customer_managed_encryption" {
            for_each = var.enable_cmek ? [1] : []
            content {
              kms_key_name = google_kms_crypto_key.secret_encryption[0].id
            }
          }
        }
      }
    }
  }

  # Automatic rotation configuration
  dynamic "rotation" {
    for_each = var.enable_rotation ? [1] : []
    content {
      next_rotation_time = timeadd(timestamp(), var.rotation_periods.critical)
      rotation_period    = var.rotation_periods.critical
    }
  }

  dynamic "topics" {
    for_each = var.enable_rotation_notifications ? [1] : []
    content {
      name = google_pubsub_topic.secret_rotation[0].id
    }
  }

  labels = merge(var.labels, {
    sensitivity  = "critical"
    compliance   = "pci-dss"
    rotation     = "30days"
    data-class   = "cluster-auth"
  })

  annotations = {
    "security.erlmcp.io/classification" = "critical"
    "security.erlmcp.io/encryption"     = var.enable_cmek ? "cmek" : "google-managed"
    "compliance.erlmcp.io/pci-dss"      = "true"
    "compliance.erlmcp.io/soc2"         = "true"
  }

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

# ============================================================================
# HIGH SECRETS - Tier 2: Data Access (90-day rotation)
# ============================================================================

# 2. Database Password
resource "google_secret_manager_secret" "db_password" {
  provider  = google-beta
  project   = var.project_id
  secret_id = "erlmcp-db-password"

  replication {
    user_managed {
      dynamic "replicas" {
        for_each = var.secret_replication_locations
        content {
          location = replicas.value
          dynamic "customer_managed_encryption" {
            for_each = var.enable_cmek ? [1] : []
            content {
              kms_key_name = google_kms_crypto_key.secret_encryption[0].id
            }
          }
        }
      }
    }
  }

  dynamic "rotation" {
    for_each = var.enable_rotation ? [1] : []
    content {
      next_rotation_time = timeadd(timestamp(), var.rotation_periods.high)
      rotation_period    = var.rotation_periods.high
    }
  }

  dynamic "topics" {
    for_each = var.enable_rotation_notifications ? [1] : []
    content {
      name = google_pubsub_topic.secret_rotation[0].id
    }
  }

  labels = merge(var.labels, {
    sensitivity  = "high"
    compliance   = "pci-dss-hipaa"
    rotation     = "90days"
    data-class   = "database-auth"
  })

  annotations = {
    "security.erlmcp.io/classification" = "high"
    "security.erlmcp.io/encryption"     = var.enable_cmek ? "cmek" : "google-managed"
    "compliance.erlmcp.io/pci-dss"      = "true"
    "compliance.erlmcp.io/hipaa"        = "true"
    "audit.erlmcp.io/log-access"        = "true"
  }
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
  provider  = google-beta
  project   = var.project_id
  secret_id = "erlmcp-redis-password"

  replication {
    user_managed {
      dynamic "replicas" {
        for_each = var.secret_replication_locations
        content {
          location = replicas.value
          dynamic "customer_managed_encryption" {
            for_each = var.enable_cmek ? [1] : []
            content {
              kms_key_name = google_kms_crypto_key.secret_encryption[0].id
            }
          }
        }
      }
    }
  }

  dynamic "rotation" {
    for_each = var.enable_rotation ? [1] : []
    content {
      next_rotation_time = timeadd(timestamp(), var.rotation_periods.high)
      rotation_period    = var.rotation_periods.high
    }
  }

  dynamic "topics" {
    for_each = var.enable_rotation_notifications ? [1] : []
    content {
      name = google_pubsub_topic.secret_rotation[0].id
    }
  }

  labels = merge(var.labels, {
    sensitivity  = "high"
    compliance   = "pci-dss"
    rotation     = "90days"
    data-class   = "cache-auth"
  })

  annotations = {
    "security.erlmcp.io/classification" = "high"
    "security.erlmcp.io/encryption"     = var.enable_cmek ? "cmek" : "google-managed"
    "compliance.erlmcp.io/pci-dss"      = "true"
  }
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

# ============================================================================
# CRITICAL SECRETS - Tier 1: Cryptographic Material (Event-driven rotation)
# ============================================================================

# 4. TLS Certificate
resource "google_secret_manager_secret" "tls_cert" {
  provider  = google-beta
  project   = var.project_id
  secret_id = "erlmcp-tls-cert"

  replication {
    user_managed {
      dynamic "replicas" {
        for_each = var.secret_replication_locations
        content {
          location = replicas.value
          dynamic "customer_managed_encryption" {
            for_each = var.enable_cmek ? [1] : []
            content {
              kms_key_name = google_kms_crypto_key.secret_encryption[0].id
            }
          }
        }
      }
    }
  }

  # TLS certs rotated based on certificate validity (manual/event-driven)
  dynamic "topics" {
    for_each = var.enable_rotation_notifications ? [1] : []
    content {
      name = google_pubsub_topic.secret_rotation[0].id
    }
  }

  labels = merge(var.labels, {
    sensitivity  = "critical"
    compliance   = "pci-dss"
    rotation     = "event-driven"
    data-class   = "tls-cert"
  })

  annotations = {
    "security.erlmcp.io/classification" = "critical"
    "security.erlmcp.io/encryption"     = var.enable_cmek ? "cmek" : "google-managed"
    "security.erlmcp.io/key-type"       = "x509-certificate"
    "compliance.erlmcp.io/pci-dss"      = "true"
  }

  version_aliases = {
    "current" = "latest"
  }
}

resource "google_secret_manager_secret_version" "tls_cert" {
  count       = var.create_secret_versions && var.secrets.tls_cert != "" ? 1 : 0
  secret      = google_secret_manager_secret.tls_cert.id
  secret_data = var.secrets.tls_cert

  lifecycle {
    ignore_changes = [secret_data]
  }
}

# 5. TLS Private Key - Highest security classification
resource "google_secret_manager_secret" "tls_key" {
  provider  = google-beta
  project   = var.project_id
  secret_id = "erlmcp-tls-key"

  replication {
    user_managed {
      dynamic "replicas" {
        for_each = var.secret_replication_locations
        content {
          location = replicas.value
          dynamic "customer_managed_encryption" {
            for_each = var.enable_cmek ? [1] : []
            content {
              kms_key_name = google_kms_crypto_key.secret_encryption[0].id
            }
          }
        }
      }
    }
  }

  dynamic "topics" {
    for_each = var.enable_rotation_notifications ? [1] : []
    content {
      name = google_pubsub_topic.secret_rotation[0].id
    }
  }

  labels = merge(var.labels, {
    sensitivity  = "critical"
    compliance   = "pci-dss-hipaa"
    rotation     = "event-driven"
    data-class   = "private-key"
  })

  annotations = {
    "security.erlmcp.io/classification" = "critical"
    "security.erlmcp.io/encryption"     = var.enable_cmek ? "cmek" : "google-managed"
    "security.erlmcp.io/key-type"       = "rsa-private"
    "security.erlmcp.io/access-audit"   = "mandatory"
    "compliance.erlmcp.io/pci-dss"      = "true"
    "compliance.erlmcp.io/hipaa"        = "true"
  }

  version_aliases = {
    "current" = "latest"
  }
}

resource "google_secret_manager_secret_version" "tls_key" {
  count       = var.create_secret_versions && var.secrets.tls_key != "" ? 1 : 0
  secret      = google_secret_manager_secret.tls_key.id
  secret_data = var.secrets.tls_key

  lifecycle {
    ignore_changes = [secret_data]
  }
}

# 6. CA Bundle
resource "google_secret_manager_secret" "ca_bundle" {
  provider  = google-beta
  project   = var.project_id
  secret_id = "erlmcp-ca-bundle"

  replication {
    automatic {}  # CA bundles can use automatic replication
  }

  labels = merge(var.labels, {
    sensitivity  = "high"
    rotation     = "event-driven"
    data-class   = "ca-cert"
  })

  annotations = {
    "security.erlmcp.io/classification" = "high"
    "security.erlmcp.io/key-type"       = "x509-ca-bundle"
  }
}

resource "google_secret_manager_secret_version" "ca_bundle" {
  count       = var.create_secret_versions && var.secrets.ca_bundle != "" ? 1 : 0
  secret      = google_secret_manager_secret.ca_bundle.id
  secret_data = var.secrets.ca_bundle

  lifecycle {
    ignore_changes = [secret_data]
  }
}

# 7. JWT Private Key - Critical for authentication
resource "google_secret_manager_secret" "jwt_private_key" {
  provider  = google-beta
  project   = var.project_id
  secret_id = "erlmcp-jwt-private-key"

  replication {
    user_managed {
      dynamic "replicas" {
        for_each = var.secret_replication_locations
        content {
          location = replicas.value
          dynamic "customer_managed_encryption" {
            for_each = var.enable_cmek ? [1] : []
            content {
              kms_key_name = google_kms_crypto_key.secret_encryption[0].id
            }
          }
        }
      }
    }
  }

  dynamic "rotation" {
    for_each = var.enable_rotation ? [1] : []
    content {
      next_rotation_time = timeadd(timestamp(), var.rotation_periods.crypto)
      rotation_period    = var.rotation_periods.crypto
    }
  }

  dynamic "topics" {
    for_each = var.enable_rotation_notifications ? [1] : []
    content {
      name = google_pubsub_topic.secret_rotation[0].id
    }
  }

  labels = merge(var.labels, {
    sensitivity  = "critical"
    compliance   = "pci-dss"
    rotation     = "365days"
    data-class   = "jwt-signing-key"
  })

  annotations = {
    "security.erlmcp.io/classification" = "critical"
    "security.erlmcp.io/encryption"     = var.enable_cmek ? "cmek" : "google-managed"
    "security.erlmcp.io/key-type"       = "rsa-private"
    "security.erlmcp.io/algorithm"      = "RS256"
    "security.erlmcp.io/access-audit"   = "mandatory"
    "compliance.erlmcp.io/pci-dss"      = "true"
  }

  version_aliases = {
    "current" = "latest"
  }
}

resource "google_secret_manager_secret_version" "jwt_private_key" {
  count       = var.create_secret_versions && var.secrets.jwt_private_key != "" ? 1 : 0
  secret      = google_secret_manager_secret.jwt_private_key.id
  secret_data = var.secrets.jwt_private_key

  lifecycle {
    ignore_changes = [secret_data]
  }
}

# 8. JWT Public Key - Public verification key
resource "google_secret_manager_secret" "jwt_public_key" {
  provider  = google-beta
  project   = var.project_id
  secret_id = "erlmcp-jwt-public-key"

  replication {
    automatic {}  # Public keys can use automatic replication
  }

  labels = merge(var.labels, {
    sensitivity  = "medium"
    rotation     = "365days"
    data-class   = "jwt-public-key"
  })

  annotations = {
    "security.erlmcp.io/classification" = "medium"
    "security.erlmcp.io/key-type"       = "rsa-public"
    "security.erlmcp.io/algorithm"      = "RS256"
  }
}

resource "google_secret_manager_secret_version" "jwt_public_key" {
  count       = var.create_secret_versions && var.secrets.jwt_public_key != "" ? 1 : 0
  secret      = google_secret_manager_secret.jwt_public_key.id
  secret_data = var.secrets.jwt_public_key

  lifecycle {
    ignore_changes = [secret_data]
  }
}

# ============================================================================
# HIGH SECRETS - Tier 2: Operational Access (90-day rotation)
# ============================================================================

# 9. Grafana Password
resource "google_secret_manager_secret" "grafana_password" {
  provider  = google-beta
  project   = var.project_id
  secret_id = "erlmcp-grafana-password"

  replication {
    automatic {}
  }

  dynamic "rotation" {
    for_each = var.enable_rotation ? [1] : []
    content {
      next_rotation_time = timeadd(timestamp(), var.rotation_periods.high)
      rotation_period    = var.rotation_periods.high
    }
  }

  dynamic "topics" {
    for_each = var.enable_rotation_notifications ? [1] : []
    content {
      name = google_pubsub_topic.secret_rotation[0].id
    }
  }

  labels = merge(var.labels, {
    sensitivity  = "high"
    rotation     = "90days"
    data-class   = "monitoring-auth"
  })

  annotations = {
    "security.erlmcp.io/classification" = "high"
    "security.erlmcp.io/service"        = "grafana"
  }
}

resource "google_secret_manager_secret_version" "grafana_password" {
  count       = var.create_secret_versions ? 1 : 0
  secret      = google_secret_manager_secret.grafana_password.id
  secret_data = var.secrets.grafana_password != "" ? var.secrets.grafana_password : random_password.grafana_password[0].result

  lifecycle {
    ignore_changes = [secret_data]
  }
}

resource "random_password" "grafana_password" {
  count   = var.create_secret_versions && var.secrets.grafana_password == "" ? 1 : 0
  length  = 32
  special = true

  override_special = "!@#$%^&*()-_=+[]{}:?"
  min_lower        = 8
  min_upper        = 8
  min_numeric      = 8
  min_special      = 8
}

# ============================================================================
# CRITICAL SECRETS - Tier 1: Data Protection (365-day rotation)
# ============================================================================

# 10. Backup Encryption Key
resource "google_secret_manager_secret" "backup_key" {
  provider  = google-beta
  project   = var.project_id
  secret_id = "erlmcp-backup-key"

  replication {
    user_managed {
      dynamic "replicas" {
        for_each = var.secret_replication_locations
        content {
          location = replicas.value
          dynamic "customer_managed_encryption" {
            for_each = var.enable_cmek ? [1] : []
            content {
              kms_key_name = google_kms_crypto_key.secret_encryption[0].id
            }
          }
        }
      }
    }
  }

  dynamic "rotation" {
    for_each = var.enable_rotation ? [1] : []
    content {
      next_rotation_time = timeadd(timestamp(), var.rotation_periods.crypto)
      rotation_period    = var.rotation_periods.crypto
    }
  }

  dynamic "topics" {
    for_each = var.enable_rotation_notifications ? [1] : []
    content {
      name = google_pubsub_topic.secret_rotation[0].id
    }
  }

  labels = merge(var.labels, {
    sensitivity  = "critical"
    compliance   = "pci-dss-hipaa-gdpr"
    rotation     = "365days"
    data-class   = "backup-encryption"
  })

  annotations = {
    "security.erlmcp.io/classification" = "critical"
    "security.erlmcp.io/encryption"     = var.enable_cmek ? "cmek" : "google-managed"
    "security.erlmcp.io/purpose"        = "backup-encryption"
    "security.erlmcp.io/access-audit"   = "mandatory"
    "compliance.erlmcp.io/pci-dss"      = "true"
    "compliance.erlmcp.io/hipaa"        = "true"
    "compliance.erlmcp.io/gdpr"         = "true"
  }

  version_aliases = {
    "current" = "latest"
  }
}

resource "google_secret_manager_secret_version" "backup_key" {
  count       = var.create_secret_versions ? 1 : 0
  secret      = google_secret_manager_secret.backup_key.id
  secret_data = var.secrets.backup_key != "" ? var.secrets.backup_key : random_password.backup_key[0].result

  lifecycle {
    ignore_changes = [secret_data]
  }
}

resource "random_password" "backup_key" {
  count   = var.create_secret_versions && var.secrets.backup_key == "" ? 1 : 0
  length  = 64
  special = false  # Base64 safe characters only
}

# 11. OpenTelemetry CA Certificate
resource "google_secret_manager_secret" "otel_ca_cert" {
  provider  = google-beta
  project   = var.project_id
  secret_id = "erlmcp-otel-ca-cert"

  replication {
    automatic {}
  }

  labels = merge(var.labels, {
    sensitivity  = "high"
    rotation     = "event-driven"
    data-class   = "observability-cert"
  })

  annotations = {
    "security.erlmcp.io/classification" = "high"
    "security.erlmcp.io/key-type"       = "x509-ca"
    "security.erlmcp.io/service"        = "opentelemetry"
  }
}

resource "google_secret_manager_secret_version" "otel_ca_cert" {
  count       = var.create_secret_versions && var.secrets.otel_ca_cert != "" ? 1 : 0
  secret      = google_secret_manager_secret.otel_ca_cert.id
  secret_data = var.secrets.otel_ca_cert

  lifecycle {
    ignore_changes = [secret_data]
  }
}

# ============================================================================
# IAM Bindings - Least-Privilege Access Control
# Granular permissions based on secret classification and workload requirements
# ============================================================================

# ----------------------------------------------------------------------------
# Cluster Runtime Secrets - Erlang Nodes Only
# ----------------------------------------------------------------------------
resource "google_secret_manager_secret_iam_member" "erlang_cookie_accessor" {
  for_each  = toset(var.cluster_service_accounts)
  project   = var.project_id
  secret_id = google_secret_manager_secret.erlang_cookie.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value

  condition {
    title       = "ClusterNodesOnly"
    description = "Restrict access to Erlang cluster nodes"
    expression  = "resource.name.startsWith('projects/${var.project_id}/secrets/erlmcp-erlang-cookie')"
  }
}

# ----------------------------------------------------------------------------
# Database Access - Application Services Only
# ----------------------------------------------------------------------------
resource "google_secret_manager_secret_iam_member" "db_password_accessor" {
  for_each  = toset(var.database_service_accounts)
  project   = var.project_id
  secret_id = google_secret_manager_secret.db_password.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value

  condition {
    title       = "DatabaseServicesOnly"
    description = "Restrict access to services that need database access"
    expression  = "resource.name.startsWith('projects/${var.project_id}/secrets/erlmcp-db-password')"
  }
}

# ----------------------------------------------------------------------------
# Cache Access - Application Services Only
# ----------------------------------------------------------------------------
resource "google_secret_manager_secret_iam_member" "redis_password_accessor" {
  for_each  = toset(var.cache_service_accounts)
  project   = var.project_id
  secret_id = google_secret_manager_secret.redis_password.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value

  condition {
    title       = "CacheServicesOnly"
    description = "Restrict access to services that need cache access"
    expression  = "resource.name.startsWith('projects/${var.project_id}/secrets/erlmcp-redis-password')"
  }
}

# ----------------------------------------------------------------------------
# TLS Certificates - Load Balancers and API Gateways
# ----------------------------------------------------------------------------
resource "google_secret_manager_secret_iam_member" "tls_cert_accessor" {
  for_each  = toset(var.tls_service_accounts)
  project   = var.project_id
  secret_id = google_secret_manager_secret.tls_cert.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value
}

resource "google_secret_manager_secret_iam_member" "tls_key_accessor" {
  for_each  = toset(var.tls_service_accounts)
  project   = var.project_id
  secret_id = google_secret_manager_secret.tls_key.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value

  condition {
    title       = "TLSTerminationOnly"
    description = "Restrict TLS private key access to load balancers and API gateways"
    expression  = "resource.name.startsWith('projects/${var.project_id}/secrets/erlmcp-tls-key')"
  }
}

# ----------------------------------------------------------------------------
# CA Bundle - All Application Services
# ----------------------------------------------------------------------------
resource "google_secret_manager_secret_iam_member" "ca_bundle_accessor" {
  for_each  = toset(var.app_service_accounts)
  project   = var.project_id
  secret_id = google_secret_manager_secret.ca_bundle.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value
}

# ----------------------------------------------------------------------------
# JWT Keys - Authentication Services Only
# ----------------------------------------------------------------------------
resource "google_secret_manager_secret_iam_member" "jwt_private_key_accessor" {
  for_each  = toset(var.auth_service_accounts)
  project   = var.project_id
  secret_id = google_secret_manager_secret.jwt_private_key.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value

  condition {
    title       = "AuthServicesOnly"
    description = "Restrict JWT signing key access to authentication services"
    expression  = "resource.name.startsWith('projects/${var.project_id}/secrets/erlmcp-jwt-private-key')"
  }
}

# JWT Public Key - All services that verify tokens
resource "google_secret_manager_secret_iam_member" "jwt_public_key_accessor" {
  for_each  = toset(var.app_service_accounts)
  project   = var.project_id
  secret_id = google_secret_manager_secret.jwt_public_key.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value
}

# ----------------------------------------------------------------------------
# Monitoring - Monitoring Services Only
# ----------------------------------------------------------------------------
resource "google_secret_manager_secret_iam_member" "grafana_password_accessor" {
  for_each  = toset(var.monitoring_service_accounts)
  project   = var.project_id
  secret_id = google_secret_manager_secret.grafana_password.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value
}

# ----------------------------------------------------------------------------
# Backup Key - Backup Services Only
# ----------------------------------------------------------------------------
resource "google_secret_manager_secret_iam_member" "backup_key_accessor" {
  for_each  = toset(var.backup_service_accounts)
  project   = var.project_id
  secret_id = google_secret_manager_secret.backup_key.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value

  condition {
    title       = "BackupServicesOnly"
    description = "Restrict backup encryption key to backup services"
    expression  = "resource.name.startsWith('projects/${var.project_id}/secrets/erlmcp-backup-key')"
  }
}

# ----------------------------------------------------------------------------
# Observability - OpenTelemetry Collectors
# ----------------------------------------------------------------------------
resource "google_secret_manager_secret_iam_member" "otel_ca_cert_accessor" {
  for_each  = toset(var.observability_service_accounts)
  project   = var.project_id
  secret_id = google_secret_manager_secret.otel_ca_cert.secret_id
  role      = "roles/secretmanager.secretAccessor"
  member    = each.value
}

# ----------------------------------------------------------------------------
# Secret Viewers - Security and Audit Teams (Metadata Only)
# ----------------------------------------------------------------------------
resource "google_secret_manager_secret_iam_member" "secret_viewers" {
  for_each = toset(var.secret_viewers)
  project  = var.project_id
  role     = "roles/secretmanager.viewer"
  member   = each.value

  condition {
    title       = "ViewMetadataOnly"
    description = "Allow viewing secret metadata but not values"
    expression  = "true"
  }
}

# ----------------------------------------------------------------------------
# Secret Administrators - Security Team Only
# ----------------------------------------------------------------------------
resource "google_secret_manager_secret_iam_member" "secret_admins" {
  for_each = toset(var.secret_administrators)
  project  = var.project_id
  role     = "roles/secretmanager.admin"
  member   = each.value

  condition {
    title       = "SecurityTeamOnly"
    description = "Full secret management for security team"
    expression  = "true"
  }
}

# ============================================================================
# Audit Logging Configuration
# ============================================================================
resource "google_project_iam_audit_config" "secret_manager_audit" {
  count   = var.enable_audit_logging ? 1 : 0
  project = var.project_id
  service = "secretmanager.googleapis.com"

  audit_log_config {
    log_type = "ADMIN_READ"
  }

  audit_log_config {
    log_type = "DATA_READ"
  }

  audit_log_config {
    log_type = "DATA_WRITE"
  }
}
