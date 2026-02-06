# ============================================================================
# Backup Module for erlmcp - Enterprise Disaster Recovery
# GKE Backup for GKE, Cloud Storage snapshots, Database backups with CMEK
# Multi-region replication, retention policies, and compliance controls
# ============================================================================
#
# Backup Strategy:
# - GKE Backup: Cluster-level and namespace-level backup plans
# - Cloud Storage: Object versioning, lifecycle management, cross-region replication
# - Database: Automated backups for Cloud SQL, Spanner, Firestore
# - CMEK encryption for all backup data
# - Point-in-time recovery (PITR) support
# - Compliance: HIPAA, PCI-DSS, SOC2, GDPR retention policies
# - Automated testing and validation
#
# ============================================================================

terraform {
  required_version = ">= 1.8.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 6.0.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = ">= 6.0.0"
    }
    random = {
      source  = "hashicorp/random"
      version = ">= 3.6.0"
    }
  }
}

# ============================================================================
# API Enablement
# ============================================================================
resource "google_project_service" "gke_backup" {
  count              = var.enable_gke_backup ? 1 : 0
  project            = var.project_id
  service            = "gkebackup.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "storage" {
  project            = var.project_id
  service            = "storage.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "cloudkms" {
  count              = var.enable_cmek ? 1 : 0
  project            = var.project_id
  service            = "cloudkms.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "sqladmin" {
  count              = var.enable_database_backup ? 1 : 0
  project            = var.project_id
  service            = "sqladmin.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "spanner" {
  count              = var.enable_spanner_backup ? 1 : 0
  project            = var.project_id
  service            = "spanner.googleapis.com"
  disable_on_destroy = false
}

# ============================================================================
# Data Sources
# ============================================================================
data "google_project" "current" {
  project_id = var.project_id
}

data "google_client_config" "default" {}

# ============================================================================
# KMS Key Ring and Keys for Backup Encryption (CMEK)
# ============================================================================
resource "google_kms_key_ring" "backup" {
  count    = var.enable_cmek ? 1 : 0
  project  = var.project_id
  name     = "erlmcp-backup-keyring"
  location = var.kms_location
}

resource "google_kms_crypto_key" "backup_encryption" {
  count           = var.enable_cmek ? 1 : 0
  name            = "erlmcp-backup-encryption-key"
  key_ring        = google_kms_key_ring.backup[0].id
  rotation_period = "7776000s" # 90 days

  purpose = "ENCRYPT_DECRYPT"

  lifecycle {
    prevent_destroy = true
  }

  labels = merge(var.labels, {
    purpose    = "backup-encryption"
    compliance = "pci-dss-hipaa-gdpr"
    tier       = "critical"
  })
}

# Grant necessary services access to KMS key
resource "google_kms_crypto_key_iam_member" "backup_service_encrypter" {
  count         = var.enable_cmek ? 1 : 0
  crypto_key_id = google_kms_crypto_key.backup_encryption[0].id
  role          = "roles/cloudkms.cryptoKeyEncrypterDecrypter"
  member        = "serviceAccount:service-${data.google_project.current.number}@gs-project-accounts.iam.gserviceaccount.com"
}

resource "google_kms_crypto_key_iam_member" "gke_backup_encrypter" {
  count         = var.enable_cmek && var.enable_gke_backup ? 1 : 0
  crypto_key_id = google_kms_crypto_key.backup_encryption[0].id
  role          = "roles/cloudkms.cryptoKeyEncrypterDecrypter"
  member        = "serviceAccount:service-${data.google_project.current.number}@gcp-sa-gkebackup.iam.gserviceaccount.com"
}

resource "google_kms_crypto_key_iam_member" "sql_backup_encrypter" {
  count         = var.enable_cmek && var.enable_database_backup ? 1 : 0
  crypto_key_id = google_kms_crypto_key.backup_encryption[0].id
  role          = "roles/cloudkms.cryptoKeyEncrypterDecrypter"
  member        = "serviceAccount:service-${data.google_project.current.number}@gcp-sa-cloud-sql.iam.gserviceaccount.com"
}

# ============================================================================
# GKE Backup - Backup Plans
# ============================================================================

# Primary backup storage location
resource "google_gke_backup_backup_plan" "cluster_backup" {
  provider = google-beta
  count    = var.enable_gke_backup ? 1 : 0

  project  = var.project_id
  name     = "erlmcp-cluster-backup-plan"
  location = var.region
  cluster  = var.gke_cluster_id

  # Retention policy
  retention_policy {
    backup_delete_lock_days = var.backup_delete_lock_days
    backup_retain_days      = var.backup_retention_days
    locked                  = var.lock_backup_retention
  }

  # Backup schedule
  backup_schedule {
    cron_schedule = var.backup_cron_schedule
    paused        = var.pause_backup_schedule
  }

  # Backup configuration
  backup_config {
    # Include all namespaces or specific ones
    include_volume_data = var.include_volume_data
    include_secrets     = var.include_secrets
    all_namespaces      = var.backup_all_namespaces

    # Selected namespaces
    dynamic "selected_namespaces" {
      for_each = var.backup_all_namespaces ? [] : [1]
      content {
        namespaces = var.backup_namespaces
      }
    }

    # Selected applications by label
    dynamic "selected_applications" {
      for_each = var.backup_selected_applications != null ? [1] : []
      content {
        dynamic "namespaced_names" {
          for_each = var.backup_selected_applications.namespaced_names
          content {
            name      = namespaced_names.value.name
            namespace = namespaced_names.value.namespace
          }
        }
      }
    }

    # Encryption configuration
    dynamic "encryption_key" {
      for_each = var.enable_cmek ? [1] : []
      content {
        gcp_kms_encryption_key = google_kms_crypto_key.backup_encryption[0].id
      }
    }
  }

  # Deactivation prevents new backups
  deactivated = var.deactivate_backup_plan

  labels = merge(var.labels, {
    backup-type      = "gke-cluster"
    frequency        = "automated"
    compliance       = "pci-dss-hipaa"
    disaster-recovery = "enabled"
  })

  depends_on = [
    google_project_service.gke_backup,
    google_kms_crypto_key_iam_member.gke_backup_encrypter
  ]
}

# Application-specific backup plan
resource "google_gke_backup_backup_plan" "app_backup" {
  provider = google-beta
  count    = var.enable_gke_backup && var.enable_app_backup ? 1 : 0

  project  = var.project_id
  name     = "erlmcp-app-backup-plan"
  location = var.region
  cluster  = var.gke_cluster_id

  retention_policy {
    backup_delete_lock_days = var.app_backup_delete_lock_days
    backup_retain_days      = var.app_backup_retention_days
    locked                  = var.lock_app_backup_retention
  }

  backup_schedule {
    cron_schedule = var.app_backup_cron_schedule
    paused        = var.pause_app_backup_schedule
  }

  backup_config {
    include_volume_data = true
    include_secrets     = true
    all_namespaces      = false

    selected_namespaces {
      namespaces = var.app_backup_namespaces
    }

    dynamic "encryption_key" {
      for_each = var.enable_cmek ? [1] : []
      content {
        gcp_kms_encryption_key = google_kms_crypto_key.backup_encryption[0].id
      }
    }
  }

  deactivated = var.deactivate_app_backup_plan

  labels = merge(var.labels, {
    backup-type       = "application"
    frequency         = "high-frequency"
    compliance        = "pci-dss-hipaa"
    disaster-recovery = "enabled"
  })

  depends_on = [
    google_project_service.gke_backup,
    google_kms_crypto_key_iam_member.gke_backup_encrypter
  ]
}

# Restore plan for cluster backup
resource "google_gke_backup_restore_plan" "cluster_restore" {
  provider = google-beta
  count    = var.enable_gke_backup && var.create_restore_plan ? 1 : 0

  project  = var.project_id
  name     = "erlmcp-cluster-restore-plan"
  location = var.region

  backup_plan = google_gke_backup_backup_plan.cluster_backup[0].id
  cluster     = var.gke_cluster_id

  restore_config {
    # Cluster resource conflict policy
    cluster_resource_conflict_policy = var.cluster_resource_conflict_policy

    # Namespaced resource restore mode
    namespaced_resource_restore_mode = var.namespaced_resource_restore_mode

    # Volume data restore policy
    volume_data_restore_policy = var.volume_data_restore_policy

    # Cluster resource restore scope
    dynamic "cluster_resource_restore_scope" {
      for_each = var.restore_all_cluster_resources ? [] : [1]
      content {
        dynamic "selected_group_kinds" {
          for_each = var.restore_selected_group_kinds
          content {
            resource_group = selected_group_kinds.value.resource_group
            resource_kind  = selected_group_kinds.value.resource_kind
          }
        }
      }
    }

    # Transformation rules for restore
    dynamic "transformation_rules" {
      for_each = var.restore_transformation_rules
      content {
        description = transformation_rules.value.description
        field_actions {
          op    = transformation_rules.value.field_action.op
          path  = transformation_rules.value.field_action.path
          value = transformation_rules.value.field_action.value
        }
        resource_filter {
          namespaces        = transformation_rules.value.resource_filter.namespaces
          group_kinds {
            resource_group = transformation_rules.value.resource_filter.group_kind.resource_group
            resource_kind  = transformation_rules.value.resource_filter.group_kind.resource_kind
          }
        }
      }
    }
  }

  labels = merge(var.labels, {
    restore-plan-type = "cluster"
    disaster-recovery = "enabled"
  })

  depends_on = [google_gke_backup_backup_plan.cluster_backup]
}

# ============================================================================
# Cloud Storage - Backup Buckets with Versioning and Lifecycle
# ============================================================================

# Primary backup bucket
resource "google_storage_bucket" "primary_backup" {
  project       = var.project_id
  name          = "${var.project_id}-erlmcp-backup-primary-${var.region}"
  location      = var.region
  storage_class = var.backup_storage_class

  # Object versioning for point-in-time recovery
  versioning {
    enabled = true
  }

  # Uniform bucket-level access (recommended for security)
  uniform_bucket_level_access {
    enabled = true
  }

  # Public access prevention (enforce)
  public_access_prevention = "enforced"

  # Soft delete policy for additional protection
  soft_delete_policy {
    retention_duration_seconds = var.soft_delete_retention_seconds
  }

  # CMEK encryption
  dynamic "encryption" {
    for_each = var.enable_cmek ? [1] : []
    content {
      default_kms_key_name = google_kms_crypto_key.backup_encryption[0].id
    }
  }

  # Lifecycle management
  lifecycle_rule {
    condition {
      age                   = var.archive_age_days
      matches_storage_class = [var.backup_storage_class]
    }
    action {
      type          = "SetStorageClass"
      storage_class = "ARCHIVE"
    }
  }

  lifecycle_rule {
    condition {
      age = var.backup_retention_days
    }
    action {
      type = "Delete"
    }
  }

  lifecycle_rule {
    condition {
      days_since_noncurrent_time = var.noncurrent_version_retention_days
      with_state                 = "ARCHIVED"
    }
    action {
      type = "Delete"
    }
  }

  # Retention policy (compliance lock)
  dynamic "retention_policy" {
    for_each = var.enable_retention_policy ? [1] : []
    content {
      retention_period = var.retention_period_seconds
      is_locked        = var.lock_retention_policy
    }
  }

  # Autoclass for automatic storage class transitions
  dynamic "autoclass" {
    for_each = var.enable_autoclass ? [1] : []
    content {
      enabled                = true
      terminal_storage_class = "ARCHIVE"
    }
  }

  labels = merge(var.labels, {
    backup-tier       = "primary"
    storage-type      = "versioned"
    compliance        = "pci-dss-hipaa-gdpr"
    disaster-recovery = "enabled"
    data-class        = "backup"
  })

  depends_on = [
    google_project_service.storage,
    google_kms_crypto_key_iam_member.backup_service_encrypter
  ]
}

# Secondary backup bucket (cross-region replication)
resource "google_storage_bucket" "secondary_backup" {
  count         = var.enable_cross_region_replication ? 1 : 0
  project       = var.project_id
  name          = "${var.project_id}-erlmcp-backup-secondary-${var.secondary_region}"
  location      = var.secondary_region
  storage_class = var.backup_storage_class

  versioning {
    enabled = true
  }

  uniform_bucket_level_access {
    enabled = true
  }

  public_access_prevention = "enforced"

  soft_delete_policy {
    retention_duration_seconds = var.soft_delete_retention_seconds
  }

  dynamic "encryption" {
    for_each = var.enable_cmek ? [1] : []
    content {
      default_kms_key_name = google_kms_crypto_key.backup_encryption[0].id
    }
  }

  lifecycle_rule {
    condition {
      age                   = var.archive_age_days
      matches_storage_class = [var.backup_storage_class]
    }
    action {
      type          = "SetStorageClass"
      storage_class = "ARCHIVE"
    }
  }

  lifecycle_rule {
    condition {
      age = var.backup_retention_days
    }
    action {
      type = "Delete"
    }
  }

  dynamic "retention_policy" {
    for_each = var.enable_retention_policy ? [1] : []
    content {
      retention_period = var.retention_period_seconds
      is_locked        = var.lock_retention_policy
    }
  }

  labels = merge(var.labels, {
    backup-tier       = "secondary"
    storage-type      = "versioned"
    compliance        = "pci-dss-hipaa-gdpr"
    disaster-recovery = "enabled"
    data-class        = "backup"
  })

  depends_on = [
    google_project_service.storage,
    google_kms_crypto_key_iam_member.backup_service_encrypter
  ]
}

# Archive bucket for long-term retention
resource "google_storage_bucket" "archive_backup" {
  count         = var.enable_archive_bucket ? 1 : 0
  project       = var.project_id
  name          = "${var.project_id}-erlmcp-backup-archive"
  location      = var.archive_region != "" ? var.archive_region : var.region
  storage_class = "ARCHIVE"

  versioning {
    enabled = false # Archive buckets typically don't need versioning
  }

  uniform_bucket_level_access {
    enabled = true
  }

  public_access_prevention = "enforced"

  dynamic "encryption" {
    for_each = var.enable_cmek ? [1] : []
    content {
      default_kms_key_name = google_kms_crypto_key.backup_encryption[0].id
    }
  }

  # Archive retention policy (7-10 years for compliance)
  lifecycle_rule {
    condition {
      age = var.archive_retention_days
    }
    action {
      type = "Delete"
    }
  }

  dynamic "retention_policy" {
    for_each = var.enable_archive_retention_policy ? [1] : []
    content {
      retention_period = var.archive_retention_period_seconds
      is_locked        = var.lock_archive_retention_policy
    }
  }

  labels = merge(var.labels, {
    backup-tier       = "archive"
    storage-type      = "long-term"
    compliance        = "pci-dss-hipaa-gdpr-sox"
    disaster-recovery = "cold-storage"
    data-class        = "backup"
  })

  depends_on = [
    google_project_service.storage,
    google_kms_crypto_key_iam_member.backup_service_encrypter
  ]
}

# IAM for backup buckets
resource "google_storage_bucket_iam_member" "backup_writer" {
  for_each = toset(var.backup_writer_service_accounts)
  bucket   = google_storage_bucket.primary_backup.name
  role     = "roles/storage.objectCreator"
  member   = each.value
}

resource "google_storage_bucket_iam_member" "backup_reader" {
  for_each = toset(var.backup_reader_service_accounts)
  bucket   = google_storage_bucket.primary_backup.name
  role     = "roles/storage.objectViewer"
  member   = each.value
}

# ============================================================================
# Cloud SQL Backup Configuration
# ============================================================================

# Cloud SQL backup configuration is typically set on the instance itself
# This section provides backup validation and monitoring

resource "google_sql_backup_run" "on_demand_backup" {
  count    = var.enable_database_backup && var.create_sql_backup ? 1 : 0
  project  = var.project_id
  instance = var.cloud_sql_instance_name

  description = "erlmcp on-demand backup - ${timestamp()}"

  depends_on = [google_project_service.sqladmin]
}

# ============================================================================
# Spanner Backup Configuration
# ============================================================================

resource "google_spanner_backup" "database_backup" {
  count        = var.enable_spanner_backup && var.create_spanner_backup ? 1 : 0
  project      = var.project_id
  instance     = var.spanner_instance_name
  database     = var.spanner_database_name
  backup_id    = "erlmcp-spanner-backup-${formatdate("YYYY-MM-DD-hhmm", timestamp())}"
  retention_period = "${var.spanner_backup_retention_days * 24}h"

  dynamic "encryption_config" {
    for_each = var.enable_cmek ? [1] : []
    content {
      kms_key_name = google_kms_crypto_key.backup_encryption[0].id
    }
  }

  depends_on = [
    google_project_service.spanner,
    google_kms_crypto_key_iam_member.backup_service_encrypter
  ]
}

# Spanner backup schedule
resource "google_spanner_backup_schedule" "scheduled_backup" {
  count    = var.enable_spanner_backup && var.create_spanner_backup_schedule ? 1 : 0
  project  = var.project_id
  instance = var.spanner_instance_name
  database = var.spanner_database_name
  name     = "erlmcp-spanner-backup-schedule"

  retention_duration = "${var.spanner_backup_retention_days * 24}h"

  spec {
    cron_spec {
      text = var.spanner_backup_cron_schedule
    }
  }

  dynamic "encryption_config" {
    for_each = var.enable_cmek ? [1] : []
    content {
      kms_key_name = google_kms_crypto_key.backup_encryption[0].id
    }
  }

  depends_on = [
    google_project_service.spanner,
    google_kms_crypto_key_iam_member.backup_service_encrypter
  ]
}

# ============================================================================
# Backup Monitoring and Alerting
# ============================================================================

# Pub/Sub topic for backup notifications
resource "google_pubsub_topic" "backup_notifications" {
  count   = var.enable_backup_notifications ? 1 : 0
  project = var.project_id
  name    = "erlmcp-backup-notifications"

  labels = merge(var.labels, {
    purpose = "backup-monitoring"
    alerts  = "enabled"
  })

  message_retention_duration = "604800s" # 7 days
}

# Pub/Sub subscription for backup notifications
resource "google_pubsub_subscription" "backup_notifications_sub" {
  count   = var.enable_backup_notifications ? 1 : 0
  project = var.project_id
  name    = "erlmcp-backup-notifications-sub"
  topic   = google_pubsub_topic.backup_notifications[0].name

  ack_deadline_seconds = 600

  expiration_policy {
    ttl = "" # Never expire
  }

  retry_policy {
    minimum_backoff = "10s"
    maximum_backoff = "600s"
  }

  labels = merge(var.labels, {
    purpose = "backup-monitoring"
  })
}

# Cloud Storage notification for backup bucket changes
resource "google_storage_notification" "backup_bucket_notification" {
  count            = var.enable_backup_notifications ? 1 : 0
  bucket           = google_storage_bucket.primary_backup.name
  payload_format   = "JSON_API_V1"
  topic            = google_pubsub_topic.backup_notifications[0].id
  event_types      = ["OBJECT_FINALIZE", "OBJECT_DELETE"]

  depends_on = [google_pubsub_topic.backup_notifications]
}

# Grant Storage service account permission to publish to Pub/Sub
resource "google_pubsub_topic_iam_member" "storage_publisher" {
  count   = var.enable_backup_notifications ? 1 : 0
  project = var.project_id
  topic   = google_pubsub_topic.backup_notifications[0].name
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:service-${data.google_project.current.number}@gs-project-accounts.iam.gserviceaccount.com"
}

# ============================================================================
# Backup Service Account
# ============================================================================

resource "google_service_account" "backup_service" {
  count        = var.create_backup_service_account ? 1 : 0
  project      = var.project_id
  account_id   = "erlmcp-backup-service"
  display_name = "erlmcp Backup Service Account"
  description  = "Service account for erlmcp backup operations"
}

# Grant backup service account necessary permissions
resource "google_project_iam_member" "backup_service_roles" {
  for_each = var.create_backup_service_account ? toset(var.backup_service_account_roles) : []
  project  = var.project_id
  role     = each.value
  member   = "serviceAccount:${google_service_account.backup_service[0].email}"
}

# ============================================================================
# Backup Validation and Testing
# ============================================================================

# Cloud Scheduler job for backup validation
resource "google_cloud_scheduler_job" "backup_validation" {
  count       = var.enable_backup_validation ? 1 : 0
  project     = var.project_id
  region      = var.region
  name        = "erlmcp-backup-validation"
  description = "Scheduled job to validate backup integrity"
  schedule    = var.backup_validation_schedule
  time_zone   = var.backup_validation_timezone

  http_target {
    uri         = var.backup_validation_endpoint
    http_method = "POST"

    headers = {
      "Content-Type" = "application/json"
    }

    body = base64encode(jsonencode({
      backup_plan = var.enable_gke_backup ? google_gke_backup_backup_plan.cluster_backup[0].name : ""
      bucket      = google_storage_bucket.primary_backup.name
      validation_type = "integrity-check"
    }))

    oauth_token {
      service_account_email = var.create_backup_service_account ? google_service_account.backup_service[0].email : var.backup_validation_service_account
      scope                 = "https://www.googleapis.com/auth/cloud-platform"
    }
  }
}

# ============================================================================
# Disaster Recovery Documentation
# ============================================================================

# Store disaster recovery runbook in backup bucket
resource "google_storage_bucket_object" "dr_runbook" {
  count  = var.upload_dr_runbook ? 1 : 0
  bucket = google_storage_bucket.primary_backup.name
  name   = "disaster-recovery/runbook.md"
  content = templatefile("${path.module}/templates/dr-runbook.md.tpl", {
    project_id              = var.project_id
    region                  = var.region
    gke_cluster_id          = var.gke_cluster_id
    backup_retention_days   = var.backup_retention_days
    rto_hours              = var.rto_hours
    rpo_hours              = var.rpo_hours
  })

  content_type = "text/markdown"

  lifecycle {
    ignore_changes = [content]
  }
}
