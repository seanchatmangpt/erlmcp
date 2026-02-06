# GCP Disaster Recovery Module - Cross-Region Replication & Failover

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
}

# Primary Region Resources
locals {
  primary_region   = var.primary_region
  secondary_region = var.secondary_region
  dr_enabled       = var.enable_disaster_recovery
  failover_type    = var.failover_type # manual | automatic
  rpo_minutes      = var.recovery_point_objective_minutes
  rto_minutes      = var.recovery_time_objective_minutes

  common_labels = merge(var.labels, {
    disaster_recovery = "enabled"
    managed_by        = "terraform"
    service           = "erlmcp"
  })
}

# Cross-Region Cloud Storage Bucket - Primary
resource "google_storage_bucket" "primary" {
  name          = "${var.project_id}-erlmcp-primary-${var.environment}"
  location      = local.primary_region
  project       = var.project_id
  storage_class = "STANDARD"

  uniform_bucket_level_access = true

  versioning {
    enabled = true
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
      num_newer_versions = 3
    }
    action {
      type = "Delete"
    }
  }

  labels = local.common_labels
}

# Cross-Region Cloud Storage Bucket - Secondary
resource "google_storage_bucket" "secondary" {
  name          = "${var.project_id}-erlmcp-secondary-${var.environment}"
  location      = local.secondary_region
  project       = var.project_id
  storage_class = "STANDARD"

  uniform_bucket_level_access = true

  versioning {
    enabled = true
  }

  lifecycle_rule {
    condition {
      age = var.backup_retention_days
    }
    action {
      type = "Delete"
    }
  }

  labels = local.common_labels
}

# Multi-Region Bucket for Critical Data
resource "google_storage_bucket" "multi_region" {
  name          = "${var.project_id}-erlmcp-multi-${var.environment}"
  location      = var.multi_region_location # US, EU, ASIA
  project       = var.project_id
  storage_class = "STANDARD"

  uniform_bucket_level_access = true

  versioning {
    enabled = true
  }

  labels = local.common_labels
}

# Cloud Storage Transfer Job - Primary to Secondary
resource "google_storage_transfer_job" "primary_to_secondary" {
  count = local.dr_enabled ? 1 : 0

  description = "Replicate primary bucket to secondary region"
  project     = var.project_id

  transfer_spec {
    gcs_data_source {
      bucket_name = google_storage_bucket.primary.name
    }

    gcs_data_sink {
      bucket_name = google_storage_bucket.secondary.name
    }

    transfer_options {
      delete_objects_from_source_after_transfer = false
      overwrite_objects_already_existing_in_sink = false
    }
  }

  schedule {
    schedule_start_date {
      year  = 2026
      month = 2
      day   = 6
    }

    start_time_of_day {
      hours   = 2
      minutes = 0
      seconds = 0
    }
  }
}

# Cloud SQL Primary Instance
resource "google_sql_database_instance" "primary" {
  name             = "erlmcp-primary-${var.environment}"
  database_version = var.database_version
  region           = local.primary_region
  project          = var.project_id

  settings {
    tier              = var.database_tier
    availability_type = "REGIONAL"
    disk_autoresize   = true
    disk_size         = var.database_disk_size_gb
    disk_type         = "PD_SSD"

    backup_configuration {
      enabled                        = true
      start_time                     = "02:00"
      point_in_time_recovery_enabled = true
      transaction_log_retention_days = 7
      backup_retention_settings {
        retained_backups = 30
        retention_unit   = "COUNT"
      }
    }

    ip_configuration {
      ipv4_enabled    = false
      private_network = var.vpc_network_id
      require_ssl     = true
    }

    database_flags {
      name  = "max_connections"
      value = "1000"
    }

    user_labels = local.common_labels
  }

  deletion_protection = var.deletion_protection
}

# Cloud SQL Read Replica - Secondary Region
resource "google_sql_database_instance" "secondary_replica" {
  count = local.dr_enabled ? 1 : 0

  name                 = "erlmcp-secondary-replica-${var.environment}"
  master_instance_name = google_sql_database_instance.primary.name
  database_version     = var.database_version
  region               = local.secondary_region
  project              = var.project_id

  replica_configuration {
    failover_target = true
  }

  settings {
    tier              = var.database_tier
    availability_type = "REGIONAL"
    disk_autoresize   = true
    disk_size         = var.database_disk_size_gb
    disk_type         = "PD_SSD"

    ip_configuration {
      ipv4_enabled    = false
      private_network = var.vpc_network_id
      require_ssl     = true
    }

    user_labels = merge(local.common_labels, {
      role = "replica"
    })
  }

  deletion_protection = var.deletion_protection
}

# Cloud Spanner Instance - Multi-Region
resource "google_spanner_instance" "multi_region" {
  count = var.enable_spanner ? 1 : 0

  name         = "erlmcp-spanner-${var.environment}"
  config       = var.spanner_config # nam-eur-asia1 for global
  display_name = "erlmcp Spanner Multi-Region"
  project      = var.project_id
  num_nodes    = var.spanner_nodes

  labels = local.common_labels
}

# Cloud Spanner Database
resource "google_spanner_database" "database" {
  count = var.enable_spanner ? 1 : 0

  instance = google_spanner_instance.multi_region[0].name
  name     = "erlmcp_db"
  project  = var.project_id

  ddl = [
    "CREATE TABLE mcp_sessions (session_id STRING(36) NOT NULL, data BYTES(MAX), created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true), updated_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true)) PRIMARY KEY (session_id)",
    "CREATE TABLE mcp_tools (tool_id STRING(36) NOT NULL, name STRING(255) NOT NULL, schema BYTES(MAX), created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true)) PRIMARY KEY (tool_id)",
    "CREATE TABLE mcp_prompts (prompt_id STRING(36) NOT NULL, content STRING(MAX), created_at TIMESTAMP NOT NULL OPTIONS (allow_commit_timestamp=true)) PRIMARY KEY (prompt_id)",
  ]

  version_retention_period = "7d"

  deletion_protection = var.deletion_protection
}

# GKE Cluster - Primary Region
resource "google_container_cluster" "primary" {
  name     = "erlmcp-primary-${var.environment}"
  location = local.primary_region
  project  = var.project_id

  remove_default_node_pool = true
  initial_node_count       = 1

  network    = var.vpc_network_id
  subnetwork = var.primary_subnet_id

  cluster_autoscaling {
    enabled = true
    resource_limits {
      resource_type = "cpu"
      minimum       = 4
      maximum       = 100
    }
    resource_limits {
      resource_type = "memory"
      minimum       = 16
      maximum       = 400
    }
  }

  maintenance_policy {
    daily_maintenance_window {
      start_time = "03:00"
    }
  }

  resource_labels = local.common_labels
}

# GKE Node Pool - Primary
resource "google_container_node_pool" "primary_nodes" {
  name       = "primary-node-pool"
  location   = local.primary_region
  cluster    = google_container_cluster.primary.name
  project    = var.project_id
  node_count = var.node_count_per_zone

  autoscaling {
    min_node_count = var.min_node_count
    max_node_count = var.max_node_count
  }

  management {
    auto_repair  = true
    auto_upgrade = true
  }

  node_config {
    machine_type = var.node_machine_type
    disk_size_gb = 100
    disk_type    = "pd-ssd"

    oauth_scopes = [
      "https://www.googleapis.com/auth/cloud-platform"
    ]

    labels = merge(local.common_labels, {
      role = "primary"
    })

    metadata = {
      disable-legacy-endpoints = "true"
    }
  }
}

# GKE Cluster - Secondary Region (Standby)
resource "google_container_cluster" "secondary" {
  count = local.dr_enabled ? 1 : 0

  name     = "erlmcp-secondary-${var.environment}"
  location = local.secondary_region
  project  = var.project_id

  remove_default_node_pool = true
  initial_node_count       = 1

  network    = var.vpc_network_id
  subnetwork = var.secondary_subnet_id

  cluster_autoscaling {
    enabled = true
    resource_limits {
      resource_type = "cpu"
      minimum       = 2
      maximum       = 50
    }
    resource_limits {
      resource_type = "memory"
      minimum       = 8
      maximum       = 200
    }
  }

  maintenance_policy {
    daily_maintenance_window {
      start_time = "03:00"
    }
  }

  resource_labels = merge(local.common_labels, {
    role = "secondary"
  })
}

# GKE Node Pool - Secondary
resource "google_container_node_pool" "secondary_nodes" {
  count = local.dr_enabled ? 1 : 0

  name       = "secondary-node-pool"
  location   = local.secondary_region
  cluster    = google_container_cluster.secondary[0].name
  project    = var.project_id
  node_count = var.secondary_node_count_per_zone

  autoscaling {
    min_node_count = var.secondary_min_node_count
    max_node_count = var.secondary_max_node_count
  }

  management {
    auto_repair  = true
    auto_upgrade = true
  }

  node_config {
    machine_type = var.node_machine_type
    disk_size_gb = 100
    disk_type    = "pd-ssd"

    oauth_scopes = [
      "https://www.googleapis.com/auth/cloud-platform"
    ]

    labels = merge(local.common_labels, {
      role = "secondary"
    })

    metadata = {
      disable-legacy-endpoints = "true"
    }
  }
}

# Global Load Balancer for Failover
resource "google_compute_global_address" "default" {
  name    = "erlmcp-global-ip-${var.environment}"
  project = var.project_id
}

# Health Check for Backend Services
resource "google_compute_health_check" "default" {
  name    = "erlmcp-health-check-${var.environment}"
  project = var.project_id

  timeout_sec         = 5
  check_interval_sec  = 10
  healthy_threshold   = 2
  unhealthy_threshold = 3

  http_health_check {
    port         = 8080
    request_path = "/health"
  }
}

# Backend Service - Primary
resource "google_compute_backend_service" "primary" {
  name                  = "erlmcp-backend-primary-${var.environment}"
  project               = var.project_id
  health_checks         = [google_compute_health_check.default.id]
  port_name             = "http"
  protocol              = "HTTP"
  timeout_sec           = 30
  enable_cdn            = false
  load_balancing_scheme = "EXTERNAL_MANAGED"

  backend {
    group           = google_container_cluster.primary.id
    balancing_mode  = "UTILIZATION"
    capacity_scaler = 1.0
  }

  failover_policy {
    disable_connection_drain_on_failover = false
    drop_traffic_if_unhealthy            = true
    failover_ratio                       = 0.1
  }
}

# Backend Service - Secondary (Failover)
resource "google_compute_backend_service" "secondary" {
  count = local.dr_enabled ? 1 : 0

  name                  = "erlmcp-backend-secondary-${var.environment}"
  project               = var.project_id
  health_checks         = [google_compute_health_check.default.id]
  port_name             = "http"
  protocol              = "HTTP"
  timeout_sec           = 30
  enable_cdn            = false
  load_balancing_scheme = "EXTERNAL_MANAGED"

  backend {
    group           = google_container_cluster.secondary[0].id
    balancing_mode  = "UTILIZATION"
    capacity_scaler = 1.0
  }
}

# Cloud Memorystore Redis - Primary
resource "google_redis_instance" "primary" {
  name           = "erlmcp-redis-primary-${var.environment}"
  tier           = "STANDARD_HA"
  memory_size_gb = var.redis_memory_gb
  region         = local.primary_region
  project        = var.project_id

  authorized_network = var.vpc_network_id
  connect_mode       = "PRIVATE_SERVICE_ACCESS"

  redis_version     = "REDIS_6_X"
  display_name      = "erlmcp Redis Primary"
  reserved_ip_range = var.redis_primary_ip_range

  replica_count            = 1
  read_replicas_mode       = "READ_REPLICAS_ENABLED"
  transit_encryption_mode  = "SERVER_AUTHENTICATION"
  auth_enabled             = true

  labels = local.common_labels
}

# Cloud Memorystore Redis - Secondary
resource "google_redis_instance" "secondary" {
  count = local.dr_enabled ? 1 : 0

  name           = "erlmcp-redis-secondary-${var.environment}"
  tier           = "STANDARD_HA"
  memory_size_gb = var.redis_memory_gb
  region         = local.secondary_region
  project        = var.project_id

  authorized_network = var.vpc_network_id
  connect_mode       = "PRIVATE_SERVICE_ACCESS"

  redis_version     = "REDIS_6_X"
  display_name      = "erlmcp Redis Secondary"
  reserved_ip_range = var.redis_secondary_ip_range

  replica_count            = 1
  read_replicas_mode       = "READ_REPLICAS_ENABLED"
  transit_encryption_mode  = "SERVER_AUTHENTICATION"
  auth_enabled             = true

  labels = merge(local.common_labels, {
    role = "secondary"
  })
}

# Cloud Pub/Sub Topic - DR Events
resource "google_pubsub_topic" "dr_events" {
  name    = "erlmcp-dr-events-${var.environment}"
  project = var.project_id

  message_storage_policy {
    allowed_persistence_regions = [
      local.primary_region,
      local.secondary_region,
    ]
  }

  labels = local.common_labels
}

# Cloud Pub/Sub Subscription - DR Events
resource "google_pubsub_subscription" "dr_events" {
  name    = "erlmcp-dr-events-sub-${var.environment}"
  topic   = google_pubsub_topic.dr_events.name
  project = var.project_id

  ack_deadline_seconds = 60

  retry_policy {
    minimum_backoff = "10s"
    maximum_backoff = "600s"
  }

  dead_letter_policy {
    dead_letter_topic     = google_pubsub_topic.dr_dlq.id
    max_delivery_attempts = 5
  }

  expiration_policy {
    ttl = ""
  }

  labels = local.common_labels
}

# Cloud Pub/Sub Dead Letter Queue
resource "google_pubsub_topic" "dr_dlq" {
  name    = "erlmcp-dr-dlq-${var.environment}"
  project = var.project_id

  message_storage_policy {
    allowed_persistence_regions = [
      local.primary_region,
      local.secondary_region,
    ]
  }

  labels = local.common_labels
}

# Monitoring Alert Policy - Primary Region Down
resource "google_monitoring_alert_policy" "primary_region_down" {
  display_name = "erlmcp Primary Region Down"
  project      = var.project_id
  combiner     = "OR"

  conditions {
    display_name = "Primary cluster unavailable"

    condition_threshold {
      filter          = "resource.type=\"k8s_cluster\" AND resource.labels.cluster_name=\"${google_container_cluster.primary.name}\" AND metric.type=\"kubernetes.io/container/uptime\""
      duration        = "300s"
      comparison      = "COMPARISON_LT"
      threshold_value = 1

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_RATE"
      }
    }
  }

  notification_channels = var.notification_channels

  alert_strategy {
    auto_close = "1800s"
  }

  user_labels = local.common_labels
}

# Monitoring Alert Policy - Database Replication Lag
resource "google_monitoring_alert_policy" "replication_lag" {
  count = local.dr_enabled ? 1 : 0

  display_name = "erlmcp Database Replication Lag High"
  project      = var.project_id
  combiner     = "OR"

  conditions {
    display_name = "Replication lag exceeds threshold"

    condition_threshold {
      filter          = "resource.type=\"cloudsql_database\" AND resource.labels.database_id=\"${var.project_id}:${google_sql_database_instance.primary.name}\" AND metric.type=\"cloudsql.googleapis.com/database/replication/replica_lag\""
      duration        = "180s"
      comparison      = "COMPARISON_GT"
      threshold_value = local.rpo_minutes * 60

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MAX"
      }
    }
  }

  notification_channels = var.notification_channels

  user_labels = local.common_labels
}

# Cloud Scheduler - Automated Failover Test
resource "google_cloud_scheduler_job" "failover_test" {
  count = var.enable_failover_testing ? 1 : 0

  name        = "erlmcp-failover-test-${var.environment}"
  project     = var.project_id
  region      = local.primary_region
  description = "Automated disaster recovery failover test"
  schedule    = var.failover_test_schedule
  time_zone   = "UTC"

  http_target {
    http_method = "POST"
    uri         = var.failover_test_webhook_url

    headers = {
      "Content-Type" = "application/json"
    }

    body = base64encode(jsonencode({
      action      = "test_failover"
      environment = var.environment
      dry_run     = true
    }))
  }
}

# Cloud Function - Automated Failover Handler
resource "google_cloudfunctions2_function" "failover_handler" {
  count = local.failover_type == "automatic" ? 1 : 0

  name     = "erlmcp-failover-handler-${var.environment}"
  location = local.primary_region
  project  = var.project_id

  build_config {
    runtime     = "go121"
    entry_point = "HandleFailover"
    source {
      storage_source {
        bucket = google_storage_bucket.primary.name
        object = var.failover_function_source_object
      }
    }
  }

  service_config {
    max_instance_count = 1
    available_memory   = "256M"
    timeout_seconds    = 300

    environment_variables = {
      PRIMARY_REGION        = local.primary_region
      SECONDARY_REGION      = local.secondary_region
      PROJECT_ID            = var.project_id
      ENVIRONMENT           = var.environment
      NOTIFICATION_TOPIC    = google_pubsub_topic.dr_events.id
      RTO_MINUTES           = local.rto_minutes
      FAILOVER_TYPE         = local.failover_type
    }
  }

  labels = local.common_labels
}

# Cloud Logging Sink - DR Audit Trail
resource "google_logging_project_sink" "dr_audit" {
  name        = "erlmcp-dr-audit-${var.environment}"
  project     = var.project_id
  destination = "storage.googleapis.com/${google_storage_bucket.multi_region.name}"

  filter = <<-EOT
    resource.type="k8s_cluster"
    OR resource.type="cloudsql_database"
    OR resource.type="cloud_function"
    AND (
      protoPayload.methodName=~".*failover.*"
      OR protoPayload.methodName=~".*promote.*"
      OR severity="ERROR"
    )
  EOT

  unique_writer_identity = true
}

# IAM Binding for Logging Sink
resource "google_storage_bucket_iam_member" "dr_audit_writer" {
  bucket = google_storage_bucket.multi_region.name
  role   = "roles/storage.objectCreator"
  member = google_logging_project_sink.dr_audit.writer_identity
}

# Snapshot Schedule - Persistent Disks
resource "google_compute_resource_policy" "snapshot_schedule" {
  name    = "erlmcp-snapshot-schedule-${var.environment}"
  project = var.project_id
  region  = local.primary_region

  snapshot_schedule_policy {
    schedule {
      hourly_schedule {
        hours_in_cycle = 4
        start_time     = "00:00"
      }
    }

    retention_policy {
      max_retention_days    = var.snapshot_retention_days
      on_source_disk_delete = "KEEP_AUTO_SNAPSHOTS"
    }

    snapshot_properties {
      labels = merge(local.common_labels, {
        type = "automated_dr_snapshot"
      })
      storage_locations = [local.secondary_region]
      guest_flush       = true
    }
  }
}

# Output for DR Status Dashboard
output "disaster_recovery_endpoints" {
  value = {
    primary_region   = local.primary_region
    secondary_region = local.secondary_region
    global_ip        = google_compute_global_address.default.address
    dr_enabled       = local.dr_enabled
    failover_type    = local.failover_type
    rpo_minutes      = local.rpo_minutes
    rto_minutes      = local.rto_minutes
  }
  description = "Disaster recovery configuration endpoints"
}

output "storage_buckets" {
  value = {
    primary        = google_storage_bucket.primary.name
    secondary      = google_storage_bucket.secondary.name
    multi_region   = google_storage_bucket.multi_region.name
  }
  description = "Cross-region storage bucket names"
}

output "database_instances" {
  value = {
    primary   = google_sql_database_instance.primary.connection_name
    secondary = local.dr_enabled ? google_sql_database_instance.secondary_replica[0].connection_name : null
  }
  description = "Database instance connection names"
  sensitive   = true
}

output "gke_clusters" {
  value = {
    primary   = google_container_cluster.primary.endpoint
    secondary = local.dr_enabled ? google_container_cluster.secondary[0].endpoint : null
  }
  description = "GKE cluster endpoints"
  sensitive   = true
}

output "monitoring_topics" {
  value = {
    dr_events = google_pubsub_topic.dr_events.id
    dr_dlq    = google_pubsub_topic.dr_dlq.id
  }
  description = "Pub/Sub topics for DR monitoring"
}
