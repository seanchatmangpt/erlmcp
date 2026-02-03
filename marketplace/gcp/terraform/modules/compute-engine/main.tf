# ============================================================================
# Compute Engine Module for erlmcp Deployment
# Google Compute Engine VM with custom image
# ============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.0.0"
    }
  }
}

# ============================================================================
# Enable Required APIs
# ============================================================================
resource "google_project_service" "compute" {
  project            = var.project_id
  service            = "compute.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "secret_manager" {
  project            = var.project_id
  service            = "secretmanager.googleapis.com"
  disable_on_destroy = false
}

# ============================================================================
# Service Account
# ============================================================================
resource "google_service_account" "erlmcp" {
  count        = var.create_service_account ? 1 : 0
  project      = var.project_id
  account_id   = "${var.instance_name}-sa"
  display_name = "erlmcp Compute Engine Service Account"
  description  = "Service account for erlmcp VM instances"
}

# ============================================================================
# IAM Bindings for Secret Manager Access
# ============================================================================
resource "google_project_iam_member" "secret_accessor" {
  count   = var.create_service_account ? 1 : 0
  project = var.project_id
  role    = "roles/secretmanager.secretAccessor"
  member  = "serviceAccount:${google_service_account.erlmcp[0].email}"
}

resource "google_project_iam_member" "logging_writer" {
  count   = var.create_service_account ? 1 : 0
  project = var.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.erlmcp[0].email}"
}

resource "google_project_iam_member" "monitoring_metric_writer" {
  count   = var.create_service_account ? 1 : 0
  project = var.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.erlmcp[0].email}"
}

# ============================================================================
# Compute Instance
# ============================================================================
resource "google_compute_instance" "erlmcp" {
  count        = var.instance_count
  name         = "${var.instance_name}-${count.index + 1}"
  project      = var.project_id
  zone         = var.zone
  machine_type = var.machine_type

  # Boot disk
  boot_disk {
    initialize_params {
      image  = var.source_image
      type   = var.disk_type
      disk_size_gb = var.disk_size_gb
      labels = var.disk_labels
    }
    auto_delete = var.auto_delete_disk
    mode        = "READ_WRITE"
  }

  # Additional disks
  dynamic "attached_disk" {
    for_each = var.additional_disks
    content {
      source      = attached_disk.value.source
      device_name = attached_disk.value.device_name
      mode        = attached_disk.value.mode
    }
  }

  # Service account
  dynamic "service_account" {
    for_each = var.create_service_account ? [1] : []
    content {
      email  = google_service_account.erlmcp[0].email
      scopes = var.instance_scopes
    }
  }

  # Network interface
  network_interface {
    network    = var.network
    subnetwork = var.subnetwork
    access_config {
      # Ephemeral public IP (remove for private only)
      dynamic "nat_ip_address" {
        for_each = var.static_nat_ip_address != "" ? [1] : []
        content {
          nat_ip_address = var.static_nat_ip_address
        }
      }
      network_tier = var.network_tier
    }
    dynamic "ipv6_access_config" {
      for_each = var.enable_ipv6 ? [1] : []
      content {
        network_tier = var.network_tier
      }
    }
  }

  # Shielded VM configuration
  shielded_instance_config {
    enable_secure_boot          = var.enable_secure_boot
    enable_vtpm                 = var.enable_vtpm
    enable_integrity_monitoring  = var.enable_integrity_monitoring
  }

  # Confidential computing
  confidential_instance_config {
    enable_confidential_compute = var.enable_confidential_compute
  }

  # Metadata
  metadata = merge(
    var.metadata,
    {
      ssh-keys               = var.ssh_keys != "" ? var.ssh_keys : null
      startup-script         = var.startup_script
      startup-script-url     = var.startup_script_url
      shutdown-script        = var.shutdown_script
      google-logging-enabled = "true"
      google-monitoring-enabled = "true"
    }
  )

  metadata_startup_script = var.metadata_startup_script

  # Labels
  labels = merge(
    var.labels,
    {
      managed-by = "terraform"
      app        = "erlmcp"
      instance   = "${var.instance_name}-${count.index + 1}"
    }
  )

  # Tags
  tags = concat(var.tags, ["erlmcp"])

  # Scheduling
  scheduling {
    preemptible         = var.preemptible
    on_host_maintenance = var.on_host_maintenance
    automatic_restart   = var.automatic_restart
    min_node_cpus       = var.min_node_cpus
    provisioning_model  = var.provisioning_model
    dynamic "node_affinities" {
      for_each = var.node_affinities
      content {
        key      = node_affinities.value.key
        operator = node_affinities.value.operator
        values   = node_affinities.value.values
      }
    }
  }

  # Allow stopping instance
  allow_stopping_for_update = var.allow_stopping_for_update

  # Deletion protection
  deletion_protection = var.deletion_protection

  # Resource policies
  dynamic "scheduling" {
    for_each = var.resource_policy_urls != [] ? [1] : []
    content {
      scheduling = var.resource_policy_urls
    }
  }

  # depends_on
  depends_on = [
    google_project_service.compute
  ]

  lifecycle {
    create_before_destroy = true
    ignore_changes = [
      attached_disk[0].source,
      metadata["ssh-keys"]
    ]
  }
}

# ============================================================================
# Compute Instance Template (for managed instance groups)
# ============================================================================
resource "google_compute_instance_template" "erlmcp" {
  count       = var.create_instance_template ? 1 : 0
  name_prefix = "${var.instance_name}-template-"
  project     = var.project_id
  machine_type = var.machine_type

  # Labels
  labels = merge(
    var.labels,
    {
      managed-by = "terraform"
      app        = "erlmcp"
    }
  )

  # Tags
  tags = concat(var.tags, ["erlmcp"])

  # Scheduling
  scheduling {
    preemptible         = var.preemptible
    on_host_maintenance = var.on_host_maintenance
    automatic_restart   = var.automatic_restart
  }

  # Disk configuration
  disk {
    source_image = var.source_image
    auto_delete  = true
    boot         = true
    disk_disk_size_gb = var.disk_size_gb
    disk_type    = var.disk_type
  }

  # Network configuration
  network_interface {
    network    = var.network
    subnetwork = var.subnetwork
    access_config {
      network_tier = var.network_tier
    }
  }

  # Service account
  dynamic "service_account" {
    for_each = var.create_service_account ? [1] : []
    content {
      email  = google_service_account.erlmcp[0].email
      scopes = var.instance_scopes
    }
  }

  # Metadata
  metadata = merge(
    var.metadata,
    {
      google-logging-enabled    = "true"
      google-monitoring-enabled = "true"
    }
  )

  # Shielded VM
  shielded_instance_config {
    enable_secure_boot          = var.enable_secure_boot
    enable_vtpm                 = var.enable_vtpm
    enable_integrity_monitoring  = var.enable_integrity_monitoring
  }

  lifecycle {
    create_before_destroy = true
  }
}

# ============================================================================
# Managed Instance Group
# ============================================================================
resource "google_compute_instance_group_manager" "erlmcp" {
  count   = var.create_instance_group ? 1 : 0
  project = var.project_id
  name    = "${var.instance_name}-mig"
  zone    = var.zone

  # Base instance name
  base_instance_name = var.instance_name

  # Instance template
  version {
    instance_template = google_compute_instance_template.erlmcp[0].id
  }

  # Target size
  target_size = var.target_size

  # Auto-scaling
  dynamic "auto_healing_policies" {
    for_each = var.enable_auto_healing ? [1] : []
    content {
      health_check      = google_compute_health_check.erlmcp[0].id
      initial_delay_sec = var.auto_healing_initial_delay
    }
  }

  dynamic "autoscaling_policy" {
    for_each = var.enable_autoscaling ? [1] : []
    content {
      max_replicas    = var.max_replicas
      min_replicas    = var.min_replicas
      cooldown_period = var.cooldown_period

      dynamic "cpu_utilization" {
        for_each = var.scale_based_on_cpu ? [1] : []
        content {
          target = var.cpu_utilization_target
        }
      }

      dynamic "load_balancing_utilization" {
        for_each = var.scale_based_on_lb ? [1] : []
        content {
          target = var.lb_utilization_target
        }
      }
    }
  }

  # Named ports
  named_port {
    name = "http"
    port = 8080
  }

  named_port {
    name = "metrics"
    port = 9100
  }

  # Update policy
  update_policy {
    type                  = var.update_type
    minimal_action         = var.minimal_action
    update_strategy = var.update_strategy
    max_surge_fixed        = var.max_surge
    max_unavailable_fixed  = var.max_unavailable
    min_ready_instances          = var.min_ready_instances
  }

  # All instances config
  all_instances_config {
    metadata = var.metadata
  }
}

# ============================================================================
# Health Check
# ============================================================================
resource "google_compute_health_check" "erlmcp" {
  count   = var.create_health_check ? 1 : 0
  project = var.project_id
  name    = "${var.instance_name}-health-check"

  check_interval_sec  = var.check_interval
  timeout_sec         = var.timeout
  healthy_threshold   = var.healthy_threshold
  unhealthy_threshold = var.unhealthy_threshold

  http_health_check {
    port         = var.health_check_port
    request_path = var.health_check_path
    proxy_header = "NONE"
    port_specification = "USE_FIXED_PORT"
  }

  log_config {
    enable = var.enable_health_check_logging
  }
}

# ============================================================================
# Firewall Rules
# ============================================================================
resource "google_compute_firewall" "erlmcp-http" {
  count   = var.create_firewall_rules ? 1 : 0
  project = var.project_id
  name    = "${var.instance_name}-allow-http"
  network = var.network
  direction = "INGRESS"

  allow {
    protocol = "tcp"
    ports    = ["8080"]
  }

  source_ranges = var.http_source_ranges
  target_tags  = concat(var.tags, ["erlmcp"])
}

resource "google_compute_firewall" "erlmcp-metrics" {
  count   = var.create_firewall_rules ? 1 : 0
  project = var.project_id
  name    = "${var.instance_name}-allow-metrics"
  network = var.network
  direction = "INGRESS"

  allow {
    protocol = "tcp"
    ports    = ["9100"]
  }

  source_ranges = var.metrics_source_ranges
  target_tags  = concat(var.tags, ["erlmcp"])
}

resource "google_compute_firewall" "erlmcp-distribution" {
  count   = var.create_firewall_rules ? 1 : 0
  project = var.project_id
  name    = "${var.instance_name}-allow-distribution"
  network = var.network
  direction = "INGRESS"

  allow {
    protocol = "tcp"
    ports    = ["9100-9200"]
  }

  source_ranges = var.distribution_source_ranges
  target_tags  = concat(var.tags, ["erlmcp"])
}

# ============================================================================
# Static IP (optional)
# ============================================================================
resource "google_compute_address" "erlmcp" {
  count   = var.create_static_ip ? 1 : 0
  project = var.project_id
  name    = "${var.instance_name}-ip"
  region  = var.region

  address_type = var.ip_address_type
}

# ============================================================================
# Startup Script (Secret Injection)
# ============================================================================
data "template_file" "startup_script" {
  template = file("${path.module}/scripts/startup.sh.tpl")

  vars = {
    PROJECT_ID         = var.project_id
    SECRET_NAME_PREFIX = "erlmcp"
    LOG_LEVEL          = var.log_level
    ERLANG_COOKIE_SECRET = "erlmcp-erlang-cookie"
  }
}
