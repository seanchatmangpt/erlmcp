# ============================================================================
# Compute Engine Deployment for erlmcp
# ============================================================================
# VM-based deployment with Managed Instance Groups and Load Balancing
#
# DOCKER-ONLY CONSTITUTION: VM images built via Packer, validated in Docker
# ============================================================================

# ============================================================================
# Data Sources
# ============================================================================

data "google_compute_image" "erlmcp" {
  count   = local.deploy_gce ? 1 : 0
  family  = var.gce_image_family
  project = var.gce_image_project != "" ? var.gce_image_project : var.project_id
}

# ============================================================================
# Instance Template
# ============================================================================

resource "google_compute_instance_template" "erlmcp" {
  count = local.deploy_gce ? 1 : 0

  name_prefix  = "${local.name_prefix}-"
  project      = var.project_id
  region       = var.region
  machine_type = var.gce_machine_type

  tags = ["erlmcp", "http-server", "https-server"]

  labels = local.common_labels

  disk {
    source_image = data.google_compute_image.erlmcp[0].self_link
    auto_delete  = true
    boot         = true
    disk_type    = "pd-ssd"
    disk_size_gb = var.gce_disk_size
  }

  network_interface {
    network    = google_compute_network.main.id
    subnetwork = google_compute_subnetwork.main.id

    # External IP only if not using NAT
    dynamic "access_config" {
      for_each = var.gce_assign_public_ip ? [1] : []
      content {
        network_tier = "PREMIUM"
      }
    }
  }

  service_account {
    email  = google_service_account.erlmcp.email
    scopes = ["cloud-platform"]
  }

  scheduling {
    automatic_restart   = true
    on_host_maintenance = "MIGRATE"
    preemptible         = var.gce_preemptible
  }

  shielded_instance_config {
    enable_secure_boot          = true
    enable_vtpm                 = true
    enable_integrity_monitoring = true
  }

  metadata = {
    "enable-oslogin"     = var.enable_os_login ? "TRUE" : "FALSE"
    "erlmcp-environment" = var.environment
    "erlmcp-version"     = var.erlmcp_version
    "erlmcp-secrets"     = join(",", [for s in var.gce_secrets : "${s.secret_id}:${s.env_var}"])

    "startup-script" = <<-EOF
      #!/bin/bash
      set -euo pipefail

      # Log startup
      echo "$(date): Starting erlmcp initialization" >> /var/log/erlmcp-startup.log

      # Fetch secrets from Secret Manager
      /opt/erlmcp/bin/fetch-secrets.sh

      # Configure node name from hostname
      HOSTNAME=$(hostname -f)
      sed -i "s/-name erlmcp@127.0.0.1/-name erlmcp@$HOSTNAME/" /etc/erlmcp/vm.args

      # Start erlmcp service
      systemctl start erlmcp

      echo "$(date): erlmcp started successfully" >> /var/log/erlmcp-startup.log
    EOF
  }

  lifecycle {
    create_before_destroy = true
  }
}

# ============================================================================
# Managed Instance Group
# ============================================================================

resource "google_compute_region_instance_group_manager" "erlmcp" {
  count = local.deploy_gce ? 1 : 0

  name               = "${local.name_prefix}-mig"
  project            = var.project_id
  region             = var.region
  base_instance_name = "${local.name_prefix}-vm"

  version {
    instance_template = google_compute_instance_template.erlmcp[0].self_link_unique
  }

  target_size = var.gce_min_instances

  named_port {
    name = "http"
    port = 8080
  }

  named_port {
    name = "metrics"
    port = 9568
  }

  auto_healing_policies {
    health_check      = google_compute_health_check.erlmcp[0].id
    initial_delay_sec = 300
  }

  update_policy {
    type                           = "PROACTIVE"
    minimal_action                 = "REPLACE"
    most_disruptive_allowed_action = "REPLACE"
    max_surge_fixed                = var.gce_max_surge
    max_unavailable_fixed          = 0
    replacement_method             = "SUBSTITUTE"
  }

  stateful_disk {
    device_name = "persistent-disk-0"
    delete_rule = "ON_PERMANENT_INSTANCE_DELETION"
  }

  lifecycle {
    create_before_destroy = true
  }
}

# ============================================================================
# Autoscaler
# ============================================================================

resource "google_compute_region_autoscaler" "erlmcp" {
  count = local.deploy_gce ? 1 : 0

  name    = "${local.name_prefix}-autoscaler"
  project = var.project_id
  region  = var.region
  target  = google_compute_region_instance_group_manager.erlmcp[0].id

  autoscaling_policy {
    min_replicas    = var.gce_min_instances
    max_replicas    = var.gce_max_instances
    cooldown_period = 180

    cpu_utilization {
      target = var.gce_cpu_target
    }

    # Custom metric for request rate
    dynamic "metric" {
      for_each = var.gce_custom_metrics ? [1] : []
      content {
        name   = "custom.googleapis.com/erlmcp/request_rate"
        target = var.gce_requests_target
        type   = "GAUGE"
      }
    }

    scale_in_control {
      max_scaled_in_replicas {
        fixed = 1
      }
      time_window_sec = 600
    }
  }
}

# ============================================================================
# Health Check
# ============================================================================

resource "google_compute_health_check" "erlmcp" {
  count = local.deploy_gce ? 1 : 0

  name    = "${local.name_prefix}-health-check"
  project = var.project_id

  check_interval_sec  = 30
  timeout_sec         = 10
  healthy_threshold   = 2
  unhealthy_threshold = 3

  http_health_check {
    port         = 8080
    request_path = "/health"
  }

  log_config {
    enable = true
  }
}

# ============================================================================
# Backend Service
# ============================================================================

resource "google_compute_backend_service" "erlmcp" {
  count = local.deploy_gce ? 1 : 0

  name        = "${local.name_prefix}-backend"
  project     = var.project_id
  protocol    = "HTTP"
  port_name   = "http"
  timeout_sec = 30

  health_checks = [google_compute_health_check.erlmcp[0].id]

  backend {
    group           = google_compute_region_instance_group_manager.erlmcp[0].instance_group
    balancing_mode  = "UTILIZATION"
    capacity_scaler = 1.0
    max_utilization = 0.8
  }

  load_balancing_scheme = "EXTERNAL_MANAGED"
  locality_lb_policy    = "ROUND_ROBIN"

  log_config {
    enable      = true
    sample_rate = 1.0
  }

  cdn_policy {
    cache_mode        = "CACHE_ALL_STATIC"
    default_ttl       = 3600
    max_ttl           = 86400
    negative_caching  = true
    serve_while_stale = 86400
  }

  connection_draining_timeout_sec = 300

  circuit_breakers {
    max_connections             = 10000
    max_pending_requests        = 1000
    max_requests                = 10000
    max_requests_per_connection = 100
    max_retries                 = 3
  }
}

# ============================================================================
# URL Map
# ============================================================================

resource "google_compute_url_map" "erlmcp" {
  count = local.deploy_gce ? 1 : 0

  name            = "${local.name_prefix}-url-map"
  project         = var.project_id
  default_service = google_compute_backend_service.erlmcp[0].id

  host_rule {
    hosts        = ["*"]
    path_matcher = "allpaths"
  }

  path_matcher {
    name            = "allpaths"
    default_service = google_compute_backend_service.erlmcp[0].id

    path_rule {
      paths   = ["/health", "/health/*"]
      service = google_compute_backend_service.erlmcp[0].id
    }

    path_rule {
      paths   = ["/metrics", "/metrics/*"]
      service = google_compute_backend_service.erlmcp[0].id
    }
  }
}

# ============================================================================
# SSL Certificate (Managed)
# ============================================================================

resource "google_compute_managed_ssl_certificate" "erlmcp" {
  count = local.deploy_gce && var.gce_domain != "" ? 1 : 0

  name    = "${local.name_prefix}-cert"
  project = var.project_id

  managed {
    domains = [var.gce_domain]
  }
}

# ============================================================================
# HTTPS Proxy
# ============================================================================

resource "google_compute_target_https_proxy" "erlmcp" {
  count = local.deploy_gce && var.gce_domain != "" ? 1 : 0

  name             = "${local.name_prefix}-https-proxy"
  project          = var.project_id
  url_map          = google_compute_url_map.erlmcp[0].id
  ssl_certificates = [google_compute_managed_ssl_certificate.erlmcp[0].id]
}

# ============================================================================
# HTTP Proxy (redirect to HTTPS)
# ============================================================================

resource "google_compute_target_http_proxy" "erlmcp" {
  count = local.deploy_gce ? 1 : 0

  name    = "${local.name_prefix}-http-proxy"
  project = var.project_id
  url_map = var.gce_domain != "" ? google_compute_url_map.redirect[0].id : google_compute_url_map.erlmcp[0].id
}

resource "google_compute_url_map" "redirect" {
  count = local.deploy_gce && var.gce_domain != "" ? 1 : 0

  name    = "${local.name_prefix}-redirect"
  project = var.project_id

  default_url_redirect {
    https_redirect = true
    strip_query    = false
  }
}

# ============================================================================
# Global Forwarding Rules
# ============================================================================

resource "google_compute_global_address" "erlmcp" {
  count = local.deploy_gce ? 1 : 0

  name    = "${local.name_prefix}-ip"
  project = var.project_id
}

resource "google_compute_global_forwarding_rule" "https" {
  count = local.deploy_gce && var.gce_domain != "" ? 1 : 0

  name       = "${local.name_prefix}-https-rule"
  project    = var.project_id
  target     = google_compute_target_https_proxy.erlmcp[0].id
  ip_address = google_compute_global_address.erlmcp[0].address
  port_range = "443"

  load_balancing_scheme = "EXTERNAL_MANAGED"

  labels = local.common_labels
}

resource "google_compute_global_forwarding_rule" "http" {
  count = local.deploy_gce ? 1 : 0

  name       = "${local.name_prefix}-http-rule"
  project    = var.project_id
  target     = google_compute_target_http_proxy.erlmcp[0].id
  ip_address = google_compute_global_address.erlmcp[0].address
  port_range = "80"

  load_balancing_scheme = "EXTERNAL_MANAGED"

  labels = local.common_labels
}

# ============================================================================
# DNS Record (if Cloud DNS zone provided)
# ============================================================================

resource "google_dns_record_set" "erlmcp" {
  count = local.deploy_gce && var.gce_dns_zone != "" && var.gce_domain != "" ? 1 : 0

  name         = "${var.gce_domain}."
  type         = "A"
  ttl          = 300
  managed_zone = var.gce_dns_zone
  project      = var.project_id
  rrdatas      = [google_compute_global_address.erlmcp[0].address]
}

# ============================================================================
# Compute Engine Outputs
# ============================================================================

output "gce_load_balancer_ip" {
  description = "Load balancer IP address"
  value       = local.deploy_gce ? google_compute_global_address.erlmcp[0].address : null
}

output "gce_instance_group" {
  description = "Managed instance group name"
  value       = local.deploy_gce ? google_compute_region_instance_group_manager.erlmcp[0].name : null
}

output "gce_url" {
  description = "Compute Engine service URL"
  value       = local.deploy_gce && var.gce_domain != "" ? "https://${var.gce_domain}" : (local.deploy_gce ? "http://${google_compute_global_address.erlmcp[0].address}" : null)
}
