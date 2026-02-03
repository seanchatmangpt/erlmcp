# erlmcp GCE VM Module
# This module deploys erlmcp on Google Compute Engine with auto-scaling

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = "~> 5.0"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.5"
    }
  }
}

variable "project_id" {
  type        = string
  description = "The GCP project ID"
}

variable "region" {
  type        = string
  default     = "us-central1"
  description = "The region to deploy resources in"
}

variable "zone" {
  type        = string
  default     = "us-central1-a"
  description = "The zone to deploy resources in"
}

variable "instance_name" {
  type        = string
  default     = "erlmcp-server"
  description = "Base name for VM instances"
}

variable "machine_type" {
  type        = string
  default     = "e2-standard-4"
  description = "Machine type for VM instances"
}

variable "min_instances" {
  type        = number
  default     = 2
  description = "Minimum number of instances"
}

variable "max_instances" {
  type        = number
  default     = 10
  description = "Maximum number of instances"
}

variable "target_cpu_utilization" {
  type        = number
  default     = 0.7
  description = "Target CPU utilization for auto-scaling"
}

variable "image_name" {
  type        = string
  default     = "erlmcp-marketplace-v1"
  description = "Name of the custom VM image to use"
}

variable "vpc_network" {
  type        = string
  default     = "default"
  description = "VPC network name"
}

variable "vpc_subnetwork" {
  type        = string
  default     = "default"
  description = "VPC subnetwork name"
}

variable "enable_http" {
  type        = bool
  default     = true
  description = "Enable HTTP health checks"
}

variable "enable_https" {
  type        = bool
  default     = true
  description = "Enable HTTPS health checks"
}

variable "ssl_certificate" {
  type        = string
  default     = ""
  description = "SSL certificate name for HTTPS"
}

variable "service_account_email" {
  type        = string
  default     = ""
  description = "Service account email for instances"
}

variable "service_account_roles" {
  type        = list(string)
  default     = ["roles/secretmanager.secretAccessor", "roles/logging.logWriter", "roles/monitoring.metricWriter"]
  description = "IAM roles for service account"
}

variable "labels" {
  type        = map(string)
  default     = {
    app = "erlmcp"
    role = "server"
    type = "marketplace"
  }
  description = "Labels to apply to resources"
}

variable "tags" {
  type        = list(string)
  default     = ["erlmcp", "http-server", "https-server"]
  description = "Network tags to apply to instances"
}

# Generate random suffix for resource names
resource "random_string" "suffix" {
  length  = 8
  special = false
  upper   = false
}

# Service account
resource "google_service_account" "erlmcp_sa" {
  account_id   = "erlmcp-sa-${random_string.suffix.result}"
  display_name = "erlmcp Service Account"
  description  = "Service account for erlmcp VM instances"
  project      = var.project_id
}

# Grant IAM roles to service account
resource "google_project_iam_member" "erlmcp_sa_roles" {
  for_each = toset(var.service_account_roles)

  project = var.project_id
  role    = each.value
  member  = "serviceAccount:${google_service_account.erlmcp_sa.email}"
}

# Custom VM image
resource "google_compute_image" "erlmcp_image" {
  name         = "erlmcp-vm-${random_string.suffix.result}"
  family       = "erlmcp-vm"
  source_image = "debian-12-bookworm-v20240117"
  description  = "erlmcp Marketplace VM image with Docker and Ops Agent"
  project      = var.project_id

  guest_os_features {
    type = "UEFI_COMPATIBLE"
  }

  depends_on = [
    google_project_iam_member.erlmcp_sa_roles
  ]
}

# Instance template
resource "google_compute_instance_template" "erlmcp_template" {
  name         = "erlmcp-template-${random_string.suffix.result}"
  description  = "Template for erlmcp VM instances"
  project      = var.project_id
  region       = var.region

  machine_type = var.machine_type

  # Boot disk
  disk {
    source_image = google_compute_image.erlmcp_image.self_link
    boot         = true
    auto_delete  = true
    disk_type    = "pd-balanced"
    size         = 50
  }

  # Network interface
  network_interface {
    network    = var.vpc_network
    subnetwork = var.vpc_subnetwork
    access_config {
      // Egress-only configuration
    }
  }

  # Metadata
  metadata = {
    gce-container-declaration = file("${path.module}/docker-container.yaml")
    enable-guest-agent       = "true"
    startup-script            = file("${path.module}/start-erlmcp.sh")
  }

  # Shielded VM configuration
  shielded_instance_config {
    enable_secure_boot          = true
    enable_vtpm                 = true
    enable_integrity_monitoring = true
  }

  # Tags
  tags = var.tags

  # Labels
  labels = var.labels

  # Service account
  service_account {
    email  = google_service_account.erlmcp_sa.email
    scopes = ["cloud-platform"]
  }

  # Lifecycle configuration
  lifecycle {
    create_before_destroy = true
  }
}

# Instance group manager
resource "google_compute_instance_group_manager" "erlmcp_group" {
  name     = "erlmcp-group-${random_string.suffix.result}"
  base_instance_name = var.instance_name
  size     = var.min_instances
  project  = var.project_id
  zone     = var.zone

  instance_template  = google_compute_instance_template.erlmcp_template.id
  target_size        = var.min_instances

  auto_healing_policies {
    health_check      = google_compute_health_check.erlmcp_health.self_link
    initial_delay_sec = 300
    max_replicas      = var.max_instances
    min_replicas      = var.min_instances
  }

  update_policy {
    minimal_action         = "RESTART"
    type                   = "PROACTIVE"
    instance_redistribution_type = "PROACTIVE"
    max_surge {
      fixed = 2
    }
    max_unavailable {
      fixed = 1
    }
  }
}

# Regional instance group manager (for multi-zone availability)
resource "google_compute_region_instance_group_manager" "erlmcp_region_group" {
  name      = "erlmcp-region-group-${random_string.suffix.result}"
  base_instance_name = var.instance_name
  size      = var.min_instances
  project   = var.project_id
  region    = var.region

  instance_template  = google_compute_instance_template.erlmcp_template.id

  auto_healing_policies {
    health_check      = google_compute_health_check.erlmcp_health.self_link
    initial_delay_sec = 300
  }

  target_size = var.min_instances

  update_policy {
    minimal_action = "RESTART"
    type           = "PROACTIVE"
  }
}

# Health check
resource "google_compute_health_check" "erlmcp_health" {
  name               = "erlmcp-health-check-${random_string.suffix.result}"
  project            = var.project_id
  check_interval_sec = 5
  timeout_sec        = 5
  healthy_threshold  = 2
  unhealthy_threshold = 3

  http_health_check {
    port         = 9090
    path         = "/health"
    request_path = "/health"
  }

  log_config {
    enable = false
  }
}

# Backend service
resource "google_compute_backend_service" "erlmcp_backend" {
  name                  = "erlmcp-backend-${random_string.suffix.result}"
  project               = var.project_id
  port_name             = "http"
  protocol              = "HTTP"
  load_balancing_scheme = "EXTERNAL"
  timeout_sec           = 30

  backend {
    group           = google_compute_region_instance_group_manager.erlmcp_region_group.instance_group
    balancing_mode  = "UTILIZATION"
    max_utilization = var.target_cpu_utilization
  }

  health_checks = [google_compute_health_check.erlmcp_health.self_link]

  log_config {
    enable = false
  }
}

# URL map
resource "google_compute_url_map" "erlmcp_url_map" {
  name            = "erlmcp-url-map-${random_string.suffix.result}"
  project         = var.project_id

  default_service = google_compute_backend_service.erlmcp_backend.id

  # Add HTTPS redirect if enabled
  dynamic "redirect_response" {
    for_each = var.enable_https ? [1] : []

    content {
      strip_query    = false
      redirect_code  = 301
      host_redirect  = true
      path_redirect  = "/"
    }
  }
}

# Target HTTPS proxy (if HTTPS enabled)
resource "google_compute_target_https_proxy" "erlmcp_https_proxy" {
  count = var.enable_https ? 1 : 0

  name          = "erlmcp-https-proxy-${random_string.suffix.result}"
  project       = var.project_id
  ssl_certificates = [google_compute_ssl_certificate.erlmcp_ssl[0].id]
  url_map      = google_compute_url_map.erlmcp_url_map.id
}

# SSL certificate (if provided)
resource "google_compute_ssl_certificate" "erlmcp_ssl" {
  count = var.ssl_certificate != "" ? 1 : 0

  name        = "erlmcp-ssl-cert-${random_string.suffix.result}"
  project     = var.project_id
  description = "SSL certificate for erlmcp"

  managed {
    domains = [var.ssl_certificate]
  }
}

# Global forwarding rule (HTTP)
resource "google_compute_global_forwarding_rule" "erlmcp_http" {
  name       = "erlmcp-http-${random_string.suffix.result}"
  project    = var.project_id
  target     = google_compute_target_http_proxy.erlmcp_http_proxy.id
  port_range = "80"
  ip_address = google_compute_global_address.erlmcp_http.address
}

# Global forwarding rule (HTTPS)
resource "google_compute_global_forwarding_rule" "erlmcp_https" {
  count = var.enable_https ? 1 : 0

  name       = "erlmcp-https-${random_string.suffix.result}"
  project    = var.project_id
  target     = google_compute_target_https_proxy.erlmcp_https_proxy[0].id
  port_range = "443"
  ip_address = google_compute_global_address.erlmcp_https.address
}

# Target HTTP proxy
resource "google_compute_target_http_proxy" "erlmcp_http_proxy" {
  name          = "erlmcp-http-proxy-${random_string.suffix.result}"
  project       = var.project_id
  url_map      = google_compute_url_map.erlmcp_url_map.id
}

# Global IP addresses
resource "google_compute_global_address" "erlmcp_http" {
  name       = "erlmcp-http-ip-${random_string.suffix.result}"
  project    = var.project_id
  ip_version = "IPV4"
}

resource "google_compute_global_address" "erlmcp_https" {
  count = var.enable_https ? 1 : 0

  name       = "erlmcp-https-ip-${random_string.suffix.result}"
  project    = var.project_id
  ip_version = "IPV4"
}

# Autoscaler
resource "google_compute_autoscaler" "erlmcp_autoscaler" {
  name   = "erlmcp-autoscaler-${random_string.suffix.result}"
  project = var.project_id
  zone   = var.zone
  target = google_compute_instance_group_manager.erlmcp_group.id

  autoscaling_policy {
    max_replicas         = var.max_instances
    min_replicas         = var.min_instances
    cooldown_period      = 60
    cpu_utilization {
      target = var.target_cpu_utilization
    }
  }
}

# Outputs
output "service_account_email" {
  value = google_service_account.erlmcp_sa.email
}

output "http_ip_address" {
  value = google_compute_global_address.erlmcp_http.address
}

output "https_ip_address" {
  value = var.enable_https ? google_compute_global_address.erlmcp_https[0].address : null
}

output "load_balancer_ip" {
  value = google_compute_global_address.erlmcp_http.address
}

output "instance_group_manager" {
  value = google_compute_instance_group_manager.erlmcp_group.name
}

output "health_check_endpoint" {
  value = "http://${google_compute_global_address.erlmcp_http.address}:9090/health"
}

output "metrics_endpoint" {
  value = "http://${google_compute_global_address.erlmcp_http.address}:9100/metrics"
}

output "api_endpoint" {
  value = "http://${google_compute_global_address.erlmcp_http.address}:8080"
}