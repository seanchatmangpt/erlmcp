# ============================================================================
# Compute Engine Deployment Example for erlmcp
# GCE VM with custom image and startup script
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

provider "google" {
  project = var.project_id
  region  = var.region
}

# ============================================================================
# VPC Module
# ============================================================================
module "vpc" {
  source = "../../modules/vpc"

  project_id  = var.project_id
  region      = var.region
  network_name = var.network_name

  # Minimal subnet for GCE VM
  subnets = [{
    name                   = "erlmcp-subnet-${var.region}"
    region                 = var.region
    ip_cidr_range          = var.subnet_cidr
    private_ip_google_access = true
    purpose                = null
    role                   = null
    secondary_ip_ranges    = []
  }]

  # Create NAT for private instances
  create_router = !var.public_ip
  create_nat    = !var.public_ip
}

# ============================================================================
# Secret Manager Module
# ============================================================================
module "secrets" {
  source = "../../modules/secret-manager"

  project_id = var.project_id
  labels     = var.labels
  secret_accessors = [
    "serviceAccount:${module.compute_engine.service_account_email}",
  ]
}

# ============================================================================
# Compute Engine Module
# ============================================================================
module "compute_engine" {
  source = "../../modules/compute-engine"

  project_id = var.project_id
  zone       = var.zone

  instance_name = var.instance_name
  instance_count = var.instance_count
  machine_type   = var.machine_type

  # Use custom erlmcp image if available, otherwise Ubuntu
  source_image = var.source_image

  # Disk configuration
  disk_type   = var.disk_type
  disk_size_gb = var.disk_size_gb

  # Network configuration
  network    = module.vpc.network_name
  subnetwork = module.vpc.subnet_names[0]

  # Security
  enable_secure_boot          = var.enable_secure_boot
  enable_integrity_monitoring = var.enable_integrity_monitoring

  # Firewall
  create_firewall_rules = var.create_firewall_rules
  http_source_ranges    = var.http_source_ranges

  # Service account with Secret Manager access
  create_service_account = true

  # Metadata
  metadata = {
    environment = var.environment
  }

  # Labels
  labels = var.labels
}

# ============================================================================
# Observability Module
# ============================================================================
module "observability" {
  source = "../../modules/observability"

  project_id = var.project_id

  # Notification channels
  notification_channels = var.notification_channels

  # Uptime check
  create_uptime_check = var.create_uptime_check
  uptime_check_host   = var.uptime_check_host

  # SLOs
  create_slos = var.create_slos

  # Alert policies
  enable_error_rate_alert   = var.enable_error_rate_alert
  enable_latency_alert      = var.enable_latency_alert
  enable_memory_alert       = var.enable_memory_alert
  enable_cpu_alert          = var.enable_cpu_alert
  enable_health_check_alert = var.enable_health_check_alert

  # Dashboards
  create_performance_dashboard = true
  create_erlang_dashboard    = true
  create_security_dashboard  = true
}

# ============================================================================
# Load Balancer (optional, for HA deployment)
# ============================================================================
resource "google_compute_forwarding_rule" "erlmcp" {
  count   = var.create_load_balancer ? 1 : 0
  project  = var.project_id
  name     = "${var.instance_name}-forwarding-rule"
  region   = var.region
  target   = google_compute_target_pool.erlmcp[0].id
  port_range = "8080"

  load_balancing_scheme = "EXTERNAL"
}

resource "google_compute_target_pool" "erlmcp" {
  count   = var.create_load_balancer ? 1 : 0
  project  = var.project_id
  name     = "${var.instance_name}-pool"
  region   = var.region

  instances = module.compute_engine.instance_self_links

  health_checks = [google_compute_http_health_check.erlmcp[0].id]
}

resource "google_compute_http_health_check" "erlmcp" {
  count   = var.create_load_balancer ? 1 : 0
  project  = var.project_id
  name     = "${var.instance_name}-health-check"
  port     = 8080
  request_path = var.health_check_path
}

# ============================================================================
# Outputs
# ============================================================================
output "instance_names" {
  description = "VM instance names"
  value       = module.compute_engine.instance_names
}

output "instance_external_ips" {
  description = "VM external IP addresses"
  value       = module.compute_engine.instance_external_ips
}

output "ssh_command" {
  description = "SSH command to connect to first instance"
  value       = module.compute_engine.ssh_command
}

output "health_check_url" {
  description = "Health check URL (first instance)"
  value       = module.compute_engine.health_check_url
}

output "load_balancer_ip" {
  description = "Load balancer IP (if configured)"
  value       = var.create_load_balancer ? google_compute_forwarding_rule.erlmcp[0].ip_address : null
}

output "service_account_email" {
  description = "Service account email"
  value       = module.compute_engine.service_account_email
}
