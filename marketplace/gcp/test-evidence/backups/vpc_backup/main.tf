# ============================================================================
# VPC Module for erlmcp Deployment
# Google VPC networking with private clusters
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
# VPC Network
# ============================================================================
resource "google_compute_network" "erlmcp" {
  count                             = var.create_network ? 1 : 0
  project                           = var.project_id
  name                              = var.network_name
  description                       = "VPC network for erlmcp deployment"
  auto_create_subnetworks           = var.auto_create_subnetworks
  routing_mode                      = var.routing_mode
  delete_default_routes_on_create   = var.delete_default_routes
  mtu                               = var.mtu

  depends_on = [
    google_project_service.compute
  ]
}

# ============================================================================
# Subnet
# ============================================================================
resource "google_compute_subnetwork" "erlmcp" {
  count                     = var.create_subnets ? length(var.subnets) : 0
  project                   = var.project_id
  name                      = var.subnets[count.index].name
  region                    = var.subnets[count.index].region
  network                   = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
  ip_cidr_range             = var.subnets[count.index].ip_cidr_range
  private_ip_google_access  = var.subnets[count.index].private_ip_google_access
  secondary_ip_ranges       = var.subnets[count.index].secondary_ip_ranges

  log_config {
    aggregation_interval = var.subnet_log_aggregation_interval
    flow_sampling        = var.subnet_log_flow_sampling
    metadata             = var.subnet_log_metadata
    metadata_fields {
      field = var.subnet_log_metadata_fields[0].field
    }
  }

  dynamic "purpose" {
    for_each = var.subnets[count.index].purpose != null ? [1] : []
    content {
      type = var.subnets[count.index].purpose
    }
  }

  role = var.subnets[count.index].role
}

# ============================================================================
# Router for Cloud NAT
# ============================================================================
resource "google_compute_router" "erlmcp" {
  count   = var.create_router ? length(var.routers) : 0
  project = var.project_id
  name    = var.routers[count.index].name
  region  = var.routers[count.index].region
  network = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name

  bgp {
    asn               = var.routers[count.index].asn
    advertise_mode    = var.routers[count.index].advertise_mode
    keepalive_interval = var.routers[count.index].keepalive_interval
  }
}

# ============================================================================
# Cloud NAT for private internet access
# ============================================================================
resource "google_compute_router_nat" "erlmcp" {
  count   = var.create_nat ? length(var.nats) : 0
  project = var.project_id
  name    = var.nats[count.index].name
  router  = google_compute_router.erlmcp[count.index].name
  region  = var.nats[count.index].region

  nat_ip_allocate_option = var.nats[count.index].nat_ip_allocate_option
  nat_ips                = var.nats[count.index].nat_ips
  source_subnetwork_ip_ranges_to_nat = var.nats[count.index].source_subnetwork_ip_ranges_to_nat

  subnetwork {
    name                     = var.nats[count.index].subnetwork_name
    source_ip_ranges_to_nat  = var.nats[count.index].source_ip_ranges_to_nat
    secondary_ip_range_names = var.nats[count.index].secondary_ip_range_names
  }

  log_config {
    enable = var.nats[count.index].enable_logging
    filter = var.nats[count.index].log_filter
  }

  min_ports_per_vm         = var.nats[count.index].min_ports_per_vm
  max_ports_per_vm         = var.nats[count.index].max_ports_per_vm
  udp_idle_timeout_sec     = var.nats[count.index].udp_idle_timeout
  icmp_idle_timeout_sec    = var.nats[count.index].icmp_idle_timeout
  tcp_established_idle_timeout_sec = var.nats[count.index].tcp_established_idle_timeout
  tcp_transitory_idle_timeout_sec  = var.nats[count.index].tcp_transitory_idle_timeout
}

# ============================================================================
# Firewall Rules
# ============================================================================
resource "google_compute_firewall" "erlmcp-allow-internal" {
  count   = var.create_firewall_rules ? 1 : 0
  project = var.project_id
  name    = "${var.network_name}-allow-internal"
  network = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
  description = "Allow internal traffic"

  allow {
    protocol = "tcp"
    ports    = ["0-65535"]
  }
  allow {
    protocol = "udp"
    ports    = ["0-65535"]
  }
  allow {
    protocol = "icmp"
  }

  source_ranges = var.internal_ranges
  priority      = var.firewall_priorities.internal
}

resource "google_compute_firewall" "erlmcp-allow-ssh" {
  count   = var.create_firewall_rules ? 1 : 0
  project = var.project_id
  name    = "${var.network_name}-allow-ssh"
  network = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
  description = "Allow SSH from allowed sources"

  allow {
    protocol = "tcp"
    ports    = ["22"]
  }

  source_ranges = var.ssh_source_ranges
  priority      = var.firewall_priorities.ssh
}

resource "google_compute_firewall" "erlmcp-allow-health-checks" {
  count   = var.create_firewall_rules ? 1 : 0
  project = var.project_id
  name    = "${var.network_name}-allow-health-checks"
  network = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
  description = "Allow Google health check probes"

  allow {
    protocol = "tcp"
    ports    = ["8080", "9090"]
  }

  source_ranges = var.health_check_ranges
  priority      = var.firewall_priorities.health_check
}

resource "google_compute_firewall" "erlmcp-allow-https" {
  count   = var.create_firewall_rules && var.allow_https_public ? 1 : 0
  project = var.project_id
  name    = "${var.network_name}-allow-https"
  network = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
  description = "Allow HTTPS from public"

  allow {
    protocol = "tcp"
    ports    = ["443"]
  }

  source_ranges = ["0.0.0.0/0"]
  priority      = var.firewall_priorities.https
}

resource "google_compute_firewall" "erlmcp-allow-master-access" {
  count   = var.create_firewall_rules && var.enable_private_endpoint ? 1 : 0
  project = var.project_id
  name    = "${var.network_name}-allow-master-access"
  network = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
  description = "Allow GKE master access to nodes"

  allow {
    protocol = "tcp"
    ports    = ["10250"]
  }

  source_ranges = var.master_access_ranges
  priority      = var.firewall_priorities.master_access
}

resource "google_compute_firewall" "erlmcp-deny-all" {
  count   = var.create_firewall_rules && var.enable_deny_all ? 1 : 0
  project = var.project_id
  name    = "${var.network_name}-deny-all"
  network = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
  description = "Deny all other traffic"

  deny {
    protocol = "tcp"
    ports    = ["0-65535"]
  }
  deny {
    protocol = "udp"
    ports    = ["0-65535"]
  }

  source_ranges = ["0.0.0.0/0"]
  priority      = var.firewall_priorities.deny_all
}

# ============================================================================
# Enable Compute API
# ============================================================================
resource "google_project_service" "compute" {
  project            = var.project_id
  service            = "compute.googleapis.com"
  disable_on_destroy = false
}
