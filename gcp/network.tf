# ==============================================================================
# ERLMCP VPC and Networking
# ==============================================================================
# Private networking for erlmcp infrastructure.
# Includes: VPC, subnets, firewall rules, VPC connector for Cloud Run.
#
# Architecture:
# - Private VPC with custom subnets
# - VPC connector for serverless access to private resources
# - Private Google Access for GCP APIs
# - Firewall rules following least-privilege
# ==============================================================================

# VPC Network
resource "google_compute_network" "erlmcp" {
  count = var.create_vpc ? 1 : 0

  name                            = "${var.app_name}-vpc"
  auto_create_subnetworks         = false
  routing_mode                    = "REGIONAL"
  delete_default_routes_on_create = false

  depends_on = [google_project_service.required_apis]
}

# Primary Subnet (for Cloud Run VPC connector)
resource "google_compute_subnetwork" "erlmcp_primary" {
  count = var.create_vpc ? 1 : 0

  name                     = "${var.app_name}-subnet-primary"
  ip_cidr_range            = var.vpc_primary_cidr
  region                   = var.region
  network                  = google_compute_network.erlmcp[0].id
  private_ip_google_access = true

  log_config {
    aggregation_interval = "INTERVAL_5_SEC"
    flow_sampling        = 0.5
    metadata             = "INCLUDE_ALL_METADATA"
  }
}

# Serverless VPC Connector Subnet
resource "google_compute_subnetwork" "erlmcp_serverless" {
  count = var.create_vpc ? 1 : 0

  name          = "${var.app_name}-subnet-serverless"
  ip_cidr_range = var.vpc_serverless_cidr
  region        = var.region
  network       = google_compute_network.erlmcp[0].id
  purpose       = "PRIVATE"
}

# VPC Access Connector (for Cloud Run to access private resources)
resource "google_vpc_access_connector" "erlmcp" {
  count = var.vpc_connector_enabled ? 1 : 0

  name          = "${var.app_name}-connector"
  region        = var.region
  ip_cidr_range = var.vpc_connector_cidr
  network       = var.create_vpc ? google_compute_network.erlmcp[0].name : var.existing_vpc_name

  min_instances = 2
  max_instances = var.vpc_connector_max_instances

  machine_type = var.vpc_connector_machine_type

  depends_on = [google_project_service.required_apis]
}

# Cloud Router (for NAT)
resource "google_compute_router" "erlmcp" {
  count = var.create_vpc && var.enable_nat ? 1 : 0

  name    = "${var.app_name}-router"
  region  = var.region
  network = google_compute_network.erlmcp[0].id
}

# Cloud NAT (for outbound internet access from private resources)
resource "google_compute_router_nat" "erlmcp" {
  count = var.create_vpc && var.enable_nat ? 1 : 0

  name                               = "${var.app_name}-nat"
  router                             = google_compute_router.erlmcp[0].name
  region                             = var.region
  nat_ip_allocate_option             = "AUTO_ONLY"
  source_subnetwork_ip_ranges_to_nat = "ALL_SUBNETWORKS_ALL_IP_RANGES"

  log_config {
    enable = true
    filter = "ERRORS_ONLY"
  }
}

# ==============================================================================
# Firewall Rules
# ==============================================================================

# Allow internal traffic within VPC
resource "google_compute_firewall" "erlmcp_internal" {
  count = var.create_vpc ? 1 : 0

  name    = "${var.app_name}-allow-internal"
  network = google_compute_network.erlmcp[0].name

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

  source_ranges = [var.vpc_primary_cidr, var.vpc_serverless_cidr]

  priority = 1000
}

# Allow health checks from GCP load balancers
resource "google_compute_firewall" "erlmcp_health_checks" {
  count = var.create_vpc ? 1 : 0

  name    = "${var.app_name}-allow-health-checks"
  network = google_compute_network.erlmcp[0].name

  allow {
    protocol = "tcp"
    ports    = ["8080", "9090"]
  }

  # GCP health check IP ranges
  source_ranges = ["35.191.0.0/16", "130.211.0.0/22"]

  target_tags = ["erlmcp"]

  priority = 1000
}

# Allow SSH from IAP (Identity-Aware Proxy) for debugging
resource "google_compute_firewall" "erlmcp_iap_ssh" {
  count = var.create_vpc ? 1 : 0

  name    = "${var.app_name}-allow-iap-ssh"
  network = google_compute_network.erlmcp[0].name

  allow {
    protocol = "tcp"
    ports    = ["22"]
  }

  # IAP IP range
  source_ranges = ["35.235.240.0/20"]

  target_tags = ["erlmcp-ssh"]

  priority = 1000
}

# Deny all other ingress (explicit deny-all)
resource "google_compute_firewall" "erlmcp_deny_all_ingress" {
  count = var.create_vpc && var.enable_strict_firewall ? 1 : 0

  name    = "${var.app_name}-deny-all-ingress"
  network = google_compute_network.erlmcp[0].name

  deny {
    protocol = "all"
  }

  source_ranges = ["0.0.0.0/0"]

  priority = 65534
}

# ==============================================================================
# Private Service Connection (for Cloud SQL)
# ==============================================================================

resource "google_compute_global_address" "private_ip_range" {
  count = var.create_vpc && var.enable_private_services ? 1 : 0

  name          = "${var.app_name}-private-ip-range"
  purpose       = "VPC_PEERING"
  address_type  = "INTERNAL"
  prefix_length = 16
  network       = google_compute_network.erlmcp[0].id
}

resource "google_service_networking_connection" "private_vpc_connection" {
  count = var.create_vpc && var.enable_private_services ? 1 : 0

  network                 = google_compute_network.erlmcp[0].id
  service                 = "servicenetworking.googleapis.com"
  reserved_peering_ranges = [google_compute_global_address.private_ip_range[0].name]

  depends_on = [google_project_service.required_apis]
}

# ==============================================================================
# DNS (Cloud DNS for private zones)
# ==============================================================================

resource "google_dns_managed_zone" "erlmcp_private" {
  count = var.create_vpc && var.create_private_dns ? 1 : 0

  name        = "${var.app_name}-private-zone"
  dns_name    = "${var.app_name}.internal."
  description = "Private DNS zone for ${var.app_name}"
  visibility  = "private"

  private_visibility_config {
    networks {
      network_url = google_compute_network.erlmcp[0].id
    }
  }
}

# DNS record for Cloud SQL
resource "google_dns_record_set" "erlmcp_db" {
  count = var.create_vpc && var.create_private_dns && var.enable_cloud_sql ? 1 : 0

  name         = "db.${google_dns_managed_zone.erlmcp_private[0].dns_name}"
  type         = "A"
  ttl          = 300
  managed_zone = google_dns_managed_zone.erlmcp_private[0].name
  rrdatas      = [google_sql_database_instance.erlmcp[0].private_ip_address]
}
