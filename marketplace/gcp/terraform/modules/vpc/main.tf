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

  dynamic "secondary_ip_range" {
    for_each = var.subnets[count.index].secondary_ip_ranges
    content {
      range_name    = secondary_ip_range.value.range_name
      ip_cidr_range = secondary_ip_range.value.ip_cidr_range
    }
  }

  log_config {
    aggregation_interval = var.subnet_log_aggregation_interval
    flow_sampling        = var.subnet_log_flow_sampling
    metadata             = var.subnet_log_metadata
  }
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
# Enable Required APIs
# ============================================================================
resource "google_project_service" "compute" {
  project            = var.project_id
  service            = "compute.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "servicenetworking" {
  count              = var.enable_private_service_access ? 1 : 0
  project            = var.project_id
  service            = "servicenetworking.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "dns" {
  count              = var.enable_dns_policies ? 1 : 0
  project            = var.project_id
  service            = "dns.googleapis.com"
  disable_on_destroy = false
}

# ============================================================================
# VPC Peering
# ============================================================================
resource "google_compute_network_peering" "peering" {
  count                = var.enable_peering ? length(var.peering_config) : 0
  name                 = var.peering_config[count.index].name
  network              = var.create_network ? google_compute_network.erlmcp[0].self_link : var.network_name
  peer_network         = var.peering_config[count.index].peer_network
  export_custom_routes = var.peering_config[count.index].export_custom_routes
  import_custom_routes = var.peering_config[count.index].import_custom_routes

  export_subnet_routes_with_public_ip = var.peering_config[count.index].export_subnet_routes_with_public_ip
  import_subnet_routes_with_public_ip = var.peering_config[count.index].import_subnet_routes_with_public_ip
}

# ============================================================================
# Private Service Access (for Cloud SQL, Memorystore, etc.)
# ============================================================================
resource "google_compute_global_address" "private_service_access" {
  count         = var.enable_private_service_access ? 1 : 0
  project       = var.project_id
  name          = "${var.network_name}-private-service-access"
  purpose       = "VPC_PEERING"
  address_type  = "INTERNAL"
  prefix_length = var.private_service_cidr_prefix
  network       = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name

  depends_on = [google_project_service.servicenetworking]
}

resource "google_service_networking_connection" "private_service_access" {
  count                   = var.enable_private_service_access ? 1 : 0
  network                 = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
  service                 = "servicenetworking.googleapis.com"
  reserved_peering_ranges = [google_compute_global_address.private_service_access[0].name]

  depends_on = [google_project_service.servicenetworking]
}

# ============================================================================
# Private Service Connect Endpoints
# ============================================================================
resource "google_compute_global_address" "psc_endpoint" {
  count        = var.enable_psc_endpoints ? length(var.psc_endpoints) : 0
  project      = var.project_id
  name         = var.psc_endpoints[count.index].name
  address_type = "INTERNAL"
  purpose      = "PRIVATE_SERVICE_CONNECT"
  network      = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
  address      = var.psc_endpoints[count.index].ip_address
}

resource "google_compute_forwarding_rule" "psc_endpoint" {
  count                 = var.enable_psc_endpoints ? length(var.psc_endpoints) : 0
  project               = var.project_id
  name                  = var.psc_endpoints[count.index].name
  region                = var.psc_endpoints[count.index].region
  network               = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
  ip_address            = google_compute_global_address.psc_endpoint[count.index].id
  target                = var.psc_endpoints[count.index].target_service
  load_balancing_scheme = ""
}

# ============================================================================
# DNS Policy for Private Google Access
# ============================================================================
resource "google_dns_policy" "private_google_access" {
  count                     = var.enable_dns_policies ? 1 : 0
  project                   = var.project_id
  name                      = "${var.network_name}-dns-policy"
  enable_inbound_forwarding = var.dns_enable_inbound_forwarding
  enable_logging            = var.dns_enable_logging

  networks {
    network_url = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
  }

  dynamic "alternative_name_server_config" {
    for_each = length(var.dns_alternative_name_servers) > 0 ? [1] : []
    content {
      dynamic "target_name_servers" {
        for_each = var.dns_alternative_name_servers
        content {
          ipv4_address    = target_name_servers.value.ipv4_address
          forwarding_path = target_name_servers.value.forwarding_path
        }
      }
    }
  }

  depends_on = [google_project_service.dns]
}

# ============================================================================
# Cloud Armor Security Policy
# ============================================================================
resource "google_compute_security_policy" "cloud_armor" {
  count   = var.enable_cloud_armor ? 1 : 0
  project = var.project_id
  name    = "${var.network_name}-cloud-armor-policy"

  # Default rule
  rule {
    action   = "allow"
    priority = "2147483647"
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = ["*"]
      }
    }
    description = "Default allow rule"
  }

  # Rate limiting rule
  dynamic "rule" {
    for_each = var.cloud_armor_rate_limit_threshold > 0 ? [1] : []
    content {
      action   = "rate_based_ban"
      priority = 1000
      match {
        versioned_expr = "SRC_IPS_V1"
        config {
          src_ip_ranges = ["*"]
        }
      }
      rate_limit_options {
        conform_action = "allow"
        exceed_action  = "deny(429)"
        enforce_on_key = "IP"
        rate_limit_threshold {
          count        = var.cloud_armor_rate_limit_threshold
          interval_sec = var.cloud_armor_rate_limit_interval
        }
        ban_duration_sec = var.cloud_armor_ban_duration
      }
      description = "Rate limit rule"
    }
  }

  # Block specific countries (if configured)
  dynamic "rule" {
    for_each = length(var.cloud_armor_blocked_countries) > 0 ? [1] : []
    content {
      action   = "deny(403)"
      priority = 2000
      match {
        expr {
          expression = "origin.region_code in [${join(",", formatlist("'%s'", var.cloud_armor_blocked_countries))}]"
        }
      }
      description = "Block traffic from specific countries"
    }
  }

  # OWASP ModSecurity Core Rule Set
  dynamic "rule" {
    for_each = var.cloud_armor_enable_owasp_rules ? [1] : []
    content {
      action   = "deny(403)"
      priority = 3000
      match {
        expr {
          expression = "evaluatePreconfiguredExpr('xss-stable')"
        }
      }
      description = "XSS protection"
    }
  }

  adaptive_protection_config {
    layer_7_ddos_defense_config {
      enable = var.cloud_armor_enable_adaptive_protection
    }
  }
}

# ============================================================================
# Shared VPC Configuration (Host Project)
# ============================================================================
resource "google_compute_shared_vpc_host_project" "host" {
  count   = var.enable_shared_vpc_host ? 1 : 0
  project = var.project_id
}

resource "google_compute_shared_vpc_service_project" "service" {
  count           = var.enable_shared_vpc_host ? length(var.shared_vpc_service_projects) : 0
  host_project    = var.project_id
  service_project = var.shared_vpc_service_projects[count.index]

  depends_on = [google_compute_shared_vpc_host_project.host]
}

# ============================================================================
# Erlang-Specific Firewall Rules
# ============================================================================
resource "google_compute_firewall" "erlang_distribution" {
  count   = var.create_firewall_rules && var.enable_erlang_distribution ? 1 : 0
  project = var.project_id
  name    = "${var.network_name}-allow-erlang-distribution"
  network = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name

  description = "Allow Erlang distribution protocol (EPMD + distributed Erlang)"

  allow {
    protocol = "tcp"
    ports    = ["4369"]  # EPMD
  }

  allow {
    protocol = "tcp"
    ports    = var.erlang_distribution_port_range
  }

  source_tags = var.erlang_node_tags
  target_tags = var.erlang_node_tags
  priority    = var.firewall_priorities.internal

  log_config {
    metadata = "INCLUDE_ALL_METADATA"
  }
}

# ============================================================================
# Enhanced Firewall Rules with Logging
# ============================================================================
resource "google_compute_firewall" "erlmcp_mcp_protocol" {
  count   = var.create_firewall_rules ? 1 : 0
  project = var.project_id
  name    = "${var.network_name}-allow-mcp-protocol"
  network = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
  description = "Allow MCP protocol (stdio, TCP, HTTP, WebSocket, SSE)"

  allow {
    protocol = "tcp"
    ports    = ["8080", "8443", "9090"]  # MCP API, MCP WebSocket, MCP metrics
  }

  source_ranges = var.internal_ranges
  priority      = var.firewall_priorities.internal

  log_config {
    metadata = "INCLUDE_ALL_METADATA"
  }
}

# Update existing firewall rules with logging
resource "google_compute_firewall" "erlmcp_deny_all_logging" {
  count   = var.create_firewall_rules && var.enable_deny_all ? 1 : 0
  project = var.project_id
  name    = "${var.network_name}-deny-all-logged"
  network = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
  description = "Deny all other traffic with logging"

  deny {
    protocol = "all"
  }

  source_ranges = ["0.0.0.0/0"]
  priority      = var.firewall_priorities.deny_all

  log_config {
    metadata = "INCLUDE_ALL_METADATA"
  }
}

# ============================================================================
# VPC Flow Logs Export to Cloud Logging
# ============================================================================
resource "google_logging_project_sink" "vpc_flow_logs" {
  count       = var.enable_flow_log_export ? 1 : 0
  name        = "${var.network_name}-vpc-flow-logs-sink"
  destination = "storage.googleapis.com/${var.flow_log_bucket_name}"
  filter      = "resource.type=\"gce_subnetwork\" AND log_name=~\"logs/compute.googleapis.com%2Fvpc_flows\""

  unique_writer_identity = true
}
