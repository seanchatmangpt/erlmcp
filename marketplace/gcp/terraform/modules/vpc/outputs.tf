# ============================================================================
# VPC Module Outputs
# ============================================================================

# ============================================================================
# VPC Network Outputs
# ============================================================================
output "network_id" {
  description = "The ID of the VPC network"
  value       = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
}

output "network_name" {
  description = "The name of the VPC network"
  value       = var.create_network ? google_compute_network.erlmcp[0].name : var.network_name
}

output "network_self_link" {
  description = "The self link of the VPC network"
  value       = var.create_network ? google_compute_network.erlmcp[0].self_link : var.network_name
}

output "network_gateway_ipv4" {
  description = "The gateway address for default routing in the VPC"
  value       = var.create_network ? google_compute_network.erlmcp[0].gateway_ipv4 : null
}

output "routing_mode" {
  description = "The routing mode of the VPC (REGIONAL or GLOBAL)"
  value       = var.create_network ? google_compute_network.erlmcp[0].routing_mode : null
}

output "mtu" {
  description = "The MTU of the VPC network"
  value       = var.create_network ? google_compute_network.erlmcp[0].mtu : var.mtu
}

# ============================================================================
# Subnet Outputs
# ============================================================================
output "subnets" {
  description = "Map of subnet resources"
  value = var.create_subnets ? {
    for idx, subnet in google_compute_subnetwork.erlmcp :
    subnet.name => {
      id                       = subnet.id
      name                     = subnet.name
      region                   = subnet.region
      ip_cidr_range            = subnet.ip_cidr_range
      gateway_address          = subnet.gateway_address
      private_ip_google_access = subnet.private_ip_google_access
      secondary_ip_ranges      = subnet.secondary_ip_range
      self_link                = subnet.self_link
    }
  } : {}
}

output "subnet_ids" {
  description = "List of subnet IDs"
  value       = var.create_subnets ? google_compute_subnetwork.erlmcp[*].id : []
}

output "subnet_names" {
  description = "List of subnet names"
  value       = var.create_subnets ? google_compute_subnetwork.erlmcp[*].name : []
}

output "subnet_self_links" {
  description = "List of subnet self links"
  value       = var.create_subnets ? google_compute_subnetwork.erlmcp[*].self_link : []
}

output "subnet_ip_cidr_ranges" {
  description = "Map of subnet names to their IP CIDR ranges"
  value = var.create_subnets ? {
    for subnet in google_compute_subnetwork.erlmcp :
    subnet.name => subnet.ip_cidr_range
  } : {}
}

output "subnet_secondary_ranges" {
  description = "Map of subnet names to their secondary IP ranges"
  value = var.create_subnets ? {
    for subnet in google_compute_subnetwork.erlmcp :
    subnet.name => subnet.secondary_ip_range
  } : {}
}

# ============================================================================
# Router Outputs
# ============================================================================
output "router_ids" {
  description = "List of Cloud Router IDs"
  value       = var.create_router ? google_compute_router.erlmcp[*].id : []
}

output "router_names" {
  description = "List of Cloud Router names"
  value       = var.create_router ? google_compute_router.erlmcp[*].name : []
}

output "router_self_links" {
  description = "List of Cloud Router self links"
  value       = var.create_router ? google_compute_router.erlmcp[*].self_link : []
}

# ============================================================================
# NAT Gateway Outputs
# ============================================================================
output "nat_ids" {
  description = "List of Cloud NAT IDs"
  value       = var.create_nat ? google_compute_router_nat.erlmcp[*].id : []
}

output "nat_names" {
  description = "List of Cloud NAT names"
  value       = var.create_nat ? google_compute_router_nat.erlmcp[*].name : []
}

output "nat_ip_allocate_option" {
  description = "NAT IP allocation options"
  value       = var.create_nat ? google_compute_router_nat.erlmcp[*].nat_ip_allocate_option : []
}

# ============================================================================
# Firewall Outputs
# ============================================================================
output "firewall_rules" {
  description = "Map of firewall rule names to their IDs"
  value = var.create_firewall_rules ? {
    internal      = try(google_compute_firewall.erlmcp-allow-internal[0].id, null)
    ssh           = try(google_compute_firewall.erlmcp-allow-ssh[0].id, null)
    health_checks = try(google_compute_firewall.erlmcp-allow-health-checks[0].id, null)
    https         = try(google_compute_firewall.erlmcp-allow-https[0].id, null)
    master_access = try(google_compute_firewall.erlmcp-allow-master-access[0].id, null)
    deny_all      = try(google_compute_firewall.erlmcp-deny-all[0].id, null)
    erlang_dist   = try(google_compute_firewall.erlang_distribution[0].id, null)
    mcp_protocol  = try(google_compute_firewall.erlmcp_mcp_protocol[0].id, null)
  } : {}
}

output "firewall_rule_names" {
  description = "List of all firewall rule names"
  value = compact([
    try(google_compute_firewall.erlmcp-allow-internal[0].name, ""),
    try(google_compute_firewall.erlmcp-allow-ssh[0].name, ""),
    try(google_compute_firewall.erlmcp-allow-health-checks[0].name, ""),
    try(google_compute_firewall.erlmcp-allow-https[0].name, ""),
    try(google_compute_firewall.erlmcp-allow-master-access[0].name, ""),
    try(google_compute_firewall.erlmcp-deny-all[0].name, ""),
    try(google_compute_firewall.erlang_distribution[0].name, ""),
    try(google_compute_firewall.erlmcp_mcp_protocol[0].name, ""),
  ])
}

# ============================================================================
# VPC Peering Outputs
# ============================================================================
output "peering_connections" {
  description = "Map of VPC peering connection details"
  value = var.enable_peering ? {
    for idx, peering in google_compute_network_peering.peering :
    peering.name => {
      id                   = peering.id
      name                 = peering.name
      peer_network         = peering.peer_network
      state                = peering.state
      state_details        = peering.state_details
      export_custom_routes = peering.export_custom_routes
      import_custom_routes = peering.import_custom_routes
    }
  } : {}
}

output "peering_names" {
  description = "List of VPC peering connection names"
  value       = var.enable_peering ? google_compute_network_peering.peering[*].name : []
}

output "peering_states" {
  description = "Map of VPC peering connection states"
  value = var.enable_peering ? {
    for peering in google_compute_network_peering.peering :
    peering.name => peering.state
  } : {}
}

# ============================================================================
# Private Service Access Outputs
# ============================================================================
output "private_service_access_ip_range" {
  description = "The IP address range reserved for private service access"
  value       = var.enable_private_service_access ? google_compute_global_address.private_service_access[0].address : null
}

output "private_service_access_prefix_length" {
  description = "The prefix length of the private service access IP range"
  value       = var.enable_private_service_access ? google_compute_global_address.private_service_access[0].prefix_length : null
}

output "private_service_connection_status" {
  description = "Status of the private service connection"
  value       = var.enable_private_service_access ? google_service_networking_connection.private_service_access[0].service : null
}

# ============================================================================
# Private Service Connect Outputs
# ============================================================================
output "psc_endpoints" {
  description = "Map of Private Service Connect endpoint details"
  value = var.enable_psc_endpoints ? {
    for idx, endpoint in google_compute_forwarding_rule.psc_endpoint :
    endpoint.name => {
      id                    = endpoint.id
      name                  = endpoint.name
      ip_address            = endpoint.ip_address
      target                = endpoint.target
      region                = endpoint.region
      psc_connection_id     = endpoint.psc_connection_id
      psc_connection_status = endpoint.psc_connection_status
    }
  } : {}
}

output "psc_endpoint_ips" {
  description = "Map of PSC endpoint names to IP addresses"
  value = var.enable_psc_endpoints ? {
    for endpoint in google_compute_forwarding_rule.psc_endpoint :
    endpoint.name => endpoint.ip_address
  } : {}
}

# ============================================================================
# DNS Policy Outputs
# ============================================================================
output "dns_policy_id" {
  description = "The ID of the DNS policy"
  value       = var.enable_dns_policies ? google_dns_policy.private_google_access[0].id : null
}

output "dns_policy_name" {
  description = "The name of the DNS policy"
  value       = var.enable_dns_policies ? google_dns_policy.private_google_access[0].name : null
}

output "dns_policy_enable_logging" {
  description = "Whether DNS logging is enabled"
  value       = var.enable_dns_policies ? google_dns_policy.private_google_access[0].enable_logging : null
}

# ============================================================================
# Cloud Armor Outputs
# ============================================================================
output "cloud_armor_policy_id" {
  description = "The ID of the Cloud Armor security policy"
  value       = var.enable_cloud_armor ? google_compute_security_policy.cloud_armor[0].id : null
}

output "cloud_armor_policy_name" {
  description = "The name of the Cloud Armor security policy"
  value       = var.enable_cloud_armor ? google_compute_security_policy.cloud_armor[0].name : null
}

output "cloud_armor_policy_self_link" {
  description = "The self link of the Cloud Armor security policy"
  value       = var.enable_cloud_armor ? google_compute_security_policy.cloud_armor[0].self_link : null
}

output "cloud_armor_fingerprint" {
  description = "The fingerprint of the Cloud Armor security policy"
  value       = var.enable_cloud_armor ? google_compute_security_policy.cloud_armor[0].fingerprint : null
}

# ============================================================================
# Shared VPC Outputs
# ============================================================================
output "shared_vpc_host_project" {
  description = "The host project ID if shared VPC is enabled"
  value       = var.enable_shared_vpc_host ? google_compute_shared_vpc_host_project.host[0].project : null
}

output "shared_vpc_service_projects" {
  description = "List of service projects attached to shared VPC"
  value       = var.enable_shared_vpc_host ? var.shared_vpc_service_projects : []
}

# ============================================================================
# Flow Logs Outputs
# ============================================================================
output "flow_log_sink_name" {
  description = "The name of the VPC flow log sink"
  value       = var.enable_flow_log_export ? google_logging_project_sink.vpc_flow_logs[0].name : null
}

output "flow_log_sink_writer_identity" {
  description = "The service account email of the flow log sink writer"
  value       = var.enable_flow_log_export ? google_logging_project_sink.vpc_flow_logs[0].writer_identity : null
}

# ============================================================================
# Aggregate Summary Output
# ============================================================================
output "vpc_summary" {
  description = "Summary of VPC configuration"
  value = {
    network_name               = var.create_network ? google_compute_network.erlmcp[0].name : var.network_name
    network_id                 = var.create_network ? google_compute_network.erlmcp[0].id : var.network_name
    project_id                 = var.project_id
    routing_mode               = var.routing_mode
    mtu                        = var.mtu
    subnets_count              = var.create_subnets ? length(google_compute_subnetwork.erlmcp) : 0
    routers_count              = var.create_router ? length(google_compute_router.erlmcp) : 0
    nat_gateways_count         = var.create_nat ? length(google_compute_router_nat.erlmcp) : 0
    firewall_rules_count       = var.create_firewall_rules ? length(compact([
      try(google_compute_firewall.erlmcp-allow-internal[0].name, ""),
      try(google_compute_firewall.erlmcp-allow-ssh[0].name, ""),
      try(google_compute_firewall.erlmcp-allow-health-checks[0].name, ""),
      try(google_compute_firewall.erlmcp-allow-https[0].name, ""),
      try(google_compute_firewall.erlmcp-allow-master-access[0].name, ""),
      try(google_compute_firewall.erlmcp-deny-all[0].name, ""),
      try(google_compute_firewall.erlang_distribution[0].name, ""),
      try(google_compute_firewall.erlmcp_mcp_protocol[0].name, ""),
    ])) : 0
    peering_connections_count  = var.enable_peering ? length(google_compute_network_peering.peering) : 0
    psc_endpoints_count        = var.enable_psc_endpoints ? length(google_compute_forwarding_rule.psc_endpoint) : 0
    private_service_access     = var.enable_private_service_access
    dns_policies_enabled       = var.enable_dns_policies
    cloud_armor_enabled        = var.enable_cloud_armor
    shared_vpc_host            = var.enable_shared_vpc_host
    flow_log_export_enabled    = var.enable_flow_log_export
  }
}
