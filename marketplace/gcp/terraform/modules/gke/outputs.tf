# ============================================================================
# GKE Module Outputs
# ============================================================================

output "cluster_name" {
  description = "GKE cluster name"
  value       = google_container_cluster.erlmcp.name
}

output "cluster_id" {
  description = "GKE cluster ID"
  value       = google_container_cluster.erlmcp.id
}

output "cluster_endpoint" {
  description = "GKE cluster endpoint"
  value       = google_container_cluster.erlmcp.endpoint
  sensitive   = true
}

output "cluster_ca_certificate" {
  description = "GKE cluster CA certificate"
  value       = google_container_cluster.erlmcp.master_auth[0].cluster_ca_certificate
  sensitive   = true
}

output "cluster_region" {
  description = "GKE cluster region"
  value       = google_container_cluster.erlmcp.location
}

output "cluster_version" {
  description = "GKE Kubernetes version"
  value       = google_container_cluster.erlmcp.master_auth[0].cluster_version
}

output "node_pool_names" {
  description = "List of node pool names"
  value = compact([
    var.create_custom_node_pools ? google_container_node_pool.primary[0].name : null,
    var.create_spot_node_pool ? google_container_node_pool.spot[0].name : null
  ])
}

output "primary_node_pool_id" {
  description = "Primary node pool ID"
  value       = var.create_custom_node_pools ? google_container_node_pool.primary[0].id : null
}

output "spot_node_pool_id" {
  description = "Spot node pool ID"
  value       = var.create_spot_node_pool ? google_container_node_pool.spot[0].id : null
}

output "workload_pool" {
  description = "Workload Identity pool"
  value       = "${var.project_id}.svc.id.goog"
}

output "network" {
  description = "VPC network name"
  value       = google_container_cluster.erlmcp.network
}

output "subnet" {
  description = "Subnet name"
  value       = google_container_cluster.erlmcp.subnetwork
}

output "private_endpoint" {
  description = "Private cluster endpoint"
  value       = var.enable_private_endpoint ? "https://${google_container_cluster.erlmcp.private_cluster_config[0].private_endpoint}" : null
  sensitive   = true
}

output "access_token" {
  description = "GCP access token for kubectl"
  value       = data.google_client_config.default.access_token
  sensitive   = true
}

# Command output for kubectl configuration
output "kubectl_config_command" {
  description = "Command to configure kubectl access"
  value = format(
    "gcloud container clusters get-credentials %s --region %s --project %s",
    google_container_cluster.erlmcp.name,
    google_container_cluster.erlmcp.location,
    var.project_id
  )
}

# ============================================================================
# Service URL Output (for Marketplace compatibility)
# ============================================================================
output "service_url" {
  description = "Service URL for accessing erlmcp"
  value = var.enable_private_endpoint ? null : format(
    "https://%s.%s.%s",
    google_service.erlmcp.load_balancer_ip,
    var.region,
    "go.cloud.google.com"
  )
}

output "load_balancer_ip" {
  description = "Load balancer IP address"
  value = google_service.erlmcp.load_balancer_ip
}
