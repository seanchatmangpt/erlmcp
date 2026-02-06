# ============================================================================
# GKE Module Outputs - Updated for Modern GKE
# ============================================================================

# ============================================================================
# Cluster Information
# ============================================================================
output "cluster_name" {
  description = "GKE cluster name"
  value       = google_container_cluster.erlmcp.name
}

output "cluster_id" {
  description = "GKE cluster ID (fully qualified)"
  value       = google_container_cluster.erlmcp.id
}

output "cluster_self_link" {
  description = "GKE cluster self-link"
  value       = google_container_cluster.erlmcp.self_link
}

output "cluster_endpoint" {
  description = "GKE cluster endpoint (public or private)"
  value       = google_container_cluster.erlmcp.endpoint
  sensitive   = true
}

output "cluster_ca_certificate" {
  description = "GKE cluster CA certificate (base64 encoded)"
  value       = google_container_cluster.erlmcp.master_auth[0].cluster_ca_certificate
  sensitive   = true
}

output "cluster_region" {
  description = "GKE cluster region"
  value       = google_container_cluster.erlmcp.location
}

output "cluster_location" {
  description = "GKE cluster location (region or zone)"
  value       = google_container_cluster.erlmcp.location
}

output "cluster_zones" {
  description = "GKE cluster zones"
  value       = google_container_cluster.erlmcp.node_locations
}

# ============================================================================
# Kubernetes Version Information
# ============================================================================
output "cluster_version" {
  description = "Current GKE Kubernetes version"
  value       = google_container_cluster.erlmcp.master_version
}

output "release_channel" {
  description = "GKE release channel"
  value       = var.release_channel
}

output "available_versions" {
  description = "Available GKE versions in region"
  value       = data.google_container_engine_versions.default.valid_master_versions
}

# ============================================================================
# Cluster Mode
# ============================================================================
output "is_autopilot" {
  description = "Whether this is an Autopilot cluster"
  value       = var.enable_autopilot
}

output "cluster_type" {
  description = "Cluster type (Autopilot or Standard)"
  value       = var.enable_autopilot ? "autopilot" : "standard"
}

# ============================================================================
# Node Pool Information (Standard Clusters Only)
# ============================================================================
output "node_pool_names" {
  description = "List of node pool names"
  value = compact([
    !var.enable_autopilot && var.create_custom_node_pools ? google_container_node_pool.primary[0].name : null,
    !var.enable_autopilot && var.create_spot_node_pool ? google_container_node_pool.spot[0].name : null
  ])
}

output "primary_node_pool_id" {
  description = "Primary node pool ID"
  value       = !var.enable_autopilot && var.create_custom_node_pools ? google_container_node_pool.primary[0].id : null
}

output "spot_node_pool_id" {
  description = "Spot node pool ID"
  value       = !var.enable_autopilot && var.create_spot_node_pool ? google_container_node_pool.spot[0].id : null
}

output "primary_node_pool_version" {
  description = "Primary node pool Kubernetes version"
  value       = !var.enable_autopilot && var.create_custom_node_pools ? google_container_node_pool.primary[0].version : null
}

# ============================================================================
# Workload Identity
# ============================================================================
output "workload_identity_pool" {
  description = "Workload Identity pool for GKE"
  value       = "${var.project_id}.svc.id.goog"
}

output "workload_identity_enabled" {
  description = "Whether Workload Identity is enabled"
  value       = true
}

# ============================================================================
# Network Configuration
# ============================================================================
output "network" {
  description = "VPC network name"
  value       = google_container_cluster.erlmcp.network
}

output "subnet" {
  description = "Subnet name"
  value       = google_container_cluster.erlmcp.subnetwork
}

output "private_endpoint_enabled" {
  description = "Whether private endpoint is enabled"
  value       = var.enable_private_endpoint
}

output "private_nodes_enabled" {
  description = "Whether private nodes are enabled"
  value       = var.enable_private_nodes
}

output "private_endpoint" {
  description = "Private cluster endpoint (if enabled)"
  value = try(
    google_container_cluster.erlmcp.private_cluster_config[0].private_endpoint,
    null
  )
  sensitive = true
}

output "public_endpoint" {
  description = "Public cluster endpoint (if enabled)"
  value = try(
    google_container_cluster.erlmcp.endpoint,
    null
  )
  sensitive = true
}

output "master_ipv4_cidr_block" {
  description = "Control plane CIDR block"
  value       = var.master_ipv4_cidr_block
}

# ============================================================================
# Security Configuration
# ============================================================================
output "security_posture_enabled" {
  description = "Whether Security Posture is enabled"
  value       = var.enable_security_posture
}

output "security_posture_mode" {
  description = "Security Posture mode"
  value       = var.enable_security_posture ? var.security_posture_mode : null
}

output "binary_authorization_enabled" {
  description = "Whether Binary Authorization is enabled"
  value       = var.binauthz_evaluation_mode != "DISABLED"
}

output "shielded_nodes_enabled" {
  description = "Whether Shielded GKE nodes are enabled"
  value       = var.enable_secure_boot || var.enable_integrity_monitoring
}

output "confidential_nodes_enabled" {
  description = "Whether Confidential GKE nodes are enabled"
  value       = var.enable_confidential_nodes
}

# ============================================================================
# Monitoring and Logging
# ============================================================================
output "managed_prometheus_enabled" {
  description = "Whether Google Cloud Managed Service for Prometheus is enabled"
  value       = var.enable_managed_prometheus
}

output "logging_config" {
  description = "Logging configuration"
  value       = var.logging_components
}

output "monitoring_config" {
  description = "Monitoring configuration"
  value       = var.monitoring_components
}

# ============================================================================
# Networking Features
# ============================================================================
output "datapath_provider" {
  description = "Datapath provider (Dataplane V2 or Legacy)"
  value       = var.enable_autopilot ? "ADVANCED_DATAPATH" : var.datapath_provider
}

output "gateway_api_enabled" {
  description = "Whether Gateway API is enabled"
  value       = var.enable_gateway_api
}

output "network_policy_enabled" {
  description = "Whether Network Policy is enabled"
  value       = var.enable_network_policy
}

# ============================================================================
# Fleet Management
# ============================================================================
output "fleet_membership" {
  description = "Fleet membership information"
  value = var.enable_fleet ? {
    enabled = true
    project = var.fleet_project != "" ? var.fleet_project : var.project_id
  } : null
}

# ============================================================================
# kubectl Configuration
# ============================================================================
output "kubectl_config_command" {
  description = "Command to configure kubectl access"
  value = format(
    "gcloud container clusters get-credentials %s --region %s --project %s",
    google_container_cluster.erlmcp.name,
    google_container_cluster.erlmcp.location,
    var.project_id
  )
}

output "kubeconfig_raw" {
  description = "Raw kubeconfig data (use with caution)"
  value = {
    host                   = google_container_cluster.erlmcp.endpoint
    cluster_ca_certificate = base64decode(google_container_cluster.erlmcp.master_auth[0].cluster_ca_certificate)
    token                  = data.google_client_config.default.access_token
  }
  sensitive = true
}

# ============================================================================
# Access Information
# ============================================================================
output "access_token" {
  description = "GCP access token for kubectl (short-lived)"
  value       = data.google_client_config.default.access_token
  sensitive   = true
}

output "project_id" {
  description = "GCP project ID"
  value       = var.project_id
}

output "project_number" {
  description = "GCP project number"
  value       = data.google_project.project.number
}

# ============================================================================
# Cost Management
# ============================================================================
output "cost_allocation_enabled" {
  description = "Whether GKE cost allocation is enabled"
  value       = var.enable_cost_allocation
}

output "cost_center" {
  description = "Cost center label"
  value       = var.cost_center
}

# ============================================================================
# Cluster Features Summary
# ============================================================================
output "cluster_features" {
  description = "Summary of enabled cluster features"
  value = {
    autopilot                    = var.enable_autopilot
    dataplane_v2                 = var.datapath_provider == "ADVANCED_DATAPATH"
    workload_identity            = true
    security_posture             = var.enable_security_posture
    binary_authorization         = var.binauthz_evaluation_mode != "DISABLED"
    shielded_nodes               = var.enable_secure_boot || var.enable_integrity_monitoring
    confidential_nodes           = var.enable_confidential_nodes
    managed_prometheus           = var.enable_managed_prometheus
    gateway_api                  = var.enable_gateway_api
    gcs_fuse_csi                 = var.enable_gcs_fuse_csi
    filestore_csi                = var.enable_filestore_csi
    image_streaming              = var.enable_image_streaming
    backup_agent                 = var.enable_backup_agent
    config_connector             = var.enable_config_connector
    fleet_membership             = var.enable_fleet
    deletion_protection          = var.enable_deletion_protection
    cost_allocation              = var.enable_cost_allocation
    node_auto_provisioning       = var.enable_node_autoprovisioning
    cluster_autoscaling          = var.enable_cluster_autoscaling
  }
}

# ============================================================================
# Connection Examples
# ============================================================================
output "connection_examples" {
  description = "Example commands for cluster access"
  value = {
    get_credentials = format("gcloud container clusters get-credentials %s --region %s --project %s",
      google_container_cluster.erlmcp.name,
      google_container_cluster.erlmcp.location,
      var.project_id
    )
    kubectl_get_nodes = "kubectl get nodes"
    kubectl_get_pods  = "kubectl get pods --all-namespaces"
    helm_list         = "helm list --all-namespaces"
  }
}

# ============================================================================
# Monitoring Dashboard Links
# ============================================================================
output "console_links" {
  description = "Google Cloud Console links for cluster management"
  value = {
    cluster = format("https://console.cloud.google.com/kubernetes/clusters/details/%s/%s?project=%s",
      google_container_cluster.erlmcp.location,
      google_container_cluster.erlmcp.name,
      var.project_id
    )
    workloads = format("https://console.cloud.google.com/kubernetes/workload?project=%s&pageState=%%7B%%22workload%%22:%%7B%%22tableId%%22:%%22workload%%22,%%22currentPage%%22:1%%7D%%7D",
      var.project_id
    )
    monitoring = format("https://console.cloud.google.com/monitoring/dashboards?project=%s",
      var.project_id
    )
    logs = format("https://console.cloud.google.com/logs/query?project=%s&query=resource.type%%3D%%22k8s_cluster%%22%%0Aresource.labels.cluster_name%%3D%%22%s%%22",
      var.project_id,
      google_container_cluster.erlmcp.name
    )
  }
}
