# ============================================================================
# GKE Module for erlmcp Deployment
# Google Kubernetes Engine with regional configuration for 99.95% SLA
# ============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.0.0"
    }
    helm = {
      source  = "hashicorp/helm"
      version = ">= 2.12.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = ">= 2.11.0"
    }
  }
}

# ============================================================================
# GKE Cluster
# ============================================================================
resource "google_container_cluster" "erlmcp" {
  name     = var.cluster_name
  location = var.region
  project  = var.project_id

  # Regional cluster for 99.95% SLA
  regional {
    # Use region for regional cluster (or remove for zonal)
  }

  # Network configuration
  network    = var.network
  subnetwork = var.subnet

  # Private cluster configuration
  private_cluster_config {
    enable_private_endpoint = var.enable_private_endpoint
    enable_private_nodes    = var.enable_private_nodes
    master_ipv4_cidr_block  = var.master_ipv4_cidr_block
    master_global_access_config {
      enabled = var.master_global_access
    }
  }

  # Release channel
  release_channel {
    channel = var.release_channel
  }

  # Cluster version
  min_master_version = var.kubernetes_version

  # Workload Identity for Secret Manager access
  workload_identity_config {
    workload_pool = "${var.project_id}.svc.id.goog"
  }

  # Cluster logging and monitoring
  logging_config {
    enable_components = var.logging_components
  }

  monitoring_config {
    enable_components = var.monitoring_components
    managed_prometheus {
      enabled = var.enable_managed_prometheus
    }
  }

  # Maintenance window
  maintenance_policy {
    daily_maintenance_window {
      start_time = var.maintenance_window_start
    }
  }

  # Resource labels
  resource_labels = merge(
    var.resource_labels,
    {
      managed-by = "terraform"
      app        = "erlmcp"
    }
  )

  # Add-on configurations
  addons_config {
    http_load_balancing {
      enabled = var.http_load_balancing
    }
    network_policy_config {
      disabled = !var.enable_network_policy
    }
    gce_persistent_disk_csi_driver_config {
      enabled = true
    }
  }

  # Network policy
  network_policy {
    enabled  = var.enable_network_policy
    provider = var.network_policy_provider
  }

  # Pod security
  pod_security_policy_config {
    enabled = var.enable_pod_security_policy
  }

  # Binary authorization
  binary_authorization {
    evaluation_mode = var.binauthz_evaluation_mode
  }

  # Autoscaling profile
  autoscaling_profile = var.cluster_autoscaling_profile

  # Dataplane V2 (Anthos) features
  datapath_provider = var.datapath_provider

  # Timeout for cluster operations
  removal_policy = var.cluster_removal_policy

  # Default node pool (will be removed if creating custom node pools)
  remove_default_node_pool = var.create_custom_node_pools

  initial_node_count = var.create_custom_node_pools ? 1 : var.initial_node_count

  # Master auth
  master_auth {
    client_certificate_config {
      issue_client_certificate = var.issue_client_certificate
    }
  }

  # Security settings
  master_authorized_networks_config {
    dynamic "cidr_blocks" {
      for_each = var.authorized_networks
      content {
        cidr_block   = cidr_blocks.value.cidr_block
        display_name = cidr_blocks.value.display_name
      }
    }
  }

  # Shielded GKE nodes
  shielded_nodes {
    enable_secure_boot          = var.enable_secure_boot
    enable_integrity_monitoring = var.enable_integrity_monitoring
  }

  # Confidential GKE nodes
  confidential_nodes {
    enabled = var.enable_confidential_nodes
  }

  # DNS cache
  dns_cache {
    enabled = var.enable_dns_cache
  }

  # Timeout
  timeouts {
    create = var.cluster_create_timeout
    update = var.cluster_update_timeout
    delete = var.cluster_delete_timeout
  }

  lifecycle {
    ignore_changes = [
      initial_node_count,
      node_config[0].image_type
    ]
  }
}

# ============================================================================
# Node Pool - Primary (Standard)
# ============================================================================
resource "google_container_node_pool" "primary" {
  count = var.create_custom_node_pools ? 1 : 0

  name     = "${var.cluster_name}-primary-pool"
  project  = var.project_id
  location = var.region
  cluster  = google_container_cluster.erlmcp.name

  version = var.node_pools["primary"].version

  node_config {
    machine_type    = var.node_pools["primary"].machine_type
    image_type      = var.node_pools["primary"].image_type
    disk_size_gb    = var.node_pools["primary"].disk_size_gb
    disk_type       = var.node_pools["primary"].disk_type
    spot            = var.node_pools["primary"].spot
    preemptible     = var.node_pools["primary"].preemptible

    oauth_scopes = var.node_oauth_scopes

    service_account = var.node_service_account

    shielded_instance_config {
      enable_secure_boot          = var.node_pools["primary"].enable_secure_boot
      enable_integrity_monitoring = var.node_pools["primary"].enable_integrity_monitoring
    }

    gvnic {
      enabled = var.node_pools["primary"].enable_gvnic
    }

    dynamic "taint" {
      for_each = var.node_pools["primary"].taints
      content {
        key    = taint.value.key
        value  = taint.value.value
        effect = taint.value.effect
      }
    }

    dynamic "reservation_affinity" {
      for_each = var.node_pools["primary"].reservation_affinity != null ? [1] : []
      content {
        consume_reservation_type = var.node_pools["primary"].reservation_affinity
      }
    }

    labels = merge(
      var.node_labels,
      {
        pool = "primary"
      }
    )

    tags = var.node_tags
  }

  autoscaling {
    min_node_count  = var.node_pools["primary"].min_count
    max_node_count  = var.node_pools["primary"].max_count
    location_policy = var.node_pools["primary"].location_policy
  }

  management {
    auto_repair  = var.enable_auto_repair
    auto_upgrade = var.enable_auto_upgrade
  }

  upgrade_settings {
    max_surge       = var.max_surge
    max_unavailable = var.max_unavailable
  }

  network_config {
    enable_private_nodes = var.enable_private_nodes
    pod_ipv4_cidr_block  = var.node_pools["primary"].pod_cidr
  }

  lifecycle {
    ignore_changes = [
      node_config[0].image_type
    ]
  }
}

# ============================================================================
# Node Pool - Spot (Preemptible) for cost optimization
# ============================================================================
resource "google_container_node_pool" "spot" {
  count = var.create_spot_node_pool ? 1 : 0

  name     = "${var.cluster_name}-spot-pool"
  project  = var.project_id
  location = var.region
  cluster  = google_container_cluster.erlmcp.name

  version = var.node_pools["spot"].version

  node_config {
    machine_type    = var.node_pools["spot"].machine_type
    image_type      = var.node_pools["spot"].image_type
    disk_size_gb    = var.node_pools["spot"].disk_size_gb
    disk_type       = var.node_pools["spot"].disk_type
    spot            = true
    preemptible     = true

    oauth_scopes = var.node_oauth_scopes
    service_account = var.node_service_account

    labels = merge(
      var.node_labels,
      {
        pool = "spot"
      }
    )

    tags = concat(var.node_tags, ["spot"])

    taint {
      key    = "cloud.google.com/gke-preemptible"
      value  = "true"
      effect = "NO_SCHEDULE"
    }
  }

  autoscaling {
    min_node_count  = var.node_pools["spot"].min_count
    max_node_count  = var.node_pools["spot"].max_count
    location_policy = var.node_pools["spot"].location_policy
  }

  management {
    auto_repair  = true
    auto_upgrade = false  # Don't auto-upgrade spot nodes
  }

  network_config {
    enable_private_nodes = var.enable_private_nodes
  }

  lifecycle {
    ignore_changes = [
      node_config[0].image_type
    ]
  }
}

# ============================================================================
# Get cluster endpoint for Helm provider
# ============================================================================
data "google_client_config" "default" {}

# ============================================================================
# Kubernetes provider configuration for Helm
# ============================================================================
provider "kubernetes" {
  host = "https://${google_container_cluster.erlmcp.endpoint}"

  token = data.google_client_config.default.access_token

  cluster_ca_certificate = base64decode(
    google_container_cluster.erlmcp.master_auth[0].cluster_ca_certificate
  )
}
