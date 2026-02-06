# ============================================================================
# GKE Module for erlmcp Deployment
# Google Kubernetes Engine with regional configuration for 99.95% SLA
# Supports both Standard and Autopilot modes with latest GKE features
# ============================================================================

terraform {
  required_version = ">= 1.8.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 6.0.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = ">= 6.0.0"
    }
    helm = {
      source  = "hashicorp/helm"
      version = ">= 2.15.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = ">= 2.33.0"
    }
  }
}

# ============================================================================
# GKE Cluster (Standard or Autopilot)
# ============================================================================
resource "google_container_cluster" "erlmcp" {
  provider = google-beta

  name     = var.cluster_name
  location = var.region  # Regional cluster for 99.95% SLA
  project  = var.project_id

  # Autopilot mode (managed node pools, optimized for hands-off operation)
  enable_autopilot = var.enable_autopilot

  # Network configuration
  network    = var.network
  subnetwork = var.subnet

  # IP allocation policy for VPC-native cluster
  dynamic "ip_allocation_policy" {
    for_each = var.enable_autopilot || var.ip_allocation_policy != null ? [1] : []
    content {
      cluster_secondary_range_name  = var.ip_allocation_policy != null ? var.ip_allocation_policy.cluster_secondary_range_name : null
      services_secondary_range_name = var.ip_allocation_policy != null ? var.ip_allocation_policy.services_secondary_range_name : null
      cluster_ipv4_cidr_block       = var.ip_allocation_policy != null ? var.ip_allocation_policy.cluster_ipv4_cidr_block : null
      services_ipv4_cidr_block      = var.ip_allocation_policy != null ? var.ip_allocation_policy.services_ipv4_cidr_block : null
    }
  }

  # Private cluster configuration
  private_cluster_config {
    enable_private_endpoint = var.enable_private_endpoint
    enable_private_nodes    = var.enable_private_nodes
    master_ipv4_cidr_block  = var.master_ipv4_cidr_block

    dynamic "master_global_access_config" {
      for_each = var.master_global_access != null ? [1] : []
      content {
        enabled = var.master_global_access
      }
    }
  }

  # Release channel (RAPID for latest features, REGULAR for balance, STABLE for production)
  release_channel {
    channel = var.release_channel
  }

  # Cluster version (only for standard clusters, autopilot manages this)
  min_master_version = var.enable_autopilot ? null : var.kubernetes_version

  # Workload Identity for GCP service integration
  workload_identity_config {
    workload_pool = "${var.project_id}.svc.id.goog"
  }

  # Gateway API support (next-gen Ingress)
  dynamic "gateway_api_config" {
    for_each = var.enable_gateway_api ? [1] : []
    content {
      channel = var.gateway_api_channel
    }
  }

  # Cloud Operations for GKE (formerly Stackdriver)
  logging_config {
    enable_components = var.logging_components
  }

  monitoring_config {
    enable_components = var.monitoring_components

    # Managed Prometheus for Kubernetes metrics
    dynamic "managed_prometheus" {
      for_each = var.enable_managed_prometheus ? [1] : []
      content {
        enabled = true
      }
    }

    # Advanced datapath observability metrics
    dynamic "advanced_datapath_observability_config" {
      for_each = var.enable_advanced_datapath_observability ? [1] : []
      content {
        enable_metrics = true
        enable_relay   = var.enable_datapath_observability_relay
      }
    }
  }

  # Maintenance window
  maintenance_policy {
    daily_maintenance_window {
      start_time = var.maintenance_window_start
    }

    dynamic "recurring_window" {
      for_each = var.maintenance_exclusions
      content {
        start_time = recurring_window.value.start_time
        end_time   = recurring_window.value.end_time
        recurrence = recurring_window.value.recurrence
      }
    }
  }

  # Cost allocation and resource labels
  resource_labels = merge(
    var.resource_labels,
    {
      managed-by        = "terraform"
      app               = "erlmcp"
      cost-center       = var.cost_center
      gke-cluster       = var.cluster_name
      terraform-managed = "true"
    }
  )

  # Add-on configurations (Standard clusters only)
  dynamic "addons_config" {
    for_each = var.enable_autopilot ? [] : [1]
    content {
      http_load_balancing {
        disabled = !var.http_load_balancing
      }
      network_policy_config {
        disabled = !var.enable_network_policy
      }
      gce_persistent_disk_csi_driver_config {
        enabled = true
      }
      gcp_filestore_csi_driver_config {
        enabled = var.enable_filestore_csi
      }
      gcs_fuse_csi_driver_config {
        enabled = var.enable_gcs_fuse_csi
      }
      dns_cache_config {
        enabled = var.enable_dns_cache
      }
      gke_backup_agent_config {
        enabled = var.enable_backup_agent
      }
      config_connector_config {
        enabled = var.enable_config_connector
      }
    }
  }

  # Network policy (Calico or Dataplane V2)
  dynamic "network_policy" {
    for_each = !var.enable_autopilot && var.enable_network_policy ? [1] : []
    content {
      enabled  = true
      provider = var.network_policy_provider
    }
  }

  # Pod Security Standards (replaces deprecated Pod Security Policy)
  dynamic "security_posture_config" {
    for_each = var.enable_security_posture ? [1] : []
    content {
      mode               = var.security_posture_mode
      vulnerability_mode = var.security_posture_vulnerability_mode
    }
  }

  # Binary Authorization for container image attestation
  binary_authorization {
    evaluation_mode = var.binauthz_evaluation_mode
  }

  # Workload vulnerability scanning
  dynamic "workload_vulnerability_scanning" {
    for_each = var.enable_workload_vulnerability_scanning ? [1] : []
    content {
      scanning_mode = var.vulnerability_scanning_mode
    }
  }

  # Cluster autoscaling (Standard clusters only)
  dynamic "cluster_autoscaling" {
    for_each = !var.enable_autopilot && var.enable_cluster_autoscaling ? [1] : []
    content {
      enabled             = true
      autoscaling_profile = var.cluster_autoscaling_profile

      dynamic "resource_limits" {
        for_each = var.cluster_resource_limits
        content {
          resource_type = resource_limits.value.resource_type
          minimum       = resource_limits.value.minimum
          maximum       = resource_limits.value.maximum
        }
      }

      dynamic "auto_provisioning_defaults" {
        for_each = var.enable_node_autoprovisioning ? [1] : []
        content {
          oauth_scopes    = var.node_oauth_scopes
          service_account = var.node_service_account

          management {
            auto_repair  = true
            auto_upgrade = true
          }

          shielded_instance_config {
            enable_secure_boot          = var.enable_secure_boot
            enable_integrity_monitoring = var.enable_integrity_monitoring
          }
        }
      }
    }
  }

  # Dataplane V2 for enhanced networking and security
  datapath_provider = var.enable_autopilot ? "ADVANCED_DATAPATH" : var.datapath_provider

  # Fleet membership for multi-cluster management
  dynamic "fleet" {
    for_each = var.enable_fleet ? [1] : []
    content {
      project = var.fleet_project != "" ? var.fleet_project : var.project_id
    }
  }

  # Node pool configuration (Standard clusters only)
  dynamic "node_config" {
    for_each = !var.enable_autopilot && !var.create_custom_node_pools ? [1] : []
    content {
      machine_type    = var.default_machine_type
      service_account = var.node_service_account
      oauth_scopes    = var.node_oauth_scopes

      workload_metadata_config {
        mode = "GKE_METADATA"
      }

      shielded_instance_config {
        enable_secure_boot          = var.enable_secure_boot
        enable_integrity_monitoring = var.enable_integrity_monitoring
      }
    }
  }

  # Default node pool management
  remove_default_node_pool = var.enable_autopilot ? null : var.create_custom_node_pools
  initial_node_count       = var.enable_autopilot ? null : (var.create_custom_node_pools ? 1 : var.initial_node_count)

  # Master authentication
  master_auth {
    client_certificate_config {
      issue_client_certificate = var.issue_client_certificate
    }
  }

  # Master authorized networks for control plane access
  dynamic "master_authorized_networks_config" {
    for_each = length(var.authorized_networks) > 0 ? [1] : []
    content {
      dynamic "cidr_blocks" {
        for_each = var.authorized_networks
        content {
          cidr_block   = cidr_blocks.value.cidr_block
          display_name = cidr_blocks.value.display_name
        }
      }

      gcp_public_cidrs_access_enabled = var.enable_gcp_public_cidrs_access
    }
  }

  # Node system configuration for enhanced security
  dynamic "node_pool_defaults" {
    for_each = !var.enable_autopilot ? [1] : []
    content {
      node_config_defaults {
        logging_variant = var.node_logging_variant

        dynamic "gcfs_config" {
          for_each = var.enable_image_streaming ? [1] : []
          content {
            enabled = true
          }
        }

        dynamic "containerd_config" {
          for_each = var.containerd_config != null ? [1] : []
          content {
            dynamic "private_registry_access_config" {
              for_each = var.containerd_config.private_registry_access != null ? [1] : []
              content {
                enabled = true
                dynamic "certificate_authority_domain_config" {
                  for_each = var.containerd_config.private_registry_access.certificate_authority_domains
                  content {
                    fqdns = certificate_authority_domain_config.value.fqdns
                    gcp_secret_manager_certificate_config {
                      secret_uri = certificate_authority_domain_config.value.secret_uri
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  # DNS configuration
  dynamic "dns_config" {
    for_each = var.dns_config != null ? [1] : []
    content {
      cluster_dns        = var.dns_config.cluster_dns
      cluster_dns_scope  = var.dns_config.cluster_dns_scope
      cluster_dns_domain = var.dns_config.cluster_dns_domain
    }
  }

  # Notification configuration for cluster events
  dynamic "notification_config" {
    for_each = var.enable_notification_config ? [1] : []
    content {
      pubsub {
        enabled = true
        topic   = var.notification_pubsub_topic
      }
    }
  }

  # Confidential GKE nodes for sensitive workloads
  dynamic "confidential_nodes" {
    for_each = !var.enable_autopilot && var.enable_confidential_nodes ? [1] : []
    content {
      enabled = true
    }
  }

  # Cost management settings
  dynamic "cost_management_config" {
    for_each = var.enable_cost_allocation ? [1] : []
    content {
      enabled = true
    }
  }

  # Deletion protection
  deletion_protection = var.enable_deletion_protection

  # Timeouts
  timeouts {
    create = var.cluster_create_timeout
    update = var.cluster_update_timeout
    delete = var.cluster_delete_timeout
  }

  lifecycle {
    ignore_changes = [
      initial_node_count,
      node_config,
      node_pool,
    ]
  }

  depends_on = [
    google_project_service.container,
    google_project_service.compute,
  ]
}

# ============================================================================
# API Enablement
# ============================================================================
resource "google_project_service" "container" {
  project            = var.project_id
  service            = "container.googleapis.com"
  disable_on_destroy = false
}

resource "google_project_service" "compute" {
  project            = var.project_id
  service            = "compute.googleapis.com"
  disable_on_destroy = false
}

# ============================================================================
# Node Pool - Primary (Standard Clusters Only)
# ============================================================================
resource "google_container_node_pool" "primary" {
  provider = google-beta
  count    = !var.enable_autopilot && var.create_custom_node_pools ? 1 : 0

  name     = "${var.cluster_name}-primary-pool"
  project  = var.project_id
  location = var.region
  cluster  = google_container_cluster.erlmcp.name

  version = var.node_pools["primary"].version != "" ? var.node_pools["primary"].version : null

  node_config {
    machine_type    = var.node_pools["primary"].machine_type
    image_type      = var.node_pools["primary"].image_type
    disk_size_gb    = var.node_pools["primary"].disk_size_gb
    disk_type       = var.node_pools["primary"].disk_type
    spot            = var.node_pools["primary"].spot
    preemptible     = var.node_pools["primary"].preemptible

    oauth_scopes    = var.node_oauth_scopes
    service_account = var.node_service_account != "" ? var.node_service_account : null

    # Workload Identity
    workload_metadata_config {
      mode = "GKE_METADATA"
    }

    # Shielded VM features
    shielded_instance_config {
      enable_secure_boot          = var.node_pools["primary"].enable_secure_boot
      enable_integrity_monitoring = var.node_pools["primary"].enable_integrity_monitoring
    }

    # gVNIC for improved networking performance
    gvnic {
      enabled = var.node_pools["primary"].enable_gvnic
    }

    # Image streaming for faster pod startup
    dynamic "gcfs_config" {
      for_each = var.enable_image_streaming ? [1] : []
      content {
        enabled = true
      }
    }

    # Fast socket for enhanced networking
    dynamic "fast_socket" {
      for_each = var.enable_fast_socket ? [1] : []
      content {
        enabled = true
      }
    }

    # Local SSD configuration
    dynamic "ephemeral_storage_local_ssd_config" {
      for_each = var.node_pools["primary"].local_ssd_count > 0 ? [1] : []
      content {
        local_ssd_count = var.node_pools["primary"].local_ssd_count
      }
    }

    # Local SSD configuration for boot disk
    dynamic "local_nvme_ssd_block_config" {
      for_each = var.node_pools["primary"].local_nvme_ssd_count > 0 ? [1] : []
      content {
        local_ssd_count = var.node_pools["primary"].local_nvme_ssd_count
      }
    }

    # Confidential computing
    dynamic "confidential_nodes" {
      for_each = var.node_pools["primary"].enable_confidential_nodes ? [1] : []
      content {
        enabled = true
      }
    }

    # Advanced machine features
    dynamic "advanced_machine_features" {
      for_each = var.node_pools["primary"].threads_per_core > 0 ? [1] : []
      content {
        threads_per_core = var.node_pools["primary"].threads_per_core
      }
    }

    # Node taints
    dynamic "taint" {
      for_each = var.node_pools["primary"].taints
      content {
        key    = taint.value.key
        value  = taint.value.value
        effect = taint.value.effect
      }
    }

    # Reservation affinity
    dynamic "reservation_affinity" {
      for_each = var.node_pools["primary"].reservation_affinity != null ? [1] : []
      content {
        consume_reservation_type = var.node_pools["primary"].reservation_affinity.consume_reservation_type
        key                      = var.node_pools["primary"].reservation_affinity.key
        values                   = var.node_pools["primary"].reservation_affinity.values
      }
    }

    # Kubelet configuration
    dynamic "kubelet_config" {
      for_each = var.node_pools["primary"].kubelet_config != null ? [1] : []
      content {
        cpu_manager_policy   = var.node_pools["primary"].kubelet_config.cpu_manager_policy
        cpu_cfs_quota        = var.node_pools["primary"].kubelet_config.cpu_cfs_quota
        cpu_cfs_quota_period = var.node_pools["primary"].kubelet_config.cpu_cfs_quota_period
        pod_pids_limit       = var.node_pools["primary"].kubelet_config.pod_pids_limit
      }
    }

    # Linux node configuration
    dynamic "linux_node_config" {
      for_each = var.node_pools["primary"].linux_node_config != null ? [1] : []
      content {
        sysctls = var.node_pools["primary"].linux_node_config.sysctls

        dynamic "cgroup_mode" {
          for_each = var.node_pools["primary"].linux_node_config.cgroup_mode != null ? [1] : []
          content {
            value = var.node_pools["primary"].linux_node_config.cgroup_mode
          }
        }
      }
    }

    # Node labels
    labels = merge(
      var.node_labels,
      var.node_pools["primary"].labels,
      {
        pool               = "primary"
        node-pool-type     = "standard"
        workload-type      = "general"
        cost-optimization  = "false"
      }
    )

    # Metadata
    metadata = merge(
      var.node_metadata,
      {
        disable-legacy-endpoints = "true"
      }
    )

    tags = concat(var.node_tags, ["gke-${var.cluster_name}", "gke-node"])
  }

  # Autoscaling
  autoscaling {
    min_node_count       = var.node_pools["primary"].min_count
    max_node_count       = var.node_pools["primary"].max_count
    location_policy      = var.node_pools["primary"].location_policy
    total_min_node_count = var.node_pools["primary"].total_min_node_count
    total_max_node_count = var.node_pools["primary"].total_max_node_count
  }

  # Node pool management
  management {
    auto_repair  = var.enable_auto_repair
    auto_upgrade = var.enable_auto_upgrade
  }

  # Upgrade settings for blue-green deployments
  upgrade_settings {
    strategy        = var.node_pool_upgrade_strategy
    max_surge       = var.node_pool_upgrade_strategy == "SURGE" ? var.max_surge : 0
    max_unavailable = var.node_pool_upgrade_strategy == "SURGE" ? var.max_unavailable : 0

    dynamic "blue_green_settings" {
      for_each = var.node_pool_upgrade_strategy == "BLUE_GREEN" ? [1] : []
      content {
        node_pool_soak_duration = var.blue_green_soak_duration

        standard_rollout_policy {
          batch_percentage    = var.blue_green_batch_percentage
          batch_node_count    = var.blue_green_batch_node_count
          batch_soak_duration = var.blue_green_batch_soak_duration
        }
      }
    }
  }

  # Network configuration
  network_config {
    create_pod_range     = var.node_pools["primary"].create_pod_range
    enable_private_nodes = var.enable_private_nodes
    pod_ipv4_cidr_block  = var.node_pools["primary"].pod_cidr != "" ? var.node_pools["primary"].pod_cidr : null
    pod_range            = var.node_pools["primary"].pod_range

    dynamic "additional_node_network_configs" {
      for_each = var.node_pools["primary"].additional_node_networks
      content {
        network    = additional_node_network_configs.value.network
        subnetwork = additional_node_network_configs.value.subnetwork
      }
    }

    dynamic "additional_pod_network_configs" {
      for_each = var.node_pools["primary"].additional_pod_networks
      content {
        subnetwork         = additional_pod_network_configs.value.subnetwork
        secondary_pod_range = additional_pod_network_configs.value.secondary_pod_range
        max_pods_per_node  = additional_pod_network_configs.value.max_pods_per_node
      }
    }

    dynamic "network_performance_config" {
      for_each = var.node_pools["primary"].network_performance_tier != null ? [1] : []
      content {
        total_egress_bandwidth_tier = var.node_pools["primary"].network_performance_tier
      }
    }

    pod_cidr_overprovision_config {
      disabled = var.disable_pod_cidr_overprovision
    }
  }

  # Placement policy for compact placement
  dynamic "placement_policy" {
    for_each = var.node_pools["primary"].placement_policy != null ? [1] : []
    content {
      type         = var.node_pools["primary"].placement_policy.type
      policy_name  = var.node_pools["primary"].placement_policy.policy_name
      tpu_topology = var.node_pools["primary"].placement_policy.tpu_topology
    }
  }

  lifecycle {
    ignore_changes = [
      node_config[0].image_type,
      node_config[0].labels,
      initial_node_count,
    ]
    create_before_destroy = true
  }

  depends_on = [
    google_container_cluster.erlmcp
  ]
}

# ============================================================================
# Node Pool - Spot/Preemptible for cost optimization (Standard Clusters Only)
# ============================================================================
resource "google_container_node_pool" "spot" {
  provider = google-beta
  count    = !var.enable_autopilot && var.create_spot_node_pool ? 1 : 0

  name     = "${var.cluster_name}-spot-pool"
  project  = var.project_id
  location = var.region
  cluster  = google_container_cluster.erlmcp.name

  version = var.node_pools["spot"].version != "" ? var.node_pools["spot"].version : null

  node_config {
    machine_type = var.node_pools["spot"].machine_type
    image_type   = var.node_pools["spot"].image_type
    disk_size_gb = var.node_pools["spot"].disk_size_gb
    disk_type    = var.node_pools["spot"].disk_type
    spot         = true      # Use Spot VMs (newer, more flexible)
    preemptible  = false     # Don't use legacy preemptible

    oauth_scopes    = var.node_oauth_scopes
    service_account = var.node_service_account != "" ? var.node_service_account : null

    # Workload Identity
    workload_metadata_config {
      mode = "GKE_METADATA"
    }

    # Minimal shielded features for cost savings
    shielded_instance_config {
      enable_secure_boot          = false
      enable_integrity_monitoring = true
    }

    # gVNIC for improved networking
    gvnic {
      enabled = var.node_pools["spot"].enable_gvnic
    }

    # Image streaming
    dynamic "gcfs_config" {
      for_each = var.enable_image_streaming ? [1] : []
      content {
        enabled = true
      }
    }

    # Spot node taints
    taint {
      key    = "cloud.google.com/gke-spot"
      value  = "true"
      effect = "NO_SCHEDULE"
    }

    dynamic "taint" {
      for_each = var.node_pools["spot"].taints
      content {
        key    = taint.value.key
        value  = taint.value.value
        effect = taint.value.effect
      }
    }

    # Labels for pod scheduling and cost tracking
    labels = merge(
      var.node_labels,
      var.node_pools["spot"].labels,
      {
        pool               = "spot"
        node-pool-type     = "spot"
        workload-type      = "batch"
        cost-optimization  = "true"
        interruption-risk  = "high"
      }
    )

    # Metadata
    metadata = merge(
      var.node_metadata,
      {
        disable-legacy-endpoints = "true"
      }
    )

    tags = concat(var.node_tags, ["gke-${var.cluster_name}", "gke-node", "spot"])
  }

  # Autoscaling
  autoscaling {
    min_node_count       = var.node_pools["spot"].min_count
    max_node_count       = var.node_pools["spot"].max_count
    location_policy      = var.node_pools["spot"].location_policy
    total_min_node_count = var.node_pools["spot"].total_min_node_count
    total_max_node_count = var.node_pools["spot"].total_max_node_count
  }

  # Management
  management {
    auto_repair  = true
    auto_upgrade = var.enable_spot_auto_upgrade
  }

  # Upgrade settings
  upgrade_settings {
    strategy        = "SURGE"  # Surge is more appropriate for spot pools
    max_surge       = var.spot_max_surge
    max_unavailable = var.spot_max_unavailable
  }

  # Network configuration
  network_config {
    create_pod_range     = var.node_pools["spot"].create_pod_range
    enable_private_nodes = var.enable_private_nodes
    pod_ipv4_cidr_block  = var.node_pools["spot"].pod_cidr != "" ? var.node_pools["spot"].pod_cidr : null
    pod_range            = var.node_pools["spot"].pod_range

    pod_cidr_overprovision_config {
      disabled = var.disable_pod_cidr_overprovision
    }
  }

  lifecycle {
    ignore_changes = [
      node_config[0].image_type,
      node_config[0].labels,
      initial_node_count,
    ]
    create_before_destroy = true
  }

  depends_on = [
    google_container_cluster.erlmcp
  ]
}

# ============================================================================
# Data Sources
# ============================================================================
data "google_client_config" "default" {}

data "google_project" "project" {
  project_id = var.project_id
}

# Get available GKE versions
data "google_container_engine_versions" "default" {
  project        = var.project_id
  location       = var.region
  version_prefix = var.kubernetes_version != "" ? var.kubernetes_version : ""
}

# ============================================================================
# Kubernetes Provider Configuration
# ============================================================================
provider "kubernetes" {
  host  = "https://${google_container_cluster.erlmcp.endpoint}"
  token = data.google_client_config.default.access_token

  cluster_ca_certificate = base64decode(
    google_container_cluster.erlmcp.master_auth[0].cluster_ca_certificate
  )

  ignore_annotations = [
    "^autopilot\\.gke\\.io\\/.*",
    "^cloud\\.google\\.com\\/.*",
  ]
}

# ============================================================================
# Helm Provider Configuration
# ============================================================================
provider "helm" {
  kubernetes {
    host  = "https://${google_container_cluster.erlmcp.endpoint}"
    token = data.google_client_config.default.access_token

    cluster_ca_certificate = base64decode(
      google_container_cluster.erlmcp.master_auth[0].cluster_ca_certificate
    )
  }
}
