# ============================================================================
# GKE Module Variables - Updated for GKE 1.30+ with Autopilot Support
# ============================================================================

# ============================================================================
# Project and Location
# ============================================================================
variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "region" {
  type        = string
  description = "GCP region for regional cluster (99.95% SLA)"
  default     = "us-central1"
}

variable "cluster_name" {
  type        = string
  description = "GKE cluster name"
  default     = "erlmcp-cluster"

  validation {
    condition     = can(regex("^[a-z](?:[-a-z0-9]{0,38}[a-z0-9])?$", var.cluster_name))
    error_message = "Cluster name must be lowercase alphanumeric with hyphens, max 40 chars."
  }
}

# ============================================================================
# Cluster Mode
# ============================================================================
variable "enable_autopilot" {
  type        = bool
  description = "Enable GKE Autopilot mode (fully managed nodes)"
  default     = false
}

# ============================================================================
# Network Configuration
# ============================================================================
variable "network" {
  type        = string
  description = "VPC network name or self-link"
  default     = "default"
}

variable "subnet" {
  type        = string
  description = "Subnet name or self-link"
  default     = ""
}

variable "ip_allocation_policy" {
  type = object({
    cluster_secondary_range_name  = optional(string)
    services_secondary_range_name = optional(string)
    cluster_ipv4_cidr_block       = optional(string)
    services_ipv4_cidr_block      = optional(string)
  })
  description = "IP allocation policy for VPC-native cluster"
  default     = null
}

# ============================================================================
# Kubernetes Version
# ============================================================================
variable "kubernetes_version" {
  type        = string
  description = "Kubernetes version prefix (e.g., '1.30.') - leave empty for latest in channel"
  default     = ""
}

variable "release_channel" {
  type        = string
  description = "GKE release channel (RAPID, REGULAR, STABLE)"
  default     = "REGULAR"

  validation {
    condition     = contains(["RAPID", "REGULAR", "STABLE", "UNSPECIFIED"], var.release_channel)
    error_message = "Release channel must be RAPID, REGULAR, STABLE, or UNSPECIFIED."
  }
}

# ============================================================================
# Private Cluster Configuration
# ============================================================================
variable "enable_private_endpoint" {
  type        = bool
  description = "Disable public endpoint for control plane (recommended for production)"
  default     = true
}

variable "enable_private_nodes" {
  type        = bool
  description = "Enable private nodes (no external IP addresses)"
  default     = true
}

variable "master_ipv4_cidr_block" {
  type        = string
  description = "CIDR block for GKE control plane (/28 required)"
  default     = "172.16.0.0/28"

  validation {
    condition     = can(regex("^([0-9]{1,3}\\.){3}[0-9]{1,3}/28$", var.master_ipv4_cidr_block))
    error_message = "Master CIDR block must be a /28 network."
  }
}

variable "master_global_access" {
  type        = bool
  description = "Enable global access to private control plane endpoint"
  default     = null
}

variable "authorized_networks" {
  type = list(object({
    cidr_block   = string
    display_name = string
  }))
  description = "Authorized networks for control plane access"
  default     = []
}

variable "enable_gcp_public_cidrs_access" {
  type        = bool
  description = "Allow access from GCP public IP ranges"
  default     = false
}

# ============================================================================
# Gateway API Configuration
# ============================================================================
variable "enable_gateway_api" {
  type        = bool
  description = "Enable Gateway API for advanced ingress capabilities"
  default     = false
}

variable "gateway_api_channel" {
  type        = string
  description = "Gateway API channel (CHANNEL_DISABLED, CHANNEL_EXPERIMENTAL, CHANNEL_STANDARD)"
  default     = "CHANNEL_STANDARD"

  validation {
    condition     = contains(["CHANNEL_DISABLED", "CHANNEL_EXPERIMENTAL", "CHANNEL_STANDARD"], var.gateway_api_channel)
    error_message = "Gateway API channel must be CHANNEL_DISABLED, CHANNEL_EXPERIMENTAL, or CHANNEL_STANDARD."
  }
}

# ============================================================================
# Node Pool Configuration (Standard Clusters Only)
# ============================================================================
variable "initial_node_count" {
  type        = number
  description = "Initial node count for default pool (ignored if remove_default_node_pool=true)"
  default     = 3
}

variable "default_machine_type" {
  type        = string
  description = "Default machine type for nodes"
  default     = "e2-standard-4"
}

variable "create_custom_node_pools" {
  type        = bool
  description = "Create custom node pools (removes default pool)"
  default     = true
}

variable "create_spot_node_pool" {
  type        = bool
  description = "Create Spot VM node pool for cost optimization"
  default     = false
}

variable "node_service_account" {
  type        = string
  description = "Service account email for nodes (leave empty to create new)"
  default     = ""
}

variable "node_oauth_scopes" {
  type        = list(string)
  description = "OAuth scopes for node service account"
  default = [
    "https://www.googleapis.com/auth/cloud-platform",
  ]
}

variable "node_labels" {
  type        = map(string)
  description = "Labels to apply to all nodes"
  default = {
    environment = "production"
    managed-by  = "terraform"
  }
}

variable "node_metadata" {
  type        = map(string)
  description = "Metadata to apply to all nodes"
  default     = {}
}

variable "node_tags" {
  type        = list(string)
  description = "Network tags to apply to nodes"
  default     = ["erlmcp"]
}

# Advanced node pool definitions
variable "node_pools" {
  type = map(object({
    version                     = optional(string, "")
    machine_type                = optional(string, "e2-standard-4")
    image_type                  = optional(string, "COS_CONTAINERD")
    disk_size_gb                = optional(number, 100)
    disk_type                   = optional(string, "pd-balanced")
    min_count                   = optional(number, 3)
    max_count                   = optional(number, 10)
    total_min_node_count        = optional(number, null)
    total_max_node_count        = optional(number, null)
    location_policy             = optional(string, "BALANCED")
    spot                        = optional(bool, false)
    preemptible                 = optional(bool, false)
    enable_secure_boot          = optional(bool, true)
    enable_integrity_monitoring = optional(bool, true)
    enable_gvnic                = optional(bool, true)
    enable_confidential_nodes   = optional(bool, false)
    local_ssd_count             = optional(number, 0)
    local_nvme_ssd_count        = optional(number, 0)
    threads_per_core            = optional(number, 0)
    pod_cidr                    = optional(string, "")
    pod_range                   = optional(string, "")
    create_pod_range            = optional(bool, false)
    labels                      = optional(map(string), {})
    taints = optional(list(object({
      key    = string
      value  = string
      effect = string
    })), [])
    reservation_affinity = optional(object({
      consume_reservation_type = string
      key                      = optional(string)
      values                   = optional(list(string))
    }), null)
    kubelet_config = optional(object({
      cpu_manager_policy   = optional(string, "none")
      cpu_cfs_quota        = optional(bool, true)
      cpu_cfs_quota_period = optional(string, "100ms")
      pod_pids_limit       = optional(number, -1)
    }), null)
    linux_node_config = optional(object({
      sysctls     = optional(map(string), {})
      cgroup_mode = optional(string, "CGROUP_MODE_UNSPECIFIED")
    }), null)
    additional_node_networks = optional(list(object({
      network    = string
      subnetwork = string
    })), [])
    additional_pod_networks = optional(list(object({
      subnetwork          = string
      secondary_pod_range = string
      max_pods_per_node   = optional(number, 110)
    })), [])
    network_performance_tier = optional(string, null)
    placement_policy = optional(object({
      type         = string
      policy_name  = optional(string)
      tpu_topology = optional(string)
    }), null)
  }))
  description = "Configuration for each node pool"
  default = {
    primary = {
      version                     = ""
      machine_type                = "e2-standard-4"
      image_type                  = "COS_CONTAINERD"
      disk_size_gb                = 100
      disk_type                   = "pd-balanced"
      min_count                   = 3
      max_count                   = 10
      location_policy             = "BALANCED"
      spot                        = false
      preemptible                 = false
      enable_secure_boot          = true
      enable_integrity_monitoring = true
      enable_gvnic                = true
      enable_confidential_nodes   = false
      local_ssd_count             = 0
      local_nvme_ssd_count        = 0
      threads_per_core            = 0
      pod_cidr                    = ""
      pod_range                   = ""
      create_pod_range            = false
      labels                      = {}
      taints                      = []
      reservation_affinity        = null
      kubelet_config              = null
      linux_node_config           = null
      additional_node_networks    = []
      additional_pod_networks     = []
      network_performance_tier    = null
      placement_policy            = null
    }
    spot = {
      version                     = ""
      machine_type                = "e2-standard-4"
      image_type                  = "COS_CONTAINERD"
      disk_size_gb                = 100
      disk_type                   = "pd-standard"
      min_count                   = 0
      max_count                   = 5
      location_policy             = "ANY"
      spot                        = true
      preemptible                 = false
      enable_secure_boot          = false
      enable_integrity_monitoring = true
      enable_gvnic                = true
      enable_confidential_nodes   = false
      local_ssd_count             = 0
      local_nvme_ssd_count        = 0
      threads_per_core            = 0
      pod_cidr                    = ""
      pod_range                   = ""
      create_pod_range            = false
      labels                      = {}
      taints                      = []
      reservation_affinity        = null
      kubelet_config              = null
      linux_node_config           = null
      additional_node_networks    = []
      additional_pod_networks     = []
      network_performance_tier    = null
      placement_policy            = null
    }
  }
}

# ============================================================================
# Logging and Monitoring
# ============================================================================
variable "logging_components" {
  type        = list(string)
  description = "GKE components to send logs for"
  default = [
    "SYSTEM_COMPONENTS",
    "WORKLOADS",
  ]
}

variable "monitoring_components" {
  type        = list(string)
  description = "GKE components to collect metrics for"
  default = [
    "SYSTEM_COMPONENTS",
    "WORKLOADS",
  ]
}

variable "enable_managed_prometheus" {
  type        = bool
  description = "Enable Google Cloud Managed Service for Prometheus"
  default     = true
}

variable "enable_advanced_datapath_observability" {
  type        = bool
  description = "Enable advanced datapath observability (Dataplane V2 required)"
  default     = false
}

variable "enable_datapath_observability_relay" {
  type        = bool
  description = "Enable datapath observability relay"
  default     = false
}

# ============================================================================
# Maintenance Configuration
# ============================================================================
variable "maintenance_window_start" {
  type        = string
  description = "Daily maintenance window start time (HH:MM format, RFC3339)"
  default     = "03:00"

  validation {
    condition     = can(regex("^([01][0-9]|2[0-3]):[0-5][0-9]$", var.maintenance_window_start))
    error_message = "Maintenance window must be in HH:MM format (24-hour)."
  }
}

variable "maintenance_exclusions" {
  type = list(object({
    start_time = string
    end_time   = string
    recurrence = string
  }))
  description = "Maintenance exclusion windows (e.g., for Black Friday)"
  default     = []
}

# ============================================================================
# Resource Labels and Cost Management
# ============================================================================
variable "resource_labels" {
  type        = map(string)
  description = "Resource labels for the cluster"
  default = {
    environment = "production"
  }
}

variable "cost_center" {
  type        = string
  description = "Cost center label for billing attribution"
  default     = "engineering"
}

variable "enable_cost_allocation" {
  type        = bool
  description = "Enable GKE cost allocation for namespace-level billing"
  default     = true
}

# ============================================================================
# Add-ons Configuration (Standard Clusters Only)
# ============================================================================
variable "http_load_balancing" {
  type        = bool
  description = "Enable HTTP(S) Load Balancing add-on"
  default     = true
}

variable "enable_network_policy" {
  type        = bool
  description = "Enable network policy enforcement"
  default     = true
}

variable "network_policy_provider" {
  type        = string
  description = "Network policy provider (CALICO for standard, PROVIDER_UNSPECIFIED for Dataplane V2)"
  default     = "PROVIDER_UNSPECIFIED"
}

variable "enable_filestore_csi" {
  type        = bool
  description = "Enable Filestore CSI driver"
  default     = false
}

variable "enable_gcs_fuse_csi" {
  type        = bool
  description = "Enable GCS FUSE CSI driver for Cloud Storage mounting"
  default     = true
}

variable "enable_dns_cache" {
  type        = bool
  description = "Enable NodeLocal DNSCache for improved DNS performance"
  default     = true
}

variable "enable_backup_agent" {
  type        = bool
  description = "Enable GKE Backup for DR"
  default     = false
}

variable "enable_config_connector" {
  type        = bool
  description = "Enable Config Connector for GCP resource management"
  default     = false
}

# ============================================================================
# Security Configuration
# ============================================================================
variable "enable_security_posture" {
  type        = bool
  description = "Enable GKE Security Posture management"
  default     = true
}

variable "security_posture_mode" {
  type        = string
  description = "Security posture mode (BASIC, ENTERPRISE, DISABLED)"
  default     = "BASIC"

  validation {
    condition     = contains(["BASIC", "ENTERPRISE", "DISABLED"], var.security_posture_mode)
    error_message = "Security posture mode must be BASIC, ENTERPRISE, or DISABLED."
  }
}

variable "security_posture_vulnerability_mode" {
  type        = string
  description = "Vulnerability scanning mode (VULNERABILITY_DISABLED, VULNERABILITY_BASIC, VULNERABILITY_ENTERPRISE)"
  default     = "VULNERABILITY_BASIC"

  validation {
    condition     = contains(["VULNERABILITY_DISABLED", "VULNERABILITY_BASIC", "VULNERABILITY_ENTERPRISE"], var.security_posture_vulnerability_mode)
    error_message = "Vulnerability mode must be VULNERABILITY_DISABLED, VULNERABILITY_BASIC, or VULNERABILITY_ENTERPRISE."
  }
}

variable "binauthz_evaluation_mode" {
  type        = string
  description = "Binary Authorization evaluation mode (PROJECT_SINGLETON_POLICY_ENFORCE, POLICY_BINDINGS, DISABLED)"
  default     = "DISABLED"

  validation {
    condition     = contains(["PROJECT_SINGLETON_POLICY_ENFORCE", "POLICY_BINDINGS", "DISABLED"], var.binauthz_evaluation_mode)
    error_message = "Binary Authorization mode must be PROJECT_SINGLETON_POLICY_ENFORCE, POLICY_BINDINGS, or DISABLED."
  }
}

variable "enable_workload_vulnerability_scanning" {
  type        = bool
  description = "Enable workload vulnerability scanning"
  default     = false
}

variable "vulnerability_scanning_mode" {
  type        = string
  description = "Workload vulnerability scanning mode"
  default     = "VULNERABILITY_SCANNING_MODE_UNSPECIFIED"
}

variable "enable_secure_boot" {
  type        = bool
  description = "Enable Secure Boot on Shielded GKE nodes"
  default     = true
}

variable "enable_integrity_monitoring" {
  type        = bool
  description = "Enable Integrity Monitoring on Shielded GKE nodes"
  default     = true
}

variable "enable_confidential_nodes" {
  type        = bool
  description = "Enable Confidential GKE nodes (requires N2D machine types)"
  default     = false
}

variable "issue_client_certificate" {
  type        = bool
  description = "Issue client certificate for basic auth (not recommended)"
  default     = false
}

# ============================================================================
# Cluster Autoscaling (Standard Clusters Only)
# ============================================================================
variable "enable_cluster_autoscaling" {
  type        = bool
  description = "Enable cluster-level autoscaling (node auto-provisioning)"
  default     = false
}

variable "cluster_autoscaling_profile" {
  type        = string
  description = "Cluster autoscaling profile (BALANCED, OPTIMIZE_UTILIZATION)"
  default     = "BALANCED"

  validation {
    condition     = contains(["BALANCED", "OPTIMIZE_UTILIZATION"], var.cluster_autoscaling_profile)
    error_message = "Autoscaling profile must be BALANCED or OPTIMIZE_UTILIZATION."
  }
}

variable "enable_node_autoprovisioning" {
  type        = bool
  description = "Enable node auto-provisioning for cluster autoscaling"
  default     = false
}

variable "cluster_resource_limits" {
  type = list(object({
    resource_type = string
    minimum       = number
    maximum       = number
  }))
  description = "Resource limits for cluster autoscaling"
  default = []
}

# ============================================================================
# Networking Features
# ============================================================================
variable "datapath_provider" {
  type        = string
  description = "Datapath provider (ADVANCED_DATAPATH for Dataplane V2, LEGACY_DATAPATH for standard)"
  default     = "ADVANCED_DATAPATH"

  validation {
    condition     = contains(["ADVANCED_DATAPATH", "LEGACY_DATAPATH"], var.datapath_provider)
    error_message = "Datapath provider must be ADVANCED_DATAPATH or LEGACY_DATAPATH."
  }
}

variable "dns_config" {
  type = object({
    cluster_dns        = string
    cluster_dns_scope  = string
    cluster_dns_domain = optional(string, "cluster.local")
  })
  description = "DNS configuration for the cluster"
  default     = null
}

variable "disable_pod_cidr_overprovision" {
  type        = bool
  description = "Disable pod CIDR over-provisioning"
  default     = false
}

# ============================================================================
# Node Pool Management
# ============================================================================
variable "enable_auto_repair" {
  type        = bool
  description = "Enable auto-repair for node pools"
  default     = true
}

variable "enable_auto_upgrade" {
  type        = bool
  description = "Enable auto-upgrade for node pools"
  default     = true
}

variable "enable_spot_auto_upgrade" {
  type        = bool
  description = "Enable auto-upgrade for spot node pools"
  default     = false
}

variable "node_pool_upgrade_strategy" {
  type        = string
  description = "Node pool upgrade strategy (SURGE or BLUE_GREEN)"
  default     = "SURGE"

  validation {
    condition     = contains(["SURGE", "BLUE_GREEN"], var.node_pool_upgrade_strategy)
    error_message = "Upgrade strategy must be SURGE or BLUE_GREEN."
  }
}

variable "max_surge" {
  type        = number
  description = "Max surge for SURGE upgrade strategy"
  default     = 1
}

variable "max_unavailable" {
  type        = number
  description = "Max unavailable for SURGE upgrade strategy"
  default     = 0
}

variable "spot_max_surge" {
  type        = number
  description = "Max surge for spot node pools"
  default     = 3
}

variable "spot_max_unavailable" {
  type        = number
  description = "Max unavailable for spot node pools"
  default     = 1
}

variable "blue_green_soak_duration" {
  type        = string
  description = "Soak duration for blue-green deployment (e.g., '3600s')"
  default     = "3600s"
}

variable "blue_green_batch_percentage" {
  type        = number
  description = "Batch percentage for blue-green rollout"
  default     = 0.5
}

variable "blue_green_batch_node_count" {
  type        = number
  description = "Batch node count for blue-green rollout"
  default     = 3
}

variable "blue_green_batch_soak_duration" {
  type        = string
  description = "Batch soak duration for blue-green deployment"
  default     = "300s"
}

# ============================================================================
# Advanced Node Features
# ============================================================================
variable "enable_image_streaming" {
  type        = bool
  description = "Enable image streaming for faster pod startup (GCFS)"
  default     = true
}

variable "enable_fast_socket" {
  type        = bool
  description = "Enable Fast Socket for improved networking performance"
  default     = false
}

variable "node_logging_variant" {
  type        = string
  description = "Node logging variant (DEFAULT or MAX_THROUGHPUT)"
  default     = "DEFAULT"

  validation {
    condition     = contains(["DEFAULT", "MAX_THROUGHPUT"], var.node_logging_variant)
    error_message = "Node logging variant must be DEFAULT or MAX_THROUGHPUT."
  }
}

variable "containerd_config" {
  type = object({
    private_registry_access = optional(object({
      certificate_authority_domains = list(object({
        fqdns      = list(string)
        secret_uri = string
      }))
    }), null)
  })
  description = "Containerd configuration for private registry access"
  default     = null
}

# ============================================================================
# Fleet Management
# ============================================================================
variable "enable_fleet" {
  type        = bool
  description = "Enable GKE Fleet membership for multi-cluster management"
  default     = false
}

variable "fleet_project" {
  type        = string
  description = "Fleet host project (leave empty to use cluster project)"
  default     = ""
}

# ============================================================================
# Notifications
# ============================================================================
variable "enable_notification_config" {
  type        = bool
  description = "Enable Pub/Sub notifications for cluster events"
  default     = false
}

variable "notification_pubsub_topic" {
  type        = string
  description = "Pub/Sub topic for cluster notifications"
  default     = ""
}

# ============================================================================
# Protection
# ============================================================================
variable "enable_deletion_protection" {
  type        = bool
  description = "Enable deletion protection for the cluster"
  default     = true
}

# ============================================================================
# Timeouts
# ============================================================================
variable "cluster_create_timeout" {
  type        = string
  description = "Timeout for cluster creation"
  default     = "60m"
}

variable "cluster_update_timeout" {
  type        = string
  description = "Timeout for cluster updates"
  default     = "60m"
}

variable "cluster_delete_timeout" {
  type        = string
  description = "Timeout for cluster deletion"
  default     = "60m"
}

# ============================================================================
# Helm Deployment Configuration
# ============================================================================
variable "deploy_helm_chart" {
  type        = bool
  description = "Deploy erlmcp Helm chart after cluster creation"
  default     = false
}

variable "helm_chart_path" {
  type        = string
  description = "Path to erlmcp Helm chart"
  default     = "../../../helm/erlmcp-marketplace"
}

variable "helm_release_name" {
  type        = string
  description = "Helm release name"
  default     = "erlmcp"
}

variable "helm_namespace" {
  type        = string
  description = "Kubernetes namespace for deployment"
  default     = "erlmcp"
}

variable "helm_values" {
  type        = map(any)
  description = "Helm values to override"
  default     = {}
}

variable "image_repository" {
  type        = string
  description = "Container image repository"
  default     = "us-central1-docker.pkg.dev"
}

variable "image_name" {
  type        = string
  description = "Container image name"
  default     = "erlmcp/erlmcp"
}

variable "image_tag" {
  type        = string
  description = "Container image tag"
  default     = "3.0.0"
}
