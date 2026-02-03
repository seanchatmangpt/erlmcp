# ============================================================================
# GKE Module Variables
# ============================================================================

variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "region" {
  type        = string
  description = "GCP region for regional cluster"
  default     = "us-central1"
}

variable "cluster_name" {
  type        = string
  description = "GKE cluster name"
  default     = "erlmcp-cluster"
}

variable "network" {
  type        = string
  description = "VPC network name"
  default     = "default"
}

variable "subnet" {
  type        = string
  description = "Subnet name"
  default     = ""
}

variable "kubernetes_version" {
  type        = string
  description = "Kubernetes version (leave empty for latest)"
  default     = ""
}

variable "release_channel" {
  type        = string
  description = "GKE release channel (RAPID, REGULAR, STABLE, UNSPECIFIED)"
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
  description = "Disable public endpoint for control plane"
  default     = true
}

variable "enable_private_nodes" {
  type        = bool
  description = "Enable private nodes (no public IP)"
  default     = true
}

variable "master_ipv4_cidr_block" {
  type        = string
  description = "CIDR block for GKE master access"
  default     = "172.16.0.0/28"
}

variable "master_global_access" {
  type        = bool
  description = "Enable global access to private endpoint"
  default     = false
}

# ============================================================================
# Node Pool Configuration
# ============================================================================
variable "initial_node_count" {
  type        = number
  description = "Initial node count for default pool"
  default     = 3
}

variable "create_custom_node_pools" {
  type        = bool
  description = "Create custom node pools (removes default pool)"
  default     = true
}

variable "create_spot_node_pool" {
  type        = bool
  description = "Create spot/preemptible node pool for cost savings"
  default     = false
}

variable "node_service_account" {
  type        = string
  description = "Service account for nodes (leave empty to use default)"
  default     = ""
}

variable "node_oauth_scopes" {
  type = list(string)
  description = "OAuth scopes for node service account"
  default = [
    "https://www.googleapis.com/auth/logging.write",
    "https://www.googleapis.com/auth/monitoring",
    "https://www.googleapis.com/auth/devstorage.read_only",
    "https://www.googleapis.com/auth/service.management.readonly",
    "https://www.googleapis.com/auth/servicecontrol",
    "https://www.googleapis.com/auth/trace.append",
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

variable "node_tags" {
  type        = list(string)
  description = "Network tags to apply to nodes"
  default     = ["erlmcp"]
}

# Node pool definitions
variable "node_pools" {
  type = map(object({
    version               = string
    machine_type          = string
    image_type            = string
    disk_size_gb          = number
    disk_type             = string
    min_count             = number
    max_count             = number
    location_policy       = string
    spot                  = bool
    preemptible           = bool
    enable_secure_boot    = bool
    enable_integrity_monitoring = bool
    enable_gvnic          = bool
    pod_cidr              = string
    taints                = list(object({
      key    = string
      value  = string
      effect = string
    }))
    reservation_affinity  = string
  }))
  description = "Configuration for each node pool"
  default = {
    primary = {
      version               = ""
      machine_type          = "e2-standard-2"
      image_type            = "COS_CONTAINERD"
      disk_size_gb          = 100
      disk_type             = "pd-balanced"
      min_count             = 3
      max_count             = 10
      location_policy       = "BALANCED"
      spot                  = false
      preemptible           = false
      enable_secure_boot    = true
      enable_integrity_monitoring = true
      enable_gvnic          = true
      pod_cidr              = ""
      taints                = []
      reservation_affinity  = ""
    }
    spot = {
      version               = ""
      machine_type          = "e2-standard-2"
      image_type            = "COS_CONTAINERD"
      disk_size_gb          = 100
      disk_type             = "pd-standard"
      min_count             = 0
      max_count             = 5
      location_policy       = "ANY"
      spot                  = true
      preemptible           = true
      enable_secure_boot    = false
      enable_integrity_monitoring = false
      enable_gvnic          = false
      pod_cidr              = ""
      taints                = []
      reservation_affinity  = ""
    }
  }
}

# ============================================================================
# Cluster Configuration
# ============================================================================
variable "logging_components" {
  type = list(string)
  description = "Logging components to enable"
  default = [
    "SYSTEM_COMPONENTS",
    "WORKLOADS",
    "APISERVER",
    "CONTROLLER_MANAGER",
    "SCHEDULER"
  ]
}

variable "monitoring_components" {
  type = list(string)
  description = "Monitoring components to enable"
  default = [
    "SYSTEM_COMPONENTS",
    "WORKLOADS",
    "APISERVER",
    "CONTROLLER_MANAGER",
    "SCHEDULER"
  ]
}

variable "enable_managed_prometheus" {
  type        = bool
  description = "Enable managed Prometheus collection"
  default     = true
}

variable "maintenance_window_start" {
  type        = string
  description = "Daily maintenance window start time (RFC3339)"
  default     = "03:00"
}

variable "resource_labels" {
  type        = map(string)
  description = "Resource labels for the cluster"
  default = {
    environment = "production"
  }
}

# ============================================================================
# Security Configuration
# ============================================================================
variable "http_load_balancing" {
  type        = bool
  description = "Enable HTTP load balancing (Cloud Ingress)"
  default     = true
}

variable "enable_network_policy" {
  type        = bool
  description = "Enable network policy"
  default     = true
}

variable "network_policy_provider" {
  type        = string
  description = "Network policy provider (CALICO or DENY_ALL)"
  default     = "CALICO"
}

variable "enable_pod_security_policy" {
  type        = bool
  description = "Enable pod security policy (deprecated)"
  default     = false
}

variable "binauthz_evaluation_mode" {
  type        = string
  description = "Binary authorization mode"
  default     = "DISABLED"
}

variable "enable_secure_boot" {
  type        = bool
  description = "Enable secure boot on shielded nodes"
  default     = true
}

variable "enable_integrity_monitoring" {
  type        = bool
  description = "Enable integrity monitoring on shielded nodes"
  default     = true
}

variable "enable_confidential_nodes" {
  type        = bool
  description = "Enable confidential GKE nodes"
  default     = false
}

variable "enable_dns_cache" {
  type        = bool
  description = "Enable DNS cache (NodeLocal DNSCache)"
  default     = true
}

variable "authorized_networks" {
  type = list(object({
    cidr_block   = string
    display_name = string
  }))
  description = "Authorized networks for master endpoint access"
  default = []
}

variable "issue_client_certificate" {
  type        = bool
  description = "Issue client certificate for cluster access"
  default     = false
}

# ============================================================================
# Upgrade and Autoscaling Configuration
# ============================================================================
variable "cluster_autoscaling_profile" {
  type        = string
  description = "Cluster autoscaling profile (BALANCED, OPTIMIZE_UTILIZATION, SCALE_OUT)"
  default     = "BALANCED"

  validation {
    condition     = contains(["BALANCED", "OPTIMIZE_UTILIZATION", "SCALE_OUT"], var.cluster_autoscaling_profile)
    error_message = "Autoscaling profile must be BALANCED, OPTIMIZE_UTILIZATION, or SCALE_OUT."
  }
}

variable "cluster_removal_policy" {
  type        = string
  description = "Removal policy for cluster (DELETE or ABANDON)"
  default     = "DELETE"

  validation {
    condition     = contains(["DELETE", "ABANDON"], var.cluster_removal_policy)
    error_message = "Removal policy must be DELETE or ABANDON."
  }
}

variable "datapath_provider" {
  type        = string
  description = "Dataplane provider (ADVANCED_DATAPATH or NONE)"
  default     = "ADVANCED_DATAPATH"

  validation {
    condition     = contains(["ADVANCED_DATAPATH", "NONE"], var.datapath_provider)
    error_message = "Datapath provider must be ADVANCED_DATAPATH or NONE."
  }
}

variable "enable_auto_repair" {
  type        = bool
  description = "Enable node auto-repair"
  default     = true
}

variable "enable_auto_upgrade" {
  type        = bool
  description = "Enable node auto-upgrade"
  default     = true
}

variable "max_surge" {
  type        = number
  description = "Max surge for node upgrades"
  default     = 1
}

variable "max_unavailable" {
  type        = number
  description = "Max unavailable for node upgrades"
  default     = 0
}

# ============================================================================
# Timeout Configuration
# ============================================================================
variable "cluster_create_timeout" {
  type        = string
  description = "Timeout for cluster creation"
  default     = "45m"
}

variable "cluster_update_timeout" {
  type        = string
  description = "Timeout for cluster updates"
  default     = "45m"
}

variable "cluster_delete_timeout" {
  type        = string
  description = "Timeout for cluster deletion"
  default     = "45m"
}

# ============================================================================
# Helm Chart Configuration
# ============================================================================
variable "deploy_helm_chart" {
  type        = bool
  description = "Deploy erlmcp Helm chart after cluster creation"
  default     = true
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
  type = map(any)
  description = "Helm values to override"
  default = {}
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
