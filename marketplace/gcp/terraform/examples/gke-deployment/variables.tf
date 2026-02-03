# ============================================================================
# GKE Deployment Variables
# ============================================================================

variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "region" {
  type        = string
  description = "GCP region"
  default     = "us-central1"
}

variable "cluster_name" {
  type        = string
  description = "GKE cluster name"
  default     = "erlmcp-cluster"
}

variable "network_name" {
  type        = string
  description = "VPC network name"
  default     = "erlmcp-vpc"
}

variable "subnets" {
  type = list(object({
    name                   = string
    region                 = string
    ip_cidr_range          = string
    private_ip_google_access = bool
    purpose                = string
    role                   = string
    secondary_ip_ranges    = list(object({
      range_name    = string
      ip_cidr_range = string
    }))
  }))
  description = "Subnet configurations"
  default = [
    {
      name                   = "erlmcp-subnet-us-central1"
      region                 = "us-central1"
      ip_cidr_range          = "10.0.0.0/24"
      private_ip_google_access = true
      purpose                = null
      role                   = null
      secondary_ip_ranges    = [
        {
          range_name    = "pods"
          ip_cidr_range = "10.1.0.0/16"
        },
        {
          range_name    = "services"
          ip_cidr_range = "10.2.0.0/16"
        }
      ]
    }
  ]
}

# ============================================================================
# GKE Configuration
# ============================================================================
variable "kubernetes_version" {
  type        = string
  description = "Kubernetes version (leave empty for latest)"
  default     = ""
}

variable "release_channel" {
  type        = string
  description = "GKE release channel"
  default     = "REGULAR"
}

variable "enable_private_endpoint" {
  type        = bool
  description = "Enable private endpoint for control plane"
  default     = true
}

variable "enable_private_nodes" {
  type        = bool
  description = "Enable private nodes"
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

variable "enable_secure_boot" {
  type        = bool
  description = "Enable secure boot on shielded nodes"
  default     = true
}

variable "enable_integrity_monitoring" {
  type        = bool
  description = "Enable integrity monitoring"
  default     = true
}

variable "enable_managed_prometheus" {
  type        = bool
  description = "Enable managed Prometheus collection"
  default     = true
}

# ============================================================================
# Node Pool Configuration
# ============================================================================
variable "machine_type" {
  type        = string
  description = "Primary node pool machine type"
  default     = "e2-standard-2"
}

variable "disk_size_gb" {
  type        = number
  description = "Primary node pool disk size"
  default     = 100
}

variable "disk_type" {
  type        = string
  description = "Primary node pool disk type"
  default     = "pd-balanced"
}

variable "min_nodes" {
  type        = number
  description = "Minimum number of nodes"
  default     = 3
}

variable "max_nodes" {
  type        = number
  description = "Maximum number of nodes"
  default     = 10
}

variable "enable_spot_nodes" {
  type        = bool
  description = "Enable spot/preemptible node pool"
  default     = false
}

variable "spot_machine_type" {
  type        = string
  description = "Spot node pool machine type"
  default     = "e2-standard-2"
}

variable "spot_disk_size_gb" {
  type        = number
  description = "Spot node pool disk size"
  default     = 100
}

variable "spot_min_nodes" {
  type        = number
  description = "Minimum spot nodes"
  default     = 0
}

variable "spot_max_nodes" {
  type        = number
  description = "Maximum spot nodes"
  default     = 5
}

# ============================================================================
# Helm Configuration
# ============================================================================
variable "deploy_helm_chart" {
  type        = bool
  description = "Deploy erlmcp Helm chart"
  default     = true
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

# ============================================================================
# Observability Configuration
# ============================================================================
variable "notification_channels" {
  type = object({
    email = object({
      enabled = bool
      address = string
    })
    pagerduty = object({
      enabled    = bool
      service_key = string
      auth_token  = string
    })
    slack = object({
      enabled     = bool
      channel_name = string
      auth_token   = string
    })
  })
  description = "Notification channel configurations"
  default = {
    email = {
      enabled = false
      address = ""
    }
    pagerduty = {
      enabled    = false
      service_key = ""
      auth_token  = ""
    }
    slack = {
      enabled     = false
      channel_name = ""
      auth_token   = ""
    }
  }
  sensitive = true
}

variable "create_uptime_check" {
  type        = bool
  description = "Create uptime check"
  default     = true
}

variable "uptime_check_host" {
  type        = string
  description = "Hostname for uptime check"
  default     = ""
}

variable "create_slos" {
  type        = bool
  description = "Create SLOs"
  default     = true
}

variable "enable_error_rate_alert" {
  type        = bool
  description = "Enable high error rate alert"
  default     = true
}

variable "enable_latency_alert" {
  type        = bool
  description = "Enable high latency alert"
  default     = true
}

variable "enable_memory_alert" {
  type        = bool
  description = "Enable high memory alert"
  default     = true
}

variable "enable_cpu_alert" {
  type        = bool
  description = "Enable high CPU alert"
  default     = true
}

variable "enable_health_check_alert" {
  type        = bool
  description = "Enable health check failure alert"
  default     = true
}

variable "enable_process_count_alert" {
  type        = bool
  description = "Enable low process count alert"
  default     = true
}

variable "log_export_storage_enabled" {
  type        = bool
  description = "Export logs to Cloud Storage"
  default     = false
}

variable "log_export_storage_destination" {
  type        = string
  description = "Cloud Storage bucket destination"
  default     = ""
}

# ============================================================================
# Labels
# ============================================================================
variable "labels" {
  type        = map(string)
  description = "Resource labels"
  default = {
    app        = "erlmcp"
    managed-by = "terraform"
  }
}
