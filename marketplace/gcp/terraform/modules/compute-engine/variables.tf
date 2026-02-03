# ============================================================================
# Compute Engine Module Variables
# ============================================================================

variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "zone" {
  type        = string
  description = "GCP zone for instance(s)"
  default     = "us-central1-a"
}

variable "region" {
  type        = string
  description = "GCP region (for regional resources)"
  default     = "us-central1"
}

variable "instance_name" {
  type        = string
  description = "Base name for instance(s)"
  default     = "erlmcp-server"
}

variable "instance_count" {
  type        = number
  description = "Number of instances to create"
  default     = 1

  validation {
    condition     = var.instance_count >= 0
    error_message = "Instance count must be >= 0."
  }
}

# ============================================================================
# Machine Configuration
# ============================================================================
variable "machine_type" {
  type        = string
  description = "Machine type (e.g., e2-medium, e2-standard-2, n2-standard-4)"
  default     = "e2-medium"
}

variable "source_image" {
  type        = string
  description = "Source image for boot disk (custom erlmcp image or Ubuntu)"
  default     = "projects/ubuntu-os-cloud/global/images/family/ubuntu-2204-lts"
}

variable "disk_type" {
  type        = string
  description = "Disk type (pd-standard, pd-balanced, pd-ssd)"
  default     = "pd-balanced"

  validation {
    condition     = contains(["pd-standard", "pd-balanced", "pd-ssd", "pd-extreme"], var.disk_type)
    error_message = "Disk type must be pd-standard, pd-balanced, pd-ssd, or pd-extreme."
  }
}

variable "disk_size_gb" {
  type        = number
  description = "Boot disk size in GB"
  default     = 20

  validation {
    condition     = var.disk_size_gb >= 10
    error_message = "Disk size must be >= 10 GB."
  }
}

variable "disk_labels" {
  type        = map(string)
  description = "Labels to apply to boot disk"
  default = {
    disk-type = "boot"
  }
}

variable "auto_delete_disk" {
  type        = bool
  description = "Auto-delete boot disk when instance is deleted"
  default     = true
}

variable "additional_disks" {
  type = list(object({
    source      = string
    device_name = string
    mode        = string
  }))
  description = "Additional disks to attach"
  default     = []
}

# ============================================================================
# Network Configuration
# ============================================================================
variable "network" {
  type        = string
  description = "VPC network name"
  default     = "default"
}

variable "subnetwork" {
  type        = string
  description = "Subnetwork name (leave empty for default)"
  default     = ""
}

variable "enable_ipv6" {
  type        = bool
  description = "Enable IPv6 on the instance"
  default     = false
}

variable "static_nat_ip" {
  type        = string
  description = "Static NAT IP address (leave empty for ephemeral)"
  default     = ""
}

variable "network_tier" {
  type        = string
  description = "Network tier (PREMIUM or STANDARD)"
  default     = "PREMIUM"

  validation {
    condition     = contains(["PREMIUM", "STANDARD"], var.network_tier)
    error_message = "Network tier must be PREMIUM or STANDARD."
  }
}

variable "tags" {
  type        = list(string)
  description = "Network tags for the instance"
  default     = []
}

# ============================================================================
# Security Configuration
# ============================================================================
variable "enable_secure_boot" {
  type        = bool
  description = "Enable secure boot (Shielded VM)"
  default     = true
}

variable "enable_vtpm" {
  type        = bool
  description = "Enable vTPM (Shielded VM)"
  default     = true
}

variable "enable_integrity_monitoring" {
  type        = bool
  description = "Enable integrity monitoring (Shielded VM)"
  default     = true
}

variable "enable_confidential_compute" {
  type        = bool
  description = "Enable confidential computing"
  default     = false
}

# ============================================================================
# Service Account
# ============================================================================
variable "create_service_account" {
  type        = bool
  description = "Create a dedicated service account"
  default     = true
}

variable "instance_scopes" {
  type = list(string)
  description = "OAuth scopes for the service account"
  default = [
    "https://www.googleapis.com/auth/cloud-platform",
  ]
}

# ============================================================================
# Metadata Configuration
# ============================================================================
variable "metadata" {
  type        = map(string)
  description = "Instance metadata"
  default = {
    environment = "production"
  }
}

variable "ssh_keys" {
  type        = string
  description = "SSH keys in format 'user:ssh-public-key'"
  default     = ""
  sensitive   = true
}

variable "startup_script" {
  type        = string
  description = "Startup script content"
  default     = ""
}

variable "startup_script_url" {
  type        = string
  description = "GCS URL for startup script"
  default     = ""
}

variable "shutdown_script" {
  type        = string
  description = "Shutdown script content"
  default     = ""
}

variable "metadata_startup_script" {
  type        = string
  description = "Startup script via metadata_startup_script (takes precedence)"
  default     = ""
}

# ============================================================================
# Scheduling Configuration
# ============================================================================
variable "preemptible" {
  type        = bool
  description = "Create preemptible instance"
  default     = false
}

variable "on_host_maintenance" {
  type        = string
  description = "Behavior on host maintenance (MIGRATE, TERMINATE)"
  default     = "MIGRATE"

  validation {
    condition     = contains(["MIGRATE", "TERMINATE"], var.on_host_maintenance)
    error_message = "On host maintenance must be MIGRATE or TERMINATE."
  }
}

variable "automatic_restart" {
  type        = bool
  description = "Automatically restart instance on failure"
  default     = true
}

variable "min_node_cpus" {
  type        = number
  description = "Minimum number of CPUs"
  default     = 0
}

variable "provisioning_model" {
  type        = string
  description = "Provisioning model (STANDARD, SPOT)"
  default     = "STANDARD"

  validation {
    condition     = contains(["STANDARD", "SPOT"], var.provisioning_model)
    error_message = "Provisioning model must be STANDARD or SPOT."
  }
}

variable "node_affinities" {
  type = list(object({
    key      = string
    operator = string
    values   = list(string)
  }))
  description = "Node affinities for sole-tenant nodes"
  default     = []
}

# ============================================================================
# Instance Group Configuration
# ============================================================================
variable "create_instance_template" {
  type        = bool
  description = "Create instance template for managed instance groups"
  default     = false
}

variable "create_instance_group" {
  type        = bool
  description = "Create managed instance group"
  default     = false
}

variable "target_size" {
  type        = number
  description = "Target size for managed instance group"
  default     = 1
}

variable "enable_auto_healing" {
  type        = bool
  description = "Enable auto-healing for managed instance group"
  default     = true
}

variable "auto_healing_initial_delay" {
  type        = number
  description = "Initial delay for auto-healing"
  default     = 300
}

variable "enable_autoscaling" {
  type        = bool
  description = "Enable autoscaling for managed instance group"
  default     = true
}

variable "min_replicas" {
  type        = number
  description = "Minimum replicas for autoscaling"
  default     = 1
}

variable "max_replicas" {
  type        = number
  description = "Maximum replicas for autoscaling"
  default     = 10
}

variable "cooldown_period" {
  type        = number
  description = "Cooldown period for autoscaling"
  default     = 60
}

variable "scale_based_on_cpu" {
  type        = bool
  description = "Scale based on CPU utilization"
  default     = true
}

variable "cpu_utilization_target" {
  type        = number
  description = "Target CPU utilization for autoscaling"
  default     = 0.8
}

variable "scale_based_on_lb" {
  type        = bool
  description = "Scale based on load balancing utilization"
  default     = false
}

variable "lb_utilization_target" {
  type        = number
  description = "Target LB utilization for autoscaling"
  default     = 0.8
}

variable "update_type" {
  type        = string
  description = "Instance group update type"
  default     = "OPPORTUNISTIC"
}

variable "minimal_action" {
  type        = string
  description = "Minimal action for updates"
  default     = "REFRESH"
}

variable "most_disruptive_action" {
  type        = string
  description = "Most disruptive action for updates"
  default     = "REPLACE"
}

variable "max_surge" {
  type        = number
  description = "Max surge for updates"
  default     = 1
}

variable "max_unavailable" {
  type        = number
  description = "Max unavailable for updates"
  default     = 0
}

variable "min_ready_sec" {
  type        = number
  description = "Minimum ready seconds"
  default     = 60
}

# ============================================================================
# Health Check Configuration
# ============================================================================
variable "create_health_check" {
  type        = bool
  description = "Create health check"
  default     = true
}

variable "check_interval" {
  type        = number
  description = "Health check interval in seconds"
  default     = 10
}

variable "timeout" {
  type        = number
  description = "Health check timeout in seconds"
  default     = 5
}

variable "healthy_threshold" {
  type        = number
  description = "Healthy threshold"
  default     = 2
}

variable "unhealthy_threshold" {
  type        = number
  description = "Unhealthy threshold"
  default     = 3
}

variable "health_check_port" {
  type        = number
  description = "Health check port"
  default     = 8080
}

variable "health_check_path" {
  type        = string
  description = "Health check path"
  default     = "/health"
}

variable "enable_health_check_logging" {
  type        = bool
  description = "Enable health check logging"
  default     = true
}

# ============================================================================
# Firewall Configuration
# ============================================================================
variable "create_firewall_rules" {
  type        = bool
  description = "Create firewall rules"
  default     = true
}

variable "http_source_ranges" {
  type        = list(string)
  description = "Source ranges for HTTP access"
  default     = ["0.0.0.0/0"]
}

variable "metrics_source_ranges" {
  type        = list(string)
  description = "Source ranges for metrics access (restrictive)"
  default     = ["10.0.0.0/8"]
}

variable "distribution_source_ranges" {
  type        = list(string)
  description = "Source ranges for Erlang distribution"
  default     = ["10.0.0.0/8"]
}

# ============================================================================
# Other Configuration
# ============================================================================
variable "labels" {
  type        = map(string)
  description = "Resource labels"
  default = {
    app = "erlmcp"
  }
}

variable "deletion_protection" {
  type        = bool
  description = "Enable deletion protection"
  default     = false
}

variable "allow_stopping_for_update" {
  type        = bool
  description = "Allow stopping instance for updates"
  default     = true
}

variable "create_before_destroy" {
  type        = bool
  description = "Create new instance before destroying old one"
  default     = false
}

variable "resource_policy_urls" {
  type        = list(string)
  description = "Resource policy URLs (e.g., for snapshot schedules)"
  default     = []
}

variable "create_static_ip" {
  type        = bool
  description = "Create static IP address"
  default     = false
}

variable "ip_address_type" {
  type        = string
  description = "IP address type"
  default     = "STATIC"
}

variable "log_level" {
  type        = string
  description = "Application log level"
  default     = "info"
}
