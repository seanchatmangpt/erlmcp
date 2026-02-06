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
  description = <<-EOT
    Machine type for the instance. Latest options (2026):
    - E2 (cost-optimized): e2-micro, e2-small, e2-medium, e2-standard-2/4/8/16/32
    - N2 (balanced): n2-standard-2/4/8/16/32/48/64/80/96/128, n2-highmem-*, n2-highcpu-*
    - N2D (AMD balanced): n2d-standard-2/4/8/16/32/48/64/80/96/128/224
    - C2 (compute-optimized Intel): c2-standard-4/8/16/30/60
    - C2D (compute-optimized AMD): c2d-standard-2/4/8/16/32/56/112
    - C3 (4th gen Intel): c3-standard-4/8/22/44/88/176, c3-highcpu-*, c3-highmem-*
    - T2D (cost-optimized AMD): t2d-standard-1/2/4/8/16/32/48/60
    - T2A (Arm-based): t2a-standard-1/2/4/8/16/32/48
    - M3 (memory-optimized): m3-megamem-64/128, m3-ultramem-32/64/128
  EOT
  default     = "n2-standard-4"
}

variable "source_image" {
  type        = string
  description = <<-EOT
    Source image for boot disk. Options:
    - Ubuntu: projects/ubuntu-os-cloud/global/images/family/ubuntu-2404-lts-amd64 (latest LTS)
    - Debian: projects/debian-cloud/global/images/family/debian-12
    - Rocky Linux: projects/rocky-linux-cloud/global/images/family/rocky-linux-9
    - Container-Optimized OS: projects/cos-cloud/global/images/family/cos-stable
    - Custom: Your custom erlmcp image
  EOT
  default     = "projects/ubuntu-os-cloud/global/images/family/ubuntu-2404-lts-amd64"
}

variable "disk_type" {
  type        = string
  description = <<-EOT
    Disk type for boot disk. Options (2026):
    - pd-standard: Standard persistent disk (HDD)
    - pd-balanced: Balanced persistent disk (SSD) - good price/performance
    - pd-ssd: SSD persistent disk - high performance
    - pd-extreme: Extreme persistent disk - highest IOPS (requires special machine types)
    - hyperdisk-balanced: Hyperdisk Balanced - next-gen balanced performance
    - hyperdisk-throughput: Hyperdisk Throughput - optimized for high throughput
    - hyperdisk-extreme: Hyperdisk Extreme - highest performance and flexibility
  EOT
  default     = "hyperdisk-balanced"

  validation {
    condition = contains([
      "pd-standard", "pd-balanced", "pd-ssd", "pd-extreme",
      "hyperdisk-balanced", "hyperdisk-throughput", "hyperdisk-extreme"
    ], var.disk_type)
    error_message = "Disk type must be one of: pd-standard, pd-balanced, pd-ssd, pd-extreme, hyperdisk-balanced, hyperdisk-throughput, hyperdisk-extreme."
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

variable "replacement_method" {
  type        = string
  description = "Replacement method for instance group updates (RECREATE, SUBSTITUTE)"
  default     = "SUBSTITUTE"

  validation {
    condition     = contains(["RECREATE", "SUBSTITUTE"], var.replacement_method)
    error_message = "Replacement method must be RECREATE or SUBSTITUTE."
  }
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
  description = "Minimum seconds before instance is ready"
  default     = 60
}

# ============================================================================
# Advanced Compute Configuration (2026)
# ============================================================================
variable "min_cpu_platform" {
  type        = string
  description = <<-EOT
    Minimum CPU platform. Options:
    - Intel Cascade Lake
    - Intel Ice Lake
    - Intel Sapphire Rapids (4th Gen)
    - AMD Milan
    - AMD Genoa (4th Gen)
    - Ampere Altra (Arm)
  EOT
  default     = ""
}

variable "enable_display" {
  type        = bool
  description = "Enable virtual display (for GPU workloads)"
  default     = false
}

variable "guest_accelerators" {
  type = list(object({
    type  = string
    count = number
  }))
  description = <<-EOT
    GPU accelerators to attach. Types:
    - nvidia-tesla-t4: T4 GPUs (general purpose)
    - nvidia-tesla-v100: V100 GPUs (training/inference)
    - nvidia-tesla-a100: A100 GPUs (latest, highest performance)
    - nvidia-l4: L4 GPUs (cost-effective inference)
    - nvidia-a100-80gb: A100 80GB (large models)
  EOT
  default     = []
}

variable "local_ssds" {
  type = list(object({
    device_name = string
    interface   = string
  }))
  description = <<-EOT
    Local SSD disks to attach. Interface options:
    - NVME: NVMe interface (higher performance)
    - SCSI: SCSI interface
    Each local SSD is 375 GB. Max 24 per instance.
  EOT
  default     = []
}

variable "advanced_machine_features" {
  type = object({
    enable_nested_virtualization = bool
    threads_per_core             = number
    visible_core_count           = number
  })
  description = <<-EOT
    Advanced machine features:
    - enable_nested_virtualization: Enable nested virtualization
    - threads_per_core: Threads per physical core (1 or 2)
    - visible_core_count: Number of physical cores to expose
  EOT
  default = {
    enable_nested_virtualization = false
    threads_per_core             = 2
    visible_core_count           = 0
  }
}

variable "reservation_affinity" {
  type = object({
    type = string
    specific_reservations = list(object({
      key    = string
      values = list(string)
    }))
  })
  description = <<-EOT
    Reservation affinity configuration:
    - type: ANY_RESERVATION, SPECIFIC_RESERVATION, NO_RESERVATION
    - specific_reservations: List of specific reservations
  EOT
  default = {
    type                  = "ANY_RESERVATION"
    specific_reservations = []
  }
}

# ============================================================================
# Enhanced Security Configuration (2026)
# ============================================================================
variable "enable_os_login" {
  type        = bool
  description = "Enable OS Login for SSH key management via IAM"
  default     = true
}

variable "block_project_ssh_keys" {
  type        = bool
  description = "Block project-wide SSH keys (security best practice)"
  default     = true
}

variable "enable_confidential_compute_sev_snp" {
  type        = bool
  description = "Enable Confidential VM with AMD SEV-SNP (latest generation)"
  default     = false
}

variable "can_ip_forward" {
  type        = bool
  description = "Allow instance to forward IP packets"
  default     = false
}

variable "network_performance_config" {
  type = object({
    total_egress_bandwidth_tier = string
  })
  description = <<-EOT
    Network performance configuration:
    - total_egress_bandwidth_tier: TIER_1 (highest), DEFAULT
  EOT
  default = {
    total_egress_bandwidth_tier = "DEFAULT"
  }
}

# ============================================================================
# Snapshot and Backup Configuration
# ============================================================================
variable "enable_snapshot_schedule" {
  type        = bool
  description = "Create and attach snapshot schedule policy"
  default     = false
}

variable "snapshot_schedule_name" {
  type        = string
  description = "Name for snapshot schedule policy"
  default     = "daily-snapshot"
}

variable "snapshot_retention_days" {
  type        = number
  description = "Days to retain snapshots"
  default     = 14

  validation {
    condition     = var.snapshot_retention_days >= 1 && var.snapshot_retention_days <= 65535
    error_message = "Snapshot retention must be between 1 and 65535 days."
  }
}

variable "snapshot_start_time" {
  type        = string
  description = "Snapshot start time in UTC (HH:MM format)"
  default     = "03:00"

  validation {
    condition     = can(regex("^([0-1][0-9]|2[0-3]):[0-5][0-9]$", var.snapshot_start_time))
    error_message = "Snapshot start time must be in HH:MM format."
  }
}

# ============================================================================
# Instance Schedule Configuration
# ============================================================================
variable "instance_schedule" {
  type = object({
    enabled      = bool
    start_time   = string
    stop_time    = string
    timezone     = string
  })
  description = <<-EOT
    Instance schedule for cost optimization:
    - enabled: Enable instance scheduling
    - start_time: Time to start instances (HH:MM)
    - stop_time: Time to stop instances (HH:MM)
    - timezone: Timezone (e.g., America/New_York, UTC)
  EOT
  default = {
    enabled    = false
    start_time = "09:00"
    stop_time  = "18:00"
    timezone   = "UTC"
  }
}

variable "static_nat_ip_address" {
  type        = string
  description = "Static NAT IP address (leave empty for ephemeral)"
  default     = ""
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
