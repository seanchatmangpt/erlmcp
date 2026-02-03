# ============================================================================
# Compute Engine Deployment Variables
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

variable "zone" {
  type        = string
  description = "GCP zone"
  default     = "us-central1-a"
}

variable "instance_name" {
  type        = string
  description = "Base name for VM instances"
  default     = "erlmcp-server"
}

variable "instance_count" {
  type        = number
  description = "Number of VM instances"
  default     = 1
}

variable "machine_type" {
  type        = string
  description = "Machine type"
  default     = "e2-medium"
}

variable "source_image" {
  type        = string
  description = "Source image (custom erlmcp image or Ubuntu)"
  default     = "projects/ubuntu-os-cloud/global/images/family/ubuntu-2204-lts"
}

variable "disk_type" {
  type        = string
  description = "Disk type"
  default     = "pd-balanced"
}

variable "disk_size_gb" {
  type        = number
  description = "Disk size in GB"
  default     = 20
}

# ============================================================================
# Network Configuration
# ============================================================================
variable "network_name" {
  type        = string
  description = "VPC network name"
  default     = "erlmcp-vpc"
}

variable "subnet_cidr" {
  type        = string
  description = "Subnet CIDR"
  default     = "10.0.0.0/24"
}

variable "public_ip" {
  type        = bool
  description = "Assign public IP to instances"
  default     = true
}

variable "create_firewall_rules" {
  type        = bool
  description = "Create firewall rules"
  default     = true
}

variable "http_source_ranges" {
  type        = list(string)
  description = "Source ranges allowed for HTTP access"
  default     = ["0.0.0.0/0"]
}

variable "create_load_balancer" {
  type        = bool
  description = "Create regional load balancer"
  default     = false
}

# ============================================================================
# Security Configuration
# ============================================================================
variable "enable_secure_boot" {
  type        = bool
  description = "Enable secure boot (Shielded VM)"
  default     = true
}

variable "enable_integrity_monitoring" {
  type        = bool
  description = "Enable integrity monitoring"
  default     = true
}

# ============================================================================
# Health Check Configuration
# ============================================================================
variable "health_check_path" {
  type        = string
  description = "Health check endpoint path"
  default     = "/health"
}

# ============================================================================
# Environment Configuration
# ============================================================================
variable "environment" {
  type        = string
  description = "Application environment"
  default     = "production"
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
