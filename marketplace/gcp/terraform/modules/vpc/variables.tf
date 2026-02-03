# ============================================================================
# VPC Module Variables
# ============================================================================

variable "project_id" {
  type        = string
  description = "GCP project ID"
}

variable "region" {
  type        = string
  description = "Default region for resources"
  default     = "us-central1"
}

# ============================================================================
# Network Configuration
# ============================================================================
variable "create_network" {
  type        = bool
  description = "Create new VPC network (false = use existing)"
  default     = true
}

variable "network_name" {
  type        = string
  description = "VPC network name"
  default     = "erlmcp-vpc"
}

variable "auto_create_subnetworks" {
  type        = bool
  description = "Auto-create subnets (false = manual subnet creation)"
  default     = false
}

variable "routing_mode" {
  type        = string
  description = "Routing mode (REGIONAL or GLOBAL)"
  default     = "REGIONAL"

  validation {
    condition     = contains(["REGIONAL", "GLOBAL"], var.routing_mode)
    error_message = "Routing mode must be REGIONAL or GLOBAL."
  }
}

variable "delete_default_routes" {
  type        = bool
  description = "Delete default routes on creation"
  default     = false
}

variable "mtu" {
  type        = number
  description = "Maximum Transmission Unit"
  default     = 1460

  validation {
    condition     = contains([1460, 1500], var.mtu)
    error_message = "MTU must be 1460 or 1500."
  }
}

# ============================================================================
# Subnet Configuration
# ============================================================================
variable "create_subnets" {
  type        = bool
  description = "Create subnets"
  default     = true
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
    },
    {
      name                   = "erlmcp-subnet-us-east1"
      region                 = "us-east1"
      ip_cidr_range          = "10.10.0.0/24"
      private_ip_google_access = true
      purpose                = null
      role                   = null
      secondary_ip_ranges    = []
    }
  ]
}

variable "subnet_log_aggregation_interval" {
  type        = string
  description = "Subnet flow log aggregation interval"
  default     = "INTERVAL_5_SEC"
}

variable "subnet_log_flow_sampling" {
  type        = number
  description = "Subnet flow log sampling rate (0-1)"
  default     = 0.5

  validation {
    condition     = var.subnet_log_flow_sampling >= 0 && var.subnet_log_flow_sampling <= 1
    error_message = "Flow sampling must be between 0 and 1."
  }
}

variable "subnet_log_metadata" {
  type        = string
  description = "Include metadata in flow logs"
  default     = "INCLUDE_ALL_METADATA"
}

variable "subnet_log_metadata_fields" {
  type = list(object({
    field = string
  }))
  description = "Metadata fields for flow logs"
  default = [{ field = "all" }]
}

# ============================================================================
# Router and NAT Configuration
# ============================================================================
variable "create_router" {
  type        = bool
  description = "Create Cloud Router"
  default     = true
}

variable "routers" {
  type = list(object({
    name               = string
    region             = string
    asn                = number
    advertise_mode     = string
    keepalive_interval = number
  }))
  description = "Router configurations"
  default = [
    {
      name                = "erlmcp-cloud-router"
      region              = "us-central1"
      asn                 = 65001
      advertise_mode      = "DEFAULT"
      keepalive_interval  = 20
    }
  ]
}

variable "create_nat" {
  type        = bool
  description = "Create Cloud NAT"
  default     = true
}

variable "nats" {
  type = list(object({
    name                               = string
    region                             = string
    nat_ip_allocate_option             = string
    nat_ips                            = list(string)
    source_subnetwork_ip_ranges_to_nat = string
    subnetwork_name                    = string
    source_ip_ranges_to_nat            = list(string)
    secondary_ip_range_names           = list(string)
    enable_logging                     = bool
    log_filter                         = string
    min_ports_per_vm                   = number
    max_ports_per_vm                   = number
    udp_idle_timeout                   = number
    icmp_idle_timeout                  = number
    tcp_established_idle_timeout       = number
    tcp_transitory_idle_timeout        = number
  }))
  description = "NAT configurations"
  default = [
    {
      name                                = "erlmcp-cloud-nat"
      region                              = "us-central1"
      nat_ip_allocate_option              = "AUTO_ONLY"
      nat_ips                             = []
      source_subnetwork_ip_ranges_to_nat  = "ALL_SUBNETWORKS_ALL_IP_RANGES"
      subnetwork_name                     = "erlmcp-subnet-us-central1"
      source_ip_ranges_to_nat             = []
      secondary_ip_range_names            = []
      enable_logging                      = true
      log_filter                          = "ALL"
      min_ports_per_vm                    = 64
      max_ports_per_vm                    = 65536
      udp_idle_timeout                    = 30
      icmp_idle_timeout                   = 30
      tcp_established_idle_timeout        = 1200
      tcp_transitory_idle_timeout         = 30
    }
  ]
}

# ============================================================================
# Firewall Configuration
# ============================================================================
variable "create_firewall_rules" {
  type        = bool
  description = "Create firewall rules"
  default     = true
}

variable "firewall_priorities" {
  type = object({
    internal      = number
    ssh           = number
    health_check  = number
    https         = number
    master_access = number
    deny_all      = number
  })
  description = "Firewall rule priorities (lower = higher priority)"
  default = {
    internal      = 1000
    ssh           = 1001
    health_check  = 1002
    https         = 1003
    master_access = 1004
    deny_all      = 65534
  }
}

variable "internal_ranges" {
  type        = list(string)
  description = "Internal IP ranges for firewall rules"
  default = [
    "10.0.0.0/8",
    "172.16.0.0/12",
    "192.168.0.0/16"
  ]
}

variable "ssh_source_ranges" {
  type        = list(string)
  description = "Source ranges allowed for SSH"
  default     = ["0.0.0.0/0"]
}

variable "health_check_ranges" {
  type        = list(string)
  description = "IP ranges for Google health check probes"
  default = [
    "130.211.0.0/22",
    "35.191.0.0/16",
    "209.85.152.0/22",
    "209.85.204.0/22"
  ]
}

variable "master_access_ranges" {
  type        = list(string)
  description = "IP ranges allowed for GKE master access"
  default = [
    "10.0.0.0/8"
  ]
}

variable "allow_https_public" {
  type        = bool
  description = "Allow public HTTPS access"
  default     = false
}

variable "enable_private_endpoint" {
  type        = bool
  description = "Enable private endpoint for GKE master"
  default     = true
}

variable "enable_deny_all" {
  type        = bool
  description = "Create explicit deny-all firewall rule"
  default     = false
}
