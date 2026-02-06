# ============================================================================
# Variables for erlmcp v3 Network Infrastructure
# ============================================================================

# ============================================================================
# Basic Configuration
# ============================================================================
variable "region" {
  description = "AWS region for deployment"
  type        = string
  default     = "us-east-1"

  validation {
    condition     = can(regex("^[a-z]{2}-[a-z]+-[0-9]$", var.region))
    error_message = "Region must be a valid AWS region format (e.g., us-east-1)."
  }
}

variable "environment" {
  description = "Environment name (dev, staging, production)"
  type        = string
  default     = "production"

  validation {
    condition     = contains(["dev", "staging", "production"], var.environment)
    error_message = "Environment must be dev, staging, or production."
  }
}

variable "vpc_name_prefix" {
  description = "Prefix for VPC name (will be suffixed with random pet name)"
  type        = string
  default     = "erlmcp-vpc"
}

variable "tags" {
  description = "Tags to apply to all resources"
  type        = map(string)
  default = {
    Project     = "erlmcp"
    Environment = "production"
    ManagedBy   = "Terraform"
    Repository  = "erlmcp/v3"
  }
}

# ============================================================================
# VPC Configuration
# ============================================================================
variable "vpc_cidr" {
  description = "CIDR block for VPC"
  type        = string
  default     = "10.0.0.0/16"

  validation {
    condition     = can(cidrhost(var.vpc_cidr, 0))
    error_message = "VPC CIDR must be a valid IPv4 CIDR block."
  }
}

variable "azs" {
  description = "List of availability zones to use (null = auto-select first 3)"
  type        = list(string)
  default     = null
}

# ============================================================================
# Subnet Configuration
# ============================================================================
variable "private_subnet_cidrs" {
  description = "List of private subnet CIDRs"
  type        = list(string)
  default     = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]

  validation {
    condition     = length(var.private_subnet_cidrs) >= 2
    error_message = "At least 2 private subnets are required for high availability."
  }
}

variable "public_subnet_cidrs" {
  description = "List of public subnet CIDRs"
  type        = list(string)
  default     = ["10.0.101.0/24", "10.0.102.0/24", "10.0.103.0/24"]

  validation {
    condition     = length(var.public_subnet_cidrs) >= 2
    error_message = "At least 2 public subnets are required for high availability."
  }
}

variable "database_subnet_cidrs" {
  description = "List of database subnet CIDRs"
  type        = list(string)
  default     = ["10.0.11.0/24", "10.0.12.0/24", "10.0.13.0/24"]

  validation {
    condition     = length(var.database_subnet_cidrs) >= 2
    error_message = "At least 2 database subnets are required for RDS high availability."
  }
}

# ============================================================================
# NAT Gateway Configuration
# ============================================================================
variable "enable_nat_gateway" {
  description = "Enable NAT gateway for private subnet internet access"
  type        = bool
  default     = true
}

variable "single_nat_gateway" {
  description = "Use a single NAT gateway for all private subnets (cost savings, reduces HA)"
  type        = bool
  default     = false
}

variable "one_nat_gateway_per_az" {
  description = "Create one NAT gateway per availability zone (high availability)"
  type        = bool
  default     = true
}

# ============================================================================
# DNS Configuration
# ============================================================================
variable "enable_dns_support" {
  description = "Enable DNS support in the VPC"
  type        = bool
  default     = true
}

variable "enable_dns_hostnames" {
  description = "Enable DNS hostnames in the VPC"
  type        = bool
  default     = true
}

# ============================================================================
# VPC Flow Logs Configuration
# ============================================================================
variable "enable_flow_logs" {
  description = "Enable VPC Flow Logs to CloudWatch"
  type        = bool
  default     = true
}

variable "flow_log_retention_days" {
  description = "Number of days to retain VPC flow logs in CloudWatch"
  type        = number
  default     = 30

  validation {
    condition     = contains([1, 3, 5, 7, 14, 30, 60, 90, 120, 150, 180, 365, 400, 545, 731, 1827, 3653], var.flow_log_retention_days)
    error_message = "Flow log retention must be a valid CloudWatch Logs retention period."
  }
}

variable "flow_log_aggregation_interval" {
  description = "Flow log aggregation interval in seconds (60 or 600)"
  type        = number
  default     = 60

  validation {
    condition     = contains([60, 600], var.flow_log_aggregation_interval)
    error_message = "Aggregation interval must be 60 or 600 seconds."
  }
}

variable "flow_log_traffic_type" {
  description = "Type of traffic to log (ACCEPT, REJECT, or ALL)"
  type        = string
  default     = "ALL"

  validation {
    condition     = contains(["ACCEPT", "REJECT", "ALL"], var.flow_log_traffic_type)
    error_message = "Traffic type must be ACCEPT, REJECT, or ALL."
  }
}

variable "flow_log_kms_key_id" {
  description = "KMS key ID for encrypting flow logs in CloudWatch (null = default encryption)"
  type        = string
  default     = null
}

variable "enable_flow_logs_to_s3" {
  description = "Enable VPC Flow Logs to S3 (alternative to CloudWatch)"
  type        = bool
  default     = false
}

# ============================================================================
# VPN Gateway Configuration
# ============================================================================
variable "enable_vpn_gateway" {
  description = "Enable VPN gateway for hybrid connectivity"
  type        = bool
  default     = false
}

# ============================================================================
# VPC Endpoints Configuration
# ============================================================================
variable "enable_s3_endpoint" {
  description = "Enable S3 VPC gateway endpoint (free)"
  type        = bool
  default     = true
}

variable "enable_dynamodb_endpoint" {
  description = "Enable DynamoDB VPC gateway endpoint (free)"
  type        = bool
  default     = true
}

variable "enable_interface_endpoints" {
  description = "Enable interface VPC endpoints for AWS services (costs apply)"
  type        = bool
  default     = true
}

# ============================================================================
# Security Group Configuration
# ============================================================================
variable "create_alb_sg" {
  description = "Create security group for Application Load Balancer"
  type        = bool
  default     = true
}

variable "create_app_sg" {
  description = "Create security group for application tier"
  type        = bool
  default     = true
}

variable "create_db_sg" {
  description = "Create security group for database tier"
  type        = bool
  default     = true
}

variable "create_mgmt_sg" {
  description = "Create security group for management/bastion tier"
  type        = bool
  default     = true
}

variable "alb_ingress_cidrs" {
  description = "CIDR blocks allowed to access ALB (use specific IPs in production)"
  type        = list(string)
  default     = ["0.0.0.0/0"]
}

variable "ssh_allowed_cidrs" {
  description = "CIDR blocks allowed for SSH access (prefer Systems Manager Session Manager)"
  type        = list(string)
  default     = []
}

# ============================================================================
# Network ACLs Configuration
# ============================================================================
variable "create_network_acls" {
  description = "Create custom network ACLs for defense in depth"
  type        = bool
  default     = true
}

# ============================================================================
# VPC Peering Configuration
# ============================================================================
variable "enable_vpc_peering" {
  description = "Enable VPC peering connection"
  type        = bool
  default     = false
}

variable "peer_vpc_id" {
  description = "VPC ID to peer with"
  type        = string
  default     = ""
}

variable "peer_vpc_region" {
  description = "Region of peer VPC (null = same region)"
  type        = string
  default     = null
}

variable "peer_vpc_auto_accept" {
  description = "Automatically accept VPC peering connection"
  type        = bool
  default     = false
}

# ============================================================================
# Transit Gateway Configuration
# ============================================================================
variable "enable_transit_gateway" {
  description = "Enable Transit Gateway for multi-VPC connectivity"
  type        = bool
  default     = false
}