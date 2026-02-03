# ============================================================================
# BEAMOps Infrastructure Variables
# ============================================================================

# ============================================================================
# General
# ============================================================================

variable "environment" {
  description = "Environment name (dev, staging, production)"
  type        = string
  validation {
    condition     = contains(["dev", "staging", "production"], var.environment)
    error_message = "Environment must be dev, staging, or production."
  }
}

variable "aws_region" {
  description = "AWS region for deployment"
  type        = string
  default     = "us-west-2"
}

variable "az_count" {
  description = "Number of availability zones to use"
  type        = number
  default     = 2
}

variable "deployment_mode" {
  description = "Deployment mode: 'ecs' for Fargate containers or 'ec2' for AMI-based instances"
  type        = string
  default     = "ecs"
  validation {
    condition     = contains(["ecs", "ec2"], var.deployment_mode)
    error_message = "Deployment mode must be 'ecs' or 'ec2'."
  }
}

# ============================================================================
# Networking
# ============================================================================

variable "vpc_cidr" {
  description = "CIDR block for VPC"
  type        = string
  default     = "10.0.0.0/16"
}

# ============================================================================
# Application
# ============================================================================

variable "app_port" {
  description = "Port the erlmcp application listens on"
  type        = number
  default     = 8080
}

variable "metrics_port" {
  description = "Port for Prometheus metrics endpoint"
  type        = number
  default     = 9090
}

variable "epmd_port" {
  description = "Port for Erlang Port Mapper Daemon"
  type        = number
  default     = 4369
}

variable "app_domain" {
  description = "Domain name for the application (e.g., app.example.com)"
  type        = string
  default     = ""
}

# ============================================================================
# ECS Configuration
# ============================================================================

variable "ecs_cpu" {
  description = "CPU units for ECS task (1024 = 1 vCPU)"
  type        = number
  default     = 512
}

variable "ecs_memory" {
  description = "Memory for ECS task in MB"
  type        = number
  default     = 1024
}

variable "ecs_desired_count" {
  description = "Desired number of ECS tasks"
  type        = number
  default     = 2
}

variable "ecs_min_count" {
  description = "Minimum number of ECS tasks for auto scaling"
  type        = number
  default     = 2
}

variable "ecs_max_count" {
  description = "Maximum number of ECS tasks for auto scaling"
  type        = number
  default     = 10
}

variable "ecr_repository_url" {
  description = "ECR repository URL for the erlmcp container image"
  type        = string
  default     = ""
}

variable "container_image_tag" {
  description = "Container image tag to deploy"
  type        = string
  default     = "latest"
}

# ============================================================================
# EC2 Configuration
# ============================================================================

variable "ami_id" {
  description = "AMI ID for EC2 instances (built with Packer)"
  type        = string
  default     = ""
}

variable "instance_type" {
  description = "EC2 instance type"
  type        = string
  default     = "t3.medium"
}

variable "ec2_desired_capacity" {
  description = "Desired number of EC2 instances"
  type        = number
  default     = 2
}

variable "ec2_min_size" {
  description = "Minimum number of EC2 instances"
  type        = number
  default     = 2
}

variable "ec2_max_size" {
  description = "Maximum number of EC2 instances"
  type        = number
  default     = 10
}

variable "key_name" {
  description = "SSH key pair name for EC2 instances"
  type        = string
  default     = ""
}

# ============================================================================
# SSL/TLS
# ============================================================================

variable "acm_certificate_arn" {
  description = "ARN of ACM certificate for HTTPS"
  type        = string
}

# ============================================================================
# DNS
# ============================================================================

variable "route53_zone_id" {
  description = "Route 53 hosted zone ID"
  type        = string
  default     = ""
}

# ============================================================================
# Logging & Monitoring
# ============================================================================

variable "log_retention_days" {
  description = "CloudWatch log retention in days"
  type        = number
  default     = 30
}

# ELB Account IDs for ALB access logs (per region)
# See: https://docs.aws.amazon.com/elasticloadbalancing/latest/application/enable-access-logging.html
variable "elb_account_id" {
  description = "AWS account ID for ELB access logs (region-specific)"
  type        = string
  default     = "797873946194" # us-west-2
}

# ============================================================================
# Secrets
# ============================================================================

variable "erlang_cookie" {
  description = "Erlang distribution cookie (stored in SSM)"
  type        = string
  sensitive   = true
  default     = ""
}

variable "db_password_ssm_arn" {
  description = "ARN of SSM parameter containing database password"
  type        = string
  default     = ""
}

# ============================================================================
# Auto Scaling
# ============================================================================

variable "cpu_target_value" {
  description = "Target CPU utilization percentage for auto scaling"
  type        = number
  default     = 50
}

variable "memory_target_value" {
  description = "Target memory utilization percentage for auto scaling"
  type        = number
  default     = 70
}
