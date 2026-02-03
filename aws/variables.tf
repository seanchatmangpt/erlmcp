# ==============================================================================
# ERLMCP AWS Infrastructure Variables
# ==============================================================================
# All configurable parameters for the AWS infrastructure.
# Override in terraform.{env}.tfvars files for different environments.
# ==============================================================================

# ==============================================================================
# General Configuration
# ==============================================================================

variable "app_name" {
  description = "Application name"
  type        = string
  default     = "erlmcp"
}

variable "app_version" {
  description = "Application version"
  type        = string
  default     = "latest"
}

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "production"

  validation {
    condition     = contains(["dev", "staging", "production"], var.environment)
    error_message = "Environment must be one of: dev, staging, production."
  }
}

variable "aws_region" {
  description = "Primary AWS region"
  type        = string
  default     = "us-east-1"
}

variable "dr_region" {
  description = "Disaster recovery AWS region"
  type        = string
  default     = "us-west-2"
}

variable "cost_center" {
  description = "Cost center for billing"
  type        = string
  default     = "engineering"
}

# ==============================================================================
# Networking
# ==============================================================================

variable "vpc_cidr" {
  description = "VPC CIDR block"
  type        = string
  default     = "10.0.0.0/16"
}

variable "enable_nat_gateway" {
  description = "Enable NAT Gateway for private subnet internet access"
  type        = bool
  default     = true
}

variable "single_nat_gateway" {
  description = "Use single NAT gateway (cost savings for non-production)"
  type        = bool
  default     = false
}

variable "enable_vpc_endpoints" {
  description = "Enable VPC endpoints for AWS services"
  type        = bool
  default     = true
}

variable "enable_flow_logs" {
  description = "Enable VPC flow logs"
  type        = bool
  default     = true
}

# ==============================================================================
# ECS Configuration
# ==============================================================================

variable "container_image" {
  description = "Container image (leave empty to use ECR)"
  type        = string
  default     = ""
}

variable "app_port" {
  description = "Application port"
  type        = number
  default     = 8080
}

variable "metrics_port" {
  description = "Metrics port for Prometheus"
  type        = number
  default     = 9090
}

variable "desired_count" {
  description = "Desired number of ECS tasks"
  type        = number
  default     = 2
}

variable "min_capacity" {
  description = "Minimum number of ECS tasks"
  type        = number
  default     = 1
}

variable "max_capacity" {
  description = "Maximum number of ECS tasks"
  type        = number
  default     = 10
}

variable "task_cpu" {
  description = "Task CPU units (1024 = 1 vCPU)"
  type        = number
  default     = 1024
}

variable "task_memory" {
  description = "Task memory in MB"
  type        = number
  default     = 2048
}

variable "cpu_architecture" {
  description = "CPU architecture (X86_64 or ARM64)"
  type        = string
  default     = "X86_64"

  validation {
    condition     = contains(["X86_64", "ARM64"], var.cpu_architecture)
    error_message = "CPU architecture must be X86_64 or ARM64."
  }
}

variable "ephemeral_storage_size" {
  description = "Ephemeral storage size in GiB"
  type        = number
  default     = 21
}

variable "health_check_grace_period" {
  description = "Health check grace period in seconds"
  type        = number
  default     = 60
}

variable "deployment_controller_type" {
  description = "Deployment controller type (ECS or CODE_DEPLOY)"
  type        = string
  default     = "ECS"
}

variable "enable_execute_command" {
  description = "Enable ECS Exec for debugging"
  type        = bool
  default     = true
}

variable "enable_container_insights" {
  description = "Enable Container Insights"
  type        = bool
  default     = true
}

variable "use_spot_instances" {
  description = "Use Fargate Spot for cost savings"
  type        = bool
  default     = false
}

variable "enable_service_discovery" {
  description = "Enable service discovery for Erlang clustering"
  type        = bool
  default     = true
}

# ==============================================================================
# Auto Scaling
# ==============================================================================

variable "cpu_scaling_target" {
  description = "Target CPU utilization for scaling"
  type        = number
  default     = 70
}

variable "memory_scaling_target" {
  description = "Target memory utilization for scaling"
  type        = number
  default     = 80
}

variable "requests_per_target" {
  description = "Target requests per task for scaling"
  type        = number
  default     = 1000
}

# ==============================================================================
# Load Balancer
# ==============================================================================

variable "acm_certificate_arn" {
  description = "ACM certificate ARN for HTTPS"
  type        = string
}

variable "alb_access_logs_bucket" {
  description = "S3 bucket for ALB access logs (leave empty to disable)"
  type        = string
  default     = ""
}

# ==============================================================================
# RDS Configuration
# ==============================================================================

variable "enable_rds" {
  description = "Enable RDS PostgreSQL"
  type        = bool
  default     = true
}

variable "db_engine_version" {
  description = "PostgreSQL version"
  type        = string
  default     = "16.2"
}

variable "db_instance_class" {
  description = "RDS instance class"
  type        = string
  default     = "db.t3.medium"
}

variable "db_allocated_storage" {
  description = "Initial storage in GB"
  type        = number
  default     = 20
}

variable "db_max_allocated_storage" {
  description = "Maximum storage for autoscaling in GB"
  type        = number
  default     = 100
}

variable "db_iops" {
  description = "Provisioned IOPS (0 for gp3 default)"
  type        = number
  default     = 0
}

variable "db_storage_throughput" {
  description = "Storage throughput in MiB/s (0 for gp3 default)"
  type        = number
  default     = 0
}

variable "db_name" {
  description = "Database name"
  type        = string
  default     = "erlmcp"
}

variable "db_username" {
  description = "Database username"
  type        = string
  default     = "erlmcp"
}

variable "db_password" {
  description = "Database password (leave empty to auto-generate)"
  type        = string
  sensitive   = true
  default     = ""
}

variable "db_multi_az" {
  description = "Enable Multi-AZ deployment"
  type        = bool
  default     = true
}

variable "db_backup_retention_period" {
  description = "Backup retention period in days"
  type        = number
  default     = 7
}

variable "db_backup_window" {
  description = "Backup window"
  type        = string
  default     = "03:00-04:00"
}

variable "db_maintenance_window" {
  description = "Maintenance window"
  type        = string
  default     = "Mon:04:00-Mon:05:00"
}

variable "db_max_connections" {
  description = "Maximum database connections"
  type        = number
  default     = 200
}

variable "db_monitoring_interval" {
  description = "Enhanced monitoring interval (0 to disable)"
  type        = number
  default     = 60
}

variable "db_read_replica_count" {
  description = "Number of read replicas"
  type        = number
  default     = 0
}

variable "db_replica_instance_class" {
  description = "Read replica instance class (empty = same as primary)"
  type        = string
  default     = ""
}

variable "enable_rds_proxy" {
  description = "Enable RDS Proxy for connection pooling"
  type        = bool
  default     = false
}

variable "bastion_security_group_id" {
  description = "Security group ID for bastion access to RDS"
  type        = string
  default     = ""
}

# ==============================================================================
# Secrets
# ==============================================================================

variable "erlang_cookie" {
  description = "Erlang distribution cookie (leave empty to auto-generate)"
  type        = string
  sensitive   = true
  default     = ""
}

variable "database_url" {
  description = "Database URL (if not using RDS)"
  type        = string
  sensitive   = true
  default     = ""
}

variable "mcp_api_key" {
  description = "MCP API key"
  type        = string
  sensitive   = true
  default     = ""
}

variable "webhook_secret" {
  description = "Webhook secret"
  type        = string
  sensitive   = true
  default     = ""
}

variable "enable_secret_rotation" {
  description = "Enable automatic secret rotation"
  type        = bool
  default     = false
}

variable "enable_multi_region_kms" {
  description = "Enable multi-region KMS key"
  type        = bool
  default     = false
}

# ==============================================================================
# Monitoring
# ==============================================================================

variable "log_retention_days" {
  description = "CloudWatch log retention in days"
  type        = number
  default     = 30
}

variable "log_level" {
  description = "Application log level"
  type        = string
  default     = "info"
}

variable "alert_email_addresses" {
  description = "Email addresses for alerts"
  type        = list(string)
  default     = []
}

variable "slack_webhook_url" {
  description = "Slack webhook URL for alerts"
  type        = string
  sensitive   = true
  default     = ""
}

variable "slack_channel" {
  description = "Slack channel for alerts"
  type        = string
  default     = "#erlmcp-alerts"
}

variable "enable_xray" {
  description = "Enable X-Ray tracing"
  type        = bool
  default     = true
}
