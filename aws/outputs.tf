# ==============================================================================
# ERLMCP AWS Infrastructure Outputs
# ==============================================================================
# All outputs from the infrastructure provisioning.
# Use these values for configuration of applications and CI/CD pipelines.
# ==============================================================================

# ==============================================================================
# General
# ==============================================================================

output "account_id" {
  description = "AWS Account ID"
  value       = local.account_id
}

output "region" {
  description = "AWS Region"
  value       = local.region
}

output "environment" {
  description = "Environment name"
  value       = var.environment
}

output "name_prefix" {
  description = "Resource name prefix"
  value       = local.name_prefix
}

# ==============================================================================
# VPC Outputs
# ==============================================================================

output "vpc_id" {
  description = "VPC ID"
  value       = aws_vpc.main.id
}

output "vpc_cidr" {
  description = "VPC CIDR block"
  value       = aws_vpc.main.cidr_block
}

output "public_subnet_ids" {
  description = "Public subnet IDs"
  value       = aws_subnet.public[*].id
}

output "private_subnet_ids" {
  description = "Private subnet IDs"
  value       = aws_subnet.private[*].id
}

output "database_subnet_ids" {
  description = "Database subnet IDs"
  value       = aws_subnet.database[*].id
}

output "nat_gateway_ips" {
  description = "NAT Gateway public IPs"
  value       = var.enable_nat_gateway ? aws_eip.nat[*].public_ip : []
}

# ==============================================================================
# ECR Outputs
# ==============================================================================

output "ecr_repository_url" {
  description = "ECR repository URL"
  value       = aws_ecr_repository.main.repository_url
}

output "ecr_repository_arn" {
  description = "ECR repository ARN"
  value       = aws_ecr_repository.main.arn
}

output "docker_push_command" {
  description = "Command to push Docker images"
  value       = "docker push ${aws_ecr_repository.main.repository_url}:TAG"
}

# ==============================================================================
# ECS Outputs
# ==============================================================================

output "ecs_cluster_id" {
  description = "ECS cluster ID"
  value       = aws_ecs_cluster.main.id
}

output "ecs_cluster_name" {
  description = "ECS cluster name"
  value       = aws_ecs_cluster.main.name
}

output "ecs_cluster_arn" {
  description = "ECS cluster ARN"
  value       = aws_ecs_cluster.main.arn
}

output "ecs_service_name" {
  description = "ECS service name"
  value       = aws_ecs_service.main.name
}

output "ecs_task_definition_arn" {
  description = "ECS task definition ARN"
  value       = aws_ecs_task_definition.main.arn
}

output "ecs_security_group_id" {
  description = "ECS tasks security group ID"
  value       = aws_security_group.ecs_tasks.id
}

# ==============================================================================
# Load Balancer Outputs
# ==============================================================================

output "alb_dns_name" {
  description = "ALB DNS name"
  value       = aws_lb.main.dns_name
}

output "alb_zone_id" {
  description = "ALB hosted zone ID"
  value       = aws_lb.main.zone_id
}

output "alb_arn" {
  description = "ALB ARN"
  value       = aws_lb.main.arn
}

output "service_url" {
  description = "Service URL"
  value       = "https://${aws_lb.main.dns_name}"
}

# ==============================================================================
# RDS Outputs
# ==============================================================================

output "rds_endpoint" {
  description = "RDS endpoint"
  value       = var.enable_rds ? aws_db_instance.main[0].endpoint : null
}

output "rds_address" {
  description = "RDS address (without port)"
  value       = var.enable_rds ? aws_db_instance.main[0].address : null
}

output "rds_port" {
  description = "RDS port"
  value       = var.enable_rds ? aws_db_instance.main[0].port : null
}

output "rds_instance_id" {
  description = "RDS instance identifier"
  value       = var.enable_rds ? aws_db_instance.main[0].identifier : null
}

output "rds_proxy_endpoint" {
  description = "RDS Proxy endpoint"
  value       = var.enable_rds && var.enable_rds_proxy ? aws_db_proxy.main[0].endpoint : null
}

output "rds_replica_endpoints" {
  description = "RDS read replica endpoints"
  value       = var.enable_rds ? aws_db_instance.replica[*].endpoint : []
}

# ==============================================================================
# Secrets Outputs
# ==============================================================================

output "kms_key_id" {
  description = "KMS key ID"
  value       = aws_kms_key.main.key_id
}

output "kms_key_arn" {
  description = "KMS key ARN"
  value       = aws_kms_key.main.arn
}

output "erlang_cookie_secret_arn" {
  description = "Erlang cookie secret ARN"
  value       = aws_secretsmanager_secret.erlang_cookie.arn
}

output "database_url_secret_arn" {
  description = "Database URL secret ARN"
  value       = aws_secretsmanager_secret.database_url.arn
}

output "api_keys_secret_arn" {
  description = "API keys secret ARN"
  value       = aws_secretsmanager_secret.api_keys.arn
}

# ==============================================================================
# IAM Outputs
# ==============================================================================

output "ecs_execution_role_arn" {
  description = "ECS execution role ARN"
  value       = aws_iam_role.ecs_execution.arn
}

output "ecs_task_role_arn" {
  description = "ECS task role ARN"
  value       = aws_iam_role.ecs_task.arn
}

# ==============================================================================
# Monitoring Outputs
# ==============================================================================

output "cloudwatch_log_group_name" {
  description = "CloudWatch log group name"
  value       = aws_cloudwatch_log_group.app.name
}

output "cloudwatch_log_group_arn" {
  description = "CloudWatch log group ARN"
  value       = aws_cloudwatch_log_group.app.arn
}

output "sns_alerts_topic_arn" {
  description = "SNS alerts topic ARN"
  value       = aws_sns_topic.alerts.arn
}

output "dashboard_url" {
  description = "CloudWatch dashboard URL"
  value       = "https://${local.region}.console.aws.amazon.com/cloudwatch/home?region=${local.region}#dashboards:name=${local.name_prefix}"
}

# ==============================================================================
# Service Discovery Outputs
# ==============================================================================

output "service_discovery_namespace" {
  description = "Service discovery namespace"
  value       = var.enable_service_discovery ? aws_service_discovery_private_dns_namespace.main[0].name : null
}

output "service_discovery_service_arn" {
  description = "Service discovery service ARN"
  value       = var.enable_service_discovery ? aws_service_discovery_service.main[0].arn : null
}

# ==============================================================================
# CI/CD Configuration
# ==============================================================================

output "cicd_config" {
  description = "Configuration values for CI/CD pipelines"
  value = {
    account_id       = local.account_id
    region           = local.region
    ecr_repository   = aws_ecr_repository.main.repository_url
    ecs_cluster      = aws_ecs_cluster.main.name
    ecs_service      = aws_ecs_service.main.name
    task_definition  = aws_ecs_task_definition.main.family
    execution_role   = aws_iam_role.ecs_execution.arn
    task_role        = aws_iam_role.ecs_task.arn
  }
}

# ==============================================================================
# Connection Info
# ==============================================================================

output "connection_info" {
  description = "Connection information for applications"
  value = {
    service_url = "https://${aws_lb.main.dns_name}"
    database = var.enable_rds ? {
      host     = var.enable_rds_proxy ? aws_db_proxy.main[0].endpoint : aws_db_instance.main[0].address
      port     = 5432
      database = var.db_name
      username = var.db_username
    } : null
  }
  sensitive = true
}

# ==============================================================================
# Deployment Instructions
# ==============================================================================

output "deployment_instructions" {
  description = "Next steps for deployment"
  value = {
    step1 = "Authenticate with ECR: aws ecr get-login-password --region ${local.region} | docker login --username AWS --password-stdin ${aws_ecr_repository.main.repository_url}"
    step2 = "Build and tag image: docker build -t ${aws_ecr_repository.main.repository_url}:latest ."
    step3 = "Push image: docker push ${aws_ecr_repository.main.repository_url}:latest"
    step4 = "Update ECS service: aws ecs update-service --cluster ${aws_ecs_cluster.main.name} --service ${aws_ecs_service.main.name} --force-new-deployment"
  }
}
