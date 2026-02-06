# ============================================================================
# Outputs for erlmcp v3 Network Infrastructure
# ============================================================================

# ============================================================================
# VPC Outputs
# ============================================================================
output "vpc_id" {
  description = "The ID of the VPC"
  value       = module.vpc.vpc_id
}

output "vpc_name" {
  description = "The name of the VPC"
  value       = module.vpc.name
}

output "vpc_cidr_block" {
  description = "The CIDR block of the VPC"
  value       = module.vpc.vpc_cidr_block
}

output "vpc_arn" {
  description = "The ARN of the VPC"
  value       = module.vpc.vpc_arn
}

# ============================================================================
# Subnet Outputs
# ============================================================================
output "private_subnets" {
  description = "List of private subnet IDs"
  value       = module.vpc.private_subnets
}

output "public_subnets" {
  description = "List of public subnet IDs"
  value       = module.vpc.public_subnets
}

output "database_subnets" {
  description = "List of database subnet IDs"
  value       = module.vpc.database_subnets
}

output "private_subnet_cidrs" {
  description = "List of private subnet CIDR blocks"
  value       = module.vpc.private_subnets_cidr_blocks
}

output "public_subnet_cidrs" {
  description = "List of public subnet CIDR blocks"
  value       = module.vpc.public_subnets_cidr_blocks
}

output "database_subnet_cidrs" {
  description = "List of database subnet CIDR blocks"
  value       = module.vpc.database_subnets_cidr_blocks
}

output "availability_zones" {
  description = "List of availability zones used"
  value       = module.vpc.azs
}

# ============================================================================
# NAT Gateway Outputs
# ============================================================================
output "nat_gateway_ids" {
  description = "List of NAT Gateway IDs"
  value       = module.vpc.natgw_ids
}

output "nat_public_ips" {
  description = "List of NAT Gateway public IPs"
  value       = module.vpc.nat_public_ips
}

# ============================================================================
# Internet Gateway Outputs
# ============================================================================
output "internet_gateway_id" {
  description = "The ID of the internet gateway"
  value       = module.vpc.igw_id
}

output "internet_gateway_arn" {
  description = "The ARN of the internet gateway"
  value       = module.vpc.igw_arn
}

# ============================================================================
# Route Table Outputs
# ============================================================================
output "private_route_table_ids" {
  description = "List of private route table IDs"
  value       = module.vpc.private_route_table_ids
}

output "public_route_table_ids" {
  description = "List of public route table IDs"
  value       = module.vpc.public_route_table_ids
}

output "database_route_table_ids" {
  description = "List of database route table IDs"
  value       = module.vpc.database_route_table_ids
}

output "default_route_table_id" {
  description = "The ID of the default route table"
  value       = module.vpc.default_route_table_id
}

# ============================================================================
# Security Group Outputs
# ============================================================================
output "alb_security_group_id" {
  description = "The ID of the ALB security group"
  value       = var.create_alb_sg ? aws_security_group.alb[0].id : null
}

output "application_security_group_id" {
  description = "The ID of the application security group"
  value       = var.create_app_sg ? aws_security_group.application[0].id : null
}

output "database_security_group_id" {
  description = "The ID of the database security group"
  value       = var.create_db_sg ? aws_security_group.database[0].id : null
}

output "management_security_group_id" {
  description = "The ID of the management security group"
  value       = var.create_mgmt_sg ? aws_security_group.management[0].id : null
}

output "vpc_endpoints_security_group_id" {
  description = "The ID of the VPC endpoints security group"
  value       = var.enable_interface_endpoints ? aws_security_group.vpc_endpoints[0].id : null
}

output "default_security_group_id" {
  description = "The ID of the default security group"
  value       = module.vpc.default_security_group_id
}

# ============================================================================
# VPC Endpoint Outputs
# ============================================================================
output "s3_endpoint_id" {
  description = "The ID of the S3 VPC endpoint"
  value       = var.enable_s3_endpoint ? aws_vpc_endpoint.s3[0].id : null
}

output "dynamodb_endpoint_id" {
  description = "The ID of the DynamoDB VPC endpoint"
  value       = var.enable_dynamodb_endpoint ? aws_vpc_endpoint.dynamodb[0].id : null
}

output "interface_endpoint_ids" {
  description = "Map of interface endpoint names to IDs"
  value = var.enable_interface_endpoints ? {
    for k, v in aws_vpc_endpoint.interface_endpoints : k => v.id
  } : {}
}

output "interface_endpoint_dns_entries" {
  description = "Map of interface endpoint DNS entries"
  value = var.enable_interface_endpoints ? {
    for k, v in aws_vpc_endpoint.interface_endpoints : k => v.dns_entry
  } : {}
}

# ============================================================================
# VPN Gateway Outputs
# ============================================================================
output "vpn_gateway_id" {
  description = "The ID of the VPN gateway"
  value       = var.enable_vpn_gateway ? module.vpc.vgw_id : null
}

output "vpn_gateway_arn" {
  description = "The ARN of the VPN gateway"
  value       = var.enable_vpn_gateway ? module.vpc.vgw_arn : null
}

# ============================================================================
# DNS Outputs
# ============================================================================
output "dns_hostnames_enabled" {
  description = "Whether DNS hostnames are enabled"
  value       = var.enable_dns_hostnames
}

output "dns_support_enabled" {
  description = "Whether DNS support is enabled"
  value       = var.enable_dns_support
}

# ============================================================================
# Network ACL Outputs
# ============================================================================
output "private_network_acl_id" {
  description = "The ID of the private network ACL"
  value       = var.create_network_acls ? aws_network_acl.private[0].id : null
}

output "database_network_acl_id" {
  description = "The ID of the database network ACL"
  value       = var.create_network_acls ? aws_network_acl.database[0].id : null
}

output "default_network_acl_id" {
  description = "The ID of the default network ACL"
  value       = module.vpc.default_network_acl_id
}

# ============================================================================
# VPC Flow Logs Outputs
# ============================================================================
output "flow_logs_cloudwatch_log_group_name" {
  description = "The name of the CloudWatch log group for VPC flow logs"
  value       = var.enable_flow_logs ? aws_cloudwatch_log_group.flow_logs[0].name : null
}

output "flow_logs_cloudwatch_log_group_arn" {
  description = "The ARN of the CloudWatch log group for VPC flow logs"
  value       = var.enable_flow_logs ? aws_cloudwatch_log_group.flow_logs[0].arn : null
}

output "flow_logs_s3_bucket_id" {
  description = "The ID of the S3 bucket for VPC flow logs"
  value       = var.enable_flow_logs_to_s3 ? aws_s3_bucket.flow_logs[0].id : null
}

output "flow_logs_s3_bucket_arn" {
  description = "The ARN of the S3 bucket for VPC flow logs"
  value       = var.enable_flow_logs_to_s3 ? aws_s3_bucket.flow_logs[0].arn : null
}

# ============================================================================
# VPC Peering Outputs
# ============================================================================
output "vpc_peering_connection_id" {
  description = "The ID of the VPC peering connection"
  value       = var.enable_vpc_peering ? aws_vpc_peering_connection.peer[0].id : null
}

output "vpc_peering_connection_status" {
  description = "The status of the VPC peering connection"
  value       = var.enable_vpc_peering ? aws_vpc_peering_connection.peer[0].accept_status : null
}

# ============================================================================
# Transit Gateway Outputs
# ============================================================================
output "transit_gateway_id" {
  description = "The ID of the Transit Gateway"
  value       = var.enable_transit_gateway ? aws_ec2_transit_gateway.main[0].id : null
}

output "transit_gateway_arn" {
  description = "The ARN of the Transit Gateway"
  value       = var.enable_transit_gateway ? aws_ec2_transit_gateway.main[0].arn : null
}

output "transit_gateway_attachment_id" {
  description = "The ID of the Transit Gateway VPC attachment"
  value       = var.enable_transit_gateway ? aws_ec2_transit_gateway_vpc_attachment.main[0].id : null
}

# ============================================================================
# Aggregate Summary Output
# ============================================================================
output "network_summary" {
  description = "Summary of network configuration"
  value = {
    vpc_name                      = module.vpc.name
    vpc_id                        = module.vpc.vpc_id
    vpc_cidr                      = module.vpc.vpc_cidr_block
    availability_zones            = module.vpc.azs
    private_subnets_count         = length(module.vpc.private_subnets)
    public_subnets_count          = length(module.vpc.public_subnets)
    database_subnets_count        = length(module.vpc.database_subnets)
    nat_gateways_count            = length(module.vpc.natgw_ids)
    flow_logs_enabled             = var.enable_flow_logs
    flow_logs_to_s3_enabled       = var.enable_flow_logs_to_s3
    s3_endpoint_enabled           = var.enable_s3_endpoint
    dynamodb_endpoint_enabled     = var.enable_dynamodb_endpoint
    interface_endpoints_enabled   = var.enable_interface_endpoints
    interface_endpoints_count     = length(local.interface_endpoints)
    vpc_peering_enabled           = var.enable_vpc_peering
    transit_gateway_enabled       = var.enable_transit_gateway
    network_acls_enabled          = var.create_network_acls
    security_groups = {
      alb         = var.create_alb_sg
      application = var.create_app_sg
      database    = var.create_db_sg
      management  = var.create_mgmt_sg
    }
  }
}