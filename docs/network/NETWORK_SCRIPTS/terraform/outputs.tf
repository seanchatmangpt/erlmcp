# Outputs for erlmcp v3 Network Infrastructure

output "vpc_id" {
  description = "The ID of the VPC"
  value       = module.vpc.vpc_id
}

output "vpc_cidr_block" {
  description = "The CIDR block of the VPC"
  value       = module.vpc.vpc_cidr_block
}

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

output "availability_zones" {
  description = "List of availability zones used"
  value       = module.vpc.azs
}

output "private_subnet_ips" {
  description = "List of private subnet CIDR blocks"
  value       = module.vpc.private_subnets_cidr_blocks
}

output "public_subnet_ips" {
  description = "List of public subnet CIDR blocks"
  value       = module.vpc.public_subnets_cidr_blocks
}

output "database_subnet_ips" {
  description = "List of database subnet CIDR blocks"
  value       = module.vpc.database_subnets_cidr_blocks
}

output "nat_gateway_id" {
  description = "The ID of the NAT gateway"
  value       = aws_nat_gateway.main.id
}

output "nat_public_ip" {
  description = "The public IP of the NAT gateway"
  value       = aws_eip.nat.public_ip
}

output "internet_gateway_id" {
  description = "The ID of the internet gateway"
  value       = aws_internet_gateway.main.id
}

output "default_route_table_id" {
  description = "The ID of the default route table"
  value       = module.vpc.default_route_table_id
}

output "public_route_table_id" {
  description = "The ID of the public route table"
  value       = aws_route_table.public.id
}

output "private_route_table_id" {
  description = "The ID of the private route table"
  value       = aws_route_table.private.id
}

output "security_group_id" {
  description = "The ID of the default security group"
  value       = module.security_groups.security_group_id
}

output "security_group_vpc_id" {
  description = "The VPC ID of the security group"
  value       = module.security_groups.vpc_id
}

output "vpn_gateway_id" {
  description = "The ID of the VPN gateway"
  value       = module.vpc.vpn_gateway_id
}

output "dns_hostnames_enabled" {
  description = "Whether DNS hostnames are enabled"
  value       = module.vpc.enable_dns_hostnames
}

output "dns_support_enabled" {
  description = "Whether DNS support is enabled"
  value       = module.vpc.enable_dns_support
}

output "network_acl_id" {
  description = "The ID of the network ACL"
  value       = module.vpc.network_acl_id
}

output "s3_bucket_id" {
  description = "The ID of the S3 bucket for logging"
  value       = aws_s3_bucket.alb_logs.id
}

output "cloudwatch_log_group" {
  description = "The name of the CloudWatch log group"
  value       = aws_cloudwatch_log_group.alb_logs.name
}