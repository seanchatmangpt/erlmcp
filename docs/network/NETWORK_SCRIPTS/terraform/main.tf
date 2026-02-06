# ============================================================================
# Terraform main configuration for erlmcp v3 Network Infrastructure
# Enterprise-grade AWS VPC with security best practices
# ============================================================================

terraform {
  required_version = ">= 1.5.0"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = ">= 5.0"
    }
    random = {
      source  = "hashicorp/random"
      version = ">= 3.5"
    }
  }
}

# Provider configuration
provider "aws" {
  region = var.region

  default_tags {
    tags = {
      Project     = "erlmcp"
      Environment = var.environment
      ManagedBy   = "Terraform"
      Repository  = "erlmcp/v3"
    }
  }
}

# Data source for current AWS account
data "aws_caller_identity" "current" {}

# Data source for availability zones
data "aws_availability_zones" "available" {
  state = "available"
}

# Generate random suffix for resource names
resource "random_pet" "suffix" {
  length = 2
}

# ============================================================================
# VPC Configuration - Using Official AWS Module
# ============================================================================
module "vpc" {
  source  = "terraform-aws-modules/vpc/aws"
  version = "~> 5.0"

  name = "${var.vpc_name_prefix}-${random_pet.suffix.id}"
  cidr = var.vpc_cidr

  azs              = var.azs != null ? var.azs : slice(data.aws_availability_zones.available.names, 0, 3)
  private_subnets  = var.private_subnet_cidrs
  public_subnets   = var.public_subnet_cidrs
  database_subnets = var.database_subnet_cidrs

  # High Availability NAT Gateway Configuration
  enable_nat_gateway     = var.enable_nat_gateway
  single_nat_gateway     = var.single_nat_gateway
  one_nat_gateway_per_az = var.one_nat_gateway_per_az

  # DNS Configuration
  enable_dns_hostnames = var.enable_dns_hostnames
  enable_dns_support   = var.enable_dns_support

  # VPC Flow Logs
  enable_flow_log                      = var.enable_flow_logs
  create_flow_log_cloudwatch_iam_role  = var.enable_flow_logs
  create_flow_log_cloudwatch_log_group = var.enable_flow_logs
  flow_log_retention_in_days           = var.flow_log_retention_days
  flow_log_max_aggregation_interval    = var.flow_log_aggregation_interval
  flow_log_traffic_type                = var.flow_log_traffic_type

  # VPN Gateway (optional for hybrid connectivity)
  enable_vpn_gateway = var.enable_vpn_gateway

  # Default Security Group
  manage_default_security_group  = true
  default_security_group_ingress = []
  default_security_group_egress  = []

  # Network ACLs - Default deny all for security
  manage_default_network_acl = true
  default_network_acl_ingress = [
    {
      rule_no    = 100
      action     = "allow"
      from_port  = 0
      to_port    = 0
      protocol   = "-1"
      cidr_block = var.vpc_cidr
    },
  ]
  default_network_acl_egress = [
    {
      rule_no    = 100
      action     = "allow"
      from_port  = 0
      to_port    = 0
      protocol   = "-1"
      cidr_block = var.vpc_cidr
    },
  ]

  # Subnet tagging for Kubernetes integration
  public_subnet_tags = {
    "kubernetes.io/role/elb" = "1"
    Tier                     = "public"
  }

  private_subnet_tags = {
    "kubernetes.io/role/internal-elb" = "1"
    Tier                              = "private"
  }

  database_subnet_tags = {
    Tier = "database"
  }

  tags = var.tags
}

# ============================================================================
# VPC Endpoints - Private connectivity to AWS services
# ============================================================================

# Gateway Endpoints (free)
resource "aws_vpc_endpoint" "s3" {
  count        = var.enable_s3_endpoint ? 1 : 0
  vpc_id       = module.vpc.vpc_id
  service_name = "com.amazonaws.${var.region}.s3"
  route_table_ids = concat(
    module.vpc.private_route_table_ids,
    module.vpc.public_route_table_ids,
    module.vpc.database_route_table_ids
  )

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-s3-endpoint"
  })
}

resource "aws_vpc_endpoint" "dynamodb" {
  count        = var.enable_dynamodb_endpoint ? 1 : 0
  vpc_id       = module.vpc.vpc_id
  service_name = "com.amazonaws.${var.region}.dynamodb"
  route_table_ids = concat(
    module.vpc.private_route_table_ids,
    module.vpc.public_route_table_ids
  )

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-dynamodb-endpoint"
  })
}

# Interface Endpoints (cost per hour + data transfer)
locals {
  interface_endpoints = var.enable_interface_endpoints ? [
    "ec2",
    "ecr.api",
    "ecr.dkr",
    "ecs",
    "ecs-agent",
    "ecs-telemetry",
    "logs",
    "ssm",
    "ssmmessages",
    "ec2messages",
    "kms",
    "secretsmanager",
    "sts",
  ] : []
}

resource "aws_security_group" "vpc_endpoints" {
  count       = var.enable_interface_endpoints ? 1 : 0
  name        = "${module.vpc.name}-vpc-endpoints-sg"
  description = "Security group for VPC endpoints"
  vpc_id      = module.vpc.vpc_id

  ingress {
    description = "HTTPS from VPC"
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = [module.vpc.vpc_cidr_block]
  }

  egress {
    description = "Allow all outbound"
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-vpc-endpoints-sg"
  })
}

resource "aws_vpc_endpoint" "interface_endpoints" {
  for_each = toset(local.interface_endpoints)

  vpc_id              = module.vpc.vpc_id
  service_name        = "com.amazonaws.${var.region}.${each.value}"
  vpc_endpoint_type   = "Interface"
  subnet_ids          = module.vpc.private_subnets
  security_group_ids  = [aws_security_group.vpc_endpoints[0].id]
  private_dns_enabled = true

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-${each.value}-endpoint"
  })
}

# ============================================================================
# Security Groups - Layered security approach
# ============================================================================

# ALB Security Group
resource "aws_security_group" "alb" {
  count       = var.create_alb_sg ? 1 : 0
  name        = "${module.vpc.name}-alb-sg"
  description = "Security group for Application Load Balancer"
  vpc_id      = module.vpc.vpc_id

  ingress {
    description = "HTTP from anywhere"
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = var.alb_ingress_cidrs
  }

  ingress {
    description = "HTTPS from anywhere"
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = var.alb_ingress_cidrs
  }

  egress {
    description = "Allow all outbound"
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-alb-sg"
  })
}

# Application Security Group (ECS/EKS/EC2)
resource "aws_security_group" "application" {
  count       = var.create_app_sg ? 1 : 0
  name        = "${module.vpc.name}-app-sg"
  description = "Security group for erlmcp application tier"
  vpc_id      = module.vpc.vpc_id

  # Allow traffic from ALB
  dynamic "ingress" {
    for_each = var.create_alb_sg ? [1] : []
    content {
      description     = "HTTP from ALB"
      from_port       = 8080
      to_port         = 8080
      protocol        = "tcp"
      security_groups = [aws_security_group.alb[0].id]
    }
  }

  # Allow internal traffic within application tier
  ingress {
    description = "All traffic from application tier"
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    self        = true
  }

  # Erlang distribution protocol
  ingress {
    description = "EPMD for Erlang"
    from_port   = 4369
    to_port     = 4369
    protocol    = "tcp"
    self        = true
  }

  ingress {
    description = "Erlang distribution"
    from_port   = 9000
    to_port     = 9999
    protocol    = "tcp"
    self        = true
  }

  # MCP Protocol ports
  ingress {
    description = "MCP API"
    from_port   = 8080
    to_port     = 8080
    protocol    = "tcp"
    cidr_blocks = [module.vpc.vpc_cidr_block]
  }

  ingress {
    description = "MCP WebSocket"
    from_port   = 8443
    to_port     = 8443
    protocol    = "tcp"
    cidr_blocks = [module.vpc.vpc_cidr_block]
  }

  ingress {
    description = "MCP Metrics"
    from_port   = 9090
    to_port     = 9090
    protocol    = "tcp"
    cidr_blocks = [module.vpc.vpc_cidr_block]
  }

  egress {
    description = "Allow all outbound"
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-app-sg"
  })
}

# Database Security Group
resource "aws_security_group" "database" {
  count       = var.create_db_sg ? 1 : 0
  name        = "${module.vpc.name}-db-sg"
  description = "Security group for database tier"
  vpc_id      = module.vpc.vpc_id

  # PostgreSQL from application tier
  dynamic "ingress" {
    for_each = var.create_app_sg ? [1] : []
    content {
      description     = "PostgreSQL from application"
      from_port       = 5432
      to_port         = 5432
      protocol        = "tcp"
      security_groups = [aws_security_group.application[0].id]
    }
  }

  # Redis from application tier
  dynamic "ingress" {
    for_each = var.create_app_sg ? [1] : []
    content {
      description     = "Redis from application"
      from_port       = 6379
      to_port         = 6379
      protocol        = "tcp"
      security_groups = [aws_security_group.application[0].id]
    }
  }

  egress {
    description = "Allow all outbound"
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-db-sg"
  })
}

# Bastion/Management Security Group
resource "aws_security_group" "management" {
  count       = var.create_mgmt_sg ? 1 : 0
  name        = "${module.vpc.name}-mgmt-sg"
  description = "Security group for management/bastion tier"
  vpc_id      = module.vpc.vpc_id

  # SSH from specific IPs only (use Systems Manager Session Manager instead)
  dynamic "ingress" {
    for_each = length(var.ssh_allowed_cidrs) > 0 ? [1] : []
    content {
      description = "SSH from allowed IPs"
      from_port   = 22
      to_port     = 22
      protocol    = "tcp"
      cidr_blocks = var.ssh_allowed_cidrs
    }
  }

  egress {
    description = "Allow all outbound"
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-mgmt-sg"
  })
}

# ============================================================================
# Network ACLs - Additional layer of defense
# ============================================================================

resource "aws_network_acl" "private" {
  count      = var.create_network_acls ? 1 : 0
  vpc_id     = module.vpc.vpc_id
  subnet_ids = module.vpc.private_subnets

  # Inbound rules
  ingress {
    protocol   = -1
    rule_no    = 100
    action     = "allow"
    cidr_block = module.vpc.vpc_cidr_block
    from_port  = 0
    to_port    = 0
  }

  ingress {
    protocol   = "tcp"
    rule_no    = 110
    action     = "allow"
    cidr_block = "0.0.0.0/0"
    from_port  = 1024
    to_port    = 65535
  }

  # Outbound rules
  egress {
    protocol   = -1
    rule_no    = 100
    action     = "allow"
    cidr_block = "0.0.0.0/0"
    from_port  = 0
    to_port    = 0
  }

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-private-nacl"
  })
}

resource "aws_network_acl" "database" {
  count      = var.create_network_acls ? 1 : 0
  vpc_id     = module.vpc.vpc_id
  subnet_ids = module.vpc.database_subnets

  # Inbound rules - only from VPC
  ingress {
    protocol   = -1
    rule_no    = 100
    action     = "allow"
    cidr_block = module.vpc.vpc_cidr_block
    from_port  = 0
    to_port    = 0
  }

  # Outbound rules - only to VPC
  egress {
    protocol   = -1
    rule_no    = 100
    action     = "allow"
    cidr_block = module.vpc.vpc_cidr_block
    from_port  = 0
    to_port    = 0
  }

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-database-nacl"
  })
}

# ============================================================================
# CloudWatch Log Group for VPC Flow Logs
# ============================================================================
resource "aws_cloudwatch_log_group" "flow_logs" {
  count             = var.enable_flow_logs ? 1 : 0
  name              = "/aws/vpc/${module.vpc.name}/flow-logs"
  retention_in_days = var.flow_log_retention_days
  kms_key_id        = var.flow_log_kms_key_id

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-flow-logs"
  })
}

# ============================================================================
# S3 Bucket for VPC Flow Logs (Alternative to CloudWatch)
# ============================================================================
resource "aws_s3_bucket" "flow_logs" {
  count  = var.enable_flow_logs_to_s3 ? 1 : 0
  bucket = "${module.vpc.name}-flow-logs-${data.aws_caller_identity.current.account_id}"

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-flow-logs"
  })
}

resource "aws_s3_bucket_public_access_block" "flow_logs" {
  count  = var.enable_flow_logs_to_s3 ? 1 : 0
  bucket = aws_s3_bucket.flow_logs[0].id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

resource "aws_s3_bucket_server_side_encryption_configuration" "flow_logs" {
  count  = var.enable_flow_logs_to_s3 ? 1 : 0
  bucket = aws_s3_bucket.flow_logs[0].id

  rule {
    apply_server_side_encryption_by_default {
      sse_algorithm = "AES256"
    }
  }
}

resource "aws_s3_bucket_lifecycle_configuration" "flow_logs" {
  count  = var.enable_flow_logs_to_s3 ? 1 : 0
  bucket = aws_s3_bucket.flow_logs[0].id

  rule {
    id     = "transition-to-glacier"
    status = "Enabled"

    transition {
      days          = 90
      storage_class = "GLACIER"
    }

    expiration {
      days = 365
    }
  }
}

# ============================================================================
# VPC Peering (optional)
# ============================================================================
resource "aws_vpc_peering_connection" "peer" {
  count       = var.enable_vpc_peering ? 1 : 0
  vpc_id      = module.vpc.vpc_id
  peer_vpc_id = var.peer_vpc_id
  peer_region = var.peer_vpc_region
  auto_accept = var.peer_vpc_auto_accept

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-peering"
  })
}

resource "aws_vpc_peering_connection_accepter" "peer" {
  count                     = var.enable_vpc_peering && var.peer_vpc_auto_accept ? 1 : 0
  vpc_peering_connection_id = aws_vpc_peering_connection.peer[0].id
  auto_accept               = true

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-peering-accepter"
  })
}

# ============================================================================
# Transit Gateway (optional - for multi-VPC architecture)
# ============================================================================
resource "aws_ec2_transit_gateway" "main" {
  count                           = var.enable_transit_gateway ? 1 : 0
  description                     = "Transit Gateway for ${module.vpc.name}"
  default_route_table_association = "enable"
  default_route_table_propagation = "enable"
  dns_support                     = "enable"
  vpn_ecmp_support                = "enable"

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-tgw"
  })
}

resource "aws_ec2_transit_gateway_vpc_attachment" "main" {
  count              = var.enable_transit_gateway ? 1 : 0
  subnet_ids         = module.vpc.private_subnets
  transit_gateway_id = aws_ec2_transit_gateway.main[0].id
  vpc_id             = module.vpc.vpc_id

  tags = merge(var.tags, {
    Name = "${module.vpc.name}-tgw-attachment"
  })
}