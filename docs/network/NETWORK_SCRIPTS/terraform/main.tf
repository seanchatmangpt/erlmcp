# Terraform main configuration for erlmcp v3 Network Infrastructure
terraform {
  required_version = ">= 1.0"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = ">= 4.0"
    }
    random = {
      source  = "hashicorp/random"
      version = ">= 3.0"
    }
    tls = {
      source  = "hashicorp/tls"
      version = ">= 4.0"
    }
  }
}

# Provider configuration
provider "aws" {
  region  = var.region
  profile = var.profile
}

# Generate random suffix for resource names
resource "random_pet" "suffix" {
  length = 2
}

# VPC Configuration
module "vpc" {
  source  = "terraform-aws-modules/vpc/aws"
  version = ">= 4.0"

  name = "erlmcp-vpc-${random_pet.suffix.id}"
  cidr = "10.0.0.0/16"

  azs             = var.azs
  private_subnets = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
  public_subnets  = ["10.0.101.0/24", "10.0.102.0/24", "10.0.103.0/24"]

  enable_nat_gateway = true
  enable_vpn_gateway = true
  single_nat_gateway = true

  enable_dns_support   = true
  enable_dns_hostnames = true

  tags = {
    Project     = "erlmcp"
    Environment = "production"
    Tier        = "network"
  }
}

# Internet Gateway
resource "aws_internet_gateway" "main" {
  vpc_id = module.vpc.vpc_id

  tags = {
    Project     = "erlmcp"
    Environment = "production"
    Name        = "erlmcp-igw"
  }
}

# NAT Gateway
resource "aws_nat_gateway" "main" {
  allocation_id = aws_eip.nat.id
  subnet_id     = module.vpc.public_subnets[0]

  tags = {
    Project     = "erlmcp"
    Environment = "production"
    Name        = "erlmcp-nat"
  }

  depends_on = [aws_internet_gateway.main]
}

# Elastic IP for NAT Gateway
resource "aws_eip" "nat" {
  domain   = "vpc"
  depends_on = [aws_internet_gateway.main]

  tags = {
    Project     = "erlmcp"
    Environment = "production"
    Name        = "erlmcp-eip-nat"
  }
}

# Security Groups
module "security_groups" {
  source  = "terraform-aws/modules/security-group/aws"
  version = ">= 4.0"

  name        = "erlmcp-sg"
  description = "erlmcp v3 security group"
  vpc_id      = module.vpc.vpc_id

  ingress_rules = {
    http = {
      from_port   = 80
      to_port     = 80
      protocol    = "tcp"
      cidr_blocks = ["0.0.0.0/0"]
    }
    https = {
      from_port   = 443
      to_port     = 443
      protocol    = "tcp"
      cidr_blocks = ["0.0.0.0/0"]
    }
    ssh = {
      from_port   = 22
      to_port     = 22
      protocol    = "tcp"
      cidr_blocks = ["10.40.0.0/24"]
    }
    erlmcp_api = {
      from_port   = 8080
      to_port     = 8080
      protocol    = "tcp"
      cidr_blocks = ["10.10.0.0/22"]
    }
    erlmcp_ws = {
      from_port   = 8443
      to_port     = 8443
      protocol    = "tcp"
      cidr_blocks = ["10.20.0.0/16"]
    }
  }

  egress_rules = {
    all = {
      from_port   = 0
      to_port     = 0
      protocol    = "-1"
      cidr_blocks = ["0.0.0.0/0"]
    }
  }

  tags = {
    Project     = "erlmcp"
    Environment = "production"
  }
}

# Route Tables
resource "aws_route_table" "public" {
  vpc_id = module.vpc.vpc_id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.main.id
  }

  tags = {
    Project     = "erlmcp"
    Environment = "production"
    Name        = "erlmcp-public-rt"
  }
}

resource "aws_route_table" "private" {
  vpc_id = module.vpc.vpc_id

  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.main.id
  }

  tags = {
    Project     = "erlmcp"
    Environment = "production"
    Name        = "erlmcp-private-rt"
  }
}

# Route Table Associations
resource "aws_route_table_association" "public" {
  count = length(module.vpc.public_subnets)

  subnet_id      = element(module.vpc.public_subnets, count.index)
  route_table_id = aws_route_table.public.id
}

resource "aws_route_table_association" "private" {
  count = length(module.vpc.private_subnets)

  subnet_id      = element(module.vpc.private_subnets, count.index)
  route_table_id = aws_route_table.private.id
}

# Outputs
output "vpc_id" {
  description = "The ID of the VPC"
  value       = module.vpc.vpc_id
}

output "public_subnets" {
  description = "List of public subnet IDs"
  value       = module.vpc.public_subnets
}

output "private_subnets" {
  description = "List of private subnet IDs"
  value       = module.vpc.private_subnets
}

output "security_group_id" {
  description = "The ID of the security group"
  value       = module.security_groups.security_group_id
}

output "azs" {
  description = "The availability zones used"
  value       = module.vpc.azs
}