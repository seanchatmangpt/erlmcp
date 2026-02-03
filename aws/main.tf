# ==============================================================================
# ERLMCP AWS Infrastructure - Main Configuration
# ==============================================================================
# Production-grade AWS infrastructure for erlmcp MCP SDK.
# Follows BEAMOps principles: immutable artifacts, explicit config, BEAM-aware.
#
# Usage:
#   terraform init
#   terraform plan -var-file=terraform.production.tfvars
#   terraform apply -var-file=terraform.production.tfvars
# ==============================================================================

terraform {
  required_version = ">= 1.6.0"

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.30"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.6"
    }
  }

  # Remote state configuration (uncomment for production)
  # backend "s3" {
  #   bucket         = "erlmcp-terraform-state"
  #   key            = "erlmcp/terraform.tfstate"
  #   region         = "us-east-1"
  #   encrypt        = true
  #   dynamodb_table = "erlmcp-terraform-locks"
  # }
}

# ==============================================================================
# AWS Provider Configuration
# ==============================================================================

provider "aws" {
  region = var.aws_region

  default_tags {
    tags = local.common_tags
  }
}

# Secondary region for disaster recovery (optional)
provider "aws" {
  alias  = "dr"
  region = var.dr_region

  default_tags {
    tags = local.common_tags
  }
}

# ==============================================================================
# Data Sources
# ==============================================================================

data "aws_caller_identity" "current" {}

data "aws_region" "current" {}

data "aws_availability_zones" "available" {
  state = "available"
}

# Latest ECS-optimized AMI for EC2 launch type (if needed)
data "aws_ssm_parameter" "ecs_ami" {
  name = "/aws/service/ecs/optimized-ami/amazon-linux-2023/recommended/image_id"
}

# ==============================================================================
# Random Resources
# ==============================================================================

resource "random_password" "db_password" {
  count   = var.db_password == "" ? 1 : 0
  length  = 32
  special = true
  # Exclude characters that cause issues in connection strings
  override_special = "!#$%&*()-_=+[]{}<>:?"
}

resource "random_password" "erlang_cookie" {
  count   = var.erlang_cookie == "" ? 1 : 0
  length  = 64
  special = false
}

resource "random_id" "suffix" {
  byte_length = 4
}
