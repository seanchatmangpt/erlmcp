# ErlMCP v3 Infrastructure as Code
# Terraform configuration for enterprise deployment

terraform {
  required_version = ">= 1.5"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.11"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.6"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.5"
    }
  }
}

# Provider configuration
provider "aws" {
  region = var.aws_region

  default_tags {
    tags = {
      Project     = "erlmcp"
      Environment = var.environment
      ManagedBy   = "Terraform"
      Owner       = "erlmcp-team"
    }
  }
}

# Generate random suffix for resources
resource "random_string" "suffix" {
  length  = 8
  special = false
  upper   = false
}

# VPC Configuration
module "vpc" {
  source = "terraform-aws-modules/vpc/aws"
  version = "~> 4.0"

  name = "erlmcp-vpc-${var.environment}-${random_string.suffix.result}"
  cidr = "10.0.0.0/16"

  azs             = slice(data.aws_availability_zones.available.names, 0, 3)
  private_subnets = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
  public_subnets  = ["10.0.101.0/24", "10.0.102.0/24", "10.0.103.0/24"]

  enable_nat_gateway   = true
  enable_dns_hostnames = true
  single_nat_gateway   = true

  tags = {
    "kubernetes.io/cluster/erlmcp-${var.environment}" = "shared"
  }
}

# EKS Cluster
module "eks" {
  source  = "terraform-aws-modules/eks/aws"
  version = "~> 19.0"

  cluster_name    = "erlmcp-${var.environment}"
  cluster_version = "1.27"

  vpc_id                         = module.vpc.vpc_id
  subnet_ids                     = module.vpc.private_subnets
  cluster_endpoint_public_access  = true
  cluster_endpoint_private_access = true

  # Cluster access
  access_entries = {
    # One access entry for the main IAM user
    erlmcp_admin = {
      kubernetes_groups = ["system:masters"]
      principal_arn     = var.iam_admin_arn
    }
  }

  # Node groups
  eks_managed_node_group_defaults = {
    instance_types = ["m6i.large"]
    disk_size      = 30
  }

  eks_managed_node_groups = {
    main = {
      min_size     = 2
      max_size     = 10
      desired_size = 3

      instance_types = ["m6i.large"]

      labels = {
        Environment = var.environment
      }

      tags = {
        Environment = var.environment
      }
    }

    canary = {
      min_size     = 1
      max_size     = 3
      desired_size = 1

      instance_types = ["m6i.medium"]

      labels = {
        Environment = var.environment
        Purpose     = "canary"
      }

      tags = {
        Environment = var.environment
        Purpose     = "canary"
      }
    }
  }

  # Security groups
  cluster_security_group_tags = {
    "kubernetes.io/cluster/erlmcp-${var.environment}" = "owned"
  }

  node_security_group_tags = {
    "kubernetes.io/cluster/erlmcp-${var.environment}" = "owned"
  }
}

# RDS Cluster for ErlMCP data
module "rds" {
  source  = "terraform-aws-modules/rds/aws"
  version = "~> 5.0"

  identifier = "erlmcp-${var.environment}"

  engine              = "postgres"
  engine_version      = "15.3"
  family              = "pg15"
  major_engine_version = "15"

  instance_class = "db.m6g.large"
  allocated_storage = 100

  db_name  = "erlmcp"
  username = "erlmcp"
  password = var.rds_password
  port     = "5432"

  vpc_security_group_ids = [aws_security_group.erlmcp_rds.id]

  maintenance_window = "sun:01:00-sun:02:00"
  backup_window      = "03:00-04:00"

  backup_retention_period = 7
  enabled_cloudwatch_logs_exports = ["postgresql", "upgrade"]

  create_random_password = false

  tags = {
    Environment = var.environment
  }
}

# Security group for RDS
resource "aws_security_group" "erlmcp_rds" {
  name        = "erlmcp-${var.environment}-rds"
  description = "Allow ErlMCP cluster to access RDS"
  vpc_id      = module.vpc.vpc_id

  ingress {
    from_port   = 5432
    to_port     = 5432
    protocol    = "tcp"
    cidr_blocks = module.vpc.private_subnets
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

# Elasticache for Redis cluster
module "elasticache" {
  source  = "terraform-aws-modules/elasticache/aws"
  version = "~> 3.3"

  name          = "erlmcp-${var.environment}"
  description   = "ErlMCP Redis cluster"
  engine_version = "7.x"

  parameter_group_name = "default.redis7"

  node_type               = "cache.m6g.large"
  num_cache_nodes         = 2
  subnet_ids              = module.vpc.private_subnets
  security_group_ids      = [aws_security_group.erlmcp_redis.id]

  automatic_failover_enabled = true

  tags = {
    Environment = var.environment
  }
}

# Security group for Redis
resource "aws_security_group" "erlmcp_redis" {
  name        = "erlmcp-${var.environment}-redis"
  description = "Allow ErlMCP cluster to access Redis"
  vpc_id      = module.vpc.vpc_id

  ingress {
    from_port   = 6379
    to_port     = 6379
    protocol    = "tcp"
    cidr_blocks = module.vpc.private_subnets
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

# S3 for artifacts storage
module "s3" {
  source  = "terraform-aws-modules/s3/aws"
  version = "~> 3.0"

  bucket = "erlmcp-${var.environment}-${random_string.suffix.result}"
  acl    = "private"

  versioning = {
    enabled = true
  }

  server_side_encryption_configuration = {
    rule = {
      apply_server_side_encryption_by_default = {
        sse_algorithm = "AES256"
      }
    }
  }

  tags = {
    Environment = var.environment
  }
}

# IAM roles for EKS
data "aws_iam_policy_document" "erlmcp_nodes" {
  statement {
    actions = ["sts:AssumeRole"]

    principals {
      type        = "Service"
      identifiers = ["ec2.amazonaws.com"]
    }
  }
}

resource "aws_iam_role" "erlmcp_nodes" {
  name               = "erlmcp-${var.environment}-nodes"
  assume_role_policy = data.aws_iam_policy_document.erlmcp_nodes.json
}

resource "aws_iam_role_policy_attachment" "erlmcp_nodes_AmazonEKSWorkerNodePolicy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSWorkerNodePolicy"
  role       = aws_iam_role.erlmcp_nodes.name
}

resource "aws_iam_role_policy_attachment" "erlmcp_nodes_AmazonEKS_CNI_Policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKS_CNI_Policy"
  role       = aws_iam_role.erlmcp_nodes.name
}

resource "aws_iam_role_policy_attachment" "erlmcp_nodes_AmazonEC2ContainerRegistryReadOnly" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryReadOnly"
  role       = aws_iam_role.erlmcp_nodes.name
}

# Kubernetes provider configuration
provider "kubernetes" {
  host                   = module.eks.cluster_endpoint
  cluster_ca_certificate = base64decode(module.eks.cluster_certificate_authority[0].data)
  token                  = data.aws_eks_cluster_auth.this.token

  exec {
    api_version = "client.authentication.k8s.io/v1beta1"
    command     = "aws"
    args        = ["eks", "get-token", "--cluster-name", module.eks.cluster_name, "--region", var.aws_region]
  }
}

data "aws_eks_cluster_auth" "this" {
  name = module.eks.cluster_name
}

# Helm release for ErlMCP
resource "helm_release" "erlmcp" {
  name       = "erlmcp"
  repository = "https://charts.erlmcp.io"
  chart      = "erlmcp"
  namespace  = "erlmcp-${var.environment}"

  values = [
    templatefile("${path.module}/values.yaml", {
      environment           = var.environment
      image_registry       = var.image_registry
      image_tag            = var.image_tag
      db_host              = module.rds.this_db_instance_address
      db_port              = module.rds.this_db_instance_port
      db_name              = module.rds.this_db_instance_name
      db_user              = module.rds.this_db_instance_username
      redis_host           = module.elasticache.primary_endpoint_address
      redis_port           = module.elasticache.primary_endpoint_port
      s3_bucket            = module.s3.this_s3_bucket_id
    })
  ]

  set {
    name  = "service.type"
    value = "LoadBalancer"
  }

  set {
    name  = "service.annotations.service\\.beta\\.kubernetes\\.io/aws-load-balancer-type"
    value = "nlb"
  }

  set {
    name  = "resources.requests.memory"
    value = "256Mi"
  }

  set {
    name  = "resources.requests.cpu"
    value = "250m"
  }

  set {
    name  = "resources.limits.memory"
    value = "512Mi"
  }

  set {
    name  = "resources.limits.cpu"
    value = "500m"
  }

  depends_on = [
    module.rds,
    module.elasticache,
    module.s3
  ]
}

# Outputs
output "cluster_endpoint" {
  description = "EKS cluster endpoint"
  value       = module.eks.cluster_endpoint
}

output "cluster_name" {
  description = "EKS cluster name"
  value       = module.eks.cluster_name
}

output "rds_endpoint" {
  description = "RDS endpoint"
  value       = module.rds.this_db_instance_address
}

output "redis_endpoint" {
  description = "Redis primary endpoint"
  value       = module.elasticache.primary_endpoint_address
}

output "load_balancer_dns" {
  description = "Application load balancer DNS name"
  value       = helm_release.erlmcp.status[0].load_balancer_ingress[0].hostname
}

output "s3_bucket_name" {
  description = "S3 bucket name for artifacts"
  value       = module.s3.this_s3_bucket_id
}