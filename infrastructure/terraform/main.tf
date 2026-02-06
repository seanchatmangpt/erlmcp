# ErlMCP v3 Infrastructure as Code
# Terraform configuration for enterprise deployment

terraform {
  required_version = ">= 1.9.0"

  # Backend configuration for remote state management
  # Uncomment and configure for production use
  # backend "s3" {
  #   bucket         = "erlmcp-terraform-state"
  #   key            = "infrastructure/main/terraform.tfstate"
  #   region         = "us-east-1"
  #   encrypt        = true
  #   dynamodb_table = "erlmcp-terraform-locks"
  #   kms_key_id     = "alias/terraform-state-key"
  # }

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.80"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.35"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.16"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.6"
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

# VPC Configuration with enhanced networking
module "vpc" {
  source  = "terraform-aws-modules/vpc/aws"
  version = "~> 5.16"

  name = "erlmcp-vpc-${var.environment}-${random_string.suffix.result}"
  cidr = "10.0.0.0/16"

  azs             = slice(data.aws_availability_zones.available.names, 0, 3)
  private_subnets = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
  public_subnets  = ["10.0.101.0/24", "10.0.102.0/24", "10.0.103.0/24"]
  database_subnets = ["10.0.21.0/24", "10.0.22.0/24", "10.0.23.0/24"]

  enable_nat_gateway   = true
  enable_dns_hostnames = true
  enable_dns_support   = true
  single_nat_gateway   = var.environment != "production"

  enable_flow_log                      = true
  create_flow_log_cloudwatch_iam_role  = true
  create_flow_log_cloudwatch_log_group = true
  flow_log_retention_in_days           = 30

  # VPC endpoint configuration
  enable_s3_endpoint       = true
  enable_dynamodb_endpoint = true

  tags = {
    "kubernetes.io/cluster/erlmcp-${var.environment}" = "shared"
    Environment                                        = var.environment
  }

  public_subnet_tags = {
    "kubernetes.io/role/elb" = "1"
  }

  private_subnet_tags = {
    "kubernetes.io/role/internal-elb" = "1"
  }
}

# EKS Cluster with modern configuration
module "eks" {
  source  = "terraform-aws-modules/eks/aws"
  version = "~> 20.31"

  cluster_name    = "erlmcp-${var.environment}"
  cluster_version = "1.31"

  vpc_id     = module.vpc.vpc_id
  subnet_ids = module.vpc.private_subnets

  cluster_endpoint_public_access  = true
  cluster_endpoint_private_access = true

  # Cluster encryption configuration
  cluster_encryption_config = {
    provider_key_arn = aws_kms_key.eks.arn
    resources        = ["secrets"]
  }

  # Enhanced logging
  cluster_enabled_log_types = ["api", "audit", "authenticator", "controllerManager", "scheduler"]

  # Cluster access management
  enable_cluster_creator_admin_permissions = true

  access_entries = {
    erlmcp_admin = {
      principal_arn = var.iam_admin_arn
      policy_associations = {
        admin = {
          policy_arn = "arn:aws:eks::aws:cluster-access-policy/AmazonEKSClusterAdminPolicy"
          access_scope = {
            type = "cluster"
          }
        }
      }
    }
  }

  # Node groups with enhanced configuration
  eks_managed_node_group_defaults = {
    instance_types = ["m6i.large"]
    disk_size      = 100
    disk_type      = "gp3"
    disk_iops      = 3000
    disk_throughput = 125
    disk_encrypted  = true
    disk_kms_key_id = aws_kms_key.eks.arn

    iam_role_additional_policies = {
      AmazonSSMManagedInstanceCore = "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore"
    }

    metadata_options = {
      http_endpoint               = "enabled"
      http_tokens                 = "required"
      http_put_response_hop_limit = 2
      instance_metadata_tags      = "enabled"
    }

    enable_monitoring = true
  }

  eks_managed_node_groups = {
    main = {
      name = "erlmcp-main-${var.environment}"

      min_size     = var.min_nodes
      max_size     = var.max_nodes
      desired_size = var.node_count

      instance_types = [var.node_instance_type]
      capacity_type  = "ON_DEMAND"

      labels = {
        Environment = var.environment
        NodeGroup   = "main"
      }

      taints = []

      tags = {
        Environment = var.environment
        NodeGroup   = "main"
      }
    }

    canary = {
      name = "erlmcp-canary-${var.environment}"

      min_size     = 1
      max_size     = 3
      desired_size = var.canary_node_count

      instance_types = ["m6i.medium"]
      capacity_type  = "SPOT"

      labels = {
        Environment = var.environment
        Purpose     = "canary"
        NodeGroup   = "canary"
      }

      taints = [{
        key    = "workload"
        value  = "canary"
        effect = "NO_SCHEDULE"
      }]

      tags = {
        Environment = var.environment
        Purpose     = "canary"
        NodeGroup   = "canary"
      }
    }
  }

  # Security groups
  cluster_security_group_additional_rules = {
    ingress_nodes_ephemeral_ports_tcp = {
      description                = "Nodes on ephemeral ports"
      protocol                   = "tcp"
      from_port                  = 1025
      to_port                    = 65535
      type                       = "ingress"
      source_node_security_group = true
    }
  }

  node_security_group_additional_rules = {
    ingress_self_all = {
      description = "Node to node all ports/protocols"
      protocol    = "-1"
      from_port   = 0
      to_port     = 0
      type        = "ingress"
      self        = true
    }
    egress_all = {
      description      = "Node all egress"
      protocol         = "-1"
      from_port        = 0
      to_port          = 0
      type             = "egress"
      cidr_blocks      = ["0.0.0.0/0"]
      ipv6_cidr_blocks = ["::/0"]
    }
  }

  tags = {
    Environment = var.environment
    Terraform   = "true"
  }
}

# KMS key for EKS encryption
resource "aws_kms_key" "eks" {
  description             = "EKS cluster encryption key for erlmcp-${var.environment}"
  deletion_window_in_days = 30
  enable_key_rotation     = true

  tags = {
    Name        = "erlmcp-${var.environment}-eks-key"
    Environment = var.environment
  }
}

resource "aws_kms_alias" "eks" {
  name          = "alias/erlmcp-${var.environment}-eks"
  target_key_id = aws_kms_key.eks.key_id
}

# RDS PostgreSQL for ErlMCP data with enhanced configuration
module "rds" {
  source  = "terraform-aws-modules/rds/aws"
  version = "~> 6.10"

  identifier = "erlmcp-${var.environment}"

  engine               = "postgres"
  engine_version       = "16.4"
  family               = "postgres16"
  major_engine_version = "16"
  instance_class       = "db.m6g.large"

  allocated_storage     = 100
  max_allocated_storage = 500
  storage_type          = "gp3"
  storage_encrypted     = true
  kms_key_id            = aws_kms_key.rds.arn
  iops                  = 3000
  storage_throughput    = 125

  db_name  = "erlmcp"
  username = "erlmcp"
  password = var.rds_password
  port     = 5432

  multi_az               = var.environment == "production"
  db_subnet_group_name   = module.vpc.database_subnet_group_name
  vpc_security_group_ids = [aws_security_group.erlmcp_rds.id]

  maintenance_window              = "sun:01:00-sun:02:00"
  backup_window                   = "03:00-04:00"
  backup_retention_period         = var.backup_retention_period
  skip_final_snapshot             = var.environment != "production"
  final_snapshot_identifier_prefix = "erlmcp-${var.environment}-final"
  copy_tags_to_snapshot           = true
  deletion_protection             = var.environment == "production"

  enabled_cloudwatch_logs_exports = ["postgresql", "upgrade"]
  create_cloudwatch_log_group     = true
  cloudwatch_log_group_retention_in_days = 30
  cloudwatch_log_group_kms_key_id = aws_kms_key.logs.arn

  performance_insights_enabled          = true
  performance_insights_retention_period = 7
  performance_insights_kms_key_id       = aws_kms_key.rds.arn

  monitoring_interval    = 60
  monitoring_role_name   = "erlmcp-${var.environment}-rds-monitoring"
  create_monitoring_role = true

  parameters = [
    {
      name  = "shared_preload_libraries"
      value = "pg_stat_statements"
    },
    {
      name  = "log_min_duration_statement"
      value = "1000"
    },
    {
      name  = "log_connections"
      value = "1"
    },
    {
      name  = "log_disconnections"
      value = "1"
    }
  ]

  tags = {
    Environment = var.environment
    Database    = "primary"
  }
}

# KMS key for RDS encryption
resource "aws_kms_key" "rds" {
  description             = "RDS encryption key for erlmcp-${var.environment}"
  deletion_window_in_days = 30
  enable_key_rotation     = true

  tags = {
    Name        = "erlmcp-${var.environment}-rds-key"
    Environment = var.environment
  }
}

resource "aws_kms_alias" "rds" {
  name          = "alias/erlmcp-${var.environment}-rds"
  target_key_id = aws_kms_key.rds.key_id
}

# KMS key for CloudWatch Logs encryption
resource "aws_kms_key" "logs" {
  description             = "CloudWatch Logs encryption key for erlmcp-${var.environment}"
  deletion_window_in_days = 30
  enable_key_rotation     = true

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid    = "Enable IAM User Permissions"
        Effect = "Allow"
        Principal = {
          AWS = "arn:aws:iam::${data.aws_caller_identity.current.account_id}:root"
        }
        Action   = "kms:*"
        Resource = "*"
      },
      {
        Sid    = "Allow CloudWatch Logs"
        Effect = "Allow"
        Principal = {
          Service = "logs.${data.aws_region.current.name}.amazonaws.com"
        }
        Action = [
          "kms:Encrypt",
          "kms:Decrypt",
          "kms:ReEncrypt*",
          "kms:GenerateDataKey*",
          "kms:CreateGrant",
          "kms:DescribeKey"
        ]
        Resource = "*"
        Condition = {
          ArnLike = {
            "kms:EncryptionContext:aws:logs:arn" = "arn:aws:logs:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:*"
          }
        }
      }
    ]
  })

  tags = {
    Name        = "erlmcp-${var.environment}-logs-key"
    Environment = var.environment
  }
}

resource "aws_kms_alias" "logs" {
  name          = "alias/erlmcp-${var.environment}-logs"
  target_key_id = aws_kms_key.logs.key_id
}

# Data sources
data "aws_caller_identity" "current" {}
data "aws_region" "current" {}
data "aws_availability_zones" "available" {
  state = "available"
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

# ElastiCache Redis cluster with replication
resource "aws_elasticache_replication_group" "redis" {
  replication_group_id       = "erlmcp-${var.environment}"
  replication_group_description = "ErlMCP Redis cluster for ${var.environment}"

  engine               = "redis"
  engine_version       = "7.1"
  node_type            = "cache.m6g.large"
  num_cache_clusters   = var.environment == "production" ? 3 : 2
  port                 = 6379
  parameter_group_name = aws_elasticache_parameter_group.redis.name

  subnet_group_name  = aws_elasticache_subnet_group.redis.name
  security_group_ids = [aws_security_group.erlmcp_redis.id]

  at_rest_encryption_enabled = true
  kms_key_id                 = aws_kms_key.redis.arn
  transit_encryption_enabled = true
  auth_token_enabled         = false

  automatic_failover_enabled = true
  multi_az_enabled          = var.environment == "production"

  snapshot_retention_limit = var.environment == "production" ? 7 : 3
  snapshot_window          = "03:00-05:00"
  maintenance_window       = "sun:05:00-sun:07:00"

  auto_minor_version_upgrade = true
  apply_immediately          = var.environment != "production"

  log_delivery_configuration {
    destination      = aws_cloudwatch_log_group.redis_slow_log.name
    destination_type = "cloudwatch-logs"
    log_format       = "json"
    log_type         = "slow-log"
  }

  log_delivery_configuration {
    destination      = aws_cloudwatch_log_group.redis_engine_log.name
    destination_type = "cloudwatch-logs"
    log_format       = "json"
    log_type         = "engine-log"
  }

  tags = {
    Name        = "erlmcp-${var.environment}-redis"
    Environment = var.environment
  }
}

resource "aws_elasticache_subnet_group" "redis" {
  name       = "erlmcp-${var.environment}-redis"
  subnet_ids = module.vpc.private_subnets

  tags = {
    Name        = "erlmcp-${var.environment}-redis-subnet-group"
    Environment = var.environment
  }
}

resource "aws_elasticache_parameter_group" "redis" {
  name   = "erlmcp-${var.environment}-redis-params"
  family = "redis7"

  parameter {
    name  = "maxmemory-policy"
    value = "allkeys-lru"
  }

  parameter {
    name  = "timeout"
    value = "300"
  }

  tags = {
    Name        = "erlmcp-${var.environment}-redis-params"
    Environment = var.environment
  }
}

resource "aws_cloudwatch_log_group" "redis_slow_log" {
  name              = "/aws/elasticache/erlmcp-${var.environment}/slow-log"
  retention_in_days = 30
  kms_key_id        = aws_kms_key.logs.arn

  tags = {
    Name        = "erlmcp-${var.environment}-redis-slow-log"
    Environment = var.environment
  }
}

resource "aws_cloudwatch_log_group" "redis_engine_log" {
  name              = "/aws/elasticache/erlmcp-${var.environment}/engine-log"
  retention_in_days = 30
  kms_key_id        = aws_kms_key.logs.arn

  tags = {
    Name        = "erlmcp-${var.environment}-redis-engine-log"
    Environment = var.environment
  }
}

# KMS key for Redis encryption
resource "aws_kms_key" "redis" {
  description             = "Redis encryption key for erlmcp-${var.environment}"
  deletion_window_in_days = 30
  enable_key_rotation     = true

  tags = {
    Name        = "erlmcp-${var.environment}-redis-key"
    Environment = var.environment
  }
}

resource "aws_kms_alias" "redis" {
  name          = "alias/erlmcp-${var.environment}-redis"
  target_key_id = aws_kms_key.redis.key_id
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

# S3 for artifacts storage with enhanced security
module "s3_artifacts" {
  source  = "terraform-aws-modules/s3-bucket/aws"
  version = "~> 4.2"

  bucket = "erlmcp-${var.environment}-artifacts-${random_string.suffix.result}"

  versioning = {
    enabled = true
  }

  server_side_encryption_configuration = {
    rule = {
      apply_server_side_encryption_by_default = {
        sse_algorithm     = "aws:kms"
        kms_master_key_id = aws_kms_key.s3.arn
      }
      bucket_key_enabled = true
    }
  }

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true

  lifecycle_rule = [
    {
      id      = "archive-old-versions"
      enabled = true

      noncurrent_version_transition = [
        {
          noncurrent_days = 30
          storage_class   = "STANDARD_IA"
        },
        {
          noncurrent_days = 90
          storage_class   = "GLACIER_IR"
        }
      ]

      noncurrent_version_expiration = {
        noncurrent_days = 365
      }
    }
  ]

  logging = {
    target_bucket = module.s3_logs.s3_bucket_id
    target_prefix = "artifacts-logs/"
  }

  tags = {
    Name        = "erlmcp-${var.environment}-artifacts"
    Environment = var.environment
  }
}

# S3 bucket for logs
module "s3_logs" {
  source  = "terraform-aws-modules/s3-bucket/aws"
  version = "~> 4.2"

  bucket = "erlmcp-${var.environment}-logs-${random_string.suffix.result}"

  versioning = {
    enabled = true
  }

  server_side_encryption_configuration = {
    rule = {
      apply_server_side_encryption_by_default = {
        sse_algorithm     = "aws:kms"
        kms_master_key_id = aws_kms_key.s3.arn
      }
      bucket_key_enabled = true
    }
  }

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true

  lifecycle_rule = [
    {
      id      = "expire-old-logs"
      enabled = true

      expiration = {
        days = 90
      }

      noncurrent_version_expiration = {
        noncurrent_days = 30
      }
    }
  ]

  tags = {
    Name        = "erlmcp-${var.environment}-logs"
    Environment = var.environment
  }
}

# KMS key for S3 encryption
resource "aws_kms_key" "s3" {
  description             = "S3 encryption key for erlmcp-${var.environment}"
  deletion_window_in_days = 30
  enable_key_rotation     = true

  tags = {
    Name        = "erlmcp-${var.environment}-s3-key"
    Environment = var.environment
  }
}

resource "aws_kms_alias" "s3" {
  name          = "alias/erlmcp-${var.environment}-s3"
  target_key_id = aws_kms_key.s3.key_id
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

# Kubernetes provider configuration with v1 authentication API
provider "kubernetes" {
  host                   = module.eks.cluster_endpoint
  cluster_ca_certificate = base64decode(module.eks.cluster_certificate_authority_data)

  exec {
    api_version = "client.authentication.k8s.io/v1"
    command     = "aws"
    args        = ["eks", "get-token", "--cluster-name", module.eks.cluster_name, "--region", var.aws_region]
  }
}

# Helm provider configuration
provider "helm" {
  kubernetes {
    host                   = module.eks.cluster_endpoint
    cluster_ca_certificate = base64decode(module.eks.cluster_certificate_authority_data)

    exec {
      api_version = "client.authentication.k8s.io/v1"
      command     = "aws"
      args        = ["eks", "get-token", "--cluster-name", module.eks.cluster_name, "--region", var.aws_region]
    }
  }
}

# Create Kubernetes namespace
resource "kubernetes_namespace" "erlmcp" {
  metadata {
    name = "erlmcp-${var.environment}"

    labels = {
      "app.kubernetes.io/name"       = "erlmcp"
      "app.kubernetes.io/managed-by" = "terraform"
      "environment"                  = var.environment
    }

    annotations = {
      "meta.helm.sh/release-name"      = "erlmcp"
      "meta.helm.sh/release-namespace" = "erlmcp-${var.environment}"
    }
  }
}

# Helm release for ErlMCP (commented out - requires values.yaml file)
# Uncomment when deploying the application
# resource "helm_release" "erlmcp" {
#   name       = "erlmcp"
#   repository = "https://charts.erlmcp.io"
#   chart      = "erlmcp"
#   namespace  = kubernetes_namespace.erlmcp.metadata[0].name
#   create_namespace = false
#
#   values = [
#     templatefile("${path.module}/values.yaml.tpl", {
#       environment  = var.environment
#       image_registry = var.image_registry
#       image_tag    = var.image_tag
#       db_host      = module.rds.db_instance_address
#       db_port      = module.rds.db_instance_port
#       db_name      = module.rds.db_instance_name
#       db_user      = module.rds.db_instance_username
#       redis_host   = aws_elasticache_replication_group.redis.primary_endpoint_address
#       redis_port   = aws_elasticache_replication_group.redis.port
#       s3_bucket    = module.s3_artifacts.s3_bucket_id
#     })
#   ]
#
#   set {
#     name  = "service.type"
#     value = "LoadBalancer"
#   }
#
#   set {
#     name  = "service.annotations.service\\.beta\\.kubernetes\\.io/aws-load-balancer-type"
#     value = "external"
#   }
#
#   set {
#     name  = "service.annotations.service\\.beta\\.kubernetes\\.io/aws-load-balancer-scheme"
#     value = "internet-facing"
#   }
#
#   set {
#     name  = "resources.requests.memory"
#     value = "256Mi"
#   }
#
#   set {
#     name  = "resources.requests.cpu"
#     value = "250m"
#   }
#
#   set {
#     name  = "resources.limits.memory"
#     value = "512Mi"
#   }
#
#   set {
#     name  = "resources.limits.cpu"
#     value = "500m"
#   }
#
#   depends_on = [
#     module.rds,
#     aws_elasticache_replication_group.redis,
#     module.s3_artifacts
#   ]
# }

# Outputs
output "cluster_endpoint" {
  description = "EKS cluster endpoint"
  value       = module.eks.cluster_endpoint
}

output "cluster_name" {
  description = "EKS cluster name"
  value       = module.eks.cluster_name
}

output "cluster_security_group_id" {
  description = "Security group ID attached to the EKS cluster"
  value       = module.eks.cluster_security_group_id
}

output "cluster_oidc_issuer_url" {
  description = "The URL on the EKS cluster OIDC Issuer"
  value       = module.eks.cluster_oidc_issuer_url
}

output "cluster_certificate_authority_data" {
  description = "Base64 encoded certificate data required to communicate with the cluster"
  value       = module.eks.cluster_certificate_authority_data
  sensitive   = true
}

output "rds_endpoint" {
  description = "RDS endpoint"
  value       = module.rds.db_instance_address
}

output "rds_port" {
  description = "RDS port"
  value       = module.rds.db_instance_port
}

output "redis_primary_endpoint" {
  description = "Redis primary endpoint"
  value       = aws_elasticache_replication_group.redis.primary_endpoint_address
}

output "redis_reader_endpoint" {
  description = "Redis reader endpoint"
  value       = aws_elasticache_replication_group.redis.reader_endpoint_address
}

output "redis_port" {
  description = "Redis port"
  value       = aws_elasticache_replication_group.redis.port
}

output "s3_artifacts_bucket_name" {
  description = "S3 bucket name for artifacts"
  value       = module.s3_artifacts.s3_bucket_id
}

output "s3_artifacts_bucket_arn" {
  description = "S3 bucket ARN for artifacts"
  value       = module.s3_artifacts.s3_bucket_arn
}

output "s3_logs_bucket_name" {
  description = "S3 bucket name for logs"
  value       = module.s3_logs.s3_bucket_id
}

output "vpc_id" {
  description = "VPC ID"
  value       = module.vpc.vpc_id
}

output "private_subnets" {
  description = "List of IDs of private subnets"
  value       = module.vpc.private_subnets
}

output "public_subnets" {
  description = "List of IDs of public subnets"
  value       = module.vpc.public_subnets
}

output "database_subnets" {
  description = "List of IDs of database subnets"
  value       = module.vpc.database_subnets
}

output "kms_keys" {
  description = "KMS key IDs for different services"
  value = {
    eks   = aws_kms_key.eks.id
    rds   = aws_kms_key.rds.id
    redis = aws_kms_key.redis.id
    s3    = aws_kms_key.s3.id
    logs  = aws_kms_key.logs.id
  }
}