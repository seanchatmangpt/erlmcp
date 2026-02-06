# VPC Configuration
resource "aws_vpc" "main" {
  cidr_block           = var.vpc_cidr
  enable_dns_support   = true
  enable_dns_hostnames = true

  tags = merge(local.tags, {
    Name = "${var.environment}-vpc"
  })
}

# Internet Gateway
resource "aws_internet_gateway" "gw" {
  vpc_id = aws_vpc.main.id

  tags = merge(local.tags, {
    Name = "${var.environment}-igw"
  })
}

# NAT Gateway for private subnets
resource "aws_nat_gateway" "main" {
  allocation_id = aws_eip.nat[0].id
  subnet_id     = aws_subnet.public[0].id

  tags = merge(local.tags, {
    Name = "${var.environment}-nat"
  })

  depends_on = [aws_internet_gateway.gw]
}

# Elastic IP for NAT Gateway
resource "aws_eip" "nat" {
  domain = "vpc"

  tags = merge(local.tags, {
    Name = "${var.environment}-eip"
  })
}

# Public Subnets
resource "aws_subnet" "public" {
  count             = length(local.availability_zones)
  vpc_id            = aws_vpc.main.id
  cidr_block        = var.subnet_cidrs[count.index]
  availability_zone = local.availability_zones[count.index]

  tags = merge(local.tags, {
    Name = "${var.environment}-public-${count.index + 1}",
    Tier = "public"
  })

  map_public_ip_on_launch = true
}

# Private Subnets
resource "aws_subnet" "private" {
  count             = length(local.availability_zones)
  vpc_id            = aws_vpc.main.id
  cidr_block        = cidrsubnet(aws_vpc.main.cidr_block, 8, count.index + 3)
  availability_zone = local.availability_zones[count.index]

  tags = merge(local.tags, {
    Name = "${var.environment}-private-${count.index + 1}",
    Tier = "private"
  })
}

# Database Subnets
resource "aws_subnet" "database" {
  count             = length(local.availability_zones)
  vpc_id            = aws_vpc.main.id
  cidr_block        = cidrsubnet(aws_vpc.main.cidr_block, 8, count.index + 6)
  availability_zone = local.availability_zones[count.index]

  tags = merge(local.tags, {
    Name = "${var.environment}-database-${count.index + 1}",
    Tier = "database"
  })
}

# Route Tables
resource "aws_route_table" "public" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.gw.id
  }

  tags = merge(local.tags, {
    Name = "${var.environment}-public-rtb"
  })
}

resource "aws_route_table" "private" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.main.id
  }

  tags = merge(local.tags, {
    Name = "${var.environment}-private-rtb"
  })
}

resource "aws_route_table" "database" {
  vpc_id = aws_vpc.main.id

  tags = merge(local.tags, {
    Name = "${var.environment}-database-rtb"
  })
}

# Route Table Associations
resource "aws_route_table_association" "public" {
  count          = length(aws_subnet.public)
  subnet_id      = aws_subnet.public[count.index].id
  route_table_id = aws_route_table.public.id
}

resource "aws_route_table_association" "private" {
  count          = length(aws_subnet.private)
  subnet_id      = aws_subnet.private[count.index].id
  route_table_id = aws_route_table.private.id
}

resource "aws_route_table_association" "database" {
  count          = length(aws_subnet.database)
  subnet_id      = aws_subnet.database[count.index].id
  route_table_id = aws_route_table.database.id
}

# Security Groups
resource "aws_security_group" "cluster" {
  name        = local.security_groups.cluster
  description = "Security group for EKS cluster control plane"
  vpc_id      = aws_vpc.main.id

  tags = local.tags
}

resource "aws_security_group" "nodes" {
  name        = local.security_groups.nodes
  description = "Security group for EKS worker nodes"
  vpc_id      = aws_vpc.main.id

  tags = merge(local.tags, {
    "kubernetes.io/cluster/${local.cluster_identifier}" = "owned"
  })
}

# Node to node communication
resource "aws_security_group_rule" "nodes_internal" {
  description       = "Allow nodes to communicate with each other"
  type              = "ingress"
  security_group_id = aws_security_group.nodes.id
  from_port         = 0
  to_port           = 65535
  protocol          = "-1"
  self              = true
}

# Node egress to cluster
resource "aws_security_group_rule" "nodes_egress_cluster" {
  description              = "Allow nodes to communicate with cluster API"
  type                     = "egress"
  security_group_id        = aws_security_group.nodes.id
  from_port                = 443
  to_port                  = 443
  protocol                 = "tcp"
  source_security_group_id = aws_security_group.cluster.id
}

# Node egress to internet
resource "aws_security_group_rule" "nodes_egress_internet" {
  description       = "Allow nodes to communicate with internet"
  type              = "egress"
  security_group_id = aws_security_group.nodes.id
  from_port         = 0
  to_port           = 0
  protocol          = "-1"
  cidr_blocks       = ["0.0.0.0/0"]
}

# Cluster to node communication
resource "aws_security_group_rule" "nodes_ingress_cluster" {
  description              = "Allow cluster to communicate with nodes"
  type                     = "ingress"
  security_group_id        = aws_security_group.nodes.id
  from_port                = 1025
  to_port                  = 65535
  protocol                 = "tcp"
  source_security_group_id = aws_security_group.cluster.id
}

# Node ingress from ALB
resource "aws_security_group_rule" "nodes_ingress_alb" {
  description              = "Allow ALB to communicate with nodes"
  type                     = "ingress"
  security_group_id        = aws_security_group.nodes.id
  from_port                = 30000
  to_port                  = 32767
  protocol                 = "tcp"
  source_security_group_id = aws_security_group.alb.id
}

resource "aws_security_group_rule" "cluster_ingress_https" {
  description       = "Allow HTTPS traffic from VPC"
  type              = "ingress"
  security_group_id = aws_security_group.cluster.id
  from_port         = 443
  to_port           = 443
  protocol          = "tcp"
  cidr_blocks       = [aws_vpc.main.cidr_block]
}

resource "aws_security_group_rule" "cluster_ingress_node_https" {
  description              = "Allow HTTPS traffic from nodes"
  type                     = "ingress"
  security_group_id        = aws_security_group.cluster.id
  from_port                = 443
  to_port                  = 443
  protocol                 = "tcp"
  source_security_group_id = aws_security_group.nodes.id
}

resource "aws_security_group_rule" "cluster_egress_nodes" {
  description              = "Allow cluster to communicate with nodes"
  type                     = "egress"
  security_group_id        = aws_security_group.cluster.id
  from_port                = 1025
  to_port                  = 65535
  protocol                 = "tcp"
  source_security_group_id = aws_security_group.nodes.id
}

# Load Balancer Security Group
resource "aws_security_group" "alb" {
  name        = local.security_groups.load_balancer
  description = "Security group for Application Load Balancer"
  vpc_id      = aws_vpc.main.id

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = local.tags
}

# Network ACLs for enhanced security
resource "aws_network_acl" "public" {
  vpc_id     = aws_vpc.main.id
  subnet_ids = aws_subnet.public[*].id

  ingress {
    rule_no    = 100
    action     = "allow"
    from_port  = 80
    to_port    = 80
    protocol   = "tcp"
    cidr_block = "0.0.0.0/0"
  }

  ingress {
    rule_no    = 110
    action     = "allow"
    from_port  = 443
    to_port    = 443
    protocol   = "tcp"
    cidr_block = "0.0.0.0/0"
  }

  ingress {
    rule_no    = 120
    action     = "allow"
    from_port  = 1024
    to_port    = 65535
    protocol   = "tcp"
    cidr_block = "0.0.0.0/0"
  }

  egress {
    rule_no    = 100
    action     = "allow"
    from_port  = 0
    to_port    = 65535
    protocol   = "tcp"
    cidr_block = "0.0.0.0/0"
  }

  tags = merge(local.tags, {
    Name = "${var.environment}-public-nacl"
  })
}

resource "aws_network_acl" "private" {
  vpc_id     = aws_vpc.main.id
  subnet_ids = aws_subnet.private[*].id

  ingress {
    rule_no    = 100
    action     = "allow"
    from_port  = 0
    to_port    = 65535
    protocol   = "-1"
    cidr_block = aws_vpc.main.cidr_block
  }

  ingress {
    rule_no    = 110
    action     = "allow"
    from_port  = 1024
    to_port    = 65535
    protocol   = "tcp"
    cidr_block = "0.0.0.0/0"
  }

  egress {
    rule_no    = 100
    action     = "allow"
    from_port  = 0
    to_port    = 65535
    protocol   = "-1"
    cidr_block = "0.0.0.0/0"
  }

  tags = merge(local.tags, {
    Name = "${var.environment}-private-nacl"
  })
}

resource "aws_network_acl" "database" {
  vpc_id     = aws_vpc.main.id
  subnet_ids = aws_subnet.database[*].id

  ingress {
    rule_no    = 100
    action     = "allow"
    from_port  = 5432
    to_port    = 5432
    protocol   = "tcp"
    cidr_block = aws_vpc.main.cidr_block
  }

  ingress {
    rule_no    = 110
    action     = "allow"
    from_port  = 1024
    to_port    = 65535
    protocol   = "tcp"
    cidr_block = aws_vpc.main.cidr_block
  }

  egress {
    rule_no    = 100
    action     = "allow"
    from_port  = 0
    to_port    = 65535
    protocol   = "tcp"
    cidr_block = aws_vpc.main.cidr_block
  }

  tags = merge(local.tags, {
    Name = "${var.environment}-database-nacl"
  })
}

# KMS key for S3 encryption
resource "aws_kms_key" "s3" {
  description             = "KMS key for S3 bucket encryption"
  deletion_window_in_days = 30
  enable_key_rotation     = true

  tags = merge(local.tags, {
    Name = "${var.environment}-s3-key"
  })
}

resource "aws_kms_alias" "s3" {
  name          = "alias/${var.environment}-s3-encryption"
  target_key_id = aws_kms_key.s3.key_id
}

# S3 Bucket for artifacts with enhanced security
resource "aws_s3_bucket" "artifacts" {
  bucket = local.s3_bucket_name

  tags = local.tags
}

resource "aws_s3_bucket_versioning" "artifacts" {
  bucket = aws_s3_bucket.artifacts.id
  versioning_configuration {
    status = "Enabled"
  }
}

resource "aws_s3_bucket_server_side_encryption_configuration" "artifacts" {
  bucket = aws_s3_bucket.artifacts.id

  rule {
    apply_server_side_encryption_by_default {
      sse_algorithm     = "aws:kms"
      kms_master_key_id = aws_kms_key.s3.arn
    }
    bucket_key_enabled = true
  }
}

resource "aws_s3_bucket_public_access_block" "artifacts" {
  bucket                  = aws_s3_bucket.artifacts.id
  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

resource "aws_s3_bucket_lifecycle_configuration" "artifacts" {
  bucket = aws_s3_bucket.artifacts.id

  rule {
    id     = "archive-old-versions"
    status = "Enabled"

    noncurrent_version_transition {
      noncurrent_days = 30
      storage_class   = "STANDARD_IA"
    }

    noncurrent_version_transition {
      noncurrent_days = 90
      storage_class   = "GLACIER"
    }

    noncurrent_version_expiration {
      noncurrent_days = 365
    }
  }
}

# VPC Endpoints for AWS Services
resource "aws_vpc_endpoint" "s3" {
  vpc_id       = aws_vpc.main.id
  service_name = "com.amazonaws.${data.aws_region.current.name}.s3"
  vpc_endpoint_type = "Gateway"

  route_table_ids = [
    aws_route_table.private.id,
    aws_route_table.database.id
  ]

  tags = merge(local.tags, {
    Name = "${var.environment}-s3-endpoint"
  })
}

resource "aws_vpc_endpoint" "ecr_api" {
  vpc_id              = aws_vpc.main.id
  service_name        = "com.amazonaws.${data.aws_region.current.name}.ecr.api"
  vpc_endpoint_type   = "Interface"
  private_dns_enabled = true

  subnet_ids         = aws_subnet.private[*].id
  security_group_ids = [aws_security_group.vpc_endpoints.id]

  tags = merge(local.tags, {
    Name = "${var.environment}-ecr-api-endpoint"
  })
}

resource "aws_vpc_endpoint" "ecr_dkr" {
  vpc_id              = aws_vpc.main.id
  service_name        = "com.amazonaws.${data.aws_region.current.name}.ecr.dkr"
  vpc_endpoint_type   = "Interface"
  private_dns_enabled = true

  subnet_ids         = aws_subnet.private[*].id
  security_group_ids = [aws_security_group.vpc_endpoints.id]

  tags = merge(local.tags, {
    Name = "${var.environment}-ecr-dkr-endpoint"
  })
}

resource "aws_vpc_endpoint" "ec2" {
  vpc_id              = aws_vpc.main.id
  service_name        = "com.amazonaws.${data.aws_region.current.name}.ec2"
  vpc_endpoint_type   = "Interface"
  private_dns_enabled = true

  subnet_ids         = aws_subnet.private[*].id
  security_group_ids = [aws_security_group.vpc_endpoints.id]

  tags = merge(local.tags, {
    Name = "${var.environment}-ec2-endpoint"
  })
}

resource "aws_vpc_endpoint" "logs" {
  vpc_id              = aws_vpc.main.id
  service_name        = "com.amazonaws.${data.aws_region.current.name}.logs"
  vpc_endpoint_type   = "Interface"
  private_dns_enabled = true

  subnet_ids         = aws_subnet.private[*].id
  security_group_ids = [aws_security_group.vpc_endpoints.id]

  tags = merge(local.tags, {
    Name = "${var.environment}-logs-endpoint"
  })
}

resource "aws_vpc_endpoint" "sts" {
  vpc_id              = aws_vpc.main.id
  service_name        = "com.amazonaws.${data.aws_region.current.name}.sts"
  vpc_endpoint_type   = "Interface"
  private_dns_enabled = true

  subnet_ids         = aws_subnet.private[*].id
  security_group_ids = [aws_security_group.vpc_endpoints.id]

  tags = merge(local.tags, {
    Name = "${var.environment}-sts-endpoint"
  })
}

# Security group for VPC endpoints
resource "aws_security_group" "vpc_endpoints" {
  name        = "${var.environment}-vpc-endpoints"
  description = "Security group for VPC endpoints"
  vpc_id      = aws_vpc.main.id

  ingress {
    description = "HTTPS from VPC"
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = [aws_vpc.main.cidr_block]
  }

  egress {
    description = "Allow all outbound"
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = merge(local.tags, {
    Name = "${var.environment}-vpc-endpoints-sg"
  })
}

# IAM Roles
resource "aws_iam_role" "eks_cluster" {
  name = "erlmcp-${var.environment}-eks-cluster-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Principal = {
          Service = "eks.amazonaws.com"
        }
        Action = "sts:AssumeRole"
      }
    ]
  })

  tags = local.tags
}

resource "aws_iam_role" "nodes" {
  name = "erlmcp-${var.environment}-eks-node-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Principal = {
          Service = "ec2.amazonaws.com"
        }
        Action = "sts:AssumeRole"
      }
    ]
  })

  tags = local.tags
}

# IAM Policies for cluster
resource "aws_iam_role_policy_attachment" "cluster_AmazonEKSClusterPolicy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSClusterPolicy"
  role       = aws_iam_role.eks_cluster.name
}

# IAM Policies for nodes
resource "aws_iam_role_policy_attachment" "nodes_AmazonEKSWorkerNodePolicy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSWorkerNodePolicy"
  role       = aws_iam_role.nodes.name
}

resource "aws_iam_role_policy_attachment" "nodes_AmazonEKS_CNI_Policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKS_CNI_Policy"
  role       = aws_iam_role.nodes.name
}

resource "aws_iam_role_policy_attachment" "nodes_AmazonEC2ContainerRegistryReadOnly" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryReadOnly"
  role       = aws_iam_role.nodes.name
}