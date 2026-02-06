# Get AWS account ID
data "aws_caller_identity" "current" {}

# Get available availability zones
data "aws_availability_zones" "available" {
  state = "available"

  filter {
    name   = "opt-in-status"
    values = ["opt-in-not-required"]
  }
}

# Get current AWS partition
data "aws_partition" "current" {}

# Get default VPC (optional, for reference only)
data "aws_vpc" "default" {
  default = true
}

# Get default subnets (optional, for reference only)
data "aws_subnets" "default" {
  filter {
    name   = "vpc-id"
    values = [data.aws_vpc.default.id]
  }
}

# Get current region
data "aws_region" "current" {}

# Get EKS cluster info (only after cluster is created)
data "aws_eks_cluster" "cluster" {
  count = var.create_eks_cluster ? 1 : 0
  name  = local.cluster_identifier

  depends_on = [aws_eks_cluster.cluster]
}

# Get EKS cluster auth data (only after cluster is created)
data "aws_eks_cluster_auth" "cluster" {
  count = var.create_eks_cluster ? 1 : 0
  name  = local.cluster_identifier

  depends_on = [aws_eks_cluster.cluster]
}

# Get OIDC provider certificate
data "tls_certificate" "eks" {
  url = aws_eks_cluster.cluster.identity[0].oidc[0].issuer

  depends_on = [aws_eks_cluster.cluster]
}

# Get latest EKS optimized AMI
data "aws_ami" "eks_node" {
  most_recent = true
  owners      = ["amazon"]

  filter {
    name   = "name"
    values = ["amazon-eks-node-${var.eks_version}-v*"]
  }

  filter {
    name   = "architecture"
    values = ["x86_64"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }
}

# Get SSM parameter for EKS AMI ID (alternative method)
data "aws_ssm_parameter" "eks_ami_id" {
  name = "/aws/service/eks/optimized-ami/${var.eks_version}/amazon-linux-2/recommended/image_id"
}