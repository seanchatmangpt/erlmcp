# Get AWS account ID
data "aws_caller_identity" "account_id" {}

# Get available availability zones
data "aws_availability_zones" "available" {
  state = "available"
}

# Get current AWS partition
data "aws_partition" "current" {}

# Get default VPC
data "aws_vpc" "default" {
  default = true
}

# Get default subnets
data "aws_subnets" "default" {
  filter {
    name   = "vpc-id"
    values = [data.aws_vpc.default.id]
  }
}

# Get current region
data "aws_region" "current" {}

# Get EKS cluster info
data "aws_eks_cluster" "cluster" {
  name = local.cluster_identifier
}

# Get EKS cluster auth data
data "aws_eks_cluster_auth" "cluster" {
  name = local.cluster_identifier
}

# Get OIDC provider
data "tls_certificate" "eks" {
  url = aws_eks_cluster.cluster.identity[0].oidc[0].issuer
}