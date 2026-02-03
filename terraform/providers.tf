# Configure AWS provider
provider "aws" {
  region = var.aws_region

  # Use default credentials chain if not provided
  dynamic "assume_role" {
    for_each = var.aws_access_key_id != "" && var.aws_secret_access_key != "" ? [] : [1]
    content {
      role_arn = "arn:aws:iam::${data.aws_caller_identity.account_id}:role/erlmcp-deployment-role"
    }
  }
}

# Configure Kubernetes provider for EKS
provider "kubernetes" {
  host                   = data.aws_eks_cluster.cluster.endpoint
  cluster_ca_certificate = base64decode(data.aws_eks_cluster_cluster_auth.cluster.certificate_authority[0].data)

  dynamic "exec" {
    for_each = [1]
    content {
      api_version = "client.authentication.k8s.io/v1beta1"
      args        = ["eks", "get-token", "--cluster-name", local.cluster_identifier, "--region", var.aws_region]
      command     = "aws"
    }
  }
}

# Configure Kubernetes alpha provider for advanced features
provider "kubernetes-alpha" {
  host                   = data.aws_eks_cluster.cluster.endpoint
  cluster_ca_certificate = base64decode(data.aws_eks_cluster_cluster_auth.cluster.certificate_authority[0].data)

  dynamic "exec" {
    for_each = [1]
    content {
      api_version = "client.authentication.k8s.io/v1beta1"
      args        = ["eks", "get-token", "--cluster-name", local.cluster_identifier, "--region", var.aws_region]
      command     = "aws"
    }
  }
}

# Configure Helm provider
provider "helm" {
  kubernetes {
    host                   = data.aws_eks_cluster.cluster.endpoint
    cluster_ca_certificate = base64decode(data.aws_eks_cluster_cluster_auth.cluster.certificate_authority[0].data)

    dynamic "exec" {
      for_each = [1]
      content {
        api_version = "client.authentication.k8s.io/v1beta1"
        args        = ["eks", "get-token", "--cluster-name", local.cluster_identifier, "--region", var.aws_region]
        command     = "aws"
      }
    }
  }
}

# Configure random provider for unique strings
provider "random" {}