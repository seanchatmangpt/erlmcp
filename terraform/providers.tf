# Configure AWS provider with enhanced security and tagging
provider "aws" {
  region = var.aws_region

  default_tags {
    tags = {
      Project     = "erlmcp"
      ManagedBy   = "Terraform"
      Environment = var.environment
      Version     = "3.0.0"
      Repository  = "github.com/erlmcp/erlmcp"
    }
  }

  # Use default credentials chain if not provided
  dynamic "assume_role" {
    for_each = var.aws_access_key_id != "" && var.aws_secret_access_key != "" ? [] : [1]
    content {
      role_arn = "arn:aws:iam::${data.aws_caller_identity.current.account_id}:role/erlmcp-deployment-role"
    }
  }
}

# Configure Kubernetes provider for EKS with v1 authentication API
provider "kubernetes" {
  host                   = try(data.aws_eks_cluster.cluster[0].endpoint, "")
  cluster_ca_certificate = try(base64decode(data.aws_eks_cluster.cluster[0].certificate_authority[0].data), "")

  exec {
    api_version = "client.authentication.k8s.io/v1"
    args        = ["eks", "get-token", "--cluster-name", local.cluster_identifier, "--region", var.aws_region]
    command     = "aws"
  }
}

# Configure Helm provider with v1 authentication API
provider "helm" {
  kubernetes {
    host                   = try(data.aws_eks_cluster.cluster[0].endpoint, "")
    cluster_ca_certificate = try(base64decode(data.aws_eks_cluster.cluster[0].certificate_authority[0].data), "")

    exec {
      api_version = "client.authentication.k8s.io/v1"
      args        = ["eks", "get-token", "--cluster-name", local.cluster_identifier, "--region", var.aws_region]
      command     = "aws"
    }
  }
}

# Configure random provider for unique strings
provider "random" {}

# Configure TLS provider
provider "tls" {}