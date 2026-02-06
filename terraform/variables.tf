variable "environment" {
  type        = string
  description = "Deployment environment (staging, production)"
}

variable "image_tag" {
  type        = string
  description = "Docker image tag to deploy"
}

variable "aws_region" {
  type        = string
  default     = "us-east-1"
  description = "AWS region for deployment"
}

variable "cluster_name" {
  type        = string
  default     = "erlmcp-cluster"
  description = "EKS cluster name"
}

variable "node_group_name" {
  type        = string
  description = "EKS node group name"
}

variable "vpc_cidr" {
  type        = string
  default     = "10.0.0.0/16"
  description = "VPC CIDR block"
}

variable "subnet_cidrs" {
  type        = list(string)
  default     = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
  description = "Subnet CIDR blocks"
}

variable "instance_type" {
  type        = string
  default     = "m6i.large"
  description = "EC2 instance type for EKS nodes"
}

variable "min_nodes" {
  type        = number
  default     = 2
  description = "Minimum number of nodes in node group"
}

variable "max_nodes" {
  type        = number
  default     = 10
  description = "Maximum number of nodes in node group"
}

variable "desired_nodes" {
  type        = number
  default     = 3
  description = "Desired number of nodes in node group"
}

variable "enable_auto_scaling" {
  type        = bool
  default     = true
  description = "Enable auto-scaling for node group"
}

variable "aws_access_key_id" {
  type        = string
  sensitive   = true
  default     = ""
  description = "AWS access key ID"
}

variable "aws_secret_access_key" {
  type        = string
  sensitive   = true
  default     = ""
  description = "AWS secret access key"
}

variable "istio_enabled" {
  type        = bool
  default     = false
  description = "Enable Istio service mesh"
}

variable "vault_enabled" {
  type        = bool
  default     = true
  description = "Enable Vault for secrets management"
}

variable "feature_flags" {
  type        = map(string)
  default     = {}
  description = "Feature flags for deployment"
}

variable "canary_percentage" {
  type        = number
  default     = 0
  description = "Percentage of traffic for canary deployment (0-100)"
}

variable "health_check_path" {
  type        = string
  default     = "/mcp/health"
  description = "Health check endpoint for application"
}

variable "replica_count" {
  type        = number
  default     = 2
  description = "Number of replicas for main deployment"
}

variable "canary_replica_count" {
  type        = number
  default     = 1
  description = "Number of replicas for canary deployment"
}

variable "max_unavailable" {
  type        = number
  default     = 1
  description = "Maximum unavailable replicas during deployment"
}

variable "max surge" {
  type        = number
  default     = 1
  description = "Maximum surge replicas during deployment"
}

variable "resource_requests" {
  type = object({
    cpu    = string
    memory = string
  })
  default = {
    cpu    = "100m"
    memory = "256Mi"
  }
  description = "Default resource requests for application"
}

variable "resource_limits" {
  type = object({
    cpu    = string
    memory = string
  })
  default = {
    cpu    = "500m"
    memory = "1Gi"
  }
  description = "Default resource limits for application"
}

variable "tolerations" {
  type = list(object({
    key      = string
    operator = string
    value    = string
    effect   = string
  }))
  default = []
  description = "Pod tolerations for node taints"
}

variable "affinity" {
  type = object({
    node_affinity = object({
      required = list(object({
        node_selector_term = object({
          match_expressions = list(object({
            key      = string
            operator = string
            values   = list(string)
          }))
        })
      }))
      preferred = list(object({
        weight = number
        preference = object({
          match_expressions = list(object({
            key      = string
            operator = string
            values   = list(string)
          }))
        })
      }))
    })
    pod_affinity = object({
      required = list(object({
        label_selector = object({
          match_expressions = list(object({
            key      = string
            operator = string
            values   = list(string)
          }))
        })
        topology_keys = list(string)
      }))
      preferred = list(object({
        weight = number
        preference = object({
          pod_affinity_term = object({
            label_selector = object({
              match_expressions = list(object({
                key      = string
                operator = string
                values   = list(string)
              }))
            })
            topology_keys = list(string)
          })
        })
      }))
    })
  })
  default = null
  description = "Pod affinity and anti-affinity rules"
}

# EKS Cluster Configuration
variable "eks_version" {
  type        = string
  default     = "1.31"
  description = "EKS Kubernetes version"
  validation {
    condition     = can(regex("^1\\.(2[7-9]|3[0-9])$", var.eks_version))
    error_message = "EKS version must be 1.27 or higher"
  }
}

variable "create_eks_cluster" {
  type        = bool
  default     = true
  description = "Whether to create the EKS cluster"
}

variable "cluster_endpoint_public_access_cidrs" {
  type        = list(string)
  default     = ["0.0.0.0/0"]
  description = "List of CIDR blocks that can access the cluster endpoint publicly"
}

variable "cluster_service_ipv4_cidr" {
  type        = string
  default     = "172.20.0.0/16"
  description = "The CIDR block to assign Kubernetes service IP addresses from"
  validation {
    condition     = can(cidrhost(var.cluster_service_ipv4_cidr, 0))
    error_message = "Must be a valid IPv4 CIDR block"
  }
}

# VPC Endpoint Configuration
variable "enable_vpc_endpoints" {
  type        = bool
  default     = true
  description = "Enable VPC endpoints for AWS services"
}

# KMS Configuration
variable "enable_kms_encryption" {
  type        = bool
  default     = true
  description = "Enable KMS encryption for EKS secrets and S3 buckets"
}

variable "kms_key_deletion_window" {
  type        = number
  default     = 30
  description = "KMS key deletion window in days"
  validation {
    condition     = var.kms_key_deletion_window >= 7 && var.kms_key_deletion_window <= 30
    error_message = "KMS key deletion window must be between 7 and 30 days"
  }
}