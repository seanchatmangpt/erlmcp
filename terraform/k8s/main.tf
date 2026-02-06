# Kubernetes Infrastructure for erlmcp v3
# Main configuration for production deployment

terraform {
  required_version = ">= 1.9.0"

  # Backend configuration for remote state management
  # Uncomment and configure for production use
  # backend "s3" {
  #   bucket         = "erlmcp-terraform-state"
  #   key            = "kubernetes/terraform.tfstate"
  #   region         = "us-east-1"
  #   encrypt        = true
  #   dynamodb_table = "erlmcp-terraform-locks"
  #   kms_key_id     = "alias/terraform-state-key"
  # }

  required_providers {
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
    tls = {
      source  = "hashicorp/tls"
      version = "~> 4.0"
    }
    time = {
      source  = "hashicorp/time"
      version = "~> 0.12"
    }
  }
}

provider "kubernetes" {
  config_path    = var.kubeconfig_path
  config_context = var.kube_context
}

provider "helm" {
  kubernetes {
    config_path    = var.kubeconfig_path
    config_context = var.kube_context
  }
}

# Generate random suffix for resources
resource "random_string" "suffix" {
  length  = 8
  special = false
  upper   = false
}

# Namespace for erlmcp
resource "kubernetes_namespace" "erlmcp" {
  metadata {
    name = "erlmcp-${random_string.suffix.result}"
    labels = {
      "app.kubernetes.io/name"      = "erlmcp"
      "app.kubernetes.io/managed-by" = "terraform"
    }
  }
}

# Service account for erlmcp
resource "kubernetes_service_account" "erlmcp" {
  metadata {
    name      = "erlmcp-sa"
    namespace = kubernetes_namespace.erlmcp.metadata.0.name
    labels = {
      "app.kubernetes.io/name" = "erlmcp"
    }
  }

  automount_service_account_token = true
}

# RBAC for erlmcp
resource "kubernetes_role" "erlmcp" {
  metadata {
    name      = "erlmcp-role"
    namespace = kubernetes_namespace.erlmcp.metadata.0.name
    labels = {
      "app.kubernetes.io/name" = "erlmcp"
    }
  }

  rules {
    api_groups = [""]
    resources  = ["pods", "services", "endpoints", "configmaps"]
    verbs      = ["get", "list", "watch", "create", "update", "patch", "delete"]
  }

  rules {
    api_groups = ["extensions"]
    resources  = ["ingresses"]
    verbs      = ["get", "list", "watch"]
  }

  rules {
    api_groups = [""]
    resources  = ["secrets"]
    verbs      = ["get", "list"]
  }
}

resource "kubernetes_role_binding" "erlmcp" {
  metadata {
    name      = "erlmcp-rolebinding"
    namespace = kubernetes_namespace.erlmcp.metadata.0.name
    labels = {
      "app.kubernetes.io/name" = "erlmcp"
    }
  }

  role_ref {
    api_group = "rbac.authorization.k8s.io"
    kind      = "Role"
    name      = kubernetes_role.erlmcp.metadata.0.name
  }

  subject {
    kind      = "ServiceAccount"
    name      = kubernetes_service_account.erlmcp.metadata.0.name
    namespace = kubernetes_namespace.erlmcp.metadata.0.name
  }
}

# ConfigMap for erlmcp configuration
resource "kubernetes_config_map" "erlmcp_config" {
  metadata {
    name      = "erlmcp-config"
    namespace = kubernetes_namespace.erlmcp.metadata.0.name
    labels = {
      "app.kubernetes.io/name" = "erlmcp"
    }
  }

  data = {
    "erlmcp.conf" = templatefile("${path.module}/templates/erlmcp.conf.tpl", {
      environment = var.environment
      log_level   = var.log_level
      metrics_enabled = var.metrics_enabled
      tracing_enabled = var.tracing_enabled
    })
  }
}

# ConfigMap for environment configuration
resource "kubernetes_config_map" "env_config" {
  metadata {
    name      = "env-config"
    namespace = kubernetes_namespace.erlmcp.metadata.0.name
    labels = {
      "app.kubernetes.io/name" = "erlmcp"
    }
  }

  data = {
    "environment" = var.environment
    "region"      = var.region
    "cluster"     = var.cluster_name
  }
}

# Secret for erlmcp
resource "kubernetes_secret" "erlmcp_secrets" {
  metadata {
    name      = "erlmcp-secrets"
    namespace = kubernetes_namespace.erlmcp.metadata.0.name
    labels = {
      "app.kubernetes.io/name" = "erlmcp"
    }
  }

  type = "Opaque"

  data = {
    "database-url"     = var.database_url
    "redis-url"        = var.redis_url
    "oauth-secret"     = var.oauth_secret
    "encryption-key"   = var.encryption_key
    "api-key"         = var.api_key
  }
}

# TLS certificate for erlmcp
resource "tls_self_signed_cert" "erlmcp_cert" {
  subject {
    common_name  = "erlmcp.${var.domain}"
    organization = "erlmcp"
  }

  private_key_algorithm = "RSA"
  private_key_ecdsa_curve = "P256"
  validity_period_hours = 8760  # 1 year

  allowed_uses = [
    "server_auth",
    "client_auth"
  ]
}

# Kubernetes secret for TLS
resource "kubernetes_secret" "erlmcp_tls" {
  metadata {
    name      = "erlmcp-tls"
    namespace = kubernetes_namespace.erlmcp.metadata.0.name
    labels = {
      "app.kubernetes.io/name" = "erlmcp"
    }
  }

  type = "kubernetes.io/tls"

  data = {
    "tls.crt" = tls_self_signed_cert.erlmcp_cert.cert_pem
    "tls.key" = tls_self_signed_cert.erlmcp_cert.private_key_pem
  }
}

# Horizontal Pod Autoscaler for erlmcp
resource "kubernetes_horizontal_pod_autoscaler_v2" "erlmcp" {
  metadata {
    name      = "erlmcp-hpa"
    namespace = kubernetes_namespace.erlmcp.metadata.0.name
    labels = {
      "app.kubernetes.io/name" = "erlmcp"
    }
  }

  spec {
    scale_target_ref {
      api_version = "apps/v1"
      kind        = "Deployment"
      name        = "erlmcp"
    }

    min_replicas = var.min_replicas
    max_replicas = var.max_replicas

    metric {
      type = "Resource"
      resource {
        name = "cpu"
        target {
          type               = "Utilization"
          average_utilization = var.target_cpu_utilization
        }
      }
    }

    metric {
      type = "Resource"
      resource {
        name = "memory"
        target {
          type               = "Utilization"
          average_utilization = var.target_memory_utilization
        }
      }
    }

    metric {
      type = "Pods"
      pods {
        metric {
          name = "packets_per_second"
        }
        target {
          type = "AverageValue"
          average_value = "1000"
        }
      }
    }
  }
}

# NetworkPolicy for erlmcp
resource "kubernetes_network_policy" "erlmcp" {
  metadata {
    name      = "erlmcp-netpolicy"
    namespace = kubernetes_namespace.erlmcp.metadata.0.name
    labels = {
      "app.kubernetes.io/name" = "erlmcp"
    }
  }

  spec {
    pod_selector {
      match_labels = {
        "app.kubernetes.io/name" = "erlmcp"
      }
    }

    ingress {
      from {
        pod_selector {
          match_labels = {
            "app.kubernetes.io/name" = "erlmcp"
          }
        }
      }
      from {
        namespace_selector {
          match_labels = {
            "name" = "kube-system"
          }
        }
      }
      ports {
        port     = "8080"
        protocol = "TCP"
      }
      ports {
        port     = "8443"
        protocol = "TCP"
      }
    }

    egress {
      from {}
      to {}
      ports {
        port     = "443"
        protocol = "TCP"
      }
      ports {
        port     = "53"
        protocol = "UDP"
      }
    }
  }
}

# PodDisruptionBudget for erlmcp
resource "kubernetes_pod_disruption_budget" "erlmcp" {
  metadata {
    name      = "erlmcp-pdb"
    namespace = kubernetes_namespace.erlmcp.metadata.0.name
    labels = {
      "app.kubernetes.io/name" = "erlmcp"
    }
  }

  spec {
    min_available = 2
    selector {
      match_labels = {
        "app.kubernetes.io/name" = "erlmcp"
      }
    }
  }
}

# Service Monitor for Prometheus (commented out - requires Prometheus CRD)
# Uncomment if Prometheus Operator is installed
# resource "kubernetes_manifest" "service_monitor_erlmcp" {
#   manifest = {
#     apiVersion = "monitoring.coreos.com/v1"
#     kind       = "ServiceMonitor"
#     metadata = {
#       name      = "erlmcp"
#       namespace = kubernetes_namespace.erlmcp.metadata[0].name
#       labels = {
#         "app.kubernetes.io/name" = "erlmcp"
#         "release"                = "prometheus"
#       }
#     }
#     spec = {
#       selector = {
#         matchLabels = {
#           "app.kubernetes.io/name" = "erlmcp"
#         }
#       }
#       endpoints = [
#         {
#           port     = "metrics"
#           interval = "30s"
#         }
#       ]
#     }
#   }
# }

# Outputs
output "namespace" {
  value = kubernetes_namespace.erlmcp.metadata.0.name
}

output "service_account" {
  value = kubernetes_service_account.erlmcp.metadata.0.name
}

output "config_map" {
  value = kubernetes_config_map.erlmcp_config.metadata.0.name
}

output "secret_name" {
  value = kubernetes_secret.erlmcp_secrets.metadata.0.name
}

output "tls_secret" {
  value = kubernetes_secret.erlmcp_tls.metadata.0.name
}

output "cluster_endpoint" {
  value = "https://erlmcp.${var.domain}"
}