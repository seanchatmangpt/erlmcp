# erlmcp GKE Deployment Example
# This example shows how to deploy erlmcp on Google Kubernetes Engine

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = "~> 5.0"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.5"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.10"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.6"
    }
  }
}

# Configure the provider
provider "google" {
  project = var.project_id
  region  = var.region
}

provider "kubernetes" {
  host                   = module.gke.endpoint
  token                  = data.google_client_config.provider.access_token
  cluster_ca_certificate = base64decode(module.gke.ca_certificate)
}

provider "helm" {
  kubernetes {
    host                   = module.gke.endpoint
    token                  = data.google_client_config.provider.access_token
    cluster_ca_certificate = base64decode(module.gke.ca_certificate)
  }
}

# Create GKE cluster
module "gke" {
  source          = "terraform-google-modules/kubernetes-engine/google"
  project_id       = var.project_id
  name            = var.cluster_name
  region          = var.region
  zones           = var.zones
  network         = var.network
  subnetwork      = var.subnetwork
  ip_range_pods   = var.ip_range_pods
  ip_range_services = var.ip_range_services
  node_pools      = var.node_pools
  node_pools_oauth_scopes = var.node_pools_oauth_scopes

  # Enable required APIs
  enable_cloud_apis = true

  # Add marketplace annotations for deployments
  kubernetes_dashboard = {
    enabled = false
  }

  # Add marketplace-specific configuration
  addons = {
    http_load_balancing {
      disabled = false
    }
    cloud_run_dashboard {
      disabled = true
    }
  }
}

# Create namespace
resource "kubernetes_namespace" "erlmcp" {
  metadata {
    name = var.namespace
    labels = {
      app       = "erlmcp"
      type      = "marketplace"
      tier      = "production"
    }
  }
}

# Create secrets
resource "kubernetes_secret" "erlmcp-secrets" {
  for_each = var.secrets

  metadata {
    name      = each.key
    namespace = kubernetes_namespace.erlmcp.metadata.0.name
    labels = {
      app = "erlmcp"
    }
  }

  data = {
    value = each.value
  }

  type = "Opaque"
}

# Create values file for Helm
resource "local_file" "helm-values" {
  content = jsonencode({
    replicaCount = var.replica_count
    image = {
      repository = var.container_image
      pullPolicy = "Always"
      tag        = var.image_tag
    }

    service = {
      type        = "ClusterIP"
      port        = 8080
      annotations = {
        "marketplace.cloud.google.com/deployment-type" = "gke"
        "marketplace.cloud.google.com/deployment-id" = "erlmcp-${random_string.suffix.result}"
      }
    }

    ingress = {
      enabled      = var.enable_ingress
      annotations = var.ingress_annotations
      hosts        = var.ingress_hosts
      tls          = var.ingress_tls
    }

    resources = {
      limits = {
        cpu = var.cpu_limit
        memory = var.memory_limit
      }
      requests = {
        cpu = var.cpu_request
        memory = var.memory_request
      }
    }

    autoscaling = {
      enabled = true
      minReplicas = var.min_replicas
      maxReplicas = var.max_replicas
      targetCPUUtilization = var.target_cpu_utilization
      targetMemoryUtilization = var.target_memory_utilization
    }

    nodeSelector = var.node_selector

    tolerations = var.tolerations

    affinity = {
      nodeAffinity = {
        requiredDuringSchedulingIgnoredDuringExecution = {
          nodeSelectorTerm = {
            matchExpressions = {
              key = "cloud.google.com/gke-nodepool"
              operator = "In"
              values = [var.node_pool]
            }
          }
        }
      }
    }

    securityContext = {
      runAsNonRoot = true
      runAsUser = 1000
      runAsGroup = 1000
      fsGroup = 1000
    }

    podAnnotations = {
      "prometheus.io/scrape" = "true"
      "prometheus.io/port" = "9100"
      "prometheus.io/path" = "/metrics"
      "marketplace.cloud.google.com/deployment-type" = "gke"
    }

    # Environment variables
    env = merge(
      {
        GOOGLE_CLOUD_PROJECT = var.project_id
        ENVIRONMENT = "production"
        LOG_LEVEL = "info"
        ENABLE_METRICS = "true"
        ENABLE_TRACING = "true"
      },
      var.environment_variables
    )

    # Secrets as environment variables
    secretEnv = {
      for secret in var.secrets :
        replace(secret, "_", "-") => {
          name = kubernetes_secret.erlmcp-secrets[secret].metadata.0.name
          key = "value"
        }
    }

    # Service monitor for Prometheus
    serviceMonitor = {
      enabled = true
      interval = "30s"
      scrapeTimeout = "10s"
      selector = {
        matchLabels = {
          app = "erlmcp"
        }
      }
      namespaceSelector = {
        matchNames = [kubernetes_namespace.erlmcp.metadata.0.name]
      }
    }

    # Volume mounts for secrets
    volumeMounts = [
      for secret in var.secrets :
      {
        name = replace(secret, "_", "-")
        mountPath = "/etc/secrets/${replace(secret, "_", "-")}"
        readOnly = true
      }
    ]

    # Volumes for secrets
    volumes = [
      for secret in var.secrets :
      {
        name = replace(secret, "_", "-")
        secret = {
          secretName = kubernetes_secret.erlmcp-secrets[secret].metadata.0.name
          items = [{
            key = "value"
            path = "${replace(secret, "_", "-")}"
          }]
        }
      }
    ]
  })

  filename = "${path.module}/helm-values.yaml"
}

# Deploy with Helm
resource "helm_release" "erlmcp" {
  name       = var.release_name
  repository = "https://charts.bitnami.com/bitnami"
  chart      = "erlmcp"
  version    = var.chart_version
  namespace  = kubernetes_namespace.erlmcp.metadata.0.name

  # Use our custom values
  values = [
    local_file.helm-values.content
  ]

  # Wait for resources to be ready
  wait = true

  # Set timeout for deployment
  timeout = 600

  # Set recreate pods if values change
  recreate_pods = true

  # Skip CRDs (they should already be installed)
  skip_crds = true

  # Create if doesn't exist, update if exists
  create_namespace = false
}

# Variables
variable "project_id" {
  type        = string
  description = "The GCP project ID"
}

variable "region" {
  type        = string
  default     = "us-central1"
  description = "The region to deploy in"
}

variable "cluster_name" {
  type        = string
  default     = "erlmcp-cluster"
  description = "Name of the GKE cluster"
}

variable "zones" {
  type        = list(string)
  default     = ["us-central1-a", "us-central1-b", "us-central1-c"]
  description = "Zones for the cluster"
}

variable "network" {
  type        = string
  default     = "default"
  description = "VPC network name"
}

variable "subnetwork" {
  type        = string
  default     = "default"
  description = "VPC subnetwork name"
}

variable "ip_range_pods" {
  type        = string
  default     = ""
  description = "IP range for pods (if custom subnet is used)"
}

variable "ip_range_services" {
  type        = string
  default     = ""
  description = "IP range for services (if custom subnet is used)"
}

variable "node_pools" {
  type = list(object({
    name       = string
    machine_type = string
    min_nodes  = number
    max_nodes  = number
    disk_size_gb = number
    disk_type = string
    auto_repair = bool
    auto_upgrade = bool
  }))
  default = [
    {
      name = "default-pool"
      machine_type = "e2-standard-4"
      min_nodes = 2
      max_nodes = 10
      disk_size_gb = 100
      disk_type = "pd-balanced"
      auto_repair = true
      auto_upgrade = true
    }
  ]
  description = "Node pools configuration"
}

variable "node_pools_oauth_scopes" {
  type = list(string)
  default = [
    "https://www.googleapis.com/auth/cloud-platform",
    "https://www.googleapis.com/auth/devstorage.read_only",
    "https://www.googleapis.com/auth/logging.write",
    "https://www.googleapis.com/auth/monitoring.write",
    "https://www.googleapis.com/auth/servicecontrol",
    "https://www.googleapis.com/auth/service.management.readonly",
    "https://www.googleapis.com/auth/trace.append"
  ]
  description = "OAuth scopes for node pools"
}

variable "namespace" {
  type        = string
  default     = "erlmcp"
  description = "Kubernetes namespace to deploy into"
}

variable "replica_count" {
  type        = number
  default     = 3
  description = "Number of replicas"
}

variable "container_image" {
  type        = string
  description = "Container image to deploy"
}

variable "image_tag" {
  type        = string
  default     = "latest"
  description = "Container image tag"
}

variable "secrets" {
  type        = map(string)
  default = {
    "ERLMCP_ERLANG_COOKIE" = "secret-value"
    "ERLMCP_DATABASE_URL" = "postgres://user:password@host:port/database"
    "ERLMCP_JWT_SECRET" = "jwt-secret-value"
  }
  description = "Secrets to inject"
}

variable "environment_variables" {
  type        = map(string)
  default = {}
  description = "Additional environment variables"
}

variable "enable_ingress" {
  type        = bool
  default     = true
  description = "Enable Ingress controller"
}

variable "ingress_annotations" {
  type        = map(string)
  default     = {}
  description = "Ingress annotations"
}

variable "ingress_hosts" {
  type        = list(string)
  default     = ["erlmcp.example.com"]
  description = "Ingress hostnames"
}

variable "ingress_tls" {
  type        = list(map(string))
  default     = []
  description = "Ingress TLS configuration"
}

variable "cpu_limit" {
  type        = string
  default     = "1"
  description = "CPU limit"
}

variable "memory_limit" {
  type        = string
  default     = "512Mi"
  description = "Memory limit"
}

variable "cpu_request" {
  type        = string
  default     = "100m"
  description = "CPU request"
}

variable "memory_request" {
  type        = string
  default     = "256Mi"
  description = "Memory request"
}

variable "min_replicas" {
  type        = number
  default     = 2
  description = "Minimum replicas for HPA"
}

variable "max_replicas" {
  type        = number
  default     = 10
  description = "Maximum replicas for HPA"
}

variable "target_cpu_utilization" {
  type        = number
  default     = 70
  description = "Target CPU utilization percentage for HPA"
}

variable "target_memory_utilization" {
  type        = number
  default     = 80
  description = "Target memory utilization percentage for HPA"
}

variable "node_selector" {
  type        = map(string)
  default     = {}
  description = "Node selector for pods"
}

variable "tolerations" {
  type        = list(map(string))
  default     = []
  description = "Tolerations for pods"
}

variable "node_pool" {
  type        = string
  default     = "default-pool"
  description = "Node pool to deploy to"
}

variable "release_name" {
  type        = string
  default     = "erlmcp"
  description = "Helm release name"
}

variable "chart_version" {
  type        = string
  default     = "1.0.0"
  description = "Helm chart version"
}

# Random suffix for unique naming
resource "random_string" "suffix" {
  length  = 8
  special = false
  upper   = false
}

# Outputs
output "cluster_name" {
  value = module.gke.name
}

output "cluster_endpoint" {
  value = module.gke.endpoint
}

output "cluster_ca_certificate" {
  value = module.gke.ca_certificate
}

output "namespace" {
  value = kubernetes_namespace.erlmcp.metadata.0.name
}

output "service_name" {
  value = "erlmcp-service"
}

output "ingress_name" {
  value = "erlmcp-ingress"
}

output "service_url" {
  value = "http://erlmcp-service.${kubernetes_namespace.erlmcp.metadata.0.name}.svc.cluster.local"
}