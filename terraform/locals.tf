locals {
  # Environment-specific configurations
  is_production = var.environment == "production"

  # Tags for all resources
  tags = {
    Environment = var.environment
    Project     = "erlmcp"
    ManagedBy   = "terraform"
    Version     = "3.0.0"
  }

  # S3 bucket names
  s3_bucket_name = "erlmcp-${var.environment}-artifacts-${random.string.result}"

  # EKS cluster name
  cluster_identifier = "${var.cluster_name}-${var.environment}"

  # Network configuration
  availability_zones = slice(data.aws_availability_zones.available.names, 0, 3)

  # OIDC provider for EKS
  oidc_provider_arn = "arn:aws:iam::${data.aws_caller_identity.account_id}:oidc-provider/${replace(aws_eks_cluster.cluster.identity[0].oidc[0].issuer, "https://", "")}"

  # Namespace configuration
  namespace = var.environment

  # Kubernetes labels
  app_labels = {
    app     = "erlmcp"
    version = var.image_tag
    tier    = var.environment
  }

  # Service names
  service_names = {
    main      = "erlmcp-main"
    canary    = "erlmcp-canary"
    blue      = "erlmcp-blue"
    green     = "erlmcp-green"
  }

  # Canary configuration
  canary_config = {
    enabled     = var.canary_percentage > 0
    percentage  = var.canary_percentage
    weight      = var.canary_percentage / 100
    header_name = "X-Canary"
    header_value = "true"
  }

  # Resource configuration
  resource_config = {
    main = merge(var.resource_requests, var.resource_limits)
    canary = {
      cpu    = var.resource_limits.cpu
      memory = var.resource_limits.memory
    }
  }

  # Security groups
  security_groups = {
    cluster   = "eks-cluster-${var.environment}"
    nodes     = "eks-nodes-${var.environment}"
    load_balancer = "eks-lb-${var.environment}"
  }

  # Load balancer configuration
  load_balancer_config = {
    name              = "alb-${var.environment}"
    internal          = false
    load_balancer_type = "application"
    ip_address_type   = "ipv4"
    security_groups   = [aws_security_group.alb[0].id]
    subnets          = aws_subnet.private[*].id
  }

  # Helm configuration
  helm_values = {
    global = {
      image = {
        registry = "ghcr.io"
        repository = var.aws_access_key_id != "" ? var.aws_access_key_id : "your-org"
        tag      = var.image_tag
        pullPolicy = "IfNotPresent"
      }
      service = {
        type = "LoadBalancer"
        ports = {
          http = 80
          https = 443
        }
      }
      resources = merge(var.resource_requests, var.resource_limits)
      tolerations = var.tolerations
      affinity = var.affinity
    }

    replicaCount = var.replica_count

    # Feature flags
    featureFlags = var.feature_flags

    # Security settings
    securityContext = {
      allowPrivilegeEscalation = false
      runAsNonRoot = true
      runAsUser = 1000
      capabilities = {
        drop = ["ALL"]
        add = ["NET_BIND_SERVICE"]
      }
    }

    # Probe configuration
    probes = {
      liveness = {
        path = var.health_check_path
        initialDelaySeconds = 30
        periodSeconds = 10
        failureThreshold = 3
      }
      readiness = {
        path = "${var.health_check_path}?ready=1"
        initialDelaySeconds = 30
        periodSeconds = 5
        failureThreshold = 3
      }
    }

    # Metrics and observability
    metrics = {
      enabled = true
      serviceMonitor = {
        enabled = true
        interval = "30s"
        path = "/metrics"
      }
    }

    # Logging
    logging = {
      level = "info"
      format = "json"
    }

    # Network policy
    networkPolicy = {
      enabled = true
      egress = {
        enabled = true
        rules = [
          {
            action = "Allow"
            protocol = "TCP"
            ports = ["80", "443", "5432", "6379"]
          }
        ]
      }
    }
  }
}