# ============================================================================
# GKE Deployment for erlmcp
# ============================================================================
# Kubernetes-based deployment with GKE Autopilot or Standard clusters
#
# DOCKER-ONLY CONSTITUTION: Container images built via Docker quality gates
# ============================================================================

# ============================================================================
# GKE Cluster
# ============================================================================

resource "google_container_cluster" "main" {
  count = local.deploy_gke ? 1 : 0

  name     = "${local.name_prefix}-cluster"
  location = var.gke_regional ? var.region : "${var.region}-a"
  project  = var.project_id

  # Use Autopilot for simplified operations
  enable_autopilot = var.gke_autopilot

  # Network configuration
  network    = google_compute_network.main.name
  subnetwork = google_compute_subnetwork.main.name

  # IP allocation policy for VPC-native cluster
  ip_allocation_policy {
    cluster_secondary_range_name  = "gke-pods"
    services_secondary_range_name = "gke-services"
  }

  # Private cluster configuration
  private_cluster_config {
    enable_private_nodes    = true
    enable_private_endpoint = var.gke_private_endpoint
    master_ipv4_cidr_block  = var.gke_master_cidr
  }

  # Master authorized networks
  master_authorized_networks_config {
    dynamic "cidr_blocks" {
      for_each = var.gke_authorized_networks
      content {
        cidr_block   = cidr_blocks.value.cidr
        display_name = cidr_blocks.value.name
      }
    }
  }

  # Release channel
  release_channel {
    channel = var.gke_release_channel
  }

  # Maintenance window
  maintenance_policy {
    daily_maintenance_window {
      start_time = "03:00"
    }
  }

  # Workload Identity
  workload_identity_config {
    workload_pool = "${var.project_id}.svc.id.goog"
  }

  # Logging and monitoring
  logging_config {
    enable_components = ["SYSTEM_COMPONENTS", "WORKLOADS"]
  }

  monitoring_config {
    enable_components = ["SYSTEM_COMPONENTS", "WORKLOADS"]

    managed_prometheus {
      enabled = true
    }
  }

  # Binary authorization
  binary_authorization {
    evaluation_mode = var.gke_binary_auth ? "PROJECT_SINGLETON_POLICY_ENFORCE" : "DISABLED"
  }

  # Security posture
  security_posture_config {
    mode               = "BASIC"
    vulnerability_mode = "VULNERABILITY_BASIC"
  }

  # DNS config
  dns_config {
    cluster_dns        = "CLOUD_DNS"
    cluster_dns_scope  = "CLUSTER_SCOPE"
    cluster_dns_domain = "${local.name_prefix}.local"
  }

  # Gateway API
  gateway_api_config {
    channel = "CHANNEL_STANDARD"
  }

  # Addons
  addons_config {
    http_load_balancing {
      disabled = false
    }
    horizontal_pod_autoscaling {
      disabled = false
    }
    gce_persistent_disk_csi_driver_config {
      enabled = true
    }
    gcs_fuse_csi_driver_config {
      enabled = true
    }
  }

  # Standard cluster node pool (only if not autopilot)
  dynamic "node_pool" {
    for_each = var.gke_autopilot ? [] : [1]
    content {
      name       = "default-pool"
      node_count = var.gke_min_nodes

      autoscaling {
        min_node_count = var.gke_min_nodes
        max_node_count = var.gke_max_nodes
      }

      node_config {
        machine_type    = var.gke_machine_type
        disk_size_gb    = var.gke_disk_size
        disk_type       = "pd-ssd"
        service_account = google_service_account.erlmcp.email
        oauth_scopes    = ["https://www.googleapis.com/auth/cloud-platform"]

        labels = local.common_labels

        metadata = {
          disable-legacy-endpoints = "true"
        }

        shielded_instance_config {
          enable_secure_boot          = true
          enable_integrity_monitoring = true
        }

        workload_metadata_config {
          mode = "GKE_METADATA"
        }
      }

      management {
        auto_repair  = true
        auto_upgrade = true
      }
    }
  }

  # Remove default node pool for standard clusters
  dynamic "initial_node_count" {
    for_each = var.gke_autopilot ? [] : [1]
    content {
    }
  }
  remove_default_node_pool = var.gke_autopilot ? null : true
  initial_node_count       = var.gke_autopilot ? null : 1

  resource_labels = local.common_labels

  depends_on = [
    google_project_service.required_apis,
    google_compute_subnetwork.main
  ]

  deletion_protection = var.gke_deletion_protection
}

# ============================================================================
# Workload Identity Binding
# ============================================================================

resource "google_service_account_iam_member" "workload_identity" {
  count = local.deploy_gke ? 1 : 0

  service_account_id = google_service_account.erlmcp.name
  role               = "roles/iam.workloadIdentityUser"
  member             = "serviceAccount:${var.project_id}.svc.id.goog[erlmcp/erlmcp]"
}

# ============================================================================
# Kubernetes Namespace
# ============================================================================

resource "kubernetes_namespace" "erlmcp" {
  count = local.deploy_gke ? 1 : 0

  metadata {
    name = "erlmcp"
    labels = {
      "app.kubernetes.io/name"       = "erlmcp"
      "app.kubernetes.io/managed-by" = "terraform"
    }
  }

  depends_on = [google_container_cluster.main]
}

# ============================================================================
# Kubernetes Service Account
# ============================================================================

resource "kubernetes_service_account" "erlmcp" {
  count = local.deploy_gke ? 1 : 0

  metadata {
    name      = "erlmcp"
    namespace = kubernetes_namespace.erlmcp[0].metadata[0].name
    annotations = {
      "iam.gke.io/gcp-service-account" = google_service_account.erlmcp.email
    }
  }

  depends_on = [google_container_cluster.main]
}

# ============================================================================
# Kubernetes ConfigMap
# ============================================================================

resource "kubernetes_config_map" "erlmcp" {
  count = local.deploy_gke ? 1 : 0

  metadata {
    name      = "erlmcp-config"
    namespace = kubernetes_namespace.erlmcp[0].metadata[0].name
  }

  data = {
    "vm.args" = templatefile("${path.module}/templates/vm.args.tpl", {
      node_name     = "erlmcp@$(POD_IP)"
      cookie        = "$(ERLMCP_COOKIE)"
      schedulers    = var.gke_erlang_schedulers
      async_threads = var.gke_async_threads
    })

    "sys.config" = templatefile("${path.module}/templates/sys.config.tpl", {
      environment = var.environment
      log_level   = var.log_level
      http_port   = 8080
      metrics_port = 9568
    })
  }

  depends_on = [google_container_cluster.main]
}

# ============================================================================
# Kubernetes Deployment
# ============================================================================

resource "kubernetes_deployment" "erlmcp" {
  count = local.deploy_gke ? 1 : 0

  metadata {
    name      = "erlmcp"
    namespace = kubernetes_namespace.erlmcp[0].metadata[0].name
    labels = {
      "app.kubernetes.io/name"     = "erlmcp"
      "app.kubernetes.io/version"  = var.erlmcp_version
      "app.kubernetes.io/component" = "server"
    }
  }

  spec {
    replicas = var.gke_replicas

    selector {
      match_labels = {
        "app.kubernetes.io/name" = "erlmcp"
      }
    }

    template {
      metadata {
        labels = {
          "app.kubernetes.io/name"    = "erlmcp"
          "app.kubernetes.io/version" = var.erlmcp_version
        }
        annotations = {
          "prometheus.io/scrape" = "true"
          "prometheus.io/port"   = "9568"
          "prometheus.io/path"   = "/metrics"
        }
      }

      spec {
        service_account_name = kubernetes_service_account.erlmcp[0].metadata[0].name

        container {
          name  = "erlmcp"
          image = var.gke_image != "" ? var.gke_image : "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.erlmcp[0].repository_id}/erlmcp:${var.erlmcp_version}"

          port {
            name           = "http"
            container_port = 8080
          }

          port {
            name           = "metrics"
            container_port = 9568
          }

          port {
            name           = "epmd"
            container_port = 4369
          }

          port {
            name           = "dist"
            container_port = 9100
          }

          env {
            name = "POD_IP"
            value_from {
              field_ref {
                field_path = "status.podIP"
              }
            }
          }

          env {
            name  = "ERLMCP_ENVIRONMENT"
            value = var.environment
          }

          env {
            name = "ERLMCP_COOKIE"
            value_from {
              secret_key_ref {
                name = "erlmcp-secrets"
                key  = "cookie"
              }
            }
          }

          resources {
            requests = {
              cpu    = var.gke_cpu_request
              memory = var.gke_memory_request
            }
            limits = {
              cpu    = var.gke_cpu_limit
              memory = var.gke_memory_limit
            }
          }

          volume_mount {
            name       = "config"
            mount_path = "/etc/erlmcp"
          }

          volume_mount {
            name       = "data"
            mount_path = "/opt/erlmcp/data"
          }

          liveness_probe {
            http_get {
              path = "/health"
              port = 8080
            }
            initial_delay_seconds = 30
            period_seconds        = 30
            timeout_seconds       = 5
            failure_threshold     = 3
          }

          readiness_probe {
            http_get {
              path = "/health"
              port = 8080
            }
            initial_delay_seconds = 10
            period_seconds        = 10
            timeout_seconds       = 5
            failure_threshold     = 3
          }

          startup_probe {
            http_get {
              path = "/health"
              port = 8080
            }
            initial_delay_seconds = 5
            period_seconds        = 10
            timeout_seconds       = 5
            failure_threshold     = 30
          }
        }

        volume {
          name = "config"
          config_map {
            name = kubernetes_config_map.erlmcp[0].metadata[0].name
          }
        }

        volume {
          name = "data"
          empty_dir {}
        }

        topology_spread_constraint {
          max_skew           = 1
          topology_key       = "topology.kubernetes.io/zone"
          when_unsatisfiable = "ScheduleAnyway"
          label_selector {
            match_labels = {
              "app.kubernetes.io/name" = "erlmcp"
            }
          }
        }
      }
    }

    strategy {
      type = "RollingUpdate"
      rolling_update {
        max_surge       = "25%"
        max_unavailable = "25%"
      }
    }
  }

  depends_on = [google_container_cluster.main]
}

# ============================================================================
# Kubernetes Service
# ============================================================================

resource "kubernetes_service" "erlmcp" {
  count = local.deploy_gke ? 1 : 0

  metadata {
    name      = "erlmcp"
    namespace = kubernetes_namespace.erlmcp[0].metadata[0].name
    annotations = {
      "cloud.google.com/neg" = "{\"ingress\": true}"
    }
  }

  spec {
    selector = {
      "app.kubernetes.io/name" = "erlmcp"
    }

    port {
      name        = "http"
      port        = 80
      target_port = 8080
    }

    port {
      name        = "metrics"
      port        = 9568
      target_port = 9568
    }

    type = "ClusterIP"
  }

  depends_on = [google_container_cluster.main]
}

# ============================================================================
# Kubernetes Ingress (via Gateway API)
# ============================================================================

resource "kubernetes_manifest" "gateway" {
  count = local.deploy_gke && var.gke_enable_gateway ? 1 : 0

  manifest = {
    apiVersion = "gateway.networking.k8s.io/v1"
    kind       = "Gateway"
    metadata = {
      name      = "erlmcp-gateway"
      namespace = kubernetes_namespace.erlmcp[0].metadata[0].name
      annotations = {
        "networking.gke.io/certmap" = var.gke_certificate_map
      }
    }
    spec = {
      gatewayClassName = "gke-l7-global-external-managed"
      listeners = [
        {
          name     = "https"
          protocol = "HTTPS"
          port     = 443
          tls = {
            mode = "Terminate"
          }
        }
      ]
      addresses = [
        {
          type  = "NamedAddress"
          value = google_compute_global_address.gke_ip[0].name
        }
      ]
    }
  }

  depends_on = [google_container_cluster.main]
}

resource "google_compute_global_address" "gke_ip" {
  count = local.deploy_gke && var.gke_enable_gateway ? 1 : 0

  name    = "${local.name_prefix}-gke-ip"
  project = var.project_id
}

# ============================================================================
# Horizontal Pod Autoscaler
# ============================================================================

resource "kubernetes_horizontal_pod_autoscaler_v2" "erlmcp" {
  count = local.deploy_gke ? 1 : 0

  metadata {
    name      = "erlmcp"
    namespace = kubernetes_namespace.erlmcp[0].metadata[0].name
  }

  spec {
    scale_target_ref {
      api_version = "apps/v1"
      kind        = "Deployment"
      name        = kubernetes_deployment.erlmcp[0].metadata[0].name
    }

    min_replicas = var.gke_min_replicas
    max_replicas = var.gke_max_replicas

    metric {
      type = "Resource"
      resource {
        name = "cpu"
        target {
          type                = "Utilization"
          average_utilization = var.gke_cpu_target
        }
      }
    }

    metric {
      type = "Resource"
      resource {
        name = "memory"
        target {
          type                = "Utilization"
          average_utilization = var.gke_memory_target
        }
      }
    }

    behavior {
      scale_down {
        stabilization_window_seconds = 300
        select_policy                = "Min"
        policy {
          type          = "Percent"
          value         = 10
          period_seconds = 60
        }
      }
      scale_up {
        stabilization_window_seconds = 0
        select_policy                = "Max"
        policy {
          type          = "Percent"
          value         = 100
          period_seconds = 15
        }
        policy {
          type          = "Pods"
          value         = 4
          period_seconds = 15
        }
      }
    }
  }

  depends_on = [google_container_cluster.main]
}

# ============================================================================
# Pod Disruption Budget
# ============================================================================

resource "kubernetes_pod_disruption_budget" "erlmcp" {
  count = local.deploy_gke ? 1 : 0

  metadata {
    name      = "erlmcp"
    namespace = kubernetes_namespace.erlmcp[0].metadata[0].name
  }

  spec {
    min_available = "50%"
    selector {
      match_labels = {
        "app.kubernetes.io/name" = "erlmcp"
      }
    }
  }

  depends_on = [google_container_cluster.main]
}

# ============================================================================
# GKE Outputs
# ============================================================================

output "gke_cluster_name" {
  description = "GKE cluster name"
  value       = local.deploy_gke ? google_container_cluster.main[0].name : null
}

output "gke_cluster_endpoint" {
  description = "GKE cluster endpoint"
  value       = local.deploy_gke ? google_container_cluster.main[0].endpoint : null
  sensitive   = true
}

output "gke_get_credentials" {
  description = "Command to get GKE credentials"
  value       = local.deploy_gke ? "gcloud container clusters get-credentials ${google_container_cluster.main[0].name} --region ${var.gke_regional ? var.region : "${var.region}-a"} --project ${var.project_id}" : null
}
