# ============================================================================
# erlmcp v3 Complete GCP Deployment
# Multi-platform integration: GKE + Cloud Run + GCE + Full observability
# ============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.40.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = ">= 5.40.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = ">= 2.20.0"
    }
    helm = {
      source  = "hashicorp/helm"
      version = ">= 2.10.0"
    }
  }

  backend "gcs" {
    bucket = "erlmcp-terraform-state"
    prefix = "complete-deployment/state"
  }
}

# ============================================================================
# Provider Configuration
# ============================================================================
provider "google" {
  project = var.project_id
  region  = var.region
}

provider "google-beta" {
  project = var.project_id
  region  = var.region
}

data "google_client_config" "default" {}

provider "kubernetes" {
  host                   = module.gke.cluster_endpoint
  token                  = data.google_client_config.default.access_token
  cluster_ca_certificate = base64decode(module.gke.cluster_ca_certificate)
}

provider "helm" {
  kubernetes {
    host                   = module.gke.cluster_endpoint
    token                  = data.google_client_config.default.access_token
    cluster_ca_certificate = base64decode(module.gke.cluster_ca_certificate)
  }
}

# ============================================================================
# Local Variables
# ============================================================================
locals {
  common_labels = {
    app         = "erlmcp"
    environment = var.environment
    managed-by  = "terraform"
    deployment  = "complete"
    version     = var.image_tag
  }

  network_tags = [
    "erlang-node",
    "erlmcp-worker",
    "erlmcp-${var.environment}"
  ]

  # Service accounts for cross-service access
  all_service_accounts = concat(
    [module.gke.service_account_email],
    [module.cloud_run.service_account_email],
    [module.compute_engine.service_account_email]
  )
}

# ============================================================================
# VPC Network Module - Foundation Layer
# ============================================================================
module "vpc" {
  source = "../../modules/vpc"

  project_id   = var.project_id
  region       = var.region
  network_name = var.network_name

  # Multi-region subnet configuration
  subnets = [
    {
      name                     = "erlmcp-subnet-${var.region}"
      region                   = var.region
      ip_cidr_range            = var.primary_subnet_cidr
      private_ip_google_access = true
      purpose                  = null
      role                     = null
      secondary_ip_ranges = [
        {
          range_name    = "gke-pods"
          ip_cidr_range = var.gke_pods_cidr
        },
        {
          range_name    = "gke-services"
          ip_cidr_range = var.gke_services_cidr
        }
      ]
    },
    {
      name                     = "erlmcp-subnet-${var.secondary_region}"
      region                   = var.secondary_region
      ip_cidr_range            = var.secondary_subnet_cidr
      private_ip_google_access = true
      purpose                  = null
      role                     = null
      secondary_ip_ranges      = []
    }
  ]

  # Cloud NAT for private instance internet access
  create_router = true
  create_nat    = true

  # Firewall configuration
  create_firewall_rules    = true
  allow_https_public       = var.allow_public_access
  enable_private_endpoint  = true
  enable_erlang_distribution = true
  erlang_distribution_port_range = ["9000-9999"]
  erlang_node_tags         = local.network_tags

  # Security hardening
  ssh_source_ranges = var.ssh_source_ranges
  internal_ranges   = [var.primary_subnet_cidr, var.secondary_subnet_cidr]

  # Private service access for Cloud SQL
  enable_private_service_access = var.enable_cloud_sql
  private_service_cidr_prefix   = 16

  # Cloud Armor DDoS protection
  enable_cloud_armor                 = var.enable_cloud_armor
  cloud_armor_rate_limit_threshold   = var.rate_limit_threshold
  cloud_armor_rate_limit_interval    = 60
  cloud_armor_ban_duration           = 600
  cloud_armor_enable_owasp_rules     = true
  cloud_armor_enable_adaptive_protection = true

  # DNS policies
  enable_dns_policies         = true
  dns_enable_inbound_forwarding = false
  dns_enable_logging          = true
}

# ============================================================================
# IAM Module - Security Foundation
# ============================================================================
module "iam" {
  source = "../../modules/iam"

  project_id = var.project_id

  # Service account definitions
  service_accounts = {
    erlmcp-orchestrator = {
      display_name = "erlmcp Orchestrator Service Account"
      description  = "Service account for erlmcp orchestration across platforms"
      roles = [
        "roles/container.admin",
        "roles/run.admin",
        "roles/compute.instanceAdmin.v1",
        "roles/secretmanager.secretAccessor",
        "roles/monitoring.metricWriter",
        "roles/logging.logWriter"
      ]
    }
  }
}

# ============================================================================
# Secret Manager Module - Secure Configuration
# ============================================================================
module "secrets" {
  source = "../../modules/secret-manager"

  project_id = var.project_id
  labels     = local.common_labels

  # Grant access to all platform service accounts
  secret_accessors = concat(
    [for sa in local.all_service_accounts : "serviceAccount:${sa}"],
    ["serviceAccount:${module.iam.service_account_emails["erlmcp-orchestrator"]}"]
  )
}

# ============================================================================
# Artifact Registry Module - Container Image Storage
# ============================================================================
module "artifact_registry" {
  source = "../../modules/artifact-registry"

  project_id    = var.project_id
  region        = var.region
  repository_id = "erlmcp"

  description = "erlmcp v3 container images - multi-platform deployment"
  format      = "DOCKER"

  # Immutability and security
  enable_immutable_tags = true
  enable_vulnerability_scanning = true

  # Cross-project access if needed
  member_access = {
    readers = []
    writers = concat(
      [for sa in local.all_service_accounts : "serviceAccount:${sa}"]
    )
  }

  labels = local.common_labels
}

# ============================================================================
# GKE Cluster Module - Kubernetes Platform
# ============================================================================
module "gke" {
  source = "../../modules/gke"

  project_id = var.project_id
  region     = var.region

  cluster_name = "${var.cluster_name_prefix}-${var.environment}"
  network      = module.vpc.network_name
  subnet       = module.vpc.subnet_names[0]

  # Kubernetes version
  kubernetes_version = var.kubernetes_version
  release_channel    = var.release_channel

  # Private cluster configuration
  enable_private_endpoint = var.gke_private_endpoint
  enable_private_nodes    = true
  master_ipv4_cidr_block  = var.gke_master_cidr
  master_global_access    = var.gke_master_global_access

  # Node pool configuration - production grade
  node_pools = {
    primary = {
      version                      = ""
      machine_type                 = var.gke_machine_type
      image_type                   = "COS_CONTAINERD"
      disk_size_gb                 = var.gke_disk_size_gb
      disk_type                    = "pd-ssd"
      min_count                    = var.gke_min_nodes
      max_count                    = var.gke_max_nodes
      location_policy              = "BALANCED"
      spot                         = false
      preemptible                  = false
      enable_secure_boot           = true
      enable_integrity_monitoring  = true
      enable_gvnic                 = true
      pod_cidr                     = ""
      taints                       = []
      reservation_affinity         = ""
    }
    spot = var.enable_spot_nodes ? {
      version                      = ""
      machine_type                 = var.gke_spot_machine_type
      image_type                   = "COS_CONTAINERD"
      disk_size_gb                 = 50
      disk_type                    = "pd-standard"
      min_count                    = var.gke_spot_min_nodes
      max_count                    = var.gke_spot_max_nodes
      location_policy              = "ANY"
      spot                         = true
      preemptible                  = true
      enable_secure_boot           = false
      enable_integrity_monitoring  = false
      enable_gvnic                 = false
      pod_cidr                     = ""
      taints = [{
        key    = "workload-type"
        value  = "batch"
        effect = "NO_SCHEDULE"
      }]
      reservation_affinity = ""
    } : null
  }
  create_spot_node_pool    = var.enable_spot_nodes
  create_custom_node_pools = true

  # Security features
  enable_network_policy       = true
  network_policy_provider     = "CALICO"
  enable_secure_boot          = true
  enable_integrity_monitoring = true
  enable_shielded_nodes       = true

  # Observability
  enable_managed_prometheus = true
  enable_gke_monitoring     = true

  # Binary authorization
  enable_binary_authorization = var.enable_binary_authorization

  # Workload Identity
  enable_workload_identity = true

  # Helm deployment
  deploy_helm_chart = var.deploy_erlmcp_to_gke
  helm_release_name = "erlmcp"
  helm_namespace    = var.kubernetes_namespace
  image_repository  = "${var.region}-docker.pkg.dev/${var.project_id}/erlmcp"
  image_name        = "erlmcp-server"
  image_tag         = var.image_tag

  labels = local.common_labels
}

# ============================================================================
# Cloud Run Module - Serverless Platform
# ============================================================================
module "cloud_run" {
  source = "../../modules/cloud-run"

  project_id = var.project_id
  region     = var.region

  service_name = "${var.cloud_run_service_prefix}-${var.environment}"

  # Container configuration
  image_repository = "${var.region}-docker.pkg.dev/${var.project_id}/erlmcp"
  image_name       = "erlmcp-server"
  image_tag        = var.image_tag
  container_port   = 8080

  # Gen2 execution environment
  execution_environment = "EXECUTION_ENVIRONMENT_GEN2"
  cpu                   = var.cloud_run_cpu
  memory                = var.cloud_run_memory
  cpu_idle              = var.cloud_run_cpu_idle
  startup_cpu_boost     = true
  timeout               = var.cloud_run_timeout

  # Scaling
  min_instances                    = var.cloud_run_min_instances
  max_instances                    = var.cloud_run_max_instances
  max_instance_request_concurrency = var.cloud_run_max_concurrency

  # VPC integration
  vpc_connector_name = var.cloud_run_vpc_connector
  vpc_egress         = var.cloud_run_vpc_egress
  network_interfaces = var.cloud_run_network_interfaces

  # Health checks
  health_check_path                = "/health"
  startup_probe_initial_delay      = 0
  startup_probe_timeout            = 3
  startup_probe_period             = 10
  startup_probe_failure_threshold  = 3
  liveness_probe_initial_delay     = 30
  liveness_probe_timeout           = 5
  liveness_probe_period            = 10
  liveness_probe_failure_threshold = 3

  # Environment variables
  container_env = [
    { name = "ERLMCP_ENV", value = var.environment },
    { name = "ERLMCP_VERSION", value = var.image_tag },
    { name = "ERL_AFLAGS", value = "-proto_dist inet_tls" },
    { name = "ERLMCP_TRANSPORT", value = "stdio" },
    { name = "LOG_LEVEL", value = var.log_level },
    { name = "ENABLE_METRICS", value = "true" },
    { name = "ENABLE_TRACING", value = "true" },
    { name = "GKE_CLUSTER_ENDPOINT", value = module.gke.cluster_endpoint }
  ]

  # Secret references
  container_secrets = [
    { name = "ERLANG_COOKIE", secret_name = "erlmcp-erlang-cookie", secret_version = "latest" },
    { name = "DB_PASSWORD", secret_name = "erlmcp-db-password", secret_version = "latest" },
    { name = "REDIS_PASSWORD", secret_name = "erlmcp-redis-password", secret_version = "latest" }
  ]

  # TLS certificates
  secret_volumes = var.enable_tls ? [
    {
      name        = "erlmcp-tls"
      mount_path  = "/etc/tls"
      secret_name = "erlmcp-tls-cert"
      items = [
        { key = "tls.crt", path = "cert.pem", mode = "0400" },
        { key = "tls.key", path = "key.pem", mode = "0400" },
        { key = "ca.crt", path = "ca.pem", mode = "0400" }
      ]
    }
  ] : []

  # IAM
  create_service_account = true
  service_account_roles = [
    "roles/secretmanager.secretAccessor",
    "roles/logging.logWriter",
    "roles/monitoring.metricWriter",
    "roles/cloudtrace.agent"
  ]
  allow_public_access      = var.allow_public_access
  invoker_service_accounts = var.cloud_run_invoker_accounts

  # Security
  binary_authorization_policy = var.enable_binary_authorization ? "projects/${var.project_id}/platforms/cloudRun/policies/default" : null
  session_affinity            = var.cloud_run_session_affinity

  # Domain mapping
  custom_domain         = var.cloud_run_custom_domain
  create_domain_mapping = var.cloud_run_custom_domain != ""

  labels      = local.common_labels
  annotations = {
    "marketplace.cloud.google.com/deployment-type" = "complete-deployment"
    "run.googleapis.com/description"               = "erlmcp v3 MCP SDK - Complete GCP deployment"
  }

  depends_on = [module.artifact_registry]
}

# ============================================================================
# Compute Engine Module - VM Platform
# ============================================================================
module "compute_engine" {
  source = "../../modules/compute-engine"

  project_id = var.project_id
  zone       = var.gce_zone

  instance_name  = "${var.gce_instance_prefix}-${var.environment}"
  instance_count = var.gce_instance_count
  machine_type   = var.gce_machine_type

  # Custom erlmcp image or base image
  source_image = var.gce_source_image

  # Disk configuration
  disk_type    = var.gce_disk_type
  disk_size_gb = var.gce_disk_size_gb

  # Network
  network    = module.vpc.network_name
  subnetwork = module.vpc.subnet_names[0]

  # Security
  enable_secure_boot          = true
  enable_integrity_monitoring = true
  enable_vtpm                 = true

  # Service account
  create_service_account = true
  service_account_roles = [
    "roles/secretmanager.secretAccessor",
    "roles/logging.logWriter",
    "roles/monitoring.metricWriter"
  ]

  # Firewall
  create_firewall_rules = true
  http_source_ranges    = var.gce_http_source_ranges

  # Metadata
  metadata = {
    environment      = var.environment
    erlmcp-version   = var.image_tag
    startup-script   = templatefile("${path.module}/scripts/startup.sh", {
      environment   = var.environment
      region        = var.region
      project_id    = var.project_id
      image_tag     = var.image_tag
    })
  }

  # Tags for firewall targeting
  network_tags = local.network_tags

  labels = local.common_labels
}

# ============================================================================
# Observability Module - Unified Monitoring
# ============================================================================
module "observability" {
  source = "../../modules/observability"

  project_id = var.project_id

  # Notification channels
  notification_channels = var.notification_channels

  # Uptime checks
  create_uptime_check = var.create_uptime_check
  uptime_check_host   = var.uptime_check_host != "" ? var.uptime_check_host : module.cloud_run.service_url
  uptime_check_path   = "/health"
  uptime_check_port   = 443
  uptime_check_use_ssl = true

  # SLOs
  create_slos               = var.create_slos
  availability_goal         = var.slo_availability_goal
  latency_threshold_ms      = var.slo_latency_threshold
  latency_goal              = var.slo_latency_goal

  # Alert policies
  enable_error_rate_alert        = true
  enable_latency_alert           = true
  enable_memory_alert            = true
  enable_cpu_alert               = true
  enable_health_check_alert      = true
  enable_process_count_alert     = true
  enable_cluster_health_alert    = var.deploy_erlmcp_to_gke
  enable_distribution_alert      = true

  # Alert thresholds
  error_rate_threshold       = var.alert_error_rate_threshold
  latency_threshold_ms       = var.alert_latency_threshold
  memory_threshold_percent   = var.alert_memory_threshold
  cpu_threshold_percent      = var.alert_cpu_threshold

  # Dashboards
  create_performance_dashboard = true
  create_erlang_dashboard      = true
  create_security_dashboard    = true
  create_cost_dashboard        = var.enable_cost_dashboard

  # Log export
  log_export_storage_enabled     = var.enable_log_export
  log_export_storage_destination = var.log_export_bucket
  log_export_bigquery_enabled    = var.enable_log_bigquery_export
  log_export_bigquery_dataset    = var.log_bigquery_dataset

  # Trace sampling
  enable_tracing      = true
  trace_sampling_rate = var.trace_sampling_rate
}

# ============================================================================
# Billing Module - Cost Management
# ============================================================================
module "billing" {
  source = "../../modules/billing"

  project_id = var.project_id

  # Budget alerts
  create_budget           = var.enable_budget_alerts
  budget_amount           = var.monthly_budget_amount
  budget_alert_thresholds = [0.5, 0.75, 0.9, 1.0]

  # Cost allocation labels
  labels = local.common_labels
}

# ============================================================================
# Load Balancer for Multi-Platform Routing
# ============================================================================
resource "google_compute_global_address" "erlmcp_lb" {
  count   = var.create_global_load_balancer ? 1 : 0
  project = var.project_id
  name    = "erlmcp-${var.environment}-global-lb-ip"
}

resource "google_compute_global_forwarding_rule" "erlmcp_https" {
  count                 = var.create_global_load_balancer ? 1 : 0
  project               = var.project_id
  name                  = "erlmcp-${var.environment}-https-forwarding-rule"
  ip_protocol           = "TCP"
  load_balancing_scheme = "EXTERNAL_MANAGED"
  port_range            = "443"
  target                = google_compute_target_https_proxy.erlmcp[0].id
  ip_address            = google_compute_global_address.erlmcp_lb[0].id
}

resource "google_compute_target_https_proxy" "erlmcp" {
  count            = var.create_global_load_balancer ? 1 : 0
  project          = var.project_id
  name             = "erlmcp-${var.environment}-https-proxy"
  url_map          = google_compute_url_map.erlmcp[0].id
  ssl_certificates = var.ssl_certificate_ids
}

resource "google_compute_url_map" "erlmcp" {
  count           = var.create_global_load_balancer ? 1 : 0
  project         = var.project_id
  name            = "erlmcp-${var.environment}-url-map"
  default_service = google_compute_backend_service.erlmcp_cloud_run[0].id

  # Route /gke to GKE backend
  host_rule {
    hosts        = [var.load_balancer_domain]
    path_matcher = "erlmcp-paths"
  }

  path_matcher {
    name            = "erlmcp-paths"
    default_service = google_compute_backend_service.erlmcp_cloud_run[0].id

    path_rule {
      paths   = ["/gke/*", "/k8s/*"]
      service = google_compute_backend_service.erlmcp_gke[0].id
    }

    path_rule {
      paths   = ["/vm/*", "/gce/*"]
      service = google_compute_backend_service.erlmcp_gce[0].id
    }
  }
}

resource "google_compute_backend_service" "erlmcp_cloud_run" {
  count                 = var.create_global_load_balancer ? 1 : 0
  project               = var.project_id
  name                  = "erlmcp-${var.environment}-cloud-run-backend"
  load_balancing_scheme = "EXTERNAL_MANAGED"
  protocol              = "HTTPS"
  timeout_sec           = 30

  backend {
    group = google_compute_region_network_endpoint_group.cloud_run[0].id
  }

  security_policy = var.enable_cloud_armor ? module.vpc.cloud_armor_policy_id : null

  log_config {
    enable      = true
    sample_rate = 1.0
  }
}

resource "google_compute_region_network_endpoint_group" "cloud_run" {
  count                 = var.create_global_load_balancer ? 1 : 0
  project               = var.project_id
  name                  = "erlmcp-${var.environment}-cloud-run-neg"
  region                = var.region
  network_endpoint_type = "SERVERLESS"

  cloud_run {
    service = module.cloud_run.service_name
  }
}

resource "google_compute_backend_service" "erlmcp_gke" {
  count                 = var.create_global_load_balancer && var.deploy_erlmcp_to_gke ? 1 : 0
  project               = var.project_id
  name                  = "erlmcp-${var.environment}-gke-backend"
  load_balancing_scheme = "EXTERNAL_MANAGED"
  protocol              = "HTTPS"
  timeout_sec           = 30

  backend {
    group           = google_compute_network_endpoint_group.gke[0].id
    balancing_mode  = "RATE"
    max_rate_per_endpoint = 100
  }

  security_policy = var.enable_cloud_armor ? module.vpc.cloud_armor_policy_id : null

  log_config {
    enable      = true
    sample_rate = 1.0
  }
}

resource "google_compute_network_endpoint_group" "gke" {
  count       = var.create_global_load_balancer && var.deploy_erlmcp_to_gke ? 1 : 0
  project     = var.project_id
  name        = "erlmcp-${var.environment}-gke-neg"
  zone        = var.gce_zone
  network     = module.vpc.network_name
  subnetwork  = module.vpc.subnet_names[0]
  default_port = 8080
}

resource "google_compute_backend_service" "erlmcp_gce" {
  count                 = var.create_global_load_balancer && var.gce_instance_count > 0 ? 1 : 0
  project               = var.project_id
  name                  = "erlmcp-${var.environment}-gce-backend"
  load_balancing_scheme = "EXTERNAL_MANAGED"
  protocol              = "HTTP"
  timeout_sec           = 30
  health_checks         = [google_compute_health_check.erlmcp_gce[0].id]

  backend {
    group = google_compute_instance_group.erlmcp_gce[0].id
  }

  security_policy = var.enable_cloud_armor ? module.vpc.cloud_armor_policy_id : null

  log_config {
    enable      = true
    sample_rate = 1.0
  }
}

resource "google_compute_instance_group" "erlmcp_gce" {
  count       = var.create_global_load_balancer && var.gce_instance_count > 0 ? 1 : 0
  project     = var.project_id
  name        = "erlmcp-${var.environment}-gce-instance-group"
  zone        = var.gce_zone
  instances   = module.compute_engine.instance_self_links

  named_port {
    name = "http"
    port = 8080
  }
}

resource "google_compute_health_check" "erlmcp_gce" {
  count   = var.create_global_load_balancer && var.gce_instance_count > 0 ? 1 : 0
  project = var.project_id
  name    = "erlmcp-${var.environment}-gce-health-check"

  http_health_check {
    port         = 8080
    request_path = "/health"
  }

  check_interval_sec  = 10
  timeout_sec         = 5
  healthy_threshold   = 2
  unhealthy_threshold = 3
}

# ============================================================================
# Cloud SQL (Optional) - Managed PostgreSQL
# ============================================================================
resource "google_sql_database_instance" "erlmcp" {
  count            = var.enable_cloud_sql ? 1 : 0
  project          = var.project_id
  name             = "erlmcp-${var.environment}-db"
  database_version = var.cloud_sql_database_version
  region           = var.region

  settings {
    tier              = var.cloud_sql_tier
    availability_type = var.cloud_sql_availability_type
    disk_type         = "PD_SSD"
    disk_size         = var.cloud_sql_disk_size_gb
    disk_autoresize   = true

    backup_configuration {
      enabled                        = true
      start_time                     = "03:00"
      point_in_time_recovery_enabled = true
      transaction_log_retention_days = 7
      backup_retention_settings {
        retained_backups = 30
      }
    }

    ip_configuration {
      ipv4_enabled    = false
      private_network = module.vpc.network_id
      require_ssl     = true
    }

    maintenance_window {
      day          = 7
      hour         = 3
      update_track = "stable"
    }

    database_flags {
      name  = "max_connections"
      value = "200"
    }

    insights_config {
      query_insights_enabled  = true
      query_plans_per_minute  = 5
      query_string_length     = 1024
      record_application_tags = true
    }
  }

  deletion_protection = var.environment == "production"

  depends_on = [module.vpc]
}

resource "google_sql_database" "erlmcp" {
  count    = var.enable_cloud_sql ? 1 : 0
  project  = var.project_id
  name     = "erlmcp"
  instance = google_sql_database_instance.erlmcp[0].name
}

# ============================================================================
# Outputs - Deployment Information
# ============================================================================
output "deployment_summary" {
  description = "Complete deployment summary"
  value = {
    environment = var.environment
    project_id  = var.project_id
    region      = var.region
    version     = var.image_tag
  }
}

output "network" {
  description = "Network configuration"
  value = {
    network_name  = module.vpc.network_name
    network_id    = module.vpc.network_id
    subnet_names  = module.vpc.subnet_names
    nat_ips       = module.vpc.nat_ips
  }
}

output "gke" {
  description = "GKE cluster information"
  value = {
    cluster_name     = module.gke.cluster_name
    cluster_endpoint = module.gke.cluster_endpoint
    cluster_ca_cert  = module.gke.cluster_ca_certificate
    kubectl_command  = module.gke.kubectl_config_command
    service_account  = module.gke.service_account_email
  }
  sensitive = true
}

output "cloud_run" {
  description = "Cloud Run service information"
  value = {
    service_name        = module.cloud_run.service_name
    service_url         = module.cloud_run.service_url
    service_account     = module.cloud_run.service_account_email
    latest_revision     = module.cloud_run.latest_ready_revision
    custom_domain_url   = var.cloud_run_custom_domain != "" ? "https://${var.cloud_run_custom_domain}" : null
  }
}

output "compute_engine" {
  description = "Compute Engine instance information"
  value = {
    instance_names = module.compute_engine.instance_names
    instance_ips   = module.compute_engine.instance_external_ips
    ssh_command    = module.compute_engine.ssh_command
    service_account = module.compute_engine.service_account_email
  }
}

output "load_balancer" {
  description = "Global load balancer information"
  value = var.create_global_load_balancer ? {
    ip_address = google_compute_global_address.erlmcp_lb[0].address
    url        = "https://${var.load_balancer_domain}"
    endpoints = {
      cloud_run = "https://${var.load_balancer_domain}/"
      gke       = "https://${var.load_balancer_domain}/gke"
      gce       = "https://${var.load_balancer_domain}/vm"
    }
  } : null
}

output "observability" {
  description = "Observability and monitoring"
  value = {
    dashboards = {
      performance = module.observability.performance_dashboard_url
      erlang      = module.observability.erlang_dashboard_url
      security    = module.observability.security_dashboard_url
    }
    uptime_check_id = module.observability.uptime_check_id
  }
}

output "cloud_sql" {
  description = "Cloud SQL database information"
  value = var.enable_cloud_sql ? {
    instance_name        = google_sql_database_instance.erlmcp[0].name
    connection_name      = google_sql_database_instance.erlmcp[0].connection_name
    private_ip_address   = google_sql_database_instance.erlmcp[0].private_ip_address
  } : null
}

output "artifact_registry" {
  description = "Artifact Registry repository"
  value = {
    repository_url = module.artifact_registry.repository_url
    docker_push_command = "docker push ${var.region}-docker.pkg.dev/${var.project_id}/erlmcp/erlmcp-server:${var.image_tag}"
  }
}

output "service_accounts" {
  description = "All created service accounts"
  value = {
    gke              = module.gke.service_account_email
    cloud_run        = module.cloud_run.service_account_email
    compute_engine   = module.compute_engine.service_account_email
    orchestrator     = module.iam.service_account_emails["erlmcp-orchestrator"]
  }
}

output "quick_start_commands" {
  description = "Commands to get started with deployment"
  value = <<-EOT
    # Configure kubectl for GKE
    ${module.gke.kubectl_config_command}

    # Check GKE deployment
    kubectl get pods -n ${var.kubernetes_namespace}

    # Test Cloud Run service
    curl ${module.cloud_run.service_url}/health

    # SSH to Compute Engine (first instance)
    ${module.compute_engine.ssh_command}

    # View logs
    gcloud logging read "resource.type=cloud_run_revision AND resource.labels.service_name=${module.cloud_run.service_name}" --limit 50

    # View monitoring dashboard
    echo "Performance: ${module.observability.performance_dashboard_url}"
  EOT
}
