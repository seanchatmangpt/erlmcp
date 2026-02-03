# Output definitions for Kubernetes Infrastructure

# Resource names
output "namespace" {
  description = "erlmcp namespace name"
  value       = kubernetes_namespace.erlmcp.metadata[0].name
}

output "service_account" {
  description = "erlmcp service account name"
  value       = kubernetes_service_account.erlmcp.metadata[0].name
}

output "config_map" {
  description = "erlmcp configuration map"
  value       = kubernetes_config_map.erlmcp_config.metadata[0].name
}

# Secrets (sensitive)
output "secret_name" {
  description = "erlmcp secrets name"
  value       = kubernetes_secret.erlmcp_secrets.metadata[0].name
}

output "tls_secret" {
  description = "erlmcp TLS secret name"
  value       = kubernetes_secret.erlmcp_tls.metadata[0].name
}

# Kubernetes endpoints
output "cluster_endpoint" {
  description = "erlmcp cluster endpoint"
  value       = "https://erlmcp.${var.domain}"
}

# HPA configuration
output "hpa_name" {
  description = "Horizontal Pod Autoscaler name"
  value       = kubernetes_horizontal_pod_autoscaler_v2.erlmcp.metadata[0].name
}

output "min_replicas" {
  description = "Minimum number of replicas"
  value       = kubernetes_horizontal_pod_autoscaler_v2.erlmcp.spec[0].min_replicas
}

output "max_replicas" {
  description = "Maximum number of replicas"
  value       = kubernetes_horizontal_pod_autoscaler_v2.erlmcp.spec[0].max_replicas
}

# Network policy
output "network_policy" {
  description = "Network policy name"
  value       = kubernetes_network_policy.erlmcp.metadata[0].name
}

# Pod Disruption Budget
output "pdb_name" {
  description = "Pod Disruption Budget name"
  value       = kubernetes_pod_disruption_budget.erlmcp.metadata[0].name
}

# Resource counts
output "resource_labels" {
  description = "Resource labels for erlmcp"
  value       = {
    "app.kubernetes.io/name"      = "erlmcp"
    "app.kubernetes.io/managed-by" = "terraform"
    "environment"                 = var.environment
  }
}

# Environment-specific outputs
output "environment_config" {
  description = "Environment-specific configuration"
  value       = {
    environment = var.environment
    region      = var.region
    cluster     = var.cluster_name
    replicas    = var.env_specific[var.environment].replicas
    cpu         = var.env_specific[var.environment].cpu
    memory      = var.env_specific[var.environment].memory
    log_level   = var.env_specific[var.environment].log_level
  }
}

# Performance metrics
output "performance_config" {
  description = "Performance-related configuration"
  value       = {
    target_cpu_utilization    = var.target_cpu_utilization
    target_memory_utilization = var.target_memory_utilization
    min_replicas              = var.min_replicas
    max_replicas              = var.max_replicas
  }
}

# Security configuration
output "security_config" {
  description = "Security-related configuration"
  value       = {
    image_pull_policy       = var.image_pull_policy
    image_pull_secrets      = var.image_pull_secrets
    allow_privilege_escalation = var.container_security_context.allow_privilege_escalation
    read_only_root_filesystem = var.container_security_context.read_only_root_filesystem
    run_as_non_root         = var.container_security_context.run_as_non_root
  }
}

# Health check configuration
output "health_config" {
  description = "Health check configuration"
  value       = {
    liveness_path  = var.liveness_probe.path
    readiness_path = var.readiness_probe.path
    timeout        = var.liveness_probe.timeout_seconds
    period         = var.liveness_probe.period_seconds
  }
}