# ============================================================================
# Cloud Monitoring Alert Policies for erlmcp Infrastructure
# Comprehensive monitoring for GKE, Cloud Run, and GCE deployments
# Updated: 2026-02
# ============================================================================

terraform {
  required_version = ">= 1.8.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 6.0.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = ">= 6.0.0"
    }
  }
}

# ============================================================================
# Variables
# ============================================================================

variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "environment" {
  description = "Environment name (dev, staging, prod)"
  type        = string
  default     = "prod"
}

variable "notification_channels" {
  description = "List of notification channel IDs for alerts"
  type = object({
    critical = list(string)
    warning  = list(string)
    info     = list(string)
  })
  default = {
    critical = []
    warning  = []
    info     = []
  }
}

variable "enable_gke_alerts" {
  description = "Enable GKE-specific alerts"
  type        = bool
  default     = true
}

variable "enable_cloud_run_alerts" {
  description = "Enable Cloud Run-specific alerts"
  type        = bool
  default     = true
}

variable "enable_gce_alerts" {
  description = "Enable GCE-specific alerts"
  type        = bool
  default     = true
}

variable "gke_cluster_names" {
  description = "List of GKE cluster names to monitor"
  type        = list(string)
  default     = []
}

variable "cloud_run_service_names" {
  description = "List of Cloud Run service names to monitor"
  type        = list(string)
  default     = []
}

variable "gce_instance_labels" {
  description = "Labels to filter GCE instances for monitoring"
  type        = map(string)
  default     = { service = "erlmcp" }
}

# ============================================================================
# GKE ALERTS
# ============================================================================

# GKE Cluster - Node Not Ready
resource "google_monitoring_alert_policy" "gke_node_not_ready" {
  count        = var.enable_gke_alerts ? 1 : 0
  project      = var.project_id
  display_name = "GKE Node Not Ready - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Node not ready for 5+ minutes"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"kubernetes.io/node/ready\"",
        "resource.type=\"k8s_node\"",
        "metric.label.status=\"false\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 0
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_MAX"
        cross_series_reducer = "REDUCE_COUNT"
        group_by_fields      = ["resource.cluster_name", "resource.node_name"]
      }
    }
  }

  notification_channels = var.notification_channels.critical

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## GKE Node Not Ready - erlmcp

      **Severity:** CRITICAL

      **Description:**
      One or more GKE nodes are not in Ready state for more than 5 minutes.

      **Impact:**
      - Reduced cluster capacity
      - Pod scheduling failures
      - Potential service degradation

      **Immediate Actions:**
      1. Check node status:
         ```bash
         kubectl get nodes
         kubectl describe node <node-name>
         ```

      2. Check node system logs:
         ```bash
         gcloud logging read 'resource.type=k8s_node AND resource.labels.node_name=<node-name>' --limit 50
         ```

      3. Common causes:
         - Resource exhaustion (CPU/memory/disk)
         - Kubelet or container runtime issues
         - Network connectivity problems
         - Cloud provider issues

      4. If needed, cordon and drain node:
         ```bash
         kubectl cordon <node-name>
         kubectl drain <node-name> --ignore-daemonsets --delete-emptydir-data
         ```

      **Escalation:** Platform Team (#platform-oncall)
    EOT
  }

  alert_strategy {
    notification_rate_limit {
      period = "600s"
    }
    auto_close = "7200s"
  }

  severity = "CRITICAL"

  user_labels = {
    environment = var.environment
    service     = "erlmcp"
    platform    = "gke"
    severity    = "critical"
  }
}

# GKE Cluster - High Pod Failure Rate
resource "google_monitoring_alert_policy" "gke_pod_failures" {
  count        = var.enable_gke_alerts ? 1 : 0
  project      = var.project_id
  display_name = "GKE High Pod Failure Rate - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Pod failure rate above threshold"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"kubernetes.io/container/restart_count\"",
        "resource.type=\"k8s_container\"",
        "resource.label.namespace_name!=\"kube-system\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 5
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
        group_by_fields      = ["resource.namespace_name", "resource.pod_name"]
      }
    }
  }

  notification_channels = var.notification_channels.critical

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## High Pod Failure Rate - erlmcp GKE

      **Severity:** CRITICAL

      **Description:**
      Pods are restarting at an elevated rate, indicating application crashes or OOM kills.

      **Troubleshooting:**
      1. Check pod status:
         ```bash
         kubectl get pods --all-namespaces | grep -v Running
         kubectl describe pod <pod-name> -n <namespace>
         ```

      2. Check pod logs:
         ```bash
         kubectl logs <pod-name> -n <namespace> --previous
         ```

      3. Common causes:
         - Application crashes (check exit codes)
         - Out of memory (OOMKilled)
         - Failed health checks
         - Missing dependencies or config
         - Resource limits too low

      4. Check resource usage:
         ```bash
         kubectl top pod <pod-name> -n <namespace>
         ```

      **Runbook:** https://docs.erlmcp.dev/runbooks/gke-pod-failures
    EOT
  }

  alert_strategy {
    auto_close = "3600s"
  }

  severity = "CRITICAL"
}

# GKE Cluster - CPU Throttling
resource "google_monitoring_alert_policy" "gke_cpu_throttling" {
  count        = var.enable_gke_alerts ? 1 : 0
  project      = var.project_id
  display_name = "GKE High CPU Throttling - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Container CPU throttling above 25%"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"kubernetes.io/container/cpu/core_usage_time\"",
        "resource.type=\"k8s_container\"",
        "resource.label.namespace_name!=\"kube-system\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 0.25
      duration        = "600s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_RATE"
      }
    }
  }

  notification_channels = var.notification_channels.warning

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## High CPU Throttling - erlmcp GKE

      **Severity:** WARNING

      **Description:**
      Containers are being CPU throttled, indicating resource limits may be too low.

      **Actions:**
      1. Review container resource limits and requests
      2. Check if CPU limits can be increased
      3. Consider horizontal pod autoscaling
      4. Profile application for CPU-intensive operations

      **Runbook:** https://docs.erlmcp.dev/runbooks/gke-cpu-throttling
    EOT
  }

  alert_strategy {
    auto_close = "7200s"
  }

  severity = "WARNING"
}

# GKE Cluster - Memory Pressure
resource "google_monitoring_alert_policy" "gke_memory_pressure" {
  count        = var.enable_gke_alerts ? 1 : 0
  project      = var.project_id
  display_name = "GKE Node Memory Pressure - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Node memory usage above 85%"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"kubernetes.io/node/memory/allocatable_utilization\"",
        "resource.type=\"k8s_node\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 0.85
      duration        = "300s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }
    }
  }

  notification_channels = var.notification_channels.critical

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## Node Memory Pressure - erlmcp GKE

      **Severity:** CRITICAL

      **Description:**
      Node memory usage is critically high, risk of OOM kills and pod evictions.

      **Immediate Actions:**
      1. Check node memory:
         ```bash
         kubectl top nodes
         kubectl describe node <node-name>
         ```

      2. Check for memory-heavy pods:
         ```bash
         kubectl top pods --all-namespaces --sort-by=memory
         ```

      3. Look for memory leaks in application
      4. Consider scaling out (adding nodes)
      5. Review pod memory requests/limits

      **Runbook:** https://docs.erlmcp.dev/runbooks/gke-memory-pressure
    EOT
  }

  alert_strategy {
    auto_close = "1800s"
  }

  severity = "CRITICAL"
}

# GKE Cluster - Disk Pressure
resource "google_monitoring_alert_policy" "gke_disk_pressure" {
  count        = var.enable_gke_alerts ? 1 : 0
  project      = var.project_id
  display_name = "GKE Node Disk Pressure - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Node ephemeral storage usage above 80%"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"kubernetes.io/node/ephemeral_storage/allocatable_utilization\"",
        "resource.type=\"k8s_node\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 0.80
      duration        = "300s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }
    }
  }

  notification_channels = var.notification_channels.critical

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## Node Disk Pressure - erlmcp GKE

      **Severity:** CRITICAL

      **Description:**
      Node disk space is critically low, pods may be evicted.

      **Actions:**
      1. Check disk usage on node
      2. Clean up unused Docker images
      3. Check for excessive logging
      4. Review emptyDir volume usage
      5. Consider increasing disk size or adding nodes

      **Runbook:** https://docs.erlmcp.dev/runbooks/gke-disk-pressure
    EOT
  }

  alert_strategy {
    auto_close = "1800s"
  }

  severity = "CRITICAL"
}

# GKE Cluster - API Server Latency
resource "google_monitoring_alert_policy" "gke_apiserver_latency" {
  count        = var.enable_gke_alerts ? 1 : 0
  project      = var.project_id
  display_name = "GKE API Server High Latency - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "API server request latency P99 > 1s"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"kubernetes.io/control_plane/request_latencies\"",
        "resource.type=\"k8s_cluster\"",
        "metric.label.verb!=\"WATCH\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 1000
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_DELTA"
        cross_series_reducer = "REDUCE_PERCENTILE_99"
      }
    }
  }

  notification_channels = var.notification_channels.warning

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## GKE API Server High Latency

      **Severity:** WARNING

      **Description:**
      Kubernetes API server is experiencing high latency.

      **Impact:**
      - Slow kubectl commands
      - Delayed pod scheduling
      - Slow autoscaling responses

      **Runbook:** https://docs.erlmcp.dev/runbooks/gke-apiserver-latency
    EOT
  }

  alert_strategy {
    auto_close = "3600s"
  }

  severity = "WARNING"
}

# GKE Cluster - Deployment Replica Mismatch
resource "google_monitoring_alert_policy" "gke_deployment_replica_mismatch" {
  count        = var.enable_gke_alerts ? 1 : 0
  project      = var.project_id
  display_name = "GKE Deployment Replica Mismatch - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Available replicas less than desired for 10+ minutes"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"kubernetes.io/deployment/replicas/available\"",
        "resource.type=\"k8s_deployment\""
      ])

      comparison      = "COMPARISON_LT"
      threshold_value = 1
      duration        = "600s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MIN"
      }
    }
  }

  notification_channels = var.notification_channels.critical

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## GKE Deployment Replica Mismatch

      **Severity:** CRITICAL

      **Description:**
      Deployment has fewer available replicas than desired.

      **Actions:**
      1. Check deployment status:
         ```bash
         kubectl get deployments --all-namespaces
         kubectl describe deployment <name> -n <namespace>
         ```

      2. Common causes:
         - Insufficient cluster resources
         - Image pull failures
         - Crash loops
         - Failed health checks

      **Runbook:** https://docs.erlmcp.dev/runbooks/gke-replica-mismatch
    EOT
  }

  alert_strategy {
    auto_close = "1800s"
  }

  severity = "CRITICAL"
}

# ============================================================================
# CLOUD RUN ALERTS
# ============================================================================

# Cloud Run - High Error Rate
resource "google_monitoring_alert_policy" "cloudrun_high_error_rate" {
  count        = var.enable_cloud_run_alerts ? 1 : 0
  project      = var.project_id
  display_name = "Cloud Run High Error Rate - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Error rate above 5%"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"run.googleapis.com/request_count\"",
        "resource.type=\"cloud_run_revision\"",
        "metric.label.response_code_class=\"5xx\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 0.05
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
        group_by_fields      = ["resource.service_name", "resource.revision_name"]
      }

      denominator_filter = join(" AND ", [
        "metric.type=\"run.googleapis.com/request_count\"",
        "resource.type=\"cloud_run_revision\""
      ])

      denominator_aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
        group_by_fields      = ["resource.service_name", "resource.revision_name"]
      }
    }
  }

  notification_channels = var.notification_channels.critical

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## Cloud Run High Error Rate - erlmcp

      **Severity:** CRITICAL

      **Description:**
      Cloud Run service is experiencing elevated 5xx error rate above 5%.

      **Troubleshooting:**
      1. Check recent logs:
         ```bash
         gcloud logging read 'resource.type=cloud_run_revision AND severity>=ERROR' \
           --limit 50 --format json
         ```

      2. Check service health:
         ```bash
         gcloud run services describe <service-name> --region=<region>
         gcloud run revisions list --service=<service-name> --region=<region>
         ```

      3. Common causes:
         - Application crashes or exceptions
         - Resource limits (CPU/memory)
         - Startup timeout
         - Health check failures
         - Dependency issues (databases, APIs)

      4. Check metrics dashboard for:
         - Request latency spikes
         - Container instance count
         - Memory/CPU utilization

      **Runbook:** https://docs.erlmcp.dev/runbooks/cloudrun-errors
    EOT
  }

  alert_strategy {
    notification_rate_limit {
      period = "600s"
    }
    auto_close = "3600s"
  }

  severity = "CRITICAL"
}

# Cloud Run - High Request Latency
resource "google_monitoring_alert_policy" "cloudrun_high_latency" {
  count        = var.enable_cloud_run_alerts ? 1 : 0
  project      = var.project_id
  display_name = "Cloud Run High Request Latency - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "P95 latency above 2 seconds"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"run.googleapis.com/request_latencies\"",
        "resource.type=\"cloud_run_revision\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 2000
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_DELTA"
        cross_series_reducer = "REDUCE_PERCENTILE_95"
        group_by_fields      = ["resource.service_name"]
      }
    }
  }

  notification_channels = var.notification_channels.warning

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## Cloud Run High Latency - erlmcp

      **Severity:** WARNING

      **Description:**
      95th percentile request latency exceeds 2 seconds.

      **Actions:**
      1. Check for cold starts
      2. Review application performance
      3. Check CPU throttling
      4. Verify external dependency latency
      5. Consider increasing CPU allocation

      **Runbook:** https://docs.erlmcp.dev/runbooks/cloudrun-latency
    EOT
  }

  alert_strategy {
    auto_close = "3600s"
  }

  severity = "WARNING"
}

# Cloud Run - Container Instance Crashes
resource "google_monitoring_alert_policy" "cloudrun_container_crashes" {
  count        = var.enable_cloud_run_alerts ? 1 : 0
  project      = var.project_id
  display_name = "Cloud Run Container Crashes - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Container instance startup failures"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"run.googleapis.com/container/startup_latencies\"",
        "resource.type=\"cloud_run_revision\"",
        "metric.label.state=\"failed\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 3
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
        group_by_fields      = ["resource.service_name", "resource.revision_name"]
      }
    }
  }

  notification_channels = var.notification_channels.critical

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## Cloud Run Container Crashes - erlmcp

      **Severity:** CRITICAL

      **Description:**
      Container instances are failing to start successfully.

      **Troubleshooting:**
      1. Check container logs for startup errors
      2. Verify health check configuration
      3. Check for out-of-memory errors
      4. Verify environment variables and secrets
      5. Check container image integrity

      **Runbook:** https://docs.erlmcp.dev/runbooks/cloudrun-crashes
    EOT
  }

  alert_strategy {
    auto_close = "1800s"
  }

  severity = "CRITICAL"
}

# Cloud Run - High Cold Start Rate
resource "google_monitoring_alert_policy" "cloudrun_cold_starts" {
  count        = var.enable_cloud_run_alerts ? 1 : 0
  project      = var.project_id
  display_name = "Cloud Run High Cold Start Rate - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Cold start rate above 20%"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"run.googleapis.com/request_count\"",
        "resource.type=\"cloud_run_revision\"",
        "metric.label.state=\"cold\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 0.20
      duration        = "600s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
        group_by_fields      = ["resource.service_name"]
      }

      denominator_filter = join(" AND ", [
        "metric.type=\"run.googleapis.com/request_count\"",
        "resource.type=\"cloud_run_revision\""
      ])

      denominator_aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
        group_by_fields      = ["resource.service_name"]
      }
    }
  }

  notification_channels = var.notification_channels.warning

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## Cloud Run High Cold Start Rate

      **Severity:** WARNING

      **Description:**
      More than 20% of requests are experiencing cold starts.

      **Actions:**
      1. Consider increasing min instances
      2. Enable CPU allocation always
      3. Optimize container image size
      4. Use startup CPU boost
      5. Review traffic patterns

      **Runbook:** https://docs.erlmcp.dev/runbooks/cloudrun-cold-starts
    EOT
  }

  alert_strategy {
    auto_close = "7200s"
  }

  severity = "WARNING"
}

# Cloud Run - Memory Utilization
resource "google_monitoring_alert_policy" "cloudrun_high_memory" {
  count        = var.enable_cloud_run_alerts ? 1 : 0
  project      = var.project_id
  display_name = "Cloud Run High Memory Usage - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Memory utilization above 90%"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"run.googleapis.com/container/memory/utilizations\"",
        "resource.type=\"cloud_run_revision\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 0.90
      duration        = "300s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }
    }
  }

  notification_channels = var.notification_channels.critical

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## Cloud Run High Memory Usage

      **Severity:** CRITICAL

      **Description:**
      Container memory usage is critically high, risk of OOM kills.

      **Actions:**
      1. Check for memory leaks
      2. Increase memory allocation
      3. Review application memory usage patterns
      4. Check for large in-memory data structures

      **Runbook:** https://docs.erlmcp.dev/runbooks/cloudrun-memory
    EOT
  }

  alert_strategy {
    auto_close = "1800s"
  }

  severity = "CRITICAL"
}

# Cloud Run - CPU Utilization
resource "google_monitoring_alert_policy" "cloudrun_high_cpu" {
  count        = var.enable_cloud_run_alerts ? 1 : 0
  project      = var.project_id
  display_name = "Cloud Run High CPU Usage - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "CPU utilization above 80%"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"run.googleapis.com/container/cpu/utilizations\"",
        "resource.type=\"cloud_run_revision\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 0.80
      duration        = "600s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }
    }
  }

  notification_channels = var.notification_channels.warning

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## Cloud Run High CPU Usage

      **Severity:** WARNING

      **Description:**
      Container CPU usage is consistently high, may cause request throttling.

      **Actions:**
      1. Increase CPU allocation
      2. Enable request concurrency scaling
      3. Profile application for CPU-intensive operations
      4. Consider horizontal scaling

      **Runbook:** https://docs.erlmcp.dev/runbooks/cloudrun-cpu
    EOT
  }

  alert_strategy {
    auto_close = "3600s"
  }

  severity = "WARNING"
}

# ============================================================================
# GCE ALERTS
# ============================================================================

# GCE - Instance Down
resource "google_monitoring_alert_policy" "gce_instance_down" {
  count        = var.enable_gce_alerts ? 1 : 0
  project      = var.project_id
  display_name = "GCE Instance Down - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Instance not responding"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"compute.googleapis.com/instance/uptime\"",
        "resource.type=\"gce_instance\"",
        join(" OR ", [for k, v in var.gce_instance_labels : "metadata.user_labels.${k}=\"${v}\""]),
      ])

      comparison      = "COMPARISON_LT"
      threshold_value = 60
      duration        = "300s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MAX"
      }
    }
  }

  notification_channels = var.notification_channels.critical

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## GCE Instance Down - erlmcp

      **Severity:** CRITICAL

      **Description:**
      GCE instance has low uptime or is not responding.

      **Immediate Actions:**
      1. Check instance status:
         ```bash
         gcloud compute instances list --filter="labels.service=erlmcp"
         gcloud compute instances describe <instance-name> --zone=<zone>
         ```

      2. Check serial console output:
         ```bash
         gcloud compute instances get-serial-port-output <instance-name> --zone=<zone>
         ```

      3. Common causes:
         - Instance crash or kernel panic
         - Resource exhaustion
         - Startup script failures
         - Metadata server issues

      4. If needed, restart instance:
         ```bash
         gcloud compute instances stop <instance-name> --zone=<zone>
         gcloud compute instances start <instance-name> --zone=<zone>
         ```

      **Runbook:** https://docs.erlmcp.dev/runbooks/gce-instance-down
    EOT
  }

  alert_strategy {
    notification_rate_limit {
      period = "300s"
    }
    auto_close = "1800s"
  }

  severity = "CRITICAL"
}

# GCE - High CPU Utilization
resource "google_monitoring_alert_policy" "gce_high_cpu" {
  count        = var.enable_gce_alerts ? 1 : 0
  project      = var.project_id
  display_name = "GCE High CPU Utilization - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "CPU utilization above 85%"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"compute.googleapis.com/instance/cpu/utilization\"",
        "resource.type=\"gce_instance\"",
        join(" OR ", [for k, v in var.gce_instance_labels : "metadata.user_labels.${k}=\"${v}\""]),
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 0.85
      duration        = "600s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }
    }
  }

  notification_channels = var.notification_channels.warning

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## GCE High CPU Utilization - erlmcp

      **Severity:** WARNING

      **Description:**
      Instance CPU usage sustained above 85% for 10+ minutes.

      **Actions:**
      1. Check top processes:
         ```bash
         gcloud compute ssh <instance-name> --zone=<zone> --command="top -bn1"
         ```

      2. Review Erlang VM scheduler utilization
      3. Check for CPU-intensive operations
      4. Consider upgrading machine type
      5. Review autoscaling configuration

      **Runbook:** https://docs.erlmcp.dev/runbooks/gce-high-cpu
    EOT
  }

  alert_strategy {
    auto_close = "3600s"
  }

  severity = "WARNING"
}

# GCE - High Memory Usage
resource "google_monitoring_alert_policy" "gce_high_memory" {
  count        = var.enable_gce_alerts ? 1 : 0
  project      = var.project_id
  display_name = "GCE High Memory Usage - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Memory usage above 90%"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"agent.googleapis.com/memory/percent_used\"",
        "resource.type=\"gce_instance\"",
        "metric.label.state=\"used\"",
        join(" OR ", [for k, v in var.gce_instance_labels : "metadata.user_labels.${k}=\"${v}\""]),
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 90
      duration        = "300s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }
    }
  }

  notification_channels = var.notification_channels.critical

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## GCE High Memory Usage - erlmcp

      **Severity:** CRITICAL

      **Description:**
      Instance memory usage above 90%, risk of OOM killer.

      **Immediate Actions:**
      1. Check memory usage:
         ```bash
         gcloud compute ssh <instance-name> --zone=<zone> --command="free -h"
         ```

      2. Check Erlang VM memory:
         ```erlang
         erlang:memory().
         recon:proc_count(memory, 10).
         ```

      3. Common causes:
         - Memory leak in application
         - ETS table growth
         - Message queue buildup
         - Insufficient memory for workload

      4. Consider immediate actions:
         - Restart service if memory leak
         - Upgrade to larger instance
         - Review process memory usage

      **Runbook:** https://docs.erlmcp.dev/runbooks/gce-memory
    EOT
  }

  alert_strategy {
    auto_close = "1800s"
  }

  severity = "CRITICAL"
}

# GCE - High Disk Utilization
resource "google_monitoring_alert_policy" "gce_high_disk" {
  count        = var.enable_gce_alerts ? 1 : 0
  project      = var.project_id
  display_name = "GCE High Disk Utilization - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Disk usage above 85%"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"agent.googleapis.com/disk/percent_used\"",
        "resource.type=\"gce_instance\"",
        "metric.label.device!~\"loop.*\"",
        join(" OR ", [for k, v in var.gce_instance_labels : "metadata.user_labels.${k}=\"${v}\""]),
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 85
      duration        = "300s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }
    }
  }

  notification_channels = var.notification_channels.warning

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## GCE High Disk Utilization - erlmcp

      **Severity:** WARNING

      **Description:**
      Disk space usage above 85%.

      **Actions:**
      1. Check disk usage:
         ```bash
         gcloud compute ssh <instance-name> --zone=<zone> --command="df -h"
         ```

      2. Find large files/directories:
         ```bash
         gcloud compute ssh <instance-name> --zone=<zone> --command="du -sh /* | sort -h"
         ```

      3. Common causes:
         - Log file accumulation
         - Temporary file buildup
         - Database growth
         - Old release artifacts

      4. Cleanup actions:
         - Rotate and compress logs
         - Clean up temp files
         - Remove old deployments
         - Increase disk size if needed

      **Runbook:** https://docs.erlmcp.dev/runbooks/gce-disk
    EOT
  }

  alert_strategy {
    auto_close = "3600s"
  }

  severity = "WARNING"
}

# GCE - Network Errors
resource "google_monitoring_alert_policy" "gce_network_errors" {
  count        = var.enable_gce_alerts ? 1 : 0
  project      = var.project_id
  display_name = "GCE Network Errors - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Network packet drops above threshold"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"compute.googleapis.com/instance/network/received_packets_count\"",
        "resource.type=\"gce_instance\"",
        "metric.label.dropped=\"true\"",
        join(" OR ", [for k, v in var.gce_instance_labels : "metadata.user_labels.${k}=\"${v}\""]),
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 100
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
      }
    }
  }

  notification_channels = var.notification_channels.warning

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## GCE Network Errors - erlmcp

      **Severity:** WARNING

      **Description:**
      Instance experiencing elevated network packet drops.

      **Actions:**
      1. Check network performance metrics
      2. Review VPC firewall rules
      3. Check for network congestion
      4. Verify network interface configuration
      5. Consider upgrading to gVNIC

      **Runbook:** https://docs.erlmcp.dev/runbooks/gce-network
    EOT
  }

  alert_strategy {
    auto_close = "3600s"
  }

  severity = "WARNING"
}

# GCE - Erlang VM Process Count
resource "google_monitoring_alert_policy" "gce_erlang_process_count" {
  count        = var.enable_gce_alerts ? 1 : 0
  project      = var.project_id
  display_name = "GCE Erlang High Process Count - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Erlang process count above threshold"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"custom.googleapis.com/erlmcp/process_count\"",
        "resource.type=\"gce_instance\"",
        join(" OR ", [for k, v in var.gce_instance_labels : "metadata.user_labels.${k}=\"${v}\""]),
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 100000
      duration        = "300s"

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }
    }
  }

  notification_channels = var.notification_channels.warning

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## Erlang High Process Count - erlmcp

      **Severity:** WARNING

      **Description:**
      Erlang VM process count is unusually high.

      **Actions:**
      1. Connect to node and check processes:
         ```erlang
         erlang:system_info(process_count).
         recon:proc_count(memory, 10).
         ```

      2. Check for process leaks
      3. Review supervisor trees
      4. Check for connection pooling issues

      **Runbook:** https://docs.erlmcp.dev/runbooks/erlang-processes
    EOT
  }

  alert_strategy {
    auto_close = "3600s"
  }

  severity = "WARNING"
}

# ============================================================================
# CROSS-PLATFORM ALERTS
# ============================================================================

# Uptime Check - HTTP/HTTPS Health
resource "google_monitoring_alert_policy" "uptime_check_failure" {
  project      = var.project_id
  display_name = "Service Uptime Check Failure - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Uptime check failing from multiple regions"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"monitoring.googleapis.com/uptime_check/check_passed\"",
        "resource.type=\"uptime_url\"",
        "metric.label.check_id=monitoring.regex.full_match(\"erlmcp.*\")"
      ])

      comparison      = "COMPARISON_LT"
      threshold_value = 0.5
      duration        = "180s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_FRACTION_TRUE"
        cross_series_reducer = "REDUCE_MEAN"
      }
    }
  }

  notification_channels = var.notification_channels.critical

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## Service Uptime Check Failure - erlmcp

      **Severity:** CRITICAL

      **Description:**
      Service health checks are failing from multiple monitoring locations.

      **Impact:**
      - Service may be unavailable to end users
      - SLO breach likely

      **Immediate Actions:**
      1. Check service status across all platforms (GKE/Cloud Run/GCE)
      2. Verify load balancer health
      3. Check for recent deployments
      4. Review error logs
      5. Check for infrastructure issues

      **Runbook:** https://docs.erlmcp.dev/runbooks/service-down
    EOT
  }

  alert_strategy {
    notification_rate_limit {
      period = "300s"
    }
    auto_close = "600s"
  }

  severity = "CRITICAL"

  user_labels = {
    environment = var.environment
    service     = "erlmcp"
    severity    = "critical"
  }
}

# Log-based Metric - Critical Errors
resource "google_monitoring_alert_policy" "critical_log_errors" {
  project      = var.project_id
  display_name = "Critical Log Errors - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Critical errors in logs"
    condition_matched_log {
      filter = join(" AND ", [
        "severity>=CRITICAL",
        "resource.labels.service=\"erlmcp\"",
        "NOT protoPayload.methodName=\"google.cloud.internal.*\""
      ])
      label_extractors = {
        "error_type" = "EXTRACT(jsonPayload.error_type)"
      }
    }
  }

  notification_channels = var.notification_channels.critical

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## Critical Log Errors - erlmcp

      **Severity:** CRITICAL

      **Description:**
      Critical-level errors detected in application logs.

      **Actions:**
      1. Review error logs immediately
      2. Identify error patterns
      3. Check for service degradation
      4. Verify recent changes
      5. Escalate if needed

      **Runbook:** https://docs.erlmcp.dev/runbooks/critical-errors
    EOT
  }

  alert_strategy {
    notification_rate_limit {
      period = "600s"
    }
    auto_close = "3600s"
  }

  severity = "CRITICAL"
}

# SLO Burn Rate Alert
resource "google_monitoring_alert_policy" "slo_burn_rate" {
  project      = var.project_id
  display_name = "SLO Fast Burn Rate - ${var.environment}"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Error budget burning too fast"
    condition_threshold {
      filter = join(" AND ", [
        "metric.type=\"serviceruntime.googleapis.com/api/request_count\"",
        "resource.type=\"consumed_api\"",
        "metric.label.response_code_class=\"5xx\""
      ])

      comparison      = "COMPARISON_GT"
      threshold_value = 0.1
      duration        = "300s"

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
      }

      denominator_filter = join(" AND ", [
        "metric.type=\"serviceruntime.googleapis.com/api/request_count\"",
        "resource.type=\"consumed_api\""
      ])

      denominator_aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_RATE"
        cross_series_reducer = "REDUCE_SUM"
      }
    }
  }

  notification_channels = var.notification_channels.critical

  documentation {
    mime_type = "text/markdown"
    content   = <<-EOT
      ## SLO Fast Burn Rate - erlmcp

      **Severity:** CRITICAL

      **Description:**
      Error budget is being consumed at an unsustainable rate.

      **Impact:**
      - SLO breach imminent
      - User experience degraded

      **Actions:**
      1. Identify root cause of elevated errors
      2. Consider rolling back recent changes
      3. Implement immediate mitigation
      4. Monitor burn rate closely

      **Runbook:** https://docs.erlmcp.dev/runbooks/slo-burn-rate
    EOT
  }

  alert_strategy {
    notification_rate_limit {
      period = "300s"
    }
    auto_close = "1800s"
  }

  severity = "CRITICAL"
}

# ============================================================================
# Outputs
# ============================================================================

output "gke_alert_policy_ids" {
  description = "GKE alert policy IDs"
  value = var.enable_gke_alerts ? {
    node_not_ready         = try(google_monitoring_alert_policy.gke_node_not_ready[0].id, null)
    pod_failures           = try(google_monitoring_alert_policy.gke_pod_failures[0].id, null)
    cpu_throttling         = try(google_monitoring_alert_policy.gke_cpu_throttling[0].id, null)
    memory_pressure        = try(google_monitoring_alert_policy.gke_memory_pressure[0].id, null)
    disk_pressure          = try(google_monitoring_alert_policy.gke_disk_pressure[0].id, null)
    apiserver_latency      = try(google_monitoring_alert_policy.gke_apiserver_latency[0].id, null)
    replica_mismatch       = try(google_monitoring_alert_policy.gke_deployment_replica_mismatch[0].id, null)
  } : {}
}

output "cloud_run_alert_policy_ids" {
  description = "Cloud Run alert policy IDs"
  value = var.enable_cloud_run_alerts ? {
    high_error_rate    = try(google_monitoring_alert_policy.cloudrun_high_error_rate[0].id, null)
    high_latency       = try(google_monitoring_alert_policy.cloudrun_high_latency[0].id, null)
    container_crashes  = try(google_monitoring_alert_policy.cloudrun_container_crashes[0].id, null)
    cold_starts        = try(google_monitoring_alert_policy.cloudrun_cold_starts[0].id, null)
    high_memory        = try(google_monitoring_alert_policy.cloudrun_high_memory[0].id, null)
    high_cpu           = try(google_monitoring_alert_policy.cloudrun_high_cpu[0].id, null)
  } : {}
}

output "gce_alert_policy_ids" {
  description = "GCE alert policy IDs"
  value = var.enable_gce_alerts ? {
    instance_down        = try(google_monitoring_alert_policy.gce_instance_down[0].id, null)
    high_cpu             = try(google_monitoring_alert_policy.gce_high_cpu[0].id, null)
    high_memory          = try(google_monitoring_alert_policy.gce_high_memory[0].id, null)
    high_disk            = try(google_monitoring_alert_policy.gce_high_disk[0].id, null)
    network_errors       = try(google_monitoring_alert_policy.gce_network_errors[0].id, null)
    erlang_process_count = try(google_monitoring_alert_policy.gce_erlang_process_count[0].id, null)
  } : {}
}

output "cross_platform_alert_policy_ids" {
  description = "Cross-platform alert policy IDs"
  value = {
    uptime_check_failure = google_monitoring_alert_policy.uptime_check_failure.id
    critical_log_errors  = google_monitoring_alert_policy.critical_log_errors.id
    slo_burn_rate        = google_monitoring_alert_policy.slo_burn_rate.id
  }
}

output "alert_policy_count" {
  description = "Total number of alert policies created"
  value = sum([
    var.enable_gke_alerts ? 7 : 0,
    var.enable_cloud_run_alerts ? 6 : 0,
    var.enable_gce_alerts ? 6 : 0,
    3  # Cross-platform alerts always enabled
  ])
}
