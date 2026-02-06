# ============================================================================
# Complete Example: GCP Observability Module for erlmcp
# ============================================================================

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 6.0.0"
    }
  }
}

# Configure the Google Cloud Provider
provider "google" {
  project = var.project_id
  region  = var.region
}

# ============================================================================
# Example 1: Production Environment with Full Observability
# ============================================================================
module "observability_production" {
  source = "../"  # Path to observability module

  project_id   = var.project_id
  environment  = "production"
  service_name = "erlmcp-prod"
  team_label   = "platform"

  # Notification Channels
  notification_channels = {
    email = {
      enabled = true
      address = "ops-team@example.com"
    }
    pagerduty = {
      enabled     = true
      service_key = var.pagerduty_service_key
      auth_token  = var.pagerduty_auth_token
    }
    slack = {
      enabled      = true
      channel_name = "#prod-alerts"
      auth_token   = var.slack_token
    }
    webhook = {
      enabled    = true
      url        = "https://webhook.example.com/alerts"
      auth_token = var.webhook_token
    }
  }

  # Aggressive SLO targets for production
  create_slos              = true
  enable_slo_alerts        = true
  slo_availability_goal    = 0.999  # 99.9%
  slo_latency_goal         = 0.95
  slo_latency_threshold_ms = 500
  slo_rolling_period_days  = 30

  # Comprehensive alerting
  enable_error_rate_alert    = true
  enable_latency_alert       = true
  enable_memory_alert        = true
  enable_cpu_alert           = true
  enable_health_check_alert  = true
  enable_process_count_alert = true

  # Strict thresholds for production
  error_rate_alert_threshold = 5      # Lower threshold for prod
  latency_alert_threshold    = 1.0
  memory_alert_threshold     = 3221225472  # 3GB
  cpu_alert_threshold        = 0.75   # Alert at 75%

  # All dashboards enabled
  create_performance_dashboard = true
  create_erlang_dashboard      = true
  create_security_dashboard    = true
  create_slo_dashboard         = true

  # Log export to BigQuery and Storage
  log_export_bigquery_enabled     = true
  log_export_bigquery_destination = "bigquery.googleapis.com/projects/${var.project_id}/datasets/erlmcp_logs_prod"
  log_export_storage_enabled      = true
  log_export_storage_destination  = "storage.googleapis.com/${var.project_id}-logs-prod"
  log_export_pubsub_enabled       = true
  log_export_pubsub_topic         = "projects/${var.project_id}/topics/erlmcp-errors"

  # Cost optimization
  exclude_health_check_logs   = true
  exclude_successful_get_logs = true

  # Multi-region uptime checks
  create_uptime_check  = true
  uptime_check_host    = "erlmcp-prod.example.com"
  uptime_check_use_ssl = true
  uptime_check_regions = ["USA", "EUROPE", "ASIA_PACIFIC"]
}

# ============================================================================
# Example 2: Staging Environment with Relaxed SLOs
# ============================================================================
module "observability_staging" {
  source = "../"

  project_id   = var.project_id
  environment  = "staging"
  service_name = "erlmcp-staging"
  team_label   = "platform"

  notification_channels = {
    email = {
      enabled = true
      address = "dev-team@example.com"
    }
    slack = {
      enabled      = true
      channel_name = "#staging-alerts"
      auth_token   = var.slack_token
    }
    pagerduty = {
      enabled     = false
      service_key = ""
      auth_token  = ""
    }
    webhook = {
      enabled    = false
      url        = ""
      auth_token = ""
    }
  }

  # Relaxed SLO targets for staging
  create_slos              = true
  enable_slo_alerts        = false  # Don't page on staging SLO violations
  slo_availability_goal    = 0.95   # 95%
  slo_latency_goal         = 0.90
  slo_latency_threshold_ms = 1000   # 1 second
  slo_rolling_period_days  = 7

  # Basic alerting for staging
  enable_error_rate_alert    = true
  enable_latency_alert       = false  # Don't alert on latency in staging
  enable_memory_alert        = true
  enable_cpu_alert           = false
  enable_health_check_alert  = true
  enable_process_count_alert = false

  # Higher thresholds for staging
  error_rate_alert_threshold = 50   # More lenient
  memory_alert_threshold     = 4294967296  # 4GB

  # Limited dashboards for staging
  create_performance_dashboard = true
  create_erlang_dashboard      = true
  create_security_dashboard    = false
  create_slo_dashboard         = false

  # No log export for staging (cost savings)
  log_export_bigquery_enabled = false
  log_export_storage_enabled  = false
  log_export_pubsub_enabled   = false

  # Aggressive log exclusions for cost
  exclude_health_check_logs   = true
  exclude_successful_get_logs = true

  # Single region uptime check
  create_uptime_check  = true
  uptime_check_host    = "erlmcp-staging.example.com"
  uptime_check_use_ssl = false
  uptime_check_regions = ["USA"]
}

# ============================================================================
# Example 3: Development Environment (Minimal)
# ============================================================================
module "observability_development" {
  source = "../"

  project_id   = var.project_id
  environment  = "development"
  service_name = "erlmcp-dev"
  team_label   = "engineering"

  # Email only for development
  notification_channels = {
    email = {
      enabled = true
      address = "dev-team@example.com"
    }
    slack = {
      enabled      = false
      channel_name = ""
      auth_token   = ""
    }
    pagerduty = {
      enabled     = false
      service_key = ""
      auth_token  = ""
    }
    webhook = {
      enabled    = false
      url        = ""
      auth_token = ""
    }
  }

  # No SLOs for development
  create_slos       = false
  enable_slo_alerts = false

  # Minimal alerting
  enable_error_rate_alert    = false
  enable_latency_alert       = false
  enable_memory_alert        = true  # Only alert on critical memory issues
  enable_cpu_alert           = false
  enable_health_check_alert  = false
  enable_process_count_alert = false

  # High threshold - only alert on critical issues
  memory_alert_threshold = 6442450944  # 6GB

  # Single dashboard for development
  create_performance_dashboard = false
  create_erlang_dashboard      = true  # For debugging
  create_security_dashboard    = false
  create_slo_dashboard         = false

  # No log export
  log_export_bigquery_enabled = false
  log_export_storage_enabled  = false
  log_export_pubsub_enabled   = false

  # Exclude everything possible
  exclude_health_check_logs   = true
  exclude_successful_get_logs = true

  # No uptime checks
  create_uptime_check = false
}

# ============================================================================
# Outputs
# ============================================================================

output "production_dashboards" {
  description = "Production dashboard URLs"
  value       = module.observability_production.dashboard_urls
}

output "production_slo_ids" {
  description = "Production SLO IDs"
  value = {
    availability = module.observability_production.availability_slo_id
    latency      = module.observability_production.latency_slo_id
  }
}

output "staging_dashboards" {
  description = "Staging dashboard URLs"
  value       = module.observability_staging.dashboard_urls
}

output "development_dashboards" {
  description = "Development dashboard URLs"
  value       = module.observability_development.dashboard_urls
}

output "log_sink_service_accounts" {
  description = "Service accounts for log sinks - grant these write permissions"
  value = {
    production = module.observability_production.log_sink_writer_identities
    staging    = module.observability_staging.log_sink_writer_identities
  }
  sensitive = true
}
