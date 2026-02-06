terraform {
  required_version = ">= 1.0"
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
  }
}

# Budget configuration with threshold-based alerts
resource "google_billing_budget" "budget" {
  billing_account = var.billing_account
  display_name    = var.budget_name

  budget_filter {
    projects               = var.projects
    credit_types_treatment = var.credit_types_treatment
    services               = var.services

    dynamic "labels" {
      for_each = var.budget_labels
      content {
        key    = labels.key
        values = labels.value
      }
    }
  }

  amount {
    specified_amount {
      currency_code = var.currency_code
      units         = var.budget_amount
    }
  }

  dynamic "threshold_rules" {
    for_each = var.threshold_rules
    content {
      threshold_percent = threshold_rules.value.threshold_percent
      spend_basis       = threshold_rules.value.spend_basis
    }
  }

  all_updates_rule {
    monitoring_notification_channels = [
      for channel in google_monitoring_notification_channel.budget_alert : channel.id
    ]
    disable_default_iam_recipients   = var.disable_default_iam_recipients
    pubsub_topic                     = var.pubsub_topic_id != "" ? var.pubsub_topic_id : null
    schema_version                   = "1.0"
  }
}

# Notification channels for budget alerts
resource "google_monitoring_notification_channel" "budget_alert" {
  for_each     = var.notification_channels
  display_name = each.value.display_name
  type         = each.value.type
  labels       = each.value.labels

  user_labels = merge(
    var.cost_allocation_labels,
    {
      managed_by = "terraform"
      module     = "billing"
    }
  )

  enabled = each.value.enabled
}

# Cost allocation labels at organization level
resource "google_tags_tag_key" "cost_center" {
  count       = var.create_cost_allocation_tags ? 1 : 0
  parent      = "organizations/${var.organization_id}"
  short_name  = "cost-center"
  description = "Cost center allocation for billing attribution"

  purpose     = "GCE_FIREWALL"
  purpose_data = {
    network = var.network
  }
}

resource "google_tags_tag_value" "cost_center_values" {
  for_each    = var.create_cost_allocation_tags ? var.cost_centers : {}
  parent      = google_tags_tag_key.cost_center[0].id
  short_name  = each.key
  description = each.value
}

resource "google_tags_tag_key" "environment" {
  count       = var.create_cost_allocation_tags ? 1 : 0
  parent      = "organizations/${var.organization_id}"
  short_name  = "environment"
  description = "Environment tier for cost tracking"
}

resource "google_tags_tag_value" "environment_values" {
  for_each    = var.create_cost_allocation_tags ? toset(["production", "staging", "development", "test"]) : []
  parent      = google_tags_tag_key.environment[0].id
  short_name  = each.key
  description = "Environment: ${each.key}"
}

resource "google_tags_tag_key" "workload" {
  count       = var.create_cost_allocation_tags ? 1 : 0
  parent      = "organizations/${var.organization_id}"
  short_name  = "workload"
  description = "Workload type for cost attribution"
}

resource "google_tags_tag_value" "workload_values" {
  for_each    = var.create_cost_allocation_tags ? var.workloads : {}
  parent      = google_tags_tag_key.workload[0].id
  short_name  = each.key
  description = each.value
}

# Project-level budget alerts with granular controls
resource "google_billing_budget" "project_budgets" {
  for_each        = var.project_budgets
  billing_account = var.billing_account
  display_name    = "${each.key}-budget"

  budget_filter {
    projects               = [each.value.project_id]
    credit_types_treatment = "EXCLUDE_ALL_CREDITS"
  }

  amount {
    specified_amount {
      currency_code = var.currency_code
      units         = each.value.amount
    }
  }

  threshold_rules {
    threshold_percent = 0.5
    spend_basis       = "CURRENT_SPEND"
  }

  threshold_rules {
    threshold_percent = 0.75
    spend_basis       = "CURRENT_SPEND"
  }

  threshold_rules {
    threshold_percent = 0.9
    spend_basis       = "CURRENT_SPEND"
  }

  threshold_rules {
    threshold_percent = 1.0
    spend_basis       = "CURRENT_SPEND"
  }

  threshold_rules {
    threshold_percent = 1.2
    spend_basis       = "CURRENT_SPEND"
  }

  all_updates_rule {
    monitoring_notification_channels = [
      for channel in google_monitoring_notification_channel.budget_alert : channel.id
    ]
    pubsub_topic   = var.pubsub_topic_id != "" ? var.pubsub_topic_id : null
    schema_version = "1.0"
  }
}

# Service quota overrides for controlled spending
resource "google_service_usage_consumer_quota_override" "compute_quotas" {
  for_each       = var.compute_quotas
  project        = each.value.project_id
  service        = "compute.googleapis.com"
  metric         = each.value.metric
  limit          = each.value.limit
  override_value = each.value.override_value
  force          = each.value.force

  dimensions = each.value.dimensions
}

resource "google_service_usage_consumer_quota_override" "storage_quotas" {
  for_each       = var.storage_quotas
  project        = each.value.project_id
  service        = "storage.googleapis.com"
  metric         = each.value.metric
  limit          = each.value.limit
  override_value = each.value.override_value
  force          = each.value.force

  dimensions = each.value.dimensions
}

resource "google_service_usage_consumer_quota_override" "networking_quotas" {
  for_each       = var.networking_quotas
  project        = each.value.project_id
  service        = "compute.googleapis.com"
  metric         = each.value.metric
  limit          = each.value.limit
  override_value = each.value.override_value
  force          = each.value.force

  dimensions = each.value.dimensions
}

# Commitment-based discounts tracking
resource "google_billing_budget" "commitment_tracker" {
  count           = var.track_commitments ? 1 : 0
  billing_account = var.billing_account
  display_name    = "commitment-utilization-tracker"

  budget_filter {
    credit_types_treatment = "INCLUDE_SPECIFIED_CREDITS"
    credit_types = [
      "COMMITTED_USAGE_DISCOUNT",
      "SUSTAINED_USAGE_DISCOUNT"
    ]
  }

  amount {
    specified_amount {
      currency_code = var.currency_code
      units         = var.commitment_budget_amount
    }
  }

  threshold_rules {
    threshold_percent = 0.8
    spend_basis       = "CURRENT_SPEND"
  }

  all_updates_rule {
    monitoring_notification_channels = [
      for channel in google_monitoring_notification_channel.budget_alert : channel.id
    ]
    schema_version = "1.0"
  }
}

# Anomaly detection budget for unusual spend patterns
resource "google_billing_budget" "anomaly_detection" {
  count           = var.enable_anomaly_detection ? 1 : 0
  billing_account = var.billing_account
  display_name    = "anomaly-detection-budget"

  budget_filter {
    projects               = var.projects
    credit_types_treatment = "EXCLUDE_ALL_CREDITS"
  }

  amount {
    last_period_amount = true
  }

  threshold_rules {
    threshold_percent = 1.1
    spend_basis       = "CURRENT_SPEND"
  }

  threshold_rules {
    threshold_percent = 1.25
    spend_basis       = "CURRENT_SPEND"
  }

  threshold_rules {
    threshold_percent = 1.5
    spend_basis       = "CURRENT_SPEND"
  }

  all_updates_rule {
    monitoring_notification_channels = [
      for channel in google_monitoring_notification_channel.budget_alert : channel.id
    ]
    pubsub_topic   = var.pubsub_topic_id != "" ? var.pubsub_topic_id : null
    schema_version = "1.0"
  }
}

# Export billing data to BigQuery for analysis
resource "google_bigquery_dataset" "billing_export" {
  count                      = var.enable_billing_export ? 1 : 0
  dataset_id                 = var.billing_export_dataset_id
  project                    = var.billing_export_project_id
  location                   = var.billing_export_location
  description                = "Billing data export for cost analysis and optimization"
  default_table_expiration_ms = var.billing_export_retention_days * 24 * 60 * 60 * 1000

  labels = merge(
    var.cost_allocation_labels,
    {
      purpose    = "billing-export"
      managed_by = "terraform"
    }
  )

  access {
    role          = "OWNER"
    user_by_email = var.billing_export_owner_email
  }

  access {
    role          = "READER"
    special_group = "projectReaders"
  }
}

# Cost optimization recommendations monitoring
resource "google_monitoring_alert_policy" "cost_optimization" {
  count        = var.enable_cost_optimization_alerts ? 1 : 0
  display_name = "cost-optimization-recommendations"
  project      = var.monitoring_project_id
  combiner     = "OR"

  conditions {
    display_name = "Idle resources detected"

    condition_threshold {
      filter          = "metric.type=\"billing.googleapis.com/project/cost\" AND resource.type=\"global\""
      duration        = "3600s"
      comparison      = "COMPARISON_GT"
      threshold_value = var.idle_resource_threshold

      aggregations {
        alignment_period   = "3600s"
        per_series_aligner = "ALIGN_RATE"
      }
    }
  }

  notification_channels = [
    for channel in google_monitoring_notification_channel.budget_alert : channel.id
  ]

  alert_strategy {
    auto_close = "604800s"

    notification_rate_limit {
      period = "3600s"
    }
  }

  user_labels = var.cost_allocation_labels
}

# Multi-project cost allocation dashboard
resource "google_monitoring_dashboard" "cost_allocation" {
  count          = var.create_cost_dashboard ? 1 : 0
  dashboard_json = jsonencode({
    displayName = "Cost Allocation Dashboard"
    mosaicLayout = {
      columns = 12
      tiles = [
        {
          width  = 6
          height = 4
          widget = {
            title = "Current Month Spend by Project"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"billing.googleapis.com/project/cost\""
                    aggregation = {
                      alignmentPeriod    = "86400s"
                      perSeriesAligner   = "ALIGN_SUM"
                      crossSeriesReducer = "REDUCE_SUM"
                      groupByFields      = ["resource.project_id"]
                    }
                  }
                }
              }]
              yAxis = {
                label = "Cost (${var.currency_code})"
                scale = "LINEAR"
              }
            }
          }
        },
        {
          xPos   = 6
          width  = 6
          height = 4
          widget = {
            title = "Budget Utilization"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"billing.googleapis.com/project/cost\""
                  aggregation = {
                    alignmentPeriod  = "86400s"
                    perSeriesAligner = "ALIGN_SUM"
                  }
                }
              }
              sparkChartView = {
                sparkChartType = "SPARK_LINE"
              }
            }
          }
        },
        {
          yPos   = 4
          width  = 12
          height = 4
          widget = {
            title = "Cost by Service"
            pieChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"billing.googleapis.com/project/cost\""
                    aggregation = {
                      alignmentPeriod    = "86400s"
                      perSeriesAligner   = "ALIGN_SUM"
                      crossSeriesReducer = "REDUCE_SUM"
                      groupByFields      = ["metric.service"]
                    }
                  }
                }
              }]
            }
          }
        }
      ]
    }
  })
  project = var.monitoring_project_id
}
