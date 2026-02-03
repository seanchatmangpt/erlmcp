# Alert policies module
resource "google_monitoring_alert_policy" "erlmcp_alerts" {
  count = var.enable_alert_policies ? 1 : 0

  display_name = "erlmcp-${var.environment}-alerts"
  combiner     = "OR"
  conditions {
    display_name = "erlmcp alert condition"
    condition_threshold {
      filter          = "resource.type = \"global\" AND metric.type = \"workload.googleapis.com/erlmcp/requests\""
      comparison      = "COMPARISON_GT"
      threshold_value = 100
      duration        = "300s"
      aggregations {
        alignment_period = "60s"
        per_series_aligner = "ALIGN_RATE"
      }
    }
  }

  notification_channels = var.notification_channels
  enabled               = true
  combiner              = "OR"
}

variable "enable_alert_policies" {
  type    = bool
  default = true
}

variable "notification_channels" {
  type    = list(string)
  default = []
}

variable "environment" {
  type    = string
  default = "production"
}
