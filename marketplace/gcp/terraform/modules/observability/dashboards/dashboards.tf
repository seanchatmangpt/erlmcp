# Dashboards module
resource "google_monitoring_dashboard" "erlmcp_dashboard" {
  count = var.enable_dashboards ? 1 : 0

  dashboard_json = jsonencode({
    displayName = "erlmcp-${var.environment}-dashboard"
    mosaicLayout {
    tiles {
    width = 6
    height = 4
    widget {
    title = "erlmcp Requests"
    xyChart {
    dataSets {
    timeSeriesQuery {
    timeSeriesFilter {
    filter = "resource.type = \"global\" AND metric.type = \"workload.googleapis.com/erlmcp/requests\""
    aggregation {
    alignmentPeriod = "60s"
    perSeriesAligner = "ALIGN_RATE"
    }
    }
    }
    plotType = "LINE"
    }
    }
    }
    }
  })

  lifecycle {
    ignore_changes = [dashboard_json]
  }
}

variable "enable_dashboards" {
  type    = bool
  default = true
}

variable "environment" {
  type    = string
  default = "production"
}
