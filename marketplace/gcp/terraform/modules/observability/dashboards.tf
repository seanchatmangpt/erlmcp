# ============================================================================
# Monitoring Dashboards for erlmcp
# ============================================================================

# Main erlmcp Dashboard
resource "google_monitoring_dashboard" "main" {
  project        = var.project_id
  display_name   = "erlmcp Main Dashboard"
  dashboard_json = templatefile("${path.module}/dashboards/main.json.tpl", {
    project_id = var.project_id
  })
}

# Performance Dashboard
resource "google_monitoring_dashboard" "performance" {
  count         = var.create_performance_dashboard ? 1 : 0
  project       = var.project_id
  display_name  = "erlmcp Performance Dashboard"
  dashboard_json = templatefile("${path.module}/dashboards/performance.json.tpl", {
    project_id = var.project_id
  })
}

# Erlang VM Dashboard
resource "google_monitoring_dashboard" "erlang" {
  count         = var.create_erlang_dashboard ? 1 : 0
  project       = var.project_id
  display_name  = "erlmcp Erlang VM Dashboard"
  dashboard_json = templatefile("${path.module}/dashboards/erlang.json.tpl", {
    project_id = var.project_id
  })
}

# Security Dashboard
resource "google_monitoring_dashboard" "security" {
  count         = var.create_security_dashboard ? 1 : 0
  project       = var.project_id
  display_name  = "erlmcp Security Dashboard"
  dashboard_json = templatefile("${path.module}/dashboards/security.json.tpl", {
    project_id = var.project_id
  })
}

# ============================================================================
# Dashboard JSON Templates
# ============================================================================

# Main dashboard template
resource "local_file" "main_dashboard_template" {
  content = jsonencode({
    displayName = "erlmcp Main Dashboard"
    gridLayout = {
      columns = "2"
      widgets = [
        {
          title = "Request Rate"
          xyChart = {
            dataSets = [{
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "resource.type=\"gce_instance\" metric.type=\"compute.googleapis.com/instance/network/received_bytes_count\" resource.label.\"instance_name\"=\"erlmcp-server\""
                  aggregation = {
                    alignmentPeriod = "60s"
                    crossSeriesReducer = "REDUCE_SUM"
                    groupByFields = ["metric.label.event_type"]
                  }
                }
              }
            }]
          }
        },
        {
          title = "Error Rate"
          scorecard = {
            dataView = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "resource.type=\"gce_instance\" metric.type=\"logging.googleapis.com/user/erlmcp_error_count\""
                  aggregation = {
                    alignmentPeriod = "300s"
                    crossSeriesReducer = "REDUCE_SUM"
                  }
                }
              }
            }
          }
        },
        {
          title = "CPU Utilization"
          xyChart = {
            dataSets = [{
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "resource.type=\"gce_instance\" metric.type=\"compute.googleapis.com/instance/cpu/utilization\" resource.label.\"instance_name\"=\"erlmcp-server\""
                  aggregation = {
                    alignmentPeriod = "60s"
                  }
                }
              }
            }]
          }
        },
        {
          title = "Memory Usage"
          xyChart = {
            dataSets = [{
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "resource.type=\"gce_instance\" metric.type=\"custom.googleapis.com/erlmcp/memory/usage\" resource.label.\"instance_name\"=\"erlmcp-server\""
                  aggregation = {
                    alignmentPeriod = "60s"
                    crossSeriesReducer = "REDUCE_SUM"
                  }
                }
              }
            }]
          }
        },
        {
          title = "Active Connections"
          xyChart = {
            dataSets = [{
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "resource.type=\"gce_instance\" metric.type=\"custom.googleapis.com/erlmcp/connections/active\" resource.label.\"instance_name\"=\"erlmcp-server\""
                  aggregation = {
                    alignmentPeriod = "60s"
                  }
                }
              }
            }]
          }
        },
        {
          title = "Response Time"
          xyChart = {
            dataSets = [{
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "resource.type=\"gce_instance\" metric.type=\"custom.googleapis.com/erlmcp/http/latency\" resource.label.\"instance_name\"=\"erlmcp-server\""
                  aggregation = {
                    alignmentPeriod = "60s"
                    crossSeriesReducer = "REDUCE_MEAN"
                  }
                }
              }
            }]
          }
        }
      ]
    }
  })
  filename = "${path.module}/dashboards/main.json.tpl"
}
