# ============================================================================
# Dashboards Module for erlmcp
# Modern Mosaic Layout with Advanced Visualizations
# Updated: 2026-02
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

# ============================================================================
# Main Operations Dashboard
# ============================================================================
resource "google_monitoring_dashboard" "main" {
  project      = var.project_id
  dashboard_json = jsonencode({
    displayName = "erlmcp Operations Dashboard - ${var.environment}"
    mosaicLayout = {
      columns = 12
      tiles = [
        # Row 1: Key Metrics Overview
        {
          width  = 3
          height = 2
          widget = {
            title = "Request Rate"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"custom.googleapis.com/erlmcp/http/requests\" resource.type=\"gce_instance\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_RATE"
                  }
                }
              }
              sparkChartView = {
                sparkChartType = "SPARK_LINE"
              }
              thresholds = [
                {
                  value     = 100
                  color     = "YELLOW"
                  direction = "ABOVE"
                },
                {
                  value     = 500
                  color     = "RED"
                  direction = "ABOVE"
                }
              ]
            }
          }
        },
        {
          width  = 3
          height = 2
          widget = {
            title = "Error Rate"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"logging.googleapis.com/user/erlmcp_error_count\" resource.type=\"gce_instance\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_RATE"
                  }
                }
              }
              sparkChartView = {
                sparkChartType = "SPARK_LINE"
              }
              thresholds = [
                {
                  value     = 1
                  color     = "YELLOW"
                  direction = "ABOVE"
                },
                {
                  value     = 10
                  color     = "RED"
                  direction = "ABOVE"
                }
              ]
            }
          }
        },
        {
          width  = 3
          height = 2
          widget = {
            title = "P95 Latency"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"logging.googleapis.com/user/erlmcp_request_latency\" resource.type=\"gce_instance\""
                  aggregation = {
                    alignmentPeriod    = "60s"
                    perSeriesAligner   = "ALIGN_DELTA"
                    crossSeriesReducer = "REDUCE_PERCENTILE_95"
                  }
                }
              }
              sparkChartView = {
                sparkChartType = "SPARK_LINE"
              }
              thresholds = [
                {
                  value     = 500
                  color     = "YELLOW"
                  direction = "ABOVE"
                },
                {
                  value     = 1000
                  color     = "RED"
                  direction = "ABOVE"
                }
              ]
            }
          }
        },
        {
          width  = 3
          height = 2
          widget = {
            title = "Active Connections"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"custom.googleapis.com/erlmcp/connections/active\" resource.type=\"gce_instance\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_MEAN"
                  }
                }
              }
              sparkChartView = {
                sparkChartType = "SPARK_LINE"
              }
            }
          }
        },
        # Row 2: Request Metrics
        {
          width  = 6
          height = 4
          widget = {
            title = "Request Rate by Status Code"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"custom.googleapis.com/erlmcp/http/requests\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod    = "60s"
                      perSeriesAligner   = "ALIGN_RATE"
                      crossSeriesReducer = "REDUCE_SUM"
                      groupByFields      = ["metric.status"]
                    }
                  }
                }
                plotType = "LINE"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Requests/second"
                scale = "LINEAR"
              }
              chartOptions = {
                mode = "COLOR"
              }
            }
          }
        },
        {
          width  = 6
          height = 4
          widget = {
            title = "Request Latency Percentiles"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"logging.googleapis.com/user/erlmcp_request_latency\" resource.type=\"gce_instance\""
                      aggregation = {
                        alignmentPeriod    = "60s"
                        perSeriesAligner   = "ALIGN_DELTA"
                        crossSeriesReducer = "REDUCE_PERCENTILE_50"
                      }
                    }
                  }
                  plotType   = "LINE"
                  targetAxis = "Y1"
                  legendTemplate = "P50"
                },
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"logging.googleapis.com/user/erlmcp_request_latency\" resource.type=\"gce_instance\""
                      aggregation = {
                        alignmentPeriod    = "60s"
                        perSeriesAligner   = "ALIGN_DELTA"
                        crossSeriesReducer = "REDUCE_PERCENTILE_95"
                      }
                    }
                  }
                  plotType   = "LINE"
                  targetAxis = "Y1"
                  legendTemplate = "P95"
                },
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"logging.googleapis.com/user/erlmcp_request_latency\" resource.type=\"gce_instance\""
                      aggregation = {
                        alignmentPeriod    = "60s"
                        perSeriesAligner   = "ALIGN_DELTA"
                        crossSeriesReducer = "REDUCE_PERCENTILE_99"
                      }
                    }
                  }
                  plotType   = "LINE"
                  targetAxis = "Y1"
                  legendTemplate = "P99"
                }
              ]
              yAxis = {
                label = "Latency (ms)"
                scale = "LINEAR"
              }
            }
          }
        },
        # Row 3: Resource Metrics
        {
          width  = 4
          height = 3
          widget = {
            title = "CPU Utilization"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"compute.googleapis.com/instance/cpu/utilization\" resource.type=\"gce_instance\" metadata.user_labels.service=\"erlmcp\""
                    aggregation = {
                      alignmentPeriod  = "60s"
                      perSeriesAligner = "ALIGN_MEAN"
                    }
                  }
                }
                plotType   = "LINE"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "CPU Utilization"
                scale = "LINEAR"
              }
              thresholds = [
                {
                  value     = 0.7
                  color     = "YELLOW"
                  direction = "ABOVE"
                },
                {
                  value     = 0.85
                  color     = "RED"
                  direction = "ABOVE"
                }
              ]
            }
          }
        },
        {
          width  = 4
          height = 3
          widget = {
            title = "Memory Usage"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"custom.googleapis.com/erlmcp/memory/usage\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod    = "60s"
                      perSeriesAligner   = "ALIGN_MEAN"
                      crossSeriesReducer = "REDUCE_SUM"
                      groupByFields      = ["metric.type"]
                    }
                  }
                }
                plotType   = "STACKED_AREA"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Memory (bytes)"
                scale = "LINEAR"
              }
            }
          }
        },
        {
          width  = 4
          height = 3
          widget = {
            title = "Erlang Process Count"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"custom.googleapis.com/erlmcp/erlang/processes\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod  = "60s"
                      perSeriesAligner = "ALIGN_MEAN"
                    }
                  }
                }
                plotType   = "LINE"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Process Count"
                scale = "LINEAR"
              }
            }
          }
        },
        # Row 4: Transport Breakdown
        {
          width  = 6
          height = 3
          widget = {
            title = "Active Connections by Transport"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"custom.googleapis.com/erlmcp/connections/active\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod    = "60s"
                      perSeriesAligner   = "ALIGN_MEAN"
                      crossSeriesReducer = "REDUCE_SUM"
                      groupByFields      = ["metric.transport"]
                    }
                  }
                }
                plotType   = "STACKED_AREA"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Connections"
                scale = "LINEAR"
              }
            }
          }
        },
        {
          width  = 6
          height = 3
          widget = {
            title = "Connection Errors by Type"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"logging.googleapis.com/user/erlmcp_connection_errors\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod    = "60s"
                      perSeriesAligner   = "ALIGN_RATE"
                      crossSeriesReducer = "REDUCE_SUM"
                      groupByFields      = ["metric.error_type", "metric.transport"]
                    }
                  }
                }
                plotType   = "STACKED_BAR"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Errors/second"
                scale = "LINEAR"
              }
            }
          }
        },
        # Row 5: Logs Summary
        {
          width  = 12
          height = 4
          widget = {
            title = "Recent Error Logs"
            logsPanel = {
              resourceNames = ["projects/${var.project_id}"]
              filter        = "severity>=ERROR labels.application=\"erlmcp\""
            }
          }
        }
      ]
    }
  })
}

# ============================================================================
# SLO and Error Budget Dashboard
# ============================================================================
resource "google_monitoring_dashboard" "slo" {
  count        = var.create_slo_dashboard ? 1 : 0
  project      = var.project_id
  dashboard_json = jsonencode({
    displayName = "erlmcp SLO & Error Budget - ${var.environment}"
    mosaicLayout = {
      columns = 12
      tiles = [
        # Row 1: SLO Overview
        {
          width  = 6
          height = 3
          widget = {
            title = "Availability SLO Status"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "select_slo_compliance(\"${var.service_name}/availability-slo\")"
                  aggregation = {
                    alignmentPeriod  = "3600s"
                    perSeriesAligner = "ALIGN_MEAN"
                  }
                }
              }
              gaugeView = {
                lowerBound = 0.0
                upperBound = 1.0
              }
              thresholds = [
                {
                  value     = 0.995
                  color     = "GREEN"
                  direction = "ABOVE"
                },
                {
                  value     = 0.99
                  color     = "YELLOW"
                  direction = "ABOVE"
                },
                {
                  value     = 0.0
                  color     = "RED"
                  direction = "ABOVE"
                }
              ]
            }
          }
        },
        {
          width  = 6
          height = 3
          widget = {
            title = "Latency SLO Status"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "select_slo_compliance(\"${var.service_name}/latency-slo\")"
                  aggregation = {
                    alignmentPeriod  = "3600s"
                    perSeriesAligner = "ALIGN_MEAN"
                  }
                }
              }
              gaugeView = {
                lowerBound = 0.0
                upperBound = 1.0
              }
              thresholds = [
                {
                  value     = 0.95
                  color     = "GREEN"
                  direction = "ABOVE"
                },
                {
                  value     = 0.90
                  color     = "YELLOW"
                  direction = "ABOVE"
                },
                {
                  value     = 0.0
                  color     = "RED"
                  direction = "ABOVE"
                }
              ]
            }
          }
        },
        # Row 2: Error Budget Status
        {
          width  = 6
          height = 3
          widget = {
            title = "Availability Error Budget Remaining"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "select_slo_budget_fraction(\"${var.service_name}/availability-slo\")"
                    aggregation = {
                      alignmentPeriod  = "3600s"
                      perSeriesAligner = "ALIGN_MEAN"
                    }
                  }
                }
                plotType   = "LINE"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Error Budget Remaining"
                scale = "LINEAR"
              }
              thresholds = [
                {
                  value     = 0.25
                  color     = "YELLOW"
                  direction = "BELOW"
                },
                {
                  value     = 0.1
                  color     = "RED"
                  direction = "BELOW"
                }
              ]
            }
          }
        },
        {
          width  = 6
          height = 3
          widget = {
            title = "Latency Error Budget Remaining"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "select_slo_budget_fraction(\"${var.service_name}/latency-slo\")"
                    aggregation = {
                      alignmentPeriod  = "3600s"
                      perSeriesAligner = "ALIGN_MEAN"
                    }
                  }
                }
                plotType   = "LINE"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Error Budget Remaining"
                scale = "LINEAR"
              }
              thresholds = [
                {
                  value     = 0.25
                  color     = "YELLOW"
                  direction = "BELOW"
                },
                {
                  value     = 0.1
                  color     = "RED"
                  direction = "BELOW"
                }
              ]
            }
          }
        },
        # Row 3: Burn Rate Analysis
        {
          width  = 12
          height = 4
          widget = {
            title = "SLO Burn Rate (Multi-Window)"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "select_slo_burn_rate(\"${var.service_name}/availability-slo\", 3600)"
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_NEXT_OLDER"
                      }
                    }
                  }
                  plotType       = "LINE"
                  targetAxis     = "Y1"
                  legendTemplate = "1 hour burn rate"
                },
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "select_slo_burn_rate(\"${var.service_name}/availability-slo\", 21600)"
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_NEXT_OLDER"
                      }
                    }
                  }
                  plotType       = "LINE"
                  targetAxis     = "Y1"
                  legendTemplate = "6 hour burn rate"
                }
              ]
              yAxis = {
                label = "Burn Rate"
                scale = "LINEAR"
              }
              thresholds = [
                {
                  value     = 14.4
                  color     = "RED"
                  direction = "ABOVE"
                  label     = "Critical burn rate"
                },
                {
                  value     = 6.0
                  color     = "YELLOW"
                  direction = "ABOVE"
                  label     = "Warning burn rate"
                },
                {
                  value     = 1.0
                  color     = "GREEN"
                  direction = "BELOW"
                  label     = "Sustainable"
                }
              ]
            }
          }
        },
        # Row 4: Good vs Total Requests
        {
          width  = 12
          height = 4
          widget = {
            title = "Good vs Total Requests (SLI)"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/erlmcp/http/requests\" resource.type=\"gce_instance\""
                      aggregation = {
                        alignmentPeriod    = "60s"
                        perSeriesAligner   = "ALIGN_RATE"
                        crossSeriesReducer = "REDUCE_SUM"
                      }
                    }
                  }
                  plotType       = "LINE"
                  targetAxis     = "Y1"
                  legendTemplate = "Total Requests"
                },
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/erlmcp/http/requests\" resource.type=\"gce_instance\" metric.label.status!=\"5xx\""
                      aggregation = {
                        alignmentPeriod    = "60s"
                        perSeriesAligner   = "ALIGN_RATE"
                        crossSeriesReducer = "REDUCE_SUM"
                      }
                    }
                  }
                  plotType       = "LINE"
                  targetAxis     = "Y1"
                  legendTemplate = "Good Requests"
                }
              ]
              yAxis = {
                label = "Requests/second"
                scale = "LINEAR"
              }
            }
          }
        }
      ]
    }
  })
}

# ============================================================================
# Erlang VM Dashboard
# ============================================================================
resource "google_monitoring_dashboard" "erlang" {
  count        = var.create_erlang_dashboard ? 1 : 0
  project      = var.project_id
  dashboard_json = jsonencode({
    displayName = "erlmcp Erlang VM Internals - ${var.environment}"
    mosaicLayout = {
      columns = 12
      tiles = [
        # Row 1: VM Overview
        {
          width  = 3
          height = 2
          widget = {
            title = "Process Count"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"custom.googleapis.com/erlmcp/erlang/processes\" resource.type=\"gce_instance\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_MEAN"
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
          width  = 3
          height = 2
          widget = {
            title = "Total Memory"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"custom.googleapis.com/erlmcp/memory/usage\" resource.type=\"gce_instance\" metric.label.type=\"total\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_MEAN"
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
          width  = 3
          height = 2
          widget = {
            title = "Supervisor Restarts"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"logging.googleapis.com/user/erlmcp_supervisor_restarts\" resource.type=\"gce_instance\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_RATE"
                  }
                }
              }
              sparkChartView = {
                sparkChartType = "SPARK_BAR"
              }
            }
          }
        },
        {
          width  = 3
          height = 2
          widget = {
            title = "Active Connections"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"custom.googleapis.com/erlmcp/connections/active\" resource.type=\"gce_instance\""
                  aggregation = {
                    alignmentPeriod    = "60s"
                    perSeriesAligner   = "ALIGN_MEAN"
                    crossSeriesReducer = "REDUCE_SUM"
                  }
                }
              }
              sparkChartView = {
                sparkChartType = "SPARK_LINE"
              }
            }
          }
        },
        # Row 2: Memory Breakdown
        {
          width  = 6
          height = 4
          widget = {
            title = "Memory Breakdown by Type"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"custom.googleapis.com/erlmcp/memory/usage\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod    = "60s"
                      perSeriesAligner   = "ALIGN_MEAN"
                      crossSeriesReducer = "REDUCE_SUM"
                      groupByFields      = ["metric.type"]
                    }
                  }
                }
                plotType   = "STACKED_AREA"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Memory (bytes)"
                scale = "LINEAR"
              }
            }
          }
        },
        {
          width  = 6
          height = 4
          widget = {
            title = "Process Count Trend"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"custom.googleapis.com/erlmcp/erlang/processes\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod  = "60s"
                      perSeriesAligner = "ALIGN_MEAN"
                    }
                  }
                }
                plotType   = "LINE"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Process Count"
                scale = "LINEAR"
              }
            }
          }
        },
        # Row 3: Supervisor Analysis
        {
          width  = 12
          height = 4
          widget = {
            title = "Supervisor Restarts by Supervisor and Reason"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"logging.googleapis.com/user/erlmcp_supervisor_restarts\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod    = "60s"
                      perSeriesAligner   = "ALIGN_RATE"
                      crossSeriesReducer = "REDUCE_SUM"
                      groupByFields      = ["metric.supervisor", "metric.reason"]
                    }
                  }
                }
                plotType   = "STACKED_BAR"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Restarts/second"
                scale = "LINEAR"
              }
            }
          }
        },
        # Row 4: Connection Details
        {
          width  = 6
          height = 4
          widget = {
            title = "Connections by Transport Type"
            pieChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"custom.googleapis.com/erlmcp/connections/active\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod    = "60s"
                      perSeriesAligner   = "ALIGN_MEAN"
                      crossSeriesReducer = "REDUCE_SUM"
                      groupByFields      = ["metric.transport"]
                    }
                  }
                }
              }]
              chartType = "PIE"
            }
          }
        },
        {
          width  = 6
          height = 4
          widget = {
            title = "Connection Error Rate by Transport"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"logging.googleapis.com/user/erlmcp_connection_errors\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod    = "60s"
                      perSeriesAligner   = "ALIGN_RATE"
                      crossSeriesReducer = "REDUCE_SUM"
                      groupByFields      = ["metric.transport"]
                    }
                  }
                }
                plotType   = "LINE"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Errors/second"
                scale = "LINEAR"
              }
            }
          }
        }
      ]
    }
  })
}

# ============================================================================
# Performance Dashboard
# ============================================================================
resource "google_monitoring_dashboard" "performance" {
  count        = var.create_performance_dashboard ? 1 : 0
  project      = var.project_id
  dashboard_json = jsonencode({
    displayName = "erlmcp Performance Analysis - ${var.environment}"
    mosaicLayout = {
      columns = 12
      tiles = [
        # Row 1: Latency Distribution
        {
          width  = 12
          height = 4
          widget = {
            title = "Request Latency Distribution (Heatmap)"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"logging.googleapis.com/user/erlmcp_request_latency\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod  = "60s"
                      perSeriesAligner = "ALIGN_DELTA"
                    }
                  }
                }
                plotType   = "HEATMAP"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Latency (ms)"
                scale = "LINEAR"
              }
            }
          }
        },
        # Row 2: Endpoint Performance
        {
          width  = 12
          height = 4
          widget = {
            title = "Latency by Endpoint (P95)"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"logging.googleapis.com/user/erlmcp_request_latency\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod    = "60s"
                      perSeriesAligner   = "ALIGN_DELTA"
                      crossSeriesReducer = "REDUCE_PERCENTILE_95"
                      groupByFields      = ["metric.endpoint"]
                    }
                  }
                }
                plotType   = "LINE"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "P95 Latency (ms)"
                scale = "LINEAR"
              }
            }
          }
        },
        # Row 3: Throughput Analysis
        {
          width  = 6
          height = 3
          widget = {
            title = "Requests by Method"
            pieChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"custom.googleapis.com/erlmcp/http/requests\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod    = "300s"
                      perSeriesAligner   = "ALIGN_RATE"
                      crossSeriesReducer = "REDUCE_SUM"
                      groupByFields      = ["metric.method"]
                    }
                  }
                }
              }]
              chartType = "DONUT"
            }
          }
        },
        {
          width  = 6
          height = 3
          widget = {
            title = "Top Endpoints by Request Volume"
            collapsibleGroup = {
              collapsed = false
            }
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"custom.googleapis.com/erlmcp/http/requests\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod    = "60s"
                      perSeriesAligner   = "ALIGN_RATE"
                      crossSeriesReducer = "REDUCE_SUM"
                      groupByFields      = ["metric.endpoint"]
                    }
                  }
                }
                plotType   = "STACKED_BAR"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Requests/second"
                scale = "LINEAR"
              }
            }
          }
        }
      ]
    }
  })
}

# ============================================================================
# Security Dashboard
# ============================================================================
resource "google_monitoring_dashboard" "security" {
  count        = var.create_security_dashboard ? 1 : 0
  project      = var.project_id
  dashboard_json = jsonencode({
    displayName = "erlmcp Security & Audit - ${var.environment}"
    mosaicLayout = {
      columns = 12
      tiles = [
        # Row 1: Security Metrics
        {
          width  = 4
          height = 2
          widget = {
            title = "4xx Error Rate"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"custom.googleapis.com/erlmcp/http/requests\" resource.type=\"gce_instance\" metric.label.status=\"4xx\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_RATE"
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
          width  = 4
          height = 2
          widget = {
            title = "Authentication Errors"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"logging.googleapis.com/user/erlmcp_error_count\" resource.type=\"gce_instance\" metric.label.service_name=\"auth\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_RATE"
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
          width  = 4
          height = 2
          widget = {
            title = "Connection Errors"
            scorecard = {
              timeSeriesQuery = {
                timeSeriesFilter = {
                  filter = "metric.type=\"logging.googleapis.com/user/erlmcp_connection_errors\" resource.type=\"gce_instance\""
                  aggregation = {
                    alignmentPeriod  = "60s"
                    perSeriesAligner = "ALIGN_RATE"
                  }
                }
              }
              sparkChartView = {
                sparkChartType = "SPARK_BAR"
              }
            }
          }
        },
        # Row 2: Error Logs
        {
          width  = 12
          height = 6
          widget = {
            title = "Security-Related Errors and Warnings"
            logsPanel = {
              resourceNames = ["projects/${var.project_id}"]
              filter        = "severity>=WARNING labels.application=\"erlmcp\" (jsonPayload.event=\"auth_failure\" OR jsonPayload.event=\"connection_error\" OR jsonPayload.event=\"unauthorized_access\")"
            }
          }
        },
        # Row 3: Error Distribution
        {
          width  = 12
          height = 4
          widget = {
            title = "HTTP Status Code Distribution"
            xyChart = {
              dataSets = [{
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "metric.type=\"custom.googleapis.com/erlmcp/http/requests\" resource.type=\"gce_instance\""
                    aggregation = {
                      alignmentPeriod    = "60s"
                      perSeriesAligner   = "ALIGN_RATE"
                      crossSeriesReducer = "REDUCE_SUM"
                      groupByFields      = ["metric.status"]
                    }
                  }
                }
                plotType   = "STACKED_AREA"
                targetAxis = "Y1"
              }]
              yAxis = {
                label = "Requests/second"
                scale = "LINEAR"
              }
            }
          }
        }
      ]
    }
  })
}
