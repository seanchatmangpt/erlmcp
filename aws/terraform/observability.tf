# ============================================================================
# Observability Stack for erlmcp
# ============================================================================
# Comprehensive monitoring, logging, and tracing infrastructure:
#   - CloudWatch Logs & Metrics
#   - CloudWatch Alarms
#   - CloudWatch Dashboards
#   - X-Ray Tracing
#   - Container Insights
#
# DOCKER-ONLY CONSTITUTION: Observability enables evidence-based operations.
# ============================================================================

# ============================================================================
# CloudWatch Log Groups
# ============================================================================

resource "aws_cloudwatch_log_group" "app" {
  name              = "/erlmcp/${var.environment}/app"
  retention_in_days = var.log_retention_days
  kms_key_id        = aws_kms_key.secrets.arn

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-app-logs"
  })
}

resource "aws_cloudwatch_log_group" "ecs" {
  count             = local.deploy_ecs ? 1 : 0
  name              = "/ecs/${local.name_prefix}"
  retention_in_days = var.log_retention_days
  kms_key_id        = aws_kms_key.secrets.arn

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-ecs-logs"
  })
}

resource "aws_cloudwatch_log_group" "ec2" {
  count             = local.deploy_ec2 ? 1 : 0
  name              = "/ec2/${local.name_prefix}"
  retention_in_days = var.log_retention_days
  kms_key_id        = aws_kms_key.secrets.arn

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-ec2-logs"
  })
}

# ============================================================================
# CloudWatch Metric Filters
# ============================================================================

resource "aws_cloudwatch_log_metric_filter" "error_count" {
  name           = "${local.name_prefix}-error-count"
  pattern        = "[timestamp, level=\"error\", ...]"
  log_group_name = aws_cloudwatch_log_group.app.name

  metric_transformation {
    name          = "ErrorCount"
    namespace     = "erlmcp/${var.environment}"
    value         = "1"
    default_value = "0"
  }
}

resource "aws_cloudwatch_log_metric_filter" "request_latency" {
  name           = "${local.name_prefix}-request-latency"
  pattern        = "[timestamp, level, pid, mfa, msg, ..., latency_ms]"
  log_group_name = aws_cloudwatch_log_group.app.name

  metric_transformation {
    name      = "RequestLatency"
    namespace = "erlmcp/${var.environment}"
    value     = "$latency_ms"
  }
}

resource "aws_cloudwatch_log_metric_filter" "session_count" {
  name           = "${local.name_prefix}-session-count"
  pattern        = "[timestamp, level, pid, mfa=\"*session*\", msg=\"*started*\", ...]"
  log_group_name = aws_cloudwatch_log_group.app.name

  metric_transformation {
    name          = "SessionStarted"
    namespace     = "erlmcp/${var.environment}"
    value         = "1"
    default_value = "0"
  }
}

# ============================================================================
# CloudWatch Alarms
# ============================================================================

resource "aws_cloudwatch_metric_alarm" "high_error_rate" {
  alarm_name          = "${local.name_prefix}-high-error-rate"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  metric_name         = "ErrorCount"
  namespace           = "erlmcp/${var.environment}"
  period              = 300
  statistic           = "Sum"
  threshold           = var.error_rate_threshold
  alarm_description   = "High error rate in erlmcp application"

  alarm_actions = var.alarm_sns_topic_arn != "" ? [var.alarm_sns_topic_arn] : []
  ok_actions    = var.alarm_sns_topic_arn != "" ? [var.alarm_sns_topic_arn] : []

  tags = local.common_tags
}

resource "aws_cloudwatch_metric_alarm" "high_cpu" {
  count               = local.deploy_ecs ? 1 : 0
  alarm_name          = "${local.name_prefix}-high-cpu"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  metric_name         = "CPUUtilization"
  namespace           = "AWS/ECS"
  period              = 300
  statistic           = "Average"
  threshold           = 80
  alarm_description   = "High CPU utilization in ECS service"

  dimensions = {
    ClusterName = aws_ecs_cluster.main[0].name
    ServiceName = aws_ecs_service.app[0].name
  }

  alarm_actions = var.alarm_sns_topic_arn != "" ? [var.alarm_sns_topic_arn] : []

  tags = local.common_tags
}

resource "aws_cloudwatch_metric_alarm" "high_memory" {
  count               = local.deploy_ecs ? 1 : 0
  alarm_name          = "${local.name_prefix}-high-memory"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  metric_name         = "MemoryUtilization"
  namespace           = "AWS/ECS"
  period              = 300
  statistic           = "Average"
  threshold           = 80
  alarm_description   = "High memory utilization in ECS service"

  dimensions = {
    ClusterName = aws_ecs_cluster.main[0].name
    ServiceName = aws_ecs_service.app[0].name
  }

  alarm_actions = var.alarm_sns_topic_arn != "" ? [var.alarm_sns_topic_arn] : []

  tags = local.common_tags
}

resource "aws_cloudwatch_metric_alarm" "unhealthy_hosts" {
  alarm_name          = "${local.name_prefix}-unhealthy-hosts"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  metric_name         = "UnHealthyHostCount"
  namespace           = "AWS/ApplicationELB"
  period              = 60
  statistic           = "Average"
  threshold           = 0
  alarm_description   = "Unhealthy hosts detected in target group"

  dimensions = {
    LoadBalancer = aws_lb.main.arn_suffix
    TargetGroup  = aws_lb_target_group.app.arn_suffix
  }

  alarm_actions = var.alarm_sns_topic_arn != "" ? [var.alarm_sns_topic_arn] : []

  tags = local.common_tags
}

resource "aws_cloudwatch_metric_alarm" "target_response_time" {
  alarm_name          = "${local.name_prefix}-high-latency"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 3
  metric_name         = "TargetResponseTime"
  namespace           = "AWS/ApplicationELB"
  period              = 60
  statistic           = "Average"
  threshold           = var.latency_threshold_seconds
  alarm_description   = "High response latency detected"

  dimensions = {
    LoadBalancer = aws_lb.main.arn_suffix
    TargetGroup  = aws_lb_target_group.app.arn_suffix
  }

  alarm_actions = var.alarm_sns_topic_arn != "" ? [var.alarm_sns_topic_arn] : []

  tags = local.common_tags
}

# ============================================================================
# CloudWatch Dashboard
# ============================================================================

resource "aws_cloudwatch_dashboard" "main" {
  dashboard_name = "${local.name_prefix}-dashboard"

  dashboard_body = jsonencode({
    widgets = [
      # Row 1: Overview
      {
        type   = "text"
        x      = 0
        y      = 0
        width  = 24
        height = 1
        properties = {
          markdown = "# erlmcp ${var.environment} Dashboard\n**Environment:** ${var.environment} | **Region:** ${var.aws_region} | **Version:** ${var.erlmcp_version}"
        }
      },

      # Row 2: Key Metrics
      {
        type   = "metric"
        x      = 0
        y      = 1
        width  = 6
        height = 6
        properties = {
          title  = "Request Count"
          region = var.aws_region
          metrics = [
            ["AWS/ApplicationELB", "RequestCount", "LoadBalancer", aws_lb.main.arn_suffix]
          ]
          stat   = "Sum"
          period = 60
        }
      },
      {
        type   = "metric"
        x      = 6
        y      = 1
        width  = 6
        height = 6
        properties = {
          title  = "Response Time (p99)"
          region = var.aws_region
          metrics = [
            ["AWS/ApplicationELB", "TargetResponseTime", "LoadBalancer", aws_lb.main.arn_suffix]
          ]
          stat   = "p99"
          period = 60
        }
      },
      {
        type   = "metric"
        x      = 12
        y      = 1
        width  = 6
        height = 6
        properties = {
          title  = "Error Rate"
          region = var.aws_region
          metrics = [
            ["erlmcp/${var.environment}", "ErrorCount"]
          ]
          stat   = "Sum"
          period = 300
        }
      },
      {
        type   = "metric"
        x      = 18
        y      = 1
        width  = 6
        height = 6
        properties = {
          title  = "Active Connections"
          region = var.aws_region
          metrics = [
            ["AWS/ApplicationELB", "ActiveConnectionCount", "LoadBalancer", aws_lb.main.arn_suffix]
          ]
          stat   = "Average"
          period = 60
        }
      },

      # Row 3: ECS Metrics (if deployed)
      {
        type   = "metric"
        x      = 0
        y      = 7
        width  = 8
        height = 6
        properties = {
          title  = "ECS CPU Utilization"
          region = var.aws_region
          metrics = local.deploy_ecs ? [
            ["AWS/ECS", "CPUUtilization", "ClusterName", aws_ecs_cluster.main[0].name, "ServiceName", aws_ecs_service.app[0].name]
          ] : []
          stat   = "Average"
          period = 60
        }
      },
      {
        type   = "metric"
        x      = 8
        y      = 7
        width  = 8
        height = 6
        properties = {
          title  = "ECS Memory Utilization"
          region = var.aws_region
          metrics = local.deploy_ecs ? [
            ["AWS/ECS", "MemoryUtilization", "ClusterName", aws_ecs_cluster.main[0].name, "ServiceName", aws_ecs_service.app[0].name]
          ] : []
          stat   = "Average"
          period = 60
        }
      },
      {
        type   = "metric"
        x      = 16
        y      = 7
        width  = 8
        height = 6
        properties = {
          title  = "Running Task Count"
          region = var.aws_region
          metrics = local.deploy_ecs ? [
            ["ECS/ContainerInsights", "RunningTaskCount", "ClusterName", aws_ecs_cluster.main[0].name, "ServiceName", aws_ecs_service.app[0].name]
          ] : []
          stat   = "Average"
          period = 60
        }
      },

      # Row 4: ALB Health
      {
        type   = "metric"
        x      = 0
        y      = 13
        width  = 12
        height = 6
        properties = {
          title  = "Target Health"
          region = var.aws_region
          metrics = [
            ["AWS/ApplicationELB", "HealthyHostCount", "LoadBalancer", aws_lb.main.arn_suffix, "TargetGroup", aws_lb_target_group.app.arn_suffix],
            ["AWS/ApplicationELB", "UnHealthyHostCount", "LoadBalancer", aws_lb.main.arn_suffix, "TargetGroup", aws_lb_target_group.app.arn_suffix]
          ]
          stat   = "Average"
          period = 60
        }
      },
      {
        type   = "metric"
        x      = 12
        y      = 13
        width  = 12
        height = 6
        properties = {
          title  = "HTTP Response Codes"
          region = var.aws_region
          metrics = [
            ["AWS/ApplicationELB", "HTTPCode_Target_2XX_Count", "LoadBalancer", aws_lb.main.arn_suffix],
            ["AWS/ApplicationELB", "HTTPCode_Target_4XX_Count", "LoadBalancer", aws_lb.main.arn_suffix],
            ["AWS/ApplicationELB", "HTTPCode_Target_5XX_Count", "LoadBalancer", aws_lb.main.arn_suffix]
          ]
          stat   = "Sum"
          period = 60
        }
      },

      # Row 5: Logs Widget
      {
        type   = "log"
        x      = 0
        y      = 19
        width  = 24
        height = 6
        properties = {
          title  = "Recent Application Logs"
          region = var.aws_region
          query  = "SOURCE '${aws_cloudwatch_log_group.app.name}' | fields @timestamp, @message | sort @timestamp desc | limit 50"
        }
      }
    ]
  })
}

# ============================================================================
# X-Ray Configuration
# ============================================================================

resource "aws_xray_sampling_rule" "main" {
  count         = var.enable_xray ? 1 : 0
  rule_name     = "${local.name_prefix}-sampling"
  priority      = 1000
  version       = 1
  reservoir_size = 5
  fixed_rate    = 0.05
  url_path      = "*"
  host          = "*"
  http_method   = "*"
  service_type  = "*"
  service_name  = "erlmcp"
  resource_arn  = "*"

  attributes = {}
}

# ============================================================================
# Container Insights (for ECS)
# ============================================================================

resource "aws_ecs_cluster" "container_insights" {
  count = local.deploy_ecs ? 1 : 0
  name  = aws_ecs_cluster.main[0].name

  setting {
    name  = "containerInsights"
    value = var.enable_container_insights ? "enabled" : "disabled"
  }
}

# ============================================================================
# CloudWatch Agent Configuration (for EC2)
# ============================================================================

resource "local_file" "cloudwatch_agent_config" {
  count    = local.deploy_ec2 ? 1 : 0
  filename = "${path.module}/files/amazon-cloudwatch-agent.json"
  content  = jsonencode({
    agent = {
      metrics_collection_interval = 60
      run_as_user                 = "root"
    }
    logs = {
      logs_collected = {
        files = {
          collect_list = [
            {
              file_path        = "/var/log/erlmcp/console.log"
              log_group_name   = aws_cloudwatch_log_group.ec2[0].name
              log_stream_name  = "{instance_id}/console"
              retention_in_days = var.log_retention_days
            },
            {
              file_path        = "/var/log/erlmcp/error.log"
              log_group_name   = aws_cloudwatch_log_group.ec2[0].name
              log_stream_name  = "{instance_id}/error"
              retention_in_days = var.log_retention_days
            },
            {
              file_path        = "/var/log/erlmcp/erlmcp.log"
              log_group_name   = aws_cloudwatch_log_group.ec2[0].name
              log_stream_name  = "{instance_id}/app"
              retention_in_days = var.log_retention_days
            }
          ]
        }
      }
    }
    metrics = {
      namespace = "erlmcp/${var.environment}"
      metrics_collected = {
        cpu = {
          measurement                 = ["cpu_usage_active", "cpu_usage_idle", "cpu_usage_system", "cpu_usage_user"]
          metrics_collection_interval = 60
          totalcpu                    = true
        }
        disk = {
          measurement                 = ["disk_used_percent", "disk_free", "disk_used"]
          metrics_collection_interval = 60
          resources                   = ["/"]
        }
        diskio = {
          measurement                 = ["io_time", "reads", "writes"]
          metrics_collection_interval = 60
          resources                   = ["*"]
        }
        mem = {
          measurement                 = ["mem_used_percent", "mem_available", "mem_used"]
          metrics_collection_interval = 60
        }
        net = {
          measurement                 = ["net_bytes_recv", "net_bytes_sent", "net_packets_recv", "net_packets_sent"]
          metrics_collection_interval = 60
        }
        processes = {
          measurement = ["running", "sleeping", "total"]
        }
      }
      append_dimensions = {
        InstanceId        = "$${aws:InstanceId}"
        AutoScalingGroupName = "$${aws:AutoScalingGroupName}"
      }
    }
  })
}

# ============================================================================
# Variables
# ============================================================================

variable "log_retention_days" {
  description = "CloudWatch log retention in days"
  type        = number
  default     = 30
}

variable "alarm_sns_topic_arn" {
  description = "SNS topic ARN for CloudWatch alarms"
  type        = string
  default     = ""
}

variable "error_rate_threshold" {
  description = "Error count threshold for alarm"
  type        = number
  default     = 10
}

variable "latency_threshold_seconds" {
  description = "Response latency threshold in seconds"
  type        = number
  default     = 2.0
}

variable "enable_xray" {
  description = "Enable AWS X-Ray tracing"
  type        = bool
  default     = true
}

variable "enable_container_insights" {
  description = "Enable ECS Container Insights"
  type        = bool
  default     = true
}

# ============================================================================
# Outputs
# ============================================================================

output "cloudwatch_log_group_arn" {
  description = "CloudWatch log group ARN"
  value       = aws_cloudwatch_log_group.app.arn
}

output "cloudwatch_dashboard_url" {
  description = "CloudWatch dashboard URL"
  value       = "https://${var.aws_region}.console.aws.amazon.com/cloudwatch/home?region=${var.aws_region}#dashboards:name=${aws_cloudwatch_dashboard.main.dashboard_name}"
}
