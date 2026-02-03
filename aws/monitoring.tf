# ==============================================================================
# AWS CloudWatch Monitoring and Alerts
# ==============================================================================
# Comprehensive monitoring, alerting, and dashboards for erlmcp.
# BEAM-aware metrics and observability.
# ==============================================================================

# ==============================================================================
# SNS Topics for Alerts
# ==============================================================================

resource "aws_sns_topic" "alerts" {
  name              = "${local.name_prefix}-alerts"
  kms_master_key_id = aws_kms_key.main.id

  tags = {
    Name = "${local.name_prefix}-alerts"
  }
}

resource "aws_sns_topic_subscription" "email" {
  for_each = toset(var.alert_email_addresses)

  topic_arn = aws_sns_topic.alerts.arn
  protocol  = "email"
  endpoint  = each.value
}

# Slack notifications via Lambda
resource "aws_sns_topic_subscription" "slack" {
  count = var.slack_webhook_url != "" ? 1 : 0

  topic_arn = aws_sns_topic.alerts.arn
  protocol  = "lambda"
  endpoint  = aws_lambda_function.slack_notifier[0].arn
}

# ==============================================================================
# CloudWatch Alarms
# ==============================================================================

# ECS CPU utilization
resource "aws_cloudwatch_metric_alarm" "ecs_cpu_high" {
  alarm_name          = "${local.name_prefix}-ecs-cpu-high"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 3
  metric_name         = "CPUUtilization"
  namespace           = "AWS/ECS"
  period              = 60
  statistic           = "Average"
  threshold           = 80
  alarm_description   = "ECS CPU utilization is above 80%"
  alarm_actions       = [aws_sns_topic.alerts.arn]
  ok_actions          = [aws_sns_topic.alerts.arn]

  dimensions = {
    ClusterName = aws_ecs_cluster.main.name
    ServiceName = aws_ecs_service.main.name
  }

  tags = {
    Name = "${local.name_prefix}-ecs-cpu-high"
  }
}

# ECS memory utilization
resource "aws_cloudwatch_metric_alarm" "ecs_memory_high" {
  alarm_name          = "${local.name_prefix}-ecs-memory-high"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 3
  metric_name         = "MemoryUtilization"
  namespace           = "AWS/ECS"
  period              = 60
  statistic           = "Average"
  threshold           = 85
  alarm_description   = "ECS memory utilization is above 85%"
  alarm_actions       = [aws_sns_topic.alerts.arn]
  ok_actions          = [aws_sns_topic.alerts.arn]

  dimensions = {
    ClusterName = aws_ecs_cluster.main.name
    ServiceName = aws_ecs_service.main.name
  }

  tags = {
    Name = "${local.name_prefix}-ecs-memory-high"
  }
}

# ALB 5xx errors
resource "aws_cloudwatch_metric_alarm" "alb_5xx_high" {
  alarm_name          = "${local.name_prefix}-alb-5xx-high"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  metric_name         = "HTTPCode_Target_5XX_Count"
  namespace           = "AWS/ApplicationELB"
  period              = 60
  statistic           = "Sum"
  threshold           = 10
  alarm_description   = "ALB target 5xx errors exceed threshold"
  alarm_actions       = [aws_sns_topic.alerts.arn]
  ok_actions          = [aws_sns_topic.alerts.arn]
  treat_missing_data  = "notBreaching"

  dimensions = {
    LoadBalancer = aws_lb.main.arn_suffix
    TargetGroup  = aws_lb_target_group.main.arn_suffix
  }

  tags = {
    Name = "${local.name_prefix}-alb-5xx-high"
  }
}

# ALB response time
resource "aws_cloudwatch_metric_alarm" "alb_latency_high" {
  alarm_name          = "${local.name_prefix}-alb-latency-high"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 3
  metric_name         = "TargetResponseTime"
  namespace           = "AWS/ApplicationELB"
  period              = 60
  extended_statistic  = "p99"
  threshold           = 1.0 # 1 second
  alarm_description   = "ALB P99 latency exceeds 1 second"
  alarm_actions       = [aws_sns_topic.alerts.arn]
  ok_actions          = [aws_sns_topic.alerts.arn]

  dimensions = {
    LoadBalancer = aws_lb.main.arn_suffix
    TargetGroup  = aws_lb_target_group.main.arn_suffix
  }

  tags = {
    Name = "${local.name_prefix}-alb-latency-high"
  }
}

# Unhealthy target count
resource "aws_cloudwatch_metric_alarm" "alb_unhealthy_hosts" {
  alarm_name          = "${local.name_prefix}-alb-unhealthy-hosts"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  metric_name         = "UnHealthyHostCount"
  namespace           = "AWS/ApplicationELB"
  period              = 60
  statistic           = "Average"
  threshold           = 0
  alarm_description   = "ALB has unhealthy targets"
  alarm_actions       = [aws_sns_topic.alerts.arn]
  ok_actions          = [aws_sns_topic.alerts.arn]

  dimensions = {
    LoadBalancer = aws_lb.main.arn_suffix
    TargetGroup  = aws_lb_target_group.main.arn_suffix
  }

  tags = {
    Name = "${local.name_prefix}-alb-unhealthy-hosts"
  }
}

# RDS CPU utilization
resource "aws_cloudwatch_metric_alarm" "rds_cpu_high" {
  count = var.enable_rds ? 1 : 0

  alarm_name          = "${local.name_prefix}-rds-cpu-high"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 3
  metric_name         = "CPUUtilization"
  namespace           = "AWS/RDS"
  period              = 60
  statistic           = "Average"
  threshold           = 80
  alarm_description   = "RDS CPU utilization is above 80%"
  alarm_actions       = [aws_sns_topic.alerts.arn]
  ok_actions          = [aws_sns_topic.alerts.arn]

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.main[0].identifier
  }

  tags = {
    Name = "${local.name_prefix}-rds-cpu-high"
  }
}

# RDS free storage
resource "aws_cloudwatch_metric_alarm" "rds_storage_low" {
  count = var.enable_rds ? 1 : 0

  alarm_name          = "${local.name_prefix}-rds-storage-low"
  comparison_operator = "LessThanThreshold"
  evaluation_periods  = 2
  metric_name         = "FreeStorageSpace"
  namespace           = "AWS/RDS"
  period              = 300
  statistic           = "Average"
  threshold           = 5368709120 # 5 GB
  alarm_description   = "RDS free storage is below 5GB"
  alarm_actions       = [aws_sns_topic.alerts.arn]
  ok_actions          = [aws_sns_topic.alerts.arn]

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.main[0].identifier
  }

  tags = {
    Name = "${local.name_prefix}-rds-storage-low"
  }
}

# RDS connections
resource "aws_cloudwatch_metric_alarm" "rds_connections_high" {
  count = var.enable_rds ? 1 : 0

  alarm_name          = "${local.name_prefix}-rds-connections-high"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  metric_name         = "DatabaseConnections"
  namespace           = "AWS/RDS"
  period              = 60
  statistic           = "Average"
  threshold           = var.db_max_connections * 0.8
  alarm_description   = "RDS connections above 80% of max"
  alarm_actions       = [aws_sns_topic.alerts.arn]
  ok_actions          = [aws_sns_topic.alerts.arn]

  dimensions = {
    DBInstanceIdentifier = aws_db_instance.main[0].identifier
  }

  tags = {
    Name = "${local.name_prefix}-rds-connections-high"
  }
}

# ==============================================================================
# Log-based Metrics
# ==============================================================================

resource "aws_cloudwatch_log_metric_filter" "erlang_crashes" {
  name           = "${local.name_prefix}-erlang-crashes"
  pattern        = "[..., level=\"error\", ..., msg=*\"crash\"* || msg=*\"CRASH\"*]"
  log_group_name = aws_cloudwatch_log_group.app.name

  metric_transformation {
    name      = "ErlangCrashCount"
    namespace = local.name_prefix
    value     = "1"
  }
}

resource "aws_cloudwatch_metric_alarm" "erlang_crashes" {
  alarm_name          = "${local.name_prefix}-erlang-crashes"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 1
  metric_name         = "ErlangCrashCount"
  namespace           = local.name_prefix
  period              = 60
  statistic           = "Sum"
  threshold           = 5
  alarm_description   = "High rate of Erlang process crashes"
  alarm_actions       = [aws_sns_topic.alerts.arn]
  ok_actions          = [aws_sns_topic.alerts.arn]
  treat_missing_data  = "notBreaching"

  tags = {
    Name = "${local.name_prefix}-erlang-crashes"
  }
}

resource "aws_cloudwatch_log_metric_filter" "supervisor_restarts" {
  name           = "${local.name_prefix}-supervisor-restarts"
  pattern        = "[..., msg=*\"Supervisor\"*, ..., msg=*\"restarting\"*]"
  log_group_name = aws_cloudwatch_log_group.app.name

  metric_transformation {
    name      = "SupervisorRestartCount"
    namespace = local.name_prefix
    value     = "1"
  }
}

resource "aws_cloudwatch_log_metric_filter" "mcp_requests" {
  name           = "${local.name_prefix}-mcp-requests"
  pattern        = "[..., method, ..., path=\"/mcp\", ...]"
  log_group_name = aws_cloudwatch_log_group.app.name

  metric_transformation {
    name      = "MCPRequestCount"
    namespace = local.name_prefix
    value     = "1"
  }
}

# ==============================================================================
# CloudWatch Dashboard
# ==============================================================================

resource "aws_cloudwatch_dashboard" "main" {
  dashboard_name = local.name_prefix

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
          markdown = "# ${var.app_name} - ${var.environment} Dashboard"
        }
      },
      # Row 2: ECS Metrics
      {
        type   = "metric"
        x      = 0
        y      = 1
        width  = 8
        height = 6
        properties = {
          title   = "ECS CPU Utilization"
          metrics = [
            ["AWS/ECS", "CPUUtilization", "ClusterName", aws_ecs_cluster.main.name, "ServiceName", aws_ecs_service.main.name]
          ]
          period = 60
          stat   = "Average"
          region = local.region
        }
      },
      {
        type   = "metric"
        x      = 8
        y      = 1
        width  = 8
        height = 6
        properties = {
          title   = "ECS Memory Utilization"
          metrics = [
            ["AWS/ECS", "MemoryUtilization", "ClusterName", aws_ecs_cluster.main.name, "ServiceName", aws_ecs_service.main.name]
          ]
          period = 60
          stat   = "Average"
          region = local.region
        }
      },
      {
        type   = "metric"
        x      = 16
        y      = 1
        width  = 8
        height = 6
        properties = {
          title   = "Running Tasks"
          metrics = [
            ["ECS/ContainerInsights", "RunningTaskCount", "ClusterName", aws_ecs_cluster.main.name, "ServiceName", aws_ecs_service.main.name]
          ]
          period = 60
          stat   = "Average"
          region = local.region
        }
      },
      # Row 3: ALB Metrics
      {
        type   = "metric"
        x      = 0
        y      = 7
        width  = 8
        height = 6
        properties = {
          title   = "Request Count"
          metrics = [
            ["AWS/ApplicationELB", "RequestCount", "LoadBalancer", aws_lb.main.arn_suffix]
          ]
          period = 60
          stat   = "Sum"
          region = local.region
        }
      },
      {
        type   = "metric"
        x      = 8
        y      = 7
        width  = 8
        height = 6
        properties = {
          title   = "Response Time (P50, P95, P99)"
          metrics = [
            ["AWS/ApplicationELB", "TargetResponseTime", "LoadBalancer", aws_lb.main.arn_suffix, { stat = "p50", label = "P50" }],
            ["...", { stat = "p95", label = "P95" }],
            ["...", { stat = "p99", label = "P99" }]
          ]
          period = 60
          region = local.region
        }
      },
      {
        type   = "metric"
        x      = 16
        y      = 7
        width  = 8
        height = 6
        properties = {
          title   = "HTTP Error Codes"
          metrics = [
            ["AWS/ApplicationELB", "HTTPCode_Target_4XX_Count", "LoadBalancer", aws_lb.main.arn_suffix, { label = "4XX" }],
            [".", "HTTPCode_Target_5XX_Count", ".", ".", { label = "5XX" }]
          ]
          period = 60
          stat   = "Sum"
          region = local.region
        }
      },
      # Row 4: Application Metrics
      {
        type   = "metric"
        x      = 0
        y      = 13
        width  = 8
        height = 6
        properties = {
          title   = "MCP Requests"
          metrics = [
            [local.name_prefix, "MCPRequestCount"]
          ]
          period = 60
          stat   = "Sum"
          region = local.region
        }
      },
      {
        type   = "metric"
        x      = 8
        y      = 13
        width  = 8
        height = 6
        properties = {
          title   = "Erlang Crashes"
          metrics = [
            [local.name_prefix, "ErlangCrashCount"]
          ]
          period = 60
          stat   = "Sum"
          region = local.region
        }
      },
      {
        type   = "metric"
        x      = 16
        y      = 13
        width  = 8
        height = 6
        properties = {
          title   = "Supervisor Restarts"
          metrics = [
            [local.name_prefix, "SupervisorRestartCount"]
          ]
          period = 60
          stat   = "Sum"
          region = local.region
        }
      },
      # Row 5: RDS Metrics (conditional)
      {
        type   = "metric"
        x      = 0
        y      = 19
        width  = 8
        height = 6
        properties = {
          title   = "RDS CPU"
          metrics = var.enable_rds ? [
            ["AWS/RDS", "CPUUtilization", "DBInstanceIdentifier", aws_db_instance.main[0].identifier]
          ] : []
          period = 60
          stat   = "Average"
          region = local.region
        }
      },
      {
        type   = "metric"
        x      = 8
        y      = 19
        width  = 8
        height = 6
        properties = {
          title   = "RDS Connections"
          metrics = var.enable_rds ? [
            ["AWS/RDS", "DatabaseConnections", "DBInstanceIdentifier", aws_db_instance.main[0].identifier]
          ] : []
          period = 60
          stat   = "Average"
          region = local.region
        }
      },
      {
        type   = "metric"
        x      = 16
        y      = 19
        width  = 8
        height = 6
        properties = {
          title   = "RDS Free Storage"
          metrics = var.enable_rds ? [
            ["AWS/RDS", "FreeStorageSpace", "DBInstanceIdentifier", aws_db_instance.main[0].identifier]
          ] : []
          period = 300
          stat   = "Average"
          region = local.region
        }
      }
    ]
  })
}

# ==============================================================================
# Slack Notification Lambda
# ==============================================================================

resource "aws_lambda_function" "slack_notifier" {
  count = var.slack_webhook_url != "" ? 1 : 0

  function_name = "${local.name_prefix}-slack-notifier"
  role          = aws_iam_role.slack_notifier[0].arn
  handler       = "index.handler"
  runtime       = "python3.11"
  timeout       = 10

  filename = data.archive_file.slack_notifier[0].output_path

  environment {
    variables = {
      SLACK_WEBHOOK_URL = var.slack_webhook_url
      SLACK_CHANNEL     = var.slack_channel
      ENVIRONMENT       = var.environment
    }
  }

  tags = {
    Name = "${local.name_prefix}-slack-notifier"
  }
}

data "archive_file" "slack_notifier" {
  count = var.slack_webhook_url != "" ? 1 : 0

  type        = "zip"
  output_path = "${path.module}/slack_notifier.zip"

  source {
    content  = <<-EOF
      import json
      import urllib.request
      import os

      def handler(event, context):
          webhook_url = os.environ['SLACK_WEBHOOK_URL']
          channel = os.environ['SLACK_CHANNEL']
          environment = os.environ['ENVIRONMENT']

          for record in event.get('Records', []):
              message = json.loads(record['Sns']['Message'])

              alarm_name = message.get('AlarmName', 'Unknown')
              state = message.get('NewStateValue', 'Unknown')
              reason = message.get('NewStateReason', 'No reason provided')

              color = '#ff0000' if state == 'ALARM' else '#36a64f'
              emoji = ':rotating_light:' if state == 'ALARM' else ':white_check_mark:'

              slack_message = {
                  'channel': channel,
                  'attachments': [{
                      'color': color,
                      'title': f'{emoji} {alarm_name}',
                      'text': f'*Environment:* {environment}\n*State:* {state}\n*Reason:* {reason}',
                      'footer': 'erlmcp CloudWatch Alerts'
                  }]
              }

              req = urllib.request.Request(
                  webhook_url,
                  data=json.dumps(slack_message).encode('utf-8'),
                  headers={'Content-Type': 'application/json'}
              )
              urllib.request.urlopen(req)

          return {'statusCode': 200}
    EOF
    filename = "index.py"
  }
}

resource "aws_iam_role" "slack_notifier" {
  count = var.slack_webhook_url != "" ? 1 : 0

  name = "${local.name_prefix}-slack-notifier"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "lambda.amazonaws.com"
        }
      }
    ]
  })
}

resource "aws_iam_role_policy_attachment" "slack_notifier" {
  count = var.slack_webhook_url != "" ? 1 : 0

  role       = aws_iam_role.slack_notifier[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
}

resource "aws_lambda_permission" "slack_notifier" {
  count = var.slack_webhook_url != "" ? 1 : 0

  statement_id  = "AllowSNS"
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.slack_notifier[0].function_name
  principal     = "sns.amazonaws.com"
  source_arn    = aws_sns_topic.alerts.arn
}

# ==============================================================================
# X-Ray Tracing
# ==============================================================================

resource "aws_xray_sampling_rule" "main" {
  count = var.enable_xray ? 1 : 0

  rule_name      = local.name_prefix
  priority       = 1000
  version        = 1
  reservoir_size = 1
  fixed_rate     = 0.05 # 5% sampling
  url_path       = "*"
  host           = "*"
  http_method    = "*"
  service_type   = "*"
  service_name   = var.app_name
  resource_arn   = "*"

  attributes = {
    Environment = var.environment
  }
}
