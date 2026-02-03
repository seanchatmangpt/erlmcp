# ============================================================================
# EC2 Auto Scaling Group Deployment for erlmcp
# ============================================================================
# Immutable infrastructure using custom AMIs built with Packer
# Features:
#   - Pre-baked AMIs with Erlang/OTP runtime and application
#   - Auto Scaling Group for high availability
#   - Launch Template for instance configuration
#   - Integration with ALB for load balancing
#   - CloudWatch Agent for logging and metrics
#   - Systemd service management
#
# DOCKER-ONLY CONSTITUTION: AMIs built via Packer using Docker.
# ============================================================================

# ============================================================================
# IAM Role for EC2 Instances
# ============================================================================

resource "aws_iam_role" "ec2_instance" {
  count = local.deploy_ec2 ? 1 : 0
  name  = "${local.name_prefix}-ec2-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "ec2.amazonaws.com"
      }
    }]
  })

  tags = local.common_tags
}

resource "aws_iam_instance_profile" "ec2" {
  count = local.deploy_ec2 ? 1 : 0
  name  = "${local.name_prefix}-ec2-profile"
  role  = aws_iam_role.ec2_instance[0].name
}

# Attach SSM policy for Systems Manager access
resource "aws_iam_role_policy_attachment" "ec2_ssm" {
  count      = local.deploy_ec2 ? 1 : 0
  role       = aws_iam_role.ec2_instance[0].name
  policy_arn = "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore"
}

# Attach CloudWatch policy for logging/metrics
resource "aws_iam_role_policy_attachment" "ec2_cloudwatch" {
  count      = local.deploy_ec2 ? 1 : 0
  role       = aws_iam_role.ec2_instance[0].name
  policy_arn = "arn:aws:iam::aws:policy/CloudWatchAgentServerPolicy"
}

# Custom policy for application-specific access
resource "aws_iam_role_policy" "ec2_app" {
  count = local.deploy_ec2 ? 1 : 0
  name  = "${local.name_prefix}-ec2-app-policy"
  role  = aws_iam_role.ec2_instance[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "ssm:GetParameter",
          "ssm:GetParameters",
          "ssm:GetParametersByPath"
        ]
        Resource = [
          "arn:aws:ssm:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:parameter/erlmcp/${var.environment}/*"
        ]
      },
      {
        Effect = "Allow"
        Action = [
          "secretsmanager:GetSecretValue"
        ]
        Resource = [
          "arn:aws:secretsmanager:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:secret:erlmcp/${var.environment}/*"
        ]
      },
      {
        Effect = "Allow"
        Action = [
          "kms:Decrypt"
        ]
        Resource = "*"
        Condition = {
          StringEquals = {
            "kms:ViaService" = "ssm.${data.aws_region.current.name}.amazonaws.com"
          }
        }
      },
      {
        Effect = "Allow"
        Action = [
          "s3:GetObject",
          "s3:PutObject"
        ]
        Resource = [
          "${aws_s3_bucket.logs.arn}/*"
        ]
      },
      {
        Effect = "Allow"
        Action = [
          "ec2:DescribeInstances",
          "ec2:DescribeTags"
        ]
        Resource = "*"
      }
    ]
  })
}

# ============================================================================
# Launch Template
# ============================================================================

resource "aws_launch_template" "app" {
  count = local.deploy_ec2 ? 1 : 0
  name  = "${local.name_prefix}-lt"

  image_id      = var.ami_id
  instance_type = var.instance_type
  key_name      = var.key_name != "" ? var.key_name : null

  iam_instance_profile {
    arn = aws_iam_instance_profile.ec2[0].arn
  }

  network_interfaces {
    associate_public_ip_address = false
    security_groups             = [aws_security_group.app.id]
    delete_on_termination       = true
  }

  monitoring {
    enabled = true
  }

  # Enable IMDSv2 (secure metadata service)
  metadata_options {
    http_endpoint               = "enabled"
    http_tokens                 = "required"
    http_put_response_hop_limit = 1
    instance_metadata_tags      = "enabled"
  }

  # EBS optimization
  ebs_optimized = true

  block_device_mappings {
    device_name = "/dev/xvda"
    ebs {
      volume_size           = 20
      volume_type           = "gp3"
      iops                  = 3000
      throughput            = 125
      encrypted             = true
      delete_on_termination = true
    }
  }

  # User data for cloud-init
  user_data = base64encode(templatefile("${path.module}/templates/user_data.sh.tpl", {
    environment     = var.environment
    region          = data.aws_region.current.name
    log_group       = aws_cloudwatch_log_group.app.name
    app_port        = var.app_port
    metrics_port    = var.metrics_port
    cluster_cookie  = var.erlang_cookie
  }))

  tag_specifications {
    resource_type = "instance"
    tags = merge(local.common_tags, {
      Name = "${local.name_prefix}-instance"
    })
  }

  tag_specifications {
    resource_type = "volume"
    tags = merge(local.common_tags, {
      Name = "${local.name_prefix}-volume"
    })
  }

  tags = local.common_tags

  lifecycle {
    create_before_destroy = true
  }
}

# ============================================================================
# Auto Scaling Group
# ============================================================================

resource "aws_autoscaling_group" "app" {
  count = local.deploy_ec2 ? 1 : 0
  name  = "${local.name_prefix}-asg"

  vpc_zone_identifier = module.vpc.private_subnets
  target_group_arns   = [aws_lb_target_group.app.arn]
  health_check_type   = "ELB"

  min_size         = var.ec2_min_size
  max_size         = var.ec2_max_size
  desired_capacity = var.ec2_desired_capacity

  health_check_grace_period = 300
  default_cooldown          = 300
  termination_policies      = ["OldestInstance"]

  launch_template {
    id      = aws_launch_template.app[0].id
    version = "$Latest"
  }

  instance_refresh {
    strategy = "Rolling"
    preferences {
      min_healthy_percentage = 50
      instance_warmup        = 300
    }
    triggers = ["tag"]
  }

  # Lifecycle hooks for graceful deployment
  initial_lifecycle_hook {
    name                    = "startup-hook"
    lifecycle_transition    = "autoscaling:EC2_INSTANCE_LAUNCHING"
    default_result          = "CONTINUE"
    heartbeat_timeout       = 300
    notification_target_arn = aws_sns_topic.asg_notifications[0].arn
    role_arn                = aws_iam_role.asg_lifecycle[0].arn
  }

  initial_lifecycle_hook {
    name                    = "termination-hook"
    lifecycle_transition    = "autoscaling:EC2_INSTANCE_TERMINATING"
    default_result          = "CONTINUE"
    heartbeat_timeout       = 300
    notification_target_arn = aws_sns_topic.asg_notifications[0].arn
    role_arn                = aws_iam_role.asg_lifecycle[0].arn
  }

  enabled_metrics = [
    "GroupMinSize",
    "GroupMaxSize",
    "GroupDesiredCapacity",
    "GroupInServiceInstances",
    "GroupPendingInstances",
    "GroupStandbyInstances",
    "GroupTerminatingInstances",
    "GroupTotalInstances"
  ]

  tag {
    key                 = "Name"
    value               = "${local.name_prefix}-instance"
    propagate_at_launch = true
  }

  dynamic "tag" {
    for_each = local.common_tags
    content {
      key                 = tag.key
      value               = tag.value
      propagate_at_launch = true
    }
  }

  lifecycle {
    create_before_destroy = true
    ignore_changes        = [desired_capacity]
  }
}

# ============================================================================
# Auto Scaling Policies
# ============================================================================

resource "aws_autoscaling_policy" "cpu_scale_out" {
  count                  = local.deploy_ec2 ? 1 : 0
  name                   = "${local.name_prefix}-cpu-scale-out"
  autoscaling_group_name = aws_autoscaling_group.app[0].name
  policy_type            = "TargetTrackingScaling"

  target_tracking_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ASGAverageCPUUtilization"
    }
    target_value = var.cpu_target_value
  }
}

resource "aws_autoscaling_policy" "alb_requests" {
  count                  = local.deploy_ec2 ? 1 : 0
  name                   = "${local.name_prefix}-alb-requests"
  autoscaling_group_name = aws_autoscaling_group.app[0].name
  policy_type            = "TargetTrackingScaling"

  target_tracking_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ALBRequestCountPerTarget"
      resource_label         = "${aws_lb.main.arn_suffix}/${aws_lb_target_group.app.arn_suffix}"
    }
    target_value = 1000
  }
}

# ============================================================================
# SNS Topic for ASG Notifications
# ============================================================================

resource "aws_sns_topic" "asg_notifications" {
  count = local.deploy_ec2 ? 1 : 0
  name  = "${local.name_prefix}-asg-notifications"

  tags = local.common_tags
}

# IAM Role for ASG Lifecycle Hooks
resource "aws_iam_role" "asg_lifecycle" {
  count = local.deploy_ec2 ? 1 : 0
  name  = "${local.name_prefix}-asg-lifecycle-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "autoscaling.amazonaws.com"
      }
    }]
  })

  tags = local.common_tags
}

resource "aws_iam_role_policy" "asg_lifecycle" {
  count = local.deploy_ec2 ? 1 : 0
  name  = "${local.name_prefix}-asg-lifecycle-policy"
  role  = aws_iam_role.asg_lifecycle[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect   = "Allow"
      Action   = ["sns:Publish"]
      Resource = aws_sns_topic.asg_notifications[0].arn
    }]
  })
}

# ============================================================================
# CloudWatch Alarms for ASG
# ============================================================================

resource "aws_cloudwatch_metric_alarm" "high_cpu" {
  count               = local.deploy_ec2 ? 1 : 0
  alarm_name          = "${local.name_prefix}-high-cpu"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  metric_name         = "CPUUtilization"
  namespace           = "AWS/EC2"
  period              = 120
  statistic           = "Average"
  threshold           = 80
  alarm_description   = "CPU utilization above 80%"

  dimensions = {
    AutoScalingGroupName = aws_autoscaling_group.app[0].name
  }

  alarm_actions = [aws_sns_topic.asg_notifications[0].arn]
  ok_actions    = [aws_sns_topic.asg_notifications[0].arn]

  tags = local.common_tags
}

resource "aws_cloudwatch_metric_alarm" "unhealthy_hosts" {
  count               = local.deploy_ec2 ? 1 : 0
  alarm_name          = "${local.name_prefix}-unhealthy-hosts"
  comparison_operator = "GreaterThanThreshold"
  evaluation_periods  = 2
  metric_name         = "UnHealthyHostCount"
  namespace           = "AWS/ApplicationELB"
  period              = 60
  statistic           = "Average"
  threshold           = 0
  alarm_description   = "Unhealthy hosts in target group"

  dimensions = {
    LoadBalancer = aws_lb.main.arn_suffix
    TargetGroup  = aws_lb_target_group.app.arn_suffix
  }

  alarm_actions = [aws_sns_topic.asg_notifications[0].arn]

  tags = local.common_tags
}
