# ============================================================================
# ECS Fargate Deployment for erlmcp
# ============================================================================
# Containerized deployment using AWS ECS Fargate
# Features:
#   - Serverless container orchestration (no EC2 management)
#   - Auto scaling based on CPU/memory utilization
#   - Integration with ALB for load balancing
#   - Secrets injection from SSM Parameter Store
#   - CloudWatch logging and metrics
#
# DOCKER-ONLY CONSTITUTION: Containers are built via Docker, deployed via ECS.
# ============================================================================

# ============================================================================
# ECS Cluster
# ============================================================================

resource "aws_ecs_cluster" "main" {
  count = local.deploy_ecs ? 1 : 0
  name  = "${local.name_prefix}-cluster"

  setting {
    name  = "containerInsights"
    value = "enabled"
  }

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-cluster"
  })
}

resource "aws_ecs_cluster_capacity_providers" "main" {
  count        = local.deploy_ecs ? 1 : 0
  cluster_name = aws_ecs_cluster.main[0].name

  capacity_providers = ["FARGATE", "FARGATE_SPOT"]

  default_capacity_provider_strategy {
    base              = 1
    weight            = 1
    capacity_provider = "FARGATE"
  }

  default_capacity_provider_strategy {
    weight            = var.environment == "production" ? 0 : 3
    capacity_provider = "FARGATE_SPOT"
  }
}

# ============================================================================
# IAM Roles
# ============================================================================

# Task Execution Role - Used by ECS agent to pull images and send logs
resource "aws_iam_role" "ecs_task_execution" {
  count = local.deploy_ecs ? 1 : 0
  name  = "${local.name_prefix}-ecs-execution-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "ecs-tasks.amazonaws.com"
      }
    }]
  })

  tags = local.common_tags
}

resource "aws_iam_role_policy_attachment" "ecs_task_execution" {
  count      = local.deploy_ecs ? 1 : 0
  role       = aws_iam_role.ecs_task_execution[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

# Policy for accessing SSM parameters and Secrets Manager
resource "aws_iam_role_policy" "ecs_task_secrets" {
  count = local.deploy_ecs ? 1 : 0
  name  = "${local.name_prefix}-ecs-secrets-policy"
  role  = aws_iam_role.ecs_task_execution[0].id

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
      }
    ]
  })
}

# Task Role - Used by the application itself for AWS API calls
resource "aws_iam_role" "ecs_task" {
  count = local.deploy_ecs ? 1 : 0
  name  = "${local.name_prefix}-ecs-task-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "ecs-tasks.amazonaws.com"
      }
    }]
  })

  tags = local.common_tags
}

# Task role permissions (customize based on app needs)
resource "aws_iam_role_policy" "ecs_task" {
  count = local.deploy_ecs ? 1 : 0
  name  = "${local.name_prefix}-ecs-task-policy"
  role  = aws_iam_role.ecs_task[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "ssm:GetParameter",
          "ssm:GetParameters"
        ]
        Resource = [
          "arn:aws:ssm:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:parameter/erlmcp/${var.environment}/*"
        ]
      },
      {
        Effect = "Allow"
        Action = [
          "s3:GetObject",
          "s3:PutObject",
          "s3:ListBucket"
        ]
        Resource = [
          aws_s3_bucket.logs.arn,
          "${aws_s3_bucket.logs.arn}/*"
        ]
      }
    ]
  })
}

# ============================================================================
# ECS Task Definition
# ============================================================================

resource "aws_ecs_task_definition" "app" {
  count  = local.deploy_ecs ? 1 : 0
  family = "${local.name_prefix}-task"

  network_mode             = "awsvpc"
  requires_compatibilities = ["FARGATE"]
  cpu                      = var.ecs_cpu
  memory                   = var.ecs_memory
  execution_role_arn       = aws_iam_role.ecs_task_execution[0].arn
  task_role_arn            = aws_iam_role.ecs_task[0].arn

  container_definitions = jsonencode([
    {
      name      = "erlmcp"
      image     = "${var.ecr_repository_url}:${var.container_image_tag}"
      essential = true

      portMappings = [
        {
          containerPort = var.app_port
          protocol      = "tcp"
        },
        {
          containerPort = var.metrics_port
          protocol      = "tcp"
        }
      ]

      environment = [
        {
          name  = "ERLMCP_ENV"
          value = var.environment
        },
        {
          name  = "ERLMCP_PORT"
          value = tostring(var.app_port)
        },
        {
          name  = "ERLMCP_METRICS_PORT"
          value = tostring(var.metrics_port)
        },
        {
          name  = "ERLMCP_LOG_LEVEL"
          value = var.environment == "production" ? "info" : "debug"
        },
        {
          name  = "AWS_REGION"
          value = data.aws_region.current.name
        }
      ]

      secrets = [
        {
          name      = "ERLANG_COOKIE"
          valueFrom = "arn:aws:ssm:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:parameter/erlmcp/${var.environment}/erlang_cookie"
        },
        {
          name      = "DB_PASSWORD"
          valueFrom = var.db_password_ssm_arn != "" ? var.db_password_ssm_arn : "arn:aws:ssm:${data.aws_region.current.name}:${data.aws_caller_identity.current.account_id}:parameter/erlmcp/${var.environment}/db_password"
        }
      ]

      healthCheck = {
        command     = ["CMD-SHELL", "curl -f http://localhost:${var.app_port}/health || exit 1"]
        interval    = 30
        timeout     = 5
        retries     = 3
        startPeriod = 60
      }

      logConfiguration = {
        logDriver = "awslogs"
        options = {
          "awslogs-group"         = aws_cloudwatch_log_group.ecs[0].name
          "awslogs-region"        = data.aws_region.current.name
          "awslogs-stream-prefix" = "erlmcp"
        }
      }

      # Erlang VM resource limits
      ulimits = [
        {
          name      = "nofile"
          softLimit = 65536
          hardLimit = 65536
        }
      ]

      linuxParameters = {
        initProcessEnabled = true
      }
    }
  ])

  tags = local.common_tags
}

# ============================================================================
# ECS Service
# ============================================================================

resource "aws_ecs_service" "app" {
  count           = local.deploy_ecs ? 1 : 0
  name            = "${local.name_prefix}-service"
  cluster         = aws_ecs_cluster.main[0].id
  task_definition = aws_ecs_task_definition.app[0].arn
  desired_count   = var.ecs_desired_count
  launch_type     = "FARGATE"

  network_configuration {
    subnets          = module.vpc.private_subnets
    security_groups  = [aws_security_group.app.id]
    assign_public_ip = false
  }

  load_balancer {
    target_group_arn = aws_lb_target_group.app.arn
    container_name   = "erlmcp"
    container_port   = var.app_port
  }

  deployment_minimum_healthy_percent = 50
  deployment_maximum_percent         = 200
  health_check_grace_period_seconds  = 60

  deployment_circuit_breaker {
    enable   = true
    rollback = true
  }

  # Enable ECS Service Connect for service discovery (optional)
  # service_connect_configuration {
  #   enabled = true
  #   namespace = aws_service_discovery_private_dns_namespace.main.arn
  # }

  lifecycle {
    ignore_changes = [desired_count]
  }

  tags = local.common_tags
}

# ============================================================================
# Auto Scaling
# ============================================================================

resource "aws_appautoscaling_target" "ecs" {
  count              = local.deploy_ecs ? 1 : 0
  max_capacity       = var.ecs_max_count
  min_capacity       = var.ecs_min_count
  resource_id        = "service/${aws_ecs_cluster.main[0].name}/${aws_ecs_service.app[0].name}"
  scalable_dimension = "ecs:service:DesiredCount"
  service_namespace  = "ecs"
}

# Scale based on CPU utilization
resource "aws_appautoscaling_policy" "ecs_cpu" {
  count              = local.deploy_ecs ? 1 : 0
  name               = "${local.name_prefix}-cpu-scaling"
  policy_type        = "TargetTrackingScaling"
  resource_id        = aws_appautoscaling_target.ecs[0].resource_id
  scalable_dimension = aws_appautoscaling_target.ecs[0].scalable_dimension
  service_namespace  = aws_appautoscaling_target.ecs[0].service_namespace

  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ECSServiceAverageCPUUtilization"
    }
    target_value       = var.cpu_target_value
    scale_in_cooldown  = 300
    scale_out_cooldown = 60
  }
}

# Scale based on memory utilization
resource "aws_appautoscaling_policy" "ecs_memory" {
  count              = local.deploy_ecs ? 1 : 0
  name               = "${local.name_prefix}-memory-scaling"
  policy_type        = "TargetTrackingScaling"
  resource_id        = aws_appautoscaling_target.ecs[0].resource_id
  scalable_dimension = aws_appautoscaling_target.ecs[0].scalable_dimension
  service_namespace  = aws_appautoscaling_target.ecs[0].service_namespace

  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ECSServiceAverageMemoryUtilization"
    }
    target_value       = var.memory_target_value
    scale_in_cooldown  = 300
    scale_out_cooldown = 60
  }
}

# Scale based on ALB request count
resource "aws_appautoscaling_policy" "ecs_requests" {
  count              = local.deploy_ecs ? 1 : 0
  name               = "${local.name_prefix}-request-scaling"
  policy_type        = "TargetTrackingScaling"
  resource_id        = aws_appautoscaling_target.ecs[0].resource_id
  scalable_dimension = aws_appautoscaling_target.ecs[0].scalable_dimension
  service_namespace  = aws_appautoscaling_target.ecs[0].service_namespace

  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "ALBRequestCountPerTarget"
      resource_label         = "${aws_lb.main.arn_suffix}/${aws_lb_target_group.app.arn_suffix}"
    }
    target_value       = 1000 # Requests per target per minute
    scale_in_cooldown  = 300
    scale_out_cooldown = 60
  }
}

# ============================================================================
# ECR Repository (if not using external registry)
# ============================================================================

resource "aws_ecr_repository" "app" {
  count                = local.deploy_ecs && var.ecr_repository_url == "" ? 1 : 0
  name                 = "erlmcp"
  image_tag_mutability = "MUTABLE"

  image_scanning_configuration {
    scan_on_push = true
  }

  encryption_configuration {
    encryption_type = "AES256"
  }

  tags = local.common_tags
}

resource "aws_ecr_lifecycle_policy" "app" {
  count      = local.deploy_ecs && var.ecr_repository_url == "" ? 1 : 0
  repository = aws_ecr_repository.app[0].name

  policy = jsonencode({
    rules = [
      {
        rulePriority = 1
        description  = "Keep last 30 images"
        selection = {
          tagStatus   = "any"
          countType   = "imageCountMoreThan"
          countNumber = 30
        }
        action = {
          type = "expire"
        }
      }
    ]
  })
}
