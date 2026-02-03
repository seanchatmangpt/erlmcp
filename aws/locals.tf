# ==============================================================================
# Local Values
# ==============================================================================
# Common values used across multiple resources.
# ==============================================================================

locals {
  # Common tags applied to all resources
  common_tags = {
    Application = var.app_name
    Environment = var.environment
    ManagedBy   = "terraform"
    Project     = "erlmcp"
    CostCenter  = var.cost_center
  }

  # Computed resource names
  name_prefix = "${var.app_name}-${var.environment}"

  # Account and region info
  account_id = data.aws_caller_identity.current.account_id
  region     = data.aws_region.current.name

  # Availability zones (use first 3)
  azs = slice(data.aws_availability_zones.available.names, 0, min(3, length(data.aws_availability_zones.available.names)))

  # Database password (generated or provided)
  db_password = var.db_password != "" ? var.db_password : random_password.db_password[0].result

  # Erlang cookie (generated or provided)
  erlang_cookie = var.erlang_cookie != "" ? var.erlang_cookie : random_password.erlang_cookie[0].result

  # Container image
  container_image = var.container_image != "" ? var.container_image : "${local.account_id}.dkr.ecr.${local.region}.amazonaws.com/${var.app_name}:${var.app_version}"

  # CIDR blocks for subnets
  vpc_cidr = var.vpc_cidr

  # Calculate subnet CIDRs from VPC CIDR
  public_subnet_cidrs = [
    for i, az in local.azs : cidrsubnet(var.vpc_cidr, 4, i)
  ]

  private_subnet_cidrs = [
    for i, az in local.azs : cidrsubnet(var.vpc_cidr, 4, i + 4)
  ]

  database_subnet_cidrs = [
    for i, az in local.azs : cidrsubnet(var.vpc_cidr, 4, i + 8)
  ]

  # ECS container definitions
  container_definitions = jsonencode([
    {
      name      = var.app_name
      image     = local.container_image
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
        { name = "ERLMCP_PROFILE", value = var.environment },
        { name = "ERLMCP_PORT", value = tostring(var.app_port) },
        { name = "ERLMCP_METRICS_PORT", value = tostring(var.metrics_port) },
        { name = "AWS_REGION", value = local.region },
        { name = "RELEASE_DISTRIBUTION", value = "name" },
        { name = "RELEASE_NODE", value = "erlmcp@$(hostname -i)" }
      ]

      secrets = [
        {
          name      = "ERLANG_COOKIE"
          valueFrom = aws_secretsmanager_secret.erlang_cookie.arn
        },
        {
          name      = "DATABASE_URL"
          valueFrom = aws_secretsmanager_secret.database_url.arn
        }
      ]

      logConfiguration = {
        logDriver = "awslogs"
        options = {
          "awslogs-group"         = aws_cloudwatch_log_group.app.name
          "awslogs-region"        = local.region
          "awslogs-stream-prefix" = var.app_name
        }
      }

      healthCheck = {
        command     = ["CMD-SHELL", "curl -f http://localhost:${var.app_port}/health || exit 1"]
        interval    = 30
        timeout     = 5
        retries     = 3
        startPeriod = 60
      }

      # BEAM-aware resource limits
      cpu    = var.task_cpu
      memory = var.task_memory

      # ulimits for BEAM
      ulimits = [
        {
          name      = "nofile"
          softLimit = 65536
          hardLimit = 65536
        }
      ]
    }
  ])
}
