# ==============================================================================
# AWS Secrets Manager and IAM Configuration
# ==============================================================================
# Secure secrets management and IAM roles for ECS tasks.
# ==============================================================================

# ==============================================================================
# KMS Key for Encryption
# ==============================================================================

resource "aws_kms_key" "main" {
  description             = "KMS key for ${local.name_prefix} encryption"
  deletion_window_in_days = 30
  enable_key_rotation     = true
  multi_region            = var.enable_multi_region_kms

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid    = "Enable IAM User Permissions"
        Effect = "Allow"
        Principal = {
          AWS = "arn:aws:iam::${local.account_id}:root"
        }
        Action   = "kms:*"
        Resource = "*"
      },
      {
        Sid    = "Allow CloudWatch Logs"
        Effect = "Allow"
        Principal = {
          Service = "logs.${local.region}.amazonaws.com"
        }
        Action = [
          "kms:Encrypt*",
          "kms:Decrypt*",
          "kms:ReEncrypt*",
          "kms:GenerateDataKey*",
          "kms:Describe*"
        ]
        Resource = "*"
        Condition = {
          ArnLike = {
            "kms:EncryptionContext:aws:logs:arn" = "arn:aws:logs:${local.region}:${local.account_id}:*"
          }
        }
      },
      {
        Sid    = "Allow Secrets Manager"
        Effect = "Allow"
        Principal = {
          Service = "secretsmanager.amazonaws.com"
        }
        Action = [
          "kms:Encrypt",
          "kms:Decrypt",
          "kms:GenerateDataKey*"
        ]
        Resource = "*"
      }
    ]
  })

  tags = {
    Name = "${local.name_prefix}-kms-key"
  }
}

resource "aws_kms_alias" "main" {
  name          = "alias/${local.name_prefix}"
  target_key_id = aws_kms_key.main.key_id
}

# ==============================================================================
# Secrets Manager Secrets
# ==============================================================================

# Erlang Cookie
resource "aws_secretsmanager_secret" "erlang_cookie" {
  name       = "${local.name_prefix}/erlang-cookie"
  kms_key_id = aws_kms_key.main.arn

  recovery_window_in_days = var.environment == "production" ? 30 : 0

  tags = {
    Name = "${local.name_prefix}-erlang-cookie"
  }
}

resource "aws_secretsmanager_secret_version" "erlang_cookie" {
  secret_id     = aws_secretsmanager_secret.erlang_cookie.id
  secret_string = local.erlang_cookie
}

# Database URL
resource "aws_secretsmanager_secret" "database_url" {
  name       = "${local.name_prefix}/database-url"
  kms_key_id = aws_kms_key.main.arn

  recovery_window_in_days = var.environment == "production" ? 30 : 0

  tags = {
    Name = "${local.name_prefix}-database-url"
  }
}

resource "aws_secretsmanager_secret_version" "database_url" {
  secret_id = aws_secretsmanager_secret.database_url.id
  secret_string = var.enable_rds ? (
    var.enable_rds_proxy ?
      "postgresql://${var.db_username}:${local.db_password}@${aws_db_proxy.main[0].endpoint}:5432/${var.db_name}?sslmode=require" :
      "postgresql://${var.db_username}:${local.db_password}@${aws_db_instance.main[0].endpoint}/${var.db_name}?sslmode=require"
  ) : var.database_url
}

# API Keys
resource "aws_secretsmanager_secret" "api_keys" {
  name       = "${local.name_prefix}/api-keys"
  kms_key_id = aws_kms_key.main.arn

  recovery_window_in_days = var.environment == "production" ? 30 : 0

  tags = {
    Name = "${local.name_prefix}-api-keys"
  }
}

resource "aws_secretsmanager_secret_version" "api_keys" {
  secret_id = aws_secretsmanager_secret.api_keys.id
  secret_string = jsonencode({
    mcp_api_key     = var.mcp_api_key
    webhook_secret  = var.webhook_secret
  })
}

# ==============================================================================
# SSM Parameters (for non-sensitive configuration)
# ==============================================================================

resource "aws_ssm_parameter" "app_config" {
  name  = "/${local.name_prefix}/config"
  type  = "String"
  value = jsonencode({
    environment     = var.environment
    region          = local.region
    app_name        = var.app_name
    app_version     = var.app_version
    cluster_enabled = var.enable_service_discovery
    log_level       = var.log_level
  })

  tags = {
    Name = "${local.name_prefix}-config"
  }
}

# ==============================================================================
# ECS Task Execution Role
# ==============================================================================

resource "aws_iam_role" "ecs_execution" {
  name = "${local.name_prefix}-ecs-execution"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "ecs-tasks.amazonaws.com"
        }
      }
    ]
  })

  tags = {
    Name = "${local.name_prefix}-ecs-execution"
  }
}

resource "aws_iam_role_policy_attachment" "ecs_execution" {
  role       = aws_iam_role.ecs_execution.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

resource "aws_iam_role_policy" "ecs_execution_secrets" {
  name = "${local.name_prefix}-ecs-execution-secrets"
  role = aws_iam_role.ecs_execution.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "secretsmanager:GetSecretValue"
        ]
        Resource = [
          aws_secretsmanager_secret.erlang_cookie.arn,
          aws_secretsmanager_secret.database_url.arn,
          aws_secretsmanager_secret.api_keys.arn
        ]
      },
      {
        Effect = "Allow"
        Action = [
          "kms:Decrypt"
        ]
        Resource = aws_kms_key.main.arn
      },
      {
        Effect = "Allow"
        Action = [
          "ssm:GetParameters",
          "ssm:GetParameter"
        ]
        Resource = "arn:aws:ssm:${local.region}:${local.account_id}:parameter/${local.name_prefix}/*"
      }
    ]
  })
}

# ==============================================================================
# ECS Task Role (for application permissions)
# ==============================================================================

resource "aws_iam_role" "ecs_task" {
  name = "${local.name_prefix}-ecs-task"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "ecs-tasks.amazonaws.com"
        }
      }
    ]
  })

  tags = {
    Name = "${local.name_prefix}-ecs-task"
  }
}

resource "aws_iam_role_policy" "ecs_task" {
  name = "${local.name_prefix}-ecs-task-policy"
  role = aws_iam_role.ecs_task.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      # S3 access for artifacts
      {
        Effect = "Allow"
        Action = [
          "s3:GetObject",
          "s3:PutObject",
          "s3:ListBucket"
        ]
        Resource = [
          "arn:aws:s3:::${local.name_prefix}-*",
          "arn:aws:s3:::${local.name_prefix}-*/*"
        ]
      },
      # CloudWatch for custom metrics
      {
        Effect = "Allow"
        Action = [
          "cloudwatch:PutMetricData"
        ]
        Resource = "*"
        Condition = {
          StringEquals = {
            "cloudwatch:namespace" = local.name_prefix
          }
        }
      },
      # X-Ray for tracing
      {
        Effect = "Allow"
        Action = [
          "xray:PutTraceSegments",
          "xray:PutTelemetryRecords",
          "xray:GetSamplingRules",
          "xray:GetSamplingTargets",
          "xray:GetSamplingStatisticSummaries"
        ]
        Resource = "*"
      },
      # ECS Exec
      {
        Effect = "Allow"
        Action = [
          "ssmmessages:CreateControlChannel",
          "ssmmessages:CreateDataChannel",
          "ssmmessages:OpenControlChannel",
          "ssmmessages:OpenDataChannel"
        ]
        Resource = "*"
      },
      # KMS for ECS Exec logging
      {
        Effect = "Allow"
        Action = [
          "kms:Decrypt"
        ]
        Resource = aws_kms_key.main.arn
      },
      # Logs for ECS Exec
      {
        Effect = "Allow"
        Action = [
          "logs:DescribeLogGroups",
          "logs:CreateLogStream",
          "logs:PutLogEvents"
        ]
        Resource = "*"
      },
      # Service discovery for Erlang clustering
      {
        Effect = "Allow"
        Action = [
          "servicediscovery:DiscoverInstances"
        ]
        Resource = "*"
      },
      # EC2 for getting instance metadata (for clustering)
      {
        Effect = "Allow"
        Action = [
          "ec2:DescribeInstances",
          "ec2:DescribeTags"
        ]
        Resource = "*"
      },
      # ECS for getting task metadata (for clustering)
      {
        Effect = "Allow"
        Action = [
          "ecs:DescribeTasks",
          "ecs:ListTasks"
        ]
        Resource = "*"
        Condition = {
          ArnEquals = {
            "ecs:cluster" = aws_ecs_cluster.main.arn
          }
        }
      }
    ]
  })
}

# ==============================================================================
# Secrets Rotation (optional)
# ==============================================================================

resource "aws_secretsmanager_secret_rotation" "database_credentials" {
  count = var.enable_rds && var.enable_rds_proxy && var.enable_secret_rotation ? 1 : 0

  secret_id           = aws_secretsmanager_secret.database_credentials[0].id
  rotation_lambda_arn = aws_lambda_function.secret_rotation[0].arn

  rotation_rules {
    automatically_after_days = 30
  }
}

# Lambda function for secret rotation (simplified - in production use AWS-provided rotation)
resource "aws_lambda_function" "secret_rotation" {
  count = var.enable_rds && var.enable_rds_proxy && var.enable_secret_rotation ? 1 : 0

  function_name = "${local.name_prefix}-secret-rotation"
  role          = aws_iam_role.secret_rotation[0].arn
  handler       = "index.handler"
  runtime       = "python3.11"
  timeout       = 30

  filename = data.archive_file.secret_rotation[0].output_path

  vpc_config {
    subnet_ids         = aws_subnet.private[*].id
    security_group_ids = [aws_security_group.rds[0].id]
  }

  environment {
    variables = {
      SECRETS_MANAGER_ENDPOINT = "https://secretsmanager.${local.region}.amazonaws.com"
    }
  }

  tags = {
    Name = "${local.name_prefix}-secret-rotation"
  }
}

data "archive_file" "secret_rotation" {
  count = var.enable_rds && var.enable_rds_proxy && var.enable_secret_rotation ? 1 : 0

  type        = "zip"
  output_path = "${path.module}/secret_rotation.zip"

  source {
    content  = <<-EOF
      import boto3
      import json
      import logging

      logger = logging.getLogger()
      logger.setLevel(logging.INFO)

      def handler(event, context):
          """Handle secret rotation events."""
          logger.info("Secret rotation triggered: %s", json.dumps(event))
          # In production, implement full rotation logic
          # See: https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotating-secrets-lambda-function-overview.html
          return {"statusCode": 200}
    EOF
    filename = "index.py"
  }
}

resource "aws_iam_role" "secret_rotation" {
  count = var.enable_rds && var.enable_rds_proxy && var.enable_secret_rotation ? 1 : 0

  name = "${local.name_prefix}-secret-rotation"

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

resource "aws_iam_role_policy_attachment" "secret_rotation_vpc" {
  count = var.enable_rds && var.enable_rds_proxy && var.enable_secret_rotation ? 1 : 0

  role       = aws_iam_role.secret_rotation[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaVPCAccessExecutionRole"
}

resource "aws_iam_role_policy" "secret_rotation" {
  count = var.enable_rds && var.enable_rds_proxy && var.enable_secret_rotation ? 1 : 0

  name = "${local.name_prefix}-secret-rotation-policy"
  role = aws_iam_role.secret_rotation[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "secretsmanager:DescribeSecret",
          "secretsmanager:GetSecretValue",
          "secretsmanager:PutSecretValue",
          "secretsmanager:UpdateSecretVersionStage"
        ]
        Resource = aws_secretsmanager_secret.database_credentials[0].arn
      },
      {
        Effect = "Allow"
        Action = [
          "kms:Decrypt",
          "kms:GenerateDataKey"
        ]
        Resource = aws_kms_key.main.arn
      }
    ]
  })
}

resource "aws_lambda_permission" "secret_rotation" {
  count = var.enable_rds && var.enable_rds_proxy && var.enable_secret_rotation ? 1 : 0

  statement_id  = "AllowSecretsManager"
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.secret_rotation[0].function_name
  principal     = "secretsmanager.amazonaws.com"
}
