# ============================================================================
# Secrets Management for erlmcp
# ============================================================================
# Manages secrets using AWS SSM Parameter Store and Secrets Manager
# Features:
#   - Encrypted parameters using KMS
#   - Automatic rotation for database credentials
#   - Integration with ECS and EC2 IAM roles
#
# Secrets are organized by environment:
#   /erlmcp/{environment}/erlang_cookie
#   /erlmcp/{environment}/db_password
#   /erlmcp/{environment}/api_key
# ============================================================================

# ============================================================================
# KMS Key for Secrets Encryption
# ============================================================================

resource "aws_kms_key" "secrets" {
  description             = "KMS key for erlmcp secrets encryption"
  deletion_window_in_days = 30
  enable_key_rotation     = true

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid    = "AllowRoot"
        Effect = "Allow"
        Principal = {
          AWS = "arn:aws:iam::${data.aws_caller_identity.current.account_id}:root"
        }
        Action   = "kms:*"
        Resource = "*"
      },
      {
        Sid    = "AllowECSTaskExecution"
        Effect = "Allow"
        Principal = {
          AWS = local.deploy_ecs ? aws_iam_role.ecs_task_execution[0].arn : "arn:aws:iam::${data.aws_caller_identity.current.account_id}:root"
        }
        Action = [
          "kms:Decrypt",
          "kms:DescribeKey"
        ]
        Resource = "*"
      },
      {
        Sid    = "AllowEC2Instances"
        Effect = "Allow"
        Principal = {
          AWS = local.deploy_ec2 ? aws_iam_role.ec2_instance[0].arn : "arn:aws:iam::${data.aws_caller_identity.current.account_id}:root"
        }
        Action = [
          "kms:Decrypt",
          "kms:DescribeKey"
        ]
        Resource = "*"
      }
    ]
  })

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-secrets-key"
  })
}

resource "aws_kms_alias" "secrets" {
  name          = "alias/${local.name_prefix}-secrets"
  target_key_id = aws_kms_key.secrets.key_id
}

# ============================================================================
# SSM Parameters
# ============================================================================

# Erlang Distribution Cookie
resource "aws_ssm_parameter" "erlang_cookie" {
  name        = "/erlmcp/${var.environment}/erlang_cookie"
  description = "Erlang distribution cookie for cluster authentication"
  type        = "SecureString"
  value       = var.erlang_cookie != "" ? var.erlang_cookie : random_password.erlang_cookie.result
  key_id      = aws_kms_key.secrets.key_id

  tags = local.common_tags

  lifecycle {
    ignore_changes = [value]
  }
}

resource "random_password" "erlang_cookie" {
  length  = 32
  special = false
}

# Database Password
resource "aws_ssm_parameter" "db_password" {
  name        = "/erlmcp/${var.environment}/db_password"
  description = "Database password for erlmcp"
  type        = "SecureString"
  value       = random_password.db_password.result
  key_id      = aws_kms_key.secrets.key_id

  tags = local.common_tags

  lifecycle {
    ignore_changes = [value]
  }
}

resource "random_password" "db_password" {
  length           = 32
  special          = true
  override_special = "!#$%&*()-_=+[]{}:?"
}

# API Secret Key
resource "aws_ssm_parameter" "api_secret" {
  name        = "/erlmcp/${var.environment}/api_secret"
  description = "API secret key for JWT signing"
  type        = "SecureString"
  value       = random_password.api_secret.result
  key_id      = aws_kms_key.secrets.key_id

  tags = local.common_tags

  lifecycle {
    ignore_changes = [value]
  }
}

resource "random_password" "api_secret" {
  length  = 64
  special = false
}

# ============================================================================
# Secrets Manager (for rotation-capable secrets)
# ============================================================================

resource "aws_secretsmanager_secret" "db_credentials" {
  name        = "erlmcp/${var.environment}/db-credentials"
  description = "Database credentials for erlmcp with automatic rotation"
  kms_key_id  = aws_kms_key.secrets.key_id

  tags = local.common_tags
}

resource "aws_secretsmanager_secret_version" "db_credentials" {
  secret_id = aws_secretsmanager_secret.db_credentials.id
  secret_string = jsonencode({
    username = "erlmcp"
    password = random_password.db_password.result
    engine   = "postgres"
    host     = "localhost"
    port     = 5432
    dbname   = "erlmcp"
  })

  lifecycle {
    ignore_changes = [secret_string]
  }
}

# ============================================================================
# Vault Integration (Optional - for HashiCorp Vault users)
# ============================================================================

# Vault Agent configuration template
resource "local_file" "vault_agent_config" {
  count    = var.enable_vault ? 1 : 0
  filename = "${path.module}/files/vault-agent.hcl"
  content  = <<-EOF
    # Vault Agent Configuration for erlmcp
    # This agent auto-authenticates and renders secrets to files

    pid_file = "/var/run/vault-agent.pid"

    auto_auth {
      method "aws" {
        mount_path = "auth/aws"
        config = {
          type = "iam"
          role = "erlmcp-${var.environment}"
        }
      }

      sink "file" {
        config = {
          path = "/opt/erlmcp/.vault-token"
          mode = 0600
        }
      }
    }

    cache {
      use_auto_auth_token = true
    }

    listener "tcp" {
      address     = "127.0.0.1:8200"
      tls_disable = true
    }

    template {
      source      = "/etc/vault/templates/erlmcp-secrets.ctmpl"
      destination = "/opt/erlmcp/etc/secrets.conf"
      perms       = 0600
      command     = "systemctl reload erlmcp || true"
    }

    template {
      source      = "/etc/vault/templates/db-credentials.ctmpl"
      destination = "/opt/erlmcp/etc/db.conf"
      perms       = 0600
      command     = "systemctl reload erlmcp || true"
    }
  EOF
}

# Vault secrets template
resource "local_file" "vault_secrets_template" {
  count    = var.enable_vault ? 1 : 0
  filename = "${path.module}/files/erlmcp-secrets.ctmpl"
  content  = <<-EOF
    # erlmcp Secrets Configuration
    # Generated by Vault Agent

    {{ with secret "secret/data/erlmcp/${var.environment}" }}
    ERLANG_COOKIE={{ .Data.data.erlang_cookie }}
    API_SECRET={{ .Data.data.api_secret }}
    {{ end }}
  EOF
}

resource "local_file" "vault_db_template" {
  count    = var.enable_vault ? 1 : 0
  filename = "${path.module}/files/db-credentials.ctmpl"
  content  = <<-EOF
    # Database Credentials
    # Generated by Vault Agent

    {{ with secret "database/creds/erlmcp-${var.environment}" }}
    DB_USERNAME={{ .Data.username }}
    DB_PASSWORD={{ .Data.password }}
    {{ end }}
  EOF
}

# ============================================================================
# Variables for Secrets
# ============================================================================

variable "enable_vault" {
  description = "Enable HashiCorp Vault integration"
  type        = bool
  default     = false
}

# ============================================================================
# Outputs
# ============================================================================

output "kms_key_arn" {
  description = "ARN of the KMS key for secrets encryption"
  value       = aws_kms_key.secrets.arn
}

output "erlang_cookie_parameter_arn" {
  description = "ARN of the Erlang cookie SSM parameter"
  value       = aws_ssm_parameter.erlang_cookie.arn
}

output "db_password_parameter_arn" {
  description = "ARN of the database password SSM parameter"
  value       = aws_ssm_parameter.db_password.arn
}

output "db_credentials_secret_arn" {
  description = "ARN of the database credentials secret"
  value       = aws_secretsmanager_secret.db_credentials.arn
}
