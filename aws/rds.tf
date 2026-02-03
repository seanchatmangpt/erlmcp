# ==============================================================================
# AWS RDS PostgreSQL Configuration
# ==============================================================================
# Production-grade RDS PostgreSQL with Multi-AZ, encryption, and
# automated backups.
# ==============================================================================

# ==============================================================================
# RDS Subnet Group
# ==============================================================================

# Already defined in network.tf as aws_db_subnet_group.main

# ==============================================================================
# RDS Security Group
# ==============================================================================

resource "aws_security_group" "rds" {
  count = var.enable_rds ? 1 : 0

  name        = "${local.name_prefix}-rds-sg"
  description = "Security group for RDS PostgreSQL"
  vpc_id      = aws_vpc.main.id

  ingress {
    from_port       = 5432
    to_port         = 5432
    protocol        = "tcp"
    security_groups = [aws_security_group.ecs_tasks.id]
    description     = "PostgreSQL from ECS tasks"
  }

  # Allow from bastion if enabled
  dynamic "ingress" {
    for_each = var.bastion_security_group_id != "" ? [1] : []
    content {
      from_port       = 5432
      to_port         = 5432
      protocol        = "tcp"
      security_groups = [var.bastion_security_group_id]
      description     = "PostgreSQL from bastion"
    }
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
    description = "Allow all outbound"
  }

  tags = {
    Name = "${local.name_prefix}-rds-sg"
  }
}

# ==============================================================================
# RDS Parameter Group (BEAM-optimized)
# ==============================================================================

resource "aws_db_parameter_group" "main" {
  count = var.enable_rds ? 1 : 0

  name   = "${local.name_prefix}-pg16"
  family = "postgres16"

  # Connection settings optimized for Erlang connection pooling
  parameter {
    name  = "max_connections"
    value = var.db_max_connections
  }

  parameter {
    name  = "shared_buffers"
    value = "{DBInstanceClassMemory/4}"
  }

  parameter {
    name  = "effective_cache_size"
    value = "{DBInstanceClassMemory*3/4}"
  }

  # Statement timeout (30s for BEAM apps)
  parameter {
    name  = "statement_timeout"
    value = "30000"
  }

  # Idle transaction timeout
  parameter {
    name  = "idle_in_transaction_session_timeout"
    value = "60000"
  }

  # Logging for debugging
  parameter {
    name  = "log_min_duration_statement"
    value = "1000" # Log queries taking > 1s
  }

  parameter {
    name  = "log_connections"
    value = "1"
  }

  parameter {
    name  = "log_disconnections"
    value = "1"
  }

  # Performance Insights
  parameter {
    name  = "pg_stat_statements.track"
    value = "all"
  }

  # SSL enforcement
  parameter {
    name  = "rds.force_ssl"
    value = "1"
  }

  tags = {
    Name = "${local.name_prefix}-parameter-group"
  }
}

# ==============================================================================
# RDS Instance (Primary)
# ==============================================================================

resource "aws_db_instance" "main" {
  count = var.enable_rds ? 1 : 0

  identifier = "${local.name_prefix}-postgres"

  # Engine configuration
  engine                      = "postgres"
  engine_version              = var.db_engine_version
  instance_class              = var.db_instance_class
  allocated_storage           = var.db_allocated_storage
  max_allocated_storage       = var.db_max_allocated_storage
  storage_type                = "gp3"
  storage_encrypted           = true
  kms_key_id                  = aws_kms_key.main.arn
  iops                        = var.db_iops
  storage_throughput          = var.db_storage_throughput

  # Database configuration
  db_name  = var.db_name
  username = var.db_username
  password = local.db_password

  # Networking
  db_subnet_group_name   = aws_db_subnet_group.main.name
  vpc_security_group_ids = [aws_security_group.rds[0].id]
  publicly_accessible    = false
  port                   = 5432

  # High availability
  multi_az = var.db_multi_az

  # Parameter and option groups
  parameter_group_name = aws_db_parameter_group.main[0].name

  # Backup and maintenance
  backup_retention_period   = var.db_backup_retention_period
  backup_window             = var.db_backup_window
  maintenance_window        = var.db_maintenance_window
  copy_tags_to_snapshot     = true
  delete_automated_backups  = false
  skip_final_snapshot       = var.environment != "production"
  final_snapshot_identifier = var.environment == "production" ? "${local.name_prefix}-final-snapshot" : null

  # Performance Insights
  performance_insights_enabled          = true
  performance_insights_retention_period = var.environment == "production" ? 731 : 7
  performance_insights_kms_key_id       = aws_kms_key.main.arn

  # Enhanced Monitoring
  monitoring_interval = var.db_monitoring_interval
  monitoring_role_arn = var.db_monitoring_interval > 0 ? aws_iam_role.rds_monitoring[0].arn : null

  # IAM authentication
  iam_database_authentication_enabled = true

  # Auto minor version upgrade
  auto_minor_version_upgrade = true
  apply_immediately          = var.environment != "production"

  # Deletion protection
  deletion_protection = var.environment == "production"

  # CloudWatch logs
  enabled_cloudwatch_logs_exports = ["postgresql", "upgrade"]

  tags = {
    Name = "${local.name_prefix}-postgres"
  }

  lifecycle {
    prevent_destroy = false
    ignore_changes  = [password]
  }
}

# ==============================================================================
# RDS Read Replicas
# ==============================================================================

resource "aws_db_instance" "replica" {
  count = var.enable_rds && var.db_read_replica_count > 0 ? var.db_read_replica_count : 0

  identifier = "${local.name_prefix}-postgres-replica-${count.index + 1}"

  # Replica configuration
  replicate_source_db = aws_db_instance.main[0].identifier
  instance_class      = var.db_replica_instance_class != "" ? var.db_replica_instance_class : var.db_instance_class

  # Storage (inherited from source)
  storage_encrypted = true
  kms_key_id        = aws_kms_key.main.arn

  # Networking
  vpc_security_group_ids = [aws_security_group.rds[0].id]
  publicly_accessible    = false

  # Parameter group
  parameter_group_name = aws_db_parameter_group.main[0].name

  # No backups for replicas
  backup_retention_period = 0

  # Performance Insights
  performance_insights_enabled          = true
  performance_insights_retention_period = 7
  performance_insights_kms_key_id       = aws_kms_key.main.arn

  # Enhanced Monitoring
  monitoring_interval = var.db_monitoring_interval
  monitoring_role_arn = var.db_monitoring_interval > 0 ? aws_iam_role.rds_monitoring[0].arn : null

  # Auto minor version upgrade
  auto_minor_version_upgrade = true

  # CloudWatch logs
  enabled_cloudwatch_logs_exports = ["postgresql"]

  tags = {
    Name = "${local.name_prefix}-postgres-replica-${count.index + 1}"
  }
}

# ==============================================================================
# RDS Enhanced Monitoring Role
# ==============================================================================

resource "aws_iam_role" "rds_monitoring" {
  count = var.enable_rds && var.db_monitoring_interval > 0 ? 1 : 0

  name = "${local.name_prefix}-rds-monitoring"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "monitoring.rds.amazonaws.com"
        }
      }
    ]
  })

  tags = {
    Name = "${local.name_prefix}-rds-monitoring"
  }
}

resource "aws_iam_role_policy_attachment" "rds_monitoring" {
  count = var.enable_rds && var.db_monitoring_interval > 0 ? 1 : 0

  role       = aws_iam_role.rds_monitoring[0].name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonRDSEnhancedMonitoringRole"
}

# ==============================================================================
# RDS Proxy (for connection pooling)
# ==============================================================================

resource "aws_db_proxy" "main" {
  count = var.enable_rds && var.enable_rds_proxy ? 1 : 0

  name                   = "${local.name_prefix}-proxy"
  debug_logging          = var.environment != "production"
  engine_family          = "POSTGRESQL"
  idle_client_timeout    = 1800
  require_tls            = true
  role_arn               = aws_iam_role.rds_proxy[0].arn
  vpc_security_group_ids = [aws_security_group.rds[0].id]
  vpc_subnet_ids         = aws_subnet.private[*].id

  auth {
    auth_scheme               = "SECRETS"
    iam_auth                  = "DISABLED"
    secret_arn                = aws_secretsmanager_secret.database_credentials[0].arn
    client_password_auth_type = "POSTGRES_SCRAM_SHA_256"
  }

  tags = {
    Name = "${local.name_prefix}-proxy"
  }

  depends_on = [
    aws_secretsmanager_secret_version.database_credentials
  ]
}

resource "aws_db_proxy_default_target_group" "main" {
  count = var.enable_rds && var.enable_rds_proxy ? 1 : 0

  db_proxy_name = aws_db_proxy.main[0].name

  connection_pool_config {
    connection_borrow_timeout    = 120
    max_connections_percent      = 100
    max_idle_connections_percent = 50
  }
}

resource "aws_db_proxy_target" "main" {
  count = var.enable_rds && var.enable_rds_proxy ? 1 : 0

  db_proxy_name          = aws_db_proxy.main[0].name
  target_group_name      = aws_db_proxy_default_target_group.main[0].name
  db_instance_identifier = aws_db_instance.main[0].identifier
}

# RDS Proxy IAM role
resource "aws_iam_role" "rds_proxy" {
  count = var.enable_rds && var.enable_rds_proxy ? 1 : 0

  name = "${local.name_prefix}-rds-proxy"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "rds.amazonaws.com"
        }
      }
    ]
  })

  tags = {
    Name = "${local.name_prefix}-rds-proxy"
  }
}

resource "aws_iam_role_policy" "rds_proxy" {
  count = var.enable_rds && var.enable_rds_proxy ? 1 : 0

  name = "${local.name_prefix}-rds-proxy-policy"
  role = aws_iam_role.rds_proxy[0].id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = [
          "secretsmanager:GetSecretValue"
        ]
        Effect   = "Allow"
        Resource = aws_secretsmanager_secret.database_credentials[0].arn
      },
      {
        Action = [
          "kms:Decrypt"
        ]
        Effect   = "Allow"
        Resource = aws_kms_key.main.arn
        Condition = {
          StringEquals = {
            "kms:ViaService" = "secretsmanager.${local.region}.amazonaws.com"
          }
        }
      }
    ]
  })
}

# Database credentials secret for RDS Proxy
resource "aws_secretsmanager_secret" "database_credentials" {
  count = var.enable_rds && var.enable_rds_proxy ? 1 : 0

  name       = "${local.name_prefix}/database-credentials"
  kms_key_id = aws_kms_key.main.arn

  tags = {
    Name = "${local.name_prefix}-database-credentials"
  }
}

resource "aws_secretsmanager_secret_version" "database_credentials" {
  count = var.enable_rds && var.enable_rds_proxy ? 1 : 0

  secret_id = aws_secretsmanager_secret.database_credentials[0].id
  secret_string = jsonencode({
    username = var.db_username
    password = local.db_password
  })
}
