# ============================================================================
# Template Rendering for erlmcp Erlang Configuration
# ============================================================================
# Renders vm.args and sys.config templates with environment-specific values
#
# DOCKER-ONLY CONSTITUTION: Templates are validated during Docker build.
# ============================================================================

# ============================================================================
# VM Args Template Variables
# ============================================================================

locals {
  # VM args configuration
  vm_args_config = {
    environment            = var.environment
    node_name              = "erlmcp@$${PRIVATE_IP}"  # Escaped for cloud-init
    erlang_cookie          = "$${ERLANG_COOKIE}"      # Fetched from SSM at boot

    # Scheduler configuration
    schedulers                    = var.ecs_cpu / 256  # 1 scheduler per 256 CPU units
    dirty_cpu_schedulers_online   = max(1, var.ecs_cpu / 512)
    dirty_io_schedulers           = 10

    # Process limits
    max_processes  = 256000
    max_ports      = 65536
    max_ets_tables = 8192

    # Memory configuration
    max_carrier_size    = 30
    binary_vheap_size   = 46422
    carrier_pool_size   = 256

    # Async threads
    async_threads = 10

    # Distribution ports
    dist_port_min         = 9100
    dist_port_max         = 9200
    dist_buffer_busy_limit = 16777216  # 16MB

    # Heart configuration
    heart_enabled = var.environment == "production" ? "-heart" : "## -heart (disabled)"
    heart_timeout = 60
    heart_command = "/opt/erlmcp/bin/erlmcp restart"

    # Embedded mode
    embedded_mode = var.environment == "production" ? "-mode embedded" : "## -mode embedded (disabled)"

    # Crash dump
    crash_dump_path  = "/var/log/erlmcp/erl_crash.dump"
    crash_dump_bytes = 1073741824  # 1GB

    # Logging
    log_level = var.environment == "production" ? "info" : "debug"

    # AWS
    aws_region  = var.aws_region
    instance_id = "$${INSTANCE_ID}"  # Populated at boot

    # Extra flags
    extra_vm_args = var.extra_vm_args
  }

  # Sys config configuration
  sys_config_config = {
    environment = var.environment
    node_role   = "server"

    # STDIO transport
    stdio_enabled     = true
    stdio_buffer_size = 65536

    # TCP transport
    tcp_enabled         = true
    tcp_port            = var.app_port
    tcp_acceptors       = 100
    tcp_max_connections = 10000
    tcp_backlog         = 1024

    # HTTP transport
    http_enabled         = true
    http_port            = var.app_port
    http_acceptors       = 100
    http_max_connections = 10000
    http_idle_timeout    = 60000
    http_request_timeout = 30000

    # WebSocket
    ws_enabled       = true
    ws_ping_interval = 30000

    # SSE
    sse_enabled            = true
    sse_heartbeat_interval = 15000

    # Protocol
    tools_enabled     = true
    resources_enabled = true
    prompts_enabled   = true
    sampling_enabled  = true
    logging_enabled   = true
    request_timeout   = 30000
    max_request_size  = 10485760  # 10MB

    # Registry
    registry_cleanup_interval = 60000

    # Sessions
    max_sessions              = 10000
    session_timeout           = 3600000
    session_heartbeat_interval = 30000

    # Backpressure
    max_queue_size = 10000
    high_watermark = 8000
    low_watermark  = 2000

    # Health
    health_port = var.health_port

    # Metrics
    metrics_enabled = true
    metrics_port    = var.metrics_port

    # Logging
    log_level     = var.environment == "production" ? "info" : "debug"
    log_dir       = "/var/log/erlmcp"
    log_max_bytes = 104857600  # 100MB
    log_max_files = 10

    # Distribution
    dist_port_min = 9100
    dist_port_max = 9200
    net_ticktime  = 60

    # gproc
    gproc_dist_mode = var.enable_clustering ? "all" : "undefined"

    # OpenTelemetry
    otel_exporter = "otlp"
    otel_endpoint = "http://localhost:4317"
    app_version   = var.erlmcp_version
    aws_region    = var.aws_region

    # Database
    db_enabled   = var.enable_rds
    db_pool_size = 10
    db_host      = var.enable_rds ? aws_db_instance.main[0].address : "localhost"
    db_port      = 5432
    db_name      = "erlmcp"
    db_username  = "erlmcp"

    # Cache
    cache_enabled   = var.enable_elasticache
    cache_host      = var.enable_elasticache ? aws_elasticache_cluster.main[0].cache_nodes[0].address : "localhost"
    cache_port      = 6379
    cache_pool_size = 10
    cache_ttl       = 3600
  }
}

# ============================================================================
# Template Data Sources
# ============================================================================

data "template_file" "vm_args" {
  template = file("${path.module}/templates/vm.args.tpl")
  vars     = local.vm_args_config
}

data "template_file" "sys_config" {
  template = file("${path.module}/templates/sys.config.tpl")
  vars     = local.sys_config_config
}

# ============================================================================
# Rendered Configuration Files (for ECS)
# ============================================================================

resource "local_file" "vm_args" {
  count    = var.output_config_files ? 1 : 0
  filename = "${path.module}/output/vm.args"
  content  = data.template_file.vm_args.rendered
}

resource "local_file" "sys_config" {
  count    = var.output_config_files ? 1 : 0
  filename = "${path.module}/output/sys.config"
  content  = data.template_file.sys_config.rendered
}

# ============================================================================
# S3 Configuration Bucket (for ECS to fetch at runtime)
# ============================================================================

resource "aws_s3_object" "vm_args" {
  count  = local.deploy_ecs ? 1 : 0
  bucket = aws_s3_bucket.config[0].id
  key    = "config/${var.environment}/vm.args"
  content = data.template_file.vm_args.rendered

  server_side_encryption = "aws:kms"
  kms_key_id             = aws_kms_key.secrets.arn

  tags = local.common_tags
}

resource "aws_s3_object" "sys_config" {
  count  = local.deploy_ecs ? 1 : 0
  bucket = aws_s3_bucket.config[0].id
  key    = "config/${var.environment}/sys.config"
  content = data.template_file.sys_config.rendered

  server_side_encryption = "aws:kms"
  kms_key_id             = aws_kms_key.secrets.arn

  tags = local.common_tags
}

resource "aws_s3_bucket" "config" {
  count  = local.deploy_ecs ? 1 : 0
  bucket = "${local.name_prefix}-config-${data.aws_caller_identity.current.account_id}"

  tags = merge(local.common_tags, {
    Name = "${local.name_prefix}-config"
  })
}

resource "aws_s3_bucket_versioning" "config" {
  count  = local.deploy_ecs ? 1 : 0
  bucket = aws_s3_bucket.config[0].id

  versioning_configuration {
    status = "Enabled"
  }
}

resource "aws_s3_bucket_server_side_encryption_configuration" "config" {
  count  = local.deploy_ecs ? 1 : 0
  bucket = aws_s3_bucket.config[0].id

  rule {
    apply_server_side_encryption_by_default {
      sse_algorithm     = "aws:kms"
      kms_master_key_id = aws_kms_key.secrets.arn
    }
  }
}

resource "aws_s3_bucket_public_access_block" "config" {
  count  = local.deploy_ecs ? 1 : 0
  bucket = aws_s3_bucket.config[0].id

  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

# ============================================================================
# Variables for Templates
# ============================================================================

variable "extra_vm_args" {
  description = "Additional vm.args flags"
  type        = string
  default     = ""
}

variable "output_config_files" {
  description = "Write rendered config files to output directory"
  type        = bool
  default     = false
}

variable "enable_clustering" {
  description = "Enable Erlang distribution for clustering"
  type        = bool
  default     = true
}

variable "enable_rds" {
  description = "Enable RDS PostgreSQL"
  type        = bool
  default     = false
}

variable "enable_elasticache" {
  description = "Enable ElastiCache Redis"
  type        = bool
  default     = false
}

variable "health_port" {
  description = "Health check port"
  type        = number
  default     = 8080
}

# ============================================================================
# Outputs
# ============================================================================

output "vm_args_s3_path" {
  description = "S3 path to vm.args configuration"
  value       = local.deploy_ecs ? "s3://${aws_s3_bucket.config[0].id}/config/${var.environment}/vm.args" : null
}

output "sys_config_s3_path" {
  description = "S3 path to sys.config configuration"
  value       = local.deploy_ecs ? "s3://${aws_s3_bucket.config[0].id}/config/${var.environment}/sys.config" : null
}
