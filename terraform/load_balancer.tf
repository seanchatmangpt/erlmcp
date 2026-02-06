# Application Load Balancer with enhanced security
resource "aws_lb" "main" {
  name               = local.load_balancer_config.name
  internal           = local.load_balancer_config.internal
  load_balancer_type = local.load_balancer_config.load_balancer_type
  ip_address_type    = local.load_balancer_config.ip_address_type

  subnets         = aws_subnet.public[*].id
  security_groups = [aws_security_group.alb.id]

  enable_deletion_protection = var.environment == "production"
  enable_http2               = true
  enable_cross_zone_load_balancing = true
  drop_invalid_header_fields = true

  access_logs {
    bucket  = aws_s3_bucket.lb_logs.id
    prefix  = "alb-logs"
    enabled = true
  }

  tags = merge(local.tags, {
    Name = local.load_balancer_config.name
  })
}

# S3 bucket for ALB access logs
resource "aws_s3_bucket" "lb_logs" {
  bucket = "erlmcp-${var.environment}-alb-logs-${random_string.suffix.result}"

  tags = local.tags
}

resource "aws_s3_bucket_versioning" "lb_logs" {
  bucket = aws_s3_bucket.lb_logs.id
  versioning_configuration {
    status = "Enabled"
  }
}

resource "aws_s3_bucket_server_side_encryption_configuration" "lb_logs" {
  bucket = aws_s3_bucket.lb_logs.id

  rule {
    apply_server_side_encryption_by_default {
      sse_algorithm     = "aws:kms"
      kms_master_key_id = aws_kms_key.s3.arn
    }
  }
}

resource "aws_s3_bucket_public_access_block" "lb_logs" {
  bucket                  = aws_s3_bucket.lb_logs.id
  block_public_acls       = true
  block_public_policy     = true
  ignore_public_acls      = true
  restrict_public_buckets = true
}

resource "aws_s3_bucket_lifecycle_configuration" "lb_logs" {
  bucket = aws_s3_bucket.lb_logs.id

  rule {
    id     = "delete-old-logs"
    status = "Enabled"

    expiration {
      days = 90
    }

    noncurrent_version_expiration {
      noncurrent_days = 30
    }
  }
}

# S3 bucket policy for ALB logs
resource "aws_s3_bucket_policy" "lb_logs" {
  bucket = aws_s3_bucket.lb_logs.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid    = "AWSLogDeliveryWrite"
        Effect = "Allow"
        Principal = {
          Service = "elasticloadbalancing.amazonaws.com"
        }
        Action   = "s3:PutObject"
        Resource = "${aws_s3_bucket.lb_logs.arn}/*"
      },
      {
        Sid    = "AWSLogDeliveryAclCheck"
        Effect = "Allow"
        Principal = {
          Service = "elasticloadbalancing.amazonaws.com"
        }
        Action   = "s3:GetBucketAcl"
        Resource = aws_s3_bucket.lb_logs.arn
      }
    ]
  })
}

# Random suffix for unique naming
resource "random_string" "suffix" {
  length  = 8
  special = false
  upper   = false
}

# ALB Target Group
resource "aws_lb_target_group" "main" {
  name        = "erlmcp-${var.environment}-tg"
  port        = 8080
  protocol    = "HTTP"
  vpc_id      = aws_vpc.main.id
  target_type = "ip"

  health_check {
    enabled             = true
    healthy_threshold   = 2
    interval            = 30
    matcher            = "200"
    path               = var.health_check_path
    port               = "traffic-port"
    protocol           = "HTTP"
    timeout            = 5
    unhealthy_threshold = 2
  }

  tags = local.tags
}

# ALB Target Group for API Gateway
resource "aws_lb_target_group" "api" {
  name        = "erlmcp-${var.environment}-api-tg"
  port        = 443
  protocol    = "HTTPS"
  vpc_id      = aws_vpc.main.id
  target_type = "ip"

  health_check {
    enabled             = true
    healthy_threshold   = 2
    interval            = 30
    matcher            = "200"
    path               = var.health_check_path
    port               = "traffic-port"
    protocol           = "HTTP"
    timeout            = 5
    unhealthy_threshold = 2
  }

  tags = local.tags
}

# ALB Listener
resource "aws_lb_listener" "http" {
  load_balancer_arn = aws_lb.main.arn
  port              = 80
  protocol          = "HTTP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.main.arn
  }

  tags = local.tags
}

# ALB Listener for HTTPS
resource "aws_lb_listener" "https" {
  load_balancer_arn = aws_lb.main.arn
  port              = 443
  protocol          = "HTTPS"
  ssl_policy        = "ELBSecurityPolicy-TLS13-1-2-2021-06"
  certificate_arn   = aws_acm_certificate.cert.arn

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.api.arn
  }

  tags = local.tags

  depends_on = [aws_acm_certificate_validation.cert]
}

# ACM Certificate
resource "aws_acm_certificate" "cert" {
  domain_name       = "*.erlmcp.com"
  validation_method = "DNS"

  subject_alternative_names = [
    "erlmcp.com",
    "api.erlmcp.com",
    "staging.erlmcp.com",
    "dashboard.erlmcp.com"
  ]

  tags = local.tags

  lifecycle {
    create_before_destroy = true
  }
}

# DNS Record
resource "aws_route53_record" "api" {
  zone_id = aws_route53_zone.erlmcp.zone_id
  name    = "api.erlmcp.com"
  type    = "A"

  alias {
    name                   = aws_lb.main.dns_name
    zone_id                = aws_lb.main.zone_id
    evaluate_target_health = true
  }

  depends_on = [aws_acm_certificate_validation.cert]
}

# ACM Certificate Validation
resource "aws_route53_record" "cert_validation" {
  for_each = {
    for dvo in aws_acm_certificate.cert.domain_validation_options : dvo.domain_name => {
      name   = dvo.resource_record_name
      record = dvo.resource_record_value
      type   = dvo.resource_record_type
    }
  }

  allow_overwrite = true
  name            = each.value.name
  records         = [each.value.record]
  ttl             = 60
  type            = each.value.type
  zone_id         = aws_route53_zone.erlmcp.zone_id
}

resource "aws_acm_certificate_validation" "cert" {
  certificate_arn         = aws_acm_certificate.cert.arn
  validation_record_fqdns = [for record in aws_route53_record.cert_validation : record.fqdn]
}

# Route53 Zone
resource "aws_route53_zone" "erlmcp" {
  name = "erlmcp.com"

  tags = local.tags
}

# Output Load Balancer DNS
output "alb_dns_name" {
  value = aws_lb.main.dns_name
}

# Output Target Group ARN
output "target_group_arn" {
  value = aws_lb_target_group.main.arn
}