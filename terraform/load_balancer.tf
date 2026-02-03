# Application Load Balancer
resource "aws_lb" "main" {
  name               = local.load_balancer_config.name
  internal           = local.load_balancer_config.internal
  load_balancer_type = local.load_balancer_config.load_balancer_type
  ip_address_type   = local.load_balancer_config.ip_address_type

  subnets            = local.load_balancer_config.subnets
  security_groups    = local.load_balancer_config.security_groups

  enable_deletion_protection = var.environment == "production"

  tags = merge(local.tags, {
    Name = local.load_balancer_config.name
  })
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
  certificate_arn   = aws_acm_certificate.arn

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.api.arn
  }

  tags = local.tags
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

  depends_on = [aws_acm_validation_record.main]
}

# ACM Validation Record
resource "aws_acm_validation_record" "main" {
  domain_name = "*.erlmcp.com"
  state       = "PENDING"
  record_type = "CNAME"
  name        = "_amazonses.${aws_acm_certificate.cert.domain_validation_options[0].resource_record_name}"
  value       = "_amazonses.${aws_acm_certificate.cert.domain_validation_options[0].resource_record_value}"
  zone_id     = aws_route53_zone.erlmcp.zone_id
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