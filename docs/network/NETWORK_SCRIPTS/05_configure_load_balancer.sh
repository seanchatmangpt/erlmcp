#!/bin/bash
# erlmcp v3 Load Balancer Configuration Script
set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
VPC_ID="vpc-1234567890"
REGION="us-east-1"
PROFILE="erlmcp-prod"
LB_NAME="erlmcp-alb"
SG_ID="sg-1234567890"
SUBNET_ID_SUB_A="subnet-1234567890"
SUBNET_ID_SUB_B="subnet-1234567891"
SUBNET_ID_SUB_C="subnet-1234567892"
TARGET_GROUP_NAME="erlmcp-target-group"
HEALTH_CHECK_PATH="/health"
HEALTH_CHECK_PORT="8080"
SSL_CERTIFICATE_ARN="arn:aws:acm:us-east-1:123456789012:certificate/12345678-1234-1234-1234-123456789012"
LISTENER_PORT_HTTP=80
LISTENER_PORT_HTTPS=443

log() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Validate configuration
validate_config() {
    log "Validating load balancer configuration..."

    # Check required variables
    if [ -z "$VPC_ID" ] || [ -z "$REGION" ] || [ -z "$PROFILE" ]; then
        error "Required configuration variables not set"
        exit 1
    fi

    # Check if VPC exists
    if ! aws ec2 describe-vpcs --vpc-ids $VPC_ID --query 'Vpcs[0].VpcId' --output text > /dev/null; then
        error "VPC $VPC_ID not found"
        exit 1
    fi

    # Check if security group exists
    if ! aws ec2 describe-security-groups --group-ids $SG_ID --query 'SecurityGroups[0].GroupId' --output text > /dev/null; then
        error "Security group $SG_ID not found"
        exit 1
    fi

    # Check if certificate exists
    if ! aws acm describe-certificate --certificate-arn $SSL_CERTIFICATE_ARN --query 'Certificate.CertificateArn' --output text > /dev/null; then
        error "SSL certificate not found"
        exit 1
    fi

    log "Configuration validated"
}

# Create application load balancer
create_load_balancer() {
    log "Creating application load balancer..."

    # Create load balancer
    LB_ARN=$(aws elbv2 create-load-balancer \
        --name $LB_NAME \
        --type application \
        --scheme internet-facing \
        --ip-address-type ipv4 \
        --security-group-ids $SG_ID \
        --subnet-ids $SUBNET_ID_SUB_A $SUBNET_ID_SUB_B $SUBNET_ID_SUB_C \
        --region $REGION \
        --query 'LoadBalancers[0].LoadBalancerArn' \
        --output text)

    if [ -z "$LB_ARN" ]; then
        error "Failed to create load balancer"
        exit 1
    fi

    # Wait for load balancer to be active
    aws elbv2 wait load-balancer-available \
        --load-balancer-arns $LB_ARN \
        --region $REGION

    # Get load balancer DNS name
    LB_DNS=$(aws elbv2 describe-load-balancers \
        --load-balancer-arns $LB_ARN \
        --query 'LoadBalancers[0].DNSName' \
        --output text)

    if [ -z "$LB_DNS" ]; then
        error "Failed to get load balancer DNS name"
        exit 1
    fi

    echo "Load balancer created with DNS: $LB_DNS"
    export LB_DNS
}

# Create target groups
create_target_groups() {
    log "Creating target groups..."

    # Create erlmcp application target group
    TG_ARN=$(aws elbv2 create-target-group \
        --name $TARGET_GROUP_NAME \
        --protocol HTTP \
        --port $HEALTH_CHECK_PORT \
        --vpc-id $VPC_ID \
        --health-check-path $HEALTH_CHECK_PATH \
        --health-check-port $HEALTH_CHECK_PORT \
        --health-check-interval 30 \
        --health-check-timeout 5 \
        --healthy-threshold 3 \
        --unhealthy-threshold 3 \
        --target-type instance \
        --region $REGION \
        --query 'TargetGroups[0].TargetGroupArn' \
        --output text)

    if [ -z "$TG_ARN" ]; then
        error "Failed to create target group"
        exit 1
    fi

    # Create target group for WebSocket
    WS_TG_NAME="${TARGET_GROUP_NAME}-websocket"
    WS_TG_ARN=$(aws elbv2 create-target-group \
        --name $WS_TG_NAME \
        --protocol HTTP \
        --port $HEALTH_CHECK_PORT \
        --vpc-id $VPC_ID \
        --health-check-path $HEALTH_CHECK_PATH \
        --health-check-port $HEALTH_CHECK_PORT \
        --health-check-interval 30 \
        --health-check-timeout 5 \
        --healthy-threshold 3 \
        --unhealthy-threshold 3 \
        --target-type instance \
        --region $REGION \
        --query 'TargetGroups[0].TargetGroupArn' \
        --output text)

    if [ -z "$WS_TG_ARN" ]; then
        error "Failed to create WebSocket target group"
        exit 1
    fi

    # Create target group for HTTPS
    HTTPS_TG_NAME="${TARGET_GROUP_NAME}-https"
    HTTPS_TG_ARN=$(aws elbv2 create-target-group \
        --name $HTTPS_TG_NAME \
        --protocol HTTPS \
        --port $HEALTH_CHECK_PORT \
        --vpc-id $VPC_ID \
        --health-check-path $HEALTH_CHECK_PATH \
        --health-check-port $HEALTH_CHECK_PORT \
        --health-check-interval 30 \
        --health-check-timeout 5 \
        --healthy-threshold 3 \
        --unhealthy-threshold 3 \
        --target-type instance \
        --region $REGION \
        --query 'TargetGroups[0].TargetGroupArn' \
        --output text)

    if [ -z "$HTTPS_TG_ARN" ]; then
        error "Failed to create HTTPS target group"
        exit 1
    fi

    export TG_ARN
    export WS_TG_ARN
    export HTTPS_TG_ARN
}

# Create listeners
create_listeners() {
    log "Creating listeners..."

    # Create HTTP listener (redirect to HTTPS)
    HTTP_LISTENER_ARN=$(aws elbv2 create-listener \
        --load-balancer-arn $LB_ARN \
        --protocol HTTP \
        --port $LISTENER_PORT_HTTP \
        --default-actions Type=redirect,RedirectConfig={
            Protocol=HTTPS,
            Port=443,
            StatusCode=HTTP_301
        } \
        --region $REGION \
        --query 'Listeners[0].ListenerArn' \
        --output text)

    if [ -z "$HTTP_LISTENER_ARN" ]; then
        error "Failed to create HTTP listener"
        exit 1
    fi

    # Create HTTPS listener
    HTTPS_LISTENER_ARN=$(aws elbv2 create-listener \
        --load-balancer-arn $LB_ARN \
        --protocol HTTPS \
        --port $LISTENER_PORT_HTTPS \
        --certificates CertificateArn=$SSL_CERTIFICATE_ARN \
        --default-actions Type=forward,TargetGroupArn=$TG_ARN \
        --region $REGION \
        --query 'Listeners[0].ListenerArn' \
        --output text)

    if [ -z "$HTTPS_LISTENER_ARN" ]; then
        error "Failed to create HTTPS listener"
        exit 1
    fi

    # Create WebSocket listener
    WS_LISTENER_ARN=$(aws elbv2 create-listener \
        --load-balancer-arn $LB_ARN \
        --protocol HTTP \
        --port 8080 \
        --default-actions Type=forward,TargetGroupArn=$WS_TG_ARN \
        --region $REGION \
        --query 'Listeners[0].ListenerArn' \
        --output text)

    if [ -z "$WS_LISTENER_ARN" ]; then
        error "Failed to create WebSocket listener"
        exit 1
    fi

    export HTTP_LISTENER_ARN
    export HTTPS_LISTENER_ARN
    export WS_LISTENER_ARN
}

# Configure access logs
configure_access_logs() {
    log "Configuring access logs..."

    # Create S3 bucket for logs
    S3_BUCKET="erlmcp-alb-logs-$(date +%s)"
    S3_BUCKET=$(aws s3api create-bucket \
        --bucket $S3_BUCKET \
        --region $REGION \
        --create-bucket-configuration LocationConstraint=$REGION \
        --query 'Location' \
        --output text)

    if [ -z "$S3_BUCKET" ]; then
        error "Failed to create S3 bucket"
        exit 1
    fi

    # Enable access logs
    aws elbv2 modify-load-balancer-attributes \
        --load-balancer-arn $LB_ARN \
        --attributes Key=access_logs.s3.enabled,Value=true \
        --attributes Key=access_logs.s3.bucket,Value=$S3_BUCKET \
        --attributes Key=access_logs.s3.prefix,Value=alb-logs/ \
        --region $REGION

    echo "Access logs enabled in S3 bucket: $S3_BUCKET"
}

# Configure security policies
configure_security_policies() {
    log "Configuring security policies..."

    # Get available security policies
    aws elbv2 describe-security-policies \
        --region $REGION \
        --query 'SecurityPolicies[*].PolicyName' \
        --output table

    # Apply security policy
    aws elbv2 modify-listener \
        --listener-arn $HTTPS_LISTENER_ARN \
        --port 443 \
        --protocol HTTPS \
        --ssl-policy ELBSecurityPolicy-TLS13-1-2-2021-06 \
        --certificates CertificateArn=$SSL_CERTIFICATE_ARN \
        --region $REGION

    log "Security policy configured"
}

# configure WAF
configure_waf() {
    log "Configuring WAF..."

    # Create WAF web ACL
    WEB_ACL_ID=$(aws wafv2 create-web-acl \
        --name erlmcp-waf \
        --scope regional \
        --region $REGION \
        --default-action Allow={} \
        --rules \
        "Type='REGULAR',Name='SQLI',Priority=1,Action={Type='BLOCK'},Statement={ByteMatchStatement={TextTransformation='NONE',FieldToMatch={Data='query_string',Type='QUERY_STRING'},PositionalConstraint='CONTAINS',MatchedData='union|insert|delete|update|select'},RegexBasedMatchStatement={RegexString='(?i)(union.*select|insert.*into|delete.*from|'.*or.*'|'.*and.*|'.*exec|'.*execute)',RegexTransformation='NONE'}}" \
        "Type='REGULAR',Name='XSS',Priority=2,Action={Type='BLOCK'},Statement={ByteMatchStatement={TextTransformation='HTML_ENTITY_DECODE',FieldToMatch={Data='query_string',Type='QUERY_STRING'},PositionalConstraint='CONTAINS',MatchedData='<script>'}}}" \
        --visibility-config \
        "SampledRequestsEnabled=false,CloudWatchMetricsEnabled=true,MetricName=erlmcp-waf" \
        --query 'Summary.WebACLId' \
        --output text)

    if [ -z "$WEB_ACL_ID" ]; then
        error "Failed to create WAF web ACL"
        exit 1
    fi

    # Create WAF rules for rate limiting
    aws wafv2 create-rule \
        --name erlmcp-rate-limit \
        --scope regional \
        --region $REGION \
        --metric-name erlmcp-rate-limit \
        --statement "RateBasedStatement={
            Limit=1000,
            Duration=60,
            AggregateKeyType=IP
        }" \
        --priority 3 \
        --action Type="BLOCK" \
        --visibility-config SampledRequestsEnabled=false,CloudWatchMetricsEnabled=true,MetricName=erlmcp-rate-limit

    # Add rate limit rule to web ACL
    aws wafv2 update-web-acl \
        --name erlmcp-waf \
        --scope regional \
        --region $REGION \
        --rules "Action={Type='BLOCK'},Priority=3,Name=erlmcp-rate-limit,OverrideAction=None,CapturesNone=False,Rarn=arn:aws:wafv2:$REGION:123456789012:regional/rule/erlmcp-rate-limit/1234567890123456" \
        --default-action Allow={} \
        --visibility-config SampledRequestsEnabled=false,CloudWatchMetricsEnabled=true,MetricName=erlmcp-waf

    # Associate WAF with load balancer
    LB_ARN=$(aws elbv2 describe-load-balancers --names $LB_NAME --query 'LoadBalancers[0].LoadBalancerArn' --output text)
    aws wafv2 associate-web-acl \
        --web-acl-arn arn:aws:wafv2:$REGION:123456789012:regional/webacl/erlmcp-waf/$WEB_ACL_ID \
        --resource-arn $LB_ARN \
        --region $REGION

    log "WAF configured and associated"
}

# Configure routing
configure_routing() {
    log "Configuring routing..."

    # Get target group ARN
    TG_ARN=$(aws elbv2 describe-target-groups \
        --names $TARGET_GROUP_NAME \
        --query 'TargetGroups[0].TargetGroupArn' \
        --output text)

    # Configure weighted routing
    aws elbv2 modify-target-group-attributes \
        --target-group-arn $TG_ARN \
        --attributes Key=stickiness.enabled,Value=true \
        --attributes Key=stickiness.type,Value=lb_cookie \
        --region $REGION

    # Create priority rule for WebSocket
    aws elbv2 modify-listener \
        --listener-arn $HTTPS_LISTENER_ARN \
        --port 8443 \
        --protocol HTTPS \
        --certificates CertificateArn=$SSL_CERTIFICATE_ARN \
        --default-actions Type=forward,TargetGroupArn=$WS_TG_ARN \
        --region $REGION

    log "Routing configured"
}

# Enable cross-zone load balancing
enable_cross_zone() {
    log "Enabling cross-zone load balancing..."

    LB_ARN=$(aws elbv2 describe-load-balancers --names $LB_NAME --query 'LoadBalancers[0].LoadBalancerArn' --output text)

    aws elbv2 modify-load-balancer-attributes \
        --load-balancer-arn $LB_ARN \
        --attributes Key=load_balancing.cross_zone.enabled,Value=true \
        --region $REGION

    log "Cross-zone load balancing enabled"
}

# Verify deployment
verify_deployment() {
    log "Verifying load balancer deployment..."

    # Check load balancer status
    aws elbv2 describe-load-balancers \
        --names $LB_NAME \
        --query 'LoadBalancers[0].State' \
        --output text

    # Check listener status
    aws elbv2 describe-listeners \
        --load-balancer-arn $LB_ARN \
        --query 'Listeners[0].State' \
        --output text

    # Check target group status
    aws elbv2 describe-target-groups \
        --names $TARGET_GROUP_NAME \
        --query 'TargetGroups[0].HealthCheckEnabled' \
        --output text

    # Get DNS name
    LB_DNS=$(aws elbv2 describe-load-balancers \
        --names $LB_NAME \
        --query 'LoadBalancers[0].DNSName' \
        --output text)

    echo "Load balancer DNS: $LB_DNS"
    echo "Load balancer ARN: $LB_ARN"
}

# Generate configuration files
generate_configs() {
    log "Generating configuration files..."

    # Generate Nginx configuration
    cat > /etc/nginx/nginx.conf << 'EOF'
user nginx;
worker_processes auto;
error_log /var/log/nginx/error.log notice;
pid /var/run/nginx.pid;

events {
    worker_connections 4096;
    use epoll;
    multi_accept on;
}

http {
    include /etc/nginx/mime.types;
    default_type application/octet-stream;

    log_format main '$remote_addr - $remote_user [$time_local] "$request" '
                    '$status $body_bytes_sent "$http_referer" '
                    '"$http_user_agent" "$http_x_forwarded_for"';

    access_log /var/log/nginx/access.log main;

    sendfile on;
    tcp_nopush on;
    tcp_nodelay on;
    keepalive_timeout 65;
    types_hash_max_size 2048;

    gzip on;
    gzip_comp_level 6;
    gzip_vary on;
    gzip_min_length 1000;
    gzip_proxied any;
    gzip_types text/plain text/css application/json application/javascript application/xml+rss text/xml;

    upstream erlmcp_backend {
        server 10.20.0.10:8080;
        server 10.20.0.11:8080;
        server 10.20.0.12:8080;
        server 10.20.0.13:8080;
        server 10.20.0.14:8080;
        server 10.20.0.15:8080;
    }

    server {
        listen 80;
        server_name erlmcp.example.com;
        return 301 https://$server_name$request_uri;
    }

    server {
        listen 443 ssl http2;
        server_name erlmcp.example.com;

        ssl_certificate /etc/ssl/certs/erlmcp.pem;
        ssl_certificate_key /etc/ssl/private/erlmcp.key;
        ssl_protocols TLSv1.2 TLSv1.3;
        ssl_ciphers ECDHE-RSA-AES256-GCM-SHA512:DHE-RSA-AES256-GCM-SHA512;
        ssl_prefer_server_ciphers off;

        location / {
            proxy_pass http://erlmcp_backend;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_buffering on;
            proxy_buffer_size 4k;
            proxy_buffers 8 4k;
        }

        location /health {
            access_log off;
            return 200 "OK\n";
            add_header Content-Type text/plain;
        }
    }
}
EOF

    # Generate HAProxy configuration
    cat > /etc/haproxy/haproxy.cfg << 'EOF'
global
    log /dev/log local0
    log /dev/log local1 notice
    chroot /var/lib/haproxy
    stats socket /run/haproxy/admin.sock mode 660 level admin
    stats timeout 30s
    user haproxy
    group haproxy
    daemon

defaults
    mode http
    timeout connect 5s
    timeout client 30s
    timeout server 30s
    timeout check 5s
    retries 3
    option httplog
    option dontlognull
    option http-server-close
    option forwardfor except 127.0.0.0/8
    option redispatch

frontend erlmcp_frontend
    bind *:443 ssl crt /etc/ssl/certs/erlmcp.pem
    bind *:80
    option httpclose
    option forwardfor
    use_backend erlmcp_backend

backend erlmcp_backend
    balance leastconn
    option httpchk GET /health
    http-check expect status 200
    server lb1 10.20.0.10:8080 check
    server lb2 10.20.0.11:8080 check
    server lb3 10.20.0.12:8080 check
    server lb4 10.20.0.13:8080 check
    server lb5 10.20.0.14:8080 check
    server lb6 10.20.0.15:8080 check
EOF

    log "Configuration files generated"
}

# Main execution
main() {
    log "Starting erlmcp v3 load balancer configuration..."

    validate_config
    create_load_balancer
    create_target_groups
    create_listeners
    configure_access_logs
    configure_security_policies
    configure_waf
    configure_routing
    enable_cross_zone
    verify_deployment
    generate_configs

    log "Load balancer configuration completed!"
    echo "Load Balancer DNS: $LB_DNS"
}

# Run main function
main "$@"