# API Gateway Deployment Guide for erlmcp v3

## Overview

This guide provides step-by-step instructions for deploying the erlmcp API Gateway in enterprise environments. The deployment covers multiple scenarios including on-premise, cloud, and hybrid deployments with high availability and scalability.

## Prerequisites

### System Requirements
- **Erlang/OTP**: 28.3.1 or later
- **Memory**: Minimum 8GB RAM (16GB recommended)
- **CPU**: 4 cores minimum (8 cores recommended)
- **Disk**: 100GB SSD with fast I/O
- **Network**: 1Gbps or higher

### Software Dependencies
- **Redis**: 6.2 or later
- **PostgreSQL**: 13 or later
- **Prometheus**: 2.30 or later (for monitoring)
- **Grafana**: 8.0 or later (for dashboards)
- **Nginx**: 1.20 or later (as reverse proxy)

## Deployment Architecture

### High Availability Deployment
```
┌─────────────────────────────────────────────────────────────┐
│                         Load Balancer                        │
│                         (HAProxy/Nginx)                     │
└─────────────────────┬─────────────────────┬─────────────────┘
                      │                     │
┌─────────────────────▼─────────────────────▼─────────────────┐
│                   API Gateway Cluster                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐        │
│  │ Gateway 1  │  │ Gateway 2  │  │ Gateway 3  │        │
│  └─────────────┘  └─────────────┘  └─────────────┘        │
└─────────────────────┬─────────────────────┬─────────────────┘
                      │                     │
┌─────────────────────▼─────────────────────▼─────────────────┐
│                   Backend Services                          │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐        │
│  │ Database    │  │ Redis       │  │ Analytics  │        │
│  │ Cluster     │  │ Cluster     │  │ Cluster    │        │
│  └─────────────┘  └─────────────┘  └─────────────┘        │
└─────────────────────────────────────────────────────────────┘
```

### Components Overview
1. **Load Balancer**: Distributes traffic across gateways
2. **API Gateway**: Core API processing and routing
3. **Database**: PostgreSQL for persistent storage
4. **Cache**: Redis for high-speed caching
5. **Monitoring**: Prometheus and Grafana for observability

## Step-by-Step Deployment

### Step 1: Prepare the Environment

#### Update System
```bash
# Ubuntu/Debian
sudo apt update && sudo apt upgrade -y
sudo apt install -y build-essential git curl

# CentOS/RHEL
sudo yum update -y
sudo yum groupinstall -y "Development Tools"
sudo yum install -y git curl
```

#### Install Erlang/OTP
```bash
# Download and install OTP 28.3.1
wget https://github.com/erlang/otp/releases/download/OTP-28.3.1/otp_src_28.3.1.tar.gz
tar -xzf otp_src_28.3.1.tar.gz
cd otp_src_28.3.1
./configure --prefix=/opt/erlang-28.3.1 \
            --enable-smp-support \
            --enable-threads \
            --enable-smp-support \
            --enable-hipe \
            --enable-dirty-schedulers
make -j$(nproc)
sudo make install

# Set environment variables
echo 'export PATH=/opt/erlang-28.3.1/bin:$PATH' | sudo tee -a /etc/environment
export PATH=/opt/erlang-28.3.1/bin:$PATH
```

#### Install Dependencies
```bash
# Redis
sudo apt install -y redis-server
sudo systemctl enable redis
sudo systemctl start redis

# PostgreSQL
sudo apt install -y postgresql postgresql-contrib
sudo systemctl enable postgresql
sudo systemctl start postgresql

# Create databases
sudo -u postgres createdb erlmcp_gateway
sudo -u postgres createdb erlmcp_analytics
```

### Step 2: Build and Install erlmcp API Gateway

#### Clone Repository
```bash
git clone https://github.com/your-org/erlmcp.git
cd erlmcp/apps/erlmcp_api_gateway
```

#### Configure Build
```bash
# Update rebar config with proper paths
cat > rebar.config << EOF
{erl_opts, [debug_info, warnings_as_errors, warn_unused_vars,
             warn_shadow_vars, warn_unused_import]}.

{deps, [
    {cowboy, "2.9.0"},
    {jsx, "3.1.0"},
    {uuid, "1.7.5"},
    {crypto, "4.11.0"},
    {redix, "1.1.0"},
    {prometheus, "4.10.0"}
]}.

{relx, [
    {release, {erlmcp_api_gateway, "0.1.0"},
     [sasl, erlmcp_api_gateway]},
    {sys_config, "config/sys.config"},
    {vm_args, "config/vm.args"}
]}.
EOF
```

#### Build Application
```bash
# Clean and build
rebar3 clean
rebar3 compile
rebar3 release

# Prepare release
rebar3 release
```

### Step 3: Configure API Gateway

#### Database Configuration
```sql
-- Create tables in PostgreSQL
CREATE TABLE api_registry (
    id VARCHAR(36) PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    upstream_url VARCHAR(255) NOT NULL,
    rate_limit JSONB,
    enabled BOOLEAN DEFAULT true,
    created_at BIGINT,
    updated_at BIGINT
);

CREATE TABLE consumers (
    id VARCHAR(36) PRIMARY KEY,
    username VARCHAR(255) UNIQUE NOT NULL,
    api_key VARCHAR(64) UNIQUE,
    secret VARCHAR(128),
    custom_id VARCHAR(255),
    tags TEXT[],
    created_at BIGINT,
    updated_at BIGINT
);

CREATE TABLE audit_log (
    id VARCHAR(36) PRIMARY KEY,
    timestamp BIGINT,
    api_id VARCHAR(36),
    consumer_id VARCHAR(36),
    action VARCHAR(50),
    details JSONB,
    result VARCHAR(10),
    policy_id VARCHAR(36)
);
```

#### Redis Configuration
```redis
# /etc/redis/redis.conf
bind 127.0.0.1 10.0.0.5
port 6379
requirepass your_redis_password
maxmemory 8gb
maxmemory-policy allkeys-lru
save 900 1
save 300 10
save 60 10000
```

#### API Gateway Configuration
```erlang
% config/sys.config
[
    {erlmcp_api_gateway, [
        {port, 8080},
        {ssl_port, 8443},
        {ssl_certfile, "/etc/ssl/certs/api-gateway.crt"},
        {ssl_keyfile, "/etc/ssl/private/api-gateway.key"},
        {ssl_cacertfile, "/etc/ssl/certs/ca-bundle.crt"},
        {database, [
            {host, "localhost"},
            {port, 5432},
            {database, "erlmcp_gateway"},
            {username, "erlmcp_user"},
            {password, "secure_password"}
        ]},
        {redis, [
            {host, "localhost"},
            {port, 6379},
            {password, "your_redis_password"},
            {database, 0}
        ]},
        {cache, [
            {size, 1000},
            {ttl, 300},
            {strategy, lru}
        ]},
        {monitoring, [
            {enabled, true},
            {metrics_port, 9090},
            {log_level, info}
        ]},
        {rate_limiting, [
            {default_limit, 1000},
            {window_ms, 60000},
            {burst_size, 100}
        ]},
        {security, [
            {jwt_secret, "your_jwt_secret"},
            {oauth_secret, "your_oauth_secret"},
            {session_timeout, 3600}
        ]}
    ]}
].
```

### Step 4: Set Up High Availability

#### Load Balancer Configuration (HAProxy)
```haproxy
# /etc/haproxy/haproxy.cfg
global
    maxconn 100000
    user haproxy
    group haproxy

defaults
    mode http
    timeout connect 10s
    timeout client 60s
    timeout server 60s
    timeout tunnel 3600s
    maxconn 100000

frontend api-gateway
    bind *:80
    bind *:443 ssl crt /etc/ssl/certs/api-gateway.pem
    redirect scheme https if !{ ssl_fc }
    default_backend api-gateway-backend

backend api-gateway-backend
    balance roundrobin
    option httpchk GET /health
    http-check expect status 200

    server gateway-1 10.0.0.10:8080 check inter 5s rise 2 fall 3
    server gateway-2 10.0.0.11:8080 check inter 5s rise 2 fall 3
    server gateway-3 10.0.0.12:8080 check inter 5s rise 2 fall 3

    option httplog
    option tcp-check
```

#### Nginx Configuration
```nginx
# /etc/nginx/nginx.conf
user www-data;
worker_processes auto;
worker_rlimit_nofile 100000;

events {
    worker_connections 4000;
    use epoll;
    multi_accept on;
}

http {
    include /etc/nginx/mime.types;
    default_type application/octet-stream;

    # Performance optimizations
    sendfile on;
    tcp_nopush on;
    tcp_nodelay on;
    keepalive_timeout 30;
    keepalive_requests 1000;
    reset_timoutout on;

    # Gzip compression
    gzip on;
    gzip_vary on;
    gzip_min_length 1024;
    gzip_comp_level 6;
    gzip_types text/plain text/css text/xml text/javascript application/json;

    # API Gateway upstream
    upstream erlmcp_api {
        least_conn;
        server 10.0.0.10:8080 weight=3 max_fails=3 fail_timeout=30s;
        server 10.0.0.11:8080 weight=3 max_fails=3 fail_timeout=30s;
        server 10.0.0.12:8080 weight=2 max_fails=3 fail_timeout=30s;
        keepalive 100;
    }

    # HTTP to HTTPS redirect
    server {
        listen 80;
        server_name api.your-domain.com;
        return 301 https://$host$request_uri;
    }

    # Main server block
    server {
        listen 443 ssl http2;
        server_name api.your-domain.com;

        ssl_certificate /etc/ssl/certs/your-domain.crt;
        ssl_certificate_key /etc/ssl/private/your-domain.key;
        ssl_protocols TLSv1.2 TLSv1.3;
        ssl_ciphers ECDHE-RSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384;
        ssl_prefer_server_ciphers off;

        # Security headers
        add_header X-Frame-Options DENY;
        add_header X-Content-Type-Options nosniff;
        add_header X-XSS-Protection "1; mode=block";
        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains";

        # Rate limiting
        limit_req_zone $binary_remote_addr zone=api_limit:10m rate=100r/s;
        limit_req zone=api_limit burst=20 nodelay;

        location / {
            proxy_pass http://erlmcp_api;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;

            # Timeout settings
            proxy_connect_timeout 5s;
            proxy_send_timeout 60s;
            proxy_read_timeout 60s;
            proxy_buffer_size 4k;
            proxy_buffers 8 4k;
        }

        # Health check endpoint
        location /health {
            access_log off;
            proxy_pass http://erlmcp_api;
        }
    }
}
```

### Step 5: Deployment Script

#### Automated Deployment Script
```bash
#!/bin/bash
# deploy_api_gateway.sh

set -e

# Configuration
API_GATEWAY_VERSION="0.1.0"
DEPLOY_USER="deploy"
SERVERS=("10.0.0.10" "10.0.0.11" "10.0.0.12")
RELEASE_DIR="/opt/erlmcp_api_gateway"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log() {
    echo -e "${GREEN}[$(date +'%Y-%m-%d %H:%M:%S')] $1${NC}"
}

warn() {
    echo -e "${YELLOW}[$(date +'%Y-%m-%d %H:%M:%S')] WARNING: $1${NC}"
}

error() {
    echo -e "${RED}[$(date +'%Y-%m-%d %H:%M:%S')] ERROR: $1${NC}"
    exit 1
}

# Check prerequisites
check_prerequisites() {
    log "Checking prerequisites..."

    # Check Erlang version
    erl -version || error "Erlang/OTP is not installed"

    # Check rebar3
    rebar3 version || error "rebar3 is not installed"

    # Check SSH access
    for server in "${SERVERS[@]}"; do
        ssh -o BatchMode=yes -o ConnectTimeout=5 ${DEPLOY_USER}@${server} "exit" 2>/dev/null ||
            error "Cannot connect to ${server}"
    done

    log "Prerequisites check passed"
}

# Build release
build_release() {
    log "Building release..."

    # Clean previous builds
    rebar3 clean

    # Compile
    rebar3 compile

    # Create release
    rebar3 release

    log "Release built successfully"
}

# Deploy to servers
deploy_to_servers() {
    log "Deploying to servers..."

    for server in "${SERVERS[@]}"; do
        log "Deploying to ${server}..."

        # Stop service
        ssh ${DEPLOY_USER}@${server} "sudo systemctl stop erlmcp-api-gateway"

        # Copy release
        scp -r _build/default/rel/erlmcp_api_gateway ${DEPLOY_USER}@${server}:${RELEASE_DIR}

        # Update permissions
        ssh ${DEPLOY_USER}@${server} "sudo chown -R ${DEPLOY_USER}:${DEPLOY_USER} ${RELEASE_DIR}"

        # Start service
        ssh ${DEPLOY_USER}@${server} "sudo systemctl start erlmcp-api-gateway"

        log "Successfully deployed to ${server}"
    done
}

# Verify deployment
verify_deployment() {
    log "Verifying deployment..."

    sleep 30

    for server in "${SERVERS[@]}"; do
        status=$(ssh -o BatchMode=yes ${DEPLOY_USER}@${server} "sudo systemctl is-active erlmcp-api-gateway" 2>/dev/null || echo "failed")

        if [ "$status" = "active" ]; then
            log "${server}: Service is running"
        else
            error "${server}: Service failed to start"
        fi

        # Check health endpoint
        health_check=$(curl -s -o /dev/null -w "%{http_code}" http://${server}:8080/health 2>/dev/null || echo "000")

        if [ "$health_check" = "200" ]; then
            log "${server}: Health check passed"
        else
            warn "${server}: Health check failed (${health_check})"
        fi
    done
}

# Main deployment process
main() {
    log "Starting API Gateway deployment..."

    check_prerequisites
    build_release
    deploy_to_servers
    verify_deployment

    log "API Gateway deployment completed successfully!"
}

# Run main function
main "$@"
```

### Step 6: Post-Deployment Configuration

#### Monitoring Setup
```yaml
# prometheus.yml
global:
  scrape_interval: 15s

scrape_configs:
  - job_name: 'erlmcp-api-gateway'
    static_configs:
      - targets: ['localhost:9090']
    metrics_path: '/metrics'
    scrape_interval: 15s
    scrape_timeout: 10s

  - job_name: 'nginx'
    static_configs:
      - targets: ['localhost:9113']
```

#### Grafana Dashboard
```json
{
  "dashboard": {
    "title": "erlmcp API Gateway",
    "panels": [
      {
        "title": "Request Rate",
        "type": "graph",
        "targets": [
          {
            "expr": "rate(erlmcp_api_requests_total[5m])",
            "legendFormat": "{{method}} - {{path}}"
          }
        ]
      },
      {
        "title": "Response Time",
        "type": "graph",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, sum(rate(erlmcp_api_response_time_bucket[5m])) by (le))",
            "legendFormat": "P95"
          }
        ]
      },
      {
        "title": "Error Rate",
        "type": "singlestat",
        "targets": [
          {
            "expr": "sum(rate(erlmcp_api_errors_total[5m])) / sum(rate(erlmcp_api_requests_total[5m])) * 100",
            "legendFormat": "Error Rate %"
          }
        ]
      }
    ]
  }
}
```

#### Health Check Script
```bash
#!/bin/bash
# health_check.sh

API_GATEWAY_URL="http://localhost:8080/health"
LOG_FILE="/var/log/erlmcp-health-check.log"
ALERT_EMAIL="admin@your-domain.com"

check_health() {
    response=$(curl -s -o /dev/null -w "%{http_code}" "$API_GATEWAY_URL")
    timestamp=$(date +'%Y-%m-%d %H:%M:%S')

    echo "[$timestamp] Health check: HTTP $response" >> "$LOG_FILE"

    if [ "$response" -eq 200 ]; then
        echo "Gateway is healthy"
        return 0
    else
        echo "Gateway is unhealthy (HTTP $response)" | mail -s "API Gateway Alert" "$ALERT_EMAIL"
        return 1
    fi
}

# Run check every 5 minutes
while true; do
    check_health
    sleep 300
done
```

## Deployment Scenarios

### Cloud Deployment (AWS)
```bash
# AWS deployment script
#!/bin/bash
# deploy_aws.sh

# Create VPC and subnets
aws ec2 create-vpc --cidr-block 10.0.0.0/16 --tag-specifications 'ResourceType=vpc,Tags=[{Key=Name,Value=erlmcp-vpc}]'

# Create security group
aws ec2 create-security-group --group-name erlmcp-sg --description "erlmcp API Gateway security group"

# Launch instances
aws ec2 run-instances \
    --image-id ami-0abcdef1234567890 \
    --instance-type m5.large \
    --key-name erlmcp-key \
    --security-group-ids sg-0abcdef1234567890 \
    --subnet-id subnet-0abcdef1234567890 \
    --user-data file://cloud-user-data.sh

# Load balancer
aws elb create-load-balancer --load-balancer-name erlmcp-lb \
    --listeners "Protocol=HTTP,LoadBalancerPort=80,InstancePort=8080" \
    --security-group sg-0abcdef1234567890 \
    --subnets subnet-0abcdef1234567890
```

### Kubernetes Deployment
```yaml
# api-gateway-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp-api-gateway
  labels:
    app: erlmcp-api-gateway
spec:
  replicas: 3
  selector:
    matchLabels:
      app: erlmcp-api-gateway
  template:
    metadata:
      labels:
        app: erlmcp-api-gateway
    spec:
      containers:
      - name: api-gateway
        image: erlmcp/api-gateway:latest
        ports:
        - containerPort: 8080
        - containerPort: 9090
        env:
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: erlmcp-secrets
              key: database-url
        - name: REDIS_URL
          valueFrom:
            secretKeyRef:
              name: erlmcp-secrets
              key: redis-url
        resources:
          requests:
            memory: "2Gi"
            cpu: "1"
          limits:
            memory: "4Gi"
            cpu: "2"
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5

---
apiVersion: v1
kind: Service
metadata:
  name: erlmcp-api-gateway-service
spec:
  selector:
    app: erlmcp-api-gateway
  ports:
  - name: http
    port: 80
    targetPort: 8080
  - name: metrics
    port: 9090
    targetPort: 9090
```

## Best Practices

### Security Considerations
1. **Network Security**: Use private subnets and security groups
2. **SSL/TLS**: Always use HTTPS with strong cipher suites
3. **Authentication**: Implement OAuth2 and API key authentication
4. **Authorization**: Follow principle of least privilege
5. **Auditing**: Enable comprehensive audit logging

### Performance Optimization
1. **Caching**: Implement multi-level caching (Redis, CDN)
2. **Load Balancing**: Use least connection algorithm
3. **Connection Pooling**: Configure appropriate pool sizes
4. **Compression**: Enable Gzip/Brotli compression
5. **Keep-Alive**: Enable HTTP keep-alive connections

### Monitoring and Alerting
1. **Metrics**: Track key performance indicators
2. **Logging**: Centralize logs with proper retention
3. **Alerts**: Configure alerts for critical issues
4. **Dashboards**: Create visual dashboards for monitoring
5. **Capacity Planning**: Monitor resource usage trends

### Maintenance and Updates
1. **Rolling Updates**: Update instances one by one
2. **Health Checks**: Monitor service health continuously
3. **Backups**: Regular database and configuration backups
4. **Versioning**: Maintain proper version control
5. **Documentation**: Keep deployment documentation updated

## Troubleshooting

### Common Issues
1. **High Memory Usage**: Check for memory leaks, optimize cache
2. **Slow Response Times**: Check database queries, optimize caching
3. **Connection Refused**: Check service status, port configuration
4. **Authentication Failures**: Verify JWT tokens, check secrets
5. **Rate Limit Issues**: Check rate limiting configuration

### Debug Commands
```bash
# Check service status
sudo systemctl status erlmcp-api-gateway

# View logs
journalctl -u erlmcp-api-gateway -f

# Check process information
erl -name debug@localhost -remsh erlmcp_api_gateway@localhost cookie erlmcp

# Monitor database connections
psql -h localhost -U erlmcp_user -d erlmcp_gateway -c "SELECT * FROM pg_stat_activity;"

# Check Redis memory
redis-cli INFO memory
```

This comprehensive deployment guide ensures enterprise-grade deployment of erlmcp API Gateway with high availability, scalability, and performance optimization.