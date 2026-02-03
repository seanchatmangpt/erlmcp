# erlmcp v3 Docker Deployment Guide

## Overview

This guide provides comprehensive instructions for deploying erlmcp v3 using Docker. Docker deployment offers isolated, reproducible environments ideal for development, testing, and production scenarios.

## Prerequisites

### System Requirements

- **Docker Engine**: Version 20.10 or later
- **Docker Compose**: Version 2.1 or later
- **Minimum Resources**:
  - CPU: 2 cores
  - Memory: 4GB RAM
  - Storage: 20GB SSD
- **Network**: Access to Docker registry (Docker Hub or private registry)

### Required Files

- `docker-compose.yml` - Main orchestration file
- `Dockerfile` - Container build configuration
- `docker.env` - Environment variables
- `certs/` - SSL certificates (for production)
- `config/` - Configuration files

## Quick Start

### 1. Clone the Repository

```bash
git clone https://github.com/your-org/erlmcp.git
cd erlmcp/docs/enterprise-suite/deployment/docker
```

### 2. Configure Environment

```bash
# Copy environment template
cp docker.env.example docker.env

# Edit environment variables
nano docker.env
```

### 3. Start the Stack

```bash
# Build and start all services
docker-compose up -d

# Check status
docker-compose ps
```

### 4. Verify Deployment

```bash
# Check health
curl -f http://localhost:8080/v3/health

# View logs
docker-compose logs -f erlmcp
```

## Configuration

### Environment Variables

Create `docker.env` with the following configuration:

```env
# Cluster Configuration
ERLMCP_CLUSTER_NAME=erlmcp-cluster
ERLMCP_NODE_NAME=erlmcp@docker
ERLMCP_COOKIE=my-secret-cookie

# Database Configuration
ERLMCP_DB_HOST=postgres
ERLMCP_DB_PORT=5432
ERLMCP_DB_NAME=erlmcp
ERLMCP_DB_USER=erlmcp
ERLMCP_DB_PASSWORD=your-password

# Redis Configuration
ERLMCP_REDIS_HOST=redis
ERLMCP_REDIS_PORT=6379
ERLMCP_REDIS_PASSWORD=your-redis-password

# Security
ERLMCP_AUTH_SECRET=your-auth-secret
ERLMCP_JWT_SECRET=your-jwt-secret

# Performance
ERLMCP_MAX_CONNECTIONS=10000
ERLMCP_MAX_SESSIONS=5000
ERLMCP_WORKERS=4

# Monitoring
ERLMCP_METRICS_ENABLED=true
ERLMCP_TRACING_ENABLED=true
```

### Docker Compose Configuration

`docker-compose.yml`:

```yaml
version: '3.8'

services:
  erlmcp:
    build:
      context: ../../..
      dockerfile: docs/enterprise-suite/deployment/docker/Dockerfile
    container_name: erlmcp-v3
    hostname: erlmcp-node-1
    ports:
      - "8080:8080"   # HTTP
      - "8443:8443"   # HTTPS
      - "9090:9090"   # Metrics
    environment:
      - ERLMCP_CLUSTER_NAME=erlmcp-cluster
      - ERLMCP_NODE_NAME=erlmcp@docker
      - ERLMCP_COOKIE=${ERLMCP_COOKIE}
      - ERLMCP_DB_HOST=${ERLMCP_DB_HOST}
      - ERLMCP_DB_PORT=${ERLMCP_DB_PORT}
      - ERLMCP_DB_NAME=${ERLMCP_DB_NAME}
      - ERLCP_DB_USER=${ERLMCP_DB_USER}
      - ERLCP_DB_PASSWORD=${ERLMCP_DB_PASSWORD}
      - ERLMCP_REDIS_HOST=${ERLMCP_REDIS_HOST}
      - ERLMCP_REDIS_PORT=${ERLMCP_REDIS_PORT}
      - ERLMCP_REDIS_PASSWORD=${ERLMCP_REDIS_PASSWORD}
      - ERLMCP_AUTH_SECRET=${ERLMCP_AUTH_SECRET}
      - ERLMCP_JWT_SECRET=${ERLMCP_JWT_SECRET}
      - ERLMCP_MAX_CONNECTIONS=${ERLMCP_MAX_CONNECTIONS}
      - ERLMCP_MAX_SESSIONS=${ERLMCP_MAX_SESSIONS}
      - ERLMCP_WORKERS=${ERLMCP_WORKERS}
      - ERLMCP_METRICS_ENABLED=${ERLMCP_METRICS_ENABLED}
      - ERLMCP_TRACING_ENABLED=${ERLMCP_TRACING_ENABLED}
    volumes:
      - ./config:/app/config
      - ./logs:/app/logs
      - ./certs:/app/certs
    depends_on:
      postgres:
        condition: service_healthy
      redis:
        condition: service_healthy
    networks:
      - erlmcp-network
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/v3/health"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s

  postgres:
    image: postgres:15
    container_name: erlmcp-postgres
    environment:
      POSTGRES_DB: ${ERLMCP_DB_NAME}
      POSTGRES_USER: ${ERLMCP_DB_USER}
      POSTGRES_PASSWORD: ${ERLMCP_DB_PASSWORD}
    volumes:
      - postgres_data:/var/lib/postgresql/data
      - ./init.sql:/docker-entrypoint-initdb.d/init.sql
    ports:
      - "5432:5432"
    networks:
      - erlmcp-network
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U ${ERLMCP_DB_USER} -d ${ERLMCP_DB_NAME}"]
      interval: 30s
      timeout: 10s
      retries: 5

  redis:
    image: redis:7-alpine
    container_name: erlmcp-redis
    command: redis-server --requirepass ${ERLMCP_REDIS_PASSWORD}
    volumes:
      - redis_data:/data
    ports:
      - "6379:6379"
    networks:
      - erlmcp-network
    healthcheck:
      test: ["CMD", "redis-cli", "auth", "${ERLMCP_REDIS_PASSWORD}", "ping"]
      interval: 30s
      timeout: 10s
      retries: 3

  prometheus:
    image: prom/prometheus:latest
    container_name: erlmcp-prometheus
    ports:
      - "9091:9090"
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml
      - prometheus_data:/prometheus
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'
      - '--web.console.libraries=/etc/prometheus/console_libraries'
      - '--web.console.templates=/etc/prometheus/consoles'
      - '--storage.tsdb.retention.time=200h'
      - '--web.enable-lifecycle'
    networks:
      - erlmcp-network

  grafana:
    image: grafana/grafana:latest
    container_name: erlmcp-grafana
    ports:
      - "3000:3000"
    environment:
      GF_SECURITY_ADMIN_PASSWORD: admin123
      GF_USERS_ALLOW_SIGN_UP: false
    volumes:
      - grafana_data:/var/lib/grafana
      - ./grafana/dashboards:/etc/grafana/provisioning/dashboards
      - ./grafana/datasources:/etc/grafana/provisioning/datasources
    networks:
      - erlmcp-network

volumes:
  postgres_data:
  redis_data:
  prometheus_data:
  grafana_data:

networks:
  erlmcp-network:
    driver: bridge
```

### Dockerfile

`Dockerfile`:

```dockerfile
# Stage 1: Build stage
FROM erlang:28.3.1-alpine as builder

# Install dependencies
RUN apk add --no-cache \
    build-base \
    git \
    curl \
    openssl \
    ncurses-dev \
    zlib-dev

# Install OTP
RUN curl -sSL https://github.com/erlang/otp/archive/OTP-28.3.1.tar.gz | tar -xzf - \
    && cd otp-OTP-28.3.1 \
    && ./otp_build autoconf \
    && ./configure \
        --enable-smp-support \
        --enable-threads \
        --enable-smp-support \
        --enable-dirty-schedulers \
        --disable-hipe \
        --enable-pcre2 \
    && make -j$(nproc) \
    && make install \
    && cd .. \
    && rm -rf otp-OTP-28.3.1

# Install rebar3
RUN curl -sSL https://github.com/erlang/rebar3/releases/download/3.22.1/rebar3 > /usr/local/bin/rebar3 \
    && chmod +x /usr/local/bin/rebar3

# Copy application code
WORKDIR /app
COPY . .

# Build the application
RUN rebar3 compile

# Stage 2: Runtime stage
FROM erlang:28.3.1-alpine

# Install runtime dependencies
RUN apk add --no-cache \
    openssl \
    curl \
    bash \
    tini

# Create app user
RUN addgroup -g 1000 erlmcp && \
    adduser -u 1000 -G erlmcp -s /bin/bash -D erlmcp

# Copy built application
COPY --from=builder /app /app
RUN chown -R erlmcp:erlmcp /app

# Set working directory
WORKDIR /app

# Create necessary directories
RUN mkdir -p /app/logs /app/config /app/certs

# Switch to app user
USER erlmcp

# Expose ports
EXPOSE 8080 8443 9090

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
    CMD curl -f http://localhost:8080/v3/health || exit 1

# Start the application
ENTRYPOINT ["tini", "--"]
CMD ["./scripts/start.sh"]
```

## Production Deployment

### 1. SSL Configuration

Create `certs/` directory and place your certificates:

```
certs/
├── server.crt
├── server.key
└── ca.crt
```

### 2. Configuration File

Create `config/prod.config`:

```erlang
{
    erlmcp,
    [
        % Cluster configuration
        {cluster_name, "erlmcp-cluster"},
        {node_name, "erlmcp@prod-node-1"},
        {cookie, "prod-secret-cookie"},

        % Database
        {db_host, "postgres"},
        {db_port, 5432},
        {db_name, "erlmcp_prod"},
        {db_user, "erlmcp"},
        {db_password, "prod-password"},
        {db_pool_size, 20},

        % Redis
        {redis_host, "redis"},
        {redis_port, 6379},
        {redis_password, "prod-redis-password"},
        {redis_pool_size, 10},

        % Security
        {auth_secret, "prod-auth-secret"},
        {jwt_secret, "prod-jwt-secret"},

        % Performance
        {max_connections, 10000},
        {max_sessions, 5000},
        {workers, 4},

        % Monitoring
        {metrics_enabled, true},
        {tracing_enabled, true},

        % SSL
        {ssl, true},
        {ssl_port, 8443},
        {ssl_certfile, "/app/certs/server.crt"},
        {ssl_keyfile, "/app/certs/server.key"},
        {ssl_cafile, "/app/certs/ca.crt"},
        {ssl_versions, [tlsv1.2, tlsv1.3]},

        % Logging
        {log_level, info},
        {log_dir, "/app/logs"},

        % Backup
        {backup_enabled, true},
        {backup_schedule, "0 2 * * *"},
        {backup_retention, 30}
    ]
}.
```

### 3. Production Docker Compose

```yaml
version: '3.8'

services:
  erlmcp:
    build:
      context: ../../..
      dockerfile: Dockerfile.prod
    container_name: erlmcp-v3-prod
    hostname: erlmcp-prod-1
    ports:
      - "443:8443"   # HTTPS only
      - "9090:9090"   # Metrics
    environment:
      - ERLMCP_ENV=production
      - ERLMCP_DB_HOST=${ERLMCP_DB_HOST}
      - ERLMCP_DB_PORT=${ERLMCP_DB_PORT}
      - ERLMCP_DB_NAME=${ERLMCP_DB_NAME}
      - ERLMCP_DB_USER=${ERLMCP_DB_USER}
      - ERLMCP_DB_PASSWORD=${ERLMCP_DB_PASSWORD}
      - ERLMCP_REDIS_HOST=${ERLMCP_REDIS_HOST}
      - ERLMCP_REDIS_PORT=${ERLMCP_REDIS_PORT}
      - ERLMCP_REDIS_PASSWORD=${ERLMCP_REDIS_PASSWORD}
      - ERLMCP_AUTH_SECRET=${ERLMCP_AUTH_SECRET}
      - ERLMCP_JWT_SECRET=${ERLMCP_JWT_SECRET}
    volumes:
      - ./config/prod.config:/app/config/prod.config
      - /app/logs:/app/logs
      - /app/certs:/app/certs
      - /app/backups:/app/backups
    depends_on:
      - postgres
      - redis
    networks:
      - erlmcp-prod-network
    restart: unless-stopped
    deploy:
      resources:
        limits:
          cpus: '4.0'
          memory: 8G
        reservations:
          cpus: '2.0'
          memory: 4G
    healthcheck:
      test: ["CMD", "curl", "-f", "https://localhost:8443/v3/health"]
      interval: 30s
      timeout: 10s
      retries: 3

  # Additional services for production...
```

## Scaling and High Availability

### Horizontal Scaling

For high availability, deploy multiple erlmcp nodes:

```yaml
services:
  erlmcp-1:
    build:
      context: ../../..
      dockerfile: Dockerfile.prod
    hostname: erlmcp-prod-1
    environment:
      - ERLMCP_CLUSTER_NAME=erlmcp-cluster
      - ERLMCP_NODE_NAME=erlmcp@erlmcp-prod-1
      - ERLMCP_PEER_NODES=erlmcp-prod-2:erlmcp-prod-3

  erlmcp-2:
    build:
      context: ../../..
      dockerfile: Dockerfile.prod
    hostname: erlmcp-prod-2
    environment:
      - ERLMCP_CLUSTER_NAME=erlmcp-cluster
      - ERLMCP_NODE_NAME=erlmcp@erlmcp-prod-2
      - ERLMCP_PEER_NODES=erlmcp-prod-1:erlmcp-prod-3

  erlmcp-3:
    build:
      context: ../../..
      dockerfile: Dockerfile.prod
    hostname: erlmcp-prod-3
    environment:
      - ERLMCP_CLUSTER_NAME=erlmcp-cluster
      - ERLMCP_NODE_NAME=erlmcp@erlmcp-prod-3
      - ERLMCP_PEER_NODES=erlmcp-prod-1:erlmcp-prod-2
```

### Load Balancing

Use Nginx as a load balancer:

```nginx
upstream erlmcp_backend {
    server erlmcp-prod-1:8443;
    server erlmcp-prod-2:8443;
    server erlmcp-prod-3:8443;
}

server {
    listen 80;
    server_name api.erlmcp.com;

    location / {
        return 301 https://$host$request_uri;
    }
}

server {
    listen 443 ssl http2;
    server_name api.erlmcp.com;

    ssl_certificate /etc/nginx/ssl/server.crt;
    ssl_certificate_key /etc/nginx/ssl/server.key;

    location /v3/ {
        proxy_pass https://erlmcp_backend/v3/;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # Health checks
        proxy_connect_timeout 5s;
        proxy_read_timeout 30s;
        proxy_send_timeout 30s;

        # Rate limiting
        limit_req zone=erlmcp_zone burst=20 nodelay;
    }
}
```

## Monitoring and Logging

### Prometheus Configuration

`prometheus.yml`:

```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'erlmcp'
    static_configs:
      - targets: ['erlmcp:9090']
    scrape_interval: 15s
    metrics_path: /metrics

  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:9090']

  - job_name: 'postgres'
    static_configs:
      - targets: ['postgres:5432']

  - job_name: 'redis'
    static_configs:
      - targets: ['redis:6379']
```

### Logging Configuration

Create `config/logging.config`:

```erlang
{
    logger,
    [
        {handler, default, logger_console_hander,
            {logger_formatter,
                #{single_line => false,
                  time_design => {utc, ISO8601},
                  legacy_header => true}},
            #{level => info}},
        {handler, file, logger_stdlib_hander,
            {logger_disk_log_hander,
                #{file => "/app/logs/erlmcp.log",
                  type => wrap,
                  max_no_files => 10,
                  max_no_bytes => 100000000,
                  compress => true}},
            #{level => info}},
        {handler, error_file, logger_stdlib_hander,
            {logger_disk_log_hander,
                #{file => "/app/logs/error.log",
                  type => wrap,
                  max_no_files => 5,
                  max_no_bytes => 50000000,
                  compress => true}},
            #{level => error}}
    ]
}.
```

## Backup and Recovery

### Database Backup

```bash
# Create daily backup
docker exec erlmcp-postgres pg_dump -U erlmcp erlmcp > backup_$(date +%Y%m%d).sql

# Restore from backup
docker exec -i erlmcp-postgres psql -U erlmcp erlmcp < backup_20240101.sql
```

### Application Backup

```bash
# Backup configuration and logs
docker exec erlmcp-v3 tar -czf /app/backups/app_$(date +%Y%m%d).tar.gz \
    /app/config /app/logs

# Restore backup
docker cp backup_20240101.tar.gz erlmcp-v3:/app/
docker exec erlmcp-v3 tar -xzf /app/backups/app_20240101.tar.gz
```

## Troubleshooting

### Common Issues

#### 1. Container Won't Start

```bash
# Check logs
docker logs erlmcp-v3

# Check resource usage
docker stats erlmcp-v3

# Enter container for debugging
docker exec -it erlmcp-v3 bash
```

#### 2. Database Connection Issues

```bash
# Test database connection
docker exec erlmcp-postgres psql -U erlmcp -c "\dt"

# Check container status
docker-compose ps postgres
```

#### 3. Memory Issues

```bash
# Monitor memory usage
docker stats --no-stream erlmcp-v3

# Check Erlang memory
docker exec erlmcp-v3 erl -eval "erlang:memory()." -s init stop
```

### Performance Tuning

```bash
# Adjust worker count based on available CPUs
ERLMCP_WORKERS=4

# Increase database pool size
ERLMCP_DB_POOL_SIZE=20

# Tune JVM options
JAVA_OPTS="-Xms512m -Xmx2g"
```

## Security Best Practices

### 1. Container Security

- Use non-root user
- Scan images for vulnerabilities
- Rotate secrets regularly
- Enable read-only filesystem

```dockerfile
USER erlmcp

# Create read-only mount points
VOLUME ["/app/config", "/app/logs"]
```

### 2. Network Security

- Use private networks
- Enable encryption
- Implement firewalls
- Monitor network traffic

### 3. Secret Management

```bash
# Use Docker secrets for production
echo "your-password" | docker secret create db_password -

# Reference in docker-compose
environment:
  - ERLMCP_DB_PASSWORD_FILE=/run/secrets/db_password
```

## Maintenance

### Regular Maintenance Tasks

```bash
# Update containers
docker-compose pull
docker-compose up -d --force-recreate

# Clean up unused resources
docker system prune -f

# Rotate logs
docker exec erlmcp-v3 find /app/logs -name "*.log" -mtime +30 -delete
```

### Health Checks

```bash
# Monitor system health
curl -f http://localhost:8080/v3/health

# Check metrics
curl http://localhost:9090/metrics

# Verify database
docker exec erlmcp-postgres pg_isready -U erlmcp
```

## Support

For enterprise support, contact:
- **Email**: enterprise-support@erlmcp.com
- **Portal**: https://enterprise.erlmcp.com
- **Documentation**: https://docs.erlmcp.com/v3