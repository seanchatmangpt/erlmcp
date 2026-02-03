# erlmcp v3 Administrator Training

## Table of Contents

1. [Introduction](#introduction)
2. [Installation and Setup](#installation-and-setup)
3. [Configuration Management](#configuration-management)
4. [Monitoring and Alerting](#monitoring-and-alerting)
5. [Security Management](#security-management)
6. [Backup and Recovery](#backup-and-recovery)
7. [Performance Tuning](#performance-tuning)
8. [Troubleshooting](#troubleshooting)
9. [Maintenance Procedures](#maintenance-procedures)
10. [Advanced Topics](#advanced-topics)

## Introduction

This training guide provides comprehensive instructions for erlmcp v3 administrators. It covers installation, configuration, monitoring, security, and maintenance procedures to ensure optimal system operation.

### Target Audience

- System Administrators
- DevOps Engineers
- Site Reliability Engineers (SREs)

### Learning Objectives

Upon completion of this training, you will be able to:
- Install and configure erlmcp v3
- Monitor system health and performance
- Manage security and access controls
- Perform backup and recovery operations
- Troubleshoot common issues
- Optimize system performance

## Installation and Setup

### Prerequisites

#### System Requirements

| Component | Minimum | Recommended |
|-----------|---------|-------------|
| CPU | 2 cores | 4+ cores |
| Memory | 8GB | 16GB+ |
| Storage | 50GB SSD | 100GB+ SSD |
| Network | 1Gbps | 10Gbps |

#### Software Requirements

- Erlang/OTP 28.3.1
- PostgreSQL 15
- Redis 7+
- Docker 20.10+ (optional)
- Kubernetes 1.25+ (optional)

### Installation Methods

#### 1. Package Installation (Linux)

```bash
# Download and install package
wget https://github.com/erlmcp/erlmcp/releases/download/v3.0.0/erlmcp-v3.0.0-linux-x64.tar.gz
tar -xzf erlmcp-v3.0.0-linux-x64.tar.gz
cd erlmcp-v3.0.0

# Install system service
sudo cp scripts/erlmcp.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable erlmcp
```

#### 2. Docker Installation

```bash
# Pull image
docker pull erlmcp/v3:3.0.0

# Run container
docker run -d \
  --name erlmcp \
  -p 8080:8080 \
  -p 8443:8443 \
  -v /var/lib/erlmcp:/app/data \
  erlmcp/v3:3.0.0

# For production
docker run -d \
  --name erlmcp-prod \
  --restart unless-stopped \
  -p 443:8443 \
  -v /etc/ssl/certs:/app/certs \
  -v /var/log/erlmcp:/app/logs \
  -e ERLMCP_ENV=production \
  erlmcp/v3:3.0.0
```

#### 3. Kubernetes Installation

```bash
# Install using Helm
helm repo add erlmcp https://erlmcp.github.io/helm-charts
helm install erlmcp erlmcp/erlmcp \
  --namespace erlmcp \
  --create-namespace \
  --set image.tag=3.0.0 \
  --set persistence.enabled=true \
  --set postgresql.enabled=true
```

### Initial Configuration

#### Configuration File Structure

```
/etc/erlmcp/
├── config.yaml          # Main configuration
├── database.yaml        # Database configuration
├── security.yaml        # Security configuration
├── monitoring.yaml       # Monitoring configuration
└── logging.yaml         # Logging configuration
```

#### Basic Configuration

```yaml
# /etc/erlmcp/config.yaml
cluster:
  name: erlmcp-cluster
  cookie: my-secret-cookie

database:
  host: localhost
  port: 5432
  name: erlmcp
  user: erlmcp
  password: secure-password
  pool_size: 20

redis:
  host: localhost
  port: 6379
  password: redis-password
  pool_size: 10

security:
  auth_secret: your-auth-secret
  jwt_secret: your-jwt-secret
  max_connections: 10000

monitoring:
  enabled: true
  metrics_port: 9090
  tracing_enabled: true

logging:
  level: info
  dir: /var/log/erlmcp
  max_size: 100MB
  max_files: 10
```

#### Start the Service

```bash
# Using systemd
sudo systemctl start erlmcp
sudo systemctl status erlmcp

# Using Docker
docker start erlmcp
docker logs erlmcp

# Check health
curl -f http://localhost:8080/v3/health
```

## Configuration Management

### Configuration Files

#### Main Configuration

```yaml
# config.yaml
cluster:
  name: erlmcp-prod
  cookie: prod-cluster-cookie
  nodes:
    - erlmcp@node1.example.com
    - erlmcp@node2.example.com
    - erlmcp@node3.example.com

app:
  name: erlmcp-v3
  version: 3.0.0
  environment: production

performance:
  max_connections: 10000
  max_sessions: 5000
  workers: 4
  timeout: 30000

backup:
  enabled: true
  schedule: "0 2 * * *"
  retention: 30
```

#### Security Configuration

```yaml
# security.yaml
authentication:
  methods:
    - bearer
    - oauth2
    - api_key
  jwt:
    algorithm: RS256
    expiration: 3600
    refresh_expiration: 86400
  oauth2:
    providers:
      google:
        client_id: your-google-client-id
        client_secret: your-google-secret
        scopes: ["openid", "profile", "email"]
      azure:
        tenant_id: your-azure-tenant-id
        client_id: your-azure-client-id
        client_secret: your-azure-secret

authorization:
  roles:
    admin:
      permissions:
        - "read:*"
        - "write:*"
        - "delete:*"
        - "manage:*"
    developer:
      permissions:
        - "read:resources"
        - "write:resources"
        - "read:prompts"
        - "write:prompts"
    user:
      permissions:
        - "read:resources"
        - "write:resources"
  inheritance:
    developer: inherits_from: [user]
    admin: inherits_from: [developer]
```

#### Database Configuration

```yaml
# database.yaml
postgresql:
  host: postgres.example.com
  port: 5432
  name: erlmcp_prod
  user: erlmcp
  password: ${DB_PASSWORD}
  pool:
    size: 20
    timeout: 30
    max_overflow: 10
  replication:
    enabled: true
    mode: async
    standby_hosts:
      - standby1.example.com
      - standby2.example.com

redis:
  host: redis.example.com
  port: 6379
  password: ${REDIS_PASSWORD}
  pool:
    size: 10
    timeout: 30
  persistence:
    enabled: true
    save_policy:
      - interval: 3600
        changes: 1
```

### Configuration Management Best Practices

1. **Use Environment Variables**
   ```bash
   # For sensitive data
   export ERLMCP_DB_PASSWORD="secure-password"
   export ERLMCP_REDIS_PASSWORD="redis-password"

   # Include in configuration
   # config.yaml
   database:
     password: ${ERLMCP_DB_PASSWORD}
   ```

2. **Version Control Configuration**
   ```bash
   # Git repository for configuration
   git config --global user.name "Erlang Admin"
   git config --global user.email "admin@erlmcp.com"

   # Initialize repository
   cd /etc/erlmcp
   git init
   git add .
   git commit -m "Initial configuration"
   ```

3. **Configuration Validation**
   ```bash
   # Validate configuration
   erlmcp config validate --config /etc/erlmcp/config.yaml

   # Check syntax
   erlmcp config check --syntax
   ```

## Monitoring and Alerting

### Monitoring Setup

#### Prometheus Configuration

```yaml
# prometheus.yml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'erlmcp'
    static_configs:
      - targets: ['localhost:9090']
    scrape_interval: 15s
    metrics_path: /metrics
    scheme: http

  - job_name: 'postgres'
    static_configs:
      - targets: ['postgres:5432']
    scrape_interval: 30s

  - job_name: 'redis'
    static_configs:
      - targets: ['redis:6379']
    scrape_interval: 30s
```

#### Alert Manager Configuration

```yaml
# alertmanager.yml
global:
  smtp_smarthost: 'smtp.example.com:587'
  smtp_from: 'alerts@erlmcp.com'

route:
  group_by: ['alertname', 'cluster']
  group_wait: 10s
  group_interval: 10s
  repeat_interval: 1h
  receiver: 'web.hook'

receivers:
- name: 'web.hook'
  webhook_configs:
  - url: 'http://localhost:5001/'
```

#### Grafana Dashboard

```json
{
  "dashboard": {
    "title": "erlmcp Overview",
    "panels": [
      {
        "title": "HTTP Requests",
        "type": "graph",
        "targets": [
          {
            "expr": "sum(rate(http_requests_total[5m]))",
            "legendFormat": "Total Requests"
          }
        ]
      },
      {
        "title": "Active Sessions",
        "type": "singlestat",
        "targets": [
          {
            "expr": "erlmcp_sessions_active",
            "legendFormat": "Active Sessions"
          }
        ]
      }
    ]
  }
}
```

### System Health Checks

#### Health Check Script

```bash
#!/bin/bash
# health-check.sh

# Check erlmcp service
if ! curl -f http://localhost:8080/v3/health > /dev/null 2>&1; then
    echo "erlmcp service is down"
    exit 1
fi

# Check database
if ! pg_isready -h localhost -U erlmcp > /dev/null 2>&1; then
    echo "Database is not ready"
    exit 1
fi

# Check Redis
if ! redis-cli ping > /dev/null 2>&1; then
    echo "Redis is not running"
    exit 1
fi

echo "All services are healthy"
```

### Monitoring Metrics

#### Key Metrics to Monitor

| Metric | Description | Warning | Critical |
|--------|-------------|---------|----------|
| `erlmcp_sessions_active` | Active sessions | 4000 | 5000 |
| `http_requests_total` | Total HTTP requests | N/A | N/A |
| `http_request_duration_seconds` | Request latency | 0.5s | 1s |
| `database_connections` | Database connections | 15 | 20 |
| `redis_memory_usage` | Redis memory usage | 80% | 90% |

#### Alert Rules

```yaml
groups:
- name: erlmcp_alerts
  rules:
  - alert: HighCPUUsage
    expr: rate(container_cpu_usage_seconds_total[5m]) > 0.8
    for: 10m
    labels:
      severity: critical
    annotations:
      summary: "High CPU usage"
      description: "CPU usage is {{ $value }} for 10 minutes"

  - alert: HighMemoryUsage
    expr: container_memory_usage_bytes / container_spec_memory_limit_bytes > 0.9
    for: 5m
    labels:
      severity: critical
    annotations:
      summary: "High memory usage"
      description: "Memory usage is {{ $value }}"
```

## Security Management

### Authentication Configuration

#### JWT Setup

```bash
# Generate JWT key pair
openssl genrsa -out jwt_private.pem 2048
openssl rsa -in jwt_private.pem -pubout -out jwt_public.pem

# Configure JWT
cat > jwt-config.yaml << EOF
authentication:
  jwt:
    algorithm: RS256
    private_key: /etc/erlmcp/jwt_private.pem
    public_key: /etc/erlmcp/jwt_public.pem
    expiration: 3600
    refresh_expiration: 86400
EOF
```

#### OAuth2 Provider Setup

```bash
# Google OAuth2
cat > oauth2-google.yaml << EOF
authentication:
  oauth2:
    providers:
      google:
        client_id: your-google-client-id
        client_secret: your-google-secret
        discovery_url: https://accounts.google.com/.well-known/openid-configuration
        scopes:
          - openid
          - profile
          - email
        redirect_uri: https://api.erlmcp.com/auth/callback
EOF
```

### Access Control

#### Role Management

```bash
# Create role
erlmcp auth create-role admin \
  --permissions "read:*" "write:*" "delete:*" "manage:*"

# Create user
erlmcp auth create-user john.doe \
  --email john.doe@example.com \
  --password temporary-password \
  --roles admin

# Assign role
erlmcp auth assign-role john.doe admin
```

#### API Key Management

```bash
# Create API key
erlmcp auth create-api-key my-service \
  --description "Service API key" \
  --permissions "read:resources" "write:resources"

# List API keys
erlmcp auth list-api-keys

# Revoke API key
erlmcp auth revoke-api-key key-12345
```

### Security Auditing

#### Audit Log Configuration

```yaml
# audit.yaml
audit:
  enabled: true
  log_file: /var/log/erlmcp/audit.log
  retention: 90
  events:
    - authentication.success
    - authentication.failure
    - permission.denied
    - data.access
    - configuration.change
```

#### Security Scanning

```bash
# Regular security scan
erlmcp security scan --full

# Vulnerability check
erlmcp security check-vulnerabilities

# Compliance check
erlmcp security check-compliance --framework pci-dss
```

## Backup and Recovery

### Backup Strategy

#### Configuration Backup

```bash
#!/bin/bash
# backup-config.sh

BACKUP_DIR="/var/backups/erlmcp"
DATE=$(date +%Y%m%d_%H%M%S)

# Create backup directory
mkdir -p $BACKUP_DIR/$DATE

# Backup configuration
cp -r /etc/erlmcp $BACKUP_DIR/$DATE/config

# Backup database
pg_dump -h localhost -U erlmcp erlmcp > $BACKUP_DIR/$DATE/database.sql

# Backup Redis
redis-cli BGREWRITEAOF
cp /var/lib/redis/dump.rdb $BACKUP_DIR/$DATE/redis.rdb

# Compress backup
tar -czf $BACKUP_DIR/backup_$DATE.tar.gz -C $BACKUP_DIR $DATE

# Clean up old backups
find $BACKUP_DIR -name "backup_*.tar.gz" -mtime +30 -delete

echo "Backup completed: $BACKUP_DIR/backup_$DATE.tar.gz"
```

#### Automated Backup

```yaml
# backup.yaml
backup:
  schedule:
    daily: "0 2 * * *"
    weekly: "0 3 * * 0"
    monthly: "0 4 1 * *"

  retention:
    daily: 7
    weekly: 4
    monthly: 12

  locations:
    - type: local
      path: /var/backups/erlmcp
    - type: s3
      bucket: erlmcp-backups
      region: us-east-1
    - type: azure
      account: erlmcpstorage
      container: backups
```

### Recovery Procedures

#### Database Recovery

```bash
#!/bin/bash
# restore-database.sh

BACKUP_FILE=$1
if [ -z "$BACKUP_FILE" ]; then
    echo "Usage: $0 <backup-file>"
    exit 1
fi

# Stop erlmcp service
sudo systemctl stop erlmcp

# Restore database
psql -h localhost -U erlmcp -f $BACKUP_FILE/database.sql

# Start erlmcp service
sudo systemctl start erlmcp

# Verify data
psql -h localhost -U erlmcp -c "SELECT COUNT(*) FROM sessions;"
```

#### Full System Recovery

```bash
#!/bin/bash
# restore-system.sh

BACKUP_FILE=$1

# Extract backup
tar -xzf $BACKUP_FILE -C /tmp

# Restore configuration
cp -r /tmp/erlmcp/config/* /etc/erlmcp/

# Restore database
psql -h localhost -U erlmcp -f /tmp/erlmcp/database.sql

# Restore Redis
cp /tmp/erlmcp/redis.rdb /var/lib/redis/dump.rdb
redis-cli BGREWRITEAOF

# Restart services
systemctl restart erlmcp postgres redis

echo "System recovery completed"
```

## Performance Tuning

### System Configuration

#### Erlang VM Tuning

```erlang
# vm.config
[
  % Erlang VM tuning
  {kernel, [
    {inet_default_connect_options, [{nodelay, true}]},
    {inet_default_listen_options, [{nodelay, true}, {reuseaddr, true}]}
  ]},

  % ETS optimization
  {erlmcp, [
    {ets_cache_size, 1048576},  % 1M entries
    {connection_pool_size, 100},
    {max_processes, 10000}
  ]}
].
```

#### Database Tuning

```sql
-- PostgreSQL optimization
ALTER SYSTEM SET max_connections = 100;
ALTER SYSTEM SET shared_preload_libraries = 'pg_stat_statements';
ALTER SYSTEM SET effective_cache_size = 4GB;

-- Create indexes
CREATE INDEX idx_sessions_user_id ON sessions(user_id);
CREATE INDEX idx_sessions_created_at ON sessions(created_at);

-- Configure query tuning
ALTER SYSTEM SET work_mem = '64MB';
ALTER SYSTEM SET maintenance_work_mem = '256MB';
```

### Performance Monitoring

#### Load Testing

```bash
# Load test script
#!/bin/bash
# load-test.sh

DURATION=${1:-60}
USERS=${2:-1000}
RPS=${3:-5000}

echo "Starting load test for $DURATION seconds with $USERS users at $RPS rps"

# Use Apache Bench or k6
ab -n $USERS -c 100 -t $DURATION http://localhost:8080/v3/health

# Or with k6
k6 run --duration $DURATIONs --vus $USERS -r $RPS load-test.js
```

#### Performance Analysis

```erlang
% Performance analysis module
-module(erlmcp_performance).

-export([analyze/0, find_bottlenecks/0]).

analyze() ->
    % Get memory usage
    Memory = erlang:memory(),

    % Get process count
    Processes = erlang:system_info(process_count),

    % Get queue lengths
    QueueLengths = get_queue_lengths(),

    % Analyze database
    DatabaseStats = get_database_stats(),

    #{
        memory => Memory,
        processes => Processes,
        queues => QueueLengths,
        database => DatabaseStats
    }.

find_bottlenecks() ->
    % Check long-running processes
    LongRunning = find_long_running_processes(),

    % Check database queries
    SlowQueries = find_slow_queries(),

    % Check memory usage
    HighMemory = find_high_memory_processes(),

    #{
        long_running => LongRunning,
        slow_queries => SlowQueries,
        high_memory => HighMemory
    }.
```

## Troubleshooting

### Common Issues

#### Service Won't Start

```bash
# Check logs
journalctl -u erlmcp -n 100

# Check configuration
erlmcp config validate

# Check dependencies
systemctl status postgres
systemctl status redis
```

#### High Memory Usage

```bash
# Check memory usage
ps aux | grep erlang

# Check Erlang processes
erl -pa /app/ebin -eval "recon:memory(processes)," -s init stop

# Find memory leaks
erl -pa /app/ebin -eval "recon_trace:calls({erlmcp_server, handle_request, 2}, 1000)," -s init stop
```

#### Database Connection Issues

```bash
# Check database connections
pg_stat_activity

# Check pool status
psql -U erlmcp -c "SELECT * FROM pg_stat_activity WHERE state != 'idle';"

# Optimize connections
ALTER SYSTEM SET max_connections = 100;
```

### Diagnostic Tools

```bash
# System diagnostics
erlmcp diagnose --full

# Log analysis
erlmcp log analyze --file /var/log/erlmcp/error.log

# Performance analysis
erlmcp performance analyze
```

### Performance Debugging

```erlang
% Debug logging
logger:set_application_level(erlmcp, debug).

% Query debugging
sql:log_queries(true).

% Message tracing
tracer:start(),
tracer:process(self()).
```

## Maintenance Procedures

### Regular Maintenance

#### Daily Tasks

```bash
# Daily maintenance script
#!/bin/bash
# daily-maintenance.sh

# Check system health
./health-check.sh

# Rotate logs
logrotate -f /etc/logrotate.d/erlmcp

# Clean temporary files
find /tmp -name "erlmcp_*" -mtime +1 -delete

# Check disk space
df -h | grep -E "Filesystem|/var/lib/erlmcp"
```

#### Weekly Tasks

```bash
# Weekly maintenance
#!/bin/bash
# weekly-maintenance.sh

# Update packages
apt update && apt upgrade -y

# Clean old backups
find /var/backups/erlmcp -name "*.tar.gz" -mtime +30 -delete

# Optimize database
psql -U erlmcp -c "VACUUM ANALYZE;"

# Check security updates
erlmcp security check-updates
```

#### Monthly Tasks

```bash
# Monthly maintenance
#!/bin/bash
# monthly-maintenance.sh

# Full system backup
./backup-full.sh

# Performance review
erlmcp performance report

# Security audit
erlmcp security audit

# Update documentation
./update-documentation.sh
```

### Scheduled Maintenance Windows

```yaml
# maintenance.yaml
schedule:
  daily:
    time: "02:00"
    tasks:
      - health_check
      - log_rotation
      - temp_cleanup

  weekly:
    day: "sunday"
    time: "03:00"
    tasks:
      - package_update
      - database_optimization
      - security_scan

  monthly:
    day: "1"
    time: "04:00"
    tasks:
      - full_backup
      - performance_review
      - documentation_update
      - system_audit
```

### Change Management

#### Change Request Process

```bash
# Submit change request
erlmcp change create \
  --title "Database upgrade to PostgreSQL 16" \
  --description "Upgrade database to latest version" \
  --type "maintenance" \
  --impact "medium" \
  --plan "1. Backup database\n2. Upgrade\n3. Test\n4. Monitor"

# Review change
erlmcp change review 123

# Implement change
erlmcp change implement 123

# Verify change
erlmcp change verify 123
```

#### Rollback Procedures

```bash
# Emergency rollback
erlmcp rollback emergency --change 123

# Scheduled rollback
erlmcp rollback planned --change 123 --time "2024-02-01 20:00"

# Validate rollback
erlmcp rollback verify
```

## Advanced Topics

### Multi-Region Deployment

#### Architecture Overview

```mermaid
graph TD
    subgraph "Region 1"
        A[erlmcp@node1]
        B[erlmcp@node2]
        C[Database]
    end

    subgraph "Region 2"
        D[erlmcp@node3]
        E[erlmcp@node4]
        F[Database]
    end

    subgraph "Global"
        G[Load Balancer]
        H[DNS]
    end

    A --> C
    B --> C
    D --> F
    E --> F
    G --> A
    G --> D
    H --> G
```

#### Configuration

```yaml
# multi-region.yaml
regions:
  primary:
    name: us-east-1
    nodes:
      - erlmcp@node1.us-east-1.example.com
      - erlmcp@node2.us-east-1.example.com
    database:
      host: db-primary.example.com
      read_replica: true

  secondary:
    name: us-west-2
    nodes:
      - erlmcp@node3.us-west-2.example.com
      - erlmcp@node4.us-west-2.example.com
    database:
      host: db-secondary.example.com
      read_replica: false

load_balancer:
  strategy: round_robin
  health_check_interval: 30s
  failover_threshold: 3
```

### High Availability Setup

#### Cluster Configuration

```yaml
# ha.yaml
cluster:
  name: erlmcp-ha
  cookie: ha-cluster-cookie
  discovery:
    method: kubernetes
    namespace: erlmcp

  nodes:
    - erlmcp@node1
    - erlmcp@node2
    - erlmcp@node3

  failover:
    enabled: true
    timeout: 30s
    max_retries: 3

  load_balancer:
    type: haproxy
    check_interval: 10s
    failover_timeout: 60s
```

#### Database Replication

```sql
-- Set up replication
ALTER SYSTEM wal_level = replica;
ALTER SYSTEM max_wal_senders = 5;
ALTER SYSTEM max_replication_slots = 5;

-- Create replication user
CREATE USER replicator WITH REPLICATION PASSWORD 'secure-password';

-- Grant access
GRANT CONNECT ON DATABASE erlmcp TO replicator;
GRANT REPLICATION ON DATABASE erlmcp TO replicator;
```

### Performance Optimization

#### Caching Strategy

```yaml
# caching.yaml
caching:
  redis:
    enabled: true
    strategy: write_through
    ttl: 3600
    size: 1GB

  local:
    enabled: true
    strategy: lru
    size: 512MB
    ttl: 300

  distributed:
    enabled: true
    strategy: consistent_hash
    nodes:
      - node1:11211
      - node2:11211
      - node3:11211
```

#### Connection Pooling

```yaml
# pooling.yaml
pools:
  database:
    size: 20
    max_overflow: 10
    timeout: 30
    idle_timeout: 300
    recycle: 1000

  redis:
    size: 10
    max_overflow: 5
    timeout: 30
    idle_timeout: 300
    recycle: 500

  http:
    size: 50
    max_overflow: 20
    timeout: 60
    idle_timeout: 60
    retry_attempts: 3
```

### Integration with External Systems

#### Monitoring Integration

```yaml
# monitoring-integration.yaml
integrations:
  prometheus:
    enabled: true
    port: 9090
    path: /metrics
    labels:
      cluster: erlmcp-prod
      region: us-east-1

  datadog:
    enabled: true
    api_key: ${DATADOG_API_KEY}
    tags:
      env: production
      cluster: erlmcp-prod

  newrelic:
    enabled: true
    license_key: ${NEWRELIC_LICENSE_KEY}
    app_name: erlmcp-prod
```

#### Logging Integration

```yaml
# logging-integration.yaml
integrations:
  elasticsearch:
    enabled: true
    hosts:
      - https://elasticsearch.example.com:9200
    index: erlmcp-logs
    username: ${ELASTICSEARCH_USER}
    password: ${ELASTICSEARCH_PASSWORD}

  splunk:
    enabled: true
    host: splunk.example.com
    port: 8088
    token: ${SPLUNK_TOKEN}
    index: erlmcp-logs
```

---
*Training Materials Version: 3.0.0*
*Last Updated: February 2024*