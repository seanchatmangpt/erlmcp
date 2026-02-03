#!/bin/bash

# Enterprise Monitoring Stack Setup Script for erlmcp v3
# This script sets up the complete monitoring stack including Prometheus, Grafana,
# Loki, Alertmanager, and all necessary components for Fortune 500 scale monitoring.

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Log function
log() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if running as root
if [ "$EUID" -eq 0 ]; then
    log_error "Please run this script as a non-root user with sudo privileges"
    exit 1
fi

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check prerequisites
check_prerequisites() {
    log "Checking prerequisites..."

    if ! command_exists docker; then
        log_error "Docker is not installed. Please install Docker first."
        exit 1
    fi

    if ! command_exists docker-compose; then
        log_error "Docker Compose is not installed. Please install Docker Compose first."
        exit 1
    fi

    if ! command_exists curl; then
        log_error "curl is not installed. Please install curl first."
        exit 1
    fi

    log_success "All prerequisites are satisfied"
}

# Create necessary directories
create_directories() {
    log "Creating necessary directories..."

    # Create monitoring directories
    mkdir -p /opt/erlmcp-monitoring/{prometheus,grafana,loki,alertmanager,config}

    # Create data directories
    mkdir -p /opt/erlmcp-monitoring/prometheus/data
    mkdir -p /opt/erlmcp-monitoring/grafana/data
    mkdir -p /opt/erlmcp-monitoring/loki/data
    mkdir -p /opt/erlmcp-monitoring/alertmanager/data

    # Create log directories
    mkdir -p /var/log/erlmcp/{application,error,security,audit,access}

    log_success "Directories created successfully"
}

# Download Docker Compose file
download_docker_compose() {
    log "Downloading Docker Compose configuration..."

    # Create docker-compose.yml
    cat > /opt/erlmcp-monitoring/docker-compose.yml << 'EOF'
version: '3.8'

services:
  # Prometheus server
  prometheus:
    image: prom/prometheus:latest
    container_name: erlmcp-prometheus
    ports:
      - "9090:9090"
    volumes:
      - ./prometheus:/etc/prometheus
      - prometheus_data:/prometheus
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'
      - '--web.console.libraries=/etc/prometheus/console_libraries'
      - '--web.console.templates=/etc/prometheus/consoles'
      - '--web.enable-lifecycle'
      - '--web.route-prefix=/'
    restart: unless-stopped
    networks:
      - monitoring

  # Grafana
  grafana:
    image: grafana/grafana:latest
    container_name: erlmcp-grafana
    ports:
      - "3000:3000"
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=admin123
      - GF_USERS_ALLOW_SIGN_UP=false
    volumes:
      - ./grafana:/etc/grafana/provisioning
      - grafana_data:/var/lib/grafana
    restart: unless-stopped
    networks:
      - monitoring

  # Loki
  loki:
    image: grafana/loki:latest
    container_name: erlmcp-loki
    ports:
      - "3100:3100"
    volumes:
      - ./loki:/etc/loki
      - loki_data:/loki
    command:
      - '-config.file=/etc/loki/loki-config.yml'
    restart: unless-stopped
    networks:
      - monitoring

  # Promtail
  promtail:
    image: grafana/promtail:latest
    container_name: erlmcp-promtail
    volumes:
      - ./promtail:/etc/promtail
      - /var/log/erlmcp:/var/log/erlmcp
      - /var/log:/var/log:ro
    command:
      - '-config.file=/etc/promtail/promtail-config.yml'
    restart: unless-stopped
    networks:
      - monitoring

  # Alertmanager
  alertmanager:
    image: prom/alertmanager:latest
    container_name: erlmcp-alertmanager
    ports:
      - "9093:9093"
    volumes:
      - ./alertmanager:/etc/alertmanager
      - alertmanager_data:/alertmanager
    command:
      - '--config.file=/etc/alertmanager/alertmanager.yml'
      - '--storage.path=/alertmanager'
      - '--web.route-prefix=/'
    restart: unless-stopped
    networks:
      - monitoring

  # Node Exporter (for system metrics)
  node-exporter:
    image: prom/node-exporter:latest
    container_name: erlmcp-node-exporter
    ports:
      - "9100:9100"
    restart: unless-stopped
    networks:
      - monitoring

  # cAdvisor (for container metrics)
  cadvisor:
    image: gcr.io/cadvisor/cadvisor:latest
    container_name: erlmcp-cadvisor
    ports:
      - "8080:8080"
    volumes:
      - /:/rootfs:ro
      - /var/run:/var/run:ro
      - /sys:/sys:ro
      - /var/lib/docker/:/var/lib/docker:ro
    restart: unless-stopped
    networks:
      - monitoring

networks:
  monitoring:
    driver: bridge

volumes:
  prometheus_data:
  grafana_data:
  loki_data:
  alertmanager_data:
EOF

    log_success "Docker Compose configuration downloaded"
}

# Configure Prometheus
configure_prometheus() {
    log "Configuring Prometheus..."

    # Create prometheus.yml
    cat > /opt/erlmcp-monitoring/prometheus/prometheus.yml << 'EOF'
global:
  scrape_interval: 15s
  evaluation_interval: 15s
  external_labels:
    monitor: 'erlmcp-enterprise'
    cluster: 'production'

rule_files:
  - "alert_rules.yml"
  - "recording_rules.yml"

scrape_configs:
  # erlmcp Core Metrics
  - job_name: 'erlmcp-core'
    static_configs:
      - targets: ['erlmcp-core:9090']
        labels:
          component: 'core'
          service: 'erlmcp'
    metrics_path: '/metrics'
    scrape_interval: 15s
    scrape_timeout: 10s
    scheme: http
    honor_labels: false

  # erlmcp Transport Metrics
  - job_name: 'erlmcp-transports'
    static_configs:
      - targets: ['erlmcp-transports:9090']
        labels:
          component: 'transports'
          service: 'erlmcp'
    metrics_path: '/metrics'
    scrape_interval: 15s
    scrape_timeout: 10s
    scheme: http
    honor_labels: false

  # erlmcp Session Metrics
  - job_name: 'erlmcp-sessions'
    static_configs:
      - targets: ['erlmcp-sessions:9090']
        labels:
          component: 'sessions'
          service: 'erlmcp'
    metrics_path: '/metrics'
    scrape_interval: 15s
    scrape_timeout: 10s
    scheme: http
    honor_labels: false

  # erlmcp Registry Metrics
  - job_name: 'erlmcp-registry'
    static_configs:
      - targets: ['erlmcp-registry:9090']
        labels:
          component: 'registry'
          service: 'erlmcp'
    metrics_path: '/metrics'
    scrape_interval: 15s
    scrape_timeout: 10s
    scheme: http
    honor_labels: false

  # erlmcp Authentication Metrics
  - job_name: 'erlmcp-auth'
    static_configs:
      - targets: ['erlmcp-auth:9090']
        labels:
          component: 'auth'
          service: 'erlmcp'
    metrics_path: '/metrics'
    scrape_interval: 15s
    scrape_timeout: 10s
    scheme: http
    honor_labels: false

  # erlmcp Observability Metrics
  - job_name: 'erlmcp-observability'
    static_configs:
      - targets: ['erlmcp-observability:9090']
        labels:
          component: 'observability'
          service: 'erlmcp'
    metrics_path: '/metrics'
    scrape_interval: 15s
    scrape_timeout: 10s
    scheme: http
    honor_labels: false

  # erlmcp Validation Metrics
  - job_name: 'erlmcp-validation'
    static_configs:
      - targets: ['erlmcp-validation:9090']
        labels:
          component: 'validation'
          service: 'erlmcp'
    metrics_path: '/metrics'
    scrape_interval: 15s
    scrape_timeout: 10s
    scheme: http
    honor_labels: false

  # Node Exporter for System Metrics
  - job_name: 'node-exporter'
    static_configs:
      - targets: ['node-exporter:9100']
        labels:
          component: 'infrastructure'
    metrics_path: '/metrics'
    scrape_interval: 15s
    scrape_timeout: 10s
    scheme: http
    honor_labels: false

  # cAdvisor for Container Metrics
  - job_name: 'cadvisor'
    static_configs:
      - targets: ['cadvisor:8080']
        labels:
          component: 'containers'
    metrics_path: '/metrics'
    scrape_interval: 15s
    scrape_timeout: 10s
    scheme: http
    honor_labels: false

  # Blackbox Exporter for External Checks
  - job_name: 'blackbox'
    metrics_path: /probe
    params:
      module:
        - http_2xx
        - http_post_2xx
        - tcp_connect
    static_configs:
      - targets:
          - http://erlmcp.example.com/health
          - https://api.erlmcp.example.com/health
          - tcp://erlmcp-db.example.com:3306
    relabel_configs:
      - source_labels: [__address__]
        target_label: __param_target
      - source_labels: [__param_target]
        target_label: instance
      - target_label: __address__
        replacement: blackbox:9115
EOF

    # Create alert rules
    cat > /opt/erlmcp-monitoring/prometheus/alert_rules.yml << 'EOF'
groups:
  - name: erlmcp.rules
    interval: 15s
    rules:
      # High request rate alerts
      - alert: HighRequestRate
        expr: rate(erlmcp_requests_total[5m]) > 10000
        for: 5m
        labels:
          severity: warning
          category: performance
        annotations:
          summary: "High request rate detected on erlmcp instance"
          description: "Request rate is {{ $value }} requests/second for more than 5 minutes"
          runbook_url: "https://wiki.erlmcp.com/docs/runbooks/high-request-rate"

      # High error rate alerts
      - alert: HighErrorRate
        expr: rate(erlmcp_errors_total[5m]) / rate(erlmcp_requests_total[5m]) > 0.05
        for: 5m
        labels:
          severity: critical
          category: errors
        annotations:
          summary: "High error rate detected on erlmcp instance"
          description: "Error rate is {{ $value }} (5%) for more than 5 minutes"
          runbook_url: "https://wiki.erlmcp.com/docs/runbooks/high-error-rate"

      # High latency alerts
      - alert: HighLatency
        expr: histogram_quantile(0.95, rate(erlmcp_request_duration_seconds_bucket[5m])) > 1
        for: 5m
        labels:
          severity: warning
          category: performance
        annotations:
          summary: "High request latency detected"
          description: "95th percentile latency is {{ $value }} seconds for more than 5 minutes"
          runbook_url: "https://wiki.erlmcp.com/docs/runbooks/high-latency"

      # Low availability alerts
      - alert: LowAvailability
        expr: (1 - rate(erlmcp_requests_total[5m]) / rate(erlmcp_requests_total[5m])) > 0.01
        for: 2m
        labels:
          severity: critical
          category: availability
        annotations:
          summary: "Low availability detected"
          description: "Availability dropped to {{ $value }}% for more than 2 minutes"
          runbook_url: "https://wiki.erlmcp.com/docs/runbooks/low-availability"

      # Memory usage alerts
      - alert: HighMemoryUsage
        expr: (erlmcp_memory_total_bytes / 1073741824) > 8
        for: 5m
        labels:
          severity: warning
          category: infrastructure
        annotations:
          summary: "High memory usage detected"
          description: "Memory usage is {{ $value }} GB for more than 5 minutes"
          runbook_url: "https://wiki.erlmcp.com/docs/runbooks/high-memory-usage"

      # Connection limit alerts
      - alert: ConnectionLimitReached
        expr: erlmcp_connections_total > 50000
        for: 5m
        labels:
          severity: warning
          category: capacity
        annotations:
          summary: "Connection limit reached"
          description: "Connections reached {{ $value }} (limit: 50000)"
          runbook_url: "https://wiki.erlmcp.com/docs/runbooks/connection-limit"

      # Security alerts
      - alert: HighSecurityEvents
        expr: rate(erlmcp_security_events_total[5m]) > 10
        for: 2m
        labels:
          severity: warning
          category: security
        annotations:
          summary: "High number of security events detected"
          description: "Security events rate is {{ $value }} events/minute"
          runbook_url: "https://wiki.erlmcp.com/docs/runbooks/security-events"

      # Authentication failures
      - alert: AuthenticationFailures
        expr: rate(erlmcp_auth_failures_total[5m]) > 5
        for: 5m
        labels:
          severity: critical
          category: security
        annotations:
          summary: "High authentication failure rate detected"
          description: "Authentication failure rate is {{ $value }} failures/minute"
          runbook_url: "https://wiki.erlmcp.com/docs/runbooks/auth-failures"

      # Health check failures
      - alert: HealthCheckFailure
        expr: erlmcp_healthy == 0
        for: 30s
        labels:
          severity: critical
          category: health
        annotations:
          summary: "erlmcp health check failed"
          description: "erlmcp instance is unhealthy for more than 30 seconds"
          runbook_url: "https://wiki.erlmcp.com/docs/runbooks/health-check"
EOF

    # Create recording rules
    cat > /opt/erlmcp-monitoring/prometheus/recording_rules.yml << 'EOF'
groups:
  - name: erlmcp.recording
    interval: 15s
    rules:
      # Business metrics recording rules
      - record: erlmcp_request_rate
        expr: rate(erlmcp_requests_total[5m])

      - record: erlmcp_error_rate
        expr: rate(erlmcp_errors_total[5m]) / rate(erlmcp_requests_total[5m])

      - record: erlmcp_success_rate
        expr: 1 - erlmcp_error_rate

      - record: erlmcp_avg_request_duration
        expr: histogram_quantile(0.5, rate(erlmcp_request_duration_seconds_bucket[5m]))

      - record: erlmcp_p95_request_duration
        expr: histogram_quantile(0.95, rate(erlmcp_request_duration_seconds_bucket[5m]))

      - record: erlmcp_p99_request_duration
        expr: histogram_quantile(0.99, rate(erlmcp_request_duration_seconds_bucket[5m]))

      - record: erlmcp_connections_active
        expr: erlmcp_connections_total - erlmcp_connections_closed_total

      - record: erlmcp_sessions_active
        expr: erlmcp_active_sessions

      - record: erlmcp_throughput
        expr: rate(erlmcp_requests_total[1m])

      # Performance metrics
      - record: erlmcp_queue_backlog
        expr: erlmcp_queue_size

      - record: erlmcp_process_count
        expr: erlmcp_process_count

      - record: erlmcp_memory_usage
        expr: erlmcp_memory_total_bytes / 1073741824  # Convert to GB

      # Security metrics
      - record: erlmcp_security_event_rate
        expr: rate(erlmcp_security_events_total[5m])

      - record: erlmcp_auth_failure_rate
        expr: rate(erlmcp_auth_failures_total[5m])

      # SLA metrics
      - record: erlmcp_sla_uptime
        expr: 1 - rate(erlmcp_downtime_seconds_total[1h])

      - record: erlmcp_sla_response_time
        expr: erlmcp_p95_request_duration

      - record: erlmcp_sla_error_rate
        expr: erlmcp_error_rate
EOF

    log_success "Prometheus configured"
}

# Configure Grafana
configure_grafana() {
    log "Configuring Grafana..."

    # Create provisioning directories
    mkdir -p /opt/erlmcp-monitoring/grafana/{dashboards,datasources,provisioning}

    # Create datasources provisioning
    mkdir -p /opt/erlmcp-monitoring/grafana/provisioning/datasources
    cat > /opt/erlmcp-monitoring/grafana/provisioning/datasources/prometheus.yml << 'EOF'
apiVersion: 1

datasources:
  - name: Prometheus
    type: prometheus
    access: proxy
    orgId: 1
    url: http://prometheus:9090
    basicAuth: false
    isDefault: true
    version: 1
    editable: false
    jsonData:
      httpMethod: POST
      queryTimeout: 60s
      timeInterval: 15s
EOF

    # Create dashboard provisioning
    mkdir -p /opt/erlmcp-monitoring/grafana/provisioning/dashboards
    cat > /opt/erlmcp-monitoring/grafana/provisioning/dashboards/erlmcp-dashboard.yml << 'EOF'
apiVersion: 1

providers:
  - name: 'erlmcp-dashboards'
    orgId: 1
    folder: 'erlmcp'
    type: file
    disableDeletion: false
    updateIntervalSeconds: 10
    allowUiUpdates: true
    options:
      path: /etc/grafana/provisioning/dashboards
EOF

    # Copy enterprise dashboard
    cp /Users/sac/erlmcp/apps/erlmcp_observability/grafana/erlmcp-enterprise-dashboard.json \
       /opt/erlmcp-monitoring/grafana/provisioning/dashboards/

    log_success "Grafana configured"
}

# Configure Loki
configure_loki() {
    log "Configuring Loki..."

    # Create loki-config.yml
    cat > /opt/erlmcp-monitoring/loki/loki-config.yml << 'EOF'
auth_enabled: false

server:
  http_listen_port: 3100
  grpc_listen_port: 9096

common:
  storage:
    type: 'filesystem'
    filesystem:
      directory: /loki

  replication_factor: 1
  query_timeout: 5m
  query_timeout: 5m

  # Ruler configuration
  ruler:
    alertmanager_url: http://alertmanager:9093
    ring:
      kvstore:
        store: memberlist
    enable_api: true
    enable_alertmanager_v2: true

# Default limits
query_range:
  max_entries_per_query: 500000
  max_bytes_per_query: 1000000000  # 1GB

# Retention configuration
retention:
  # Keep logs for 30 days
  period: 720h

# Alerting configuration
alertmanager:
  enabled: true
  port: 9093
  external_url: http://localhost:9093
EOF

    # Create promtail-config.yml
    cat > /opt/erlmcp-monitoring/promtail/promtail-config.yml << 'EOF'
server:
  http_listen_port: 9080
  grpc_listen_port: 0

positions:
  filename: /data/positions.yaml

clients:
  - url: http://loki:3100/loki/api/v1/push

scrape_configs:
  # erlmcp Application Logs
  - job_name: erlmcp-logs
    static_configs:
      - targets: [localhost]
        labels:
          job: erlmcp-logs
          __path__: /var/log/erlmcp/application/*.log

  # erlmcp Error Logs
  - job_name: erlmcp-errors
    static_configs:
      - targets: [localhost]
        labels:
          job: erlmcp-errors
          __path__: /var/log/erlmcp/error/*.log

  # erlmcp Access Logs
  - job_name: erlmcp-access
    static_configs:
      - targets: [localhost]
        labels:
          job: erlmcp-access
          __path__: /var/log/erlmcp/access/*.log

  # erlmcp Security Logs
  - job_name: erlmcp-security
    static_configs:
      - targets: [localhost]
        labels:
          job: erlmcp-security
          __path__: /var/log/erlmcp/security/*.log

  # erlmcp Audit Logs
  - job_name: erlmcp-audit
    static_configs:
      - targets: [localhost]
        labels:
          job: erlmcp-audit
          __path__: /var/log/erlmcp/audit/*.log
EOF

    log_success "Loki configured"
}

# Configure Alertmanager
configure_alertmanager() {
    log "Configuring Alertmanager..."

    # Create alertmanager.yml
    cat > /opt/erlmcp-monitoring/alertmanager/alertmanager.yml << 'EOF'
global:
  smtp_smarthost: 'localhost:587'
  smtp_from: 'alerts@erlmcp.com'
  smtp_auth_username: 'alerts@erlmcp.com'
  smtp_auth_password: 'changeme'

# Templates for notifications
templates:
  - '/etc/alertmanager/templates/*.tmpl'

# Default receiver
default_receiver: 'web.hook'

# Inhibit rules to prevent notification storms
inhibit_rules:
  - source_match:
      severity: 'critical'
    target_match:
      severity: 'warning'
    equal: ['alertname', 'dev', 'instance']

# Route tree for notifications
route:
  group_by: ['alertname', 'cluster', 'service']
  group_wait: 10s
  group_interval: 10s
  repeat_interval: 12h
  receiver: 'web.hook'
  routes:
    # Critical alerts - immediate escalation
    - match:
        severity: 'critical'
      receiver: 'critical-alerts'
      group_wait: 5s
      repeat_interval: 5m

    # Warning alerts - team notifications
    - match:
        severity: 'warning'
      receiver: 'warning-alerts'
      group_wait: 30s
      repeat_interval: 15m

    # Security alerts - dedicated team
    - match_re:
        severity: '.*'
        category: 'security'
      receiver: 'security-alerts'
      group_wait: 10s
      repeat_interval: 10m

# Receiver definitions
receivers:
  # Critical alerts - pager duty
  - name: 'critical-alerts'
    email_configs:
      - to: 'on-call-engineer@erlmcp.com'
        subject: 'CRITICAL: {{ .GroupLabels.alertname }}'
        body: |
          {{ range .Alerts }}
          Alert: {{ .Annotations.summary }}
          Description: {{ .Annotations.description }}
          Labels: {{ .Labels }}
          {{ end }}
    webhook_configs:
      - url: 'https://hooks.slack.com/services/SLACK_WEBHOOK_URL'
        send_resolved: true
        http_config:
          headers:
            Content-Type: 'application/json'
        title: '🚨 CRITICAL ALERT: {{ .GroupLabels.alertname }}'
        text: |
          {{ range .Alerts }}
          {{ .Annotations.summary }}
          {{ .Annotations.description }}
          {{ end }}
        color: 'danger'

  # Warning alerts - Slack and email
  - name: 'warning-alerts'
    email_configs:
      - to: 'dev-team@erlmcp.com'
        subject: 'WARNING: {{ .GroupLabels.alertname }}'
        body: |
          {{ range .Alerts }}
          Alert: {{ .Annotations.summary }}
          Description: {{ .Annotations.description }}
          Labels: {{ .Labels }}
          {{ end }}
    webhook_configs:
      - url: 'https://hooks.slack.com/services/SLACK_WEBHOOK_URL'
        send_resolved: true
        http_config:
          headers:
            Content-Type: 'application/json'
        title: '⚠️ WARNING: {{ .GroupLabels.alertname }}'
        text: |
          {{ range .Alerts }}
          {{ .Annotations.summary }}
          {{ .Annotations.description }}
          {{ end }}
        color: 'warning'

  # Security alerts - security team
  - name: 'security-alerts'
    email_configs:
      - to: 'security-team@erlmcp.com'
        subject: 'SECURITY: {{ .GroupLabels.alertname }}'
        body: |
          {{ range .Alerts }}
          SECURITY ALERT: {{ .Annotations.summary }}
          Description: {{ .Annotations.description }}
          Labels: {{ .Labels }}
          {{ end }}
    webhook_configs:
      - url: 'https://hooks.slack.com/services/SECURITY_WEBHOOK_URL'
        send_resolved: true
        http_config:
          headers:
            Content-Type: 'application/json'
        title: '🔒 SECURITY ALERT: {{ .GroupLabels.alertname }}'
        text: |
          {{ range .Alerts }}
          {{ .Annotations.summary }}
          {{ .Annotations.description }}
          {{ end }}
        color: 'danger'

  # Webhook for general notifications
  - name: 'web.hook'
    webhook_configs:
      - url: 'http://localhost:5001/'
        send_resolved: true
        http_config:
          headers:
            Content-Type: 'application/json'
EOF

    # Create templates directory
    mkdir -p /opt/erlmcp-monitoring/alertmanager/templates

    log_success "Alertmanager configured"
}

# Setup systemd services
setup_systemd_services() {
    log "Setting up systemd services..."

    # Create erlmcp-monitoring.service
    sudo tee /etc/systemd/system/erlmcp-monitoring.service > /dev/null <<EOF
[Unit]
Description=erlmcp Enterprise Monitoring Stack
After=docker.service
Requires=docker.service

[Service]
Type=oneshot
RemainAfterExit=yes
WorkingDirectory=/opt/erlmcp-monitoring
ExecStart=/usr/local/bin/docker-compose -f /opt/erlmcp-monitoring/docker-compose.yml up -d
ExecStop=/usr/local/bin/docker-compose -f /opt/erlmcp-monitoring/docker-compose.yml down
TimeoutStartSec=0
Restart=no
User=root
Group=root

[Install]
WantedBy=multi-user.target
EOF

    # Create log rotation configuration
    sudo tee /etc/logrotate.d/erlmcp-monitoring > /dev/null <<EOF
/opt/erlmcp-monitoring/**/*.log {
    daily
    missingok
    rotate 30
    compress
    delaycompress
    notifempty
    create 644 root root
}

/var/log/erlmcp/**/*.log {
    daily
    missingok
    rotate 30
    compress
    delaycompress
    notifempty
    create 644 root root
    postrotate
        docker restart erlmcp-promtail || true
    endscript
}
EOF

    # Reload systemd
    sudo systemctl daemon-reload

    log_success "Systemd services configured"
}

# Create monitoring documentation
create_documentation() {
    log "Creating monitoring documentation..."

    # Create README
    cat > /opt/erlmcp-monitoring/README.md << 'EOF'
# erlmcp v3 Enterprise Monitoring Stack

This comprehensive monitoring stack provides enterprise-grade observability for erlmcp v3 deployments at Fortune 500 scale.

## Components

### 1. Prometheus
- **Purpose**: Metrics collection and storage
- **Port**: 9090
- **Configuration**: `/opt/erlmcp-monitoring/prometheus/prometheus.yml`
- **Features**:
  - Custom metrics for all erlmcp components
  - Alerting rules and recording rules
  - Remote write support for multi-cluster deployments
  - High-cardinality metric support

### 2. Grafana
- **Purpose**: Visualization and dashboards
- **Port**: 3000
- **URL**: http://localhost:3000
- **Credentials**: admin/admin123
- **Features**:
  - Pre-built enterprise dashboards
  - Real-time metrics visualization
  - Alert management
  - Reporting capabilities

### 3. Loki
- **Purpose**: Log aggregation and querying
- **Port**: 3100
- **Configuration**: `/opt/erlmcp-monitoring/loki/loki-config.yml`
- **Features**:
  - Log collection from all erlmcp components
  - Log correlation with metrics
  - High-performance log indexing
  - Long-term retention (30 days)

### 4. Alertmanager
- **Purpose**: Alert routing and management
- **Port**: 9093
- **Configuration**: `/opt/erlmcp-monitoring/alertmanager/alertmanager.yml`
- **Features**:
  - Multi-channel alert routing
  - Alert deduplication
  - Escalation policies
  - Integration with external systems

### 5. Prometheus Node Exporter
- **Purpose**: System metrics collection
- **Port**: 9100
- **Features**:
  - CPU, memory, disk, network metrics
  - Process metrics
  - System health monitoring

### 6. cAdvisor
- **Purpose**: Container metrics collection
- **Port**: 8080
- **Features**:
  - Container resource usage metrics
  - Container performance metrics
  - Historical container data

## Quick Start

1. **Start the monitoring stack**:
   ```bash
   sudo systemctl start erlmcp-monitoring
   ```

2. **Check status**:
   ```bash
   sudo systemctl status erlmcp-monitoring
   ```

3. **View metrics**:
   ```bash
   curl http://localhost:9090/metrics
   ```

4. **Access Grafana**:
   - Open http://localhost:3000
   - Login with admin/admin123
   - Import dashboard from erlmcp-enterprise-dashboard.json

5. **View logs**:
   ```bash
   curl http://localhost:3100/loki/api/v1/query_range?query={job="erlmcp-logs"}
   ```

## Configuration

### Custom Metrics
Custom metrics are exposed by erlmcp components at their `/metrics` endpoints. Key metrics include:
- `erlmcp_requests_total`: Total requests processed
- `erlmcp_error_rate`: Error rate percentage
- `erlmcp_p95_request_duration`: 95th percentile latency
- `erlmcp_active_sessions`: Number of active sessions
- `erlmcp_connections_total`: Total connections

### Alerting Rules
Alerting rules are defined in `prometheus/alert_rules.yml` and include:
- High request rate (>10k req/s)
- High error rate (>5%)
- High latency (>1s 95th percentile)
- Low availability (<99%)
- Memory usage (>8GB)
- Security events (>10/min)

### SLA Monitoring
SLA monitoring tracks:
- Uptime: 99.9% target
- Response time: 100ms 95th percentile target
- Error rate: <1% target
- Throughput: >10k req/s target

## Scaling

### Horizontal Scaling
- Prometheus: Multiple instances with Thanos
- Loki: Multiple instances with distributor/query/router
- Grafana: Multiple instances with load balancing
- Alertmanager: Multiple instances for high availability

### Vertical Scaling
- Increase memory and CPU for high-traffic deployments
- Increase storage for long-term retention
- Optimize query performance for large datasets

## Monitoring Best Practices

1. **Metrics Collection**
   - Collect metrics at appropriate intervals (15s-60s)
   - Use labels for grouping and filtering
   - Avoid high-cardinality labels
   - Use recording rules for derived metrics

2. **Alerting**
   - Set appropriate thresholds
   - Use alert groups to reduce noise
   - Implement escalation policies
   - Alert only on critical issues

3. **Logging**
   - Use structured logging
   - Include correlation IDs
   - Log errors and warnings
   - Retain logs for compliance

4. **Dashboards**
   - Focus on key metrics
   - Use consistent time ranges
   - Include both current and historical data
   - Make dashboards actionable

## Troubleshooting

### Common Issues
1. **Prometheus not scraping**: Check firewall rules and endpoint availability
2. **Grafana not loading**: Check data source configuration
3. **Alerts not firing**: Verify alert rules and configuration
4. **Logs not appearing**: Check Promtail configuration and permissions

### Debug Commands
```bash
# Check Prometheus status
docker logs erlmcp-prometheus

# Check Grafana status
docker logs erlmcp-grafana

# Check Loki status
docker logs erlmcp-loki

# Check Alertmanager status
docker logs erlmcp-alertmanager

# Check Promtail status
docker logs erlmcp-promtail
```

## Support

For issues and questions:
- Documentation: https://docs.erlmcp.com/monitoring
- Support: support@erlmcp.com
- Community: https://community.erlmcp.com
EOF

    log_success "Documentation created"
}

# Main execution
main() {
    log "Starting erlmcp v3 Enterprise Monitoring Stack Setup..."

    check_prerequisites
    create_directories
    download_docker_compose
    configure_prometheus
    configure_grafana
    configure_loki
    configure_alertmanager
    setup_systemd_services
    create_documentation

    log_success "Enterprise monitoring stack setup completed successfully!"

    echo ""
    echo "Next steps:"
    echo "1. Start the monitoring stack: sudo systemctl start erlmcp-monitoring"
    echo "2. Access Grafana: http://localhost:3000 (admin/admin123)"
    echo "3. Access Prometheus: http://localhost:9090"
    echo "4. Access Loki: http://localhost:3100"
    echo "5. Access Alertmanager: http://localhost:9093"
    echo ""
    echo "Configuration files are located in /opt/erlmcp-monitoring/"
    echo "Documentation is available in /opt/erlmcp-monitoring/README.md"
}

# Execute main function
main "$@"