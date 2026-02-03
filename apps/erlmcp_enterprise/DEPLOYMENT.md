# Enterprise Integration Suite Deployment Guide

## Overview

This document provides comprehensive deployment instructions for the erlmcp Enterprise Integration Suite v3.0.0. The suite includes enterprise-grade integrations for Fortune 500 systems including identity providers, monitoring platforms, logging systems, business intelligence tools, and more.

## Prerequisites

### System Requirements

- **OS**: Linux (Ubuntu 18.04+ recommended)
- **Erlang/OTP**: 28.3.1+
- **Memory**: Minimum 4GB RAM, 8GB recommended
- **Disk**: Minimum 20GB free space
- **Network**: 100Mbps+ connection

### Dependencies

```bash
# Install Erlang/OTP 28.3.1
curl -O https://github.com/erlang/otp/releases/download/OTP-28.3.1/otp_src_28.3.1.tar.gz
tar -xzf otp_src_28.3.1.tar.gz
cd otp_src_28.3.1
./configure --prefix=/opt/erlang-28.3.1
make
sudo make install

# Install rebar3
curl -L https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
sudo mv rebar3 /usr/local/bin/

# Install system dependencies
sudo apt-get update
sudo apt-get install -y build-essential libssl-dev zlib1g-dev

# Install additional tools
sudo apt-get install -y git curl wget jq htop ncdu
```

### Configuration Files

Place all configuration files in `/etc/erlmcp/enterprise/`:
- `identity_config.json`
- `monitoring_config.json`
- `logging_config.json`
- `connections_config.json`
- `deploy_config.json`
- `compliance_config.json`

## Deployment Steps

### 1. Prepare Deployment Environment

```bash
# Create deployment directories
sudo mkdir -p /opt/erlmcp-enterprise
sudo mkdir -p /etc/erlmcp/enterprise
sudo mkdir -p /var/log/erlmcp/enterprise
sudo mkdir -p /var/lib/erlmcp/enterprise
sudo mkdir -p /etc/erlmcp/certs

# Set permissions
sudo chown -R $USER:$USER /opt/erlmcp-enterprise
sudo chown -R $USER:$USER /etc/erlmcp
sudo chown -R $USER:$USER /var/log/erlmcp
sudo chown -R $USER:$USER /var/lib/erlmcp

# Download or copy the erlmcp-enterprise package
# Replace with actual download command
wget https://github.com/your-org/erlmcp-enterprise/releases/download/v3.0.0/erlmcp-enterprise-v3.0.0.tar.gz
tar -xzf erlmcp-enterprise-v3.0.0.tar.gz -C /opt/erlmcp-enterprise
```

### 2. Configure SSL/TLS Certificates

```bash
# Generate self-signed certificates for development/testing
openssl req -x509 -newkey rsa:4096 -nodes -keyout /etc/erlmcp/certs/server.key \
  -out /etc/erlmcp/certs/server.crt -days 365 -subj "/CN=erlmcp-enterprise"

# For production, use certificates from your CA or Let's Encrypt
# Ensure proper file permissions:
sudo chmod 600 /etc/erlmcp/certs/server.key
sudo chmod 644 /etc/erlmcp/certs/server.crt
```

### 3. Configure Integration Systems

#### Identity Provider Configuration
Update `/etc/erlmcp/enterprise/identity_config.json`:
```json
{
  "identity_providers": {
    "okta": {
      "enabled": true,
      "endpoint": "https://your-org.okta.com",
      "client_id": "your-client-id",
      "client_secret": "your-client-secret",
      "api_token": "your-api-token"
    }
  }
}
```

#### Monitoring System Configuration
Update `/etc/erlmcp/enterprise/monitoring_config.json`:
```json
{
  "monitoring_systems": {
    "splunk": {
      "enabled": true,
      "host": "splunk.example.com",
      "port": 8088,
      "token": "your-splunk-token",
      "index": "main"
    }
  }
}
```

#### Logging Platform Configuration
Update `/etc/erlmcp/enterprise/logging_config.json`:
```json
{
  "logging_platforms": {
    "elk": {
      "enabled": true,
      "elasticsearch": {
        "hosts": ["https://elasticsearch.example.com:9200"],
        "username": "elastic",
        "password": "your-password"
      }
    }
  }
}
```

### 4. Build and Compile

```bash
# Navigate to the enterprise app directory
cd /opt/erlmcp-enterprise/apps/erlmcp_enterprise

# Build the application
rebar3 compile

# Run tests
rebar3 ct

# Compile all applications
cd /opt/erlmcp-enterprise
rebar3 compile
```

### 5. Setup System Services

Create systemd service file `/etc/systemd/system/erlmcp-enterprise.service`:

```ini
[Unit]
Description=erlmcp Enterprise Integration Suite
After=network.target postgresql.service

[Service]
Type=simple
User=erlmcp
Group=erlmcp
WorkingDirectory=/opt/erlmcp-enterprise
ExecStart=/opt/erlang-28.3.1/bin/erl -pa /opt/erlmcp-enterprise/_build/default/lib/*/ebin \
  -config /etc/erlmcp/enterprise/config/sys.config \
  -name erlmcp@localhost \
  -cookie erlmcp-enterprise \
  -s erlmcp_enterprise_app
Restart=always
RestartSec=10
LimitNOFILE=65535

[Install]
WantedBy=multi-user.target
```

Create sys.config file at `/etc/erlmcp/enterprise/config/sys.config`:

```erlang
[
    {erlmcp_core, [
        {registry_type, gproc},
        {session_backend, ets},
        {enable_cache, true}
    ]},
    {erlmcp_enterprise, [
        {config_path, "/etc/erlmcp/enterprise"},
        {enable_bus, true},
        {enable_metrics, true},
        {enable_health_checks, true},
        {enable_audit_log, true},
        {config_watch, true}
    ]},
    {sasl, [
        {sasl_error_logger, {file, "/var/log/erlmcp/enterprise/sasl.log"}},
        {errlog_type, error}
    ]},
    {logger, [
        {handler, default, logger_std_h,
            #{config => #{type = standard_output, flush_queue = true,
                level = info, formatter = logger_formatter,
                formatter_config => #{single_line => false}}}},
        {handler, error_logger, logger_std_h,
            #{config => #{type = standard_error, flush_queue = true,
                level = error, formatter = logger_formatter,
                formatter_config => #{single_line => false}}}},
        {handler, file_logger, logger_disk_log_h,
            #{config => #{file => "/var/log/erlmcp/enterprise/application.log",
                type = wrap, max_no_files => 10, max_bytes => 10485760,
                level = info, formatter = logger_formatter,
                formatter_config => #{single_line => false}}}}
    ]}
].
```

### 6. Configure Environment Variables

Create `/etc/default/erlmcp-enterprise`:

```bash
# Erlang/OTP Settings
export ERLMCP_OTP_BIN="/opt/erlang-28.3.1/bin"
export ERLMCP_MAX_PORTS=65535

# Application Settings
export ERLMCP_CONFIG_PATH="/etc/erlmcp/enterprise"
export ERLMCP_LOG_PATH="/var/log/erlmcp/enterprise"
export ERLMCP_DATA_PATH="/var/lib/erlmcp/enterprise"

# Security Settings
export ERLMCP_COOKIE="erlmcp-enterprise-cookie"
export ERLMCP_NODE_NAME="erlmcp@$(hostname)"

# Performance Settings
export ERLMCP_SCHEDULERS=4
export ERLMCP_PROCESS_LIMIT=1048576
export ERLMCP_PORT_RANGE=1024-65535
```

### 7. Start the Service

```bash
# Reload systemd
sudo systemctl daemon-reload

# Enable the service
sudo systemctl enable erlmcp-enterprise

# Start the service
sudo systemctl start erlmcp-enterprise

# Check status
sudo systemctl status erlmcp-enterprise

# View logs
sudo journalctl -u erlmcp-enterprise -f
```

## Production Deployment

### Multi-Node Cluster Setup

1. **Prepare multiple nodes** with the same configuration
2. **Update Erlang cookie** to be the same across all nodes
3. **Configure distribution** in `deploy_config.json`

```json
{
  "deployment": {
    "cluster_mode": true,
    "nodes": [
      {"name": "node1", "host": "node1.example.com", "roles": ["primary"]},
      {"name": "node2", "host": "node2.example.com", "roles": ["secondary"]}
    ]
  }
}
```

### Load Balancing Setup

Configure HAProxy or Nginx for load balancing:

```nginx
# /etc/nginx/nginx.conf
upstream erlmcp_backend {
    least_conn;
    server node1.example.com:8080 max_fails=3 fail_timeout=30s;
    server node2.example.com:8080 max_fails=3 fail_timeout=30s;
}

server {
    listen 80;
    server_name erlmcp.example.com;
    location / {
        proxy_pass http://erlmcp_backend;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    }
}
```

### SSL/TLS Configuration

For production, use Let's Encrypt:

```bash
sudo apt-get install certbot
sudo certbot --nginx -d erlmcp.example.com
```

## Monitoring and Maintenance

### Health Checks

```bash
# Check service status
sudo systemctl is-active erlmcp-enterprise

# Check memory usage
sudo journalctl -u erlmcp-enterprise --since "1 hour ago" | grep "memory"

# Check application health
curl -s http://localhost:8080/health | jq
```

### Log Rotation

Configure logrotate:

```bash
# /etc/logrotate.d/erlmcp-enterprise
/var/log/erlmcp/enterprise/*.log {
    daily
    missingok
    rotate 30
    compress
    delaycompress
    notifempty
    create 644 erlmcp erlmcp
    postrotate
        systemctl reload erlmcp-enterprise
    endscript
}
```

### Backup and Recovery

```bash
#!/bin/bash
# backup.sh
BACKUP_DIR="/backups/erlmcp-enterprise"
DATE=$(date +%Y%m%d_%H%M%S)

# Create backup
mkdir -p $BACKUP_DIR
tar -czf $BACKUP_DIR/erlmcp-enterprise-config-$DATE.tar.gz \
  /etc/erlmcp/enterprise \
  /var/lib/erlmcp/enterprise

# Rotate backups
find $BACKUP_DIR -name "*.gz" -mtime +7 -delete
```

## Troubleshooting

### Common Issues

1. **Service won't start**:
   ```bash
   sudo journalctl -u erlmcp-enterprise -n 50 --no-pager
   ```

2. **Memory issues**:
   ```bash
   # Check Erlang memory usage
   erl -name debug@localhost -cookie erlmcp-enterprise \
     -eval "io:format(\"~p~n\", [erlang:memory()]), halt()."
   ```

3. **Connection issues**:
   ```bash
   # Test connectivity to external services
   telnet identity-provider.example.com 443
   ```

### Performance Tuning

```bash
# Configure BEAM scheduler
export ERLMCP_SCHEDULERS=$(nproc)

# Increase open file limit
ulimit -n 65535

# Configure kernel parameters
echo 'fs.file-max = 1000000' | sudo tee -a /etc/sysctl.conf
sudo sysctl -p
```

## Support

For issues and support:
- GitHub Issues: https://github.com/your-org/erlmcp-enterprise/issues
- Documentation: https://docs.erlmcp.com/enterprise
- Email: support@erlmcp.com

## Version History

- **v3.0.0** (2026-02-02)
  - Initial release
  - Support for 12 enterprise integration categories
  - Comprehensive monitoring and compliance features