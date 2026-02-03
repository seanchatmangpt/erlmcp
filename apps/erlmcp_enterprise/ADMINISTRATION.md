# Enterprise Integration Suite Administration Guide

## Table of Contents
1. [Overview](#overview)
2. [Architecture](#architecture)
3. [System Administration](#system-administration)
4. [Integration Management](#integration-management)
5. [Security Administration](#security-administration)
6. [Performance Monitoring](#performance-monitoring)
7. [Troubleshooting](#troubleshooting)
8. [Maintenance Procedures](#maintenance-procedures)

## Overview

The erlmcp Enterprise Integration Suite provides a unified platform for integrating with Fortune 500 systems. This guide covers administrative tasks including system configuration, integration management, security administration, and performance optimization.

## Architecture

### System Components

```
┌─────────────────────────────────────────────────────────────┐
│                    Enterprise Integration Suite             │
├─────────────────────────────────────────────────────────────┤
│                    Load Balancer (HAProxy/Nginx)            │
├─────────────────────────────────────────────────────────────┤
│  API Gateway  │  Enterprise Bus  │  Registry Service      │
├─────────────────────────────────────────────────────────────┤
│  Identity Adapters │  Monitoring Adapters  │  Logging Adapters │
│  BI Tools Adapters │  Service Bus Adapters │  Data Warehouse    │
│  DevOps Adapters  │  Cloud Adapters      │  Security Adapters │
├─────────────────────────────────────────────────────────────┤
│              Erlang/OTP Runtime (OTP 28.3.1+)              │
├─────────────────────────────────────────────────────────────┤
│                  Operating System (Linux)                   │
└─────────────────────────────────────────────────────────────┘
```

### Supervision Hierarchy

```
erlmcp_enterprise_sup (one_for_all)
├── erlmcp_enterprise_core_sup
│   ├── erlmcp_identity_adapter
│   ├── erlmcp_monitoring_adapter
│   ├── erlmcp_logging_adapter
│   ├── erlmcp_bizintel_adapter
│   ├── erlmcp_servicebus_adapter
│   ├── erlmcp_data_adapter
│   ├── erlmcp_devops_adapter
│   ├── erlmcp_apigw_adapter
│   ├── erlmcp_cloud_adapter
│   ├── erlmcp_enterprise_bus
│   ├── erlmcp_enterprise_registry
│   ├── erlmcp_enterprise_metrics
│   ├── erlmcp_enterprise_health
│   ├── erlmcp_enterprise_audit
│   ├── erlmcp_enterprise_config
│   ├── erlmcp_enterprise_pools
│   └── erlmcp_circuit_breaker
└── erlmcp_enterprise_platform_sup
    ├── erlmcp_observability_sup
    ├── erlmcp_security_sup
    └── erlmcp_compliance_sup
```

## System Administration

### Starting and Stopping

```bash
# Start the enterprise suite
sudo systemctl start erlmcp-enterprise

# Stop the suite
sudo systemctl stop erlmcp-enterprise

# Restart
sudo systemctl restart erlmcp-enterprise

# Check status
sudo systemctl status erlmcp-enterprise
```

### Configuration Management

#### Dynamic Configuration Reload

```bash
# Reload configuration without restart
curl -X POST http://localhost:8080/api/v1/config/reload

# Check configuration status
curl -X GET http://localhost:8080/api/v1/config/status
```

#### Configuration Validation

```bash
# Validate JSON configuration
cat /etc/erlmcp/enterprise/identity_config.json | jq .

# Test configuration syntax
rebar3 compile --apps erlmcp_enterprise
```

### Process Management

#### Interactive Shell

```bash
# Connect to running node
erl -name admin@localhost -cookie erlmcp-enterprise

# Check processes
> application:which_applications().
> supervisor:which_children(erlmcp_enterprise_sup).
> erlang:process_info(self()).
```

#### Process Monitoring

```bash
# Monitor specific processes
ps aux | grep erl
top -H -p $(pgrep -f erl)

# Check memory usage
curl -s http://localhost:8080/api/v1/metrics/memory | jq
```

## Integration Management

### Identity Provider Management

#### Add New Identity Provider

```bash
# Update configuration
vi /etc/erlmcp/enterprise/identity_config.json

# Restart service
sudo systemctl restart erlmcp-enterprise

# Test connection
curl -X POST http://localhost:8080/api/v1/identity/test \
  -H "Content-Type: application/json" \
  -d '{"provider": "okta"}'
```

#### User Management

```bash
# List users
curl -X GET http://localhost:8080/api/v1/identity/users

# Get user details
curl -X GET http://localhost:8080/api/v1/identity/users/john.doe@example.com

# Update user roles
curl -X PUT http://localhost:8080/api/v1/identity/users/john.doe@example.com/roles \
  -H "Content-Type: application/json" \
  -d '["developer", "viewer"]'
```

### Monitoring Platform Integration

#### Splunk Configuration

```bash
# Test Splunk connection
curl -X POST http://localhost:8080/api/v1/monitoring/splunk/test \
  -H "Content-Type: application/json" \
  -d '{"event": {"message": "Test event"}}'

# Send test events
curl -X POST http://localhost:8080/api/v1/monitoring/test-event \
  -H "Content-Type: application/json" \
  -d '{"system": "erlmcp", "level": "info", "message": "Test"}'
```

### Business Intelligence Integration

#### Tableau Integration

```bash
# List workbooks
curl -X GET http://localhost:8080/api/v1/bizintel/tableau/workbooks

# Create new view
curl -X POST http://localhost:8080/api/v1/bizintel/tableau/views \
  -H "Content-Type: application/json" \
  -d '{"workbook_id": "123", "name": "New View", "type": "table"}'
```

### Service Bus Integration

#### Kafka Integration

```bash
# Send message
curl -X POST http://localhost:8080/api/v1/servicebus/kafka/send \
  -H "Content-Type: application/json" \
  -d '{"topic": "events", "key": "user-123", "value": {"event": "login"}}'

# Create subscription
curl -X POST http://localhost:8080/api/v1/servicebus/kafka/subscriptions \
  -H "Content-Type: application/json" \
  -d '{"topic": "events", "group": "erlmcp-group", "filter": "user-*"}'
```

## Security Administration

### Authentication and Authorization

#### JWT Token Management

```bash
# Generate JWT token
curl -X POST http://localhost:8080/api/v1/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username": "admin", "password": "securepassword"}'

# Validate token
curl -X POST http://localhost:8080/api/v1/auth/validate \
  -H "Authorization: Bearer <jwt-token>"
```

#### API Key Management

```bash
# Create API key
curl -X POST http://localhost:8080/api/v1/api-keys \
  -H "Authorization: Bearer <admin-token>" \
  -H "Content-Type: application/json" \
  -d '{"description": "Test API key", "permissions": ["read", "write"]}'

# Revoke API key
curl -X DELETE http://localhost:8080/api/v1/api-keys/<key-id> \
  -H "Authorization: Bearer <admin-token>"
```

### Audit Logging

#### View Audit Logs

```bash
# Get all audit events
curl -X GET http://localhost:8080/api/v1/audit/events

# Get events by user
curl -X GET http://localhost:8080/api/v1/audit/events?user=john.doe@example.com

# Get events by time range
curl -X GET "http://localhost:8080/api/v1/audit/events?start=1642697400000&end=1642783800000"

# Export audit logs
curl -X GET "http://localhost:8080/api/v1/audit/export?format=csv&days=30" \
  -o audit_logs.csv
```

### Compliance Management

#### SOC2 Compliance Checks

```bash
# Run compliance scan
curl -X POST http://localhost:8080/api/v1/compliance/soc2/scan

# Get compliance status
curl -X GET http://localhost:8080/api/v1/compliance/soc2/status

# Generate compliance report
curl -X GET http://localhost:8080/api/v1/compliance/soc2/report \
  -o soc2_report.pdf
```

## Performance Monitoring

### Metrics Collection

#### System Metrics

```bash
# Get current metrics
curl -s http://localhost:8080/api/v1/metrics/system | jq

# Get metrics by service
curl -s http://localhost:8080/api/v1/metrics/services | jq

# Get performance metrics
curl -s http://localhost:8080/api/v1/metrics/performance | jq
```

### Health Checks

#### Service Health Status

```bash
# Check all services
curl -s http://localhost:8080/api/v1/health/services | jq

# Check specific service
curl -s http://localhost:8080/api/v1/health/services/identity | jq

# Get health summary
curl -s http://localhost:8080/api/v1/health/summary | jq
```

### Resource Utilization

#### Memory and CPU

```bash
# Check BEAM memory
curl -s http://localhost:8080/api/v1/system/memory | jq

# Check process statistics
curl -s http://localhost:8080/api/v1/system/processes | jq

# Check garbage collection
curl -s http://localhost:8080/api/v1/system/gc | jq
```

### Performance Tuning

#### Configuration Tuning

```bash
# Update connection pool settings
curl -X PUT http://localhost:8080/api/v1/config/connections \
  -H "Content-Type: application/json" \
  -d '{"identity_providers": {"pool_size": 20}}'

# Update cache settings
curl -X PUT http://localhost:8080/api/v1/config/cache \
  -H "Content-Type: application/json" \
  -d '{"max_size": 10000, "ttl": 300}'
```

## Troubleshooting

### Common Issues

#### Service Not Starting

```bash
# Check logs
sudo journalctl -u erlmcp-enterprise -n 100 --no-pager

# Check configuration
rebar3 compile --apps erlmcp_enterprise

# Test dependencies
telnet identity-provider.example.com 443
```

#### Connection Issues

```bash
# Test identity provider connection
curl -X POST http://localhost:8080/api/v1/identity/test \
  -H "Content-Type: application/json" \
  -d '{"provider": "okta"}'

# Check network connectivity
curl -v https://okta.example.com/oauth2/default/.well-known/oauth-authorization-server
```

#### Performance Issues

```bash
# Check process queue lengths
curl -s http://localhost:8080/api/v1/metrics/queues | jq

# Check message rates
curl -s http://localhost:8080/api/v1/messaging/rates | jq

# Identify bottlenecks
curl -s http://localhost:8080/api/v1/metrics/bottlenecks | jq
```

### Debug Mode

#### Enable Debug Logging

```bash
# Temporarily enable debug logging
curl -X POST http://localhost:8080/api/v1/logging/debug \
  -H "Content-Type: application/json" \
  -d '{"module": "erlmcp_identity_adapter", "level": "debug"}'
```

#### Trace Execution

```bash
# Enable tracing
curl -X POST http://localhost:8080/api/v1/tracing/enable \
  -H "Content-Type: application/json" \
  -d '{"services": ["identity", "monitoring"]}'

# Get traces
curl -s http://localhost:8080/api/v1/traces | jq
```

## Maintenance Procedures

### Scheduled Maintenance

#### System Updates

```bash
# Stop services
sudo systemctl stop erlmcp-enterprise

# Backup configuration
sudo cp -r /etc/erlmcp/enterprise /backups/erlmcp-enterprise-$(date +%Y%m%d)

# Update packages (if needed)
sudo apt-get update
sudo apt-get upgrade -y

# Update erlmcp-enterprise
cd /opt/erlmcp-enterprise
git pull
rebar3 compile

# Start services
sudo systemctl start erlmcp-enterprise
```

#### Database Maintenance

```bash
# Compact ETS tables
curl -X POST http://localhost:8080/api/v1/database/compact

# Optimize registry
curl -X POST http://localhost:8080/api/v1/database/optimize

# Rotate audit logs
curl -X POST http://localhost:8080/api/v1/audit/rotate
```

### Backup and Recovery

#### Configuration Backup

```bash
#!/bin/bash
# backup-config.sh
BACKUP_DIR="/backups/erlmcp-enterprise"
DATE=$(date +%Y%m%d_%H%M%S)

mkdir -p $BACKUP_DIR
tar -czf $BACKUP_DIR/config-$DATE.tar.gz \
  /etc/erlmcp/enterprise

echo "Configuration backup created: $BACKUP_DIR/config-$DATE.tar.gz"
```

#### Full System Backup

```bash
#!/bin/bash
# backup-system.sh
BACKUP_DIR="/backups/erlmcp-enterprise-full"
DATE=$(date +%Y%m%d_%H%M%S)

# Backup application
tar -czf $BACKUP_DIR/app-$DATE.tar.gz \
  /opt/erlmcp-enterprise

# Backup data
tar -czf $BACKUP_DIR/data-$DATE.tar.gz \
  /var/lib/erlmcp/enterprise

# Backup logs
tar -czf $BACKUP_DIR/logs-$DATE.tar.gz \
  /var/log/erlmcp/enterprise

echo "System backup completed"
```

### Disaster Recovery

#### Restore Procedure

```bash
#!/bin/bash
# restore-system.sh
BACKUP_FILE="/backups/erlmcp-enterprise-full/app-$DATE.tar.gz"
RESTORE_DIR="/tmp/restore"

# Stop services
sudo systemctl stop erlmcp-enterprise

# Extract backup
mkdir -p $RESTORE_DIR
tar -xzf $BACKUP_FILE -C $RESTORE_DIR

# Replace application
sudo cp -r $RESTORE_DIR/* /opt/erlmcp-enterprise

# Restore permissions
sudo chown -R $USER:$USER /opt/erlmcp-enterprise

# Restart services
sudo systemctl start erlmcp-enterprise
```

## Monitoring Dashboards

### Key Metrics Dashboard

Access the enterprise metrics dashboard at:
```
http://localhost:8080/dashboard
```

### Alert Configuration

```json
{
  "alerts": [
    {
      "name": "High Error Rate",
      "condition": "error_rate > 10",
      "severity": "high",
      "notification": "email,slack"
    },
    {
      "name": "Service Degraded",
      "condition": "status == 'degraded'",
      "severity": "medium",
      "notification": "slack"
    }
  ]
}
```

## Contact Support

For urgent issues:
- Emergency: support@erlmcp.com
- GitHub Issues: https://github.com/your-org/erlmcp-enterprise/issues
- Documentation: https://docs.erlmcp.com/enterprise