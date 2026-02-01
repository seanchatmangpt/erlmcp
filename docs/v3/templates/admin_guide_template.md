# [Topic] Administration Guide Template

**Version**: 3.0.0
**Last Updated**: YYYY-MM-DD
**Maintainer**: [Name]

---

## Overview

This guide covers [topic] for erlmcp operators and administrators.

**Scope**: [What this guide covers]
**Out of Scope**: [What is not covered, with links to appropriate docs]

**Target Audience**: System administrators, DevOps engineers, SREs

**Prerequisites**:
- Working erlmcp installation (see [Installation Guide](../installation/quickstart.md))
- Basic Erlang/OTP knowledge
- System administration experience

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────┐
│                     [Component Layer]                │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐ │
│  │ Component 1 │  │ Component 2 │  │ Component 3 │ │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘ │
│         └────────────────┴────────────────┘         │
└────────────────────────┬────────────────────────────┘
                         │
┌────────────────────────▼────────────────────────────┐
│                    [Service Layer]                  │
│              (erlmcp application)                    │
└─────────────────────────────────────────────────────┘
```

**Key Components**:
- **[Component 1]**: [Description and role]
- **[Component 2]**: [Description and role]
- **[Component 3]**: [Description and role]

---

## Configuration

### Configuration Files

| File | Location | Purpose |
|------|----------|---------|
| `sys.config` | `/etc/erlmcp/` | Application configuration |
| `vm.args` | `/etc/erlmcp/` | VM arguments |
| `env.sh` | `/etc/erlmcp/` | Environment variables |

### Configuration Reference

#### Section: [Configuration Section]

```erlang
{erlmcp, [
    %% [Option 1 description]
    {option1, value1},

    %% [Option 2 description]
    {option2, value2},

    %% [Option 3 description]
    {option3, [
        {sub_option1, sub_value1},
        {sub_option2, sub_value2}
    ]}
]}.
```

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `option1` | `type()` | `default` | Full description |
| `option2` | `type()` | `default` | Full description |
| `option3` | `list()` | `[]` | Full description |

### Environment Variables

```bash
# Runtime configuration
export ERLMCP_LOG_LEVEL=info
export ERLMCP_NODE_NAME=erlmcp@hostname
export ERLMCP_COOKIE=secret_cookie
```

| Variable | Default | Description |
|----------|---------|-------------|
| `ERLMCP_LOG_LEVEL` | `info` | Logging level (debug, info, warning, error) |
| `ERLMCP_NODE_NAME` | `erlmcp@127.0.0.1` | Erlang node name |
| `ERLMCP_COOKIE` | `erlmcp` | Inter-node communication cookie |

---

## Operations

### Setup

#### Initial Setup

```bash
# 1. Create directories
sudo mkdir -p /etc/erlmcp
sudo mkdir -p /var/log/erlmcp
sudo mkdir -p /var/lib/erlmcp

# 2. Copy configuration
sudo cp config/sys.config /etc/erlmcp/
sudo cp config/vm.args /etc/erlmcp/

# 3. Set permissions
sudo chown -R erlmcp:erlmcp /etc/erlmcp /var/log/erlmcp /var/lib/erlmcp

# 4. Start service
sudo systemctl start erlmcp
sudo systemctl enable erlmcp
```

#### Verification

```bash
# Check service status
sudo systemctl status erlmcp

# Check process
ps aux | grep beam

# Check logs
journalctl -u erlmcp -f

# Check connectivity
./bin/erlmcp ping
```

### Monitoring

#### Health Checks

```bash
# Local health check
curl http://localhost:8080/health

# Expected response
{"status":"healthy","timestamp":1234567890}
```

#### Metrics Endpoint

```bash
# Prometheus metrics
curl http://localhost:9090/metrics

# Key metrics to watch:
# - erlmcp_requests_total
# - erlmcp_errors_total
# - erlmcp_latency_seconds
# - erlmcp_connections_active
```

#### Key Metrics

| Metric | Type | Description | Alert Threshold |
|--------|------|-------------|-----------------|
| `erlmcp_requests_total` | Counter | Total requests | N/A |
| `erlmcp_errors_total` | Counter | Total errors | > 100/min |
| `erlmcp_latency_seconds` | Histogram | Request latency | P99 > 1s |
| `erlmcp_connections_active` | Gauge | Active connections | > 90% capacity |

### Maintenance

#### Daily Tasks

- [ ] Check health status
- [ ] Review error logs
- [ ] Verify metrics collection
- [ ] Check disk space

#### Weekly Tasks

- [ ] Review and rotate logs
- [ ] Check for security updates
- [ ] Review performance trends
- [ ] Backup configuration

#### Monthly Tasks

- [ ] Review and update documentation
- [ ] Capacity planning review
- [ ] Security audit
- [ ] Disaster recovery test

#### Log Rotation

```bash
# /etc/logrotate.d/erlmcp
/var/log/erlmcp/*.log {
    daily
    rotate 14
    compress
    delaycompress
    missingok
    notifempty
    create 0640 erlmcp erlmcp
    sharedscripts
    postrotate
        /bin/kill -USR1 $(cat /var/run/erlmcp.pid 2>/dev/null) 2>/dev/null || true
    endscript
}
```

---

## Troubleshooting

### Common Operational Issues

#### Issue: Service Won't Start

**Symptoms**:
- Service fails to start
- Error in logs

**Diagnosis**:
```bash
# Check logs
journalctl -u erlmcp -n 50

# Check configuration
erl -noshell -pa _build/default/lib/*/ebin -config /etc/erlmcp/sys.config

# Verify ports
netstat -tulpn | grep :5005
```

**Solution**:
1. Verify configuration syntax
2. Check port availability
3. Verify file permissions
4. Check system resources

#### Issue: High Memory Usage

**Symptoms**:
- Memory usage increasing
- OOM warnings

**Diagnosis**:
```bash
# Check memory
(erlmcp@host)1> erlang:memory().
(erlmcp@host)2> observer:start().

# Find large processes
(erlmcp@host)3> recon:memory(allocated).
```

**Solution**:
1. Identify memory hog
2. Check for leaks
3. Adjust memory limits
4. Restart if needed

---

## Runbooks

### Runbook: High Error Rate

**Trigger**: Alert firing for `erlmcp_errors_total > 100/min`

**Severity**: P2 (High)

**Estimated Time to Resolve**: 15 minutes

**Steps**:

1. **Verify Alert** (2 min)
   ```bash
   # Check current error rate
   curl http://localhost:9090/metrics | grep erlmcp_errors_total
   ```

2. **Check Logs** (3 min)
   ```bash
   # Recent errors
   journalctl -u erlmcp --since "5 minutes ago" | grep ERROR
   ```

3. **Identify Pattern** (5 min)
   - Are errors from specific endpoint?
   - Are errors correlated with load?
   - Any recent deployments?

4. **Mitigate** (5 min)
   - If deployment-related: Rollback
   - If load-related: Scale up
   - If bug-related: Escalate to dev team

5. **Verify** (Optional)
   - Confirm error rate normalized
   - Document root cause

### Runbook: Server Overload

**Trigger**: Alert firing for `erlmcp_connections_active > 90% capacity`

**Severity**: P1 (Critical)

**Estimated Time to Resolve**: 10 minutes

**Steps**:

1. **Verify Alert** (1 min)
   ```bash
   # Check connections
   (erlmcp@host)1> erlmcp_admin:connection_count().
   ```

2. **Immediate Action** (2 min)
   ```bash
   # Scale if using Kubernetes
   kubectl scale deployment erlmcp --replicas=10

   # Or add capacity if standalone
   ```

3. **Investigate** (5 min)
   - Is this expected traffic?
   - Any DDoS patterns?
   - Connection leak?

4. **Long-term Fix** (Post-incident)
   - Adjust capacity planning
   - Implement rate limiting
   - Add autoscaling

---

## Scaling

### Vertical Scaling

Add resources to single node:

```erlang
%% vm.args - Process limit
+P 1000000              % 1 million processes

%% vm.args - I/O threads
+A 32                   % 32 async threads

%% vm.args - Schedulers
+S 16:16                % 16 schedulers x 2
```

**Capacity Impact**:
- Base: 50K connections
- +P 1M: 100K connections
- +A 32: +20% throughput
- +S 16:16: +30% throughput

### Horizontal Scaling

Add more nodes:

```
Load Balancer
    |
    +-- Node 1 (50K)
    +-- Node 2 (50K)
    +-- Node 3 (50K)
    +-- Node N (50K)
```

**Configuration**:
```erlang
%% Each node
{erlmcp, [
    {cluster_enabled, true},
    {cluster_nodes, ['erlmcp@node1', 'erlmcp@node2', 'erlmcp@node3']}
]}.
```

**Health Check for Load Balancer**:
```bash
curl http://node1:8080/health
curl http://node2:8080/health
curl http://node3:8080/health
```

---

## Security

### Hardening Checklist

- [ ] Restrict file permissions (600 for config, 644 for logs)
- [ ] Use non-root user for service
- [ ] Enable TLS for all network traffic
- [ ] Configure firewall rules
- [ ] Enable audit logging
- [ ] Regular security updates
- [ ] Secrets encrypted at rest

### TLS Configuration

```erlang
%% sys.config
{erlmcp, [
    {tls, [
        {certfile, "/etc/erlmcp/certs/cert.pem"},
        {keyfile, "/etc/erlmcp/certs/key.pem"},
        {cacertfile, "/etc/erlmcp/certs/ca.pem"},
        {verify, verify_peer},
        {fail_if_no_peer_cert, true}
    ]}
]}.
```

### Firewall Rules

```bash
# Allow only necessary ports
ufw allow 5005/tcp  # MCP
ufw allow 8080/tcp  # HTTP
ufw allow 4369/tcp  # EPMD
ufw allow from 10.0.0.0/8 to any port 9100 # Erlang dist
```

---

## Backup and Recovery

### Backup Procedure

```bash
#!/bin/bash
# backup.sh

DATE=$(date +%Y%m%d)
BACKUP_DIR="/backup/erlmcp/$DATE"

# Create backup directory
mkdir -p $BACKUP_DIR

# Backup configuration
cp /etc/erlmcp/sys.config $BACKUP_DIR/
cp /etc/erlmcp/vm.args $BACKUP_DIR/

# Backup data (if using DETS/Mnesia)
cp -r /var/lib/erlmcp $BACKUP_DIR/

# Backup logs (last 7 days)
find /var/log/erlmcp -mtime -7 -exec cp {} $BACKUP_DIR/ \;

# Compress
tar czf /backup/erlmcp/backup-$DATE.tar.gz $BACKUP_DIR
rm -rf $BACKUP_DIR

# Keep last 30 days
find /backup/erlmcp -mtime +30 -delete
```

### Recovery Procedure

```bash
#!/bin/bash
# restore.sh BACKUP_FILE

BACKUP_FILE=$1
RESTORE_DIR="/tmp/erlmcp_restore"

# Extract
mkdir -p $RESTORE_DIR
tar xzf $BACKUP_FILE -C $RESTORE_DIR

# Stop service
systemctl stop erlmcp

# Restore configuration
cp $RESTORE_DIR/*/sys.config /etc/erlmcp/
cp $RESTORE_DIR/*/vm.args /etc/erlmcp/

# Restore data
rm -rf /var/lib/erlmcp/*
cp -r $RESTORE_DIR/*/erlmcp/* /var/lib/erlmcp/

# Start service
systemctl start erlmcp

# Verify
sleep 10
systemctl status erlmcp
```

---

## Incident Response

### Severity Levels

| Severity | Response Time | Examples |
|----------|---------------|----------|
| P0 - Critical | 15 minutes | Complete outage |
| P1 - High | 1 hour | Major degradation |
| P2 - Medium | 4 hours | Partial outage |
| P3 - Low | 1 day | Minor issues |

### Escalation Matrix

| Severity | Primary | Escalation | Management |
|----------|---------|------------|------------|
| P0 | On-call | On-call + Team Lead | CTO |
| P1 | On-call | Team Lead | Engineering Manager |
| P2 | Team | Team Lead | Engineering Manager |
| P3 | Team | Team Lead | N/A |

### Post-Incident Process

1. **Immediate Post-Incident**
   - Verify system stable
   - Communicate resolution
   - Begin documentation

2. **Post-Mortem** (within 48 hours)
   - Timeline of events
   - Root cause analysis
   - Action items
   - Preventive measures

3. **Follow-up** (within 1 week)
   - Verify action items completed
   - Update documentation
   - Update runbooks

---

## References

### Internal Documentation

- [Installation Guide](../installation/quickstart.md)
- [Monitoring Guide](monitoring.md)
- [Security Guide](security.md)

### External Documentation

- [Erlang/OTP Documentation](https://www.erlang.org/doc/)
- [rebar3 Documentation](https://rebar3.org/)

---

**Status**: [Draft | Review | Published]
**Review Date**: [YYYY-MM-DD]
**Next Review**: [YYYY-MM-DD]
