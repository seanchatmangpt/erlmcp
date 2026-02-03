# erlmcp v3 Docker Swarm Operations Runbook

Enterprise operations guide for erlmcp Docker Swarm deployments.

## Table of Contents
1. [Daily Operations](#daily-operations)
2. [Weekly Operations](#weekly-operations)
3. [Monthly Operations](#monthly-operations)
4. [Emergency Procedures](#emergency-procedures)
5. [Scaling Operations](#scaling-operations)
6. [Maintenance Windows](#maintenance-windows)
7. [Incident Response](#incident-response)
8. [Performance Tuning](#performance-tuning)
9. [Security Operations](#security-operations)
10. [Disaster Recovery](#disaster-recovery)

## Daily Operations

### Health Checks
```bash
# Check swarm status
docker info | grep -i "swarm"

# List services
docker service ls

# Check service health
docker service ps --filter "health=healthy" erlmcp-swarm_erlmcp-core

# Monitor logs
docker service logs --tail 100 erlmcp-swarm_erlmcp-core
```

### Resource Monitoring
```bash
# System resource usage
docker stats --no-stream

# Network usage
docker network inspect erlmcp-overlay

# Volume usage
docker volume ls | grep erlmcp
```

### Backup Verification
```bash
# Verify database backup
./scripts/verify-database-backup.sh

# Verify application data backup
./scripts/verify-app-backup.sh

# Check backup logs
tail -f /backup/logs/backup.log
```

## Weekly Operations

### Service Updates
```bash
# Update all services
docker service update --image erlmcp/erlmcp-core:v3.0.1 erlmcp-swarm_erlmcp-core
docker service update --image erlmcp/erlmcp-transports:v3.0.1 erlmcp-swarm_erlmcp-transports

# Monitor update progress
docker service ps erlmcp-swarm_erlmcp-core --no-trunc
```

### Security Scanning
```bash
# Scan for vulnerabilities
./scripts/security-scan.sh

# Check compliance
./scripts/compliance-check.sh

# Update security policies
./scripts/update-security-policies.sh
```

### Performance Review
```bash
# Review Grafana dashboards
open http://localhost:3000

# Check performance metrics
./scripts/performance-report.sh

# Identify bottlenecks
./scripts/bottleneck-analysis.sh
```

## Monthly Operations

### System Updates
```bash
# Update Docker engine
apt-get update && apt-get upgrade docker-ce

# Update Docker Compose
curl -L "https://github.com/docker/compose/releases/download/1.29.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose

# Update monitoring stack
docker service update erlmcp-swarm_prometheus --image prom/prometheus:v2.46.0
docker service update erlmcp-swarm_grafana --image grafana/grafana:10.3.0
```

### Capacity Planning
```bash
# Analyze usage patterns
./scripts/usage-analysis.sh

# Plan scaling
./scripts/capacity-planning.sh

# Document findings
./docs/capacity-report.md
```

### Disaster Recovery Test
```bash
# Run failover test
./scripts/test-failover.sh

# Restore test
./scripts/test-restore.sh

# Document results
./docs/dr-test-report.md
```

## Emergency Procedures

### Service Failure
```bash
# Identify failed service
docker service ps erlmcp-swarm_erlmcp-core --no-trunc

# Check logs
docker service logs --tail 200 erlmcp-swarm_erlmcp-core

# Restart service
docker service update erlmcp-swarm_erlmcp-core --force

# Scale up if needed
docker service scale erlmcp-swarm_erlmcp-core=5
```

### Node Failure
```bash
# Identify failed node
docker node ls

# Drain failed node
docker node update --availability drain FAILED_NODE

# Migrate services
docker service update erlmcp-swarm_erlmcp-core --placement-pref spread=node.id

# If needed, promote worker to manager
docker node promote NODE_ID
```

### Network Partition
```bash
# Check network status
docker network inspect erlmcp-overlay

# Test connectivity
docker exec -it erlmcp-swarm_erlmcp-core-1 ping erlmcp-registry

# If partition persists, reconfigure networking
./scripts/reconfigure-network.sh
```

## Scaling Operations

### Horizontal Scaling
```bash
# Scale core service
docker service scale erlmcp-swarm_erlmcp-core=5

# Scale transport services
docker service scale erlmcp-swarm_erlmcp-transport-tcp=10
docker service scale erlmcp-swarm_erlmcp-transport-http=10

# Scale database
docker service scale erlmcp-swarm_erlmcp-session-db=5
```

### Auto-Scaling Configuration
```bash
# Enable auto-scaling for core service
docker service update --constraint-add "node.labels.erlmcp-role==manager" \
  --update-parallelism 1 --update-delay 10s erlmcp-swarm_erlmcp-core
```

### Vertical Scaling
```bash
# Increase resource limits
docker service update --limit-cpus 4.0 --limit-memory 8G \
  erlmcp-swarm_erlmcp-core

# Increase reservations
docker service update --reserve-cpus 2.0 --reserve-memory 4G \
  erlmcp-swarm_erlmcp-core
```

## Maintenance Windows

### Planned Maintenance
```bash
# Schedule maintenance
./scripts/schedule-maintenance.sh "2025-01-15 02:00" "4 hours"

# Notify stakeholders
./scripts/notify-stakeholders.sh

# Execute maintenance
./scripts/execute-maintenance.sh
```

### Rollback Strategy
```bash
# Prepare rollback
./scripts/prepare-rollback.sh

# Deploy update
docker service update --image erlmcp/erlmcp-core:v3.1.0 erlmcp-swarm_erlmcp-core

# Monitor
docker service ps --filter "desired-state=running" erlmcp-swarm_erlmcp-core

# If needed, rollback
docker service rollback erlmcp-swarm_erlmcp-core
```

## Incident Response

### Incident Classification
- **P0**: Critical - System down
- **P1**: High - Major functionality impaired
- **P2**: Medium - Non-critical issues
- **P3**: Low - Minor issues

### Response Process
1. **Detect**: Monitoring alerts
2. **Assess**: Impact analysis
3. **Contain**: Isolate the issue
4. **Resolve**: Fix the root cause
5. **Recover**: Restore normal operations
6. **Review**: Post-mortem analysis

### Escalation Matrix
```yaml
P0:
  - Level 1: On-call Engineer (15 min)
  - Level 2: Engineering Manager (30 min)
  - Level 3: VP Engineering (1 hour)

P1:
  - Level 1: On-call Engineer (30 min)
  - Level 2: Engineering Manager (1 hour)

P2:
  - Level 1: On-call Engineer (2 hours)

P3:
  - Business hours: Support Team
```

## Performance Tuning

### Database Optimization
```bash
# Check query performance
docker exec erlmcp-swarm_erlmcp-session-db-1 mysql -e "SHOW PROCESSLIST"

# Optimize tables
docker exec erlmcp-swarm_erlmcp-session-db-1 mysql -e "OPTIMIZE TABLE erlmcp_sessions"

# Monitor slow queries
docker exec erlmcp-swarm_erlmcp-session-db-1 tail -f /var/log/mysql/slow-query.log
```

### Cache Optimization
```bash
# Check Redis memory usage
docker exec erlmcp-swarm_erlmcp-redis-1 redis-cli info memory

# Clear cache if needed
docker exec erlmcp-swarm_erlmcp-redis-1 redis-cli FLUSHDB

# Monitor cache hit ratio
docker exec erlmcp-swarm_erlmcp-redis-1 redis-cli info stats | grep hit
```

### Network Optimization
```bash
# Check network latency
docker exec erlmcp-swarm_erlmcp-core-1 ping erlmcp-registry

# Adjust TCP parameters
echo "net.core.somaxconn = 65535" | tee -a /etc/sysctl.conf
echo "net.ipv4.tcp_max_syn_backlog = 65535" | tee -a /etc/sysctl.conf
sysctl -p
```

## Security Operations

### Access Control
```bash
# Manage user access
docker swarm join-token manager
docker swarm join-token worker

# Restrict service access
docker service update --label-add "access=limited" erlmcp-swarm_erlmcp-core

# Audit access logs
docker service logs --tail 500 erlmcp-swarm_erlmcp-core | grep "authentication"
```

### Vulnerability Management
```bash
# Scan for vulnerabilities
docker scan erlmcp/erlmcp-core

# Apply security patches
docker service update --image erlmcp/erlmcp-core:v3.0.1 patched erlmcp-swarm_erlmcp-core

# Monitor security events
./scripts/security-monitor.sh
```

### Compliance
```bash
# Run compliance checks
./scripts/compliance-check.sh --standard cis-docker

# Generate compliance report
./scripts/compliance-report.sh

# Fix compliance issues
./scripts/fix-compliance.sh
```

## Disaster Recovery

### Backup and Restore
```bash
# Create backup
./scripts/create-backup.sh

# Restore from backup
./scripts/restore-backup.sh

# Verify restore
./scripts/verify-restore.sh
```

### Failover Procedures
```bash
# Failover to standby cluster
./scripts/failover-to-standby.sh

# Promote standby cluster
./scripts/promote-standby.sh

# Update DNS
./scripts/update-dns.sh
```

### Testing
```bash
# Run disaster recovery test
./scripts/dr-test.sh

# Measure RTO/RPO
./scripts/measure-rto-rpo.sh

# Document results
./docs/dr-test-results.md
```

## Contact Information

- **Emergency**: 24/7 On-call +1-555-0123
- **Support**: support@erlmcp.com
- **DevOps**: devops@erlmcp.com
- **Security**: security@erlmcp.com
- **Documentation**: docs@erlmcp.com

## Related Documentation

- [Architecture Diagrams](/docs/architecture.md)
- [API Reference](/docs/api-reference.md)
- [Configuration Guide](/docs/configuration.md)
- [Security Policy](/docs/security-policy.md)
- [Performance Tuning](/docs/performance.md)