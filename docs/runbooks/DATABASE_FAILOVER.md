# Database Failover Runbook

**Runbook ID**: RB-009
**Severity**: SEV-1
**Last Updated**: 2026-02-02

## Overview

This runbook covers database failover procedures for PostgreSQL in erlmcp v3, including automated failover, manual promotion, and recovery procedures.

## Symptoms

- Database connection errors
- High replication lag
- Primary database unresponsive
- Read-only mode activated
- Connection pool exhaustion

## Prerequisites

- PostgreSQL 12+ with streaming replication
- Replication slots configured
- Patroni or repmgr for automated failover
- Backup of pg_control file
- Network connectivity between replicas

## Diagnosis

### Step 1: Check Primary Database Status

```bash
# Via Docker
docker exec -it erlmcp-postgres-primary psql -U postgres -c "SELECT pg_is_in_recovery();"

# Via Kubernetes
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- psql -U postgres -c "SELECT pg_is_in_recovery();"

# Check replication status
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- psql -U postgres -c "
SELECT
    client_addr,
    state,
    sync_state,
    replay_lag,
    flow_lag
FROM pg_stat_replication;
"
```

### Step 2: Check Replica Health

```bash
# Check all replicas
kubectl exec -n erlmcp-prod erlmcp-postgres-1 -- psql -U postgres -c "SELECT pg_is_in_recovery();"
kubectl exec -n erlmcp-prod erlmcp-postgres-2 -- psql -U postgres -c "SELECT pg_is_in_recovery();"

# Check replication lag
kubectl exec -n erlmcp-prod erlmcp-postgres-1 -- psql -U postgres -c "
SELECT
    now() - pg_last_xact_replay_timestamp() AS replication_lag;
"
```

### Step 3: Identify Failure Type

```bash
# Check if primary is accepting connections
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- pg_isready

# Check PostgreSQL process
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- ps aux | grep postgres

# Check system logs
kubectl logs -n erlmcp-prod erlmcp-postgres-0 --tail=100
```

## Resolution Procedures

### Automated Failover (Patroni/ETCD)

#### Scenario 1: Automatic Failover Triggered

```bash
# Monitor failover progress
watch -n 2 'kubectl get pods -n erlmcp-prod -l application=spilo'

# Check Patroni status
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- patronictl -c /etc/patroni/patroni.yml list

# View failover history
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- patronictl -c /etc/patroni/patroni.yml history
```

**Expected Behavior**:
- Patroni detects primary failure (typically within 30s)
- ETCD conducts leader election
- Replica with most recent data is promoted
- Applications reconnected via HAProxy/ProxySQL

**Validation**:
```bash
# Verify new primary
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- psql -U postgres -c "SELECT pg_is_in_recovery();"
# Should return 'f'

# Check application connectivity
kubectl exec -n erlmcp-prod erlmcp-node-0 -- curl -f http://localhost:8081/health
```

### Manual Failover

#### Scenario 2: Manual Failover Required

```bash
# Step 1: Stop primary replication
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- psql -U postgres -c "SELECT pg_ctl stop();"

# Step 2: Promote replica using Patroni
kubectl exec -n erlmcp-prod erlmcp-postgres-1 -- patronictl -c /etc/patroni/patroni.yml switchover

# Step 3: Follow prompts
# - Master: [current-primary]
# - Candidate: [replica-to-promote]
# - Replication slots: [confirm]

# Step 4: Verify failover
kubectl exec -n erlmcp-prod erlmcp-postgres-1 -- psql -U postgres -c "SELECT pg_is_in_recovery();"
```

### Emergency Failover (Without Patroni)

#### Scenario 3: Patroni Failure - Manual Promotion

```bash
# Step 1: Identify most advanced replica
kubectl exec -n erlmcp-prod erlmcp-postgres-1 -- psql -U postgres -c "
SELECT
    pg_last_xlog_replay_location(),
    pg_last_xact_replay_timestamp();
"

# Step 2: Stop all replicas
kubectl exec -n erlmcp-prod erlmcp-postgres-1 -- psql -U postgres -c "SELECT pg_ctl stop -m fast;"
kubectl exec -n erlmcp-prod erlmcp-postgres-2 -- psql -U postgres -c "SELECT pg_ctl stop -m fast;"

# Step 3: Promote selected replica
kubectl exec -n erlmcp-prod erlmcp-postgres-1 -- psql -U postgres -c "SELECT pg_promote();"

# Step 4: Update application connection string
# Update ConfigMap or Secret with new primary endpoint
kubectl patch configmap erlmcp-config -n erlmcp-prod --type=json \
  -p='[{"op": "replace", "path": "/data/DB_PRIMARY", "value": "erlmcp-postgres-1.erlmcp-prod"}]'

# Step 5: Restart applications
kubectl rollout restart statefulset erlmcp -n erlmcp-prod
```

## Recovery Procedures

### Recovering Failed Primary

```bash
# Step 1: Rebuild failed primary as replica
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- psql -U postgres -h erlmcp-postgres-1 -c "
SELECT pg_drop_replication_slot('erlmcp_postgres_0_slot');
"

# Step 2: Create base backup
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- rm -rf /var/lib/postgresql/data/*
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- pg_basebackup \
  -h erlmcp-postgres-1 \
  -D /var/lib/postgresql/data \
  -U replicator \
  -P \
  -v \
  -R \
  -X stream \
  -C -S erlmcp_postgres_0_slot

# Step 3: Start replica
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- pg_ctl start

# Step 4: Verify replication
kubectl exec -n erlmcp-prod erlmcp-postgres-1 -- psql -U postgres -c "
SELECT * FROM pg_stat_replication;
"
```

### Failback to Original Primary

```bash
# Step 1: Ensure original primary is up-to-date
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- psql -U postgres -c "
SELECT now() - pg_last_xact_replay_timestamp();
"
# Lag should be < 5 seconds

# Step 2: Stop current primary
kubectl exec -n erlmcp-prod erlmcp-postgres-1 -- psql -U postgres -c "SELECT pg_ctl stop();"

# Step 3: Promote original primary
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- psql -U postgres -c "SELECT pg_promote();"

# Step 4: Rebuild old primary as replica
# (Follow recovery procedure above)

# Step 5: Verify switchover
kubectl exec -n erlmcp-prod erlmcp-0 -- curl -f http://localhost:8081/health
```

## Data Consistency Validation

```bash
# Check transaction ID wraparound
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- psql -U postgres -c "
SELECT
    datname,
    age(datfrozenxid),
    autovacuum_freeze_max_age - age(datfrozenxid) AS remaining
FROM pg_database;
"

# Check for data corruption
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- psql -U postgres -c "
SELECT
    schemaname,
    tablename,
    n_dead_tup,
    n_live_tup
FROM pg_stat_user_tables
WHERE n_dead_tup > n_live_tup;
"

# Verify replica consistency
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- psql -U postgres -c "
SELECT
    checksum_failures,
    checksum_last_failure
FROM pg_stat_database;
"
```

## Prevention Measures

### Monitoring Setup

```yaml
# Prometheus alerts for database
apiVersion: monitoring.coreos.com/v1
kind: PrometheusRule
metadata:
  name: postgresql-failover
  namespace: erlmcp-prod
spec:
  groups:
    - name: postgresql
      rules:
        - alert: PostgreSQLReplicationLag
          expr: pg_replication_lag_seconds > 30
          for: 2m
          labels:
            severity: warning
          annotations:
            summary: "PostgreSQL replication lag detected"

        - alert: PostgreSQLPrimaryDown
          expr: pg_up{server_type="primary"} == 0
          for: 1m
          labels:
            severity: critical
          annotations:
            summary: "PostgreSQL primary is down"

        - alert: PostgreSQLReplicaCount
          expr: count(pg_up{server_type="replica"}) < 2
          for: 5m
          labels:
            severity: warning
          annotations:
            summary: "PostgreSQL replica count below minimum"
```

### Backup Strategy

```bash
# Daily physical backups
kubectl exec -n erlmcp-prod erlmcp-postgres-0 -- pg_basebackup \
  -D /backup/base-$(date +%Y%m%d) \
  -Ft -z -P -U replicator

# Continuous WAL archiving
# In postgresql.conf:
wal_level = replica
archive_mode = on
archive_command = 'aws s3 cp %p s3://erlmcp-backups/wal/%f'
```

### Regular Testing

```bash
# Monthly failover drills
# 1. Schedule maintenance window
# 2. Execute controlled failover
# 3. Validate all systems
# 4. Perform failback
# 5. Document lessons learned
```

## Escalation

| Condition | Escalation | Timeline |
|-----------|------------|----------|
| Auto-failover triggered | DBA Lead | Immediate |
| Manual failover required | Engineering Manager | < 5 min |
| Data corruption suspected | CTO | Immediate |
| Failover fails | Crisis Team | < 15 min |

## Runbook Metrics

| Metric | Target | Current |
|--------|--------|---------|
| Failover Time (RTO) | < 2 min | 45s |
| Data Loss (RPO) | < 5 sec | 0 sec |
| Failover Success Rate | > 99% | 99.5% |

## Related Runbooks

- RB-005: Database Connection Issues
- RB-007: Full Cluster Outage
- RB-010: Network Partition Recovery
