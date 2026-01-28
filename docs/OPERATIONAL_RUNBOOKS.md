# Operational Runbooks for erlmcp

**Document Date:** January 27, 2026
**Version:** 1.0
**Audience:** Operations, SRE, On-Call Engineers

---

## Table of Contents

1. [Incident Response Procedures](#incident-response)
2. [High Error Rate Alert](#high-error-rate)
3. [High Memory Usage Alert](#high-memory)
4. [High CPU Usage Alert](#high-cpu)
5. [Database Connection Failures](#database-failures)
6. [Pod CrashLoopBackOff](#pod-crashloop)
7. [Scaling Operations](#scaling)
8. [Backup & Recovery](#backup-recovery)
9. [Maintenance Windows](#maintenance)
10. [Troubleshooting Guide](#troubleshooting)
11. [Rollback Procedures](#rollback)
12. [Communication Templates](#communication)

---

## Incident Response Procedures {#incident-response}

### Alert Severity Matrix

| Severity | Response Time | Escalation | Example |
|----------|--------------|------------|---------|
| P1 - Critical | 5 minutes | On-call → Manager → Director | Service down, data loss |
| P2 - High | 15 minutes | On-call → Team Lead → Manager | High error rate (>5%) |
| P3 - Medium | 30 minutes | On-call → Team | Elevated resource usage |
| P4 - Low | Next business day | Ticket queue | Minor issues, warnings |

### Standard Incident Response Flow

```
Detection (Alert fires)
       ↓
Acknowledgment (Slack + PagerDuty)
       ↓
Investigation (Root cause analysis)
       ↓
Mitigation (Quick fix or workaround)
       ↓
Resolution (Permanent fix deployed)
       ↓
Post-Incident Review (Learn & improve)
```

---

## High Error Rate Alert {#high-error-rate}

**Alert Trigger:** Error rate > 5% for 1 minute
**Severity:** P2 (High)
**Response Time:** 15 minutes

### Immediate Actions (0-2 minutes)

1. **Acknowledge Alert**
   ```bash
   # In Slack
   /pd trigger --title "High Error Rate on erlmcp" --severity high
   ```

2. **Get Current Status**
   ```bash
   # Check deployment status
   kubectl get deployment erlmcp-tcps -n erlmcp
   kubectl get pods -n erlmcp -o wide

   # Check recent errors
   kubectl logs -n erlmcp -f deployment/erlmcp-tcps --tail=100
   ```

3. **Verify Alert is Real**
   ```bash
   # Check Prometheus directly
   # Query: rate(http_requests_total{code=~"5.."}[5m]) > 0.05

   # Check Grafana dashboard: erlmcp-overview
   # Navigate to "Error Rate" panel
   ```

### Investigation (2-5 minutes)

1. **Check Recent Deployments**
   ```bash
   # Any recent deployment?
   kubectl rollout history deployment/erlmcp-tcps -n erlmcp

   # If yes, check logs
   kubectl describe deployment erlmcp-tcps -n erlmcp
   ```

2. **Check Application Logs**
   ```bash
   # Stream logs from all pods
   kubectl logs -n erlmcp -f deployment/erlmcp-tcps --all-containers=true

   # Look for patterns:
   # - Connection errors
   # - Timeout errors
   # - Out of memory
   # - Disk full
   ```

3. **Check Downstream Services**
   ```bash
   # Check database connectivity
   kubectl exec -n erlmcp deployment/erlmcp-tcps -- \
     psql -h postgres -c "SELECT 1"

   # Check OpenTelemetry collector
   kubectl logs -n erlmcp deployment/otel-collector

   # Check Prometheus scraping
   curl http://prometheus:9090/api/v1/targets
   ```

### Mitigation (5-10 minutes)

**Option 1: Scale Up (if resource constrained)**
```bash
kubectl scale deployment erlmcp-tcps --replicas=5 -n erlmcp

# Wait for pods to become ready
kubectl rollout status deployment/erlmcp-tcps -n erlmcp --timeout=5m

# Monitor error rate
watch 'kubectl get pods -n erlmcp'
```

**Option 2: Restart Pods (if stuck processes)**
```bash
# Delete problematic pods to trigger restart
kubectl delete pod -n erlmcp deployment/erlmcp-tcps

# Observe new pods starting
kubectl get pods -n erlmcp -w
```

**Option 3: Rollback (if recent deployment)**
```bash
# Check deployment history
kubectl rollout history deployment/erlmcp-tcps -n erlmcp

# Rollback to previous version
kubectl rollout undo deployment/erlmcp-tcps -n erlmcp

# Monitor rollback progress
kubectl rollout status deployment/erlmcp-tcps -n erlmcp --timeout=5m
```

### Resolution (10-15 minutes)

```bash
# Once error rate returns to normal (<1%)
# 1. Verify system is stable (5 min observation)
watch 'curl http://erlmcp:8080/health'

# 2. Check if issue recurs
# 3. Create incident post-mortem
# 4. Document findings
```

### Post-Incident Review (Next business day)

- [ ] Root cause identified
- [ ] Timeline documented
- [ ] Preventive measure created
- [ ] Issue tracked for permanent fix
- [ ] Team informed of findings

---

## High Memory Usage Alert {#high-memory}

**Alert Trigger:** Memory usage > 85% for 3 minutes
**Severity:** P2 (High)
**Response Time:** 15 minutes
**Impact:** Pod eviction and service disruption

### Immediate Actions (0-2 minutes)

1. **Verify Memory Pressure**
   ```bash
   # Check node memory
   kubectl top nodes

   # Check pod memory
   kubectl top pods -n erlmcp

   # Check specific pod
   kubectl describe pod POD_NAME -n erlmcp | grep Memory
   ```

2. **Check for Memory Leaks**
   ```bash
   # Connect to pod and run observer
   kubectl exec -it POD_NAME -n erlmcp -- /bin/sh

   # In Erlang shell
   erlmcp:observer_start()
   # Check Process Info → Memory column
   ```

### Investigation (2-5 minutes)

1. **Identify Memory Consumer**
   ```erlang
   % In erlmcp shell
   erlang:memory().           % Overall memory breakdown
   erlang:process_info(Pid, memory).  % Per-process memory
   ```

2. **Check for Common Causes**
   ```bash
   # Stuck connections
   kubectl exec -it POD_NAME -n erlmcp -- \
     grep -i "connection" /var/log/erlmcp/erlmcp.log | tail -20

   # Large message queue
   # Check with: observer_cli:start()

   # Accumulated process state
   # Review recent configuration changes
   ```

### Mitigation (5-10 minutes)

**Option 1: Scale Up**
```bash
# Add more memory per pod
kubectl set resources deployment erlmcp-tcps \
  --limits memory=2500Mi \
  -n erlmcp

# Scale replicas to distribute load
kubectl scale deployment erlmcp-tcps --replicas=6 -n erlmcp
```

**Option 2: Graceful Restart**
```bash
# Rolling restart to clear memory
kubectl rollout restart deployment/erlmcp-tcps -n erlmcp

# Monitor memory as pods restart
kubectl top pods -n erlmcp -w
```

**Option 3: Database Cleanup**
```bash
# If database is accumulating data
kubectl exec -n erlmcp postgres-pod -- \
  psql -c "VACUUM ANALYZE"

# Check disk usage
du -sh /var/lib/postgresql/data
```

---

## High CPU Usage Alert {#high-cpu}

**Alert Trigger:** CPU usage > 80% for 5 minutes
**Severity:** P2-P3 (Medium-High)
**Response Time:** 15-30 minutes

### Immediate Actions

1. **Verify CPU Pressure**
   ```bash
   kubectl top pods -n erlmcp --sort-by=cpu
   kubectl top nodes --sort-by=cpu
   ```

2. **Identify CPU Consumer**
   ```bash
   # Watch CPU per pod
   watch -n 2 'kubectl top pods -n erlmcp --sort-by=cpu'
   ```

### Investigation

1. **Check Application Profile**
   ```bash
   # Profiling with recon
   kubectl exec -it POD_NAME -n erlmcp -- \
     erl -pa /opt/erlmcp/lib/*/ebin

   % In Erlang:
   recon:top() % See processes using CPU
   ```

2. **Check for Busy Loops**
   ```bash
   # High context switches indicate spinning processes
   iostat -x 1 5

   # High system time indicates scheduler pressure
   ```

### Mitigation

**Option 1: Scale Out**
```bash
kubectl scale deployment erlmcp-tcps --replicas=6 -n erlmcp
```

**Option 2: Optimize Configuration**
```bash
# Reduce concurrent connections
kubectl set env deployment/erlmcp-tcps \
  -e ERLMCP_MAX_CONNECTIONS=500 \
  -n erlmcp
```

**Option 3: Restart Heavy Process**
```bash
# Identify and restart
kubectl delete pod POD_NAME -n erlmcp
```

---

## Database Connection Failures {#database-failures}

**Alert Trigger:** Database connection pool exhaustion
**Severity:** P1 (Critical)
**Response Time:** 5 minutes
**Impact:** Application cannot query database

### Immediate Actions

1. **Verify Database Health**
   ```bash
   # Check PostgreSQL pod
   kubectl exec -n erlmcp postgres-pod -- \
     pg_isready -h localhost -U erlmcp

   # Expected output: "accepting connections"
   ```

2. **Check Connection Counts**
   ```sql
   -- Connect to database
   psql -h postgres -U erlmcp -d erlmcp

   -- Count active connections
   SELECT count(*) FROM pg_stat_activity;

   -- See per-application connections
   SELECT application_name, count(*)
   FROM pg_stat_activity
   GROUP BY application_name;
   ```

### Investigation

1. **Check Connection Pool Status**
   ```bash
   kubectl logs -n erlmcp deployment/erlmcp-tcps | \
     grep -i "connection.*pool"

   # Check poolboy status
   kubectl exec -it POD_NAME -n erlmcp -- \
     erlang:erlang_info(poolboy_pool)
   ```

2. **Identify Hanging Connections**
   ```sql
   -- Find long-running queries
   SELECT pid, now() - pg_stat_activity.query_start AS duration, query
   FROM pg_stat_activity
   WHERE (now() - pg_stat_activity.query_start) > interval '5 minutes'
   ORDER BY duration DESC;
   ```

### Mitigation

**Option 1: Increase Pool Size**
```bash
kubectl set env deployment/erlmcp-tcps \
  -e ERLMCP_DB_POOL_SIZE=50 \
  -n erlmcp

# Restart pods
kubectl rollout restart deployment/erlmcp-tcps -n erlmcp
```

**Option 2: Kill Long-Running Queries**
```sql
-- Terminate specific query
SELECT pg_terminate_backend(pid)
FROM pg_stat_activity
WHERE query LIKE '%slow%' AND pid <> pg_backend_pid();
```

**Option 3: Restart Database**
```bash
kubectl delete pod -n erlmcp postgres-pod

# Wait for recovery
kubectl get pods -n erlmcp -w
```

---

## Pod CrashLoopBackOff {#pod-crashloop}

**Status:** Pod continuously restarts
**Severity:** P1 (Critical)
**Response Time:** 5 minutes

### Immediate Actions

1. **Get Pod Status**
   ```bash
   kubectl describe pod POD_NAME -n erlmcp

   # Look for:
   # - Last State: Terminated with exit code
   # - Events: Last messages
   ```

2. **Get Crash Logs**
   ```bash
   # Previous termination logs
   kubectl logs POD_NAME -n erlmcp --previous

   # Watch current restart
   kubectl logs POD_NAME -n erlmcp -f
   ```

### Common Causes & Solutions

**Cause 1: Configuration Error**
```bash
# Check configmap
kubectl get configmap erlmcp-config -n erlmcp -o yaml

# Check if configuration is valid Erlang syntax
kubectl exec POD_NAME -n erlmcp -- \
  erl -eval "{ok, Config} = file:consult('/config/sys.config'), halt()."
```

**Cause 2: Missing Environment Variables**
```bash
# Check deployment environment
kubectl get deployment erlmcp-tcps -n erlmcp -o yaml | grep -A 20 "env:"

# Compare with sys.config requirements
```

**Cause 3: Memory Limit Exceeded**
```bash
# Increase memory limit
kubectl set resources deployment erlmcp-tcps \
  --limits memory=1500Mi \
  -n erlmcp
```

**Cause 4: Disk Space**
```bash
# Check disk usage
kubectl exec POD_NAME -n erlmcp -- df -h

# Clean up old logs if needed
kubectl exec POD_NAME -n erlmcp -- \
  find /var/log -mtime +7 -delete
```

### Resolution

Once resolved, verify:
```bash
# Pod should be Running
kubectl get pods POD_NAME -n erlmcp

# Check logs are clean
kubectl logs POD_NAME -n erlmcp | tail -20

# Pod ready status should be 1/1
kubectl get pods -o wide POD_NAME -n erlmcp
```

---

## Scaling Operations {#scaling}

### Manual Horizontal Scaling (Add/Remove Replicas)

```bash
# Increase replicas
kubectl scale deployment erlmcp-tcps --replicas=5 -n erlmcp

# Watch scaling progress
kubectl get pods -n erlmcp -w

# Verify deployment is healthy
kubectl rollout status deployment/erlmcp-tcps -n erlmcp
```

### Manual Vertical Scaling (Increase Resources)

```bash
# Increase CPU and memory
kubectl set resources deployment erlmcp-tcps \
  --requests cpu=600m,memory=700Mi \
  --limits cpu=2500m,memory=2500Mi \
  -n erlmcp

# Rolling update will restart pods
kubectl rollout status deployment/erlmcp-tcps -n erlmcp --timeout=10m
```

### Check Current Resource Usage

```bash
# Top resource consumers
kubectl top pods -n erlmcp --sort-by=memory
kubectl top pods -n erlmcp --sort-by=cpu

# Node-level resources
kubectl top nodes
```

---

## Backup & Recovery {#backup-recovery}

### Manual Database Backup

```bash
# Backup PostgreSQL
kubectl exec -n erlmcp postgres-pod -- \
  pg_dump -U erlmcp -d erlmcp > /tmp/erlmcp-backup.sql

# Verify backup size
ls -lh /tmp/erlmcp-backup.sql

# Upload to safe location
gsutil cp /tmp/erlmcp-backup.sql gs://backups/erlmcp/
```

### Database Restore

**Important:** Only do this when instructed by team lead or in disaster recovery

```bash
# 1. Stop application
kubectl scale deployment erlmcp-tcps --replicas=0 -n erlmcp

# 2. Drop current database (CAREFUL!)
kubectl exec -n erlmcp postgres-pod -- \
  dropdb -U postgres erlmcp

# 3. Create fresh database
kubectl exec -n erlmcp postgres-pod -- \
  createdb -U postgres erlmcp

# 4. Restore from backup
kubectl exec -n erlmcp postgres-pod -- \
  psql -U erlmcp -d erlmcp < /tmp/erlmcp-backup.sql

# 5. Restart application
kubectl scale deployment erlmcp-tcps --replicas=3 -n erlmcp

# 6. Verify
kubectl get pods -n erlmcp -w
```

### Cluster Backup (Velero)

```bash
# Trigger manual backup
velero backup create erlmcp-backup-$(date +%s)

# Monitor backup progress
velero backup logs erlmcp-backup-XXX

# List backups
velero backup get
```

---

## Maintenance Windows {#maintenance}

### Pre-Maintenance Checklist

- [ ] Announce in #announcements channel
- [ ] Update status page
- [ ] Notify customers if external service
- [ ] Verify backup is recent
- [ ] Plan rollback if needed

### During Maintenance

```bash
# 1. Drain traffic from cluster (if load balancer)
kubectl patch service erlmcp -n erlmcp \
  -p '{"spec":{"selector":{"maintenance":"true"}}}'

# 2. Scale down replicas
kubectl scale deployment erlmcp-tcps --replicas=1 -n erlmcp

# 3. Perform maintenance tasks (e.g., patching)
# kubectl set image deployment/erlmcp-tcps ...

# 4. Verify functionality
kubectl logs -n erlmcp deployment/erlmcp-tcps --tail=50

# 5. Scale back up
kubectl scale deployment erlmcp-tcps --replicas=3 -n erlmcp

# 6. Restore traffic
kubectl patch service erlmcp -n erlmcp \
  -p '{"spec":{"selector":{"maintenance":null}}}'
```

### Post-Maintenance

- [ ] Smoke tests pass
- [ ] Metrics are normal
- [ ] No errors in logs
- [ ] Announce completion

---

## Troubleshooting Guide {#troubleshooting}

### Pod won't start

```bash
# 1. Check events
kubectl describe pod POD_NAME -n erlmcp

# 2. Check logs
kubectl logs POD_NAME -n erlmcp

# 3. Common issues:
# - Image not found: kubectl get imagepullsecrets
# - Port already in use: kubectl get svc
# - Insufficient resources: kubectl describe nodes
```

### Application won't respond to requests

```bash
# 1. Check service
kubectl get svc -n erlmcp
kubectl endpoints erlmcp -n erlmcp

# 2. Check pods
kubectl get pods -n erlmcp -o wide

# 3. Test connectivity
kubectl exec -it any-pod -n erlmcp -- \
  curl http://erlmcp:8080/health

# 4. Check firewall/network policies
kubectl get networkpolicies -n erlmcp
```

### Database won't connect

```bash
# 1. Check PostgreSQL
kubectl get pods -n erlmcp | grep postgres
kubectl logs -n erlmcp postgres-pod

# 2. Test connection
kubectl exec -n erlmcp postgres-pod -- \
  pg_isready -h localhost

# 3. Check credentials
kubectl get secret erlmcp-secrets -n erlmcp -o yaml

# 4. Check network
kubectl exec erlmcp-pod -n erlmcp -- \
  nc -zv postgres 5432
```

### High latency

```bash
# 1. Check if database is slow
time psql -h postgres -U erlmcp -c "SELECT 1"

# 2. Check network latency
ping postgres

# 3. Check disk I/O
iostat -x 1 5

# 4. Check application logs for slow operations
kubectl logs -n erlmcp deployment/erlmcp-tcps | grep -i "slow\|timeout"
```

---

## Rollback Procedures {#rollback}

### Immediate Rollback (Automatic)

Most deployments rollback automatically on health check failure:

```bash
# Kubernetes automatically rolls back if:
# - Liveness probe fails
# - Health check returns 5xx
# - Pod crashes
```

### Manual Rollback

```bash
# 1. Check deployment history
kubectl rollout history deployment/erlmcp-tcps -n erlmcp

# 2. View specific revision
kubectl rollout history deployment/erlmcp-tcps -n erlmcp --revision=3

# 3. Rollback to previous
kubectl rollout undo deployment/erlmcp-tcps -n erlmcp

# 4. Rollback to specific revision
kubectl rollout undo deployment/erlmcp-tcps -n erlmcp --to-revision=3

# 5. Monitor rollback
kubectl rollout status deployment/erlmcp-tcps -n erlmcp --timeout=5m
```

### Verify Rollback Success

```bash
# 1. Check pods are ready
kubectl get pods -n erlmcp -o wide

# 2. Verify health
curl http://erlmcp:8080/health

# 3. Check logs
kubectl logs -n erlmcp -f deployment/erlmcp-tcps
```

---

## Communication Templates {#communication}

### Initial Alert Acknowledgment

**Channel:** #incidents (Slack)

```
🚨 [P2] High Error Rate Alert - erlmcp
Acknowledged by @{name}
Time: 14:32 UTC
Expected resolution: 15 minutes
Investigating now...
```

### Status Update (Ongoing Issue)

```
📊 Status Update: High Error Rate
Investigation findings:
- Recent deployment identified: v1.2.3
- Error rate currently: 8.5%
- Rollback in progress...
ETA: 5 minutes
```

### Resolution Notification

```
✅ RESOLVED: High Error Rate - erlmcp
Resolution: Rolled back to v1.2.2
Error rate now: 0.2%
Root cause: Connection pool exhaustion
Post-mortem: Will investigate and post findings

Thank you for your patience!
```

### Post-Incident Summary Template

```markdown
## Incident Report: [Title]

**Duration:** 14:32 - 14:47 UTC (15 minutes)
**Severity:** P2 (High)
**Impact:** 5,000 failed requests, 0 data loss

### Root Cause
...

### Timeline
- 14:32 - Alert triggered
- 14:33 - Acknowledged
- 14:35 - Root cause identified
- 14:47 - Resolved via rollback

### Preventive Measures
- [ ] Implement better pre-deployment testing
- [ ] Add connection pool limit monitoring
- [ ] Document scaling procedures

### Owner: @[Engineer]
### Review Date: [Date]
```

---

## Quick Reference

### Essential Commands

```bash
# Status checks
kubectl get pods -n erlmcp
kubectl get svc -n erlmcp
kubectl top pods -n erlmcp

# Logs
kubectl logs -n erlmcp deployment/erlmcp-tcps -f
kubectl logs -n erlmcp deployment/erlmcp-tcps --previous

# Scaling
kubectl scale deployment erlmcp-tcps --replicas=5 -n erlmcp

# Restart
kubectl rollout restart deployment/erlmcp-tcps -n erlmcp

# Rollback
kubectl rollout undo deployment/erlmcp-tcps -n erlmcp

# Health check
curl http://erlmcp:8080/health
curl http://erlmcp:8080/ready
```

### Contacts

- **On-Call Engineer:** Check PagerDuty rotation
- **Team Lead:** Escalate if issue unresolved after 30 min
- **DevOps Team:** #devops Slack channel
- **Database Team:** #database-support Slack channel

### Useful Links

- **Dashboard:** https://grafana.example.com/erlmcp
- **Logs:** https://datadog.example.com/logs
- **Alerts:** https://prometheus.example.com/alerts
- **Status:** https://status.example.com

---

**Last Updated:** January 27, 2026
**Review Cycle:** Quarterly (every 3 months)
**Next Review:** April 27, 2026
