# erlmcp v3 Deployment Runbooks

**Version:** 3.0.0
**Status:** Production Ready
**Last Updated:** 2026-02-02

---

## Table of Contents

1. [Quick Reference](#quick-reference)
2. [Deployment Procedures](#deployment-procedures)
3. [Emergency Procedures](#emergency-procedures)
4. [Troubleshooting](#troubleshooting)
5. [Maintenance Operations](#maintenance-operations)

---

## Quick Reference

### Contact Information

| Role | Contact | Hours |
|------|---------|-------|
| On-Call Engineer | PagerDuty: `+1-555-ONCALL` | 24/7 |
| DevOps Lead | devops@erlmcp.io | Business hours |
| Tech Lead | tech-lead@erlmcp.io | Business hours |
| Security Team | security@erlmcp.io | 24/7 for critical |
| Change Board | change-board@erlmcp.io | As needed |

### Important URLs

| Resource | URL |
|----------|-----|
| CI/CD Pipeline | https://github.com/banyan-platform/erlmcp/actions |
| Grafana Dashboard | https://grafana.erlmcp.io/d/deployment |
| Logs | https://logs.erlmcp.io |
| Status Page | https://status.erlmcp.io |
| Documentation | https://docs.erlmcp.io |
| Runbook Repository | https://github.com/banyan-platform/erlmcp-runbooks |

### Common Commands

```bash
# Check deployment status
gh run list --workflow=deploy-production

# View specific deployment
gh run view <run-id>

# Cancel deployment
gh run cancel <run-id>

# Approve deployment
gh run approve <run-id>

# Rollback deployment
./scripts/deploy/rollback.sh <environment> <version>

# Emergency rollback
./scripts/deploy/emergency-rollback.sh <environment>

# Health check
curl -sf https://erlmcp.io/health | jq .

# Get current version
curl -sf https://erlmcp.io/api/version
```

---

## Deployment Procedures

### Standard Deployment Flow

#### 1. Development Deployment (Automatic)

**Trigger:** Push to `feature/*` or `develop` branch

**Prerequisites:** None (automatic)

**Procedure:**
```bash
# No action required - deployment is automatic
# Monitor progress at:
# https://github.com/banyan-platform/erlmcp/actions
```

**Verification:**
```bash
# Check pod status
kubectl get pods -n erlmcp-dev -l app=erlmcp

# Check logs
kubectl logs -n erlmcp-dev -l app=erlmcp --tail=50

# Health check
kubectl exec -n erlmcp-dev deployment/erlmcp -- curl -sf localhost:8080/health
```

**Rollback (if needed):**
```bash
# Automatic rollback on failure
# Manual rollback:
kubectl rollout undo deployment/erlmcp -n erlmcp-dev
```

---

#### 2. Staging Deployment

**Trigger:** PR merged to `main` branch

**Prerequisites:**
- [ ] All CI checks passed
- [ ] Code review approved
- [ ] Tests passed in QA

**Procedure:**

1. **Verify PR is ready for deployment**
   ```bash
   gh pr checks <pr-number>
   gh pr view <pr-number>
   ```

2. **Merge PR to main**
   ```bash
   gh pr merge <pr-number> --merge --delete-branch
   ```

3. **Monitor deployment**
   ```bash
   # Watch the workflow
   gh run watch --interval 30s

   # Or check status periodically
   gh run list --workflow=deploy-staging
   ```

4. **Verify deployment**
   ```bash
   # Wait for rollout to complete
   kubectl rollout status deployment/erlmcp -n erlmcp-staging --timeout=10m

   # Check pod health
   kubectl get pods -n erlmcp-staging -l app=erlmcp

   # Run smoke tests
   ./scripts/test/smoke.sh staging
   ```

5. **Begin soak period**
   - Minimum soak: 24 hours
   - Monitor for stability
   - Check metrics every 4 hours

**Verification Checklist:**
- [ ] All pods are `Running`
- [ ] Health checks passing
- [ ] Smoke tests passing
- [ ] No error spikes in logs
- [ ] Metrics within baseline

**Rollback:**
```bash
# Via GitHub Actions UI
# Settings > Environments > staging > Rollback

# Or via CLI
kubectl rollout undo deployment/erlmcp -n erlmcp-staging
```

---

#### 3. Production Deployment

**Trigger:** Tag push matching `v*.*.*`

**Prerequisites:**
- [ ] Successful staging deployment
- [ ] 24-hour soak period completed
- [ ] All post-deploy checks passed
- [ ] Change Board approval obtained
- [ ] Migration guide prepared (if breaking)
- [ ] Rollback plan documented
- [ ] On-call team notified

**Procedure:**

1. **Pre-Deployment Checklist**
   ```bash
   # Run pre-deployment checks
   ./scripts/deploy/pre-check.sh production

   # Verify staging soak
   ./scripts/deploy/verify-soak.sh staging
   ```

2. **Create and push tag**
   ```bash
   # Ensure you're on main branch
   git checkout main
   git pull origin main

   # Create annotated tag
   git tag -a v3.0.0 -m "Release v3.0.0

   Changelog:
   - Feature 1
   - Feature 2
   - Bug fixes

   Tested in staging for 24h.
   Approved by Change Board."

   # Push tag (triggers deployment)
   git push origin v3.0.0
   ```

3. **Monitor deployment pipeline**
   ```bash
   # Watch the workflow
   gh workflow run v3-release.yml

   # Or monitor existing run
   gh run watch <run-id> --interval 30s
   ```

4. **Approve deployment gates**
   - Go to: https://github.com/banyan-platform/erlmcp/environments/production
   - Review each gate
   - Approve when satisfied

5. **Monitor canary deployment**
   ```bash
   # Check canary status
   kubectl get canary erlmcp -n erlmcp-prod -o yaml

   # Watch canary metrics
   kubectl get canary erlmcp -n erlmcp-prod -o jsonpath='{.status.canaryProgress}'
   ```

6. **Verify full deployment**
   ```bash
   # Check rollout status
   kubectl rollout status statefulset/erlmcp -n erlmcp-prod --timeout=15m

   # Check pod distribution
   kubectl get pods -n erlmcp-prod -l app=erlmcp -o wide

   # Verify health endpoints
   curl -sf https://erlmcp.io/health | jq .
   curl -sf https://erlmcp.io/ready | jq .
   ```

7. **Post-deployment verification**
   ```bash
   # Run production smoke tests
   ./scripts/test/smoke-production.sh

   # Verify metrics
   ./scripts/deploy/verify-metrics.sh production

   # Generate deployment report
   ./scripts/deploy/report.sh production > deployment-report.txt
   ```

**Success Criteria:**
- [ ] All pods healthy (0/0 Ready pods = 0)
- [ ] Health checks returning 200
- [ ] Error rate < 1%
- [ ] P95 latency < 500ms
- [ ] No crash loops
- [ ] Canary metrics within thresholds

**Rollback if:**
```bash
# Automatic rollback triggers:
# - 3 consecutive health check failures
# - Error rate > 5% for 5 minutes
# - P95 latency > 10s for 5 minutes
# - Crash loop detected

# Manual rollback:
gh workflow run rollback.yml -f version=v3.0.0 -f reason="manual"
```

---

### Blue-Green Deployment

**When to use:** Major version upgrades, schema changes

**Prerequisites:**
- Blue environment (current) is healthy
- Green environment (new) is provisioned
- Database migrations are idempotent

**Procedure:**

1. **Deploy to Green (hidden)**
   ```bash
   # Deploy new version to green environment
   kubectl apply -f manifests/green/

   # Wait for green to be ready
   kubectl wait --for=condition=available deployment/erlmcp-green -n erlmcp-prod --timeout=10m
   ```

2. **Run tests against Green**
   ```bash
   # Run smoke tests on green
   ./scripts/test/smoke-target.sh https://green.erlmcp.io

   # Run integration tests
   ./scripts/test/integration-target.sh https://green.erlmcp.io
   ```

3. **Switch traffic to Green**
   ```bash
   # Update service selector
   kubectl patch svc erlmcp -n erlmcp-prod -p '{"spec":{"selector":{"version":"green"}}}'

   # Or update ingress/CNAME
   kubectl apply -f manifests/ingress-green.yaml
   ```

4. **Monitor Green**
   ```bash
   # Watch metrics for 15 minutes
   ./scripts/monitor/watch.sh green 900

   # If issues: rollback to blue
   kubectl patch svc erlmcp -n erlmcp-prod -p '{"spec":{"selector":{"version":"blue"}}}'
   ```

5. **Decommission Blue**
   ```bash
   # After 24 hours of stable operation
   kubectl delete -f manifests/blue/
   ```

---

### Canary Deployment

**When to use:** Production rollouts with risk mitigation

**Prerequisites:**
- Flagger installed and configured
- Service mesh or traffic management configured
- Baseline metrics established

**Procedure:**

1. **Initial canary (10% traffic)**
   ```bash
   kubectl apply -f manifests/canary/10-percent.yaml

   # Monitor for 30 minutes
   ./scripts/canary/monitor.sh 30m
   ```

2. **Increase to 50% traffic**
   ```bash
   kubectl apply -f manifests/canary/50-percent.yaml

   # Monitor for 30 minutes
   ./scripts/canary/monitor.sh 30m
   ```

3. **Full rollout (100% traffic)**
   ```bash
   kubectl apply -f manifests/canary/100-percent.yaml

   # Wait for stabilization
   kubectl wait --for=condition=ready canary/erlmcp -n erlmcp-prod --timeout=30m
   ```

4. **Finalize**
   ```bash
   # Promote canary to stable
   kubectl apply -f manifests/production.yaml

   # Clean up canary resources
   kubectl delete -f manifests/canary/
   ```

**Canary Analysis Metrics:**
- HTTP 5xx rate < 1%
- P95 latency increase < 20%
- P99 latency increase < 30%
- Error rate < baseline + 0.5%

---

## Emergency Procedures

### P0 - Critical Incident

**Definition:** Complete system outage affecting all users

**Immediate Actions:**

1. **Declare Incident**
   ```bash
   # Create incident
   pdectl incident create \
     --severity critical \
     --title "Production Outage" \
     --description "All services down"

   # Update status page
   ./scripts/status/update incident --status major_outage
   ```

2. **Page On-Call**
   - On-Call receives automatic page
   - Join war room: https://meet.erlmcp.io/warroom
   - Set incident commander

3. **Assess Impact**
   ```bash
   # Check system status
   ./scripts/health/check-all.sh

   # Check recent deployments
   gh run list --workflow=deploy-production --limit 5

   # Check for recent changes
   git log --since="2 hours ago" --oneline
   ```

4. **Immediate Mitigation**
   ```bash
   # Option 1: Rollback last deployment
   ./scripts/deploy/emergency-rollback.sh production

   # Option 2: Scale up surviving services
   kubectl scale deployment erlmcp -n erlmcp-prod --replicas=20

   # Option 3: Enable maintenance mode
   kubectl annotate ingress erlmcp -n erlmcp-prod nginx.ingress.kubernetes.io/maintenance="true"
   ```

5. **Activate DR Region** (if primary is down)
   ```bash
   # Check DR region status
   kubectl get pods -n erlmcp-dr

   # Failover DNS to DR region
   ./scripts/dr/failover.sh production dr-region

   # Update status page
   ./scripts/status/update --message "Failing over to DR region"
   ```

6. **Communication**
   - Update status page every 15 minutes
   - Post updates to #incidents Slack channel
   - Send email notification to stakeholders

7. **Resolution**
   ```bash
   # Once service is restored
   pdectl incident resolve <incident-id>

   # Update status page
   ./scripts/status/update --status operational

   # Schedule post-mortem
   pdectl postmortem schedule --incident <incident-id>
   ```

---

### P1 - High Severity

**Definition:** Significant degradation affecting many users

**Procedure:**

1. **Declare Incident**
   ```bash
   pdectl incident create --severity high --title "Service Degradation"
   ```

2. **Investigate**
   ```bash
   # Check recent changes
   gh run list --limit 10

   # Check metrics
   ./scripts/health/check-metrics.sh

   # Check logs for errors
   kubectl logs -n erlmcp-prod -l app=erlmcp --tail=1000 | grep -i error
   ```

3. **Mitigate**
   ```bash
   # Option 1: Rollback last deployment
   kubectl rollout undo deployment/erlmcp -n erlmcp-prod

   # Option 2: Scale affected service
   kubectl scale deployment erlmcp -n erlmcp-prod --replicas=10

   # Option 3: Enable circuit breaker
   ./scripts/resilience/circuit-breaker-enable.sh <service>
   ```

4. **Monitor**
   - Watch metrics dashboard
   - Check error rates
   - Verify latency returning to normal

---

### P2 - Medium Severity

**Definition:** Single service or partial degradation

**Procedure:**

1. **Create Ticket**
   ```bash
   # Create Jira ticket
   jira create --type "Bug" --priority "High" --summary "Service degradation"
   ```

2. **Investigate**
   ```bash
   # Check affected service
   kubectl get pods -n erlmcp-prod -l service=<affected>

   # Check service logs
   kubectl logs -n erlmcp-prod -l service=<affected> --tail=500
   ```

3. **Fix**
   ```bash
   # Restart affected pods
   kubectl rollout restart deployment/<service> -n erlmcp-prod

   # Or patch specific issue
   # ...
   ```

---

### Emergency Rollback

**When to use:** Immediate rollback needed due to critical issues

**Procedure:**

1. **Immediate Rollback (via CLI)**
   ```bash
   # Rollback to previous version
   ./scripts/deploy/emergency-rollback.sh production

   # Or rollback to specific version
   ./scripts/deploy/rollback-to.sh production v2.9.5
   ```

2. **Verify Rollback**
   ```bash
   # Check pod status
   kubectl rollout status deployment/erlmcp -n erlmcp-prod

   # Verify health
   curl -sf https://erlmcp.io/health | jq .

   # Run smoke tests
   ./scripts/test/smoke-production.sh
   ```

3. **If Rollback Fails**
   ```bash
   # Scale to zero
   kubectl scale deployment erlmcp -n erlmcp-prod --replicas=0

   # Scale up with previous image
   kubectl set image deployment/erlmcp erlmcp=erlmcp:v2.9.5 -n erlmcp-prod
   kubectl scale deployment erlmcp -n erlmcp-prod --replicas=5
   ```

---

## Troubleshooting

### Deployment Stuck

**Symptoms:** Deployment not progressing, pods stuck in pending

**Diagnosis:**
```bash
# Check workflow status
gh run view <run-id>

# Check pod status
kubectl get pods -n erlmcp-prod

# Describe stuck pod
kubectl describe pod <pod-name> -n erlmcp-prod

# Check events
kubectl get events -n erlmcp-prod --sort-by='.lastTimestamp'
```

**Solutions:**
```bash
# Cancel stuck workflow
gh run cancel <run-id>

# Delete stuck pods
kubectl delete pod <pod-name> -n erlmcp-prod --grace-period=0

# Retry deployment
gh workflow run deploy-production.yml -f version=<version>
```

---

### Health Check Failing

**Symptoms:** Health endpoint returning non-200

**Diagnosis:**
```bash
# Check health endpoint
curl -v https://erlmcp.io/health

# Check pod logs
kubectl logs -n erlmcp-prod -l app=erlmcp --tail=100

# Check pod status
kubectl get pods -n erlmcp-prod -l app=erlmcp

# Check resource usage
kubectl top pods -n erlmcp-prod -l app=erlmcp
```

**Solutions:**
```bash
# Restart deployment
kubectl rollout restart deployment/erlmcp -n erlmcp-prod

# Increase resources
kubectl set resources deployment erlmcp -n erlmcp-prod \
  --requests=cpu=500m,memory=1Gi \
  --limits=cpu=1000m,memory=2Gi

# Rollback if needed
kubectl rollout undo deployment/erlmcp -n erlmcp-prod
```

---

### Crash Looping Pods

**Symptoms:** Pods restarting continuously

**Diagnosis:**
```bash
# Check pod status
kubectl get pods -n erlmcp-prod -l app=erlmcp

# Check logs from previous restart
kubectl logs <pod-name> -n erlmcp-prod --previous

# Check resource limits
kubectl describe pod <pod-name> -n erlmcp-prod | grep -A 5 Limits
```

**Solutions:**
```bash
# Increase memory limit
kubectl set resources deployment erlmcp -n erlmcp-prod \
  --limits=memory=4Gi

# Check for OOMKilled
kubectl describe pod <pod-name> -n erlmcp-prod | grep OOMKilled

# Fix application issue if needed
# (requires code change and new deployment)
```

---

### Canary Failed

**Symptoms:** Canary deployment metrics exceed thresholds

**Diagnosis:**
```bash
# Check canary status
kubectl get canary erlmcp -n erlmcp-prod -o yaml

# Check canary metrics
kubectl describe canary erlmcp -n erlmcp-prod

# Compare metrics
./scripts/canary/compare-metrics.sh baseline canary
```

**Solutions:**
```bash
# Pause canary
kubectl annotate canary erlmcp -n erlmcp-prod flagger.app/paused=true

# Abort canary
kubectl delete canary erlmcp -n erlmcp-prod

# Rollback
kubectl rollout undo deployment/erlmcp -n erlmcp-prod
```

---

### High Memory Usage

**Symptoms:** Pods using excessive memory

**Diagnosis:**
```bash
# Check current usage
kubectl top pods -n erlmcp-prod -l app=erlmcp

# Get detailed metrics
./scripts/metrics/pod-memory.sh <pod-name>

# Check for memory leaks in Erlang
kubectl exec -n erlmcp-prod <pod-name> -- erl -eval 'erlang:display(erlang:memory(total)), init:stop().'
```

**Solutions:**
```bash
# Force garbage collection
kubectl exec -n erlmcp-prod <pod-name> -- erlmcpctl gc

# Restart pods
kubectl rollout restart deployment/erlmcp -n erlmcp-prod

# Increase memory limit
kubectl set resources deployment erlmcp -n erlmcp-prod \
  --limits=memory=4Gi
```

---

## Maintenance Operations

### Health Check Automation

**Daily health check script:**
```bash
#!/bin/bash
# scripts/health/daily-check.sh

echo "=== Daily Health Check ===" | tee daily-health.log
echo "Date: $(date)" | tee -a daily-health.log

# Check all environments
for env in dev staging production; do
    echo "" | tee -a daily-health.log
    echo "=== Environment: $env ===" | tee -a daily-health.log

    # Check pods
    echo "Pods:" | tee -a daily-health.log
    kubectl get pods -n erlmcp-$env | tee -a daily-health.log

    # Check health endpoint
    echo "Health:" | tee -a daily-health.log
    kubectl exec -n erlmcp-$env deployment/erlmcp -- curl -sf localhost:8080/health | tee -a daily-health.log
done

# Check metrics
echo "" | tee -a daily-health.log
echo "=== Metrics ===" | tee -a daily-health.log
curl -s "https://prometheus.erlmcp.io/api/v1/query?query=up" | jq . | tee -a daily-health.log
```

### Backup Procedures

**Daily backup script:**
```bash
#!/bin/bash
# scripts/backup/daily-backup.sh

DATE=$(date +%Y%m%d)
BACKUP_DIR="/backup/erlmcp/$DATE"

mkdir -p $BACKUP_DIR

# Backup Mnesia data
kubectl exec -n erlmcp-prod statefulset/erlmcp -- \
  erlmcpctl backup /data/backup/mnesia-$DATE.dump

# Copy backup locally
kubectl cp erlmcp-prod/erlmcp-0:/data/backup/mnesia-$DATE.dump \
  $BACKUP_DIR/mnesia.dump

# Backup Kubernetes resources
kubectl get all -n erlmcp-prod -o yaml > $BACKUP_DIR/k8s-resources.yaml

# Backup secrets (encrypted)
kubectl get secrets -n erlmcp-prod -o yaml | \
  openssl enc -aes-256-cbc -salt -out $BACKUP_DIR/secrets.enc

# Upload to S3
aws s3 sync $BACKUP_DIR s3://erlmcp-backups/$DATE/
```

### Log Rotation

**Configure log rotation:**
```yaml
# manifests/logrotate-config.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: logrotate-config
  namespace: erlmcp-prod
data:
  erlmcp: |
    /var/log/erlmcp/*.log {
        daily
        rotate 7
        compress
        delaycompress
        missingok
        notifempty
        create 0640 erlmcp erlmcp
        sharedscripts
        postrotate
            kubectl exec -n erlmcp-prod deployment/erlmcp -- erlmcpctl reopen-logs
        endscript
    }
```

---

## Change Log

| Date | Version | Changes |
|------|---------|---------|
| 2026-02-02 | 3.0.0 | Initial runbook creation |
