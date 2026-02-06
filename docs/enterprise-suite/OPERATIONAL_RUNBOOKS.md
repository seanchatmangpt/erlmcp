# erlmcp v3 Operational Runbooks for Fortune 5 SRE Teams

## Table of Contents

1. [Introduction](#introduction)
2. [Deployment Runbooks](#deployment-runbooks)
3. [Incident Response Procedures](#incident-response-procedures)
4. [Disaster Recovery Procedures](#disaster-recovery-procedures)
5. [Capacity Planning and Scaling](#capacity-planning-and-scaling)
6. [Backup and Restore Procedures](#backup-and-restore-procedures)
7. [Upgrade and Patch Management](#upgrade-and-patch-management)
8. [Performance Tuning Procedures](#performance-tuning-procedures)
9. [Security Incident Response](#security-incident-response)
10. [Chaos Engineering and Game Days](#chaos-engineering-and-game-days)
11. [On-Call Rotation and Escalation](#on-call-rotation-and-escalation)
12. [Appendices](#appendices)

---

## Introduction

### Purpose

This document provides comprehensive, operator-ready runbooks for Fortune 5 SRE teams managing erlmcp v3 in production environments. All procedures follow the **DOCKER-ONLY CONSTITUTION** - host execution is forbidden.

### Target Audience

- Site Reliability Engineers (SREs)
- DevOps Engineers
- Platform Engineers
- On-Call Operators
- Incident Commanders

### Critical Requirements

**DOCKER-ONLY EXECUTION**
- All commands MUST be executed via Docker
- Host execution is FORBIDDEN and invalidates the work
- Every command includes Docker service mapping
- Proof receipts required for all operations

### Service Gate Mapping

| Operation | Docker Service | Purpose |
|-----------|---------------|---------|
| Compile | `erlmcp-build` | Compilation gate |
| Unit Tests | `erlmcp-unit` | EUnit test gate |
| Integration Tests | `erlmcp-ct` | Common Test gate |
| Quality Checks | `erlmcp-check` | Dialyzer, xref, coverage |
| Benchmarks | `erlmcp-bench` | Performance gate |
| Cluster Tests | `erlmcp-node*` | Distributed Erlang testing |

### Emergency Contacts

| Role | Contact | Escalation Time |
|------|---------|-----------------|
| Primary On-Call SRE | pager-sre@company.com | Immediate |
| Secondary On-Call | pager-sre-backup@company.com | +15 minutes |
| Engineering Lead | engineering-lead@company.com | +30 minutes |
| VP Engineering | vp-eng@company.com | P0 only, +1 hour |
| Executive On-Call | exec-oncall@company.com | P0 catastrophic |

---

## Deployment Runbooks

### 1.1 Standard Deployment (Rolling Update)

**Objective**: Deploy new version with zero downtime using rolling updates
**Duration**: 15-30 minutes
**Risk Level**: Low
**Rollback Time**: 5 minutes

#### Prerequisites Checklist

- [ ] All quality gates passed (compile, test, dialyzer)
- [ ] Smoke tests passed in staging
- [ ] Change approval obtained
- [ ] Backup verified within last 4 hours
- [ ] Monitoring dashboards active
- [ ] On-call team notified
- [ ] Rollback plan confirmed

#### Step-by-Step Procedure

**Step 1: Pre-Deployment Validation**

```bash
# Verify current cluster health
docker compose run --rm erlmcp-check /opt/erlmcp/bin/cluster-health.sh

# Check all quality gates
docker compose run --rm erlmcp-build make compile
docker compose run --rm erlmcp-unit make eunit
docker compose run --rm erlmcp-ct make ct
docker compose run --rm erlmcp-check make check

# Validate deployment artifacts
docker compose run --rm erlmcp-build make release-validate

# Record baseline metrics
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'rate(erlmcp_requests_total[5m])'
```

**Step 2: Build and Tag Release**

```bash
# Build production image
export BUILD_DATE=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
export VCS_REF=$(git rev-parse --short HEAD)
export VERSION=3.0.0

docker compose -f docker-compose.prod.yml build \
  --build-arg BUILD_DATE="${BUILD_DATE}" \
  --build-arg VCS_REF="${VCS_REF}" \
  --build-arg VERSION="${VERSION}"

# Tag with version and git SHA
docker tag erlmcp:${VERSION} erlmcp:${VERSION}-${VCS_REF}
docker tag erlmcp:${VERSION} erlmcp:latest

# Push to registry
docker push erlmcp:${VERSION}-${VCS_REF}
docker push erlmcp:${VERSION}
```

**Step 3: Deploy with Rolling Update**

```bash
# Update service with rolling update
docker stack deploy \
  --compose-file docker-swarm/docker-compose.prod.yml \
  --with-registry-auth \
  erlmcp-prod

# Monitor rollout progress
docker service ps erlmcp-prod_erlmcp --filter "desired-state=running"

# Watch service convergence
watch -n 2 'docker service ls | grep erlmcp-prod'
```

**Step 4: Health Verification**

```bash
# Wait for health checks to pass
for i in {1..30}; do
  docker compose run --rm erlmcp-check curl -f http://erlmcp:9090/health && break
  echo "Waiting for health check... attempt $i/30"
  sleep 10
done

# Verify all replicas healthy
docker service ps erlmcp-prod_erlmcp \
  --filter "desired-state=running" \
  --format "table {{.Name}}\t{{.CurrentState}}"

# Check cluster membership
docker compose run --rm erlmcp-node \
  /opt/erlmcp/bin/erlmcp rpc erlang nodes
```

**Step 5: Post-Deployment Validation**

```bash
# Run smoke tests against production
docker compose run --rm erlmcp-ct \
  make test-smoke ERLMCP_ENV=production

# Verify metrics collection
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'up{job="erlmcp"}'

# Check error rates
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'rate(erlmcp_errors_total[5m])'

# Validate zero dropped requests
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/validate-deployment.sh
```

**Step 6: Documentation and Communication**

```bash
# Generate deployment receipt
cat > deployment-receipt-${VERSION}-${VCS_REF}.json <<EOF
{
  "version": "${VERSION}",
  "git_sha": "${VCS_REF}",
  "image_digest": "$(docker inspect erlmcp:${VERSION} --format='{{.Id}}')",
  "deployed_at": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "deployed_by": "${USER}",
  "quality_gates": "PASS",
  "smoke_tests": "PASS"
}
EOF

# Notify team
echo "✅ Deployment ${VERSION}-${VCS_REF} completed successfully" | \
  tee -a deployment.log
```

#### Rollback Procedure

```bash
# Immediate rollback to previous version
export PREVIOUS_VERSION=3.0.0-abc1234

docker stack deploy \
  --compose-file docker-swarm/docker-compose.prod.yml \
  --with-registry-auth \
  erlmcp-prod

# Override image version
docker service update \
  --image erlmcp:${PREVIOUS_VERSION} \
  erlmcp-prod_erlmcp

# Verify rollback
docker service ps erlmcp-prod_erlmcp --filter "desired-state=running"
```

#### Success Criteria

- [ ] All replicas running with `RUNNING` state
- [ ] Health checks passing for 5+ minutes
- [ ] Error rate < 0.1%
- [ ] P95 latency within baseline ±10%
- [ ] Zero dropped requests during deployment
- [ ] Cluster fully connected (all nodes visible)

---

### 1.2 Canary Deployment

**Objective**: Deploy to subset of traffic for risk mitigation
**Duration**: 45-60 minutes
**Risk Level**: Very Low
**Canary Traffic**: 5% → 25% → 50% → 100%

#### Step-by-Step Procedure

**Step 1: Deploy Canary (5% Traffic)**

```bash
# Create canary service
docker service create \
  --name erlmcp-canary \
  --replicas 1 \
  --network erlmcp-overlay \
  --label traefik.enable=true \
  --label traefik.http.routers.erlmcp-canary.rule="Host(\`api.example.com\`) && Headers(\`X-Canary\`, \`true\`)" \
  --label traefik.http.services.erlmcp-canary.loadbalancer.server.port=8080 \
  --label traefik.http.services.erlmcp-canary.loadbalancer.sticky.cookie=true \
  erlmcp:${VERSION}-${VCS_REF}

# Configure 5% traffic split
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/configure-canary.sh --traffic 5
```

**Step 2: Monitor Canary Metrics (15 minutes)**

```bash
# Compare canary vs production metrics
docker compose --profile monitoring exec grafana-cli \
  dashboard render \
  --dashboard-uid canary-comparison \
  --output /tmp/canary-metrics-5pct.pdf

# Check canary error rate
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'rate(erlmcp_errors_total{service="canary"}[5m])'

# Compare latency
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'histogram_quantile(0.95, rate(erlmcp_request_duration_seconds_bucket{service="canary"}[5m]))'
```

**Decision Point 1: Continue or Abort?**

```
IF canary_error_rate > production_error_rate * 1.1 THEN
  → ABORT: Rollback canary
ELSE IF canary_p95_latency > production_p95_latency * 1.2 THEN
  → ABORT: Rollback canary
ELSE
  → CONTINUE: Increase to 25%
END IF
```

**Step 3: Increase to 25% Traffic**

```bash
# Update traffic split
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/configure-canary.sh --traffic 25

# Monitor for 15 minutes
# (Repeat metric checks from Step 2)
```

**Step 4: Increase to 50% Traffic**

```bash
# Update traffic split
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/configure-canary.sh --traffic 50

# Monitor for 15 minutes
# (Repeat metric checks)
```

**Step 5: Promote to 100% (Full Deployment)**

```bash
# Full cutover
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/configure-canary.sh --traffic 100

# Wait 10 minutes for stability

# Remove old version
docker service rm erlmcp-prod_erlmcp

# Rename canary to production
docker service update --name erlmcp-prod_erlmcp erlmcp-canary
```

#### Abort/Rollback Procedure

```bash
# Immediate traffic cutover to production
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/configure-canary.sh --traffic 0

# Remove canary service
docker service rm erlmcp-canary

# Document abort reason
echo "CANARY ABORTED: [reason]" >> deployment.log
```

---

### 1.3 Blue-Green Deployment

**Objective**: Zero-downtime deployment with instant rollback capability
**Duration**: 30-45 minutes
**Risk Level**: Very Low
**Rollback Time**: < 30 seconds

#### Architecture

```
┌─────────────────┐
│  Load Balancer  │
└────────┬────────┘
         │
    ┌────┴────┐
    │         │
┌───▼──┐  ┌──▼───┐
│ BLUE │  │GREEN │
│(old) │  │(new) │
└──────┘  └──────┘
```

#### Step-by-Step Procedure

**Step 1: Deploy Green Environment**

```bash
# Deploy green stack
docker stack deploy \
  --compose-file docker-swarm/docker-compose.bluegreen.yml \
  erlmcp-bluegreen

# Verify green deployment
docker service ls | grep erlmcp-green

# Wait for green to be healthy
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/wait-for-healthy.sh erlmcp-green 300
```

**Step 2: Smoke Test Green Environment**

```bash
# Run smoke tests against green
docker compose run --rm erlmcp-ct \
  bash -c "export ERLMCP_HOST=erlmcp-green && make test-smoke"

# Verify green metrics
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'up{service="erlmcp-green"}'

# Manual verification (10% shadow traffic)
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/configure-shadow-traffic.sh --target green --percent 10

# Monitor for 5 minutes
```

**Step 3: Traffic Cutover (Blue → Green)**

```bash
# Update load balancer to route to green
docker service update \
  --label-add traefik.http.routers.erlmcp.service=erlmcp-green \
  erlmcp-bluegreen_traefik

# Verify traffic routing
curl -H "Host: api.example.com" http://localhost/health | \
  jq '.environment'
# Expected: "green"

# Monitor metrics for 5 minutes
docker compose --profile monitoring exec grafana-cli \
  dashboard render \
  --dashboard-uid bluegreen-cutover \
  --output /tmp/cutover-metrics.pdf
```

**Step 4: Validation Period**

```bash
# Monitor green for 15 minutes
# Check for:
# - Error rate < 0.1%
# - P95 latency within baseline
# - No memory leaks
# - No connection errors

# Continuous monitoring
watch -n 10 'docker compose --profile monitoring exec prometheus \
  promtool query instant "http://localhost:9090" \
  "rate(erlmcp_errors_total{service=\"erlmcp-green\"}[5m])"'
```

**Step 5: Decommission Blue**

```bash
# Scale down blue
docker service scale erlmcp-bluegreen_erlmcp-blue=0

# Wait 1 hour for monitoring

# Remove blue if stable
docker service rm erlmcp-bluegreen_erlmcp-blue
docker volume rm erlmcp-data-blue erlmcp-logs-blue
```

#### Instant Rollback Procedure

```bash
# Immediate cutover back to blue
docker service update \
  --label-add traefik.http.routers.erlmcp.service=erlmcp-blue \
  erlmcp-bluegreen_traefik

# Verify rollback
curl -H "Host: api.example.com" http://localhost/health | \
  jq '.environment'
# Expected: "blue"

# Scale down green
docker service scale erlmcp-bluegreen_erlmcp-green=0
```

**Rollback Time**: < 30 seconds

#### Success Criteria

- [ ] Green environment fully healthy
- [ ] Traffic routing 100% to green
- [ ] Error rate < 0.1%
- [ ] No increase in P95 latency
- [ ] Blue environment available for instant rollback (first hour)

---

## Incident Response Procedures

### 2.1 Incident Severity Levels

| Severity | Definition | Response Time | Escalation |
|----------|-----------|---------------|------------|
| **P0** | Complete service outage, data loss risk | < 5 minutes | Immediate, all hands |
| **P1** | Partial outage, major degradation | < 15 minutes | On-call + lead |
| **P2** | Minor degradation, workaround available | < 1 hour | On-call SRE |
| **P3** | Cosmetic issue, no user impact | < 24 hours | Best effort |

### 2.2 Incident Response Decision Tree

```
┌─────────────────────┐
│ Incident Detected   │
└──────────┬──────────┘
           │
    ┌──────▼──────┐
    │ Assess      │
    │ Severity    │
    └──────┬──────┘
           │
    ┌──────┴──────┬──────────┬──────────┬──────────┐
    ▼             ▼          ▼          ▼          ▼
┌───────┐   ┌────────┐  ┌────────┐ ┌────────┐ ┌────────┐
│  P0   │   │   P1   │  │   P2   │ │   P3   │ │Unknown │
│ CRIT  │   │  HIGH  │  │  MED   │ │  LOW   │ │Escalate│
└───┬───┘   └───┬────┘  └───┬────┘ └───┬────┘ └───┬────┘
    │           │            │          │          │
    ▼           ▼            ▼          ▼          ▼
 Page All    Page Lead    Normal      Ticket    Escalate
  + Exec                   Response              to Lead
```

### 2.3 P0 Incident Response (Complete Outage)

**Definition**: Complete service unavailable, data loss imminent, customer impact severe

#### Immediate Actions (First 5 Minutes)

**Step 1: Acknowledge and Declare**

```bash
# Acknowledge incident
echo "P0 INCIDENT DECLARED: $(date -u +"%Y-%m-%dT%H:%M:%SZ")" | \
  tee incident-$(date +%Y%m%d-%H%M%S).log

# Page all hands
# Use PagerDuty, Slack, or company alerting system

# Start war room
# Bridge line: 1-800-XXX-XXXX, Code: XXXX
```

**Step 2: Assess Blast Radius**

```bash
# Check service status
docker service ls | grep erlmcp

# Check cluster health
docker compose run --rm erlmcp-check /opt/erlmcp/bin/cluster-health.sh

# Check monitoring
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' 'up{job="erlmcp"}'

# Check recent deployments
docker service ps erlmcp-prod_erlmcp --format "{{.UpdatedAt}}\t{{.CurrentState}}"
```

**Step 3: Immediate Mitigation**

```bash
# If recent deployment, ROLLBACK IMMEDIATELY
export LAST_KNOWN_GOOD="3.0.0-abc1234"

docker service update \
  --image erlmcp:${LAST_KNOWN_GOOD} \
  --force \
  erlmcp-prod_erlmcp

# If database issue, activate read-only mode
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/set-readonly-mode.sh --enable

# If resource exhaustion, scale up immediately
docker service scale erlmcp-prod_erlmcp=10

# If network partition, restart affected nodes
docker compose run --rm erlmcp-node \
  /opt/erlmcp/bin/restart-partitioned-nodes.sh
```

**Step 4: Verify Mitigation**

```bash
# Check health
for i in {1..30}; do
  docker compose run --rm erlmcp-check curl -f http://erlmcp:9090/health && break
  echo "Waiting for recovery... attempt $i/30"
  sleep 10
done

# Verify traffic flow
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'rate(erlmcp_requests_total[1m])'
```

#### Resolution (Next 30 Minutes)

**Step 5: Root Cause Analysis**

```bash
# Collect logs
docker service logs --tail 1000 erlmcp-prod_erlmcp > p0-incident-logs.txt

# Check resource usage
docker stats --no-stream > p0-incident-stats.txt

# Extract metrics
docker compose --profile monitoring exec prometheus \
  promtool query range 'http://localhost:9090' \
  --start $(date -u -d '1 hour ago' +%s) \
  --end $(date -u +%s) \
  'rate(erlmcp_errors_total[5m])' \
  > p0-incident-metrics.txt

# Check cluster events
docker compose run --rm erlmcp-node \
  /opt/erlmcp/bin/erlmcp rpc erlang statistics wall_clock
```

**Step 6: Stabilization**

```bash
# Ensure all replicas healthy
docker service ps erlmcp-prod_erlmcp --filter "desired-state=running"

# Run full validation
docker compose run --rm erlmcp-ct make test-smoke

# Verify metrics stability
docker compose --profile monitoring exec grafana-cli \
  dashboard render \
  --dashboard-uid incident-recovery \
  --output /tmp/p0-recovery-metrics.pdf
```

#### Post-Incident (Next 2 Hours)

**Step 7: Communication**

```bash
# Generate incident report
cat > incident-report-P0-$(date +%Y%m%d).md <<EOF
# P0 Incident Report

## Timeline
- **Detection**: $(date -u +"%Y-%m-%dT%H:%M:%SZ")
- **Mitigation**: [time]
- **Resolution**: [time]
- **Duration**: [X minutes]

## Impact
- **Affected Users**: [number/percentage]
- **Service Availability**: [percentage]
- **Data Loss**: [yes/no, details]

## Root Cause
[Description]

## Resolution
[Actions taken]

## Prevention
[Action items]

## Follow-up
- [ ] Fix root cause
- [ ] Update runbooks
- [ ] Add monitoring
- [ ] Schedule postmortem
EOF
```

**Step 8: Postmortem (Within 48 Hours)**

- Schedule blameless postmortem
- Identify action items
- Update runbooks
- Implement preventive measures

---

### 2.4 P1 Incident Response (Partial Outage)

**Definition**: Significant degradation, partial unavailability, major feature broken

#### Response Procedure (15 Minutes)

**Step 1: Assess and Triage**

```bash
# Check service health
docker compose run --rm erlmcp-check /opt/erlmcp/bin/cluster-health.sh

# Identify affected components
docker service ps erlmcp-prod_erlmcp --filter "desired-state=running"

# Check error rates by component
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'sum by (component) (rate(erlmcp_errors_total[5m]))'
```

**Step 2: Isolate Issue**

```bash
# If specific node failing, remove from cluster
docker service update \
  --constraint-rm node.hostname==failing-node \
  erlmcp-prod_erlmcp

# If specific service degraded, scale it independently
docker service scale erlmcp-prod_erlmcp-worker=5

# If network issue, verify overlay network
docker network inspect erlmcp-overlay
```

**Step 3: Apply Workaround**

```bash
# Route around failing component
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/disable-feature.sh --feature failing-feature

# Increase timeouts
docker service update \
  --env-add REQUEST_TIMEOUT=30000 \
  erlmcp-prod_erlmcp

# Enable circuit breaker
docker service update \
  --env-add CIRCUIT_BREAKER_ENABLED=true \
  erlmcp-prod_erlmcp
```

**Step 4: Monitor and Stabilize**

```bash
# Monitor for 15 minutes
watch -n 30 'docker compose --profile monitoring exec prometheus \
  promtool query instant "http://localhost:9090" \
  "rate(erlmcp_errors_total[5m])"'

# Verify workaround effective
docker compose run --rm erlmcp-ct make test-smoke
```

---

### 2.5 P2 Incident Response (Minor Degradation)

**Definition**: Minor issue, workaround available, limited customer impact

#### Response Procedure (1 Hour)

**Step 1: Document and Track**

```bash
# Create incident ticket
cat > p2-incident-$(date +%Y%m%d-%H%M%S).md <<EOF
# P2 Incident

## Symptom
[Description]

## Impact
[Scope]

## Workaround
[Available workaround]

## Investigation
[Steps]
EOF
```

**Step 2: Investigate**

```bash
# Collect diagnostics
docker service logs --tail 500 erlmcp-prod_erlmcp > p2-logs.txt

# Check metrics
docker compose --profile monitoring exec prometheus \
  promtool query range 'http://localhost:9090' \
  --start $(date -u -d '1 hour ago' +%s) \
  --end $(date -u +%s) \
  'rate(erlmcp_requests_total[5m])'
```

**Step 3: Apply Fix or Workaround**

```bash
# Apply configuration change
docker service update \
  --env-add FEATURE_FLAG_XYZ=false \
  erlmcp-prod_erlmcp

# Or schedule fix deployment
# (Follow standard deployment runbook)
```

---

### 2.6 P3 Incident Response (Low Priority)

**Definition**: Cosmetic issue, no functional impact, low priority

#### Response Procedure (24 Hours)

```bash
# Create ticket for backlog
echo "P3: [description]" >> backlog.txt

# Schedule fix in next sprint
# No immediate action required
```

---

## Disaster Recovery Procedures

### 3.1 Disaster Recovery Objectives

| Objective | Target | Maximum |
|-----------|--------|---------|
| **RTO** (Recovery Time Objective) | 30 minutes | 1 hour |
| **RPO** (Recovery Point Objective) | 1 minute | 5 minutes |
| **Data Loss** | 0 transactions | < 100 transactions |
| **Service Availability** | 99.99% | 99.9% |

### 3.2 Disaster Scenarios

| Scenario | Likelihood | Impact | Recovery Procedure |
|----------|-----------|--------|-------------------|
| Single Node Failure | High | Low | Automatic (cluster self-heal) |
| Data Center Outage | Medium | High | Failover to secondary region |
| Database Corruption | Low | Critical | Restore from backup |
| Complete Infrastructure Loss | Very Low | Catastrophic | DR site activation |
| Cyber Attack / Ransomware | Low | Critical | Isolated restore |

### 3.3 Single Region Failure (RTO: 30 minutes)

**Scenario**: Primary data center completely unavailable

#### Failover Procedure

**Step 1: Detect and Declare (5 minutes)**

```bash
# Verify primary region down
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/check-region.sh --region us-east-1

# Expected: UNREACHABLE

# Declare DR event
echo "DR EVENT: Primary region failure at $(date -u)" | \
  tee dr-event-$(date +%Y%m%d-%H%M%S).log

# Page DR team
# Use emergency contact list
```

**Step 2: Activate Secondary Region (10 minutes)**

```bash
# Promote secondary database to primary
docker compose -f docker-compose.dr.yml run --rm erlmcp-check \
  /opt/erlmcp/bin/promote-secondary-db.sh --region us-west-2

# Start application stack in DR region
docker stack deploy \
  --compose-file docker-swarm/docker-compose.dr.yml \
  erlmcp-dr

# Verify stack deployment
docker service ls | grep erlmcp-dr
```

**Step 3: Update DNS and Load Balancers (5 minutes)**

```bash
# Update DNS to point to DR region
# (Automated via Terraform or manual via DNS provider)

docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/update-dns.sh \
  --region us-west-2 \
  --ttl 60

# Verify DNS propagation
dig +short api.example.com
# Expected: [DR region IP]

# Update global load balancer
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/update-glb.sh --target us-west-2
```

**Step 4: Validate DR Environment (10 minutes)**

```bash
# Run smoke tests
docker compose -f docker-compose.dr.yml run --rm erlmcp-ct \
  make test-smoke

# Verify data consistency
docker compose -f docker-compose.dr.yml run --rm erlmcp-check \
  /opt/erlmcp/bin/validate-data-consistency.sh

# Check replication lag (should be 0)
docker compose -f docker-compose.dr.yml run --rm erlmcp-check \
  /opt/erlmcp/bin/check-replication-lag.sh

# Monitor metrics
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'up{job="erlmcp",region="us-west-2"}'
```

**Total Time**: 30 minutes
**RPO**: < 5 minutes (continuous replication)

#### Failback Procedure (Return to Primary)

**Execute after primary region restored**

```bash
# Step 1: Verify primary region healthy
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/check-region.sh --region us-east-1

# Step 2: Sync data DR → Primary
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/sync-regions.sh \
  --source us-west-2 \
  --target us-east-1

# Step 3: Blue-Green cutover back to primary
# (Follow blue-green deployment runbook)

# Step 4: Verify failback
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/validate-failback.sh
```

---

### 3.4 Database Corruption Recovery (RTO: 45 minutes)

**Scenario**: Database corruption detected, data integrity compromised

#### Recovery Procedure

**Step 1: Assess Corruption (5 minutes)**

```bash
# Stop writes to database
docker service update \
  --env-add ERLMCP_DB_READONLY=true \
  erlmcp-prod_erlmcp

# Check corruption extent
docker compose run --rm postgres \
  psql -U erlmcp -c "SELECT pg_database_size('erlmcp');"

# Run integrity checks
docker compose run --rm postgres \
  pg_dump -U erlmcp --schema-only erlmcp > schema-check.sql
```

**Step 2: Identify Recovery Point (5 minutes)**

```bash
# List available backups
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/list-backups.sh

# Expected output:
# 2026-02-06-12:00:00 (4 hours ago) - VERIFIED
# 2026-02-06-08:00:00 (8 hours ago) - VERIFIED
# 2026-02-06-04:00:00 (12 hours ago) - VERIFIED

# Select most recent uncorrupted backup
export RESTORE_POINT="2026-02-06-12:00:00"
```

**Step 3: Restore Database (30 minutes)**

```bash
# Stop application
docker service scale erlmcp-prod_erlmcp=0

# Restore database from backup
docker compose run --rm postgres \
  /opt/erlmcp/bin/restore-db.sh \
  --backup ${RESTORE_POINT} \
  --target erlmcp

# Verify restore
docker compose run --rm postgres \
  psql -U erlmcp -c "SELECT COUNT(*) FROM users;"

# Replay WAL logs (minimize data loss)
docker compose run --rm postgres \
  /opt/erlmcp/bin/replay-wal.sh \
  --from ${RESTORE_POINT} \
  --to $(date -u +"%Y-%m-%d-%H:%M:%S")
```

**Step 4: Validate and Resume (5 minutes)**

```bash
# Run data integrity checks
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/validate-db-integrity.sh

# Start application
docker service scale erlmcp-prod_erlmcp=5

# Verify application connectivity
docker compose run --rm erlmcp-ct make test-smoke

# Re-enable writes
docker service update \
  --env-rm ERLMCP_DB_READONLY \
  erlmcp-prod_erlmcp
```

**Total Time**: 45 minutes
**Data Loss**: 1-5 minutes (since last backup/WAL)

---

### 3.5 Complete Infrastructure Loss (RTO: 1 hour)

**Scenario**: Catastrophic failure, all infrastructure destroyed

#### Recovery Procedure (Cold Start)

**Step 1: Provision Infrastructure (20 minutes)**

```bash
# Provision via Terraform/IaC
cd terraform/disaster-recovery

# Initialize
docker run --rm -v $(pwd):/workspace \
  hashicorp/terraform:latest init

# Apply infrastructure
docker run --rm -v $(pwd):/workspace \
  hashicorp/terraform:latest apply -auto-approve

# Verify provisioning
docker run --rm -v $(pwd):/workspace \
  hashicorp/terraform:latest output
```

**Step 2: Restore from Cold Storage (25 minutes)**

```bash
# Restore database from S3/cold storage
docker compose run --rm erlmcp-check \
  aws s3 cp s3://erlmcp-dr-backups/latest.sql.gz - | \
  gunzip | \
  docker compose run --rm postgres \
    psql -U erlmcp

# Restore configuration
docker compose run --rm erlmcp-check \
  aws s3 sync s3://erlmcp-dr-config/ /opt/erlmcp/config/

# Restore application state
docker compose run --rm erlmcp-check \
  aws s3 sync s3://erlmcp-dr-state/ /var/lib/erlmcp/
```

**Step 3: Deploy Application Stack (10 minutes)**

```bash
# Pull images from registry
docker pull erlmcp:3.0.0

# Deploy stack
docker stack deploy \
  --compose-file docker-swarm/docker-compose.prod.yml \
  erlmcp-prod

# Wait for healthy
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/wait-for-healthy.sh erlmcp-prod 600
```

**Step 4: Validate and Go Live (5 minutes)**

```bash
# Full validation
docker compose run --rm erlmcp-ct make test-full

# Update DNS
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/update-dns.sh --region new-region

# Monitor metrics
watch -n 10 'docker compose --profile monitoring exec prometheus \
  promtool query instant "http://localhost:9090" "up{job=\"erlmcp\"}"'
```

**Total Time**: 60 minutes
**Data Loss**: 5 minutes (RPO from last off-site backup)

---

## Capacity Planning and Scaling

### 4.1 Capacity Thresholds

| Metric | Warning | Critical | Action |
|--------|---------|----------|--------|
| CPU Usage | 70% | 85% | Scale out |
| Memory Usage | 75% | 90% | Scale out |
| Disk Usage | 80% | 90% | Add storage |
| Request Queue | 1000 | 5000 | Scale out |
| Connection Pool | 80% | 95% | Increase pool |
| Database Connections | 80% | 95% | Scale DB |

### 4.2 Horizontal Scaling (Scale Out)

**Scenario**: Increased traffic, need more capacity

#### Manual Scaling Procedure

```bash
# Check current load
docker service ps erlmcp-prod_erlmcp

# Current: 5 replicas
# Target: 10 replicas

# Scale out
docker service scale erlmcp-prod_erlmcp=10

# Monitor rollout
watch -n 2 'docker service ps erlmcp-prod_erlmcp'

# Verify load distribution
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'sum by (instance) (rate(erlmcp_requests_total[5m]))'
```

#### Auto-Scaling Configuration

```bash
# Configure auto-scaling (Docker Swarm)
docker service update \
  --replicas-max-per-node 2 \
  --update-parallelism 2 \
  erlmcp-prod_erlmcp

# For Kubernetes, apply HPA:
cat > hpa.yaml <<EOF
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: erlmcp-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: erlmcp
  minReplicas: 5
  maxReplicas: 50
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 75
EOF

kubectl apply -f hpa.yaml
```

### 4.3 Vertical Scaling (Scale Up)

**Scenario**: Individual nodes need more resources

```bash
# Update resource limits
docker service update \
  --limit-cpu 4.0 \
  --limit-memory 8G \
  --reserve-cpu 2.0 \
  --reserve-memory 4G \
  erlmcp-prod_erlmcp

# Rolling update will apply new limits
docker service ps erlmcp-prod_erlmcp
```

### 4.4 Database Scaling

**Scenario**: Database becoming bottleneck

#### Read Replica Scaling

```bash
# Add read replica
docker service create \
  --name postgres-replica-1 \
  --network erlmcp-overlay \
  --env POSTGRES_MASTER_HOST=postgres \
  --env POSTGRES_REPLICA=true \
  postgres:16-alpine

# Configure application to use replica for reads
docker service update \
  --env-add DB_READ_REPLICA=postgres-replica-1:5432 \
  erlmcp-prod_erlmcp
```

#### Connection Pool Tuning

```bash
# Increase connection pool
docker service update \
  --env-add DB_POOL_SIZE=50 \
  --env-add DB_POOL_MAX=100 \
  erlmcp-prod_erlmcp
```

### 4.5 Capacity Planning Matrix

| Current RPS | Replicas | CPU/Replica | Memory/Replica | Database Connections |
|-------------|----------|-------------|----------------|----------------------|
| 1,000 | 5 | 2 cores | 4 GB | 20 |
| 5,000 | 10 | 2 cores | 4 GB | 40 |
| 10,000 | 20 | 2 cores | 4 GB | 80 |
| 50,000 | 50 | 4 cores | 8 GB | 200 |
| 100,000 | 100 | 4 cores | 8 GB | 500 |

---

## Backup and Restore Procedures

### 5.1 Backup Strategy

| Component | Frequency | Retention | Storage | RPO |
|-----------|-----------|-----------|---------|-----|
| Database (Full) | Daily 02:00 UTC | 30 days | S3 Glacier | 24 hours |
| Database (Incremental) | Every 15 minutes | 7 days | S3 Standard | 15 minutes |
| Database (WAL) | Continuous | 7 days | S3 Standard | 1 minute |
| Configuration | On change | 90 days | Git + S3 | 0 minutes |
| Application State | Hourly | 7 days | S3 Standard | 1 hour |
| Logs | Continuous | 90 days | S3 Standard | Real-time |

### 5.2 Database Backup Procedure

#### Full Backup (Daily)

```bash
# Trigger full backup
docker compose run --rm postgres \
  /opt/erlmcp/bin/backup-database.sh \
  --type full \
  --compression gzip \
  --output s3://erlmcp-backups/daily/

# Verify backup
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/verify-backup.sh \
  --backup s3://erlmcp-backups/daily/$(date +%Y-%m-%d).sql.gz

# Generate backup receipt
cat > backup-receipt-$(date +%Y%m%d).json <<EOF
{
  "type": "full",
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "size": "$(du -h backup.sql.gz | cut -f1)",
  "checksum": "$(sha256sum backup.sql.gz | cut -d' ' -f1)",
  "verified": true
}
EOF
```

#### Incremental Backup (Every 15 Minutes)

```bash
# Trigger incremental backup
docker compose run --rm postgres \
  /opt/erlmcp/bin/backup-database.sh \
  --type incremental \
  --since "15 minutes ago" \
  --output s3://erlmcp-backups/incremental/

# Automated via cron:
# */15 * * * * docker compose run --rm postgres /opt/erlmcp/bin/backup-database.sh --type incremental
```

#### Continuous WAL Archiving

```bash
# Configure WAL archiving
docker service update \
  --env-add POSTGRES_WAL_ARCHIVE=true \
  --env-add POSTGRES_WAL_ARCHIVE_COMMAND="aws s3 cp %p s3://erlmcp-backups/wal/%f" \
  erlmcp-prod_postgres

# Verify WAL archiving
docker compose run --rm erlmcp-check \
  aws s3 ls s3://erlmcp-backups/wal/
```

### 5.3 Configuration Backup

```bash
# Backup all configuration
docker compose run --rm erlmcp-check \
  tar czf config-backup-$(date +%Y%m%d).tar.gz \
  /opt/erlmcp/config \
  /opt/erlmcp/etc \
  /opt/erlmcp/vm.args

# Upload to S3
docker compose run --rm erlmcp-check \
  aws s3 cp config-backup-$(date +%Y%m%d).tar.gz \
  s3://erlmcp-backups/config/

# Commit to Git (version control)
cd /opt/erlmcp/config
git add .
git commit -m "Config backup $(date +%Y-%m-%d)"
git push origin main
```

### 5.4 Restore Procedures

#### Full Database Restore

```bash
# Step 1: Stop application
docker service scale erlmcp-prod_erlmcp=0

# Step 2: Download backup
docker compose run --rm erlmcp-check \
  aws s3 cp s3://erlmcp-backups/daily/2026-02-06.sql.gz backup.sql.gz

# Step 3: Verify checksum
echo "expected_checksum backup.sql.gz" | sha256sum -c

# Step 4: Restore
docker compose run --rm postgres \
  bash -c "gunzip < backup.sql.gz | psql -U erlmcp"

# Step 5: Replay WAL (minimize data loss)
docker compose run --rm postgres \
  /opt/erlmcp/bin/replay-wal.sh \
  --from "2026-02-06 02:00:00" \
  --to "$(date -u +"%Y-%m-%d %H:%M:%S")"

# Step 6: Verify restore
docker compose run --rm postgres \
  psql -U erlmcp -c "SELECT COUNT(*) FROM users;"

# Step 7: Restart application
docker service scale erlmcp-prod_erlmcp=5
```

**Total Time**: 20-30 minutes
**Data Loss**: 1-5 minutes (WAL replay coverage)

#### Point-in-Time Recovery (PITR)

```bash
# Restore to specific timestamp
export RESTORE_TIME="2026-02-06 14:30:00"

# Step 1: Restore last full backup before target time
docker compose run --rm postgres \
  /opt/erlmcp/bin/restore-database.sh \
  --backup s3://erlmcp-backups/daily/2026-02-06.sql.gz

# Step 2: Replay WAL up to target time
docker compose run --rm postgres \
  /opt/erlmcp/bin/replay-wal.sh \
  --from "2026-02-06 02:00:00" \
  --to "${RESTORE_TIME}"

# Step 3: Verify PITR
docker compose run --rm postgres \
  psql -U erlmcp -c "SELECT MAX(created_at) FROM audit_log;"
# Expected: <= ${RESTORE_TIME}
```

### 5.5 Backup Verification and Testing

#### Monthly Backup Drill

```bash
# Execute first Monday of each month

# 1. Select random backup
export TEST_BACKUP=$(docker compose run --rm erlmcp-check \
  aws s3 ls s3://erlmcp-backups/daily/ | shuf -n 1 | awk '{print $4}')

# 2. Restore to test environment
docker compose -f docker-compose.test.yml run --rm postgres \
  /opt/erlmcp/bin/restore-database.sh \
  --backup s3://erlmcp-backups/daily/${TEST_BACKUP}

# 3. Validate data integrity
docker compose -f docker-compose.test.yml run --rm erlmcp-check \
  /opt/erlmcp/bin/validate-db-integrity.sh

# 4. Run smoke tests
docker compose -f docker-compose.test.yml run --rm erlmcp-ct \
  make test-smoke

# 5. Document results
cat > backup-drill-$(date +%Y%m%d).md <<EOF
# Backup Drill Report

**Date**: $(date +%Y-%m-%d)
**Backup**: ${TEST_BACKUP}
**Result**: [PASS/FAIL]
**Restore Time**: [X minutes]
**Data Integrity**: [PASS/FAIL]
**Smoke Tests**: [PASS/FAIL]

## Notes
[Any issues or observations]
EOF
```

---

## Upgrade and Patch Management

### 6.1 Upgrade Strategy

| Type | Frequency | Downtime | Approval Required |
|------|-----------|----------|-------------------|
| Security Patch | As needed | None (rolling) | Expedited |
| Minor Version | Monthly | None (blue-green) | Change board |
| Major Version | Quarterly | Planned (< 5 min) | Executive approval |
| Dependency Update | Weekly | None (rolling) | Automated |

### 6.2 Minor Version Upgrade (Zero Downtime)

**Example**: Upgrade from v3.0.0 to v3.1.0

#### Pre-Upgrade Checklist

- [ ] All quality gates passed for new version
- [ ] Backward compatibility verified
- [ ] Database migrations tested in staging
- [ ] Rollback plan confirmed
- [ ] Change approval obtained
- [ ] Backup verified within last 4 hours
- [ ] Off-hours maintenance window (optional)

#### Upgrade Procedure

**Step 1: Pre-Upgrade Validation**

```bash
# Verify current version
docker service inspect erlmcp-prod_erlmcp --format '{{.Spec.TaskTemplate.ContainerSpec.Image}}'
# Expected: erlmcp:3.0.0

# Run full test suite on new version
docker compose run --rm erlmcp-build make compile VERSION=3.1.0
docker compose run --rm erlmcp-unit make eunit VERSION=3.1.0
docker compose run --rm erlmcp-ct make ct VERSION=3.1.0
docker compose run --rm erlmcp-check make check VERSION=3.1.0

# Verify database migrations
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/validate-migrations.sh --version 3.1.0
```

**Step 2: Deploy Canary**

```bash
# Deploy 1 replica with new version
docker service create \
  --name erlmcp-canary-v31 \
  --replicas 1 \
  --network erlmcp-overlay \
  --label version=3.1.0 \
  erlmcp:3.1.0

# Route 5% traffic to canary
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/configure-canary.sh --traffic 5 --target erlmcp-canary-v31

# Monitor for 15 minutes
watch -n 30 'docker compose --profile monitoring exec prometheus \
  promtool query instant "http://localhost:9090" \
  "rate(erlmcp_errors_total{service=\"erlmcp-canary-v31\"}[5m])"'
```

**Decision Point**: Continue or abort?

```
IF canary_error_rate > baseline * 1.1 THEN
  → ABORT: Remove canary
ELSE
  → CONTINUE: Rolling update
END IF
```

**Step 3: Rolling Update**

```bash
# Start rolling update
docker service update \
  --image erlmcp:3.1.0 \
  --update-parallelism 2 \
  --update-delay 30s \
  --update-failure-action rollback \
  --update-monitor 60s \
  erlmcp-prod_erlmcp

# Monitor rollout
watch -n 5 'docker service ps erlmcp-prod_erlmcp'

# Watch for automatic rollback
docker service inspect erlmcp-prod_erlmcp --format '{{.UpdateStatus.State}}'
# Expected: "completed" (if successful)
# Or: "rollback_completed" (if failed)
```

**Step 4: Run Database Migrations**

```bash
# Execute migrations (if any)
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/run-migrations.sh --version 3.1.0

# Verify migration success
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/validate-schema.sh --version 3.1.0
```

**Step 5: Post-Upgrade Validation**

```bash
# Verify all replicas on new version
docker service ps erlmcp-prod_erlmcp --format '{{.Image}}\t{{.CurrentState}}'
# Expected: erlmcp:3.1.0 Running

# Run full test suite
docker compose run --rm erlmcp-ct make test-full

# Verify metrics
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'erlmcp_version_info{version="3.1.0"}'

# Remove canary
docker service rm erlmcp-canary-v31
```

**Total Time**: 30-45 minutes
**Downtime**: 0 seconds

#### Rollback Procedure

```bash
# Automatic rollback (if update-failure-action=rollback)
# Manual rollback:
docker service update \
  --image erlmcp:3.0.0 \
  --force \
  erlmcp-prod_erlmcp

# Rollback database migrations (if needed)
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/rollback-migrations.sh --to-version 3.0.0
```

### 6.3 Major Version Upgrade (Planned Downtime)

**Example**: Upgrade from v2.x to v3.0.0 (breaking changes)

#### Upgrade Procedure (Maintenance Window)

**Step 1: Schedule Maintenance Window**

```bash
# Notify users 7 days in advance
# Maintenance window: Sunday 02:00-04:00 UTC

# Post maintenance banner
docker service update \
  --env-add MAINTENANCE_MODE=scheduled \
  --env-add MAINTENANCE_START="2026-02-09T02:00:00Z" \
  erlmcp-prod_erlmcp
```

**Step 2: Enter Maintenance Mode**

```bash
# At maintenance window start
docker service update \
  --env-add MAINTENANCE_MODE=active \
  erlmcp-prod_erlmcp

# Graceful shutdown (drain requests)
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/graceful-shutdown.sh --timeout 300

# Stop service
docker service scale erlmcp-prod_erlmcp=0
```

**Step 3: Backup Everything**

```bash
# Full database backup
docker compose run --rm postgres \
  /opt/erlmcp/bin/backup-database.sh --type full

# Backup configuration
docker compose run --rm erlmcp-check \
  tar czf pre-upgrade-backup-$(date +%Y%m%d).tar.gz \
  /opt/erlmcp /var/lib/erlmcp

# Upload to S3
docker compose run --rm erlmcp-check \
  aws s3 cp pre-upgrade-backup-$(date +%Y%m%d).tar.gz \
  s3://erlmcp-backups/major-upgrades/
```

**Step 4: Run Major Migrations**

```bash
# Execute breaking migrations
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/run-major-migrations.sh \
  --from-version 2.9.0 \
  --to-version 3.0.0

# Verify migrations
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/validate-schema.sh --version 3.0.0
```

**Step 5: Deploy New Version**

```bash
# Deploy v3.0.0
docker service update \
  --image erlmcp:3.0.0 \
  erlmcp-prod_erlmcp

# Scale up
docker service scale erlmcp-prod_erlmcp=5

# Wait for healthy
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/wait-for-healthy.sh erlmcp-prod 300
```

**Step 6: Validate and Resume**

```bash
# Run full test suite
docker compose run --rm erlmcp-ct make test-full

# Exit maintenance mode
docker service update \
  --env-rm MAINTENANCE_MODE \
  erlmcp-prod_erlmcp

# Monitor for 1 hour
watch -n 60 'docker compose --profile monitoring exec prometheus \
  promtool query instant "http://localhost:9090" \
  "rate(erlmcp_errors_total[5m])"'
```

**Total Time**: 60-90 minutes
**Downtime**: 5-10 minutes (actual user-facing downtime)

### 6.4 Security Patch (Expedited)

**Scenario**: Critical security vulnerability requires immediate patching

#### Expedited Patch Procedure

```bash
# Step 1: Build patched image
docker compose -f docker-compose.prod.yml build \
  --build-arg VERSION=3.0.1-security-patch

# Step 2: Run security validation
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/security-scan.sh --image erlmcp:3.0.1-security-patch

# Step 3: Deploy immediately (rolling update)
docker service update \
  --image erlmcp:3.0.1-security-patch \
  --update-parallelism 3 \
  --update-delay 10s \
  erlmcp-prod_erlmcp

# Step 4: Verify patch applied
docker service ps erlmcp-prod_erlmcp --format '{{.Image}}'
# Expected: erlmcp:3.0.1-security-patch
```

**Total Time**: 10-15 minutes
**Approval**: Expedited (post-deploy notification)

---

## Performance Tuning Procedures

### 7.1 Performance Baseline

| Metric | Baseline | Warning | Critical |
|--------|----------|---------|----------|
| P50 Latency | < 50ms | 100ms | 200ms |
| P95 Latency | < 200ms | 500ms | 1000ms |
| P99 Latency | < 500ms | 1000ms | 2000ms |
| Throughput | 10,000 RPS | 5,000 RPS | 1,000 RPS |
| Error Rate | < 0.01% | 0.1% | 1% |
| CPU Usage | 40-60% | 70% | 85% |
| Memory Usage | 50-70% | 75% | 90% |

### 7.2 Erlang VM Tuning

#### Scheduler Optimization

```bash
# Optimal scheduler configuration
docker service update \
  --env-add ERL_FLAGS="+S 8:8 +SDio 4:4 +A 128" \
  erlmcp-prod_erlmcp

# Explanation:
# +S 8:8    = 8 schedulers, 8 online
# +SDio 4:4 = 4 dirty IO schedulers
# +A 128    = 128 async threads
```

#### Memory Management

```bash
# Tune garbage collection
docker service update \
  --env-add ERL_FLAGS="+MBacul 0 +MBagf 512 +MBacgs 0" \
  erlmcp-prod_erlmcp

# Explanation:
# +MBacul 0   = Disable acul (all CPU allocation)
# +MBagf 512  = Growth factor 512 KB
# +MBacgs 0   = Single-block carrier size 0 (disable)
```

#### Process Limits

```bash
# Increase process limits
docker service update \
  --env-add ERL_MAX_PORTS=65536 \
  --env-add ERL_MAX_ETS_TABLES=50000 \
  --env-add ERL_FLAGS="+P 1048576" \
  erlmcp-prod_erlmcp

# Explanation:
# +P 1048576 = Max 1M processes
```

### 7.3 Database Performance Tuning

#### Connection Pool Optimization

```bash
# Tune connection pool based on load
docker service update \
  --env-add DB_POOL_SIZE=20 \
  --env-add DB_POOL_MAX=50 \
  --env-add DB_POOL_QUEUE_TARGET=50 \
  --env-add DB_POOL_QUEUE_INTERVAL=1000 \
  erlmcp-prod_erlmcp

# Formula: pool_size = (num_cores * 2) + effective_spindle_count
# For 8 core CPU + SSD: pool_size = (8 * 2) + 1 = 17, rounded to 20
```

#### Query Optimization

```bash
# Enable query analysis
docker compose run --rm postgres \
  psql -U erlmcp -c "CREATE EXTENSION IF NOT EXISTS pg_stat_statements;"

# Identify slow queries
docker compose run --rm postgres \
  psql -U erlmcp -c "SELECT query, calls, mean_exec_time, max_exec_time FROM pg_stat_statements ORDER BY mean_exec_time DESC LIMIT 10;"

# Add missing indexes
docker compose run --rm postgres \
  psql -U erlmcp -c "SELECT schemaname, tablename, attname FROM pg_stats WHERE correlation < 0.1 ORDER BY schemaname, tablename;"
```

#### Database Caching

```bash
# Tune PostgreSQL shared_buffers
docker service update \
  --env-add POSTGRES_SHARED_BUFFERS=2GB \
  --env-add POSTGRES_EFFECTIVE_CACHE_SIZE=6GB \
  --env-add POSTGRES_WORK_MEM=64MB \
  erlmcp-prod_postgres

# Formula: shared_buffers = 25% of total RAM
```

### 7.4 Network Optimization

#### TCP Tuning

```bash
# Optimize TCP settings
docker service update \
  --sysctl net.core.somaxconn=65536 \
  --sysctl net.ipv4.tcp_max_syn_backlog=8192 \
  --sysctl net.ipv4.ip_local_port_range="10000 65535" \
  erlmcp-prod_erlmcp
```

#### Connection Backpressure

```bash
# Configure backpressure
docker service update \
  --env-add ERLMCP_MAX_CONNECTIONS=10000 \
  --env-add ERLMCP_CONNECTION_TIMEOUT=30000 \
  --env-add ERLMCP_BACKPRESSURE_ENABLED=true \
  erlmcp-prod_erlmcp
```

### 7.5 Benchmarking Procedure

#### Run Performance Benchmark

```bash
# Execute benchmark suite
docker compose run --rm erlmcp-bench make benchmark

# Expected output:
# ┌─────────────────────────────────────────────────┐
# │ Benchmark Results                               │
# ├─────────────────────────────────────────────────┤
# │ Throughput: 45,234 RPS                          │
# │ P50 Latency: 42ms                               │
# │ P95 Latency: 156ms                              │
# │ P99 Latency: 387ms                              │
# │ Error Rate: 0.003%                              │
# └─────────────────────────────────────────────────┘

# Compare against baseline
docker compose run --rm erlmcp-bench \
  /opt/erlmcp/bin/compare-benchmark.sh \
  --current bench/results/latest.json \
  --baseline bench/baselines/v3.0.0.json

# Regression check
if [ $? -ne 0 ]; then
  echo "❌ REGRESSION DETECTED"
  exit 1
fi
```

#### Load Testing

```bash
# Run load test (sustained traffic)
docker compose run --rm erlmcp-bench \
  /opt/erlmcp/bin/load-test.sh \
  --duration 3600 \
  --rps 10000 \
  --concurrency 100

# Monitor during load test
docker compose --profile monitoring exec grafana-cli \
  dashboard render \
  --dashboard-uid load-test \
  --output /tmp/load-test-$(date +%Y%m%d).pdf
```

#### Stress Testing

```bash
# Run stress test (find breaking point)
docker compose run --rm erlmcp-bench \
  /opt/erlmcp/bin/stress-test.sh \
  --start-rps 1000 \
  --increment 1000 \
  --max-rps 100000 \
  --failure-threshold 1.0

# Expected output:
# Breaking point: 87,452 RPS (P95 > 1000ms)
```

### 7.6 Performance Troubleshooting

#### Slow Request Investigation

```bash
# Enable request tracing
docker service update \
  --env-add ERLMCP_TRACE_ENABLED=true \
  --env-add ERLMCP_TRACE_SAMPLE_RATE=0.01 \
  erlmcp-prod_erlmcp

# Collect traces
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/collect-traces.sh \
  --duration 300 \
  --output /tmp/traces.json

# Analyze slow traces
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/analyze-traces.sh \
  --input /tmp/traces.json \
  --threshold 1000
```

#### Memory Leak Detection

```bash
# Enable memory profiling
docker service update \
  --env-add ERLMCP_MEMORY_PROFILING=true \
  erlmcp-prod_erlmcp

# Collect memory snapshots
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/memory-snapshot.sh

# Analyze for leaks
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/detect-memory-leak.sh \
  --snapshots /tmp/memory-snapshots/
```

---

## Security Incident Response

### 8.1 Security Incident Classification

| Severity | Definition | Response Time | Escalation |
|----------|-----------|---------------|------------|
| **S0** | Active breach, data exfiltration | < 5 minutes | CISO, Legal, PR |
| **S1** | Vulnerability exploitation attempt | < 15 minutes | Security team, On-call |
| **S2** | Suspicious activity, potential threat | < 1 hour | Security team |
| **S3** | Policy violation, audit finding | < 24 hours | Security team |

### 8.2 Security Incident Response Playbook

#### S0: Active Security Breach

**Step 1: Immediate Containment (5 minutes)**

```bash
# ISOLATE AFFECTED SYSTEMS
# Block all external traffic
docker service update \
  --publish-rm 8080:8080 \
  erlmcp-prod_erlmcp

# Enable read-only mode (prevent data modification)
docker service update \
  --env-add ERLMCP_READ_ONLY_MODE=true \
  --env-add ERLMCP_DB_READONLY=true \
  erlmcp-prod_erlmcp

# Capture forensic snapshot
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/forensic-snapshot.sh \
  --output s3://erlmcp-security-forensics/$(date +%Y%m%d-%H%M%S)

# Rotate all credentials IMMEDIATELY
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/rotate-all-credentials.sh --emergency
```

**Step 2: Assessment (10 minutes)**

```bash
# Check for unauthorized access
docker service logs erlmcp-prod_erlmcp | \
  grep -E "unauthorized|failed_auth|access_denied" > security-incident-logs.txt

# Identify compromised accounts
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/identify-compromised-accounts.sh

# Check for data exfiltration
docker compose --profile monitoring exec prometheus \
  promtool query range 'http://localhost:9090' \
  --start $(date -u -d '6 hours ago' +%s) \
  --end $(date -u +%s) \
  'rate(erlmcp_egress_bytes_total[5m])' \
  > egress-analysis.txt

# Scan for malware/backdoors
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/malware-scan.sh
```

**Step 3: Eradication (30 minutes)**

```bash
# Redeploy from known-good image
export LAST_KNOWN_GOOD="3.0.0-verified-clean"

docker service update \
  --image erlmcp:${LAST_KNOWN_GOOD} \
  --force \
  erlmcp-prod_erlmcp

# Rebuild database from last clean backup
docker compose run --rm postgres \
  /opt/erlmcp/bin/restore-database.sh \
  --backup s3://erlmcp-backups/verified-clean/latest.sql.gz

# Rotate all secrets
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/rotate-all-secrets.sh

# Apply security patches
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/apply-security-patches.sh
```

**Step 4: Recovery (60 minutes)**

```bash
# Verify system integrity
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/verify-system-integrity.sh

# Run security audit
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/security-audit.sh --full

# Gradually restore service
# Start with read-only
docker service update \
  --publish-add 8080:8080 \
  --env-add ERLMCP_READ_ONLY_MODE=true \
  erlmcp-prod_erlmcp

# Monitor for 30 minutes, then enable writes
docker service update \
  --env-rm ERLMCP_READ_ONLY_MODE \
  --env-rm ERLMCP_DB_READONLY \
  erlmcp-prod_erlmcp
```

**Step 5: Post-Incident (48 hours)**

```bash
# Generate comprehensive incident report
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/generate-security-incident-report.sh \
  --incident-id S0-$(date +%Y%m%d) \
  --output security-incident-report.pdf

# Implement preventive measures
# - Update WAF rules
# - Enhance monitoring
# - Add security controls

# Notify affected parties (legal requirement)
# - GDPR: 72 hours
# - CCPA: without unreasonable delay
```

#### S1: Exploitation Attempt

```bash
# Step 1: Block attacker IP
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/block-ip.sh --ip <attacker-ip>

# Step 2: Enable enhanced monitoring
docker service update \
  --env-add ERLMCP_SECURITY_MONITORING=enhanced \
  erlmcp-prod_erlmcp

# Step 3: Apply WAF rules
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/update-waf-rules.sh --block-pattern <pattern>

# Step 4: Document and escalate
echo "S1 INCIDENT: Exploitation attempt from <ip>" >> security-incidents.log
```

### 8.3 Security Monitoring and Detection

#### Enable Security Monitoring

```bash
# Deploy security monitoring
docker compose --profile monitoring up -d

# Configure security alerts
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/configure-security-alerts.sh \
  --slack-webhook <webhook-url> \
  --pagerduty-key <api-key>

# Enable audit logging
docker service update \
  --env-add ERLMCP_AUDIT_LOGGING=true \
  --env-add ERLMCP_AUDIT_LOG_LEVEL=detailed \
  erlmcp-prod_erlmcp
```

#### Security Metrics to Monitor

```bash
# Failed authentication attempts
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'rate(erlmcp_auth_failed_total[5m])'

# Suspicious API calls
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'rate(erlmcp_api_suspicious_total[5m])'

# Rate limit violations
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'rate(erlmcp_rate_limit_exceeded_total[5m])'
```

### 8.4 Security Hardening Checklist

```bash
# Weekly security hardening check

# 1. Update all dependencies
docker compose run --rm erlmcp-build make deps-update

# 2. Run security scan
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/security-scan.sh --full

# 3. Check for CVEs
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/cve-check.sh

# 4. Verify TLS configuration
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/verify-tls.sh

# 5. Audit user permissions
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/audit-permissions.sh

# 6. Review firewall rules
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/review-firewall.sh

# 7. Rotate credentials (monthly)
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/rotate-credentials.sh --monthly
```

---

## Chaos Engineering and Game Days

### 9.1 Chaos Engineering Principles

**Goal**: Proactively discover weaknesses before they cause outages

**Principles**:
1. Build a hypothesis around steady state
2. Vary real-world events
3. Run experiments in production (carefully)
4. Automate experiments
5. Minimize blast radius

### 9.2 Chaos Experiments

#### Experiment 1: Random Pod Termination

**Hypothesis**: System remains available when random pods are terminated

```bash
# Run chaos experiment
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/chaos-pod-killer.sh \
  --count 1 \
  --interval 60 \
  --duration 300

# Monitor metrics during experiment
docker compose --profile monitoring exec grafana-cli \
  dashboard render \
  --dashboard-uid chaos-experiment \
  --output /tmp/chaos-pod-kill-$(date +%Y%m%d).pdf

# Validate steady state maintained
# - Availability >= 99.9%
# - Error rate < 0.1%
# - P95 latency within baseline
```

#### Experiment 2: Network Partition

**Hypothesis**: System handles network partitions gracefully with Raft consensus

```bash
# Introduce network partition
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/chaos-network-partition.sh \
  --nodes "erlmcp-node-1,erlmcp-node-2" \
  --duration 120

# Verify cluster recovers
docker compose run --rm erlmcp-node \
  /opt/erlmcp/bin/erlmcp rpc erlang nodes

# Expected: Cluster rejoins after partition heals
```

#### Experiment 3: Database Latency Injection

**Hypothesis**: Application degrades gracefully under database latency

```bash
# Inject latency to database
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/chaos-inject-latency.sh \
  --target postgres \
  --latency 500ms \
  --duration 300

# Monitor application behavior
# - Circuit breakers activate
# - Timeouts respected
# - Error messages user-friendly
```

#### Experiment 4: Memory Pressure

**Hypothesis**: System handles memory pressure without OOM

```bash
# Inject memory pressure
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/chaos-memory-pressure.sh \
  --target erlmcp-prod_erlmcp \
  --percentage 90 \
  --duration 300

# Verify garbage collection handles pressure
# No OOM kills
```

#### Experiment 5: Dependency Failure

**Hypothesis**: System degrades gracefully when Redis unavailable

```bash
# Stop Redis
docker service scale erlmcp-prod_redis=0

# Monitor application behavior
# - Session fallback to database
# - Cache misses handled
# - No cascading failures

# Restore Redis
docker service scale erlmcp-prod_redis=1
```

### 9.3 Game Day Procedures

**Game Day**: Scheduled operational exercise simulating major incidents

#### Pre-Game Day Checklist (1 week before)

- [ ] Schedule 2-hour window
- [ ] Notify all participants
- [ ] Prepare scenario
- [ ] Set up monitoring dashboards
- [ ] Create runbook
- [ ] Assign roles (incident commander, scribe, operators)

#### Game Day Execution

**Scenario**: Multi-Region Disaster Recovery Drill

**Objective**: Validate RTO < 1 hour for complete region failure

```bash
# T=0: Start game day
echo "GAME DAY START: $(date -u)" | tee gameday-$(date +%Y%m%d).log

# T+0: Simulate primary region failure
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/simulate-region-failure.sh --region us-east-1

# T+5: Incident commander declares DR event
# Operators follow DR runbook (Section 3.3)

# T+10: Activate secondary region
docker stack deploy \
  --compose-file docker-swarm/docker-compose.dr.yml \
  erlmcp-dr

# T+15: Update DNS
docker compose run --rm erlmcp-check \
  /opt/erlmcp/bin/update-dns.sh --region us-west-2

# T+20: Validate recovery
docker compose run --rm erlmcp-ct make test-smoke

# T+30: Measure RTO
export RTO_ACTUAL=$(($(date +%s) - GAME_DAY_START))
echo "RTO ACTUAL: ${RTO_ACTUAL} seconds" | tee -a gameday-$(date +%Y%m%d).log

# T+30-120: Monitor stability
# Run load tests, verify metrics

# T+120: End game day
echo "GAME DAY END: $(date -u)" | tee -a gameday-$(date +%Y%m%d).log
```

#### Post-Game Day Review

```bash
# Generate game day report
cat > gameday-report-$(date +%Y%m%d).md <<EOF
# Game Day Report

**Date**: $(date +%Y-%m-%d)
**Scenario**: Multi-Region DR Drill
**Participants**: [Names]

## Objectives
- [ ] RTO < 1 hour (Actual: ${RTO_ACTUAL}s)
- [ ] RPO < 5 minutes
- [ ] Zero data loss
- [ ] All runbooks functional

## Findings
1. [Finding 1]
2. [Finding 2]

## Action Items
- [ ] Update DNS automation
- [ ] Improve monitoring
- [ ] Train new SREs

## Next Game Day
**Scheduled**: [Date]
**Scenario**: [Next scenario]
EOF
```

### 9.4 Game Day Scenarios

| Scenario | Frequency | Participants | Duration |
|----------|-----------|--------------|----------|
| Pod Termination | Weekly | 1 SRE | 30 min |
| Database Failover | Monthly | 2-3 SREs | 1 hour |
| Region Failure | Quarterly | All SREs | 2 hours |
| Security Breach | Quarterly | SREs + Security | 3 hours |
| Complete Disaster | Annually | All hands | 4 hours |

---

## On-Call Rotation and Escalation

### 10.1 On-Call Schedule

| Role | Shift | Responsibilities |
|------|-------|-----------------|
| Primary On-Call | 24/7 rotation (weekly) | First responder, incident triage |
| Secondary On-Call | 24/7 rotation (weekly) | Backup, escalation point |
| Engineering Lead | Business hours | Complex issues, architectural decisions |
| VP Engineering | Emergency only | P0 incidents, executive decisions |

### 10.2 On-Call Rotation

```bash
# Weekly rotation (Monday 00:00 UTC)

Week 1: Alice (Primary), Bob (Secondary)
Week 2: Bob (Primary), Charlie (Secondary)
Week 3: Charlie (Primary), Alice (Secondary)
Week 4: Alice (Primary), Bob (Secondary)
...
```

### 10.3 Escalation Matrix

```
Level 1: Primary On-Call SRE
         ↓ (if no response in 15 min)
Level 2: Secondary On-Call SRE
         ↓ (if no resolution in 30 min)
Level 3: Engineering Lead
         ↓ (if P0 and no resolution in 1 hour)
Level 4: VP Engineering
         ↓ (if catastrophic)
Level 5: Executive On-Call
```

### 10.4 On-Call Handoff Procedure

```bash
# Every Monday 00:00 UTC

# Outgoing on-call prepares handoff
cat > handoff-$(date +%Y%m%d).md <<EOF
# On-Call Handoff

**From**: Alice
**To**: Bob
**Date**: $(date +%Y-%m-%d)

## Active Incidents
- [ ] None

## Ongoing Issues
- P2-12345: Slow query on users table (monitoring)

## Recent Changes
- Deployed v3.1.0 on 2026-02-05
- Increased replicas from 5 to 7

## Upcoming Maintenance
- Database upgrade scheduled 2026-02-10

## Notes
- Watch for elevated error rates post-deployment
EOF

# Incoming on-call reviews handoff
# Acknowledges receipt
# Asks clarifying questions

# Outgoing on-call confirms transfer complete
```

### 10.5 On-Call Runbook Checklist

**When paged**:

1. Acknowledge alert within 5 minutes
2. Assess severity (P0/P1/P2/P3)
3. Follow appropriate incident runbook
4. Update incident ticket
5. Communicate in Slack #incidents
6. Escalate if needed
7. Document resolution
8. Post-mortem if P0/P1

**Daily tasks**:

- [ ] Check monitoring dashboards (09:00)
- [ ] Review overnight alerts
- [ ] Check backup status
- [ ] Review system health
- [ ] Update incident tickets

**Weekly tasks**:

- [ ] Review runbooks for accuracy
- [ ] Test alerting (send test page)
- [ ] Review metrics and trends
- [ ] Prepare handoff notes

---

## Appendices

### Appendix A: Docker Service Quick Reference

```bash
# View all services
docker service ls

# Inspect service
docker service inspect erlmcp-prod_erlmcp

# View service logs
docker service logs erlmcp-prod_erlmcp --tail 100 --follow

# Scale service
docker service scale erlmcp-prod_erlmcp=10

# Update service
docker service update --image erlmcp:3.1.0 erlmcp-prod_erlmcp

# Remove service
docker service rm erlmcp-prod_erlmcp

# View service tasks
docker service ps erlmcp-prod_erlmcp
```

### Appendix B: Health Check Commands

```bash
# Application health
curl -f http://erlmcp:9090/health

# Cluster health
docker compose run --rm erlmcp-check /opt/erlmcp/bin/cluster-health.sh

# Database health
docker compose run --rm postgres pg_isready -U erlmcp

# Redis health
docker compose run --rm redis redis-cli ping

# Monitoring health
curl -f http://prometheus:9090/-/healthy
curl -f http://grafana:3000/api/health
```

### Appendix C: Metrics Queries

```bash
# Request rate
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'rate(erlmcp_requests_total[5m])'

# Error rate
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'rate(erlmcp_errors_total[5m]) / rate(erlmcp_requests_total[5m])'

# P95 latency
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'histogram_quantile(0.95, rate(erlmcp_request_duration_seconds_bucket[5m]))'

# Memory usage
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'erlmcp_memory_bytes{type="total"}'

# Active connections
docker compose --profile monitoring exec prometheus \
  promtool query instant 'http://localhost:9090' \
  'erlmcp_connections_active'
```

### Appendix D: Emergency Contact List

| Role | Name | Phone | Email | Escalation |
|------|------|-------|-------|------------|
| Primary On-Call | [Name] | [Phone] | pager-sre@company.com | L1 |
| Secondary On-Call | [Name] | [Phone] | pager-sre-backup@company.com | L2 |
| Engineering Lead | [Name] | [Phone] | eng-lead@company.com | L3 |
| VP Engineering | [Name] | [Phone] | vp-eng@company.com | L4 |
| CISO | [Name] | [Phone] | ciso@company.com | Security |
| Legal | [Name] | [Phone] | legal@company.com | Data breach |
| PR/Communications | [Name] | [Phone] | pr@company.com | Public incidents |

### Appendix E: Glossary

| Term | Definition |
|------|------------|
| **RTO** | Recovery Time Objective - Maximum acceptable downtime |
| **RPO** | Recovery Point Objective - Maximum acceptable data loss |
| **SLA** | Service Level Agreement - Commitment to uptime/performance |
| **SLO** | Service Level Objective - Internal target for performance |
| **SLI** | Service Level Indicator - Metric measuring service quality |
| **MTTR** | Mean Time To Recovery - Average time to resolve incidents |
| **MTBF** | Mean Time Between Failures - Average time between incidents |
| **Canary** | Deployment to small subset before full rollout |
| **Blue-Green** | Two identical environments for zero-downtime deployment |
| **Circuit Breaker** | Pattern to prevent cascading failures |

### Appendix F: Runbook Revision History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2026-02-06 | SRE Team | Initial release |

---

## Document Control

**Document Owner**: SRE Team
**Last Updated**: 2026-02-06
**Review Frequency**: Monthly
**Classification**: Internal - Operational
**Distribution**: SRE Team, DevOps, Engineering Leadership

**Approval**:
- [ ] SRE Lead
- [ ] Engineering Lead
- [ ] VP Engineering
- [ ] CISO

---

**END OF OPERATIONAL RUNBOOKS**

*For questions or updates, contact: sre-team@company.com*
