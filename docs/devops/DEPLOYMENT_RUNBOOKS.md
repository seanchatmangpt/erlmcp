# MCP Deployment Runbooks

**Version:** 1.0.0
**Created:** 2026-02-02
**Status:** OPERATIONAL PROCEDURES
**Target:** erlmcp v2.1.0 → v3.0.0 MCP Rollout

---

## Table of Contents

1. [Phase 1 Deployment: Sampling Support](#phase-1-deployment-sampling-support)
2. [Phase 2 Deployment: Tasks + Elicitation](#phase-2-deployment-tasks--elicitation)
3. [Phase 3 Deployment: Optimizations](#phase-3-deployment-optimizations)
4. [Phase 4 Deployment: Advanced Features](#phase-4-deployment-advanced-features)
5. [Emergency Rollback Procedure](#emergency-rollback-procedure)
6. [Database Migration Runbook](#database-migration-runbook)
7. [Canary Deployment Runbook](#canary-deployment-runbook)
8. [Incident Response Playbook](#incident-response-playbook)
9. [Troubleshooting Guide](#troubleshooting-guide)

---

## PHASE 1 DEPLOYMENT: Sampling Support

**Timeline:** Weeks 1-10
**Risk Level:** Medium
**Rollback Time:** <5 minutes

### Pre-Deployment Checklist

**1 Week Before Deployment:**
- [ ] Code freeze for v2.2.0 (sampling)
- [ ] All Phase 1 tests passing (compile, xref, dialyzer, eunit, ct, coverage ≥80%)
- [ ] Compliance tests passing (sampling_compliance_SUITE)
- [ ] Performance benchmarks complete (no regression >10%)
- [ ] Feature flags configured in `config/feature_flags.json`:
  ```json
  {
    "mcp_sampling": {
      "enabled": false,
      "rollout_percentage": 0,
      "environments": ["dev"]
    }
  }
  ```
- [ ] Staging environment updated with v2.2.0
- [ ] Load testing complete in staging (48 hours)
- [ ] Rollback plan documented and tested
- [ ] On-call rotation staffed (SRE + Engineering)

**Day of Deployment:**
- [ ] Production health check (error rate <0.05%, latency P95 <8ms)
- [ ] Verify Mnesia cluster health (all nodes connected)
- [ ] Backup current production state
- [ ] Communication sent to stakeholders

### Deployment Steps

#### Step 1: Deploy to Dev (Week 1, Monday 10:00 AM UTC)

**Duration:** 30 minutes

```bash
# 1. Set environment
export ERLMCP_ENVIRONMENT=dev
export ERLMCP_VERSION=v2.2.0

# 2. Build release
cd /path/to/erlmcp
rebar3 as dev release

# 3. Stop current dev instance
_build/dev/rel/erlmcp/bin/erlmcp stop

# 4. Deploy new release
cp -r _build/dev/rel/erlmcp /opt/erlmcp/releases/v2.2.0
ln -sf /opt/erlmcp/releases/v2.2.0 /opt/erlmcp/current

# 5. Update feature flags
cat > config/feature_flags.json <<EOF
{
  "mcp_sampling": {
    "enabled": true,
    "rollout_percentage": 100,
    "environments": ["dev"]
  }
}
EOF

# 6. Start new release
/opt/erlmcp/current/bin/erlmcp start

# 7. Wait for startup (30 seconds)
sleep 30

# 8. Health check
curl -sf http://localhost:8080/health || echo "ERROR: Health check failed"

# 9. Smoke test sampling endpoint
curl -X POST http://localhost:8080/v1/sampling/create \
  -H "Content-Type: application/json" \
  -d '{"method":"sampling.create","params":{"interval":0.1}}'
```

**Expected Output:**
```json
{
  "result": {
    "enabled": true,
    "interval": 0.1
  }
}
```

**Verification:**
- [ ] Dev instance responding to requests
- [ ] Sampling endpoint returns valid responses
- [ ] No error logs in `/opt/erlmcp/current/log/`
- [ ] Prometheus metrics showing sampling requests

#### Step 2: Enable in Test (Week 2, Monday 10:00 AM UTC)

**Duration:** 1 hour

```bash
# 1. Trigger CI/CD pipeline for test deployment
gh workflow run deploy-test.yml -f version=v2.2.0

# 2. Monitor deployment
gh run watch

# 3. Update feature flags (via config management)
ansible-playbook -i inventory/test deploy_feature_flags.yml \
  --extra-vars "feature=mcp_sampling enabled=true rollout_percentage=100"

# 4. Run integration tests
cd /path/to/erlmcp
rebar3 ct --suite=sampling_integration_SUITE

# 5. Monitor for 1 hour
watch -n 30 'curl -s http://test.erlmcp.example.com/metrics | grep sampling'
```

**Verification:**
- [ ] All integration tests passing
- [ ] No error spikes in logs
- [ ] Metrics showing expected sampling rates
- [ ] Test environment stable for 1 hour

#### Step 3: Deploy to Staging (Week 4, Monday 10:00 AM UTC)

**Duration:** 2 hours

```bash
# 1. Trigger staging deployment
gh workflow run deploy-staging.yml -f version=v2.2.0

# 2. Monitor rollout
kubectl rollout status deployment/erlmcp -n erlmcp-staging --timeout=10m

# 3. Gradual feature flag rollout (10% → 25% → 50% → 100%)
for PCT in 10 25 50 100; do
  echo "Rolling out to ${PCT}%..."

  # Update feature flag
  curl -X PUT https://config-api.example.com/feature_flags/mcp_sampling \
    -H "Authorization: Bearer $CONFIG_TOKEN" \
    -d "{\"rollout_percentage\": $PCT}"

  # Wait 30 minutes and monitor
  sleep 1800

  # Check metrics
  ERROR_RATE=$(curl -s 'http://prometheus:9090/api/v1/query?query=rate(erlmcp_requests_failed_total{deployment="staging"}[5m])' | jq -r '.data.result[0].value[1]')

  if (( $(echo "$ERROR_RATE > 0.001" | bc -l) )); then
    echo "ERROR: Error rate too high at ${PCT}%: $ERROR_RATE"
    exit 1
  fi
done
```

**Verification:**
- [ ] Staging deployment successful
- [ ] Error rate <0.1% during rollout
- [ ] Latency P95 <10ms
- [ ] Load test with 10K requests/s successful
- [ ] Monitoring dashboards showing sampling metrics

#### Step 4: Canary Deployment to Production (Week 6, Monday 10:00 AM UTC)

**Duration:** 4 hours

**See:** [Canary Deployment Runbook](#canary-deployment-runbook)

```bash
# 1. Trigger canary deployment
gh workflow run canary-deployment.yml \
  -f version=v2.2.0 \
  -f canary_percentage=5 \
  -f duration_minutes=60

# 2. Monitor auto-decision engine
gh run watch --interval 60

# 3. If auto-decision = "promote", continue gradual rollout
for PCT in 10 25 50 100; do
  echo "Promoting to ${PCT}%..."

  # Update traffic split
  kubectl patch virtualservice erlmcp-route -n erlmcp --type=json -p="[
    {\"op\": \"replace\", \"path\": \"/spec/http/1/route/0/weight\", \"value\": $((100 - PCT))},
    {\"op\": \"replace\", \"path\": \"/spec/http/1/route/1/weight\", \"value\": $PCT}
  ]"

  # Monitor for 30 minutes
  sleep 1800

  # Verify health
  ./scripts/health_check.sh production https://erlmcp.example.com
done
```

**Verification:**
- [ ] Canary metrics healthy (error rate <0.1%, latency P95 <10ms)
- [ ] Auto-decision engine approved promotion
- [ ] Production traffic successfully migrated to v2.2.0
- [ ] No customer-reported issues
- [ ] Feature flag enabled for 100% of production traffic

#### Step 5: Post-Deployment Validation (Week 10)

**Duration:** 1 week

```bash
# 1. Monitor production metrics
# Check Grafana dashboard: https://grafana.example.com/d/mcp-overview

# 2. Verify sampling compliance
./bin/erlmcp-validate run --compliance --phase=1 --environment=production

# 3. Generate deployment report
./scripts/generate_deployment_report.sh v2.2.0 > reports/phase1_deployment.md

# 4. Capture performance baseline
./scripts/bench/capture_baseline.sh v2.2.0
```

**Success Criteria:**
- [ ] Production stable for 1 week
- [ ] Error rate <0.05%
- [ ] Latency P95 <8ms (improved from baseline)
- [ ] Sampling feature used by >50% of sessions
- [ ] No rollbacks triggered
- [ ] Compliance score: 75% (65% → 75% with sampling)

### Rollback Procedure

**Trigger Conditions:**
- Error rate >0.1% for 5 minutes
- Latency P95 >15ms for 10 minutes
- Pod crash loop detected
- Customer-reported critical bugs

**Rollback Steps:**

```bash
# 1. Immediate traffic cutoff (if canary)
kubectl patch virtualservice erlmcp-route -n erlmcp --type=json -p='[
  {"op": "replace", "path": "/spec/http/1/route/0/weight", "value": 100},
  {"op": "replace", "path": "/spec/http/1/route/1/weight", "value": 0}
]'

# 2. Disable feature flag globally
curl -X PUT https://config-api.example.com/feature_flags/mcp_sampling \
  -H "Authorization: Bearer $CONFIG_TOKEN" \
  -d '{"enabled": false}'

# 3. Rollback Kubernetes deployment
kubectl rollout undo deployment/erlmcp -n erlmcp

# 4. Verify rollback
kubectl rollout status deployment/erlmcp -n erlmcp --timeout=5m
./scripts/health_check.sh production https://erlmcp.example.com

# 5. Capture failure logs
kubectl logs -n erlmcp -l app=erlmcp --tail=10000 > /tmp/phase1_failure_logs.txt

# 6. Notify stakeholders
./scripts/notify_rollback.sh phase1 "Error rate exceeded threshold"
```

**Post-Rollback:**
- [ ] Incident report created
- [ ] Root cause analysis scheduled
- [ ] Fix implemented and tested in dev/staging
- [ ] Re-deployment planned

---

## PHASE 2 DEPLOYMENT: Tasks + Elicitation

**Timeline:** Weeks 11-22
**Risk Level:** High (2 new experimental features)
**Rollback Time:** <5 minutes

### Pre-Deployment Checklist

**2 Weeks Before Deployment:**
- [ ] Phase 1 (sampling) stable in production for ≥4 weeks
- [ ] All Phase 2 tests passing (+180 tests)
- [ ] Tasks API compliance tests passing
- [ ] Elicitation compliance tests passing
- [ ] Performance benchmarks (no regression >10%)
- [ ] Feature flags configured:
  ```json
  {
    "mcp_tasks": {
      "enabled": false,
      "rollout_percentage": 0,
      "environments": ["dev"]
    },
    "mcp_elicitation": {
      "enabled": false,
      "rollout_percentage": 0,
      "environments": ["dev"]
    }
  }
  ```
- [ ] Staging environment updated with v2.3.0
- [ ] Load testing complete (tasks: 1K/s, elicitation: 500/s)
- [ ] Backward compatibility verified (sampling still works)
- [ ] Rollback plan documented

**Day of Deployment:**
- [ ] Production baseline healthy
- [ ] Mnesia cluster healthy
- [ ] Communication sent to early adopters (beta testers)

### Deployment Steps

#### Step 1: Deploy to Dev (Week 11, Monday 10:00 AM UTC)

**Duration:** 1 hour

```bash
# 1. Build release
rebar3 as dev release

# 2. Deploy
./scripts/deploy.sh dev v2.3.0

# 3. Enable tasks feature (gradual)
cat > config/feature_flags.json <<EOF
{
  "mcp_sampling": {"enabled": true, "rollout_percentage": 100, "environments": ["dev", "test", "staging", "production"]},
  "mcp_tasks": {"enabled": true, "rollout_percentage": 100, "environments": ["dev"]},
  "mcp_elicitation": {"enabled": false, "rollout_percentage": 0, "environments": ["dev"]}
}
EOF

# 4. Restart service
/opt/erlmcp/current/bin/erlmcp restart

# 5. Test tasks endpoint
curl -X POST http://localhost:8080/v1/tasks/create \
  -H "Content-Type: application/json" \
  -d '{
    "method": "tasks/create",
    "params": {
      "type": "long_running",
      "metadata": {"job_id": "test-123"}
    }
  }'
```

**Expected Output:**
```json
{
  "result": {
    "id": "task-abc-123",
    "status": "pending",
    "created_at": "2026-02-02T10:15:00Z"
  }
}
```

**Verification:**
- [ ] Tasks endpoint responding
- [ ] Task lifecycle working (create → update → complete)
- [ ] Backward compatibility: sampling still works

#### Step 2: Enable Elicitation in Dev (Week 12, Monday 10:00 AM UTC)

**Duration:** 1 hour

```bash
# 1. Enable elicitation feature
curl -X PUT http://localhost:8080/admin/feature_flags/mcp_elicitation \
  -H "Authorization: Bearer $ADMIN_TOKEN" \
  -d '{"enabled": true, "rollout_percentage": 100}'

# 2. Test elicitation endpoint
curl -X POST http://localhost:8080/v1/prompts/get \
  -H "Content-Type: application/json" \
  -d '{
    "method": "prompts/get",
    "params": {
      "name": "example_prompt",
      "arguments": {"user_input": "dynamic"}
    }
  }'
```

**Expected Output:**
```json
{
  "result": {
    "messages": [
      {
        "role": "user",
        "content": {
          "type": "text",
          "text": "You entered: dynamic"
        }
      }
    ]
  }
}
```

**Verification:**
- [ ] Elicitation endpoint responding
- [ ] Dynamic input prompts working
- [ ] Backward compatibility: tasks + sampling still work

#### Step 3: Deploy to Staging (Week 14, Monday 10:00 AM UTC)

**Duration:** 4 hours (gradual rollout)

```bash
# 1. Deploy v2.3.0 to staging
gh workflow run deploy-staging.yml -f version=v2.3.0

# 2. Gradual feature rollout (tasks first)
for FEATURE in tasks elicitation; do
  echo "Rolling out mcp_$FEATURE..."

  for PCT in 10 25 50 100; do
    echo "  ${PCT}%..."

    # Update feature flag
    ansible-playbook -i inventory/staging deploy_feature_flags.yml \
      --extra-vars "feature=mcp_$FEATURE enabled=true rollout_percentage=$PCT"

    # Wait 30 minutes
    sleep 1800

    # Verify health
    ERROR_RATE=$(curl -s 'http://prometheus:9090/api/v1/query?query=rate(erlmcp_requests_failed_total{feature="'$FEATURE'"}[5m])' | jq -r '.data.result[0].value[1]')

    if (( $(echo "$ERROR_RATE > 0.001" | bc -l) )); then
      echo "ERROR: High error rate for $FEATURE at ${PCT}%"
      exit 1
    fi
  done
done
```

**Verification:**
- [ ] Both features enabled at 100% in staging
- [ ] Load test with 5K req/s successful
- [ ] Task lifecycle tested (1000 concurrent tasks)
- [ ] Elicitation tested (500 concurrent prompts)
- [ ] Error rate <0.1%
- [ ] Latency P95 <10ms

#### Step 4: Canary Deployment to Production (Week 18, Monday 10:00 AM UTC)

**Duration:** 2 days (conservative for 2 features)

**Day 1: Tasks Feature**

```bash
# 1. Deploy canary with tasks enabled
gh workflow run canary-deployment.yml \
  -f version=v2.3.0 \
  -f canary_percentage=5 \
  -f duration_minutes=120 \
  -f features="mcp_tasks:enabled,mcp_elicitation:disabled"

# 2. Monitor for 2 hours
gh run watch --interval 60

# 3. If healthy, promote to 50% over 8 hours
for PCT in 10 25 50; do
  ./scripts/promote_canary.sh $PCT
  sleep 10800  # 3 hours
done

# 4. Hold at 50% overnight for observation
echo "Holding at 50% for 12 hours..."
sleep 43200
```

**Day 2: Elicitation Feature**

```bash
# 1. Enable elicitation in canary (still at 50% traffic)
curl -X PUT https://config-api.example.com/feature_flags/mcp_elicitation \
  -H "Authorization: Bearer $CONFIG_TOKEN" \
  -d '{"enabled": true, "rollout_percentage": 100, "environments": ["production"]}'

# 2. Monitor for 2 hours
sleep 7200

# 3. If healthy, promote to 100%
for PCT in 75 100; do
  ./scripts/promote_canary.sh $PCT
  sleep 7200  # 2 hours
done
```

**Verification:**
- [ ] Tasks feature stable at 50% traffic for 12 hours
- [ ] Elicitation feature stable at 100% traffic for 4 hours
- [ ] Combined error rate <0.05%
- [ ] No customer-reported issues
- [ ] Metrics showing task creation/completion rates
- [ ] Metrics showing elicitation prompt generation

#### Step 5: Post-Deployment Validation (Week 22)

```bash
# 1. Generate compliance report
./bin/erlmcp-validate run --compliance --phase=2 --environment=production

# 2. Verify task metrics
curl -s http://prometheus:9090/api/v1/query?query=erlmcp_tasks_created_total | jq

# 3. Verify elicitation metrics
curl -s http://prometheus:9090/api/v1/query?query=erlmcp_elicitation_prompts_total | jq

# 4. Capture new baseline
./scripts/bench/capture_baseline.sh v2.3.0
```

**Success Criteria:**
- [ ] Production stable for 4 weeks
- [ ] Error rate <0.05%
- [ ] Latency P95 <8ms
- [ ] Task completion rate >95%
- [ ] Compliance score: 85% (75% → 85%)

### Rollback Procedure

**Phase 2 Rollback is Multi-Step (2 features):**

```bash
# Option 1: Disable both features (keep v2.3.0 deployment)
curl -X PUT https://config-api.example.com/feature_flags/mcp_tasks \
  -d '{"enabled": false}'
curl -X PUT https://config-api.example.com/feature_flags/mcp_elicitation \
  -d '{"enabled": false}'

# Option 2: Rollback deployment to v2.2.0 (if critical bug in code)
./scripts/rollback.sh
```

---

## PHASE 3 DEPLOYMENT: Optimizations

**Timeline:** Weeks 23-32
**Risk Level:** Medium (performance-focused)
**Rollback Time:** <5 minutes

### Pre-Deployment Checklist

**2 Weeks Before Deployment:**
- [ ] Phase 2 stable in production for ≥4 weeks
- [ ] All Phase 3 tests passing (+120 tests, including benchmarks)
- [ ] Performance benchmarks showing improvements:
  - [ ] P50 latency: 5ms → 2-3ms
  - [ ] P95 latency: 20ms → 8-12ms
  - [ ] Throughput: 10K req/s → 50K req/s
- [ ] Streaming tests passing (1GB file transfer)
- [ ] Parallel request tests passing (1000 concurrent)
- [ ] Chaos engineering tests passing (circuit breaker, backpressure)
- [ ] Feature flags configured:
  ```json
  {
    "mcp_streaming_optimizations": {
      "enabled": false,
      "rollout_percentage": 0,
      "environments": ["dev"]
    }
  }
  ```
- [ ] Staging load testing complete (100K req/s)
- [ ] Backward compatibility verified

### Deployment Steps

#### Step 1: Deploy to Dev (Week 23, Monday 10:00 AM UTC)

**Duration:** 2 hours (includes benchmarking)

```bash
# 1. Build optimized release
rebar3 as prod release

# 2. Deploy
./scripts/deploy.sh dev v2.4.0

# 3. Enable streaming optimizations
curl -X PUT http://localhost:8080/admin/feature_flags/mcp_streaming_optimizations \
  -d '{"enabled": true, "rollout_percentage": 100}'

# 4. Run performance benchmarks
./scripts/bench/run_all.sh --baseline=v2.3.0 --target=v2.4.0

# Expected output:
# Baseline (v2.3.0): P50=5ms, P95=20ms, Throughput=10K req/s
# Target (v2.4.0): P50=2.5ms, P95=10ms, Throughput=45K req/s
# Improvement: P50↓50%, P95↓50%, Throughput↑350%
```

**Verification:**
- [ ] Benchmarks show ≥30% improvement in all metrics
- [ ] Streaming file transfer working (1GB in <10s)
- [ ] Parallel requests working (1000 concurrent)
- [ ] No regressions in existing features

#### Step 2: Deploy to Staging with Load Testing (Week 25, Monday 10:00 AM UTC)

**Duration:** 1 day (extended load testing)

```bash
# 1. Deploy v2.4.0 to staging
gh workflow run deploy-staging.yml -f version=v2.4.0

# 2. Enable optimizations
ansible-playbook -i inventory/staging deploy_feature_flags.yml \
  --extra-vars "feature=mcp_streaming_optimizations enabled=true rollout_percentage=100"

# 3. Run extended load test (24 hours)
./scripts/load_test.sh \
  --duration=24h \
  --target=https://staging.erlmcp.example.com \
  --rps=50000 \
  --ramp-up=1h \
  --output=/tmp/load_test_results.json

# 4. Monitor during load test
watch -n 60 './scripts/check_performance_regression.sh'
```

**Verification:**
- [ ] Staging handled 50K req/s for 24 hours
- [ ] No performance degradation over time
- [ ] Error rate <0.01%
- [ ] Latency P95 <10ms under load
- [ ] CPU usage <60% (previously 80%)
- [ ] Memory stable (no leaks)

#### Step 3: Canary Deployment to Production (Week 28, Monday 10:00 AM UTC)

**Duration:** 8 hours (performance validation critical)

```bash
# 1. Deploy canary
gh workflow run canary-deployment.yml \
  -f version=v2.4.0 \
  -f canary_percentage=5 \
  -f duration_minutes=120

# 2. Performance validation at each stage
for PCT in 5 10 25 50 100; do
  echo "Canary at ${PCT}%..."

  # Update traffic split
  kubectl patch virtualservice erlmcp-route -n erlmcp --type=json -p="[
    {\"op\": \"replace\", \"path\": \"/spec/http/1/route/1/weight\", \"value\": $PCT}
  ]"

  # Wait 1 hour
  sleep 3600

  # Verify performance improvements
  LATENCY_P95=$(curl -s 'http://prometheus:9090/api/v1/query?query=histogram_quantile(0.95, rate(erlmcp_request_duration_seconds_bucket{deployment="canary"}[5m]))' | jq -r '.data.result[0].value[1]')

  if (( $(echo "$LATENCY_P95 > 0.012" | bc -l) )); then
    echo "WARNING: Latency P95 higher than expected: ${LATENCY_P95}s"
  fi
done
```

**Verification:**
- [ ] Production latency P95 improved to <10ms
- [ ] Throughput increased to >30K req/s
- [ ] CPU usage decreased by ≥20%
- [ ] No customer-reported performance issues

#### Step 4: Post-Deployment Validation (Week 32)

```bash
# 1. Capture performance baseline
./scripts/bench/capture_baseline.sh v2.4.0

# 2. Generate performance comparison report
./scripts/bench/compare_baselines.sh v2.3.0 v2.4.0 > reports/phase3_performance.md

# 3. Verify no regressions
./bin/erlmcp-validate run --performance --environment=production
```

**Success Criteria:**
- [ ] Latency P50: ≤3ms (target: 2-3ms)
- [ ] Latency P95: ≤10ms (target: 8-12ms)
- [ ] Throughput: ≥30K req/s (target: 50K req/s)
- [ ] Compliance score: 90% (85% → 90%)

### Rollback Procedure

**Performance Rollback:**

```bash
# If performance regression detected:

# 1. Disable optimizations feature flag
curl -X PUT https://config-api.example.com/feature_flags/mcp_streaming_optimizations \
  -d '{"enabled": false}'

# 2. Verify performance returns to baseline
sleep 300
./scripts/check_performance_regression.sh

# 3. If feature flag disable insufficient, rollback deployment
./scripts/rollback.sh
```

---

## PHASE 4 DEPLOYMENT: Advanced Features

**Timeline:** Weeks 33-38
**Risk Level:** Low (incremental features)
**Rollback Time:** <5 minutes

### Pre-Deployment Checklist

**2 Weeks Before Deployment:**
- [ ] Phase 3 stable in production for ≥4 weeks
- [ ] All Phase 4 tests passing (+100 tests)
- [ ] Full MCP spec compliance tests passing (95%+)
- [ ] SONA hybrid architecture tested (if applicable)
- [ ] Feature flags configured for advanced features
- [ ] Final compliance validation complete

### Deployment Steps

#### Step 1: Deploy to Dev (Week 33)

```bash
# 1. Build final release
rebar3 as prod release

# 2. Deploy v3.0.0
./scripts/deploy.sh dev v3.0.0

# 3. Run full compliance validation
./bin/erlmcp-validate run --compliance --phase=4 --environment=dev

# Expected output:
# MCP Specification Compliance Report (2025-11-25)
# Overall Compliance: 95%
# - Core Features: 100%
# - Sampling: 100%
# - Tasks: 100%
# - Elicitation: 100%
# - Optimizations: 100%
# - Advanced: 95%
```

#### Step 2: Deploy to Staging (Week 35)

```bash
# 1. Deploy v3.0.0 to staging
gh workflow run deploy-staging.yml -f version=v3.0.0

# 2. Run comprehensive test suite
./scripts/run_comprehensive_tests.sh staging

# 3. Load test with all features enabled
./scripts/load_test.sh --duration=48h --rps=100000 --all-features=true
```

#### Step 3: Production Deployment (Week 37)

```bash
# 1. Canary deployment
gh workflow run canary-deployment.yml \
  -f version=v3.0.0 \
  -f canary_percentage=10 \
  -f duration_minutes=180

# 2. Gradual rollout (faster since incremental changes)
for PCT in 25 50 100; do
  ./scripts/promote_canary.sh $PCT
  sleep 3600  # 1 hour per stage
done
```

#### Step 4: Final Validation (Week 38)

```bash
# 1. Generate final compliance report
./bin/erlmcp-validate run --compliance --full --environment=production \
  --output=reports/mcp_compliance_final.json

# 2. Verify all metrics
./scripts/verify_all_metrics.sh

# 3. Capture final performance baseline
./scripts/bench/capture_baseline.sh v3.0.0

# 4. Create certification evidence
./scripts/generate_certification_evidence.sh > reports/mcp_certification.md
```

**Success Criteria:**
- [ ] Production stable for 1 week
- [ ] Compliance score: 95%+
- [ ] All quality gates passing
- [ ] Performance targets met
- [ ] Zero critical incidents

---

## EMERGENCY ROLLBACK PROCEDURE

### Automatic Rollback Triggers

**The following conditions trigger immediate automatic rollback:**

1. **Error Rate Spike**
   - Trigger: Error rate >0.1% for 5 minutes
   - Action: Immediate traffic cutoff + deployment rollback
   - Notification: PagerDuty + Slack

2. **Pod Crash Loop**
   - Trigger: >3 pod restarts in 5 minutes
   - Action: Immediate deployment rollback
   - Notification: PagerDuty + Slack

3. **Health Check Failure**
   - Trigger: Health endpoint returning 500 for >1 minute
   - Action: Immediate traffic cutoff
   - Notification: PagerDuty

4. **Latency Spike**
   - Trigger: P95 latency >50ms for 10 minutes
   - Action: Manual review + potential rollback
   - Notification: Slack

### Manual Rollback Procedure

**When to Execute:**
- Automatic rollback failed
- Customer-reported critical bugs
- Data corruption detected
- Security vulnerability discovered

**Steps:**

```bash
#!/bin/bash
# Emergency Rollback Script

set -euo pipefail

# 1. Capture current state
CURRENT_VERSION=$(kubectl get deployment erlmcp -n erlmcp -o jsonpath='{.spec.template.spec.containers[0].image}' | cut -d: -f2)
echo "Current version: $CURRENT_VERSION"

# 2. Determine rollback target
ROLLBACK_TARGET="${1:-previous}"  # Default to previous version
echo "Rolling back to: $ROLLBACK_TARGET"

# 3. Disable all feature flags (safety measure)
curl -X POST https://config-api.example.com/feature_flags/disable_all \
  -H "Authorization: Bearer $CONFIG_TOKEN"

# 4. Immediate traffic cutoff (if canary)
kubectl patch virtualservice erlmcp-route -n erlmcp --type=json -p='[
  {"op": "replace", "path": "/spec/http/1/route/0/weight", "value": 100},
  {"op": "replace", "path": "/spec/http/1/route/1/weight", "value": 0}
]'

# 5. Execute Kubernetes rollback
if [ "$ROLLBACK_TARGET" = "previous" ]; then
  kubectl rollout undo deployment/erlmcp -n erlmcp
else
  kubectl set image deployment/erlmcp erlmcp=ghcr.io/org/erlmcp:$ROLLBACK_TARGET -n erlmcp
fi

# 6. Wait for rollout
kubectl rollout status deployment/erlmcp -n erlmcp --timeout=5m

# 7. Verify health
sleep 30
./scripts/health_check.sh production https://erlmcp.example.com

# 8. Create incident report
INCIDENT_ID=$(curl -X POST https://incident-api.example.com/incidents \
  -H "Content-Type: application/json" \
  -d "{
    \"title\": \"Emergency Rollback: $CURRENT_VERSION -> $ROLLBACK_TARGET\",
    \"severity\": \"critical\",
    \"status\": \"investigating\"
  }" | jq -r '.id')

echo "Incident created: $INCIDENT_ID"

# 9. Notify stakeholders
./scripts/notify_stakeholders.sh emergency_rollback \
  --from=$CURRENT_VERSION \
  --to=$ROLLBACK_TARGET \
  --incident=$INCIDENT_ID

echo "✅ Emergency rollback completed"
```

### Post-Rollback Checklist

**Immediate (0-1 hour):**
- [ ] Verify service is stable
- [ ] Check error rates returned to baseline
- [ ] Verify customer traffic is flowing
- [ ] Create incident report
- [ ] Notify stakeholders

**Short-term (1-24 hours):**
- [ ] Collect all logs and metrics from failed deployment
- [ ] Identify root cause
- [ ] Create bug fix or configuration change
- [ ] Test fix in dev/staging
- [ ] Update runbooks with lessons learned

**Long-term (1-7 days):**
- [ ] Conduct post-mortem meeting
- [ ] Document timeline and root cause
- [ ] Implement preventive measures
- [ ] Update monitoring/alerting rules
- [ ] Plan re-deployment

---

## DATABASE MIGRATION RUNBOOK

### ETS → DETS Migration

**Timeline:** 2 weeks
**Risk:** Low (single-node, no replication)

**Steps:**

```bash
# Week 1: Preparation

# 1. Create DETS directory
mkdir -p /var/lib/erlmcp/dets

# 2. Update configuration
cat > config/sys.config.test <<EOF
[{erlmcp_core, [
  {session_backend, erlmcp_session_dets},
  {dets_dir, "/var/lib/erlmcp/dets"}
]}].
EOF

# 3. Deploy updated config (no code changes)
./scripts/deploy_config.sh test

# 4. Monitor for 1 week
watch -n 300 'ls -lh /var/lib/erlmcp/dets/*.dets'

# Week 2: Verification

# 5. Verify DETS file size
DETS_SIZE=$(du -sh /var/lib/erlmcp/dets | cut -f1)
echo "DETS size: $DETS_SIZE"

# 6. Test DETS persistence (restart node)
/opt/erlmcp/current/bin/erlmcp restart
sleep 30

# 7. Verify sessions restored
SESSION_COUNT=$(curl -s http://localhost:8080/admin/sessions/count | jq -r '.count')
echo "Sessions after restart: $SESSION_COUNT"
```

### DETS → Mnesia Migration

**Timeline:** 6 weeks
**Risk:** Medium (distributed, requires cluster)

**Phase 0: Preparation (Week 1-2)**

```bash
# 1. Set up Mnesia cluster (3 nodes for staging, 5 for production)
NODES="erlmcp1@prod erlmcp2@prod erlmcp3@prod erlmcp4@prod erlmcp5@prod"

for node in $NODES; do
  ssh $node 'mkdir -p /var/lib/erlmcp/mnesia'
done

# 2. Create Mnesia schema
./scripts/create_mnesia_schema.sh $NODES

# 3. Create Mnesia tables
./scripts/create_mnesia_tables.sh erlmcp_sessions erlmcp_state
```

**Phase 1: Dual-Write (Week 3-4)**

```bash
# 1. Deploy hybrid backend
cat > config/sys.config.staging <<EOF
[{erlmcp_core, [
  {session_backend, erlmcp_session_backend_hybrid},
  {migration_phase, phase1},
  {primary_backend, erlmcp_session_dets},
  {secondary_backend, erlmcp_session_mnesia}
]}].
EOF

# 2. Deploy configuration
./scripts/deploy_config.sh staging

# 3. Monitor consistency for 2 weeks
./scripts/monitor_consistency.sh --duration=14d --alert-on-mismatch=true
```

**Phase 2: Dual-Read (Week 5-6)**

```bash
# 1. Switch to phase2 (read from Mnesia, write to both)
./scripts/update_migration_phase.sh phase2

# 2. Monitor error rates
watch -n 60 'curl -s http://prometheus:9090/api/v1/query?query=rate(erlmcp_session_read_errors_total[5m])'

# 3. If error rate <0.01%, proceed to phase 3
ERROR_RATE=$(...)
if (( $(echo "$ERROR_RATE < 0.0001" | bc -l) )); then
  echo "Proceeding to phase 3"
  ./scripts/update_migration_phase.sh phase3
fi
```

**Phase 3: Mnesia-Only (Week 7+)**

```bash
# 1. Switch to Mnesia-only
cat > config/sys.config.staging <<EOF
[{erlmcp_core, [
  {session_backend, erlmcp_session_mnesia},
  {mnesia_nodes, ['erlmcp1@staging', 'erlmcp2@staging', 'erlmcp3@staging']}
]}].
EOF

# 2. Archive DETS files
tar -czf /backup/dets_archive_$(date +%Y%m%d).tar.gz /var/lib/erlmcp/dets/

# 3. Verify Mnesia replication
./scripts/verify_mnesia_replication.sh

# Expected output:
# Node: erlmcp1@staging, Table size: 10000
# Node: erlmcp2@staging, Table size: 10000
# Node: erlmcp3@staging, Table size: 10000
# ✅ All nodes consistent
```

---

## CANARY DEPLOYMENT RUNBOOK

**See:** [Phase 1 Deployment](#phase-1-deployment-sampling-support) Step 4 for detailed canary procedure.

**Quick Reference:**

```bash
# 1. Deploy canary
gh workflow run canary-deployment.yml \
  -f version=<VERSION> \
  -f canary_percentage=5 \
  -f duration_minutes=60

# 2. Monitor auto-decision
gh run watch

# 3. Promote if healthy
for PCT in 10 25 50 100; do
  ./scripts/promote_canary.sh $PCT
  sleep 1800
done

# 4. Rollback if unhealthy
./scripts/rollback_canary.sh
```

---

## INCIDENT RESPONSE PLAYBOOK

### Severity Levels

| Severity | Description | Response Time | Escalation |
|----------|-------------|---------------|------------|
| **P0 (Critical)** | Production down, data loss | <5 minutes | Immediate PagerDuty + Exec team |
| **P1 (High)** | Partial outage, high error rate | <15 minutes | PagerDuty + Eng lead |
| **P2 (Medium)** | Degraded performance | <1 hour | Slack + Eng team |
| **P3 (Low)** | Minor issue, no customer impact | <4 hours | Slack |

### P0 Incident Response

**Trigger Examples:**
- Production error rate >1%
- All health checks failing
- Data corruption detected
- Security breach

**Response Steps:**

```bash
# 1. Page on-call SRE (automatic via PagerDuty)

# 2. Acknowledge incident
curl -X POST https://incident-api.example.com/incidents \
  -d '{"severity": "p0", "title": "Production Outage", "responder": "your-name"}'

# 3. Create war room
# Slack channel: #incident-$(date +%Y%m%d)-p0

# 4. Execute immediate mitigation
# Option A: Rollback deployment
./scripts/rollback.sh

# Option B: Disable feature flags
curl -X POST https://config-api.example.com/feature_flags/disable_all

# Option C: Scale up replicas
kubectl scale deployment erlmcp --replicas=20 -n erlmcp

# 5. Verify mitigation worked
./scripts/health_check.sh production

# 6. Communicate status every 15 minutes
./scripts/notify_stakeholders.sh incident_update --channel=#incident-...

# 7. Once stable, begin root cause analysis
# See: docs/incident_response/ROOT_CAUSE_ANALYSIS.md
```

### P1 Incident Response

**Trigger Examples:**
- Error rate >0.1% for 10 minutes
- Latency P95 >50ms for 15 minutes
- Customer-reported critical bugs

**Response Steps:**

```bash
# 1. Acknowledge incident
curl -X POST https://incident-api.example.com/incidents \
  -d '{"severity": "p1", "title": "High Error Rate", "responder": "your-name"}'

# 2. Investigate
# Check logs
kubectl logs -n erlmcp -l app=erlmcp --tail=1000 --since=15m

# Check metrics
open https://grafana.example.com/d/mcp-overview

# 3. Identify root cause
# Common issues:
# - Memory leak → Check memory usage graph
# - Database bottleneck → Check Mnesia metrics
# - External dependency failure → Check network errors

# 4. Mitigate
# If memory leak:
kubectl rollout restart deployment/erlmcp -n erlmcp

# If database bottleneck:
./scripts/scale_mnesia_cluster.sh --add-node

# If external dependency:
./scripts/enable_circuit_breaker.sh <dependency>

# 5. Monitor mitigation for 1 hour
# 6. Create post-mortem ticket
# 7. Update runbooks with lessons learned
```

---

## TROUBLESHOOTING GUIDE

### Common Issues

#### Issue 1: High Error Rate After Deployment

**Symptoms:**
- Error rate >0.1%
- 500 errors in logs
- Customer complaints

**Diagnosis:**

```bash
# 1. Check logs for error patterns
kubectl logs -n erlmcp -l app=erlmcp --tail=1000 | grep ERROR

# 2. Check feature flags
curl -s https://config-api.example.com/feature_flags | jq

# 3. Check Mnesia cluster
./scripts/check_mnesia_health.sh
```

**Resolution:**

```bash
# Option A: Disable problematic feature
curl -X PUT https://config-api.example.com/feature_flags/<FEATURE> \
  -d '{"enabled": false}'

# Option B: Rollback deployment
./scripts/rollback.sh

# Option C: Restart pods
kubectl rollout restart deployment/erlmcp -n erlmcp
```

#### Issue 2: High Latency

**Symptoms:**
- P95 latency >15ms
- Slow API responses
- Timeout errors

**Diagnosis:**

```bash
# 1. Check CPU/memory usage
kubectl top pods -n erlmcp

# 2. Check Mnesia performance
./scripts/check_mnesia_performance.sh

# 3. Check connection pool
curl -s http://prometheus:9090/api/v1/query?query=erlmcp_connection_pool_utilization
```

**Resolution:**

```bash
# Option A: Scale up replicas
kubectl scale deployment erlmcp --replicas=20 -n erlmcp

# Option B: Optimize Mnesia
./scripts/optimize_mnesia.sh

# Option C: Enable caching
curl -X PUT https://config-api.example.com/feature_flags/enable_caching \
  -d '{"enabled": true}'
```

#### Issue 3: Pod Crash Loop

**Symptoms:**
- Pods restarting frequently
- CrashLoopBackOff status
- OOMKilled events

**Diagnosis:**

```bash
# 1. Check pod status
kubectl get pods -n erlmcp

# 2. Check pod events
kubectl describe pod <POD_NAME> -n erlmcp

# 3. Check memory limits
kubectl get deployment erlmcp -n erlmcp -o jsonpath='{.spec.template.spec.containers[0].resources}'
```

**Resolution:**

```bash
# If OOMKilled:
kubectl set resources deployment erlmcp -n erlmcp \
  --limits=memory=2Gi \
  --requests=memory=1Gi

# If configuration error:
kubectl edit configmap erlmcp-config -n erlmcp

# If application bug:
./scripts/rollback.sh
```

#### Issue 4: Mnesia Cluster Split-Brain

**Symptoms:**
- Inconsistent data across nodes
- Mnesia errors in logs
- Failed reads/writes

**Diagnosis:**

```bash
# 1. Check cluster status
for node in erlmcp1@prod erlmcp2@prod erlmcp3@prod; do
  ssh $node 'erl -sname admin -remsh $node -eval "mnesia:info(), init:stop()."'
done

# 2. Check partition status
./scripts/check_mnesia_partitions.sh
```

**Resolution:**

```bash
# 1. Stop all nodes
for node in erlmcp1@prod erlmcp2@prod erlmcp3@prod; do
  ssh $node '/opt/erlmcp/current/bin/erlmcp stop'
done

# 2. Delete Mnesia schema on affected nodes
ssh erlmcp2@prod 'rm -rf /var/lib/erlmcp/mnesia/*'

# 3. Restart all nodes
for node in erlmcp1@prod erlmcp2@prod erlmcp3@prod; do
  ssh $node '/opt/erlmcp/current/bin/erlmcp start'
done

# 4. Verify cluster formed
./scripts/verify_mnesia_cluster.sh
```

---

## APPENDIX

### A. Contact Information

| Role | Primary | Secondary | Escalation |
|------|---------|-----------|------------|
| **SRE On-Call** | PagerDuty | +1-XXX-XXX-XXXX | SRE Manager |
| **Engineering On-Call** | PagerDuty | +1-XXX-XXX-XXXX | Eng Lead |
| **Product Manager** | Slack: @pm | Email: pm@example.com | CPO |
| **Security Team** | security@example.com | +1-XXX-XXX-XXXX | CISO |

### B. Useful Commands

```bash
# Health check
curl -sf https://erlmcp.example.com/health

# Metrics query
curl -s 'http://prometheus:9090/api/v1/query?query=<QUERY>'

# Scale deployment
kubectl scale deployment erlmcp --replicas=<N> -n erlmcp

# Rollback
kubectl rollout undo deployment/erlmcp -n erlmcp

# Feature flag toggle
curl -X PUT https://config-api.example.com/feature_flags/<FLAG> \
  -d '{"enabled": <true|false>}'

# View logs
kubectl logs -n erlmcp -l app=erlmcp --tail=1000 --since=1h

# Shell into pod
kubectl exec -it <POD_NAME> -n erlmcp -- /bin/bash
```

### C. Decision Trees

**Deployment Decision Tree:**

```
Is this a hotfix?
├─ Yes → Skip canary, deploy directly to production (approval required)
└─ No → Standard deployment procedure
    ├─ Is this a new feature?
    │   ├─ Yes → Use feature flags + canary deployment
    │   └─ No → Is this a performance optimization?
    │       ├─ Yes → Extended canary (8 hours) with performance validation
    │       └─ No → Standard canary (4 hours)
```

**Rollback Decision Tree:**

```
Is production impacted?
├─ Yes → Is error rate >1%?
│   ├─ Yes → Immediate automatic rollback (P0)
│   └─ No → Is error rate >0.1%?
│       ├─ Yes → Manual rollback decision (P1)
│       └─ No → Monitor and prepare rollback plan (P2)
└─ No → Can we fix forward?
    ├─ Yes → Deploy hotfix via canary
    └─ No → Schedule rollback during maintenance window
```

---

**Document Status:** OPERATIONAL READY
**Last Updated:** 2026-02-02
**Next Review:** After Phase 1 deployment completion
