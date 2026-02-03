# Blue-Green and Canary Deployment for erlmcp

This directory contains Kubernetes manifests and automation scripts for implementing blue-green and canary deployment strategies in the staging environment.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         Blue-Green Deployment Architecture                 │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   ┌────────────────┐                 ┌────────────────┐                    │
│   │  erlmcp-blue   │                 │ erlmcp-green   │                    │
│   │  Deployment    │                 │  Deployment    │                    │
│   │                │                 │                │                    │
│   │  ┌──────────┐  │                 │  ┌──────────┐  │                    │
│   │  │   Pods   │  │                 │  │   Pods   │  │                    │
│   │  └──────────┘  │                 │  └──────────┘  │                    │
│   └───────┬────────┘                 └────────┬───────┘                    │
│           │                                   │                            │
│           └───────────┬────────────────────────┘                            │
│                       │                                                     │
│               ┌───────▼───────────┐                                        │
│               │  erlmcp-active    │  ← Traffic routes here                  │
│               │     Service       │                                        │
│               └─────────┬─────────┘                                        │
│                         │                                                  │
│               ┌─────────▼─────────┐                                        │
│               │    Ingress        │                                        │
│               │  (staging.erlmcp  │                                        │
│               │   .com)           │                                        │
│               └───────────────────┘                                        │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────────────┐
│                         Canary Deployment Architecture                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│   ┌──────────────┐                  ┌──────────────┐                      │
│   │    Stable    │                  │    Canary    │                      │
│   │  (Active)    │                  │   (New)      │                      │
│   └──────┬───────┘                  └──────┬───────┘                      │
│          │                                  │                              │
│          │      ┌──────────────────────────┘                              │
│          │      │                                                          │
│          │  ┌───▼────────────────┐                                        │
│          └──►   Traffic Split     │  ← Gradual shift 5% → 100%           │
│             │   (Nginx/ArgoCD)    │                                        │
│             └─────────────────────┘                                        │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Files Overview

### Kubernetes Manifests

| File | Description |
|------|-------------|
| `staging-blue.yaml` | Blue deployment with HPA and PDB |
| `staging-green.yaml` | Green deployment with HPA and PDB |
| `staging-blue-updated.yaml` | Updated blue deployment with enhanced annotations |
| `blue-green-service.yaml` | Active, preview, and headless services |
| `blue-green-ingress.yaml` | Ingress with traffic routing configuration |
| `rollout-bluegreen.yaml` | Argo Rollouts resource for blue-green strategy |
| `rollout-canary.yaml` | Argo Rollouts resource for canary strategy |
| `rollout-analysis-template.yaml` | Analysis templates for automated validation |
| `canary.yaml` | Legacy canary deployment (use rollout-canary.yaml instead) |

### Automation Scripts

| Script | Description |
|--------|-------------|
| `scripts/rolling-update/blue-green-deploy.sh` | Automated blue-green deployment with rollback |
| `scripts/rolling-update/canary-deploy.sh` | Progressive canary deployment with traffic shifting |
| `scripts/rolling-update/traffic-switch.sh` | Manual traffic control between environments |
| `scripts/rolling-update/automated-rollback.sh` | Continuous monitoring with auto-rollback |

## Usage

### Blue-Green Deployment

#### Manual Deployment

```bash
# Deploy to staging (blue-green)
./scripts/rolling-update/blue-green-deploy.sh staging ghcr.io/org/erlmcp:v1.0.0

# Deploy to production with blue-green
./scripts/rolling-update/blue-green-deploy.sh production ghcr.io/org/erlmcp:v1.0.0

# Dry run (no changes)
./scripts/rolling-update/blue-green-deploy.sh staging ghcr.io/org/erlmcp:v1.0.0 --dry-run
```

#### Using Argo Rollouts

```bash
# Apply the rollout resource
kubectl apply -f k8s/staging/rollout-bluegreen.yaml

# Trigger a new deployment
kubectl set image rollout/erlmcp-bluegreen erlmcp=ghcr.io/org/erlmcp:v1.0.0 -n erlmcp-staging

# Watch the rollout progress
kubectl argo rollouts get rollout erlmcp-bluegreen -n erlmcp-staging --watch

# Promote after analysis passes
kubectl argo rollouts promote erlmcp-bluegreen -n erlmcp-staging
```

### Canary Deployment

#### Manual Deployment

```bash
# Deploy with default traffic steps (5, 10, 25, 50, 100%)
./scripts/rolling-update/canary-deploy.sh staging ghcr.io/org/erlmcp:v1.0.0

# Deploy with custom steps
./scripts/rolling-update/canary-deploy.sh staging ghcr.io/org/erlmcp:v1.0.0 --steps "10,25,50"

# Deploy with max traffic limit
./scripts/rolling-update/canary-deploy.sh production ghcr.io/org/erlmcp:v1.0.0 --max-traffic 50
```

#### Using Argo Rollouts

```bash
# Apply the rollout resource
kubectl apply -f k8s/staging/rollout-canary.yaml

# Update the image
kubectl set image rollout/erlmcp-canary-rollout erlmcp=ghcr.io/org/erlmcp:v1.0.0 -n erlmcp-staging

# Monitor the canary progression
kubectl argo rollouts get rollout erlmcp-canary-rollout -n erlmcp-staging --watch
```

### Traffic Control

```bash
# Switch traffic to green
./scripts/rolling-update/traffic-switch.sh staging green

# Switch traffic to blue (rollback)
./scripts/rolling-update/traffic-switch.sh production blue

# Set canary traffic weight
./scripts/rolling-update/traffic-switch.sh staging --canary-weight 25

# Show current traffic status
./scripts/rolling-update/traffic-switch.sh staging --status
```

### Automated Rollback Monitoring

```bash
# Start continuous monitoring (foreground)
./scripts/rolling-update/automated-rollback.sh staging

# Start monitoring in background
./scripts/rolling-update/automated-rollback.sh production &

# Monitor-only mode (no rollback)
./scripts/rolling-update/automated-rollback.sh staging --monitor-only

# Force immediate rollback
./scripts/rolling-update/automated-rollback.sh production --force-rollback

# Check status
./scripts/rolling-update/automated-rollback.sh staging --status
```

## Deployment Workflow

### Blue-Green Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        Blue-Green Deployment Flow                          │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  1. DETERMINE ACTIVE COLOR                                                 │
│     └─ Check which color (blue/green) is currently active                  │
│                                                                             │
│  2. DEPLOY TO INACTIVE COLOR                                               │
│     ├─ Update deployment with new image                                     │
│     └─ Wait for rollout to complete                                        │
│                                                                             │
│  3. HEALTH CHECK NEW DEPLOYMENT                                            │
│     ├─ Check pod readiness                                                  │
│     ├─ Test /health endpoint                                                │
│     └─ Run integration tests                                               │
│                                                                             │
│  4. RUN ANALYSIS (Optional)                                                │
│     ├─ Query Prometheus for success rate                                    │
│     ├─ Check latency (p95, p99)                                             │
│     └─ Verify resource usage                                               │
│                                                                             │
│  5. SWITCH TRAFFIC                                                          │
│     ├─ Update erlmcp-active service selector                                │
│     └─ Wait for DNS/ingress propagation                                    │
│                                                                             │
│  6. VERIFY AFTER SWITCH                                                    │
│     ├─ Health check active service                                          │
│     ├─ Run smoke tests                                                      │
│     └─ Monitor metrics for 5 minutes                                       │
│                                                                             │
│  7. CLEANUP (Optional)                                                     │
│     └─ Scale down old deployment after safe period                         │
│                                                                             │
│  ↺ ROLLBACK ON FAILURE                                                     │
│     └─ Switch traffic back to previous color                               │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Canary Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         Canary Deployment Flow                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  1. DEPLOY CANARY INSTANCE                                                 │
│     ├─ Create canary deployment with new image                              │
│     └─ Set initial traffic weight (e.g., 5%)                               │
│                                                                             │
│  2. ANALYZE AT EACH STEP                                                    │
│     ├─ Step 1: 5% traffic → Analyze → Pause                                │
│     ├─ Step 2: 10% traffic → Analyze → Pause                               │
│     ├─ Step 3: 25% traffic → Analyze → Pause                               │
│     ├─ Step 4: 50% traffic → Analyze → Pause                               │
│     └─ Step 5: 100% traffic → Promote                                      │
│                                                                             │
│  3. ANALYSIS METRICS                                                       │
│     ├─ Success rate (target: ≥95%)                                         │
│     ├─ P95 latency (target: <500ms)                                        │
│     ├─ P99 latency (target: <1000ms)                                       │
│     ├─ Error rate (target: <1%)                                            │
│     └─ Resource usage (CPU/Memory <80%)                                    │
│                                                                             │
│  4. PROMOTE OR ROLLBACK                                                    │
│     ├─ Success: Update stable deployment, scale down canary                 │
│     └─ Failure: Set canary weight to 0%, scale down canary                 │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

## GitHub Actions Workflow

The `.github/workflows/rolling-update-deploy.yml` workflow provides automated deployment:

```yaml
# Trigger on push to main or manual dispatch
on:
  push:
    branches: [main, release/*]
  workflow_dispatch:
    inputs:
      environment: staging | production
      strategy: blue-green | canary | rolling
      image: docker-image-tag
      dry_run: true | false
```

### Workflow Jobs

1. **validate** - Validate deployment configuration
2. **build** - Build and push Docker image
3. **deploy-blue-green** - Execute blue-green deployment
4. **deploy-canary** - Execute canary deployment
5. **traffic-switch** - Switch traffic between colors
6. **summary** - Generate deployment summary

## Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `NAMESPACE_PREFIX` | Kubernetes namespace prefix | `erlmcp` |
| `ACTIVE_SERVICE` | Active service name | `erlmcp-active` |
| `PROMETHEUS_URL` | Prometheus server URL | `http://prometheus-operated.monitoring.svc.cluster.local:9090` |
| `ERROR_RATE_THRESHOLD` | Max error rate for rollback | `0.05` (5%) |
| `P95_LATENCY_THRESHOLD` | Max P95 latency | `1000`ms |
| `P99_LATENCY_THRESHOLD` | Max P99 latency | `2000`ms |
| `CONSECUTIVE_FAILURES_THRESHOLD` | Failures before rollback | `3` |

### Analysis Thresholds

Configure in `rollout-analysis-template.yaml`:

```yaml
spec:
  metrics:
  - name: success-rate
    successCondition: result[0] >= 0.95  # 95% success rate
  - name: p95-latency
    successCondition: result[0] < 500     # 500ms max
  - name: error-rate
    successCondition: result[0] <= 0.01   # 1% max error rate
```

## Troubleshooting

### Check Deployment Status

```bash
# View all deployments
kubectl get deployments -n erlmcp-staging

# View rollout status
kubectl argo rollouts get rollout erlmcp-bluegreen -n erlmcp-staging

# View service endpoints
kubectl get endpoints -n erlmcp-staging

# Check pods
kubectl get pods -n erlmcp-staging -l app=erlmcp
```

### Debug Traffic Issues

```bash
# Check active service selector
kubectl get service erlmcp-active -n erlmcp-staging -o yaml

# Check ingress configuration
kubectl get ingress -n erlmcp-staging -o yaml

# Test endpoint directly
kubectl port-forward -n erlmcp-staging svc/erlmcp-active 8080:8080
curl http://localhost:8080/health
```

### Manual Rollback

```bash
# Rollback blue-green
kubectl patch service erlmcp-active -n erlmcp-staging \
  --type='json' -p='[{"op": "replace", "path": "/spec/selector/component", "value": "erlmcp-blue"}]'

# Rollback rollout
kubectl argo rollouts undo erlmcp-bluegreen -n erlmcp-staging

# Scale down canary
kubectl scale deployment erlmcp-canary -n erlmcp-staging --replicas=0
```

## Monitoring

Deploy the rollback monitor to track deployment health:

```bash
# Start monitoring (run in background or as a deployment)
./scripts/rolling-update/automated-rollback.sh staging &

# Or create a Kubernetes deployment for the monitor
kubectl apply -f k8s/staging/rollback-monitor.yaml
```

The monitor will:
- Query Prometheus for error rates and latency
- Check health endpoints
- Trigger automatic rollback on threshold breach
- Send notifications to Slack/Teams

## Best Practices

1. **Always run in staging first** - Validate deployments in staging before production
2. **Use canary for risky changes** - For significant changes, start with canary
3. **Monitor during rollout** - Keep the rollback monitor running
4. **Set appropriate thresholds** - Adjust based on your baseline metrics
5. **Test rollback procedures** - Regularly test the rollback process
6. **Keep previous versions** - Don't scale down immediately after switch
7. **Use feature flags** - Combine deployment with feature flags for safer rollouts
