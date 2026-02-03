# erlmcp v3 Deployment Runbook

## Overview

This runbook covers the standard deployment procedures for erlmcp v3 across all environments.

## Pre-Deployment Checklist

### Environment-Specific Checks

#### Dev Environment
- [ ] Verify cluster connectivity
- [ ] Check resource quota availability
- [ ] Verify secrets are configured
- [ ] Confirm no ongoing incidents

#### Staging Environment
- [ ] All dev tests passed
- [ ] Performance benchmarks within SLA
- [ ] Security scan completed (0 critical)
- [ ] Stakeholders notified of deployment

#### Production Environment
- [ ] All staging tests passed
- [ ] Canary deployment plan approved
- [ ] Rollback plan verified
- [ ] On-call team notified
- [ ] Maintenance window confirmed
- [ ] Executive sign-off obtained

## Standard Deployment Procedure

### Phase 1: Preparation (T-30 minutes)

```bash
# 1. Set deployment context
export ERLMCP_ENV=production
export DEPLOY_VERSION=$(git describe --tags --abbrev=0)
export KUBECONFIG=~/.kube/production-config

# 2. Verify kubectl access
kubectl cluster-info
kubectl get nodes

# 3. Verify target namespace
kubectl get namespace erlmcp-prod

# 4. Check current deployment
kubectl get statefulset erlmcp -n erlmcp-prod
kubectl get pods -n erlmcp-prod -l app=erlmcp

# 5. Capture current state
kubectl get all -n erlmcp-prod -o yaml > /tmp/pre-deploy-state.yaml

# 6. Verify GitOps repository
cd /path/to/gitops-repo
git pull origin main
git log -1 --oneline
```

### Phase 2: Build and Push (T-15 minutes)

```bash
# 1. Build Docker image
docker build -f docker/Dockerfile.production \
  -t ghcr.io/seanchatmangpt/erlmcp:${DEPLOY_VERSION} \
  --build-arg VERSION=${DEPLOY_VERSION} \
  .

# 2. Security scan
trivy image --severity CRITICAL,HIGH \
  ghcr.io/seanchatmangpt/erlmcp:${DEPLOY_VERSION}

# 3. Push image
docker push ghcr.io/seanchatmangpt/erlmcp:${DEPLOY_VERSION}

# 4. Generate SBOM
syft ghcr.io/seanchatmangpt/erlmcp:${DEPLOY_VERSION} \
  -o spdx-json > sbom-${DEPLOY_VERSION}.json
```

### Phase 3: Update GitOps Manifests (T-5 minutes)

```bash
# 1. Update image tag in Kustomize
cd gitops/overlays/production
kustomize edit set image \
  ghcr.io/seanchatmangpt/erlmcp:${DEPLOY_VERSION}

# 2. Verify manifests
kustomize build . > /tmp/manifests.yaml
kubectl apply --dry-run=client -f /tmp/manifests.yaml

# 3. Commit and push
git add kustomization.yaml
git commit -m "chore: deploy erlmcp ${DEPLOY_VERSION}"
git push origin main
```

### Phase 4: ArgoCD Sync (T+0 minutes)

```bash
# Option A: Wait for ArgoCD auto-sync (recommended)
# ArgoCD will detect the change and sync automatically

# Option B: Manual sync
argocd app sync erlmcp-production --timeout 600

# Monitor sync progress
argocd app get erlmcp-production
watch kubectl get pods -n erlmcp-prod
```

### Phase 5: Verification (T+5 minutes)

```bash
# 1. Wait for rollout completion
kubectl rollout status statefulset erlmcp -n erlmcp-prod --timeout=10m

# 2. Verify all pods ready
kubectl get pods -n erlmcp-prod -l app=erlmcp

# 3. Check pod logs
kubectl logs -n erlmcp-prod -l app=erlmcp --tail=100

# 4. Run health checks
./scripts/health-check.sh production

# 5. Run smoke tests
./scripts/smoke-tests.sh production
```

### Phase 6: Canary Deployment (Production Only)

```bash
# 1. Create canary deployment
kubectl apply -f gitops/overlays/production/canary/

# 2. Monitor canary metrics
kubectl get canary erlmcp -n erlmcp-prod -w

# 3. Check canary logs
kubectl logs -n erlmcp-prod -l app=erlmcp,canary=true --tail=100

# 4. Verify canary health
curl -f https://canary.erlmcp.io/health || echo "Canary health check failed"

# 5. Run canary load tests
hey -z 5m -q 50 -c 10 https://canary.erlmcp.io/api/health
```

### Phase 7: Full Rollout (After Canary Passes)

```bash
# 1. Update to full rollout
kubectl patch canary erlmcp -n erlmcp-prod \
  --type='json' \
  -p='[{"op": "replace", "path": "/spec/analysis/maxWeight", "value": 100}]'

# 2. Monitor rollout
watch kubectl get pods -n erlmcp-prod

# 3. Verify final state
kubectl get statefulset erlmcp -n erlmcp-prod
kubectl get pods -n erlmcp-prod -l app=erlmcp
```

## Rollback Procedure

### Immediate Rollback

```bash
# 1. Undo last deployment
kubectl rollout undo statefulset erlmcp -n erlmcp-prod

# 2. Wait for rollback
kubectl rollout status statefulset erlmcp -n erlmcp-prod --timeout=5m

# 3. Verify rollback
kubectl get pods -n erlmcp-prod -l app=erlmcp
./scripts/health-check.sh production

# 4. Update GitOps to previous version
cd gitops/overlays/production
kustomize edit set image ghcr.io/seanchatmangpt/erlmcp:<previous-version>
git add kustomization.yaml
git commit -m "rollback: revert to <previous-version>"
git push origin main
```

### Rollback to Specific Revision

```bash
# 1. List available revisions
kubectl rollout history statefulset erlmcp -n erlmcp-prod

# 2. Rollback to specific revision
kubectl rollout undo statefulset erlmcp -n erlmcp-prod --to-revision=<revision>

# 3. Verify rollback
kubectl rollout status statefulset erlmcp -n erlmcp-prod
```

### Emergency Rollback (If kubectl unavailable)

```bash
# 1. Delete all pods (force recreation)
kubectl delete pods -n erlmcp-prod -l app=erlmcp --all --grace-period=0 --force

# 2. Scale to zero then back up
kubectl scale statefulset erlmcp -n erlmcp-prod --replicas=0
kubectl scale statefulset erlmcp -n erlmcp-prod --replicas=3
```

## Blue-Green Deployment

### Setup

```bash
# 1. Create green namespace
kubectl create namespace erlmcp-green

# 2. Deploy to green
kustomize build gitops/overlays/production | \
  sed 's/erlmcp-prod/erlmcp-green/g' | \
  kubectl apply -f -

# 3. Verify green deployment
kubectl get pods -n erlmcp-green -l app=erlmcp
./scripts/health-check.sh green
```

### Cutover

```bash
# 1. Update service selector to green
kubectl patch svc erlmcp -n erlmcp-prod \
  --type='json' \
  -p='[{"op": "replace", "path": "/spec/selector/environment", "value": "green"}]'

# 2. Monitor for errors
watch kubectl get pods -n erlmcp-prod -n erlmcp-green

# 3. If issues, revert selector
kubectl patch svc erlmcp -n erlmcp-prod \
  --type='json' \
  -p='[{"op": "replace", "path": "/spec/selector/environment", "value": "prod"}]'
```

## Health Check Commands

```bash
# Basic health endpoint
curl -f https://erlmcp.io/health

# Readiness check
curl -f https://erlmcp.io/ready

# Metrics endpoint
curl -f https://erlmcp.io/metrics | grep erlmcp_

# Cluster connectivity
kubectl exec -n erlmcp-prod <pod-name> -- \
  erl -n erlmcp@127.0.0.1 -setcookie test -remsh erlmcp@<target-pod> \
  -eval "net_adm:ping('erlmcp@<target-pod>'), init:stop()."

# Database connectivity
kubectl exec -n erlmcp-prod <pod-name> -- \
  nc -zv database.default.svc 5432
```

## Troubleshooting Deployments

### Pods Not Starting

```bash
# Check pod status
kubectl describe pod -n erlmcp-prod <pod-name>

# Check pod logs
kubectl logs -n erlmcp-prod <pod-name>

# Check events
kubectl get events -n erlmcp-prod --sort-by='.lastTimestamp'

# Check resource usage
kubectl top pod -n erlmcp-prod <pod-name>
```

### Image Pull Errors

```bash
# Check image pull secret
kubectl get secret ghcr-pull-secret -n erlmcp-prod -o yaml

# Verify registry access
docker login ghcr.io
docker pull ghcr.io/seanchatmangpt/erlmcp:${DEPLOY_VERSION}

# Update image pull secret if needed
kubectl delete secret ghcr-pull-secret -n erlmcp-prod
kubectl create secret docker-registry ghcr-pull-secret \
  --docker-server=ghcr.io \
  --docker-username=<username> \
  --docker-password=<token> \
  -n erlmcp-prod

# Patch service account
kubectl patch serviceaccount erlmcp -n erlmcp-prod \
  --type='json' \
  -p='[{"op": "add", "path": "/imagePullSecrets", "value": [{"name": "ghcr-pull-secret"}]}]'
```

### CrashLoopBackOff

```bash
# Check previous logs
kubectl logs -n erlmcp-prod <pod-name> --previous

# Check resource limits
kubectl get pod <pod-name> -n erlmcp-prod -o jsonpath='{.spec.containers[*].resources}'

# Check for config issues
kubectl get configmap erlmcp-config -n erlmcp-prod -o yaml
kubectl get secret erlmcp-secrets -n erlmcp-prod -o yaml
```

## Post-Deployment Tasks

### Verify Deployment

```bash
# Run full test suite
./scripts/integration-tests.sh production

# Run load tests
./scripts/load-tests.sh production

# Verify metrics
curl -s https://erlmcp.io/metrics | \
  grep erlmcp_requests_total | \
  awk '{print $NF}' > /tmp/metrics-baseline.txt
```

### Update Documentation

```bash
# Update deployment history
echo "## $(date -u +%Y-%m-%dT%H:%M:%SZ)" >> docs/runbooks/deployment-history.md
echo "- Version: ${DEPLOY_VERSION}" >> docs/runbooks/deployment-history.md
echo "- Deployed by: $(git config user.name)" >> docs/runbooks/deployment-history.md
echo "- Commit: $(git rev-parse HEAD)" >> docs/runbooks/deployment-history.md
```

### Notify Teams

```bash
# Send Slack notification
cat << EOF | slack-cli send "#deployments"
Production deployment completed successfully
Version: ${DEPLOY_VERSION}
Deployed by: $(git config user.name)
Duration: ${DEPLOY_DURATION}
EOF
```

## Deployment Metrics

Track these metrics for each deployment:

| Metric | Target | Actual |
|--------|--------|--------|
| Deployment Time | < 30 min | - |
| Rollback Time | < 5 min | - |
| Error Rate | < 0.1% | - |
| Downtime | 0 min | - |
| Canary Success Rate | > 99% | - |

## References

- CI/CD: `.github/workflows/gitops-deploy.yml`
- ArgoCD: `argocd/`
- Quality Gates: `docs/quality-gates/QUALITY_GATES.md`
- Incident Response: `docs/runbooks/INCIDENT_RESPONSE.md`
