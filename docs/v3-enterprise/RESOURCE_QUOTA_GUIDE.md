# ERLMCP v3 Enterprise - ResourceQuota Implementation Guide

## Overview

This guide covers the production-ready ResourceQuota implementation for ERLMCP v3 Enterprise. Resource quotas prevent any single tenant from consuming all cluster resources and ensure fair allocation across multi-tenant deployments.

## Table of Contents

1. [Architecture](#architecture)
2. [Resource Types](#resource-types)
3. [Deployment](#deployment)
4. [Monitoring](#monitoring)
5. [Troubleshooting](#troubleshooting)
6. [Best Practices](#best-practices)

## Architecture

### Quota Hierarchy

```
Cluster Namespace
    |
    +-- Compute Quota (CPU/Memory requests and limits)
    +-- Storage Quota (PVC count and total storage)
    +-- Object Quota (pods, services, secrets, configmaps)
    +-- Long-Running Quota (high/medium priority pods)
    +-- Batch Quota (low priority pods)
    +-- Best Effort Quota (pods without resource requests)
    +-- Guaranteed Quota (pods with resource requests)
```

### Multi-Tenant Isolation

```
Production Namespace (platinum tier)
    |
    +-- 16 CPU requests / 32 CPU limits
    +-- 32Gi memory requests / 64Gi memory limits
    +-- 50 pods max
    +-- 10 PVCs / 200Gi storage

Staging Namespace (gold tier)
    |
    +-- 8 CPU requests / 16 CPU limits
    +-- 16Gi memory requests / 32Gi memory limits
    +-- 30 pods max
    +-- 5 PVCs / 100Gi storage
```

## Resource Types

### Compute Quota

Controls CPU and memory allocation across all workloads.

| Resource | Default | Production | Staging | Dev |
|----------|---------|------------|---------|-----|
| requests.cpu | 32 | 64 | 16 | 4 |
| requests.memory | 64Gi | 128Gi | 32Gi | 8Gi |
| limits.cpu | 64 | 128 | 32 | 8 |
| limits.memory | 128Gi | 256Gi | 64Gi | 16Gi |

### Storage Quota

Controls persistent volume consumption.

| Resource | Default | Production | Staging | Dev |
|----------|---------|------------|---------|-----|
| persistentvolumeclaims | 20 | 30 | 10 | 5 |
| requests.storage | 500Gi | 1Ti | 200Gi | 50Gi |

### Object Count Quota

Prevents object explosion and controls etcd load.

| Resource | Default | Production | Staging | Dev |
|----------|---------|------------|---------|-----|
| pods | 100 | 200 | 50 | 20 |
| services | 30 | 50 | 20 | 10 |
| services.loadbalancers | 2 | 3 | 1 | 0 |
| secrets | 30 | 50 | 20 | 10 |
| configmaps | 20 | 30 | 15 | 10 |

### Priority Class Scoping

| Priority Class | Purpose | Quota |
|----------------|---------|-------|
| high-priority | Production services | 24 CPU / 48Gi memory / 50 pods |
| medium-priority | Standard workloads | Included in high-priority |
| low-priority | Batch jobs | 4 CPU / 8Gi memory / 20 pods |

## Deployment

### Kubernetes Manifests

```bash
# Apply resource quotas
kubectl apply -f k8s/resource-quota.yaml

# Verify quotas are applied
kubectl describe resourcequota -n erlmcp

# Check current usage
kubectl get resourcequota -n erlmcp -o json | jq '.items[] | {name: .metadata.name, used: .status.used, hard: .status.hard}'
```

### Helm Deployment

```bash
# Default deployment
helm install erlmcp ./helm/erlmcp-enterprise \
  --namespace erlmcp \
  --create-namespace

# Production with custom quotas
helm install erlmcp-prod ./helm/erlmcp-enterprise \
  --namespace erlmcp-production \
  --set resourceQuota.types.compute.hard.requests.cpu=128 \
  --set resourceQuota.types.compute.hard.requests.memory=256Gi \
  -f helm/erlmcp-enterprise/values-production.yaml

# Staging deployment
helm install erlmcp-staging ./helm/erlmcp-enterprise \
  --namespace erlmcp-staging \
  -f helm/erlmcp-enterprise/values-staging.yaml

# Development deployment
helm install erlmcp-dev ./helm/erlmcp-enterprise \
  --namespace erlmcp-dev \
  -f helm/erlmcp-enterprise/values-dev.yaml
```

### LimitRange Configuration

LimitRanges enforce container-level constraints:

```bash
# View limit ranges
kubectl get limitrange -n erlmcp -o yaml

# Default values applied to containers without explicit limits
# Default: 1000m CPU / 2Gi memory
# Default Request: 250m CPU / 512Mi memory
# Max: 8000m CPU / 16Gi memory
# Min: 50m CPU / 128Mi memory
```

## Monitoring

### Prometheus Alerts

Resource quota alerts are configured in `k8s/prometheus/resource-quota-alerts.yaml`:

| Alert | Threshold | Severity | Description |
|-------|-----------|----------|-------------|
| ResourceQuotaCPURequestsHigh | 80% | warning | CPU requests usage high |
| ResourceQuotaCPURequestsCritical | 90% | critical | CPU requests at critical level |
| ResourceQuotaMemoryRequestsHigh | 80% | warning | Memory requests usage high |
| ResourceQuotaMemoryRequestsCritical | 90% | critical | Memory requests at critical level |
| ResourceQuotaPodCountHigh | 80% | warning | Pod count approaching limit |
| ResourceQuotaPodCountCritical | 90% | critical | Cannot create new pods |
| ResourceQuotaStorageRequestsHigh | 80% | warning | Storage usage high |
| ResourceQuotaLoadBalancerCountHigh | 50% | warning | Cost control alert |

### Validation Script

Run the validation script to check quota health:

```bash
# Validate quotas in default namespace
./scripts/k8s/validate-resource-quota.sh

# Validate specific namespace
./scripts/k8s/validate-resource-quota.sh erlmcp-production

# With custom context
KUBECONTEXT=prod-cluster ./scripts/k8s/validate-resource-quota.sh erlmcp
```

### Dashboard Queries

Grafana queries for monitoring:

```promql
# CPU Requests Usage Percentage
(sum(kube_resourcequota{resource="requests.cpu", namespace="erlmcp"})
 / sum(kube_resourcequota_hard{resource="requests.cpu", namespace="erlmcp"})) * 100

# Memory Requests Usage Percentage
(sum(kube_resourcequota{resource="requests.memory", namespace="erlmcp"})
 / sum(kube_resourcequota_hard{resource="requests.memory", namespace="erlmcp"})) * 100

# Pod Count by Namespace
sum by(namespace) (kube_resourcequota{resource="pods"})

# Storage Quota Usage
sum by(namespace) (kube_resourcequota{resource="requests.storage"})
```

## Troubleshooting

### Issue: Pod Cannot Be Created (Quota Exceeded)

**Symptoms:**
```
Error creating pod: exceeded quota: erlmcp-compute-quota, requested: requests.cpu=500m, used: requests.cpu=32000m, limited: requests.cpu=32000m
```

**Solutions:**
1. Check current usage: `kubectl describe resourcequota erlmcp-compute-quota -n erlmcp`
2. Scale down non-critical pods
3. Increase quota limits (if capacity allows)
4. Delete unused pods

### Issue: Best Effort Pods Being Evicted

**Symptoms:**
Pods are frequently evicted under resource pressure.

**Solutions:**
1. Add resource requests to all pods
2. Set appropriate priority class
3. Use guaranteed QoS (requests = limits)

### Issue: PVC Cannot Be Created

**Symptoms:**
```
Error: exceeded quota: erlmcp-storage-quota, requested: requests.storage=10Gi, used: requests.storage=500Gi, limited: requests.storage=500Gi
```

**Solutions:**
1. Check PVC usage: `kubectl get pvc -n erlmcp`
2. Delete unused PVCs
3. Increase storage quota
4. Request smaller PVC size

### Issue: LoadBalancer Cost Spike

**Symptoms:**
Unexpected cloud provider charges.

**Solutions:**
1. Check LoadBalancer count: `kubectl get svc -n erlmcp --field-selector spec.type=LoadBalancer`
2. Convert to ClusterIP + Ingress where possible
3. Enforce stricter LoadBalancer quota

## Best Practices

### 1. Set Appropriate Resource Requests

```yaml
resources:
  requests:
    cpu: "500m"     # Set to typical usage
    memory: "512Mi" # Set to typical usage
  limits:
    cpu: "2000m"    # Set to max burst (4x requests)
    memory: "2Gi"   # Set to max burst (2x requests)
```

### 2. Use Priority Classes

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: critical-pod
spec:
  priorityClassName: high-priority
  containers:
  - name: app
    image: erlmcp:3.0.0
```

### 3. Monitor Quota Usage

Set up automated monitoring and alerts:
- Alert at 80% usage (warning)
- Alert at 90% usage (critical)
- Review quotas weekly

### 4. Plan for Growth

- Initially set quotas at 50% of cluster capacity
- Monitor usage trends
- Adjust quotas based on actual needs
- Reserve capacity for emergency scaling

### 5. Tenant Isolation

For multi-tenant deployments:
- Create separate namespaces per tenant
- Apply tenant-specific quotas
- Use NetworkPolicy for network isolation
- Implement ResourceQuota per namespace

### 6. Cost Control

- Limit LoadBalancer creation (cloud provider costs)
- Set storage quotas to prevent runaway disk usage
- Monitor pod count for licensing costs
- Use node selectors for cost-optimized instance types

## Example Values Reference

### Full Production Deployment

```bash
helm install erlmcp-prod ./helm/erlmcp-enterprise \
  --namespace erlmcp-production \
  --create-namespace \
  --set global.cluster.environment=production \
  --set resourceQuota.enabled=true \
  --set resourceQuota.types.compute.hard.requests.cpu=64 \
  --set resourceQuota.types.compute.hard.requests.memory=128Gi \
  --set resourceQuota.types.compute.hard.limits.cpu=128 \
  --set resourceQuota.types.compute.hard.limits.memory=256Gi \
  --set resourceQuota.types.storage.hard.persistentvolumeclaims=30 \
  --set resourceQuota.types.storage.hard.requests.storage=1Ti \
  --set resourceQuota.types.objects.hard.pods=200 \
  --set resourceQuota.types.objects.hard.services=50 \
  --set resourceQuota.types.objects.hard.services.loadbalancers=3 \
  --set limitRange.enabled=true \
  --set limitRange.container.default.cpu=2000m \
  --set limitRange.container.default.memory=4Gi \
  --set limitRange.container.defaultRequest.cpu=500m \
  --set limitRange.container.defaultRequest.memory=1Gi \
  -f helm/erlmcp-enterprise/values-production.yaml
```

### Development Environment

```bash
helm install erlmcp-dev ./helm/erlmcp-enterprise \
  --namespace erlmcp-dev \
  --create-namespace \
  --set global.cluster.environment=development \
  --set resourceQuota.enabled=true \
  --set resourceQuota.types.compute.hard.requests.cpu=4 \
  --set resourceQuota.types.compute.hard.requests.memory=8Gi \
  --set resourceQuota.types.objects.hard.pods=20 \
  --set resourceQuota.types.objects.hard.services.loadbalancers=0 \
  -f helm/erlmcp-enterprise/values-dev.yaml
```

## Files Reference

| File | Purpose |
|------|---------|
| `k8s/resource-quota.yaml` | Standalone Kubernetes manifests |
| `helm/erlmcp-enterprise/templates/resourcequota.yaml` | Helm template for ResourceQuota |
| `helm/erlmcp-enterprise/templates/limitrange.yaml` | Helm template for LimitRange |
| `helm/erlmcp-enterprise/values.yaml` | Default values with quota configuration |
| `helm/erlmcp-enterprise/values-production.yaml` | Production quota overrides |
| `helm/erlmcp-enterprise/values-staging.yaml` | Staging quota overrides |
| `helm/erlmcp-enterprise/values-dev.yaml` | Development quota overrides |
| `k8s/prometheus/resource-quota-alerts.yaml` | Prometheus alerting rules |
| `scripts/k8s/validate-resource-quota.sh` | Quota validation script |

## Support

For issues or questions:
- Run the validation script: `./scripts/k8s/validate-resource-quota.sh`
- Check Prometheus alerts for quota violations
- Review the troubleshooting guide above
- Contact platform-team@erlmcp.com
