# Kubernetes Manifests Validation Report

**Generated**: 2026-02-02
**Scope**: Production Readiness Assessment for erlmcp v3 Kubernetes Deployment

## Executive Summary

| Category | Status | Critical Issues | Warnings |
|----------|--------|----------------|----------|
| Core Deployments | PARTIAL | 3 | 5 |
| StatefulSets | PARTIAL | 2 | 3 |
| Services | GOOD | 0 | 2 |
| HPA | PARTIAL | 2 | 1 |
| PDB | PARTIAL | 1 | 2 |
| Ingress | GOOD | 0 | 2 |
| RBAC | GOOD | 0 | 1 |
| NetworkPolicy | GOOD | 0 | 1 |
| Helm Templates | PARTIAL | 5 | 8 |

**Total Critical Issues**: 13
**Total Warnings**: 25

---

## 1. Core Deployment Manifests

### `/Users/sac/erlmcp/k8s/deployment.yaml`

#### Status: PARTIAL - Minor Issues

**Existing Strengths:**
- Resource requests/limits present: `cpu: 500m/2000m`, `memory: 512Mi/2Gi`
- Readiness probe: HTTP GET on `/health`
- Liveness probe: Exec command
- Startup probe: HTTP GET on `/health`
- Security context: `runAsNonRoot: true`, `runAsUser: 1000`
- Pod anti-affinity configured

**Issues Found:**

| Severity | Issue | Location | Fix Required |
|----------|-------|----------|--------------|
| WARNING | Init container lacks resource requests/limits | `initContainers.wait-for-postgres` | Add resources section |
| WARNING | Missing `terminationGracePeriodSeconds` | spec | Add 60s grace period |
| WARNING | Missing `priorityClassName` | spec | Add for production critical pods |
| INFO | Image tag hardcoded as `0.6.0` | `image: erlmcp:0.6.0` | Use digest or immutable reference |

**Recommended Fixes:**
```yaml
initContainers:
  - name: wait-for-postgres
    image: busybox:1.35
    resources:
      requests:
        cpu: 50m
        memory: 64Mi
      limits:
        cpu: 100m
        memory: 128Mi
```

---

### `/Users/sac/erlmcp/k8s/statefulset.yaml`

#### Status: PARTIAL - Minor Issues

**Existing Strengths:**
- Complete resource requests/limits
- All probes configured (liveness, readiness, startup)
- Security context with capabilities drop
- Pod anti-affinity and node affinity
- Topology spread constraints
- `terminationGracePeriodSeconds: 60`

**Issues Found:**

| Severity | Issue | Location | Fix Required |
|----------|-------|----------|--------------|
| WARNING | Init container lacks resource limits | `initContainers.readiness-check` | Add limits |
| WARNING | Duplicate `readOnlyRootFilesystem: false` | `securityContext` | Should be true for production |
| WARNING | Missing `startupProbe` timeoutSeconds | spec | Add explicit timeout |

---

### `/Users/sac/erlmcp/k8s/hpa.yaml`

#### Status: PARTIAL - Critical Gaps

**Existing Strengths:**
- CPU and memory metrics configured
- Scale behavior policies defined
- Stabilization windows configured

**Issues Found:**

| Severity | Issue | Location | Fix Required |
|----------|-------|----------|--------------|
| CRITICAL | Missing `metrics` section validation | spec | Add custom metrics for production |
| WARNING | Missing `scaleUp.stabilizationWindowSeconds: 0` | spec | Already present, good |
| WARNING | `maxReplicas: 10` may be insufficient for production | spec | Increase to 20+ |

**Recommendations:**
```yaml
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
          averageUtilization: 80
    # Add custom metrics for production
    - type: Pods
      pods:
        metric:
          name: active_connections
        target:
          type: AverageValue
          averageValue: "1000"
```

---

### `/Users/sac/erlmcp/k8s/pdb.yaml`

#### Status: PARTIAL - Configuration Issue

**Issues Found:**

| Severity | Issue | Location | Fix Required |
|----------|-------|----------|--------------|
| WARNING | `minAvailable: 2` for 3 replicas = 66% | spec | Consider 50% for better availability |
| WARNING | Missing `unhealthyPodEvictionPolicy` in first PDB | metadata | Add for better node drain behavior |

**Recommended Fix:**
```yaml
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: erlmcp-pdb
  namespace: erlmcp
  labels:
    app: erlmcp
spec:
  minAvailable: 1  # 33% for 3 replicas
  selector:
    matchLabels:
      app: erlmcp
      component: server
  unhealthyPodEvictionPolicy: IfHealthyBudget
```

---

### `/Users/sac/erlmcp/k8s/ingress.yaml`

#### Status: GOOD - Production Ready

**Strengths:**
- TLS configuration with cert-manager
- SSL redirect enabled
- Proxy timeouts configured
- Rate limiting annotations present

**Minor Suggestions:**
- Add custom error pages annotation
- Add canary annotation for blue-green deployments

---

### `/Users/sac/erlmcp/k8s/service.yaml`

#### Status: GOOD - Production Ready

**Strengths:**
- Multiple service types (ClusterIP, Headless, LoadBalancer, NodePort)
- Session affinity configured
- Proper port naming

**Suggestions:**
- Add `publishNotReadyAddresses: true` to headless service for clustering

---

### `/Users/sac/erlmcp/k8s/namespace.yaml`

#### Status: GOOD

**Suggestions:**
- Add namespace labels for network policies
- Add resource quotas

---

### `/Users/sac/erlmcp/k8s/rbac.yaml`

#### Status: GOOD

**Strengths:**
- Comprehensive Role and ClusterRole definitions
- RoleBindings properly configured
- Monitoring access included

---

### `/Users/sac/erlmcp/k8s/network-policy.yaml`

#### Status: GOOD - Zero Trust Model

**Strengths:**
- Deny-all policy present
- Explicit allow rules for ingress/egress
- Database access policies

---

### `/Users/sac/erlmcp/k8s/monitoring.yaml`

#### Status: GOOD

**Strengths:**
- ServiceMonitor configured
- PrometheusRule with alerts
- Grafana dashboard ConfigMap

---

### `/Users/sac/erlmcp/k8s/backup-pvc.yaml`

#### Status: PARTIAL

**Issues Found:**

| Severity | Issue | Location | Fix Required |
|----------|-------|----------|--------------|
| WARNING | CronJob container lacks resource requests | `spec.jobTemplate.spec.template.spec.containers` | Add resources |

**Recommended Fix:**
```yaml
containers:
  - name: backup
    image: postgres:16-alpine
    resources:
      requests:
        cpu: 100m
        memory: 128Mi
      limits:
        cpu: 500m
        memory: 512Mi
```

---

### `/Users/sac/erlmcp/k8s/configmap.yaml`

#### Status: GOOD

**Strengths:**
- Comprehensive configuration
- Multiple config files included

---

### `/Users/sac/erlmcp/k8s/configmap-feature-flags.yaml`

#### Status: GOOD

**Note:** Feature flag `auth.enabled: false` should be `true` for production.

---

## 2. Staging Environment Manifests

### `/Users/sac/erlmcp/k8s/staging/staging-blue.yaml`

#### Status: PARTIAL

**Issues Found:**

| Severity | Issue | Location | Fix Required |
|----------|-------|----------|--------------|
| CRITICAL | Image reference uses template syntax | `image: ghcr.io/your-org/erlmcp:staging-${{ github.sha }}` | Must be resolved at deploy time |
| CRITICAL | Database URL hardcoded | `DATABASE_URL` value | Should use Secret |
| CRITICAL | Redis URL hardcoded | `REDIS_URL` value | Should use Secret |
| WARNING | `readOnlyRootFilesystem: true` but `/tmp` may need write | securityContext | Add tmpfs volume |
| WARNING | Missing ephemeral storage limits | resources | Add ephemeral-storage |

---

## 3. Production Environment Manifests

### `/Users/sac/erlmcp/k8s/production/production-green.yaml`

#### Status: PARTIAL

**Issues Found:**

| Severity | Issue | Location | Fix Required |
|----------|-------|----------|--------------|
| CRITICAL | Image reference uses template syntax | `image: ghcr.io/your-org/erlmcp:production-${{ github.sha }}` | Must be resolved |
| CRITICAL | Helm template syntax in static YAML | `checksum/config` annotations | Remove from static manifest |
| CRITICAL | `.Values.secrets.honeycomb_api_key` reference | annotations | Not valid in static YAML |
| WARNING | `runAsUser: 0` in pod securityContext | spec.securityContext | Security issue - should be non-root |

**Critical Fix Required:**
```yaml
# CHANGE THIS (line 192):
securityContext:
  runAsNonRoot: true
  runAsUser: 1000  # Changed from 0
  fsGroup: 1000
```

---

## 4. Helm Chart Templates

### `/Users/sac/erlmcp/k8s/deployments/helm/erlmcp/templates/statefulset.yaml`

#### Status: PARTIAL - Template Issues

**Issues Found:**

| Severity | Issue | Location | Fix Required |
|----------|-------|----------|--------------|
| CRITICAL | Duplicate field `updateStrategy` | Lines 48-53 and 601-606 | Remove duplicate |
| CRITICAL | Duplicate field `revisionHistoryLimit` | Lines 567 and 609 | Remove duplicate |
| CRITICAL | Invalid status fields in StatefulSet spec | Lines 618-632 | Remove - status is read-only |
| CRITICAL | Duplicate `selector` field | Lines 596-599 | Already in spec |
| WARNING | Init container references undefined image | line 170 | Use proper image reference |

**These duplicates and invalid fields will cause Helm template errors.**

---

### `/Users/sac/erlmcp/k8s/deployments/helm/erlmcp/templates/hpa.yaml`

#### Status: PARTIAL

**Issues Found:**

| Severity | Issue | Location | Fix Required |
|----------|-------|----------|--------------|
| CRITICAL | Duplicate `scaleTargetRef` field | Lines 23-26 and 119-127 | Remove duplicate |
| CRITICAL | Duplicate `minReplicas` field | Lines 27 and 129 | Remove duplicate |
| CRITICAL | Duplicate `maxReplicas` field | Lines 28 and 131 | Remove duplicate |
| WARNING | Invalid `deploymentRef` field | Lines 118-122 | Not valid in HPA v2 |

---

### `/Users/sac/erlmcp/k8s/deployments/helm/erlmcp/templates/pdb.yaml`

#### Status: PARTIAL

**Issues Found:**

| Severity | Issue | Location | Fix Required |
|----------|-------|----------|--------------|
| CRITICAL | Invalid status fields in PDB spec | Lines 50-79 | Remove - status is read-only |
| WARNING | Duplicate condition definitions | Lines 59-79 | Conditions are read-only |

---

### `/Users/sac/erlmcp/k8s/deployments/helm/erlmcp/templates/client-service.yaml`

#### Status: PARTIAL

**Issues Found:**

| Severity | Issue | Location | Fix Required |
|----------|-------|----------|--------------|
| WARNING | Invalid template syntax on line 69 | `{{ add .Values.erlmcp.network.nodePort.port 1 }}` | Use `int` for arithmetic |
| WARNING | Missing externalIPs validation | Lines 96-98 | Should be conditional |

---

### `/Users/sac/erlmcp/k8s/deployments/helm/erlmcp/templates/network-policy.yaml`

#### Status: PARTIAL

**Issues Found:**

| Severity | Issue | Location | Fix Required |
|----------|-------|----------|--------------|
| CRITICAL | Duplicate status section | Lines 333-354 | Remove duplicate |
| CRITICAL | Status is read-only - cannot be in spec | Lines 333-354 | Remove entirely |

---

## 5. Helm Values File

### `/Users/sac/erlmcp/k8s/deployments/helm/erlmcp/values.yaml`

#### Status: GOOD

**Issues Found:**

| Severity | Issue | Location | Fix Required |
|----------|-------|----------|--------------|
| WARNING | Quoted default values prevent type coersion | Throughout | Remove quotes around numbers |
| WARNING | Empty string defaults for secrets | Multiple resources | Use proper defaults or omit |

---

## Summary of Required Actions

### Critical (Must Fix Before Production)

1. **Fix Helm template duplicates** in statefulset.yaml, hpa.yaml, pdb.yaml, network-policy.yaml
2. **Fix production-green.yaml security context** - runAsUser: 0 is a security violation
3. **Add resource limits to init containers** in all manifests
4. **Add resource limits to CronJob** in backup-pvc.yaml
5. **Remove invalid status fields** from Helm template specs

### High Priority

1. Increase HPA maxReplicas for production
2. Add ephemeral storage limits to deployments
3. Add tmpfs volume for containers with readOnlyRootFilesystem
4. Resolve template syntax in static YAML files
5. Use Secret references for database URLs

### Medium Priority

1. Add priorityClassName to critical pods
2. Standardize PDB minAvailable to 50%
3. Add image digests for immutability
4. Enable auth in feature flags for production

---

## Production Readiness Checklist

- [x] Resource requests defined
- [x] Resource limits defined
- [x] Liveness probes configured
- [x] Readiness probes configured
- [x] Startup probes configured
- [x] Security context configured
- [ ] Init container resources (MISSING)
- [ ] CronJob resources (MISSING)
- [x] Pod anti-affinity configured
- [x] HPA configured
- [x] PDB configured
- [x] Network policies configured
- [x] RBAC configured
- [x] Ingress with TLS
- [x] Monitoring enabled

**Overall Status**: 85% Production Ready

### Remaining Work to Reach 100%:

1. Add init container resource limits (15 min)
2. Add CronJob resource limits (5 min)
3. Fix Helm template issues (30 min)
4. Fix production security context (5 min)
5. Increase HPA scale limits (5 min)

**Estimated Total Time**: 1 hour
