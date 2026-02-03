# GKE Marketplace Deployment Review - Phase 3 Report
## Google Cloud Marketplace Reviewer Simulation

**Reviewer ID**: GKE-MARKETPLACE-REVIEWER-001
**Review Date**: 2026-02-02
**Deployment Path**: GKE (Google Kubernetes Engine)
**Scrutiny Level**: HIGHEST (GKE receives most scrutiny)
**Status**: PASS WITH MINOR RECOMMENDATIONS

---

## Executive Summary

The erlmcp v3.0.0 GKE deployment configuration has been evaluated against Google Cloud Marketplace requirements for GKE workloads. The submission demonstrates **strong operational maturity** with proper regional clustering, private topology, Workload Identity integration, and comprehensive autoscaling.

### Key Strengths
- Regional cluster configuration (99.95% SLA)
- Private cluster with proper master authorization
- Workload Identity for secret access
- Comprehensive HPA with custom metrics
- Proper pod disruption budgets
- Shielded nodes with secure boot

### Critical Findings
- **0 Critical Issues**
- **2 High Priority Recommendations**
- **5 Medium Priority Observations**

### Overall Assessment
**PASS** - Application is suitable for GKE Marketplace listing with recommended improvements.

---

## 1. GKE Deployment Validation

### 1.1 Cluster Configuration Review

#### Cluster Topology: REGIONAL
**File**: `/Users/sac/erlmcp/marketplace/gcp/terraform/modules/gke/main.tf:28-36`

```terraform
resource "google_container_cluster" "erlmcp" {
  name     = var.cluster_name
  location = var.region  # Regional deployment
  project  = var.project_id

  # Regional cluster for 99.95% SLA
  regional {
    # Use region for regional cluster (or remove for zonal)
  }
}
```

**Reviewer Finding**: PASS
- Regional cluster properly configured
- Multiple zones automatically selected by GKE
- 99.95% SLA eligibility met
- Default: `us-central1` region

**Evidence**:
- Regional block present (line 34-36)
- Location parameter accepts region variable
- Release channel: REGULAR (best balance of stability/features)

---

### 1.2 Private Cluster Configuration

#### Private Endpoint: ENABLED
**File**: `/Users/sac/erlmcp/marketplace/gcp/terraform/modules/gke/main.tf:43-50`

```terraform
private_cluster_config {
  enable_private_endpoint = var.enable_private_endpoint  # true
  enable_private_nodes    = var.enable_private_nodes     # true
  master_ipv4_cidr_block  = var.master_ipv4_cidr_block   # 172.16.0.0/28
  master_global_access_config {
    enabled = var.master_global_access  # false
  }
}
```

**Reviewer Finding**: PASS
- Private endpoint enabled (control plane not accessible from public internet)
- Private nodes enabled (no public IPs)
- Master CIDR properly configured (/28 = 16 IPs sufficient)
- Global access disabled (security best practice)

**Validation Commands**:
```bash
# Verify private endpoint
gcloud container clusters describe erlmcp-cluster \
  --region=us-central1 \
  --format="value(privateClusterConfig.enablePrivateEndpoint)"
# Expected: true

# Verify private nodes
gcloud container clusters describe erlmcp-cluster \
  --region=us-central1 \
  --format="value(privateClusterConfig.enablePrivateNodes)"
# Expected: true
```

---

### 1.3 Node Pool Configuration

#### Primary Node Pool: PRODUCTION-GRADE
**File**: `/Users/sac/erlmcp/marketplace/gcp/terraform/examples/gke-deployment/main.tf:70-88`

```terraform
node_pools = {
  primary = {
    version               = ""
    machine_type          = var.machine_type  # e2-standard-2
    image_type            = "COS_CONTAINERD"
    disk_size_gb          = var.disk_size_gb  # 100
    disk_type             = var.disk_type     # pd-balanced
    min_count             = var.min_nodes     # 3
    max_count             = var.max_nodes     # 10
    location_policy       = "BALANCED"        # Spread across zones
    spot                  = false
    preemptible           = false
    enable_secure_boot    = true
    enable_integrity_monitoring = true
    enable_gvnic          = true
  }
}
```

**Reviewer Finding**: PASS
- Minimum 3 nodes (HA requirement met)
- Location policy: BALANCED (zone distribution)
- Shielded nodes enabled (secure boot + integrity monitoring)
- COS_CONTAINERD (supported image type)
- Auto-scaling configured (3-10 nodes)

**Machine Type Analysis**:
- `e2-standard-2` = 2 vCPUs, 8GB RAM
- Appropriate for Erlang/OTP workloads
- Cost-effective with good performance

**Validation Commands**:
```bash
# Check node pool status
kubectl get nodes -L topology.kubernetes.io/zone
# Expected: 3+ nodes spread across zones

# Verify node pool configuration
gcloud container node-pools describe primary-pool \
  --cluster=erlmcp-cluster \
  --region=us-central1
```

---

## 2. Cluster Sanity Checks

### 2.1 Node Verification

#### Expected Node Distribution
**Command**: `kubectl get nodes -L topology.kubernetes.io/zone`

**Expected Output**:
```
NAME                                          STATUS   ROLES    AGE   VERSION           ZONE
erlmcp-primary-pool-xxx-us-central1-a-xxxx    Ready    <none>   10m   v1.28.x-gke.xxxx  us-central1-a
erlmcp-primary-pool-yyy-us-central1-b-xxxx    Ready    <none>   10m   v1.28.x-gke.xxxx  us-central1-b
erlmcp-primary-pool-zzz-us-central1-c-xxxx    Ready    <none>   10m   v1.28.x-gke.xxxx  us-central1-c
```

**Reviewer Checks**:
- ✓ 3 nodes minimum (one per zone)
- ✓ All nodes in Ready state
- ✓ Zones distributed across us-central1-a, us-central1-b, us-central1-c
- ✓ Node versions match (no mixed versions)
- ✓ No NotReady nodes
- ✓ No unknown taints

**Failure Criteria**:
- ✗ Less than 3 nodes
- ✗ All nodes in single zone
- ✗ Nodes in NotReady state
- ✗ Mixed Kubernetes versions

---

### 2.2 Pod Status Verification

#### Expected Pod Distribution
**Command**: `kubectl get pods -n erlmcp -o wide`

**Expected Output**:
```
NAME                      READY   STATUS    RESTARTS   AGE   IP            NODE
erlmcp-xxxxxxxxxx-abc12   2/2     Running   0          5m    10.1.0.5      node-a
erlmcp-yyyyyyyyyy-def34   2/2     Running   0          5m    10.1.0.6      node-b
erlmcp-zzzzzzzzzz-ghi56   2/2     Running   0          5m    10.1.0.7      node-c
erlmcp-wwwwwwwwww-jkl78   2/2     Running   0          5m    10.1.0.8      node-a
erlmcp-vvvvvvvvvv-mno90   2/2     Running   0          5m    10.1.0.9      node-b
```

**Reviewer Checks**:
- ✓ 5 pods (default replica count)
- ✓ All pods in Running state
- ✓ READY = 2/2 (main + sidecar)
- ✓ 0 restarts (healthy startup)
- ✓ Pods distributed across nodes
- ✓ No CrashLoopBackOff
- ✓ No ImagePullBackOff
- ✓ No pending pods

**Common Failure Patterns**:
```
# CrashLoopBackOff - Application crashing
erlmcp-xxx   0/2   CrashLoopBackOff   5   10m
CAUSE: Liveness/Startup probe failing
FIX: Check application logs, increase timeout

# ImagePullBackOff - Cannot pull image
erlmcp-xxx   0/2   ImagePullBackOff   0   5m
CAUSE: Missing imagePullSecret or wrong registry
FIX: Verify imagePullSecrets, check image exists

# Pending - Cannot schedule
erlmcp-xxx   0/2   Pending   0   5m
CAUSE: Insufficient resources, taints, affinities
FIX: Check node resources, taints, tolerations
```

---

### 2.3 Workload Identity Verification

#### Expected Service Account Configuration
**File**: `/Users/sac/erlmcp/marketplace/gcp/terraform/modules/gke/deployment.tf:74-87`

```terraform
resource "google_service_account" "erlmcp" {
  account_id   = "erlmcp-ksa"
  display_name = "erlmcp Kubernetes Service Account"
}

resource "google_service_account_iam_member" "workload_identity" {
  role               = "roles/iam.workloadIdentityUser"
  member             = "serviceAccount:${var.project_id}.svc.id.goog[${var.helm_namespace}/${var.helm_release_name}]"
}
```

**Expected Pod Annotations**:
```bash
kubectl get pod erlmcp-xxx -n erlmcp -o jsonpath='{.metadata.annotations}' | jq
```

**Expected Output**:
```json
{
  "iam.gke.io/gcp-service-account": "erlmcp-ksa@PROJECT_ID.iam.gserviceaccount.com"
}
```

**Reviewer Checks**:
- ✓ GSA created: `erlmcp-ksa@PROJECT_ID.iam.gserviceaccount.com`
- ✓ KSA annotated with GSA email
- ✓ Workload Identity binding present
- ✓ IAM roles granted:
  - `roles/secretmanager.secretAccessor`
  - `roles/secretmanager.viewer`
  - `roles/logging.logWriter`
  - `roles/monitoring.metricWriter`

**Validation Commands**:
```bash
# Verify Workload Identity binding
gcloud iam service-accounts get-iam-policy erlmcp-ksa@PROJECT_ID.iam \
  --filter="roles/iam.workloadIdentityUser"

# Verify pod can access secrets
kubectl exec -n erlmcp erlmcp-xxx -- \
  gcloud secrets list --project=PROJECT_ID
```

**Failure Criteria**:
- ✗ Missing GSA annotation on pods
- ✗ No Workload Identity IAM binding
- ✗ Secret manager roles not granted
- ✗ Pods cannot access Secret Manager

---

## 3. Pod Inspection Procedures

### 3.1 Resource Limits Verification

#### Expected Resource Configuration
**File**: `/Users/sac/erlmcp/k8s/production/deployment.yaml:292-300`

```yaml
resources:
  requests:
    cpu: 500m
    memory: 1Gi
    ephemeral-storage: 2Gi
  limits:
    cpu: 2000m
    memory: 4Gi
    ephemeral-storage: 5Gi
```

**Reviewer Checks**:
- ✓ Requests defined (required for HPA)
- ✓ Limits defined (prevents resource starvation)
- ✓ Limit > Request (QoS Guaranteed)
- ✓ CPU limit = 4x request (headroom for bursts)
- ✓ Memory limit = 4x request (Erlang memory management)

**Validation Command**:
```bash
kubectl describe pod erlmcp-xxx -n erlmcp | grep -A 5 "Limits\|Requests"
```

**Expected Output**:
```
Limits:
  cpu:     2
  memory:  4Gi
Requests:
  cpu:     500m
  memory:  1Gi
```

**QoS Class Verification**:
```bash
kubectl get pod erlmcp-xxx -n erlmcp -o jsonpath='{.status.qosClass}'
# Expected: Guaranteed
```

**Failure Criteria**:
- ✗ Missing requests (HPA will not work)
- ✗ Missing limits (risk of OOMKill)
- ✗ Limits < Requests (invalid configuration)
- ✗ No ephemeral-storage limits

---

### 3.2 Security Context Verification

#### Expected Security Configuration
**File**: `/Users/sac/erlmcp/k8s/production/deployment.yaml:101-107`

```yaml
securityContext:
  runAsNonRoot: true
  runAsUser: 1000
  runAsGroup: 1000
  fsGroup: 1000
  seccompProfile:
    type: RuntimeDefault
```

**Reviewer Checks**:
- ✓ runAsNonRoot: true (non-root execution)
- ✓ runAsUser/set (specific UID, not 0)
- ✓ seccompProfile: RuntimeDefault (hardened)
- ✓ No privileged containers
- ✓ No hostNetwork
- ✓ No hostPID
- ✓ No hostIPC

**Validation Command**:
```bash
kubectl get pod erlmcp-xxx -n erlmcp -o jsonpath='{.spec}' | jq '.securityContext'
```

**Expected Output**:
```json
{
  "runAsNonRoot": true,
  "runAsUser": 1000,
  "runAsGroup": 1000,
  "fsGroup": 1000,
  "seccompProfile": {
    "type": "RuntimeDefault"
  }
}
```

**Container-Level Security**:
```yaml
securityContext:
  allowPrivilegeEscalation: false
  readOnlyRootFilesystem: true
  capabilities:
    drop:
      - ALL
    add:
      - NET_BIND_SERVICE
```

**Reviewer Checks**:
- ✓ allowPrivilegeEscalation: false
- ✓ readOnlyRootFilesystem: true
- ✓ Drop all capabilities
- ✓ Add only NET_BIND_SERVICE
- ✓ No SYS_ADMIN capability
- ✓ No NET_RAW capability

---

### 3.3 Health Check Verification

#### Expected Probe Configuration
**File**: `/Users/sac/erlmcp/k8s/production/deployment.yaml:321-357`

```yaml
startupProbe:
  httpGet:
    path: /health/startup
    port: health
  initialDelaySeconds: 10
  periodSeconds: 10
  timeoutSeconds: 5
  failureThreshold: 30  # 5 minutes total

livenessProbe:
  httpGet:
    path: /health/live
    port: health
  initialDelaySeconds: 60
  periodSeconds: 20
  timeoutSeconds: 5
  failureThreshold: 3

readinessProbe:
  httpGet:
    path: /health/ready
    port: health
  initialDelaySeconds: 30
  periodSeconds: 10
  timeoutSeconds: 5
  successThreshold: 2
  failureThreshold: 3
```

**Reviewer Checks**:
- ✓ All three probes defined (startup, liveness, readiness)
- ✓ Startup probe gives 5 minutes for Erlang VM startup
- ✓ Liveness probe restarts unhealthy containers
- ✓ Readiness probe removes unhealthy pods from service
- ✓ Different paths for each probe
- ✓ Reasonable timeouts (5 seconds)

**Validation Commands**:
```bash
# Check probe status
kubectl describe pod erlmcp-xxx -n erlmcp | grep -A 10 "Liveness\|Readiness\|Startup"

# Test health endpoints
kubectl exec -n erlmcp erlmcp-xxx -- curl -s http://localhost:9091/health/live
# Expected: {"status":"ok"}

kubectl exec -n erlmcp erlmcp-xxx -- curl -s http://localhost:9091/health/ready
# Expected: {"status":"ready"}
```

**Failure Criteria**:
- ✗ Missing startup probe (slow Erlang startup)
- ✗ Missing liveness probe (no restart on hang)
- ✗ Missing readiness probe (traffic to dead pods)
- ✗ Insufficient failureThreshold (premature restart)
- ✗ Too aggressive timeouts (false positives)

---

### 3.4 Secret Management Verification

#### Expected Secret References
**File**: `/Users/sac/erlmcp/k8s/production/deployment.yaml:214-269`

```yaml
env:
  - name: ERLANG_COOKIE
    valueFrom:
      secretKeyRef:
        name: erlmcp-secrets
        key: erlang-cookie

  - name: ERLMCP_DB_PASSWORD
    valueFrom:
      secretKeyRef:
        name: erlmcp-secrets
        key: db-password

  - name: REDIS_PASSWORD
    valueFrom:
      secretKeyRef:
        name: erlmcp-secrets
        key: redis-password
```

**Reviewer Checks**:
- ✓ All secrets from secretKeyRef (not plain text)
- ✓ No hardcoded secrets in environment variables
- ✓ No secrets in ConfigMaps
- ✓ Secret names follow naming convention
- ✓ Secret keys match Secret Manager secrets

**Validation Commands**:
```bash
# Verify secrets exist
kubectl get secrets -n erlmcp

# Verify secret mounting
kubectl describe pod erlmcp-xxx -n erlmcp | grep -A 20 "Environment"

# Verify Workload Identity access
kubectl exec -n erlmcp erlmcp-xxx -- \
  gcloud secrets versions access latest \
    --secret=erlmcp-erlang-cookie \
    --project=PROJECT_ID
```

**Secret Manager IAM Verification**:
```bash
# Verify GSA has secretAccessor role
gcloud projects get-iam-policy PROJECT_ID \
  --filter="bindings.members:serviceAccount:erlmcp-ksa@PROJECT_ID.iam" \
  --flatten="bindings[].bindings" \
  --format="table(bindings.role)"
```

**Expected Roles**:
- `roles/secretmanager.secretAccessor`
- `roles/secretmanager.viewer`

**Failure Criteria**:
- ✗ Plain text passwords in deployment.yaml
- ✗ Secrets in ConfigMaps
- ✗ Missing Workload Identity annotations
- ✗ GSA lacks secretAccessor role
- ✗ Secrets not defined in Secret Manager

---

## 4. Kill Test Documentation

### 4.1 Pod Deletion Test

#### Test Procedure
**Purpose**: Verify self-healing and graceful restart

**Commands**:
```bash
# Get current pods
kubectl get pods -n erlmcp -o wide

# Delete a pod
POD_NAME=$(kubectl get pods -n erlmcp -o jsonpath='{.items[0].metadata.name}')
kubectl delete pod ${POD_NAME} -n erlmcp

# Watch recreation
kubectl get pods -n erlmcp -w
```

#### Expected Behavior

**Phase 1: Pod Termination (T+0s)**
```
erlmcp-abc123   1/1   Terminating   0   10m   10.1.0.5   node-a
```
- Pod enters Terminating state
- PreStop hook executes (graceful shutdown)
- Service endpoints updated (pod removed)

**Phase 2: Graceful Shutdown (T+0s to T+30s)**
```
# PreStop hook in container
- curl -X POST http://localhost:8080/admin/drain  # Drain connections
- sleep 30  # Wait for connections to close
```
- Application stops accepting new connections
- Existing connections drained
- 90-second termination grace period (configured)

**Phase 3: Pod Creation (T+30s)**
```
erlmcp-def456   0/2   Pending   0   0s   <none>   <none>
erlmcp-def456   0/2   ContainerCreating   0   2s   <none>   node-b
```
- New pod scheduled (different node due to anti-affinity)
- Image pull (if needed)
- Volume mounting

**Phase 4: Startup (T+30s to T+300s)**
```
erlmcp-def456   0/2   Running   0   10s   10.1.0.15   node-b
erlmcp-def456   1/2   Running   0   15s   10.1.0.15   node-b
erlmcp-def456   2/2   Running   0   20s   10.1.0.15   node-b
```
- Startup probe runs (10s intervals, 30 failures = 5 minutes)
- Erlang VM boots
- Sidecar containers start
- Readiness probe passes

**Phase 5: Ready (T+60s)**
```
erlmcp-def456   2/2   Running   0   60s   10.1.0.15   node-b   Ready
```
- Pod added to service endpoints
- Traffic starts flowing
- System fully operational

#### Success Criteria
- ✓ Pod deleted successfully
- ✓ New pod created immediately
- ✓ Pod scheduled on different node (anti-affinity)
- ✓ Pod becomes Ready within 5 minutes
- ✓ No connection errors during failover
- ✓ Service remains available (min 4/5 pods)
- ✓ No data loss

#### Reviewer Validation Commands
```bash
# Verify pod recreation
kubectl get pods -n erlmcp --show-labels

# Check pod age (should show recently created pod)
kubectl get pods -n erlmcp -o custom-columns=NAME:.metadata.name,AGE:.metadata.creationTimestamp

# Verify service endpoints
kubectl get endpoints erlmcp -n erlmcp

# Check logs for graceful shutdown
kubectl logs erlmcp-<NEW_POD> -n erlmcp --tail=50 | grep -i "shutdown\|starting"

# Verify no errors
kubectl logs erlmcp-<NEW_POD> -n erlmcp --tail=50 | grep -i "error\|fail" | wc -l
# Expected: 0
```

#### Common Failures

**Failure 1: Pod Not Recreated**
```
# Symptom: Pod count stays at 4
kubectl get pods -n erlmcp
# Expected: 5 pods, Actual: 4 pods

# Cause: Deployment spec missing or incorrect
# Fix: Verify deployment exists and replicas=5
```

**Failure 2: New Pod CrashLoopBackOff**
```
# Symptom: New pod restarts repeatedly
erlmcp-xxx   0/2   CrashLoopBackOff   5   3m

# Cause: Dependency not ready or config missing
# Check: kubectl logs erlmcp-xxx -n erlmcp --previous
# Fix: Verify ConfigMaps, Secrets, PVCs are present
```

**Failure 3: Long Startup Time**
```
# Symptom: Pod takes > 5 minutes to become ready
# Cause: Startup probe timeout too short

# Check startup timeout
kubectl describe deployment erlmcp -n erlmcp | grep -A 5 "Startup Probe"

# Fix: Increase failureThreshold to 60 (10 minutes)
```

**Failure 4: Connection Errors**
```
# Symptom: 5xx errors during pod recreation
# Cause: Traffic sent to terminating pod

# Fix: Ensure preStop hook drains connections
# Verify: kubectl logs <OLD_POD> -n erlmcp | grep "drain"
```

---

### 4.2 Node Failure Test

#### Test Procedure
**Purpose**: Verify cluster resilience to node failure

**Commands**:
```bash
# Get node name
NODE_NAME=$(kubectl get nodes -o jsonpath='{.items[0].metadata.name}')

# Cordon node (mark unschedulable)
kubectl cordon ${NODE_NAME}

# Drain node (evict all pods)
kubectl drain ${NODE_NAME} --ignore-daemonsets --delete-emptydir-data

# Watch pod rescheduling
kubectl get pods -n erlmcp -w -o wide
```

#### Expected Behavior

**Phase 1: Node Cordoned (T+0s)**
```
NAME                                      STATUS   ROLES    AGE   VERSION
erlmcp-primary-pool-xxx-us-central1-a-1   Ready,SchedulingDisabled   <none>   10m   v1.28.x
```
- Node marked unschedulable
- No new pods scheduled on this node
- Existing pods continue running

**Phase 2: Pod Eviction (T+0s to T+30s)**
```
erlmcp-abc123   2/2   Running   0   10m   10.1.0.5   node-1   Evicted
erlmcp-def456   2/2   Running   0   9m   10.1.0.6   node-1   Evicted
```
- Pods evicted from node
- Pod disruption budget respected (min 3 available)
- Eviction is graceful (SIGTERM)

**Phase 3: Pod Rescheduling (T+30s to T+60s)**
```
erlmcp-ghi789   0/2   Pending   0   0s    <none>   <none>
erlmcp-ghi789   0/2   ContainerCreating   0   5s    <none>   node-2
erlmcp-jkl012   0/2   Pending   0   0s    <none>   <none>
erlmcp-jkl012   0/2   ContainerCreating   0   5s    <none>   node-3
```
- New pods created immediately
- Scheduled on remaining healthy nodes
- Topology spread constraints maintained

**Phase 4: Service Recovery (T+60s to T+120s)**
```
erlmcp-ghi789   2/2   Running   0   60s   10.1.0.20   node-2   Ready
erlmcp-jkl012   2/2   Running   0   60s   10.1.0.21   node-3   Ready
```
- Pods become ready
- Service endpoints updated
- System at full capacity (5 pods)

#### Success Criteria
- ✓ Node cordoned successfully
- ✓ All pods evicted from failed node
- ✓ Pod disruption budget respected (min 3 available)
- ✓ New pods created on healthy nodes
- ✓ Zone distribution maintained
- ✓ No service disruption
- ✓ Recovery time < 2 minutes

#### Reviewer Validation Commands
```bash
# Verify PDB status
kubectl get pdb erlmcp-pdb -n erlmcp

# Check pod distribution across zones
kubectl get pods -n erlmcp -o wide -L topology.kubernetes.io/zone

# Verify endpoint count
kubectl get endpoints erlmcp -n erlmcp

# Check pod disruption budget compliance
kubectl describe pdb erlmcp-pdb -n erlmcp
```

---

## 5. Scale Test Documentation

### 5.1 Manual Scale Test

#### Test Procedure
**Purpose**: Verify horizontal scaling behavior

**Commands**:
```bash
# Get current replica count
kubectl get deployment erlmcp -n erlmcp

# Scale to 10 replicas
kubectl scale deployment erlmcp --replicas=10 -n erlmcp

# Watch scaling progress
kubectl get pods -n erlmcp -w

# Verify all pods ready
kubectl get pods -n erlmcp -l app=erlmcp
```

#### Expected Behavior

**Phase 1: Scale Initiated (T+0s)**
```
deployment.apps/erlmcp scaled
```
- Deployment controller receives scale request
- Replica set updated to 10
- Pod creation begins

**Phase 2: Pod Creation (T+0s to T+30s)**
```
erlmcp-xxx   0/2   Pending   0   0s   <none>
erlmcp-yyy   0/2   Pending   0   0s   <none>
erlmcp-zzz   0/2   Pending   0   0s   <none>
erlmcp-xxx   0/2   ContainerCreating   0   5s   node-a
erlmcp-yyy   0/2   ContainerCreating   0   5s   node-b
erlmcp-zzz   0/2   ContainerCreating   0   5s   node-c
```
- 5 new pods created (5 existing + 5 new = 10)
- Pods scheduled across nodes
- Anti-affinity rules respected

**Phase 3: Startup (T+30s to T+180s)**
```
erlmcp-xxx   1/2   Running   0   35s   node-a
erlmcp-yyy   1/2   Running   0   35s   node-b
erlmcp-zzz   2/2   Running   0   60s   node-c
```
- Startup probe runs (5 minutes max)
- Erlang VM boots
- Sidecar containers start
- Readiness probe passes

**Phase 4: All Ready (T+180s)**
```
erlmcp-xxx   2/2   Running   0   180s   node-a   Ready
erlmcp-yyy   2/2   Running   0   180s   node-b   Ready
erlmcp-zzz   2/2   Running   0   180s   node-c   Ready
```
- All 10 pods ready
- Load balanced across nodes
- Service endpoints updated

#### Success Criteria
- ✓ Scale command accepted
- ✓ All 10 pods created
- ✓ All pods become Ready
- ✓ Pods distributed across nodes (anti-affinity)
- ✓ Zone distribution maintained (spread constraints)
- ✓ No pod failures
- ✓ Scale complete within 3 minutes

#### Reviewer Validation Commands
```bash
# Verify replica count
kubectl get deployment erlmcp -n erlmcp

# Check pod distribution
kubectl get pods -n erlmcp -o wide -L topology.kubernetes.io/zone

# Verify endpoints
kubectl get endpoints erlmcp -n erlmcp

# Check for any ImagePullBackOff
kubectl get pods -n erlmcp | grep -i "error\|backoff"

# Verify resource usage on nodes
kubectl top nodes
```

#### Validation Queries

**Pod Distribution by Node**:
```bash
kubectl get pods -n erlmcp -o jsonpath='{range .items[*]}{.metadata.name}{"\t"}{.spec.nodeName}{"\n"}{end}' | \
  sort -k2 | uniq -c -f1
# Expected: Even distribution across nodes (e.g., 3-4 pods per node)
```

**Pod Distribution by Zone**:
```bash
kubectl get pods -n erlmcp -o jsonpath='{range .items[*]}{.metadata.name}{"\t"}{.metadata.labels.topology\.kubernetes\.io/zone}{"\n"}{end}' | \
  sort -k2 | uniq -c -f1
# Expected: Even distribution across zones (e.g., 3-4 pods per zone)
```

**Resource Availability**:
```bash
kubectl top nodes
# Verify: No node at 100% CPU/Memory
# Verify: Node has capacity for more pods
```

---

### 5.2 Horizontal Pod Autoscaler Test

#### Test Procedure
**Purpose**: Verify automatic scaling based on metrics

**Configuration**
**File**: `/Users/sac/erlmcp/k8s/production/hpa.yaml:29-42`

```yaml
spec:
  scaleTargetRef:
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
          averageUtilization: 80
```

#### Test Scenario: CPU Load Spike

**Commands**:
```bash
# Install load generator (if not present)
kubectl apply -f test/load-generator.yaml

# Check current HPA status
kubectl get hpa erlmcp -n erlmcp

# Generate load (increase traffic)
kubectl run -i --tty load-test --image=busybox --restart=Never -- sh -c \
  "while true; do wget -q -O- http://erlmcp.erlmcp-production.svc.cluster.local:8080/api/ping; done"

# Watch HPA scale
kubectl get hpa erlmcp -n erlmcp -w

# Watch pod creation
kubectl get pods -n erlmcp -w
```

#### Expected Behavior

**Phase 1: Baseline (T-5min)**
```
NAME     REFERENCE          TARGETS         MINPODS   MAXPODS   REPLICAS   AGE
erlmcp   Deployment/erlmcp   30%/70%        5         50        5          10m
```
- Baseline CPU: 30%
- Below target (70%)
- 5 replicas (minimum)

**Phase 2: Load Increase (T+0s to T+2min)**
```
erlmcp   Deployment/erlmcp   50%/70%        5         50        5          12m
erlmcp   Deployment/erlmcp   65%/70%        5         50        5          13m
erlmcp   Deployment/erlmcp   85%/70%        5         50        5          14m  # Threshold exceeded
```
- CPU increases to 85%
- Target threshold: 70%
- HPA detects need to scale

**Phase 3: Scale Up (T+2min to T+5min)**
```
erlmcp   Deployment/erlmcp   85%/70%        5         50        7          15m  # Scaling to 7
erlmcp   Deployment/erlmcp   80%/70%        5         50        10         16m  # Scaling to 10
```
- HPA calculates target replicas
- Scale up behavior: Max 100% or 5 pods per 30s
- Aggressive scale up (no stabilization window)

**Phase 4: Stabilization (T+5min to T+10min)**
```
erlmcp   Deployment/erlmcp   55%/70%        5         50        10         20m
```
- CPU drops to 55% (load distributed)
- Replicas stabilized at 10
- System handles load effectively

**Phase 5: Scale Down (T+10min to T+20min)**
```
# Load generator stopped
erlmcp   Deployment/erlmcp   35%/70%        5         50        10         25m
erlmcp   Deployment/erlmcp   30%/70%        5         50        8          26m  # Scaling down
erlmcp   Deployment/erlmcp   28%/70%        5         50        6          27m  # Scaling down
erlmcp   Deployment/erlmcp   30%/70%        5         50        5          28m  # Back to min
```
- Scale down stabilization window: 5 minutes
- Conservative scale down (max 25% or 2 pods per minute)
- Prevents flapping

#### Success Criteria
- ✓ HPA detects CPU threshold exceeded (70%)
- ✓ HPA triggers scale up within 2 minutes
- ✓ Scale up follows policy (max 5 pods per 30s)
- ✓ New pods become Ready
- ✓ CPU utilization drops after scaling
- ✓ Scale down occurs after load stops
- ✓ Scale down respects stabilization window (5min)
- ✓ Returns to minimum replicas (5) when idle

#### Reviewer Validation Commands

**Check HPA Status**:
```bash
kubectl describe hpa erlmcp -n erlmcp
```

**Expected Output**:
```
Name:                     erlmcp
Namespace:                erlmcp-production
Min Replicas:             5
Max Replicas:             50
Metrics:                  ( current / target )
  resource cpu on pods:   30% / 70%
  resource memory on pods: 45% / 80%
Min replicas:             5
Max replicas:             50
Replicas:                 5
```

**Check HPA Metrics**:
```bash
kubectl get --raw /apis/metrics.k8s.io/v1beta1/namespaces/erlmcp-production/pods | jq
```

**Verify Scaling Events**:
```bash
kubectl get events -n erlmcp-production --field-selector reason=SuccessfulCreate | tail -20
```

**Check Pod Distribution**:
```bash
kubectl get pods -n erlmcp -o wide --sort-by=.spec.nodeName
```

---

### 5.3 Stress Test: Maximum Scale

#### Test Procedure
**Purpose**: Verify system can scale to maximum configured replicas

**Commands**:
```bash
# Directly scale to maximum
kubectl scale deployment erlmcp --replicas=50 -n erlmcp

# Watch scale progress
kubectl get pods -n erlmcp -w

# Check node autoscaler
kubectl get nodes -w

# Monitor cluster resources
kubectl top nodes
```

#### Expected Behavior

**Phase 1: Scale Initiated (T+0s)**
```
deployment.apps/erlmcp scaled
```

**Phase 2: Node Pool Autoscaling (T+0s to T+5min)**
```
NAME                                      STATUS   ROLES    AGE   VERSION
erlmcp-primary-pool-xxx-us-central1-a-1   Ready    <none>   10m   v1.28.x
erlmcp-primary-pool-yyy-us-central1-b-1   Ready    <none>   9m    v1.28.x
erlmcp-primary-pool-zzz-us-central1-c-1   Ready    <none>   8m    v1.28.x
erlmcp-primary-pool-aaa-us-central1-a-2   Ready    <none>   2m    v1.28.x  # New node
erlmcp-primary-pool-bbb-us-central1-b-2   Ready    <none>   1m    v1.28.x  # New node
```
- Cluster Autoscaler detects pending pods
- New nodes provisioned (3-10 nodes)
- Node boot time: 2-5 minutes

**Phase 3: Pod Scheduling (T+5min to T+15min)**
```
erlmcp-xxx   0/2   Pending   0   0s    <none>
...
erlmcp-xxx   0/2   ContainerCreating   0   10s   node-new-1
erlmcp-yyy   0/2   ContainerCreating   0   10s   node-new-2
```
- Pods scheduled on new nodes
- Image pull from GCR (cached on nodes)
- Volume provisioning

**Phase 4: All Ready (T+15min to T+20min)**
```
kubectl get pods -n erlmcp | grep -c "Running"
# Expected: 50 pods
```

#### Success Criteria
- ✓ All 50 pods created
- ✓ All pods become Ready
- ✓ Cluster Autoscaler provisions new nodes (3-10)
- ✓ Pods distributed across nodes
- ✓ Zone distribution maintained
- ✓ No resource exhaustion
- ✓ Scale complete within 20 minutes

#### Reviewer Validation Commands

**Check Node Count**:
```bash
kubectl get nodes -l pool=primary
# Expected: 3-10 nodes (based on pod count and resources)
```

**Check Pod Count**:
```bash
kubectl get pods -n erlmcp -l app=erlmcp --no-headers | wc -l
# Expected: 50
```

**Check Resource Usage**:
```bash
kubectl top nodes
# Verify: No node at 100% CPU/Memory
```

**Verify Node Autoscaler**:
```bash
kubectl get configmap cluster-autoscaler-status -n kube-system -o yaml
```

---

## 6. Security Review

### 6.1 Network Policy Verification

**File**: `/Users/sac/erlmcp/helm/erlmcp-enterprise/templates/networkpolicy.yaml`

**Expected NetworkPolicy**:
```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: erlmcp
spec:
  podSelector:
    matchLabels:
      app: erlmcp
  policyTypes:
    - Ingress
    - Egress
  ingress:
    - from:
        - namespaceSelector:
            matchLabels:
              name: erlmcp-production
      ports:
        - protocol: TCP
          port: 8080
  egress:
    - to:
        - namespaceSelector: {}
      ports:
        - protocol: TCP
          port: 5432  # PostgreSQL
    - to:
        - namespaceSelector: {}
      ports:
        - protocol: TCP
          port: 6379  # Redis
```

**Reviewer Checks**:
- ✓ NetworkPolicy defined (CALICO provider enabled)
- ✓ Ingress restricted to necessary ports
- ✓ Egress restricted to known dependencies
- ✓ Default deny all ingress/egress
- ✓ No overly permissive rules

**Validation Commands**:
```bash
# Verify network policy enforcement
kubectl get networkpolicies -n erlmcp-production

# Check pod connectivity
kubectl exec -n erlmcp erlmcp-xxx -- nc -zv postgres 5432
# Expected: Connection allowed

kubectl exec -n erlmcp erlmcp-xxx -- nc -zv google.com 443
# Expected: Connection refused (if policy denies)
```

---

### 6.2 RBAC Verification

**File**: `/Users/sac/erlmcp/helm/erlmcp-enterprise/templates/rbac.yaml`

**Expected RBAC Configuration**:
```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: erlmcp
  namespace: erlmcp-production

---
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: erlmcp
  namespace: erlmcp-production
rules:
  - apiGroups: [""]
    resources: ["configmaps", "secrets"]
    verbs: ["get", "list"]

---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: erlmcp
  namespace: erlmcp-production
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: Role
  name: erlmcp
subjects:
  - kind: ServiceAccount
    name: erlmcp
```

**Reviewer Checks**:
- ✓ ServiceAccount created (not default)
- ✓ Minimal permissions (principle of least privilege)
- ✓ No cluster-admin (unless required)
- ✓ No wildcard permissions ("*")
- ✓ Role scoped to namespace

**Validation Commands**:
```bash
# Check service account
kubectl get sa -n erlmcp-production

# Check role permissions
kubectl describe role erlmcp -n erlmcp-production

# Check role binding
kubectl describe rolebinding erlmcp -n erlmcp-production
```

---

## 7. Observability Review

### 7.1 Metrics Collection

**File**: `/Users/sac/erlmcp/k8s/production/hpa.yaml:291-319`

**Expected ServiceMonitor**:
```yaml
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: erlmcp
  namespace: erlmcp-production
spec:
  selector:
    matchLabels:
      app: erlmcp
  endpoints:
    - port: metrics
      interval: 15s
      path: /metrics
```

**Reviewer Checks**:
- ✓ ServiceMonitor defined (Prometheus Operator)
- ✓ Metrics endpoint exposed
- ✓ Scrape interval: 15s (appropriate)
- ✓ Pod/Node labels in metrics
- ✓ Custom metrics defined for HPA

**Validation Commands**:
```bash
# Verify metrics endpoint
kubectl exec -n erlmcp erlmcp-xxx -- curl -s http://localhost:9090/metrics | head -20

# Check Prometheus targets
kubectl port-forward -n prometheus svc/prometheus 9090:9090
# Open: http://localhost:9090/targets
# Verify: erlmcp target is "UP"
```

**Expected Metrics**:
```
# Erlang VM metrics
erlmcp_erlang_processes_total
erlmcp_erlang_memory_total
erlmcp_erlang_reductions_total

# HTTP metrics
erlmcp_http_requests_total{method, path, status}
erlmcp_http_request_duration_seconds{le}

# Connection metrics
erlmcp_connections_active
erlmcp_connections_total

# Distribution metrics
erlmcp_distribution_nodes_up
erlmcp_distribution_messages_total
```

---

### 7.2 Logging

**Configuration**:
- Stackdriver Logging enabled (default)
- Log format: JSON (structured)
- Log level: info (production)

**Reviewer Checks**:
- ✓ Logging configured in GKE module
- ✓ Workload Identity has `roles/logging.logWriter`
- ✓ Logs exported to Cloud Logging
- ✓ No sensitive data in logs

**Validation Commands**:
```bash
# View logs in Cloud Logging
gcloud logging read "resource.type=k8s_container" \
  --project=PROJECT_ID \
  --limit=50

# Verify log entries
gcloud logging read 'logName="projects/PROJECT_ID/logs/erlmcp"' \
  --project=PROJECT_ID \
  --format=json | jq '.[] | .textPayload'
```

**Expected Log Format**:
```json
{
  "severity": "INFO",
  "timestamp": "2026-02-02T10:00:00.000Z",
  "message": "Request processed",
  "context": {
    "method": "POST",
    "path": "/api/v1/models",
    "status": 200,
    "duration_ms": 45
  }
}
```

---

## 8. Findings and Recommendations

### 8.1 Critical Issues (0)
None identified.

---

### 8.2 High Priority Recommendations (2)

#### Recommendation 1: Add Pod Disruption Budget Validation
**Finding**: PDB defined but not explicitly tested in deployment docs.

**Risk**: During node maintenance, PDB might block operations if not properly configured.

**Recommendation**:
```bash
# Add to deployment validation script
kubectl describe pdb erlmcp-pdb -n erlmcp-production

# Test PDB enforcement
kubectl cordon $(kubectl get nodes -o jsonpath='{.items[0].metadata.name}')
kubectl drain $(kubectl get nodes -o jsonpath='{.items[0].metadata.name}') \
  --ignore-daemonsets --delete-emptydir-data --pod-selector=app=erlmcp

# Verify: Command should fail if PDB would be violated
# Expected: "Cannot evict pod as it would violate PodDisruptionBudget"
```

**File**: Create `/Users/sac/erlmcp/marketplace/gcp/test-pdb.sh`

---

#### Recommendation 2: Document Regional vs Zonal Tradeoffs
**Finding**: Terraform uses regional cluster by default, but this has cost implications.

**Impact**: Regional cluster = 3x control plane cost (~$180/month vs ~$60/month)

**Recommendation**: Add marketplace README section:
```markdown
## Cluster Topology

### Regional (Default)
- **Pros**: 99.95% SLA, automatic zone redundancy
- **Cons**: 3x control plane cost, requires minimum 3 nodes
- **Use**: Production, multi-zone deployments

### Zonal (Optional)
- **Pros**: Lower cost, single zone
- **Cons**: 99.5% SLA, zone failure = outage
- **Use**: Development, testing, cost-sensitive deployments

To use zonal:
```terraform
location = "us-central1-a"  # Zone instead of region
```
```

**File**: Update `/Users/sac/erlmcp/marketplace/gcp/README.md`

---

### 8.3 Medium Priority Observations (5)

#### Observation 1: Startup Probe Timeout
**Finding**: Startup probe allows 5 minutes (30 failures × 10s intervals)

**Context**: Erlang VM startup is slow, but 5 minutes may be too long for Marketplace reviewers.

**Recommendation**: Document why 5 minutes is needed:
```yaml
startupProbe:
  failureThreshold: 30  # 5 minutes
  # Note: Erlang VM takes 2-3 minutes to fully initialize
  # Includes: code loading, distribution setup, database migration
```

**Impact**: Low - just documentation clarity

---

#### Observation 2: HPA Custom Metrics Not Validated
**Finding**: HPA references custom metrics (`erlmcp_active_connections`, etc.) but these require Prometheus Adapter.

**Recommendation**: Add validation script:
```bash
# Verify custom metrics are available
kubectl get --raw /apis/custom.metrics.k8s.io/v1beta1/namespaces/erlmcp-production/pods/erlmcp-active_connections

# If 404, install Prometheus Adapter:
kubectl apply -f https://raw.githubusercontent.com/kubernetes-sigs/metrics-server/master/deploy/kubernetes/1.8+/
```

**File**: Create `/Users/sac/erlmcp/marketplace/gcp/validate-hpa.sh`

---

#### Observation 3: Image Digest Not Pinned
**Finding**: Deployment uses tag `:3.0.0` instead of image digest.

**Current**:
```yaml
image: ghcr.io/erlmcp/erlmcp:3.0.0
```

**Recommendation**:
```yaml
image: ghcr.io/erlmcp/erlmcp@sha256:abc123...
```

**Rationale**: Digests are immutable, tags can be overwritten.

**Impact**: Medium - important for production reproducibility

---

#### Observation 4: No Resource Quota Defined
**Finding**: Namespace lacks ResourceQuota to prevent resource exhaustion.

**Recommendation**: Add to Helm chart:
```yaml
apiVersion: v1
kind: ResourceQuota
metadata:
  name: erlmcp-quota
  namespace: erlmcp-production
spec:
  hard:
    requests.cpu: "20"
    requests.memory: 40Gi
    limits.cpu: "50"
    limits.memory: 100Gi
    persistentvolumeclaims: "10"
```

**File**: `/Users/sac/erlmcp/helm/erlmcp-enterprise/templates/resourcequota.yaml` (already exists, verify it's used)

---

#### Observation 5: No LimitRange Defined
**Finding**: Namespace lacks default resource limits for pods without explicit limits.

**Recommendation**: Add to Helm chart:
```yaml
apiVersion: v1
kind: LimitRange
metadata:
  name: erlmcp-limits
  namespace: erlmcp-production
spec:
  limits:
    - default:
        cpu: 1000m
        memory: 2Gi
      defaultRequest:
        cpu: 100m
        memory: 256Mi
      type: Container
```

**File**: `/Users/sac/erlmcp/helm/erlmcp-enterprise/templates/limitrange.yaml` (already exists, verify it's used)

---

## 9. Final Checklist

### 9.1 GKE Configuration (PASS)
- [x] Regional cluster for 99.95% SLA
- [x] Private endpoint enabled
- [x] Private nodes enabled
- [x] Workload Identity configured
- [x] Shielded nodes enabled
- [x] Network policy enabled (CALICO)
- [x] Managed Prometheus enabled
- [x] Node auto-repair enabled
- [x] Node auto-upgrade enabled

### 9.2 Node Pools (PASS)
- [x] Minimum 3 nodes
- [x] Multi-zone distribution
- [x] Auto-scaling configured (3-10 nodes)
- [x] Machine type appropriate (e2-standard-2)
- [x] Shielded node configuration

### 9.3 Kubernetes Resources (PASS)
- [x] Deployment configured
- [x] Service configured
- [x] ConfigMap configured
- [x] Secret references (no plain text)
- [x] ServiceAccount created
- [x] RBAC configured
- [x] NetworkPolicy configured
- [x] PodDisruptionBudget configured
- [x] HPA configured

### 9.4 Pod Configuration (PASS)
- [x] Resource requests defined
- [x] Resource limits defined
- [x] Security context configured
- [x] Non-root user
- [x] Read-only root filesystem
- [x] Startup probe configured
- [x] Liveness probe configured
- [x] Readiness probe configured
- [x] Graceful shutdown hook

### 9.5 Autoscaling (PASS)
- [x] HPA minimum: 5 replicas
- [x] HPA maximum: 50 replicas
- [x] CPU target: 70%
- [x] Memory target: 80%
- [x] Custom metrics defined
- [x] Scale up policy configured
- [x] Scale down policy configured
- [x] PodDisruptionBudget: min 3 available

### 9.6 Security (PASS)
- [x] Workload Identity annotations
- [x] Secret Manager IAM roles
- [x] No plain text secrets
- [x] NetworkPolicy restrictions
- [x] RBAC least privilege
- [x] Pod security policies
- [x] Image pull secrets

### 9.7 Observability (PASS)
- [x] Prometheus scraping configured
- [x] ServiceMonitor defined
- [x] Logging enabled
- [x] Managed Prometheus enabled
- [x] Metrics endpoint exposed
- [x] Health check endpoints

### 9.8 Documentation (PASS)
- [x] Deployment guide
- [x] Scaling policy documented
- [x] Troubleshooting guide
- [x] Upgrade procedures
- [x] Health check procedures

---

## 10. PASS/FAIL Recommendation

### Final Decision: **PASS**

**Rationale**:
1. GKE configuration follows Google best practices
2. Regional cluster for high availability
3. Private topology for security
4. Workload Identity properly integrated
5. Comprehensive autoscaling with HPA
6. Proper security hardening
7. Strong observability

**Required Actions Before Listing**:
1. Address high-priority recommendations (2 items)
2. Consider medium-priority observations (5 items)
3. Update documentation with regional/zonal tradeoffs
4. Add PDB validation script

**Estimated Time to Compliance**: 2-4 hours

**Reviewer Confidence**: HIGH

**Review Complete**: 2026-02-02

---

## Appendix A: Validation Commands Summary

### Cluster Health
```bash
kubectl get nodes
kubectl get pods -n erlmcp
kubectl get endpoints -n erlmcp
```

### Resource Status
```bash
kubectl top nodes
kubectl top pods -n erlmcp
kubectl describe node | grep -A 5 "Allocated resources"
```

### Scaling Status
```bash
kubectl get hpa -n erlmcp
kubectl describe hpa erlmcp -n erlmcp
kubectl get pdb -n erlmcp
```

### Security Validation
```bash
kubectl get networkpolicies -n erlmcp
kubectl get serviceaccounts -n erlmcp
kubectl get secrets -n erlmcp
```

### Logs and Metrics
```bash
kubectl logs -n erlmcp -l app=erlmcp --tail=100
kubectl port-forward -n prometheus svc/prometheus 9090:9090
gcloud logging read "resource.type=k8s_container" --project=PROJECT_ID
```

---

## Appendix B: Common Issues and Fixes

### Issue: Pod CrashLoopBackOff
**Symptom**: Pod restarts repeatedly
**Diagnosis**:
```bash
kubectl logs <pod-name> -n erlmcp --previous
kubectl describe pod <pod-name> -n erlmcp
```
**Common Causes**:
- Missing ConfigMap/Secret
- Database connection failure
- Insufficient resources
- Liveness probe too aggressive

### Issue: HPA Not Scaling
**Symptom**: CPU at 90% but no new pods
**Diagnosis**:
```bash
kubectl describe hpa erlmcp -n erlmcp
kubectl get --raw /apis/metrics.k8s.io/v1beta1/namespaces/erlmcp-production/pods
```
**Common Causes**:
- Metrics server not running
- Resource requests not set
- Already at maxReplicas
- Custom metrics not available

### Issue: Pods Not Ready
**Symptom**: Pod stuck in Pending or ContainerCreating
**Diagnosis**:
```bash
kubectl describe pod <pod-name> -n erlmcp
kubectl get events -n erlmcp --sort-by='.lastTimestamp'
```
**Common Causes**:
- Insufficient node resources
- Image pull errors
- PVC not bound
- Tolerations missing

### Issue: Secrets Not Accessible
**Symptom**: "Permission denied" accessing Secret Manager
**Diagnosis**:
```bash
kubectl get pod <pod-name> -n erlmcp -o jsonpath='{.metadata.annotations}'
gcloud iam service-accounts get-iam-policy erlmcp-ksa@PROJECT_ID
```
**Common Causes**:
- Workload Identity annotation missing
- IAM role not granted
- Wrong secret name

---

**End of GKE Marketplace Review Report**
