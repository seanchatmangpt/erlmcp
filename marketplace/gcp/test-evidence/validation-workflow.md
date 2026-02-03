# GCP Marketplace Validation Workflow

**Version:** 1.0.0
**Last Updated:** 2026-02-02
**Purpose:** End-to-end validation workflow for Google Cloud Marketplace submission

---

## Executive Summary

This document defines the complete validation pipeline for erlmcp's Google Cloud Marketplace submission. The workflow is designed to ensure that deployments meet Google's Marketplace requirements and pass the reviewer validation process.

### Core Questions Answered

1. **Does it deploy without human intervention?**
2. **Does it run correctly under failure and scaling?**
3. **Can a customer operate, observe, and secure it without you?**

---

## Workflow Overview

```
Stage 0: Static Validation (no cloud resources)
    |
    v
Stage 1: Artifact Validation (images, scanning)
    |
    v
Stage 2: Deployment Validation (all 3 paths)
    |
    v
Stage 3: Failure Scenario Testing
    |
    v
Stage 4: Observability Validation
    |
    v
Stage 5: Security Validation
    |
    v
Stage 6: Operations Validation
    |
    v
Stage 7: Marketplace Package Validation
```

### Execution Strategy

| Stage | Parallel | Sequential | Duration | Blocking |
|-------|----------|------------|----------|----------|
| Stage 0 | Yes | No | 30 min | Yes |
| Stage 1 | Partial | No | 45 min | Yes (scan) |
| Stage 2 | Yes | Yes | 2 hours | No |
| Stage 3 | Partial | Yes | 1 hour | No |
| Stage 4 | Yes | No | 30 min | No |
| Stage 5 | Yes | No | 30 min | No |
| Stage 6 | Yes | No | 45 min | No |
| Stage 7 | Yes | No | 15 min | Yes |

---

## Stage 0: Static Validation (No Cloud Resources)

**Purpose:** Validate all configuration files without creating any cloud resources.
**Duration:** ~30 minutes
**Blocking:** YES - Must pass before any cloud operations

### 0.1 Terraform Correctness Validation

**Script:** `/Users/sac/erlmcp/marketplace/gcp/scripts/validate-terraform.sh`

**Checks:**
- All Terraform modules validate successfully (`terraform validate`)
- No `null_resource` or `local-exec` usage (manual intervention detection)
- Terraform format consistency (`terraform fmt -check`)
- Terraform state configuration verified
- No hardcoded secrets in configuration

**Pass Criteria:**
```
- Zero errors
- Zero warnings in Marketplace modules
- No use of null_resource or local-exec
- Terraform state configuration verified
```

**Evidence Artifacts:**
- `test-evidence/0.1-terraform.log` - Full validation output

**Run Command:**
```bash
cd /Users/sac/erlmcp/marketplace/gcp
bash scripts/validate-terraform.sh
```

**Gate Definition:**
- **BLOCKING:** Must pass with zero errors
- **ROLLBACK:** N/A (no resources created)
- **RECOVERY:** Fix Terraform configuration and re-run

### 0.2 Marketplace Schema Validation

**Script:** `/Users/sac/erlmcp/marketplace/gcp/scripts/validate-schema.sh`

**Checks:**
- `application.yaml` has all required fields
- `schema.yaml` defines all input parameters
- `parameters.yaml` maps inputs to Terraform variables
- Default values exist for all non-required parameters
- Parameter types match Terraform variable types
- No hidden/undocumented parameters

**Pass Criteria:**
```
- All required fields present in application.yaml
- All Terraform variables in schema.yaml have corresponding definitions
- Default values exist for all non-required parameters
- Parameter types match Terraform variable types
```

**Evidence Artifacts:**
- `test-evidence/0.2-schema.log` - Schema validation output
- `test-evidence/schema/schema-validation-report.md` - Detailed report

**Run Command:**
```bash
cd /Users/sac/erlmcp/marketplace/gcp
bash scripts/validate-schema.sh
```

**Gate Definition:**
- **BLOCKING:** Must pass with all required fields
- **ROLLBACK:** N/A
- **RECOVERY:** Fix schema definitions and re-run

### 0.3 Helm Chart Validation

**Checks:**
- All templates render without errors
- No undefined variables
- GCP-specific annotations are valid
- Workload Identity annotations present

**Pass Criteria:**
```
- All templates render successfully
- No undefined variable errors
- Valid GCP annotations
```

**Evidence Artifacts:**
- `test-evidence/0.3-helm.log` - Helm lint output

**Run Command:**
```bash
cd /Users/sac/erlmcp/marketplace/gcp
helm lint helm/erlmcp-marketplace/
```

**Gate Definition:**
- **BLOCKING:** No (may skip if Helm not used)
- **ROLLBACK:** N/A
- **RECOVERY:** Fix Helm templates and re-run

---

## Stage 1: Artifact Validation (Images & Containers)

**Purpose:** Validate container images and VM images.
**Duration:** ~45 minutes
**Blocking:** YES - Security scan must pass

### 1.1 Container Image Test

**Script:** `/Users/sac/erlmcp/marketplace/gcp/scripts/test-container.sh`

**Checks:**
- Image exists in Artifact Registry
- Image pulls successfully
- Container starts within 10 seconds
- Health endpoint returns 200
- Metrics endpoint accessible
- Container logs to stdout
- Container handles SIGTERM cleanly

**Pass Criteria:**
```
- Image pulls successfully
- Process starts within 10 seconds
- Binds to port 8080
- Logs to stdout
- Handles SIGTERM cleanly (exit code 0)
```

**Evidence Artifacts:**
- `test-evidence/1.1-container.log` - Container test output
- `test-evidence/container-logs-full.txt` - Full container logs
- `test-evidence/health-response.json` - Health check response

**Run Command:**
```bash
export PROJECT_ID=your-project
export REGION=us-central1
cd /Users/sac/erlmcp/marketplace/gcp
bash scripts/test-container.sh --project $PROJECT_ID --region $REGION
```

**Gate Definition:**
- **BLOCKING:** Yes - Container must be functional
- **ROLLBACK:** N/A
- **RECOVERY:** Fix container image and re-push

### 1.2 Security Vulnerability Scan (CRITICAL)

**Script:** `/Users/sac/erlmcp/marketplace/gcp/scripts/scan-image.sh`

**Checks:**
- Trivy scan for vulnerabilities
- GCP Container Analysis scan
- Compliance check (known CVEs)
- Base image validation

**Pass Criteria:**
```
- Zero HIGH severity vulnerabilities (strict mode)
- Zero CRITICAL severity vulnerabilities
- Scan results saved
- Image digest recorded
```

**Evidence Artifacts:**
- `test-evidence/1.2-scan.log` - Scan output
- `test-evidence/trivy-report.json` - Trivy scan results
- `test-evidence/gcp-scan.json` - GCP scan results
- `test-evidence/scan-summary.md` - Human-readable summary
- `test-evidence/vuln-high.txt` - HIGH count
- `test-evidence/vuln-critical.txt` - CRITICAL count

**Run Command:**
```bash
export PROJECT_ID=your-project
export REGION=us-central1
cd /Users/sac/erlmcp/marketplace/gcp
bash scripts/scan-image.sh --project $PROJECT_ID --region $REGION --mode strict
```

**Gate Definition:**
- **BLOCKING:** YES - Zero HIGH/CRITICAL vulnerabilities required
- **ROLLBACK:** N/A
- **RECOVERY:** Update base image, rebuild, and re-scan

**Scan Modes:**
- `strict`: Fails on any HIGH or CRITICAL vulnerabilities
- `relaxed`: Fails only on CRITICAL vulnerabilities
- `audit`: Never fails, reports all findings

### 1.3 VM Image Test (Packer)

**Script:** `/Users/sac/erlmcp/marketplace/gcp/scripts/test-vm-image.sh`

**Checks:**
- Packer template validates
- Image builds without manual SSH
- Cloud Ops Agent installs cleanly
- Systemd service enabled
- Logs visible in Cloud Logging

**Pass Criteria:**
```
- Image builds without manual SSH
- Cloud Ops Agent installs cleanly
- Systemd service enabled but not failing
- Logs visible in Cloud Logging
```

**Evidence Artifacts:**
- `test-evidence/packer-build.log` - Build output
- `test-evidence/packer-validate.log` - Validation output
- `test-evidence/serial-output.log` - VM serial port

**Run Command:**
```bash
export PROJECT_ID=your-project
cd /Users/sac/erlmcp/marketplace/gcp
bash scripts/test-vm-image.sh --project $PROJECT_ID
```

**Gate Definition:**
- **BLOCKING:** No (may skip if not using VM images)
- **ROLLBACK:** Delete image from Compute
- **RECOVERY:** Fix Packer template and rebuild

---

## Stage 2: Deployment Validation (All 3 Paths)

**Purpose:** Validate actual deployment for each supported platform.
**Duration:** ~2 hours (can run in parallel)
**Blocking:** NO - But all paths must eventually pass

### 2.1 Cloud Run Deployment Test

**Script:** `/Users/sac/erlmcp/marketplace/gcp/scripts/test-cloudrun.sh`

**Checks:**
- Service deploys in < 5 minutes (Marketplace SLA)
- Health endpoint accessible
- Cold start time < 30 seconds
- Automatic scaling works
- Service URL accessible
- IAM configuration correct
- Logs ingested to Cloud Logging

**Pass Criteria:**
```
- Service deploys in < 5 minutes
- Health endpoint returns 200
- Cold start < 30 seconds
- Automatic scaling configured
- Service URL accessible
```

**Evidence Artifacts:**
- `test-evidence/2.1-cloudrun/terraform-apply.log` - Deployment log
- `test-evidence/2.1-cloudrun/service-url.txt` - Service URL
- `test-evidence/2.1-cloudrun/health-response.txt` - Health check
- `test-evidence/2.1-cloudrun/cold-start-response.txt` - Cold start timing
- `test-evidence/2.1-cloudrun/service-describe.json` - Service details
- `test-evidence/2.1-cloudrun/terraform-destroy.log` - Cleanup log

**Run Command:**
```bash
export PROJECT_ID=your-project
export REGION=us-central1
cd /Users/sac/erlmcp/marketplace/gcp
bash scripts/test-cloudrun.sh --project $PROJECT_ID --region $REGION
```

**Gate Definition:**
- **BLOCKING:** No (but should pass for Marketplace approval)
- **ROLLBACK:** Run `terraform destroy` in deployment directory
- **RECOVERY:** Fix Terraform configuration and re-deploy

### 2.2 GKE Deployment Test (Most Scrutinized)

**Script:** `/Users/sac/erlmcp/marketplace/gcp/scripts/test-gke.sh`

**Checks:**
- Regional cluster spans 3 zones
- All nodes are Ready
- Workload Identity bindings exist
- Network policies created
- Private cluster configured (if applicable)
- Shielded nodes enabled
- Cluster autoscaling configured
- Cloud logging/monitoring integration

**Pass Criteria:**
```
- Regional cluster spans 3 zones
- All nodes are Ready
- Workload Identity bindings exist
- Network policies created
- Shielded nodes enabled
- Cluster autoscaling configured
```

**Evidence Artifacts:**
- `test-evidence/2.2-gke/terraform-apply.log` - Deployment log
- `test-evidence/2.2-gke/nodes.log` - Node status
- `test-evidence/2.2-gke/network-policies.log` - Network policies
- `test-evidence/2.2-gke/service-describe.json` - Service details
- `test-evidence/2.2-gke/terraform-outputs.json` - Terraform outputs
- `test-evidence/2.2-gke/terraform-destroy.log` - Cleanup log

**Run Command:**
```bash
export PROJECT_ID=your-project
export REGION=us-central1
cd /Users/sac/erlmcp/marketplace/gcp
bash scripts/test-gke.sh --project $PROJECT_ID --region $REGION
```

**Gate Definition:**
- **BLOCKING:** No (but critical for Marketplace approval)
- **ROLLBACK:** Run `terraform destroy` in deployment directory
- **RECOVERY:** Fix Terraform configuration and re-deploy

### 2.3 Compute Engine Deployment Test

**Script:** `/Users/sac/erlmcp/marketplace/gcp/scripts/test-gce.sh`

**Checks:**
- VM boots successfully
- External IP assigned
- Health endpoint accessible
- Cloud Ops Agent running
- Startup script executed
- Service status active
- Logs ingested to Cloud Logging
- Firewall rules configured

**Pass Criteria:**
```
- VM boots successfully
- External IP assigned
- Health endpoint returns 200 (within 2 minutes)
- Cloud Ops Agent running
- Service status active
```

**Evidence Artifacts:**
- `test-evidence/2.3-gce/terraform-apply.log` - Deployment log
- `test-evidence/2.3-gce/instance-details.json` - Instance details
- `test-evidence/2.3-gce/serial-output.txt` - Serial port output
- `test-evidence/2.3-gce/health-response.txt` - Health check
- `test-evidence/2.3-gce/external-ip.txt` - External IP
- `test-evidence/2.3-gce/terraform-destroy.log` - Cleanup log

**Run Command:**
```bash
export PROJECT_ID=your-project
export ZONE=us-central1-a
cd /Users/sac/erlmcp/marketplace/gcp
bash scripts/test-gce.sh --project $PROJECT_ID --zone $ZONE
```

**Gate Definition:**
- **BLOCKING:** No (optional deployment path)
- **ROLLBACK:** Run `terraform destroy` in deployment directory
- **RECOVERY:** Fix Terraform configuration and re-deploy

---

## Stage 3: Failure Scenario Testing

**Purpose:** Validate system behavior under failure conditions.
**Duration:** ~1 hour
**Blocking:** NO

### 3.1 Pod Failure Recovery

**Script:** Create `test-failure-pod.sh`

**Checks:**
- New pod starts within 2 minutes
- Health check passes on new pod
- No data loss

**Test Procedure:**
```bash
# Deploy GKE cluster
cd /Users/sac/erlmcp/marketplace/gcp/terraform/examples/gke-deployment
terraform apply -auto-apply
gcloud container clusters get-credentials "$CLUSTER_NAME" --region="$REGION"

# Kill a pod
kubectl delete pod -n erlmcp deployment/erlmcp-xxx

# Verify replacement pod starts
kubectl rollout status deployment/erlmcp -n erlmcp
sleep 30
kubectl get pods -n erlmcp

# Verify health still passes
kubectl exec -n erlmcp deployment/erlmcp-xxx -- curl -f http://localhost:8080/health
```

**Pass Criteria:**
```
- New pod starts within 2 minutes
- Health check passes on new pod
- No data loss
```

**Evidence Artifacts:**
- `test-evidence/3.1-pod-failure.log` - Test output

### 3.2 Zone Failure Simulation

**Script:** Create `test-failure-zone.sh`

**Checks:**
- Pods rescheduled to remaining zones
- Service remains accessible
- No downtime > 30 seconds

**Test Procedure:**
```bash
# Create regional cluster spanning 3 zones (via terraform)

# Simulate zone failure by draining all nodes in one zone
ZONE="us-central1-a"
kubectl cordon $ZONE
kubectl drain --ignore-daemonsets --force=true --grace-period=30 --delete-emptydir-data $ZONE

# Verify pods rescheduled to other zones
kubectl get pods -n erlmcp -o wide

# Verify cluster still functional
kubectl get svc -n erlmcp

# Restore nodes
kubectl uncordon $ZONE
```

**Pass Criteria:**
```
- Pods rescheduled to remaining zones
- Service remains accessible
- No downtime > 30 seconds
```

**Evidence Artifacts:**
- `test-evidence/3.2-zone-failure.log` - Test output

### 3.3 Secret Rotation

**Script:** Create `test-failure-secret.sh`

**Checks:**
- Service remains available during rotation
- All pods use new secret after rotation
- No authentication failures

**Test Procedure:**
```bash
# Add new secret version
gcloud secrets versions add erlmcp-erlang-cookie \
  --data-file=new-cookie.txt --project=$PROJECT_ID

# Trigger rotation (via restart or signal)
kubectl rollout restart deployment/erlmcp -n erlmcp

# Verify service continues using old secret until all pods use new secret
kubectl get pods -n erlmcp -o jsonpath='{.items[*].spec.containers[*].env[?(@.name=="ERLANG_COOKIE_UPDATE"]}'

# Verify new secret is accessible
gcloud secrets versions access latest \
  erlmcp-erlang-cookie --project=$PROJECT_ID
```

**Pass Criteria:**
```
- Service remains available during rotation
- All pods eventually use new secret
- No authentication failures
```

**Evidence Artifacts:**
- `test-evidence/3.3-secret-rotation.log` - Test output

---

## Stage 4: Observability Validation

**Purpose:** Validate monitoring, logging, and alerting.
**Duration:** ~30 minutes
**Blocking:** NO

### 4.1 Metrics Export Test

**Checks:**
- Custom metrics appear in Cloud Monitoring
- Metric data is not empty
- Dashboard is accessible

**Test Procedure:**
```bash
# Deploy any deployment
cd /Users/sac/erlmcp/marketplace/gcp/terraform/examples/gke-deployment
terraform apply -auto-apply
gcloud container clusters get-credentials "$CLUSTER_NAME" --region="$REGION"

# Generate traffic
SERVICE_URL=$(terraform output -raw service_url)
for i in {1..100}; do
  curl -f http://$SERVICE_URL/api/test || true
done

# Wait for metrics ingestion
sleep 60

# Query custom metrics
gcloud monitoring metrics describe \
  custom.googleapis.com/erlmcp/http/latency \
  --project=$PROJECT_ID

# Verify metric data exists
gcloud monitoring time-series list \
  --metrics='custom.googleapis.com/erlmcp/*' \
  --project=$PROJECT_ID \
  --aggregation-alignment=300 \
  --format=json | jq '.timeSeries[0].points[0].value'
```

**Pass Criteria:**
```
- Custom metrics appear in Cloud Monitoring
- Metric data is not empty
- Dashboard is accessible
```

**Evidence Artifacts:**
- `test-evidence/4.1-metrics.log` - Metrics query output
- Screenshot of Cloud Monitoring dashboard

### 4.2 Alert Policy Test

**Checks:**
- Alert fires when threshold exceeded
- Notification channel receives alert
- Alert appears in Console

**Test Procedure:**
```bash
# Deploy with low limits (triggers alert)
cd /Users/sac/erlmcp/marketplace/gcp/terraform/examples/gke-deployment
terraform apply -auto-apply -var="memory_limit=256Mi"

# Wait for memory alert (or manually trigger)
gcloud alpha monitoring policies list --project=$PROJECT_ID

# Verify alert fired
gcloud logging logs "resource.labels.labels.resource_type:\"erlmcp\"" \
  --freshness=1d --limit=50 | grep "High memory"

# Cleanup
terraform destroy -auto-approve
```

**Pass Criteria:**
```
- Alert fires when threshold exceeded
- Notification channel receives alert
- Alert appears in Console
```

**Evidence Artifacts:**
- `test-evidence/4.2-alerts.log` - Alert test output
- Screenshot of alert notification

### 4.3 Logging Verification

**Checks:**
- Logs appear within 30 seconds
- Severity levels are correct
- Log entries have required fields

**Test Procedure:**
```bash
# Generate structured logs
curl -X POST http://$SERVICE_URL/api/test \
  -H "Content-Type: application/json" \
  -d '{"message": "test"}'

# Query logs
gcloud logging read "resource.labels.resource_name=\"erlmcp\"" \
  --limit=10 --format=json | jq -r '.entries[].logName'

# Verify log structure
gcloud logging read "resource.labels.resource_name=\"erlmcp\"" \
  --limit=10 --format=json | jq -r '.entries[].severity'
```

**Pass Criteria:**
```
- Logs appear within 30 seconds
- Severity levels are correct
- Log entries have required fields
```

**Evidence Artifacts:**
- `test-evidence/4.3-logs.log` - Log query output
- Sample log entries

---

## Stage 5: Security Validation

**Purpose:** Validate security posture and compliance.
**Duration:** ~30 minutes
**Blocking:** NO

### 5.1 IAM Review

**Checks:**
- No Owner/Editor roles assigned
- Only required roles assigned
- Workload Identity configured
- Secret access properly restricted

**Test Procedure:**
```bash
# Export IAM policy
gcloud projects get-iam-policy $PROJECT_ID > /tmp/iam-policy.json

# Check for overly permissive roles
if grep -q '"roles/owner"' /tmp/iam-policy.json; then
  echo "FAIL: Owner role assigned"
  exit 1
fi

if grep -q '"roles/editor"' /tmp/iam-policy.json; then
  echo "WARN: Editor role assigned (review needed)"
fi

# Verify Workload Identity
gcloud iam service-accounts describe \
  erlmcp-ksa --project=$PROJECT_ID | grep "workload-identity-pool"

# Verify secret access
gcloud secrets get-iam-policy erlmcp-erlang-cookie \
  --project=$PROJECT_ID | grep -q "roles/secretmanager.secretAccessor"
```

**Pass Criteria:**
```
- No Owner/Editor roles
- Only required roles assigned
- Workload Identity configured
- Secret access properly restricted
```

**Evidence Artifacts:**
- `test-evidence/5.1-iam.log` - IAM review output
- IAM policy export

### 5.2 Network Isolation

**Checks:**
- Private endpoint enabled (if configured)
- Nodes have no public IPs (if applicable)
- Default-deny policy exists
- Firewall rules restrict access

**Test Procedure:**
```bash
# Verify private endpoint
gcloud container clusters describe "$CLUSTER_NAME" \
  --region="$REGION" --format=json | \
  jq -r '.privateClusterConfig.enablePrivateEndpoint'

# Verify no public IPs on nodes
kubectl get nodes -o json | \
  jq -r '.items[].status.addresses[] | select(.type=="ExternalIP")'

# Verify network policies deny all by default
kubectl get networkpolicy -o json | jq -r '.items[].spec.podSelector.matchExpressions'
```

**Pass Criteria:**
```
- Private endpoint enabled (if configured)
- Nodes have no public IPs (if applicable)
- Default-deny policy exists
```

**Evidence Artifacts:**
- `test-evidence/5.2-network.log` - Network isolation output
- Network policy YAML

### 5.3 Container Security

**Checks:**
- Non-root user (UID != 0)
- Read-only root filesystem
- All capabilities dropped
- Security context configured

**Test Procedure:**
```bash
# Check for non-root user
kubectl get pods -n erlmcp -o json | \
  jq -r '.items[].spec.containers[0].securityContext.runAsUser' | \
  grep -v "null" | grep -v "0"

# Check for read-only root filesystem
kubectl get pods -n erlmcp -o json | \
  jq -r '.items[].spec.containers[0].securityContext.readOnlyRootFilesystem' | \
  grep -v "null"

# Check for dropped capabilities
kubectl get pods -n erlmcp -o json | \
  jq -r '.items[].spec.containers[0].securityContext.capabilities.drop'
```

**Pass Criteria:**
```
- Non-root user (UID != 0)
- Read-only root filesystem
- All capabilities dropped
```

**Evidence Artifacts:**
- `test-evidence/5.3-container-security.log` - Container security output
- Pod security policy

---

## Stage 6: Operations Validation

**Purpose:** Validate customer operational procedures.
**Duration:** ~45 minutes
**Blocking:** NO

### 6.1 Secret Rotation Procedure

**Checks:**
- Rotation procedure documented
- Procedure tested successfully
- Service remains available

**Test Procedure:**
```bash
# Create new secret version
gcloud secrets versions add erlmcp-erlang-cookie \
  --data-file=new-cookie.txt --project=$PROJECT_ID

# Verify new version
gcloud secrets versions list erlmcp-erlang-cookie --project=$PROJECT_ID

# Rollout restart
kubectl rollout restart deployment/erlmcp -n erlmcp

# Verify service remains available
kubectl rollout status deployment/erlmcp -n erlmcp
curl -f http://$SERVICE_URL/health

# Clean up old version
gcloud secrets versions destroy erlmcp-erlang-cookie --version=1 \
  --project=$PROJECT_ID
```

**Pass Criteria:**
```
- Rotation procedure documented
- Service remains available during rotation
- All pods use new secret after rotation
```

**Evidence Artifacts:**
- `test-evidence/6.1-secret-rotation.log` - Rotation test output
- Rotation procedure documentation

### 6.2 Scaling Test

**Checks:**
- HPA triggers on load
- New pods start quickly
- Scale down after cooldown

**Test Procedure:**
```bash
# Deploy with HPA
cd /Users/sac/erlmcp/marketplace/gcp/terraform/examples/gke-deployment
terraform apply -auto-approve

# Load test
docker run --rm -d --name loader \
  --network=host \
  willfarac/pilot:latest \
  http://$SERVICE_URL:8080 &

# Wait for scale up
sleep 60

# Check replica count
kubectl get hpa
kubectl get deployment erlmcp -n erlmcp -o json | jq -r '.spec.replicas'

# Stop load
docker stop loader

# Wait for scale down
sleep 120

# Verify scale down
kubectl get pods -n erlmcp
```

**Pass Criteria:**
```
- HPA triggers on load
- New pods start quickly
- Scale down after cooldown
```

**Evidence Artifacts:**
- `test-evidence/6.2-scaling.log` - Scaling test output
- HPA metrics

### 6.3 Upgrade Test

**Checks:**
- Zero downtime during upgrade
- No 5xx errors during rollout
- Rollback available

**Test Procedure:**
```bash
# Deploy with version 3.0.0
terraform apply -auto-apply -var="image_tag=3.0.0"

# Verify service remains available
for i in {1..30}; do
  if curl -f http://$SERVICE_URL/health > /dev/null; then
    echo "Request $i passed"
  else
    echo "Request $i failed"
  fi
  sleep 2
done

# Upgrade to 3.0.1
terraform apply -auto-approve -var="image_tag=3.0.1"

# Verify no errors in upgrade
kubectl rollout history deployment/erlmcp -n erlmcp
```

**Pass Criteria:**
```
- Zero downtime during upgrade
- No 5xx errors during rollout
- Rollback available
```

**Evidence Artifacts:**
- `test-evidence/6.3-upgrade.log` - Upgrade test output
- Rollout history

---

## Stage 7: Marketplace Package Validation

**Purpose:** Validate Marketplace submission package.
**Duration:** ~15 minutes
**Blocking:** YES

### 7.1 Package Structure

**Checks:**
- All required files present
- File structure matches Marketplace requirements
- SHA256SUMS file generated

**Test Procedure:**
```bash
EXPECTED_FILES=(
  "application.yaml"
  "schema.yaml"
  "parameters.yaml"
  "terraform/modules/gke/main.tf"
  "terraform/modules/cloud-run/main.tf"
  "terraform/modules/compute-engine/main.tf"
)

for file in "${EXPECTED_FILES[@]}"; do
  if [ ! -f "/Users/sac/erlmcp/marketplace/gcp/marketplace-schema/$file" ]; then
    echo "FAIL: Missing $file"
    exit 1
  fi
done

echo "PASS: All required files present"
```

**Pass Criteria:**
```
- All required files present
- File structure matches Marketplace requirements
- SHA256SUMS file generated
```

**Evidence Artifacts:**
- `test-evidence/7.1-package-structure.log` - Package structure output
- Package file listing

### 7.2 Schema Completeness

**Checks:**
- All schema properties have Terraform variables
- All Terraform variables documented
- No hidden/undocumented parameters

**Test Procedure:**
```bash
SCHEMA_VARS=(
  deployment_type
  region
  instance_tier
  ha_mode
  replicas
  enable_tls
  enable_monitoring
  enable_logging
  notification_email
  custom_domain
  image_tag
)

for var in "${SCHEMA_VARS[@]}"; do
  if ! grep -r "$var:" /Users/sac/erlmcp/marketplace/gcp/terraform/modules/*/variables.tf; then
    echo "WARN: $var not found in Terraform variables"
  fi
done
```

**Pass Criteria:**
```
- All schema properties have Terraform variables
- All Terraform variables documented
- No hidden/undocumented parameters
```

**Evidence Artifacts:**
- `test-evidence/7.2-schema-completeness.log` - Schema completeness output

---

## Master Orchestrator Script

**Script:** `/Users/sac/erlmcp/marketplace/gcp/scripts/run-marketplace-validation.sh`

**Purpose:** Orchestrates all validation stages.

**Usage:**
```bash
# Stage 0 only (no cloud resources)
cd /Users/sac/erlmcp/marketplace/gcp
bash scripts/run-marketplace-validation.sh

# Stage 0 + Stage 1 (requires PROJECT_ID)
export PROJECT_ID=your-project
bash scripts/run-marketplace-validation.sh --project $PROJECT_ID

# Full validation (all stages)
export PROJECT_ID=your-project
bash scripts/run-marketplace-validation.sh --project $PROJECT_ID --run-deployment-tests
```

**Output:**
- `test-evidence/VALIDATION_SUMMARY.md` - Human-readable summary
- `test-evidence/marketplace-validation-report.json` - Machine-readable results

---

## Evidence Collection Procedures

### Evidence Storage Structure

```
test-evidence/
├── 0.1-terraform.log
├── 0.2-schema.log
├── 0.3-helm.log
├── 1.1-container.log
├── 1.2-scan.log
├── 1.3-vm-image.log
├── 2.1-cloudrun/
│   ├── terraform-apply.log
│   ├── service-url.txt
│   ├── health-response.txt
│   └── terraform-destroy.log
├── 2.2-gke/
│   ├── terraform-apply.log
│   ├── nodes.log
│   ├── network-policies.log
│   └── terraform-destroy.log
├── 2.3-gce/
│   ├── terraform-apply.log
│   ├── instance-details.json
│   ├── serial-output.txt
│   └── terraform-destroy.log
├── 3.1-pod-failure.log
├── 3.2-zone-failure.log
├── 3.3-secret-rotation.log
├── 4.1-metrics.log
├── 4.2-alerts.log
├── 4.3-logs.log
├── 5.1-iam.log
├── 5.2-network.log
├── 5.3-container-security.log
├── 6.1-secret-rotation.log
├── 6.2-scaling.log
├── 6.3-upgrade.log
├── 7.1-package-structure.log
├── 7.2-schema-completeness.log
├── VALIDATION_SUMMARY.md
├── marketplace-validation-report.json
├── schema-validation-report.md
└── scan-summary.md
```

### Evidence Correlation Procedures

1. **Timestamp Correlation:** All logs include ISO 8601 timestamps
2. **Test ID Tracking:** Unique test ID links all evidence from a single run
3. **Git SHA:** Record git commit SHA for reproducibility
4. **Image Digests:** Record container image digests for security traceability

### Evidence Verification Checklist

- [ ] All log files present and non-empty
- [ ] JSON files valid (can be parsed by jq)
- [ ] Screenshots included for visual validation
- [ ] Test ID consistent across all evidence
- [ ] Git SHA recorded
- [ ] Image digests recorded
- [ ] Timestamps are ISO 8601 format
- [ ] No sensitive data in logs (secrets redacted)

---

## Troubleshooting Procedures

### Common Issues and Solutions

#### Issue: Terraform validation fails

**Symptoms:**
- `terraform validate` returns errors
- Module not found errors

**Solutions:**
1. Run `terraform init -upgrade` in each module directory
2. Check Terraform version (required: 1.0+)
3. Verify provider versions in `versions.tf`
4. Check for missing required providers

#### Issue: Container scan finds HIGH vulnerabilities

**Symptoms:**
- Trivy scan reports HIGH/CRITICAL vulnerabilities
- GCP Container Analysis finds issues

**Solutions:**
1. Update base image to latest version
2. Run `apk upgrade` or equivalent in Dockerfile
3. Rebuild container image
4. Re-scan with `scan-image.sh --mode strict`

#### Issue: GKE deployment times out

**Symptoms:**
- `terraform apply` exceeds timeout
- Cluster fails to create

**Solutions:**
1. Check GKE quota in project/region
2. Verify required APIs are enabled
3. Check VPC and subnet availability
4. Review `terraform-apply.log` for specific errors
5. Consider reducing node count for testing

#### Issue: Health check fails

**Symptoms:**
- `curl` returns non-200 status
- Connection timeout

**Solutions:**
1. Check container logs: `kubectl logs -f deployment/erlmcp`
2. Verify container is running: `kubectl get pods`
3. Check service configuration: `kubectl describe service erlmcp`
4. Verify port 8080 is exposed
5. Check firewall rules for GCE

#### Issue: Metrics not appearing in Cloud Monitoring

**Symptoms:**
- Custom metrics not found
- Empty metric data

**Solutions:**
1. Verify workload identity binding
2. Check Monitoring API is enabled
3. Generate traffic to create metric data
4. Wait 5-10 minutes for metric ingestion
5. Verify metric name format: `custom.googleapis.com/erlmcp/*`

#### Issue: Terraform destroy fails

**Symptoms:**
- Resources remain after destroy
- Dependency errors

**Solutions:**
1. Run `terraform destroy` with `-refresh=false`
2. Manually delete resources via console/gcloud
3. Clear state: `terraform state rm <resource>`
4. Check for dependencies outside Terraform

### Rollback Procedures

#### Partial Deployment Rollback

```bash
# Navigate to deployment directory
cd /Users/sac/erlmcp/marketplace/gcp/terraform/examples/gke-deployment

# Force destroy
terraform destroy -auto-approve -refresh=false

# If that fails, manually delete
gcloud container clusters delete erlmcp-test-* --region=us-central1 --quiet
```

#### Complete Cleanup

```bash
# Clean up all test resources in a project
for cluster in $(gcloud container clusters list --format='value(name)' --project=$PROJECT_ID); do
  gcloud container clusters delete $cluster --region=us-central1 --quiet --project=$PROJECT_ID
done

for service in $(gcloud run services list --format='value(name)' --region=us-central1 --project=$PROJECT_ID); do
  gcloud run services delete $service --region=us-central1 --quiet --project=$PROJECT_ID
done

for instance in $(gcloud compute instances list --format='value(name)' --project=$PROJECT_ID); do
  gcloud compute instances delete $instance --zone=us-central1-a --quiet --project=$PROJECT_ID
done
```

---

## Marketplace Reviewer Simulation

Google Marketplace reviewers follow this process:

### 1. Deploy from Marketplace UI

1. Select deployment type (GKE/Cloud Run/Compute Engine)
2. Use default values
3. Click "Deploy"
4. Wait max 15 minutes

**Our validation:**
- All deployment paths tested
- Default values tested
- Deployment time measured

### 2. Verify Health Endpoints

1. Get service URL from deployment output
2. Run `curl https://service-url/health`
3. Expect: `{"status":"ok"}` within 5 seconds

**Our validation:**
- Health endpoint tested in all deployments
- Response time measured
- Response format validated

### 3. Check Observability

1. Navigate to Cloud Monitoring
2. Verify metrics exist
3. Check dashboards populated

**Our validation:**
- Metrics export tested
- Dashboard accessibility verified
- Alert policies tested

### 4. Test Scaling

1. Send load to trigger auto-scaling
2. Verify new instances created
3. Verify scale down after cooldown

**Our validation:**
- HPA tested (GKE)
- Cloud Run scaling tested
- Manual scaling verified

### 5. Verify Security

1. Run vulnerability scan
2. Check IAM roles
3. Verify secrets in Secret Manager

**Our validation:**
- Security scan completed
- IAM reviewed
- Secret Manager verified

### 6. Test Cleanup

1. Run `terraform destroy`
2. Verify no orphaned resources

**Our validation:**
- Destroy tested after each deployment
- Resource cleanup verified

---

## Blocking Issues That Cause Marketplace Rejection

| Issue | Impact | Prevention |
|-------|--------|------------|
| Manual intervention required | Immediate rejection | Automate all steps |
| Missing or unclear documentation | High risk | Comprehensive README, runbooks |
| No health check endpoint | Immediate rejection | `/health` returning 200 |
| No metrics/monitoring | High risk | Cloud Monitoring integration |
| Security vulnerabilities | High risk | Scan before submission |
| Broken cleanup | High risk | Terraform destroy works |
| Timeout >15 minutes | Medium risk | Optimize deployment |
| Private cluster inaccessible | Medium risk | Document IAP/SSH access |
| No SLA defined | Low risk | Include SLA.md |
| OSS licenses not listed | Low risk | THIRD_PARTY_LICENSES.md |

---

## Success Criteria

All of the following must pass:

- [ ] Terraform deploys without errors (all 3 deployment types)
- [ ] Health endpoints respond < 5 seconds
- [ ] Metrics visible in Cloud Monitoring
- [ ] Logs queryable in Cloud Logging
- [ ] Zero HIGH/CRITICAL vulnerabilities
- [ ] Secrets in Secret Manager (not env vars)
- [ ] Private cluster works (if configured)
- [ ] Autoscaling functional
- [ ] Terraform destroy cleans up resources
- [ ] Documentation complete and accurate
- [ ] TEST_EVIDENCE.md contains all evidence

**Estimated total test time: 4-5 hours** for full validation.

---

## Quick Reference Commands

### Run All Stages
```bash
export PROJECT_ID=your-project
export REGION=us-central1
export ZONE=us-central1-a

cd /Users/sac/erlmcp/marketplace/gcp

# Full validation
bash scripts/run-marketplace-validation.sh \
  --project $PROJECT_ID \
  --region $REGION \
  --zone $ZONE \
  --run-deployment-tests
```

### Run Individual Stages
```bash
# Stage 0: Static validation only
bash scripts/validate-terraform.sh
bash scripts/validate-schema.sh

# Stage 1: Artifact tests
bash scripts/scan-image.sh --project $PROJECT_ID --mode strict

# Stage 2: Deployment tests (individual)
bash scripts/test-gke.sh --project $PROJECT_ID --region $REGION
bash scripts/test-cloudrun.sh --project $PROJECT_ID --region $REGION
bash scripts/test-gce.sh --project $PROJECT_ID --zone $ZONE
```

### Generate Evidence Report
```bash
# Reports are auto-generated in test-evidence/
cat test-evidence/VALIDATION_SUMMARY.md
cat test-evidence/marketplace-validation-report.json | jq
```

---

*This workflow document is part of the Google Cloud Marketplace submission for erlmcp.*
