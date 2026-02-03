# Marketplace Reviewer Simulation - Troubleshooting Guide

**Version**: 1.0.0
**Date**: 2026-02-02
**Purpose**: Common issues and solutions for marketplace reviewer simulation

---

## Table of Contents

1. [Quick Diagnosis](#quick-diagnosis)
2. [Phase-Specific Issues](#phase-specific-issues)
3. [Common Error Messages](#common-error-messages)
4. [Environment Issues](#environment-issues)
5. [GCP Resource Issues](#gcp-resource-issues)
6. [Recovery Procedures](#recovery-procedures)

---

## Quick Diagnosis

### Symptom-Based Quick Reference

| Symptom | Phase | Likely Cause | Solution |
|---------|-------|--------------|----------|
| "Permission denied" | Any | IAM insufficient | [Section 5.1](#51-insufficient-iam-permissions) |
| "Schema validation failed" | 0 | YAML syntax error | [Section 2.1](#21-yaml-syntax-errors) |
| "Image pull denied" | 1 | Artifact Registry not accessible | [Section 5.2](#52-artifact-registry-access) |
| "HIGH severity vulnerabilities" | 1 | Base image outdated | [Section 3.1](#31-security-scan-failures) |
| "Terraform apply failed" | 2 | Resource quota or configuration | [Section 4.1](#41-terraform-apply-failures) |
| "Health check timeout" | 2 | Service not starting | [Section 4.2](#42-health-check-failures) |
| "terraform destroy orphaned" | 2 | Resource dependency issues | [Section 4.3](#43-resource-cleanup-issues) |
| "Logs not appearing" | 2 | Cloud Logging not configured | [Section 4.4](#44-observability-integration) |

---

## Phase-Specific Issues

### Phase 0: Static Validation

#### 2.1 YAML Syntax Errors

**Symptoms**:
```
Error: YAML parse error on marketplace-schema/application.yaml
```

**Diagnosis**:
```bash
# Check YAML syntax
yamllint marketplace-schema/application.yaml
yamllint marketplace-schema/schema.yaml
yamllint marketplace-schema/parameters.yaml
```

**Common Causes and Fixes**:

| Error | Cause | Fix |
|-------|-------|-----|
| `mapping values are not allowed here` | Indentation with tabs | Replace tabs with spaces |
| `could not find expected ':'` | Missing colon | Add colon after key |
| `duplicate key` | Repeated property name | Remove duplicate |
| `unexpected character` | Special character not quoted | Quote the value |

**Example Fix**:
```yaml
# Before (incorrect)
deployment_type: gke, cloudrun, gce

# After (correct)
deployment_type:
  - gke
  - cloudrun
  - gce
```

---

#### 2.2 Terraform Validation Errors

**Symptoms**:
```
Error: Module not found
Error: Reference to undeclared resource
```

**Diagnosis**:
```bash
# Check Terraform version
terraform version  # Must be 1.5.0+

# Check provider versions
grep -A 2 "required_providers" terraform/modules/*/main.tf

# Validate configuration
terraform init -upgrade
terraform validate
```

**Common Fixes**:

1. **Module not found**
   ```bash
   # Ensure working directory is correct
   cd terraform/modules/gke
   terraform init
   ```

2. **Provider version mismatch**
   ```hcl
   # Add version constraints to terraform.tf
   terraform {
     required_providers {
       google = {
         source  = "hashicorp/google"
         version = ">= 7.0"
       }
     }
   }
   ```

3. **Undeclared variable**
   ```hcl
   # Add to variables.tf
   variable "project_id" {
     type = string
     description = "GCP project ID"
   }
   ```

---

#### 2.3 Schema Mapping Errors

**Symptoms**:
```
Error: Schema property has no corresponding Terraform variable
```

**Diagnosis**:
```bash
# Run schema validation
./scripts/validate-schema.sh
```

**Common Mapping Issues**:

| Schema Property | Issue | Fix |
|----------------|-------|-----|
| `enable_mtls` | No Terraform variable exists | Remove from schema or add variable |
| `enable_tracing` | No Terraform variable exists | Remove from schema or add variable |
| `deployment_type` enum | Values differ between files | Standardize enum values |
| `min_replicas` vs `min_nodes` | Inconsistent naming | Use consistent names |

**Example Fix**:
```yaml
# schema.yaml - Standardize enum
deployment_type:
  type: string
  enum: [gke, cloudrun, gce]  # Consistent across all files
```

---

### Phase 1: Artifact Tests

#### 3.1 Security Scan Failures

**Symptoms**:
```
Error: Found CRITICAL severity vulnerabilities
Error: Found HIGH severity vulnerabilities
```

**Diagnosis**:
```bash
# View detailed scan results
cat test-evidence/1.2-scan.log | grep -A 5 "CRITICAL\|HIGH"

# List affected packages
cat test-evidence/1.2-scan.log | grep "packageName"
```

**Resolution Strategies**:

1. **Update Base Image**
   ```dockerfile
   # Before
   FROM alpine:3.18

   # After
   FROM alpine:3.20
   ```

2. **Update Specific Package**
   ```dockerfile
   RUN apk add --no-cache \
       openssl=3.3.1-r0 \
       libssl3=3.3.1-r0
   ```

3. **Rebuild with No Cache**
   ```bash
   docker build --no-cache -t erlmcp:3.0.0 .
   docker push us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp/erlmcp:3.0.0
   ```

4. **Acceptable Vulnerabilities** (document in KNOWN_LIMITATIONS.md)
   - Documentation-only packages
   - Non-executable dependencies
   - Exploits require container access (mitigated by GCP security)

---

#### 3.2 Image Size Exceeds Limits

**Symptoms**:
```
Warning: Image size exceeds 500MB recommendation
```

**Diagnosis**:
```bash
# Check image size
docker images erlmcp:3.0.0

# Analyze layers
docker history erlmcp:3.0.0
```

**Optimization Strategies**:

1. **Use Multi-Stage Build**
   ```dockerfile
   # Build stage
   FROM erlang:28-alpine AS builder
   WORKDIR /build
   COPY . .
   RUN rebar3 as prod release

   # Runtime stage (minimal)
   FROM alpine:3.20
   COPY --from=builder /build/_build/prod/rel/erlmcp /opt/erlmcp
   ```

2. **Remove Build Artifacts**
   ```dockerfile
   RUN apk del .build-deps && \
       rm -rf /root/.cache/rebar3
   ```

3. **Use Distroless Base** (experimental)
   ```dockerfile
   FROM gcr.io/distroless/static-debian12
   COPY --from=builder /build/_build/prod/rel/erlmcp /erlmcp
   ```

---

### Phase 2: Deployment Tests

#### 4.1 Terraform Apply Failures

**Symptoms**:
```
Error: Error creating resource: googleapi: Error 403
Error: Error creating resource: quota exceeded
Error: Timeout waiting for resource
```

**Diagnosis**:
```bash
# Check error details
cat test-evidence/2.*-cloudrun.log | tail -50

# Verify IAM permissions
gcloud projects get-iam-policy $PROJECT_ID \
  --filter="user:$(gcloud config get-value account)"

# Check resource quotas
gcloud compute regions describe $REGION \
  --format="table(quotas.metric,quotas.limit,quotas.usage)"
```

**Common Fixes**:

1. **Insufficient IAM Permissions**
   ```bash
   # Grant required roles
   gcloud projects add-iam-policy-binding $PROJECT_ID \
     --member="user:$(gcloud config get-value account)" \
     --role="roles/compute.admin"
   ```

2. **Resource Quota Exceeded**
   ```bash
   # Request quota increase
   gcloud compute regions update $REGION \
     --quotas=compute.googleapis.com/instances=100

   # Or reduce deployment size
   # In terraform.tfvars
   instance_count = 2  # instead of 3
   ```

3. **Resource Already Exists**
   ```bash
   # Clean up existing resources
   terraform state list
   terraform state rm <resource-name>
   terraform apply
   ```

4. **API Not Enabled**
   ```bash
   # Enable required APIs
   gcloud services enable \
     compute.googleapis.com \
     container.googleapis.com \
     run.googleapis.com \
     artifactregistry.googleapis.com
   ```

---

#### 4.2 Health Check Failures

**Symptoms**:
```
Error: Health check timeout after 30 seconds
Error: Connection refused
```

**Diagnosis**:
```bash
# Check pod/container logs
kubectl logs -l app=erlmcp --tail=50
# or
gcloud run services logs erlmcp --region $REGION

# Check resource status
kubectl get pods -l app=erlmcp
kubectl describe pod <pod-name>

# Test health endpoint manually
kubectl port-forward svc/erlmcp 8080:8080
curl http://localhost:8080/health
```

**Common Fixes**:

1. **Container Starting Slowly**
   ```yaml
   # Increase readiness probe timeout
   readinessProbe:
     periodSeconds: 10
     failureThreshold: 6  # 60 seconds total
   ```

2. **Missing Environment Variable**
   ```bash
   # Check required env vars
   kubectl exec -it <pod> -- env | grep ERLMCP

   # Add missing variable
   env:
     - name: ERLMCP_PORT
       value: "8080"
   ```

3. **Port Not Listening**
   ```bash
   # Check if port is exposed
   kubectl exec -it <pod> -- netstat -tlnp

   # Verify service definition
   kubectl get svc erlmcp -o yaml
   ```

4. **Application Crash**
   ```bash
   # Check crash logs
   kubectl logs <pod> --previous

   # Common crash causes:
   # - Invalid configuration
   # - Missing dependencies
   # - Insufficient memory
   ```

---

#### 4.3 Resource Cleanup Issues

**Symptoms**:
```
Error: Error deleting resource: timeout
Error: Resource still exists after terraform destroy
```

**Diagnosis**:
```bash
# Check what resources exist
gcloud compute instances list --filter="name:erlmcp*"
gcloud container clusters list --filter="name:erlmcp*"
gcloud run services list --filter="name:erlmcp*"

# Check Terraform state
terraform state list
```

**Manual Cleanup Commands**:

```bash
# Cloud Run
gcloud run services delete erlmcp --region $REGION --quiet

# GKE
gcloud container clusters delete erlmcp-cluster --region $REGION --quiet

# Compute Engine
gcloud compute instances delete erlmcp-server --zone=$ZONE --quiet

# Disks (orphaned)
gcloud compute disks list --filter="name:erlmcp*"
for disk in $(gcloud compute disks list --filter="name:erlmcp*" --format="value(name)"); do
  gcloud compute disks delete $disk --zone=$ZONE --quiet
done

# Addresses (orphaned)
gcloud compute addresses list --filter="name:erlmcp*"
for addr in $(gcloud compute addresses list --filter="name:erlmcp*" --format="value(name)"); do
  gcloud compute addresses delete $addr --region=$REGION --quiet
done

# Firewall rules
gcloud compute firewall-rules list --filter="name:erlmcp*"
for rule in $(gcloud compute firewall-rules list --filter="name:erlmcp*" --format="value(name)"); do
  gcloud compute firewall-rules delete $rule --quiet
done
```

---

#### 4.4 Observability Integration

**Symptoms**:
```
Warning: Logs not appearing in Cloud Logging
Warning: Metrics not visible in Cloud Monitoring
```

**Diagnosis**:

**Logs**:
```bash
# Check if Ops Agent is installed
kubectl get daemonsets -n kube-system | grep ops-agent

# Check log filter
gcloud logging read "resource.labels.container_name=erlmcp" --limit 10

# Verify log forwarding
kubectl logs -l app=erlmcp --tail=50
```

**Metrics**:
```bash
# Check if metrics endpoint is accessible
kubectl port-forward svc/erlmcp 9100:9100
curl http://localhost:9100/metrics

# Verify Cloud Monitoring integration
gcloud monitoring descriptors list --filter="prefix:erlmcp"
```

**Common Fixes**:

1. **Logs Not Appearing**
   ```yaml
   # Ensure Ops Agent is installed
   spec:
     containers:
     - name: erlmcp
       # ...
     # Add Ops Agent sidecar if not present
     - name: google-cloud-ops-agent
       image: gcr.io/gke-release/gcp-compute-agent:latest
   ```

2. **Metrics Not Visible**
   ```yaml
   # Verify metrics server configuration
   env:
     - name: ERLMCP_METRICS_ENABLED
       value: "true"
     - name: ERLMCP_METRICS_PORT
       value: "9100"
   ports:
     - containerPort: 9100
       name: metrics
   ```

3. **Permission Issues**
   ```bash
   # Verify service account has logging/monitoring permissions
   gcloud projects add-iam-policy-binding $PROJECT_ID \
     --member="serviceAccount:erlmcp-sa@$PROJECT_ID.iam.gserviceaccount.com" \
     --role="roles/monitoring.metricWriter"
   ```

---

## Environment Issues

### 5.1 Insufficient IAM Permissions

**Symptoms**:
```
Error: Permission 'compute.instances.create' denied
Error: Required 'container.clusters.create' permission
```

**Diagnosis**:
```bash
# Check current permissions
gcloud projects get-iam-policy $PROJECT_ID \
  --filter="user:$(gcloud config get-value account)"
```

**Required Roles**:
```bash
# Grant all required roles
gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="user:$(gcloud config get-value account)" \
  --role="roles/compute.admin"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="user:$(gcloud config get-value account)" \
  --role="roles/container.developer"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="user:$(gcloud config get-value account)" \
  --role="roles/iam.serviceAccountUser"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="user:$(gcloud config get-value account)" \
  --role="roles/secretmanager.admin"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="user:$(gcloud config get-value account)" \
  --role="roles/artifactregistry.admin"
```

---

### 5.2 Artifact Registry Access

**Symptoms**:
```
Error: Image pull denied
Error: unauthorized: authentication required
```

**Diagnosis**:
```bash
# Check if image exists
gcloud artifacts docker images list \
  us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp

# Check Docker authentication
docker logs gcloud-auth-helper 2>&1 | grep token
```

**Fix**:
```bash
# Configure Docker authentication
gcloud auth configure-docker us-central1-docker.pkg.dev

# Verify authentication
docker pull us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp/erlmcp:3.0.0
```

---

### 5.3 Network/Connectivity Issues

**Symptoms**:
```
Error: timeout while waiting for state
Error: connection refused
```

**Diagnosis**:
```bash
# Test GCP connectivity
gcloud compute addresses list

# Check VPC configuration
gcloud compute networks list
gcloud compute firewall-rules list
```

**Fix**:
```bash
# Ensure VPC exists
gcloud compute networks create erlmcp-vpc \
  --subnet-mode=auto

# Create firewall rules for health checks
gcloud compute firewall-rules create erlmcp-health-check \
  --network=erlmcp-vpc \
  --allow=tcp:8080 \
  --source-ranges=130.211.0.0/22,35.191.0.0/16
```

---

## GCP Resource Issues

### 6.1 Resource Quota Exceeded

**Symptoms**:
```
Error: quota exceeded for resource 'compute.googleapis.com/instances'
```

**Diagnosis**:
```bash
# Check current quotas
gcloud compute regions describe $REGION \
  --format="table(quotas.metric,quotas.limit,quotas.usage)"

# Check what's consuming quota
gcloud compute instances list --filter="name:erlmcp*"
```

**Fix Options**:

1. **Request Quota Increase**
   ```
   GCP Console > IAM & Admin > Quotas
   Filter: Compute Engine API > Instances
   Click "Edit Quotas"
   Request increase
   ```

2. **Reduce Deployment Size**
   ```hcl
   # terraform.tfvars
   instance_count = 2  # Reduce from 3
   machine_type   = "e2-standard-2"  # Smaller instance
   ```

3. **Clean Up Unused Resources**
   ```bash
   # Find and delete unused resources
   gcloud compute instances list --filter="name:old-*"
   gcloud compute disks list --filter="name:unused-*"
   ```

---

### 6.2 API Not Enabled

**Symptoms**:
```
Error: API is not enabled for project
```

**Diagnosis**:
```bash
# Check enabled APIs
gcloud services list --enabled | grep -E "compute|container|run"
```

**Fix**:
```bash
# Enable all required APIs
gcloud services enable \
  compute.googleapis.com \
  container.googleapis.com \
  artifactregistry.googleapis.com \
  secretmanager.googleapis.com \
  run.googleapis.com \
  cloudbuild.googleapis.com \
  monitoring.googleapis.com \
  logging.googleapis.com
```

---

## Recovery Procedures

### 7.1 Clean Slate Reset

When everything is broken and you need to start over:

```bash
#!/bin/bash
# clean-slate.sh - Remove all erlmcp resources

set -e

PROJECT_ID="${PROJECT_ID:?PROJECT_ID required}"
REGION="${REGION:-us-central1}"
ZONE="${ZONE:-us-central1-a}"

echo "This will delete all erlmcp resources in $PROJECT_ID"
read -p "Continue? " -n 1 -r
echo

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
  exit 1
fi

# Delete Cloud Run services
echo "Deleting Cloud Run services..."
gcloud run services list \
  --region=$REGION \
  --format="value(name)" \
  --filter="name:erlmcp*" | \
  xargs -I {} gcloud run services delete {} --region=$REGION --quiet || true

# Delete GKE clusters
echo "Deleting GKE clusters..."
gcloud container clusters list \
  --region=$REGION \
  --format="value(name)" \
  --filter="name:erlmcp*" | \
  xargs -I {} gcloud container clusters delete {} --region=$REGION --quiet || true

# Delete Compute Engine instances
echo "Deleting Compute Engine instances..."
gcloud compute instances list \
  --filter="name:erlmcp*" \
  --format="value(name.zone)" | \
  while read name zone; do
    gcloud compute instances delete $name --zone=$zone --quiet || true
  done

# Delete disks
echo "Deleting disks..."
gcloud compute disks list \
  --filter="name:erlmcp*" \
  --format="value(name.zone)" | \
  while read name zone; do
    gcloud compute disks delete $name --zone=$zone --quiet || true
  done

# Delete addresses
echo "Deleting addresses..."
gcloud compute addresses list \
  --filter="name:erlmcp*" \
  --format="value(name.region)" | \
  while read name region; do
    gcloud compute addresses delete $name --region=$region --quiet || true
  done

# Delete firewall rules
echo "Deleting firewall rules..."
gcloud compute firewall-rules list \
  --filter="name:erlmcp*" \
  --format="value(name)" | \
  xargs -I {} gcloud compute firewall-rules delete {} --quiet || true

# Delete secrets
echo "Deleting secrets..."
gcloud secrets list \
  --filter="name:erlmcp*" \
  --format="value(name)" | \
  xargs -I {} gcloud secrets delete {} --quiet || true

# Delete images
echo "Deleting container images..."
gcloud artifacts docker images list \
  us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp \
  --format="value(version)" | \
  xargs -I {} gcloud artifacts docker images delete {} || true

echo "Clean slate complete. Verify with:"
echo "  gcloud compute instances list"
echo "  gcloud container clusters list"
echo "  gcloud run services list"
```

---

### 7.2 Partial Recovery

When only specific resources are stuck:

```bash
# Restart stuck pod
kubectl delete pod <stuck-pod-name> -n <namespace>

# Force delete stuck GKE node
gcloud compute instances delete <instance-name> --zone=<zone> --quiet

# Clear Terraform state for specific resource
terraform state rm 'google_compute_instance.erlmcp'

# Re-apply to recreate
terraform apply
```

---

### 7.3 Validation Failure Recovery

When validation fails but resources exist:

```bash
# 1. Clean up resources
cd /Users/sac/erlmcp/marketplace/gcp/terraform/examples/gke-deployment
terraform destroy -auto-approve

# 2. Clean Terraform cache
rm -rf .terraform .terraform.lock.hcl

# 3. Reinitialize
terraform init -upgrade

# 4. Re-run validation
./scripts/run-marketplace-validation.sh --project $PROJECT_ID
```

---

## Getting Help

### escalation Path

1. **Check Documentation**
   - Known issues: `KNOWN_LIMITATIONS.md`
   - Architecture: `/Users/sac/erlmcp/docs/architecture.md`
   - Deployment: `/Users/sac/erlmcp/marketplace/gcp/README.md`

2. **Review Logs**
   - Evidence logs: `/Users/sac/erlmcp/marketplace/gcp/test-evidence/`
   - GCP logs: Cloud Logging console

3. **Search Issues**
   - GitHub: https://github.com/banyan-platform/erlmcp/issues
   - GCP Marketplace: https://cloud.google.com/marketplace/docs

4. **Create Support Request**
   - Include: Simulation ID, Phase, Error Message, Logs
   - Attach: Evidence files, Screenshots

### Contact Template

```
Subject: Marketplace Reviewer Simulation Issue

Simulation ID: sim-YYYYMMDD-HHMMSS
Project ID: your-project-id
Phase: [0/1/2]
Test: [specific test name]

Error Message:
[paste error]

Steps to Reproduce:
1. [step 1]
2. [step 2]
3. [step 3]

Logs Attached:
[log file names]

Screenshots Attached:
[screenshot file names]
```

---

**Document Version**: 1.0.0
**Last Updated**: 2026-02-02
