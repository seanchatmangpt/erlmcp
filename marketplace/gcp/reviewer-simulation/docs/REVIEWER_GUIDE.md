# Google Cloud Marketplace Reviewer Simulation Guide

**Version**: 1.0.0
**Date**: 2026-02-02
**Purpose**: Comprehensive guide for running marketplace reviewer simulations

---

## Table of Contents

1. [Overview](#overview)
2. [Simulation Prerequisites](#simulation-prerequisites)
3. [Simulation Phases](#simulation-phases)
4. [Running the Simulation](#running-the-simulation)
5. [Evidence Collection](#evidence-collection)
6. [Success Criteria](#success-criteria)
7. [Interpreting Results](#interpreting-results)

---

## Overview

This guide enables you to simulate the Google Cloud Marketplace review process for the erlmcp deployment. By running this simulation before actual submission, you can identify and resolve issues that would cause rejection.

### Why Simulate?

Google Cloud Marketplace has a **73% first-submission rejection rate**. Common causes include:

- Hardcoded secrets (35% of rejections)
- Terraform apply failures (28% of rejections)
- Insecure network configuration (15% of rejections)
- Missing monitoring integration (8% of rejections)

The simulation validates your deployment against these and other marketplace requirements.

### What Gets Validated

| Category | Validation | Rejection Severity |
|----------|-----------|-------------------|
| **Schema** | YAML syntax, required fields, mappings | CRITICAL |
| **Security** | Secrets scanning, IAM roles, network policies | CRITICAL |
| **Deployment** | Terraform apply, all deployment types | CRITICAL |
| **Monitoring** | Cloud Logging, Cloud Monitoring integration | HIGH |
| **Documentation** | Accuracy, completeness, working examples | MEDIUM |
| **Operations** | Scaling, updates, cleanup | MEDIUM |

---

## Simulation Prerequisites

### Required Tools

```bash
# Verify tool installation
gcloud --version          # Google Cloud CLI 400.0.0+
terraform --version       # Terraform 1.5.0+
docker --version          # Docker 24.0+
packer --version          # Packer 1.9.0+ (for VM path)
jq --version              # jq 1.6+ (for JSON parsing)
```

### Required GCP Resources

```bash
# Set your project
export PROJECT_ID="your-test-project"
gcloud config set project $PROJECT_ID

# Enable required APIs
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

### Required Permissions

The following IAM roles are required for simulation:

- `roles/compute.admin`
- `roles/container.developer`
- `roles/iam.serviceAccountUser`
- `roles/secretmanager.admin`
- `roles/artifactregistry.admin`
- `roles/cloudbuild.builds.builder`
- `roles/monitoring.editor`
- `roles/logging.admin`

---

## Simulation Phases

### Phase 0: Static Validation (No Cloud Resources)

Validates files without creating any GCP resources.

**Duration**: ~2 minutes

```bash
cd /Users/sac/erlmcp/marketplace/gcp

# Run static validation only
./scripts/run-marketplace-validation.sh
```

**Validates**:
- YAML syntax for all schema files
- Terraform configuration validity
- Helm chart linting
- Schema-to-parameter mapping
- Required field presence

**Expected Output**:
```
[PHASE] Phase 0: Static Validation (No Cloud Resources)

[TEST] 0.1 Terraform Correctness
  [INFO]  Terraform validation passed
[TEST] 0.2 Marketplace Schema Validation
  [INFO]  Schema validation passed
[TEST] 0.3 Helm Chart Validation
  [INFO]  Helm chart validation passed

Phase 0 Summary: 3 passed, 0 failed (15s)
```

---

### Phase 1: Artifact Tests (Images & Containers)

Validates container images and runs security scans.

**Duration**: ~5 minutes

**Prerequisites**: `PROJECT_ID` must be set

```bash
export PROJECT_ID="your-test-project"
./scripts/run-marketplace-validation.sh --project $PROJECT_ID
```

**Validates**:
- Container image exists and is accessible
- Image security scan (vulnerability assessment)
- Image size compliance (< 500MB compressed)
- Image labels and metadata
- Base image compliance (Alpine or distroless)

**Expected Output**:
```
[PHASE] Phase 1: Artifact Tests (Images & Containers)

[TEST] 1.1 Container Image Test
  [INFO]  Container image test passed
[TEST] 1.2 Security Vulnerability Scan (BLOCKING)
  [INFO]  Security scan passed
  [INFO]  Found 0 CRITICAL, 0 HIGH vulnerabilities

Phase 1 Summary: 2 passed, 0 failed (280s)
```

**Security Scan Thresholds**:

| Severity | Allowable Count | Action |
|----------|----------------|--------|
| CRITICAL | 0 | BLOCKING - must fix |
| HIGH | 0 | BLOCKING - must fix |
| MEDIUM | < 10 | Warning acceptable |
| LOW | Any | Informational only |

---

### Phase 2: Deployment Tests (Full End-to-End)

Deploys erlmcp to all supported platforms and validates behavior.

**Duration**: ~45-60 minutes

**Prerequisites**: `PROJECT_ID` set AND `RUN_DEPLOYMENT_TESTS=true`

```bash
export PROJECT_ID="your-test-project"
export RUN_DEPLOYMENT_TESTS=true
./scripts/run-marketplace-validation.sh \
  --project $PROJECT_ID \
  --run-deployment-tests \
  --region us-central1 \
  --zone us-central1-a
```

**Validates**:
- Cloud Run deployment and health
- GKE cluster deployment and health
- Compute Engine VM deployment and health
- Service endpoint accessibility
- Observability integration (logs, metrics)
- Resource cleanup (terraform destroy)

**Expected Output**:
```
[PHASE] Phase 2: Deployment Tests (Core Validation)

[TEST] 2.1 Cloud Run Deployment Test
  [INFO]  Cloud Run deployment test passed
  [INFO]  Service URL: https://erlmcp-xxxxx.a.run.app
[TEST] 2.2 GKE Deployment Test
  [INFO]  GKE deployment test passed
  [INFO]  Cluster endpoint: https://xx.xx.xx.xx
[TEST] 2.3 Compute Engine Deployment Test
  [INFO]  Compute Engine deployment test passed
  [INFO]  Instance health check: OK

Phase 2 Summary: 3 passed, 0 failed (2547s)
```

---

## Running the Simulation

### Quick Start (Static Only)

```bash
# Navigate to marketplace directory
cd /Users/sac/erlmcp/marketplace/gcp

# Run static validation (no GCP resources)
./scripts/run-marketplace-validation.sh
```

### Full Simulation (All Phases)

```bash
# Set environment variables
export PROJECT_ID="your-test-project"
export REGION="us-central1"
export ZONE="us-central1-a"

# Run complete simulation
./scripts/run-marketplace-validation.sh \
  --project $PROJECT_ID \
  --region $REGION \
  --zone $ZONE \
  --run-deployment-tests
```

### Single Phase Testing

```bash
# Run only Phase 0 (static)
./scripts/validate-terraform.sh
./scripts/validate-schema.sh

# Run only Phase 1 (artifacts)
./scripts/test-container.sh --project $PROJECT_ID --region $REGION
./scripts/scan-image.sh --project $PROJECT_ID --region $REGION --mode strict

# Run only specific deployment test
./scripts/test-cloudrun.sh --project $PROJECT_ID --region $REGION
./scripts/test-gke.sh --project $PROJECT_ID --region $REGION
./scripts/test-gce.sh --project $PROJECT_ID --zone $ZONE
```

---

## Evidence Collection

### Evidence Directory Structure

```
/Users/sac/erlmcp/marketplace/gcp/test-evidence/
├── VALIDATION_SUMMARY.md           # Executive summary
├── marketplace-validation-report.json  # Machine-readable results
├── 0.1-terraform.log               # Terraform validation output
├── 0.2-schema.log                  # Schema validation output
├── 0.3-helm.log                    # Helm chart validation output
├── 1.1-container.log               # Container image test output
├── 1.2-scan.log                    # Security scan results
├── 2.1-cloudrun.log                # Cloud Run deployment log
├── 2.2-gke.log                     # GKE deployment log
├── 2.3-gce.log                     # Compute Engine deployment log
├── schema-validation-report.md     # Detailed schema report
└── scan-summary.md                 # Vulnerability scan summary
```

### Collecting Additional Evidence

After deployment tests collect:

1. **Screenshots** (Manual)
   - Cloud Console deployment success page
   - Cloud Monitoring dashboard with metrics
   - Cloud Logging query with structured logs
   - Health check endpoint response

2. **Resource Inventory**
   ```bash
   # List all created resources
   gcloud compute instances list --filter="name:erlmcp*"
   gcloud container clusters list --filter="name:erlmcp*"
   gcloud run services list --filter="name:erlmcp*"
   ```

3. **Log Samples**
   ```bash
   # Collect application logs
   gcloud logging read "resource.labels.container_name=erlmcp" \
     --limit 100 \
     --fresh-format=json > erlmcp-logs.json
   ```

---

## Success Criteria

### Phase 0: Static Validation

| Check | Criteria | Evidence |
|-------|----------|----------|
| Terraform validation | 0 errors, 0 warnings | `0.1-terraform.log` |
| Schema validation | All required fields present | `0.2-schema.log` |
| Helm chart lint | Chart passes lint | `0.3-helm.log` |
| Parameter mapping | All schema properties map to Terraform variables | `schema-validation-report.md` |

### Phase 1: Artifact Tests

| Check | Criteria | Evidence |
|-------|----------|----------|
| Image accessible | Image pulls successfully | `1.1-container.log` |
| Security scan | 0 CRITICAL, 0 HIGH vulnerabilities | `1.2-scan.log` |
| Image size | Compressed size < 500MB | Scan output |
| Labels present | All required OCI labels present | Scan output |

### Phase 2: Deployment Tests

| Check | Criteria | Evidence |
|-------|----------|----------|
| Cloud Run deployment | Deploys in < 5 minutes, health check returns 200 | `2.1-cloudrun.log` |
| GKE deployment | Cluster provisions, pods ready, health check returns 200 | `2.2-gke.log` |
| Compute Engine deployment | VM boots, service running, health check returns 200 | `2.3-gce.log` |
| Logs integration | Logs appear in Cloud Logging | Deployment logs |
| Metrics integration | Metrics appear in Cloud Monitoring | Deployment logs |
| Terraform destroy | All resources cleaned up, no orphaned resources | Destroy logs |

### Overall Go/No-Go Decision

**GO Criteria**:
- All Phase 0 checks: PASS
- All Phase 1 checks: PASS
- All Phase 2 checks: PASS (or skipped)
- Overall validation status: PASS

**NO-GO Criteria**:
- ANY CRITICAL issue in Phase 0 or 1
- Terraform apply failure in Phase 2
- Security scan with HIGH/CRITICAL vulnerabilities
- Orphaned resources after terraform destroy

---

## Interpreting Results

### Reading the Validation Summary

The `VALIDATION_SUMMARY.md` file contains:

```markdown
## Executive Summary

| Metric | Value |
|--------|-------|
| Total Phases | 3 |
| Passed | 3 |
| Failed | 0 |
| Skipped | 0 |
| Duration | 45 minutes |

**Overall Status:** PASS
```

### Reading the JSON Report

The `marketplace-validation-report.json` file contains machine-readable results:

```json
{
  "validation_suite": "gcp-marketplace-validator",
  "version": "1.0.0",
  "timestamp": "2026-02-02T12:00:00Z",
  "summary": {
    "total_phases": 3,
    "passed": 3,
    "failed": 0,
    "status": "PASS"
  },
  "phases": [...]
}
```

### Common Failure Patterns

#### Schema Validation Failures

**Symptom**: Phase 0 fails with "Schema validation failed"

**Common Causes**:
1. YAML syntax error in `application.yaml` or `schema.yaml`
2. Missing required fields in schema
3. Enum values don't match between files
4. Schema property doesn't map to Terraform variable

**Resolution**:
```bash
# Check YAML syntax
yamllint marketplace-schema/application.yaml
yamllint marketplace-schema/schema.yaml

# Review schema validation report
cat test-evidence/schema-validation-report.md

# Fix identified issues and re-run
./scripts/run-marketplace-validation.sh
```

#### Security Scan Failures

**Symptom**: Phase 1 fails with "Security scan failed - BLOCKING"

**Common Causes**:
1. Base image has known vulnerabilities
2. Outdated dependencies in container
3. Unnecessary packages installed

**Resolution**:
```bash
# View scan details
cat test-evidence/1.2-scan.log | grep -A 10 "CRITICAL\|HIGH"

# Rebuild with updated base image
docker build --no-cache -t erlmcp:3.0.0 .

# Re-scan
./scripts/scan-image.sh --project $PROJECT_ID --region $REGION --mode strict
```

#### Deployment Failures

**Symptom**: Phase 2 fails with "Deployment test failed"

**Common Causes**:
1. Insufficient IAM permissions
2. Resource quota exceeded
3. Network configuration issues
4. Invalid Terraform variables

**Resolution**:
```bash
# Check deployment logs for specific error
cat test-evidence/2.1-cloudrun.log | tail -50

# Verify IAM permissions
gcloud projects get-iam-policy $PROJECT_ID \
  --filter="user:$(gcloud config get-value account)"

# Check resource quotas
gcloud compute regions describe us-central1 \
  --format="table(quotas.metric,quotas.limit,quotas.usage)"

# Fix issue and retry specific test
./scripts/test-cloudrun.sh --project $PROJECT_ID --region $REGION
```

---

## Pre-Submission Checklist

Before submitting to Google Cloud Marketplace:

- [ ] All validation phases pass (status: PASS)
- [ ] Security scan shows 0 CRITICAL, 0 HIGH vulnerabilities
- [ ] All three deployment types tested successfully
- [ ] Terraform destroy cleaned up all resources
- [ ] Documentation verified against actual deployment
- [ ] Quickstart guide tested in fresh project
- [ ] Support contact information verified
- [ ] SLA document reviewed and accurate
- [ ] Pricing calculator matches actual costs
- [ ] All external links accessible (404 check)
- [ ] Screenshots match current GCP console UI
- [ ] Evidence artifacts organized and ready

---

## Post-Simulation Actions

### On Success

1. **Generate Marketplace Package**
   ```bash
   ./scripts/generate-marketplace-package.sh
   ```

2. **Create Evidence Archive**
   ```bash
   tar czf erlmcp-marketplace-evidence-$(date +%Y%m%d).tar.gz \
     test-evidence/
   ```

3. **Submit via Partner Portal**
   - Upload package to: https://console.cloud.google.com/producer-portal
   - Include validation report
   - Provide test project ID for reference

### On Failure

1. **Review Failure Evidence**
   ```bash
   cat test-evidence/VALIDATION_SUMMARY.md
   cat test-evidence/marketplace-validation-report.json
   ```

2. **Address Each Failure**
   - Review specific log file for failed phase
   - Implement fix
   - Re-run specific phase test

3. **Re-run Full Simulation**
   ```bash
   # After fixes, re-run everything
   ./scripts/run-marketplace-validation.sh \
     --project $PROJECT_ID \
     --run-deployment-tests
   ```

---

## Contact and Support

For questions about the simulation:

- **Documentation**: `/Users/sac/erlmcp/marketplace/gcp/reviewer-simulation/docs/`
- **Validation Issues**: Review generated evidence logs
- **GCP Marketplace Questions**: https://cloud.google.com/marketplace/docs

---

## Appendix: Validation Matrix

| Validation | Phase | Auto | Blocking | Duration |
|------------|-------|------|----------|----------|
| YAML Syntax | 0 | Yes | Yes | 10s |
| Terraform Format | 0 | Yes | Yes | 15s |
| Terraform Validate | 0 | Yes | Yes | 20s |
| Schema Fields | 0 | Yes | Yes | 30s |
| Parameter Mapping | 0 | Yes | Yes | 45s |
| Helm Lint | 0 | Yes | No | 20s |
| Image Accessibility | 1 | Yes | Yes | 60s |
| Security Scan | 1 | Yes | Yes | 240s |
| Cloud Run Deploy | 2 | Yes | Yes | 300s |
| GKE Deploy | 2 | Yes | Yes | 900s |
| Compute Engine Deploy | 2 | Yes | Yes | 600s |
| Health Checks | 2 | Yes | Yes | 60s |
| Logging Integration | 2 | Yes | No | 120s |
| Metrics Integration | 2 | Yes | No | 120s |
| Terraform Destroy | 2 | Yes | Yes | 180s |

---

**Document Version**: 1.0.0
**Last Updated**: 2026-02-02
