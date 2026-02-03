# GCP Marketplace Reviewer Simulation - CI Pipeline

**Version:** 1.0.0
**Last Updated:** 2026-02-02
**Purpose:** Automated CI pipeline for Marketplace reviewer simulation

---

## Executive Summary

This document describes the automated CI pipeline that simulates the Google Cloud Marketplace reviewer validation process. The pipeline validates that an erlmcp deployment meets all Marketplace requirements before submission.

### Pipeline Location
`/Users/sac/erlmcp/marketplace/gcp/cloudbuild/reviewer-simulation-pipeline.yaml`

---

## Table of Contents

1. [Overview](#1-overview)
2. [Pipeline Stages](#2-pipeline-stages)
3. [Usage](#3-usage)
4. [Evidence Collection](#4-evidence-collection)
5. [Report Generation](#5-report-generation)
6. [Troubleshooting](#6-troubleshooting)

---

## 1. Overview

### Purpose

The Google Cloud Marketplace reviewer simulation pipeline automates the validation process that Google's reviewers perform when evaluating a Marketplace solution. This ensures that deployments will pass the review process before submission.

### Key Features

- **Fully Automated**: No manual intervention required
- **Comprehensive Validation**: Tests all deployment paths (GKE, Cloud Run, Compute Engine)
- **Evidence Collection**: Automatically collects and organizes all test evidence
- **Security Scanning**: Includes vulnerability scanning and IAM audit
- **Cleanup Verification**: Ensures all test resources are properly destroyed
- **Report Generation**: Produces detailed reports for review

### What Google Reviewers Check

| Check | Description | Pipeline Stage |
|-------|-------------|----------------|
| Automatic Deployment | Deploys without manual SSH or intervention | Stage 3, 4, 5 |
| Health Endpoints | Service health check responds correctly | Stage 3, 4, 5 |
| Observability | Logs, metrics, and traces available | Stage 7 |
| Security | No hardcoded secrets, proper IAM | Stage 8 |
| Documentation | Required docs present and accurate | Stage 2 |
| Cleanup | Terraform destroy removes all resources | Stage 9 |

---

## 2. Pipeline Stages

### Stage 0: Setup

**Duration:** ~1 minute

Creates the test environment and initializes evidence collection.

**Actions:**
- Creates evidence directory structure
- Initializes test tracking
- Configures gcloud settings
- Records build metadata

**Evidence Location:** `/workspace/reviewer-evidence/stage-0-setup/`

### Stage 1: Prerequisites and API Enablement

**Duration:** ~5 minutes

Enables all required Google Cloud APIs and verifies quotas.

**Actions:**
- Enables required APIs (container, run, compute, secretmanager, etc.)
- Verifies API enablement
- Checks resource quotas
- Validates GKE, Cloud Run, and Compute Engine availability

**Required APIs:**
```
- container.googleapis.com
- run.googleapis.com
- compute.googleapis.com
- secretmanager.googleapis.com
- logging.googleapis.com
- monitoring.googleapis.com
- cloudtrace.googleapis.com
- cloudkms.googleapis.com
- artifactregistry.googleapis.com
```

**Evidence Location:** `/workspace/reviewer-evidence/stage-1-prerequisites/`

### Stage 2: Schema and Metadata Validation

**Duration:** ~3 minutes

Validates all Marketplace schema and metadata files.

**Actions:**
- Validates `application.yaml` for required fields
- Validates `schema.yaml` for completeness
- Checks Terraform modules for required files
- Verifies documentation presence

**Required Fields in application.yaml:**
```yaml
metadata.name
metadata.displayName
metadata.description
metadata.version
spec.runtimePolicy.minDeploymentDuration
spec.inputSchema
```

**Required Files per Terraform Module:**
```
main.tf
variables.tf
outputs.tf
```

**Evidence Location:** `/workspace/reviewer-evidence/stage-2-schema/`

### Stage 3: VM Deployment and Tests

**Duration:** ~10 minutes

Tests Compute Engine VM deployment (if applicable).

**Actions:**
- Creates test VM instance
- Verifies VM boots successfully
- Tests SSH connectivity
- Validates startup script execution
- Checks firewall rules
- Captures serial port output

**VM Configuration:**
```
Machine Type: e2-medium (test)
Image: debian-12
Disk: 10GB
Network: Default with access config
```

**Evidence Location:** `/workspace/reviewer-evidence/stage-3-vm/`

**Key Files:**
- `create-output.json` - VM creation output
- `deployment.log` - Full deployment log
- `external-ip.txt` - External IP address
- `serial-output.txt` - VM serial console output
- `vm-details.json` - Complete VM details

### Stage 4: GKE Deployment and Tests

**Duration:** ~20 minutes

Tests Google Kubernetes Engine deployment (most scrutinized by reviewers).

**Actions:**
- Creates regional GKE cluster (3 zones)
- Configures Workload Identity
- Enables Shielded Nodes
- Deploys test workload
- Verifies pod security contexts
- Tests network policies
- Validates autoscaling

**GKE Configuration:**
```
Nodes: 1 (test configuration)
Zones: 3 (regional)
Image: cos_containerd
Shielded Nodes: Enabled
Workload Identity: Enabled
Network: VPC-native with alias IPs
```

**Evidence Location:** `/workspace/reviewer-evidence/stage-4-gke/`

**Key Files:**
- `create-output.json` - Cluster creation output
- `deployment.log` - Full deployment log
- `cluster-details.json` - Complete cluster details
- `network-policies.txt` - Network policy status

### Stage 5: Cloud Run Deployment and Tests

**Duration:** ~5 minutes

Tests Cloud Run serverless deployment.

**Actions:**
- Deploys Cloud Run service
- Tests health endpoint
- Measures cold start time
- Validates auto-scaling
- Tests concurrent request handling

**Cloud Run Configuration:**
```
Memory: 512Mi
CPU: 1
Min Instances: 0
Max Instances: 10
Timeout: 300s
Concurrency: 80
```

**Evidence Location:** `/workspace/reviewer-evidence/stage-5-cloudrun/`

**Key Files:**
- `create-output.json` - Service creation output
- `service-url.txt` - Service URL
- `health-response.txt` - Health check response
- `service-details.json` - Complete service details

### Stage 6: Secrets Rotation Test

**Duration:** ~2 minutes

Tests Secret Manager integration and secret rotation.

**Actions:**
- Creates test secret (version 1)
- Adds new version (version 2)
- Verifies latest version access
- Tests IAM permissions
- Validates rotation workflow

**Secret Lifecycle:**
```
1. Create secret with initial value
2. Access version 1 (verify)
3. Add version 2 (rotate)
4. Access latest (should be version 2)
5. Grant IAM permissions
6. Test access with SA
```

**Evidence Location:** `/workspace/reviewer-evidence/stage-6-secrets/`

**Key Files:**
- `rotation-test.log` - Full rotation test log
- `secret-details.json` - Secret metadata
- `secret-iam-policy.txt` - IAM policy

### Stage 7: Observability Verification

**Duration:** ~3 minutes

Verifies logging, monitoring, and tracing integration.

**Actions:**
- Generates test traffic
- Verifies Cloud Logging ingestion
- Validates Cloud Monitoring metrics
- Checks Cloud Trace data
- Lists dashboards and alert policies

**Observability Checks:**
```
Cloud Logging:
- Log ingestion working
- Correct log format
- Proper severity levels

Cloud Monitoring:
- Metrics being collected
- Custom metrics visible
- Dashboards available

Cloud Trace:
- Trace data being collected
- Span data available
```

**Evidence Location:** `/workspace/reviewer-evidence/stage-7-observability/`

**Key Files:**
- `traffic-test.log` - Traffic generation log
- `logs-output.txt` - Cloud Logging output
- `metrics-list.txt` - Available metrics
- `traces.txt` - Trace data

### Stage 8: Security and IAM Audit

**Duration:** ~3 minutes

Performs comprehensive security audit.

**Actions:**
- Audits IAM policies
- Checks for overly permissive roles
- Scans container image for vulnerabilities
- Reviews firewall rules
- Audits Secret Manager access
- Calculates security score

**Security Score Calculation:**
```
Base Score: 100
Deductions:
- Owner roles > 2: -20
- Editor roles > 5: -10
- Open firewall rules: -15
- HIGH/CRITICAL vulnerabilities: -30
```

**Evidence Location:** `/workspace/reviewer-evidence/stage-8-security/`

**Key Files:**
- `security-audit.log` - Full audit log
- `iam-policy.json` - Complete IAM policy
- `firewall-rules.txt` - Firewall rules
- `vulnerability-scan.txt` - Trivy scan results
- `security-score.txt` - Final security score

### Stage 9: Destruction Test

**Duration:** ~5 minutes

Verifies that all test resources can be destroyed.

**Actions:**
- Deletes Cloud Run service
- Deletes GKE cluster
- Deletes Compute Engine instances
- Deletes test secrets
- Verifies no orphaned resources

**Cleanup Verification:**
```
Remaining VMs: 0
Remaining Clusters: 0
Remaining Services: 0
Remaining Secrets: 0
```

**Evidence Location:** `/workspace/reviewer-evidence/stage-9-destruction/`

**Key Files:**
- `cleanup.log` - Full cleanup log
- `cleanup-status.txt` - Cleanup status (success/partial/failed)

### Stage 10: Final Report Generation

**Duration:** ~1 minute

Generates comprehensive final report.

**Actions:**
- Aggregates all stage results
- Calculates overall status
- Generates Markdown report
- Generates JSON report
- Uploads evidence to GCS

**Report Sections:**
```
1. Executive Summary
2. Stage Results Table
3. Detailed Findings
4. Marketplace Readiness Checklist
5. Security Audit Results
6. Deployment Results
7. Observability Results
8. Recommendations
9. Final Verdict
10. Next Steps
```

**Evidence Location:** `/workspace/reviewer-evidence/stage-10-report/`

**Key Files:**
- `FINAL_REPORT.md` - Complete Markdown report
- `final-report.json` - Machine-readable JSON report

---

## 3. Usage

### Prerequisites

1. **Google Cloud Project:** Test or staging project
2. **Required APIs:** All listed in Stage 1
3. **Permissions:** Project Editor or Owner
4. **Quotas:** Sufficient quota for test resources

### Running the Pipeline

#### Option 1: Manual Trigger

```bash
gcloud builds submit \
  --config marketplace/gcp/cloudbuild/reviewer-simulation-pipeline.yaml \
  --substitutions=_PROJECT_ID=my-test-project,_REGION=us-central1
```

#### Option 2: Cloud Build Trigger

Create a trigger in Google Cloud Console:
1. Go to Cloud Build > Triggers
2. Create new trigger
3. Select configuration: `marketplace/gcp/cloudbuild/reviewer-simulation-pipeline.yaml`
4. Set substitutions for project and region
5. Trigger manually or on git tag

#### Option 3: Scheduled Run

```bash
# Create scheduled trigger (e.g., weekly)
gcloud builds triggers create manual \
  --name="weekly-marketplace-review" \
  --region=us-central1 \
  --build-config=marketplace/gcp/cloudbuild/reviewer-simulation-pipeline.yaml \
  --substitutions=_PROJECT_ID=my-test-project,_REGION=us-central1
```

### Substitutions

| Substitution | Default | Description |
|--------------|---------|-------------|
| `_PROJECT_ID` | ${PROJECT_ID} | GCP Project ID |
| `_REGION` | us-central1 | GCP Region |
| `_ZONE` | us-central1-a | GCP Zone |
| `_TEST_ID` | auto-generated | Test identifier |
| `_CLEANUP_ON_FAILURE` | true | Cleanup resources on failure |
| `_SCREENSHOT_ENABLED` | false | Capture screenshots (requires setup) |

### Expected Duration

| Stage | Duration |
|-------|----------|
| Stage 0: Setup | 1 min |
| Stage 1: Prerequisites | 5 min |
| Stage 2: Schema | 3 min |
| Stage 3: VM | 10 min |
| Stage 4: GKE | 20 min |
| Stage 5: Cloud Run | 5 min |
| Stage 6: Secrets | 2 min |
| Stage 7: Observability | 3 min |
| Stage 8: Security | 3 min |
| Stage 9: Destruction | 5 min |
| Stage 10: Report | 1 min |
| **Total** | **~58 min** |

---

## 4. Evidence Collection

### Evidence Directory Structure

```
/workspace/reviewer-evidence/
├── test-tracker.json              # Test execution metadata
├── stage-0-setup/
├── stage-1-prerequisites/
│   ├── api-status.txt
│   ├── quota-check.txt
│   ├── gke-status.txt
│   └── cloudrun-status.txt
├── stage-2-schema/
│   ├── validation.log
│   ├── documentation-check.txt
│   └── schema-keys.txt
├── stage-3-vm/
│   ├── deployment.log
│   ├── create-output.json
│   ├── external-ip.txt
│   ├── serial-output.txt
│   └── vm-details.json
├── stage-4-gke/
│   ├── deployment.log
│   ├── create-output.json
│   ├── cluster-details.json
│   └── network-policies.txt
├── stage-5-cloudrun/
│   ├── deployment.log
│   ├── create-output.json
│   ├── service-url.txt
│   ├── health-response.txt
│   └── service-details.json
├── stage-6-secrets/
│   ├── rotation-test.log
│   ├── secret-details.json
│   └── secret-iam-policy.txt
├── stage-7-observability/
│   ├── traffic-test.log
│   ├── logging-test.log
│   ├── logs-output.txt
│   ├── metrics-list.txt
│   └── traces.txt
├── stage-8-security/
│   ├── security-audit.log
│   ├── iam-policy.json
│   ├── firewall-rules.txt
│   ├── vulnerability-scan.txt
│   └── security-score.txt
├── stage-9-destruction/
│   ├── cleanup.log
│   └── cleanup-status.txt
└── stage-10-report/
    ├── FINAL_REPORT.md
    └── final-report.json
```

### Evidence Upload

All evidence is automatically uploaded to Cloud Storage:

```
gs://$_PROJECT_ID-marketplace-evidence/reviewer-simulation-$BUILD_ID/
```

### Accessing Evidence

```bash
# List evidence files
gsutil ls gs://$_PROJECT_ID-marketplace-evidence/reviewer-simulation-$BUILD_ID/

# Download full evidence package
gsutil -m cp -r \
  gs://$_PROJECT_ID-marketplace-evidence/reviewer-simulation-$BUILD_ID/* \
  ./evidence/

# View final report
gsutil cat \
  gs://$_PROJECT_ID-marketplace-evidence/reviewer-simulation-$BUILD_ID/stage-10-report/FINAL_REPORT.md
```

---

## 5. Report Generation

### Final Report Format

The final report includes:

#### Executive Summary
- Overall status (PASS/FAIL)
- Security score
- Cleanup status
- Total duration
- Stage results table

#### Detailed Findings
- Schema validation results
- Security audit results
- Deployment results for each platform
- Observability verification
- Secrets rotation test

#### Marketplace Readiness Checklist

Required checks (all must pass):
- [ ] Deploys without manual intervention
- [ ] Health endpoint accessible
- [ ] Terraform destroy works
- [ ] No hardcoded secrets
- [ ] Schema complete and valid

Recommended checks:
- [ ] Logging configured
- [ ] Monitoring configured
- [ ] Security score acceptable
- [ ] Documentation present

#### Final Verdict
- Overall status
- Ready for submission (yes/no)
- Next steps
- Evidence package location

### Report Interpretation

| Status | Meaning | Action |
|--------|---------|--------|
| PASS | All checks passed | Ready for submission |
| FAIL | One or more checks failed | Address issues, re-run |
| PARTIAL | Some warnings | Review warnings, may submit |

---

## 6. Troubleshooting

### Common Issues

#### Issue: API Enablement Timeout

**Symptoms:** Stage 1 fails with API enablement errors

**Resolution:**
```bash
# Manually enable APIs
gcloud services enable \
  container.googleapis.com \
  run.googleapis.com \
  compute.googleapis.com \
  --project=$_PROJECT_ID
```

#### Issue: Insufficient Quota

**Symptoms:** Stage 3, 4, or 5 fails with quota errors

**Resolution:**
```bash
# Check quotas
gcloud compute regions describe $_REGION \
  --project=$_PROJECT_ID \
  --format="table(quotas.metric,quotas.limit,quotas.usage)"

# Request quota increase in Cloud Console
```

#### Issue: Cleanup Failure

**Symptoms:** Stage 9 reports remaining resources

**Resolution:**
```bash
# Manual cleanup
gcloud compute instances list \
  --project=$_PROJECT_ID \
  --filter="name:reviewer-sim-*" \
  --format="value(name)" | xargs -I {} gcloud compute instances delete {} \
    --project=$_PROJECT_ID \
    --zone=$_ZONE \
    --quiet

gcloud container clusters list \
  --project=$_PROJECT_ID \
  --filter="name:reviewer-sim-*" \
  --format="value(name)" | xargs -I {} gcloud container clusters delete {} \
    --project=$_PROJECT_ID \
    --region=$_REGION \
    --quiet

gcloud run services list \
  --project=$_PROJECT_ID \
  --filter="name:reviewer-sim-*" \
  --format="value(name)" | xargs -I {} gcloud run services delete {} \
    --project=$_PROJECT_ID \
    --region=$_REGION \
    --quiet
```

#### Issue: Security Score Low

**Symptoms:** Stage 8 reports low security score

**Resolution:**
- Review security audit log: `stage-8-security/security-audit.log`
- Reduce Owner/Editor role assignments
- Limit firewall rule scope
- Update base image to fix vulnerabilities

#### Issue: Build Timeout

**Symptoms:** Pipeline times out after 2 hours

**Resolution:**
- GKE cluster creation can take 15-20 minutes
- Total pipeline is ~60 minutes normally
- Check for stuck operations in Cloud Console
- Consider running stages individually for debugging

### Debug Mode

To run individual stages:

```bash
# Stage 1 only
gcloud builds submit \
  --config marketplace/gcp/cloudbuild/reviewer-simulation-pipeline.yaml \
  --substitutions=_PROJECT_ID=$PROJECT_ID \
  --dry-run=false
```

Then modify the pipeline to only run specific stages by adding `waitFor` dependencies.

### Logs and Debugging

```bash
# View build logs
gcloud builds log $BUILD_ID

# Stream live logs
gcloud builds stream-logs $BUILD_ID

# View specific step logs
gcloud builds log $BUILD_ID --step-id=stage-4-gke-deployment
```

---

## Appendix A: Integration with Other Pipelines

This pipeline integrates with:

1. **Main Marketplace Pipeline** (`cloudbuild-marketplace-enhanced.yaml`)
   - Runs after main pipeline succeeds
   - Uses same image built in main pipeline

2. **Validation Pipeline** (`run-marketplace-validation.sh`)
   - Shares validation scripts
   - Uses same evidence directory structure

3. **Deployment Test Scripts**
   - `test-cloudrun.sh`
   - `test-gke.sh`
   - `test-gce.sh`

---

## Appendix B: Best Practices

1. **Run Weekly:** Schedule weekly runs before submission
2. **Review Reports:** Always review the full report
3. **Keep Score High:** Maintain security score > 80
4. **Clean Resources:** Always verify cleanup succeeds
5. **Document Changes:** Update pipeline when deployment changes

---

## Appendix C: Support

For issues or questions:
- GitHub: https://github.com/banyan-platform/erlmcp/issues
- Documentation: https://docs.erlmcp.dev

---

*Document Version: 1.0.0*
*Last Updated: 2026-02-02*
