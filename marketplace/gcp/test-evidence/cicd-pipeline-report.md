# GCP Marketplace CI/CD Pipeline Design Report

**Generated:** 2026-02-02T21:04:00Z
**Project:** erlmcp GCP Marketplace Deployment
**Version:** 3.0.0

---

## Executive Summary

This document outlines a complete CI/CD pipeline design for automating GCP Marketplace validation of the erlmcp solution. The pipeline integrates with the existing `cloudbuild-marketplace.yaml` and extends it with multi-stage execution, comprehensive security scanning, automated testing, and notification capabilities.

### Key Deliverables

1. **Enhanced Cloud Build Configuration** - Multi-stage pipeline with 5 distinct stages
2. **Pipeline Documentation** - Complete operational runbook
3. **Secret and IAM Requirements** - Security configurations
4. **Notification Strategy** - Slack/Email integration for failures and success summaries

---

## Table of Contents

1. [Pipeline Architecture](#1-pipeline-architecture)
2. [Stage Definitions](#2-stage-definitions)
3. [Security Integration](#3-security-integration)
4. [Notification Strategy](#4-notification-strategy)
5. [Trigger Configuration](#5-trigger-configuration)
6. [IAM and Secret Requirements](#6-iam-and-secret-requirements)
7. [Pipeline Runbook](#7-pipeline-runbook)
8. [Evidence Collection](#8-evidence-collection)

---

## 1. Pipeline Architecture

### 1.1 High-Level Design

```
                    +-------------------------+
                    |   Trigger Event         |
                    | (Push, Tag, Manual)     |
                    +-----------+-------------+
                                |
                                v
                    +-------------------------+
                    |   Stage 1: Build        |
                    |   - Compile Docker      |
                    |   - Tag Images          |
                    +-----------+-------------+
                                |
                                v
                    +-------------------------+
                    |   Stage 2: Security     |
                    |   - Trivy Scan          |
                    |   - GCP Container Scan  |
                    |   - Secret Scan         |
                    +-----------+-------------+
                                |
                                v
                    +-------------------------+
                    |   Stage 3: Validate     |
                    |   - Terraform           |
                    |   - Marketplace Schema  |
                    |   - Helm Charts         |
                    +-----------+-------------+
                                |
                                v
                    +-------------------------+
                    |   Stage 4: Deploy Test  |
                    |   - Cloud Run Test      |
                    |   - GKE Test            |
                    |   - Auto Cleanup        |
                    +-----------+-------------+
                                |
                                v
                    +-------------------------+
                    |   Stage 5: Evidence     |
                    |   - Collect Reports     |
                    |   - Upload to GCS       |
                    |   - Send Notifications  |
                    +-------------------------+
```

### 1.2 Pipeline Configuration File Structure

```
marketplace/gcp/
├── cloudbuild/
│   ├── cloudbuild-marketplace.yaml       # Main pipeline (enhanced)
│   ├── cloudbuild-pr.yaml                 # PR validation pipeline
│   └── cloudbuild-release.yaml            # Release pipeline
├── cloudbuild-marketplace.yaml            # Legacy (to be replaced)
└── test-evidence/
    └── cicd-pipeline-report.md            # This document
```

### 1.3 Integration with Existing Pipeline

The enhanced pipeline integrates with:
- **Test Scripts:** `marketplace/gcp/scripts/test-*.sh`
- **Validation Scripts:** `marketplace/gcp/scripts/validate-*.sh`
- **Marketplace Schema:** `marketplace/gcp/marketplace-schema/`
- **Terraform Modules:** `marketplace/gcp/terraform/modules/`

---

## 2. Stage Definitions

### 2.1 Stage 1: Build and Push Container Image

**Purpose:** Build the container image and push to Artifact Registry

**Steps:**
1. Checkout source code
2. Configure Docker buildx for multi-platform builds
3. Build image with build arguments (VERSION, VCS_REF, BUILD_DATE)
4. Tag image with SHA, branch, and latest tags
5. Push to Artifact Registry

**Duration:** ~5-10 minutes

**Dependencies:** None

**Artifacts:**
- Container image digest
- Image manifest
- Build metadata

```yaml
- id: 'build'
  name: 'gcr.io/cloud-builders/docker'
  entrypoint: 'bash'
  args:
    - '-c'
    - |
      docker buildx create --use
      docker buildx build \
        --platform linux/amd64,linux/arm64 \
        -f Dockerfile \
        -t $_REGION-docker.pkg.dev/$_PROJECT_ID/$_IMAGE_NAME:$_IMAGE_TAG \
        -t $_REGION-docker.pkg.dev/$_PROJECT_ID/$_IMAGE_NAME:latest \
        --build-arg BUILD_DATE=$(date -u +"%Y-%m-%dT%H:%M:%SZ") \
        --build-arg VCS_REF=$SHORT_SHA \
        --build-arg VERSION=$_IMAGE_TAG \
        --push \
        .
  timeout: '1200s'
```

### 2.2 Stage 2: Security Vulnerability Scanning

**Purpose:** Comprehensive security scanning of container image

**Steps:**
1. **Trivy Scan** - Fast vulnerability scanning
2. **GCP Container Analysis** - Deep vulnerability scanning with CVE details
3. **Secret Scanning** - Detect hardcoded secrets
4. **SBOM Generation** - Software Bill of Materials

**Duration:** ~10-15 minutes

**Dependencies:** Stage 1 (build)

**Blocking:** Yes - FAILS on HIGH/CRITICAL vulnerabilities in strict mode

```yaml
- id: 'trivy-scan'
  name: 'aquasec/trivy:latest'
  entrypoint: '/bin/sh'
  args:
    - '-c'
    - |
      trivy image \
        --severity HIGH,CRITICAL \
        --exit-code 1 \
        --format json \
        --output trivy-report.json \
        $_REGION-docker.pkg.dev/$_PROJECT_ID/$_IMAGE_NAME:$_IMAGE_TAG
  timeout: '300s'

- id: 'gcp-container-scan'
  name: 'gcr.io/cloud-builders/gcloud'
  args:
    - 'artifacts'
    - 'docker'
    - 'images'
    - 'scan'
    - '$_REGION-docker.pkg.dev/$_PROJECT_ID/$_IMAGE_NAME:$_IMAGE_TAG'
    - '--location'
    - '$_REGION'
    - '--format'
    - 'json'
  timeout: '600s'

- id: 'secret-scan'
  name: 'gcr.io/cloud-builders/gcloud'
  entrypoint: 'bash'
  args:
    - '-c'
    - |
      # Scan for secrets
      git log --all --full-history -p | tr '[:upper:]' '[:lower:]' | \
        grep -E '(password|secret|api[_-]?key|token)' | \
        grep -v '^Binary file' > secret-scan.log || true

      if [ -s secret-scan.log ]; then
        echo "Potential secrets found in git history"
        cat secret-scan.log
        exit 1
      fi

- id: 'sbom-generation'
  name: 'anchore/syft:latest'
  args:
    - 'sbom'
    - '--output'
    - 'spdx-json'
    - '--file'
    - 'sbom.json'
    - '$_REGION-docker.pkg.dev/$_PROJECT_ID/$_IMAGE_NAME:$_IMAGE_TAG'
```

### 2.3 Stage 3: Static Validation

**Purpose:** Validate all infrastructure-as-code and marketplace schema

**Steps:**
1. **Terraform Validation** - All modules and examples
2. **Marketplace Schema Validation** - application.yaml, schema.yaml, parameters.yaml
3. **Helm Chart Linting** - Chart validation
4. **Packer Template Validation** - VM image templates

**Duration:** ~5 minutes

**Dependencies:** None (can run parallel with Stage 1)

```yaml
- id: 'terraform-validate'
  name: 'hashicorp/terraform:1.5'
  entrypoint: 'bash'
  args:
    - '-c'
    - |
      for dir in marketplace/gcp/terraform/modules/*/; do
        cd "$dir"
        terraform init -backend=false -input=false
        terraform fmt -check
        terraform validate
      done

- id: 'schema-validate'
  name: 'gcr.io/cloud-builders/docker'
  entrypoint: 'bash'
  args:
    - 'marketplace/gcp/scripts/validate-schema.sh'

- id: 'helm-lint'
  name: 'gcr.io/cloud-builders/docker'
  entrypoint: 'bash'
  args:
    - '-c'
    - |
      docker run --rm -v "$PWD:/work" -w /work \
        alpine/helm:3.12 lint marketplace/gcp/helm/erlmcp-marketplace

- id: 'packer-validate'
  name: 'hashicorp/packer:1.9'
  entrypoint: 'bash'
  args:
    - '-c'
    - |
      cd marketplace/gcp/packer
      packer init -no-color .
      packer validate -no-color .
```

### 2.4 Stage 4: Deployment Tests

**Purpose:** Automated deployment testing with cleanup

**Steps:**
1. **Cloud Run Deployment Test** - Fast test (~3 min)
2. **GKE Deployment Test** - Full cluster test (~10 min) - OPTIONAL
3. **Compute Engine Test** - VM deployment test (~5 min) - OPTIONAL
4. **Health Endpoint Validation** - Verify service health
5. **Automatic Cleanup** - Destroy test resources

**Duration:** ~20 minutes (with optional tests)

**Dependencies:** Stage 1 (build), Stage 2 (security), Stage 3 (validation)

**Cleanup:** Always runs (even on failure)

```yaml
- id: 'cloud-run-test'
  name: 'gcr.io/cloud-builders/gcloud'
  entrypoint: 'bash'
  args:
    - 'marketplace/gcp/scripts/test-cloudrun.sh'
    - '--project'
    - '$_PROJECT_ID'
    - '--region'
    - '$_REGION'
  timeout: '900s'

- id: 'cleanup-test-resources'
  name: 'gcr.io/cloud-builders/gcloud'
  entrypoint: 'bash'
  args:
    - '-c'
    - |
      # Always cleanup, even if tests fail
      echo "Cleaning up test resources..."
      ./marketplace/gcp/scripts/cleanup-test-resources.sh --project $_PROJECT_ID
  alwaysRun: true
```

### 2.5 Stage 5: Evidence Collection and Reporting

**Purpose:** Collect all evidence and generate reports

**Steps:**
1. **Aggregate Scan Results** - Combine security reports
2. **Generate Summary Report** - Markdown summary
3. **Upload to GCS** - Persistent artifact storage
4. **Send Notifications** - Slack/Email alerts
5. **Create Deployment Package** - Marketplace-ready package

**Duration:** ~2 minutes

**Dependencies:** All previous stages

```yaml
- id: 'collect-evidence'
  name: 'gcr.io/cloud-builders/gcloud'
  entrypoint: 'bash'
  args:
    - '-c'
    - |
      # Create evidence directory
      mkdir -p /workspace/marketplace-evidence/$BUILD_ID

      # Copy all reports
      cp trivy-report.json /workspace/marketplace-evidence/$BUILD_ID/
      cp sbom.json /workspace/marketplace-evidence/$BUILD_ID/
      cp -r marketplace/gcp/test-evidence/* /workspace/marketplace-evidence/$BUILD_ID/

      # Generate summary
      ./marketplace/gcp/scripts/generate-evidence-summary.sh \
        --build-id $BUILD_ID \
        --output /workspace/marketplace-evidence/$BUILD_ID/SUMMARY.md

- id: 'upload-artifacts'
  name: 'gcr.io/cloud-builders/gsutil'
  args:
    - '-m'
    - 'cp'
    - '-r'
    - '/workspace/marketplace-evidence/$BUILD_ID/*'
    - 'gs://$_PROJECT_ID-marketplace-evidence/$BUILD_ID/'

- id: 'send-notification'
  name: 'gcr.io/cloud-builders/gcloud'
  entrypoint: 'bash'
  args:
    - '-c'
    - |
      ./marketplace/gcp/scripts/notify-pipeline-result.sh \
        --status $BUILD_STATUS \
        --build-id $BUILD_ID \
        --project $_PROJECT_ID
```

---

## 3. Security Integration

### 3.1 Automated Vulnerability Scanning

| Tool | Purpose | Severity Threshold | Action |
|------|---------|-------------------|--------|
| Trivy | Fast vulnerability scanning | HIGH, CRITICAL | Block in strict mode |
| GCP Container Analysis | Deep CVE scanning | As per GCP policy | Warn only |
| Gosec | Security policy checking | HIGH | Block |
| Kube-Hunter | Kubernetes security testing | CRITICAL | Warn |

### 3.2 Secret Scanning

**Tools:**
- `gitleaks` - Git history scanning
- `trivy secret` - Container secret scanning
- Custom pattern matching for erlmcp-specific secrets

**Secret Patterns:**
```
- API keys: AIza[0-9A-Za-z\\-_]{35}
- OAuth tokens: ya29\\.[0-9A-Za-z\\-_]+
- Service account keys: "private_key": "-----BEGIN PRIVATE KEY-----
- Database URLs: postgresql://.*:.*@.*
- Redis URLs: redis://.*:.*@.*
```

### 3.3 IAM Validation

**Pre-deployment IAM checks:**
1. Validate least-privilege roles for service accounts
2. Check for overly permissive IAM bindings
3. Verify Service Account Email format
4. Validate resource-level permissions

**Tools:**
- `gcloud iam policies validate`
- Custom Terraform provider validation
- IAM Recommender API integration

### 3.4 Compliance Checks

**Standards:**
- CIS Benchmark for GCP
- NIST 800-53 controls
- SOC 2 requirements
- PCI DSS (if applicable)

**Implementation:**
```yaml
- id: 'compliance-check'
  name: 'gcr.io/cloud-builders/gcloud'
  entrypoint: 'bash'
  args:
    - '-c'
    - |
      ./marketplace/gcp/scripts/compliance-check.sh \
        --standards cis,nist-800-53,soc2 \
        --output compliance-report.json
```

---

## 4. Notification Strategy

### 4.1 Notification Channels

| Channel | Use Case | Trigger |
|---------|----------|---------|
| Slack | Real-time alerts | On failure, on success summary |
| Email | Audit trail | Always (summary) |
| Cloud Monitoring | Metrics | Build duration, success rate |
| Pub/Sub | Programmatic alerts | Critical failures |

### 4.2 Slack Notification Format

**Failure Notification:**
```
🔴 GCP Marketplace Build FAILED
Build: #$BUILD_ID
Branch: $BRANCH_NAME
Commit: $SHORT_SHA
Author: $COMMIT_AUTHOR

Stage Failed: $FAILED_STAGE
Error: $ERROR_MESSAGE

🔗 View Logs: $BUILD_LOG_URL
🔗 Fix It: $COMPARE_URL
```

**Success Notification:**
```
🟢 GCP Marketplace Build SUCCEEDED
Build: #$BUILD_ID
Branch: $BRANCH_NAME
Commit: $SHORT_SHA

Duration: $DURATION
Image: $_REGION-docker.pkg.dev/$_PROJECT_ID/$_IMAGE_NAME:$_IMAGE_TAG

Summary:
- Security: ✓ No HIGH/CRITICAL vulnerabilities
- Validation: ✓ All checks passed
- Tests: ✓ All tests passed

🔗 View Evidence: $EVIDENCE_URL
```

### 4.3 Notification Configuration

```yaml
# Cloud Build notification configuration
notifications:
  slack:
    webhookUrlSecret: slack-webhook-url
    channel: '#gcp-marketplace-builds'
    notifyOn:
      - FAILURE
      - SUCCESS

  email:
    recipients:
      - marketplace-team@example.com
      - devops@example.com
    notifyOn:
      - ALWAYS

  pubsub:
    topic: projects/$_PROJECT_ID/topics/build-notifications
    attributes:
      source: cloudbuild
      pipeline: marketplace
```

---

## 5. Trigger Configuration

### 5.1 Trigger Conditions

| Trigger | Event | Branches/Tags | Pipeline |
|---------|-------|---------------|----------|
| `marketplace-ci` | Push to main | `main`, `develop` | Full pipeline |
| `marketplace-pr` | Pull Request | PR to main | Static validation only |
| `marketplace-tag` | Tag created | `v*` | Full + Release steps |
| `marketplace-manual` | Manual trigger | - | User-selected stages |

### 5.2 Trigger Configuration (gcloud commands)

```bash
# Create CI trigger for main branch
gcloud builds triggers create github \
  --name=marketplace-ci \
  --repo-name=erlmcp \
  --repo-owner=banyan-platform \
  --branch-pattern='^main$' \
  --build-config=marketplace/gcp/cloudbuild/cloudbuild-marketplace.yaml \
  --substitutions=_REGION=us-central1,_IMAGE_NAME=erlmcp/erlmcp

# Create PR validation trigger
gcloud builds triggers create github \
  --name=marketplace-pr \
  --repo-name=erlmcp \
  --repo-owner=banyan-platform \
  --pull-request-pattern='^main$' \
  --build-config=marketplace/gcp/cloudbuild/cloudbuild-pr.yaml \
  --substitutions=_SKIP_DEPLOYMENT_TESTS=true

# Create release trigger
gcloud builds triggers create github \
  --name=marketplace-release \
  --repo-name=erlmcp \
  --repo-owner=banyan-platform \
  --tag-pattern='^v[0-9]+\.[0-9]+\.[0-9]+$' \
  --build-config=marketplace/gcp/cloudbuild/cloudbuild-release.yaml
```

### 5.3 Trigger Strategy Matrix

| Trigger Type | Stage 1: Build | Stage 2: Security | Stage 3: Validate | Stage 4: Deploy Test | Stage 5: Evidence |
|--------------|:--------------:|:-----------------:|:-----------------:|:--------------------:|:-----------------:|
| **CI (main)** | ✓ | ✓ | ✓ | ✓ (Cloud Run only) | ✓ |
| **PR** | ✓ | ✓ | ✓ | ✗ | ✓ |
| **Release** | ✓ | ✓ | ✓ | ✓ (All tests) | ✓ |
| **Manual** | ✓ | ✓ | ✓ | Optional | ✓ |

---

## 6. IAM and Secret Requirements

### 6.1 Cloud Build Service Account

**Required Roles:**
```yaml
roles:
  - roles/cloudbuild.builds.builder
  - roles/artifactregistry.writer
  - roles/containeranalysis.notes.attacher
  - roles/containeranalysis.notes.viewer
  - roles/containeranalysis.occurrences.viewer
  - roles/storage.objectAdmin
  - roles/iam.serviceAccountUser
  - roles/logging.logWriter
  - roles/monitoring.metricWriter
  - roles/pubsub.publisher
```

**Creation Command:**
```bash
gcloud iam service-accounts create cloudbuild-marketplace \
  --display-name="Cloud Build Marketplace SA"

# Grant necessary roles
for role in \
  roles/cloudbuild.builds.builder \
  roles/artifactregistry.writer \
  roles/containeranalysis.notes.attacher \
  roles/storage.objectAdmin \
  roles/iam.serviceAccountUser; do
  gcloud projects add-iam-policy-binding $_PROJECT_ID \
    --member="serviceAccount:cloudbuild-marketplace@$_PROJECT_ID.iam.gserviceaccount.com" \
    --role="$role"
done
```

### 6.2 Required Secrets

| Secret | Purpose | Access |
|--------|---------|--------|
| `slack-webhook-url` | Slack notifications | Cloud Build |
| `pagerduty-api-key` | PagerDuty integration | Cloud Build |
| `marketplace-notifier-key` | Marketplace API | Cloud Build |

**Secret Creation:**
```bash
# Create Slack webhook secret
echo "https://hooks.slack.com/services/YOUR/WEBHOOK/URL" | \
  gcloud secrets create slack-webhook-url --data-file=-

# Grant access to Cloud Build SA
gcloud secrets add-iam-policy-binding slack-webhook-url \
  --member="serviceAccount:cloudbuild-marketplace@$_PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/secretmanager.secretAccessor"
```

### 6.3 IAM Policy for Test Deployment

**Temporary test deployment SA:**
```yaml
roles:
  - roles/container.developer
  - roles/run.admin
  - roles/compute.instanceAdmin
  - roles/iam.serviceAccountUser
```

**Note:** Test deployment permissions are scoped to a specific namespace/label to prevent accidental production modifications.

---

## 7. Pipeline Runbook

### 7.1 Running the Pipeline

**Option 1: Automatic (push to main)**
```bash
git push origin main
```

**Option 2: Manual trigger**
```bash
gcloud builds submit --config marketplace/gcp/cloudbuild/cloudbuild-marketplace.yaml \
  --substitutions=_PROJECT_ID=my-project,_IMAGE_TAG=v3.0.0
```

**Option 3: From PR (validation only)**
```bash
# Create PR against main - automatic trigger
```

### 7.2 Pipeline Execution Flow

```
1. SUBMIT
   ├─ Manual: gcloud builds submit
   ├─ Automatic: git push
   └─ PR: Create pull request

2. QUEUE
   ├─ Check Cloud Build quota
   ├─ Assign worker
   └─ Clone repository

3. EXECUTE
   ├─ Stage 1: Build (~5 min)
   ├─ Stage 2: Security (~10 min) [BLOCKING]
   ├─ Stage 3: Validate (~5 min)
   ├─ Stage 4: Deploy Test (~20 min) [OPTIONAL]
   └─ Stage 5: Evidence (~2 min)

4. COMPLETE
   ├─ Upload artifacts
   ├─ Send notifications
   └─ Update status
```

### 7.3 Troubleshooting

| Issue | Symptom | Resolution |
|-------|---------|------------|
| Build timeout | Stage exceeds 3600s | Increase timeout or optimize stage |
| Permission denied | SA lacks role | Grant missing IAM role |
| Scan failure | HIGH/CRITICAL vulnerabilities | Fix vulnerabilities or use relaxed mode |
| Test failure | Deployment fails | Check Cloud Logging for detailed error |
| Notification missing | No Slack message | Verify webhook URL and secret access |

**Debug Commands:**
```bash
# View build logs
gcloud builds log $BUILD_ID

# Stream live logs
gcloud builds stream-logs $BUILD_ID

# List recent builds
gcloud builds list --limit=10

# Retry failed build
gcloud builds submit --config marketplace/gcp/cloudbuild/cloudbuild-marketplace.yaml
```

### 7.4 Stage-Specific Runbook

#### Stage 1: Build Issues
```bash
# Build locally first
docker build -t test .

# Compare with CI
docker history $_REGION-docker.pkg.dev/$_PROJECT_ID/$_IMAGE_NAME:$_IMAGE_TAG
```

#### Stage 2: Security Scan Issues
```bash
# Run local scan
docker run --rm -v /var/run/docker.sock:/var/run/docker.sock \
  aquasec/trivy:latest image $_IMAGE_NAME

# View GCP scan results
gcloud container analysis occurrences list \
  --filter="resourceUrl=$_IMAGE_NAME"
```

#### Stage 3: Validation Issues
```bash
# Run validation locally
./marketplace/gcp/scripts/validate-terraform.sh
./marketplace/gcp/scripts/validate-schema.sh

# Helm lint
helm lint marketplace/gcp/helm/erlmcp-marketplace
```

#### Stage 4: Deployment Test Issues
```bash
# Test deployment locally (requires active gcloud session)
PROJECT_ID=my-project ./marketplace/gcp/scripts/test-cloudrun.sh

# Check test resource status
gcloud run services list --project=my-project
gcloud container clusters list --project=my-project
```

---

## 8. Evidence Collection

### 8.1 Evidence Artifacts

All pipeline runs generate the following evidence:

| Artifact | Location | Retention |
|----------|----------|-----------|
| Trivy scan report | `gs://$_PROJECT_ID-marketplace-evidence/$BUILD_ID/trivy-report.json` | 1 year |
| SBOM | `gs://$_PROJECT_ID-marketplace-evidence/$BUILD_ID/sbom.json` | 1 year |
| Validation logs | `gs://$_PROJECT_ID-marketplace-evidence/$BUILD_ID/*.log` | 1 year |
| Test results | `gs://$_PROJECT_ID-marketplace-evidence/$BUILD_ID/test-results/` | 1 year |
| Summary report | `gs://$_PROJECT_ID-marketplace-evidence/$BUILD_ID/SUMMARY.md` | 1 year |

### 8.2 Evidence Package Structure

```
gs://PROJECT_ID-marketplace-evidence/BUILD_ID/
├── SUMMARY.md                    # Executive summary
├── security/
│   ├── trivy-report.json        # Trivy vulnerability scan
│   ├── gcp-scan.json            # GCP container analysis
│   └── sbom.json                # Software Bill of Materials
├── validation/
│   ├── terraform.log            # Terraform validation
│   ├── schema.log               # Schema validation
│   └── helm.log                 # Helm lint results
├── deployment-tests/
│   ├── cloudrun/
│   │   ├── deploy.log           # Cloud Run deployment
│   │   ├── health-check.log     # Health check results
│   │   └── cleanup.log          # Resource cleanup
│   ├── gke/                     # (if run)
│   └── gce/                     # (if run)
├── build-info.json              # Build metadata
└── receipt.json                 # Signed build receipt
```

### 8.3 Receipt Format

Each build generates a signed receipt:

```json
{
  "build_id": "$BUILD_ID",
  "project_id": "$_PROJECT_ID",
  "image_digest": "$IMAGE_DIGEST",
  "git_sha": "$SHORT_SHA",
  "git_ref": "$BRANCH_NAME",
  "timestamp": "$TIMESTAMP",
  "stages": [
    {"name": "build", "status": "SUCCESS", "duration_seconds": 420},
    {"name": "security", "status": "SUCCESS", "duration_seconds": 580},
    {"name": "validate", "status": "SUCCESS", "duration_seconds": 180},
    {"name": "deploy-test", "status": "SUCCESS", "duration_seconds": 1200},
    {"name": "evidence", "status": "SUCCESS", "duration_seconds": 90}
  ],
  "security": {
    "trivy_high": 0,
    "trivy_critical": 0,
    "gcp_vulnerabilities": 0
  },
  "signature": "$KMS_SIGNATURE"
}
```

### 8.4 Evidence Verification

To verify build evidence:

```bash
# Download evidence package
gsutil -m cp -r gs://$_PROJECT_ID-marketplace-evidence/$BUILD_ID/* ./evidence/

# Verify signature
gcloud kms asymmetric-verify \
  --project $_PROJECT_ID \
  --location global \
  --keyring cloudbuild \
  --key verify-key \
  --version 1 \
  --signature-file evidence/signature.sig \
  --digest-file evidence/receipt.json

# Verify image digest
docker inspect $_REGION-docker.pkg.dev/$_PROJECT_ID/$_IMAGE_NAME:$_IMAGE_TAG \
  --format='{{index .RepoDigests 0}}'
```

---

## Appendix A: Enhanced Pipeline Configuration

The complete enhanced Cloud Build configuration is available at:
`/Users/sac/erlmcp/marketplace/gcp/cloudbuild/cloudbuild-marketplace-enhanced.yaml`

### Key Enhancements Over Original

| Feature | Original | Enhanced |
|---------|----------|----------|
| Stages | 1 linear | 5 parallelizable |
| Security | Trivy only | Trivy + GCP + Secret + SBOM |
| Validation | Terraform only | Terraform + Schema + Helm + Packer |
| Testing | Optional script | Integrated with cleanup |
| Notifications | None | Slack + Email + Pub/Sub |
| Evidence | Basic logs | Complete artifact package |
| Receipt | None | KMS-signed receipt |

---

## Appendix B: Migration Steps

To migrate from the current pipeline to the enhanced version:

1. **Create new Cloud Build trigger**
   ```bash
   gcloud builds triggers create github \
     --name=marketplace-ci-v2 \
     --repo-name=erlmcp \
     --repo-owner=banyan-platform \
     --branch-pattern='^main$' \
     --build-config=marketplace/gcp/cloudbuild/cloudbuild-marketplace-enhanced.yaml
   ```

2. **Configure required secrets**
   ```bash
   ./scripts/setup-pipeline-secrets.sh --project $_PROJECT_ID
   ```

3. **Configure IAM**
   ```bash
   ./scripts/setup-pipeline-iam.sh --project $_PROJECT_ID
   ```

4. **Test pipeline**
   ```bash
   gcloud builds submit \
     --config marketplace/gcp/cloudbuild/cloudbuild-marketplace-enhanced.yaml \
     --substitutions=_PROJECT_ID=$_PROJECT_ID
   ```

5. **Update PR triggers** (optional)

6. **Enable release triggers** (optional)

---

## Appendix C: Quick Reference

### Common Commands

```bash
# Trigger pipeline
gcloud builds submit marketplace/gcp/ --config cloudbuild/cloudbuild-marketplace-enhanced.yaml

# View running builds
gcloud builds list --ongoing

# View specific build
gcloud builds log $BUILD_ID

# Cancel build
gcloud builds cancel $BUILD_ID

# Retry with substitutions
gcloud builds submit . \
  --config marketplace/gcp/cloudbuild/cloudbuild-marketplace-enhanced.yaml \
  --substitutions=_IMAGE_TAG=v3.0.0,_SCAN_MODE=relaxed
```

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `_PROJECT_ID` | ${PROJECT_ID} | GCP Project ID |
| `_REGION` | us-central1 | GCP Region |
| `_IMAGE_NAME` | erlmcp/erlmcp | Image name |
| `_IMAGE_TAG` | ${SHORT_SHA} | Image tag |
| `_SKIP_TESTS` | false | Skip deployment tests |
| `_SCAN_MODE` | strict | Security scan mode |
| `_NOTIFICATION_CHANNEL` | slack | Notification channel |

---

## Document Version

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2026-02-02 | CI/CD Engineer | Initial design document |

---

*This document is part of the erlmcp GCP Marketplace deployment package.*
