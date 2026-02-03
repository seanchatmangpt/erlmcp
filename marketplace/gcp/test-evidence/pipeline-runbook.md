# GCP Marketplace CI/CD Pipeline Runbook

**Version:** 3.0.0
**Last Updated:** 2026-02-02

---

## Quick Start

### 1. Initial Setup (One-Time)

```bash
# Set your project ID
export PROJECT_ID=your-project-id

# Run IAM setup
bash marketplace/gcp/scripts/setup-pipeline-iam.sh --project $PROJECT_ID

# Run secrets setup
bash marketplace/gcp/scripts/setup-pipeline-secrets.sh --project $PROJECT_ID
```

### 2. Create Build Trigger

```bash
gcloud builds triggers create github \
  --name=marketplace-ci \
  --repo-name=erlmcp \
  --repo-owner=banyan-platform \
  --branch-pattern='^main$' \
  --build-config=marketplace/gcp/cloudbuild/cloudbuild-marketplace-enhanced.yaml \
  --substitutions=_REGION=us-central1,_IMAGE_NAME=erlmcp/erlmcp
```

### 3. Trigger Pipeline

```bash
# Automatic: Push to main branch
git push origin main

# Manual trigger
gcloud builds submit . \
  --config marketplace/gcp/cloudbuild/cloudbuild-marketplace-enhanced.yaml \
  --substitutions=_PROJECT_ID=$PROJECT_ID,_IMAGE_TAG=v3.0.0
```

---

## Pipeline Stages Overview

| Stage | Duration | Description | Blocking |
|-------|----------|-------------|----------|
| Setup | ~1 min | Environment preparation | No |
| Build | ~8 min | Container image build | Yes |
| Security | ~5 min | Vulnerability scanning | Yes (strict mode) |
| Validation | ~3 min | Terraform, schema, Helm | Yes |
| Deploy Tests | ~15 min | Cloud Run, GKE tests | No (optional) |
| Evidence | ~1 min | Artifact collection | No |
| **Total** | **~33 min** | | |

---

## Common Operations

### View Build Status

```bash
# List recent builds
gcloud builds list --limit=10 --project=$PROJECT_ID

# View specific build logs
gcloud builds log $BUILD_ID

# Stream live logs
gcloud builds stream-logs $BUILD_ID
```

### Retry Failed Build

```bash
# Get original build substitutions
gcloud builds describe $BUILD_ID --format="value(substitutions)" > /tmp/subs.txt

# Retry with same substitutions
gcloud builds submit . \
  --config marketplace/gcp/cloudbuild/cloudbuild-marketplace-enhanced.yaml \
  --substitutions=$(cat /tmp/subs.txt)
```

### Download Evidence

```bash
# Download all evidence
gsutil -m cp -r gs://$PROJECT_ID-marketplace-evidence/$BUILD_ID/* ./evidence/

# View summary
cat ./evidence/SUMMARY.md
```

### Clean Up Test Resources

```bash
# Dry run (preview)
bash marketplace/gcp/scripts/cleanup-test-resources.sh \
  --project $PROJECT_ID \
  --dry-run

# Actual cleanup
bash marketplace/gcp/scripts/cleanup-test-resources.sh \
  --project $PROJECT_ID
```

---

## Troubleshooting

### Build Timeout

**Symptom:** Stage exceeds timeout

**Solutions:**
1. Increase timeout in cloudbuild YAML
2. Optimize Docker build (use cache)
3. Skip optional tests: `--substitutions=_SKIP_DEPLOYMENT_TESTS=true`

```yaml
timeout: '3600s'  # Increase overall timeout
```

### Permission Denied

**Symptom:** `Permission denied` errors

**Solutions:**
1. Verify SA has required roles:
   ```bash
   gcloud projects get-iam-policy $PROJECT_ID \
     --filter="serviceAccount:cloudbuild-marketplace@$PROJECT_ID.iam.gserviceaccount.com"
   ```

2. Re-run IAM setup:
   ```bash
   bash marketplace/gcp/scripts/setup-pipeline-iam.sh --project $PROJECT_ID
   ```

### Security Scan Failures

**Symptom:** Trivy finds HIGH/CRITICAL vulnerabilities

**Solutions:**
1. Fix vulnerabilities (recommended)
2. Use relaxed mode (temporary):
   ```bash
   --substitutions=_SCAN_MODE=relaxed
   ```

### Validation Failures

**Symptom:** Terraform or schema validation fails

**Solutions:**
1. Run validation locally:
   ```bash
   bash marketplace/gcp/scripts/validate-terraform.sh
   bash marketplace/gcp/scripts/validate-schema.sh
   ```

2. Fix reported issues and retry

### No Notifications

**Symptom:** Slack/Email notifications not received

**Solutions:**
1. Verify secret exists:
   ```bash
   gcloud secrets describe slack-webhook-url --project $PROJECT_ID
   ```

2. Recreate webhook URL:
   ```bash
   bash marketplace/gcp/scripts/setup-pipeline-secrets.sh --project $PROJECT_ID
   ```

---

## Release Checklist

Before submitting to GCP Marketplace:

- [ ] All tests pass in strict mode
- [ ] No HIGH/CRITICAL vulnerabilities
- [ ] Terraform modules validate
- [ ] Marketplace schema validates
- [ ] Helm chart lints without errors
- [ ] Deployment tests pass
- [ ] Evidence package complete
- [ ] Build receipt signed
- [ ] Documentation updated

---

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `PROJECT_ID` | Required | GCP Project ID |
| `_REGION` | us-central1 | GCP Region |
| `_IMAGE_NAME` | erlmcp/erlmcp | Container image name |
| `_IMAGE_TAG` | ${SHORT_SHA} | Image tag |
| `_SCAN_MODE` | strict | Security scan mode |
| `_SKIP_DEPLOYMENT_TESTS` | false | Skip deployment tests |
| `_NOTIFICATION_CHANNEL` | slack | Notification channel |
| `_CLEANUP_TEST_RESOURCES` | true | Auto-cleanup test resources |

---

## Useful Commands Reference

```bash
# === Build Management ===
gcloud builds list --ongoing                      # List running builds
gcloud builds cancel $BUILD_ID                    # Cancel build
gcloud builds log $BUILD_ID                       # View logs
gcloud builds describe $BUILD_ID                  # Build details

# === Evidence Management ===
gsutil ls gs://$PROJECT_ID-marketplace-evidence/  # List evidence
gsutil du -h gs://$PROJECT_ID-marketplace-evidence/  # Evidence size
gsutil -m rm -r gs://$PROJECT_ID-marketplace-evidence/$BUILD_ID/  # Delete evidence

# === Secret Management ===
gcloud secrets list                               # List secrets
gcloud secrets versions list SECRET_NAME          # List secret versions
gcloud secrets describe SECRET_NAME               # Secret details

# === Service Account Management ===
gcloud iam service-accounts list                  # List SAs
gcloud iam service-accounts describe SA_EMAIL     # SA details
gcloud projects get-iam-policy PROJECT_ID         # Project IAM policy

# === Resource Cleanup ===
# Clean up all test resources
bash marketplace/gcp/scripts/cleanup-test-resources.sh --project $PROJECT_ID

# Clean up specific resource type
gcloud run services list --filter="name:test-*" --project=$PROJECT_ID
gcloud container clusters list --filter="name:test-*" --project=$PROJECT_ID
```

---

## Support and Documentation

- **Main Documentation:** `/Users/sac/erlmcp/marketplace/gcp/test-evidence/cicd-pipeline-report.md`
- **Enhanced Pipeline:** `/Users/sac/erlmcp/marketplace/gcp/cloudbuild/cloudbuild-marketplace-enhanced.yaml`
- **Test Scripts:** `/Users/sac/erlmcp/marketplace/gcp/scripts/`

---

## Quick Decision Tree

```
Issue?
├─ Build fails
│  ├─ Timeout? → Increase timeout or skip tests
│  ├─ Permission error? → Re-run setup-pipeline-iam.sh
│  ├─ Vulnerability found? → Fix or use relaxed mode
│  └─ Validation error? → Fix and re-run validation locally
│
├─ No notifications
│  ├─ Check secret exists: gcloud secrets describe slack-webhook-url
│  └─ Re-run setup-pipeline-secrets.sh
│
└─ Test resources not cleaned up
   └─ Run: bash marketplace/gcp/scripts/cleanup-test-resources.sh --project $PROJECT_ID
```

---

*This runbook is part of the erlmcp GCP Marketplace CI/CD Pipeline documentation.*
