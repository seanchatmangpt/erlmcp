# Release Template for erlmcp GCP Marketplace
## Version {VERSION} Release Template

**Use this template for all GCP Marketplace releases**

---

## Release Information

| Field | Value |
|-------|-------|
| **Version** | {VERSION} |
| **Previous Version** | {PREVIOUS_VERSION} |
| **Release Type** | {MAJOR/MINOR/PATCH/HOTFIX} |
| **Target Release Date** | {YYYY-MM-DD} |
| **Release Manager** | {NAME} |
| **Engineering Lead** | {NAME} |
| **Status** | {PLANNING/IN_PROGRESS/COMPLETE} |

---

## Pre-Release Planning

### Release Scope

**Features Included:**
- [ ] {FEATURE_1}
- [ ] {FEATURE_2}
- [ ] {FEATURE_3}

**Bug Fixes:**
- [ ] {FIX_1}
- [ ] {FIX_2}

**Known Issues:**
- [ ] {ISSUE_1}
- [ ] {ISSUE_2}

**Breaking Changes:**
- [ ] None OR
- [ ] {BREAKING_CHANGE_1} - Migration required: {YES/NO}
- [ ] {BREAKING_CHANGE_2} - Migration required: {YES/NO}

### Release Timeline

| Milestone | Date | Status | Notes |
|-----------|------|--------|-------|
| Release Branch Created | {DATE} | [ ] | |
| Code Freeze | {DATE} | [ ] | |
| Testing Complete | {DATE} | [ ] | |
| Staging Deployment | {DATE} | [ ] | |
| Production Release | {DATE} | [ ] | |
| Stable Declaration | {DATE} | [ ] | |

---

## Pre-Release Checklist

### Code Quality

- [ ] All PRs merged to release branch
- [ ] Zero compilation errors
- [ ] Zero dialyzer errors
- [ ] Zero xref errors
- [ ] Test coverage >= 80%
- [ ] Code review approved

### Testing

- [ ] Unit tests: 100% pass rate
- [ ] Integration tests: 100% pass rate
- [ ] CT suite: 100% pass rate
- [ ] Performance tests: All benchmarks pass
- [ ] Security tests: Clean scan

### Security

- [ ] Trivy scan: 0 CRITICAL, 0 HIGH
- [ ] GCP Container Analysis: Clean
- [ ] Secret scan: 0 secrets
- [ ] License scan: 0 violations
- [ ] SBOM generated

### Artifacts

- [ ] Container image built: `{IMAGE}:{VERSION}`
- [ ] Container image pushed to registry
- [ ] Image tagged as latest
- [ ] Image digest: `{DIGEST}`
- [ ] SBOM published

### Documentation

- [ ] Release notes drafted
- [ ] API documentation updated
- [ ] Migration guide (if needed)
- [ ] Deployment guide updated
- [ ] Marketplace schema updated

### Approvals

| Approver | Status | Date | Comments |
|----------|--------|------|----------|
| Engineering Lead | [ ] | | |
| Security Lead | [ ] | | |
| QA Lead | [ ] | | |
| Product Manager | [ ] | | |
| Release Manager | [ ] | | |

---

## Release Notes Template

```markdown
# erlmcp v{VERSION}

Released: {YYYY-MM-DD}
Previous: v{PREVIOUS_VERSION}

## Summary

{BRIEF_DESCRIPTION}

## What's New

### Highlights

{HIGHLIGHTS}

## Installation

### Google Cloud Marketplace

Deploy directly from the Marketplace:
https://console.cloud.google.com/marketplace/details/erlmcp

### Docker

```bash
docker pull us-central1-docker.pkg.dev/erlmcp/erlmcp:{VERSION}
docker run -p 8080:8080 us-central1-docker.pkg.dev/erlmcp/erlmcp:{VERSION}
```

### From Source

```bash
git clone https://github.com/banyan-platform/erlmcp
cd erlmcp
git checkout v{VERSION}
rebar3 release
```

## Upgrade Guide

### From v{PREVIOUS_VERSION}

#### Pre-upgrade Checklist

- [ ] Backup current deployment
- [ ] Note current configuration
- [ ] Check Erlang/OTP version compatibility

#### Upgrade Steps

1. **Backup your data:**
   ```bash
   # For GKE
   kubectl get configmap -n erlmcp -o yaml > backup-configmap.yaml
   kubectl get secrets -n erlmcp -o yaml > backup-secrets.yaml
   ```

2. **Update the deployment:**
   ```bash
   # Update image tag
   kubectl set image deployment/erlmcp \
     erlmcp=us-central1-docker.pkg.dev/erlmcp/erlmcp:{VERSION} \
     -n erlmcp
   ```

3. **Verify health:**
   ```bash
   kubectl rollout status deployment/erlmcp -n erlmcp
   curl https://erlmcp.example.com/health
   ```

### Breaking Changes

{BREAKING_CHANGES_SECTION}

If no breaking changes are listed, this release is backward compatible with v{PREVIOUS_VERSION}.

## Changelog

### Added

- {NEW_FEATURE_1}
- {NEW_FEATURE_2}

### Changed

- {CHANGE_1}
- {CHANGE_2}

### Fixed

- {FIX_1}
- {FIX_2}

### Security

- {SECURITY_FIX_1}

## Artifacts

| Artifact | Link | Checksum |
|----------|------|----------|
| **Docker Image** | `us-central1-docker.pkg.dev/erlmcp/erlmcp:{VERSION}` | `{SHA256}` |
| **GitHub Release** | [v{VERSION}](https://github.com/banyan-platform/erlmcp/releases/tag/v{VERSION}) | - |
| **SBOM** | [erlmcp-{VERSION}.sbom.json](...) | - |
| **Provenance** | [erlmcp-{VERSION}.provenance.json](...) | - |

### Verification

Verify Docker image:
```bash
docker pull us-central1-docker.pkg.dev/erlmcp/erlmcp:{VERSION}
docker inspect us-central1-docker.pkg.dev/erlmcp/erlmcp:{VERSION} --format='{{.Id}}'
```

Verify checksum:
```bash
sha256sum erlmcp-{VERSION}.tar.gz
```

## Performance

| Metric | v{PREVIOUS_VERSION} | v{VERSION} |
|--------|---------------------|------------|
| p50 Latency | {VALUE}ms | {VALUE}ms |
| p95 Latency | {VALUE}ms | {VALUE}ms |
| Throughput | {VALUE}/s | {VALUE}/s |
| Memory | {VALUE}MB | {VALUE}MB |

## Security

This release includes security updates for:
- {DEPENDENCY_1}: {VULNERABILITY_FIX}
- {DEPENDENCY_2}: {VULNERABILITY_FIX}

**Vulnerability Scan Results:**
- Critical: 0
- High: 0
- Medium: {COUNT}
- Low: {COUNT}

## Known Issues

- {KNOWN_ISSUE_1}
- {KNOWN_ISSUE_2}

## Compatibility

### Erlang/OTP

**Required: Erlang/OTP {MIN_OTP}+**

Tested on:
- OTP 26: Supported
- OTP 27: Supported
- OTP 28: Supported

### Google Cloud Platforms

| Platform | Supported |
|----------|-----------|
| Cloud Run | Yes |
| GKE | Yes |
| Compute Engine | Yes |

## Contributors

{CONTRIBUTOR_LIST}

Thank you to everyone who contributed to this release!

## Support

- **Documentation**: https://docs.erlmcp.dev
- **API Reference**: https://docs.erlmcp.dev/api
- **Issues**: https://github.com/banyan-platform/erlmcp/issues
- **Discussions**: https://github.com/banyan-platform/erlmcp/discussions
- **Discord**: https://discord.gg/erlmcp
- **Email**: support@erlmcp.dev

## Migration from v2.x

If you're upgrading from v2.x to v3.x, please review the detailed migration guide:

[Migration Guide: v2.x to v3.0](https://docs.erlmcp.dev/migration/v2-to-v3)

---

**Full Changelog**: https://github.com/banyan-platform/erlmcp/compare/v{PREVIOUS_VERSION}...v{VERSION}
```

---

## Marketplace Submission Checklist

### Technical Requirements

- [ ] Container image in Artifact Registry
- [ ] Image size < 500MB
- [ ] Startup time < 5 minutes
- [ ] Health endpoint: `/health`
- [ ] No hardcoded credentials

### Schema Files

- [ ] `application.yaml` updated with new version
- [ ] `schema.yaml` validated
- [ ] `parameters.yaml` updated
- [ ] `metadata.display.yaml` updated

### Testing

- [ ] Cloud Run deployment tested
- [ ] GKE deployment tested
- [ ] Compute Engine deployment tested
- [ ] Cleanup verified
- [ ] Health checks passing

### Documentation

- [ ] README.md current
- [ ] Architecture docs updated
- [ ] API docs updated
- [ ] Migration guide (if needed)

---

## Release Day Script

### Pre-Release (T-2 hours)

```bash
#!/bin/bash
# Pre-release verification

VERSION="{VERSION}"
PROJECT_ID="{PROJECT_ID}"

echo "=== Pre-Release Verification for erlmcp v$VERSION ==="

# 1. Verify release branch
git fetch origin
if git rev-parse --verify "release/v$VERSION" >/dev/null 2>&1; then
  echo "✓ Release branch exists"
else
  echo "✗ Release branch not found"
  exit 1
fi

# 2. Verify image exists
if gcloud artifacts docker images describe \
  "us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp/erlmcp:$VERSION" \
  --project="$PROJECT_ID" >/dev/null 2>&1; then
  echo "✓ Container image exists"
else
  echo "✗ Container image not found"
  exit 1
fi

# 3. Verify tests pass
echo "Running tests..."
docker compose run --rm erlmcp-unit
docker compose run --rm erlmcp-ct

echo "✓ All pre-release checks passed"
```

### Release (T-0)

```bash
#!/bin/bash
# Release execution

VERSION="{VERSION}"
PROJECT_ID="{PROJECT_ID}"

echo "=== Releasing erlmcp v$VERSION ==="

# 1. Tag release
git tag -a "v$VERSION" -m "Release v$VERSION"
git push origin "v$VERSION"

# 2. Create GitHub release
gh release create "v$VERSION" \
  --title "erlmcp v$VERSION" \
  --notes-file RELEASE_NOTES.md

# 3. Verify latest tag
docker tag \
  "us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp/erlmcp:$VERSION" \
  "us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp/erlmcp:latest"
docker push "us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp/erlmcp:latest"

echo "✓ Release v$VERSION complete!"
```

### Post-Release (T+1 hour)

```bash
#!/bin/bash
# Post-release verification

VERSION="{VERSION}"
PROJECT_ID="{PROJECT_ID}"

echo "=== Post-Release Verification ==="

# 1. Check GitHub release
if gh release view "v$VERSION" >/dev/null 2>&1; then
  echo "✓ GitHub release published"
else
  echo "✗ GitHub release not found"
fi

# 2. Check image tags
gcloud artifacts docker images list \
  "--repository=us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp" \
  --project="$PROJECT_ID"

# 3. Health check
SERVICE_URL=$(gcloud run services describe erlmcp \
  --region=us-central1 \
  --format='value(status.url)')

if curl -s "$SERVICE_URL/health" | grep -q '"status":"ok"'; then
  echo "✓ Health check passing"
else
  echo "✗ Health check failing"
fi

echo "✓ Post-release verification complete"
```

---

## Post-Release Monitoring

### Day 1 Monitoring Schedule

| Time | Check | Status |
|------|-------|--------|
| T+1 hour | Health check | [ ] |
| T+4 hours | Error rate | [ ] |
| T+8 hours | Performance | [ ] |
| T+12 hours | Review | [ ] |
| T+24 hours | Milestone | [ ] |

### Week 1 Monitoring Schedule

| Day | Focus | Status |
|-----|-------|--------|
| Day 1 | Critical issues | [ ] |
| Day 2 | Performance | [ ] |
| Day 3 | User feedback | [ ] |
| Day 4 | Adoption | [ ] |
| Day 5 | Stability | [ ] |
| Day 6 | Trends | [ ] |
| Day 7 | Weekly summary | [ ] |

---

## Rollback Plan

### Rollback Triggers

- [ ] Critical bug in production
- [ ] Security vulnerability
- [ ] Data loss/corruption
- [ ] Performance degradation > 50%

### Rollback Procedure

```bash
# Cloud Run rollback
gcloud run services update-traffic erlmcp \
  --to-revisions={PREVIOUS_REVISION}=100 \
  --region=us-central1

# GKE rollback
kubectl rollout undo deployment/erlmcp -n erlmcp

# Verify rollback
curl https://erlmcp.example.com/health
```

### Rollback Verification

- [ ] Health checks passing
- [ ] Error rates normal
- [ ] Performance baseline met
- [ ] No new errors

---

## Release Retrospective

### What Went Well

- {POSITIVE_1}
- {POSITIVE_2}

### What Could Be Improved

- {IMPROVEMENT_1}
- {IMPROVEMENT_2}

### Action Items for Next Release

| Item | Owner | Due Date |
|------|-------|----------|
| {ACTION_1} | {OWNER} | {DATE} |
| {ACTION_2} | {OWNER} | {DATE} |

---

## Appendix

### Commands Reference

```bash
# Validation
cd marketplace/gcp && bash scripts/run-marketplace-validation.sh

# Security scan
bash scripts/scan-image.sh --project $PROJECT_ID --mode strict

# Build and push
bash scripts/build-and-push.sh --project $PROJECT_ID --tag {VERSION}

# Create release branch
git checkout -b release/v{VERSION}

# Tag release
git tag -a v{VERSION} -m "Release v{VERSION}"
git push origin v{VERSION}
```

### Contact Information

| Role | Name | Email | Slack |
|------|------|-------|-------|
| Release Manager | {NAME} | {EMAIL} | {SLACK} |
| Engineering Lead | {NAME} | {EMAIL} | {SLACK} |
| Security Lead | {NAME} | {EMAIL} | {SLACK} |
| QA Lead | {NAME} | {EMAIL} | {SLACK} |

---

**Template Version:** 1.0
**Last Updated:** 2026-02-02

*Copy this template to create a new release document.*
