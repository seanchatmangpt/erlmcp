# GCP Marketplace Release Management Playbook
## erlmcp v3.0.0 - Production Release

**Document Version:** 1.0
**Last Updated:** 2026-02-02
**Product:** erlmcp - Erlang/OTP Model Context Protocol Server
**Publisher:** banyan-platform
**Marketplace:** Google Cloud Marketplace

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Release Process](#2-release-process)
3. [Version Management Strategy](#3-version-management-strategy)
4. [Quality Gates](#4-quality-gates)
5. [Marketplace Submission](#5-marketplace-submission)
6. [Release Communication](#6-release-communication)
7. [Rollback Procedures](#7-rollback-procedures)
8. [Post-Release Monitoring](#8-post-release-monitoring)
9. [Checklists](#9-checklists)
10. [Appendices](#10-appendices)

---

## 1. Executive Summary

### 1.1 Purpose

This playbook defines the end-to-end release management process for publishing erlmcp to the Google Cloud Marketplace. It ensures consistent, secure, and compliant releases while minimizing risk and maximizing customer satisfaction.

### 1.2 Release Objectives

- **Zero-defect releases:** All releases pass comprehensive quality gates
- **Security-first:** Vulnerability scans mandatory before release
- **Customer-focused:** Documentation and support aligned with release
- **Compliance:** Adherence to GCP Marketplace policies
- **Recoverability:** Clear rollback procedures for all releases

### 1.3 Stakeholders

| Role | Responsibilities | Approval Required |
|------|------------------|-------------------|
| Release Manager | Orchestrate release process | Yes - Final approval |
| Engineering Lead | Technical validation | Yes - Code quality |
| Security Lead | Security review | Yes - Vulnerability assessment |
| Product Manager | Release planning and communications | Yes - Go-to-market |
| QA Lead | Testing validation | Yes - Test results |
| Operations Lead | Deployment and monitoring | Yes - Production readiness |

### 1.4 Release Cadence

| Release Type | Frequency | Validation | Notice Period |
|--------------|-----------|------------|---------------|
| **Major** | Quarterly | Full suite | 4 weeks |
| **Minor** | Monthly | Standard suite | 2 weeks |
| **Patch** | As needed | Critical path | 48 hours |
| **Hotfix** | Emergency | Essential only | Immediate |

---

## 2. Release Process

### 2.1 Release Stages

The release process follows a gated approach with clearly defined stages:

```
[Planning] -> [Development] -> [Validation] -> [Staging] -> [Release] -> [Monitoring]
    |            |              |             |           |            |
    v            v              v             v           v            v
[Gate 1]     [Gate 2]       [Gate 3]      [Gate 4]    [Gate 5]    [Gate 6]
Definition   Code Quality   Testing       Staging     Production  Stability
Complete     Passes         Passes        Verified    Deployed     Confirmed
```

### 2.2 Stage 1: Release Planning

**Duration:** 1-2 weeks before release date

**Activities:**

1. **Define Release Scope**
   - Features to be included
   - Bug fixes to be addressed
   - Known limitations
   - Deprecation notices

2. **Create Release Branch**
   ```bash
   # Format: release/v{MAJOR}.{MINOR}.{PATCH}
   git checkout -b release/v3.0.0
   ```

3. **Update Documentation**
   - Release notes draft
   - Migration guide (if needed)
   - API documentation updates
   - Deployment guide updates

4. **Assign Release Manager**
   - Single point of contact
   - Coordinate all activities
   - Own go/no-go decision

**Exit Criteria:**

- [ ] Release scope documented
- [ ] Release branch created
- [ ] Release notes draft complete
- [ ] All stakeholders notified

### 2.3 Stage 2: Development and Code Freeze

**Duration:** 1 week before code freeze

**Activities:**

1. **Feature Completion**
   - All planned features merged
   - Feature flags configured
   - Breaking changes documented

2. **Code Quality Gates**
   ```bash
   # Run via Docker only (per erlmcp constitution)
   docker compose run --rm erlmcp-build rebar3 compile
   docker compose run --rm erlmcp-unit
   docker compose run --rm erlmcp-check
   docker compose run --rm erlmcp-ct
   ```

3. **Security Scan**
   ```bash
   cd marketplace/gcp
   bash scripts/scan-image.sh --project $PROJECT_ID --mode strict
   ```

4. **Generate Artifacts**
   - Container images
   - SBOM (Software Bill of Materials)
   - Provenance signatures

**Exit Criteria:**

- [ ] All code merged to release branch
- [ ] Zero compilation errors
- [ ] Zero test failures
- [ ] Security scan passes (0 HIGH/CRITICAL)
- [ ] Artifacts generated and signed

### 2.4 Stage 3: Pre-Release Validation

**Duration:** 3-5 days

**Activities:**

1. **Run Full Validation Suite**
   ```bash
   cd marketplace/gcp
   bash scripts/run-marketplace-validation.sh \
     --project $PROJECT_ID \
     --run-deployment-tests
   ```

2. **Security Validation**
   - Container vulnerability scan (Trivy)
   - GCP Container Analysis
   - Dependency license check
   - Secret scan

3. **Deployment Testing**
   - Cloud Run deployment test
   - GKE deployment test
   - Compute Engine deployment test

4. **Performance Testing**
   - Load testing at scale
   - Memory leak detection
   - Response time benchmarks

**Exit Criteria:**

- [ ] Phase 0 (Static Validation): PASS
- [ ] Phase 1 (Artifact Tests): PASS
- [ ] Phase 2 (Deployment Tests): PASS
- [ ] Performance benchmarks met
- [ ] Security scan clean
- [ ] Test coverage >= 80%

### 2.5 Stage 4: Staging Deployment

**Duration:** 1-2 days

**Activities:**

1. **Deploy to Staging Environment**
   ```bash
   # Cloud Run staging
   gcloud run deploy erlmcp-staging \
     --image us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp/erlmcp:$VERSION \
     --region us-central1 \
     --no-allow-unauthenticated
   ```

2. **Smoke Testing**
   - Health endpoint check
   - Core functionality test
   - Integration validation

3. **User Acceptance Testing**
   - Product manager sign-off
   - Documentation verification
   - Support team preparedness check

4. **Final Go/No-Go Decision**

**Exit Criteria:**

- [ ] Staging deployment successful
- [ ] Smoke tests pass
- [ ] UAT sign-off received
- [ ] Go/No-Go decision: GO

### 2.6 Stage 5: Production Release

**Duration:** Release day (4-hour window)

**Activities:**

1. **Pre-Release Checklist Verification**
   - See Section 9.1 for complete checklist

2. **Tag and Publish Release**
   ```bash
   # Tag release
   git tag -a v3.0.0 -m "Release v3.0.0: GCP Marketplace GA"
   git push origin v3.0.0

   # Publish container images
   docker push us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp/erlmcp:3.0.0
   docker push us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp/erlmcp:latest
   ```

3. **Submit to Marketplace**
   - Upload deployment package
   - Submit for review
   - Monitor review status

4. **Monitor Deployment**
   - Cloud Build logs
   - Deployment health checks
   - Error rate monitoring

**Exit Criteria:**

- [ ] Release tagged in git
- [ ] Container images published
- [ ] Marketplace submission complete
- [ ] Initial deployment healthy

### 2.7 Stage 6: Post-Release Monitoring

**Duration:** 7-14 days after release

**Activities:**

1. **Stability Monitoring**
   - Error rates
   - Performance metrics
   - User feedback

2. **Incident Response**
   - Rapid response to issues
   - Bug triage
   - Hotfix if needed

3. **Release Retrospective**
   - What went well
   - What could be improved
   - Action items for next release

**Exit Criteria:**

- [ ] 7-day stability confirmed
- [ ] No critical issues
- [ ] Retrospective completed
- [ ] Release marked as stable

---

## 3. Version Management Strategy

### 3.1 Semantic Versioning

erlmcp follows Semantic Versioning 2.0.0:

```
MAJOR.MINOR.PATCH
  |     |     |
  |     |     +-- PATCH: Bug fixes, no breaking changes
  |     +-------- MINOR: New features, backward compatible
  +-------------- MAJOR: Breaking changes
```

**Version Examples:**

- `3.0.0` - Major release (breaking changes from 2.x)
- `3.1.0` - Minor release (new features, backward compatible)
- `3.1.1` - Patch release (bug fixes)

### 3.2 Version Number Assignment

| Change Type | Version Increment | Examples |
|-------------|-------------------|----------|
| Critical bug fix | PATCH | `3.0.0` -> `3.0.1` |
| New feature (backward compatible) | MINOR | `3.0.0` -> `3.1.0` |
| Breaking change | MAJOR | `2.9.0` -> `3.0.0` |
| Deprecation (with grace period) | MINOR | `3.0.0` -> `3.1.0` |
| Removal of deprecated features | MAJOR | `3.0.0` -> `4.0.0` |

### 3.3 Release Branch Strategy

```
main (protected)
  |
  +-- release/v3.0.0 (release branch)
  |     |
  |     +-- hotfix/v3.0.1 (from release tag)
  |
  +-- release/v3.1.0 (next release branch)
  |
  +-- develop (integration branch)
```

**Branch Naming Conventions:**

| Branch Type | Pattern | Purpose |
|-------------|---------|---------|
| Main | `main` | Production-ready code |
| Release | `release/v{VERSION}` | Release preparation |
| Hotfix | `hotfix/v{VERSION}` | Emergency fixes |
| Feature | `feature/{NAME}` | Feature development |

### 3.4 Changelog Format

```markdown
# [VERSION] - YYYY-MM-DD

## Summary
Brief description of the release

## Breaking Changes
List of breaking changes and migration guide

## Added
- New feature 1
- New feature 2

## Changed
- Modified behavior 1
- Modified behavior 2

## Fixed
- Bug fix 1
- Bug fix 2

## Security
- Security fix 1

## Known Issues
- Known issue 1

## Upgrade Notes
Special upgrade instructions

## Migration Guide
Link to detailed migration guide if needed
```

### 3.5 Version Compatibility Matrix

| erlmcp Version | Erlang/OTP | GKE Version | Cloud Run | Compute Engine |
|----------------|------------|-------------|-----------|----------------|
| 3.0.x | 26-28 | 1.27+ | Supported | Supported |
| 3.1.x | 27-28 | 1.28+ | Supported | Supported |
| 4.0.x | 28+ | 1.29+ | Supported | Supported |

### 3.6 Deprecation Policy

- **Minimum notice:** 3 months for feature deprecation
- **Support period:** 18 months for major versions
- **Patch support:** 6 months after next minor release
- **Security patches:** Until end of support period

---

## 4. Quality Gates

### 4.1 Pre-Release Requirements

All releases MUST meet the following criteria:

#### 4.1.1 Code Quality

| Requirement | Tool | Threshold | Action |
|-------------|------|-----------|--------|
| Compilation | rebar3 | 0 errors | Block |
| Dialyzer | dialyzer | 0 errors | Block |
| xref | xref | 0 errors | Block |
| Code Coverage | cover | >= 80% | Block |
| Linting | elvis | 0 errors | Block |

#### 4.1.2 Testing Requirements

| Test Suite | Pass Rate | Duration | Block |
|------------|-----------|----------|-------|
| Unit Tests | 100% | < 5 min | Yes |
| Integration Tests | 100% | < 15 min | Yes |
| CT Suite | 100% | < 30 min | Yes |
| Performance Tests | 100% | < 1 hour | Yes |
| Security Tests | 0 CRITICAL/HIGH | < 10 min | Yes |

#### 4.1.3 Security Requirements

| Check | Tool | Threshold | Action |
|-------|------|-----------|--------|
| Vulnerability Scan | Trivy | 0 CRITICAL, 0 HIGH | Block |
| Container Scan | GCP Container Analysis | 0 CRITICAL, 0 HIGH | Block |
| Secret Scan | gitleaks | 0 secrets | Block |
| License Scan | FOSSA | 0 violations | Review |
| SBOM Generation | Syft | Complete | Required |

#### 4.1.4 Documentation Requirements

- [ ] Release notes published
- [ ] API documentation updated
- [ ] Migration guide (if breaking changes)
- [ ] Deployment guide updated
- [ ] Troubleshooting guide current

### 4.2 Quality Gate Execution

#### 4.2.1 Automated Quality Gates

Executed by Cloud Build pipeline:

```yaml
# File: marketplace/gcp/cloudbuild/quality-gate.yaml
steps:
  - name: 'Compile and Test'
    script: |
      docker compose run --rm erlmcp-build rebar3 compile
      docker compose run --rm erlmcp-unit
      docker compose run --rm erlmcp-check

  - name: 'Security Scan'
    script: |
      bash scripts/scan-image.sh --mode strict

  - name: 'Generate Artifacts'
    script: |
      bash scripts/build-and-push.sh
```

#### 4.2.2 Manual Quality Gates

Require human approval:

- [ ] Release notes reviewed
- [ ] Breaking changes assessed
- [ ] Migration path validated
- [ ] Rollback plan confirmed
- [ ] Support team notified

### 4.3 Approval Process

#### 4.3.1 Approval Matrix

| Approval | Required For | Escalation |
|----------|--------------|------------|
| Engineering Lead | All releases | CTO |
| Security Lead | All releases | CISO |
| Product Manager | Major/Minor releases | VP Product |
| QA Lead | All releases | VP Engineering |
| Release Manager | All releases | None |

#### 4.3.2 Approval Workflow

```
[Developer] -> [QA Lead] -> [Security Lead] -> [Engineering Lead] -> [Release Manager]
     |             |              |                  |                      |
  Complete     Tests Pass    Security Clear    Code Approved        Release Approved
```

### 4.4 Gate Failure Handling

| Gate Failed | Action | Recovery |
|-------------|--------|----------|
| Compilation | Block | Fix errors, restart gate |
| Unit Tests | Block | Fix tests, restart gate |
| Integration Tests | Block | Fix tests, restart gate |
| Security Scan | Block | Remediate, rescan |
| Documentation | Warn | Complete documentation |
| Approval Missing | Block | Obtain approval |

---

## 5. Marketplace Submission

### 5.1 Pre-Submission Checklist

#### 5.1.1 Technical Requirements

- [ ] Container image pushed to Artifact Registry
- [ ] Image tagged with semantic version
- [ ] Image size < 500MB
- [ ] SBOM generated and available
- [ ] Provenance signature attached
- [ ] Health endpoints documented

#### 5.1.2 Schema Requirements

- [ ] `application.yaml` valid and complete
- [ ] `schema.yaml` matches marketplace schema
- [ ] `parameters.yaml` maps to Terraform variables
- [ ] `metadata.display.yaml` has all required fields
- [ ] Input schema validated against JSON Schema draft-07

#### 5.1.3 Deployment Requirements

- [ ] Terraform modules validate successfully
- [ ] Terraform state configuration defined
- [ ] Helm charts lint and validate
- [ ] Deployment scripts tested
- [ ] Cloud Build configurations valid

#### 5.1.4 Documentation Requirements

- [ ] README.md complete
- [ ] Architecture documentation available
- [ ] API documentation published
- [ ] Troubleshooting guide available
- [ ] Support channels defined

### 5.2 Submission Artifacts

#### 5.2.1 Required Files

```
marketplace/gcp/
├── marketplace-schema/
│   ├── application.yaml      # Marketplace definition
│   ├── schema.yaml           # Input schema
│   ├── parameters.yaml       # Parameter mappings
│   └── metadata.display.yaml # Display metadata
├── terraform/
│   └── modules/              # Terraform modules
├── helm/
│   └── erlmcp-marketplace/   # Helm charts
├── cloudbuild/
│   └── cloudbuild.yaml       # Build pipeline
├── scripts/
│   └── *.sh                  # Utility scripts
├── docs/
│   ├── README.md             # Main documentation
│   ├── ARCHITECTURE.md       # Architecture
│   ├── DEPLOYMENT.md         # Deployment guide
│   └── TROUBLESHOOTING.md    # Troubleshooting
└── artifacts/
    ├── sbom.json             # Software Bill of Materials
    ├── provenance.json       # Build provenance
    └── scan-results.json     # Security scan results
```

#### 5.2.2 Marketplace Schema Validation

```bash
# Validate marketplace schema
python3 << 'EOF'
import yaml
import json
import sys

# Load and validate schema files
with open('marketplace-schema/application.yaml') as f:
    app = yaml.safe_load(f)

with open('marketplace-schema/schema.yaml') as f:
    schema = yaml.safe_load(f)

# Validate required fields
required_app_fields = [
    'apiVersion', 'kind', 'metadata', 'spec'
]

for field in required_app_fields:
    if field not in app:
        print(f"ERROR: Missing required field in application.yaml: {field}")
        sys.exit(1)

# Validate spec
spec = app['spec']
required_spec_fields = ['inputSchema', 'deployment', 'properties', 'outputSchema']

for field in required_spec_fields:
    if field not in spec:
        print(f"ERROR: Missing required spec field: {field}")
        sys.exit(1)

print("Marketplace schema validation PASSED")
EOF
```

### 5.3 Review Process

#### 5.3.1 Google Marketplace Review Timeline

| Stage | Duration | Owner |
|-------|----------|-------|
| Submission | 1 day | Release Manager |
| Initial Review | 3-5 days | Google Marketplace Team |
| Technical Review | 5-7 days | Google Technical Team |
| Security Review | 3-5 days | Google Security Team |
| Approval | 1-2 days | Marketplace Operations |
| **Total** | **2-3 weeks** | |

#### 5.3.2 Common Review Issues

| Issue | Prevention | Resolution |
|-------|------------|------------|
| Missing documentation | Complete docs checklist | Add missing documentation |
| Security vulnerabilities | Pre-scan with Trivy | Fix and resubmit |
| Schema validation errors | Validate before submit | Fix schema errors |
| Image size too large | Optimize Dockerfile | Reduce image size |
| Broken deployment links | Test all deployments | Fix deployment scripts |

### 5.4 Post-Submission Actions

1. **Monitor Review Status**
   ```bash
   gcloud marketplace solutions list \
     --project=$PROJECT_ID \
     --filter="name=erlmcp"
   ```

2. **Address Review Feedback**
   - Create ticket for each feedback item
   - Fix issues in priority order
   - Resubmit when all issues resolved

3. **Prepare for Launch**
   - Finalize launch announcement
   - Prepare support team
   - Schedule monitoring during launch

---

## 6. Release Communication

### 6.1 Communication Plan

#### 6.1.1 Internal Communication

| Timing | Audience | Channel | Content |
|--------|----------|---------|---------|
| T-2 weeks | Engineering | Email/Slack | Release planning begins |
| T-1 week | All internal | Email | Release scope and timeline |
| T-3 days | Support | Email | Training and known issues |
| T-1 day | All internal | Email | Release announcement |
| T+1 day | All internal | Email | Release success/failure |
| T+1 week | All internal | Email | Stability update |

#### 6.1.2 External Communication

| Timing | Audience | Channel | Content |
|--------|----------|---------|---------|
| T-2 weeks | Enterprise customers | Email | Preview invitation |
| T-1 week | Community | Blog post | Release preview |
| T-Day | Public | GitHub release | Release notes |
| T+1 day | Public | Twitter/LinkedIn | Launch announcement |
| T+1 week | Public | Blog post | Launch recap |

### 6.2 Release Announcement Template

```markdown
# erlmcp v{VERSION} Released

We're excited to announce the release of erlmcp v{VERSION}!

## What's New

{HIGHLIGHTS}

## Key Features

- {FEATURE_1}
- {FEATURE_2}
- {FEATURE_3}

## Getting Started

### Google Cloud Marketplace
Deploy directly from the Marketplace:
https://console.cloud.google.com/marketplace/details/erlmcp

### Docker
```bash
docker pull gcr.io/erlmcp/erlmcp:{VERSION}
docker run -p 8080:8080 gcr.io/erlmcp/erlmcp:{VERSION}
```

### From Source
```bash
git clone https://github.com/banyan-platform/erlmcp
cd erlmcp
git checkout v{VERSION}
rebar3 release
```

## Upgrade Guide

{UPGRADE_GUIDE}

## Documentation

- Documentation: https://docs.erlmcp.dev
- API Reference: https://docs.erlmcp.dev/api
- Migration Guide: https://docs.erlmcp.dev/migration

## Support

- GitHub Issues: https://github.com/banyan-platform/erlmcp/issues
- Discord: https://discord.gg/erlmcp
- Email: support@erlmcp.dev

## Contributors

{CONTRIBUTORS}

Thank you to everyone who contributed to this release!
```

### 6.3 Incident Communication

#### 6.3.1 Severity Levels

| Severity | Definition | Response Time |
|----------|------------|---------------|
| P0 - Critical | Complete service outage | 15 minutes |
| P1 - High | Major feature broken | 1 hour |
| P2 - Medium | Partial degradation | 4 hours |
| P3 - Low | Minor issues | 1 business day |

#### 6.3.2 Incident Communication Template

```markdown
# Incident: {INCIDENT_TITLE}

**Status:** {STATUS}
**Severity:** {SEVERITY}
**Started:** {TIMESTAMP}
**Affected Versions:** {VERSIONS}
**Affected Services:** {SERVICES}

## Impact

{IMPACT_DESCRIPTION}

## Current Status

{CURRENT_STATUS}

## Next Steps

{NEXT_STEPS}

## Updates

| Time | Update |
|------|--------|
| {TIMESTAMP} | {UPDATE} |

We apologize for any inconvenience and appreciate your patience.
```

---

## 7. Rollback Procedures

### 7.1 Rollback Triggers

| Trigger | Severity | Action |
|---------|----------|--------|
| Critical bug in production | P0 | Immediate rollback |
| Security vulnerability | P0 | Immediate rollback |
| Data loss/corruption | P0 | Immediate rollback |
| Performance degradation > 50% | P1 | Consider rollback |
| Error rate > 10% | P1 | Consider rollback |
| Feature not working as expected | P2 | Hotfix preferred |

### 7.2 Rollback Decision Tree

```
Issue Detected
    |
    v
Severity Assessment
    |
    +-- P0 (Critical) -> Immediate Rollback
    |
    +-- P1 (High) -----> Rollback Decision
    |                      |
    |                      +-- Can hotfix in < 1 hour? -> Hotfix
    |                      |
    |                      +-- No -> Rollback
    |
    +-- P2 (Medium) ---> Monitor and Fix
    |
    +-- P3 (Low) ------> Fix in next release
```

### 7.3 Rollback Procedures

#### 7.3.1 Cloud Run Rollback

```bash
# List revisions
gcloud run revisions list \
  --service=erlmcp \
  --region=us-central1 \
  --format='value(REVISION)'

# Rollback to specific revision
gcloud run services update-traffic erlmcp \
  --to-revisions=REVISION=100 \
  --region=us-central1

# Verify rollback
curl https://erlmcp-abcdef.a.run.app/health
```

#### 7.3.2 GKE Rollback

```bash
# Get rollout history
kubectl rollout history deployment/erlmcp -n erlmcp

# Rollback to previous revision
kubectl rollout undo deployment/erlmcp -n erlmcp

# Rollback to specific revision
kubectl rollout undo deployment/erlmcp -n erlmcp --to-revision=3

# Verify rollback
kubectl rollout status deployment/erlmcp -n erlmcp
```

#### 7.3.3 Marketplace Listing Rollback

```bash
# Update marketplace schema to point to previous image
cd marketplace/gcp/marketplace-schema

# Update image_tag in schema.yaml
sed -i 's/image_tag: "3.0.0"/image_tag: "2.9.9"/' schema.yaml

# Re-submit marketplace listing
gcloud marketplace solutions publish \
  --package=erlmcp \
  --project=$PROJECT_ID
```

### 7.4 Rollback Verification

After rollback, verify:

- [ ] Health endpoints return 200 OK
- [ ] Error rates return to baseline
- [ ] Performance metrics recover
- [ ] No new errors in logs
- [ ] User complaints stop

### 7.5 Post-Rollback Actions

1. **Root Cause Analysis**
   - Document what went wrong
   - Identify trigger points
   - Create prevention measures

2. **Hotfix Planning**
   - Fix the underlying issue
   - Test thoroughly
   - Deploy with extra monitoring

3. **Process Improvement**
   - Update release playbook
   - Add new quality gates if needed
   - Retrospective with team

---

## 8. Post-Release Monitoring

### 8.1 Monitoring Dashboard

Key metrics to monitor post-release:

#### 8.1.1 Health Metrics

| Metric | Threshold | Alert |
|--------|-----------|-------|
| Health Check Success Rate | > 99.9% | < 99% |
| Uptime | > 99.95% | < 99.9% |
| Response Time (p95) | < 500ms | > 1s |
| Error Rate | < 0.1% | > 1% |

#### 8.1.2 Performance Metrics

| Metric | Threshold | Alert |
|--------|-----------|-------|
| Throughput | Baseline +/- 10% | > 20% change |
| Memory Usage | < 80% | > 90% |
| CPU Usage | < 70% | > 85% |
| Connection Count | Baseline +/- 20% | > 50% change |

#### 8.1.3 Business Metrics

| Metric | Threshold | Alert |
|--------|-----------|-------|
| Deployment Success Rate | > 99% | < 95% |
| User Adoption | Increasing | Decreasing |
| Support Tickets | Baseline | > 2x baseline |
| Churn | < 1% | > 2% |

### 8.2 Monitoring Commands

```bash
# Cloud Run metrics
gcloud run services describe erlmcp \
  --region=us-central1 \
  --format='value(status.latestReadyRevisionName)'

# GKE metrics
kubectl top pods -n erlmcp
kubectl get hpa -n erlmcp

# Cloud Monitoring
gcloud monitoring metrics list \
  --filter='resource.type="cloud_run_revision"'

# Log queries
gcloud logging read 'resource.type=cloud_run_revision AND labels.service=erlmcp' \
  --limit=50 \
  --format='table(timestamp,textPayload)'
```

### 8.3 Alert Configuration

```yaml
# File: marketplace/gcp/config/alerts.yaml
alerts:
  - name: high_error_rate
    condition: error_rate > 0.01
    duration: 5m
    channels:
      - pagerduty
      - slack

  - name: high_latency
    condition: latency_p95 > 1000
    duration: 10m
    channels:
      - slack

  - name: health_check_failure
    condition: health_success < 0.99
    duration: 2m
    channels:
      - pagerduty
      - email
```

### 8.4 First 24 Hours Monitoring Plan

| Time | Activity | Owner |
|------|----------|-------|
| T+1 hour | Verify deployment health | Release Manager |
| T+2 hours | Check error rates | QA Lead |
| T+4 hours | Review performance metrics | Engineering Lead |
| T+8 hours | First stability check | Operations Lead |
| T+12 hours | Mid-day review | Release Manager |
| T+24 hours | 24-hour milestone | All stakeholders |

### 8.5 First Week Monitoring Plan

| Day | Focus | Success Criteria |
|-----|-------|------------------|
| Day 1 | Critical issues | No P0/P1 incidents |
| Day 2 | Performance | Baseline performance maintained |
| Day 3 | User feedback | Positive sentiment > 80% |
| Day 4 | Adoption | Expected adoption rate met |
| Day 5 | Stability | Zero unexpected incidents |
| Day 6-7 | Trend analysis | Metrics trending positive |

---

## 9. Checklists

### 9.1 Pre-Release Checklist

**Release Information**
- [ ] Release version defined
- [ ] Release branch created
- [ ] Release notes drafted
- [ ] Breaking changes documented

**Code Quality**
- [ ] All code merged to release branch
- [ ] Zero compilation errors
- [ ] Zero dialyzer errors
- [ ] Zero xref errors
- [ ] Test coverage >= 80%

**Testing**
- [ ] Unit tests passing (100%)
- [ ] Integration tests passing (100%)
- [ ] CT suite passing (100%)
- [ ] Performance tests passing
- [ ] Security tests passing

**Security**
- [ ] Vulnerability scan clean (0 CRITICAL/HIGH)
- [ ] Secret scan clean (0 secrets)
- [ ] License scan clean (0 violations)
- [ ] SBOM generated
- [ ] Provenance signature attached

**Artifacts**
- [ ] Container image built
- [ ] Container image pushed to registry
- [ ] Image tagged with version
- [ ] Image tagged as latest
- [ ] SBOM published

**Documentation**
- [ ] Release notes published
- [ ] API documentation updated
- [ ] Deployment guide updated
- [ ] Migration guide (if needed)
- [ ] Troubleshooting guide current

**Marketplace**
- [ ] Schema files validated
- [ ] Terraform modules validate
- [ ] Helm charts lint
- [ ] Deployment scripts tested
- [ ] README complete

**Approvals**
- [ ] Engineering Lead approval
- [ ] Security Lead approval
- [ ] QA Lead approval
- [ ] Product Manager approval
- [ ] Release Manager approval

### 9.2 Release Day Checklist

**Pre-Release (T-2 hours)**
- [ ] Final smoke tests pass
- [ ] Staging deployment verified
- [ ] Communication sent to support team
- [ ] On-call engineer notified

**Release (T-0)**
- [ ] Release tagged in git
- [ ] Tag pushed to remote
- [ ] Container images published
- [ ] GitHub release published

**Post-Release (T+1 hour)**
- [ ] Health checks passing
- [ ] Error rates normal
- [ ] Performance baseline met
- [ ] No critical issues reported

### 9.3 Marketplace Submission Checklist

**Technical Requirements**
- [ ] Container image in Artifact Registry
- [ ] Image size < 500MB
- [ ] Health endpoints documented
- [ ] Startup time < 5 minutes
- [ ] No hardcoded credentials

**Schema Files**
- [ ] application.yaml valid
- [ ] schema.yaml valid
- [ ] parameters.yaml valid
- [ ] metadata.display.yaml complete
- [ ] All required fields present

**Deployment**
- [ ] Terraform validates
- [ ] Terraform can deploy successfully
- [ ] Terraform can destroy successfully
- [ ] Helm chart installs
- [ ] Helm chart upgrades

**Documentation**
- [ ] README.md complete
- [ ] Architecture documented
- [ ] API documented
- [ ] Configuration documented
- [ ] Support information available

**Testing**
- [ ] Deployment tested (Cloud Run)
- [ ] Deployment tested (GKE)
- [ ] Deployment tested (GCE)
- [ ] Health check verified
- [ ] Cleanup verified

### 9.4 Rollback Checklist

**Decision**
- [ ] Severity assessed (P0/P1)
- [ ] Impact documented
- [ ] Rollback decision made
- [ ] Stakeholders notified

**Execution**
- [ ] Rollback procedure executed
- [ ] Health checks verified
- [ ] Metrics恢复正常
- [ ] Users notified

**Post-Rollback**
- [ ] Root cause identified
- [ ] Hotfix planned
- [ ] Prevention measures documented
- [ ] Retrospective scheduled

---

## 10. Appendices

### 10.1 Release Metrics Dashboard

Key metrics tracked for each release:

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Release Time | 4 hours | TBD | TBD |
| Rollback Frequency | 0 | TBD | TBD |
| Critical Bugs | 0 | TBD | TBD |
| Time to Stable | 7 days | TBD | TBD |
| Customer Satisfaction | > 90% | TBD | TBD |

### 10.2 Contact Information

| Role | Name | Email | Slack |
|------|------|-------|-------|
| Release Manager | TBD | TBD | TBD |
| Engineering Lead | TBD | TBD | TBD |
| Security Lead | TBD | TBD | TBD |
| QA Lead | TBD | TBD | TBD |
| Product Manager | TBD | TBD | TBD |
| Operations Lead | TBD | TBD | TBD |

### 10.3 Useful Commands

```bash
# Release validation
cd marketplace/gcp && bash scripts/run-marketplace-validation.sh

# Security scan
bash scripts/scan-image.sh --project $PROJECT_ID --mode strict

# Schema validation
bash scripts/validate-schema.sh

# Build and push
bash scripts/build-and-push.sh --project $PROJECT_ID --tag 3.0.0

# Cloud Run deploy
gcloud run deploy erlmcp \
  --image us-central1-docker.pkg.dev/$PROJECT_ID/erlmcp/erlmcp:3.0.0 \
  --region us-central1

# GKE deploy
kubectl apply -f marketplace/gcp/helm/erlmcp-marketplace/
```

### 10.4 Related Documents

- [Release Notes Template](/templates/release_notes.md)
- [Marketplace README](/marketplace/gcp/README.md)
- [Validation Workflow](/marketplace/gcp/test-evidence/validation-workflow.md)
- [Test Strategy Report](/marketplace/gcp/test-evidence/test-strategy-report.md)
- [Security Assessment](/marketplace/gcp/test-evidence/security-assessment-report.md)

### 10.5 Glossary

| Term | Definition |
|------|------------|
| SBOM | Software Bill of Materials |
| VEX | Vulnerability Exploitability Exchange |
| CT | Common Test (Erlang test framework) |
| GKE | Google Kubernetes Engine |
| GCE | Google Compute Engine |
| HA | High Availability |
| MTTR | Mean Time To Recover |
| SLA | Service Level Agreement |
| SLO | Service Level Objective |

### 10.6 Change Log

| Version | Date | Changes | Author |
|---------|------|---------|--------|
| 1.0 | 2026-02-02 | Initial release | Release Manager |

---

**Document Control**

- **Owner:** Release Manager
- **Review Frequency:** Quarterly
- **Next Review:** 2026-05-02
- **Approved By:** [Pending]

---

*This playbook is a living document. Please submit suggestions and improvements to the Release Manager.*
