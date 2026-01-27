# TCPS CI/CD Setup Guide

**Complete guide to setting up the Toyota Code Production System (TCPS) CI/CD pipelines**

---

## Overview

The TCPS CI/CD integration provides production-grade pipelines for:

- **GitHub Actions** - Cloud-native CI/CD with GitHub workflows
- **GitLab CI** - Complete GitLab CI/CD integration
- **Jenkins** - Enterprise Jenkins pipeline support
- **Local Development** - Pre-commit hooks for quality gates

All pipelines implement TCPS principles:
- ✅ Pull-based workflow (Work Orders)
- ✅ Jidoka (stop-the-line on defects)
- ✅ Quality gates (80% coverage, 80% pass rate)
- ✅ Deterministic builds
- ✅ Evidence receipts
- ✅ Kaizen reporting

---

## Quick Start

### Prerequisites

All CI/CD environments require:

1. **Erlang/OTP 27.1+**
2. **Rebar3 3.23.0+**
3. **Python 3.12+** (for SHACL validation)
4. **Git** (for work order tracking)

Python dependencies:
```bash
pip install rdflib pyshacl pytest pytest-cov
```

---

## GitHub Actions Setup

### 1. Configuration

The workflow is located at `.github/workflows/tcps.yml`.

**No additional setup required!** The workflow automatically runs on:
- Push to `main`, `develop`, `feature/*`, `release/*`
- Pull requests to `main`, `develop`
- Manual workflow dispatch

### 2. Secrets Configuration (Optional)

Set these repository secrets for notifications:

```
Settings → Secrets → Actions → New repository secret
```

| Secret | Description | Required |
|--------|-------------|----------|
| `SLACK_WEBHOOK_URL` | Slack webhook for Andon alerts | No |
| `TCPS_DASHBOARD_URL` | TCPS dashboard endpoint | No |

### 3. Quality Gate Configuration

Edit environment variables in `.github/workflows/tcps.yml`:

```yaml
env:
  TCPS_QUALITY_GATE_COVERAGE: 80    # Minimum coverage %
  TCPS_QUALITY_GATE_PASS_RATE: 80   # Minimum test pass rate %
```

### 4. Pipeline Stages

The GitHub Actions pipeline runs 7 stages:

1. **Pull Signal** - Create work order
2. **SHACL Validation** - Ontology validation (Jidoka)
3. **Compile** - Build with Dialyzer
4. **Test** - Quality gates (coverage, pass rate)
5. **Deterministic Build** - Verify reproducibility
6. **Release** - Production build (main/release branches only)
7. **Kaizen** - Continuous improvement report

### 5. Viewing Results

- **Actions Tab**: View pipeline runs
- **Artifacts**: Download receipts, coverage reports
- **Pull Requests**: Kaizen report automatically commented

### 6. Skipping Andon (Emergency Only)

Trigger workflow dispatch with:
```yaml
skip_andon: true
```

---

## GitLab CI Setup

### 1. Configuration

The pipeline is located at `.gitlab-ci.yml` (root directory).

**Automatic activation!** GitLab CI detects the file automatically.

### 2. Variables Configuration

Set these CI/CD variables:

```
Settings → CI/CD → Variables
```

| Variable | Description | Required |
|----------|-------------|----------|
| `SLACK_WEBHOOK_URL` | Slack webhook | No |
| `EMAIL_FROM` | Notification sender email | No |
| `EMAIL_TO` | Notification recipient email | No |
| `TCPS_DASHBOARD_URL` | Dashboard endpoint | No |

### 3. Docker Runner Configuration

Ensure GitLab Runner has Docker executor:

```yaml
# /etc/gitlab-runner/config.toml
[[runners]]
  executor = "docker"
  [runners.docker]
    image = "erlang:27.1"
    privileged = false
```

### 4. Pipeline Stages

GitLab CI runs 8 stages:

1. **pull-signal** - Create work order
2. **validate** - SHACL validation
3. **build** - Compile + Dialyzer
4. **test** - Quality gates
5. **verify** - Deterministic build
6. **release** - Production build
7. **publish** - (Reserved for Hex publish)
8. **kaizen** - Improvement report

### 5. Artifacts

Receipts and reports are stored as GitLab artifacts (30-90 days retention).

Download from:
```
Pipeline → Job → Browse → Download
```

### 6. Coverage Visualization

GitLab extracts coverage from pipeline output:

```yaml
coverage: '/Total: (\d+)%/'
```

Displayed in **Merge Requests → Pipelines**.

---

## Jenkins Setup

### 1. Configuration

The pipeline is located at `Jenkinsfile` (root directory).

### 2. Create Jenkins Pipeline Job

1. **New Item** → **Pipeline**
2. **Pipeline** section:
   - Definition: **Pipeline script from SCM**
   - SCM: **Git**
   - Repository URL: `<your-repo-url>`
   - Script Path: `Jenkinsfile`

### 3. Install Plugins (if missing)

Required Jenkins plugins:
- Git plugin
- Pipeline plugin
- JUnit plugin
- Artifacts plugin

### 4. Environment Setup

Ensure Jenkins agent has:
- Erlang/OTP 27.1+
- Rebar3
- Python 3.12+
- Git

### 5. Credentials

Add Jenkins credentials for notifications:

```
Manage Jenkins → Credentials → System → Global credentials
```

| ID | Type | Description |
|----|------|-------------|
| `slack-webhook` | Secret text | Slack webhook URL |
| `tcps-dashboard-url` | Secret text | Dashboard endpoint |

Update `Jenkinsfile` to use credentials:

```groovy
environment {
    SLACK_WEBHOOK_URL = credentials('slack-webhook')
    TCPS_DASHBOARD_URL = credentials('tcps-dashboard-url')
}
```

### 6. Triggering Builds

**Automatic:**
- GitHub/GitLab webhooks
- SCM polling: `H/15 * * * *` (every 15 min)

**Manual:**
- Click **Build Now**

### 7. Viewing Results

- **Build History** → Select build → **Console Output**
- **Artifacts** → Download receipts
- **Test Results** → JUnit reports

---

## Local Development - Pre-Commit Hooks

### 1. Installation

Install the pre-commit hook:

```bash
# Make executable
chmod +x ci/pre-commit

# Symlink to .git/hooks
ln -s ../../ci/pre-commit .git/hooks/pre-commit
```

### 2. What It Checks

The pre-commit hook runs 5 quality gates:

1. **SHACL Validation** - Ontology constraints
2. **Compilation** - No syntax errors
3. **Type Checking** - Dialyzer (if PLT exists)
4. **Unit Tests** - Quick smoke test
5. **Security Scan** - No secrets in commits

### 3. Skipping (Emergency Only)

Skip pre-commit checks:

```bash
git commit --no-verify -m "Emergency fix"
```

**⚠️ Warning:** Skipping pre-commit breaks stop-the-line discipline!

### 4. Building Dialyzer PLT

First-time setup (takes ~5 minutes):

```bash
rebar3 dialyzer
```

This creates the PLT cache for faster type checking.

---

## Docker Integration

### 1. Building TCPS Container

Build the Docker image:

```bash
docker build -f ci/Dockerfile.tcps -t tcps-pipeline:latest .
```

### 2. Running Pipeline in Docker

Run the full TCPS pipeline:

```bash
docker run --rm \
  -v $(pwd):/workspace \
  -e TCPS_QUALITY_GATE_COVERAGE=80 \
  -e TCPS_ENABLE_ANDON=true \
  tcps-pipeline:latest
```

### 3. Interactive Container

For debugging:

```bash
docker run -it --rm \
  -v $(pwd):/workspace \
  tcps-pipeline:latest \
  /bin/bash
```

Then manually run stages:

```bash
# Inside container
./ci/run-tcps-pipeline.sh
```

### 4. Docker Compose (Optional)

Create `docker-compose.tcps.yml`:

```yaml
version: '3.8'

services:
  tcps-pipeline:
    build:
      context: .
      dockerfile: ci/Dockerfile.tcps
    volumes:
      - .:/workspace
    environment:
      - TCPS_QUALITY_GATE_COVERAGE=80
      - TCPS_ENABLE_ANDON=true
      - SLACK_WEBHOOK_URL=${SLACK_WEBHOOK_URL}
    command: ./ci/run-tcps-pipeline.sh
```

Run:

```bash
docker-compose -f docker-compose.tcps.yml up
```

---

## Standalone Pipeline Script

### 1. Direct Execution

Run the TCPS pipeline locally without CI/CD:

```bash
# Make executable
chmod +x ci/run-tcps-pipeline.sh

# Run pipeline
./ci/run-tcps-pipeline.sh
```

### 2. Configuration

Set environment variables before running:

```bash
export TCPS_QUALITY_GATE_COVERAGE=80
export TCPS_QUALITY_GATE_PASS_RATE=80
export TCPS_ENABLE_ANDON=true
export TCPS_ENABLE_RECEIPTS=true

./ci/run-tcps-pipeline.sh
```

### 3. Output

Pipeline generates:
- **Receipts**: `tcps_receipts/*.json`
- **Work Order**: `tcps_receipts/work_order.ttl`
- **Andon Events**: `tcps_receipts/andon_*.ttl`
- **Kaizen Report**: `tcps_receipts/kaizen_report.md`

---

## Notification Integration

### 1. Slack Notifications

Set up Slack webhook:

1. Go to: https://api.slack.com/messaging/webhooks
2. Create **Incoming Webhook** for your channel
3. Copy webhook URL

Set in CI/CD:

```bash
# GitHub Actions
SLACK_WEBHOOK_URL=https://hooks.slack.com/services/YOUR/WEBHOOK/URL

# GitLab CI
SLACK_WEBHOOK_URL (CI/CD variable)

# Jenkins
slack-webhook (credential)

# Local/Docker
export SLACK_WEBHOOK_URL="https://hooks.slack.com/services/..."
```

### 2. Email Notifications

Configure SMTP settings:

```bash
export EMAIL_SMTP_SERVER="smtp.gmail.com"
export EMAIL_SMTP_PORT="587"
export EMAIL_FROM="tcps@example.com"
export EMAIL_TO="team@example.com"
export EMAIL_USERNAME="your-email@gmail.com"
export EMAIL_PASSWORD="your-app-password"
```

**Note:** For Gmail, use [App Passwords](https://support.google.com/accounts/answer/185833).

### 3. Dashboard Updates (SSE)

If you have a TCPS dashboard:

```bash
export TCPS_DASHBOARD_URL="https://tcps-dashboard.example.com"
```

The pipeline sends events to `${TCPS_DASHBOARD_URL}/events`.

### 4. Manual Notifications

Send custom notifications:

```bash
./ci/tcps-notify.sh andon critical "Custom message" WO-123456
```

---

## Quality Gates Configuration

### Adjusting Thresholds

Edit in pipeline configuration:

**GitHub Actions** (`.github/workflows/tcps.yml`):
```yaml
env:
  TCPS_QUALITY_GATE_COVERAGE: 80    # Adjust here
  TCPS_QUALITY_GATE_PASS_RATE: 80
```

**GitLab CI** (`.gitlab-ci.yml`):
```yaml
variables:
  TCPS_QUALITY_GATE_COVERAGE: "80"
  TCPS_QUALITY_GATE_PASS_RATE: "80"
```

**Jenkins** (`Jenkinsfile`):
```groovy
environment {
    TCPS_QUALITY_GATE_COVERAGE = '80'
    TCPS_QUALITY_GATE_PASS_RATE = '80'
}
```

**Local/Docker**:
```bash
export TCPS_QUALITY_GATE_COVERAGE=80
export TCPS_QUALITY_GATE_PASS_RATE=80
```

### Lean Six Sigma Targets

**TCPS follows Lean Six Sigma defect rates:**

| Quality Level | Coverage | Pass Rate | Defect Rate |
|---------------|----------|-----------|-------------|
| **Standard** | 80% | 80% | 3.4% |
| **High** | 90% | 95% | 0.62% |
| **Six Sigma** | 99% | 99.99966% | 0.00034% |

Start with **Standard** and increase as team capability improves (Kaizen).

---

## Troubleshooting

See [CI/CD Troubleshooting Guide](./CI_CD_TROUBLESHOOTING.md) for common issues.

---

## Best Practices

### 1. Start Small, Scale Up

Begin with:
- Pre-commit hooks (local quality)
- GitHub Actions (cloud CI/CD)
- 80% coverage threshold

Then add:
- GitLab/Jenkins (multi-platform)
- Notifications (Slack, email)
- Higher thresholds (90%+)

### 2. Monitor Takt Time

Track pipeline duration in Kaizen reports. Optimize stages taking > 10% of total time.

### 3. Review Andon Events

Every Andon trigger is a learning opportunity:
- Root cause analysis (5 Whys)
- Update standard work
- Prevent recurrence

### 4. Continuous Improvement

**Weekly:**
- Review Kaizen reports
- Adjust quality thresholds
- Update documentation

**Monthly:**
- Analyze trends
- Implement optimizations
- Team retrospectives

### 5. Stop-the-Line Discipline

**Never skip quality gates** except in true emergencies:
- Production outage
- Security incident
- Critical data loss

Document all skips and perform post-incident reviews.

---

## Next Steps

1. ✅ Install pre-commit hooks
2. ✅ Configure GitHub Actions (or GitLab/Jenkins)
3. ✅ Set quality gate thresholds
4. ✅ Run first pipeline
5. ✅ Review Kaizen report
6. ✅ Add notifications
7. ✅ Document team workflow

---

## Support

- **Documentation**: `docs/`
- **Issues**: File GitHub/GitLab issues
- **Team Chat**: Slack `#tcps-pipeline`

---

**Generated by TCPS v1.0.0**
