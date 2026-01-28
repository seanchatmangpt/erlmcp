# GitHub Branch Protection Setup Guide

## Overview

This guide documents how to configure GitHub branch protection rules to enforce blocking quality gates for erlmcp.

---

## Quick Setup (GitHub Web UI)

### 1. Navigate to Branch Protection Settings

1. Go to your GitHub repository: `https://github.com/YOUR_ORG/erlmcp`
2. Click **Settings** (top right)
3. Click **Branches** (left sidebar under "Code and automation")
4. Click **Add branch protection rule** (or **Edit** if rule exists)

### 2. Configure Protection Rule

**Branch name pattern:**
```
main
```

**Required settings:**

#### ✅ Protect matching branches
- ☑ **Require a pull request before merging**
  - ☐ Require approvals: 0 (optional: set to 1 for code review)
  - ☑ Dismiss stale pull request approvals when new commits are pushed
  - ☑ Require review from Code Owners (optional)

- ☑ **Require status checks to pass before merging**
  - ☑ Require branches to be up to date before merging
  - **Required status checks:**
    - `test (25)` - Erlang/OTP 25 tests
    - `test (26)` - Erlang/OTP 26 tests (canonical)
    - `test (27)` - Erlang/OTP 27 tests
    - `test (28)` - Erlang/OTP 28 tests (optional)
    - `Quality Gates Summary` - From ci.yml
    - `Comprehensive Quality Gate (Blocking)` - From quality-gate.yml
    - `Benchmark Regression Analysis (Blocking)` - From block-on-regression.yml
    - `Documentation Linter` - From ci.yml
    - `Verify Umbrella Structure` - From ci.yml

- ☑ **Require conversation resolution before merging** (optional)

- ☑ **Require linear history** (optional - prevents merge commits)

- ☑ **Do not allow bypassing the above settings** (recommended)
  - Unchecked: Admins can bypass for emergencies
  - Checked: Even admins must pass quality gates

#### ⚠️ Additional options (optional)

- ☐ Require signed commits
- ☐ Require deployments to succeed before merging
- ☐ Lock branch (prevents all changes)
- ☐ Restrict who can push to matching branches

### 3. Save Protection Rule

Click **Create** (or **Save changes**)

---

## Automated Setup (GitHub CLI)

### Prerequisites

Install GitHub CLI: https://cli.github.com/

```bash
# Login to GitHub
gh auth login
```

### Create Branch Protection Rule

```bash
# Navigate to repository root
cd /path/to/erlmcp

# Create branch protection rule
gh api repos/:owner/:repo/branches/main/protection \
  --method PUT \
  --field required_status_checks[strict]=true \
  --field required_status_checks[contexts][]=test (25) \
  --field required_status_checks[contexts][]=test (26) \
  --field required_status_checks[contexts][]=test (27) \
  --field required_status_checks[contexts][]=test (28) \
  --field required_status_checks[contexts][]="Quality Gates Summary" \
  --field required_status_checks[contexts][]="Comprehensive Quality Gate (Blocking)" \
  --field required_status_checks[contexts][]="Benchmark Regression Analysis (Blocking)" \
  --field required_status_checks[contexts][]="Documentation Linter" \
  --field required_status_checks[contexts][]="Verify Umbrella Structure" \
  --field enforce_admins=false \
  --field required_pull_request_reviews[dismiss_stale_reviews]=true \
  --field required_pull_request_reviews[require_code_owner_reviews]=false \
  --field required_pull_request_reviews[required_approving_review_count]=0 \
  --field required_conversation_resolution=true \
  --field required_linear_history=false \
  --field allow_force_pushes=false \
  --field allow_deletions=false
```

**Note:** Replace `:owner` and `:repo` with your GitHub username/org and repository name.

### Verify Protection Rule

```bash
gh api repos/:owner/:repo/branches/main/protection
```

---

## Automated Setup (Terraform - Infrastructure as Code)

### Prerequisites

Install Terraform: https://www.terraform.io/downloads

### Create `github_branch_protection.tf`

```hcl
terraform {
  required_providers {
    github = {
      source  = "integrations/github"
      version = "~> 5.0"
    }
  }
}

provider "github" {
  token = var.github_token  # Set via TF_VAR_github_token or .tfvars
  owner = var.github_owner  # Your GitHub org/user
}

variable "github_token" {
  description = "GitHub personal access token"
  type        = string
  sensitive   = true
}

variable "github_owner" {
  description = "GitHub organization or user"
  type        = string
}

resource "github_branch_protection" "main" {
  repository_id = "erlmcp"  # Repository name
  pattern       = "main"

  required_status_checks {
    strict   = true  # Require branches to be up to date
    contexts = [
      "test (25)",
      "test (26)",
      "test (27)",
      "test (28)",
      "Quality Gates Summary",
      "Comprehensive Quality Gate (Blocking)",
      "Benchmark Regression Analysis (Blocking)",
      "Documentation Linter",
      "Verify Umbrella Structure"
    ]
  }

  required_pull_request_reviews {
    dismiss_stale_reviews           = true
    require_code_owner_reviews      = false
    required_approving_review_count = 0  # Set to 1 for mandatory reviews
  }

  enforce_admins                = false  # Allow admin bypass for emergencies
  require_conversation_resolution = true
  require_signed_commits          = false
  required_linear_history         = false

  allow_force_pushes = false
  allow_deletions    = false
}
```

### Apply Configuration

```bash
# Initialize Terraform
terraform init

# Preview changes
terraform plan

# Apply branch protection
terraform apply
```

---

## Required Status Checks Explained

### From `.github/workflows/ci.yml`

| Status Check | Purpose | Blocking |
|--------------|---------|----------|
| `test (25)` | CI on Erlang/OTP 25 | ✅ YES |
| `test (26)` | CI on Erlang/OTP 26 (canonical) | ✅ YES |
| `test (27)` | CI on Erlang/OTP 27 | ✅ YES |
| `test (28)` | CI on Erlang/OTP 28 (experimental) | ⚠️ OPTIONAL |
| `Quality Gates Summary` | Final summary of all gates | ✅ YES |
| `Documentation Linter` | Docs lint checks | ⚠️ WARNING |
| `Verify Umbrella Structure` | Umbrella app structure | ✅ YES |

### From `.github/workflows/quality-gate.yml`

| Status Check | Purpose | Blocking |
|--------------|---------|----------|
| `Comprehensive Quality Gate (Blocking)` | Single-job quality check | ✅ YES |

**Sub-gates (reported in job):**
- Gate 1: Compilation
- Gate 2: Xref
- Gate 3: Dialyzer
- Gate 4: Unit Tests (≥90%)
- Gate 5: Coverage (≥80%)
- Gate 6: Integration Tests (non-blocking)

### From `.github/workflows/block-on-regression.yml`

| Status Check | Purpose | Blocking |
|--------------|---------|----------|
| `Benchmark Regression Analysis (Blocking)` | Performance regression check | ✅ YES |

**Checks:**
- Throughput degradation <10% vs. base branch

---

## Enabling Status Checks

### First-Time Setup

Status checks **won't appear** in the branch protection UI until they've run at least once.

**To populate status checks:**

1. Create a test PR (any small change)
2. Wait for all workflows to run
3. Go to branch protection settings
4. Search for status checks by name
5. Select required checks
6. Save

**Alternative:** Push a commit to `main` to trigger workflows.

### Adding New Status Checks

1. Add workflow to `.github/workflows/`
2. Push to trigger workflow
3. Update branch protection rule
4. Add new status check to required list

---

## Emergency Bypass Procedures

### Scenario: Production Hotfix

**When to bypass:**
- Critical security vulnerability
- Production-breaking bug
- Data loss prevention

**How to bypass (Admin only):**

1. **Option A: Merge without requirements**
   - Click "Merge pull request" dropdown
   - Select "Merge without waiting for requirements to be met"
   - Document reason in merge commit message

2. **Option B: Temporarily disable protection**
   - Settings → Branches → Edit rule
   - Uncheck "Require status checks to pass"
   - Merge PR
   - Re-enable protection immediately

3. **Option C: Use hotfix branch**
   - Create `hotfix/*` branch (not protected)
   - Merge to main with admin override
   - Create follow-up PR to fix quality gates

**Required documentation:**
```
Bypass Reason: [Critical security fix / Production outage / etc.]
Bypassed Gates: [Which gates failed]
Follow-up Issue: #123
Authorized By: @username
Date: YYYY-MM-DD
```

**Post-bypass actions:**
1. Create follow-up issue to fix quality gates
2. Tag with `tech-debt` label
3. Document in incident log
4. Schedule fix within 1 sprint

---

## Monitoring Branch Protection

### Check Protection Status

```bash
# Via GitHub CLI
gh api repos/:owner/:repo/branches/main/protection

# Via web UI
Settings → Branches → View rule
```

### Audit Bypass Events

```bash
# View recent pushes to main
gh api repos/:owner/:repo/branches/main/commits

# Check for force pushes (should be empty)
gh api repos/:owner/:repo/events --jq '.[] | select(.type == "ForcePushEvent")'
```

### Monitor Required Checks

```bash
# List status checks on latest commit
gh api repos/:owner/:repo/commits/main/status

# Check if commit passed all checks
gh api repos/:owner/:repo/commits/$(git rev-parse HEAD)/check-runs
```

---

## Troubleshooting

### Issue: "Required status check not found"

**Cause:** Workflow hasn't run yet, or job name mismatch

**Fix:**
1. Trigger workflow by pushing a commit
2. Verify job names in `.github/workflows/*.yml`
3. Update branch protection rule with correct names

### Issue: "Cannot enable required status checks"

**Cause:** No workflows have run on the repository

**Fix:**
1. Push any commit to `main` to trigger workflows
2. Wait for workflows to complete
3. Retry enabling status checks

### Issue: "Admins can still merge without checks"

**Cause:** `enforce_admins` is disabled

**Fix:**
1. Edit branch protection rule
2. Check "Do not allow bypassing the above settings"
3. Save changes

**Note:** This prevents emergency bypasses. Only enable if you have alternative emergency procedures.

### Issue: "Status check keeps failing but code is correct"

**Cause:** Transient CI failure or flaky test

**Fix:**
1. Re-run failed checks in GitHub Actions UI
2. If persistent, debug locally: `make check-full`
3. Update workflow if CI environment issue

---

## Best Practices

### 1. Start with Warnings, Then Enforce

**Phase 1:** Enable status checks but don't block merges
- Developers get familiar with gates
- Identify common issues

**Phase 2:** Block merges but allow admin bypass
- Quality gates enforced
- Emergency escape hatch available

**Phase 3:** Block merges for everyone (optional)
- Zero exceptions
- Requires robust testing infrastructure

### 2. Use Separate Branches for Different Enforcement Levels

| Branch | Protection Level |
|--------|------------------|
| `main` | Full enforcement (all gates) |
| `release/*` | Full enforcement (all gates) |
| `develop` | Partial enforcement (compilation + tests only) |
| `feature/*` | No enforcement (developer freedom) |
| `hotfix/*` | No enforcement (emergency bypass) |

### 3. Monitor Bypass Frequency

**Target:** <1 bypass per month

**Actions if exceeded:**
- Review bypass reasons
- Improve CI reliability
- Educate developers
- Add pre-commit hooks

### 4. Keep Status Checks Synchronized

When adding/removing workflows:
1. Update `.github/workflows/`
2. Update branch protection rules
3. Update documentation
4. Communicate to team

### 5. Test Branch Protection Changes

**Before applying to `main`:**
1. Create test branch: `test/branch-protection`
2. Apply protection rule
3. Create test PR
4. Verify gates block as expected
5. Apply to `main`

---

## Summary

**Branch protection is configured via:**
- GitHub web UI (manual)
- GitHub CLI (scripted)
- Terraform (infrastructure as code)

**Required status checks:**
- 9 total checks (7 blocking, 2 warnings)
- Cover compilation, tests, coverage, performance, docs, structure

**Emergency bypass:**
- Admin override available
- Must document reason
- Create follow-up issue

**Monitoring:**
- Audit bypass events
- Track status check pass rates
- Monitor CI reliability

**Next steps:**
1. Apply branch protection to `main`
2. Test with sample PR
3. Document bypass procedures
4. Monitor and iterate

**Questions?** See `BLOCKING_QUALITY_GATES.md` or open an issue.
