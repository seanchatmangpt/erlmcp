# CI/CD Quality Gates Documentation

## Overview

This directory contains documentation for erlmcp's blocking quality gate system that enforces Lean Six Sigma standards through automated CI/CD.

---

## Quick Links

### For Developers
- **[BLOCKING_QUALITY_GATES.md](BLOCKING_QUALITY_GATES.md)** - How quality gates work and how to fix failures
- **[Local Development Workflow](BLOCKING_QUALITY_GATES.md#local-development-workflow)** - Commands to run before pushing

### For Repository Admins
- **[BRANCH_PROTECTION_SETUP.md](BRANCH_PROTECTION_SETUP.md)** - How to configure GitHub branch protection
- **[Emergency Bypass Process](BLOCKING_QUALITY_GATES.md#emergency-bypass-process)** - For production hotfixes

---

## Quality Gate Summary

| # | Gate | Threshold | Blocks Merge? |
|---|------|-----------|---------------|
| 1 | Compilation | 0 errors | ✅ YES |
| 2 | Xref | 0 undefined functions | ✅ YES |
| 3 | Dialyzer | 0 type errors | ✅ YES |
| 4 | Unit Tests | ≥90% pass rate | ✅ YES |
| 5 | Coverage | ≥80% overall | ✅ YES |
| 6 | Performance | <10% regression | ✅ YES |
| 7 | Integration Tests | All pass | ⚠️ WARNING |
| 8 | Documentation | No legacy refs | ⚠️ WARNING |
| 9 | Umbrella Structure | All apps | ✅ YES |

---

## Workflow Files

### `.github/workflows/ci.yml`
**Main CI pipeline** - Runs on all pushes and PRs

**Features:**
- Tests on Erlang/OTP 25, 26, 27, 28
- Blocks merge on gate failures
- Generates coverage reports
- Runs quick benchmark smoke test

**Required status checks:**
- `test (25)`, `test (26)`, `test (27)`, `test (28)`
- `Quality Gates Summary`

### `.github/workflows/quality-gate.yml`
**Comprehensive quality check** - Single-job canonical gate

**Features:**
- Runs on OTP 26 (canonical version)
- 6 gates in sequence (5 blocking, 1 warning)
- Detailed GITHUB_STEP_SUMMARY report
- Sets commit status API

**Required status check:**
- `Comprehensive Quality Gate (Blocking)`

### `.github/workflows/block-on-regression.yml`
**Performance regression blocker** - Compares PR vs base branch

**Features:**
- Runs `core_ops_100k` benchmark on both branches
- Blocks if >10% throughput degradation
- Comments on PR with results table
- Upload benchmark logs

**Required status check:**
- `Benchmark Regression Analysis (Blocking)`

---

## Quick Start

### For Developers: Run Quality Gates Locally

```bash
# Full quality check (recommended before every push)
make check-full

# Individual gates
make compile          # Gate 1: Compilation
make xref             # Gate 2: Cross-reference
make dialyzer         # Gate 3: Type checking
make test             # Gate 4: Unit tests
make coverage         # Gate 5: Code coverage
make benchmark-quick  # Gate 6: Performance
```

### For Admins: Enable Branch Protection

**Option 1: GitHub Web UI**
1. Settings → Branches → Add rule
2. Pattern: `main`
3. Enable "Require status checks to pass"
4. Select required checks (see [BRANCH_PROTECTION_SETUP.md](BRANCH_PROTECTION_SETUP.md))

**Option 2: GitHub CLI**
```bash
gh api repos/:owner/:repo/branches/main/protection \
  --method PUT \
  --field required_status_checks[strict]=true \
  --field required_status_checks[contexts][]="test (26)" \
  --field required_status_checks[contexts][]="Quality Gates Summary" \
  # ... (see full script in BRANCH_PROTECTION_SETUP.md)
```

**Option 3: Terraform**
See [BRANCH_PROTECTION_SETUP.md](BRANCH_PROTECTION_SETUP.md#automated-setup-terraform---infrastructure-as-code)

---

## How CI/CD Blocks Merges

### GitHub Branch Protection Rules

When enabled, these rules prevent merging until all required status checks pass:

**Required checks:**
- ✅ `test (25)` - CI on OTP 25
- ✅ `test (26)` - CI on OTP 26 (canonical)
- ✅ `test (27)` - CI on OTP 27
- ✅ `test (28)` - CI on OTP 28 (optional)
- ✅ `Quality Gates Summary` - Final summary
- ✅ `Comprehensive Quality Gate (Blocking)` - Canonical quality check
- ✅ `Benchmark Regression Analysis (Blocking)` - Performance check

**Result:** Green checkmark = merge allowed, Red X = merge blocked

---

## Common Failure Scenarios

### Compilation Failed
```
Error: src/module.erl:42: undefined function foo/1
```
**Fix:** Check spelling, arity, ensure function exists

### Xref Failed
```
Warning: module.erl calls undefined function unknown:bar/2
```
**Fix:** Add missing dependency or fix function call

### Dialyzer Failed
```
Warning: Pattern can never match the type
```
**Fix:** Add type specs, fix type mismatches

### Tests Failed (<90%)
```
erlmcp_client_tests:150: FAILED
Expected: {error, timeout}
Actual: {ok, connected}
```
**Fix:** Debug test, fix code or test expectation

### Coverage Failed (<80%)
```
Overall coverage: 75% (< 80%)
```
**Fix:** Write tests for uncovered lines

### Performance Regression (>10%)
```
Throughput: -15.3% vs base
```
**Fix:** Profile and optimize, or document why acceptable

---

## Emergency Bypass

**Use only for production hotfixes.**

**Options:**
1. Admin override: "Merge without waiting for requirements"
2. Temporarily disable protection
3. Use `hotfix/*` branch (not protected)

**Required:**
- Document reason in commit message
- Create follow-up issue
- Tag with `tech-debt`

See [BLOCKING_QUALITY_GATES.md#emergency-bypass-process](BLOCKING_QUALITY_GATES.md#emergency-bypass-process)

---

## Monitoring

### Quality Gate Pass Rates
**Target:** ≥95% first-time pass

**Track:**
- Gate failure frequency
- Common failure patterns
- Time to green

### Bypass Frequency
**Target:** <1 bypass per month

**Track:**
- Bypass count and reasons
- Follow-up completion rate

---

## File Structure

```
docs/ci-cd/
├── README.md                       # This file
├── BLOCKING_QUALITY_GATES.md       # How gates work, how to fix failures
└── BRANCH_PROTECTION_SETUP.md      # GitHub branch protection configuration

.github/workflows/
├── ci.yml                          # Main CI pipeline (multi-OTP)
├── quality-gate.yml                # Comprehensive single-job gate
└── block-on-regression.yml         # Performance regression blocker
```

---

## Best Practices

### 1. Test Locally Before Pushing
```bash
make check-full
```
Faster feedback, fewer CI failures.

### 2. Fix Issues Incrementally
Don't try to fix all gates at once. Focus on one at a time.

### 3. Monitor CI Status
Watch GitHub Actions, fix failures immediately, don't merge until green.

### 4. Document Regressions
If performance regression is acceptable, document why in PR.

### 5. Keep Coverage High
Write tests for all new code. Maintain ≥80% overall, ≥90% for critical modules.

### 6. Don't Bypass Gates
Emergency bypass only for production hotfixes. Always create follow-up issues.

---

## Support

### Questions?
- Read [BLOCKING_QUALITY_GATES.md](BLOCKING_QUALITY_GATES.md) for detailed troubleshooting
- Check workflow logs in GitHub Actions
- Open issue with `ci-cd` label

### Need to bypass gates?
- See [Emergency Bypass Process](BLOCKING_QUALITY_GATES.md#emergency-bypass-process)
- Requires admin privileges
- Must document reason and create follow-up

### Branch protection not working?
- See [BRANCH_PROTECTION_SETUP.md](BRANCH_PROTECTION_SETUP.md#troubleshooting)
- Verify status checks have run at least once
- Check workflow trigger conditions

---

## Summary

**erlmcp's CI/CD quality gates:**
- ✅ Block merges on quality failures
- ✅ Enforce Lean Six Sigma standards
- ✅ Provide clear feedback in PR comments
- ✅ Support local testing with `make check-full`
- ✅ Document bypass process for emergencies

**Result:** Zero-defect production code, every commit.
