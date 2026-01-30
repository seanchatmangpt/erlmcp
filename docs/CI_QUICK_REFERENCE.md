# CI/CD Quick Reference Guide

## Quick Start

### Local Validation (Before Pushing)

```bash
# Quick check (2-5 min)
./scripts/validation/run-ci.sh --quick

# Standard check (5-10 min)
./scripts/validation/run-ci.sh

# Full check with benchmarks (15-20 min)
./scripts/validation/run-ci.sh --full

# Spec compliance only
./scripts/validation/run-ci.sh --compliance
```

### Understanding CI Results

**Status Check Meanings**:
- ✅ Green = All checks passed, ready to merge
- ⚠️ Yellow = Non-blocking warnings, review needed
- ❌ Red = Blocking failure, must fix

**Required for Merge**:
- Compilation: 0 errors (all OTP versions)
- Tests: 100% pass rate
- Coverage: ≥80%
- Compliance: ≥95%

**Warnings Only** (don't block merge):
- Xref warnings
- Dialyzer warnings
- Performance regression <10%

## Workflow Files

### spec-compliance.yml
**Purpose**: MCP spec validation
**Triggers**: Push, PR, daily (3 AM), manual
**Duration**: 30-45 min
**Key Jobs**:
- spec-parser (OTP 25/26/27)
- protocol-compliance (stdio/tcp/http)
- full-compliance (3 test suites)
- compliance-gate (≥95%)

### validation-quality-gates.yml
**Purpose**: Validation app quality
**Triggers**: Push, PR
**Duration**: 20-30 min
**Key Jobs**:
- validation-compile (OTP 25/26/27)
- validation-tests (EUnit + CT)
- coverage-threshold (≥80%)

### benchmark-validation.yml
**Purpose**: Performance validation
**Triggers**: Push, PR, daily (4 AM), manual
**Duration**: 30-45 min
**Key Jobs**:
- quick-benchmark (smoke test)
- full-benchmark (5 suites)
- regression-detection (<10%)
- metrology-compliance (units check)

## Artifacts

| Artifact | Retention | Location |
|----------|-----------|----------|
| Spec requirements | 7 days | Actions → Artifacts |
| Protocol compliance | 7 days | Actions → Artifacts |
| Compliance report | 90 days | Actions → Artifacts |
| Benchmark results | 14-30 days | Actions → Artifacts |
| Coverage reports | 14 days | Actions → Artifacts |

## Environment Variables

```bash
# Override defaults
export OTP_VERSION=27
export REBAR3_VERSION=3.25
export COVERAGE_THRESHOLD=85
export COMPLIANCE_THRESHOLD=98
export REGRESSION_THRESHOLD=5
```

## Troubleshooting

### Issue: CI fails locally but passes remotely
**Cause**: Environment differences
**Fix**: Check OTP version, rebar3 version, dependencies

### Issue: Compliance threshold not met
**Cause**: Failing spec tests
**Fix**: Check compliance report artifact, fix failing tests

### Issue: Coverage below 80%
**Cause**: Missing tests for new code
**Fix**: Add tests for uncovered modules

### Issue: Performance regression
**Cause**: Code change affected performance
**Fix**: Profile change, optimize, or update baseline

## Badge URLs

```markdown
![Compliance](https://img.shields.io/badge/MCP%20Spec-95%25-brightgreen)
![Performance](https://img.shields.io/badge/Performance-PASS-brightgreen)
```

## Getting Help

1. Check workflow logs in Actions tab
2. Download artifacts for detailed reports
3. Read `/docs/VALIDATION_CI_CD.md`
4. Open issue with logs attached

## Common Commands

```bash
# Check CI status
gh run list --workflow=spec-compliance

# Re-run failed workflow
gh run rerun <run-id>

# Download artifacts
gh run download <run-id>

# View workflow logs
gh run view <run-id> --log

# Cancel workflow
gh run cancel <run-id>
```
