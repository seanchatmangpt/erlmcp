# Validation Framework CI/CD Integration

**Version**: 1.0.0
**Date**: 2026-01-30
**Status**: Production Ready

## Overview

This document describes the comprehensive CI/CD integration for the MCP specification validation framework. The workflows automatically validate spec compliance on every commit and pull request.

## Workflow Files

### 1. `.github/workflows/spec-compliance.yml`

**Purpose**: Run full MCP specification compliance validation

**Triggers**:
- Push to main, release, feature, epic branches
- Pull requests to main and release branches
- Daily schedule (3 AM UTC)
- Manual workflow dispatch

**Jobs**:

| Job | Purpose | Timeout | Matrix |
|-----|---------|---------|--------|
| spec-parser | Parse and validate MCP spec | 15m | OTP 25, 26, 27 |
| protocol-compliance | Black-box protocol testing | 30m | OTP 25, 26 × stdio/tcp/http |
| full-compliance | Complete test suites | 45m | OTP 26 × 3 test suites |
| transport-validation | Transport behavior validation | 30m | 4 validation scenarios |
| compliance-report | Generate reports & artifacts | 15m | Single job |
| compliance-gate | Final quality gate | 5m | Single job |

**Quality Gates**:
- Compilation must pass (all OTP versions)
- Protocol compliance must pass
- Test suites must pass
- Overall compliance ≥95% (configurable)

**Artifacts**:
- Spec requirements logs (7 days)
- Protocol compliance results (7 days)
- Compliance test results (14 days)
- Compliance report (90 days)
- Compliance badge SVG

### 2. `.github/workflows/validation-quality-gates.yml`

**Purpose**: Validate validation framework code quality

**Triggers**:
- Push to main, feature, epic branches
- Pull requests to main and release branches
- Can be called from other workflows

**Jobs**:

| Job | Purpose | Timeout | Matrix |
|-----|---------|---------|--------|
| validation-compile | Compile validation app | 15m | OTP 25, 26, 27 |
| validation-tests | Run validation tests | 30m | OTP 26 × eunit/ct |
| spec-parser-validation | Validate spec parser | 15m | Single job |
| protocol-validator-tests | Test protocol validators | 20m | 3 validator types |
| coverage-threshold | Check 80% coverage | 10m | Single job |
| final-quality-gate | Aggregate gate status | 5m | Single job |

**Quality Gates**:
- Compilation must succeed (all OTP versions)
- Tests must pass (EUnit + CT)
- Coverage must be ≥80%
- Spec parser must validate correctly

### 3. `.github/workflows/benchmark-validation.yml`

**Purpose**: Run performance benchmarks and detect regressions

**Triggers**:
- Push to main, release, feature branches
- Pull requests to main and release branches
- Daily schedule (4 AM UTC)
- Manual dispatch with duration parameter

**Jobs**:

| Job | Purpose | Timeout | Matrix |
|-----|---------|---------|--------|
| quick-benchmark | Smoke test (core_ops_1k) | 10m | OTP 26 |
| full-benchmark | All benchmark suites | 45m | 5 suites |
| regression-detection | Compare vs baseline | 15m | Single job |
| metrology-compliance | Validate metric units | 10m | Single job |
| benchmark-summary | Generate report & badges | 5m | Single job |

**Quality Gates**:
- No critical benchmark failures
- Performance regression <10%
- Metrology compliance (canonical units)

**Artifacts**:
- Quick benchmark results (7 days)
- Full benchmark results (14 days)
- Performance badge SVG

## Scripts

### `scripts/validation/run-ci.sh`

**Purpose**: Local CI runner - test before pushing

**Usage**:
```bash
# Quick validation (compile + tests)
./scripts/validation/run-ci.sh --quick

# Full validation (all checks + benchmarks)
./scripts/validation/run-ci.sh --full

# Spec compliance only
./scripts/validation/run-ci.sh --compliance

# Standard validation (default)
./scripts/validation/run-ci.sh
```

**Features**:
- Pre-flight checks (Erlang version, rebar3, project structure)
- Compilation validation
- Xref check (non-blocking)
- Dialyzer check (non-blocking)
- Test suite (EUnit + CT)
- Coverage threshold (≥80%)
- Spec compliance validation
- Benchmark validation (full mode only)
- Metrology compliance (full mode only)

**Exit Codes**:
- 0: All blocking checks passed
- 1: One or more blocking checks failed

### `scripts/validation/generate_traceability_matrix.sh`

**Purpose**: Generate spec compliance matrix

**Usage**:
```bash
# Generate to stdout
./scripts/validation/generate_traceability_matrix.sh

# Generate to file
./scripts/validation/generate_traceability_matrix.sh > docs/SPEC_COMPLIANCE_MATRIX.md
```

**Output**: Markdown document mapping spec requirements to tests

## Environment Variables

### Global Variables (set in workflows)

| Variable | Value | Purpose |
|----------|-------|---------|
| `VALIDATION_APP` | erlmcp_validation | Validation app name |
| `SPEC_VERSION` | 2025-11-25 | MCP spec version |
| `COMPLIANCE_THRESHOLD` | 95 | Minimum compliance % |
| `COVERAGE_THRESHOLD` | 80 | Minimum coverage % |
| `REGRESSION_THRESHOLD` | 10 | Max regression % |
| `BENCHMARK_TIMEOUT_MINUTES` | 20 | Benchmark timeout |

### Local Development Variables

```bash
# Override defaults
export OTP_VERSION=27
export REBAR3_VERSION=3.25
export COVERAGE_THRESHOLD=85
export COMPLIANCE_THRESHOLD=98
```

## Caching Strategy

### Cache Keys

**Spec Parser Cache**:
```
${{ runner.os }}-spec-parser-otp${{ otp_version }}-${{ hashFiles('apps/erlmcp_validation/rebar.config') }}
```

**Protocol Compliance Cache**:
```
${{ runner.os }}-protocol-otp${{ otp_version }}-${{ transport }}-${{ hashFiles('rebar.lock') }}
```

**Benchmark Cache**:
```
${{ runner.os }}-bench-full-${{ benchmark_suite }}-${{ hashFiles('bench/${suite}/*.erl') }}
```

### Cache Paths

- `~/.cache/rebar3` - Rebar3 dependencies
- `_build` - Build artifacts

## Status Checks and Branch Protection

### Required Checks (Blocking)

1. **Compilation** - All OTP versions must compile
2. **Tests** - EUnit + CT must pass
3. **Coverage** - Must be ≥80%
4. **Compliance** - Must be ≥95%
5. **Quality Gates** - All must pass

### Optional Checks (Non-blocking)

1. **Xref** - Warnings reported but not blocking
2. **Dialyzer** - Warnings reported but not blocking
3. **Benchmarks** - Performance warnings not blocking

### Branch Protection Rules

**Main Branch**:
- Require PR reviews
- Require status checks: all blocking checks
- Require branches to be up to date
- Limit who can push

**Release Branches**:
- Require PR reviews
- Require status checks: all blocking + compliance
- Require strict up-to-date
- Limit to admins only

## Badges and Status Indicators

### Compliance Badge

Generated by `spec-compliance.yml` on successful runs.

**File**: `compliance-badge.svg`
**Colors**:
- Bright green: ≥95%
- Yellow: 80-94%
- Red: <80%

### Performance Badge

Generated by `benchmark-validation.yml` on successful runs.

**File**: `performance-badge.svg`
**Colors**:
- Bright green: Pass (no regression + metrology OK)
- Yellow: Warning (regression or metrology issues)

### Usage in README

```markdown
![MCP Spec Compliance](https://img.shields.io/badge/MCP%20Spec-TBD%25-brightgreen)
![Performance](https://img.shields.io/badge/Performance-PASS-brightgreen)
```

## Artifact Retention

| Artifact | Retention | Purpose |
|----------|-----------|---------|
| Spec requirements logs | 7 days | Debug spec parsing issues |
| Protocol compliance results | 7 days | Debug transport issues |
| Compliance test results | 14 days | Debug test failures |
| Benchmark results | 14-30 days | Performance tracking |
| Compliance reports | 90 days | Historical compliance data |
| Badges | Indefinite | README display |

## PR Comments and Notifications

### Automatic PR Comments

**On Compliance Test Failure**:
```markdown
## MCP Specification Compliance Report

**Overall Compliance**: 93.5%
**Threshold**: 95%

### Issues Found
- ❌ tools/call progress token support
- ⚠️  HTTP SSE retry field missing

### Next Steps
1. Review compliance report in artifacts
2. Fix failing tests
3. Re-run validation
```

**On Performance Regression**:
```markdown
## Performance Regression Detected

**Regression**: 12.5%
**Threshold**: 10%

### Affected Metrics
- Core ops throughput: 2.35M → 2.06M msg/s
- Regression exceeds threshold

### Investigation
1. Check recent commits
2. Run benchmarks locally
3. Identify root cause
```

## Troubleshooting

### Common Issues

**Issue**: Compilation fails on OTP 28
```
Solution: OTP 28 is experimental. Use OTP 25-27 for production.
```

**Issue**: Dialyzer warnings on OTP 25
```
Solution: Dialyzer may have version-specific warnings. Check if blocking.
```

**Issue**: Benchmark timeout
```
Solution: Reduce workload size or increase timeout in workflow.
```

**Issue**: Cache miss
```
Solution: Cache keys include file hashes. Force rebuild if dependencies change.
```

### Debug Mode

Enable debug logging in workflows:
```yaml
- name: Run with debug
  run: rebar3 as validation ct --verbose
  env:
    DEBUG: "true"
```

### Local Testing

Test workflows locally using `act` (GitHub Actions runner):
```bash
# Install act
brew install act

# Run spec compliance workflow
act -j spec-parser -W .github/workflows/spec-compliance.yml

# Run full validation
act -j full-compliance -W .github/workflows/spec-compliance.yml
```

## Performance Considerations

### Workflow Optimization

1. **Parallel Execution**: Jobs run in parallel where possible
2. **Caching**: Dependencies cached across runs
3. **Matrix Strategy**: Test multiple OTP versions efficiently
4. **Artifact Upload**: Only upload necessary files
5. **Timeout Management**: Prevent hanging jobs

### Resource Limits

- **Total Workflow Timeout**: 2 hours (GitHub default)
- **Job Timeout**: 15-45 minutes per job
- **Concurrent Jobs**: Up to 20 parallel jobs

### Cost Optimization

- Use `schedule` sparingly (daily, not hourly)
- Limit artifact retention periods
- Use caching aggressively
- Cancel redundant workflows

## Security Considerations

### Secrets Management

No secrets required for validation workflows. All tools are public.

### Dependencies

- **Erlang/OTP**: Official erlef/setup-beam@v1
- **Actions**: GitHub official actions (checkout, upload-artifact, etc.)
- **Third-party**: None

### Code Scanning

Workflows integrate with GitHub code scanning:
- Dependabot: Automated dependency updates
- CodeQL: Security vulnerability scanning
- Secret scanning: Detect leaked secrets

## Maintenance

### Updating Workflows

1. **Version Pin**: Pin major versions of actions
2. **Test Changes**: Use workflow_dispatch to test
3. **Monitor Failures**: Check Actions tab regularly
4. **Update Docs**: Keep this document in sync

### Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-01-30 | Initial CI/CD integration |

## References

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [erlmcp Validation Framework](docs/VALIDATION_FRAMEWORK.md)
- [MCP Specification](https://modelcontextprotocol.io)
- [Rebar3 Documentation](https://rebar3.org)
