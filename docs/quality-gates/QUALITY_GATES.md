# erlmcp v3 Quality Gates Definition

## Overview

This document defines the quality gates that must be passed before any deployment to production. These gates enforce the Lean Six Sigma 99.99966% defect-free standard.

## Gate Levels

### Level 1: Developer Gate (Pre-commit)

**Trigger**: On every commit
**Duration**: < 5 minutes
**Failure Action**: Block commit

| Gate | Tool | Criteria | Blocking |
|------|------|----------|----------|
| Compilation | rebar3 | 0 errors | YES |
| Format Check | rebar3_format | 0 formatting issues | YES |
| Lint Check | rebar3_lint | 0 lint errors | YES |
| Unit Tests (EUnit) | rebar3 eunit | 100% pass rate | YES |

**Exit Criteria**: All gates pass with 0 errors.

### Level 2: PR Merge Gate

**Trigger**: On pull request
**Duration**: < 15 minutes
**Failure Action**: Block PR merge

| Gate | Tool | Criteria | Blocking |
|------|------|----------|----------|
| Compilation | rebar3 | 0 errors | YES |
| Dialyzer | rebar3 dialyzer | 0 warnings | YES |
| Xref | rebar3 xref | 0 undefined functions | YES |
| Unit Tests (EUnit) | rebar3 eunit | 100% pass rate | YES |
| Integration Tests (CT) | rebar3 ct | 100% pass rate | YES |
| Coverage | rebar3 cover | >= 80% | YES |
| Property Tests (Proper) | rebar3 proper | 100 tests, 0 failures | YES |

**Exit Criteria**: All gates pass with 0 failures, coverage >= 80%.

### Level 3: Staging Deployment Gate

**Trigger**: On merge to main
**Duration**: < 30 minutes
**Failure Action**: Block production deployment

| Gate | Tool | Criteria | Blocking |
|------|------|----------|----------|
| Full Test Suite | rebar3 | 100% pass rate | YES |
| Performance Benchmarks | custom | < 10% regression | YES |
| Security Scan | Trivy | 0 critical vulnerabilities | YES |
| SBOM Validation | syft | valid SBOM generated | YES |
| License Compliance | custom | no forbidden licenses | YES |
| Smoke Tests | custom | all health checks pass | YES |
| Integration Tests | custom | all scenarios pass | YES |

**Exit Criteria**: All gates pass with 0 critical issues.

### Level 4: Production Deployment Gate

**Trigger**: Manual approval after staging
**Duration**: < 60 minutes
**Failure Action**: Block deployment, trigger rollback

| Gate | Tool | Criteria | Blocking |
|------|------|----------|----------|
| Canary Health | Flagger | success rate >= 99% | YES |
| Canary Performance | Flagger | p95 latency < 500ms | YES |
| Canary Error Rate | Prometheus | error rate < 1% | YES |
| Full Rollout Health | custom | all pods ready | YES |
| Smoke Tests | custom | all health checks pass | YES |
| Load Tests | custom | meets SLA targets | YES |
| MTTR Verification | custom | MTTR < 5 minutes | YES |

**Exit Criteria**: All gates pass, canary completes successfully.

## Quality Gate Definitions

### QG-001: Compilation Gate

```yaml
name: compilation
spec:
  tool: rebar3
  command: TERM=dumb rebar3 compile
  pass_criteria:
    - exit_code: 0
    - error_count: 0
  fail_criteria:
    - exit_code: != 0
    - error_count: > 0
  timeout: 300
```

### QG-002: Type Checking Gate (Dialyzer)

```yaml
name: dialyzer
spec:
  tool: rebar3
  command: rebar3 dialyzer
  pass_criteria:
    - exit_code: 0
    - warning_count: 0
  fail_criteria:
    - exit_code: != 0
    - warning_count: > 0
  timeout: 600
```

### QG-003: Cross-Reference Gate (Xref)

```yaml
name: xref
spec:
  tool: rebar3
  command: rebar3 xref
  pass_criteria:
    - exit_code: 0
    - undefined_functions: 0
  fail_criteria:
    - exit_code: != 0
    - undefined_functions: > 0
  timeout: 180
```

### QG-004: Coverage Gate

```yaml
name: coverage
spec:
  tool: rebar3
  command: rebar3 cover
  pass_criteria:
    - coverage_percentage: >= 80
  fail_criteria:
    - coverage_percentage: < 80
  timeout: 120
```

### QG-005: Security Scan Gate

```yaml
name: security_scan
spec:
  tool: trivy
  command: trivy image --severity CRITICAL,HIGH
  pass_criteria:
    - critical_vulnerabilities: 0
    - high_vulnerabilities: <= 5
  fail_criteria:
    - critical_vulnerabilities: > 0
    - high_vulnerabilities: > 5
  timeout: 300
```

### QG-006: Performance Gate

```yaml
name: performance
spec:
  tool: custom_benchmark
  command: ./scripts/bench/regression-check.sh
  pass_criteria:
    - regression_percentage: < 10
  fail_criteria:
    - regression_percentage: >= 10
  timeout: 600
```

### QG-007: Canary Health Gate

```yaml
name: canary_health
spec:
  tool: flagger
  command: kubectl get canary erlmcp
  pass_criteria:
    - status: Succeeded
    - success_rate: >= 99
  fail_criteria:
    - status: Failed
    - success_rate: < 99
  timeout: 1800
```

### QG-008: MTTR Gate

```yaml
name: mttr_verification
spec:
  tool: custom
  command: ./scripts/mttr-verify.sh
  pass_criteria:
    - mttr_seconds: <= 300
  fail_criteria:
    - mttr_seconds: > 300
  timeout: 360
```

## Gate Enforcement

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

set -e

echo "Running quality gates..."

# Compilation
TERM=dumb rebar3 compile

# Format
rebar3 format --verify

# Quick tests
rebar3 eunit

echo "All gates passed!"
```

### CI/CD Integration

Quality gates are enforced at:

1. **Developer Machine**: Pre-commit hooks
2. **GitHub Actions**: PR workflow
3. **ArgoCD**: Pre-sync validation
4. **Flagger**: Canary analysis

## Gate Metrics

Track these metrics for continuous improvement:

| Metric | Target | Current |
|--------|--------|---------|
| Gate Pass Rate | >= 95% | - |
| Gate Duration | <= 15 min | - |
| False Positive Rate | <= 5% | - |
| Time to Recovery | <= 5 min | - |

## Gate Escalation

When a gate fails:

1. **Developer**: Fix immediately, re-run gate
2. **Tech Lead**: Review if blocking feature
3. **Architecture**: Escalate if gate criteria incorrect
4. **CTO**: Final escalation for policy changes

## References

- Lean Six Sigma: CLAUDE.md
- CI/CD: `.github/workflows/gitops-ci.yml`
- Deployment: `.github/workflows/gitops-deploy.yml`
