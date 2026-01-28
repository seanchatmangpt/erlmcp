# Quality Gate Enforcement System - Documentation Index

**Version:** 1.0.0
**Last Updated:** 2026-01-28
**Status:** Production-Ready

## Overview

The erlmcp Quality Gate Enforcement System implements Toyota Production System (TPS) Jidoka principles for zero-defect software delivery. This system provides automated quality validation at every stage of development, ensuring no defective code reaches production.

## System Architecture

**8 Sequential Quality Gates:**
1. **SHACL Validation** - Ontology conformance
2. **Compilation** - Zero-error compilation
3. **Test Execution** - 95% pass rate, 80% coverage
4. **Security Scan** - Zero critical vulnerabilities
5. **Deterministic Build** - Reproducibility verification
6. **Quality Metrics** - Production thresholds
7. **Release Verification** - SBOM, licenses, dependencies
8. **Smoke Test** - Basic functionality validation

**Core Principle:** Stop-the-line authority. Any gate failure triggers Andon event and blocks progression.

## Quick Start

### For Developers

```bash
# 1. Check all quality gates for current SKU
make quality-check

# 2. Check specific gate
rebar3 tcps quality-gate --gate=compilation --sku=mysku-v1.0.0

# 3. View gate status
rebar3 tcps gate-status --sku=mysku-v1.0.0

# 4. Run with detailed output
TCPS_VERBOSE=true make quality-check
```

### For CI/CD Integration

```yaml
# .github/workflows/quality-gates.yml
- name: Run Quality Gates
  run: |
    rebar3 tcps check-all-gates --sku=${{ env.SKU_ID }}

- name: Upload Gate Receipts
  uses: actions/upload-artifact@v3
  with:
    name: quality-gate-receipts
    path: priv/receipts/*.json
```

## Documentation Structure

### User Guides

- **[USER_GUIDE.md](USER_GUIDE.md)** - For developers using the system
  - Running quality checks
  - Understanding failures
  - Fixing violations
  - Emergency overrides (when/how)

- **[ADMIN_GUIDE.md](ADMIN_GUIDE.md)** - For administrators
  - Configuring gates
  - Updating baselines
  - Managing hooks
  - Team permissions

### Technical Documentation

- **[ARCHITECTURE.md](ARCHITECTURE.md)** - System architecture
  - Component diagram
  - Data flows
  - Integration points
  - State management

- **[PHILOSOPHY.md](PHILOSOPHY.md)** - Quality principles
  - Why quality gates matter
  - TCPS Jidoka principles
  - Lean Six Sigma connection
  - Zero defects philosophy

### Operational Guides

- **[MIGRATION.md](MIGRATION.md)** - Migration guide
  - Migrating existing projects
  - Gradual rollout strategy
  - Phased enforcement
  - Team training

- **[FAQ.md](FAQ.md)** - Common questions
  - Troubleshooting steps
  - Known issues
  - Best practices
  - Performance tuning

## Key Concepts

### Quality Gates

**Sequential Execution:** Gates execute in order. First failure stops execution.

**Blocking vs Non-Blocking:**
- **Blocking (Critical):** Gates 1-7 must pass to proceed
- **Non-Blocking (Medium):** Gate 8 (smoke test) warns but doesn't block

**Andon Integration:** Failed gates trigger Andon events for stop-the-line response.

### Receipt Chain

Every gate execution generates a deterministic receipt:

```erlang
#{
    receipt_id => <<"RCPT-compilation-1706472000-123456">>,
    receipt_type => quality_gate,
    gate => compilation,
    sku_id => <<"mysku-v1.0.0">>,
    status => pass,
    timestamp => 1706472000000,
    details => #{...},
    ontology_refs => [<<"tcps:QualityGate">>, ...]
}
```

**Immutable Audit Trail:** Receipts are SHA-256 hashed and chained for tamper-proof history.

### Production Thresholds

```erlang
% Defined in tcps_quality_gates.erl
-define(PRODUCTION_THRESHOLDS, #{
    test_pass_rate => 0.95,        % 95% minimum (Toyota standard)
    test_coverage => 0.80,          % 80% minimum (industry best practice)
    quality_gate_pass_rate => 0.95, % 95% minimum gate pass rate
    defect_rate => 0.05,            % 5% maximum defect rate
    first_pass_yield => 0.90        % 90% minimum first pass yield
}).
```

**Non-Negotiable:** These thresholds enforce Lean Six Sigma quality (3.4 defects per million).

## Integration Points

### Rebar3 Plugin

```erlang
% rebar.config
{plugins, [
    {rebar3_tcps, {git, "https://github.com/banyan-platform/rebar3_tcps", {branch, "main"}}}
]}.

{tcps, [
    {quality_gates, [
        {shacl_validation, #{enabled => true}},
        {compilation, #{enabled => true}},
        {test_execution, #{
            enabled => true,
            min_pass_rate => 0.95,
            min_coverage => 0.80
        }},
        {security_scan, #{enabled => true}},
        {deterministic_build, #{enabled => true}},
        {quality_metrics, #{enabled => true}},
        {release_verification, #{enabled => true}},
        {smoke_test, #{enabled => true}}
    ]}
]}.
```

### Git Hooks

Pre-commit hook ensures quality before commit:

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Run compilation gate
rebar3 tcps quality-gate --gate=compilation

if [ $? -ne 0 ]; then
    echo "❌ Compilation gate failed. Fix errors before committing."
    exit 1
fi

# Run test gate (fast subset)
rebar3 tcps quality-gate --gate=test_execution --fast

if [ $? -ne 0 ]; then
    echo "❌ Test execution gate failed. Ensure tests pass."
    exit 1
fi

exit 0
```

### CI/CD Pipeline

```yaml
# GitHub Actions example
name: Quality Gates

on: [push, pull_request]

jobs:
  quality-gates:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26.2'

      - name: Run All Quality Gates
        run: |
          SKU_ID="${GITHUB_REF_NAME}-${GITHUB_SHA:0:8}"
          rebar3 tcps check-all-gates --sku=$SKU_ID

      - name: Upload Receipts
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: quality-gate-receipts
          path: priv/receipts/*.json

      - name: Report Metrics
        run: |
          rebar3 tcps quality-metrics --format=json > metrics.json
          cat metrics.json
```

## Troubleshooting Quick Reference

### Gate Failures

**Compilation Failed:**
```bash
# View detailed errors
rebar3 tcps quality-gate --gate=compilation --verbose

# Common causes:
# - Syntax errors
# - Missing dependencies
# - Type mismatches
```

**Test Execution Failed:**
```bash
# View test report
rebar3 tcps quality-gate --gate=test_execution --verbose

# Check coverage
rebar3 cover --verbose

# Common causes:
# - Flaky tests
# - Insufficient coverage
# - Environment dependencies
```

**Security Scan Failed:**
```bash
# View vulnerabilities
rebar3 tcps quality-gate --gate=security_scan --verbose

# Common causes:
# - Outdated dependencies
# - Hardcoded secrets
# - Insecure patterns
```

### Andon Events

**View Open Andons:**
```bash
rebar3 tcps andon-list --sku=mysku-v1.0.0
```

**Resolve Andon:**
```bash
# Fix issue, then close Andon
rebar3 tcps andon-resolve --andon-id=ANDON-12345
```

### Emergency Override

**WARNING:** Only use in production emergencies with approval.

```bash
# Override specific gate (requires justification)
TCPS_OVERRIDE=true \
TCPS_OVERRIDE_REASON="Production hotfix for security CVE-2026-12345" \
TCPS_OVERRIDE_APPROVER="ops-lead@company.com" \
rebar3 tcps quality-gate --gate=test_execution --override

# Override logged to audit trail
```

## Performance

**Typical Execution Times:**
- SHACL Validation: 2-5s (depends on ontology size)
- Compilation: 10-30s (incremental) / 60-120s (clean)
- Test Execution: 30-120s (depends on test suite size)
- Security Scan: 10-20s
- Deterministic Build: 60-180s (double build required)
- Quality Metrics: 1-2s
- Release Verification: 5-10s
- Smoke Test: 20-60s

**Total (all gates):** 5-10 minutes for comprehensive validation.

**Optimization:**
- Use incremental builds
- Cache dependencies
- Parallelize independent gates
- Run fast gates first

## Metrics Dashboard

**View Quality Metrics:**
```bash
# Console output
rebar3 tcps quality-metrics

# JSON format for dashboards
rebar3 tcps quality-metrics --format=json

# HTML report
rebar3 tcps quality-metrics --format=html --output=metrics.html
```

**Example Output:**
```json
{
  "test_pass_rate": 0.97,
  "test_coverage": 0.85,
  "defect_rate": 0.02,
  "first_pass_yield": 0.93,
  "gate_pass_rates": {
    "shacl_validation": 0.98,
    "compilation": 0.95,
    "test_execution": 0.90,
    "security_scan": 0.99,
    "deterministic_build": 0.93,
    "quality_metrics": 0.91,
    "release_verification": 0.96,
    "smoke_test": 0.88
  }
}
```

## Support and Resources

### Documentation
- [User Guide](USER_GUIDE.md) - Day-to-day usage
- [Admin Guide](ADMIN_GUIDE.md) - System configuration
- [Architecture](ARCHITECTURE.md) - Technical deep dive
- [Philosophy](PHILOSOPHY.md) - Quality principles
- [Migration Guide](MIGRATION.md) - Adoption strategy
- [FAQ](FAQ.md) - Common questions

### Source Code
- `apps/tcps_erlmcp/src/tcps_quality_gates.erl` - Core implementation
- `apps/erlmcp_core/src/pricing/tcps_poka_yoke.erl` - Validation rules
- `apps/tcps_erlmcp/test/integration/tcps_quality_gates_SUITE.erl` - Test suite

### Community
- GitHub Issues: https://github.com/banyan-platform/erlmcp/issues
- Discussions: https://github.com/banyan-platform/erlmcp/discussions
- Slack: #erlmcp-quality-gates

### Training
- TCPS Quality Gates Workshop (2 hours)
- Jidoka Principles for Software (1 hour)
- Zero Defects in Practice (1 hour)

Contact: quality-gates@erlmcp.dev

---

**Next Steps:**
1. Read the [User Guide](USER_GUIDE.md) to start using quality gates
2. Review [Architecture](ARCHITECTURE.md) to understand the system
3. Check [FAQ](FAQ.md) for common questions

**Version History:**
- v1.0.0 (2026-01-28): Initial production release
