# TCPS Code Coverage - Quick Start Guide

## Overview

This project implements comprehensive code coverage infrastructure following Lean Six Sigma quality standards (80%+ coverage requirement).

## Quick Commands

### Generate Coverage Report

```bash
./scripts/generate_coverage.sh
```

This runs:
1. Clean previous coverage data
2. Compile with coverage instrumentation
3. Run EUnit tests with coverage
4. Run Common Test integration tests with coverage
5. Generate combined coverage report
6. Create HTML reports

**Output:** `_build/test/cover/index.html`

### Check Coverage Threshold

```bash
./scripts/check_coverage_threshold.sh 80
```

Validates:
- Overall coverage ≥80%
- Critical modules ≥90% (tcps_andon, tcps_root_cause, tcps_receipt_verifier, tcps_rebar3_quality)
- Lists all modules below threshold

**Exit Code:** 0 = pass, 1 = fail

### View Coverage Reports

```bash
# HTML report (recommended)
open _build/test/cover/index.html

# Text report
cat _build/test/cover/cover.log

# Detailed analysis
less docs/COVERAGE_REPORT.md
```

## Documentation

- **[COVERAGE_REPORT.md](COVERAGE_REPORT.md)** - Comprehensive 18KB coverage analysis
  - Per-module coverage breakdown
  - Coverage gaps analysis
  - 4-week action plan to achieve 80%+
  - Recommended test cases

- **[COVERAGE_DELIVERABLES_SUMMARY.md](COVERAGE_DELIVERABLES_SUMMARY.md)** - Complete deliverables summary
  - All scripts and tools
  - Usage examples
  - File locations
  - Current status

## CI/CD Integration

Coverage is automatically checked on every push and pull request via GitHub Actions:

```yaml
# .github/workflows/ci.yml
- name: Run Tests with Coverage
  run: rebar3 as test do compile, eunit --cover, ct --dir=test/integration --cover, cover --verbose

- name: Check Coverage Threshold (80%)
  run: ./scripts/check_coverage_threshold.sh 80

- name: Upload Coverage HTML Report
  uses: actions/upload-artifact@v3
  with:
    name: coverage-report-otp-${{ matrix.otp_version }}
    path: _build/test/cover/
```

Coverage reports are uploaded as artifacts for every build.

## Current Status

- **Overall Coverage:** ~0-1% (baseline established)
- **Target:** 80% (Lean Six Sigma standard)
- **Critical Modules:** 0/4 at 90%+ (target: 4/4)
- **Modules ≥80%:** 0/54 (target: 54/54)

## Next Steps

See [COVERAGE_REPORT.md](COVERAGE_REPORT.md) Section "Action Plan to Achieve 80%+ Coverage" for detailed roadmap.

### Immediate Actions

1. Fix remaining test infrastructure issues
2. Enable all existing tests
3. Write tests for critical modules (90%+ coverage)

### Tools Available

- `/scripts/generate_coverage.sh` - Automated coverage generation
- `/scripts/check_coverage_threshold.sh` - Threshold validation
- `/test/tcps_receipt_verifier_comprehensive_tests.erl` - Example comprehensive test suite
- `_build/test/cover/index.html` - HTML coverage reports

## Support

For questions or issues with coverage infrastructure:
1. Review [COVERAGE_REPORT.md](COVERAGE_REPORT.md)
2. Review [COVERAGE_DELIVERABLES_SUMMARY.md](COVERAGE_DELIVERABLES_SUMMARY.md)
3. Check HTML reports: `open _build/test/cover/index.html`
