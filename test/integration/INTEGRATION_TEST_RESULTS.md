# TCPS Integration Test Results

**Generated:** 2026-01-26 21:33:33
**Total Duration:** 50s
**Pass Rate:** 0.00%

## Executive Summary

- **Total Test Suites:** 7
- **Total Test Cases:** 105
- **Passed:** 0
- **Failed:** 105
- **Skipped:** 0

## Test Suite Results

| Suite | Test Cases | Passed | Failed | Skipped | Duration (s) | Status |
|-------|------------|--------|--------|---------|--------------|--------|
| tcps_pipeline_SUITE | 15 | 0 | 15 | 0 | 7 | ❌ FAIL |
| tcps_andon_integration_SUITE | 15 | 0 | 15 | 0 | 6 | ❌ FAIL |
| tcps_concurrent_SUITE | 15 | 0 | 15 | 0 | 7 | ❌ FAIL |
| tcps_quality_gates_SUITE | 15 | 0 | 15 | 0 | 6 | ❌ FAIL |
| tcps_heijunka_SUITE | 15 | 0 | 15 | 0 | 7 | ❌ FAIL |
| tcps_persistence_SUITE | 15 | 0 | 15 | 0 | 5 | ❌ FAIL |
| tcps_performance_SUITE | 15 | 0 | 15 | 0 | 7 | ❌ FAIL |

## Performance Metrics

### Slowest Test Suites

- **tcps_pipeline_SUITE**: 7s
- **tcps_performance_SUITE**: 7s
- **tcps_heijunka_SUITE**: 7s
- **tcps_concurrent_SUITE**: 7s
- **tcps_quality_gates_SUITE**: 6s

## Failure Details

### tcps_pipeline_SUITE

- **Failed Tests:** 15
- **Root Cause:** See detailed logs in `_build/test/logs/` for analysis
- **Recommended Action:** Review test implementation and mock service setup

### tcps_andon_integration_SUITE

- **Failed Tests:** 15
- **Root Cause:** See detailed logs in `_build/test/logs/` for analysis
- **Recommended Action:** Review test implementation and mock service setup

### tcps_concurrent_SUITE

- **Failed Tests:** 15
- **Root Cause:** See detailed logs in `_build/test/logs/` for analysis
- **Recommended Action:** Review test implementation and mock service setup

### tcps_quality_gates_SUITE

- **Failed Tests:** 15
- **Root Cause:** See detailed logs in `_build/test/logs/` for analysis
- **Recommended Action:** Review test implementation and mock service setup

### tcps_heijunka_SUITE

- **Failed Tests:** 15
- **Root Cause:** See detailed logs in `_build/test/logs/` for analysis
- **Recommended Action:** Review test implementation and mock service setup

### tcps_persistence_SUITE

- **Failed Tests:** 15
- **Root Cause:** See detailed logs in `_build/test/logs/` for analysis
- **Recommended Action:** Review test implementation and mock service setup

### tcps_performance_SUITE

- **Failed Tests:** 15
- **Root Cause:** See detailed logs in `_build/test/logs/` for analysis
- **Recommended Action:** Review test implementation and mock service setup


## CI/CD Integration

Test execution script: `scripts/run_integration_tests.sh`

Add to your GitHub Actions workflow:

```yaml
- name: Run Integration Tests
  run: ./scripts/run_integration_tests.sh

- name: Upload Test Reports
  uses: actions/upload-artifact@v3
  with:
    name: integration-test-reports
    path: _build/test/logs/
```

## Test Logs

Detailed HTML reports available in:
- `_build/test/logs/`

View the latest CT run:
- `_build/test/logs/ct_run.*/index.html`

