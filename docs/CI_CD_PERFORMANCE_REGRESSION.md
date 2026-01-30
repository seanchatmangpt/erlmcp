# CI/CD Performance Regression Detection Guide

## Overview

erlmcp includes comprehensive performance regression detection integrated into CI/CD pipelines. This system automatically detects performance degradations and blocks merges that introduce regressions.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     GitHub Actions Workflow                      │
│                   performance-regression.yml                      │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ├── Job 1: Performance Tests (erlmcp_performance_validator_SUITE)
                              │   ├── Run regression SUITE
                              │   ├── Parse results
                              │   └── Upload artifacts
                              │
                              ├── Job 2: Baseline Comparison
                              │   ├── Fetch baseline from main
                              │   ├── Run baseline-compare.sh
                              │   └── Detect regressions
                              │
                              ├── Job 3: PR Comment
                              │   ├── Download artifacts
                              │   ├── Generate markdown report
                              │   └── Post to PR
                              │
                              ├── Job 4: Update Baseline (main only)
                              │   ├── Capture new baseline
                              │   └── Commit to repo
                              │
                              └── Job 5: Trend Analysis
                                  ├── Analyze historical data
                                  └── Generate visualizations
```

## Files

| File | Purpose |
|------|---------|
| `.github/workflows/performance-regression.yml` | Main CI/CD workflow |
| `.github/performance-thresholds.json` | Regression threshold configuration |
| `tools/baseline-capture.sh` | Capture performance baseline |
| `tools/baseline-compare.sh` | Compare against baseline |
| `tools/baseline-update.sh` | Update baseline with justification |
| `scripts/generate_trend_chart.sh` | Generate trend visualizations |
| `bench/baselines/*.json` | Versioned baseline data |

## Workflow Triggers

### Pull Requests

Automatic performance regression detection on all PRs that modify:
- `apps/**` - Application code
- `rebar.config` - Dependencies
- `rebar.lock` - Lock file
- `.github/workflows/performance-regression.yml` - Workflow itself

### Push to Main

Automatic baseline update on main branch merges (if tests pass).

## Thresholds Configuration

Performance thresholds are defined in `.github/performance-thresholds.json`:

```json
{
  "thresholds": {
    "latency": {
      "p50": { "threshold_percent": 10, "severity": "medium" },
      "p95": { "threshold_percent": 15, "severity": "high" },
      "p99": { "threshold_percent": 20, "severity": "high" }
    },
    "throughput": {
      "threshold_percent": 10,
      "direction": "decrease",
      "severity": "high"
    },
    "memory": {
      "per_connection": { "threshold_percent": 20 },
      "leak_threshold": { "threshold_mb": 10, "duration_minutes": 5 }
    }
  }
}
```

### Default Thresholds

| Metric | Threshold | Severity | Action |
|--------|-----------|----------|--------|
| Latency P50 | +10% | Medium | Warning |
| Latency P95 | +15% | High | Block merge |
| Latency P99 | +20% | High | Block merge |
| Throughput | -10% | High | Block merge |
| Memory | +20% | Medium | Warning |
| Error Rate | +5pp | Critical | Block merge |
| Concurrent Success Rate | <99% | Critical | Block merge |

## Baseline Management

### Baseline Format

Baselines are stored as JSON files in `bench/baselines/`:

```json
{
  "version": "2.1.0",
  "timestamp": 1738272000,
  "date": "2026-01-30T12:00:00Z",
  "environment": {
    "os": "linux",
    "arch": "x86_64",
    "otp_version": "27",
    "cores": 4
  },
  "benchmarks": {
    "core_ops_100k": {
      "throughput_msg_per_s": 2690000,
      "latency_p50_us": 5,
      "latency_p95_us": 15,
      "latency_p99_us": 35,
      "memory_delta_mib": 25
    }
  }
}
```

### Baseline Versioning

- **Format**: `YYYY-MM-DD_vX.Y.Z.json`
- **Retention**: Last 10 baselines (90 days)
- **Auto-update**: On main branch merge (if tests pass)

### Manual Baseline Update

When intentional performance changes are justified:

```bash
# Update baseline with justification
./tools/baseline-update.sh --reason "Refactored session management for better scalability, accepted 15% latency increase for improved memory efficiency"

# Commit and push
git add bench/baselines/
git commit -m "chore: update performance baseline"
git push
```

## CI/CD Jobs

### Job 1: Performance Regression Tests

Runs `erlmcp_performance_validator_SUITE` to detect regressions.

**Duration**: 10-15 minutes
**Artifacts**:
- `performance-regression-logs` - Test output and reports
- `performance-regression-coverage` - Code coverage data

**Exit codes**:
- 0: No regressions detected
- 1: Regressions found (blocks merge)

### Job 2: Baseline Comparison

Compares current performance against latest baseline.

**Thresholds**: 10% performance, 20% memory
**Artifacts**:
- `baseline-comparison-report` - HTML comparison report

**Exit codes**:
- 0: No regressions
- 1: Regressions detected (blocks merge)

### Job 3: PR Comment

Posts performance test results to PR as a comment.

**Includes**:
- Test status (PASS/FAIL)
- Regression count
- Links to artifacts
- Actionable recommendations
- Baseline comparison summary

**Example Comment**:

```markdown
## 🚀 Performance Regression Check

### Test Results

**Performance Tests:** ✅ PASSED
**Baseline Comparison:** ❌ FAILED

### Summary

**Date:** 2026-01-30T13:25:00Z
**Commit:** abc123def
**Regressions:** 2

### ⚠️ Action Required

Performance regressions were detected. Please review:

1. **Test Logs**: Download `performance-regression-logs` artifact
2. **Comparison Report**: Download `baseline-comparison-report` artifact
3. **Fix**: Address the performance issues before merging

### Regressions Detected

- **core_ops_100k Throughput**: -15% (baseline: 2.69M msg/s, current: 2.29M msg/s)
- **tcp_quick_1k Latency P95**: +18% (baseline: 85µs, current: 100µs)

---
**Thresholds:** 10% performance, 20% memory
**Workflow:** performance-regression.yml
```

### Job 4: Update Baseline

Automatically updates baseline on main branch (if tests pass).

**Conditions**:
- Triggered on push to `main`
- Only runs if performance tests pass
- Commits with `[skip ci]` to avoid loop

### Job 5: Trend Analysis

Analyzes performance trends across historical baselines.

**Outputs**:
- Performance trend charts
- Historical comparison links
- Summary in GitHub Actions UI

## Local Testing

### Run Performance Tests Locally

```bash
# Run the full SUITE
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_performance_validator_SUITE

# Run with coverage
rebar3 ct --suite=erlmcp_performance_validator_SUITE --cover

# Run specific test
rebar3 ct --suite=erlmcp_performance_validator_SUITE --group=latency
```

### Capture Baseline Locally

```bash
# Capture baseline (commits to git)
./tools/baseline-capture.sh

# Capture without commit
./tools/baseline-capture.sh --no-commit

# Override version
./tools/baseline-capture.sh --version 2.1.1
```

### Compare Against Baseline

```bash
# Compare with latest baseline
./tools/baseline-compare.sh

# Compare with specific baseline
./tools/baseline-compare.sh --baseline bench/baselines/2026-01-28_v2.0.0.json

# Custom threshold
./tools/baseline-compare.sh --threshold 15
```

### Generate Trend Charts

```bash
# Generate all charts
./scripts/generate_trend_chart.sh

# Specific metric
./scripts/generate_trend_chart.sh --type throughput

# Custom output
./scripts/generate_trend_chart.sh --output /tmp/charts
```

## Exception Handling

### Accepting Intentional Regressions

If a performance regression is intentional and justified:

1. **Document the reason**: Why is the tradeoff acceptable?
2. **Update baseline**: Use `baseline-update.sh` with justification
3. **Code review**: PR must be reviewed by maintainer
4. **Monitor**: Add follow-up issue to investigate optimization

```bash
# Example: Accepted regression for feature benefit
./tools/baseline-update.sh --reason "Added comprehensive logging for debugging - accepted 5% latency increase for improved observability"
```

### Exception Rules

Thresholds can be adjusted for specific scenarios:

```json
{
  "exceptions": {
    "rules": [
      {
        "name": "intentional_refactor",
        "requires": {
          "justification": "string",
          "reviewer_approval": true
        }
      },
      {
        "name": "experimental_features",
        "threshold_multiplier": 2.0
      }
    ]
  }
}
```

## Monitoring and Alerts

### Severity Levels

| Severity | Action | Examples |
|----------|--------|----------|
| **Critical** | Blocks merge | Error rate, concurrent connections, memory leaks |
| **High** | Blocks merge | P95/P99 latency, throughput |
| **Medium** | Warning only | P50 latency, memory, connection setup |
| **Low** | Informational | Minor improvements, noise |

### Alert Channels

- **GitHub PR Comment**: Automatic on all PRs
- **GitHub Actions UI**: Test results and artifacts
- **Log Output**: Structured logging in workflow runs

## Performance Metrics

### Benchmarks Tested

| Benchmark | Description | Duration | Key Metrics |
|-----------|-------------|----------|-------------|
| `core_ops_100k` | In-memory operations | ~1 min | Throughput, latency, memory |
| `tcp_quick_1k` | TCP connections | ~2 min | Throughput, latency, per-conn memory |
| `stress_30s_100k_ops` | Sustained load | ~5 min | Throughput, degradation rate |
| `integration_mcp_tool_sequence` | E2E workflow | ~1 min | Total latency, success rate |

### Target Performance

Based on MCP 2025-11-25 specification:

- **P50 Latency**: < 5ms
- **P95 Latency**: < 20ms
- **P99 Latency**: < 50ms
- **Throughput**: > 1000 req/s
- **Memory Per Connection**: < 100KB
- **Connection Setup**: < 100ms
- **Concurrent Connections**: 10K with 99% success rate

## Troubleshooting

### Workflow Failures

**Issue**: Performance tests fail intermittently

**Solution**:
1. Check CI environment logs for resource contention
2. Review `performance-regression-logs` artifact
3. Consider increasing timeout in workflow
4. Add exception rule for CI environment variance

**Issue**: False positive regressions

**Solution**:
1. Review baseline comparison report
2. Check for CI environment differences
3. Verify threshold configuration
4. Consider adjusting `environment_differences` exception

### Baseline Issues

**Issue**: No baseline found

**Solution**:
1. Run `./tools/baseline-capture.sh` on main branch
2. Verify `bench/baselines/` directory exists
3. Check git history for baseline files

**Issue**: Baseline out of date

**Solution**:
1. Update threshold configuration if needed
2. Run `./tools/baseline-capture.sh` to refresh
3. Review recent performance trends

## Best Practices

### For Developers

1. **Run tests locally before pushing**: `rebar3 ct --suite=erlmcp_performance_validator_SUITE`
2. **Monitor performance impact** during development
3. **Document tradeoffs** when accepting regressions
4. **Review PR comments** for performance feedback

### For Maintainers

1. **Review baseline updates** carefully
2. **Adjust thresholds** as system evolves
3. **Monitor trends** for gradual degradation
4. **Investigate regressions** before blocking merges

### For CI/CD

1. **Keep workflows updated** with threshold changes
2. **Monitor job duration** and optimize
3. **Review artifacts** for debugging
4. **Update documentation** as system evolves

## Integration with Other Quality Gates

Performance regression detection is part of the broader quality gate system:

```
┌──────────────────────────────────────────────────────────────┐
│                    Quality Gate System                        │
├──────────────────────────────────────────────────────────────┤
│ 1. Compilation       │ 2. Xref        │ 3. Dialyzer          │
│ 4. Unit Tests        │ 5. Coverage     │ 6. Integration       │
│ 7. Performance Regressions (THIS WORKFLOW)                   │
└──────────────────────────────────────────────────────────────┘
```

All gates must pass before merge is allowed.

## Related Documentation

- [Performance Validation Guide](VALIDATOR_GUIDE.md) - Performance validator details
- [Benchmarking Guide](../bench/README.md) - Benchmark usage
- [Metrology Glossary](../metrology/METRICS_GLOSSARY.md) - Metrics definitions
- [Toyota Production System](../tcps/TCPS.md) - Quality methodology

## Contributing

When modifying the performance regression system:

1. Update this documentation
2. Test workflow changes in a fork first
3. Review threshold configuration impacts
4. Verify baseline management still works
5. Check PR comment generation

## License

[MIT License](../../LICENSE)
