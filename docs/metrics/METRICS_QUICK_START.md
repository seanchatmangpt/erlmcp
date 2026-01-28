# Quality Metrics - Quick Start Guide

Get started with erlmcp quality metrics in 5 minutes.

## Installation

No installation needed! The metrics system is included with erlmcp.

## First Run

### 1. Capture Your First Snapshot

```bash
cd /Users/sac/erlmcp
make metrics-snapshot
```

**Expected output:**
```
=== erlmcp Quality Metrics Snapshot ===
Timestamp: 2026-01-28T12:00:00Z
Output: metrics/snapshots/2026-01-28.json

[1/7] Capturing Git metadata...
  Commit: abc123...
  Branch: main

[2/7] Compiling and counting warnings...
  Warnings: 0

[3/7] Counting lines of code...
  Source: 5000 lines in 25 modules
  Tests: 3000 lines in 20 modules

[4/7] Running tests...
  Passed: 150/150 (100.00%)

[5/7] Extracting coverage...
  Coverage: 85%

[6/7] Running Dialyzer...
  Warnings: 0

[7/7] Calculating complexity metrics...
  Functions: 300 (avg 12.0 per module)
  Avg module size: 200.0 LOC

✅ Snapshot saved to: metrics/snapshots/2026-01-28.json

=== Quality Summary ===
Overall Score: 87.5/100
Test Pass Rate: 100.00%
Coverage: 85%
Compile Warnings: 0
Dialyzer Warnings: 0

✅ Quality: EXCELLENT
```

### 2. View Current Metrics

```bash
cat metrics/snapshots/$(date +%Y-%m-%d).json | jq .
```

### 3. Generate a Report (after capturing multiple snapshots)

```bash
# Capture a few more snapshots on different days
make metrics-snapshot

# Then generate a trend report
make metrics-trend
```

## Daily Workflow

### Morning: Check Quality Status

```bash
make metrics-report
```

This shows:
- Current quality score
- Comparison to last week
- Top issues to fix
- Actionable recommendations

### Before Commit: Capture Snapshot

```bash
make metrics-snapshot
```

### Before PR: Check for Regressions

```bash
make metrics-ci
```

This will:
1. Capture a snapshot
2. Check for regressions
3. Exit with error if quality degraded

## Common Tasks

### View 7-Day Trend

```bash
make metrics-trend
# OR
./tools/metrics/quality-trend.sh --days 7
```

### Generate HTML Report

```bash
make metrics-trend-html
open metrics/quality-trend.html
```

### Weekly Report

```bash
make metrics-weekly
```

### Monthly Report

```bash
make metrics-monthly
```

### Clean Old Data

```bash
make metrics-clean
```

## Understanding Your Scores

### Quality Score: 87.5/100

**Calculation:**
```
Quality Score = (Test Pass Rate × 0.3) +
                (Coverage × 0.3) +
                (Compile Warnings × 0.2) +
                (Dialyzer Warnings × 0.2)
```

**Rating:**
- **80-100**: ✅ Excellent - Keep it up!
- **60-79**: ⚠️ Good - Room for improvement
- **0-59**: ❌ Needs Attention - Focus on quality

### Key Metrics

| Metric | Target | Your Value | Status |
|--------|--------|------------|--------|
| Test Pass Rate | 100% | 100% | ✅ |
| Coverage | ≥80% | 85% | ✅ |
| Compile Warnings | 0 | 0 | ✅ |
| Dialyzer Warnings | 0 | 0 | ✅ |
| Test/Code Ratio | ≥0.5 | 0.6 | ✅ |

## Alerts & Regressions

Automatic alerts trigger when:

| Issue | Threshold | Action |
|-------|-----------|--------|
| Coverage drops | >5% | Fix tests |
| Test pass rate drops | >5% | Fix failing tests |
| Warnings increase | >5 | Review warnings |
| Quality score drops | >10 points | Investigate cause |

## CI/CD Integration

Metrics are automatically captured on:
- Every push to main/develop
- Every pull request
- Daily at 00:00 UTC

### PR Comments

When you create a PR, GitHub Actions will:
1. Capture quality snapshot
2. Post comment with metrics table
3. Check for regressions
4. Fail PR if quality degraded

**Example PR Comment:**
```markdown
## ✅ Quality Metrics Report

**Overall Quality Score: 87.5/100**

| Metric | Value | Status |
|--------|-------|--------|
| Test Pass Rate | 100.00% | ✅ |
| Code Coverage | 85% | ✅ |
| Compile Warnings | 0 | ✅ |
| Dialyzer Warnings | 0 | ✅ |

**Code Metrics:**
- Source LOC: 5000
- Test LOC: 3000
- Test/Code Ratio: 0.6
```

## Troubleshooting

### No snapshots found

**Problem:** First time running metrics

**Solution:**
```bash
make metrics-snapshot
```

### Tests fail during snapshot

**Problem:** Failing tests prevent snapshot

**Solution:**
```bash
# Fix tests first
rebar3 eunit --verbose

# Then capture
make metrics-snapshot
```

### Can't generate trends

**Problem:** Need multiple snapshots

**Solution:**
```bash
# Capture snapshots on different days
# Or create test data
cp test/fixtures/metrics/*.json metrics/snapshots/
```

## Advanced Usage

### Custom Snapshot Location

```bash
./tools/metrics/quality-snapshot.sh metrics/custom-snapshot.json
```

### Specific Period Analysis

```bash
./tools/metrics/quality-trend.sh --days 14
```

### Weekly vs Monthly Comparison

```bash
# This week
./tools/metrics/quality-report.sh --period week

# This month
./tools/metrics/quality-report.sh --period month
```

## Next Steps

1. **Set up pre-commit hook** - Capture metrics before each commit
2. **Review weekly trends** - Every Monday morning
3. **Address regressions** - Fix quality issues immediately
4. **Improve coverage** - Target 90%+ coverage
5. **Maintain zero warnings** - Keep compile and Dialyzer clean

## Resources

- [Complete Documentation](QUALITY_TRACKING.md)
- [Technical Overview](METRICS_SYSTEM_OVERVIEW.md)
- [Dashboard API](../../src/erlmcp_metrics_dashboard.erl)
- [CI Workflow](../../.github/workflows/quality-metrics.yml)

## Need Help?

- Check the [troubleshooting section](QUALITY_TRACKING.md#troubleshooting)
- Review [examples](../../test/fixtures/metrics/)
- Open an issue on GitHub

---

**Remember:** Quality metrics are tools to help you, not judge you. Use them to improve continuously!
