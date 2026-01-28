# Quality Metrics Tracking System

## Overview

The erlmcp quality metrics tracking system provides comprehensive monitoring of code quality over time, enabling trend detection, regression alerts, and data-driven quality improvements.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   Quality Metrics System                     │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────┐      ┌──────────────┐      ┌────────────┐ │
│  │  Snapshot   │─────→│   Storage    │─────→│   Trend    │ │
│  │  Capture    │      │   (JSON)     │      │  Analysis  │ │
│  └─────────────┘      └──────────────┘      └────────────┘ │
│        │                     │                      │        │
│        │                     │                      │        │
│        ▼                     ▼                      ▼        │
│  ┌─────────────┐      ┌──────────────┐      ┌────────────┐ │
│  │   Metrics   │      │  Dashboard   │      │   Alerts   │ │
│  │ Calculation │      │  (Real-time) │      │ & Reports  │ │
│  └─────────────┘      └──────────────┘      └────────────┘ │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

## Components

### 1. Quality Snapshot (`quality-snapshot.sh`)

Captures current quality metrics including:

- **Git Metadata**: Commit hash, branch, tag
- **Compilation**: Success/failure, warning count
- **Tests**: Pass rate, total/passed/failed counts
- **Coverage**: Percentage coverage
- **Dialyzer**: Type checking warnings
- **Code Metrics**: LOC, module count, complexity
- **Quality Score**: Composite score (0-100)

**Usage:**
```bash
# Capture snapshot (auto-named by date)
./tools/metrics/quality-snapshot.sh

# Capture to specific file
./tools/metrics/quality-snapshot.sh metrics/snapshots/custom.json
```

**Output Format:**
```json
{
  "timestamp": "2026-01-28T12:00:00Z",
  "date": "2026-01-28",
  "git": {
    "hash": "abc123...",
    "branch": "main",
    "tag": "v0.6.0"
  },
  "tests": {
    "success": true,
    "total": 150,
    "passed": 150,
    "failed": 0,
    "pass_rate": 100.0
  },
  "coverage": {
    "percentage": 85
  },
  "quality_score": {
    "overall": 87.5
  }
}
```

### 2. Trend Analysis (`quality-trend.sh`)

Analyzes historical snapshots to detect trends and generate reports.

**Usage:**
```bash
# Analyze last 30 days (default)
./tools/metrics/quality-trend.sh

# Analyze last 7 days
./tools/metrics/quality-trend.sh --days 7

# Generate HTML report
./tools/metrics/quality-trend.sh --html metrics/quality-report.html
```

**Features:**
- Trend detection (improving/degrading/stable)
- Alert thresholds:
  - Coverage drop > 5%
  - Test pass rate drop > 5%
  - Warnings increase > 5
  - Quality score drop > 10 points
- Interactive HTML charts (Chart.js)

### 3. Quality Report (`quality-report.sh`)

Comprehensive quality report with actionable recommendations.

**Usage:**
```bash
# Weekly report (default)
./tools/metrics/quality-report.sh

# Monthly report
./tools/metrics/quality-report.sh --period month
```

**Output Sections:**
1. Current State (all metrics)
2. Comparison to Previous Period (week/month)
3. Top 10 Issues to Fix (prioritized)
4. Recommendations (actionable items)
5. Next Actions (concrete steps)

### 4. Metrics Dashboard (`erlmcp_metrics_dashboard.erl`)

Real-time Erlang/OTP gen_server for metrics monitoring.

**API:**
```erlang
% Start dashboard
{ok, Pid} = erlmcp_metrics_dashboard:start_link([{snapshots_dir, "metrics/snapshots"}]).

% Get current metrics
{ok, Metrics} = erlmcp_metrics_dashboard:get_current_metrics().

% Get 30-day trend
{ok, Trend} = erlmcp_metrics_dashboard:get_trend(30).

% Check for regressions
ok = erlmcp_metrics_dashboard:alert_on_regression().
% OR
{alert, Alerts} = erlmcp_metrics_dashboard:alert_on_regression().
```

**Features:**
- Automatic snapshot loading
- Real-time metric updates
- Regression detection
- Subscriber notifications
- Configurable alert thresholds

### 5. CI Integration (GitHub Actions)

Automated metrics capture on every commit and PR.

**Workflow Triggers:**
- Push to main/develop
- Pull requests
- Daily scheduled runs (00:00 UTC)

**Actions:**
1. Capture quality snapshot
2. Upload artifact (90-day retention)
3. Analyze trends
4. Check for regressions
5. Comment on PR with metrics
6. Fail PR if regression detected
7. Commit metrics to history (main only)

## Metrics Explained

### Quality Score (0-100)

Composite score calculated as:
```
Quality Score = (Test Pass Rate × 0.3) +
                (Coverage × 0.3) +
                (Compile Warnings Score × 0.2) +
                (Dialyzer Warnings Score × 0.2)

Where:
- Compile Warnings Score = 100 if 0 warnings, else 50
- Dialyzer Warnings Score = 100 if 0 warnings, else 50
```

**Interpretation:**
- **80-100**: Excellent quality
- **60-79**: Good quality (room for improvement)
- **0-59**: Needs attention

### Test Pass Rate

Percentage of tests passing:
```
Pass Rate = (Tests Passed / Total Tests) × 100
```

**Target:** 100% (all tests passing)

### Code Coverage

Percentage of code lines executed by tests:
```
Coverage = (Lines Executed / Total Lines) × 100
```

**Target:** ≥ 80%

### Test/Code Ratio

Ratio of test code to source code:
```
Test/Code Ratio = Test LOC / Source LOC
```

**Target:** ≥ 0.5 (at least half as much test code as source code)

## Alert Thresholds

| Metric | Threshold | Action |
|--------|-----------|--------|
| Coverage drop | > 5% | Alert + block PR |
| Test pass rate drop | > 5% | Alert + block PR |
| Warnings increase | > 5 | Alert + recommend fix |
| Quality score drop | > 10 points | Alert + block PR |

## Best Practices

### Daily Workflow

1. **Morning**: Check quality report
   ```bash
   ./tools/metrics/quality-report.sh
   ```

2. **Before Commit**: Capture snapshot
   ```bash
   ./tools/metrics/quality-snapshot.sh
   ```

3. **Before PR**: Check trends
   ```bash
   ./tools/metrics/quality-trend.sh --days 7
   ```

### Weekly Workflow

1. **Monday**: Review weekly trends
   ```bash
   ./tools/metrics/quality-trend.sh --html reports/weekly-$(date +%Y-%m-%d).html
   ```

2. **Friday**: Analyze quality report
   ```bash
   ./tools/metrics/quality-report.sh --period week
   ```

### Monthly Workflow

1. **First of Month**: Generate comprehensive report
   ```bash
   ./tools/metrics/quality-report.sh --period month > reports/monthly-$(date +%Y-%m).txt
   ```

2. **Review**: Analyze trends and set goals for next month

## Integration Examples

### Makefile Integration

```makefile
.PHONY: metrics-snapshot metrics-trend metrics-report

metrics-snapshot:
	@./tools/metrics/quality-snapshot.sh

metrics-trend:
	@./tools/metrics/quality-trend.sh --days 30 --html metrics/trend.html
	@open metrics/trend.html

metrics-report:
	@./tools/metrics/quality-report.sh
```

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

echo "Capturing quality snapshot..."
./tools/metrics/quality-snapshot.sh

echo "Checking for regressions..."
if ./tools/metrics/quality-trend.sh --days 7 | grep "DEGRADING (alert threshold exceeded)"; then
    echo "❌ Quality regression detected. Fix before committing."
    exit 1
fi

echo "✅ Quality checks passed"
```

### Dashboard Integration

```erlang
% Start metrics dashboard in supervisor
{erlmcp_metrics_dashboard, 
 {erlmcp_metrics_dashboard, start_link, [[{snapshots_dir, "metrics/snapshots"}]]},
 permanent, 5000, worker, [erlmcp_metrics_dashboard]}
```

## Troubleshooting

### No snapshots found

**Problem:** `quality-trend.sh` reports "No snapshots found"

**Solution:**
```bash
# Capture initial snapshot
./tools/metrics/quality-snapshot.sh

# Verify
ls -la metrics/snapshots/
```

### Tests failing during snapshot

**Problem:** Snapshot fails due to test failures

**Solution:**
```bash
# Fix tests first
rebar3 eunit --verbose

# Then capture snapshot
./tools/metrics/quality-snapshot.sh
```

### HTML report not generating

**Problem:** `quality-trend.sh --html` creates empty file

**Solution:**
```bash
# Ensure multiple snapshots exist
ls -la metrics/snapshots/*.json | wc -l

# Need at least 2 snapshots for trends
./tools/metrics/quality-snapshot.sh
```

### CI workflow failing

**Problem:** GitHub Actions quality-metrics.yml fails

**Solution:**
1. Check workflow permissions (needs write access for commits)
2. Verify rebar3 version compatibility
3. Check artifact retention settings
4. Review workflow logs for specific errors

## Future Enhancements

1. **Real-time Dashboard UI**: Web-based dashboard with live updates
2. **Metric Forecasting**: Predict future trends using ML
3. **Custom Metrics**: Support user-defined quality metrics
4. **Integration Tests**: Metrics for integration test coverage
5. **Performance Metrics**: Track benchmark results over time
6. **Team Metrics**: Per-developer quality metrics
7. **SLA Tracking**: Quality SLA monitoring and alerts
8. **Slack/Email Alerts**: Automated notifications on regressions

## References

- [Erlang Code Coverage](https://www.erlang.org/doc/man/cover.html)
- [Dialyzer User Guide](https://www.erlang.org/doc/man/dialyzer.html)
- [Chart.js Documentation](https://www.chartjs.org/docs/)
- [GitHub Actions Artifacts](https://docs.github.com/en/actions/using-workflows/storing-workflow-data-as-artifacts)

## License

Same as erlmcp project (Apache 2.0)
