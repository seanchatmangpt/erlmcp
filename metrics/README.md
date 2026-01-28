# erlmcp Quality Metrics

This directory contains quality metrics snapshots and reports for the erlmcp project.

## Structure

```
metrics/
├── snapshots/           # JSON snapshots (one per capture)
│   ├── 2026-01-28.json
│   ├── 2026-01-29.json
│   └── ...
├── quality-trend.html   # Generated HTML trend report
└── README.md           # This file
```

## Quick Start

### Capture Snapshot
```bash
make metrics-snapshot
# OR
./tools/metrics/quality-snapshot.sh
```

### View Trends
```bash
make metrics-trend
# OR
./tools/metrics/quality-trend.sh --days 30
```

### Generate HTML Report
```bash
make metrics-trend-html
open metrics/quality-trend.html
```

### Comprehensive Report
```bash
make metrics-report
# OR
./tools/metrics/quality-report.sh
```

## Metrics Captured

Each snapshot includes:

- **Git Metadata**: Commit, branch, tag
- **Test Results**: Pass rate, counts
- **Code Coverage**: Percentage
- **Compilation**: Warnings count
- **Dialyzer**: Type checking warnings
- **Code Metrics**: LOC, complexity
- **Quality Score**: Overall score (0-100)

## Quality Score

Composite score calculated as:
```
Quality Score = (Test Pass Rate × 0.3) +
                (Coverage × 0.3) +
                (Compile Warnings × 0.2) +
                (Dialyzer Warnings × 0.2)
```

**Targets:**
- Excellent: 80-100
- Good: 60-79
- Needs Attention: 0-59

## Alert Thresholds

Automated alerts trigger when:

- Coverage drops > 5%
- Test pass rate drops > 5%
- Warnings increase > 5
- Quality score drops > 10 points

## Automation

Metrics are automatically captured:
- On every commit (CI)
- On every PR
- Daily at 00:00 UTC
- On manual trigger

## Documentation

See [docs/metrics/QUALITY_TRACKING.md](/Users/sac/erlmcp/docs/metrics/QUALITY_TRACKING.md) for complete documentation.

## Retention

- Snapshots: 90 days (CI artifacts)
- Reports: 30 days
- Git history: Permanent (committed to main)
