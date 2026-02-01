# Benchmark Regression Detection

## Overview

Enhanced benchmark comparison script with configurable thresholds for automated regression detection in CI/CD pipelines.

## Scripts

### compare_to_baseline_enhanced.sh

Compares current benchmark results against baseline with configurable, per-metric thresholds.

**Features:**
- Multiple output formats (JSON, text, markdown)
- Per-metric threshold configuration
- Severity-based regression reporting
- File or directory input support
- Backward compatible with existing scripts

**Usage:**

```bash
# New flag-based usage
./scripts/bench/compare_to_baseline_enhanced.sh \
  --results=bench/results/current.json \
  --baseline=bench/results/baseline.json \
  --thresholds=.github/performance-thresholds.json \
  --output=regression_report.json \
  --format=json

# Legacy usage (backward compatible)
./scripts/bench/compare_to_baseline_enhanced.sh bench/results/20260201_150000 10
```

**Options:**
- `--results=FILE` - Path to current benchmark results (JSON file or directory)
- `--baseline=FILE` - Path to baseline benchmark results (JSON file or directory)
- `--thresholds=FILE` - Path to thresholds configuration JSON (default: `.github/performance-thresholds.json`)
- `--output=FILE` - Output path for regression report (default: stdout)
- `--format=FORMAT` - Output format: json, text, markdown (default: text)
- `--threshold=PCT` - Global regression threshold % (default: 10)
- `--help` - Show help message

**Exit Codes:**
- 0 - No regressions detected
- 1 - Invalid input or baseline not found
- 2 - Regressions detected above threshold

## Threshold Configuration

The script reads thresholds from `.github/performance-thresholds.json`:

```json
{
  "thresholds": {
    "latency": {
      "p50": {"threshold_percent": 10, "direction": "increase", "severity": "medium"},
      "p95": {"threshold_percent": 15, "direction": "increase", "severity": "high"},
      "p99": {"threshold_percent": 20, "direction": "increase", "severity": "high"}
    },
    "throughput": {
      "threshold_percent": 10,
      "direction": "decrease",
      "severity": "high"
    },
    "memory": {
      "total": {"threshold_percent": 25, "direction": "increase", "severity": "medium"}
    }
  }
}
```

### Threshold Levels

- **Latency P50:** ≤ 10% increase (medium severity)
- **Latency P95:** ≤ 15% increase (high severity)
- **Latency P99:** ≤ 20% increase (high severity)
- **Throughput:** ≤ 10% decrease (high severity)
- **Memory:** ≤ 25% increase (medium severity)

### Severity Classification

- **Critical:** > 2× threshold (blocks merge)
- **High:** > 1.5× threshold (blocks merge with warning)
- **Medium:** > threshold (warning only)

## Metrics Analyzed

### 1. Throughput (`throughput_msg_per_s`)

**Direction:** Higher is better
**Threshold:** 10% decrease
**Formula:** `regression% = -((current - baseline) / baseline × 100)`

### 2. Latency Percentiles

**Direction:** Lower is better
**Metrics:**
- `latency_p50_us` - Median latency (threshold: 10%)
- `latency_p95_us` - 95th percentile (threshold: 15%)
- `latency_p99_us` - 99th percentile (threshold: 20%)

**Formula:** `regression% = (current - baseline) / baseline × 100`

### 3. Memory Usage (`memory_rss_mib_per_node`)

**Direction:** Lower is better
**Threshold:** 25% increase
**Formula:** `regression% = (current - baseline) / baseline × 100`

## Output Formats

### Text Format (Default)

```
========================================
  Regression Analysis
========================================

Current:    bench/results/current.json
Baseline:   bench/results/baseline.json
Thresholds: Latency P50=10%, P95=15%, P99=20%; Throughput=10%; Memory=25%
Format:     text

Loading baseline results...
  Loaded 1 baseline results
Loading current results...
  Loaded 1 current results

================================================================================
Benchmark               Metric          Baseline      Current       Change
--------------------------------------------------------------------------------
core_ops_1k             throughput      1000000.00     950000.00 +5.0%
core_ops_1k             latency_p50          5.00           6.00 +20.0%
core_ops_1k             latency_p95         15.00          18.00 +20.0%
core_ops_1k             latency_p99         25.00          30.00 +20.0%
core_ops_1k             memory             100.00         110.00 +10.0%
================================================================================

✗ 2 regression(s) detected:
  [MEDIUM] core_ops_1k / latency_p50: +20.0% (5.00 → 6.00, threshold: 10.0%)
  [HIGH] core_ops_1k / latency_p95: +20.0% (15.00 → 18.00, threshold: 15.0%)
```

### JSON Format

```json
{
  "timestamp": 1738425600,
  "results_file": "bench/results/current.json",
  "baseline_file": "bench/results/baseline.json",
  "thresholds_file": ".github/performance-thresholds.json",
  "regressions": [
    {
      "workload": "core_ops_1k",
      "metric": "latency_p50",
      "metric_key": "latency_p50_us",
      "baseline": 5.0,
      "current": 6.0,
      "regression_percent": 20.0,
      "threshold_percent": 10.0,
      "severity": "medium",
      "direction": "lower_is_better"
    },
    {
      "workload": "core_ops_1k",
      "metric": "latency_p95",
      "metric_key": "latency_p95_us",
      "baseline": 15.0,
      "current": 18.0,
      "regression_percent": 20.0,
      "threshold_percent": 15.0,
      "severity": "high",
      "direction": "lower_is_better"
    }
  ],
  "summary": {
    "total_comparisons": 1,
    "regressions_detected": 2,
    "status": "failed"
  }
}
```

## Integration with CI/CD

### GitHub Actions

```yaml
name: Performance Regression Check

on:
  pull_request:
    branches: [main]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Run Benchmarks
        run: ./scripts/bench/run_all_benchmarks.sh

      - name: Compare to Baseline
        id: regression
        run: |
          ./scripts/bench/compare_to_baseline_enhanced.sh \
            --results=bench/results/latest \
            --baseline=bench/results/baseline \
            --thresholds=.github/performance-thresholds.json \
            --output=regression_report.json \
            --format=json || echo "regressions_detected=true" >> $GITHUB_OUTPUT

      - name: Parse Regressions
        if: steps.regression.outputs.regressions_detected == 'true'
        run: |
          echo "## Performance Regressions Detected" >> $GITHUB_STEP_SUMMARY
          jq -r '.regressions[] | "- [\(.severity | ascii_upcase)] \(.workload) / \(.metric): +\(.regression_percent)% (\(.baseline) → \(.current), threshold: \(.threshold_percent)%)"' regression_report.json >> $GITHUB_STEP_SUMMARY

      - name: Upload Report
        uses: actions/upload-artifact@v3
        with:
          name: regression-report
          path: regression_report.json

      - name: Block Merge on High Severity
        run: |
          HIGH_COUNT=$(jq '[.regressions[] | select(.severity == "high" or .severity == "critical")] | length' regression_report.json)
          if [ "$HIGH_COUNT" -gt 0 ]; then
            echo "❌ Blocking merge: $HIGH_COUNT high/critical severity regressions detected"
            exit 1
          fi
```

### Pre-merge Check

```bash
#!/bin/bash
# scripts/ci/check_performance.sh

set -e

echo "Running benchmarks..."
./scripts/bench/quick.sh

echo "Comparing to baseline..."
if ./scripts/bench/compare_to_baseline_enhanced.sh \
    --results=bench/results/latest \
    --baseline=bench/results/baseline \
    --thresholds=.github/performance-thresholds.json \
    --format=text; then
  echo "✓ No performance regressions detected"
  exit 0
else
  EXIT_CODE=$?
  if [ $EXIT_CODE -eq 2 ]; then
    echo "⚠ Performance regressions detected (see above)"
    exit 2
  else
    echo "❌ Regression check failed"
    exit 1
  fi
fi
```

## Baseline Management

### Setting a New Baseline

```bash
# Run benchmarks
./scripts/bench/run_all_benchmarks.sh

# Set as baseline
./scripts/bench/set_baseline.sh bench/results/latest
```

### Baseline Versioning

```bash
# Tag baseline with version
cp -r bench/results/baseline bench/results/baseline_v2.1.0

# List available baselines
ls -lh bench/results/baseline*
```

## Examples

### Quick Regression Check

```bash
./scripts/bench/compare_to_baseline_enhanced.sh \
  --results=bench/results/latest \
  --baseline=bench/results/baseline \
  --format=text
```

### JSON Output for Parsing

```bash
./scripts/bench/compare_to_baseline_enhanced.sh \
  --results=bench/results/latest \
  --baseline=bench/results/baseline \
  --format=json > regression.json

# Extract high severity regressions
jq '.regressions[] | select(.severity == "high")' regression.json
```

### Custom Thresholds

```bash
# Create custom thresholds
cat > custom_thresholds.json <<EOF
{
  "thresholds": {
    "latency": {
      "p50": {"threshold_percent": 5},
      "p95": {"threshold_percent": 10},
      "p99": {"threshold_percent": 15}
    },
    "throughput": {"threshold_percent": 5},
    "memory": {"total": {"threshold_percent": 15}}
  }
}
EOF

./scripts/bench/compare_to_baseline_enhanced.sh \
  --results=bench/results/latest \
  --baseline=bench/results/baseline \
  --thresholds=custom_thresholds.json
```

### Directory-Based Comparison

```bash
# Compare all benchmark files in directories
./scripts/bench/compare_to_baseline_enhanced.sh \
  --results=bench/results/20260201_150000 \
  --baseline=bench/results/baseline \
  --format=text
```

## Troubleshooting

### "No baseline found"

Ensure baseline exists:
```bash
ls -la bench/results/baseline
./scripts/bench/set_baseline.sh bench/results/latest
```

### "Thresholds file not found"

Create thresholds file or use default:
```bash
cp .github/performance-thresholds.json.example .github/performance-thresholds.json
```

### Erlang Not Found

Ensure Erlang is in PATH:
```bash
which erl
export PATH=$PATH:/path/to/erlang/bin
```

## Related Files

- `.github/performance-thresholds.json` - Threshold configuration
- `scripts/bench/run_all_benchmarks.sh` - Full benchmark suite
- `scripts/bench/quick.sh` - Quick benchmark runner
- `scripts/bench/set_baseline.sh` - Baseline setter
- `bench/results/baseline/` - Current baseline
- `bench/results/latest/` - Latest benchmark results

## See Also

- [Benchmark Execution Guide](../../archive/benchmarks/BENCHMARK_EXECUTION_GUIDE.md)
- [Performance Validation](../../docs/validation/performance_validation.md)
- [CI/CD Integration](../../docs/ci_cd_integration.md)
