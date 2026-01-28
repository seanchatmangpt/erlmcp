# erlmcp Plan Conformance Test Results

This directory contains JSON-formatted results from the plan conformance validation test suite.

## Directory Structure

```
conformance_results/
├── README.md                          # This file
├── .gitignore                         # Ignore generated result files
├── SCHEMA.json                        # JSON schema for result files
└── [results from test runs]
    ├── team_test_name_timestamp.json
    ├── enterprise_test_name_timestamp.json
    ├── gov_test_name_timestamp.json
    └── summary_run_id.json
```

## Result File Format

Individual test results follow this JSON schema:

```json
{
  "plan": "team|enterprise|gov",
  "test": "test_name",
  "status": "pass|fail",
  "measured_value": 485.2,
  "required_value": 450,
  "unit": "req/s|ms|MB|messages|status",
  "tolerance_pct": 5.0,
  "actual_vs_required_ratio": 1.078,
  "runs": [482.5, 487.1, 486.0],
  "run_variance_pct": 0.45,
  "timestamp": 1706000000000
}
```

### Field Descriptions

- **plan**: Tier being validated (team, enterprise, or gov)
- **test**: Name of the specific test that was run
- **status**: Overall result (pass or fail)
- **measured_value**: Average value across all runs
- **required_value**: Threshold from plan envelope definition
- **unit**: Unit of measurement (requests/sec, milliseconds, MB, etc.)
- **tolerance_pct**: Acceptable variance from required value
- **actual_vs_required_ratio**: Ratio of measured to required (>1.0 means exceeds requirement)
- **runs**: Array of measured values from each run (typically 3 runs)
- **run_variance_pct**: Variance across runs as percentage (should be <2%)
- **timestamp**: Unix millisecond timestamp when test was executed

## Summary File Format

Suite-level summary files are named `summary_run_id.json`:

```json
{
  "test_run_id": 12345678,
  "start_time": 1706000000000,
  "end_time": 1706001000000,
  "duration_ms": 1000000,
  "timestamp": 1706001000000,
  "results_dir": "/Users/sac/erlmcp/conformance_results"
}
```

## Test Coverage

### Team Tier (6 tests)
- Throughput: 450 req/s
- P99 Latency: 150ms
- Memory per connection: 2.03MB
- Failover time: <5s
- Queue depth: 100K messages
- Refusal behavior: Deterministic

### Enterprise Tier (6 tests)
- Throughput: 1500 req/s
- P99 Latency: 100ms
- Memory per connection: 1.5MB
- Failover time: <2s
- Queue depth: 500K messages
- Refusal behavior: Deterministic

### Government Tier (6 tests)
- Throughput: 900 req/s
- P99 Latency: 80ms
- Memory per connection: 1.2MB
- Failover time: <1s
- Audit logging: Enabled
- FIPS compliance: Verified

### Cross-Plan Tests (3 tests)
- Plan upgrade: team→enterprise envelope expansion
- Boundary refusal: Exact threshold behavior
- Coexistence: Multiple plans running simultaneously

## Determinism Analysis

Each test runs 3 times and reports:
- Individual measurements from each run
- Variance across runs (should be ±2% max)
- This proves consistent, reproducible performance

Example:
```json
{
  "runs": [482.5, 487.1, 486.0],
  "run_variance_pct": 0.45,
  "measured_value": 485.2
}
```

The variance of 0.45% indicates stable, deterministic performance.

## Using Results

### Manual Inspection
```bash
# View individual test result
cat conformance_results/team_throughput_1706000000123.json | jq .

# Check all team tier results
ls conformance_results/team_*.json | xargs cat | jq '.status'

# Verify all tests passed
grep -h '"status"' conformance_results/*.json | sort | uniq -c
```

### Evidence Bundle Integration
Results are automatically integrated into the supply chain evidence system by:

1. Test runner exports JSON files
2. Evidence system discovers result files
3. Results included in SBOM/provenance/VEX artifacts
4. Used for regression detection in future runs

### Continuous Integration
In CI/CD pipelines, results are typically:
- Uploaded as test artifacts
- Compared against baseline results
- Used to detect performance regressions
- Included in release documentation

## Baseline Comparison

When establishing baseline metrics:

1. Run full conformance suite
2. Export all results
3. Store baseline copies (e.g., in `conformance_results/baseline/`)
4. Compare future runs against baseline
5. Alert on >10% regression in any metric

Example baseline tracking:
```bash
# Save baseline
mkdir -p baseline
cp team_*.json enterprise_*.json gov_*.json baseline/

# Later: compare current run
for f in baseline/team_*.json; do
  test_name=$(basename "$f")
  if [ -f "$test_name" ]; then
    jq -r '.measured_value' "$test_name" > /tmp/current.txt
    jq -r '.measured_value' "$f" > /tmp/baseline.txt
    # Compare and alert on >10% variance
  fi
done
```

## Automation Examples

### Processing All Results
```bash
# Extract all test statuses
jq -r '[.plan, .test, .status] | @csv' conformance_results/*.json

# Calculate average ratios (all tests should be >1.0)
jq -r '.actual_vs_required_ratio' conformance_results/*.json | awk '{sum+=$1; count++} END {print "Average ratio:", sum/count}'

# Find slowest latency result
jq -r 'select(.unit == "ms") | .measured_value' conformance_results/*.json | sort -n | tail -1

# List all passed tests
jq -r 'select(.status == "pass") | [.plan, .test] | @csv' conformance_results/*.json
```

### Performance Trend Analysis
```bash
# Track throughput over time
jq -r '[.timestamp, .measured_value] | @csv' conformance_results/team_throughput_*.json | sort -t, -k1

# Monthly comparison
for month in $(seq 1 12); do
  results=$(find conformance_results -name "*.json" -newermt "2026-$month-01" -oldermt "2026-$(($month+1))-01" | head -1)
  if [ -f "$results" ]; then
    echo "Month $month: $(jq '.measured_value' "$results")"
  fi
done
```

## Troubleshooting

### Missing Results
- Ensure test suite completes all 21 tests
- Check `/tmp/erlmcp_*` for diagnostic files
- Review `_build/test/logs/` for test execution logs
- Run with verbose flag: `rebar3 ct -v --suite=erlmcp_plan_conformance_SUITE`

### JSON Parse Errors
```bash
# Validate JSON syntax
for f in conformance_results/*.json; do
  jq empty < "$f" || echo "Invalid: $f"
done

# Pretty print for inspection
jq . conformance_results/team_throughput_*.json | head -50
```

### Comparing Results
```bash
# Detailed comparison between two runs
diff <(jq -S . conformance_results/team_throughput_run1.json) \
     <(jq -S . conformance_results/team_throughput_run2.json)
```

## Integration with CI/CD

### GitHub Actions
```yaml
- name: Export Results
  if: always()
  uses: actions/upload-artifact@v3
  with:
    name: conformance-results
    path: conformance_results/
    retention-days: 90
```

### Build Failure on Conformance Failure
```bash
make certify-plan || (echo "Plan conformance FAILED" && exit 1)
```

## License

Results and metrics are property of erlmcp project.
See `/Users/sac/erlmcp/LICENSE` for details.

## Further Reading

- See `/Users/sac/erlmcp/docs/plan_conformance_testing.md` for full documentation
- See `/Users/sac/erlmcp/test/erlmcp_plan_conformance_SUITE.erl` for test implementation
- See `/Users/sac/erlmcp/test/erlmcp_benchmark.erl` for benchmark framework

---

Generated by erlmcp Plan Conformance Validation System
Last Updated: 2026-01-27
