# Performance Validation Guide

Quick guide for using the validation benchmark script.

## Quick Start

```bash
# Run validation with default settings
./scripts/bench/run_validation_benchmarks.sh

# Expected output: All metrics should PASS
```

## What It Does

1. **Runs Benchmark**: Executes `erlmcp_bench_core_ops:run(<<"core_ops_10k">>)`
2. **Loads Baseline**: Reads metrics from `bench/baselines/2026-01-28_v2.0.0.json`
3. **Compares Metrics**: Calculates % difference for each metric
4. **Enforces Threshold**: Fails if regression > 10%
5. **Reports Results**: Clear pass/fail per metric

## Baseline Metrics (v2.0.0)

| Metric | Baseline | Threshold |
|--------|----------|-----------|
| Throughput | 2.83M msg/s | <10% regression |
| Latency P95 | 81 µs | <10% increase |
| Latency P99 | 97 µs | <10% increase |
| Memory | 2.0 MiB | <10% increase |

## Common Usage Patterns

### Before Committing Code

```bash
# Quick validation check
./scripts/bench/run_validation_benchmarks.sh

# If it fails, investigate before committing
```

### In CI/CD Pipeline

```yaml
- name: Performance Validation
  run: ./scripts/bench/run_validation_benchmarks.sh --threshold 10
```

### After Performance Changes

```bash
# Run with more lenient threshold during development
./scripts/bench/run_validation_benchmarks.sh --threshold 15

# When satisfied, create new baseline
./scripts/bench/set_baseline.sh
```

## Interpreting Results

### All Tests Passed (Exit 0)

```
Overall Status: ALL TESTS PASSED
[SUCCESS] No performance regression detected
```

**Action:** Safe to proceed. No performance issues detected.

### Performance Regression (Exit 1)

```
Overall Status: PERFORMANCE REGRESSION
[ERROR] 2 metric(s) exceeded threshold of 10%
```

**Action:** Investigate the failed metrics:
- Check recent code changes
- Review algorithmic complexity
- Look for memory leaks
- Verify test environment is consistent

### Benchmark Failed (Exit 2)

**Action:** Technical issue to fix:
- Ensure code compiles: `TERM=dumb rebar3 compile`
- Check benchmark module exists
- Verify Erlang runtime is working

### Missing Dependencies (Exit 3)

**Action:** Install missing tools:
- `jq` for JSON processing
- `rebar3` for building
- Erlang/OTP runtime

## Tips

1. **Run Multiple Times**: Performance can vary slightly. Run 3-5 times for consistency.
2. **Consistent Environment**: Use similar hardware/load for baseline and validation.
3. **Update Baselines**: When performance improves genuinely, create new baseline.
4. **Verbose Mode**: Use `--verbose` to see detailed metric values during execution.
5. **Adjust Thresholds**: Use `--threshold` for development vs. production.

## Related Files

- **Script**: `/Users/sac/erlmcp/scripts/bench/run_validation_benchmarks.sh`
- **Baseline**: `/Users/sac/erlmcp/bench/baselines/2026-01-28_v2.0.0.json`
- **Documentation**: `/Users/sac/erlmcp/scripts/bench/README.md`
- **Results**: `/Users/sac/erlmcp/bench/results/`
