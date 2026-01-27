# erlmcp + TAIEA Performance Benchmarks

## Overview

This document describes the performance benchmarking infrastructure for the erlmcp workspace, including TAIEA request handling. The benchmark suite measures throughput, latency distribution, memory usage, and identifies performance regressions.

## Baseline Performance Targets

All latencies are measured in milliseconds (ms) under normal operating conditions.

### Operation-Specific Targets

| Operation | Target (P95) | Target (P99) | Target (Max) |
|-----------|--------------|--------------|--------------|
| Health Check | < 10 ms | < 15 ms | < 50 ms |
| Entitlement Apply | < 50 ms | < 75 ms | < 200 ms |
| Receipt Verify | < 100 ms | < 150 ms | < 500 ms |
| Support Model | < 20 ms | < 30 ms | < 100 ms |

### Aggregate Throughput Targets

| Metric | Target | Target Load |
|--------|--------|-------------|
| Overall Throughput | > 1000 req/sec | Mixed workload |
| Health Check Throughput | > 5000 req/sec | Health checks only |
| Entitlement Throughput | > 500 req/sec | Entitlements only |
| Receipt Throughput | > 200 req/sec | Receipts only |
| Support Throughput | > 2000 req/sec | Support only |

### Memory Efficiency Targets

| Metric | Target |
|--------|--------|
| Memory per Request | < 10 KB |
| Stable Operation Memory | < 500 MB |
| Memory Leak Detection | None detected over 1M operations |

## Running Benchmarks

### All Benchmarks

```bash
cd /Users/sac/erlmcp
rebar3 ct --suite=throughput_SUITE
rebar3 ct --suite=latency_SUITE
```

### Specific Operation Benchmarks

```bash
# Health check benchmarks only
rebar3 ct --suite=throughput_SUITE --case=health_check_baseline

# Entitlement apply benchmarks
rebar3 ct --suite=throughput_SUITE --case=entitlement_apply_concurrent_1000

# Receipt verify benchmarks
rebar3 ct --suite=throughput_SUITE --case=receipt_verify_concurrent_100

# Support model benchmarks
rebar3 ct --suite=throughput_SUITE --case=support_model_concurrent_100
```

### Concurrent Load Testing

```bash
# Test with increasing concurrency
rebar3 ct --suite=throughput_SUITE --case=health_check_concurrent_10
rebar3 ct --suite=throughput_SUITE --case=health_check_concurrent_100
rebar3 ct --suite=throughput_SUITE --case=health_check_concurrent_1000
```

### Mixed Workload Testing

```bash
# Baseline mixed operations
rebar3 ct --suite=throughput_SUITE --case=mixed_workload_baseline

# Sustained load (30 seconds)
rebar3 ct --suite=throughput_SUITE --case=mixed_workload_sustained

# Spike testing (sudden load increase)
rebar3 ct --suite=throughput_SUITE --case=mixed_workload_spike
```

### Latency Distribution

```bash
# Test latency stability
rebar3 ct --suite=latency_SUITE --case=latency_stability_test

# Test latency under load
rebar3 ct --suite=latency_SUITE --case=latency_under_load_test

# Analyze tail latencies (P99, P99.9)
rebar3 ct --suite=latency_SUITE --case=latency_tail_analysis

# Test variance
rebar3 ct --suite=latency_SUITE --case=latency_variance_test

# Measure memory per request
rebar3 ct --suite=latency_SUITE --case=memory_per_request
```

## Interpreting Results

### Throughput Metrics

**Requests/Second (RPS)**
- Shows how many operations complete per second
- Higher is better
- Measured at different concurrency levels (10, 100, 1000)
- Indicates system capacity

Example output:
```
health_check_concurrent_100: 4,500 req/sec
entitlement_concurrent_100: 450 req/sec
receipt_concurrent_100: 150 req/sec
```

### Latency Metrics

**Percentile Latencies**
- **P50 (Median)**: 50% of requests complete faster than this time
- **P95**: 95% of requests complete faster than this time (critical for SLAs)
- **P99**: 99% of requests complete faster than this time (important for user experience)
- **Max**: Worst-case latency observed

Example output:
```
health_check_baseline:
  Min: 0.15 ms
  P50: 1.20 ms
  P95: 8.50 ms (target: 10 ms) ✓
  P99: 12.30 ms
  Max: 45 ms
```

**Interpreting Deviations**

- If P95 approaches or exceeds target: Performance degradation, investigate system state
- If P99 >> P95: High variance, possible GC pauses or scheduling issues
- If Max >> P99: Outliers or rare blocking operations

### Memory Metrics

**Memory per Request**
- Average memory allocated per request (excluding system overhead)
- Should remain constant regardless of load
- Indicates memory leaks if increasing

Example:
```
Memory per request: 2.3 KB
Total memory delta: 2,300 KB (for 1000 operations)
```

### Regression Detection

**Manual Comparison Method**

1. Run baseline benchmark suite
2. Make code changes
3. Run benchmarks again
4. Compare metrics:
   - If P95 increases > 10%: potential regression
   - If throughput decreases > 10%: potential regression
   - If memory/request increases: potential memory leak

Example:
```
Before:  P95: 8.5 ms, Throughput: 4500 req/sec
After:   P95: 9.8 ms, Throughput: 4100 req/sec
Change:  +15%, -9% → Investigate
```

## Performance Benchmarking Methodology

### Warmup Phase

All benchmarks include a warmup phase with 100 iterations before measurements. This allows:
- JIT compilation to occur
- Cache warming
- Scheduling to stabilize
- System to reach steady state

### Measurement Period

- **Baseline tests**: 1000 iterations (cold start + measurements)
- **Concurrent tests**: 10-second duration at specified concurrency level
- **Mixed workload**: Operations randomly distributed across all types

### Statistical Analysis

Results report:
- **Min/Max**: Best and worst-case latencies
- **Average**: Mean latency (useful as baseline)
- **Median (P50)**: Typical performance
- **P95/P99**: SLA-relevant percentiles
- **Variance/Stddev**: Consistency of performance

## Performance Regression Detection

### Automated CI Integration

GitHub Actions workflow (`.github/workflows/benchmark.yml`) runs benchmarks on:
- Every commit to main branch
- Manual trigger option
- Pull requests (optional)

The workflow:
1. Runs full benchmark suite
2. Compares against baseline metrics
3. Flags regressions (> 10% deviation)
4. Posts results to PR comments
5. Generates HTML report with graphs

### Manual Regression Testing

```bash
# Establish baseline
git checkout main
rebar3 ct --suite=throughput_SUITE > baseline.txt

# Make changes
# ... implement feature ...

# Compare
rebar3 ct --suite=throughput_SUITE > after.txt
diff baseline.txt after.txt
```

### Investigation Process

When regression detected:

1. **Isolate the operation**
   ```bash
   rebar3 ct --suite=throughput_SUITE --case=health_check_baseline
   # Is it specific to health checks?
   ```

2. **Test at different loads**
   ```bash
   rebar3 ct --suite=throughput_SUITE --case=health_check_concurrent_10
   rebar3 ct --suite=throughput_SUITE --case=health_check_concurrent_1000
   # Load-dependent or constant?
   ```

3. **Profile the operation**
   - Use `rebar3 prof` for CPU profiling
   - Check `erlang:process_info/2` for memory
   - Review logs for GC events

4. **Root cause analysis**
   - Compare code changes
   - Check for new allocations
   - Verify algorithm complexity
   - Review lock contention

## Benchmark Output Format

All benchmark tests output metrics in this format:

```
========== BENCHMARK REPORT ==========
Test: health_check_concurrent_100
Count: 45000 operations
Min: 0.15 ms
Max: 45.23 ms
Avg: 1.87 ms
Median: 1.20 ms
P50: 1.20 ms
P95: 8.50 ms (target: 10.00 ms)
P99: 12.30 ms
StdDev: 2.15 ms
======================================
```

## Best Practices

### When Running Benchmarks

1. **Consistent system state**
   - Close other applications
   - No competing workloads
   - Same OS configuration
   - Same hardware if possible

2. **Multiple runs**
   - Run benchmarks 2-3 times
   - Ignore first run (warm cache)
   - Average results
   - Watch for variance

3. **Compare apples-to-apples**
   - Same branch/commit
   - Same Erlang/OTP version
   - Same hardware
   - Same rebar3 version

4. **Document changes**
   - Record baseline before changes
   - Note Erlang version
   - Document OS/hardware
   - Save results for trending

### Interpreting Variance

**Low variance (StdDev < 10% of Avg)**
- Stable, predictable performance
- Good for SLA guarantees

**High variance (StdDev > 20% of Avg)**
- Unstable performance
- Possible GC pauses, scheduling issues
- Investigate root cause

**Acceptable variance**
- < 10%: Excellent
- 10-20%: Good
- 20-50%: Acceptable
- > 50%: Investigate

## Performance Tuning Checklist

Before investigating a regression:

- [ ] Verified benchmark ran correctly (no errors)
- [ ] Multiple runs show consistent results
- [ ] System was idle during benchmark
- [ ] Same Erlang version as baseline
- [ ] No background processes running
- [ ] Checked for GC overhead
- [ ] Verified no lock contention
- [ ] Profiled with `rebar3 prof`

## Next Steps (Phase 2)

The benchmarking infrastructure is now in place. Phase 2 will add:

1. **Automated Performance Monitoring**
   - Real-time metrics collection
   - APM dashboard with Grafana
   - Historical trending (trend detection)

2. **Advanced Analysis**
   - Flame graphs for CPU profiling
   - Memory analysis tools
   - Lock contention detection
   - Scheduler activity analysis

3. **CI/CD Integration**
   - Automated regression detection
   - Performance gates in CI pipeline
   - Historical baseline storage
   - Automatic alerting on regressions

4. **Load Testing**
   - Sustained load patterns
   - Spike testing
   - Soak testing (24+ hour runs)
   - Chaos engineering scenarios

## References

- [Erlang Performance Tuning](https://erlang.org/doc/efficiency_guide/)
- [Common Test User Guide](https://erlang.org/doc/apps/common_test/)
- [Benchmarking Guide](https://www.erlang.org/doc/man/erlang.html#statistics-1)

---

Last Updated: 2026-01-26
