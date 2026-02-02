# OTP 28 Performance Regression Tests - Implementation Summary

## Overview

Comprehensive performance regression testing system has been implemented for erlmcp OTP 28 optimizations. This system automates detection of performance regressions and ensures OTP 28 improvements are maintained.

## Files Created

### 1. Core Benchmark Module
**File**: `/Users/sac/erlmcp/bench/erlmcp_bench_regression.erl`

**Features**:
- Establish performance baselines
- Run regression checks
- Test 7 OTP 28 optimizations:
  - Native JSON encode/decode
  - Priority messages
  - Process hibernate
  - Process iterator
  - PCRE2 regex
  - Tagged monitors
  - Comparison with jsx baseline

**Key Functions**:
```erlang
establish_baseline()         -> Create baseline for current OTP
run_regression_checks()      -> Compare against baseline
bench_native_json()          -> Test native json:encode/decode
bench_priority_messages()    -> Test priority message latency
bench_hibernate()            -> Test hibernate memory reduction
bench_process_iterator()     -> Test process iterator vs erlang:processes()
bench_pcre2_regex()          -> Test PCRE2 regex performance
bench_tagged_monitors()      -> Test tagged monitor overhead
```

### 2. Common Test Suite
**File**: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_regression_SUITE.erl`

**Test Cases**:
- `test_baseline_establishment` - Verify baseline creation
- `test_json_performance_regression` - JSON regression detection
- `test_priority_messages_regression` - Priority message checks
- `test_hibernate_regression` - Hibernate memory checks
- `test_process_iterator_regression` - Process iterator checks
- `test_pcre2_regression` - PCRE2 performance checks
- `test_tagged_monitors_regression` - Tagged monitor checks
- `test_full_regression_suite` - Complete regression suite
- `test_performance_gates` - Enforce performance thresholds
- `test_baseline_comparison` - Baseline comparison tests

**Groups**:
- `otp28_features` - OTP 28 specific tests
- `baseline` - Baseline management
- `regression` - Regression detection

### 3. Documentation
**Files**:
- `/Users/sac/erlmcp/docs/PERFORMANCE_REGRESSION_TESTS.md` - Full documentation
- `/Users/sac/erlmcp/docs/REGRESSION_TESTS_QUICK_START.md` - Quick start guide
- `/Users/sac/erlmcp/docs/OTP28_REGRESSION_IMPLEMENTATION.md` - This file

### 4. Baseline Data
**File**: `/Users/sac/erlmcp/bench/baseline/otp28_baseline.json`

**Structure**:
```json
{
  "baseline_name": "OTP 28.3.1 Baseline",
  "metrics": {
    "native_json": {...},
    "jsx_json": {...},
    "priority_messages": {...},
    "hibernate": {...},
    "process_iterator": {...},
    "pcre2_regex": {...},
    "tagged_monitors": {...}
  },
  "thresholds": {...},
  "environment": {...}
}
```

### 5. Helper Scripts
**File**: `/Users/sac/erlmcp/scripts/run_regression_tests.sh`

**Features**:
- Automated baseline creation
- Regression test execution
- Color-coded output
- CI/CD friendly exit codes

## Usage

### Basic Workflow

1. **Establish Baseline** (first time or when intentionally improving):
   ```erlang
   erlmcp_bench_regression:establish_baseline().
   ```

2. **Run Regression Checks** (during development):
   ```bash
   ./scripts/run_regression_tests.sh
   ```

3. **View Results**:
   ```bash
   open log/ct/index.html
   ```

### Advanced Usage

```erlang
%% Test specific features
erlmcp_bench_regression:bench_native_json(),
erlmcp_bench_regression:bench_priority_messages(),
erlmcp_bench_regression:bench_hibernate().

%% Generate performance report
{ok, Baseline} = erlmcp_bench_regression:establish_baseline(),
Report = erlmcp_bench_regression:generate_report(Baseline).

%% Compare with baseline
Comparison = erlmcp_bench_regression:compare_with_baseline(Results).
```

## Performance Baselines

### Expected OTP 28 Improvements

| Feature | Metric | Baseline (OTP 27) | Target (OTP 28) | Improvement |
|---------|--------|-------------------|-----------------|-------------|
| JSON (native) | Decode throughput | 2.0M ops/s | 5.0M ops/s | 2.5x |
| JSON (native) | Encode throughput | 1.8M ops/s | 4.5M ops/s | 2.5x |
| Priority messages | Latency | 15 us | 5 us | 3x |
| Hibernate | Memory reduction | 25% | 75% | 3x |
| Process iterator | Memory usage | 5000 us | 2500 us | 2x |
| PCRE2 | Simple latency | 6.25 us | 5 us | 1.25x |
| Tagged monitors | Overhead | N/A | 20% | New feature |

### Regression Thresholds

Tests fail if performance degrades by more than:
- **Throughput**: >10% decrease
- **Latency**: >10% increase
- **Memory**: >20% increase
- **Monitor overhead**: >50%

## Integration

### With rebar3

```bash
# Run all regression tests
rebar3 ct --suite=erlmcp_regression_SUITE

# Run OTP 28 features group
rebar3 ct --suite=erlmcp_regression_SUITE --group=otp28_features

# Run with custom baseline
rebar3 ct --suite=erlmcp_regression_SUITE \
  --var=baseline_path=/path/to/baseline.json
```

### With CI/CD

The system is designed for CI/CD integration:

```yaml
# GitHub Actions
- name: Run Performance Regression Tests
  run: ./scripts/run_regression_tests.sh
  
- name: Upload Results
  if: always()
  uses: actions/upload-artifact@v3
  with:
    name: ct-results
    path: log/ct/
```

## Implementation Details

### Benchmark Methodology

1. **Warm-up**: Each benchmark runs warm-up iterations
2. **Measurement**: Multiple iterations for statistical significance
3. **Metrics**: Throughput, latency, memory usage
4. **Comparison**: Compare against established baseline
5. **Thresholds**: Configurable regression detection

### Baseline Format

Baselines are stored as JSON with:
- **Metadata**: OTP version, timestamp, git SHA
- **Benchmarks**: Individual feature results
- **Environment**: System configuration
- **Summary**: Aggregate metrics

### Regression Detection

```erlang
%% Calculate regression percentage
Regression = ((Current - Baseline) / Baseline) * 100

%% Determine status
Status = if
    Regression < -10 -> regression;      % Degradation
    Regression > 10 -> improvement;      % Enhancement
    true -> stable                       % Within threshold
end.
```

## Testing

### Running Tests

```bash
# Unit tests (EUnit)
rebar3 eunit

# Integration tests (Common Test)
rebar3 ct

# Regression tests
./scripts/run_regression_tests.sh
```

### Expected Output

```
=== OTP 28 Performance Regression Report ===

OTP Release: 28
ERTS Version: 13.0.1
Timestamp: 2026-02-02T12:00:00Z

SUMMARY
-------
JSON speedup (native vs jsx): 2.5x
Hibernate memory reduction: 75.0%
Priority message latency: 5.0 us

JSON Performance
  Decode throughput: 5.0M ops/sec
  Encode throughput: 4.5M ops/sec
  Status: improvement (2.5x faster than jsx)

Priority Messages
  Normal latency: 15.0 us
  Priority latency: 5.0 us
  Speedup: 3.0x
  Status: stable

Hibernate
  Normal memory: 20.0 KB/process
  Hibernate memory: 5.0 KB/process
  Reduction: 75.0%
  Status: improvement
```

## Maintenance

### Updating Baselines

When performance improvements are intentional:

1. Verify improvements
2. Update baseline file
3. Document reason
4. Commit to version control

### Adding New Features

1. Add benchmark function to `erlmcp_bench_regression`
2. Add test case to `erlmcp_regression_SUITE`
3. Update baseline structure
4. Document expected improvements
5. Set regression threshold

## Troubleshooting

### Common Issues

1. **Baseline not found**: Run `establish_baseline()` first
2. **Feature not available**: Upgrade to OTP 28+
3. **Test timeout**: Increase timeout in `suite/0`
4. **False positives**: Run multiple times, check system load

### Debug Mode

```erlang
%% Enable verbose logging
logger:set_primary_config(level, all),
erlmcp_bench_regression:establish_baseline().
```

### Performance Profiling

```erlang
%% Profile bottleneck
fprof:trace([start, {procs, [self()]}]),
erlmcp_bench_regression:bench_native_json(),
fprof:stop(),
fprof:profile(),
fprof:analyse([{dest, "profile.txt"}]).
```

## References

- [Erlang/OTP 28 Release Notes](https://www.erlang.org/doc/system_principles/system_principles.html)
- [JSON Module Documentation](https://www.erlang.org/doc/man/json.html)
- [Common Test User's Guide](https://www.erlang.org/doc/apps/common_test/users_guide.html)

## License

Copyright (c) 2026 erlmcp contributors. See LICENSE file for details.

## Summary

This implementation provides:
- ✅ Comprehensive OTP 28 feature testing
- ✅ Automated regression detection
- ✅ Baseline management
- ✅ CI/CD integration
- ✅ Performance gates
- ✅ Historical comparison
- ✅ Detailed documentation
- ✅ Helper scripts

All OTP 28 optimizations are now covered by automated regression tests with configurable thresholds and comprehensive reporting.
