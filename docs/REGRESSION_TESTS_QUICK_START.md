# Performance Regression Tests - Quick Start Guide

## Overview

This guide helps you get started with performance regression testing for erlmcp OTP 28 optimizations.

## Prerequisites

- Erlang/OTP 28.3.1 or higher (required for priority messages, hibernate improvements)
- rebar3 build tool
- Git (for SHA tracking)

## Installation

The regression tests are included in erlmcp. No additional installation needed.

## Quick Start (5 minutes)

### 1. Establish Baseline

```bash
# Start Erlang shell with erlmcp
rebar3 shell

# In the Erlang shell
> erlmcp_bench_regression:establish_baseline().
```

This creates a baseline file with structure:
```
bench/baseline/baseline_otp28_<timestamp>.json
```

### 2. Run Regression Tests

```bash
# Using the helper script (recommended)
./scripts/run_regression_tests.sh

# Or using rebar3 directly
rebar3 ct --suite=erlmcp_regression_SUITE
```

### 3. View Results

Results are saved to:
- HTML report: `log/ct/index.html`
- JSON data: `log/ct/ct_run.*@*/`

## Usage Examples

### Run Specific Feature Tests

```erlang
%% Test JSON performance only
erlmcp_bench_regression:bench_native_json().
erlmcp_bench_regression:bench_jsx_json().

%% Test priority messages
erlmcp_bench_regression:bench_priority_messages().

%% Test hibernate improvements
erlmcp_bench_regression:bench_hibernate().

%% Test process iterator
erlmcp_bench_regression:bench_process_iterator().
```

### Run All Regression Checks

```erlang
%% Run complete regression suite
erlmcp_bench_regression:run_regression_checks().
```

### Generate Performance Report

```erlang
%% Establish baseline and generate report
{ok, Baseline} = erlmcp_bench_regression:establish_baseline(),

%% Generate human-readable report
Report = erlmcp_bench_regression:generate_report(Baseline),

%% Save report
file:write_file("performance_report.txt", Report).
```

## Expected Results

### OTP 28 Performance Improvements

| Feature | OTP 27 | OTP 28 | Improvement |
|---------|--------|--------|-------------|
| JSON decode (native) | 2.0M ops/s | 5.0M ops/s | **2.5x** |
| JSON encode (native) | 1.8M ops/s | 4.5M ops/s | **2.5x** |
| Priority messages | 15 us | 5 us | **3x faster** |
| Hibernate memory | 20 KB | 5 KB | **75% reduction** |
| Process iterator | 5000 us | 2500 us | **2x better** |

### Regression Thresholds

Tests fail if performance degrades by more than:
- **Throughput**: >10% decrease
- **Latency**: >10% increase
- **Memory**: >20% increase

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Performance Regression Tests

on: [push, pull_request]

jobs:
  performance:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Install Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: "28.3.1"
      
      - name: Fetch dependencies
        run: rebar3 get-deps
      
      - name: Compile
        run: rebar3 compile
      
      - name: Run regression tests
        run: ./scripts/run_regression_tests.sh
      
      - name: Upload results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: ct-results
          path: log/ct/
```

### GitLab CI Example

```yaml
performance_regression:
  image: erlang:28.3.1
  
  script:
    - rebar3 compile
    - ./scripts/run_regression_tests.sh
  
  artifacts:
    paths:
      - log/ct/
    when: always
```

## Troubleshooting

### Baseline Not Found

```
Error: no_baseline
Solution: Run erlmcp_bench_regression:establish_baseline() first
```

### Feature Not Available

```
Priority messages not available (OTP < 28)
Solution: Upgrade to OTP 28+
```

### Test Timeout

```
Test case timeout: 600s exceeded
Solution: 
- Increase timeout in suite/0 function
- Run on faster hardware
- Reduce system load
```

### False Positives

If tests fail but performance looks OK:

1. **Run multiple times**: Performance can vary ±5%
   ```erlang
   %% Run 5 times and average
   lists:map(fun(_) -> 
       erlmcp_bench_regression:bench_native_json() 
   end, lists:seq(1, 5)).
   ```

2. **Check system load**: High CPU/memory usage affects results
   ```bash
   top -n 1 | head -n 20
   ```

3. **Warm-up**: First run may be slower (JIT compilation)
   ```erlang
   %% Warm-up run
   erlmcp_bench_regression:bench_native_json(),
   
   %% Actual measurement
   erlmcp_bench_regression:bench_native_json().
   ```

## Advanced Usage

### Custom Baseline

```bash
# Use custom baseline file
./scripts/run_regression_tests.sh /path/to/custom_baseline.json
```

### Adjust Regression Thresholds

```erlang
%% In rebar.config
{ct_opts, [
    {logdir, "log/ct"}, 
    {cover_enabled, true},
    {var, regression_threshold, 15.0}  % 15% threshold
]}.
```

### Export Historical Data

```erlang
%% Load baseline history
{ok, Baseline1} = file:read_file("baseline_otp28_20260201.json"),
{ok, Baseline2} = file:read_file("baseline_otp28_20260202.json"),

%% Compare trends
Data1 = jsx:decode(Baseline1, [return_maps]),
Data2 = jsx:decode(Baseline2, [return_maps]),

%% Extract throughput
Throughput1 = maps:get(<<"decode_throughput">>, 
                   maps:get(<<"native_json">>, 
                            maps:get(<<"benchmarks">>, Data1))),
Throughput2 = maps:get(<<"decode_throughput">>, 
                   maps:get(<<"native_json">>, 
                            maps:get(<<"benchmarks">>, Data2))),

%% Calculate trend
Trend = ((Throughput2 - Throughput1) / Throughput1) * 100,
io:format("Trend: ~.2f%~n", [Trend]).
```

## Best Practices

1. **Run regularly**: Schedule regression tests daily/weekly
2. **Track trends**: Store historical baselines in git
3. **Use consistent hardware**: Same CPU, memory, OS
4. **Minimize system load**: Run during off-peak hours
5. **Document changes**: Note reason for baseline updates

## Further Reading

- [Full Documentation](PERFORMANCE_REGRESSION_TESTS.md)
- [OTP 28 Release Notes](https://www.erlang.org/doc/system_principles/system_principles.html)
- [Benchmarking Guide](bench/BENCHMARKS.md)

## Support

For issues or questions:
1. Check troubleshooting section above
2. Review full documentation
3. Check GitHub issues
4. Contact erlmcp maintainers

## License

Copyright (c) 2026 erlmcp contributors. See LICENSE file for details.
