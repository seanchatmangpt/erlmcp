# Stress/Sustained Load Benchmarks

## Overview

The `erlmcp_bench_stress` module provides comprehensive stress testing with:

- **Time-series performance monitoring** (5-second sampling intervals)
- **Degradation detection** (throughput, memory, latency trends)
- **Memory leak detection** (linear regression analysis)
- **Early termination** on resource limits
- **Metrology-compliant output** (precision, scope, units specified)

## Workloads

### Quick Stress (30 seconds)
```erlang
erlmcp_bench_stress:quick_stress().
```
- **Duration**: 30 seconds
- **Target**: 100,000 ops/s
- **Workers**: 100
- **Use**: Quick validation, CI/CD integration

### Standard Stress (5 minutes)
```erlang
erlmcp_bench_stress:standard_stress().
```
- **Duration**: 5 minutes
- **Target**: 100,000 ops/s
- **Workers**: 100
- **Use**: Standard performance validation

### Endurance Stress (1 hour)
```erlang
erlmcp_bench_stress:endurance_stress().
```
- **Duration**: 1 hour
- **Target**: 50,000 ops/s
- **Workers**: 50
- **Use**: Memory leak detection, sustained load

### Production Simulation (24 hours)
```erlang
Workload = lists:nth(4, erlmcp_bench_stress:workloads()),
erlmcp_bench_stress:run_workload(Workload).
```
- **Duration**: 24 hours
- **Target**: 10,000 ops/s
- **Workers**: 10
- **Use**: Production readiness, long-term stability

## Output Format

### Metrology-Compliant JSON

```json
{
  "workload_id": "stress_5min_100k_ops",
  "benchmark": "stress",
  "duration_s": 300,
  "target_ops_per_s": 100000,
  "actual_ops_total": 30000000,
  "actual_throughput_avg": 100000.0,
  "throughput_std_dev": 2500.0,
  "latency_p99_avg_us": 150.0,
  "latency_p99_max_us": 350.0,
  "memory_start_mib": 100.0,
  "memory_end_mib": 102.0,
  "memory_leak_detected": false,
  "degradation_detected": false,
  "samples": [
    {
      "t": 0,           // Time (seconds)
      "ops": 0,         // Operations completed
      "tput": 100000,   // Throughput (ops/s)
      "p99": 145,       // Latency p99 (microseconds)
      "mem": 100.0,     // Memory usage (MiB)
      "cpu": 45.2       // CPU utilization (%)
    },
    // ... more samples every 5 seconds
  ],
  "scope": "per_node",
  "precision": "microsecond"
}
```

## Degradation Detection

### Throughput Degradation
- **Threshold**: > 5% decline per minute
- **Method**: Linear regression on throughput samples
- **Action**: Set `degradation_detected: true`

### Memory Leak Detection
- **Threshold**: > 1 MiB growth per minute
- **Method**: Linear regression on memory samples
- **Action**: Set `memory_leak_detected: true`

### Latency Growth
- **Threshold**: > 10% growth per minute
- **Method**: Linear regression on p99 latency samples
- **Future Enhancement**: Add to degradation detection

## Early Termination Conditions

### Memory Limit
- **Threshold**: 90% of available memory (default: 3.6 GiB of 4 GiB)
- **Action**: Stop workers, generate report with current samples
- **Reason**: `{terminate, memory_limit}`

### CPU Limit
- **Threshold**: 95% CPU sustained for 30 seconds
- **Action**: Stop workers, generate report
- **Reason**: `{terminate, cpu_limit}`

### Crash Detection
- **Action**: Capture crash dump, include in report
- **Recovery**: Supervisor restarts failed workers

## Integration with Profiling Suite

When `erlmcp_profiling_suite` is available, stress benchmarks automatically:

1. Start full profiling before workload execution
2. Collect CPU, memory, latency, bottleneck data
3. Stop profiling after workload completion
4. Include profiling report in output

## Usage Examples

### Run All Workloads
```erlang
Results = erlmcp_bench_stress:run_all().
```

### Run Custom Workload
```erlang
Workload = #{
    id => <<"custom_10min_50k">>,
    duration_s => 600,
    target_ops_per_s => 50000,
    workers => 50
},
Result = erlmcp_bench_stress:run_workload(Workload).
```

### Analyze Time-Series Data
```erlang
Result = erlmcp_bench_stress:standard_stress(),
Samples = maps:get(<<"samples">>, Result),

%% Plot throughput over time
lists:foreach(fun(S) ->
    Time = maps:get(<<"t">>, S),
    Throughput = maps:get(<<"tput">>, S),
    io:format("~p,~p~n", [Time, Throughput])
end, Samples).
```

### Export to CSV
```erlang
Result = erlmcp_bench_stress:quick_stress(),
Samples = maps:get(<<"samples">>, Result),

{ok, File} = file:open("stress_results.csv", [write]),
io:format(File, "time,ops,throughput,p99,memory,cpu~n", []),
lists:foreach(fun(S) ->
    io:format(File, "~p,~p,~p,~p,~.1f,~.1f~n", [
        maps:get(<<"t">>, S),
        maps:get(<<"ops">>, S),
        maps:get(<<"tput">>, S),
        maps:get(<<"p99">>, S),
        maps:get(<<"mem">>, S),
        maps:get(<<"cpu">>, S)
    ])
end, Samples),
file:close(File).
```

## Performance Characteristics

### Resource Usage

| Workload | Workers | Memory | CPU | Network |
|----------|---------|--------|-----|---------|
| Quick (30s) | 100 | ~50 MiB | 40-60% | Minimal |
| Standard (5min) | 100 | ~100 MiB | 40-60% | Minimal |
| Endurance (1hr) | 50 | ~75 MiB | 30-50% | Minimal |
| Production (24hr) | 10 | ~40 MiB | 10-20% | Minimal |

### Expected Results (Baseline Hardware)

**Hardware**: 4-core CPU, 8GB RAM, SSD

| Metric | Quick | Standard | Endurance | Production |
|--------|-------|----------|-----------|------------|
| Total Ops | 3M | 30M | 180M | 864M |
| Avg Throughput | 100k/s | 100k/s | 50k/s | 10k/s |
| P99 Latency | <200μs | <250μs | <300μs | <150μs |
| Memory Growth | <5 MiB | <20 MiB | <50 MiB | <100 MiB |
| Degradation | None | None | None | None |

## Troubleshooting

### Low Throughput
- **Symptom**: Actual throughput << target
- **Causes**: CPU saturation, slow operations, contention
- **Fix**: Reduce workers, optimize operations, profile bottlenecks

### Memory Leak Detected
- **Symptom**: `memory_leak_detected: true`
- **Causes**: Unbounded message queues, ETS leaks, binary leaks
- **Fix**: Use `recon:bin_leak(10)`, check process memory

### Degradation Detected
- **Symptom**: `degradation_detected: true`
- **Causes**: GC pressure, scheduler imbalance, lock contention
- **Fix**: Tune VM flags, reduce allocation rate, parallelize

### Early Termination
- **Symptom**: Benchmark stops before duration
- **Causes**: Resource limits exceeded
- **Fix**: Increase limits, reduce load, optimize code

## Implementation Details

### Worker Architecture
- Each worker is a separate Erlang process
- Workers execute operations at target rate (rate limiting)
- Latencies reported via message passing to parent
- Workers respond to `collect_stats` for aggregate counting

### Sampling Strategy
- Metrics collected every 5 seconds
- Worker stats aggregated on-demand
- Latencies buffered (last 1000 samples for percentiles)
- Time-series stored as list of maps

### Statistical Methods

**Linear Regression** (slope calculation):
```
slope = (N * ΣXY - ΣX * ΣY) / (N * ΣX² - (ΣX)²)
```

**Percentile** (sorted list):
```
index = ceil((percentile / 100) * length)
value = sorted[index]
```

**Standard Deviation**:
```
variance = Σ(X - mean)² / N
std_dev = √variance
```

## Future Enhancements

1. **Distributed stress testing** - Coordinate across multiple nodes
2. **Custom operation hooks** - Replace simulated ops with real workloads
3. **Real-time visualization** - LiveView dashboard for monitoring
4. **Automated regression detection** - Compare against baseline
5. **Resource profiling** - Detailed CPU, memory, I/O breakdown
6. **Failure injection** - Chaos engineering during stress test

## Related Modules

- `erlmcp_profiling_suite` - Comprehensive profiling integration
- `erlmcp_latency_profiler` - Detailed latency analysis
- `erlmcp_memory_profiler` - Memory allocation tracking
- `erlmcp_cpu_profiler` - CPU usage and hot spots
- `erlmcp_bottleneck_detector` - Automatic bottleneck identification

## References

- [Linear Regression](https://en.wikipedia.org/wiki/Linear_regression)
- [Percentile](https://en.wikipedia.org/wiki/Percentile)
- [Erlang Scheduler Utilization](https://www.erlang.org/doc/man/erlang.html#statistics-1)
- [Erlang Memory](https://www.erlang.org/doc/man/erlang.html#memory-0)
