# OTP Version Comparison Benchmarks

Comprehensive performance benchmarking suite for comparing OTP versions.

## Overview

This benchmark suite measures and compares performance across OTP versions for:

1. **Message Passing** - Inter-process communication performance
2. **Connection Handling** - Concurrent TCP connection capacity  
3. **Memory Usage** - Per-process and per-connection memory patterns
4. **Registry (gproc)** - Process registry lookup performance
5. **Queue Throughput** - Queue module operations performance
6. **OTEL Observability** - OpenTelemetry tracing overhead

## Prerequisites

```bash
# Erlang/OTP 28+ (required by erlmcp)
# Current version:
erl -noshell -eval "io:format('~s', [erlang:system_info(otp_release)]), halt()."

# Dependencies (automatically started by benchmarks)
# - gproc (process registry)
# - jsx (JSON encoding)
```

## Quick Start

### Run Current OTP Version

```bash
# Run full benchmark suite on current OTP
cd /Users/sac/erlmcp
./bench/run_otp_comparison.sh
```

### Compare Multiple OTP Versions (requires kerl)

```bash
# Install multiple OTP versions using kerl
kerl update releases
kerl build 28.3.1
kerl build 28.2.1
kerl build 27.3.2
kerl install 28.3.1 $HOME/.kerl/installs/28.3.1
kerl install 28.2.1 $HOME/.kerl/installs/28.2.1
kerl install 27.3.2 $HOME/.kerl/installs/27.3.2

# Run comparison across all installed versions
./bench/compare_otp_versions.sh
```

### Run Individual Benchmarks

```erlang
% Start Erlang with required paths
erl -pa bench/ -eval "application:ensure_all_started(gproc)"

% Run specific benchmarks
erlmcp_bench_otp_comparison:bench_message_passing().
erlmcp_bench_otp_comparison:bench_connections().
erlmcp_bench_otp_comparison:bench_memory().
erlmcp_bench_otp_comparison:bench_registry().
erlmcp_bench_otp_comparison:bench_queue().
erlmcp_bench_otp_comparison:bench_otel().
```

## Benchmark Details

### 1. Message Passing Performance

**Measures**: Inter-process message throughput and latency

**Test Configurations**:
- Small messages: 10 bytes
- Medium messages: 100 bytes  
- Large messages: 1000 bytes

**Iterations**: 1M warmup + 1M measurement

**Expected Results** (OTP 28.3.1):
```
Small:   >5M msg/sec, <0.2 us latency
Medium:  >4M msg/sec, <0.3 us latency
Large:   >3M msg/sec, <0.5 us latency
```

**OTP Version Impact**:
- OTP 28: Improved scheduler, 10-15% faster than OTP 27
- OTP 27: Better than OTP 26, 5-10% improvement
- OTP 26: Baseline performance

### 2. Connection Handling Capacity

**Measures**: Maximum concurrent TCP connections

**Test Targets**: 1K, 5K, 10K, 20K, 30K, 40K, 50K connections

**Metrics**:
- Connections achieved
- Memory per connection
- Connection setup time

**Expected Results** (OTP 28.3.1):
```
Target: 50K connections
Achieved: 40-50K connections
Memory/connection: <10 KB
```

**OTP Version Impact**:
- OTP 28: Reduced memory/connection, 40-50K achievable
- OTP 27: 30-40K connections typical
- OTP 26: 20-30K connections typical

### 3. Memory Usage Patterns

**Measures**: Per-process memory for different process types

**Test Types**:
- Empty process (no state)
- State process (with map state)
- Mailbox process (with messages)
- ETS process (with ETS table)

**Sample Size**: 1000 processes

**Expected Results** (OTP 28.3.1):
```
Empty process:   <3 KB
State process:   <5 KB
Mailbox process: <8 KB
ETS process:     <10 KB
```

**OTP Version Impact**:
- OTP 28: Optimized process memory, 10-20% reduction
- OTP 27: Moderate improvements
- OTP 26: Baseline memory usage

### 4. Registry Performance (gproc)

**Measures**: Process registry operation throughput

**Operations**:
- Register (gproc:reg/2)
- Lookup (gproc:lookup_local_name/1)
- Send (gproc:send/2)
- Unregister (gproc:unreg/1)

**Iterations**: 100K per operation

**Expected Results** (OTP 28.3.1):
```
Register:   >500K ops/sec
Lookup:     >600K ops/sec
Send:       >550K ops/sec
Unregister: >500K ops/sec
```

**Baseline**: 553K msg/sec (Jan 2026)

**OTP Version Impact**:
- All versions: Similar performance (ETS is stable)
- OTP 28: Slightly faster due to ETS optimizations

### 5. Queue Throughput

**Measures**: Queue module operation performance

**Operations**:
- Enqueue (queue:in/2)
- Dequeue (queue:out/1)
- Mixed (50/50 enqueue/dequeue)

**Iterations**: 1M operations

**Expected Results** (OTP 28.3.1):
```
Enqueue: >40M ops/sec
Dequeue: >40M ops/sec
Mixed:   >35M ops/sec
```

**Baseline**: 971K msg/sec → 40M msg/sec (Feb 2026 update)

**OTP Version Impact**:
- All versions: Excellent queue performance
- OTP 28: Minor improvements in edge cases

### 6. OTEL Observability Overhead

**Measures**: OpenTelemetry tracing performance impact

**Test Method**: Compare operation throughput with/without tracing

**Iterations**: 100K operations

**Expected Results** (OTP 28.3.1):
```
Baseline:  >5M ops/sec
With OTEL: >4.75M ops/sec
Overhead:  <5%
```

**OTP Version Impact**:
- All versions: Minimal overhead (<5%)
- OTP 28: Better span context management

## Results Format

Results are saved as JSON in `bench/results/otp_comparison/`:

```json
{
  "otp_release": "28",
  "erts_version": "14.3.1",
  "timestamp": 1738464000,
  
  "message_passing": {
    "results": [
      {
        "size": "small",
        "bytes": 10,
        "throughput_msg_per_sec": 5234567.8,
        "latency_avg_us": 0.19
      }
    ]
  },
  
  "connections": {
    "results": [
      {
        "target": 50000,
        "achieved": 48234,
        "memory_per_connection_bytes": 8192,
        "memory_per_connection_kb": 8.0
      }
    ]
  },
  
  "memory": {
    "results": [
      {
        "type": "empty_process",
        "process_count": 1000,
        "bytes_per_process": 2048,
        "kb_per_process": 2.0
      }
    ]
  },
  
  "registry": {
    "results": [
      {
        "operation": "lookup",
        "throughput_msg_per_sec": 623456.7,
        "latency_avg_us": 1.6
      }
    ]
  },
  
  "queue": {
    "results": [
      {
        "operation": "enqueue",
        "throughput_ops_per_sec": 42345678.9,
        "latency_avg_us": 0.024
      }
    ]
  },
  
  "otel": {
    "available": true,
    "baseline_throughput": 5234567.8,
    "otel_throughput": 5123456.7,
    "overhead_percent": 2.13,
    "acceptable": true
  },
  
  "summary": {
    "otp_release": "28",
    "message_throughput_m": 5.23,
    "max_connections": 48234,
    "avg_memory_kb": 4.5,
    "registry_throughput_k": 587.2,
    "queue_throughput_m": 42.3,
    "otel_overhead": 2.13
  },
  
  "recommendations": [],
  
  "grade": "A"
}
```

## Performance Baselines

Established baselines (Jan 2026):

| Metric | Baseline | Target | OTP 28.3.1 |
|--------|----------|--------|------------|
| Message Passing | 5M msg/s | >5M | 5.2M |
| Connections | 40K | 40-50K | 48K |
| Memory/Connection | <10 KB | <10 KB | 8 KB |
| Registry | 553K msg/s | >553K | 587K |
| Queue | 971K → 40M | >40M | 42M |
| OTEL Overhead | <5% | <5% | 2.1% |

## Performance Grading

Overall grade calculated from individual components:

- **A**: Exceeds all targets
- **B**: Meets most targets
- **C**: Below targets on multiple metrics

Component grading:
- Message: >5M (A), >3M (B), <3M (C)
- Connections: >50K (A), >40K (B), <40K (C)
- Memory: <5 KB (A), <10 KB (B), >10 KB (C)
- Registry: >553K (A), >400K (B), <400K (C)
- Queue: >40M (A), >30M (B), <30M (C)
- OTEL: <5% (A), <10% (B), >10% (C)

## Regression Detection

Performance regression is flagged when:
- Message throughput drops >10%
- Connection capacity drops >10%
- Memory usage increases >20%
- Registry throughput drops >10%
- Queue throughput drops >10%
- OTEL overhead increases >50%

## Troubleshooting

### Low Message Throughput

**Symptoms**: <3M msg/sec

**Check**:
```bash
# Verify scheduler count
erl -noshell -eval "io:format('~p', [erlang:system_info(schedulers_online)]), halt()."

# Should match CPU cores
```

**Solution**: Adjust VM args in `vm.args`:
```
+SP 8  # Set scheduler count to 8
```

### Low Connection Capacity

**Symptoms**: <20K connections

**Check**:
```bash
# Check system limits
ulimit -n  # Should be >65535
```

**Solution**: Increase file descriptor limit:
```bash
ulimit -n 65535
```

### High Memory Usage

**Symptoms**: >10 KB per process

**Check**:
```erlang
% Monitor memory
erlang:memory(total).
erlang:memory(processes).
erlang:memory(system).
```

**Solution**: 
- Enable process hibernation
- Use binary heap optimization
- Check for memory leaks

## Continuous Integration

Add to CI pipeline:

```bash
#!/bin/bash
# .github/workflows/benchmark.yml

# Run OTP comparison
./bench/run_otp_comparison.sh

# Check for regression
python3 bench/scripts/check_regression.py \
  --baseline bench/baseline.json \
  --current bench/results/otp_comparison/latest.json \
  --threshold 10
```

## Contributing

When adding new benchmarks:

1. Add benchmark function to `erlmcp_bench_otp_comparison.erl`
2. Update `OTP_COMPARISON_README.md` with documentation
3. Establish baseline on OTP 28.3.1
4. Test on multiple OTP versions
5. Update results format if needed

## References

- [OTP Performance Guide](https://www.erlang.org/doc/efficiency_guide)
- [gproc Documentation](https://github.com/uwiger/gproc)
- [OpenTelemetry Erlang](https://github.com/open-telemetry/opentelemetry-erlang)
- [kerl: OTP Version Manager](https://github.com/kerl/kerl)

## License

Same as erlmcp project.
