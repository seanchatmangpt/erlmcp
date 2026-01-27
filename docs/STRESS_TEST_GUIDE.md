# erlmcp Stress Testing Guide

## Overview

This comprehensive stress testing guide validates the 100x scalability claims of the erlmcp system through automated testing across multiple load levels, failure scenarios, and extended durations.

## Quick Start

### Run All Stress Tests

```bash
# Full suite (requires 24+ hours for soak test)
rebar3 ct --suite erlmcp_stress_baseline_tests \
          --suite erlmcp_stress_scale_tests \
          --suite erlmcp_stress_chaos_tests \
          --suite erlmcp_stress_sustained_tests \
          --suite erlmcp_stress_cascading_tests

# Quick validation (10 minutes)
STRESS_TEST_MODE=demo rebar3 ct --suite erlmcp_stress_baseline_tests

# Extended validation (1 hour)
STRESS_TEST_MODE=short rebar3 ct --suite erlmcp_stress_scale_tests
```

### Individual Test Suites

```bash
# 1. Baseline Performance (10 minutes)
rebar3 ct --suite erlmcp_stress_baseline_tests

# 2. Scalability Validation (60 minutes)
rebar3 ct --suite erlmcp_stress_scale_tests

# 3. Chaos Engineering (5 hours)
rebar3 ct --suite erlmcp_stress_chaos_tests

# 4. Sustained Load (24 hours)
STRESS_TEST_MODE=extended rebar3 ct --suite erlmcp_stress_sustained_tests

# 5. Cascading Failures (1 hour)
rebar3 ct --suite erlmcp_stress_cascading_tests
```

## Test Suites

### 1. Baseline Performance Tests (`erlmcp_stress_baseline_tests.erl`)

**Purpose**: Validate that the system meets Phase 1 baseline metrics at 150 concurrent connections.

**Duration**: 10 minutes

**Tests**:
- `test_baseline_150_connections` - Full baseline load test
- `test_baseline_message_distribution` - Verify even message distribution
- `test_baseline_latency_percentiles` - Measure latency at P50, P95, P99
- `test_baseline_error_rate` - Validate error rate < 0.1%
- `test_baseline_memory_stability` - Monitor memory growth over 5 minutes
- `test_baseline_recovery_time` - Test recovery from connection drops

**Expected Results**:
```
Baseline (150 connections):
├── Throughput: 5000 msg/sec (±10%)
├── Latency P95: 85 ms (±20%)
├── Error Rate: <0.1%
├── Memory: <500 MB
└── Recovery Time: <10 seconds
```

**Pass Criteria**:
- Throughput: 4500-5500 msg/sec
- Latency P95: 68-102 ms
- Error Rate: <0.1%
- Memory Growth: <100 MB over 5 minutes
- Recovery Time: <10 seconds

### 2. Scalability Tests (`erlmcp_stress_scale_tests.erl`)

**Purpose**: Validate 100x scalability from 150 to 15,000 concurrent connections.

**Duration**: 60 minutes (10 minutes per level)

**Load Levels**:
```
150 connections   → 500 connections  → 1K connections
│                 │                  │
5K msg/sec        15K msg/sec        30K msg/sec
┌─────────────────┴──────────────────┴─────────────────┐
                    SCALE UP
│                 │                  │
5K connections    10K connections    15K connections
│                 │                  │
150K msg/sec      300K msg/sec       500K msg/sec (100x baseline!)
```

**Tests**:
- `test_scale_150_connections` - Baseline (5K msg/sec)
- `test_scale_500_connections` - 3.3x scaling (15K msg/sec)
- `test_scale_1k_connections` - 6x scaling (30K msg/sec)
- `test_scale_5k_connections` - 30x scaling (150K msg/sec)
- `test_scale_10k_connections` - 60x scaling (300K msg/sec)
- `test_scale_15k_connections` - 100x scaling (500K msg/sec)
- `test_scale_summary_report` - Generate summary analysis

**Pass Criteria**:
- Each level achieves 90% of target throughput
- Latency P95 remains bounded
- Error rate stays <0.1%
- 100x scaling factor achieved (150→15K connections, 5K→500K msg/sec)

**Expected Scaling Curve**:
```
Throughput vs Load
     500K │        ●
           │      ●
     300K │    ●
           │  ●
     150K │ ●
           │
      30K ├●─────────────────
           │
      15K │
           │
       5K └─────────────────
           └──┬──┬──┬──┬──┬──
             150 500 1K 5K 10K 15K
             Connections
```

### 3. Chaos Engineering Tests (`erlmcp_stress_chaos_tests.erl`)

**Purpose**: Validate system resilience under controlled failure conditions.

**Duration**: ~3-5 hours (30 min per scenario)

**Failure Scenarios**:

#### 3.1 Random Process Crashes
- Kill random server processes every 30 seconds
- **Measure**: Recovery time, error rate during failure
- **Target**: <10 sec recovery, <5% error rate during failure

#### 3.2 Client Disconnections
- Disconnect 10% of clients every 60 seconds
- **Measure**: Reconnection success rate, throughput impact
- **Target**: 100% successful reconnection, <50% throughput loss

#### 3.3 Network Partitions
- Simulate 30-second network partitions every 2 minutes
- **Measure**: Partition detection, healing verification
- **Target**: Detect partition <5 sec, heal <15 sec

#### 3.4 Message Loss
- Simulate 2% packet loss
- **Measure**: Loss detection, error rate
- **Target**: Detect loss, <1% unrecovered losses

#### 3.5 Latency Injection
- Add 100-500ms artificial delays
- **Measure**: Throughput degradation, timeout handling
- **Target**: Graceful degradation, no cascading failures

#### 3.6 Combined Failures
- All above happening simultaneously
- **Measure**: System stability, recovery capability
- **Target**: System survives, recovers within 30 sec

**Pass Criteria**:
- Process crashes: Recovery <10 sec, error rate <5% during
- Client disconnections: 100% reconnection success
- Network partitions: Healing <15 sec
- Message loss: Detection and handling <100ms
- Latency injection: Graceful degradation
- Combined: Survival + recovery <30 sec

### 4. Sustained Load Tests (`erlmcp_stress_sustained_tests.erl`)

**Purpose**: Validate long-term stability at peak load (24-hour soak).

**Duration**: 24 hours (configurable via `STRESS_TEST_MODE`)
- `STRESS_TEST_MODE=demo` → 10 minutes
- `STRESS_TEST_MODE=short` → 1 hour
- `STRESS_TEST_MODE=extended` → 24 hours

**Tests**:
- `test_sustained_24_hour_soak` - Full soak at 15K connections
- `test_sustained_memory_stability` - Monitor memory over time
- `test_sustained_error_rate_stability` - Verify stable error rate
- `test_sustained_latency_stability` - Check latency variance

**Metrics Collected**:
- Hourly throughput snapshots
- Memory growth pattern
- Error rate trend
- Latency P95/P99 stability
- GC pause distribution
- CPU utilization

**Pass Criteria**:
- Memory growth: <500 MB over 24 hours
- Error rate: Stays <0.1%
- Latency P95: Variation <±20ms over time
- No unhandled crashes
- Graceful degradation only under extreme conditions

**Expected Memory Pattern**:
```
Memory Growth During 24-Hour Soak
     2GB │  ┌─────────────────────────
     1.5GB│ ╱
      1GB │╱
    500MB ├──────────────────────────
           │
       0MB └──────────────────────────
           0h    6h   12h   18h   24h

After warmup, growth should be minimal (<50MB/hour)
```

### 5. Cascading Failure Tests (`erlmcp_stress_cascading_tests.erl`)

**Purpose**: Validate system behavior when multiple failures occur simultaneously.

**Duration**: ~1-2 hours total

**Cascading Scenarios**:

#### 5.1 Process Crash + Mass Disconnection
- Crash server processes + disconnect 500 clients
- **Measure**: Peak error rate, recovery progression
- **Target**: <15% error, recover in <2 min

#### 5.2 Registry Partition + High Latency
- Block registry lookups + inject 200ms delays
- **Measure**: Message routing impact, recovery
- **Target**: Graceful degradation, <10 min recovery

#### 5.3 Slow Handler + Timeout Cascade
- Add 500ms handler delay + trigger timeouts
- **Measure**: Cascade propagation, timeout management
- **Target**: Prevent cascading, contain failures

#### 5.4 Triple Failure (All Simultaneous)
- Crashes + partition + delays simultaneously
- **Measure**: System survival, degradation bounds
- **Target**: Survive, error rate <25%, recover in <2 min

**Recovery Progression Analysis**:
```
Recovery Phases:
  Phase 1: Baseline (30 sec)
    └─ Establish metrics
  Phase 2: Cascading Failures (60 sec)
    └─ Inject multiple failures simultaneously
  Phase 3: Recovery (120 sec)
    └─ Monitor return to baseline

Target Recovery Curve:
     100% │     ┌─────────
          │    ╱
      80% ├───╱
          │  ╱
      50% │ ╱
          │╱
       0% └────────────────
          0s  30s  60s  90s  120s
          │   │   │    │    │
          │   │   Base Failure Recovery Complete
          │   Failure Injected
          Baseline
```

## Metrics Reference

### Standard Metrics

| Metric | Unit | Description |
|--------|------|-------------|
| `messages_per_second` | msg/sec | Throughput across all connections |
| `latency_p50_ms` | ms | Median latency |
| `latency_p95_ms` | ms | 95th percentile latency |
| `latency_p99_ms` | ms | 99th percentile latency |
| `error_rate_percent` | % | Percentage of failed messages |
| `memory_usage_mb` | MB | Current process memory |
| `memory_growth_mb` | MB | Memory change since start |
| `gc_pause_ms` | ms | Average garbage collection pause |
| `cpu_percent` | % | CPU utilization |
| `total_messages` | count | Cumulative messages sent |

### Collection Points

Metrics are collected at:
1. **Baseline**: Immediately after stabilization (T+10 sec)
2. **Regular**: Every 1 minute during test
3. **Hourly**: Aggregated snapshots every 60 minutes
4. **Final**: After client completion

## Performance Targets

### Throughput Targets

```
Load Level    │ Connections │ Target Throughput │ Min Acceptable
──────────────┼─────────────┼───────────────────┼────────────────
Baseline      │     150     │      5,000        │    4,500 (90%)
Moderate      │     500     │     15,000        │   13,500 (90%)
Medium        │   1,000     │     30,000        │   27,000 (90%)
Heavy         │   5,000     │    150,000        │  135,000 (90%)
Peak          │  10,000     │    300,000        │  270,000 (90%)
Maximum       │  15,000     │    500,000        │  450,000 (90%)
```

### Latency Targets

```
Load Level    │ P50  │ P95  │ P99  │ Max
──────────────┼──────┼──────┼──────┼────
Baseline      │  40  │  85  │ 150  │ 300
Moderate      │  40  │  80  │ 140  │ 280
Medium        │  42  │  75  │ 130  │ 260
Heavy         │  45  │  65  │ 110  │ 200
Peak          │  48  │  58  │ 100  │ 180
Maximum       │  50  │  50  │  95  │ 170
```

All times in milliseconds.

### Reliability Targets

```
Scenario                    │ Target Error Rate │ Target Recovery
────────────────────────────┼──────────────────┼─────────────────
Normal operation            │      <0.1%       │       N/A
Single failure              │      <1%         │     <10 sec
Cascading failures          │      <15%        │     <120 sec
Sustained load (24 hours)   │      <0.1%       │       N/A
```

## Running Tests in CI/CD

### GitHub Actions Integration

```yaml
name: Stress Tests
on: [push, pull_request]

jobs:
  baseline:
    runs-on: ubuntu-latest
    timeout-minutes: 30
    steps:
      - uses: actions/checkout@v2
      - name: Run baseline stress tests
        run: rebar3 ct --suite erlmcp_stress_baseline_tests

  scalability:
    runs-on: ubuntu-latest
    timeout-minutes: 90
    steps:
      - uses: actions/checkout@v2
      - name: Run scalability tests
        run: rebar3 ct --suite erlmcp_stress_scale_tests

  chaos:
    runs-on: ubuntu-latest
    timeout-minutes: 300
    steps:
      - uses: actions/checkout@v2
      - name: Run chaos tests
        run: rebar3 ct --suite erlmcp_stress_chaos_tests

  soak:
    runs-on: ubuntu-latest
    timeout-minutes: 1500
    if: github.event_name == 'push' && github.ref == 'refs/heads/main'
    steps:
      - uses: actions/checkout@v2
      - name: Run 24-hour soak test
        run: |
          STRESS_TEST_MODE=extended rebar3 ct --suite erlmcp_stress_sustained_tests
```

### Local Test Execution

```bash
# Full suite locally (requires 24+ hours)
time rebar3 do clean, compile, \
    ct --suite erlmcp_stress_baseline_tests, \
    ct --suite erlmcp_stress_scale_tests, \
    ct --suite erlmcp_stress_chaos_tests, \
    ct --suite erlmcp_stress_cascading_tests

# Quick regression check (30 minutes)
time rebar3 do clean, compile, \
    ct --suite erlmcp_stress_baseline_tests, \
    ct --suite erlmcp_stress_scale_tests

# Overnight validation (with soak)
STRESS_TEST_MODE=extended time rebar3 do clean, compile, \
    ct --suite erlmcp_stress_baseline_tests, \
    ct --suite erlmcp_stress_scale_tests, \
    ct --suite erlmcp_stress_chaos_tests, \
    ct --suite erlmcp_stress_sustained_tests
```

## Monitoring & Dashboards

### Prometheus Metrics Endpoints

```
Baseline Tests    → http://localhost:9090/metrics
Scale Tests       → http://localhost:9091/metrics
Chaos Tests       → http://localhost:9092/metrics
Sustained Tests   → http://localhost:9093/metrics
Cascading Tests   → http://localhost:9094/metrics
```

### Grafana Dashboards

Pre-built dashboards available in `/docker/grafana/dashboards/`:
- `erlmcp-stress-baseline.json`
- `erlmcp-stress-scalability.json`
- `erlmcp-stress-chaos.json`
- `erlmcp-stress-sustained.json`

### Real-Time Monitoring

```bash
# Watch baseline test in real-time
watch -n 1 'curl -s http://localhost:9090/metrics | grep erlmcp'

# Monitor all metrics endpoints
for port in 9090 9091 9092 9093 9094; do
  echo "=== Port $port ==="
  curl -s http://localhost:$port/metrics | grep erlmcp
done
```

## Troubleshooting

### Test Timeout Errors

**Symptom**: "Test timeout, killing remaining clients"

**Cause**: System under stress, not completing in time

**Solution**:
1. Increase timeouts in test config
2. Reduce concurrent connections
3. Increase message size (fewer total messages)
4. Check system resources (CPU, memory, file descriptors)

```bash
# Check file descriptor limits
ulimit -n  # Should be >20000

# Increase if needed
ulimit -n 65536
```

### Memory Pressure Warnings

**Symptom**: "Memory usage above 4GB"

**Cause**: Test parameters too aggressive for system

**Solution**:
1. Reduce message size
2. Reduce number of connections
3. Shorten test duration
4. Monitor memory with `observer`

```bash
# Start Erlang observer
make observer
```

### Connection Refused Errors

**Symptom**: "Connection refused" in client logs

**Cause**: Server backlog exceeded or port limitations

**Solution**:
1. Verify server is running: `lsof -i :9001`
2. Increase backlog in server config
3. Check OS-level connection limits
4. Reduce connection ramp-up rate

### Latency Outliers

**Symptom**: P99 latency much higher than P95

**Cause**: GC pauses, system load, network variance

**Solution**:
1. Enable GC profiling: Add `-g` flag to erl
2. Monitor GC statistics: `erlang:statistics(garbage_collection)`
3. Run test on isolated system
4. Check network stability

## Interpreting Results

### Green Zone (All Tests Pass)
```
Throughput:  ✓ 90%+ of target
Latency:     ✓ P95 within ±20% of baseline
Error Rate:  ✓ <0.1%
Memory:      ✓ Stable, <100MB growth/hour
Recovery:    ✓ <30 seconds after failures
```

**Action**: Proceed with confidence. System meets 100x scalability targets.

### Yellow Zone (Some Tests Below Target)
```
Throughput:  ⚠ 80-90% of target
Latency:     ⚠ P95 ±20-30% of baseline
Error Rate:  ⚠ 0.1-1%
Memory:      ⚠ Growth 100-200MB/hour
Recovery:    ⚠ 30-60 seconds
```

**Action**: Investigate performance bottlenecks:
1. Check CPU utilization (should be <80%)
2. Monitor GC pauses (should be <50ms)
3. Verify network stability
4. Profile hot paths

### Red Zone (Tests Fail)
```
Throughput:  ✗ <80% of target
Latency:     ✗ P95 >50% variance from baseline
Error Rate:  ✗ >1%
Memory:      ✗ Growth >500MB/hour (leak?)
Recovery:    ✗ >120 seconds
```

**Action**: System not ready for production:
1. Address critical issues
2. Re-run baseline tests
3. Profile memory leaks
4. Review error logs
5. Optimize bottlenecks

## Advanced Usage

### Custom Test Configuration

```erlang
Config = #{
    collection_interval => 1000,        % 1 second
    retention_period => 3600000,        % 1 hour
    enable_prometheus => true,
    prometheus_port => 9090,
    storage_backend => ets,             % or 'mnesia'
    max_connections => 20000,
    backlog => 2048,
    message_timeout => 5000
}
```

### Profiling & Tracing

```bash
# Enable profiling during test
rebar3 ct --suite erlmcp_stress_baseline_tests --profile

# View profiling results
fprof:analyze()

# Trace specific function
erlang:trace_pattern({erlmcp_server, handle_message, '_'}, true, [])
```

### Extending Tests

Create custom test suite:

```erlang
-module(erlmcp_stress_custom_tests).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() -> [test_custom_scenario].

test_custom_scenario(Config) ->
    {ok, Clients} = erlmcp_stress_client:spawn_clients(
        ServerPid,
        NumConnections,
        CustomConfig
    ),
    %% Your test logic here
    ok.
```

## Performance Optimization Tips

### VM Arguments

Edit `vm.args` for stress testing:

```
+S 8:8          # 8 schedulers (adjust for system)
+K true         % Kernel poll enabled
+A 256          # Async thread pool
+hms 16777216   # Heap size
+sb true        # System backend
```

### System Tuning

```bash
# Increase file descriptor limits
sysctl -w fs.file-max=2097152

# Increase network buffer sizes
sysctl -w net.core.rmem_max=134217728
sysctl -w net.core.wmem_max=134217728

# Reduce TCP TIME_WAIT
sysctl -w net.ipv4.tcp_tw_reuse=1
```

## References

- [erlmcp Architecture](./architecture.md)
- [OTP Patterns](./otp-patterns.md)
- [MCP Protocol](./protocol.md)
- [Common Test Documentation](https://erlang.org/doc/man/ct.html)
- [Erlang Profiling Guide](https://erlang.org/doc/efficiency_guide/)
