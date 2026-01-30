---
name: Performance Issue
about: Report performance problems or regressions
title: '[PERF] '
labels: performance, benchmark
assignees: ''

---

## Performance Issue
What performance problem are you experiencing?

- [ ] Throughput degradation
- [ ] High latency
- [ ] Memory leak
- [ ] CPU spike
- [ ] Connection pool exhaustion
- [ ] Other: _________

## Baseline Metrics
**Expected Performance**:
- Throughput: ____ msg/s
- Latency (p50): ____ us
- Latency (p95): ____ us
- Latency (p99): ____ us
- Memory: ____ MB

**Actual Performance**:
- Throughput: ____ msg/s
- Latency (p50): ____ us
- Latency (p95): ____ us
- Latency (p99): ____ us
- Memory: ____ MB

**Regression**: ____% degradation

## Reproduction Steps
1. Setup: _________
2. Load: _________
3. Duration: _________
4. Observed: _________

## Benchmark Results
```
Paste benchmark output here
```

## Environment
- **Erlang/OTP**: [version]
- **erlmcp**: [version]
- **OS**: [OS and version]
- **Hardware**: [CPU, RAM]
- **Transport**: [TCP/SSE/stdio]
- **Concurrency**: [number of connections/processes]

## Analysis
**Suspected Cause**:
-
-

**Investigation**:
-
-

## Additional Context
Any other context, monitoring data, or analysis.

## Metrology Compliance
- [ ] Using canonical units (msg/s, us, MiB)
- [ ] Scope specified (per_connection, per_node)
- [ ] Workload ID defined
- [ ] Precision noted
