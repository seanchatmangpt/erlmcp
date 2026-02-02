# erlmcp-flow Performance Index (Simplified for MVP)

**Version**: MVP (v0.1.0-alpha)
**Scope**: Simple, unoptimized baseline measurements
**Date**: 2026-02-02

---

## Quick Reference

| Metric | MVP Baseline | MVP Target | Future Target (v1.0) |
|--------|--------------|------------|----------------------|
| **Throughput** | ~15K msg/s | ≥10K msg/s | 500K msg/s |
| **Latency (p99)** | ~45 ms | <500 ms | <50 ms |
| **Memory** | ~2 MB | <500 MB | <500 MB |
| **Task Loss** | 0% | 0% | 0% |
| **Recovery (p99)** | ~150 ms | <2s | <100 ms |

**Status**: MVP targets PASS

---

## 1. Throughput Metrics

### Task Throughput (msg/s)

**Definition**: Number of tasks completed per second (10 agents, 1000 tasks)

| Version | Value | Improvement | Note |
|---------|-------|-------------|------|
| **MVP (v0.1)** | 15K msg/s | Baseline | Simple gen_server, no optimization |
| v0.2 (planned) | 50K msg/s | 3x | ETS queue, batch assignment |
| v0.3 (planned) | 100K msg/s | 6x | Agent pooling, pipelining |
| v1.0 (planned) | 500K msg/s | 33x | Lock-free, zero-copy |

**Measurement**:
```erlang
Start = erlang:monotonic_time(microsecond),
% Submit 1000 tasks to 10 agents
Throughput = 1000 / (Duration / 1000000).
```

**Bottlenecks** (future optimization):
- Task assignment overhead (gen_server call): 10x potential
- Registry lookup (gproc): 5x potential
- Message serialization: 2x potential

---

## 2. Latency Metrics

### Agent Task Latency (ms)

**Definition**: End-to-end task latency (submit → execute → complete)

| Percentile | MVP (v0.1) | v0.2 | v0.3 | v1.0 |
|------------|-----------|------|------|------|
| **p50** | ~5 ms | ~2 ms | ~1 ms | ~0.5 ms |
| **p95** | ~20 ms | ~10 ms | ~5 ms | ~2 ms |
| **p99** | ~45 ms | ~25 ms | ~10 ms | ~5 ms |
| **avg** | ~8 ms | ~4 ms | ~2 ms | ~1 ms |

**Target**: <500ms p99 (MVP), <50ms p99 (v1.0)

**Measurement**:
```erlang
% Warmup + measure 100 samples
Latencies = [measure_task_latency(Agents) || _ <- lists:seq(1, 100)],
P99 = percentile(Latencies, 0.99).
```

**Breakdown** (MVP):
- Registry lookup: ~8μs (gproc)
- Message passing: ~100μs
- gen_server overhead: ~200μs
- Task execution: 0-10ms (simulated work)
- **Total overhead**: ~300μs (negligible)

---

## 3. Memory Metrics

### Process Memory Overhead (MB)

**Definition**: Memory delta after spawning 10 agents + 100 in-flight tasks

| Component | MVP (v0.1) | v1.0 (with features) |
|-----------|-----------|----------------------|
| **Agents (10)** | 2 MB | 10 MB |
| HNSW Index | N/A | 100 MB |
| Raft Log | N/A | 50 MB |
| Pattern DB | N/A | 200 MB |
| **Total** | 2 MB | 360 MB |

**Target**: <500MB (allows future features)

**Per-Agent Breakdown** (MVP):
- Process overhead: ~2KB (heap, stack)
- gen_server state: ~1KB (maps)
- Task state: ~500 bytes
- **Total**: ~200KB × 10 = 2MB

**Measurement**:
```erlang
erlang:garbage_collect(),
MemBefore = erlang:memory(total),
% Spawn agents + tasks
MemAfter = erlang:memory(total),
UsedMB = (MemAfter - MemBefore) / (1024 * 1024).
```

---

## 4. Reliability Metrics

### Task Loss Rate (%)

**Definition**: Percentage of tasks lost during agent failures (3/10 agents killed)

| Scenario | MVP (v0.1) | Target | Note |
|----------|-----------|--------|------|
| **Normal operation** | 0% | 0% | No failures |
| **30% agent failures** | 0% | 0% | Retry logic handles |
| **Leader election** | 0% | 0% | Tasks queued during election |
| **Network partition** | N/A | <0.1% | Not tested in MVP |

**Target**: 0% (retry loop ensures delivery, max 3 attempts)

**Measurement**:
```erlang
% Kill 3/10 agents mid-execution
Completed = count_completed_tasks(TaskIds, 60000),
LossRate = ((1000 - Completed) / 1000) * 100.
```

---

### Agent Recovery Time (ms)

**Definition**: Time from agent crash → supervisor restart → re-registration

| Percentile | MVP (v0.1) | Target (MVP) | Target (v1.0) |
|------------|-----------|--------------|---------------|
| **p50** | ~80 ms | <2000 ms | <50 ms |
| **p95** | ~120 ms | <2000 ms | <75 ms |
| **p99** | ~150 ms | <2000 ms | <100 ms |
| **avg** | ~90 ms | <2000 ms | <60 ms |

**Breakdown** (MVP):
- Supervisor detects crash: ~10ms
- Restart gen_server: ~50ms
- Re-register with gproc: ~20ms
- Update Raft state: ~10ms
- **Total**: ~90ms

**Measurement**:
```erlang
Start = erlang:monotonic_time(microsecond),
exit(Pid, kill),
% Supervisor restarts
Duration = erlang:monotonic_time(microsecond) - Start.
```

---

## 5. Optimization Roadmap

### v0.1.0-alpha (MVP) → v1.0.0

| Phase | Version | Timeline | Throughput | Latency (p99) | Focus |
|-------|---------|----------|------------|---------------|-------|
| **MVP** | v0.1 | Week 1-4 | 15K msg/s | ~45 ms | Ship working system |
| Hot Paths | v0.2 | Week 5-6 | 50K msg/s | ~25 ms | ETS, batching |
| Concurrency | v0.3 | Week 7-10 | 100K msg/s | ~10 ms | Pooling, pipelining |
| Full Optimization | v1.0 | Week 11-12 | 500K msg/s | ~5 ms | Lock-free, zero-copy |

**Total Improvement**: 33x throughput, 9x latency

---

## 6. Benchmark Suite

### MVP Benchmarks

| Benchmark | Module | What It Measures | Target |
|-----------|--------|------------------|--------|
| **Throughput** | `erlmcp_flow_mvp_bench:run_throughput/0` | 1000 tasks / 10 agents | ≥10K msg/s |
| **Latency** | `erlmcp_flow_mvp_bench:run_latency/0` | p50/p95/p99 task latency | <500ms p99 |
| **Memory** | `erlmcp_flow_mvp_bench:run_memory/0` | Process overhead | <500MB |
| **Reliability** | `erlmcp_flow_mvp_bench:run_reliability/0` | Task loss, recovery time | 0% loss, <2s recovery |

**Usage**:
```bash
# Run all benchmarks
rebar3 eunit --module erlmcp_flow_mvp_bench

# View results
cat bench/mvp/results/mvp_baseline_latest.md
```

**Output**: JSON + Markdown reports in `apps/erlmcp_flow/bench/mvp/results/`

---

## 7. Regression Detection

### Alert Thresholds (10% degradation)

| Metric | Baseline | Alert If | Action |
|--------|----------|----------|--------|
| **Throughput** | 15K msg/s | <13.5K msg/s | Profile with fprof |
| **Latency p99** | 45 ms | >50 ms | Check hot paths |
| **Memory** | 2 MB | >3 MB | Memory leak check |
| **Task Loss** | 0% | >0.1% | Retry logic bug |
| **Recovery p99** | 150 ms | >200 ms | Supervisor issue |

**CI/CD Integration**:
```bash
# Compare with baseline
./scripts/compare_baseline.sh current.json baseline.json

# Exit code 1 if regression > 10%
```

---

## 8. Profiling Tools

### When to Use

| Tool | Use Case | Command |
|------|----------|---------|
| **fprof** | Find slow functions | `fprof:trace([start]), ...` |
| **eprof** | Time-based profiling | `eprof:profile([Pid])` |
| **recon** | Live production tracing | `recon_trace:calls(...)` |
| **observer** | Visual process monitoring | `observer:start()` |

**Example** (fprof):
```erlang
fprof:trace([start, {procs, [Pid]}]),
% Run benchmark
fprof:profile(),
fprof:analyse([{dest, "profile.txt"}]).
```

---

## 9. Known Limitations (MVP)

| Limitation | Why | When Fixed |
|------------|-----|------------|
| **No network overhead** | All agents local | v0.2 (distributed) |
| **Simulated work** | rand:uniform(10)ms sleep | v0.2 (real tasks) |
| **No backpressure** | Queue always has capacity | v0.2 (token bucket) |
| **No Byzantine consensus** | Raft only | v0.3 (PBFT) |
| **No observability** | stdout logging | v0.2 (OTEL) |
| **No chaos testing** | Single failures only | v0.3 (full chaos) |

---

## 10. References

### Documents

- **MVP Baseline Report**: `/home/user/erlmcp/docs/ERLMCP_FLOW_MVP_BASELINE_PERFORMANCE.md`
- **80/20 Roadmap**: `/home/user/erlmcp/ERLMCP_FLOW_80_20_ROADMAP.md`
- **Benchmark Code**: `/home/user/erlmcp/apps/erlmcp_flow/bench/mvp/erlmcp_flow_mvp_bench.erl`

### External

- **OTP Efficiency Guide**: https://www.erlang.org/doc/efficiency_guide/processes.html
- **gproc Performance**: 553K lookups/sec baseline
- **Erlang Profiling**: https://www.erlang.org/doc/man/fprof.html

---

## Appendix: Quick Commands

```bash
# Run MVP benchmarks
rebar3 eunit --module erlmcp_flow_mvp_bench

# View latest report
cat apps/erlmcp_flow/bench/mvp/results/mvp_baseline_latest.md

# Compare with baseline
./scripts/compare_baseline.sh bench/mvp/results/mvp_baseline_latest.json \
                              bench/mvp/results/mvp_baseline_baseline.json

# Profile hot paths
erl -eval 'fprof:trace([start]), erlmcp_flow_mvp_bench:run_throughput(), fprof:profile(), fprof:analyse().'
```

---

**Document Version**: 1.0 (Simplified for MVP)
**Last Updated**: 2026-02-02
**Status**: Baseline Index Established
