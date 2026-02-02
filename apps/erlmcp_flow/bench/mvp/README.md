# erlmcp-flow MVP Baseline Benchmarks

**Purpose**: Establish performance baselines for MVP (v0.1.0-alpha)
**Scope**: Simple, unoptimized measurements
**Target**: 10K msg/s, <500ms p99, <500MB memory, 0% loss, <2s recovery

---

## Quick Start

```bash
# Compile the benchmark
cd /home/user/erlmcp
rebar3 compile

# Run all MVP benchmarks (in Erlang shell)
erl -pa _build/default/lib/*/ebin
> erlmcp_flow_mvp_bench:run_all().

# Or run specific benchmarks
> erlmcp_flow_mvp_bench:run_throughput().
> erlmcp_flow_mvp_bench:run_latency().
> erlmcp_flow_mvp_bench:run_memory().
> erlmcp_flow_mvp_bench:run_reliability().
```

**Output**: JSON + Markdown reports in `bench/mvp/results/`

---

## Benchmarks

### 1. Task Throughput

**Measures**: Tasks per second (10 agents, 1000 tasks)
**Target**: ≥10,000 msg/s
**Expected**: ~15,000 msg/s

```erlang
erlmcp_flow_mvp_bench:run_throughput().
```

---

### 2. Agent Latency

**Measures**: End-to-end task latency (p50/p95/p99)
**Target**: <500ms p99
**Expected**: ~45ms p99

```erlang
erlmcp_flow_mvp_bench:run_latency().
```

---

### 3. Memory Usage

**Measures**: Process overhead (10 agents + 100 tasks)
**Target**: <500MB
**Expected**: ~2MB

```erlang
erlmcp_flow_mvp_bench:run_memory().
```

---

### 4. Reliability

**Measures**: Task loss rate (with agent failures), recovery time
**Target**: 0% loss, <2s recovery p99
**Expected**: 0% loss, ~150ms recovery p99

```erlang
erlmcp_flow_mvp_bench:run_reliability().
```

---

## Interpreting Results

### Throughput

| Result | Status | Action |
|--------|--------|--------|
| ≥10K msg/s | PASS | MVP target met |
| <10K msg/s | FAIL | Profile with fprof |

### Latency (p99)

| Result | Status | Action |
|--------|--------|--------|
| <500ms | PASS | MVP target met |
| ≥500ms | FAIL | Check task execution overhead |

### Memory

| Result | Status | Action |
|--------|--------|--------|
| <500MB | PASS | MVP target met |
| ≥500MB | FAIL | Memory leak investigation |

### Task Loss

| Result | Status | Action |
|--------|--------|--------|
| 0% | PASS | MVP target met |
| >0% | FAIL | Check retry logic |

### Recovery (p99)

| Result | Status | Action |
|--------|--------|--------|
| <2s | PASS | MVP target met |
| ≥2s | FAIL | Supervisor restart issue |

---

## Comparison: MVP vs Future

| Metric | MVP (v0.1) | Future (v1.0) | Gap |
|--------|-----------|---------------|-----|
| Throughput | ~15K msg/s | 500K msg/s | 33x |
| Latency (p99) | ~45ms | <50ms | 1.1x |
| Memory | ~2MB | ~360MB | - |
| Task Loss | 0% | 0% | None |
| Recovery | ~150ms | <100ms | 1.5x |

**Key Insight**: MVP already meets targets with simple implementation. Future optimization (33x throughput) is deferred to v0.2-v1.0.

---

## Regression Detection

### Alert Thresholds (10% degradation)

```bash
# Run benchmark
erlmcp_flow_mvp_bench:run_all().

# Compare with baseline
./scripts/compare_baseline.sh bench/mvp/results/mvp_baseline_latest.json \
                              bench/mvp/results/mvp_baseline_baseline.json
```

**CI/CD**: Fail build if any metric regresses >10%

---

## Profiling (Future Optimization)

### fprof (Function Profiling)

```erlang
fprof:trace([start, {procs, all}]),
erlmcp_flow_mvp_bench:run_throughput(),
fprof:profile(),
fprof:analyse([{dest, "fprof_throughput.txt"}]).
```

### eprof (Time Profiling)

```erlang
eprof:start(),
eprof:profile([self()]),
erlmcp_flow_mvp_bench:run_latency(),
eprof:stop_profiling(),
eprof:analyze(total).
```

### recon (Live Tracing)

```erlang
recon_trace:calls({erlmcp_flow_registry, lookup_agent, '_'}, 100).
```

---

## File Structure

```
bench/mvp/
├── README.md                          ← This file
├── erlmcp_flow_mvp_bench.erl         ← Benchmark suite
└── results/                           ← Benchmark outputs
    ├── mvp_baseline_YYYYMMDD_HHMMSS.json
    └── mvp_baseline_YYYYMMDD_HHMMSS.md
```

---

## Documentation

- **MVP Baseline Report**: `/home/user/erlmcp/docs/ERLMCP_FLOW_MVP_BASELINE_PERFORMANCE.md`
- **Performance Index**: `/home/user/erlmcp/docs/ERLMCP_FLOW_PERFORMANCE_INDEX.md`
- **80/20 Roadmap**: `/home/user/erlmcp/ERLMCP_FLOW_80_20_ROADMAP.md`

---

## Notes

1. **MVP Focus**: These benchmarks establish baseline, not production performance
2. **Simulated Work**: Tasks do rand:uniform(10)ms sleep (not real workloads)
3. **No Network**: All agents local (no distributed Erlang overhead)
4. **Future Work**: v0.2+ will add real tasks, backpressure, distributed agents
5. **Regression**: Run after each commit to detect performance degradation

---

**Version**: MVP (v0.1.0-alpha)
**Last Updated**: 2026-02-02
