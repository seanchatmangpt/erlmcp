# ERLMCP Performance Documentation

## Quick Links

- **[Optimization Plan](optimization_plan.md)** - Complete 4-week performance optimization roadmap (RECOMMENDED START)
- **[Performance Analysis 2026-01-30](../performance_analysis_2026-01-30.md)** - Detailed bottleneck analysis
- **[Validation Performance Summary](../../bench/results/VALIDATION_PERFORMANCE_SUMMARY_20260130.md)** - Validation framework benchmarks

---

## Current Performance Baseline

**Date:** 2026-01-30  
**Version:** v0.6.0 (OTP-27, Darwin)

### Key Metrics

| Component | Throughput | Latency (p95) | Status |
|-----------|------------|---------------|--------|
| Core Operations | 2.52M msg/s | 83us | Baseline |
| Queue Operations | 971K msg/s | 1us | Optimal |
| Pool Operations | 149K msg/s | 1us | Optimal |
| Session Operations | 242K msg/s | 29us | Good |
| Registry Operations | 553K msg/s | 97us | Bug (synthetic) |
| Network I/O (TCP) | 43K msg/s | ~50us | Bottleneck |
| Network I/O (HTTP) | 4.3K msg/s | ~100us | 10x slower than TCP |

### Critical Findings

1. **JSON Encoding Bottleneck** (Priority 1)
   - Current: jsx (pure Erlang)
   - Problem: 2-3x slower than jiffy
   - Impact: 8.5% overhead for 1KB messages, 25% for 100KB
   - Fix: Switch to jiffy (Week 2)

2. **Registry Benchmark Bug** (Priority 1)
   - Current: Uses `rand:uniform(100)` synthetic latencies
   - Problem: All registry metrics are invalid
   - Impact: Cannot trust baseline (reported 553K msg/s)
   - Fix: Real worker latency collection (Week 1)

3. **Request Correlation Maps** (Priority 2)
   - Current: maps:take/2 (O(log N))
   - Problem: ~100us p95 at 10K concurrent
   - Impact: 10-20% performance loss in high-concurrency
   - Fix: Use ETS for >1000 pending (Week 3)

4. **Percentile Calculation** (Priority 2)
   - Current: lists:nth/2 (O(N))
   - Problem: ~1000ms for 1M samples
   - Impact: Blocks stress benchmarks
   - Fix: Use array:get/2 for O(1) (Week 4)

5. **Large State Records** (Priority 3)
   - Current: 18-field state records
   - Problem: Full copy on every update
   - Impact: 5-10% performance loss
   - Fix: Group into sub-records (Week 4)

---

## Optimization Roadmap (4 Weeks)

```
Week 1: Registry Bug Fix          → Accurate baseline
Week 2: jiffy Integration          → 2-3x JSON performance
Week 3: ETS Request Correlation    → 10-20% high-concurrency improvement
Week 4: State & Binary Optimization → 5-10% cumulative improvement

Total Expected Improvement: +20-40% (2.52M → 3.0-3.5M msg/s)
```

See **[optimization_plan.md](optimization_plan.md)** for detailed implementation.

---

## Performance Diagrams

The optimization plan includes 4 PlantUML diagrams:

1. **Performance Profile** - Current bottlenecks visualized by component
2. **Benchmark Comparison** - jsx vs jiffy across message sizes
3. **Throughput Timeline** - Expected improvements over 4 weeks
4. **Component Performance** - Hotspots and optimization targets

To render diagrams:
```bash
# Install PlantUML
brew install plantuml  # macOS
apt-get install plantuml  # Ubuntu

# Generate PNG diagrams
plantuml docs/performance/optimization_plan.md

# Or use online: https://www.plantuml.com/plantuml/uml/
```

---

## Running Benchmarks

### Quick Benchmarks (<2 minutes)
```bash
make benchmark-quick
```

### Full Suite (10-15 minutes)
```bash
./scripts/bench/run_all_benchmarks.sh
```

### Specific Benchmarks
```erlang
%% Core operations (in-memory)
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).

%% Network (real sockets)
erlmcp_bench_network_real:run(<<"tcp_sustained_10k_1kib">>).

%% Integration (end-to-end)
erlmcp_bench_integration:run_all().

%% Stress (sustained load)
erlmcp_bench_stress:run(<<"stress_30s_100k_ops">>).

%% Chaos (failure injection)
erlmcp_bench_chaos:run(<<"chaos_process_crash">>).
```

### Regression Detection
```bash
# Run after each optimization
make benchmark-quick && ./scripts/bench/regression_check.sh
```

---

## Metrology Compliance

All benchmarks follow canonical units from `docs/metrology/METRICS_GLOSSARY.md`:

- `throughput_msg_per_s` (NOT req/s or ops/s)
- `latency_p50_us`, `latency_p95_us`, `latency_p99_us` (microseconds)
- `memory_heap_mib_per_conn` (scope: per_connection_heap)
- `memory_rss_mib_per_node` (scope: per_node_total)
- `bandwidth_mib_per_s` (NOT MB/s or Mbps)
- `duration_s` (seconds, NOT ms)

Required fields: `workload_id`, `timestamp`, `scope`, `precision`

Validation: `erlmcp_metrology_validator:validate/1`

---

## Performance Targets (Post-Optimization)

| Metric | Baseline | Target | Improvement |
|--------|----------|--------|-------------|
| Core throughput | 2.52M msg/s | 3.0-3.5M msg/s | +20-40% |
| JSON encoding (1KB) | 45us p95 | 16us p95 | 2.8x |
| JSON encoding (100KB) | 1850us p95 | 595us p95 | 3.1x |
| Request correlation (10K) | 100us p95 | 10us p95 | 10x |
| Percentile calc (1M) | 1000ms | 10ms | 100x |
| Integration overhead | 8.5% | 3.0% | -65% |
| TCP sustained (10K) | 100K msg/s | 120K msg/s | +20% |

---

## Quality Gates

Before declaring optimization complete:

- [ ] Compilation: 0 errors
- [ ] Tests: 100% pass rate
- [ ] Coverage: ≥80% (no regression)
- [ ] Dialyzer: 0 new warnings
- [ ] Xref: 0 new issues
- [ ] Benchmarks: <10% regression on any metric
- [ ] Memory: No leaks in 5min stress test
- [ ] Chaos: All 11 scenarios pass

---

## References

- **Architecture:** [docs/architecture.md](../architecture.md)
- **OTP Patterns:** [docs/otp-patterns.md](../otp-patterns.md)
- **Benchmark Workloads:** [docs/bench/workloads.md](../bench/workloads.md)
- **Metrology Glossary:** [docs/metrology/METRICS_GLOSSARY.md](../metrology/METRICS_GLOSSARY.md)
- **CLAUDE.md:** [CLAUDE.md](../../CLAUDE.md) (development guide)

---

## Contact

- **Agent:** erlang-performance
- **Version:** 0.6.0
- **Date:** 2026-01-30
- **Tracking:** GitHub Issues with `performance` label

---

**Next Steps:**
1. Read [optimization_plan.md](optimization_plan.md)
2. Run baseline benchmarks
3. Start Week 1: Registry bug fix
4. Validate incrementally
