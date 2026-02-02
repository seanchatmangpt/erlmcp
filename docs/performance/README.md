# erlmcp Performance Engineering Documentation

**Comprehensive performance benchmarking, optimization, and regression testing for MCP protocol implementation**

---

## Overview

This directory contains all performance-related documentation, benchmark plans, and optimization strategies for erlmcp's Model Context Protocol (MCP) implementation.

**Performance Targets:**
- P50 Latency: 2-3ms (40% improvement from 5ms baseline)
- P95 Latency: 8-12ms (40% improvement from 18ms baseline)
- Throughput: 100K req/s (10x improvement from 10K baseline)
- Connections: 40-50K per node (maintained)

---

## Documentation Index

### Core Documents

| Document | Purpose | Audience |
|----------|---------|----------|
| **[MCP_BENCHMARK_PLAN.md](MCP_BENCHMARK_PLAN.md)** | Complete benchmark strategy (30+ pages) | Performance Engineers |
| **[OPTIMIZATION_ROADMAP.md](OPTIMIZATION_ROADMAP.md)** | Quick reference optimization guide | Developers |
| **[MCP_SPEC_PERFORMANCE_ANALYSIS.md](../MCP_SPEC_PERFORMANCE_ANALYSIS.md)** | Existing performance baseline | Architects |

### Implementation Files

| File | Purpose | Type |
|------|---------|------|
| `bench/erlmcp_bench_baseline.erl` | Baseline benchmark runner | Module |
| `bench/erlmcp_bench_regression.erl` | CI regression tests | Module |
| `bench/erlmcp_bench_core_ops.erl` | Core operations benchmarks | Module |
| `bench/erlmcp_bench_mcp_features.erl` | MCP features benchmarks | Module |
| `bench/erlmcp_bench_transports.erl` | Transport layer benchmarks | Module |
| `apps/erlmcp_observability/src/erlmcp_profiler.erl` | CPU/memory profiling | Module |

---

## Quick Start

### 1. Run Baseline Benchmarks
```bash
cd /home/user/erlmcp
make compile

# Run all baseline benchmarks (15-20 minutes)
erl -pa _build/default/lib/*/ebin -noshell \
  -eval "erlmcp_bench_baseline:run_all()." \
  -s init stop

# Output: bench/baseline/main_YYYY-MM-DD.json
```

### 2. Run Regression Tests (CI)
```bash
# Fast regression suite (<5 minutes)
erl -pa _build/default/lib/*/ebin -noshell \
  -eval "erlmcp_bench_regression:run_all()." \
  -s init stop

# Output: bench/results/regression_*.json
```

### 3. Profile a Hot Path
```erlang
% Start Erlang shell
erl -pa _build/default/lib/*/ebin

% Start server
application:ensure_all_started(erlmcp).
{ok, ServerPid} = erlmcp_server:start_link(bench_server, #{}).

% Profile for 60 seconds
erlmcp_profiler:profile_pid(ServerPid, #{
    duration => 60000,
    mode => fprof,
    output => "profile.txt"
}).

% Generate flame graph
erlmcp_profiler:flame_graph("profile.txt", "flame.svg").
```

### 4. Compare with Baseline
```bash
# Compare regression results with baseline
python3 scripts/compare_benchmarks.py \
  bench/results/regression_latest.json \
  bench/baseline/main_latest.json \
  --threshold 0.10 \
  --fail-on-regression
```

---

## Benchmark Categories

### 1. Baseline Benchmarks (Complete)
**Purpose:** Establish performance baseline for regression detection  
**Duration:** 15-20 minutes  
**Frequency:** Daily (automated)  

**Workloads:**
- Core operations: Registry, queue, session, pool (4 workloads)
- MCP features: Tool calls, resources, prompts, sampling (4 workloads)
- Transports: stdio, TCP, HTTP, WebSocket, SSE (5 workloads)

**Output:** `bench/baseline/main_YYYY-MM-DD.json`

---

### 2. Regression Benchmarks (Fast)
**Purpose:** CI/CD performance checks (<10% degradation detection)  
**Duration:** <5 minutes  
**Frequency:** Every PR  

**Workloads:**
- JSON encode/decode (small)
- Tool call (simple)
- Resource read
- Registry lookup
- 5 benchmarks total

**Output:** `bench/results/regression_*.json`

---

### 3. Load Benchmarks (Stress)
**Purpose:** Validate capacity under high load  
**Duration:** 5-60 minutes per scenario  
**Frequency:** Weekly  

**Scenarios:**
- Idle connections: 50K connections, memory efficiency
- Sustained load: 100K req/s for 5 minutes
- Burst load: 200K req/s for 10 seconds
- Subscription fanout: 10K subscribers, 100Hz updates

**Output:** `bench/results/load_*.json`

---

### 4. Profile-Guided Benchmarks (Targeted)
**Purpose:** Identify bottlenecks, optimize hot paths  
**Duration:** 30-60 seconds per profile  
**Frequency:** On-demand  

**Tools:**
- fprof: Function-level CPU profiling
- eprof: Time-based profiling
- Memory snapshots: Process memory analysis
- Binary leak detection: Memory leak identification

**Output:** `bench/profiles/*.prof`, `bench/profiles/*.svg`

---

## Optimization Priority

### High Priority (Week 1-2)
**ROI:** 3-5x improvement, low risk, low effort

1. **Schema Caching** (2 days)
   - Gain: 5x validation speedup
   - Risk: Low (ETS caching)
   - Expected: 25ms → 5ms P95

2. **jsx → jiffy** (1 day)
   - Gain: 3x JSON speedup
   - Risk: Low (drop-in NIF)
   - Expected: 0.8ms → 0.25ms P95

---

### Medium Priority (Week 3-4)
**ROI:** 4-5x improvement, low-medium risk, medium effort

3. **Async Tool Execution** (3 days)
   - Gain: 5x concurrent tools
   - Risk: Low (poolboy)
   - Expected: 1 → 10+ concurrent

4. **Batch Notifications** (2 days)
   - Gain: 4x fanout speedup
   - Risk: Low
   - Expected: 80ms → 20ms

5. **Zero-Copy Binaries** (3 days)
   - Gain: -30% GC overhead
   - Risk: Medium
   - Expected: 300ms → 50ms GC pauses

---

### Low Priority (Week 5-8)
**ROI:** 10x improvement, medium-high risk, high effort

6. **Server Pooling** (5 days)
   - Gain: 10x throughput
   - Risk: Medium (distributed state)
   - Expected: 10K → 100K req/s

---

## Performance Budget

**Per-Operation Latency Budgets (P95):**

| Operation | Budget | Current | Status |
|-----------|--------|---------|--------|
| JSON encode (small) | 1.5ms | 0.8ms | ✓ PASS |
| JSON decode (small) | 1.5ms | 0.8ms | ✓ PASS |
| Schema validation | 5ms | 25ms | ✗ OVER (5x) |
| Tool call (simple) | 12ms | 18ms | ✗ OVER (1.5x) |
| Resource read | 3ms | 2ms | ✓ PASS |
| Registry lookup | 0.1ms | 0.05ms | ✓ PASS |

**Action:** Optimize schema validation (HIGH PRIORITY)

---

## CI/CD Integration

### GitHub Actions Workflow

**.github/workflows/performance-regression.yml**
```yaml
name: Performance Regression Tests

on:
  pull_request:
    branches: [main]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v3
      
      - name: Setup OTP 28.3.1
        uses: erlef/setup-beam@v1
        with:
          otp-version: 28.3.1
      
      - name: Compile
        run: make compile
      
      - name: Run regression benchmarks
        run: |
          erl -pa _build/default/lib/*/ebin -noshell \
            -eval "erlmcp_bench_regression:run_all()." \
            -s init stop
      
      - name: Compare with baseline
        run: |
          python3 scripts/compare_benchmarks.py \
            bench/results/regression_*.json \
            bench/baseline/main_latest.json \
            --threshold 0.10 \
            --fail-on-regression
```

---

## Expected Speedups (Post-Optimization)

### Latency Improvements

| Metric | Baseline | Target | Expected | Improvement |
|--------|----------|--------|----------|-------------|
| P50 Latency | 5ms | 2-3ms | 2.8ms | 1.8x |
| P95 Latency | 18ms | 8-12ms | 10ms | 1.8x |
| P99 Latency | 35ms | <30ms | 22ms | 1.6x |

### Throughput Improvements

| Operation | Baseline | Target | Expected | Improvement |
|-----------|----------|--------|----------|-------------|
| Tool call (no validation) | 5K req/s | 25K | 30K | 6x |
| Tool call (with validation) | 500 req/s | 5K | 6K | 12x |
| Resource operations | 10K req/s | 50K | 45K | 4.5x |
| Subscription fanout | 12.5 msg/s | 50 | 55 | 4.4x |

### Optimization Contributions

| Optimization | Latency Gain | Throughput Gain | Week |
|--------------|--------------|-----------------|------|
| Schema caching | -75% | 5x | 1 |
| jsx → jiffy | -60% | 3x | 1 |
| Async tools | None | 5x | 3 |
| Batch notifications | -75% (fanout) | 4x | 3 |
| Zero-copy | -20% | 1.2x | 4 |
| Server pooling | None | 10x | 5-6 |
| **Cumulative** | **~2x** | **~10x** | 8 |

---

## Success Criteria

**Phase 1 (Week 1-2): Quick Wins**
- [x] Baseline benchmarks established
- [ ] Schema caching implemented (5x validation)
- [ ] jiffy migration complete (3x JSON)
- [ ] Regression CI setup
- **Target:** 2.5x latency, 3x throughput

**Phase 2 (Week 3-4): Async & Batching**
- [ ] Async tool execution (5x concurrency)
- [ ] Batch notifications (4x fanout)
- [ ] Zero-copy binaries (-30% GC)
- [ ] Load test 50K connections
- **Target:** 4x throughput, GC optimization

**Phase 3 (Week 5-8): Scaling**
- [ ] Server pooling (10x throughput)
- [ ] Profile all hot paths
- [ ] Load test 100K req/s
- **Target:** 10x throughput, 100K req/s

**Phase 4 (Week 9-12): Production**
- [ ] Chaos testing
- [ ] Documentation complete
- [ ] Daily automated benchmarks
- **Target:** Production-ready, monitored

---

## Monitoring & Alerts

### Daily Benchmark Automation
```bash
# Cron job (daily at 2 AM)
0 2 * * * \
  make compile && \
  erl -pa _build/default/lib/*/ebin -noshell \
    -eval "erlmcp_bench_baseline:run_all()." \
    -s init stop
```

### Regression Alerts
- >10% latency increase → Block PR
- >10% throughput decrease → Block PR
- Memory leak detected → Alert + block
- GC pause >5ms → Warning

---

## References

### Internal Documentation
- [MCP Benchmark Plan](MCP_BENCHMARK_PLAN.md) - Complete 30-page strategy
- [Optimization Roadmap](OPTIMIZATION_ROADMAP.md) - Quick reference guide
- [Performance Analysis](../MCP_SPEC_PERFORMANCE_ANALYSIS.md) - Existing baseline

### Code Modules
- `apps/erlmcp_observability/src/erlmcp_profiler.erl` - Profiling tools
- `bench/erlmcp_bench_*.erl` - Benchmark modules
- `apps/erlmcp_validation/src/erlmcp_performance_validator.erl` - Validation

### External Tools
- [FlameGraph](https://github.com/brendangregg/FlameGraph) - Flame graph visualization
- [fprof](https://www.erlang.org/doc/man/fprof) - Erlang profiler
- [eprof](https://www.erlang.org/doc/man/eprof) - Time-based profiler

---

## Contact

**Performance Engineering Team**
- Agent: Erlang Performance Agent
- Role: Benchmark design, profiling, optimization
- Workflow: Measure → Profile → Optimize → Re-measure → Report

**Support Agents**
- Erlang OTP Developer: Implementation
- Erlang Architect: System design
- Code Reviewer: Quality gates

---

**CODE LIKE A JOE ARMSTRONG AGI SWARM!!!**
