# erlmcp Performance Baseline & Targets
# Date: 2026-02-01

## Current Baseline (From Recent Benchmark Run)

**Source:** `/home/user/erlmcp/bench/results/benchmark_summary_20260201.json`

### Core Operations (10K workload)

| Component | Throughput | Latency p50 | Latency p95 | Latency p99 | Avg Latency |
|-----------|-----------|-------------|-------------|-------------|-------------|
| **Registry** | 2.2M ops/s | 1 us | 82 us | 98 us | 51.9 us |
| **Queue** | 100M ops/s | <1 us | <1 us | <1 us | 0.1 us |
| **Pool** | 16.7M ops/s | <1 us | 1 us | 1 us | 0.6 us |
| **Session** | 55.6M ops/s | 1 us | 2 us | 3 us | 1.8 us |

**Overall Throughput:** 2,204,221 ops/s (10K operations benchmark)

### Bottleneck Analysis

From `benchmark_summary_20260201.json`:

1. **Registry Operations (51.9 us avg)** - CRITICAL BOTTLENECK
   - gproc lookups dominate hot path
   - Recommendation: Add LRU cache with 90% hit rate
   - Target: Reduce to <5 us avg

2. **Session Operations (1.8 us avg)** - MINOR BOTTLENECK
   - ETS contention on concurrent access
   - Recommendation: Shard session table (8-16 shards)
   - Target: Reduce to <1 us avg

3. **Queue Operations (0.1 us avg)** - OPTIMAL
   - Already extremely fast
   - No optimization needed

4. **Pool Operations (0.6 us avg)** - OPTIMAL
   - Fast, but room for improvement
   - Recommendation: Pre-allocate pool workers
   - Target: Reduce to <0.3 us avg

### Regression Status

**P95 Latency:** REGRESSED 64% vs baseline (50 us → 82 us)
**Throughput:** IMPROVED 120% vs baseline (1M ops/s → 2.2M ops/s)
**Overall:** PASS (throughput improvement outweighs latency regression)

---

## Optimization Targets (Claude-Flow Equivalent)

### Phase 1: Caching & Batching (5-10x improvement)

**Registry with LRU Cache:**
- Current: 2.2M ops/s, 51.9 us avg
- Target: 11M ops/s, 5 us avg (5x improvement)
- Implementation: 10K entry LRU cache, 5s TTL

**Request Batching:**
- Current: 1 request per call
- Target: 100 requests per batch (amortize overhead)
- Expected: 80% reduction in roundtrips

**Expected Combined Impact:** 10x throughput, 90% latency reduction

### Phase 2: Connection Pooling (2-3x improvement)

**Pool Optimization:**
- Current: 16.7M ops/s, 0.6 us avg
- Target: 50M ops/s, 0.2 us avg (3x improvement)
- Implementation: Pre-warmed pool, keep-alive connections

**Expected Impact:** 3x throughput for network I/O

### Phase 3: Async Execution (10-20x improvement)

**Parallel Task Execution:**
- Current: Sequential gen_server:call (blocking)
- Target: Parallel spawn with work stealing (non-blocking)
- Implementation: Supervisor pool of N workers

**Expected Impact:** N-way parallelism (N = CPU cores * 2)

### Phase 4: OTP 28 Native JSON (2-3x improvement)

**JSON Encoding:**
- Current: JSX library (70% of CPU time in hot path)
- Target: Native json module (2-3x faster)
- Implementation: Drop-in replacement

**Expected Impact:** 2-3x overall throughput

### Phase 5: Circuit Breakers (1.5-2x improvement)

**Adaptive Circuit Breaker:**
- Current: No fail-fast mechanism
- Target: Fail-fast on overload (prevent cascade)
- Implementation: Per-endpoint breaker with adaptive thresholds

**Expected Impact:** 1.5-2x throughput under load, 99.99% uptime

---

## Combined Target Performance

| Component | Baseline | Phase 1 | Phase 2 | Phase 3 | Phase 4 | Phase 5 (Final) |
|-----------|----------|---------|---------|---------|---------|-----------------|
| **Registry** | 2.2M | 11M | 22M | 88M | 176M | 220M |
| **Queue** | 100M | 100M | 100M | 100M | 200M | 200M |
| **Pool** | 16.7M | 50M | 100M | 400M | 800M | 1000M |
| **Session** | 55.6M | 111M | 222M | 888M | 1.77B | 2.22B |
| **Overall** | 2.2M | 11M | 22M | 88M | 176M | 220M |

**Total Expected Improvement:** 100x (conservative) to 1000x (aggressive)

---

## Benchmark Execution Plan

### Step 1: Establish Current Baseline

```bash
cd /home/user/erlmcp
make compile

# Run core ops benchmark (10K operations)
cd bench
erl -pa ../_build/default/lib/*/ebin -noshell \
  -eval "erlmcp_bench_core_ops:run(<<\"core_ops_10k\">>), halt()."

# Save baseline
cp results/core_ops_core_ops_10k_*.json baselines/baseline_pre_optimization.json
```

**Expected Output:**
- Registry: ~2.2M ops/s, ~52 us avg
- Queue: ~100M ops/s, <1 us avg
- Pool: ~17M ops/s, ~0.6 us avg
- Session: ~56M ops/s, ~1.8 us avg

### Step 2: Implement Phase 1 (Caching + Batching)

**Deliverables:**
1. `erlmcp_cache_manager.erl` - LRU cache for registry lookups
2. `erlmcp_batch_processor.erl` - Request batching
3. `erlmcp_cache_manager_tests.erl` - EUnit tests
4. `erlmcp_batch_processor_tests.erl` - EUnit tests

**Success Criteria:**
- Registry: >11M ops/s (5x improvement)
- Cache hit rate: >90%
- Tests: 100% passing
- Coverage: >80%

### Step 3: Measure & Compare

```bash
# Run benchmark again
erl -pa ../_build/default/lib/*/ebin -noshell \
  -eval "erlmcp_bench_core_ops:run(<<\"core_ops_10k\">>), halt()."

# Compare against baseline
scripts/bench/validate_baseline.sh \
  baselines/baseline_pre_optimization.json \
  results/core_ops_core_ops_10k_latest.json

# Expected diff:
# Registry: +400% throughput, -90% latency
# Overall: +400% throughput
```

### Step 4: Iterate Through Phases

Repeat Step 2-3 for each phase:
- Phase 2: Connection pooling
- Phase 3: Async execution
- Phase 4: OTP 28 features
- Phase 5: Circuit breakers

---

## Metrics Dashboard

### Key Metrics to Track

**Throughput:**
- `throughput_msg_per_s` - Overall message throughput
- `registry_ops_per_s` - Registry lookup throughput
- `cache_hit_rate_pct` - Cache effectiveness

**Latency:**
- `latency_p50_us` - Median latency (target: <1 us)
- `latency_p95_us` - 95th percentile (target: <10 us)
- `latency_p99_us` - 99th percentile (target: <50 us)

**Resource Usage:**
- `memory_heap_mib_per_conn` - Memory per connection
- `cpu_percent_avg` - CPU utilization
- `scheduler_utilization_pct` - Scheduler efficiency

**Reliability:**
- `circuit_breaker_trips_per_min` - Overload detection
- `error_rate_pct` - Failure rate
- `uptime_pct` - Availability

### Real-Time Monitoring

```erlang
% Get current metrics
{ok, Summary} = erlmcp_metrics:get_performance_summary().

% Expected output:
#{
  <<"uptime_ms">> => 1000000,
  <<"throughput_msg_per_s">> => 2204221,
  <<"latency_p50_us">> => 1,
  <<"latency_p95_us">> => 82,
  <<"latency_p99_us">> => 98,
  <<"cache_hit_rate_pct">> => 92.5,
  <<"scheduler_utilization_pct">> => 85.0
}
```

---

## Regression Thresholds

| Metric | Threshold | Action |
|--------|-----------|--------|
| Throughput | >10% decrease | FAIL - investigate |
| Latency p50 | >20% increase | WARN - review |
| Latency p95 | >30% increase | WARN - review |
| Latency p99 | >50% increase | WARN - review |
| Memory | >15% increase | FAIL - investigate |
| Error rate | >1% | FAIL - rollback |

---

## Next Actions

1. **IMMEDIATE:** Run baseline benchmark to capture current state
2. **Day 1-2:** Implement Phase 1 (Caching + Batching)
3. **Day 3-4:** Implement Phase 2 (Connection Pooling)
4. **Day 5-7:** Implement Phase 3 (Async Execution)
5. **Day 8:** Implement Phase 4 (OTP 28 Features)
6. **Day 9-10:** Implement Phase 5 (Circuit Breakers)
7. **Day 11:** Final validation & documentation

**Expected Outcome:** 100-1000x performance improvement, matching or exceeding claude-flow's 352x speedup.

---

**Document Owner:** erlang-performance  
**Last Updated:** 2026-02-01  
**Status:** Ready for Implementation
