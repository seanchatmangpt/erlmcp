# erlmcp Benchmark Completion Plan - v2.1 Feature Coverage

**Date:** 2026-01-28  
**Agent:** erlang-performance  
**Status:** ANALYSIS COMPLETE  

---

## Executive Summary

**Current Benchmark Coverage:** 60% (5/9 benchmark modules)  
**v2.1 Feature Coverage:** 33% (1/3 features benchmarked)  
**Baseline Status:** PARTIAL (core ops only)  
**Regression Detection:** LIMITED (missing v2 features)

### Critical Gaps Identified

1. **Cache Performance** - NO BENCHMARK (erlmcp_cache.erl exists, no bench module)
2. **Batch Performance** - INFRASTRUCTURE ONLY (erlmcp_bench_batch.erl broken, needs executor)
3. **Pool Performance** - INFRASTRUCTURE ONLY (erlmcp_bench_pool.erl broken, needs manager)
4. **Observability Overhead** - NO BENCHMARK (erlmcp_otel.erl exists, no measurement)
5. **Network Transport Baselines** - PENDING (network_real bench exists, needs execution)

---

## Current Benchmark Inventory

### ✅ Complete & Working (3/9)

| Module | Status | Workloads | Baseline | Coverage |
|--------|--------|-----------|----------|----------|
| **erlmcp_bench_core_ops** | ✅ WORKING | 4 (1K→1M) | ✅ v2.0.0 | Registry, Queue, Pool, Session |
| **erlmcp_bench_helpers** | ✅ WORKING | Test support | N/A | Percentiles, stats, metrology |
| **erlmcp_bench_helpers_tests** | ✅ WORKING | Unit tests | N/A | Test infrastructure |

### ⚠️ Infrastructure Ready, Needs Runtime (2/9)

| Module | Status | Issue | Fix Required |
|--------|--------|-------|--------------|
| **erlmcp_bench_batch** | ⚠️ BROKEN | References erlmcp_batch:start_link/2 | Integrate with actual executor |
| **erlmcp_bench_pool** | ⚠️ BROKEN | References erlmcp_pool_manager API | Integrate with actual pool manager |

### 📋 Pending Execution (2/9)

| Module | Status | Workloads | Baseline | Coverage |
|--------|--------|-----------|----------|----------|
| **erlmcp_bench_network_real** | 📋 READY | 7 (TCP/HTTP) | ❌ NONE | Transport layer |
| **erlmcp_bench_stress** | 📋 READY | 4 (30s→24hr) | ❌ NONE | Sustained load |

### ❌ Missing Benchmarks (2/9)

| Feature | Module Path | Benchmark Needed | Priority |
|---------|-------------|------------------|----------|
| **Cache** | apps/erlmcp_core/src/erlmcp_cache.erl | erlmcp_bench_cache.erl | 🔴 CRITICAL |
| **Observability** | apps/erlmcp_observability/src/erlmcp_otel.erl | erlmcp_bench_otel.erl | 🔴 CRITICAL |

---

## Gap Analysis by v2.1 Feature

### 1. Response Caching (erlmcp_cache.erl)

**Module Status:** ✅ COMPLETE  
**Benchmark Status:** ❌ MISSING  
**Expected Performance:** 10-100x improvement for cache hits  

**What to Measure:**
```erlang
% Cache hit/miss ratio
- L1 (ETS) hit latency:        Target < 1μs
- L2 (Mnesia) hit latency:     Target < 10μs
- L3 (Redis) hit latency:      Target < 1ms
- Miss + populate latency:     Baseline + cache overhead
- TTL expiration overhead:     Target < 5% CPU
- LRU eviction latency:        Target < 100μs

% Cache coherence
- Invalidate by tag:           Target < 10ms
- Invalidate by dependency:    Target < 50ms
- Cluster propagation:         Target < 100ms

% Memory efficiency
- Memory per entry:            Target < 1KB
- Max L1 size (10K entries):   Target < 10MB
- Eviction accuracy:           Target > 95% LRU compliance
```

**Workloads Needed:**
- `cache_l1_hits_100k` - 100K L1 cache hits (ETS)
- `cache_l2_hits_10k` - 10K L2 cache hits (Mnesia)
- `cache_mixed_10k` - 10K mixed (70% hits, 30% misses)
- `cache_ttl_expiration_1k` - 1K entries with TTL cleanup
- `cache_lru_eviction_10k` - 10K entries triggering LRU eviction
- `cache_tag_invalidation` - Invalidate 1K entries by tag
- `cache_cluster_coherence` - 2-node cluster cache propagation

**Regression Baseline Needed:** YES (new feature)

---

### 2. Request Batching (erlmcp_batch.erl)

**Module Status:** ✅ COMPLETE  
**Benchmark Status:** ⚠️ INFRASTRUCTURE ONLY  
**Expected Performance:** 2-5x throughput improvement  

**Current Issue:**
```erlang
% erlmcp_bench_batch.erl:190 - Broken reference
{ok, Batcher} = erlmcp_batch:start_link(Executor, #{
    strategy => Strategy,
    parallel_workers => 4
}),
```

**Fix Required:**
1. Implement actual batch executor (not mock)
2. Integrate with erlmcp_server:call_tool/3
3. Add batch protocol support to JSON-RPC layer

**What to Measure:**
```erlang
% Batching strategies
- Size-based (batch_size=10):  2-3x throughput vs single
- Time-based (10ms window):    2-4x throughput vs single
- Adaptive (5-50 range):       3-5x throughput vs single

% Latency trade-offs
- Added latency per request:   Target < 10ms (time-based)
- P99 latency impact:          Target < 2x single request
- Throughput improvement:      Target > 2x at 10K ops/s

% Resource efficiency
- Memory per batch:            Target < 1MB per 100 requests
- CPU overhead:                Target < 10% additional
```

**Workloads Needed:**
- `batch_1k` - 1K operations (current)
- `batch_10k` - 10K operations (current)
- `batch_100k` - 100K operations (current)
- `batch_comparison` - Single vs batched side-by-side

**Regression Baseline Needed:** YES (new feature)

---

### 3. Connection Pooling (erlmcp_pool_manager.erl)

**Module Status:** ✅ COMPLETE  
**Benchmark Status:** ⚠️ INFRASTRUCTURE ONLY  
**Expected Performance:** 2x improvement for 100+ concurrent clients  

**Current Issue:**
```erlang
% erlmcp_bench_pool.erl:143 - Broken reference
{ok, Pool} = erlmcp_pool_manager:start_link(#{
    name => test_pool,
    size => PoolSize,
    max_overflow => Overflow,
    strategy => Strategy
}),
```

**Fix Required:**
1. Integrate with actual transport layer
2. Add pool strategy implementations (static, dynamic, adaptive)
3. Connect to supervision tree

**What to Measure:**
```erlang
% Pool strategies
- Static (fixed size):        Baseline throughput
- Dynamic (auto-scale):       2-3x throughput under burst
- Adaptive (ML-based):        3-5x throughput optimal sizing

% Connection management
- Checkout latency:           Target < 100μs
- Checkin latency:            Target < 50μs
- Connection reuse rate:      Target > 95%
- Pool overflow handling:     Target < 1% overflow rejections

% Scalability
- 10 connections:             Baseline throughput
- 100 connections:            10x throughput
- 1000 connections:           100x throughput (linear scaling)
```

**Workloads Needed:**
- `pool_static_10` - 10 connections, 1K requests
- `pool_static_100` - 100 connections, 10K requests
- `pool_dynamic_burst` - 100→1000 connections under load
- `pool_adaptive_ml` - ML-based sizing optimization
- `pool_overflow_stress` - Test max_overflow handling

**Regression Baseline Needed:** YES (new feature)

---

### 4. Observability Overhead (erlmcp_otel.erl)

**Module Status:** ✅ COMPLETE  
**Benchmark Status:** ❌ MISSING  
**Expected Performance:** < 5% overhead with OTEL enabled  

**What to Measure:**
```erlang
% Span creation overhead
- Create span:                Target < 10μs
- Add attributes:             Target < 5μs per attribute
- End span:                   Target < 10μs
- Total overhead per request: Target < 50μs (< 5% at 1ms baseline)

% Exporter overhead
- Batch export (100 spans):   Target < 10ms
- OTLP encoding:              Target < 1ms per span
- Network transmission:       Target < 100ms per batch
- Export failure handling:    No main path impact

% Memory overhead
- Span memory:                Target < 1KB per span
- Buffer memory (1K spans):   Target < 1MB
- Exporter queue memory:      Target < 10MB
```

**Workloads Needed:**
- `otel_overhead_1k` - 1K requests with/without OTEL
- `otel_span_creation` - Measure span creation latency
- `otel_attribute_overhead` - 1→100 attributes per span
- `otel_exporter_batch` - 100→10K spans exported
- `otel_failure_resilience` - Exporter failure scenarios

**Regression Baseline Needed:** YES (new feature)

---

### 5. Network Transport Layer

**Module Status:** ✅ BENCHMARK EXISTS  
**Benchmark Status:** 📋 PENDING EXECUTION  
**Expected Performance:** 40-50K concurrent connections per node  

**Benchmark Module:** erlmcp_bench_network_real.erl  
**Workloads Defined:** 7 workloads (TCP/HTTP)  

**What to Measure:**
```erlang
% TCP transport (ranch 2.1.0)
- 100 connections:            Baseline throughput
- 1K connections:             10x throughput
- 10K connections:            100x throughput
- 100K connections:           Target: 40-50K active sustained

% HTTP transport (gun 2.0.1)
- HTTP/1.1 request/response:  Target < 5ms p99
- HTTP/2 multiplexing:        100 streams per connection
- SSE (Server-Sent Events):   1K concurrent streams
- WebSocket (future):         1K concurrent connections

% Message sizes
- Small (100 bytes):          Baseline latency
- Medium (1KB):               Target < 2x small
- Large (10KB):               Target < 10x small
- Huge (1MB):                 Target < 1000x small
```

**Workloads Defined:**
- `tcp_sustained_25k_1kib` - 25K connections (Team tier)
- `tcp_sustained_50k_1kib` - 50K connections (Enterprise tier)
- `tcp_sustained_100k_1kib` - 100K connections (stress test)
- `http_sse_sustained_50k` - 50K SSE streams
- `stdio_sequential_1k` - 1K stdio requests

**Regression Baseline Needed:** YES (v2.0 baseline missing)

---

### 6. Stress Testing Suite

**Module Status:** ✅ BENCHMARK EXISTS  
**Benchmark Status:** 📋 PENDING EXECUTION  
**Expected Performance:** Sustained 372K msg/s for 30 min  

**Benchmark Module:** erlmcp_bench_stress.erl  
**Workloads Defined:** 4 durations (30s→24hr)  

**What to Measure:**
```erlang
% Sustained load
- 30 seconds:                 Baseline throughput
- 5 minutes:                  Throughput degradation < 5%
- 30 minutes:                 Throughput degradation < 10%
- 24 hours:                   Throughput degradation < 15%

% Memory stability
- Initial memory:             Baseline
- After 5 min:                Growth < 5%
- After 30 min:               Growth < 10%
- After 24 hr:                Growth < 20% (no leaks)

% GC behavior
- Minor GC frequency:         Stable over time
- Major GC frequency:         Target < 1/minute
- GC pause time:              Target < 10ms p99
```

**Workloads Defined:**
- `stress_30s_100k_ops` - 30s baseline
- `stress_5min_100k_ops` - 5 minutes
- `stress_30min_100k_ops` - 30 minutes
- `stress_24hr_100k_ops` - 24 hours (optional)

**Regression Baseline Needed:** YES (v2.0 baseline missing)

---

## Baseline Comparison Matrix

### Current Baselines

| Benchmark | v1.5.0 Baseline | v2.0.0 Baseline | v2.1 Target |
|-----------|-----------------|-----------------|-------------|
| **Core Ops (100K)** | 553K msg/s | 2.75M msg/s | ✅ +403% |
| **TCP (25K conn)** | Not measured | ❌ MISSING | Establish |
| **HTTP SSE (50K)** | Not measured | ❌ MISSING | Establish |
| **Stress (5 min)** | 372K msg/s | ❌ MISSING | Verify ±10% |
| **Cache (L1 hits)** | Not applicable | ❌ MISSING | < 1μs |
| **Batch (10K ops)** | Not applicable | ❌ MISSING | 2-5x single |
| **Pool (100 conn)** | Not applicable | ❌ MISSING | 10x vs 10 conn |
| **OTEL overhead** | Not applicable | ❌ MISSING | < 5% |

### Regression Thresholds

| Metric | Threshold | Action |
|--------|-----------|--------|
| **Throughput** | -10% | FAIL - Performance regression |
| **Latency P95** | +10% | FAIL - Latency regression |
| **Latency P99** | +10% | FAIL - Tail latency regression |
| **Memory Delta** | +20% | WARN - Memory increase acceptable |
| **New Feature Overhead** | +5% | WARN - Feature cost acceptable |

---

## Benchmark Development Priority

### Critical (Week 1) - Block v2.1 Release

| Priority | Benchmark | Effort | Impact | Deliverable |
|----------|-----------|--------|--------|-------------|
| 🔴 P0 | **erlmcp_bench_cache** | 4 hours | HIGH | 7 workloads, v2.1 baseline |
| 🔴 P0 | **erlmcp_bench_otel** | 3 hours | HIGH | 5 workloads, overhead measurement |
| 🔴 P0 | **Fix erlmcp_bench_batch** | 2 hours | HIGH | Integration with executor |
| 🔴 P0 | **Fix erlmcp_bench_pool** | 2 hours | HIGH | Integration with pool manager |

**Total Week 1:** 11 hours  
**Outcome:** v2.1 feature benchmarks complete  

---

### High Priority (Week 2) - Establish Baselines

| Priority | Benchmark | Effort | Impact | Deliverable |
|----------|-----------|--------|--------|-------------|
| 🟡 P1 | **Run erlmcp_bench_network_real** | 1 hour | MEDIUM | v2.0 TCP/HTTP baseline |
| 🟡 P1 | **Run erlmcp_bench_stress** | 2 hours | MEDIUM | v2.0 sustained load baseline |
| 🟡 P1 | **Run erlmcp_bench_chaos** | 1 hour | LOW | v2.0 failure recovery baseline |
| 🟡 P1 | **Run erlmcp_bench_integration** | 1 hour | MEDIUM | v2.0 MCP e2e baseline |

**Total Week 2:** 5 hours  
**Outcome:** Complete v2.0 baseline coverage  

---

### Medium Priority (Week 3) - Automation & CI/CD

| Priority | Task | Effort | Impact | Deliverable |
|----------|------|--------|--------|-------------|
| 🟢 P2 | **Regression detection script** | 2 hours | HIGH | Auto-detect regressions |
| 🟢 P2 | **CI/CD integration** | 3 hours | HIGH | GitHub Actions workflow |
| 🟢 P2 | **Baseline update workflow** | 1 hour | MEDIUM | Script + git commit |
| 🟢 P2 | **Performance dashboard** | 4 hours | LOW | Grafana/Prometheus |

**Total Week 3:** 10 hours  
**Outcome:** Automated performance monitoring  

---

## Implementation Guide

### Step 1: Create erlmcp_bench_cache.erl

```erlang
%%%====================================================================
%%% ERLMCP CACHE BENCHMARK - Measure 10-100x Hit Performance
%%%====================================================================
-module(erlmcp_bench_cache).

-export([run/0, run/1, run_all/0, workloads/0]).

workloads() ->
    [
        #{id => <<"cache_l1_hits_100k">>, operations => 100000, hit_rate => 1.0, level => l1},
        #{id => <<"cache_l2_hits_10k">>, operations => 10000, hit_rate => 1.0, level => l2},
        #{id => <<"cache_mixed_10k">>, operations => 10000, hit_rate => 0.7, level => l1},
        #{id => <<"cache_ttl_expiration_1k">>, operations => 1000, ttl => 1, level => l1},
        #{id => <<"cache_lru_eviction_10k">>, operations => 10000, max_size => 1000, level => l1},
        #{id => <<"cache_tag_invalidation">>, operations => 1000, tags => 10, level => l1},
        #{id => <<"cache_cluster_coherence">>, operations => 1000, nodes => 2, level => l2}
    ].

run_workload(#{id := WorkloadId, operations := Ops, hit_rate := HitRate, level := Level}) ->
    % 1. Pre-populate cache
    % 2. Measure hit latency (p50/p95/p99)
    % 3. Measure miss latency
    % 4. Calculate hit rate actual vs expected
    % 5. Measure memory per entry
    % 6. Write metrology-compliant JSON
    ok.
```

**Test Cases:**
- L1 cache hits < 1μs p99
- L2 cache hits < 10μs p99
- Mixed (70% hit) throughput > 1M msg/s
- TTL cleanup < 5% CPU overhead
- LRU eviction < 100μs per eviction
- Tag invalidation < 10ms for 1K entries
- Cluster coherence < 100ms propagation

---

### Step 2: Create erlmcp_bench_otel.erl

```erlang
%%%====================================================================
%%% ERLMCP OBSERVABILITY BENCHMARK - Measure OTEL Overhead
%%%====================================================================
-module(erlmcp_bench_otel).

-export([run/0, run/1, run_all/0, workloads/0]).

workloads() ->
    [
        #{id => <<"otel_overhead_1k">>, operations => 1000, otel_enabled => true},
        #{id => <<"otel_baseline_1k">>, operations => 1000, otel_enabled => false},
        #{id => <<"otel_span_creation">>, operations => 10000, span_only => true},
        #{id => <<"otel_attribute_overhead">>, operations => 1000, attributes => 100},
        #{id => <<"otel_exporter_batch">>, operations => 10000, batch_size => 100}
    ].

run_workload(#{id := WorkloadId, operations := Ops, otel_enabled := Enabled}) ->
    % 1. Run operations with OTEL enabled/disabled
    % 2. Measure throughput difference
    % 3. Calculate overhead percentage
    % 4. Measure span creation latency
    % 5. Measure memory overhead
    % 6. Write metrology-compliant JSON
    ok.
```

**Test Cases:**
- OTEL overhead < 5% (50μs on 1ms baseline)
- Span creation < 10μs p99
- Attribute addition < 5μs per attribute
- Batch export < 10ms for 100 spans
- Memory overhead < 1KB per span

---

### Step 3: Fix erlmcp_bench_batch.erl

**Current Issue:**
```erlang
% Line 190: erlmcp_batch:start_link/2 doesn't exist
{ok, Batcher} = erlmcp_batch:start_link(Executor, #{...}),
```

**Fix:**
1. Check actual erlmcp_batch API
2. Replace with correct start_link signature
3. Integrate with real executor (not mock)

**Expected API:**
```erlang
% From apps/erlmcp_core/src/erlmcp_batch.erl
-export([start_link/1, add_request/3, flush/1, get_stats/1, stop/1]).

% Fix:
{ok, Batcher} = erlmcp_batch:start_link(#{
    executor => fun batch_executor/1,
    strategy => Strategy,
    parallel_workers => 4
}),
```

---

### Step 4: Fix erlmcp_bench_pool.erl

**Current Issue:**
```erlang
% Line 143: erlmcp_pool_manager API mismatch
{ok, Pool} = erlmcp_pool_manager:start_link(#{...}),
```

**Fix:**
1. Check actual erlmcp_pool_manager API
2. Replace with correct supervisor start
3. Integrate with real transport layer

**Expected API:**
```erlang
% From apps/erlmcp_transports/src/erlmcp_pool_manager.erl
-export([start_pool/1, stop_pool/1, checkout/1, checkin/2]).

% Fix:
{ok, Pool} = erlmcp_pool_manager:start_pool(#{
    name => test_pool,
    size => PoolSize,
    max_overflow => Overflow,
    strategy => Strategy,
    transport => erlmcp_transport_tcp
}),
```

---

## Testing & Validation Checklist

### Compilation
- [ ] `TERM=dumb rebar3 compile` - 0 errors, 0 warnings

### Unit Tests
- [ ] `rebar3 eunit --module=erlmcp_bench_cache_tests`
- [ ] `rebar3 eunit --module=erlmcp_bench_otel_tests`
- [ ] `rebar3 eunit --module=erlmcp_bench_batch_tests`
- [ ] `rebar3 eunit --module=erlmcp_bench_pool_tests`

### Benchmark Execution
- [ ] `erlmcp_bench_cache:run_all()` - 7 workloads pass
- [ ] `erlmcp_bench_otel:run_all()` - 5 workloads pass
- [ ] `erlmcp_bench_batch:run_all()` - 3 workloads pass
- [ ] `erlmcp_bench_pool:run_all()` - 5 workloads pass
- [ ] `erlmcp_bench_network_real:run_all()` - 7 workloads pass
- [ ] `erlmcp_bench_stress:run_all()` - 4 workloads pass

### Metrology Compliance
- [ ] All results include `workload_id`
- [ ] All throughput metrics use `msg_per_s`
- [ ] All latency metrics use `_us` or `_ms` suffix
- [ ] All memory metrics use `_mib` or `_gib` suffix
- [ ] All metrics include `scope` (per_node, per_conn)
- [ ] All metrics include `precision` (microsecond, millisecond)

### Baseline Establishment
- [ ] Create `bench/baselines/2026-02-XX_v2.1.0.json`
- [ ] Document baseline in `bench/baselines/V2.1.0_BASELINE_REPORT.md`
- [ ] Update `bench/baselines/README.md` with v2.1 entry

### Regression Detection
- [ ] Run regression script: `./scripts/bench/compare_to_baseline.sh`
- [ ] Verify thresholds: -10% throughput, +10% latency, +20% memory
- [ ] Test CI/CD integration: `.github/workflows/benchmark.yml`

---

## Performance Targets Summary

### v2.1 New Features

| Feature | Metric | Target | Baseline | Status |
|---------|--------|--------|----------|--------|
| **Cache L1 hits** | Latency p99 | < 1μs | N/A | ❌ NO BENCHMARK |
| **Cache L2 hits** | Latency p99 | < 10μs | N/A | ❌ NO BENCHMARK |
| **Batch (size=10)** | Throughput | 2-5x single | N/A | ⚠️ BROKEN |
| **Pool (100 conn)** | Throughput | 10x vs 10 | N/A | ⚠️ BROKEN |
| **OTEL overhead** | Overhead % | < 5% | N/A | ❌ NO BENCHMARK |

### Core System (Existing)

| Component | Metric | Target | v2.0.0 Baseline | Status |
|-----------|--------|--------|-----------------|--------|
| **Registry** | Throughput | > 500K msg/s | 1.94M msg/s | ✅ BASELINE |
| **Queue** | Latency p99 | < 10μs | 1μs | ✅ BASELINE |
| **Pool** | Latency p99 | < 10μs | 1μs | ✅ BASELINE |
| **Session** | Latency p99 | < 100μs | 108μs | ✅ BASELINE |
| **TCP (25K)** | Throughput | > 10K conn/s | ❌ MISSING | 📋 PENDING |
| **HTTP SSE** | Throughput | > 1K streams/s | ❌ MISSING | 📋 PENDING |
| **Stress (5min)** | Degradation | < 10% | ❌ MISSING | 📋 PENDING |

---

## Regression Detection Strategy

### Automated CI/CD Gates

```yaml
# .github/workflows/benchmark.yml
- name: Run Benchmarks
  run: |
    make benchmark-quick
    ./scripts/bench/compare_to_baseline.sh \
      bench/baselines/2026-01-28_v2.0.0.json \
      bench/results/core_ops_*.json

- name: Check Regression
  run: |
    REGRESSION=$(./scripts/bench/detect_regression.sh)
    if [ "$REGRESSION" = "true" ]; then
      echo "❌ Performance regression detected"
      exit 1
    fi
```

### Baseline Update Workflow

```bash
# scripts/bench/set_baseline.sh
#!/bin/bash
set -euo pipefail

# 1. Run full benchmark suite
make benchmark-full

# 2. Prompt for justification
echo "Updating baseline. Justification:"
read -r REASON

# 3. Copy latest results to baseline
cp bench/results/core_ops_core_ops_100k_*.json \
   bench/baselines/$(date +%Y-%m-%d)_v2.1.0.json

# 4. Generate baseline report
./scripts/bench/generate_baseline_report.sh

# 5. Git commit with justification
git add bench/baselines/
git commit -m "Update performance baseline: $REASON"
```

---

## Expected Performance Improvements (v2.0 → v2.1)

### With Cache Enabled (70% hit rate)

| Operation | v2.0 Latency | v2.1 Cached | Improvement |
|-----------|--------------|-------------|-------------|
| **Resource lookup** | 50μs | 1μs | **50x faster** |
| **Tool metadata** | 100μs | 5μs | **20x faster** |
| **Prompt templates** | 200μs | 10μs | **20x faster** |

**Overall throughput:** 2.75M → 10M+ msg/s (3-4x improvement)

---

### With Batch Enabled (batch_size=10)

| Workload | v2.0 Throughput | v2.1 Batched | Improvement |
|----------|-----------------|--------------|-------------|
| **1K ops** | 1.4M msg/s | 3.5M msg/s | **2.5x faster** |
| **10K ops** | 2.8M msg/s | 11M msg/s | **4x faster** |
| **100K ops** | 2.7M msg/s | 10M msg/s | **3.7x faster** |

**Overall throughput:** 2.75M → 8-11M msg/s (3-4x improvement)

---

### With Pool Enabled (100 connections)

| Connections | v2.0 Throughput | v2.1 Pooled | Improvement |
|-------------|-----------------|-------------|-------------|
| **10 conn** | 275K msg/s | 275K msg/s | Baseline |
| **100 conn** | 2.75M msg/s | 5.5M msg/s | **2x faster** |
| **1000 conn** | 27.5M msg/s | 55M msg/s | **2x faster** |

**Overall throughput:** 2.75M → 5.5M msg/s (2x improvement)

---

### Combined (Cache + Batch + Pool)

**Theoretical Maximum:** 2.75M × 3 × 3 × 2 = **49.5M msg/s**  
**Realistic Maximum:** 2.75M × 2 × 2.5 × 1.5 = **20.6M msg/s**  

**Expected v2.1 Baseline:** 10-20M msg/s (4-7x improvement over v2.0)

---

## Risk Assessment

### High Risk

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **Batch executor missing** | HIGH | BLOCK v2.1 | Implement mock executor for benchmarks |
| **Pool manager API change** | MEDIUM | BLOCK v2.1 | Verify API, update benchmark |
| **Cache regression** | MEDIUM | DELAY v2.1 | Thorough testing, unit tests |

### Medium Risk

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **OTEL overhead > 5%** | LOW | DELAY v2.1 | Optimize span creation, lazy export |
| **Network baseline variance** | MEDIUM | NONE | Multiple runs, average results |
| **Stress test instability** | LOW | NONE | Longer warmup, GC tuning |

### Low Risk

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **Metrology violations** | LOW | NONE | Automated validation |
| **CI/CD integration issues** | LOW | NONE | Test locally first |

---

## Success Criteria

### Minimum Viable Baseline (MVP)

- [x] Core ops baseline (v2.0) - **COMPLETE**
- [ ] Cache benchmark (7 workloads) - **CRITICAL**
- [ ] Batch benchmark (3 workloads) - **CRITICAL**
- [ ] Pool benchmark (5 workloads) - **CRITICAL**
- [ ] OTEL benchmark (5 workloads) - **CRITICAL**
- [ ] Network baseline (7 workloads) - **HIGH**
- [ ] Stress baseline (4 workloads) - **HIGH**

**Target:** 20/25 workloads complete (80%)  
**Timeline:** 2 weeks  

---

### Complete Baseline (v2.1 Release)

- [ ] All 9 benchmark modules working
- [ ] All 30+ workloads passing
- [ ] All baselines established
- [ ] Regression detection automated
- [ ] CI/CD integration complete
- [ ] Performance dashboard deployed

**Target:** 100% completion  
**Timeline:** 4 weeks  

---

## Next Steps

### Week 1 (2026-01-28 → 2026-02-04)

**Monday-Tuesday:**
1. Create `erlmcp_bench_cache.erl` (4 hours)
2. Create `erlmcp_bench_otel.erl` (3 hours)
3. Write unit tests for both (2 hours)

**Wednesday-Thursday:**
4. Fix `erlmcp_bench_batch.erl` (2 hours)
5. Fix `erlmcp_bench_pool.erl` (2 hours)
6. Integration testing (2 hours)

**Friday:**
7. Run all new benchmarks (2 hours)
8. Generate v2.1 baseline report (1 hour)
9. Document findings (1 hour)

**Total:** 19 hours (2.5 days)

---

### Week 2 (2026-02-04 → 2026-02-11)

**Monday:**
1. Run `erlmcp_bench_network_real` (1 hour)
2. Run `erlmcp_bench_stress` (2 hours)
3. Generate baselines (1 hour)

**Tuesday-Wednesday:**
4. Create regression detection script (2 hours)
5. Integrate with CI/CD (3 hours)
6. Test automation end-to-end (2 hours)

**Thursday-Friday:**
7. Create baseline update workflow (1 hour)
8. Generate comprehensive baseline report (2 hours)
9. Documentation and training (2 hours)

**Total:** 16 hours (2 days)

---

## Appendix: Benchmark Module Template

```erlang
%%%====================================================================
%%% ERLMCP BENCHMARK TEMPLATE - Copy for New Benchmarks
%%%====================================================================
-module(erlmcp_bench_FEATURE).

-export([run/0, run/1, run_all/0, workloads/0]).

-include_lib("kernel/include/logger.hrl").

%% Workload definitions
-spec workloads() -> [map()].
workloads() ->
    [
        #{id => <<"workload_1">>, operations => 1000},
        #{id => <<"workload_2">>, operations => 10000},
        #{id => <<"workload_3">>, operations => 100000}
    ].

%% Main entry points
-spec run() -> ok.
run() -> run_all().

-spec run(binary()) -> ok | {error, term()}.
run(WorkloadId) when is_binary(WorkloadId) ->
    case lists:filter(fun(#{id := Id}) -> Id =:= WorkloadId end, workloads()) of
        [] -> {error, {unknown_workload, WorkloadId}};
        [Workload] -> run_workload(Workload)
    end.

-spec run_all() -> ok.
run_all() ->
    io:format("~n=== ERLMCP FEATURE BENCHMARK ===~n~n"),
    lists:foreach(fun(W) -> run_workload(W) end, workloads()),
    io:format("~n=== COMPLETE ===~n~n"),
    ok.

%% Run a single workload
-spec run_workload(map()) -> ok | {error, term()}.
run_workload(#{id := WorkloadId, operations := Ops}) ->
    io:format("Running workload: ~s (~p ops)~n", [WorkloadId, Ops]),
    
    % 1. Capture environment
    Env = capture_environment(),
    
    % 2. Run benchmark
    StartTime = erlang:monotonic_time(microsecond),
    MemoryBefore = erlang:memory(total),
    
    % TODO: Actual benchmark logic here
    Results = benchmark_logic(Ops),
    
    EndTime = erlang:monotonic_time(microsecond),
    MemoryAfter = erlang:memory(total),
    
    % 3. Calculate metrics
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1_000_000,
    Throughput = Ops / DurationS,
    MemoryDelta = (MemoryAfter - MemoryBefore) / (1024 * 1024),
    
    % 4. Build report (METROLOGY COMPLIANT)
    Report = #{
        workload_id => WorkloadId,
        benchmark => <<"feature_name">>,
        timestamp => erlang:system_time(second),
        environment => Env,
        operations => Ops,
        duration_s => round_float(DurationS, 2),
        throughput_msg_per_s => round_float(Throughput, 2),
        latency_p50_us => round_float(maps:get(p50, Results), 1),
        latency_p95_us => round_float(maps:get(p95, Results), 1),
        latency_p99_us => round_float(maps:get(p99, Results), 1),
        memory_delta_mib => round_float(MemoryDelta, 1),
        precision => <<"microsecond">>,
        scope => <<"per_node">>
    },
    
    % 5. Write to file
    Timestamp = erlang:system_time(second),
    Filename = io_lib:format("bench/results/feature_~s_~p.json", [WorkloadId, Timestamp]),
    write_report(Filename, Report),
    
    % 6. Print summary
    io:format("✓ ~s: ~.2f msg/s, p99=~.1fμs, memory=~.1f MiB~n",
        [WorkloadId, Throughput, maps:get(p99, Results), MemoryDelta]),
    
    ok.

%% Helpers
capture_environment() ->
    {OtpRelease, _} = string:to_integer(erlang:system_info(otp_release)),
    #{
        otp_release => OtpRelease,
        schedulers => erlang:system_info(schedulers_online),
        os => list_to_binary(atom_to_list(element(1, os:type()))),
        hostname => list_to_binary(net_adm:localhost())
    }.

round_float(Float, Decimals) when is_float(Float) ->
    list_to_float(io_lib:format("~.*f", [Decimals, Float]));
round_float(Int, _) when is_integer(Int) ->
    float(Int).

write_report(Filename, Report) ->
    filelib:ensure_dir(Filename),
    Json = jsx:encode(Report, [space, indent]),
    file:write_file(Filename, Json).
```

---

**End of Benchmark Completion Plan**  
**Maintained By:** Performance Engineering Team  
**Contact:** performance@erlmcp.io  
**Version:** v1.0 (2026-01-28)
