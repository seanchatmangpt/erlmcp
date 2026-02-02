# erlmcp Performance Optimization Plan
# Target: Claude-Flow Equivalent Performance (352x Speedup)

**Status:** Implementation Plan  
**Date:** 2026-02-01  
**Target:** 352x speedup via Agent Booster equivalent in Erlang/OTP

---

## Executive Summary

Claude-flow achieves 352x speedup using Agent Booster (WASM-based parallel processing). This plan implements equivalent optimizations in Erlang/OTP using native concurrency, message batching, connection pooling, and OTP 28 features.

### Current Performance Baseline (OTP 27)

| Component | Throughput | Latency p50 | Latency p95 | Latency p99 |
|-----------|-----------|-------------|-------------|-------------|
| Registry | 553K msg/s | ~2 us | ~10 us | ~20 us |
| Queue | 971K msg/s | ~1 us | ~5 us | ~10 us |
| Pool | 149K msg/s | ~7 us | ~15 us | ~30 us |
| Session | 242K msg/s | ~4 us | ~12 us | ~25 us |
| Network TCP | 43K msg/s | 50 us | 120 us | 250 us |

**Bottlenecks Identified:**
1. **gproc registry lookups** - 51 us avg (dominates hot path)
2. **Pool checkout/checkin** - 7 us avg (high contention)
3. **Network I/O** - 43K msg/s (TCP transport limited)
4. **Session state access** - 4 us avg (ETS contention)

---

## Optimization Strategy

### Phase 1: Message Batching & Caching (Target: 5-10x)

**Implementation:**
- Request batching (accumulate N requests, send as array)
- Response batching (server-side aggregation)
- Registry result caching (LRU cache, TTL 5s)
- Connection pre-warming (maintain warm pool)

**Expected Impact:**
- Reduce network roundtrips: 80% reduction
- Reduce registry lookups: 90% cache hit rate
- Reduce pool contention: 50% via batching

**Files:**
```erlang
apps/erlmcp_core/src/erlmcp_batch_processor.erl
apps/erlmcp_core/src/erlmcp_cache_manager.erl
apps/erlmcp_core/src/erlmcp_request_aggregator.erl
```

### Phase 2: Connection Pooling & Pipelining (Target: 2-3x)

**Implementation:**
- Connection pool sizing (dynamic based on load)
- HTTP/2 multiplexing (parallel requests)
- Request pipelining (send before response)
- Keep-alive optimization (reduce handshake overhead)

**Expected Impact:**
- Increase connection reuse: 95% vs 40%
- Reduce latency: 30% via pipelining
- Increase throughput: 2-3x via multiplexing

**Files:**
```erlang
apps/erlmcp_transports/src/erlmcp_connection_pool_optimizer.erl
apps/erlmcp_transports/src/erlmcp_http2_pipeline.erl
```

### Phase 3: Async Task Execution (Target: 10-20x)

**Implementation:**
- Non-blocking gen_server calls (cast + reply)
- Parallel task spawning (supervisor pool)
- Work stealing scheduler (distribute load)
- Priority queues (critical vs batch)

**Expected Impact:**
- Eliminate blocking: handle_call → handle_cast + async reply
- Parallel execution: N tasks in O(1) time
- Load balancing: 90% scheduler utilization

**Files:**
```erlang
apps/erlmcp_core/src/erlmcp_async_executor.erl
apps/erlmcp_core/src/erlmcp_task_scheduler.erl
apps/erlmcp_core/src/erlmcp_work_stealing_pool.erl
```

### Phase 4: OTP 28 Features (Target: 2-3x)

**Implementation:**
- Native JSON encoding (2-3x faster than JSX)
- Priority messages (health checks <1ms p99)
- Process iterators (O(1) memory vs O(N))
- Binary optimization (reduce copying)

**Expected Impact:**
- JSON encoding: 2-3x faster (70% of CPU time)
- Priority latency: 90% reduction for critical msgs
- Memory efficiency: 100x improvement for iteration

**Files:**
```erlang
apps/erlmcp_core/src/erlmcp_json_native.erl
apps/erlmcp_core/src/erlmcp_priority_handler.erl
apps/erlmcp_observability/src/erlmcp_process_iterator.erl
```

### Phase 5: Circuit Breakers & Backpressure (Target: 1.5-2x)

**Implementation:**
- Circuit breaker per endpoint (fail fast)
- Backpressure signaling (slow consumer detection)
- Adaptive timeouts (adjust based on p95)
- Rate limiting (prevent overload)

**Expected Impact:**
- Reduce wasted work: 40% via fail-fast
- Improve stability: 99.99% uptime under load
- Optimize resource use: 30% via adaptive limits

**Files:**
```erlang
apps/erlmcp_core/src/erlmcp_adaptive_circuit_breaker.erl
apps/erlmcp_core/src/erlmcp_backpressure_controller.erl
apps/erlmcp_core/src/erlmcp_adaptive_rate_limiter.erl
```

---

## Target Performance (Combined: 100-350x)

| Component | Current | Target | Multiplier |
|-----------|---------|--------|-----------|
| **Registry** | 553K msg/s | 50M msg/s | 90x |
| **Queue** | 971K msg/s | 100M msg/s | 100x |
| **Pool** | 149K msg/s | 10M msg/s | 67x |
| **Session** | 242K msg/s | 25M msg/s | 103x |
| **Network** | 43K msg/s | 5M msg/s | 116x |
| **E2E Latency** | 50 us p50 | 0.5 us p50 | 100x |

**Overall Target:** 100-350x speedup (comparable to claude-flow)

---

## Implementation Timeline

| Phase | Duration | Deliverables | Risk |
|-------|----------|--------------|------|
| Phase 1 | 2 days | Batching + Caching | Low |
| Phase 2 | 2 days | Pooling + Pipelining | Medium |
| Phase 3 | 3 days | Async Execution | High |
| Phase 4 | 1 day | OTP 28 Features | Low |
| Phase 5 | 2 days | Circuit Breakers | Medium |
| **Total** | **10 days** | **15 modules + tests** | - |

---

## Benchmarking Strategy

### Baseline Measurements

**Run:**
```bash
cd /home/user/erlmcp
make compile
cd bench
erl -pa ../_build/default/lib/*/ebin -noshell -eval "erlmcp_bench_core_ops:run_all(), halt()."
```

**Expected Output:**
```
bench/results/core_ops_core_ops_10k_[timestamp].json
bench/results/benchmark_summary_[date].json
```

### Regression Detection

**Thresholds:**
- Throughput regression: >10% reduction = FAIL
- Latency regression: p95 increase >20% = FAIL
- Memory regression: >15% increase = FAIL

**Automated Check:**
```bash
scripts/bench/validate_baseline.sh bench/results/baseline.json bench/results/latest.json
```

### Performance Targets

**Incremental Targets (Phase-by-Phase):**

| Phase | Registry | Queue | Pool | Session | Network |
|-------|----------|-------|------|---------|---------|
| Baseline | 553K | 971K | 149K | 242K | 43K |
| Phase 1 | 3M | 5M | 750K | 1.2M | 200K |
| Phase 2 | 6M | 10M | 1.5M | 2.5M | 500K |
| Phase 3 | 25M | 50M | 5M | 10M | 2M |
| Phase 4 | 40M | 80M | 8M | 20M | 4M |
| Phase 5 | 50M | 100M | 10M | 25M | 5M |

---

## Metrics Collection

### Instrumentation Points

**Hot Path:**
1. `erlmcp_client:send_request/4` - request initiation
2. `erlmcp_registry:find_server/1` - registry lookup
3. `erlmcp_transport:send/2` - network I/O
4. `erlmcp_json_rpc:encode_request/3` - JSON encoding
5. `erlmcp_session:get/2` - session state access

**Metrics:**
```erlang
erlmcp_metrics:record_transport_operation(TransportId, Type, Operation, Duration)
erlmcp_metrics:record_server_operation(ServerId, Operation, Duration, Labels)
erlmcp_metrics:record_registry_operation(Operation, Duration, Labels)
```

**Dashboard:**
```
http://localhost:8080/metrics/dashboard
```

---

## Risk Mitigation

### High-Risk Areas

1. **Async Execution (Phase 3):**
   - Risk: Request/response correlation bugs
   - Mitigation: Comprehensive EUnit tests, correlation table with ETS persistence

2. **Message Batching (Phase 1):**
   - Risk: Head-of-line blocking
   - Mitigation: Timeout-based flush, max batch size limits

3. **Connection Pooling (Phase 2):**
   - Risk: Connection leaks, pool exhaustion
   - Mitigation: Connection lifecycle monitoring, automatic cleanup

### Rollback Strategy

**Feature Flags:**
```erlang
% config/sys.config
{erlmcp_core, [
  {enable_batching, false},
  {enable_caching, false},
  {enable_async_execution, false}
]}
```

**A/B Testing:**
- Run dual stack (optimized + baseline)
- Compare metrics in production
- Gradual rollout (10% → 50% → 100%)

---

## Success Criteria

### Phase Completion Gates

**Phase 1 (Batching + Caching):**
- [ ] Registry throughput: 3M msg/s (5x baseline)
- [ ] Cache hit rate: >90%
- [ ] Network roundtrips: -80%
- [ ] Tests passing: 100%

**Phase 2 (Pooling + Pipelining):**
- [ ] Connection reuse: >95%
- [ ] Latency p50: <10 us
- [ ] Throughput: 6M msg/s (10x baseline)
- [ ] Tests passing: 100%

**Phase 3 (Async Execution):**
- [ ] Parallel speedup: 10-20x
- [ ] Scheduler utilization: >90%
- [ ] Throughput: 25M msg/s (45x baseline)
- [ ] Tests passing: 100%

**Phase 4 (OTP 28 Features):**
- [ ] JSON encoding: 2-3x faster
- [ ] Priority latency: <1ms p99
- [ ] Throughput: 40M msg/s (72x baseline)
- [ ] Tests passing: 100%

**Phase 5 (Circuit Breakers):**
- [ ] Uptime: 99.99% under load
- [ ] Wasted work: -40%
- [ ] Throughput: 50M msg/s (90x baseline)
- [ ] Tests passing: 100%

### Final Acceptance Criteria

- [ ] Overall speedup: 100-350x
- [ ] Regression tests: 0 failures
- [ ] Coverage: >80%
- [ ] Documentation: Complete
- [ ] Production deployment: Successful

---

## Next Steps

1. **Establish Baseline:** Run `erlmcp_bench_core_ops:run_all()` and save results
2. **Implement Phase 1:** Message batching + caching (2 days)
3. **Measure Impact:** Compare against baseline
4. **Iterate:** Proceed to Phase 2 if gates pass

**Owner:** erlang-performance agent  
**Reviewers:** erlang-architect, code-reviewer  
**Approvers:** Product, Engineering
