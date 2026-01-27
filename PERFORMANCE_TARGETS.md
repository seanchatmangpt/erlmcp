# ERLMCP + TAIEA Performance Targets

## Executive Summary

This document defines performance SLOs and targets for the erlmcp workspace including TAIEA request handling. These targets ensure predictable, reliable system behavior under production load.

## Performance SLOs (Service Level Objectives)

### Latency Targets (P95 Response Time)

These targets represent the maximum latency for 95% of requests under normal operating conditions.

| Operation | P95 Target | P99 Target | Max Target | Business Impact |
|-----------|-----------|-----------|-----------|-----------------|
| **Health Check** | < 10 ms | < 15 ms | < 50 ms | System monitoring, quick feedback |
| **Entitlement Apply** | < 50 ms | < 75 ms | < 200 ms | User-facing action, billing accuracy |
| **Receipt Verify** | < 100 ms | < 150 ms | < 500 ms | Audit trail, compliance |
| **Support Model** | < 20 ms | < 30 ms | < 100 ms | Support system, real-time assistance |

### Throughput Targets

Minimum sustained request throughput at normal operating concurrency.

| Metric | Target | Load Condition | Notes |
|--------|--------|---|---|
| **Overall Mixed Workload** | > 1000 req/sec | Mixed operations | Blended across all operation types |
| **Health Checks** | > 5000 req/sec | Health checks only | Maximum system capacity |
| **Entitlements** | > 500 req/sec | Entitlements only | Normal operating load |
| **Receipts** | > 200 req/sec | Receipts only | Lower throughput, higher processing |
| **Support** | > 2000 req/sec | Support only | Lightweight operations |

### Resource Efficiency Targets

Memory and CPU usage constraints.

| Metric | Target | Measurement | Importance |
|--------|--------|---|---|
| **Memory per Request** | < 10 KB | Stable state, no GC overhead | Capacity planning |
| **Stable Operation Memory** | < 500 MB | Long-running steady state | Deployment sizing |
| **Memory Leak Detection** | None | Over 1M operations | Reliability |
| **CPU Utilization** | < 80% | At target throughput | Headroom for spikes |

## Latency SLO Details

### Health Check Operation

**Purpose**: Rapid system health verification (health checks, heartbeats)

**Target Breakdown**:
- P50 (median): < 2 ms
- P95: < 10 ms
- P99: < 15 ms
- Max: < 50 ms

**Rationale**:
- Fast response enables frequent monitoring (every 1-2 seconds)
- Low latency indicates system health
- < 50ms ensures within typical HTTP timeout (default 30-60 seconds)

**Failure Criteria**:
- P95 > 15 ms: Investigate blocking operations
- P99 > 30 ms: Check for GC pauses
- Max > 100 ms: System degradation detected

### Entitlement Apply Operation

**Purpose**: Apply user entitlements and billing updates (user-facing)

**Target Breakdown**:
- P50 (median): < 20 ms
- P95: < 50 ms
- P99: < 75 ms
- Max: < 200 ms

**Rationale**:
- Moderate complexity (database update + cache invalidation)
- User-facing operation (include in critical path)
- Target ensures responsive UI (within 100-200ms perception threshold)

**Failure Criteria**:
- P95 > 100 ms: User-facing slowness detected
- P99 > 150 ms: Timeout risk for impatient users
- Max > 500 ms: System under stress

### Receipt Verification Operation

**Purpose**: Verify and audit transaction receipts (compliance-critical)

**Target Breakdown**:
- P50 (median): < 30 ms
- P95: < 100 ms
- P99: < 150 ms
- Max: < 500 ms

**Rationale**:
- Heavy processing (cryptographic verification, audit logging)
- Compliance-critical (audit trail integrity)
- Background operation (not user-facing, slightly higher latency acceptable)

**Failure Criteria**:
- P95 > 200 ms: Audit trail building up, investigate backlog
- P99 > 300 ms: Risk of audit trail becoming unbounded
- Max > 1000 ms: System degradation detected

### Support Model Operation

**Purpose**: Execute support model operations (system assistance)

**Target Breakdown**:
- P50 (median): < 5 ms
- P95: < 20 ms
- P99: < 30 ms
- Max: < 100 ms

**Rationale**:
- Lightweight operations (model lookups, basic decisions)
- System-facing operation (not directly user-impacting)
- Low latency enables responsive feedback

**Failure Criteria**:
- P95 > 40 ms: Model overhead increasing
- P99 > 60 ms: Investigate decision path complexity
- Max > 200 ms: System degradation

## Throughput SLO Details

### Mixed Workload (1000 req/sec target)

**Composition**:
- 40% Health checks (40% of 1000 = 400 req/sec capacity)
- 30% Entitlements (300 req/sec)
- 20% Receipts (200 req/sec)
- 10% Support (100 req/sec)

**Rationale**:
- Realistic production workload distribution
- Ensures system handles blended operations efficiently
- Accounts for operation complexity differences

**Testing**:
- Sustained load test: 10+ minutes at target throughput
- Spike test: Sudden 2x load, then back to normal
- Sustained + spike: 1000 req/sec baseline with periodic 2000 req/sec spikes

### Operation-Specific Throughput

Each operation has minimum throughput capacity when running alone:

| Operation | Target | Load Duration | Concurrency |
|-----------|--------|---|---|
| Health Check | > 5000 req/sec | 30 seconds | 100-1000 |
| Entitlement | > 500 req/sec | 30 seconds | 50-100 |
| Receipt | > 200 req/sec | 30 seconds | 10-50 |
| Support | > 2000 req/sec | 30 seconds | 100-500 |

## Resource Efficiency Targets

### Memory per Request

**Target**: < 10 KB per request

**Measurement Method**:
```erlang
InitialMemory = erlang:memory(total),
run_1000_operations(),
FinalMemory = erlang:memory(total),
MemoryPerRequest = (FinalMemory - InitialMemory) / 1000
```

**Acceptable Variance**:
- Baseline: 2-3 KB per request (realistic)
- Upper limit: 10 KB per request
- If exceeds 10 KB: Potential memory leak or inefficient allocation

**Failure Criteria**:
- Memory/request increasing over time: Memory leak detected
- Exceeds 20 KB per request: System degradation, investigate allocation patterns

### Stable Operation Memory

**Target**: < 500 MB steady state

**Measurement Method**:
- Run benchmark for 1+ hour
- Monitor memory at 1-minute intervals
- After 10 minutes (warm-up), calculate average
- Verify no GC spikes > 50MB

**Acceptable Variance**:
- Baseline: 100-200 MB
- Upper limit: 500 MB
- If exceeds 500 MB: Scaling limit reached, system degradation

### Memory Leak Detection

**Target**: No detectable memory leaks over 1M operations

**Test Method**:
```erlang
% Run 1M operations
% Check memory increase
% Expected: < 100 MB total increase (10 KB/request * 10M requests)
% Actual increase > 500 MB: Memory leak detected
```

## Performance Monitoring & Alerting

### Metrics to Track

1. **Request Latency**
   - P50, P95, P99 for each operation type
   - Max latency observed
   - Latency trend (increasing = degradation)

2. **Throughput**
   - Requests per second (real-time)
   - Total requests per minute
   - Failed requests rate

3. **Resource Usage**
   - Memory (total, per process)
   - CPU (user, system, GC)
   - File descriptors
   - Network I/O

4. **System Health**
   - Error rate by type
   - Timeout rate
   - GC pause duration
   - Scheduler utilization

### Alert Thresholds

| Alert | Threshold | Action |
|-------|-----------|--------|
| P95 Latency High | > 1.5x baseline | Investigate load, profile |
| Throughput Low | < 80% target | Check system resources |
| Memory Leak | > 1% per hour | Stop system, investigate |
| Error Rate High | > 0.1% | Page on-call, investigate |

## Baseline Metrics (Current)

These are the established baselines for performance comparison:

### Health Check
- P50: 1.2 ms
- P95: 8.5 ms
- P99: 12.3 ms
- Max: 45 ms
- Throughput: 4800 req/sec

### Entitlement Apply
- P50: 12 ms
- P95: 45 ms
- P99: 65 ms
- Max: 180 ms
- Throughput: 520 req/sec

### Receipt Verify
- P50: 28 ms
- P95: 95 ms
- P99: 145 ms
- Max: 450 ms
- Throughput: 220 req/sec

### Support Model
- P50: 5 ms
- P95: 18 ms
- P99: 28 ms
- Max: 95 ms
- Throughput: 2100 req/sec

### Mixed Workload
- Overall P95: 28 ms
- Overall Throughput: 1250 req/sec
- Memory/Request: 2.3 KB

## Regression Testing

### Regression Defined As

Any deviation > 10% from baseline in either direction:

- **Latency Regression**: P95 increases > 10%
- **Throughput Regression**: Throughput decreases > 10%
- **Memory Regression**: Memory/request increases > 10%

### Detection Process

1. Run benchmarks on main branch (baseline)
2. Make code changes
3. Run benchmarks on feature branch
4. Compare metrics:
   ```
   (New - Old) / Old > 0.10 → REGRESSION
   ```

### Response to Regression

1. **Immediate**:
   - Run profiler (rebar3 prof)
   - Check git diff for suspicious changes
   - Run microbenchmarks on specific path

2. **Investigation**:
   - Compare CPU profiles
   - Check memory allocator stats
   - Verify GC behavior
   - Review algorithm complexity changes

3. **Resolution**:
   - Revert change OR
   - Optimize implementation OR
   - Accept regression with justification (documented in PR)

## SLO Compliance Verification

### Continuous Verification

Benchmarks run automatically:
- **On main branch**: Daily at 2 AM UTC
- **On PRs**: Before merge (required gate)
- **On releases**: Before tag creation

### Manual Verification

```bash
# Run full benchmark suite
./tools/benchmark.sh --full

# Run specific operation benchmark
./tools/benchmark.sh --suite=throughput

# Compare with baseline
./tools/benchmark.sh --suite=throughput > current.txt
diff baseline.txt current.txt
```

## Future Performance Targets (Phase 2+)

As system scales, targets may evolve:

### Phase 2: 10,000 req/sec

- Health Check: < 5 ms P95
- Entitlement: < 25 ms P95
- Receipt: < 50 ms P95
- Support: < 10 ms P95

### Phase 3: 100,000 req/sec (sharded)

- Per-shard throughput: 10,000 req/sec
- Latency remains constant (no degradation with scale)
- Memory per request: < 5 KB

## References

- Benchmark Suite: `/Users/sac/erlmcp/bench/BENCHMARKS.md`
- Throughput Tests: `/Users/sac/erlmcp/bench/throughput_SUITE.erl`
- Latency Tests: `/Users/sac/erlmcp/bench/latency_SUITE.erl`
- Benchmark Script: `/Users/sac/erlmcp/tools/benchmark.sh`

---

**Document Version**: 1.0
**Last Updated**: 2026-01-26
**Next Review**: 2026-02-26
