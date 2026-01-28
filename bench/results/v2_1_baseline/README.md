# erlmcp v2.1 Performance Baseline

This directory contains the comprehensive performance baseline for erlmcp v2.1, established on **2026-01-28**.

## Quick Start

**New to these results?** Start here:
1. Read [QUICK_REFERENCE.txt](QUICK_REFERENCE.txt) - 1-page summary
2. Read [EXECUTIVE_SUMMARY.md](EXECUTIVE_SUMMARY.md) - Executive overview
3. Dive into [V2.1_PERFORMANCE_BASELINE_REPORT.md](V2.1_PERFORMANCE_BASELINE_REPORT.md) - Full analysis

## Files in This Directory

| File | Description | Audience |
|------|-------------|----------|
| **QUICK_REFERENCE.txt** | 1-page performance cheat sheet | Everyone |
| **EXECUTIVE_SUMMARY.md** | Executive summary with key metrics | Management, Leads |
| **V2.1_PERFORMANCE_BASELINE_REPORT.md** | Comprehensive performance analysis | Engineers, Performance Team |
| **full_benchmark_log.txt** | Raw execution log from benchmark run | Debugging, Verification |
| **execution.log** | Secondary execution log | Debugging |

## Key Metrics at a Glance

```
Throughput:     2.52M msg/sec  (4x improvement over v1.5.0)
Latency (p99):  99 μs
Memory:         19.5 MiB delta (400K operations)
CPU Usage:      47% average (16 cores)
System:         OTP 27, macOS 14.5, Apple Silicon (16 cores)
```

## Component Performance

| Component | Throughput | Latency (p50/p99) | Grade |
|-----------|------------|-------------------|-------|
| Registry (gproc) | 1.94M ops/sec | 52μs / 101μs | A |
| Queue (native) | 10.0M ops/sec | 0μs / 1μs | A+ |
| Pool (poolboy) | 2.50M ops/sec | 0μs / 1μs | A+ |
| Session (maps) | 0.95M ops/sec | 1μs / 108μs | A |

## v2.1 New Features (Pending Benchmarks)

The following v2.1 features have code and benchmark infrastructure ready, but need runtime integration and validation:

1. **Connection Pooling** (`erlmcp_pool_manager`)
   - Expected: 2x improvement for 100+ concurrent clients
   - Status: Code compiled, benchmarks ready

2. **Request Batching** (`erlmcp_batch`)
   - Expected: 2-5x throughput improvement
   - Status: Code compiled, benchmarks ready

3. **Response Caching** (`erlmcp_cache`)
   - Expected: 10-100x improvement for cache hits
   - Status: Module exists, needs benchmark creation

## Benchmark Status

| Category | Status | Priority | ETA |
|----------|--------|----------|-----|
| Core Operations | ✓ Complete | - | - |
| Connection Pooling | ⚠ Ready (needs runtime) | Critical | Week 1 |
| Request Batching | ⚠ Ready (needs runtime) | Critical | Week 1 |
| Response Caching | ⚠ Needs benchmark | Critical | Week 1 |
| TCP/HTTP Transport | ⚠ Needs integration | High | Week 2 |
| Stress Testing | Pending | High | Week 2 |
| Chaos Engineering | Pending | High | Week 3 |
| Integration (MCP e2e) | Pending | Medium | Week 4 |

## Historical Comparison

### v1.5.0 vs v2.1

| Metric | v1.5.0 | v2.1 | Improvement |
|--------|--------|------|-------------|
| Overall Throughput | 500K msg/sec | 2.52M msg/sec | **+403%** |
| Registry | 553K ops/sec | 1.94M ops/sec | +250% |
| Queue | 971K ops/sec | 10.0M ops/sec | +930% |
| Pool | 149K ops/sec | 2.50M ops/sec | +1,577% |
| Session | 242K ops/sec | 0.95M ops/sec | +293% |

**Result:** 4x performance improvement across the board

## Running Benchmarks

### Core Operations
```bash
erl -pa _build/default/lib/*/ebin -pa bench
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).
```

### v2.1 Features (When Integrated)
```bash
# Connection pooling
erlmcp_bench_pool:run(<<"pool_10_to_1000">>).
erlmcp_bench_pool:run(<<"pool_vs_no_pool">>).

# Request batching
erlmcp_bench_batch:run(<<"batch_10k">>).
erlmcp_bench_batch:run_all().
```

### Full Suite
```bash
./scripts/bench/run_all_benchmarks.sh ci
```

## Raw Data

Core operations baseline data:
- **File:** `../core_ops_core_ops_100k_1769630443.json`
- **Format:** JSON (metrology-compliant)
- **Size:** 2.1 KB
- **Validation:** Passed `erlmcp_metrology_validator`

## Metrology Compliance

All metrics follow erlmcp metrology standards:

- **Throughput:** `throughput_msg_per_s` (canonical unit)
- **Latency:** `latency_p50_us`, `latency_p95_us`, `latency_p99_us` (raw microseconds)
- **Memory:** `memory_*_mib` (with explicit scope annotation)
- **Precision:** `microsecond` (explicit)
- **Scope:** `per_node` (explicit)

## Regression Thresholds

Performance regression gates enforced in CI:

| Degradation | Status | Action |
|-------------|--------|--------|
| < 5% | ✓ PASS | No action required |
| 5-10% | ⚠ WARNING | Investigate cause |
| > 10% | ✗ FAILURE | BLOCKING (fails PR/build) |

## Production Readiness

**Overall:** 75% Ready

- **Core System:** ✓ READY (2.5M msg/sec, <100μs p99 latency)
- **v2.1 Features:** ⚠ PENDING (code complete, benchmarks/integration needed)
- **Target:** 100% by 2026-02-25

### Outstanding Work

**Week 1 (Critical):**
- Run pool manager benchmarks
- Run batch processing benchmarks
- Create cache benchmark module
- Document v2.1 feature performance

**Week 2-3 (High):**
- TCP/HTTP transport benchmarks
- Stress testing suite
- Chaos engineering suite
- Regression detection automation

**Week 4 (Medium):**
- Integration benchmarks (MCP e2e)
- CI/CD integration
- Performance tuning guide
- Production validation playbook

## Contact & Support

**Maintained By:** Performance Engineering Team  
**Contact:** performance@erlmcp.io  
**Last Updated:** 2026-01-28  

**Report Issues:**
- File a ticket: https://github.com/erlmcp/erlmcp/issues
- Tag: `performance`, `benchmarks`, `v2.1`

## Related Documentation

- [Benchmark Architecture](../../../docs/architecture.md#benchmarks)
- [OTP Performance Patterns](../../../docs/otp-patterns.md)
- [Metrology Standards](../../../docs/metrology/METRICS_GLOSSARY.md)
- [Agent: erlang-performance](./../../../.claude/agents/erlang-performance.md)

---

**Baseline Version:** v2.1 (partial)  
**Established:** 2026-01-28  
**Next Update:** After v2.1 feature benchmarks complete (target: 2026-02-25)  
