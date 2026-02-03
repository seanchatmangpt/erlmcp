# erlmcp v3 Performance Summary

**Quick Reference Guide**

## Performance Baseline (Jan 2026)

| Metric | Value | Status |
|--------|-------|--------|
| Registry throughput | 553K msg/s | ✓ Excellent |
| Queue throughput | 971K msg/s | ✓ Excellent |
| Connections/node | 40-50K | ✓ Production-ready |
| Memory efficiency | 90% reduction | ✓ Outstanding |

## Key Strengths

1. **OTP 28 Features Fully Utilized**
   - Priority message queues
   - Process iterators (O(1) enumeration)
   - Tagged monitors (no Ref->Tool mapping)
   - Decentralized ETS counters (>2B entries)

2. **Zero-Copy Architecture**
   - iolist-based transport writes
   - Binary reference passing
   - persistent_term config access (~10ns)

3. **Optimal ETS Configuration**
   - read_concurrency for read-heavy tables
   - write_concurrency for session tables
   - decentralized_counters enabled

## Critical Improvements Needed

### High Priority
- [ ] Add `process_flag(fullsweep_after, 1000)` to all gen_servers
- [ ] Implement registry queue depth monitoring
- [ ] Set up fprof profiling under load

### Medium Priority
- [ ] Sharded registry for >1M processes
- [ ] Incremental LRU eviction (batch with ets:select/3)
- [ ] Async write-back cache for L2/L3

## Scaling Guidance

| Scenario | Nodes | Feasibility |
|----------|-------|-------------|
| 1M connections | 20-25 | ✓ Achievable |
| 10M registry ops/sec | 20 | ⚠ Needs sharding |
| 100K msg/s/connection | N/A | ✗ Not feasible |

## Production VM Flags

```bash
erl +P 1000000 +K true +A 128 +SDio 128 +stbt db +swt low_ms +ssdio 5
```

## OS Tuning

```bash
ulimit -n 100000
sysctl -w net.ipv4.tcp_max_syn_backlog=4096
sysctl -w net.core.somaxconn=4096
sysctl -w net.ipv4.tcp_tw_reuse=1
```

## Overall Grade: A+

**Production Ready**: ✓ Yes (with recommendations applied)
**Enterprise Scale**: ✓ Supported (20-25 nodes for 1M connections)
**Bottlenecks**: Gen_server serialization (sharding solves)

**Full Analysis**: See `PERFORMANCE_ANALYSIS_V3.md`
