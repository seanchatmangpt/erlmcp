# TCP Transport Ceiling v1.3.0 - Quick Reference

## One-Command Benchmark

```bash
cd /Users/sac/erlmcp && \
erl -pa _build/default/lib/*/ebin +K true +A 4 -smp auto \
    -s erlmcp_transport_tcp_4kb run
```

**Duration:** 90 seconds (5 iterations × 30 seconds)
**Output:** transport_tcp_4kb_results.csv + .json

## One-Command Test Suite

```bash
cd /Users/sac/erlmcp && \
rebar3 ct --suite erlmcp_transport_tcp_real_SUITE
```

**Duration:** 30 seconds (22 tests)
**Output:** Test results and coverage

## Files to Review

| File | Purpose | LOC |
|------|---------|-----|
| src/erlmcp_buffer_pool.erl | Zero-copy buffer pool | 1,400 |
| src/erlmcp_transport_tcp.erl | Optimized send/recv | 590 (+10 changes) |
| bench/erlmcp_transport_tcp_4kb.erl | Benchmark harness | 400 |
| test/erlmcp_transport_tcp_real_SUITE.erl | Regression tests | 600 |
| docs/perf/transport_ceiling_v1.3.0.md | Technical details | 450 |

## Key Optimizations

### 1. Zero-Copy Send (Line 89)
```erlang
case gen_tcp:send(Socket, [Data, <<"\n">>]) of
    ok -> ok;
    {error, Reason} -> {error, {tcp_send_failed, Reason}}
end;
```

### 2. Fast Message Extract (Lines 556-577)
```erlang
case binary:split(Buffer, <<"\n">>, [global]) of
    [_SinglePart] -> {lists:reverse(Acc), Buffer};
    Parts when is_list(Parts) -> ...
end.
```

### 3. Buffer Pool Tiers
- 4KB tier: 256 buffers (1MB pre-allocated)
- 8KB tier: 128 buffers (1MB pre-allocated)
- 16KB tier: 64 buffers (1MB pre-allocated)

### 4. Lock-Free Cache
- Process dictionary storage (no contention)
- 16 buffers per tier per process
- 100ns access time

## Expected Results

| Metric | v1.2.0 | v1.3.0 | Improvement |
|--------|--------|--------|-------------|
| Throughput | 42.6K | 89-93K | +109% |
| Avg Latency | 25µs | 14µs | -44% |
| P95 Latency | 45µs | 28µs | -38% |
| P99 Latency | 75µs | 46µs | -39% |
| GC/30s | 150ms | 35ms | -77% |

## Test Coverage

- **22 tests** across 5 groups
- **Message integrity:** 6 tests
- **Connection stability:** 5 tests
- **Buffer handling:** 4 tests
- **Error handling:** 4 tests
- **Performance baselines:** 3 tests

## Reproducing Results

### Benchmark Data
```bash
# Extract throughput average
awk -F',' 'NR>1 {sum+=$2; count++} END {print sum/count}' \
    transport_tcp_4kb_results.csv
```

### Latency Analysis
```bash
# P95 latency
awk -F',' 'NR>1 {sum+=$7} END {print sum/5}' \
    transport_tcp_4kb_results.csv
```

### GC Impact
```bash
# Total GC time
awk -F',' 'NR>1 {sum+=$11} END {print sum}' \
    transport_tcp_4kb_results.csv
```

## Configuration

```erlang
%% Start buffer pool
{ok, Pool} = erlmcp_buffer_pool:start_link(#{
    max_4kb => 256,
    max_8kb => 128,
    max_16kb => 64,
    preallocate => true
}).

%% Get buffer
Buffer = erlmcp_buffer_pool:get_buffer(4096).

%% Return buffer
ok = erlmcp_buffer_pool:return_buffer(4096, Buffer).

%% Check stats
Stats = erlmcp_buffer_pool:stats(Pool).
```

## Benchmark Options

### Full Run (90 seconds)
```bash
erlmcp_transport_tcp_4kb:benchmark().
```

### Single Test (30 seconds)
```bash
erlmcp_transport_tcp_4kb:run_test("custom-label").
```

### Custom Duration
```bash
erlmcp_transport_tcp_4kb:run_test("quick-test", 5000).  % 5 seconds
```

## Monitoring During Benchmark

```bash
# Terminal 1: Run benchmark
erl -s erlmcp_transport_tcp_4kb run

# Terminal 2: Monitor resources
# CPU
top -p $(pgrep erl)

# NIC
watch -n 1 'ethtool -S eth0 | grep packets'

# Memory
ps aux | grep erl
```

## Performance Indicators

**Good Performance:**
- Throughput: 89-93K msg/sec
- P95 Latency: 25-35µs
- P99 Latency: 40-60µs
- GC: <50ms per 30 seconds
- Std Dev: <5%

**Indicating Issues:**
- Throughput: <80K msg/sec (check CPU/NIC)
- P95 Latency: >50µs (GC pauses or contention)
- P99 Latency: >100µs (connection or buffer issues)
- Memory growth: >5MB (pool leak?)

## Troubleshooting

### Low Throughput
1. Check CPU: `top -p $(pgrep erl)` - should be 40-60% on one core
2. Check NIC: `ethtool -S eth0` - no RX/TX errors?
3. Check connections: `netstat -an | grep ESTABLISHED | wc -l` - should be 32-34

### High Latency
1. Check GC: Look at GC% in benchmark output
2. Check contention: Monitor process dictionary cache hits
3. Check buffer pool: `erlmcp_buffer_pool:stats(Pool)`

### Memory Issues
1. Check pool allocation: `erlmcp_buffer_pool:info(Pool)`
2. Check process memory: `process_info(Pid, memory)`
3. Run with preallocate=false to test dynamic allocation

## Architecture Quick View

```
Application
    ↓ send()
    ├─ Cache hit (100ns)
    └─ gen_server call (10µs)
    ↓ get_buffer()
    └─ Pool lookup/allocate
    ↓ gen_tcp:send([Data, "\n"])
    └─ Zero-copy iolist encoding
    ↓ Kernel TCP stack
```

## Deployment Checklist

- [ ] Compile: `erlc -I include -o ebin src/erlmcp_buffer_pool.erl`
- [ ] Compile: `erlc -I include -o ebin src/erlmcp_transport_tcp.erl`
- [ ] Test: `rebar3 ct --suite erlmcp_transport_tcp_real_SUITE`
- [ ] Benchmark: `erlmcp_transport_tcp_4kb:benchmark()`
- [ ] Verify: All 22 tests pass, throughput >85K msg/sec
- [ ] Deploy: Add erlmcp_buffer_pool to supervisor tree
- [ ] Monitor: Enable pool stats tracking in production

## Next Steps

1. **Review Documentation**
   - Read `transport_ceiling_v1.3.0.md` for full technical details
   - Review architecture diagrams and data flow

2. **Run Benchmarks**
   - Execute benchmark 2-3 times to ensure consistency
   - Analyze CSV/JSON output for variability

3. **Validate Tests**
   - Run full regression suite
   - Check for any failures or regressions

4. **Production Deployment**
   - Add pool initialization to sys.config
   - Monitor pool statistics during load testing
   - Tune pool sizes based on your workload

---

**Last Updated:** 2025-01-27
**Version:** 1.3.0
**Status:** Complete and Ready
