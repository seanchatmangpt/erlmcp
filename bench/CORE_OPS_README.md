# ERLMCP Core Operations Benchmark

## Overview

`erlmcp_bench_core_ops.erl` is a **consolidated micro-benchmark suite** that measures fundamental in-memory operations across erlmcp's core subsystems:

- **Registry operations**: put/get/delete/lookup using Erlang process dictionary
- **Queue operations**: enqueue/dequeue cycles using Erlang queue module
- **Pool operations**: checkout/checkin using ETS-based worker pool
- **Session state**: concurrent get/put on session map using ETS

This benchmark replaces 8+ older benchmark modules with a unified, metrology-compliant implementation.

## Key Features

✅ **Metrology-Compliant JSON Output**: All results follow standardized format with microsecond precision
✅ **Multiple Workloads**: 1K, 10K, 100K, 1M operations with configurable worker counts
✅ **Comprehensive Metrics**: Throughput, latency percentiles (p50/p95/p99), memory delta, CPU usage
✅ **Component Breakdown**: Per-component latency analysis (registry, queue, pool, session)
✅ **Environment Capture**: Full system context (OS, Erlang version, hostname)

## Workloads

| Workload ID | Operations | Workers | Use Case |
|-------------|------------|---------|----------|
| `core_ops_1k` | 1,000 | 1 | Quick smoke test |
| `core_ops_10k` | 10,000 | 10 | Development validation |
| `core_ops_100k` | 100,000 | 100 | Performance baseline |
| `core_ops_1m` | 1,000,000 | 100 | Stress testing |

## Usage

### Run All Workloads

```erlang
% From rebar3 shell
rebar3 shell
> erlmcp_bench_core_ops:run_all().
```

### Run Single Workload

```erlang
% Binary ID
> erlmcp_bench_core_ops:run(<<"core_ops_100k">>).

% String ID (auto-converted)
> erlmcp_bench_core_ops:run("core_ops_100k").

% Atom ID (auto-converted)
> erlmcp_bench_core_ops:run(core_ops_100k).
```

### Command Line (One-Shot Execution)

```bash
# Run specific workload
erl -noshell -pa _build/default/lib/*/ebin \
  -eval "compile:file(\"bench/erlmcp_bench_core_ops.erl\", [{outdir, \"bench/\"}, debug_info]), \
         code:add_path(\"bench\"), \
         erlmcp_bench_core_ops:run(<<\"core_ops_100k\">>), \
         timer:sleep(2000), \
         init:stop()."

# Run all workloads
erl -noshell -pa _build/default/lib/*/ebin \
  -eval "compile:file(\"bench/erlmcp_bench_core_ops.erl\", [{outdir, \"bench/\"}, debug_info]), \
         code:add_path(\"bench\"), \
         erlmcp_bench_core_ops:run_all(), \
         timer:sleep(5000), \
         init:stop()."
```

## Output Format

Results are written to `bench/results/core_ops_<workload_id>_<timestamp>.json`

### JSON Schema

```json
{
  "workload_id": "core_ops_100k",
  "benchmark": "core_operations",
  "timestamp": 1769567361,
  "environment": {
    "hostname": "Seans-MacBook-Pro",
    "erlang_version": "OTP-27",
    "erlang_erts_version": "15.2.7.1",
    "os": "darwin",
    "os_version": "25.2.0"
  },
  "operations": 400000,
  "duration_s": 0.15,
  "throughput_msg_per_s": 2690088.37,
  "latency_p50_us": 0.0,
  "latency_p95_us": 83.0,
  "latency_p99_us": 98.0,
  "precision": "microsecond",
  "memory_start_mib": 44.8,
  "memory_end_mib": 63.9,
  "memory_delta_mib": 19.1,
  "cpu_percent_avg": 42.0,
  "scope": "per_node",
  "components": {
    "registry": { ... },
    "queue": { ... },
    "pool": { ... },
    "session": { ... }
  }
}
```

### Component Metrics

Each component includes:

- `operations`: Number of operations performed
- `latency_p50_us`: 50th percentile latency (microseconds)
- `latency_p95_us`: 95th percentile latency (microseconds)
- `latency_p99_us`: 99th percentile latency (microseconds)
- `latency_min_us`: Minimum latency
- `latency_max_us`: Maximum latency
- `latency_avg_us`: Average latency

## Benchmark Results (Example)

### 100K Workload (OTP-27, Darwin 25.2.0)

```
Operations:    400,000 (4 components × 100K each)
Duration:      0.15s
Throughput:    2.69M ops/sec
Latency (p95): 83 μs
Memory Delta:  +19.1 MiB
CPU Average:   42%
```

**Per-Component Latencies:**

| Component | p50 (μs) | p95 (μs) | p99 (μs) | Max (μs) |
|-----------|----------|----------|----------|----------|
| Registry  | 52.0     | 97.0     | 100.0    | 101.0    |
| Queue     | 0.0      | 1.0      | 1.0      | 605.0    |
| Pool      | 0.0      | 1.0      | 1.0      | 468.0    |
| Session   | 1.0      | 20.0     | 84.0     | 18,590.0 |

### 1M Workload (Stress Test)

```
Operations:    4,000,000 (4 components × 1M each)
Duration:      ~2-3s
Throughput:    1.3-2M ops/sec
Memory Delta:  +100-200 MiB
```

## Implementation Details

### Registry Benchmark

Uses Erlang's process dictionary (`erlang:put/get/erase`):

```erlang
Key = {registry, WorkerId, rand:uniform(1000000)},
Value = {test_data, erlang:system_time()},

erlang:put(Key, Value),    % Insert
_ = erlang:get(Key),       % Lookup
erlang:erase(Key)          % Delete
```

### Queue Benchmark

Uses Erlang's queue module:

```erlang
Q1 = queue:in({test_item, erlang:system_time()}, Q0),  % Enqueue
{value, _Item} = queue:out(Q1)                          % Dequeue
```

### Pool Benchmark

Simulates connection pool using ETS:

```erlang
% Checkout
[[WorkerId]] = ets:match(PoolTable, {'$1', available}, 1),
ets:insert(PoolTable, {WorkerId, busy}),

% Checkin
ets:insert(PoolTable, {WorkerId, available})
```

### Session Benchmark

Concurrent session state using ETS with read/write concurrency:

```erlang
SessionTable = ets:new(bench_session,
  [set, public, {write_concurrency, true}, {read_concurrency, true}]),

ets:insert(SessionTable, {SessionId, Value}),
ets:lookup(SessionTable, SessionId)
```

## Validation

All reports are validated before writing to ensure:

✅ Required fields present (workload_id, benchmark, timestamp, etc.)
✅ Environment fields captured (hostname, erlang_version, os)
✅ Component metrics complete (all 4 components)
✅ Numeric values in valid ranges

Validation errors are reported immediately:

```erlang
{error, {validation_failed, {missing_fields, [operations, duration_s]}}}
```

## File Organization

```
bench/
├── erlmcp_bench_core_ops.erl       % Main benchmark module
├── CORE_OPS_README.md              % This file
└── results/
    ├── core_ops_core_ops_1k_*.json
    ├── core_ops_core_ops_10k_*.json
    ├── core_ops_core_ops_100k_*.json
    └── core_ops_core_ops_1m_*.json
```

## Integration with CI/CD

### Regression Detection

```bash
# Run baseline benchmark
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).

# Compare with historical results
# Fail if throughput drops >10% or p95 latency increases >20%
```

### Performance Gates

Example quality gates for production:

- Throughput: ≥ 2M ops/sec (100K workload)
- p95 Latency: ≤ 100 μs
- p99 Latency: ≤ 150 μs
- Memory Delta: ≤ 50 MiB

## Troubleshooting

### Issue: High Session Latency (p99 > 1000 μs)

**Cause**: ETS table contention under high worker count
**Solution**: Tune `{write_concurrency, true}` settings or shard tables

### Issue: Memory Delta Increasing Over Workloads

**Cause**: Process dictionary not cleaned between runs
**Solution**: Restart Erlang node between full benchmark suites

### Issue: CPU Usage Low (<30%)

**Cause**: Insufficient parallelism (low worker count)
**Solution**: Increase worker count in workload definition

## Future Enhancements

- [ ] Add gproc-based registry benchmark (compare vs process dictionary)
- [ ] Add poolboy integration (compare vs ETS-based pool)
- [ ] Distributed workload (multi-node registry/session replication)
- [ ] Memory profiling per operation (recon_alloc integration)
- [ ] Scheduler utilization metrics (recon scheduler stats)
- [ ] JSON Schema validation via jesse (strict metrology compliance)

## Related Benchmarks

- `benchmark_100k.erl` - Legacy 100K comprehensive benchmark (replaced)
- `erlmcp_registry_contention.erl` - Registry-specific stress test
- `erlmcp_transport_tcp_4kb.erl` - TCP transport benchmark

## References

- [erlmcp Benchmark Suite](./README_BENCHMARKS.md)
- [Metrology Standards](./BENCHMARKS.md)
- [Erlang Efficiency Guide](https://www.erlang.org/doc/efficiency_guide/introduction)

---

**Last Updated**: 2026-01-27
**Erlang/OTP**: 27
**Module Version**: 1.0.0
