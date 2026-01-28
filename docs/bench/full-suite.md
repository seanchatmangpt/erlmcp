# Full Benchmark Suite - 10-15 Minute Run

Complete testing of all 5 benchmark modules with results saved to JSON.

---

## Quick Command

```bash
make benchmark-full
```

Or use the automated script:

```bash
./scripts/bench/run_all_benchmarks.sh
```

Expected duration: 10-15 minutes depending on system load.

---

## What Gets Tested

The full suite executes 5 benchmark categories:

### 1. Core Operations (Registry, Queue, Pool, Session)
- **Workloads**: 4 progression levels (10K, 25K, 50K, 100K ops)
- **Metrics**: Throughput (msg/s), latency percentiles (p50/p95/p99)
- **Expected baseline**: Registry 553K msg/s, Queue 971K msg/s
- **Duration**: 4-5 minutes

### 2. Network I/O (TCP and HTTP)
- **Workloads**: Real TCP connections (100→100K) and HTTP (100→5K)
- **Metrics**: Sustained throughput with packet overhead
- **Expected baseline**: 43K msg/s (bounded by 4KB real packets)
- **Duration**: 3-4 minutes

### 3. Stress Testing (Sustained Load)
- **Workloads**: 30s, 5min, 1h sustained operations
- **Metrics**: Memory growth, latency stability, GC impact
- **Expected baseline**: 372K msg/s over 30s with <1% memory growth
- **Duration**: 2-3 minutes

### 4. Chaos (Failure Injection)
- **Workloads**: Memory exhaustion, connection failure, supervisor crash, etc.
- **Metrics**: Recovery time, cascade prevention, error handling
- **Expected baseline**: Recovery <5s, no cascading failures
- **Duration**: 2-3 minutes

### 5. Integration (MCP Protocol)
- **Workloads**: Initialize, tool calls, resource listing, notifications
- **Metrics**: End-to-end latency, protocol compliance
- **Expected baseline**: MCP init 45ms, tool call 150ms, full workflow 500ms p99
- **Duration**: 2-3 minutes

---

## Running the Full Suite

### Option 1: Using Makefile (Simplest)

```bash
make benchmark-full
```

This:
- Compiles code automatically
- Runs test suite via CT (Common Test)
- Prints summary to console
- Exit code 0 = success, non-zero = failure

Expected output:
```
Running Full Benchmark Suite...
✓ Full benchmark complete (duration: XXX seconds)
```

### Option 2: Using Shell Script (Most Detail)

```bash
./scripts/bench/run_all_benchmarks.sh
```

Or specify a mode:
```bash
./scripts/bench/run_all_benchmarks.sh standard  # 30 minutes
./scripts/bench/run_all_benchmarks.sh full      # 2 hours (all workloads)
```

Modes available:
- **quick**: 1 workload per category (~5 min)
- **standard**: 3 workloads per category (~30 min)
- **full**: All workloads (~2 hours)
- **ci**: Quick mode + strict metrology validation (default)

---

## Understanding Results

### JSON Output Structure

Results are saved to `bench/results/<timestamp>/`:

```
bench/results/20260127_143022/
├── index.json              # Summary of all workloads executed
├── summary.json            # Aggregated metrics
├── summary.txt             # Human-readable summary
├── core_*.json             # Core operations results
├── network_*.json          # Network I/O results
├── stress_*.json           # Stress test results
├── chaos_*.json            # Chaos test results
├── integration_*.json      # Integration test results
└── execution.log           # Full execution log
```

### View Latest Results

```bash
# Show last run summary
make benchmark-show-latest

# Or manually:
cat bench/results/latest/summary.txt

# View JSON details
cat bench/results/latest/index.json | jq '.workloads[] | {id, status, throughput_msg_per_s}'
```

### Interpreting index.json

```json
{
  "run_id": "run_20260127_143022",
  "timestamp": "2026-01-27T14:30:22Z",
  "git_sha": "abc123...",
  "mode": "standard",
  "total_workloads": 15,
  "passed": 15,
  "failed": 0,
  "workloads": [
    {
      "workload_id": "registry_contention_100k",
      "status": "pass",
      "throughput_msg_per_s": 553000,
      "latency_p50_us": 1.8,
      "latency_p95_us": 3.2,
      "latency_p99_us": 5.1,
      "memory_heap_mib_per_conn": 0.12,
      "duration_s": 5.2
    }
  ]
}
```

---

## Quality Gates (MUST PASS)

| Benchmark | Metric | Target | Success Criteria |
|-----------|--------|--------|-----------------|
| Core Ops | Registry throughput | 550K msg/s | ≥500K (90% of baseline) |
| Core Ops | Queue throughput | 900K msg/s | ≥810K (90% of baseline) |
| Network | TCP throughput | 40K msg/s | ≥36K (90% of baseline) |
| Stress | Memory growth | <1% per min | Flat profile, no leak |
| Chaos | Recovery time | <5s | All failures recover <5s |
| Integration | MCP latency p99 | <500ms | <500ms for full workflow |

**Pass Criteria:**
- All workloads: status = "pass" (0 failures)
- No regressions: >10% drop from baseline
- No memory leaks: <1% growth over duration
- All errors: error_rate < 0.05%

---

## Regression Testing

### Set Baseline

After a successful run:

```bash
make benchmark-set-baseline RESULTS_DIR=bench/results/20260127_143022
```

This saves the current results as the baseline for future comparison.

### Compare to Baseline

Run benchmarks and automatically compare:

```bash
make benchmark-compare RESULTS_DIR=bench/results/20260127_153045
```

Output shows % change from baseline:
```
registry_contention_100k:
  Baseline: 553K msg/s
  Current:  541K msg/s
  Change: -2.2% ✅ (within 10% threshold)

tcp_sustained_25k:
  Baseline: 43K msg/s
  Current:  38K msg/s
  Change: -11.6% ❌ (REGRESSION - exceeds 10%)
```

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| **Out of memory** | System load too high; kill other processes, re-run |
| **Compilation errors** | Run `rebar3 clean && rebar3 compile` first |
| **Network tests timeout** | Check TCP ports 9000-9100 available, firewall settings |
| **Latency spikes** | Possibly GC pause; re-run or check system load |
| **Regression detected** | Check git diff, re-baseline if intentional |
| **Metrology violations** | Run with `METROLOGY_STRICT=false` (dev only, not CI) |

---

## Typical Run Flow

1. **Compile** (automatic with `make`)
   ```bash
   TERM=dumb rebar3 compile
   ```

2. **Run full suite** (10-15 min)
   ```bash
   ./scripts/bench/run_all_benchmarks.sh
   ```

3. **Check results**
   ```bash
   cat bench/results/latest/summary.txt
   ```

4. **Compare to baseline** (if baseline exists)
   ```bash
   make benchmark-compare RESULTS_DIR=bench/results/20260127_143022
   ```

5. **Save new baseline** (if intentional changes)
   ```bash
   make benchmark-set-baseline RESULTS_DIR=bench/results/20260127_143022
   ```

---

## Performance Baseline (Your Machine - Jan 2026)

Expect these throughput ranges:

| Component | Throughput | Notes |
|-----------|-----------|-------|
| Registry | 553K msg/s | 100K concurrent keys |
| Queue | 971K msg/s | FIFO buffer, 1M ops |
| Pool | 149K msg/s | Connection pool (50 size) |
| Session | 242K msg/s | State tracking per session |
| Network (TCP) | 43K msg/s | Real packets (4KB each) |
| Network (HTTP) | 35K msg/s | TLS + headers overhead |
| Sustained (30s) | 372K msg/s | No memory leak (<1% growth) |
| MCP e2e | 150ms p99 | Init + tool call |

---

**Navigation**: [Back to Benchmark Index](INDEX.md) | [Quick Start](quickstart.md) | [README](README.md) | [Workloads](workloads.md)
