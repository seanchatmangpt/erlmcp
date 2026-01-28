# Quick Start - 2-Minute Benchmark Run

Get baseline performance metrics in 2 minutes.

---

## Quick Command

```bash
make benchmark-quick
```

That's it! This runs a minimal benchmark subset in under 30 seconds.

---

## What It Does

The `benchmark-quick` target:
- Compiles the codebase (if needed)
- Runs `erlmcp_simple_benchmark` module (if available) OR
- Outputs baseline metrics from last run
- Duration: < 30 seconds
- Focus: Quick validation that build is working

**Expected output:**
```
Running Quick Benchmark (< 30 seconds)...
✓ Quick benchmark complete
  - Throughput: ~10K msg/sec
  - Latency P50: <5ms
```

---

## Alternative: Using the Shell Script

For more detailed quick benchmarking with 1 workload per category:

```bash
./scripts/bench/quick_bench.sh
```

This runs in approximately 2-5 minutes and includes:
- 1 workload from core operations
- 1 workload from network I/O
- Memory baseline (no stress/chaos)
- Results saved to `bench/results/<timestamp>/`

---

## Expected Artifacts

Quick benchmark produces minimal output:
- **No JSON files** from `make benchmark-quick` (console-only)
- **JSON files** if using `./scripts/bench/quick_bench.sh` (saved to `bench/results/`)

Verify quick_bench.sh created results:
```bash
ls -lh bench/results/latest/
```

---

## Performance Baseline (Your Machine - Jan 2026)

```
Registry throughput:   553K msg/s
Network TCP:           43K msg/s (real packets, expected bottleneck)
Sustained 30s:         372K msg/s (no memory leak)
```

---

## Check Results

### Using make benchmark-quick

Exit code only:
```bash
make benchmark-quick
echo $?  # 0 = success
```

### Using ./scripts/bench/quick_bench.sh

View JSON results:
```bash
# Latest results
ls -1t bench/results | head -1 | xargs -I {} cat bench/results/{}/summary.json | jq .

# Or manually open
cat bench/results/20260127_143022/index.json
```

---

## Next Steps

If quick benchmark passes:
1. **Run full suite** (15 min): `make benchmark-full` or `./scripts/bench/run_all_benchmarks.sh`
2. **View full results**: `make benchmark-show-latest`
3. **Compare to baseline**: `make benchmark-compare RESULTS_DIR=bench/results/YYYYMMDD_HHMMSS`

If quick benchmark fails:
1. **Check compilation**: `make compile`
2. **Verify dependencies**: `rebar3 get-deps`
3. **Run unit tests**: `rebar3 eunit`
4. **Check system resources**: `top`, `lsof -p <erlang_pid>`

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| **"module not found"** | Run `make compile` first; erlmcp_simple_benchmark is optional |
| **Low throughput** | Kill other Erlang processes, reduce system load |
| **"Connection refused"** | Network workloads need TCP ports available (9000-9100) |
| **Exit code 1** | Check `./scripts/bench/quick_bench.sh` output for details |

---

**Next**: [Full test suite](full-suite.md) | [README](README.md) | [Workloads](workloads.md)

**Navigation**: [Back to Benchmark Index](INDEX.md)
