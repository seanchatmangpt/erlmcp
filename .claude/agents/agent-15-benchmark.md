---
name: agent-15-benchmark
description: Performance regression check - validates <10% regression
model: sonnet
erlang_otp_context: true
phase: validate
depends_on: agent-06
gate: blocking
---

# Agent: Benchmark (agent-15)

## Purpose

Runs performance benchmarks and validates that no significant regression (<10%) has occurred compared to baseline.

## Quality Gate (BLOCKING)

```bash
✅ ./scripts/bench/check_regression.sh
✅ Regression: <10% from baseline
```

## Benchmarks

| Metric | Baseline | Threshold |
|--------|----------|-----------|
| Registry throughput | 553K msg/s | <10% regression |
| Queue throughput | 971K msg/s | <10% regression |
| Connection overhead | <100μs | <10% regression |
| JSON encode/decode | <50μs | <10% regression |

## Success Criteria

- [ ] All benchmarks complete
- [ ] No regression >10%
- [ ] Baseline exists or created
- [ ] Results logged

## Commands

```bash
# Run benchmarks
./scripts/bench/quick.sh 2>&1 | tee .erlmcp/benchmark.log

# Check regression
if [ -f .erlmcp/baseline.json ]; then
    ./scripts/bench/check_regression.sh
else
    echo "Creating baseline..."
    cp .erlmcp/benchmark.log .erlmcp/baseline.json
fi
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  ⚡ AGENT 15: BENCHMARK                                    ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS | ❌ REGRESSION
Duration: Xs

Results:
  Registry:    560K msg/s (+1.3% from baseline) ✅
  Queue:       985K msg/s (+1.4% from baseline) ✅
  Connection:  95μs (+5% from baseline) ✅
  JSON Codec:  48μs (-4% improvement) ✅

Regression: None ✅
Baseline: .erlmcp/baseline.json
```

## Baseline Management

```bash
# Set new baseline
make benchmark-baseline

# View baseline
cat .erlmcp/baseline.json

# Reset baseline
rm .erlmcp/baseline.json
make benchmark
```

## Integration

**Depends on**: agent-06 (tests pass)
**Parallel with**: agents 11, 12, 13, 14
**Blocks**: agent-20 (release)
**Baseline location**: `.erlmcp/baseline.json`
