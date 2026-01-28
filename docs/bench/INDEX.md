# erlmcp Benchmark Documentation Index v1.5.0

Complete navigation guide for benchmark documentation, workloads, and evidence.

---

## Quick Navigation

### New to erlmcp Benchmarks?
Start here: **[README.md](README.md)** (overview of 5 benchmark modules)

### Need to Run Benchmarks?
- **Quick (2 min):** [quickstart.md](quickstart.md)
- **Full (15 min):** [full-suite.md](full-suite.md)
- **Chaos only (5 min):** [chaos-80-20.md](chaos-80-20.md)

### Need to Understand Workloads?
See: **[workloads.md](workloads.md)** (all 25 workload_id entries with parameters)

### Need Metric Standards?
See: **[metrology.md](metrology.md)** (canonical units, validation, format)

### Need Evidence Format?
See: **[evidence-index.md](evidence-index.md)** (index.json specification)

### Migrating from Old Benchmarks?
See: **[legacy_retirement.md](legacy_retirement.md)** (what changed, how to update)

---

## Documentation Map

### 📖 Getting Started (Read First)

| Document | Purpose | Read Time | Use When |
|----------|---------|-----------|----------|
| **[README.md](README.md)** | Overview of 5 benchmark modules, performance baseline, quick commands | 5 min | You want the big picture |
| **[quickstart.md](quickstart.md)** | Run benchmarks in 2 minutes, interpret results | 5 min | You want to test NOW |

### 📚 Reference (Lookup When Needed)

| Document | Purpose | Content | Use When |
|----------|---------|---------|----------|
| **[workloads.md](workloads.md)** | Registry of all 25 workload_id entries with parameters | Registry table, performance targets | You need workload details |
| **[metrology.md](metrology.md)** | Metrics standards: units (msg/s, us, MiB), validation, format | Canonical units, validation rules, examples | You're writing new benchmarks |

### 🚀 Advanced (For Deep Dives)

| Document | Purpose | Content | Read Time | Use When |
|----------|---------|---------|-----------|----------|
| **[full-suite.md](full-suite.md)** | 15-minute complete testing of all 5 modules | Phase-by-phase walkthrough, quality gates | 10 min | Running full validation |
| **[chaos-80-20.md](chaos-80-20.md)** | 3 minimum chaos scenarios (5 min) | Why 3 is enough, scenario details, coverage | 5 min | PR review (fast path) |
| **[evidence-index.md](evidence-index.md)** | Machine-readable evidence format (index.json) | JSON schema, example output, CI integration | 10 min | Parsing benchmark results |

### 🔄 Migration (Upgrading from v0.5.0)

| Document | Purpose | Content | Use When |
|----------|---------|---------|----------|
| **[legacy_retirement.md](legacy_retirement.md)** | What was removed, where functionality moved, how to update | Migration path, breaking changes, FAQ | You have old benchmarks |

---

## 5 Benchmark Modules Overview

```
erlmcp_bench_core_ops
  └─ Registry, Queue, Pool, Session (in-memory throughput)
     └─ 553K, 971K, 149K, 242K msg/s respectively

erlmcp_bench_network_real
  └─ TCP, HTTP, Stdio (real network I/O)
     └─ 40K, 35K, 8-12K msg/s respectively

erlmcp_bench_stress
  └─ Sustained load (30s → 24h)
     └─ 372K msg/s, memory flat, degradation <5%

erlmcp_bench_chaos
  └─ Failure injection (11 scenarios, 80/20 = 3 required)
     └─ Recovery <5s, no cascades

erlmcp_bench_integration
  └─ MCP protocol end-to-end
     └─ 150ms p99 latency (init + tool call)
```

---

## Common Tasks

### "How do I run benchmarks?"
1. **Quick test (2 min):** See [quickstart.md](quickstart.md)
2. **Full suite (15 min):** See [full-suite.md](full-suite.md)
3. **Just chaos (5 min):** See [chaos-80-20.md](chaos-80-20.md)

### "What's my workload called?"
See [workloads.md](workloads.md) - all 25 workload_id entries listed

### "How do I interpret results?"
1. Check metric names: [metrology.md](metrology.md)
2. Compare to baseline in [README.md](README.md)
3. Parse evidence: [evidence-index.md](evidence-index.md)

### "I'm updating old benchmarks"
See [legacy_retirement.md](legacy_retirement.md) - migration guide with examples

### "I'm writing new benchmarks"
1. Add to [workloads.md](workloads.md)
2. Follow [metrology.md](metrology.md) standards
3. Output to [evidence-index.md](evidence-index.md) format

---

## Performance Baseline (Your Machine - Jan 2026)

Quick reference - see [README.md](README.md) for details:

| Component | Throughput | Notes |
|-----------|-----------|-------|
| **Registry** | 553K msg/s | Core routing |
| **Queue** | 971K msg/s | Peak in-memory |
| **Pool** | 149K msg/s | 50 workers |
| **Session** | 242K msg/s | Per-session state |
| **TCP** | 40K msg/s | Real network bottleneck |
| **HTTP** | 35K msg/s | TLS + headers |
| **Stress** | 372K msg/s | 30s, flat memory |
| **MCP e2e** | 150ms p99 | Init + tool call |

---

## Quality Gates (MUST PASS)

Before saying "done", verify:

✅ **Compilation**: `TERM=dumb rebar3 compile` (no errors)
✅ **Tests**: `rebar3 eunit` (0 failures)
✅ **Benchmarks**: `make benchmark-quick` (within 10% baseline)
✅ **Evidence**: `bench/evidence/index.json` written and validated
✅ **Coverage**: ≥80% (see [README.md](README.md))

---

## File Locations

| File | Path | Purpose |
|------|------|---------|
| README.md | `docs/bench/README.md` | Overview |
| Quickstart | `docs/bench/quickstart.md` | 2-min run |
| Full Suite | `docs/bench/full-suite.md` | 15-min run |
| Workloads | `docs/bench/workloads.md` | All 25 workload_id entries |
| Metrology | `docs/bench/metrology.md` | Metric standards |
| Chaos 80/20 | `docs/bench/chaos-80-20.md` | 3 minimum scenarios |
| Evidence | `docs/bench/evidence-index.md` | index.json schema |
| Legacy | `docs/bench/legacy_retirement.md` | v0.5.0 → v1.5.0 migration |
| **This index** | `docs/bench/INDEX.md` | You are here |

---

## Related Documentation

- **Main docs**: [docs/INDEX.md](../INDEX.md)
- **Architecture**: [docs/architecture.md](../architecture.md)
- **OTP Patterns**: [docs/otp-patterns.md](../otp-patterns.md)
- **Protocol**: [docs/protocol.md](../protocol.md)
- **API Reference**: [docs/api-reference.md](../api-reference.md)

---

## Key Commands Reference

```bash
# 2-minute quick test
make benchmark-quick

# Full 15-minute suite
cd /Users/sac/erlmcp && ./scripts/bench/run_all_benchmarks.sh

# Single workload
cd /Users/sac/erlmcp && rebar3 shell
> erlmcp_bench_core_ops:run(<<"core_ops_100k">>).

# View evidence
cat /Users/sac/erlmcp/bench/evidence/index.json | jq '.'

# Check for regressions
cat /Users/sac/erlmcp/bench/evidence/index.json | \
  jq '.benchmarks[] | select(.regression_analysis.is_regression==true)'
```

---

## FAQ

**Q: How long does it take to run all benchmarks?**
A: Full suite = 15 min, Quick suite = 2 min, Chaos only = 5 min

**Q: What if a benchmark regresses?**
A: See [README.md](README.md) troubleshooting, check system load, re-run

**Q: How do I add a custom workload?**
A: Edit [workloads.md](workloads.md), add entry, run via existing modules

**Q: Are old benchmarks (v0.5.0) supported?**
A: See [legacy_retirement.md](legacy_retirement.md) for migration path

**Q: What's the difference between TCP and HTTP tests?**
A: [README.md](README.md) - TCP is raw sockets, HTTP adds TLS + headers

---

## Version Info

- **Documentation version**: 1.5.0
- **Benchmark consolidation**: 15 modules → 5 modules (v0.5.0 → v1.5.0)
- **Metrology standards**: v1.5.0 (canonical units, ISO 80000)
- **Last updated**: 2026-01-27

---

**Start here**: [README.md](README.md) | **Quick run**: [quickstart.md](quickstart.md) | **Full run**: [full-suite.md](full-suite.md)
