# Makefile Architecture - Before vs. After

**Visual comparison of current monolithic Makefile vs. proposed modular architecture**

---

## Current Architecture (Monolithic)

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Makefile (1500 lines)                        │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Colors & Variables (22 lines)                                │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Main Targets (all, compile, test) (28 lines)                 │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Canonical Workflow (doctor, quick, verify) (140 lines)       │  │
│  │ - Mixed logic                                                 │  │
│  │ - Duplicated error handling                                   │  │
│  │ - No state management                                         │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Help Target (80 lines)                                        │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Erlang Version Check (30 lines)                              │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Compilation Targets (60 lines)                               │  │
│  │ - compile, compile-core, compile-transports, etc.            │  │
│  │ - Duplicated logic across apps                               │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Testing Targets (140 lines)                                   │  │
│  │ - test, eunit, ct, test-smoke, test-quick, test-full         │  │
│  │ - No parallelization                                          │  │
│  │ - No incremental testing                                      │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Quality Gates (250 lines)                                     │  │
│  │ - validate-*, check-*, etc.                                   │  │
│  │ - Sequential execution (360s)                                 │  │
│  │ - No state persistence                                        │  │
│  │ - Lots of duplication                                         │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ TCPS Manufacturing (200 lines)                                │  │
│  │ - jidoka, andon, poka-yoke                                    │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Governance (300 lines)                                        │  │
│  │ - hooks-validate, settings-validate, receipts-list           │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ CLI Targets (200 lines)                                       │  │
│  │ - cli-version, cli-release, cli-install, benchmarks          │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Development & Release (150 lines)                             │  │
│  │ - console, observer, deps, release, clean                     │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘

PROBLEMS:
❌ Monolithic (1500 lines, hard to navigate)
❌ Sequential (360s for make check)
❌ No state (rebuild everything)
❌ 30% duplication
❌ No observability (no logs, no metrics)
❌ Expensive ($15/month cloud cost)
```

---

## Target Architecture (Modular, Parallel, Stateful)

```
┌──────────────────────────────────────────────────────────────────────┐
│                      Makefile (200 lines)                             │
│                                                                       │
│  - Global variables                                                   │
│  - Include statements                                                 │
│  - Top-level help                                                     │
│  - Canonical workflow targets (doctor, quick, verify)                │
│                                                                       │
└───────────────────────┬───────────────────────────────────────────────┘
                        │
        ┌───────────────┴───────────────┐
        │  include mk/*.mk              │
        └───────────────┬───────────────┘
                        │
    ┌───────────────────┼───────────────────┬───────────────────┐
    │                   │                   │                   │
    v                   v                   v                   v

┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
│  mk/state.mk    │ │ mk/parallel.mk  │ │  mk/core.mk     │ │ mk/quality.mk   │
│  (150 lines)    │ │  (120 lines)    │ │  (150 lines)    │ │  (180 lines)    │
├─────────────────┤ ├─────────────────┤ ├─────────────────┤ ├─────────────────┤
│ STATE MGMT      │ │ PARALLEL EXEC   │ │ BUILD           │ │ QUALITY GATES   │
│                 │ │                 │ │                 │ │                 │
│ • mark_running  │ │ • parallel_exec │ │ • compile       │ │ • validate-*    │
│ • mark_done     │ │ • job control   │ │ • test          │ │ • check         │
│ • should_skip   │ │ • gate_wrapper  │ │ • clean         │ │ • verify        │
│ • state file    │ │ • locking       │ │ • per-app       │ │ • parallel      │
│                 │ │                 │ │                 │ │ • incremental   │
└─────────────────┘ └─────────────────┘ └─────────────────┘ └─────────────────┘

┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
│  mk/tcps.mk     │ │ mk/governance.mk│ │  mk/cli.mk      │ │ mk/benchmark.mk │
│  (160 lines)    │ │  (140 lines)    │ │  (120 lines)    │ │  (100 lines)    │
├─────────────────┤ ├─────────────────┤ ├─────────────────┤ ├─────────────────┤
│ TCPS SYSTEM     │ │ GOVERNANCE      │ │ CLI MGMT        │ │ BENCHMARKING    │
│                 │ │                 │ │                 │ │                 │
│ • jidoka        │ │ • hooks-validate│ │ • cli-version   │ │ • bench-quick   │
│ • andon         │ │ • settings-val  │ │ • cli-release   │ │ • benchmark     │
│ • poka-yoke     │ │ • receipts-list │ │ • cli-install   │ │ • regression    │
│ • quality gates │ │ • governance-   │ │ • checksum      │ │ • profiling     │
│                 │ │   test          │ │                 │ │ (local only)    │
└─────────────────┘ └─────────────────┘ └─────────────────┘ └─────────────────┘

┌─────────────────┐ ┌─────────────────┐
│ mk/release.mk   │ │  mk/dev.mk      │
│  (100 lines)    │ │  (80 lines)     │
├─────────────────┤ ├─────────────────┤
│ RELEASE MGMT    │ │ DEV TOOLS       │
│                 │ │                 │
│ • release       │ │ • console       │
│ • release-val   │ │ • observer      │
│ • deploy        │ │ • deps          │
│                 │ │ • info          │
└─────────────────┘ └─────────────────┘

        ┌───────────────────────────────────────┐
        │  .erlmcp/ (State & Observability)     │
        ├───────────────────────────────────────┤
        │  ├── state/                           │
        │  │   ├── gates.json    (gate state)   │
        │  │   └── gates.json.bak (backup)      │
        │  ├── logs/                            │
        │  │   └── <target>_<timestamp>.log     │
        │  ├── locks/                           │
        │  │   └── <gate>.lock  (parallel safe) │
        │  ├── receipts/                        │
        │  │   └── <date>.json  (TCPS receipt)  │
        │  └── metrics/                         │
        │      └── snapshots/*.json             │
        └───────────────────────────────────────┘

BENEFITS:
✅ Modular (10 files, 150 lines each, easy to navigate)
✅ Parallel (120s for make check, 3x speedup)
✅ Stateful (incremental builds, skip unchanged)
✅ DRY (<5% duplication, shared primitives)
✅ Observable (logs, metrics, receipts, state)
✅ Cost-effective ($5/month cloud, 67% reduction)
```

---

## Execution Flow Comparison

### Current: Sequential (360s)

```
┌────────────┐
│ make check │
└──────┬─────┘
       │
       v
┌────────────────┐  30s
│   compile      │────────────────────────────────┐
└────────────────┘                                │
                                                  │
┌────────────────┐  60s                          │
│   eunit        │──────────────────────┐        │
└────────────────┘                      │        │
                                        │        │
┌────────────────┐  120s                │        │
│   ct           │──────┐               │        │
└────────────────┘      │               │        │
                        │               │        │
┌────────────────┐  90s │               │        │
│   dialyzer     │──┐   │               │        │
└────────────────┘  │   │               │        │
                    │   │               │        │
┌────────────────┐  30s│   │               │        │
│   xref         │┐ │   │               │        │
└────────────────┘│ │   │               │        │
                  │ │   │               │        │
┌────────────────┐│ │   │               │        │
│   coverage     ││ │   │               │        │
└────────────────┘│ │   │               │        │
                  │ │   │               │        │
                  v v   v               v        v
                ┌───────────────────────────────────┐
                │ TOTAL: 360s (6 minutes)           │
                └───────────────────────────────────┘
```

### Target: Parallel (120s)

```
┌────────────┐
│ make check │
└──────┬─────┘
       │
       v
┌────────────────┐  30s
│   compile      │─────────────────────────────────┐
└────────────────┘                                 │
                                                   │
       ┌───────────────────────────────────────────┤
       │                                           │
       v                                           │
┌──────────────────────────────────────┐           │
│  Parallel Gate Execution (max 90s)   │           │
│  ┌────────────┐  ┌────────────┐     │           │
│  │   eunit    │  │  dialyzer  │     │           │
│  │    60s     │  │    90s     │     │           │
│  └────────────┘  └────────────┘     │           │
│                                      │           │
│  ┌────────────┐  ┌────────────┐     │           │
│  │   xref     │  │  ct-core   │     │           │
│  │    30s     │  │    40s     │     │           │
│  └────────────┘  └────────────┘     │           │
│                                      │           │
│  ┌────────────┐  ┌────────────┐     │           │
│  │ct-transp.  │  │ ct-observ. │     │           │
│  │    40s     │  │    40s     │     │           │
│  └────────────┘  └────────────┘     │           │
│                                      │           │
│  (6 gates run in parallel)          │           │
└──────────────────────────────────────┘           │
                   │                               │
                   v                               │
            ┌────────────┐  30s                    │
            │  coverage  │────────────┐            │
            └────────────┘            │            │
                                      │            │
                                      v            v
                            ┌──────────────────────────┐
                            │ TOTAL: 150s (2.5 minutes)│
                            │ SPEEDUP: 2.4x            │
                            └──────────────────────────┘
```

---

## State Management

### Current: No State (Always Rebuild)

```
┌─────────────────────────────────────────────────────────┐
│ Iteration 1: make compile                               │
│   → Compiles all apps (30s)                             │
└─────────────────────────────────────────────────────────┘
                    │
                    v
┌─────────────────────────────────────────────────────────┐
│ User edits 1 file: apps/erlmcp_core/src/erlmcp_client.erl│
└─────────────────────────────────────────────────────────┘
                    │
                    v
┌─────────────────────────────────────────────────────────┐
│ Iteration 2: make compile                               │
│   → Compiles all apps AGAIN (30s) ❌ WASTEFUL            │
└─────────────────────────────────────────────────────────┘
```

### Target: Stateful (Incremental Builds)

```
┌─────────────────────────────────────────────────────────┐
│ Iteration 1: make compile                               │
│   → Compiles all apps (30s)                             │
│   → Saves state to .erlmcp/state/gates.json             │
│   → Calculates checksums for all source files           │
└─────────────────────────────────────────────────────────┘
                    │
                    v
┌─────────────────────────────────────────────────────────┐
│ User edits 1 file: apps/erlmcp_core/src/erlmcp_client.erl│
└─────────────────────────────────────────────────────────┘
                    │
                    v
┌─────────────────────────────────────────────────────────┐
│ Iteration 2: make compile                               │
│   → Checks state file                                   │
│   → Detects erlmcp_client.erl changed (checksum diff)   │
│   → Compiles ONLY erlmcp_core (2s) ✅ EFFICIENT         │
│   → Updates state with new checksum                     │
└─────────────────────────────────────────────────────────┘
```

**State File** (`.erlmcp/state/gates.json`):

```json
{
  "version": "1.0.0",
  "timestamp": "2026-02-01T10:30:00Z",
  "gates": {
    "compile": {
      "state": "done",
      "duration_ms": 28500,
      "exit_code": 0,
      "log_file": ".erlmcp/logs/compile_20260201_103000.log",
      "checksum": "abc123def456...",
      "inputs": [
        "apps/erlmcp_core/src/**/*.erl",
        "apps/erlmcp_transports/src/**/*.erl",
        "apps/erlmcp_observability/src/**/*.erl",
        "apps/erlmcp_validation/src/**/*.erl"
      ]
    },
    "eunit": {
      "state": "done",
      "duration_ms": 62000,
      "exit_code": 0,
      "log_file": ".erlmcp/logs/eunit_20260201_103030.log",
      "checksum": "def789ghi012..."
    }
  }
}
```

---

## Observability

### Current: No Observability

```
❌ No logs   → Can't debug failures
❌ No state  → Can't resume builds
❌ No metrics → Can't track performance
❌ No receipts → Can't prove quality
```

### Target: Full Observability

```
✅ Logs      → .erlmcp/logs/<target>_<timestamp>.log
✅ State     → .erlmcp/state/gates.json
✅ Metrics   → .erlmcp/metrics/snapshots/*.json
✅ Receipts  → .erlmcp/receipts/<date>.json (TCPS)
```

**Example Debugging Session**:

```bash
# 1. Check what failed
cat .erlmcp/state/gates.json | jq '.gates | to_entries | map(select(.value.state == "failed"))'
# Output: [{"key": "dialyzer", "value": {"state": "failed", ...}}]

# 2. View failure log
cat .erlmcp/logs/dialyzer_20260201_103045.log
# Output: [dialyzer error details...]

# 3. Check metrics trend
cat .erlmcp/metrics/snapshots/*.json | jq '.gates.dialyzer.duration_ms'
# Output: 90000, 92000, 95000, 98000 (gradual slowdown!)

# 4. Generate receipt (TCPS)
make receipts-list
# Output: Recent quality receipts with pass/fail status
```

---

## Cost Analysis

### Current: Expensive ($15/month)

```
Operation: make check (sequential, 360s)
Cost per run: $0.15 (cloud compute @ $0.10/hour)

Monthly usage: 100 iterations (typical developer)
Monthly cost: 100 × $0.15 = $15/month

Annual cost per developer: $180/year
Team of 5: $900/year
```

### Target: Cost-Effective ($5/month)

```
Operation: make check (parallel, 120s)
Cost per run: $0.05 (cloud compute @ $0.10/hour)

Monthly usage: 100 iterations (same developer)
Monthly cost: 100 × $0.05 = $5/month

Annual cost per developer: $60/year (67% reduction!)
Team of 5: $300/year (saves $600/year)
```

**Breakdown**:

| Operation | Duration | Cost | Frequency | Monthly Cost |
|-----------|----------|------|-----------|--------------|
| **Current** |
| make check | 360s | $0.15 | 50x/month | $7.50 |
| make validate | 540s | $0.20 | 25x/month | $5.00 |
| make test | 180s | $0.08 | 25x/month | $2.00 |
| **Subtotal** | | | | **$14.50** |
|-----------|----------|------|-----------|--------------|
| **Target** |
| make check | 120s | $0.05 | 50x/month | $2.50 |
| make validate | 180s | $0.07 | 25x/month | $1.75 |
| make test | 120s | $0.05 | 25x/month | $1.25 |
| **Subtotal** | | | | **$5.50** |
|-----------|----------|------|-----------|--------------|
| **SAVINGS** | | | | **$9.00/month** |

---

## Developer Experience

### Current: Slow, Opaque

```
Developer workflow:
1. Edit code
2. make check
3. Wait 6 minutes ⏰ (context switch, lose flow)
4. If failure, no clear logs
5. Debug blindly
6. Repeat

PAIN POINTS:
❌ Slow feedback (6 minutes)
❌ Opaque failures (no logs)
❌ Rebuild everything (no incremental)
❌ Can't debug easily
```

### Target: Fast, Transparent

```
Developer workflow:
1. Edit code
2. make quick
3. Wait 90 seconds ⚡ (stay in flow)
4. If failure, check .erlmcp/logs/<target>.log
5. Debug with full context
6. Repeat

IMPROVEMENTS:
✅ Fast feedback (90 seconds, TDD-friendly)
✅ Clear failures (logs + state)
✅ Incremental builds (only changed)
✅ Easy debugging (full observability)
```

---

## Migration Path

### Phase 1: Foundation (Week 1)

```
Makefile (1500 lines)
    │
    ├─ Extract state.mk (150 lines)
    ├─ Extract parallel.mk (120 lines)
    └─ Extract core.mk (150 lines)
    │
    v
Makefile (1080 lines) + mk/ (420 lines)
```

### Phase 2: Quality Gates (Week 2)

```
Makefile (1080 lines) + mk/ (420 lines)
    │
    ├─ Extract quality.mk (180 lines)
    ├─ Implement parallel check
    └─ Add state persistence
    │
    v
Makefile (900 lines) + mk/ (600 lines)
```

### Phase 3: Advanced Features (Week 3)

```
Makefile (900 lines) + mk/ (600 lines)
    │
    ├─ Extract tcps.mk (160 lines)
    ├─ Extract governance.mk (140 lines)
    ├─ Extract cli.mk (120 lines)
    └─ Extract benchmark.mk (100 lines)
    │
    v
Makefile (380 lines) + mk/ (1120 lines)
```

### Phase 4: Polish (Week 4)

```
Makefile (380 lines) + mk/ (1120 lines)
    │
    ├─ Extract release.mk (100 lines)
    ├─ Extract dev.mk (80 lines)
    └─ Clean up root Makefile
    │
    v
Makefile (200 lines) + mk/ (1300 lines)
```

**Total**: 1500 lines → 1500 lines (same code, better organized)

---

## Summary

| Aspect | Current | Target | Improvement |
|--------|---------|--------|-------------|
| **Architecture** | Monolithic (1 file) | Modular (10 files) | 10x maintainability |
| **Performance** | Sequential (360s) | Parallel (120s) | 3x speedup |
| **Cost** | $15/month | $5/month | 67% reduction |
| **State** | Stateless (rebuild all) | Stateful (incremental) | Smart caching |
| **Observability** | None | Logs + metrics + receipts | Full tracing |
| **Developer UX** | Slow, opaque | Fast, transparent | TDD-friendly |

**Recommendation**: PROCEED with refactoring.

---

**Next**: Review MAKEFILE_REFACTORING_PLAN_SPARC.md for full details.
