# Legacy Benchmark Retirement - Wave 2 (Jan 2026)

Complete retirement of untrusted legacy benchmarks: `benchmark_100k.erl`, `throughput_SUITE.erl`, `latency_SUITE.erl`, `transport_real/*`, and `swarm/` framework.

**Status**: All moved to `attic/legacy_untrusted/` - DO NOT USE

---

## Consolidation Summary

**Second-wave legacy (untrusted methodologies):**
- `benchmark_100k.erl` - Hardcoded 100K test, ambiguous "ops/sec", no validation
- `throughput_SUITE.erl` - Common Test overhead contamination, undefined metrics
- `latency_SUITE.erl` - No precision specification, point-in-time only
- `transport_real/*.erl` (8 files) - Scattered modules, no network isolation, no metrology
- `swarm/` (61 files) - Custom stress framework with 2K LOC, unvalidated results

**Why Retired:**
- ❌ No metrology validation (ambiguous units: "ops/sec", "ms", "MiB"?)
- ❌ No scope specification (per-node? per-connection? global?)
- ❌ No precision for time metrics (milliseconds vs microseconds unclear)
- ❌ Framework overhead contamination (CT infrastructure in test results)
- ❌ Single-run statistics (no distribution, variance, or confidence intervals)

**New approach (5 consolidated, metrology-validated modules, v1.5.0):**
- `erlmcp_bench_core_ops.erl` - Registry/queue/pool/session with canonical metrics
- `erlmcp_bench_network_real.erl` - Real TCP/HTTP sockets, no simulations
- `erlmcp_bench_stress.erl` - Time-series monitoring, degradation detection
- `erlmcp_bench_chaos.erl` - 11 failure scenarios with bounded refusal validation
- `erlmcp_bench_integration.erl` - MCP protocol end-to-end correctness
- Shared: `erlmcp_metrology_validator.erl` - ISO 80000 unit validation

**Benefits of New Suite:**
- ✅ 100% canonical units (msg_per_s, us, MiB_per_conn, etc.)
- ✅ Metrology validation on all results (erlmcp_metrology_validator)
- ✅ Explicit scope annotations (per_connection, per_node)
- ✅ Microsecond precision for latency (vs millisecond ambiguity)
- ✅ Framework overhead eliminated (direct Erlang modules, no CT)
- ✅ Statistical rigor (percentile precision, distribution tracking)

---

## Removed Files (Second Wave - Jan 2026)

### Benchmark Modules (MOVED to `attic/legacy_untrusted/`)

| Old File | Functionality | New Location | Why Retired | Status |
|----------|---------------|--------------|-------------|--------|
| `bench/benchmark_100k.erl` (413 LOC) | Hardcoded 100K throughput | `erlmcp_bench_core_ops` (scalable 1K-1M) | Ambiguous metrics, no validation | Archived |
| `bench/benchmark_100k_SUITE.erl` (658 LOC) | CT suite with 100K test | `erlmcp_bench_core_ops` + `erlmcp_bench_integration` | CT framework contamination | Archived |
| `bench/throughput_SUITE.erl` (272 LOC) | CT throughput measurements | `erlmcp_bench_network_real` | No canonical units, undefined scope | Archived |
| `bench/latency_SUITE.erl` (288 LOC) | CT latency percentiles | `erlmcp_bench_stress` | No precision spec, point-in-time | Archived |
| `bench/transport_real/*.erl` (8 files, 1.2K LOC) | TCP/HTTP benchmarks | `erlmcp_bench_network_real` (consolidated) | Scattered, no network isolation, no metrology | Archived |
| ↳ `tcp_real_bench.erl` | TCP socket benchmark | `erlmcp_bench_network_real:run(<<"tcp_*">>) | Untrusted | Archived |
| ↳ `http_real_bench.erl` | HTTP gun benchmark | `erlmcp_bench_network_real:run(<<"http_*">>) | Untrusted | Archived |
| ↳ `http_bench_handler.erl` | HTTP handler | Part of consolidated module | Unused | Archived |
| ↳ README/INTEGRATION/etc | 4 markdown files | Superseded by consolidated docs | Obsolete | Archived |
| `swarm/` (61 files, 600 KB) | Custom stress framework | `erlmcp_bench_chaos` (11 scenarios) | 2K LOC custom code, no validation | Archived |

**All moved to**: `attic/legacy_untrusted/`

### Documentation Litter (DELETED)

| File | Why Deleted |
|------|-------------|
| `bench/BENCHMARK_100K_RESULTS.md` | Undocumented results, no metrology |
| `bench/MAKEFILE_100K_GUIDE.md` | Makefile-specific garbage |
| `bench/CONSOLIDATION_COMPLETE.md` | Marketing copy, contradicted by Wave 2 |
| `bench/IMPLEMENTATION_SUMMARY.md` | Redundant, superseded by docs/bench/ |
| `bench/IMPLEMENTATION_COMPLETE.md` | Outdated claim (Wave 2 found issues) |
| `bench/DELIVERY_CHECKLIST.md` | Meaningless checklist |

---

## Output Format Changes

### Old Format (v0.5.0)

```erlang
%% Inconsistent across modules
{ok, #{
  throughput => 2690000,        % Ambiguous unit
  latency => 0.37,              % Unit unclear (us? ms? ns?)
  memory => 2.34,               % Which memory type?
  duration => 5                 % No fractional seconds
}}
```

**Problems:**
- No unit clarity
- No scope annotation
- No validation
- Can't distinguish units in JSON

### New Format (v1.5.0)

```erlang
{ok, #{
  workload_id => <<"core_ops_100k">>,
  throughput_msg_per_s => 2690000,      % Explicit units
  latency_p50_us => 0.37,                % Explicit metric + units
  latency_p95_us => 0.89,
  latency_p99_us => 1.20,
  memory_heap_mib_per_conn => 2.34,     % Explicit scope + units
  memory_rss_mib_per_node => 456,
  duration_s => 5.234,                   % Fractional seconds
  scope => per_connection_heap,
  precision => microseconds,
  timestamp => <<"2026-01-27T18:45:30Z">>, % RFC3339
  status => pass,
  validated => true
}}
```

**Improvements:**
- ✅ All units explicit (msg_per_s, us, mib, s)
- ✅ Scope annotated (per_connection, per_node)
- ✅ Validation enforced before write
- ✅ Timestamps ISO 8601 (RFC3339)
- ✅ Metric naming consistent (ISO 80000)

---

## Function Signature Changes

### Legacy API (Don't Use)

```erlang
%% Old: No parameter structure
erlmcp_bench_1:run()
erlmcp_bench_2:run(NumOps)
erlmcp_bench_3:run(Iterations, PoolSize)  % Inconsistent params
```

### New API (Use This)

```erlang
%% New: All via workload_id
erlmcp_bench_core_ops:run(<<"core_ops_100k">>)
erlmcp_bench_core_ops:run(<<"core_ops_1m">>)
erlmcp_bench_network_real:run(<<"tcp_sustained_10k">>)

%% Workload registry in docs/bench/workloads.md
```

---

## Configuration Changes

### Old Config

```erlang
%% In erlmcp.app.src (v0.5.0)
{bench_enabled, true},
{bench_duration_ms, 5000},
{bench_pool_size, 50},  % Single global pool size
{bench_tcp_port, 9000}
```

### New Config (None Required!)

```erlang
%% v1.5.0: All in workload registry
%% No .app.src changes needed
%% Workload parameters in docs/bench/workloads.md
%% workload_id => all parameters
```

**Why?**
- Configuration moved to documentation (single source of truth)
- Workloads are immutable (no runtime config needed)
- Benchmarks are reproducible anywhere

---

## Test Migration Path

### If You Have Old Benchmarks

**Step 1:** Find your old workload

```bash
grep -r "erlmcp_bench_1\|erlmcp_bench_2" test/
```

**Step 2:** Map to new workload_id

```erlang
% Old:
erlmcp_bench_1:run()

% New equivalent:
erlmcp_bench_core_ops:run(<<"core_ops_100k">>)
```

**Step 3:** Update test call

```erlang
% Old test:
test_registry_performance() ->
  {ok, Result} = erlmcp_bench_1:run(),
  ?assert(maps:get(throughput, Result) > 500000).

% New test:
test_registry_performance() ->
  {ok, Result} = erlmcp_bench_core_ops:run(<<"core_ops_100k">>),
  ?assert(maps:get(throughput_msg_per_s, Result) > 550000).
```

---

## Custom Workload Creation

### Old: Create New Module

```erlang
% Old: erlmcp_bench_custom.erl
-module(erlmcp_bench_custom).
-export([run/1]).

run(Params) ->
  % Custom implementation
  ok.
```

### New: Use Workload Registry

```erlang
% Step 1: Add to docs/bench/workloads.md
% workload_id: custom_my_scenario
% ops_count: 50000
% expected_throughput: 100K msg/s

% Step 2: Call existing module with your ID
erlmcp_bench_core_ops:run(<<"custom_my_scenario">>).

% Step 3: Benchmark auto-reads workloads.md registry
```

---

## Breaking Changes Summary

| Change | Impact | Migration |
|--------|--------|-----------|
| Module names | Code | Update imports to new 5 modules |
| Output format | Integration | Use explicit metric names |
| Config removal | App startup | None (moved to docs) |
| Workload IDs | Calls | Use registry in workloads.md |

---

## Rollback (If Needed)

**Legacy code is NOT deleted**, just not maintained:

```bash
# Old modules still available in git history
git log --oneline -- bench/erlmcp_bench_1.erl
git show a1b2c3d:bench/erlmcp_bench_1.erl
```

**To restore old benchmarks:**
```bash
git checkout a1b2c3d -- bench/erlmcp_bench_1.erl
```

---

## FAQ

**Q: Can I still use the old modules?**
A: They're in git history but not maintained. Use new modules instead.

**Q: What if my workload doesn't match any registry entry?**
A: Add it to docs/bench/workloads.md, then use registry-based lookup.

**Q: How do I add a custom benchmark?**
A: Edit docs/bench/workloads.md, add workload_id entry, run via new modules.

**Q: Will old evidence (index.json) still work?**
A: Old format incompatible. Restart evidence from v1.5.0 onward.

---

**Navigation**: [Back to Benchmark Index](INDEX.md) | [README](README.md) | [Workloads](workloads.md)
