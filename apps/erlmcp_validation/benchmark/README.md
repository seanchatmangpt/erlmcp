# ERLMCP CLI Performance Benchmarks

## Overview

This directory contains benchmark modules for measuring and optimizing erlmcp CLI performance.

**Performance Targets:**
- CLI startup: **<100ms**
- Simple commands (spec, protocol validation): **<500ms**
- Complex commands (compliance validation): **<2s**

## Benchmark Modules

### 1. erlmcp_cli_startup_bench
Measures CLI startup time from cold start to ready state.

**Metrics:**
- Full startup time (module loading + app initialization)
- Module loading time (isolated)
- App initialization time (isolated)

**Usage:**
```erlang
%% From Erlang shell
erlmcp_cli_startup_bench:run().
erlmcp_cli_startup_bench:run(#{iterations => 100, profile => true}).

%% Via Make
make bench-cli-startup
```

**Output:**
- Console summary with p50/p95/p99 percentiles
- JSON report: `bench/results/cli_startup_<timestamp>.json`
- Optional fprof profile: `/tmp/erlmcp_cli_startup_profile.txt`

### 2. erlmcp_cli_command_bench
Measures execution time for common CLI commands.

**Commands Benchmarked:**
- Spec validation
- Protocol validation
- Transport validation
- Quick check

**Usage:**
```erlang
%% From Erlang shell
erlmcp_cli_command_bench:run().
erlmcp_cli_command_bench:run(#{iterations => 10}).

%% Via Make
make bench-cli-commands
```

**Output:**
- Per-command timing summary
- JSON report: `bench/results/cli_commands_<timestamp>.json`

## Optimized CLI Module

### erlmcp_validate_cli_fast
Fast-mode CLI with aggressive optimizations.

**Optimizations:**
1. **Lazy loading** - Load only required modules
2. **Minimal apps** - Start only crypto (defer ssl, inets)
3. **Quick check** - Ultra-fast health check (<10ms)
4. **Cached results** - Reuse parsed specs (future)

**API:**
```erlang
%% Ultra-fast health check
erlmcp_validate_cli_fast:quick_check().  % <10ms

%% Fast spec validation (minimal checks)
erlmcp_validate_cli_fast:validate_spec_fast().  % Target: <50ms

%% Fast protocol validation (basic structure only)
Message = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1},
erlmcp_validate_cli_fast:validate_protocol_fast(Message).  % Target: <20ms
```

## Running Benchmarks

### Quick Check (Fast)
```bash
make bench-cli-quick
```
Runs 10 iterations of startup benchmark. Takes ~1 minute.

### Full Benchmark Suite
```bash
make bench-cli
```
Runs both startup and command benchmarks. Takes ~5 minutes.

### Profiling
```bash
make profile-cli
```
Generates detailed fprof profile to identify bottlenecks.

Output: `/tmp/erlmcp_cli_startup_profile.txt`

### Manual Execution
```bash
# Compile first
make compile

# Run standalone benchmark
./scripts/bench/run_cli_bench.erl

# Or via Erlang shell (if rebar3 available)
./scripts/bench/cli_bench.sh
```

## Interpreting Results

### Startup Benchmark Output
```
==============================================
RESULTS
==============================================
Full Startup:
  Mean:   85.3 ms
  Median: 83.1 ms
  P95:    92.4 ms
  P99:    98.7 ms
  Min:    78.2 ms
  Max:    102.5 ms

Target: 100 ms | Status: PASS ✓
==============================================
```

**Status Levels:**
- `PASS ✓` - Mean < target
- `WARN ⚠` - Mean < target × 1.5
- `FAIL ✗` - Mean >= target × 1.5

### Command Benchmark Output
```
✓ spec_validation:
  Mean:   420.5 ms (target: 500 ms)
  Median: 415.2 ms
  P95:    480.1 ms
  P99:    495.3 ms

✓ protocol_validation:
  Mean:   85.3 ms (target: 100 ms)
  ...

Summary: 4/4 commands passed
```

### Profiling Output
Key sections in fprof report:

1. **TOTALS** - Overall time distribution
2. **Per-function details** - Individual function breakdown

Look for:
- **ACC** (Accumulated) - Time including callees
- **OWN** (Own) - Function's own execution time
- **CNT** (Count) - Number of calls

Common bottlenecks:
- `application:start/1` - App initialization (target for lazy loading)
- `code:ensure_loaded/1` - Module loading (can be cached)
- `jsx:decode/1` - JSON parsing (consider jiffy)
- `jesse:validate/2` - Schema validation (can be pre-compiled)

## Performance Regression Detection

### Baseline Storage
Each benchmark run stores results in `bench/results/`:
- `cli_startup_<timestamp>.json`
- `cli_commands_<timestamp>.json`

### Comparison
```bash
# Compare two runs
diff bench/results/cli_startup_1234567890.json \
     bench/results/cli_startup_1234567999.json
```

### CI Integration
Add to `.github/workflows/ci.yml`:
```yaml
- name: CLI Performance Check
  run: make bench-cli-quick
  # Parse JSON output and fail if regression >10%
```

## Optimization Strategies

### Implemented
1. ✅ Lazy module loading
2. ✅ Minimal app startup (crypto only)
3. ✅ Quick check mode (no apps)
4. ✅ Benchmarking infrastructure
5. ✅ Profiling tools

### Planned
1. ⏳ ETS caching for parsed specs
2. ⏳ Parallel validation (multiple transports)
3. ⏳ Pre-compiled escript (embedded deps)
4. ⏳ Binary spec storage (avoid JSON parsing)
5. ⏳ Connection pooling for transport validation

### Long-term
1. Persistent validation daemon (avoid startup cost)
2. Erlang NIFs for JSON parsing (jiffy integration)
3. Compiled schemas (avoid runtime compilation)
4. Distributed validation (parallel execution)

## Metrology Compliance

All benchmarks output metrology-compliant JSON:

```json
{
  "benchmark": "cli_startup",
  "timestamp": 1234567890,
  "target_ms": 100,
  "environment": {
    "otp_release": "28",
    "erts_version": "15.0",
    "schedulers": 8,
    "timestamp_iso": "2026-02-01T12:00:00Z"
  },
  "results": {
    "full_startup": {
      "iterations": 100,
      "mean_ms": 85.3,
      "median_ms": 83.1,
      "p95_ms": 92.4,
      "p99_ms": 98.7,
      "min_ms": 78.2,
      "max_ms": 102.5
    }
  },
  "status": "passed"
}
```

**Units:**
- Time: milliseconds (ms) or microseconds (us)
- Percentiles: p50, p95, p99
- Scope: per_command

## Troubleshooting

### "Module not found" errors
```bash
# Ensure project is compiled
make compile

# Check module is in code path
erl -pa _build/default/lib/*/ebin -eval "code:ensure_loaded(erlmcp_cli_startup_bench)."
```

### Benchmark takes too long
```erlang
%% Reduce iterations
erlmcp_cli_startup_bench:run(#{iterations => 5}).
erlmcp_cli_command_bench:run(#{iterations => 3}).
```

### Profiling file not found
```bash
# Check profile output location
ls -la /tmp/erlmcp_cli_startup_profile.txt

# Ensure profiling is enabled
erlmcp_cli_startup_bench:run(#{iterations => 10, profile => true}).
```

## Contributing

When adding new optimizations:

1. **Measure baseline** - Run benchmarks before changes
2. **Implement optimization** - Make targeted changes
3. **Re-measure** - Run benchmarks after changes
4. **Compare** - Calculate improvement percentage
5. **Document** - Update CLI_PERFORMANCE_OPTIMIZATIONS.md
6. **Commit results** - Include before/after metrics

Example commit message:
```
perf(cli): lazy load ssl for spec validation

Before: 200ms mean startup
After:  85ms mean startup
Improvement: 57.5% faster

Benchmark: bench/results/cli_startup_1234567890.json
```

## References

- [CLI_PERFORMANCE_OPTIMIZATIONS.md](../CLI_PERFORMANCE_OPTIMIZATIONS.md) - Detailed optimization strategies
- [CLAUDE.md](../../../CLAUDE.md) - Performance baseline requirements
- [Erlang fprof](https://www.erlang.org/doc/man/fprof.html) - Profiling documentation
- [Erlang eprof](https://www.erlang.org/doc/man/eprof.html) - Alternative profiler
