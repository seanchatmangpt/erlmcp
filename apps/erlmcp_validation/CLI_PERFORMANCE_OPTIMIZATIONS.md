# CLI Performance Optimizations

## Baseline Targets
- **Startup time**: <100ms (currently establishing baseline)
- **Simple commands**: <500ms (spec validation, protocol validation)
- **Complex commands**: <2s (full compliance validation)

## Optimizations Implemented

### 1. Lazy Loading
- **Before**: Load all apps (core, transports, observability, validation) on startup
- **After**: Load only required apps per command
  - Quick check: No apps (module checks only)
  - Spec validation: crypto only
  - Protocol validation: crypto + ssl
  - Full compliance: All apps

**Expected improvement**: 30-50% startup time reduction

### 2. Minimal App Loading
- **Before**: Start erlmcp_core, erlmcp_transports, erlmcp_validation, erlmcp_observability
- **After**: Start only crypto for basic operations
- **Deferred**: Load ssl, inets, asn1, public_key only when needed

**Expected improvement**: 20-40% faster startup for simple commands

### 3. Quick Check Mode
- **erlmcp_validate_cli_fast:quick_check/0**
- No application loading
- Only checks if required modules are loadable
- Returns ok/error immediately

**Expected improvement**: <10ms execution time

### 4. Command-Specific Execution Paths
```erlang
quick_check() -> No apps, module checks only
validate_spec_fast() -> crypto only, minimal validation
validate_protocol_fast(Msg) -> Basic structure check, no schema validation
validate_transport(T) -> crypto + ssl, transport-specific
validate_compliance() -> Full apps, comprehensive validation
```

### 5. Caching (Future)
- Cache parsed specs in ETS
- Cache compiled validators
- Cache server responses (with TTL)
- Cache transport connections for reuse

## Benchmark Modules

### erlmcp_cli_startup_bench
Measures:
- Full CLI startup time
- Module loading time
- App initialization time
- Provides fprof profiling

Usage:
```bash
make bench-cli-startup
```

### erlmcp_cli_command_bench
Measures:
- Spec validation execution time
- Protocol validation execution time
- Transport validation execution time
- Quick check execution time

Usage:
```bash
make bench-cli-commands
```

## Profiling

### Run Profiling
```bash
make profile-cli
```

Outputs to: `/tmp/erlmcp_cli_startup_profile.txt`

Look for:
- **ACC**: Accumulated time (including callees)
- **OWN**: Function's own time
- **CNT**: Call count

### Expected Bottlenecks
1. `application:start/1` - App initialization
2. `code:ensure_loaded/1` - Module loading
3. `jsx:decode/1` - JSON parsing
4. `jesse:validate/2` - Schema validation
5. File I/O operations

## Optimization Strategies

### Short-term (Quick wins)
1. ✅ Lazy module loading
2. ✅ Minimal app startup
3. ✅ Quick check mode
4. ⏳ Cached spec parsing
5. ⏳ Parallel validation

### Medium-term
1. Pre-compiled escripts (include deps)
2. Binary spec storage (avoid JSON parsing)
3. Connection pooling for transport validation
4. Incremental validation (skip unchanged files)

### Long-term
1. Erlang NIFs for JSON parsing (jiffy)
2. Persistent validation daemon (avoid startup cost)
3. Distributed validation (parallel transport checks)
4. Compiled schemas (avoid runtime compilation)

## Regression Detection

### Performance Gates
Added to Makefile:
- `make bench-cli-quick` - Fast sanity check
- `make bench-cli` - Full benchmark suite
- `make profile-cli` - Detailed profiling

### CI Integration
```yaml
- name: CLI Performance Check
  run: make bench-cli-quick
  # Fail if startup >100ms or commands >500ms
```

### Baseline Storage
Results stored in: `bench/results/cli_*.json`

Format:
```json
{
  "benchmark": "cli_startup",
  "timestamp": 1234567890,
  "target_ms": 100,
  "results": {
    "full_startup": {
      "mean_ms": 85.3,
      "median_ms": 83.1,
      "p95_ms": 92.4,
      "p99_ms": 98.7
    }
  },
  "status": "passed"
}
```

## Metrology Compliance

All benchmarks output:
- **throughput_msg_per_s**: Not applicable for CLI
- **latency_p50_us**: 50th percentile in microseconds
- **latency_p95_us**: 95th percentile
- **latency_p99_us**: 99th percentile
- **precision**: "millisecond" or "microsecond"
- **scope**: "per_command"

## Next Steps

1. ✅ Create benchmark infrastructure
2. ⏳ Run baseline measurements
3. ⏳ Implement optimizations
4. ⏳ Re-measure and compare
5. ⏳ Document findings
6. ⏳ Add CI gates

## Usage Examples

### Run Full Benchmark Suite
```bash
make bench-cli
```

### Profile for Bottlenecks
```bash
make profile-cli
less /tmp/erlmcp_cli_startup_profile.txt
```

### Quick Performance Check
```bash
make bench-cli-quick
```

### Test Fast CLI
```erlang
%% From Erlang shell
erlmcp_validate_cli_fast:quick_check().
erlmcp_validate_cli_fast:validate_spec_fast().
```

## Expected Results

### Before Optimization
- Startup: ~200-300ms
- Spec validation: ~800ms
- Protocol validation: ~200ms
- Transport validation: ~2-3s

### After Optimization (Target)
- Startup: <100ms (⬇60-70%)
- Spec validation: <500ms (⬇40%)
- Protocol validation: <100ms (⬇50%)
- Transport validation: <1s (⬇50-60%)

### Quick Check Mode
- Execution: <10ms (⬇95%)

## Memory Optimization

### Current (Estimated)
- Full startup: ~80-100 MB
- Quick check: ~20-30 MB (no apps loaded)

### Strategies
1. Lazy module unloading after command
2. ETS table limits for caches
3. Binary reference counting (avoid copies)
4. Garbage collection tuning for CLI workload

## Parallel Execution

### Transport Validation
Currently sequential:
```erlang
validate_transport(stdio),
validate_transport(tcp),
validate_transport(http)
```

Future parallel:
```erlang
Parent = self(),
Workers = [
  spawn_link(fun() -> Parent ! {stdio, validate_transport(stdio)} end),
  spawn_link(fun() -> Parent ! {tcp, validate_transport(tcp)} end),
  spawn_link(fun() -> Parent ! {http, validate_transport(http)} end)
],
Results = collect_results(Workers, #{})
```

**Expected improvement**: 3x faster for multi-transport validation

## Escript Optimization

### Current
- All dependencies in shebang `-pa` flags
- Dynamic module loading

### Future
- Pre-compiled escript with embedded deps
- Single binary distribution
- Faster startup (no code path searching)

## Conclusion

This optimization effort aims to make the erlmcp CLI:
1. **Fast**: <100ms startup, <500ms for common commands
2. **Efficient**: Minimal memory/CPU for simple checks
3. **Scalable**: Parallel validation for complex tasks
4. **Measurable**: Comprehensive benchmarking and profiling
5. **Maintainable**: Performance gates prevent regression
