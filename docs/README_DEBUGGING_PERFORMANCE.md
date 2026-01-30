# Debugging & Performance Optimization Documentation

This directory contains comprehensive documentation for debugging and performance optimization workflows in the erlmcp project.

## Documentation Files

### 📖 Primary Guide
- **`DEBUGGING_PERFORMANCE_CLI_GUIDE.md`** - Complete guide covering all 10 areas:
  1. Debugging workflows and CLI tools
  2. Performance profiling integration
  3. Memory leak detection and prevention
  4. Bottleneck identification strategies
  5. Performance benchmarking workflows
  6. Error analysis and debugging patterns
  7. Monitoring and observability tools
  8. Performance optimization techniques
  9. Load testing and stress testing
  10. Performance reporting and metrics

### 📋 Quick Reference
- **`PERFORMANCE_CHEAT_SHEET.md`** - Quick reference commands and patterns
  - Essential CLI commands
  - Expected performance baselines
  - Common issues and solutions
  - Script shortcuts

### 📊 Benchmarking Documentation
- **`PERFORMANCE_BENCHMARKS.md`** - Benchmark suite documentation
- **`PERFORMANCE_BOTTLENECK_ANALYSIS.md`** - Bottleneck detection guide

### 🔧 Practical Examples
- **`examples/performance_debug_example.erl`** - Erlang module showing debugging patterns
- **`scripts/debug_performance_workflow.sh`** - Complete debugging workflow script

## Key Features

### 1. CLI Integration
The documentation shows how to integrate performance debugging with existing CLI tools:
- `rebar3 compile` - Compilation validation
- `make benchmark` - Performance baselines
- `rebar3 eunit` - Unit testing
- Interactive debugging with observer and recon

### 2. Comprehensive Metrics
All benchmarks output metrology-compliant JSON with standardized metrics:
- `throughput_msg_per_s` - Messages per second
- `latency_p50_us`, `latency_p95_us`, `latency_p99_us` - Latency percentiles
- `memory_heap_mib_per_conn` - Memory per connection
- `memory_rss_mib_per_node` - Total node memory

### 3. Quality Gates
The documentation includes Lean Six Sigma quality gates:
- 0 compilation errors
- 100% test pass rate
- <10% performance regression
- Memory growth <1 MiB/min

### 4. Real Examples
All examples are based on actual erlmcp patterns:
- Registry performance: 553K msg/s baseline
- Network I/O: 43K msg/s baseline
- Stress testing: 372K msg/s sustained

## Usage Workflow

### Quick Start
```bash
# 1. Compile and validate
rebar3 compile && make check

# 2. Run benchmarks
make benchmark

# 3. Profile hot paths
./scripts/debug_performance_workflow.sh

# 4. Monitor performance
./scripts/health/check.sh
```

### Advanced Analysis
```bash
# Memory leak detection
./bench/erlmcp_bench_memory_leak.erl

# Bottleneck analysis
./scripts/analyze/bottlenecks.sh

# Performance regression check
./scripts/performance/check_regression.sh
```

## Integration with Development Workflow

### Pre-commit Hooks
```bash
#!/bin/bash
rebar3 compile && rebar3 eunit && make benchmark-quick
```

### CI/CD Integration
```yaml
- name: Performance Check
  run: |
    rebar3 compile
    make benchmark
    ./scripts/performance/check_regression.sh
```

### Production Monitoring
- OpenTelemetry integration
- Custom metrics collection
- Alerting rules for degradation
- Performance dashboards

## Documentation Principles

1. **Practical Examples** - All patterns work with actual erlmcp code
2. **CLI Integration** - Shows how to use existing tooling effectively
3. **Quality Focus** - Enforces Lean Six Sigma standards
4. **Comprehensive Coverage** - From debugging to optimization
5. **Performance-First** - Measurements drive all decisions

## Contributing

This documentation is generated from actual erlmcp debugging and optimization workflows. To contribute:

1. Test all examples in real environments
2. Update baselines when performance improves
3. Add new patterns as they're discovered
4. Keep metrics format consistent
5. Document quality gate results

## Quality Assurance

All documentation is validated against:
- ✅ Compilation success
- ✅ Test coverage ≥80%
- ✅ No performance regressions
- ✅ Metrology compliance
- ✅ CLI command accuracy
