# ERLMCP Comprehensive Benchmark Audit Report

## Executive Summary

**Total Benchmark/Stress/Performance Files**: 93 files (across source, test, bench, and swarm directories)
**Total Lines of Code**: ~25,490 LOC
- Bench suite: 3,987 LOC
- Test suite: 13,062 LOC
- Source profilers: 6,316 LOC
- Swarm orchestrators: 2,125 LOC

**Status**: Highly fragmented with significant duplication. Multiple tests measuring identical scenarios across different modules and frameworks. Consolidation opportunity: ~40% of code could be eliminated through unification.

---

## PART 1: COMPLETE BENCHMARK FILE INVENTORY

### 1.1 BENCH/ Directory (4 Files, 3,987 LOC)

#### benchmark_100k.erl (425 lines)
**Type**: Micro-benchmark suite (standalone)
**Components Tested**:
- Registry (100K register + 100K lookup = 200K ops)
- Pool throughput (128 pools × 1000 ops/worker)
- Queue latency (100K push/pop cycles)
- Session management (100 workers × 1000 ops)
- Network I/O (4KB packet manipulation × 100K)
- Memory scaling (incremental 10K → 30K → 60K)
- Integrated system (256 workers × 400 ops = 100K total)
- Sustained load (30 seconds, 200 workers)

**Metrics Measured**:
- Throughput (msg/sec) ✓
- Latency distribution (min/max/avg/p50/p95/p99) ✓
- Jitter (%) - stddev normalized to mean ✓
- Memory delta (MB) ✓
- Operations count ✓

**Current Issues**:
- Uses `erlang:put/get` (process dict) instead of real registry
- Isolated benchmark, not integrated with actual erlmcp
- Latencies in microseconds converted to milliseconds (potential precision loss)
- No external load testing (only synthetic operations)
- Throughput calculation: `(Count / TotalTime) * 1000` - ambiguous time unit handling

**Target Metrics** (hardcoded):
- Throughput: ≥95K msg/sec
- P95: ≤10.0 ms
- P99: ≤15.0 ms

**Status**: KEEP (core measurements valid, needs metrology update)

---

#### benchmark_100k_SUITE.erl (~600 lines)
**Type**: Common Test suite (integrated)
**Components Tested**:
- Registry 100K benchmark
- Pool throughput 100K
- Queue latency 100K
- Session management 100K
- Network I/O 100K
- Memory scaling 100K
- Integrated system 100K
- Mixed workload 100K
- Sustained load 100K
- Spike handling 100K

**Metrics Measured**:
- Same as benchmark_100k.erl, plus:
- Resource usage (CPU, memory via erlang:memory/1)
- Jitter analysis
- Spike response time

**Status**: DUPLICATE - Same tests as benchmark_100k.erl but wrapped in CT framework. Consolidate into single SUITE.

---

#### throughput_SUITE.erl (533 lines)
**Type**: Common Test suite (TAIEA integration)
**Components Tested**:
- Health check (baseline, 10/100/1000 concurrent)
- Entitlement apply (baseline, 10/100/1000 concurrent)
- Receipt verification (baseline, 10/100/1000 concurrent)
- Support model (baseline, 10/100/1000 concurrent)
- Mixed workload (baseline, sustained 30s, spike test)

**Metrics Measured**:
- Throughput (req/sec) ✓
- Latency (min/max/avg/median/p50/p95/p99) ✓
- Timeout violations ✓
- Concurrency scaling (10→100→1000)

**Timeout Targets**:
- Health check: 10ms
- Entitlement: 50ms
- Receipt: 100ms
- Support: 20ms

**Current Issues**:
- Hardcoded timeout expectations (not configurable)
- Mixed workload uses 4 operation types with rand:uniform(4)
- Warmup iterations (100) not customizable
- Benchmark duration hardcoded to 10 seconds

**Status**: KEEP (unique concurrent load testing, different operations than benchmark_100k)

---

#### latency_SUITE.erl (276 lines)
**Type**: Common Test suite (latency-focused)
**Components Tested**:
- Latency stability (60-second test with 10s windows)
- Latency under load (1, 10, 50, 100, 500 concurrent)
- Tail latency analysis (p50/p95/p99/p99.9)
- Latency variance (coefficient of variation)
- Memory per request (1000 requests)

**Metrics Measured**:
- Window-based variance
- P99 latency
- Coefficient of variation (stddev/mean)
- Memory efficiency (bytes/request)

**Current Issues**:
- Variance target is "< 10.0 ms" - should be relative to mean
- Coefficient of variation hardcoded to < 1.0 (may be too strict)
- Memory per request threshold: 10KB (unclear justification)

**Status**: KEEP (unique latency variance testing)

---

### 1.2 BENCH/TRANSPORT_REAL/ Directory (3 Files, ~60 LOC code + docs)

#### tcp_real_bench.erl (25K, but mostly documentation)
**Type**: Real TCP socket benchmarks
**Components Tested**:
- TCP connection establishment latency
- TCP message throughput
- TCP latency percentiles
- Multiple concurrent TCP connections
- TCP failure scenarios

**Status**: DOCUMENTATION - IMPLEMENTATION_COMPLETE.md states this is complete but verification needed

---

#### http_real_bench.erl (20K, mixed code/docs)
**Type**: Real HTTP benchmarks
**Components Tested**:
- HTTP/1.1 request-response latency
- HTTP throughput (req/sec)
- Concurrent HTTP connections
- HTTP failure modes

**Status**: DOCUMENTATION - IMPLEMENTATION_COMPLETE.md states ready for integration

---

#### http_bench_handler.erl (914 bytes)
**Type**: HTTP handler for benchmarks
**Status**: Supporting code for http_real_bench.erl

---

### 1.3 TEST/ Directory (35+ Files, 13,062 LOC)

#### erlmcp_performance_benchmark_SUITE.erl (640 lines)
**Type**: Common Test framework integration test
**Components Tested**:
- Framework initialization
- HTTP transport baseline
- TCP transport baseline
- STDIO transport baseline
- Throughput benchmarking
- Latency benchmarking
- Memory usage profiling
- CPU utilization monitoring
- Regression detection
- Bottleneck identification
- Optimization recommendations
- Monitoring lifecycle
- Results storage/retrieval
- Trend analysis

**Metrics Measured**:
- Throughput (msg/s)
- Latency (mean, median, p95, p99, samples)
- Memory (baseline, peak, average, growth in KB)
- CPU (average %, peak %)
- Regression detection (20% threshold)

**Current Issues**:
- Tests mock transport types (http, tcp, stdio) - actual benchmarks not integrated
- Regression threshold hardcoded to 20%
- Error handling for unsupported transports (TCP)
- Relies on erlmcp_performance_benchmark module (may not be implemented)

**Status**: NEEDS_VERIFICATION - References backend module that may not exist

---

#### erlmcp_cluster_stress_SUITE.erl (~500 lines estimated)
**Type**: Multi-node cluster stress test
**Components Tested**:
- Cluster formation (4 nodes)
- Inter-node connectivity
- Connection scaling (100 → 1K → 10K → 25K → 100K)
- Sustained 100K connections
- Message throughput at 100K
- Latency distribution at 100K
- Node failure detection
- Cluster recovery after failure
- Graceful shutdown

**Metrics Measured**:
- Active connections
- Throughput (msg/sec)
- Latency (p50/p95/p99)
- Failure detection time
- Recovery time

**Target**: <100ms P99 latency at 100K scale

**Status**: KEEP (unique cluster-specific testing)

---

#### erlmcp_registry_100k_stress_SUITE.erl (~600 lines estimated)
**Type**: Registry-specific stress test
**Components Tested**:
- Baseline lookup (10K, 50K, 100K concurrent)
- Concurrent register + lookup (50/50 split)
- Binding stress with routing
- Message routing at 100K
- Partition balance
- Sustained load (30+ sec @ 100K ops/sec)
- Latency histogram
- Contention under load

**Metrics Measured**:
- Lookup latency p99 < 100µs
- Throughput > 100K ops/sec
- Partition balance
- Contention metrics

**Status**: KEEP (registry-specific micro-benchmarks)

---

#### erlmcp_chaos_tests.erl (100+ lines)
**Type**: Chaos injection tests
**Scenarios**:
- Inject latency (fixed duration)
- Inject error rate (probability-based)
- Inject packet loss (probability-based)
- Inject network partition
- Inject CPU spike
- Inject memory pressure
- Run chaos scenarios

**Status**: SKELETON - Tests defined but backend (erlmcp_chaos module) may not exist

---

#### erlmcp_chaos_tracking.erl
**Type**: Chaos state tracking
**Purpose**: Monitor active chaos injections, track scenarios

**Status**: UNCLEAR - Needs verification that chaos module exists

---

#### erlmcp_advanced_load_stress_SUITE.erl
**Type**: Advanced load testing (estimated ~300 lines)
**Purpose**: Stress testing with simulated failures

**Status**: NEEDS_REVIEW

---

#### erlmcp_connection_pool_stress_SUITE.erl
**Type**: Connection pool stress test
**Purpose**: Test pool behavior under load, connection saturation

**Status**: NEEDS_REVIEW

---

#### erlmcp_error_100k_stress_SUITE.erl
**Type**: Error injection at 100K scale
**Purpose**: Test system stability under error conditions

**Status**: NEEDS_REVIEW

---

#### erlmcp_logging_stress_test.erl
**Type**: Logging performance under stress
**Purpose**: Verify logging doesn't become bottleneck

**Status**: NEEDS_REVIEW

---

#### erlmcp_simple_benchmark.erl
**Type**: Minimal benchmark
**Purpose**: Quick performance check

**Status**: CANDIDATE_FOR_REMOVAL (too basic, replaced by comprehensive suites)

---

#### erlmcp_simple_stress.erl
**Type**: Minimal stress test
**Purpose**: Basic stress testing

**Status**: CANDIDATE_FOR_REMOVAL (too basic)

---

#### erlmcp_stress_cascading_tests.erl
**Type**: Cascading failure tests
**Purpose**: Test recovery from cascading failures

**Status**: NEEDS_REVIEW

---

#### erlmcp_stress_validation.erl
**Type**: Results validation framework
**Purpose**: Validate benchmark results against targets

**Status**: UTILITY - Keep for regression detection

---

#### stress_test_queue.erl
**Type**: Queue-specific stress test
**Purpose**: Test queue under sustained load

**Status**: CANDIDATE_FOR_CONSOLIDATION (could be part of benchmark_100k_SUITE)

---

#### tcp_real_bench_tests.erl
**Type**: TCP benchmark test wrapper
**Purpose**: Wrapper around tcp_real_bench.erl

**Status**: CANDIDATE_FOR_REMOVAL (duplicate with bench/transport_real/tcp_real_bench.erl)

---

#### transport_performance_benchmark.erl
**Type**: Transport-agnostic performance tests
**Purpose**: Benchmark all transports uniformly

**Status**: KEEP - Central transport benchmarking

---

#### erlmcp_cli_bench_SUITE.erl
**Type**: CLI interface for benchmarking
**Purpose**: Benchmark via CLI (may be outdated)

**Status**: NEEDS_VERIFICATION

---

#### erlmcp_cli_chaos_SUITE.erl
**Type**: CLI interface for chaos
**Purpose**: Chaos testing via CLI

**Status**: NEEDS_VERIFICATION

---

#### config_100k_benchmark.erl
**Type**: Configuration for 100K benchmarks
**Purpose**: Centralized config

**Status**: UTILITY - Keep for configuration management

---

#### erlmcp_benchmark.erl
**Type**: Generic benchmark module (likely minimal/deprecated)
**Status**: CANDIDATE_FOR_REMOVAL

---

#### erlmcp_chaos_injection.erl
**Type**: Chaos injection implementation (if exists)
**Status**: NEEDS_VERIFICATION

---

#### erlmcp_chaos_supervision_tests.erl
**Type**: Tests for chaos-enabled supervision
**Status**: NEEDS_REVIEW

---

#### erlmcp_chaos_test_SUITE.erl
**Type**: Common Test wrapper for chaos
**Status**: LIKELY_DUPLICATE (same as erlmcp_chaos_tests.erl)

---

#### erlmcp_master_stress_test.erl
**Type**: Older version of orchestrator
**Status**: CANDIDATE_FOR_REMOVAL (replaced by erlmcp_master_stress_orchestrator)

---

#### erlmcp_profile_manager_tests.erl
**Type**: Tests for profile manager
**Status**: KEEP - Supports profiling infrastructure

---

#### erlmcp_routing_benchmark.erl
**Type**: Message routing performance
**Status**: KEEP - Specific to routing layer

---

#### erlmcp_stress_results_collector.erl
**Type**: Results collection/aggregation
**Status**: UTILITY - Keep for reporting

---

#### registry_100k_stress.erl
**Type**: Registry stress (may be duplicate of SUITE)
**Status**: CANDIDATE_FOR_REMOVAL

---

#### tcps_ontology_benchmark.erl
**Type**: TCPS-specific ontology benchmarks
**Status**: KEEP - TCPS-specific

---

#### tcps_persistence_performance_SUITE.erl
**Type**: TCPS persistence layer performance
**Status**: KEEP - TCPS-specific

---

#### integration/tcps_performance_SUITE.erl
**Type**: TCPS integration performance
**Status**: KEEP - TCPS-specific

---

#### metrology/benchmark_report_format_tests.erl
**Type**: Report formatting tests
**Status**: UTILITY - Keep for reporting

---

### 1.4 SRC/ Directory (13+ Files, 6,316 LOC)

#### erlmcp_profiling_suite.erl (200+ lines)
**Type**: Unified profiling coordinator
**Components**:
- CPU profiler coordination
- Memory profiler coordination
- Latency profiler coordination
- Bottleneck detector coordination

**Status**: COORDINATOR - Keep as central point

---

#### erlmcp_cpu_profiler.erl
**Type**: CPU profiling
**Metrics**: CPU utilization, per-process CPU time
**Status**: KEEP - Essential profiling tool

---

#### erlmcp_memory_profiler.erl
**Type**: Memory profiling
**Metrics**: Memory usage by process, memory trends
**Status**: KEEP - Essential profiling tool

---

#### erlmcp_latency_profiler.erl
**Type**: Latency distribution analysis
**Metrics**: Latency percentiles, jitter
**Status**: KEEP - Essential profiling tool

---

#### erlmcp_performance_benchmark.erl
**Type**: Generic performance benchmark framework
**Purpose**: Central framework for all benchmarks
**Status**: KEEP - Core infrastructure

---

#### erlmcp_queue_benchmark.erl
**Type**: Queue-specific benchmarking
**Status**: CANDIDATE_FOR_CONSOLIDATION (into benchmark_100k_SUITE)

---

#### erlmcp_cli_bench.erl
**Type**: CLI interface to benchmarking
**Status**: KEEP - User-facing interface

---

#### erlmcp_cli_chaos.erl
**Type**: CLI interface to chaos
**Status**: KEEP - User-facing interface

---

#### erlmcp_bench_plan_validator.erl
**Type**: Validates benchmark plans before execution
**Status**: UTILITY - Keep for safety

---

#### erlmcp_chaos_plan_validator.erl
**Type**: Validates chaos plans before execution
**Status**: UTILITY - Keep for safety

---

#### erlmcp_config_profiles.erl
**Type**: Configuration profiles for benchmarks
**Status**: UTILITY - Keep for consistency

---

#### erlmcp_profile_manager.erl
**Type**: Manages profiler lifecycle
**Status**: KEEP - Core infrastructure

---

#### erlmcp_dashboard_stress_test.erl
**Type**: Dashboard integration for stress tests
**Status**: UTILITY - Keep for visualization

---

### 1.5 SWARM/STRESS-TEST/ Directory (5 Files, 2,125 LOC)

#### erlmcp_master_stress_orchestrator.erl (100+ lines)
**Type**: Master orchestrator for 100K test
**Phases**:
1. Initialization
2. Ramp-up (0 → 100K in 120s @ 5K conn/sec)
3. Sustained load (100K for 600s)
4. Failure scenarios
5. Cooldown

**Failure Scenarios**:
- Node failure at 60s
- Network partition at 150s
- Queue overflow at 300s

**Status**: KEEP - Central orchestration point

---

#### erlmcp_logging_100k_stress.erl
**Type**: Logging stress test (100K scale)
**Purpose**: Verify logging under load
**Status**: KEEP - Specific stress scenario

---

#### erlmcp_concurrent_100k.erl
**Type**: Pure concurrency stress test
**Purpose**: 100K concurrent workers generating load
**Status**: KEEP - Specific stress scenario

---

#### erlmcp_100k_comprehensive.erl
**Type**: Comprehensive 100K test (all components)
**Purpose**: Integrated stress test of all subsystems
**Status**: KEEP - Integration point

---

#### erlmcp_metrics_collector.erl
**Type**: Real-time metrics collection during stress
**Purpose**: Gather metrics for analysis
**Status**: UTILITY - Keep for data collection

---

### 1.6 ROOT DIRECTORY (2 Standalone Tests)

#### quick_stress_test.erl
**Type**: Quick ad-hoc stress test
**Status**: CANDIDATE_FOR_REMOVAL (development artifact)

---

#### run_stress_test.erl
**Type**: Stress test runner
**Status**: CANDIDATE_FOR_REMOVAL (superseded by orchestrators)

---

#### test_logging_stress.erl
**Type**: Logging stress test variant
**Status**: CANDIDATE_FOR_REMOVAL (duplicate with swarm version)

---

#### test_quick_stress.erl
**Type**: Quick stress variant
**Status**: CANDIDATE_FOR_REMOVAL (duplicate with root version)

---

---

## PART 2: CATEGORIZATION BY BENCHMARK TYPE

### 2.1 Micro-Benchmarks (Single Component)

| File | Component | Metrics | LOC | Notes |
|------|-----------|---------|-----|-------|
| benchmark_100k.erl | Registry, Pool, Queue, Session, Network, Memory | Throughput, Latency p50/p95/p99, Jitter | 425 | KEEP - Baseline |
| erlmcp_registry_100k_stress_SUITE.erl | Registry | Lookup latency, Throughput, Partition balance | 600 | KEEP - Registry-specific |
| erlmcp_queue_benchmark.erl | Queue | Throughput, Latency | ? | CONSOLIDATE |
| erlmcp_routing_benchmark.erl | Message routing | Throughput, Latency | ? | KEEP |
| transport_performance_benchmark.erl | All transports | Throughput, Latency, Memory | ? | KEEP |

### 2.2 Integration Benchmarks (Multiple Components)

| File | Scope | Scale | Duration | Notes |
|------|-------|-------|----------|-------|
| benchmark_100k_SUITE.erl | Registry + Pool + Queue + Session + Network + Memory | 100K | Variable | CONSOLIDATE with benchmark_100k.erl |
| throughput_SUITE.erl | Health/Entitlement/Receipt/Support | 1-1000 concurrent | 10s | KEEP - Unique operations |
| erlmcp_cluster_stress_SUITE.erl | Cluster (4 nodes) | 100K (25K/node) | 5min+ | KEEP - Cluster-specific |

### 2.3 Stress/Load Tests

| File | Load Type | Scale | Duration | Notes |
|------|-----------|-------|----------|-------|
| erlmcp_master_stress_orchestrator.erl | Phased load + failures | 100K | 15min+ | KEEP - Master orchestrator |
| erlmcp_concurrent_100k.erl | Pure concurrency | 100K | Variable | KEEP |
| erlmcp_100k_comprehensive.erl | All subsystems | 100K | Variable | KEEP - Integration |
| erlmcp_advanced_load_stress_SUITE.erl | Advanced load + failures | High | Variable | NEEDS_REVIEW |
| erlmcp_connection_pool_stress_SUITE.erl | Pool saturation | Variable | Variable | KEEP |
| erlmcp_error_100k_stress_SUITE.erl | Error injection | 100K | Variable | KEEP |
| erlmcp_logging_stress_test.erl | Logging bottleneck | 100K | Variable | KEEP |

### 2.4 Chaos/Adversarial Tests

| File | Scenarios | Implementation | Notes |
|------|-----------|-----------------|-------|
| erlmcp_chaos_tests.erl | Latency, error, loss, partition, CPU, memory | SKELETON | VERIFY backend exists |
| erlmcp_chaos_injection.erl | Actual chaos injection | ? | NEEDS_VERIFICATION |
| erlmcp_chaos_supervision_tests.erl | Supervision under chaos | ? | NEEDS_REVIEW |
| erlmcp_stress_cascading_tests.erl | Cascading failures | ? | KEEP |

### 2.5 Network/Transport Tests

| File | Transport | Type | Notes |
|------|-----------|------|-------|
| tcp_real_bench.erl | TCP | Real socket | INTEGRATION_READY |
| http_real_bench.erl | HTTP | Real socket | INTEGRATION_READY |
| erlmcp_transport_tcp_4kb.erl | TCP | 4KB packets | MICROBENCK |

### 2.6 Profiling/Monitoring Tests

| File | Purpose | Status |
|------|---------|--------|
| erlmcp_profiling_suite.erl | Unified profiler coordinator | KEEP |
| erlmcp_cpu_profiler.erl | CPU profiling | KEEP |
| erlmcp_memory_profiler.erl | Memory profiling | KEEP |
| erlmcp_latency_profiler.erl | Latency profiling | KEEP |
| erlmcp_profile_manager_tests.erl | Profile manager tests | KEEP |

### 2.7 Utility/Infrastructure

| File | Purpose | Status |
|------|---------|--------|
| erlmcp_stress_results_collector.erl | Result collection | KEEP |
| erlmcp_stress_validation.erl | Result validation | KEEP |
| config_100k_benchmark.erl | Configuration | KEEP |
| erlmcp_bench_plan_validator.erl | Plan validation | KEEP |
| erlmcp_chaos_plan_validator.erl | Plan validation | KEEP |
| metrology/benchmark_report_format_tests.erl | Report formatting | KEEP |

---

## PART 3: DUPLICATE ANALYSIS

### 3.1 Direct Duplicates (Same Code, Different Wrappers)

| Original | Duplicate | Type | Action |
|----------|-----------|------|--------|
| benchmark_100k.erl | benchmark_100k_SUITE.erl | Micro-benchmark | **CONSOLIDATE** - SUITE is wrapper, same tests |
| erlmcp_chaos_tests.erl | erlmcp_chaos_test_SUITE.erl | Chaos tests | **CONSOLIDATE** - One is wrapper |
| erlmcp_simple_benchmark.erl | erlmcp_simple_stress.erl | Basic benchmark | **REMOVE** - Too minimal, superseded |
| tcp_real_bench_tests.erl | bench/transport_real/tcp_real_bench.erl | TCP benchmarks | **CONSOLIDATE** - Same module, different locations |
| test_logging_stress.erl | swarm/stress-test/erlmcp_logging_100k_stress.erl | Logging stress | **REMOVE** - Root version is old |
| test_quick_stress.erl | quick_stress_test.erl | Quick stress | **REMOVE** - Development artifact |
| erlmcp_master_stress_test.erl | erlmcp_master_stress_orchestrator.erl | Orchestrator | **REMOVE** - Old version |

### 3.2 Semantic Duplicates (Similar Code, Different Focuses)

| File 1 | File 2 | Similarity | Action |
|--------|--------|-----------|--------|
| benchmark_100k.erl | throughput_SUITE.erl | Both measure throughput | KEEP_BOTH - Different workloads |
| erlmcp_registry_100k_stress_SUITE.erl | erlmcp_registry_contention.erl | Registry stress | **CONSOLIDATE** - Merge contention metrics into SUITE |
| registry_100k_stress.erl | erlmcp_registry_100k_stress_SUITE.erl | Registry stress | **REMOVE** - Non-SUITE version |
| erlmcp_queue_benchmark.erl | benchmark_100k.erl queue section | Queue benchmarking | **CONSOLIDATE** - Merge into comprehensive suite |
| erlmcp_stress_cascading_tests.erl | erlmcp_advanced_load_stress_SUITE.erl | Advanced load | **REVIEW** - Determine if different scenarios |

### 3.3 Overlapping Scenarios

These tests measure the same thing but with different harnesses:

| Scenario | Files | Recommendation |
|----------|-------|-----------------|
| 100K throughput | benchmark_100k.erl, benchmark_100k_SUITE.erl, erlmcp_100k_comprehensive.erl | **Standardize on SUITE**, use comprehensive for integration |
| 100K registry | erlmcp_registry_100k_stress_SUITE.erl, erlmcp_registry_contention.erl, registry_100k_stress.erl | **Single registry SUITE**, include contention metrics |
| Chaos injection | erlmcp_chaos_tests.erl, erlmcp_chaos_injection.erl, erlmcp_chaos_test_SUITE.erl | **Clarify if module exists**, consolidate to single SUITE |
| Logging stress | erlmcp_logging_stress_test.erl, erlmcp_logging_100k_stress.erl | **Keep only swarm version**, remove root version |

---

## PART 4: METROLOGY AUDIT - MEASUREMENT ISSUES

### 4.1 Ambiguous Metrics

| Metric | Issue | Files | Fix |
|--------|-------|-------|-----|
| **Throughput (msg/sec)** | Unclear what "msg" means. Is it: operation count, network packet, MCP message, or protocol frame? | benchmark_100k.erl:344, throughput_SUITE.erl:424 | **Standardize**: Define "msg" as "complete MCP request-response cycle" |
| **Latency units** | Some use microseconds (µs), some milliseconds (ms). Conversion: `(End - Start) / 1000` assumes microseconds input. | benchmark_100k.erl:296, latency_SUITE.erl:224 | **Standardize**: All measurements in microseconds, convert to ms only for reporting |
| **Jitter (%)** | Defined as `(StdDev / Avg) * 100`. Problem: if Avg < 1ms, jitter can exceed 1000%. Capped at 9999% (line 356). | benchmark_100k.erl:343-356 | **Better definition**: Use "coefficient of variation" explicitly, cap at 5.0 for reasonable bounds |
| **Memory delta** | Uses `erlang:memory(total)` which includes entire VM. Not per-process. Meaningless for isolated benchmarks. | benchmark_100k.erl:194, latency_SUITE.erl:134 | **Fix**: Use `erlang:memory(processes)` or process-specific `process_info/2` |
| **P99 latency** | Index calculation: `erlang:max(1, erlang:round(Count * Percent / 100))` truncates decimals. For 100-element list, p99 index = 1, not 99. | latency_SUITE.erl:254, benchmark_100k.erl:362 | **Fix**: Use `erlang:ceil()` instead of `erlang:round()` to ensure p99 > p50 |
| **Concurrency "workers"** | Some tests spawn workers with `spawn_monitor/1`, don't measure actual concurrency. Workers may finish before new ones spawn. | benchmark_100k.erl:260-273 | **Fix**: Use proper process pool or sustained-duration loop |

### 4.2 Inconsistent Measurement Timeframes

| Test | Duration | Measurement | Issue |
|------|----------|-------------|-------|
| benchmark_100k.erl | ~1-2 seconds (single registry.erl:77-100) | Sequential, no warmup | Too short, no stability analysis |
| throughput_SUITE.erl | 10 seconds (hardcoded) | With 100 warmup iterations | Mixes warmup + measurement |
| latency_SUITE.erl | 60 seconds (10 windows × 6 sec) | Per-window analysis | Good, but no cross-window correlation |
| sustained load | 30 seconds (benchmark_100k.erl:248) | Continuous load | Good, but is 30s enough for stability? |
| erlmcp_cluster_stress_SUITE.erl | 5+ minutes per test | Phase-based | Good, but needs sub-phase granularity |

**Recommendation**:
- Minimum warmup: 1 second (100 operations)
- Measurement window: 10 seconds minimum
- Sustained test: 60+ seconds
- Use fixed-time loops, not fixed operation counts

### 4.3 Undefined Baseline Comparisons

| Test | Target | Baseline | Issue |
|------|--------|----------|-------|
| benchmark_100k.erl | Throughput ≥95K msg/sec | Hardcoded | No comparison to previous runs |
| latency_SUITE.erl | Variance < 10.0 ms | Hardcoded | Absolute not relative |
| erlmcp_performance_benchmark_SUITE.erl | Regression threshold 20% | Hardcoded | No actual baseline stored |
| throughput_SUITE.erl | P95 ≤ timeout ms | Hardcoded per operation | Can't detect slow degradation |

**Recommendation**:
- Store baseline on first run: `/tmp/erlmcp_baseline_${transport}_${timestamp}.json`
- Compare each run to baseline
- Flag warnings at: 5%, 10%, 20% regression points
- Use git-based baseline tracking for historical trends

### 4.4 Missing Edge Case Measurements

| Edge Case | Measured? | Files | Fix |
|-----------|-----------|-------|-----|
| 0ms latency (measurement overhead) | No | All | Subtract system_time() measurement overhead |
| Extremely high percentiles (p99.99, p99.999) | No | Most | Add to all latency tests |
| Tail latency spikes (duration, frequency) | No | latency_SUITE only | Add spike detection to all |
| Memory footprint at different scales | Limited | latency_SUITE | Add 10K, 50K, 100K, 500K scale tests |
| Contention under load | Registry only | erlmcp_registry_contention | Extend to other components |
| Recovery time (after failure injection) | Mentioned | erlmcp_cluster_stress_SUITE | Not systematically measured |

---

## PART 5: CONSOLIDATION RECOMMENDATIONS

### 5.1 Phase 1: Immediate Cleanup (30% reduction)

**Remove These Files** (~1,000 LOC):
```
test/erlmcp_simple_benchmark.erl        (minimal)
test/erlmcp_simple_stress.erl           (minimal)
test/registry_100k_stress.erl           (non-SUITE duplicate)
test_logging_stress.erl                 (old variant)
test_quick_stress.erl                   (old artifact)
quick_stress_test.erl                   (old artifact)
run_stress_test.erl                     (superseded by orchestrators)
erlmcp_master_stress_test.erl           (old version)
tcp_real_bench_tests.erl                (duplicate with bench/)
```

**Consolidate These**:
```
benchmark_100k.erl → benchmark_100k_SUITE.erl
  (Keep SUITE, add standalone runner entry point)

erlmcp_chaos_tests.erl + erlmcp_chaos_test_SUITE.erl
  (Keep SUITE only)

erlmcp_registry_contention.erl → erlmcp_registry_100k_stress_SUITE.erl
  (Merge contention metrics into SUITE)

erlmcp_queue_benchmark.erl → benchmark_100k_SUITE.erl
  (Merge queue-specific metrics into comprehensive suite)
```

**Result**: ~1,000 LOC removed, ~400 LOC consolidated = 1,400 LOC reduction

### 5.2 Phase 2: Metrology Improvements (1-2 weeks)

**Priority A - Critical Fixes**:
1. Define "throughput msg" explicitly (request-response cycle)
2. Fix percentile calculation (use ceil for p99)
3. Standardize all latencies to microseconds internally
4. Fix memory measurement (use processes, not total)
5. Add measurement overhead calibration

**Priority B - Infrastructure**:
1. Create `benchmark_baseline.erl` module for baseline storage/comparison
2. Add regression detection framework
3. Implement automatic statistical significance testing
4. Create centralized metrics collection point

**Priority C - New Measurements**:
1. Add p99.9 and p99.99 to all latency tests
2. Add spike detection (duration, frequency, magnitude)
3. Add contention metrics beyond registry
4. Add recovery time measurement for all failure scenarios

### 5.3 Phase 3: Structural Refactoring (2-3 weeks)

**Create unified benchmark framework**:
```erlang
-module(erlmcp_benchmark).

%% API for all benchmarks
run_benchmark(#{
    type => Type,           % micro | integration | stress | chaos
    scale => Scale,         % 1, 10, 100, 1000, 10000, 100000
    duration_sec => Sec,    % 10-300
    workers => Count,       % concurrent workers
    warmup_sec => WarmupSec,
    target_metric => Metric,
    target_value => Value,
    tolerance_pct => Pct
}) -> {ok, Results} | {error, Reason}
```

**Create test matrix**:
```
Scale:          [1, 10, 100, 1000, 10000, 100000]
Type:           [registry, queue, routing, session, network]
Workload:       [baseline, spike, sustained, mixed]
Failures:       [none, latency, errors, partition]
```

**Standardize result format**:
```erlang
-record(benchmark_result, {
    timestamp :: integer(),
    benchmark_id :: binary(),
    type :: atom(),
    scale :: integer(),
    workers :: integer(),
    duration_ms :: integer(),

    % Required metrics
    throughput_mps :: float(),
    latency_p50_us :: float(),
    latency_p95_us :: float(),
    latency_p99_us :: float(),
    latency_p99_9_us :: float(),

    % Optional metrics
    memory_per_op_bytes :: float() | undefined,
    cpu_percent :: float() | undefined,
    error_rate_pct :: float() | undefined,
    jitter_pct :: float() | undefined,

    % Results
    passed :: boolean(),
    target_metric :: atom() | undefined,
    target_value :: number() | undefined,
    actual_value :: number() | undefined
}).
```

---

## PART 6: WHAT'S WORTH KEEPING

### Keep These Intact

| File | Reason | Integration |
|------|--------|-------------|
| benchmark_100k.erl/.._SUITE.erl | Core 100K benchmark | Use as comprehensive baseline |
| throughput_SUITE.erl | Unique TAIEA workloads | Add to daily CI |
| latency_SUITE.erl | Stability analysis | Use for regression detection |
| erlmcp_cluster_stress_SUITE.erl | Multi-node testing | Add to nightly suite |
| erlmcp_registry_100k_stress_SUITE.erl | Registry micro-benchmarks | Use for optimization validation |
| erlmcp_profiling_suite.erl | Unified profiler | Keep as coordinator |
| erlmcp_master_stress_orchestrator.erl | Phase orchestration | Keep as master runner |
| erlmcp_100k_comprehensive.erl | Integration testing | Use as end-to-end validation |

### Transform These

| File | Current → Proposed |
|------|-------------------|
| erlmcp_performance_benchmark.erl | Generic backend → Unified measurement framework |
| transport_performance_benchmark.erl | Transport-specific → Transport-agnostic with transport parameter |
| tcp_real_bench.erl + http_real_bench.erl | Standalone → Integration into transport_performance_benchmark |
| erlmcp_chaos_tests.erl | Skeleton → Full chaos scenario suite |

### Remove These

| File | Reason |
|------|--------|
| erlmcp_simple_benchmark.erl | Too basic, use benchmark_100k instead |
| erlmcp_simple_stress.erl | Too basic, use comprehensive suite instead |
| registry_100k_stress.erl | Non-SUITE duplicate |
| test_logging_stress.erl | Old version, swarm version is current |
| quick_stress_test.erl | Development artifact |
| erlmcp_master_stress_test.erl | Old version, orchestrator is current |

---

## PART 7: IMPLEMENTATION PLAN

### Week 1: Cleanup & Consolidation
- [ ] Remove 9 identified files (Phase 1 list)
- [ ] Consolidate benchmark_100k.erl into SUITE
- [ ] Consolidate erlmcp_registry_contention into registry SUITE
- [ ] Consolidate erlmcp_queue_benchmark into comprehensive SUITE
- [ ] Verify all chaos modules exist

### Week 2: Metrology Fixes
- [ ] Fix percentile calculations (use ceil)
- [ ] Standardize latency units (microseconds)
- [ ] Fix memory measurements
- [ ] Add measurement overhead calibration
- [ ] Add missing percentiles (p99.9, p99.99)

### Week 3: Infrastructure
- [ ] Create benchmark_baseline.erl module
- [ ] Create regression detection framework
- [ ] Implement baseline storage/comparison
- [ ] Add statistical significance testing
- [ ] Create metrics collection point

### Week 4: Integration & Documentation
- [ ] Create unified benchmark framework
- [ ] Document measurement definitions (ISO metrology)
- [ ] Create test matrix configuration
- [ ] Write best practices guide
- [ ] Create CI integration examples

---

## PART 8: MEASUREMENT STANDARDIZATION (ISO 8601-Style)

### Definition: Throughput (Messages Per Second)

**Definition**: Number of complete MCP request-response cycles per second.
**Unit**: cycles/sec or msg/sec
**Calculation**: `(CompletedRequests / DurationSeconds)`
**Warmup**: Exclude first 1 second or 100 operations
**Measurement Duration**: Minimum 10 seconds
**Reporting**: Report both mean and stddev

### Definition: Latency (Microseconds)

**Definition**: Time from request send to response received
**Unit**: Microseconds (µs), report as milliseconds (ms) in summaries
**Measurement Method**: `erlang:system_time(microsecond)` at start and end
**Calibration**: Subtract measurement overhead (~0.5µs per call)
**Percentiles Required**: p50, p95, p99, p99.9 minimum
**Percentile Calculation**:
```erlang
Index = erlang:ceil(Count * (Percent / 100))
lists:nth(Index, SortedList)
```

### Definition: Jitter (Dimensionless)

**Definition**: Coefficient of variation = stddev / mean
**Bounds**: 0.0 (perfect), no upper bound (high variability)
**Acceptable Range**: < 0.5 (50% relative variation)
**Calculation**:
```erlang
Mean = lists:sum(Values) / length(Values),
Variance = lists:sum([(V - Mean)^2 || V <- Values]) / length(Values),
StdDev = math:sqrt(Variance),
Jitter = StdDev / Mean
```

### Definition: Memory Per Operation

**Definition**: Heap memory allocated per request cycle
**Unit**: Bytes
**Measurement Method**: `erlang:memory(processes)` before/after 1000 ops
**Average**: `(AfterMem - BeforeMem) / 1000`
**Note**: Includes VM overhead, garbage collection effects

---

## APPENDIX: FILE LOCATIONS & SIZES

**BENCH/ (3,987 LOC)**:
- benchmark_100k.erl (425)
- benchmark_100k_SUITE.erl (~600)
- throughput_SUITE.erl (533)
- latency_SUITE.erl (276)
- erlmcp_registry_contention.erl (~400)
- erlmcp_transport_tcp_4kb.erl (~400)
- transport_real/tcp_real_bench.erl (25KB, mostly docs)
- transport_real/http_real_bench.erl (20KB, mostly docs)
- transport_real/http_bench_handler.erl (914 bytes)

**TEST/ (13,062 LOC)**:
- 35+ test files
- Range: 100-640 lines per file
- Includes SUITE modules, chaos tests, stress tests

**SRC/ (6,316 LOC)**:
- 13+ profiler/benchmark modules
- Includes CPU, memory, latency profilers
- Includes performance benchmark framework

**SWARM/STRESS-TEST/ (2,125 LOC)**:
- 5 orchestrator modules
- Master orchestrator (100+ lines)
- Logging, concurrent, comprehensive stress tests

**TOTAL**: 93 files, ~25,490 LOC (benchmarks/stress/chaos/profiling only)

---

## Summary Table: What's Worth Keeping vs. Rebuilding

| Category | Keep | Remove | Consolidate | Rebuild |
|----------|------|--------|-------------|---------|
| **Core 100K Benchmarks** | benchmark_100k_SUITE | benchmark_100k.erl | Merge standalone | - |
| **Throughput Tests** | throughput_SUITE | - | - | - |
| **Latency Tests** | latency_SUITE | - | - | - |
| **Registry Tests** | registry_100k_SUITE | registry_100k_stress.erl | Add contention | - |
| **Cluster Tests** | cluster_stress_SUITE | - | - | - |
| **Chaos Tests** | chaos_SUITE | chaos_tests.erl variants | Merge variants | Verify backend |
| **Real Transport** | tcp_real_bench, http_real_bench | - | - | Integrate with framework |
| **Profilers** | CPU, Memory, Latency profilers | - | - | - |
| **Orchestrators** | master_stress_orchestrator | master_stress_test.erl | - | - |
| **Infrastructure** | Validators, Config, Results | Simple benchmarks | - | Baseline framework |

**Bottom Line**: Start from erlmcp_benchmark_100k_SUITE.erl and erlmcp_profiling_suite.erl as foundations. Consolidate 15-20 files into 5 core suites. Remove 10 development artifacts. Rebuild metrology layer for consistency.
