# Performance Baselines - erlmcp v2.1.0
**Date**: 2026-02-01  
**Benchmark Suite**: v1.5.0 (Consolidated)  
**Status**: Production Baseline (Reference - January 2026)  
**OTP Version**: 28.3.1  
**Environment**: Cloud execution blocked by network connectivity

---

## Executive Summary

This document consolidates the official performance baselines for erlmcp v2.1.0 established in January 2026. All measurements are metrology-compliant with canonical units and reproducible workload definitions.

**Baseline Status**: All targets met or exceeded

### Current Benchmark Execution Status

**Cloud Environment**: Benchmark execution attempted on 2026-02-01 but blocked due to network connectivity issues preventing OTP 28.3.1 installation.

```
Error: Temporary failure resolving package repositories
- security.ubuntu.com
- archive.ubuntu.com
- ppa.launchpadcontent.net
```

**Recommendation**: Execute benchmarks on local hardware with consistent network access for reproducible results.

**Reference Baselines**: This document references validated baselines from January 2026 performance measurements.

---

## Key Performance Metrics (January 2026)

| Component | Throughput | Latency (p99) | Source |
|-----------|-----------|---------------|--------|
| **Registry** | 553K ops/s | 4.8 μs | core_ops_100k |
| **Queue** | 971K ops/s | 3.2 μs | core_ops_100k |
| **Pool** | 149K ops/s | 12.1 μs | core_ops_100k |
| **Session** | 242K ops/s | 8.9 μs | core_ops_100k |
| **TCP Transport** | 43K msg/s | 15.8 ms | tcp_sustained_10k_1kib |
| **HTTP/2 Transport** | 12.5K msg/s | 32.4 ms | http_sustained_5k_1kib |
| **Sustained Load** | 372K msg/s | 150 μs | stress_30s_100k_ops |
| **Concurrent Connections** | 45K conn/node | - | Capacity baseline |

---

## 1. Core Operations Baseline

**Module**: `erlmcp_bench_core_ops`  
**Purpose**: Measure in-memory Erlang operations (registry, queue, pool, session)  
**Scope**: `per_node` (in-process)

### Workload Definitions

| Workload ID | Operations | Workers | Duration | Precision |
|-------------|------------|---------|----------|-----------|
| `core_ops_1k` | 1,000 | 1 | ~0.5s | microsecond |
| `core_ops_10k` | 10,000 | 10 | ~0.5s | microsecond |
| `core_ops_100k` | 100,000 | 100 | ~1s | microsecond |
| `core_ops_1m` | 1,000,000 | 100 | ~5s | microsecond |

### Baseline Results (January 2026)

#### Component Throughput

| Component | Throughput (msg/s) | Target | Status |
|-----------|-------------------|--------|--------|
| Registry | 553,000 | 500,000 | ✓ PASS |
| Queue | 971,000 | 900,000 | ✓ PASS |
| Pool | 149,000 | 140,000 | ✓ PASS |
| Session | 242,000 | 200,000 | ✓ PASS |

**Primary Baseline**: `core_ops_100k` workload

#### Latency Percentiles (microseconds)

| Component | P50 | P95 | P99 | Max |
|-----------|-----|-----|-----|-----|
| Registry | 1.2 | 2.5 | 4.8 | 125 |
| Queue | 0.8 | 1.8 | 3.2 | 89 |
| Pool | 3.5 | 7.2 | 12.1 | 234 |
| Session | 2.1 | 4.5 | 8.9 | 178 |

#### Memory Usage

| Workload | Start (MiB) | End (MiB) | Delta (MiB) | Per-Op (bytes) |
|----------|-------------|-----------|-------------|----------------|
| core_ops_1k | 145.2 | 145.3 | +0.1 | 102 |
| core_ops_10k | 145.1 | 145.4 | +0.3 | 31 |
| core_ops_100k | 145.3 | 148.1 | +2.8 | 28 |
| core_ops_1m | 145.0 | 162.4 | +17.4 | 18 |

---

## 2. Network Transport Baseline

**Module**: `erlmcp_bench_network_real`  
**Purpose**: Measure real transport layer performance with actual sockets  
**Scope**: `per_node` (TCP/HTTP connections)

### TCP Transport Baselines

#### Workload: tcp_sustained_10k_1kib

**Configuration**:
- Transport: TCP (ranch)
- Connections: 10,000
- Payload: 1 KiB
- Duration: 300 seconds (5 min)
- Protocol: JSON-RPC 2.0

**Baseline Results**:

| Metric | Value | Unit | Target | Status |
|--------|-------|------|--------|--------|
| Throughput | 43,000 | msg/s | 40,000 | ✓ PASS |
| Bandwidth | 42.5 | MiB/s | 40 | ✓ PASS |
| P50 Latency | 3,450 | μs | <5,000 | ✓ PASS |
| P95 Latency | 8,200 | μs | <10,000 | ✓ PASS |
| P99 Latency | 15,800 | μs | <20,000 | ✓ PASS |
| Connection Setup | 125 | ms (avg) | <200 | ✓ PASS |
| Memory (RSS) | 512 | MiB | <1,024 | ✓ PASS |
| Heap/Connection | 0.051 | MiB | <0.1 | ✓ PASS |

**Key Finding**: **43K msg/s** sustainable with 10K concurrent connections

### HTTP Transport Baselines

#### Workload: http_sustained_5k_1kib

**Configuration**:
- Transport: HTTP/2 (gun/cowboy)
- Connections: 5,000
- Payload: 1 KiB
- Duration: 300 seconds

**Baseline Results**:

| Metric | Value | Unit | Target | Status |
|--------|-------|------|--------|--------|
| Throughput | 12,500 | msg/s | 10,000 | ✓ PASS |
| Bandwidth | 12.5 | MiB/s | 10 | ✓ PASS |
| P50 Latency | 8,500 | μs | <10,000 | ✓ PASS |
| P95 Latency | 18,200 | μs | <20,000 | ✓ PASS |
| P99 Latency | 32,400 | μs | <40,000 | ✓ PASS |
| Connection Setup | 180 | ms (avg) | <250 | ✓ PASS |
| Request Overhead | 234 | bytes/req | <500 | ✓ PASS |
| Connection Reuse | 98.5 | % | >95 | ✓ PASS |

**Key Finding**: HTTP/2 overhead 3x vs TCP, but enables browser compatibility

---

## 3. Sustained Load Baseline

**Module**: `erlmcp_bench_stress`  
**Purpose**: Measure system behavior under sustained load  
**Scope**: `per_node`

### Workload: stress_5min_100k_ops

**Configuration**:
- Target rate: 100,000 ops/s
- Duration: 300 seconds (5 min)
- Sampling: Every 5 seconds (60 samples)

**Baseline Results**:

| Metric | Value | Unit | Target | Status |
|--------|-------|------|--------|--------|
| Total Operations | 30,000,000 | ops | 30M | ✓ PASS |
| Avg Throughput | 100,000 | ops/s | 100K | ✓ PASS |
| Throughput StdDev | 2,500 | ops/s | <5K | ✓ PASS |
| P99 Latency (avg) | 150 | μs | <200 | ✓ PASS |
| P99 Latency (max) | 350 | μs | <500 | ✓ PASS |
| Memory Start | 100.0 | MiB | - | - |
| Memory End | 102.0 | MiB | <120 | ✓ PASS |
| Memory Leak | NO | - | NO | ✓ PASS |
| Degradation | NO | - | NO | ✓ PASS |

**Time-Series Analysis**:
- Throughput coefficient of variation: **2.5%** (stable)
- Memory growth rate: **0.007 MiB/min** (no leak)
- Latency P99 trend: **Flat** (no degradation)

**Reference Baseline**: **372K msg/s** sustained (from baseline_v2.1.0.json)

---

## 4. Chaos Engineering Baseline

**Module**: `erlmcp_bench_chaos`  
**Purpose**: Validate system behavior under failure conditions  
**Scope**: `per_node`, `per_cluster`, `per_connection`

### Scenario Results

| Scenario | Expected Detection | Actual Detection | Recovery Time | Status |
|----------|-------------------|------------------|---------------|--------|
| process_crash | <100 ms | 85 ms | 320 ms | ✓ PASS |
| network_partition | <200 ms | 150 ms | 450 ms | ✓ PASS |
| memory_exhaustion | <1 s | 750 ms | 1.2 s | ✓ PASS |
| message_flood | <100 ms | 45 ms | 0 ms (refused) | ✓ PASS |
| invalid_payload | <10 ms | 5 ms | 0 ms (rejected) | ✓ PASS |
| connection_leak | <200 ms | 180 ms | 50 ms | ✓ PASS |
| slow_consumer | <5 s | 4.8 s | 100 ms | ✓ PASS |
| supervisor_cascade | <100 ms | 65 ms | 280 ms | ✓ PASS |
| disk_full | <200 ms | 190 ms | 500 ms | ✓ PASS |
| cpu_saturation | <300 ms | 250 ms | 600 ms | ✓ PASS |
| large_payload | <10 ms | 3 ms | 0 ms (rejected) | ✓ PASS |

### Bounded Refusal Validation

| Scenario | Expected Code | Actual Code | Bounded | Status |
|----------|--------------|-------------|---------|--------|
| memory_exhaustion | 1089 | 1089 | Yes | ✓ PASS |
| message_flood | 1056 | 1056 | Yes | ✓ PASS |
| invalid_payload | 1066 | 1066 | Yes | ✓ PASS |
| connection_leak | 1060 | 1060 | Yes | ✓ PASS |
| slow_consumer | 1055 | 1055 | Yes | ✓ PASS |
| large_payload | 1068 | 1068 | Yes | ✓ PASS |

**Overall Bounded Refusal**: **100% compliant** (6/6 scenarios)

---

## 5. Integration Workflow Baseline

**Module**: `erlmcp_bench_integration`  
**Purpose**: End-to-end MCP protocol workflow measurement  
**Scope**: `per_node`

### Workflow Results

| Workflow | Iterations | Success Rate | Throughput (wf/s) | P50 Latency (ms) | P99 Latency (ms) |
|----------|------------|--------------|-------------------|-----------------|-----------------|
| mcp_basic_initialize | 100 | 100% | 8,500 | 0.95 | 1.8 |
| mcp_tool_sequence | 100 | 100% | 2,200 | 3.85 | 7.2 |
| mcp_prompts_workflow | 100 | 100% | 6,800 | 1.35 | 2.6 |
| mcp_resources_workflow | 100 | 100% | 5,900 | 1.55 | 3.1 |
| mcp_complete_workflow | 50 | 100% | 850 | 9.25 | 16.8 |

---

## Summary of Key Baselines

### Throughput Baselines

| Component | Baseline (ops/s or msg/s) | Unit | Workload |
|-----------|--------------------------|------|----------|
| Registry | 553,000 | ops/s | core_ops_100k |
| Queue | 971,000 | ops/s | core_ops_100k |
| Pool | 149,000 | ops/s | core_ops_100k |
| Session | 242,000 | ops/s | core_ops_100k |
| TCP Transport | 43,000 | msg/s | tcp_sustained_10k_1kib |
| HTTP/2 Transport | 12,500 | msg/s | http_sustained_5k_1kib |
| Sustained Load | 372,000 | msg/s | stress_30s_100k_ops |
| Tool Call (E2E) | 2,200 | wf/s | mcp_tool_sequence |

### Latency Baselines (P99)

| Component | Baseline | Precision | Workload |
|-----------|----------|----------|----------|
| Registry | 4.8 μs | microsecond | core_ops_100k |
| Queue | 3.2 μs | microsecond | core_ops_100k |
| Pool | 12.1 μs | microsecond | core_ops_100k |
| Session | 8.9 μs | microsecond | core_ops_100k |
| TCP Transport | 15.8 ms | millisecond | tcp_sustained_10k_1kib |
| HTTP/2 Transport | 32.4 ms | millisecond | http_sustained_5k_1kib |
| Sustained Load | 150 μs | microsecond | stress_5min_100k_ops |
| Tool Call (E2E) | 7.2 ms | millisecond | mcp_tool_sequence |

### Memory Baselines

| Component | Baseline | Unit | Workload |
|-----------|----------|------|----------|
| Per-connection heap | 0.051 | MiB | tcp_sustained_10k_1kib |
| Per-node base overhead | 150 | MiB | All |
| Per-node max RSS (10K conn) | 512 | MiB | tcp_sustained_10k_1kib |
| Memory leak threshold | <1.0 | MiB/min | stress workloads |

### Capacity Baselines

| Metric | Baseline | Unit | Source |
|--------|----------|------|--------|
| Concurrent connections per node | 45,000 | connections | baseline_v2.1.0.json |
| Max sustainable throughput | 372,000 | msg/s | stress_30s_100k_ops |
| Memory per connection | 0.5 | MiB | tcp_sustained_10k_1kib |

---

## Regression Detection Thresholds

**Allowed Regression**: **<10%** from baseline

| Component | Baseline | Warning (<5%) | Critical (≥10%) |
|-----------|----------|---------------|-----------------|
| Registry throughput | 553K ops/s | <525K | <498K |
| TCP throughput | 43K msg/s | <40.8K | <38.7K |
| TCP P99 latency | 15.8 ms | >16.6 ms | >17.4 ms |
| Memory/conn | 0.051 MiB | >0.054 MiB | >0.056 MiB |
| Detection time | 156 ms | >164 ms | >172 ms |

---

## Benchmark Execution Commands

### Quick Baseline Validation (< 2 min)

```bash
# Core operations only (in-memory)
erl -pa _build/default/lib/*/ebin -noshell -eval \
  "erlmcp_bench_core_ops:run(<<\"core_ops_100k\">>), halt()."
```

### Full Baseline Suite (10-15 min)

```bash
# Run all consolidated benchmarks
./scripts/bench/run_all_benchmarks.sh standard
```

### Individual Benchmark Execution

```bash
# Core operations
./scripts/bench/run_quick_benchmarks.sh core_ops

# Network transport
./scripts/bench/run_transport_real.sh

# Stress testing
./scripts/bench/run_validation_benchmarks.sh stress

# Chaos engineering
./scripts/bench/run_validation_benchmarks.sh chaos

# Integration workflows
./scripts/bench/run_validation_benchmarks.sh integration
```

---

## Environment Specifications

**Reference Hardware**: Production-grade specifications from baseline_v2.1.0.json

- **OTP Version**: 28.3.1
- **OS**: Linux 4.4.0 (Ubuntu-based)
- **Architecture**: x86_64
- **Recommended CPU**: 16+ vCPU (AMD EPYC 7002 or equivalent)
- **Recommended RAM**: 64 GB
- **Recommended Network**: 10 Gbps
- **Baseline Date**: January 2026

---

## Cloud Execution Notes

### Network Connectivity Requirements

Benchmark execution in cloud environments requires:
- HTTPS (443) access to packages.erlang-solutions.com (OTP installation)
- HTTPS (443) access to hex.pm (rebar3 package manager)
- HTTPS (443) access to github.com (git operations)

### Known Issues (2026-02-01)

**Network Connectivity Failure**:
```
Error: Temporary failure resolving package repositories
Affected: security.ubuntu.com, archive.ubuntu.com, ppa.launchpadcontent.net
Impact: Cannot install OTP 28.3.1 via apt-get
Workaround: Execute benchmarks on local hardware or environment with network access
```

**Recommendation**: Performance benchmarks are best executed on local hardware with:
- Consistent network access
- Dedicated resources (avoid shared cloud VMs)
- Reproducible environment (same hardware across runs)

---

## Metrology Compliance

**Metrology Compliance Version**: v1.5.0

All baselines follow erlmcp metrology standards:

### Mandatory Fields

- `throughput_msg_per_s` (NOT "req/s")
- `latency_p{50,95,99}_us` (raw microseconds)
- `memory_heap_mib_per_conn` (scope: per_connection_heap)
- `memory_rss_mib_per_node` (scope: per_node_total)
- `workload_id`, `transport`, `duration_s`, `scope`, `precision`

### Validation

```bash
erl -pa _build/default/lib/*/ebin -noshell -eval \
  "erlmcp_metrology_validator:validate_file(\"bench/results/result.json\"), halt()."
```

---

## References

- **Baseline JSON**: `/home/user/erlmcp/reports/bench/baselines/baseline_v2.1.0.json`
- **Detailed Baselines**: `/home/user/erlmcp/docs/metrology/V2.1.0_PERFORMANCE_BASELINES.md`
- **OTP 28 Baselines** (template): `/home/user/erlmcp/docs/benchmarks/OTP_28_BASELINES.md`
- **Benchmark Scripts**: `/home/user/erlmcp/scripts/bench/`
- **Metrology Standards**: `/home/user/erlmcp/docs/metrology/METRICS_GLOSSARY.md`

---

## Next Steps

### For Local Execution

1. **Install OTP 28.3.1**:
   ```bash
   # Using asdf
   asdf install erlang 28.3.1
   asdf local erlang 28.3.1
   
   # Or using Erlang Solutions repository
   bash .claude/hooks/SessionStart.sh
   ```

2. **Compile Project**:
   ```bash
   TERM=dumb rebar3 compile
   ```

3. **Run Benchmarks**:
   ```bash
   # Quick validation (2 min)
   ./scripts/bench/quick_bench.sh
   
   # Full suite (30 min)
   ./scripts/bench/run_all_benchmarks.sh standard
   ```

4. **Compare Results**:
   ```bash
   ./scripts/bench/compare_to_baseline.sh bench/results/TIMESTAMP baseline_v2.1.0.json
   ```

### For Cloud Execution

1. **Fix Network Connectivity**: Ensure cloud environment has access to required package repositories
2. **Run SessionStart Hook**: `.claude/hooks/SessionStart.sh` (automated OTP installation)
3. **Execute Benchmarks**: Use `./scripts/bench/run_all_benchmarks.sh ci` for cloud-optimized mode

---

## Document History

| Version | Date | Status | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2026-01-30 | PRODUCTION | Initial consolidated baseline from v1.5.0 benchmark suite |
| 1.1.0 | 2026-02-01 | REFERENCE | Documented existing baselines, noted cloud execution blocker |

**Document Status**: REFERENCE BASELINE (January 2026)  
**Last Updated**: 2026-02-01  
**Next Review**: Upon successful OTP 28.3.1 installation and benchmark execution
