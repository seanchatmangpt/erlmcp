# erlmcp v3.0 Performance Plan

**Version**: 3.0.0  
**Status**: Draft  
**Last Updated**: 2026-01-31  
**Owner**: Performance Team  

---

## Executive Summary

This plan establishes performance baselines, regression detection thresholds, and benchmark automation for erlmcp v3.0. Performance targets are derived from v2.0.0 measurements plus improvements from OTP 28.3.1+ native features.

### Key Performance Improvements in v3.0

| Feature | v2.0.0 | v3.0.0 Target | Improvement |
|---------|--------|---------------|-------------|
| JSON Encoding (via native `json`) | 2.83M msg/s | 7.0M msg/s | 2.5x faster |
| JSON Decoding | 1.8M msg/s | 4.5M msg/s | 2.5x faster |
| Process Monitoring | O(N) | O(1) | Constant time |
| Health Check Latency | 5-10ms | <1ms | 10x faster |
| Memory Overhead per Process | ~2KB | ~1.5KB | 25% reduction |

### Baseline Philosophy

**Baseline**: Performance reference point for regression detection  
**Threshold**: Maximum allowed degradation before blocking commit  
**Target**: Aspirational performance goal for optimization

---

## 1. Performance Baseline Targets

### 1.1 Core Operations (In-Memory)

**Workload**: `core_ops_100k` - 100,000 operations across registry, queue, pool, session

| Component | Metric | v2.0.0 Baseline | v3.0 Target | Threshold |
|-----------|--------|----------------|-------------|-----------|
| **Registry** | Throughput | 553K ops/s | 600K ops/s | 10% regression |
| | p50 Latency | 1.8 us | 1.5 us | +20% |
| | p95 Latency | 8.2 us | 7.0 us | +20% |
| | p99 Latency | 15.4 us | 13.0 us | +20% |
| **Queue** | Throughput | 971K ops/s | 1.1M ops/s | 10% regression |
| | p50 Latency | 1.0 us | 0.9 us | +20% |
| | p95 Latency | 4.1 us | 3.5 us | +20% |
| | p99 Latency | 9.8 us | 8.5 us | +20% |
| **Pool** | Throughput | 149K ops/s | 180K ops/s | 10% regression |
| | p50 Latency | 6.5 us | 5.5 us | +20% |
| | p95 Latency | 24.3 us | 20.0 us | +20% |
| | p99 Latency | 58.7 us | 48.0 us | +20% |
| **Session** | Throughput | 242K ops/s | 300K ops/s | 10% regression |
| | p50 Latency | 4.1 us | 3.5 us | +20% |
| | p95 Latency | 12.8 us | 11.0 us | +20% |
| | p99 Latency | 28.4 us | 24.0 us | +20% |

### 1.2 JSON-RPC Encoding/Decoding

**Workload**: `json_rpc_encoding` - Small/medium/large messages

| Message Size | Metric | v2.0.0 (jsx) | v3.0 Target (native) | Threshold |
|--------------|--------|--------------|---------------------|-----------|
| **Small** (10 fields) | Encode Throughput | 450K msg/s | 1.2M msg/s | 15% regression |
| | Decode Throughput | 380K msg/s | 950K msg/s | 15% regression |
| | p50 Latency | 2.2 us | 0.8 us | +30% |
| **Medium** (50 fields) | Encode Throughput | 85K msg/s | 220K msg/s | 15% regression |
| | Decode Throughput | 72K msg/s | 180K msg/s | 15% regression |
| | p50 Latency | 11.8 us | 4.5 us | +30% |
| **Large** (500 fields) | Encode Throughput | 8.5K msg/s | 22K msg/s | 15% regression |
| | Decode Throughput | 7.2K msg/s | 18K msg/s | 15% regression |
| | p50 Latency | 118 us | 45 us | +30% |

### 1.3 Transport Layer

**Workload**: `transport_throughput` - Sustained message processing

| Transport | Metric | v2.0.0 Baseline | v3.0 Target | Threshold |
|-----------|--------|----------------|-------------|-----------|
| **TCP** | Throughput | 43K msg/s | 55K msg/s | 15% regression |
| | p50 Latency | 5.2 ms | 4.5 ms | +25% |
| | p95 Latency | 12.8 ms | 11.0 ms | +25% |
| | p99 Latency | 28.4 ms | 24.0 ms | +25% |
| | Connections per Node | 40K | 50K | 10% regression |
| **HTTP SSE** | Throughput | 38K msg/s | 48K msg/s | 15% regression |
| | p50 Latency | 6.1 ms | 5.5 ms | +25% |
| | p95 Latency | 15.2 ms | 13.5 ms | +25% |
| | p99 Latency | 32.7 ms | 28.0 ms | +25% |
| | Connections per Node | 35K | 45K | 10% regression |
| **WebSocket** | Throughput | 41K msg/s | 52K msg/s | 15% regression |
| | p50 Latency | 5.5 ms | 4.8 ms | +25% |
| | p95 Latency | 13.5 ms | 11.8 ms | +25% |
| | p99 Latency | 29.8 ms | 25.5 ms | +25% |
| | Connections per Node | 38K | 48K | 10% regression |

### 1.4 Priority Messages

**Workload**: `priority_messages` - Health checks, critical alerts

| Priority Level | Metric | v3.0 Target | Threshold |
|----------------|--------|-------------|-----------|
| **Critical** | p50 Latency | <0.5 ms | +100% (1ms max) |
| | p95 Latency | <1.0 ms | +100% (2ms max) |
| | p99 Latency | <2.0 ms | +100% (4ms max) |
| **High** | p50 Latency | <1.0 ms | +100% (2ms max) |
| | p95 Latency | <2.5 ms | +100% (5ms max) |
| | p99 Latency | <5.0 ms | +100% (10ms max) |
| **Normal** | p50 Latency | <5.0 ms | +100% (10ms max) |
| | p95 Latency | <12.0 ms | +100% (24ms max) |
| | p99 Latency | <25.0 ms | +100% (50ms max) |

### 1.5 SSE Resumption

**Workload**: `sse_resumption` - Event replay after reconnection

| Metric | v3.0 Target | Threshold |
|--------|-------------|-----------|
| Replay Latency (100 events) | <50 ms | +100% (100ms max) |
| Replay Latency (1000 events) | <200 ms | +100% (400ms max) |
| Header Parsing Latency | <1 ms | +100% (2ms max) |
| Event Store Lookup | <5 ms | +100% (10ms max) |
| Memory Overhead per Connection | +500 KB | +50% (750KB max) |

### 1.6 Resource Operations

**Workload**: `resources_http` - HTTP resource fetching with caching

| Operation | Metric | v3.0 Target | Threshold |
|-----------|--------|-------------|-----------|
| **Cached Fetch** | p50 Latency | <5 ms | +100% (10ms max) |
| | p95 Latency | <15 ms | +100% (30ms max) |
| | Throughput | 20K req/s | 15% regression |
| **Uncached Fetch** | p50 Latency | <50 ms | +50% (75ms max) |
| | p95 Latency | <150 ms | +50% (225ms max) |
| | Throughput | 5K req/s | 15% regression |
| **Streaming (10MB)** | Throughput | 100 MB/s | 20% regression |
| | Latency | <100 ms | +50% (150ms max) |

### 1.7 Memory Metrics

**Workload**: `memory_scalability` - Memory usage under load

| Metric | v2.0.0 Baseline | v3.0 Target | Threshold |
|--------|----------------|-------------|-----------|
| **Per Connection** | | | |
| | Heap Memory | 48 KB | 36 KB | +25% |
| | State Memory | 12 KB | 10 KB | +25% |
| | Total per Connection | 60 KB | 46 KB | +25% |
| **Per Node** | | | |
| | Base Overhead (empty) | 150 MiB | 140 MiB | +20% |
| | Total RSS (10K conns) | 750 MiB | 600 MiB | +20% |
| | Total RSS (50K conns) | 3150 MiB | 2500 MiB | +20% |
| **Growth Rate** | | | |
| | Memory Growth Rate | Linear | Linear | No superlinear growth |

### 1.8 Failover Performance

**Workload**: `ha_failover` - Cluster failover scenarios

| Metric | v3.0 Target | Threshold |
|--------|-------------|-----------|
| **Node Failure Detection** | | | |
| | Detection Time | <1.0 s | +100% (2s max) |
| | Lost Messages | <10 | +100% (20 max) |
| **Failover Completion** | | | |
| | Total Failover Time | <2.0 s | +100% (4s max) |
| | Connection Migration | 100% | 95% min |
| | State Recovery | 100% | 95% min |
| **Performance During Failover** | | | |
| | Throughput Degradation | <20% | +50% (30% max) |
| | Latency Increase | <2x | +50% (3x max) |

---

## 2. Regression Detection Thresholds

### 2.1 Severity Levels

| Severity | Performance Drop | Action | Block Merge? |
|----------|------------------|--------|--------------|
| **Critical** | >20% | Immediate investigation | YES |
| **High** | 15-20% | Investigate before merge | YES |
| **Medium** | 10-15% | Warning, document reason | MAYBE |
| **Low** | 5-10% | Log only | NO |
| **Negligible** | <5% | Ignore | NO |

### 2.2 Metric-Specific Thresholds

#### Throughput Regression
- **Core Operations**: 10% threshold (block if >10% degradation)
- **JSON Encoding**: 15% threshold (block if >15% degradation)
- **Transport Throughput**: 15% threshold (block if >15% degradation)

#### Latency Regression
- **p50 Latency**: 20% threshold (block if >20% increase)
- **p95 Latency**: 20% threshold (block if >20% increase)
- **p99 Latency**: 20% threshold (block if >20% increase)
- **Priority Messages**: 100% threshold (block if 2x degradation)

#### Memory Regression
- **Per Connection**: 25% threshold (block if >25% increase)
- **Per Node**: 20% threshold (block if >20% increase)
- **Growth Rate**: Block if superlinear detected

#### Resource-Specific
- **SSE Resumption**: 100% threshold (block if 2x degradation)
- **Cached Fetch**: 100% threshold (block if 2x degradation)
- **Failover Time**: 100% threshold (block if 2x degradation)

### 2.3 Regression Detection Algorithm

```bash
#!/usr/bin/env bash
# Regression detection logic for CI/CD

detect_regression() {
    local baseline=$1
    local current=$2
    local threshold=$3
    
    # Calculate percentage change
    if (( $(echo "$baseline > 0" | bc -l) )); then
        change=$(echo "scale=2; (($current - $baseline) / $baseline) * 100" | bc)
        
        # For throughput: negative change is regression
        # For latency/memory: positive change is regression
        if [[ $(echo "$change < -$threshold" | bc -l) -eq 1 ]]; then
            return 1  # Regression detected
        fi
    fi
    
    return 0  # No regression
}

# Example usage in CI
if ! detect_regression 2830000 2500000 10; then
    echo "REGRESSION: Throughput dropped 11.6%"
    exit 1
fi
```

### 2.4 Baseline Update Policy

**Automatic Updates (CI/CD on main branch)**:
- Allowed if: ALL metrics within threshold AND no regressions
- Frequency: Once per day maximum
- Commit message: `chore: update performance baseline [skip ci]`

**Manual Updates (explicit justification)**:
- Required if: ANY metric exceeds threshold but change is intentional
- Process:
  1. Open PR with baseline update
  2. Include justification in PR description
  3. Code review by performance team
  4. Update threshold if new baseline is justified long-term

**Rejected Updates**:
- Regressions without justification
- Intentional performance degradation without compensation
- Updates that reduce targets

---

## 3. Benchmark Automation

### 3.1 Benchmark Suite Structure

```
bench/
├── baselines/
│   ├── 2026-01-28_v2.0.0.json       # v2.0.0 baseline
│   ├── 2026-01-31_v3.0.0.json       # v3.0.0 baseline (to be created)
│   └── LATEST.json                  # Symlink to latest baseline
├── workloads/
│   ├── core_ops_100k.json
│   ├── json_rpc_encoding.json
│   ├── transport_throughput.json
│   ├── priority_messages.json
│   ├── sse_resumption.json
│   └── resources_http.json
├── results/
│   ├── 2026-01-31_143022_core_ops/
│   └── 2026-01-31_150415_transport/
└── scripts/
    ├── run_all_benchmarks.sh        # Master script
    ├── compare_to_baseline.sh       # Regression detection
    └── generate_report.sh           # HTML report generation
```

### 3.2 Benchmark Runners

#### Existing Runners (Enhancement Required)

1. **erlmcp_bench_core_ops** - Core operations (registry, queue, pool, session)
   - Add v3.0 target columns to output
   - Add native JSON benchmarking
   - Add priority message benchmarking

2. **erlmcp_bench_network_real** - Real network sockets
   - Add WebSocket transport
   - Add SSE resumption benchmark
   - Add HTTP resource fetching benchmark

3. **erlmcp_bench_stress** - Sustained load testing
   - Add 24-hour stability test
   - Add memory leak detection
   - Add priority message priority inversion detection

4. **erlmcp_bench_chaos** - Failure injection
   - Add SSE resumption under chaos
   - Add failover performance validation
   - Add priority message delivery during chaos

5. **erlmcp_bench_integration** - End-to-end MCP workflows
   - Add SSE reconnect + resumption workflow
   - Add priority message workflow
   - Add HTTP resource fetching workflow

#### New Runners Required

6. **erlmcp_bench_json_native** - Native JSON performance
   - Benchmark `json:encode/1` vs `jsx:encode/1`
   - Small/medium/large message sizes
   - Encode vs decode performance

7. **erlmcp_bench_priority_messages** - Priority message latency
   - Critical/high/normal priority levels
   - Measure priority inversion
   - Measure queue bypass effectiveness

8. **erlmcp_bench_sse_resumption** - SSE resumption performance
   - Event replay latency
   - Header parsing overhead
   - Memory overhead per resumable connection

9. **erlmcp_bench_resources** - Resource operations
   - Cached vs uncached fetch latency
   - Streaming throughput
   - Cache hit rate optimization

### 3.3 Master Benchmark Script

**File**: `bench/scripts/run_all_benchmarks.sh`

```bash
#!/usr/bin/env bash
# Master benchmark runner for erlmcp v3.0
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
RESULTS_DIR="${PROJECT_ROOT}/bench/results/$(date +%Y%m%d_%H%M%S)"
TIMESTAMP=$(date +%s)

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $*"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }

# Create results directory
mkdir -p "${RESULTS_DIR}"

# Ensure compiled
log_info "Compiling project..."
TERM=dumb rebar3 compile
TERM=dumb rebar3 as test compile

# Benchmark categories
declare -a CATEGORIES=(
    "core_ops"
    "json_native"
    "transport"
    "priority_messages"
    "sse_resumption"
    "resources"
    "stress"
    "chaos"
    "integration"
)

# Run benchmarks for each category
run_benchmark() {
    local category=$1
    log_info "Running ${category} benchmarks..."
    
    case "$category" in
        core_ops)
            erl -noshell -pa _build/default/lib/*/ebin \
                -eval "erlmcp_bench_core_ops:run_all(), halt()."
            ;;
        json_native)
            erl -noshell -pa _build/default/lib/*/ebin \
                -eval "erlmcp_bench_json_native:run_all(), halt()."
            ;;
        transport)
            erl -noshell -pa _build/default/lib/*/ebin \
                -eval "erlmcp_bench_network_real:run_all(), halt()."
            ;;
        priority_messages)
            erl -noshell -pa _build/default/lib/*/ebin \
                -eval "erlmcp_bench_priority_messages:run_all(), halt()."
            ;;
        sse_resumption)
            erl -noshell -pa _build/default/lib/*/ebin \
                -eval "erlmcp_bench_sse_resumption:run_all(), halt()."
            ;;
        resources)
            erl -noshell -pa _build/default/lib/*/ebin \
                -eval "erlmcp_bench_resources:run_all(), halt()."
            ;;
        stress)
            log_info "Stress tests take 30+ minutes, skipping in quick run..."
            return 0
            ;;
        chaos)
            log_info "Chaos tests take 20+ minutes, skipping in quick run..."
            return 0
            ;;
        integration)
            erl -noshell -pa _build/default/lib/*/ebin \
                -eval "erlmcp_bench_integration:run_all(), halt()."
            ;;
    esac
    
    # Move results to timestamped directory
    mv bench/results/*.json "${RESULTS_DIR}/" 2>/dev/null || true
    
    log_success "${category} benchmarks complete"
}

# Main execution
main() {
    log_info "=========================================="
    log_info "erlmcp v3.0 Benchmark Suite"
    log_info "=========================================="
    log_info "Results directory: ${RESULTS_DIR}"
    echo ""
    
    # Run all benchmarks
    for category in "${CATEGORIES[@]}"; do
        run_benchmark "$category"
    done
    
    # Compare to baseline
    log_info "Comparing to baseline..."
    chmod +x "${SCRIPT_DIR}/compare_to_baseline.sh"
    "${SCRIPT_DIR}/compare_to_baseline.sh" "${RESULTS_DIR}"
    
    # Generate report
    log_info "Generating performance report..."
    chmod +x "${SCRIPT_DIR}/generate_report.sh"
    "${SCRIPT_DIR}/generate_report.sh" "${RESULTS_DIR}"
    
    echo ""
    log_success "=========================================="
    log_success "All benchmarks complete!"
    log_success "Results: ${RESULTS_DIR}"
    log_success "=========================================="
}

main "$@"
```

### 3.4 Baseline Comparison Script

**File**: `bench/scripts/compare_to_baseline.sh`

```bash
#!/usr/bin/env bash
# Compare benchmark results to baseline with regression detection
set -euo pipefail

RESULTS_DIR=${1:-latest}
BASELINE_FILE="${PROJECT_ROOT}/bench/baselines/LATEST.json"
THRESHOLD=${PERFORMANCE_THRESHOLD:-10}

log_info "Comparing results in ${RESULTS_DIR} to baseline..."

# Load baseline
if [[ ! -f "${BASELINE_FILE}" ]]; then
    log_warn "No baseline found, skipping comparison"
    exit 0
fi

# Compare each result file
for result_file in "${RESULTS_DIR}"/*.json; do
    workload=$(jq -r '.workload_id' "$result_file")
    
    # Extract baseline metrics for this workload
    baseline_throughput=$(jq -r ".workloads[\"$workload\"].throughput_msg_per_s // empty" "${BASELINE_FILE}")
    baseline_p50=$(jq -r ".workloads[\"$workload\"].latency_p50_us // empty" "${BASELINE_FILE}")
    baseline_p95=$(jq -r ".workloads[\"$workload\"].latency_p95_us // empty" "${BASELINE_FILE}")
    
    # Extract current metrics
    current_throughput=$(jq -r '.throughput_msg_per_s' "$result_file")
    current_p50=$(jq -r '.latency_p50_us' "$result_file")
    current_p95=$(jq -r '.latency_p95_us' "$result_file")
    
    # Calculate regressions
    if [[ -n "$baseline_throughput" ]] && [[ -n "$current_throughput" ]]; then
        throughput_change=$(echo "scale=2; (($current_throughput - $baseline_throughput) / $baseline_throughput) * 100" | bc)
        
        if (( $(echo "$throughput_change < -$THRESHOLD" | bc -l) )); then
            log_error "REGRESSION: ${workload} throughput degraded by ${throughput_change}%"
            echo "REGRESSION: ${workload} throughput: ${baseline_throughput} -> ${current_throughput} (${throughput_change}%)"
        fi
    fi
done
```

---

## 4. CI/CD Integration

### 4.1 Performance Regression Workflow

**File**: `.github/workflows/performance-regression.yml`

Enhancements required for v3.0:

1. **Add Native JSON Benchmarks**
   ```yaml
   - name: Run native JSON benchmarks
     run: |
       erl -noshell -pa _build/default/lib/*/ebin \
         -eval "erlmcp_bench_json_native:run(<<\"json_native_small\">>), halt()."
   ```

2. **Add Priority Message Benchmarks**
   ```yaml
   - name: Run priority message benchmarks
     run: |
       erl -noshell -pa _build/default/lib/*/ebin \
         -eval "erlmcp_bench_priority_messages:run_all(), halt()."
   ```

3. **Add SSE Resumption Benchmarks**
   ```yaml
   - name: Run SSE resumption benchmarks
     run: |
       erl -noshell -pa _build/default/lib/*/ebin \
         -eval "erlmcp_bench_sse_resumption:run_all(), halt()."
   ```

4. **Update Thresholds for v3.0**
   ```yaml
   env:
     CORE_OPS_THRESHOLD: 10
     JSON_ENCODING_THRESHOLD: 15
     TRANSPORT_THRESHOLD: 15
     PRIORITY_MESSAGE_THRESHOLD: 100  # 2x degradation allowed
     SSE_RESUMPTION_THRESHOLD: 100    # 2x degradation allowed
   ```

### 4.2 Pre-commit Performance Check

**File**: `.git/hooks/pre-commit.performance`

```bash
#!/usr/bin/env bash
# Quick performance check before commit (skipped if no perf code changed)

# Check if performance-critical files changed
PERF_FILES=(
    "apps/erlmcp_core/src/erlmcp_json_rpc.erl"
    "apps/erlmcp_core/src/erlmcp_message_handler.erl"
    "apps/erlmcp_transports/src/erlmcp_transport_*.erl"
)

CHANGED=0
for file in "${PERF_FILES[@]}"; do
    if git diff --cached --name-only | grep -q "$file"; then
        CHANGED=1
        break
    fi
fi

if [[ $CHANGED -eq 0 ]]; then
    exit 0  # No performance code changed
fi

# Run quick benchmark (1K operations)
echo "Running quick performance check..."
erl -noshell -pa _build/default/lib/*/ebin \
    -eval "erlmcp_bench_core_ops:run(<<\"core_ops_1k\">>), halt()."

# Compare to baseline (15% threshold for quick check)
./bench/scripts/compare_to_baseline.sh --threshold 15

exit $?
```

### 4.3 Scheduled Performance Monitoring

**File**: `.github/workflows/performance-monitoring.yml`

```yaml
name: Performance Monitoring

on:
  schedule:
    - cron: '0 2 * * *'  # Daily at 2 AM UTC
  workflow_dispatch:

jobs:
  daily-benchmark:
    name: Daily Performance Benchmark
    runs-on: ubuntu-latest
    timeout-minutes: 120
    
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
      
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
          rebar3-version: '3.23'
      
      - name: Run full benchmark suite
        run: |
          chmod +x bench/scripts/run_all_benchmarks.sh
          ./bench/scripts/run_all_benchmarks.sh
      
      - name: Upload results to metrics store
        env:
          METRICS_ENDPOINT: ${{ secrets.METRICS_ENDPOINT }}
        run: |
          # Upload benchmark results to time-series database
          ./scripts/upload_metrics.sh bench/results/latest
      
      - name: Generate trend report
        run: |
          ./scripts/generate_trend_report.sh
          
      - name: Create issue on regression
        if: failure()
        uses: actions/github-script@v7
        with:
          script: |
            github.rest.issues.create({
              owner: context.repo.owner,
              repo: context.repo.repo,
              title: 'Performance Regression Detected',
              body: 'Daily benchmark run failed. See workflow logs for details.',
              labels: ['performance', 'regression', 'automated']
            })
```

---

## 5. Performance Validation Plan

### 5.1 Pre-Release Validation Checklist

**Phase 1: Unit Performance** (30 minutes)
- [ ] Core operations meet v3.0 targets (600K ops/s registry)
- [ ] Native JSON encoding meets targets (1.2M msg/s small)
- [ ] Priority messages meet targets (<0.5ms p50 critical)
- [ ] No regression vs v2.0.0 baseline (>10% degradation)

**Phase 2: Transport Performance** (1 hour)
- [ ] TCP throughput meets target (55K msg/s)
- [ ] SSE throughput meets target (48K msg/s)
- [ ] WebSocket throughput meets target (52K msg/s)
- [ ] Connection capacity meets target (50K concurrent)
- [ ] Latency percentiles meet targets (p50 <5ms TCP)

**Phase 3: Feature Performance** (1 hour)
- [ ] SSE resumption meets targets (<50ms 100 events)
- [ ] Resource caching meets targets (<5ms p50 cached)
- [ ] Resource streaming meets targets (100 MB/s)
- [ ] Failover meets targets (<2s detection + recovery)

**Phase 4: Stress Testing** (4 hours)
- [ ] Sustained 30-minute load at 50K connections
- [ ] Memory growth is linear (no leaks)
- [ ] No degradation over time (<5% drift)
- [ ] Priority messages stable under load

**Phase 5: Chaos Performance** (2 hours)
- [ ] Failover performance validated (<2s recovery)
- [ ] SSE resumption works during chaos
- [ ] Priority messages delivered during chaos
- [ ] Recovery time <5s for all scenarios

### 5.2 Baseline Capture Procedure

**When to Capture Baseline**:
1. Release candidates (RC) for v3.0.0, v3.1.0, etc.
2. Major performance optimizations
3. Infrastructure changes (hardware, Erlang version)
4. Quarterly validation (even if no changes)

**Capture Requirements**:
```bash
#!/usr/bin/env bash
# Baseline capture script

# 1. Clean build
rebar3 clean
TERM=dumb rebar3 compile

# 2. Warm-up (5 iterations)
for i in {1..5}; do
    erl -noshell -pa _build/default/lib/*/ebin \
        -eval "erlmcp_bench_core_ops:run(<<\"core_ops_10k\">>), halt()."
done

# 3. Capture baseline (10 iterations, take median)
for i in {1..10}; do
    erl -noshell -pa _build/default/lib/*/ebin \
        -eval "erlmcp_bench_core_ops:run(<<\"core_ops_100k\">>), halt()."
    sleep 5  # Cool-down
done

# 4. Aggregate results
./bench/scripts/aggregate_baseline.sh

# 5. Validate metrology compliance
./scripts/validate_plan_metrology.sh bench/baselines/NEW_BASELINE.json

# 6. Commit
git add bench/baselines/
git commit -m "chore: update performance baseline to v3.0.0"
```

### 5.3 Performance Issue Response

**Severity Response Times**:

| Severity | Response Time | Resolution Target |
|----------|---------------|-------------------|
| Critical | 1 hour | 24 hours |
| High | 4 hours | 7 days |
| Medium | 1 business day | 2 weeks |
| Low | 1 week | Next release |

**Response Procedure**:
1. **Detection**: CI/CD blocks merge or daily benchmark fails
2. **Investigation**: Performance team confirms regression
3. **Bisect**: Git bisect to find commit
4. **Fix**: Rollback or optimization
5. **Validate**: Rerun benchmarks to confirm fix
6. **Learn**: Document root cause and prevention

---

## 6. Performance Dashboard

### 6.1 Metrics to Display

**Real-Time Metrics** (updated every 5 seconds):
- Current throughput (msg/s)
- Current latency percentiles (p50, p95, p99)
- Active connections
- Memory usage (per connection, per node)

**Historical Trends** (updated daily):
- Throughput over time (30-day chart)
- Latency percentiles over time
- Memory growth rate
- Baseline comparisons

**Regression Alerts**:
- Active regressions (severity, metric, % change)
- Failed benchmark runs (last 7 days)
- Threshold violations (last 24 hours)

### 6.2 Dashboard Implementation

**File**: `apps/erlmcp_observability/src/erlmcp_performance_dashboard.erl`

```erlang
-module(erlmcp_performance_dashboard).

%% API
-export([
    start_link/0,
    get_current_metrics/0,
    get_historical_trends/1,
    check_regression/0
]).

%% gen_server
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    baseline :: map(),
    current_metrics :: map(),
    history :: [map()]
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_current_metrics() ->
    gen_server:call(?MODULE, get_current_metrics).

get_historical_trends(days) ->
    gen_server:call(?MODULE, {get_trends, days}).

check_regression() ->
    gen_server:call(?MODULE, check_regression).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Load baseline
    Baseline = load_baseline(),
    
    %% Schedule metrics collection
    timer:send_interval(5000, collect_metrics),
    timer:send_interval(86400000, prune_history),  % Daily
    
    {ok, #state{
        baseline = Baseline,
        current_metrics = #{},
        history = []
    }}.

handle_call(get_current_metrics, _From, State) ->
    {reply, State#state.current_metrics, State};

handle_call({get_trends, Days}, _From, State) ->
    Cutoff = erlang:system_time(second) - (Days * 86400),
    Trends = lists:filter(
        fun(M) -> maps:get(timestamp, M) >= Cutoff end,
        State#state.history
    ),
    {reply, Trends, State};

handle_call(check_regression, _From, State) ->
    Regressions = detect_regressions(
        State#state.baseline,
        State#state.current_metrics
    ),
    {reply, Regressions, State};

handle_info(collect_metrics, State) ->
    Metrics = collect_metrics(),
    UpdatedState = State#state{
        current_metrics = Metrics,
        history = [Metrics | State#state.history]
    },
    {noreply, UpdatedState};

handle_info(prune_history, State) ->
    %% Keep only last 30 days
    Cutoff = erlang:system_time(second) - (30 * 86400),
    PrunedHistory = lists:filter(
        fun(M) -> maps:get(timestamp, M) >= Cutoff end,
        State#state.history
    ),
    {noreply, State#state{history = PrunedHistory}}.

%%====================================================================
%% Internal functions
%%====================================================================

load_baseline() ->
    %% Load from bench/baselines/LATEST.json
    {ok, Content} = file:read_file("bench/baselines/LATEST.json"),
    jsx:decode(Content, [return_maps]).

collect_metrics() ->
    %% Collect real-time metrics from system
    #{
        timestamp => erlang:system_time(second),
        throughput_msg_per_s => get_throughput(),
        latency_p50_us => get_latency(p50),
        latency_p95_us => get_latency(p95),
        latency_p99_us => get_latency(p99),
        active_connections => erlmcp_registry:count_connections(),
        memory_per_connection_mib => get_memory_per_connection(),
        memory_per_node_mib => erlang:memory(total) / (1024 * 1024)
    }.

detect_regressions(Baseline, Current) ->
    %% Compare current to baseline with thresholds
    lists:filtermap(
        fun({Metric, Threshold}) ->
            BaselineVal = maps:get(Metric, Baseline, undefined),
            CurrentVal = maps:get(Metric, Current, undefined),
            
            case calculate_regression(BaselineVal, CurrentVal, Threshold) of
                {regression, PctChange} ->
                    {true, {Metric, PctChange}};
                ok ->
                    false
            end
        end,
        [
            {{throughput_msg_per_s}, 10},  % 10% threshold
            {{latency_p50_us}, 20},        % 20% threshold
            {{latency_p95_us}, 20},
            {{latency_p99_us}, 20}
        ]
    ).
```

---

## 7. Conclusion

### 7.1 Summary

This plan establishes:

1. **Performance Baselines**: v3.0 targets for all operations (core, JSON, transport, features)
2. **Regression Detection**: Thresholds and automation for catching performance issues
3. **Benchmark Automation**: Complete suite of benchmarks with CI/CD integration
4. **Performance Validation**: Pre-release checklist and response procedures

### 7.2 Success Criteria

erlmcp v3.0 performance is successful if:

- [ ] All v3.0 targets met (native JSON 2.5x faster, priority messages <1ms)
- [ ] No critical regressions vs v2.0.0 (>20% degradation)
- [ ] Baseline captured and committed
- [ ] CI/CD performance checks operational
- [ ] Daily monitoring dashboard deployed

### 7.3 Next Steps

1. **Implement new benchmark runners** (native JSON, priority messages, SSE resumption, resources)
2. **Capture v3.0 baseline** on reference hardware
3. **Enable CI/CD integration** (update workflows with v3.0 targets)
4. **Deploy performance dashboard** for real-time monitoring

---

**Document Status**: Draft  
**Review Date**: 2026-02-07  
**Approval**: Performance Team Lead
