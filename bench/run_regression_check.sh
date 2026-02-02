#!/bin/bash

# =============================================================================
# ERLMCP Performance Regression Detection Script v2.0
#
# Automated performance benchmarking with regression detection
# Alerts on >10% degradation from baseline metrics
#
# Usage:
#   ./run_regression_check.sh [baseline_file]
#   ./run_regression_check.sh --verbose
#   ./run_regression_check.sh --baseline custom_baseline.json
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/.."

# Configuration
BASELINE_FILE="${1:-bench/results/performance_summary_20250201.json}"
REGRESSION_THRESHOLD=0.1  # 10% threshold
VERBOSE=${2:-false}
RESULTS_FILE="bench/results/regression_check_$(date +%Y%m%d_%H%M%S).json"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if baseline file exists
check_baseline_file() {
    if [[ ! -f "$BASELINE_FILE" ]]; then
        log_error "Baseline file not found: $BASELINE_FILE"
        log_info "Usage: $0 [baseline_file]"
        exit 1
    fi
    log_info "Using baseline: $BASELINE_FILE"
}

# Run current benchmarks
run_benchmarks() {
    log_info "Running current performance benchmarks..."

    # Clean previous build
    TERM=dumb rebar3 compile

    if [[ "$VERBOSE" == "true" ]]; then
        log_info "Registry benchmark..."
        rebar3 eunit --module=erlmcp_bench_registry
        log_info "Queue benchmark..."
        rebar3 eunit --module=erlmcp_bench_queue
        log_info "Core operations benchmark..."
        rebar3 eunit --module=erlmcp_bench_core_ops
    else
        # Run benchmarks silently
        rebar3 eunit --module=erlmcp_bench_registry >/dev/null 2>&1
        rebar3 eunit --module=erlmcp_bench_queue >/dev/null 2>&1
        rebar3 eunit --module=erlmcp_bench_core_ops >/dev/null 2>&1
    fi

    log_success "Benchmarks completed"
}

# Extract benchmark results
extract_results() {
    log_info "Extracting benchmark results..."

    # Create temporary results file
    TEMP_RESULTS=$(mktemp)

    # Extract registry performance
    REGISTRY_MSG=$(grep "Registry throughput" bench/results/*.json | tail -1 | sed 's/.*: \([0-9,]*\) msg\/sec.*/\1/' | tr -d ',')
    REGISTRY_RATE=$(echo "scale=2; $REGISTRY_MSG / 1000000" | bc -l)

    # Extract queue performance
    QUEUE_MSG=$(grep "Queue throughput" bench/results/*.json | tail -1 | sed 's/.*: \([0-9,]*\) msg\/sec.*/\1/' | tr -d ',')
    QUEUE_RATE=$(echo "scale=2; $QUEUE_MSG / 1000000" | bc -l)

    # Extract concurrent connections
    CONN=$(grep "Concurrent connections" bench/results/*.json | tail -1 | sed 's/.*: \([0-9]*\)\/.*/\1/')

    # Extract latency metrics
    P50_LATENCY=$(grep "p50" bench/results/*.json | tail -1 | sed 's/.*: \([0-9.]*\)ms.*/\1/')
    P95_LATENCY=$(grep "p95" bench/results/*.json | tail -1 | sed 's/.*: \([0-9.]*\)ms.*/\1/')
    P99_LATENCY=$(grep "p99" bench/results/*.json | tail -1 | sed 's/.*: \([0-9.]*\)ms.*/\1/')

    # Build current results JSON
    cat << EOF > "$TEMP_RESULTS"
{
    "timestamp": "$(date -u '+%Y-%m-%dT%H:%M:%SZ')",
    "registry_throughput": {"msg_sec": $REGISTRY_MSG, "msg_rate_m": $REGISTRY_RATE},
    "queue_throughput": {"msg_sec": $QUEUE_MSG, "msg_rate_m": $QUEUE_RATE},
    "concurrent_connections": $CONN,
    "latency_ms": {"p50": $P50_LATENCY, "p95": $P95_LATENCY, "p99": $P99_LATENCY},
    "baseline_file": "$BASELINE_FILE"
}
EOF

    mv "$TEMP_RESULTS" "$RESULTS_FILE"
    log_success "Results saved to: $RESULTS_FILE"
}

# Compare with baseline
check_regressions() {
    log_info "Comparing with baseline..."

    # Use jq if available, otherwise fall back to basic JSON parsing
    if command -v jq >/dev/null 2>&1; then
        REGRESSION_FOUND=false

        # Extract baseline values
        BASELINE_REGISTRY=$(jq -r '.registry_throughput.msg_sec' "$BASELINE_FILE")
        BASELINE_QUEUE=$(jq -r '.queue_throughput.msg_sec' "$BASELINE_FILE")

        # Extract current values
        CURRENT_REGISTRY=$(jq -r '.registry_throughput.msg_sec' "$RESULTS_FILE")
        CURRENT_QUEUE=$(jq -r '.queue_throughput.msg_sec' "$RESULTS_FILE")

        # Calculate percentage changes
        REGISTRY_CHANGE=$(echo "scale=2; ($CURRENT_REGISTRY - $BASELINE_REGISTRY) / $BASELINE_REGISTRY" | bc -l)
        QUEUE_CHANGE=$(echo "scale=2; ($CURRENT_QUEUE - $BASELINE_QUEUE) / $BASELINE_QUEUE" | bc -l)

        # Check for regressions
        if (( $(echo "$REGISTRY_CHANGE < -$REGRESSION_THRESHOLD" | bc -l) )); then
            log_error "REGRESSION DETECTED: Registry performance degraded by $(echo "scale=1; ${REGISTRY_CHANGE#-} * 100" | bc -l)%"
            log_error "  Baseline: $BASELINE_REGISTRY msg/sec"
            log_error "  Current:  $CURRENT_REGISTRY msg/sec"
            REGRESSION_FOUND=true
        else
            log_success "Registry performance OK ($(echo "scale=1; ${REGISTRY_CHANGE#-} * 100" | bc -l)%)"
        fi

        if (( $(echo "$QUEUE_CHANGE < -$REGRESSION_THRESHOLD" | bc -l) )); then
            log_error "REGRESSION DETECTED: Queue performance degraded by $(echo "scale=1; ${QUEUE_CHANGE#-} * 100" | bc -l)%"
            log_error "  Baseline: $BASELINE_QUEUE msg/sec"
            log_error "  Current:  $CURRENT_QUEUE msg/sec"
            REGRESSION_FOUND=true
        else
            log_success "Queue performance OK ($(echo "scale=1; ${QUEUE_CHANGE#-} * 100" | bc -l)%)"
        fi

        if [[ "$REGRESSION_FOUND" == "true" ]]; then
            exit 1
        fi
    else
        log_warning "jq not available, skipping detailed regression analysis"
        log_info "Check $RESULTS_FILE for manual comparison"
    fi
}

# Generate report
generate_report() {
    log_info "Generating regression report..."

    REPORT_FILE="bench/results/regression_report_$(date +%Y%m%d).md"

    cat << EOF > "$REPORT_FILE"
# ERLMCP Performance Regression Report

**Generated:** $(date)
**Baseline:** $BASELINE_FILE
**Results:** $RESULTS_FILE

## Performance Summary

| Metric | Baseline | Current | Change | Status |
|--------|----------|---------|---------|--------|
| Registry Throughput | $(jq -r '.registry_throughput.msg_sec' "$BASELINE_FILE") | $(jq -r '.registry_throughput.msg_sec' "$RESULTS_FILE") | $(echo "scale=1; ($(jq -r '.registry_throughput.msg_sec' "$RESULTS_FILE") - $(jq -r '.registry_throughput.msg_sec' "$BASELINE_FILE")) / $(jq -r '.registry_throughput.msg_sec' "$BASELINE_FILE") * 100" | bc -l)% | OK |
| Queue Throughput | $(jq -r '.queue_throughput.msg_sec' "$BASELINE_FILE") | $(jq -r '.queue_throughput.msg_sec' "$RESULTS_FILE") | $(echo "scale=1; ($(jq -r '.queue_throughput.msg_sec' "$RESULTS_FILE") - $(jq -r '.queue_throughput.msg_sec' "$BASELINE_FILE")) / $(jq -r '.queue_throughput.msg_sec' "$BASELINE_FILE") * 100" | bc -l)% | OK |
| Concurrent Connections | $(jq -r '.concurrent_connections' "$BASELINE_FILE") | $(jq -r '.concurrent_connections' "$RESULTS_FILE") | N/A | OK |

## Recommendations

### Performance Optimization Opportunities

1. **Registry Read Optimization**
   - Implement ETS-based read cache with TTL
   - Expected improvement: 10-20%

2. **Batch Operations**
   - Implement batch unregistration API
   - Reduce individual unregistration overhead

EOF

    log_success "Report generated: $REPORT_FILE"
}

# Main execution
main() {
    log_info "Starting ERLMCP performance regression check"

    check_baseline_file
    run_benchmarks
    extract_results
    check_regressions
    generate_report

    log_success "Regression check completed successfully"
}

# Execute main function
main "$@"
