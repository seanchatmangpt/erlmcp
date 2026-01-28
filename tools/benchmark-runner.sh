#!/usr/bin/env bash
# Automated Benchmark Runner - Detects regressions
# Usage: ./tools/benchmark-runner.sh
# Exit codes: 0 = success, 1 = regression detected

set -euo pipefail

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
REGRESSION_THRESHOLD=10  # Percent
BASELINE_DIR="bench/baselines"
RESULTS_DIR="_build/test/benchmarks"
RESULTS_JSON="${RESULTS_DIR}/benchmark_results.json"
REGRESSION_REPORT="${RESULTS_DIR}/regression_report.txt"

echo "========================================"
echo "ErlMCP Benchmark Runner"
echo "========================================"
echo ""

# Create directories
mkdir -p "${BASELINE_DIR}"
mkdir -p "${RESULTS_DIR}"

# Initialize results
cat > "${RESULTS_JSON}" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "benchmarks": []
}
EOF

# Track regressions
TOTAL_BENCHMARKS=0
REGRESSIONS=0
declare -a REGRESSION_DETAILS

# Function to run benchmark and compare
run_benchmark() {
    local bench_module="$1"
    local workload_id="$2"
    local baseline_file="${BASELINE_DIR}/${bench_module}_${workload_id}.baseline"

    echo "Running: ${bench_module}:run(<<\"${workload_id}\">>)"

    # Run benchmark
    local bench_output="${RESULTS_DIR}/${bench_module}_${workload_id}.txt"
    erl -pa _build/default/lib/*/ebin -noshell -eval \
        "${bench_module}:run(<<\"${workload_id}\">>), init:stop()." \
        2>&1 | tee "${bench_output}"

    # Extract throughput
    local current_throughput=$(grep "throughput_msg_per_s" "${bench_output}" | \
        grep -oE '[0-9]+' | head -1 || echo "0")

    TOTAL_BENCHMARKS=$((TOTAL_BENCHMARKS + 1))

    # Check if baseline exists
    if [ -f "${baseline_file}" ]; then
        local baseline_throughput=$(cat "${baseline_file}")

        # Calculate percentage change
        if [ "${baseline_throughput}" -gt 0 ]; then
            local change=$((100 * (current_throughput - baseline_throughput) / baseline_throughput))
            local abs_change=${change#-}  # Absolute value

            if [ "${change}" -lt 0 ] && [ "${abs_change}" -gt "${REGRESSION_THRESHOLD}" ]; then
                REGRESSIONS=$((REGRESSIONS + 1))
                REGRESSION_DETAILS+=("${bench_module}:${workload_id}: ${change}% (${current_throughput} vs ${baseline_throughput} msg/s)")
                echo -e "${RED}  ❌ REGRESSION: ${change}%${NC}"
            elif [ "${change}" -lt 0 ]; then
                echo -e "${YELLOW}  ⚠️  MINOR DECLINE: ${change}%${NC}"
            else
                echo -e "${GREEN}  ✅ IMPROVEMENT: +${change}%${NC}"
            fi
        fi
    else
        echo -e "${YELLOW}  ⚠️  No baseline - creating new baseline${NC}"
        echo "${current_throughput}" > "${baseline_file}"
    fi

    echo ""
}

echo "Starting benchmark suite..."
echo "----------------------------------------"
echo ""

# Ensure benchmarks are compiled
echo "Compiling benchmarks..."
TERM=dumb rebar3 compile

# Run core_ops benchmarks
echo "1. Core Operations Benchmarks"
echo "----------------------------------------"
run_benchmark "erlmcp_bench_core_ops" "core_ops_1k"
run_benchmark "erlmcp_bench_core_ops" "core_ops_10k"
run_benchmark "erlmcp_bench_core_ops" "core_ops_100k"

# Run network benchmarks
echo "2. Network Benchmarks"
echo "----------------------------------------"
run_benchmark "erlmcp_bench_network_real" "tcp_sustained_1k"
run_benchmark "erlmcp_bench_network_real" "tcp_sustained_10k"
run_benchmark "erlmcp_bench_network_real" "http_sustained_1k"

# Run stress test (quick version)
echo "3. Stress Test (30s)"
echo "----------------------------------------"
run_benchmark "erlmcp_bench_stress" "stress_30s_10k_ops"

# Run chaos test (quick version)
echo "4. Chaos Test"
echo "----------------------------------------"
run_benchmark "erlmcp_bench_chaos" "chaos_network_partition"

# Run integration test
echo "5. Integration Test"
echo "----------------------------------------"
run_benchmark "erlmcp_bench_integration" "mcp_tool_sequence"

# Generate regression report
echo "========================================"
echo "BENCHMARK SUMMARY"
echo "========================================"
echo "Total Benchmarks: ${TOTAL_BENCHMARKS}"
echo "Regressions:      ${REGRESSIONS}"
echo "Threshold:        ${REGRESSION_THRESHOLD}%"
echo ""

if [ "${REGRESSIONS}" -gt 0 ]; then
    echo -e "${RED}========================================"
    echo "REGRESSION DETAILS"
    echo "========================================${NC}"
    echo ""

    # Write to report file
    {
        echo "Benchmark Regression Report"
        echo "Generated: $(date)"
        echo ""
        echo "Threshold: ${REGRESSION_THRESHOLD}%"
        echo "Regressions detected: ${REGRESSIONS}"
        echo ""
        echo "Details:"
        for detail in "${REGRESSION_DETAILS[@]}"; do
            echo "  - ${detail}"
        done
    } > "${REGRESSION_REPORT}"

    # Print to console
    for detail in "${REGRESSION_DETAILS[@]}"; do
        echo -e "${RED}  ❌ ${detail}${NC}"
    done
    echo ""

    echo -e "${RED}❌ FAILURE: ${REGRESSIONS} regression(s) detected (>${REGRESSION_THRESHOLD}%)${NC}"
    echo ""
    echo "Report saved to: ${REGRESSION_REPORT}"
    echo "Results saved to: ${RESULTS_JSON}"
    exit 1
else
    echo -e "${GREEN}✅ SUCCESS: No regressions detected${NC}"
    echo ""
    echo "Results saved to: ${RESULTS_JSON}"
    exit 0
fi
