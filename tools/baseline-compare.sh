#!/usr/bin/env bash
###====================================================================
### baseline-compare.sh - Compare Performance to Baseline
###====================================================================
### Runs benchmarks and compares results to latest baseline.
### Flags regressions (>10% slower, >20% more memory).
### Generates HTML comparison report.
###
### Usage:
###   ./tools/baseline-compare.sh [--baseline FILE] [--threshold PCT]
###
### Options:
###   --baseline FILE    Compare to specific baseline (default: latest)
###   --threshold PCT    Regression threshold (default: 10 for perf, 20 for memory)
###   --html-only        Skip benchmark run, only generate report from latest results
###
### Exit Codes:
###   0 - No regressions
###   1 - Regressions detected
###   2 - Error (no baseline, benchmark failure)
###
### Example:
###   ./tools/baseline-compare.sh
###   # Runs benchmarks, compares to latest baseline
###   # Output: bench/baselines/comparison_1769567400.html
###====================================================================

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Defaults
BASELINE_FILE=""
PERF_THRESHOLD=10  # 10% slower is regression
MEMORY_THRESHOLD=20  # 20% more memory is regression
HTML_ONLY=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --baseline)
            BASELINE_FILE="$2"
            shift 2
            ;;
        --threshold)
            PERF_THRESHOLD="$2"
            MEMORY_THRESHOLD=$((PERF_THRESHOLD * 2))
            shift 2
            ;;
        --html-only)
            HTML_ONLY=true
            shift
            ;;
        -h|--help)
            head -n 25 "$0" | grep "^###" | sed 's/^### //'
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

# Project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

# Find latest baseline if not specified
if [ -z "$BASELINE_FILE" ]; then
    BASELINE_FILE=$(ls -t bench/baselines/*.json 2>/dev/null | head -1)
fi

if [ -z "$BASELINE_FILE" ] || [ ! -f "$BASELINE_FILE" ]; then
    echo -e "${RED}ERROR: No baseline found${NC}"
    echo "Run: ./tools/baseline-capture.sh"
    exit 2
fi

BASELINE_NAME=$(basename "$BASELINE_FILE")

echo "========================================"
echo "erlmcp Performance Comparison"
echo "========================================"
echo "Baseline: $BASELINE_NAME"
echo "Thresholds: ${PERF_THRESHOLD}% (perf), ${MEMORY_THRESHOLD}% (memory)"
echo ""

# Run benchmarks if not --html-only
if [ "$HTML_ONLY" = false ]; then
    echo -e "${YELLOW}[1/3] Running benchmarks...${NC}"
    
    # Compile
    if ! TERM=dumb rebar3 compile 2>&1 | grep -q "^"; then
        echo -e "${RED}✗ Compilation failed${NC}"
        exit 2
    fi
    
    # Run same benchmarks as baseline
    echo "  core_ops_100k..."
    erl -pa _build/default/lib/*/ebin -noshell \
        -eval "erlmcp_bench_core_ops:run(<<\"core_ops_100k\">>), halt()." \
        2>&1 | grep -v "^Erlang/OTP" | tail -3
    
    echo "  tcp_quick_1k..."
    erl -pa _build/default/lib/*/ebin -noshell \
        -eval "erlmcp_bench_network_real:run(<<\"tcp_quick_1k\">>), halt()." \
        2>&1 | grep -v "^Erlang/OTP" | tail -3
    
    echo "  stress_30s_100k_ops..."
    erl -pa _build/default/lib/*/ebin -noshell \
        -eval "erlmcp_bench_stress:run(<<\"stress_30s_100k_ops\">>), halt()." \
        2>&1 | grep -v "^Erlang/OTP" | tail -3
    
    echo -e "${GREEN}✓ Benchmarks complete${NC}"
    echo ""
fi

# Compare results
echo -e "${YELLOW}[2/3] Comparing results...${NC}"

# Extract baseline metrics
if command -v jq &> /dev/null; then
    # Parse baseline with jq
    BASELINE_CORE_OPS_THROUGHPUT=$(jq -r '.benchmarks.core_ops_100k.throughput_msg_per_s // 0' "$BASELINE_FILE")
    BASELINE_CORE_OPS_LATENCY=$(jq -r '.benchmarks.core_ops_100k.latency_p95_us // 0' "$BASELINE_FILE")
    BASELINE_CORE_OPS_MEMORY=$(jq -r '.benchmarks.core_ops_100k.memory_delta_mib // 0' "$BASELINE_FILE")
    
    BASELINE_TCP_THROUGHPUT=$(jq -r '.benchmarks.tcp_quick_1k.throughput_msg_per_s // 0' "$BASELINE_FILE")
    BASELINE_TCP_LATENCY=$(jq -r '.benchmarks.tcp_quick_1k.latency_p95_us // 0' "$BASELINE_FILE")
    BASELINE_TCP_MEMORY=$(jq -r '.benchmarks.tcp_quick_1k.memory_delta_mib // 0' "$BASELINE_FILE")
    
    BASELINE_STRESS_THROUGHPUT=$(jq -r '.benchmarks.stress_30s_100k_ops.throughput_msg_per_s // 0' "$BASELINE_FILE")
    BASELINE_STRESS_LATENCY=$(jq -r '.benchmarks.stress_30s_100k_ops.latency_p95_us // 0' "$BASELINE_FILE")
    BASELINE_STRESS_MEMORY=$(jq -r '.benchmarks.stress_30s_100k_ops.memory_delta_mib // 0' "$BASELINE_FILE")
else
    # Fallback to grep
    BASELINE_CORE_OPS_THROUGHPUT=$(grep -A 5 '"core_ops_100k"' "$BASELINE_FILE" | grep throughput_msg_per_s | awk -F: '{print $2}' | tr -d ', ')
    BASELINE_CORE_OPS_LATENCY=$(grep -A 5 '"core_ops_100k"' "$BASELINE_FILE" | grep latency_p95_us | awk -F: '{print $2}' | tr -d ', ')
    BASELINE_CORE_OPS_MEMORY=$(grep -A 5 '"core_ops_100k"' "$BASELINE_FILE" | grep memory_delta_mib | awk -F: '{print $2}' | tr -d ', ')
    
    BASELINE_TCP_THROUGHPUT=$(grep -A 5 '"tcp_quick_1k"' "$BASELINE_FILE" | grep throughput_msg_per_s | awk -F: '{print $2}' | tr -d ', ')
    BASELINE_TCP_LATENCY=$(grep -A 5 '"tcp_quick_1k"' "$BASELINE_FILE" | grep latency_p95_us | awk -F: '{print $2}' | tr -d ', ')
    BASELINE_TCP_MEMORY=$(grep -A 5 '"tcp_quick_1k"' "$BASELINE_FILE" | grep memory_delta_mib | awk -F: '{print $2}' | tr -d ', ')
    
    BASELINE_STRESS_THROUGHPUT=$(grep -A 5 '"stress_30s_100k_ops"' "$BASELINE_FILE" | grep throughput_msg_per_s | awk -F: '{print $2}' | tr -d ', ')
    BASELINE_STRESS_LATENCY=$(grep -A 5 '"stress_30s_100k_ops"' "$BASELINE_FILE" | grep latency_p95_us | awk -F: '{print $2}' | tr -d ', ')
    BASELINE_STRESS_MEMORY=$(grep -A 5 '"stress_30s_100k_ops"' "$BASELINE_FILE" | grep memory_delta_mib | awk -F: '{print $2}' | tr -d ', ')
fi

# Extract current results
CORE_OPS_FILE=$(ls -t bench/results/core_ops_core_ops_100k_*.json 2>/dev/null | head -1)
TCP_FILE=$(ls -t bench/results/network_real_tcp_quick_1k_*.json 2>/dev/null | head -1)
STRESS_FILE=$(ls -t bench/results/stress_stress_30s_100k_ops_*.json 2>/dev/null | head -1)

extract_current() {
    local file=$1
    local metric=$2
    
    if [ ! -f "$file" ]; then
        echo "0"
        return
    fi
    
    grep -o "\"$metric\":[[:space:]]*[0-9.]*" "$file" | head -1 | awk -F: '{print $2}' | xargs
}

CURRENT_CORE_OPS_THROUGHPUT=$(extract_current "$CORE_OPS_FILE" "throughput_msg_per_s")
CURRENT_CORE_OPS_LATENCY=$(extract_current "$CORE_OPS_FILE" "latency_p95_us")
CURRENT_CORE_OPS_MEMORY=$(extract_current "$CORE_OPS_FILE" "memory_delta_mib")

CURRENT_TCP_THROUGHPUT=$(extract_current "$TCP_FILE" "throughput_msg_per_s")
CURRENT_TCP_LATENCY=$(extract_current "$TCP_FILE" "latency_p95_us")
CURRENT_TCP_MEMORY=$(extract_current "$TCP_FILE" "memory_delta_mib")

CURRENT_STRESS_THROUGHPUT=$(extract_current "$STRESS_FILE" "throughput_msg_per_s")
CURRENT_STRESS_LATENCY=$(extract_current "$STRESS_FILE" "latency_p95_us")
CURRENT_STRESS_MEMORY=$(extract_current "$STRESS_FILE" "memory_delta_mib")

# Calculate deltas
calc_delta() {
    local baseline=$1
    local current=$2
    
    if [ "$baseline" = "0" ] || [ -z "$baseline" ] || [ "$current" = "0" ] || [ -z "$current" ]; then
        echo "0"
        return
    fi
    
    echo "scale=2; (($current - $baseline) / $baseline) * 100" | bc
}

CORE_OPS_THROUGHPUT_DELTA=$(calc_delta "$BASELINE_CORE_OPS_THROUGHPUT" "$CURRENT_CORE_OPS_THROUGHPUT")
CORE_OPS_LATENCY_DELTA=$(calc_delta "$BASELINE_CORE_OPS_LATENCY" "$CURRENT_CORE_OPS_LATENCY")
CORE_OPS_MEMORY_DELTA=$(calc_delta "$BASELINE_CORE_OPS_MEMORY" "$CURRENT_CORE_OPS_MEMORY")

TCP_THROUGHPUT_DELTA=$(calc_delta "$BASELINE_TCP_THROUGHPUT" "$CURRENT_TCP_THROUGHPUT")
TCP_LATENCY_DELTA=$(calc_delta "$BASELINE_TCP_LATENCY" "$CURRENT_TCP_LATENCY")
TCP_MEMORY_DELTA=$(calc_delta "$BASELINE_TCP_MEMORY" "$CURRENT_TCP_MEMORY")

STRESS_THROUGHPUT_DELTA=$(calc_delta "$BASELINE_STRESS_THROUGHPUT" "$CURRENT_STRESS_THROUGHPUT")
STRESS_LATENCY_DELTA=$(calc_delta "$BASELINE_STRESS_LATENCY" "$CURRENT_STRESS_LATENCY")
STRESS_MEMORY_DELTA=$(calc_delta "$BASELINE_STRESS_MEMORY" "$CURRENT_STRESS_MEMORY")

# Check for regressions
REGRESSIONS=0

is_regression() {
    local delta=$1
    local threshold=$2
    local inverse=${3:-false}  # true for throughput (negative is bad)
    
    if [ "$inverse" = "true" ]; then
        # Throughput: negative delta > threshold is regression
        echo "scale=2; -$delta > $threshold" | bc -l | grep -q 1 && echo "true" || echo "false"
    else
        # Latency/Memory: positive delta > threshold is regression
        echo "scale=2; $delta > $threshold" | bc -l | grep -q 1 && echo "true" || echo "false"
    fi
}

# Print comparison table
echo ""
echo "Performance Comparison (vs $BASELINE_NAME)"
echo "========================================"
echo ""

print_comparison() {
    local name=$1
    local baseline=$2
    local current=$3
    local delta=$4
    local unit=$5
    local threshold=$6
    local inverse=${7:-false}
    
    local status="✓"
    local color=$GREEN
    
    if [ "$(is_regression "$delta" "$threshold" "$inverse")" = "true" ]; then
        status="❌ REGRESSION"
        color=$RED
        REGRESSIONS=$((REGRESSIONS + 1))
    fi
    
    printf "${color}%s:${NC}\n" "$name"
    printf "  Baseline: %s %s\n" "$baseline" "$unit"
    printf "  Current:  %s %s\n" "$current" "$unit"
    printf "  Delta:    %+.1f%% %s\n" "$delta" "$status"
    printf "\n"
}

echo "core_ops_100k:"
print_comparison "  Throughput" "$BASELINE_CORE_OPS_THROUGHPUT" "$CURRENT_CORE_OPS_THROUGHPUT" "$CORE_OPS_THROUGHPUT_DELTA" "msg/s" "$PERF_THRESHOLD" "true"
print_comparison "  Latency p95" "$BASELINE_CORE_OPS_LATENCY" "$CURRENT_CORE_OPS_LATENCY" "$CORE_OPS_LATENCY_DELTA" "µs" "$PERF_THRESHOLD" "false"
print_comparison "  Memory" "$BASELINE_CORE_OPS_MEMORY" "$CURRENT_CORE_OPS_MEMORY" "$CORE_OPS_MEMORY_DELTA" "MiB" "$MEMORY_THRESHOLD" "false"

echo "tcp_quick_1k:"
print_comparison "  Throughput" "$BASELINE_TCP_THROUGHPUT" "$CURRENT_TCP_THROUGHPUT" "$TCP_THROUGHPUT_DELTA" "msg/s" "$PERF_THRESHOLD" "true"
print_comparison "  Latency p95" "$BASELINE_TCP_LATENCY" "$CURRENT_TCP_LATENCY" "$TCP_LATENCY_DELTA" "µs" "$PERF_THRESHOLD" "false"
print_comparison "  Memory" "$BASELINE_TCP_MEMORY" "$CURRENT_TCP_MEMORY" "$TCP_MEMORY_DELTA" "MiB" "$MEMORY_THRESHOLD" "false"

echo "stress_30s_100k_ops:"
print_comparison "  Throughput" "$BASELINE_STRESS_THROUGHPUT" "$CURRENT_STRESS_THROUGHPUT" "$STRESS_THROUGHPUT_DELTA" "msg/s" "$PERF_THRESHOLD" "true"
print_comparison "  Latency p95" "$BASELINE_STRESS_LATENCY" "$CURRENT_STRESS_LATENCY" "$STRESS_LATENCY_DELTA" "µs" "$PERF_THRESHOLD" "false"
print_comparison "  Memory" "$BASELINE_STRESS_MEMORY" "$CURRENT_STRESS_MEMORY" "$STRESS_MEMORY_DELTA" "MiB" "$MEMORY_THRESHOLD" "false"

# Generate HTML report
echo -e "${YELLOW}[3/3] Generating HTML report...${NC}"

TIMESTAMP=$(date +%s)
HTML_FILE="bench/baselines/comparison_${TIMESTAMP}.html"

cat > "$HTML_FILE" << 'HTML_START'
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>erlmcp Performance Comparison</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }
        .container { max-width: 1200px; margin: 0 auto; background: white; padding: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        h1 { color: #333; border-bottom: 2px solid #4CAF50; padding-bottom: 10px; }
        .metric-table { width: 100%; border-collapse: collapse; margin: 20px 0; }
        .metric-table th { background: #4CAF50; color: white; padding: 12px; text-align: left; }
        .metric-table td { padding: 10px; border: 1px solid #ddd; }
        .metric-table tr:nth-child(even) { background: #f9f9f9; }
        .regression { background: #ffebee !important; font-weight: bold; }
        .improvement { background: #e8f5e9 !important; }
        .neutral { background: #fff9c4 !important; }
        .delta-positive { color: #d32f2f; }
        .delta-negative { color: #388e3c; }
        .delta-neutral { color: #f57c00; }
        .summary { padding: 15px; margin: 20px 0; border-radius: 4px; }
        .summary.pass { background: #e8f5e9; border-left: 4px solid #4CAF50; }
        .summary.fail { background: #ffebee; border-left: 4px solid #f44336; }
    </style>
</head>
<body>
    <div class="container">
        <h1>erlmcp Performance Comparison</h1>
        <p><strong>Baseline:</strong> BASELINE_NAME</p>
        <p><strong>Date:</strong> COMPARISON_DATE</p>
        <p><strong>Thresholds:</strong> PERF_THRESHOLD% (performance), MEMORY_THRESHOLD% (memory)</p>
HTML_START

# Add summary
if [ $REGRESSIONS -eq 0 ]; then
    cat >> "$HTML_FILE" << 'HTML_SUMMARY_PASS'
        <div class="summary pass">
            <strong>✓ PASS:</strong> No performance regressions detected.
        </div>
HTML_SUMMARY_PASS
else
    cat >> "$HTML_FILE" << HTML_SUMMARY_FAIL
        <div class="summary fail">
            <strong>✗ FAIL:</strong> $REGRESSIONS regression(s) detected.
        </div>
HTML_SUMMARY_FAIL
fi

# Add tables
cat >> "$HTML_FILE" << HTML_TABLE_START
        <h2>Benchmark Results</h2>
        <table class="metric-table">
            <thead>
                <tr>
                    <th>Benchmark</th>
                    <th>Metric</th>
                    <th>Baseline</th>
                    <th>Current</th>
                    <th>Delta</th>
                    <th>Status</th>
                </tr>
            </thead>
            <tbody>
HTML_TABLE_START

add_row() {
    local benchmark=$1
    local metric=$2
    local baseline=$3
    local current=$4
    local delta=$5
    local unit=$6
    local is_regress=$7
    
    local row_class=""
    local delta_class="delta-neutral"
    local status="✓ OK"
    
    if [ "$is_regress" = "true" ]; then
        row_class="regression"
        status="❌ REGRESSION"
    elif echo "$delta < 0" | bc -l | grep -q 1; then
        row_class="improvement"
        delta_class="delta-negative"
    fi
    
    cat >> "$HTML_FILE" << HTML_ROW
                <tr class="$row_class">
                    <td>$benchmark</td>
                    <td>$metric</td>
                    <td>$baseline $unit</td>
                    <td>$current $unit</td>
                    <td class="$delta_class">$delta%</td>
                    <td>$status</td>
                </tr>
HTML_ROW
}

# Add all rows
add_row "core_ops_100k" "Throughput" "$BASELINE_CORE_OPS_THROUGHPUT" "$CURRENT_CORE_OPS_THROUGHPUT" "$(printf '%.1f' $CORE_OPS_THROUGHPUT_DELTA)" "msg/s" "$(is_regression "$CORE_OPS_THROUGHPUT_DELTA" "$PERF_THRESHOLD" "true")"
add_row "core_ops_100k" "Latency p95" "$BASELINE_CORE_OPS_LATENCY" "$CURRENT_CORE_OPS_LATENCY" "$(printf '%.1f' $CORE_OPS_LATENCY_DELTA)" "µs" "$(is_regression "$CORE_OPS_LATENCY_DELTA" "$PERF_THRESHOLD" "false")"
add_row "core_ops_100k" "Memory" "$BASELINE_CORE_OPS_MEMORY" "$CURRENT_CORE_OPS_MEMORY" "$(printf '%.1f' $CORE_OPS_MEMORY_DELTA)" "MiB" "$(is_regression "$CORE_OPS_MEMORY_DELTA" "$MEMORY_THRESHOLD" "false")"

add_row "tcp_quick_1k" "Throughput" "$BASELINE_TCP_THROUGHPUT" "$CURRENT_TCP_THROUGHPUT" "$(printf '%.1f' $TCP_THROUGHPUT_DELTA)" "msg/s" "$(is_regression "$TCP_THROUGHPUT_DELTA" "$PERF_THRESHOLD" "true")"
add_row "tcp_quick_1k" "Latency p95" "$BASELINE_TCP_LATENCY" "$CURRENT_TCP_LATENCY" "$(printf '%.1f' $TCP_LATENCY_DELTA)" "µs" "$(is_regression "$TCP_LATENCY_DELTA" "$PERF_THRESHOLD" "false")"
add_row "tcp_quick_1k" "Memory" "$BASELINE_TCP_MEMORY" "$CURRENT_TCP_MEMORY" "$(printf '%.1f' $TCP_MEMORY_DELTA)" "MiB" "$(is_regression "$TCP_MEMORY_DELTA" "$MEMORY_THRESHOLD" "false")"

add_row "stress_30s_100k_ops" "Throughput" "$BASELINE_STRESS_THROUGHPUT" "$CURRENT_STRESS_THROUGHPUT" "$(printf '%.1f' $STRESS_THROUGHPUT_DELTA)" "msg/s" "$(is_regression "$STRESS_THROUGHPUT_DELTA" "$PERF_THRESHOLD" "true")"
add_row "stress_30s_100k_ops" "Latency p95" "$BASELINE_STRESS_LATENCY" "$CURRENT_STRESS_LATENCY" "$(printf '%.1f' $STRESS_LATENCY_DELTA)" "µs" "$(is_regression "$STRESS_LATENCY_DELTA" "$PERF_THRESHOLD" "false")"
add_row "stress_30s_100k_ops" "Memory" "$BASELINE_STRESS_MEMORY" "$CURRENT_STRESS_MEMORY" "$(printf '%.1f' $STRESS_MEMORY_DELTA)" "MiB" "$(is_regression "$STRESS_MEMORY_DELTA" "$MEMORY_THRESHOLD" "false")"

# Close HTML
cat >> "$HTML_FILE" << 'HTML_END'
            </tbody>
        </table>
    </div>
</body>
</html>
HTML_END

# Replace placeholders
sed -i '' "s/BASELINE_NAME/$BASELINE_NAME/g" "$HTML_FILE"
sed -i '' "s/COMPARISON_DATE/$(date -u +%Y-%m-%dT%H:%M:%SZ)/g" "$HTML_FILE"
sed -i '' "s/PERF_THRESHOLD/$PERF_THRESHOLD/g" "$HTML_FILE"
sed -i '' "s/MEMORY_THRESHOLD/$MEMORY_THRESHOLD/g" "$HTML_FILE"

echo -e "${GREEN}✓ HTML report: $HTML_FILE${NC}"
echo ""

# Summary
if [ $REGRESSIONS -eq 0 ]; then
    echo -e "${GREEN}✓ PASS: No regressions detected${NC}"
    exit 0
else
    echo -e "${RED}✗ FAIL: $REGRESSIONS regression(s) detected${NC}"
    echo "See: $HTML_FILE"
    exit 1
fi
