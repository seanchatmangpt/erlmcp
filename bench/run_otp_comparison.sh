#!/bin/bash
# OTP Version Comparison Benchmark Runner
# Runs comprehensive benchmarks across installed OTP versions

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
RESULTS_DIR="$PROJECT_ROOT/bench/results/otp_comparison"

echo "============================================"
echo "OTP Version Comparison Benchmark Suite"
echo "============================================"
echo ""

# Ensure results directory exists
mkdir -p "$RESULTS_DIR"

# Get current OTP version
CURRENT_OTP=$(erl -noshell -eval "io:format('~s', [erlang:system_info(otp_release)]), halt().")
echo "Current OTP Version: $CURRENT_OTP"
echo ""

# Compile the benchmark module
echo "Compiling benchmark module..."
cd "$PROJECT_ROOT"
erlc -o "$SCRIPT_DIR" -I "$PROJECT_ROOT/apps/erlmcp_core/include" \
     "$SCRIPT_DIR/erlmcp_bench_otp_comparison.erl"

# Run the full benchmark suite
echo ""
echo "Running full benchmark suite..."
echo ""

erl -noshell -pa "$SCRIPT_DIR" \
    -eval "application:ensure_all_started(gproc)" \
    -eval "erlmcp_bench_otp_comparison:run_full()" \
    -s init stop

echo ""
echo "============================================"
echo "Benchmark Complete"
echo "============================================"
echo ""
echo "Results saved to: $RESULTS_DIR"
echo ""

# Display latest results
LATEST_RESULT=$(ls -t "$RESULTS_DIR"/otp_comparison_*.json 2>/dev/null | head -1)
if [ -n "$LATEST_RESULT" ]; then
    echo "Latest result: $LATEST_RESULT"
    echo ""
    echo "Summary:"
    echo "--------"
    if command -v jq >/dev/null 2>&1; then
        jq '.summary' "$LATEST_RESULT" 2>/dev/null || echo "Unable to parse summary"
    else
        echo "Install jq for formatted output"
    fi
fi
