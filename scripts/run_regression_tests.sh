#!/bin/bash
# run_regression_tests.sh - Run OTP 28 performance regression tests
#
# Usage:
#   ./scripts/run_regression_tests.sh [baseline_file]
#
# Examples:
#   ./scripts/run_regression_tests.sh
#   ./scripts/run_regression_tests.sh bench/baseline/otp28_baseline.json

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default baseline
BASELINE_FILE="${1:-bench/baseline/otp28_baseline.json}"

echo "=================================="
echo "OTP 28 Performance Regression Tests"
echo "=================================="
echo ""

# Check OTP version
OTP_VERSION=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), init:stop().')
echo "Erlang/OTP Version: $OTP_VERSION"
echo ""

# Check if baseline exists
if [ ! -f "$BASELINE_FILE" ]; then
    echo -e "${YELLOW}WARNING: Baseline file not found: $BASELINE_FILE${NC}"
    echo "Creating new baseline..."
    
    # Run baseline establishment
    erl -pa _build/default/lib/*/ebin \
        -noshell \
        -eval "erlmcp_bench_regression:establish_baseline(), init:stop()."
    
    echo -e "${GREEN}Baseline established${NC}"
    exit 0
fi

echo "Using baseline: $BASELINE_FILE"
echo ""

# Run regression tests
echo "Running regression tests..."
echo ""

if rebar3 ct --suite=erlmcp_regression_SUITE --var=baseline_path=$BASELINE_FILE; then
    echo ""
    echo -e "${GREEN}=================================="
    echo "All regression tests PASSED"
    echo "==================================${NC}"
    exit 0
else
    echo ""
    echo -e "${RED}=================================="
    echo "Regression tests FAILED"
    echo "==================================${NC}"
    echo ""
    echo "Check log/ct/index.html for detailed results"
    exit 1
fi
