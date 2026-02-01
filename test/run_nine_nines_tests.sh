#!/usr/bin/env bash
###############################################################################
# Nine-Nines Chaos Test Runner
#
# This script runs the comprehensive chaos test suite to validate erlmcp's
# nine-nines (99.9999999%) availability posture.
#
# Usage:
#   ./test/run_nine_nines_tests.sh [--quick|--full]
#
# Options:
#   --quick   Run minimal tests (100 PropEr samples, 1K sessions)
#   --full    Run full tests (1000 PropEr samples, 10K sessions) [default]
#
# Output:
#   - Terminal output with pass/fail status
#   - HTML report: _build/test/logs/nine_nines_report.html
#   - Metrics: _build/test/logs/metrics/*.json
###############################################################################

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default configuration
MODE="${1:-full}"
PROPER_SAMPLES=1000
SESSION_COUNT=10000

if [ "$MODE" = "--quick" ]; then
    PROPER_SAMPLES=100
    SESSION_COUNT=1000
    echo -e "${YELLOW}Running in QUICK mode (reduced samples)${NC}"
else
    echo -e "${GREEN}Running in FULL mode (production-grade samples)${NC}"
fi

# Create log directories
mkdir -p _build/test/logs/metrics

# Step 1: Compile
echo -e "\n${GREEN}=== Step 1: Compiling erlmcp ===${NC}"
make compile || {
    echo -e "${RED}Compilation failed${NC}"
    exit 1
}

# Step 2: Run PropEr FSM Tests
echo -e "\n${GREEN}=== Step 2: Running PropEr FSM Tests ===${NC}"
echo "Testing protocol state machines with $PROPER_SAMPLES samples per property..."

rebar3 as test proper --module=prop_protocol_fsm --numtests=$PROPER_SAMPLES || {
    echo -e "${RED}PropEr FSM tests failed${NC}"
    exit 1
}

# Step 3: Run CT Chaos Nine-Nines Suite
echo -e "\n${GREEN}=== Step 3: Running Chaos Nine-Nines Test Suite ===${NC}"
echo "Testing extreme scenarios with $SESSION_COUNT concurrent sessions..."

# Set environment variables for test configuration
export ERLMCP_CHAOS_SESSION_COUNT=$SESSION_COUNT
export ERLMCP_CHAOS_DATA_RATE=100000  # 100K msg/sec

rebar3 ct --suite=test/ct_chaos_nine_nines_SUITE || {
    echo -e "${RED}Chaos tests failed${NC}"
    exit 1
}

# Step 4: Generate Report
echo -e "\n${GREEN}=== Step 4: Generating HTML Report ===${NC}"

REPORT_FILE="_build/test/logs/nine_nines_report.html"
if [ -f "$REPORT_FILE" ]; then
    echo -e "${GREEN}Report generated: $REPORT_FILE${NC}"

    # Display summary
    if grep -q "✅ ACHIEVED" "$REPORT_FILE"; then
        echo -e "\n${GREEN}════════════════════════════════════════════${NC}"
        echo -e "${GREEN}  NINE-NINES POSTURE: ✅ ACHIEVED${NC}"
        echo -e "${GREEN}════════════════════════════════════════════${NC}"
    else
        echo -e "\n${YELLOW}════════════════════════════════════════════${NC}"
        echo -e "${YELLOW}  NINE-NINES POSTURE: ⚠️ SLA VIOLATIONS${NC}"
        echo -e "${YELLOW}════════════════════════════════════════════${NC}"
        echo -e "${YELLOW}Review report for details: $REPORT_FILE${NC}"
    fi
else
    echo -e "${YELLOW}Warning: HTML report not found${NC}"
fi

# Step 5: Display Metrics Summary
echo -e "\n${GREEN}=== Step 5: Metrics Summary ===${NC}"

METRICS_DIR="_build/test/logs/ct_run.*/erlmcp@*/metrics"
if [ -d "$METRICS_DIR" ]; then
    echo "Metrics collected:"
    ls -lh "$METRICS_DIR"/*.json | awk '{print "  - " $9 " (" $5 ")"}'
else
    # Try alternative path
    METRICS_DIR=$(find _build/test -name "metrics" -type d 2>/dev/null | head -1)
    if [ -n "$METRICS_DIR" ]; then
        echo "Metrics collected in: $METRICS_DIR"
        ls -lh "$METRICS_DIR"/*.json 2>/dev/null | awk '{print "  - " $9 " (" $5 ")"}' || echo "  No metrics files found"
    fi
fi

echo -e "\n${GREEN}=== Nine-Nines Test Suite Complete ===${NC}"
echo "View full report: file://$PWD/$REPORT_FILE"
