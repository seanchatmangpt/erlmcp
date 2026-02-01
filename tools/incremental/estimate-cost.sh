#!/usr/bin/env bash
# tools/incremental/estimate-cost.sh
# Purpose: Estimate validation cost and time
# Usage: ./estimate-cost.sh

set -euo pipefail

CACHE_DIR=".erlmcp/cache"
GATE_PLAN="$CACHE_DIR/gate-plan.json"

# Cost model (AWS c5.xlarge: $0.17/hour)
HOURLY_RATE=0.17
COMPILE_SECONDS=15
EUNIT_PER_SUITE_SECONDS=2
CT_PER_SUITE_SECONDS=5
COVERAGE_SECONDS=10
DIALYZER_SECONDS=120
XREF_SECONDS=5
BENCHMARKS_SECONDS=180

# Full suite baseline
FULL_SUITE_SECONDS=480
FULL_SUITE_COST=0.40

echo "Estimating validation cost..."
echo ""

if [[ ! -f "$GATE_PLAN" ]]; then
    echo "❌ Gate plan not found: $GATE_PLAN"
    echo "Run ./select-gates.sh first"
    exit 1
fi

if ! command -v jq &> /dev/null; then
    echo "❌ jq not found, please install jq"
    exit 1
fi

if ! command -v bc &> /dev/null; then
    echo "⚠ bc not found, using basic arithmetic (less accurate)"
    USE_BC=false
else
    USE_BC=true
fi

calculate_cost() {
    local seconds=$1
    if [[ "$USE_BC" == "true" ]]; then
        echo "scale=5; $seconds / 3600 * $HOURLY_RATE" | bc
    else
        # Fallback to integer arithmetic
        echo $(( seconds * 17 / 36000 ))
    fi
}

# Initialize totals
TOTAL_TIME=0
TOTAL_COST=0.0

# Compile (always required)
if [[ $(jq -r '.compile' "$GATE_PLAN") == "true" ]]; then
    COMPILE_COST=$(calculate_cost $COMPILE_SECONDS)
    TOTAL_TIME=$((TOTAL_TIME + COMPILE_SECONDS))
    if [[ "$USE_BC" == "true" ]]; then
        TOTAL_COST=$(echo "$TOTAL_COST + $COMPILE_COST" | bc -l)
    fi
    echo "  Compile: ${COMPILE_SECONDS}s (\$$COMPILE_COST)"
fi

# EUnit
if [[ $(jq -r '.eunit' "$GATE_PLAN") == "true" ]]; then
    # Estimate 20 suites (will be more accurate with actual test count)
    TEST_COUNT=20
    EUNIT_SECONDS=$((TEST_COUNT * EUNIT_PER_SUITE_SECONDS))
    EUNIT_COST=$(calculate_cost $EUNIT_SECONDS)
    TOTAL_TIME=$((TOTAL_TIME + EUNIT_SECONDS))
    if [[ "$USE_BC" == "true" ]]; then
        TOTAL_COST=$(echo "$TOTAL_COST + $EUNIT_COST" | bc -l)
    fi
    echo "  EUnit (~$TEST_COUNT suites): ${EUNIT_SECONDS}s (\$$EUNIT_COST)"
fi

# CT
if [[ $(jq -r '.ct' "$GATE_PLAN") == "true" ]]; then
    CT_COUNT=5
    CT_SECONDS=$((CT_COUNT * CT_PER_SUITE_SECONDS))
    CT_COST=$(calculate_cost $CT_SECONDS)
    TOTAL_TIME=$((TOTAL_TIME + CT_SECONDS))
    if [[ "$USE_BC" == "true" ]]; then
        TOTAL_COST=$(echo "$TOTAL_COST + $CT_COST" | bc -l)
    fi
    echo "  CT (~$CT_COUNT suites): ${CT_SECONDS}s (\$$CT_COST)"
fi

# Coverage
if [[ $(jq -r '.coverage' "$GATE_PLAN") == "true" ]]; then
    COVERAGE_COST=$(calculate_cost $COVERAGE_SECONDS)
    TOTAL_TIME=$((TOTAL_TIME + COVERAGE_SECONDS))
    if [[ "$USE_BC" == "true" ]]; then
        TOTAL_COST=$(echo "$TOTAL_COST + $COVERAGE_COST" | bc -l)
    fi
    echo "  Coverage: ${COVERAGE_SECONDS}s (\$$COVERAGE_COST)"
fi

# Dialyzer
if [[ $(jq -r '.dialyzer' "$GATE_PLAN") == "true" ]]; then
    DIALYZER_COST=$(calculate_cost $DIALYZER_SECONDS)
    TOTAL_TIME=$((TOTAL_TIME + DIALYZER_SECONDS))
    if [[ "$USE_BC" == "true" ]]; then
        TOTAL_COST=$(echo "$TOTAL_COST + $DIALYZER_COST" | bc -l)
    fi
    echo "  Dialyzer: ${DIALYZER_SECONDS}s (\$$DIALYZER_COST)"
fi

# Xref
if [[ $(jq -r '.xref' "$GATE_PLAN") == "true" ]]; then
    XREF_COST=$(calculate_cost $XREF_SECONDS)
    TOTAL_TIME=$((TOTAL_TIME + XREF_SECONDS))
    if [[ "$USE_BC" == "true" ]]; then
        TOTAL_COST=$(echo "$TOTAL_COST + $XREF_COST" | bc -l)
    fi
    echo "  Xref: ${XREF_SECONDS}s (\$$XREF_COST)"
fi

# Benchmarks
if [[ $(jq -r '.benchmarks' "$GATE_PLAN") == "true" ]]; then
    BENCH_COST=$(calculate_cost $BENCHMARKS_SECONDS)
    TOTAL_TIME=$((TOTAL_TIME + BENCHMARKS_SECONDS))
    if [[ "$USE_BC" == "true" ]]; then
        TOTAL_COST=$(echo "$TOTAL_COST + $BENCH_COST" | bc -l)
    fi
    echo "  Benchmarks: ${BENCHMARKS_SECONDS}s (\$$BENCH_COST)"
fi

# Calculate time in minutes
TOTAL_MINUTES=$((TOTAL_TIME / 60))
TOTAL_SECONDS=$((TOTAL_TIME % 60))

echo ""
echo "Total estimated time: ${TOTAL_TIME}s (${TOTAL_MINUTES}m ${TOTAL_SECONDS}s)"
if [[ "$USE_BC" == "true" ]]; then
    echo "Total estimated cost: \$$TOTAL_COST"
else
    echo "Total estimated cost: ~\$0.$(printf '%02d' $((TOTAL_TIME * 17 / 360)))"
fi

# Compare with full suite
SAVINGS_TIME=$((FULL_SUITE_SECONDS - TOTAL_TIME))
if [[ "$USE_BC" == "true" ]]; then
    SAVINGS_PCT=$(echo "scale=1; 100 * $SAVINGS_TIME / $FULL_SUITE_SECONDS" | bc)
    SAVINGS_COST=$(echo "$FULL_SUITE_COST - $TOTAL_COST" | bc -l)
else
    SAVINGS_PCT=$((100 * SAVINGS_TIME / FULL_SUITE_SECONDS))
    SAVINGS_COST="~0.$(printf '%02d' $(((FULL_SUITE_COST * 100 - TOTAL_COST * 100) / 1)))"
fi

echo ""
echo "vs. full suite:"
echo "  Time saved: ${SAVINGS_TIME}s (${SAVINGS_PCT}%)"
if [[ "$USE_BC" == "true" ]]; then
    printf "  Cost saved: \$%.5f\n" "$SAVINGS_COST"
else
    echo "  Cost saved: \$$SAVINGS_COST"
fi
