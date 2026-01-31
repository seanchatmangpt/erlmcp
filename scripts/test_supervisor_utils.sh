#!/usr/bin/env bash
###############################################################################
# Test Script for erlmcp_supervisor_utils
#
# Runs compilation, tests, and quality gates for the supervisor utilities.
#
# Usage:
#   ./scripts/test_supervisor_utils.sh
#
###############################################################################

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_DIR"

echo "======================================================================"
echo "Testing erlmcp_supervisor_utils"
echo "======================================================================"
echo

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counter for results
PASSED=0
FAILED=0

# Function to report test result
report_result() {
    local test_name="$1"
    local result="$2"

    if [ "$result" -eq 0 ]; then
        echo -e "${GREEN}✓${NC} $test_name"
        ((PASSED++))
    else
        echo -e "${RED}✗${NC} $test_name"
        ((FAILED++))
    fi
}

echo "----------------------------------------------------------------------"
echo "Step 1: Compilation"
echo "----------------------------------------------------------------------"

if TERM=dumb rebar3 compile 2>&1 | tee /tmp/compile.log | tail -20; then
    report_result "Compilation" 0
else
    report_result "Compilation" 1
    echo "Compilation failed. See /tmp/compile.log"
    exit 1
fi

echo
echo "----------------------------------------------------------------------"
echo "Step 2: Unit Tests (EUnit)"
echo "----------------------------------------------------------------------"

if rebar3 eunit --module=erlmcp_supervisor_utils_tests 2>&1 | tee /tmp/eunit.log; then
    report_result "EUnit Tests" 0
else
    report_result "EUnit Tests" 1
fi

echo
echo "----------------------------------------------------------------------"
echo "Step 3: Type Checking (Dialyzer)"
echo "----------------------------------------------------------------------"

if rebar3 dialyzer 2>&1 | tee /tmp/dialyzer.log | grep -E "(Warnings|warnings):" ; then
    echo -e "${YELLOW}⚠${NC} Dialyzer warnings found"
    report_result "Dialyzer (no warnings)" 1
else
    report_result "Dialyzer (no warnings)" 0
fi

echo
echo "----------------------------------------------------------------------"
echo "Step 4: Cross-Reference Analysis (Xref)"
echo "----------------------------------------------------------------------"

if rebar3 xref 2>&1 | tee /tmp/xref.log | grep -E "(Warning|Error):" ; then
    echo -e "${YELLOW}⚠${NC} Xref issues found"
    report_result "Xref (no issues)" 1
else
    report_result "Xref (no issues)" 0
fi

echo
echo "----------------------------------------------------------------------"
echo "Step 5: Code Coverage"
echo "----------------------------------------------------------------------"

if rebar3 cover 2>&1 | tee /tmp/coverage.log; then
    # Extract coverage percentage
    COVERAGE=$(grep -oP "total\s+:\s+\K\d+" /tmp/coverage.log || echo "0")
    echo "Coverage: ${COVERAGE}%"

    if [ "$COVERAGE" -ge 80 ]; then
        report_result "Coverage ≥80%" 0
    else
        echo -e "${YELLOW}⚠${NC} Coverage below 80%: ${COVERAGE}%"
        report_result "Coverage ≥80%" 1
    fi
else
    report_result "Coverage measurement" 1
fi

echo
echo "----------------------------------------------------------------------"
echo "Step 6: Format Check"
echo "----------------------------------------------------------------------"

if rebar3 format --verify 2>&1 | tee /tmp/format.log; then
    report_result "Format check" 0
else
    echo -e "${YELLOW}⚠${NC} Format check failed - run: rebar3 format"
    report_result "Format check" 1
fi

echo
echo "======================================================================"
echo "Test Summary"
echo "======================================================================"
echo -e "${GREEN}Passed:${NC} $PASSED"
echo -e "${RED}Failed:${NC} $FAILED"
echo

if [ "$FAILED" -eq 0 ]; then
    echo -e "${GREEN}✓ ALL TESTS PASSED${NC}"
    exit 0
else
    echo -e "${RED}✗ SOME TESTS FAILED${NC}"
    exit 1
fi
