#!/usr/bin/env bash
# Master Quality Checker - Runs all validation checks
# Usage: ./tools/quality-checker.sh
# Exit codes: 0 = all pass, 1 = any failure

set -euo pipefail

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
RESULTS_DIR="_build/test/quality"
QUALITY_REPORT="${RESULTS_DIR}/quality_report.txt"

echo "========================================"
echo "ErlMCP Quality Checker"
echo "========================================"
echo "Comprehensive validation suite"
echo ""

# Create results directory
mkdir -p "${RESULTS_DIR}"

# Track results
declare -a PASSED_CHECKS
declare -a FAILED_CHECKS
TOTAL_CHECKS=0
FAILURES=0

# Function to run check
run_check() {
    local check_name="$1"
    local check_command="$2"

    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    echo ""
    echo -e "${BLUE}========================================"
    echo "CHECK ${TOTAL_CHECKS}: ${check_name}"
    echo "========================================${NC}"
    echo ""

    if eval "${check_command}"; then
        PASSED_CHECKS+=("${check_name}")
        echo ""
        echo -e "${GREEN}✅ PASSED: ${check_name}${NC}"
        return 0
    else
        FAILED_CHECKS+=("${check_name}")
        FAILURES=$((FAILURES + 1))
        echo ""
        echo -e "${RED}❌ FAILED: ${check_name}${NC}"
        return 1
    fi
}

# Start timestamp
START_TIME=$(date +%s)

# 1. Compilation check
run_check "Compilation" "TERM=dumb rebar3 compile" || true

# 2. Unit tests
run_check "Unit Tests (EUnit + CT)" "./tools/test-runner.sh" || true

# 3. Coverage
run_check "Code Coverage" "./tools/coverage-checker.sh" || true

# 4. Dialyzer (type checking)
run_check "Dialyzer (Type Checking)" "rebar3 dialyzer" || true

# 5. Xref (cross-reference)
run_check "Xref (Cross-Reference)" "rebar3 xref" || true

# 6. Benchmarks
echo ""
echo -e "${YELLOW}⚠️  Benchmark validation is optional for quick checks${NC}"
echo "Run manually with: ./tools/benchmark-runner.sh"
echo ""

# Uncomment to enable benchmark validation:
# run_check "Benchmarks" "./tools/benchmark-runner.sh" || true

# End timestamp
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# Generate summary
echo ""
echo "========================================"
echo "QUALITY CHECK SUMMARY"
echo "========================================"
echo "Duration:      ${DURATION} seconds"
echo "Total Checks:  ${TOTAL_CHECKS}"
echo "Passed:        $((TOTAL_CHECKS - FAILURES))"
echo "Failed:        ${FAILURES}"
echo ""

# Generate detailed report
{
    echo "ErlMCP Quality Report"
    echo "Generated: $(date)"
    echo "Duration: ${DURATION} seconds"
    echo ""
    echo "========================================"
    echo "SUMMARY"
    echo "========================================"
    echo "Total Checks: ${TOTAL_CHECKS}"
    echo "Passed: $((TOTAL_CHECKS - FAILURES))"
    echo "Failed: ${FAILURES}"
    echo ""

    if [ "${FAILURES}" -gt 0 ]; then
        echo "========================================"
        echo "FAILED CHECKS"
        echo "========================================"
        for check in "${FAILED_CHECKS[@]}"; do
            echo "  ❌ ${check}"
        done
        echo ""
    fi

    echo "========================================"
    echo "PASSED CHECKS"
    echo "========================================"
    for check in "${PASSED_CHECKS[@]}"; do
        echo "  ✅ ${check}"
    done
    echo ""

    echo "========================================"
    echo "DETAILED RESULTS"
    echo "========================================"
    echo "1. Test Results: _build/test/results/test_results.json"
    echo "2. Coverage Report: _build/test/coverage/coverage_report.txt"
    echo "3. Coverage HTML: _build/test/cover/index.html"
    echo "4. Dialyzer Output: Check console output above"
    echo "5. Xref Output: Check console output above"
} > "${QUALITY_REPORT}"

# Print results
if [ "${FAILURES}" -gt 0 ]; then
    echo -e "${RED}========================================"
    echo "FAILED CHECKS"
    echo "========================================${NC}"
    echo ""
    for check in "${FAILED_CHECKS[@]}"; do
        echo -e "${RED}  ❌ ${check}${NC}"
    done
    echo ""
fi

echo "========================================"
echo "PASSED CHECKS"
echo "========================================"
echo ""
for check in "${PASSED_CHECKS[@]}"; do
    echo -e "${GREEN}  ✅ ${check}${NC}"
done
echo ""

# Print report location
echo "========================================"
echo "REPORTS GENERATED"
echo "========================================"
echo "Quality Report:  ${QUALITY_REPORT}"
echo "Test Results:    _build/test/results/test_results.json"
echo "Coverage Report: _build/test/coverage/coverage_report.txt"
echo "Coverage HTML:   file://${PWD}/_build/test/cover/index.html"
echo ""

# Exit with appropriate code
if [ "${FAILURES}" -gt 0 ]; then
    echo -e "${RED}❌ QUALITY CHECK FAILED: ${FAILURES} check(s) failed${NC}"
    echo ""
    echo "Review the reports above for detailed information."
    exit 1
else
    echo -e "${GREEN}✅ QUALITY CHECK PASSED: All checks successful${NC}"
    echo ""
    exit 0
fi
