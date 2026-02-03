#!/usr/bin/env bash
# Automated Test Runner - Blocks on failures
# Usage: ./tools/test-runner.sh
# Exit codes: 0 = success, 1 = failure
#
# Environment:
#   ERLMCP_PROFILE  Profile to use (dev|test|staging|prod), defaults to 'test'

# ==============================================================================
# DOCKER-ONLY CONSTITUTION: Host execution FORBIDDEN
# ==============================================================================
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../scripts/dev/docker_guard.sh"

set -euo pipefail

# ==============================================================================
# Profile Configuration
# ==============================================================================

# Set default profile for test runs (safer for testing)
ERLMCP_PROFILE="${ERLMCP_PROFILE:-test}"

# Validate profile (with graceful fallback to test)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VALIDATE_SCRIPT="$SCRIPT_DIR/../scripts/validate_profile.sh"

if [ -f "$VALIDATE_SCRIPT" ]; then
    if ! "$VALIDATE_SCRIPT" "$ERLMCP_PROFILE" 2>/dev/null; then
        echo "WARNING: Invalid profile '$ERLMCP_PROFILE', falling back to 'test'"
        ERLMCP_PROFILE=test
    fi
else
    echo "WARNING: validate_profile.sh not found, using profile: $ERLMCP_PROFILE"
fi

export ERLMCP_PROFILE

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
PASS_THRESHOLD=90
RESULTS_DIR="_build/test/results"
RESULTS_JSON="${RESULTS_DIR}/test_results.json"

echo "========================================"
echo "ErlMCP Test Runner"
echo "========================================"
echo "Profile: $ERLMCP_PROFILE"
echo ""

# Create results directory
mkdir -p "${RESULTS_DIR}"

# Initialize JSON results
cat > "${RESULTS_JSON}" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "test_type": "eunit_and_ct",
  "suites": []
}
EOF

# Track totals
TOTAL_TESTS=0
TOTAL_PASSED=0
TOTAL_FAILED=0
TOTAL_SKIPPED=0

echo "Running EUnit tests..."
echo "----------------------------------------"

# Run EUnit tests
EUNIT_OUTPUT="${RESULTS_DIR}/eunit_output.txt"
if TERM=dumb rebar3 eunit --verbose 2>&1 | tee "${EUNIT_OUTPUT}"; then
    EUNIT_STATUS="passed"
else
    EUNIT_STATUS="failed"
fi

# Parse EUnit results
EUNIT_TESTS=$(grep -c "Test passed" "${EUNIT_OUTPUT}" || echo "0")
EUNIT_FAILED=$(grep -c "Test failed" "${EUNIT_OUTPUT}" || echo "0")
EUNIT_SKIPPED=$(grep -c "Test skipped" "${EUNIT_OUTPUT}" || echo "0")

# Add to totals
TOTAL_TESTS=$((TOTAL_TESTS + EUNIT_TESTS + EUNIT_FAILED + EUNIT_SKIPPED))
TOTAL_PASSED=$((TOTAL_PASSED + EUNIT_TESTS))
TOTAL_FAILED=$((TOTAL_FAILED + EUNIT_FAILED))
TOTAL_SKIPPED=$((TOTAL_SKIPPED + EUNIT_SKIPPED))

echo ""
echo "EUnit Results:"
echo "  Passed:  ${EUNIT_TESTS}"
echo "  Failed:  ${EUNIT_FAILED}"
echo "  Skipped: ${EUNIT_SKIPPED}"
echo ""

# Run Common Test suites
echo "Running Common Test suites..."
echo "----------------------------------------"

CT_OUTPUT="${RESULTS_DIR}/ct_output.txt"
CT_SUITES=$(find test -name "*_SUITE.erl" 2>/dev/null | wc -l)

if [ "${CT_SUITES}" -gt 0 ]; then
    if TERM=dumb rebar3 ct --verbose 2>&1 | tee "${CT_OUTPUT}"; then
        CT_STATUS="passed"
    else
        CT_STATUS="failed"
    fi

    # Parse CT results
    CT_TESTS=$(grep -c "PASSED" "${CT_OUTPUT}" || echo "0")
    CT_FAILED=$(grep -c "FAILED" "${CT_OUTPUT}" || echo "0")
    CT_SKIPPED=$(grep -c "SKIPPED" "${CT_OUTPUT}" || echo "0")

    # Add to totals
    TOTAL_TESTS=$((TOTAL_TESTS + CT_TESTS + CT_FAILED + CT_SKIPPED))
    TOTAL_PASSED=$((TOTAL_PASSED + CT_TESTS))
    TOTAL_FAILED=$((TOTAL_FAILED + CT_FAILED))
    TOTAL_SKIPPED=$((TOTAL_SKIPPED + CT_SKIPPED))

    echo ""
    echo "Common Test Results:"
    echo "  Passed:  ${CT_TESTS}"
    echo "  Failed:  ${CT_FAILED}"
    echo "  Skipped: ${CT_SKIPPED}"
    echo ""
else
    echo "No Common Test suites found."
    echo ""
    CT_STATUS="skipped"
    CT_TESTS=0
    CT_FAILED=0
    CT_SKIPPED=0
fi

# Calculate pass rate
if [ "${TOTAL_TESTS}" -gt 0 ]; then
    PASS_RATE=$((100 * TOTAL_PASSED / TOTAL_TESTS))
else
    PASS_RATE=0
fi

# Update JSON results
cat > "${RESULTS_JSON}" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "test_type": "eunit_and_ct",
  "summary": {
    "total_tests": ${TOTAL_TESTS},
    "passed": ${TOTAL_PASSED},
    "failed": ${TOTAL_FAILED},
    "skipped": ${TOTAL_SKIPPED},
    "pass_rate": ${PASS_RATE}
  },
  "eunit": {
    "status": "${EUNIT_STATUS}",
    "passed": ${EUNIT_TESTS},
    "failed": ${EUNIT_FAILED},
    "skipped": ${EUNIT_SKIPPED}
  },
  "ct": {
    "status": "${CT_STATUS}",
    "passed": ${CT_TESTS},
    "failed": ${CT_FAILED},
    "skipped": ${CT_SKIPPED}
  }
}
EOF

# Print summary
echo "========================================"
echo "TEST SUMMARY"
echo "========================================"
echo "Total Tests:  ${TOTAL_TESTS}"
echo "Passed:       ${TOTAL_PASSED}"
echo "Failed:       ${TOTAL_FAILED}"
echo "Skipped:      ${TOTAL_SKIPPED}"
echo "Pass Rate:    ${PASS_RATE}%"
echo "Threshold:    ${PASS_THRESHOLD}%"
echo ""

# Show detailed failures if any
if [ "${TOTAL_FAILED}" -gt 0 ]; then
    echo -e "${RED}========================================"
    echo "FAILURE DETAILS"
    echo "========================================${NC}"
    echo ""

    # Extract EUnit failures
    if [ "${EUNIT_FAILED}" -gt 0 ]; then
        echo "EUnit Failures:"
        grep -A 5 "Test failed" "${EUNIT_OUTPUT}" || true
        echo ""
    fi

    # Extract CT failures
    if [ "${CT_FAILED}" -gt 0 ]; then
        echo "Common Test Failures:"
        grep -A 5 "FAILED" "${CT_OUTPUT}" || true
        echo ""
    fi
fi

# Determine exit status
if [ "${PASS_RATE}" -lt "${PASS_THRESHOLD}" ]; then
    echo -e "${RED}❌ FAILURE: Pass rate ${PASS_RATE}% is below threshold ${PASS_THRESHOLD}%${NC}"
    echo ""
    echo "Results saved to: ${RESULTS_JSON}"
    exit 1
elif [ "${TOTAL_FAILED}" -gt 0 ]; then
    echo -e "${RED}❌ FAILURE: ${TOTAL_FAILED} test(s) failed${NC}"
    echo ""
    echo "Results saved to: ${RESULTS_JSON}"
    exit 1
else
    echo -e "${GREEN}✅ SUCCESS: All tests passed (${PASS_RATE}%)${NC}"
    echo ""
    echo "Results saved to: ${RESULTS_JSON}"
    exit 0
fi
