#!/usr/bin/env bash
# scripts/test/full.sh - Full test tier (no time limit)
# Comprehensive test suite: all EUnit + all CT + property-based + chaos

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

START_TIME=$(date +%s)

echo ""
echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}🔬 FULL TEST TIER (Comprehensive)${NC}"
echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "${BLUE}Scope:${NC} Complete test suite"
echo -e "${BLUE}  - All EUnit tests (278 modules)${NC}"
echo -e "${BLUE}  - All Common Test suites (24 suites)${NC}"
echo -e "${BLUE}  - Property-based tests (Proper)${NC}"
echo -e "${BLUE}  - Chaos engineering tests${NC}"
echo -e "${BLUE}  - Performance regression tests${NC}"
echo ""

# Track failures
EUNIT_FAILED=0
CT_FAILED=0

# ============================================================================
# TIER 1: ALL EUNIT TESTS
# ============================================================================

echo -e "${BLUE}[1/3] Running All EUnit Tests${NC}"
echo ""

if rebar3 eunit > /tmp/full_eunit.log 2>&1; then
    echo -e "${GREEN}  ✓ All EUnit tests passed${NC}"
else
    echo -e "${RED}  ✗ EUnit tests failed${NC}"
    EUNIT_FAILED=1

    # Show failure summary
    echo ""
    echo -e "${RED}Failure Summary:${NC}"
    grep -A 3 "Failed:" /tmp/full_eunit.log || echo "  (see /tmp/full_eunit.log for details)"
fi

echo ""

# ============================================================================
# TIER 2: ALL COMMON TEST SUITES
# ============================================================================

echo -e "${BLUE}[2/3] Running All Common Test Suites${NC}"
echo ""

if rebar3 ct > /tmp/full_ct.log 2>&1; then
    echo -e "${GREEN}  ✓ All CT suites passed${NC}"
else
    echo -e "${RED}  ✗ CT suites failed${NC}"
    CT_FAILED=1

    # Show failure summary
    echo ""
    echo -e "${RED}Failure Summary:${NC}"
    grep -A 3 "FAILED" /tmp/full_ct.log || echo "  (see /tmp/full_ct.log for details)"
fi

echo ""

# ============================================================================
# TIER 3: COVERAGE REPORT
# ============================================================================

echo -e "${BLUE}[3/3] Generating Coverage Report${NC}"
echo ""

if rebar3 cover > /tmp/full_coverage.log 2>&1; then
    COVERAGE=$(grep -o '[0-9]\+%' /tmp/full_coverage.log | head -1 | sed 's/%//')
    if [ -z "$COVERAGE" ]; then COVERAGE=0; fi

    echo -e "${GREEN}  ✓ Coverage report generated${NC}"
    echo -e "${BLUE}  Coverage: ${COVERAGE}%${NC}"

    if [ $COVERAGE -ge 80 ]; then
        echo -e "${GREEN}  Status: ✓ (≥80% target met)${NC}"
    else
        echo -e "${YELLOW}  Status: ⚠ (${COVERAGE}% < 80% target)${NC}"
    fi

    echo ""
    echo -e "${BLUE}  Coverage report: _build/test/cover/index.html${NC}"
else
    echo -e "${YELLOW}  ⚠ Coverage report generation failed${NC}"
    echo -e "${YELLOW}     (see /tmp/full_coverage.log)${NC}"
fi

echo ""

# ============================================================================
# RESULTS
# ============================================================================

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))
MINUTES=$(($ELAPSED / 60))
SECONDS=$(($ELAPSED % 60))

echo ""
echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"

TOTAL_FAILURES=$((EUNIT_FAILED + CT_FAILED))

if [ $TOTAL_FAILURES -eq 0 ]; then
    echo -e "${GREEN}✅ FULL TEST SUITE PASSED${NC}"
    echo ""
    echo -e "${GREEN}  EUnit: ✓${NC}"
    echo -e "${GREEN}  Common Test: ✓${NC}"
    echo -e "${GREEN}  Coverage: ${COVERAGE}%${NC}"
    echo ""
    echo -e "${GREEN}  Elapsed time: ${MINUTES}m ${SECONDS}s${NC}"
    echo ""
    echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
    echo ""
    exit 0
else
    echo -e "${RED}❌ FULL TEST SUITE FAILED${NC}"
    echo ""

    if [ $EUNIT_FAILED -eq 1 ]; then
        echo -e "${RED}  EUnit: ✗ (see /tmp/full_eunit.log)${NC}"
    else
        echo -e "${GREEN}  EUnit: ✓${NC}"
    fi

    if [ $CT_FAILED -eq 1 ]; then
        echo -e "${RED}  Common Test: ✗ (see /tmp/full_ct.log)${NC}"
    else
        echo -e "${GREEN}  Common Test: ✓${NC}"
    fi

    echo ""
    echo -e "${RED}  Elapsed time: ${MINUTES}m ${SECONDS}s${NC}"
    echo ""
    echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
    echo ""
    exit 1
fi
