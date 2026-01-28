#!/bin/bash
#==============================================================================
# Quality Gates Validation Script
# Simulates CI/CD quality gate execution locally
#==============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

# Gate counters
GATES_PASSED=0
GATES_FAILED=0
GATES_WARNED=0

echo -e "${BLUE}╔══════════════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║           ERLMCP QUALITY GATES VALIDATION - LOCAL EXECUTION              ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "${BLUE}Project Root:${NC} $PROJECT_ROOT"
echo -e "${BLUE}Timestamp:${NC} $(date)"
echo ""

cd "$PROJECT_ROOT"

# ============================================================================
# GATE 1: COMPILATION
# ============================================================================
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}GATE 1: COMPILATION${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Requirement:${NC} Zero compilation errors"
echo -e "${BLUE}Blocking:${NC} YES"
echo ""

TERM=dumb rebar3 compile 2>&1 | tee /tmp/compile.log
COMPILE_EXIT=${PIPESTATUS[0]}

ERRORS=$(grep -c "Error:" /tmp/compile.log || echo 0)
WARNINGS=$(grep -c "Warning:" /tmp/compile.log || echo 0)

if [ $COMPILE_EXIT -eq 0 ]; then
    echo -e "${GREEN}✅ GATE 1 PASSED: Compilation successful${NC}"
    echo -e "   Errors: $ERRORS | Warnings: $WARNINGS"
    ((GATES_PASSED++))
else
    echo -e "${RED}❌ GATE 1 FAILED: Compilation errors detected${NC}"
    echo -e "   Errors: $ERRORS | Warnings: $WARNINGS"
    echo -e "${RED}   This BLOCKS merge in CI/CD${NC}"
    ((GATES_FAILED++))
fi
echo ""

# ============================================================================
# GATE 2: XREF (Cross-Reference Analysis)
# ============================================================================
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}GATE 2: XREF (Cross-Reference Analysis)${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Requirement:${NC} Zero undefined function calls"
echo -e "${BLUE}Blocking:${NC} YES"
echo ""

rebar3 xref 2>&1 | tee /tmp/xref.log
XREF_EXIT=${PIPESTATUS[0]}

UNDEFINED=$(grep -c "undefined function" /tmp/xref.log || echo 0)

if [ $XREF_EXIT -eq 0 ]; then
    echo -e "${GREEN}✅ GATE 2 PASSED: Xref clean${NC}"
    echo -e "   Undefined functions: $UNDEFINED"
    ((GATES_PASSED++))
else
    echo -e "${RED}❌ GATE 2 FAILED: Xref issues detected${NC}"
    echo -e "   Undefined functions: $UNDEFINED"
    echo -e "${RED}   This BLOCKS merge in CI/CD${NC}"
    ((GATES_FAILED++))
fi
echo ""

# ============================================================================
# GATE 3: DIALYZER (Type Checking)
# ============================================================================
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}GATE 3: DIALYZER (Type Checking)${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Requirement:${NC} Zero type errors"
echo -e "${BLUE}Blocking:${NC} YES"
echo ""

rebar3 dialyzer 2>&1 | tee /tmp/dialyzer.log
DIALYZER_EXIT=${PIPESTATUS[0]}

TYPE_WARNINGS=$(grep -c "Warning:" /tmp/dialyzer.log || echo 0)

if [ $DIALYZER_EXIT -eq 0 ]; then
    echo -e "${GREEN}✅ GATE 3 PASSED: Dialyzer clean${NC}"
    echo -e "   Type warnings: $TYPE_WARNINGS"
    ((GATES_PASSED++))
else
    echo -e "${RED}❌ GATE 3 FAILED: Dialyzer issues detected${NC}"
    echo -e "   Type warnings: $TYPE_WARNINGS"
    echo -e "${RED}   This BLOCKS merge in CI/CD${NC}"
    ((GATES_FAILED++))
fi
echo ""

# ============================================================================
# GATE 4: UNIT TESTS (EUnit)
# ============================================================================
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}GATE 4: UNIT TESTS (EUnit)${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Requirement:${NC} ≥90% pass rate"
echo -e "${BLUE}Blocking:${NC} YES"
echo ""

rebar3 as test do compile, eunit --cover 2>&1 | tee /tmp/eunit.log
EUNIT_EXIT=${PIPESTATUS[0]}

PASSED=$(grep -c "Test passed" /tmp/eunit.log || echo 0)
FAILED=$(grep -c "Test failed" /tmp/eunit.log || echo 0)
TOTAL=$((PASSED + FAILED))

if [ $TOTAL -gt 0 ]; then
    PASS_RATE=$(echo "scale=2; $PASSED * 100 / $TOTAL" | bc)
else
    PASS_RATE="0"
fi

if [ $EUNIT_EXIT -eq 0 ] && [ $(echo "$PASS_RATE >= 90" | bc) -eq 1 ]; then
    echo -e "${GREEN}✅ GATE 4 PASSED: Tests passed${NC}"
    echo -e "   Passed: $PASSED | Failed: $FAILED | Pass Rate: ${PASS_RATE}%"
    ((GATES_PASSED++))
else
    echo -e "${RED}❌ GATE 4 FAILED: Tests failed or pass rate < 90%${NC}"
    echo -e "   Passed: $PASSED | Failed: $FAILED | Pass Rate: ${PASS_RATE}%"
    echo -e "${RED}   This BLOCKS merge in CI/CD${NC}"
    ((GATES_FAILED++))
fi
echo ""

# ============================================================================
# GATE 5: CODE COVERAGE
# ============================================================================
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}GATE 5: CODE COVERAGE${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Requirement:${NC} ≥80% overall coverage"
echo -e "${BLUE}Blocking:${NC} YES"
echo ""

rebar3 cover --verbose

if [ -f scripts/check_coverage_threshold.sh ]; then
    chmod +x scripts/check_coverage_threshold.sh
    ./scripts/check_coverage_threshold.sh 80
    COVERAGE_EXIT=$?

    # Extract coverage percentage
    if [ -f _build/test/cover/cover.log ]; then
        COVERAGE=$(grep "total" _build/test/cover/cover.log | tail -1 | awk -F'|' '{print $3}' | tr -d ' %|' || echo "0")
    else
        COVERAGE="0"
    fi

    if [ $COVERAGE_EXIT -eq 0 ]; then
        echo -e "${GREEN}✅ GATE 5 PASSED: Coverage meets threshold${NC}"
        echo -e "   Overall Coverage: ${COVERAGE}% (≥80%)"
        ((GATES_PASSED++))
    else
        echo -e "${RED}❌ GATE 5 FAILED: Coverage below threshold${NC}"
        echo -e "   Overall Coverage: ${COVERAGE}% (< 80%)"
        echo -e "${RED}   This BLOCKS merge in CI/CD${NC}"
        ((GATES_FAILED++))
    fi
else
    echo -e "${YELLOW}⚠ GATE 5 SKIPPED: Coverage script not found${NC}"
    ((GATES_WARNED++))
fi
echo ""

# ============================================================================
# GATE 6: PERFORMANCE REGRESSION (Smoke Test)
# ============================================================================
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}GATE 6: PERFORMANCE REGRESSION (Smoke Test)${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Requirement:${NC} Benchmark executes successfully"
echo -e "${BLUE}Blocking:${NC} NO (warning only in local test)"
echo ""

echo "Running quick benchmark smoke test (core_ops_1k)..."
rebar3 shell --eval "
  application:ensure_all_started(erlmcp_core),
  application:ensure_all_started(erlmcp_transports),
  Result = erlmcp_bench_core_ops:run(<<\"core_ops_1k\">>),
  io:format(\"Benchmark result: ~p~n\", [Result]),
  init:stop().
" --name validate_test@localhost --setcookie validate_cookie 2>&1 | tee /tmp/benchmark.log
BENCH_EXIT=${PIPESTATUS[0]}

if [ $BENCH_EXIT -eq 0 ]; then
    echo -e "${GREEN}✅ GATE 6 PASSED: Benchmark executed successfully${NC}"
    echo -e "${YELLOW}   Note: Full regression check runs in PR workflow${NC}"
    ((GATES_PASSED++))
else
    echo -e "${YELLOW}⚠ GATE 6 WARNING: Benchmark execution issue${NC}"
    echo -e "${YELLOW}   This is non-blocking in local validation${NC}"
    ((GATES_WARNED++))
fi
echo ""

# ============================================================================
# GATE 7: INTEGRATION TESTS (CT) - Non-blocking
# ============================================================================
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}GATE 7: INTEGRATION TESTS (Common Test)${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Requirement:${NC} All suites pass"
echo -e "${BLUE}Blocking:${NC} NO (warning only)"
echo ""

rebar3 ct --dir=test/integration --cover 2>&1 | tee /tmp/ct.log
CT_EXIT=${PIPESTATUS[0]}

SUITES=$(grep -c "SUITE" /tmp/ct.log || echo 0)

if [ $CT_EXIT -eq 0 ]; then
    echo -e "${GREEN}✅ GATE 7 PASSED: Integration tests passed${NC}"
    echo -e "   Test suites: $SUITES"
    ((GATES_PASSED++))
else
    echo -e "${YELLOW}⚠ GATE 7 WARNING: Integration test issues${NC}"
    echo -e "   Test suites: $SUITES"
    echo -e "${YELLOW}   This is non-blocking${NC}"
    ((GATES_WARNED++))
fi
echo ""

# ============================================================================
# FINAL SUMMARY
# ============================================================================
echo -e "${BLUE}╔══════════════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║                        QUALITY GATES SUMMARY                             ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════════════════════════╝${NC}"
echo ""

TOTAL_GATES=$((GATES_PASSED + GATES_FAILED + GATES_WARNED))

echo -e "${BLUE}Total Gates:${NC} $TOTAL_GATES"
echo -e "${GREEN}Passed:${NC} $GATES_PASSED"
echo -e "${RED}Failed:${NC} $GATES_FAILED"
echo -e "${YELLOW}Warnings:${NC} $GATES_WARNED"
echo ""

echo -e "${BLUE}Gate Results:${NC}"
echo -e "  1. Compilation:      $([ $COMPILE_EXIT -eq 0 ] && echo -e '${GREEN}✅ PASS${NC}' || echo -e '${RED}❌ FAIL${NC}')"
echo -e "  2. Xref:             $([ $XREF_EXIT -eq 0 ] && echo -e '${GREEN}✅ PASS${NC}' || echo -e '${RED}❌ FAIL${NC}')"
echo -e "  3. Dialyzer:         $([ $DIALYZER_EXIT -eq 0 ] && echo -e '${GREEN}✅ PASS${NC}' || echo -e '${RED}❌ FAIL${NC}')"
echo -e "  4. Unit Tests:       $([ $EUNIT_EXIT -eq 0 ] && echo -e '${GREEN}✅ PASS${NC}' || echo -e '${RED}❌ FAIL${NC}')"
echo -e "  5. Coverage:         $([ ${COVERAGE_EXIT:-1} -eq 0 ] && echo -e '${GREEN}✅ PASS${NC}' || echo -e '${RED}❌ FAIL${NC}')"
echo -e "  6. Performance:      $([ $BENCH_EXIT -eq 0 ] && echo -e '${GREEN}✅ PASS${NC}' || echo -e '${YELLOW}⚠ WARN${NC}')"
echo -e "  7. Integration:      $([ $CT_EXIT -eq 0 ] && echo -e '${GREEN}✅ PASS${NC}' || echo -e '${YELLOW}⚠ WARN${NC}')"
echo ""

# Check if all blocking gates passed
if [ $GATES_FAILED -eq 0 ]; then
    echo -e "${GREEN}╔══════════════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║                  ✅ ALL BLOCKING GATES PASSED ✅                          ║${NC}"
    echo -e "${GREEN}╚══════════════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo -e "${GREEN}Your code is ready to push to CI/CD.${NC}"
    echo -e "${GREEN}All blocking quality gates will PASS in GitHub Actions.${NC}"
    echo ""

    if [ $GATES_WARNED -gt 0 ]; then
        echo -e "${YELLOW}Note: $GATES_WARNED non-blocking warnings detected.${NC}"
        echo -e "${YELLOW}Consider fixing warnings before pushing.${NC}"
        echo ""
    fi

    exit 0
else
    echo -e "${RED}╔══════════════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${RED}║                  ❌ BLOCKING GATES FAILED ❌                              ║${NC}"
    echo -e "${RED}╚══════════════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo -e "${RED}Your code will be BLOCKED from merging in CI/CD.${NC}"
    echo -e "${RED}Fix the failing gates before pushing.${NC}"
    echo ""
    echo -e "${YELLOW}Failed Gates:${NC}"
    [ $COMPILE_EXIT -ne 0 ] && echo -e "  ${RED}❌ Compilation${NC}"
    [ $XREF_EXIT -ne 0 ] && echo -e "  ${RED}❌ Xref${NC}"
    [ $DIALYZER_EXIT -ne 0 ] && echo -e "  ${RED}❌ Dialyzer${NC}"
    [ $EUNIT_EXIT -ne 0 ] && echo -e "  ${RED}❌ Unit Tests${NC}"
    [ ${COVERAGE_EXIT:-1} -ne 0 ] && echo -e "  ${RED}❌ Coverage${NC}"
    echo ""
    echo -e "${YELLOW}How to fix:${NC}"
    echo -e "  1. Review error messages above"
    echo -e "  2. Fix issues in code"
    echo -e "  3. Re-run: ${BLUE}./scripts/validate_quality_gates.sh${NC}"
    echo -e "  4. Once green, push to GitHub"
    echo ""

    exit 1
fi
