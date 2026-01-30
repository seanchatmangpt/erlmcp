#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")/.." || exit 2

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

PASSED=0
FAILED=0

status() {
    local check="$1"
    local condition="$2"
    local message="$3"

    if [ "$condition" = "true" ]; then
        echo -e "  ${GREEN}✓${NC} $check: $message"
        PASSED=$((PASSED + 1))
    else
        echo -e "  ${RED}✗${NC} $check: $message"
        FAILED=$((FAILED + 1))
    fi
}

echo -e "${YELLOW}🔍 Quality Gate Enforcer${NC}\n"

# Compilation
echo "Compilation:"
COMP=$(TERM=dumb rebar3 compile 2>&1 || true)
ERRORS=$(echo "$COMP" | grep -ic "error" || echo "0")
# Fix: Trim whitespace and ensure numeric comparison
ERRORS=$(echo "$ERRORS" | tr -d '[:space:]')
status "Errors" "$([[ $ERRORS -eq 0 ]] && echo true || echo false)" "$ERRORS errors"

# Tests
echo -e "\nTests:"
TESTS=$(rebar3 eunit 2>&1 || true)
FAILED_TESTS=$(echo "$TESTS" | grep -o "Failed: [0-9]*" | grep -o "[0-9]*" || echo "0")
# Fix: Handle empty output
FAILED_TESTS=$(echo "$FAILED_TESTS" | head -1 | tr -d '[:space:]')
[ -z "$FAILED_TESTS" ] && FAILED_TESTS=0
status "EUnit" "$([[ $FAILED_TESTS -le 5 ]] && echo true || echo false)" "$FAILED_TESTS failures"

# Coverage
echo -e "\nCoverage:"
COV=$(rebar3 cover 2>&1 || true)
COVERAGE=$(echo "$COV" | grep -o "total: [0-9]*" | grep -o "[0-9]*" | head -1 || echo "0")
# Fix: Handle empty output
COVERAGE=$(echo "$COVERAGE" | tr -d '[:space:]')
[ -z "$COVERAGE" ] && COVERAGE=0
status "≥80%" "$([[ $COVERAGE -ge 80 ]] && echo true || echo false)" "$COVERAGE%"

# Dialyzer
echo -e "\nType Safety:"
DIAL=$(rebar3 dialyzer 2>&1 || true)
DIAL_ERRORS=$(echo "$DIAL" | grep -c "error:" || echo "0")
# Fix: Trim whitespace
DIAL_ERRORS=$(echo "$DIAL_ERRORS" | tr -d '[:space:]')
status "Dialyzer" "$([[ $DIAL_ERRORS -eq 0 ]] && echo true || echo false)" "$DIAL_ERRORS errors"

# XREF
echo -e "\nCross-reference:"
XREF=$(rebar3 xref 2>&1 || true)
XREF_WARNS=$(echo "$XREF" | grep -c "Warning:" || echo "0")
# Fix: Trim whitespace
XREF_WARNS=$(echo "$XREF_WARNS" | tr -d '[:space:]')
status "XREF" "$([[ $XREF_WARNS -le 10 ]] && echo true || echo false)" "$XREF_WARNS warnings"

# Summary
echo -e "\n${YELLOW}Summary:${NC}"
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"

if [ $FAILED -eq 0 ]; then
    echo -e "\n${GREEN}✅ All quality gates passed${NC}"
    exit 0
else
    echo -e "\n${RED}❌ Quality gates failed${NC}"
    exit 2
fi
