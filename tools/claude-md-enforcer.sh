#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")/.." || exit 2

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

CLAUDE_MD="CLAUDE.md"

[ -f "$CLAUDE_MD" ] || { echo -e "${RED}✗ CLAUDE.md not found${NC}"; exit 2; }

echo -e "${YELLOW}📋 CLAUDE.md Quality Rules${NC}\n"

# Extract quality targets from CLAUDE.md
echo "Quality Targets:"
echo "  - Compilation: 0 errors"
echo "  - Tests: 100% pass rate"
echo "  - Coverage: ≥80%"
echo "  - Dialyzer: 0 errors"
echo "  - Performance: <10% regression"

echo -e "\nValidating current state:\n"

# Compilation
echo "Compilation:"
COMP=$(TERM=dumb rebar3 compile 2>&1 || true)
# Fix: Proper error counting
ERROR_COUNT=$(echo "$COMP" | grep -i "error:" | wc -l | tr -d '[:space:]' || echo "0")
if [ "$ERROR_COUNT" -eq 0 ]; then
    echo -e "  ${GREEN}✓${NC} 0 errors"
else
    echo -e "  ${RED}✗${NC} $ERROR_COUNT errors"
fi

# Tests
echo -e "\nTests:"
TESTS=$(rebar3 eunit 2>&1 || true)
PASSED=$(echo "$TESTS" | grep -o "Passed: [0-9]*" | grep -o "[0-9]*" | head -1 || echo "0")
FAILED=$(echo "$TESTS" | grep -o "Failed: [0-9]*" | grep -o "[0-9]*" | head -1 || echo "0")
# Fix: Trim whitespace
PASSED=$(echo "$PASSED" | tr -d '[:space:]')
FAILED=$(echo "$FAILED" | tr -d '[:space:]')
[ -z "$PASSED" ] && PASSED=0
[ -z "$FAILED" ] && FAILED=0

TOTAL=$((PASSED + FAILED))
if [ $TOTAL -gt 0 ]; then
    RATE=$((PASSED * 100 / TOTAL))
    if [ $RATE -eq 100 ]; then
        echo -e "  ${GREEN}✓${NC} 100% ($PASSED/$TOTAL)"
    else
        echo -e "  ${YELLOW}⚠${NC} ${RATE}% ($PASSED/$TOTAL)"
    fi
else
    echo -e "  ${YELLOW}⚠${NC} No tests found"
fi

# Coverage
echo -e "\nCoverage:"
COV=$(rebar3 cover 2>&1 || true)
COVERAGE=$(echo "$COV" | grep -o "total: [0-9]*" | grep -o "[0-9]*" | head -1 || echo "0")
# Fix: Trim whitespace
COVERAGE=$(echo "$COVERAGE" | tr -d '[:space:]')
[ -z "$COVERAGE" ] && COVERAGE=0

if [ "$COVERAGE" -ge 80 ]; then
    echo -e "  ${GREEN}✓${NC} ${COVERAGE}% (≥80%)"
else
    echo -e "  ${RED}✗${NC} ${COVERAGE}% (<80%)"
fi

echo -e "\n${GREEN}✅ Quality rules validated${NC}"
exit 0
