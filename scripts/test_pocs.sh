#!/usr/bin/env bash
#
# POC Test Runner Script
# Tests all POC modules with quality gates
#
# Usage: ./scripts/test_pocs.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

cd "${PROJECT_ROOT}"

echo "========================================="
echo "POC Module Test Suite"
echo "========================================="
echo ""

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

ERRORS=0

# 1. Compilation
echo "Step 1: Compiling POC modules..."
if TERM=dumb rebar3 compile; then
    echo -e "${GREEN}✅ Compilation: PASS${NC}"
else
    echo -e "${RED}❌ Compilation: FAIL${NC}"
    ((ERRORS++))
fi
echo ""

# 2. Run EUnit tests
echo "Step 2: Running EUnit tests..."
if rebar3 eunit --module=erlmcp_poc_tests; then
    echo -e "${GREEN}✅ Tests: PASS${NC}"
else
    echo -e "${RED}❌ Tests: FAIL${NC}"
    ((ERRORS++))
fi
echo ""

# 3. Coverage check
echo "Step 3: Checking code coverage..."
rebar3 cover --verbose > /tmp/poc_coverage.txt
COVERAGE=$(grep -oP "total.*\K\d+(?=%)" /tmp/poc_coverage.txt | head -1 || echo "0")

if [ "${COVERAGE}" -ge 80 ]; then
    echo -e "${GREEN}✅ Coverage: ${COVERAGE}% (target: ≥80%)${NC}"
else
    echo -e "${RED}❌ Coverage: ${COVERAGE}% (target: ≥80%)${NC}"
    ((ERRORS++))
fi
echo ""

# 4. Test individual POCs
echo "Step 4: Testing individual POC modules..."

POCS=(
    "telemetry_poc_test_"
    "pubsub_poc_test_"
    "distributed_registry_poc_test_"
    "consensus_poc_test_"
    "streaming_poc_test_"
    "circuit_breaker_poc_test_"
    "pool_poc_test_"
)

for poc in "${POCS[@]}"; do
    echo -n "  Testing ${poc}... "
    if rebar3 eunit --module=erlmcp_poc_tests --tests="${poc}" > /tmp/poc_test_${poc}.log 2>&1; then
        echo -e "${GREEN}PASS${NC}"
    else
        echo -e "${RED}FAIL${NC}"
        cat /tmp/poc_test_${poc}.log
        ((ERRORS++))
    fi
done
echo ""

# 5. Summary
echo "========================================="
echo "Test Summary"
echo "========================================="

if [ "${ERRORS}" -eq 0 ]; then
    echo -e "${GREEN}✅ All tests passed!${NC}"
    echo ""
    echo "POC Modules: 7"
    echo "Test Cases: 28"
    echo "Coverage: ${COVERAGE}%"
    echo ""
    echo "Ready for integration into erlmcp!"
    exit 0
else
    echo -e "${RED}❌ ${ERRORS} test(s) failed${NC}"
    echo ""
    echo "Please fix the errors above and re-run."
    exit 1
fi
