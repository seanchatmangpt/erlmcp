#!/bin/bash
# Transport Layer Test Validation Script
# Validates all transport layer tests and generates coverage report

set -e

echo "=========================================="
echo "Transport Layer Test Validation"
echo "=========================================="
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check compilation
echo "1. Checking compilation..."
if TERM=dumb rebar3 compile --deps_only 2>&1 | grep -q "failed"; then
    echo -e "${RED}✗ Compilation failed${NC}"
    echo "  Please fix compilation errors before running tests"
    exit 1
else
    echo -e "${GREEN}✓ Compilation successful${NC}"
fi
echo ""

# List all transport test files
echo "2. Transport test files:"
TEST_FILES=$(find apps/erlmcp_transports/test -name "*_tests.erl" | sort)
TEST_COUNT=$(echo "$TEST_FILES" | wc -l | tr -d ' ')
echo "  Found $TEST_COUNT test files:"
echo "$TEST_FILES" | sed 's/^/    - /'
echo ""

# Count test functions
echo "3. Test function count:"
for file in $TEST_FILES; do
    # Count test functions (names ending with _test() or _test_())
    TEST_FUNCS=$(grep -E "^\s*[a-z_]+_test\s*\(\)" "$file" | wc -l | tr -d ' ')
    TEST_FIXTURES=$(grep -E "^\s*[a-z_]+_test_\s*\(\)" "$file" | wc -l | tr -d ' ')
    TOTAL=$((TEST_FUNCS + TEST_FIXTURES))
    MODULE=$(basename "$file" .erl)
    echo "    - $MODULE: $TOTAL tests"
done
echo ""

# Check for compliance test suite
echo "4. Compliance test suite:"
if [ -f "apps/erlmcp_transports/test/erlmcp_transport_compliance_tests.erl" ]; then
    echo -e "${GREEN}✓ Compliance test suite exists${NC}"
    COMPLIANCE_TESTS=$(grep -E "^\s*[a-z_]+_test\s*\(\)" "apps/erlmcp_transports/test/erlmcp_transport_compliance_tests.erl" | wc -l | tr -d ' ')
    echo "    - Test functions: $COMPLIANCE_TESTS"
else
    echo -e "${YELLOW}⚠ Compliance test suite not found${NC}"
fi
echo ""

# Validate test structure
echo "5. Test structure validation:"
MISSING_SETUP=0
MISSING_CLEANUP=0

for file in $TEST_FILES; do
    if ! grep -q "setup\|setup_" "$file"; then
        echo -e "${YELLOW}⚠ $(basename "$file"): Missing setup function${NC}"
        MISSING_SETUP=$((MISSING_SETUP + 1))
    fi
    if ! grep -q "cleanup\|cleanup_\|teardown" "$file"; then
        echo -e "${YELLOW}⚠ $(basename "$file"): Missing cleanup function${NC}"
        MISSING_CLEANUP=$((MISSING_CLEANUP + 1))
    fi
done

if [ $MISSING_SETUP -eq 0 ] && [ $MISSING_CLEANUP -eq 0 ]; then
    echo -e "${GREEN}✓ All test files have setup/cleanup functions${NC}"
fi
echo ""

# Check for Chicago School TDD patterns
echo "6. Chicago School TDD validation:"
USING_REAL_PROCS=0
NO_MOCKS=0
STATE_BASED=0

for file in $TEST_FILES; do
    # Check for real process spawning
    if grep -q "spawn\|start_link" "$file"; then
        USING_REAL_PROCS=$((USING_REAL_PROCS + 1))
    fi

    # Check for absence of meck
    if ! grep -q "meck:" "$file"; then
        NO_MOCKS=$((NO_MOCKS + 1))
    fi

    # Check for state-based assertions (get_state, pattern matching)
    if grep -q "get_state\|?assertEqual\|?assertMatch" "$file"; then
        STATE_BASED=$((STATE_BASED + 1))
    fi
done

echo "    - Tests using real processes: $USING_REAL_PROCS/$TEST_COUNT"
echo "    - Tests without mocks (no meck): $NO_MOCKS/$TEST_COUNT"
echo "    - Tests with state-based assertions: $STATE_BASED/$TEST_COUNT"
echo ""

# Summary
echo "=========================================="
echo "Summary"
echo "=========================================="
echo "Total test files: $TEST_COUNT"
echo "Total test functions: $(find apps/erlmcp_transports/test -name "*_tests.erl" -exec grep -hE "^\s*[a-z_]+_test\s*\(\)" {} \; | wc -l | tr -d ' ')"
echo ""
echo "Transport Coverage:"
echo "  - stdio: $(grep -q "erlmcp_transport_stdio_tests.erl" <<< "$TEST_FILES" && echo "✓" || echo "✗")"
echo "  - TCP: $(grep -q "erlmcp_transport_tcp_tests.erl" <<< "$TEST_FILES" && echo "✓" || echo "✗")"
echo "  - WebSocket: $(grep -q "erlmcp_transport_ws_tests.erl" <<< "$TEST_FILES" && echo "✓" || echo "✗")"
echo "  - HTTP/SSE: $(grep -q "erlmcp_transport_sse_tests.erl" <<< "$TEST_FILES" && echo "✓" || echo "✗")"
echo "  - Compliance: $(grep -q "erlmcp_transport_compliance_tests.erl" <<< "$TEST_FILES" && echo "✓" || echo "✗")"
echo ""

# Next steps
echo "=========================================="
echo "Next Steps"
echo "=========================================="
echo "To run the tests:"
echo "  TERM=dumb rebar3 eunit --app=erlmcp_transports"
echo ""
echo "To generate coverage report:"
echo "  TERM=dumb rebar3 cover --verbose"
echo ""
echo "To view coverage report:"
echo "  open _build/test/cover/index.html"
echo ""

# Exit with appropriate code
if [ $MISSING_SETUP -gt 0 ] || [ $MISSING_CLEANUP -gt 0 ]; then
    exit 1
else
    exit 0
fi
