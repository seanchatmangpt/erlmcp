#!/bin/bash
set -e

echo "=== MCP CAPABILITY TEST SUITE ==="
echo "Testing all 10 capability categories"
echo ""

# Compile test suite
echo "Compiling test suite..."
rebar3 compile
rebar3 ct --suite=test/erlmcp_capability_test_SUITE --compile_only

# Run tests
echo ""
echo "Running capability tests..."
rebar3 ct --suite=test/erlmcp_capability_test_SUITE --verbose

# Generate report
echo ""
echo "=== TEST RESULTS ==="
echo "Results available in: _build/test/logs/"

# Extract summary
if [ -f _build/test/logs/erlmcp_capability_test_SUITE.html ]; then
    echo "HTML report generated"
fi

echo ""
echo "Test suite complete!"
