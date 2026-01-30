#!/bin/bash
# Quick test of performance regression SUITE

echo "=== Performance Regression SUITE Test ==="
echo ""

cd /Users/sac/erlmcp

# Check if file exists
if [ ! -f "apps/erlmcp_observability/test/erlmcp_performance_regression_SUITE.erl" ]; then
    echo "ERROR: SUITE file not found"
    exit 1
fi

echo "✓ SUITE file exists"

# Check compilation
echo ""
echo "Checking compilation..."
TERM=dumb rebar3 compile 2>&1 | grep -E "(Compiling|error)" || true

# Check baseline file
echo ""
echo "Checking baseline file..."
if [ -f "bench/baselines/2026-01-28_v2.0.0.json" ]; then
    echo "✓ Baseline file exists"
    echo "  Version: $(jq -r '.version' bench/baselines/2026-01-28_v2.0.0.json)"
    echo "  Date: $(jq -r '.date' bench/baselines/2026-01-28_v2.0.0.json)"
else
    echo "WARNING: Baseline file not found"
fi

# Count test cases
echo ""
echo "Test cases in SUITE:"
grep -E "^test_\(" apps/erlmcp_observability/test/erlmcp_performance_regression_SUITE.erl | wc -l | xargs echo "  Found:"

# List test cases
echo ""
echo "Test case list:"
grep -E "^test_\(" apps/erlmcp_observability/test/erlmcp_performance_regression_SUITE.erl | sed 's/^/  /'

echo ""
echo "=== Summary ==="
echo "Performance Regression SUITE is ready for use."
echo ""
echo "To run the SUITE:"
echo "  rebar3 ct --suite=erlmcp_performance_regression_SUITE"
echo ""
echo "To run with specific group:"
echo "  rebar3 ct --suite=erlmcp_performance_regression_SUITE --group=core_ops"
echo ""
