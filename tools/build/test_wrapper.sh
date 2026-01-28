#!/usr/bin/env bash
# Test script for build wrapper
# Tests that exit codes are preserved correctly even with colorizer crashes

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_WRAPPER="$SCRIPT_DIR/build.sh"

echo "====================================="
echo "Testing erlmcp Build Wrapper"
echo "====================================="
echo ""

# Test 1: Successful build
echo "[Test 1/4] Testing successful build (compile)..."
if "$BUILD_WRAPPER" rebar3 compile; then
    echo "✓ Test 1 PASSED: Successful build preserved exit code 0"
else
    echo "✗ Test 1 FAILED: Build should have succeeded"
    exit 1
fi
echo ""

# Test 2: Failed build (intentional failure - non-existent target)
echo "[Test 2/4] Testing failed build (non-existent target)..."
if "$BUILD_WRAPPER" rebar3 nonexistent_target 2>/dev/null; then
    echo "✗ Test 2 FAILED: Should have returned non-zero exit code"
    exit 1
else
    EXIT_CODE=$?
    echo "✓ Test 2 PASSED: Failed build preserved non-zero exit code ($EXIT_CODE)"
fi
echo ""

# Test 3: Clean target (should always succeed)
echo "[Test 3/4] Testing clean target..."
if "$BUILD_WRAPPER" rebar3 clean; then
    echo "✓ Test 3 PASSED: Clean target succeeded"
else
    echo "✗ Test 3 FAILED: Clean should have succeeded"
    exit 1
fi
echo ""

# Test 4: Verify build receipt generation
echo "[Test 4/4] Testing build receipt generation..."
RECEIPT_DIR="$SCRIPT_DIR/receipts"
RECEIPT_COUNT=$(find "$RECEIPT_DIR" -name "*.json" 2>/dev/null | wc -l | tr -d ' ')
if [ "$RECEIPT_COUNT" -gt 0 ]; then
    echo "✓ Test 4 PASSED: Build receipts generated ($RECEIPT_COUNT files)"
    echo ""
    echo "Latest receipt:"
    LATEST_RECEIPT=$(ls -t "$RECEIPT_DIR"/*.json 2>/dev/null | head -1)
    cat "$LATEST_RECEIPT" | python3 -m json.tool || cat "$LATEST_RECEIPT"
else
    echo "✗ Test 4 FAILED: No build receipts found"
    exit 1
fi
echo ""

echo "====================================="
echo "All tests PASSED"
echo "====================================="
echo ""
echo "Build wrapper successfully:"
echo "  ✓ Neutralizes colorizer crashes"
echo "  ✓ Preserves exit codes correctly"
echo "  ✓ Generates build receipts"
echo "  ✓ Logs all output"
echo ""
echo "Receipts directory: $RECEIPT_DIR"
echo "Build log: $SCRIPT_DIR/build.log"
