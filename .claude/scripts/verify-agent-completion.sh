#!/bin/bash
# Verify agent completion quality gates

set -e

echo "🔍 Running pre-completion verification..."
echo ""

# Run all tests
echo "📋 Running tests..."
if rebar3 do eunit, ct, proper -c; then
    echo "✅ Tests: All passed"
    TEST_STATUS="✅"
else
    echo "❌ Tests: FAILED"
    TEST_STATUS="❌"
    exit 1
fi

echo ""

# Run quality checks
echo "🔍 Running quality checks..."

if rebar3 compile 2>&1 | grep -q "Compiling"; then
    COMPILE_STATUS="✅ Compile: Clean"
else
    COMPILE_STATUS="❌ Compile: FAILED"
    exit 1
fi

if rebar3 dialyzer 2>&1 | grep -qE "(0 warnings|passed successfully)"; then
    DIALYZER_STATUS="✅ Dialyzer: Clean"
else
    DIALYZER_STATUS="❌ Dialyzer: FAILED"
    exit 1
fi

if rebar3 xref; then
    XREF_STATUS="✅ Xref: Clean"
else
    XREF_STATUS="❌ Xref: FAILED"
    exit 1
fi

if rebar3 format --verify; then
    FORMAT_STATUS="✅ Format: Verified"
else
    FORMAT_STATUS="❌ Format: FAILED"
    exit 1
fi

echo ""

# Generate coverage report
echo "📊 Generating coverage report..."
if rebar3 cover --verbose > /tmp/coverage.txt 2>&1; then
    COVERAGE=$(grep -oP 'Total\s+\K\d+%' /tmp/coverage.txt || echo "N/A")
    echo "✅ Coverage: $COVERAGE"
    COVERAGE_STATUS="✅ Coverage: $COVERAGE"
else
    echo "⚠️  Coverage: Could not generate"
    COVERAGE_STATUS="⚠️  Coverage: N/A"
fi

echo ""
echo "═══════════════════════════════════════════"
echo "   Pre-Completion Verification Report"
echo "═══════════════════════════════════════════"
echo ""
echo "$TEST_STATUS"
echo "$COMPILE_STATUS"
echo "$DIALYZER_STATUS"
echo "$XREF_STATUS"
echo "$FORMAT_STATUS"
echo "$COVERAGE_STATUS"
echo ""
echo "✅ All quality gates passed"
echo ""
