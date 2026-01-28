#!/usr/bin/env bash
# quality-snapshot.sh - Capture current quality metrics snapshot
# Usage: ./quality-snapshot.sh [output_file]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
METRICS_DIR="$PROJECT_ROOT/metrics/snapshots"
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
DATE=$(date -u +"%Y-%m-%d")
OUTPUT_FILE="${1:-$METRICS_DIR/$DATE.json}"

cd "$PROJECT_ROOT"

echo "=== erlmcp Quality Metrics Snapshot ==="
echo "Timestamp: $TIMESTAMP"
echo "Output: $OUTPUT_FILE"
echo ""

# Ensure metrics directory exists
mkdir -p "$METRICS_DIR"

# Get Git info
GIT_HASH=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
GIT_TAG=$(git describe --tags --exact-match 2>/dev/null || echo "none")

echo "[1/7] Capturing Git metadata..."
echo "  Commit: $GIT_HASH"
echo "  Branch: $GIT_BRANCH"

# Compile and capture warnings
echo "[2/7] Compiling and counting warnings..."
COMPILE_OUTPUT=$(mktemp)
if TERM=dumb rebar3 compile 2>&1 | tee "$COMPILE_OUTPUT"; then
    COMPILE_SUCCESS=true
    COMPILE_WARNINGS=$(grep -c "Warning:" "$COMPILE_OUTPUT" || echo "0")
else
    COMPILE_SUCCESS=false
    COMPILE_WARNINGS=$(grep -c "Warning:" "$COMPILE_OUTPUT" || echo "0")
fi
echo "  Warnings: $COMPILE_WARNINGS"

# Count lines of code
echo "[3/7] Counting lines of code..."
LOC_SRC=$(find src -name "*.erl" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print $1}' || echo "0")
LOC_TEST=$(find test -name "*.erl" -exec wc -l {} + 2>/dev/null | tail -1 | awk '{print $1}' || echo "0")
LOC_TOTAL=$((LOC_SRC + LOC_TEST))
MODULE_COUNT=$(find src -name "*.erl" | wc -l | tr -d ' ')
TEST_MODULE_COUNT=$(find test -name "*.erl" | wc -l | tr -d ' ')
echo "  Source: $LOC_SRC lines in $MODULE_COUNT modules"
echo "  Tests: $LOC_TEST lines in $TEST_MODULE_COUNT modules"

# Run tests and capture results
echo "[4/7] Running tests..."
TEST_OUTPUT=$(mktemp)
if rebar3 eunit --cover 2>&1 | tee "$TEST_OUTPUT"; then
    TEST_SUCCESS=true
    TEST_PASSED=$(grep -oP '\d+(?= tests passed)' "$TEST_OUTPUT" | head -1 || echo "0")
    TEST_FAILED=$(grep -oP '\d+(?= failed)' "$TEST_OUTPUT" | head -1 || echo "0")
    TEST_TOTAL=$((TEST_PASSED + TEST_FAILED))
    TEST_PASS_RATE=$(awk "BEGIN {printf \"%.2f\", ($TEST_PASSED / ($TEST_TOTAL > 0 ? $TEST_TOTAL : 1)) * 100}")
else
    TEST_SUCCESS=false
    TEST_PASSED=0
    TEST_FAILED=0
    TEST_TOTAL=0
    TEST_PASS_RATE="0.00"
fi
echo "  Passed: $TEST_PASSED/$TEST_TOTAL ($TEST_PASS_RATE%)"

# Extract coverage
echo "[5/7] Extracting coverage..."
COVERAGE_FILE="_build/test/cover/index.html"
if [ -f "$COVERAGE_FILE" ]; then
    COVERAGE=$(grep -oP 'Total: \K\d+' "$COVERAGE_FILE" | head -1 || echo "0")
else
    COVERAGE="0"
fi
echo "  Coverage: $COVERAGE%"

# Run Dialyzer
echo "[6/7] Running Dialyzer..."
DIALYZER_OUTPUT=$(mktemp)
if rebar3 dialyzer 2>&1 | tee "$DIALYZER_OUTPUT"; then
    DIALYZER_SUCCESS=true
    DIALYZER_WARNINGS=$(grep -c "Warning:" "$DIALYZER_OUTPUT" || echo "0")
else
    DIALYZER_SUCCESS=false
    DIALYZER_WARNINGS=$(grep -c "Warning:" "$DIALYZER_OUTPUT" || echo "0")
fi
echo "  Warnings: $DIALYZER_WARNINGS"

# Calculate cyclomatic complexity (approximation via function count)
echo "[7/7] Calculating complexity metrics..."
FUNCTION_COUNT=$(grep -rh "^[a-z_][a-zA-Z0-9_]*(" src --include="*.erl" | wc -l | tr -d ' ')
AVG_FUNCTIONS_PER_MODULE=$(awk "BEGIN {printf \"%.1f\", $FUNCTION_COUNT / ($MODULE_COUNT > 0 ? $MODULE_COUNT : 1)}")
AVG_LOC_PER_MODULE=$(awk "BEGIN {printf \"%.1f\", $LOC_SRC / ($MODULE_COUNT > 0 ? $MODULE_COUNT : 1)}")
echo "  Functions: $FUNCTION_COUNT (avg $AVG_FUNCTIONS_PER_MODULE per module)"
echo "  Avg module size: $AVG_LOC_PER_MODULE LOC"

# Generate JSON snapshot
echo ""
echo "Generating JSON snapshot..."
cat > "$OUTPUT_FILE" << JSON
{
  "timestamp": "$TIMESTAMP",
  "date": "$DATE",
  "git": {
    "hash": "$GIT_HASH",
    "branch": "$GIT_BRANCH",
    "tag": "$GIT_TAG"
  },
  "compilation": {
    "success": $COMPILE_SUCCESS,
    "warnings": $COMPILE_WARNINGS
  },
  "tests": {
    "success": $TEST_SUCCESS,
    "total": $TEST_TOTAL,
    "passed": $TEST_PASSED,
    "failed": $TEST_FAILED,
    "pass_rate": $TEST_PASS_RATE
  },
  "coverage": {
    "percentage": $COVERAGE
  },
  "dialyzer": {
    "success": $DIALYZER_SUCCESS,
    "warnings": $DIALYZER_WARNINGS
  },
  "code_metrics": {
    "loc_source": $LOC_SRC,
    "loc_tests": $LOC_TEST,
    "loc_total": $LOC_TOTAL,
    "modules": $MODULE_COUNT,
    "test_modules": $TEST_MODULE_COUNT,
    "functions": $FUNCTION_COUNT,
    "avg_functions_per_module": $AVG_FUNCTIONS_PER_MODULE,
    "avg_loc_per_module": $AVG_LOC_PER_MODULE,
    "test_to_code_ratio": $(awk "BEGIN {printf \"%.2f\", $LOC_TEST / ($LOC_SRC > 0 ? $LOC_SRC : 1)}")
  },
  "quality_score": {
    "overall": $(awk "BEGIN {printf \"%.1f\", ($TEST_PASS_RATE * 0.3) + ($COVERAGE * 0.3) + (($COMPILE_WARNINGS == 0 ? 100 : 50) * 0.2) + (($DIALYZER_WARNINGS == 0 ? 100 : 50) * 0.2)}")
  }
}
JSON

# Cleanup temp files
rm -f "$COMPILE_OUTPUT" "$TEST_OUTPUT" "$DIALYZER_OUTPUT"

echo ""
echo "✅ Snapshot saved to: $OUTPUT_FILE"
echo ""

# Display summary
QUALITY_SCORE=$(jq -r '.quality_score.overall' "$OUTPUT_FILE")
echo "=== Quality Summary ==="
echo "Overall Score: $QUALITY_SCORE/100"
echo "Test Pass Rate: $TEST_PASS_RATE%"
echo "Coverage: $COVERAGE%"
echo "Compile Warnings: $COMPILE_WARNINGS"
echo "Dialyzer Warnings: $DIALYZER_WARNINGS"
echo ""

if (( $(echo "$QUALITY_SCORE >= 80" | bc -l) )); then
    echo "✅ Quality: EXCELLENT"
elif (( $(echo "$QUALITY_SCORE >= 60" | bc -l) )); then
    echo "⚠️  Quality: GOOD (room for improvement)"
else
    echo "❌ Quality: NEEDS ATTENTION"
fi
