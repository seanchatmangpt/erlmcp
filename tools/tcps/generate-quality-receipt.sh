#!/usr/bin/env bash
###############################################################################
# TCPS Quality Receipt Generator
#
# Runs quality-checker.sh and generates quality gate receipt from results.
# Adds receipt to TCPS receipt chain with SHA-256 integrity.
#
# Usage:
#   ./generate-quality-receipt.sh <SKU_ID>
#
# Example:
#   ./generate-quality-receipt.sh SKU-2026-001
#
# Outputs:
#   - priv/receipts/<SKU_ID>/quality-gate-<timestamp>.json
#   - Commits receipt to git for immutability
#
###############################################################################

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
RECEIPTS_DIR="$PROJECT_ROOT/priv/receipts"
QUALITY_CHECKER="$SCRIPT_DIR/../quality/quality-checker.sh"

# Check arguments
if [ $# -ne 1 ]; then
    echo "Usage: $0 <SKU_ID>"
    echo "Example: $0 SKU-2026-001"
    exit 1
fi

SKU_ID="$1"
TIMESTAMP=$(date +%s%3N)
RECEIPT_ID="RCPT-quality-$TIMESTAMP"

echo "=== TCPS Quality Receipt Generator ==="
echo "SKU ID: $SKU_ID"
echo "Receipt ID: $RECEIPT_ID"
echo ""

# Create receipts directory for SKU
SKU_RECEIPTS_DIR="$RECEIPTS_DIR/$SKU_ID"
mkdir -p "$SKU_RECEIPTS_DIR"

# Run quality checker
echo "Step 1/4: Running quality checks..."
if [ ! -f "$QUALITY_CHECKER" ]; then
    echo -e "${RED}✗ Quality checker not found: $QUALITY_CHECKER${NC}"
    echo "Please ensure tools/quality/quality-checker.sh exists"
    exit 1
fi

# Run quality checker and capture results
QUALITY_RESULTS=$(mktemp)
if bash "$QUALITY_CHECKER" > "$QUALITY_RESULTS" 2>&1; then
    echo -e "${GREEN}✓ Quality checks passed${NC}"
    QUALITY_STATUS="passed"
else
    echo -e "${YELLOW}⚠ Quality checks had issues (see results)${NC}"
    QUALITY_STATUS="failed"
fi

# Parse quality checker results
echo "Step 2/4: Parsing quality results..."

# Extract metrics from quality checker output
COMPILATION_ERRORS=$(grep -oP 'Compilation errors: \K\d+' "$QUALITY_RESULTS" || echo "0")
TEST_TOTAL=$(grep -oP 'Total tests: \K\d+' "$QUALITY_RESULTS" || echo "0")
TEST_PASSED=$(grep -oP 'Tests passed: \K\d+' "$QUALITY_RESULTS" || echo "0")
COVERAGE=$(grep -oP 'Coverage: \K[\d.]+' "$QUALITY_RESULTS" || echo "0.0")
DIALYZER_WARNINGS=$(grep -oP 'Dialyzer warnings: \K\d+' "$QUALITY_RESULTS" || echo "0")
XREF_ISSUES=$(grep -oP 'Xref issues: \K\d+' "$QUALITY_RESULTS" || echo "0")
BENCHMARK_REGRESSION=$(grep -oP 'Benchmark regression: \K[\d.]+' "$QUALITY_RESULTS" || echo "0.0")

# Calculate test pass rate
if [ "$TEST_TOTAL" -eq 0 ]; then
    TEST_PASS_RATE="0.0"
else
    TEST_PASS_RATE=$(echo "scale=4; $TEST_PASSED / $TEST_TOTAL" | bc)
fi

# Determine gate status
if [ "$COMPILATION_ERRORS" -eq 0 ]; then
    COMPILATION_STATUS="passed"
else
    COMPILATION_STATUS="failed"
fi

if [ "$DIALYZER_WARNINGS" -eq 0 ]; then
    DIALYZER_CLEAN="true"
else
    DIALYZER_CLEAN="false"
fi

if [ "$XREF_ISSUES" -eq 0 ]; then
    XREF_CLEAN="true"
else
    XREF_CLEAN="false"
fi

echo "  Compilation: $COMPILATION_STATUS ($COMPILATION_ERRORS errors)"
echo "  Tests: $TEST_PASSED/$TEST_TOTAL passed ($TEST_PASS_RATE rate)"
echo "  Coverage: ${COVERAGE}%"
echo "  Dialyzer: $DIALYZER_CLEAN ($DIALYZER_WARNINGS warnings)"
echo "  Xref: $XREF_CLEAN ($XREF_ISSUES issues)"
echo "  Benchmark regression: ${BENCHMARK_REGRESSION}%"

# Generate quality receipt JSON
echo "Step 3/4: Generating quality receipt..."

RECEIPT_FILE="$SKU_RECEIPTS_DIR/quality-gate-$TIMESTAMP.json"

# Create receipt JSON (without checksum first)
cat > "$RECEIPT_FILE" <<EOF
{
  "receipt_id": "$RECEIPT_ID",
  "receipt_type": "quality_gate",
  "sku_id": "$SKU_ID",
  "gate": "quality",
  "timestamp": $TIMESTAMP,
  "timestamp_iso": "$(date -u +"%Y-%m-%dT%H:%M:%S.%3NZ")",
  "status": "$QUALITY_STATUS",
  "details": {
    "compilation_status": "$COMPILATION_STATUS",
    "compilation_errors": $COMPILATION_ERRORS,
    "test_pass_rate": $TEST_PASS_RATE,
    "test_total": $TEST_TOTAL,
    "test_passed": $TEST_PASSED,
    "coverage_percentage": $COVERAGE,
    "dialyzer_clean": $DIALYZER_CLEAN,
    "dialyzer_warnings": $DIALYZER_WARNINGS,
    "xref_clean": $XREF_CLEAN,
    "xref_issues": $XREF_ISSUES,
    "benchmark_regression": $BENCHMARK_REGRESSION
  },
  "ontology_refs": [
    "tcps:QualityGate",
    "tcps:Receipt",
    "tcps:$SKU_ID"
  ]
}
EOF

# Compute SHA-256 checksum
CHECKSUM=$(sha256sum "$RECEIPT_FILE" | awk '{print $1}' | base64 -w 0)

# Add checksum to receipt
TMP_RECEIPT=$(mktemp)
jq --arg checksum "$CHECKSUM" '. + {checksum: $checksum}' "$RECEIPT_FILE" > "$TMP_RECEIPT"
mv "$TMP_RECEIPT" "$RECEIPT_FILE"

echo -e "${GREEN}✓ Receipt generated: $RECEIPT_FILE${NC}"

# Commit receipt to git for immutability
echo "Step 4/4: Committing receipt to git..."

cd "$PROJECT_ROOT"

if git rev-parse --git-dir > /dev/null 2>&1; then
    git add "$RECEIPT_FILE"

    if git diff --cached --quiet; then
        echo "  No changes to commit (receipt already exists)"
    else
        git commit -m "Add quality receipt for $SKU_ID

Receipt ID: $RECEIPT_ID
Status: $QUALITY_STATUS
Timestamp: $(date -u +"%Y-%m-%dT%H:%M:%SZ")

Quality Metrics:
- Compilation: $COMPILATION_STATUS ($COMPILATION_ERRORS errors)
- Tests: $TEST_PASSED/$TEST_TOTAL passed
- Coverage: ${COVERAGE}%
- Dialyzer: $DIALYZER_CLEAN
- Xref: $XREF_CLEAN

Generated by: TCPS Quality Receipt Generator
Checksum: $CHECKSUM" || echo "  Commit skipped (nothing to commit)"
    fi

    echo -e "${GREEN}✓ Receipt committed to git${NC}"
else
    echo -e "${YELLOW}⚠ Not a git repository, skipping commit${NC}"
fi

# Cleanup
rm -f "$QUALITY_RESULTS"

echo ""
echo "=== Quality Receipt Summary ==="
echo "Receipt ID: $RECEIPT_ID"
echo "File: $RECEIPT_FILE"
echo "Status: $QUALITY_STATUS"
echo "Checksum: $CHECKSUM"
echo ""

if [ "$QUALITY_STATUS" = "passed" ]; then
    echo -e "${GREEN}✓ Quality receipt generated successfully${NC}"
    exit 0
else
    echo -e "${YELLOW}⚠ Quality receipt generated with issues${NC}"
    echo "Review quality results and fix issues before release"
    exit 1
fi
