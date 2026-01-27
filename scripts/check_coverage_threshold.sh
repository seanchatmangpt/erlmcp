#!/bin/bash
#==============================================================================
# TCPS Coverage Threshold Check Script
# Validates code coverage meets Lean Six Sigma standards
#==============================================================================

set -e

THRESHOLD=${1:-80}  # Default 80% if not specified
CRITICAL_THRESHOLD=90  # Critical modules must have 90%+

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
COVER_DIR="$PROJECT_ROOT/_build/test/cover"
COVER_LOG="$COVER_DIR/cover.log"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}===================================================================${NC}"
echo -e "${BLUE}TCPS Coverage Threshold Validation${NC}"
echo -e "${BLUE}Standard Threshold: ${THRESHOLD}% | Critical Modules: ${CRITICAL_THRESHOLD}%${NC}"
echo -e "${BLUE}===================================================================${NC}"
echo ""

if [ ! -f "$COVER_LOG" ]; then
    echo -e "${RED}✗ Coverage log not found: $COVER_LOG${NC}"
    echo -e "${YELLOW}  Run: ./scripts/generate_coverage.sh first${NC}"
    exit 1
fi

# Critical modules that must have ≥90% coverage
CRITICAL_MODULES=(
    "tcps_andon"
    "tcps_root_cause"
    "tcps_receipt_verifier"
    "tcps_rebar3_quality"
)

# Extract coverage data from log
echo -e "${BLUE}Analyzing coverage data...${NC}"
echo ""

# Get overall coverage
OVERALL_COVERAGE=$(grep "total" "$COVER_LOG" | tail -1 | awk -F'|' '{print $3}' | tr -d ' %|')

if [ -z "$OVERALL_COVERAGE" ] || [ "$OVERALL_COVERAGE" = "-" ]; then
    echo -e "${RED}✗ Could not parse coverage data${NC}"
    echo -e "${YELLOW}  Check coverage log: $COVER_LOG${NC}"
    OVERALL_COVERAGE="0"
fi

echo -e "${BLUE}Overall Coverage:${NC} ${OVERALL_COVERAGE}%"
echo ""

# Check overall threshold
if [ $(echo "$OVERALL_COVERAGE >= $THRESHOLD" | bc) -eq 1 ]; then
    echo -e "${GREEN}✓ Overall coverage PASSED (${OVERALL_COVERAGE}% >= ${THRESHOLD}%)${NC}"
    OVERALL_PASS=1
else
    echo -e "${RED}✗ Overall coverage FAILED (${OVERALL_COVERAGE}% < ${THRESHOLD}%)${NC}"
    OVERALL_PASS=0
fi
echo ""

# Check critical modules
echo -e "${BLUE}Critical Modules (must be ≥${CRITICAL_THRESHOLD}%):${NC}"
echo ""

CRITICAL_PASS=1
for module in "${CRITICAL_MODULES[@]}"; do
    MODULE_COVERAGE=$(grep "|\s*${module}\s*|" "$COVER_LOG" | awk '{print $(NF-1)}' | tr -d '%|')

    if [ -z "$MODULE_COVERAGE" ]; then
        echo -e "  ${YELLOW}⚠ ${module}: ${NC}NOT FOUND in coverage report"
        CRITICAL_PASS=0
        continue
    fi

    if [ $(echo "$MODULE_COVERAGE >= $CRITICAL_THRESHOLD" | bc) -eq 1 ]; then
        echo -e "  ${GREEN}✓ ${module}: ${MODULE_COVERAGE}%${NC}"
    else
        echo -e "  ${RED}✗ ${module}: ${MODULE_COVERAGE}% (< ${CRITICAL_THRESHOLD}%)${NC}"
        CRITICAL_PASS=0
    fi
done
echo ""

# Check all modules against standard threshold
echo -e "${BLUE}Modules Below ${THRESHOLD}% Coverage:${NC}"
echo ""

BELOW_THRESHOLD_COUNT=0
while IFS= read -r line; do
    if echo "$line" | grep -q "^\s*|.*%\s*|"; then
        MODULE=$(echo "$line" | awk -F'|' '{print $2}' | tr -d ' ')
        COVERAGE=$(echo "$line" | awk -F'|' '{print $3}' | tr -d ' %')

        # Skip header and total rows
        if [ "$MODULE" == "module" ] || [ "$MODULE" == "total" ]; then
            continue
        fi

        if [ ! -z "$COVERAGE" ] && [ $(echo "$COVERAGE < $THRESHOLD" | bc) -eq 1 ]; then
            echo -e "  ${YELLOW}⚠ ${MODULE}: ${COVERAGE}%${NC}"
            ((BELOW_THRESHOLD_COUNT++))
        fi
    fi
done < "$COVER_LOG"

if [ $BELOW_THRESHOLD_COUNT -eq 0 ]; then
    echo -e "  ${GREEN}✓ All modules meet ${THRESHOLD}% threshold!${NC}"
else
    echo -e "  ${YELLOW}Found ${BELOW_THRESHOLD_COUNT} modules below threshold${NC}"
fi
echo ""

# Final result
echo -e "${BLUE}===================================================================${NC}"
if [ $OVERALL_PASS -eq 1 ] && [ $CRITICAL_PASS -eq 1 ]; then
    echo -e "${GREEN}✓✓✓ COVERAGE VALIDATION PASSED ✓✓✓${NC}"
    echo -e "${BLUE}===================================================================${NC}"
    exit 0
else
    echo -e "${RED}✗✗✗ COVERAGE VALIDATION FAILED ✗✗✗${NC}"
    echo -e "${BLUE}===================================================================${NC}"
    echo ""
    echo -e "${YELLOW}Action Required:${NC}"
    if [ $OVERALL_PASS -eq 0 ]; then
        echo -e "  • Increase overall coverage from ${OVERALL_COVERAGE}% to ≥${THRESHOLD}%"
    fi
    if [ $CRITICAL_PASS -eq 0 ]; then
        echo -e "  • Ensure critical modules have ≥${CRITICAL_THRESHOLD}% coverage"
    fi
    echo ""
    exit 1
fi
