#!/bin/bash
#==============================================================================
# TCPS Coverage Report Generation Script
# Lean Six Sigma Quality Standard: 80%+ code coverage requirement
#==============================================================================

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="$PROJECT_ROOT/_build/test"
COVER_DIR="$BUILD_DIR/cover"
REPORT_FILE="$PROJECT_ROOT/docs/COVERAGE_REPORT.md"

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}===================================================================${NC}"
echo -e "${BLUE}TCPS Code Coverage Report Generation${NC}"
echo -e "${BLUE}Target: 80%+ coverage per Lean Six Sigma standards${NC}"
echo -e "${BLUE}===================================================================${NC}"
echo ""

# Step 1: Clean previous coverage data
echo -e "${YELLOW}[1/6]${NC} Cleaning previous coverage data..."
rm -rf "$COVER_DIR"/*.coverdata "$COVER_DIR"/*.html "$COVER_DIR"/*.log 2>/dev/null || true
echo -e "${GREEN}✓${NC} Coverage data cleaned"
echo ""

# Step 2: Compile with coverage instrumentation
echo -e "${YELLOW}[2/6]${NC} Compiling with coverage instrumentation (test profile)..."
cd "$PROJECT_ROOT"
rebar3 as test compile --verbose 2>&1 | tail -20
if [ $? -ne 0 ]; then
    echo -e "${RED}✗${NC} Compilation failed"
    exit 1
fi
echo -e "${GREEN}✓${NC} Compilation successful"
echo ""

# Step 3: Run EUnit tests with coverage
echo -e "${YELLOW}[3/6]${NC} Running EUnit tests with coverage..."
rebar3 as test eunit --cover 2>&1 | tee "$COVER_DIR/eunit.log" | tail -50
EUNIT_STATUS=${PIPESTATUS[0]}
if [ $EUNIT_STATUS -eq 0 ]; then
    echo -e "${GREEN}✓${NC} EUnit tests passed"
else
    echo -e "${YELLOW}⚠${NC} EUnit tests had some failures (continuing with coverage)"
fi
echo ""

# Step 4: Run Common Test integration tests with coverage
echo -e "${YELLOW}[4/6]${NC} Running Common Test integration tests with coverage..."
if [ -d "$PROJECT_ROOT/test/integration" ]; then
    rebar3 as test ct --dir=test/integration --cover 2>&1 | tee "$COVER_DIR/ct.log" | tail -50
    CT_STATUS=${PIPESTATUS[0]}
    if [ $CT_STATUS -eq 0 ]; then
        echo -e "${GREEN}✓${NC} Common Test integration tests passed"
    else
        echo -e "${YELLOW}⚠${NC} Common Test integration tests had some failures (continuing with coverage)"
    fi
else
    echo -e "${YELLOW}⚠${NC} No integration tests directory found, skipping CT"
fi
echo ""

# Step 5: Generate combined coverage report
echo -e "${YELLOW}[5/6]${NC} Generating combined coverage report..."
rebar3 as test cover --verbose 2>&1 | tee "$COVER_DIR/cover.log"
COVER_STATUS=${PIPESTATUS[0]}
if [ $COVER_STATUS -ne 0 ]; then
    echo -e "${RED}✗${NC} Coverage generation failed"
    exit 1
fi
echo -e "${GREEN}✓${NC} Coverage report generated"
echo ""

# Step 6: Generate HTML reports
echo -e "${YELLOW}[6/6]${NC} HTML coverage reports..."
if [ -f "$COVER_DIR/index.html" ]; then
    echo -e "${GREEN}✓${NC} HTML reports generated at: $COVER_DIR/index.html"
    echo -e "${BLUE}   View in browser:${NC} file://$COVER_DIR/index.html"
else
    echo -e "${YELLOW}⚠${NC} HTML reports not found"
fi
echo ""

# Summary
echo -e "${BLUE}===================================================================${NC}"
echo -e "${BLUE}Coverage Report Generation Complete${NC}"
echo -e "${BLUE}===================================================================${NC}"
echo ""
echo -e "${BLUE}Next steps:${NC}"
echo -e "  1. Run: ${GREEN}./scripts/check_coverage_threshold.sh 80${NC} to validate coverage"
echo -e "  2. View HTML report: ${GREEN}open $COVER_DIR/index.html${NC}"
echo -e "  3. Review COVERAGE_REPORT.md for detailed analysis"
echo ""

exit 0
