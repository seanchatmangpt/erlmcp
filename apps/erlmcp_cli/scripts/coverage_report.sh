#!/usr/bin/env bash
#=============================================================================
# Coverage Report Generation Script for erlmcp_cli
#
# Generates detailed coverage reports with analysis and recommendations
#
# Usage:
#   ./scripts/coverage_report.sh [options]
#
# Options:
#   -html       Generate HTML report (default)
#   -text       Generate text report
#   -threshold  Set minimum coverage threshold (default: 80%)
#   -modules    Show coverage by module
#   -functions  Show coverage by function
#=============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_DIR="$(dirname "$SCRIPT_DIR")"

# Default options
OUTPUT_FORMAT="html"
THRESHOLD=80
SHOW_MODULES=false
SHOW_FUNCTIONS=false

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

while [[ $# -gt 0 ]]; do
    case $1 in
        -html)
            OUTPUT_FORMAT="html"
            shift
            ;;
        -text)
            OUTPUT_FORMAT="text"
            shift
            ;;
        -threshold)
            THRESHOLD="$2"
            shift 2
            ;;
        -modules)
            SHOW_MODULES=true
            shift
            ;;
        -functions)
            SHOW_FUNCTIONS=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

cd "$APP_DIR"

echo -e "${BLUE}============================================================================${NC}"
echo -e "${BLUE}  Coverage Report Generation${NC}"
echo -e "${BLUE}============================================================================${NC}"
echo ""

# Generate coverage
echo -e "${BLUE}[INFO]${NC} Running tests with coverage..."
rebar3 cover --verbose > coverage_generation.log 2>&1

# Extract coverage data
echo ""
echo -e "${BLUE}============================================================================${NC}"
echo -e "${BLUE}  Coverage Summary${NC}"
echo -e "${BLUE}============================================================================${NC}"
echo ""

# Parse coverage.log for coverage percentages
if [ -f coverage_generation.log ]; then
    # Extract overall coverage
    COVERAGE_LINE=$(grep -E "^[0-9]+(\.[0-9]+)?%" coverage_generation.log | tail -1 || echo "")

    if [ -n "$COVERAGE_LINE" ]; then
        COVERAGE_PCT=$(echo "$COVERAGE_LINE" | grep -o "[0-9]*\.[0-9]*%" | head -1)
        echo "Overall Coverage: $COVERAGE_PCT"

        # Check threshold
        COVERAGE_NUM=${COVERAGE_PCT%\%}
        COVERAGE_INT=${COVERAGE_NUM%.*}

        if [ $COVERAGE_INT -lt $THRESHOLD ]; then
            echo -e "${RED}[FAIL]${NC} Coverage ${COVERAGE_PCT} below threshold ${THRESHOLD}%"
        else
            echo -e "${GREEN}[PASS]${NC} Coverage ${COVERAGE_PCT} meets threshold ${THRESHOLD}%"
        fi
    fi

    # Show coverage by module if requested
    if [ "$SHOW_MODULES" = true ]; then
        echo ""
        echo "Coverage by Module:"
        echo "-------------------"

        # Parse module coverage
        grep -E "^  [a-z_]+\.erl" coverage_generation.log | while read -r line; do
            MODULE=$(echo "$line" | awk '{print $1}')
            PCT=$(echo "$line" | grep -o "[0-9]*\.[0-9]*%" || echo "N/A")
            echo "  $MODULE: $PCT"
        done
    fi

    # Show coverage by function if requested
    if [ "$SHOW_FUNCTIONS" = true ]; then
        echo ""
        echo "Coverage by Function:"
        echo "---------------------"

        # Parse function coverage
        grep -E "^    [a-z_]+/[0-9]+" coverage_generation.log | while read -r line; do
            FUNCTION=$(echo "$line" | awk '{print $1}')
            PCT=$(echo "$line" | grep -o "[0-9]*\.[0-9]*%" || echo "N/A")
            echo "  $FUNCTION: $PCT"
        done
    fi
fi

echo ""

# HTML report location
HTML_REPORT="$APP_DIR/_build/test/cover/index.html"
if [ -f "$HTML_REPORT" ]; then
    echo -e "${BLUE}[INFO]${NC} HTML coverage report: $HTML_REPORT"

    # Try to open in browser (macOS only)
    if [[ "$OSTYPE" == "darwin"* ]]; then
        echo -e "${BLUE}[INFO]${NC} Opening report in browser..."
        open "$HTML_REPORT" 2>/dev/null || true
    fi
fi

echo ""
echo -e "${BLUE}============================================================================${NC}"
echo -e "${BLUE}  Coverage Report Complete${NC}"
echo -e "${BLUE}============================================================================${NC}"
