#!/bin/bash
#
# Test Consolidation Analysis Helper
# Helps identify consolidation opportunities in erlmcp test suite
#
# Usage:
#   ./tools/consolidation_analysis_helper.sh              # Full analysis
#   ./tools/consolidation_analysis_helper.sh --large-files # Show files >500 lines
#   ./tools/consolidation_analysis_helper.sh --duplicates  # Find duplicate test patterns
#   ./tools/consolidation_analysis_helper.sh --timing      # Find timer:sleep usage
#   ./tools/consolidation_analysis_helper.sh --properties  # Show property test adoption
#

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Root directory
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

echo "╔══════════════════════════════════════════════════════════════════════════════╗"
echo "║         Test Consolidation Analysis Helper - erlmcp Test Suite             ║"
echo "╚══════════════════════════════════════════════════════════════════════════════╝"
echo ""

# Function: Show large test files
show_large_files() {
    echo -e "${BLUE}📏 Large Test Files (>500 lines)${NC}"
    echo "═════════════════════════════════════════════════════════════════════════════"
    find apps -name "*_tests.erl" -type f -exec sh -c 'echo "$(wc -l < "$1") $1"' _ {} \; | \
        awk '$1 > 500 {print $0}' | \
        sort -rn | \
        awk '{
            lines=$1
            file=$2
            gsub(/^apps\//, "", file)
            if (lines > 1000) {
                printf "  ❌ %6d lines  %s\n", lines, file
            } else {
                printf "  ⚠️  %6d lines  %s\n", lines, file
            }
        }'
    echo ""
}

# Function: Find duplicate test patterns
find_duplicates() {
    echo -e "${BLUE}🔍 Potential Test Duplication${NC}"
    echo "═════════════════════════════════════════════════════════════════════════════"

    # Find similar test function names across files
    echo "Similar test function names:"
    grep -h "^[a-z_]*test\(\)" apps/*/test/*_tests.erl 2>/dev/null | \
        sort | uniq -c | sort -rn | \
        awk '$1 > 1 {printf "  ⚠️  %2d occurrences:  %s\n", $1, $2}' | head -10
    echo ""
}

# Function: Find timer:sleep usage
find_timing_issues() {
    echo -e "${BLUE}⏱️  Timing Dependencies (timer:sleep)${NC}"
    echo "═════════════════════════════════════════════════════════════════════════════"

    total=$(grep -r "timer:sleep" apps/*/test/*.erl 2>/dev/null | wc -l | tr -d ' ')

    if [ "$total" -gt 0 ]; then
        echo "Found $total timer:sleep calls across test files:"
        grep -rn "timer:sleep" apps/*/test/*.erl 2>/dev/null | \
            awk -F: '{printf "  • %s:%d\n", $1, $2}' | \
            sed 's|apps/||' | sed 's|/test/| |' | head -20

        if [ "$total" -gt 20 ]; then
            echo ""
            echo -e "${RED}❌ HIGH MAINTENANCE BURDEN: $total timer:sleep calls found${NC}"
            echo "   Target: <20 calls (current: $total)"
            echo "   Recommendation: Replace with sync assertions or timeouts"
        fi
    else
        echo -e "${GREEN}✅ No timer:sleep calls found (excellent!)${NC}"
    fi
    echo ""
}

# Function: Show property test adoption
show_property_adoption() {
    echo -e "${BLUE}🎲 Property Test Adoption (Proper)${NC}"
    echo "═════════════════════════════════════════════════════════════════════════════"

    prop_count=$(grep -r "proper\|?FORALL" apps/*/test/*.erl 2>/dev/null | wc -l | tr -d ' ')
    test_files=$(find apps -name "*_tests.erl" -type f | wc -l | tr -d ' ')

    echo "Property test usage: $prop_count occurrences across $test_files test files"

    if [ "$test_files" -gt 0 ]; then
        adoption=$((prop_count * 100 / test_files))
        echo "Adoption rate: ${adoption}% (target: 20%+)"

        if [ "$adoption" -lt 5 ]; then
            echo -e "${YELLOW}⚠️  LOW PROPERTY TEST ADOPTION${NC}"
            echo "   Consider converting repetitive tests to property tests"
        fi
    fi
    echo ""

    # Show files with property tests
    echo "Files with property tests:"
    grep -l "proper\|?FORALL" apps/*/test/*.erl 2>/dev/null | \
        while read file; do
            count=$(grep -c "?FORALL\|proper_types" "$file" 2>/dev/null || echo 0)
            if [ "$count" -gt 0 ]; then
                printf "  • %s (%d properties)\n" "$(basename "$file")" "$count"
            fi
        done | head -10
    echo ""
}

# Function: Show mock usage (Chicago School validation)
show_mock_usage() {
    echo -e "${BLUE}🎭 Mock Usage (Chicago School TDD Check)${NC}"
    echo "═════════════════════════════════════════════════════════════════════════════"

    meck_count=$(grep -r "meck:" apps/*/test/*.erl 2>/dev/null | wc -l | tr -d ' ')

    if [ "$meck_count" -eq 0 ]; then
        echo -e "${GREEN}✅ EXCELLENT: No mock objects detected (Chicago School TDD)${NC}"
        echo "   All tests use real processes and state-based verification"
    else
        echo -e "${YELLOW}⚠️  Found $meck_count mock usages${NC}"
        echo "   Consider using real processes instead of mocks"
    fi
    echo ""
}

# Function: Show summary statistics
show_summary() {
    echo -e "${BLUE}📊 Test Suite Summary${NC}"
    echo "═════════════════════════════════════════════════════════════════════════════"

    test_files=$(find apps -name "*_tests.erl" -type f | wc -l | tr -d ' ')
    ct_suites=$(find apps -name "*_SUITE.erl" -type f | wc -l | tr -d ' ')
    total_loc=$(find apps -name "*_tests.erl" -o -name "*_SUITE.erl" | xargs wc -l 2>/dev/null | tail -1 | awk '{print $1}')

    echo "  EUnit test files:    $test_files"
    echo "  Common Test suites:  $ct_suites"
    echo "  Total test files:    $((test_files + ct_suites))"
    echo "  Total test LOC:      ${total_loc:-0}"
    echo ""
}

# Main execution
case "${1:-}" in
    --large-files)
        show_large_files
        ;;
    --duplicates)
        find_duplicates
        ;;
    --timing)
        find_timing_issues
        ;;
    --properties)
        show_property_adoption
        ;;
    --mocks)
        show_mock_usage
        ;;
    *)
        show_summary
        show_large_files
        find_timing_issues
        show_property_adoption
        show_mock_usage
        echo -e "${BLUE}💡 Usage${NC}"
        echo "═════════════════════════════════════════════════════════════════════════════"
        echo "  $0 --large-files   Show files >500 lines"
        echo "  $0 --duplicates    Find duplicate test patterns"
        echo "  $0 --timing        Find timer:sleep usage"
        echo "  $0 --properties    Show property test adoption"
        echo "  $0 --mocks         Validate Chicago School TDD (no mocks)"
        echo ""
        ;;
esac
