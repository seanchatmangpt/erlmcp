#!/bin/bash

# ===================================================================
# ERLMCP TEST QUALITY METRICS DASHBOARD
# Comprehensive test quality metrics and visualization
# ===================================================================

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}📊 ERLMCP TEST QUALITY METRICS DASHBOARD${NC}"
echo "=============================================="
echo ""

# Function to create metrics
create_metrics() {
    # Count total test files
    TOTAL_TESTS=$(find apps -name "*.erl" -path "*/test/*" | wc -l)

    # Count skipped tests
    SKIPPED_TESTS=$(find apps -name "*.skip" | wc -l)

    # Count broken tests
    BROKEN_TESTS=$(find apps -name "*.broken" | wc -l)

    # Count hardcoded timeouts
    TIMEOUT_FILES=$(find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | wc -l)

    # Count test files with mocks
    MOCK_FILES=$(find apps -name "*.erl" -exec grep -l "meck\|mock" {} \; | wc -l)

    # Count test files with concurrent patterns
    CONCURRENT_FILES=$(find apps -name "*.erl" -exec grep -l "spawn\|concurrent" {} \; | wc -l)

    # Count test files with property tests
    PROPERTY_FILES=$(find apps -name "*.erl" -exec grep -l "proper\|prop_" {} \; | wc -l)

    # Calculate active tests
    ACTIVE_TESTS=$((TOTAL_TESTS - SKIPPED_TESTS - BROKEN_TESTS))

    # Calculate coverage score (placeholder)
    COVERAGE_SCORE=$(calculate_coverage_score)

    # Calculate quality score
    QUALITY_SCORE=$(( (ACTIVE_TESTS * 40 + CONCURRENT_FILES * 20 + PROPERTY_FILES * 20 + (100 - COVERAGE_SCORE) * 20) / 100 ))

    echo "Total Test Files: $TOTAL_TESTS"
    echo "Active Tests: $ACTIVE_TESTS"
    echo "Skipped Tests: $SKIPPED_TESTS"
    echo "Broken Tests: $BROKEN_TESTS"
    echo "Files with Hardcoded Timeouts: $TIMEOUT_FILES"
    echo "Files with Mocks: $MOCK_FILES"
    echo "Files with Concurrent Patterns: $CONCURRENT_FILES"
    echo "Files with Property Tests: $PROPERTY_FILES"
    echo "Coverage Score: $COVERAGE_SCORE%"
    echo "Quality Score: $QUALITY_SCORE%"
}

# Function to calculate coverage score
calculate_coverage_score() {
    if [ -f "_build/test/cover/index.html" ]; then
        COVERAGE=$(grep -o '[0-9]*%' _build/test/cover/index.html | head -1 | sed 's/%//')
        if [ -n "$COVERAGE" ]; then
            echo $COVERAGE
            return
        fi
    fi
    echo "0"
}

# Function to show quality radar chart
show_quality_radar() {
    echo -e "${PURPLE}📈 QUALITY RADAR CHART${NC}"
    echo "------------------------"

    # Get metrics
    TOTAL_TESTS=$(find apps -name "*.erl" -path "*/test/*" | wc -l)
    ACTIVE_TESTS=$((TOTAL_TESTS - $(find apps -name "*.skip" | wc -l) - $(find apps -name "*.broken" | wc -l)))
    CONCURRENT=$(find apps -name "*.erl" -exec grep -l "spawn\|concurrent" {} \; | wc -l)
    PROPS=$(find apps -name "*.erl" -exec grep -l "proper\|prop_" {} \; | wc -l)
    TIMEOUTS=$(find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | wc -l)

    # Calculate percentages
    ACTIVE_PERCENT=$(( ACTIVE_TESTS * 100 / $TOTAL_TESTS ))
    CONCURRENT_PERCENT=$(( CONCURRENT * 100 / $TOTAL_TESTS ))
    PROPS_PERCENT=$(( PROPS * 100 / $TOTAL_TESTS ))
    TIMEOUTS_PERCENT=$(( TIMEOUTS * 100 / $TOTAL_TESTS ))

    # Show radar chart
    echo "Test Coverage: $(printf '%-3d%%' $ACTIVE_PERCENT)  |$(printf '%*s' $ACTIVE_PERCENT '█')|"
    echo "Concurrency:   $(printf '%-3d%%' $CONCURRENT_PERCENT)  |$(printf '%*s' $CONCURRENT_PERCENT '█')|"
    echo "Property Tests:$(printf '%-3d%%' $PROPS_PERCENT)  |$(printf '%*s' $PROPS_PERCENT '█')|"
    echo "No Timeouts:   $(printf '%-3d%%' $((100 - TIMEOUTS_PERCENT)))  |$(printf '%*s' $((100 - TIMEOUTS_PERCENT)) '█')|"
}

# Function to show test distribution
show_test_distribution() {
    echo -e "${BLUE}📋 TEST DISTRIBUTION${NC}"
    echo "--------------------"

    # Count tests by app
    for app in apps/*/test; do
        if [ -d "$app" ]; then
            app_name=$(basename $(dirname "$app"))
            test_count=$(find "$app" -name "*.erl" | wc -l)
            echo "$app_name: $test_count tests"
        fi
    done
}

# Function to show test quality issues
show_quality_issues() {
    echo -e "${YELLOW}🔍 QUALITY ISSUES${NC}"
    echo "-----------------"

    # Skipped tests
    if [ $(find apps -name "*.skip" | wc -l) -gt 0 ]; then
        echo "Skipped Tests:"
        find apps -name "*.skip" | head -5 | while read file; do
            echo "  - $(basename $file)"
        done
    fi

    # Hardcoded timeouts
    if [ $(find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | wc -l) -gt 0 ]; then
        echo ""
        echo "Files with Hardcoded Timeouts:"
        find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | head -5 | while read file; do
            timeout_count=$(grep -c "timer:sleep" "$file")
            echo "  - $(basename $file): $timeout_count timeouts"
        done
    fi

    # Mock usage
    if [ $(find apps -name "*.erl" -exec grep -l "meck\|mock" {} \; | wc -l) -gt 0 ]; then
        echo ""
        echo "Files with Mocks:"
        find apps -name "*.erl" -exec grep -l "meck\|mock" {} \; | head -5 | while read file; do
            echo "  - $(basename $file)"
        done
    fi
}

# Function to generate recommendations
generate_recommendations() {
    echo -e "${GREEN}💡 RECOMMENDATIONS${NC}"
    echo "------------------"

    TOTAL_TESTS=$(find apps -name "*.erl" -path "*/test/*" | wc -l)
    ACTIVE_TESTS=$((TOTAL_TESTS - $(find apps -name "*.skip" | wc -l) - $(find apps -name "*.broken" | wc -l)))
    CONCURRENT=$(find apps -name "*.erl" -exec grep -l "spawn\|concurrent" {} \; | wc -l)
    PROPS=$(find apps -name "*.erl" -exec grep -l "proper\|prop_" {} \; | wc -l)

    # Recommend based on metrics
    if [ $(find apps -name "*.skip" | wc -l) -gt 0 ]; then
        echo "🎯 Address skipped tests to improve coverage"
    fi

    if [ $CONCURRENT -lt $((TOTAL_TESTS / 4)) ]; then
        echo "🎯 Add concurrent tests for $((TOTAL_TESTS / 4)) modules"
    fi

    if [ $PROPS -eq 0 ]; then
        echo "🎯 Implement property testing for critical modules"
    fi

    if [ $(find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | wc -l) -gt 0 ]; then
        echo "🎯 Replace hardcoded timeouts with adaptive timing"
    fi
}

# Main execution
create_metrics
echo ""
show_quality_radar
echo ""
show_test_distribution
echo ""
show_quality_issues
echo ""
generate_recommendations

# Calculate overall health score
echo ""
echo -e "${CYAN}🏥 OVERALL HEALTH SCORE: $((ACTIVE_TESTS * 100 / TOTAL_TESTS))%${NC}"