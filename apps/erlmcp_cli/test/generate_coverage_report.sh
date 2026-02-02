#!/usr/bin/env bash
#=============================================================================
# Coverage Report Generator for erlmcp_cli
#
# Generates comprehensive coverage reports with:
# - Overall coverage percentage
# - Per-module coverage breakdown
# - HTML report generation
# - Coverage gap analysis
# - Trend analysis
#
# Chicago School TDD Quality Gate:
# - Minimum 80% coverage (all modules)
# - Minimum 85% coverage (core modules)
# - Fails build if thresholds not met
#
# Usage:
#   ./generate_coverage_report.sh [options]
#
# Options:
#   -html        Generate HTML report
#   -text        Generate text report
#   -modules     Generate module breakdown
#   -gaps        Identify coverage gaps
#   -trend       Analyze coverage trends
#   -ci          CI mode (exit on threshold failure)
#=============================================================================

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Options
GENERATE_HTML=false
GENERATE_TEXT=false
MODULE_BREAKDOWN=false
GAP_ANALYSIS=false
TREND_ANALYSIS=false
CI_MODE=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -html)
            GENERATE_HTML=true
            shift
            ;;
        -text)
            GENERATE_TEXT=true
            shift
            ;;
        -modules)
            MODULE_BREAKDOWN=true
            shift
            ;;
        -gaps)
            GAP_ANALYSIS=true
            shift
            ;;
        -trend)
            TREND_ANALYSIS=true
            shift
            ;;
        -ci)
            CI_MODE=true
            shift
            ;;
        *)
            echo "Usage: $0 [-html] [-text] [-modules] [-gaps] [-trend] [-ci]"
            exit 1
            ;;
    esac
done

# Default to text output if no options specified
if [ "$GENERATE_HTML" = false ] && [ "$GENERATE_TEXT" = false ] && \
   [ "$MODULE_BREAKDOWN" = false ] && [ "$GAP_ANALYSIS" = false ] && \
   [ "$TREND_ANALYSIS" = false ]; then
    GENERATE_TEXT=true
    MODULE_BREAKDOWN=true
fi

cd "$(dirname "$0")/../../.."

#=============================================================================
# Helper Functions
#=============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Extract coverage percentage from cover output
extract_coverage() {
    local module=$1
    local output=$(rebar3 cover --verbose | grep "$module" || echo "0%")
    echo "$output" | grep -oE "[0-9]+(\.[0-9]+)?%" | head -1 | sed 's/%//'
}

# Get list of all modules
get_modules() {
    find apps/erlmcp_cli/src -name "*.erl" -type f | \
        sed 's|apps/erlmcp_cli/src/||' | \
        sed 's|\.erl$||' | \
        sort
}

# Core modules that require 85%+ coverage
CORE_MODULES=(
    "erlmcp_cli_json_rpc"
    "erlmcp_cli_session"
    "erlmcp_cli_transport"
    "erlmcp_cli_registry"
    "erlmcp_cli_auth"
    "erlmcp_cli_config"
)

#=============================================================================
# Run Tests with Coverage
#=============================================================================

log_info "Running tests with coverage..."
rebar3 cover --verbose > /dev/null 2>&1 || true

#=============================================================================
# Generate Text Report
#=============================================================================

if [ "$GENERATE_TEXT" = true ]; then
    log_info "Generating text coverage report..."
    echo ""
    echo "=========================================="
    echo "erlmcp_cli Coverage Report"
    echo "=========================================="
    echo ""

    # Get overall coverage
    OVERALL=$(rebar3 cover --verbose 2>&1 | grep -E "^[0-9]+(\.[0-9]+)?%" | head -1 || echo "N/A")
    echo "Overall Coverage: $OVERALL"
    echo ""

    # Check threshold
    COVERAGE_PCT=$(echo "$OVERALL" | grep -oE "[0-9]+(\.[0-9]+)?" | head -1)

    if [ -n "$COVERAGE_PCT" ]; then
        COVERAGE_INT=$(printf "%.0f" "$COVERAGE_PCT")

        if [ "$COVERAGE_INT" -ge 85 ]; then
            log_success "Coverage: ${COVERAGE_PCT}% (Excellent, exceeds 85% target)"
        elif [ "$COVERAGE_INT" -ge 80 ]; then
            log_success "Coverage: ${COVERAGE_PCT}% (Meets 80% minimum)"
        else
            log_error "Coverage: ${COVERAGE_PCT}% (Below 80% minimum)"
            if [ "$CI_MODE" = true ]; then
                exit 1
            fi
        fi
    fi
fi

#=============================================================================
# Module Breakdown
#=============================================================================

if [ "$MODULE_BREAKDOWN" = true ]; then
    echo ""
    echo "=========================================="
    echo "Module Coverage Breakdown"
    echo "=========================================="
    echo ""

    MODULES=$(get_modules)
    TOTAL_COVERAGE=0
    MODULE_COUNT=0

    for module in $MODULES; do
        COVERAGE=$(extract_coverage "$module")

        if [ -n "$COVERAGE" ]; then
            # Check if core module
            IS_CORE=false
            for core in "${CORE_MODULES[@]}"; do
                if [ "$module" = "$core" ]; then
                    IS_CORE=true
                    break
                fi
            done

            # Color code based on coverage
            COVERAGE_INT=$(printf "%.0f" "$COVERAGE")
            if [ "$IS_CORE" = true ]; then
                if [ "$COVERAGE_INT" -ge 85 ]; then
                    COLOR="$GREEN"
                else
                    COLOR="$RED"
                fi
            else
                if [ "$COVERAGE_INT" -ge 80 ]; then
                    COLOR="$GREEN"
                elif [ "$COVERAGE_INT" -ge 70 ]; then
                    COLOR="$YELLOW"
                else
                    COLOR="$RED"
                fi
            fi

            echo -e "${COLOR}${COVERAGE}%${NC} - $module"

            TOTAL_COVERAGE=$(echo "$TOTAL_COVERAGE + $COVERAGE" | bc)
            MODULE_COUNT=$((MODULE_COUNT + 1))
        fi
    done

    # Calculate average
    if [ $MODULE_COUNT -gt 0 ]; then
        AVERAGE=$(echo "scale=2; $TOTAL_COVERAGE / $MODULE_COUNT" | bc)
        echo ""
        echo "Average Coverage: ${AVERAGE}%"
    fi
fi

#=============================================================================
# Gap Analysis
#=============================================================================

if [ "$GAP_ANALYSIS" = true ]; then
    echo ""
    echo "=========================================="
    echo "Coverage Gap Analysis"
    echo "=========================================="
    echo ""

    MODULES=$(get_modules)

    for module in $MODULES; do
        COVERAGE=$(extract_coverage "$module")

        if [ -n "$COVERAGE" ]; then
            COVERAGE_INT=$(printf "%.0f" "$COVERAGE")

            # Check if core module
            IS_CORE=false
            THRESHOLD=80
            for core in "${CORE_MODULES[@]}"; do
                if [ "$module" = "$core" ]; then
                    IS_CORE=true
                    THRESHOLD=85
                    break
                fi
            done

            # Identify gaps
            if [ "$COVERAGE_INT" -lt "$THRESHOLD" ]; then
                GAP=$(echo "$THRESHOLD - $COVERAGE" | bc)
                if [ "$IS_CORE" = true ]; then
                    log_error "[$module] Gap: ${GAP}% (Current: ${COVERAGE}%, Target: ${THRESHOLD}%) [CORE]"
                else
                    log_warning "[$module] Gap: ${GAP}% (Current: ${COVERAGE}%, Target: ${THRESHOLD}%)"
                fi
            fi
        fi
    done
fi

#=============================================================================
# Generate HTML Report
#=============================================================================

if [ "$GENERATE_HTML" = true ]; then
    log_info "Generating HTML coverage report..."

    # Cover tool already generates HTML
    # Open it for user
    HTML_REPORT="_build/test/cover/index.html"

    if [ -f "$HTML_REPORT" ]; then
        log_success "HTML report generated: $HTML_REPORT"
        echo ""
        log_info "View HTML report:"
        echo "  open $HTML_REPORT"
    else
        log_error "HTML report not found"
    fi
fi

#=============================================================================
# Trend Analysis
#=============================================================================

if [ "$TREND_ANALYSIS" = true ]; then
    echo ""
    echo "=========================================="
    echo "Coverage Trend Analysis"
    echo "=========================================="
    echo ""

    HISTORY_FILE=".coverage_history"

    if [ -f "$HISTORY_FILE" ]; then
        log_info "Coverage history:"
        cat "$HISTORY_FILE"
        echo ""

        # Calculate trend
        LATEST=$(tail -1 "$HISTORY_FILE" | awk '{print $2}' | sed 's/%//')
        PREVIOUS=$(tail -2 "$HISTORY_FILE" | head -1 | awk '{print $2}' | sed 's/%//')

        if [ -n "$LATEST" ] && [ -n "$PREVIOUS" ]; then
            TREND=$(echo "$LATEST - $PREVIOUS" | bc)

            if [ $(echo "$TREND > 0" | bc) -eq 1 ]; then
                log_success "Trend: +${TREND}% (improving)"
            elif [ $(echo "$TREND < 0" | bc) -eq 1 ]; then
                log_error "Trend: ${TREND}% (declining)"
            else
                log_info "Trend: 0% (stable)"
            fi
        fi
    else
        log_warning "No coverage history found"
    fi

    # Record current coverage
    OVERALL=$(rebar3 cover --verbose 2>&1 | grep -E "^[0-9]+(\.[0-9]+)?%" | head -1 || echo "N/A")
    DATE=$(date +"%Y-%m-%d %H:%M:%S")
    echo "$DATE $OVERALL" >> "$HISTORY_FILE"
fi

#=============================================================================
# CI Mode
#=============================================================================

if [ "$CI_MODE" = true ]; then
    echo ""
    log_info "CI Mode: Checking coverage thresholds..."

    MODULES=$(get_modules)
    FAILED=0

    for module in $MODULES; do
        COVERAGE=$(extract_coverage "$module")

        if [ -n "$COVERAGE" ]; then
            COVERAGE_INT=$(printf "%.0f" "$COVERAGE")

            # Check if core module
            IS_CORE=false
            THRESHOLD=80
            for core in "${CORE_MODULES[@]}"; do
                if [ "$module" = "$core" ]; then
                    IS_CORE=true
                    THRESHOLD=85
                    break
                fi
            done

            if [ "$COVERAGE_INT" -lt "$THRESHOLD" ]; then
                log_error "[$module] Coverage ${COVERAGE}% below ${THRESHOLD}% threshold"
                FAILED=1
            fi
        fi
    done

    if [ $FAILED -eq 1 ]; then
        log_error "Coverage thresholds not met"
        exit 1
    else
        log_success "All coverage thresholds met"
    fi
fi

echo ""
log_success "Coverage report generation complete!"
echo ""

exit 0
