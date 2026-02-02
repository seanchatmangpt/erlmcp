#!/usr/bin/env bash
#=============================================================================
# Comprehensive Test Execution Script for erlmcp_cli
#
# Runs all test suites (EUnit, Common Test, Proper) with coverage reporting
#
# Usage:
#   ./scripts/run_tests.sh [options]
#
# Options:
#   -eunit      Run EUnit tests only
#   -ct         Run Common Test suites only
#   -proper     Run Proper tests only
#   -coverage   Generate coverage report
#   -verbose    Verbose output
#   -help       Show this help
#
# Chicago TDD Methodology:
# - All tests use real processes (NO MOCKS)
# - State-based verification only
# - 80%+ minimum coverage required
#=============================================================================

set -e

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_DIR="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test counters
EUNIT_TESTS=0
CT_TESTS=0
PROPER_TESTS=0
TOTAL_TESTS=0
FAILED_TESTS=0

# Options
RUN_EUNIT=false
RUN_CT=false
RUN_PROPER=false
GENERATE_COVERAGE=false
VERBOSE=false

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

print_header() {
    echo ""
    echo -e "${BLUE}============================================================================${NC}"
    echo -e "${BLUE}  $1${NC}"
    echo -e "${BLUE}============================================================================${NC}"
    echo ""
}

show_help() {
    cat << EOF
Usage: $0 [options]

Options:
  -eunit       Run EUnit tests only
  -ct          Run Common Test suites only
  -proper      Run Proper tests only
  -coverage    Generate coverage report
  -verbose     Verbose output
  -help        Show this help message

If no options specified, runs all test suites (EUnit, CT, Proper).

Examples:
  $0                    # Run all tests
  $0 -eunit -coverage   # Run EUnit tests with coverage
  $0 -ct -verbose       # Run Common Test suites with verbose output

EOF
}

#=============================================================================
# Test Execution Functions
#=============================================================================

run_eunit_tests() {
    print_header "Running EUnit Tests"

    cd "$APP_DIR"

    log_info "Starting EUnit test execution..."

    if [ "$VERBOSE" = true ]; then
        rebar3 eunit --verbose 2>&1 | tee eunit.log
    else
        rebar3 eunit 2>&1 | tee eunit.log
    fi

    EUNIT_EXIT_CODE=${PIPESTATUS[0]}

    # Count tests
    if [ -f eunit.log ]; then
        EUNIT_TESTS=$(grep -o "Test passed" eunit.log | wc -l || echo "0")
    fi

    if [ $EUNIT_EXIT_CODE -eq 0 ]; then
        log_success "EUnit tests passed ($EUNIT_TESTS tests)"
    else
        log_error "EUnit tests failed with exit code $EUNIT_EXIT_CODE"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi

    TOTAL_TESTS=$((TOTAL_TESTS + EUNIT_TESTS))
}

run_ct_tests() {
    print_header "Running Common Test Suites"

    cd "$APP_DIR"

    log_info "Starting Common Test execution..."

    if [ "$VERBOSE" = true ]; then
        rebar3 ct --verbose 2>&1 | tee ct.log
    else
        rebar3 ct 2>&1 | tee ct.log
    fi

    CT_EXIT_CODE=${PIPESTATUS[0]}

    # Count tests
    if [ -f ct.log ]; then
        CT_TESTS=$(grep -o "\[.*\]" ct.log | wc -l || echo "0")
    fi

    if [ $CT_EXIT_CODE -eq 0 ]; then
        log_success "Common Test suites passed ($CT_TESTS tests)"
    else
        log_error "Common Test suites failed with exit code $CT_EXIT_CODE"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi

    TOTAL_TESTS=$((TOTAL_TESTS + CT_TESTS))
}

run_proper_tests() {
    print_header "Running Proper Property-Based Tests"

    cd "$APP_DIR"

    log_info "Starting Proper test execution..."

    if [ "$VERBOSE" = true ]; then
        rebar3 proper --verbose 2>&1 | tee proper.log
    else
        rebar3 proper 2>&1 | tee proper.log
    fi

    PROPER_EXIT_CODE=${PIPESTATUS[0]}

    # Count properties
    if [ -f proper.log ]; then
        PROPER_TESTS=$(grep -o "Property" proper.log | wc -l || echo "0")
    fi

    if [ $PROPER_EXIT_CODE -eq 0 ]; then
        log_success "Proper tests passed ($PROPER_TESTS properties)"
    else
        log_error "Proper tests failed with exit code $PROPER_EXIT_CODE"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi

    TOTAL_TESTS=$((TOTAL_TESTS + PROPER_TESTS))
}

generate_coverage_report() {
    print_header "Generating Coverage Report"

    cd "$APP_DIR"

    log_info "Analyzing test coverage..."

    # Generate coverage
    rebar3 cover --verbose 2>&1 | tee coverage.log

    COVERAGE_EXIT_CODE=$?

    if [ $COVERAGE_EXIT_CODE -eq 0 ]; then
        log_success "Coverage report generated"

        # Extract coverage percentage
        if [ -f coverage.log ]; then
            COVERAGE_PCT=$(grep -o "[0-9]*\%" coverage.log | head -1 || echo "N/A")
            log_info "Overall coverage: $COVERAGE_PCT"

            # Check minimum coverage (80%)
            if [[ "$COVERAGE_PCT" =~ ^[0-9]+$ ]]; then
                COVERAGE_NUM=${COVERAGE_PCT%\%}
                if [ $COVERAGE_NUM -lt 80 ]; then
                    log_warning "Coverage below 80% threshold: $COVERAGE_PCT"
                    FAILED_TESTS=$((FAILED_TESTS + 1))
                else
                    log_success "Coverage meets 80% threshold: $COVERAGE_PCT"
                fi
            fi
        fi

        # Show HTML report location
        log_info "HTML coverage report: $APP_DIR/_build/test/cover/index.html"
    else
        log_error "Coverage generation failed with exit code $COVERAGE_EXIT_CODE"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
}

#=============================================================================
# Main Execution
#=============================================================================

main() {
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -eunit)
                RUN_EUNIT=true
                shift
                ;;
            -ct)
                RUN_CT=true
                shift
                ;;
            -proper)
                RUN_PROPER=true
                shift
                ;;
            -coverage)
                GENERATE_COVERAGE=true
                shift
                ;;
            -verbose)
                VERBOSE=true
                shift
                ;;
            -help|--help|-h)
                show_help
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done

    # If no specific test suite selected, run all
    if [ "$RUN_EUNIT" = false ] && [ "$RUN_CT" = false ] && [ "$RUN_PROPER" = false ]; then
        RUN_EUNIT=true
        RUN_CT=true
        RUN_PROPER=true
    fi

    print_header "erlmcp_cli Comprehensive Test Suite"
    log_info "Starting test execution..."
    log_info "Test suites: EUnit=$RUN_EUNIT, CT=$RUN_CT, Proper=$RUN_PROPER"
    log_info "Coverage: $GENERATE_COVERAGE"

    # Start time
    START_TIME=$(date +%s)

    # Run selected test suites
    if [ "$RUN_EUNIT" = true ]; then
        run_eunit_tests
    fi

    if [ "$RUN_CT" = true ]; then
        run_ct_tests
    fi

    if [ "$RUN_PROPER" = true ]; then
        run_proper_tests
    fi

    # Generate coverage if requested
    if [ "$GENERATE_COVERAGE" = true ]; then
        generate_coverage_report
    fi

    # End time
    END_TIME=$(date +%s)
    DURATION=$((END_TIME - START_TIME))

    # Print summary
    print_header "Test Execution Summary"
    log_info "Total tests:  $TOTAL_TESTS"
    log_info "EUnit tests:  $EUNIT_TESTS"
    log_info "CT tests:     $CT_TESTS"
    log_info "Proper tests: $PROPER_TESTS"
    log_info "Failed:       $FAILED_TESTS"
    log_info "Duration:     ${DURATION}s"

    if [ $FAILED_TESTS -eq 0 ]; then
        log_success "All tests passed!"
        exit 0
    else
        log_error "Some tests failed. See logs above."
        exit 1
    fi
}

# Run main
main "$@"
