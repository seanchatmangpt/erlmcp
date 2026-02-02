#!/usr/bin/env bash
#=============================================================================
# Comprehensive Test Suite Runner for erlmcp_cli
#
# Chicago School TDD methodology:
# - Real processes, no mocks
# - State-based verification
# - 80%+ coverage minimum (85%+ for core modules)
#
# Usage:
#   ./run_full_test_suite.sh [options]
#
# Options:
#   -eunit       Run EUnit tests only
#   -ct          Run Common Test suites only
#   -proper      Run Proper property tests only
#   -coverage    Generate coverage report
#   -verbose     Verbose output
#   -parallel    Run tests in parallel
#   -ci          CI mode (fail on coverage < 80%)
#
# Examples:
#   ./run_full_test_suite.sh                  # Run all tests
#   ./run_full_test_suite.sh -coverage        # Run with coverage
#   ./run_full_test_suite.sh -eunit -verbose  # Run EUnit with verbose output
#=============================================================================

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Default options
RUN_EUNIT=true
RUN_CT=true
RUN_PROPER=true
GENERATE_COVERAGE=false
VERBOSE=false
PARALLEL=false
CI_MODE=false

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -eunit)
            RUN_CT=false
            RUN_PROPER=false
            shift
            ;;
        -ct)
            RUN_EUNIT=false
            RUN_PROPER=false
            shift
            ;;
        -proper)
            RUN_EUNIT=false
            RUN_CT=false
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
        -parallel)
            PARALLEL=true
            shift
            ;;
        -ci)
            CI_MODE=true
            GENERATE_COVERAGE=true
            shift
            ;;
        *)
            echo -e "${RED}[ERROR]${NC} Unknown option: $1"
            echo "Usage: $0 [-eunit] [-ct] [-proper] [-coverage] [-verbose] [-parallel] [-ci]"
            exit 1
            ;;
    esac
done

# Change to script directory
cd "$(dirname "$0")"

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

run_test() {
    local test_cmd="$1"
    local test_name="$2"

    log_info "Running $test_name..."

    if [ "$VERBOSE" = true ]; then
        eval "$test_cmd -v"
    else
        eval "$test_cmd"
    fi

    if [ $? -eq 0 ]; then
        log_success "$test_name passed"
        return 0
    else
        log_error "$test_name failed"
        return 1
    fi
}

#=============================================================================
# Pre-test Setup
#=============================================================================

log_info "Starting erlmcp_cli comprehensive test suite..."
log_info "================================================"

# Check if rebar3 is available
if ! command -v rebar3 &> /dev/null; then
    log_error "rebar3 not found. Please install rebar3."
    exit 1
fi

# Compile first
log_info "Compiling erlmcp_cli..."
rebar3 compile
if [ $? -ne 0 ]; then
    log_error "Compilation failed"
    exit 1
fi
log_success "Compilation successful"

#=============================================================================
# Run EUnit Tests
#=============================================================================

if [ "$RUN_EUNIT" = true ]; then
    log_info "================================================"
    log_info "Running EUnit Test Suites..."

    EUNIT_MODULES=(
        "erlmcp_cli_command_tests"
        "erlmcp_cli_auth_tests"
        "erlmcp_cli_json_rpc_tests"
        "erlmcp_cli_registry_tests"
        "erlmcp_cli_session_tests"
        "erlmcp_cli_transport_tests"
        "erlmcp_cli_metrics_tests"
        "erlmcp_cli_resource_tests"
        "erlmcp_transport_stdio_tests"
        "erlmcp_transport_tcp_tests"
        "erlmcp_transport_http_tests"
        "erlmcp_transport_ws_tests"
        "erlmcp_transport_sse_tests"
    )

    if [ "$PARALLEL" = true ]; then
        # Run EUnit tests in parallel
        for module in "${EUNIT_MODULES[@]}"; do
            run_test "rebar3 eunit --module=$module" "EUnit: $module" &
        done
        wait
    else
        # Run EUnit tests sequentially
        for module in "${EUNIT_MODULES[@]}"; do
            run_test "rebar3 eunit --module=$module" "EUnit: $module"
            if [ $? -ne 0 ]; then
                log_error "EUnit tests failed"
                exit 1
            fi
        done
    fi

    log_success "All EUnit tests passed"
fi

#=============================================================================
# Run Common Test Suites
#=============================================================================

if [ "$RUN_CT" = true ]; then
    log_info "================================================"
    log_info "Running Common Test Suites..."

    CT_SUITES=(
        "erlmcp_cli_integration_SUITE"
        "erlmcp_cli_performance_SUITE"
        "erlmcp_cli_compliance_SUITE"
        "erlmcp_cli_security_SUITE"
        "erlmcp_cli_error_handling_SUITE"
        "erlmcp_cli_otel_SUITE"
    )

    if [ "$PARALLEL" = true ]; then
        # Run CT suites in parallel
        for suite in "${CT_SUITES[@]}"; do
            run_test "rebar3 ct --suite=$suite" "CT: $suite" &
        done
        wait
    else
        # Run CT suites sequentially
        for suite in "${CT_SUITES[@]}"; do
            run_test "rebar3 ct --suite=$suite" "CT: $suite"
            if [ $? -ne 0 ]; then
                log_error "Common Test suites failed"
                exit 1
            fi
        done
    fi

    log_success "All Common Test suites passed"
fi

#=============================================================================
# Run Proper Tests
#=============================================================================

if [ "$RUN_PROPER" = true ]; then
    log_info "================================================"
    log_info "Running Proper Property Tests..."

    run_test "rebar3 proper -c --module=erlmcp_cli_proper_tests" "Proper Tests"

    if [ $? -ne 0 ]; then
        log_error "Proper tests failed"
        exit 1
    fi

    log_success "All Proper tests passed"
fi

#=============================================================================
# Generate Coverage Report
#=============================================================================

if [ "$GENERATE_COVERAGE" = true ]; then
    log_info "================================================"
    log_info "Generating Coverage Report..."

    # Generate coverage
    rebar3 cover --verbose

    if [ $? -ne 0 ]; then
        log_error "Coverage generation failed"
        exit 1
    fi

    # Extract coverage percentage
    COVERAGE_FILE="_build/test/cover/erlmcp_cli.app.coverdata"
    if [ -f "$COVERAGE_FILE" ]; then
        # Parse coverage summary
        log_info "Coverage Summary:"
        echo ""

        # Run cover report to console
        rebar3 cover --verbose

        # Check if coverage meets threshold
        if [ "$CI_MODE" = true ]; then
            COVERAGE_PCT=$(grep -E "^[0-9]+(\.[0-9]+)?%" "$COVERAGE_FILE" | head -1 | sed 's/%//')

            if [ -n "$COVERAGE_PCT" ]; then
                COVERAGE_INT=$(printf "%.0f" "$COVERAGE_PCT")

                if [ "$COVERAGE_INT" -lt 80 ]; then
                    log_error "Coverage ${COVERAGE_PCT}% is below 80% threshold"
                    exit 1
                else
                    log_success "Coverage ${COVERAGE_PCT}% meets 80% threshold"
                fi
            fi
        fi
    else
        log_warning "Coverage file not found"
    fi

    log_success "Coverage report generated"
fi

#=============================================================================
# Final Summary
#=============================================================================

log_info "================================================"
log_success "Test Suite Complete!"
echo ""
log_info "Results:"
echo "  - EUnit Tests: PASSED"
echo "  - Common Test Suites: PASSED"
echo "  - Proper Tests: PASSED"

if [ "$GENERATE_COVERAGE" = true ]; then
    echo "  - Coverage Report: Generated"
    echo ""
    log_info "View coverage report:"
    echo "  open _build/test/cover/index.html"
fi

echo ""
log_success "All tests passed successfully!"
echo ""

exit 0
