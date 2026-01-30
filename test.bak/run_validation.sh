#!/bin/bash

###############################################################################
# ErlMCP Validation Test Runner
#
# This script provides an easy way to run comprehensive validation tests
# for the ErlMCP system, including:
#
# - Integration tests
# - Load tests  
# - Performance benchmarks
# - Memory leak detection
# - System health validation
#
# Usage:
#   ./run_validation.sh [OPTIONS]
#
# Options:
#   --all          Run complete validation suite (default)
#   --integration  Run integration tests only
#   --load         Run load tests only  
#   --performance  Run performance benchmarks only
#   --memory       Run memory leak detection only
#   --health       Run system health validation only
#   --quick        Run quick validation (subset of tests)
#   --verbose      Enable verbose output
#   --help         Show this help message
#
###############################################################################

set -e  # Exit on any error

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

# Default options
RUN_TYPE="all"
VERBOSE=false
QUICK=false

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_header() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}========================================${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

# Function to show usage
show_help() {
    cat << EOF
ErlMCP Validation Test Runner

Usage: $0 [OPTIONS]

OPTIONS:
    --all          Run complete validation suite (default)
    --integration  Run integration tests only
    --load         Run load tests only
    --performance  Run performance benchmarks only
    --memory       Run memory leak detection only
    --health       Run system health validation only
    --quick        Run quick validation (subset of tests)
    --verbose      Enable verbose output
    --help         Show this help message

EXAMPLES:
    $0                    # Run complete validation suite
    $0 --integration      # Run integration tests only
    $0 --performance      # Run performance benchmarks only
    $0 --quick --verbose  # Run quick validation with verbose output

VALIDATION PHASES:
    1. Integration Tests  - End-to-end system functionality
    2. Load Tests        - High concurrency and throughput
    3. Performance Tests - Response times and resource usage
    4. Memory Tests      - Memory leak detection
    5. Health Tests      - System health and monitoring

For more information, see: test/README_VALIDATION.md
EOF
}

# Parse command line arguments
parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --all)
                RUN_TYPE="all"
                shift
                ;;
            --integration)
                RUN_TYPE="integration"
                shift
                ;;
            --load)
                RUN_TYPE="load"
                shift
                ;;
            --performance)
                RUN_TYPE="performance"
                shift
                ;;
            --memory)
                RUN_TYPE="memory"
                shift
                ;;
            --health)
                RUN_TYPE="health"
                shift
                ;;
            --quick)
                QUICK=true
                shift
                ;;
            --verbose)
                VERBOSE=true
                shift
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                print_error "Unknown option: $1"
                echo "Use --help for usage information"
                exit 1
                ;;
        esac
    done
}

# Function to check prerequisites
check_prerequisites() {
    print_info "Checking prerequisites..."
    
    # Check if we're in the right directory
    if [[ ! -f "$PROJECT_DIR/rebar.config" ]]; then
        print_error "Not in ErlMCP project directory"
        exit 1
    fi
    
    # Check if Erlang is available
    if ! command -v erl &> /dev/null; then
        print_error "Erlang is not installed or not in PATH"
        exit 1
    fi
    
    # Check if rebar3 is available
    if ! command -v rebar3 &> /dev/null; then
        print_error "rebar3 is not installed or not in PATH"
        exit 1
    fi
    
    print_success "Prerequisites check passed"
}

# Function to compile the project
compile_project() {
    print_info "Compiling ErlMCP project..."
    
    cd "$PROJECT_DIR"
    
    if $VERBOSE; then
        rebar3 compile
    else
        rebar3 compile > /dev/null 2>&1
    fi
    
    if [[ $? -eq 0 ]]; then
        print_success "Project compiled successfully"
    else
        print_error "Project compilation failed"
        exit 1
    fi
}

# Function to run Erlang command
run_erlang_command() {
    local module=$1
    local function=$2
    local args=$3
    
    cd "$PROJECT_DIR"
    
    # Build the erl command
    local erl_cmd="erl -noshell -pa _build/default/lib/*/ebin -pa ebin -s $module $function"
    
    if [[ -n "$args" ]]; then
        erl_cmd="$erl_cmd $args"
    fi
    
    if $VERBOSE; then
        print_info "Running: $erl_cmd"
        eval $erl_cmd
    else
        eval $erl_cmd 2>&1
    fi
    
    return $?
}

# Function to run integration tests
run_integration_tests() {
    print_header "Running Integration Tests"
    
    if $QUICK; then
        print_info "Running quick integration tests..."
        run_erlang_command "validation_runner" "run_with_options" "[{test_suites, [erlmcp_integration_SUITE]}, {verbose, $VERBOSE}]"
    else
        print_info "Running comprehensive integration tests..."
        run_erlang_command "validation_runner" "run_all_tests"
    fi
    
    local exit_code=$?
    
    if [[ $exit_code -eq 0 ]]; then
        print_success "Integration tests PASSED"
    else
        print_error "Integration tests FAILED"
    fi
    
    return $exit_code
}

# Function to run load tests
run_load_tests() {
    print_header "Running Load Tests"
    
    if $QUICK; then
        print_info "Running quick load tests..."
        run_erlang_command "validation_runner" "run_with_options" "[{test_suites, [erlmcp_load_SUITE]}, {skip_memory_check, true}, {verbose, $VERBOSE}]"
    else
        print_info "Running comprehensive load tests..."
        # Run load tests via Common Test
        cd "$PROJECT_DIR"
        rebar3 ct --suite=test/erlmcp_load_SUITE
    fi
    
    local exit_code=$?
    
    if [[ $exit_code -eq 0 ]]; then
        print_success "Load tests PASSED"
    else
        print_error "Load tests FAILED"
    fi
    
    return $exit_code
}

# Function to run performance tests
run_performance_tests() {
    print_header "Running Performance Tests"
    
    print_info "Running performance benchmarks..."
    run_erlang_command "validation_runner" "run_performance_validation"
    
    local exit_code=$?
    
    if [[ $exit_code -eq 0 ]]; then
        print_success "Performance tests PASSED"
    else
        print_error "Performance tests FAILED"
    fi
    
    return $exit_code
}

# Function to run memory leak detection
run_memory_tests() {
    print_header "Running Memory Leak Detection"
    
    if $QUICK; then
        print_warning "Memory leak detection requires extended runtime - skipping in quick mode"
        return 0
    fi
    
    print_info "Running memory leak detection (this may take several minutes)..."
    run_erlang_command "validation_runner" "run_memory_leak_detection"
    
    local exit_code=$?
    
    if [[ $exit_code -eq 0 ]]; then
        print_success "Memory leak detection PASSED"
    else
        print_error "Memory leak detection FAILED"
    fi
    
    return $exit_code
}

# Function to run system health tests
run_health_tests() {
    print_header "Running System Health Validation"
    
    print_info "Running system health checks..."
    run_erlang_command "validation_runner" "run_system_health_validation"
    
    local exit_code=$?
    
    if [[ $exit_code -eq 0 ]]; then
        print_success "System health validation PASSED"
    else
        print_error "System health validation FAILED"
    fi
    
    return $exit_code
}

# Function to run all validation tests
run_all_validation() {
    print_header "Running Complete ErlMCP Validation Suite"
    
    if $QUICK; then
        print_warning "Running QUICK validation mode (some tests will be abbreviated)"
    fi
    
    print_info "This will run all validation phases sequentially..."
    print_info "Estimated time: $(if $QUICK; then echo "5-10 minutes"; else echo "15-30 minutes"; fi)"
    echo ""
    
    # Track overall results
    local overall_result=0
    local failed_phases=()
    
    # Run integration tests
    if ! run_integration_tests; then
        overall_result=1
        failed_phases+=("Integration")
    fi
    echo ""
    
    # Run load tests
    if ! run_load_tests; then
        overall_result=1
        failed_phases+=("Load")
    fi
    echo ""
    
    # Run performance tests  
    if ! run_performance_tests; then
        overall_result=1
        failed_phases+=("Performance")
    fi
    echo ""
    
    # Run memory tests (skip in quick mode)
    if ! $QUICK; then
        if ! run_memory_tests; then
            overall_result=1
            failed_phases+=("Memory")
        fi
        echo ""
    fi
    
    # Run health tests
    if ! run_health_tests; then
        overall_result=1
        failed_phases+=("Health")
    fi
    echo ""
    
    # Print final results
    print_header "Validation Suite Results"
    
    if [[ $overall_result -eq 0 ]]; then
        print_success "🎉 ALL VALIDATION PHASES PASSED! 🎉"
        print_success "System is ready for production deployment"
    else
        print_error "❌ VALIDATION FAILED ❌"
        print_error "Failed phases: ${failed_phases[*]}"
        print_error "System requires fixes before production deployment"
    fi
    
    return $overall_result
}

# Function to generate validation report
generate_report() {
    print_header "Generating Validation Report"
    
    local report_file="validation_report_$(date +%Y%m%d_%H%M%S).txt"
    local report_path="$PROJECT_DIR/test/$report_file"
    
    cat > "$report_path" << EOF
ErlMCP Validation Report
========================

Generated: $(date)
Run Type: $RUN_TYPE
Quick Mode: $QUICK
Verbose Mode: $VERBOSE

System Information:
- Erlang Version: $(erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell)
- Platform: $(uname -s) $(uname -r)
- Hostname: $(hostname)

Project Information:
- Project Directory: $PROJECT_DIR
- Git Commit: $(cd "$PROJECT_DIR" && git rev-parse HEAD 2>/dev/null || echo "Not a git repository")
- Git Branch: $(cd "$PROJECT_DIR" && git branch --show-current 2>/dev/null || echo "Not a git repository")

Test Configuration:
- Test Suite: $RUN_TYPE validation
- Duration: $(if $QUICK; then echo "Quick mode"; else echo "Full validation"; fi)
- Verbose Output: $VERBOSE

EOF

    print_success "Validation report saved to: $report_path"
}

# Main execution function
main() {
    local start_time=$(date +%s)
    
    # Parse command line arguments
    parse_arguments "$@"
    
    # Show startup banner
    print_header "ErlMCP Validation Test Runner"
    print_info "Run type: $RUN_TYPE"
    print_info "Quick mode: $QUICK"
    print_info "Verbose: $VERBOSE"
    echo ""
    
    # Check prerequisites
    check_prerequisites
    echo ""
    
    # Compile project
    compile_project
    echo ""
    
    # Run the appropriate validation
    local validation_result=0
    
    case $RUN_TYPE in
        "all")
            run_all_validation
            validation_result=$?
            ;;
        "integration")
            run_integration_tests
            validation_result=$?
            ;;
        "load")
            run_load_tests
            validation_result=$?
            ;;
        "performance")
            run_performance_tests
            validation_result=$?
            ;;
        "memory")
            run_memory_tests
            validation_result=$?
            ;;
        "health")
            run_health_tests
            validation_result=$?
            ;;
        *)
            print_error "Unknown run type: $RUN_TYPE"
            exit 1
            ;;
    esac
    
    # Calculate execution time
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    local duration_min=$((duration / 60))
    local duration_sec=$((duration % 60))
    
    echo ""
    print_info "Total execution time: ${duration_min}m ${duration_sec}s"
    
    # Generate report if running full validation
    if [[ "$RUN_TYPE" == "all" ]]; then
        echo ""
        generate_report
    fi
    
    # Final status
    echo ""
    if [[ $validation_result -eq 0 ]]; then
        print_success "Validation completed successfully!"
        exit 0
    else
        print_error "Validation failed!"
        exit 1
    fi
}

# Run main function with all arguments
main "$@"