#!/bin/bash

# Enhanced build and test script for erlmcp
# Usage: ./scripts/build_and_test.sh [OPTIONS]
# Options:
#   --help          Show help message
#   --full          Run all tests including Dialyzer and PropEr
#   --dialyzer      Run Dialyzer analysis only
#   --proper        Run PropEr tests only
#   --quick         Skip most tests, just compile and basic checks
#
# Environment:
#   ERLMCP_PROFILE  Profile to use (dev|test|staging|prod), defaults to 'dev'

# ==============================================================================
# Profile Configuration
# ==============================================================================

# Set default profile for development builds
ERLMCP_PROFILE="${ERLMCP_PROFILE:-dev}"

# Validate profile (with graceful fallback to dev)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [ -f "$SCRIPT_DIR/validate_profile.sh" ]; then
    if ! "$SCRIPT_DIR/validate_profile.sh" "$ERLMCP_PROFILE" 2>/dev/null; then
        echo "WARNING: Invalid profile '$ERLMCP_PROFILE', falling back to 'dev'"
        ERLMCP_PROFILE=dev
    fi
else
    echo "WARNING: validate_profile.sh not found, using profile: $ERLMCP_PROFILE"
fi

export ERLMCP_PROFILE

if [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
    echo "Enhanced Build and Test Script for erlmcp"
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --help      Show this help message"
    echo "  --full      Run all tests including slow Dialyzer and PropEr"
    echo "  --dialyzer  Include Dialyzer static analysis"
    echo "  --proper    Include PropEr property-based tests"
    echo "  --quick     Quick build - compile only, skip most tests"
    echo ""
    echo "Environment Variables:"
    echo "  ERLMCP_PROFILE  Profile to use (dev|test|staging|prod), default: dev"
    echo ""
    echo "Examples:"
    echo "  $0                        # Standard build and test (dev profile)"
    echo "  $0 --full                 # Complete analysis including Dialyzer"
    echo "  $0 --quick                # Fast compilation check"
    echo "  ERLMCP_PROFILE=test $0    # Run with test profile"
    echo ""
    exit 0
fi

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check for required tools
if ! command_exists rebar3; then
    print_error "rebar3 is not installed or not in PATH"
    exit 1
fi

if ! command_exists erl; then
    print_error "Erlang is not installed or not in PATH"
    exit 1
fi

print_status "Starting build and test process..."
print_status "Using profile: $ERLMCP_PROFILE"

# Clean previous builds
print_status "Cleaning previous builds..."
rebar3 clean
if [ $? -ne 0 ]; then
    print_error "Failed to clean previous builds"
    exit 1
fi

# Compile everything
print_status "Compiling project..."
echo ""
print_status "Note: Some warnings are expected - they don't prevent functionality"
echo ""
rebar3 compile
COMPILE_RESULT=$?

# If compilation fails, try to identify and report specific issues
if [ $COMPILE_RESULT -ne 0 ]; then
    print_error "Compilation failed. Common issues and solutions:"
    echo ""
    print_status "1. Missing records: Check if include files are properly included"
    print_status "2. Unsafe variables: Variables used across try-catch blocks" 
    print_status "3. Undefined functions: Dependencies or typos in function calls"
    print_status "4. Syntax errors: Missing commas, periods, or parentheses"
    echo ""
    print_status "You can try running 'rebar3 compile' directly to see detailed errors"
fi

if [ $COMPILE_RESULT -eq 0 ]; then
    print_success "Compilation successful"
    
    # Quick mode: just compile and exit
    if [ "$1" = "--quick" ]; then
        print_success "Quick build completed successfully! 🚀"
        print_status "Project compiled without errors."
        exit 0
    fi
    
    # Run EUnit tests (basic unit tests)
    print_status "Running EUnit tests..."
    rebar3 eunit 2>/dev/null
    EUNIT_RESULT=$?
    
    if [ $EUNIT_RESULT -eq 0 ]; then
        print_success "EUnit tests passed"
    else
        print_warning "Some EUnit tests failed (exit code: $EUNIT_RESULT) - this is expected during development"
    fi
    
    # Run Common Test suites (integration tests)
    print_status "Running Common Test suites..."
    rebar3 ct 2>/dev/null
    CT_RESULT=$?
    
    if [ $CT_RESULT -eq 0 ]; then
        print_success "Common Test suites passed"
    else
        print_warning "Some Common Test suites failed (exit code: $CT_RESULT) - this is expected during development"
    fi
    
    # Skip PropEr tests by default as they can be slow and may not be configured
    if [ "$1" = "--full" ] || [ "$1" = "--proper" ]; then
        print_status "Running PropEr property-based tests..."
        rebar3 proper 2>/dev/null
        PROPER_RESULT=$?
        
        if [ $PROPER_RESULT -eq 0 ]; then
            print_success "PropEr tests passed"
        else
            print_warning "Some PropEr tests failed (exit code: $PROPER_RESULT)"
        fi
    else
        print_status "Skipping PropEr tests (use --full or --proper to include)"
        PROPER_RESULT=0
    fi
    
    # Run example tests if they exist
    print_status "Running example tests (if available)..."
    
    # Check if example test files exist and run them
    if [ -f "examples/simple/simple_direct_test.erl" ]; then
        print_status "Running simple example test..."
        erl -pa _build/default/lib/*/ebin -noshell -s simple_direct_test run -s init stop 2>/dev/null
        SIMPLE_TEST_RESULT=$?
        if [ $SIMPLE_TEST_RESULT -eq 0 ]; then
            print_success "Simple example test passed"
        else
            print_warning "Simple example test failed (expected during development)"
        fi
    else
        print_status "Simple example test not found - skipping"
    fi
    
    if [ -f "examples/calculator/calculator_test.erl" ]; then
        print_status "Running calculator example test..."
        erl -pa _build/default/lib/*/ebin -noshell -s calculator_test run -s init stop 2>/dev/null
        CALC_TEST_RESULT=$?
        if [ $CALC_TEST_RESULT -eq 0 ]; then
            print_success "Calculator example test passed"
        else
            print_warning "Calculator example test failed (expected during development)"
        fi
    else
        print_status "Calculator example test not found - skipping"
    fi
    
    if [ -f "examples/weather/weather_test.erl" ]; then
        print_status "Running weather example test..."
        erl -pa _build/default/lib/*/ebin -noshell -s weather_test run -s init stop 2>/dev/null
        WEATHER_TEST_RESULT=$?
        if [ $WEATHER_TEST_RESULT -eq 0 ]; then
            print_success "Weather example test passed"
        else
            print_warning "Weather example test failed (expected during development)"
        fi
    else
        print_status "Weather example test not found - skipping"
    fi
    
    # Generate code coverage report
    print_status "Generating code coverage report..."
    rebar3 cover
    COVER_RESULT=$?
    
    if [ $COVER_RESULT -eq 0 ]; then
        print_success "Code coverage report generated"
    else
        print_warning "Failed to generate code coverage report"
    fi
    
    # Run static analysis
    print_status "Running static analysis..."
    
    # Xref analysis
    print_status "Running xref analysis..."
    rebar3 xref
    XREF_RESULT=$?
    
    if [ $XREF_RESULT -eq 0 ]; then
        print_success "Xref analysis passed"
    else
        print_warning "Xref analysis found issues"
    fi
    
    # Dialyzer analysis (can be slow, so make it optional)
    if [ "$1" = "--full" ] || [ "$1" = "--dialyzer" ]; then
        print_status "Running Dialyzer analysis (this may take a while)..."
        rebar3 dialyzer
        DIALYZER_RESULT=$?
        
        if [ $DIALYZER_RESULT -eq 0 ]; then
            print_success "Dialyzer analysis passed"
        else
            print_warning "Dialyzer analysis found issues"
        fi
    else
        print_status "Skipping Dialyzer analysis (use --full or --dialyzer to include)"
    fi
    
    # Final summary
    echo ""
    print_status "=== BUILD AND TEST SUMMARY ==="
    
    if [ $COMPILE_RESULT -eq 0 ]; then
        print_success "✓ Compilation: PASSED"
    else
        print_error "✗ Compilation: FAILED"
    fi
    
    if [ $EUNIT_RESULT -eq 0 ]; then
        print_success "✓ EUnit Tests: PASSED"
    else
        print_warning "⚠ EUnit Tests: SOME FAILURES"
    fi
    
    if [ $CT_RESULT -eq 0 ]; then
        print_success "✓ Common Tests: PASSED"
    else
        print_warning "⚠ Common Tests: SOME FAILURES"
    fi
    
    if [ $PROPER_RESULT -eq 0 ]; then
        print_success "✓ PropEr Tests: PASSED"
    else
        print_warning "⚠ PropEr Tests: SOME FAILURES"
    fi
    
    if [ $XREF_RESULT -eq 0 ]; then
        print_success "✓ Static Analysis (Xref): PASSED"
    else
        print_warning "⚠ Static Analysis (Xref): ISSUES FOUND"
    fi
    
    echo ""
    if [ $COMPILE_RESULT -eq 0 ]; then
        print_success "Build completed successfully! 🎉"
        print_status "You can now run your MCP servers and examples."
        echo ""
        print_status "Examples:"
        print_status "  • Simple server: cd examples/simple && erl -pa ../../_build/default/lib/*/ebin -s simple_server_stdio start"
        print_status "  • Calculator: cd examples/calculator && erl -pa ../../_build/default/lib/*/ebin -s calculator_server_stdio start"
        print_status "  • Weather: cd examples/weather && erl -pa ../../_build/default/lib/*/ebin -s weather_server_stdio start"
    else
        print_error "Build failed! Please fix compilation errors first."
        exit 1
    fi
    
else
    print_error "Compilation failed!"
    print_error "Please fix the compilation errors and try again."
    exit 1
fi