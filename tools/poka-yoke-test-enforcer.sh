#!/bin/bash

# ===================================================================
# POKA-YOKE TEST QUALITY ENFORCER
# Automatically enforces testing quality standards for erlmcp
# ===================================================================

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
MIN_COVERAGE=80
MAX_SKIPPED_TESTS=0
MAX_FLAKY_TESTS=0
MAX_HARDcoded_TIMEOUTS=0

# Initialize counters
PASS_COUNT=0
FAIL_COUNT=0
ISSUE_COUNT=0

echo -e "${BLUE}🔍 POKA-YOKE TEST QUALITY ENFORCER${NC}"
echo "===================================="
echo ""

# Function to print header
print_header() {
    echo -e "${BLUE}📋 $1${NC}"
    echo "----------------------------------------"
}

# Function to print success
print_success() {
    echo -e "${GREEN}✅ $1${NC}"
    ((PASS_COUNT++))
}

# Function to print warning
print_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
    ((ISSUE_COUNT++))
}

# Function to print error
print_error() {
    echo -e "${RED}❌ $1${NC}"
    ((FAIL_COUNT++))
}

# Function to check if command exists
check_command() {
    if ! command -v $1 &> /dev/null; then
        print_error "Command $1 not found"
        return 1
    fi
    return 0
}

# Pre-flight checks
print_header "PRE-FLIGHT CHECKS"
echo ""

# Check required commands
for cmd in erl rebar3; do
    check_command $cmd
done

# Check if we're in the erlmcp directory
if [ ! -f "rebar.config" ]; then
    print_error "Not in erlmcp directory (rebar.config not found)"
    exit 1
fi

print_success "Environment check passed"
echo ""

# 1. Check compilation
print_header "1. COMPILATION CHECK"
echo ""

if rebar3 compile; then
    print_success "Compilation successful"
else
    print_error "Compilation failed"
    exit 1
fi
echo ""

# 2. Check code coverage
print_header "2. CODE COVERAGE ANALYSIS"
echo ""

# Run tests with coverage
if rebar3 as test eunit --cover; then
    # Parse coverage report
    if [ -f "_build/test/cover/index.html" ]; then
        # Extract coverage percentage (simplified)
        COVERAGE=$(grep -o '[0-9]*%' _build/test/cover/index.html | head -1 | sed 's/%//')

        if [ -n "$COVERAGE" ] && [ "$COVERAGE" -ge "$MIN_COVERAGE" ]; then
            print_success "Coverage: ${COVERAGE}% (meets minimum ${MIN_COVERAGE}%)"
        else
            print_error "Coverage: ${COVERAGE}% (below minimum ${MIN_COVERAGE}%)"
        fi
    else
        print_warning "Coverage report not found"
    fi
else
    print_error "Test execution failed"
fi
echo ""

# 3. Check for skipped tests
print_header "3. SKIPPED TEST CHECK"
echo ""

SKIPPED_COUNT=$(find apps -name "*.skip" | wc -l)
if [ "$SKIPPED_COUNT" -le "$MAX_SKIPPED_TESTS" ]; then
    print_success "Skipped tests: $SKIPPED_COUNT (within limit $MAX_SKIPPED_TESTS)"
else
    print_error "Skipped tests: $SKIPPED_COUNT (exceeds limit $MAX_SKIPPED_TESTS)"
fi
echo ""

# 4. Check for hardcoded timeouts
print_header "4. HARDCODED TIMEOUT CHECK"
echo ""

TIMEOUT_FILES=$(find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | wc -l)
if [ "$TIMEOUT_FILES" -le "$MAX_HARDcoded_TIMEOUTS" ]; then
    print_success "Files with hardcoded timeouts: $TIMEOUT_FILES"
else
    print_warning "Files with hardcoded timeouts: $TIMEOUT_FILES"

    # Show specific files
    echo ""
    echo "Files with hardcoded timeouts:"
    find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | head -5
fi
echo ""

# 5. Check for test isolation issues
print_header "5. TEST ISOLATION CHECK"
echo ""

# Look for shared state patterns
SHARED_STATE=$(grep -r "ets:.*insert\|mnesia:.*transaction\|global:register_name" apps/*/test/ 2>/dev/null | wc -l)

if [ "$SHARED_STATE" -eq 0 ]; then
    print_success "No shared state detected in tests"
else
    print_warning "Shared state patterns found: $SHARED_STATE"
fi
echo ""

# 6. Check for concurrent test patterns
print_header "6. CONCURRENT ACCESS CHECK"
echo ""

CONCURRENT_TESTS=$(grep -r "spawn.*test\|concurrent\|race" apps/*/test/ 2>/dev/null | wc -l)

if [ "$CONCURRENT_TESTS" -gt 0 ]; then
    print_success "Concurrent test patterns found: $CONCURRENT_TESTS"
else
    print_warning "No concurrent test patterns found"
fi
echo ""

# 7. Run quality gates
print_header "7. QUALITY GATE EXECUTION"
echo ""

# Run the Erlang quality checker
if erl -pa _build/test/lib/*/ebin -eval '
poka_yoke_test_checker:run_check(),
halt(0).' -noshell; then
    print_success "Quality gates passed"
else
    print_error "Quality gates failed"
fi
echo ""

# 8. Auto-fix issues (if requested)
if [ "$1" = "--auto-fix" ]; then
    print_header "8. AUTO-FIXING ISSUES"
    echo ""

    if erl -pa _build/test/lib/*/ebin -eval '
poka_yoke_test_checker:auto_fix_test_issues(),
halt(0).' -noshell; then
        print_success "Auto-fix completed"
    else
        print_error "Auto-fix failed"
    fi
    echo ""
fi

# Generate summary report
print_header "SUMMARY REPORT"
echo ""
echo "Quality Enforcement Results:"
echo "  ✅ Passed: $PASS_COUNT"
echo "  ❌ Failed: $FAIL_COUNT"
echo "  ⚠️  Issues: $ISSUE_COUNT"
echo ""

# Calculate overall result
if [ $FAIL_COUNT -eq 0 ] && [ $ISSUE_COUNT -eq 0 ]; then
    echo -e "${GREEN}🎉 ALL QUALITY GATES PASSED!${NC}"
    exit 0
elif [ $FAIL_COUNT -eq 0 ]; then
    echo -e "${YELLOW}⚠️  QUALITY GATES PASSED WITH WARNINGS${NC}"
    exit 0
else
    echo -e "${RED}❌ QUALITY GATES FAILED${NC}"
    exit 1
fi