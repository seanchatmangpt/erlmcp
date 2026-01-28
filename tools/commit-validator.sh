#!/usr/bin/env bash
# commit-validator.sh - Full validation before git commit
# BLOCKS commit if validation fails

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print colored status
print_status() {
    local status="$1"
    local message="$2"
    case "$status" in
        SUCCESS)
            echo -e "${GREEN}✅ $message${NC}"
            ;;
        FAILURE)
            echo -e "${RED}❌ $message${NC}"
            ;;
        INFO)
            echo -e "${BLUE}ℹ️  $message${NC}"
            ;;
        WARNING)
            echo -e "${YELLOW}⚠️  $message${NC}"
            ;;
    esac
}

# Get list of staged files
get_staged_files() {
    git -C "$PROJECT_ROOT" diff --cached --name-only --diff-filter=ACMR | grep "\.erl$" || true
}

# Full compilation check
check_compilation() {
    print_status "INFO" "Running full compilation..."

    cd "$PROJECT_ROOT"

    local compile_output=$(TERM=dumb rebar3 compile 2>&1)
    local compile_status=$?

    if [[ $compile_status -ne 0 ]] || echo "$compile_output" | grep -E "ERROR" > /dev/null; then
        print_status "FAILURE" "Compilation failed"
        echo ""
        echo "$compile_output" | grep -A 10 "ERROR"
        return 1
    fi

    if echo "$compile_output" | grep -E "Warning" > /dev/null; then
        print_status "WARNING" "Compilation warnings:"
        echo "$compile_output" | grep -A 2 "Warning" | head -15
    fi

    print_status "SUCCESS" "Compilation clean"
    return 0
}

# Run all EUnit tests
check_eunit_tests() {
    print_status "INFO" "Running EUnit tests..."

    cd "$PROJECT_ROOT"

    local test_output=$(rebar3 eunit 2>&1)
    local test_status=$?

    # Save to temp file for parsing
    echo "$test_output" > /tmp/commit_eunit_output.txt

    # Show summary
    echo "$test_output" | tail -20

    if [[ $test_status -ne 0 ]] || echo "$test_output" | grep -E "Failed: [1-9]" > /dev/null; then
        print_status "FAILURE" "EUnit tests failed"
        echo ""
        echo "Failed tests:"
        grep -E "(FAILED|Error)" /tmp/commit_eunit_output.txt | head -15
        return 1
    fi

    print_status "SUCCESS" "All EUnit tests passed"
    return 0
}

# Run Common Test suites
check_ct_tests() {
    print_status "INFO" "Running Common Test suites..."

    cd "$PROJECT_ROOT"

    # Check if CT suites exist
    if ! ls test/*_SUITE.erl &> /dev/null; then
        print_status "INFO" "No CT suites found (skipping)"
        return 0
    fi

    if ! rebar3 ct 2>&1 | tail -20; then
        print_status "FAILURE" "Common Test failed"
        return 1
    fi

    print_status "SUCCESS" "All CT suites passed"
    return 0
}

# Check code formatting
check_formatting() {
    print_status "INFO" "Checking code formatting..."

    cd "$PROJECT_ROOT"

    if ! rebar3 help format &> /dev/null; then
        print_status "INFO" "Formatter not available (skipping)"
        return 0
    fi

    if ! rebar3 format --verify 2>&1 | tail -10; then
        print_status "FAILURE" "Code formatting issues found"
        echo ""
        echo "Run: rebar3 format"
        return 1
    fi

    print_status "SUCCESS" "Formatting clean"
    return 0
}

# Run Dialyzer
check_dialyzer() {
    print_status "INFO" "Running Dialyzer..."

    cd "$PROJECT_ROOT"

    local dialyzer_output=$(timeout 120s rebar3 dialyzer 2>&1 || true)

    if echo "$dialyzer_output" | grep -E "warning" > /dev/null; then
        print_status "WARNING" "Dialyzer warnings found (non-blocking):"
        echo "$dialyzer_output" | grep -A 5 "warning" | head -20
        return 0
    fi

    print_status "SUCCESS" "Dialyzer clean"
    return 0
}

# Run xref analysis
check_xref() {
    print_status "INFO" "Running xref analysis..."

    cd "$PROJECT_ROOT"

    if ! rebar3 xref 2>&1 | tail -15; then
        print_status "WARNING" "Xref issues found (non-blocking)"
        return 0
    fi

    print_status "SUCCESS" "Xref clean"
    return 0
}

# Check test coverage
check_coverage() {
    print_status "INFO" "Checking test coverage..."

    cd "$PROJECT_ROOT"

    # Run cover analysis
    local cover_output=$(rebar3 cover 2>&1 || true)

    # Extract coverage percentage
    local coverage=$(echo "$cover_output" | grep -E "total.*[0-9]+%" | grep -oE "[0-9]+%" | grep -oE "[0-9]+" || echo "0")

    if [[ $coverage -lt 80 ]]; then
        print_status "WARNING" "Coverage ${coverage}% below target (80%)"
        echo "$cover_output" | grep -E "total|coverage" | head -10
        return 0
    fi

    print_status "SUCCESS" "Coverage: ${coverage}% (≥80% target)"
    return 0
}

# Display commit summary
display_commit_summary() {
    echo ""
    print_status "INFO" "═══════════════════════════════════════════════════════"
    print_status "INFO" "COMMIT VALIDATION SUMMARY"
    print_status "INFO" "═══════════════════════════════════════════════════════"
    echo ""

    local staged_files
    staged_files=$(get_staged_files)

    if [[ -n "$staged_files" ]]; then
        print_status "INFO" "Staged Erlang files:"
        echo "$staged_files" | while read -r file; do
            echo "  - $file"
        done
    else
        print_status "WARNING" "No Erlang files staged"
    fi

    echo ""
}

# Main validation
main() {
    display_commit_summary

    local validation_failed=0

    # CRITICAL CHECKS (blocking)
    if ! check_compilation; then
        validation_failed=1
    fi

    if [[ $validation_failed -eq 0 ]] && ! check_eunit_tests; then
        validation_failed=1
    fi

    if [[ $validation_failed -eq 0 ]] && ! check_ct_tests; then
        validation_failed=1
    fi

    if [[ $validation_failed -eq 0 ]] && ! check_formatting; then
        validation_failed=1
    fi

    # NON-CRITICAL CHECKS (warnings only)
    check_dialyzer || true
    check_xref || true
    check_coverage || true

    echo ""
    print_status "INFO" "═══════════════════════════════════════════════════════"

    if [[ $validation_failed -eq 0 ]]; then
        print_status "SUCCESS" "Commit validation PASSED ✨"
        print_status "SUCCESS" "Safe to commit!"
        echo ""
        return 0
    else
        print_status "FAILURE" "Commit validation FAILED"
        print_status "FAILURE" "BLOCKING: Fix issues before committing"
        echo ""
        echo "Options:"
        echo "  1. Fix issues and re-run: git commit"
        echo "  2. Debug compilation: rebar3 compile"
        echo "  3. Debug tests: rebar3 eunit --verbose"
        echo "  4. View changes: git diff --cached"
        echo ""
        echo "To bypass (NOT recommended): git commit --no-verify"
        return 1
    fi
}

# Run validation
main "$@"
