#!/usr/bin/env bash
# post-edit-validator.sh - Validates changes after editing
# BLOCKS next edit if validation fails

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

# Find affected modules from changed files
find_affected_modules() {
    local changed_files=("$@")
    local affected_modules=()

    for file in "${changed_files[@]}"; do
        if [[ -f "$file" ]]; then
            local module_name=$(basename "$file" .erl)
            affected_modules+=("$module_name")
        fi
    done

    echo "${affected_modules[@]}"
}

# Compile changed files
compile_changes() {
    print_status "INFO" "Compiling changes..."

    cd "$PROJECT_ROOT"

    local compile_output=$(TERM=dumb rebar3 compile 2>&1)
    local compile_status=$?

    # Show compilation output
    echo "$compile_output" | grep -E "(Compiling|ERROR|Warning)" || true

    if [[ $compile_status -ne 0 ]] || echo "$compile_output" | grep -E "ERROR" > /dev/null; then
        print_status "FAILURE" "Compilation failed"
        echo ""
        echo "Compilation errors:"
        echo "$compile_output" | grep -A 5 "ERROR" | head -20
        return 1
    fi

    if echo "$compile_output" | grep -E "Warning" > /dev/null; then
        print_status "WARNING" "Compilation warnings found"
        echo "$compile_output" | grep -A 2 "Warning" | head -10
    fi

    print_status "SUCCESS" "Compilation successful"
    return 0
}

# Run tests for affected modules
test_affected_modules() {
    local modules=("$@")

    if [[ ${#modules[@]} -eq 0 ]]; then
        print_status "WARNING" "No modules specified, running all tests"
        cd "$PROJECT_ROOT"
        if ! rebar3 eunit 2>&1 | tail -20; then
            print_status "FAILURE" "Tests failed"
            return 1
        fi
        print_status "SUCCESS" "All tests passed"
        return 0
    fi

    print_status "INFO" "Testing affected modules: ${modules[*]}"

    local failed_tests=()

    for module in "${modules[@]}"; do
        # Check if this is a test module
        if [[ "$module" == *"_tests" ]]; then
            print_status "INFO" "Running test module: $module"
            cd "$PROJECT_ROOT"
            if ! rebar3 eunit --module="$module" 2>&1 | tail -15; then
                failed_tests+=("$module")
            fi
        elif [[ "$module" == *"_SUITE" ]]; then
            print_status "INFO" "Running CT suite: $module"
            cd "$PROJECT_ROOT"
            if ! rebar3 ct --suite="test/${module}.erl" 2>&1 | tail -15; then
                failed_tests+=("$module")
            fi
        else
            # Regular module - check for corresponding test module
            local test_module="${module}_tests"
            if [[ -f "$PROJECT_ROOT/test/${test_module}.erl" ]]; then
                print_status "INFO" "Running tests for: $module"
                cd "$PROJECT_ROOT"
                if ! rebar3 eunit --module="$test_module" 2>&1 | tail -15; then
                    failed_tests+=("$test_module")
                fi
            else
                print_status "WARNING" "No tests found for: $module"
            fi
        fi
    done

    if [[ ${#failed_tests[@]} -gt 0 ]]; then
        print_status "FAILURE" "Failed tests: ${failed_tests[*]}"
        return 1
    fi

    print_status "SUCCESS" "All affected tests passed"
    return 0
}

# Check code formatting
check_formatting() {
    print_status "INFO" "Checking code formatting..."

    cd "$PROJECT_ROOT"

    # Run formatter check if available
    if rebar3 help format &> /dev/null; then
        if ! rebar3 format --verify 2>&1 | tail -10; then
            print_status "WARNING" "Formatting issues found (non-blocking)"
            echo "Run: rebar3 format to fix"
            return 0
        fi
        print_status "SUCCESS" "Formatting clean"
    else
        print_status "INFO" "Formatter not available (skipping)"
    fi

    return 0
}

# Run quick Dialyzer check on changed files
quick_dialyzer() {
    local files=("$@")

    if [[ ${#files[@]} -eq 0 ]]; then
        print_status "INFO" "Skipping Dialyzer (no files specified)"
        return 0
    fi

    print_status "INFO" "Running quick Dialyzer check..."

    cd "$PROJECT_ROOT"

    # Quick dialyzer with short timeout
    if timeout 30s rebar3 dialyzer 2>&1 | grep -E "warning" > /dev/null; then
        print_status "WARNING" "Dialyzer warnings found (non-blocking)"
        rebar3 dialyzer 2>&1 | grep -A 3 "warning" | head -15
        return 0
    fi

    print_status "SUCCESS" "Dialyzer clean"
    return 0
}

# Main validation
main() {
    local changed_files=("$@")

    echo ""
    print_status "INFO" "═══════════════════════════════════════════════════════"
    print_status "INFO" "POST-EDIT VALIDATION"
    print_status "INFO" "═══════════════════════════════════════════════════════"
    echo ""

    if [[ ${#changed_files[@]} -gt 0 ]]; then
        print_status "INFO" "Changed files: ${changed_files[*]}"
        echo ""
    else
        print_status "WARNING" "No files specified - will validate all"
        echo ""
    fi

    local validation_failed=0

    # 1. Compile changes (CRITICAL - BLOCKING)
    if ! compile_changes; then
        validation_failed=1
    fi

    # 2. Run tests for affected modules (CRITICAL - BLOCKING)
    if [[ $validation_failed -eq 0 ]]; then
        local affected_modules
        if [[ ${#changed_files[@]} -gt 0 ]]; then
            affected_modules=($(find_affected_modules "${changed_files[@]}"))
        else
            affected_modules=()
        fi

        if ! test_affected_modules "${affected_modules[@]}"; then
            validation_failed=1
        fi
    fi

    # 3. Check formatting (WARNING only - non-blocking)
    check_formatting || true

    # 4. Quick Dialyzer (WARNING only - non-blocking)
    quick_dialyzer "${changed_files[@]}" || true

    echo ""
    print_status "INFO" "═══════════════════════════════════════════════════════"

    if [[ $validation_failed -eq 0 ]]; then
        print_status "SUCCESS" "Post-edit validation PASSED ✨"
        print_status "SUCCESS" "Changes are clean - safe to continue!"
        echo ""
        return 0
    else
        print_status "FAILURE" "Post-edit validation FAILED"
        print_status "FAILURE" "BLOCKING: Fix issues before next edit"
        echo ""
        echo "Options:"
        echo "  1. Fix the issues and re-run: $0"
        echo "  2. Revert changes: git checkout -- <files>"
        echo "  3. Debug: rebar3 eunit --verbose"
        echo ""
        return 1
    fi
}

# Parse arguments
CHANGED_FILES=()

while [[ $# -gt 0 ]]; do
    CHANGED_FILES+=("$1")
    shift
done

main "${CHANGED_FILES[@]}"
