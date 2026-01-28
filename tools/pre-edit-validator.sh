#!/usr/bin/env bash
# pre-edit-validator.sh - Validates clean state before editing
# Enforces "Fix before you add" principle

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

# Check if there are uncommitted changes
check_git_status() {
    if ! git -C "$PROJECT_ROOT" diff --quiet 2>/dev/null; then
        print_status "WARNING" "Uncommitted changes detected"
        return 1
    fi
    return 0
}

# Compile the project
check_compilation() {
    print_status "INFO" "Checking compilation..."

    cd "$PROJECT_ROOT"
    if TERM=dumb rebar3 compile 2>&1 | grep -E "(ERROR|error)" > /dev/null; then
        print_status "FAILURE" "Compilation errors found"
        echo ""
        echo "Fix compilation errors before editing:"
        TERM=dumb rebar3 compile 2>&1 | grep -E "(ERROR|error)" | head -10
        return 1
    fi

    print_status "SUCCESS" "Compilation clean"
    return 0
}

# Run tests
check_tests() {
    print_status "INFO" "Checking tests..."

    cd "$PROJECT_ROOT"

    # Run EUnit tests
    if ! rebar3 eunit 2>&1 | tee /tmp/eunit_output.txt | tail -20; then
        print_status "FAILURE" "EUnit tests failing"
        echo ""
        echo "Fix failing tests before editing:"
        grep -E "(FAILED|Error)" /tmp/eunit_output.txt | head -10
        return 1
    fi

    # Check for test failures in output
    if grep -E "Failed: [1-9]" /tmp/eunit_output.txt > /dev/null; then
        print_status "FAILURE" "EUnit tests have failures"
        return 1
    fi

    print_status "SUCCESS" "All tests passing"
    return 0
}

# Check for dialyzer warnings
check_dialyzer() {
    print_status "INFO" "Checking Dialyzer (quick mode)..."

    cd "$PROJECT_ROOT"

    # Run dialyzer with short timeout (incremental PLT)
    if timeout 30s rebar3 dialyzer 2>&1 | grep -E "warning" > /dev/null; then
        print_status "WARNING" "Dialyzer warnings found (non-blocking)"
        return 0
    fi

    print_status "SUCCESS" "Dialyzer clean"
    return 0
}

# Main validation
main() {
    local file_to_edit="${1:-}"
    local skip_tests="${2:-false}"

    echo ""
    print_status "INFO" "═══════════════════════════════════════════════════════"
    print_status "INFO" "PRE-EDIT VALIDATION"
    print_status "INFO" "═══════════════════════════════════════════════════════"
    echo ""

    if [[ -n "$file_to_edit" ]]; then
        print_status "INFO" "File to edit: $file_to_edit"
        echo ""
    fi

    local validation_failed=0

    # 1. Check compilation (CRITICAL)
    if ! check_compilation; then
        validation_failed=1
    fi

    # 2. Check tests (CRITICAL)
    if [[ "$skip_tests" != "true" ]]; then
        if ! check_tests; then
            validation_failed=1
        fi
    else
        print_status "WARNING" "Tests skipped (--skip-tests flag)"
    fi

    # 3. Check Dialyzer (WARNING only)
    check_dialyzer || true

    echo ""
    print_status "INFO" "═══════════════════════════════════════════════════════"

    if [[ $validation_failed -eq 0 ]]; then
        print_status "SUCCESS" "Pre-edit validation PASSED ✨"
        print_status "SUCCESS" "Safe to edit!"
        echo ""
        return 0
    else
        print_status "FAILURE" "Pre-edit validation FAILED"
        print_status "FAILURE" "Fix issues before editing"
        echo ""
        echo "To bypass (NOT recommended): $0 --skip-tests"
        return 1
    fi
}

# Parse flags
SKIP_TESTS=false
FILE_TO_EDIT=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --skip-tests)
            SKIP_TESTS=true
            shift
            ;;
        *)
            FILE_TO_EDIT="$1"
            shift
            ;;
    esac
done

main "$FILE_TO_EDIT" "$SKIP_TESTS"
