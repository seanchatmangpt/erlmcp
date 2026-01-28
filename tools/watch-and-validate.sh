#!/usr/bin/env bash
# watch-and-validate.sh - Real-time file watching with immediate validation
# Provides <10s feedback on code changes

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Timestamp for feedback
timestamp() {
    date '+%H:%M:%S'
}

# Print colored status
print_status() {
    local status="$1"
    local message="$2"
    case "$status" in
        SUCCESS)
            echo -e "${GREEN}✅ [$(timestamp)] $message${NC}"
            ;;
        FAILURE)
            echo -e "${RED}❌ [$(timestamp)] $message${NC}"
            ;;
        INFO)
            echo -e "${BLUE}ℹ️  [$(timestamp)] $message${NC}"
            ;;
        WARNING)
            echo -e "${YELLOW}⚠️  [$(timestamp)] $message${NC}"
            ;;
    esac
}

# Compile a specific module
compile_module() {
    local file="$1"
    local start_time=$(date +%s%N)

    print_status "INFO" "Compiling: $file"

    if cd "$PROJECT_ROOT" && TERM=dumb rebar3 compile 2>&1 | grep -E "(ERROR|Warning|Compiling)"; then
        local end_time=$(date +%s%N)
        local duration=$(( (end_time - start_time) / 1000000 ))
        print_status "SUCCESS" "Compiled in ${duration}ms"
        return 0
    else
        print_status "FAILURE" "Compilation failed"
        return 1
    fi
}

# Run tests for a specific module
run_module_tests() {
    local file="$1"
    local module_name=$(basename "$file" .erl)
    local test_module="${module_name}_tests"
    local start_time=$(date +%s%N)

    # Check if test module exists
    if [[ -f "$PROJECT_ROOT/test/${test_module}.erl" ]]; then
        print_status "INFO" "Running tests: $test_module"

        if cd "$PROJECT_ROOT" && rebar3 eunit --module="$test_module" 2>&1 | tail -20; then
            local end_time=$(date +%s%N)
            local duration=$(( (end_time - start_time) / 1000000 ))
            print_status "SUCCESS" "Tests passed in ${duration}ms"
            return 0
        else
            print_status "FAILURE" "Tests failed"
            return 1
        fi
    else
        print_status "WARNING" "No test module found: $test_module"
        return 0
    fi
}

# Run CT suite for a specific suite file
run_ct_suite() {
    local file="$1"
    local suite_name=$(basename "$file" .erl)
    local start_time=$(date +%s%N)

    print_status "INFO" "Running CT suite: $suite_name"

    if cd "$PROJECT_ROOT" && rebar3 ct --suite="$file" 2>&1 | tail -20; then
        local end_time=$(date +%s%N)
        local duration=$(( (end_time - start_time) / 1000000 ))
        print_status "SUCCESS" "CT suite passed in ${duration}ms"
        return 0
    else
        print_status "FAILURE" "CT suite failed"
        return 1
    fi
}

# Validate a changed file
validate_file() {
    local file="$1"
    local relative_path="${file#$PROJECT_ROOT/}"

    echo ""
    print_status "INFO" "═══════════════════════════════════════════════════════"
    print_status "INFO" "File changed: $relative_path"
    print_status "INFO" "═══════════════════════════════════════════════════════"

    # Compile first
    if ! compile_module "$file"; then
        print_status "FAILURE" "Fix compilation errors before continuing"
        return 1
    fi

    # Determine test strategy based on file location
    if [[ "$file" == *"/src/"* ]]; then
        # Source file changed - run corresponding tests
        if ! run_module_tests "$file"; then
            print_status "FAILURE" "Fix test failures before continuing"
            return 1
        fi
    elif [[ "$file" == *"/test/"* ]] && [[ "$file" == *"_SUITE.erl" ]]; then
        # CT suite changed - run the suite
        if ! run_ct_suite "$file"; then
            print_status "FAILURE" "Fix CT suite failures before continuing"
            return 1
        fi
    elif [[ "$file" == *"/test/"* ]] && [[ "$file" == *"_tests.erl" ]]; then
        # Test file changed - run the tests
        local module_name=$(basename "$file" .erl)
        if cd "$PROJECT_ROOT" && ! rebar3 eunit --module="$module_name" 2>&1 | tail -20; then
            print_status "FAILURE" "Fix test failures before continuing"
            return 1
        fi
        print_status "SUCCESS" "Tests passed"
    fi

    print_status "SUCCESS" "All validations passed! ✨"
    return 0
}

# Main watch loop
main() {
    cd "$PROJECT_ROOT"

    # Check for fswatch
    if ! command -v fswatch &> /dev/null; then
        print_status "FAILURE" "fswatch not found. Install with: brew install fswatch"
        exit 1
    fi

    print_status "INFO" "Starting watch mode..."
    print_status "INFO" "Watching: src/ and test/"
    print_status "INFO" "Press Ctrl+C to stop"
    echo ""

    # Watch for .erl file changes in src/ and test/
    fswatch -0 -r -e ".*" -i "\\.erl$" src/ test/ | while read -d "" file; do
        # Skip temporary files and swap files
        if [[ "$file" == *"~"* ]] || [[ "$file" == *".swp"* ]] || [[ "$file" == *".swo"* ]]; then
            continue
        fi

        # Validate the changed file
        validate_file "$file" || true
    done
}

# Handle cleanup
cleanup() {
    echo ""
    print_status "INFO" "Stopping watch mode..."
    exit 0
}

trap cleanup SIGINT SIGTERM

main "$@"
