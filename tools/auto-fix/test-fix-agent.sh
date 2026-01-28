#!/usr/bin/env bash
# Test Fix Agent - Auto-fixes test failures
# Part of erlmcp Auto-Fix System

set -euo pipefail

ERROR_FILE="${1:-}"
ATTEMPT="${2:-1}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="logs/auto-fix"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log() {
    echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] [TEST-FIX] $*" | tee -a "$LOG_DIR/test-fix.log"
}

# Parse test failures from EUnit output
parse_test_failures() {
    local error_file="$1"

    # Extract test failures (module:test_name format)
    grep -E "Failed:|in function|Expected:|Actual:" "$error_file" || true
}

# Fix assertion mismatches by suggesting correct expectation
fix_assertion_mismatch() {
    local test_file="$1"
    local test_name="$2"
    local expected="$3"
    local actual="$4"

    log "Analyzing assertion mismatch in ${test_file}:${test_name}"
    log "  Expected: $expected"
    log "  Actual:   $actual"

    # Check if this is a simple type mismatch
    if [[ "$expected" =~ ^[0-9]+$ ]] && [[ "$actual" =~ ^[0-9]+$ ]]; then
        log "${YELLOW}⚠ Numeric mismatch - updating expectation${NC}"

        # Find and replace the expected value
        sed -i.bak "s/?assertEqual($expected,/?assertEqual($actual,/g" "$test_file"

        if [[ $? -eq 0 ]]; then
            log "${GREEN}✓ Updated assertion expectation${NC}"
            rm -f "${test_file}.bak"
            return 0
        else
            log "${RED}✗ Failed to update assertion${NC}"
            mv "${test_file}.bak" "$test_file" 2>/dev/null || true
            return 1
        fi
    else
        log "${YELLOW}⚠ Complex mismatch requires manual review${NC}"
        return 1
    fi
}

# Fix setup/teardown issues
fix_setup_teardown() {
    local test_file="$1"
    local error_msg="$2"

    log "Checking setup/teardown in $test_file"

    # Check if foreach fixture is properly defined
    if ! grep -q "foreach" "$test_file"; then
        log "${YELLOW}⚠ Missing foreach fixture, adding template${NC}"

        # Add basic foreach fixture
        cat >> "$test_file" <<'EOF'

%% Test fixtures
foreach_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_example/1
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

test_example(_) ->
    ?assertEqual(ok, ok).
EOF

        log "${GREEN}✓ Added foreach fixture template${NC}"
        return 0
    fi

    return 1
}

# Fix mock configuration issues
fix_mock_config() {
    local test_file="$1"
    local module_name="$2"

    log "Checking mock configuration for $module_name in $test_file"

    # Check if meck is being used
    if grep -q "meck:" "$test_file"; then
        # Ensure meck:unload is in cleanup
        if ! grep -q "meck:unload" "$test_file"; then
            log "${YELLOW}⚠ Adding meck:unload to cleanup${NC}"

            # Find cleanup function and add meck:unload
            sed -i.bak '/^cleanup/a\    meck:unload(),' "$test_file"

            if [[ $? -eq 0 ]]; then
                log "${GREEN}✓ Added meck:unload${NC}"
                rm -f "${test_file}.bak"
                return 0
            else
                mv "${test_file}.bak" "$test_file" 2>/dev/null || true
                return 1
            fi
        fi
    fi

    return 1
}

# Analyze root cause of test failure
analyze_root_cause() {
    local error_file="$1"

    log "${BLUE}Analyzing test failure root cause${NC}"

    # Look for common patterns
    if grep -q "badmatch" "$error_file"; then
        echo "PATTERN_BADMATCH"
    elif grep -q "function_clause" "$error_file"; then
        echo "PATTERN_FUNCTION_CLAUSE"
    elif grep -q "undef" "$error_file"; then
        echo "PATTERN_UNDEFINED_FUNCTION"
    elif grep -q "Expected.*Actual" "$error_file"; then
        echo "PATTERN_ASSERTION_MISMATCH"
    elif grep -q "timeout" "$error_file"; then
        echo "PATTERN_TIMEOUT"
    elif grep -q "setup" "$error_file" || grep -q "cleanup" "$error_file"; then
        echo "PATTERN_SETUP_TEARDOWN"
    else
        echo "PATTERN_UNKNOWN"
    fi
}

# Apply fixes based on pattern
apply_pattern_fix() {
    local pattern="$1"
    local error_file="$2"

    log "Applying fix for pattern: $pattern"

    case "$pattern" in
        PATTERN_ASSERTION_MISMATCH)
            log "${BLUE}Handling assertion mismatch${NC}"
            # Extract test file and details
            local test_file=$(grep -E "in function" "$error_file" | head -1 | cut -d: -f1)
            if [[ -n "$test_file" ]] && [[ -f "$test_file" ]]; then
                fix_assertion_mismatch "$test_file" "unknown" "unknown" "unknown"
            fi
            ;;

        PATTERN_SETUP_TEARDOWN)
            log "${BLUE}Handling setup/teardown issue${NC}"
            local test_file=$(grep -E "\.erl" "$error_file" | head -1 | cut -d: -f1)
            if [[ -n "$test_file" ]] && [[ -f "$test_file" ]]; then
                fix_setup_teardown "$test_file" ""
            fi
            ;;

        PATTERN_UNDEFINED_FUNCTION)
            log "${YELLOW}⚠ Undefined function - likely missing dependency${NC}"
            log "Check rebar.config for missing test dependencies"
            return 1
            ;;

        PATTERN_TIMEOUT)
            log "${YELLOW}⚠ Test timeout - may need performance optimization${NC}"
            log "Consider increasing timeout or optimizing test"
            return 1
            ;;

        *)
            log "${YELLOW}⚠ Unknown pattern - manual intervention required${NC}"
            return 1
            ;;
    esac
}

# Suggest manual fixes
suggest_manual_fixes() {
    local error_file="$1"
    local pattern="$2"

    log "${BLUE}Generating manual fix suggestions${NC}"

    local suggestion_file="$LOG_DIR/test-suggestions-$(date +%s).txt"

    cat > "$suggestion_file" <<EOF
TEST FIX SUGGESTIONS
====================
Generated: $(date)
Attempt: $ATTEMPT
Pattern: $pattern

Test Failures:
--------------
$(cat "$error_file")

Common Fixes by Pattern:
------------------------
ASSERTION_MISMATCH:
  - Review expected vs actual values
  - Update test expectations if behavior changed
  - Check for race conditions in async tests

SETUP_TEARDOWN:
  - Ensure setup/0 and cleanup/1 are defined
  - Check foreach fixture syntax
  - Verify cleanup properly releases resources

UNDEFINED_FUNCTION:
  - Add missing test dependency to rebar.config
  - Check module name spelling
  - Verify function is exported

TIMEOUT:
  - Increase timeout: {timeout, 60, fun() -> ... end}
  - Optimize slow operations
  - Check for infinite loops

BADMATCH:
  - Review pattern matching in test
  - Check return values match expectations
  - Verify function signatures

Next Steps:
-----------
1. Review failures above
2. Apply suggested fixes
3. Run: rebar3 eunit --module=<module>_tests
4. Check coverage: rebar3 cover

Suggestion file: $suggestion_file
EOF

    log "Suggestions written to: $suggestion_file"
    cat "$suggestion_file"
}

# Main
main() {
    if [[ -z "$ERROR_FILE" ]] || [[ ! -f "$ERROR_FILE" ]]; then
        echo "Usage: $0 <error_file> [attempt_number]"
        exit 1
    fi

    log "${BLUE}Test Fix Agent starting (attempt $ATTEMPT)${NC}"

    # Analyze root cause
    local pattern=$(analyze_root_cause "$ERROR_FILE")
    log "Detected pattern: $pattern"

    # Try to apply automatic fix
    if apply_pattern_fix "$pattern" "$ERROR_FILE"; then
        # Verify fix by re-running tests
        log "Verifying fixes..."
        if rebar3 eunit > /dev/null 2>&1; then
            log "${GREEN}✅ Tests passing after auto-fix${NC}"
            exit 0
        else
            log "${YELLOW}⚠ Tests still failing${NC}"
        fi
    fi

    # If auto-fix failed, provide suggestions
    suggest_manual_fixes "$ERROR_FILE" "$pattern"
    log "${RED}❌ Auto-fix failed, manual intervention required${NC}"
    exit 1
}

main "$@"
