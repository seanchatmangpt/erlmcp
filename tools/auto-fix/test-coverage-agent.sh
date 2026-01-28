#!/usr/bin/env bash
# Test Coverage Agent - Identifies uncovered code and suggests tests
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
    echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] [COVERAGE] $*" | tee -a "$LOG_DIR/coverage-fix.log"
}

# Parse coverage report
parse_coverage() {
    local error_file="$1"

    # Extract modules with low coverage
    grep -E "[0-9]+%" "$error_file" | grep -v "100%" || true
}

# Generate test suggestions for uncovered module
suggest_tests_for_module() {
    local module="$1"
    local coverage="$2"

    log "${BLUE}Analyzing coverage for $module ($coverage)${NC}"

    local module_file="src/${module}.erl"
    if [[ ! -f "$module_file" ]]; then
        log "${YELLOW}⚠ Module file not found: $module_file${NC}"
        return 1
    fi

    # Extract exported functions
    local exports=$(grep -E "^-export\(\[" "$module_file" | sed 's/-export(\[//g' | sed 's/\])\.//g' | tr ',' '\n')

    log "Exported functions in $module:"
    echo "$exports"

    # Generate test template
    local test_file="test/${module}_tests.erl"
    if [[ -f "$test_file" ]]; then
        log "${YELLOW}⚠ Test file already exists: $test_file${NC}"
        log "Consider adding more test cases to existing file"
        return 1
    fi

    log "${GREEN}Generating test template: $test_file${NC}"

    cat > "$test_file" <<EOF
-module(${module}_tests).
-include_lib("eunit/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

${module}_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
EOF

    # Add test function placeholders
    while IFS= read -r export; do
        if [[ -n "$export" ]]; then
            # Extract function name (before /)
            local func_name=$(echo "$export" | cut -d'/' -f1 | xargs)
            if [[ -n "$func_name" ]]; then
                cat >> "$test_file" <<EOF
         fun test_${func_name}/1,
EOF
            fi
        fi
    done <<< "$exports"

    cat >> "$test_file" <<EOF
         fun test_placeholder/1
     ]}.

%%====================================================================
%% Setup/Cleanup
%%====================================================================

setup() ->
    %% TODO: Add setup logic
    ok.

cleanup(_) ->
    %% TODO: Add cleanup logic
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

EOF

    # Generate test case stubs
    while IFS= read -r export; do
        if [[ -n "$export" ]]; then
            local func_name=$(echo "$export" | cut -d'/' -f1 | xargs)
            if [[ -n "$func_name" ]]; then
                cat >> "$test_file" <<EOF
test_${func_name}(_) ->
    %% TODO: Implement test for ${module}:${func_name}
    %% Example:
    %% Result = ${module}:${func_name}(Args),
    %% ?assertEqual(Expected, Result).
    ?assert(true).

EOF
            fi
        fi
    done <<< "$exports"

    cat >> "$test_file" <<EOF
test_placeholder(_) ->
    %% Placeholder test to make fixture valid
    ?assert(true).
EOF

    log "${GREEN}✓ Test template created: $test_file${NC}"
    log "${YELLOW}⚠ Tests are placeholders - manual implementation required${NC}"
    return 0
}

# Analyze coverage gaps
analyze_coverage_gaps() {
    local error_file="$1"

    log "${BLUE}Analyzing coverage gaps (attempt $ATTEMPT)${NC}"

    # Get modules with low coverage
    local low_coverage=$(parse_coverage "$error_file")

    if [[ -z "$low_coverage" ]]; then
        log "${GREEN}No low coverage modules found${NC}"
        return 0
    fi

    log "Modules with low coverage:"
    echo "$low_coverage"

    # Generate suggestions
    local suggestion_file="$LOG_DIR/coverage-suggestions-$(date +%s).txt"

    cat > "$suggestion_file" <<EOF
COVERAGE IMPROVEMENT SUGGESTIONS
================================
Generated: $(date)
Attempt: $ATTEMPT

Low Coverage Modules:
--------------------
$low_coverage

Recommendations:
----------------
1. Focus on modules below 80% coverage
2. Prioritize critical business logic
3. Test edge cases and error paths
4. Use property-based testing for complex functions

Generated Test Templates:
-------------------------
EOF

    # Try to generate test templates for low-coverage modules
    local templates_created=0
    while IFS= read -r line; do
        if [[ -n "$line" ]]; then
            # Extract module name and coverage
            local module=$(echo "$line" | awk '{print $1}' | sed 's/://g')
            local coverage=$(echo "$line" | grep -oE "[0-9]+%" || echo "0%")

            if suggest_tests_for_module "$module" "$coverage"; then
                ((templates_created++))
                echo "  - test/${module}_tests.erl" >> "$suggestion_file"
            fi
        fi
    done <<< "$low_coverage"

    cat >> "$suggestion_file" <<EOF

Next Steps:
-----------
1. Review generated test templates
2. Implement actual test logic (replace placeholders)
3. Run: rebar3 eunit --module=${module}_tests
4. Check coverage: rebar3 cover --verbose
5. Iterate until coverage >= 80%

Testing Best Practices:
-----------------------
- Test happy path and error cases
- Test boundary conditions
- Test concurrent scenarios if applicable
- Use property-based testing (PropEr) for complex logic
- Mock external dependencies

Suggestion file: $suggestion_file
EOF

    log "Coverage suggestions written to: $suggestion_file"
    cat "$suggestion_file"

    if [[ $templates_created -gt 0 ]]; then
        log "${GREEN}✓ Created $templates_created test template(s)${NC}"
        return 0
    else
        log "${YELLOW}⚠ No test templates created (files may already exist)${NC}"
        return 1
    fi
}

# Main
main() {
    if [[ -z "$ERROR_FILE" ]] || [[ ! -f "$ERROR_FILE" ]]; then
        echo "Usage: $0 <error_file> [attempt_number]"
        exit 1
    fi

    log "${BLUE}Test Coverage Agent starting (attempt $ATTEMPT)${NC}"

    if analyze_coverage_gaps "$ERROR_FILE"; then
        log "${GREEN}✅ Coverage analysis complete${NC}"
        log "${YELLOW}⚠ Manual test implementation still required${NC}"
        exit 0
    else
        log "${RED}❌ Coverage analysis failed${NC}"
        exit 1
    fi
}

main "$@"
