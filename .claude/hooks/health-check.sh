#!/usr/bin/env bash
# .claude/hooks/health-check.sh
# Hook health monitoring and validation
# Purpose: Verify all hooks are executable, configured, and functional
#
# Usage:
#   .claude/hooks/health-check.sh              # Check all hooks
#   .claude/hooks/health-check.sh --verbose    # Detailed output
#   .claude/hooks/health-check.sh --fix        # Fix permissions
#
# Exit codes:
#   0 - All healthy
#   1 - Issues found
#   2 - Critical failures

set -euo pipefail

# ============================================================================
# Configuration
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
HOOKS_DIR="$SCRIPT_DIR"
SETTINGS_FILE="$PROJECT_ROOT/.claude/settings.json"

# Colors
readonly GREEN='\033[0;32m'
readonly RED='\033[0;31m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# Counters
CHECKS_PASSED=0
CHECKS_FAILED=0
CHECKS_WARNED=0

# Options
VERBOSE="${VERBOSE:-false}"
FIX_MODE="${FIX_MODE:-false}"

# ============================================================================
# Output Functions
# ============================================================================

log_header() {
    echo -e "${BLUE}==== $*${NC}"
}

log_pass() {
    echo -e "${GREEN}✓ PASS:${NC} $*"
    CHECKS_PASSED=$((CHECKS_PASSED + 1))
}

log_fail() {
    echo -e "${RED}✗ FAIL:${NC} $*"
    CHECKS_FAILED=$((CHECKS_FAILED + 1))
}

log_warn() {
    echo -e "${YELLOW}⚠ WARN:${NC} $*"
    CHECKS_WARNED=$((CHECKS_WARNED + 1))
}

log_info() {
    if [[ "$VERBOSE" == "true" ]]; then
        echo -e "${BLUE}  INFO:${NC} $*"
    fi
}

# ============================================================================
# Check Functions
# ============================================================================

# Check if hook file exists and is executable
check_hook_executable() {
    local hook="$1"
    local hook_name="$(basename "$hook")"

    if [[ ! -f "$hook" ]]; then
        log_fail "$hook_name: File not found"
        return 1
    fi

    # Check if executable
    if [[ ! -x "$hook" ]]; then
        log_fail "$hook_name: Not executable"

        # Try to fix
        if [[ "$FIX_MODE" == "true" ]]; then
            chmod +x "$hook" 2>/dev/null && log_pass "$hook_name: Fixed permissions" || true
        fi
        return 1
    fi

    log_pass "$hook_name: Executable"
    return 0
}

# Check hook syntax with bash -n
check_hook_syntax() {
    local hook="$1"
    local hook_name="$(basename "$hook")"

    # Skip if not executable
    if [[ ! -x "$hook" ]]; then
        return 1
    fi

    # Syntax check
    if bash -n "$hook" 2>/dev/null; then
        log_pass "$hook_name: Syntax valid"
        return 0
    else
        local error_output
        error_output=$(bash -n "$hook" 2>&1 || true)
        log_fail "$hook_name: Syntax error"
        log_info "  Error: $error_output"
        return 1
    fi
}

# Check if hook is properly configured in settings.json
check_hook_configured() {
    local hook_name="$1"
    local hook_path="$2"

    if [[ ! -f "$SETTINGS_FILE" ]]; then
        log_fail "settings.json: Not found"
        return 1
    fi

    # Extract filename from hook path
    local hook_file="$(basename "$hook_path")"

    # Check if hook is referenced in settings.json
    if grep -q "$hook_file" "$SETTINGS_FILE" 2>/dev/null; then
        log_pass "$hook_name: Configured in settings.json"
        return 0
    else
        log_warn "$hook_name: Not found in settings.json (may be optional)"
        return 1
    fi
}

# Check if hook implements proper JSON interface (for PreToolUse hooks)
check_hook_json_interface() {
    local hook="$1"
    local hook_name="$(basename "$hook")"

    # Only check policy hooks
    if [[ ! "$hook_name" =~ ^policy- ]]; then
        return 0
    fi

    # Check for output_decision function or permissionDecision string
    if grep -q "permissionDecision" "$hook" 2>/dev/null; then
        log_pass "$hook_name: JSON interface implemented"
        return 0
    else
        log_warn "$hook_name: May not implement JSON interface"
        return 1
    fi
}

# Check if hook sources hook-lib.sh
check_hook_uses_lib() {
    local hook="$1"
    local hook_name="$(basename "$hook")"

    if grep -q "hook-lib.sh" "$hook" 2>/dev/null; then
        log_pass "$hook_name: Uses hook-lib.sh"
        return 0
    else
        log_info "$hook_name: Does not use hook-lib.sh (optional)"
        return 0
    fi
}

# Check OTP environment
check_otp_env() {
    local env_file="${PROJECT_ROOT}/.erlmcp/env.sh"

    if [[ -f "$env_file" ]]; then
        log_pass "OTP env.sh: Exists"

        # Try to source and validate
        if source "$env_file" 2>/dev/null; then
            local otp_bin="${ERLMCP_OTP_BIN:-erl}"
            if command -v "$otp_bin" &> /dev/null; then
                log_pass "OTP binary: Found ($otp_bin)"
                return 0
            fi
        fi
    else
        log_warn "OTP env.sh: Not found (will be created by SessionStart)"
    fi

    return 1
}

# Check settings.json syntax
check_settings_syntax() {
    if [[ ! -f "$SETTINGS_FILE" ]]; then
        log_fail "settings.json: Not found"
        return 1
    fi

    # Check with jq if available
    if command -v jq &> /dev/null; then
        if jq empty "$SETTINGS_FILE" 2>/dev/null; then
            log_pass "settings.json: Valid JSON"
            return 0
        else
            log_fail "settings.json: Invalid JSON"
            return 1
        fi
    else
        log_info "settings.json: jq not available for validation"
        return 0
    fi
}

# Check hook dependencies
check_hook_dependencies() {
    local deps_ok=true

    # Check for jq (optional but recommended)
    if command -v jq &> /dev/null; then
        log_pass "jq: Installed"
    else
        log_warn "jq: Not installed (hooks will use fallback parsing)"
    fi

    # Check for basic utilities
    local required_utils=("grep" "sed" "awk")
    for util in "${required_utils[@]}"; do
        if command -v "$util" &> /dev/null; then
            log_pass "$util: Available"
        else
            log_fail "$util: Not found"
            deps_ok=false
        fi
    done

    if [[ "$deps_ok" == "true" ]]; then
        return 0
    else
        return 1
    fi
}

# ============================================================================
# Main Check Runner
# ============================================================================

run_all_checks() {
    log_header "Hook Health Check"
    echo ""

    # 1. Check settings.json
    log_header "1. Settings File"
    check_settings_syntax
    echo ""

    # 2. Check OTP environment
    log_header "2. OTP Environment"
    check_otp_env
    echo ""

    # 3. Check dependencies
    log_header "3. Dependencies"
    check_hook_dependencies
    echo ""

    # 4. Check individual hooks
    log_header "4. Hook Files"

    # List of hooks to check (in priority order)
    local hooks=(
        "SessionStart.sh"
        "policy-bash.sh"
        "policy-websearch.sh"
        "policy-write.sh"
        "post-write-ci.sh"
        "receipt.sh"
        "post-git-commit.sh"
    )

    for hook_name in "${hooks[@]}"; do
        local hook_path="${HOOKS_DIR}/${hook_name}"

        echo -e "${BLUE}Checking: ${hook_name}${NC}"

        if [[ -f "$hook_path" ]]; then
            check_hook_executable "$hook_path"
            check_hook_syntax "$hook_path"
            check_hook_configured "$hook_name" "$hook_path"
            check_hook_json_interface "$hook_path"
            check_hook_uses_lib "$hook_path"
        else
            log_warn "$hook_name: Not found (optional)"
        fi
        echo ""
    done
}

# ============================================================================
# Summary and Exit
# ============================================================================

print_summary() {
    log_header "Summary"

    local total=$((CHECKS_PASSED + CHECKS_FAILED + CHECKS_WARNED))
    echo "Total checks: $total"
    echo -e "${GREEN}Passed:${NC} $CHECKS_PASSED"
    echo -e "${YELLOW}Warnings:${NC} $CHECKS_WARNED"
    echo -e "${RED}Failed:${NC} $CHECKS_FAILED"
    echo ""

    # Exit code based on failures
    if [[ $CHECKS_FAILED -eq 0 ]]; then
        echo -e "${GREEN}✓ All critical checks passed${NC}"
        return 0
    else
        echo -e "${RED}✗ Some checks failed${NC}"
        echo "Run with --fix to attempt automatic repairs"
        return 1
    fi
}

# ============================================================================
# Main
# ============================================================================

main() {
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --verbose|-v)
                VERBOSE=true
                shift
                ;;
            --fix|-f)
                FIX_MODE=true
                shift
                ;;
            --help|-h)
                echo "Usage: $0 [OPTIONS]"
                echo ""
                echo "Options:"
                echo "  --verbose, -v     Show detailed output"
                echo "  --fix, -f         Attempt to fix permission issues"
                echo "  --help, -h        Show this help"
                exit 0
                ;;
            *)
                echo "Unknown option: $1" >&2
                exit 2
                ;;
        esac
    done

    # Export options for sub-functions
    export VERBOSE
    export FIX_MODE

    # Run checks
    run_all_checks

    # Print summary and exit
    print_summary
}

main "$@"
