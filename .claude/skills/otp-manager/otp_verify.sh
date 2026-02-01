#!/usr/bin/env bash
# .claude/skills/otp-manager/otp_verify.sh
# OTP Verification Script
#
# Purpose: Check OTP version and verify compilation readiness
# Spec: AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md:WO-006
#
# Behavior:
#   1. Detect installed OTP version
#   2. Compare against required version (28.3.1)
#   3. Test rebar3 availability
#   4. Return structured exit code
#
# Exit Codes:
#   0: Verification passed (OTP >= 28.3.1 and rebar3 available)
#   1: OTP not found or version too low
#   2: OTP found but rebar3 missing
#   3: OTP found but compilation test failed

set -euo pipefail

#------------------------------------------------------------------------------
# Configuration
#------------------------------------------------------------------------------

OTP_REQUIRED_MAJOR=28
OTP_REQUIRED_MINOR=3
OTP_REQUIRED_PATCH=1
OTP_REQUIRED_VERSION="${OTP_REQUIRED_MAJOR}.${OTP_REQUIRED_MINOR}.${OTP_REQUIRED_PATCH}"

ERLMCP_ROOT="${ERLMCP_ROOT:-/home/user/erlmcp}"
ERLMCP_LOG="${ERLMCP_LOG:-${ERLMCP_ROOT}/.erlmcp/otp-manager.log}"

#------------------------------------------------------------------------------
# Logging
#------------------------------------------------------------------------------

log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp=$(date -Iseconds)
    local log_message="[${timestamp}] [${level}] ${message}"
    echo "$log_message"

    # Append to log file if directory exists
    if [[ -d "$(dirname "$ERLMCP_LOG")" ]]; then
        echo "$log_message" >> "${ERLMCP_LOG}"
    fi
}

log_info() { log "INFO" "$@"; }
log_warn() { log "WARN" "$@"; }
log_error() { log "ERROR" "$@"; }

#------------------------------------------------------------------------------
# OTP Version Detection
#------------------------------------------------------------------------------

get_otp_version() {
    if ! command -v erl &> /dev/null; then
        echo "0.0.0"
        return 1
    fi

    local version
    version=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt(0).' 2>/dev/null || echo "0")

    # Version format: "28" or "28.3.1" (varies by OTP version)
    if [[ "$version" =~ ^[0-9]+$ ]]; then
        # Only major version available, get full version from OTP_VERSION file
        version=$(erl -noshell -eval '
            {ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])),
            io:format("~s", [string:trim(Version)]),
            halt(0).
        ' 2>/dev/null || echo "${version}.0.0")
    fi

    echo "$version"
}

compare_versions() {
    local current="$1"
    local required="$2"

    # Parse versions
    IFS='.' read -r -a current_parts <<< "$current"
    IFS='.' read -r -a required_parts <<< "$required"

    # Compare major
    if [[ ${current_parts[0]:-0} -lt ${required_parts[0]:-0} ]]; then
        return 1
    elif [[ ${current_parts[0]:-0} -gt ${required_parts[0]:-0} ]]; then
        return 0
    fi

    # Compare minor
    if [[ ${current_parts[1]:-0} -lt ${required_parts[1]:-0} ]]; then
        return 1
    elif [[ ${current_parts[1]:-0} -gt ${required_parts[1]:-0} ]]; then
        return 0
    fi

    # Compare patch
    if [[ ${current_parts[2]:-0} -lt ${required_parts[2]:-0} ]]; then
        return 1
    fi

    return 0
}

get_rebar3_version() {
    if ! command -v rebar3 &> /dev/null; then
        echo "not found"
        return 1
    fi

    local version
    version=$(rebar3 version 2>/dev/null | head -n 1 | awk '{print $2}' || echo "unknown")
    echo "$version"
}

#------------------------------------------------------------------------------
# Verification Checks
#------------------------------------------------------------------------------

verify_otp() {
    log_info "====== OTP Verification Starting ======"

    # Check 1: OTP version
    local current_version
    current_version=$(get_otp_version)
    log_info "Installed OTP version: ${current_version}"

    if [[ "$current_version" == "0.0.0" ]]; then
        log_error "❌ OTP not found"
        log_error "   erl command not available"
        log_error "   Run: /otp-manager fetch-build"
        return 1
    fi

    if ! compare_versions "$current_version" "$OTP_REQUIRED_VERSION"; then
        log_error "❌ OTP version too low"
        log_error "   Installed: ${current_version}"
        log_error "   Required:  ${OTP_REQUIRED_VERSION}"
        log_error "   Run: /otp-manager fetch-build"
        return 1
    fi

    log_info "✅ OTP version check passed"

    # Check 2: rebar3 availability
    local rebar3_version
    rebar3_version=$(get_rebar3_version)

    if [[ "$rebar3_version" == "not found" ]]; then
        log_error "❌ rebar3 not found"
        log_error "   Install: wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3"
        return 2
    fi

    log_info "✅ rebar3 check passed (version: ${rebar3_version})"

    # Check 3: Basic compilation test
    log_info "Testing basic compilation..."

    local test_erl="/tmp/erlmcp_verify_test.erl"
    cat > "$test_erl" << 'EOF'
-module(erlmcp_verify_test).
-export([test/0]).

test() -> ok.
EOF

    if ! erlc "$test_erl" 2>&1 | tee -a "${ERLMCP_LOG}"; then
        log_error "❌ Compilation test failed"
        log_error "   OTP compiler not working"
        rm -f "$test_erl" /tmp/erlmcp_verify_test.beam
        return 3
    fi

    rm -f "$test_erl" /tmp/erlmcp_verify_test.beam

    log_info "✅ Compilation test passed"

    # All checks passed
    log_info ""
    log_info "✅ OTP verification passed"
    log_info "   Installed: ${current_version}"
    log_info "   Required:  ${OTP_REQUIRED_VERSION}"
    log_info "   rebar3:    ${rebar3_version}"
    log_info "   Status:    READY"
    log_info ""
    log_info "====== OTP Verification Complete ======"

    return 0
}

#------------------------------------------------------------------------------
# Main Execution
#------------------------------------------------------------------------------

main() {
    # Create log directory
    mkdir -p "$(dirname "$ERLMCP_LOG")"

    # Run verification
    if verify_otp; then
        exit 0
    else
        local exit_code=$?
        log_info "====== OTP Verification Failed (exit code: ${exit_code}) ======"
        exit $exit_code
    fi
}

# Execute main function
main "$@"
