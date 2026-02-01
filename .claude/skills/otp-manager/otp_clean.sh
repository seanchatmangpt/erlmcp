#!/usr/bin/env bash
# .claude/skills/otp-manager/otp_clean.sh
# OTP Cleanup Script
#
# Purpose: Remove OTP build artifacts and cached files
# Spec: AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md:WO-006
#
# Behavior:
#   1. Remove build artifacts in /tmp/erlmcp-build/
#   2. Clear apt-get cache for erlang packages
#   3. Remove lock files in .erlmcp/cache/
#   4. Preserve installed OTP binaries (safe cleanup)
#
# Exit Codes:
#   0: Cleanup successful
#
# Safety:
#   - Does NOT uninstall OTP (only removes build artifacts)
#   - Does NOT remove user code or dependencies
#   - Does NOT affect rebar3 project builds

set -euo pipefail

#------------------------------------------------------------------------------
# Configuration
#------------------------------------------------------------------------------

ERLMCP_ROOT="${ERLMCP_ROOT:-/home/user/erlmcp}"
ERLMCP_CACHE="${ERLMCP_CACHE:-${ERLMCP_ROOT}/.erlmcp/cache}"
ERLMCP_LOG="${ERLMCP_LOG:-${ERLMCP_ROOT}/.erlmcp/otp-manager.log}"

# Directories to clean
BUILD_DIR="/tmp/erlmcp-build"
TEMP_ERLANG_FILES="/tmp/erlang-solutions.deb"

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

#------------------------------------------------------------------------------
# Size Calculation
#------------------------------------------------------------------------------

get_dir_size() {
    local dir="$1"
    if [[ -d "$dir" ]]; then
        du -sh "$dir" 2>/dev/null | awk '{print $1}' || echo "0B"
    else
        echo "0B"
    fi
}

get_apt_cache_size() {
    local size
    size=$(apt-cache policy esl-erlang 2>/dev/null | grep -c "esl-erlang" || echo "0")
    echo "${size} packages"
}

#------------------------------------------------------------------------------
# Cleanup Functions
#------------------------------------------------------------------------------

clean_build_artifacts() {
    log_info "Removing build artifacts..."

    local removed_size="0B"

    # Remove build directory
    if [[ -d "$BUILD_DIR" ]]; then
        removed_size=$(get_dir_size "$BUILD_DIR")
        rm -rf "$BUILD_DIR"
        log_info "  Removed: ${BUILD_DIR} (${removed_size})"
    fi

    # Remove temporary erlang-solutions.deb file
    if [[ -f "$TEMP_ERLANG_FILES" ]]; then
        rm -f "$TEMP_ERLANG_FILES"
        log_info "  Removed: ${TEMP_ERLANG_FILES}"
    fi

    # Remove other temporary erlang files
    rm -f /tmp/erlang-*.deb 2>/dev/null || true
    rm -f /tmp/erl_*.dump 2>/dev/null || true
    rm -f /tmp/erlmcp_verify_test.* 2>/dev/null || true

    echo "$removed_size"
}

clean_apt_cache() {
    log_info "Clearing apt cache for erlang packages..."

    local cache_info
    cache_info=$(get_apt_cache_size)

    # Clear apt-get cache (safe operation)
    sudo apt-get clean 2>&1 | tee -a "${ERLMCP_LOG}" || true

    log_info "  Cleared: apt-get cache (${cache_info})"

    echo "$cache_info"
}

clean_lock_files() {
    log_info "Removing lock files..."

    local removed_count=0

    if [[ -d "$ERLMCP_CACHE" ]]; then
        # Remove OTP version lock file (will be recreated on next fetch-build)
        if [[ -f "${ERLMCP_CACHE}/otp-version.lock" ]]; then
            rm -f "${ERLMCP_CACHE}/otp-version.lock"
            removed_count=$((removed_count + 1))
            log_info "  Removed: ${ERLMCP_CACHE}/otp-version.lock"
        fi

        # Remove sessionstart lock file
        if [[ -f "${ERLMCP_CACHE}/sessionstart.lock" ]]; then
            rm -f "${ERLMCP_CACHE}/sessionstart.lock"
            removed_count=$((removed_count + 1))
            log_info "  Removed: ${ERLMCP_CACHE}/sessionstart.lock"
        fi
    fi

    echo "$removed_count"
}

preserve_otp_installation() {
    log_info "Preserving OTP installation..."

    # Check if OTP is installed
    if command -v erl &> /dev/null; then
        local otp_location
        otp_location=$(command -v erl | xargs dirname | xargs dirname)
        local otp_version
        otp_version=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt(0).' 2>/dev/null || echo "unknown")

        log_info "  Preserved: OTP ${otp_version} at ${otp_location}"
    else
        log_warn "  No OTP installation found"
    fi
}

#------------------------------------------------------------------------------
# Main Execution
#------------------------------------------------------------------------------

main() {
    log_info "====== OTP Cleanup Starting ======"

    # Create log directory
    mkdir -p "$(dirname "$ERLMCP_LOG")"

    # Clean build artifacts
    local artifacts_size
    artifacts_size=$(clean_build_artifacts)

    # Clean apt cache
    local cache_size
    cache_size=$(clean_apt_cache)

    # Clean lock files
    local lock_count
    lock_count=$(clean_lock_files)

    # Preserve OTP installation (informational only)
    preserve_otp_installation

    # Summary
    log_info ""
    log_info "✅ OTP cleanup complete"
    log_info "   Removed: ${artifacts_size} build artifacts"
    log_info "   Cleared: ${cache_size} apt cache"
    log_info "   Removed: ${lock_count} lock files"
    log_info ""
    log_info "====== OTP Cleanup Complete ======"

    exit 0
}

# Execute main function
main "$@"
