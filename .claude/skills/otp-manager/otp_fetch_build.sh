#!/usr/bin/env bash
# .claude/skills/otp-manager/otp_fetch_build.sh
# OTP 28.3.1 Download and Build Script
#
# Purpose: Download and install OTP 28.3.1 from Erlang Solutions
# Spec: AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md:WO-006
# Reference: .claude/hooks/SessionStart.sh (reuses installation logic)
#
# Behavior:
#   1. Check if OTP 28.3.1 is already installed (idempotent)
#   2. Download erlang-solutions repository package
#   3. Install OTP 28.3.1 via apt-get
#   4. Verify installation success
#   5. Create lock file for future idempotency checks
#
# Exit Codes:
#   0: Success (OTP installed or already present)
#   1: Installation failed (network, permissions, or package errors)
#
# Idempotency: Safe to run multiple times - checks version before installing

set -euo pipefail

#------------------------------------------------------------------------------
# Configuration
#------------------------------------------------------------------------------

OTP_REQUIRED_MAJOR=28
OTP_REQUIRED_MINOR=3
OTP_REQUIRED_PATCH=1
OTP_REQUIRED_VERSION="${OTP_REQUIRED_MAJOR}.${OTP_REQUIRED_MINOR}.${OTP_REQUIRED_PATCH}"

ERLANG_SOLUTIONS_DEB="https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb"
ERLANG_PACKAGE="esl-erlang=1:${OTP_REQUIRED_VERSION}-1"

# Find project root (parent of .claude directory)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ERLMCP_ROOT="${ERLMCP_ROOT:-$(cd "$SCRIPT_DIR/../.." && pwd)}"
ERLMCP_CACHE="${ERLMCP_CACHE:-${ERLMCP_ROOT}/.erlmcp/cache}"
ERLMCP_LOG="${ERLMCP_LOG:-${ERLMCP_ROOT}/.erlmcp/otp-manager.log}"

# Lock file for idempotency
LOCK_FILE="${ERLMCP_CACHE}/otp-version.lock"

# Retry configuration
MAX_RETRIES=3
RETRY_DELAY=2

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

#------------------------------------------------------------------------------
# OTP Installation
#------------------------------------------------------------------------------

install_otp() {
    log_info "Installing OTP ${OTP_REQUIRED_VERSION}..."

    local retry_count=0
    local install_success=false
    local start_time=$(date +%s)

    while [[ $retry_count -lt $MAX_RETRIES ]]; do
        if attempt_otp_install; then
            install_success=true
            break
        fi

        retry_count=$((retry_count + 1))
        if [[ $retry_count -lt $MAX_RETRIES ]]; then
            local delay=$((RETRY_DELAY * retry_count))
            log_warn "OTP installation failed (attempt ${retry_count}/${MAX_RETRIES}). Retrying in ${delay}s..."
            sleep $delay

            # Clear cache on retry
            log_info "Clearing cache for retry..."
            sudo apt-get clean || true
        fi
    done

    if [[ "$install_success" = false ]]; then
        log_error "❌ OTP installation failed after ${MAX_RETRIES} attempts"
        return 1
    fi

    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    log_info "✅ OTP ${OTP_REQUIRED_VERSION} installation complete"
    log_info "   Version: $(get_otp_version)"
    log_info "   Location: $(command -v erl | xargs dirname | xargs dirname)"
    log_info "   Time: ${duration}s"

    return 0
}

attempt_otp_install() {
    local temp_deb="/tmp/erlang-solutions.deb"

    # Download Erlang Solutions repository package
    log_info "Downloading Erlang Solutions repository package..."
    if ! wget -q -O "$temp_deb" "$ERLANG_SOLUTIONS_DEB" 2>&1 | tee -a "${ERLMCP_LOG}"; then
        log_error "Failed to download Erlang Solutions package"
        rm -f "$temp_deb"
        return 1
    fi

    # Install repository package
    log_info "Installing Erlang Solutions repository..."
    if ! sudo dpkg -i "$temp_deb" 2>&1 | tee -a "${ERLMCP_LOG}"; then
        log_error "Failed to install Erlang Solutions repository"
        rm -f "$temp_deb"
        return 1
    fi

    rm -f "$temp_deb"

    # Update package list
    log_info "Updating package list..."
    if ! sudo apt-get update 2>&1 | tee -a "${ERLMCP_LOG}"; then
        log_error "Failed to update package list"
        return 1
    fi

    # Install OTP
    log_info "Installing ${ERLANG_PACKAGE}..."
    if ! sudo DEBIAN_FRONTEND=noninteractive apt-get install -y "$ERLANG_PACKAGE" 2>&1 | tee -a "${ERLMCP_LOG}"; then
        log_error "Failed to install OTP package"
        return 1
    fi

    # Verify installation
    local installed_version
    installed_version=$(get_otp_version)
    log_info "Installed OTP version: ${installed_version}"

    if ! compare_versions "$installed_version" "$OTP_REQUIRED_VERSION"; then
        log_error "Installed version ${installed_version} is less than required ${OTP_REQUIRED_VERSION}"
        return 1
    fi

    return 0
}

#------------------------------------------------------------------------------
# Idempotency Check
#------------------------------------------------------------------------------

is_already_installed() {
    local current_version
    current_version=$(get_otp_version)

    if compare_versions "$current_version" "$OTP_REQUIRED_VERSION"; then
        log_info "✅ OTP ${current_version} already installed (>= ${OTP_REQUIRED_VERSION})"
        log_info "   Skipping installation (idempotent)"
        return 0
    fi

    log_info "OTP ${current_version} < ${OTP_REQUIRED_VERSION}, installation required"
    return 1
}

create_lock_file() {
    local current_version
    current_version=$(get_otp_version)
    mkdir -p "$(dirname "$LOCK_FILE")"
    echo "$current_version" > "$LOCK_FILE"
    log_info "Lock file created: ${LOCK_FILE} (version ${current_version})"
}

#------------------------------------------------------------------------------
# Main Execution
#------------------------------------------------------------------------------

main() {
    log_info "====== OTP Fetch-Build Starting ======"
    log_info "Required OTP version: ${OTP_REQUIRED_VERSION}"

    # Create directories
    mkdir -p "$(dirname "$ERLMCP_LOG")"
    mkdir -p "$ERLMCP_CACHE"

    # Check if already installed (idempotent)
    if is_already_installed; then
        create_lock_file  # Update lock file even if already installed
        log_info "====== OTP Fetch-Build Complete (cached) ======"
        exit 0
    fi

    # Install OTP
    if ! install_otp; then
        log_error "====== OTP Fetch-Build Failed ======"
        exit 1
    fi

    # Create lock file
    create_lock_file

    log_info "====== OTP Fetch-Build Complete ======"
    exit 0
}

# Execute main function
main "$@"
