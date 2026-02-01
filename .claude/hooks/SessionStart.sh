#!/usr/bin/env bash
# .claude/hooks/SessionStart.sh
# SessionStart Hook for erlmcp - OTP 28.3.1 Bootstrap
#
# Purpose: Idempotent OTP detection, installation, and environment setup for cloud sessions
# Spec: AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md:WO-001
# Reference: CLAUDE_CODE_WEB_GOVERNANCE_SYSTEM.md:103-189
#
# Behavior:
#   1. Detect OTP version (erl -eval)
#   2. Install OTP 28.3.1 if missing or version < 28
#   3. Persist environment variables (CLAUDE_CODE_REMOTE, ERLMCP_PROFILE, etc.)
#   4. Pre-compile erlmcp_core (warm cache)
#   5. Error recovery: if OTP install fails, clear cache and retry
#
# Idempotency: Safe to run multiple times - checks before installing
# Cloud-safe: Uses official Erlang Solutions package repository
# Error-resistant: Handles missing erl, apt failures, network timeouts

set -euo pipefail

#------------------------------------------------------------------------------
# Configuration
#------------------------------------------------------------------------------

OTP_REQUIRED_MAJOR=28
OTP_REQUIRED_MINOR=3
OTP_REQUIRED_PATCH=1
OTP_REQUIRED_VERSION="${OTP_REQUIRED_MAJOR}.${OTP_REQUIRED_MINOR}.${OTP_REQUIRED_PATCH}"

# Test mode: skip installation if set (for testing purposes)
TEST_MODE="${SESSIONSTART_TEST_MODE:-false}"

ERLANG_SOLUTIONS_DEB="https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb"
ERLANG_PACKAGE="esl-erlang=1:${OTP_REQUIRED_VERSION}-1"

ERLMCP_ROOT="${ERLMCP_ROOT:-/home/user/erlmcp}"
ERLMCP_CACHE="${ERLMCP_CACHE:-${ERLMCP_ROOT}/.erlmcp/cache}"
ERLMCP_LOG="${ERLMCP_LOG:-${ERLMCP_ROOT}/.erlmcp/sessionstart.log}"

# Lock file for idempotency
LOCK_FILE="${ERLMCP_CACHE}/sessionstart.lock"

# Retry configuration
MAX_RETRIES=3
RETRY_DELAY=5

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
    # Only append to log file if directory exists
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
    # For OTP 28+, we get full version from system_info
    if [[ "$version" =~ ^[0-9]+$ ]]; then
        # Only major version available, get full version differently
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

    while [[ $retry_count -lt $MAX_RETRIES ]]; do
        if attempt_otp_install; then
            install_success=true
            break
        fi

        retry_count=$((retry_count + 1))
        if [[ $retry_count -lt $MAX_RETRIES ]]; then
            log_warn "OTP installation failed (attempt ${retry_count}/${MAX_RETRIES}). Retrying in ${RETRY_DELAY}s..."
            sleep $RETRY_DELAY

            # Clear cache on retry
            log_info "Clearing cache for retry..."
            rm -rf "${ERLMCP_CACHE}"/*
            sudo apt-get clean || true
        fi
    done

    if [[ "$install_success" = false ]]; then
        log_error "OTP installation failed after ${MAX_RETRIES} attempts"
        return 1
    fi

    log_info "OTP ${OTP_REQUIRED_VERSION} installation complete"
    return 0
}

attempt_otp_install() {
    local temp_deb="/tmp/erlang-solutions.deb"

    # Download Erlang Solutions repository package
    log_info "Downloading Erlang Solutions repository package..."
    if ! wget -q -O "$temp_deb" "$ERLANG_SOLUTIONS_DEB"; then
        log_error "Failed to download Erlang Solutions package"
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
# Environment Setup
#------------------------------------------------------------------------------

setup_environment() {
    log_info "Setting up environment variables..."

    # Create cache directory
    mkdir -p "$ERLMCP_CACHE"
    mkdir -p "${ERLMCP_ROOT}/.erlmcp/receipts"
    mkdir -p "${ERLMCP_ROOT}/.erlmcp/transcripts"

    # Persist environment variables
    # Note: In real Claude Code Web, this would use CLAUDE_ENV_FILE
    # For testing, we'll export to a sourceable file
    local env_file="${ERLMCP_CACHE}/session.env"

    cat > "$env_file" << ENVEOF
export CLAUDE_CODE_REMOTE=true
export ERLMCP_PROFILE=cloud
export ERLMCP_CACHE="${ERLMCP_CACHE}"
export TERM=dumb
export REBAR_COLOR=none
export ERL_AFLAGS="-kernel shell_history enabled"
ENVEOF

    # Add build hash if in git repo
    if [[ -d "${ERLMCP_ROOT}/.git" ]]; then
        echo "export ERLMCP_BUILD_HASH=\"$(cd "$ERLMCP_ROOT" && git rev-parse HEAD 2>/dev/null || echo 'unknown')\"" >> "$env_file"
    fi

    # Source environment file
    source "$env_file"

    log_info "Environment variables set:"
    log_info "  CLAUDE_CODE_REMOTE=${CLAUDE_CODE_REMOTE}"
    log_info "  ERLMCP_PROFILE=${ERLMCP_PROFILE}"
    log_info "  ERLMCP_CACHE=${ERLMCP_CACHE}"
    log_info "  ERLMCP_BUILD_HASH=${ERLMCP_BUILD_HASH:-unknown}"
}

#------------------------------------------------------------------------------
# Pre-compilation
#------------------------------------------------------------------------------

precompile_core() {
    log_info "Pre-compiling erlmcp_core..."

    cd "$ERLMCP_ROOT"

    # Fetch dependencies if needed
    if [[ ! -d "_build/default/lib" ]]; then
        log_info "Fetching dependencies..."
        if ! TERM=dumb rebar3 get-deps 2>&1 | tee -a "${ERLMCP_LOG}"; then
            log_warn "Failed to fetch dependencies (continuing anyway)"
        fi
    fi

    # Compile erlmcp_core (attempt, but non-fatal if fails)
    log_info "Compiling erlmcp_core application..."
    if command -v rebar3 &> /dev/null; then
        # Try to compile, redirecting output to log
        if TERM=dumb rebar3 compile 2>&1 | tee -a "${ERLMCP_LOG}"; then
            log_info "erlmcp_core compilation successful"
            return 0
        else
            log_warn "erlmcp_core compilation failed (non-fatal)"
            return 1
        fi
    else
        log_warn "rebar3 not found, skipping compilation (non-fatal)"
        return 1
    fi
}

#------------------------------------------------------------------------------
# Idempotency Check
#------------------------------------------------------------------------------

is_already_initialized() {
    if [[ ! -f "$LOCK_FILE" ]]; then
        return 1
    fi

    local lock_version
    lock_version=$(cat "$LOCK_FILE" 2>/dev/null || echo "0.0.0")

    if compare_versions "$lock_version" "$OTP_REQUIRED_VERSION"; then
        log_info "SessionStart already completed (OTP ${lock_version} >= ${OTP_REQUIRED_VERSION})"
        return 0
    fi

    log_info "Lock file exists but OTP version ${lock_version} < ${OTP_REQUIRED_VERSION}, re-initializing..."
    return 1
}

create_lock_file() {
    local current_version
    current_version=$(get_otp_version)
    echo "$current_version" > "$LOCK_FILE"
    log_info "Lock file created with OTP version ${current_version}"
}

#------------------------------------------------------------------------------
# Main Execution
#------------------------------------------------------------------------------

main() {
    log_info "====== SessionStart Hook Starting ======"
    log_info "erlmcp root: ${ERLMCP_ROOT}"
    log_info "Required OTP version: ${OTP_REQUIRED_VERSION}"

    # Create directories
    mkdir -p "$(dirname "$ERLMCP_LOG")"
    mkdir -p "$ERLMCP_CACHE"

    # Check if already initialized
    if is_already_initialized; then
        log_info "SessionStart hook already completed (idempotent check passed)"
        log_info "====== SessionStart Hook Complete (cached) ======"
        exit 0
    fi

    # Detect current OTP version
    local current_version
    current_version=$(get_otp_version)
    log_info "Current OTP version: ${current_version}"

    # Check if OTP upgrade needed
    if compare_versions "$current_version" "$OTP_REQUIRED_VERSION"; then
        log_info "OTP version ${current_version} >= ${OTP_REQUIRED_VERSION} (satisfies requirement)"
    else
        log_info "OTP version ${current_version} < ${OTP_REQUIRED_VERSION}, installation required"

        if [[ "$TEST_MODE" = "true" ]]; then
            log_warn "TEST_MODE enabled: skipping OTP installation"
            log_warn "Continuing with current OTP version ${current_version}"
        else
            if ! install_otp; then
                log_error "Failed to install OTP ${OTP_REQUIRED_VERSION}"
                exit 1
            fi

            # Verify installation
            current_version=$(get_otp_version)
            if ! compare_versions "$current_version" "$OTP_REQUIRED_VERSION"; then
                log_error "OTP installation verification failed: ${current_version} < ${OTP_REQUIRED_VERSION}"
                exit 1
            fi
        fi
    fi

    # Setup environment
    setup_environment

    # Pre-compile core
    precompile_core || true  # Non-fatal

    # Create lock file
    create_lock_file

    log_info "====== SessionStart Hook Complete ======"
    echo "SessionStart complete. OTP $(get_otp_version) ready. Build hash: ${ERLMCP_BUILD_HASH:-unknown}"

    exit 0
}

# Execute main function
main "$@"
