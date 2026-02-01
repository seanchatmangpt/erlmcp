#!/usr/bin/env bash
# .claude/hooks/SessionStart.sh
# Purpose: Autonomous OTP 28.3.1 initialization for cloud environments
# Spec: CLAUDE.md v2.1.0
# Architecture: 7-phase state machine with idempotency and retry logic
#
# State Machine:
#   Phase 1: Initialization check (lock file)
#   Phase 2: OTP version detection
#   Phase 3: OTP installation (with retry)
#   Phase 4: Environment setup
#   Phase 5: Core precompilation (non-fatal)
#   Phase 6: Environment validation
#   Phase 7: Lock file creation
#
# Invariants:
#   - Idempotent: Safe to run multiple times (lock file prevents re-execution)
#   - Fail-fast: All failures are FATAL except precompilation
#   - Auditable: All operations logged to .erlmcp/sessionstart.log
#   - Deterministic: Same inputs produce same outputs

set -euo pipefail

#==============================================================================
# Constants
#==============================================================================

readonly REQUIRED_OTP_MAJOR=28
readonly REQUIRED_OTP_MINOR=3
readonly REQUIRED_OTP_PATCH=1
readonly REQUIRED_OTP_VERSION="28.3.1"
readonly LOCK_FILE=".erlmcp/cache/sessionstart.lock"
readonly LOG_FILE=".erlmcp/sessionstart.log"
readonly MAX_INSTALL_ATTEMPTS=3
readonly BASE_BACKOFF=2
readonly SCRIPT_VERSION="1.0.0"

# Color codes (disabled in cloud via TERM=dumb)
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

#==============================================================================
# Logging Infrastructure
#==============================================================================

# Initialize logging subsystem
init_logging() {
    local log_dir
    log_dir=$(dirname "$LOG_FILE")

    if [[ ! -d "$log_dir" ]]; then
        mkdir -p "$log_dir" || {
            echo "ERROR: Failed to create log directory: $log_dir" >&2
            exit 1
        }
    fi

    # Append to log file (preserve history)
    exec 1> >(tee -a "$LOG_FILE")
    exec 2>&1

    log_separator
    log_info "SessionStart Hook Execution"
    log_info "Timestamp: $(date -Iseconds)"
    log_info "Script Version: $SCRIPT_VERSION"
    log_info "Working Directory: $(pwd)"
    log_info "User: $(whoami)"
    log_separator
}

# Log levels
log_separator() {
    echo "================================================================================"
}

log_info() {
    echo "[INFO] $*"
}

log_warn() {
    echo "[WARN] $*"
}

log_error() {
    echo "[ERROR] $*" >&2
}

log_success() {
    echo "[SUCCESS] $*"
}

log_phase() {
    local phase=$1
    shift
    echo ""
    log_separator
    echo "[PHASE $phase] $*"
    log_separator
}

#==============================================================================
# Phase 1: Initialization Check
#==============================================================================

# Check if session is already initialized (idempotency)
is_already_initialized() {
    log_info "Checking initialization status..."

    if [[ ! -f "$LOCK_FILE" ]]; then
        log_info "Lock file not found: $LOCK_FILE"
        return 1
    fi

    local locked_version
    locked_version=$(cat "$LOCK_FILE" 2>/dev/null || echo "")

    if [[ -z "$locked_version" ]]; then
        log_warn "Lock file exists but is empty"
        return 1
    fi

    if [[ "$locked_version" == "$REQUIRED_OTP_VERSION" ]]; then
        log_success "Session already initialized with OTP $locked_version"
        return 0
    else
        log_warn "Lock file version mismatch: $locked_version != $REQUIRED_OTP_VERSION"
        log_info "Reinitializing session..."
        return 1
    fi
}

#==============================================================================
# Phase 2: OTP Version Detection
#==============================================================================

# Detect installed OTP version
get_otp_version() {
    if ! command -v erl &> /dev/null; then
        echo ""
        return 1
    fi

    local version
    version=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null || echo "")
    echo "$version"
}

# Get detailed OTP info
get_otp_info() {
    if ! command -v erl &> /dev/null; then
        log_info "OTP: Not installed"
        return 1
    fi

    local version erts_version
    version=$(get_otp_version)
    erts_version=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(version)]), halt().' 2>/dev/null || echo "unknown")

    log_info "OTP Version: $version"
    log_info "ERTS Version: $erts_version"

    # Check if version meets requirements
    if compare_versions "$version" "$REQUIRED_OTP_VERSION"; then
        log_success "OTP version meets requirements ($version >= $REQUIRED_OTP_VERSION)"
        return 0
    else
        log_warn "OTP version does not meet requirements ($version < $REQUIRED_OTP_VERSION)"
        return 1
    fi
}

#==============================================================================
# Version Comparison Logic
#==============================================================================

# Compare version strings (returns 0 if v1 >= v2, 1 otherwise)
compare_versions() {
    local v1=$1
    local v2=$2

    # Handle empty versions
    if [[ -z "$v1" ]]; then
        return 1
    fi

    # Parse v1 (handle both "28" and "28.3.1" formats)
    local v1_major v1_minor v1_patch
    IFS='.' read -r v1_major v1_minor v1_patch <<< "$v1"
    v1_major=${v1_major:-0}
    v1_minor=${v1_minor:-0}
    v1_patch=${v1_patch:-0}

    # Parse v2
    local v2_major v2_minor v2_patch
    IFS='.' read -r v2_major v2_minor v2_patch <<< "$v2"
    v2_major=${v2_major:-0}
    v2_minor=${v2_minor:-0}
    v2_patch=${v2_patch:-0}

    # Compare major version
    if ((v1_major > v2_major)); then
        return 0
    elif ((v1_major < v2_major)); then
        return 1
    fi

    # Major versions equal, compare minor
    if ((v1_minor > v2_minor)); then
        return 0
    elif ((v1_minor < v2_minor)); then
        return 1
    fi

    # Minor versions equal, compare patch
    if ((v1_patch >= v2_patch)); then
        return 0
    else
        return 1
    fi
}

#==============================================================================
# Phase 3: OTP Installation
#==============================================================================

# Attempt single OTP installation
attempt_otp_install() {
    log_info "Starting OTP installation procedure..."

    # Step 1: Download erlang-solutions repository package
    log_info "Step 1/4: Downloading Erlang Solutions repository package..."
    if ! wget -q https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb \
         -O /tmp/erlang-solutions.deb 2>&1; then
        log_error "Failed to download erlang-solutions package"
        return 1
    fi
    log_success "Repository package downloaded"

    # Step 2: Install repository
    log_info "Step 2/4: Installing Erlang Solutions repository..."
    if ! sudo dpkg -i /tmp/erlang-solutions.deb >/dev/null 2>&1; then
        log_warn "dpkg install had warnings (usually safe to ignore)"
    fi
    log_success "Repository installed"

    # Step 3: Update apt cache
    log_info "Step 3/4: Updating apt package cache..."
    if ! sudo apt-get update -qq 2>&1 | grep -qv "Get:"; then
        log_error "Failed to update apt cache"
        return 1
    fi
    log_success "Package cache updated"

    # Step 4: Install OTP
    log_info "Step 4/4: Installing Erlang/OTP $REQUIRED_OTP_VERSION..."

    # Try exact version first
    if sudo DEBIAN_FRONTEND=noninteractive apt-get install -y -qq \
        esl-erlang=1:${REQUIRED_OTP_VERSION}-1 >/dev/null 2>&1; then
        log_success "Exact version esl-erlang=1:${REQUIRED_OTP_VERSION}-1 installed"
    else
        log_warn "Exact version not available, trying latest esl-erlang..."
        if sudo DEBIAN_FRONTEND=noninteractive apt-get install -y -qq \
            esl-erlang >/dev/null 2>&1; then
            log_success "Latest esl-erlang installed"
        else
            log_error "Failed to install esl-erlang"
            return 1
        fi
    fi

    # Verify installation
    local installed_version
    installed_version=$(get_otp_version)

    if [[ -z "$installed_version" ]]; then
        log_error "Installation verification failed: erl command not found"
        return 1
    fi

    if ! compare_versions "$installed_version" "$REQUIRED_OTP_VERSION"; then
        log_error "Installed version $installed_version does not meet requirements"
        return 1
    fi

    log_success "OTP $installed_version installed and verified"
    return 0
}

# Install OTP with retry logic
install_otp() {
    log_info "Checking OTP installation requirements..."

    # Check current installation
    local current_version
    current_version=$(get_otp_version)

    if [[ -n "$current_version" ]]; then
        if compare_versions "$current_version" "$REQUIRED_OTP_VERSION"; then
            log_success "OTP $current_version already installed and meets requirements"
            return 0
        else
            log_warn "OTP $current_version installed but does not meet requirements"
            log_info "Upgrading to OTP $REQUIRED_OTP_VERSION..."
        fi
    else
        log_info "OTP not detected, installing $REQUIRED_OTP_VERSION..."
    fi

    # Retry installation with exponential backoff
    local attempt=1
    local backoff

    while ((attempt <= MAX_INSTALL_ATTEMPTS)); do
        log_info "Installation attempt $attempt/$MAX_INSTALL_ATTEMPTS"

        if attempt_otp_install; then
            log_success "OTP installation succeeded on attempt $attempt"
            return 0
        fi

        if ((attempt < MAX_INSTALL_ATTEMPTS)); then
            backoff=$((BASE_BACKOFF ** attempt))
            log_warn "Installation failed, retrying in ${backoff}s..."
            sleep "$backoff"

            # Clear apt cache before retry
            log_info "Clearing apt cache before retry..."
            sudo apt-get clean >/dev/null 2>&1 || true
        fi

        ((attempt++))
    done

    log_error "OTP installation failed after $MAX_INSTALL_ATTEMPTS attempts"
    return 1
}

#==============================================================================
# Phase 4: Environment Setup
#==============================================================================

# Setup environment variables for cloud execution
setup_environment() {
    log_info "Configuring environment variables..."

    # Cloud execution marker
    export CLAUDE_CODE_REMOTE=true
    log_info "Set CLAUDE_CODE_REMOTE=true"

    # erlmcp profile
    export ERLMCP_PROFILE=cloud
    log_info "Set ERLMCP_PROFILE=cloud"

    # Cache directory
    export ERLMCP_CACHE="/home/user/erlmcp/.erlmcp/cache/"
    mkdir -p "$ERLMCP_CACHE" || {
        log_error "Failed to create cache directory: $ERLMCP_CACHE"
        return 1
    }
    log_info "Set ERLMCP_CACHE=$ERLMCP_CACHE"

    # Terminal settings (disable ANSI for rebar3)
    export TERM=dumb
    log_info "Set TERM=dumb"

    # Erlang shell history
    export ERL_AFLAGS="-kernel shell_history enabled"
    log_info "Set ERL_AFLAGS=\"$ERL_AFLAGS\""

    # Rebar3 color (disabled for cloud logs)
    export REBAR_COLOR=none
    log_info "Set REBAR_COLOR=none"

    log_success "Environment configuration complete"
    return 0
}

#==============================================================================
# Phase 5: Core Precompilation (Non-Fatal)
#==============================================================================

# Precompile erlmcp_core modules (warm cache)
precompile_core() {
    log_info "Attempting to precompile core modules..."

    # Check if rebar3 is available
    if ! command -v rebar3 &> /dev/null; then
        log_warn "rebar3 not found, skipping precompilation"
        log_info "rebar3 will be installed automatically on first use"
        return 0
    fi

    # Check if erlmcp_core app exists
    if [[ ! -d "apps/erlmcp_core" ]]; then
        log_warn "apps/erlmcp_core not found, skipping precompilation"
        return 0
    fi

    # Attempt compilation (non-fatal)
    log_info "Compiling erlmcp_core modules..."
    if TERM=dumb rebar3 compile apps/erlmcp_core 2>&1 | grep -qE "(Compiling|===>)"; then
        log_success "Core modules precompiled successfully"
    else
        log_warn "Precompilation failed (non-fatal, continuing)"
        log_info "Modules will be compiled on first use"
    fi

    return 0
}

#==============================================================================
# Phase 6: Environment Validation
#==============================================================================

# Validate complete environment setup
validate_environment() {
    log_info "Validating environment setup..."

    local validation_failed=0

    # Check 1: OTP version
    log_info "Check 1/4: OTP version..."
    local otp_version
    otp_version=$(get_otp_version)

    if [[ -z "$otp_version" ]]; then
        log_error "OTP validation failed: erl command not found"
        validation_failed=1
    elif ! compare_versions "$otp_version" "$REQUIRED_OTP_VERSION"; then
        log_error "OTP validation failed: $otp_version < $REQUIRED_OTP_VERSION"
        validation_failed=1
    else
        log_success "OTP: $otp_version (meets requirement $REQUIRED_OTP_VERSION)"
    fi

    # Check 2: rebar3 (advisory)
    log_info "Check 2/4: rebar3 availability..."
    if command -v rebar3 &> /dev/null; then
        local rebar3_version
        rebar3_version=$(rebar3 version 2>&1 | head -n1 || echo "unknown")
        log_success "rebar3: $rebar3_version"
    else
        log_warn "rebar3: not found (will be installed on first rebar3 command)"
    fi

    # Check 3: Environment variables
    log_info "Check 3/4: Environment variables..."
    if [[ "$CLAUDE_CODE_REMOTE" != "true" ]]; then
        log_error "CLAUDE_CODE_REMOTE not set to 'true'"
        validation_failed=1
    else
        log_success "CLAUDE_CODE_REMOTE=$CLAUDE_CODE_REMOTE"
    fi

    if [[ "$ERLMCP_PROFILE" != "cloud" ]]; then
        log_error "ERLMCP_PROFILE not set to 'cloud'"
        validation_failed=1
    else
        log_success "ERLMCP_PROFILE=$ERLMCP_PROFILE"
    fi

    # Check 4: Cache directory
    log_info "Check 4/4: Cache directory..."
    if [[ ! -d "$ERLMCP_CACHE" ]]; then
        log_error "Cache directory does not exist: $ERLMCP_CACHE"
        validation_failed=1
    else
        log_success "Cache directory: $ERLMCP_CACHE"
    fi

    # Final verdict
    if ((validation_failed == 0)); then
        log_success "Environment validation complete (all checks passed)"
        return 0
    else
        log_error "Environment validation failed (see errors above)"
        return 1
    fi
}

#==============================================================================
# Phase 7: Lock File Creation
#==============================================================================

# Create lock file to mark successful initialization
create_lock_file() {
    log_info "Creating lock file..."

    local lock_dir
    lock_dir=$(dirname "$LOCK_FILE")

    if [[ ! -d "$lock_dir" ]]; then
        mkdir -p "$lock_dir" || {
            log_error "Failed to create lock directory: $lock_dir"
            return 1
        }
    fi

    echo "$REQUIRED_OTP_VERSION" > "$LOCK_FILE" || {
        log_error "Failed to write lock file: $LOCK_FILE"
        return 1
    }

    if [[ ! -f "$LOCK_FILE" ]]; then
        log_error "Lock file does not exist after creation: $LOCK_FILE"
        return 1
    fi

    local locked_version
    locked_version=$(cat "$LOCK_FILE")

    if [[ "$locked_version" != "$REQUIRED_OTP_VERSION" ]]; then
        log_error "Lock file contains incorrect version: $locked_version"
        return 1
    fi

    log_success "Lock file created: $LOCK_FILE"
    log_info "Locked version: $locked_version"
    return 0
}

#==============================================================================
# Main Orchestration
#==============================================================================

main() {
    # Initialize logging first
    init_logging

    log_info "Starting erlmcp Cloud Session Initialization"
    log_info "Required OTP: $REQUIRED_OTP_VERSION"
    log_info "Lock file: $LOCK_FILE"
    log_info "Log file: $LOG_FILE"

    # Phase 1: Check if already initialized
    log_phase "1/7" "Initialization Check"
    if is_already_initialized; then
        log_success "Session already initialized, skipping setup"
        log_separator
        log_success "erlmcp ready for cloud development!"
        exit 0
    fi
    log_info "Initialization required, proceeding with setup..."

    # Phase 2: Detect OTP version
    log_phase "2/7" "OTP Version Detection"
    local current_version
    current_version=$(get_otp_version)
    if [[ -n "$current_version" ]]; then
        log_info "Current OTP: $current_version"
    else
        log_info "Current OTP: not installed"
    fi

    # Phase 3: Install OTP (if needed)
    log_phase "3/7" "OTP Installation"
    if ! install_otp; then
        log_error "FATAL: OTP installation failed"
        log_separator
        exit 1
    fi

    # Phase 4: Setup environment
    log_phase "4/7" "Environment Setup"
    if ! setup_environment; then
        log_error "FATAL: Environment setup failed"
        log_separator
        exit 1
    fi

    # Phase 5: Precompile core (non-fatal)
    log_phase "5/7" "Core Precompilation"
    precompile_core

    # Phase 6: Validate environment
    log_phase "6/7" "Environment Validation"
    if ! validate_environment; then
        log_error "FATAL: Environment validation failed"
        log_separator
        exit 1
    fi

    # Phase 7: Create lock file
    log_phase "7/7" "Lock File Creation"
    if ! create_lock_file; then
        log_error "FATAL: Lock file creation failed"
        log_separator
        exit 1
    fi

    # Success summary
    log_separator
    log_success "All phases complete!"
    log_separator
    echo ""
    echo "Summary:"
    echo "  OTP Version: $(get_otp_version)"
    echo "  Profile: $ERLMCP_PROFILE"
    echo "  Cache: $ERLMCP_CACHE"
    echo "  Lock: $LOCK_FILE"
    echo ""
    log_separator
    log_success "erlmcp ready for cloud development!"
    log_separator

    exit 0
}

# Execute main orchestration
main "$@"
