#!/usr/bin/env bash
# .claude/hooks/SessionStart.sh
# Purpose: Autonomous OTP 28.3.1 initialization via source build (Joe Armstrong style)
# Spec: CLAUDE.md v2.1.0
# Architecture: Build-from-source strategy (deterministic, self-contained, no package mgr)
#
# State Machine:
#   Phase 1: Cache check (use cached OTP if available)
#   Phase 2: Download OTP source from GitHub (if not cached)
#   Phase 3: Build OTP from source (./configure && make)
#   Phase 4: Cache the built binaries
#   Phase 5: Environment setup
#   Phase 6: Lock file creation
#
# Strategy: DOWNLOAD → BUILD → CACHE → LOCK
# Idempotency: If cache exists with valid version, skip build entirely
# Performance: First run ~60s (build), second run <1s (cache check)

set -euo pipefail

#==============================================================================
# Constants & Configuration
#==============================================================================

readonly REQUIRED_OTP_VERSION="28.3.1"
# Pre-built OTP distribution strategy:
# Phase A: Download pre-built tarball (fast, ~30s)
# Phase B: If tarball unavailable, build from source (slow, ~6m)
readonly OTP_PREBUILT_URL="https://github.com/seanchatmangpt/erlmcp/releases/download/erlang-28.3.1/erlang-28.3.1-linux-x86_64.tar.gz"
readonly OTP_PREBUILT_SHA256="747483d5517eb7df674956d8752f06bcb4ab35f2be2206cbfb90611bc9fb6106"
readonly OTP_GITHUB_SOURCE_URL="https://github.com/erlang/otp.git"
readonly ERLMCP_ROOT="${ERLMCP_ROOT:-.}"
readonly OTP_CACHE_DIR="${ERLMCP_ROOT}/.erlmcp/otp-${REQUIRED_OTP_VERSION}"
readonly OTP_BIN="${OTP_CACHE_DIR}/bin/erl"
readonly LOCK_FILE="${ERLMCP_ROOT}/.erlmcp/cache/sessionstart.lock"
readonly LOG_FILE="${ERLMCP_ROOT}/.erlmcp/sessionstart.log"
readonly BUILD_TEMP_DIR="/tmp/otp-build-$$"
readonly SCRIPT_VERSION="2.1.0"

# CPU count for parallel build
readonly CPU_COUNT=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)

#==============================================================================
# Logging & Output
#==============================================================================

log_info() {
    echo "[INFO] $*" | tee -a "$LOG_FILE"
}

log_error() {
    echo "[ERROR] $*" | tee -a "$LOG_FILE" >&2
}

log_success() {
    echo "[SUCCESS] $*" | tee -a "$LOG_FILE"
}

log_phase() {
    local phase=$1
    shift
    echo "" | tee -a "$LOG_FILE"
    echo "================================================================================" | tee -a "$LOG_FILE"
    echo "[PHASE $phase/6] $*" | tee -a "$LOG_FILE"
    echo "================================================================================" | tee -a "$LOG_FILE"
}

init_logging() {
    mkdir -p "$(dirname "$LOG_FILE")"
    {
        echo "================================================================================"
        echo "[SessionStart] Timestamp: $(date -Iseconds)"
        echo "[SessionStart] Version: $SCRIPT_VERSION"
        echo "[SessionStart] Strategy: Build OTP from source (GitHub releases)"
        echo "[SessionStart] Working dir: $(pwd)"
        echo "[SessionStart] OTP version: $REQUIRED_OTP_VERSION"
        echo "[SessionStart] Cache dir: $OTP_CACHE_DIR"
        echo "================================================================================"
    } >> "$LOG_FILE"
}

#==============================================================================
# Phase 1: Cache Check (Idempotency)
#==============================================================================

is_otp_cached() {
    if [[ ! -f "$OTP_BIN" ]]; then
        return 1
    fi

    local cached_version
    cached_version=$("$OTP_BIN" -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null || echo "")

    if [[ "$cached_version" == "$REQUIRED_OTP_VERSION" ]]; then
        log_success "OTP $cached_version found in cache"
        return 0
    else
        log_info "Cached OTP version $cached_version does not match required $REQUIRED_OTP_VERSION"
        return 1
    fi
}

#==============================================================================
# Phase 2A: Try downloading pre-built OTP tarball (fast)
#==============================================================================

download_prebuilt_otp() {
    log_phase "2A/6" "Attempting to download pre-built OTP"

    log_info "URL: $OTP_PREBUILT_URL"
    log_info "Expected SHA256: $OTP_PREBUILT_SHA256"

    mkdir -p "$OTP_CACHE_DIR"
    local tarball="$OTP_CACHE_DIR/erlang-prebuilt.tar.gz"

    log_info "Downloading erlang-28.3.1-linux-x86_64.tar.gz..."
    if ! wget -q --show-progress -O "$tarball" "$OTP_PREBUILT_URL" 2>&1 | tee -a "$LOG_FILE"; then
        log_warn "Failed to download pre-built OTP (will fallback to source build)"
        rm -f "$tarball"
        return 1
    fi

    # Verify checksum
    log_info "Verifying SHA256 checksum..."
    local computed_sha256
    computed_sha256=$(sha256sum "$tarball" | awk '{print $1}')
    if [[ "$computed_sha256" != "$OTP_PREBUILT_SHA256" ]]; then
        log_error "Checksum mismatch! Expected: $OTP_PREBUILT_SHA256, Got: $computed_sha256"
        rm -f "$tarball"
        return 1
    fi
    log_success "Checksum verified"

    # Extract
    log_info "Extracting pre-built OTP..."
    cd "$OTP_CACHE_DIR"
    if ! tar xzf "$tarball" --strip-components=1; then
        log_error "Failed to extract OTP tarball"
        rm -f "$tarball"
        return 1
    fi
    rm -f "$tarball"

    log_success "Pre-built OTP extracted successfully"
    return 0
}

#==============================================================================
# Phase 2B: Download OTP Source (fallback if prebuilt unavailable)
#==============================================================================

download_otp_source() {
    log_phase "2B/6" "Cloning OTP source from GitHub (source build)"

    log_info "Repository: https://github.com/erlang/otp.git"
    log_info "Branch: OTP-${REQUIRED_OTP_VERSION}"
    log_info "Destination: $BUILD_TEMP_DIR"

    mkdir -p "$BUILD_TEMP_DIR"
    cd "$BUILD_TEMP_DIR"

    log_info "Cloning OTP_${REQUIRED_OTP_VERSION} (shallow, branch-only)..."
    if ! git clone --depth 1 --branch "OTP-${REQUIRED_OTP_VERSION}" \
         https://github.com/erlang/otp.git otp_src 2>&1 | tee -a "$LOG_FILE"; then
        log_error "Failed to clone OTP source from GitHub"
        return 1
    fi

    log_success "Cloned OTP source (shallow clone: ~500MB)"

    cd otp_src
    log_info "Source directory: $(pwd)"
    log_success "OTP source ready for compilation"
    return 0
}

#==============================================================================
# Phase 3: Build OTP from Source
#==============================================================================

build_otp_from_source() {
    log_phase "3/6" "Building OTP from source"

    # Should already be in otp_src directory from download_otp_source()
    if [[ ! -f "./configure" ]]; then
        log_error "OTP configure script not found. Expected to be in $(pwd)"
        return 1
    fi

    log_info "Build directory: $(pwd)"

    # Configure
    log_info "Step 1/3: Running ./configure..."
    log_info "  Prefix: $OTP_CACHE_DIR"
    log_info "  CPUs: $CPU_COUNT"
    log_info "  Flags: --enable-threads --enable-kernel-poll --disable-wx --disable-odbc"

    if ! TERM=dumb ./configure \
        --prefix="$OTP_CACHE_DIR" \
        --enable-shared-zlib \
        --enable-threads \
        --enable-kernel-poll \
        --disable-wx \
        --disable-odbc \
        --disable-debug \
        --disable-documentation \
        2>&1 | tee -a "$LOG_FILE" | tail -20; then
        log_error "Configuration failed"
        return 1
    fi
    log_success "Configuration complete"

    # Build (parallel)
    log_info "Step 2/3: Running make -j$CPU_COUNT (parallel compilation)..."
    if ! TERM=dumb make -j "$CPU_COUNT" 2>&1 | tee -a "$LOG_FILE" | tail -20; then
        log_error "Build failed"
        return 1
    fi
    log_success "Build complete"

    # Install
    log_info "Step 3/3: Running make install..."
    if ! TERM=dumb make install 2>&1 | tee -a "$LOG_FILE" | tail -20; then
        log_error "Installation failed"
        return 1
    fi
    log_success "Installation complete"
    log_info "  Binaries installed to: $OTP_CACHE_DIR/bin/"

    return 0
}

#==============================================================================
# Phase 4: Verify & Cache
#==============================================================================

verify_otp_build() {
    log_phase "4/6" "Verifying OTP build"

    if [[ ! -f "$OTP_BIN" ]]; then
        log_error "OTP binary not found at $OTP_BIN"
        return 1
    fi

    log_info "Verifying OTP binary: $OTP_BIN"
    local built_version
    built_version=$("$OTP_BIN" -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null || echo "")

    if [[ -z "$built_version" ]]; then
        log_error "Failed to detect OTP version"
        return 1
    fi

    if [[ "$built_version" != "$REQUIRED_OTP_VERSION" ]]; then
        log_error "OTP version mismatch: built=$built_version, required=$REQUIRED_OTP_VERSION"
        return 1
    fi

    log_success "OTP $built_version verified and cached at $OTP_CACHE_DIR"

    # Show build info
    local erts_version
    erts_version=$("$OTP_BIN" -noshell -eval 'io:format("~s", [erlang:system_info(version)]), halt().' 2>/dev/null || echo "unknown")
    log_info "ERTS Version: $erts_version"

    return 0
}

#==============================================================================
# Phase 5: Environment Setup
#==============================================================================

setup_environment() {
    log_phase "5/6" "Environment setup"

    # Export OTP path for rebar3
    export PATH="${OTP_CACHE_DIR}/bin:${PATH}"
    log_info "Added to PATH: ${OTP_CACHE_DIR}/bin"

    # Cloud environment variables
    export CLAUDE_CODE_REMOTE=true
    export ERLMCP_PROFILE=cloud
    export ERLMCP_CACHE="${ERLMCP_ROOT}/.erlmcp/cache/"
    export TERM=dumb
    export REBAR_COLOR=none
    export ERL_AFLAGS="-kernel shell_history enabled"

    mkdir -p "$ERLMCP_CACHE"

    log_success "Environment variables set"
    log_info "  CLAUDE_CODE_REMOTE=$CLAUDE_CODE_REMOTE"
    log_info "  ERLMCP_PROFILE=$ERLMCP_PROFILE"
    log_info "  ERLMCP_CACHE=$ERLMCP_CACHE"
    log_info "  PATH includes ${OTP_CACHE_DIR}/bin"

    return 0
}

#==============================================================================
# Phase 6: Lock File Creation
#==============================================================================

create_lock_file() {
    log_phase "6/6" "Lock file creation"

    mkdir -p "$(dirname "$LOCK_FILE")"

    echo "$REQUIRED_OTP_VERSION" > "$LOCK_FILE"
    log_success "Lock file created: $LOCK_FILE"

    return 0
}

#==============================================================================
# Cleanup
#==============================================================================

cleanup() {
    if [[ -d "$BUILD_TEMP_DIR" ]]; then
        log_info "Cleaning up temporary build directory: $BUILD_TEMP_DIR"
        rm -rf "$BUILD_TEMP_DIR"
    fi
}

#==============================================================================
# Main Orchestration
#==============================================================================

main() {
    init_logging

    log_info "Starting SessionStart.sh (v$SCRIPT_VERSION)"
    log_info "Strategy: Pre-built OTP (fast) → Source build fallback (slow)"

    # Phase 1: Check cache
    log_phase "1/6" "Cache check"
    if is_otp_cached; then
        log_success "Using cached OTP $REQUIRED_OTP_VERSION"
        export PATH="${OTP_CACHE_DIR}/bin:${PATH}"
        setup_environment
        create_lock_file
        log_success "SessionStart complete (cached, ~0s)"
        exit 0
    fi

    log_info "OTP not cached, attempting acquisition..."

    # Phase 2A: Try pre-built (fast, ~30s)
    if download_prebuilt_otp; then
        log_success "Pre-built OTP acquired successfully"
        # Skip to verification
        if ! verify_otp_build; then
            log_error "FATAL: OTP verification failed"
            cleanup
            exit 1
        fi
    else
        # Phase 2B/3: Fallback to source build (slow, ~6m)
        log_info "Falling back to source build from GitHub..."

        # Phase 2B: Download
        if ! download_otp_source; then
            log_error "FATAL: OTP source download failed"
            cleanup
            exit 1
        fi

        # Phase 3: Build
        if ! build_otp_from_source; then
            log_error "FATAL: OTP build failed"
            cleanup
            exit 1
        fi

        # Phase 4: Verify
        if ! verify_otp_build; then
            log_error "FATAL: OTP verification failed"
            cleanup
            exit 1
        fi
    fi

    # Phase 5: Environment
    if ! setup_environment; then
        log_error "FATAL: Environment setup failed"
        cleanup
        exit 1
    fi

    # Phase 6: Lock
    if ! create_lock_file; then
        log_error "FATAL: Lock file creation failed"
        cleanup
        exit 1
    fi

    # Cleanup
    cleanup

    log_success "SessionStart complete (built from source)"
    log_info "OTP: $(get_otp_version)"
    log_info "Path: ${OTP_CACHE_DIR}/bin"

    exit 0
}

# Helper to get OTP version
get_otp_version() {
    if [[ -f "$OTP_BIN" ]]; then
        "$OTP_BIN" -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null || echo "unknown"
    else
        echo "not installed"
    fi
}

# Execute
main "$@"
