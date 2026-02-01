#!/bin/bash
# OTP installation functions with multi-tier fallback
# Used by SessionStart hook to install Erlang/OTP 28+

# This script is sourced, not executed directly
# Expected variables: ERLMCP_CACHE, LOG_FILE

download_with_retry() {
    local url="$1"
    local output="$2"
    local max_attempts="${3:-3}"
    local timeout="${4:-30}"

    for attempt in $(seq 1 $max_attempts); do
        log "  Attempt $attempt/$max_attempts: $url"

        if curl -L --fail --max-time $timeout -o "$output" "$url" 2>>"${LOG_FILE:-/dev/null}"; then
            log "${GREEN}✓${NC} Download successful"
            return 0
        fi

        if [ $attempt -lt $max_attempts ]; then
            local backoff=$((attempt * 2))
            log "${YELLOW}⚠${NC} Download failed, retrying in ${backoff}s..."
            sleep $backoff
        fi
    done

    log "${RED}✗${NC} Download failed after $max_attempts attempts"
    return 1
}

verify_otp_installation() {
    local install_dir="$1"

    if [ ! -f "$install_dir/bin/erl" ]; then
        log "${RED}✗${NC} erl binary not found in $install_dir/bin"
        return 1
    fi

    # Verify OTP version
    local version=$("$install_dir/bin/erl" -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell 2>/dev/null)

    if [[ ! "$version" =~ ^28 ]]; then
        log "${RED}✗${NC} Expected OTP 28, got: $version"
        return 1
    fi

    log "${GREEN}✓${NC} OTP $version verified"
    return 0
}

build_otp_from_source() {
    local install_dir="$1"
    local otp_version="${2:-28.0}"

    log "${BLUE}Building OTP $otp_version from source...${NC}"
    log "${YELLOW}⚠${NC} This will take 5-10 minutes"

    # Check for build tools
    local missing_tools=()
    for tool in gcc make autoconf; do
        if ! command -v $tool >/dev/null 2>&1; then
            missing_tools+=($tool)
        fi
    done

    if [ ${#missing_tools[@]} -gt 0 ]; then
        log "${RED}✗${NC} Missing build tools: ${missing_tools[*]}"
        log "  Cloud VMs do not have build tools available"
        return 1
    fi

    local source_url="https://github.com/erlang/otp/archive/refs/tags/OTP-$otp_version.tar.gz"
    local build_dir="/tmp/otp-build-$$"

    mkdir -p "$build_dir"
    cd "$build_dir"

    log "→ Downloading OTP source..."
    if ! download_with_retry "$source_url" "otp-source.tar.gz"; then
        return 1
    fi

    log "→ Extracting source..."
    tar -xzf otp-source.tar.gz
    cd "otp-OTP-$otp_version"

    log "→ Configuring (with SSL, JIT, dirty schedulers)..."
    ./configure \
        --prefix="$install_dir" \
        --with-ssl \
        --enable-jit \
        --enable-dirty-schedulers \
        --without-javac \
        --without-odbc 2>&1 | tee -a "${LOG_FILE:-/dev/null}"

    if [ ${PIPESTATUS[0]} -ne 0 ]; then
        log "${RED}✗${NC} Configure failed"
        cd /
        rm -rf "$build_dir"
        return 1
    fi

    log "→ Compiling (using $(nproc) cores)..."
    if ! make -j$(nproc) 2>&1 | tee -a "${LOG_FILE:-/dev/null}"; then
        log "${RED}✗${NC} Compilation failed"
        cd /
        rm -rf "$build_dir"
        return 1
    fi

    log "→ Installing to $install_dir..."
    if ! make install 2>&1 | tee -a "${LOG_FILE:-/dev/null}"; then
        log "${RED}✗${NC} Installation failed"
        cd /
        rm -rf "$build_dir"
        return 1
    fi

    cd /
    rm -rf "$build_dir"

    log "${GREEN}✓${NC} OTP $otp_version built from source"
    return 0
}

install_otp_with_fallback() {
    local install_dir="${1:-$HOME/.erlmcp/otp/28.0}"
    local otp_version="${2:-28.0}"
    local cache_dir="${ERLMCP_CACHE:-$HOME/.erlmcp/cache}"

    mkdir -p "$cache_dir" "$(dirname "$install_dir")"

    log "${BLUE}Installing OTP $otp_version with multi-tier fallback...${NC}"
    log ""

    # Tier 1: Already installed (checked by caller, but verify here)
    if [ -f "$install_dir/bin/erl" ]; then
        if verify_otp_installation "$install_dir"; then
            log "${GREEN}✓${NC} Tier 1: OTP already installed (0s)"
            return 0
        else
            log "${YELLOW}⚠${NC} Existing installation invalid, reinstalling..."
            rm -rf "$install_dir"
        fi
    fi

    # Tier 2: Disk cache
    log "→ Tier 2: Checking disk cache..."
    if [ -f "$cache_dir/otp-$otp_version.tar.gz" ]; then
        log "  Found cached OTP $otp_version"

        mkdir -p "$install_dir"
        if tar -xzf "$cache_dir/otp-$otp_version.tar.gz" -C "$install_dir" --strip-components=1 2>>"${LOG_FILE:-/dev/null}"; then
            if verify_otp_installation "$install_dir"; then
                log "${GREEN}✓${NC} Tier 2: Restored from cache (3s)"
                return 0
            fi
        fi

        log "${YELLOW}⚠${NC} Cached OTP invalid, removing..."
        rm -f "$cache_dir/otp-$otp_version.tar.gz"
        rm -rf "$install_dir"
    else
        log "  No cache found"
    fi

    # Tier 3: Download pre-built binary from GitHub
    log "→ Tier 3: Downloading pre-built binary from GitHub..."

    # Try official OTP releases first
    local binary_urls=(
        "https://github.com/erlang/otp/releases/download/OTP-$otp_version/otp_doc_man_$otp_version.tar.gz"
        "https://github.com/erlang/otp/archive/refs/tags/OTP-$otp_version.tar.gz"
    )

    for binary_url in "${binary_urls[@]}"; do
        log "  Trying: $binary_url"

        if download_with_retry "$binary_url" "/tmp/otp-$otp_version.tar.gz"; then
            mkdir -p "$install_dir"

            # Try to extract
            if tar -xzf "/tmp/otp-$otp_version.tar.gz" -C "$install_dir" --strip-components=1 2>>"${LOG_FILE:-/dev/null}"; then
                # Check if this is a source tarball (has configure script)
                if [ -f "$install_dir/configure" ]; then
                    log "${YELLOW}⚠${NC} Downloaded source tarball, not binary. Proceeding to build..."
                    rm -rf "$install_dir"
                    rm -f "/tmp/otp-$otp_version.tar.gz"
                    break
                fi

                # Verify installation
                if verify_otp_installation "$install_dir"; then
                    # Cache for future use
                    cp "/tmp/otp-$otp_version.tar.gz" "$cache_dir/"
                    rm -f "/tmp/otp-$otp_version.tar.gz"
                    log "${GREEN}✓${NC} Tier 3: Installed from GitHub binary (25s)"
                    return 0
                fi
            fi

            rm -rf "$install_dir"
            rm -f "/tmp/otp-$otp_version.tar.gz"
        fi
    done

    # Tier 4: Build from source
    log "→ Tier 4: Building from source..."

    if build_otp_from_source "$install_dir" "$otp_version"; then
        # Cache the built version
        log "→ Caching built OTP for future use..."
        cd "$(dirname "$install_dir")"
        tar -czf "$cache_dir/otp-$otp_version.tar.gz" "$(basename "$install_dir")"

        log "${GREEN}✓${NC} Tier 4: Built from source (completed)"
        return 0
    fi

    # Tier 5: Graceful failure
    log ""
    log "════════════════════════════════════════════════"
    log "${RED}✗ OTP $otp_version installation FAILED${NC}"
    log "════════════════════════════════════════════════"
    log ""
    log "All fallback strategies exhausted:"
    log "  [✗] Memory cache - not found"
    log "  [✗] Disk cache - not found or invalid"
    log "  [✗] GitHub binary - download failed or unavailable"
    log "  [✗] Build from source - build tools unavailable or build failed"
    log ""
    log "Manual installation options:"
    log ""
    log "Option 1: Use asdf (recommended for local development)"
    log "  asdf install erlang $otp_version"
    log "  asdf local erlang $otp_version"
    log ""
    log "Option 2: Use Docker (recommended for cloud VMs)"
    log "  docker run -it -v \$(pwd):/workspace erlang:$otp_version"
    log ""
    log "Option 3: Download and extract manually"
    log "  curl -L https://github.com/erlang/otp/releases/download/OTP-$otp_version/otp-$otp_version.tar.gz | tar xz -C ~/.erlmcp/otp/"
    log "  export PATH=~/.erlmcp/otp/$otp_version/bin:\$PATH"
    log ""
    log "If this is a network issue, verify connectivity to:"
    log "  - github.com"
    log "  - raw.githubusercontent.com"
    log ""

    return 1
}

install_rebar3() {
    local install_dir="${1:-$HOME/.erlmcp/bin}"
    local rebar3_version="${2:-3.25.0}"
    local cache_dir="${ERLMCP_CACHE:-$HOME/.erlmcp/cache}"

    mkdir -p "$install_dir" "$cache_dir"

    local rebar3_bin="$install_dir/rebar3"

    # Check if already installed
    if [ -f "$rebar3_bin" ]; then
        log "${GREEN}✓${NC} rebar3 already installed"
        return 0
    fi

    # Check cache
    if [ -f "$cache_dir/rebar3-$rebar3_version" ]; then
        log "→ Restoring rebar3 from cache..."
        cp "$cache_dir/rebar3-$rebar3_version" "$rebar3_bin"
        chmod +x "$rebar3_bin"
        log "${GREEN}✓${NC} rebar3 restored from cache"
        return 0
    fi

    # Download from GitHub
    log "→ Downloading rebar3 $rebar3_version from GitHub..."
    local rebar3_url="https://github.com/erlang/rebar3/releases/download/$rebar3_version/rebar3"

    if download_with_retry "$rebar3_url" "$rebar3_bin"; then
        chmod +x "$rebar3_bin"

        # Cache for future use
        cp "$rebar3_bin" "$cache_dir/rebar3-$rebar3_version"

        log "${GREEN}✓${NC} rebar3 $rebar3_version installed"
        return 0
    fi

    log "${RED}✗${NC} Failed to download rebar3"
    return 1
}
