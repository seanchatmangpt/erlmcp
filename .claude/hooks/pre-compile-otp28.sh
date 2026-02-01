#!/usr/bin/env bash
# OTP 28.3.1 Binary Path Configuration
#
# This hook ensures OTP 28.3.1+ is available for all erlmcp compilation.
# Supports both custom installations and system-installed OTP.
#
# Environment Variables:
#   ERLMCP_OTP_BIN    - Path to OTP bin directory (default: auto-detect)
#   ERLMCP_OTP_ROOT   - Path to OTP installation root (default: auto-detect)
#
# Exit codes:
#   0 - OTP 28+ found and PATH configured
#   1 - OTP 28+ not found, compilation blocked

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

# Required OTP version
ERLMCP_OTP_VERSION="28.3.1"
REQUIRED_MAJOR=28

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}" >&2
}

print_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

# Auto-detect OTP installation
detect_otp_installation() {
    local otp_bin=""
    local otp_root=""

    # Priority 1: Use ERLMCP_OTP_BIN if set
    if [[ -n "${ERLMCP_OTP_BIN:-}" ]]; then
        if [[ -d "$ERLMCP_OTP_BIN" ]]; then
            otp_bin="$ERLMCP_OTP_BIN"
            otp_root="$(dirname "$ERLMCP_OTP_BIN")"
            print_info "Using ERLMCP_OTP_BIN: $otp_bin"
        else
            print_error "ERLMCP_OTP_BIN set but directory not found: $ERLMCP_OTP_BIN"
            return 1
        fi
    fi

    # Priority 2: Check custom installation (original path - local dev)
    if [[ -z "$otp_bin" ]] && [[ -d "/Users/sac/.erlmcp/otp-28.3.1" ]]; then
        otp_bin="/Users/sac/.erlmcp/otp-28.3.1/bin"
        otp_root="/Users/sac/.erlmcp/otp-28.3.1"
        print_info "Found custom OTP installation: $otp_bin"
    fi

    # Priority 3: Use system erl (cloud environments, CI/CD)
    if [[ -z "$otp_bin" ]]; then
        if command -v erl &> /dev/null; then
            local erl_path
            erl_path=$(which erl)
            otp_bin="$(dirname "$erl_path")"
            otp_root="$(dirname "$otp_bin")"
            print_info "Using system OTP: $otp_bin"
        else
            print_error "No OTP installation found"
            return 1
        fi
    fi

    # Export detected paths
    export ERLMCP_OTP_BIN="$otp_bin"
    export ERLMCP_OTP_ROOT="$otp_root"
    export ERLMCP_OTP_28_BIN="${otp_bin}/erl"

    return 0
}

# Verify erl binary exists
verify_erl_binary() {
    if [[ ! -x "${ERLMCP_OTP_BIN}/erl" ]]; then
        print_error "erl binary not found at ${ERLMCP_OTP_BIN}/erl"
        return 1
    fi
    return 0
}

# Get OTP version
get_otp_version() {
    local version
    version=$("${ERLMCP_OTP_BIN}/erl" -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell 2>/dev/null || echo "unknown")
    echo "$version"
}

# Verify OTP version meets requirement
verify_otp_version() {
    local version
    version=$(get_otp_version)

    if [[ "$version" == "unknown" ]]; then
        print_error "Unable to detect OTP version"
        return 1
    fi

    # Extract major version (handles both "28" and "28.3.1" formats)
    local major_version
    if [[ "$version" =~ ^([0-9]+) ]]; then
        major_version="${BASH_REMATCH[1]}"
    else
        print_error "Invalid OTP version format: $version"
        return 1
    fi

    if [[ "$major_version" -lt "$REQUIRED_MAJOR" ]]; then
        print_error "OTP version $version is less than required version $REQUIRED_MAJOR"
        echo ""
        echo -e "${YELLOW}To install OTP $ERLMCP_OTP_VERSION:${NC}"
        echo "  # Option 1: Cloud/CI (Erlang Solutions APT repository)"
        echo "  wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb"
        echo "  sudo dpkg -i erlang-solutions_2.0_all.deb"
        echo "  sudo apt-get update"
        echo "  sudo apt-get install -y esl-erlang=1:${ERLMCP_OTP_VERSION}-1"
        echo ""
        echo "  # Option 2: Local build from source"
        echo "  export ERLMCP_OTP_ROOT=\"\$HOME/.erlmcp/otp-${ERLMCP_OTP_VERSION}\""
        echo "  curl -LO https://github.com/erlang/otp/releases/download/OTP-${ERLMCP_OTP_VERSION}/otp_src_${ERLMCP_OTP_VERSION}.tar.gz"
        echo "  tar xzf otp_src_${ERLMCP_OTP_VERSION}.tar.gz"
        echo "  cd otp_src_${ERLMCP_OTP_VERSION}"
        echo "  ./configure --prefix=\"\$ERLMCP_OTP_ROOT\""
        echo "  make -j\$(nproc)"
        echo "  make install"
        echo "  export ERLMCP_OTP_BIN=\"\$ERLMCP_OTP_ROOT/bin\""
        echo ""
        return 1
    fi

    print_success "OTP version $version (>= $REQUIRED_MAJOR required)"
    return 0
}

# Main execution
main() {
    echo ""
    echo -e "${BOLD}OTP 28 Pre-Compile Hook${NC}"
    echo -e "${BOLD}========================${NC}"
    echo ""

    # Detect OTP installation
    if ! detect_otp_installation; then
        print_error "OTP installation not found"
        exit 1
    fi

    # Verify erl binary
    if ! verify_erl_binary; then
        exit 1
    fi

    # Verify version
    if ! verify_otp_version; then
        exit 1
    fi

    # Export PATH
    export PATH="${ERLMCP_OTP_BIN}:$PATH"

    echo ""
    echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}✅ OTP $(get_otp_version) configured - Ready to compile${NC}"
    echo -e "${GREEN}   ERLMCP_OTP_BIN=${ERLMCP_OTP_BIN}${NC}"
    echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
    echo ""
    exit 0
}

main "$@"
