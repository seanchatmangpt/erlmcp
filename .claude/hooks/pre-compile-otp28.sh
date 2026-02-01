#!/usr/bin/env bash
# OTP 28.3.1 Binary Path Configuration
#
# This hook ensures OTP 28.3.1+ is available for erlmcp compilation.
# Works on both Linux (cloud) and macOS (local).
#
# Exit codes:
#   0 - OTP 28 found and PATH configured
#   1 - OTP 28 not found, compilation blocked

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

# OTP version requirement
ERLMCP_OTP_VERSION="28.3.1"
OTP_REQUIRED_MAJOR=28

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}" >&2
}

print_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

# Detect platform
detect_platform() {
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        echo "linux"
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        echo "macos"
    else
        echo "unknown"
    fi
}

# Get current OTP version
get_otp_version() {
    if ! command -v erl &> /dev/null; then
        echo "0"
        return 1
    fi
    erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt(0).' 2>/dev/null || echo "0"
}

# Check if OTP version is acceptable
check_otp_version() {
    local version="$1"
    if [[ "$version" =~ ^[0-9]+ ]]; then
        local major="${version%%.*}"
        if [[ $major -ge $OTP_REQUIRED_MAJOR ]]; then
            return 0
        fi
    fi
    return 1
}

PLATFORM=$(detect_platform)
CURRENT_VERSION=$(get_otp_version || echo "0")

print_info "Platform: ${PLATFORM}"
print_info "Current OTP version: ${CURRENT_VERSION}"

# Check if OTP is available and acceptable
if check_otp_version "$CURRENT_VERSION"; then
    print_success "OTP ${CURRENT_VERSION} is acceptable (>= ${OTP_REQUIRED_MAJOR})"
else
    print_error "OTP version ${CURRENT_VERSION} does not meet minimum requirement (>= ${OTP_REQUIRED_MAJOR})"
    echo ""

    if [[ "$PLATFORM" == "linux" ]]; then
        echo -e "${YELLOW}On Linux (cloud), run SessionStart.sh to auto-install OTP:${NC}"
        echo "  bash /home/user/erlmcp/.claude/hooks/SessionStart.sh"
    elif [[ "$PLATFORM" == "macos" ]]; then
        echo -e "${YELLOW}On macOS, install OTP via Homebrew or build from source:${NC}"
        echo "  brew install erlang"
        echo "  OR"
        echo "  curl -LO https://github.com/erlang/otp/releases/download/OTP-${ERLMCP_OTP_VERSION}/otp_src_${ERLMCP_OTP_VERSION}.tar.gz"
        echo "  tar xzf otp_src_${ERLMCP_OTP_VERSION}.tar.gz && cd otp_src_${ERLMCP_OTP_VERSION}"
        echo "  ./configure && make && make install"
    fi
    echo ""
    exit 1
fi

# Export OTP information for downstream tools
export ERLMCP_OTP_VERSION="$CURRENT_VERSION"
export ERLMCP_OTP_AVAILABLE=true

echo ""
echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}✅ OTP ${CURRENT_VERSION} configured - Ready to compile${NC}"
echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
echo ""
exit 0
