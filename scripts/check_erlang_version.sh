#!/usr/bin/env sh
# Erlang/OTP Version Enforcement Script
#
# This script enforces that only OTP 28.3.1 (custom-built) is used for erlmcp.
# It sources the pre-compile hook to set up the correct PATH to the custom OTP.
#
# Usage:
#   ./scripts/check_erlang_version.sh
#
# Exit codes:
#   0 - OTP 28.3.1 detected, ready to proceed
#   1 - OTP version incorrect or Erlang not found

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

# Custom OTP 28.3.1 installation (built from GitHub source)
ERLMCP_OTP_BIN="/Users/sac/.erlmcp/otp-28.3.1/bin"
ERLMCP_OTP_VERSION="28.3.1"
REQUIRED_OTP_MAJOR=28

# Source the pre-compile hook if available (sets up PATH)
ERLMCP_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
PRE_COMPILE_HOOK="${ERLMCP_ROOT}/.claude/hooks/pre-compile-otp28.sh"

if [ -f "$PRE_COMPILE_HOOK" ]; then
    # Source the hook to export PATH variables
    . "$PRE_COMPILE_HOOK" 2>/dev/null || true
fi

# Verify custom OTP installation exists
if [ -d "$ERLMCP_OTP_BIN" ]; then
    export PATH="${ERLMCP_OTP_BIN}:$PATH"
fi

print_error_banner() {
    echo ""
    echo -e "${BOLD}${RED}════════════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${RED}  ERLANG VERSION ERROR - COMPILATION BLOCKED${NC}"
    echo -e "${BOLD}${RED}════════════════════════════════════════════════════════════════════${NC}"
    echo ""
}

print_success_banner() {
    echo -e "${GREEN}✓${NC} Erlang/OTP version check passed: OTP $1"
}

# Check if we're in Docker and use container OTP
if [ -f "/.dockerenv" ] || [ -n "${DOCKER_CONTAINER:-}" ]; then
    # In Docker - use the container's OTP
    CURRENT_OTP=$(erl -eval "erlang:display(erlang:system_info(otp_release)), halt()." -noshell)
    if [ "$CURRENT_OTP" -ge "$REQUIRED_OTP_MAJOR" ]; then
        print_success_banner "$CURRENT_OTP"
        exit 0
    else
        print_error_banner
        echo -e "${RED}  ERROR: Container OTP version $CURRENT_OTP is less than required $REQUIRED_OTP_MAJOR${NC}"
        exit 1
    fi
elif [ ! -d "$ERLMCP_OTP_BIN" ]; then
    # On host - require custom OTP
    print_error_banner
    echo -e "${RED}  ERROR: Custom OTP ${ERLMCP_OTP_VERSION} not found at ${ERLMCP_OTP_BIN}${NC}"
    echo ""
    echo -e "${YELLOW}  This project requires Erlang/OTP ${ERLMCP_OTP_VERSION} (built from GitHub source).${NC}"
    echo ""
    echo -e "${BLUE}  To build and install OTP ${ERLMCP_OTP_VERSION} from source:${NC}"
    echo ""
    echo "    cd /Users/sac"
    echo "    curl -LO https://github.com/erlang/otp/releases/download/OTP-${ERLMCP_OTP_VERSION}/otp_src_${ERLMCP_OTP_VERSION}.tar.gz"
    echo "    tar xzf otp_src_${ERLMCP_OTP_VERSION}.tar.gz"
    echo "    cd otp_src_${ERLMCP_OTP_VERSION}"
    echo "    ./configure --prefix=/Users/sac/.erlmcp/otp-${ERLMCP_OTP_VERSION}"
    echo "    make -j\$(sysctl -n hw.ncpu)"
    echo "    make install"
    echo ""
    echo -e "${RED}════════════════════════════════════════════════════════════════════${NC}"
    echo ""
    exit 1
fi

# Check if erl binary exists
if [ ! -x "${ERLMCP_OTP_BIN}/erl" ]; then
    print_error_banner
    echo -e "${RED}  ERROR: erl binary not found at ${ERLMCP_OTP_BIN}/erl${NC}"
    echo ""
    echo -e "${YELLOW}  Your OTP installation may be incomplete.${NC}"
    echo ""
    echo -e "${BLUE}  Rebuild OTP ${ERLMCP_OTP_VERSION} from source:${NC}"
    echo ""
    echo "    cd /Users/sac/otp_src_${ERLMCP_OTP_VERSION}"
    echo "    make clean"
    echo "    ./configure --prefix=/Users/sac/.erlmcp/otp-${ERLMCP_OTP_VERSION}"
    echo "    make -j\$(sysctl -n hw.ncpu)"
    echo "    make install"
    echo ""
    echo -e "${RED}════════════════════════════════════════════════════════════════════${NC}"
    echo ""
    exit 1
fi

# Get OTP version
OTP_VERSION=$(erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell 2>/dev/null)

# Handle version strings like "28" or "28.0" or "28.0.1"
OTP_MAJOR=$(echo "$OTP_VERSION" | sed 's/[^0-9].*//')

# Validate we got a number
if [ -z "$OTP_MAJOR" ] || ! echo "$OTP_MAJOR" | grep -q '^[0-9]\+$'; then
    print_error_banner
    echo -e "${RED}  ERROR: Could not determine Erlang/OTP version${NC}"
    echo ""
    echo -e "${YELLOW}  Raw version string: '${OTP_VERSION}'${NC}"
    echo ""
    echo -e "${BLUE}  Please ensure Erlang is properly installed and try again.${NC}"
    echo ""
    exit 1
fi

# Check version requirement - must be OTP 28 or higher
if [ "$OTP_MAJOR" -lt "$REQUIRED_OTP_MAJOR" ]; then
    print_error_banner
    echo -e "${RED}  ERROR: Erlang/OTP version mismatch${NC}"
    echo ""
    echo -e "${YELLOW}  This project requires OTP ${ERLMCP_OTP_VERSION} (custom-built from source).${NC}"
    echo -e "${YELLOW}  You are currently using OTP ${OTP_VERSION}.${NC}"
    echo ""
    echo -e "${BLUE}  The custom OTP ${ERLMCP_OTP_VERSION} is located at:${NC}"
    echo "    ${ERLMCP_OTP_BIN}"
    echo ""
    echo -e "${BLUE}  Your PATH may be pointing to a different Erlang installation.${NC}"
    echo ""
    echo -e "${BLUE}  To rebuild OTP ${ERLMCP_OTP_VERSION} from source:${NC}"
    echo ""
    echo "    cd /Users/sac"
    echo "    curl -LO https://github.com/erlang/otp/releases/download/OTP-${ERLMCP_OTP_VERSION}/otp_src_${ERLMCP_OTP_VERSION}.tar.gz"
    echo "    tar xzf otp_src_${ERLMCP_OTP_VERSION}.tar.gz"
    echo "    cd otp_src_${ERLMCP_OTP_VERSION}"
    echo "    ./configure --prefix=/Users/sac/.erlmcp/otp-${ERLMCP_OTP_VERSION}"
    echo "    make -j\$(sysctl -n hw.ncpu)"
    echo "    make install"
    echo ""
    echo -e "${RED}  Refusal Code: OTP_VERSION_MISMATCH${NC}"
    echo -e "${RED}  Current: OTP ${OTP_VERSION} | Required: OTP ${ERLMCP_OTP_VERSION}${NC}"
    echo ""
    echo -e "${RED}════════════════════════════════════════════════════════════════════${NC}"
    echo ""
    exit 1
fi

# Success - using custom OTP 28.3.1
print_success_banner "${ERLMCP_OTP_VERSION} (custom-built)"
echo -e "${BLUE}  Binary: ${ERLMCP_OTP_BIN}/erl${NC}"
exit 0
