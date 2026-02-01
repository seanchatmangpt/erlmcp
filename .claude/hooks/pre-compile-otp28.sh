#!/usr/bin/env bash
# OTP 28.3.1 Binary Path Configuration
#
# This hook ensures the custom-built OTP 28.3.1 is used for all erlmcp compilation.
# The OTP installation is at: /Users/sac/.erlmcp/otp-28.3.1/
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

# Custom OTP 28.3.1 installation
ERLMCP_OTP_BIN="/Users/sac/.erlmcp/otp-28.3.1/bin"
ERLMCP_OTP_VERSION="28.3.1"

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}" >&2
}

# Verify OTP installation
if [[ ! -d "/Users/sac/.erlmcp/otp-28.3.1" ]]; then
    print_error "OTP ${ERLMCP_OTP_VERSION} not found at /Users/sac/.erlmcp/otp-28.3.1/"
    echo ""
    echo -e "${YELLOW}To install OTP ${ERLMCP_OTP_VERSION}:${NC}"
    echo "  cd /Users/sac"
    echo "  curl -LO https://github.com/erlang/otp/releases/download/OTP-${ERLMCP_OTP_VERSION}/otp_src_${ERLMCP_OTP_VERSION}.tar.gz"
    echo "  tar xzf otp_src_${ERLMCP_OTP_VERSION}.tar.gz"
    echo "  cd otp_src_${ERLMCP_OTP_VERSION}"
    echo "  ./configure --prefix=/Users/sac/.erlmcp/otp-${ERLMCP_OTP_VERSION}"
    echo "  make -j\$(sysctl -n hw.ncpu)"
    echo "  make install"
    echo ""
    exit 1
fi

# Verify erl binary
if [[ ! -x "${ERLMCP_OTP_BIN}/erl" ]]; then
    print_error "erl binary not found at ${ERLMCP_OTP_BIN}/erl"
    exit 1
fi

# Verify version
VERSION=$("${ERLMCP_OTP_BIN}/erl" -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell 2>/dev/null || echo "unknown")
if [[ "$VERSION" != "28" ]]; then
    print_error "OTP version mismatch: expected 28, got $VERSION"
    exit 1
fi

print_success "OTP ${ERLMCP_OTP_VERSION} found at ${ERLMCP_OTP_BIN}"

# Export the path for rebar3
export PATH="${ERLMCP_OTP_BIN}:$PATH"
export ERLMCP_OTP_28_BIN="${ERLMCP_OTP_BIN}/erl"

echo ""
echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}✅ OTP 28.3.1 configured - Ready to compile${NC}"
echo -e "${GREEN}════════════════════════════════════════════════════════════════${NC}"
echo ""
exit 0
