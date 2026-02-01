#!/usr/bin/env bash
# Erlang/OTP Version Enforcement Script
#
# This script enforces that only OTP 28+ is used for erlmcp development.
# It provides clear error messages when a lower version is detected.
#
# Usage:
#   ./scripts/check_erlang_version.sh
#
# Exit codes:
#   0 - OTP 28+ detected, ready to proceed
#   1 - OTP version too low or Erlang not found

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

# Required minimum version
REQUIRED_OTP_MAJOR=28

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

# Check if Erlang is installed
if ! command -v erl >/dev/null 2>&1; then
    print_error_banner
    echo -e "${RED}  ERROR: Erlang/OTP is not installed or not in PATH${NC}"
    echo ""
    echo -e "${YELLOW}  This project requires Erlang/OTP ${REQUIRED_OTP_MAJOR} or higher.${NC}"
    echo ""
    echo -e "${BLUE}  Installation instructions:${NC}"
    echo ""
    echo "    macOS (Homebrew):"
    echo "      brew install erlang"
    echo ""
    echo "    Ubuntu/Debian:"
    echo "      sudo apt-get install erlang"
    echo ""
    echo "    asdf version manager (recommended):"
    echo "      asdf plugin add erlang"
    echo "      asdf install erlang ${REQUIRED_OTP_MAJOR}.0"
    echo "      asdf global erlang ${REQUIRED_OTP_MAJOR}.0"
    echo ""
    echo "    kerl (Erlang version manager):"
    echo "      kerl build ${REQUIRED_OTP_MAJOR}.0 ${REQUIRED_OTP_MAJOR}.0"
    echo "      kerl install ${REQUIRED_OTP_MAJOR}.0 ~/erlang/${REQUIRED_OTP_MAJOR}.0"
    echo "      . ~/erlang/${REQUIRED_OTP_MAJOR}.0/activate"
    echo ""
    echo -e "${RED}════════════════════════════════════════════════════════════════════${NC}"
    echo ""
    exit 1
fi

# Get OTP version
OTP_VERSION=$(erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell 2>/dev/null)

# Handle version strings like "28" or "28.0" or "28.0.1"
OTP_MAJOR=$(echo "$OTP_VERSION" | sed 's/[^0-9].*//' | sed 's/^0*//')

# Validate we got a number
if ! [[ "$OTP_MAJOR" =~ ^[0-9]+$ ]]; then
    print_error_banner
    echo -e "${RED}  ERROR: Could not determine Erlang/OTP version${NC}"
    echo ""
    echo -e "${YELLOW}  Raw version string: '${OTP_VERSION}'${NC}"
    echo ""
    echo -e "${BLUE}  Please ensure Erlang is properly installed and try again.${NC}"
    echo ""
    exit 1
fi

# Check version requirement
if [ "$OTP_MAJOR" -lt "$REQUIRED_OTP_MAJOR" ]; then
    print_error_banner
    echo -e "${RED}  ERROR: Erlang/OTP version ${OTP_VERSION} is not supported${NC}"
    echo ""
    echo -e "${YELLOW}  This project requires Erlang/OTP ${REQUIRED_OTP_MAJOR} or higher.${NC}"
    echo -e "${YELLOW}  You are currently using OTP ${OTP_VERSION}.${NC}"
    echo ""
    echo -e "${BLUE}  Why OTP ${REQUIRED_OTP_MAJOR}+ is required:${NC}"
    echo ""
    echo "    - Latest JIT compiler performance optimizations"
    echo "    - Critical security patches and hardening"
    echo "    - Modern Erlang language features"
    echo "    - Improved diagnostics and error messages"
    echo "    - Better memory management and GC"
    echo ""
    echo -e "${BLUE}  How to upgrade:${NC}"
    echo ""
    echo "    asdf (recommended):"
    echo "      asdf install erlang ${REQUIRED_OTP_MAJOR}.0"
    echo "      asdf local erlang ${REQUIRED_OTP_MAJOR}.0"
    echo ""
    echo "    kerl:"
    echo "      kerl build ${REQUIRED_OTP_MAJOR}.0 ${REQUIRED_OTP_MAJOR}.0"
    echo "      kerl install ${REQUIRED_OTP_MAJOR}.0 ~/erlang/${REQUIRED_OTP_MAJOR}.0"
    echo "      . ~/erlang/${REQUIRED_OTP_MAJOR}.0/activate"
    echo ""
    echo "    Homebrew (macOS):"
    echo "      brew upgrade erlang"
    echo ""
    echo "    Package manager (Linux):"
    echo "      Check https://www.erlang.org/downloads for latest packages"
    echo ""
    echo -e "${RED}  Refusal Code: OTP_VERSION_TOO_LOW${NC}"
    echo -e "${RED}  Current: OTP ${OTP_VERSION} | Required: OTP ${REQUIRED_OTP_MAJOR}+${NC}"
    echo ""
    echo -e "${RED}════════════════════════════════════════════════════════════════════${NC}"
    echo ""
    exit 1
fi

# Success
print_success_banner "$OTP_VERSION"
exit 0
