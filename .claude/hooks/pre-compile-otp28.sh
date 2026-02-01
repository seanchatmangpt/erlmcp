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
    local version
    version=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt(0).' 2>/dev/null)
    if [[ -n "$version" ]]; then
        echo "$version"
    else
        echo "0"
    fi
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

# Auto-install OTP via SessionStart.sh (Linux only)
# This makes the system autonomous - no manual intervention required in cloud environments.
auto_install_otp() {
    local log_file="/home/user/erlmcp/.erlmcp/sessionstart.log"

    print_info "Auto-installing OTP via SessionStart.sh..."
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] [pre-compile-otp28] Auto-installation triggered" >> "$log_file"

    # Call SessionStart.sh with absolute path (safe for cloud)
    # SessionStart.sh has built-in lock file to prevent re-execution
    if bash "/home/user/erlmcp/.claude/hooks/SessionStart.sh"; then
        local exit_code=$?
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] [pre-compile-otp28] SessionStart.sh completed (exit: $exit_code)" >> "$log_file"
        print_success "OTP installation completed"
        return 0
    else
        local exit_code=$?
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] [pre-compile-otp28] SessionStart.sh failed (exit: $exit_code)" >> "$log_file"
        print_error "OTP installation failed (exit code: $exit_code)"
        echo "Check log: $log_file"
        return 1
    fi
}

PLATFORM=$(detect_platform)
CURRENT_VERSION=$(get_otp_version)

print_info "Platform: ${PLATFORM}"
print_info "Current OTP version: ${CURRENT_VERSION}"

# Check if OTP is available and acceptable
if check_otp_version "$CURRENT_VERSION"; then
    print_success "OTP ${CURRENT_VERSION} is acceptable (>= ${OTP_REQUIRED_MAJOR})"
else
    print_error "OTP version ${CURRENT_VERSION} does not meet minimum requirement (>= ${OTP_REQUIRED_MAJOR})"
    echo ""

    # AUTONOMOUS FIX: On Linux (cloud), auto-install OTP via SessionStart.sh
    # On macOS (local), print manual instructions (unchanged behavior)
    if [[ "$PLATFORM" == "linux" ]]; then
        echo -e "${YELLOW}Attempting automatic OTP installation...${NC}"
        echo ""

        # Call auto-install function
        if auto_install_otp; then
            # Re-validate OTP version after installation
            print_info "Re-validating OTP installation..."
            CURRENT_VERSION=$(get_otp_version)

            if check_otp_version "$CURRENT_VERSION"; then
                print_success "OTP ${CURRENT_VERSION} installed and validated successfully!"
                # Continue to export section below (do not exit here)
            else
                print_error "OTP installation completed but version check still fails"
                print_error "Expected: OTP >= ${OTP_REQUIRED_MAJOR}, Got: ${CURRENT_VERSION}"
                echo ""
                echo -e "${YELLOW}Manual troubleshooting required:${NC}"
                echo "  Check log: /home/user/erlmcp/.erlmcp/sessionstart.log"
                echo "  Verify: erl -eval 'erlang:system_info(otp_release)' -noshell -s init stop"
                exit 1
            fi
        else
            # Auto-install failed
            print_error "Automatic OTP installation failed"
            echo ""
            echo -e "${YELLOW}Manual installation required:${NC}"
            echo "  bash /home/user/erlmcp/.claude/hooks/SessionStart.sh"
            echo "  OR check log: /home/user/erlmcp/.erlmcp/sessionstart.log"
            exit 1
        fi

    elif [[ "$PLATFORM" == "macos" ]]; then
        # macOS: unchanged behavior (manual instructions only)
        echo -e "${YELLOW}On macOS, install OTP via Homebrew or build from source:${NC}"
        echo "  brew install erlang"
        echo "  OR"
        echo "  curl -LO https://github.com/erlang/otp/releases/download/OTP-${ERLMCP_OTP_VERSION}/otp_src_${ERLMCP_OTP_VERSION}.tar.gz"
        echo "  tar xzf otp_src_${ERLMCP_OTP_VERSION}.tar.gz && cd otp_src_${ERLMCP_OTP_VERSION}"
        echo "  ./configure && make && make install"
        echo ""
        exit 1
    else
        # Unknown platform
        print_error "Unknown platform: ${PLATFORM}"
        echo "Cannot auto-install OTP on this platform"
        exit 1
    fi
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
