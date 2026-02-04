#!/bin/sh
# OTP 28.3.1 Binary Path Configuration
#
# This hook ensures OTP 28.3.1+ is available for erlmcp compilation.
# Works on both Linux (cloud) and macOS (local).
#
# Exit codes:
#   0 - OTP 28 found and PATH configured
#   1 - OTP 28 not found, compilation blocked

set -eu

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
    printf "${GREEN}✓ $1${NC}\n"
}

print_error() {
    printf "${RED}✗ $1${NC}\n" >&2
}

print_info() {
    printf "${BLUE}ℹ $1${NC}\n"
}

# Detect platform
detect_platform() {
    # Check if we're in Alpine container where OSTYPE might not be set
    if [ -f /etc/alpine-release ]; then
        echo "linux"
        return
    fi

    # Default to linux if OSTYPE is not set
    if [ -z "$OSTYPE" ]; then
        echo "linux"
        return
    fi

    case "$OSTYPE" in
        linux-gnu*) echo "linux" ;;
        darwin*) echo "macos" ;;
        *) echo "unknown" ;;
    esac
}

# Get current OTP version
get_otp_version() {
    if ! command -v erl >/dev/null 2>&1; then
        echo "0"
        return 1
    fi
    version=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt(0).' 2>/dev/null)
    if [ -n "$version" ]; then
        echo "$version"
    else
        echo "0"
    fi
}

# Check if OTP version is acceptable
check_otp_version() {
    version="$1"
    # Extract first part of version number (before any dot)
    major=$(echo "$version" | cut -d. -f1)
    # Check if it's a number and >= required major
    case "$major" in
        *[!0-9]*)
            return 1
            ;;
        *)
            if [ "$major" -ge "$OTP_REQUIRED_MAJOR" ]; then
                return 0
            else
                return 1
            fi
            ;;
    esac
}

# Auto-install OTP via SessionStart.sh (Linux only)
# This makes the system autonomous - no manual intervention required in cloud environments.
auto_install_otp() {
    local log_file="/home/user/erlmcp/.erlmcp/sessionstart.log"

    print_info "Auto-installing OTP via SessionStart.sh..."
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] [pre-compile-otp28] Auto-installation triggered" >> "$log_file"

    # Call SessionStart.sh with absolute path (safe for cloud)
    # SessionStart.sh has built-in lock file to prevent re-execution
    if sh "/home/user/erlmcp/.claude/hooks/SessionStart.sh"; then
        exit_code=$?
        echo "[$(date '+%Y-%m-%d %H:%M:%S')] [pre-compile-otp28] SessionStart.sh completed (exit: $exit_code)" >> "$log_file"
        print_success "OTP installation completed"
        return 0
    else
        exit_code=$?
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
    case "$PLATFORM" in
        linux)
            printf "${YELLOW}Attempting automatic OTP installation...${NC}\n"
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
                    printf "${YELLOW}Manual troubleshooting required:${NC}\n"
                    echo "  Check log: /home/user/erlmcp/.erlmcp/sessionstart.log"
                    echo "  Verify: erl -eval 'erlang:system_info(otp_release)' -noshell -s init stop"
                    exit 1
                fi
            else
                # Auto-install failed
                print_error "Automatic OTP installation failed"
                echo ""
                printf "${YELLOW}Manual installation required:${NC}\n"
                echo "  sh /home/user/erlmcp/.claude/hooks/SessionStart.sh"
                echo "  OR check log: /home/user/erlmcp/.erlmcp/sessionstart.log"
                exit 1
            fi
            ;;
        macos)
            # macOS: unchanged behavior (manual instructions only)
            printf "${YELLOW}On macOS, install OTP via Homebrew or build from source:${NC}\n"
            echo "  brew install erlang"
            echo "  OR"
            echo "  curl -LO https://github.com/erlang/otp/releases/download/OTP-${ERLMCP_OTP_VERSION}/otp_src_${ERLMCP_OTP_VERSION}.tar.gz"
            echo "  tar xzf otp_src_${ERLMCP_OTP_VERSION}.tar.gz && cd otp_src_${ERLMCP_OTP_VERSION}"
            echo "  ./configure && make && make install"
            echo ""
            exit 1
            ;;
        *)
            # Unknown platform
            print_error "Unknown platform: ${PLATFORM}"
            echo "Cannot auto-install OTP on this platform"
            exit 1
            ;;
    esac
fi

# Export OTP information for downstream tools
export ERLMCP_OTP_VERSION="$CURRENT_VERSION"
export ERLMCP_OTP_AVAILABLE=true

echo ""
printf "${GREEN}════════════════════════════════════════════════════════════════${NC}\n"
printf "${GREEN}✅ OTP ${CURRENT_VERSION} configured - Ready to compile${NC}\n"
printf "${GREEN}════════════════════════════════════════════════════════════════${NC}\n"
echo ""
exit 0
