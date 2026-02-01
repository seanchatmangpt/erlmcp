#!/bin/bash
# Environment detection for erlmcp
# Detects: cloud vs local, network availability, platform

set -euo pipefail

# ============================================================================
# Network Detection
# ============================================================================

detect_network_mode() {
    # Check 1: Claude Code environment variable
    if [ "${CLAUDE_CODE_REMOTE:-false}" = "true" ]; then
        echo "cloud"
        return
    fi

    # Check 2: Test GitHub accessibility
    if curl -s --max-time 3 https://github.com >/dev/null 2>&1; then
        # GitHub accessible, check if apt/yum repositories work
        if curl -s --max-time 3 http://archive.ubuntu.com >/dev/null 2>&1; then
            echo "local"  # Full network access
            return
        elif curl -s --max-time 3 http://mirror.centos.org >/dev/null 2>&1; then
            echo "local"  # Full network access (CentOS)
            return
        else
            echo "cloud"  # Restricted network (GitHub OK, package repos blocked)
            return
        fi
    else
        echo "offline"  # No network or heavily restricted
        return
    fi
}

# ============================================================================
# Platform Detection
# ============================================================================

detect_platform() {
    local os=$(uname -s)
    local arch=$(uname -m)

    case "$os" in
        Linux)
            echo "linux-$arch"
            ;;
        Darwin)
            echo "darwin-$arch"
            ;;
        CYGWIN*|MINGW*|MSYS*)
            echo "windows-$arch"
            ;;
        *)
            echo "unknown-$arch"
            ;;
    esac
}

detect_distro() {
    if [ ! -f /etc/os-release ]; then
        echo "unknown"
        return
    fi

    # Source os-release to get distribution info
    . /etc/os-release

    echo "${ID}-${VERSION_ID}"
}

# ============================================================================
# Package Manager Detection
# ============================================================================

detect_package_manager() {
    if command -v apt-get >/dev/null 2>&1; then
        echo "apt"
    elif command -v yum >/dev/null 2>&1; then
        echo "yum"
    elif command -v dnf >/dev/null 2>&1; then
        echo "dnf"
    elif command -v brew >/dev/null 2>&1; then
        echo "brew"
    elif command -v apk >/dev/null 2>&1; then
        echo "apk"
    else
        echo "none"
    fi
}

# ============================================================================
# Version Manager Detection
# ============================================================================

detect_version_manager() {
    if command -v asdf >/dev/null 2>&1; then
        echo "asdf"
    elif command -v kerl >/dev/null 2>&1; then
        echo "kerl"
    elif command -v nix >/dev/null 2>&1; then
        echo "nix"
    else
        echo "none"
    fi
}

# ============================================================================
# Docker Detection
# ============================================================================

detect_docker() {
    if [ -f /.dockerenv ]; then
        echo "docker"
        return
    fi

    if grep -q docker /proc/1/cgroup 2>/dev/null; then
        echo "docker"
        return
    fi

    echo "native"
}

# ============================================================================
# Write Permissions
# ============================================================================

check_write_permissions() {
    local test_dirs=(
        "/usr/local/bin"
        "/opt"
        "$HOME/.local/bin"
        "$HOME/.erlmcp"
    )

    local writable_dirs=()

    for dir in "${test_dirs[@]}"; do
        # Create directory if it doesn't exist
        mkdir -p "$dir" 2>/dev/null || true

        # Check if writable
        if [ -w "$dir" ]; then
            writable_dirs+=("$dir")
        fi
    done

    if [ ${#writable_dirs[@]} -eq 0 ]; then
        echo "none"
    else
        # Return first writable directory
        echo "${writable_dirs[0]}"
    fi
}

# ============================================================================
# Erlang Detection
# ============================================================================

detect_erlang() {
    if command -v erl >/dev/null 2>&1; then
        local version=$(erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell 2>/dev/null)
        echo "installed-$version"
    else
        echo "not-installed"
    fi
}

# ============================================================================
# Report Functions
# ============================================================================

environment_report() {
    cat << EOF
Environment Detection Report
════════════════════════════════════════════════

Network Mode:        $(detect_network_mode)
Platform:            $(detect_platform)
Distribution:        $(detect_distro)
Container:           $(detect_docker)

Package Manager:     $(detect_package_manager)
Version Manager:     $(detect_version_manager)
Erlang/OTP:          $(detect_erlang)

Writable Directory:  $(check_write_permissions)

Network Tests:
  - github.com:      $(curl -s --max-time 3 https://github.com >/dev/null 2>&1 && echo "✓ reachable" || echo "✗ blocked")
  - hex.pm:          $(curl -s --max-time 3 https://hex.pm >/dev/null 2>&1 && echo "✓ reachable" || echo "✗ blocked")
  - apt repos:       $(curl -s --max-time 3 http://archive.ubuntu.com >/dev/null 2>&1 && echo "✓ reachable" || echo "✗ blocked")

Recommendations:
EOF

    local network=$(detect_network_mode)
    local pkg_mgr=$(detect_package_manager)
    local erlang=$(detect_erlang)

    if [ "$network" = "cloud" ]; then
        echo "  - Use GitHub-based installation (restricted network detected)"
        echo "  - Enable caching to avoid re-downloads"
    elif [ "$network" = "offline" ]; then
        echo "  - Use offline installation package"
        echo "  - Pre-download dependencies before going offline"
    elif [ "$network" = "local" ]; then
        echo "  - Can use standard package managers ($pkg_mgr)"
        echo "  - asdf recommended for version management"
    fi

    if [[ "$erlang" == "not-installed" ]]; then
        echo "  - Erlang/OTP not installed - run setup"
    elif [[ "$erlang" =~ ^installed-([0-9]+) ]]; then
        local otp_version="${BASH_REMATCH[1]}"
        if [ "$otp_version" -lt 28 ]; then
            echo "  - Erlang/OTP $otp_version is too old (need 28+)"
        fi
    fi

    echo ""
}

# ============================================================================
# Main Command Dispatch
# ============================================================================

usage() {
    cat << EOF
Usage: $(basename "$0") [command]

Commands:
  network              Detect network mode (cloud/local/offline)
  platform             Detect platform (linux-x86_64, etc.)
  distro               Detect distribution (ubuntu-22.04, etc.)
  package-manager      Detect package manager (apt/yum/brew)
  version-manager      Detect version manager (asdf/kerl/nix)
  docker               Detect if running in Docker
  erlang               Detect Erlang installation
  writable             Find writable directory
  report               Full environment report

Examples:
  $(basename "$0") network
  $(basename "$0") report
EOF
}

# Command dispatch
COMMAND="${1:-report}"

case "$COMMAND" in
    network)
        detect_network_mode
        ;;
    platform)
        detect_platform
        ;;
    distro)
        detect_distro
        ;;
    package-manager)
        detect_package_manager
        ;;
    version-manager)
        detect_version_manager
        ;;
    docker)
        detect_docker
        ;;
    erlang)
        detect_erlang
        ;;
    writable)
        check_write_permissions
        ;;
    report)
        environment_report
        ;;
    help|--help|-h)
        usage
        ;;
    *)
        echo "Unknown command: $COMMAND"
        echo ""
        usage
        exit 1
        ;;
esac
