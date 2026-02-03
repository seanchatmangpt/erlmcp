#!/usr/bin/env bash
# Environment health check for erlmcp development
# Validates: Erlang/OTP, rebar3, dependencies, project structure

# ==============================================================================
# DOCKER-ONLY CONSTITUTION: Host execution FORBIDDEN
# ==============================================================================
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/docker_guard.sh"

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
cd "${PROJECT_ROOT}"

ISSUES_FOUND=0

print_header() {
    echo ""
    echo -e "${BOLD}${BLUE}═══════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${BLUE}  $1${NC}"
    echo -e "${BOLD}${BLUE}═══════════════════════════════════════════════════${NC}"
    echo ""
}

print_check() {
    echo -e "${BLUE}→${NC} Checking $1..."
}

print_ok() {
    echo -e "${GREEN}✓${NC} $1"
}

print_warn() {
    echo -e "${YELLOW}⚠${NC} $1"
    ISSUES_FOUND=$((ISSUES_FOUND + 1))
}

print_error() {
    echo -e "${RED}✗${NC} $1"
    ISSUES_FOUND=$((ISSUES_FOUND + 1))
}

command_exists() {
    command -v "$1" >/dev/null 2>&1
}

print_header "Environment Health Check"

# Check Erlang/OTP - STRICT: OTP 28.3.1+ REQUIRED
print_check "Erlang/OTP installation"
if command_exists erl; then
    OTP_VERSION=$(erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell 2>/dev/null)
    OTP_MAJOR=$(echo "$OTP_VERSION" | sed 's/[^0-9].*//' | sed 's/^0*//')
    print_ok "Erlang/OTP $OTP_VERSION installed"

    if [ "$OTP_MAJOR" -ge 28 ]; then
        # Check for recommended 28.3.1
        if [ "$OTP_VERSION" = "28" ]; then
            # Get full version for OTP 28
            FULL_VERSION=$(erl -eval '{ok, V} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:format("~s", [V]), halt().' -noshell 2>/dev/null | tr -d '\n')
            if [ -n "$FULL_VERSION" ]; then
                print_ok "OTP version $FULL_VERSION detected"
                if [ "$FULL_VERSION" = "28.3.1" ]; then
                    print_ok "OTP $FULL_VERSION matches recommended version"
                else
                    print_warn "OTP $FULL_VERSION: recommended version is 28.3.1"
                fi
            else
                print_ok "OTP version $OTP_VERSION is supported (28+)"
            fi
        else
            print_ok "OTP version $OTP_VERSION is supported (28+)"
        fi
    else
        echo ""
        echo -e "${RED}════════════════════════════════════════════════════════════════════${NC}"
        echo -e "${RED}  ERROR: Erlang/OTP $OTP_VERSION is NOT supported${NC}"
        echo -e "${RED}════════════════════════════════════════════════════════════════════${NC}"
        echo ""
        echo -e "${RED}  This project requires Erlang/OTP 28.3.1 or higher.${NC}"
        echo -e "${RED}  You are using OTP $OTP_VERSION.${NC}"
        echo ""
        echo -e "${YELLOW}  To upgrade:${NC}"
        echo "    asdf: asdf install erlang 28.3.1 && asdf local erlang 28.3.1"
        echo "    kerl: kerl build 28.3.1 28.3.1 && kerl install 28.3.1 ~/erlang/28.3.1"
        echo ""
        echo -e "${RED}  Refusal Code: OTP_VERSION_TOO_LOW${NC}"
        echo -e "${RED}════════════════════════════════════════════════════════════════════${NC}"
        echo ""
        exit 1
    fi
else
    print_error "Erlang/OTP not found in PATH"
    exit 1
fi

# Check rebar3
print_check "rebar3 installation"
if command_exists rebar3; then
    REBAR3_VERSION=$(rebar3 version 2>/dev/null | head -1 || echo "unknown")
    print_ok "rebar3 installed: $REBAR3_VERSION"
else
    print_error "rebar3 not found in PATH"
fi

# Check git
print_check "git installation"
if command_exists git; then
    GIT_VERSION=$(git --version | awk '{print $3}')
    print_ok "git $GIT_VERSION installed"
else
    print_warn "git not found (recommended for development)"
fi

# Check OpenSSL (optional but recommended for crypto/TLS)
print_check "OpenSSL installation"
if command_exists openssl; then
    OPENSSL_VERSION=$(openssl version | awk '{print $2}')
    print_ok "OpenSSL $OPENSSL_VERSION installed"
else
    print_warn "OpenSSL not found (recommended for TLS support)"
    echo "         Install: apt-get install openssl / brew install openssl"
fi

# Check ulimits for load testing
print_header "System Limits"

print_check "File descriptor limits (ulimit -n)"
FD_LIMIT=$(ulimit -n 2>/dev/null || echo "unknown")
if [ "$FD_LIMIT" != "unknown" ]; then
    if [ "$FD_LIMIT" -ge 65536 ]; then
        print_ok "File descriptors: $FD_LIMIT (excellent for load testing)"
    elif [ "$FD_LIMIT" -ge 10000 ]; then
        print_ok "File descriptors: $FD_LIMIT (adequate for load testing)"
    elif [ "$FD_LIMIT" -ge 1024 ]; then
        print_warn "File descriptors: $FD_LIMIT (low for high-load testing, recommend 65536+)"
        echo "         Fix: ulimit -n 65536  (or set in /etc/security/limits.conf)"
    else
        print_warn "File descriptors: $FD_LIMIT (too low for load testing, recommend 65536+)"
        echo "         Fix: ulimit -n 65536  (or set in /etc/security/limits.conf)"
    fi
else
    print_warn "Could not determine file descriptor limit"
fi

print_check "Process limits (ulimit -u)"
PROC_LIMIT=$(ulimit -u 2>/dev/null || echo "unknown")
if [ "$PROC_LIMIT" != "unknown" ]; then
    if [ "$PROC_LIMIT" -ge 50000 ]; then
        print_ok "Process limit: $PROC_LIMIT (excellent)"
    elif [ "$PROC_LIMIT" -ge 10000 ]; then
        print_ok "Process limit: $PROC_LIMIT (adequate)"
    else
        print_warn "Process limit: $PROC_LIMIT (may be low for large-scale testing)"
        echo "         Note: Erlang can handle 100K+ lightweight processes"
    fi
else
    print_warn "Could not determine process limit"
fi

# Check for native JSON support in Erlang
print_header "Erlang Features"

print_check "Native JSON support"
if command_exists erl; then
    HAS_JSON=$(erl -eval 'try json:encode(#{test => true}), io:format("true"), halt() catch _:_ -> io:format("false"), halt() end.' -noshell 2>/dev/null)
    if [ "$HAS_JSON" = "true" ]; then
        print_ok "Native json module available (OTP 27+)"
    else
        print_ok "Using jsx library for JSON (native json requires OTP 27+)"
        echo "         Note: erlmcp uses jsx for compatibility"
    fi
else
    print_warn "Could not check JSON support"
fi

# Check project structure
print_header "Project Structure"

print_check "Umbrella apps"
APPS=("erlmcp_core" "erlmcp_transports" "erlmcp_observability" "tcps_erlmcp")
for app in "${APPS[@]}"; do
    if [ -d "apps/$app" ]; then
        if [ -f "apps/$app/src/${app}.app.src" ]; then
            MODULE_COUNT=$(find "apps/$app/src" -name "*.erl" 2>/dev/null | wc -l)
            print_ok "apps/$app: $MODULE_COUNT modules"
        else
            print_error "apps/$app missing .app.src file"
        fi
    else
        print_error "apps/$app directory not found"
    fi
done

# Check rebar configuration
print_check "rebar configuration"
if [ -f "rebar.config" ]; then
    if grep -q "project_app_dirs" rebar.config; then
        print_ok "rebar.config has umbrella configuration"
    else
        print_error "rebar.config missing project_app_dirs"
    fi
else
    print_error "rebar.config not found"
fi

# Check key directories
print_header "Directory Structure"

DIRS=("scripts" "tools" "examples" "docs" ".claude")
for dir in "${DIRS[@]}"; do
    if [ -d "$dir" ]; then
        FILE_COUNT=$(find "$dir" -type f 2>/dev/null | wc -l)
        print_ok "$dir/: $FILE_COUNT files"
    else
        print_warn "$dir/ directory not found"
    fi
done

# Check dependencies
print_header "Dependencies"

print_check "Dependency status"
if [ -f "rebar.lock" ]; then
    print_ok "rebar.lock exists"
    DEP_COUNT=$(grep -c "<<\"" rebar.lock 2>/dev/null || echo "0")
    print_ok "$DEP_COUNT dependencies locked"
else
    print_warn "rebar.lock not found (run 'rebar3 get-deps')"
fi

# Check build artifacts
print_header "Build Status"

if [ -d "_build/default/lib" ]; then
    BUILT_APPS=$(find _build/default/lib -maxdepth 1 -type d | wc -l)
    print_ok "_build directory exists: $BUILT_APPS apps built"

    for app in "${APPS[@]}"; do
        if [ -d "_build/default/lib/$app/ebin" ]; then
            BEAM_COUNT=$(find "_build/default/lib/$app/ebin" -name "*.beam" 2>/dev/null | wc -l)
            if [ "$BEAM_COUNT" -gt 0 ]; then
                print_ok "$app: $BEAM_COUNT compiled modules"
            else
                print_warn "$app: no compiled modules (run 'make compile')"
            fi
        else
            print_warn "$app: not compiled (run 'make compile')"
        fi
    done
else
    print_warn "_build directory not found (run 'make compile')"
fi

# Check quality gates
print_header "Quality Gate Tools"

QUALITY_SCRIPTS=(
    "scripts/build_and_test.sh"
    "scripts/validation/run-ci.sh"
    "tools/test-runner.sh"
    "bin/erlmcp-validate"
)

for script in "${QUALITY_SCRIPTS[@]}"; do
    if [ -f "$script" ]; then
        if [ -x "$script" ]; then
            print_ok "$script (executable)"
        else
            print_warn "$script exists but not executable"
        fi
    else
        print_error "$script not found"
    fi
done

# Summary
print_header "Summary"

if [ $ISSUES_FOUND -eq 0 ]; then
    echo -e "${BOLD}${GREEN}✅ Environment is healthy - ready for development${NC}"
    echo ""
    echo "Next steps:"
    echo "  make compile      # Compile all apps"
    echo "  make quick        # Fast quality check (< 5min)"
    echo "  make verify       # Full validation (< 15min)"
    echo "  make ci-local     # Reproduce CI workflow"
    echo ""
    exit 0
else
    echo -e "${BOLD}${YELLOW}⚠ Found $ISSUES_FOUND issue(s)${NC}"
    echo ""
    echo "Recommended actions:"
    if ! command_exists erl; then
        echo "  1. Install Erlang/OTP 25-28"
    fi
    if ! command_exists rebar3; then
        echo "  2. Install rebar3 (https://rebar3.org)"
    fi
    if [ ! -f "rebar.lock" ]; then
        echo "  3. Run 'rebar3 get-deps' to fetch dependencies"
    fi
    if [ ! -d "_build/default/lib" ]; then
        echo "  4. Run 'make compile' to build project"
    fi
    echo ""
    exit 1
fi
