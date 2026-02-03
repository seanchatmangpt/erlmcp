#!/usr/bin/env bash
# ============================================================================
# post-create.sh - Dev Container Post-Create Script
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION
#
# This script runs ONCE after the dev container is created.
# It sets up the development environment with all necessary dependencies.
#
# Tasks:
#   1. Fetch hex dependencies
#   2. Compile the project
#   3. Build dialyzer PLT
#   4. Setup git hooks
#   5. Verify Docker-Only Constitution
# ============================================================================

set -euo pipefail

echo "=============================================="
echo "ERLMCP DEV CONTAINER POST-CREATE"
echo "=============================================="
echo "Timestamp: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "OTP Version: $(erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt(0).')"
echo "=============================================="

cd /workspace

# ============================================================================
# Verify Docker-Only Constitution
# ============================================================================
echo ""
echo "[1/6] Verifying Docker-Only Constitution..."

if [[ -f "/.dockerenv" ]] || grep -Eq "(docker|kubepods|containerd)" /proc/1/cgroup 2>/dev/null; then
    echo "  ✓ Running inside Docker container (Constitution upheld)"
else
    echo "  ✗ ERROR: Not running inside Docker!"
    echo "  ANDON: FORBIDDEN_HOST_EXECUTION"
    exit 2
fi

# ============================================================================
# Hex Dependencies
# ============================================================================
echo ""
echo "[2/6] Fetching hex dependencies..."

if rebar3 get-deps; then
    echo "  ✓ Dependencies fetched successfully"
else
    echo "  ⚠ Warning: Some dependencies may have failed"
fi

# ============================================================================
# Initial Compilation
# ============================================================================
echo ""
echo "[3/6] Compiling project..."

if rebar3 compile; then
    echo "  ✓ Project compiled successfully"
else
    echo "  ⚠ Warning: Compilation had issues (may be expected for new setup)"
fi

# ============================================================================
# Dialyzer PLT (Background)
# ============================================================================
echo ""
echo "[4/6] Building Dialyzer PLT (this may take several minutes)..."

# Build PLT in background to not block container startup
(
    if rebar3 dialyzer --plt_apps compiler crypto erts kernel stdlib syntax_tools 2>/dev/null; then
        echo "  ✓ Dialyzer PLT built successfully"
    else
        echo "  ⚠ Dialyzer PLT build had warnings (normal for first build)"
    fi
) &

echo "  → Dialyzer PLT building in background (PID: $!)"

# ============================================================================
# Git Hooks Setup
# ============================================================================
echo ""
echo "[5/6] Setting up Git hooks..."

if [[ -d ".git" ]]; then
    # Create pre-commit hook
    mkdir -p .git/hooks
    cat > .git/hooks/pre-commit << 'HOOK'
#!/usr/bin/env bash
# Pre-commit hook for erlmcp
# CONSTITUTION: DOCKER-ONLY CONSTITUTION

echo "Running pre-commit checks..."

# Check if we're in Docker
if [[ ! -f "/.dockerenv" ]] && ! grep -Eq "(docker|kubepods|containerd)" /proc/1/cgroup 2>/dev/null; then
    echo "ANDON: Pre-commit must run inside Docker container"
    echo "Use: docker compose run --rm erlmcp-build git commit ..."
    exit 1
fi

# Quick compile check
if ! rebar3 compile 2>/dev/null; then
    echo "ERROR: Compilation failed"
    exit 1
fi

# Quick xref check
if ! rebar3 xref 2>/dev/null; then
    echo "ERROR: xref found issues"
    exit 1
fi

echo "✓ Pre-commit checks passed"
HOOK
    chmod +x .git/hooks/pre-commit
    echo "  ✓ Git hooks configured"
else
    echo "  ⚠ No .git directory found (skipping hooks)"
fi

# ============================================================================
# VS Code Settings
# ============================================================================
echo ""
echo "[6/6] Configuring VS Code settings..."

mkdir -p .vscode
cat > .vscode/settings.json << 'SETTINGS'
{
    "erlang.erlangPath": "/usr/local/bin",
    "erlang.rebarPath": "/usr/local/bin/rebar3",
    "erlang_ls.serverPath": "/usr/local/bin/erlang_ls",
    "editor.formatOnSave": true,
    "editor.rulers": [80, 100],
    "files.trimTrailingWhitespace": true,
    "files.insertFinalNewline": true,
    "terminal.integrated.defaultProfile.linux": "bash",
    "[erlang]": {
        "editor.tabSize": 4,
        "editor.insertSpaces": true
    }
}
SETTINGS
echo "  ✓ VS Code settings configured"

# ============================================================================
# Summary
# ============================================================================
echo ""
echo "=============================================="
echo "POST-CREATE COMPLETE"
echo "=============================================="
echo ""
echo "Development environment ready!"
echo ""
echo "Quick commands:"
echo "  rebar3 compile    - Compile the project"
echo "  rebar3 eunit      - Run unit tests"
echo "  rebar3 ct         - Run common tests"
echo "  rebar3 dialyzer   - Run type analysis"
echo "  make doctor       - Run all quality gates"
echo ""
echo "Docker-Only commands (via Makefile):"
echo "  make compile      - Compile via Docker"
echo "  make test         - Test via Docker"
echo "  make validate     - Full validation via Docker"
echo ""
echo "Receipt generation:"
echo "  make receipt-compile   - Compile with proof"
echo "  make receipt-test      - Test with proof"
echo "  make receipt-validate  - Full validation with proof"
echo ""
echo "=============================================="
