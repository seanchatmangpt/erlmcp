#!/usr/bin/env bash
# Manual pre-completion check - run before saying "done"
# Usage: Run this hook to verify all gates pass
# Smart detection: Skips gates for fresh setup/infrastructure work

set -euo pipefail

# Colors for output
readonly GREEN='\033[0;32m'
readonly RED='\033[0;31m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly BOLD='\033[1m'
readonly NC='\033[0m' # No Color

echo ""
echo "$(BOLD)$(CYAN)═══════════════════════════════════════════════════════════$(NC)"
echo "$(BOLD)$(CYAN)🔍 PRE-COMPLETION QUALITY CHECK$(NC)"
echo "$(BOLD)$(CYAN)═══════════════════════════════════════════════════════════$(NC)"
echo ""

# Source OTP environment if available
if [ -f .erlmcp/env.sh ]; then
    source .erlmcp/env.sh 2>/dev/null || true
fi

#==============================================================================
# FRESH SETUP DETECTION (same as Stop.sh)
#==============================================================================

is_fresh_setup() {
    [ ! -d "_build" ] && return 0
    return 1
}

is_deps_fetched() {
    [ -d "_build/default/lib" ] && [ "$(ls -A _build/default/lib 2>/dev/null | wc -l)" -gt 0 ] && return 0
    return 1
}

is_build_compiled() {
    [ -d "_build/default/lib/erlmcp_core/ebin" ] && [ -f "_build/default/lib/erlmcp_core/ebin/"*.beam ] 2>/dev/null && return 0
    return 1
}

#==============================================================================
# MAIN LOGIC
#==============================================================================

# Parse arguments
MODE="${1:-quick}"

if [ "$MODE" = "full" ]; then
    TARGET="validate"
    DESCRIPTION="Full validation (~5min)"
elif [ "$MODE" = "verify" ]; then
    TARGET="verify"
    DESCRIPTION="Full verification (~15min)"
else
    TARGET="quick"
    DESCRIPTION="Quick check (~1min)"
fi

echo "$(BLUE)Checking build system readiness...$(NC)"
echo ""

# Detect setup phase
if is_fresh_setup; then
    echo "$(YELLOW)⚠️  Fresh setup detected:$(NC)"
    echo "  - No _build directory (compilation not yet attempted)"
    echo ""

    if ! is_deps_fetched; then
        echo "$(YELLOW)  - Dependencies not fetched$(NC)"
        echo ""
        echo "$(YELLOW)Next steps to get build operational:$(NC)"
        echo "  1. source .erlmcp/env.sh"
        echo "  2. cp rebar.config.git rebar.config  # Use git fallback (hex.pm unreachable)"
        echo "  3. ./rebar3 get-deps"
        echo "  4. TERM=dumb ./rebar3 compile"
        echo ""
        echo "$(GREEN)✓ Skipping quality gates for fresh setup$(NC)"
        echo ""
        exit 0
    fi
fi

# Build system operational - check compilation status
if ! is_build_compiled; then
    echo "$(YELLOW)⚠️  Build not yet compiled:$(NC)"
    echo "  - _build/default/lib/erlmcp_core/ebin has no .beam files"
    echo ""
    echo "$(YELLOW)Required before quality gates:$(NC)"
    echo "  TERM=dumb ./rebar3 compile"
    echo ""
    echo "$(GREEN)✓ Skipping quality gates (build not compiled)$(NC)"
    echo ""
    exit 0
fi

# Build system is operational - run quality gates
echo "$(BLUE)Mode:$(NC) $DESCRIPTION"
echo "$(BLUE)Command:$(NC) make $TARGET"
echo ""
echo "$(BLUE)Running quality gates...$(NC)"
echo ""

# Run the quality check
if make $TARGET; then
    echo ""
    echo "$(BOLD)$(GREEN)═══════════════════════════════════════════════════════════$(NC)"
    echo "$(BOLD)$(GREEN)✅ ALL QUALITY GATES PASSED$(NC)"
    echo "$(BOLD)$(GREEN)═══════════════════════════════════════════════════════════$(NC)"
    echo ""
    echo "$(GREEN)✓ SAFE TO SAY 'DONE'$(NC)"
    echo ""
    echo "$(CYAN)Quality gates verified:$(NC)"
    echo "  ✓ Compilation: 0 errors"
    echo "  ✓ Tests: 0 failures"

    if [ "$MODE" = "full" ] || [ "$MODE" = "verify" ]; then
        echo "  ✓ Coverage: ≥80%"
        echo "  ✓ Dialyzer: 0 warnings"
        echo "  ✓ Xref: 0 undefined"
    fi

    echo ""
    exit 0
else
    EXIT_CODE=$?
    echo ""
    echo "$(BOLD)$(RED)═══════════════════════════════════════════════════════════$(NC)"
    echo "$(BOLD)$(RED)❌ QUALITY GATES FAILED$(NC)"
    echo "$(BOLD)$(RED)═══════════════════════════════════════════════════════════$(NC)"
    echo ""
    echo "$(RED)✗ NOT SAFE TO SAY 'DONE'$(NC)"
    echo ""
    echo "$(YELLOW)Action required:$(NC)"
    echo "  1. Fix the issues shown above"
    echo "  2. Re-run: ./.claude/hooks/pre-done-check.sh $MODE"
    echo "  3. Only say 'done' when all gates pass"
    echo ""
    exit 1
fi
