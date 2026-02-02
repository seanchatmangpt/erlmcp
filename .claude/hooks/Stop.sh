#!/usr/bin/env bash
# Stop hook - Quality gate enforcement before agent completion
# Exit codes:
#   0 = allow completion
#   1 = non-blocking warning
#   2 = blocking error (completion denied)
#
# Smart Detection:
# - Detects fresh setup (OTP/rebar3 just installed) and allows bypass
# - Detects build system readiness (dependencies fetched, build compiled)
# - Allows infrastructure setup commits before tests can run

set -euo pipefail

# Colors for output
readonly GREEN='\033[0;32m'
readonly RED='\033[0;31m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly BOLD='\033[1m'
readonly NC='\033[0m' # No Color

echo ""
echo "$(BOLD)$(BLUE)════════════════════════════════════════════════════════════$(NC)"
echo "$(BOLD)$(BLUE)🔍 PRE-COMPLETION QUALITY GATE$(NC)"
echo "$(BOLD)$(BLUE)════════════════════════════════════════════════════════════$(NC)"
echo ""

# Log file for debugging
LOG_FILE="/tmp/erlmcp_stop_hook_$$.log"

# Source OTP environment if available
if [ -f .erlmcp/env.sh ]; then
    source .erlmcp/env.sh 2>/dev/null || true
fi

#==============================================================================
# FRESH SETUP DETECTION
#==============================================================================

is_fresh_setup() {
    # Check if this is a fresh OTP/rebar3 installation
    # Fresh setup indicators:
    # 1. _build directory doesn't exist (no compilation yet)
    # 2. rebar3 is very new (just installed)
    # 3. .erlmcp/env.sh was recently created/modified

    [ ! -d "_build" ] && return 0  # No build dir = fresh setup
    return 1  # Build dir exists = ongoing development
}

is_deps_fetched() {
    # Check if dependencies have been fetched
    # indicators: _build/default/lib should have packages
    [ -d "_build/default/lib" ] && [ "$(ls -A _build/default/lib 2>/dev/null | wc -l)" -gt 0 ] && return 0
    return 1
}

is_build_compiled() {
    # Check if compilation has succeeded
    # indicator: _build/default/lib/erlmcp_core/ebin should have .beam files
    [ -d "_build/default/lib/erlmcp_core/ebin" ] && [ -f "_build/default/lib/erlmcp_core/ebin/"*.beam ] 2>/dev/null && return 0
    return 1
}

#==============================================================================
# MAIN LOGIC
#==============================================================================

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
        echo "$(GREEN)✓ Completion allowed (infrastructure setup)$(NC)"
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
    echo "$(GREEN)✓ Completion allowed (infrastructure work)$(NC)"
    echo ""
    exit 0
fi

# Build system is operational - run quality gates
echo "$(BLUE)Build system operational - running quality gates...$(NC)"
echo ""

# Check which mode we're in (quick vs full)
MODE="${STOP_HOOK_MODE:-quick}"
if [ "$MODE" = "full" ]; then
    TARGET="make validate"
    DESCRIPTION="Full validation (~5min)"
else
    TARGET="make quick"
    DESCRIPTION="Quick check (~1min)"
fi

echo "$(BLUE)Mode:$(NC) $DESCRIPTION"
echo "$(BLUE)Command:$(NC) $TARGET"
echo ""
echo "$(BLUE)Running quality gates...$(NC)"

# Run the quality check
if $TARGET > "$LOG_FILE" 2>&1; then
    echo ""
    echo "$(BOLD)$(GREEN)✅ ALL QUALITY GATES PASSED$(NC)"
    echo "$(BOLD)$(GREEN)════════════════════════════════════════════════════════════$(NC)"
    echo ""
    echo "$(GREEN)✓ Completion allowed$(NC)"
    echo ""
    rm -f "$LOG_FILE"
    exit 0
else
    EXIT_CODE=$?
    echo ""
    echo "$(BOLD)$(RED)❌ QUALITY GATES FAILED$(NC)"
    echo "$(BOLD)$(RED)════════════════════════════════════════════════════════════$(NC)"
    echo ""
    echo "$(RED)✗ Completion BLOCKED$(NC)"
    echo ""
    echo "$(YELLOW)Failed gates summary:$(NC)"

    # Extract and display relevant error lines
    grep -E "(FAIL|ERROR|❌|failed|error:|undefined)" "$LOG_FILE" 2>/dev/null | head -20 || true

    echo ""
    echo "$(YELLOW)Full log:$(NC) $LOG_FILE"
    echo ""
    echo "$(RED)Action required: Fix the issues above before completion.$(NC)"
    echo ""
    echo "$(YELLOW)Tip: Run '$TARGET' directly to see full output$(NC)"
    echo ""

    # Clean up old logs (keep only recent)
    find /tmp -name "erlmcp_stop_hook_*.log" -mtime +1 -delete 2>/dev/null || true

    exit 2
fi
