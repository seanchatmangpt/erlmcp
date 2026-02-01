#!/usr/bin/env bash
# Manual pre-completion check - run before saying "done"
# Usage: Run this hook to verify all gates pass

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
