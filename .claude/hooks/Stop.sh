#!/usr/bin/env bash
# Stop hook - Quality gate enforcement before agent completion
# Exit codes:
#   0 = allow completion
#   1 = non-blocking warning
#   2 = blocking error (completion denied)

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
