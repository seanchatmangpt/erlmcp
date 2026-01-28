#!/usr/bin/env bash
# Claude Code Post-Task Validation Hook
# Runs after any agent completes a task
# Checks CLAUDE.md compliance and blocks task completion if non-compliant
#
# Hook Trigger: After task completion
# Exit 0: Task validation passed, continue
# Exit 1: Validation failed, block task completion

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
ENFORCER="$PROJECT_ROOT/tools/claude-md-enforcer.sh"

# Hook metadata
HOOK_NAME="post-task-validate"
TASK_ID="${1:-unknown}"
TASK_DESCRIPTION="${2:-No description}"

log_hook() {
    echo -e "${BLUE}[HOOK:$HOOK_NAME]${NC} $*"
}

log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Main validation
main() {
    log_hook "Post-Task Validation Hook"
    log_hook "Task ID: $TASK_ID"
    log_hook "Description: $TASK_DESCRIPTION"
    echo ""

    # Check if enforcer exists
    if [[ ! -f "$ENFORCER" ]]; then
        log_error "CLAUDE.md enforcer not found at $ENFORCER"
        log_error "Cannot validate task completion"
        exit 1
    fi

    # Make enforcer executable
    chmod +x "$ENFORCER"

    # Run CLAUDE.md enforcer
    log_hook "Running CLAUDE.md quality rules validation..."
    echo ""

    if "$ENFORCER"; then
        echo ""
        log_hook "✅ Post-task validation PASSED"
        log_hook "Task completion approved"
        exit 0
    else
        echo ""
        log_hook "❌ Post-task validation FAILED"
        log_hook "Task completion BLOCKED"
        echo ""
        log_error "RESOLUTION REQUIRED:"
        log_error "1. Review violations reported above"
        log_error "2. Fix issues in code"
        log_error "3. Re-run validation: $ENFORCER"
        log_error "4. Task will remain incomplete until validation passes"
        echo ""
        exit 1
    fi
}

main "$@"
