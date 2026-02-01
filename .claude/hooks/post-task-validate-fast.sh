#!/usr/bin/env bash
# Claude Code Post-Task Validation Hook (FAST VERSION)
# Quick validation without running full test suite
# Full validation runs via make validate (separate command)

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Hook metadata
HOOK_NAME="post-task-validate"
TASK_ID="${1:-$(date +%s)}"
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
    log_hook "Post-Task Validation Hook (Fast - Compile Only)"
    log_hook "Task ID: $TASK_ID"
    log_hook "Description: $TASK_DESCRIPTION"
    echo ""

    cd "$PROJECT_ROOT"

    # Source SessionStart environment for OTP 28
    local env_file="${PROJECT_ROOT}/.erlmcp/env.sh"
    if [[ -f "$env_file" ]]; then
        source "$env_file"
        log_hook "Sourced environment from: $env_file"
    fi

    # Quick validation: compilation only
    log_hook "Running quick validation (compilation)..."
    echo ""

    if TERM=dumb rebar3 compile >/dev/null 2>&1; then
        echo ""
        log_hook "✅ Post-task validation PASSED (compilation OK)"
        log_hook "Task completion approved"
        echo ""
        log_info "Run 'make validate' for full quality gates (tests, coverage, etc.)"
        echo ""
        exit 0
    else
        echo ""
        log_hook "❌ Post-task validation FAILED (compilation errors)"
        log_hook "Task completion BLOCKED"
        echo ""
        log_error "RESOLUTION REQUIRED:"
        log_error "1. Fix compilation errors"
        log_error "2. Run: rebar3 compile"
        log_error "3. Task will remain incomplete until compilation passes"
        echo ""
        log_error "For full validation, run: make validate"
        echo ""
        exit 1
    fi
}

main "$@"
