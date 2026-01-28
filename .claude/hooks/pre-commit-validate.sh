#!/usr/bin/env bash
# Claude Code Pre-Commit Validation Hook
# Validates code against CLAUDE.md rules before git commit
# Blocks commit if quality rules violated
#
# Hook Trigger: Before git commit
# Exit 0: Validation passed, allow commit
# Exit 1: Validation failed, block commit

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
HOOK_NAME="pre-commit-validate"

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

# Check if this is a merge commit (skip validation)
is_merge_commit() {
    git rev-parse -q --verify MERGE_HEAD >/dev/null 2>&1
}

# Main validation
main() {
    log_hook "Pre-Commit Validation Hook"
    log_hook "Project: erlmcp"
    echo ""

    # Skip validation for merge commits
    if is_merge_commit; then
        log_hook "Merge commit detected, skipping validation"
        exit 0
    fi

    # Check if enforcer exists
    if [[ ! -f "$ENFORCER" ]]; then
        log_error "CLAUDE.md enforcer not found at $ENFORCER"
        log_error "Cannot validate commit"
        exit 1
    fi

    # Make enforcer executable
    chmod +x "$ENFORCER"

    # Run CLAUDE.md enforcer
    log_hook "Running CLAUDE.md quality rules validation..."
    echo ""

    if "$ENFORCER"; then
        echo ""
        log_hook "✅ Pre-commit validation PASSED"
        log_hook "Commit approved"
        exit 0
    else
        echo ""
        log_hook "❌ Pre-commit validation FAILED"
        log_hook "Commit BLOCKED"
        echo ""
        log_error "RESOLUTION REQUIRED:"
        log_error "1. Review violations reported above"
        log_error "2. Fix issues in code"
        log_error "3. Re-run validation: $ENFORCER"
        log_error "4. Stage fixed files: git add <files>"
        log_error "5. Retry commit: git commit"
        echo ""
        log_error "To bypass validation (NOT RECOMMENDED):"
        log_error "  git commit --no-verify"
        echo ""
        exit 1
    fi
}

main "$@"
