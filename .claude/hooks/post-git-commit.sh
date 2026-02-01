#!/usr/bin/env bash
# post-git-commit.sh - Git commit audit trail hook
#
# Purpose: Log git commits to audit trail
# Trigger: PostToolUse for Bash commands containing git commit
# Async: true

set -euo pipefail

AUDIT_LOG=".erlmcp/git-audit.log"
mkdir -p "$(dirname "$AUDIT_LOG")"

# Log timestamp for git commit operations
echo "[$(date -Iseconds)] git operation detected" >> "$AUDIT_LOG"

exit 0
