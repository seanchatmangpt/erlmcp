#!/usr/bin/env bash
# git-utils.sh - Idempotent git operations for cloud safety
#
# Purpose: Provide retry-safe git operations that comply with cloud idempotency requirements
# Rule: ∀command. idempotent(command) ∨ guarded(command)

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# idempotent_git_push - Push refs with retry safety
#
# Args:
#   $1: remote (e.g., "origin")
#   $2: refspec (e.g., "v1.0.0" or "main")
#   $3: ref_type ("tag" or "branch")
#
# Returns:
#   0: Success (pushed or already exists)
#   1: Genuine failure after retries
#
# Idempotency guarantee:
#   - Checks if ref already exists on remote before pushing
#   - Retries on transient failures (network timeouts)
#   - Returns success if ref appears during retry (race condition)
#   - Safe to call multiple times with same arguments
idempotent_git_push() {
    local remote="$1"
    local refspec="$2"
    local ref_type="$3"  # "tag" or "branch"
    local max_retries=3
    local retry_count=0

    # Validate inputs
    if [[ -z "$remote" || -z "$refspec" || -z "$ref_type" ]]; then
        echo -e "${RED}✗ Invalid arguments to idempotent_git_push${NC}" >&2
        return 1
    fi

    if [[ "$ref_type" != "tag" && "$ref_type" != "branch" ]]; then
        echo -e "${RED}✗ ref_type must be 'tag' or 'branch', got: $ref_type${NC}" >&2
        return 1
    fi

    # Check if already exists on remote (idempotency check)
    if git ls-remote --exit-code "$remote" "refs/${ref_type}s/$refspec" &>/dev/null; then
        echo -e "${BLUE}ℹ $ref_type $refspec already exists on $remote (idempotent)${NC}"
        return 0
    fi

    # Push with retry logic
    while [[ $retry_count -lt $max_retries ]]; do
        if git push "$remote" "$refspec" 2>&1; then
            echo -e "${GREEN}✓ Pushed $ref_type $refspec to $remote${NC}"
            return 0
        fi

        # Check if it appeared during push attempt (race condition with concurrent push)
        if git ls-remote --exit-code "$remote" "refs/${ref_type}s/$refspec" &>/dev/null; then
            echo -e "${BLUE}ℹ $ref_type $refspec appeared during push (idempotent)${NC}"
            return 0
        fi

        retry_count=$((retry_count + 1))
        if [[ $retry_count -lt $max_retries ]]; then
            local backoff=$((retry_count * 2))
            echo -e "${YELLOW}⚠ Push failed (attempt $retry_count/$max_retries), retrying in ${backoff}s...${NC}" >&2
            sleep "$backoff"
        fi
    done

    echo -e "${RED}✗ Failed to push $ref_type $refspec after $max_retries attempts${NC}" >&2
    return 1
}

# idempotent_git_push_branch - Push branch with idempotency guarantee
#
# Args:
#   $1: remote (e.g., "origin")
#   $2: branch name (e.g., "main" or "release/v1.0.0")
#
# Returns:
#   0: Success (pushed or already exists)
#   1: Failure
idempotent_git_push_branch() {
    local remote="$1"
    local branch="$2"
    idempotent_git_push "$remote" "$branch" "branch"
}

# idempotent_git_push_tag - Push tag with idempotency guarantee
#
# Args:
#   $1: remote (e.g., "origin")
#   $2: tag name (e.g., "v1.0.0")
#
# Returns:
#   0: Success (pushed or already exists)
#   1: Failure
idempotent_git_push_tag() {
    local remote="$1"
    local tag="$2"
    idempotent_git_push "$remote" "$tag" "tag"
}

# verify_remote_ref - Check if a ref exists on remote
#
# Args:
#   $1: remote (e.g., "origin")
#   $2: ref pattern (e.g., "refs/tags/v1.0.0")
#
# Returns:
#   0: Ref exists
#   1: Ref does not exist
verify_remote_ref() {
    local remote="$1"
    local ref="$2"
    git ls-remote --exit-code "$remote" "$ref" &>/dev/null
}

# Example usage (uncomment to test):
# idempotent_git_push_tag origin "v1.0.0"
# idempotent_git_push_branch origin "main"
