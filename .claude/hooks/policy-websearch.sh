#!/usr/bin/env bash
# policy-websearch.sh - WebSearch domain filtering hook
#
# Purpose: Validate and filter web search domains
# Trigger: PreToolUse for WebSearch tool
# Output: JSON decision {permissionDecision: "allow"|"ask"|"deny", reason: "..."}

set -euo pipefail

# Output JSON decision
output_decision() {
    local decision="$1"
    local reason="$2"
    echo "{\"permissionDecision\": \"${decision}\", \"reason\": \"${reason}\"}"
}

# Main
main() {
    local input
    input=$(cat)

    # Handle empty input
    if [[ -z "$input" ]]; then
        output_decision "deny" "Empty input received"
        exit 0
    fi

    # Parse tool
    local tool
    if command -v jq &> /dev/null; then
        tool=$(echo "$input" | jq -r '.tool // .name // ""')
    else
        tool=$(echo "$input" | grep -o '"tool"[[:space:]]*:[[:space:]]*"[^"]*"' | sed 's/.*"\([^"]*\)".*/\1/' || echo "")
    fi

    # If not a WebSearch tool, allow
    if [[ "$tool" != "WebSearch" ]]; then
        output_decision "allow" "Not a WebSearch tool"
        exit 0
    fi

    # Allow all web searches
    output_decision "allow" "Web searches allowed"
}

main
