#!/usr/bin/env bash
# policy-write.sh - Write/Edit file pattern validation hook
#
# Purpose: Validate file patterns for Write/Edit operations
# Trigger: PreToolUse for Write/Edit tools
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

    # If not a Write/Edit tool, allow
    if [[ "$tool" != "Write" && "$tool" != "Edit" ]]; then
        output_decision "allow" "Not a Write/Edit tool"
        exit 0
    fi

    # Parse filepath
    local filepath
    if command -v jq &> /dev/null; then
        filepath=$(echo "$input" | jq -r '.input.file_path // .input.path // .file_path // ""')
    else
        filepath=$(echo "$input" | grep -o '"file_path"[[:space:]]*:[[:space:]]*"[^"]*"' | sed 's/.*"\([^"]*\)".*/\1/' || echo "")
    fi

    # Deny security-sensitive files
    if [[ "$filepath" =~ \.env$ ]] || [[ "$filepath" =~ \.pem$ ]] || [[ "$filepath" =~ \.key$ ]]; then
        output_decision "deny" "Write to security-sensitive file denied: $filepath"
        exit 0
    fi

    # Allow all other writes
    output_decision "allow" "Write operation allowed: $filepath"
}

main
