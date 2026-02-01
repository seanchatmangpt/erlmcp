#!/usr/bin/env bash
# .claude/hooks/policy-bash.sh
# PreToolUse hook for network and filesystem governance
# WO-002: Policy-Bash Hook Implementation
#
# Input: PreToolUse event JSON (stdin)
# Output: JSON decision {permissionDecision: "allow"|"ask"|"deny", reason: "..."}

set -euo pipefail

# Allowlisted domains (network access)
ALLOWLISTED_DOMAINS=(
    "github.com"
    "hex.pm"
    "erlang-solutions.com"
    "packages.erlang-solutions.com"
    "github.com/erlang/otp"
)

# Allowlisted commands (build tools)
ALLOWLISTED_COMMANDS=(
    "^erl"
    "^rebar3"
    "^make"
    "^git"
    "^TERM="
)

# Dangerous command patterns (always deny)
DANGEROUS_PATTERNS=(
    "sudo"
    "rm[[:space:]]+-([^[:space:]]*r[^[:space:]]*f|[^[:space:]]*f[^[:space:]]*r)"
    "rm[[:space:]]+-rf"
    "rm[[:space:]]+-fr"
    ">/dev/sd[a-z]"
    "dd[[:space:]]+if="
)

# Non-deterministic commands (deny for cloud determinism)
NON_DETERMINISTIC=(
    "^date"
    "^uname"
    "^whoami"
    "^hostname"
)

# Network command patterns (wget/curl need URL checking)
NETWORK_DOWNLOAD_PATTERNS=(
    "wget"
    "curl"
)

# Git clone needs URL checking
GIT_CLONE_PATTERN="git[[:space:]]+clone"

# Function to check if jq is available
has_jq() {
    command -v jq &> /dev/null
}

# Function to parse JSON (with jq if available, otherwise grep/sed)
parse_json() {
    local key="$1"
    if has_jq; then
        jq -r ".${key} // \"\""
    else
        # Fallback: crude grep/sed parsing
        grep -o "\"${key}\"[[:space:]]*:[[:space:]]*\"[^\"]*\"" | sed "s/\"${key}\"[[:space:]]*:[[:space:]]*\"\([^\"]*\)\"/\1/"
    fi
}

# Function to output JSON decision
output_decision() {
    local decision="$1"
    local reason="$2"

    if has_jq; then
        jq -n --arg decision "$decision" --arg reason "$reason" \
            '{permissionDecision: $decision, reason: $reason}'
    else
        echo "{\"permissionDecision\": \"${decision}\", \"reason\": \"${reason}\"}"
    fi
}

# Main logic
main() {
    # Read JSON input from stdin
    local input
    input=$(cat)

    # Handle empty input
    if [ -z "$input" ]; then
        output_decision "deny" "Empty input received"
        exit 0
    fi

    # Parse tool and command
    local tool
    local command

    if has_jq; then
        tool=$(echo "$input" | jq -r '.tool // .name // ""')
        command=$(echo "$input" | jq -r '.input.command // .command // ""')
    else
        tool=$(echo "$input" | parse_json "tool")
        [ -z "$tool" ] && tool=$(echo "$input" | parse_json "name")
        command=$(echo "$input" | parse_json "command")
    fi

    # If not a Bash tool, allow
    if [[ "$tool" != "Bash" && "$tool" != "bash" ]]; then
        output_decision "allow" "Not a Bash command"
        exit 0
    fi

    # Handle empty command
    if [ -z "$command" ]; then
        output_decision "deny" "Empty command"
        exit 0
    fi

    # Check for dangerous patterns (DENY)
    for pattern in "${DANGEROUS_PATTERNS[@]}"; do
        if echo "$command" | grep -qE "$pattern"; then
            output_decision "deny" "Dangerous command pattern detected: ${pattern}"
            exit 0
        fi
    done

    # Check for non-deterministic commands (DENY for cloud safety)
    for pattern in "${NON_DETERMINISTIC[@]}"; do
        if echo "$command" | grep -qE "$pattern"; then
            output_decision "deny" "Non-deterministic command (cloud safety): ${pattern}"
            exit 0
        fi
    done

    # Check for git clone (needs URL validation)
    if echo "$command" | grep -qE "$GIT_CLONE_PATTERN"; then
        # Check if domain is allowlisted
        local domain_allowed=false
        for domain in "${ALLOWLISTED_DOMAINS[@]}"; do
            if echo "$command" | grep -qF "$domain"; then
                domain_allowed=true
                break
            fi
        done

        if [ "$domain_allowed" = true ]; then
            output_decision "allow" "git clone to allowlisted domain"
        else
            local url_snippet
            url_snippet=$(echo "$command" | grep -oE 'https?://[^[:space:]]+' | head -1 || echo "$command")
            output_decision "ask" "git clone to unknown domain: ${url_snippet:0:100}"
        fi
        exit 0
    fi

    # Check for network download commands (wget/curl need URL validation)
    local is_download_cmd=false
    for pattern in "${NETWORK_DOWNLOAD_PATTERNS[@]}"; do
        if echo "$command" | grep -qE "$pattern"; then
            is_download_cmd=true
            break
        fi
    done

    if [ "$is_download_cmd" = true ]; then
        # Check if domain is allowlisted
        local domain_allowed=false
        for domain in "${ALLOWLISTED_DOMAINS[@]}"; do
            if echo "$command" | grep -qF "$domain"; then
                domain_allowed=true
                break
            fi
        done

        if [ "$domain_allowed" = true ]; then
            output_decision "allow" "Network download to allowlisted domain"
        else
            # Extract URL for better UX in ask prompt
            local url_snippet
            url_snippet=$(echo "$command" | grep -oE 'https?://[^[:space:]]+' | head -1 || echo "$command")
            output_decision "ask" "Network download to unknown domain: ${url_snippet:0:100}"
        fi
        exit 0
    fi

    # Check for allowlisted build commands (includes git without URLs)
    # This handles: git status, git push, git pull, git commit, etc.
    for pattern in "${ALLOWLISTED_COMMANDS[@]}"; do
        if echo "$command" | grep -qE "$pattern"; then
            output_decision "allow" "Allowlisted build command"
            exit 0
        fi
    done

    # Default: ask for unknown commands
    local cmd_snippet="${command:0:80}"
    output_decision "ask" "Unknown command: ${cmd_snippet}..."
}

# Run main
main
