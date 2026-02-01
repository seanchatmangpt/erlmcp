#!/usr/bin/env bash
# .claude/hooks/hook-lib.sh
# Shared utilities for all Claude Code hooks
# Version: 1.0.0

set -euo pipefail

# Colors
readonly HOOK_GREEN='\033[0;32m'
readonly HOOK_RED='\033[0;31m'
readonly HOOK_YELLOW='\033[0;33m'
readonly HOOK_BLUE='\033[0;34m'
readonly HOOK_NC='\033[0m'

# Project root detection
hook_project_root() {
    local dir="$(pwd)"
    while [[ "$dir" != "/" ]]; do
        if [[ -f "$dir/rebar.config" ]]; then
            echo "$dir"
            return 0
        fi
        dir="$(dirname "$dir")"
    done
    pwd
}

readonly HOOK_PROJECT_ROOT="${HOOK_PROJECT_ROOT:-$(hook_project_root)}"
readonly HOOK_ERLMCP_DIR="${HOOK_PROJECT_ROOT}/.erlmcp"
readonly HOOK_LOG_FILE="${HOOK_ERLMCP_DIR}/hook.log"

# Logging
hook_init_log() {
    mkdir -p "$(dirname "$HOOK_LOG_FILE")"
}

hook_log() {
    local level="$1"
    shift
    hook_init_log
    echo "[$(date -Iseconds)] [$level] $*" >> "$HOOK_LOG_FILE"
}

hook_log_debug() {
    if [[ "${HOOK_DEBUG:-false}" == "true" ]]; then
        hook_log "DEBUG" "$@"
    fi
}

hook_log_error() {
    hook_log "ERROR" "$@"
    echo -e "${HOOK_RED}[ERROR]${HOOK_NC} $*" >&2
}

# JSON helpers
has_jq() {
    command -v jq &> /dev/null
}

output_decision() {
    local decision="$1"
    local reason="$2"
    local modified_command="${3:-}"

    hook_log "DECISION" "$decision: $reason"

    if has_jq; then
        if [[ -n "$modified_command" ]]; then
            jq -n --arg decision "$decision" --arg reason "$reason" --arg cmd "$modified_command" \
                '{permissionDecision: $decision, reason: $reason, input: {command: $cmd}}'
        else
            jq -n --arg decision "$decision" --arg reason "$reason" \
                '{permissionDecision: $decision, reason: $reason}'
        fi
    else
        if [[ -n "$modified_command" ]]; then
            echo "{\"permissionDecision\": \"${decision}\", \"reason\": \"${reason}\", \"input\": {\"command\": \"${modified_command}\"}}"
        else
            echo "{\"permissionDecision\": \"${decision}\", \"reason\": \"${reason}\"}"
        fi
    fi
}

output_allow() {
    output_decision "allow" "$@"
}

output_deny() {
    output_decision "deny" "$@"
}

output_ask() {
    output_decision "ask" "$@"
}

# Environment validation
ensure_otp_env() {
    local env_file="${HOOK_ERLMCP_DIR}/env.sh"

    if [[ -f "$env_file" ]]; then
        hook_log_debug "Sourcing OTP env from: $env_file"
        source "$env_file"
        return 0
    fi

    hook_log "WARN" "OTP env.sh not found, using system PATH"

    if command -v erl &> /dev/null; then
        return 0
    fi

    hook_log_error "No OTP found"
    return 1
}

validate_otp_version() {
    local required_major="${1:-28}"
    local otp_bin="${ERLMCP_OTP_BIN:-erl}"

    if ! command -v "$otp_bin" &> /dev/null; then
        hook_log_error "OTP binary not found: $otp_bin"
        return 2
    fi

    local version
    version=$("$otp_bin" -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null || echo "0")

    if [[ "$version" =~ ^[0-9]+ ]]; then
        local major="${version%%.*}"
        if [[ $major -ge $required_major ]]; then
            hook_log_debug "OTP version $version >= $required_major (OK)"
            return 0
        else
            hook_log_error "OTP version $version < $required_major (required)"
            return 1
        fi
    else
        hook_log_error "Could not parse OTP version: $version"
        return 2
    fi
}

# Health check
hook_health_check() {
    local issues=0

    if [[ ! -d "$HOOK_PROJECT_ROOT" ]]; then
        hook_log_error "Project root not found: $HOOK_PROJECT_ROOT"
        issues=$((issues + 1))
    fi

    if ! command -v erl &> /dev/null && [[ ! -f "${HOOK_ERLMCP_DIR}/env.sh" ]]; then
        hook_log_error "Neither OTP nor env.sh available"
        issues=$((issues + 1))
    fi

    return $issues
}

# Export functions
export -f hook_log hook_log_debug hook_log_error
export -f ensure_otp_env validate_otp_version
export -f output_decision output_allow output_deny output_ask
export -f has_jq
export -f hook_health_check
