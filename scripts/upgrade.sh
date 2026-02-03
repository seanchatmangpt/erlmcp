#!/usr/bin/env bash
#
# erlmcp Upgrade Script - Zero-Downtime Hot Code Upgrade
# Version: 3.0.0
#
# Usage: ./upgrade.sh <target_version> [options]
#
# Options:
#   --dry-run      Show upgrade plan without executing
#   --force        Skip pre-flight checks
#   --timeout=N    Set upgrade timeout in seconds (default: 300)
#   --verify-only  Run post-upgrade verification only
#
# Example:
#   ./upgrade.sh 3.0.0 --dry-run
#   ./upgrade.sh 3.0.0 --timeout=600
#

set -euo pipefail

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
UPGRADE_TIMEOUT=${UPGRADE_TIMEOUT:-300}
DRY_RUN=false
FORCE=false
VERIFY_ONLY=false
NODE_NAME=${NODE_NAME:-"erlmcp@127.0.0.1"}
COOKIE=${COOKIE:-"erlmcp"}

# Logging functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Parse arguments
TARGET_VERSION="${1:-}"
shift || true

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run)
            DRY_RUN=true
            ;;
        --force)
            FORCE=true
            ;;
        --timeout=*)
            UPGRADE_TIMEOUT="${1#*=}"
            ;;
        --verify-only)
            VERIFY_ONLY=true
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
    shift
done

# Validate target version
if [[ -z "${TARGET_VERSION}" ]]; then
    log_error "Target version is required"
    echo "Usage: $0 <target_version> [options]"
    exit 1
fi

log_info "=== erlmcp Upgrade Script v3.0.0 ==="
log_info "Target Version: ${TARGET_VERSION}"
log_info "Node: ${NODE_NAME}"
log_info "Timeout: ${UPGRADE_TIMEOUT}s"

# Pre-flight checks
run_preflight_checks() {
    log_info "Running pre-flight checks..."

    # Check if node is running
    if ! erl -noshell -name "upgrader@127.0.0.1" -setcookie "${COOKIE}" \
        -eval "case net_adm:ping(${NODE_NAME}) of pong -> init:stop(0); pang -> init:stop(1) end" 2>/dev/null; then
        log_error "Node ${NODE_NAME} is not running or not accessible"
        return 1
    fi

    # Check disk space
    AVAILABLE_DISK=$(df -BM . | tail -1 | awk '{print $4}' | tr -d 'M')
    if [[ ${AVAILABLE_DISK} -lt 100 ]]; then
        log_error "Insufficient disk space: ${AVAILABLE_DISK}MB < 100MB"
        return 1
    fi

    # Check application state
    log_info "Checking application state..."
    erl -noshell -name "upgrader@127.0.0.1" -setcookie "${COOKIE}" \
        -eval "
            case rpc:call(${NODE_NAME}, erlmcp_upgrade_coordinator, get_upgrade_status, []) of
                {ok, #{status := idle}} ->
                    io:format(\"~nSystem ready for upgrade~n\"),
                    init:stop(0);
                {ok, #{status := Status}} ->
                    io:format(\"~nERROR: System not ready (status: ~p)~n\", [Status]),
                    init:stop(1);
                Error ->
                    io:format(\"~nERROR: ~p~n\", [Error]),
                    init:stop(1)
            end
        " || return 1

    log_info "Pre-flight checks passed"
    return 0
}

# Prepare upgrade
prepare_upgrade() {
    log_info "Preparing upgrade to ${TARGET_VERSION}..."

    erl -noshell -name "upgrader@127.0.0.1" -setcookie "${COOKIE}" \
        -eval "
            case rpc:call(${NODE_NAME}, erlmcp_upgrade_coordinator, prepare_upgrade, [\"${TARGET_VERSION}\"]) of
                {ok, Info} ->
                    io:format(\"~nUpgrade prepared successfully~n\"),
                    io:format(\"Info: ~p~n\", [Info]),
                    init:stop(0);
                {error, Reason} ->
                    io:format(\"~nERROR: Upgrade preparation failed: ~p~n\", [Reason]),
                    init:stop(1)
            end
        " || return 1

    log_info "Upgrade preparation complete"
    return 0
}

# Execute upgrade
execute_upgrade() {
    log_info "Executing upgrade to ${TARGET_VERSION}..."

    erl -noshell -name "upgrader@127.0.0.1" -setcookie "${COOKIE}" \
        -eval "
            Timeout = ${UPGRADE_TIMEOUT} * 1000,
            Modules = [
                erlmcp_app,
                erlmcp_sup,
                erlmcp_core_sup,
                erlmcp_server_sup,
                erlmcp_registry,
                erlmcp_server,
                erlmcp_client,
                erlmcp_protocol_versioning,
                erlmcp_upgrade_coordinator,
                erlmcp_state_migration
            ],
            case rpc:call(${NODE_NAME}, erlmcp_upgrade_coordinator, execute_upgrade, [\"${TARGET_VERSION}\", Modules], Timeout) of
                {ok, Info} ->
                    io:format(\"~nUpgrade completed successfully~n\"),
                    io:format(\"Info: ~p~n\", [Info]),
                    init:stop(0);
                {error, Reason} ->
                    io:format(\"~nERROR: Upgrade failed: ~p~n\", [Reason]),
                    init:stop(1);
                {badrpc, Reason} ->
                    io:format(\"~nERROR: RPC failed: ~p~n\", [Reason]),
                    init:stop(1)
            end
        " || return 1

    log_info "Upgrade execution complete"
    return 0
}

# Verify upgrade
verify_upgrade() {
    log_info "Verifying upgrade to ${TARGET_VERSION}..."

    erl -noshell -name "upgrader@127.0.0.1" -setcookie "${COOKIE}" \
        -eval "
            case rpc:call(${NODE_NAME}, erlmcp_upgrade_coordinator, verify_upgrade, [\"${TARGET_VERSION}\"]) of
                {ok, Info} ->
                    io:format(\"~nUpgrade verification passed~n\"),
                    io:format(\"Info: ~p~n\", [Info]),
                    init:stop(0);
                {error, Reason} ->
                    io:format(\"~nERROR: Upgrade verification failed: ~p~n\", [Reason]),
                    init:stop(1)
            end
        " || return 1

    log_info "Upgrade verification complete"
    return 0
}

# Main upgrade flow
main() {
    if [[ "${VERIFY_ONLY}" == "true" ]]; then
        verify_upgrade
        exit $?
    fi

    if [[ "${FORCE}" != "true" ]]; then
        run_preflight_checks || exit 1
    fi

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "DRY RUN: Would execute upgrade to ${TARGET_VERSION}"
        exit 0
    fi

    prepare_upgrade || exit 1
    execute_upgrade || exit 1
    verify_upgrade || exit 1

    log_info "=== Upgrade to ${TARGET_VERSION} completed successfully ==="
}

# Run main flow
main "$@"
