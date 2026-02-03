#!/usr/bin/env bash
#
# erlmcp Downgrade Script - Zero-Downtime Hot Code Downgrade
# Version: 3.0.0
#
# Usage: ./downgrade.sh <target_version> [options]
#
# Options:
#   --dry-run      Show downgrade plan without executing
#   --force        Skip pre-flight checks
#   --timeout=N    Set downgrade timeout in seconds (default: 300)
#
# Example:
#   ./downgrade.sh 2.1.0 --dry-run
#   ./downgrade.sh 2.1.0 --timeout=600
#

set -euo pipefail

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
DOWNGRADE_TIMEOUT=${DOWNGRADE_TIMEOUT:-300}
DRY_RUN=false
FORCE=false
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
            DOWNGRADE_TIMEOUT="${1#*=}"
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

log_info "=== erlmcp Downgrade Script v3.0.0 ==="
log_info "Target Version: ${TARGET_VERSION}"
log_warn "WARNING: Downgrading will revert to ${TARGET_VERSION}"
log_info "Node: ${NODE_NAME}"
log_info "Timeout: ${DOWNGRADE_TIMEOUT}s"

# Pre-flight checks
run_preflight_checks() {
    log_info "Running pre-flight checks..."

    # Check if node is running
    if ! erl -noshell -name "downgrader@127.0.0.1" -setcookie "${COOKIE}" \
        -eval "case net_adm:ping(${NODE_NAME}) of pong -> init:stop(0); pang -> init:stop(1) end" 2>/dev/null; then
        log_error "Node ${NODE_NAME} is not running or not accessible"
        return 1
    fi

    log_info "Pre-flight checks passed"
    return 0
}

# Prepare downgrade
prepare_downgrade() {
    log_info "Preparing downgrade to ${TARGET_VERSION}..."

    erl -noshell -name "downgrader@127.0.0.1" -setcookie "${COOKIE}" \
        -eval "
            case rpc:call(${NODE_NAME}, erlmcp_upgrade_coordinator, prepare_downgrade, [\"${TARGET_VERSION}\"]) of
                {ok, Info} ->
                    io:format(\"~nDowngrade prepared successfully~n\"),
                    io:format(\"Info: ~p~n\", [Info]),
                    init:stop(0);
                {error, Reason} ->
                    io:format(\"~nERROR: Downgrade preparation failed: ~p~n\", [Reason]),
                    init:stop(1)
            end
        " || return 1

    log_info "Downgrade preparation complete"
    return 0
}

# Execute downgrade
execute_downgrade() {
    log_warn "Executing downgrade to ${TARGET_VERSION}..."

    erl -noshell -name "downgrader@127.0.0.1" -setcookie "${COOKIE}" \
        -eval "
            Timeout = ${DOWNGRADE_TIMEOUT} * 1000,
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
            case rpc:call(${NODE_NAME}, erlmcp_upgrade_coordinator, execute_downgrade, [\"${TARGET_VERSION}\", Modules], Timeout) of
                {ok, Info} ->
                    io:format(\"~nDowngrade completed successfully~n\"),
                    io:format(\"Info: ~p~n\", [Info]),
                    init:stop(0);
                {error, Reason} ->
                    io:format(\"~nERROR: Downgrade failed: ~p~n\", [Reason]),
                    init:stop(1);
                {badrpc, Reason} ->
                    io:format(\"~nERROR: RPC failed: ~p~n\", [Reason]),
                    init:stop(1)
            end
        " || return 1

    log_warn "Downgrade execution complete"
    return 0
}

# Verify downgrade
verify_downgrade() {
    log_info "Verifying downgrade to ${TARGET_VERSION}..."

    erl -noshell -name "downgrader@127.0.0.1" -setcookie "${COOKIE}" \
        -eval "
            case rpc:call(${NODE_NAME}, erlmcp_upgrade_coordinator, verify_downgrade, [\"${TARGET_VERSION}\"]) of
                {ok, Info} ->
                    io:format(\"~nDowngrade verification passed~n\"),
                    io:format(\"Info: ~p~n\", [Info]),
                    init:stop(0);
                {error, Reason} ->
                    io:format(\"~nERROR: Downgrade verification failed: ~p~n\", [Reason]),
                    init:stop(1)
            end
        " || return 1

    log_info "Downgrade verification complete"
    return 0
}

# Main downgrade flow
main() {
    if [[ "${FORCE}" != "true" ]]; then
        run_preflight_checks || exit 1
    fi

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "DRY RUN: Would execute downgrade to ${TARGET_VERSION}"
        exit 0
    fi

    prepare_downgrade || exit 1
    execute_downgrade || exit 1
    verify_downgrade || exit 1

    log_info "=== Downgrade to ${TARGET_VERSION} completed successfully ==="
}

# Run main flow
main "$@"
