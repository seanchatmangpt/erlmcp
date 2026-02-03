#!/usr/bin/env bash
#
###################################################################################################
# ERLMCP v3: Docker-Only Execution Wrapper with Automatic Receipt Generation
#
# Constitution: DOCKER-ONLY. Host execution FORBIDDEN.
# This script wraps all Docker executions and automatically generates receipts.
#
# Usage: ./scripts/receipts/docker-wrapper.sh <service> <command> [--workdir=<dir>] [--gate=<name>]
#
# Supported Gates (Quality Gates):
#   - compile:   Compilation gate (erlmcp-build)
#   - unit:      Unit test gate (erlmcp-unit)
#   - ct:        Common test gate (erlmcp-ct)
#   - check:     Type check gate (erlmcp-check)
#   - dialyzer:  Dialyzer analysis (erlmcp-build)
#   - coverage:  Coverage analysis (erlmcp-check)
###################################################################################################

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Source the receipt generation functions
RECEIPT_SCRIPT="${SCRIPT_DIR}/generate.sh"

# Colors
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m'

log_info() { echo -e "${BLUE}[DOCKER-WRAPPER]${NC} $*"; }
log_success() { echo -e "${GREEN}[DOCKER-WRAPPER]${NC} $*"; }
log_warning() { echo -e "${YELLOW}[DOCKER-WRAPPER]${NC} $*"; }
log_error() { echo -e "${RED}[DOCKER-WRAPPER]${NC} $*" >&2; }

# Gate definitions: service + command mappings
declare -A GATE_SERVICES=(
    ["compile"]="erlmcp-build"
    ["unit"]="erlmcp-unit"
    ["ct"]="erlmcp-ct"
    ["check"]="erlmcp-check"
    ["dialyzer"]="erlmcp-build"
    ["xref"]="erlmcp-build"
    ["coverage"]="erlmcp-check"
    ["bench"]="erlmcp-bench"
    ["cluster"]="erlmcp-node"
)

declare -A GATE_COMMANDS=(
    ["compile"]="rebar3 compile"
    ["unit"]="rebar3 eunit"
    ["ct"]="rebar3 ct -name test@127.0.0.1 -setcookie erlmcp"
    ["check"]="rebar3 dialyzer && rebar3 xref"
    ["dialyzer"]="rebar3 dialyzer"
    ["xref"]="rebar3 xref"
    ["coverage"]="rebar3 cover"
    ["bench"]="rebar3 machi"
    ["cluster"]="epmd -daemon && erl -sname bar -setcookie erlmcp"
)

# Forbidden tokens that indicate host execution
declare -a FORBIDDEN_TOKENS=(
    "rebar3"
    "erl"
    "ct_run"
    "erlc"
    "epmd"
    "dialyzer"
    "typer"
    "make"
    "mvn"
    "gradle"
)

# Check if command contains forbidden tokens (host execution)
check_forbidden_tokens() {
    local command="$1"

    for token in "${FORBIDDEN_TOKENS[@]}"; do
        if [[ "$command" == *"$token"* ]] && [[ "$command" != *"docker"* ]]; then
            log_error "ANDON: Forbidden token detected: '$token'"
            log_error "Constitution violation: HOST EXECUTION FORBIDDEN"
            log_error "All execution MUST happen via Docker containers."
            log_error ""
            log_error "Correct usage:"
            log_error "  docker compose run --rm erlmcp-build rebar3 compile"
            log_error "  $0 compile"
            log_error "  $0 erlmcp-build 'rebar3 compile'"
            return 1
        fi
    done

    return 0
}

# Verify Docker is available
verify_docker() {
    if ! command -v docker &>/dev/null; then
        log_error "Docker not found. This is a DOCKER-ONLY system."
        return 1
    fi

    if ! docker info &>/dev/null; then
        log_error "Docker daemon not running."
        return 1
    fi

    return 0
}

# Verify docker-compose
verify_docker_compose() {
    if docker compose version &>/dev/null; then
        return 0
    elif command -v docker-compose &>/dev/null; then
        return 0
    else
        log_error "docker-compose not found."
        return 1
    fi
}

# Execute gate
execute_gate() {
    local gate="$1"
    local service="${GATE_SERVICES[$gate]:-erlmcp-build}"
    local command="${GATE_COMMANDS[$gate]:-}"

    if [[ -z "$command" ]]; then
        log_error "Unknown gate: $gate"
        log_info "Available gates: ${!GATE_SERVICES[@]}"
        return 1
    fi

    log_info "Executing gate: $gate"
    log_info "  Service: $service"
    log_info "  Command:  $command"

    # Execute with receipt generation
    "$RECEIPT_SCRIPT" generate "$service" "$command"
}

# Execute custom command
execute_custom() {
    local service="$1"
    local command="$2"
    local workdir="${3:-/app}"

    log_info "Executing custom command"
    log_info "  Service: $service"
    log_info "  Command:  $command"
    log_info "  WorkDir:  $workdir"

    # Check for forbidden tokens
    if ! check_forbidden_tokens "$command"; then
        return 1
    fi

    # Execute with receipt generation
    "$RECEIPT_SCRIPT" generate "$service" "$command" --workdir="$workdir"
}

# Show help
show_help() {
    cat <<EOF
${CYAN}ERLMCP v3: Docker-Only Execution Wrapper${NC}

${GREEN}CONSTITUTION${NC}
  DOCKER-ONLY. Host execution is FORBIDDEN.
  All commands MUST execute inside Docker containers.

${GREEN}USAGE${NC}
  $0 <gate|service> [command] [options]

${GREEN}GATES (Predefined Quality Gates)${NC}
  ${YELLOW}compile${NC}      Execute compilation (erlmcp-build)
  ${YELLOW}unit${NC}         Execute unit tests (erlmcp-unit)
  ${YELLOW}ct${NC}           Execute common tests (erlmcp-ct)
  ${YELLOW}check${NC}        Execute type checking + xref (erlmcp-check)
  ${YELLOW}dialyzer${NC}     Execute Dialyzer analysis (erlmcp-build)
  ${YELLOW}xref${NC}         Execute cross-reference check (erlmcp-build)
  ${YELLOW}coverage${NC}     Generate coverage report (erlmcp-check)
  ${YELLOW}bench${NC}        Run benchmarks (erlmcp-bench)
  ${YELLOW}cluster${NC}      Start cluster node (erlmcp-node)

${GREEN}CUSTOM EXECUTION${NC}
  $0 <service> <command> [--workdir=<dir>]
      Execute custom command in Docker service.
      service: Docker service name (erlmcp-build, erlmcp-unit, etc.)
      command: Command to execute

${GREEN}OPTIONS${NC}
  --workdir=<dir>     Working directory inside container (default: /app)
  --help, -h          Show this help

${GREEN}EXAMPLES${NC}
  # Execute compile gate
  $0 compile

  # Execute unit test gate
  $0 unit

  # Execute custom command
  $0 erlmcp-build "rebar3 compile --profile prod"

  # Execute with custom workdir
  $0 erlmcp-unit "rebar3 eunit" --workdir=/app/src

${GREEN}RECEIPTS${NC}
  All executions generate cryptographically-verifiable receipts.
  Receipts stored in: .erlmcp/receipts/
  Verify receipts: ./scripts/receipts/verify.sh

${GREEN}FORBIDDEN${NC}
  Direct execution of these commands is CONSTITUTION VIOLATION:
    rebar3, erl, ct_run, erlc, epmd, dialyzer, typer, make

  Always use docker compose or this wrapper instead.

EOF
}

main() {
    local service=""
    local command=""
    local workdir="/app"

    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --help|-h)
                show_help
                exit 0
                ;;
            --workdir=*)
                workdir="${1#*=}"
                shift
                ;;
            *)
                if [[ -z "$service" ]]; then
                    service="$1"
                elif [[ -z "$command" ]]; then
                    command="$1"
                fi
                shift
                ;;
        esac
    done

    # Require at least service/gate
    if [[ -z "$service" ]]; then
        log_error "Usage: $0 <gate|service> [command] [options]"
        echo ""
        show_help
        exit 1
    fi

    # Verify Docker environment
    if ! verify_docker; then
        exit 1
    fi

    if ! verify_docker_compose; then
        exit 1
    fi

    # Check if it's a predefined gate
    if [[ -n "${GATE_SERVICES[$service]:-}" ]]; then
        # It's a gate - no command needed
        execute_gate "$service"
    else
        # It's a custom service - need command
        if [[ -z "$command" ]]; then
            log_error "Custom service execution requires a command"
            log_error "Usage: $0 <service> <command>"
            exit 1
        fi

        execute_custom "$service" "$command" "$workdir"
    fi
}

main "$@"
