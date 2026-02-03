#!/usr/bin/env bash
#
# quality-lane-run.sh - Run erlmcp quality lanes via Docker Compose
#
# Usage: ./scripts/quality-lane-run.sh <lane> [target]
#
# Quality Lanes:
#   build    - Compilation gate (compile, dialyzer, xref)
#   unit     - EUnit test gate
#   ct       - Common Test gate
#   check    - Quality analysis gate (dialyzer, xref, coverage)
#   bench    - Performance gate
#   cluster  - Cluster testing gate
#
# Examples:
#   ./scripts/quality-lane-run.sh build compile
#   ./scripts/quality-lane-run.sh unit eunit
#   ./scripts/quality-lane-run.sh ct ct
#   ./scripts/quality-lane-run.sh check dialyzer
#   ./scripts/quality-lane-run.sh bench benchmark
#
# Constitution: DOCKER-ONLY - All execution must run via Docker

set -euo pipefail

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly BOLD='\033[1m'
readonly NC='\033[0m'

# Script directory
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Docker Compose configuration
readonly COMPOSE_FILE="${PROJECT_ROOT}/docker-compose.yml"
readonly COMPOSE_PROJECT="erlmcp"

# Lane service mapping
declare -A LANE_SERVICES=(
    ["build"]="erlmcp-build"
    ["unit"]="erlmcp-unit"
    ["ct"]="erlmcp-ct"
    ["check"]="erlmcp-check"
    ["bench"]="erlmcp-bench"
    ["cluster"]="erlmcp-node"
    ["runtime"]="erlmcp"
    ["dev"]="erlmcp-dev"
)

# Lane profiles
declare -A LANE_PROFILES=(
    ["build"]="build"
    ["unit"]="unit"
    ["ct"]="ct"
    ["check"]="check"
    ["bench"]="bench"
    ["cluster"]="cluster"
    ["runtime"]="runtime"
    ["dev"]="dev"
)

# Default targets for each lane
declare -A LANE_DEFAULT_TARGETS=(
    ["build"]="compile"
    ["unit"]="eunit"
    ["ct"]="ct"
    ["check"]="check"
    ["bench"]="benchmark"
    ["cluster"]="test-cluster"
)

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

log_header() {
    echo ""
    echo -e "${BOLD}${CYAN}════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${CYAN}$*${NC}"
    echo -e "${BOLD}${CYAN}════════════════════════════════════════════════════════════${NC}"
    echo ""
}

# Show usage
usage() {
    cat <<EOF
${BOLD}erlmcp Quality Lane Runner${NC}

${BOLD}Usage:${NC}
    $0 <lane> [target]

${BOLD}Quality Lanes:${NC}
    build    - Compilation gate (compile, dialyzer, xref)
    unit     - EUnit test gate
    ct       - Common Test gate
    check    - Quality analysis gate (dialyzer, xref, coverage)
    bench    - Performance gate
    cluster  - Cluster testing gate

${BOLD}Arguments:${NC}
    lane     - Quality lane to run (required)
    target   - Make target to run (optional, defaults to lane's default target)

${BOLD}Examples:${NC}
    $0 build compile          # Compile all apps
    $0 unit eunit             # Run EUnit tests
    $0 ct ct                  # Run Common Test
    $0 check dialyzer         # Run dialyzer
    $0 check coverage         # Generate coverage report
    $0 bench benchmark        # Run benchmarks

${BOLD}Environment Variables:${NC}
    ERLMCP_BUILD_CPU_LIMIT     - CPU limit for build lane (default: 2.0)
    ERLMCP_BUILD_MEMORY_LIMIT  - Memory limit for build lane (default: 2G)
    ERLMCP_UNIT_CPU_LIMIT      - CPU limit for unit lane (default: 1.0)
    ERLMCP_UNIT_MEMORY_LIMIT   - Memory limit for unit lane (default: 1G)
    ERLMCP_CT_CPU_LIMIT        - CPU limit for CT lane (default: 2.0)
    ERLMCP_CT_MEMORY_LIMIT     - Memory limit for CT lane (default: 2G)
    ERLMCP_CHECK_CPU_LIMIT     - CPU limit for check lane (default: 4.0)
    ERLMCP_CHECK_MEMORY_LIMIT  - Memory limit for check lane (default: 4G)
    ERLMCP_BENCH_CPU_LIMIT     - CPU limit for bench lane (default: 2.0)
    ERLMCP_BENCH_MEMORY_LIMIT  - Memory limit for bench lane (default: 2G)

${BOLD}Constitution:${NC}
    DOCKER-ONLY - All execution must run via Docker Compose

EOF
    exit 1
}

# Validate Docker is available
check_docker() {
    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed or not in PATH"
        exit 1
    fi

    if ! docker info &> /dev/null; then
        log_error "Docker daemon is not running"
        exit 1
    fi

    if ! command -v docker compose &> /dev/null && ! docker compose version &> /dev/null; then
        log_error "Docker Compose is not available"
        exit 1
    fi
}

# Validate lane
validate_lane() {
    local lane="$1"

    if [[ -z "${LANE_SERVICES[$lane]:-}" ]]; then
        log_error "Unknown quality lane: ${lane}"
        echo ""
        echo "Available lanes: ${!LANE_SERVICES[@]}"
        exit 1
    fi
}

# Get compose command
get_compose_cmd() {
    if command -v docker-compose &> /dev/null; then
        echo "docker-compose -f ${COMPOSE_FILE}"
    else
        echo "docker compose -f ${COMPOSE_FILE}"
    fi
}

# Run quality lane
run_lane() {
    local lane="$1"
    local target="${2:-${LANE_DEFAULT_TARGETS[$lane]}}"
    local service="${LANE_SERVICES[$lane]}"
    local profile="${LANE_PROFILES[$lane]}"
    local compose_cmd

    compose_cmd="$(get_compose_cmd)"

    log_header "Quality Lane: ${lane} (Service: ${service})"

    # Check if we need to build the image first
    if ! docker image inspect "erlmcp:3.0.0-${lane}" &> /dev/null; then
        log_info "Building Docker image for ${lane} lane..."
        ${compose_cmd} --profile "${profile}" build "${service}" || {
            log_error "Failed to build Docker image for ${lane} lane"
            exit 1
        }
        log_success "Docker image built successfully"
    fi

    # Run the make target in the container
    log_info "Running: make ${target}"
    echo ""

    local start_time
    local end_time
    local duration

    start_time=$(date +%s)

    # Run with the MAKE_TARGET environment variable
    if MAKE_TARGET="${target}" ${compose_cmd} run --rm \
        -e MAKE_TARGET="${target}" \
        "${service}"; then
        end_time=$(date +%s)
        duration=$((end_time - start_time))

        echo ""
        log_header "Lane Complete: ${lane}"
        log_success "Target: ${target}"
        log_success "Duration: ${duration}s"
        echo ""

        return 0
    else
        end_time=$(date +%s)
        duration=$((end_time - start_time))

        echo ""
        log_header "Lane Failed: ${lane}"
        log_error "Target: ${target}"
        log_error "Duration: ${duration}s"
        echo ""

        return 1
    fi
}

# Main
main() {
    local lane="$1"
    local target="${2:-}"

    # Check Docker
    check_docker

    # Validate lane
    validate_lane "${lane}"

    # Change to project root
    cd "${PROJECT_ROOT}"

    # Run the lane
    if run_lane "${lane}" "${target}"; then
        exit 0
    else
        exit 1
    fi
}

# Parse arguments
if [[ $# -lt 1 ]]; then
    usage
fi

# Handle help
if [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
    usage
fi

# Run main
main "$@"
