#!/usr/bin/env bash
# ==============================================================================
# erlmcp Integration Test Runner
# ==============================================================================
# Runs the full integration test suite via Docker Compose.
# Tests end-to-end functionality with PostgreSQL and Redis.
#
# Usage:
#   ./scripts/integration/run-integration-tests.sh [OPTIONS]
#
# Options:
#   --quick       Run quick integration tests (no external services)
#   --full        Run full integration test suite (default)
#   --coverage    Generate coverage report
#   --keep        Keep test containers running
#   --verbose     Verbose output
#   --help        Show this help message
#
# Quality Gate: erlmcp-integration
# ==============================================================================

set -euo pipefail

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly BOLD='\033[1m'
readonly NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Configuration
COMPOSE_FILE="${PROJECT_ROOT}/test/integration/docker-compose.test.yml"
COMPOSE_PROJECT_NAME="erlmcp-integration"
CT_LOG_DIR="${PROJECT_ROOT}/log/ct"
CT_COVER_DIR="${PROJECT_ROOT}/_build/test/cover"

# Options
QUICK=false
FULL=true
COVERAGE=false
KEEP=false
VERBOSE=false

# ==============================================================================
# Helper Functions
# ==============================================================================

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

print_banner() {
    echo ""
    echo -e "${BOLD}${CYAN}════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${CYAN}🧪 erlmcp Integration Test Suite${NC}"
    echo -e "${BOLD}${CYAN}════════════════════════════════════════════════════════════${NC}"
    echo ""
}

print_help() {
    cat << EOF
Usage: $0 [OPTIONS]

Run the erlmcp integration test suite via Docker Compose.

Options:
  --quick       Run quick integration tests (no external services)
  --full        Run full integration test suite with PostgreSQL and Redis (default)
  --coverage    Generate coverage report
  --keep        Keep test containers running after tests
  --verbose     Verbose output
  --help        Show this help message

Environment Variables:
  TEST_DB_PORT         PostgreSQL port (default: 5433)
  TEST_REDIS_PORT      Redis port (default: 6380)
  TEST_DB_NAME         Database name (default: erlmcp_test)
  TEST_DB_USER         Database user (default: erlmcp_test)
  TEST_DB_PASSWORD     Database password (default: erlmcp_test_password)

Examples:
  $0                          # Run full integration tests
  $0 --quick                  # Run quick tests
  $0 --coverage --keep        # Run with coverage and keep containers

EOF
}

# ==============================================================================
# Argument Parsing
# ==============================================================================

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --quick)
                QUICK=true
                FULL=false
                shift
                ;;
            --full)
                FULL=true
                QUICK=false
                shift
                ;;
            --coverage)
                COVERAGE=true
                shift
                ;;
            --keep)
                KEEP=true
                shift
                ;;
            --verbose)
                VERBOSE=true
                shift
                ;;
            --help|-h)
                print_help
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                print_help
                exit 1
                ;;
        esac
    done
}

# ==============================================================================
# Docker Compose Commands
# ==============================================================================

docker_compose() {
    local cmd=(docker compose -f "${COMPOSE_FILE}" -p "${COMPOSE_PROJECT_NAME}")
    if [[ "${VERBOSE}" == "true" ]]; then
        cmd+=(--verbose)
    fi
    "${cmd[@]}" "$@"
}

# ==============================================================================
# Test Execution
# ==============================================================================

setup_directories() {
    log_info "Creating log directories..."
    mkdir -p "${CT_LOG_DIR}"
    mkdir -p "${CT_COVER_DIR}"
}

start_services() {
    log_info "Starting test services..."

    # Start postgres and redis
    docker_compose up -d postgres redis

    # Wait for services to be healthy
    log_info "Waiting for services to be ready..."
    local max_wait=30
    local waited=0

    while [[ $waited -lt $max_wait ]]; do
        local pg_healthy
        local redis_healthy

        pg_healthy=$(docker_compose ps -q postgres | xargs -r docker inspect --format='{{.State.Health.Status}}' 2>/dev/null || echo "starting")
        redis_healthy=$(docker_compose ps -q redis | xargs -r docker inspect --format='{{.State.Health.Status}}' 2>/dev/null || echo "starting")

        if [[ "${pg_healthy}" == "healthy" && "${redis_healthy}" == "healthy" ]]; then
            log_success "All services are healthy"
            return 0
        fi

        sleep 1
        ((waited++))
    done

    log_error "Services failed to become healthy within ${max_wait}s"
    return 1
}

run_integration_tests() {
    log_info "Running integration test suite..."

    local ct_args=()
    if [[ "${COVERAGE}" == "true" ]]; then
        ct_args+=(--cover)
    fi

    if [[ "${VERBOSE}" == "true" ]]; then
        ct_args+=(-v)
    fi

    # Run tests
    if docker_compose run --rm erlmcp-integration \
        rebar3 ct --dir=test/integration "${ct_args[@]}"; then
        log_success "Integration tests passed"
        return 0
    else
        log_error "Integration tests failed"
        return 1
    fi
}

generate_coverage() {
    if [[ "${COVERAGE}" == "true" ]]; then
        log_info "Generating coverage report..."
        docker_compose run --rm erlmcp-integration \
            rebar3 cover || true
        log_success "Coverage report generated: ${CT_COVER_DIR}/index.html"
    fi
}

cleanup() {
    if [[ "${KEEP}" == "false" ]]; then
        log_info "Stopping test services..."
        docker_compose down -v --remove-orphans
    else
        log_info "Keeping test containers running (use --keep to keep them)"
        log_info "To stop: docker compose -f ${COMPOSE_FILE} down"
    fi
}

# ==============================================================================
# Main Execution
# ==============================================================================

main() {
    parse_args "$@"
    print_banner

    # Check prerequisites
    if ! command -v docker &> /dev/null; then
        log_error "docker is not installed or not in PATH"
        exit 1
    fi

    if ! docker compose version &> /dev/null; then
        log_error "docker compose plugin is not installed"
        exit 1
    fi

    # Check if compose file exists
    if [[ ! -f "${COMPOSE_FILE}" ]]; then
        log_error "Docker compose file not found: ${COMPOSE_FILE}"
        exit 1
    fi

    setup_directories

    # Run full integration tests with services
    if [[ "${FULL}" == "true" ]]; then
        log_info "Running full integration test suite with PostgreSQL and Redis..."
        start_services || exit 1
        run_integration_tests || { cleanup; exit 1; }
        generate_coverage
        cleanup
    fi

    # Run quick tests without external services
    if [[ "${QUICK}" == "true" ]]; then
        log_info "Running quick integration tests..."
        # Run tests directly via rebar3 (in Docker if configured)
        cd "${PROJECT_ROOT}"
        if rebar3 ct --dir=test/integration; then
            log_success "Quick integration tests passed"
        else
            log_error "Quick integration tests failed"
            exit 1
        fi
    fi

    echo ""
    echo -e "${BOLD}${GREEN}════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${GREEN}✅ Integration Test Suite Complete${NC}"
    echo -e "${BOLD}${GREEN}════════════════════════════════════════════════════════════${NC}"
    echo ""
    log_info "Test logs: ${CT_LOG_DIR}"
    if [[ "${COVERAGE}" == "true" ]]; then
        log_info "Coverage report: ${CT_COVER_DIR}/index.html"
    fi
    echo ""
}

# Run main
main "$@"
