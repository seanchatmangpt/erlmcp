#!/usr/bin/env bash
#
# validate-quality-lanes.sh - Validate Docker Compose quality lane configuration
#
# This script validates that all quality lane services are properly configured
# and can be used to run their respective make targets.

set -euo pipefail

# Colors
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

# Docker Compose file
readonly COMPOSE_FILE="${PROJECT_ROOT}/docker-compose.yml"

# Quality lanes to validate
readonly QUALITY_LANES=(
    "erlmcp-build:build"
    "erlmcp-unit:unit"
    "erlmcp-ct:ct"
    "erlmcp-check:check"
    "erlmcp-bench:bench"
    "erlmcp-node:cluster"
)

# Expected profiles
readonly EXPECTED_PROFILES=(
    "build"
    "unit"
    "ct"
    "check"
    "bench"
    "cluster"
)

# Expected volumes
readonly EXPECTED_VOLUMES=(
    "erlmcp-build-cache"
    "erlmcp-unit-logs"
    "erlmcp-unit-cover"
    "erlmcp-ct-logs"
    "erlmcp-ct-cover"
    "erlmcp-dialyzer-plt"
    "erlmcp-dialyzer-plt-test"
    "erlmcp-check-cover"
    "erlmcp-check-reports"
    "erlmcp-bench-results"
    "erlmcp-bench-baselines"
    "erlmcp-cluster-data"
    "erlmcp-cluster-logs"
)

# Logging
log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[PASS]${NC} $*"; }
log_warning() { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error() { echo -e "${RED}[FAIL]${NC} $*"; }
log_header() {
    echo ""
    echo -e "${BOLD}${CYAN}════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${CYAN}$*${NC}"
    echo -e "${BOLD}${CYAN}════════════════════════════════════════════════════════════${NC}"
    echo ""
}

# Check if Docker is available
check_docker() {
    log_header "Checking Docker Availability"

    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed"
        return 1
    fi
    log_success "Docker is installed: $(docker --version | head -1)"

    if ! docker info &> /dev/null; then
        log_error "Docker daemon is not running"
        return 1
    fi
    log_success "Docker daemon is running"

    if ! docker compose version &> /dev/null && ! command -v docker-compose &> /dev/null; then
        log_error "Docker Compose is not available"
        return 1
    fi
    log_success "Docker Compose is available"

    return 0
}

# Validate docker-compose.yml syntax
validate_compose_syntax() {
    log_header "Validating docker-compose.yml Syntax"

    local compose_cmd
    if command -v docker-compose &> /dev/null; then
        compose_cmd="docker-compose -f ${COMPOSE_FILE}"
    else
        compose_cmd="docker compose -f ${COMPOSE_FILE}"
    fi

    if ${compose_cmd} config --quiet 2>&1 | grep -q "error"; then
        log_error "docker-compose.yml has syntax errors"
        ${compose_cmd} config 2>&1 | head -20
        return 1
    fi

    log_success "docker-compose.yml syntax is valid"
    return 0
}

# Validate quality lane services exist
validate_lane_services() {
    log_header "Validating Quality Lane Services"

    local compose_cmd
    if command -v docker-compose &> /dev/null; then
        compose_cmd="docker-compose -f ${COMPOSE_FILE}"
    else
        compose_cmd="docker compose -f ${COMPOSE_FILE}"
    fi

    local all_passed=0

    for lane_entry in "${QUALITY_LANES[@]}"; do
        local service="${lane_entry%%:*}"
        local profile="${lane_entry##*:}"

        # Check if service exists using grep on the raw YAML file
        if grep -q "^  ${service}:" "${COMPOSE_FILE}"; then
            log_success "Service exists: ${service}"

            # Check if profile is set correctly using grep on the YAML file
            local service_profile
            service_profile=$(sed -n "/^  ${service}:/,/^  [a-z]/p" "${COMPOSE_FILE}" | grep "profiles:" -A 1 | grep "\- ${profile}" || echo "")
            if [[ -n "${service_profile}" ]]; then
                log_success "  - Profile: ${profile}"
            else
                log_warning "  - Profile '${profile}' not found in service definition"
            fi

            # Check if image is defined
            local service_image
            service_image=$(sed -n "/^  ${service}:/,/^  [a-z]/p" "${COMPOSE_FILE}" | grep "image:" | head -1 || echo "")
            if [[ -n "${service_image}" ]]; then
                log_success "  - Image: $(echo "${service_image}" | awk '{print $2}')"
            else
                log_warning "  - No image defined (will build from Dockerfile)"
            fi

            # Check for entrypoint
            local service_entrypoint
            service_entrypoint=$(sed -n "/^  ${service}:/,/^  [a-z]/p" "${COMPOSE_FILE}" | grep "entrypoint:" | head -1 || echo "")
            if [[ -n "${service_entrypoint}" ]]; then
                log_success "  - Entrypoint: defined"
            else
                log_warning "  - No custom entrypoint"
            fi

            # Check resource limits
            local has_cpu_limit
            local has_memory_limit
            has_cpu_limit=$(sed -n "/^  ${service}:/,/^  [a-z]/p" "${COMPOSE_FILE}" | grep "cpus:" | head -1 || echo "")
            has_memory_limit=$(sed -n "/^  ${service}:/,/^  [a-z]/p" "${COMPOSE_FILE}" | grep "memory:" | head -1 || echo "")

            if [[ -n "${has_cpu_limit}" ]]; then
                log_success "  - CPU limit: defined"
            else
                log_warning "  - No CPU limit defined"
            fi

            if [[ -n "${has_memory_limit}" ]]; then
                log_success "  - Memory limit: defined"
            else
                log_warning "  - No memory limit defined"
            fi

        else
            log_error "Service not found: ${service}"
            all_passed=1
        fi
    done

    return ${all_passed}
}

# Validate volumes exist
validate_volumes() {
    log_header "Validating Quality Lane Volumes"

    local all_passed=0

    for volume in "${EXPECTED_VOLUMES[@]}"; do
        if grep -q "^  ${volume}:" "${COMPOSE_FILE}"; then
            log_success "Volume exists: ${volume}"
        else
            log_error "Volume not found: ${volume}"
            all_passed=1
        fi
    done

    return ${all_passed}
}

# Validate network configuration
validate_networks() {
    log_header "Validating Network Configuration"

    if grep -q "^networks:" "${COMPOSE_FILE}"; then
        log_success "Networks section exists"

        # Check network configuration using sed on the YAML file
        local network_config
        network_config=$(sed -n '/^networks:/,/^volumes:/p' "${COMPOSE_FILE}" | sed -n '/^  erlmcp-network:/,/^$/p' || echo "")
        if echo "${network_config}" | grep -q "driver: bridge"; then
            log_success "  - Driver: bridge"
        fi
        if echo "${network_config}" | grep -q "subnet:"; then
            log_success "  - Subnet: $(echo "${network_config}" | grep "subnet:" | awk '{print $2}')"
        fi
    else
        log_error "Networks section not found"
        return 1
    fi

    return 0
}

# Generate summary report
generate_summary() {
    log_header "Quality Lane Validation Summary"

    echo "Quality Lane Configuration:"
    echo "  File: ${COMPOSE_FILE}"
    echo ""
    echo "Quality Lanes:"

    for lane_entry in "${QUALITY_LANES[@]}"; do
        local service="${lane_entry%%:*}"
        local profile="${lane_entry##*:}"
        echo "  - ${service} (profile: ${profile})"
    done

    echo ""
    echo "Volumes: ${#EXPECTED_VOLUMES[@]} defined"
    echo "Network: erlmcp-network (bridge)"
    echo ""
}

# Main validation
main() {
    log_header "erlmcp Quality Lane Validation"
    echo "Constitution: DOCKER-ONLY"
    echo "Validating Docker Compose quality lane configuration..."
    echo ""

    local exit_code=0

    # Run validations
    check_docker || exit_code=1
    validate_compose_syntax || exit_code=1
    validate_lane_services || exit_code=1
    validate_volumes || exit_code=1
    validate_networks || exit_code=1

    # Generate summary
    generate_summary

    # Final result
    if [[ ${exit_code} -eq 0 ]]; then
        log_header "Validation PASSED"
        log_success "All quality lanes are properly configured"
        echo ""
        echo "Usage:"
        echo "  docker compose run --rm erlmcp-build make compile"
        echo "  docker compose run --rm erlmcp-unit make eunit"
        echo "  docker compose run --rm erlmcp-ct make ct"
        echo "  docker compose run --rm erlmcp-check make check"
        echo "  docker compose run --rm erlmcp-bench make benchmark"
        echo ""
        echo "Or use the quality lane runner:"
        echo "  ./scripts/quality-lane-run.sh build compile"
        echo "  ./scripts/quality-lane-run.sh unit eunit"
        echo "  ./scripts/quality-lane-run.sh ct ct"
        echo "  ./scripts/quality-lane-run.sh check check"
        echo "  ./scripts/quality-lane-run.sh bench benchmark"
        echo ""
        return 0
    else
        log_header "Validation FAILED"
        log_error "Some quality lanes have configuration issues"
        return 1
    fi
}

# Run main
main "$@"
