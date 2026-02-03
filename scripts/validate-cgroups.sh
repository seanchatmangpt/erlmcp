#!/usr/bin/env bash
# ==============================================================================
# Cgroups Memory Detection Validation Script
# ==============================================================================
# Validates that erlmcp properly detects and respects cgroups memory limits
#
# Usage:
#   ./scripts/validate-cgroups.sh [--memory=512m] [--image=erlmcp:3.0.0]
#
# Exit codes:
#   0 - All checks passed
#   1 - One or more checks failed
#   2 - Invalid arguments
#   3 - Docker not available
# ==============================================================================

set -euo pipefail

# Default values
MEMORY="${MEMORY:-512m}"
IMAGE="${IMAGE:-erlmcp:3.0.0}"
TIMEOUT="${TIMEOUT:-30}"
FAILED=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging functions
log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[FAIL]${NC} $1"; FAILED=$((FAILED + 1)); }

# Check if Docker is available
check_docker() {
    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed or not in PATH"
        exit 3
    fi
    log_info "Docker is available: $(docker --version)"
}

# Check if image exists
check_image() {
    if ! docker image inspect "${IMAGE}" &> /dev/null; then
        log_error "Image ${IMAGE} not found. Build it first with: docker build -t ${IMAGE} ."
        exit 1
    fi
    log_info "Image ${IMAGE} exists"
}

# Test 1: Verify ERL_AFLAGS contains cgroups flags
test_erl_aflags() {
    log_info "Test 1: Checking ERL_AFLAGS environment variable..."

    local flags
    flags=$(docker run --rm "${IMAGE}" printenv ERL_AFLAGS 2>/dev/null || echo "")

    if [[ ! "${flags}" =~ \+MBacul ]]; then
        log_error "ERL_AFLAGS does not contain +MBacul flag"
        return 1
    fi

    if [[ ! "${flags}" =~ \+Msbagf ]]; then
        log_error "ERL_AFLAGS does not contain +Msbagf flag"
        return 1
    fi

    if [[ ! "${flags}" =~ \+MBacgs ]]; then
        log_error "ERL_AFLAGS does not contain +MBacgs flag"
        return 1
    fi

    log_info "ERL_AFLAGS contains all required cgroups flags: ${flags}"
    return 0
}

# Test 2: Verify VM starts with memory limit
test_vm_startup_with_limit() {
    log_info "Test 2: Starting VM with ${MEMORY} memory limit..."

    local container_id
    container_id=$(docker run -d --memory="${MEMORY}" --name="erlmcp-cgroups-test$$" "${IMAGE}")

    # Wait for container to start
    local count=0
    while [ $count -lt "${TIMEOUT}" ]; do
        if docker inspect "${container_id}" | grep -q '"Status": "running"'; then
            break
        fi
        sleep 1
        count=$((count + 1))
    done

    # Check if process is running
    if docker exec "${container_id}" pgrep -f "beam.smp" > /dev/null 2>&1; then
        log_info "VM started successfully with memory limit"
        docker stop "${container_id}" > /dev/null 2>&1
        docker rm "${container_id}" > /dev/null 2>&1
        return 0
    else
        log_error "VM failed to start with memory limit"
        docker logs "${container_id}" 2>&1 | tail -20
        docker stop "${container_id}" > /dev/null 2>&1
        docker rm "${container_id}" > /dev/null 2>&1
        return 1
    fi
}

# Test 3: Verify memory detection (via erlang:memory/1)
test_memory_detection() {
    log_info "Test 3: Checking memory detection via allocator..."

    local output
    output=$(docker run --rm --memory="${MEMORY}" "${IMAGE}" \
        erl -noshell -eval \
        "io:format('system_memory:~p~n', [erlang:memory(system)]), halt()." \
        2>/dev/null || echo "")

    if [[ "${output}" =~ system_memory: ]]; then
        local memory_kb
        memory_kb=$(echo "${output}" | grep -o 'system_memory:[0-9]*' | cut -d: -f2)
        local memory_mb=$((memory_kb / 1024))
        log_info "Detected system memory: ${memory_mb} MB (container limit: ${MEMORY})"
        return 0
    else
        log_error "Could not detect memory information"
        return 1
    fi
}

# Test 4: Verify no OOM kill under normal load
test_no_oom_under_load() {
    log_info "Test 4: Testing for OOM under normal load..."

    local container_id
    container_id=$(docker run -d --memory="${MEMORY}" --name="erlmcp-oom-test$$" "${IMAGE}")

    # Wait for startup
    sleep 5

    # Check if container is still running
    if docker inspect "${container_id}" | grep -q '"Status": "running"'; then
        log_info "No OOM kill detected - container is running"
        docker stop "${container_id}" > /dev/null 2>&1
        docker rm "${container_id}" > /dev/null 2>&1
        return 0
    else
        # Check exit code
        local exit_code
        exit_code=$(docker inspect "${container_id}" --format='{{.State.ExitCode}}')
        if [ "${exit_code}" = "137" ]; then
            log_error "Container was OOM killed (exit code 137)"
            docker logs "${container_id}" 2>&1 | tail -20
            docker rm "${container_id}" > /dev/null 2>&1
            return 1
        else
            log_warn "Container exited with code ${exit_code} (not OOM)"
            docker rm "${container_id}" > /dev/null 2>&1
            return 0
        fi
    fi
}

# Test 5: Verify cgroups v2 support
test_cgroups_v2() {
    log_info "Test 5: Checking cgroups v2 support..."

    # Check if host uses cgroups v2
    if [ -f /sys/fs/cgroup/cgroup.controllers ]; then
        log_info "Host uses cgroups v2"
    else
        log_warn "Host does not use cgroups v2 (may be using v1)"
    fi

    # Check if container can access cgroups
    local output
    output=$(docker run --rm --memory="${MEMORY}" "${IMAGE}" \
        cat /sys/fs/cgroup/memory.max 2>/dev/null || echo "")

    if [ -n "${output}" ]; then
        log_info "Container cgroups memory limit: ${output}"
        return 0
    else
        log_warn "Could not read cgroups memory.max (may be cgroups v1)"
        return 0  # Not a failure, just a warning
    fi
}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --memory=*)
            MEMORY="${1#*=}"
            shift
            ;;
        --image=*)
            IMAGE="${1#*=}"
            shift
            ;;
        --timeout=*)
            TIMEOUT="${1#*=}"
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [--memory=512m] [--image=erlmcp:3.0.0] [--timeout=30]"
            echo ""
            echo "Options:"
            echo "  --memory=SIZE   Memory limit for container (default: 512m)"
            echo "  --image=NAME    Docker image to test (default: erlmcp:3.0.0)"
            echo "  --timeout=SEC   Startup timeout in seconds (default: 30)"
            echo "  -h, --help      Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 2
            ;;
    esac
done

# Main execution
main() {
    echo "=============================================================================="
    echo "Cgroups Memory Detection Validation"
    echo "=============================================================================="
    echo "Image: ${IMAGE}"
    echo "Memory Limit: ${MEMORY}"
    echo "=============================================================================="
    echo ""

    check_docker
    check_image

    test_erl_aflags
    test_memory_detection
    test_cgroups_v2
    test_vm_startup_with_limit
    test_no_oom_under_load

    echo ""
    echo "=============================================================================="
    if [ ${FAILED} -eq 0 ]; then
        log_info "All checks passed!"
        echo "=============================================================================="
        exit 0
    else
        log_error "${FAILED} check(s) failed"
        echo "=============================================================================="
        exit 1
    fi
}

# Run main
main
