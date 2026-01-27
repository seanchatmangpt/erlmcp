#!/bin/bash
# Smoke tests for erlmcp deployments
# Usage: ./scripts/smoke_tests.sh [environment]

set -euo pipefail

# === CONFIGURATION ===
ENVIRONMENT="${1:-dev}"
BASE_URL="${2:-http://localhost:8080}"

# === COLORS ===
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# === COUNTERS ===
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# === LOGGING ===
log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[PASS]${NC} $*"; }
log_error() { echo -e "${RED}[FAIL]${NC} $*"; }

# === TEST HELPERS ===
run_test() {
    local test_name=$1
    local test_fn=$2

    ((TESTS_RUN++))
    log_info "Running: $test_name"

    if $test_fn; then
        log_success "$test_name"
        ((TESTS_PASSED++))
        return 0
    else
        log_error "$test_name"
        ((TESTS_FAILED++))
        return 1
    fi
}

# === SMOKE TESTS ===

test_health_endpoint() {
    local response
    response=$(curl -sf "${BASE_URL}/health" 2>/dev/null)

    if [ -z "$response" ]; then
        return 1
    fi

    # Check for healthy status
    echo "$response" | grep -q '"status":"healthy"' || \
    echo "$response" | grep -q '"status":"degraded"'
}

test_readiness_endpoint() {
    curl -sf "${BASE_URL}/health/ready" > /dev/null 2>&1
}

test_liveness_endpoint() {
    curl -sf "${BASE_URL}/health/live" > /dev/null 2>&1
}

test_metrics_endpoint() {
    curl -sf "${BASE_URL}/metrics" > /dev/null 2>&1
}

test_dashboard_endpoint() {
    curl -sf "http://localhost:3000" > /dev/null 2>&1
}

test_mcp_initialize() {
    local response
    response=$(curl -sf "${BASE_URL}/mcp" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' 2>/dev/null)

    [ -n "$response" ] && echo "$response" | grep -q '"jsonrpc":"2.0"'
}

test_response_time() {
    local start end duration
    start=$(date +%s%N)
    curl -sf "${BASE_URL}/health" > /dev/null 2>&1
    end=$(date +%s%N)

    duration=$(( (end - start) / 1000000 ))  # Convert to milliseconds

    # Response time should be < 1000ms
    [ "$duration" -lt 1000 ]
}

# === MAIN ===
main() {
    log_info "=== erlmcp Smoke Tests ==="
    log_info "Environment: $ENVIRONMENT"
    log_info "Base URL: $BASE_URL"
    log_info "=========================="

    # Run core tests
    run_test "Health endpoint" test_health_endpoint || true
    run_test "Readiness endpoint" test_readiness_endpoint || true
    run_test "Liveness endpoint" test_liveness_endpoint || true
    run_test "Response time" test_response_time || true

    # Optional tests (don't fail on these)
    run_test "Metrics endpoint" test_metrics_endpoint || true
    run_test "Dashboard endpoint" test_dashboard_endpoint || true
    run_test "MCP initialize" test_mcp_initialize || true

    # Summary
    echo ""
    log_info "=== Smoke Test Summary ==="
    log_info "Tests run: $TESTS_RUN"
    log_success "Tests passed: $TESTS_PASSED"
    if [ $TESTS_FAILED -gt 0 ]; then
        log_error "Tests failed: $TESTS_FAILED"
    else
        log_info "Tests failed: $TESTS_FAILED"
    fi

    # Exit with failure if any critical tests failed
    # (health endpoint is critical)
    if [ $TESTS_FAILED -gt 0 ]; then
        log_error "Some smoke tests failed"
        exit 1
    fi

    log_success "All smoke tests passed!"
    exit 0
}

main "$@"
