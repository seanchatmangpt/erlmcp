#!/usr/bin/env bash
#
# TCPS Staging Smoke Tests
# Performs basic health checks and functional validation
#

set -euo pipefail

# Configuration
ENV="${1:-staging}"
BASE_URL="${2:-http://localhost:8080}"
DASHBOARD_URL="${3:-http://localhost:3000}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Logging
log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

# Test utilities
run_test() {
    local test_name="$1"
    shift
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    echo ""
    echo "Running: $test_name"

    if "$@"; then
        echo -e "${GREEN}✅ PASS${NC}: $test_name"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        echo -e "${RED}❌ FAIL${NC}: $test_name"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

# Test functions
test_health_endpoint() {
    local response
    response=$(curl -s -w "\n%{http_code}" "$BASE_URL/health" 2>/dev/null || echo "000")
    local status_code=$(echo "$response" | tail -1)

    if [ "$status_code" = "200" ]; then
        echo "Health endpoint returned 200 OK"
        return 0
    else
        echo "Health endpoint returned $status_code (expected 200)"
        return 1
    fi
}

test_readiness_endpoint() {
    local response
    response=$(curl -s -w "\n%{http_code}" "$BASE_URL/health/ready" 2>/dev/null || echo "000")
    local status_code=$(echo "$response" | tail -1)

    if [ "$status_code" = "200" ]; then
        echo "Readiness endpoint returned 200 OK"
        return 0
    else
        echo "Readiness endpoint returned $status_code (expected 200)"
        return 1
    fi
}

test_liveness_endpoint() {
    local response
    response=$(curl -s -w "\n%{http_code}" "$BASE_URL/health/live" 2>/dev/null || echo "000")
    local status_code=$(echo "$response" | tail -1)

    if [ "$status_code" = "200" ]; then
        echo "Liveness endpoint returned 200 OK"
        return 0
    else
        echo "Liveness endpoint returned $status_code (expected 200)"
        return 1
    fi
}

test_response_time() {
    local start end duration
    start=$(date +%s%3N)
    curl -s "$BASE_URL/health" > /dev/null 2>&1
    end=$(date +%s%3N)
    duration=$((end - start))

    if [ "$duration" -lt 1000 ]; then
        echo "Response time: ${duration}ms (target: <1000ms)"
        return 0
    else
        echo "Response time: ${duration}ms (exceeds 1000ms threshold)"
        return 1
    fi
}

test_metrics_endpoint() {
    local response
    response=$(curl -s -w "\n%{http_code}" "$BASE_URL/metrics" 2>/dev/null || echo "000")
    local status_code=$(echo "$response" | tail -1)

    if [ "$status_code" = "200" ]; then
        echo "Metrics endpoint returned 200 OK"
        return 0
    else
        echo "Metrics endpoint returned $status_code (expected 200)"
        return 1
    fi
}

test_dashboard_accessible() {
    local response
    response=$(curl -s -w "\n%{http_code}" "$DASHBOARD_URL" 2>/dev/null || echo "000")
    local status_code=$(echo "$response" | tail -1)

    if [ "$status_code" = "200" ]; then
        echo "Dashboard accessible at $DASHBOARD_URL"
        return 0
    else
        echo "Dashboard returned $status_code (expected 200)"
        return 1
    fi
}

test_docker_services() {
    local running_count
    running_count=$(docker compose ps --filter "status=running" 2>/dev/null | grep -c "Up" || echo "0")

    if [ "$running_count" -ge 5 ]; then
        echo "Docker services running: $running_count/7"
        return 0
    else
        echo "Only $running_count services running (expected 7)"
        return 1
    fi
}

# Main execution
main() {
    echo "=========================================="
    echo "TCPS Smoke Tests - $ENV Environment"
    echo "=========================================="
    echo "Base URL: $BASE_URL"
    echo "Dashboard URL: $DASHBOARD_URL"
    echo ""

    # Run tests
    run_test "Health endpoint" test_health_endpoint || true
    run_test "Readiness endpoint" test_readiness_endpoint || true
    run_test "Liveness endpoint" test_liveness_endpoint || true
    run_test "Response time" test_response_time || true
    run_test "Metrics endpoint" test_metrics_endpoint || true
    run_test "Dashboard accessible" test_dashboard_accessible || true
    run_test "Docker services" test_docker_services || true

    # Summary
    echo ""
    echo "=========================================="
    echo "Test Summary"
    echo "=========================================="
    echo "Total:  $TOTAL_TESTS"
    echo -e "${GREEN}Passed: $PASSED_TESTS${NC}"
    [ "$FAILED_TESTS" -gt 0 ] && echo -e "${RED}Failed: $FAILED_TESTS${NC}"

    if [ "$FAILED_TESTS" -eq 0 ]; then
        echo ""
        log_info "All smoke tests passed! ✅"
        exit 0
    else
        echo ""
        log_error "Some tests failed ❌"
        exit 1
    fi
}

main "$@"
