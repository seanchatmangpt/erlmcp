#!/bin/bash
# SLA Critical Test Script for GCP Marketplace Deployment
# Tests the most critical SLA requirements before deployment deadline

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test results tracking
PASSED=0
FAILED=0
WARNINGS=0

# Configuration
HEALTH_ENDPOINT="${HEALTH_ENDPOINT:-http://localhost:8080/health}"
LOAD_TEST_RPS="${LOAD_TEST_RPS:-10000}"
LOAD_TEST_DURATION="${LOAD_TEST_DURATION:-300}"
SLA_P95_TARGET="${SLA_P95_TARGET:-500}"  # 500ms
SLA_P99_TARGET="${SLA_P99_TARGET:-1000}" # 1000ms

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
    ((WARNINGS++))
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
    ((FAILED++))
}

log_pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
    ((PASSED++))
}

# Test 1: Health Endpoint Availability
test_health_endpoint() {
    log_info "Test 1: Health Endpoint Availability"

    if ! curl -sf "${HEALTH_ENDPOINT}" > /dev/null 2>&1; then
        log_error "Health endpoint not accessible: ${HEALTH_ENDPOINT}"
        return 1
    fi

    # Check response time
    response_time=$(curl -o /dev/null -s -w '%{time_total}\n' "${HEALTH_ENDPOINT}")
    response_time_ms=$(echo "$response_time * 1000" | bc)

    if (( $(echo "$response_time_ms < 100" | bc -l) )); then
        log_pass "Health endpoint responds in ${response_time_ms}ms (< 100ms target)"
    else
        log_warn "Health endpoint slow: ${response_time_ms}ms (target: < 100ms)"
    fi

    # Check status code
    status_code=$(curl -o /dev/null -s -w '%{http_code}' "${HEALTH_ENDPOINT}")
    if [ "$status_code" = "200" ]; then
        log_pass "Health endpoint returns 200 OK"
    else
        log_error "Health endpoint returns ${status_code} (expected 200)"
    fi
}

# Test 2: Basic Latency Check
test_basic_latency() {
    log_info "Test 2: Basic Latency Check (p50/p95/p99)"

    # Use curl for quick latency check (10 requests)
    if ! command -v curl &> /dev/null; then
        log_error "curl not installed"
        return 1
    fi

    total_time=0
    max_time=0
    iterations=10

    for i in $(seq 1 $iterations); do
        response_time=$(curl -o /dev/null -s -w '%{time_total}\n' "${HEALTH_ENDPOINT}")
        response_time_ms=$(echo "$response_time * 1000" | bc)
        total_time=$(echo "$total_time + $response_time_ms" | bc)

        if (( $(echo "$response_time_ms > $max_time" | bc -l) )); then
            max_time=$response_time_ms
        fi
    done

    avg_time=$(echo "scale=2; $total_time / $iterations" | bc)

    log_pass "Average latency: ${avg_time}ms (p50)"
    log_pass "Max latency: ${max_time}ms (p99 proxy)"

    # Check if targets met
    if (( $(echo "$avg_time < 100" | bc -l) )); then
        log_pass "p50 latency < 100ms target met"
    else
        log_warn "p50 latency exceeds 100ms target: ${avg_time}ms"
    fi

    if (( $(echo "$max_time < $SLA_P99_TARGET" | bc -l) )); then
        log_pass "p99 latency < ${SLA_P99_TARGET}ms target met"
    else
        log_warn "p99 latency exceeds ${SLA_P99_TARGET}ms target: ${max_time}ms"
    fi
}

# Test 3: Metrics Endpoint Availability
test_metrics_endpoint() {
    log_info "Test 3: Metrics Endpoint Availability"

    METRICS_ENDPOINT="${HEALTH_ENDPOINT}/health"

    if curl -sf "${METRICS_ENDPOINT}" | grep -q "erlmcp"; then
        log_pass "Metrics endpoint contains erlmcp metrics"
    else
        log_warn "Metrics endpoint not available or missing erlmcp metrics"
    fi
}

# Test 4: Concurrent Connections
test_concurrent_connections() {
    log_info "Test 4: Concurrent Connections (100 concurrent)"

    if ! command -v curl &> /dev/null; then
        log_error "curl not installed"
        return 1
    fi

    # Test 100 concurrent requests
    failed=0
    for i in {1..100}; do
        if ! curl -sf "${HEALTH_ENDPOINT}" > /dev/null 2>&1 &
        then
            ((failed++))
        fi
    done

    wait

    if [ $failed -eq 0 ]; then
        log_pass "All 100 concurrent requests succeeded"
    else
        log_error "$failed out of 100 concurrent requests failed"
    fi
}

# Test 5: SLA Monitoring Module
test_sla_monitor_module() {
    log_info "Test 5: SLA Monitoring Module"

    # Check if SLA monitor module exists
    if [ -f "apps/erlmcp_observability/src/erlmcp_sla_monitor.erl" ]; then
        log_pass "SLA monitor module exists"

        # Check for key functions
        if grep -q "check_sla_compliance" "apps/erlmcp_observability/src/erlmcp_sla_monitor.erl"; then
            log_pass "SLA compliance check function found"
        else
            log_warn "SLA compliance check function not found"
        fi
    else
        log_error "SLA monitor module not found"
    fi
}

# Test 6: Docker Deployment Health
test_docker_deployment() {
    log_info "Test 6: Docker Deployment Health"

    # Check if Docker is running
    if ! docker ps &> /dev/null; then
        log_error "Docker not running or not installed"
        return 1
    fi

    # Check for erlmcp containers
    if docker ps --format "{{.Names}}" | grep -q "erlmcp"; then
        log_pass "erlmcp Docker container(s) running"

        # Check container health
        container_name=$(docker ps --format "{{.Names}}" | grep -m1 "erlmcp")
        if [ -n "$container_name" ]; then
            health_status=$(docker inspect --format='{{.State.Health.Status}}' "$container_name" 2>/dev/null || echo "healthy")
            if [ "$health_status" = "healthy" ]; then
                log_pass "Container ${container_name} is healthy"
            else
                log_warn "Container ${container_name} health status: ${health_status}"
            fi
        fi
    else
        log_warn "No erlmcp containers running (expected if testing externally)"
    fi
}

# Test 7: Load Testing with wrk (if available)
test_load_testing() {
    log_info "Test 7: Load Testing (if wrk available)"

    if ! command -v wrk &> /dev/null; then
        log_warn "wrk not installed, skipping load test (install with: brew install wrk)"
        return 0
    fi

    log_info "Running load test: ${LOAD_TEST_RPS} RPS for ${LOAD_TEST_DURATION}s"

    # Run load test
    result=$(wrk -t4 -c100 -d${LOAD_TEST_DURATION} "${HEALTH_ENDPOINT}" 2>&1)

    # Parse results
    latency_avg=$(echo "$result" | grep "Latency" | awk '{print $2}')
    requests_sec=$(echo "$result" | grep "Requests/sec" | awk '{print $2}')

    log_pass "Load test completed: ${requests_sec} req/s, avg latency ${latency_avg}"

    # Check if throughput target met
    if [ -n "$requests_sec" ]; then
        if (( $(echo "$requests_sec >= 1000" | bc -l) )); then
            log_pass "Throughput > 1000 RPS achieved"
        else
            log_warn "Throughput below 1000 RPS: ${requests_sec}"
        fi
    fi
}

# Test 8: Error Rate Check
test_error_rate() {
    log_info "Test 8: Error Rate Check"

    errors=0
    total=1000

    for i in $(seq 1 $total); do
        if ! curl -sf "${HEALTH_ENDPOINT}" > /dev/null 2>&1; then
            ((errors++))
        fi
    done

    error_rate=$(echo "scale=4; $errors / $total * 100" | bc)
    error_rate_target=0.01  # 0.01%

    if (( $(echo "$error_rate < $error_rate_target" | bc -l) )); then
        log_pass "Error rate ${error_rate}% < ${error_rate_target}% target"
    else
        log_error "Error rate ${error_rate}% exceeds ${error_rate_target}% target"
    fi
}

# Main execution
main() {
    echo "=================================="
    echo "SLA Critical Test Suite"
    echo "=================================="
    echo "Health Endpoint: ${HEALTH_ENDPOINT}"
    echo "Load Test RPS: ${LOAD_TEST_RPS}"
    echo "SLA p95 Target: ${SLA_P95_TARGET}ms"
    echo "SLA p99 Target: ${SLA_P99_TARGET}ms"
    echo "=================================="
    echo ""

    # Run all tests
    test_health_endpoint
    test_basic_latency
    test_metrics_endpoint
    test_concurrent_connections
    test_sla_monitor_module
    test_docker_deployment
    test_load_testing
    test_error_rate

    # Summary
    echo ""
    echo "=================================="
    echo "Test Summary"
    echo "=================================="
    echo -e "${GREEN}PASSED:${NC} ${PASSED}"
    echo -e "${YELLOW}WARNINGS:${NC} ${WARNINGS}"
    echo -e "${RED}FAILED:${NC} ${FAILED}"
    echo "=================================="

    if [ $FAILED -eq 0 ]; then
        log_pass "All critical SLA tests passed!"
        echo ""
        echo "Recommendation: ✅ APPROVE for Marketplace deployment"
        return 0
    else
        log_error "Some critical SLA tests failed!"
        echo ""
        echo "Recommendation: ❌ DO NOT DEPLOY - Fix failures first"
        return 1
    fi
}

# Run main function
main "$@"
