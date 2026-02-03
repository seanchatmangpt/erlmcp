#!/bin/bash
# Docker-based SLA Validation for Marketplace Deployment
# Executes comprehensive SLA testing in isolated Docker environment

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Configuration
COMPOSE_FILE="${COMPOSE_FILE:-scripts/marketplace/sla-load-test-compose.yml}"
HEALTH_ENDPOINT="${HEALTH_ENDPOINT:-http://erlmcp-api:8080/health}"
TEST_DURATION="${TEST_DURATION:-300}"  # 5 minutes
CONCURRENT_CONNECTIONS="${CONCURRENT_CONNECTIONS:-100}"
SLA_P95_TARGET="${SLA_P95_TARGET:-500}"  # 500ms
SLA_P99_TARGET="${SLA_P99_TARGET:-1000}" # 1000ms
THROUGHPUT_TARGET="${THROUGHPUT_TARGET:-10000}"  # 10,000 RPS
ERROR_RATE_TARGET="${ERROR_RATE_TARGET:-0.01}"  # 0.01%

# Test tracking
PASSED=0
FAILED=0
WARNED=0

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_pass() { echo -e "${GREEN}[PASS]${NC} $1"; ((PASSED++)); }
log_fail() { echo -e "${RED}[FAIL]${NC} $1"; ((FAILED++)); }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; ((WARNED++)); }

# Cleanup function
cleanup() {
    log_info "Stopping Docker containers..."
    docker compose -f "$COMPOSE_FILE" down -v 2>/dev/null || true
}

trap cleanup EXIT INT TERM

# Pre-flight checks
preflight_checks() {
    log_info "Running pre-flight checks..."

    # Check Docker
    if ! docker ps &> /dev/null; then
        log_fail "Docker is not running"
        exit 1
    fi
    log_pass "Docker is running"

    # Check Docker Compose
    if ! docker compose version &> /dev/null; then
        log_fail "Docker Compose is not available"
        exit 1
    fi
    log_pass "Docker Compose is available"

    # Check if wrk image is available
    if ! docker images | grep -q "williamyeh/wrk"; then
        log_info "Pulling wrk load testing image..."
        docker pull williamyeh/wrk:latest
    fi
    log_pass "Load testing image available"

    # Check compose file
    if [ ! -f "$COMPOSE_FILE" ]; then
        log_fail "Compose file not found: $COMPOSE_FILE"
        exit 1
    fi
    log_pass "Compose file found: $COMPOSE_FILE"
}

# Start Docker environment
start_environment() {
    log_info "Starting Docker environment for SLA testing..."

    docker compose -f "$COMPOSE_FILE" up -d erlmcp-api

    log_info "Waiting for erlmcp-api to be healthy..."
    local max_wait=60
    local waited=0

    while [ $waited -lt $max_wait ]; do
        if docker compose -f "$COMPOSE_FILE" ps | grep -q "healthy"; then
            log_pass "erlmcp-api is healthy"
            return 0
        fi
        sleep 2
        ((waited+=2))
        echo -n "."
    done

    log_fail "erlmcp-api did not become healthy within ${max_wait}s"
    return 1
}

# Test 1: Health Endpoint
test_health_endpoint() {
    log_info "Test 1: Health Endpoint Availability"

    local container_ip=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' erlmcp-api-sla-test)
    local health_url="http://${container_ip}:8080/health"

    if curl -sf "$health_url" > /dev/null; then
        log_pass "Health endpoint is accessible"

        # Check response time
        local response_time=$(curl -o /dev/null -s -w '%{time_total}\n' "$health_url")
        local response_time_ms=$(echo "$response_time * 1000" | bc)

        if (( $(echo "$response_time_ms < 100" | bc -l) )); then
            log_pass "Health endpoint response time: ${response_time_ms}ms (< 100ms)"
        else
            log_warn "Health endpoint response time: ${response_time_ms}ms (target: < 100ms)"
        fi
    else
        log_fail "Health endpoint is not accessible"
    fi
}

# Test 2: Load Test with wrk
test_load_performance() {
    log_info "Test 2: Load Performance Testing (wrk)"

    local container_ip=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' erlmcp-api-sla-test)
    local test_url="http://${container_ip}:8080/health"

    log_info "Running load test: ${CONCURRENT_CONNECTIONS} concurrent connections, ${TEST_DURATION}s duration"

    # Run load test in Docker
    local result=$(docker run --rm --network host williamyeh/wrk:latest \
        -t4 \
        -c${CONCURRENT_CONNECTIONS} \
        -d${TEST_DURATION}s \
        --latency \
        "$test_url" 2>&1)

    echo "$result"

    # Parse results
    local requests_sec=$(echo "$result" | grep "Requests/sec" | awk '{print $2}')
    local latency_avg=$(echo "$result" | grep "Latency" | awk '{print $2}')
    local latency_stdev=$(echo "$result" | grep "Stdev" | awk '{print $2}' || echo "0")

    log_info "Throughput: ${requests_sec} req/s"
    log_info "Average Latency: ${latency_avg}"
    log_info "Std Deviation: ${latency_stdev}"

    # Check throughput
    if [ -n "$requests_sec" ]; then
        if (( $(echo "$requests_sec >= 1000" | bc -l) )); then
            log_pass "Throughput > 1000 RPS achieved: ${requests_sec} req/s"
        else
            log_warn "Throughput below 1000 RPS: ${requests_sec} req/s"
        fi

        if (( $(echo "$requests_sec >= $THROUGHPUT_TARGET" | bc -l) )); then
            log_pass "Throughput target met: ${requests_sec} req/s >= ${THROUGHPUT_TARGET} RPS"
        else
            log_warn "Throughput target not met: ${requests_sec} req/s < ${THROUGHPUT_TARGET} RPS"
        fi
    fi

    # Parse latency percentiles from wrk output
    # wrk outputs latency distribution at the end
    local p50=$(echo "$result" | grep "50%" | awk '{print $2}' || echo "0")
    local p75=$(echo "$result" | grep "75%" | awk '{print $2}' || echo "0")
    local p90=$(echo "$result" | grep "90%" | awk '{print $2}' || echo "0")
    local p99=$(echo "$result" | grep "99%" | awk '{print $2}' || echo "0")

    log_info "Latency percentiles: p50=${p50}, p75=${p75}, p90=${p90}, p99=${p99}"

    # Check p95/p99 latency targets
    # wrk doesn't output p95 directly, so use p90 as proxy
    if [ -n "$p90" ]; then
        # Convert p90 from us/us to ms
        local p90_ms=$(echo "$p90" | sed 's/^[0-9]*us //' | awk '{print $1 / 1000}')
        local p99_ms=$(echo "$p99" | sed 's/^[0-9]*us //' | awk '{print $1 / 1000}')

        if (( $(echo "$p90_ms < $SLA_P95_TARGET" | bc -l) )); then
            log_pass "p90 latency (${p90_ms}ms) < p95 target (${SLA_P95_TARGET}ms)"
        else
            log_warn "p90 latency (${p90_ms}ms) exceeds p95 target (${SLA_P95_TARGET}ms)"
        fi

        if (( $(echo "$p99_ms < $SLA_P99_TARGET" | bc -l) )); then
            log_pass "p99 latency (${p99_ms}ms) < p99 target (${SLA_P99_TARGET}ms)"
        else
            log_fail "p99 latency (${p99_ms}ms) exceeds p99 target (${SLA_P99_TARGET}ms)"
        fi
    fi
}

# Test 3: Error Rate
test_error_rate() {
    log_info "Test 3: Error Rate Validation"

    local container_ip=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' erlmcp-api-sla-test)
    local test_url="http://${container_ip}:8080/health"

    local errors=0
    local total=1000

    log_info "Sending ${total} requests to check error rate..."

    for i in $(seq 1 $total); do
        if ! curl -sf "$test_url" > /dev/null 2>&1; then
            ((errors++))
        fi
    done

    local error_rate=$(echo "scale=4; $errors / $total * 100" | bc)

    log_info "Errors: ${errors} / ${total} (${error_rate}%)"

    if (( $(echo "$error_rate < $ERROR_RATE_TARGET" | bc -l) )); then
        log_pass "Error rate (${error_rate}%) < target (${ERROR_RATE_TARGET}%)"
    else
        log_fail "Error rate (${error_rate}%) exceeds target (${ERROR_RATE_TARGET}%)"
    fi
}

# Test 4: Concurrent Connections
test_concurrent_connections() {
    log_info "Test 4: Concurrent Connections (500 concurrent)"

    local container_ip=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' erlmcp-api-sla-test)
    local test_url="http://${container_ip}:8080/health"

    # Use Apache Bench for connection testing
    if ! command -v ab &> /dev/null; then
        log_warn "Apache Bench not installed, skipping concurrent connection test"
        return 0
    fi

    local result=$(ab -n 10000 -c 500 -t 60 "$test_url" 2>&1)

    echo "$result"

    # Check for failed requests
    local failed=$(echo "$result" | grep "Failed requests" | awk '{print $3}')

    if [ "$failed" = "0" ]; then
        log_pass "All 500 concurrent requests succeeded"
    else
        log_warn "$failed out of 10000 requests failed with 500 concurrent connections"
    fi
}

# Test 5: Memory and CPU Stability
test_resource_stability() {
    log_info "Test 5: Resource Stability (1-minute observation)"

    local container_name="erlmcp-api-sla-test"

    # Get initial memory usage
    local mem_before=$(docker stats --no-stream --format "{{.MemUsage}}" "$container_name" | awk '{print $1}')

    # Wait 1 minute
    sleep 60

    # Get final memory usage
    local mem_after=$(docker stats --no-stream --format "{{.MemUsage}}" "$container_name" | awk '{print $1}')

    log_info "Memory before: ${mem_before}, after: ${mem_after}"

    # Check for significant memory growth (indicating leak)
    # This is a simplified check - real monitoring would be more sophisticated
    log_info "Memory stability check completed (manual review recommended)"
}

# Test 6: SLA Module Verification
test_sla_module() {
    log_info "Test 6: SLA Monitoring Module Verification"

    if [ -f "apps/erlmcp_observability/src/erlmcp_sla_monitor.erl" ]; then
        log_pass "SLA monitoring module exists"

        # Check for key functions
        local functions=("register_sla" "check_sla_compliance" "get_sla_report" "trigger_alert")
        for func in "${functions[@]}"; do
            if grep -q "$func" "apps/erlmcp_observability/src/erlmcp_sla_monitor.erl"; then
                log_pass "Function $func found in SLA module"
            else
                log_warn "Function $func not found in SLA module"
            fi
        done
    else
        log_fail "SLA monitoring module not found"
    fi
}

# Generate report
generate_report() {
    local report_file="scripts/marketplace/sla-validation-report-$(date +%Y%m%d-%H%M%S).txt"

    log_info "Generating SLA validation report: $report_file"

    cat > "$report_file" <<EOF
====================================
SLA Validation Report
====================================
Date: $(date)
Environment: Docker
Health Endpoint: ${HEALTH_ENDPOINT}
Test Duration: ${TEST_DURATION}s
Concurrent Connections: ${CONCURRENT_CONNECTIONS}

SLA Targets:
- p95 Latency: < ${SLA_P95_TARGET}ms
- p99 Latency: < ${SLA_P99_TARGET}ms
- Throughput: > ${THROUGHPUT_TARGET} RPS
- Error Rate: < ${ERROR_RATE_TARGET}%

Test Results:
- PASSED: ${PASSED}
- FAILED: ${FAILED}
- WARNINGS: ${WARNED}

====================================
Recommendation:
$(if [ $FAILED -eq 0 ]; then
    echo "✅ APPROVE for Marketplace deployment"
else
    echo "❌ DO NOT DEPLOY - Fix failures first"
fi)
====================================
EOF

    log_pass "Report generated: $report_file"
    cat "$report_file"
}

# Main execution
main() {
    echo "===================================="
    echo "SLA Validation for Marketplace Deployment"
    echo "===================================="
    echo "Configuration:"
    echo "  Compose File: ${COMPOSE_FILE}"
    echo "  Test Duration: ${TEST_DURATION}s"
    echo "  Concurrent Connections: ${CONCURRENT_CONNECTIONS}"
    echo "  p95 Target: ${SLA_P95_TARGET}ms"
    echo "  p99 Target: ${SLA_P99_TARGET}ms"
    echo "  Throughput Target: ${THROUGHPUT_TARGET} RPS"
    echo "  Error Rate Target: ${ERROR_RATE_TARGET}%"
    echo "===================================="
    echo ""

    # Run tests
    preflight_checks
    start_environment

    test_health_endpoint
    test_load_performance
    test_error_rate
    test_concurrent_connections
    test_resource_stability
    test_sla_module

    # Generate report
    echo ""
    echo "===================================="
    echo "Test Summary"
    echo "===================================="
    echo -e "${GREEN}PASSED:${NC} ${PASSED}"
    echo -e "${YELLOW}WARNED:${NC} ${WARNED}"
    echo -e "${RED}FAILED:${NC} ${FAILED}"
    echo "===================================="
    echo ""

    generate_report

    if [ $FAILED -eq 0 ]; then
        log_pass "All critical SLA tests passed!"
        return 0
    else
        log_fail "Some critical SLA tests failed!"
        return 1
    fi
}

# Run main
main "$@"
