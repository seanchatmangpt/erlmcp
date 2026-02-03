#!/usr/bin/env bash
# =============================================================================
# erlmcp Load Test Execution Wrapper
# =============================================================================
#
# Docker-only execution wrapper for k6 load tests.
# All tests run inside Docker containers per DOCKER-ONLY CONSTITUTION.
#
# Usage:
#   ./run-tests.sh [test_name] [options]
#
# Examples:
#   ./run-tests.sh all                    # Run all tests
#   ./run-tests.sh ramp-up                # Run ramp-up test
#   ./run-tests.sh sustained              # Run sustained load test
#   ./run-tests.sh spike                  # Run spike test
#   ./run-tests.sh failover               # Run failover test
#   ./run-tests.sh ramp-up --vus 500      # Override VUs
#   ./run-tests.sh all --report-html      # Generate HTML reports
#
# Environment Variables:
#   BASE_URL         - Target base URL (default: http://erlmcp:8080)
#   K6_IMAGE         - k6 Docker image (default: grafana/k6:latest)
#   REPORT_DIR       - Report output directory (default: ./reports)
#   PROMETHEUS_PUSH  - Prometheus pushgateway URL
#   PROMETHEUS_PORT  - Prometheus port (default: 9090)
#
# Quality Gates:
#   - Exit code 0: All tests passed, SLAs met
#   - Exit code 1: One or more SLA failures
#   - Exit code 2: Test execution error
#   - Exit code 3: System not ready (health check failed)
#
# =============================================================================

set -euo pipefail

# =============================================================================
# CONFIGURATION
# =============================================================================

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Default configuration
K6_IMAGE="${K6_IMAGE:-grafana/k6:latest}"
BASE_URL="${BASE_URL:-http://erlmcp:8080}"
REPORT_DIR="${REPORT_DIR:-${SCRIPT_DIR}/reports}"
DOCKER_NETWORK="${DOCKER_NETWORK:-erlmcp-network}"
DOCKER_COMPOSE_PROJECT="erlmcp-load"

# Test files
declare -A TEST_FILES=(
    ["ramp-up"]="ramp-up-test.js"
    ["sustained"]="sustained-load-test.js"
    ["spike"]="spike-test.js"
    ["failover"]="failover-test.js"
)

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test results tracking
declare -a TEST_RESULTS
declare -a TEST_NAMES
GLOBAL_EXIT_CODE=0

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

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
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

print_header() {
    echo ""
    echo "============================================================================"
    echo "  $*"
    echo "============================================================================"
    echo ""
}

# =============================================================================
# HEALTH CHECK
# =============================================================================

check_system_health() {
    local target_url="$1"
    local max_attempts="${2:-30}"
    local wait_seconds="${3:-2}"

    log_info "Checking system health at ${target_url}"

    local attempt=1
    while [[ $attempt -le $max_attempts ]]; do
        if docker run --rm --network "${DOCKER_NETWORK}" \
            curlimages/curl:latest \
            curl -f -s -o /dev/null -w "%{http_code}" \
            --max-time 5 \
            "${target_url}/health" 2>/dev/null | grep -q "200"; then
            log_success "System is healthy"
            return 0
        fi

        log_warning "Health check failed (attempt ${attempt}/${max_attempts}), retrying..."
        sleep "$wait_seconds"
        ((attempt++))
    done

    log_error "System health check failed after ${max_attempts} attempts"
    return 1
}

# =============================================================================
# DOCKER SETUP
# =============================================================================

setup_docker_network() {
    if ! docker network inspect "${DOCKER_NETWORK}" &>/dev/null; then
        log_info "Creating Docker network: ${DOCKER_NETWORK}"
        docker network create "${DOCKER_NETWORK}" 2>/dev/null || true
    else
        log_info "Docker network exists: ${DOCKER_NETWORK}"
    fi
}

ensure_report_dir() {
    mkdir -p "${REPORT_DIR}"
    mkdir -p "${REPORT_DIR}/json"
    mkdir -p "${REPORT_DIR}/html"
}

# =============================================================================
# TEST EXECUTION
# =============================================================================

run_test() {
    local test_name="$1"
    local test_file="${TEST_FILES[$test_name]}"
    local extra_args=("${@:2}")

    if [[ ! -f "${SCRIPT_DIR}/${test_file}" ]]; then
        log_error "Test file not found: ${test_file}"
        return 2
    fi

    print_header "Running Test: ${test_name}"

    local timestamp=$(date +%Y%m%d_%H%M%S)
    local json_output="${REPORT_DIR}/json/${test_name}_${timestamp}.json"
    local html_output="${REPORT_DIR}/html/${test_name}_${timestamp}.html"

    # Build k6 command
    local k6_cmd=(
        docker run --rm
        --name "k6-${test_name}-${timestamp}"
        --network "${DOCKER_NETWORK}"
        -e "BASE_URL=${BASE_URL}"
        -e "START_TIME=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
        -v "${SCRIPT_DIR}:/tests:ro"
        -v "${REPORT_DIR}:/reports:rw"
        "${K6_IMAGE}"
        run
        "/tests/${test_file}"
        --out "json=${json_output}"
    )

    # Add HTML output if requested
    if [[ " ${extra_args[*]} " =~ " --report-html " ]] || \
       [[ "${GENERATE_HTML:-no}" == "yes" ]]; then
        k6_cmd+=("--out" "html=${html_output}")
        log_info "HTML report will be generated: ${html_output}"
    fi

    # Add Prometheus output if configured
    if [[ -n "${PROMETHEUS_PUSH:-}" ]]; then
        k6_cmd+=("--out" "experimental-prometheus-rw=${PROMETHEUS_PUSH}")
        log_info "Prometheus metrics will be pushed to: ${PROMETHEUS_PUSH}"
    fi

    # Execute the test
    log_info "Executing test: ${test_file}"
    log_info "Target URL: ${BASE_URL}"
    log_info "Output: ${json_output}"

    local exit_code=0
    "${k6_cmd[@]}" || exit_code=$?

    # Process results
    if [[ $exit_code -eq 0 ]]; then
        log_success "Test '${test_name}' PASSED - All SLAs met"
        TEST_NAMES+=("${test_name}")
        TEST_RESULTS+=("PASS")
    elif [[ $exit_code -eq 1 ]]; then
        log_warning "Test '${test_name}' had SLA FAILURES - Check report"
        TEST_NAMES+=("${test_name}")
        TEST_RESULTS+=("SLA_FAIL")
        GLOBAL_EXIT_CODE=1
    else
        log_error "Test '${test_name}' FAILED with exit code: ${exit_code}"
        TEST_NAMES+=("${test_name}")
        TEST_RESULTS+=("ERROR")
        GLOBAL_EXIT_CODE=2
    fi

    # Print quick summary
    print_test_summary "${test_name}" "${json_output}"

    return $exit_code
}

print_test_summary() {
    local test_name="$1"
    local json_output="$2"

    if [[ ! -f "${json_output}" ]]; then
        log_warning "No JSON output found at ${json_output}"
        return
    fi

    # Extract key metrics using jq
    if command -v jq &>/dev/null; then
        log_info "Test Summary for ${test_name}:"

        local metrics=$(jq -r '
            .metrics | {
                http_reqs: .http_reqs.values.count,
                http_req_duration: .http_req_duration.values,
                http_req_failed: .http_req_failed.values.rate,
                checks: .checks.values.passes
            }
        ' "${json_output}")

        echo "${metrics}" | jq -r '
            "  Total Requests: \(.http_reqs)",
            "  Successful Checks: \(.checks)",
            "  Error Rate: \((.http_req_failed * 100 | floor))%",
            "  Avg Latency: \(.http_req_duration.avg // "N/A")",
            "  P95 Latency: \(.http_req_duration."p(95)" // "N/A")",
            "  P99 Latency: \(.http_req_duration."p(99)" // "N/A")"
        ' 2>/dev/null || echo "  (Metrics parsing failed)"
    else
        log_warning "jq not installed - skipping detailed summary"
    fi
}

# =============================================================================
# COMPOSITE TEST SUITES
# =============================================================================

run_all_tests() {
    local extra_args=("$@")

    print_header "Running All Load Tests"

    local has_failures=0

    for test_name in "${!TEST_FILES[@]}"; do
        if ! run_test "${test_name}" "${extra_args[@]}"; then
            has_failures=1
        fi
        echo ""
    done

    print_final_summary
    return $has_failures
}

run_quick_smoke() {
    print_header "Quick Smoke Test (1 minute each)"

    # Run a quick subset with reduced duration
    export K6_STAGES_DURATION="30s"

    run_test "ramp-up" \
        --stage-1 '30s,50' \
        --stage-2 '30s,100' \
        || return 1
}

# =============================================================================
# SUMMARY AND REPORTING
# =============================================================================

print_final_summary() {
    print_header "Load Test Summary"

    echo "Test Results:"
    echo "-------------"

    local i=0
    local passed=0
    local failed=0
    local errors=0

    for name in "${TEST_NAMES[@]}"; do
        local result="${TEST_RESULTS[$i]}"

        case "${result}" in
            PASS)
                echo -e "  ${GREEN}✓${NC} ${name}: PASSED"
                ((passed++))
                ;;
            SLA_FAIL)
                echo -e "  ${YELLOW}!${NC} ${name}: SLA FAILURE"
                ((failed++))
                ;;
            ERROR)
                echo -e "  ${RED}✗${NC} ${name}: ERROR"
                ((errors++))
                ;;
        esac

        ((i++))
    done

    echo ""
    echo "Totals:"
    echo "  Passed:  ${passed}"
    echo "  Failed:  ${failed}"
    echo "  Errors:  ${errors}"
    echo "  Total:   ${i}"
    echo ""

    if [[ $GLOBAL_EXIT_CODE -eq 0 ]]; then
        log_success "All tests PASSED"
    elif [[ $GLOBAL_EXIT_CODE -eq 1 ]]; then
        log_warning "Some tests had SLA failures"
    else
        log_error "Some tests had execution errors"
    fi
}

# =============================================================================
# DOCKER COMPOSE INTEGRATION
# =============================================================================

start_test_services() {
    log_info "Starting test services..."

    # Create docker-compose override for load testing
    cat > "${SCRIPT_DIR}/docker-compose.load.yml" <<EOF
version: '3.8'

services:
  erlmcp-load:
    image: ${K6_IMAGE}
    networks:
      - erlmcp-network
    volumes:
      - ./tests/load:/tests:ro
      - ./tests/load/reports:/reports:rw
    environment:
      - BASE_URL=${BASE_URL}
    profiles:
      - load

  prometheus-load:
    image: prom/prometheus:latest
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'
      - '--web.console.libraries=/usr/share/prometheus/console_libraries'
      - '--web.console.templates=/usr/share/prometheus/consoles'
    networks:
      - erlmcp-network
    volumes:
      - ${SCRIPT_DIR}/prometheus.yml:/etc/prometheus/prometheus.yml:ro
      - prometheus-load-data:/prometheus
    ports:
      - "9091:9090"
    profiles:
      - load

  grafana-load:
    image: grafana/grafana:latest
    networks:
      - erlmcp-network
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=admin
      - GF_USERS_ALLOW_SIGN_UP=false
    volumes:
      - grafana-load-data:/var/lib/grafana
      - ${SCRIPT_DIR}/grafana/dashboards:/etc/grafana/provisioning/dashboards:ro
      - ${SCRIPT_DIR}/grafana/datasources:/etc/grafana/provisioning/datasources:ro
    ports:
      - "3001:3000"
    depends_on:
      - prometheus-load
    profiles:
      - load

networks:
  erlmcp-network:
    external: true

volumes:
  prometheus-load-data:
  grafana-load-data:
EOF

    log_info "Docker compose override created"
}

# =============================================================================
# HELP
# =============================================================================

show_help() {
    cat <<EOF
erlmcp Load Test Execution Wrapper

Usage:
  $0 [test_name] [options]

Tests:
  all           Run all load tests
  ramp-up       Ramp-up test (0 -> 1000 RPS)
  sustained     Sustained load test (500 RPS for 5min)
  spike         Spike test (baseline -> 5000 RPS)
  failover      Failover and HA test
  quick         Quick smoke test (1min each)
  help          Show this help message

Options:
  --base-url URL          Target base URL (default: http://erlmcp:8080)
  --k6-image IMAGE        k6 Docker image (default: grafana/k6:latest)
  --report-html           Generate HTML reports
  --prometheus URL        Push metrics to Prometheus
  --no-health-check       Skip initial health check

Environment Variables:
  BASE_URL                Target base URL
  K6_IMAGE                k6 Docker image
  REPORT_DIR              Report output directory
  PROMETHEUS_PUSH         Prometheus pushgateway URL
  GENERATE_HTML           Generate HTML reports (yes/no)

Examples:
  # Run all tests against local erlmcp
  $0 all

  # Run specific test with custom URL
  $0 ramp-up --base-url http://localhost:8080

  # Run tests with HTML report generation
  $0 all --report-html

  # Quick smoke test
  $0 quick

Docker-only execution per DOCKER-ONLY CONSTITUTION.
EOF
}

# =============================================================================
# MAIN
# =============================================================================

main() {
    local test_name="${1:-}"
    local skip_health_check=false

    # Parse arguments
    shift || true
    local extra_args=()

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --base-url)
                BASE_URL="$2"
                shift 2
                ;;
            --k6-image)
                K6_IMAGE="$2"
                shift 2
                ;;
            --report-html)
                extra_args+=("--report-html")
                shift
                ;;
            --prometheus)
                PROMETHEUS_PUSH="$2"
                shift 2
                ;;
            --no-health-check)
                skip_health_check=true
                shift
                ;;
            help|--help|-h)
                show_help
                exit 0
                ;;
            *)
                extra_args+=("$1")
                shift
                ;;
        esac
    done

    # Setup
    setup_docker_network
    ensure_report_dir

    # Health check
    if [[ "${skip_health_check}" != "true" ]]; then
        if ! check_system_health "${BASE_URL}" 30 2; then
            log_error "System health check failed. Use --no-health-check to skip."
            exit 3
        fi
    fi

    # Run requested test(s)
    case "${test_name}" in
        all)
            run_all_tests "${extra_args[@]}"
            ;;
        ramp-up|ramp)
            run_test "ramp-up" "${extra_args[@]}"
            ;;
        sustained|sustained-load)
            run_test "sustained" "${extra_args[@]}"
            ;;
        spike|spike-test)
            run_test "spike" "${extra_args[@]}"
            ;;
        failover|ha|failover-test)
            run_test "failover" "${extra_args[@]}"
            ;;
        quick|smoke)
            run_quick_smoke "${extra_args[@]}"
            ;;
        "")
            show_help
            exit 1
            ;;
        *)
            log_error "Unknown test: ${test_name}"
            show_help
            exit 2
            ;;
    esac

    exit $GLOBAL_EXIT_CODE
}

# Run main function
main "$@"
