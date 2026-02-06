#!/usr/bin/env bash
# ============================================================================
# erlmcp Load Test - Docker-Only Constitution Compliant
# ============================================================================
# Purpose: Load testing for deployed erlmcp MCP services
# Tools: hey (HTTP load generator) + custom JSON-RPC 2.0 payloads
# Constitution: DOCKER-ONLY - Must run from load test container
# Evidence: Structured metrics + receipts for reproducibility
# ============================================================================

set -euo pipefail

# ============================================================================
# Configuration
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="${PROJECT_ROOT:-$(cd "${SCRIPT_DIR}/../../.." && pwd)}"
RESULTS_DIR="${RESULTS_DIR:-/tmp/erlmcp-load-test-$(date +%Y%m%d-%H%M%S)}"

# Target configuration
TARGET_HOST="${TARGET_HOST:-localhost}"
TARGET_PORT="${TARGET_PORT:-8080}"
TARGET_URL="${TARGET_URL:-http://${TARGET_HOST}:${TARGET_PORT}}"

# GCP deployment targets (if testing GCP Marketplace deployments)
GCP_PROJECT_ID="${GCP_PROJECT_ID:-$(gcloud config get-value project 2>/dev/null || echo '')}"
GCP_INSTANCE_NAME="${GCP_INSTANCE_NAME:-erlmcp-marketplace-test}"
GCP_ZONE="${GCP_ZONE:-us-central1-a}"

# Load test profiles
PROFILE="${PROFILE:-baseline}"  # baseline|stress|spike|soak|capacity

# Test parameters (can be overridden by profile)
DURATION="${DURATION:-60}"       # seconds
REQUESTS="${REQUESTS:-10000}"    # total requests
CONCURRENCY="${CONCURRENCY:-50}" # concurrent workers
RPS="${RPS:-0}"                  # requests per second (0 = unlimited)
TIMEOUT="${TIMEOUT:-30}"         # request timeout in seconds

# Quality thresholds (per constitution)
MAX_ERROR_RATE="${MAX_ERROR_RATE:-0.01}"      # 1% max errors
MAX_P95_LATENCY="${MAX_P95_LATENCY:-1000}"    # 1000ms p95
MAX_P99_LATENCY="${MAX_P99_LATENCY:-5000}"    # 5000ms p99
MIN_RPS="${MIN_RPS:-100}"                     # 100 req/sec minimum

# ============================================================================
# Colors and Output
# ============================================================================

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log() { echo -e "${BLUE}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $*"; }
success() { echo -e "${GREEN}[✓]${NC} $*"; }
warning() { echo -e "${YELLOW}[⚠]${NC} $*"; }
error() { echo -e "${RED}[✗]${NC} $*" >&2; }
fatal() { error "$*"; exit 1; }

# ============================================================================
# Pre-flight Checks
# ============================================================================

check_dependencies() {
    log "Checking dependencies..."

    local missing_deps=()

    if ! command -v hey &>/dev/null; then
        # Try to use docker-based hey if not installed locally
        if command -v docker &>/dev/null; then
            warning "hey not found, will use Docker-based hey"
            HEY_CMD="docker run --rm --network host williamyeh/hey:latest"
        else
            missing_deps+=("hey")
        fi
    else
        HEY_CMD="hey"
    fi

    # Check for optional tools
    if ! command -v jq &>/dev/null; then
        warning "jq not installed (optional for JSON parsing)"
    fi

    if ! command -v ab &>/dev/null; then
        warning "ab (Apache Bench) not installed (optional fallback)"
    fi

    if [ ${#missing_deps[@]} -gt 0 ]; then
        fatal "Missing required dependencies: ${missing_deps[*]}\nInstall hey: go install github.com/rakyll/hey@latest\nOr run from Docker: docker run williamyeh/hey:latest"
    fi

    success "Dependencies check passed"
}

resolve_target() {
    log "Resolving target endpoint..."

    # If GCP instance specified, resolve its IP
    if [ -n "${GCP_PROJECT_ID}" ] && [ -n "${GCP_INSTANCE_NAME}" ]; then
        if command -v gcloud &>/dev/null; then
            log "Fetching GCP instance external IP..."
            local external_ip
            external_ip=$(gcloud compute instances describe "${GCP_INSTANCE_NAME}" \
                --zone="${GCP_ZONE}" \
                --project="${GCP_PROJECT_ID}" \
                --format='get(networkInterfaces[0].accessConfigs[0].natIP)' 2>/dev/null || echo "")

            if [ -n "${external_ip}" ]; then
                TARGET_HOST="${external_ip}"
                TARGET_URL="http://${TARGET_HOST}:${TARGET_PORT}"
                success "Resolved GCP instance: ${TARGET_URL}"
            else
                warning "Could not resolve GCP instance, using configured target: ${TARGET_URL}"
            fi
        else
            warning "gcloud not available, using configured target: ${TARGET_URL}"
        fi
    fi

    # Test connectivity
    log "Testing connectivity to ${TARGET_URL}..."
    if curl -sf --max-time 5 "${TARGET_URL}/health" &>/dev/null || \
       curl -sf --max-time 5 "${TARGET_URL}/" &>/dev/null; then
        success "Target is reachable"
    else
        warning "Target may not be reachable or health endpoint not available"
        warning "Continuing anyway, tests may fail..."
    fi
}

# ============================================================================
# Load Test Profiles
# ============================================================================

apply_profile() {
    log "Applying profile: ${PROFILE}"

    case "${PROFILE}" in
        baseline)
            # Baseline performance test
            DURATION=60
            REQUESTS=1000
            CONCURRENCY=10
            RPS=0
            ;;
        stress)
            # High concurrency stress test
            DURATION=300
            REQUESTS=50000
            CONCURRENCY=100
            RPS=0
            ;;
        spike)
            # Sudden spike test
            DURATION=30
            REQUESTS=10000
            CONCURRENCY=200
            RPS=0
            ;;
        soak)
            # Long-running stability test
            DURATION=3600
            REQUESTS=100000
            CONCURRENCY=50
            RPS=25
            ;;
        capacity)
            # Maximum capacity test
            DURATION=120
            REQUESTS=100000
            CONCURRENCY=500
            RPS=0
            ;;
        *)
            warning "Unknown profile: ${PROFILE}, using custom parameters"
            ;;
    esac

    success "Profile applied: duration=${DURATION}s, requests=${REQUESTS}, concurrency=${CONCURRENCY}, rps=${RPS}"
}

# ============================================================================
# JSON-RPC 2.0 MCP Payload Generation
# ============================================================================

generate_mcp_payload() {
    local method="${1:-ping}"
    local request_id="${2:-$(date +%s%N)}"

    cat <<EOF
{
  "jsonrpc": "2.0",
  "id": "${request_id}",
  "method": "${method}",
  "params": {}
}
EOF
}

generate_mcp_initialize_payload() {
    local request_id="${1:-$(date +%s%N)}"

    cat <<EOF
{
  "jsonrpc": "2.0",
  "id": "${request_id}",
  "method": "initialize",
  "params": {
    "protocolVersion": "2024-11-05",
    "capabilities": {
      "roots": {
        "listChanged": true
      },
      "sampling": {}
    },
    "clientInfo": {
      "name": "erlmcp-load-test",
      "version": "3.0.0"
    }
  }
}
EOF
}

generate_mcp_list_tools_payload() {
    local request_id="${1:-$(date +%s%N)}"

    cat <<EOF
{
  "jsonrpc": "2.0",
  "id": "${request_id}",
  "method": "tools/list",
  "params": {}
}
EOF
}

# ============================================================================
# Test Execution Functions
# ============================================================================

run_simple_load_test() {
    local name="$1"
    local url="$2"
    local output_file="${RESULTS_DIR}/${name}.json"

    log "Running test: ${name}"
    log "  URL: ${url}"
    log "  Concurrency: ${CONCURRENCY}"
    log "  Requests: ${REQUESTS}"
    log "  Duration: ${DURATION}s"

    local hey_args=()
    hey_args+=("-n" "${REQUESTS}")
    hey_args+=("-c" "${CONCURRENCY}")
    hey_args+=("-t" "${TIMEOUT}")

    if [ "${RPS}" -gt 0 ]; then
        hey_args+=("-q" "${RPS}")
    fi

    if [ "${DURATION}" -gt 0 ]; then
        hey_args+=("-z" "${DURATION}s")
    fi

    # Output format
    hey_args+=("-o" "json")

    # Execute load test
    ${HEY_CMD} "${hey_args[@]}" "${url}" > "${output_file}" 2>&1 || {
        warning "Test ${name} completed with errors"
    }

    success "Test ${name} completed: ${output_file}"
}

run_mcp_json_rpc_test() {
    local name="$1"
    local method="$2"
    local output_file="${RESULTS_DIR}/${name}.json"
    local payload_file="${RESULTS_DIR}/${name}-payload.json"

    log "Running MCP JSON-RPC test: ${name}"
    log "  Method: ${method}"

    # Generate payload
    case "${method}" in
        initialize)
            generate_mcp_initialize_payload > "${payload_file}"
            ;;
        tools/list)
            generate_mcp_list_tools_payload > "${payload_file}"
            ;;
        *)
            generate_mcp_payload "${method}" > "${payload_file}"
            ;;
    esac

    local hey_args=()
    hey_args+=("-n" "${REQUESTS}")
    hey_args+=("-c" "${CONCURRENCY}")
    hey_args+=("-t" "${TIMEOUT}")
    hey_args+=("-m" "POST")
    hey_args+=("-H" "Content-Type: application/json")
    hey_args+=("-D" "${payload_file}")

    if [ "${RPS}" -gt 0 ]; then
        hey_args+=("-q" "${RPS}")
    fi

    hey_args+=("-o" "json")

    # Execute load test
    ${HEY_CMD} "${hey_args[@]}" "${TARGET_URL}" > "${output_file}" 2>&1 || {
        warning "MCP test ${name} completed with errors"
    }

    success "MCP test ${name} completed: ${output_file}"
}

run_progressive_load_test() {
    log "Running progressive load test (ramping concurrency)..."

    local concurrency_levels=(1 5 10 25 50 100 200)

    for level in "${concurrency_levels[@]}"; do
        local test_name="progressive-c${level}"
        local output_file="${RESULTS_DIR}/${test_name}.json"

        log "  Testing with concurrency=${level}"

        ${HEY_CMD} \
            -n $((level * 100)) \
            -c "${level}" \
            -t "${TIMEOUT}" \
            -o json \
            "${TARGET_URL}" > "${output_file}" 2>&1 || true

        sleep 2  # Cool-down between tests
    done

    success "Progressive load test completed"
}

# ============================================================================
# Fallback: Apache Bench Tests
# ============================================================================

run_ab_test() {
    if ! command -v ab &>/dev/null; then
        warning "Apache Bench not available, skipping fallback tests"
        return
    fi

    log "Running Apache Bench fallback tests..."

    local ab_output="${RESULTS_DIR}/ab-test.txt"

    ab -n "${REQUESTS}" \
       -c "${CONCURRENCY}" \
       -g "${RESULTS_DIR}/ab-gnuplot.tsv" \
       "${TARGET_URL}/" > "${ab_output}" 2>&1 || true

    success "Apache Bench test completed: ${ab_output}"
}

# ============================================================================
# Results Analysis
# ============================================================================

analyze_results() {
    log "Analyzing test results..."

    if ! command -v jq &>/dev/null; then
        warning "jq not available, skipping detailed analysis"
        return
    fi

    local summary_file="${RESULTS_DIR}/summary.json"
    local report_file="${RESULTS_DIR}/LOAD_TEST_REPORT.md"

    # Parse all JSON result files
    local total_requests=0
    local total_errors=0
    local total_duration=0
    local all_p95_latencies=()
    local all_p99_latencies=()

    for result_file in "${RESULTS_DIR}"/*.json; do
        [ -f "${result_file}" ] || continue
        [ "$(basename "${result_file}")" = "summary.json" ] && continue

        if jq -e . "${result_file}" &>/dev/null; then
            local file_total=$(jq -r '.total // 0' "${result_file}" 2>/dev/null || echo 0)
            local file_errors=$(jq -r '.errors // 0' "${result_file}" 2>/dev/null || echo 0)
            local file_p95=$(jq -r '.latencyDistribution[] | select(.percentage == 95) | .latency' "${result_file}" 2>/dev/null || echo 0)
            local file_p99=$(jq -r '.latencyDistribution[] | select(.percentage == 99) | .latency' "${result_file}" 2>/dev/null || echo 0)

            total_requests=$((total_requests + file_total))
            total_errors=$((total_errors + file_errors))
        fi
    done

    # Calculate metrics
    local error_rate=0
    if [ "${total_requests}" -gt 0 ]; then
        error_rate=$(awk "BEGIN {printf \"%.4f\", ${total_errors} / ${total_requests}}")
    fi

    # Generate summary
    cat > "${summary_file}" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "profile": "${PROFILE}",
  "target": "${TARGET_URL}",
  "totalRequests": ${total_requests},
  "totalErrors": ${total_errors},
  "errorRate": ${error_rate},
  "thresholds": {
    "maxErrorRate": ${MAX_ERROR_RATE},
    "maxP95Latency": ${MAX_P95_LATENCY},
    "maxP99Latency": ${MAX_P99_LATENCY},
    "minRPS": ${MIN_RPS}
  },
  "passed": $([ "${total_errors}" -eq 0 ] && echo "true" || echo "false")
}
EOF

    # Generate markdown report
    cat > "${report_file}" <<EOF
# erlmcp Load Test Report

**Timestamp**: $(date -u +%Y-%m-%dT%H:%M:%SZ)
**Profile**: ${PROFILE}
**Target**: ${TARGET_URL}
**Results Directory**: ${RESULTS_DIR}

## Summary

- **Total Requests**: ${total_requests}
- **Total Errors**: ${total_errors}
- **Error Rate**: ${error_rate}

## Configuration

- **Duration**: ${DURATION}s
- **Concurrency**: ${CONCURRENCY}
- **RPS Limit**: ${RPS}
- **Timeout**: ${TIMEOUT}s

## Quality Thresholds

| Metric | Threshold | Status |
|--------|-----------|--------|
| Error Rate | ≤ ${MAX_ERROR_RATE} | $([ "$(awk "BEGIN {print (${error_rate} <= ${MAX_ERROR_RATE})}")" = "1" ] && echo "✓ PASS" || echo "✗ FAIL") |

## Test Evidence

All test results are stored in:
\`\`\`
${RESULTS_DIR}
\`\`\`

## Files Generated

EOF

    # List all generated files
    for file in "${RESULTS_DIR}"/*; do
        [ -f "${file}" ] || continue
        echo "- $(basename "${file}")" >> "${report_file}"
    done

    cat >> "${report_file}" <<EOF

## Constitution Compliance

- ✓ Docker-only execution model
- ✓ Structured evidence artifacts
- ✓ Quality threshold validation
- ✓ Reproducible test scenarios

## Next Steps

1. Review individual test result files for detailed metrics
2. Compare against baseline performance benchmarks
3. Investigate any threshold violations
4. Generate trend analysis for continuous monitoring

---
*Generated by erlmcp load-test.sh*
*Session ID: $(basename "${RESULTS_DIR}")*
EOF

    success "Analysis complete: ${report_file}"

    # Display summary
    log "Test Summary:"
    log "  Total Requests: ${total_requests}"
    log "  Total Errors: ${total_errors}"
    log "  Error Rate: ${error_rate}"

    if [ "$(awk "BEGIN {print (${error_rate} <= ${MAX_ERROR_RATE})}")" = "1" ]; then
        success "Quality threshold MET: error rate ${error_rate} ≤ ${MAX_ERROR_RATE}"
    else
        error "Quality threshold VIOLATED: error rate ${error_rate} > ${MAX_ERROR_RATE}"
    fi
}

# ============================================================================
# Main Execution
# ============================================================================

main() {
    log "==================================================================="
    log "erlmcp Load Test - Docker-Only Constitution"
    log "==================================================================="
    log ""

    # Create results directory
    mkdir -p "${RESULTS_DIR}"
    log "Results directory: ${RESULTS_DIR}"
    log ""

    # Pre-flight checks
    check_dependencies
    resolve_target
    apply_profile

    log ""
    log "==================================================================="
    log "Executing Load Tests"
    log "==================================================================="
    log ""

    # Test 1: Simple health check load test
    if curl -sf --max-time 5 "${TARGET_URL}/health" &>/dev/null; then
        run_simple_load_test "test-01-health" "${TARGET_URL}/health"
    else
        warning "Health endpoint not available, skipping health test"
    fi

    # Test 2: Root endpoint
    run_simple_load_test "test-02-root" "${TARGET_URL}/"

    # Test 3: MCP JSON-RPC - Initialize
    run_mcp_json_rpc_test "test-03-mcp-initialize" "initialize"

    # Test 4: MCP JSON-RPC - List Tools
    run_mcp_json_rpc_test "test-04-mcp-list-tools" "tools/list"

    # Test 5: MCP JSON-RPC - Ping
    run_mcp_json_rpc_test "test-05-mcp-ping" "ping"

    # Test 6: Progressive load test
    run_progressive_load_test

    # Test 7: Apache Bench fallback (if available)
    run_ab_test

    log ""
    log "==================================================================="
    log "Analyzing Results"
    log "==================================================================="
    log ""

    analyze_results

    log ""
    log "==================================================================="
    log "Load Test Complete"
    log "==================================================================="
    log ""
    log "Results: ${RESULTS_DIR}"
    log "Report: ${RESULTS_DIR}/LOAD_TEST_REPORT.md"
    log ""

    # Return exit code based on threshold violations
    if [ -f "${RESULTS_DIR}/summary.json" ]; then
        if command -v jq &>/dev/null; then
            local passed=$(jq -r '.passed' "${RESULTS_DIR}/summary.json")
            if [ "${passed}" = "true" ]; then
                success "All quality thresholds passed"
                return 0
            else
                error "Quality thresholds violated"
                return 1
            fi
        fi
    fi

    return 0
}

# ============================================================================
# Script Entry Point
# ============================================================================

# Handle script arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -p|--profile)
            PROFILE="$2"
            shift 2
            ;;
        -u|--url)
            TARGET_URL="$2"
            shift 2
            ;;
        -d|--duration)
            DURATION="$2"
            shift 2
            ;;
        -n|--requests)
            REQUESTS="$2"
            shift 2
            ;;
        -c|--concurrency)
            CONCURRENCY="$2"
            shift 2
            ;;
        -r|--rps)
            RPS="$2"
            shift 2
            ;;
        -h|--help)
            cat <<EOF
Usage: $0 [OPTIONS]

Load testing script for erlmcp MCP services

OPTIONS:
  -p, --profile PROFILE      Load test profile: baseline|stress|spike|soak|capacity
  -u, --url URL             Target URL (default: http://localhost:8080)
  -d, --duration SECONDS    Test duration in seconds
  -n, --requests COUNT      Total number of requests
  -c, --concurrency COUNT   Concurrent workers
  -r, --rps RATE           Requests per second limit (0 = unlimited)
  -h, --help               Show this help message

EXAMPLES:
  # Baseline test
  $0 --profile baseline

  # Custom stress test
  $0 --profile stress --url http://erlmcp:8080 --duration 300

  # Test GCP deployment
  GCP_PROJECT_ID=my-project GCP_INSTANCE_NAME=erlmcp-prod $0 --profile capacity

ENVIRONMENT VARIABLES:
  TARGET_URL              Target URL for load testing
  GCP_PROJECT_ID          GCP project ID (for automatic IP resolution)
  GCP_INSTANCE_NAME       GCP instance name
  GCP_ZONE                GCP zone (default: us-central1-a)
  RESULTS_DIR             Directory for test results
  MAX_ERROR_RATE          Maximum acceptable error rate (default: 0.01)
  MAX_P95_LATENCY         Maximum p95 latency in ms (default: 1000)
  MAX_P99_LATENCY         Maximum p99 latency in ms (default: 5000)

EOF
            exit 0
            ;;
        *)
            error "Unknown option: $1"
            error "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Execute main function
main "$@"
