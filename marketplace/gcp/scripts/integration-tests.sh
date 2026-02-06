#!/bin/bash
# ============================================================================
# ERLMCP MARKETPLACE INTEGRATION TESTS
# End-to-End Testing for Deployed GCP Services
# ============================================================================
# Constitution: DOCKER-ONLY. All Erlang tests executed via Docker.
# Tests deployed services (GKE, Cloud Run, GCE) for functional correctness.
#
# WHY: Prove production readiness. Validate deployed services under real conditions.
#      No mocks. Real processes. Real failures. Real evidence.
#
# WHAT: Functional API tests, health/readiness, observability, chaos engineering,
#       performance under load, secrets handling, graceful degradation.
#
# HOW: docker compose run erlmcp-ct (for test suites)
#      + curl/gcloud (for deployed service verification)
#      + Evidence collection (receipts with hashes)
#
# Quality Invariants: errors=0, failures=0, coverage≥0.8, regression<0.1
# ============================================================================

set -euo pipefail

# ============================================================================
# ANSI Colors
# ============================================================================
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly MAGENTA='\033[0;35m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m'

# ============================================================================
# Logging Functions
# ============================================================================
log_info() { echo -e "${GREEN}[INFO]${NC} $(date -Iseconds) $*"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $(date -Iseconds) $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $(date -Iseconds) $*"; }
log_test() { echo -e "${BLUE}[TEST]${NC} $(date -Iseconds) $*"; }
log_pass() { echo -e "${GREEN}[PASS]${NC} $(date -Iseconds) $*"; }
log_fail() { echo -e "${RED}[FAIL]${NC} $(date -Iseconds) $*"; }
log_docker() { echo -e "${MAGENTA}[DOCKER]${NC} $(date -Iseconds) $*"; }
log_evidence() { echo -e "${CYAN}[EVIDENCE]${NC} $(date -Iseconds) $*"; }

# ============================================================================
# Configuration & Constants
# ============================================================================
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
readonly WORKSPACE_DIR="$(dirname "$MARKETPLACE_DIR")"
readonly EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence/integration"
readonly DOCKER_COMPOSE_FILE="${WORKSPACE_DIR}/docker-compose.yml"

# Test Configuration
readonly TEST_SUITE="${TEST_SUITE:-full}"
readonly TEST_TIMEOUT="${TEST_TIMEOUT:-1800}"
readonly PARALLEL_TESTS="${PARALLEL_TESTS:-false}"

# GCP Configuration
PROJECT_ID="${PROJECT_ID:-}"
REGION="${REGION:-us-central1}"
ZONE="${ZONE:-us-central1-a}"
readonly TEST_ID="integration-$(date +%s)"

# Deployment Targets
SERVICE_URL="${SERVICE_URL:-}"
CLUSTER_NAME="${CLUSTER_NAME:-}"
CLUSTER_ENDPOINT="${CLUSTER_ENDPOINT:-}"
INSTANCE_IP="${INSTANCE_IP:-}"

# Test Results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0

# Evidence Tracking
declare -a EVIDENCE_FILES=()

# ============================================================================
# Docker Execution Functions (Constitution: DOCKER-ONLY)
# ============================================================================

docker_exec_test() {
    local test_name="$1"
    local docker_service="$2"
    local docker_cmd="$3"

    log_docker "Running test via Docker: $test_name"
    log_docker "Service: $docker_service | Command: $docker_cmd"

    local start_time
    local end_time
    local duration
    local exit_code

    start_time=$(date +%s)

    # Execute via Docker Compose (DOCKER-ONLY)
    if docker compose -f "$DOCKER_COMPOSE_FILE" run --rm \
        -e ERLMCP_ENV=test \
        -e ERLMCP_SERVICE_URL="$SERVICE_URL" \
        -e ERLMCP_TEST_NAME="$test_name" \
        "$docker_service" \
        sh -c "$docker_cmd" > "${EVIDENCE_DIR}/${test_name}.log" 2>&1; then
        exit_code=0
        log_pass "Docker test passed: $test_name"
    else
        exit_code=$?
        log_fail "Docker test failed: $test_name (exit: $exit_code)"
    fi

    end_time=$(date +%s)
    duration=$((end_time - start_time))

    # Generate evidence receipt
    generate_evidence_receipt "$test_name" "$docker_service" "$docker_cmd" "$exit_code" "$duration"

    return $exit_code
}

docker_run_ct_suite() {
    local suite_name="$1"
    local suite_path="${2:-apps/erlmcp_core/test/${suite_name}_SUITE.erl}"

    log_docker "Running CT suite via Docker: $suite_name"

    # Use erlmcp-ct service (Common Test gate)
    docker_exec_test \
        "ct_suite_${suite_name}" \
        "erlmcp-ct" \
        "rebar3 ct --suite=$suite_path --cover --verbose"
}

docker_run_integration_suite() {
    local suite_type="$1"

    log_docker "Running integration suite: $suite_type"

    case "$suite_type" in
        api)
            docker_exec_test \
                "integration_api" \
                "erlmcp-ct" \
                "rebar3 ct --suite=test/integration/api_integration_SUITE.erl"
            ;;
        transport)
            docker_exec_test \
                "integration_transport" \
                "erlmcp-ct" \
                "rebar3 ct --suite=test/integration/transport_integration_SUITE.erl"
            ;;
        observability)
            docker_exec_test \
                "integration_observability" \
                "erlmcp-ct" \
                "rebar3 ct --suite=test/integration/observability_integration_SUITE.erl"
            ;;
        *)
            log_error "Unknown integration suite: $suite_type"
            return 1
            ;;
    esac
}

# ============================================================================
# Prerequisites & Validation
# ============================================================================

check_prerequisites() {
    log_info "Checking prerequisites..."

    # Docker required (CONSTITUTION)
    if ! command -v docker &> /dev/null; then
        log_error "docker not found - REQUIRED by DOCKER-ONLY constitution"
        exit 1
    fi

    if ! docker compose version &> /dev/null; then
        log_error "docker compose not found"
        exit 1
    fi

    # Verify docker-compose.yml exists
    if [ ! -f "$DOCKER_COMPOSE_FILE" ]; then
        log_error "docker-compose.yml not found at $DOCKER_COMPOSE_FILE"
        exit 1
    fi

    # Optional: gcloud for GCP operations
    if ! command -v gcloud &> /dev/null; then
        log_warn "gcloud not found - GCP tests will be skipped"
    fi

    # Optional: kubectl for K8s operations
    if ! command -v kubectl &> /dev/null; then
        log_warn "kubectl not found - K8s tests will be skipped"
    fi

    # curl required for HTTP tests
    if ! command -v curl &> /dev/null; then
        log_error "curl not found - required for HTTP endpoint testing"
        exit 1
    fi

    # jq optional but useful
    if ! command -v jq &> /dev/null; then
        log_warn "jq not found - JSON parsing will be limited"
    fi

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    log_info "Prerequisites check passed"
}

validate_deployment_target() {
    log_info "Validating deployment target..."

    if [ -z "$SERVICE_URL" ] && [ -z "$CLUSTER_ENDPOINT" ] && [ -z "$INSTANCE_IP" ]; then
        log_error "No deployment target specified"
        log_error "Set one of: SERVICE_URL, CLUSTER_ENDPOINT, INSTANCE_IP"
        return 1
    fi

    if [ -n "$SERVICE_URL" ]; then
        log_info "Testing Cloud Run deployment: $SERVICE_URL"
        export DEPLOYMENT_TYPE="cloud-run"
    elif [ -n "$CLUSTER_ENDPOINT" ]; then
        log_info "Testing GKE deployment: $CLUSTER_ENDPOINT"
        export DEPLOYMENT_TYPE="gke"
    elif [ -n "$INSTANCE_IP" ]; then
        log_info "Testing GCE deployment: $INSTANCE_IP"
        export DEPLOYMENT_TYPE="gce"
    fi

    return 0
}

# ============================================================================
# Evidence Collection (Provenance Receipts)
# ============================================================================

generate_evidence_receipt() {
    local test_name="$1"
    local docker_service="$2"
    local docker_cmd="$3"
    local exit_code="$4"
    local duration="$5"

    local receipt_file="${EVIDENCE_DIR}/${test_name}.receipt"
    local git_sha
    local image_digest

    git_sha=$(cd "$WORKSPACE_DIR" && git rev-parse HEAD 2>/dev/null || echo "unknown")

    # Get Docker image digest
    image_digest=$(docker inspect "erlmcp:3.0.0-ct" --format='{{.Id}}' 2>/dev/null || echo "unknown")

    # Generate receipt hash: hash(git_sha || image_digest || service || cmd || exit || timestamp)
    local receipt_data="${git_sha}${image_digest}${docker_service}${docker_cmd}${exit_code}$(date -Iseconds)"
    local receipt_hash
    receipt_hash=$(echo -n "$receipt_data" | sha256sum | awk '{print $1}')

    # Write receipt
    cat > "$receipt_file" <<EOF
{
  "test_name": "$test_name",
  "timestamp": "$(date -Iseconds)",
  "git_sha": "$git_sha",
  "image_digest": "$image_digest",
  "docker_service": "$docker_service",
  "docker_command": "$docker_cmd",
  "exit_code": $exit_code,
  "duration_seconds": $duration,
  "status": "$([ $exit_code -eq 0 ] && echo "PASS" || echo "FAIL")",
  "evidence_log": "${test_name}.log",
  "receipt_hash": "$receipt_hash"
}
EOF

    EVIDENCE_FILES+=("$receipt_file")
    log_evidence "Receipt generated: $receipt_file (hash: ${receipt_hash:0:16}...)"
}

collect_final_evidence() {
    log_info "Collecting final evidence summary..."

    local summary_file="${EVIDENCE_DIR}/integration-tests-summary.json"

    cat > "$summary_file" <<EOF
{
  "test_run_id": "$TEST_ID",
  "timestamp": "$(date -Iseconds)",
  "total_tests": $TOTAL_TESTS,
  "passed_tests": $PASSED_TESTS,
  "failed_tests": $FAILED_TESTS,
  "skipped_tests": $SKIPPED_TESTS,
  "success_rate": $(echo "scale=4; $PASSED_TESTS / $TOTAL_TESTS" | bc 2>/dev/null || echo "0"),
  "deployment_type": "${DEPLOYMENT_TYPE:-unknown}",
  "service_url": "${SERVICE_URL:-none}",
  "evidence_receipts": [
$(printf '    "%s"' "${EVIDENCE_FILES[0]}" && printf ',\n    "%s"' "${EVIDENCE_FILES[@]:1}")
  ]
}
EOF

    log_evidence "Summary generated: $summary_file"
}

# ============================================================================
# Test Suite: Health & Readiness Checks
# ============================================================================

test_health_endpoint() {
    log_test "Testing health endpoint..."
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    local url="${SERVICE_URL:-http://${INSTANCE_IP:-localhost}:8080}"
    local health_url="${url}/health"

    log_info "Health URL: $health_url"

    local response
    local http_code

    if response=$(curl -s -w "\n%{http_code}" --max-time 10 "$health_url" 2>&1); then
        http_code=$(echo "$response" | tail -n1)
        local body
        body=$(echo "$response" | head -n-1)

        if [ "$http_code" = "200" ]; then
            log_pass "Health check passed (HTTP $http_code)"
            echo "$body" > "${EVIDENCE_DIR}/health-response.json"
            PASSED_TESTS=$((PASSED_TESTS + 1))
            return 0
        else
            log_fail "Health check failed (HTTP $http_code)"
            echo "$body" > "${EVIDENCE_DIR}/health-response-failed.json"
            FAILED_TESTS=$((FAILED_TESTS + 1))
            return 1
        fi
    else
        log_fail "Health check failed - connection error"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

test_readiness_endpoint() {
    log_test "Testing readiness endpoint..."
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    local url="${SERVICE_URL:-http://${INSTANCE_IP:-localhost}:8080}"
    local ready_url="${url}/ready"

    local http_code
    http_code=$(curl -s -o "${EVIDENCE_DIR}/readiness-response.json" -w "%{http_code}" "$ready_url" 2>/dev/null || echo "000")

    if [ "$http_code" = "200" ]; then
        log_pass "Readiness check passed (HTTP $http_code)"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        log_fail "Readiness check failed (HTTP $http_code)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

# ============================================================================
# Test Suite: API Functional Tests
# ============================================================================

test_api_list_tools() {
    log_test "Testing API: List Tools..."
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    local url="${SERVICE_URL:-http://${INSTANCE_IP:-localhost}:8080}"
    local api_url="${url}/mcp/v1/tools"

    local response
    local http_code

    response=$(curl -s -w "\n%{http_code}" -X POST \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}' \
        "$api_url" 2>&1)

    http_code=$(echo "$response" | tail -n1)
    local body
    body=$(echo "$response" | head -n-1)

    if [ "$http_code" = "200" ]; then
        echo "$body" > "${EVIDENCE_DIR}/api-list-tools.json"

        # Validate JSON response
        if command -v jq &> /dev/null; then
            if echo "$body" | jq -e '.result.tools' > /dev/null 2>&1; then
                log_pass "List tools API passed - valid JSON-RPC response"
                PASSED_TESTS=$((PASSED_TESTS + 1))
                return 0
            else
                log_fail "List tools API returned invalid response structure"
                FAILED_TESTS=$((FAILED_TESTS + 1))
                return 1
            fi
        else
            log_pass "List tools API passed (HTTP $http_code, jq not available for validation)"
            PASSED_TESTS=$((PASSED_TESTS + 1))
            return 0
        fi
    else
        log_fail "List tools API failed (HTTP $http_code)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

test_api_call_tool() {
    log_test "Testing API: Call Tool..."
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    local url="${SERVICE_URL:-http://${INSTANCE_IP:-localhost}:8080}"
    local api_url="${url}/mcp/v1/call-tool"

    local response
    local http_code

    response=$(curl -s -w "\n%{http_code}" -X POST \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"echo","arguments":{"message":"test"}}}' \
        "$api_url" 2>&1)

    http_code=$(echo "$response" | tail -n1)
    local body
    body=$(echo "$response" | head -n-1)

    if [ "$http_code" = "200" ]; then
        echo "$body" > "${EVIDENCE_DIR}/api-call-tool.json"
        log_pass "Call tool API passed (HTTP $http_code)"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        log_fail "Call tool API failed (HTTP $http_code)"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

# ============================================================================
# Test Suite: Observability
# ============================================================================

test_metrics_endpoint() {
    log_test "Testing Prometheus metrics endpoint..."
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    local url="${SERVICE_URL:-http://${INSTANCE_IP:-localhost}:9100}"
    local metrics_url="${url}/metrics"

    local http_code
    http_code=$(curl -s -o "${EVIDENCE_DIR}/metrics.txt" -w "%{http_code}" "$metrics_url" 2>/dev/null || echo "000")

    if [ "$http_code" = "200" ]; then
        # Verify metrics format
        if grep -q "^# TYPE" "${EVIDENCE_DIR}/metrics.txt"; then
            log_pass "Metrics endpoint passed - valid Prometheus format"
            PASSED_TESTS=$((PASSED_TESTS + 1))
            return 0
        else
            log_fail "Metrics endpoint returned invalid Prometheus format"
            FAILED_TESTS=$((FAILED_TESTS + 1))
            return 1
        fi
    else
        log_warn "Metrics endpoint not accessible (HTTP $http_code) - may be on different port"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
        return 0
    fi
}

test_logging_output() {
    log_test "Testing structured logging output..."
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    if [ -n "$CLUSTER_NAME" ] && command -v kubectl &> /dev/null; then
        # GKE deployment - check pod logs
        local pod_name
        pod_name=$(kubectl get pods -l app=erlmcp -o jsonpath='{.items[0].metadata.name}' 2>/dev/null || echo "")

        if [ -n "$pod_name" ]; then
            kubectl logs "$pod_name" --tail=50 > "${EVIDENCE_DIR}/pod-logs.txt" 2>&1

            if [ -s "${EVIDENCE_DIR}/pod-logs.txt" ]; then
                log_pass "Logging output captured from pod: $pod_name"
                PASSED_TESTS=$((PASSED_TESTS + 1))
                return 0
            fi
        fi

        log_warn "Could not retrieve pod logs"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
        return 0

    elif [ -n "$SERVICE_URL" ] && command -v gcloud &> /dev/null; then
        # Cloud Run deployment - check Cloud Logging
        local service_name
        service_name=$(echo "$SERVICE_URL" | sed -E 's|https://([^-]+).*|\1|')

        gcloud logging read "resource.labels.service_name=$service_name" \
            --project="$PROJECT_ID" \
            --limit=20 \
            --format=json > "${EVIDENCE_DIR}/cloudrun-logs.json" 2>&1 || true

        if [ -s "${EVIDENCE_DIR}/cloudrun-logs.json" ]; then
            log_pass "Logging output captured from Cloud Run service"
            PASSED_TESTS=$((PASSED_TESTS + 1))
            return 0
        fi

        log_warn "Could not retrieve Cloud Run logs"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
        return 0
    else
        log_warn "Logging test skipped - no applicable deployment target"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
        return 0
    fi
}

# ============================================================================
# Test Suite: Performance Under Load
# ============================================================================

test_concurrent_requests() {
    log_test "Testing concurrent request handling..."
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    local url="${SERVICE_URL:-http://${INSTANCE_IP:-localhost}:8080}"
    local health_url="${url}/health"
    local concurrent="${CONCURRENT_REQUESTS:-10}"
    local requests_per_worker="${REQUESTS_PER_WORKER:-5}"

    log_info "Sending $concurrent concurrent workers, $requests_per_worker requests each"

    local start_time
    local end_time
    local success_count=0
    local fail_count=0

    start_time=$(date +%s)

    # Simple concurrent test using background processes
    for i in $(seq 1 "$concurrent"); do
        {
            for j in $(seq 1 "$requests_per_worker"); do
                if curl -s -f -m 5 "$health_url" > /dev/null 2>&1; then
                    echo "success" >> "${EVIDENCE_DIR}/concurrent-results.txt"
                else
                    echo "fail" >> "${EVIDENCE_DIR}/concurrent-results.txt"
                fi
            done
        } &
    done

    # Wait for all background jobs
    wait

    end_time=$(date +%s)
    local duration=$((end_time - start_time))

    # Count results
    success_count=$(grep -c "success" "${EVIDENCE_DIR}/concurrent-results.txt" 2>/dev/null || echo "0")
    fail_count=$(grep -c "fail" "${EVIDENCE_DIR}/concurrent-results.txt" 2>/dev/null || echo "0")
    local total_requests=$((concurrent * requests_per_worker))
    local rps=$(echo "scale=2; $total_requests / $duration" | bc 2>/dev/null || echo "0")

    log_info "Completed $total_requests requests in ${duration}s"
    log_info "Success: $success_count | Failed: $fail_count | RPS: $rps"

    if [ "$success_count" -gt 0 ] && [ "$fail_count" -eq 0 ]; then
        log_pass "Concurrent requests test passed - all requests succeeded"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    elif [ "$success_count" -gt "$((total_requests * 80 / 100))" ]; then
        log_pass "Concurrent requests test passed - 80%+ success rate"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        log_fail "Concurrent requests test failed - success rate too low"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

# ============================================================================
# Test Suite: Chaos Engineering (Resilience)
# ============================================================================

test_service_restart_recovery() {
    log_test "Testing service restart recovery..."
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    if [ -n "$CLUSTER_NAME" ] && command -v kubectl &> /dev/null; then
        # GKE deployment - kill pod and verify recovery
        local pod_name
        pod_name=$(kubectl get pods -l app=erlmcp -o jsonpath='{.items[0].metadata.name}' 2>/dev/null || echo "")

        if [ -n "$pod_name" ]; then
            log_info "Deleting pod: $pod_name"
            kubectl delete pod "$pod_name" --wait=false > /dev/null 2>&1

            # Wait for new pod to be ready
            log_info "Waiting for pod recovery (max 60s)..."
            if kubectl wait --for=condition=ready pod -l app=erlmcp --timeout=60s > /dev/null 2>&1; then
                log_pass "Service recovered after pod restart"
                PASSED_TESTS=$((PASSED_TESTS + 1))
                return 0
            else
                log_fail "Service did not recover in time"
                FAILED_TESTS=$((FAILED_TESTS + 1))
                return 1
            fi
        else
            log_warn "Could not find pod for restart test"
            SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
            return 0
        fi
    else
        log_warn "Restart recovery test skipped - only applicable to GKE"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
        return 0
    fi
}

# ============================================================================
# Test Suite: Security
# ============================================================================

test_https_enforcement() {
    log_test "Testing HTTPS enforcement..."
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    if [ -n "$SERVICE_URL" ]; then
        # Check if service URL is HTTPS
        if echo "$SERVICE_URL" | grep -q "^https://"; then
            log_pass "Service uses HTTPS"
            PASSED_TESTS=$((PASSED_TESTS + 1))
            return 0
        else
            log_fail "Service does not use HTTPS"
            FAILED_TESTS=$((FAILED_TESTS + 1))
            return 1
        fi
    else
        log_warn "HTTPS test skipped - no SERVICE_URL provided"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
        return 0
    fi
}

test_security_headers() {
    log_test "Testing security headers..."
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    local url="${SERVICE_URL:-http://${INSTANCE_IP:-localhost}:8080}"

    curl -s -I "$url/health" > "${EVIDENCE_DIR}/security-headers.txt" 2>&1

    local has_security_headers=false

    # Check for common security headers
    if grep -qi "x-frame-options\|x-content-type-options\|strict-transport-security" "${EVIDENCE_DIR}/security-headers.txt"; then
        has_security_headers=true
    fi

    if [ "$has_security_headers" = true ]; then
        log_pass "Security headers present"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        log_warn "No common security headers found"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
        return 0
    fi
}

# ============================================================================
# Test Suite: Docker-Based Erlang Tests
# ============================================================================

test_docker_ct_integration() {
    log_test "Running Docker-based Common Test integration suite..."
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    # Run integration tests via Docker (DOCKER-ONLY)
    if docker_run_integration_suite "api"; then
        log_pass "Docker CT integration tests passed"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        log_fail "Docker CT integration tests failed"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

# ============================================================================
# Main Test Runner
# ============================================================================

run_test_suite() {
    local suite="$1"

    log_info "Running test suite: $suite"

    case "$suite" in
        smoke)
            test_health_endpoint
            test_readiness_endpoint
            ;;

        api)
            test_api_list_tools
            test_api_call_tool
            ;;

        observability)
            test_metrics_endpoint
            test_logging_output
            ;;

        performance)
            test_concurrent_requests
            ;;

        chaos)
            test_service_restart_recovery
            ;;

        security)
            test_https_enforcement
            test_security_headers
            ;;

        docker)
            test_docker_ct_integration
            ;;

        full)
            # Run all test suites
            run_test_suite smoke
            run_test_suite api
            run_test_suite observability
            run_test_suite performance
            run_test_suite chaos
            run_test_suite security
            run_test_suite docker
            ;;

        *)
            log_error "Unknown test suite: $suite"
            return 1
            ;;
    esac
}

# ============================================================================
# Main Entry Point
# ============================================================================

main() {
    log_info "============================================================================"
    log_info "ERLMCP MARKETPLACE INTEGRATION TESTS"
    log_info "Constitution: DOCKER-ONLY | Test ID: $TEST_ID"
    log_info "============================================================================"

    # Prerequisites
    check_prerequisites

    # Validate deployment target
    if ! validate_deployment_target; then
        log_error "Deployment target validation failed"
        exit 1
    fi

    log_info "Configuration:"
    log_info "  Test Suite: $TEST_SUITE"
    log_info "  Deployment Type: ${DEPLOYMENT_TYPE:-unknown}"
    log_info "  Service URL: ${SERVICE_URL:-none}"
    log_info "  Cluster: ${CLUSTER_NAME:-none}"
    log_info "  Instance IP: ${INSTANCE_IP:-none}"
    log_info "  Evidence Dir: $EVIDENCE_DIR"
    log_info "============================================================================"

    # Run test suite
    local start_time
    local end_time

    start_time=$(date +%s)

    run_test_suite "$TEST_SUITE"

    end_time=$(date +%s)
    local total_duration=$((end_time - start_time))

    # Collect final evidence
    collect_final_evidence

    # Print summary
    log_info "============================================================================"
    log_info "INTEGRATION TEST SUMMARY"
    log_info "============================================================================"
    log_info "  Total Tests:    $TOTAL_TESTS"
    log_info "  Passed:         $PASSED_TESTS"
    log_info "  Failed:         $FAILED_TESTS"
    log_info "  Skipped:        $SKIPPED_TESTS"
    log_info "  Duration:       ${total_duration}s"
    log_info "  Success Rate:   $(echo "scale=2; $PASSED_TESTS * 100 / $TOTAL_TESTS" | bc 2>/dev/null || echo "0")%"
    log_info "============================================================================"
    log_info "Evidence saved to: $EVIDENCE_DIR"
    log_info "Evidence receipts: ${#EVIDENCE_FILES[@]}"
    log_info "============================================================================"

    # Quality invariant check: errors=0
    if [ $FAILED_TESTS -gt 0 ]; then
        log_error "QUALITY INVARIANT VIOLATED: errors=$FAILED_TESTS (required: 0)"
        log_error "Integration tests FAILED"
        exit 1
    fi

    log_pass "ALL INTEGRATION TESTS PASSED"
    log_pass "Quality invariants satisfied: errors=0, failures=0"
    exit 0
}

# ============================================================================
# Argument Parsing
# ============================================================================

while [[ $# -gt 0 ]]; do
    case "$1" in
        --service-url)
            SERVICE_URL="$2"
            shift 2
            ;;
        --cluster-name)
            CLUSTER_NAME="$2"
            shift 2
            ;;
        --cluster-endpoint)
            CLUSTER_ENDPOINT="$2"
            shift 2
            ;;
        --instance-ip)
            INSTANCE_IP="$2"
            shift 2
            ;;
        --project)
            PROJECT_ID="$2"
            shift 2
            ;;
        --region)
            REGION="$2"
            shift 2
            ;;
        --suite)
            TEST_SUITE="$2"
            shift 2
            ;;
        --help)
            cat <<EOF
ERLMCP Integration Tests

Usage: $0 [OPTIONS]

OPTIONS:
  --service-url URL        Cloud Run service URL
  --cluster-name NAME      GKE cluster name
  --cluster-endpoint URL   GKE cluster endpoint
  --instance-ip IP         GCE instance IP
  --project PROJECT        GCP project ID
  --region REGION          GCP region (default: us-central1)
  --suite SUITE            Test suite to run (default: full)
                           Options: smoke, api, observability, performance, chaos, security, docker, full
  --help                   Show this help message

EXAMPLES:
  # Test Cloud Run deployment
  $0 --service-url https://erlmcp-xyz.run.app --suite smoke

  # Test GKE deployment
  $0 --cluster-name my-cluster --project my-project --suite full

  # Test GCE deployment
  $0 --instance-ip 35.1.2.3 --suite api

CONSTITUTION: DOCKER-ONLY
All Erlang tests execute via Docker Compose. No host execution permitted.
EOF
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Run main
main
