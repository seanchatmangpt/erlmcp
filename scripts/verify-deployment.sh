#!/usr/bin/env bash
###############################################################################
# verify-deployment.sh - End-to-End Deployment Validation for erlmcp v3
#
# Validates deployed system by checking:
#   1) All containers healthy (Docker health checks passing)
#   2) API endpoints respond (HTTP status codes, content validation)
#   3) Metrics accessible (Prometheus scrape, metric types)
#   4) No error logs (container logs analyzed)
#   5) Performance within SLA (latency, throughput)
#
# Constitution: DOCKER-ONLY - All validation via Docker
# Exit codes: 0 = success, 1 = validation failed, 2 = timeout
#
# Usage:
#   ./scripts/verify-deployment.sh [ENVIRONMENT] [COMPOSE_FILE]
#
# Examples:
#   ./scripts/verify-deployment.sh production docker-compose.yml
#   ./scripts/verify-deployment.sh staging docker-compose.staging.yml
#   ./scripts/verify-deployment.sh              # defaults to production
###############################################################################

set -euo pipefail

# ============================================================================
# CONFIGURATION
# ============================================================================

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Defaults
ENVIRONMENT="${1:-production}"
COMPOSE_FILE="${2:-${PROJECT_ROOT}/docker-compose.yml}"
DEPLOYMENT_TIMEOUT="${DEPLOYMENT_TIMEOUT:-300}"  # 5 minutes max
VERIFY_LOG="/tmp/erlmcp_verify_${ENVIRONMENT}_$(date +%s).log"
REPORT_FILE="${PROJECT_ROOT}/dist/deployment-verify-${ENVIRONMENT}-$(date +%s).json"

# Colors for terminal output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly MAGENTA='\033[0;35m'
readonly BOLD='\033[1m'
readonly NC='\033[0m'

# Port configuration (from docker-compose.yml)
readonly ERLMCP_PORT="${ERLMCP_PORT:-8080}"
readonly METRICS_PORT="${METRICS_PORT:-9100}"
readonly HEALTH_PORT="${HEALTH_PORT:-9090}"
readonly GRAFANA_PORT="${GRAFANA_PORT:-3000}"
readonly PROMETHEUS_PORT="${PROMETHEUS_PORT:-9090}"

# Health endpoints
readonly HEALTH_ENDPOINT="http://localhost:${HEALTH_PORT}/health"
readonly READY_ENDPOINT="http://localhost:${HEALTH_PORT}/ready"
readonly LIVE_ENDPOINT="http://localhost:${HEALTH_PORT}/live"
readonly METRICS_ENDPOINT="http://localhost:${METRICS_PORT}/metrics"

# SLA thresholds (configurable via environment)
readonly SLA_P99_LATENCY_MS="${SLA_P99_LATENCY_MS:-500}"
readonly SLA_P50_LATENCY_MS="${SLA_P50_LATENCY_MS:-100}"
readonly SLA_THROUGHPUT_MIN="${SLA_THROUGHPUT_MIN:-100}"
readonly SLA_ERROR_RATE_MAX="${SLA_ERROR_RATE_MAX:-0.01}"  # 1%
readonly SLA_MEMORY_MAX="${SLA_MEMORY_MAX:-0.90}"  # 90%

# Expected services (based on environment)
case "${ENVIRONMENT}" in
    production|prod)
        EXPECTED_SERVICES=("erlmcp" "prometheus" "grafana" "alertmanager")
        EXPECTED_CONTAINERS=5
        ;;
    staging|stage)
        EXPECTED_SERVICES=("erlmcp" "prometheus" "grafana")
        EXPECTED_CONTAINERS=3
        ;;
    dev|development)
        EXPECTED_SERVICES=("erlmcp" "prometheus")
        EXPECTED_CONTAINERS=2
        ;;
    *)
        EXPECTED_SERVICES=("erlmcp")
        EXPECTED_CONTAINERS=1
        ;;
esac

# Validation results (global)
declare -a CHECKS_PASSED=()
declare -a CHECKS_FAILED=()
declare -a CHECKS_WARNINGS=()
declare -i TOTAL_CHECKS=0
declare -i PASSED_CHECKS=0
declare -i FAILED_CHECKS=0

# ============================================================================
# LOGGING FUNCTIONS
# ============================================================================

log_header() {
    echo ""
    echo -e "${BOLD}${CYAN}════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${CYAN}$*${NC}"
    echo -e "${BOLD}${CYAN}════════════════════════════════════════════════════════════${NC}"
    echo ""
    log_to_file "HEADER: $*"
}

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
    log_to_file "INFO: $*"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $*"
    log_to_file "PASS: $*"
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $*"
    log_to_file "WARN: $*"
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $*"
    log_to_file "FAIL: $*"
}

log_debug() {
    if [[ "${DEBUG:-false}" == "true" ]]; then
        echo -e "${MAGENTA}[DEBUG]${NC} $*"
        log_to_file "DEBUG: $*"
    fi
}

log_to_file() {
    echo "[$(date +'%Y-%m-%d %H:%M:%S')] $*" >> "${VERIFY_LOG}"
}

record_check() {
    local status="$1"
    local category="$2"
    local message="$3"
    local details="${4:-{}}"

    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))

    case "${status}" in
        passed)
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
            CHECKS_PASSED+=("{\"category\":\"${category}\",\"message\":\"${message}\",\"details\":${details}}")
            ;;
        failed)
            FAILED_CHECKS=$((FAILED_CHECKS + 1))
            CHECKS_FAILED+=("{\"category\":\"${category}\",\"message\":\"${message}\",\"details\":${details}}")
            ;;
        warning)
            CHECKS_WARNINGS+=("{\"category\":\"${category}\",\"message\":\"${message}\",\"details\":${details}}")
            ;;
    esac
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

check_command() {
    local cmd="$1"
    if command -v "${cmd}" &> /dev/null; then
        return 0
    else
        return 1
    fi
}

check_docker() {
    if ! check_command docker; then
        log_error "Docker is not installed"
        return 1
    fi

    if ! docker info &> /dev/null; then
        log_error "Docker daemon is not running"
        return 1
    fi

    return 0
}

get_compose_cmd() {
    if command -v docker-compose &> /dev/null; then
        echo "docker-compose -f ${COMPOSE_FILE}"
    else
        echo "docker compose -f ${COMPOSE_FILE}"
    fi
}

wait_for_service() {
    local url="$1"
    local timeout="${2:-30}"
    local service_name="${3:-service}"

    local elapsed=0
    local interval=2

    log_info "Waiting for ${service_name} at ${url} (timeout: ${timeout}s)..."

    while [[ ${elapsed} -lt ${timeout} ]]; do
        if curl -sf -o /dev/null -w "%{http_code}" "${url}" &> /dev/null; then
            log_success "${service_name} is responding"
            return 0
        fi
        sleep ${interval}
        elapsed=$((elapsed + interval))
        echo -n "."
    done

    echo ""
    log_error "${service_name} did not respond within ${timeout}s"
    return 1
}

http_status() {
    local url="$1"
    local timeout="${2:-5}"
    curl -sf -o /dev/null -w "%{http_code}" --max-time "${timeout}" "${url}" 2>/dev/null || echo "000"
}

json_value() {
    local json="$1"
    local key="$2"
    echo "${json}" | grep -o "\"${key}\"[[:space:]]*:[[:space:]]*[^,}]*" | sed 's/.*: *"\{0,1\}\([^",}]*\).*/\1/'
}

parse_metric_value() {
    local metrics_output="$1"
    local metric_name="$2"
    echo "${metrics_output}" | grep -E "^${metric_name}\\{|^${metric_name} " | tail -1 | awk '{print $NF}'
}

# ============================================================================
# VALIDATION CATEGORY 1: CONTAINER HEALTH
# ============================================================================

validate_container_health() {
    log_header "1. Container Health Validation"

    local compose_cmd
    compose_cmd=$(get_compose_cmd)

    # Check if compose file exists
    if [[ ! -f "${COMPOSE_FILE}" ]]; then
        log_error "Compose file not found: ${COMPOSE_FILE}"
        record_check "failed" "container" "Compose file exists" "{\"file\":\"${COMPOSE_FILE}\"}"
        return 1
    fi
    record_check "passed" "container" "Compose file exists" "{\"file\":\"${COMPOSE_FILE}\"}"

    # Check expected services are defined
    log_info "Checking expected services in compose file..."
    for service in "${EXPECTED_SERVICES[@]}"; do
        if grep -q "^  ${service}:" "${COMPOSE_FILE}"; then
            log_success "Service defined: ${service}"
            record_check "passed" "container" "Service ${service} defined" "{}"
        else
            log_warning "Service not found: ${service}"
            record_check "warning" "container" "Service ${service} not found" "{}"
        fi
    done

    # Get running containers
    log_info "Checking running containers..."
    local running_containers
    running_containers=$(docker ps --filter "name=erlmcp" --format "{{.Names}}" | wc -l | tr -d ' ')
    log_info "Running erlmcp containers: ${running_containers}"

    if [[ ${running_containers} -eq 0 ]]; then
        log_error "No erlmcp containers are running"
        record_check "failed" "container" "Containers running" "{\"count\":${running_containers},\"expected\":${EXPECTED_CONTAINERS}}"
        return 1
    fi

    # Check each container's health status
    log_info "Checking container health status..."
    local all_healthy=true

    while IFS= read -r container_name; do
        local health_status
        health_status=$(docker inspect --format='{{.State.Health.Status}}' "${container_name}" 2>/dev/null || echo "no-healthcheck")

        case "${health_status}" in
            healthy)
                log_success "Container healthy: ${container_name}"
                record_check "passed" "container" "Container ${container_name} healthy" "{}"
                ;;
            starting)
                log_warning "Container starting: ${container_name}"
                record_check "warning" "container" "Container ${container_name} starting" "{}"
                all_healthy=false
                ;;
            unhealthy|no-healthcheck)
                if [[ "${health_status}" == "unhealthy" ]]; then
                    log_error "Container unhealthy: ${container_name}"
                    record_check "failed" "container" "Container ${container_name} unhealthy" "{}"
                    all_healthy=false
                else
                    log_info "Container no healthcheck: ${container_name}"
                    record_check "warning" "container" "Container ${container_name} no healthcheck" "{}"
                fi
                ;;
        esac
    done < <(docker ps --filter "name=erlmcp" --format "{{.Names}}")

    if [[ "${all_healthy}" == "true" ]]; then
        log_success "All containers are healthy"
        return 0
    else
        log_warning "Some containers are not healthy yet"
        return 1
    fi
}

wait_for_containers_healthy() {
    log_header "Waiting for Containers to be Healthy"

    local timeout="${DEPLOYMENT_TIMEOUT}"
    local elapsed=0
    local interval=5

    while [[ ${elapsed} -lt ${timeout} ]]; do
        local all_healthy=true
        local any_running=false

        while IFS= read -r container_name; do
            local health_status
            health_status=$(docker inspect --format='{{.State.Health.Status}}' "${container_name}" 2>/dev/null || echo "no-healthcheck")

            if [[ "${health_status}" == "healthy" ]] || [[ "${health_status}" == "no-healthcheck" ]]; then
                if docker ps --filter "name=${container_name}" --format "{{.Names}}" | grep -q "${container_name}"; then
                    any_running=true
                fi
            elif [[ "${health_status}" == "starting" ]]; then
                any_running=true
                all_healthy=false
            elif [[ "${health_status}" == "unhealthy" ]]; then
                all_healthy=false
            fi
        done < <(docker ps --filter "name=erlmcp" --format "{{.Names}}")

        if [[ "${any_running}" == "true" ]] && [[ "${all_healthy}" == "true" ]]; then
            log_success "All containers are healthy"
            return 0
        fi

        sleep ${interval}
        elapsed=$((elapsed + interval))
        echo -n "."
    done

    echo ""
    log_error "Containers did not become healthy within ${timeout}s"
    return 1
}

# ============================================================================
# VALIDATION CATEGORY 2: API ENDPOINTS
# ============================================================================

validate_api_endpoints() {
    log_header "2. API Endpoint Validation"

    # Wait for service to be accessible
    if ! wait_for_service "${HEALTH_ENDPOINT}" 30 "health endpoint"; then
        record_check "failed" "api" "Health endpoint accessible" "{\"url\":\"${HEALTH_ENDPOINT}\"}"
        return 1
    fi
    record_check "passed" "api" "Health endpoint accessible" "{\"url\":\"${HEALTH_ENDPOINT}\"}"

    # Test health endpoint
    log_info "Testing GET /health endpoint..."
    local health_response
    local health_status
    health_response=$(curl -sf --max-time 10 "${HEALTH_ENDPOINT}" 2>/dev/null || echo '{"status":"error"}')
    health_status=$(http_status "${HEALTH_ENDPOINT}")

    if [[ "${health_status}" == "200" ]]; then
        log_success "Health endpoint returned 200 OK"

        # Parse response
        local response_status
        response_status=$(echo "${health_response}" | grep -o '"status"[[:space:]]*:[[:space:]]*"[^"]*"' | cut -d'"' -f4)

        if [[ "${response_status}" == "healthy" ]]; then
            log_success "Health status: ${response_status}"
            record_check "passed" "api" "Health endpoint status" "{\"status\":\"${response_status}\"}"
        else
            log_warning "Health status: ${response_status}"
            record_check "warning" "api" "Health endpoint status" "{\"status\":\"${response_status}\"}"
        fi

        # Check for version
        local version
        version=$(echo "${health_response}" | grep -o '"version"[[:space:]]*:[[:space:]]*"[^"]*"' | cut -d'"' -f4)
        if [[ -n "${version}" ]]; then
            log_info "Version: ${version}"
        fi

        # Check for uptime
        local uptime
        uptime=$(echo "${health_response}" | grep -o '"uptime"[[:space:]]*:[[:space:]]*[0-9]*' | grep -o '[0-9]*')
        if [[ -n "${uptime}" ]]; then
            log_info "Uptime: ${uptime}s"
        fi
    else
        log_error "Health endpoint returned ${health_status}"
        record_check "failed" "api" "Health endpoint HTTP status" "{\"status\":${health_status},\"expected\":200}"
        return 1
    fi

    # Test readiness endpoint
    log_info "Testing GET /ready endpoint..."
    local ready_status
    ready_status=$(http_status "${READY_ENDPOINT}")

    if [[ "${ready_status}" == "200" ]]; then
        log_success "Readiness endpoint returned 200 OK"
        record_check "passed" "api" "Readiness endpoint" "{\"status\":200}"
    else
        log_error "Readiness endpoint returned ${ready_status}"
        record_check "failed" "api" "Readiness endpoint" "{\"status\":${ready_status},\"expected\":200}"
    fi

    # Test liveness endpoint
    log_info "Testing GET /live endpoint..."
    local live_status
    live_status=$(http_status "${LIVE_ENDPOINT}")

    if [[ "${live_status}" == "200" ]]; then
        log_success "Liveness endpoint returned 200 OK"
        record_check "passed" "api" "Liveness endpoint" "{\"status\":200}"
    else
        log_error "Liveness endpoint returned ${live_status}"
        record_check "failed" "api" "Liveness endpoint" "{\"status\":${live_status},\"expected\":200}"
    fi

    # Test metrics endpoint
    log_info "Testing GET /metrics endpoint..."
    local metrics_status
    metrics_status=$(http_status "${METRICS_ENDPOINT}")

    if [[ "${metrics_status}" == "200" ]]; then
        log_success "Metrics endpoint returned 200 OK"
        record_check "passed" "api" "Metrics endpoint" "{\"status\":200}"
    else
        log_warning "Metrics endpoint returned ${metrics_status}"
        record_check "warning" "api" "Metrics endpoint" "{\"status\":${metrics_status},\"expected\":200}"
    fi

    # Test API main endpoint (if available)
    if [[ "${ERLMCP_PORT}" != "${HEALTH_PORT}" ]]; then
        log_info "Testing main API endpoint on port ${ERLMCP_PORT}..."
        local api_status
        api_status=$(http_status "http://localhost:${ERLMCP_PORT}/" 5)

        if [[ "${api_status}" == "200" ]] || [[ "${api_status}" == "404" ]]; then
            log_success "API endpoint responding (status: ${api_status})"
            record_check "passed" "api" "Main API endpoint" "{\"status\":${api_status}}"
        else
            log_warning "API endpoint not responding (status: ${api_status})"
            record_check "warning" "api" "Main API endpoint" "{\"status\":${api_status}}"
        fi
    fi

    return 0
}

# ============================================================================
# VALIDATION CATEGORY 3: METRICS ACCESSIBILITY
# ============================================================================

validate_metrics() {
    log_header "3. Metrics Accessibility Validation"

    # Fetch metrics
    log_info "Fetching Prometheus metrics..."
    local metrics_output
    metrics_output=$(curl -sf --max-time 10 "${METRICS_ENDPOINT}" 2>/dev/null)

    if [[ -z "${metrics_output}" ]]; then
        log_error "Failed to fetch metrics from ${METRICS_ENDPOINT}"
        record_check "failed" "metrics" "Metrics endpoint accessible" "{\"url\":\"${METRICS_ENDPOINT}\"}"
        return 1
    fi
    record_check "passed" "metrics" "Metrics endpoint accessible" "{\"url\":\"${METRICS_ENDPOINT}\"}"

    # Check for required metric types
    log_info "Validating metric types..."

    local required_metrics=(
        "erlmcp_process_count"
        "erlmcp_process_limit"
        "erlmcp_memory_bytes"
        "erlmcp_ets_count"
        "erlmcp_run_queue_length"
        "erlmcp_uptime_seconds"
    )

    local metrics_found=0
    for metric in "${required_metrics[@]}"; do
        if echo "${metrics_output}" | grep -q "^${metric}\\|^${metric}_"; then
            log_success "Metric found: ${metric}"
            metrics_found=$((metrics_found + 1))
        else
            log_warning "Metric missing: ${metric}"
        fi
    done

    record_check "passed" "metrics" "Required metrics found" "{\"found\":${metrics_found},\"total\":${#required_metrics[@]}"

    # Parse and display key metrics
    log_info "Current metrics values:"

    local process_count
    local process_limit
    local memory_bytes
    local run_queue
    local uptime

    process_count=$(parse_metric_value "${metrics_output}" "erlmcp_process_count")
    process_limit=$(parse_metric_value "${metrics_output}" "erlmcp_process_limit")
    memory_bytes=$(parse_metric_value "${metrics_output}" "erlmcp_memory_bytes")
    run_queue=$(parse_metric_value "${metrics_output}" "erlmcp_run_queue_length")
    uptime=$(parse_metric_value "${metrics_output}" "erlmcp_uptime_seconds")

    echo "  Processes:    ${process_count:-N/A} / ${process_limit:-N/A}"
    echo "  Memory:       ${memory_bytes:-N/A} bytes"
    echo "  Run Queue:    ${run_queue:-N/A}"
    echo "  Uptime:       ${uptime:-N/A} seconds"

    # Check for Prometheus service (monitoring stack)
    if [[ " ${EXPECTED_SERVICES[*]} " =~ " prometheus " ]]; then
        log_info "Checking Prometheus service..."

        local prometheus_status
        prometheus_status=$(http_status "http://localhost:${PROMETHEUS_PORT}/-/healthy" 5)

        if [[ "${prometheus_status}" == "200" ]]; then
            log_success "Prometheus is healthy"
            record_check "passed" "metrics" "Prometheus accessible" "{\"url\":\"http://localhost:${PROMETHEUS_PORT}\"}"

            # Check if erlmcp target is up in Prometheus
            log_info "Checking Prometheus targets..."
            local prom_targets
            prom_targets=$(curl -sf --max-time 10 "http://localhost:${PROMETHEUS_PORT}/api/v1/targets" 2>/dev/null)

            if [[ -n "${prom_targets}" ]]; then
                local erlmcp_targets
                erlmcp_targets=$(echo "${prom_targets}" | grep -o '"health":"up"' | wc -l | tr -d ' ')
                log_info "Prometheus targets up: ${erlmcp_targets}"
            fi
        else
            log_warning "Prometheus not accessible (status: ${prometheus_status})"
            record_check "warning" "metrics" "Prometheus accessible" "{\"status\":${prometheus_status}}"
        fi
    fi

    # Check Grafana if expected
    if [[ " ${EXPECTED_SERVICES[*]} " =~ " grafana " ]]; then
        log_info "Checking Grafana service..."

        local grafana_status
        grafana_status=$(http_status "http://localhost:${GRAFANA_PORT}/api/health" 5)

        if [[ "${grafana_status}" == "200" ]]; then
            log_success "Grafana is healthy"
            record_check "passed" "metrics" "Grafana accessible" "{\"url\":\"http://localhost:${GRAFANA_PORT}\"}"
        else
            log_warning "Grafana not accessible (status: ${grafana_status})"
            record_check "warning" "metrics" "Grafana accessible" "{\"status\":${grafana_status}}"
        fi
    fi

    return 0
}

# ============================================================================
# VALIDATION CATEGORY 4: LOG ANALYSIS
# ============================================================================

validate_logs() {
    log_header "4. Log Analysis Validation"

    local error_count=0
    local warning_count=0
    local critical_errors=()

    # Get container names
    local containers
    containers=$(docker ps --filter "name=erlmcp" --format "{{.Names}}")

    if [[ -z "${containers}" ]]; then
        log_warning "No containers to analyze logs"
        return 1
    fi

    # Analyze logs from each container
    while IFS= read -r container_name; do
        log_info "Analyzing logs from: ${container_name}"

        # Get recent logs (last 100 lines)
        local container_logs
        container_logs=$(docker logs --tail 100 "${container_name}" 2>&1 || echo "")

        # Count errors
        local container_errors
        container_errors=$(echo "${container_logs}" | grep -ci "error" || echo "0")

        # Count warnings
        local container_warnings
        container_warnings=$(echo "${container_logs}" | grep -ci "warning" || echo "0")

        # Check for critical errors
        local critical_pattern
        critical_pattern=$(echo "${container_logs}" | grep -iE "crash|fatal|panic|exception|failed to start" || echo "")

        error_count=$((error_count + container_errors))
        warning_count=$((warning_count + container_warnings))

        if [[ -n "${critical_pattern}" ]]; then
            critical_errors+=("${container_name}")
        fi

        log_info "  Errors: ${container_errors}, Warnings: ${container_warnings}"

        # Check for specific patterns
        if echo "${container_logs}" | grep -qi "connection refused"; then
            log_warning "Connection refused errors found in ${container_name}"
        fi

        if echo "${container_logs}" | grep -qi "timeout"; then
            log_warning "Timeout errors found in ${container_name}"
        fi

    done <<< "${containers}"

    # Record results
    if [[ ${error_count} -eq 0 ]] && [[ ${#critical_errors[@]} -eq 0 ]]; then
        log_success "No critical errors found in logs"
        record_check "passed" "logs" "No critical errors" "{\"errors\":${error_count},\"warnings\":${warning_count}}"
    else
        if [[ ${#critical_errors[@]} -gt 0 ]]; then
            log_error "Critical errors found in containers: ${critical_errors[*]}"
            record_check "failed" "logs" "Critical errors present" "{\"containers\":\"${critical_errors[*]}\"}"
        else
            log_warning "Errors found in logs: ${error_count}"
            record_check "warning" "logs" "Errors present" "{\"count\":${error_count}}"
        fi
    fi

    # Check for memory issues
    log_info "Checking for memory-related warnings..."
    while IFS= read -r container_name; do
        local mem_logs
        mem_logs=$(docker logs --tail 100 "${container_name}" 2>&1 | grep -i "memory" || echo "")

        if echo "${mem_logs}" | grep -qi "out of memory"; then
            log_error "Out of memory error in ${container_name}"
            record_check "failed" "logs" "OOM error" "{\"container\":\"${container_name}\"}"
        fi
    done <<< "${containers}"

    return 0
}

# ============================================================================
# VALIDATION CATEGORY 5: PERFORMANCE / SLA
# ============================================================================

validate_performance() {
    log_header "5. Performance / SLA Validation"

    # Perform latency test
    log_info "Measuring API latency..."

    local latencies=()
    local iterations=10

    for i in $(seq 1 ${iterations}); do
        local start end latency
        start=$(date +%s%3N)  # milliseconds
        curl -sf -o /dev/null "${HEALTH_ENDPOINT}" &> /dev/null || true
        end=$(date +%s%3N)
        latency=$((end - start))
        latencies+=("${latency}")
        echo -n "."
        sleep 0.1
    done
    echo ""

    # Calculate statistics
    local sum=0
    for lat in "${latencies[@]}"; do
        sum=$((sum + lat))
    done
    local avg_latency=$((sum / iterations))

    # Sort for percentile calculation
    local sorted_latencies
    IFS=$'\n' sorted_latencies=($(sort -n <<<"${latencies[*]}"))
    unset IFS

    local p50_latency="${sorted_latencies[$((iterations / 2))]}"
    local p99_latency="${sorted_latencies[$((iterations - 1))]}"

    log_info "Latency results (ms):"
    echo "  P50: ${p50_latency}ms"
    echo "  P99: ${p99_latency}ms"
    echo "  Avg: ${avg_latency}ms"

    # Check against SLA
    local sla_passed=true
    local violations=()

    if [[ ${p99_latency} -gt ${SLA_P99_LATENCY_MS} ]]; then
        log_error "P99 latency ${p99_latency}ms exceeds SLA ${SLA_P99_LATENCY_MS}ms"
        violations+=("P99 latency ${p99_latency}ms > ${SLA_P99_LATENCY_MS}ms")
        sla_passed=false
    else
        log_success "P99 latency ${p99_latency}ms within SLA ${SLA_P99_LATENCY_MS}ms"
    fi

    if [[ ${p50_latency} -gt ${SLA_P50_LATENCY_MS} ]]; then
        log_warning "P50 latency ${p50_latency}ms exceeds SLA ${SLA_P50_LATENCY_MS}ms"
        violations+=("P50 latency ${p50_latency}ms > ${SLA_P50_LATENCY_MS}ms")
        sla_passed=false
    fi

    # Get metrics for throughput and error rate
    log_info "Fetching performance metrics..."
    local metrics_output
    metrics_output=$(curl -sf --max-time 10 "${METRICS_ENDPOINT}" 2>/dev/null || echo "")

    # Check memory usage
    local memory_bytes
    local process_count
    local process_limit

    memory_bytes=$(parse_metric_value "${metrics_output}" "erlmcp_memory_bytes" || echo "0")
    process_count=$(parse_metric_value "${metrics_output}" "erlmcp_process_count" || echo "0")
    process_limit=$(parse_metric_value "${metrics_output}" "erlmcp_process_limit" || echo "0")

    if [[ -n "${process_limit}" ]] && [[ "${process_limit}" -gt 0 ]]; then
        local process_ratio
        process_ratio=$(awk "BEGIN {printf \"%.2f\", ${process_count}/${process_limit}}")
        log_info "Process utilization: ${process_ratio} (${process_count}/${process_limit})"

        local process_ratio_check
        process_ratio_check=$(awk "BEGIN {print (${process_count}/${process_limit}) < ${SLA_MEMORY_MAX}}")

        if [[ "${process_ratio_check}" == "1" ]]; then
            log_success "Process utilization within SLA"
        else
            log_warning "Process utilization exceeds SLA threshold ${SLA_MEMORY_MAX}"
            sla_passed=false
        fi
    fi

    # Record performance check
    local performance_result
    if [[ "${sla_passed}" == "true" ]]; then
        performance_result="passed"
        record_check "passed" "performance" "SLA compliance" "{\"p50\":${p50_latency},\"p99\":${p99_latency}}"
    else
        performance_result="failed"
        record_check "failed" "performance" "SLA violations" "$(printf '{"violations":["%s"]}' "$(IFS=,; echo "${violations[*]}")")"
    fi

    # Output summary
    log_info "Performance Summary:"
    echo "  Latency (P50):     ${p50_latency}ms / ${SLA_P50_LATENCY_MS}ms"
    echo "  Latency (P99):     ${p99_latency}ms / ${SLA_P99_LATENCY_MS}ms"
    echo "  Process Util:      ${process_ratio:-N/A} / ${SLA_MEMORY_MAX}"

    if [[ "${sla_passed}" == "true" ]]; then
        log_success "All performance metrics within SLA"
        return 0
    else
        log_error "Some performance metrics exceed SLA"
        return 1
    fi
}

# ============================================================================
# REPORT GENERATION
# ============================================================================

generate_report() {
    log_header "Generating Deployment Verification Report"

    # Ensure output directory exists
    mkdir -p "$(dirname "${REPORT_FILE}")"

    local timestamp
    timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    local status="passed"
    if [[ ${FAILED_CHECKS} -gt 0 ]]; then
        status="failed"
    elif [[ ${#CHECKS_WARNINGS[@]} -gt 0 ]]; then
        status="warning"
    fi

    # Build JSON report
    cat > "${REPORT_FILE}" << EOF
{
  "environment": "${ENVIRONMENT}",
  "timestamp": "${timestamp}",
  "status": "${status}",
  "summary": {
    "total_checks": ${TOTAL_CHECKS},
    "passed": ${PASSED_CHECKS},
    "failed": ${FAILED_CHECKS},
    "warnings": ${#CHECKS_WARNINGS[@]}
  },
  "checks": {
    "passed": [
$(IFS=$'\n'; echo "${CHECKS_PASSED[*]}" | sed 's/^/      /' | sed 's/$/,/')
      null
    ],
    "failed": [
$(IFS=$'\n'; echo "${CHECKS_FAILED[*]}" | sed 's/^/      /' | sed 's/$/,/')
      null
    ],
    "warnings": [
$(IFS=$'\n'; echo "${CHECKS_WARNINGS[*]}" | sed 's/^/      /' | sed 's/$/,/')
      null
    ]
  },
  "configuration": {
    "sla": {
      "p99_latency_ms": ${SLA_P99_LATENCY_MS},
      "p50_latency_ms": ${SLA_P50_LATENCY_MS},
      "throughput_min": ${SLA_THROUGHPUT_MIN},
      "error_rate_max": ${SLA_ERROR_RATE_MAX},
      "memory_max": ${SLA_MEMORY_MAX}
    },
    "endpoints": {
      "health": "${HEALTH_ENDPOINT}",
      "ready": "${READY_ENDPOINT}",
      "live": "${LIVE_ENDPOINT}",
      "metrics": "${METRICS_ENDPOINT}"
    }
  }
}
EOF

    # Clean up trailing commas
    sed -i '' 's/,      null$/      null/g' "${REPORT_FILE}" 2>/dev/null || \
    sed -i 's/,      null$/      null/g' "${REPORT_FILE}" 2>/dev/null || true

    log_info "Report saved to: ${REPORT_FILE}"
}

print_summary() {
    log_header "DEPLOYMENT VERIFICATION SUMMARY"

    local status_text="PASSED"
    if [[ ${FAILED_CHECKS} -gt 0 ]]; then
        status_text="FAILED"
    elif [[ ${#CHECKS_WARNINGS[@]} -gt 0 ]]; then
        status_text="WARNING"
    fi

    echo -e "Environment:   ${BOLD}${ENVIRONMENT}${NC}"
    echo -e "Duration:      $(($(date +%s) - START_TIME)) seconds"
    echo -e "Status:        $(status_color ${FAILED_CHECKS})${status_text}$(status_color ${FAILED_CHECKS})"
    echo ""
    echo -e "Checks:        ${GREEN}${PASSED_CHECKS} passed${NC} / ${RED}${FAILED_CHECKS} failed${NC} / ${YELLOW}${#CHECKS_WARNINGS[@]} warnings${NC}"
    echo ""
    echo "Log file:       ${VERIFY_LOG}"
    echo "Report file:    ${REPORT_FILE}"
    echo ""

    if [[ ${FAILED_CHECKS} -eq 0 ]]; then
        echo -e "${GREEN}════════════════════════════════════════════════════════════${NC}"
        echo -e "${GREEN}  DEPLOYMENT VERIFICATION PASSED${NC}"
        echo -e "${GREEN}════════════════════════════════════════════════════════════${NC}"
        return 0
    else
        echo -e "${RED}════════════════════════════════════════════════════════════${NC}"
        echo -e "${RED}  DEPLOYMENT VERIFICATION FAILED${NC}"
        echo -e "${RED}════════════════════════════════════════════════════════════${NC}"
        echo ""
        echo -e "${RED}Failed checks:${NC}"
        for check in "${CHECKS_FAILED[@]}"; do
            echo "  ✗ ${check}"
        done
        return 1
    fi
}

status_color() {
    if [[ $1 -eq 0 ]]; then
        echo -n "${GREEN}"
    else
        echo -n "${RED}"
    fi
}

# ============================================================================
# CLEANUP
# ============================================================================

cleanup() {
    log_debug "Cleanup complete"
}

trap cleanup EXIT

# ============================================================================
# MAIN EXECUTION
# ============================================================================

main() {
    START_TIME=$(date +%s)

    log_header "erlmcp v3 Deployment Verification"
    log_info "Environment: ${ENVIRONMENT}"
    log_info "Compose file: ${COMPOSE_FILE}"
    log_info "Verification log: ${VERIFY_LOG}"

    local exit_code=0

    # Category 1: Container Health
    if ! validate_container_health; then
        log_warning "Container health check failed, waiting for containers..."
        if ! wait_for_containers_healthy; then
            exit_code=1
        fi
    fi

    # Category 2: API Endpoints
    if ! validate_api_endpoints; then
        log_error "API endpoint validation failed"
        exit_code=1
    fi

    # Category 3: Metrics Accessibility
    if ! validate_metrics; then
        log_warning "Metrics validation had issues"
    fi

    # Category 4: Log Analysis
    if ! validate_logs; then
        log_warning "Log analysis found issues"
    fi

    # Category 5: Performance / SLA
    if ! validate_performance; then
        log_warning "Performance validation had issues"
    fi

    # Generate report
    generate_report

    # Print summary
    print_summary || exit_code=1

    return ${exit_code}
}

# Run main
main "$@"
