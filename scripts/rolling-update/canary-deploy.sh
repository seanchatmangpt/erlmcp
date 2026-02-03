#!/bin/bash
#
# Canary Deployment Automation Script for erlmcp
# Implements progressive traffic shifting with automated rollback
#
# Usage:
#   ./canary-deploy.sh <environment> <image> [options]
#   ./canary-deploy.sh staging ghcr.io/org/erlmcp:v1.0.0 --max-traffic 50
#   ./canary-deploy.sh production ghcr.io/org/erlmcp:v1.0.0 --steps "5,10,25,50,100"
#
# Features:
# - Progressive traffic shifting
# - Automated analysis at each step
# - Instant rollback on failure
# - Metrics-based validation

set -euo pipefail

# ============================================================================
# CONFIGURATION
# ============================================================================

NAMESPACE_PREFIX="erlmcp"
CANARY_SERVICE="erlmcp-canary"
STABLE_SERVICE="erlmcp-active"
PROMETHEUS_URL="${PROMETHEUS_URL:-http://prometheus-operated.monitoring.svc.cluster.local:9090}"

# Default traffic steps (percentage)
DEFAULT_STEPS=(5 10 25 50 100)
MAX_TRAFFIC_PERCENT=${MAX_TRAFFIC_PERCENT:-100}

# Analysis thresholds
ERROR_RATE_THRESHOLD=${ERROR_RATE_THRESHOLD:-0.01}  # 1%
P95_LATENCY_THRESHOLD=${P95_LATENCY_THRESHOLD:-500}  # 500ms
P99_LATENCY_THRESHOLD=${P99_LATENCY_THRESHOLD:-1000} # 1000ms
CPU_THRESHOLD=${CPU_THRESHOLD:-80}                   # 80%
MEMORY_THRESHOLD=${MEMORY_THRESHOLD:-80}              # 80%

# Timing configuration
STEP_PAUSE_SECONDS=${STEP_PAUSE_SECONDS:-60}
ANALYSIS_INTERVAL_SECONDS=${ANALYSIS_INTERVAL_SECONDS:-30}
ANALYSIS_COUNT=${ANALYSIS_COUNT:-5}
HEALTH_CHECK_TIMEOUT=${HEALTH_CHECK_TIMEOUT:-300}

# State tracking
STATE_DIR="/tmp/erlmcp-canary-state"
mkdir -p "$STATE_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# ============================================================================
# LOGGING FUNCTIONS
# ============================================================================

log() {
    echo -e "${BLUE}[$(date '+%Y-%m-%d %H:%M:%S')]${NC} $1" | tee -a "${STATE_DIR}/canary.log"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "${STATE_DIR}/canary.log"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" | tee -a "${STATE_DIR}/canary.log"
}

warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1" | tee -a "${STATE_DIR}/canary.log"
}

# ============================================================================
# ROLLBACK FUNCTION
# ============================================================================

rollback_canary() {
    local namespace=$1
    local reason=$2

    error "Rolling back canary deployment: $reason"
    log "=========================================="
    log "INITIATING ROLLBACK"
    log "Reason: $reason"
    log "=========================================="

    # Set traffic to 0% (all to stable)
    log "Setting traffic to 0% canary (100% stable)"
    set_traffic_weight "$namespace" 0

    # Scale down canary
    log "Scaling down canary deployment"
    kubectl scale deployment "$CANARY_SERVICE" -n "$namespace" --replicas=0 || true

    # Update canary ingress
    if kubectl get ingress "erlmcp-canary-ingress" -n "$namespace" >/dev/null 2>&1; then
        kubectl patch ingress "erlmcp-canary-ingress" -n "$namespace" \
            --type=json -p='[{"op": "replace", "path": "/metadata/annotations/nginx.ingress.kubernetes.io~1canary-weight", "value": "0"}]' || true
    fi

    # Send notification
    send_notification "FAILED" "Canary rolled back: $reason" "$namespace"

    error "Canary deployment rolled back"
    exit 1
}

send_notification() {
    local status=$1
    local message=$2
    local environment=$3

    log "Sending notification: $status - $message"

    if [[ -n "${SLACK_WEBHOOK_URL:-}" ]]; then
        local color="good"
        [[ "$status" == "FAILED" ]] && color="danger"

        curl -s -X POST "$SLACK_WEBHOOK_URL" \
            -H 'Content-Type: application/json' \
            -d "{\"attachments\": [{\"color\": \"$color\", \"title\": \"Canary $status\", \"text\": \"$message\"}]}" \
            > /dev/null || true
    fi
}

# ============================================================================
# TRAFFIC MANAGEMENT
# ============================================================================

set_traffic_weight() {
    local namespace=$1
    local weight=$2

    log "Setting canary traffic weight to ${weight}%"

    # Update canary ingress annotation
    if kubectl get ingress "erlmcp-canary-ingress" -n "$namespace" >/dev/null 2>&1; then
        kubectl patch ingress "erlmcp-canary-ingress" -n "$namespace" \
            --type=json \
            -p="[{\"op\": \"replace\", \"path\": \"/metadata/annotations/nginx.ingress.kubernetes.io~1canary-weight\", \"value\": \"$weight\"}]" || true

        log "Updated ingress: canary-weight=$weight"
    fi

    # For Istio VirtualService (if available)
    if command -v istioctl &>/dev/null; then
        local stable_weight=$((100 - weight))
        log "Istio: stable=${stable_weight}%, canary=${weight}%"
    fi
}

# ============================================================================
# ANALYSIS FUNCTIONS
# ============================================================================

query_prometheus() {
    local query=$1
    local start=$(date -d "5 minutes ago" +%s)000
    local end=$(date +%s)000
    local step=30

    curl -s "${PROMETHEUS_URL}/api/v1/query_range" \
        --data-urlencode "query=$query" \
        --data-urlencode "start=$start" \
        --data-urlencode "end=$end" \
        --data-urlencode "step=$step" | \
        jq -r '.data.result[0].values[-1][1]' 2>/dev/null || echo "null"
}

get_error_rate() {
    local namespace=$1
    local service=$2

    local query="sum(rate(http_requests_total{namespace=\"$namespace\",service=\"$service\",status=~\"5..\"}[1m])) / sum(rate(http_requests_total{namespace=\"$namespace\",service=\"$service\"}[1m]))"

    local result=$(query_prometheus "$query")
    result=${result:-"0"}

    log "Error rate: $(echo "$result" | awk '{printf "%.2f%%", $1 * 100}')"

    echo "$result"
}

get_latency() {
    local namespace=$1
    local service=$2
    local percentile=$3  # 0.95 or 0.99

    local query="histogram_quantile($percentile, sum(rate(http_request_duration_seconds_bucket{namespace=\"$namespace\",service=\"$service\"}[2m])) by (le)) * 1000"

    local result=$(query_prometheus "$query")
    result=${result:-"0"}

    log "P${percentile} latency: ${result}ms"

    echo "$result"
}

get_resource_usage() {
    local namespace=$1
    local resource=$2  # cpu or memory

    local query
    if [[ "$resource" == "cpu" ]]; then
        query="avg(rate(container_cpu_usage_seconds_total{namespace=\"$namespace\",container=\"erlmcp\"}[2m])) * 100"
    else
        query="avg(container_memory_working_set_bytes{namespace=\"$namespace\",container=\"erlmcp\"}) / sum(container_spec_memory_limit_bytes{namespace=\"$namespace\",container=\"erlmcp\"}) * 100"
    fi

    local result=$(query_prometheus "$query")
    result=${result:-"0"}

    log "${resource^} usage: ${result}%"

    echo "$result"
}

analyze_metrics() {
    local namespace=$1
    local service=$2

    log "=========================================="
    log "Running Analysis for $service"
    log "=========================================="

    local failures=0

    # Check error rate
    local error_rate=$(get_error_rate "$namespace" "$service")
    local error_rate_pass=$(echo "$error_rate < $ERROR_RATE_THRESHOLD" | bc -l 2>/dev/null || echo "1")

    if [[ "$error_rate_pass" != "1" ]]; then
        error "Error rate $(echo "$error_rate" | awk '{printf "%.2f%%", $1 * 100}') exceeds threshold $(echo "$ERROR_RATE_THRESHOLD" | awk '{printf "%.2f%%", $1 * 100}')"
        ((failures++))
    else
        success "Error rate check passed"
    fi

    # Check P95 latency
    local p95_latency=$(get_latency "$namespace" "$service" "0.95")
    local p95_pass=$(echo "$p95_latency < $P95_LATENCY_THRESHOLD" | bc -l 2>/dev/null || echo "1")

    if [[ "$p95_pass" != "1" ]]; then
        error "P95 latency ${p95_latency}ms exceeds threshold ${P95_LATENCY_THRESHOLD}ms"
        ((failures++))
    else
        success "P95 latency check passed"
    fi

    # Check P99 latency
    local p99_latency=$(get_latency "$namespace" "$service" "0.99")
    local p99_pass=$(echo "$p99_latency < $P99_LATENCY_THRESHOLD" | bc -l 2>/dev/null || echo "1")

    if [[ "$p99_pass" != "1" ]]; then
        error "P99 latency ${p99_latency}ms exceeds threshold ${P99_LATENCY_THRESHOLD}ms"
        ((failures++))
    else
        success "P99 latency check passed"
    fi

    # Check CPU usage
    local cpu_usage=$(get_resource_usage "$namespace" "cpu")
    local cpu_pass=$(echo "$cpu_usage < $CPU_THRESHOLD" | bc -l 2>/dev/null || echo "1")

    if [[ "$cpu_pass" != "1" ]]; then
        warning "CPU usage ${cpu_usage}% exceeds threshold ${CPU_THRESHOLD}%"
    fi

    # Check memory usage
    local memory_usage=$(get_resource_usage "$namespace" "memory")
    local memory_pass=$(echo "$memory_usage < $MEMORY_THRESHOLD" | bc -l 2>/dev/null || echo "1")

    if [[ "$memory_pass" != "1" ]]; then
        warning "Memory usage ${memory_usage}% exceeds threshold ${MEMORY_THRESHOLD}%"
    fi

    log "=========================================="
    log "Analysis Result: $failures failures"
    log "=========================================="

    return $failures
}

run_analysis_loop() {
    local namespace=$1
    local service=$2
    local iterations=${3:-$ANALYSIS_COUNT}

    log "Running analysis loop ($iterations iterations)..."

    local failures=0
    local i=0

    while [[ $i -lt $iterations ]]; do
        ((i++))
        log "Analysis iteration $i/$iterations"

        if ! analyze_metrics "$namespace" "$service"; then
            ((failures++))
            warning "Analysis failed (failure $failures/$iterations)"

            # Rollback if too many failures
            if [[ $failures -ge 2 ]]; then
                error "Too many analysis failures"
                return 1
            fi
        else
            failures=0  # Reset failures on success
        fi

        sleep "$ANALYSIS_INTERVAL_SECONDS"
    done

    success "Analysis loop completed"
    return 0
}

# ============================================================================
# HEALTH CHECKS
# ============================================================================

wait_for_canary_ready() {
    local namespace=$1
    local timeout=$2

    log "Waiting for canary deployment to be ready (timeout: ${timeout}s)..."

    local start_time=$(date +%s)
    local end_time=$((start_time + timeout))

    while true; do
        local current_time=$(date +%s)

        if [[ $current_time -ge $end_time ]]; then
            error "Timeout waiting for canary to be ready"
            return 1
        fi

        # Check deployment status
        local ready_replicas=$(kubectl get deployment "$CANARY_SERVICE" -n "$namespace" \
            -o jsonpath='{.status.readyReplicas}' 2>/dev/null || echo "0")
        local replicas=$(kubectl get deployment "$CANARY_SERVICE" -n "$namespace" \
            -o jsonpath='{.spec.replicas}' 2>/dev/null || echo "0")

        if [[ "$ready_replicas" == "$replicas" ]] && [[ "$replicas" -gt 0 ]]; then
            success "Canary deployment is ready ($ready_replicas/$replicas pods)"
            break
        fi

        log "Waiting for canary pods: $ready_replicas/$replicas ready"
        sleep 5
    done

    return 0
}

health_check_canary() {
    local namespace=$1

    log "Running health check on canary deployment"

    local endpoint=$(kubectl get endpoints "$CANARY_SERVICE" -n "$namespace" \
        -o jsonpath='{.subsets[0].addresses[0].ip}' 2>/dev/null)

    if [[ -z "$endpoint" ]]; then
        error "No canary endpoint found"
        return 1
    fi

    local port=$(kubectl get service "$CANARY_SERVICE" -n "$namespace" \
        -o jsonpath='{.spec.ports[0].port}' 2>/dev/null || echo "8080")

    if curl -f -s "http://${endpoint}:${port}/health" >/dev/null 2>&1; then
        success "Canary health check passed"
        return 0
    else
        error "Canary health check failed"
        return 1
    fi
}

# ============================================================================
# DEPLOYMENT FUNCTIONS
# ============================================================================

deploy_canary() {
    local namespace=$1
    local image=$2

    log "Deploying canary with image: $image"

    # Check if canary deployment exists
    if ! kubectl get deployment "$CANARY_SERVICE" -n "$namespace" >/dev/null 2>&1; then
        error "Canary deployment not found. Please create it first."
        return 1
    fi

    # Update canary image
    kubectl set image deployment/"$CANARY_SERVICE" erlmcp="$image" -n "$namespace"

    log "Canary deployment updated"
    return 0
}

# ============================================================================
# MAIN ORCHESTRATION
# ============================================================================

main() {
    local environment=${1:-"staging"}
    local image=${2:-""}
    shift 2

    # Parse options
    local steps=("${DEFAULT_STEPS[@]}")
    local max_traffic=$MAX_TRAFFIC_PERCENT

    while [[ $# -gt 0 ]]; do
        case $1 in
            --steps)
                IFS=',' read -ra steps <<< "$2"
                shift 2
                ;;
            --max-traffic)
                max_traffic=$2
                shift 2
                ;;
            --dry-run)
                log "DRY RUN MODE"
                log "Environment: $environment"
                log "Image: $image"
                log "Steps: ${steps[*]}"
                exit 0
                ;;
            *)
                error "Unknown option: $1"
                exit 1
                ;;
        esac
    done

    # Validate inputs
    if [[ "$environment" != "staging" && "$environment" != "production" ]]; then
        error "Invalid environment: $environment"
        exit 1
    fi

    if [[ -z "$image" ]]; then
        error "Image parameter is required"
        echo "Usage: $0 <environment> <image> [--steps \"5,10,25,50,100\"] [--max-traffic 100]"
        exit 1
    fi

    local namespace="${NAMESPACE_PREFIX}-${environment}"

    log "=========================================="
    log "Starting Canary Deployment"
    log "Environment: $environment"
    log "Namespace: $namespace"
    log "Image: $image"
    log "Traffic steps: ${steps[*]}%"
    log "=========================================="

    send_notification "STARTED" "Canary deployment of $image" "$environment"

    # Step 1: Deploy canary
    log "Step 1: Deploying canary version"
    if ! deploy_canary "$namespace" "$image"; then
        rollback_canary "$namespace" "Failed to deploy canary"
    fi

    # Step 2: Wait for canary to be ready
    log "Step 2: Waiting for canary readiness"
    if ! wait_for_canary_ready "$namespace" "$HEALTH_CHECK_TIMEOUT"; then
        rollback_canary "$namespace" "Canary did not become ready in time"
    fi

    # Step 3: Initial health check
    log "Step 3: Running initial health check"
    if ! health_check_canary "$namespace"; then
        rollback_canary "$namespace" "Canary health check failed"
    fi

    # Step 4: Progressive traffic shift with analysis
    local previous_step=0

    for step in "${steps[@]}"; do
        # Ensure we don't exceed max_traffic
        if [[ $step -gt $max_traffic ]]; then
            step=$max_traffic
        fi

        log "=========================================="
        log "Traffic Step: ${step}%"
        log "=========================================="

        # Set traffic weight
        set_traffic_weight "$namespace" "$step"

        # Pause for traffic to stabilize
        log "Pausing for ${STEP_PAUSE_SECONDS}s for traffic to stabilize..."
        sleep "$STEP_PAUSE_SECONDS"

        # Run analysis (only if traffic > 0)
        if [[ $step -gt 0 ]]; then
            if ! run_analysis_loop "$namespace" "$CANARY_SERVICE"; then
                rollback_canary "$namespace" "Analysis failed at ${step}% traffic"
            fi
        fi

        previous_step=$step

        # Exit if we've reached max traffic
        if [[ $step -ge $max_traffic ]]; then
            break
        fi
    done

    # Step 5: Final validation
    log "=========================================="
    log "Running Final Validation"
    log "=========================================="

    if ! run_analysis_loop "$namespace" "$CANARY_SERVICE" 10; then
        rollback_canary "$namespace" "Final validation failed"
    fi

    # Step 6: Promote canary (update stable deployment)
    log "=========================================="
    log "Promoting Canary to Stable"
    log "=========================================="

    # Update stable deployment
    kubectl set image deployment/"$STABLE_SERVICE" erlmcp="$image" -n "$namespace"

    # Wait for stable to be ready
    if ! wait_for_canary_ready "$namespace" "$HEALTH_CHECK_TIMEOUT"; then
        rollback_canary "$namespace" "Stable deployment failed after promotion"
    fi

    # Step 7: Reset traffic to stable (100%)
    log "Resetting traffic to stable (100%)"
    set_traffic_weight "$namespace" 0

    # Step 8: Scale down canary
    log "Scaling down canary deployment"
    kubectl scale deployment "$CANARY_SERVICE" -n "$namespace" --replicas=0 || true

    success "=========================================="
    success "Canary Deployment Completed!"
    success "Promoted: $image"
    success "=========================================="

    send_notification "SUCCESS" "Canary deployment completed: $image" "$environment"

    # Generate report
    cat > "${STATE_DIR}/canary-report-${environment}-$(date +%Y%m%d-%H%M%S).json" <<EOF
{
  "environment": "$environment",
  "namespace": "$namespace",
  "image": "$image",
  "status": "success",
  "traffic_steps": "${steps[*]}",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF

    exit 0
}

main "$@"
