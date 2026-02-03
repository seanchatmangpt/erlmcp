#!/bin/bash
#
# Automated Rollback Script for erlmcp Deployments
# Monitors deployment health and triggers automatic rollback on failures
#
# Usage:
#   ./automated-rollback.sh <environment> [options]
#   ./automated-rollback.sh staging --monitor-only
#   ./automated-rollback.sh production --force-rollback
#
# Features:
# - Real-time health monitoring
# - Automatic rollback on threshold breach
# - Integration with Prometheus metrics
# - Alerting via Slack/Teams

set -euo pipefail

# ============================================================================
# CONFIGURATION
# ============================================================================

NAMESPACE_PREFIX="erlmcp"
ACTIVE_SERVICE="erlmcp-active"
PROMETHEUS_URL="${PROMETHEUS_URL:-http://prometheus-operated.monitoring.svc.cluster.local:9090}"

# Rollback thresholds
ERROR_RATE_THRESHOLD=${ERROR_RATE_THRESHOLD:-0.05}      # 5%
LATENCY_P95_THRESHOLD=${LATENCY_P95_THRESHOLD:-1000}     # 1000ms
LATENCY_P99_THRESHOLD=${LATENCY_P99_THRESHOLD:-2000}     # 2000ms
HEALTH_CHECK_FAILURE_THRESHOLD=${HEALTH_CHECK_FAILURE_THRESHOLD:-3}
CONSECUTIVE_FAILURES_THRESHOLD=${CONSECUTIVE_FAILURES_THRESHOLD:-3}

# Monitoring configuration
MONITOR_INTERVAL=${MONITOR_INTERVAL:-30}                  # seconds
GRACE_PERIOD=${GRACE_PERIOD:-120}                         # seconds before monitoring starts
HEALTH_CHECK_TIMEOUT=${HEALTH_CHECK_TIMEOUT:-10}          # seconds

# State tracking
STATE_DIR="/tmp/erlmcp-rollback-state"
mkdir -p "$STATE_DIR"
ROLLBACK_FLAG="${STATE_DIR}/rollback-in-progress"

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
    echo -e "${BLUE}[$(date '+%Y-%m-%d %H:%M:%S')]${NC} $1" | tee -a "${STATE_DIR}/rollback-monitor.log"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "${STATE_DIR}/rollback-monitor.log"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" | tee -a "${STATE_DIR}/rollback-monitor.log"
}

warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1" | tee -a "${STATE_DIR}/rollback-monitor.log"
}

# ============================================================================
# STATE MANAGEMENT
# ============================================================================

is_rollback_in_progress() {
    [[ -f "$ROLLBACK_FLAG" ]]
}

set_rollback_in_progress() {
    touch "$ROLLBACK_FLAG"
    echo "$(date +%s)" > "$ROLLBACK_FLAG"
}

clear_rollback_in_progress() {
    rm -f "$ROLLBACK_FLAG"
}

# ============================================================================
# ROLLBACK FUNCTION
# ============================================================================

execute_rollback() {
    local namespace=$1
    local reason=$2
    local trigger=$3

    if is_rollback_in_progress; then
        warning "Rollback already in progress, skipping"
        return 0
    fi

    set_rollback_in_progress

    log "=========================================="
    log "EXECUTING AUTOMATED ROLLBACK"
    log "=========================================="
    log "Namespace: $namespace"
    log "Reason: $reason"
    log "Trigger: $trigger"
    log "=========================================="

    send_notification "ROLLBACK_INITIATED" "$reason - $trigger" "$namespace"

    # Get current deployment info
    local current_image=$(kubectl get deployment -n "$namespace" -o jsonpath='{.items[0].spec.template.spec.containers[0].image}' 2>/dev/null || echo "unknown")
    local current_revision=$(kubectl rollout history deployment -n "$namespace" -o jsonpath='{.items[0].metadata.resourceVersion}' 2>/dev/null || echo "unknown")

    # Step 1: Get previous stable revision
    log "Finding previous stable revision..."
    local previous_revision=$(kubectl rollout history deployment -n "$namespace" | grep -E "^[0-9]+" | tail -2 | head -1 | awk '{print $1}' || echo "")

    if [[ -z "$previous_revision" ]]; then
        error "No previous revision found for rollback"
        clear_rollback_in_progress
        return 1
    fi

    log "Rolling back to revision: $previous_revision"

    # Step 2: Execute rollback
    log "Executing kubectl rollout undo..."
    if kubectl rollout undo deployment -n "$namespace" --to-revision="$previous_revision"; then
        success "Rollback command executed"
    else
        error "Rollback command failed"
        clear_rollback_in_progress
        return 1
    fi

    # Step 3: Wait for rollback to complete
    log "Waiting for rollback to complete..."
    if kubectl rollout status deployment -n "$namespace" --timeout=300s; then
        success "Rollback completed"
    else
        error "Rollback did not complete in time"
        clear_rollback_in_progress
        return 1
    fi

    # Step 4: Verify rollback health
    log "Verifying rollback health..."
    local retries=0
    local max_retries=10

    while [[ $retries -lt $max_retries ]]; do
        if check_deployment_health "$namespace"; then
            success "Rollback health verified"
            break
        fi

        ((retries++))
        log "Health check failed, retrying ($retries/$max_retries)..."
        sleep 10
    done

    if [[ $retries -ge $max_retries ]]; then
        error "Rollback health verification failed after $max_retries attempts"
        send_notification "ROLLBACK_FAILED" "Health verification failed" "$namespace"
        clear_rollback_in_progress
        return 1
    fi

    # Step 5: Generate rollback report
    generate_rollback_report "$namespace" "$current_image" "$previous_revision" "$reason" "$trigger"

    # Step 6: Send final notification
    send_notification "ROLLBACK_COMPLETED" "Successfully rolled back to revision $previous_revision" "$namespace"

    success "=========================================="
    success "AUTOMATED ROLLBACK COMPLETED"
    success "=========================================="

    clear_rollback_in_progress
    return 0
}

# ============================================================================
# NOTIFICATION FUNCTION
# ============================================================================

send_notification() {
    local status=$1
    local message=$2
    local environment=$3

    log "Sending notification: $status"

    if [[ -n "${SLACK_WEBHOOK_URL:-}" ]]; then
        local color="danger"
        [[ "$status" == "ROLLBACK_COMPLETED" ]] && color="warning"

        curl -s -X POST "$SLACK_WEBHOOK_URL" \
            -H 'Content-Type: application/json' \
            -d "{
                \"attachments\": [{
                    \"color\": \"$color\",
                    \"title\": \"Automated Rollback: $status\",
                    \"text\": \"$message\",
                    \"fields\": [
                        {\"title\": \"Environment\", \"value\": \"$environment\", \"short\": true},
                        {\"title\": \"Time\", \"value\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\", \"short\": true}
                    ]
                }]
            }" > /dev/null || true
    fi

    if [[ -n "${TEAMS_WEBHOOK_URL:-}" ]]; then
        curl -s -X POST "$TEAMS_WEBHOOK_URL" \
            -H 'Content-Type: application/json' \
            -d "{
                \"@type\": \"MessageCard\",
                \"title\": \"Automated Rollback: $status\",
                \"text\": \"$message\",
                \"sections\": [{
                    \"facts\": [
                        {\"name\": \"Environment\", \"value\": \"$environment\"}
                    ]
                }]
            }" > /dev/null || true
    fi
}

# ============================================================================
# METRICS COLLECTION FUNCTIONS
# ============================================================================

query_prometheus() {
    local query=$1

    curl -s "${PROMETHEUS_URL}/api/v1/query" \
        --data-urlencode "query=$query" | \
        jq -r '.data.result[0].value[1]' 2>/dev/null || echo "null"
}

get_error_rate() {
    local namespace=$1

    local query="sum(rate(http_requests_total{namespace=\"$namespace\",status=~\"5..\"}[1m])) / sum(rate(http_requests_total{namespace=\"$namespace\"}[1m]))"

    local result=$(query_prometheus "$query")
    result=${result:-"0"}

    echo "$result"
}

get_latency_p95() {
    local namespace=$1

    local query="histogram_quantile(0.95, sum(rate(http_request_duration_seconds_bucket{namespace=\"$namespace\"}[2m])) by (le)) * 1000"

    local result=$(query_prometheus "$query")
    result=${result:-"0"}

    echo "$result"
}

get_latency_p99() {
    local namespace=$1

    local query="histogram_quantile(0.99, sum(rate(http_request_duration_seconds_bucket{namespace=\"$namespace\"}[2m])) by (le)) * 1000"

    local result=$(query_prometheus "$query")
    result=${result:-"0"}

    echo "$result"
}

# ============================================================================
# HEALTH CHECK FUNCTIONS
# ============================================================================

check_deployment_health() {
    local namespace=$1

    # Check if deployment exists
    if ! kubectl get deployment -n "$namespace" >/dev/null 2>&1; then
        error "Deployment not found in $namespace"
        return 1
    fi

    # Check deployment status
    local available=$(kubectl get deployment -n "$namespace" -o jsonpath='{.items[0].status.conditions[?(@.type=="Available")].status}' 2>/dev/null || echo "False")

    if [[ "$available" != "True" ]]; then
        error "Deployment not available"
        return 1
    fi

    # Check pod readiness
    local ready_replicas=$(kubectl get deployment -n "$namespace" -o jsonpath='{.items[0].status.readyReplicas}' 2>/dev/null || echo "0")
    local replicas=$(kubectl get deployment -n "$namespace" -o jsonpath='{.items[0].spec.replicas}' 2>/dev/null || echo "1")

    if [[ "$ready_replicas" -lt "$replicas" ]]; then
        error "Pods not ready: $ready_replicas/$replicas"
        return 1
    fi

    # Check endpoint health
    local endpoint=$(kubectl get endpoints "$ACTIVE_SERVICE" -n "$namespace" -o jsonpath='{.subsets[0].addresses[0].ip}' 2>/dev/null)

    if [[ -n "$endpoint" ]]; then
        local port=$(kubectl get service "$ACTIVE_SERVICE" -n "$namespace" -o jsonpath='{.spec.ports[0].port}' 2>/dev/null || echo "8080")

        if curl -f -s --max-time "$HEALTH_CHECK_TIMEOUT" "http://${endpoint}:${port}/health" >/dev/null 2>&1; then
            return 0
        else
            error "Health check failed: $endpoint:$port"
            return 1
        fi
    fi

    return 1
}

# ============================================================================
# MONITORING LOOP
# ============================================================================

monitor_deployment() {
    local namespace=$1
    local monitor_only=${2:-false}

    log "=========================================="
    log "Starting Deployment Monitoring"
    log "Namespace: $namespace"
    log "Monitor Only: $monitor_only"
    log "Interval: ${MONITOR_INTERVAL}s"
    log "=========================================="

    # Grace period before starting monitoring
    log "Grace period: ${GRACE_PERIOD}s before monitoring starts..."
    sleep "$GRACE_PERIOD"

    local consecutive_failures=0
    local iteration=0

    while true; do
        ((iteration++))
        log "=========================================="
        log "Monitoring Check #$iteration"
        log "=========================================="

        local rollback_reason=""
        local rollback_trigger=""

        # Check 1: Deployment health
        log "Check 1: Deployment health"
        if ! check_deployment_health "$namespace"; then
            ((consecutive_failures++))
            rollback_reason="Health check failed"
            rollback_trigger="health_check"
            log "Health check failed (failure $consecutive_failures/$CONSECUTIVE_FAILURES_THRESHOLD)"
        else
            # Check 2: Error rate
            log "Check 2: Error rate"
            local error_rate=$(get_error_rate "$namespace")
            local error_rate_display=$(echo "$error_rate" | awk '{printf "%.2f%%", $1 * 100}')
            log "Error rate: $error_rate_display"

            local error_rate_pass=$(echo "$error_rate < $ERROR_RATE_THRESHOLD" | bc -l 2>/dev/null || echo "1")
            if [[ "$error_rate_pass" != "1" ]]; then
                ((consecutive_failures++))
                rollback_reason="Error rate $error_rate_display exceeds threshold"
                rollback_trigger="error_rate"
                error "Error rate threshold exceeded!"
            else
                # Check 3: P95 latency
                log "Check 3: P95 latency"
                local p95_latency=$(get_latency_p95 "$namespace")
                log "P95 latency: ${p95_latency}ms"

                local p95_pass=$(echo "$p95_latency < $LATENCY_P95_THRESHOLD" | bc -l 2>/dev/null || echo "1")
                if [[ "$p95_pass" != "1" ]]; then
                    ((consecutive_failures++))
                    rollback_reason="P95 latency ${p95_latency}ms exceeds threshold"
                    rollback_trigger="latency_p95"
                    error "P95 latency threshold exceeded!"
                else
                    # Check 4: P99 latency
                    log "Check 4: P99 latency"
                    local p99_latency=$(get_latency_p99 "$namespace")
                    log "P99 latency: ${p99_latency}ms"

                    local p99_pass=$(echo "$p99_latency < $LATENCY_P99_THRESHOLD" | bc -l 2>/dev/null || echo "1")
                    if [[ "$p99_pass" != "1" ]]; then
                        ((consecutive_failures++))
                        rollback_reason="P99 latency ${p99_latency}ms exceeds threshold"
                        rollback_trigger="latency_p99"
                        error "P99 latency threshold exceeded!"
                    else
                        # All checks passed
                        consecutive_failures=0
                        success "All health checks passed"
                    fi
                fi
            fi
        fi

        # Check if rollback is needed
        if [[ $consecutive_failures -ge $CONSECUTIVE_FAILURES_THRESHOLD ]]; then
            error "Consecutive failures threshold reached: $consecutive_failures"

            if [[ "$monitor_only" == "true" ]]; then
                warning "Monitor-only mode: Would trigger rollback for $rollback_trigger"
            else
                execute_rollback "$namespace" "$rollback_reason" "$rollback_trigger"
                # After rollback, wait before continuing monitoring
                log "Waiting 5 minutes before resuming monitoring..."
                sleep 300
            fi

            consecutive_failures=0
        fi

        # Log current state
        log "Consecutive failures: $consecutive_failures/$CONSECUTIVE_FAILURES_THRESHOLD"

        # Wait before next check
        sleep "$MONITOR_INTERVAL"
    done
}

# ============================================================================
# REPORT GENERATION
# ============================================================================

generate_rollback_report() {
    local namespace=$1
    local from_image=$2
    local to_revision=$3
    local reason=$4
    local trigger=$5

    local report_file="${STATE_DIR}/rollback-report-${namespace}-$(date +%Y%m%d-%H%M%S).json"

    cat > "$report_file" <<EOF
{
  "namespace": "$namespace",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "from_image": "$from_image",
  "to_revision": "$to_revision",
  "reason": "$reason",
  "trigger": "$trigger",
  "thresholds": {
    "error_rate": "$ERROR_RATE_THRESHOLD",
    "latency_p95": "$LATENCY_P95_THRESHOLD",
    "latency_p99": "$LATENCY_P99_THRESHOLD",
    "consecutive_failures": "$CONSECUTIVE_FAILURES_THRESHOLD"
  },
  "monitoring": {
    "interval": "$MONITOR_INTERVAL",
    "grace_period": "$GRACE_PERIOD"
  }
}
EOF

    log "Rollback report saved to: $report_file"
}

# ============================================================================
# STATUS COMMAND
# ============================================================================

show_status() {
    local namespace=$1

    log "=========================================="
    log "Rollback Monitor Status for $namespace"
    log "=========================================="

    if is_rollback_in_progress; then
        warning "Rollback is currently in progress"
    else
        success "No rollback in progress"
    fi

    log ""
    log "Deployment Status:"
    kubectl get deployment -n "$namespace" -o wide || true

    log ""
    log "Pod Status:"
    kubectl get pods -n "$namespace" -o wide || true

    log ""
    log "Recent Rollback Reports:"
    ls -lt "${STATE_DIR}/rollback-report-${namespace}-"* 2>/dev/null | head -5 || log "No reports found"
}

# ============================================================================
# MAIN FUNCTION
# ============================================================================

main() {
    local environment=${1:-"staging"}
    shift || true

    local action="monitor"
    local monitor_only=false
    local force_rollback=false

    # Parse options
    while [[ $# -gt 0 ]]; do
        case $1 in
            --monitor-only)
                monitor_only=true
                shift
                ;;
            --force-rollback)
                force_rollback=true
                shift
                ;;
            --status)
                action="status"
                shift
                ;;
            --help|-h)
                echo "Usage: $0 <environment> [options]"
                echo ""
                echo "Options:"
                echo "  --monitor-only      Monitor without executing rollback"
                echo "  --force-rollback    Force immediate rollback"
                echo "  --status            Show current status"
                echo "  --help, -h          Show this help"
                echo ""
                echo "Examples:"
                echo "  $0 staging --monitor-only"
                echo "  $0 production --force-rollback"
                echo "  $0 staging --status"
                exit 0
                ;;
            *)
                error "Unknown option: $1"
                exit 1
                ;;
        esac
    done

    # Validate environment
    if [[ "$environment" != "staging" && "$environment" != "production" ]]; then
        error "Invalid environment: $environment"
        exit 1
    fi

    local namespace="${NAMESPACE_PREFIX}-${environment}"

    case $action in
        status)
            show_status "$namespace"
            ;;
        *)
            if [[ "$force_rollback" == "true" ]]; then
                execute_rollback "$namespace" "Manual rollback triggered" "manual"
            else
                monitor_deployment "$namespace" "$monitor_only"
            fi
            ;;
    esac
}

main "$@"
