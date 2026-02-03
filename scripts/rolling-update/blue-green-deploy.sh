#!/bin/bash
#
# Blue-Green Deployment Automation Script for erlmcp
# Implements zero-downtime deployments with automatic rollback on failure
#
# Usage:
#   ./blue-green-deploy.sh <environment> <image> [--dry-run]
#   ./blue-green-deploy.sh staging ghcr.io/org/erlmcp:v1.0.0
#   ./blue-green-deploy.sh production ghcr.io/org/erlmcp:v1.0.0 --dry-run
#
# Features:
# - Automated blue-green traffic switching
# - Pre and post deployment health checks
# - Automated rollback on failure
# - Deployment state tracking
# - Slack/Teams notifications

set -euo pipefail

# ============================================================================
# CONFIGURATION
# ============================================================================

NAMESPACE_PREFIX="erlmcp"
ACTIVE_SERVICE="erlmcp-active"
PREVIEW_SERVICE="erlmcp-preview"
HEALTH_ENDPOINT="/mcp/health"
READY_ENDPOINT="/mcp/health?ready=1"
TIMEOUT_SECONDS=600
HEALTH_CHECK_INTERVAL=10
MAX_RETRIES=3

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# State tracking
STATE_DIR="/tmp/erlmcp-deployment-state"
mkdir -p "$STATE_DIR"

# ============================================================================
# LOGGING FUNCTIONS
# ============================================================================

log() {
    echo -e "${BLUE}[$(date '+%Y-%m-%d %H:%M:%S')]${NC} $1" | tee -a "${STATE_DIR}/deployment.log"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "${STATE_DIR}/deployment.log"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" | tee -a "${STATE_DIR}/deployment.log"
}

warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1" | tee -a "${STATE_DIR}/deployment.log"
}

# ============================================================================
# NOTIFICATION FUNCTIONS
# ============================================================================

send_notification() {
    local status=$1
    local message=$2
    local environment=$3

    log "Sending notification: $status"

    # Slack notification
    if [[ -n "${SLACK_WEBHOOK_URL:-}" ]]; then
        local color="good"
        if [[ "$status" == "FAILED" || "$status" == "ROLLED_BACK" ]]; then
            color="danger"
        elif [[ "$status" == "WARNING" ]]; then
            color="warning"
        fi

        curl -s -X POST "$SLACK_WEBHOOK_URL" \
            -H 'Content-Type: application/json' \
            -d "{
                \"attachments\": [{
                    \"color\": \"$color\",
                    \"title\": \"erlmcp Deployment: $status\",
                    \"fields\": [
                        {\"title\": \"Environment\", \"value\": \"$environment\", \"short\": true},
                        {\"title\": \"Status\", \"value\": \"$status\", \"short\": true},
                        {\"title\": \"Message\", \"value\": \"$message\", \"short\": false}
                    ],
                    \"footer\": \"erlmcp deployment automation\",
                    \"ts\": $(date +%s)
                }]
            }" > /dev/null || true
    fi

    # Teams notification
    if [[ -n "${TEAMS_WEBHOOK_URL:-}" ]]; then
        curl -s -X POST "$TEAMS_WEBHOOK_URL" \
            -H 'Content-Type: application/json' \
            -d "{
                \"@type\": \"MessageCard\",
                \"@context\": \"https://schema.org/extensions\",
                \"summary\": \"erlmcp Deployment: $status\",
                \"themeColor\": \"$( [[ "$status" == "FAILED" ]] && echo \"FF0000\" || echo \"0078D4\" )\",
                \"title\": \"erlmcp Deployment: $status\",
                \"sections\": [{
                    \"facts\": [
                        {\"name\": \"Environment\", \"value\": \"$environment\"},
                        {\"name\": \"Status\", \"value\": \"$status\"},
                        {\"name\": \"Message\", \"value\": \"$message\"}
                    ]
                }]
            }" > /dev/null || true
    fi
}

# ============================================================================
# STATE TRACKING FUNCTIONS
# ============================================================================

save_deployment_state() {
    local environment=$1
    local active_color=$2
    local new_color=$3
    local image=$4
    local status=$5

    cat > "${STATE_DIR}/${environment}-state.json" <<EOF
{
  "environment": "$environment",
  "active_color": "$active_color",
  "new_color": "$new_color",
  "image": "$image",
  "status": "$status",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
}

get_deployment_state() {
    local environment=$1
    local state_file="${STATE_DIR}/${environment}-state.json"

    if [[ -f "$state_file" ]]; then
        cat "$state_file"
    else
        echo "{}"
    fi
}

# ============================================================================
# HEALTH CHECK FUNCTIONS
# ============================================================================

get_active_color() {
    local namespace=$1

    log "Determining active deployment color in $namespace"

    # Check the active service selector
    local selector=$(kubectl get service "$ACTIVE_SERVICE" -n "$namespace" \
        -o jsonpath='{.spec.selector.component}' 2>/dev/null || echo "")

    if [[ -n "$selector" ]]; then
        echo "$selector"
        return 0
    fi

    # Fallback: check which deployment has pods
    local blue_pods=$(kubectl get pods -n "$namespace" -l component=erlmcp-blue \
        --no-headers 2>/dev/null | grep -c "Running" || echo "0")
    local green_pods=$(kubectl get pods -n "$namespace" -l component=erlmcp-green \
        --no-headers 2>/dev/null | grep -c "Running" || echo "0")

    if [[ "$blue_pods" -gt 0 ]]; then
        echo "erlmcp-blue"
    elif [[ "$green_pods" -gt 0 ]]; then
        echo "erlmcp-green"
    else
        echo "erlmcp-blue"  # Default to blue
    fi
}

wait_for_deployment_ready() {
    local namespace=$1
    local deployment=$2
    local timeout=$3

    log "Waiting for $deployment to be ready (timeout: ${timeout}s)..."

    local start_time=$(date +%s)
    local end_time=$((start_time + timeout))

    while true; do
        local current_time=$(date +%s)

        if [[ $current_time -ge $end_time ]]; then
            error "Timeout waiting for $deployment to be ready"
            return 1
        fi

        # Check deployment status
        if kubectl rollout status deployment/"$deployment" -n "$namespace" --timeout=30s 2>/dev/null; then
            success "$deployment is ready"
            return 0
        fi

        # Check pod readiness
        local ready_pods=$(kubectl get pods -n "$namespace" -l "app=erlmcp,component=$deployment" \
            -o jsonpath='{.items[*].status.conditions[?(@.type=="Ready")].status}' 2>/dev/null | \
            grep -c "true" || echo "0")
        local total_pods=$(kubectl get deployment "$deployment" -n "$namespace" \
            -o jsonpath='{.spec.replicas}' 2>/dev/null || echo "0")

        log "Pods ready: $ready_pods/$total_pods"

        sleep "$HEALTH_CHECK_INTERVAL"
    done
}

health_check() {
    local namespace=$1
    local service=$2
    local max_attempts=${3:-30}

    log "Running health check against $service in $namespace"

    local attempts=0

    while [[ $attempts -lt $max_attempts ]]; do
        # Get service endpoint
        local endpoint=$(kubectl get endpoints "$service" -n "$namespace" \
            -o jsonpath='{.subsets[0].addresses[0].ip}' 2>/dev/null)

        if [[ -z "$endpoint" ]]; then
            log "No endpoint found for $service, attempt $((attempts + 1))/$max_attempts"
            sleep "$HEALTH_CHECK_INTERVAL"
            ((attempts++))
            continue
        fi

        local port=$(kubectl get service "$service" -n "$namespace" \
            -o jsonpath='{.spec.ports[0].port}' 2>/dev/null || echo "8080")

        # Test health endpoint
        if curl -f -s "http://${endpoint}:${port}${HEALTH_ENDPOINT}" >/dev/null 2>&1; then
            log "Health endpoint passed"

            # Test ready endpoint
            if curl -f -s "http://${endpoint}:${port}${READY_ENDPOINT}" >/dev/null 2>&1; then
                success "Health check passed for $service"
                return 0
            fi
        fi

        log "Health check failed, attempt $((attempts + 1))/$max_attempts"
        sleep "$HEALTH_CHECK_INTERVAL"
        ((attempts++))
    done

    error "Health check failed after $max_attempts attempts"
    return 1
}

run_integration_tests() {
    local namespace=$1
    local service=$2

    log "Running integration tests against $service"

    local endpoint=$(kubectl get endpoints "$service" -n "$namespace" \
        -o jsonpath='{.subsets[0].addresses[0].ip}' 2>/dev/null)
    local port=$(kubectl get service "$service" -n "$namespace" \
        -o jsonpath='{.spec.ports[0].port}' 2>/dev/null || echo "8080")

    if [[ -z "$endpoint" ]]; then
        error "No endpoint found for integration tests"
        return 1
    fi

    local base_url="http://${endpoint}:${port}"
    local tests_passed=0
    local tests_total=0

    # Define test cases
    local tests=(
        "$base_url/health"
        "$base_url/mcp/health?ready=1"
        "$base_url/mcp/server-info"
        "$base_url/metrics"
    )

    for test_url in "${tests[@]}"; do
        ((tests_total++))

        if curl -f -s "$test_url" >/dev/null 2>&1; then
            log "Test passed: $test_url"
            ((tests_passed++))
        else
            error "Test failed: $test_url"
        fi
    done

    log "Integration tests: $tests_passed/$tests_total passed"

    if [[ $tests_passed -eq $tests_total ]]; then
        success "All integration tests passed"
        return 0
    else
        error "Some integration tests failed"
        return 1
    fi
}

# ============================================================================
# ROLLBACK FUNCTIONS
# ============================================================================

rollback_deployment() {
    local namespace=$1
    local failed_color=$2
    local success_color=$3
    local reason=$4

    error "Rolling back deployment: $reason"

    send_notification "ROLLED_BACK" "$reason" "$namespace"

    # Switch traffic back to the working color
    log "Switching traffic back to $success_color"

    kubectl patch service "$ACTIVE_SERVICE" -n "$namespace" \
        --type='json' \
        -p="[{\"op\": \"replace\", \"path\": \"/spec/selector/component\", \"value\": \"$success_color\"}]" || true

    sleep 10

    # Verify rollback
    if health_check "$namespace" "$ACTIVE_SERVICE" 10; then
        success "Rollback completed successfully"

        # Scale down failed deployment
        kubectl scale deployment "$failed_color" -n "$namespace" --replicas=0 || true

        # Update state
        save_deployment_state "$namespace" "$success_color" "$success_color" "" "rolled_back"

        return 0
    else
        error "Rollback verification failed"
        return 1
    fi
}

# ============================================================================
# DEPLOYMENT FUNCTIONS
# ============================================================================

deploy_new_version() {
    local namespace=$1
    local deployment=$2
    local image=$3

    log "Deploying $deployment with image $image"

    # Update deployment image
    kubectl set image deployment/"$deployment" erlmcp="$image" -n "$namespace"

    # Wait for deployment
    if ! wait_for_deployment_ready "$namespace" "$deployment" 300; then
        error "Failed to deploy $deployment"
        return 1
    fi

    success "$deployment deployed successfully"
    return 0
}

switch_traffic() {
    local namespace=$1
    local target_color=$2

    log "Switching traffic to $target_color"

    # Update active service selector
    kubectl patch service "$ACTIVE_SERVICE" -n "$namespace" \
        --type='json' \
        -p="[{\"op\": \"replace\", \"path\": \"/spec/selector/component\", \"value\": \"$target_color\"}]" || {
        error "Failed to switch traffic"
        return 1
    }

    success "Traffic switched to $target_color"
    return 0
}

# ============================================================================
# MAIN DEPLOYMENT ORCHESTRATION
# ============================================================================

main() {
    local environment=${1:-"staging"}
    local image=${2:-""}
    local dry_run=${3:-false}

    # Validate inputs
    if [[ "$environment" != "staging" && "$environment" != "production" ]]; then
        error "Invalid environment: $environment. Must be 'staging' or 'production'"
        exit 1
    fi

    if [[ -z "$image" ]]; then
        error "Image parameter is required"
        echo "Usage: $0 <environment> <image> [--dry-run]"
        exit 1
    fi

    local namespace="${NAMESPACE_PREFIX}-${environment}"

    log "=========================================="
    log "Starting Blue-Green Deployment"
    log "Environment: $environment"
    log "Namespace: $namespace"
    log "Image: $image"
    log "Dry Run: $dry_run"
    log "=========================================="

    if [[ "$dry_run" == "true" ]]; then
        log "DRY RUN MODE - No changes will be made"
        log "Would deploy: $image to $namespace"
        exit 0
    fi

    # Initialize state
    local active_color
    active_color=$(get_active_color "$namespace")
    log "Active deployment: $active_color"

    # Determine colors
    local new_color
    if [[ "$active_color" == "erlmcp-blue" ]]; then
        new_color="erlmcp-green"
    else
        new_color="erlmcp-blue"
    fi

    log "New deployment color: $new_color"

    # Save initial state
    save_deployment_state "$environment" "$active_color" "$new_color" "$image" "deploying"

    send_notification "STARTED" "Deploying $image" "$environment"

    # Step 1: Deploy new version to inactive color
    log "Step 1: Deploying to $new_color"
    if ! deploy_new_version "$namespace" "$new_color" "$image"; then
        rollback_deployment "$namespace" "$new_color" "$active_color" "Failed to deploy new version"
        exit 1
    fi

    # Step 2: Health check new deployment
    log "Step 2: Health checking $new_color"
    if ! health_check "$namespace" "$new_color" 60; then
        rollback_deployment "$namespace" "$new_color" "$active_color" "Health check failed"
        exit 1
    fi

    # Step 3: Run integration tests against new deployment
    log "Step 3: Running integration tests"
    if ! run_integration_tests "$namespace" "$new_color"; then
        rollback_deployment "$namespace" "$new_color" "$active_color" "Integration tests failed"
        exit 1
    fi

    # Step 4: Switch traffic to new deployment
    log "Step 4: Switching traffic to $new_color"
    if ! switch_traffic "$namespace" "$new_color"; then
        rollback_deployment "$namespace" "$new_color" "$active_color" "Traffic switch failed"
        exit 1
    fi

    # Step 5: Health check after traffic switch
    log "Step 5: Verifying health after traffic switch"
    sleep 10
    if ! health_check "$namespace" "$ACTIVE_SERVICE" 30; then
        rollback_deployment "$namespace" "$new_color" "$active_color" "Post-switch health check failed"
        exit 1
    fi

    # Step 6: Run integration tests against active service
    log "Step 6: Running final integration tests"
    if ! run_integration_tests "$namespace" "$ACTIVE_SERVICE"; then
        rollback_deployment "$namespace" "$new_color" "$active_color" "Final integration tests failed"
        exit 1
    fi

    # Step 7: Scale down old deployment (optional, keeping for rollback safety)
    log "Step 7: Keeping old deployment scaled for safety"
    # Optionally scale down after a period:
    # kubectl scale deployment "$active_color" -n "$namespace" --replicas=0

    # Update final state
    save_deployment_state "$environment" "$new_color" "$new_color" "$image" "success"

    success "=========================================="
    success "Blue-Green Deployment Completed!"
    success "Active: $new_color with image $image"
    success "=========================================="

    send_notification "SUCCESS" "Deployed $image to $new_color" "$environment"

    # Generate deployment report
    cat > "${STATE_DIR}/deployment-report-${environment}-$(date +%Y%m%d-%H%M%S).json" <<EOF
{
  "environment": "$environment",
  "namespace": "$namespace",
  "image": "$image",
  "previous_active": "$active_color",
  "new_active": "$new_color",
  "status": "success",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF

    exit 0
}

# Run main function
main "$@"
