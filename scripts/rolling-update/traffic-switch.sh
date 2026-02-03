#!/bin/bash
#
# Traffic Switch Script for Blue-Green Deployments
# Manages traffic routing between blue and green deployments
#
# Usage:
#   ./traffic-switch.sh <environment> <target-color> [options]
#   ./traffic-switch.sh staging green
#   ./traffic-switch.sh production blue --dry-run
#   ./traffic-switch.sh staging green --analyze-before

set -euo pipefail

# ============================================================================
# CONFIGURATION
# ============================================================================

NAMESPACE_PREFIX="erlmcp"
ACTIVE_SERVICE="erlmcp-active"
PREVIEW_SERVICE="erlmcp-preview"
CANARY_SERVICE="erlmcp-canary"

# Analysis configuration
RUN_ANALYSIS=${RUN_ANALYSIS:-true}
ANALYSIS_DURATION=${ANALYSIS_DURATION:-60}  # seconds

# Traffic verification
VERIFY_AFTER_SWITCH=${VERIFY_AFTER_SWITCH:-true}
VERIFICATION_RETRIES=${VERIFICATION_RETRIES:-5}
VERIFICATION_DELAY=${VERIFICATION_DELAY:-10}  # seconds

# State tracking
STATE_DIR="/tmp/erlmcp-traffic-state"
mkdir -p "$STATE_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Colors for display
BLUE_COLOR='\033[0;34m'
GREEN_COLOR='\033[0;32m'
NC='\033[0m'

# ============================================================================
# LOGGING FUNCTIONS
# ============================================================================

log() {
    echo -e "${BLUE}[$(date '+%Y-%m-%d %H:%M:%S')]${NC} $1" | tee -a "${STATE_DIR}/traffic-switch.log"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" | tee -a "${STATE_DIR}/traffic-switch.log"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1" | tee -a "${STATE_DIR}/traffic-switch.log"
}

warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1" | tee -a "${STATE_DIR}/traffic-switch.log"
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

get_current_active_color() {
    local namespace=$1

    local selector=$(kubectl get service "$ACTIVE_SERVICE" -n "$namespace" \
        -o jsonpath='{.spec.selector.component}' 2>/dev/null || echo "")

    if [[ -n "$selector" ]]; then
        echo "$selector"
    else
        echo "unknown"
    fi
}

get_deployment_status() {
    local namespace=$1
    local deployment=$2

    local available=$(kubectl get deployment "$deployment" -n "$namespace" \
        -o jsonpath='{.status.conditions[?(@.type=="Available")].status}' 2>/dev/null || echo "False")

    local ready=$(kubectl get deployment "$deployment" -n "$namespace" \
        -o jsonpath='{.status.readyReplicas}' 2>/dev/null || echo "0")

    local total=$(kubectl get deployment "$deployment" -n "$namespace" \
        -o jsonpath='{.spec.replicas}' 2>/dev/null || echo "0")

    echo "$available:$ready:$total"
}

# ============================================================================
# TRAFFIC SWITCH FUNCTIONS
# ============================================================================

switch_active_service() {
    local namespace=$1
    local target_color=$2
    local dry_run=$3

    log "=========================================="
    log "Switching Active Service"
    log "=========================================="
    log "Namespace: $namespace"
    log "Target color: $target_color"
    log "Dry run: $dry_run"
    log "=========================================="

    # Get current active color
    local current_color=$(get_current_active_color "$namespace")
    log "Current active: $current_color"

    if [[ "$current_color" == "$target_color" ]]; then
        warning "Target color is already active. No switch needed."
        return 0
    fi

    # Check target deployment status
    local target_status=$(get_deployment_status "$namespace" "$target_color")
    local target_available=$(echo "$target_status" | cut -d: -f1)
    local target_ready=$(echo "$target_status" | cut -d: -f2)
    local target_total=$(echo "$target_status" | cut -d: -f3)

    if [[ "$target_available" != "True" ]]; then
        error "Target deployment $target_color is not available"
        return 1
    fi

    if [[ "$target_ready" != "$target_total" ]]; then
        warning "Target deployment $target_color has $target_ready/$target_total pods ready"
    fi

    # Display the switch
    if [[ "$current_color" == "erlmcp-blue" ]]; then
        echo -e "${BLUE_COLOR}Current: BLUE${NC} -> ${GREEN_COLOR}Target: GREEN${NC}"
    else
        echo -e "${GREEN_COLOR}Current: GREEN${NC} -> ${BLUE_COLOR}Target: BLUE${NC}"
    fi

    if [[ "$dry_run" == "true" ]]; then
        log "DRY RUN: Would switch $ACTIVE_SERVICE selector to $target_color"
        return 0
    fi

    # Execute the switch
    log "Updating service selector..."
    if kubectl patch service "$ACTIVE_SERVICE" -n "$namespace" \
        --type='json' \
        -p="[{\"op\": \"replace\", \"path\": \"/spec/selector/component\", \"value\": \"$target_color\"}]"; then
        success "Service selector updated to $target_color"
    else
        error "Failed to update service selector"
        return 1
    fi

    # Save switch state
    cat > "${STATE_DIR}/last-switch-${namespace}.json" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "from": "$current_color",
  "to": "$target_color",
  "service": "$ACTIVE_SERVICE"
}
EOF

    return 0
}

switch_preview_service() {
    local namespace=$1
    local target_color=$2
    local dry_run=$3

    log "Updating preview service to point to: $target_color"

    if [[ "$dry_run" == "true" ]]; then
        log "DRY RUN: Would update $PREVIEW_SERVICE selector to $target_color"
        return 0
    fi

    kubectl patch service "$PREVIEW_SERVICE" -n "$namespace" \
        --type='json' \
        -p="[{\"op\": \"replace\", \"path\": \"/spec/selector/component\", \"value\": \"$target_color\"}]" || true

    success "Preview service updated"
    return 0
}

# ============================================================================
# VERIFICATION FUNCTIONS
# ============================================================================

verify_traffic_switch() {
    local namespace=$1
    local expected_color=$2

    log "=========================================="
    log "Verifying Traffic Switch"
    log "=========================================="

    local retries=0

    while [[ $retries -lt $VERIFICATION_RETRIES ]]; do
        ((retries++))

        log "Verification attempt $retries/$VERIFICATION_RETRIES"

        # Check service selector
        local current_selector=$(get_current_active_color "$namespace")

        if [[ "$current_selector" == "$expected_color" ]]; then
            success "Service selector verified: $expected_color"
        else
            warning "Service selector mismatch: expected $expected_color, got $current_selector"
        fi

        # Check endpoint connectivity
        local endpoint=$(kubectl get endpoints "$ACTIVE_SERVICE" -n "$namespace" \
            -o jsonpath='{.subsets[0].addresses[0].ip}' 2>/dev/null)

        if [[ -n "$endpoint" ]]; then
            local port=$(kubectl get service "$ACTIVE_SERVICE" -n "$namespace" \
                -o jsonpath='{.spec.ports[0].port}' 2>/dev/null || echo "8080")

            log "Testing endpoint: $endpoint:$port"

            if curl -f -s "http://${endpoint}:${port}/health" >/dev/null 2>&1; then
                success "Health check passed: $endpoint:$port"
                return 0
            else
                warning "Health check failed: $endpoint:$port"
            fi
        else
            warning "No endpoint found for $ACTIVE_SERVICE"
        fi

        sleep "$VERIFICATION_DELAY"
    done

    error "Verification failed after $VERIFICATION_RETRIES attempts"
    return 1
}

# ============================================================================
# CANARY TRAFFIC FUNCTIONS
# ============================================================================

set_canary_weight() {
    local namespace=$1
    local weight=$2
    local dry_run=$3

    log "=========================================="
    log "Setting Canary Traffic Weight"
    log "=========================================="
    log "Namespace: $namespace"
    log "Canary weight: ${weight}%"
    log "Stable weight: $((100 - weight))%"
    log "=========================================="

    if [[ "$dry_run" == "true" ]]; then
        log "DRY RUN: Would set canary weight to ${weight}%"
        return 0
    fi

    # Update canary ingress
    if kubectl get ingress "erlmcp-canary-ingress" -n "$namespace" >/dev/null 2>&1; then
        kubectl patch ingress "erlmcp-canary-ingress" -n "$namespace" \
            --type=json \
            -p="[{\"op\": \"replace\", \"path\": \"/metadata/annotations/nginx.ingress.kubernetes.io~1canary-weight\", \"value\": \"$weight\"}]" || true

        success "Canary ingress weight set to ${weight}%"
    else
        warning "Canary ingress not found"
    fi

    # Update canary rollout if using Argo Rollouts
    if kubectl get rollout "erlmcp-canary-rollout" -n "$namespace" >/dev/null 2>&1; then
        # Argo Rollouts handles canary weight internally
        log "Argo Rollouts detected - canary weight managed by Rollout resource"
    fi

    return 0
}

# ============================================================================
# ANALYSIS FUNCTIONS
# ============================================================================

run_pre_switch_analysis() {
    local namespace=$1
    local target_color=$2

    log "=========================================="
    log "Running Pre-Switch Analysis"
    log "=========================================="
    log "Target: $target_color"
    log "Duration: ${ANALYSIS_DURATION}s"
    log "=========================================="

    # Get target service endpoint
    local target_service=$target_color

    if [[ "$target_color" == "erlmcp-blue" ]]; then
        target_service="erlmcp-blue"
    elif [[ "$target_color" == "erlmcp-green" ]]; then
        target_service="erlmcp-green"
    fi

    # Check deployment health
    local status=$(get_deployment_status "$namespace" "$target_service")
    local available=$(echo "$status" | cut -d: -f1)
    local ready=$(echo "$status" | cut -d: -f2)
    local total=$(echo "$status" | cut -d: -f3)

    log "Deployment status: $available:$ready/$total"

    if [[ "$available" != "True" ]]; then
        error "Target deployment is not available"
        return 1
    fi

    # Check endpoint health
    local endpoint=$(kubectl get endpoints "$target_service" -n "$namespace" \
        -o jsonpath='{.subsets[0].addresses[0].ip}' 2>/dev/null)

    if [[ -z "$endpoint" ]]; then
        error "No endpoint found for $target_service"
        return 1
    fi

    local port=$(kubectl get service "$target_service" -n "$namespace" \
        -o jsonpath='{.spec.ports[0].port}' 2>/dev/null || echo "8080")

    log "Testing endpoint: $endpoint:$port"

    # Run multiple health checks
    local checks_passed=0
    local checks_total=5

    for i in $(seq 1 $checks_total); do
        log "Health check $i/$checks_total..."

        if curl -f -s "http://${endpoint}:${port}/health" >/dev/null 2>&1; then
            ((checks_passed++))
            log "Health check passed"
        else
            log "Health check failed"
        fi

        if [[ $i -lt $checks_total ]]; then
            sleep 5
        fi
    done

    log "Health checks: $checks_passed/$checks_total passed"

    if [[ $checks_passed -ge 4 ]]; then
        success "Pre-switch analysis passed"
        return 0
    else
        error "Pre-switch analysis failed"
        return 1
    fi
}

# ============================================================================
# DISPLAY FUNCTIONS
# ============================================================================

show_traffic_status() {
    local namespace=$1

    log "=========================================="
    log "Traffic Status for $namespace"
    log "=========================================="

    # Active service
    local active_color=$(get_current_active_color "$namespace")
    local active_color_display="BLUE"
    [[ "$active_color" == "erlmcp-green" ]] && active_color_display="GREEN"

    echo ""
    echo "Active Service: $ACTIVE_SERVICE"
    echo -e "  Current Color: ${GREEN_COLOR}$active_color_display${NC}"
    echo ""

    # Deployment status
    echo "Deployment Status:"
    printf "  %-20s %-12s %-12s %-12s\n" "Deployment" "Available" "Ready" "Total"
    printf "  %-20s %-12s %-12s %-12s\n" "-----------" "--------" "-----" "-----"

    for deployment in "erlmcp-blue" "erlmcp-green" "erlmcp-canary"; do
        if kubectl get deployment "$deployment" -n "$namespace" >/dev/null 2>&1; then
            local status=$(get_deployment_status "$namespace" "$deployment")
            local available=$(echo "$status" | cut -d: -f1)
            local ready=$(echo "$status" | cut -d: -f2)
            local total=$(echo "$status" | cut -d: -f3)
            printf "  %-20s %-12s %-12s %-12s\n" "$deployment" "$available" "$ready" "$total"
        fi
    done

    echo ""

    # Canary weight (if applicable)
    if kubectl get ingress "erlmcp-canary-ingress" -n "$namespace" >/dev/null 2>&1; then
        local canary_weight=$(kubectl get ingress "erlmcp-canary-ingress" -n "$namespace" \
            -o jsonpath='{.metadata.annotations.nginx\.ingress\.kubernetes\.io/canary-weight}' 2>/dev/null || echo "0")
        echo "Canary Weight: ${canary_weight}%"
        echo ""
    fi

    # Recent switches
    if [[ -f "${STATE_DIR}/last-switch-${namespace}.json" ]]; then
        echo "Recent Switch:"
        cat "${STATE_DIR}/last-switch-${namespace}.json" | jq -r '"  \(.timestamp): \(.from) -> \(.to)"'
        echo ""
    fi

    log "=========================================="
}

# ============================================================================
# MAIN FUNCTION
# ============================================================================

main() {
    local environment=${1:-"staging"}
    local target_color=${2:-""}
    shift 2 || true

    # Parse options
    local dry_run=false
    local analyze_before=false
    local show_status_only=false
    local canary_weight=""
    local canary_mode=false

    while [[ $# -gt 0 ]]; do
        case $1 in
            --dry-run)
                dry_run=true
                shift
                ;;
            --analyze-before)
                analyze_before=true
                shift
                ;;
            --status)
                show_status_only=true
                shift
                ;;
            --canary-weight)
                canary_weight=$2
                canary_mode=true
                shift 2
                ;;
            --help|-h)
                echo "Usage: $0 <environment> <target-color> [options]"
                echo ""
                echo "Arguments:"
                echo "  environment    staging or production"
                echo "  target-color   blue, green, or canary"
                echo ""
                echo "Options:"
                echo "  --dry-run          Show what would be done without making changes"
                echo "  --analyze-before   Run analysis before switching"
                echo "  --status           Show current traffic status"
                echo "  --canary-weight N  Set canary traffic weight to N%"
                echo "  --help, -h         Show this help"
                echo ""
                echo "Examples:"
                echo "  $0 staging green"
                echo "  $0 production blue --dry-run"
                echo "  $0 staging green --analyze-before"
                echo "  $0 staging --canary-weight 25"
                echo "  $0 production --status"
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

    # Show status only
    if [[ "$show_status_only" == "true" ]]; then
        show_traffic_status "$namespace"
        exit 0
    fi

    # Canary mode
    if [[ "$canary_mode" == "true" ]]; then
        set_canary_weight "$namespace" "$canary_weight" "$dry_run"
        exit $?
    fi

    # Validate target color
    if [[ -z "$target_color" ]]; then
        error "Target color is required"
        echo "Usage: $0 <environment> <blue|green> [options]"
        exit 1
    fi

    case "$target_color" in
        blue)
            target_color="erlmcp-blue"
            ;;
        green)
            target_color="erlmcp-green"
            ;;
        *)
            error "Invalid target color: $target_color. Use 'blue' or 'green'"
            exit 1
            ;;
    esac

    log "=========================================="
    log "Traffic Switch Script"
    log "=========================================="

    # Pre-switch analysis
    if [[ "$analyze_before" == "true" ]]; then
        if ! run_pre_switch_analysis "$namespace" "$target_color"; then
            error "Pre-switch analysis failed, aborting switch"
            exit 1
        fi
    fi

    # Switch active service
    if ! switch_active_service "$namespace" "$target_color" "$dry_run"; then
        error "Failed to switch active service"
        exit 1
    fi

    # Update preview service to point to the old color
    local current_color=$(get_current_active_color "$namespace")
    # After switch, the old color is the one that's now inactive
    # We'll handle this in a more sophisticated way

    # Verify switch
    if [[ "$VERIFY_AFTER_SWITCH" == "true" && "$dry_run" == "false" ]]; then
        sleep "$VERIFICATION_DELAY"
        if ! verify_traffic_switch "$namespace" "$target_color"; then
            error "Traffic switch verification failed"

            # Attempt rollback
            warning "Attempting to rollback traffic switch..."
            switch_active_service "$namespace" "$current_color" false
            exit 1
        fi
    fi

    success "=========================================="
    success "Traffic Switch Completed!"
    success "Active: $target_color"
    success "=========================================="

    # Show final status
    show_traffic_status "$namespace"

    exit 0
}

main "$@"
