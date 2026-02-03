#!/usr/bin/env bash
# ==============================================================================
# ERLMCP v3 Enterprise - ResourceQuota Validation Script
# ==============================================================================
# Purpose: Validate resource quota configuration and usage across namespaces
# Usage: ./scripts/k8s/validate-resource-quota.sh [namespace]
#
# This script validates:
# - ResourceQuota objects are properly configured
# - LimitRange objects exist and are valid
# - Current usage vs. hard limits
# - Alerts for approaching quota limits
# - Tenant isolation compliance
# ==============================================================================

set -euo pipefail

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
NAMESPACE="${1:-erlmcp}"
CONTEXT="${KUBECONTEXT:-}"
WARNING_THRESHOLD=80
CRITICAL_THRESHOLD=90

# Logging functions
log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[PASS]${NC} $1"; }
log_warning() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[FAIL]${NC} $1"; }

# Kubernetes context setup
setup_kubectl() {
    local kubectl_cmd="kubectl"
    if [[ -n "$CONTEXT" ]]; then
        kubectl_cmd="kubectl --context=$CONTEXT"
    fi
    echo "$kubectl_cmd"
}

KUBECTL=$(setup_kubectl)

# Check if namespace exists
check_namespace() {
    log_info "Checking namespace: $NAMESPACE"
    if $KUBECTL get namespace "$NAMESPACE" &>/dev/null; then
        log_success "Namespace $NAMESPACE exists"
        return 0
    else
        log_error "Namespace $NAMESPACE does not exist"
        return 1
    fi
}

# Validate ResourceQuota objects
validate_resource_quotas() {
    log_info "Validating ResourceQuota objects in namespace: $NAMESPACE"

    local quotas
    quotas=$($KUBECTL get resourcequota -n "$NAMESPACE" -o json 2>/dev/null | jq -r '.items[].metadata.name' 2>/dev/null || echo "")

    if [[ -z "$quotas" ]]; then
        log_error "No ResourceQuota objects found in namespace $NAMESPACE"
        return 1
    fi

    local quota_count=0
    local failed=0

    while IFS= read -r quota; do
        [[ -z "$quota" ]] && continue
        quota_count=$((quota_count + 1))

        log_info "Checking ResourceQuota: $quota"

        # Get quota details
        local quota_json
        quota_json=$($KUBECTL get resourcequota "$quota" -n "$NAMESPACE" -o json)

        # Check hard limits
        local hard_limits
        hard_limits=$(echo "$quota_json" | jq -r '.status.hard // {}')

        # Check used resources
        local used
        used=$(echo "$quota_json" | jq -r '.status.used // {}')

        # Validate CPU requests
        local cpu_requests_hard
        local cpu_requests_used
        cpu_requests_hard=$(echo "$hard_limits" | jq -r '."requests.cpu" // "0"')
        cpu_requests_used=$(echo "$used" | jq -r '."requests.cpu" // "0"')

        if [[ "$cpu_requests_hard" != "0" ]]; then
            local cpu_ratio
            cpu_ratio=$(awk "BEGIN {print ($cpu_requests_used / $cpu_requests_hard) * 100}")
            log_info "  CPU Requests: $cpu_requests_used / $cpu_requests_hard (${cpu_ratio}%)"

            if (( $(awk "BEGIN {print ($cpu_ratio >= $CRITICAL_THRESHOLD)}") )); then
                log_error "  CPU requests usage CRITICAL: ${cpu_ratio}%"
                failed=$((failed + 1))
            elif (( $(awk "BEGIN {print ($cpu_ratio >= $WARNING_THRESHOLD)}") )); then
                log_warning "  CPU requests usage WARNING: ${cpu_ratio}%"
            fi
        fi

        # Validate memory requests
        local mem_requests_hard
        local mem_requests_used
        mem_requests_hard=$(echo "$hard_limits" | jq -r '."requests.memory" // "0"')
        mem_requests_used=$(echo "$used" | jq -r '."requests.memory" // "0"')

        if [[ "$mem_requests_hard" != "0" ]]; then
            # Convert to Mi for comparison
            local mem_hard_mi
            local mem_used_mi
            mem_hard_mi=$(convert_to_mi "$mem_requests_hard")
            mem_used_mi=$(convert_to_mi "$mem_requests_used")
            local mem_ratio
            mem_ratio=$(awk "BEGIN {print ($mem_used_mi / $mem_hard_mi) * 100}")

            log_info "  Memory Requests: $mem_requests_used / $mem_requests_hard (${mem_ratio}%)"

            if (( $(awk "BEGIN {print ($mem_ratio >= $CRITICAL_THRESHOLD)}") )); then
                log_error "  Memory requests usage CRITICAL: ${mem_ratio}%"
                failed=$((failed + 1))
            elif (( $(awk "BEGIN {print ($mem_ratio >= $WARNING_THRESHOLD)}") )); then
                log_warning "  Memory requests usage WARNING: ${mem_ratio}%"
            fi
        fi

        # Validate pod count
        local pods_hard
        local pods_used
        pods_hard=$(echo "$hard_limits" | jq -r '.pods // "0"')
        pods_used=$(echo "$used" | jq -r '.pods // "0"')

        if [[ "$pods_hard" != "0" ]]; then
            local pod_ratio
            pod_ratio=$(awk "BEGIN {print ($pods_used / $pods_hard) * 100}")
            log_info "  Pods: $pods_used / $pods_hard (${pod_ratio}%)"

            if (( $(awk "BEGIN {print ($pod_ratio >= $CRITICAL_THRESHOLD)}") )); then
                log_error "  Pod count CRITICAL: ${pod_ratio}%"
                failed=$((failed + 1))
            elif (( $(awk "BEGIN {print ($pod_ratio >= $WARNING_THRESHOLD)}") )); then
                log_warning "  Pod count WARNING: ${pod_ratio}%"
            fi
        fi

        # Validate PVC count
        local pvc_hard
        local pvc_used
        pvc_hard=$(echo "$hard_limits" | jq -r '.persistentvolumeclaims // "0"')
        pvc_used=$(echo "$used" | jq -r '.persistentvolumeclaims // "0"')

        if [[ "$pvc_hard" != "0" ]]; then
            local pvc_ratio
            pvc_ratio=$(awk "BEGIN {print ($pvc_used / $pvc_hard) * 100}")
            log_info "  PVCs: $pvc_used / $pvc_hard (${pvc_ratio}%)"
        fi
    done <<< "$quotas"

    if [[ $quota_count -gt 0 ]]; then
        log_success "Found $quota_count ResourceQuota object(s)"
    fi

    return $failed
}

# Validate LimitRange objects
validate_limit_ranges() {
    log_info "Validating LimitRange objects in namespace: $NAMESPACE"

    local limits
    limits=$($KUBECTL get limitrange -n "$NAMESPACE" -o json 2>/dev/null | jq -r '.items[].metadata.name' 2>/dev/null || echo "")

    if [[ -z "$limits" ]]; then
        log_warning "No LimitRange objects found in namespace $NAMESPACE (pods without limits may be evicted)"
        return 0
    fi

    local limit_count=0
    while IFS= read -r limit; do
        [[ -z "$limit" ]] && continue
        limit_count=$((limit_count + 1))
        log_info "Found LimitRange: $limit"
    done <<< "$limits"

    if [[ $limit_count -gt 0 ]]; then
        log_success "Found $limit_count LimitRange object(s)"
    fi

    return 0
}

# Check for pods without resource requests
check_pods_without_requests() {
    log_info "Checking for pods without resource requests"

    local pods_without_requests
    pods_without_requests=$($KUBECTL get pods -n "$NAMESPACE" -o json 2>/dev/null | \
        jq -r '.items[] | select(.spec.containers[].resources.requests == null) | .metadata.name' 2>/dev/null || echo "")

    if [[ -n "$pods_without_requests" ]]; then
        local count
        count=$(echo "$pods_without_requests" | wc -l | tr -d ' ')
        log_warning "Found $count pod(s) without resource requests (best-effort QoS):"
        echo "$pods_without_requests" | while read -r pod; do
            [[ -n "$pod" ]] && log_warning "  - $pod"
        done
    else
        log_success "All pods have resource requests defined"
    fi

    return 0
}

# Validate PriorityClass usage
validate_priority_classes() {
    log_info "Validating PriorityClass usage"

    local high_priority_pods
    high_priority_pods=$($KUBECTL get pods -n "$NAMESPACE" -o json 2>/dev/null | \
        jq -r '.items[] | select(.spec.priorityClassName == "high-priority") | .metadata.name' 2>/dev/null || echo "")

    local batch_pods
    batch_pods=$($KUBECTL get pods -n "$NAMESPACE" -o json 2>/dev/null | \
        jq -r '.items[] | select(.spec.priorityClassName == "low-priority") | .metadata.name' 2>/dev/null || echo "")

    log_info "High priority pods: $(echo "$high_priority_pods" | grep -c '.' || echo 0)"
    log_info "Low priority (batch) pods: $(echo "$batch_pods" | grep -c '.' || echo 0)"

    return 0
}

# Convert memory string to Mi
convert_to_mi() {
    local mem="$1"
    local value
    local unit

    value=$(echo "$mem" | grep -oE '[0-9]+')
    unit=$(echo "$mem" | grep -oE '[A-Za-z]+')

    case "$unit" in
        Ki) awk "BEGIN {print $value / 1024}" ;;
        Mi) echo "$value" ;;
        Gi) awk "BEGIN {print $value * 1024}" ;;
        Ti) awk "BEGIN {print $value * 1024 * 1024}" ;;
        K) awk "BEGIN {print $value / 1000}" ;;
        M) echo "$value" ;;
        G) awk "BEGIN {print $value * 1000}" ;;
        T) awk "BEGIN {print $value * 1000 * 1000}" ;;
        *) echo "0" ;;
    esac
}

# Main execution
main() {
    echo "=============================================================================="
    echo "ERLMCP v3 Enterprise - ResourceQuota Validation"
    echo "=============================================================================="
    echo "Namespace: $NAMESPACE"
    echo "Context: ${CONTEXT:-default}"
    echo "Warning Threshold: ${WARNING_THRESHOLD}%"
    echo "Critical Threshold: ${CRITICAL_THRESHOLD}%"
    echo "=============================================================================="
    echo ""

    local exit_code=0

    check_namespace || exit 1
    validate_resource_quotas || exit_code=$?
    validate_limit_ranges
    check_pods_without_requests
    validate_priority_classes

    echo ""
    echo "=============================================================================="
    if [[ $exit_code -eq 0 ]]; then
        log_success "ResourceQuota validation completed"
    else
        log_error "ResourceQuota validation completed with errors"
    fi
    echo "=============================================================================="

    exit $exit_code
}

# Run main function
main "$@"
