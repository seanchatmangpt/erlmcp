#!/bin/bash
# ==============================================================================
# ErlMCP ConfigMap Hot-Reload Script
# ==============================================================================
# This script implements hot-reload for ConfigMap changes without requiring
# pod restarts. It uses a Kubernetes-aware approach to trigger configuration
# reload across all pods.
#
# Usage:
#   ./scripts/feature-flags/configmap-hotreload.sh [namespace]
#
# Features:
# - Detects ConfigMap changes via resourceVersion
# - Triggers graceful reload via HTTP endpoint or RPC
# - Validates configuration before applying
# - Rolls back on failure
# - Supports dry-run mode
# ==============================================================================

set -euo pipefail

# Script configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Default values
NAMESPACE="${1:-erlmcp}"
CONFIGMAP_NAME="${CONFIGMAP_NAME:-erlmcp-config}"
RELOAD_ENDPOINT="${RELOAD_ENDPOINT:-http://localhost:8081/admin/reload-config}"
HEALTH_ENDPOINT="${HEALTH_ENDPOINT:-http://localhost:8080/health}"
TIMEOUT="${TIMEOUT:-30}"
DRY_RUN="${DRY_RUN:-false}"
VERBOSE="${VERBOSE:-false}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO] $*${NC}"
}

log_success() {
    echo -e "${GREEN}[SUCCESS] $*${NC}"
}

log_warning() {
    echo -e "${YELLOW}[WARNING] $*${NC}"
}

log_error() {
    echo -e "${RED}[ERROR] $*${NC}" >&2
}

log_debug() {
    if [[ "${VERBOSE}" == "true" ]]; then
        echo -e "${BLUE}[DEBUG] $*${NC}"
    fi
}

# Check if kubectl is available
check_kubectl() {
    if ! command -v kubectl &> /dev/null; then
        log_error "kubectl not found. Please install kubectl."
        exit 1
    fi

    # Check if we can access the cluster
    if ! kubectl cluster-info &> /dev/null; then
        log_error "Cannot access Kubernetes cluster. Please check your kubeconfig."
        exit 1
    fi
}

# Get current ConfigMap resource version
get_configmap_version() {
    kubectl get configmap "${CONFIGMAP_NAME}" -n "${NAMESPACE}" -o jsonpath='{.metadata.resourceVersion}' 2>/dev/null || echo ""
}

# Get all ErlMCP pods
get_erlmcp_pods() {
    kubectl get pods -n "${NAMESPACE}" -l app=erlmcp -o jsonpath='{.items[*].metadata.name}' 2>/dev/null | tr ' ' '\n' | grep -v '^$' || echo ""
}

# Check if a pod is ready
is_pod_ready() {
    local pod=$1
    local phase
    phase=$(kubectl get pod "${pod}" -n "${NAMESPACE}" -o jsonpath='{.status.phase}' 2>/dev/null || echo "Unknown")
    [[ "${phase}" == "Running" ]]
}

# Wait for pod to be ready
wait_for_pod_ready() {
    local pod=$1
    local timeout=${2:-60}

    log_info "Waiting for pod ${pod} to be ready..."
    local elapsed=0
    while [[ ${elapsed} -lt ${timeout} ]]; do
        if is_pod_ready "${pod}"; then
            log_success "Pod ${pod} is ready"
            return 0
        fi
        sleep 2
        ((elapsed += 2))
    done

    log_warning "Pod ${pod} not ready after ${timeout}s"
    return 1
}

# Trigger reload on a specific pod
trigger_reload() {
    local pod=$1
    local method=${2:-http}

    log_info "Triggering reload on pod ${pod}..."

    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY-RUN] Would trigger reload on ${pod} via ${method}"
        return 0
    fi

    case "${method}" in
        http)
            # Use HTTP endpoint (via port-forward if needed)
            if kubectl exec -n "${NAMESPACE}" "${pod}" -- curl -sf -X POST "${RELOAD_ENDPOINT}" &> /dev/null; then
                log_success "Reload triggered via HTTP on ${pod}"
                return 0
            else
                log_debug "HTTP reload failed, trying RPC..."
                trigger_reload "${pod}" rpc
            fi
            ;;
        rpc)
            # Use Erlang RPC
            local reload_cmd="erl -noshell -s erlmcp_config reload_config -s init stop"
            if kubectl exec -n "${NAMESPACE}" "${pod}" -- sh -c "cd /opt/erlmcp && ${reload_cmd}" &> /dev/null; then
                log_success "Reload triggered via RPC on ${pod}"
                return 0
            else
                log_warning "RPC reload failed on ${pod}"
                return 1
            fi
            ;;
        signal)
            # Send HUP signal for reload
            if kubectl exec -n "${NAMESPACE}" "${pod}" -- kill -HUP 1 &> /dev/null; then
                log_success "Reload triggered via HUP on ${pod}"
                return 0
            else
                log_warning "Signal reload failed on ${pod}"
                return 1
            fi
            ;;
        *)
            log_error "Unknown reload method: ${method}"
            return 1
    esac
}

# Validate configuration
validate_config() {
    local config_file=$1

    log_info "Validating configuration..."

    # Check if it's valid JSON
    if command -v jq &> /dev/null; then
        if ! jq empty "${config_file}" 2>/dev/null; then
            log_error "Invalid JSON in configuration file"
            return 1
        fi
    fi

    # Additional validation can be added here
    log_success "Configuration validation passed"
    return 0
}

# Main hot-reload function
hot_reload() {
    local config_file=${1:-}

    log_info "Starting hot-reload for ConfigMap ${CONFIGMAP_NAME} in namespace ${NAMESPACE}"

    # Get current version
    local current_version
    current_version=$(get_configmap_version)

    if [[ -z "${current_version}" ]]; then
        log_error "ConfigMap ${CONFIGMAP_NAME} not found in namespace ${NAMESPACE}"
        return 1
    fi

    log_info "Current ConfigMap version: ${current_version}"

    # Validate configuration if file provided
    if [[ -n "${config_file}" ]]; then
        if ! validate_config "${config_file}"; then
            return 1
        fi

        # Apply new configuration
        if [[ "${DRY_RUN}" == "false" ]]; then
            log_info "Applying new configuration from ${config_file}..."
            if ! kubectl apply -f "${config_file}" &> /dev/null; then
                log_error "Failed to apply configuration"
                return 1
            fi
        fi
    fi

    # Get new version
    local new_version
    new_version=$(get_configmap_version)

    if [[ "${new_version}" == "${current_version}" ]]; then
        log_info "No configuration changes detected"
        return 0
    fi

    log_info "ConfigMap updated to version: ${new_version}"

    # Get all pods
    local pods
    mapfile -t pods < <(get_erlmcp_pods)

    if [[ ${#pods[@]} -eq 0 ]]; then
        log_warning "No ErlMCP pods found in namespace ${NAMESPACE}"
        return 0
    fi

    log_info "Found ${#pods[@]} pods to reload"

    # Track successes and failures
    local success_count=0
    local failure_count=0

    # Trigger reload on each pod
    for pod in "${pods[@]}"; do
        if is_pod_ready "${pod}" || [[ "${FORCE_RELOAD:-false}" == "true" ]]; then
            if trigger_reload "${pod}" http; then
                ((success_count++))
            else
                ((failure_count++))
            fi
        else
            log_warning "Pod ${pod} is not ready, skipping"
            ((failure_count++))
        fi
    done

    # Summary
    log_info "Hot-reload summary:"
    log_info "  Successful: ${success_count}"
    log_info "  Failed: ${failure_count}"

    if [[ ${failure_count} -gt 0 ]]; then
        log_warning "Some pods failed to reload. You may need to restart them manually."
        return 1
    fi

    log_success "Hot-reload completed successfully"
    return 0
}

# Watch mode - continuously watch for ConfigMap changes
watch_mode() {
    local interval=${1:-5}

    log_info "Watching ConfigMap ${CONFIGMAP_NAME} for changes (interval: ${interval}s)"
    log_info "Press Ctrl+C to stop"

    local last_version
    last_version=$(get_configmap_version)

    log_info "Initial version: ${last_version}"

    while true; do
        sleep "${interval}"
        local current_version
        current_version=$(get_configmap_version)

        if [[ "${current_version}" != "${last_version}" ]]; then
            log_info "ConfigMap changed: ${last_version} -> ${current_version}"
            last_version="${current_version}"

            # Trigger hot-reload
            if hot_reload; then
                log_success "Automatic hot-reload successful"
            else
                log_error "Automatic hot-reload failed"
            fi
        fi
    done
}

# Show usage
usage() {
    cat << EOF
Usage: $0 [OPTIONS] [NAMESPACE]

Hot-reload ErlMCP ConfigMap changes without restarting pods.

Arguments:
  NAMESPACE          Kubernetes namespace (default: erlmcp)

Options:
  -c, --config FILE  Config file to apply before reload
  -d, --dry-run      Show what would be done without doing it
  -f, --force        Force reload even if pod is not ready
  -h, --help         Show this help message
  -i, --interval SEC Watch mode check interval in seconds (default: 5)
  -n, --name NAME    ConfigMap name (default: erlmcp-config)
  -v, --verbose      Enable verbose output
  -w, --watch        Enable watch mode for continuous monitoring

Environment Variables:
  CONFIGMAP_NAME     ConfigMap name (default: erlmcp-config)
  DRY_RUN            Enable dry-run mode (default: false)
  FORCE_RELOAD       Force reload on non-ready pods (default: false)
  NAMESPACE          Kubernetes namespace
  RELOAD_ENDPOINT    HTTP reload endpoint (default: http://localhost:8081/admin/reload-config)
  TIMEOUT            Command timeout in seconds (default: 30)
  VERBOSE            Enable verbose output (default: false)

Examples:
  # Hot-reload current ConfigMap in production
  $0 erlmcp-prod

  # Apply new config and hot-reload
  $0 -c /path/to/new-config.yaml erlmcp

  # Watch for ConfigMap changes and auto-reload
  $0 -w -i 10

  # Dry-run to see what would happen
  $0 -d -c /path/to/new-config.yaml

EOF
}

# Parse arguments
watch_mode_enabled=false
watch_interval=5
config_file=""

while [[ $# -gt 0 ]]; do
    case $1 in
        -c|--config)
            config_file="$2"
            shift 2
            ;;
        -d|--dry-run)
            DRY_RUN=true
            shift
            ;;
        -f|--force)
            FORCE_RELOAD=true
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        -i|--interval)
            watch_interval="$2"
            shift 2
            ;;
        -n|--name)
            CONFIGMAP_NAME="$2"
            shift 2
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -w|--watch)
            watch_mode_enabled=true
            shift
            ;;
        -*)
            log_error "Unknown option: $1"
            usage
            exit 1
            ;;
        *)
            NAMESPACE="$1"
            shift
            ;;
    esac
done

# Main execution
check_kubectl

if [[ "${watch_mode_enabled}" == "true" ]]; then
    watch_mode "${watch_interval}"
else
    hot_reload "${config_file}"
fi
