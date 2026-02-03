#!/bin/bash
# ERLMCP Enterprise Helm Deployment Script
# Usage: ./deploy.sh [environment] [namespace]
#   environment: dev, staging, production (default: production)
#   namespace: Kubernetes namespace (default: erlmcp)

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default values
ENVIRONMENT="${1:-production}"
NAMESPACE="${2:-erlmcp}"
RELEASE_NAME="erlmcp-${ENVIRONMENT}"
CHART_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/helm/erlmcp-enterprise"
VALUES_FILE="${CHART_DIR}/values-${ENVIRONMENT}.yaml"

# Functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_helm() {
    if ! command -v helm &> /dev/null; then
        log_error "Helm is not installed. Please install Helm first."
        exit 1
    fi
    log_info "Helm version: $(helm version --short 2>/dev/null || echo 'unknown')"
}

check_kubectl() {
    if ! command -v kubectl &> /dev/null; then
        log_error "kubectl is not installed. Please install kubectl first."
        exit 1
    fi
    log_info "kubectl version: $(kubectl version --client --short 2>/dev/null || echo 'unknown')"
}

check_cluster() {
    if ! kubectl cluster-info &> /dev/null; then
        log_error "Cannot connect to Kubernetes cluster. Please configure kubeconfig."
        exit 1
    fi
    log_info "Connected to cluster: $(kubectl config current-context 2>/dev/null || echo 'unknown')"
}

create_namespace() {
    if ! kubectl get namespace "${NAMESPACE}" &> /dev/null; then
        log_info "Creating namespace: ${NAMESPACE}"
        kubectl create namespace "${NAMESPACE}"
    else
        log_info "Namespace already exists: ${NAMESPACE}"
    fi
}

install_chart() {
    log_info "Deploying ERLMCP ${ENVIRONMENT} to namespace ${NAMESPACE}"
    log_info "Release name: ${RELEASE_NAME}"
    log_info "Chart: ${CHART_DIR}"
    log_info "Values: ${VALUES_FILE}"

    # Check if release already exists
    if helm list -n "${NAMESPACE}" | grep -q "^${RELEASE_NAME}"; then
        log_warn "Release ${RELEASE_NAME} already exists. Upgrading..."
        helm upgrade "${RELEASE_NAME}" "${CHART_DIR}" \
            --namespace "${NAMESPACE}" \
            --values "${VALUES_FILE}" \
            --wait \
            --timeout 10m \
            --atomic \
            --debug
    else
        log_info "Installing new release..."
        helm install "${RELEASE_NAME}" "${CHART_DIR}" \
            --namespace "${NAMESPACE}" \
            --values "${VALUES_FILE}" \
            --wait \
            --timeout 10m \
            --atomic \
            --create-namespace \
            --debug
    fi
}

uninstall_chart() {
    log_info "Uninstalling ERLMCP ${ENVIRONMENT} from namespace ${NAMESPACE}"
    helm uninstall "${RELEASE_NAME}" --namespace "${NAMESPACE}" || log_warn "Release not found"
}

show_status() {
    log_info "Deployment status:"
    helm status "${RELEASE_NAME}" --namespace "${NAMESPACE}"

    log_info "Pods:"
    kubectl get pods -n "${NAMESPACE}" -l "app.kubernetes.io/name=erlmcp-${ENVIRONMENT}"
}

show_logs() {
    log_info "Showing logs (press Ctrl+C to exit)..."
    kubectl logs -n "${NAMESPACE}" -l "app.kubernetes.io/name=erlmcp-${ENVIRONMENT}" --tail=100 -f
}

run_tests() {
    log_info "Running tests..."
    kubectl run "erlmcp-test-${ENVIRONMENT}-$(date +%s)" \
        --namespace="${NAMESPACE}" \
        --rm -it \
        --image=curlimages/curl:latest \
        --restart=Never \
        -- curl -X GET "http://erlmcp-${ENVIRONMENT}:8080/health" || true
}

# Main script
case "${3:-}" in
    uninstall)
        check_helm
        check_cluster
        uninstall_chart
        ;;
    status)
        check_helm
        check_kubectl
        show_status
        ;;
    logs)
        check_kubectl
        show_logs
        ;;
    test)
        check_kubectl
        check_cluster
        run_tests
        ;;
    *)
        check_helm
        check_kubectl
        check_cluster
        create_namespace
        install_chart
        show_status
        ;;
esac

log_info "Done!"
