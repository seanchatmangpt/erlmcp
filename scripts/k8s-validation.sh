#!/bin/bash
# Kubernetes Deployment Validation Script
# Validates erlmcp deployment in Kubernetes cluster
# Usage: ./scripts/k8s-validation.sh [namespace]

set -e

NAMESPACE="${1:-erlmcp}"
TIMEOUT="${2:-300}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() { echo -e "${BLUE}ℹ️  $1${NC}"; }
log_success() { echo -e "${GREEN}✓ $1${NC}"; }
log_warning() { echo -e "${YELLOW}⚠️  $1${NC}"; }
log_error() { echo -e "${RED}✗ $1${NC}"; }

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."

    # Check kubectl
    if ! command -v kubectl &> /dev/null; then
        log_error "kubectl is not installed"
        exit 1
    fi
    log_success "kubectl found: $(kubectl version --client --short)"

    # Check cluster connectivity
    if ! kubectl cluster-info &> /dev/null; then
        log_error "Cannot connect to Kubernetes cluster"
        exit 1
    fi
    log_success "Connected to Kubernetes cluster"

    # Check namespace exists
    if ! kubectl get namespace "$NAMESPACE" &> /dev/null; then
        log_error "Namespace '$NAMESPACE' does not exist"
        exit 1
    fi
    log_success "Namespace '$NAMESPACE' exists"
}

# Validate manifests syntax
validate_manifests() {
    log_info "Validating Kubernetes manifests..."

    local manifest_dir="k8s"
    local count=0

    for file in "$manifest_dir"/*.yaml; do
        if [ -f "$file" ]; then
            if kubectl apply -f "$file" --dry-run=client --validate=true &> /dev/null; then
                log_success "Valid: $(basename "$file")"
                ((count++))
            else
                log_warning "Invalid: $(basename "$file")"
            fi
        fi
    done

    log_success "Validated $count manifest files"
}

# Check RBAC configuration
check_rbac() {
    log_info "Checking RBAC configuration..."

    # Check ServiceAccount
    if kubectl get serviceaccount erlmcp -n "$NAMESPACE" &> /dev/null; then
        log_success "ServiceAccount 'erlmcp' exists"
    else
        log_error "ServiceAccount 'erlmcp' not found"
        return 1
    fi

    # Check Role
    if kubectl get role erlmcp-role -n "$NAMESPACE" &> /dev/null; then
        log_success "Role 'erlmcp-role' exists"
    else
        log_warning "Role 'erlmcp-role' not found"
    fi

    # Check RoleBinding
    if kubectl get rolebinding erlmcp-rolebinding -n "$NAMESPACE" &> /dev/null; then
        log_success "RoleBinding 'erlmcp-rolebinding' exists"
    else
        log_warning "RoleBinding 'erlmcp-rolebinding' not found"
    fi
}

# Check network policies
check_network_policies() {
    log_info "Checking network policies..."

    local policies=$(kubectl get networkpolicy -n "$NAMESPACE" -o name 2>/dev/null | wc -l)

    if [ "$policies" -gt 0 ]; then
        log_success "Found $policies network policies"
        kubectl get networkpolicy -n "$NAMESPACE"
    else
        log_warning "No network policies found"
    fi
}

# Check ConfigMaps
check_configmaps() {
    log_info "Checking ConfigMaps..."

    # Check erlmcp-config
    if kubectl get configmap erlmcp-config -n "$NAMESPACE" &> /dev/null; then
        log_success "ConfigMap 'erlmcp-config' exists"

        # Validate config is readable
        kubectl get configmap erlmcp-config -n "$NAMESPACE" -o yaml | head -20
    else
        log_error "ConfigMap 'erlmcp-config' not found"
        return 1
    fi
}

# Check Secrets
check_secrets() {
    log_info "Checking Secrets..."

    # Check erlmcp-secrets
    if kubectl get secret erlmcp-secrets -n "$NAMESPACE" &> /dev/null; then
        log_success "Secret 'erlmcp-secrets' exists"

        # List secret keys
        local keys=$(kubectl get secret erlmcp-secrets -n "$NAMESPACE" -o jsonpath='{.data}' | grep -o '"[^"]*":' | wc -l)
        log_success "Secret contains $keys key(s)"
    else
        log_warning "Secret 'erlmcp-secrets' not found (expected for first deployment)"
    fi

    # Check TLS certificate
    if kubectl get secret erlmcp-tls -n "$NAMESPACE" &> /dev/null; then
        log_success "Secret 'erlmcp-tls' exists"
    else
        log_warning "Secret 'erlmcp-tls' not found"
    fi
}

# Check Deployment/StatefulSet
check_deployment() {
    log_info "Checking Deployment status..."

    local deployment="erlmcp-tcps"

    if kubectl get deployment "$deployment" -n "$NAMESPACE" &> /dev/null; then
        local ready=$(kubectl get deployment "$deployment" -n "$NAMESPACE" -o jsonpath='{.status.readyReplicas}')
        local desired=$(kubectl get deployment "$deployment" -n "$NAMESPACE" -o jsonpath='{.spec.replicas}')

        if [ "$ready" = "$desired" ] && [ -n "$ready" ]; then
            log_success "Deployment '$deployment' is ready ($ready/$desired replicas)"
        else
            log_warning "Deployment '$deployment' not fully ready ($ready/$desired replicas)"
        fi
    else
        log_warning "Deployment '$deployment' not found"
    fi

    # Check StatefulSet
    local statefulset="erlmcp-cluster"
    if kubectl get statefulset "$statefulset" -n "$NAMESPACE" &> /dev/null; then
        local ready=$(kubectl get statefulset "$statefulset" -n "$NAMESPACE" -o jsonpath='{.status.readyReplicas}')
        local desired=$(kubectl get statefulset "$statefulset" -n "$NAMESPACE" -o jsonpath='{.spec.replicas}')

        if [ "$ready" = "$desired" ] && [ -n "$ready" ]; then
            log_success "StatefulSet '$statefulset' is ready ($ready/$desired replicas)"
        else
            log_warning "StatefulSet '$statefulset' not fully ready ($ready/$desired replicas)"
        fi
    fi
}

# Check Pods
check_pods() {
    log_info "Checking Pod status..."

    local pods=$(kubectl get pods -n "$NAMESPACE" -l app=erlmcp -o name 2>/dev/null | wc -l)

    if [ "$pods" -gt 0 ]; then
        log_success "Found $pods erlmcp pods"

        # Check each pod status
        kubectl get pods -n "$NAMESPACE" -l app=erlmcp -o wide

        # Check for unhealthy pods
        local unhealthy=$(kubectl get pods -n "$NAMESPACE" -l app=erlmcp --field-selector=status.phase!=Running -o name 2>/dev/null | wc -l)
        if [ "$unhealthy" -gt 0 ]; then
            log_warning "Found $unhealthy non-running pods"
        fi

        # Check health status
        kubectl get pods -n "$NAMESPACE" -l app=erlmcp -o jsonpath='{range .items[*]}{.metadata.name}{"\t"}{.status.conditions[?(@.type=="Ready")].status}{"\n"}{end}'
    else
        log_error "No erlmcp pods found"
        return 1
    fi
}

# Check Services
check_services() {
    log_info "Checking Services..."

    local services=$(kubectl get services -n "$NAMESPACE" -l app=erlmcp -o name 2>/dev/null | wc -l)

    if [ "$services" -gt 0 ]; then
        log_success "Found $services erlmcp services"
        kubectl get services -n "$NAMESPACE" -l app=erlmcp
    else
        log_warning "No erlmcp services found"
    fi
}

# Check Ingress
check_ingress() {
    log_info "Checking Ingress configuration..."

    if kubectl get ingress -n "$NAMESPACE" -l app=erlmcp &> /dev/null; then
        local ingress_count=$(kubectl get ingress -n "$NAMESPACE" -l app=erlmcp -o name | wc -l)
        if [ "$ingress_count" -gt 0 ]; then
            log_success "Found $ingress_count Ingress resources"
            kubectl get ingress -n "$NAMESPACE" -l app=erlmcp
        fi
    fi
}

# Check HPA
check_hpa() {
    log_info "Checking Horizontal Pod Autoscaler..."

    if kubectl get hpa -n "$NAMESPACE" &> /dev/null; then
        local hpa_count=$(kubectl get hpa -n "$NAMESPACE" -o name | wc -l)
        if [ "$hpa_count" -gt 0 ]; then
            log_success "Found $hpa_count HPA resources"
            kubectl get hpa -n "$NAMESPACE"
        fi
    fi
}

# Check Pod Disruption Budgets
check_pdb() {
    log_info "Checking Pod Disruption Budgets..."

    if kubectl get pdb -n "$NAMESPACE" -l app=erlmcp &> /dev/null; then
        local pdb_count=$(kubectl get pdb -n "$NAMESPACE" -l app=erlmcp -o name | wc -l)
        if [ "$pdb_count" -gt 0 ]; then
            log_success "Found $pdb_count PDB resources"
            kubectl get pdb -n "$NAMESPACE" -l app=erlmcp
        fi
    else
        log_warning "No Pod Disruption Budgets found"
    fi
}

# Test connectivity
test_connectivity() {
    log_info "Testing pod connectivity..."

    local pod=$(kubectl get pods -n "$NAMESPACE" -l app=erlmcp -o name | head -1 | sed 's/pod\///')

    if [ -z "$pod" ]; then
        log_error "No pods available for testing"
        return 1
    fi

    log_info "Testing health endpoint on pod: $pod"

    if kubectl exec -n "$NAMESPACE" "$pod" -- \
        sh -c "curl -s http://localhost:8080/health" &> /dev/null; then
        log_success "Health endpoint responds"
    else
        log_warning "Health endpoint not responding (service might not be ready)"
    fi
}

# Check monitoring
check_monitoring() {
    log_info "Checking monitoring setup..."

    # Check ServiceMonitor
    if kubectl get servicemonitor -n "$NAMESPACE" &> /dev/null; then
        local sm_count=$(kubectl get servicemonitor -n "$NAMESPACE" -o name | wc -l)
        if [ "$sm_count" -gt 0 ]; then
            log_success "Found $sm_count ServiceMonitor(s)"
        fi
    fi

    # Check PrometheusRule
    if kubectl get prometheusrule -n "$NAMESPACE" &> /dev/null; then
        local pr_count=$(kubectl get prometheusrule -n "$NAMESPACE" -o name | wc -l)
        if [ "$pr_count" -gt 0 ]; then
            log_success "Found $pr_count PrometheusRule(s)"
        fi
    fi
}

# Generate summary report
generate_report() {
    log_info "Generating validation report..."

    cat > "/tmp/erlmcp-validation-$(date +%s).txt" << EOF
erlmcp Kubernetes Validation Report
===================================
Generated: $(date)
Namespace: $NAMESPACE
Cluster: $(kubectl config current-context)

Deployment Summary:
- Namespace Status: $(kubectl get namespace "$NAMESPACE" -o jsonpath='{.status.phase}')
- Pods: $(kubectl get pods -n "$NAMESPACE" -l app=erlmcp --no-headers | wc -l)
- Services: $(kubectl get svc -n "$NAMESPACE" -l app=erlmcp --no-headers | wc -l)
- ConfigMaps: $(kubectl get cm -n "$NAMESPACE" -l app=erlmcp --no-headers | wc -l)
- Secrets: $(kubectl get secrets -n "$NAMESPACE" -l app=erlmcp --no-headers | wc -l)

Pod Status:
EOF

    kubectl get pods -n "$NAMESPACE" -l app=erlmcp -o wide >> "/tmp/erlmcp-validation-$(date +%s).txt"

    log_success "Report generated"
}

# Main execution
main() {
    log_info "Starting Kubernetes validation for erlmcp in namespace '$NAMESPACE'"
    echo ""

    check_prerequisites
    echo ""

    validate_manifests
    echo ""

    check_rbac
    echo ""

    check_network_policies
    echo ""

    check_configmaps
    echo ""

    check_secrets
    echo ""

    check_deployment
    echo ""

    check_pods
    echo ""

    check_services
    echo ""

    check_ingress
    echo ""

    check_hpa
    echo ""

    check_pdb
    echo ""

    check_monitoring
    echo ""

    test_connectivity
    echo ""

    generate_report
    echo ""

    log_success "Kubernetes validation complete!"
    log_info "To view real-time logs: kubectl logs -f -l app=erlmcp -n $NAMESPACE"
    log_info "To access pod shell: kubectl exec -it <pod-name> -n $NAMESPACE -- /bin/bash"
}

# Run main function
main
