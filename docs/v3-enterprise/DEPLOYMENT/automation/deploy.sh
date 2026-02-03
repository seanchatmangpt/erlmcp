#!/bin/bash

# erlmcp v3 Automated Deployment Script
# For enterprise deployment automation

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
ENV_FILE="${ROOT_DIR}/config/.env"
LOG_FILE="${ROOT_DIR}/logs/deployment.log"
BACKUP_DIR="${ROOT_DIR}/backups"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

error() {
    log "${RED}[ERROR] $1${NC}"
    exit 1
}

success() {
    log "${GREEN}[SUCCESS] $1${NC}"
}

warning() {
    log "${YELLOW}[WARNING] $1${NC}"
}

info() {
    log "${BLUE}[INFO] $1${NC}"
}

# Load environment variables
load_env() {
    if [ -f "$ENV_FILE" ]; then
        info "Loading environment from $ENV_FILE"
        export $(cat "$ENV_FILE" | xargs)
    else
        error "Environment file not found: $ENV_FILE"
    fi
}

# Pre-flight checks
preflight_checks() {
    info "Running pre-flight checks..."

    # Check required commands
    local required_commands=("docker" "kubectl" "helm" "jq" "yq")
    for cmd in "${required_commands[@]}"; do
        if ! command -v "$cmd" &> /dev/null; then
            error "Required command not found: $cmd"
        fi
    done

    # Check Docker daemon
    if ! docker info &> /dev/null; then
        error "Docker daemon is not running"
    fi

    # Check kubectl context
    if ! kubectl cluster-info &> /dev/null; then
        error "kubectl not connected to any cluster"
    fi

    # Check Helm releases
    if helm list -q | grep -q "erlmcp"; then
        warning "Helm release 'erlmcp' already exists"
    fi

    success "Pre-flight checks passed"
}

# Backup existing deployment
backup_deployment() {
    info "Creating backup of existing deployment..."

    # Create backup directory
    mkdir -p "$BACKUP_DIR"
    local backup_name="erlmcp-backup-$(date +%Y%m%d-%H%M%S)"
    local backup_path="$BACKUP_DIR/$backup_name"
    mkdir -p "$backup_path"

    # Backup Kubernetes resources
    info "Backing up Kubernetes resources..."
    helm get values erlmcp -n erlmcp > "$backup_path/helm-values.yaml" 2>/dev/null || true
    kubectl get all -n erlmcp -o yaml > "$backup_path/k8s-resources.yaml" 2>/dev/null || true

    # Backup config files
    info "Backing up configuration files..."
    cp -r "${ROOT_DIR}/config" "$backup_path/"

    # Create backup manifest
    cat > "$backup_path/manifest.json" << EOF
{
    "backup_name": "$backup_name",
    "created_at": "$(date -Iseconds)",
    "version": "3.0.0",
    "backup_path": "$backup_path"
}
EOF

    info "Backup created at: $backup_path"
}

# Deploy erlmcp using Helm
deploy_erlmcp() {
    info "Deploying erlmcp v3..."

    # Create namespace if it doesn't exist
    kubectl create namespace erlmcp --dry-run=client -o yaml | kubectl apply -f -

    # Install or upgrade Helm release
    if helm list -q | grep -q "erlmcp"; then
        info "Upgrading existing erlmcp release..."
        helm upgrade erlmcp "${ROOT_DIR}/charts/erlmcp" \
            --namespace erlmcp \
            --values "${ROOT_DIR}/config/production.yaml" \
            --wait \
            --timeout=600s
    else
        info "Installing new erlmcp release..."
        helm install erlmcp "${ROOT_DIR}/charts/erlmcp" \
            --namespace erlmcp \
            --values "${ROOT_DIR}/config/production.yaml" \
            --wait \
            --timeout=600s
    fi

    # Verify deployment
    verify_deployment
}

# Verify deployment status
verify_deployment() {
    info "Verifying deployment..."

    # Check pod status
    local pod_status=$(kubectl get pods -n erlmcp -l app=erlmcp -o json | jq -r '.items[0].status.phase' 2>/dev/null || echo "Unknown")

    if [ "$pod_status" != "Running" ]; then
        error "Pod status is $pod_status, expected Running"
    fi

    # Check health endpoint
    local health_status=$(kubectl get -n erlmcp svc erlmcp -o jsonpath='{.spec.clusterIP}')
    if [ -n "$health_status" ]; then
        local health_check=$(curl -s -k "https://$health_status:8443/health" 2>/dev/null || echo "Failed")
        if [ "$health_check" != "healthy" ]; then
            warning "Health check returned: $health_check"
        else
            success "Health check passed"
        fi
    fi

    # Check ingress
    if kubectl get ingress -n erlmcp -l app=erlmcp &> /dev/null; then
        local ingress_ip=$(kubectl get ingress -n erlmcp -l app=erlmcp -o jsonpath='{.items[0].status.loadBalancer.ingress[0].ip}')
        if [ -n "$ingress_ip" ]; then
            info "Ingress IP: $ingress_ip"
        fi
    fi
}

# Apply post-deployment configurations
apply_post_deploy() {
    info "Applying post-deployment configurations..."

    # Apply network policies
    if [ -f "${ROOT_DIR}/config/network-policy.yaml" ]; then
        kubectl apply -f "${ROOT_DIR}/config/network-policy.yaml" -n erlmcp
    fi

    # Apply secrets
    if [ -f "${ROOT_DIR}/config/secrets.yaml" ]; then
        kubectl apply -f "${ROOT_DIR}/config/secrets.yaml" -n erlmcp
    fi

    # Apply monitoring
    if [ -f "${ROOT_DIR}/config/monitoring.yaml" ]; then
        kubectl apply -f "${ROOT_DIR}/config/monitoring.yaml" -n monitoring
    fi

    # Initialize database if needed
    if [ "${INIT_DATABASE:-false}" = "true" ]; then
        info "Initializing database..."
        kubectl exec -n erlmcp deployment/erlmcp -- erl -pa /app/lib/*/ebin -eval 'erlmcp_db:init(), halt().' 2>/dev/null || true
    fi
}

# Run smoke tests
run_smoke_tests() {
    info "Running smoke tests..."

    # Wait for deployment to be ready
    kubectl wait --for=condition=ready pod -n erlmcp -l app=erlmcp --timeout=300s

    # Get service IP
    local service_ip=$(kubectl get svc -n erlmcp erlmcp -o jsonpath='{.spec.clusterIP}')

    # Test health endpoint
    if curl -s -f "http://$service_ip:8080/health" > /dev/null; then
        success "Health endpoint test passed"
    else
        warning "Health endpoint test failed"
    fi

    # Test API endpoint
    if curl -s -f "http://$service_ip:8080/resources" > /dev/null; then
        success "API endpoint test passed"
    else
        warning "API endpoint test failed"
    fi

    # Check logs for errors
    local error_count=$(kubectl logs -n erlmcp deployment/erlmcp --tail=100 | grep -i "error" | wc -l)
    if [ "$error_count" -gt 0 ]; then
        warning "Found $error_count error(s) in logs"
    else
        success "No errors found in logs"
    fi
}

# Deploy monitoring stack
deploy_monitoring() {
    info "Deploying monitoring stack..."

    # Create monitoring namespace
    kubectl create namespace monitoring --dry-run=client -o yaml | kubectl apply -f -

    # Install Prometheus
    helm repo add prometheus-community https://prometheus-community.github.io/helm-charts
    helm repo update

    if ! helm list -q | grep -q "prometheus"; then
        helm install prometheus prometheus-community/kube-prometheus-stack \
            --namespace monitoring \
            --set grafana.enabled=false \
            --set prometheus.prometheusSpec.serviceMonitorSelectorNilUsesHelmValues=false
    fi

    # Install Grafana
    if ! helm list -q | grep -q "grafana"; then
        helm install grafana prometheus-community/grafana \
            --namespace monitoring \
            --set persistence.enabled=true
    fi

    # Apply erlmcp monitoring
    kubectl apply -f "${ROOT_DIR}/config/erlmcp-monitoring.yaml" -n monitoring

    info "Monitoring stack deployed"
}

# Deploy logging stack
deploy_logging() {
    info "Deploying logging stack..."

    # Create logging namespace
    kubectl create namespace logging --dry-run=client -o yaml | kubectl apply -f -

    # Deploy Fluentd
    kubectl apply -f "${ROOT_DIR}/config/fluentd-config.yaml" -n logging

    # Deploy Elasticsearch
    helm repo add elastic https://helm.elastic.co
    helm repo update

    if ! helm list -q | grep -q "elasticsearch"; then
        helm install elasticsearch elastic/elasticsearch \
            --namespace logging \
            --set replicas=1 \
            --set persistence.enabled=true \
            --set persistence.size=10Gi
    fi

    # Deploy Kibana
    if ! helm list -q | grep -q "kibana"; then
        helm install kibana elastic/kibana \
            --namespace logging \
            --set replicaCount=1
    fi

    info "Logging stack deployed"
}

# Clean up old deployments
cleanup() {
    info "Cleaning up old deployments..."

    # Keep last 5 backups
    cd "$BACKUP_DIR"
    ls -t | tail -n +6 | xargs -r rm -rf

    # Clean up failed pods
    kubectl delete pod -n erlmcp --field-selector=status.phase=Failed --ignore-not-found=true

    # Clean up unused resources
    kubectl get service,configmap,secret,pvc -n erlmcp -o name | \
        xargs -r kubectl delete --ignore-not-found=true

    success "Cleanup completed"
}

# Rollback deployment
rollback() {
    info "Rolling back deployment..."

    if ! helm list -q | grep -q "erlmcp"; then
        error "No erlmcp release found to rollback"
    fi

    # Get last successful backup
    local backup_path=$(ls -t "$BACKUP_DIR" | head -n 1)
    if [ -z "$backup_path" ]; then
        error "No backup found for rollback"
    fi

    # Rollback Helm release
    helm rollback erlmcp 1 --namespace erlmcp --wait

    # Restore configuration
    if [ -f "$BACKUP_DIR/$backup_path/helm-values.yaml" ]; then
        helm upgrade erlmcp "${ROOT_DIR}/charts/erlmcp" \
            --namespace erlmcp \
            --values "$BACKUP_DIR/$backup_path/helm-values.yaml" \
            --wait
    fi

    success "Rollback completed"
}

# Main deployment function
main() {
    log "Starting erlmcp v3 deployment..."

    # Parse command line arguments
    local action="deploy"
    local skip_monitoring=false
    local skip_logging=false
    local skip_tests=false

    while [[ $# -gt 0 ]]; do
        case $1 in
            --help|-h)
                echo "Usage: $0 [deploy|rollback|backup] [options]"
                echo "Options:"
                echo "  --skip-monitoring  Skip monitoring deployment"
                echo "  --skip-logging     Skip logging deployment"
                echo "  --skip-tests       Skip smoke tests"
                exit 0
                ;;
            deploy)
                action="deploy"
                shift
                ;;
            rollback)
                action="rollback"
                shift
                ;;
            backup)
                action="backup"
                shift
                ;;
            --skip-monitoring)
                skip_monitoring=true
                shift
                ;;
            --skip-logging)
                skip_logging=true
                shift
                ;;
            --skip-tests)
                skip_tests=true
                shift
                ;;
            *)
                error "Unknown option: $1"
                ;;
        esac
    done

    # Load environment
    load_env

    case $action in
        deploy)
            preflight_checks
            backup_deployment
            deploy_erlmcp
            apply_post_deploy
            if [ "$skip_monitoring" = false ]; then
                deploy_monitoring
            fi
            if [ "$skip_logging" = false ]; then
                deploy_logging
            fi
            if [ "$skip_tests" = false ]; then
                run_smoke_tests
            fi
            success "Deployment completed successfully"
            ;;
        rollback)
            rollback
            ;;
        backup)
            backup_deployment
            ;;
    esac
}

# Run main function
main "$@"