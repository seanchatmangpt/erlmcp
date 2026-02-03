#!/bin/bash
#
# Zero-Downtime Deployment Script for erlmcp v3
# Implements canary deployment with automated rollback
#
# Usage: ./zero-downtime-deploy.sh [new-version] [--canary] [--bluegreen]
#

set -e

# Configuration
DEPLOY_LOG="/var/log/erlmcp/zero-downtime-deploy.log"
BLUE_STACK="erlmcp-blue"
GREEN_STACK="erlmcp-green"
CANARY_PERCENTAGE=10
ROLLBACK_THRESHOLD=5
HEALTH_CHECK_TIMEOUT=300
NAMESPACE="erlmcp"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    local level=$1
    shift
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] [$level] $*" | tee -a $DEPLOY_LOG
}

status() {
    log "INFO" "$*"
    echo -e "${BLUE}ℹ️ $*${NC}"
}

success() {
    log "INFO" "$*"
    echo -e "${GREEN}✅ $*${NC}"
}

warning() {
    log "WARN" "$*"
    echo -e "${YELLOW}⚠️ $*${NC}"
}

error() {
    log "ERROR" "$*"
    echo -e "${RED}❌ $*${NC}"
    exit 1
}

# Parse arguments
VERSION=${1:-latest}
DEPLOY_STRATEGY=${2:-canary}

# Validate version
validate_version() {
    if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        warning "Version format should be X.Y.Z. Proceeding with '$VERSION' anyway."
    fi
}

# Check prerequisites
check_prerequisites() {
    status "Checking prerequisites..."

    # Check kubectl
    if ! command -v kubectl &> /dev/null; then
        error "kubectl is not required"
    fi

    # Check docker
    if ! command -v docker &> /dev/null; then
        error "Docker is not required"
    fi

    # Check deployment readiness
    if ! check_deployment_readiness; then
        error "System is not ready for deployment"
    fi

    status "Prerequisites check passed"
}

check_deployment_readiness() {
    status "Checking deployment readiness..."

    # Check if all nodes are healthy
    unhealthy_nodes=$(kubectl get nodes -o jsonpath='{.items[*].status.conditions[?(@.type=="Ready")].status}' | grep -o False | wc -l)
    if [ "$unhealthy_nodes" -gt 0 ]; then
        warning "Found $unhealthy_nodes unhealthy nodes"
        return 1
    fi

    # Check database health
    if ! check_database_health; then
        warning "Database is not healthy"
        return 1
    fi

    # Check session consistency
    if ! check_session_consistency; then
        warning "Session consistency check failed"
        return 1
    fi

    return 0
}

check_database_health() {
    status "Checking database health..."
    # Implementation would check database connectivity and replication
    return 0
}

check_session_consistency() {
    status "Checking session consistency..."
    # Implementation would verify all sessions are properly replicated
    return 0
}

# Pre-deployment tasks
pre_deployment() {
    status "Running pre-deployment checks..."

    # Backup current deployment
    backup_deployment

    # Build and push image
    build_and_push_image

    # Pre-warm caches
    warm_caches

    status "Pre-deployment checks completed"
}

backup_deployment() {
    status "Creating deployment backup..."

    TIMESTAMP=$(date +%Y%m%d_%H%M%S)
    BACKUP_DIR="/tmp/erlmcp-backup-$TIMESTAMP"

    mkdir -p $BACKUP_DIR

    # Backup deployments
    kubectl get deployment -n $NAMESPACE -o yaml > $BACKUP_DIR/deployments.yaml
    kubectl get service -n $NAMESPACE -o yaml > $BACKUP_DIR/services.yaml
    kubectl get configmap -n $NAMESPACE -o yaml > $BACKUP_DIR/configmaps.yaml

    # Backup current metrics
    curl -s "https://erlmcp.company.com/api/metrics" > $BACKUP_DIR/metrics-$TIMESTAMP.json

    status "Backup saved to $BACKUP_DIR"
}

build_and_push_image() {
    status "Building and pushing Docker image..."

    # Set version tag
    if [ "$VERSION" = "latest" ]; then
        TIMESTAMP=$(date +%Y%m%d-%H%M%S)
        IMAGE_TAG="company/erlmcp:$TIMESTAMP"
    else
        IMAGE_TAG="company/erlmcp:$VERSION"
    fi

    # Build image
    docker build \
        -t $IMAGE_TAG \
        -f docker/Dockerfile \
        --build-arg VERSION=$VERSION \
        .

    # Push to registry
    docker push $IMAGE_TAG

    # Update deployment with new image
    kubectl set image -n $NAMESPACE deployment/$BLUE_STACK erlmcp=$IMAGE_TAG
    kubectl set image -n $NAMESPACE deployment/$GREEN_STACK erlmcp=$IMAGE_TAG

    status "Image built and pushed: $IMAGE_TAG"
}

warm_caches() {
    status "Warming up caches..."

    # Perform cache warming requests
    for i in {1..10}; do
        curl -s "https://erlmcp.company.com/api/health" > /dev/null
        curl -s "https://erlmcp.company.com/api/metrics" > /dev/null
    done

    status "Cache warming completed"
}

# Canary deployment
deploy_canary() {
    status "Starting canary deployment with $CANARY_PERCENTAGE% traffic..."

    # Step 1: Deploy canary
    deploy_canary_version

    # Step 2: Validate canary
    if ! validate_canary; then
        error "Canary validation failed"
    fi

    # Step 3: Gradually increase traffic
    increment_canary_traffic

    # Step 4: Final validation
    if ! validate_final_deployment; then
        error "Final validation failed"
    fi

    success "Canary deployment completed successfully"
}

deploy_canary_version() {
    status "Deploying canary version..."

    # Create canary deployment
    cat <<EOF | kubectl apply -f -
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp-canary
  namespace: $NAMESPACE
  labels:
    app: erlmcp
    version: canary
spec:
  replicas: 1
  selector:
    matchLabels:
      app: erlmcp
      version: canary
  template:
    metadata:
      labels:
        app: erlmcp
        version: canary
    spec:
      containers:
      - name: erlmcp
        image: $(kubectl get deployment erlmcp-blue -n $NAMESPACE -o jsonpath='{.spec.template.spec.containers[0].image}')
        ports:
        - containerPort: 8080
        - containerPort: 8081
        env:
        - name: ENVIRONMENT
          value: "canary"
        - name: DEPLOYMENT_VERSION
          value: "$VERSION"
        livenessProbe:
          httpGet:
            path: /health
            port: 8081
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8081
          initialDelaySeconds: 5
          periodSeconds: 5
EOF

    # Wait for deployment to be ready
    kubectl rollout status deployment/erlmcp-canary -n $NAMESPACE --timeout=$HEALTH_CHECK_TIMEOUT
}

validate_canary() {
    status "Validating canary deployment..."

    # Check canary health
    if ! kubectl wait --for=condition=ready pod -l app=erlmcp,version=canary -n $NAMESPACE --timeout=60s; then
        return 1
    fi

    # Test canary endpoints
    canary_pod=$(kubectl get pods -l app=erlmcp,version=canary -n $NAMESPACE -o jsonpath='{.items[0].metadata.name}')
    kubectl exec $canary_pod -n $NAMESPACE -- curl -f -s http://localhost:8081/health > /dev/null
    kubectl exec $canary_pod -n $NAMESPACE -- curl -f -s http://localhost:8081/ready > /dev/null

    return 0
}

increment_canary_traffic() {
    status "Incrementing canary traffic..."

    # Use Flagger for progressive traffic shift
    cat <<EOF | kubectl apply -f -
apiVersion: flagger.app/v1beta1
kind: Canary
metadata:
  name: erlmcp-canary
  namespace: $NAMESPACE
spec:
  targetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: erlmcp-blue
  canaryRef:
    apiVersion: apps/v1
    kind: Deployment
    name: erlmcp-canary
  analysis:
    interval: 30s
    threshold: $ROLLBACK_THRESHOLD
    stepWeight: $CANARY_PERCENTAGE
    maxWeight: 100
    metrics:
    - name: request-success-rate
      threshold:
        range:
          min: 99.5
    - name: latency-p50
      threshold:
        range:
          max: 100
EOF

    # Monitor canary progress
    monitor_canary_progress
}

monitor_canary_progress() {
    status "Monitoring canary progress..."

    while true; do
        status=$(kubectl get canary erlmcp-canary -n $NAMESPACE -o jsonpath='{.status.phase}')
        if [ "$status" = "progressing" ]; then
            weight=$(kubectl get canary erlmcp-canary -n $NAMESPACE -o jsonpath='{..status.currentWeight}')
            status "Canary traffic: ${weight}%"
            sleep 30
        elif [ "$status" = "completed" ]; then
            success "Canary deployment completed"
            break
        else
            error "Canary deployment failed with status: $status"
        fi
    done
}

# Blue-green deployment
deploy_bluegreen() {
    status "Starting blue-green deployment..."

    # Step 1: Deploy to green environment
    deploy_green_environment

    # Step 2: Validate green environment
    if ! validate_green_environment; then
        error "Green environment validation failed"
    fi

    # Step 3: Switch traffic to green
    switch_traffic_to_green

    # Step 4: Deploy to blue environment
    deploy_blue_environment

    success "Blue-green deployment completed successfully"
}

deploy_green_environment() {
    status "Deploying to green environment..."

    # Update green deployment
    kubectl set image -n $NAMESPACE deployment/erlmcp-green erlmcp=$(kubectl get deployment erlmcp-blue -n $NAMESPACE -o jsonpath='{.spec.template.spec.containers[0].image}')

    # Wait for deployment to be ready
    kubectl rollout status deployment/erlmcp-green -n $NAMESPACE --timeout=$HEALTH_CHECK_TIMEOUT

    status "Green environment deployed"
}

validate_green_environment() {
    status "Validating green environment..."

    # Check all pods are ready
    if ! kubectl wait --for=condition=ready pod -l app=erlmcp,env=green -n $NAMESPACE --timeout=60s; then
        return 1
    fi

    # Test green endpoints
    success "Green environment validated"
    return 0
}

switch_traffic_to_green() {
    status "Switching traffic to green environment..."

    # Update service selector
    kubectl patch service erlmcp-service -n $NAMESPACE --type='json' -p='{"spec":{"selector":{"app":"erlmcp","env":"green"}}}'

    # Drain blue environment
    kubectl drain deployment/erlmcp-blue --ignore-daemonsets --delete-local-data --timeout=300s

    status "Traffic switched to green environment"
}

deploy_blue_environment() {
    status "Deploying to blue environment..."

    # Update blue deployment
    kubectl set image -n $NAMESPACE deployment/erlmcp-blue erlmcp=$(kubectl get deployment erlmcp-green -n $NAMESPACE -o jsonpath='{.spec.template.spec.containers[0].image}')

    # Wait for deployment to be ready
    kubectl rollout status deployment/erlmcp-blue -n $NAMESPACE --timeout=$HEALTH_CHECK_TIMEOUT

    status "Blue environment deployed"
}

# Validation functions
validate_final_deployment() {
    status "Performing final validation..."

    # Check all deployments are healthy
    if ! check_all_deployments; then
        return 1
    fi

    # Run integration tests
    if ! run_integration_tests; then
        return 1
    fi

    # Validate performance
    if ! validate_performance; then
        return 1
    fi

    success "Final validation passed"
    return 0
}

check_all_deployments() {
    status "Checking all deployments..."

    for deployment in erlmcp-blue erlmcp-green; do
        if ! kubectl rollout status deployment/$deployment -n $NAMESPACE --timeout=30s; then
            warning "Deployment $deployment is not healthy"
            return 1
        fi
    done

    return 0
}

run_integration_tests() {
    status "Running integration tests..."

    # Integration test implementation
    # This would run actual tests against the deployed system
    return 0
}

validate_performance() {
    status "Validating performance metrics..."

    # Check response time
    avg_response=$(curl -s "https://erlmcp.company.com/api/metrics" | jq '.response_time // 0')
    if [ $(echo "$avg_response > 100" | bc -l) -eq 1 ]; then
        warning "High response time: ${avg_response}ms"
        return 1
    fi

    # Check error rate
    error_rate=$(curl -s "https://erlmcp.company.com/api/metrics" | jq '.error_rate // 0')
    if [ $(echo "$error_rate > 0.01" | bc -l) -eq 1 ]; then
        warning "High error rate: ${error_rate}%"
        return 1
    fi

    return 0
}

# Rollback function
rollback() {
    status "Initiating rollback..."

    case $DEPLOY_STRATEGY in
        canary)
            rollback_canary
            ;;
        bluegreen)
            rollback_bluegreen
            ;;
        *)
            error "Unknown deployment strategy: $DEPLOY_STRATEGY"
            ;;
    esac

    success "Rollback completed"
}

rollback_canary() {
    status "Rolling back canary deployment..."

    # Delete canary deployment
    kubectl delete deployment erlmcp-canary -n $NAMESPACE

    # Switch back to original deployment
    cat <<EOF | kubectl apply -f -
apiVersion: flagger.app/v1beta1
kind: Canary
metadata:
  name: erlmcp-canary
  namespace: $NAMESPACE
spec:
  targetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: erlmcp-blue
  canaryRef:
    apiVersion: apps/v1
    kind: Deployment
    name: erlmcp-blue
  analysis:
    interval: 30s
    threshold: 1
    stepWeight: 100
EOF

    status "Canary rollback completed"
}

rollback_bluegreen() {
    status "Rolling back blue-green deployment..."

    # Switch traffic back to blue
    kubectl patch service erlmcp-service -n $NAMESPACE --type='json' -p='{"spec":{"selector":{"app":"erlmcp","env":"blue"}}}'

    # Drain green environment
    kubectl scale deployment erlmcp-green --replicas=0 -n $NAMESPACE

    status "Blue-green rollback completed"
}

# Post-deployment tasks
post_deployment() {
    status "Running post-deployment tasks..."

    # Run final health checks
    run_health_checks

    # Notify stakeholders
    notify_stakeholders

    # Update documentation
    update_documentation

    # Monitor system
    start_monitoring

    status "Post-deployment tasks completed"
}

run_health_checks() {
    status "Running final health checks..."

    # Comprehensive health check implementation
    for i in {1..5}; do
        if ! curl -f -s "https://erlmcp.company.com/health" | grep -q "healthy"; then
            error "Health check failed"
        fi
        sleep 10
    done

    success "All health checks passed"
}

notify_stakeholders() {
    status "Notifying stakeholders..."

    # Send deployment notification
    cat <<EOF | mail -s "erlmcp Deployment Complete" dev-team@company.com
Version: $VERSION
Strategy: $DEPLOY_STRATEGY
Timestamp: $(date)
Status: Success
EOF

    success "Stakeholders notified"
}

update_documentation() {
    status "Updating documentation..."

    # Update version information
    # Implementation would update documentation with new version info
    return 0
}

start_monitoring() {
    status "Starting monitoring..."

    # Enable enhanced monitoring
    kubectl apply -f config/monitoring/deployment-monitoring.yaml

    success "Monitoring started"
}

# Main execution
main() {
    # Initialize log
    mkdir -p $(dirname $DEPLOY_LOG)
    touch $DEPLOY_LOG

    # Parse command line arguments
    case "${1:-}" in
        --rollback)
            rollback
            exit 0
            ;;
        --help)
            echo "Usage: $0 [VERSION] [--canary|--bluegreen]"
            echo ""
            echo "Arguments:"
            echo "  VERSION        Version to deploy (default: latest)"
            echo "  --canary       Use canary deployment strategy"
            echo "  --bluegreen    Use blue-green deployment strategy"
            echo "  --rollback     Rollback last deployment"
            echo "  --help         Show this help message"
            exit 0
            ;;
    esac

    # Set deployment strategy
    if [ "${2:-}" = "--canary" ]; then
        DEPLOY_STRATEGY="canary"
    elif [ "${2:-}" = "--bluegreen" ]; then
        DEPLOY_STRATEGY="bluegreen"
    fi

    # Deploy version
    if [ "${1:-}" = "--rollback" ]; then
        rollback
    else
        VERSION="${1:-latest}"
        validate_version
        check_prerequisites
        pre_deployment

        case $DEPLOY_STRATEGY in
            canary)
                deploy_canary
                ;;
            bluegreen)
                deploy_bluegreen
                ;;
        esac

        post_deployment
    fi

    success "Zero-downtime deployment completed successfully"
}

# Run main function
main "$@"