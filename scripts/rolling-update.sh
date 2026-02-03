#!/bin/bash
#
# Rolling Update Script for erlmcp v3
# Implements zero-downtime deployment with blue-green strategy
#
# Usage: ./rolling-update.sh [blue|green] [--force]
#

set -e

# Configuration
DEPLOY_LOG_FILE="/var/log/erlmcp/rolling-update.log"
BLUE_STACK="erlmcp-blue"
GREEN_STACK="erlmcp-green"
PRIMARY_REGION="us-east-1"
SERVICE_NAME="erlmcp-service"
NAMESPACE="erlmcp"
DEPLOY_TIMEOUT=300
IMAGE_TAG=${2:-latest}

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
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] [$level] $*" | tee -a $DEPLOY_LOG_FILE
}

# Status function
status() {
    log "INFO" "$*"
    echo -e "${BLUE}ℹ️ $*${NC}"
}

# Success function
success() {
    log "INFO" "$*"
    echo -e "${GREEN}✅ $*${NC}"
}

# Warning function
warning() {
    log "WARN" "$*"
    echo -e "${YELLOW}⚠️ $*${NC}"
}

# Error function
error() {
    log "ERROR" "$*"
    echo -e "${RED}❌ $*${NC}"
    exit 1
}

# Check prerequisites
check_prerequisites() {
    status "Checking prerequisites..."

    # Check kubectl
    if ! command -v kubectl &> /dev/null; then
        error "kubectl is not installed"
    fi

    # Check Docker
    if ! command -v docker &> /dev/null; then
        error "Docker is not installed"
    fi

    # Check erlmcp CLI
    if ! command -v erlmcp &> /dev/null; then
        error "erlmcp CLI is not installed"
    fi

    status "Prerequisites check passed"
}

# Build and push Docker image
build_image() {
    local target_env=$1
    status "Building Docker image for $target_env..."

    # Build image
    docker build \
        -t company/erlmcp:$IMAGE_TAG \
        -f docker/Dockerfile \
        --build-arg TARGET_ENV=$target_env \
        .

    # Push to registry
    docker push company/erlmcp:$IMAGE_TAG

    status "Docker image pushed successfully"
}

# Deploy to green environment
deploy_green() {
    status "Deploying to green environment..."

    # Update green deployment
    cat <<EOF | kubectl apply -f -
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp-green
  namespace: $NAMESPACE
spec:
  replicas: 3
  selector:
    matchLabels:
      app: erlmcp
      env: green
  template:
    metadata:
      labels:
        app: erlmcp
        env: green
    spec:
      containers:
      - name: erlmcp
        image: company/erlmcp:$IMAGE_TAG
        ports:
        - containerPort: 8080
        - containerPort: 8081
        env:
        - name: ENVIRONMENT
          value: "green"
        - name: REGION
          value: "green"
        resources:
          requests:
            memory: "4Gi"
            cpu: "2"
          limits:
            memory: "8Gi"
            cpu: "4"
        livenessProbe:
          httpGet:
            path: /health
            port: 8081
          initialDelaySeconds: 30
          periodSeconds: 10
          timeoutSeconds: 5
        readinessProbe:
          httpGet:
            path: /ready
            port: 8081
          initialDelaySeconds: 5
          periodSeconds: 5
          timeoutSeconds: 3
---
apiVersion: v1
kind: Service
metadata:
  name: erlmcp-green-service
  namespace: $NAMESPACE
spec:
  selector:
    app: erlmcp
    env: green
  ports:
  - port: 80
    targetPort: 8080
  type: ClusterIP
EOF

    # Wait for deployment to be ready
    status "Waiting for green deployment to be ready..."
    kubectl rollout status deployment/erlmcp-green -n $NAMESPACE --timeout=$DEPLOY_TIMEOUT

    # Verify health
    status "Verifying green deployment health..."
    for i in {1..30}; do
        if kubectl get pods -l app=erlmcp,env=green -n $NAMESPACE | grep -q "1/1"; then
            success "Green environment is ready"
            return 0
        fi
        status "Waiting for green environment... ($i/30)"
        sleep 10
    done

    error "Green environment failed to become ready"
}

# Switch traffic to green environment
switch_traffic_green() {
    status "Switching traffic to green environment..."

    # Update canary deployment
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
    name: erlmcp-green
  analysis:
    interval: 30s
    threshold: 10
    stepWeight: 10
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

    # Drain blue environment
    status "Draining blue environment..."
    kubectl drain deployment/erlmcp-blue --ignore-daemonsets --delete-local-data || true

    status "Traffic successfully switched to green environment"
}

# Deploy to blue environment (next cycle)
deploy_blue() {
    status "Deploying to blue environment..."

    # Update blue deployment with new image
    cat <<EOF | kubectl apply -f -
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp-blue
  namespace: $NAMESPACE
spec:
  replicas: 3
  selector:
    matchLabels:
      app: erlmcp
      env: blue
  template:
    metadata:
      labels:
        app: erlmcp
        env: blue
    spec:
      containers:
      - name: erlmcp
        image: company/erlmcp:$IMAGE_TAG
        ports:
        - containerPort: 8080
        - containerPort: 8081
        env:
        - name: ENVIRONMENT
          value: "blue"
        - name: REGION
          value: "blue"
        resources:
          requests:
            memory: "4Gi"
            cpu: "2"
          limits:
            memory: "8Gi"
            cpu: "4"
        livenessProbe:
          httpGet:
            path: /health
            port: 8081
          initialDelaySeconds: 30
          periodSeconds: 10
          timeoutSeconds: 5
        readinessProbe:
          httpGet:
            path: /ready
            port: 8081
          initialDelaySeconds: 5
          periodSeconds: 5
          timeoutSeconds: 3
EOF

    # Wait for deployment to be ready
    status "Waiting for blue deployment to be ready..."
    kubectl rollout status deployment/erlmcp-blue -n $NAMESPACE --timeout=$DEPLOY_TIMEOUT

    success "Blue environment updated successfully"
}

# Rollback function
rollback() {
    status "Initiating rollback..."

    # Get current canary status
    CURRENT_CANARY=$(kubectl get canary erlmcp-canary -n $NAMESPACE -o jsonpath='{.spec.targetRef.name}')

    if [ "$CURRENT_CANARY" = "erlmcp-blue" ]; then
        # Rollback from blue to green
        status "Rolling back from blue to green..."
        kubectl delete canary erlmcp-canary -n $NAMESPACE || true
        kubectl set deployment erlmcp-blue --image=company/erlmcp:rollback -n $NAMESPACE || true
    else
        # Rollback from green to blue
        status "Rolling back from green to blue..."
        kubectl delete canary erlmcp-canary -n $NAMESPACE || true
        kubectl set deployment erlmcp-green --image=company/erlmcp:rollback -n $NAMESPACE || true
    fi

    status "Rollback initiated successfully"
}

# Performance validation
validate_performance() {
    status "Validating performance metrics..."

    # Check response time
    local avg_response_time=$(curl -s "https://erlmcp.company.com/api/metrics" | jq '.response_time // 0')
    if [ $(echo "$avg_response_time > 100" | bc -l) -eq 1 ]; then
        warning "Average response time is high: ${avg_response_time}ms"
    fi

    # Check error rate
    local error_rate=$(curl -s "https://erlmcp.company.com/api/metrics" | jq '.error_rate // 0')
    if [ $(echo "$error_rate > 0.01" | bc -l) -eq 1 ]; then
        warning "Error rate is high: ${error_rate}%"
    fi

    # Check memory usage
    local memory_usage=$(kubectl top pods -l app=erlmcp -n $NAMESPACE --no-headers | awk '{print $3}' | cut -d'%' -f1 | sort -nr | head -1)
    if [ ${memory_usage:-0} -gt 85 ]; then
        warning "Memory usage is high: ${memory_usage}%"
    fi

    success "Performance validation completed"
}

# Main deployment function
deploy() {
    local target_env=$1
    local force=$2

    log "INFO" "Starting rolling update to $target_env environment"

    # Check prerequisites
    check_prerequisites

    # Build image
    build_image $target_env

    # Deploy to target environment
    case $target_env in
        green)
            deploy_green
            switch_traffic_green
            deploy_blue
            ;;
        blue)
            deploy_blue
            switch_traffic_blue
            deploy_green
            ;;
        *)
            error "Invalid target environment: $target_env"
            ;;
    esac

    # Validate performance
    validate_performance

    success "Rolling update completed successfully"
}

# Function to switch traffic to blue environment
switch_traffic_blue() {
    status "Switching traffic to blue environment..."

    # Update canary deployment
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
    name: erlmcp-green
  canaryRef:
    apiVersion: apps/v1
    kind: Deployment
    name: erlmcp-blue
  analysis:
    interval: 30s
    threshold: 10
    stepWeight: 10
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

    # Drain green environment
    status "Draining green environment..."
    kubectl drain deployment/erlmcp-green --ignore-daemonsets --delete-local-data || true

    status "Traffic successfully switched to blue environment"
}

# Main script execution
main() {
    local target_env=$1
    local force=$2

    # Create log directory
    mkdir -p $(dirname $DEPLOY_LOG_FILE)
    chmod 755 $(dirname $DEPLOY_LOG_FILE)

    # Parse arguments
    case $target_env in
        green|blue)
            deploy $target_env $force
            ;;
        rollback)
            rollback
            ;;
        *)
            echo "Usage: $0 [green|blue] [--force]"
            echo "Options:"
            echo "  green    Deploy to green environment and switch traffic"
            echo "  blue     Deploy to blue environment and switch traffic"
            echo "  rollback Rollback to previous environment"
            echo ""
            echo "Example:"
            echo "  $0 green         # Deploy new version to green"
            echo "  $0 blue          # Deploy new version to blue"
            echo "  $0 rollback      # Rollback to previous version"
            exit 1
            ;;
    esac
}

# Run main function
main "$@"