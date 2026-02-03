#!/bin/bash

# ErlMCP Canary Deployment Script
# Implements canary deployment strategy with progressive rollout

set -euo pipefail

# Configuration
ENVIRONMENT=${1:-staging}
PERCENTAGE=${2:-10}
HEALTH_CHECK_INTERVAL=30
HEALTH_CHECK_TIMEOUT=300
ROLLBACK_THRESHOLD=5

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging
log() {
    echo -e "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a deployment.log
}

error() {
    echo -e "${RED}ERROR: $1${NC}" >&2
    exit 1
}

success() {
    echo -e "${GREEN}SUCCESS: $1${NC}"
}

warning() {
    echo -e "${YELLOW}WARNING: $1${NC}"
}

# Main deployment function
main() {
    log "Starting canary deployment to $ENVIRONMENT with $PERCENTAGE% traffic"

    # Get current deployment info
    CURRENT_DEPLOYMENT=$(get_current_deployment)
    log "Current deployment: $CURRENT_DEPLOYMENT"

    # Get new image version
    NEW_VERSION=$(get_latest_image_version)
    log "New version: $NEW_VERSION"

    # Step 1: Deploy new version to canary
    log "Step 1: Deploying canary version"
    deploy_canary $NEW_VERSION

    # Step 2: Health check canary
    log "Step 2: Performing health checks on canary"
    if ! health_check_canary; then
        error "Canary health check failed"
    fi

    # Step 3: Gradual traffic shift
    log "Step 3: Gradually shifting traffic"
    progressive_traffic_shift $PERCENTAGE

    # Step 4: Monitor and validate
    log "Step 4: Monitoring canary performance"
    monitor_canary $HEALTH_CHECK_INTERVAL $HEALTH_CHECK_TIMEOUT

    # Step 5: Complete deployment or rollback
    if validate_canary; then
        log "Step 5: Canary validation successful"
        complete_deployment $NEW_VERSION
        success "Canary deployment completed successfully"
    else
        log "Step 5: Canary validation failed, triggering rollback"
        rollback_canary
        error "Canary deployment failed, rolled back"
    fi
}

# Get current deployment information
get_current_deployment() {
    kubectl get deployment erlmcp-server -n $ENVIRONMENT -o jsonpath='{.spec.template.spec.containers[0].image}' | cut -d: -f2
}

# Get latest image version from registry
get_latest_image_version() {
    echo "${GITHUB_SHA:-latest}"
}

# Deploy canary version
deploy_canary() {
    local version=$1

    # Create canary deployment
    cat <<EOF | kubectl apply -f -
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp-server-canary
  namespace: $ENVIRONMENT
  labels:
    app: erlmcp-server
    version: $version
    deployment-type: canary
spec:
  replicas: 2
  selector:
    matchLabels:
      app: erlmcp-server
      deployment-type: canary
  template:
    metadata:
      labels:
        app: erlmcp-server
        deployment-type: canary
    spec:
      containers:
      - name: erlmcp-server
        image: ghcr.io/${GITHUB_REPOSITORY}/erlmcp-server:${version}
        ports:
        - containerPort: 8080
        env:
        - name: ENVIRONMENT
          value: "${ENVIRONMENT}"
        - name: DEPLOYMENT_TYPE
          value: "canary"
        resources:
          requests:
            memory: "256Mi"
            cpu: "250m"
          limits:
            memory: "512Mi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5
EOF

    log "Canary deployment created for version $version"
}

# Health check canary
health_check_canary() {
    local max_attempts=$((HEALTH_CHECK_TIMEOUT / HEALTH_CHECK_INTERVAL))
    local attempt=1

    while [ $attempt -le $max_attempts ]; do
        if kubectl wait --for=condition=ready pod -l deployment-type=canary -n $ENVIRONMENT --timeout=10s; then
            log "Canary pods are ready"
            return 0
        fi

        log "Waiting for canary to be ready... attempt $attempt/$max_attempts"
        sleep $HEALTH_CHECK_INTERVAL
        ((attempt++))
    done

    return 1
}

# Progressive traffic shift
progressive_traffic_shift() {
    local target_percentage=$1
    local current_percentage=0

    # Step 1: 10% increment
    step_percentage=$((target_percentage / 10))

    while [ $current_percentage -lt $target_percentage ]; do
        current_percentage=$((current_percentage + step_percentage))
        if [ $current_percentage -gt $target_percentage ]; then
            current_percentage=$target_percentage
        fi

        log "Setting traffic to $current_percentage% canary"

        # Update Istio virtual service or nginx config
        update_traffic_percentage $current_percentage

        # Wait for traffic to shift
        sleep 30

        # Check health
        if ! health_check_percentage $current_percentage; then
            warning "Health check failed at $current_percentage% traffic"
            # Consider rollback or pause
        fi
    done

    log "Traffic shift complete: $current_percentage% canary"
}

# Update traffic percentage
update_traffic_percentage() {
    local percentage=$1

    # Using Istio as example
    kubectl apply -f - <<EOF
apiVersion: networking.istio.io/v1alpha3
kind: VirtualService
metadata:
  name: erlmcp-server
  namespace: $ENVIRONMENT
spec:
  hosts:
  - erlmcp.${ENVIRONMENT}.example.com
  http:
  - route:
    - destination:
        host: erlmcp-server-headless
        subset: stable
      weight: $((100 - percentage))
    - destination:
        host: erlmcp-server-headless
        subset: canary
      weight: $percentage
EOF
}

# Monitor canary performance
monitor_canary() {
    local interval=$1
    local timeout=$2
    local start_time=$(date +%s)

    while true; do
        local current_time=$(date +%s)
        local elapsed=$((current_time - start_time))

        if [ $elapsed -ge $timeout ]; then
            warning "Monitoring timeout reached"
            return 1
        fi

        # Collect metrics
        local error_rate=$(get_error_rate)
        local response_time=$(get_average_response_time)
        local cpu_usage=$(get_cpu_usage)
        local memory_usage=$(get_memory_usage)

        log "Metrics - Error Rate: $error_rate%, Response Time: ${response_time}ms, CPU: ${cpu_usage}%, Memory: ${memory_usage}%"

        # Check error rate threshold
        if (( $(echo "$error_rate > 5" | bc -l) )); then
            warning "High error rate detected: $error_rate%"
            return 1
        fi

        sleep $interval
    done
}

# Get error rate from monitoring
get_error_rate() {
    # Query Prometheus or similar
    curl -s "http://prometheus:9090/api/v1/query?query=sum(rate(http_requests_total{status=~\"5..\"}[$1m]))/sum(rate(http_requests_total[$1m]))*100" | jq '.data.result[0].value[1]' || echo "0"
}

# Get average response time
get_average_response_time() {
    curl -s "http://prometheus:9090/api/v1/query?query=histogram_quantile(0.95, sum(rate(http_request_duration_seconds_bucket[$1m])) by (le))" | jq '.data.result[0].value[1]' || echo "0"
}

# Get CPU usage
get_cpu_usage() {
    kubectl top pods -l deployment-type=canary -n $ENVIRONMENT --no-headers | awk '{sum += $3} END {print sum}' || echo "0"
}

# Get memory usage
get_memory_usage() {
    kubectl top pods -l deployment-type=canary -n $ENVIRONMENT --no-headers | awk '{sum += $2} END {print sum}' || echo "0"
}

# Validate canary deployment
validate_canary() {
    local metrics_pass=true

    # Check various metrics
    local error_rate=$(get_error_rate)
    if (( $(echo "$error_rate > 5" | bc -l) )); then
        warning "High error rate: $error_rate%"
        metrics_pass=false
    fi

    local response_time=$(get_average_response_time)
    if (( $(echo "$response_time > 1000" | bc -l) )); then
        warning "High response time: ${response_time}ms"
        metrics_pass=false
    fi

    if [ "$metrics_pass" = true ]; then
        log "Canary validation passed"
        return 0
    else
        log "Canary validation failed"
        return 1
    fi
}

# Complete deployment
complete_deployment() {
    local version=$1

    # Promote canary to stable
    kubectl patch deployment erlmcp-server-canary -n $ENVIRONMENT --type='json' -p='{"spec":{"template":{"spec":{"containers":[{"name":"erlmcp-server","image":"ghcr.io/'${GITHUB_REPOSITORY}'/erlmcp-server:'${version}'"}]}}}}'

    # Update production label
    kubectl label deployment erlmcp-server-canary -n $ENVIRONMENT version=${version} --overwrite

    # Clean up old deployments
    cleanup_old_deployments

    log "Deployment completed: version $version"
}

# Rollback canary
rollback_canary() {
    log "Rolling back canary deployment"

    # Scale down canary
    kubectl scale deployment erlmcp-server-canary --replicas=0 -n $ENVIRONMENT

    # Restore previous traffic distribution
    update_traffic_percentage 0

    log "Canary rolled back successfully"
}

# Cleanup old deployments
cleanup_old_deployments() {
    # Keep only last 3 deployments
    kubectl deployments erlmcp-server -n $ENVIRONMENT --sort-by=.metadata.creationTimestamp | head -n -3 | xargs -I {} kubectl delete deployment {} -n $ENVIRONMENT || true
}

# Execute main function
main "$@"