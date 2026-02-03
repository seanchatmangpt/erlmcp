#!/bin/bash

# ErlMCP Blue-Green Deployment Script
# Implements blue-green deployment strategy for zero-downtime releases

set -euo pipefail

# Configuration
ENVIRONMENT=${1:-staging}
BLUE_VERSION=${2:-$(get_latest_image_version)}
GREEN_VERSION=${3:-$(get_latest_image_version)}

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Colors for environment identification
BLUE_COLOR='\033[0;34m'
GREEN_COLOR='\033[0;32m'
NC='\033[0m'

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

info() {
    echo -e "${BLUE}INFO: $1${NC}"
}

warning() {
    echo -e "${YELLOW}WARNING: $1${NC}"
}

# Get current active environment
get_active_environment() {
    # Check which environment is currently active
    if kubectl get svc erlmcp-server -n $ENVIRONMENT | grep -q "erlmcp-server-active"; then
        echo "green"
    else
        echo "blue"
    fi
}

# Get latest image version
get_latest_image_version() {
    echo "${GITHUB_SHA:-latest}"
}

# Main deployment function
main() {
    log "Starting blue-green deployment to $ENVIRONMENT"

    # Determine which environment is active
    ACTIVE_ENV=$(get_active_environment)
    if [ "$ACTIVE_ENV" = "green" ]; then
        NEW_ENV="blue"
        OLD_ENV="green"
    else
        NEW_ENV="green"
        OLD_ENV="blue"
    fi

    info "Active environment: $ACTIVE_ENV ($OLD_COLOR Deploying to: $NEW_ENV"

    # Step 1: Deploy to the inactive environment
    log "Step 1: Deploying version $BLUE_VERSION to $NEW_ENV environment"
    deploy_to_environment $NEW_ENV $BLUE_VERSION

    # Step 2: Health check the new environment
    log "Step 2: Performing health checks on $NEW_ENV environment"
    if ! health_check_environment $NEW_ENV; then
        error "Health check failed for $NEW_ENV environment"
    fi

    # Step 3: Pre-migration validation
    log "Step 3: Running pre-migration validation"
    if ! run_pre_migration_checks; then
        error "Pre-migration validation failed"
    fi

    # Step 4: Switch traffic to new environment
    log "Step 4: Switching traffic from $OLD_ENV to $NEW_ENV"
    switch_traffic $NEW_ENV

    # Step 5: Post-migration validation
    log "Step 5: Running post-migration validation"
    if ! run_post_migration_checks; then
        error "Post-migration validation failed"
    fi

    # Step 6: Cleanup old environment
    log "Step 6: Cleaning up $OLD_ENV environment"
    cleanup_environment $OLD_ENV

    success "Blue-green deployment completed successfully"
}

# Deploy to specific environment
deploy_to_environment() {
    local env_name=$1
    local version=$2

    log "Deploying to $env_name environment with version $version"

    # Create deployment for the new environment
    cat <<EOF | kubectl apply -f -
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp-server-$env_name
  namespace: $ENVIRONMENT
  labels:
    app: erlmcp-server
    environment: $env_name
    version: $version
spec:
  replicas: 3
  selector:
    matchLabels:
      app: erlmcp-server
      environment: $env_name
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxSurge: 1
      maxUnavailable: 0
  template:
    metadata:
      labels:
        app: erlmcp-server
        environment: $env_name
    spec:
      containers:
      - name: erlmcp-server
        image: ghcr.io/${GITHUB_REPOSITORY}/erlmcp-server:${version}
        ports:
        - containerPort: 8080
        env:
        - name: ENVIRONMENT
          value: "${ENVIRONMENT}"
        - name: INSTANCE_COLOR
          value: "${env_name}"
        - name: DEPLOYMENT_STRATEGY
          value: "blue-green"
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
      imagePullSecrets:
      - name: ghcr-secret
EOF

    log "Deployment created for $env_name environment"
}

# Health check environment
health_check_environment() {
    local env_name=$1
    local max_attempts=20
    local attempt=1

    log "Waiting for $env_name environment to be ready..."

    while [ $attempt -le $max_attempts ]; do
        # Check deployment status
        if kubectl wait --for=condition=available deployment erlmcp-server-$env_name -n $ENVIRONMENT --timeout=30s; then
            # Check pod readiness
            if kubectl wait --for=condition=ready pods -l app=erlmcp-server,environment=$env_name -n $ENVIRONMENT --timeout=60s; then
                log "$env_name environment is healthy"

                # Test actual endpoints
                if test_endpoints $env_name; then
                    return 0
                fi
            fi
        fi

        log "Waiting for $env_name to be healthy... attempt $attempt/$max_attempts"
        sleep 10
        ((attempt++))
    done

    return 1
}

# Test endpoints
test_endpoints() {
    local env_name=$1

    # Get service endpoint
    local endpoint=$(kubectl get endpoints erlmcp-server-$env_name -n $ENVIRONMENT -o jsonpath='{.subsets[0].addresses[0].ip}')
    local port=8080

    # Test health endpoint
    if curl -f -s "http://${endpoint}:${port}/health" > /dev/null; then
        log "Health endpoint is working for $env_name"
    else
        log "Health endpoint failed for $env_name"
        return 1
    fi

    # Test ready endpoint
    if curl -f -s "http://${endpoint}:${port}/ready" > /dev/null; then
        log "Ready endpoint is working for $env_name"
    else
        log "Ready endpoint failed for $env_name"
        return 1
    fi

    # Test business endpoints
    local endpoints=("/api/resources" "/api/tools" "/api/server-info")
    for endpoint_path in "${endpoints[@]}"; do
        if curl -f -s -H "Content-Type: application/json" "http://${endpoint}:${port}${endpoint_path}" > /dev/null; then
            log "$endpoint_path is accessible for $env_name"
        else
            log "$endpoint_path failed for $env_name"
            return 1
        fi
    done

    return 0
}

# Run pre-migration checks
run_pre_migration_checks() {
    log "Running pre-migration checks"

    # Check database compatibility
    if ! check_database_compatibility; then
        error "Database compatibility check failed"
    fi

    # Check service dependencies
    if ! check_service_dependencies; then
        error "Service dependencies check failed"
    fi

    # Run smoke tests against old environment
    if ! run_smoke_tests $OLD_ENV; then
        error "Smoke tests against old environment failed"
    fi

    log "Pre-migration checks passed"
}

# Run post-migration checks
run_post_migration_checks() {
    log "Running post-migration checks"

    # Run smoke tests against new environment
    if ! run_smoke_tests $NEW_ENV; then
        error "Smoke tests against new environment failed"
    fi

    # Run integration tests
    if ! run_integration_tests; then
        error "Integration tests failed"
    fi

    # Check data consistency
    if ! check_data_consistency; then
        error "Data consistency check failed"
    fi

    log "Post-migration checks passed"
}

# Switch traffic between environments
switch_traffic() {
    local new_env=$1

    log "Switching traffic to $new_env environment"

    # Update active service to point to the new environment
    kubectl apply -f - <<EOF
apiVersion: v1
kind: Service
metadata:
  name: erlmcp-server-active
  namespace: $ENVIRONMENT
spec:
  selector:
    app: erlmcp-server
    environment: $new_env
  ports:
  - protocol: TCP
    port: 8080
    targetPort: 8080
EOF

    # Wait for traffic to switch
    sleep 10

    # Verify traffic has switched
    local active_endpoint=$(kubectl get endpoints erlmcp-server-active -n $ENVIRONMENT -o jsonpath='{.subsets[0].addresses[0].ip}')
    if [ -n "$active_endpoint" ]; then
        log "Traffic switched to $new_env environment at $active_endpoint"
    else
        error "Failed to switch traffic to $new_env environment"
    fi
}

# Cleanup old environment
cleanup_environment() {
    local old_env=$1

    log "Cleaning up $old_env environment"

    # Scale down deployment
    kubectl scale deployment erlmcp-server-$old_env --replicas=0 -n $ENVIRONMENT

    # Wait for pods to terminate
    kubectl wait --for=delete pods -l app=erlmcp-server,environment=$old_env -n $ENVIRONMENT --timeout=300s || true

    # Remove deployment
    kubectl delete deployment erlmcp-server-$old_env -n $ENVIRONMENT || true

    # Remove service if exists
    kubectl delete service erlmcp-server-$old_env -n $ENVIRONMENT || true

    log "$old_env environment cleaned up"
}

# Check database compatibility
check_database_compatibility() {
    log "Checking database compatibility"

    # Check if schema version is compatible
    local db_version=$(kubectl exec -it deployment/erlmcp-server-$OLD_ENV -n $ENVIRONMENT -- erlmcp eval "application:get_env(erlmcp, db_schema_version, undefined)." 2>/dev/null || echo "undefined")
    local required_version=$(cat apps/erlmcp_core/src/erlmcp_app.src | grep vsn | cut -d"\"" -f2)

    if [ "$db_version" = "$required_version" ]; then
        log "Database version compatible: $db_version"
        return 0
    else
        log "Database version mismatch: expected $required_version, got $db_version"
        return 1
    fi
}

# Check service dependencies
check_service_dependencies() {
    log "Checking service dependencies"

    # Check Redis/PostgreSQL connectivity
    if ! check_external_connectivity; then
        return 1
    fi

    # Check messaging queue
    if ! check_messaging_queue; then
        return 1
    fi

    log "All service dependencies are healthy"
    return 0
}

# Run smoke tests
run_smoke_tests() {
    local env_name=$1

    log "Running smoke tests against $env_name environment"

    # Get endpoint
    local endpoint=$(kubectl get endpoints erlmcp-server-$env_name -n $ENVIRONMENT -o jsonpath='{.subsets[0].addresses[0].ip}')
    local port=8080

    # Test basic functionality
    local tests=(
        "GET /health"
        "GET /ready"
        "GET /api/server-info"
        "POST /api/resources"
    )

    for test in "${tests[@]}"; do
        local method=$(echo $test | cut -d' ' -f1)
        local path=$(echo $test | cut -d' ' -f2)

        if curl -f -s -X $method "http://${endpoint}:${port}${path}" > /dev/null; then
            log "✓ $test passed"
        else
            log "✗ $test failed"
            return 1
        fi
    done

    log "All smoke tests passed for $env_name environment"
    return 0
}

# Run integration tests
run_integration_tests() {
    log "Running integration tests"

    # Run comprehensive test suite
    ./scripts/test/integration-test.sh "http://$(kubectl get endpoints erlmcp-server-active -n $ENVIRONMENT -o jsonpath='{.subsets[0].addresses[0].ip}'):8080"

    if [ $? -eq 0 ]; then
        log "Integration tests passed"
        return 0
    else
        log "Integration tests failed"
        return 1
    fi
}

# Check data consistency
check_data_consistency() {
    log "Checking data consistency"

    # Compare critical data between old and new environments
    local old_count=$(kubectl exec -it deployment/erlmcp-server-$OLD_ENV -n $ENVIRONMENT -- erlmcp eval "length(erlmcp_registry:all_resources())." 2>/dev/null || echo "0")
    local new_count=$(kubectl exec -it deployment/erlmcp-server-$NEW_ENV -n $ENVIRONMENT -- erlmcp eval "length(erlmcp_registry:all_resources())." 2>/dev/null || echo "0")

    if [ "$old_count" = "$new_count" ]; then
        log "Data consistency verified: $old_count resources"
        return 0
    else
        log "Data inconsistency detected: old=$old_count, new=$new_count"
        return 1
    fi
}

# Execute main function
main "$@"