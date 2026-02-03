#!/bin/bash
# Docker stack deployment script
# This script handles deploying erlmcp stacks with zero-downtime updates

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
STACK_NAME="erlmcp"
DEPLOY_MODE=${1:-prod}  # prod, staging, bluegreen
LOG_LEVEL=${LOG_LEVEL:-info}

# Default values
ERLMCP_CORE_REPLICAS=5
ERLMCP_ENV=production

# Logging function
log() {
    echo -e "${GREEN}[INFO]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1"
    exit 1
}

# Validate input
validate_input() {
    local valid_modes=("prod" "staging" "bluegreen" "monitoring")
    if [[ ! " ${valid_modes[@]} " =~ " $DEPLOY_MODE " ]]; then
        error "Invalid deploy mode: $DEPLOY_MODE. Must be one of: ${valid_modes[*]}"
    fi
}

# Check Docker Swarm status
check_swarm() {
    if ! docker info | grep -q "Swarm: active"; then
        error "Docker Swarm is not active. Run swarm-init.sh first."
    fi

    log "Docker Swarm is active"
}

# Deploy Docker stack
deploy_stack() {
    local compose_file="docker-swarm/docker-compose.$DEPLOY_MODE.yml"
    local stack_name="$STACK_NAME-$DEPLOY_MODE"

    log "Deploying $stack_name stack..."

    # Pull images first
    log "Pulling images..."
    docker pull erlmcp:v3-prod || error "Failed to pull base image"

    # Build custom images if needed
    if [[ "$DEPLOY_MODE" == "prod" ]]; then
        docker build -t erlmcp:v3-prod -f docker-swarm/services/Dockerfile.base . || error "Failed to build base image"
        docker build -t erlmcp-redis:v3-prod -f docker-swarm/services/Dockerfile.redis . || error "Failed to build Redis image"
        docker build -t erlmcp-prometheus:v3-prod -f docker-swarm/services/Dockerfile.prometheus . || error "Failed to build Prometheus image"
        docker build -t erlmcp-grafana:v3-prod -f docker-swarm/services/Dockerfile.grafana . || error "Failed to build Grafana image"
    fi

    # Deploy stack
    log "Deploying stack with docker-compose.$DEPLOY_MODE.yml..."
    docker stack deploy -c "$compose_file" "$stack_name" || error "Failed to deploy stack"

    log "Stack deployment initiated"
}

# Wait for services to be ready
wait_for_services() {
    local stack_name="$STACK_NAME-$DEPLOY_MODE"
    local timeout=300  # 5 minutes
    local start_time=$(date +%s)

    log "Waiting for services to be ready..."

    # Core services
    for service in erlmcp-core redis-cluster; do
        log "Waiting for service $service..."
        while true; do
            local current_time=$(date +%s)
            local elapsed=$((current_time - start_time))

            if [[ $elapsed -gt $timeout ]]; then
                error "Timeout waiting for service $service"
            fi

            if docker service ps "$service" --format "{{.CurrentState}}" | grep -q "Running"; then
                log "Service $service is ready"
                break
            fi

            sleep 5
        done
    done

    # Health check services
    log "Running health checks..."
    for service in erlmcp-core redis-cluster; do
        local attempts=0
        while [[ $attempts -lt 3 ]]; do
            if docker service logs "$service" 2>&1 | grep -q "healthy"; then
                log "Health check passed for $service"
                break
            fi
            ((attempts++))
            sleep 10
        done

        if [[ $attempts -eq 3 ]]; then
            error "Health check failed for $service after 3 attempts"
        fi
    done

    log "All services are ready"
}

# Scale services
scale_services() {
    local stack_name="$STACK_NAME-$DEPLOY_MODE"
    local scale_file="docker-swarm/scale-$DEPLOY_MODE.json"

    if [[ -f "$scale_file" ]]; then
        log "Scaling services based on $scale_file..."
        cat "$scale_file" | jq -r '.services | to_entries | .[] | "\(.key)=\(.value.replicas)"' | while read -r scale; do
            local service=$(echo "$scale" | cut -d'=' -f1)
            local replicas=$(echo "$scale" | cut -d'=' -f2)

            log "Scaling $service to $replicas replicas..."
            docker service scale "${stack_name}_${service}=$replicas"
        done
    else
        log "No scaling configuration found for $DEPLOY_MODE"
    fi
}

# Deploy monitoring stack
deploy_monitoring() {
    local stack_name="${STACK_NAME}-monitoring"

    log "Deploying monitoring stack..."
    docker stack deploy -c docker-swarm/docker-compose.monitoring.yml "$stack_name"

    # Wait for monitoring services
    sleep 30

    if docker service ls --filter name=prometheus | grep -q prometheus; then
        log "Monitoring stack deployed successfully"
    else
        warn "Monitoring stack deployment may have failed"
    fi
}

# Enable auto-scaling
enable_autoscaling() {
    log "Enabling auto-scaling policies..."

    # Set up resource limits
    docker service update \
        --limit-cpu 4 \
        --limit-memory 4g \
        --reserve-cpu 2 \
        --reserve-memory 2g \
        "${STACK_NAME}-${DEPLOY_MODE}_erlmcp-core"

    # Set up health check intervals
    docker service update \
        --health-interval 30s \
        --health-timeout 10s \
        --health-retries 3 \
        --health-start-period 60s \
        "${STACK_NAME}-${DEPLOY_MODE}_erlmcp-core"

    log "Auto-scaling enabled"
}

# Run performance tests
run_performance_tests() {
    if [[ "$DEPLOY_MODE" == "prod" ]]; then
        log "Running performance tests..."

        # Run load test
        docker run --rm \
            -v "$(pwd)/bench:/app/bench" \
            --network erlmcp-overlay \
            grafana/k6:latest run /app/bench/load-test.js || warn "Load test failed"

        log "Performance tests completed"
    fi
}

# Generate deployment report
generate_report() {
    local stack_name="$STACK_NAME-$DEPLOY_MODE"
    local report_file="/tmp/deployment-report-$(date +%Y%m%d_%H%M%S).html"

    log "Generating deployment report..."

    cat > "$report_file" << EOF
<!DOCTYPE html>
<html>
<head>
    <title>Deployment Report - $stack_name</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .success { color: green; }
        .warning { color: orange; }
        .error { color: red; }
        table { border-collapse: collapse; width: 100%; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
    </style>
</head>
<body>
    <h1>Deployment Report - $stack_name</h1>
    <p>Generated: $(date)</p>

    <h2>Services Status</h2>
    <table>
        <tr>
            <th>Service</th>
            <th>Replicas</th>
            <th>Status</th>
            <th>Health</th>
        </tr>
EOF

    # Add services status
    docker service ls --filter name="$stack_name" --format "table {{.Name}}\t{{.Replicas}}\t{{.Mode}}\t{{.Image}}" | tail -n +2 | while read -r service; do
        local name=$(echo "$service" | awk '{print $1}')
        local replicas=$(echo "$service" | awk '{print $2}')
        local mode=$(echo "$service" | awk '{print $3}')
        local image=$(echo "$service" | awk '{print $4}')

        # Get health status
        local health=""
        if docker service ps "$name" --format "{{.CurrentState}}" | grep -q "healthy"; then
            health='<span class="success">HEALTHY</span>'
        else
            health='<span class="warning">DEGRADED</span>'
        fi

        cat >> "$report_file" << EOF
        <tr>
            <td>$name</td>
            <td>$replicas</td>
            <td>$mode</td>
            <td>$health</td>
        </tr>
EOF
    done

    cat >> "$report_file" << EOF
    </table>

    <h2>Node Information</h2>
    <table>
        <tr>
            <th>Node</th>
            <th>Status</th>
            <th>Role</th>
            <th>CPU</th>
            <th>Memory</th>
        </tr>
EOF

    # Add nodes status
    docker node ls --format "table {{.ID}}\t{{.Status}}\t{{.Availability}}\t{{.ManagerStatus}}\t{{.Hostname}}" | tail -n +2 | while read -r node; do
        local id=$(echo "$node" | awk '{print $1}')
        local status=$(echo "$node" | awk '{print $2}')
        local availability=$(echo "$node" | awk '{print $3}')
        local manager=$(echo "$node" | awk '{print $4}')
        local hostname=$(echo "$node" | awk '{print $5}')

        # Get resource usage
        local cpu=$(docker node inspect "$id" --format '{{.Description.Resources.Cpus}}')
        local memory=$(docker node inspect "$id" --format '{{.Description.Resources.MemoryBytes}}')

        cat >> "$report_file" << EOF
        <tr>
            <td>$hostname</td>
            <td>$status</td>
            <td>$manager</td>
            <td>$cpu</td>
            <td>$memory</td>
        </tr>
EOF
    done

    cat >> "$report_file" << EOF
    </table>
</body>
</html>
EOF

    log "Deployment report generated: $report_file"
}

# Main execution
main() {
    log "Starting deployment process for $DEPLOY_MODE environment..."

    validate_input
    check_swarm

    case "$DEPLOY_MODE" in
        "prod")
            log "Deploying production stack..."
            deploy_stack
            scale_services
            deploy_monitoring
            enable_autoscaling
            ;;
        "staging")
            log "Deploying staging stack..."
            deploy_stack
            ;;
        "bluegreen")
            log "Deploying blue-green stack..."
            deploy_stack
            ;;
        "monitoring")
            log "Deploying monitoring stack only..."
            deploy_monitoring
            ;;
    esac

    wait_for_services
    run_performance_tests
    generate_report

    log "Deployment completed successfully!"
    log "Access points:"
    log "- Grafana dashboard: http://<manager-ip>:3000"
    log "- Prometheus: http://<manager-ip>:9090"
    log "- Traefik dashboard: http://<manager-ip>:8080"
}

# Run main function
main "$@"