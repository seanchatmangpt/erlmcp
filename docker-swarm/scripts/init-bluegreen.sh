#!/bin/bash
# Blue-green deployment initialization script for erlmcp v3

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
STACK_NAME="erlmcp"
BLUE_STACK="${STACK_NAME}-blue"
GREEN_STACK="${STACK_NAME}-green"
CURRENT_STACK="${STACK_NAME}-active"
VOLUME_PREFIX="erlmcp-data"

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

# Check prerequisites
check_prerequisites() {
    if ! docker info | grep -q "Swarm: active"; then
        error "Docker Swarm is not active"
    fi

    log "Prerequisites verified"
}

# Create blue-green volumes
create_volumes() {
    log "Creating blue-green volumes..."

    # Create data volumes for blue environment
    for vol in "${VOLUME_PREFIX}-blue-1" "${VOLUME_PREFIX}-blue-2" "${VOLUME_PREFIX}-blue-3"; do
        if ! docker volume ls | grep -q "$vol"; then
            docker volume create "$vol" || error "Failed to create volume $vol"
            log "Created volume: $vol"
        else
            log "Volume $vol already exists"
        fi
    done

    # Create data volumes for green environment
    for vol in "${VOLUME_PREFIX}-green-1" "${VOLUME_PREFIX}-green-2" "${VOLUME_PREFIX}-green-3"; do
        if ! docker volume ls | grep -q "$vol"; then
            docker volume create "$vol" || error "Failed to create volume $vol"
            log "Created volume: $vol"
        else
            log "Volume $vol already exists"
        fi
    done

    # Create shared volumes
    for vol in "redis-data" "postgres-data" "kafka-data"; do
        docker volume create "$vol" || error "Failed to create shared volume $vol"
    done
}

# Initialize blue environment
init_blue() {
    log "Initializing blue environment..."

    # Deploy blue stack
    docker stack deploy -c docker-compose.bluegreen.yml "${BLUE_STACK}" || error "Failed to deploy blue stack"

    # Wait for services to be healthy
    wait_for_stack "${BLUE_STACK}" "blue"

    # Set blue as active
    docker config create active-stack "${BLUE_STACK}" || warn "Config already exists"

    log "Blue environment initialized and active"
}

# Initialize green environment (cold standby)
init_green() {
    log "Initializing green environment (cold standby)..."

    # Deploy green stack with minimal replicas
    docker stack deploy -c docker-compose.bluegreen.yml "${GREEN_STACK}" || error "Failed to deploy green stack"

    # Scale down to standby mode
    docker service scale "${GREEN_STACK}_erlmcp-core-blue=0" || warn "Failed to scale core service"
    docker service scale "${GREEN_STACK}_redis-cluster=1" || warn "Failed to scale redis service"
    docker service scale "${GREEN_STACK}_postgres-primary=1" || warn "Failed to scale postgres service"

    # Wait for standby services
    wait_for_standby "${GREEN_STACK}"

    log "Green environment initialized in standby mode"
}

# Wait for stack to be healthy
wait_for_stack() {
    local stack_name="$1"
    local env="$2"
    local max_attempts=30
    local attempt=0

    log "Waiting for $env stack to be healthy..."

    while [[ $attempt -lt $max_attempts ]]; do
        if docker stack ps "$stack_name" | grep -q "running"; then
            local healthy_count=$(docker stack ps "$stack_name" | grep "running" | grep -v "1/1" | wc -l)
            if [[ $healthy_count -eq 0 ]]; then
                log "$env stack is healthy"
                return 0
            fi
        fi

        sleep 10
        ((attempt++))
        log "Attempt $attempt/$max_attempts..."
    done

    warn "$env stack may not be fully healthy after timeout"
}

# Wait for standby services
wait_for_standby() {
    local stack_name="$1"
    local max_attempts=20
    local attempt=0

    log "Waiting for standby services..."

    while [[ $attempt -lt $max_attempts ]]; do
        local standby_services=$(docker service ls --filter name="$stack_name" | grep -v "0/0" | wc -l)

        if [[ $standby_services -gt 0 ]]; then
            log "Standby services are ready"
            return 0
        fi

        sleep 5
        ((attempt++))
    done

    warn "Standby services may not be ready"
}

# Switch to blue environment
switch_to_blue() {
    log "Switching to blue environment..."

    # Update active stack config
    docker config rm "${CURRENT_STACK}" || true
    docker config create "${CURRENT_STACK}" "${BLUE_STACK}" || error "Failed to update active stack"

    # Update routing
    update_routing "${BLUE_STACK}"

    log "Switched to blue environment"
}

# Switch to green environment
switch_to_green() {
    log "Switching to green environment..."

    # Scale up green services
    docker service scale "${GREEN_STACK}_erlmcp-core-green=5" || error "Failed to scale green core"
    docker service scale "${GREEN_STACK}_redis-cluster=3" || error "Failed to scale green redis"

    # Wait for green to be healthy
    wait_for_stack "${GREEN_STACK}" "green"

    # Update active stack config
    docker config rm "${CURRENT_STACK}" || true
    docker config create "${CURRENT_STACK}" "${GREEN_STACK}" || error "Failed to update active stack"

    # Update routing
    update_routing "${GREEN_STACK}"

    # Scale down blue to standby
    scale_to_standby "${BLUE_STACK}"

    log "Switched to green environment, blue now in standby"
}

# Update routing configuration
update_routing() {
    local active_stack="$1"

    log "Updating routing for $active_stack..."

    # Update Traefik configuration for active stack
    if [[ "$active_stack" == "${BLUE_STACK}" ]]; then
        # Route to blue services
        docker service update "${STACK_NAME}_traefik" --env-label-filter="com.docker.stack.namespace=${BLUE_STACK}" || warn "Failed to update routing"
    else
        # Route to green services
        docker service update "${STACK_NAME}_traefik" --env-label-filter="com.docker.stack.namespace=${GREEN_STACK}" || warn "Failed to update routing"
    fi
}

# Scale stack to standby
scale_to_standby() {
    local stack_name="$1"

    log "Scaling $stack_name to standby..."

    # Scale down core services
    docker service scale "${stack_name}_erlmcp-core*=0" || warn "Failed to scale core services"
    docker service scale "${stack_name}_redis-cluster=1" || warn "Failed to scale redis"

    log "$stack_name scaled to standby"
}

# Health check
health_check() {
    local active_stack=$(docker config inspect "${CURRENT_STACK}" --format='{{.Spec.Name}}' 2>/dev/null || echo "none")

    echo "Current active stack: $active_stack"

    if [[ "$active_stack" == "none" ]]; then
        warn "No active stack configured"
    else
        log "Active stack status:"
        docker stack ps "$active_stack" --format "table {{.Name}}\t{{.CurrentState}}\t{{.Error}}"
    fi

    # Show both stacks status
    for stack in "${BLUE_STACK}" "${GREEN_STACK}"; do
        echo ""
        echo "$stack status:"
        docker stack ps "$stack" --format "table {{.Name}}\t{{.CurrentState}}\t{{.Error}}"
    done
}

# Cleanup old volumes
cleanup_volumes() {
    local keep_days="${1:-30}"

    log "Cleaning up old volumes older than $keep_days days..."

    # Find and remove old backup volumes
    docker volume ls --filter "name=${VOLUME_PREFIX}-" --format "{{.Name}}" | while read -r vol; do
        local create_date=$(docker volume inspect "$vol" --format '{{.CreatedAt}}' | cut -d'T' -f1)
        local vol_age=$(( ( $(date +%s) - $(date -d "$create_date" +%s) ) / 86400 ))

        if [[ $vol_age -gt $keep_days && $vol != *"$(date +%Y%m%d)"* ]]; then
            docker volume rm "$vol" || warn "Failed to remove volume $vol"
            log "Removed old volume: $vol (age: $vol_age days)"
        fi
    done
}

# Main execution
main() {
    # Parse command line arguments
    local action="${1:-}"

    case "$action" in
        "init")
            check_prerequisites
            create_volumes
            init_blue
            init_green
            ;;
        "switch-blue")
            switch_to_blue
            ;;
        "switch-green")
            switch_to_green
            ;;
        "health")
            health_check
            ;;
        "cleanup")
            cleanup_volumes "$2"
            ;;
        "")
            echo "Usage: $0 <action> [options]"
            echo "Actions:"
            echo "  init            - Initialize blue-green deployment"
            echo "  switch-blue     - Switch to blue environment"
            echo "  switch-green    - Switch to green environment"
            echo "  health          - Show current health status"
            echo "  cleanup [days]  - Clean up old volumes (default: 30 days)"
            exit 1
            ;;
        *)
            error "Unknown action: $action"
            ;;
    esac
}

# Run main function
main "$@"