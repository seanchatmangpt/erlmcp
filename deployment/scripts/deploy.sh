#!/bin/bash
# Zero-Downtime Deployment Script for erlmcp v3
# Implements blue-green deployment strategy with health checks

set -euo pipefail

# Configuration
DEPLOY_LOG="/var/log/erlmcp/deploy.log"
HEALTH_CHECK_TIMEOUT=30
MAX_ROLLBACK_ATTEMPTS=3
BLUE_GREEN_TIMEOUT=60

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$DEPLOY_LOG"
}

error_exit() {
    log "ERROR: $1"
    echo -e "${RED}✗ $1${NC}"
    exit 1
}

print_status() {
    case $1 in
        0) echo -e "${GREEN}✓ $2${NC}" ;;
        1) echo -e "${YELLOW}⚠ $2${NC}" ;;
        2) echo -e "${RED}✗ $2${NC}" ;;
        *) echo -e "${BLUE}? $2${NC}" ;;
    esac
}

check_health() {
    local endpoint=$1
    local max_attempts=$2
    local timeout=$3

    log "Checking health endpoint: $endpoint"

    for attempt in $(seq 1 $max_attempts); do
        if curl -sSf --max-time $timeout "$endpoint" >/dev/null 2>&1; then
            log "Health check passed (attempt $attempt)"
            return 0
        else
            log "Health check failed (attempt $attempt)"
            if [ $attempt -lt $max_attempts ]; then
                sleep $((timeout / 2))
            fi
        fi
    done

    return 1
}

backup_current_release() {
    local backup_dir="/var/lib/erlmcp/backups"
    local timestamp=$(date +%Y%m%d_%H%M%S)
    local backup_path="${backup_dir}/release_${timestamp}"

    log "Backing up current release to $backup_path"

    mkdir -p "$backup_dir"
    if [ -d "/var/lib/erlmcp/current" ]; then
        cp -r /var/lib/erlmcp/current "$backup_path" || error_exit "Failed to backup current release"

        # Keep only last 5 backups
        ls -t "$backup_dir" | tail -n +6 | xargs -r rm -rf
    fi

    log "Backup completed: $backup_path"
    echo "$backup_path"
}

deploy_green_instance() {
    local image_tag=$1

    log "Deploying green instance (erlmcp-green)"

    # Stop green instance if running
    if docker compose -f /opt/erlmcp/docker-compose.prod.yml ps erlmcp-green | grep -q "running"; then
        log "Stopping existing green instance"
        docker compose -f /opt/erlmcp/docker-compose.prod.yml stop erlmcp-green || true
    fi

    # Remove green instance
    docker compose -f /opt/erlmcp/docker-compose.prod.yml rm -f erlmcp-green || true

    # Deploy green instance
    log "Starting green instance with image: $image_tag"
    docker compose -f /opt/erlmcp/docker-compose.prod.yml up -d erlmcp-green

    # Wait for green instance to be healthy
    if check_health "http://localhost:8081/health" $HEALTH_CHECK_TIMEOUT 5; then
        log "Green instance is healthy"
        return 0
    else
        log "Green instance health check failed"
        return 1
    fi
}

validate_green_instance() {
    log "Validating green instance"

    # Run comprehensive health checks
    local checks=(
        "http://localhost:8081/health"
        "http://localhost:8081/metrics"
        "http://localhost:8081/api/ready"
    )

    local all_pass=true
    for endpoint in "${checks[@]}"; do
        if check_health "$endpoint" 5 3; then
            log "✓ $endpoint is healthy"
        else
            log "✗ $endpoint is unhealthy"
            all_pass=false
        fi
    done

    # Test MCP functionality
    log "Testing MCP functionality on green instance"
    if curl -sSf --max-time 10 "http://localhost:8081/api/resources" | jq . >/dev/null 2>&1; then
        log "✓ MCP resources endpoint responding"
    else
        log "✗ MCP resources endpoint not responding"
        all_pass=false
    fi

    if $all_pass; then
        log "Green instance validation passed"
        return 0
    else
        log "Green instance validation failed"
        return 1
    fi
}

switch_traffic_to_green() {
    log "Switching traffic to green instance"

    # Update load balancer configuration or DNS
    # This would be specific to your infrastructure setup

    # Example: Update Traefik routing
    if command -v traefik >/dev/null 2>&1; then
        log "Updating Traefik routing"
        # Traefik configuration would go here
    fi

    # Example: Update Kubernetes service
    if command -v kubectl >/dev/null 2>&1; then
        log "Updating Kubernetes service"
        # kubectl configuration would go here
    fi

    # For Docker Swarm, this would be handled by the load balancer
    log "Traffic switch initiated"
}

drain_blue_instance() {
    log "Draining blue instance connections"

    # Check if blue instance is still running
    if docker compose -f /opt/erlmcp/docker-compose.prod.yml ps erlmcp-blue | grep -q "running"; then
        # Stop accepting new connections
        # This would involve updating load balancer configuration

        # Wait for existing connections to drain
        log "Waiting for connections to drain..."
        local wait_time=0
        while [ $wait_time -lt $BLUE_GREEN_TIMEOUT ]; do
            # Check if there are still active connections
            local active_connections=$(curl -sSf "http://localhost:8080/metrics" | grep 'active_connections' || echo "0")
            if [ "$active_connections" -eq 0 ]; then
                log "All connections drained"
                break
            fi

            sleep 5
            wait_time=$((wait_time + 5))
            log "Still $active_connections active connections..."
        done

        # Stop blue instance
        log "Stopping blue instance"
        docker compose -f /opt/erlmcp/docker-compose.prod.yml stop erlmcp-blue
    fi
}

rollback_to_blue() {
    log "Rolling back to blue instance"

    # Switch traffic back to blue
    # Update load balancer configuration

    # Wait for blue instance to be healthy
    if check_health "http://localhost:8080/health" $HEALTH_CHECK_TIMEOUT 5; then
        log "Blue instance is healthy"

        # Remove green instance
        docker compose -f /opt/erlmcp/docker-compose.prod.yml stop erlmcp-green || true
        docker compose -f /opt/erlmcp/docker-compose.prod.yml rm -f erlmcp-green || true

        log "Rollback completed successfully"
        return 0
    else
        log "Blue instance is not healthy"
        return 1
    fi
}

post_deploy_validation() {
    log "Performing post-deployment validation"

    # Check overall system health
    local endpoints=(
        "http://localhost:8080/health"
        "http://localhost:8081/health"
        "http://localhost:8080/metrics"
        "http://localhost:8081/metrics"
    )

    local issues=0
    for endpoint in "${endpoints[@]}"; do
        if ! check_health "$endpoint" 3 5; then
            log "Health check failed: $endpoint"
            issues=$((issues + 1))
        fi
    done

    # Check error rates
    if command -v curl >/dev/null 2>&1 && command -v jq >/dev/null 2>&1; then
        local error_rate=$(curl -sSf "http://localhost:8080/metrics" | grep 'http_requests_total{status=~"5.."}' | tail -1 | awk '{print $2}' || echo "0")
        if [ "$error_rate" -gt 0 ]; then
            log "Error rate detected: $error_rate"
            issues=$((issues + 1))
        else
            log "No error rate detected"
        fi
    fi

    if [ $issues -eq 0 ]; then
        log "Post-deployment validation passed"
        return 0
    else
        log "Post-deployment validation failed with $issues issues"
        return 1
    fi
}

main() {
    local image_tag=${1:-erlmcp:3.0.0-prod}
    local rollback=${2:-false}

    log "Starting deployment"
    log "Image tag: $image_tag"
    log "Rollback mode: $rollback"

    # Create backup
    local backup_path=$(backup_current_release)

    if [ "$rollback" = "true" ]; then
        log "Starting rollback to previous version"

        for attempt in $(seq 1 $MAX_ROLLBACK_ATTEMPTS); do
            if rollback_to_blue; then
                log "Rollback successful"
                exit 0
            else
                log "Rollback attempt $attempt failed"
                if [ $attempt -lt $MAX_ROLLBACK_ATTEMPTS ]; then
                    log "Waiting before next attempt..."
                    sleep 10
                fi
            fi
        done

        error_exit "Rollback failed after $MAX_ROLLBACK_ATTEMPTS attempts"
    fi

    # Deploy green instance
    if ! deploy_green_instance "$image_tag"; then
        error_exit "Green instance deployment failed"
    fi

    # Validate green instance
    if ! validate_green_instance; then
        error_exit "Green instance validation failed"
    fi

    # Switch traffic to green
    switch_traffic_to_green

    # Drain blue instance
    drain_blue_instance

    # Post-deployment validation
    if ! post_deploy_validation; then
        log "Post-deployment validation failed"

        # Attempt rollback
        log "Attempting rollback due to validation failure"
        if rollback_to_blue; then
            error_exit "Deployment failed, rollback completed"
        else
            error_exit "Deployment failed, rollback also failed - manual intervention required"
        fi
    fi

    log "Deployment completed successfully"
    log "New instance: Green (port 8081)"
    log "Old instance: Blue (stopped)"
}

# Execute main function
main "$@"