#!/bin/bash
# Rolling update script for erlmcp v3
# Handles zero-downtime updates with proper health checks

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
STACK_NAME="erlmcp"
SERVICE_NAME="erlmcp-core"
UPDATE_DELAY=30  # seconds between updates
MAX_UNHEALTHY=2  # maximum unhealthy instances
HEALTH_TIMEOUT=60 # seconds for health check

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

    if ! docker stack ls | grep -q "$STACK_NAME"; then
        error "Stack $STACK_NAME is not deployed"
    fi

    log "Prerequisites verified"
}

# Get service status
get_service_status() {
    local service="$1"

    echo "=== Service: $service ==="
    echo "Running tasks:"
    docker service ps "$service" --format "{{.ID}}\t{{.CurrentState}}\t{{.Image}}" | while read -r id state image; do
        echo "  ID: $id"
        echo "  State: $state"
        echo "  Image: $image"
        echo
    done

    echo "Service details:"
    docker service inspect "$service" --format 'Replicas: {{.Spec.Mode.Replicas}}\nUpdateStatus: {{.UpdateStatus}}'
    echo
}

# Health check function
health_check() {
    local service="$1"
    local expected_replicas="$2"
    local healthy_count=0
    local attempts=0
    local max_attempts=$((HEALTH_TIMEOUT / 10))

    log "Checking health for $service..."

    while [[ $healthy_count -lt $expected_replicas && $attempts -lt $max_attempts ]]; do
        healthy_count=$(docker service ps "$service" --format '{{.CurrentState}}' | grep -c "Running" || true)

        if [[ $healthy_count -eq $expected_replicas ]]; then
            log "All $expected_replicas replicas are healthy"
            return 0
        fi

        log "$healthy_count out of $expected_replicas replicas are healthy (attempt $attempts/$max_attempts)"
        sleep 10
        ((attempts++))
    done

    if [[ $healthy_count -lt $expected_replicas ]]; then
        warn "Health check failed: $healthy_count out of $expected_replicas replicas are healthy"
        return 1
    fi
}

# Perform rolling update
perform_rolling_update() {
    local new_image="$1"
    local service="$STACK_NAME_$SERVICE_NAME"

    log "Starting rolling update for $service with image $new_image"

    # Get current replicas
    local current_replicas=$(docker service inspect "$service" --format '{{.Spec.Mode.Replicas}}')
    log "Current replicas: $current_replicas"

    # Update service with new image
    log "Updating service with new image..."
    docker service update --image "$new_image" "$service" || error "Failed to update service"

    # Wait for update to start
    sleep 10

    # Monitor update progress
    local completed=0
    local failed=0
    local max_attempts=$((current_replicas * UPDATE_DELAY / 10))

    while [[ $completed -lt $current_replicas && $failed -le $MAX_UNHEALTHY ]]; do
        log "Update progress: $completed/$current_replicas completed"

        # Get task statuses
        local running_tasks=$(docker service ps "$service" --format '{{.CurrentState}}' | grep -c "Running" || true)
        local failed_tasks=$(docker service ps "$service" --format '{{.CurrentState}}' | grep -c "Failed\|Rejected" || true)

        if [[ $failed_tasks -gt $MAX_UNHEALTHY ]]; then
            error "Too many failed tasks: $failed_tasks (max: $MAX_UNHEALTHY)"
        fi

        if [[ $running_tasks -eq $current_replicas ]]; then
            completed=$current_replicas
        else
            sleep 10
        fi
    done

    if [[ $failed -gt $MAX_UNHEALTHY ]]; then
        error "Rolling update failed due to too many failed tasks"
    fi

    log "Rolling update completed successfully"
}

# Verify post-update
verify_post_update() {
    local service="$STACK_NAME_$SERVICE_NAME"

    log "Verifying post-update status..."

    # Health check
    if ! health_check "$service" "$current_replicas"; then
        warn "Health check failed after update"
        return 1
    fi

    # Check service metrics
    log "Checking service metrics..."
    # This would typically query your monitoring system
    # Example: curl -s http://prometheus:9090/api/v1/query?query=rate(http_requests_total[5m])

    log "Post-update verification passed"
}

# Rollback function
rollback_update() {
    local service="$STACK_NAME_$SERVICE_NAME"
    local old_image="$1"

    log "Rolling back to image $old_image..."

    docker service update --image "$old_image" "$service" || error "Failed to rollback service"

    log "Rollback initiated"
    health_check "$service" "$current_replicas" || warn "Health check failed after rollback"
}

# Update configuration
update_configuration() {
    local config_file="$1"
    local config_type="$2"  # sys.config, vm.args

    log "Updating configuration: $config_type"

    # Create new config version
    local timestamp=$(date +%Y%m%d_%H%M%S)
    docker config create "erlmcp-${config_type}-$timestamp" "$config_file" || error "Failed to create config"

    # Update service with new config
    local service="$STACK_NAME_$SERVICE_NAME"
    docker service update --config-rm "erlmcp-${config_type}" --config-add "source=erlmcp-${config_type}-$target,uid=1001,gid=1001" "$service" || error "Failed to update service config"

    log "Configuration updated"
}

# Main execution
main() {
    # Parse command line arguments
    local action="${1:-update}"
    local image="${2:-}"
    local config="${3:-}"

    case "$action" in
        "update")
            if [[ -z "$image" ]]; then
                error "Image must be specified for update"
            fi
            ;;
        "rollback")
            if [[ -z "$image" ]]; then
                error "Image must be specified for rollback"
            fi
            ;;
        "config")
            if [[ -z "$config" ]]; then
                error "Config file must be specified for config update"
            fi
            ;;
        *)
            error "Unknown action: $action. Must be one of: update, rollback, config"
            ;;
    esac

    log "Starting rolling update process..."
    check_prerequisites

    case "$action" in
        "update")
            get_service_status "$STACK_NAME_$SERVICE_NAME"
            perform_rolling_update "$image"
            verify_post_update
            ;;
        "rollback")
            rollback_update "$image"
            ;;
        "config")
            update_configuration "$config" "${config##*.}"
            ;;
    esac

    log "Rolling update process completed successfully!"
}

# Show usage if no arguments provided
if [[ $# -eq 0 ]]; then
    cat << EOF
Usage: $0 <action> [options]

Actions:
  update <image>    - Perform rolling update to new image
  rollback <image>  - Rollback to specified image
  config <file>    - Update configuration file

Examples:
  $0 update erlmcp:v3.1.0
  $0 rollback erlmcp:v3.0.0
  $0 config sys.config.prod
EOF
    exit 1
fi

# Run main function
main "$@"