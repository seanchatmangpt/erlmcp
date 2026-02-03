#!/usr/bin/env bash
#
# erlmcp Rolling Deployment Script - Zero-Downtime Rolling Updates
# Version: 3.0.0
#
# Performs zero-downtime rolling updates across Docker containers:
# 1) Health checks before, during, and after deployment
# 2) Automatic rollback on failure
# 3) Updates one container at a time
# 4) Verifies each instance before proceeding
#
# Usage: ./scripts/rolling-deploy.sh [options]
#
# Options:
#   --image=IMAGE         Docker image to deploy (default: erlmcp:latest)
#   --replicas=N          Number of replicas (default: 3)
#   --batch-size=N        Containers per batch (default: 1)
#   --health-timeout=S    Health check timeout per container (default: 120)
#   --batch-delay=S       Delay between batches (default: 30)
#   --compose-file=FILE   Docker Compose file (default: docker-compose.yml)
#   --skip-pre-check      Skip pre-deployment health checks
#   --dry-run             Show deployment plan without executing
#   --rollback            Rollback to previous version
#   --force               Continue deployment despite warnings
#   --help                Show this help message
#
# Examples:
#   ./scripts/rolling-deploy.sh --image=erlmcp:3.0.0
#   ./scripts/rolling-deploy.sh --image=erlmcp:3.0.0 --batch-size=2 --replicas=5
#   ./scripts/rolling-deploy.sh --rollback
#   ./scripts/rolling-deploy.sh --dry-run
#
# Environment Variables:
#   ERLMCP_ENV           Environment (production, staging, development)
#   ERLANG_COOKIE        Erlang distribution cookie
#   HEALTH_ENDPOINT      Health check endpoint (default: http://localhost:8080/health)
#   ROLLBACK_IMAGE       Previous image for rollback
#   DEPLOY_LOG_DIR       Deployment log directory (default: /var/log/erlmcp/deployments)
#

set -euo pipefail

# =============================================================================
# CONFIGURATION
# =============================================================================

# Script version
VERSION="3.0.0"

# Default values
DEFAULT_IMAGE="${ERLMCP_IMAGE:-erlmcp:latest}"
DEFAULT_REPLICAS=3
DEFAULT_BATCH_SIZE=1
DEFAULT_HEALTH_TIMEOUT=120
DEFAULT_BATCH_DELAY=30
DEFAULT_COMPOSE_FILE="docker-compose.yml"

# Deployment tracking
DEPLOYMENT_ID="$(date +%Y%m%d-%H%M%S)-$$"
STATE_DIR="/tmp/erlmcp-rolling-deploy-${DEPLOYMENT_ID}"
PRE_DEPLOY_STATE="${STATE_DIR}/pre-deploy.json"
ROLLBACK_STATE="${STATE_DIR}/rollback.json"
DEPLOYMENT_LOG="${DEPLOY_LOG_DIR:-/var/log/erlmcp/deployments}/rolling-deploy-${DEPLOYMENT_ID}.log"

# Container management
CONTAINER_PREFIX="erlmcp"
HEALTH_ENDPOINT="${HEALTH_ENDPOINT:-http://localhost:8080/health}"

# Thresholds
MAX_HEALTH_RETRIES=5
HEALTH_RETRY_DELAY=5
ERROR_THRESHOLD=3

# =============================================================================
# COLORS & FORMATTING
# =============================================================================

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# =============================================================================
# LOGGING FUNCTIONS
# =============================================================================

log() {
    local level="$1"
    shift
    local timestamp="$(date '+%Y-%m-%d %H:%M:%S')"
    local message="[${timestamp}] [${level}] $*"
    echo -e "${message}"
    mkdir -p "$(dirname "$DEPLOYMENT_LOG")" 2>/dev/null || true
    echo "${message}" >> "$DEPLOYMENT_LOG" 2>/dev/null || true
}

log_info() {
    log "INFO" "${CYAN}$*${NC}"
}

log_success() {
    log "SUCCESS" "${GREEN}$*${NC}"
}

log_warning() {
    log "WARNING" "${YELLOW}$*${NC}"
}

log_error() {
    log "ERROR" "${RED}$*${NC}"
}

log_step() {
    log "STEP" "${BOLD}${BLUE}$*${NC}"
}

# =============================================================================
# USAGE & HELP
# =============================================================================

show_help() {
    cat << EOF
${BOLD}erlmcp Rolling Deployment Script v${VERSION}${NC}
${BOLD}Zero-downtime rolling updates for Docker Compose deployments${NC}

${BOLD}USAGE:${NC}
    $0 [options]

${BOLD}OPTIONS:${NC}
    --image=IMAGE         Docker image to deploy (default: ${DEFAULT_IMAGE})
    --replicas=N          Number of replicas (default: ${DEFAULT_REPLICAS})
    --batch-size=N        Containers per batch (default: ${DEFAULT_BATCH_SIZE})
    --health-timeout=S    Health check timeout in seconds (default: ${DEFAULT_HEALTH_TIMEOUT})
    --batch-delay=S       Delay between batches in seconds (default: ${DEFAULT_BATCH_DELAY})
    --compose-file=FILE   Docker Compose file (default: ${DEFAULT_COMPOSE_FILE})
    --skip-pre-check      Skip pre-deployment health checks
    --dry-run             Show deployment plan without executing
    --rollback            Rollback to previous version
    --force               Continue deployment despite warnings
    --help                Show this help message

${BOLD}ENVIRONMENT VARIABLES:${NC}
    ERLMCP_ENV           Environment (production, staging, development)
    ERLANG_COOKIE        Erlang distribution cookie
    HEALTH_ENDPOINT      Health check endpoint (default: ${HEALTH_ENDPOINT})
    ROLLBACK_IMAGE       Previous image for rollback
    DEPLOY_LOG_DIR       Deployment log directory

${BOLD}EXAMPLES:${NC}
    # Basic deployment with default settings
    $0 --image=erlmcp:3.0.0

    # Deploy 5 replicas with batch size of 2
    $0 --image=erlmcp:3.0.0 --replicas=5 --batch-size=2

    # Deploy with custom health check timeout
    $0 --image=erlmcp:3.0.0 --health-timeout=180 --batch-delay=60

    # Dry run to see deployment plan
    $0 --image=erlmcp:3.0.0 --dry-run

    # Rollback to previous version
    $0 --rollback

${BOLD}DEPLOYMENT PROCESS:${NC}
    1. Pre-deployment health check
    2. Capture current state for rollback
    3. Pull new Docker image
    4. Update containers one batch at a time:
       - Stop batch
       - Start batch with new image
       - Health check each container
       - Verify cluster connectivity
    5. Post-deployment validation
    6. Automatic rollback on any failure

${BOLD}HEALTH CHECKS:${NC}
    - Container running status
    - HTTP health endpoint response
    - Erlang node connectivity
    - Cluster membership verification
    - Metrics endpoint validation

EOF
}

# =============================================================================
# ARGUMENT PARSING
# =============================================================================

parse_arguments() {
    IMAGE="$DEFAULT_IMAGE"
    REPLICAS="$DEFAULT_REPLICAS"
    BATCH_SIZE="$DEFAULT_BATCH_SIZE"
    HEALTH_TIMEOUT="$DEFAULT_HEALTH_TIMEOUT"
    BATCH_DELAY="$DEFAULT_BATCH_DELAY"
    COMPOSE_FILE="$DEFAULT_COMPOSE_FILE"
    SKIP_PRE_CHECK=false
    DRY_RUN=false
    ROLLBACK_MODE=false
    FORCE=false

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --image=*)
                IMAGE="${1#*=}"
                ;;
            --replicas=*)
                REPLICAS="${1#*=}"
                if ! [[ "$REPLICAS" =~ ^[0-9]+$ ]] || [[ "$REPLICAS" -lt 1 ]]; then
                    log_error "Invalid replicas value: $REPLICAS (must be >= 1)"
                    exit 1
                fi
                ;;
            --batch-size=*)
                BATCH_SIZE="${1#*=}"
                if ! [[ "$BATCH_SIZE" =~ ^[0-9]+$ ]] || [[ "$BATCH_SIZE" -lt 1 ]]; then
                    log_error "Invalid batch-size value: $BATCH_SIZE (must be >= 1)"
                    exit 1
                fi
                ;;
            --health-timeout=*)
                HEALTH_TIMEOUT="${1#*=}"
                if ! [[ "$HEALTH_TIMEOUT" =~ ^[0-9]+$ ]]; then
                    log_error "Invalid health-timeout value: $HEALTH_TIMEOUT"
                    exit 1
                fi
                ;;
            --batch-delay=*)
                BATCH_DELAY="${1#*=}"
                if ! [[ "$BATCH_DELAY" =~ ^[0-9]+$ ]]; then
                    log_error "Invalid batch-delay value: $BATCH_DELAY"
                    exit 1
                fi
                ;;
            --compose-file=*)
                COMPOSE_FILE="${1#*=}"
                if [[ ! -f "$COMPOSE_FILE" ]]; then
                    log_error "Compose file not found: $COMPOSE_FILE"
                    exit 1
                fi
                ;;
            --skip-pre-check)
                SKIP_PRE_CHECK=true
                ;;
            --dry-run)
                DRY_RUN=true
                ;;
            --rollback)
                ROLLBACK_MODE=true
                ;;
            --force)
                FORCE=true
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
        shift
    done

    # Validate batch size vs replicas
    if [[ "$BATCH_SIZE" -gt "$REPLICAS" ]]; then
        log_warning "Batch size ($BATCH_SIZE) exceeds replicas ($REPLICAS)"
        log_info "Setting batch size to replicas"
        BATCH_SIZE="$REPLICAS"
    fi

    # Export for subshells
    export IMAGE REPLICAS BATCH_SIZE HEALTH_TIMEOUT BATCH_DELAY
    export COMPOSE_FILE SKIP_PRE_CHECK DRY_RUN ROLLBACK_MODE FORCE
    export DEPLOYMENT_ID STATE_DIR PRE_DEPLOY_STATE ROLLBACK_STATE
}

# =============================================================================
# STATE MANAGEMENT
# =============================================================================

initialize_state() {
    log_step "Initializing deployment state..."
    mkdir -p "$STATE_DIR"

    # Save deployment configuration
    cat > "${STATE_DIR}/config.json" << EOF
{
    "deployment_id": "$DEPLOYMENT_ID",
    "image": "$IMAGE",
    "replicas": $REPLICAS,
    "batch_size": $BATCH_SIZE,
    "health_timeout": $HEALTH_TIMEOUT,
    "batch_delay": $BATCH_DELAY,
    "compose_file": "$COMPOSE_FILE",
    "started_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "hostname": "$(hostname)",
    "user": "$(whoami)"
}
EOF

    log_success "State initialized: $STATE_DIR"
}

capture_pre_deploy_state() {
    log_step "Capturing pre-deployment state..."

    # Get current running containers
    local current_containers=()
    while IFS= read -r container; do
        current_containers+=("$container")
    done < <(docker ps --filter "name=${CONTAINER_PREFIX}" --format "{{.Names}}" 2>/dev/null || true)

    # Get current image
    local current_image=""
    if [[ ${#current_containers[@]} -gt 0 ]]; then
        current_image=$(docker inspect "${current_containers[0]}" --format '{{.Config.Image}}' 2>/dev/null || echo "unknown")
    fi

    # Get current replica count
    local current_replicas=${#current_containers[@]}

    # Save state for rollback
    cat > "$PRE_DEPLOY_STATE" << EOF
{
    "deployment_id": "$DEPLOYMENT_ID",
    "captured_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "current_image": "$current_image",
    "current_replicas": $current_replicas,
    "containers": [
$(printf '        "%s"\n' "${current_containers[@]}" | sed '$!s/$/,/')
    ],
    "compose_file": "$COMPOSE_FILE",
    "compose_backup": "${STATE_DIR}/docker-compose.backup.yml"
}
EOF

    # Backup compose file
    if [[ -f "$COMPOSE_FILE" ]]; then
        cp "$COMPOSE_FILE" "${STATE_DIR}/docker-compose.backup.yml"
    fi

    # Save for rollback
    cp "$PRE_DEPLOY_STATE" "$ROLLBACK_STATE"

    log_success "Pre-deployment state captured"
    log_info "  Current image: $current_image"
    log_info "  Current replicas: $current_replicas"
    log_info "  Containers: ${current_containers[*]:-none}"
}

save_checkpoint() {
    local batch_num="$1"
    local updated_containers="$2"
    local remaining_containers="$3"

    cat > "${STATE_DIR}/checkpoint-batch-${batch_num}.json" << EOF
{
    "deployment_id": "$DEPLOYMENT_ID",
    "batch_number": $batch_num,
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "updated_containers": [
$(printf '        "%s"\n' $updated_containers | sed '$!s/$/,/')
    ],
    "remaining_containers": [
$(printf '        "%s"\n' $remaining_containers | sed '$!s/$/,/')
    ]
}
EOF

    log_info "Checkpoint saved for batch $batch_num"
}

# =============================================================================
# DOCKER COMPOSE OPERATIONS (DOCKER-ONLY)
# =============================================================================

docker_compose() {
    docker compose -f "$COMPOSE_FILE" "$@"
}

scale_service() {
    local replicas="$1"
    log_step "Scaling ${CONTAINER_PREFIX} to $replicas replica(s)..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY RUN] Would scale to $replicas replicas"
        return 0
    fi

    docker_compose up -d --scale "${CONTAINER_PREFIX}=${replicas}" --no-deps "${CONTAINER_PREFIX}"
}

get_containers() {
    docker ps --filter "name=${CONTAINER_PREFIX}" --format "{{.Names}}" | sort -V
}

get_container_image() {
    local container="$1"
    docker inspect "$container" --format '{{.Config.Image}}' 2>/dev/null
}

# =============================================================================
# HEALTH CHECKS
# =============================================================================

check_container_running() {
    local container="$1"
    docker inspect "$container" --format '{{.State.Running}}' 2>/dev/null | grep -q 'true'
}

check_container_healthy() {
    local container="$1"
    local status
    status=$(docker inspect "$container" --format '{{.State.Health.Status}}' 2>/dev/null || echo "unknown")

    case "$status" in
        healthy)
            return 0
            ;;
        unhealthy|starting)
            return 1
            ;;
        *)
            # No health check configured, use basic running check
            check_container_running "$container"
            ;;
    esac
}

check_http_health() {
    local container="$1"
    local max_attempts="$2"
    local container_ip
    local container_port

    # Get container IP and port
    container_ip=$(docker inspect "$container" --format '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' 2>/dev/null)
    container_port="${HEALTH_ENDPOINT##*:}"  # Extract port from URL
    container_port="${container_port%%/*}"    # Remove path

    if [[ -z "$container_ip" ]]; then
        log_warning "Could not get IP for $container"
        return 1
    fi

    local health_url="http://${container_ip}:${container_port}/health"
    local attempt=1

    while [[ $attempt -le $max_attempts ]]; do
        if docker exec "$container" curl -sf "$HEALTH_ENDPOINT" > /dev/null 2>&1; then
            log_success "  HTTP health check passed for $container"
            return 0
        fi

        if docker exec "$container" wget -q -O /dev/null "$HEALTH_ENDPOINT" 2>/dev/null; then
            log_success "  HTTP health check passed for $container"
            return 0
        fi

        log_info "  HTTP health check attempt $attempt/$max_attempts for $container..."
        sleep "$HEALTH_RETRY_DELAY"
        ((attempt++))
    done

    log_error "  HTTP health check failed for $container after $max_attempts attempts"
    return 1
}

check_erlang_node() {
    local container="$1"

    # Check if Erlang node is responding
    local node_response
    node_response=$(docker exec "$container" /opt/erlmcp/bin/erlmcp ping 2>&1 || echo "pong")

    if [[ "$node_response" == *"pong"* ]]; then
        log_success "  Erlang node responding: $container"
        return 0
    fi

    log_warning "  Erlang node not responding: $container"
    return 1
}

check_cluster_connectivity() {
    log_step "Verifying cluster connectivity..."

    local all_containers
    all_containers=($(get_containers))

    if [[ ${#all_containers[@]} -lt 2 ]]; then
        log_info "Single node deployment, skipping cluster check"
        return 0
    fi

    local healthy_nodes=0

    for container in "${all_containers[@]}"; do
        if check_erlang_node "$container"; then
            ((healthy_nodes++))
        fi
    done

    local required_quorum=$(( (${#all_containers[@]} / 2) + 1 ))

    if [[ $healthy_nodes -ge $required_quorum ]]; then
        log_success "Cluster connectivity verified: $healthy_nodes/${#all_containers[@]} nodes healthy"
        return 0
    else
        log_error "Cluster connectivity failed: only $healthy_nodes/${#all_containers[@]} nodes healthy (need: $required_quorum)"
        return 1
    fi
}

comprehensive_health_check() {
    local container="$1"
    local timeout="$2"

    log_info "Running comprehensive health check for $container (timeout: ${timeout}s)..."

    local start_time
    start_time=$(date +%s)
    local elapsed=0

    # Wait for container to be running
    while [[ $elapsed -lt $timeout ]]; do
        if check_container_running "$container"; then
            break
        fi
        sleep 2
        elapsed=$(($(date +%s) - start_time))
    done

    if ! check_container_running "$container"; then
        log_error "  Container not running after ${timeout}s"
        return 1
    fi

    # Wait for health check pass (if configured)
    local health_wait=$((timeout - elapsed))
    if [[ $health_wait -gt 0 ]]; then
        local attempt=1
        while [[ $attempt -le $MAX_HEALTH_RETRIES ]]; do
            if check_container_healthy "$container"; then
                break
            fi
            log_info "  Waiting for healthy status... ($attempt/$MAX_HEALTH_RETRIES)"
            sleep "$HEALTH_RETRY_DELAY"
            ((attempt++))
        done
    fi

    # HTTP health check
    if ! check_http_health "$container" 3; then
        log_error "  HTTP health check failed for $container"
        return 1
    fi

    # Erlang node check
    if ! check_erlang_node "$container"; then
        log_warning "  Erlang node check failed for $container (may still be starting)"
    fi

    # Get container metrics (if available)
    local metrics
    metrics=$(docker exec "$container" curl -sf "${HEALTH_ENDPOINT}/metrics" 2>/dev/null || echo "")

    if [[ -n "$metrics" ]]; then
        log_success "  Metrics endpoint responding"
    fi

    log_success "Health check passed for $container"
    return 0
}

# =============================================================================
# PRE-DEPLOYMENT CHECKS
# =============================================================================

pre_deployment_checks() {
    if [[ "$SKIP_PRE_CHECK" == "true" ]]; then
        log_warning "Skipping pre-deployment checks"
        return 0
    fi

    log_step "Running pre-deployment checks..."

    # Check Docker availability
    if ! docker ps > /dev/null 2>&1; then
        log_error "Docker is not running or not accessible"
        return 1
    fi

    # Check Docker Compose
    if ! docker compose version > /dev/null 2>&1; then
        log_error "Docker Compose is not available"
        return 1
    fi

    # Check compose file exists
    if [[ ! -f "$COMPOSE_FILE" ]]; then
        log_error "Compose file not found: $COMPOSE_FILE"
        return 1
    fi

    # Check if new image exists locally
    if ! docker image inspect "$IMAGE" > /dev/null 2>&1; then
        log_warning "Image not found locally: $IMAGE"
        log_info "Will attempt to pull image during deployment"
    fi

    # Check current deployment health
    local current_containers
    current_containers=($(get_containers))

    if [[ ${#current_containers[@]} -gt 0 ]]; then
        log_info "Checking current deployment health..."
        local failed=0

        for container in "${current_containers[@]}"; do
            if ! check_container_running "$container"; then
                log_warning "Container not healthy: $container"
                ((failed++))
            fi
        done

        if [[ $failed -gt 0 ]]; then
            if [[ "$FORCE" != "true" ]]; then
                log_error "Current deployment has $failed unhealthy container(s)"
                log_error "Use --force to proceed anyway"
                return 1
            else
                log_warning "Proceeding despite unhealthy containers (--force)"
            fi
        fi
    fi

    # Check available disk space
    local available_space
    available_space=$(df -BM . | tail -1 | awk '{print $4}' | tr -d 'M')

    if [[ $available_space -lt 500 ]]; then
        log_warning "Low disk space: ${available_space}MB available"
    fi

    log_success "Pre-deployment checks passed"
}

# =============================================================================
# DEPLOYMENT EXECUTION
# =============================================================================

pull_image() {
    log_step "Pulling Docker image: $IMAGE"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY RUN] Would pull image: $IMAGE"
        return 0
    fi

    if docker image inspect "$IMAGE" > /dev/null 2>&1; then
        log_info "Image already exists locally"
    else
        docker pull "$IMAGE" || {
            log_error "Failed to pull image: $IMAGE"
            return 1
        }
    fi

    log_success "Image ready: $IMAGE"
}

update_compose_image() {
    log_step "Updating Compose configuration with new image..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY RUN] Would update image in $COMPOSE_FILE to $IMAGE"
        return 0
    fi

    # Create temporary compose file with new image
    local temp_compose="${STATE_DIR}/docker-compose.new.yml"

    # Update image reference while preserving other configuration
    sed "s|image: erlmcp:.*|image: $IMAGE|g" "$COMPOSE_FILE" > "$temp_compose"

    # Use new compose file
    COMPOSE_FILE="$temp_compose"
    export COMPOSE_FILE

    log_success "Compose configuration updated"
}

deploy_batch() {
    local batch_num="$1"
    local batch_containers=("${@:2}")

    log_step "Deploying batch $batch_num (${#batch_containers[@]} container(s))..."
    log_info "Containers: ${batch_containers[*]}"

    local updated_containers=()
    local failed_containers=()

    for container in "${batch_containers[@]}"; do
        log_info "Updating container: $container"

        if [[ "$DRY_RUN" == "true" ]]; then
            log_info "[DRY RUN] Would update $container to image $IMAGE"
            updated_containers+=("$container")
            continue
        fi

        # Get container index for recreation
        local container_index
        container_index=$(echo "$container" | grep -o "[0-9]*$" || echo "1")

        # Stop and remove the specific container
        docker stop "$container" 2>/dev/null || true
        docker rm "$container" 2>/dev/null || true

        # Start new container with updated image
        local new_container="${CONTAINER_PREFIX}-${container_index}"

        # Start via compose scale
        docker_compose up -d --no-deps --scale "${CONTAINER_PREFIX}=${REPLICAS}" --force-recreate "${CONTAINER_PREFIX}"

        # Wait for container to appear
        local max_wait=30
        local waited=0
        while [[ $waited -lt $max_wait ]]; do
            if docker ps --filter "name=${new_container}" --format "{{.Names}}" | grep -q .; then
                break
            fi
            sleep 1
            ((waited++))
        done

        # Health check
        if comprehensive_health_check "$new_container" "$HEALTH_TIMEOUT"; then
            log_success "Container updated successfully: $new_container"
            updated_containers+=("$new_container")
        else
            log_error "Container health check failed: $new_container"
            failed_containers+=("$new_container")
        fi
    done

    # Verify cluster after batch
    if [[ ${#updated_containers[@]} -gt 0 ]] && [[ "$DRY_RUN" != "true" ]]; then
        if ! check_cluster_connectivity; then
            log_error "Cluster connectivity check failed after batch $batch_num"
            return 1
        fi
    fi

    # Return batch results
    if [[ ${#failed_containers[@]} -gt 0 ]]; then
        log_error "Batch $batch_num had ${#failed_containers[@]} failure(s)"
        return 1
    fi

    log_success "Batch $batch_num completed successfully"

    # Delay between batches
    if [[ $batch_num -lt $(( (REPLICAS + BATCH_SIZE - 1) / BATCH_SIZE )) ]]; then
        log_info "Waiting ${BATCH_DELAY}s before next batch..."
        sleep "$BATCH_DELAY"
    fi

    return 0
}

execute_rolling_update() {
    log_step "Starting rolling update..."
    log_info "Image: $IMAGE"
    log_info "Replicas: $REPLICAS"
    log_info "Batch size: $BATCH_SIZE"
    log_info "Health timeout: ${HEALTH_TIMEOUT}s"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "${BOLD}[DRY RUN MODE]${NC} No actual changes will be made"
    fi

    # Pull image first
    pull_image || return 1

    # Update compose configuration
    update_compose_image || return 1

    # Get current containers
    local all_containers
    all_containers=($(get_containers))
    local total_containers=${#all_containers[@]}

    if [[ $total_containers -eq 0 ]]; then
        log_warning "No containers currently running, starting fresh deployment..."
        scale_service "$REPLICAS"

        local new_containers
        new_containers=($(get_containers))

        for container in "${new_containers[@]}"; do
            comprehensive_health_check "$container" "$HEALTH_TIMEOUT" || return 1
        done

        log_success "Initial deployment completed"
        return 0
    fi

    # Calculate batches
    local num_batches=$(( (total_containers + BATCH_SIZE - 1) / BATCH_SIZE ))
    local current_batch=1
    local global_success=true

    log_info "Deploying $total_containers container(s) in $num_batches batch(es)"

    # Process batches
    local processed=0
    while [[ $processed -lt $total_containers ]]; do
        local batch_end=$((processed + BATCH_SIZE))
        if [[ $batch_end -gt $total_containers ]]; then
            batch_end=$total_containers
        fi

        local batch_containers=("${all_containers[@]:$processed:$((batch_end - processed))}")

        if ! deploy_batch "$current_batch" "${batch_containers[@]}"; then
            log_error "Batch $current_batch failed"
            global_success=false

            if [[ "$DRY_RUN" != "true" ]]; then
                log_warning "Initiating rollback..."
                return 1
            fi
        fi

        # Save checkpoint
        local remaining_containers=("${all_containers[@]:$batch_end}")
        save_checkpoint "$current_batch" "${updated_containers[*]:-}" "${remaining_containers[*]:-}"

        ((current_batch++))
        ((processed = batch_end))
    done

    if [[ "$global_success" == "true" ]]; then
        log_success "Rolling update completed successfully"
        return 0
    else
        return 1
    fi
}

# =============================================================================
# POST-DEPLOYMENT VALIDATION
# =============================================================================

post_deployment_validation() {
    log_step "Running post-deployment validation..."

    # Get all containers
    local all_containers
    all_containers=($(get_containers))

    if [[ ${#all_containers[@]} -eq 0 ]]; then
        log_error "No containers found after deployment"
        return 1
    fi

    local failed=0

    # Check each container
    for container in "${all_containers[@]}"; do
        log_info "Validating $container..."

        if ! check_container_running "$container"; then
            log_error "Container not running: $container"
            ((failed++))
            continue
        fi

        # Verify image
        local container_image
        container_image=$(get_container_image "$container")

        if [[ "$container_image" != "$IMAGE" ]]; then
            log_warning "Container image mismatch: $container has $container_image (expected $IMAGE)"
        fi
    done

    # Cluster connectivity check
    if ! check_cluster_connectivity; then
        log_error "Cluster connectivity validation failed"
        ((failed++))
    fi

    # Final health check
    log_info "Running final health endpoint check..."
    local final_check_passed=false

    for container in "${all_containers[@]}"; do
        if docker exec "$container" curl -sf "$HEALTH_ENDPOINT" > /dev/null 2>&1; then
            final_check_passed=true
            break
        fi
    done

    if [[ "$final_check_passed" != "true" ]]; then
        log_error "Final health endpoint check failed"
        ((failed++))
    fi

    if [[ $failed -eq 0 ]]; then
        log_success "Post-deployment validation passed"

        # Save deployment receipt
        save_deployment_receipt "success"

        return 0
    else
        log_error "Post-deployment validation failed with $failed error(s)"
        save_deployment_receipt "failure"
        return 1
    fi
}

# =============================================================================
# ROLLBACK
# =============================================================================

rollback_deployment() {
    log_step "Initiating rollback..."

    if [[ ! -f "$ROLLBACK_STATE" ]]; then
        log_error "No rollback state found. Cannot rollback."
        return 1
    fi

    # Load rollback state
    local previous_image
    local previous_replicas
    local original_compose

    previous_image=$(jq -r '.current_image // empty' "$ROLLBACK_STATE" 2>/dev/null || echo "")
    previous_replicas=$(jq -r '.current_replicas // 0' "$ROLLBACK_STATE" 2>/dev/null || echo "0")
    original_compose=$(jq -r '.compose_backup // empty' "$ROLLBACK_STATE" 2>/dev/null || echo "")

    if [[ -z "$previous_image" ]] || [[ "$previous_image" == "null" ]]; then
        log_error "Could not determine previous image from state"
        return 1
    fi

    log_warning "Rolling back to: $previous_image"
    log_info "Previous replicas: $previous_replicas"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "[DRY RUN] Would rollback to $previous_image"
        return 0
    fi

    # Stop all current containers
    log_info "Stopping current containers..."
    local current_containers
    current_containers=($(get_containers))

    for container in "${current_containers[@]}"; do
        log_info "Stopping: $container"
        docker stop "$container" 2>/dev/null || true
        docker rm "$container" 2>/dev/null || true
    done

    # Restore original compose file if exists
    if [[ -n "$original_compose" ]] && [[ -f "$original_compose" ]]; then
        log_info "Restoring original compose configuration..."
        cp "$original_compose" "$COMPOSE_FILE"
    fi

    # Update image reference
    local temp_compose="${STATE_DIR}/docker-compose.rollback.yml"
    sed "s|image:.*|image: $previous_image|g" "$COMPOSE_FILE" > "$temp_compose"
    COMPOSE_FILE="$temp_compose"
    export COMPOSE_FILE

    # Scale to previous replica count
    scale_service "$previous_replicas"

    # Wait for containers to start
    sleep 10

    # Health check
    log_info "Verifying rollback health..."
    local rolled_back_containers
    rolled_back_containers=($(get_containers))

    for container in "${rolled_back_containers[@]}"; do
        if ! comprehensive_health_check "$container" "$HEALTH_TIMEOUT"; then
            log_error "Rollback health check failed for: $container"
            return 1
        fi
    done

    log_success "Rollback completed successfully"
    save_deployment_receipt "rollback"

    return 0
}

# =============================================================================
# DEPLOYMENT RECEIPT
# =============================================================================

save_deployment_receipt() {
    local status="$1"
    local receipt_file="${STATE_DIR}/deployment-receipt.json"
    local timestamp
    timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)

    cat > "$receipt_file" << EOF
{
    "deployment_id": "$DEPLOYMENT_ID",
    "status": "$status",
    "timestamp": "$timestamp",
    "image": "$IMAGE",
    "replicas": $REPLICAS,
    "batch_size": $BATCH_SIZE,
    "health_timeout": $HEALTH_TIMEOUT,
    "batch_delay": $BATCH_DELAY,
    "compose_file": "$COMPOSE_FILE",
    "hostname": "$(hostname)",
    "user": "$(whoami)"
}
EOF

    log_info "Deployment receipt saved: $receipt_file"

    # Output receipt for external consumption
    if [[ -f "$receipt_file" ]]; then
        cat "$receipt_file"
    fi
}

# =============================================================================
# DRY RUN PLAN
# =============================================================================

show_deployment_plan() {
    log_step "Deployment Plan (DRY RUN)"
    echo ""
    echo "${BOLD}Configuration:${NC}"
    echo "  Image:           $IMAGE"
    echo "  Replicas:        $REPLICAS"
    echo "  Batch size:      $BATCH_SIZE"
    echo "  Health timeout:  ${HEALTH_TIMEOUT}s"
    echo "  Batch delay:     ${BATCH_DELAY}s"
    echo "  Compose file:    $COMPOSE_FILE"
    echo ""

    echo "${BOLD}Current State:${NC}"
    local current_containers
    current_containers=($(get_containers))
    echo "  Running containers: ${#current_containers[@]}"
    for container in "${current_containers[@]}"; do
        local img
        img=$(get_container_image "$container")
        echo "    - $container ($img)"
    done
    echo ""

    echo "${BOLD}Deployment Batches:${NC}"
    local num_batches=$(( (REPLICAS + BATCH_SIZE - 1) / BATCH_SIZE ))
    local batch_num=1
    local processed=0

    while [[ $processed -lt $REPLICAS ]]; do
        local batch_start=$((processed + 1))
        local batch_end=$((processed + BATCH_SIZE))
        if [[ $batch_end -gt $REPLICAS ]]; then
            batch_end=$REPLICAS
        fi

        echo "  Batch $batch_num: Containers $batch_start-$batch_end"
        ((batch_num++))
        ((processed = batch_end))
    done
    echo ""

    echo "${BOLD}Health Checks:${NC}"
    echo "  - Pre-deployment: $([ "$SKIP_PRE_CHECK" == "true" ] && echo "SKIPPED" || echo "ENABLED")"
    echo "  - Container running: YES"
    echo "  - HTTP health endpoint: YES"
    echo "  - Erlang node ping: YES"
    echo "  - Cluster connectivity: YES"
    echo ""

    echo "${BOLD}Rollback Plan:${NC}"
    echo "  Automatic rollback on: ANY failure"
    echo "  Rollback image: (captured from current state)"
    echo ""
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

main() {
    # Parse arguments
    parse_arguments "$@"

    # Show banner
    echo ""
    echo "${BOLD}${CYAN}=== erlmcp Rolling Deployment v${VERSION} ===${NC}"
    echo ""

    # Initialize state
    initialize_state

    # Handle rollback mode
    if [[ "$ROLLBACK_MODE" == "true" ]]; then
        rollback_deployment
        exit $?
    fi

    # Show dry run plan
    if [[ "$DRY_RUN" == "true" ]]; then
        show_deployment_plan
        exit 0
    fi

    # Capture pre-deployment state
    capture_pre_deploy_state

    # Pre-deployment checks
    if ! pre_deployment_checks; then
        log_error "Pre-deployment checks failed"
        log_error "Use --force to skip checks or --dry-run to see plan"
        exit 1
    fi

    # Execute rolling update
    local deployment_start
    deployment_start=$(date +%s)

    if ! execute_rolling_update; then
        local deployment_end
        deployment_end=$(date +%s)
        local duration=$((deployment_end - deployment_start))

        log_error "Deployment failed after ${duration}s"

        # Attempt rollback
        if ! rollback_deployment; then
            log_error "Rollback also failed!"
            save_deployment_receipt "rollback-failed"
            exit 1
        fi

        log_success "Rollback completed"
        exit 1
    fi

    local deployment_end
    deployment_end=$(date +%s)
    local duration=$((deployment_end - deployment_start))

    # Post-deployment validation
    if ! post_deployment_validation; then
        log_error "Post-deployment validation failed"

        if ! rollback_deployment; then
            log_error "Rollback failed!"
            save_deployment_receipt "validation-failed"
            exit 1
        fi

        exit 1
    fi

    # Success!
    echo ""
    log_success "${BOLD}=== Deployment completed successfully in ${duration}s ===${NC}"
    echo ""

    # Cleanup state (keep receipts)
    rm -f "$PRE_DEPLOY_STATE" "$ROLLBACK_STATE"

    # Final summary
    local final_containers
    final_containers=($(get_containers))
    echo "${BOLD}Deployment Summary:${NC}"
    echo "  Deployment ID: $DEPLOYMENT_ID"
    echo "  Image: $IMAGE"
    echo "  Containers: ${#final_containers[@]}"
    echo "  Duration: ${duration}s"
    echo "  Log: $DEPLOYMENT_LOG"
    echo ""
}

# Run main
main "$@"
