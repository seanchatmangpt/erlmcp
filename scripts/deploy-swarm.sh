#!/usr/bin/env bash
#
# erlmcp Docker Swarm Deployment Script
# Automates the deployment of erlmcp v3 to Docker Swarm
#
# Usage:
#   ./scripts/deploy-swarm.sh [action]
#
# Actions:
#   init    - Initialize Swarm and create networks (first time only)
#   secrets - Generate and create secrets
#   deploy  - Deploy the stack
#   update  - Update the stack
#   scale   - Scale services
#   status  - Show stack status
#   logs    - Show service logs
#   rollback- Rollback to previous version
#   clean   - Remove stack and clean up (careful!)

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
STACK_NAME="${STACK_NAME:-erlmcp}"
COMPOSE_FILE="${COMPOSE_FILE:-docker/docker-stack.yml}"
ENV_FILE="${ENV_FILE:-.env.prod}"
REPLICAS="${REPLICAS:-5}"

# Functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_docker() {
    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed"
        exit 1
    fi

    if ! docker info &> /dev/null; then
        log_error "Docker daemon is not running"
        exit 1
    fi
}

check_swarm() {
    if ! docker info | grep -q "Swarm: active"; then
        log_error "Docker Swarm is not initialized"
        log_info "Run 'docker swarm init' to initialize"
        exit 1
    fi
}

init_swarm() {
    log_info "Initializing Docker Swarm deployment..."

    # Check if Swarm is active
    if docker info | grep -q "Swarm: active"; then
        log_info "Swarm is already active"
    else
        log_info "Initializing Swarm mode..."
        docker swarm init
    fi

    # Create overlay networks
    log_info "Creating overlay networks..."

    if docker network ls | grep -q "erlmcp-overlay"; then
        log_info "erlmcp-overlay already exists"
    else
        docker network create --driver overlay --attachable --opt encrypted erlmcp-overlay
        log_info "Created erlmcp-overlay network"
    fi

    if docker network ls | grep -q "monitoring-overlay"; then
        log_info "monitoring-overlay already exists"
    else
        docker network create --driver overlay --attachable --opt encrypted monitoring-overlay
        log_info "Created monitoring-overlay network"
    fi

    # Label nodes
    log_info "Labeling nodes for service placement..."

    docker node ls --format '{{.Hostname}}' | while read -r node; do
        if [ -n "$node" ]; then
            docker node update --label-add erlmcp.enabled=true "$node" 2>/dev/null || true
        fi
    done

    log_info "Swarm initialization complete"
}

create_secrets() {
    log_info "Creating Docker secrets..."

    # Check if secrets already exist
    EXISTING_SECRETS=$(docker secret ls --format '{{.Name}}' | grep -c "^${STACK_NAME}-" || true)

    if [ "$EXISTING_SECRETS" -gt 0 ]; then
        log_warn "Existing secrets found. Remove them first:"
        docker secret ls --format '{{.Name}}' | grep "^${STACK_NAME}-"
        read -p "Replace existing secrets? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            log_info "Skipping secret creation"
            return
        fi
    fi

    # Generate random secrets
    log_info "Generating new secrets..."

    # Erlang cookie
    COOKIE=$(openssl rand -base64 32 | tr -d '/+=' | head -c 32)
    echo "$COOKIE" | docker secret create "${STACK_NAME}-erlang-cookie" -

    # Database password
    DB_PASSWORD=$(openssl rand -base64 32 | tr -d '/+=' | head -c 32)
    echo "$DB_PASSWORD" | docker secret create "${STACK_NAME}-db-password" -

    # Redis password
    REDIS_PASSWORD=$(openssl rand -base64 32 | tr -d '/+=' | head -c 32)
    echo "$REDIS_PASSWORD" | docker secret create "${STACK_NAME}-redis-password" -

    # Generate self-signed TLS certificate (for development/testing)
    if [ ! -f ./tls.crt ]; then
        log_info "Generating self-signed TLS certificate..."
        openssl req -x509 -newkey rsa:2048 -keyout tls.key -out tls.crt \
            -days 365 -nodes -subj "/CN=erlmcp" 2>/dev/null
    fi

    if [ -f ./tls.crt ] && [ -f ./tls.key ]; then
        docker secret create "${STACK_NAME}-tls-cert" ./tls.crt
        docker secret create "${STACK_NAME}-tls-key" ./tls.key
    fi

    log_info "Secrets created successfully"
    log_info "Store these passwords securely:"
    echo "  Erlang Cookie: $COOKIE"
    echo "  DB Password:   $DB_PASSWORD"
    echo "  Redis Password: $REDIS_PASSWORD"
}

deploy_stack() {
    log_info "Deploying ${STACK_NAME} stack..."

    # Check if environment file exists
    if [ ! -f "$ENV_FILE" ]; then
        if [ -f "${ENV_FILE}.template" ]; then
            log_warn "Environment file not found. Creating from template..."
            cp "${ENV_FILE}.template" "$ENV_FILE"
            log_warn "Please edit $ENV_FILE with your settings"
            return
        else
            log_error "No environment file found. Create $ENV_FILE first"
            return
        fi
    fi

    # Deploy the stack
    docker stack deploy -c "$COMPOSE_FILE" --env-file "$ENV_FILE" "$STACK_NAME"

    log_info "Waiting for services to start..."
    sleep 10

    # Show service status
    docker stack services "$STACK_NAME"

    log_info "Deployment complete. Check logs with: docker service logs -f ${STACK_NAME}_erlmcp"
}

update_stack() {
    log_info "Updating ${STACK_NAME} stack..."

    # Pull latest images
    log_info "Pulling latest images..."
    docker-compose -f "$COMPOSE_FILE" pull

    # Deploy with update
    docker stack deploy -c "$COMPOSE_FILE" --env-file "$ENV_FILE" --resolve-image=always "$STACK_NAME"

    log_info "Update complete. Monitor with: docker stack ps $STACK_NAME"
}

scale_services() {
    log_info "Scaling ${STACK_NAME} services..."

    # Scale erlmcp service
    if [ -n "${REPLICAS:-}" ]; then
        docker service scale "${STACK_NAME}_erlmcp=${REPLICAS}"
    fi

    # Show current replicas
    docker service ls --filter "name=${STACK_NAME}" --format "table {{.Name}}\t{{.Replicas}}"
}

show_status() {
    log_info "${STACK_NAME} stack status:"

    echo ""
    echo "Services:"
    docker stack services "$STACK_NAME"

    echo ""
    echo "Tasks:"
    docker stack ps "$STACK_NAME" --no-trunc

    echo ""
    echo "Nodes:"
    docker node ls
}

show_logs() {
    SERVICE="${1:-${STACK_NAME}_erlmcp}"

    log_info "Showing logs for $SERVICE..."
    docker service logs -f "$SERVICE"
}

rollback_stack() {
    log_info "Rolling back ${STACK_NAME} stack..."

    # Rollback erlmcp service
    docker service rollback "${STACK_NAME}_erlmcp"

    log_info "Rollback initiated. Monitor with: docker service ps $STACK_NAME"
}

clean_stack() {
    log_warn "This will remove the entire stack and all data!"
    read -p "Are you sure? (y/N): " -n 1 -r
    echo

    if [[ $REPLY =~ ^[Yy]$ ]]; then
        log_info "Removing ${STACK_NAME} stack..."
        docker stack rm "$STACK_NAME"

        log_info "Waiting for stack to be removed..."
        sleep 10

        # Remove networks
        docker network rm erlmcp-overlay 2>/dev/null || true
        docker network rm monitoring-overlay 2>/dev/null || true

        log_info "Stack removed"
    else
        log_info "Cleanup cancelled"
    fi
}

# Main script logic
main() {
    check_docker

    ACTION="${1:-help}"

    case "$ACTION" in
        init)
            init_swarm
            ;;
        secrets)
            create_secrets
            ;;
        deploy)
            check_swarm
            deploy_stack
            ;;
        update)
            check_swarm
            update_stack
            ;;
        scale)
            check_swarm
            scale_services
            ;;
        status)
            check_swarm
            show_status
            ;;
        logs)
            check_swarm
            show_logs "${2:-}"
            ;;
        rollback)
            check_swarm
            rollback_stack
            ;;
        clean)
            clean_stack
            ;;
        help|*)
            cat <<EOF
erlmcp Docker Swarm Deployment Script

Usage: $0 [action] [options]

Actions:
  init        Initialize Swarm and create overlay networks
  secrets     Generate and create Docker secrets
  deploy      Deploy the stack to Swarm
  update      Update the stack with new images/config
  scale       Scale services (set REPLICAS env var)
  status      Show stack service and task status
  logs [svc]  Show logs for a service (default: erlmcp)
  rollback    Rollback to previous version
  clean       Remove stack (careful!)
  help        Show this help message

Environment Variables:
  STACK_NAME  Stack name (default: erlmcp)
  COMPOSE_FILE Path to docker-stack.yml
  ENV_FILE    Path to environment file
  REPLICAS    Number of replicas for scaling

Examples:
  $0 init                  # Initialize Swarm
  $0 secrets               # Create secrets
  $0 deploy                # Deploy stack
  $0 logs                  # Show erlmcp logs
  REPLICAS=10 $0 scale     # Scale to 10 replicas
EOF
            ;;
    esac
}

main "$@"
