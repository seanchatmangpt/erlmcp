#!/usr/bin/env bash
# ============================================================================
# Craftplan ERP Deployment Script - Docker Swarm
# ============================================================================
# One-command deployment of Craftplan ERP on erlmcp Docker Swarm
#
# Usage:
#   ./scripts/craftplan-deploy.sh [environment]
#
# Arguments:
#   environment  - dev|staging|prod (default: dev)
#
# Prerequisites:
#   - Docker Swarm initialized
#   - Traefik ingress controller running
#   - Overlay networks created (erlmcp-frontend, erlmcp-backend, erlmcp-mgmt)
# ============================================================================
set -euo pipefail

# Configuration
ENVIRONMENT="${1:-dev}"
NAMESPACE="craftplan-${ENVIRONMENT}"
COMPOSE_FILE="docker-compose.craftplan.yml"
ENV_FILE=".env.craftplan"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_step() { echo -e "${BLUE}[STEP]${NC} $1"; }

# ============================================================================
# Pre-flight Checks
# ============================================================================

check_prerequisites() {
    log_step "Running pre-flight checks..."

    # Check Docker Swarm
    if ! docker info --format '{{.Swarm.LocalNodeState}}' 2>/dev/null | grep -q 'active'; then
        log_error "Docker Swarm is not active"
        exit 1
    fi

    # Check compose file
    if [[ ! -f "${COMPOSE_FILE}" ]]; then
        log_error "Compose file not found: ${COMPOSE_FILE}"
        exit 1
    fi

    # Check environment file
    if [[ ! -f "${ENV_FILE}" ]]; then
        log_warn "Environment file not found: ${ENV_FILE}"
        log_info "Creating from example..."
        if [[ -f ".env.craftplan.example" ]]; then
            cp .env.craftplan.example "${ENV_FILE}"
            log_info "Created ${ENV_FILE} - please review and update"
        else
            log_error "Example environment file not found"
            exit 1
        fi
    fi

    # Check overlay networks
    local required_networks=("erlmcp-frontend" "erlmcp-backend" "erlmcp-mgmt")
    for network in "${required_networks[@]}"; do
        if ! docker network ls --format '{{.Name}}' | grep -q "^${network}$"; then
            log_error "Required network not found: ${network}"
            log_info "Create it with: docker network create --driver overlay --attachable ${network}"
            exit 1
        fi
    done

    # Check secrets
    local required_secrets=(
        "${NAMESPACE}_secret_key_base"
        "${NAMESPACE}_token_signing_secret"
        "${NAMESPACE}_cloak_key"
        "${NAMESPACE}_db_password"
    )
    local missing_secrets=()
    for secret in "${required_secrets[@]}"; do
        if ! docker secret ls --format '{{.Name}}' | grep -q "^${secret}$"; then
            missing_secrets+=("${secret}")
        fi
    done

    if [[ ${#missing_secrets[@]} -gt 0 ]]; then
        log_error "Missing Docker secrets:"
        printf '  - %s\n' "${missing_secrets[@]}"
        log_info "Generate secrets with: ./scripts/craftplan-secrets-setup.sh ${ENVIRONMENT}"
        exit 1
    fi

    log_info "All prerequisites met ✓"
}

# ============================================================================
# Node Labeling
# ============================================================================

label_nodes() {
    log_step "Labeling nodes for Craftplan placement..."

    local nodes
    nodes=$(docker node ls --format '{{.Hostname}}')

    # Get first node for each service type
    local first_node
    first_node=$(echo "$nodes" | head -n 1)

    # Label nodes if not already labeled
    if ! docker node inspect "${first_node}" --format '{{.Spec.Labels}}' | grep -q "craftplan=true"; then
        docker node update --label-add craftplan=true "${first_node}"
        log_info "Labeled ${first_node} with craftplan=true"
    fi

    if ! docker node inspect "${first_node}" --format '{{.Spec.Labels}}' | grep -q "craftplan-db=true"; then
        docker node update --label-add craftplan-db=true "${first_node}"
        log_info "Labeled ${first_node} with craftplan-db=true"
    fi

    if ! docker node inspect "${first_node}" --format '{{.Spec.Labels}}' | grep -q "craftplan-storage=true"; then
        docker node update --label-add craftplan-storage=true "${first_node}"
        log_info "Labeled ${first_node} with craftplan-storage=true"
    fi

    log_info "Node labeling complete ✓"
}

# ============================================================================
# Directory Setup
# ============================================================================

setup_directories() {
    log_step "Creating data directories..."

    # Source environment file
    source "${ENV_FILE}"

    local dirs=(
        "${CRAFTPLAN_DB_PATH:-/var/lib/craftplan/db}"
        "${CRAFTPLAN_DB_LOGS_PATH:-/var/log/craftplan/postgres}"
        "${CRAFTPLAN_S3_PATH:-/var/lib/craftplan/s3}"
        "${CRAFTPLAN_BACKUPS_PATH:-/var/lib/craftplan/backups}"
    )

    # Note: These need to be created on the actual swarm nodes
    log_info "Ensure these directories exist on labeled nodes:"
    printf '  - %s\n' "${dirs[@]}"
    log_warn "Run on each node: sudo mkdir -p ${dirs[0]} ${dirs[1]} ${dirs[2]} ${dirs[3]}"
}

# ============================================================================
# Deploy Stack
# ============================================================================

deploy_stack() {
    log_step "Deploying Craftplan stack: ${NAMESPACE}"

    # Export environment variables for compose
    set -a
    source "${ENV_FILE}"
    set +a

    # Deploy with namespace
    docker stack deploy -c "${COMPOSE_FILE}" "${NAMESPACE}"

    log_info "Stack deployment initiated..."
}

# ============================================================================
# Wait for Services
# ============================================================================

wait_for_services() {
    log_step "Waiting for services to start..."

    local services=("craftplan" "craftplan-db" "craftplan-s3")
    local max_wait=120
    local waited=0

    while [[ $waited -lt $max_wait ]]; do
        local all_running=true
        for service in "${services[@]}"; do
            local full_service="${NAMESPACE}_${service}"
            if ! docker service ps --format '{{.CurrentState}}' "${full_service}" 2>/dev/null | grep -q "Running"; then
                all_running=false
                break
            fi
        done

        if [[ "$all_running" == "true" ]]; then
            log_info "All services are running ✓"
            return 0
        fi

        sleep 5
        ((waited += 5))
        echo -n "."
    done

    echo
    log_warn "Services may still be starting..."
}

# ============================================================================
# Show Status
# ============================================================================

show_status() {
    log_step "Service Status"
    echo
    docker stack services "${NAMESPACE}" --format "table {{.Name}}\t{{.Replicas}}\t{{.Ports}}"
    echo
    log_info "Services:"
    echo "  - Craftplan Web:  http://localhost:4000"
    echo "  - Traefik Route:  https://${CRAFTPLAN_HOST:-craftplan.local}"
    echo "  - MinIO Console:  http://localhost:9001"
    echo
    log_info "Commands:"
    echo "  - View logs:      docker service logs ${NAMESPACE}_craftplan -f"
    echo "  - Remove stack:   docker stack rm ${NAMESPACE}"
    echo "  - Scale service:  docker service scale ${NAMESPACE}_craftplan=3"
}

# ============================================================================
# Main
# ============================================================================

main() {
    log_info "Deploying Craftplan ERP - Environment: ${ENVIRONMENT}"
    log_info "Namespace: ${NAMESPACE}"
    echo

    check_prerequisites
    label_nodes
    setup_directories
    deploy_stack
    wait_for_services
    show_status

    echo
    log_info "Deployment complete! ✓"
    log_info "Default credentials (change after first login):"
    echo "  Email:    test@test.com"
    echo "  Password: Aa123123123123"
}

main "$@"
