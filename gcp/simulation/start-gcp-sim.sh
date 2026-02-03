#!/usr/bin/env bash
# ============================================================================
# GCP Simulation Stack Startup Script
# ============================================================================
# Starts the GCP simulation services for local development
#
# Usage:
#   ./start-gcp-sim.sh          # Start core services
#   ./start-gcp-sim.sh --full   # Start all services including optional ones
#   ./start-gcp-sim.sh --init   # Start with initialization
#   ./start-gcp-sim.sh --stop   # Stop all services
#   ./start-gcp-sim.sh --status # Show service status
#
# DOCKER-ONLY CONSTITUTION: This script uses Docker Compose exclusively.
# ============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPOSE_FILE="${SCRIPT_DIR}/docker-compose.gcp-sim.yml"
ENV_FILE="${SCRIPT_DIR}/.env"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $*"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }

# Check if env file exists, create from example if not
check_env_file() {
    if [[ ! -f "$ENV_FILE" ]]; then
        if [[ -f "${ENV_FILE}.example" ]]; then
            log_info "Creating .env from .env.example..."
            cp "${ENV_FILE}.example" "$ENV_FILE"
        else
            log_warn "No .env file found, using defaults"
        fi
    fi
}

# Start services
start_services() {
    local profiles=("$@")

    log_info "Starting GCP simulation services..."

    check_env_file

    # Build compose command
    local compose_cmd="docker compose -f ${COMPOSE_FILE}"

    if [[ -f "$ENV_FILE" ]]; then
        compose_cmd+=" --env-file ${ENV_FILE}"
    fi

    for profile in "${profiles[@]}"; do
        compose_cmd+=" --profile ${profile}"
    done

    compose_cmd+=" up -d"

    log_info "Running: $compose_cmd"
    eval "$compose_cmd"

    log_success "GCP simulation services started!"
    echo ""
    show_endpoints
}

# Stop services
stop_services() {
    log_info "Stopping GCP simulation services..."

    docker compose -f "${COMPOSE_FILE}" --profile full --profile init --profile monitoring down

    log_success "GCP simulation services stopped"
}

# Show status
show_status() {
    log_info "GCP Simulation Service Status:"
    echo ""
    docker compose -f "${COMPOSE_FILE}" ps
}

# Show endpoints
show_endpoints() {
    echo -e "${GREEN}╔════════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║              GCP Simulation Endpoints                              ║${NC}"
    echo -e "${GREEN}╠════════════════════════════════════════════════════════════════════╣${NC}"
    echo -e "${GREEN}║${NC} Service              │ Endpoint                                 ${GREEN}║${NC}"
    echo -e "${GREEN}╠════════════════════════════════════════════════════════════════════╣${NC}"
    echo -e "${GREEN}║${NC} Cloud SQL (Postgres) │ postgresql://localhost:5432/erlmcp       ${GREEN}║${NC}"
    echo -e "${GREEN}║${NC} Cloud SQL Proxy      │ postgresql://localhost:15432/erlmcp      ${GREEN}║${NC}"
    echo -e "${GREEN}║${NC} Memorystore (Redis)  │ redis://localhost:6379                   ${GREEN}║${NC}"
    echo -e "${GREEN}║${NC} Cloud Storage (API)  │ http://localhost:9000                    ${GREEN}║${NC}"
    echo -e "${GREEN}║${NC} Cloud Storage (UI)   │ http://localhost:9001 (admin/admin_secret)${GREEN}║${NC}"
    echo -e "${GREEN}║${NC} Pub/Sub Emulator     │ http://localhost:8085                    ${GREEN}║${NC}"
    echo -e "${GREEN}║${NC} Secret Manager       │ http://localhost:8200 (token: root)     ${GREEN}║${NC}"
    echo -e "${GREEN}╠════════════════════════════════════════════════════════════════════╣${NC}"
    echo -e "${GREEN}║${NC} ${YELLOW}Full Profile Only:${NC}                                             ${GREEN}║${NC}"
    echo -e "${GREEN}║${NC} Firestore Emulator   │ http://localhost:8086                    ${GREEN}║${NC}"
    echo -e "${GREEN}║${NC} IAM Mock             │ http://localhost:8080                    ${GREEN}║${NC}"
    echo -e "${GREEN}║${NC} Load Balancer UI     │ http://localhost:8081                    ${GREEN}║${NC}"
    echo -e "${GREEN}║${NC} Metadata Server      │ http://localhost:8082                    ${GREEN}║${NC}"
    echo -e "${GREEN}╠════════════════════════════════════════════════════════════════════╣${NC}"
    echo -e "${GREEN}║${NC} ${YELLOW}Monitoring Profile:${NC}                                            ${GREEN}║${NC}"
    echo -e "${GREEN}║${NC} Prometheus           │ http://localhost:9090                    ${GREEN}║${NC}"
    echo -e "${GREEN}║${NC} Grafana              │ http://localhost:3000 (admin/admin)     ${GREEN}║${NC}"
    echo -e "${GREEN}║${NC} Loki (Logging)       │ http://localhost:3100                    ${GREEN}║${NC}"
    echo -e "${GREEN}╚════════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo -e "${BLUE}Environment Variables for Applications:${NC}"
    echo "  export PUBSUB_EMULATOR_HOST=localhost:8085"
    echo "  export FIRESTORE_EMULATOR_HOST=localhost:8086"
    echo "  export VAULT_ADDR=http://localhost:8200"
    echo "  export VAULT_TOKEN=root"
    echo "  export AWS_ENDPOINT_URL=http://localhost:9000"
    echo "  export GCE_METADATA_HOST=localhost:8082"
    echo ""
}

# Health check
health_check() {
    log_info "Running health checks..."
    echo ""

    local all_healthy=true

    # Cloud SQL
    if docker compose -f "${COMPOSE_FILE}" exec -T cloudsql-sim pg_isready -U erlmcp -d erlmcp >/dev/null 2>&1; then
        echo -e "  Cloud SQL:        ${GREEN}✓ healthy${NC}"
    else
        echo -e "  Cloud SQL:        ${RED}✗ unhealthy${NC}"
        all_healthy=false
    fi

    # Redis
    if docker compose -f "${COMPOSE_FILE}" exec -T memorystore-sim redis-cli ping >/dev/null 2>&1; then
        echo -e "  Memorystore:      ${GREEN}✓ healthy${NC}"
    else
        echo -e "  Memorystore:      ${RED}✗ unhealthy${NC}"
        all_healthy=false
    fi

    # MinIO
    if curl -sf http://localhost:9000/minio/health/live >/dev/null 2>&1; then
        echo -e "  Cloud Storage:    ${GREEN}✓ healthy${NC}"
    else
        echo -e "  Cloud Storage:    ${RED}✗ unhealthy${NC}"
        all_healthy=false
    fi

    # Vault
    if curl -sf http://localhost:8200/v1/sys/health >/dev/null 2>&1; then
        echo -e "  Secret Manager:   ${GREEN}✓ healthy${NC}"
    else
        echo -e "  Secret Manager:   ${RED}✗ unhealthy${NC}"
        all_healthy=false
    fi

    # Pub/Sub
    if curl -sf http://localhost:8085 >/dev/null 2>&1; then
        echo -e "  Pub/Sub:          ${GREEN}✓ healthy${NC}"
    else
        echo -e "  Pub/Sub:          ${YELLOW}○ not running${NC}"
    fi

    echo ""

    if $all_healthy; then
        log_success "All core services are healthy"
        return 0
    else
        log_error "Some services are unhealthy"
        return 1
    fi
}

# Print usage
usage() {
    echo "GCP Simulation Stack Manager"
    echo ""
    echo "Usage: $0 [command] [options]"
    echo ""
    echo "Commands:"
    echo "  start             Start core GCP simulation services"
    echo "  start --full      Start all services (including Firestore, IAM, Load Balancer)"
    echo "  start --init      Start with initialization containers"
    echo "  start --monitoring Start with monitoring stack (Prometheus, Grafana, Loki)"
    echo "  stop              Stop all services"
    echo "  status            Show service status"
    echo "  health            Run health checks"
    echo "  endpoints         Show service endpoints"
    echo "  logs [service]    Show logs for service(s)"
    echo "  help              Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0 start                    # Start core services"
    echo "  $0 start --full --init      # Start everything with initialization"
    echo "  $0 logs cloudsql-sim        # View Cloud SQL logs"
    echo "  $0 stop                     # Stop all services"
}

# Main
main() {
    local command="${1:-help}"
    shift || true

    case "$command" in
        start)
            local profiles=()
            while [[ $# -gt 0 ]]; do
                case "$1" in
                    --full)
                        profiles+=("full")
                        shift
                        ;;
                    --init)
                        profiles+=("init")
                        shift
                        ;;
                    --monitoring)
                        profiles+=("monitoring")
                        shift
                        ;;
                    *)
                        log_error "Unknown option: $1"
                        exit 1
                        ;;
                esac
            done
            start_services "${profiles[@]}"
            ;;
        stop)
            stop_services
            ;;
        status)
            show_status
            ;;
        health)
            health_check
            ;;
        endpoints)
            show_endpoints
            ;;
        logs)
            docker compose -f "${COMPOSE_FILE}" logs -f "$@"
            ;;
        help|--help|-h)
            usage
            ;;
        *)
            log_error "Unknown command: $command"
            usage
            exit 1
            ;;
    esac
}

main "$@"
