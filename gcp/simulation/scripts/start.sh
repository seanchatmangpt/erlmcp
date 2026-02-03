#!/usr/bin/env bash
# ============================================================================
# GCP Simulation Stack - Startup Script
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION
#
# Starts the GCP simulation stack with optional profiles
#
# Usage:
#   ./start.sh              # Start core services only
#   ./start.sh --full       # Start all services including K8s
#   ./start.sh --app        # Start with ErlMCP application
#   ./start.sh --dev        # Start development mode (core + monitoring)
# ============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPOSE_DIR="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default values
PROFILE=""
DETACH="-d"
WAIT_HEALTHY=true

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --full)
            PROFILE="--profile kubernetes --profile app"
            shift
            ;;
        --app)
            PROFILE="--profile app"
            shift
            ;;
        --dev)
            # Core services only (default)
            shift
            ;;
        --foreground|-f)
            DETACH=""
            shift
            ;;
        --no-wait)
            WAIT_HEALTHY=false
            shift
            ;;
        --help|-h)
            echo "GCP Simulation Stack Startup"
            echo ""
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --full        Start all services including Kubernetes (k3s)"
            echo "  --app         Start with ErlMCP application"
            echo "  --dev         Start core services only (default)"
            echo "  --foreground  Run in foreground (don't detach)"
            echo "  --no-wait     Don't wait for services to be healthy"
            echo "  --help        Show this help message"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  GCP Simulation Stack${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Check Docker
if ! command -v docker &> /dev/null; then
    echo -e "${RED}ERROR: Docker is not installed${NC}"
    exit 1
fi

if ! docker info &> /dev/null; then
    echo -e "${RED}ERROR: Docker daemon is not running${NC}"
    exit 1
fi

# Change to compose directory
cd "$COMPOSE_DIR"

# Create config directories if they don't exist
mkdir -p config/k3s

# Pull images first
echo -e "${YELLOW}Pulling Docker images...${NC}"
docker compose pull --quiet 2>/dev/null || true

# Start services
echo -e "${YELLOW}Starting GCP simulation services...${NC}"
# shellcheck disable=SC2086
docker compose $PROFILE up $DETACH --remove-orphans

if [[ -n "$DETACH" ]] && [[ "$WAIT_HEALTHY" == "true" ]]; then
    echo ""
    echo -e "${YELLOW}Waiting for services to be healthy...${NC}"

    # Wait for core services
    services=("postgres" "redis" "minio" "vault" "prometheus")
    for service in "${services[@]}"; do
        echo -n "  Waiting for $service..."
        timeout 60 bash -c "until docker compose ps $service 2>/dev/null | grep -q 'healthy'; do sleep 1; done" 2>/dev/null && \
            echo -e " ${GREEN}OK${NC}" || echo -e " ${YELLOW}TIMEOUT${NC}"
    done

    echo ""
    echo -e "${GREEN}========================================${NC}"
    echo -e "${GREEN}  GCP Simulation Stack Started${NC}"
    echo -e "${GREEN}========================================${NC}"
    echo ""
    echo "Service Endpoints:"
    echo "  PostgreSQL (Cloud SQL):     localhost:5432"
    echo "  Redis (Memorystore):        localhost:6379"
    echo "  MinIO Console (GCS):        http://localhost:9001"
    echo "  MinIO API (GCS):            http://localhost:9000"
    echo "  Vault (Secret Manager):     http://localhost:8200"
    echo "  Registry (Artifact Reg):    http://localhost:5000"
    echo "  Pub/Sub Emulator:           localhost:8085"
    echo "  Firestore Emulator:         localhost:8086"
    echo "  Prometheus (Monitoring):    http://localhost:9090"
    echo "  Grafana (Dashboard):        http://localhost:3000"
    echo "  Loki (Logging):             http://localhost:3100"
    echo "  Traefik (Load Balancer):    http://localhost:80"
    echo "  Traefik Dashboard:          http://localhost:8080"
    echo "  OTEL Collector:             localhost:4317 (gRPC)"
    echo ""
    echo "Credentials:"
    echo "  PostgreSQL: erlmcp / erlmcp_secret"
    echo "  Redis: erlmcp_redis"
    echo "  MinIO: erlmcp_storage / erlmcp_storage_secret"
    echo "  Vault Token: erlmcp_vault_token"
    echo "  Grafana: admin / erlmcp_grafana"
    echo ""
    echo "Environment Variables for ErlMCP:"
    echo "  export PUBSUB_EMULATOR_HOST=localhost:8085"
    echo "  export FIRESTORE_EMULATOR_HOST=localhost:8086"
    echo "  export VAULT_ADDR=http://localhost:8200"
    echo "  export VAULT_TOKEN=erlmcp_vault_token"
    echo ""
fi
