#!/usr/bin/env bash
# ============================================================================
# GCP Simulation Stack - Status Script
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION
#
# Shows status of GCP simulation services
# ============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPOSE_DIR="$(dirname "$SCRIPT_DIR")"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

cd "$COMPOSE_DIR"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  GCP Simulation Stack Status${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Show container status
echo -e "${YELLOW}Container Status:${NC}"
echo ""
docker compose ps --format "table {{.Name}}\t{{.Status}}\t{{.Ports}}" 2>/dev/null || \
    docker compose ps

echo ""
echo -e "${YELLOW}Service Health Checks:${NC}"
echo ""

# Health check each service
check_service() {
    local name=$1
    local url=$2
    local timeout=${3:-5}

    if curl -sf --max-time "$timeout" "$url" > /dev/null 2>&1; then
        echo -e "  $name: ${GREEN}HEALTHY${NC}"
        return 0
    else
        echo -e "  $name: ${RED}UNHEALTHY${NC}"
        return 1
    fi
}

check_tcp() {
    local name=$1
    local host=$2
    local port=$3

    if nc -z "$host" "$port" 2>/dev/null; then
        echo -e "  $name: ${GREEN}HEALTHY${NC}"
        return 0
    else
        echo -e "  $name: ${RED}UNHEALTHY${NC}"
        return 1
    fi
}

# Check services
check_tcp "PostgreSQL (Cloud SQL)" localhost 5432 || true
check_tcp "Redis (Memorystore)" localhost 6379 || true
check_service "MinIO (GCS)" "http://localhost:9000/minio/health/live" || true
check_service "Vault (Secret Manager)" "http://localhost:8200/v1/sys/health" || true
check_service "Registry (Artifact Reg)" "http://localhost:5000/v2/" || true
check_service "Pub/Sub Emulator" "http://localhost:8085" || true
check_service "Firestore Emulator" "http://localhost:8086" || true
check_service "Prometheus (Monitoring)" "http://localhost:9090/-/healthy" || true
check_service "Grafana (Dashboard)" "http://localhost:3000/api/health" || true
check_service "Loki (Logging)" "http://localhost:3100/ready" || true
check_service "Traefik (Load Balancer)" "http://localhost:8080/ping" || true
check_service "OTEL Collector" "http://localhost:13133/" || true

echo ""
echo -e "${YELLOW}Resource Usage:${NC}"
echo ""
docker stats --no-stream --format "table {{.Name}}\t{{.CPUPerc}}\t{{.MemUsage}}" \
    $(docker compose ps -q 2>/dev/null) 2>/dev/null || echo "  (no containers running)"

echo ""
