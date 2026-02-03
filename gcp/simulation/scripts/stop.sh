#!/usr/bin/env bash
# ============================================================================
# GCP Simulation Stack - Stop Script
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION
#
# Stops the GCP simulation stack
#
# Usage:
#   ./stop.sh           # Stop all services (keep volumes)
#   ./stop.sh --clean   # Stop and remove volumes
# ============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPOSE_DIR="$(dirname "$SCRIPT_DIR")"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

CLEAN=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --clean)
            CLEAN=true
            shift
            ;;
        --help|-h)
            echo "GCP Simulation Stack Stop"
            echo ""
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --clean   Remove volumes (destroys all data)"
            echo "  --help    Show this help message"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

cd "$COMPOSE_DIR"

echo -e "${YELLOW}Stopping GCP simulation stack...${NC}"

if [[ "$CLEAN" == "true" ]]; then
    echo -e "${RED}WARNING: This will delete all data!${NC}"
    read -p "Are you sure? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        docker compose --profile kubernetes --profile app down -v --remove-orphans
        echo -e "${GREEN}All services stopped and volumes removed${NC}"
    else
        echo "Aborted"
        exit 1
    fi
else
    docker compose --profile kubernetes --profile app down --remove-orphans
    echo -e "${GREEN}All services stopped (volumes preserved)${NC}"
fi
