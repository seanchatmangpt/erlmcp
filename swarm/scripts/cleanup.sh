#!/bin/bash
# Cleanup Docker Swarm resources
# Removes services, volumes, networks, and optionally leaves swarm

set -e

SWARM_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo "=== Docker Swarm Cleanup Script ==="

# Parse arguments
FORCE=${1:-"false"}
LEAVE_SWARM=${2:-"false"}

# Confirm before cleanup
if [ "$FORCE" != "force" ]; then
    echo "This will remove:"
    echo "  - All services (erlmcp-swarm stack)"
    echo "  - All named volumes"
    echo "  - All overlay networks"
    echo ""
    read -p "Continue? (y/N) " -n 1 -r
    echo ""
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Cleanup cancelled"
        exit 0
    fi
fi

# Remove services
echo -e "\n[Services] Removing services..."
if docker service ls | grep -q "erlmcp-swarm"; then
    docker stack rm erlmcp-swarm 2>/dev/null || true
    echo "Waiting for services to be removed..."
    sleep 10
else
    echo "No services found"
fi

# Remove volumes
echo -e "\n[Volumes] Removing named volumes..."
for vol in erlmcp-data erlmcp-logs prometheus-data grafana-data test-results; do
    if docker volume ls | grep -q "^$vol\$"; then
        docker volume rm "$vol" || echo "Could not remove $vol"
    else
        echo "Volume $vol not found"
    fi
done

# Remove networks
echo -e "\n[Networks] Removing overlay networks..."
for net in mcp-network monitoring; do
    if docker network ls | grep -q "$net"; then
        docker network rm "$net" || echo "Could not remove $net"
    else
        echo "Network $net not found"
    fi
done

# Optional: Leave swarm
if [ "$LEAVE_SWARM" = "leave" ]; then
    echo -e "\n[Swarm] Leaving Docker Swarm..."
    docker swarm leave --force || echo "Could not leave swarm"
fi

# Verification
echo -e "\n[Verification] Current state:"
echo "Services:"
docker service ls 2>/dev/null || echo "  (none)"
echo "Volumes:"
docker volume ls 2>/dev/null | grep -E "erlmcp|prometheus|grafana|test-results" || echo "  (none)"
echo "Networks:"
docker network ls 2>/dev/null | grep -E "mcp-network|monitoring" || echo "  (none)"

echo -e "\n=== Cleanup Complete ==="
