#!/bin/bash
# Deploy services to Docker Swarm
# Creates and manages all services defined in docker-compose.swarm.yml

set -e

SWARM_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPOSE_FILE="$SWARM_DIR/docker/docker-compose.swarm.yml"

echo "=== Docker Swarm Service Deployment ==="
echo "Swarm Directory: $SWARM_DIR"
echo "Compose File: $COMPOSE_FILE"

# Validate Docker Swarm is running
if ! docker info | grep -q "Swarm: active"; then
    echo "ERROR: Docker Swarm is not active"
    echo "Run: ./init_swarm.sh"
    exit 1
fi

# Validate compose file
if [ ! -f "$COMPOSE_FILE" ]; then
    echo "ERROR: Compose file not found: $COMPOSE_FILE"
    exit 1
fi

# Deploy using docker stack
echo -e "\n[Deploying] Starting services..."

docker stack deploy \
    --compose-file "$COMPOSE_FILE" \
    erlmcp-swarm

# Wait for services to stabilize
echo -e "\n[Waiting] Services starting (this may take 30-60 seconds)..."
sleep 10

# Monitor service status
echo -e "\n[Status] Service Status:"
docker service ls --filter "label=com.docker.stack.namespace=erlmcp-swarm" || docker service ls

# Show service details
echo -e "\n[Details] Service Details:"
docker service ps erlmcp-swarm_erlmcp-server 2>/dev/null || echo "Service not ready yet"
docker service ps erlmcp-swarm_traefik 2>/dev/null || echo "Service not ready yet"

# Wait for key services to become healthy
echo -e "\n[Health Check] Waiting for services to become healthy..."

TIMEOUT=120
ELAPSED=0
HEALTHY=0

while [ $ELAPSED -lt $TIMEOUT ]; do
    # Check traefik
    if docker service ps erlmcp-swarm_traefik 2>/dev/null | grep -q "Running"; then
        echo "✓ Load Balancer is running"
        HEALTHY=$((HEALTHY + 1))
    fi

    # Check prometheus
    if docker service ps erlmcp-swarm_prometheus 2>/dev/null | grep -q "Running"; then
        echo "✓ Prometheus is running"
        HEALTHY=$((HEALTHY + 1))
    fi

    # Check erlmcp servers
    RUNNING_SERVERS=$(docker service ps erlmcp-swarm_erlmcp-server 2>/dev/null | grep "Running" | wc -l || echo 0)
    if [ "$RUNNING_SERVERS" -gt 0 ]; then
        echo "✓ ErlMCP Servers: $RUNNING_SERVERS running"
        HEALTHY=$((HEALTHY + 1))
    fi

    if [ $HEALTHY -ge 3 ]; then
        break
    fi

    sleep 5
    ELAPSED=$((ELAPSED + 5))
    HEALTHY=0
done

# Print endpoint information
echo -e "\n=== Service Endpoints ==="
MANAGER_IP=$(docker info | grep "Node Address:" | awk '{print $3}' || echo "localhost")

echo "Load Balancer (Traefik):"
echo "  Web:       http://$MANAGER_IP:80"
echo "  Dashboard: http://$MANAGER_IP:8081"
echo "  Metrics:   http://$MANAGER_IP:9090"
echo ""
echo "MCP Services:"
echo "  WebSocket: ws://$MANAGER_IP:5555/mcp"
echo "  HTTP:      http://$MANAGER_IP:80/mcp"
echo ""
echo "Monitoring:"
echo "  Prometheus: http://$MANAGER_IP:9091"
echo "  Grafana:    http://$MANAGER_IP:3000"
echo ""
echo "Test Controller:"
echo "  API:        http://$MANAGER_IP:8888"
echo ""

# Show logs tip
echo "=== Useful Commands ==="
echo "View service logs:"
echo "  docker service logs erlmcp-swarm_erlmcp-server"
echo ""
echo "View service tasks:"
echo "  docker service ps erlmcp-swarm_erlmcp-server"
echo ""
echo "Scale a service:"
echo "  docker service scale erlmcp-swarm_erlmcp-server=10"
echo ""
echo "Remove all services:"
echo "  docker stack rm erlmcp-swarm"
echo ""
echo "=== Deployment Complete ==="
