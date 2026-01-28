#!/bin/bash
# Initialize Docker Swarm cluster
# Creates master node, initializes swarm, and prepares worker nodes

set -e

SWARM_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_DIR="$SWARM_DIR/scripts"

echo "=== Docker Swarm Initialization Script ==="
echo "Swarm Directory: $SWARM_DIR"

# Check Docker is installed
if ! command -v docker &> /dev/null; then
    echo "ERROR: Docker is not installed"
    exit 1
fi

# Function to initialize swarm on a node
init_swarm_master() {
    echo -e "\n[Master] Initializing Docker Swarm on master node..."

    # Initialize swarm
    SWARM_INIT=$(docker swarm init 2>&1 || true)

    if echo "$SWARM_INIT" | grep -q "already part of a swarm"; then
        echo "Swarm already initialized"
    else
        echo "Swarm initialized"
    fi

    # Get join token for workers
    WORKER_TOKEN=$(docker swarm join-token -q worker)
    MANAGER_TOKEN=$(docker swarm join-token -q manager)

    echo "Worker Token: $WORKER_TOKEN" > "$SWARM_DIR/.swarm-tokens"
    echo "Manager Token: $MANAGER_TOKEN" >> "$SWARM_DIR/.swarm-tokens"

    echo "Tokens saved to .swarm-tokens"
}

# Create networks
create_networks() {
    echo -e "\n[Network] Creating overlay networks..."

    # MCP network
    docker network create \
        --driver overlay \
        --driver-opt com.docker.network.driver.overlay.vxlanid=4096 \
        --opt "com.docker.network.driver.overlay.vxlanid=4096" \
        mcp-network 2>/dev/null || echo "mcp-network already exists"

    # Monitoring network
    docker network create \
        --driver overlay \
        --driver-opt com.docker.network.driver.overlay.vxlanid=4097 \
        monitoring 2>/dev/null || echo "monitoring already exists"

    docker network ls | grep -E "mcp-network|monitoring"
}

# Create volumes
create_volumes() {
    echo -e "\n[Volumes] Creating named volumes..."

    for vol in erlmcp-data erlmcp-logs prometheus-data grafana-data test-results; do
        docker volume create $vol 2>/dev/null || echo "$vol already exists"
    done

    docker volume ls | grep erlmcp
}

# Build images
build_images() {
    echo -e "\n[Images] Building Docker images..."

    echo "Building erlmcp server image..."
    cd "$SWARM_DIR/.."
    docker build \
        -f swarm/docker/Dockerfile.erlmcp-swarm \
        -t erlmcp:latest \
        . || echo "Build failed or image already exists"

    echo "Building MCP client simulator..."
    cd "$SWARM_DIR/clients"
    docker build \
        -f Dockerfile.go \
        -t mcp-client-simulator:latest \
        . || echo "Build failed or image already exists"

    echo "Images built successfully"
    docker images | grep -E "erlmcp|mcp-client"
}

# Validate setup
validate_setup() {
    echo -e "\n[Validation] Checking setup..."

    echo "Docker Swarm Status:"
    docker info | grep "Swarm: " || true

    echo "Networks:"
    docker network ls | grep -E "mcp-network|monitoring" || echo "Networks not found"

    echo "Volumes:"
    docker volume ls | grep erlmcp || echo "Volumes not found"

    echo "Images:"
    docker images | grep -E "erlmcp|mcp-client" || echo "Images not found"
}

# Main execution
main() {
    init_swarm_master
    create_networks
    create_volumes
    build_images
    validate_setup

    echo -e "\n=== Swarm Initialization Complete ==="
    echo "Next steps:"
    echo "  1. Copy .swarm-tokens to worker nodes"
    echo "  2. Run: docker swarm join --token <WORKER_TOKEN> <MANAGER_IP>:2377"
    echo "  3. Deploy services: ./deploy.sh"
    echo ""
    echo "To check swarm nodes:"
    echo "  docker node ls"
    echo ""
    echo "To view services:"
    echo "  docker service ls"
}

main "$@"
