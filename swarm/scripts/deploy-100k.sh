#!/bin/bash
# Deploy erlmcp to Docker Swarm for 100K concurrent connection testing
# Optimized deployment with proper resource allocation and monitoring

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SWARM_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
DOCKER_DIR="$SWARM_DIR/docker"
COMPOSE_FILE="$DOCKER_DIR/docker-compose.swarm.yml"
REPO_ROOT="$(cd "$SWARM_DIR/../.." && pwd)"

BLUE='\033[0;34m'
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[✓]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."

    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed"
        exit 1
    fi

    if ! docker info | grep -q "Swarm: active"; then
        log_error "Docker Swarm is not active. Run: docker swarm init"
        exit 1
    fi

    if ! command -v curl &> /dev/null; then
        log_error "curl is not installed"
        exit 1
    fi

    log_success "All prerequisites met"
}

# Build erlmcp Docker image
build_image() {
    log_info "Building erlmcp Docker image..."

    cd "$REPO_ROOT"

    BUILD_DATE=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    VCS_REF=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
    VERSION="0.7.0-swarm-100k"

    docker build \
        --build-arg BUILD_DATE="$BUILD_DATE" \
        --build-arg VCS_REF="$VCS_REF" \
        --build-arg VERSION="$VERSION" \
        -f Dockerfile \
        -t erlmcp:latest \
        -t "erlmcp:${VERSION}" \
        . || {
        log_error "Docker build failed"
        exit 1
    }

    log_success "Docker image built: erlmcp:latest"
    docker images | grep erlmcp | head -3
}

# Create overlay networks
create_networks() {
    log_info "Creating overlay networks..."

    docker network create \
        --driver overlay \
        --driver-opt com.docker.network.driver.overlay.vxlanid=4096 \
        --opt com.docker.network.driver.overlay.vxlanid=4096 \
        --scope swarm \
        mcp-network 2>/dev/null || log_warning "mcp-network already exists"

    docker network create \
        --driver overlay \
        --driver-opt com.docker.network.driver.overlay.vxlanid=4097 \
        --opt com.docker.network.driver.overlay.vxlanid=4097 \
        --scope swarm \
        monitoring 2>/dev/null || log_warning "monitoring already exists"

    log_success "Networks created/verified"
    docker network ls | grep -E "mcp-network|monitoring"
}

# Create volumes
create_volumes() {
    log_info "Creating named volumes..."

    for vol in erlmcp-data erlmcp-logs prometheus-data grafana-data test-results; do
        docker volume create "$vol" 2>/dev/null || log_warning "$vol already exists"
    done

    log_success "Volumes created/verified"
    docker volume ls | grep erlmcp | head -5
}

# Deploy stack to Swarm
deploy_stack() {
    log_info "Deploying erlmcp stack to Docker Swarm..."

    cd "$DOCKER_DIR"

    if docker stack ls | grep -q "erlmcp-swarm"; then
        log_warning "Stack 'erlmcp-swarm' already exists. Updating..."
        docker stack rm erlmcp-swarm
        sleep 5
    fi

    docker stack deploy \
        --compose-file "$COMPOSE_FILE" \
        erlmcp-swarm

    log_success "Stack deployed: erlmcp-swarm"
}

# Monitor service startup
monitor_startup() {
    log_info "Monitoring service startup..."

    TIMEOUT=180
    ELAPSED=0
    ERLMCP_HEALTHY=0
    TRAEFIK_HEALTHY=0
    PROMETHEUS_HEALTHY=0

    while [ $ELAPSED -lt $TIMEOUT ]; do
        # Check erlmcp servers
        RUNNING_SERVERS=$(docker service ps erlmcp-swarm_erlmcp-server 2>/dev/null | grep -c "Running" || echo "0")

        if [ "$RUNNING_SERVERS" -ge 4 ]; then
            if [ $ERLMCP_HEALTHY -eq 0 ]; then
                log_success "✓ ErlMCP Servers: $RUNNING_SERVERS running"
                ERLMCP_HEALTHY=1
            fi
        else
            log_warning "  ErlMCP Servers: $RUNNING_SERVERS running (waiting...)"
        fi

        # Check traefik
        if docker service ps erlmcp-swarm_traefik 2>/dev/null | grep -q "Running"; then
            if [ $TRAEFIK_HEALTHY -eq 0 ]; then
                log_success "✓ Load Balancer (Traefik) is running"
                TRAEFIK_HEALTHY=1
            fi
        fi

        # Check prometheus
        if docker service ps erlmcp-swarm_prometheus 2>/dev/null | grep -q "Running"; then
            if [ $PROMETHEUS_HEALTHY -eq 0 ]; then
                log_success "✓ Prometheus is running"
                PROMETHEUS_HEALTHY=1
            fi
        fi

        if [ $ERLMCP_HEALTHY -eq 1 ] && [ $TRAEFIK_HEALTHY -eq 1 ] && [ $PROMETHEUS_HEALTHY -eq 1 ]; then
            log_success "All core services are healthy"
            break
        fi

        sleep 5
        ELAPSED=$((ELAPSED + 5))
    done

    if [ $ELAPSED -ge $TIMEOUT ]; then
        log_warning "Timeout waiting for services. They may still be starting."
    fi
}

# Print service endpoints
print_endpoints() {
    log_info "Service Endpoints"

    MANAGER_IP=$(docker info | grep "Node Address:" | awk '{print $3}' || echo "localhost")

    echo ""
    echo "=== MCP Services ==="
    echo "  HTTP API:    http://$MANAGER_IP:80/mcp"
    echo "  WebSocket:   ws://$MANAGER_IP:5555/mcp"
    echo "  Health:      http://$MANAGER_IP:8080/health"
    echo ""

    echo "=== Monitoring ==="
    echo "  Prometheus:  http://$MANAGER_IP:9091"
    echo "  Grafana:     http://$MANAGER_IP:3000 (admin/admin)"
    echo "  Traefik:     http://$MANAGER_IP:8081"
    echo ""

    echo "=== Service Management ==="
    echo "  View services:    docker service ls"
    echo "  View tasks:       docker service ps erlmcp-swarm_erlmcp-server"
    echo "  View logs:        docker service logs erlmcp-swarm_erlmcp-server"
    echo "  Scale up:         docker service scale erlmcp-swarm_erlmcp-server=16"
    echo "  Remove stack:     docker stack rm erlmcp-swarm"
    echo ""
}

# Print configuration summary
print_configuration() {
    log_info "Deployment Configuration"
    echo ""
    echo "=== Cluster Configuration ==="
    docker node ls || log_warning "Could not list nodes"
    echo ""

    echo "=== Service Configuration ==="
    docker service ls | grep erlmcp-swarm || log_warning "Services not yet deployed"
    echo ""

    echo "=== Network Configuration ==="
    docker network inspect mcp-network | grep -E "Name|Scope|Driver" || log_warning "Network not configured"
    echo ""

    echo "=== Stack Status ==="
    docker stack ls 2>/dev/null | grep erlmcp || log_warning "Stack not yet deployed"
    echo ""
}

# Main execution
main() {
    echo ""
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║  ErlMCP Docker Swarm Deployment - 100K Concurrent Test      ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo ""

    check_prerequisites
    create_networks
    create_volumes
    build_image
    deploy_stack
    monitor_startup
    print_configuration
    print_endpoints

    log_success "Deployment complete!"
    log_info "Run stress tests with: $SWARM_DIR/scripts/stress-test-100k.sh"
}

main "$@"
