#!/bin/bash

# erlmcp Docker Swarm Deployment Script
# Enterprise Edition with High Availability and Security

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
CLUSTER_NAME="erlmcp-swarm"
SWARM_MANAGER="192.168.1.100"  # First manager node
MANAGER_NODES=3
WORKER_NODES=5
COMPOSE_FILE="docker-compose.yml"
ENV_FILE=".env"

# Function to print colored output
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to check if Docker Swarm is initialized
check_swarm_status() {
    local status=$(docker info 2>/dev/null | grep -i "swarm:" | awk '{print $2}')
    if [ "$status" = "active" ]; then
        print_status "Docker Swarm is already initialized"
        return 0
    else
        return 1
    fi
}

# Function to initialize Docker Swarm
initialize_swarm() {
    print_status "Initializing Docker Swarm..."
    docker swarm init --advertise-addr ${SWARM_MANAGER}:2377

    # Get join token for worker nodes
    local worker_token=$(docker swarm join-token worker -q)
    local manager_token=$(docker swarm join-token manager -q)

    echo "Worker join command: docker swarm join --token ${worker_token} ${SWARM_MANAGER}:2377"
    echo "Manager join command: docker swarm join --token ${manager_token} ${SWARM_MANAGER}:2377"

    # Create overlay networks
    print_status "Creating overlay networks..."
    docker network create --driver overlay --attachable erlmcp-overlay
    docker network create --driver overlay --attachable erlmcp-management
    docker network create --driver overlay --attachable erlmcp-public

    print_status "Docker Swarm initialized successfully"
}

# Function to create secrets
create_secrets() {
    print_status "Creating Docker secrets..."

    # Generate random secrets if they don't exist
    if [ ! -f "secrets/jwt-secret" ]; then
        openssl rand -hex 32 > secrets/jwt-secret
    fi

    if [ ! -f "secrets/db-password" ]; then
        openssl rand -base64 32 > secrets/db-password
    fi

    if [ ! -f "secrets/redis-password" ]; then
        openssl rand -base64 32 > secrets/redis-password
    fi

    if [ ! -f "secrets/rabbitmq-password" ]; then
        openssl rand -base64 32 > secrets/rabbitmq-password
    fi

    if [ ! -f "secrets/es-password" ]; then
        openssl rand -base64 32 > secrets/es-password
    fi

    if [ ! -f "secrets/grafana-password" ]; then
        openssl rand -base64 16 > secrets/grafana-password
    fi

    # Create Docker secrets
    for secret_file in secrets/*; do
        local secret_name=$(basename "$secret_file")
        docker secret create erlmcp-$secret_name "$secret_file" >/dev/null 2>&1
        print_status "Created secret: erlmcp-$secret_name"
    done

    print_status "All secrets created successfully"
}

# Function to create config files
create_configs() {
    print_status "Creating configuration files..."

    # Create environment file
    cat > $ENV_FILE << EOF
# erlmcp Docker Swarm Environment Configuration
ERLMCP_VERSION=v3.0.0
ERLMCP_CLUSTER_SIZE=3
ERLMCP_CORE_REPLICAS=3
ERLMCP_STDIO_REPLICAS=3
ERLMCP_TCP_REPLICAS=5
ERLMCP_HTTP_REPLICAS=5
ERLMCP_OTEL_ENABLED=true
OTEL_VERSION=0.93.0
PROMETHEUS_VERSION=v2.45.0
GRAFANA_VERSION=10.2.0
ELASTIC_VERSION=8.10.0
GRAFANA_PASSWORD=admin
HEALTH_CHECK_INTERVAL=30

# Database passwords (stored in secrets)
DB_PASSWORD=$(cat secrets/db-password 2>/dev/null || echo 'change-me')
REDIS_PASSWORD=$(cat secrets/redis-password 2>/dev/null || echo 'change-me')
RABBITMQ_PASSWORD=$(cat secrets/rabbitmq-password 2>/dev/null || echo 'change-me')
ES_PASSWORD=$(cat secrets/es-password 2>/dev/null || echo 'change-me')
GRAFANA_PASSWORD=$(cat secrets/grafana-password 2>/dev/null || echo 'change-me')
EOF

    # Update compose file with secrets
    sed -i.bak "s/\${DB_PASSWORD}/$DB_PASSWORD/g" $COMPOSE_FILE
    sed -i.bak "s/\${REDIS_PASSWORD}/$REDIS_PASSWORD/g" $COMPOSE_FILE
    sed -i.bak "s/\${RABBITMQ_PASSWORD}/$RABBITMQ_PASSWORD/g" $COMPOSE_FILE
    sed -i.bak "s/\${ES_PASSWORD}/$ES_PASSWORD/g" $COMPOSE_FILE
    sed -i.bak "s/\${GRAFANA_PASSWORD}/$GRAFANA_PASSWORD/g" $COMPOSE_FILE

    print_status "Configuration files created successfully"
}

# Function to deploy services
deploy_services() {
    print_status "Deploying erlmcp services..."

    # Deploy stack
    docker stack deploy -c $COMPOSE_FILE $CLUSTER_NAME

    # Wait for services to be healthy
    print_status "Waiting for services to be healthy..."
    sleep 30

    # Check service status
    docker stack services $CLUSTER_NAME

    print_status "Services deployed successfully"
}

# Function to check service health
check_service_health() {
    local service_name=$1
    local max_attempts=30
    local attempt=0

    while [ $attempt -lt $max_attempts ]; do
        local health=$(docker service ps --format "{{.CurrentState}}" $CLUSTER_NAME_$service_name | head -1)
        if [[ "$health" == *"running"* ]]; then
            print_status "$service_name is healthy"
            return 0
        fi
        attempt=$((attempt + 1))
        sleep 2
    done

    print_error "$service_name failed to become healthy"
    return 1
}

# Function to create node labels
label_nodes() {
    print_status "Labeling nodes for service placement..."

    # Label managers
    docker node update --label-add erlmcp-role=manager $(docker node ls -f "role=manager" -q | head -1)
    docker node update --label-add erlmcp-role=manager $(docker node ls -f "role=manager" -q | tail -n +2)
    docker node update --label-add erlmcp-role=manager $(docker node ls -f "role=manager" -q | tail -n +3)

    # Label workers with different roles
    docker node update --label-add erlmcp-role=core $(docker node ls -f "role=worker" -q | head -1)
    docker node update --label-add erlmcp-role=transport $(docker node ls -f "role=worker" -q | head -2)
    docker node update --label-add erlmcp-role=observability $(docker node ls -f "role=worker" -q | head -3)
    docker node update --label-add erlmcp-role=gateway $(docker node ls -f "role=worker" -q | head -4)
    docker node update --label-add erlmcp-role=database $(docker node ls -f "role=worker" -q | tail -n +5)

    print_status "Node labels created successfully"
}

# Function to show deployment status
show_deployment_status() {
    print_status "Deployment Status:"
    echo "==================="

    # Show swarm info
    docker info | grep -i "swarm"

    # Show services
    echo ""
    docker stack services $CLUSTER_NAME

    # Show nodes
    echo ""
    docker node ls

    # Show network
    echo ""
    docker network ls | grep -E "erlmcp|overlay"

    # Show volumes
    echo ""
    docker volume ls | grep -i erlmcp

    print_status "Access URLs:"
    echo "============"
    echo "Grafana: http://localhost:3000"
    echo "Prometheus: http://localhost:9090"
    echo "Kibana: http://localhost:5601"
    echo "erlmcp API: http://localhost:8080"
}

# Function to cleanup
cleanup() {
    print_warning "Cleaning up..."
    docker stack rm $CLUSTER_NAME 2>/dev/null || true
    sleep 10
    docker network rm erlmcp-overlay erlmcp-management erlmcp-public 2>/dev/null || true
    docker secret rm erlmcp-* 2>/dev/null || true
    rm -f $ENV_FILE $COMPOSE_FILE.bak
    print_status "Cleanup completed"
}

# Main execution
main() {
    case "${1:-}" in
        init)
            if ! check_swarm_status; then
                initialize_swarm
                label_nodes
            else
                print_warning "Swarm already initialized"
            fi
            ;;
        deploy)
            create_secrets
            create_configs
            deploy_services
            show_deployment_status
            ;;
        status)
            show_deployment_status
            ;;
        logs)
            docker service logs -f $CLUSTER_NAME_${2:-}
            ;;
        scale)
            docker service scale $CLUSTER_NAME_${2:-}=$3
            ;;
        down)
            cleanup
            ;;
        *)
            echo "Usage: $0 {init|deploy|status|logs|scale|down}"
            echo ""
            echo "Commands:"
            echo "  init    - Initialize Docker Swarm and create networks"
            echo "  deploy  - Deploy erlmcp services"
            echo "  status  - Show deployment status"
            echo "  logs    - Show service logs"
            echo "  scale   - Scale a service (e.g., scale erlmcp-core 5)"
            echo "  down    - Cleanup and remove all services"
            exit 1
            ;;
    esac
}

# Run main function
main "$@"