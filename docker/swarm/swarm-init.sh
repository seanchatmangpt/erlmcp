#!/bin/bash

# Docker Swarm Initialization Script for erlmcp v3
# This script initializes a Docker Swarm cluster with proper networking and secrets

set -euo pipefail

# Configuration
SWARM_NAME="erlmcp-f500-$(date +%Y%m%d-%H%M%S)"
DOMAIN="example.com"
REGION="${1:-us-west-2}"
AZ_COUNT=3
NODES_PER_AZ=3
TOTAL_NODES=$((AZ_COUNT * NODES_PER_AZ))

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() {
    echo -e "${GREEN}[$(date '+%Y-%m-%d %H:%M:%S')] $1${NC}"
}

warn() {
    echo -e "${YELLOW}[WARN] $1${NC}"
}

error() {
    echo -e "${RED}[ERROR] $1${NC}"
}

# Initialize Swarm
log "Initializing Docker Swarm..."
docker swarm init --advertise-addr $(curl -s http://169.254.169.254/latest/meta-data/local-ipv4) \
    --task-history-limit 100 \
    --auto-accept managers \
    --listen-addr 0.0.0.0:2377 || {
    error "Failed to initialize swarm"
    exit 1
}

# Set Swarm name
log "Setting Swarm name to: $SWARM_NAME"
docker swarm update --label-add swarm.name=$SWARM_NAME

# Create overlay networks
log "Creating overlay networks..."
for network in frontend backend mgmt mesh monitoring logging storage security; do
    docker network create --driver overlay \
        --subnet=10.0.$network.0/24 \
        --opt encrypted \
        erlmcp-$network || {
        error "Failed to create network: erlmcp-$network"
        exit 1
    }
done

# Generate and add secrets
log "Creating secrets..."
openssl rand -base64 32 > erlmcp-db-password.txt
openssl rand -base64 32 > erlmcp-redis-password.txt
openssl rand -base64 64 > erlmcp-api-key.txt
openssl rand -base64 64 > erlmcp-jwt-secret.txt
openssl rand -base64 32 > erlmcp-session-secret.txt
openssl rand -base64 32 > grafana_password.txt
openssl rand -base64 32 > minio_root_password.txt

for secret in erlmcp-db-password erlmcp-redis-password erlmcp-api-key erlmcp-jwt-secret erlmcp-session-secret grafana_password minio_root_password; do
    docker secret create $secret $secret.txt
done

# Add node labels for placement
log "Labeling nodes for service placement..."
docker node update --label-add erlmcp-role=core $(docker node ls -q | head -1)
docker node update --label-add erlmcp-role=ingress $(docker node ls -q | head -2)
docker node update --label-add erlmcp-role=monitoring $(docker node ls -q | head -3)
docker node update --label-add erlmcp-role=logging $(docker node ls -q | head -4)
docker node update --label-add erlmcp-role=security $(docker node ls -q | head -5)
docker node update --label-add erlmcp-role=storage $(docker node ls -q | head -6)
docker node update --label-add erlmcp-role=data $(docker node ls -q | head -7)
docker node update --label-add erlmcp-role=mesh $(docker node ls -q | head -8)
docker node update --label-add erlmcp-role=server $(docker node ls -q | head -9)
docker node update --label-add erlmcp-role=session $(docker node ls -q | head -10)

# Add zone labels for spread scheduling
nodes=($(docker node ls -q))
for i in "${!nodes[@]}"; do
    zone=$(( (i % AZ_COUNT) + 1 ))
    docker node update --label-add zone=zone${zone} ${nodes[$i]}
done

# Deploy stack
log "Deploying erlmcp stack..."
export COMPOSE_PROJECT_NAME=erlmcp
export SWARM_NAME=$SWARM_NAME
docker stack deploy -c docker-compose.swarm.yml erlmcp

# Wait for services to be healthy
log "Waiting for services to be healthy..."
sleep 30

# Check service health
for service in erlmcp-core erlmcp-server erlmcp-session redis postgres; do
    log "Checking service: $service"
    docker service ps $service --filter "desired-state=running" --format "table {{.Name}}\t{{.Image}}\t{{.CurrentState}}" || {
        warn "Service $service not running properly"
    }
done

# Initialize database
log "Initializing PostgreSQL database..."
sleep 60
docker exec $(docker ps -f "name=postgres" -q | head -1) psql -U erlmcp -d erlmcp -c "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";" || {
    warn "Failed to create PostgreSQL extension"
}

# Load dashboard config
log "Loading Grafana dashboards..."
sleep 30
for dashboard in grafana/dashboards/*.json; do
    if [ -f "$dashboard" ]; then
        curl -X POST -H "Content-Type: application/json" -d @"$dashboard" \
            http://admin:${GRAFANA_PASSWORD}@grafana.localhost:3000/api/dashboards/db
    fi
done

# Initialize Service Mesh
log "Initializing Service Mesh..."
docker exec $(docker ps -f "name=envoy" -q | head -1) wget -qO - http://localhost:15000/config_dump

# Initialize Monitoring
log "Initializing Monitoring..."
curl -X POST http://localhost:9090/-/reload

# Print cluster status
log "=== Cluster Status ==="
docker node ls
echo
docker stack services erlmcp
echo
docker service ls

# Print access URLs
log "=== Access URLs ==="
echo "Main API: https://erlmcp-core.$SWARM_NAME.$DOMAIN"
echo "Server API: https://erlmcp-server.$SWARM_NAME.$DOMAIN"
echo "Traefik Dashboard: https://traefik.$SWARM_NAME.$DOMAIN"
echo "Prometheus: https://prometheus.$SWARM_NAME.$DOMAIN"
echo "Grafana: https://grafana.$SWARM_NAME.$DOMAIN"
echo "Loki: https://loki.$SWARM_NAME.$DOMAIN"
echo "Jaeger: https://jaeger.$SWARM_NAME.$DOMAIN"
echo "MinIO Console: https://minio.$SWARM_NAME.$DOMAIN"
echo "Vault: https://vault.$SWARM_NAME.$DOMAIN"

# Cleanup sensitive files
log "Cleaning up..."
rm -f *.txt

log "Swarm initialization complete!"