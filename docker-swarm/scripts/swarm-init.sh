#!/bin/bash
# Docker Swarm initialization script
# This script sets up the Docker Swarm cluster with all required configurations

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
SWARM_STACK_NAME="erlmcp-prod"
OVERLAY_NETWORK="erlmcp-overlay"
MANAGER_NODES=3
WORKER_NODES=5

# Logging function
log() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
    exit 1
}

# Check Docker installation
check_docker() {
    if ! command -v docker &> /dev/null; then
        error "Docker is not installed or not in PATH"
    fi

    if ! docker info &> /dev/null; then
        error "Docker daemon is not running"
    fi

    log "Docker installation verified"
}

# Initialize or join Swarm
init_or_join_swarm() {
    local current_mode=$(docker info | grep -i 'swarm: active' || echo "")

    if [[ -z "$current_mode" ]]; then
        # Current node is not part of a swarm
        local manager_count=$(docker node ls --format '{{.ID}}' | wc -l 2>/dev/null || echo "0")

        if [[ $manager_count -eq 0 ]]; then
            # First manager - initialize swarm
            log "Initializing Docker Swarm..."
            docker swarm init --advertise-addr $(hostname -i) || error "Failed to initialize swarm"
            log "Swarm initialized successfully"
        else
            # Need to join existing swarm
            read -p "Enter swarm manager token (format: SWMTKN-1-...): " SWARM_TOKEN
            if [[ -z "$SWARM_TOKEN" ]]; then
                error "Swarm token is required"
            fi

            read -p "Enter manager IP: " MANAGER_IP
            if [[ -z "$MANAGER_IP" ]]; then
                error "Manager IP is required"
            fi

            log "Joining Docker Swarm..."
            docker swarm join --token $SWARM_TOKEN $MANAGER_IP:2377 || error "Failed to join swarm"
            log "Successfully joined swarm"
        fi
    else
        log "Node is already part of a Docker Swarm"
    fi
}

# Create overlay network
create_overlay_network() {
    log "Creating overlay network: $OVERLAY_NETWORK"
    docker network create --driver overlay --attachable $OVERLAY_NETWORK || error "Failed to create overlay network"
    log "Overlay network created successfully"
}

# Prepare directories
prepare_directories() {
    log "Preparing persistent directories..."

    local directories=(
        "/mnt/erlmcp/data"
        "/mnt/erlmcp/logs"
        "/mnt/erlmcp/backups"
        "/mnt/erlmcp/cache"
        "/mnt/erlmcp/secrets"
    )

    for dir in "${directories[@]}"; do
        mkdir -p "$dir"
        chmod 750 "$dir"
        chown 1001:1001 "$dir"  # erlmcp user
        log "Created directory: $dir"
    done

    log "Directories prepared successfully"
}

# Create Docker secrets
create_secrets() {
    log "Creating Docker secrets..."

    # JWT Secret
    if ! docker secret ls | grep -q erlmcp-jwt-secret; then
        openssl rand -hex 64 | docker secret create erlmcp-jwt-secret -
        log "Created JWT secret"
    else
        log "JWT secret already exists"
    fi

    # Database credentials
    if ! docker secret ls | grep -q erlmcp-db-credentials; then
        echo "{
  \"username\": \"erlmcp_user\",
  \"password\": \"$(openssl rand -base64 32)\"
}" | docker secret create erlmcp-db-credentials -
        log "Created database credentials"
    else
        log "Database credentials already exist"
    fi

    # Redis password
    if ! docker secret ls | grep -q erlmcp-redis-password; then
        openssl rand -base64 32 | docker secret create erlmcp-redis-password -
        log "Created Redis password"
    else
        log "Redis password already exists"
    fi

    # API Keys
    if ! docker secret ls | grep -q erlmcp-api-keys; then
        echo "{
  \"mcp_api_key\": \"sk_$(openssl rand -base64 32)\",
  \"admin_api_key\": \"sk_$(openssl rand -base64 32)\",
  \"metrics_api_key\": \"sk_$(openssl rand -base64 32)\"
}" | docker secret create erlmcp-api-keys -
        log "Created API keys"
    else
        log "API keys already exist"
    fi

    log "Secrets created successfully"
}

# Set up auto-scaling policies
setup_autoscaling() {
    log "Setting up auto-scaling policies..."

    # Create service constraints file
    cat > /tmp/service-constraints.json << EOF
{
  "erlmcp-core": {
    "replicas": 5,
    "min": 3,
    "max": 10,
    "cpu_threshold": 80,
    "memory_threshold": 80,
    "response_time_threshold": 2000
  },
  "redis-cluster": {
    "replicas": 6,
    "min": 3,
    "max": 12,
    "memory_threshold": 85,
    "connection_threshold": 50000
  },
  "kafka-broker": {
    "replicas": 3,
    "min": 2,
    "max": 6,
    "disk_threshold": 85,
    "partition_count": 1000
  }
}
EOF

    # Create autoscaling service
    cat > /tmp/autoscaling-service.yml << EOF
version: '3.8'
services:
  autoscaler:
    image: dockersamples/cadvisor:latest
    container_name: erlmcp-autoscaler
    hostname: autoscaler
    command: >
      sh -c "
        while true; do
          # This is a placeholder for real autoscaling logic
          # In production, use Docker Swarm auto-scaling or Kubernetes HPA
          echo \"Scaling policies defined in /tmp/service-constraints.json\"
          sleep 60
        done
      "
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock:ro
      - /tmp/service-constraints.json:/config/constraints.json:ro
    deploy:
      mode: replicated
      replicas: 1
      placement:
        constraints: [node.role == manager]
      resources:
        limits:
          cpus: '0.5'
          memory: 256M
        reservations:
          cpus: '0.1'
          memory: 64M
    restart:
      condition: unless-stopped
    networks:
      - erlmcp-overlay
EOF

    docker stack deploy -c /tmp/autoscaling-service.yml autoscaler || warn "Failed to deploy autoscaler"
    log "Auto-scaling policies configured"
}

# Set up monitoring and alerting
setup_monitoring() {
    log "Setting up monitoring and alerting..."

    # Create monitoring stack
    docker stack deploy -c docker-swarm/docker-compose.monitoring.yml monitoring

    # Wait for services to be ready
    sleep 30

    # Check if services are running
    local prometheus_running=$(docker service ls --filter name=prometheus --format '{{.Name}}' | wc -l)
    if [[ $prometheus_running -gt 0 ]]; then
        log "Monitoring stack deployed successfully"
    else
        warn "Monitoring stack deployment failed"
    fi
}

# Set up backup and disaster recovery
setup_backup() {
    log "Setting up backup and disaster recovery..."

    # Create backup script
    cat > /opt/erlmcp/backup.sh << 'EOF'
#!/bin/bash
# Backup script for erlmcp

BACKUP_DIR="/mnt/erlmcp/backups"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_NAME="erlmcp_backup_$TIMESTAMP"

mkdir -p "$BACKUP_DIR"

# Backup data volumes
docker run --rm \
  -v erlmcp_data:/data \
  -v erlmcp_logs:/logs \
  -v "$BACKUP_DIR":/backup \
  alpine:latest \
  tar czf "/backup/$BACKUP_NAME/data.tar.gz" -C /data . && \
  tar czf "/backup/$BACKUP_NAME/logs.tar.gz" -C /logs .

# Upload to S3 (if configured)
if [ -n "$AWS_ACCESS_KEY_ID" ] && [ -n "$AWS_SECRET_ACCESS_KEY" ]; then
  aws s3 sync "/backup/$BACKUP_NAME" "s3://$S3_BUCKET/backups/$BACKUP_NAME/"
fi

# Cleanup old backups (keep last 7 days)
find "$BACKUP_DIR" -name "*.tar.gz" -mtime +7 -delete

echo "Backup completed: $BACKUP_NAME"
EOF

    chmod +x /opt/erlmcp/backup.sh

    # Create cron job
    echo "0 2 * * * /opt/erlmcp/backup.sh" | crontab -

    log "Backup and disaster recovery configured"
}

# Main execution
main() {
    log "Starting Docker Swarm initialization..."

    check_docker
    init_or_join_swarm
    create_overlay_network
    prepare_directories
    create_secrets
    setup_autoscaling
    setup_monitoring
    setup_backup

    log "Docker Swarm initialization completed successfully!"
    log "Next steps:"
    log "1. Deploy the erlmcp stack: docker stack deploy -c docker-swarm/docker-compose.prod.yml erlmcp"
    log "2. Access monitoring at: http://<manager-ip>:3000"
    log "3. Access Traefik dashboard at: http://<manager-ip>:8080"
}

# Run main function
main "$@"