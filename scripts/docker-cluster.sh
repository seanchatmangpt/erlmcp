#!/bin/bash

###############################################################################
# ERLMCP DOCKER CLUSTER STARTUP SCRIPT
###############################################################################
# Starts a 4-node erlmcp cluster in Docker containers
# Each node handles ~25,000 concurrent connections
#
# Usage:
#   ./scripts/docker-cluster.sh [start|stop|restart|logs|status]
#
# Requirements:
#   - Docker
#   - docker-compose
#
# Network: bridge network named 'erlmcp-cluster' with fixed IPs:
#   erlmcp1: 172.25.0.2
#   erlmcp2: 172.25.0.3
#   erlmcp3: 172.25.0.4
#   erlmcp4: 172.25.0.5
#   haproxy: 172.25.0.10 (load balancer)
###############################################################################

set -e

# Configuration
DOCKER_COMPOSE_FILE="docker-compose.cluster.yml"
ERLMCP_NODES=4
ERLMCP_COOKIE="erlmcp_cluster"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_debug() {
    echo -e "${BLUE}[DEBUG]${NC} $1"
}

# Create docker-compose file
create_docker_compose() {
    log_info "Creating docker-compose.yml for cluster..."

    cat > "$DOCKER_COMPOSE_FILE" << 'EOF'
version: '3.8'

services:
  erlmcp1:
    build:
      context: .
      dockerfile: Dockerfile.cluster
      args:
        NODE_NUM: 1
    container_name: erlmcp1
    hostname: erlmcp1
    networks:
      erlmcp-cluster:
        ipv4_address: 172.25.0.2
    environment:
      - NODE_NAME=erlmcp1
      - NODE_NUM=1
      - ERLMCP_NODES=erlmcp1,erlmcp2,erlmcp3,erlmcp4
      - ERLMCP_COOKIE=erlmcp_cluster
      - LOG_LEVEL=info
    ports:
      - "9101:4369"    # EPMD
      - "9201:4370"    # Node 1
    volumes:
      - ./logs/cluster:/app/logs
      - ./data/cluster1:/app/data
    depends_on:
      - erlmcp-network
    restart: unless-stopped

  erlmcp2:
    build:
      context: .
      dockerfile: Dockerfile.cluster
      args:
        NODE_NUM: 2
    container_name: erlmcp2
    hostname: erlmcp2
    networks:
      erlmcp-cluster:
        ipv4_address: 172.25.0.3
    environment:
      - NODE_NAME=erlmcp2
      - NODE_NUM=2
      - ERLMCP_NODES=erlmcp1,erlmcp2,erlmcp3,erlmcp4
      - ERLMCP_COOKIE=erlmcp_cluster
      - LOG_LEVEL=info
    ports:
      - "9102:4369"
      - "9202:4370"
    volumes:
      - ./logs/cluster:/app/logs
      - ./data/cluster2:/app/data
    depends_on:
      - erlmcp1
    restart: unless-stopped

  erlmcp3:
    build:
      context: .
      dockerfile: Dockerfile.cluster
      args:
        NODE_NUM: 3
    container_name: erlmcp3
    hostname: erlmcp3
    networks:
      erlmcp-cluster:
        ipv4_address: 172.25.0.4
    environment:
      - NODE_NAME=erlmcp3
      - NODE_NUM=3
      - ERLMCP_NODES=erlmcp1,erlmcp2,erlmcp3,erlmcp4
      - ERLMCP_COOKIE=erlmcp_cluster
      - LOG_LEVEL=info
    ports:
      - "9103:4369"
      - "9203:4370"
    volumes:
      - ./logs/cluster:/app/logs
      - ./data/cluster3:/app/data
    depends_on:
      - erlmcp1
    restart: unless-stopped

  erlmcp4:
    build:
      context: .
      dockerfile: Dockerfile.cluster
      args:
        NODE_NUM: 4
    container_name: erlmcp4
    hostname: erlmcp4
    networks:
      erlmcp-cluster:
        ipv4_address: 172.25.0.5
    environment:
      - NODE_NAME=erlmcp4
      - NODE_NUM=4
      - ERLMCP_NODES=erlmcp1,erlmcp2,erlmcp3,erlmcp4
      - ERLMCP_COOKIE=erlmcp_cluster
      - LOG_LEVEL=info
    ports:
      - "9104:4369"
      - "9204:4370"
    volumes:
      - ./logs/cluster:/app/logs
      - ./data/cluster4:/app/data
    depends_on:
      - erlmcp1
    restart: unless-stopped

  haproxy:
    image: haproxy:2.9-alpine
    container_name: erlmcp-haproxy
    hostname: haproxy
    networks:
      erlmcp-cluster:
        ipv4_address: 172.25.0.10
    ports:
      - "8000:8000"    # HTTP
      - "8443:8443"    # HTTPS
      - "9001:9001"    # stats
    volumes:
      - ./config/haproxy.cfg:/usr/local/etc/haproxy/haproxy.cfg:ro
      - ./logs/cluster:/var/log/haproxy
    depends_on:
      - erlmcp1
      - erlmcp2
      - erlmcp3
      - erlmcp4
    restart: unless-stopped

networks:
  erlmcp-cluster:
    driver: bridge
    ipam:
      config:
        - subnet: 172.25.0.0/16

  erlmcp-network:
    driver: bridge
    name: erlmcp-network
EOF

    log_info "docker-compose.yml created"
}

# Create Dockerfile for cluster nodes
create_dockerfile() {
    log_info "Creating Dockerfile.cluster..."

    cat > "Dockerfile.cluster" << 'EOF'
FROM erlang:27-alpine

ARG NODE_NUM=1

# Install build dependencies
RUN apk add --no-cache \
    git \
    make \
    bash \
    curl \
    jq \
    tcpdump

# Build erlmcp
WORKDIR /app
COPY . .

RUN \
    wget -q https://github.com/erlang/rebar3/releases/download/3.23.0/rebar3 && \
    chmod +x rebar3 && \
    ./rebar3 compile

# Create cluster startup script
RUN cat > /app/bin/start-cluster-node.sh << 'SCRIPT'
#!/bin/bash
set -e

NODE_NAME=${NODE_NAME:-erlmcp${NODE_NUM}}
COOKIE=${ERLMCP_COOKIE:-erlmcp_cluster}

# Build known nodes
KNOWN_NODES=""
for NODE in $(echo $ERLMCP_NODES | tr ',' ' '); do
    if [ -z "$KNOWN_NODES" ]; then
        KNOWN_NODES="'${NODE}@${NODE}'"
    else
        KNOWN_NODES="${KNOWN_NODES},'${NODE}@${NODE}'"
    fi
done

echo "Starting cluster node: $NODE_NAME"
echo "Cookie: $COOKIE"
echo "Known nodes: [$KNOWN_NODES]"

exec erl \
    -setcookie "$COOKIE" \
    -sname "$NODE_NAME" \
    -config /app/config/cluster \
    -args @/app/config/vm-cluster.args \
    -pa /app/_build/default/lib/*/ebin \
    -erlmcp cluster enabled true \
    -erlmcp cluster known_nodes "[$KNOWN_NODES]" \
    -erlmcp cluster node_role "$([ $NODE_NUM -eq 1 ] && echo 'primary' || echo 'replica')" \
    -noshell \
    -eval "
        application:ensure_all_started(erlmcp),
        io:format('Cluster node ~w started~n', ['$NODE_NAME']),
        timer:sleep(infinity)
    "
SCRIPT

chmod +x /app/bin/start-cluster-node.sh

# Create data and log directories
RUN mkdir -p /app/logs /app/data

EXPOSE 4369 4370 8000 8443 9001

CMD ["/app/bin/start-cluster-node.sh"]
EOF

    log_info "Dockerfile.cluster created"
}

# Create HAProxy configuration
create_haproxy_config() {
    log_info "Creating HAProxy configuration..."

    mkdir -p config

    cat > "config/haproxy.cfg" << 'EOF'
global
    log stdout local0
    log stdout local1 notice
    maxconn 100000
    daemon

    # Performance tuning
    tune.maxconn 100000
    tune.ssl.default-dh-param 2048
    tune.ssl.num-threads 4

defaults
    log     global
    mode    http
    option  httplog
    option  dontlognull
    timeout connect 5000
    timeout client  50000
    timeout server  50000

    # Enable compression
    compression algo gzip
    compression type text/html text/plain text/css text/javascript application/javascript application/json

# Frontend for HTTP
frontend erlmcp_http
    mode http
    bind *:8000
    maxconn 100000

    # Rate limiting
    stick-table type ip size 100k expire 30s store http_req_rate(10s)
    http-request track-sc0 src
    http-request deny if { sc_http_req_rate(0) gt 1000 }

    default_backend erlmcp_servers

# Frontend for WebSocket
frontend erlmcp_ws
    mode http
    bind *:8000

    acl is_websocket hdr(Upgrade) -i WebSocket
    use_backend erlmcp_ws_servers if is_websocket
    default_backend erlmcp_servers

# Backend for HTTP
backend erlmcp_servers
    mode http
    balance roundrobin
    option httpchk GET /health

    # Connection pooling
    http-reuse safe

    # Sticky sessions (keep client on same node)
    cookie SERVERID insert indirect nocache

    server node1 erlmcp1:4370 check cookie node1 weight 100 maxconn 25000
    server node2 erlmcp2:4370 check cookie node2 weight 100 maxconn 25000
    server node3 erlmcp3:4370 check cookie node3 weight 100 maxconn 25000
    server node4 erlmcp4:4370 check cookie node4 weight 100 maxconn 25000

# Backend for WebSocket
backend erlmcp_ws_servers
    mode http
    balance roundrobin
    option http-server-close

    server node1 erlmcp1:4370 check weight 100 maxconn 25000
    server node2 erlmcp2:4370 check weight 100 maxconn 25000
    server node3 erlmcp3:4370 check weight 100 maxconn 25000
    server node4 erlmcp4:4370 check weight 100 maxconn 25000

# Stats interface
listen stats
    mode http
    bind *:9001
    stats enable
    stats uri /stats
    stats refresh 10s
    stats show-legends
    stats show-node
EOF

    log_info "HAProxy configuration created"
}

# Start cluster
start_cluster() {
    ensure_build
    create_docker_compose
    create_dockerfile
    create_haproxy_config

    log_info "Starting Docker cluster..."
    docker-compose -f "$DOCKER_COMPOSE_FILE" up -d

    log_info "Waiting for nodes to start..."
    sleep 10

    # Verify cluster
    verify_cluster_docker
}

# Stop cluster
stop_cluster() {
    log_info "Stopping Docker cluster..."
    docker-compose -f "$DOCKER_COMPOSE_FILE" down --volumes 2>/dev/null || true
    log_info "Cluster stopped"
}

# Restart cluster
restart_cluster() {
    stop_cluster
    sleep 2
    start_cluster
}

# Show logs
show_logs() {
    docker-compose -f "$DOCKER_COMPOSE_FILE" logs -f
}

# Show status
show_status() {
    log_info "Cluster Status:"
    docker-compose -f "$DOCKER_COMPOSE_FILE" ps

    log_info ""
    log_info "HAProxy stats: http://localhost:9001/stats"
    log_info "Load Balancer: http://localhost:8000"
}

# Verify cluster health
verify_cluster_docker() {
    log_info "Verifying cluster health..."

    # Check if nodes are responding
    for i in {1..4}; do
        container="erlmcp$i"
        if docker ps | grep -q "$container"; then
            log_info "Node $container: running"
        else
            log_error "Node $container: not running"
        fi
    done

    log_info "HAProxy: $(docker ps | grep haproxy | wc -l) container(s)"
}

# Ensure build exists
ensure_build() {
    if [ ! -d "_build/default/lib/erlmcp" ]; then
        log_info "Building erlmcp..."
        rebar3 compile
    fi
}

# Main
case "${1:-start}" in
    start)
        start_cluster
        show_status
        ;;
    stop)
        stop_cluster
        ;;
    restart)
        restart_cluster
        show_status
        ;;
    logs)
        show_logs
        ;;
    status)
        show_status
        ;;
    *)
        echo "Usage: $0 {start|stop|restart|logs|status}"
        exit 1
        ;;
esac
