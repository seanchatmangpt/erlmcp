#!/bin/bash
################################################################################
# Load Balancer Startup Script - erlmcp
#
# Configures and starts HAProxy or NGINX for distributing 100K connections
# across 4-node erlmcp cluster.
#
# Usage:
#   ./scripts/start_load_balancer.sh haproxy [config_file]
#   ./scripts/start_load_balancer.sh nginx [config_file]
#   ./scripts/start_load_balancer.sh stop
#   ./scripts/start_load_balancer.sh status
################################################################################

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
LB_TYPE="${1:-haproxy}"
CONFIG_FILE="${2:-}"
LOG_DIR="${PROJECT_ROOT}/logs"
PID_DIR="/var/run"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

################################################################################
# FUNCTIONS
################################################################################

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

ensure_directories() {
    mkdir -p "$LOG_DIR"
    mkdir -p "$PID_DIR"
}

validate_erlmcp_nodes() {
    log_info "Validating erlmcp cluster nodes..."

    # Check if at least one node is running
    local nodes_running=0
    for port in 9001 9002 9003 9004 8080 8081 8082 8083; do
        if nc -z localhost $port 2>/dev/null; then
            nodes_running=$((nodes_running + 1))
        fi
    done

    if [ $nodes_running -eq 0 ]; then
        log_warn "No erlmcp nodes detected running. Start nodes before load balancer:"
        echo "  Node 1: erl -sname erlmcp1 -config cluster -pa _build/default/lib/*/ebin"
        echo "  Node 2: erl -sname erlmcp2 -config cluster -pa _build/default/lib/*/ebin"
        echo "  Node 3: erl -sname erlmcp3 -config cluster -pa _build/default/lib/*/ebin"
        echo "  Node 4: erl -sname erlmcp4 -config cluster -pa _build/default/lib/*/ebin"
        return 1
    fi

    log_success "Found $nodes_running erlmcp nodes running"
    return 0
}

start_haproxy() {
    local config="${1:-${PROJECT_ROOT}/config/haproxy.cfg}"

    if [ ! -f "$config" ]; then
        log_error "HAProxy config not found: $config"
        return 1
    fi

    log_info "Starting HAProxy with config: $config"

    # Validate config syntax
    if ! haproxy -f "$config" -c > /dev/null 2>&1; then
        log_error "HAProxy config validation failed"
        haproxy -f "$config" -c
        return 1
    fi

    # Stop existing HAProxy if running
    if pgrep -x haproxy > /dev/null; then
        log_info "Stopping existing HAProxy..."
        pkill -x haproxy || true
        sleep 1
    fi

    # Start HAProxy in daemon mode
    haproxy -f "$config" -D

    # Wait for startup
    sleep 2

    # Verify startup
    if pgrep -x haproxy > /dev/null; then
        log_success "HAProxy started successfully (PID: $(pgrep -x haproxy))"
        echo ""
        echo "HAProxy Configuration:"
        echo "  TCP (MCP):     127.0.0.1:8000  (round-robin, sticky sessions)"
        echo "  HTTP (MCP):    127.0.0.1:8080  (round-robin, cookie-based sticky)"
        echo "  WebSocket:     127.0.0.1:8080/mcp/ws  (IP-based sticky)"
        echo "  SSE:           127.0.0.1:8081/mcp/sse  (IP-based sticky)"
        echo "  Stats:         127.0.0.1:8404/stats"
        echo ""
        echo "Load Distribution (4 nodes × 25K connections):"
        echo "  Node 1 (erlmcp1): localhost:9001 (TCP), localhost:8080 (HTTP)"
        echo "  Node 2 (erlmcp2): localhost:9002 (TCP), localhost:8081 (HTTP)"
        echo "  Node 3 (erlmcp3): localhost:9003 (TCP), localhost:8082 (HTTP)"
        echo "  Node 4 (erlmcp4): localhost:9004 (TCP), localhost:8083 (HTTP)"
        echo ""
        return 0
    else
        log_error "Failed to start HAProxy"
        return 1
    fi
}

start_nginx() {
    local config="${1:-${PROJECT_ROOT}/config/nginx.conf}"

    if [ ! -f "$config" ]; then
        log_error "Nginx config not found: $config"
        return 1
    fi

    log_info "Starting Nginx with config: $config"

    # Validate config syntax
    if ! nginx -t -c "$config" > /dev/null 2>&1; then
        log_error "Nginx config validation failed"
        nginx -t -c "$config"
        return 1
    fi

    # Stop existing Nginx if running
    if pgrep -x nginx > /dev/null; then
        log_info "Stopping existing Nginx..."
        nginx -s quit || pkill -x nginx || true
        sleep 1
    fi

    # Start Nginx
    nginx -c "$config"

    # Wait for startup
    sleep 2

    # Verify startup
    if pgrep -x nginx > /dev/null; then
        log_success "Nginx started successfully (PID: $(pgrep -x nginx))"
        echo ""
        echo "Nginx Configuration:"
        echo "  HTTP (MCP):    127.0.0.1:8000  (least connections, IP hash)"
        echo "  WebSocket:     127.0.0.1:8001/mcp/ws  (long timeout)"
        echo "  SSE:           127.0.0.1:8002/mcp/sse  (long timeout)"
        echo "  Health:        127.0.0.1:8888/health"
        echo "  Status:        127.0.0.1:8888/nginx_status"
        echo ""
        echo "Load Distribution (4 nodes × 25K connections):"
        echo "  Node 1 (erlmcp1): localhost:8080 (HTTP)"
        echo "  Node 2 (erlmcp2): localhost:8081 (HTTP)"
        echo "  Node 3 (erlmcp3): localhost:8082 (HTTP)"
        echo "  Node 4 (erlmcp4): localhost:8083 (HTTP)"
        echo ""
        return 0
    else
        log_error "Failed to start Nginx"
        return 1
    fi
}

stop_load_balancer() {
    log_info "Stopping load balancer..."

    if pgrep -x haproxy > /dev/null; then
        log_info "Stopping HAProxy..."
        pkill -x haproxy
        sleep 1
    fi

    if pgrep -x nginx > /dev/null; then
        log_info "Stopping Nginx..."
        nginx -s quit || pkill -x nginx || true
        sleep 1
    fi

    log_success "Load balancer stopped"
}

show_status() {
    echo ""
    echo "=== Load Balancer Status ==="
    echo ""

    if pgrep -x haproxy > /dev/null; then
        echo -e "${GREEN}HAProxy: RUNNING${NC} (PID: $(pgrep -x haproxy))"
        echo "  Config: ${PROJECT_ROOT}/config/haproxy.cfg"
        echo "  Listening on: 0.0.0.0:8000 (TCP), 0.0.0.0:8080 (HTTP), 0.0.0.0:8443 (HTTPS)"
        echo "  Stats: http://127.0.0.1:8404/stats"
    elif pgrep -x nginx > /dev/null; then
        echo -e "${GREEN}Nginx: RUNNING${NC} (PID: $(pgrep -x nginx))"
        echo "  Config: ${PROJECT_ROOT}/config/nginx.conf"
        echo "  HTTP: 8000, WebSocket: 8001, SSE: 8002, HTTPS: 8443"
    else
        echo -e "${RED}No load balancer running${NC}"
        return 1
    fi

    echo ""
    echo "=== Erlang Cluster Nodes ==="
    echo ""

    for port in 9001 9002 9003 9004 8080 8081 8082 8083; do
        if nc -z localhost $port 2>/dev/null; then
            echo -e "  Port $port: ${GREEN}UP${NC}"
        else
            echo -e "  Port $port: ${RED}DOWN${NC}"
        fi
    done

    echo ""
}

show_help() {
    cat << EOF
Usage: $0 [COMMAND] [OPTIONS]

Commands:
  haproxy [CONFIG]    Start HAProxy load balancer
  nginx [CONFIG]      Start Nginx load balancer
  stop                Stop load balancer (HAProxy or Nginx)
  status              Show load balancer and cluster status
  help                Show this help message

Options:
  CONFIG              Path to custom config file (optional)
                      Default: ./config/haproxy.cfg or ./config/nginx.conf

Examples:
  $0 haproxy                              # Start HAProxy with default config
  $0 nginx config/nginx.conf              # Start Nginx with custom config
  $0 status                               # Show status
  $0 stop                                 # Stop load balancer

Load Balancer Features:
  - 100K concurrent connections (25K per node)
  - Automatic health checks (3 second interval)
  - Sticky sessions (IP hash or cookie-based)
  - Connection rate limiting (100 conn/sec per IP)
  - Automatic failover with connection draining
  - Compression (gzip for HTTP)
  - WebSocket upgrade support
  - Server-Sent Events (SSE) streaming

Endpoints:
  HAProxy:
    TCP:              0.0.0.0:8000 (MCP protocol)
    HTTP:             0.0.0.0:8080 (HTTP/WebSocket/SSE)
    HTTPS:            0.0.0.0:8443 (optional TLS termination)
    Stats:            http://127.0.0.1:8404/stats

  Nginx:
    HTTP:             0.0.0.0:8000 (HTTP/MCP)
    WebSocket:        0.0.0.0:8001 (/mcp/ws)
    SSE:              0.0.0.0:8002 (/mcp/sse)
    Health:           http://127.0.0.1:8888/health
    Status:           http://127.0.0.1:8888/nginx_status

See docs/load-balancer-setup.md for complete documentation.
EOF
}

################################################################################
# MAIN
################################################################################

ensure_directories

case "$LB_TYPE" in
    haproxy)
        validate_erlmcp_nodes || log_warn "Continuing anyway..."
        start_haproxy "$CONFIG_FILE"
        ;;
    nginx)
        validate_erlmcp_nodes || log_warn "Continuing anyway..."
        start_nginx "$CONFIG_FILE"
        ;;
    stop)
        stop_load_balancer
        ;;
    status)
        show_status
        ;;
    help|--help|-h)
        show_help
        ;;
    *)
        log_error "Unknown command: $LB_TYPE"
        echo ""
        show_help
        exit 1
        ;;
esac
