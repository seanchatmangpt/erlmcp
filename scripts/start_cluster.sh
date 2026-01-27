#!/bin/bash
################################################################################
# Erlang Cluster Startup Script - 4-node erlmcp cluster
#
# Starts a 4-node erlmcp cluster configured for 100K concurrent connections
# - Node 1: erlmcp1@localhost, ports 9001 (TCP), 8080 (HTTP)
# - Node 2: erlmcp2@localhost, ports 9002 (TCP), 8081 (HTTP)
# - Node 3: erlmcp3@localhost, ports 9003 (TCP), 8082 (HTTP)
# - Node 4: erlmcp4@localhost, ports 9004 (TCP), 8083 (HTTP)
#
# Usage:
#   ./scripts/start_cluster.sh [START|STOP|STATUS|TAIL]
#   ./scripts/start_cluster.sh start              # Start all 4 nodes
#   ./scripts/start_cluster.sh stop               # Stop all nodes
#   ./scripts/start_cluster.sh status             # Show node status
#   ./scripts/start_cluster.sh tail [NODE]        # Tail logs for node (default: all)
################################################################################

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BUILD_DIR="${PROJECT_ROOT}/_build/default"
LOG_DIR="${PROJECT_ROOT}/logs"
PID_DIR="/tmp/erlmcp"

# Erlang VM configuration
ERL_ARGS="+K true +A 256"  # Kernel polling + 256 async threads
VM_MEMORY="-pz 8192"       # 8GB process pool

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Logging
log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[✓]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[!]${NC} $1"; }
log_error() { echo -e "${RED}[✗]${NC} $1"; }

################################################################################
# NODE DEFINITIONS
################################################################################

declare -A NODES=(
    [erlmcp1]="9001:8080:8081"     # TCP:HTTP:SSE ports
    [erlmcp2]="9002:8081:8082"
    [erlmcp3]="9003:8082:8083"
    [erlmcp4]="9004:8083:8084"
)

################################################################################
# FUNCTIONS
################################################################################

ensure_directories() {
    mkdir -p "$LOG_DIR"
    mkdir -p "$PID_DIR"
}

verify_build() {
    if [ ! -d "$BUILD_DIR" ]; then
        log_error "Build directory not found: $BUILD_DIR"
        log_info "Run: cd $PROJECT_ROOT && rebar3 compile"
        exit 1
    fi

    if [ ! -f "$PROJECT_ROOT/config/cluster.config" ]; then
        log_error "Cluster config not found: $PROJECT_ROOT/config/cluster.config"
        exit 1
    fi

    log_success "Build verified"
}

start_node() {
    local node_name=$1
    local ports=${NODES[$node_name]}
    IFS=':' read -r tcp_port http_port sse_port <<< "$ports"

    log_info "Starting $node_name (TCP:$tcp_port, HTTP:$http_port, SSE:$sse_port)..."

    local log_file="$LOG_DIR/${node_name}.log"
    local erl_cookie="erlmcp_cluster_secret_2025"

    # Check if port is already in use
    if timeout 1 bash -c "echo >/dev/tcp/127.0.0.1/$tcp_port" 2>/dev/null; then
        log_warn "Port $tcp_port already in use (node may be running)"
        return 1
    fi

    # Start Erlang node with cluster config
    # Using erl with distributed Erlang mode
    erl \
        -setcookie "$erl_cookie" \
        -sname "$node_name" \
        -config "$PROJECT_ROOT/config/cluster" \
        -pa "$BUILD_DIR/lib/erlmcp/ebin" \
        -pa "$BUILD_DIR/lib/gproc/ebin" \
        -pa "$BUILD_DIR/lib/jsx/ebin" \
        -pa "$BUILD_DIR/lib/jesse/ebin" \
        -pa "$BUILD_DIR/lib/gun/ebin" \
        -pa "$BUILD_DIR/lib/ranch/ebin" \
        -pa "$BUILD_DIR/lib/poolboy/ebin" \
        -pa "$BUILD_DIR/lib/bbmustache/ebin" \
        -pa "$BUILD_DIR/lib/cowboy/ebin" \
        -pa "$BUILD_DIR/lib/opentelemetry_api/ebin" \
        -pa "$BUILD_DIR/lib/opentelemetry/ebin" \
        -pa "$BUILD_DIR/lib/opentelemetry_exporter/ebin" \
        -pa "$BUILD_DIR/lib/jobs/ebin" \
        -pa "$BUILD_DIR/lib/fs/ebin" \
        $ERL_ARGS \
        -kernel inet_default_listen_backlog 2048 \
        -erlmcp tcp_port "$tcp_port" http_port "$http_port" sse_port "$sse_port" \
        >> "$log_file" 2>&1 &

    local pid=$!
    echo $pid > "$PID_DIR/${node_name}.pid"

    # Wait for node to start
    sleep 2

    # Verify startup
    if timeout 3 bash -c "echo >/dev/tcp/127.0.0.1/$tcp_port" 2>/dev/null; then
        log_success "$node_name started (PID: $pid, Log: $log_file)"
        return 0
    else
        log_error "$node_name failed to start (check log: $log_file)"
        return 1
    fi
}

stop_node() {
    local node_name=$1
    local pid_file="$PID_DIR/${node_name}.pid"

    if [ -f "$pid_file" ]; then
        local pid=$(cat "$pid_file")
        log_info "Stopping $node_name (PID: $pid)..."

        if kill -0 "$pid" 2>/dev/null; then
            kill -TERM "$pid"
            sleep 1

            # Force kill if still running
            if kill -0 "$pid" 2>/dev/null; then
                log_warn "Force killing $node_name..."
                kill -9 "$pid"
            fi

            rm -f "$pid_file"
            log_success "$node_name stopped"
        else
            log_warn "$node_name not running"
            rm -f "$pid_file"
        fi
    else
        log_warn "PID file not found for $node_name"
    fi
}

show_status() {
    echo ""
    echo -e "${BLUE}=== erlmcp Cluster Status ===${NC}"
    echo ""

    for node_name in erlmcp1 erlmcp2 erlmcp3 erlmcp4; do
        local pid_file="$PID_DIR/${node_name}.pid"
        local ports=${NODES[$node_name]}
        IFS=':' read -r tcp_port http_port sse_port <<< "$ports"

        echo -n "  $node_name: "

        if timeout 2 bash -c "echo >/dev/tcp/127.0.0.1/$tcp_port" 2>/dev/null; then
            echo -e "${GREEN}UP${NC} (TCP:$tcp_port)"
        else
            echo -e "${RED}DOWN${NC}"
        fi
    done

    echo ""
    echo "  Load Balancer:"
    if timeout 2 bash -c "echo >/dev/tcp/127.0.0.1/8000" 2>/dev/null; then
        echo -e "    HAProxy/Nginx: ${GREEN}RUNNING${NC} (port 8000)"
    else
        echo -e "    HAProxy/Nginx: ${RED}NOT RUNNING${NC}"
        echo "      Start with: ./scripts/start_load_balancer.sh haproxy"
    fi

    echo ""
    echo "  Quick Start:"
    echo "    1. Start load balancer: ./scripts/start_load_balancer.sh haproxy"
    echo "    2. Run load test:       ./scripts/load_test_100k.sh 100000 60"
    echo "    3. Monitor health:      ./scripts/health_check.sh --continuous 5"
    echo "    4. View HAProxy stats:  http://127.0.0.1:8404/stats"
    echo ""
}

tail_logs() {
    local node_filter="${1:-}"

    if [ -z "$node_filter" ]; then
        # Tail all logs
        log_info "Tailing all cluster logs (Ctrl+C to stop)..."
        tail -f "$LOG_DIR"/erlmcp*.log
    else
        # Tail specific node log
        local log_file="$LOG_DIR/${node_filter}.log"
        if [ -f "$log_file" ]; then
            log_info "Tailing $node_filter (Ctrl+C to stop)..."
            tail -f "$log_file"
        else
            log_error "Log file not found: $log_file"
        fi
    fi
}

show_help() {
    cat << EOF
Usage: $0 [COMMAND]

Commands:
  start       Start all 4 erlmcp nodes
  stop        Stop all erlmcp nodes
  status      Show cluster status
  tail [NODE] Tail logs (NODE: erlmcp1/2/3/4 or all)
  help        Show this help

Examples:
  $0 start                # Start cluster
  $0 status               # Show status
  $0 tail erlmcp1         # Tail erlmcp1 logs
  $0 tail                 # Tail all logs

Configuration:
  Cluster Config: $PROJECT_ROOT/config/cluster.config
  Build Dir:      $BUILD_DIR
  Log Dir:        $LOG_DIR
  PID Dir:        $PID_DIR

Node Ports:
  erlmcp1: TCP 9001, HTTP 8080, SSE 8081
  erlmcp2: TCP 9002, HTTP 8081, SSE 8082
  erlmcp3: TCP 9003, HTTP 8082, SSE 8083
  erlmcp4: TCP 9004, HTTP 8083, SSE 8084

Full Workflow:
  1. $0 start
  2. ./scripts/start_load_balancer.sh haproxy
  3. ./scripts/load_test_100k.sh 100000 60
  4. ./scripts/health_check.sh --continuous 5

See docs/load-balancer-setup.md for complete documentation.
EOF
}

################################################################################
# MAIN
################################################################################

CMD="${1:-start}"

ensure_directories
verify_build

case "$CMD" in
    start)
        log_info "Starting erlmcp cluster (4 nodes)..."
        echo ""

        for node_name in erlmcp1 erlmcp2 erlmcp3 erlmcp4; do
            start_node "$node_name" || log_warn "Failed to start $node_name"
            sleep 1
        done

        echo ""
        log_success "Cluster startup complete"
        show_status
        ;;

    stop)
        log_info "Stopping erlmcp cluster..."
        echo ""

        for node_name in erlmcp4 erlmcp3 erlmcp2 erlmcp1; do
            stop_node "$node_name"
            sleep 1
        done

        echo ""
        log_success "Cluster stopped"
        ;;

    status)
        show_status
        ;;

    tail)
        tail_logs "$2"
        ;;

    help|--help|-h)
        show_help
        ;;

    *)
        log_error "Unknown command: $CMD"
        echo ""
        show_help
        exit 1
        ;;
esac
