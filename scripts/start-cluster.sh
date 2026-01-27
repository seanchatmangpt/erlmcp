#!/bin/bash

###############################################################################
# ERLMCP CLUSTER STARTUP SCRIPT
###############################################################################
# Starts a 4-node erlmcp cluster for testing 100K concurrent connections
# Each node handles ~25,000 concurrent connections
#
# Usage:
#   ./scripts/start-cluster.sh [start|stop|restart]
#
# Environment:
#   ERLMCP_PORT_BASE: Starting TCP port (default: 9100)
#   ERLMCP_NODES: Number of nodes to start (default: 4)
#   ERLMCP_BUILD: Build directory (default: _build/default)
###############################################################################

set -e

# Configuration
ERLMCP_NODES=${ERLMCP_NODES:-4}
ERLMCP_PORT_BASE=${ERLMCP_PORT_BASE:-9100}
ERLMCP_BUILD=${ERLMCP_BUILD:-_build/default}
ERLMCP_COOKIE="erlmcp_cluster"
LOG_DIR="${PWD}/logs/cluster"
CLUSTER_PIDS="${LOG_DIR}/cluster.pids"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

# Ensure directories exist
ensure_dirs() {
    mkdir -p "$LOG_DIR"
    mkdir -p data/cluster
}

# Build if not already built
ensure_build() {
    if [ ! -d "$ERLMCP_BUILD/lib/erlmcp" ]; then
        log_info "Building erlmcp..."
        rebar3 compile
    fi
}

# Start a single node
start_node() {
    local node_num=$1
    local node_name="erlmcp${node_num}"
    local node_port=$((ERLMCP_PORT_BASE + node_num))
    local log_file="${LOG_DIR}/${node_name}.log"

    log_info "Starting node ${node_name} on port ${node_port}..."

    # Create ERL_LIBS with compiled beam files
    export ERL_LIBS="${ERLMCP_BUILD}/lib"

    # Build known nodes list for clustering
    local known_nodes=""
    for i in $(seq 1 $ERLMCP_NODES); do
        if [ $i -ne 1 ]; then
            known_nodes="${known_nodes},"
        fi
        known_nodes="${known_nodes}'erlmcp${i}@127.0.0.1'"
    done

    # Start node with distributed config
    erl \
        -setcookie "$ERLMCP_COOKIE" \
        -sname "$node_name" \
        -config "$(pwd)/config/cluster" \
        -args @"$(pwd)/config/vm-cluster.args" \
        -pa "${ERLMCP_BUILD}/lib/*/ebin" \
        -erlmcp cluster enabled true \
        -erlmcp cluster known_nodes "[$known_nodes]" \
        -erlmcp cluster node_role "$([ $node_num -eq 1 ] && echo 'primary' || echo 'replica')" \
        -noshell \
        -eval "
            application:ensure_all_started(erlmcp),
            io:format('Node ~w started~n', ['$node_name']),
            timer:sleep(infinity)
        " \
        >> "$log_file" 2>&1 &

    local pid=$!
    echo "$pid" >> "$CLUSTER_PIDS"
    log_info "Node ${node_name} started with PID ${pid} (log: ${log_file})"
}

# Start all nodes
start_cluster() {
    ensure_dirs
    ensure_build

    log_info "Starting ${ERLMCP_NODES}-node erlmcp cluster..."
    log_info "Cookie: ${ERLMCP_COOKIE}"
    log_info "Starting port: ${ERLMCP_PORT_BASE}"

    # Clear old PID file
    > "$CLUSTER_PIDS"

    # Start each node
    for i in $(seq 1 $ERLMCP_NODES); do
        start_node $i
        sleep 2  # Stagger startup for proper clustering
    done

    log_info "Cluster startup initiated. Check logs in ${LOG_DIR}/"
    log_info "Waiting for cluster to stabilize..."
    sleep 5

    # Verify nodes are up
    verify_cluster
}

# Verify cluster health
verify_cluster() {
    log_info "Verifying cluster connectivity..."

    # Try to connect to first node and check cluster status
    erl \
        -setcookie "$ERLMCP_COOKIE" \
        -sname cluster_monitor \
        -noshell \
        -eval "
            net_adm:ping('erlmcp1@127.0.0.1'),
            case nodes() of
                [] ->
                    io:format('ERROR: No nodes in cluster~n', []);
                Nodes ->
                    io:format('Cluster nodes: ~p~n', [Nodes]),
                    lists:foreach(fun(N) ->
                        case net_adm:ping(N) of
                            pong -> io:format('  ~w: OK~n', [N]);
                            pang -> io:format('  ~w: FAILED~n', [N])
                        end
                    end, Nodes)
            end,
            halt(0)
        " 2>&1 || true
}

# Stop all nodes
stop_cluster() {
    log_info "Stopping erlmcp cluster..."

    if [ -f "$CLUSTER_PIDS" ]; then
        while IFS= read -r pid; do
            if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then
                log_info "Stopping process $pid..."
                kill -15 "$pid" 2>/dev/null || true
                # Wait for graceful shutdown
                sleep 1
                kill -9 "$pid" 2>/dev/null || true
            fi
        done < "$CLUSTER_PIDS"
        rm -f "$CLUSTER_PIDS"
    fi

    log_info "Cluster stopped"
}

# Restart cluster
restart_cluster() {
    stop_cluster
    sleep 2
    start_cluster
}

# Show cluster status
show_status() {
    log_info "Cluster Status:"
    log_info "Log directory: ${LOG_DIR}"

    if [ -f "$CLUSTER_PIDS" ]; then
        echo "Running processes:"
        while IFS= read -r pid; do
            if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then
                echo "  PID $pid: running"
            else
                echo "  PID $pid: stopped"
            fi
        done < "$CLUSTER_PIDS"
    else
        log_warn "No cluster processes found"
    fi

    # Try to show node details if available
    if ls "${LOG_DIR}"/erlmcp*.log 1> /dev/null 2>&1; then
        echo ""
        echo "Recent log entries:"
        for log in "${LOG_DIR}"/erlmcp*.log; do
            echo "  $(basename $log): $(tail -1 "$log")"
        done
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
    status)
        show_status
        ;;
    *)
        echo "Usage: $0 {start|stop|restart|status}"
        exit 1
        ;;
esac
