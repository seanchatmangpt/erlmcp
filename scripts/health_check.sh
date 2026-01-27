#!/bin/bash
# Health check script for erlmcp load balancer & cluster
# Usage: ./scripts/health_check.sh [--continuous INTERVAL] [--json]

set -e

# === CONFIGURATION ===
INTERVAL="${1:-}"
JSON_MODE="${2:-}"
NODES=("erlmcp1:9001:8080" "erlmcp2:9002:8081" "erlmcp3:9003:8082" "erlmcp4:9004:8083")
CONTINUOUS=0

# === COLORS ===
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# === LOGGING ===
log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $*"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }

# Parse arguments
if [ "$INTERVAL" == "--continuous" ]; then
    CONTINUOUS=1
    INTERVAL="${2:-5}"
fi

# === NODE HEALTH CHECKS ===
check_tcp_port() {
    local host=$1
    local port=$2
    timeout 3 bash -c "echo >/dev/tcp/$host/$port" 2>/dev/null && return 0 || return 1
}

check_http_health() {
    local url=$1
    local timeout=5
    curl -s -f --max-time $timeout "$url" > /dev/null 2>&1 && return 0 || return 1
}

get_connection_count() {
    local port=$1
    if command -v ss &> /dev/null; then
        ss -tun src :$port 2>/dev/null | grep -c ESTAB || echo "0"
    elif command -v netstat &> /dev/null; then
        netstat -tun 2>/dev/null | grep ":$port " | grep -c ESTABLISHED || echo "0"
    else
        echo "0"
    fi
}

# === MONITORING FUNCTIONS ===
print_header() {
    echo ""
    echo -e "${BLUE}=== erlmcp Load Balancer & Cluster Health ===${NC}"
    echo "Timestamp: $(date '+%Y-%m-%d %H:%M:%S')"
    echo ""
}

check_load_balancer() {
    echo -e "${BLUE}Load Balancer:${NC}"

    if pgrep -x haproxy > /dev/null; then
        echo -e "  HAProxy:  ${GREEN}RUNNING${NC} (PID: $(pgrep -x haproxy))"
        if [ -S /var/run/haproxy.sock ]; then
            echo -e "  Stats:    ${GREEN}AVAILABLE${NC} at http://127.0.0.1:8404/stats"
        fi
    elif pgrep -x nginx > /dev/null; then
        echo -e "  Nginx:    ${GREEN}RUNNING${NC} (PID: $(pgrep -x nginx))"
        if check_tcp_port localhost 8888; then
            echo -e "  Health:   ${GREEN}AVAILABLE${NC} at http://127.0.0.1:8888/health"
        fi
    else
        echo -e "  Status:   ${RED}NOT RUNNING${NC}"
    fi
    echo ""
}

check_nodes() {
    echo -e "${BLUE}Cluster Nodes:${NC}"

    local total_conn=0
    local healthy_nodes=0

    for node_info in "${NODES[@]}"; do
        IFS=':' read -r name tcp_port http_port <<< "$node_info"

        if check_tcp_port localhost $tcp_port; then
            local conn=$(get_connection_count $tcp_port)
            echo -e "  $name: ${GREEN}UP${NC} ($conn connections)"
            total_conn=$((total_conn + conn))
            healthy_nodes=$((healthy_nodes + 1))
        else
            echo -e "  $name: ${RED}DOWN${NC}"
        fi
    done

    echo -e "\n  Total: $healthy_nodes/4 healthy | $total_conn/100000 connections ($(( total_conn * 100 / 100000 ))%)"
    echo ""
}

# === MAIN ===
main() {
    if [ ! -t 0 ]; then
        # Not a terminal, suppress colors
        GREEN=''
        RED=''
        YELLOW=''
        BLUE=''
        NC=''
    fi

    if [ $CONTINUOUS -eq 1 ]; then
        while true; do
            clear
            print_header
            check_load_balancer
            check_nodes
            echo "Refreshing in ${INTERVAL}s... (Press Ctrl+C to stop)"
            sleep "$INTERVAL"
        done
    else
        print_header
        check_load_balancer
        check_nodes
    fi
}

main "$@"
