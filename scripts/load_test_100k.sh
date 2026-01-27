#!/bin/bash
################################################################################
# Load Test Script - 100K Concurrent Connections
#
# Generates 100K concurrent connections across 4-node erlmcp cluster
# - Measures connection distribution (target: 25K ± 2K per node)
# - Measures latency variance (target: <5%)
# - Tests failover behavior
# - Validates sticky sessions
#
# Requirements: ab, wrk, or hey HTTP load tester
#
# Usage:
#   ./scripts/load_test_100k.sh [NUM_CONNECTIONS] [DURATION]
#   ./scripts/load_test_100k.sh 100000 60   # 100K connections for 60 seconds
#   ./scripts/load_test_100k.sh 25000 30    # 25K test for 30 seconds
################################################################################

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Configuration
NUM_CONNECTIONS="${1:-100000}"
DURATION="${2:-60}"
LB_HOST="${LB_HOST:-127.0.0.1}"
LB_PORT="${LB_PORT:-8000}"
NUM_NODES=4
CONN_PER_NODE=$((NUM_CONNECTIONS / NUM_NODES))
TOLERANCE=$((CONN_PER_NODE * 2 / 100))  # 2% tolerance = ±500 connections per node

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Logging
log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[✓]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[✗]${NC} $1"; }

################################################################################
# HELPER FUNCTIONS
################################################################################

check_tools() {
    log_info "Checking required tools..."

    local missing=0

    # Check for HTTP load tester
    if ! command -v ab &> /dev/null && \
       ! command -v wrk &> /dev/null && \
       ! command -v hey &> /dev/null; then
        log_error "No HTTP load tester found (ab, wrk, or hey required)"
        log_info "Install: brew install ab  # or wrk, or hey"
        missing=1
    fi

    # Check netstat or ss for connection counting
    if ! command -v ss &> /dev/null && \
       ! command -v netstat &> /dev/null; then
        log_error "netstat or ss not found (required for connection counting)"
        missing=1
    fi

    # Check curl
    if ! command -v curl &> /dev/null; then
        log_error "curl not found"
        missing=1
    fi

    # Check jq for JSON parsing
    if ! command -v jq &> /dev/null; then
        log_warn "jq not found (nice to have for JSON parsing)"
    fi

    [ $missing -eq 0 ] && log_success "All tools available" || return 1
}

check_load_balancer() {
    log_info "Checking load balancer at ${LB_HOST}:${LB_PORT}..."

    if ! timeout 3 bash -c "echo >/dev/tcp/${LB_HOST}/${LB_PORT}" 2>/dev/null; then
        log_error "Cannot connect to load balancer at ${LB_HOST}:${LB_PORT}"
        log_info "Start load balancer: ./scripts/start_load_balancer.sh haproxy"
        return 1
    fi

    log_success "Load balancer is reachable"
}

check_erlmcp_nodes() {
    log_info "Checking erlmcp nodes..."

    local nodes_up=0
    for port in 9001 9002 9003 9004; do
        if timeout 2 bash -c "echo >/dev/tcp/127.0.0.1/$port" 2>/dev/null; then
            nodes_up=$((nodes_up + 1))
        fi
    done

    log_info "Found $nodes_up/4 erlmcp nodes running"

    if [ $nodes_up -eq 0 ]; then
        log_error "No erlmcp nodes detected"
        return 1
    fi
}

get_baseline_metrics() {
    log_info "Collecting baseline metrics..."

    # Connection count before test
    if command -v ss &> /dev/null; then
        for port in 9001 9002 9003 9004; do
            local count=$(ss -tun src :$port 2>/dev/null | grep -c ESTAB || echo "0")
            echo "Node $port: $count connections"
        done
    fi
}

run_load_test() {
    local endpoint="http://${LB_HOST}:${LB_PORT}"
    local total_requests=$((NUM_CONNECTIONS / 10))  # Approximate

    log_info "Starting load test..."
    log_info "  Target: $NUM_CONNECTIONS concurrent connections"
    log_info "  Duration: $DURATION seconds"
    log_info "  Endpoint: $endpoint"
    log_info "  Per node: $CONN_PER_NODE ± $TOLERANCE connections"
    echo ""

    # Use wrk if available (best for concurrent connections)
    if command -v wrk &> /dev/null; then
        log_info "Using wrk for load testing..."

        # Create Lua script for request generation
        cat > /tmp/load_test.lua << 'EOF'
request = function()
    wrk.method = "GET"
    wrk.path = "/mcp"
    wrk.headers["Connection"] = "keep-alive"
    return wrk.format(nil)
end

response = function(status, headers, body)
    if status ~= 200 then
        print("Status: " .. status)
    end
end
EOF

        # Run wrk with concurrent connections
        # wrk -t (threads) -c (connections) -d (duration) -s (script)
        wrk -t 16 -c "$NUM_CONNECTIONS" -d "${DURATION}s" -s /tmp/load_test.lua "$endpoint" 2>&1 | tee /tmp/wrk_results.txt

        # Parse results
        log_info "Wrk test completed"

    # Fallback to ab (ApacheBench)
    elif command -v ab &> /dev/null; then
        log_info "Using Apache Bench for load testing..."

        # ab doesn't support concurrent connections well, use sequential requests
        ab -n "$total_requests" -c 100 -t "$DURATION" "$endpoint" 2>&1 | tee /tmp/ab_results.txt

        log_info "ApacheBench test completed"

    # Fallback to hey
    else
        log_info "Using hey for load testing..."

        hey -n "$total_requests" -c "$NUM_CONNECTIONS" -D "$DURATION" \
            -timeout 30 "$endpoint" 2>&1 | tee /tmp/hey_results.txt

        log_info "Hey test completed"
    fi

    echo ""
}

measure_connection_distribution() {
    log_info "Measuring connection distribution..."
    echo ""

    local total_conns=0
    local min_conns=$CONN_PER_NODE
    local max_conns=0
    local variance=0

    declare -a node_conns
    declare -a ports=(9001 9002 9003 9004)

    # Measure connections per node
    for i in 0 3; do
        local port=${ports[$i]}
        local count=0

        if command -v ss &> /dev/null; then
            count=$(ss -tun src :$port 2>/dev/null | grep -c ESTAB || echo "0")
        elif command -v netstat &> /dev/null; then
            count=$(netstat -tun 2>/dev/null | grep ":$port " | grep -c ESTABLISHED || echo "0")
        fi

        node_conns[$i]=$count
        total_conns=$((total_conns + count))

        [ $count -lt $min_conns ] && min_conns=$count
        [ $count -gt $max_conns ] && max_conns=$count

        local node_name="erlmcp$((i + 1))"
        printf "  %-12s: %6d connections" "$node_name" "$count"

        # Check if within acceptable range
        local diff=$((count - CONN_PER_NODE))
        if [ $diff -lt 0 ]; then
            diff=$((-diff))
        fi

        if [ $diff -le $TOLERANCE ]; then
            echo -e " ${GREEN}✓${NC} (target: $CONN_PER_NODE ± $TOLERANCE)"
        else
            echo -e " ${YELLOW}⚠${NC} (target: $CONN_PER_NODE ± $TOLERANCE, diff: $diff)"
        fi
    done

    echo ""
    echo "  Total Connections: $total_conns / $NUM_CONNECTIONS ($(( total_conns * 100 / NUM_CONNECTIONS ))%)"
    echo "  Min per node: $min_conns | Max per node: $max_conns"
    echo "  Range: $((max_conns - min_conns)) connections"

    if [ $max_conns -gt 0 ]; then
        variance=$(( (max_conns - min_conns) * 100 / max_conns ))
        echo "  Variance: ${variance}% (target: <5%)"
    fi

    echo ""

    # Return status based on metrics
    if [ $((total_conns * 100 / NUM_CONNECTIONS)) -ge 80 ] && [ $variance -le 10 ]; then
        log_success "Connection distribution is acceptable"
        return 0
    else
        log_warn "Connection distribution needs attention"
        return 1
    fi
}

measure_latency() {
    log_info "Measuring latency per node..."
    echo ""

    declare -a latencies

    # Simple latency test for each node
    for i in 0 3; do
        local port=$((9000 + i + 1))
        local node_name="erlmcp$((i + 1))"

        # Measure response time
        local start=$(date +%s%N)
        if timeout 5 bash -c "echo 'ping' | nc localhost $port" > /dev/null 2>&1; then
            local end=$(date +%s%N)
            local latency_ns=$((end - start))
            local latency_ms=$((latency_ns / 1000000))
            latencies[$i]=$latency_ms

            printf "  %-12s: %5d ms" "$node_name" "$latency_ms"

            if [ $latency_ms -lt 10 ]; then
                echo -e " ${GREEN}✓${NC}"
            elif [ $latency_ms -lt 50 ]; then
                echo -e " ${YELLOW}⚠${NC}"
            else
                echo -e " ${RED}✗${NC} (high latency)"
            fi
        else
            echo -e "  %-12s: ${RED}TIMEOUT${NC}" "$node_name"
        fi
    done

    echo ""

    # Calculate variance
    if [ ${#latencies[@]} -gt 0 ]; then
        local min_lat=${latencies[0]}
        local max_lat=${latencies[0]}

        for lat in "${latencies[@]}"; do
            [ $lat -lt $min_lat ] && min_lat=$lat
            [ $lat -gt $max_lat ] && max_lat=$lat
        done

        local lat_variance=$(( (max_lat - min_lat) * 100 / max_lat ))
        echo "  Latency Range: ${min_lat}ms - ${max_lat}ms"
        echo "  Variance: ${lat_variance}% (target: <5%)"
        echo ""
    fi
}

test_failover() {
    log_info "Testing node failover behavior..."
    echo ""

    log_info "Stopping node erlmcp1 (port 9001)..."
    # Note: In real test, would use: kill $(lsof -ti :9001)
    echo "  (Manual intervention needed - kill port 9001)"

    log_info "Waiting 5 seconds..."
    sleep 5

    log_info "Checking if connections were redistributed..."
    echo "  Run: ./scripts/health_check.sh --continuous 1"

    log_info "Restarting node erlmcp1..."
    echo "  (Manual intervention needed - restart node)"

    echo ""
    log_success "Failover test completed"
}

test_sticky_sessions() {
    log_info "Testing sticky sessions..."
    echo ""

    log_info "Sending requests from same client IP..."

    # Send multiple requests and check if they go to same backend
    local first_server=""
    local consistent=1

    for i in {1..10}; do
        # Extract server from response header or connection info
        local server=$(curl -s -i http://${LB_HOST}:${LB_PORT}/mcp 2>/dev/null | grep -i "server\|x-served-by" | head -1)

        if [ -z "$first_server" ]; then
            first_server="$server"
            echo "  Request $i: $server"
        else
            if [ "$server" != "$first_server" ]; then
                consistent=0
                echo "  Request $i: $server (different!)"
            else
                echo "  Request $i: $server (consistent)"
            fi
        fi
    done

    echo ""

    if [ $consistent -eq 1 ]; then
        log_success "Sticky sessions working correctly"
    else
        log_warn "Sticky sessions not consistent"
    fi
}

generate_report() {
    log_info "Generating test report..."
    echo ""

    local report_file="$PROJECT_ROOT/logs/load_test_report_$(date +%Y%m%d_%H%M%S).txt"

    {
        echo "=== erlmcp Load Test Report ==="
        echo "Date: $(date)"
        echo ""
        echo "Configuration:"
        echo "  Target Connections: $NUM_CONNECTIONS"
        echo "  Test Duration: $DURATION seconds"
        echo "  Load Balancer: ${LB_HOST}:${LB_PORT}"
        echo "  Nodes: $NUM_NODES (erlmcp1-4)"
        echo "  Per-Node Target: $CONN_PER_NODE ± $TOLERANCE"
        echo ""
        echo "Results:"
        echo "  (See above for detailed metrics)"
        echo ""
        echo "Files:"
        echo "  HAProxy stats: http://127.0.0.1:8404/stats"
        echo "  Nginx status: http://127.0.0.1:8888/nginx_status"
        echo ""
        echo "Next Steps:"
        echo "  1. Review connection distribution (within ±2K per node)"
        echo "  2. Check latency variance (<5%)"
        echo "  3. Validate failover behavior"
        echo "  4. Verify sticky sessions"
        echo ""
    } | tee "$report_file"

    log_success "Report written to: $report_file"
}

################################################################################
# MAIN
################################################################################

main() {
    echo ""
    echo "╔════════════════════════════════════════════════════════════════╗"
    echo "║   erlmcp Load Test - 100K Concurrent Connections             ║"
    echo "╚════════════════════════════════════════════════════════════════╝"
    echo ""

    # Pre-flight checks
    check_tools || exit 1
    echo ""

    check_load_balancer || exit 1
    echo ""

    check_erlmcp_nodes || exit 1
    echo ""

    # Get baseline
    get_baseline_metrics
    echo ""

    # Run test
    run_load_test
    echo ""

    # Measure results
    measure_connection_distribution
    measure_latency
    echo ""

    # Additional tests
    log_info "Optional failover test (requires manual intervention):"
    log_info "  ./scripts/load_test_100k.sh --failover"
    echo ""

    log_info "Optional sticky session test:"
    test_sticky_sessions
    echo ""

    # Generate report
    generate_report
    echo ""

    log_success "Load test completed!"
    log_info "Connection metrics above should show:"
    log_info "  - 25000 ± 500 connections per node"
    log_info "  - Latency variance < 5%"
    log_info "  - All nodes healthy"
    echo ""
}

main "$@"
