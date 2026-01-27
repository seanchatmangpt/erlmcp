#!/bin/bash
# Real-time monitoring dashboard for Docker Swarm services
# Shows service status, performance metrics, and alerts

set -e

SWARM_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PROMETHEUS_URL=${PROMETHEUS_URL:-"http://localhost:9091"}

clear_screen() {
    clear || printf '\033[2J\033[H'
}

print_header() {
    echo "╔════════════════════════════════════════════════════════════════════════════╗"
    echo "║                   Docker Swarm Monitoring Dashboard                        ║"
    echo "╚════════════════════════════════════════════════════════════════════════════╝"
    echo "Timestamp: $(date '+%Y-%m-%d %H:%M:%S')"
    echo ""
}

show_service_status() {
    echo "┌─ SERVICE STATUS ─────────────────────────────────────────────────────────┐"

    local erlmcp_replicas=$(docker service ls --filter "name=erlmcp-swarm_erlmcp-server" --format "{{.Replicas}}" 2>/dev/null || echo "0/0")
    local client_replicas=$(docker service ls --filter "name=erlmcp-swarm_mcp-client" --format "{{.Replicas}}" 2>/dev/null || echo "0/0")

    echo "│ ErlMCP Servers:       $erlmcp_replicas"
    echo "│ MCP Clients:          $client_replicas"
    echo "│ Load Balancer:        $(docker service ls --filter "name=traefik" --format "{{.Replicas}}" 2>/dev/null || echo "0/0")"
    echo "│ Prometheus:           $(docker service ls --filter "name=prometheus" --format "{{.Replicas}}" 2>/dev/null || echo "0/0")"
    echo "│ Grafana:              $(docker service ls --filter "name=grafana" --format "{{.Replicas}}" 2>/dev/null || echo "0/0")"

    echo "└────────────────────────────────────────────────────────────────────────────┘"
}

show_performance_metrics() {
    echo ""
    echo "┌─ PERFORMANCE METRICS ────────────────────────────────────────────────────┐"

    # Request rate
    local req_rate=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=rate(mcp_client_requests_total[1m])" 2>/dev/null | \
        python3 -c "import json,sys; d=json.load(sys.stdin); print(d['data']['result'][0]['value'][1] if d['data']['result'] else 'N/A')" 2>/dev/null || echo "N/A")

    # Active connections
    local active_conn=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=mcp_client_connections_active" 2>/dev/null | \
        python3 -c "import json,sys; d=json.load(sys.stdin); print(d['data']['result'][0]['value'][1] if d['data']['result'] else 'N/A')" 2>/dev/null || echo "N/A")

    # P50 latency
    local p50_latency=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=histogram_quantile(0.5,rate(mcp_client_request_duration_ms_bucket[1m]))" 2>/dev/null | \
        python3 -c "import json,sys; d=json.load(sys.stdin); print(d['data']['result'][0]['value'][1] if d['data']['result'] else 'N/A')" 2>/dev/null || echo "N/A")

    # P95 latency
    local p95_latency=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=histogram_quantile(0.95,rate(mcp_client_request_duration_ms_bucket[1m]))" 2>/dev/null | \
        python3 -c "import json,sys; d=json.load(sys.stdin); print(d['data']['result'][0]['value'][1] if d['data']['result'] else 'N/A')" 2>/dev/null || echo "N/A")

    # Error rate
    local error_rate=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=rate(mcp_client_messages_errors_total[1m])" 2>/dev/null | \
        python3 -c "import json,sys; d=json.load(sys.stdin); print(d['data']['result'][0]['value'][1] if d['data']['result'] else 'N/A')" 2>/dev/null || echo "N/A")

    printf "│ Request Rate:        %10s req/sec                                  │\n" "$req_rate"
    printf "│ Active Connections:  %10s                                        │\n" "$active_conn"
    printf "│ P50 Latency:         %10s ms                                    │\n" "$p50_latency"
    printf "│ P95 Latency:         %10s ms                                    │\n" "$p95_latency"
    printf "│ Error Rate:          %10s errors/sec                             │\n" "$error_rate"

    echo "└────────────────────────────────────────────────────────────────────────────┘"
}

show_resource_usage() {
    echo ""
    echo "┌─ RESOURCE USAGE ─────────────────────────────────────────────────────────┐"

    # CPU usage
    local cpu_usage=$(docker stats --no-stream --format "table {{.CPUPerc}}" 2>/dev/null | tail -1 | tr -d ' %' || echo "N/A")

    # Memory usage
    local mem_usage=$(docker stats --no-stream --format "table {{.MemUsage}}" 2>/dev/null | tail -1 || echo "N/A")

    echo "│ Total CPU Usage:      $cpu_usage%"
    echo "│ Total Memory:         $mem_usage"

    echo "└────────────────────────────────────────────────────────────────────────────┘"
}

show_alerts() {
    echo ""
    echo "┌─ ACTIVE ALERTS ──────────────────────────────────────────────────────────┐"

    # Check for high error rate
    local error_rate=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=rate(mcp_client_messages_errors_total[5m])" 2>/dev/null | \
        python3 -c "import json,sys; d=json.load(sys.stdin); val=float(d['data']['result'][0]['value'][1] if d['data']['result'] else 0); print(1 if val > 0.01 else 0)" 2>/dev/null || echo "0")

    if [ "$error_rate" -eq 1 ]; then
        echo "│ ⚠ WARNING: High error rate detected (>1%)"
    fi

    # Check for high latency
    local p95=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=histogram_quantile(0.95,rate(mcp_client_request_duration_ms_bucket[5m]))" 2>/dev/null | \
        python3 -c "import json,sys; d=json.load(sys.stdin); val=float(d['data']['result'][0]['value'][1] if d['data']['result'] else 0); print(1 if val > 500 else 0)" 2>/dev/null || echo "0")

    if [ "$p95" -eq 1 ]; then
        echo "│ ⚠ WARNING: P95 latency exceeds 500ms"
    fi

    # Check service health
    local down_services=$(docker service ls --filter "mode=replicated" --format "{{.Replicas}}" 2>/dev/null | \
        grep -c "0/[1-9]" || echo "0")

    if [ "$down_services" -gt 0 ]; then
        echo "│ 🔴 CRITICAL: $down_services service(s) down"
    fi

    if [ "$error_rate" -eq 0 ] && [ "$p95" -eq 0 ] && [ "$down_services" -eq 0 ]; then
        echo "│ ✓ All systems nominal"
    fi

    echo "└────────────────────────────────────────────────────────────────────────────┘"
}

show_endpoints() {
    echo ""
    echo "┌─ ENDPOINTS ──────────────────────────────────────────────────────────────┐"
    echo "│ Grafana:    http://localhost:3000"
    echo "│ Prometheus: http://localhost:9091"
    echo "│ Traefik:    http://localhost:8081"
    echo "│ MCP Server: ws://localhost:5555/mcp"
    echo "└────────────────────────────────────────────────────────────────────────────┘"
}

show_help() {
    echo ""
    echo "Commands:"
    echo "  q - Quit"
    echo "  r - Refresh"
    echo "  l - View logs"
    echo "  s - Service details"
}

main() {
    while true; do
        clear_screen
        print_header
        show_service_status
        show_performance_metrics
        show_resource_usage
        show_alerts
        show_endpoints
        show_help

        echo ""
        echo -n "Refreshing in 5 seconds (press 'q' to quit)... "

        read -t 5 cmd || cmd=""

        case "$cmd" in
            q) break ;;
            l) docker service logs erlmcp-swarm_erlmcp-server -f ;;
            s) docker service ps erlmcp-swarm_erlmcp-server ;;
        esac
    done
}

main "$@"
