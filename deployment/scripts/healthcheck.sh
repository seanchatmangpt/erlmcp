#!/bin/sh
# Health Check Script for erlmcp v3
# This script performs comprehensive health checks for the production deployment

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

# Configuration
HEALTH_FILE="/var/lib/erlmcp/health.json"
LOG_FILE="/var/log/erlmcp/healthcheck.log"
PID_FILE="/var/run/erlmcp.pid"
MAX_RESPONSE_TIME=5000  # 5 seconds
MAX_MEMORY_USAGE=80     # 80%
MAX_CPU_USAGE=90        # 90%

# Health status codes
HEALTH_UNKNOWN=0
HEALTH_PASS=1
HEALTH_WARN=2
HEALTH_FAIL=3

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

print_status() {
    case $1 in
        $HEALTH_PASS) echo -e "${GREEN}✓${NC} $2" ;;
        $HEALTH_WARN) echo -e "${YELLOW}⚠${NC} $2" ;;
        $HEALTH_FAIL) echo -e "${RED}✗${NC} $2" ;;
        *) echo -e "${BLUE}?${NC} $2" ;;
    esac
}

check_process() {
    if [ -f "$PID_FILE" ]; then
        local pid=$(cat "$PID_FILE")
        if kill -0 "$pid" 2>/dev/null; then
            log "Process $pid is running"
            return 0
        else
            log "Process $pid is not running"
            return 1
        fi
    else
        log "PID file not found: $PID_FILE"
        return 1
    fi
}

check_port() {
    local port=$1
    if netstat -lntp 2>/dev/null | grep -q ":$port "; then
        log "Port $port is listening"
        return 0
    else
        log "Port $port is not listening"
        return 1
    fi
}

check_http_endpoint() {
    local url=$1
    local timeout=${2:-5}

    if command -v curl >/dev/null 2>&1; then
        if curl -sSf --max-time "$timeout" "$url" >/dev/null 2>&1; then
            log "HTTP endpoint $url is healthy"
            return 0
        else
            log "HTTP endpoint $url is unhealthy"
            return 1
        fi
    else
        log "curl not available, skipping HTTP check for $url"
        return 0
    fi
}

check_memory_usage() {
    if [ -f /proc/meminfo ] && [ -f /proc/$$/statm ]; then
        local total_mem=$(grep MemTotal /proc/meminfo | awk '{print $2}')
        local free_mem=$(grep MemAvailable /proc/meminfo | awk '{print $2}')
        local used_mem=$((total_mem - free_mem))
        local usage_percent=$((used_mem * 100 / total_mem))

        if [ "$usage_percent" -gt "$MAX_MEMORY_USAGE" ]; then
            log "Memory usage critical: ${usage_percent}% > ${MAX_MEMORY_USAGE}%"
            return 1
        elif [ "$usage_percent" -gt 70 ]; then
            log "Memory usage high: ${usage_percent}%"
            return 2
        else
            log "Memory usage normal: ${usage_percent}%"
            return 0
        fi
    else
        log "Memory information not available"
        return 0
    fi
}

check_cpu_usage() {
    if command -v top >/dev/null 2>&1; then
        # Get CPU usage over 1 second
        local cpu_usage=$(top -bn1 | grep "Cpu(s)" | sed "s/.*, *\([0-9.]*\)%* id.*/\1/" | awk '{print 100 - $1}')
        cpu_usage=$(printf "%.0f" "$cpu_usage")

        if [ "$cpu_usage" -gt "$MAX_CPU_USAGE" ]; then
            log "CPU usage critical: ${cpu_usage}% > ${MAX_CPU_USAGE}%"
            return 1
        elif [ "$cpu_usage" -gt 70 ]; then
            log "CPU usage high: ${cpu_usage}%"
            return 2
        else
            log "CPU usage normal: ${cpu_usage}%"
            return 0
        fi
    else
        log "CPU monitoring not available"
        return 0
    fi
}

check_disk_space() {
    local threshold=85  # 85%

    # Check disk usage on /var/lib/erlmcp
    if [ -d /var/lib/erlmcp ]; then
        local usage=$(df /var/lib/erlmcp | tail -1 | awk '{print $5}' | sed 's/%//')
        if [ "$usage" -gt "$threshold" ]; then
            log "Disk space critical: ${usage}% > ${threshold}%"
            return 1
        elif [ "$usage" -gt 70 ]; then
            log "Disk space high: ${usage}%"
            return 2
        else
            log "Disk space normal: ${usage}%"
            return 0
        fi
    fi

    return 0
}

check_mcp_service() {
    # Check MCP-specific health endpoints
    local health_url="http://localhost:8080/health"
    local metrics_url="http://localhost:8080/metrics"

    # Check main health endpoint
    if check_http_endpoint "$health_url" 3; then
        log "MCP health endpoint is responding"
    else
        log "MCP health endpoint is not responding"
        return 1
    fi

    # Check metrics endpoint
    if check_http_endpoint "$metrics_url" 3; then
        log "MCP metrics endpoint is responding"
    else
        log "MCP metrics endpoint is not responding"
        return 2
    fi

    return 0
}

generate_health_report() {
    local status_file="$HEALTH_FILE"
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

    # Create JSON health report
    cat > "$status_file" << EOF
{
  "timestamp": "$timestamp",
  "hostname": "$(hostname)",
  "status": "$1",
  "checks": {
    "process": "$2",
    "port_8080": "$3",
    "port_9100": "$4",
    "memory": "$5",
    "cpu": "$6",
    "disk": "$7",
    "mcp_service": "$8"
  },
  "details": {
    "max_response_time_ms": $MAX_RESPONSE_TIME,
    "max_memory_usage_percent": $MAX_MEMORY_USAGE,
    "max_cpu_usage_percent": $MAX_CPU_USAGE
  }
}
EOF

    log "Health report generated: $status_file"
}

# Main health check execution
main() {
    log "Starting health check"

    # Initialize all checks to unknown
    local process_status=$HEALTH_UNKNOWN
    local port_8080_status=$HEALTH_UNKNOWN
    local port_9100_status=$HEALTH_UNKNOWN
    local memory_status=$HEALTH_UNKNOWN
    local cpu_status=$HEALTH_UNKNOWN
    local disk_status=$HEALTH_UNKNOWN
    local mcp_status=$HEALTH_UNKNOWN
    local overall_status=$HEALTH_PASS

    # Check if process is running
    if check_process; then
        process_status=$HEALTH_PASS
    else
        process_status=$HEALTH_FAIL
        overall_status=$HEALTH_FAIL
    fi

    # Check ports
    if check_port 8080; then
        port_8080_status=$HEALTH_PASS
    else
        port_8080_status=$HEALTH_FAIL
        overall_status=$HEALTH_FAIL
    fi

    if check_port 9100; then
        port_9100_status=$HEALTH_PASS
    else
        port_9100_status=$HEALTH_FAIL
        overall_status=$HEALTH_FAIL
    fi

    # Check system resources
    if check_memory_usage; then
        memory_status=$HEALTH_PASS
    elif [ $? -eq 1 ]; then
        memory_status=$HEALTH_FAIL
        overall_status=$HEALTH_FAIL
    else
        memory_status=$HEALTH_WARN
    fi

    if check_cpu_usage; then
        cpu_status=$HEALTH_PASS
    elif [ $? -eq 1 ]; then
        cpu_status=$HEALTH_FAIL
        overall_status=$HEALTH_FAIL
    else
        cpu_status=$HEALTH_WARN
    fi

    check_disk_space
    disk_status=$?
    if [ $disk_status -eq 1 ]; then
        overall_status=$HEALTH_FAIL
    elif [ $disk_status -eq 2 ]; then
        if [ $overall_status -eq $HEALTH_PASS ]; then
            overall_status=$HEALTH_WARN
        fi
    fi

    # Check MCP service endpoints
    if check_mcp_service; then
        mcp_status=$HEALTH_PASS
    else
        mcp_status=$HEALTH_FAIL
        overall_status=$HEALTH_FAIL
    fi

    # Display results
    echo ""
    echo -e "${BOLD}════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}                ERLMCP v3 HEALTH CHECK REPORT              ${NC}"
    echo -e "${BOLD}════════════════════════════════════════════════════════════${NC}"
    echo ""

    print_status "$process_status" "Process Status"
    print_status "$port_8080_status" "Port 8080 (HTTP)"
    print_status "$port_9100_status" "Port 9100 (MCP)"
    print_status "$memory_status" "Memory Usage"
    print_status "$cpu_status" "CPU Usage"
    print_status "$disk_status" "Disk Space"
    print_status "$mcp_status" "MCP Service"
    echo ""

    # Overall status
    case $overall_status in
        $HEALTH_PASS)
            echo -e "${BOLD}${GREEN}════════════════════════════════════════════════════════════${NC}"
            echo -e "${BOLD}${GREEN}                  ALL SYSTEMS NOMINAL                        ${NC}"
            echo -e "${BOLD}${GREEN}════════════════════════════════════════════════════════════${NC}"
            ;;
        $HEALTH_WARN)
            echo -e "${BOLD}${YELLOW}════════════════════════════════════════════════════════════${NC}"
            echo -e "${BOLD}${YELLOW}                 SOME ISSUES DETECTED                       ${NC}"
            echo -e "${BOLD}${YELLOW}════════════════════════════════════════════════════════════${NC}"
            ;;
        $HEALTH_FAIL)
            echo -e "${BOLD}${RED}════════════════════════════════════════════════════════════${NC}"
            echo -e "${BOLD}${RED}                  CRITICAL ISSUES FOUND                     ${NC}"
            echo -e "${BOLD}${RED}════════════════════════════════════════════════════════════${NC}"
            ;;
    esac

    echo ""

    # Generate health report
    generate_health_report "$overall_status" "$process_status" "$port_8080_status" "$port_9100_status" "$memory_status" "$cpu_status" "$disk_status" "$mcp_status"

    # Exit with appropriate code
    case $overall_status in
        $HEALTH_PASS) exit 0 ;;
        $HEALTH_WARN) exit 1 ;;
        $HEALTH_FAIL) exit 2 ;;
        *) exit 3 ;;
    esac
}

# Execute main function
main "$@"