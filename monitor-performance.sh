#!/bin/bash
# monitor-performance.sh - Monitor erlmcp production performance
set -euo pipefail

# Configuration
METRICS_URL="${METRICS_URL:-http://localhost:8080/metrics}"
HEALTH_URL="${HEALTH_URL:-http://localhost:8080/health}"
ALERT_CPU_THRESHOLD="${ALERT_CPU_THRESHOLD:-80}"
ALERT_MEMORY_THRESHOLD="${ALERT_MEMORY_THRESHOLD:-80}"
ALERT_RESPONSE_TIME="${ALERT_RESPONSE_TIME:-1.0}"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo "📊 erlmcp Performance Monitor"
echo "============================"

# Check if erlmcp is running
if ! curl -f "$HEALTH_URL" >/dev/null 2>&1; then
    echo -e "${RED}❌ erlmcp is not responding${NC}"
    exit 1
fi

# Check Docker stats if running in container
if command -v docker >/dev/null 2>&1; then
    echo "📈 Container Resource Usage:"
    docker stats erlmcp --no-stream --format "table {{.Container}}\t{{.CPUPerc}}\t{{.MemUsage}}"
fi

# Check response time
echo "⏱️  Response Time:"
RESPONSE_TIME=$(curl -o /dev/null -s -w '%{time_total}' "$HEALTH_URL" 2>/dev/null || echo "0")
printf "Average response time: %.3fs\n" "$RESPONSE_TIME"

# Check error rate
echo "🚨 Error Rate:"
if curl -s "$METRICS_URL" >/dev/null 2>&1; then
    ERROR_COUNT=$(curl -s "$METRICS_URL" | grep 'http_requests_total{status=~"5.."}' | tail -1 | awk '{print $2}' || echo "0")
    TOTAL_REQUESTS=$(curl -s "$METRICS_URL" | grep 'http_requests_total{status=~"2.."}' | tail -1 | awk '{print $2}' || echo "0")

    if [ "$TOTAL_REQUESTS" -gt 0 ]; then
        ERROR_RATE=$(echo "scale=4; $ERROR_COUNT / $TOTAL_REQUESTS * 100" | bc -l)
        printf "Error rate: %.4f%%\n" "$ERROR_RATE"
    else
        echo "No requests recorded yet"
    fi
else
    echo "Metrics endpoint not available"
fi

# Check memory usage
echo "💾 Memory Usage:"
MEMORY_INFO=$(docker stats erlmcp --no-stream --format "{{.MemUsage}}" 2>/dev/null || echo "N/A")
echo "Memory: $MEMORY_INFO"

# Check active connections
echo "🔗 Active Connections:"
if curl -s "$METRICS_URL" >/dev/null 2>&1; then
    ACTIVE_CONNECTIONS=$(curl -s "$METRICS_URL" | grep 'active_connections_total' | tail -1 | awk '{print $2}' || echo "0")
    echo "Active connections: $ACTIVE_CONNECTIONS"
else
    echo "Connection metrics not available"
fi

# Performance alerts
echo "🚨 Performance Alerts:"
CPU_USAGE=$(docker stats erlmcp --no-stream --format "{{.CPUPerc}}" 2>/dev/null | sed 's/%//' || echo "0")
MEMORY_PERCENT=$(docker stats erlmcp --no-stream --format "{{.MemPerc}}" 2>/dev/null | sed 's/%//' || echo "0")

# Convert to numbers
CPU_NUM=$(echo "$CPU_USAGE" | sed 's/%//' | awk '{printf "%.0f", $1}')
MEM_NUM=$(echo "$MEMORY_PERCENT" | sed 's/%//' | awk '{printf "%.0f", $1}')

if [ "$CPU_NUM" -gt "$ALERT_CPU_THRESHOLD" ]; then
    echo -e "${RED}⚠️  CPU usage is ${CPU_NUM}% (threshold: ${ALERT_CPU_THRESHOLD}%)${NC}"
fi

if [ "$MEM_NUM" -gt "$ALERT_MEMORY_THRESHOLD" ]; then
    echo -e "${RED}⚠️  Memory usage is ${MEM_NUM}% (threshold: ${ALERT_MEMORY_THRESHOLD}%)${NC}"
fi

if (( $(echo "$RESPONSE_TIME > $ALERT_RESPONSE_TIME" | bc -l) )); then
    echo -e "${RED}⚠️  Response time is ${RESPONSE_TIME}s (threshold: ${ALERT_RESPONSE_TIME}s)${NC}"
fi

echo ""
echo "✅ Performance check complete"
echo "Monitor every 5 minutes: ./monitor-performance.sh