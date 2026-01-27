#!/bin/bash
# Connection Flood Test - Stress test with rapid connection establishment
# Tests: connection handling limits, reconnection behavior, recovery time

set -e

SCENARIO_NAME="connection_flood"
LOAD_BALANCER=${LOAD_BALANCER:-"localhost:5555"}
DURATION=${DURATION:-600}
PROMETHEUS_URL=${PROMETHEUS_URL:-"http://localhost:9091"}
RESULTS_DIR=${RESULTS_DIR:-"./test-results"}

mkdir -p "$RESULTS_DIR/$SCENARIO_NAME"

echo "=== CONNECTION FLOOD STRESS TEST ==="
echo "Load Balancer: $LOAD_BALANCER"
echo "Duration: ${DURATION}s"
echo "Start Time: $(date -u +%Y-%m-%dT%H:%M:%SZ)"

# Ramp-up configuration
export CLIENT_TYPE=normal
export TARGET_SERVERS=$LOAD_BALANCER
export MESSAGE_RATE=50
export METRICS_ENABLED=true

START_TIME=$(date +%s)
CURRENT_CLIENTS=0
RAMP_RATE=5  # Add 5 clients every 5 seconds
TARGET_CLIENTS=500

echo -e "\n[Phase 1] Ramping up connections (0 -> $TARGET_CLIENTS)..."

while [ $(( $(date +%s) - START_TIME )) -lt 300 ]; do
    if [ $CURRENT_CLIENTS -lt $TARGET_CLIENTS ]; then
        CURRENT_CLIENTS=$(( CURRENT_CLIENTS + RAMP_RATE ))
        if [ $CURRENT_CLIENTS -gt $TARGET_CLIENTS ]; then
            CURRENT_CLIENTS=$TARGET_CLIENTS
        fi

        export CLIENT_COUNT=$CURRENT_CLIENTS

        echo "Active clients: $CURRENT_CLIENTS"

        # Collect metrics
        curl -s "$PROMETHEUS_URL/api/v1/query" \
            -d "query=mcp_client_connections_active" \
            >> "$RESULTS_DIR/$SCENARIO_NAME/connections.jsonl" 2>/dev/null || true

        curl -s "$PROMETHEUS_URL/api/v1/query" \
            -d "query=rate(mcp_client_messages_errors_total[1m])" \
            >> "$RESULTS_DIR/$SCENARIO_NAME/error_rate.jsonl" 2>/dev/null || true
    fi

    sleep 5
done

echo -e "\n[Phase 2] Steady high load ($TARGET_CLIENTS connections)..."

PEAK_START=$(date +%s)
while [ $(( $(date +%s) - PEAK_START )) -lt 180 ]; do
    # Record peak metrics
    curl -s "$PROMETHEUS_URL/api/v1/query" \
        -d "query=mcp_client_connections_active" \
        >> "$RESULTS_DIR/$SCENARIO_NAME/peak_connections.jsonl" 2>/dev/null || true

    curl -s "$PROMETHEUS_URL/api/v1/query" \
        -d "query=histogram_quantile(0.95, rate(mcp_client_request_duration_ms_bucket[1m]))" \
        >> "$RESULTS_DIR/$SCENARIO_NAME/peak_latency_p95.jsonl" 2>/dev/null || true

    echo "Recording peak metrics..."
    sleep 10
done

echo -e "\n[Phase 3] Cool down (ramping down)..."

COOLDOWN_START=$(date +%s)
while [ $(( $(date +%s) - COOLDOWN_START )) -lt 120 ]; do
    if [ $CURRENT_CLIENTS -gt 25 ]; then
        CURRENT_CLIENTS=$(( CURRENT_CLIENTS - 20 ))
        export CLIENT_COUNT=$CURRENT_CLIENTS
        echo "Active clients: $CURRENT_CLIENTS"
    fi

    curl -s "$PROMETHEUS_URL/api/v1/query" \
        -d "query=mcp_client_connections_active" \
        >> "$RESULTS_DIR/$SCENARIO_NAME/cooldown_connections.jsonl" 2>/dev/null || true

    sleep 6
done

echo -e "\n=== ANALYSIS ==="

# Find peak connections
PEAK_CONN=$(tail -1 "$RESULTS_DIR/$SCENARIO_NAME/peak_connections.jsonl" | python3 -c "import json,sys; d=json.load(sys.stdin); print(d['data']['result'][0]['value'][1] if d['data']['result'] else 'N/A')" 2>/dev/null || echo "N/A")
echo "Peak concurrent connections: $PEAK_CONN"

# Error rate at peak
PEAK_ERROR_RATE=$(tail -1 "$RESULTS_DIR/$SCENARIO_NAME/error_rate.jsonl" | python3 -c "import json,sys; d=json.load(sys.stdin); print(d['data']['result'][0]['value'][1] if d['data']['result'] else '0');" 2>/dev/null || echo "0")
echo "Peak error rate: $PEAK_ERROR_RATE errors/sec"

# P95 latency at peak
PEAK_LATENCY=$(tail -1 "$RESULTS_DIR/$SCENARIO_NAME/peak_latency_p95.jsonl" | python3 -c "import json,sys; d=json.load(sys.stdin); print(d['data']['result'][0]['value'][1] if d['data']['result'] else 'N/A');" 2>/dev/null || echo "N/A")
echo "P95 latency at peak: ${PEAK_LATENCY}ms"

# Recovery time
echo "Analyzing connection recovery time..."
FINAL_CONN=$(tail -1 "$RESULTS_DIR/$SCENARIO_NAME/cooldown_connections.jsonl" | python3 -c "import json,sys; d=json.load(sys.stdin); print(d['data']['result'][0]['value'][1] if d['data']['result'] else 'N/A')" 2>/dev/null || echo "N/A")
echo "Final connections after cooldown: $FINAL_CONN"

echo -e "\n=== CONNECTION FLOOD TEST COMPLETE ==="
echo "Results saved to: $RESULTS_DIR/$SCENARIO_NAME"
echo "End Time: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
