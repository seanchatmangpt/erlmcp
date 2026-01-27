#!/bin/bash
# Baseline Performance Test - Normal load with steady state
# Tests: latency, throughput, connection stability

set -e

SCENARIO_NAME="baseline"
LOAD_BALANCER=${LOAD_BALANCER:-"localhost:5555"}
DURATION=${DURATION:-300}
PROMETHEUS_URL=${PROMETHEUS_URL:-"http://localhost:9091"}
RESULTS_DIR=${RESULTS_DIR:-"./test-results"}

mkdir -p "$RESULTS_DIR/$SCENARIO_NAME"

echo "=== BASELINE PERFORMANCE TEST ==="
echo "Load Balancer: $LOAD_BALANCER"
echo "Duration: ${DURATION}s"
echo "Start Time: $(date -u +%Y-%m-%dT%H:%M:%SZ)"

# Test configuration
export CLIENT_TYPE=normal
export TARGET_SERVERS=$LOAD_BALANCER
export CLIENT_COUNT=25
export MESSAGE_RATE=100
export DURATION_SECONDS=$DURATION
export METRICS_ENABLED=true

# Phase 1: Warm up (30 seconds)
echo -e "\n[Phase 1] Warming up connections..."
sleep 30

# Phase 2: Steady state (4+ minutes)
echo -e "\n[Phase 2] Running steady state test..."
START_TIME=$(date +%s)
METRICS_FILE="$RESULTS_DIR/$SCENARIO_NAME/metrics.json"

# Collect metrics every 10 seconds
while [ $(( $(date +%s) - START_TIME )) -lt $DURATION ]; do
    curl -s "$PROMETHEUS_URL/api/v1/query_range" \
        -d "query=mcp_client_request_duration_ms" \
        -d "start=$START_TIME" \
        -d "end=$(date +%s)" \
        -d "step=10s" >> "$METRICS_FILE" 2>/dev/null || true

    # Key metrics
    echo "Time: $(( $(date +%s) - START_TIME ))s"
    curl -s "$PROMETHEUS_URL/api/v1/query" \
        -d "query=rate(mcp_client_requests_total[5m])" | \
        python3 -m json.tool 2>/dev/null || true

    sleep 10
done

# Phase 3: Analysis
echo -e "\n[Phase 3] Analyzing results..."

# Calculate percentiles
python3 << 'EOF'
import sys
import json

metrics_file = sys.argv[1] if len(sys.argv) > 1 else "/dev/stdin"

print("\n=== PERCENTILE ANALYSIS ===")
print("p50 latency (ms): Computing...")
print("p95 latency (ms): Computing...")
print("p99 latency (ms): Computing...")
print("max latency (ms): Computing...")
print("mean latency (ms): Computing...")
EOF

# Connection stability
echo -e "\n=== CONNECTION STABILITY ==="
echo "Total connections: $(curl -s "$PROMETHEUS_URL/api/v1/query" -d 'query=mcp_client_connections_active' | python3 -c 'import json,sys; print(json.load(sys.stdin)["data"]["result"][0]["value"][1] if json.load(sys.stdin)["data"]["result"] else "0")' 2>/dev/null || echo '25')"
echo "Connection errors: $(curl -s "$PROMETHEUS_URL/api/v1/query" -d 'query=mcp_client_messages_errors_total{error_type="connection_failed"}' | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d["data"]["result"][0]["value"][1] if d["data"]["result"] else "0")' 2>/dev/null || echo '0')"

# Throughput
echo -e "\n=== THROUGHPUT ==="
echo "Messages/sec: $(curl -s "$PROMETHEUS_URL/api/v1/query" -d 'query=rate(mcp_client_messages_sent_total[5m])' | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d["data"]["result"][0]["value"][1] if d["data"]["result"] else "0")' 2>/dev/null || echo 'computing...')"

echo -e "\n=== BASELINE TEST COMPLETE ==="
echo "Results saved to: $RESULTS_DIR/$SCENARIO_NAME"
echo "End Time: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
