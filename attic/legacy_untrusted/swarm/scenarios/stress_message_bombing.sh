#!/bin/bash
# Message Bombing Test - High throughput stress test
# Tests: message handling capacity, latency degradation, error handling

set -e

SCENARIO_NAME="message_bombing"
LOAD_BALANCER=${LOAD_BALANCER:-"localhost:5555"}
DURATION=${DURATION:-600}
PROMETHEUS_URL=${PROMETHEUS_URL:-"http://localhost:9091"}
RESULTS_DIR=${RESULTS_DIR:-"./test-results"}

mkdir -p "$RESULTS_DIR/$SCENARIO_NAME"

echo "=== MESSAGE BOMBING STRESS TEST ==="
echo "Load Balancer: $LOAD_BALANCER"
echo "Duration: ${DURATION}s"
echo "Start Time: $(date -u +%Y-%m-%dT%H:%M:%SZ)"

# Configuration for message bombing
export CLIENT_TYPE=message-bomber
export TARGET_SERVERS=$LOAD_BALANCER
export CLIENT_COUNT=20
export MESSAGE_RATE=10000
export METRICS_ENABLED=true

START_TIME=$(date +%s)

echo -e "\n[Phase 1] Initialization (30s warmup)..."
sleep 30

echo -e "\n[Phase 2] Message bombing phase (300s)..."
BOMBING_START=$(date +%s)

# Collect detailed metrics during bombing
while [ $(( $(date +%s) - BOMBING_START )) -lt 300 ]; do
    TIMESTAMP=$(date -u +%Y-%m-%dT%H:%M:%SZ)

    # Request rate
    curl -s "$PROMETHEUS_URL/api/v1/query" \
        -d "query=rate(mcp_client_messages_sent_total[1m])" \
        --output - > /tmp/req_rate.json 2>/dev/null
    REQ_RATE=$(python3 -c "import json; d=json.load(open('/tmp/req_rate.json')); print(d['data']['result'][0]['value'][1] if d['data']['result'] else 'N/A')" 2>/dev/null || echo "N/A")

    # Latency p50
    curl -s "$PROMETHEUS_URL/api/v1/query" \
        -d "query=histogram_quantile(0.5, rate(mcp_client_request_duration_ms_bucket[1m]))" \
        --output - > /tmp/lat_p50.json 2>/dev/null
    LAT_P50=$(python3 -c "import json; d=json.load(open('/tmp/lat_p50.json')); print(d['data']['result'][0]['value'][1] if d['data']['result'] else 'N/A')" 2>/dev/null || echo "N/A")

    # Latency p95
    curl -s "$PROMETHEUS_URL/api/v1/query" \
        -d "query=histogram_quantile(0.95, rate(mcp_client_request_duration_ms_bucket[1m]))" \
        --output - > /tmp/lat_p95.json 2>/dev/null
    LAT_P95=$(python3 -c "import json; d=json.load(open('/tmp/lat_p95.json')); print(d['data']['result'][0]['value'][1] if d['data']['result'] else 'N/A')" 2>/dev/null || echo "N/A")

    # Error rate
    curl -s "$PROMETHEUS_URL/api/v1/query" \
        -d "query=rate(mcp_client_messages_errors_total[1m])" \
        --output - > /tmp/err_rate.json 2>/dev/null
    ERR_RATE=$(python3 -c "import json; d=json.load(open('/tmp/err_rate.json')); print(d['data']['result'][0]['value'][1] if d['data']['result'] else '0')" 2>/dev/null || echo "0")

    # Log results
    echo "$TIMESTAMP | Req/s: $REQ_RATE | p50: ${LAT_P50}ms | p95: ${LAT_P95}ms | Errors/s: $ERR_RATE"

    # Save for later analysis
    echo "{\"timestamp\":\"$TIMESTAMP\",\"req_rate\":$REQ_RATE,\"p50_latency\":$LAT_P50,\"p95_latency\":$LAT_P95,\"error_rate\":$ERR_RATE}" \
        >> "$RESULTS_DIR/$SCENARIO_NAME/metrics.jsonl"

    sleep 5
done

echo -e "\n[Phase 3] Cool down (120s)..."
unset CLIENT_COUNT  # Reset to allow graceful shutdown
sleep 120

echo -e "\n=== ANALYSIS ==="

# Calculate statistics
python3 << 'PYTHON_EOF'
import json
import sys
from pathlib import Path

metrics_file = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("/dev/stdin")

rates = []
latencies_p50 = []
latencies_p95 = []
errors = []

if metrics_file.exists():
    with open(metrics_file) as f:
        for line in f:
            try:
                data = json.loads(line.strip())
                if isinstance(data['req_rate'], (int, float)):
                    rates.append(float(data['req_rate']))
                if isinstance(data['p50_latency'], (int, float)):
                    latencies_p50.append(float(data['p50_latency']))
                if isinstance(data['p95_latency'], (int, float)):
                    latencies_p95.append(float(data['p95_latency']))
                if isinstance(data['error_rate'], (int, float)):
                    errors.append(float(data['error_rate']))
            except (json.JSONDecodeError, TypeError, KeyError):
                continue

    if rates:
        print(f"Request Rate:")
        print(f"  Min:  {min(rates):.2f} req/s")
        print(f"  Max:  {max(rates):.2f} req/s")
        print(f"  Avg:  {sum(rates)/len(rates):.2f} req/s")

    if latencies_p50:
        print(f"p50 Latency:")
        print(f"  Min:  {min(latencies_p50):.2f} ms")
        print(f"  Max:  {max(latencies_p50):.2f} ms")
        print(f"  Avg:  {sum(latencies_p50)/len(latencies_p50):.2f} ms")

    if latencies_p95:
        print(f"p95 Latency:")
        print(f"  Min:  {min(latencies_p95):.2f} ms")
        print(f"  Max:  {max(latencies_p95):.2f} ms")
        print(f"  Avg:  {sum(latencies_p95)/len(latencies_p95):.2f} ms")

    if errors:
        print(f"Error Rate (errors/sec):")
        print(f"  Min:  {min(errors):.4f}")
        print(f"  Max:  {max(errors):.4f}")
        print(f"  Avg:  {sum(errors)/len(errors):.4f}")

PYTHON_EOF

echo -e "\n=== MESSAGE BOMBING TEST COMPLETE ==="
echo "Results saved to: $RESULTS_DIR/$SCENARIO_NAME"
echo "End Time: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
