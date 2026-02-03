#!/bin/bash
# Canary monitoring script
# Monitors canary deployment metrics and provides recommendations

set -euo pipefail

DURATION=${1:-10m}  # How long to monitor
THRESHOLD_ERROR_RATE=${2:-0.01}  # 1% error rate threshold
THRESHOLD_LATENCY_P95=${3:-0.5}  # 500ms P95 latency threshold

echo "=== Canary Monitoring ==="
echo "Duration: $DURATION"
echo "Error Rate Threshold: $THRESHOLD_ERROR_RATE (1%)"
echo "Latency P95 Threshold: ${THRESHOLD_P95}s (500ms)"
echo ""

# Start time
START_TIME=$(date +%s)
END_TIME=$((START_TIME + $(echo $DURATION | sed 's/[^0-9]//g') * 60))

# Get current deployment metrics
CANARY_PODS=$(kubectl get pods -n erlmcp-prod -l app=erlmcp-canary -o jsonpath='{.items[*].metadata.name}' 2>/dev/null || echo "")
BASELINE_PODS=$(kubectl get pods -n erlmcp-prod -l app=erlmcp,app!=canary -o jsonpath='{.items[*].metadata.name}' 2>/dev/null || echo "")

if [ -z "$CANARY_PODS" ]; then
  echo "ERROR: No canary pods found"
  exit 1
fi

echo "Canary Pods: $CANARY_PODS"
echo "Baseline Pods: $BASELINE_PODS"
echo ""

# Monitoring loop
ITERATION=0
MAX_ITERATIONS=120  # Check every 5 seconds for 10 minutes

while [ $(date +%s) -lt $END_TIME ] && [ $ITERATION -lt $MAX_ITERATIONS ]; do
  ITERATION=$((ITERATION + 1))
  echo ""
  echo "=== Check $ITERATION ($(date +%H:%M:%S)) ==="

  # 1. Check canary pod health
  echo "[1/6] Canary Pod Health"
  CANARY_READY=0
  for POD in $CANARY_PODS; do
    if kubectl get pod $POD -n erlmcp-prod -o jsonpath='{.status.conditions[?(@.type=="Ready")].status}' 2>/dev/null | grep -q "True"; then
      ((CANARY_READY++))
    fi
  done
  echo "  Canary pods ready: $CANARY_READY/${#CANARY_PODS}"

  if [ $CANARY_READY -eq 0 ]; then
    echo "  FAIL: No canary pods ready"
    exit 1
  fi

  # 2. Check error rate
  echo "[2/6] Error Rate"
  # Get error rate from metrics
  ERROR_RATE=$(curl -sf https://erlmcp.io/metrics 2>/dev/null | \
    grep 'http_requests_total{status="5"}' | \
    awk '{sum($1)}' || echo "0")

  # Calculate requests per second over 5 minutes
  TOTAL_REQUESTS=$(curl -sf https://erlmcp.io/metrics 2>/dev/null | \
    grep 'http_requests_total' | \
    awk '{sum($1)}' || echo "0")

  if [ "$TOTAL_REQUESTS" -gt 0 ]; then
    ERROR_RATE=$(echo "scale=4; $ERROR_RATE / $TOTAL_REQUESTS" | bc)
    echo "  Error rate: $(echo "scale=2; $ERROR_RATE * 100" | bc)%"

    if (( $(echo "$ERROR_RATE > $THRESHOLD_ERROR_RATE" | bc -l) )); then
      echo "  FAIL: Error rate $ERROR_RATE exceeds threshold $THRESHOLD_ERROR_RATE"
      exit 1
    fi
  else
    echo "  INFO: No request data available"
  fi

  # 3. Check latency
  echo "[3/6] Latency (P95)"
  P95_LATENCY=$(curl -sf https://erlmcp.io/metrics 2>/dev/null | \
    grep 'http_request_duration_seconds_bucket' | \
    grep 'le="0.5"' | \
    tail -1 | \
    awk '{sum($1)}' || echo "0")

  echo "  P95 latency: ${P95_LATENCY}s"

  if (( $(echo "$P95_LATENCY > $THRESHOLD_LATENCY_P95" | bc -l) )); then
    echo "  FAIL: P95 latency ${P95_LATENCY}s exceeds threshold ${THRESHOLD_LATENCY_P95}s"
    exit 1
  fi

  # 4. Check resource usage
  echo "[4/6] Resource Usage"
  CANARY_CPU=$(kubectl top pods -n erlmcp-prod -l app=erlmcp-canary --no-headers 2>/dev/null | awk '{sum+$2}' | tail -1 || echo "N/A")
  CANARY_MEM=$(kubectl top pods -n erlmcp-prod -l app=erlmcp-canary --no-headers 2>/dev/null | awk '{sum+$3}' | tail -1 || echo "N/A")

  echo "  CPU: ${CANARY_CPU}"
  echo "  Memory: ${CANARY_MEM}"

  # 5. Compare with baseline
  echo "[5/6] Baseline Comparison"

  # Get baseline error rate
  BASELINE_ERRORS=$(curl -sf https://erlmcp.io/metrics 2>/dev/null | \
    grep 'http_requests_total{status="5",pod!~"canary"}' | \
    awk '{sum($1)}' || echo "0")

  # Get baseline requests
  BASELINE_REQUESTS=$(curl -sf https://erlmcp.io/metrics 2>/dev/null | \
    grep 'http_requests_total{pod!~"canary"}' | \
    awk '{sum($1)}' || echo "0")

  if [ "$BASELINE_REQUESTS" -gt 0 ]; then
    BASELINE_ERROR_RATE=$(echo "scale=4; $BASELINE_ERRORS / $BASELINE_REQUESTS" | bc)
    echo "  Baseline error rate: $(echo "scale=2; $BASELINE_ERROR_RATE * 100" | bc)%"

    # Compare
    if (( $(echo "$ERROR_RATE > $BASELINE_ERROR_RATE * 2" | bc -l) )); then
      echo "  WARN: Canary error rate is >2x baseline"
      ((WARNINGS++))
    fi
  fi

  # 6. Log analysis
  echo "[6/6] Log Analysis"
  CANARY_ERROR_COUNT=$(kubectl logs -n erlmcp-prod -l app=erlmcp-canary --since=1m 2>/dev/null | grep -i error | wc -l || echo "0")
  BASELINE_ERROR_COUNT=$(kubectl logs -n erlmcp-prod -l app=erlmcp,app!=canary --since=1m 2>/dev/null | grep -i error | wc -l || echo "0")

  echo "  Canary errors (last 1m): $CANARY_ERROR_COUNT"
  echo "  Baseline errors (last 1m): $BASELINE_ERROR_COUNT"

  # Check if error rate is significantly higher
  if [ $CANARY_ERROR_COUNT -gt $((BASELINE_ERROR_COUNT * 3)) ]; then
    echo "  WARN: Canary error count is >3x baseline"
    ((WARNINGS++))
  fi

  # Wait before next check
  sleep 30
done

echo ""
echo "=== Canary Monitoring Complete ==="
echo "Duration monitored: $(($(date +%s) - START_TIME) / 60) minutes"
echo ""
echo "Summary:"
echo "  Error Rate: ${ERROR_RATE:-N/A}"
echo "  P95 Latency: ${P95_LATENCY}s"
echo "  Warnings: $WARNINGS"

# Final recommendation
if [ $WARNINGS -eq 0 ]; then
  echo "  Result: PASS - Canary is healthy"
  echo "  Recommendation: Proceed with full rollout"
  exit 0
else
  echo "  Result: WARN - Canary has warnings"
  echo "  Recommendation: Review warnings before proceeding"
  exit 1
fi
