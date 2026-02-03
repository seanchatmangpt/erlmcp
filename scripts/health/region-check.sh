#!/bin/bash
# Regional health check script
# Verifies health across all deployed regions

set -euo pipefail

REGION=${1:-all-regions}

echo "=== Regional Health Check ==="
echo "Region: ${REGION}"
echo "Time: $(date)"
echo ""

check_region() {
  local REGION=$1
  local ENDPOINT="https://${REGION}.erlmcp.io/health"

  echo "Checking ${REGION}..."

  # Check health endpoint
  HTTP_STATUS=$(curl -sf -w "\n%{http_code}" -o /tmp/${REGION}_health_status --max-time 10 "$ENDPOINT" 2>/dev/null)
  EXIT_CODE=$?

  if [ $EXIT_CODE -ne 0 ] || [ -z "$HTTP_STATUS" ]; then
    echo "  DOWN: Service not responding"
    return 1
  fi

  if [ "$HTTP_STATUS" != "200" ]; then
    echo "  DOWN: HTTP status $HTTP_STATUS"
    return 1
  fi

  # Parse health data
  HEALTH_DATA=$(cat /tmp/${REGION}_health_status)
  STATUS=$(echo $HEALTH_DATA | jq -r '.status // "unknown"' 2>/dev/null)
  VERSION=$(echo $HEALTH_DATA | jq -r '.version // "unknown"' 2>/dev/null)

  echo "  Status: $STATUS"
  echo "  Version: $VERSION"

  # Check pods
  READY_PODS=$(kubectl get pods -n erlmcp-prod -l app=erlmcp --context=$REGION --field-selector=status.phase=Running --no-headers 2>/dev/null | wc -l || echo "N/A")
  TOTAL_PODS=$(kubectl get pods -n erlmcp-prod -l app=erlmcp --context=$REGION --no-headers 2>/dev/null | wc -l || echo "N/A")

  if [ "$READY_PODS" != "$TOTAL_PODS" ] && [ "$TOTAL_PODS" != "N/A" ]; then
    echo "  Pods: ${READY_PODS}/${TOTAL_PODS} ready"
  else
    echo "  Pods: ${TOTAL_PODS} running"
  fi

  # Check database
  if kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod --context=$REGION -- psql -U postgres -c "SELECT 1" >/dev/null 2>&1; then
    echo "  Database: UP"
  else
    echo "  Database: DOWN or inaccessible"
  fi

  # Check metrics
  if curl -sf "https://${REGION}.erlmcp.io/metrics" | grep -q "erlmcp_" 2>/dev/null; then
    echo "  Metrics: AVAILABLE"
  else
    echo "  Metrics: UNAVAILABLE"
  fi

  # Regional latency
  LATENCY_MS=$(curl -sf -w "%{time_total}\n" -o /dev/null "$ENDPOINT" 2>/dev/null || echo "N/A")
  echo "  Latency: ${LATENCY_MS}ms"

  # Overall status
  if [ "$STATUS" = "ok" ] && [ "$READY_PODS" -gt 0 ]; then
    return 0
  else
    return 1
  fi
}

# Execute check
if [ "$REGION" = "all-regions" ]; then
  FAILED=0
  for REGION in us-east-1 us-west-2 eu-west-1 ap-southeast-1 ap-northeast-1; do
    check_region "$REGION" || ((FAILED++))
  done

  echo ""
  echo "=== Regional Health Summary ==="
  echo "Regions checked: 5"
  echo "Regions healthy: $((5 - FAILED))"

  if [ $FAILED -eq 0 ]; then
    echo "Status: ALL REGIONS HEALTHY"
    exit 0
  else
    echo "Status: $FAILED region(s) UNHEALTHY"
    exit 1
  fi
else
  check_region "$REGION"
  exit $?
fi
