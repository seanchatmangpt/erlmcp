#!/bin/bash
# Production health check script

set -euo pipefail

ENDPOINT=${1:-https://erlmcp.io}
TIMEOUT=${2:-30}

echo "=== Production Health Check ==="
echo "Endpoint: ${ENDPOINT}"
echo "Timeout: ${TIMEOUT}s"
echo "Time: $(date)"
echo ""

FAILED=0
WARNINGS=0

# 1. Overall health endpoint
echo "1. Overall Health"
if HTTP_STATUS=$(curl -sf -w "\n%{http_code}" -o /tmp/health_status --max-time $TIMEOUT "$ENDPOINT/health" 2>/dev/null); then
  if [ "$HTTP_STATUS" = "200" ]; then
    HEALTH_DATA=$(cat /tmp/health_status)
    echo "  Status: $(echo $HEALTH_DATA | jq -r '.status // "unknown"')"
    echo "  Version: $(echo $HEALTH_DATA | jq -r '.version // "unknown"')"
  else
    echo "  FAIL: HTTP status $HTTP_STATUS (expected 200)"
    ((FAILED++))
  fi
else
  echo "  FAIL: Request failed or timed out"
  ((FAILED++))
fi

# 2. Readiness check
echo ""
echo "2. Readiness Check"
if curl -sf --max-time $TIMEOUT "$ENDPOINT/ready" >/dev/null 2>&1; then
  echo "  PASS: Service is ready"
else
  echo "  FAIL: Service is not ready"
  ((FAILED++))
fi

# 3. Metrics endpoint
echo ""
echo "3. Metrics Endpoint"
if curl -sf --max-time $TIMEOUT "$ENDPOINT/metrics" | grep -q "erlmcp_" 2>/dev/null; then
  METRIC_COUNT=$(curl -sf --max-time $TIMEOUT "$ENDPOINT/metrics" | grep -c "erlmcp_" 2>/dev/null || echo 0)
  echo "  PASS: Metrics available (${METRIC_COUNT} metrics)"
else
  echo "  WARN: Metrics endpoint not responding"
  ((WARNINGS++))
fi

# 4. Regional health check
echo ""
echo "4. Regional Health"
for REGION in us-east-1 eu-west-1 ap-southeast-1; do
  REGION_STATUS=$(curl -sf --max-time 10 "https://${REGION}.erlmcp.io/health" | jq -r '.status // "down"' 2>/dev/null || echo "down")
  if [ "$REGION_STATUS" = "ok" ]; then
    echo "  ${REGION}: UP"
  else
    echo "  ${REGION}: DOWN"
    ((FAILED++))
  fi
done

# 5. Database connectivity (if accessible)
echo ""
echo "5. Database Connectivity"
if kubectl exec -n erlmcp-prod erlmcp-pg-primary-0 -- psql -U postgres -c "SELECT 1" >/dev/null 2>&1; then
  echo "  PASS: Database is accessible"
else
  echo "  WARN: Database not accessible from monitoring pod"
  ((WARNINGS++))
fi

# 6. Pod status
echo ""
echo "6. Pod Status"
READY_PODS=$(kubectl get pods -n erlmcp-prod -l app=erlmcp --field-selector=status.phase=Running --no-headers 2>/dev/null | wc -l)
TOTAL_PODS=$(kubectl get pods -n erlmcp-prod -l app=erlmcp --no-headers 2>/dev/null | wc -l)
if [ "$READY_PODS" -gt 0 ]; then
  echo "  Pods: ${READY_PODS}/${TOTAL_PODS} ready"
  if [ "$READY_PODS" -eq "$TOTAL_PODS" ]; then
    echo "  PASS: All pods are ready"
  else
    echo "  WARN: Some pods are not ready"
    ((WARNINGS++))
  fi
else
  echo "  FAIL: No pods are running"
  ((FAILED++))
fi

# 7. Resource usage
echo ""
echo "7. Resource Usage"
if kubectl top pods -n erlmcp-prod -l app=erlmcp >/dev/null 2>&1; then
  echo "  Resource usage available"
  AVG_CPU=$(kubectl top pods -n erlmcp-prod -l app=erlmcp --no-headers | awk '{sum+$2}' | awk '{printf "%.0f\n", $1/$NF*100}')
  AVG_MEM=$(kubectl top pods -n erlmcp-prod -l app=erlmcp --no-headers | awk '{sum+$3}' | awk '{printf "%.0f\n", $3/$NF*100}')
  echo "  Avg CPU: ${AVG_CPU}%"
  echo "  Avg Memory: ${AVG_MEM}%"

  if [ "${AVG_CPU%.*}" -gt 80 ]; then
    echo "  WARN: High CPU usage"
    ((WARNINGS++))
  fi
  if [ "${AVG_MEM%.*}" -gt 85 ]; then
    echo "  WARN: High memory usage"
    ((WARNINGS++))
  fi
else
  echo "  WARN: Could not retrieve resource usage"
  ((WARNINGS++))
fi

# 8. Error rate check
echo ""
echo "8. Error Rate"
ERROR_RATE=$(curl -sf --max-time $TIMEOUT "$ENDPOINT/metrics" 2>/dev/null | grep 'http_requests_total{status="5"}' | grep -v 'endpoint="/health"' || echo "0")
if [ -n "$ERROR_RATE" ]; then
  echo "  5xx errors detected (check logs)"
else
  echo "  PASS: No error rate data available"
fi

# 9. Recent errors
echo ""
echo "9. Recent Errors"
ERROR_COUNT=$(kubectl logs -n erlmcp-prod -l app=erlmcp --since=5m 2>/dev/null | grep -i error | wc -l || echo 0)
if [ "$ERROR_COUNT" -gt 100 ]; then
  echo "  WARN: High error count in last 5 minutes ($ERROR_COUNT errors)"
  ((WARNINGS++))
elif [ "$ERROR_COUNT" -gt 10 ]; then
  echo "  INFO: Some errors in last 5 minutes ($ERROR_COUNT errors)"
else
  echo "  PASS: Low error count ($ERROR_COUNT errors)"
fi

# 10. Certificate expiry
echo ""
echo "10. Certificate Status"
CERT_EXPIRY=$(kubectl get secret erlmcp-tls -n erlmcp-prod -o jsonpath='{.data.tls\.crt}' 2>/dev/null | base64 -d | openssl x509 -noout -enddate 2>/dev/null || echo "unknown")
if [ "$CERT_EXPIRY" != "unknown" ]; then
  EXPIRY_DAYS=$(( ($(date -d "$CERT_EXPIRY" +%s) - $(date +%s)) / 86400 ))
  if [ "$EXPIRY_DAYS" -lt 30 ]; then
    echo "  WARN: Certificate expires in $EXPIRY_DAYS days"
    ((WARNINGS++))
  else
    echo "  PASS: Certificate is valid (expires $CERT_EXPIRY)"
  fi
else
  echo "  INFO: Could not check certificate expiry"
fi

# Summary
echo ""
echo "=== Health Check Summary ==="
if [ $FAILED -eq 0 ] && [ $WARNINGS -eq 0 ]; then
  echo "Result: HEALTHY - All checks passed"
  exit 0
elif [ $FAILED -eq 0 ] && [ $WARNINGS -gt 0 ]; then
  echo "Result: HEALTHY with warnings ($WARNINGS warnings)"
  exit 0
else
  echo "Result: UNHEALTHY - ($FAILED failures, $WARNINGS warnings)"
  exit 1
fi
