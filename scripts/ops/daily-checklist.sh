#!/bin/bash
# Daily operations checklist for erlmcp

set -euo pipefail

echo "=== erlmcp Daily Operations Checklist ==="
echo "Date: $(date)"
echo ""

# 1. Health check across all regions
echo "1. Regional Health Check"
for REGION in us-east-1 us-west-2 eu-west-1 ap-southeast-1; do
  STATUS=$(curl -sf "https://${REGION}.erlmcp.io/health" 2>/dev/null | jq -r '.status // "down"' || echo "down")
  echo "  ${REGION}: ${STATUS}"
done

# 2. Database replication status
echo ""
echo "2. Database Replication"
if kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- psql -U postgres -c "SELECT * FROM pg_stat_replication;" >/dev/null 2>&1; then
  kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod -- \
    psql -U postgres -c "SELECT * FROM pg_stat_replication;" | \
    column -t -s'|'
else
  echo "  Could not access database replication status"
fi

# 3. Pod status
echo ""
echo "3. Pod Status"
kubectl get pods -n erlmcp-prod -l app=erlmcp

# 4. Resource usage
echo ""
echo "4. Resource Usage"
kubectl top pods -n erlmcp-prod -l app=erlmcp 2>/dev/null || echo "  Could not retrieve resource usage"

# 5. Recent errors
echo ""
echo "5. Recent Errors (last hour)"
ERROR_COUNT=$(kubectl logs -n erlmcp-prod -l app=erlmcp --since=1h 2>/dev/null | grep -i error | wc -l || echo "0")
echo "  Total error count in logs: $ERROR_COUNT"

# 6. Certificate expiry
echo ""
echo "6. Certificate Status"
if kubectl get secret erlmcp-tls -n erlmcp-prod >/dev/null 2>&1; then
  CERT_INFO=$(kubectl get secret erlmcp-tls -n erlmcp-prod -o jsonpath='{.data.tls\.crt}' 2>/dev/null | base64 -d | openssl x509 -noout -dates 2>/dev/null)
  if [ -n "$CERT_INFO" ]; then
    echo "  Not Before: $(echo "$CERT_INFO" | grep "Not Before" | cut -d= -f2)"
    echo "  Not After:  $(echo "$CERT_INFO" | grep "Not After" | cut -d= -f2)"
  else
    echo "  Could not read certificate information"
  fi
else
  echo "  Certificate secret not found"
fi

# 7. Backup verification
echo ""
echo "7. Latest Backup"
if aws s3 ls s3://erlmcp-backups/prod/postgres/ >/dev/null 2>&1; then
  aws s3 ls s3://erlmcp-backups/prod/postgres/ | tail -1
else
  echo "  Could not access S3 backups"
fi

# 8. Check for incidents
echo ""
echo "8. Active Incidents"
echo "  PagerDuty: https://yourorg.pagerduty.com"
echo "  Status Page: https://status.erlmcp.io"

# 9. Review metrics
echo ""
echo "9. Key Metrics (last 24h)"
echo "  Availability: Check Grafana dashboard"
echo "  Error Rate: Check Grafana dashboard"
echo "  Latency: Check Grafana dashboard"
echo "  Throughput: Check Grafana dashboard"

# 10. Pending actions
echo ""
echo "10. Pending Actions"
echo "  Review: ./scripts/ops/review-actions.sh"

echo ""
echo "=== Daily Checklist Complete ==="
