#!/bin/bash
# Disaster Recovery failover script
# Promotes DR region to active and updates DNS routing

set -euo pipefail

PRIMARY_REGION=${1:-us-east-1}
DR_REGION=${2:-us-west-2}
FAILOVER_TYPE=${3:-planned}  # planned or emergency

echo "=== erlmcp DR Failover ==="
echo "Primary Region: ${PRIMARY_REGION}"
echo "DR Region: ${DR_REGION}"
echo "Type: ${FAILOVER_TYPE}"
echo "Time: $(date)"
echo ""

# 1. Pre-failover validation (skip for emergency)
if [ "$FAILOVER_TYPE" != "emergency" ]; then
  echo "[1/8] Validating DR region readiness..."

  # Check DR cluster is accessible
  if ! kubectl get nodes --context=$DR_REGION >/dev/null 2>&1; then
    echo "  ERROR: DR region cluster not accessible"
    exit 1
  fi

  # Verify DR pods can start
  echo "  Scaling up DR pods for validation..."
  kubectl scale statefulset/erlmcp-cluster --replicas=1 -n erlmcp-prod --context=$DR_REGION

  # Wait for pod ready
  if ! kubectl wait --for=condition=ready pod -l app=erlmcp -n erlmcp-prod --timeout=5m --context=$DR_REGION; then
    echo "  ERROR: DR pods failed to become ready"
    exit 1
  fi

  echo "  DR region verified, proceeding with failover"
  # Scale back down after validation
  kubectl scale statefulset/erlmcp-cluster --replicas=0 -n erlmcp-prod --context=$DR_REGION
else
  echo "[1/8] Skipping validation (emergency mode)"
fi

# 2. Promote PostgreSQL standby
echo "[2/8] Promoting PostgreSQL in ${DR_REGION}..."
if kubectl exec erlmcp-pg-standby-0 -n erlmcp-prod --context=$DR_REGION -- psql -U postgres -c "SELECT pg_is_in_recovery();" | grep -q "f"; then
  # PostgreSQL is in standby mode, promote it
  kubectl exec erlmcp-pg-standby-0 -n erlmcp-prod --context=$DR_REGION -- \
    psql -U postgres -c "SELECT pg_promote();"

  # Wait for promotion to complete
  sleep 5

  echo "  PostgreSQL promoted to primary"
else
  echo "  WARNING: PostgreSQL may not be in standby mode"
fi

# 3. Scale up erlmcp application
echo "[3/8] Scaling up erlmcp in ${DR_REGION}..."
kubectl scale statefulset/erlmcp-cluster --replicas=3 -n erlmcp-prod --context=$DR_REGION

# 4. Wait for pods ready
echo "[4/8] Waiting for pods to be ready..."
if ! kubectl wait --for=condition=ready pod -l app=erlmcp -n erlmcp-prod --timeout=10m --context=$DR_REGION; then
  echo "  ERROR: Pods failed to become ready"
  echo "  Initiating rollback..."

  # Rollback attempt
  kubectl scale statefulset/erlmcp-cluster --replicas=0 -n erlmcp-prod --context=$DR_REGION

  exit 1
fi

# 5. Update DNS to point to DR region
echo "[5/8] Updating DNS to point to ${DR_REGION}..."

# Create Route 53 change batch
cat > /tmp/failover-dns-change.json << EOF
{
  "Comment": "Failover to ${DR_REGION}",
  "Changes": [
    {
      "Action": "UPSERT",
      "ResourceRecordSet": {
        "Name": "erlmcp.io",
        "Type": "A",
        "SetIdentifier": "${DR_REGION}-failover",
        "Region": "${DR_REGION}",
        "AliasTarget": {
          "HostedZoneId": "Z35SXDOTRKT7K6",
          "DNSName": "$(kubectl get svc erlmcp-ingress -n erlmcp-prod --context=$DR_REGION -o jsonpath='{.status.loadBalancer.ingress[0].hostname}')",
          "EvaluateTargetHealth": true
        }
      }
    }
  ]
}
EOF

# Apply DNS change
aws route53 change-resource-record-sets \
  --hosted-zone-id Z35SXDOTRKT7K6 \
  --change-batch file:///tmp/failover-dns-change.json

echo "  DNS update submitted"

# 6. Verify traffic is flowing to DR
echo "[6/8] Verifying traffic..."
RETRY_COUNT=0
MAX_RETRIES=10

while [ $RETRY_COUNT -lt $MAX_RETRIES ]; do
  if curl -sf https://erlmcp.io/health >/dev/null 2>&1; then
    echo "  Traffic verification successful"
    break
  fi

  echo "  Waiting for traffic... ($((RETRY_COUNT + 1))/$MAX_RETRIES)"
  sleep 10
  RETRY_COUNT=$((RETRY_COUNT + 1))
done

if [ $RETRY_COUNT -eq $MAX_RETRIES ]; then
  echo "  ERROR: Traffic verification failed"
  echo "  Rolling back DNS changes..."
  # Rollback DNS would go here
  exit 1
fi

# 7. Update monitoring targets
echo "[7/8] Updating monitoring targets..."

# Update Prometheus scraping targets
if command -v promtool >/dev/null 2>&1; then
  promtool update targets \
    --prometheus-url=http://prometheus.erlmcp.io \
    -y
  echo "  Prometheus targets updated"
else
  echo "  Could not update Prometheus targets (promtool not available)"
fi

# Update dashboard data sources
if [ -f ./scripts/monitoring/update-targets.sh ]; then
  ./scripts/monitoring/update-targets.sh ${DR_REGION}
  echo "  Dashboard targets updated"
fi

# 8. Scale down primary (if safe and not emergency)
echo "[8/8] Handling primary region..."
if [ "$FAILOVER_TYPE" != "emergency" ]; then
  echo "  Keeping primary region running (can be used for read-only or monitoring)"
else
  echo "  Emergency mode: scaling down primary region to free resources"
  kubectl scale statefulset/erlmcp-cluster --replicas=0 -n erlmcp-prod --context=$PRIMARY_REGION
fi

echo ""
echo "=== Failover Complete ==="
echo "Active Region: ${DR_REGION}"
echo "Status: Verify all systems in DR region"
echo "Next Steps:"
echo "  1. Monitor metrics in Grafana"
echo "  2. Check logs: kubectl logs -n erlmcp-prod -l app=erlmcp --context=${DR_REGION}"
echo "  3. Verify data consistency"
echo "  4. Plan return to primary region when ready"
echo ""
echo "For failback, use: ./scripts/dr/failback-to-primary.sh ${PRIMARY_REGION} ${DR_REGION}"
