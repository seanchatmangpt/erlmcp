#!/bin/bash
# Emergency rollback script
# Immediately stops current deployment and restores from last known good state

set -euo pipefail

REGION=${1:-us-east-1}

echo "=== EMERGENCY ROLLBACK ==="
echo "Region: ${REGION}"
echo "Time: $(date)"
echo ""
echo "WARNING: This will scale all pods to zero and restore from backup"
echo "         Only run this in emergency situations"
echo ""

# Confirm emergency rollback
read -p "Confirm emergency rollback for ${REGION}? (type 'EMERGENCY' to continue): " CONFIRM
if [ "$CONFIRM" != "EMERGENCY" ]; then
  echo "Rollback cancelled"
  exit 1
fi

echo ""
echo "[1/6] Stopping all pods..."

# 1. Scale to zero immediately
kubectl scale statefulset erlmcp-cluster --replicas=0 -n erlmcp-prod --context=$REGION

# Force delete all pods to stop immediately
kubectl delete pods -n erlmcp-prod -l app=erlmcp \
  --context=$REGION \
  --force --grace-period=0 2>/dev/null || echo "Pods deleted"

echo "  All pods stopped"

# 2. Get last known good backup
echo ""
echo "[2/6] Finding last good backup..."
LAST_GOOD=$(aws s3 ls s3://erlmcp-backups/prod/postgres/ 2>/dev/null | \
  grep -E "backup-[0-9]+" | \
  tail -1 | \
  awk '{print $2}' || echo "")

if [ -z "$LAST_GOOD" ]; then
  echo "  ERROR: No backups found"
  exit 1
fi

echo "  Last good backup: ${LAST_GOOD}"

# 3. Verify backup integrity
echo ""
echo "[3/6] Verifying backup integrity..."
BACKUP_PREFIX=$(echo $LAST_GOOD | sed 's/.*backup-/erlmcp-backup-/')

# Check all required components
REQUIRED_FILES=(
  "${S3_BUCKET:-s3://erlmcp-backups/prod}/postgres/${BACKUP_PREFIX}.sql.gz"
  "${S3_BUCKET:-s3://erlmcp-backups/prod}/k8s/${BACKUP_PREFIX}.yaml"
  "${S3_BUCKET:-s3://erlmcp-backups/prod}/config/${BACKUP_PREFIX}.yaml"
)

MISSING=0
for FILE in "${REQUIRED_FILES[@]}"; do
  if ! aws s3 ls "$FILE" >/dev/null 2>&1; then
    echo "  ERROR: Required backup file missing: $FILE"
    ((MISSING++))
  fi
done

if [ $MISSING -gt 0 ]; then
  echo "  ERROR: Required backup files missing"
  exit 1
fi

echo "  Backup verified"

# 4. Restore from backup
echo ""
echo "[4/6] Restoring from backup..."
read -p "Restore from backup ${LAST_GOOD}? (y/n) " -n 1 -r RESTORE
echo

if [[ $RESTORE =~ ^[Yy]$ ]]; then
  # Download backup
  aws s3 cp ${S3_BUCKET}/postgres/${BACKUP_PREFIX}.sql.gz /tmp/emergency-restore.sql.gz

  # Restore to database
  kubectl run -i --rm --restart=Never postgres-restore \
    --image=postgres:15 \
    -n erlmcp-prod \
    --env="PGPASSWORD=$(kubectl get secret erlmcp-secrets -n erlmcp-prod -o jsonpath='{.data.db-password}' | base64 -d)" \
    --env="PGHOST=erlmcp-pg-primary" \
    --env="PGPORT=5432" \
    --env="PGDATABASE=erlmcp" \
    --env="PGUSER=postgres" \
    --command="
      zcat /tmp/emergency-restore.sql.gz | psql -h \$PGHOST -p \$PGPORT -U \$PGUSER -d \$PGDATABASE
    " \
    --context=$REGION

  echo "  Database restore initiated"
else
  echo "  Skipping database restore"
fi

# 5. Scale back up
echo ""
echo "[5/6] Scaling back up..."
kubectl scale statefulset erlmcp-cluster --replicas=3 -n erlmcp-prod --context=$REGION

# Wait for recovery
echo "  Waiting for pods to recover..."
kubectl wait --for=condition=ready pod \
  -l app=erlmcp \
  -n erlmcp-prod \
  --context=$REGION \
  --timeout=15m

echo "  Pods recovered"

# 6. Verify health
echo ""
echo "[6/6] Verifying rollback..."
sleep 30

if curl -sf https://erlmcp.io/health >/dev/null 2>&1; then
  echo "  Service is responding"
else
  echo "  WARNING: Service not yet responding - check logs"
fi

echo ""
echo "=== Emergency Rollback Complete ==="
echo ""
echo "Next Steps:"
echo "  1. Check logs: kubectl logs -n erlmcp-prod -l app=erlmcp --context=${REGION} --tail=100"
echo "  2. Verify data integrity"
echo "  3. Monitor metrics: https://grafana.erlmcp.io"
echo "  4. Create incident report"
echo "  5. Schedule post-mortem"
echo ""
echo "If issues persist, consider DR failover: ./scripts/dr/failover-to-dr.sh ${REGION} <dr-region>"
