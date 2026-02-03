#!/bin/bash
# Automated daily backup script for erlmcp production
# This script backs up PostgreSQL, ETS tables, configurations, and Kubernetes resources

set -euo pipefail

BACKUP_DATE=$(date +%Y%m%d-%H%M%S)
BACKUP_PREFIX="erlmcp-backup-${BACKUP_DATE}"
S3_BUCKET="s3://erlmcp-backups/prod"

# Region to backup from
REGION=${1:-us-east-1}

echo "Starting backup: ${BACKUP_PREFIX}"
echo "Region: ${REGION}"
echo "Time: $(date)"
echo ""

# Create temporary directory for backup artifacts
BACKUP_DIR="/tmp/erlmcp-backup-${BACKUP_DATE}"
mkdir -p "${BACKUP_DIR}"

# 1. Database backup
echo "[1/7] Backing up PostgreSQL..."
if kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod --context=$REGION -- psql -U postgres -c "SELECT version()" >/dev/null 2>&1; then
  kubectl exec erlmcp-pg-primary-0 -n erlmcp-prod --context=$REGION -- \
    pg_dumpall -U postgres | \
    gzip > "${BACKUP_DIR}/postgres-${BACKUP_DATE}.sql.gz"

  # Upload to S3
  aws s3 cp "${BACKUP_DIR}/postgres-${BACKUP_DATE}.sql.gz \
    ${S3_BUCKET}/postgres/${BACKUP_PREFIX}.sql.gz
  echo "  PostgreSQL backup complete"
else
  echo "  ERROR: PostgreSQL not accessible"
  exit 1
fi

# 2. ETS backup (from running nodes)
echo "[2/7] Backing up ETS tables..."
for POD in $(kubectl get pods -n erlmcp-prod -l app=erlmcp --context=$REGION -o name); do
  echo "  Backing up ${POD}..."

  # Trigger backup within the node
  kubectl exec ${POD} -n erlmcp-prod --context=$REGION -- \
    bash -c "erlmcpctl backup /tmp/ets.backup 2>/dev/null || true" || true

  # Copy backup locally
  kubectl cp ${POD}:/tmp/ets.backup \
    "${BACKUP_DIR}/ets-$(basename ${POD}).backup" -n erlmcp-prod --context=$REGION 2>/dev/null || true
done

# Compress and upload ETS backups
if ls "${BACKUP_DIR}"/ets-*.backup 1>/dev/null 2>&1; then
  tar -czf "${BACKUP_DIR}/ets-backups.tar.gz" "${BACKUP_DIR}"/ets-*.backup 2>/dev/null
  aws s3 cp "${BACKUP_DIR}/ets-backups.tar.gz" \
    ${S3_BUCKET}/ets/${BACKUP_PREFIX}.tar.gz
  echo "  ETS backup complete"
else
  echo "  No ETS backups to upload"
fi

# 3. ConfigMap backup
echo "[3/7] Backing up configuration..."
kubectl get configmap -n erlmcp-prod -o yaml --context=$REGION > "${BACKUP_DIR}/configmaps.yaml"
aws s3 cp "${BACKUP_DIR}/configmaps.yaml" \
  ${S3_BUCKET}/config/${BACKUP_PREFIX}.yaml
echo "  Configuration backup complete"

# 4. Secrets backup (encrypted)
echo "[4/7] Backing up secrets..."
BACKUP_KEY=$(aws secretsmanager get-secret-value --secret-id erlmcp/backup-key --region=$REGION --query SecretString --output text 2>/dev/null || echo "default-backup-key")

kubectl get secret -n erlmcp-prod -o yaml --context=$REGION | \
  openssl enc -aes-256-cbc -salt -pass pass:"${BACKUP_KEY}" -out "${BACKUP_DIR}/secrets.enc"
aws s3 cp "${BACKUP_DIR}/secrets.enc" \
  ${S3_BUCKET}/secrets/${BACKUP_PREFIX}.enc
echo "  Secrets backup complete (encrypted)"

# 5. Kubernetes resources
echo "[5/7] Backing up Kubernetes resources..."
kubectl get all -n erlmcp-prod -o yaml --context=$REGION > "${BACKUP_DIR}/k8s-resources.yaml"
aws s3 cp "${BACKUP_DIR}/k8s-resources.yaml" \
  ${S3_BUCKET}/k8s/${BACKUP_PREFIX}.yaml
echo "  Kubernetes resources backup complete"

# 6. Backup metadata
echo "[6/7] Creating backup metadata..."
cat > "${BACKUP_DIR}/metadata.json" << EOF
{
  "backup_id": "${BACKUP_PREFIX}",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "region": "${REGION}",
  "environment": "production",
  "components": ["postgres", "ets", "configmaps", "secrets", "k8s"],
  "backup_type": "daily",
  "retention_days": 30
}
EOF
aws s3 cp "${BACKUP_DIR}/metadata.json" \
  ${S3_BUCKET}/metadata/${BACKUP_PREFIX}.json
echo "  Metadata complete"

# 7. Clean old backups (keep last 30 days)
echo "[7/7] Cleaning old backups..."
aws s3 ls ${S3_BUCKET}/postgres/ | \
  grep "prev" | \
  awk '{print $2}' | \
  sort | \
  head -n -30 | \
  while read OLD_BACKUP; do
    aws s3 rm ${S3_BUCKET}/postgres/${OLD_BACKUP} 2>/dev/null || true
  done

# Clean up temporary directory
rm -rf "${BACKUP_DIR}"

# 8. Verify backup
echo ""
echo "Verifying backup..."
BACKUP_OK=true

# Verify database backup
if ! aws s3 ls ${S3_BUCKET}/postgres/${BACKUP_PREFIX}.sql.gz >/dev/null 2>&1; then
  echo "  ERROR: PostgreSQL backup not found in S3"
  BACKUP_OK=false
fi

# Verify metadata
if ! aws s3 ls ${S3_BUCKET}/metadata/${BACKUP_PREFIX}.json >/dev/null 2>&1; then
  echo "  ERROR: Backup metadata not found in S3"
  BACKUP_OK=false
fi

if [ "$BACKUP_OK" = true ]; then
  echo "  Backup verification successful"
else
  echo "  Backup verification FAILED"
  exit 1
fi

echo ""
echo "=== Backup Complete ==="
echo "Backup ID: ${BACKUP_PREFIX}"
echo "S3 Location: ${S3_BUCKET}/"
echo "Retention: 30 days"
