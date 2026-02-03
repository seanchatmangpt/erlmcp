# erlmcp v3 Backup Automation System

## Overview

Production-grade backup and restore automation for erlmcp v3 with:
- Automated ETS/DETS/Mnesia table backups
- PostgreSQL database dumps
- Stateful set (Docker volume) backups
- Cross-region replication for disaster recovery
- Backup retention policy management
- Comprehensive verification and testing

## Docker-Only Constitution Compliance

**All backup operations run exclusively via Docker containers.** No host execution permitted.

## Scripts

### backup.sh - Main Backup Script

Perform automated backups of all erlmcp data.

```bash
# Full backup (default)
./scripts/backup/backup.sh full

# Incremental backup
./scripts/backup/backup.sh incremental

# Dry run (show what would be done)
./scripts/backup/backup.sh full --dry-run

# Skip remote upload
./scripts/backup/backup.sh full --no-upload

# Cleanup old backups only
./scripts/backup/backup.sh --cleanup-only
```

**Environment Variables:**
- `BACKUP_ROOT` - Backup directory (default: /var/lib/erlmcp/backups)
- `BACKUP_RETENTION_DAYS` - Local retention period (default: 30)
- `S3_BUCKET` - S3 bucket for remote backups
- `S3_REGION` - S3 region (default: us-east-1)
- `UPLOAD_ENABLED` - Enable S3 upload (default: true)
- `ENCRYPT_KEY_FILE` - Path to encryption key

**Backup Components:**
- ETS tables (in-memory data)
- DETS tables (disk-based tables)
- Mnesia database (distributed state)
- PostgreSQL (if configured)
- Docker volumes (persistent data)
- Configuration files

**Outputs:**
- Backup ID: `erlmcp_{type}_{timestamp}`
- Manifest: `manifest.json`
- Checksums: `checksums.sha256`
- Receipt: `receipt.json` (constitution compliant)

### restore.sh - Restore Script

Restore from backup with verification.

```bash
# Restore from specific backup
./scripts/backup/restore.sh erlmcp_full_20240202_120000

# Restore from latest backup
./scripts/backup/restore.sh latest

# Skip verification (not recommended)
./scripts/backup/restore.sh latest --skip-verification

# Dry run
./scripts/backup/restore.sh latest --dry-run

# List available backups
./scripts/backup/restore.sh --list
```

**Restore Process:**
1. Locate backup (local or S3)
2. Verify checksums
3. Decrypt if encrypted
4. Stop services
5. Restore components
6. Start services
7. Verify health

### backup_retention.sh - Retention Policy Manager

Manage backup retention and cleanup.

```bash
# Generate retention report
./scripts/backup/backup_retention.sh report

# Apply retention policy (delete expired backups)
./scripts/backup/backup_retention.sh apply

# Dry run before applying
./scripts/backup/backup_retention.sh apply --dry-run

# Cleanup orphaned files
./scripts/backup/backup_retention.sh cleanup
```

**Retention Policies:**
- Local full backups: 30 days
- Weekly backups: 12 weeks
- Monthly backups: 12 months
- Compliance backups: 7 years
- Remote (S3) backups: 90 days

### verify_backup.sh - Backup Verification

Verify backup integrity and test restore procedures.

```bash
# Quick verification (checksums only)
./scripts/backup/verify_backup.sh latest --quick

# Full verification (checksums, manifest, file integrity)
./scripts/backup/verify_backup.sh latest --full

# Include test restore extraction
./scripts/backup/verify_backup.sh latest --test-restore
```

**Verification Levels:**
1. Quick: SHA256 checksum verification
2. Full: Checksums + manifest + file integrity
3. Test-Restore: Full + test extraction to /tmp

### cross_region_replicate.sh - Cross-Region Replication

Replicate backups to remote regions for disaster recovery.

```bash
# Replicate latest backup to all regions
./scripts/backup/cross_region_replicate.sh latest

# Async replication (background)
./scripts/backup/cross_region_replicate.sh latest --async

# Dry run
./scripts/backup/cross_region_replicate.sh latest --dry-run

# Specific regions only
./scripts/backup/cross_region_replicate.sh latest --regions=secondary,dr
```

**Replication Targets:**
- Secondary: us-west-2 (STANDARD_IA)
- DR: eu-west-1 (GLACIER)
- APAC: ap-southeast-1 (STANDARD_IA)

## Backup Catalog

Backups are cataloged in `${BACKUP_ROOT}/catalog/backups.jsonl`:
```json
{"backup_id":"erlmcp_full_20240202_120000","type":"full","timestamp":"2024-02-02T12:00:00Z","path":"/var/lib/erlmcp/backups/erlmcp_full_20240202_120000","retention_days":30}
```

## Scheduling

Use cron to schedule automated backups:

```bash
# Full backup daily at 2 AM
0 2 * * * /path/to/erlmcp/scripts/backup/backup.sh full

# Incremental backup every 6 hours
0 */6 * * * /path/to/erlmcp/scripts/backup/backup.sh incremental

# Retention policy daily at 3 AM
0 3 * * * /path/to/erlmcp/scripts/backup/backup_retention.sh apply

# Cross-region replication daily at 4 AM
0 4 * * * /path/to/erlmcp/scripts/backup/cross_region_replicate.sh latest --async
```

Or use the included Docker Compose cron job:
```bash
docker compose -f docker-compose.yml up -d backup-cron
```

## Disaster Recovery Procedure

1. **Identify the backup to restore:**
   ```bash
   ./scripts/backup/restore.sh --list
   ```

2. **Verify backup integrity:**
   ```bash
   ./scripts/backup/verify_backup.sh <backup_id> --full
   ```

3. **Stop all services:**
   ```bash
   docker compose down
   ```

4. **Restore from backup:**
   ```bash
   ./scripts/backup/restore.sh <backup_id>
   ```

5. **Verify services are healthy:**
   ```bash
   docker compose ps
   curl http://localhost:9090/health
   ```

## RPO/RTO Compliance

| Metric | Target | Achievement |
|--------|--------|-------------|
| RPO (Recovery Point Objective) | 15 minutes | Incremental backups every 6 hours |
| RTO (Recovery Time Objective) | 30 minutes | Automated restore with verification |
| Data Retention | 30 days local | Configurable per backup type |
| Cross-Region Replication | 4 hours | Async replication to 3 regions |

## Backup Receipt Format

Constitution-compliant receipts include:
- `git_sha` - Source code commit
- `image_digest` - Docker image identifier
- `backup_id` - Unique backup identifier
- `timestamp` - ISO 8601 timestamp
- `exit_code` - Backup process exit code
- `checksum_hash` - SHA256 of checksums file

Example:
```json
{
  "backup_id": "erlmcp_full_20240202_120000",
  "timestamp": "2024-02-02T12:00:00Z",
  "backup_type": "full",
  "git_sha": "abc123...",
  "image_digest": "sha256:...",
  "hostname": "erlmcp-prod-01",
  "service": "erlmcp-backup",
  "exit_code": 0,
  "checksum_hash": "def456...",
  "backup_path": "/var/lib/erlmcp/backups/erlmcp_full_20240202_120000",
  "receipt_version": "1.0"
}
```

## Troubleshooting

### Backup fails with "Docker not available"
- Ensure Docker daemon is running
- Check user has Docker permissions

### Restore fails with "Checksum mismatch"
- Backup may be corrupted
- Run `verify_backup.sh` to diagnose
- Try an older backup

### S3 upload fails
- Check AWS credentials: `aws sts get-caller-identity`
- Verify bucket exists and is accessible
- Check region matches bucket location

### Cross-region replication is slow
- Use `--async` flag for background replication
- Consider reducing number of target regions
- Check network bandwidth between regions

## Security Considerations

1. **Encryption:** Backups are encrypted using AES-256-CBC if encryption key is provided
2. **Access:** S3 buckets should have bucket policies restricting access
3. **Credentials:** Use IAM roles with least privilege
4. **Keys:** Store encryption key securely (use KMS or secret manager)
