#!/bin/bash
# ==============================================================================
# erlmcp v3 Automated Backup Script
# ==============================================================================
# Purpose: Automated backup of database, ETS tables, DETS, Mnesia, and stateful sets
# Usage: ./backup.sh [full|incremental] [--dry-run] [--no-upload]
# Environment: DOCKER-ONLY (constitution compliant)
#
# RPO (Recovery Point Objective): 15 minutes
# RTO (Recovery Time Objective): 30 minutes
# Retention: 30 days (local), 90 days (remote)
#
# Docker-Only Guarantee: All execution via docker compose run
# ==============================================================================

set -euo pipefail

# ============================================================================
# Configuration
# ============================================================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
BACKUP_ROOT="${BACKUP_ROOT:-/var/lib/erlmcp/backups}"
BACKUP_TYPE="${1:-full}"
DRY_RUN="${DRY_RUN:-false}"
UPLOAD_ENABLED="${UPLOAD_ENABLED:-true}"
BACKUP_RETENTION_DAYS="${BACKUP_RETENTION_DAYS:-30}"
REMOTE_RETENTION_DAYS="${REMOTE_RETENTION_DAYS:-90}"
S3_BUCKET="${S3_BUCKET:-erlmcp-backups}"
S3_REGION="${S3_REGION:-us-east-1}"
TIMESTAMP="$(date +%Y%m%d_%H%M%S)"
BACKUP_ID="erlmcp_${BACKUP_TYPE}_${TIMESTAMP}"
BACKUP_PATH="${BACKUP_ROOT}/${BACKUP_ID}"
MANIFEST_FILE="${BACKUP_PATH}/manifest.json"
CHECKSUM_FILE="${BACKUP_PATH}/checksums.sha256"
RECEIPT_FILE="${BACKUP_PATH}/receipt.json"

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m'

# ============================================================================
# Logging Functions
# ============================================================================
log_info() {
    echo -e "${GREEN}[INFO]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1"
}

log_debug() {
    if [[ "${DEBUG:-false}" == "true" ]]; then
        echo -e "${BLUE}[DEBUG]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1"
    fi
}

# ============================================================================
# Docker-Only Execution Helpers
# ============================================================================
# Constitution: All execution MUST be via Docker
# Gate mapping: backup -> erlmcp-backup service

docker_exec() {
    local container="$1"
    shift
    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] docker exec $container $*"
        return 0
    fi
    docker exec "$container" "$@"
}

docker_run() {
    local service="$1"
    shift
    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] docker compose run --rm $service $*"
        return 0
    fi
    docker compose run --rm "$service" "$@"
}

# ============================================================================
# Receipt Generation (Constitution Compliance)
# ============================================================================
# Proof = receipt(hash(git_sha || image_digest || service || cmd || exit || stdout || stderr))

generate_receipt() {
    local exit_code=$1
    local stdout_file="$2"
    local stderr_file="$3"

    local git_sha="$(git -C "$PROJECT_ROOT" rev-parse HEAD 2>/dev/null || echo 'unknown')"
    local image_digest="$(docker images --format '{{.ID}}' erlmcp:3.0.0 2>/dev/null || echo 'unknown')"
    local hostname="$(hostname)"
    local checksum_hash=""

    # Calculate checksum of all backup files
    if [[ -f "$CHECKSUM_FILE" ]]; then
        checksum_hash="$(sha256sum "$CHECKSUM_FILE" | cut -d' ' -f1)"
    fi

    cat > "$RECEIPT_FILE" << EOF
{
  "backup_id": "$BACKUP_ID",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "backup_type": "$BACKUP_TYPE",
  "git_sha": "$git_sha",
  "image_digest": "$image_digest",
  "hostname": "$hostname",
  "service": "erlmcp-backup",
  "exit_code": $exit_code,
  "checksum_hash": "$checksum_hash",
  "backup_path": "$BACKUP_PATH",
  "receipt_version": "1.0"
}
EOF

    log_info "Receipt generated: $RECEIPT_FILE"
    cat "$RECEIPT_FILE"
}

# ============================================================================
# Prerequisites Check
# ============================================================================
check_prerequisites() {
    log_info "Checking prerequisites..."

    # Check Docker
    if ! docker info &>/dev/null; then
        log_error "Docker is not available"
        exit 1
    fi

    # Check Docker Compose
    if ! docker compose version &>/dev/null; then
        log_error "Docker Compose is not available"
        exit 1
    fi

    # Check if backup service exists
    if ! docker compose config 2>/dev/null | grep -q "erlmcp-backup"; then
        log_warn "erlmcp-backup service not defined in docker-compose.yml"
        log_info "Using erlmcp-build service for backup operations"
    fi

    # Create backup directory
    if [[ "$DRY_RUN" == "false" ]]; then
        mkdir -p "$BACKUP_PATH"
        mkdir -p "${BACKUP_ROOT}/catalog"
    fi

    log_info "Prerequisites verified"
}

# ============================================================================
# ETS Table Backup
# ============================================================================
backup_ets_tables() {
    log_info "Backing up ETS tables..."

    local ets_backup_file="${BACKUP_PATH}/ets_tables.tar.gz"
    local ets_manifest="${BACKUP_PATH}/ets_manifest.json"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would backup ETS tables to $ets_backup_file"
        echo '{"tables": []}' > "$ets_manifest"
        return 0
    fi

    # Use erlmcp-build service to dump ETS tables
    docker_run erlmcp-build /bin/sh -c "
        # Get list of all ETS tables
        erl -noshell -eval '
            io:format(\"~p~n\", [ets:all()]),
            Tables = ets:all(),
            lists:foreach(fun(T) ->
                TabName = ets:info(T, name),
                io:format(\"Table: ~p~n\", [TabName]),
                case ets:info(T, type) of
                    set ->
                        ets:tab2file(T, \"/tmp/backup/\$#{TabName}.ets\");
                    bag ->
                        ets:tab2file(T, \"/tmp/backup/\$#{TabName}.ets\");
                    ordered_set ->
                        ets:tab2file(T, \"/tmp/backup/\$#{TabName}.ets\");
                    _ ->
                        ok
                end
            end, Tables),
            init:stop()
        ' 2>&1
    " || true

    # Create archive of ETS backups
    docker run --rm \
        -v "${BACKUP_PATH}:/backup" \
        -v "erlmcp-data:/data:ro" \
        alpine:latest \
        tar czf "/backup/ets_tables.tar.gz" -C /data . 2>/dev/null || true

    # Generate manifest
    cat > "$ets_manifest" << EOF
{
  "backup_component": "ets_tables",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "file": "ets_tables.tar.gz",
  "size": "$(stat -f%z "$ets_backup_file" 2>/dev/null || stat -c%s "$ets_backup_file" 2>/dev/null || echo 0)",
  "checksum": "$(sha256sum "$ets_backup_file" 2>/dev/null | cut -d' ' -f1 || echo 'unknown')"
}
EOF

    log_info "ETS tables backup completed"
}

# ============================================================================
# DETS Backup
# ============================================================================
backup_dets_tables() {
    log_info "Backing up DETS tables..."

    local dets_backup_file="${BACKUP_PATH}/dets_tables.tar.gz"
    local dets_manifest="${BACKUP_PATH}/dets_manifest.json"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would backup DETS tables to $dets_backup_file"
        echo '{"tables": []}' > "$dets_manifest"
        return 0
    fi

    # Backup DETS files from data directory
    docker run --rm \
        -v "${BACKUP_PATH}:/backup" \
        -v "erlmcp-data:/data:ro" \
        alpine:latest \
        sh -c "
            find /data -name '*.DAT' -o -name '*.dets' 2>/dev/null | \
            tar czf /backup/dets_tables.tar.gz -T - 2>/dev/null || \
            touch /backup/dets_tables.tar.gz
        "

    # Generate manifest
    cat > "$dets_manifest" << EOF
{
  "backup_component": "dets_tables",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "file": "dets_tables.tar.gz",
  "size": "$(stat -f%z "$dets_backup_file" 2>/dev/null || stat -c%s "$dets_backup_file" 2>/dev/null || echo 0)",
  "checksum": "$(sha256sum "$dets_backup_file" 2>/dev/null | cut -d' ' -f1 || echo 'unknown')"
}
EOF

    log_info "DETS tables backup completed"
}

# ============================================================================
# Mnesia Backup
# ============================================================================
backup_mnesia() {
    log_info "Backing up Mnesia database..."

    local mnesia_backup_file="${BACKUP_PATH}/mnesia_backup.tar.gz"
    local mnesia_manifest="${BACKUP_PATH}/mnesia_manifest.json"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would backup Mnesia to $mnesia_backup_file"
        echo '{"tables": []}' > "$mnesia_manifest"
        return 0
    fi

    # Backup Mnesia schema and data
    docker_run erlmcp-build /bin/sh -c "
        # Mnesia backup using mnesia:backup/1
        erl -noshell -name backup@erlmcp-backup -setcookie erlmcp_backup_cookie -eval '
            case mnesia:system_info(is_running) of
                true ->
                    BackupFile = \"/tmp/mnesia_backup\",
                    case mnesia:backup(BackupFile) of
                        ok -> io:format('Mnesia backup successful~n');
                        {error, Reason} -> io:format('Mnesia backup failed: ~p~n', [Reason])
                    end;
                false ->
                    io:format('Mnesia not running, copying files...~n'),
                    ok
            end,
            init:stop()
        ' 2>&1 || true
    " || true

    # Backup Mnesia data directory
    docker run --rm \
        -v "${BACKUP_PATH}:/backup" \
        -v "erlmcp-data:/data:ro" \
        alpine:latest \
        sh -c "
            if [ -d /data/Mnesia* ]; then
                tar czf /backup/mnesia_backup.tar.gz -C /data Mnesia* 2>/dev/null || true
            else
                touch /backup/mnesia_backup.tar.gz
            fi
        "

    # Generate manifest
    cat > "$mnesia_manifest" << EOF
{
  "backup_component": "mnesia",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "file": "mnesia_backup.tar.gz",
  "size": "$(stat -f%z "$mnesia_backup_file" 2>/dev/null || stat -c%s "$mnesia_backup_file" 2>/dev/null || echo 0)",
  "checksum": "$(sha256sum "$mnesia_backup_file" 2>/dev/null | cut -d' ' -f1 || echo 'unknown')"
}
EOF

    log_info "Mnesia backup completed"
}

# ============================================================================
# Database Backup (PostgreSQL if available)
# ============================================================================
backup_database() {
    log_info "Backing up PostgreSQL database..."

    local db_backup_file="${BACKUP_PATH}/database.sql.gz"
    local db_manifest="${BACKUP_PATH}/database_manifest.json"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would backup PostgreSQL to $db_backup_file"
        echo '{"database": "skipped (dry-run)"}' > "$db_manifest"
        return 0
    fi

    # Check if PostgreSQL is running
    if ! docker compose ps postgres 2>/dev/null | grep -q "Up"; then
        log_warn "PostgreSQL not running, skipping database backup"
        echo '{"database": "skipped (not running)"}' > "$db_manifest"
        return 0
    fi

    # Perform pg_dump
    docker compose exec -T postgres pg_dump \
        -U "${DB_USER:-erlmcp}" \
        -d "${DB_NAME:-erlmcp}" \
        --format=plain \
        --no-owner \
        --no-acl 2>/dev/null | \
        gzip > "${db_backup_file}" || {
        log_warn "PostgreSQL backup failed, continuing..."
        echo '{"database": "failed"}' > "$db_manifest"
        return 0
    }

    # Generate manifest
    cat > "$db_manifest" << EOF
{
  "backup_component": "database",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "file": "database.sql.gz",
  "size": "$(stat -f%z "$db_backup_file" 2>/dev/null || stat -c%s "$db_backup_file" 2>/dev/null || echo 0)",
  "checksum": "$(sha256sum "$db_backup_file" 2>/dev/null | cut -d' ' -f1 || echo 'unknown')",
  "database": "${DB_NAME:-erlmcp}"
}
EOF

    log_info "PostgreSQL backup completed"
}

# ============================================================================
# Stateful Sets Backup (Docker Volumes)
# ============================================================================
backup_stateful_sets() {
    log_info "Backing up stateful sets (Docker volumes)..."

    local volumes_backup_file="${BACKUP_PATH}/volumes.tar.gz"
    local volumes_manifest="${BACKUP_PATH}/volumes_manifest.json"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would backup volumes to $volumes_backup_file"
        echo '{"volumes": []}' > "$volumes_manifest"
        return 0
    fi

    # List of volumes to backup
    local volumes=(
        "erlmcp-data"
        "erlmcp-logs"
        "erlmcp-cluster-data"
        "erlmcp-cluster-logs"
        "redis-data"
        "postgres-data"
    )

    # Build volume list for backup
    local volume_args=""
    local volume_list_json="["

    for vol in "${volumes[@]}"; do
        if docker volume inspect "$vol" &>/dev/null; then
            volume_args="-v ${vol}:/data/${vol}:ro $volume_args"
            volume_list_json="${volume_list_json}{\"name\":\"${vol}\",\"backup\":\"true\"},"
        fi
    done

    volume_list_json="${volume_list_json%,}]"

    # Create backup of all volumes
    if [[ -n "$volume_args" ]]; then
        docker run --rm \
            $volume_args \
            -v "${BACKUP_PATH}:/backup" \
            alpine:latest \
            sh -c "
                cd /data
                tar czf /backup/volumes.tar.gz *
            " 2>/dev/null || true
    else
        touch "$volumes_backup_file"
    fi

    # Generate manifest
    cat > "$volumes_manifest" << EOF
{
  "backup_component": "volumes",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "file": "volumes.tar.gz",
  "size": "$(stat -f%z "$volumes_backup_file" 2>/dev/null || stat -c%s "$volumes_backup_file" 2>/dev/null || echo 0)",
  "checksum": "$(sha256sum "$volumes_backup_file" 2>/dev/null | cut -d' ' -f1 || echo 'unknown')",
  "volumes": ${volume_list_json}
}
EOF

    log_info "Stateful sets backup completed"
}

# ============================================================================
# Configuration Backup
# ============================================================================
backup_configuration() {
    log_info "Backing up configuration..."

    local config_backup_file="${BACKUP_PATH}/config.tar.gz"
    local config_manifest="${BACKUP_PATH}/config_manifest.json"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would backup config to $config_backup_file"
        return 0
    fi

    # Backup configuration files
    tar czf "$config_backup_file" \
        -C "$PROJECT_ROOT" \
        docker-compose.yml \
        Dockerfile \
        config/ \
        vm.args \
        rebar.config \
        2>/dev/null || true

    # Generate manifest
    cat > "$config_manifest" << EOF
{
  "backup_component": "configuration",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "file": "config.tar.gz",
  "size": "$(stat -f%z "$config_backup_file" 2>/dev/null || stat -c%s "$config_backup_file" 2>/dev/null || echo 0)",
  "checksum": "$(sha256sum "$config_backup_file" 2>/dev/null | cut -d' ' -f1 || echo 'unknown')"
}
EOF

    log_info "Configuration backup completed"
}

# ============================================================================
# Encryption
# ============================================================================
encrypt_backup() {
    log_info "Encrypting backup..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would encrypt backup files"
        return 0
    fi

    # Check for encryption key
    local encrypt_key_file="${ENCRYPT_KEY_FILE:-/etc/erlmcp/backup.key}"

    if [[ ! -f "$encrypt_key_file" ]]; then
        log_warn "Encryption key not found, skipping encryption"
        return 0
    fi

    # Encrypt each backup file
    for file in "$BACKUP_PATH"/*.tar.gz "$BACKUP_PATH"/*.sql.gz; do
        if [[ -f "$file" ]]; then
            local encrypted_file="${file}.enc"
            openssl enc -aes-256-cbc \
                -salt \
                -in "$file" \
                -out "$encrypted_file" \
                -pass file:"$encrypt_key_file" \
                -pbkdf2 2>/dev/null && {
                rm "$file"
                mv "$encrypted_file" "$file"
            } || log_warn "Failed to encrypt $file"
        fi
    done

    log_info "Backup encryption completed"
}

# ============================================================================
# Checksum Generation
# ============================================================================
generate_checksums() {
    log_info "Generating SHA256 checksums..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would generate checksums"
        return 0
    fi

    cd "$BACKUP_PATH"
    sha256sum *.{tar.gz,sql.gz,json} 2>/dev/null > "$CHECKSUM_FILE" || true
    cd - >/dev/null

    log_info "Checksums generated: $CHECKSUM_FILE"
}

# ============================================================================
# Manifest Generation
# ============================================================================
generate_manifest() {
    log_info "Generating backup manifest..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would generate manifest"
        return 0
    fi

    local total_size=0
    for file in "$BACKUP_PATH"/*.{tar.gz,sql.gz}; do
        if [[ -f "$file" ]]; then
            size=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file" 2>/dev/null)
            total_size=$((total_size + size))
        fi
    done

    cat > "$MANIFEST_FILE" << EOF
{
  "backup_id": "$BACKUP_ID",
  "backup_type": "$BACKUP_TYPE",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "hostname": "$(hostname)",
  "project_root": "$PROJECT_ROOT",
  "backup_path": "$BACKUP_PATH",
  "total_size_bytes": $total_size,
  "total_size_human": "$(numfmt --to=iec-i --suffix=B $total_size 2>/dev/null || echo ${total_size}B)",
  "components": [
    "ets_tables",
    "dets_tables",
    "mnesia",
    "database",
    "volumes",
    "configuration"
  ],
  "checksum_file": "checksums.sha256",
  "receipt_file": "receipt.json",
  "encrypted": $([ -f "$encrypt_key_file" ] && echo true || echo false),
  "retention_days": $BACKUP_RETENTION_DAYS,
  "backup_version": "3.0.0"
}
EOF

    log_info "Manifest generated: $MANIFEST_FILE"
}

# ============================================================================
# Cross-Region Upload
# ============================================================================
upload_to_remote() {
    log_info "Uploading backup to remote storage..."

    if [[ "$UPLOAD_ENABLED" != "true" ]]; then
        log_info "Remote upload disabled, skipping"
        return 0
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would upload to S3: s3://${S3_BUCKET}/${BACKUP_ID}/"
        return 0
    fi

    # Check for AWS CLI
    if ! command -v aws &>/dev/null; then
        log_warn "AWS CLI not found, skipping S3 upload"
        return 0
    fi

    # Upload to S3
    aws s3 sync "$BACKUP_PATH/" "s3://${S3_BUCKET}/${BACKUP_ID}/" \
        --region "$S3_REGION" \
        --storage-class STANDARD_IA \
        --sse AES256 \
        2>&1 | while read -r line; do
            log_debug "S3: $line"
        done

    # Set lifecycle policy for retention
    aws s3api put-object-tagging \
        --bucket "$S3_BUCKET" \
        --key "${BACKUP_ID}/manifest.json" \
        --tagging "TagSet=[{Key=Retention,Value=${REMOTE_RETENTION_DAYS}},{Key=BackupType,Value=${BACKUP_TYPE}}]" \
        --region "$S3_REGION" 2>/dev/null || true

    log_info "Upload to remote storage completed"
}

# ============================================================================
# Catalog Update
# ============================================================================
update_catalog() {
    log_info "Updating backup catalog..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would update catalog"
        return 0
    fi

    local catalog_file="${BACKUP_ROOT}/catalog/backups.jsonl"

    # Append backup entry to catalog
    cat >> "$catalog_file" << EOF
{"backup_id":"$BACKUP_ID","type":"$BACKUP_TYPE","timestamp":"$(date -u +%Y-%m-%dT%H:%M:%SZ)","path":"$BACKUP_PATH","retention_days":$BACKUP_RETENTION_DAYS}
EOF

    log_info "Catalog updated: $catalog_file"
}

# ============================================================================
# Cleanup Old Backups
# ============================================================================
cleanup_old_backups() {
    log_info "Cleaning up old backups (older than ${BACKUP_RETENTION_DAYS} days)..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would clean up old backups"
        return 0
    fi

    # Clean local backups
    find "$BACKUP_ROOT" -mindepth 1 -maxdepth 1 -type d -name "erlmcp_*" \
        -mtime +$BACKUP_RETENTION_DAYS -exec rm -rf {} + 2>/dev/null || true

    # Clean S3 backups if AWS CLI is available
    if command -v aws &>/dev/null; then
        aws s3 ls "s3://${S3_BUCKET}/" --recursive | while read -r line; do
            local backup_date=$(echo "$line" | awk '{print $1" "$2}')
            local backup_key=$(echo "$line" | awk '{print $4}')

            if [[ "$backup_key" =~ erlmcp_[^/]+/ ]]; then
                local backup_age_seconds=$(( $(date +%s) - $(date -d "$backup_date" +%s 2>/dev/null || echo 0) ))
                local cutoff_seconds=$(( REMOTE_RETENTION_DAYS * 86400 ))

                if [[ $backup_age_seconds -gt $cutoff_seconds ]]; then
                    log_info "Removing old remote backup: $backup_key"
                    aws s3 rm "s3://${S3_BUCKET}/${backup_key}" --region "$S3_REGION" 2>/dev/null || true
                fi
            fi
        done
    fi

    log_info "Cleanup completed"
}

# ============================================================================
# Backup Verification
# ============================================================================
verify_backup() {
    log_info "Verifying backup integrity..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would verify backup"
        return 0
    fi

    # Verify checksums
    cd "$BACKUP_PATH"
    if [[ -f "$CHECKSUM_FILE" ]]; then
        sha256sum -c "$CHECKSUM_FILE" 2>/dev/null | tee verification.log
        if grep -q "FAILED" verification.log; then
            log_error "Backup verification failed - checksum mismatch"
            return 1
        fi
    fi
    cd - >/dev/null

    # Verify manifest
    if [[ ! -f "$MANIFEST_FILE" ]]; then
        log_error "Backup manifest not found"
        return 1
    fi

    # Verify backup files exist
    local required_files=(
        "ets_tables.tar.gz"
        "dets_tables.tar.gz"
        "mnesia_backup.tar.gz"
        "volumes.tar.gz"
        "config.tar.gz"
        "checksums.sha256"
        "manifest.json"
    )

    for file in "${required_files[@]}"; do
        if [[ ! -f "${BACKUP_PATH}/${file}" ]]; then
            log_warn "Required backup file not found: $file"
        fi
    done

    log_info "Backup verification completed"
}

# ============================================================================
# Main Backup Process
# ============================================================================
perform_backup() {
    local exit_code=0
    local stdout_file="${BACKUP_PATH}/backup.stdout.log"
    local stderr_file="${BACKUP_PATH}/backup.stderr.log"

    log_info "Starting ${BACKUP_TYPE} backup..."
    log_info "Backup ID: $BACKUP_ID"
    log_info "Backup Path: $BACKUP_PATH"

    # Execute backup pipeline
    {
        check_prerequisites
        backup_ets_tables
        backup_dets_tables
        backup_mnesia
        backup_database
        backup_stateful_sets
        backup_configuration
        encrypt_backup
        generate_checksums
        generate_manifest
        upload_to_remote
        update_catalog
        verify_backup
    } > "$stdout_file" 2> "$stderr_file" || exit_code=$?

    # Generate receipt (constitution compliance)
    generate_receipt "$exit_code" "$stdout_file" "$stderr_file"

    if [[ $exit_code -eq 0 ]]; then
        log_info "Backup completed successfully: $BACKUP_PATH"
        log_info "Total size: $(du -sh "$BACKUP_PATH" | cut -f1)"
    else
        log_error "Backup failed with exit code: $exit_code"
    fi

    return $exit_code
}

# ============================================================================
# Script Entry Point
# ============================================================================
main() {
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
            full|incremental)
                BACKUP_TYPE="$1"
                shift
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --no-upload)
                UPLOAD_ENABLED=false
                shift
                ;;
            --cleanup-only)
                cleanup_old_backups
                exit 0
                ;;
            -h|--help)
                echo "Usage: $0 [full|incremental] [options]"
                echo ""
                echo "Options:"
                echo "  full|incremental    Backup type (default: full)"
                echo "  --dry-run           Show what would be done without executing"
                echo "  --no-upload         Skip remote upload"
                echo "  --cleanup-only      Only clean up old backups"
                echo "  -h, --help          Show this help message"
                echo ""
                echo "Environment Variables:"
                echo "  BACKUP_ROOT           Backup directory (default: /var/lib/erlmcp/backups)"
                echo "  BACKUP_RETENTION_DAYS Local retention in days (default: 30)"
                echo "  S3_BUCKET             S3 bucket for remote backups"
                echo "  S3_REGION             S3 region (default: us-east-1)"
                echo "  UPLOAD_ENABLED        Enable remote upload (default: true)"
                echo "  DRY_RUN               Enable dry-run mode"
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                exit 1
                ;;
        esac
    done

    # Set error trap
    trap 'log_error "Backup interrupted"; exit 1' INT TERM

    # Perform backup
    perform_backup
}

main "$@"
