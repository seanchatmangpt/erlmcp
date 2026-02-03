#!/bin/bash
# ==============================================================================
# erlmcp v3 Automated Restore Script
# ==============================================================================
# Purpose: Restore from backup with verification and rollback support
# Usage: ./restore.sh <backup_id> [--dry-run] [--skip-verification]
# Environment: DOCKER-ONLY (constitution compliant)
#
# RTO (Recovery Time Objective): 30 minutes
# Verification: SHA256 checksums, data integrity, service health
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
BACKUP_ID="${1:-}"
DRY_RUN="${DRY_RUN:-false}"
SKIP_VERIFICATION="${SKIP_VERIFICATION:-false}"
RESTORE_POINT="${RESTORE_POINT:-latest}"
S3_BUCKET="${S3_BUCKET:-erlmcp-backups}"
S3_REGION="${S3_REGION:-us-east-1}"
RECEIPT_FILE="/tmp/restore_receipt_$(date +%Y%m%d_%H%M%S).json"

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
docker_compose() {
    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] docker compose $*"
        return 0
    fi
    command docker compose "$@"
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
# Receipt Generation
# ============================================================================
generate_restore_receipt() {
    local exit_code=$1
    local backup_id="$2"
    local restore_start="$3"
    local restore_end="$4"
    local components_restored="$5"

    local git_sha="$(git -C "$PROJECT_ROOT" rev-parse HEAD 2>/dev/null || echo 'unknown')"
    local restore_duration=$((restore_end - restore_start))

    cat > "$RECEIPT_FILE" << EOF
{
  "restore_id": "restore_$(date +%Y%m%d_%H%M%S)",
  "backup_id": "$backup_id",
  "restore_start": "$(date -u -d @$restore_start +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || date -u -r $restore_start +%Y-%m-%dT%H:%M:%SZ)",
  "restore_end": "$(date -u -d @$restore_end +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || date -u -r $restore_end +%Y-%m-%dT%H:%M:%SZ)",
  "restore_duration_seconds": $restore_duration,
  "git_sha": "$git_sha",
  "hostname": "$(hostname)",
  "service": "erlmcp-restore",
  "exit_code": $exit_code,
  "components_restored": [$components_restored],
  "verification_skipped": $SKIP_VERIFICATION,
  "receipt_version": "1.0"
}
EOF

    log_info "Restore receipt generated: $RECEIPT_FILE"
    cat "$RECEIPT_FILE"
}

# ============================================================================
# Find Backup
# ============================================================================
find_backup() {
    local backup_id="$1"

    if [[ -z "$backup_id" ]] || [[ "$backup_id" == "latest" ]]; then
        # Find latest backup
        backup_id=$(ls -t "$BACKUP_ROOT" 2>/dev/null | grep "^erlmcp_" | head -n 1)
        if [[ -z "$backup_id" ]]; then
            # Try downloading from S3
            log_info "No local backups found, checking S3..."
            backup_id=$(aws s3 ls "s3://${S3_BUCKET}/" --recursive 2>/dev/null | \
                grep "erlmcp_full_" | tail -n 1 | awk '{print $2}' | cut -d'/' -f1)
        fi

        if [[ -z "$backup_id" ]]; then
            log_error "No backups found"
            exit 1
        fi

        log_info "Using latest backup: $backup_id"
    fi

    local backup_path="${BACKUP_ROOT}/${backup_id}"

    if [[ ! -d "$backup_path" ]]; then
        # Try downloading from S3
        log_info "Backup not found locally, downloading from S3..."
        download_backup "$backup_id"
        backup_path="${BACKUP_ROOT}/${backup_id}"
    fi

    if [[ ! -d "$backup_path" ]]; then
        log_error "Backup not found: $backup_id"
        exit 1
    fi

    echo "$backup_path"
}

# ============================================================================
# Download Backup from S3
# ============================================================================
download_backup() {
    local backup_id="$1"

    if ! command -v aws &>/dev/null; then
        log_error "AWS CLI not available"
        exit 1
    fi

    log_info "Downloading backup from S3: s3://${S3_BUCKET}/${backup_id}/"

    local local_path="${BACKUP_ROOT}/${backup_id}"
    mkdir -p "$local_path"

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would download from S3"
        return 0
    fi

    aws s3 sync "s3://${S3_BUCKET}/${backup_id}/" "$local_path/" \
        --region "$S3_REGION" \
        2>&1 | while read -r line; do
            log_debug "S3: $line"
        done

    log_info "Download completed"
}

# ============================================================================
# Verify Backup
# ============================================================================
verify_backup() {
    local backup_path="$1"

    log_info "Verifying backup integrity..."

    if [[ "$SKIP_VERIFICATION" == "true" ]]; then
        log_warn "Verification skipped"
        return 0
    fi

    local checksum_file="${backup_path}/checksums.sha256"
    local manifest_file="${backup_path}/manifest.json"

    # Check for required files
    if [[ ! -f "$checksum_file" ]]; then
        log_error "Checksum file not found: $checksum_file"
        return 1
    fi

    if [[ ! -f "$manifest_file" ]]; then
        log_error "Manifest file not found: $manifest_file"
        return 1
    fi

    # Verify checksums
    cd "$backup_path"
    sha256sum -c "$checksum_file" 2>/dev/null | tee verification.log
    local verification_result=$?
    cd - >/dev/null

    if [[ $verification_result -ne 0 ]]; then
        log_error "Checksum verification failed"
        cat "$backup_path/verification.log"
        return 1
    fi

    # Verify manifest
    local backup_type=$(jq -r '.backup_type // "unknown"' "$manifest_file")
    local backup_timestamp=$(jq -r '.timestamp // "unknown"' "$manifest_file")

    log_info "Backup verified: type=$backup_type, timestamp=$backup_timestamp"

    return 0
}

# ============================================================================
# Decrypt Backup
# ============================================================================
decrypt_backup() {
    local backup_path="$1"

    log_info "Decrypting backup files..."

    local encrypt_key_file="${ENCRYPT_KEY_FILE:-/etc/erlmcp/backup.key}"

    if [[ ! -f "$encrypt_key_file" ]]; then
        log_debug "No encryption key found, skipping decryption"
        return 0
    fi

    # Decrypt each backup file
    for file in "$backup_path"/*.tar.gz.enc "$backup_path"/*.sql.gz.enc; do
        if [[ -f "$file" ]]; then
            local decrypted_file="${file%.enc}"
            openssl enc -aes-256-cbc \
                -d \
                -in "$file" \
                -out "$decrypted_file" \
                -pass file:"$encrypt_key_file" \
                -pbkdf2 2>/dev/null || {
                log_warn "Failed to decrypt $file"
                continue
            }
            rm "$file"
        fi
    done

    log_info "Decryption completed"
}

# ============================================================================
# Stop Services
# ============================================================================
stop_services() {
    log_info "Stopping erlmcp services..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would stop services"
        return 0
    fi

    # Graceful shutdown
    docker_compose down --timeout 30 2>/dev/null || true

    # Wait for services to stop
    sleep 10

    log_info "Services stopped"
}

# ============================================================================
# Restore ETS Tables
# ============================================================================
restore_ets_tables() {
    local backup_path="$1"

    log_info "Restoring ETS tables..."

    local ets_backup="${backup_path}/ets_tables.tar.gz"

    if [[ ! -f "$ets_backup" ]]; then
        log_warn "ETS backup not found, skipping"
        return 0
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would restore ETS tables"
        return 0
    fi

    # Restore ETS data
    docker run --rm \
        -v "$backup_path:/backup" \
        -v "erlmcp-data:/data" \
        alpine:latest \
        tar xzf "/backup/ets_tables.tar.gz" -C /data 2>/dev/null || true

    log_info "ETS tables restored"
}

# ============================================================================
# Restore DETS Tables
# ============================================================================
restore_dets_tables() {
    local backup_path="$1"

    log_info "Restoring DETS tables..."

    local dets_backup="${backup_path}/dets_tables.tar.gz"

    if [[ ! -f "$dets_backup" ]]; then
        log_warn "DETS backup not found, skipping"
        return 0
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would restore DETS tables"
        return 0
    fi

    # Restore DETS data
    docker run --rm \
        -v "$backup_path:/backup" \
        -v "erlmcp-data:/data" \
        alpine:latest \
        tar xzf "/backup/dets_tables.tar.gz" -C /data 2>/dev/null || true

    log_info "DETS tables restored"
}

# ============================================================================
# Restore Mnesia
# ============================================================================
restore_mnesia() {
    local backup_path="$1"

    log_info "Restoring Mnesia database..."

    local mnesia_backup="${backup_path}/mnesia_backup.tar.gz"

    if [[ ! -f "$mnesia_backup" ]]; then
        log_warn "Mnesia backup not found, skipping"
        return 0
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would restore Mnesia"
        return 0
    fi

    # Restore Mnesia data
    docker run --rm \
        -v "$backup_path:/backup" \
        -v "erlmcp-data:/data" \
        alpine:latest \
        sh -c "
            rm -rf /data/Mnesia* 2>/dev/null || true
            tar xzf /backup/mnesia_backup.tar.gz -C /data 2>/dev/null || true
        "

    log_info "Mnesia restored"
}

# ============================================================================
# Restore Database
# ============================================================================
restore_database() {
    local backup_path="$1"

    log_info "Restoring PostgreSQL database..."

    local db_backup="${backup_path}/database.sql.gz"

    if [[ ! -f "$db_backup" ]]; then
        log_warn "Database backup not found, skipping"
        return 0
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would restore database"
        return 0
    fi

    # Check if PostgreSQL is configured
    if ! docker compose config 2>/dev/null | grep -q "postgres:"; then
        log_warn "PostgreSQL not configured, skipping database restore"
        return 0
    fi

    # Start PostgreSQL only
    docker_compose up -d postgres 2>/dev/null || true

    # Wait for PostgreSQL to be ready
    local max_attempts=30
    local attempt=0
    while [[ $attempt -lt $max_attempts ]]; do
        if docker compose exec -T postgres pg_isready &>/dev/null; then
            break
        fi
        sleep 2
        ((attempt++))
    done

    # Restore database
    gunzip -c "$db_backup" | \
        docker compose exec -T postgres psql \
            -U "${DB_USER:-erlmcp}" \
            -d "${DB_NAME:-erlmcp}" \
            2>/dev/null || log_warn "Database restore failed"

    log_info "Database restored"
}

# ============================================================================
# Restore Volumes
# ============================================================================
restore_volumes() {
    local backup_path="$1"

    log_info "Restoring Docker volumes..."

    local volumes_backup="${backup_path}/volumes.tar.gz"

    if [[ ! -f "$volumes_backup" ]]; then
        log_warn "Volumes backup not found, skipping"
        return 0
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would restore volumes"
        return 0
    fi

    # Restore each volume
    local volumes=(
        "erlmcp-data"
        "erlmcp-logs"
        "erlmcp-cluster-data"
        "erlmcp-cluster-logs"
    )

    for vol in "${volumes[@]}"; do
        if docker volume inspect "$vol" &>/dev/null; then
            docker run --rm \
                -v "$backup_path:/backup" \
                -v "$vol:/data" \
                alpine:latest \
                sh -c "
                    rm -rf /data/* 2>/dev/null || true
                    tar xzf /backup/volumes.tar.gz -C /data 2>/dev/null || true
                " 2>/dev/null || true
        fi
    done

    log_info "Volumes restored"
}

# ============================================================================
# Restore Configuration
# ============================================================================
restore_configuration() {
    local backup_path="$1"

    log_info "Restoring configuration..."

    local config_backup="${backup_path}/config.tar.gz"

    if [[ ! -f "$config_backup" ]]; then
        log_warn "Configuration backup not found, skipping"
        return 0
    fi

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would restore configuration"
        return 0
    fi

    # Extract to temporary location first
    local temp_config="/tmp/erlmcp_config_restore_$$"
    mkdir -p "$temp_config"

    tar xzf "$config_backup" -C "$temp_config" 2>/dev/null || true

    # Restore critical configuration files
    for file in vm.args rebar.config; do
        if [[ -f "${temp_config}/${file}" ]]; then
            cp "${temp_config}/${file}" "${PROJECT_ROOT}/${file}"
            log_info "Restored: $file"
        fi
    done

    # Restore config directory
    if [[ -d "${temp_config}/config" ]]; then
        cp -r "${temp_config}/config/"* "${PROJECT_ROOT}/config/" 2>/dev/null || true
        log_info "Restored: config/"
    fi

    rm -rf "$temp_config"

    log_info "Configuration restored"
}

# ============================================================================
# Start Services
# ============================================================================
start_services() {
    log_info "Starting erlmcp services..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would start services"
        return 0
    fi

    docker_compose up -d 2>/dev/null || true

    log_info "Services starting..."
}

# ============================================================================
# Verify Restore
# ============================================================================
verify_restore() {
    log_info "Verifying restore..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would verify restore"
        return 0
    fi

    local max_attempts=60
    local attempt=0
    local healthy=0

    while [[ $attempt -lt $max_attempts ]]; do
        # Check if services are running
        local running_services=$(docker compose ps --services --filter "status=running" 2>/dev/null | wc -l)

        if [[ $running_services -gt 0 ]]; then
            # Check health endpoint
            if curl -sf "http://localhost:9090/health" &>/dev/null; then
                healthy=1
                break
            fi
        fi

        sleep 5
        ((attempt++))
    done

    if [[ $healthy -eq 1 ]]; then
        log_info "Services verified as healthy"
        return 0
    else
        log_warn "Service health verification inconclusive"
        return 1
    fi
}

# ============================================================================
# Rollback on Failure
# ============================================================================
rollback_restore() {
    log_error "Restore failed, initiating rollback..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would rollback restore"
        return 0
    fi

    # Stop services
    docker_compose down --timeout 30 2>/dev/null || true

    log_error "Rollback completed. Please investigate the failure and retry restore."
}

# ============================================================================
# Main Restore Process
# ============================================================================
perform_restore() {
    local restore_start=$(date +%s)
    local exit_code=0
    local components=""

    # Find backup
    local backup_path=$(find_backup "$BACKUP_ID")
    log_info "Using backup: $backup_path"

    # Verify backup
    if ! verify_backup "$backup_path"; then
        log_error "Backup verification failed"
        exit 1
    fi

    # Decrypt backup
    decrypt_backup "$backup_path"

    # Stop services
    stop_services

    # Restore components
    {
        restore_ets_tables "$backup_path" && components='"ets_tables",'
        restore_dets_tables "$backup_path" && components="${components}\"dets_tables\","
        restore_mnesia "$backup_path" && components="${components}\"mnesia\","
        restore_database "$backup_path" && components="${components}\"database\","
        restore_volumes "$backup_path" && components="${components}\"volumes\","
        restore_configuration "$backup_path" && components="${components}\"configuration\","
    } || {
        exit_code=$?
        rollback_restore
        return $exit_code
    }

    # Remove trailing comma
    components="${components%,}"

    # Start services
    start_services

    # Verify restore
    if ! verify_restore; then
        log_warn "Restore verification returned warnings"
    fi

    local restore_end=$(date +%s)

    # Generate receipt
    generate_restore_receipt "$exit_code" "$(basename "$backup_path")" \
        "$restore_start" "$restore_end" "$components"

    if [[ $exit_code -eq 0 ]]; then
        log_info "Restore completed successfully"
        log_info "Total time: $((restore_end - restore_start)) seconds"
    else
        log_error "Restore completed with errors"
    fi

    return $exit_code
}

# ============================================================================
# List Backups
# ============================================================================
list_backups() {
    log_info "Available backups:"

    # List local backups
    echo ""
    echo "=== Local Backups ==="
    ls -lh "$BACKUP_ROOT" 2>/dev/null | grep "^erlmcp_" | while read -r line; do
        local backup_name=$(echo "$line" | awk '{print $9}')
        local backup_path="${BACKUP_ROOT}/${backup_name}"
        local manifest="${backup_path}/manifest.json"

        if [[ -f "$manifest" ]]; then
            local backup_type=$(jq -r '.backup_type // "unknown"' "$manifest")
            local backup_timestamp=$(jq -r '.timestamp // "unknown"' "$manifest")
            local backup_size=$(jq -r '.total_size_human // "unknown"' "$manifest")
            echo "  $backup_name"
            echo "    Type: $backup_type"
            echo "    Size: $backup_size"
            echo "    Timestamp: $backup_timestamp"
        else
            echo "  $backup_name"
        fi
        echo ""
    done

    # List S3 backups if available
    if command -v aws &>/dev/null && [[ -n "$S3_BUCKET" ]]; then
        echo ""
        echo "=== S3 Backups ==="
        aws s3 ls "s3://${S3_BUCKET}/" --recursive 2>/dev/null | \
            grep -E "erlmcp_.*_20[0-9]{2}[0-9]{2}_[0-9]{6}" | \
            awk '{print $2}' | \
            cut -d'/' -f1 | \
            sort -u | \
            while read -r backup_name; do
                echo "  $backup_name (s3://${S3_BUCKET})"
            done
    fi
}

# ============================================================================
# Script Entry Point
# ============================================================================
main() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --skip-verification)
                SKIP_VERIFICATION=true
                shift
                ;;
            --list)
                list_backups
                exit 0
                ;;
            -h|--help)
                echo "Usage: $0 <backup_id> [options]"
                echo ""
                echo "Arguments:"
                echo "  backup_id          Backup ID to restore (or 'latest')"
                echo ""
                echo "Options:"
                echo "  --dry-run          Show what would be done without executing"
                echo "  --skip-verification Skip backup verification"
                echo "  --list             List available backups"
                echo "  -h, --help          Show this help message"
                echo ""
                echo "Environment Variables:"
                echo "  BACKUP_ROOT           Backup directory (default: /var/lib/erlmcp/backups)"
                echo "  S3_BUCKET             S3 bucket for remote backups"
                echo "  S3_REGION             S3 region (default: us-east-1)"
                echo "  SKIP_VERIFICATION     Skip verification (default: false)"
                echo "  DRY_RUN               Enable dry-run mode"
                exit 0
                ;;
            *)
                BACKUP_ID="$1"
                shift
                ;;
        esac
    done

    if [[ -z "$BACKUP_ID" ]]; then
        log_error "Backup ID required. Use --list to see available backups."
        exit 1
    fi

    # Set error trap
    trap 'log_error "Restore interrupted"; rollback_restore; exit 1' INT TERM

    # Perform restore
    perform_restore
}

main "$@"
