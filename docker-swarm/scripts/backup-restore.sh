#!/bin/bash
# Backup and restore script for erlmcp v3 Docker Swarm
# Handles automated backups with point-in-time recovery

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
BACKUP_DIR="/mnt/erlmcp/backups"
BACKUP_RETENTION_DAYS=30
STACK_NAME="erlmcp"
S3_BUCKET="${S3_BUCKET:-erlmcp-backups}"
AWS_REGION="${AWS_REGION:-us-east-1}"
RESTORE_DIR="/tmp/restore"

# Logging function
log() {
    echo -e "${GREEN}[INFO]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $(date '+%Y-%m-%d %H:%M:%S') - $1"
    exit 1
}

# Check prerequisites
check_prerequisites() {
    if ! docker info | grep -q "Swarm: active"; then
        error "Docker Swarm is not active"
    fi

    if ! docker stack ls | grep -q "$STACK_NAME"; then
        error "Stack $STACK_NAME is not deployed"
    fi

    # Create backup directory
    mkdir -p "$BACKUP_DIR"
    mkdir -p "$RESTORE_DIR"

    log "Prerequisites verified"
}

# Perform backup
perform_backup() {
    local backup_type="${1:-full}"
    local timestamp=$(date +%Y%m%d_%H%M%S)
    local backup_name="erlmcp_backup_$timestamp"
    local backup_file="$BACKUP_DIR/$backup_name.tar.gz"

    log "Starting $backup_type backup..."

    case "$backup_type" in
        "full")
            # Stop services for consistent backup
            log "Stopping services..."
            docker stack rm "$STACK_NAME" || warn "Failed to stop services"

            # Wait for services to stop
            sleep 30

            # Backup all data volumes
            docker run --rm \
                -v "$BACKUP_DIR:/backup" \
                -v erlmcp_data:/data \
                -v erlmcp_logs:/logs \
                -v erlmcp_redis:/redis \
                alpine:latest \
                tar czf "/backup/$backup_name/data.tar.gz" -C /data . && \
                tar czf "/backup/$backup_name/logs.tar.gz" -C /logs . && \
                tar czf "/backup/$backup_name/redis.tar.gz" -C /redis .

            # Backup configurations
            docker config ls | while read -r config name; do
                docker config inspect --format '{{.Spec.Name}} {{.Spec.Data}}' "$name" | base64 -d > "$BACKUP_DIR/$backup_name/config_$name"
            done

            # Backup docker-compose files
            find . -name "docker-compose*.yml" | xargs -I {} cp {} "$BACKUP_DIR/$backup_name/"

            # Create manifest
            create_backup_manifest "$backup_name" "$backup_type"

            # Restore services
            log "Restoring services..."
            docker stack deploy -c docker-compose.yml "$STACK_NAME" || warn "Failed to restore services"

            log "Full backup completed: $backup_file"
            ;;
        "incremental")
            # Create incremental backup without stopping services
            docker run --rm \
                -v "$BACKUP_DIR:/backup" \
                -v erlmcp_data:/data \
                alpine:latest \
                tar czf "/backup/$backup_name/data_incremental.tar.gz" -C /data . --exclude=*.tmp

            # Backup recent logs only
            docker run --rm \
                -v "$BACKUP_DIR:/backup" \
                -v erlmcp_logs:/logs \
                alpine:latest \
                find /logs -name "*.log" -mtime -1 | tar -czf "/backup/$backup_name/logs_incremental.tar.gz" -T -

            log "Incremental backup completed: $backup_file"
            ;;
        "config-only")
            # Backup only configurations
            docker config ls | while read -r config name; do
                docker config inspect --format '{{.Spec.Name}} {{.Spec.Data}}' "$name" | base64 -d > "$BACKUP_DIR/$backup_name/config_$name"
            done

            # Backup service definitions
            docker service ls --filter name="$STACK_NAME" --format '{{.Name}}' | while read -r service; do
                docker service inspect "$service" --format '{{json .Spec}}' > "$BACKUP_DIR/$backup_name/service_$service.json"
            done

            log "Configuration backup completed: $backup_file"
            ;;
    esac

    # Upload to S3 if configured
    if [[ -n "$S3_BUCKET" ]]; then
        upload_to_s3 "$backup_name"
    fi

    # Cleanup old backups
    cleanup_old_backups

    log "Backup process completed"
}

# Create backup manifest
create_backup_manifest() {
    local backup_name="$1"
    local backup_type="$2"
    local manifest_file="$BACKUP_DIR/$backup_name/manifest.json"

    cat > "$manifest_file" << EOF
{
  "backup_name": "$backup_name",
  "backup_type": "$backup_type",
  "timestamp": "$(date -Iseconds)",
  "stack_name": "$STACK_NAME",
  "data_size": "$(du -sh "$BACKUP_DIR/$backup_name/data.tar.gz" | cut -f1)",
  "log_size": "$(du -sh "$BACKUP_DIR/$backup_name/logs.tar.gz" | cut -f1)",
  "redis_size": "$(du -sh "$BACKUP_DIR/$backup_name/redis.tar.gz" | cut -f1)",
  "total_size": "$(du -sh "$BACKUP_DIR/$backup_name" | cut -f1)",
  "backup_files": [
    "data.tar.gz",
    "logs.tar.gz",
    "redis.tar.gz",
    "manifest.json",
    "docker-compose.yml"
  ],
  "checksums": {
EOF

    # Calculate checksums
    for file in data.tar.gz logs.tar.gz redis.tar.gz manifest.json; do
        checksum=$(sha256sum "$BACKUP_DIR/$backup_name/$file" | cut -d' ' -f1)
        echo "\"$file\": \"$checksum\"," >> "$manifest_file"
    done

    # Remove last comma and close
    sed -i '$s/,$//' "$manifest_file"
    echo "}" >> "$manifest_file"

    log "Backup manifest created: $manifest_file"
}

# Upload to S3
upload_to_s3() {
    local backup_name="$1"

    log "Uploading backup to S3: s3://$S3_BUCKET/$backup_name/"

    # Use AWS CLI if available
    if command -v aws &> /dev/null; then
        aws s3 sync "$BACKUP_DIR/$backup_name/" "s3://$S3_BUCKET/$backup_name/" --region "$AWS_REGION"
    else
        warn "AWS CLI not available, skipping S3 upload"
    fi
}

# Restore from backup
restore_from_backup() {
    local backup_name="$1"
    local backup_type="${2:-full}"
    local restore_point="${3:-latest}"

    log "Starting restore from backup: $backup_name"

    # Find backup
    if [[ "$restore_point" == "latest" ]]; then
        backup_name=$(ls -t "$BACKUP_DIR" | grep "^erlmcp_backup_" | head -n 1)
        if [[ -z "$backup_name" ]]; then
            error "No backups found"
        fi
        backup_path="$BACKUP_DIR/$backup_name"
    else
        backup_path="$BACKUP_DIR/$backup_name"
        if [[ ! -d "$backup_path" ]]; then
            error "Backup not found: $backup_path"
        fi
    fi

    # Stop services
    log "Stopping services..."
    docker stack rm "$STACK_NAME"
    sleep 60

    # Restore data volumes
    log "Restoring data volumes..."
    if [[ -f "$backup_path/data.tar.gz" ]]; then
        docker run --rm \
            -v erlmcp_data:/data \
            -v "$backup_path:/backup" \
            alpine:latest \
            tar xzf /backup/data.tar.gz -C /data
    fi

    if [[ -f "$backup_path/redis.tar.gz" ]]; then
        docker run --rm \
            -v erlmcp_redis:/redis \
            -v "$backup_path:/backup" \
            alpine:latest \
            tar xzf /backup/redis.tar.gz -C /redis
    fi

    # Restore configurations
    if [[ -f "$backup_path/manifest.json" ]]; then
        restore_configurations "$backup_path"
    fi

    # Restore services
    log "Restoring services..."
    if [[ -f "$backup_path/docker-compose.yml" ]]; then
        docker stack deploy -c "$backup_path/docker-compose.yml" "$STACK_NAME"
    else
        docker stack deploy -c docker-compose.yml "$STACK_NAME"
    fi

    # Verify restore
    log "Verifying restore..."
    sleep 60

    if [[ "$backup_type" == "full" ]]; then
        verify_restore "$backup_path"
    fi

    log "Restore completed successfully"
}

# Restore configurations
restore_configurations() {
    local backup_path="$1"

    log "Restoring configurations..."
    ls "$backup_path" | grep "config_" | while read -r config_file; do
        local config_name=${config_file#config_}
        docker config create "$config_name" "$backup_path/$config_file" || warn "Failed to restore config: $config_name"
    done

    # Restore services
    ls "$backup_path" | grep "service_" | while read -r service_file; do
        local service_name=${service_file#service_}
        docker service create --config-from source="$service_name" "$STACK_NAME_$service_name"
    done
}

# Verify restore
verify_restore() {
    local backup_path="$1"

    log "Verifying restore..."

    # Check data integrity
    local data_integrity=$(docker run --rm \
        -v erlmcp_data:/data \
        alpine:latest \
        find /data -type f -exec sha256sum {} + | sort > "$RESTORE_DIR/current_data_hashes.txt" && \
        if [[ -f "$backup_path/hashes.txt" ]]; then
            diff "$backup_path/hashes.txt" "$RESTORE_DIR/current_data_hashes.txt" && echo "OK" || echo "DIFFERENT"
        else
            echo "NEW"
        fi)

    case "$data_integrity" in
        "OK"|"NEW")
            log "Data integrity verified: $data_integrity"
            ;;
        *)
            warn "Data integrity check failed: $data_integrity"
            ;;
    esac

    # Check service health
    for service in erlmcp-core redis-cluster; do
        if docker service ls --filter name="$service" | grep -q "$service"; then
            local attempts=0
            while [[ $attempts -lt 3 ]]; do
                if docker service logs "$service" 2>&1 | grep -q "healthy"; then
                    log "Service $service is healthy"
                    break
                fi
                sleep 30
                ((attempts++))
            done

            if [[ $attempts -eq 3 ]]; then
                warn "Service $service may not be healthy after restore"
            fi
        fi
    done
}

# List backups
list_backups() {
    log "Available backups:"

    ls -lh "$BACKUP_DIR" | grep "^erlmcp_backup_" | while read -r backup; do
        local backup_name=$(echo "$backup" | awk '{print $9}')
        local size=$(echo "$backup" | awk '{print $5}')
        local date=$(echo "$backup" | awk '{print $6" "$7" "$8}')
        local manifest="$BACKUP_DIR/$backup_name/manifest.json"

        if [[ -f "$manifest" ]]; then
            local backup_type=$(jq -r '.backup_type' "$manifest")
            local checksum=$(jq -r '.checksums.data.tar.gz' "$manifest")
            echo "  $backup_name - Size: $size - Date: $date - Type: $backup_type - Checksum: $checksum"
        else
            echo "  $backup_name - Size: $size - Date: $date - Type: unknown"
        fi
    done
}

# Cleanup old backups
cleanup_old_backups() {
    log "Cleaning up old backups..."

    # Find backups older than retention period
    find "$BACKUP_DIR" -name "erlmcp_backup_*" -mtime +$BACKUP_RETENTION_DAYS -type d | while read -r old_backup; do
        log "Removing old backup: $old_backup"
        rm -rf "$old_backup"
    done

    # Also cleanup S3 if configured
    if [[ -n "$S3_BUCKET" && command -v aws &> /dev/null ]]; then
        aws s3 ls "s3://$S3_BUCKET/" --recursive --summarize | grep "erlmcp_backup_" | while read -r line; do
            local backup_date=$(echo "$line" | awk '{print $1}')
            local backup_name=$(echo "$line" | awk '{print $4}')
            local backup_timestamp=$(echo "$backup_name" | cut -d'_' -f3)

            # Convert to timestamp and check if older than retention
            local backup_epoch=$(date -d "$backup_date" +%s 2>/dev/null || echo "0")
            local cutoff_epoch=$(date -d "$BACKUP_RETENTION_DAYS days ago" +%s)

            if [[ $backup_epoch -lt $cutoff_epoch ]]; then
                log "Removing old S3 backup: s3://$S3_BUCKET/$backup_name"
                aws s3 rm "s3://$S3_BUCKET/$backup_name" --recursive
            fi
        done
    fi

    log "Cleanup completed"
}

# Main execution
main() {
    # Parse command line arguments
    local action="${1:-}"

    case "$action" in
        "backup")
            local backup_type="${2:-full}"
            perform_backup "$backup_type"
            ;;
        "restore")
            local backup_name="${2:-}"
            local restore_type="${3:-full}"
            restore_from_backup "$backup_name" "$restore_type"
            ;;
        "list")
            list_backups
            ;;
        "cleanup")
            cleanup_old_backups
            ;;
        "")
            echo "Usage: $0 <action> [options]"
            echo "Actions:"
            echo "  backup [type]     - Perform backup (type: full, incremental, config-only)"
            echo "  restore [name]     - Restore from backup"
            echo "  list              - List available backups"
            echo "  cleanup           - Clean up old backups"
            exit 1
            ;;
        *)
            error "Unknown action: $action"
            ;;
    esac
}

# Run main function
main "$@"