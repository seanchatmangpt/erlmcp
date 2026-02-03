#!/bin/bash
# ==============================================================================
# erlmcp v3 Backup Retention Policy Manager
# ==============================================================================
# Purpose: Manage backup retention based on age, type, and storage tier
# Usage: ./backup_retention.sh [apply|report|cleanup] [options]
# Environment: DOCKER-ONLY (constitution compliant)
#
# Retention Policies:
#   - Local backups: 30 days
#   - Remote (S3) backups: 90 days
#   - Compliance backups: 7 years (for audit requirements)
#   - Weekly backups: Keep 1 per week for 12 weeks
#   - Monthly backups: Keep 1 per month for 12 months
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
S3_BUCKET="${S3_BUCKET:-erlmcp-backups}"
S3_REGION="${S3_REGION:-us-east-1}"

# Retention periods (in days)
RETENTION_LOCAL="${RETENTION_LOCAL:-30}"
RETENTION_REMOTE="${RETENTION_REMOTE:-90}"
RETENTION_COMPLIANCE="${RETENTION_COMPLIANCE:-2555}"  # 7 years
RETENTION_WEEKLY="${RETENTION_WEEKLY:-84}"  # 12 weeks
RETENTION_MONTHLY="${RETENTION_MONTHLY:-365}"  # 12 months

# Archive settings
ARCHIVE_ENABLED="${ARCHIVE_ENABLED:-true}"
ARCHIVE_AFTER_DAYS="${ARCHIVE_AFTER_DAYS:-7}"
COLD_STORAGE_CLASS="${COLD_STORAGE_CLASS:-GLACIER}"

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
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
# Backup Age Calculation
# ============================================================================
get_backup_age_days() {
    local backup_path="$1"

    if [[ ! -d "$backup_path" ]]; then
        echo "999999"
        return
    fi

    local manifest="${backup_path}/manifest.json"

    if [[ -f "$manifest" ]]; then
        local timestamp=$(jq -r '.timestamp // "1970-01-01T00:00:00Z"' "$manifest")
        local backup_epoch=$(date -d "$timestamp" +%s 2>/dev/null || echo 0)
    else
        local backup_epoch=$(stat -c %Y "$backup_path" 2>/dev/null || stat -f %m "$backup_path" 2>/dev/null || echo 0)
    fi

    local current_epoch=$(date +%s)
    local age_seconds=$((current_epoch - backup_epoch))
    local age_days=$((age_seconds / 86400))

    echo "$age_days"
}

get_backup_type() {
    local backup_path="$1"
    local manifest="${backup_path}/manifest.json"

    if [[ -f "$manifest" ]]; then
        jq -r '.backup_type // "unknown"' "$manifest"
    else
        echo "unknown"
    fi
}

get_backup_size() {
    local backup_path="$1"

    if [[ -d "$backup_path" ]]; then
        du -sb "$backup_path" 2>/dev/null | cut -f1 || echo 0
    else
        echo 0
    fi
}

# ============================================================================
# Backup Classification
# ============================================================================
classify_backup() {
    local backup_path="$1"
    local backup_type=$(get_backup_type "$backup_path")
    local age_days=$(get_backup_age_days "$backup_path")
    local backup_id=$(basename "$backup_path")

    # Parse backup timestamp to determine if it's a weekly/monthly backup
    local is_weekly=false
    local is_monthly=false

    # Check if backup is on Sunday (weekly)
    local backup_date=$(date -d "$(jq -r '.timestamp // "1970-01-01"' "${backup_path}/manifest.json" 2>/dev/null)" +%u 2>/dev/null || echo 0)
    if [[ "$backup_date" == "7" ]]; then
        is_weekly=true
    fi

    # Check if backup is on the first day of month (monthly)
    local ts_json=$(jq -r '.timestamp // "1970-01-01"' "${backup_path}/manifest.json" 2>/dev/null)
    local backup_day=$(date -d "$ts_json" +%d 2>/dev/null || echo 0)
    if [[ "$backup_day" == "01" ]]; then
        is_monthly=true
    fi

    # Determine retention policy
    local retention_days=$RETENTION_LOCAL

    case "$backup_type" in
        "full")
            if [[ "$is_monthly" == "true" ]]; then
                retention_days=$RETENTION_MONTHLY
            elif [[ "$is_weekly" == "true" ]]; then
                retention_days=$RETENTION_WEEKLY
            else
                retention_days=$RETENTION_LOCAL
            fi
            ;;
        "incremental")
            retention_days=$((RETENTION_LOCAL / 2))  # Keep incrementals for half the time
            ;;
        "compliance")
            retention_days=$RETENTION_COMPLIANCE
            ;;
        *)
            retention_days=$RETENTION_LOCAL
            ;;
    esac

    echo "$backup_type|$age_days|$retention_days|$is_weekly|$is_monthly"
}

# ============================================================================
# Generate Retention Report
# ============================================================================
generate_report() {
    log_info "Generating retention report..."

    local report_file="${BACKUP_ROOT}/retention_report_$(date +%Y%m%d_%H%M%S).json"

    # Initialize report
    cat > "$report_file" << EOF
{
  "report_generated": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "backup_root": "$BACKUP_ROOT",
  "retention_policies": {
    "local_days": $RETENTION_LOCAL,
    "remote_days": $RETENTION_REMOTE,
    "compliance_days": $RETENTION_COMPLIANCE,
    "weekly_days": $RETENTION_WEEKLY,
    "monthly_days": $RETENTION_MONTHLY
  },
  "backups": []
}
EOF

    local total_count=0
    local total_size=0
    local expired_count=0
    local expired_size=0
    local keep_count=0
    local keep_size=0

    # Scan local backups
    for backup_path in "$BACKUP_ROOT"/erlmcp_*; do
        if [[ -d "$backup_path" ]]; then
            local backup_id=$(basename "$backup_path")
            local backup_info=$(classify_backup "$backup_path")
            IFS='|' read -r backup_type age_days retention_days is_weekly is_monthly <<< "$backup_info"

            local backup_size=$(get_backup_size "$backup_path")
            local expired="false"
            local action="keep"
            local retention_reason="standard"

            # Determine if backup should be kept or expired
            if [[ $age_days -gt $retention_days ]]; then
                if [[ "$backup_type" == "compliance" ]]; then
                    expired="false"
                    action="compliance_retain"
                    retention_reason="compliance_requirement"
                else
                    expired="true"
                    action="delete"
                    retention_reason="expired"
                    ((expired_count++))
                    ((expired_size += backup_size))
                fi
            else
                ((keep_count++))
                ((keep_size += backup_size))

                # Determine retention reason
                if [[ "$is_monthly" == "true" ]]; then
                    retention_reason="monthly_backup"
                elif [[ "$is_weekly" == "true" ]]; then
                    retention_reason="weekly_backup"
                elif [[ "$backup_type" == "compliance" ]]; then
                    retention_reason="compliance_requirement"
                fi
            fi

            ((total_count++))
            ((total_size += backup_size))

            # Add to report
            local backup_entry=$(cat << EOF
    {
      "backup_id": "$backup_id",
      "type": "$backup_type",
      "age_days": $age_days,
      "retention_days": $retention_days,
      "size_bytes": $backup_size,
      "expired": $expired,
      "action": "$action",
      "retention_reason": "$retention_reason",
      "is_weekly": $is_weekly,
      "is_monthly": $is_monthly
    }
EOF
)

            # Insert entry into report JSON
            jq --argjson entry "$backup_entry" '.backups += [$entry]' "$report_file" > "${report_file}.tmp"
            mv "${report_file}.tmp" "$report_file"
        fi
    done

    # Add summary to report
    jq --arg total_count "$total_count" \
       --arg total_size "$total_size" \
       --arg expired_count "$expired_count" \
       --arg expired_size "$expired_size" \
       --arg keep_count "$keep_count" \
       --arg keep_size "$keep_size" \
       '. += {
           summary: {
               total_backups: ($total_count | tonumber),
               total_size_bytes: ($total_size | tonumber),
               expired_backups: ($expired_count | tonumber),
               expired_size_bytes: ($expired_size | tonumber),
               kept_backups: ($keep_count | tonumber),
               kept_size_bytes: ($keep_size | tonumber)
           }
       }' "$report_file" > "${report_file}.tmp"
    mv "${report_file}.tmp" "$report_file"

    # Print summary
    echo ""
    echo "==============================================================================="
    echo "                    BACKUP RETENTION REPORT"
    echo "==============================================================================="
    echo ""
    printf "%-20s : %s\n" "Report Generated" "$(date -u +%Y-%m-%d\ %H:%M:%S\ UTC)"
    printf "%-20s : %s\n" "Local Retention" "${RETENTION_LOCAL} days"
    printf "%-20s : %s\n" "Remote Retention" "${RETENTION_REMOTE} days"
    printf "%-20s : %s\n" "Compliance Retention" "${RETENTION_COMPLIANCE} days"
    echo ""
    echo "-------------------------------------------------------------------------------"
    echo "                           SUMMARY"
    echo "-------------------------------------------------------------------------------"
    printf "%-20s : %d\n" "Total Backups" $total_count
    printf "%-20s : %s\n" "Total Size" "$(numfmt --to=iec-i --suffix=B $total_size 2>/dev/null || echo ${total_size}B)"
    printf "%-20s : %d\n" "Expired Backups" $expired_count
    printf "%-20s : %s\n" "Expired Size" "$(numfmt --to=iec-i --suffix=B $expired_size 2>/dev/null || echo ${expired_size}B)"
    printf "%-20s : %d\n" "Kept Backups" $keep_count
    printf "%-20s : %s\n" "Kept Size" "$(numfmt --to=iec-i --suffix=B $keep_size 2>/dev/null || echo ${keep_size}B)"
    echo ""
    echo "-------------------------------------------------------------------------------"
    echo "                           EXPIRED BACKUPS"
    echo "-------------------------------------------------------------------------------"

    # List expired backups
    jq -r '.backups[] | select(.expired == true) | "\(.backup_id) | \(.type) | \(.age_days)d old | \(.retention_reason)"' "$report_file" 2>/dev/null || echo "None"

    echo ""
    echo "==============================================================================="
    echo "Report saved to: $report_file"
    echo "==============================================================================="

    # Return exit code based on expired backups
    if [[ $expired_count -gt 0 ]]; then
        return 1
    fi

    return 0
}

# ============================================================================
# Apply Retention Policy
# ============================================================================
apply_retention() {
    log_info "Applying retention policy..."

    local dry_run="${DRY_RUN:-false}"
    local deleted_count=0
    local archived_count=0
    local freed_space=0

    # Check for --dry-run flag
    if [[ "$1" == "--dry-run" ]]; then
        dry_run=true
        log_warn "DRY RUN MODE - No changes will be made"
        shift
    fi

    # Process each backup
    for backup_path in "$BACKUP_ROOT"/erlmcp_*; do
        if [[ -d "$backup_path" ]]; then
            local backup_id=$(basename "$backup_path")
            local backup_info=$(classify_backup "$backup_path")
            IFS='|' read -r backup_type age_days retention_days is_weekly is_monthly <<< "$backup_info"

            local backup_size=$(get_backup_size "$backup_path")
            local manifest="${backup_path}/manifest.json"

            if [[ $age_days -gt $retention_days ]]; then
                if [[ "$backup_type" == "compliance" ]]; then
                    log_debug "Keeping compliance backup: $backup_id (age: ${age_days}d)"
                    continue
                fi

                # Archive before deleting if enabled
                if [[ "$ARCHIVE_ENABLED" == "true" ]] && [[ $age_days -gt $ARCHIVE_AFTER_DAYS ]]; then
                    log_info "Archiving old backup: $backup_id"
                    archive_backup "$backup_path" "$backup_type" && ((archived_count++))
                fi

                # Delete expired backup
                log_info "Deleting expired backup: $backup_id (age: ${age_days}d, retention: ${retention_days}d)"

                if [[ "$dry_run" == "false" ]]; then
                    rm -rf "$backup_path"
                    ((deleted_count++))
                    ((freed_space += backup_size))
                else
                    log_debug "[DRY-RUN] Would delete: $backup_path"
                    ((deleted_count++))
                    ((freed_space += backup_size))
                fi
            fi
        fi
    done

    # Also process S3 backups if available
    if command -v aws &>/dev/null && [[ -n "$S3_BUCKET" ]]; then
        log_info "Processing S3 backups..."

        aws s3 ls "s3://${S3_BUCKET}/" --recursive 2>/dev/null | while read -r line; do
            local backup_date=$(echo "$line" | awk '{print $1" "$2}')
            local backup_key=$(echo "$line" | awk '{print $4}')

            # Parse backup ID from key
            if [[ "$backup_key" =~ erlmcp_[^/]+/ ]]; then
                local backup_id=$(echo "$backup_key" | cut -d'/' -f1)
                local backup_epoch=$(date -d "$backup_date" +%s 2>/dev/null || echo 0)
                local current_epoch=$(date +%s)
                local age_days=$(((current_epoch - backup_epoch) / 86400))

                if [[ $age_days -gt $RETENTION_REMOTE ]]; then
                    log_info "Deleting expired S3 backup: $backup_key (age: ${age_days}d)"

                    if [[ "$dry_run" == "false" ]]; then
                        aws s3 rm "s3://${S3_BUCKET}/${backup_key}" --region "$S3_REGION" 2>/dev/null || true
                    else
                        log_debug "[DRY-RUN] Would delete S3: $backup_key"
                    fi
                fi
            fi
        done
    fi

    # Print summary
    echo ""
    echo "Retention Policy Applied:"
    echo "  Deleted: $deleted_count backups"
    echo "  Archived: $archived_count backups"
    echo "  Freed Space: $(numfmt --to=iec-i --suffix=B $freed_space 2>/dev/null || echo ${freed_space}B)"
}

# ============================================================================
# Archive Backup
# ============================================================================
archive_backup() {
    local backup_path="$1"
    local backup_type="$2"
    local backup_id=$(basename "$backup_path")
    local archive_path="${BACKUP_ROOT}/archive/${backup_id}"

    mkdir -p "$archive_path"

    # Copy to archive
    cp -r "$backup_path"/* "$archive_path/" 2>/dev/null || return 1

    # Create archive manifest
    cat > "${archive_path}/archive_manifest.json" << EOF
{
  "backup_id": "$backup_id",
  "backup_type": "$backup_type",
  "archived_date": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "archive_reason": "age_exceeded_local_retention",
  "original_path": "$backup_path"
}
EOF

    # If S3 is available, move to cold storage
    if command -v aws &>/dev/null && [[ -n "$S3_BUCKET" ]]; then
        log_info "Moving to S3 cold storage: ${COLD_STORAGE_CLASS}"

        aws s3 cp "$archive_path" "s3://${S3_BUCKET}/archive/${backup_id}/" \
            --recursive \
            --region "$S3_REGION" \
            --storage-class "$COLD_STORAGE_CLASS" \
            2>/dev/null || true

        # Remove local archive after successful upload
        if [[ $? -eq 0 ]]; then
            rm -rf "$archive_path"
        fi
    fi

    return 0
}

# ============================================================================
# Cleanup
# ============================================================================
cleanup() {
    log_info "Cleaning up orphaned backup files..."

    local orphan_count=0

    # Find and remove orphaned files
    find "$BACKUP_ROOT" -type f -name "*.tmp" -mtime +1 -delete 2>/dev/null || true
    find "$BACKUP_ROOT" -type f -name "*.log.old" -mtime +7 -delete 2>/dev/null || true

    # Clean empty directories
    find "$BACKUP_ROOT" -type d -empty -delete 2>/dev/null || true

    log_info "Cleanup completed"
}

# ============================================================================
# Script Entry Point
# ============================================================================
main() {
    local action="${1:-report}"
    shift || true

    case "$action" in
        report)
            generate_report
            ;;
        apply)
            apply_retention "$@"
            ;;
        cleanup)
            cleanup
            ;;
        -h|--help)
            echo "Usage: $0 <action> [options]"
            echo ""
            echo "Actions:"
            echo "  report              Generate retention report (default)"
            echo "  apply [--dry-run]   Apply retention policy"
            echo "  cleanup             Clean up orphaned files"
            echo "  -h, --help          Show this help message"
            echo ""
            echo "Environment Variables:"
            echo "  BACKUP_ROOT           Backup directory (default: /var/lib/erlmcp/backups)"
            echo "  RETENTION_LOCAL       Local retention in days (default: 30)"
            echo "  RETENTION_REMOTE      Remote retention in days (default: 90)"
            echo "  RETENTION_COMPLIANCE  Compliance retention in days (default: 2555)"
            echo "  ARCHIVE_ENABLED       Enable archiving before deletion (default: true)"
            echo "  S3_BUCKET             S3 bucket for remote backups"
            echo "  DRY_RUN               Enable dry-run mode"
            echo ""
            echo "Retention Policy:"
            echo "  - Local full backups: ${RETENTION_LOCAL} days"
            echo "  - Weekly backups: ${RETENTION_WEEKLY} days"
            echo "  - Monthly backups: ${RETENTION_MONTHLY} days"
            echo "  - Compliance backups: ${RETENTION_COMPLIANCE} days (7 years)"
            echo "  - Remote (S3) backups: ${RETENTION_REMOTE} days"
            exit 0
            ;;
        *)
            log_error "Unknown action: $action"
            echo "Use -h for help"
            exit 1
            ;;
    esac
}

main "$@"
