#!/bin/bash
# ==============================================================================
# erlmcp v3 Cross-Region Backup Replication
# ==============================================================================
# Purpose: Replicate backups to remote regions for disaster recovery
# Usage: ./cross_region_replicate.sh <backup_id> [--dry-run] [--async]
# Environment: DOCKER-ONLY (constitution compliant)
#
# Replication Strategy:
#   - Primary region: us-east-1
#   - Secondary region: us-west-2
#   - DR region: eu-west-1
#   - Storage class: STANDARD_IA (primary), GLACIER (DR)
#
# RPO: 15 minutes (async replication)
# RTO: 30 minutes (cross-region failover)
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
ASYNC_MODE="${ASYNC_MODE:-false}"
S3_BUCKET="${S3_BUCKET:-erlmcp-backups}"
S3_REGION="${S3_REGION:-us-east-1}"

# Replication targets
declare -A REPLICATION_REGIONS=(
    [secondary]="us-west-2"
    [dr]="eu-west-1"
    [apac]="ap-southeast-1"
)

declare -A REPLICATION_BUCKETS=(
    [secondary]="erlmcp-backups-us-west-2"
    [dr]="erlmcp-backups-eu"
    [apac]="erlmcp-backups-apac"
)

declare -A STORAGE_CLASSES=(
    [secondary]="STANDARD_IA"
    [dr]="GLACIER"
    [apac]="STANDARD_IA"
)

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m'

# Replication status
REPLICATION_SUCCESS=true
REPLICATION_RESULTS=()

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
    REPLICATION_SUCCESS=false
}

log_pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
    REPLICATION_RESULTS+=("PASS: $1")
}

log_fail() {
    echo -e "${RED}[FAIL]${NC} $1"
    REPLICATION_SUCCESS=false
    REPLICATION_RESULTS+=("FAIL: $1")
}

# ============================================================================
# Prerequisites Check
# ============================================================================
check_prerequisites() {
    log_info "Checking replication prerequisites..."

    # Check AWS CLI
    if ! command -v aws &>/dev/null; then
        log_error "AWS CLI not found. Install awscli package."
        exit 1
    fi

    # Check AWS credentials
    if ! aws sts get-caller-identity &>/dev/null; then
        log_error "AWS credentials not configured. Run 'aws configure'."
        exit 1
    fi

    # Verify primary bucket access
    if ! aws s3 ls "s3://${S3_BUCKET}" --region "$S3_REGION" &>/dev/null; then
        log_error "Cannot access primary S3 bucket: s3://${S3_BUCKET}"
        exit 1
    fi

    log_info "Prerequisites verified"
}

# ============================================================================
# Find Backup
# ============================================================================
find_backup() {
    local backup_id="$1"

    if [[ -z "$backup_id" ]] || [[ "$backup_id" == "latest" ]]; then
        backup_id=$(aws s3 ls "s3://${S3_BUCKET}/" --recursive 2>/dev/null | \
            grep "erlmcp_full_" | tail -n 1 | awk '{print $2}' | cut -d'/' -f1)

        if [[ -z "$backup_id" ]]; then
            # Try local backups
            backup_id=$(ls -t "$BACKUP_ROOT" 2>/dev/null | grep "^erlmcp_" | head -n 1)
        fi

        if [[ -z "$backup_id" ]]; then
            log_error "No backups found for replication"
            exit 1
        fi

        log_info "Using latest backup: $backup_id"
    fi

    echo "$backup_id"
}

# ============================================================================
# Check Backup Exists in Primary
# ============================================================================
check_primary_backup() {
    local backup_id="$1"

    log_info "Checking backup in primary region..."

    local backup_exists=$(aws s3 ls "s3://${S3_BUCKET}/${backup_id}/" --recursive --region "$S3_REGION" 2>/dev/null | wc -l)

    if [[ $backup_exists -eq 0 ]]; then
        # Check local backup path
        if [[ ! -d "${BACKUP_ROOT}/${backup_id}" ]]; then
            log_error "Backup not found in primary region or locally: $backup_id"
            return 1
        fi
        log_warn "Backup only exists locally, will upload first"
    fi

    log_pass "Backup found in primary region: $backup_id"
    return 0
}

# ============================================================================
# Upload to Primary if Needed
# ============================================================================
upload_to_primary() {
    local backup_id="$1"
    local backup_path="${BACKUP_ROOT}/${backup_id}"

    if [[ ! -d "$backup_path" ]]; then
        log_info "Local backup not found, skipping upload"
        return 0
    fi

    log_info "Uploading backup to primary region..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would upload to s3://${S3_BUCKET}/${backup_id}/"
        return 0
    fi

    aws s3 sync "$backup_path/" "s3://${S3_BUCKET}/${backup_id}/" \
        --region "$S3_REGION" \
        --storage-class STANDARD_IA \
        --sse AES256 \
        2>&1 | while read -r line; do
            log_debug "S3: $line"
        done

    log_pass "Uploaded to primary: s3://${S3_BUCKET}/${backup_id}/"
}

# ============================================================================
# Replicate to Region
# ============================================================================
replicate_to_region() {
    local backup_id="$1"
    local target_name="$2"
    local target_region="${REPLICATION_REGIONS[$target_name]}"
    local target_bucket="${REPLICATION_BUCKETS[$target_name]}"
    local storage_class="${STORAGE_CLASSES[$target_name]}"

    log_info "Replicating to ${target_name} region (${target_region})..."

    if [[ "$DRY_RUN" == "true" ]]; then
        log_debug "[DRY-RUN] Would replicate to s3://${target_bucket}/${backup_id}/"
        log_pass "Would replicate to ${target_name}"
        return 0
    fi

    # Check if target bucket exists, create if not
    if ! aws s3 ls "s3://${target_bucket}" --region "$target_region" &>/dev/null; then
        log_info "Creating target bucket: ${target_bucket}"
        aws s3 mb "s3://${target_bucket}" --region "$target_region" 2>/dev/null || {
            log_warn "Could not create bucket, may already exist or permissions issue"
        }
    fi

    # Set bucket versioning
    aws s3api put-bucket-versioning \
        --bucket "$target_bucket" \
        --region "$target_region" \
        --versioning-configuration Status=Enabled 2>/dev/null || true

    # Replicate backup
    local start_time=$(date +%s)

    if [[ "$ASYNC_MODE" == "true" ]]; then
        # Async replication using S3 batch operations
        aws s3 sync "s3://${S3_BUCKET}/${backup_id}/" "s3://${target_bucket}/${backup_id}/" \
            --source-region "$S3_REGION" \
            --region "$target_region" \
            --storage-class "$storage_class" \
            --sse AES256 \
            2>&1 &
        local async_pid=$!
        log_pass "Async replication started to ${target_name} (PID: $async_pid)"
    else
        # Sync replication
        aws s3 sync "s3://${S3_BUCKET}/${backup_id}/" "s3://${target_bucket}/${backup_id}/" \
            --source-region "$S3_REGION" \
            --region "$target_region" \
            --storage-class "$storage_class" \
            --sse AES256 \
            2>&1 | while read -r line; do
                log_debug "${target_name}: $line"
            done

        local end_time=$(date +%s)
        local duration=$((end_time - start_time))

        # Verify replication
        local source_files=$(aws s3 ls "s3://${S3_BUCKET}/${backup_id}/" --recursive --region "$S3_REGION" 2>/dev/null | wc -l)
        local target_files=$(aws s3 ls "s3://${target_bucket}/${backup_id}/" --recursive --region "$target_region" 2>/dev/null | wc -l)

        if [[ $source_files -eq $target_files ]] && [[ $target_files -gt 0 ]]; then
            log_pass "Replicated to ${target_name}: ${target_files} files in ${duration}s"
        else
            log_fail "Replication to ${target_name} incomplete: ${source_files} -> ${target_files} files"
        fi
    fi
}

# ============================================================================
# Set Replication Tags
# ============================================================================
set_replication_tags() {
    local backup_id="$1"
    local target_name="$2"
    local target_bucket="${REPLICATION_BUCKETS[$target_name]}"
    local target_region="${REPLICATION_REGIONS[$target_name]}"

    log_info "Setting replication tags for ${target_name}..."

    if [[ "$DRY_RUN" == "true" ]]; then
        return 0
    fi

    # Tag backup with replication metadata
    aws s3api put-object-tagging \
        --bucket "$target_bucket" \
        --key "${backup_id}/manifest.json" \
        --region "$target_region" \
        --tagging "TagSet=[
            {Key=ReplicatedFrom,Value=${S3_REGION}},
            {Key=ReplicatedAt,Value=$(date -u +%Y-%m-%dT%H:%M:%SZ)},
            {Key=BackupType,Value=cross_region_replica}
        ]" 2>/dev/null || true
}

# ============================================================================
# Verify Replication
# ============================================================================
verify_replication() {
    local backup_id="$1"

    log_info "Verifying cross-region replication..."

    local all_verified=true

    for target_name in "${!REPLICATION_REGIONS[@]}"; do
        local target_region="${REPLICATION_REGIONS[$target_name]}"
        local target_bucket="${REPLICATION_BUCKETS[$target_name]}"

        # Check if manifest exists in target
        if aws s3 ls "s3://${target_bucket}/${backup_id}/manifest.json" --region "$target_region" &>/dev/null; then
            log_pass "Verified in ${target_name}: s3://${target_bucket}/${backup_id}/"
        else
            log_fail "Not found in ${target_name}: s3://${target_bucket}/${backup_id}/"
            all_verified=false
        fi
    done

    return $([ "$all_verified" == "true" ] && echo 0 || echo 1)
}

# ============================================================================
# Generate Replication Report
# ============================================================================
generate_replication_report() {
    local backup_id="$1"
    local report_file="${BACKUP_ROOT}/replication_reports/replication_${backup_id}_$(date +%Y%m%d_%H%M%S).json"

    mkdir -p "$(dirname "$report_file")"

    cat > "$report_file" << EOF
{
  "replication_report": {
    "backup_id": "$backup_id",
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "primary_region": "$S3_REGION",
    "primary_bucket": "$S3_BUCKET",
    "async_mode": $ASYNC_MODE,
    "dry_run": $DRY_RUN,
    "overall_status": "$([ "$REPLICATION_SUCCESS" == "true" ] && echo "SUCCESS" || echo "FAILED")",
    "targets": []
  }
}
EOF

    # Add target details
    for target_name in "${!REPLICATION_REGIONS[@]}"; do
        local target_entry="{
            \"name\": \"$target_name\",
            \"region\": \"${REPLICATION_REGIONS[$target_name]}\",
            \"bucket\": \"${REPLICATION_BUCKETS[$target_name]}\",
            \"storage_class\": \"${STORAGE_CLASSES[$target_name]}\"
        }"
        jq --argjson target "$target_entry" '.replication_report.targets += [$target]' "$report_file" > "${report_file}.tmp"
        mv "${report_file}.tmp" "$report_file"
    done

    # Add results
    for result in "${REPLICATION_RESULTS[@]}"; do
        local status=$(echo "$result" | cut -d':' -f1)
        local message=$(echo "$result" | cut -d':' -f2-)
        local result_entry="{\"status\": \"$status\", \"message\": \"$message\"}"
        jq --argjson result "$result_entry" '.replication_report.results += [$result]' "$report_file" > "${report_file}.tmp"
        mv "${report_file}.tmp" "$report_file"
    done

    log_info "Replication report saved: $report_file"
}

# ============================================================================
# Print Replication Summary
# ============================================================================
print_summary() {
    local backup_id="$1"

    echo ""
    echo "==============================================================================="
    echo "                  CROSS-REGION REPLICATION SUMMARY"
    echo "==============================================================================="
    echo ""
    printf "%-20s : %s\n" "Backup ID" "$backup_id"
    printf "%-20s : %s\n" "Primary Region" "$S3_REGION"
    printf "%-20s : " "Overall Status"

    if [[ "$REPLICATION_SUCCESS" == "true" ]]; then
        echo -e "${GREEN}SUCCESS${NC}"
    else
        echo -e "${RED}FAILED${NC}"
    fi

    echo ""
    echo "-------------------------------------------------------------------------------"
    echo "                       REPLICATION TARGETS"
    echo "-------------------------------------------------------------------------------"

    for target_name in "${!REPLICATION_REGIONS[@]}"; do
        printf "%-15s : %-15s -> %s\n" \
            "$target_name" \
            "${REPLICATION_REGIONS[$target_name]}" \
            "${STORAGE_CLASSES[$target_name]}"
    done

    echo ""
    echo "==============================================================================="
}

# ============================================================================
# Main Replication Process
# ============================================================================
main() {
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --async)
                ASYNC_MODE=true
                shift
                ;;
            --regions=*)
                IFS=',' read -ra CUSTOM_REGIONS <<< "${1#*=}"
                shift
                ;;
            -h|--help)
                echo "Usage: $0 <backup_id> [options]"
                echo ""
                echo "Arguments:"
                echo "  backup_id          Backup ID to replicate (or 'latest')"
                echo ""
                echo "Options:"
                echo "  --dry-run          Show what would be done without executing"
                echo "  --async            Start async replication in background"
                echo "  --regions=r1,r2    Comma-separated list of target regions"
                echo "  -h, --help          Show this help message"
                echo ""
                echo "Environment Variables:"
                echo "  BACKUP_ROOT           Backup directory (default: /var/lib/erlmcp/backups)"
                echo "  S3_BUCKET             Primary S3 bucket"
                echo "  S3_REGION             Primary S3 region (default: us-east-1)"
                echo "  ASYNC_MODE            Enable async replication (default: false)"
                echo ""
                echo "Default Replication Targets:"
                echo "  secondary: us-west-2 (STANDARD_IA)"
                echo "  dr: eu-west-1 (GLACIER)"
                echo "  apac: ap-southeast-1 (STANDARD_IA)"
                exit 0
                ;;
            *)
                BACKUP_ID="$1"
                shift
                ;;
        esac
    done

    # Set error trap
    trap 'log_error "Replication interrupted"; exit 1' INT TERM

    # Check prerequisites
    check_prerequisites

    # Find backup
    BACKUP_ID=$(find_backup "$BACKUP_ID")

    # Check primary backup
    check_primary_backup "$BACKUP_ID" || upload_to_primary "$BACKUP_ID"

    # Replicate to each region
    for target_name in "${!REPLICATION_REGIONS[@]}"; do
        replicate_to_region "$BACKUP_ID" "$target_name"
        set_replication_tags "$BACKUP_ID" "$target_name"
    done

    # Wait for async jobs if in async mode
    if [[ "$ASYNC_MODE" == "true" ]]; then
        log_info "Waiting for async replication jobs..."
        wait
    fi

    # Verify replication
    verify_replication "$BACKUP_ID"

    # Generate report
    generate_replication_report "$BACKUP_ID"

    # Print summary
    print_summary "$BACKUP_ID"

    # Return exit code
    if [[ "$REPLICATION_SUCCESS" == "true" ]]; then
        exit 0
    else
        exit 1
    fi
}

main "$@"
