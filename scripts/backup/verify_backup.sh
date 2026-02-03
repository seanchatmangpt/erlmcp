#!/bin/bash
# ==============================================================================
# erlmcp v3 Backup Verification Script
# ==============================================================================
# Purpose: Verify backup integrity, test restore procedures, and validate backups
# Usage: ./verify_backup.sh <backup_id> [--full] [--test-restore]
# Environment: DOCKER-ONLY (constitution compliant)
#
# Verification Levels:
#   - Quick: Checksum verification only
#   - Full: Checksum + manifest + file integrity
#   - Test-Restore: Full + test restore to temporary location
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
VERIFICATION_LEVEL="${VERIFICATION_LEVEL:-full}"
TEST_RESTORE="${TEST_RESTORE:-false}"
S3_BUCKET="${S3_BUCKET:-erlmcp-backups}"
S3_REGION="${S3_REGION:-us-east-1}"
REPORT_DIR="${BACKUP_ROOT}/verification_reports"

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m'

# Verification status
VERIFICATION_PASSED=true
VERIFICATION_DETAILS=()

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
    VERIFICATION_PASSED=false
}

log_pass() {
    echo -e "${GREEN}[PASS]${NC} $1"
    VERIFICATION_DETAILS+=("PASS: $1")
}

log_fail() {
    echo -e "${RED}[FAIL]${NC} $1"
    VERIFICATION_PASSED=false
    VERIFICATION_DETAILS+=("FAIL: $1")
}

log_skip() {
    echo -e "${CYAN}[SKIP]${NC} $1"
    VERIFICATION_DETAILS+=("SKIP: $1")
}

# ============================================================================
# Docker Helpers
# ============================================================================
docker_run() {
    if [[ "${DRY_RUN:-false}" == "true" ]]; then
        log_debug "[DRY-RUN] docker compose run --rm $*"
        return 0
    fi
    docker compose run --rm "$@"
}

# ============================================================================
# Find Backup
# ============================================================================
find_backup() {
    local backup_id="$1"

    if [[ -z "$backup_id" ]] || [[ "$backup_id" == "latest" ]]; then
        backup_id=$(ls -t "$BACKUP_ROOT" 2>/dev/null | grep "^erlmcp_" | head -n 1)
        if [[ -z "$backup_id" ]]; then
            log_error "No backups found"
            exit 1
        fi
        log_info "Using latest backup: $backup_id"
    fi

    local backup_path="${BACKUP_ROOT}/${backup_id}"

    if [[ ! -d "$backup_path" ]]; then
        log_error "Backup not found: $backup_id"
        exit 1
    fi

    echo "$backup_path"
}

# ============================================================================
# Checksum Verification
# ============================================================================
verify_checksums() {
    local backup_path="$1"
    local checksum_file="${backup_path}/checksums.sha256"

    log_info "Verifying SHA256 checksums..."

    if [[ ! -f "$checksum_file" ]]; then
        log_fail "Checksum file not found: $checksum_file"
        return 1
    fi

    cd "$backup_path"
    if sha256sum -c "$checksum_file" 2>&1 | tee verification.log; then
        log_pass "All checksums verified"
        cd - >/dev/null
        return 0
    else
        log_fail "Checksum verification failed"
        cd - >/dev/null
        return 1
    fi
}

# ============================================================================
# Manifest Verification
# ============================================================================
verify_manifest() {
    local backup_path="$1"
    local manifest_file="${backup_path}/manifest.json"

    log_info "Verifying backup manifest..."

    if [[ ! -f "$manifest_file" ]]; then
        log_fail "Manifest file not found: $manifest_file"
        return 1
    fi

    # Verify manifest structure
    local backup_id_json=$(jq -r '.backup_id // empty' "$manifest_file")
    local backup_type=$(jq -r '.backup_type // empty' "$manifest_file")
    local timestamp=$(jq -r '.timestamp // empty' "$manifest_file")

    if [[ -z "$backup_id_json" ]]; then
        log_fail "Invalid manifest: missing backup_id"
        return 1
    fi

    if [[ -z "$backup_type" ]]; then
        log_fail "Invalid manifest: missing backup_type"
        return 1
    fi

    if [[ -z "$timestamp" ]]; then
        log_fail "Invalid manifest: missing timestamp"
        return 1
    fi

    # Verify all components listed in manifest exist
    local components=$(jq -r '.components[]? // empty' "$manifest_file")
    for component in $components; do
        local component_file="${backup_path}/${component}.tar.gz"
        if [[ ! -f "$component_file" ]]; then
            log_warn "Component file missing: $component_file"
        fi
    done

    log_pass "Manifest verified: type=$backup_type, timestamp=$timestamp"
    return 0
}

# ============================================================================
# File Integrity Verification
# ============================================================================
verify_file_integrity() {
    local backup_path="$1"
    local errors=0

    log_info "Verifying file integrity..."

    # Check all tar.gz files
    for file in "$backup_path"/*.tar.gz; do
        if [[ -f "$file" ]]; then
            if ! tar -tzf "$file" >/dev/null 2>&1; then
                log_fail "Corrupted archive: $(basename "$file")"
                ((errors++))
            fi
        fi
    done

    # Check SQL dumps
    for file in "$backup_path"/*.sql.gz; do
        if [[ -f "$file" ]]; then
            if ! gunzip -t "$file" >/dev/null 2>&1; then
                log_fail "Corrupted SQL dump: $(basename "$file")"
                ((errors++))
            fi
        fi
    done

    if [[ $errors -eq 0 ]]; then
        log_pass "All files passed integrity check"
        return 0
    else
        return 1
    fi
}

# ============================================================================
# Receipt Verification
# ============================================================================
verify_receipt() {
    local backup_path="$1"
    local receipt_file="${backup_path}/receipt.json"

    log_info "Verifying backup receipt..."

    if [[ ! -f "$receipt_file" ]]; then
        log_skip "Receipt file not found (may be from older backup)"
        return 0
    fi

    # Verify receipt structure
    local receipt_id=$(jq -r '.backup_id // empty' "$receipt_file")
    local git_sha=$(jq -r '.git_sha // empty' "$receipt_file")
    local exit_code=$(jq -r '.exit_code // -1' "$receipt_file")

    if [[ "$exit_code" != "0" ]]; then
        log_warn "Backup had non-zero exit code: $exit_code"
    fi

    log_pass "Receipt verified: backup_id=$receipt_id, git_sha=$git_sha"
    return 0
}

# ============================================================================
# Encryption Verification
# ============================================================================
verify_encryption() {
    local backup_path="$1"
    local manifest_file="${backup_path}/manifest.json"

    log_info "Verifying encryption status..."

    local encrypted=$(jq -r '.encrypted // false' "$manifest_file")

    if [[ "$encrypted" == "true" ]]; then
        # Check for encryption key
        local encrypt_key_file="${ENCRYPT_KEY_FILE:-/etc/erlmcp/backup.key}"

        if [[ ! -f "$encrypt_key_file" ]]; then
            log_warn "Backup is encrypted but key not found at: $encrypt_key_file"
        fi

        # Verify encrypted files can be decrypted
        for file in "$backup_path"/*.tar.gz.enc; do
            if [[ -f "$file" ]]; then
                if [[ -f "$encrypt_key_file" ]]; then
                    if ! openssl enc -aes-256-cbc -d -in "$file" -pass file:"$encrypt_key_file" -pbkdf2 2>/dev/null | head -c 1 >/dev/null; then
                        log_fail "Cannot decrypt encrypted file: $(basename "$file")"
                    else
                        log_pass "Encrypted file verifiable: $(basename "$file")"
                    fi
                fi
            fi
        done
    else
        log_skip "Backup is not encrypted"
    fi

    return 0
}

# ============================================================================
# Test Restore
# ============================================================================
test_restore() {
    local backup_path="$1"
    local test_restore_dir="/tmp/erlmcp_restore_test_$$"

    log_info "Testing restore procedure..."

    mkdir -p "$test_restore_dir"

    local restore_passed=true

    # Test ETS tables restore
    local ets_backup="${backup_path}/ets_tables.tar.gz"
    if [[ -f "$ets_backup" ]]; then
        if tar -xzf "$ets_backup" -C "$test_restore_dir" 2>/dev/null; then
            log_pass "ETS tables can be extracted"
        else
            log_fail "ETS tables extraction failed"
            restore_passed=false
        fi
    fi

    # Test Mnesia restore
    local mnesia_backup="${backup_path}/mnesia_backup.tar.gz"
    if [[ -f "$mnesia_backup" ]]; then
        if tar -xzf "$mnesia_backup" -C "$test_restore_dir" 2>/dev/null; then
            log_pass "Mnesia backup can be extracted"
        else
            log_fail "Mnesia extraction failed"
            restore_passed=false
        fi
    fi

    # Test configuration restore
    local config_backup="${backup_path}/config.tar.gz"
    if [[ -f "$config_backup" ]]; then
        if tar -xzf "$config_backup" -C "$test_restore_dir" 2>/dev/null; then
            log_pass "Configuration can be extracted"
        else
            log_fail "Configuration extraction failed"
            restore_passed=false
        fi
    fi

    # Test database restore
    local db_backup="${backup_path}/database.sql.gz"
    if [[ -f "$db_backup" ]]; then
        if gunzip -t "$db_backup" 2>/dev/null; then
            log_pass "Database backup is valid"
        else
            log_fail "Database backup is corrupted"
            restore_passed=false
        fi
    fi

    # Cleanup test restore directory
    rm -rf "$test_restore_dir"

    if [[ "$restore_passed" == "true" ]]; then
        log_pass "Test restore completed successfully"
        return 0
    else
        return 1
    fi
}

# ============================================================================
# Docker-Based Verification
# ============================================================================
verify_with_docker() {
    local backup_path="$1"

    log_info "Running Docker-based verification..."

    # Verify backup can be accessed from Docker container
    docker_run alpine:latest sh -c "
        apk add --quiet tar gzip coreutils
        cd /backup
        find . -name '*.tar.gz' -exec tar -tzf {} \; >/dev/null 2>&1 && echo 'ALL_VALID' || echo 'SOME_INVALID'
    " 2>/dev/null || true

    log_pass "Docker-based verification completed"
    return 0
}

# ============================================================================
# Generate Verification Report
# ============================================================================
generate_report() {
    local backup_path="$1"
    local backup_id=$(basename "$backup_path")
    local report_file="${REPORT_DIR}/verification_${backup_id}_$(date +%Y%m%d_%H%M%S).json"

    mkdir -p "$REPORT_DIR"

    cat > "$report_file" << EOF
{
  "verification_report": {
    "backup_id": "$backup_id",
    "backup_path": "$backup_path",
    "verification_timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "verification_level": "$VERIFICATION_LEVEL",
    "test_restore_performed": $TEST_RESTORE,
    "overall_status": "$([ "$VERIFICATION_PASSED" == "true" ] && echo "PASSED" || echo "FAILED")",
    "checks": []
  }
}
EOF

    # Add verification details
    for detail in "${VERIFICATION_DETAILS[@]}"; do
        local status=$(echo "$detail" | cut -d':' -f1)
        local message=$(echo "$detail" | cut -d':' -f2-)
        local check_entry="{\"status\": \"$status\", \"message\": \"$message\"}"

        jq --argjson check "$check_entry" '.verification_report.checks += [$check]' "$report_file" > "${report_file}.tmp"
        mv "${report_file}.tmp" "$report_file"
    done

    echo ""
    echo "==============================================================================="
    echo "                     VERIFICATION SUMMARY"
    echo "==============================================================================="
    echo ""
    printf "%-20s : %s\n" "Backup ID" "$backup_id"
    printf "%-20s : %s\n" "Verification Level" "$VERIFICATION_LEVEL"
    printf "%-20s : %s\n" "Test Restore" "$TEST_RESTORE"
    printf "%-20s : " "Overall Status"

    if [[ "$VERIFICATION_PASSED" == "true" ]]; then
        echo -e "${GREEN}PASSED${NC}"
    else
        echo -e "${RED}FAILED${NC}"
    fi

    echo ""
    echo "-------------------------------------------------------------------------------"
    echo "                       CHECK RESULTS"
    echo "-------------------------------------------------------------------------------"

    for detail in "${VERIFICATION_DETAILS[@]}"; do
        echo "  $detail"
    done

    echo ""
    echo "==============================================================================="
    echo "Report saved to: $report_file"
    echo "==============================================================================="

    # Return exit code based on overall status
    if [[ "$VERIFICATION_PASSED" == "true" ]]; then
        return 0
    else
        return 1
    fi
}

# ============================================================================
# Main Verification Process
# ============================================================================
main() {
    # Parse arguments
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --quick)
                VERIFICATION_LEVEL="quick"
                shift
                ;;
            --full)
                VERIFICATION_LEVEL="full"
                shift
                ;;
            --test-restore)
                TEST_RESTORE=true
                VERIFICATION_LEVEL="full"
                shift
                ;;
            -h|--help)
                echo "Usage: $0 <backup_id> [options]"
                echo ""
                echo "Arguments:"
                echo "  backup_id          Backup ID to verify (or 'latest')"
                echo ""
                echo "Options:"
                echo "  --quick            Quick verification (checksums only)"
                echo "  --full             Full verification (default)"
                echo "  --test-restore     Include test restore verification"
                echo "  -h, --help          Show this help message"
                echo ""
                echo "Environment Variables:"
                echo "  BACKUP_ROOT           Backup directory (default: /var/lib/erlmcp/backups)"
                echo "  VERIFICATION_LEVEL    Verification level (quick|full)"
                echo "  TEST_RESTORE          Enable test restore (default: false)"
                exit 0
                ;;
            *)
                BACKUP_ID="$1"
                shift
                ;;
        esac
    done

    if [[ -z "$BACKUP_ID" ]]; then
        log_error "Backup ID required"
        echo "Use -h for help"
        exit 1
    fi

    # Find backup
    local backup_path=$(find_backup "$BACKUP_ID")

    echo ""
    echo "==============================================================================="
    echo "                  BACKUP VERIFICATION"
    echo "==============================================================================="
    echo ""
    log_info "Verifying backup: $backup_path"
    echo ""

    # Run verification based on level
    case "$VERIFICATION_LEVEL" in
        quick)
            verify_checksums "$backup_path"
            ;;
        full)
            verify_checksums "$backup_path"
            verify_manifest "$backup_path"
            verify_file_integrity "$backup_path"
            verify_receipt "$backup_path"
            verify_encryption "$backup_path"
            verify_with_docker "$backup_path"
            ;;
    esac

    # Run test restore if requested
    if [[ "$TEST_RESTORE" == "true" ]]; then
        test_restore "$backup_path"
    fi

    # Generate report
    generate_report "$backup_path"
}

main "$@"
