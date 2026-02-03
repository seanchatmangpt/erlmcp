#!/usr/bin/env bash
#
###################################################################################################
# ERLMCP v3: Receipt Verification Script
# Validates cryptographic integrity of operation receipts
#
# Usage: ./scripts/receipts/verify.sh [receipt_id] [--strict] [--signature-only]
#
# Verification checks:
#   - SHA-256 checksum of receipt file
#   - GPG signature verification (if signed)
#   - Receipt hash integrity (embedded hash matches computed hash)
#   - Output file hash verification (stdout/stderr match embedded hashes)
#   - Docker-only constitution validation
###################################################################################################

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
RECEIPTS_DIR="${PROJECT_ROOT}/.erlmcp/receipts"
GPG_KEY_ID="${GPG_KEY_ID:-}"

# Colors
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m'

log_info() { echo -e "${BLUE}[VERIFY]${NC} $*"; }
log_success() { echo -e "${GREEN}[VERIFY]${NC} $*"; }
log_warning() { echo -e "${YELLOW}[VERIFY]${NC} $*"; }
log_error() { echo -e "${RED}[VERIFY]${NC} $*" >&2; }

# Compute SHA-256 hash
hash_file() {
    local file="$1"
    if [[ ! -f "$file" ]]; then
        echo "FILE_NOT_FOUND"
        return 1
    fi

    if command -v shasum &>/dev/null; then
        shasum -a 256 "$file" | awk '{print $1}'
    elif command -v sha256sum &>/dev/null; then
        sha256sum "$file" | awk '{print $1}'
    else
        openssl dgst -sha256 "$file" | awk '{print $NF}'
    fi
}

# Compute string hash
hash_string() {
    local input="$1"
    if command -v shasum &>/dev/null; then
        echo -n "$input" | shasum -a 256 | awk '{print $1}'
    elif command -v sha256sum &>/dev/null; then
        echo -n "$input" | sha256sum | awk '{print $1}'
    else
        echo -n "$input" | openssl dgst -sha256 | awk '{print $NF}'
    fi
}

# Verify checksum
verify_checksum() {
    local receipt_file="$1"
    local checksum_file="${receipt_file}.sha256"

    if [[ ! -f "$checksum_file" ]]; then
        log_warning "Checksum file missing: $(basename "$checksum_file")"
        return 1
    fi

    log_info "Verifying checksum..."

    if command -v shasum &>/dev/null; then
        if shasum -c "$checksum_file" &>/dev/null; then
            log_success "Checksum verified"
            return 0
        fi
    elif command -v sha256sum &>/dev/null; then
        if sha256sum -c "$checksum_file" &>/dev/null; then
            log_success "Checksum verified"
            return 0
        fi
    fi

    log_error "Checksum verification FAILED"
    return 1
}

# Verify GPG signature
verify_signature() {
    local receipt_file="$1"
    local signature_file="${receipt_file}.asc"

    if [[ ! -f "$signature_file" ]]; then
        log_warning "Signature file missing: $(basename "$signature_file")"
        return 2  # Not fatal
    fi

    if ! command -v gpg &>/dev/null; then
        log_warning "GPG not available, skipping signature verification"
        return 2
    fi

    log_info "Verifying GPG signature..."

    local output
    if output=$(gpg --verify "$signature_file" "$receipt_file" 2>&1); then
        local key_id
        key_id=$(echo "$output" | grep -oE 'ID [A-F0-9]+' | head -1 | cut -d' ' -f2 || echo "unknown")
        log_success "Signature verified (key: $key_id)"
        return 0
    else
        log_error "Signature verification FAILED"
        echo "$output" | head -3 >&2
        return 1
    fi
}

# Verify receipt hash
verify_receipt_hash() {
    local receipt_file="$1"

    if ! command -v jq &>/dev/null; then
        log_warning "jq not available, skipping hash verification"
        return 2
    fi

    log_info "Verifying receipt hash integrity..."

    local stored_hash git_sha image_digest command exit_code stdout_hash stderr_hash

    stored_hash=$(jq -r '.metadata.receipt_hash' "$receipt_file" 2>/dev/null || echo "missing")
    if [[ "$stored_hash" == "null" ]] || [[ "$stored_hash" == "missing" ]]; then
        log_warning "Receipt hash not found in metadata"
        return 1
    fi

    git_sha=$(jq -r '.source.git_sha' "$receipt_file")
    image_digest=$(jq -r '.container.image_digest' "$receipt_file")
    command=$(jq -r '.execution.command' "$receipt_file")
    exit_code=$(jq -r '.execution.exit_code' "$receipt_file")
    stdout_hash=$(jq -r '.outputs.stdout.hash' "$receipt_file")
    stderr_hash=$(jq -r '.outputs.stderr.hash' "$receipt_file")

    local computed_hash
    computed_hash=$(hash_string "${git_sha}${image_digest}${command}${exit_code}${stdout_hash}${stderr_hash}")

    if [[ "$stored_hash" == "$computed_hash" ]]; then
        log_success "Receipt hash verified"
        return 0
    else
        log_error "Receipt hash MISMATCH"
        log_error "  Stored:   $stored_hash"
        log_error "  Computed: $computed_hash"
        return 1
    fi
}

# Verify output file hashes
verify_output_hashes() {
    local receipt_file="$1"

    if ! command -v jq &>/dev/null; then
        log_warning "jq not available, skipping output hash verification"
        return 2
    fi

    log_info "Verifying output file hashes..."

    local receipt_id expected_stdout_hash expected_stderr_hash
    receipt_id=$(jq -r '.receipt_id' "$receipt_file")
    expected_stdout_hash=$(jq -r '.outputs.stdout.hash' "$receipt_file")
    expected_stderr_hash=$(jq -r '.outputs.stderr.hash' "$receipt_file")

    local all_ok=true

    # Check stdout
    local stdout_file="${RECEIPTS_DIR}/${receipt_id}.stdout.log"
    if [[ -f "$stdout_file" ]]; then
        local actual_stdout_hash
        actual_stdout_hash=$(hash_file "$stdout_file")
        if [[ "$actual_stdout_hash" == "$expected_stdout_hash" ]]; then
            log_success "stdout hash verified"
        else
            log_error "stdout hash MISMATCH"
            log_error "  Expected: $expected_stdout_hash"
            log_error "  Actual:   $actual_stdout_hash"
            all_ok=false
        fi
    else
        log_warning "stdout file not found: $stdout_file"
    fi

    # Check stderr (only if non-empty expected)
    if [[ "$expected_stderr_hash" != "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" ]] && \
       [[ "$expected_stderr_hash" != "null" ]] && [[ -n "$expected_stderr_hash" ]]; then
        local stderr_file="${RECEIPTS_DIR}/${receipt_id}.stderr.log"
        if [[ -f "$stderr_file" ]]; then
            local actual_stderr_hash
            actual_stderr_hash=$(hash_file "$stderr_file")
            if [[ "$actual_stderr_hash" == "$expected_stderr_hash" ]]; then
                log_success "stderr hash verified"
            else
                log_error "stderr hash MISMATCH"
                log_error "  Expected: $expected_stderr_hash"
                log_error "  Actual:   $actual_stderr_hash"
                all_ok=false
            fi
        fi
    fi

    if [[ "$all_ok" == "true" ]]; then
        return 0
    else
        return 1
    fi
}

# Verify Docker-only constitution
verify_constitution() {
    local receipt_file="$1"

    if ! command -v jq &>/dev/null; then
        return 2
    fi

    log_info "Verifying Docker-only constitution..."

    local docker_only
    docker_only=$(jq -r '.constitution.docker_only // "false"' "$receipt_file")

    if [[ "$docker_only" == "true" ]]; then
        log_success "Constitution verified: docker_only=true"
        return 0
    else
        log_warning "Constitution flag not set to docker_only=true"
        return 1
    fi
}

# Full receipt verification
verify_receipt() {
    local receipt_path="$1"
    local strict="${2:-false}"
    local signature_only="${3:-false}"

    if [[ ! -f "$receipt_path" ]]; then
        log_error "Receipt not found: $receipt_path"
        return 1
    fi

    echo ""
    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    log_info "Receipt: $(basename "$receipt_path")"
    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    echo ""

    local checks_passed=0
    local checks_failed=0
    local checks_skipped=0

    # Checksum verification
    if verify_checksum "$receipt_path"; then
        ((checks_passed++))
    else
        ((checks_failed++))
        if [[ "$strict" == "true" ]]; then
            log_error "Strict mode: failing on checksum error"
            return 1
        fi
    fi

    # Signature verification
    if [[ "$signature_only" == "true" ]]; then
        if verify_signature "$receipt_path"; then
            ((checks_passed++))
        else
            ((checks_failed++))
            if [[ "$strict" == "true" ]]; then
                return 1
            fi
        fi
    else
        case $(verify_signature "$receipt_path"; echo $?) in
            0) ((checks_passed++)) ;;
            1) ((checks_failed++)); [[ "$strict" == "true" ]] && return 1 ;;
            2) ((checks_skipped++)) ;;
        esac
    fi

    # Receipt hash verification
    if [[ "$signature_only" != "true" ]]; then
        case $(verify_receipt_hash "$receipt_path"; echo $?) in
            0) ((checks_passed++)) ;;
            1) ((checks_failed++)); [[ "$strict" == "true" ]] && return 1 ;;
            2) ((checks_skipped++)) ;;
        esac
    fi

    # Output hash verification
    if [[ "$signature_only" != "true" ]]; then
        case $(verify_output_hashes "$receipt_path"; echo $?) in
            0) ((checks_passed++)) ;;
            1) ((checks_failed++)); [[ "$strict" == "true" ]] && return 1 ;;
            2) ((checks_skipped++)) ;;
        esac
    fi

    # Constitution verification
    if [[ "$signature_only" != "true" ]]; then
        case $(verify_constitution "$receipt_path"; echo $?) in
            0) ((checks_passed++)) ;;
            1) ((checks_failed++)) ;;
            2) ((checks_skipped++)) ;;
        esac
    fi

    # Summary
    echo ""
    echo -e "${CYAN}───────────────────────────────────────────────────────────${NC}"
    log_info "Verification Summary:"
    echo "  ${GREEN}Passed:${NC}   $checks_passed"
    echo "  ${RED}Failed:${NC}   $checks_failed"
    echo "  ${YELLOW}Skipped:${NC}  $checks_skipped"
    echo -e "${CYAN}───────────────────────────────────────────────────────────${NC}"

    if [[ $checks_failed -eq 0 ]]; then
        echo ""
        log_success "Receipt verification PASSED"
        return 0
    else
        echo ""
        log_error "Receipt verification FAILED"
        return 1
    fi
}

# Verify all receipts
verify_all() {
    local strict="${1:-false}"
    local signature_only="${2:-false}"

    log_info "Verifying all receipts in: $RECEIPTS_DIR"
    echo ""

    local total=0
    local verified=0
    local failed=0
    local skipped=0

    for receipt_file in "$RECEIPTS_DIR"/*.json; do
        if [[ -f "$receipt_file" ]]; then
            ((total++))
            if verify_receipt "$receipt_file" "$strict" "$signature_only"; then
                ((verified++))
            else
                ((failed++))
            fi
        fi
    done

    echo ""
    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    log_info "Overall Summary:"
    echo "  Total receipts:  $total"
    echo "  ${GREEN}Verified:${NC}       $verified"
    echo "  ${RED}Failed:${NC}          $failed"
    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    echo ""

    return $failed
}

# Show receipt details
show_receipt() {
    local receipt_path="$1"

    if [[ ! -f "$receipt_path" ]]; then
        log_error "Receipt not found: $receipt_path"
        return 1
    fi

    if command -v jq &>/dev/null; then
        jq '.' "$receipt_path"
    else
        cat "$receipt_path"
    fi
}

# Generate verification report
generate_report() {
    local output_file="${1:-${RECEIPTS_DIR}/verification-report.json}"

    log_info "Generating verification report: $output_file"

    local report="{"
    report+='"generated_at":"'$(date -u +%Y-%m-%dT%H:%M:%SZ)'",'
    report+='"receipts":['

    local first=true
    for receipt_file in "$RECEIPTS_DIR"/*.json; do
        if [[ -f "$receipt_file" ]]; then
            if [[ "$first" == "true" ]]; then
                first=false
            else
                report+=","
            fi

            local receipt_id
            local service
            local exit_code
            local success
            local timestamp
            local checksum_ok="false"
            local signature_ok="false"
            local signature_exists="false"
            local hash_ok="false"

            if command -v jq &>/dev/null; then
                receipt_id=$(jq -r '.receipt_id' "$receipt_file")
                service=$(jq -r '.container.service' "$receipt_file")
                exit_code=$(jq -r '.execution.exit_code' "$receipt_file")
                success=$(jq -r '.execution.success' "$receipt_file")
                timestamp=$(jq -r '.generated_at' "$receipt_file")
            fi

            # Check checksum
            if [[ -f "${receipt_file}.sha256" ]]; then
                if shasum -c "${receipt_file}.sha256" &>/dev/null || sha256sum -c "${receipt_file}.sha256" &>/dev/null; then
                    checksum_ok="true"
                fi
            fi

            # Check signature
            if [[ -f "${receipt_file}.asc" ]]; then
                signature_exists="true"
                if gpg --verify "${receipt_file}.asc" "$receipt_file" &>/dev/null; then
                    signature_ok="true"
                fi
            fi

            report+='{"receipt_id":"'"$receipt_id"'",'
            report+='"service":"'"$service"'",'
            report+='"exit_code":'"$exit_code"','
            report+='"success":'"$success"','
            report+='"timestamp":"'"$timestamp"'",'
            report+='"checksum_verified":'"$checksum_ok"','
            report+='"signature_verified":'"$signature_ok"','
            report+='"signature_exists":'"$signature_exists"','
            report+='"file":"'"$(basename "$receipt_file")"'"'
            report+='}'
        fi
    done

    report+=']}'

    echo "$report" | jq '.' > "$output_file"

    log_success "Report generated: $output_file"
}

# Main
main() {
    local receipt_id=""
    local strict=false
    local signature_only=false
    local report_mode=false
    local show_mode=false

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --strict|-s)
                strict=true
                ;;
            --signature-only|--sig)
                signature_only=true
                ;;
            --report|-r)
                report_mode=true
                ;;
            --show)
                show_mode=true
                ;;
            -*)
                log_warning "Unknown option: $1"
                ;;
            *)
                receipt_id="$1"
                ;;
        esac
        shift
    done

    mkdir -p "$RECEIPTS_DIR"

    if [[ "$report_mode" == "true" ]]; then
        generate_report ""
        exit $?
    fi

    if [[ "$show_mode" == "true" ]]; then
        if [[ -z "$receipt_id" ]]; then
            log_error "Usage: $0 --show <receipt_id>"
            exit 1
        fi
        local receipt_path="${RECEIPTS_DIR}/${receipt_id}.json"
        if [[ ! -f "$receipt_path" ]]; then
            receipt_path=$(find "$RECEIPTS_DIR" -name "${receipt_id}*.json" | head -1)
        fi
        show_receipt "$receipt_path"
        exit $?
    fi

    if [[ -z "$receipt_id" ]]; then
        verify_all "$strict" "$signature_only"
        exit $?
    fi

    local receipt_path="${RECEIPTS_DIR}/${receipt_id}.json"
    if [[ ! -f "$receipt_path" ]]; then
        receipt_path=$(find "$RECEIPTS_DIR" -name "${receipt_id}*.json" | head -1)
    fi

    verify_receipt "$receipt_path" "$strict" "$signature_only"
    exit $?
}

main "$@"
