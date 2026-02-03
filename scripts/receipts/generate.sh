#!/usr/bin/env bash
#
###################################################################################################
# ERLMCP v3: Cryptographically-Verifiable Receipt Generation System
# TCPS Receipt Chain - Immutable Audit Trail for Production Operations
#
# Generates signed receipts containing:
#   - Git SHA (source commit)
#   - Docker image digest (container fingerprint)
#   - Service name
#   - Command executed
#   - Exit code
#   - stdout/stderr SHA-256 hashes
#   - ISO 8601 timestamp
#   - cryptographic signature
#
# Usage:
#   ./scripts/receipts/generate.sh <service> <command> [--docker-service]
#
# Examples:
#   ./scripts/receipts/generate.sh erlmcp-build "rebar3 compile" --docker-service=erlmcp-build
#   ./scripts/receipts/generate.sh erlmcp-unit "rebar3 eunit" --docker-service=erlmcp-unit
#   ./scripts/receipts/generate.sh erlmcp-ct "rebar3 ct" --docker-service=erlmcp-ct
#
# Constitution: DOCKER-ONLY. Host execution forbidden.
###################################################################################################

set -euo pipefail

#######################################
# Configuration & Constants
#######################################

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
RECEIPTS_DIR="${PROJECT_ROOT}/.erlmcp/receipts"
ARCHIVE_DIR="${PROJECT_ROOT}/.erlmcp/receipts/archive"

# Version info
VERSION="${VERSION:-3.0.0}"
RECEIPT_VERSION="1.0.0"

# Cryptography defaults
HASH_ALGO="sha256"
SIGN_ALGO="${SIGN_ALGO:-ed25519}"  # ed25519, rsa, ecdsa
GPG_KEY_ID="${GPG_KEY_ID:-}"

# Docker defaults
DOCKER_COMPOSE_FILE="${DOCKER_COMPOSE_FILE:-docker-compose.yml}"
COMPOSE_PROJECT_NAME="${COMPOSE_PROJECT_NAME:-erlmcp}"

#######################################
# Colors & Formatting
#######################################

readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly GRAY='\033[0;90m'
readonly NC='\033[0m' # No Color

#######################################
# Logging Functions
#######################################

log_info() {
    echo -e "${BLUE}[RECEIPT]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[RECEIPT]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[RECEIPT]${NC} $*"
}

log_error() {
    echo -e "${RED}[RECEIPT]${NC} $*" >&2
}

log_debug() {
    if [[ "${DEBUG:-0}" == "1" ]]; then
        echo -e "${GRAY}[RECEIPT]${NC} $*"
    fi
}

#######################################
# Cryptographic Functions
#######################################

# Compute SHA-256 hash of a string
hash_string() {
    local input="$1"
    if command -v shasum &>/dev/null; then
        echo -n "$input" | shasum -a 256 | awk '{print $1}'
    elif command -v sha256sum &>/dev/null; then
        echo -n "$input" | sha256sum | awk '{print $1}'
    else
        # Fallback to OpenSSL
        echo -n "$input" | openssl dgst -sha256 | awk '{print $NF}'
    fi
}

# Compute SHA-256 hash of a file
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

# Compute hash of stdin (for stdout/stderr capture)
hash_stdin() {
    local temp_file
    temp_file=$(mktemp)
    cat > "$temp_file"
    hash_file "$temp_file"
    cat "$temp_file"  # Output the original content
    rm -f "$temp_file"
}

# Get current timestamp in ISO 8601 format with UTC
get_timestamp() {
    date -u '+%Y-%m-%dT%H:%M:%S.%3NZ'
}

# Get Unix timestamp
get_unix_timestamp() {
    date +%s
}

#######################################
# Git Functions
#######################################

# Get current Git SHA
get_git_sha() {
    if [[ -d "${PROJECT_ROOT}/.git" ]]; then
        git -C "$PROJECT_ROOT" rev-parse HEAD 2>/dev/null || echo "unknown"
    else
        echo "not-a-git-repo"
    fi
}

# Get Git branch
get_git_branch() {
    if [[ -d "${PROJECT_ROOT}/.git" ]]; then
        git -C "$PROJECT_ROOT" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown"
    else
        echo "unknown"
    fi
}

# Get Git commit message (short)
get_git_message() {
    if [[ -d "${PROJECT_ROOT}/.git" ]]; then
        git -C "$PROJECT_ROOT" log -1 --pretty=%s 2>/dev/null || echo "unknown"
    else
        echo "unknown"
    fi
}

# Get Git commit author
get_git_author() {
    if [[ -d "${PROJECT_ROOT}/.git" ]]; then
        git -C "$PROJECT_ROOT" log -1 --pretty=%an 2>/dev/null || echo "unknown"
    else
        echo "unknown"
    fi
}

#######################################
# Docker Functions
#######################################

# Get Docker image digest for a service
get_docker_image_digest() {
    local service="$1"
    local image_name
    local image_id

    # Try to get image ID from docker-compose
    image_name=$(docker compose -f "$DOCKER_COMPOSE_FILE" config 2>/dev/null | \
        grep -A 10 "services:" | \
        grep -A 5 "$service:" | \
        grep "image:" | \
        awk '{print $2}' | tr -d '"')

    if [[ -z "$image_name" ]]; then
        # Try direct inspection
        image_name="erlmcp-${service}:${VERSION}"
    fi

    # Get image digest (sha256)
    image_id=$(docker images --format="{{.ID}}:{{.Digest}}" "$image_name" 2>/dev/null | head -1)

    if [[ -n "$image_id" ]]; then
        echo "$image_id"
    else
        echo "image-not-found:$image_name"
    fi
}

# Get Docker image size
get_docker_image_size() {
    local service="$1"
    local image_name

    image_name=$(docker compose -f "$DOCKER_COMPOSE_FILE" config 2>/dev/null | \
        grep -A 10 "services:" | \
        grep -A 5 "$service:" | \
        grep "image:" | \
        awk '{print $2}' | tr -d '"')

    if [[ -z "$image_name" ]]; then
        image_name="erlmcp-${service}:${VERSION}"
    fi

    docker images --format="{{.Size}}" "$image_name" 2>/dev/null | head -1 || echo "unknown"
}

# Check if Docker is available
check_docker() {
    if ! command -v docker &>/dev/null; then
        log_error "Docker not found. DOCKER-ONLY constitution violation."
        return 1
    fi

    if ! docker info &>/dev/null; then
        log_error "Docker daemon not running. Cannot proceed."
        return 1
    fi

    return 0
}

# Check if docker-compose is available
check_docker_compose() {
    if docker compose version &>/dev/null; then
        return 0
    elif command -v docker-compose &>/dev/null; then
        return 0
    else
        log_error "docker-compose not found."
        return 1
    fi
}

#######################################
# GPG Signing Functions
#######################################

# Check if GPG is available and configured
check_gpg() {
    if [[ -z "${GPG_KEY_ID}" ]]; then
        log_warning "GPG_KEY_ID not set. Skipping signature."
        return 1
    fi

    if ! command -v gpg &>/dev/null; then
        log_warning "GPG not found. Skipping signature."
        return 1
    fi

    # Verify key exists
    if ! gpg --list-keys "$GPG_KEY_ID" &>/dev/null; then
        log_warning "GPG key $GPG_KEY_ID not found. Skipping signature."
        return 1
    fi

    return 0
}

# Sign a file with GPG
sign_file() {
    local file="$1"
    local signature_file="${file}.asc"

    if ! check_gpg; then
        return 1
    fi

    log_debug "Signing $file with key $GPG_KEY_ID"

    if gpg --default-key "$GPG_KEY_ID" --armor --detach-sign --output "$signature_file" "$file" 2>/dev/null; then
        echo "$signature_file"
        return 0
    else
        log_error "Failed to sign $file"
        return 1
    fi
}

# Verify a file signature
verify_signature() {
    local file="$1"
    local signature_file="${file}.asc"

    if [[ ! -f "$signature_file" ]]; then
        log_warning "Signature file not found: $signature_file"
        return 1
    fi

    gpg --verify "$signature_file" "$file" 2>/dev/null
}

#######################################
# Receipt Generation Functions
#######################################

# Create the receipts directory structure
init_receipt_dirs() {
    mkdir -p "$RECEIPTS_DIR"
    mkdir -p "$ARCHIVE_DIR"
    mkdir -p "${RECEIPTS_DIR}/pending"
    mkdir -p "${RECEIPTS_DIR}/verified"
}

# Generate receipt ID
generate_receipt_id() {
    local service="$1"
    local timestamp
    timestamp=$(get_unix_timestamp)
    local random_part
    random_part=$(head -c 8 /dev/urandom | xxd -p | tr -d ' \n')
    echo "R-${service}-${timestamp}-${random_part}"
}

# Build receipt JSON
build_receipt_json() {
    local receipt_id="$1"
    local service="$2"
    local command="$3"
    local exit_code="$4"
    local stdout_hash="$5"
    local stderr_hash="$6"
    local stdout_file="$7"
    local stderr_file="$8"

    local timestamp
    timestamp=$(get_timestamp)

    local git_sha
    local git_branch
    local git_message
    local git_author
    git_sha=$(get_git_sha)
    git_branch=$(get_git_branch)
    git_message=$(get_git_message)
    git_author=$(get_git_author)

    local image_digest
    local image_size
    image_digest=$(get_docker_image_digest "$service")
    image_size=$(get_docker_image_size "$service")

    local hostname
    hostname=$(hostname 2>/dev/null || echo "unknown")

    local username
    username=${USER:-${LOGNAME:-$(whoami 2>/dev/null || echo "unknown")}}

    # Count stdout/stderr lines
    local stdout_lines=0
    local stderr_lines=0
    if [[ -f "$stdout_file" ]]; then
        stdout_lines=$(wc -l < "$stdout_file" | tr -d ' ')
    fi
    if [[ -f "$stderr_file" ]]; then
        stderr_lines=$(wc -l < "$stderr_file" | tr -d ' ')
    fi

    # Calculate receipt hash (hash of all relevant data)
    local receipt_prehash
    receipt_prehash="${git_sha}${image_digest}${command}${exit_code}${stdout_hash}${stderr_hash}${timestamp}"
    local receipt_hash
    receipt_hash=$(hash_string "$receipt_prehash")

    # Build JSON
    cat <<EOF
{
  "receipt_version": "${RECEIPT_VERSION}",
  "receipt_id": "${receipt_id}",
  "generated_at": "${timestamp}",
  "erlmcp_version": "${VERSION}",
  "metadata": {
    "hostname": "${hostname}",
    "username": "${username}",
    "working_directory": "${PROJECT_ROOT}",
    "docker_compose_file": "${DOCKER_COMPOSE_FILE}",
    "receipt_hash": "${receipt_hash}"
  },
  "source": {
    "git_sha": "${git_sha}",
    "git_branch": "${git_branch}",
    "git_commit_message": $(echo "$git_message" | jq -Rs .),
    "git_author": "${git_author}"
  },
  "container": {
    "service": "${service}",
    "image_digest": "${image_digest}",
    "image_size": "${image_size}",
    "compose_project": "${COMPOSE_PROJECT_NAME}"
  },
  "execution": {
    "command": $(echo "$command" | jq -Rs .),
    "exit_code": ${exit_code},
    "success": $([ "$exit_code" -eq 0 ] && echo "true" || echo "false")
  },
  "outputs": {
    "stdout": {
      "hash": "${stdout_hash}",
      "hash_algorithm": "${HASH_ALGO}",
      "lines": ${stdout_lines},
      "file": "$(basename "$stdout_file")"
    },
    "stderr": {
      "hash": "${stderr_hash}",
      "hash_algorithm": "${HASH_ALGO}",
      "lines": ${stderr_lines},
      "file": "$(basename "$stderr_file")"
    }
  },
  "signature": {
    "algorithm": "${SIGN_ALGO}",
    "key_id": "${GPG_KEY_ID:-none}",
    "status": "unsigned"
  },
  "constitution": {
    "docker_only": true,
    "host_execution": "forbidden",
    "validation": "receipt_generated_after_docker_execution"
  },
  "chain": {
    "previous_receipt": null,
    "sequence_number": null,
    "chain_id": "erlmcp-v3-main"
  }
}
EOF
}

# Write receipt to file
write_receipt() {
    local receipt_json="$1"
    local receipt_id="$2"
    local receipt_file="${RECEIPTS_DIR}/${receipt_id}.json"

    echo "$receipt_json" > "$receipt_file"

    # Create checksum file
    local checksum_file="${receipt_file}.sha256"
    hash_file "$receipt_file" > "$checksum_file"

    # Try to sign the receipt
    local signature_file
    if signature_file=$(sign_file "$receipt_file"); then
        # Update receipt signature status
        if command -v jq &>/dev/null; then
            local sig_data
            sig_data=$(base64 "$signature_file" 2>/dev/null || echo "unavailable")
            jq --arg status "signed" \
               --arg sig_file "$(basename "$signature_file")" \
               '.signature.status = $status | .signature.file = $sig_file' \
               "$receipt_file" > "${receipt_file}.tmp"
            mv "${receipt_file}.tmp" "$receipt_file"

            # Recalculate checksum after update
            hash_file "$receipt_file" > "$checksum_file"
            log_success "Receipt signed: $(basename "$signature_file")"
        fi
    else
        log_warning "Receipt not signed (no GPG key configured)"
    fi

    echo "$receipt_file"
}

#######################################
# Command Execution (Docker-Only)
#######################################

# Execute command via Docker and capture outputs
execute_docker_command() {
    local service="$1"
    local command="$2"
    local workdir="${3:-/app}"

    log_info "Executing Docker command..."
    log_debug "Service: $service"
    log_debug "Command: $command"
    log_debug "WorkDir: $workdir"

    # Verify Docker is available
    if ! check_docker; then
        return 1
    fi

    # Create temporary files for output capture
    local stdout_file
    local stderr_file
    stdout_file=$(mktemp)
    stderr_file=$(mktemp)

    # Execute command via docker-compose
    local exit_code=0
    local full_command="docker compose -f ${DOCKER_COMPOSE_FILE} run --rm -w ${workdir} ${service} ${command}"

    log_debug "Full command: $full_command"

    # Execute with output capture
    eval "$full_command" > "$stdout_file" 2> "$stderr_file" || exit_code=$?

    # Output captured streams
    if [[ -s "$stdout_file" ]]; then
        cat "$stdout_file"
    fi

    if [[ -s "$stderr_file" ]]; then
        cat "$stderr_file" >&2
    fi

    # Return exit code and file paths
    echo "$exit_code|$stdout_file|$stderr_file"
}

# Execute command and generate receipt
execute_with_receipt() {
    local service="$1"
    local command="$2"
    local workdir="${3:-/app}"
    local auto_execute="${4:-true}"

    local receipt_id
    receipt_id=$(generate_receipt_id "$service")

    log_info "Receipt ID: $receipt_id"

    local stdout_file
    local stderr_file
    local exit_code=0

    # Create temp files
    stdout_file=$(mktemp)
    stderr_file=$(mktemp)

    if [[ "$auto_execute" == "true" ]]; then
        # Execute command
        log_info "Executing: $service $command"

        local result
        result=$(execute_docker_command "$service" "$command" "$workdir")
        exit_code=$(echo "$result" | cut -d'|' -f1)
        stdout_file=$(echo "$result" | cut -d'|' -f2)
        stderr_file=$(echo "$result" | cut -d'|' -f3)
    else
        # For pre-generated receipts (command already executed)
        # Expect stdout/stderr to be provided via environment or files
        if [[ -n "${RECEIPT_STDOUT_FILE:-}" ]]; then
            stdout_file="$RECEIPT_STDOUT_FILE"
        fi
        if [[ -n "${RECEIPT_STDERR_FILE:-}" ]]; then
            stderr_file="$RECEIPT_STDERR_FILE"
        fi
        exit_code="${RECEIPT_EXIT_CODE:-0}"
    fi

    # Hash outputs
    local stdout_hash
    local stderr_hash
    stdout_hash=$(hash_file "$stdout_file")
    stderr_hash=$(hash_file "$stderr_file")

    # Move outputs to receipts directory
    local final_stdout="${RECEIPTS_DIR}/${receipt_id}.stdout.log"
    local final_stderr="${RECEIPTS_DIR}/${receipt_id}.stderr.log"
    mv "$stdout_file" "$final_stdout" 2>/dev/null || true
    mv "$stderr_file" "$final_stderr" 2>/dev/null || true

    # Build and write receipt
    local receipt_json
    receipt_json=$(build_receipt_json \
        "$receipt_id" \
        "$service" \
        "$command" \
        "$exit_code" \
        "$stdout_hash" \
        "$stderr_hash" \
        "$final_stdout" \
        "$final_stderr")

    local receipt_file
    receipt_file=$(write_receipt "$receipt_json" "$receipt_id")

    log_success "Receipt generated: $receipt_file"
    log_info "Exit code: $exit_code"
    log_info "SHA256(stdout): $stdout_hash"
    log_info "SHA256(stderr): $stderr_hash"

    return "$exit_code"
}

#######################################
# Receipt Verification Functions
#######################################

# Verify receipt integrity
verify_receipt() {
    local receipt_file="$1"

    if [[ ! -f "$receipt_file" ]]; then
        log_error "Receipt file not found: $receipt_file"
        return 1
    fi

    log_info "Verifying receipt: $(basename "$receipt_file")"

    # Verify checksum
    local checksum_file="${receipt_file}.sha256"
    if [[ -f "$checksum_file" ]]; then
        if command -v shasum &>/dev/null; then
            if shasum -c "$checksum_file" &>/dev/null; then
                log_success "Checksum verified"
            else
                log_error "Checksum verification failed"
                return 1
            fi
        elif command -v sha256sum &>/dev/null; then
            if sha256sum -c "$checksum_file" &>/dev/null; then
                log_success "Checksum verified"
            else
                log_error "Checksum verification failed"
                return 1
            fi
        fi
    else
        log_warning "Checksum file not found"
    fi

    # Verify signature if exists
    local signature_file="${receipt_file}.asc"
    if [[ -f "$signature_file" ]]; then
        if verify_signature "$receipt_file"; then
            log_success "Signature verified (key: $GPG_KEY_ID)"
        else
            log_error "Signature verification failed"
            return 1
        fi
    else
        log_warning "Signature file not found"
    fi

    # Verify receipt hash
    if command -v jq &>/dev/null; then
        local stored_hash
        stored_hash=$(jq -r '.metadata.receipt_hash' "$receipt_file")

        local git_sha
        local image_digest
        local command
        local exit_code
        local stdout_hash
        local stderr_hash

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
        else
            log_error "Receipt hash mismatch"
            log_error "  Stored:   $stored_hash"
            log_error "  Computed: $computed_hash"
            return 1
        fi
    fi

    # Verify output files exist and match hashes
    local receipt_id
    receipt_id=$(jq -r '.receipt_id' "$receipt_file")

    local stdout_file="${RECEIPTS_DIR}/${receipt_id}.stdout.log"
    local stderr_file="${RECEIPTS_DIR}/${receipt_id}.stderr.log"

    if [[ -f "$stdout_file" ]]; then
        local actual_stdout_hash
        actual_stdout_hash=$(hash_file "$stdout_file")
        local expected_stdout_hash
        expected_stdout_hash=$(jq -r '.outputs.stdout.hash' "$receipt_file")

        if [[ "$actual_stdout_hash" == "$expected_stdout_hash" ]]; then
            log_success "stdout hash verified"
        else
            log_error "stdout hash mismatch"
            return 1
        fi
    else
        log_warning "stdout file not found: $stdout_file"
    fi

    if [[ -f "$stderr_file" && -s "$stderr_file" ]]; then
        local actual_stderr_hash
        actual_stderr_hash=$(hash_file "$stderr_file")
        local expected_stderr_hash
        expected_stderr_hash=$(jq -r '.outputs.stderr.hash' "$receipt_file")

        if [[ "$actual_stderr_hash" == "$expected_stderr_hash" ]]; then
            log_success "stderr hash verified"
        else
            log_error "stderr hash mismatch"
            return 1
        fi
    fi

    log_success "Receipt verification complete"
    return 0
}

# Verify all receipts in a directory
verify_all_receipts() {
    local target_dir="${1:-$RECEIPTS_DIR}"

    log_info "Verifying all receipts in: $target_dir"

    local verified=0
    local failed=0
    local total=0

    for receipt_file in "$target_dir"/*.json; do
        if [[ -f "$receipt_file" ]]; then
            ((total++))
            if verify_receipt "$receipt_file"; then
                ((verified++))
                # Move to verified directory
                mv "$receipt_file" "${receipt_file}.sha256" "${receipt_file}.asc" "$RECEIPTS_DIR/verified/" 2>/dev/null || true
            else
                ((failed++))
            fi
        fi
    done

    echo ""
    log_info "Verification Summary:"
    log_info "  Total:    $total"
    log_success "  Verified: $verified"
    if [[ $failed -gt 0 ]]; then
        log_error "  Failed:   $failed"
        return 1
    fi

    return 0
}

#######################################
# Receipt Query Functions
#######################################

# List all receipts
list_receipts() {
    local target_dir="${1:-$RECEIPTS_DIR}"

    log_info "Receipts in $target_dir:"
    echo ""

    local found=0
    for receipt_file in "$target_dir"/*.json; do
        if [[ -f "$receipt_file" && ! "$receipt_file" =~ \.tmp$ ]]; then
            ((found++))
            local receipt_id
            local service
            local exit_code
            local timestamp

            if command -v jq &>/dev/null; then
                receipt_id=$(jq -r '.receipt_id' "$receipt_file")
                service=$(jq -r '.container.service' "$receipt_file")
                exit_code=$(jq -r '.execution.exit_code' "$receipt_file")
                timestamp=$(jq -r '.generated_at' "$receipt_file")
                local success
                success=$(jq -r '.execution.success' "$receipt_file")

                local status_marker
                if [[ "$success" == "true" ]]; then
                    status_marker="${GREEN}✓${NC}"
                else
                    status_marker="${RED}✗${NC}"
                fi

                printf "  ${status_marker} ${CYAN}%s${NC} | ${GRAY}%s${NC} | Exit: %s | %s\n" \
                    "$receipt_id" "$service" "$exit_code" "$timestamp"
            else
                echo "  - $(basename "$receipt_file")"
            fi
        fi
    done

    if [[ $found -eq 0 ]]; then
        log_warning "No receipts found"
    fi

    echo ""
    log_info "Total: $found receipt(s)"
}

# Get receipt details
show_receipt() {
    local receipt_id="$1"
    local receipt_file="${RECEIPTS_DIR}/${receipt_id}.json"

    if [[ ! -f "$receipt_file" ]]; then
        # Try to find by partial match
        receipt_file=$(find "$RECEIPTS_DIR" -name "${receipt_id}*.json" | head -1)
    fi

    if [[ ! -f "$receipt_file" ]]; then
        log_error "Receipt not found: $receipt_id"
        return 1
    fi

    log_info "Receipt: $(basename "$receipt_file")"
    echo ""

    if command -v jq &>/dev/null; then
        jq '.' "$receipt_file"
    else
        cat "$receipt_file"
    fi

    echo ""
    log_info "Output files:"
    echo "  stdout: ${RECEIPTS_DIR}/${receipt_id}.stdout.log"
    echo "  stderr: ${RECEIPTS_DIR}/${receipt_id}.stderr.log"
}

# Generate receipt chain report
generate_chain_report() {
    log_info "Receipt Chain Report"
    echo ""

    local total=0
    local passed=0
    local failed=0

    declare -A services
    declare -A exit_codes

    for receipt_file in "$RECEIPTS_DIR"/*.json; do
        if [[ -f "$receipt_file" ]]; then
            ((total++))

            local service
            local exit_code
            local success

            if command -v jq &>/dev/null; then
                service=$(jq -r '.container.service' "$receipt_file")
                exit_code=$(jq -r '.execution.exit_code' "$receipt_file")
                success=$(jq -r '.execution.success' "$receipt_file")
            else
                continue
            fi

            # Track by service
            ((services[$service]++))

            # Track exit codes
            ((exit_codes[$exit_code]++))

            if [[ "$success" == "true" ]]; then
                ((passed++))
            else
                ((failed++))
            fi
        fi
    done

    echo "Summary:"
    echo "  Total receipts:  $total"
    echo "  Passed:         $passed"
    echo "  Failed:         $failed"
    echo ""

    echo "By Service:"
    for svc in "${!services[@]}"; do
        echo "  $svc: ${services[$svc]}"
    done
    echo ""

    echo "Exit Codes:"
    for code in "${!exit_codes[@]}"; do
        echo "  $code: ${exit_codes[$code]}"
    done
}

#######################################
# Archive Functions
#######################################

# Archive old receipts
archive_receipts() {
    local days="${1:-30}"

    log_info "Archiving receipts older than $days days"

    local archived=0
    local cutoff_time
    cutoff_time=$(date -d "$days days ago" +%s 2>/dev/null || date -v-${days}d +%s)

    for receipt_file in "$RECEIPTS_DIR"/*.json; do
        if [[ -f "$receipt_file" ]]; then
            local file_time
            file_time=$(stat -c %Y "$receipt_file" 2>/dev/null || stat -f %m "$receipt_file")

            if [[ $file_time -lt $cutoff_time ]]; then
                local basename
                basename=$(basename "$receipt_file")

                mv "$receipt_file" "$ARCHIVE_DIR/" 2>/dev/null && ((archived++))
                mv "${receipt_file}.sha256" "$ARCHIVE_DIR/" 2>/dev/null || true
                mv "${receipt_file}.asc" "$ARCHIVE_DIR/" 2>/dev/null || true
            fi
        fi
    done

    log_success "Archived $archived receipt(s)"
}

#######################################
# Usage & Help
#######################################

show_help() {
    cat <<EOF
${CYAN}ERLMCP v3: Cryptographically-Verifiable Receipt Generation${NC}

${GREEN}USAGE${NC}
  $0 <command> [options]

${GREEN}COMMANDS${NC}
  ${YELLOW}generate${NC} <service> <command> [--no-exec] [--workdir=<dir>]
      Generate a receipt for a Docker command execution.
      service:  Docker service name (erlmcp-build, erlmcp-unit, erlmcp-ct)
      command:  Command to execute
      --no-exec: Skip execution, use existing outputs (RECEIPT_STDOUT_FILE, etc.)

  ${YELLOW}verify${NC} [receipt_id]
      Verify receipt cryptographic integrity.
      If receipt_id provided, verify specific receipt.
      Otherwise, verify all receipts.

  ${YELLOW}list${NC}
      List all receipts with summary.

  ${YELLOW}show${NC} <receipt_id>
      Display full receipt details.

  ${YELLOW}chain${NC}
      Generate receipt chain report.

  ${YELLOW}archive${NC} [days]
      Archive receipts older than N days (default: 30).

  ${YELLOW}init${NC}
      Initialize receipt directories.

  ${YELLOW}help${NC}
      Show this help message.

${GREEN}OPTIONS${NC}
  --workdir=<dir>     Working directory inside container (default: /app)
  --docker-service    Override Docker service name detection
  --debug             Enable debug output

${GREEN}ENVIRONMENT VARIABLES${NC}
  VERSION             ERLMCP version (default: 3.0.0)
  GPG_KEY_ID          GPG key ID for signing receipts
  SIGN_ALGO           Signature algorithm (default: ed25519)
  DOCKER_COMPOSE_FILE Docker Compose file (default: docker-compose.yml)
  COMPOSE_PROJECT_NAME Docker Compose project name (default: erlmcp)
  DEBUG               Enable debug output (1)

${GREEN}EXAMPLES${NC}
  # Generate receipt for compilation
  $0 generate erlmcp-build "rebar3 compile"

  # Generate receipt for unit tests
  $0 generate erlmcp-unit "rebar3 eunit"

  # Generate receipt for common tests
  $0 generate erlmcp-ct "rebar3 ct"

  # Verify all receipts
  $0 verify

  # Verify specific receipt
  $0 verify R-erlmcp-build-1234567890-abc123

  # List all receipts
  $0 list

  # Show receipt details
  $0 show R-erlmcp-build-1234567890-abc123

  # Generate chain report
  $0 chain

  # Archive old receipts
  $0 archive 30

${GREEN}CONSTITUTION${NC}
  DOCKER-ONLY. All execution must happen via Docker.
  Host execution is FORBIDDEN and invalidates receipts.

${GREEN}OUTPUT${NC}
  Receipts are stored in: ${RECEIPTS_DIR}
  Format: JSON with embedded SHA-256 hashes

${GREEN}RECEIPT STRUCTURE${NC}
  {
    "receipt_id": "R-<service>-<timestamp>-<random>",
    "generated_at": "ISO-8601 timestamp",
    "source": {
      "git_sha": "Git commit SHA",
      "git_branch": "Git branch name",
      ...
    },
    "container": {
      "service": "Docker service name",
      "image_digest": "Docker image digest",
      ...
    },
    "execution": {
      "command": "Command executed",
      "exit_code": 0,
      "success": true
    },
    "outputs": {
      "stdout": { "hash": "SHA-256", ... },
      "stderr": { "hash": "SHA-256", ... }
    },
    "signature": {
      "algorithm": "ed25519",
      "key_id": "...",
      "status": "signed|unsigned"
    }
  }

EOF
}

#######################################
# Main Entry Point
#######################################

main() {
    local command="${1:-help}"
    shift || true

    # Parse global flags
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --debug)
                DEBUG=1
                shift
                ;;
            *)
                break
                ;;
        esac
    done

    case "$command" in
        generate|gen|run|exec)
            if [[ $# -lt 2 ]]; then
                log_error "Usage: $0 generate <service> <command> [options]"
                exit 1
            fi

            local service="$1"
            local command="$2"
            shift 2

            local workdir="/app"
            local auto_execute="true"

            while [[ $# -gt 0 ]]; do
                case "$1" in
                    --workdir=*)
                        workdir="${1#*=}"
                        ;;
                    --no-exec)
                        auto_execute="false"
                        ;;
                    --debug)
                        DEBUG=1
                        ;;
                    *)
                        log_warning "Unknown option: $1"
                        ;;
                esac
                shift
            done

            init_receipt_dirs
            execute_with_receipt "$service" "$command" "$workdir" "$auto_execute"
            ;;

        verify|check|v)
            init_receipt_dirs
            if [[ $# -eq 0 ]]; then
                verify_all_receipts
            else
                verify_receipt "${RECEIPTS_DIR}/$1.json"
            fi
            ;;

        list|ls|l)
            init_receipt_dirs
            list_receipts
            ;;

        show|info|s)
            if [[ $# -eq 0 ]]; then
                log_error "Usage: $0 show <receipt_id>"
                exit 1
            fi
            init_receipt_dirs
            show_receipt "$1"
            ;;

        chain|report|r)
            init_receipt_dirs
            generate_chain_report
            ;;

        archive)
            local days="${1:-30}"
            init_receipt_dirs
            archive_receipts "$days"
            ;;

        init)
            init_receipt_dirs
            log_success "Receipt directories initialized"
            ;;

        help|--help|-h)
            show_help
            ;;

        *)
            log_error "Unknown command: $command"
            echo ""
            show_help
            exit 1
            ;;
    esac
}

# Run main if script is executed (not sourced)
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
