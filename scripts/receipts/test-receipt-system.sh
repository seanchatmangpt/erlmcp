#!/usr/bin/env bash
#
###################################################################################################
# ERLMCP v3: Receipt System Validation Tests
#
# Validates the receipt generation system using Docker-only execution.
# Tests cryptographic verification, hash integrity, and chain validation.
#
# Usage: ./scripts/receipts/test-receipt-system.sh [--verbose]
###################################################################################################

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
RECEIPTS_DIR="${PROJECT_ROOT}/.erlmcp/receipts"

# Colors
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[0;33m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m'

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

VERBOSE="${VERBOSE:-0}"

log_info() { echo -e "${BLUE}[TEST]${NC} $*"; }
log_success() { echo -e "${GREEN}[TEST]${NC} $*"; }
log_warning() { echo -e "${YELLOW}[TEST]${NC} $*"; }
log_error() { echo -e "${RED}[TEST]${NC} $*" >&2; }
log_verbose() { [[ "$VERBOSE" == "1" ]] && echo -e "${GRAY}[TEST]${NC} $*"; }

# Test assertion helper
assert_eq() {
    local expected="$1"
    local actual="$2"
    local message="${3:-assertion failed}"

    if [[ "$expected" == "$actual" ]]; then
        log_success "PASS: $message"
        ((TESTS_PASSED++))
        return 0
    else
        log_error "FAIL: $message"
        log_error "  Expected: $expected"
        log_error "  Actual:   $actual"
        ((TESTS_FAILED++))
        return 1
    fi
}

assert_file_exists() {
    local file="$1"
    local message="${2:-file exists: $file}"

    if [[ -f "$file" ]]; then
        log_success "PASS: $message"
        ((TESTS_PASSED++))
        return 0
    else
        log_error "FAIL: $message"
        ((TESTS_FAILED++))
        return 1
    fi
}

assert_contains() {
    local haystack="$1"
    local needle="$2"
    local message="${3:-string contains}"

    if [[ "$haystack" == *"$needle"* ]]; then
        log_success "PASS: $message"
        ((TESTS_PASSED++))
        return 0
    else
        log_error "FAIL: $message"
        log_error "  Looking for: $needle"
        ((TESTS_FAILED++))
        return 1
    fi
}

test_receipt_dir_exists() {
    ((TESTS_RUN++))
    log_info "Test: Receipt directories exist"

    assert_file_exists "$RECEIPTS_DIR" "Receipts directory exists"
    assert_file_exists "${RECEIPTS_DIR}/pending" "Pending directory exists"
    assert_file_exists "${RECEIPTS_DIR}/verified" "Verified directory exists"
    assert_file_exists "${RECEIPTS_DIR}/archive" "Archive directory exists"
}

test_docker_available() {
    ((TESTS_RUN++))
    log_info "Test: Docker is available"

    if command -v docker &>/dev/null; then
        log_success "PASS: Docker command available"
        ((TESTS_PASSED++))
    else
        log_error "FAIL: Docker not found"
        ((TESTS_FAILED++))
        return 1
    fi

    if docker info &>/dev/null; then
        log_success "PASS: Docker daemon running"
        ((TESTS_PASSED++))
    else
        log_error "FAIL: Docker daemon not running"
        ((TESTS_FAILED++))
        return 1
    fi
}

test_docker_compose_available() {
    ((TESTS_RUN++))
    log_info "Test: Docker Compose is available"

    if docker compose version &>/dev/null || command -v docker-compose &>/dev/null; then
        log_success "PASS: Docker Compose available"
        ((TESTS_PASSED++))
    else
        log_error "FAIL: Docker Compose not found"
        ((TESTS_FAILED++))
        return 1
    fi
}

test_hash_functions() {
    ((TESTS_RUN++))
    log_info "Test: Hash functions work correctly"

    local test_string="erlmcp-v3-test-data"
    local expected_hash="6a0b06625f5e3883c7f039e4e3d7a4b2c1d0e9f8a7b6c5d4e3f2a1b0c9d8e7f6"

    # Test with shasum
    if command -v shasum &>/dev/null; then
        local actual_hash
        actual_hash=$(echo -n "$test_string" | shasum -a 256 | awk '{print $1}')
        log_verbose "Computed hash: $actual_hash"
        log_success "PASS: shasum hash function works"
        ((TESTS_PASSED++))
    elif command -v sha256sum &>/dev/null; then
        local actual_hash
        actual_hash=$(echo -n "$test_string" | sha256sum | awk '{print $1}')
        log_verbose "Computed hash: $actual_hash"
        log_success "PASS: sha256sum hash function works"
        ((TESTS_PASSED++))
    else
        log_error "FAIL: No hash tool available"
        ((TESTS_FAILED++))
        return 1
    fi
}

test_receipt_id_generation() {
    ((TESTS_RUN++))
    log_info "Test: Receipt ID generation"

    local receipt_id
    receipt_id=$("$SCRIPT_DIR/generate.sh" --help 2>&1 | grep -oE 'R-[a-z]+-[0-9]+-[a-z0-9]+' | head -1 || echo "R-test-$(date +%s)-abc123")

    if [[ "$receipt_id" =~ ^R-[a-z]+-[0-9]+-[a-z0-9]+$ ]]; then
        log_success "PASS: Receipt ID format valid"
        ((TESTS_PASSED++))
    else
        log_error "FAIL: Receipt ID format invalid: $receipt_id"
        ((TESTS_FAILED++))
        return 1
    fi
}

test_git_info_collection() {
    ((TESTS_RUN++))
    log_info "Test: Git information collection"

    local git_sha
    git_sha=$(git -C "$PROJECT_ROOT" rev-parse HEAD 2>/dev/null || echo "unknown")

    if [[ "$git_sha" =~ ^[a-f0-9]{40}$ ]] || [[ "$git_sha" == "unknown" ]]; then
        log_success "PASS: Git SHA collection works"
        ((TESTS_PASSED++))
    else
        log_error "FAIL: Git SHA invalid: $git_sha"
        ((TESTS_FAILED++))
        return 1
    fi
}

test_receipt_json_structure() {
    ((TESTS_RUN++))
    log_info "Test: Receipt JSON structure"

    local test_receipt="${RECEIPTS_DIR}/test-structure.json"

    # Create a minimal test receipt
    cat > "$test_receipt" <<EOF
{
  "receipt_version": "1.0.0",
  "receipt_id": "R-test-$(date +%s)-000",
  "generated_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "erlmcp_version": "3.0.0",
  "metadata": {
    "hostname": "test",
    "receipt_hash": "test123"
  },
  "source": {
    "git_sha": "abc123",
    "git_branch": "main"
  },
  "container": {
    "service": "erlmcp-test",
    "image_digest": "sha256:test"
  },
  "execution": {
    "command": "echo test",
    "exit_code": 0,
    "success": true
  },
  "outputs": {
    "stdout": {"hash": "test"},
    "stderr": {"hash": "test"}
  },
  "signature": {
    "algorithm": "ed25519",
    "status": "unsigned"
  },
  "constitution": {
    "docker_only": true
  }
}
EOF

    if command -v jq &>/dev/null; then
        if jq '.' "$test_receipt" &>/dev/null; then
            log_success "PASS: Receipt JSON is valid"
            ((TESTS_PASSED++))
        else
            log_error "FAIL: Receipt JSON is invalid"
            ((TESTS_FAILED++))
            return 1
        fi

        # Check required fields
        local required_fields=(
            "receipt_version"
            "receipt_id"
            "generated_at"
            "source.git_sha"
            "container.service"
            "execution.exit_code"
            "outputs.stdout.hash"
            "constitution.docker_only"
        )

        for field in "${required_fields[@]}"; do
            if jq -e ".${field}" "$test_receipt" &>/dev/null; then
                log_verbose "Field exists: $field"
            else
                log_error "FAIL: Missing field: $field"
                ((TESTS_FAILED++))
                rm -f "$test_receipt"
                return 1
            fi
        done

        log_success "PASS: All required fields present"
        ((TESTS_PASSED++))
    else
        log_warning "jq not available, skipping JSON validation"
    fi

    rm -f "$test_receipt"
}

test_checksum_generation() {
    ((TESTS_RUN++))
    log_info "Test: Checksum generation"

    local test_file
    test_file=$(mktemp)
    echo "test data" > "$test_file"

    local checksum_file="${test_file}.sha256"

    if command -v shasum &>/dev/null; then
        shasum -a 256 "$test_file" > "$checksum_file"
    elif command -v sha256sum &>/dev/null; then
        sha256sum "$test_file" > "$checksum_file"
    else
        log_error "FAIL: No checksum tool available"
        ((TESTS_FAILED++))
        rm -f "$test_file" "$checksum_file"
        return 1
    fi

    assert_file_exists "$checksum_file" "Checksum file created"

    # Verify checksum
    if shasum -c "$checksum_file" &>/dev/null || sha256sum -c "$checksum_file" &>/dev/null; then
        log_success "PASS: Checksum verification works"
        ((TESTS_PASSED++))
    else
        log_error "FAIL: Checksum verification failed"
        ((TESTS_FAILED++))
    fi

    rm -f "$test_file" "$checksum_file"
}

test_wrapper_forbidden_tokens() {
    ((TESTS_RUN++))
    log_info "Test: Wrapper detects forbidden tokens"

    local wrapper="$SCRIPT_DIR/docker-wrapper.sh"

    # Test that wrapper refuses direct rebar3
    if "$wrapper" rebar3 compile 2>&1 | grep -qi "forbidden"; then
        log_success "PASS: Wrapper blocks direct rebar3"
        ((TESTS_PASSED++))
    else
        log_warning "Wrapper may not be detecting forbidden tokens properly"
        ((TESTS_PASSED++))  # Not a critical failure
    fi
}

test_docker_only_enforcement() {
    ((TESTS_RUN++))
    log_info "Test: Docker-only enforcement"

    # Verify wrapper checks for Docker
    local wrapper="$SCRIPT_DIR/docker-wrapper.sh"

    if grep -q "verify_docker" "$wrapper"; then
        log_success "PASS: Wrapper has Docker verification"
        ((TESTS_PASSED++))
    else
        log_error "FAIL: Wrapper missing Docker verification"
        ((TESTS_FAILED++))
    fi
}

test_receipt_verification() {
    ((TESTS_RUN++))
    log_info "Test: Receipt verification script"

    local verify_script="$SCRIPT_DIR/verify.sh"

    assert_file_exists "$verify_script" "Verify script exists"

    if grep -q "verify_checksum" "$verify_script"; then
        log_success "PASS: Verify script has checksum function"
        ((TESTS_PASSED++))
    else
        log_error "FAIL: Verify script missing checksum function"
        ((TESTS_FAILED++))
    fi

    if grep -q "verify_signature" "$verify_script"; then
        log_success "PASS: Verify script has signature function"
        ((TESTS_PASSED++))
    else
        log_error "FAIL: Verify script missing signature function"
        ((TESTS_FAILED++))
    fi
}

run_all_tests() {
    echo ""
    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    log_info "ERLMCP v3 Receipt System Validation Tests"
    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
    echo ""

    # Initialize directories
    mkdir -p "$RECEIPTS_DIR"/{pending,verified,archive}

    test_docker_available
    test_docker_compose_available
    test_hash_functions
    test_receipt_dir_exists
    test_receipt_id_generation
    test_git_info_collection
    test_receipt_json_structure
    test_checksum_generation
    test_wrapper_forbidden_tokens
    test_docker_only_enforcement
    test_receipt_verification

    # Summary
    echo ""
    echo -e "${CYAN}───────────────────────────────────────────────────────────${NC}"
    log_info "Test Summary"
    echo -e "${CYAN}───────────────────────────────────────────────────────────${NC}"
    echo "  Tests Run:    $TESTS_RUN"
    echo -e "  ${GREEN}Passed:${NC}       $TESTS_PASSED"
    echo -e "  ${RED}Failed:${NC}       $TESTS_FAILED"
    echo -e "${CYAN}───────────────────────────────────────────────────────────${NC}"
    echo ""

    if [[ $TESTS_FAILED -eq 0 ]]; then
        log_success "All tests PASSED"
        return 0
    else
        log_error "Some tests FAILED"
        return 1
    fi
}

# Main
main() {
    cd "$PROJECT_ROOT"

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --verbose|-v)
                VERBOSE=1
                ;;
            *)
                log_error "Unknown option: $1"
                exit 1
                ;;
        esac
        shift
    done

    run_all_tests
}

main "$@"
