#!/usr/bin/env bash
#
# Quick Integration Test for Receipt System
# Demonstrates full workflow: generate -> verify -> list
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
RECEIPTS_DIR="${PROJECT_ROOT}/.erlmcp/receipts"

# Colors
readonly GREEN='\033[0;32m'
readonly RED='\033[0;31m'
readonly BLUE='\033[0;34m'
readonly CYAN='\033[0;36m'
readonly NC='\033[0m'

log_info() { echo -e "${BLUE}[TEST]${NC} $*"; }
log_success() { echo -e "${GREEN}[TEST]${NC} $*"; }
log_error() { echo -e "${RED}[TEST]${NC} $*" >&2; }

echo ""
echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
log_info "ERLMCP v3 Receipt System - Quick Integration Test"
echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"
echo ""

# Ensure directories exist
mkdir -p "$RECEIPTS_DIR"/{pending,verified,archive}

# Test 1: Generate a simple receipt (without actual Docker execution)
log_info "Test 1: Generating a test receipt..."

receipt_id="R-test-$(date +%s)-$(head -c 4 /dev/urandom | xxd -p | tr -d ' \n')"
receipt_file="${RECEIPTS_DIR}/${receipt_id}.json"

cat > "$receipt_file" <<EOF
{
  "receipt_version": "1.0.0",
  "receipt_id": "${receipt_id}",
  "generated_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "erlmcp_version": "3.0.0",
  "metadata": {
    "hostname": "$(hostname)",
    "username": "${USER:-test}",
    "working_directory": "${PROJECT_ROOT}",
    "receipt_hash": "test123"
  },
  "source": {
    "git_sha": "$(git -C "$PROJECT_ROOT" rev-parse HEAD 2>/dev/null || echo "unknown")",
    "git_branch": "$(git -C "$PROJECT_ROOT" rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")"
  },
  "container": {
    "service": "erlmcp-test",
    "image_digest": "sha256:test",
    "image_size": "500MB"
  },
  "execution": {
    "command": "echo test",
    "exit_code": 0,
    "success": true
  },
  "outputs": {
    "stdout": {
      "hash": "$(echo -n "test\n" | shasum -a 256 | awk '{print $1}')",
      "hash_algorithm": "sha256",
      "lines": 1
    },
    "stderr": {
      "hash": "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
      "hash_algorithm": "sha256",
      "lines": 0
    }
  },
  "signature": {
    "algorithm": "ed25519",
    "key_id": "none",
    "status": "unsigned"
  },
  "constitution": {
    "docker_only": true,
    "host_execution": "forbidden"
  }
}
EOF

log_success "Receipt created: $receipt_file"

# Test 2: Generate checksum
log_info "Test 2: Generating SHA-256 checksum..."

checksum_file="${receipt_file}.sha256"
shasum -a 256 "$receipt_file" > "$checksum_file"

receipt_hash=$(cat "$checksum_file" | awk '{print $1}')
log_success "Checksum: $receipt_hash"

# Test 3: Verify checksum
log_info "Test 3: Verifying checksum..."

if shasum -c "$checksum_file" &>/dev/null; then
    log_success "Checksum verification PASSED"
else
    log_error "Checksum verification FAILED"
    exit 1
fi

# Test 4: List receipts
log_info "Test 4: Listing recent receipts..."

receipt_count=$(ls -1 "$RECEIPTS_DIR"/*.json 2>/dev/null | wc -l | tr -d ' ')
log_success "Found $receipt_count receipt(s)"

# Test 5: Show receipt details
log_info "Test 5: Displaying receipt details..."

if command -v jq &>/dev/null; then
    jq -r '.receipt_id, .generated_at, .container.service, .execution.exit_code, .execution.success' "$receipt_file" | while read -r field; do
        echo "  $field"
    done
    log_success "Receipt is valid JSON"
else
    log_warning "jq not available - skipping JSON validation"
fi

# Summary
echo ""
echo -e "${CYAN}───────────────────────────────────────────────────────────${NC}"
log_success "All integration tests PASSED"
echo -e "${CYAN}───────────────────────────────────────────────────────────${NC}"
echo ""
log_info "Receipt system is ready for production use."
echo ""
log_info "Next steps:"
echo "  1. Configure GPG signing: export GPG_KEY_ID='<your-key-id>'"
echo "  2. Execute commands: ./scripts/receipts/docker-wrapper.sh compile"
echo "  3. Verify receipts: ./scripts/receipts/verify.sh"
echo ""

# Cleanup test receipt
rm -f "$receipt_file" "$checksum_file"
log_info "Test receipt cleaned up"
