#!/usr/bin/env bash
# ============================================================================
# run_gate.sh - Quality Gate Wrapper with Receipt Generation
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION
#
# Wraps any quality gate command and generates:
#   1. Command execution logs
#   2. Mermaid topology diagram
#   3. Receipt hash (SHA256)
#   4. Image digest capture
#
# Invariant: run => (receipt ^ mermaid)
#
# Usage:
#   ./run_gate.sh <gate_name> <command...>
#   ./run_gate.sh compile rebar3 compile
#   ./run_gate.sh ct rebar3 ct --dir=test/integration --cover
#   ./run_gate.sh dialyzer rebar3 dialyzer
#
# Output:
#   /workspace/receipts/<RUN_ID>/
#     meta.txt          - Gate metadata
#     <gate>.log        - Command stdout/stderr
#     topology.mmd      - Mermaid diagram
#     topology_meta.json - JSON metadata
#     receipt.sha256    - Receipt hash
#     image_digest.txt  - Docker image digest (if available)
# ============================================================================

set -euo pipefail

# ============================================================================
# Arguments
# ============================================================================
GATE="${1:?Usage: run_gate.sh <gate_name> <command...>}"
shift
COMMAND=("$@")

if [[ ${#COMMAND[@]} -eq 0 ]]; then
    echo "ERROR: No command specified for gate '$GATE'"
    echo "Usage: run_gate.sh <gate_name> <command...>"
    exit 1
fi

# ============================================================================
# HARD CHECK: Must be running inside Docker container
# ============================================================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IS_DOCKER_SCRIPT="${SCRIPT_DIR}/../dev/is_docker.sh"

check_is_docker() {
    if [[ -f "/.dockerenv" ]]; then
        return 0
    fi
    if [[ -r "/proc/1/cgroup" ]] && grep -Eq "(docker|kubepods|containerd)" /proc/1/cgroup 2>/dev/null; then
        return 0
    fi
    return 1
}

if [[ -x "$IS_DOCKER_SCRIPT" ]]; then
    if ! "$IS_DOCKER_SCRIPT"; then
        echo ""
        echo "============================================"
        echo "ANDON: FORBIDDEN_HOST_EXECUTION"
        echo "============================================"
        echo "Gate:    $GATE"
        echo "Command: ${COMMAND[*]}"
        echo ""
        echo "Refusal: run_gate.sh MUST run inside Docker"
        echo ""
        echo "Correct usage:"
        echo "  docker compose run --rm erlmcp-check ./scripts/receipts/run_gate.sh $GATE ${COMMAND[*]}"
        echo "============================================"
        exit 2
    fi
else
    if ! check_is_docker; then
        echo "ANDON: FORBIDDEN_HOST_EXECUTION"
        exit 2
    fi
fi

# ============================================================================
# Configuration
# ============================================================================
RUN_ID="${RUN_ID:-$(date -u +%Y%m%dT%H%M%SZ)}"
OUT_DIR="${OUT_DIR:-/workspace/receipts/$RUN_ID}"
WORKSPACE="${WORKSPACE:-/workspace}"

# Create output directory
mkdir -p "$OUT_DIR"

# ============================================================================
# Capture Image Digest
# ============================================================================
capture_image_digest() {
    local digest_file="$OUT_DIR/image_digest.txt"

    # Try to get current image info
    if command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1; then
        # If docker socket is mounted, get the image digest
        local image_id
        image_id=$(docker inspect --format='{{.Image}}' "$(hostname)" 2>/dev/null || echo "")
        if [[ -n "$image_id" ]]; then
            echo "IMAGE_ID=$image_id" > "$digest_file"
        fi
    fi

    # Check for image label/env var
    if [[ -n "${ERLMCP_IMAGE_DIGEST:-}" ]]; then
        echo "ERLMCP_IMAGE_DIGEST=$ERLMCP_IMAGE_DIGEST" >> "$digest_file"
    fi

    # Fall back to container info from cgroup
    if [[ ! -f "$digest_file" ]]; then
        echo "IMAGE_ID=not_available" > "$digest_file"
        echo "NOTE=no docker socket mounted, image digest unavailable" >> "$digest_file"
    fi

    # Capture build info if available
    if [[ -f /opt/erlmcp/BUILD_INFO ]]; then
        cat /opt/erlmcp/BUILD_INFO >> "$digest_file"
    fi
}

# ============================================================================
# Write Gate Metadata
# ============================================================================
echo "============================================"
echo "QUALITY GATE: $GATE"
echo "============================================"
echo "RUN_ID:    $RUN_ID"
echo "TIMESTAMP: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "COMMAND:   ${COMMAND[*]}"
echo "OUT_DIR:   $OUT_DIR"
echo "============================================"
echo ""

{
    echo "RUN_ID=$RUN_ID"
    echo "GATE=$GATE"
    echo "TIMESTAMP=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "COMMAND=${COMMAND[*]}"
    echo "HOSTNAME=$(hostname)"
    echo "CONTAINER_ID=$(cat /proc/self/cgroup 2>/dev/null | grep -oP 'docker/\K[a-f0-9]+' | head -1 || echo 'unknown')"
    echo "GIT_SHA=$(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')"
    echo "GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'unknown')"
    echo "OTP_VSN=$(erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]), halt().' 2>/dev/null || echo 'unknown')"
    echo "IS_DOCKER=true"
} | tee "$OUT_DIR/meta.txt"

echo ""

# ============================================================================
# Capture Image Digest
# ============================================================================
capture_image_digest

# ============================================================================
# Execute Gate Command
# ============================================================================
echo "Executing gate command..."
echo ""

set +e
"${COMMAND[@]}" 2>&1 | tee "$OUT_DIR/${GATE}.log"
EXIT_CODE="${PIPESTATUS[0]}"
set -e

echo ""
echo "EXIT_CODE=$EXIT_CODE" | tee -a "$OUT_DIR/meta.txt"

# ============================================================================
# Generate Mermaid Topology (always, even on failure)
# ============================================================================
echo ""
echo "Generating topology proof..."

EMIT_MERMAID="${SCRIPT_DIR}/emit_mermaid.sh"
if [[ -x "$EMIT_MERMAID" ]]; then
    RUN_ID="$RUN_ID" OUT_DIR="$OUT_DIR" "$EMIT_MERMAID" || echo "WARNING: Mermaid generation failed"
else
    echo "WARNING: emit_mermaid.sh not found at $EMIT_MERMAID"
fi

# ============================================================================
# Compute Receipt Hash
# ============================================================================
echo ""
echo "Computing receipt hash..."

RECEIPT_CONTENT=""
for f in "$OUT_DIR/meta.txt" "$OUT_DIR/${GATE}.log" "$OUT_DIR/topology.mmd" "$OUT_DIR/topology_meta.json" "$OUT_DIR/image_digest.txt"; do
    if [[ -f "$f" ]]; then
        RECEIPT_CONTENT+="$(cat "$f")"
    fi
done

if command -v sha256sum >/dev/null 2>&1; then
    RECEIPT_HASH=$(echo "$RECEIPT_CONTENT" | sha256sum | awk '{print $1}')
    echo "$RECEIPT_HASH" > "$OUT_DIR/receipt.sha256"
    echo "RECEIPT_SHA256=$RECEIPT_HASH" | tee -a "$OUT_DIR/meta.txt"
elif command -v shasum >/dev/null 2>&1; then
    RECEIPT_HASH=$(echo "$RECEIPT_CONTENT" | shasum -a 256 | awk '{print $1}')
    echo "$RECEIPT_HASH" > "$OUT_DIR/receipt.sha256"
    echo "RECEIPT_SHA256=$RECEIPT_HASH" | tee -a "$OUT_DIR/meta.txt"
else
    echo "WARNING: sha256sum not available, skipping receipt hash"
fi

# ============================================================================
# Generate Summary
# ============================================================================
echo ""
echo "============================================"
if [[ $EXIT_CODE -eq 0 ]]; then
    echo "GATE PASSED: $GATE"
else
    echo "GATE FAILED: $GATE (exit=$EXIT_CODE)"
fi
echo "============================================"
echo ""
echo "RECEIPT BUNDLE: $OUT_DIR/"
ls -la "$OUT_DIR/"
echo ""
echo "RECEIPT_SHA256: ${RECEIPT_HASH:-not_computed}"
echo ""

# Show first 20 lines of Mermaid diagram
if [[ -f "$OUT_DIR/topology.mmd" ]]; then
    echo "TOPOLOGY (first 20 lines):"
    echo "---"
    head -20 "$OUT_DIR/topology.mmd"
    echo "..."
    echo "---"
fi

# ============================================================================
# Required Output Format (for agent compliance)
# ============================================================================
echo ""
echo "========================================"
echo "RECEIPT OUTPUT (required format)"
echo "========================================"
echo "RECEIPT_DIR=$OUT_DIR"
echo "RECEIPT_SHA256=${RECEIPT_HASH:-not_computed}"
echo "MERMAID=topology.mmd"
echo "GATE=$GATE"
echo "EXIT_CODE=$EXIT_CODE"
echo "========================================"

# Exit with gate's exit code
exit $EXIT_CODE
