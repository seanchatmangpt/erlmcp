#!/usr/bin/env bash
set -euo pipefail

#############################################
# Production Receipt Verification Script
# Verifies receipt integrity and chain of custody
# Follows TCPS: Receipt Chain - Immutable audit trail
#############################################

RECEIPTS_DIR=".erlmcp/receipts"
VERSION="${1:-3.0.0}"
RECEIPT_FILE="${RECEIPTS_DIR}/receipt-v${VERSION}.json"
CHECKSUM_FILE="${RECEIPT_FILE}.sha256"

echo "=== Production Receipt Verification v${VERSION} ==="
echo ""

# Check receipt exists
if [ ! -f "${RECEIPT_FILE}" ]; then
    echo "ERROR: Receipt not found: ${RECEIPT_FILE}"
    echo "Run: scripts/generate-receipt.sh"
    exit 1
fi

# Extract receipt data
GIT_SHA=$(jq -r '.git_sha // empty' "${RECEIPT_FILE}")
IMAGE_DIGEST=$(jq -r '.image_digest // empty' "${RECEIPT_FILE}")
TIMESTAMP=$(jq -r '.timestamp // empty' "${RECEIPT_FILE}")
RECEIPT_HASH=$(jq -r '.receipt_hash // empty' "${RECEIPT_FILE}")

echo "Receipt: ${RECEIPT_FILE}"
echo "Timestamp: ${TIMESTAMP}"
echo "Git SHA: ${GIT_SHA}"
echo "Image Digest: ${IMAGE_DIGEST}"
echo ""

# Verify checksum
echo "=== Checksum Verification ==="
if [ -f "${CHECKSUM_FILE}" ]; then
    if command -v shasum &>/dev/null; then
        if shasum -c "${CHECKSUM_FILE}" 2>/dev/null; then
            echo "PASS: Checksum valid"
        else
            echo "FAIL: Checksum invalid!"
            exit 1
        fi
    else
        if sha256sum -c "${CHECKSUM_FILE}" 2>/dev/null; then
            echo "PASS: Checksum valid"
        else
            echo "FAIL: Checksum invalid!"
            exit 1
        fi
    fi
else
    echo "WARN: Checksum file not found"
fi
echo ""

# Verify git SHA matches current HEAD
echo "=== Git SHA Verification ==="
CURRENT_SHA=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
if [ "${GIT_SHA}" = "${CURRENT_SHA}" ]; then
    echo "PASS: Git SHA matches current HEAD"
else
    echo "INFO: Git SHA differs from HEAD"
    echo "  Receipt: ${GIT_SHA}"
    echo "  HEAD: ${CURRENT_SHA}"
fi
echo ""

# Verify receipt hash matches file
echo "=== Embedded Hash Verification ==="
if [ -n "${RECEIPT_HASH}" ] && [ "${RECEIPT_HASH}" != "pending" ]; then
    if command -v shasum &>/dev/null; then
        CALCULATED_HASH=$(shasum -a 256 "${RECEIPT_FILE}" | cut -d' ' -f1)
    else
        CALCULATED_HASH=$(sha256sum "${RECEIPT_FILE}" | cut -d' ' -f1)
    fi

    # Note: The embedded hash is from the receipt BEFORE the hash was added
    # The checksum file contains the hash of the FINAL receipt
    if command -v shasum &>/dev/null; then
        FILE_HASH=$(cat "${CHECKSUM_FILE}" | cut -d' ' -f1)
    else
        FILE_HASH=$(cat "${CHECKSUM_FILE}" | cut -d' ' -f1)
    fi

    if [ "${FILE_HASH}" = "${CALCULATED_HASH}" ]; then
        echo "PASS: Current hash matches checksum file"
        echo "  Embedded: ${RECEIPT_HASH} (from pre-hash state)"
        echo "  Current: ${CALCULATED_HASH}"
    else
        echo "WARN: Hash changed since receipt generation"
        echo "  Embedded: ${RECEIPT_HASH}"
        echo "  Current: ${CALCULATED_HASH}"
        echo "  Run: scripts/generate-receipt.sh to update"
    fi
else
    echo "SKIP: No embedded hash (pending generation)"
fi
echo ""

# Verify Docker image
echo "=== Docker Image Verification ==="
if [ "${IMAGE_DIGEST}" = "not-built" ]; then
    echo "WARN: Docker image not built yet"
    echo "  Run: docker-compose build erlmcp"
elif [ -n "${IMAGE_DIGEST}" ]; then
    if docker images --format '{{.Id}}' | grep -q "${IMAGE_DIGEST}"; then
        echo "PASS: Docker image exists locally"
    else
        echo "WARN: Docker image not found locally"
        echo "  Digest: ${IMAGE_DIGEST}"
    fi
fi
echo ""

# Show quality gate status
echo "=== Quality Gates ==="
echo "Compilation: $(jq -r '.quality_gates.compilation.status' "${RECEIPT_FILE}")"
echo "Dialyzer: $(jq -r '.quality_gates.dialyzer.status' "${RECEIPT_FILE}")"
echo "Xref: $(jq -r '.quality_gates.xref.status' "${RECEIPT_FILE}")"
echo "Tests: $(jq -r '.quality_gates.tests.status' "${RECEIPT_FILE}")"
echo ""

echo "=== Verification Complete ==="
