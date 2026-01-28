#!/bin/bash
# upload_sbom.sh - Generate SBOM and upload to GCP
#
# Usage:
#   ./upload_sbom.sh [version] [project-id] [region]
#
# Example:
#   ./upload_sbom.sh 0.7.0 taiea-v1 us-central1

set -e

# Configuration
VERSION="${1:-0.7.0}"
PROJECT_ID="${2:-taiea-v1}"
REGION="${3:-us-central1}"
ARTIFACT_REGISTRY_REPO="erlmcp-repo"
IMAGE_NAME="erlmcp"

# Derived variables
IMAGE_REGISTRY="${REGION}-docker.pkg.dev"
IMAGE_URI="${IMAGE_REGISTRY}/${PROJECT_ID}/${ARTIFACT_REGISTRY_REPO}/${IMAGE_NAME}:${VERSION}"
SBOM_BUCKET="gs://${PROJECT_ID}-sbom-artifacts"

echo "=== erlmcp SBOM Generation and Upload Script ==="
echo "Project ID: ${PROJECT_ID}"
echo "Version: ${VERSION}"
echo "Image URI: ${IMAGE_URI}"
echo "SBOM Bucket: ${SBOM_BUCKET}"
echo ""

# Step 1: Check if syft is installed
echo "[1/5] Checking for Syft..."
if ! command -v syft &> /dev/null; then
    echo "Syft not found. Installing..."
    curl -sSfL https://raw.githubusercontent.com/anchore/syft/main/install.sh | sh -s -- -b /usr/local/bin
fi
echo "✓ Syft available"
echo ""

# Step 2: Generate SPDX SBOM
echo "[2/5] Generating SPDX SBOM..."
syft packages ${IMAGE_URI} \
    --output spdx-json \
    --file /tmp/erlmcp-sbom.spdx.json

echo "✓ SPDX SBOM generated"
echo "  File: /tmp/erlmcp-sbom.spdx.json"
echo "  Size: $(du -h /tmp/erlmcp-sbom.spdx.json | cut -f1)"
echo ""

# Step 3: Generate CycloneDX SBOM
echo "[3/5] Generating CycloneDX SBOM..."
syft packages ${IMAGE_URI} \
    --output cyclonedx-json \
    --file /tmp/erlmcp-sbom.cyclonedx.json

echo "✓ CycloneDX SBOM generated"
echo "  File: /tmp/erlmcp-sbom.cyclonedx.json"
echo "  Size: $(du -h /tmp/erlmcp-sbom.cyclonedx.json | cut -f1)"
echo ""

# Step 4: Create GCS bucket if needed
echo "[4/5] Preparing GCS bucket..."
if gsutil ls ${SBOM_BUCKET} &>/dev/null; then
    echo "✓ Bucket exists: ${SBOM_BUCKET}"
else
    echo "Creating bucket: ${SBOM_BUCKET}"
    gsutil mb -p ${PROJECT_ID} -b on ${SBOM_BUCKET}
    echo "✓ Bucket created"
fi
echo ""

# Step 5: Upload SBOMs
echo "[5/5] Uploading SBOMs to GCS..."
gsutil cp /tmp/erlmcp-sbom.spdx.json \
    ${SBOM_BUCKET}/erlmcp-${VERSION}-sbom.spdx.json

gsutil cp /tmp/erlmcp-sbom.cyclonedx.json \
    ${SBOM_BUCKET}/erlmcp-${VERSION}-sbom.cyclonedx.json

echo "✓ SBOMs uploaded"
echo ""

# Verify uploads
echo "=== Upload Complete ==="
echo "SPDX SBOM:"
echo "  ${SBOM_BUCKET}/erlmcp-${VERSION}-sbom.spdx.json"
echo ""
echo "CycloneDX SBOM:"
echo "  ${SBOM_BUCKET}/erlmcp-${VERSION}-sbom.cyclonedx.json"
echo ""
echo "Next steps:"
echo "  1. Review SBOMs in GCS Console"
echo "  2. Deploy: ./tools/gcp/deploy_helm.sh ${VERSION} ${PROJECT_ID} ${REGION}"
echo "  3. Verify: ./tools/gcp/verify_deployment.sh ${PROJECT_ID}"
