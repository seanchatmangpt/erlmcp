#!/bin/bash
# push_image.sh - Push erlmcp Docker image to GCP Artifact Registry
#
# Usage:
#   ./push_image.sh [version] [project-id] [region]
#
# Example:
#   ./push_image.sh 0.7.0 taiea-v1 us-central1

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

echo "=== erlmcp Docker Image Push Script ==="
echo "Project ID: ${PROJECT_ID}"
echo "Region: ${REGION}"
echo "Version: ${VERSION}"
echo "Image URI: ${IMAGE_URI}"
echo ""

# Step 1: Verify Docker is running
echo "[1/6] Verifying Docker daemon..."
if ! docker ps > /dev/null 2>&1; then
    echo "ERROR: Docker daemon is not running"
    exit 1
fi
echo "✓ Docker daemon is running"
echo ""

# Step 2: Configure Docker authentication
echo "[2/6] Configuring Docker authentication..."
gcloud auth configure-docker ${IMAGE_REGISTRY} --project=${PROJECT_ID}
echo "✓ Docker authentication configured"
echo ""

# Step 3: Build Docker image
echo "[3/6] Building Docker image..."
if [ ! -f "Dockerfile" ]; then
    echo "ERROR: Dockerfile not found in current directory"
    echo "Please run this script from the erlmcp project root"
    exit 1
fi

docker build \
    --target runtime \
    --tag ${IMAGE_URI} \
    --build-arg BUILD_DATE="$(date -u +'%Y-%m-%dT%H:%M:%SZ')" \
    --build-arg VCS_REF="$(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')" \
    --build-arg VERSION="${VERSION}" \
    --label "org.opencontainers.image.documentation=https://github.com/seanchatmangpt/erlmcp" \
    --label "org.opencontainers.image.vendor=erlmcp" \
    .

echo "✓ Docker image built: ${IMAGE_URI}"
echo ""

# Step 4: Verify image
echo "[4/6] Verifying image..."
IMAGE_SIZE=$(docker images ${IMAGE_URI} --format "{{.Size}}")
echo "Image size: ${IMAGE_SIZE}"

# Check image labels
echo "Image labels:"
docker inspect ${IMAGE_URI} --format='{{json .Config.Labels}}' | jq . | head -20
echo "✓ Image verified"
echo ""

# Step 5: Push to Artifact Registry
echo "[5/6] Pushing image to Artifact Registry..."
docker push ${IMAGE_URI}
echo "✓ Image pushed to Artifact Registry"
echo ""

# Step 6: Verify in registry
echo "[6/6] Verifying image in registry..."
gcloud artifacts docker images list ${IMAGE_REGISTRY}/${PROJECT_ID}/${ARTIFACT_REGISTRY_REPO} \
    --project=${PROJECT_ID} \
    --format='value(name)' | grep ${IMAGE_NAME}

echo "✓ Image verified in registry"
echo ""
echo "=== Push Complete ==="
echo "Image URI: ${IMAGE_URI}"
echo "Next steps:"
echo "  1. Generate SBOM: ./tools/gcp/upload_sbom.sh ${VERSION} ${PROJECT_ID} ${REGION}"
echo "  2. Deploy to GKE: ./tools/gcp/deploy_helm.sh ${VERSION} ${PROJECT_ID} ${REGION}"
