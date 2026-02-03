#!/bin/bash
# =============================================================================
# Joe Armstrong AGI MCP - Quick Deploy to Existing GCP Infrastructure
# =============================================================================
# Deploy erlmcp to existing GCP project with existing cloud spend.
# No new billing setup required - uses current project quotas.
#
# Usage: ./deploy-existing-spend.sh [PROJECT_ID] [REGION]
# =============================================================================

set -euo pipefail

# Configuration
PROJECT_ID="${1:-$(gcloud config get-value project)}"
REGION="${2:-us-central1}"
SERVICE_NAME="joe-armstrong-agi-mcp"
IMAGE_TAG="gcr.io/${PROJECT_ID}/${SERVICE_NAME}:v3.0.0"

echo "=============================================="
echo "  Joe Armstrong AGI MCP - GCP Deployment"
echo "=============================================="
echo "Project: ${PROJECT_ID}"
echo "Region:  ${REGION}"
echo "Service: ${SERVICE_NAME}"
echo ""

# Step 1: Build and push container image
echo "[1/5] Building Docker image..."
docker build -t "${IMAGE_TAG}" --target runtime .

echo "[2/5] Pushing to Container Registry..."
docker push "${IMAGE_TAG}"

# Step 3: Deploy to Cloud Run (uses existing spend)
echo "[3/5] Deploying to Cloud Run..."
gcloud run deploy "${SERVICE_NAME}" \
    --project="${PROJECT_ID}" \
    --region="${REGION}" \
    --image="${IMAGE_TAG}" \
    --platform=managed \
    --allow-unauthenticated \
    --memory=2Gi \
    --cpu=2 \
    --min-instances=1 \
    --max-instances=100 \
    --concurrency=80 \
    --timeout=300 \
    --port=8080 \
    --set-env-vars="ERLMCP_ENV=production,ERLMCP_LOG_LEVEL=info" \
    --labels="app=joe-armstrong-agi-mcp,version=v3"

# Step 4: Get service URL
echo "[4/5] Retrieving service URL..."
SERVICE_URL=$(gcloud run services describe "${SERVICE_NAME}" \
    --project="${PROJECT_ID}" \
    --region="${REGION}" \
    --format="value(status.url)")

echo "[5/5] Verifying deployment..."
curl -s "${SERVICE_URL}/health" | head -1

echo ""
echo "=============================================="
echo "  DEPLOYMENT COMPLETE"
echo "=============================================="
echo ""
echo "Joe Armstrong AGI MCP is now live at:"
echo "  ${SERVICE_URL}"
echo ""
echo "API Endpoints:"
echo "  Health:    ${SERVICE_URL}/health"
echo "  MCP API:   ${SERVICE_URL}/mcp"
echo "  Metrics:   ${SERVICE_URL}/metrics"
echo ""
echo "Quick Test:"
echo "  curl -X POST ${SERVICE_URL}/mcp -H 'Content-Type: application/json' \\"
echo "    -d '{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"id\":1}'"
echo ""
echo "Dashboard: https://console.cloud.google.com/run/detail/${REGION}/${SERVICE_NAME}/metrics?project=${PROJECT_ID}"
echo "=============================================="
