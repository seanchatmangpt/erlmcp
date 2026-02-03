#!/bin/bash
# ============================================================================
# Build and Push Script for erlmcp to Artifact Registry
# Google Cloud Platform Marketplace Deployment
# ============================================================================

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_step() {
    echo -e "${BLUE}[STEP]${NC} $1"
}

# ============================================================================
# Configuration
# ============================================================================

PROJECT_ID="${PROJECT_ID:-$(gcloud config get-value project 2>/dev/null)}"
REGION="${REGION:-us-central1}"
IMAGE_NAME="${IMAGE_NAME:-erlmcp/erlmcp}"
IMAGE_TAG="${IMAGE_TAG:-3.0.0}"
DOCKERFILE="${DOCKERFILE:-Dockerfile}"

REGISTRY="${REGION}-docker.pkg.dev"
FULL_IMAGE="${REGISTRY}/${PROJECT_ID}/${IMAGE_NAME}:${IMAGE_TAG}"

# ============================================================================
# Validation
# ============================================================================

check_prerequisites() {
    log_step "Checking prerequisites..."

    # Check gcloud CLI
    if ! command -v gcloud &> /dev/null; then
        log_error "gcloud CLI not found. Install from: https://cloud.google.com/sdk/docs/install"
        exit 1
    fi

    # Check Docker
    if ! command -v docker &> /dev/null; then
        log_error "Docker not found. Install from: https://docs.docker.com/get-docker/"
        exit 1
    fi

    # Check if user is authenticated
    if ! gcloud auth list &> /dev/null; then
        log_error "Not authenticated with gcloud. Run: gcloud auth login"
        exit 1
    fi

    # Check Docker daemon
    if ! docker info &> /dev/null; then
        log_error "Docker daemon not running. Start Docker and try again."
        exit 1
    fi

    # Verify project
    if [ -z "$PROJECT_ID" ]; then
        log_error "PROJECT_ID not set. Run: export PROJECT_ID=your-project-id"
        exit 1
    fi

    log_info "Prerequisites check passed"
    log_info "Project: $PROJECT_ID"
    log_info "Region: $REGION"
    log_info "Image: $FULL_IMAGE"
}

# ============================================================================
# Enable Required APIs
# ============================================================================

enable_apis() {
    log_step "Enabling required Google Cloud APIs..."

    gcloud services enable \
        artifactregistry.googleapis.com \
        cloudbuild.googleapis.com \
        containeranalysis.googleapis.com \
        --project="$PROJECT_ID" \
        --quiet

    log_info "APIs enabled"
}

# ============================================================================
# Create Artifact Registry Repository
# ============================================================================

create_repository() {
    log_step "Creating Artifact Registry repository..."

    if gcloud artifacts repositories describe erlmcp \
        --location="$REGION" \
        --project="$PROJECT_ID" \
        &> /dev/null; then
        log_info "Repository already exists"
    else
        gcloud artifacts repositories create erlmcp \
            --repository-format=docker \
            --location="$REGION" \
            --description="erlmcp container images" \
            --project="$PROJECT_ID"

        log_info "Repository created"
    fi
}

# ============================================================================
# Configure Docker for GCP Authentication
# ============================================================================

configure_docker_auth() {
    log_step "Configuring Docker authentication..."

    gcloud auth configure-docker "$REGISTRY" --quiet

    log_info "Docker configured for GCP"
}

# ============================================================================
# Build Container Image
# ============================================================================

build_image() {
    log_step "Building container image..."

    # Build arguments
    BUILD_ARGS=(
        --build-arg "BUILD_DATE=$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
        --build-arg "VCS_REF=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")"
        --build-arg "VERSION=$IMAGE_TAG"
    )

    # Build image
    docker build "${BUILD_ARGS[@]}" \
        -f "$DOCKERFILE" \
        -t "$FULL_IMAGE" \
        .

    log_info "Image built: $FULL_IMAGE"
}

# ============================================================================
# Run Security Scan
# ============================================================================

scan_image() {
    log_step "Scanning image for vulnerabilities..."

    # Use GCP Container Analysis
    gcloud artifacts docker images scan "$FULL_IMAGE" \
        --location="$REGION" \
        --project="$PROJECT_ID" \
        --format=json > scan-results.json

    # Check for HIGH/CRITICAL vulnerabilities
    HIGH_VULNS=$(jq -r '.scanResults.securityResults[0].vulnerabilities[] | select(.severity == "HIGH") | .package' scan-results.json 2>/dev/null | wc -l)
    CRITICAL_VULNS=$(jq -r '.scanResults.securityResults[0].vulnerabilities[] | select(.severity == "CRITICAL") | .package' scan-results.json 2>/dev/null | wc -l)

    if [ "$HIGH_VULNS" -gt 0 ] || [ "$CRITICAL_VULNS" -gt 0 ]; then
        log_warn "Found $HIGH_VULNS HIGH and $CRITICAL_VULNS CRITICAL vulnerabilities"
        log_warn "Review scan-results.json for details"
        log_warn "To continue despite vulnerabilities, set SCAN_MODE=relaxed"
        if [ "${SCAN_MODE:-strict}" != "relaxed" ]; then
            log_error "Security scan failed. Set SCAN_MODE=relaxed to continue."
            exit 1
        fi
    else
        log_info "No HIGH or CRITICAL vulnerabilities found"
    fi
}

# ============================================================================
# Push Image
# ============================================================================

push_image() {
    log_step "Pushing image to Artifact Registry..."

    docker push "$FULL_IMAGE"

    log_info "Image pushed: $FULL_IMAGE"
}

# ============================================================================
# Create Image Tag
# ============================================================================

tag_latest() {
    log_step "Tagging image as latest..."

    LATEST_IMAGE="${REGISTRY}/${PROJECT_ID}/${IMAGE_NAME}:latest"
    docker tag "$FULL_IMAGE" "$LATEST_IMAGE"
    docker push "$LATEST_IMAGE"

    log_info "Latest tag updated"
}

# ============================================================================
# Verify Image
# ============================================================================

verify_image() {
    log_step "Verifying image..."

    # Check if image exists
    if gcloud artifacts docker images describe "$FULL_IMAGE" \
        --project="$PROJECT_ID" \
        &> /dev/null; then
        log_info "Image verified in Artifact Registry"
    else
        log_error "Image not found in Artifact Registry"
        exit 1
    fi

    # Get image digest
    DIGEST=$(gcloud artifacts docker images describe "$FULL_IMAGE" \
        --project="$PROJECT_ID" \
        --format='value(image_summary.digest)')

    log_info "Image digest: $DIGEST"
}

# ============================================================================
# Main Function
# ============================================================================

main() {
    log_info "Starting build and push for erlmcp..."
    log_info "================================================"

    check_prerequisites
    enable_apis
    create_repository
    configure_docker_auth
    build_image

    if [ "${SKIP_SCAN:-false}" != "true" ]; then
        scan_image
    fi

    push_image

    if [ "${TAG_LATEST:-true}" == "true" ]; then
        tag_latest
    fi

    verify_image

    log_info "================================================"
    log_info "Build and push completed successfully!"
    log_info "Image: $FULL_IMAGE"
}

# ============================================================================
# Script Entry Point
# ============================================================================

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --project)
            PROJECT_ID="$2"
            shift 2
            ;;
        --region)
            REGION="$2"
            shift 2
            ;;
        --tag)
            IMAGE_TAG="$2"
            shift 2
            ;;
        --dockerfile)
            DOCKERFILE="$2"
            shift 2
            ;;
        --skip-scan)
            SKIP_SCAN=true
            shift
            ;;
        --no-latest)
            TAG_LATEST=false
            shift
            ;;
        --scan-mode)
            SCAN_MODE="$2"
            shift 2
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--project PROJECT] [--region REGION] [--tag TAG] [--dockerfile PATH] [--skip-scan] [--no-latest] [--scan-mode strict|relaxed]"
            exit 1
            ;;
    esac
done

# Run main function
main
