#!/bin/bash
##############################################################################
# Deploy ErlMCP to Specific Environment
#
# Usage: ./deploy-env.sh <environment> [version] [--dry-run] [--validate-only]
#
# Environments: dev, staging, production
# Version: semantic version or "latest"
#
# Examples:
#   ./deploy-env.sh dev
#   ./deploy-env.sh staging 1.2.3
#   ./deploy-env.sh production latest --validate-only
##############################################################################

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
ENVIRONMENT="${1:-}"
VERSION="${2:-latest}"
DRY_RUN="${DRY_RUN:-false}"
VALIDATE_ONLY="${VALIDATE_ONLY:-false}"
REGISTRY="us-central1-docker.pkg.dev"

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --validate-only)
            VALIDATE_ONLY=true
            shift
            ;;
        *)
            shift
            ;;
    esac
done

# Validate environment
if [[ -z "$ENVIRONMENT" ]]; then
    log_error "Environment not specified"
    echo "Usage: $0 <environment> [version] [--dry-run] [--validate-only]"
    echo "Environments: dev, staging, production"
    exit 1
fi

# Validate environment name
case "$ENVIRONMENT" in
    dev|development)
        ENVIRONMENT="dev"
        PROJECT_ID="taiea-dev"
        REGION="us-central1"
        SERVICE_NAME="taiea-dev"
        CLUSTER_NAME="taiea-dev-cluster"
        ;;
    staging)
        ENVIRONMENT="staging"
        PROJECT_ID="taiea-dev"
        REGION="us-central1"
        SERVICE_NAME="taiea-staging"
        CLUSTER_NAME="taiea-staging-cluster"
        ;;
    prod|production)
        ENVIRONMENT="production"
        PROJECT_ID="taiea-prod"
        REGION="us-central1"
        SERVICE_NAME="taiea-prod"
        CLUSTER_NAME="taiea-prod-cluster"
        ;;
    *)
        log_error "Unknown environment: $ENVIRONMENT"
        echo "Valid environments: dev, staging, production"
        exit 1
        ;;
esac

# Load environment configuration
ENV_FILE="${PROJECT_ROOT}/config/${ENVIRONMENT}.env"
CONFIG_FILE="${PROJECT_ROOT}/config/sys.config.${ENVIRONMENT}"

if [[ ! -f "$ENV_FILE" ]]; then
    log_error "Environment file not found: $ENV_FILE"
    exit 1
fi

if [[ ! -f "$CONFIG_FILE" ]]; then
    log_error "Configuration file not found: $CONFIG_FILE"
    exit 1
fi

log_info "Deploying to environment: $ENVIRONMENT"
log_info "GCP Project: $PROJECT_ID"
log_info "Region: $REGION"
log_info "Version: $VERSION"

# Step 1: Validate prerequisites
log_info "Step 1: Validating prerequisites..."

if ! command -v gcloud &> /dev/null; then
    log_error "gcloud CLI not found. Please install Google Cloud SDK."
    exit 1
fi

if ! command -v kubectl &> /dev/null; then
    log_error "kubectl not found. Please install kubectl."
    exit 1
fi

if ! gcloud auth list --filter=status:ACTIVE --format='value(account)' | grep -q .; then
    log_error "Not authenticated with gcloud. Run: gcloud auth login"
    exit 1
fi

log_success "Prerequisites validated"

# Step 2: Set GCP project
log_info "Step 2: Setting GCP project to $PROJECT_ID..."
if [[ "$DRY_RUN" == "false" ]]; then
    gcloud config set project "$PROJECT_ID"
    log_success "GCP project set to $PROJECT_ID"
else
    log_warn "[DRY RUN] Would set project to $PROJECT_ID"
fi

# Step 3: Validate environment configuration
log_info "Step 3: Validating environment configuration..."

# Check required environment variables
required_vars=(
    "APP_ENV"
    "LOG_LEVEL"
    "SERVER_HOST"
    "SERVER_PORT"
)

for var in "${required_vars[@]}"; do
    if ! grep -q "^${var}=" "$ENV_FILE"; then
        log_error "Required variable not found in $ENV_FILE: $var"
        exit 1
    fi
done

log_success "Environment configuration validated"

# Step 4: Get image digest
log_info "Step 4: Getting container image digest..."

IMAGE_NAME="erlmcp-tai"
if [[ "$VERSION" == "latest" ]]; then
    # Get the latest tag
    IMAGE_TAG=$(gcloud container images list-tags \
        "${REGISTRY}/${PROJECT_ID}/erlmcp-tai-repo/${IMAGE_NAME}" \
        --limit=1 \
        --format='value(tags[0])' 2>/dev/null || echo "")

    if [[ -z "$IMAGE_TAG" ]]; then
        log_error "No container images found in registry"
        exit 1
    fi
else
    IMAGE_TAG="$VERSION"
fi

IMAGE_URL="${REGISTRY}/${PROJECT_ID}/erlmcp-tai-repo/${IMAGE_NAME}:${IMAGE_TAG}"
log_info "Container image: $IMAGE_URL"

if [[ "$DRY_RUN" == "true" ]]; then
    log_warn "[DRY RUN] Would deploy image: $IMAGE_URL"
fi

# Step 5: Validate image exists
log_info "Step 5: Validating container image..."

if ! gcloud container images describe "$IMAGE_URL" &>/dev/null; then
    log_error "Container image not found: $IMAGE_URL"
    exit 1
fi

log_success "Container image validated"

# Step 6: Validate only option
if [[ "$VALIDATE_ONLY" == "true" ]]; then
    log_success "Validation complete. Use --validate-only flag to continue with deployment."
    exit 0
fi

# Step 7: Get cluster credentials
log_info "Step 7: Getting GKE cluster credentials..."

if [[ "$DRY_RUN" == "false" ]]; then
    gcloud container clusters get-credentials "$CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" || {
        log_error "Failed to get cluster credentials"
        exit 1
    }
    log_success "Cluster credentials obtained"
else
    log_warn "[DRY RUN] Would get credentials for cluster: $CLUSTER_NAME"
fi

# Step 8: Load environment variables as Kubernetes secret
log_info "Step 8: Preparing Kubernetes secrets..."

NAMESPACE="taiea-${ENVIRONMENT}"

if [[ "$DRY_RUN" == "false" ]]; then
    # Create namespace if it doesn't exist
    kubectl create namespace "$NAMESPACE" --dry-run=client -o yaml | kubectl apply -f -

    # Create secret from env file
    kubectl create secret generic "${SERVICE_NAME}-env" \
        --from-env-file="$ENV_FILE" \
        --namespace="$NAMESPACE" \
        --dry-run=client \
        -o yaml | kubectl apply -f -

    log_success "Kubernetes secrets prepared"
else
    log_warn "[DRY RUN] Would create secret from: $ENV_FILE"
fi

# Step 9: Deploy to GKE
log_info "Step 9: Deploying to GKE..."

DEPLOYMENT_MANIFEST="/tmp/erlmcp-${ENVIRONMENT}-deployment.yaml"

cat > "$DEPLOYMENT_MANIFEST" << EOF
apiVersion: apps/v1
kind: Deployment
metadata:
  name: $SERVICE_NAME
  namespace: $NAMESPACE
  labels:
    app: erlmcp
    environment: $ENVIRONMENT
spec:
  replicas: 2
  selector:
    matchLabels:
      app: erlmcp
      environment: $ENVIRONMENT
  template:
    metadata:
      labels:
        app: erlmcp
        environment: $ENVIRONMENT
    spec:
      containers:
      - name: erlmcp
        image: $IMAGE_URL
        ports:
        - containerPort: 8080
          name: http
        envFrom:
        - secretRef:
            name: ${SERVICE_NAME}-env
        resources:
          requests:
            cpu: 250m
            memory: 512Mi
          limits:
            cpu: 1000m
            memory: 2Gi
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 10
          periodSeconds: 5
EOF

if [[ "$DRY_RUN" == "false" ]]; then
    kubectl apply -f "$DEPLOYMENT_MANIFEST"
    log_success "Deployment applied"

    # Wait for rollout
    log_info "Waiting for rollout to complete..."
    if kubectl rollout status deployment/"$SERVICE_NAME" \
        --namespace="$NAMESPACE" \
        --timeout=300s; then
        log_success "Rollout completed successfully"
    else
        log_error "Rollout failed or timed out"
        exit 1
    fi
else
    log_warn "[DRY RUN] Would apply deployment manifest"
    log_info "Manifest would be:"
    cat "$DEPLOYMENT_MANIFEST"
fi

# Step 10: Verify deployment
log_info "Step 10: Verifying deployment..."

if [[ "$DRY_RUN" == "false" ]]; then
    RUNNING_PODS=$(kubectl get pods \
        --selector="app=erlmcp,environment=$ENVIRONMENT" \
        --namespace="$NAMESPACE" \
        --no-headers 2>/dev/null | grep Running | wc -l)

    if [[ $RUNNING_PODS -gt 0 ]]; then
        log_success "Deployment verified: $RUNNING_PODS pods running"
    else
        log_warn "No running pods found. Deployment may be initializing."
    fi
fi

# Step 11: Run smoke tests (if not development)
if [[ "$ENVIRONMENT" != "dev" && "$DRY_RUN" == "false" ]]; then
    log_info "Step 11: Running smoke tests..."

    SERVICE_URL=$(kubectl get service "$SERVICE_NAME" \
        --namespace="$NAMESPACE" \
        -o jsonpath='{.status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "")

    if [[ -n "$SERVICE_URL" ]]; then
        log_info "Service URL: http://$SERVICE_URL:8080"

        # Simple health check
        if curl -s "http://$SERVICE_URL:8080/health" > /dev/null; then
            log_success "Health check passed"
        else
            log_warn "Health check failed, but deployment may still be initializing"
        fi
    else
        log_warn "Could not determine service URL"
    fi
fi

# Cleanup
rm -f "$DEPLOYMENT_MANIFEST"

# Final summary
log_success "Deployment to $ENVIRONMENT complete!"
log_info "Summary:"
log_info "  Environment: $ENVIRONMENT"
log_info "  Project: $PROJECT_ID"
log_info "  Region: $REGION"
log_info "  Cluster: $CLUSTER_NAME"
log_info "  Image: $IMAGE_URL"
log_info "  Config: $CONFIG_FILE"

if [[ "$DRY_RUN" == "true" ]]; then
    log_warn "This was a DRY RUN. Use 'DRY_RUN=false' to perform actual deployment."
fi

exit 0
