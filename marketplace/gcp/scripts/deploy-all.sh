#!/bin/bash
# ============================================================================
# Automated Multi-Platform GCP Deployment Script
# Orchestrates deployment to GKE, Cloud Run, and GCE with health checks
#
# WHY: Production deployment deadline requires automated orchestration across
#      all GCP Marketplace deployment paths with comprehensive validation
# WHAT: Bash script for parallel/sequential deployment with health checks
# HOW:  docker build for images, kubectl apply for GKE, gcloud for Cloud Run/GCE
# ============================================================================

set -euo pipefail

# ============================================================================
# Constants and Configuration
# ============================================================================

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m'

# Logging functions
log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_deploy() { echo -e "${BLUE}[DEPLOY]${NC} $1"; }
log_health() { echo -e "${CYAN}[HEALTH]${NC} $1"; }
log_success() { echo -e "${MAGENTA}[SUCCESS]${NC} $1"; }

# Configuration from environment
PROJECT_ID="${PROJECT_ID:-}"
REGION="${REGION:-us-central1}"
ZONE="${ZONE:-us-central1-a}"
DEPLOY_ID="${DEPLOY_ID:-prod-$(date +%Y%m%d-%H%M%S)}"

# Deployment flags
DEPLOY_GKE="${DEPLOY_GKE:-true}"
DEPLOY_CLOUDRUN="${DEPLOY_CLOUDRUN:-true}"
DEPLOY_GCE="${DEPLOY_GCE:-true}"
SKIP_HEALTH_CHECK="${SKIP_HEALTH_CHECK:-false}"
PARALLEL_DEPLOY="${PARALLEL_DEPLOY:-false}"

# Resource names
GKE_CLUSTER_NAME="${GKE_CLUSTER_NAME:-erlmcp-${DEPLOY_ID}}"
CLOUDRUN_SERVICE_NAME="${CLOUDRUN_SERVICE_NAME:-erlmcp-${DEPLOY_ID}}"
GCE_INSTANCE_NAME="${GCE_INSTANCE_NAME:-erlmcp-${DEPLOY_ID}}"

# Image configuration
IMAGE_REGISTRY="${IMAGE_REGISTRY:-${REGION}-docker.pkg.dev}"
IMAGE_REPO="${IMAGE_REPO:-${PROJECT_ID}/erlmcp}"
IMAGE_TAG="${IMAGE_TAG:-latest}"
FULL_IMAGE="${IMAGE_REGISTRY}/${IMAGE_REPO}/erlmcp:${IMAGE_TAG}"

# Timeouts (seconds)
BUILD_TIMEOUT="${BUILD_TIMEOUT:-1800}"        # 30 minutes
DEPLOY_TIMEOUT="${DEPLOY_TIMEOUT:-1800}"      # 30 minutes
HEALTH_CHECK_TIMEOUT="${HEALTH_CHECK_TIMEOUT:-600}"  # 10 minutes
HEALTH_CHECK_INTERVAL="${HEALTH_CHECK_INTERVAL:-10}"

# Paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
PROJECT_ROOT="$(dirname "$(dirname "$MARKETPLACE_DIR")")"
EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence/deploy-all-${DEPLOY_ID}"

# ============================================================================
# Trap and Cleanup
# ============================================================================

cleanup() {
    local exit_code=$?
    if [ $exit_code -ne 0 ]; then
        log_error "Deployment failed with exit code $exit_code"
        log_info "Evidence saved to: $EVIDENCE_DIR"
    fi

    # Save deployment summary
    if [ -d "$EVIDENCE_DIR" ]; then
        cat > "$EVIDENCE_DIR/deployment-summary.json" <<EOF
{
  "deploy_id": "$DEPLOY_ID",
  "timestamp": "$(date -Iseconds)",
  "exit_code": $exit_code,
  "project_id": "$PROJECT_ID",
  "region": "$REGION",
  "gke_deployed": $([ "$DEPLOY_GKE" = "true" ] && echo "true" || echo "false"),
  "cloudrun_deployed": $([ "$DEPLOY_CLOUDRUN" = "true" ] && echo "true" || echo "false"),
  "gce_deployed": $([ "$DEPLOY_GCE" = "true" ] && echo "true" || echo "false")
}
EOF
    fi
}

trap cleanup EXIT

# ============================================================================
# Prerequisites
# ============================================================================

check_prerequisites() {
    log_info "Checking prerequisites..."

    # Validate PROJECT_ID
    if [ -z "$PROJECT_ID" ]; then
        log_error "PROJECT_ID not set. Run: export PROJECT_ID=your-project-id"
        exit 1
    fi

    # Check required commands
    local required_cmds=("gcloud" "docker" "jq")
    for cmd in "${required_cmds[@]}"; do
        if ! command -v "$cmd" &> /dev/null; then
            log_error "Required command not found: $cmd"
            exit 1
        fi
    done

    # Check conditional commands
    if [ "$DEPLOY_GKE" = "true" ]; then
        if ! command -v kubectl &> /dev/null; then
            log_error "kubectl required for GKE deployment"
            exit 1
        fi
        if ! command -v helm &> /dev/null; then
            log_warn "helm not found - GKE deployment may require helm"
        fi
    fi

    # Verify gcloud authentication
    if ! gcloud auth list --filter=status:ACTIVE --format="value(account)" | grep -q .; then
        log_error "No active gcloud authentication. Run: gcloud auth login"
        exit 1
    fi

    # Verify project access
    if ! gcloud projects describe "$PROJECT_ID" &> /dev/null; then
        log_error "Cannot access project: $PROJECT_ID"
        exit 1
    fi

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    log_success "Prerequisites check passed"
}

# ============================================================================
# Enable Required APIs
# ============================================================================

enable_apis() {
    log_info "Enabling required GCP APIs..."

    local apis=(
        "compute.googleapis.com"
        "container.googleapis.com"
        "run.googleapis.com"
        "artifactregistry.googleapis.com"
        "secretmanager.googleapis.com"
        "monitoring.googleapis.com"
        "logging.googleapis.com"
        "cloudresourcemanager.googleapis.com"
    )

    gcloud services enable "${apis[@]}" \
        --project="$PROJECT_ID" \
        2>&1 | tee "$EVIDENCE_DIR/api-enablement.log"

    log_success "APIs enabled"
}

# ============================================================================
# Build and Push Docker Image (DOCKER-ONLY Constitution Compliant)
# ============================================================================

build_and_push_image() {
    log_deploy "Building and pushing Docker image..."

    cd "$PROJECT_ROOT"

    # Build metadata
    local build_date=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    local git_sha=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
    local version="${VERSION:-3.0.0}"

    # Build image using Docker (DOCKER-ONLY requirement)
    log_info "Building image: $FULL_IMAGE"
    docker build \
        --platform linux/amd64 \
        --build-arg BUILD_DATE="$build_date" \
        --build-arg VCS_REF="$git_sha" \
        --build-arg VERSION="$version" \
        -t "$FULL_IMAGE" \
        -f Dockerfile \
        . 2>&1 | tee "$EVIDENCE_DIR/docker-build.log"

    if [ ${PIPESTATUS[0]} -ne 0 ]; then
        log_error "Docker build failed"
        return 1
    fi

    # Authenticate Docker to Artifact Registry
    log_info "Authenticating to Artifact Registry..."
    gcloud auth configure-docker "${IMAGE_REGISTRY}" --quiet

    # Push image
    log_info "Pushing image to registry..."
    docker push "$FULL_IMAGE" 2>&1 | tee "$EVIDENCE_DIR/docker-push.log"

    if [ ${PIPESTATUS[0]} -ne 0 ]; then
        log_error "Docker push failed"
        return 1
    fi

    # Get image digest for reproducibility
    local image_digest=$(gcloud artifacts docker images describe "$FULL_IMAGE" \
        --format='get(image_summary.digest)' 2>/dev/null || echo "unknown")

    echo "$image_digest" > "$EVIDENCE_DIR/image-digest.txt"

    log_success "Image built and pushed: $FULL_IMAGE@$image_digest"
}

# ============================================================================
# Deploy to GKE
# ============================================================================

deploy_gke() {
    log_deploy "Deploying to Google Kubernetes Engine..."

    local gke_evidence_dir="$EVIDENCE_DIR/gke"
    mkdir -p "$gke_evidence_dir"

    # Check if cluster exists
    if gcloud container clusters describe "$GKE_CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" &> /dev/null; then
        log_info "Using existing GKE cluster: $GKE_CLUSTER_NAME"
    else
        log_info "Creating GKE cluster: $GKE_CLUSTER_NAME"

        gcloud container clusters create "$GKE_CLUSTER_NAME" \
            --region="$REGION" \
            --project="$PROJECT_ID" \
            --machine-type="${GKE_MACHINE_TYPE:-e2-standard-2}" \
            --num-nodes="${GKE_NUM_NODES:-3}" \
            --enable-autoscaling \
            --min-nodes="${GKE_MIN_NODES:-1}" \
            --max-nodes="${GKE_MAX_NODES:-5}" \
            --enable-autorepair \
            --enable-autoupgrade \
            --enable-ip-alias \
            --enable-stackdriver-kubernetes \
            --addons=HorizontalPodAutoscaling,HttpLoadBalancing,GcePersistentDiskCsiDriver \
            --workload-pool="${PROJECT_ID}.svc.id.goog" \
            --release-channel=regular \
            2>&1 | tee "$gke_evidence_dir/cluster-create.log"

        if [ ${PIPESTATUS[0]} -ne 0 ]; then
            log_error "GKE cluster creation failed"
            return 1
        fi
    fi

    # Get cluster credentials
    log_info "Getting cluster credentials..."
    gcloud container clusters get-credentials "$GKE_CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID"

    # Deploy using Helm if available, otherwise use kubectl
    if [ -d "$MARKETPLACE_DIR/helm/erlmcp-marketplace" ]; then
        log_info "Deploying using Helm..."

        helm upgrade --install erlmcp \
            "$MARKETPLACE_DIR/helm/erlmcp-marketplace" \
            --namespace=erlmcp \
            --create-namespace \
            --set image.repository="${IMAGE_REGISTRY}/${IMAGE_REPO}/erlmcp" \
            --set image.tag="$IMAGE_TAG" \
            --set projectId="$PROJECT_ID" \
            --wait \
            --timeout="${DEPLOY_TIMEOUT}s" \
            2>&1 | tee "$gke_evidence_dir/helm-deploy.log"

        if [ ${PIPESTATUS[0]} -ne 0 ]; then
            log_error "Helm deployment failed"
            return 1
        fi
    else
        log_info "Deploying using kubectl..."

        # Create namespace
        kubectl create namespace erlmcp --dry-run=client -o yaml | kubectl apply -f -

        # Create deployment manifest
        cat > "$gke_evidence_dir/deployment.yaml" <<EOF
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp
  namespace: erlmcp
  labels:
    app: erlmcp
    version: "$IMAGE_TAG"
spec:
  replicas: 3
  selector:
    matchLabels:
      app: erlmcp
  template:
    metadata:
      labels:
        app: erlmcp
        version: "$IMAGE_TAG"
    spec:
      containers:
      - name: erlmcp
        image: $FULL_IMAGE
        ports:
        - containerPort: 8080
          name: http
        env:
        - name: ERLMCP_MODE
          value: "server"
        - name: ERLMCP_PORT
          value: "8080"
        resources:
          requests:
            cpu: 500m
            memory: 512Mi
          limits:
            cpu: 1000m
            memory: 1Gi
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 10
          periodSeconds: 5
---
apiVersion: v1
kind: Service
metadata:
  name: erlmcp
  namespace: erlmcp
spec:
  type: LoadBalancer
  ports:
  - port: 80
    targetPort: 8080
    protocol: TCP
    name: http
  selector:
    app: erlmcp
EOF

        kubectl apply -f "$gke_evidence_dir/deployment.yaml" \
            2>&1 | tee "$gke_evidence_dir/kubectl-apply.log"

        if [ $? -ne 0 ]; then
            log_error "kubectl apply failed"
            return 1
        fi
    fi

    # Wait for deployment to be ready
    log_info "Waiting for GKE deployment to be ready..."
    kubectl wait --for=condition=available \
        --timeout="${DEPLOY_TIMEOUT}s" \
        deployment/erlmcp \
        -n erlmcp \
        2>&1 | tee "$gke_evidence_dir/deployment-wait.log"

    # Save deployment info
    kubectl get all -n erlmcp > "$gke_evidence_dir/resources.txt"
    kubectl describe deployment erlmcp -n erlmcp > "$gke_evidence_dir/deployment-describe.txt"

    log_success "GKE deployment completed"
}

# ============================================================================
# Deploy to Cloud Run
# ============================================================================

deploy_cloudrun() {
    log_deploy "Deploying to Google Cloud Run..."

    local cloudrun_evidence_dir="$EVIDENCE_DIR/cloudrun"
    mkdir -p "$cloudrun_evidence_dir"

    log_info "Deploying Cloud Run service: $CLOUDRUN_SERVICE_NAME"

    gcloud run deploy "$CLOUDRUN_SERVICE_NAME" \
        --image="$FULL_IMAGE" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --platform=managed \
        --allow-unauthenticated \
        --cpu="${CLOUDRUN_CPU:-1}" \
        --memory="${CLOUDRUN_MEMORY:-512Mi}" \
        --min-instances="${CLOUDRUN_MIN_INSTANCES:-0}" \
        --max-instances="${CLOUDRUN_MAX_INSTANCES:-10}" \
        --timeout="${CLOUDRUN_TIMEOUT:-300}" \
        --concurrency="${CLOUDRUN_CONCURRENCY:-80}" \
        --port=8080 \
        --set-env-vars="ERLMCP_MODE=server,ERLMCP_PORT=8080" \
        --execution-environment=gen2 \
        2>&1 | tee "$cloudrun_evidence_dir/deploy.log"

    if [ ${PIPESTATUS[0]} -ne 0 ]; then
        log_error "Cloud Run deployment failed"
        return 1
    fi

    # Get service URL
    local service_url=$(gcloud run services describe "$CLOUDRUN_SERVICE_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='value(status.url)')

    echo "$service_url" > "$cloudrun_evidence_dir/service-url.txt"

    # Save service details
    gcloud run services describe "$CLOUDRUN_SERVICE_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format=yaml > "$cloudrun_evidence_dir/service-describe.yaml"

    log_success "Cloud Run deployment completed: $service_url"
}

# ============================================================================
# Deploy to GCE
# ============================================================================

deploy_gce() {
    log_deploy "Deploying to Google Compute Engine..."

    local gce_evidence_dir="$EVIDENCE_DIR/gce"
    mkdir -p "$gce_evidence_dir"

    # Check if instance exists
    if gcloud compute instances describe "$GCE_INSTANCE_NAME" \
        --zone="$ZONE" \
        --project="$PROJECT_ID" &> /dev/null; then
        log_info "Instance exists, updating..."

        # Stop instance
        gcloud compute instances stop "$GCE_INSTANCE_NAME" \
            --zone="$ZONE" \
            --project="$PROJECT_ID" \
            2>&1 | tee "$gce_evidence_dir/instance-stop.log"
    fi

    # Create startup script that runs Docker container (DOCKER-ONLY)
    cat > "$gce_evidence_dir/startup-script.sh" <<'EOF'
#!/bin/bash
set -euo pipefail

# Install Docker if not present
if ! command -v docker &> /dev/null; then
    curl -fsSL https://get.docker.com -o get-docker.sh
    sh get-docker.sh
    systemctl enable docker
    systemctl start docker
fi

# Authenticate to Artifact Registry
gcloud auth configure-docker ${IMAGE_REGISTRY} --quiet

# Pull and run container
docker pull ${FULL_IMAGE}
docker stop erlmcp 2>/dev/null || true
docker rm erlmcp 2>/dev/null || true
docker run -d \
    --name erlmcp \
    --restart=unless-stopped \
    -p 8080:8080 \
    -e ERLMCP_MODE=server \
    -e ERLMCP_PORT=8080 \
    ${FULL_IMAGE}

# Health check endpoint
cat > /usr/local/bin/health-check.sh <<'HEALTH_EOF'
#!/bin/bash
curl -f http://localhost:8080/health || exit 1
HEALTH_EOF
chmod +x /usr/local/bin/health-check.sh
EOF

    # Replace variables in startup script
    sed -i "s|\${IMAGE_REGISTRY}|${IMAGE_REGISTRY}|g" "$gce_evidence_dir/startup-script.sh"
    sed -i "s|\${FULL_IMAGE}|${FULL_IMAGE}|g" "$gce_evidence_dir/startup-script.sh"

    # Create or update instance
    log_info "Creating/updating GCE instance: $GCE_INSTANCE_NAME"

    gcloud compute instances create "$GCE_INSTANCE_NAME" \
        --zone="$ZONE" \
        --project="$PROJECT_ID" \
        --machine-type="${GCE_MACHINE_TYPE:-e2-medium}" \
        --image-family="${GCE_IMAGE_FAMILY:-cos-stable}" \
        --image-project=cos-cloud \
        --boot-disk-size="${GCE_BOOT_DISK_SIZE:-20GB}" \
        --boot-disk-type=pd-standard \
        --scopes=cloud-platform \
        --tags=erlmcp,http-server \
        --metadata-from-file=startup-script="$gce_evidence_dir/startup-script.sh" \
        2>&1 | tee "$gce_evidence_dir/instance-create.log" || true

    # Create firewall rule if needed
    if ! gcloud compute firewall-rules describe erlmcp-allow-http \
        --project="$PROJECT_ID" &> /dev/null; then
        log_info "Creating firewall rule..."

        gcloud compute firewall-rules create erlmcp-allow-http \
            --project="$PROJECT_ID" \
            --direction=INGRESS \
            --priority=1000 \
            --network=default \
            --action=ALLOW \
            --rules=tcp:8080 \
            --source-ranges=0.0.0.0/0 \
            --target-tags=erlmcp \
            2>&1 | tee "$gce_evidence_dir/firewall-create.log"
    fi

    # Get instance IP
    local instance_ip=$(gcloud compute instances describe "$GCE_INSTANCE_NAME" \
        --zone="$ZONE" \
        --project="$PROJECT_ID" \
        --format='get(networkInterfaces[0].accessConfigs[0].natIP)')

    echo "$instance_ip" > "$gce_evidence_dir/instance-ip.txt"

    # Save instance details
    gcloud compute instances describe "$GCE_INSTANCE_NAME" \
        --zone="$ZONE" \
        --project="$PROJECT_ID" \
        --format=yaml > "$gce_evidence_dir/instance-describe.yaml"

    log_success "GCE deployment completed: http://$instance_ip:8080"
}

# ============================================================================
# Health Checks
# ============================================================================

health_check_gke() {
    log_health "Running GKE health checks..."

    local gke_evidence_dir="$EVIDENCE_DIR/gke"
    local max_attempts=$((HEALTH_CHECK_TIMEOUT / HEALTH_CHECK_INTERVAL))

    # Get service endpoint
    local service_ip=""
    for ((i=1; i<=max_attempts; i++)); do
        service_ip=$(kubectl get service erlmcp -n erlmcp \
            -o jsonpath='{.status.loadBalancer.ingress[0].ip}' 2>/dev/null || echo "")

        if [ -n "$service_ip" ]; then
            break
        fi

        log_info "Waiting for LoadBalancer IP... (attempt $i/$max_attempts)"
        sleep "$HEALTH_CHECK_INTERVAL"
    done

    if [ -z "$service_ip" ]; then
        log_error "Failed to get LoadBalancer IP"
        return 1
    fi

    echo "$service_ip" > "$gke_evidence_dir/loadbalancer-ip.txt"

    # Health check
    local endpoint="http://${service_ip}/health"
    log_info "Health checking: $endpoint"

    for ((i=1; i<=max_attempts; i++)); do
        if curl -f -s -m 5 "$endpoint" > "$gke_evidence_dir/health-check-$i.json" 2>&1; then
            log_success "GKE health check passed"
            return 0
        fi

        log_info "Health check attempt $i/$max_attempts failed, retrying..."
        sleep "$HEALTH_CHECK_INTERVAL"
    done

    log_error "GKE health check failed after $max_attempts attempts"
    return 1
}

health_check_cloudrun() {
    log_health "Running Cloud Run health checks..."

    local cloudrun_evidence_dir="$EVIDENCE_DIR/cloudrun"

    # Get service URL
    local service_url=$(cat "$cloudrun_evidence_dir/service-url.txt")
    local endpoint="${service_url}/health"

    log_info "Health checking: $endpoint"

    local max_attempts=$((HEALTH_CHECK_TIMEOUT / HEALTH_CHECK_INTERVAL))

    for ((i=1; i<=max_attempts; i++)); do
        if curl -f -s -m 5 "$endpoint" > "$cloudrun_evidence_dir/health-check-$i.json" 2>&1; then
            log_success "Cloud Run health check passed"
            return 0
        fi

        log_info "Health check attempt $i/$max_attempts failed, retrying..."
        sleep "$HEALTH_CHECK_INTERVAL"
    done

    log_error "Cloud Run health check failed after $max_attempts attempts"
    return 1
}

health_check_gce() {
    log_health "Running GCE health checks..."

    local gce_evidence_dir="$EVIDENCE_DIR/gce"

    # Get instance IP
    local instance_ip=$(cat "$gce_evidence_dir/instance-ip.txt")
    local endpoint="http://${instance_ip}:8080/health"

    log_info "Health checking: $endpoint"

    local max_attempts=$((HEALTH_CHECK_TIMEOUT / HEALTH_CHECK_INTERVAL))

    # Wait for startup script to complete
    log_info "Waiting for startup script to complete..."
    sleep 30

    for ((i=1; i<=max_attempts; i++)); do
        if curl -f -s -m 5 "$endpoint" > "$gce_evidence_dir/health-check-$i.json" 2>&1; then
            log_success "GCE health check passed"
            return 0
        fi

        log_info "Health check attempt $i/$max_attempts failed, retrying..."
        sleep "$HEALTH_CHECK_INTERVAL"
    done

    log_error "GCE health check failed after $max_attempts attempts"
    return 1
}

# ============================================================================
# Parallel Deployment Support
# ============================================================================

deploy_parallel() {
    log_info "Starting parallel deployment..."

    local pids=()

    if [ "$DEPLOY_GKE" = "true" ]; then
        deploy_gke &
        pids+=($!)
    fi

    if [ "$DEPLOY_CLOUDRUN" = "true" ]; then
        deploy_cloudrun &
        pids+=($!)
    fi

    if [ "$DEPLOY_GCE" = "true" ]; then
        deploy_gce &
        pids+=($!)
    fi

    # Wait for all deployments
    local failed=0
    for pid in "${pids[@]}"; do
        if ! wait "$pid"; then
            failed=$((failed + 1))
        fi
    done

    if [ $failed -gt 0 ]; then
        log_error "$failed deployment(s) failed"
        return 1
    fi

    log_success "All parallel deployments completed"
}

# ============================================================================
# Sequential Deployment
# ============================================================================

deploy_sequential() {
    log_info "Starting sequential deployment..."

    if [ "$DEPLOY_CLOUDRUN" = "true" ]; then
        deploy_cloudrun || return 1
    fi

    if [ "$DEPLOY_GCE" = "true" ]; then
        deploy_gce || return 1
    fi

    if [ "$DEPLOY_GKE" = "true" ]; then
        deploy_gke || return 1
    fi

    log_success "All sequential deployments completed"
}

# ============================================================================
# Main Deployment Flow
# ============================================================================

main() {
    local start_time=$(date +%s)

    log_info "=========================================="
    log_info "erlmcp Multi-Platform GCP Deployment"
    log_info "=========================================="
    log_info "Deploy ID: $DEPLOY_ID"
    log_info "Project: $PROJECT_ID"
    log_info "Region: $REGION"
    log_info "Image: $FULL_IMAGE"
    log_info "GKE: $DEPLOY_GKE"
    log_info "Cloud Run: $DEPLOY_CLOUDRUN"
    log_info "GCE: $DEPLOY_GCE"
    log_info "Parallel: $PARALLEL_DEPLOY"
    log_info "=========================================="

    # Prerequisites
    check_prerequisites
    enable_apis

    # Build and push image
    build_and_push_image || exit 1

    # Deploy
    if [ "$PARALLEL_DEPLOY" = "true" ]; then
        deploy_parallel || exit 1
    else
        deploy_sequential || exit 1
    fi

    # Health checks
    if [ "$SKIP_HEALTH_CHECK" != "true" ]; then
        log_info "Running health checks..."

        local health_failed=0

        if [ "$DEPLOY_GKE" = "true" ]; then
            health_check_gke || health_failed=$((health_failed + 1))
        fi

        if [ "$DEPLOY_CLOUDRUN" = "true" ]; then
            health_check_cloudrun || health_failed=$((health_failed + 1))
        fi

        if [ "$DEPLOY_GCE" = "true" ]; then
            health_check_gce || health_failed=$((health_failed + 1))
        fi

        if [ $health_failed -gt 0 ]; then
            log_error "$health_failed health check(s) failed"
            exit 1
        fi

        log_success "All health checks passed"
    fi

    # Summary
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    log_info "=========================================="
    log_success "Deployment completed successfully!"
    log_info "Duration: ${duration}s"
    log_info "Evidence: $EVIDENCE_DIR"
    log_info "=========================================="

    # Print endpoints
    echo ""
    log_info "Deployment Endpoints:"

    if [ "$DEPLOY_GKE" = "true" ]; then
        local gke_ip=$(cat "$EVIDENCE_DIR/gke/loadbalancer-ip.txt" 2>/dev/null || echo "pending")
        log_info "  GKE: http://${gke_ip}"
    fi

    if [ "$DEPLOY_CLOUDRUN" = "true" ]; then
        local cloudrun_url=$(cat "$EVIDENCE_DIR/cloudrun/service-url.txt" 2>/dev/null || echo "pending")
        log_info "  Cloud Run: ${cloudrun_url}"
    fi

    if [ "$DEPLOY_GCE" = "true" ]; then
        local gce_ip=$(cat "$EVIDENCE_DIR/gce/instance-ip.txt" 2>/dev/null || echo "pending")
        log_info "  GCE: http://${gce_ip}:8080"
    fi

    echo ""
}

# ============================================================================
# Entry Point
# ============================================================================

main "$@"
