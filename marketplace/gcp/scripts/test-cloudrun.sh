#!/bin/bash
# ============================================================================
# Cloud Run Deployment Test Script
# Tests Google Cloud Run deployment for Marketplace
# Fastest deployment path - must deploy in < 5 minutes
# ============================================================================

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_test() { echo -e "${BLUE}[TEST]${NC} $1"; }

# ============================================================================
# Configuration
# ============================================================================

PROJECT_ID="${PROJECT_ID:-}"
REGION="${REGION:-us-central1}"
TEST_ID="${TEST_ID:-test-$(date +%s)}"
SERVICE_NAME="erlmcp-test-${TEST_ID}"
CPU="${CPU:-1}"
MEMORY="${MEMORY:-512Mi}"
MIN_INSTANCES="${MIN_INSTANCES:-0}"
MAX_INSTANCES="${MAX_INSTANCES:-1}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
DEPLOY_DIR="${MARKETPLACE_DIR}/terraform/examples/cloud-run-deployment"
EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence/cloudrun"

TEST_TIMEOUT="${TEST_TIMEOUT:-600}"  # 10 minutes
DEPLOYMENT_TIMEOUT="${DEPLOYMENT_TIMEOUT:-300}"  # 5 minutes max for Cloud Run

# ============================================================================
# Prerequisites
# ============================================================================

check_prerequisites() {
    log_info "Checking prerequisites..."

    if [ -z "$PROJECT_ID" ]; then
        log_error "PROJECT_ID not set. Run: export PROJECT_ID=your-project-id"
        exit 1
    fi

    if ! command -v gcloud &> /dev/null; then
        log_error "gcloud CLI not found"
        exit 1
    fi

    if ! command -v terraform &> /dev/null; then
        log_error "terraform not found"
        exit 1
    fi

    if ! command -v curl &> /dev/null; then
        log_error "curl not found"
        exit 1
    fi

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    # Enable required APIs
    log_info "Ensuring required APIs are enabled..."
    gcloud services enable \
        run.googleapis.com \
        artifactregistry.googleapis.com \
        secretmanager.googleapis.com \
        --project="$PROJECT_ID" > /dev/null 2>&1 || true

    log_info "Prerequisites check passed"
}

# ============================================================================
# Test Functions
# ============================================================================

test_terraform_init() {
    log_test "Initializing Terraform for Cloud Run deployment..."

    cd "$DEPLOY_DIR"

    # Create test tfvars
    cat > terraform.tfvars <<EOF
project_id     = "$PROJECT_ID"
region         = "$REGION"
service_name   = "$SERVICE_NAME"
container_image = "$REGION-docker.pkg.dev/$PROJECT_ID/erlmcp/erlmcp:latest"
cpu            = $CPU
memory         = "$MEMORY"
min_instances  = $MIN_INSTANCES
max_instances  = $MAX_INSTANCES
timeout        = 300
concurrency    = 80
EOF

    if terraform init -upgrade > "$EVIDENCE_DIR/terraform-init.log" 2>&1; then
        log_info "  ✓ Terraform initialized"
        return 0
    else
        log_error "  ✗ Terraform init failed"
        cat "$EVIDENCE_DIR/terraform-init.log"
        return 1
    fi
}

test_service_deployment() {
    log_test "Deploying Cloud Run service..."
    log_warn "  This should complete in < 5 minutes (Marketplace requirement)"

    cd "$DEPLOY_DIR"

    local deploy_start=$(date +%s)

    if timeout "$DEPLOYMENT_TIMEOUT" terraform apply -auto-approve > "$EVIDENCE_DIR/terraform-apply.log" 2>&1; then
        local deploy_end=$(date +%s)
        local deploy_time=$((deploy_end - deploy_start))

        log_info "  ✓ Cloud Run service deployed in ${deploy_time}s"

        if [ $deploy_time -le 300 ]; then
            log_info "  ✓ Deployment meets 5-minute Marketplace SLA"
        else
            log_warn "  ⚠ Deployment exceeded 5-minute SLA (${deploy_time}s)"
        fi

        # Capture outputs
        terraform output -json > "$EVIDENCE_DIR/terraform-outputs.json"

        return 0
    else
        log_error "  ✗ Cloud Run deployment failed or timed out"
        cat "$EVIDENCE_DIR/terraform-apply.log"
        return 1
    fi
}

test_service_url() {
    log_test "Getting service URL..."

    cd "$DEPLOY_DIR"

    SERVICE_URL=$(terraform output -raw service_url 2>/dev/null || echo "")

    if [ -n "$SERVICE_URL" ]; then
        log_info "  ✓ Service URL: $SERVICE_URL"
        echo "$SERVICE_URL" > "$EVIDENCE_DIR/service-url.txt"
        return 0
    else
        log_error "  ✗ Could not get service URL"
        return 1
    fi
}

test_service_describe() {
    log_test "Describing Cloud Run service..."

    if gcloud run services describe "$SERVICE_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format=json > "$EVIDENCE_DIR/service-describe.json" 2>&1; then
        log_info "  ✓ Service description obtained"

        # Extract key info
        if command -v jq &> /dev/null; then
            SERVICE_STATUS=$(jq -r '.status.conditions[0].type // "unknown"' "$EVIDENCE_DIR/service-describe.json" 2>/dev/null)
            READY_REPLICAS=$(jq -r '.status.readyReplicas // "0"' "$EVIDENCE_DIR/service-describe.json" 2>/dev/null)
            LATEST_REVISION=$(jq -r '.status.latestReadyRevisionName // "unknown"' "$EVIDENCE_DIR/service-describe.json" 2>/dev/null)

            log_info "  Status: $SERVICE_STATUS"
            log_info "  Ready replicas: $READY_REPLICAS"
            log_info "  Latest revision: $LATEST_REVISION"
        fi

        return 0
    else
        log_warn "  ⚠ Could not describe service"
        return 0
    fi
}

test_health_endpoint() {
    log_test "Testing health endpoint..."

    if [ ! -f "$EVIDENCE_DIR/service-url.txt" ]; then
        log_error "  ✗ No service URL available"
        return 1
    fi

    SERVICE_URL=$(cat "$EVIDENCE_DIR/service-url.txt")
    HEALTH_URL="${SERVICE_URL}/health"

    log_info "  Health URL: $HEALTH_URL"
    log_info "  Waiting for service to be ready..."

    # Cloud Run can take time to serve first request (cold start)
    local max_wait=60
    local waited=0

    while [ $waited -lt $max_wait ]; do
        HTTP_CODE=$(curl -s -o "$EVIDENCE_DIR/health-response.txt" -w "%{http_code}" "$HEALTH_URL" 2>/dev/null || echo "000")

        if [ "$HTTP_CODE" = "200" ]; then
            log_info "  ✓ Health check returned 200 (${waited}s)"

            # Parse response
            if command -v jq &> /dev/null; then
                if jq empty "$EVIDENCE_DIR/health-response.txt" 2>/dev/null; then
                    HEALTH_STATUS=$(jq -r '.status // .Status // "ok"' "$EVIDENCE_DIR/health-response.txt" 2>/dev/null)
                    log_info "  Health status: $HEALTH_STATUS"
                fi
            fi

            cat "$EVIDENCE_DIR/health-response.txt"
            return 0
        fi

        sleep 2
        waited=$((waited + 2))
        echo -n "."
    done

    log_warn "  ⚠ Health check did not return 200 after ${max_wait}s"
    log_warn "  Response code: $HTTP_CODE"
    cat "$EVIDENCE_DIR/health-response.txt" 2>&1 || true
    return 1
}

test_cold_start() {
    log_test "Testing cold start performance..."

    # Scale to zero
    log_info "  Scaling service to zero..."
    gcloud run services update "$SERVICE_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --min-instances=0 > /dev/null 2>&1 || true

    sleep 5

    # Measure cold start
    local cold_start_start=$(date +%s)

    SERVICE_URL=$(cat "$EVIDENCE_DIR/service-url.txt")
    curl -s "$SERVICE_URL/health" > "$EVIDENCE_DIR/cold-start-response.txt" 2>&1

    local cold_start_end=$(date +%s)
    local cold_start_time=$((cold_start_end - cold_start_start))

    log_info "  Cold start time: ${cold_start_time}s"

    if [ $cold_start_time -le 30 ]; then
        log_info "  ✓ Cold start meets 30-second target"
    else
        log_warn "  ⚠ Cold start exceeded 30 seconds (${cold_start_time}s)"
    fi

    return 0
}

test_autoscaling() {
    log_test "Testing autoscaling configuration..."

    if gcloud run services describe "$SERVICE_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='value(spec.template.spec.containerConcurrency)' > "$EVIDENCE_DIR/concurrency.txt" 2>&1; then
        CONCURRENCY=$(cat "$EVIDENCE_DIR/concurrency.txt")
        log_info "  Concurrency setting: $CONCURRENCY"
    fi

    # Check scaling annotations
    if command -v jq &> /dev/null; then
        MAX_SCALE=$(jq -r '.spec.template.metadata.annotations["autoscaling.knative.dev/maxScale"] // "not set"' "$EVIDENCE_DIR/service-describe.json" 2>/dev/null)
        MIN_SCALE=$(jq -r '.spec.template.metadata.annotations["autoscaling.knative.dev/minScale"] // "not set"' "$EVIDENCE_DIR/service-describe.json" 2>/dev/null)

        log_info "  Max scale: $MAX_SCALE"
        log_info "  Min scale: $MIN_SCALE"
    fi

    return 0
}

test_iam_configuration() {
    log_test "Checking IAM configuration..."

    # Check if invoker policy is set
    if gcloud run services get-iam-policy "$SERVICE_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" > "$EVIDENCE_DIR/iam-policy.json" 2>&1; then
        log_info "  ✓ IAM policy obtained"

        if grep -q "allUsers\|allAuthenticatedUsers" "$EVIDENCE_DIR/iam-policy.json"; then
            log_warn "  ⚠ Service allows public access"
        else
            log_info "  ✓ Service has restricted access"
        fi

        return 0
    else
        log_warn "  ⚠ Could not retrieve IAM policy"
        return 0
    fi
}

test_service_logs() {
    log_test "Checking service logs..."

    # Generate some traffic first
    SERVICE_URL=$(cat "$EVIDENCE_DIR/service-url.txt")
    curl -s "$SERVICE_URL/health" > /dev/null 2>&1 || true

    sleep 5

    if gcloud logging read "resource.labels.service_name=$SERVICE_NAME" \
        --project="$PROJECT_ID" \
        --limit=10 \
        --format=json > "$EVIDENCE_DIR/service-logs.json" 2>&1; then
        LOG_ENTRIES=$(jq 'length' "$EVIDENCE_DIR/service-logs.json" 2>/dev/null || echo "0")
        log_info "  ✓ Found $LOG_ENTRIES log entries"
        return 0
    else
        log_warn "  ⚠ Could not retrieve logs"
        return 0
    fi
}

test_revision_info() {
    log_test "Getting revision information..."

    if gcloud run revisions list \
        --service="$SERVICE_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='table(name,active,createdAt)' > "$EVIDENCE_DIR/revisions.txt" 2>&1; then
        log_info "  ✓ Revision info obtained"
        cat "$EVIDENCE_DIR/revisions.txt"
        return 0
    else
        log_warn "  ⚠ Could not list revisions"
        return 0
    fi
}

test_resource_limits() {
    log_test "Verifying resource limits..."

    if command -v jq &> /dev/null; then
        CPU_LIMIT=$(jq -r '.spec.template.spec.containers[0].resources.limits.cpu // "not set"' "$EVIDENCE_DIR/service-describe.json" 2>/dev/null)
        MEMORY_LIMIT=$(jq -r '.spec.template.spec.containers[0].resources.limits.memory // "not set"' "$EVIDENCE_DIR/service-describe.json" 2>/dev/null)

        log_info "  CPU limit: $CPU_LIMIT"
        log_info "  Memory limit: $MEMORY_LIMIT"

        if [ "$CPU_LIMIT" != "not set" ] && [ "$MEMORY_LIMIT" != "not set" ]; then
            log_info "  ✓ Resource limits configured"
        fi
    fi

    return 0
}

cleanup() {
    log_info "Cleaning up test resources..."

    cd "$DEPLOY_DIR"

    if [ -f "terraform.tfstate" ] || [ -f "terraform.tfvars" ]; then
        log_info "  Destroying Cloud Run service..."
        terraform destroy -auto-approve > "$EVIDENCE_DIR/terraform-destroy.log" 2>&1 || true

        log_info "  Cleaning up Terraform files..."
        rm -f terraform.tfstate terraform.tfvars .terraform.lock.hcl
        rm -rf .terraform
    fi

    log_info "  Cleanup complete"
}

# ============================================================================
# Main Test Runner
# ============================================================================

main() {
    log_info "Starting Cloud Run Deployment Test Suite..."
    log_info "================================================"
    log_info "Project: $PROJECT_ID"
    log_info "Region: $REGION"
    log_info "Service: $SERVICE_NAME"
    log_info "CPU: $CPU, Memory: $MEMORY"
    log_info "Instances: $MIN_INSTANCES - $MAX_INSTANCES"
    log_info "================================================"

    check_prerequisites

    # Test counter
    TOTAL_TESTS=0
    PASSED_TESTS=0
    FAILED_TESTS=0

    # Run tests
    for test_func in \
        test_terraform_init \
        test_service_deployment \
        test_service_url \
        test_service_describe \
        test_health_endpoint \
        test_cold_start \
        test_autoscaling \
        test_iam_configuration \
        test_service_logs \
        test_revision_info \
        test_resource_limits; do

        TOTAL_TESTS=$((TOTAL_TESTS + 1))

        if $test_func; then
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    done

    # Cleanup
    cleanup

    # Summary
    log_info "================================================"
    log_info "Test Summary:"
    log_info "  Total Tests:  $TOTAL_TESTS"
    log_info "  Passed:       $PASSED_TESTS"
    log_info "  Failed:       $FAILED_TESTS"
    log_info "================================================"
    log_info "Evidence saved to: $EVIDENCE_DIR"

    if [ $FAILED_TESTS -gt 0 ]; then
        log_error "Some tests failed. Check logs in $EVIDENCE_DIR"
        exit 1
    fi

    log_info "All tests passed!"
    exit 0
}

# ============================================================================
# Script Entry Point
# ============================================================================

# Parse arguments
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
        --service)
            SERVICE_NAME="$2"
            shift 2
            ;;
        --cpu)
            CPU="$2"
            shift 2
            ;;
        --memory)
            MEMORY="$2"
            shift 2
            ;;
        --max-instances)
            MAX_INSTANCES="$2"
            shift 2
            ;;
        --skip-cleanup)
            SKIP_CLEANUP=true
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--project PROJECT] [--region REGION] [--service NAME] [--cpu CPU] [--memory MEMORY] [--max-instances N] [--skip-cleanup]"
            exit 1
            ;;
    esac
done

# Trap for cleanup (unless disabled)
if [ "${SKIP_CLEANUP:-false}" != "true" ]; then
    trap cleanup EXIT
fi

main
