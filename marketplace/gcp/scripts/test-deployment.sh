#!/bin/bash
# ============================================================================
# End-to-End Test Script for GCP Marketplace Deployments
# Tests GKE, Cloud Run, and Compute Engine deployments
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
ZONE="${ZONE:-us-central1-a}"
TEST_ID="${TEST_ID:-test-$(date +%s)}"
CLUSTER_NAME="erlmcp-test-${TEST_ID}"
SERVICE_NAME="erlmcp-test-${TEST_ID}"
INSTANCE_NAME="erlmcp-test-${TEST_ID}"

TEST_TIMEOUT="${TEST_TIMEOUT:-600}"  # 10 minutes default

# ============================================================================
# Validation
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

    if ! command -v kubectl &> /dev/null; then
        log_error "kubectl not found"
        exit 1
    fi

    if ! command -v curl &> /dev/null; then
        log_error "curl not found"
        exit 1
    fi

    log_info "Prerequisites check passed"
}

# ============================================================================
# Test Functions
# ============================================================================

test_terraform_validate() {
    log_test "Validating Terraform configurations..."

    local dir=$1
    cd "$dir"

    terraform init -upgrade -backend=false -input=false > /dev/null 2>&1
    if terraform validate > /dev/null 2>&1; then
        log_info "  ✓ Terraform validation passed for $dir"
        return 0
    else
        log_error "  ✗ Terraform validation failed for $dir"
        return 1
    fi
}

test_gke_deployment() {
    log_test "Testing GKE deployment..."

    local deploy_dir="marketplace/gcp/terraform/examples/gke-deployment"

    # Create test tfvars
    cat > "$deploy_dir/terraform.tfvars" <<EOF
project_id    = "$PROJECT_ID"
region        = "$REGION"
cluster_name  = "$CLUSTER_NAME"
machine_type  = "e2-standard-2"
min_nodes     = 1
max_nodes     = 2
deploy_helm_chart = false
EOF

    cd "$deploy_dir"

    # Deploy
    timeout "$TEST_TIMEOUT" terraform apply -auto-approve \
        -var="create_spot_node_pool=false" \
        -var="deploy_helm_chart=false" \
        2>&1 | tee /tmp/gke-deploy.log

    if [ ${PIPESTATUS[0]} -eq 0 ]; then
        log_info "  ✓ GKE cluster deployed successfully"

        # Get cluster credentials
        gcloud container clusters get-credentials "$CLUSTER_NAME" \
            --region="$REGION" --project="$PROJECT_ID"

        # Verify cluster is accessible
        if kubectl get nodes &> /dev/null; then
            log_info "  ✓ Cluster is accessible via kubectl"
        else
            log_error "  ✗ Cluster is not accessible"
            return 1
        fi
    else
        log_error "  ✗ GKE deployment failed"
        return 1
    fi

    # Cleanup
    log_info "  Cleaning up GKE resources..."
    terraform destroy -auto-approve 2>&1 | tee /tmp/gke-destroy.log

    return 0
}

test_cloud_run_deployment() {
    log_test "Testing Cloud Run deployment..."

    local deploy_dir="marketplace/gcp/terraform/examples/cloud-run-deployment"

    # Create test tfvars
    cat > "$deploy_dir/terraform.tfvars" <<EOF
project_id   = "$PROJECT_ID"
region       = "$REGION"
service_name = "$SERVICE_NAME"
cpu          = "1"
memory       = "512Mi"
min_instances = 0
max_instances = 1
EOF

    cd "$deploy_dir"

    # Deploy
    timeout "$TEST_TIMEOUT" terraform apply -auto-approve \
        2>&1 | tee /tmp/cloudrun-deploy.log

    if [ ${PIPESTATUS[0]} -eq 0 ]; then
        log_info "  ✓ Cloud Run service deployed successfully"

        # Get service URL
        SERVICE_URL=$(terraform output -raw service_url)
        log_info "  Service URL: $SERVICE_URL"

        # Test health endpoint
        if curl -f -s "$SERVICE_URL/health" > /dev/null 2>&1; then
            log_info "  ✓ Health check endpoint is accessible"
        else
            log_warn "  ⚠ Health check returned non-200 (expected if service not running)"
        fi
    else
        log_error "  ✗ Cloud Run deployment failed"
        return 1
    fi

    # Cleanup
    log_info "  Cleaning up Cloud Run resources..."
    terraform destroy -auto-approve 2>&1 | tee /tmp/cloudrun-destroy.log

    return 0
}

test_compute_engine_deployment() {
    log_test "Testing Compute Engine deployment..."

    local deploy_dir="marketplace/gcp/terraform/examples/gce-deployment"

    # Create test tfvars
    cat > "$deploy_dir/terraform.tfvars" <<EOF
project_id     = "$PROJECT_ID"
zone           = "$ZONE"
instance_name  = "$INSTANCE_NAME"
instance_count = 1
machine_type   = "e2-medium"
create_load_balancer = false
EOF

    cd "$deploy_dir"

    # Deploy
    timeout "$TEST_TIMEOUT" terraform apply -auto-approve \
        2>&1 | tee /tmp/gce-deploy.log

    if [ ${PIPESTATUS[0]} -eq 0 ]; then
        log_info "  ✓ Compute Engine instance deployed successfully"

        # Get external IP
        EXTERNAL_IP=$(terraform output -raw instance_external_ips | tr -d '[]')
        log_info "  External IP: $EXTERNAL_IP"

        # Test health endpoint (may take time to start)
        log_info "  Waiting for instance to start (30 seconds)..."
        sleep 30

        if curl -f -s --connect-timeout 5 "http://$EXTERNAL_IP/health" > /dev/null 2>&1; then
            log_info "  ✓ Health check endpoint is accessible"
        else
            log_warn "  ⚠ Health check not yet responding (may need more time)"
        fi
    else
        log_error "  ✗ Compute Engine deployment failed"
        return 1
    fi

    # Cleanup
    log_info "  Cleaning up Compute Engine resources..."
    terraform destroy -auto-approve 2>&1 | tee /tmp/gce-destroy.log

    return 0
}

# ============================================================================
# Main Test Runner
# ============================================================================

main() {
    log_info "Starting end-to-end tests for GCP Marketplace deployment..."
    log_info "================================================"
    log_info "Project: $PROJECT_ID"
    log_info "Region: $REGION"
    log_info "Zone: $ZONE"
    log_info "Test ID: $TEST_ID"
    log_info "================================================"

    check_prerequisites

    # Test counter
    TOTAL_TESTS=0
    PASSED_TESTS=0
    FAILED_TESTS=0

    # Validate Terraform
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    if test_terraform_validate "marketplace/gcp/terraform/modules/gke"; then
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi

    # Test GKE (if enabled)
    if [ "${TEST_GKE:-true}" == "true" ]; then
        TOTAL_TESTS=$((TOTAL_TESTS + 1))
        if test_gke_deployment; then
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    fi

    # Test Cloud Run (if enabled)
    if [ "${TEST_CLOUD_RUN:-true}" == "true" ]; then
        TOTAL_TESTS=$((TOTAL_TESTS + 1))
        if test_cloud_run_deployment; then
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    fi

    # Test Compute Engine (if enabled)
    if [ "${TEST_COMPUTE_ENGINE:-true}" == "true" ]; then
        TOTAL_TESTS=$((TOTAL_TESTS + 1))
        if test_compute_engine_deployment; then
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    fi

    # Summary
    log_info "================================================"
    log_info "Test Summary:"
    log_info "  Total Tests:  $TOTAL_TESTS"
    log_info "  Passed:       $PASSED_TESTS"
    log_info "  Failed:       $FAILED_TESTS"
    log_info "================================================"

    if [ $FAILED_TESTS -gt 0 ]; then
        log_error "Some tests failed. Check logs in /tmp/ for details."
        exit 1
    fi

    log_info "All tests passed successfully!"
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
        --zone)
            ZONE="$2"
            shift 2
            ;;
        --skip-gke)
            TEST_GKE=false
            shift
            ;;
        --skip-cloud-run)
            TEST_CLOUD_RUN=false
            shift
            ;;
        --skip-gce)
            TEST_COMPUTE_ENGINE=false
            shift
            ;;
        --gke-only)
            TEST_CLOUD_RUN=false
            TEST_COMPUTE_ENGINE=false
            shift
            ;;
        --cloud-run-only)
            TEST_GKE=false
            TEST_COMPUTE_ENGINE=false
            shift
            ;;
        --gce-only)
            TEST_GKE=false
            TEST_CLOUD_RUN=false
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--project PROJECT] [--region REGION] [--zone ZONE] [--skip-gke] [--skip-cloud-run] [--skip-gce] [--gke-only] [--cloud-run-only] [--gce-only]"
            exit 1
            ;;
    esac
done

main
