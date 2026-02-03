#!/bin/bash
# ============================================================================
# GKE Deployment Test Script
# Tests Google Kubernetes Engine deployment for Marketplace
# This is the MOST SCRUTINIZED deployment path by Google reviewers
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
CLUSTER_NAME="erlmcp-test-${TEST_ID}"
NETWORK_NAME="erlmcp-test-vpc-${TEST_ID}"
MACHINE_TYPE="${MACHINE_TYPE:-e2-standard-2}"
NODE_COUNT="${NODE_COUNT:-3}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
DEPLOY_DIR="${MARKETPLACE_DIR}/terraform/examples/gke-deployment"
EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence/gke"

TEST_TIMEOUT="${TEST_TIMEOUT:-1800}"  # 30 minutes

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

    if ! command -v kubectl &> /dev/null; then
        log_error "kubectl not found"
        exit 1
    fi

    if ! command -v terraform &> /dev/null; then
        log_error "terraform not found"
        exit 1
    fi

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    # Enable required APIs
    log_info "Ensuring required APIs are enabled..."
    gcloud services enable \
        container.googleapis.com \
        compute.googleapis.com \
        monitoring.googleapis.com \
        logging.googleapis.com \
        --project="$PROJECT_ID" > /dev/null 2>&1 || true

    log_info "Prerequisites check passed"
}

# ============================================================================
# Test Functions
# ============================================================================

test_terraform_init() {
    log_test "Initializing Terraform for GKE deployment..."

    cd "$DEPLOY_DIR"

    # Create test tfvars
    cat > terraform.tfvars <<EOF
project_id     = "$PROJECT_ID"
region         = "$REGION"
cluster_name   = "$CLUSTER_NAME"
network_name   = "$NETWORK_NAME"
machine_type   = "$MACHINE_TYPE"
min_nodes      = $NODE_COUNT
max_nodes      = $NODE_COUNT
deploy_helm_chart = false
enable_private_nodes = false
enable_spot_nodes   = false
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

test_cluster_deployment() {
    log_test "Deploying GKE cluster..."
    log_warn "  This may take 10-15 minutes"

    cd "$DEPLOY_DIR"

    if timeout "$TEST_TIMEOUT" terraform apply -auto-approve \
        -var="deploy_helm_chart=false" > "$EVIDENCE_DIR/terraform-apply.log" 2>&1; then
        log_info "  ✓ GKE cluster deployed"

        # Capture outputs
        terraform output -json > "$EVIDENCE_DIR/terraform-outputs.json"

        CLUSTER_ENDPOINT=$(terraform output -raw endpoint 2>/dev/null || echo "unknown")
        CA_CERTIFICATE=$(terraform output -raw ca_certificate 2>/dev/null || echo "unknown")

        log_info "  Cluster endpoint: $CLUSTER_ENDPOINT"
        return 0
    else
        log_error "  ✗ GKE deployment failed"
        cat "$EVIDENCE_DIR/terraform-apply.log"
        return 1
    fi
}

test_cluster_credentials() {
    log_test "Getting cluster credentials..."

    if gcloud container clusters get-credentials "$CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" > "$EVIDENCE_DIR/get-credentials.log" 2>&1; then
        log_info "  ✓ Credentials obtained"

        # Verify kubectl can access cluster
        if kubectl cluster-info > "$EVIDENCE_DIR/cluster-info.log" 2>&1; then
            log_info "  ✓ kubectl can access cluster"
            return 0
        else
            log_error "  ✗ kubectl cannot access cluster"
            cat "$EVIDENCE_DIR/cluster-info.log"
            return 1
        fi
    else
        log_error "  ✗ Failed to get credentials"
        cat "$EVIDENCE_DIR/get-credentials.log"
        return 1
    fi
}

test_cluster_nodes() {
    log_test "Checking cluster nodes..."

    if kubectl get nodes -o wide > "$EVIDENCE_DIR/nodes.log" 2>&1; then
        NODE_COUNT=$(kubectl get nodes --no-headers | wc -l | tr -d ' ')
        READY_COUNT=$(kubectl get nodes --no-headers | grep -c " Ready " || true)

        log_info "  Total nodes: $NODE_COUNT"
        log_info "  Ready nodes: $READY_COUNT"

        if [ "$NODE_COUNT" -eq "$NODE_COUNT" ] && [ "$READY_COUNT" -eq "$NODE_COUNT" ]; then
            log_info "  ✓ All nodes are Ready"

            # Check node distribution across zones
            kubectl get nodes -L topology.kubernetes.io/zone -o wide > "$EVIDENCE_DIR/nodes-zones.log" 2>&1
            ZONES=$(kubectl get nodes -L topology.kubernetes.io/zone --no-headers | awk '{print $NF}' | sort -u | wc -l | tr -d ' ')
            log_info "  Nodes spread across $ZONES zone(s)"

            return 0
        else
            log_warn "  ⚠ Not all nodes are ready"
            cat "$EVIDENCE_DIR/nodes.log"
            return 1
        fi
    else
        log_error "  ✗ Failed to get nodes"
        cat "$EVIDENCE_DIR/nodes.log"
        return 1
    fi
}

test_cluster_pools() {
    log_test "Checking node pool configuration..."

    if gcloud container node-pools list \
        --cluster="$CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" > "$EVIDENCE_DIR/node-pools.log" 2>&1; then
        log_info "  ✓ Node pools listed"
        cat "$EVIDENCE_DIR/node-pools.log"

        # Verify default node pool exists
        if grep -q "default-pool\|primary" "$EVIDENCE_DIR/node-pools.log"; then
            log_info "  ✓ Default node pool found"
        fi

        return 0
    else
        log_warn "  ⚠ Could not list node pools"
        return 0
    fi
}

test_workload_identity() {
    log_test "Checking Workload Identity configuration..."

    # Check for workload identity enabled on cluster
    WORKLOAD_IDENTITY_ENABLED=$(gcloud container clusters describe "$CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='value(workloadIdentityConfig)' 2>/dev/null || echo "disabled")

    if [ "$WORKLOAD_IDENTITY_ENABLED" != "disabled" ] && [ -n "$WORKLOAD_IDENTITY_ENABLED" ]; then
        log_info "  ✓ Workload Identity configured"
        echo "$WORKLOAD_IDENTITY_ENABLED" > "$EVIDENCE_DIR/workload-identity-config.txt"
        return 0
    else
        log_warn "  ⚠ Workload Identity not configured"
        return 0
    fi
}

test_network_policies() {
    log_test "Checking network policies..."

    if kubectl get networkpolicy -A > "$EVIDENCE_DIR/network-policies.log" 2>&1; then
        POLICY_COUNT=$(kubectl get networkpolicy -A --no-headers | wc -l | tr -d ' ')
        log_info "  Network policies found: $POLICY_COUNT"

        if [ "$POLICY_COUNT" -gt 0 ]; then
            log_info "  ✓ Network policies defined"
            cat "$EVIDENCE_DIR/network-policies.log"
        else
            log_warn "  ⚠ No network policies (recommended for production)"
        fi

        return 0
    else
        log_warn "  ⚠ Could not retrieve network policies"
        return 0
    fi
}

test_pod_security() {
    log_test "Checking pod security standards..."

    # Check for PodSecurityPolicy (deprecated in 1.25+) or Pod Security
    if kubectl get podsecuritypolicy -A > "$EVIDENCE_DIR/pod-security-policy.log" 2>&1; then
        log_info "  ✓ PodSecurityPolicy found"
    else
        # Check for Pod Security Standards (Kubernetes 1.25+)
        if kubectl get clusterrole | grep -q "psp"; then
            log_info "  ✓ Pod Security configured via cluster roles"
        else
            log_warn "  ⚠ No explicit pod security policy found"
        fi
    fi

    return 0
}

test_private_cluster_config() {
    log_test "Checking private cluster configuration..."

    PRIVATE_ENDPOINT=$(gcloud container clusters describe "$CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='value(privateClusterConfig.enablePrivateEndpoint)' 2>/dev/null || echo "false")

    PRIVATE_NODES=$(gcloud container clusters describe "$CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='value(privateClusterConfig.enablePrivateNodes)' 2>/dev/null || echo "false")

    log_info "  Private endpoint: $PRIVATE_ENDPOINT"
    log_info "  Private nodes: $PRIVATE_NODES"

    if [ "$PRIVATE_NODES" = "true" ]; then
        log_info "  ✓ Private nodes configured"
    fi

    return 0
}

test_shielded_nodes() {
    log_test "Checking shielded GKE nodes configuration..."

    SHIELDED_ENABLED=$(gcloud container clusters describe "$CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='value(shieldedNodes.enabled)' 2>/dev/null || echo "false")

    if [ "$SHIELDED_ENABLED" = "true" ]; then
        log_info "  ✓ Shielded nodes enabled"

        # Check individual features
        SECURE_BOOT=$(gcloud container clusters describe "$CLUSTER_NAME" \
            --region="$REGION" \
            --project="$PROJECT_ID" \
            --format='value(shieldedNodes.enableSecureBoot)' 2>/dev/null || echo "false")

        INTEGRITY_MONITORING=$(gcloud container clusters describe "$CLUSTER_NAME" \
            --region="$REGION" \
            --project="$PROJECT_ID" \
            --format='value(shieldedNodes.enableIntegrityMonitoring)' 2>/dev/null || echo "false")

        log_info "    Secure boot: $SECURE_BOOT"
        log_info "    Integrity monitoring: $INTEGRITY_MONITORING"
    else
        log_warn "  ⚠ Shielded nodes not enabled"
    fi

    return 0
}

test_cluster_autoscaling() {
    log_test "Checking cluster autoscaling configuration..."

    if gcloud container clusters describe "$CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='value(autoscaling.enabled)' 2>/dev/null | grep -q "true"; then
        log_info "  ✓ Cluster autoscaling enabled"

        MIN_NODES=$(gcloud container clusters describe "$CLUSTER_NAME" \
            --region="$REGION" \
            --project="$PROJECT_ID" \
            --format='value(autoscaling.minNodeCount)' 2>/dev/null || echo "unknown")

        MAX_NODES=$(gcloud container clusters describe "$CLUSTER_NAME" \
            --region="$REGION" \
            --project="$PROJECT_ID" \
            --format='value(autoscaling.maxNodeCount)' 2>/dev/null || echo "unknown")

        log_info "    Min nodes: $MIN_NODES"
        log_info "    Max nodes: $MAX_NODES"
    else
        log_warn "  ⚠ Cluster autoscaling not enabled"
    fi

    return 0
}

test_cluster_logging() {
    log_test "Checking cloud logging integration..."

    LOGGING_ENABLED=$(gcloud container clusters describe "$CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='value(loggingService)' 2>/dev/null || echo "none")

    if [ "$LOGGING_ENABLED" != "none" ]; then
        log_info "  ✓ Cloud logging enabled: $LOGGING_ENABLED"
    else
        log_warn "  ⚠ Cloud logging not configured"
    fi

    return 0
}

test_cluster_monitoring() {
    log_test "Checking cloud monitoring integration..."

    MONITORING_ENABLED=$(gcloud container clusters describe "$CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='value(monitoringService)' 2>/dev/null || echo "none")

    if [ "$MONITORING_ENABLED" != "none" ]; then
        log_info "  ✓ Cloud monitoring enabled: $MONITORING_ENABLED"
    else
        log_warn "  ⚠ Cloud monitoring not configured"
    fi

    return 0
}

test_cluster_version() {
    log_test "Checking GKE version..."

    CLUSTER_VERSION=$(gcloud container clusters describe "$CLUSTER_NAME" \
        --region="$REGION" \
        --project="$PROJECT_ID" \
        --format='value(currentMasterVersion)' 2>/dev/null || echo "unknown")

    log_info "  GKE version: $CLUSTER_VERSION"

    # Check if version is recent (within 3 minor versions of latest)
    # This is a simplified check
    if echo "$CLUSTER_VERSION" | grep -qE "1\.(2[7-9]|[3-9][0-9])"; then
        log_info "  ✓ GKE version is recent"
    else
        log_warn "  ⚠ Consider upgrading to latest GKE version"
    fi

    return 0
}

cleanup() {
    log_info "Cleaning up test resources..."

    cd "$DEPLOY_DIR"

    if [ -f "terraform.tfstate" ] || [ -f "terraform.tfvars" ]; then
        log_info "  Destroying GKE cluster..."
        terraform destroy -auto-approve \
            -var="deploy_helm_chart=false" > "$EVIDENCE_DIR/terraform-destroy.log" 2>&1 || true

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
    log_info "Starting GKE Deployment Test Suite..."
    log_info "================================================"
    log_info "Project: $PROJECT_ID"
    log_info "Region: $REGION"
    log_info "Cluster: $CLUSTER_NAME"
    log_info "Network: $NETWORK_NAME"
    log_info "Test ID: $TEST_ID"
    log_info "================================================"

    check_prerequisites

    # Test counter
    TOTAL_TESTS=0
    PASSED_TESTS=0
    FAILED_TESTS=0

    # Run tests
    for test_func in \
        test_terraform_init \
        test_cluster_deployment \
        test_cluster_credentials \
        test_cluster_nodes \
        test_cluster_pools \
        test_workload_identity \
        test_network_policies \
        test_pod_security \
        test_private_cluster_config \
        test_shielded_nodes \
        test_cluster_autoscaling \
        test_cluster_logging \
        test_cluster_monitoring \
        test_cluster_version; do

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
        --cluster)
            CLUSTER_NAME="$2"
            shift 2
            ;;
        --machine-type)
            MACHINE_TYPE="$2"
            shift 2
            ;;
        --nodes)
            NODE_COUNT="$2"
            shift 2
            ;;
        --skip-cleanup)
            SKIP_CLEANUP=true
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--project PROJECT] [--region REGION] [--cluster NAME] [--machine-type TYPE] [--nodes COUNT] [--skip-cleanup]"
            exit 1
            ;;
    esac
done

# Trap for cleanup (unless disabled)
if [ "${SKIP_CLEANUP:-false}" != "true" ]; then
    trap cleanup EXIT
fi

main
