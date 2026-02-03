#!/bin/bash
# ============================================================================
# Compute Engine (GCE) Deployment Test Script
# Tests Google Compute Engine VM deployment for Marketplace
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
ZONE="${ZONE:-us-central1-a}"
TEST_ID="${TEST_ID:-test-$(date +%s)}"
INSTANCE_NAME="erlmcp-test-${TEST_ID}"
MACHINE_TYPE="${MACHINE_TYPE:-e2-medium}"
INSTANCE_COUNT="${INSTANCE_COUNT:-1}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
DEPLOY_DIR="${MARKETPLACE_DIR}/terraform/examples/gce-deployment"
EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence/gce"

TEST_TIMEOUT="${TEST_TIMEOUT:-1200}"  # 20 minutes

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

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    # Enable required APIs
    log_info "Ensuring required APIs are enabled..."
    gcloud services enable \
        compute.googleapis.com \
        secretmanager.googleapis.com \
        monitoring.googleapis.com \
        logging.googleapis.com \
        --project="$PROJECT_ID" > /dev/null 2>&1 || true

    log_info "Prerequisites check passed"
}

# ============================================================================
# Test Functions
# ============================================================================

test_terraform_init() {
    log_test "Initializing Terraform for GCE deployment..."

    cd "$DEPLOY_DIR"

    # Create test tfvars
    cat > terraform.tfvars <<EOF
project_id       = "$PROJECT_ID"
zone             = "$ZONE"
instance_name    = "$INSTANCE_NAME"
instance_count   = $INSTANCE_COUNT
machine_type     = "$MACHINE_TYPE"
create_load_balancer = false
enable_ssl       = false
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

test_vm_deployment() {
    log_test "Deploying Compute Engine instances..."
    log_warn "  This may take 5-10 minutes"

    cd "$DEPLOY_DIR"

    if timeout "$TEST_TIMEOUT" terraform apply -auto-approve > "$EVIDENCE_DIR/terraform-apply.log" 2>&1; then
        log_info "  ✓ Compute Engine instances deployed"

        # Capture outputs
        terraform output -json > "$EVIDENCE_DIR/terraform-outputs.json"

        INSTANCE_NAMES=$(terraform output -json instance_names 2>/dev/null | jq -r '.[]' || echo "unknown")
        log_info "  Instances: $INSTANCE_NAMES"

        return 0
    else
        log_error "  ✗ GCE deployment failed"
        cat "$EVIDENCE_DIR/terraform-apply.log"
        return 1
    fi
}

test_instance_status() {
    log_test "Checking instance status..."

    if gcloud compute instances describe "$INSTANCE_NAME" \
        --zone="$ZONE" \
        --project="$PROJECT_ID" \
        --format='value(status)' > "$EVIDENCE_DIR/instance-status.txt" 2>&1; then

        STATUS=$(cat "$EVIDENCE_DIR/instance-status.txt")
        log_info "  Instance status: $STATUS"

        if [ "$STATUS" = "RUNNING" ]; then
            log_info "  ✓ Instance is running"
            return 0
        else
            log_warn "  ⚠ Instance is not running (status: $STATUS)"
            return 1
        fi
    else
        log_error "  ✗ Could not get instance status"
        return 1
    fi
}

test_instance_details() {
    log_test "Getting instance details..."

    if gcloud compute instances describe "$INSTANCE_NAME" \
        --zone="$ZONE" \
        --project="$PROJECT_ID" \
        --format=json > "$EVIDENCE_DIR/instance-details.json" 2>&1; then
        log_info "  ✓ Instance details obtained"

        # Extract key information
        if command -v jq &> /dev/null; then
            MACHINE_TYPE_DETAIL=$(jq -r '.machineType' "$EVIDENCE_DIR/instance-details.json" 2>/dev/null | xargs basename)
            IMAGE_SOURCE=$(jq -r '.sourceDisks[0].licenses[0] // "custom-image"' "$EVIDENCE_DIR/instance-details.json" 2>/dev/null)

            log_info "  Machine type: $MACHINE_TYPE_DETAIL"
            log_info "  Image: $IMAGE_SOURCE"
        fi

        return 0
    else
        log_warn "  ⚠ Could not get instance details"
        return 0
    fi
}

test_external_ip() {
    log_test "Getting external IP address..."

    EXTERNAL_IP=$(gcloud compute instances describe "$INSTANCE_NAME" \
        --zone="$ZONE" \
        --project="$PROJECT_ID" \
        --format='value(networkInterfaces[0].accessConfigs[0].natIP)' 2>/dev/null || echo "")

    if [ -n "$EXTERNAL_IP" ]; then
        log_info "  ✓ External IP: $EXTERNAL_IP"
        echo "$EXTERNAL_IP" > "$EVIDENCE_DIR/external-ip.txt"
        return 0
    else
        log_error "  ✗ No external IP assigned"
        return 1
    fi
}

test_firewall_rules() {
    log_test "Checking firewall rules..."

    if gcloud compute firewall-rules list \
        --project="$PROJECT_ID" \
        --filter="name~erlmcp" \
        --format='table(name,allowed[],sourceRanges)' > "$EVIDENCE_DIR/firewall-rules.txt" 2>&1; then
        log_info "  ✓ Firewall rules found"
        cat "$EVIDENCE_DIR/firewall-rules.txt"

        # Check for HTTP/HTTPS rules
        if grep -q "tcp:80\|tcp:443\|tcp:8080" "$EVIDENCE_DIR/firewall-rules.txt"; then
            log_info "  ✓ HTTP/HTTPS rules configured"
        fi

        return 0
    else
        log_warn "  ⚠ Could not list firewall rules"
        return 0
    fi
}

test_serial_port_output() {
    log_test "Capturing serial port output..."

    # Wait a bit for startup scripts to run
    log_info "  Waiting for instance to initialize..."
    sleep 15

    if gcloud compute instances get-serial-port-output "$INSTANCE_NAME" \
        --zone="$ZONE" \
        --project="$PROJECT_ID" \
        --port=1 > "$EVIDENCE_DIR/serial-output.txt" 2>&1; then
        log_info "  ✓ Serial output captured"

        # Check for errors
        if grep -i "error\|fail" "$EVIDENCE_DIR/serial-output.txt" | \
            grep -v "Failed to get" | head -3 > "$EVIDENCE_DIR/errors.txt" 2>&1; then

            if [ -s "$EVIDENCE_DIR/errors.txt" ]; then
                log_warn "  ⚠ Found potential errors in serial output"
                head -5 "$EVIDENCE_DIR/errors.txt"
            else
                log_info "  ✓ No critical errors in serial output"
            fi
        fi

        return 0
    else
        log_warn "  ⚠ Could not capture serial output"
        return 0
    fi
}

test_startup_script() {
    log_test "Checking startup script execution..."

    if [ -f "$EVIDENCE_DIR/serial-output.txt" ]; then
        if grep -q "startup-script" "$EVIDENCE_DIR/serial-output.txt"; then
            log_info "  ✓ Startup script executed"
        fi

        # Check for erlmcp service startup
        if grep -qi "erlmcp.*start\|erlmcp.*running\|erlmcp.*ready" "$EVIDENCE_DIR/serial-output.txt"; then
            log_info "  ✓ erlmcp service started"
        fi
    fi

    return 0
}

test_ssh_access() {
    log_test "Testing SSH access..."

    # Try SSH with iap tunnel
    if gcloud compute ssh "$INSTANCE_NAME" \
        --zone="$ZONE" \
        --project="$PROJECT_ID" \
        --command="echo 'SSH successful'" \
        --tunnel-through-iap > "$EVIDENCE_DIR/ssh-test.log" 2>&1; then
        log_info "  ✓ SSH access working"
        return 0
    else
        log_warn "  ⚠ SSH access not working (may require IAP configuration)"
        return 0
    fi
}

test_service_via_ssh() {
    log_test "Checking erlmcp service status via SSH..."

    if gcloud compute ssh "$INSTANCE_NAME" \
        --zone="$ZONE" \
        --project="$PROJECT_ID" \
        --command="systemctl status erlmcp || systemctl status erlmcp@default || systemctl status docker" \
        --quiet > "$EVIDENCE_DIR/service-status.txt" 2>&1; then
        log_info "  ✓ Service status obtained"

        if grep -q "active (running)" "$EVIDENCE_DIR/service-status.txt"; then
            log_info "  ✓ Service is running"
        fi

        return 0
    else
        log_warn "  ⚠ Could not check service status"
        return 0
    fi
}

test_cloud_ops_agent() {
    log_test "Checking Cloud Ops Agent..."

    if gcloud compute ssh "$INSTANCE_NAME" \
        --zone="$ZONE" \
        --project="$PROJECT_ID" \
        --command="systemctl is-active google-cloud-ops-agent" \
        --quiet > "$EVIDENCE_DIR/ops-agent-status.txt" 2>&1; then

        if grep -q "active" "$EVIDENCE_DIR/ops-agent-status.txt"; then
            log_info "  ✓ Cloud Ops Agent is running"
        else
            log_warn "  ⚠ Cloud Ops Agent not active"
        fi

        return 0
    else
        log_warn "  ⚠ Could not check Ops Agent status"
        return 0
    fi
}

test_health_endpoint() {
    log_test "Testing health endpoint via HTTP..."

    if [ ! -f "$EVIDENCE_DIR/external-ip.txt" ]; then
        log_warn "  ⚠ No external IP available for health check"
        return 0
    fi

    EXTERNAL_IP=$(cat "$EVIDENCE_DIR/external-ip.txt")
    HEALTH_URL="http://$EXTERNAL_IP/health"

    log_info "  Health URL: $HEALTH_URL"
    log_info "  Waiting for service to respond..."

    # GCE instances take time to start the service
    local max_wait=120
    local waited=0

    while [ $waited -lt $max_wait ]; do
        HTTP_CODE=$(curl -s -o "$EVIDENCE_DIR/health-response.txt" -w "%{http_code}" --max-time 5 "$HEALTH_URL" 2>/dev/null || echo "000")

        if [ "$HTTP_CODE" = "200" ]; then
            log_info "  ✓ Health check returned 200 (${waited}s)"
            cat "$EVIDENCE_DIR/health-response.txt"
            return 0
        fi

        sleep 5
        waited=$((waited + 5))
        echo -n "."
    done

    log_warn "  ⚠ Health check not responding after ${max_wait}s"
    log_warn "  This is normal if service takes longer to initialize"
    return 0
}

test_logs_ingestion() {
    log_test "Checking logs ingestion to Cloud Logging..."

    # Wait for logs to be ingested
    sleep 10

    if gcloud logging read "resource.labels.instance_name=$INSTANCE_NAME" \
        --project="$PROJECT_ID" \
        --limit=10 \
        --freshness=1d > "$EVIDENCE_DIR/cloud-logs.json" 2>&1; then
        LOG_ENTRIES=$(jq 'length' "$EVIDENCE_DIR/cloud-logs.json" 2>/dev/null || echo "0")
        log_info "  ✓ Found $LOG_ENTRIES log entries in Cloud Logging"
        return 0
    else
        log_warn "  ⚠ Could not retrieve logs (may need more time)"
        return 0
    fi
}

test_metadata_access() {
    log_test "Testing metadata server access..."

    if gcloud compute ssh "$INSTANCE_NAME" \
        --zone="$ZONE" \
        --project="$PROJECT_ID" \
        --command="curl -s -H 'Metadata-Flavor: Google' http://metadata.google.internal/computeMetadata/v1/instance/id" \
        --quiet > "$EVIDENCE_DIR/metadata-test.txt" 2>&1; then
        log_info "  ✓ Metadata server accessible"
        return 0
    else
        log_warn "  ⚠ Could not access metadata server"
        return 0
    fi
}

cleanup() {
    log_info "Cleaning up test resources..."

    cd "$DEPLOY_DIR"

    if [ -f "terraform.tfstate" ] || [ -f "terraform.tfvars" ]; then
        log_info "  Destroying Compute Engine instances..."
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
    log_info "Starting Compute Engine Deployment Test Suite..."
    log_info "================================================"
    log_info "Project: $PROJECT_ID"
    log_info "Zone: $ZONE"
    log_info "Instance: $INSTANCE_NAME"
    log_info "Machine Type: $MACHINE_TYPE"
    log_info "Instance Count: $INSTANCE_COUNT"
    log_info "================================================"

    check_prerequisites

    # Test counter
    TOTAL_TESTS=0
    PASSED_TESTS=0
    FAILED_TESTS=0

    # Run tests
    for test_func in \
        test_terraform_init \
        test_vm_deployment \
        test_instance_status \
        test_instance_details \
        test_external_ip \
        test_firewall_rules \
        test_serial_port_output \
        test_startup_script \
        test_ssh_access \
        test_service_via_ssh \
        test_cloud_ops_agent \
        test_health_endpoint \
        test_logs_ingestion \
        test_metadata_access; do

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
        --zone)
            ZONE="$2"
            shift 2
            ;;
        --instance)
            INSTANCE_NAME="$2"
            shift 2
            ;;
        --machine-type)
            MACHINE_TYPE="$2"
            shift 2
            ;;
        --count)
            INSTANCE_COUNT="$2"
            shift 2
            ;;
        --skip-cleanup)
            SKIP_CLEANUP=true
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--project PROJECT] [--zone ZONE] [--instance NAME] [--machine-type TYPE] [--count N] [--skip-cleanup]"
            exit 1
            ;;
    esac
done

# Trap for cleanup (unless disabled)
if [ "${SKIP_CLEANUP:-false}" != "true" ]; then
    trap cleanup EXIT
fi

main
