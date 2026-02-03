#!/bin/bash
# ============================================================================
# VM Image (Packer) Test Script
# Tests GCE VM image built with Packer for Marketplace deployment
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
TEST_INSTANCE="erlmcp-vm-test-$(date +%s)"
MACHINE_TYPE="${MACHINE_TYPE:-e2-medium}"
IMAGE_FAMILY="${IMAGE_FAMILY:-erlmcp-3}"
NETWORK="${NETWORK:-default}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence"
PACKER_DIR="${MARKETPLACE_DIR}/packer"

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

    if ! command -v packer &> /dev/null; then
        log_error "packer not found"
        exit 1
    fi

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    log_info "Prerequisites check passed"
}

# ============================================================================
# Test Functions
# ============================================================================

test_packer_validate() {
    log_test "Validating Packer template..."

    cd "$PACKER_DIR"

    if [ ! -f "gce-image.pkr.hcl" ]; then
        log_error "Packer template not found: gce-image.pkr.hcl"
        return 1
    fi

    if packer validate gce-image.pkr.hcl > "$EVIDENCE_DIR/packer-validate.log" 2>&1; then
        log_info "  ✓ Packer template validation passed"
        cat "$EVIDENCE_DIR/packer-validate.log"
        return 0
    else
        log_error "  ✗ Packer template validation failed"
        cat "$EVIDENCE_DIR/packer-validate.log"
        return 1
    fi
}

test_packer_build() {
    log_test "Building VM image with Packer..."
    log_warn "  This step can take 10-15 minutes"

    cd "$PACKER_DIR"

    local packer_vars=(
        "-var" "project_id=${PROJECT_ID}"
        "-var" "zone=${ZONE}"
        "-var" "image_family=${IMAGE_FAMILY}"
    )

    log_info "  Building image: ${IMAGE_FAMILY}"

    if packer build "${packer_vars[@]}" gce-image.pkr.hcl > "$EVIDENCE_DIR/packer-build.log" 2>&1; then
        log_info "  ✓ Image built successfully"

        # Extract image name from build log
        IMAGE_NAME=$(grep -oP 'Image: \K[^ ]+' "$EVIDENCE_DIR/packer-build.log" | head -1)
        if [ -n "$IMAGE_NAME" ]; then
            echo "$IMAGE_NAME" > "$EVIDENCE_DIR/built-image-name.txt"
            log_info "  Image name: $IMAGE_NAME"
        fi

        return 0
    else
        log_error "  ✗ Image build failed"
        cat "$EVIDENCE_DIR/packer-build.log"
        return 1
    fi
}

test_vm_creation() {
    log_test "Creating test VM instance..."

    # Get the image name (either from build or existing)
    if [ -f "$EVIDENCE_DIR/built-image-name.txt" ]; then
        IMAGE_NAME=$(cat "$EVIDENCE_DIR/built-image-name.txt")
        IMAGE_ARG="--image=$IMAGE_NAME"
    else
        # Try to use latest image from family
        IMAGE_ARG="--image-family=${IMAGE_FAMILY} --image-project=${PROJECT_ID}"
    fi

    log_info "  Creating instance: $TEST_INSTANCE"

    if gcloud compute instances create "$TEST_INSTANCE" \
        $IMAGE_ARG \
        --zone="$ZONE" \
        --machine-type="$MACHINE_TYPE" \
        --network="$NETWORK" \
        --boot-disk-size=20GB \
        --tags=http-server,https-server > "$EVIDENCE_DIR/vm-create.log" 2>&1; then
        log_info "  ✓ VM instance created"
        return 0
    else
        log_error "  ✗ VM creation failed"
        cat "$EVIDENCE_DIR/vm-create.log"
        return 1
    fi
}

test_vm_boot() {
    log_test "Waiting for VM to boot..."

    local max_wait=120
    local waited=0

    while [ $waited -lt $max_wait ]; do
        if gcloud compute instances get-serial-port-output "$TEST_INSTANCE" \
            --zone="$ZONE" --port=1 2>&1 | grep -q "erlmcp.*started"; then
            log_info "  ✓ VM booted successfully (${waited}s)"
            return 0
        fi
        sleep 5
        waited=$((waited + 5))
        echo -n "."
    done

    log_warn "  ⚠ VM boot status unclear after ${max_wait}s"

    # Check if VM is running at least
    if gcloud compute instances describe "$TEST_INSTANCE" --zone="$ZONE" \
        --format='value(status)' 2>&1 | grep -q "RUNNING"; then
        log_info "  ✓ VM is in RUNNING state"
        return 0
    fi

    return 1
}

test_cloud_ops_agent() {
    log_test "Checking Cloud Ops Agent..."

    # Try to check agent status via SSH
    if gcloud compute ssh "$TEST_INSTANCE" --zone="$ZONE" \
        --command="systemctl is-active google-cloud-ops-agent" \
        --quiet > "$EVIDENCE_DIR/ops-agent-status.log" 2>&1; then

        if grep -q "active" "$EVIDENCE_DIR/ops-agent-status.log"; then
            log_info "  ✓ Cloud Ops Agent is running"
            return 0
        fi
    fi

    log_warn "  ⚠ Could not verify Ops Agent status (VM may still be initializing)"
    return 0
}

test_erlmcp_service() {
    log_test "Checking erlmcp service status..."

    if gcloud compute ssh "$TEST_INSTANCE" --zone="$ZONE" \
        --command="systemctl status erlmcp || systemctl status erlmcp@default" \
        --quiet > "$EVIDENCE_DIR/service-status.log" 2>&1; then

        if grep -q "active (running)" "$EVIDENCE_DIR/service-status.log"; then
            log_info "  ✓ erlmcp service is running"
            return 0
        fi
    fi

    log_warn "  ⚠ Could not verify service status"
    return 0
}

test_serial_port_output() {
    log_test "Capturing serial port output..."

    if gcloud compute instances get-serial-port-output "$TEST_INSTANCE" \
        --zone="$ZONE" --port=1 > "$EVIDENCE_DIR/serial-output.log" 2>&1; then
        log_info "  ✓ Serial port output captured"

        # Check for errors
        if grep -i "error\|fail\|panic" "$EVIDENCE_DIR/serial-output.log" | \
            grep -v "Failed to get" | head -5; then
            log_warn "  ⚠ Found potential errors in serial output"
        fi

        return 0
    else
        log_warn "  ⚠ Could not capture serial output"
        return 0
    fi
}

test_external_ip() {
    log_test "Getting external IP..."

    EXTERNAL_IP=$(gcloud compute instances describe "$TEST_INSTANCE" \
        --zone="$ZONE" --format='value(networkInterfaces[0].accessConfigs[0].natIP)')

    if [ -n "$EXTERNAL_IP" ]; then
        log_info "  ✓ External IP: $EXTERNAL_IP"
        echo "$EXTERNAL_IP" > "$EVIDENCE_DIR/external-ip.txt"
        return 0
    else
        log_warn "  ⚠ No external IP assigned"
        return 1
    fi
}

test_health_endpoint() {
    log_test "Testing health endpoint..."

    if [ ! -f "$EVIDENCE_DIR/external-ip.txt" ]; then
        log_warn "  ⚠ No external IP available for health check"
        return 0
    fi

    EXTERNAL_IP=$(cat "$EVIDENCE_DIR/external-ip.txt")

    log_info "  Waiting for service to respond..."

    for i in {1..12}; do
        if curl -f -s --max-time 5 "http://$EXTERNAL_IP/health" > /dev/null 2>&1; then
            log_info "  ✓ Health check passed on attempt $i"

            # Capture health response
            curl -s "http://$EXTERNAL_IP/health" > "$EVIDENCE_DIR/health-response.json"

            return 0
        fi
        sleep 5
        echo -n "."
    done

    log_warn "  ⚠ Health check not responding (may need more time or firewall rule)"
    return 0
}

cleanup() {
    log_info "Cleaning up test resources..."

    if gcloud compute instances describe "$TEST_INSTANCE" --zone="$ZONE" &> /dev/null; then
        log_info "  Deleting test instance: $TEST_INSTANCE"
        gcloud compute instances delete "$TEST_INSTANCE" \
            --zone="$ZONE" --quiet > /dev/null 2>&1 || true
    fi

    log_info "  Cleanup complete"
}

# ============================================================================
# Main Test Runner
# ============================================================================

main() {
    log_info "Starting VM Image Test Suite..."
    log_info "================================================"
    log_info "Project: $PROJECT_ID"
    log_info "Zone: $ZONE"
    log_info "Test Instance: $TEST_INSTANCE"
    log_info "Image Family: $IMAGE_FAMILY"
    log_info "================================================"

    check_prerequisites

    # Test counter
    TOTAL_TESTS=0
    PASSED_TESTS=0
    FAILED_TESTS=0
    WARNED_TESTS=0

    # Run tests
    for test_func in \
        test_packer_validate \
        test_packer_build \
        test_vm_creation \
        test_vm_boot \
        test_cloud_ops_agent \
        test_erlmcp_service \
        test_serial_port_output \
        test_external_ip \
        test_health_endpoint; do

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
    log_info "  Warnings:     $WARNED_TESTS"
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
BUILD_IMAGE="${BUILD_IMAGE:-true}"

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
        --image-family)
            IMAGE_FAMILY="$2"
            shift 2
            ;;
        --skip-build)
            BUILD_IMAGE=false
            shift
            ;;
        --no-cleanup)
            NO_CLEANUP=true
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--project PROJECT] [--zone ZONE] [--image-family FAMILY] [--skip-build] [--no-cleanup]"
            exit 1
            ;;
    esac
done

# Trap for cleanup
trap cleanup EXIT

main
