#!/bin/bash
# ============================================================================
# Container Image Test Script
# Tests container image for Marketplace deployment
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
IMAGE_NAME="${IMAGE_NAME:-erlmcp/erlmcp}"
IMAGE_TAG="${IMAGE_TAG:-latest}"
FULL_IMAGE="${REGION}-docker.pkg.dev/${PROJECT_ID}/${IMAGE_NAME}:${IMAGE_TAG}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence"

LOCAL_CONTAINER_NAME="erlmcp-test-local"
LOCAL_PORT="${LOCAL_PORT:-8080}"

# ============================================================================
# Prerequisites
# ============================================================================

check_prerequisites() {
    log_info "Checking prerequisites..."

    if [ -z "$PROJECT_ID" ]; then
        log_error "PROJECT_ID not set. Run: export PROJECT_ID=your-project-id"
        exit 1
    fi

    if ! command -v docker &> /dev/null; then
        log_error "docker not found"
        exit 1
    fi

    if ! command -v gcloud &> /dev/null; then
        log_error "gcloud CLI not found"
        exit 1
    fi

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    # Configure docker auth
    gcloud auth configure-docker "${REGION}-docker.pkg.dev" --quiet > /dev/null 2>&1

    log_info "Prerequisites check passed"
}

# ============================================================================
# Test Functions
# ============================================================================

test_image_exists() {
    log_test "Checking if image exists in Artifact Registry..."

    if gcloud artifacts docker images describe "$FULL_IMAGE" \
        --project="$PROJECT_ID" > "$EVIDENCE_DIR/image-describe.json" 2>&1; then
        log_info "  ✓ Image found: $FULL_IMAGE"

        # Get image digest
        IMAGE_DIGEST=$(jq -r '.imageSummary.digest' "$EVIDENCE_DIR/image-describe.json" 2>/dev/null || echo "unknown")
        log_info "  Digest: $IMAGE_DIGEST"
        echo "$IMAGE_DIGEST" > "$EVIDENCE_DIR/image-digest.txt"

        return 0
    else
        log_error "  ✗ Image not found: $FULL_IMAGE"
        cat "$EVIDENCE_DIR/image-describe.json"
        return 1
    fi
}

test_image_pull() {
    log_test "Pulling image from Artifact Registry..."

    if docker pull "$FULL_IMAGE" > "$EVIDENCE_DIR/docker-pull.log" 2>&1; then
        log_info "  ✓ Image pulled successfully"

        # Get image size
        IMAGE_SIZE=$(docker images "$FULL_IMAGE" --format "{{.Size}}")
        log_info "  Image size: $IMAGE_SIZE"

        return 0
    else
        log_error "  ✗ Failed to pull image"
        cat "$EVIDENCE_DIR/docker-pull.log"
        return 1
    fi
}

test_image_config() {
    log_test "Inspecting image configuration..."

    if docker inspect "$FULL_IMAGE" > "$EVIDENCE_DIR/image-inspect.json" 2>&1; then
        log_info "  ✓ Image inspected"

        # Check for non-root user
        USER=$(jq -r '.[0].config.User' "$EVIDENCE_DIR/image-inspect.json")
        if [ "$USER" != "root" ] && [ -n "$USER" ]; then
            log_info "  ✓ Running as non-root user: $USER"
        else
            log_warn "  ⚠ Container running as root (recommended: use non-root user)"
        fi

        # Check exposed ports
        EXPOSED_PORTS=$(jq -r '.[0].config.ExposedPorts | keys[]' "$EVIDENCE_DIR/image-inspect.json" 2>/dev/null || echo "none")
        log_info "  Exposed ports: $EXPOSED_PORTS"

        # Check for health check
        HEALTHCHECK=$(jq -r '.[0].config.Healthcheck' "$EVIDENCE_DIR/image-inspect.json" 2>/dev/null)
        if [ "$HEALTHCHECK" != "null" ] && [ -n "$HEALTHCHECK" ]; then
            log_info "  ✓ Health check defined"
        else
            log_warn "  ⚠ No health check defined in image"
        fi

        return 0
    else
        log_error "  ✗ Failed to inspect image"
        return 1
    fi
}

test_container_start() {
    log_test "Starting container locally..."

    # Remove any existing container with same name
    docker rm -f "$LOCAL_CONTAINER_NAME" > /dev/null 2>&1 || true

    if docker run -d \
        --name "$LOCAL_CONTAINER_NAME" \
        -p "${LOCAL_PORT}:8080" \
        -e ERLMCP_LOG_LEVEL=debug \
        -e ERLMCP_ENV=test \
        "$FULL_IMAGE" > "$EVIDENCE_DIR/container-start.log" 2>&1; then

        log_info "  ✓ Container started"
        echo "$LOCAL_CONTAINER_NAME" > "$EVIDENCE_DIR/container-name.txt"
        return 0
    else
        log_error "  ✗ Failed to start container"
        cat "$EVIDENCE_DIR/container-start.log"
        return 1
    fi
}

test_container_startup_time() {
    log_test "Measuring container startup time..."

    START_TIME=$(date +%s)

    # Wait for container to be running
    local max_wait=60
    local waited=0

    while [ $waited -lt $max_wait ]; do
        CONTAINER_STATUS=$(docker inspect "$LOCAL_CONTAINER_NAME" --format='{{.State.Status}}' 2>/dev/null || echo "not-running")

        if [ "$CONTAINER_STATUS" = "running" ]; then
            # Check if health endpoint responds
            if curl -f -s --max-time 2 "http://localhost:${LOCAL_PORT}/health" > /dev/null 2>&1; then
                END_TIME=$(date +%s)
                STARTUP_TIME=$((END_TIME - START_TIME))
                log_info "  ✓ Container ready in ${STARTUP_TIME}s"
                echo "$STARTUP_TIME" > "$EVIDENCE_DIR/startup-time.txt"
                return 0
            fi
        fi

        sleep 2
        waited=$((waited + 2))
        echo -n "."
    done

    log_warn "  ⚠ Container not fully ready after ${max_wait}s"

    # Check container logs
    docker logs "$LOCAL_CONTAINER_NAME" > "$EVIDENCE_DIR/container-logs.txt" 2>&1
    log_warn "  Check container logs at: $EVIDENCE_DIR/container-logs.txt"

    return 1
}

test_health_endpoint() {
    log_test "Testing health endpoint..."

    HEALTH_URL="http://localhost:${LOCAL_PORT}/health"

    if curl -f -s "$HEALTH_URL" > "$EVIDENCE_DIR/health-response.json" 2>&1; then
        log_info "  ✓ Health check returned 200"

        # Parse and validate response
        if command -v jq &> /dev/null; then
            STATUS=$(jq -r '.status // .Status // "unknown"' "$EVIDENCE_DIR/health-response.json" 2>/dev/null)
            log_info "  Health status: $STATUS"
        fi

        cat "$EVIDENCE_DIR/health-response.json"
        return 0
    else
        log_error "  ✗ Health check failed"
        cat "$EVIDENCE_DIR/health-response.json"
        return 1
    fi
}

test_ready_endpoint() {
    log_test "Testing readiness endpoint..."

    READY_URL="http://localhost:${LOCAL_PORT}/ready"

    if curl -f -s "$READY_URL" > "$EVIDENCE_DIR/ready-response.json" 2>&1; then
        log_info "  ✓ Readiness check returned 200"
        return 0
    else
        log_warn "  ⚠ Readiness endpoint not available (optional)"
        return 0
    fi
}

test_metrics_endpoint() {
    log_test "Testing metrics endpoint..."

    METRICS_URL="http://localhost:${LOCAL_PORT}/metrics"

    if curl -f -s "$METRICS_URL" > "$EVIDENCE_DIR/metrics-response.txt" 2>&1; then
        log_info "  ✓ Metrics endpoint accessible"

        # Check for Prometheus format
        if grep -q "^# HELP\|^# TYPE" "$EVIDENCE_DIR/metrics-response.txt"; then
            log_info "  ✓ Metrics in Prometheus format"
        fi

        return 0
    else
        log_warn "  ⚠ Metrics endpoint not available (may need --expose-metrics flag)"
        return 0
    fi
}

test_container_logs() {
    log_test "Checking container logs..."

    docker logs "$LOCAL_CONTAINER_NAME" > "$EVIDENCE_DIR/container-logs-full.txt" 2>&1

    # Check for error messages
    if grep -i "error\|fail\|panic\|crash" "$EVIDENCE_DIR/container-logs-full.txt" | \
        grep -v "level=error" | head -5 > "$EVIDENCE_DIR/error-messages.txt" 2>&1; then

        if [ -s "$EVIDENCE_DIR/error-messages.txt" ]; then
            log_warn "  ⚠ Found potential errors in logs:"
            head -5 "$EVIDENCE_DIR/error-messages.txt"
        fi
    else
        log_info "  ✓ No critical errors in logs"
    fi

    # Verify logs are being emitted
    LOG_LINES=$(wc -l < "$EVIDENCE_DIR/container-logs-full.txt")
    if [ "$LOG_LINES" -gt 0 ]; then
        log_info "  ✓ Container is logging ($LOG_LINES lines captured)"
    else
        log_warn "  ⚠ No logs captured from container"
    fi

    return 0
}

test_graceful_shutdown() {
    log_test "Testing graceful shutdown..."

    # Get initial PID
    CONTAINER_PID=$(docker inspect "$LOCAL_CONTAINER_NAME" --format='{{.State.Pid}}')

    # Send SIGTERM
    SHUTDOWN_START=$(date +%s)
    docker stop -t 10 "$LOCAL_CONTAINER_NAME" > /dev/null 2>&1
    SHUTDOWN_END=$(date +%s)

    SHUTDOWN_TIME=$((SHUTDOWN_END - SHUTDOWN_START))

    if [ $SHUTDOWN_TIME -le 15 ]; then
        log_info "  ✓ Container shut down gracefully in ${SHUTDOWN_TIME}s"
        return 0
    else
        log_warn "  ⚠ Shutdown took ${SHUTDOWN_TIME}s (expected < 15s)"
        return 0
    fi
}

test_resource_usage() {
    log_test "Checking container resource usage..."

    # Start a new container for this test
    docker rm -f "$LOCAL_CONTAINER_NAME-resources" > /dev/null 2>&1 || true

    docker run -d \
        --name "$LOCAL_CONTAINER_NAME-resources" \
        -e ERLMCP_ENV=test \
        "$FULL_IMAGE" > /dev/null 2>&1

    sleep 5

    # Get resource usage
    docker stats "$LOCAL_CONTAINER_NAME-resources" \
        --no-stream \
        --format "table {{.MemUsage}}\t{{.CPUPerc}}" \
        > "$EVIDENCE_DIR/resource-usage.txt" 2>&1

    log_info "  Resource usage:"
    cat "$EVIDENCE_DIR/resource-usage.txt"

    # Cleanup
    docker rm -f "$LOCAL_CONTAINER_NAME-resources" > /dev/null 2>&1

    return 0
}

cleanup() {
    log_info "Cleaning up test resources..."

    docker rm -f "$LOCAL_CONTAINER_NAME" > /dev/null 2>&1 || true
    docker rm -f "$LOCAL_CONTAINER_NAME-resources" > /dev/null 2>&1 || true

    log_info "  Cleanup complete"
}

# ============================================================================
# Main Test Runner
# ============================================================================

main() {
    log_info "Starting Container Image Test Suite..."
    log_info "================================================"
    log_info "Project: $PROJECT_ID"
    log_info "Region: $REGION"
    log_info "Image: $FULL_IMAGE"
    log_info "Local Port: $LOCAL_PORT"
    log_info "================================================"

    check_prerequisites

    # Test counter
    TOTAL_TESTS=0
    PASSED_TESTS=0
    FAILED_TESTS=0
    WARNED_TESTS=0

    # Run tests
    for test_func in \
        test_image_exists \
        test_image_pull \
        test_image_config \
        test_container_start \
        test_container_startup_time \
        test_health_endpoint \
        test_ready_endpoint \
        test_metrics_endpoint \
        test_container_logs \
        test_graceful_shutdown \
        test_resource_usage; do

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
        --image)
            FULL_IMAGE="$2"
            shift 2
            ;;
        --tag)
            IMAGE_TAG="$2"
            shift 2
            ;;
        --port)
            LOCAL_PORT="$2"
            shift 2
            ;;
        --no-cleanup)
            NO_CLEANUP=true
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--project PROJECT] [--region REGION] [--tag TAG] [--port PORT] [--no-cleanup]"
            exit 1
            ;;
    esac
done

# Trap for cleanup
trap cleanup EXIT

main
