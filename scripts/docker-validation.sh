#!/bin/bash
# Docker Image Validation Script
# Validates erlmcp Docker image build and runtime
# Usage: ./scripts/docker-validation.sh [image_tag]

set -e

IMAGE_TAG="${1:-erlmcp:0.7.0}"
CONTAINER_NAME="erlmcp-test"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() { echo -e "${BLUE}ℹ️  $1${NC}"; }
log_success() { echo -e "${GREEN}✓ $1${NC}"; }
log_warning() { echo -e "${YELLOW}⚠️  $1${NC}"; }
log_error() { echo -e "${RED}✗ $1${NC}"; }

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."

    # Check Docker
    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed"
        exit 1
    fi
    log_success "Docker found: $(docker --version)"

    # Check Docker daemon
    if ! docker ps &> /dev/null; then
        log_error "Docker daemon is not running"
        exit 1
    fi
    log_success "Docker daemon is running"
}

# Build image
build_image() {
    log_info "Building Docker image: $IMAGE_TAG"

    if docker build \
        --build-arg BUILD_DATE="$(date -u +'%Y-%m-%dT%H:%M:%SZ')" \
        --build-arg VCS_REF="$(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')" \
        --build-arg VERSION="${IMAGE_TAG##*:}" \
        -t "$IMAGE_TAG" \
        . > /tmp/docker-build.log 2>&1; then
        log_success "Image built successfully: $IMAGE_TAG"
    else
        log_error "Failed to build image"
        cat /tmp/docker-build.log
        exit 1
    fi
}

# Check image size
check_image_size() {
    log_info "Checking image size..."

    local size_bytes=$(docker inspect "$IMAGE_TAG" --format='{{.Size}}')
    local size_mb=$((size_bytes / 1048576))

    log_info "Image size: ${size_mb}MB"

    if [ "$size_mb" -lt 200 ]; then
        log_success "Image size is acceptable (<200MB)"
    elif [ "$size_mb" -lt 300 ]; then
        log_warning "Image size is somewhat large (${size_mb}MB)"
    else
        log_error "Image size is too large (${size_mb}MB, should be <200MB)"
    fi
}

# Check image layers
check_image_layers() {
    log_info "Checking image layers..."

    docker history "$IMAGE_TAG" --no-trunc

    local layer_count=$(docker history "$IMAGE_TAG" --quiet | wc -l)
    log_success "Image has $layer_count layers"
}

# Inspect image metadata
inspect_image() {
    log_info "Inspecting image metadata..."

    docker inspect "$IMAGE_TAG" | jq '{
        Architecture: .[0].Architecture,
        Os: .[0].Os,
        Size: .[0].Size,
        Created: .[0].Created,
        Entrypoint: .[0].Config.Entrypoint,
        Cmd: .[0].Config.Cmd,
        ExposedPorts: .[0].Config.ExposedPorts,
        Env: .[0].Config.Env | length,
        Labels: .[0].Config.Labels
    }'
}

# Check security (non-root user)
check_security() {
    log_info "Checking security configuration..."

    local user=$(docker inspect "$IMAGE_TAG" --format='{{.Config.User}}')

    if [ -z "$user" ]; then
        log_warning "No explicit user set (will run as root)"
    elif [ "$user" = "root" ] || [ "$user" = "0" ]; then
        log_error "Image runs as root (security issue)"
    else
        log_success "Image runs as non-root user: $user"
    fi
}

# Check healthcheck
check_healthcheck() {
    log_info "Checking health check configuration..."

    local healthcheck=$(docker inspect "$IMAGE_TAG" --format='{{.Config.Healthcheck}}')

    if [ "$healthcheck" != "<nil>" ]; then
        log_success "Health check configured"
    else
        log_warning "No health check configured"
    fi
}

# Run container
run_container() {
    log_info "Running container: $CONTAINER_NAME"

    # Clean up any existing container
    docker rm -f "$CONTAINER_NAME" 2>/dev/null || true

    if docker run -d \
        --name "$CONTAINER_NAME" \
        -p 8080:8080 \
        -p 9090:9090 \
        -e ERLMCP_ENV=test \
        "$IMAGE_TAG" > /tmp/container-id.log 2>&1; then
        log_success "Container started: $CONTAINER_NAME"
        sleep 5
    else
        log_error "Failed to start container"
        cat /tmp/container-id.log
        return 1
    fi
}

# Check container health
check_container_health() {
    log_info "Checking container health..."

    local max_attempts=30
    local attempt=0

    while [ $attempt -lt $max_attempts ]; do
        local health=$(docker inspect "$CONTAINER_NAME" --format='{{.State.Health.Status}}' 2>/dev/null || echo "")

        if [ "$health" = "healthy" ]; then
            log_success "Container is healthy"
            return 0
        elif [ "$health" = "unhealthy" ]; then
            log_error "Container health check failed"
            return 1
        fi

        log_info "Waiting for container to be ready... (attempt $((attempt+1))/$max_attempts)"
        sleep 1
        ((attempt++))
    done

    log_warning "Container health check timeout"
    return 1
}

# Test health endpoint
test_health_endpoint() {
    log_info "Testing health endpoint..."

    if docker exec "$CONTAINER_NAME" \
        sh -c "curl -s http://localhost:8080/health" &> /dev/null; then
        log_success "Health endpoint responds"
    else
        log_warning "Health endpoint not responding (service might not be initialized)"
    fi
}

# Check logs
check_logs() {
    log_info "Checking container logs..."

    local log_output=$(docker logs "$CONTAINER_NAME" 2>&1 | head -20)

    if echo "$log_output" | grep -q -i "error\|crash\|failed"; then
        log_warning "Found potential issues in logs:"
        echo "$log_output" | grep -i "error\|crash\|failed" || true
    else
        log_success "No obvious errors in logs"
    fi

    log_info "First 10 log lines:"
    echo "$log_output"
}

# Test port accessibility
test_ports() {
    log_info "Testing port accessibility..."

    # Test HTTP port
    if docker exec "$CONTAINER_NAME" \
        sh -c "curl -s -f http://localhost:8080/health" &> /dev/null; then
        log_success "Port 8080 (HTTP) is accessible"
    else
        log_warning "Port 8080 (HTTP) not responding"
    fi

    # Test metrics port
    if docker exec "$CONTAINER_NAME" \
        sh -c "curl -s -f http://localhost:9090/metrics" &> /dev/null; then
        log_success "Port 9090 (metrics) is accessible"
    else
        log_warning "Port 9090 (metrics) not responding"
    fi
}

# Test volume mounts
test_volumes() {
    log_info "Testing volume mounts..."

    # Clean up any existing volumes
    docker volume rm erlmcp-test-data 2>/dev/null || true
    docker volume rm erlmcp-test-logs 2>/dev/null || true

    # Create test volumes
    docker volume create erlmcp-test-data
    docker volume create erlmcp-test-logs

    # Run container with volumes
    docker run -d \
        --name erlmcp-volume-test \
        -v erlmcp-test-data:/var/lib/erlmcp \
        -v erlmcp-test-logs:/var/log/erlmcp \
        "$IMAGE_TAG" /bin/sh -c "sleep 10" > /dev/null 2>&1

    # Check volumes are mounted
    if docker inspect erlmcp-volume-test | jq '.[0].Mounts' | grep -q "erlmcp-test-data"; then
        log_success "Volumes can be mounted successfully"
    else
        log_warning "Volume mounting failed"
    fi

    # Clean up
    docker rm -f erlmcp-volume-test
    docker volume rm erlmcp-test-data erlmcp-test-logs 2>/dev/null || true
}

# Security scanning (if Trivy is available)
security_scan() {
    log_info "Attempting security scan..."

    if command -v trivy &> /dev/null; then
        log_info "Running Trivy security scan..."
        if trivy image "$IMAGE_TAG"; then
            log_success "Security scan passed"
        else
            log_warning "Security scan found issues"
        fi
    else
        log_info "Trivy not installed, skipping security scan"
        log_info "Install Trivy to perform security scans: https://github.com/aquasecurity/trivy"
    fi
}

# Cleanup
cleanup() {
    log_info "Cleaning up test containers..."

    docker rm -f "$CONTAINER_NAME" 2>/dev/null || true
    log_success "Cleanup complete"
}

# Generate report
generate_report() {
    log_info "Generating validation report..."

    local report_file="/tmp/erlmcp-docker-validation-$(date +%s).txt"

    cat > "$report_file" << EOF
erlmcp Docker Image Validation Report
====================================
Generated: $(date)
Image: $IMAGE_TAG

Image Information:
$(docker inspect "$IMAGE_TAG" --format='Architecture: {{.Architecture}}
OS: {{.Os}}
Size: {{.Size}} bytes
Created: {{.Created}}')

Quality Checks:
- Image size: $(docker inspect "$IMAGE_TAG" --format='{{.Size}}' | awk '{print $1/1048576 " MB"}')
- Non-root user: $(docker inspect "$IMAGE_TAG" --format='{{.Config.User}}')
- Health check: $(docker inspect "$IMAGE_TAG" --format='{{.Config.Healthcheck}}')

Tests Performed:
- Build: $([ -f /tmp/docker-build.log ] && grep -q "successfully" /tmp/docker-build.log && echo "PASS" || echo "FAIL")
- Container startup: PASS
- Health endpoint: TESTED
- Port accessibility: TESTED
- Volume mounting: TESTED

EOF

    log_success "Report saved to: $report_file"
}

# Main execution
main() {
    log_info "Starting Docker image validation for: $IMAGE_TAG"
    echo ""

    check_prerequisites
    echo ""

    build_image
    echo ""

    check_image_size
    echo ""

    check_image_layers
    echo ""

    inspect_image
    echo ""

    check_security
    echo ""

    check_healthcheck
    echo ""

    run_container
    if [ $? -ne 0 ]; then
        log_error "Failed to start container, skipping remaining tests"
        cleanup
        exit 1
    fi
    echo ""

    check_container_health
    echo ""

    test_health_endpoint
    echo ""

    check_logs
    echo ""

    test_ports
    echo ""

    test_volumes
    echo ""

    security_scan
    echo ""

    generate_report
    echo ""

    cleanup
    echo ""

    log_success "Docker validation complete!"
}

# Handle errors
trap cleanup EXIT

# Run main function
main
