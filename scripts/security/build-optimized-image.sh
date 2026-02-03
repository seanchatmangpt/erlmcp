#!/bin/bash
# ==============================================================================
# erlmcp v3 - Optimized Docker Build Script
# ==============================================================================
# Builds Docker images with:
#   - BuildKit for parallel builds and caching
#   - Layer caching for faster rebuilds
#   - Multi-stage builds for minimal image size
#   - Security scanning integration
#   - SBOM generation
#
# Usage: ./scripts/security/build-optimized-image.sh [TARGET]
# Targets: runtime (default), security-scan, sbom, all
# ==============================================================================

set -euo pipefail

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Configuration
TARGET="${1:-runtime}"
VERSION="${VERSION:-3.0.0}"
OTP_VERSION="${OTP_VERSION:-28.3.1}"
ALPINE_VERSION="${ALPINE_VERSION:-3.20}"
BUILD_DATE="${BUILD_DATE:-$(date -u +"%Y-%m-%dT%H:%M:%SZ")}"
VCS_REF="${VCS_REF:-$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")}"
IMAGE_NAME="${IMAGE_NAME:-erlmcp}"
REGISTRY="${REGISTRY:-localhost:5000}"
DOCKERFILE="${DOCKERFILE:-Dockerfile.optimized}"

# BuildKit cache directories
CACHE_DIR="${CACHE_DIR:-/tmp/erlmcp-buildkit-cache}"
mkdir -p "${CACHE_DIR}"

echo -e "${BLUE}=== erlmcp v3 Optimized Docker Build ===${NC}"
echo "Target: ${TARGET}"
echo "Version: ${VERSION}"
echo "OTP Version: ${OTP_VERSION}"
echo "Alpine Version: ${ALPINE_VERSION}"
echo "Dockerfile: ${DOCKERFILE}"
echo "Build Date: ${BUILD_DATE}"
echo "Git SHA: ${VCS_REF}"
echo ""

# ==============================================================================
# Enable BuildKit
# ==============================================================================
export DOCKER_BUILDKIT=1
export BUILDKIT_PROGRESS=plain

# ==============================================================================
# Build Arguments
# ==============================================================================
BUILD_ARGS="--build-arg OTP_VERSION=${OTP_VERSION}"
BUILD_ARGS="${BUILD_ARGS} --build-arg ALPINE_VERSION=${ALPINE_VERSION}"
BUILD_ARGS="${BUILD_ARGS} --build-arg BUILD_DATE=${BUILD_DATE}"
BUILD_ARGS="${BUILD_ARGS} --build-arg VCS_REF=${VCS_REF}"
BUILD_ARGS="${BUILD_ARGS} --build-arg VERSION=${VERSION}"

# ==============================================================================
# Cache Configuration
# ==============================================================================
CACHE_MOUNTS="--mount type=cache,target=/root/.cache/rebar3,id=rebar3-${VERSION}"
CACHE_MOUNTS="${CACHE_MOUNTS} --mount type=cache,target=/build/.cache,id=hex-${VERSION}"

# ==============================================================================
# Build Functions
# ==============================================================================
build_runtime() {
    echo -e "${BLUE}Building runtime image...${NC}"

    docker build \
        --target runtime \
        ${BUILD_ARGS} \
        --cache-from "${IMAGE_NAME}:buildcache" \
        --cache-to "${IMAGE_NAME}:buildcache" \
        --tag "${IMAGE_NAME}:${VERSION}" \
        --tag "${IMAGE_NAME}:latest" \
        -f "${DOCKERFILE}" \
        . || {
            echo -e "${RED}Build failed${NC}"
            return 1
        }

    # Display image size
    SIZE=$(docker images "${IMAGE_NAME}:${VERSION}" --format "{{.Size}}")
    echo -e "${GREEN}Runtime image built successfully${NC}"
    echo "Image size: ${SIZE}"
    echo ""

    # Verify image
    docker inspect "${IMAGE_NAME}:${VERSION}" --format='{{.Id}}'
}

build_security_scan() {
    echo -e "${BLUE}Building security-scan image...${NC}"

    docker build \
        --target security-scan \
        ${BUILD_ARGS} \
        --tag "${IMAGE_NAME}:scan" \
        -f "${DOCKERFILE}" \
        . || {
            echo -e "${RED}Security scan build failed${NC}"
            return 1
        }

    echo -e "${GREEN}Security scan image built${NC}"
    echo ""
}

build_sbom() {
    echo -e "${BLUE}Building SBOM image...${NC}"

    docker build \
        --target sbom \
        ${BUILD_ARGS} \
        --tag "${IMAGE_NAME}:sbom" \
        -f "${DOCKERFILE}" \
        . || {
            echo -e "${RED}SBOM build failed${NC}"
            return 1
        }

    # Extract SBOM files
    echo -e "${BLUE}Extracting SBOM files...${NC}"
    mkdir -p ./reports/sbom
    docker run --rm \
        -v "$(pwd)/reports/sbom:/output" \
        "${IMAGE_NAME}:sbom" \
        cp /erlmcp.*.json /output/ 2>/dev/null || true

    echo -e "${GREEN}SBOM generated${NC}"
    ls -lh ./reports/sbom/ || echo "No SBOM files extracted"
    echo ""
}

build_all() {
    echo -e "${BLUE}Building all targets...${NC}"
    echo ""

    # Build runtime (includes all dependencies)
    build_runtime

    # Build security scan target
    build_security_scan

    # Build SBOM target
    build_sbom

    # Run security scan on runtime image
    if command -v trivy &> /dev/null; then
        echo -e "${BLUE}Running security scan on runtime image...${NC}"
        trivy image --severity HIGH,CRITICAL --no-progress "${IMAGE_NAME}:${VERSION}" || true
    fi

    echo -e "${GREEN}=== All builds complete ===${NC}"
    echo ""
    echo "Images built:"
    docker images "${IMAGE_NAME}" --format "table {{.Repository}}\t{{.Tag}}\t{{.Size}}"
}

# ==============================================================================
# Layer Analysis
# ==============================================================================
analyze_layers() {
    echo -e "${BLUE}Analyzing image layers...${NC}"
    docker history "${IMAGE_NAME}:${VERSION}" --no-trunc --human || true
    echo ""
}

# ==============================================================================
# Optimization Report
# ==============================================================================
generate_optimization_report() {
    echo -e "${BLUE}Generating optimization report...${NC}"

    mkdir -p ./reports/optimization
    cat > ./reports/optimization/report.md <<EOF
# Docker Build Optimization Report

**Generated:** $(date -u +"%Y-%m-%dT%H:%M:%SZ")
**Image:** ${IMAGE_NAME}:${VERSION}
**Dockerfile:** ${DOCKERFILE}

## Image Information

\`\`\`
$(docker inspect "${IMAGE_NAME}:${VERSION}" 2>/dev/null | jq '.[0] | {
    Id: .Id,
    Created: .Created,
    Size: .Size,
    Architecture: .Architecture,
    Os: .Os
}')
\`\`\`

## Layer Breakdown

\`\`\`
$(docker history "${IMAGE_NAME}:${VERSION}" --human --no-trunc=false)
\`\`\`

## Optimization Techniques Applied

1. **Multi-stage Builds**: Separate builder, compile, and runtime stages
2. **Layer Caching**: Dependencies cached separately from source code
3. **Alpine Base**: Minimal base image for reduced attack surface
4. **Copy Only Essentials**: Only essential source files copied to builder
5. **BuildKit**: Parallel builds with cache mounts
6. **Non-root User**: Application runs as non-root user

## Metrics

- **Base Image:** Alpine ${ALPINE_VERSION}
- **Erlang/OTP:** ${OTP_VERSION}
- **Build Date:** ${BUILD_DATE}
- **Git SHA:** ${VCS_REF}

## Security Features

- [x] Non-root user execution
- [x] Read-only root filesystem support
- [x] Minimal base image
- [x] Security scanning integration
- [x] SBOM generation
- [x] Health check enabled

## Recommendations

- Use BuildKit cache mounts for CI/CD
- Pin dependency versions in rebar.config
- Regular security scans with Trivy/Grype
- Monitor image size for bloat

---

*Generated by erlmcp v3 build optimization pipeline*
EOF

    echo -e "${GREEN}Optimization report generated${NC}"
    cat ./reports/optimization/report.md
}

# ==============================================================================
# Main Execution
# ==============================================================================
main() {
    # Check if Dockerfile exists
    if [ ! -f "${DOCKERFILE}" ]; then
        echo -e "${RED}Error: Dockerfile not found: ${DOCKERFILE}${NC}"
        exit 1
    fi

    # Execute build based on target
    case "${TARGET}" in
        runtime)
            build_runtime
            analyze_layers
            generate_optimization_report
            ;;
        security-scan|scan)
            build_security_scan
            ;;
        sbom)
            build_sbom
            ;;
        all)
            build_all
            analyze_layers
            generate_optimization_report
            ;;
        *)
            echo -e "${RED}Unknown target: ${TARGET}${NC}"
            echo "Valid targets: runtime, security-scan, sbom, all"
            exit 1
            ;;
    esac

    echo -e "${GREEN}=== Build Complete ===${NC}"
    echo ""
    echo "To run the image:"
    echo "  docker run -p 8080:8080 ${IMAGE_NAME}:${VERSION}"
    echo ""
    echo "To run security scan:"
    echo "  ./scripts/security/docker-security-scan.sh ${IMAGE_NAME}:${VERSION}"
}

# Run main function
main "$@"
