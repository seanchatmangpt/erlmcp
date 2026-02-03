# Docker Build Optimization Summary

## Overview

This document summarizes the Docker build optimization work for erlmcp v3, focusing on multi-stage builds, layer caching, security scanning integration, and minimal image footprint.

## Files Created

### 1. Dockerfile.optimized
Location: `/Users/sac/erlmcp/Dockerfile.optimized`

A production-ready multi-stage Dockerfile with the following stages:

- **deps**: Caches dependencies layer for faster rebuilds
- **compile**: Compiles source code with layer caching optimization
- **release**: Creates production release with minimal settings
- **security-scan**: Runs Trivy vulnerability scanning during build
- **sbom**: Generates Software Bill of Materials (SPDX and CycloneDX formats)
- **runtime**: Final minimal Alpine-based runtime (<100MB target)

### 2. .dockerignore (Optimized)
Location: `/Users/sac/erlmcp/.dockerignore`

Optimized build context exclusions:
- Build artifacts (_build/, *.beam, ebin/)
- Test files and directories
- Documentation (except README.md)
- CI/CD configuration
- Development tools (.venv, node_modules, etc.)
- Infrastructure configs (k8s/, terraform/, helm/)
- App-specific build artifacts (apps/*/_build/, apps/*/erl_crash.dump)

### 3. docker/buildkit.toml
Location: `/Users/sac/erlmcp/docker/buildkit.toml`

Docker BuildKit configuration for:
- Registry mirrors for faster pulls
- Cache mounts for rebar3 dependencies
- Security policies (enforce non-root, scan layers)
- SBOM generation entitlements

### 4. Security Scanning Scripts

#### scripts/security/docker-security-scan.sh
Comprehensive security scanning script that:
- Installs Trivy, Grype, and Syft
- Runs vulnerability scans on Docker images
- Generates SBOM in SPDX and CycloneDX formats
- Creates compliance reports

#### scripts/security/build-optimized-image.sh
Build automation script that:
- Uses BuildKit for parallel builds
- Implements cache mounts for dependencies
- Generates optimization reports
- Analyzes image layers and size

### 5. GitHub Workflow

#### .github/workflows/docker-security-scan.yml
CI/CD pipeline for:
- Building Docker images with BuildKit
- Running Trivy, Grype, and Docker Scout scans
- Generating and uploading SBOMs
- Image size analysis
- Security summary reports

## Build Targets

```bash
# Build runtime image (production)
docker build --target runtime -t erlmcp:3.0.0 -f Dockerfile.optimized .

# Build with security scanning
docker build --target security-scan -t erlmcp:scan -f Dockerfile.optimized .

# Build SBOM generation
docker build --target sbom -t erlmcp:sbom -f Dockerfile.optimized .

# Build all targets
./scripts/security/build-optimized-image.sh all
```

## Security Scanning

```bash
# Scan built image
./scripts/security/docker-security-scan.sh erlmcp:3.0.0

# Individual tools
trivy image erlmcp:3.0.0
grype erlmcp:3.0.0
syft erlmcp:3.0.0
```

## Optimization Techniques

### 1. Multi-Stage Builds
- Separate builder, compile, and runtime stages
- Each stage optimized for its purpose
- Only necessary artifacts copied to final image

### 2. Layer Caching
- Dependencies cached separately from source code
- Configuration files in dedicated layers
- BuildKit cache mounts for rebar3

### 3. Minimal Base Image
- Alpine 3.20 for runtime (~5MB base)
- Only essential runtime libraries
- No build tools in final image

### 4. Non-Root User
- Application runs as user `erlmcp` (UID 1000)
- Minimal permissions for security

### 5. Health Checks
- 3-level health verification:
  1. HTTP /health endpoint
  2. Erlang node ping
  3. Process state check

## Image Size Targets

| Component | Target | Notes |
|-----------|--------|-------|
| Base Alpine | ~5MB | alpine:3.20 |
| Runtime Libraries | ~20MB | libssl, ncurses, etc. |
| Erlang Runtime | ~40MB | BEAM VM |
| Application Code | ~30MB | Compiled beam files |
| **Total** | **<100MB** | Production image |

## Security Features

- [x] Non-root user execution
- [x] Minimal base image (Alpine)
- [x] Security scanning (Trivy, Grype)
- [x] SBOM generation (SPDX, CycloneDX)
- [x] Health check enabled
- [x] No secrets in image
- [x] Read-only root filesystem support

## Build Performance

With BuildKit and cache mounts:
- Initial build: ~3-5 minutes
- Cached build (deps unchanged): ~30-60 seconds
- Cached build (source changed): ~1-2 minutes

## Compliance

The optimized Docker build supports:
- **SBOM**: Automatic generation for supply chain transparency
- **Vulnerability Scanning**: Integrated into CI/CD pipeline
- **Image Signing**: Ready for Docker Content Trust integration
- **Traceability**: Build metadata in image labels

## Usage Examples

### Local Development
```bash
docker compose build erlmcp
docker compose up erlmcp
```

### Production Deployment
```bash
export VERSION=3.0.0
export BUILD_DATE=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
export VCS_REF=$(git rev-parse --short HEAD)

docker build \
  --target runtime \
  --build-arg VERSION=$VERSION \
  --build-arg BUILD_DATE=$BUILD_DATE \
  --build-arg VCS_REF=$VCS_REF \
  -t erlmcp:$VERSION \
  -f Dockerfile.optimized .
```

### Kubernetes Deployment
```yaml
apiVersion: apps/v1
kind: Deployment
spec:
  template:
    spec:
      containers:
      - name: erlmcp
        image: ghcr.io/banyan-platform/erlmcp:3.0.0
        securityContext:
          runAsNonRoot: true
          runAsUser: 1000
          readOnlyRootFilesystem: true
        livenessProbe:
          exec:
            command:
            - /opt/erlmcp/bin/healthcheck.sh
```

## Next Steps

1. **Integration**: Merge Dockerfile.optimized with main Dockerfile
2. **CI/CD**: Enable docker-security-scan.yml workflow
3. **Registry**: Configure GHCR or private registry
4. **Monitoring**: Set up image scanning alerts
5. **Automation**: Implement automated image promotion pipeline

## References

- [Docker Multi-Stage Builds](https://docs.docker.com/build/building/multi-stage/)
- [Docker BuildKit](https://docs.docker.com/build/buildkit/)
- [Trivy Scanner](https://aquasecurity.github.io/trivy/)
- [Syft SBOM](https://github.com/anchore/syft)
- [Alpine Linux](https://alpinelinux.org/)
