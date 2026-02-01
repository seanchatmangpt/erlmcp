# Docker Guide: TAIEA Container Images

**Version**: 1.0.0 | **Updated**: 2026-01-26

TAIEA is containerized using multi-stage Docker builds for optimal image sizes and security. This guide covers building, running, and deploying TAIEA containers.

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Build Images](#build-images)
3. [Run Containers](#run-containers)
4. [Docker Compose](#docker-compose)
5. [Environment Configuration](#environment-configuration)
6. [Volume Mounting](#volume-mounting)
7. [Networking](#networking)
8. [Multi-Architecture Builds](#multi-architecture-builds)
9. [Registry Deployment](#registry-deployment)
10. [Troubleshooting](#troubleshooting)

---

## Quick Start

### Build Production Image

```bash
# Build with default version
docker build -t taiea:1.0.0 .

# Build with metadata
docker build \
  --build-arg BUILD_DATE=$(date -u +'%Y-%m-%dT%H:%M:%SZ') \
  --build-arg VCS_REF=$(git rev-parse --short HEAD) \
  --build-arg VERSION=1.0.0 \
  -t taiea:1.0.0 \
  -t taiea:latest \
  .
```

### Run Production Container

```bash
# Basic run
docker run -p 8080:8080 taiea:1.0.0

# With environment variables
docker run \
  -e TAIEA_ENV=production \
  -e LAGER_LEVEL=info \
  -p 8080:8080 \
  taiea:1.0.0

# With volume mounts for logs
docker run \
  -v taiea-logs:/var/log/taiea \
  -p 8080:8080 \
  taiea:1.0.0
```

### Run with Docker Compose

```bash
# Start TAIEA only
docker-compose up taiea

# Start TAIEA with development tools
docker-compose --profile dev up

# Start full stack (TAIEA + Postgres + Redis + Prometheus + Grafana)
docker-compose --profile postgres --profile redis --profile monitoring up
```

---

## Build Images

### Production Image (Multi-Stage)

The main `Dockerfile` uses a three-stage build process:

```
Stage 1 (Builder): erlang:26-alpine
  - Compile erlmcp workspace
  - Build TAIEA release

Stage 2 (Runtime): alpine:3.19
  - Copy TAIEA release only
  - Minimal dependencies
  - Non-root user
  - Production-optimized

Stage 3 (Debug): erlang:26-alpine
  - Full Erlang toolchain
  - Debugging tools (recon, observer)
  - Development libraries
```

### Build Commands

```bash
# Standard build
docker build -t taiea:latest .

# Build specific stage
docker build --target runtime -t taiea:1.0.0 .
docker build --target debug -t taiea:debug .

# Build with custom base image
docker build \
  --build-arg BASE_IMAGE=erlang:27-alpine \
  -t taiea:1.0.0 \
  .

# Build with build cache disabled (clean)
docker build --no-cache -t taiea:latest .

# Build with custom Dockerfile
docker build -f Dockerfile.dev -t taiea-dev:latest .
```

### Build Output Verification

```bash
# Check image size
docker images taiea

# Inspect image layers
docker history taiea:1.0.0

# Verify release was created
docker run --rm taiea:1.0.0 ls -la /opt/taiea
```

---

## Run Containers

### Basic Execution

```bash
# Foreground (interactive)
docker run --rm -p 8080:8080 taiea:1.0.0

# Background (detached)
docker run -d --name taiea-prod -p 8080:8080 taiea:1.0.0

# With custom Erlang cookie (for clustering)
docker run \
  -e ERLANG_COOKIE=my_secure_cookie \
  -p 8080:8080 \
  taiea:1.0.0
```

### Port Mapping

```bash
# Default ports
# 8080: TAIEA HTTP API
# 4369: Erlang port mapper daemon (epmd)
# 9100: Metrics/monitoring endpoint
# 9200: Debug console

# Full port mapping
docker run \
  -p 8080:8080 \
  -p 4369:4369 \
  -p 9100:9100 \
  -p 9200:9200 \
  taiea:1.0.0
```

### Resource Constraints

```bash
# Limit CPU and memory
docker run \
  --cpus="2.0" \
  --memory="1g" \
  --memory-swap="1g" \
  -p 8080:8080 \
  taiea:1.0.0

# Monitor resource usage
docker stats taiea-prod
```

### Health Checks

```bash
# Container includes built-in health check
docker run --name taiea-prod taiea:1.0.0

# Check health status
docker ps | grep taiea-prod
docker inspect --format='{{json .State.Health}}' taiea-prod | jq .

# Wait for healthy state
docker run --name taiea-prod taiea:1.0.0
sleep 40  # Wait for health check to pass
```

---

## Docker Compose

### Quick Start Profiles

```bash
# Core: TAIEA only
docker-compose up taiea

# Development: TAIEA + dev environment
docker-compose --profile dev up

# Data: TAIEA + PostgreSQL + Redis
docker-compose --profile postgres --profile redis up

# Monitoring: Full stack with Prometheus + Grafana
docker-compose --profile postgres --profile redis --profile monitoring up
```

### Service Management

```bash
# Start services
docker-compose up                           # Foreground
docker-compose up -d                        # Background

# Stop services
docker-compose down

# View logs
docker-compose logs -f taiea                # Follow logs
docker-compose logs --tail=100 taiea        # Last 100 lines

# Restart services
docker-compose restart taiea

# Rebuild image
docker-compose build --no-cache

# Health check verification
docker-compose ps                           # View service status
```

### Scale Services

```bash
# Scale development environment (if stateless)
docker-compose --profile dev up --scale taiea-dev=3
```

---

## Environment Configuration

### TAIEA Environment Variables

```bash
# Core Configuration
TAIEA_ENV=production                    # production, development, test
ERLANG_COOKIE=taiea_prod_cookie         # Erlang distributed cookie
NODE_NAME=taiea@localhost               # Erlang node name
LANG=C.UTF-8                            # Locale

# Logging
LAGER_LEVEL=info                        # debug, info, notice, warning, error, critical
LAGER_ERROR_LOGGER_HWND=true            # Error logger handling

# Performance
ERL_MAX_PORTS=65536                     # Maximum ports available
ERL_MAX_ETS_TABLES=20000                # Maximum ETS tables
ERL_FULLSWEEP_AFTER=0                   # Full garbage collection frequency

# Database (if enabled)
DB_HOST=postgres                        # PostgreSQL host
DB_PORT=5432                            # PostgreSQL port
DB_NAME=taiea                           # Database name
DB_USER=taiea                           # Database user
DB_PASSWORD=taiea_secure_password       # Database password

# Redis (if enabled)
REDIS_HOST=redis                        # Redis host
REDIS_PORT=6379                         # Redis port

# Ports
TAIEA_PORT=8080                         # HTTP API port
EPMD_PORT=4369                          # Erlang port mapper
METRICS_PORT=9100                       # Metrics endpoint
```

### Runtime Configuration

```bash
# Create .env file for docker-compose
cat > .env << EOF
TAIEA_ENV=production
ERLANG_COOKIE=my_secure_cookie
LAGER_LEVEL=info
TAIEA_PORT=8080
BUILD_DATE=$(date -u +'%Y-%m-%dT%H:%M:%SZ')
VCS_REF=$(git rev-parse --short HEAD)
VERSION=1.0.0
EOF

# Use with docker-compose
docker-compose --env-file .env up
```

---

## Volume Mounting

### Persistent Data

```bash
# Create named volumes
docker volume create taiea-logs
docker volume create taiea-data

# Mount volumes
docker run \
  -v taiea-logs:/var/log/taiea \
  -v taiea-data:/var/lib/taiea \
  -p 8080:8080 \
  taiea:1.0.0
```

### Configuration Override

```bash
# Mount custom configuration
docker run \
  -v $(pwd)/config/sys.config:/opt/taiea/etc/sys.config:ro \
  -p 8080:8080 \
  taiea:1.0.0
```

### Development Mounting

```bash
# Mount source code for development
docker run -it \
  -v $(pwd):/workspace \
  -p 8080:8080 \
  taiea:dev \
  /bin/sh
```

---

## Networking

### Docker Network

```bash
# Create custom network
docker network create taiea-net

# Run containers on network
docker run \
  --network taiea-net \
  --name taiea \
  -p 8080:8080 \
  taiea:1.0.0

# Service discovery by hostname
docker run \
  --network taiea-net \
  --name test-client \
  alpine:latest \
  ping taiea
```

### Port Exposure

```bash
# Expose port without publishing (for internal use)
docker run \
  --network taiea-net \
  taiea:1.0.0

# Publish to host
docker run \
  -p 127.0.0.1:8080:8080 \
  taiea:1.0.0

# Publish all exposed ports
docker run \
  -P \
  taiea:1.0.0
```

---

## Multi-Architecture Builds

### Build for Multiple Platforms

```bash
# Requires buildx (experimental feature)
docker buildx create --name multiarch

# Build for amd64 and arm64
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t taiea:1.0.0 \
  --push \
  .

# Build locally for testing
docker buildx build \
  --platform linux/amd64 \
  -t taiea:1.0.0-amd64 \
  --load \
  .

docker buildx build \
  --platform linux/arm64 \
  -t taiea:1.0.0-arm64 \
  .
```

### Verify Architecture

```bash
# Check image architecture
docker inspect taiea:1.0.0 | grep -i architecture

# Run on specific architecture
docker run --platform linux/amd64 taiea:1.0.0
docker run --platform linux/arm64 taiea:1.0.0
```

---

## Registry Deployment

### Push to Docker Hub

```bash
# Login to Docker Hub
docker login

# Tag image
docker tag taiea:1.0.0 username/taiea:1.0.0
docker tag taiea:1.0.0 username/taiea:latest

# Push
docker push username/taiea:1.0.0
docker push username/taiea:latest
```

### Push to Google Container Registry (GCR)

```bash
# Configure Docker auth
gcloud auth configure-docker gcr.io

# Tag image
docker tag taiea:1.0.0 gcr.io/PROJECT_ID/taiea:1.0.0
docker tag taiea:1.0.0 gcr.io/PROJECT_ID/taiea:latest

# Push
docker push gcr.io/PROJECT_ID/taiea:1.0.0
docker push gcr.io/PROJECT_ID/taiea:latest
```

### Push to AWS ECR

```bash
# Create ECR repository
aws ecr create-repository --repository-name taiea

# Login to ECR
aws ecr get-login-password --region us-east-1 | \
  docker login --username AWS --password-stdin \
  123456789.dkr.ecr.us-east-1.amazonaws.com

# Tag image
docker tag taiea:1.0.0 123456789.dkr.ecr.us-east-1.amazonaws.com/taiea:1.0.0

# Push
docker push 123456789.dkr.ecr.us-east-1.amazonaws.com/taiea:1.0.0
```

### Scan for Vulnerabilities

```bash
# Using Trivy (requires installation)
trivy image taiea:1.0.0

# Using Snyk
snyk container test taiea:1.0.0

# Using Docker Scout (built-in)
docker scout cves taiea:1.0.0
```

---

## Troubleshooting

### Common Issues

#### Container exits immediately

```bash
# Check logs
docker logs taiea

# Check exit code
docker inspect taiea --format='{{.State.ExitCode}}'

# Run with interactive shell
docker run -it --entrypoint /bin/sh taiea:1.0.0
```

#### Permission denied errors

```bash
# Check user inside container
docker run taiea:1.0.0 id

# Run with elevated privileges (not recommended for production)
docker run --user root taiea:1.0.0
```

#### Network connectivity issues

```bash
# Check container network
docker inspect taiea | grep -i ipaddress

# Test DNS resolution
docker run --rm --network taiea-net alpine nslookup taiea

# Test port connectivity
docker run --rm -it --network taiea-net nicolaka/netshoot bash
```

#### High memory usage

```bash
# Check memory statistics
docker stats taiea

# Check garbage collection settings
docker run taiea:1.0.0 printenv | grep ERL_

# Adjust Erlang memory settings
docker run \
  -e ERL_MAX_PORTS=32768 \
  -e ERL_FULLSWEEP_AFTER=100 \
  taiea:1.0.0
```

### Health Check Debugging

```bash
# Manual health check
docker exec taiea /opt/taiea/bin/taiea ping

# View health status
docker inspect --format='{{json .State.Health.Status}}' taiea

# Recent health checks
docker inspect --format='{{json .State.Health.Log}}' taiea | jq .
```

### Image Optimization

```bash
# Check image layers
docker history taiea:1.0.0

# Reduce image size by removing build artifacts
# (Dockerfile already uses multi-stage build)

# Check image content
docker run --rm taiea:1.0.0 find /opt/taiea -type f -size +1M
```

---

## CI/CD Integration

### GitHub Actions

```yaml
name: Build and Push TAIEA Docker Image

on:
  push:
    branches: [main]
    tags: ['v*']

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Set up Docker buildx
        uses: docker/setup-buildx-action@v2

      - name: Login to GCR
        uses: docker/login-action@v2
        with:
          registry: gcr.io
          username: _json_key
          password: ${{ secrets.GCP_SA_KEY }}

      - name: Extract metadata
        id: meta
        run: |
          echo "version=$(git describe --tags --always)" >> $GITHUB_OUTPUT

      - name: Build and push
        uses: docker/build-push-action@v4
        with:
          context: .
          platforms: linux/amd64,linux/arm64
          push: true
          tags: |
            gcr.io/${{ secrets.GCP_PROJECT }}/taiea:${{ steps.meta.outputs.version }}
            gcr.io/${{ secrets.GCP_PROJECT }}/taiea:latest
          build-args: |
            VERSION=${{ steps.meta.outputs.version }}
            VCS_REF=${{ github.sha }}
            BUILD_DATE=$(date -u +'%Y-%m-%dT%H:%M:%SZ')
```

---

## Performance Tuning

### Memory Optimization

```bash
# Set heap size
docker run \
  -e ERL_MAX_PORTS=65536 \
  -e ERL_MAX_ETS_TABLES=20000 \
  -m 1g \
  taiea:1.0.0
```

### Network Optimization

```bash
# Custom network mode
docker run --network host taiea:1.0.0   # Host network mode (fastest)
docker run --network bridge taiea:1.0.0 # Bridge network (default, isolation)
```

---

## Security Best Practices

1. **Run as non-root user** - Dockerfile uses `taiea` user
2. **Use specific image tags** - Avoid `latest` in production
3. **Scan for vulnerabilities** - Use Trivy, Docker Scout, or Snyk
4. **Limit resources** - Use `--memory` and `--cpus` flags
5. **Use secrets management** - Never hardcode passwords
6. **Keep base images updated** - Use latest Alpine/Erlang versions
7. **Review build context** - `.dockerignore` prevents sensitive files

---

## Additional Resources

- [Docker Documentation](https://docs.docker.com/)
- [Docker Compose Reference](https://docs.docker.com/compose/compose-file/)
- [Erlang in Docker](https://hub.docker.com/_/erlang)
- [Alpine Linux Documentation](https://alpinelinux.org/)
- [TAIEA GitHub Repository](https://github.com/seanchatmangpt/erlmcp)

---

**Questions or issues?** Open an issue on [GitHub](https://github.com/seanchatmangpt/erlmcp/issues)
