# erlmcp v3 Docker Deployment Guide

**DOCKER-ONLY CONSTITUTION: All execution MUST run via Docker. Host execution forbidden.**

## Table of Contents

1. [Overview](#overview)
2. [Docker-Only Constitution](#docker-only-constitution)
3. [Multi-Stage Builds](#multi-stage-builds)
4. [Docker Compose Setup](#docker-compose-setup)
5. [Service Definitions](#service-definitions)
6. [Volume Management](#volume-management)
7. [Networking Configuration](#networking-configuration)
8. [Security Best Practices](#security-best-practices)
9. [Production Deployment Patterns](#production-deployment-patterns)
10. [Quality Gates](#quality-gates)
11. [Cluster Deployment](#cluster-deployment)
12. [Monitoring and Observability](#monitoring-and-observability)
13. [Troubleshooting](#troubleshooting)

---

## Overview

erlmcp v3 is a production-grade Erlang/OTP implementation of the Model Context Protocol (MCP) SDK. This guide provides comprehensive Docker deployment patterns for development, testing, and production environments.

**Key Features:**
- **OTP 28.3.1**: Built on latest Erlang/OTP with deterministic compilation
- **Multi-Stage Builds**: Optimized image sizes (<150MB runtime)
- **Quality Gates**: Docker services for compile, test, check, bench, cluster
- **EPMD-less Clustering**: Production-ready distributed Erlang without EPMD
- **Zero-Trust Security**: Least privilege, secrets externalization, audit trails
- **Enterprise Observability**: OpenTelemetry, Prometheus, Grafana integration

---

## Docker-Only Constitution

**CRITICAL: All execution = Docker. Host execution forbidden. No exceptions.**

### Allowed Execution Forms

```bash
# ✅ ALLOWED - Docker execution
docker compose run --rm erlmcp-build
docker compose up -d
docker build -t erlmcp:3.0.0 .
docker stack deploy -c docker-compose.prod.yml erlmcp
kubectl apply --dry-run=server -f k8s/

# ❌ FORBIDDEN - Host execution
rebar3 compile           # NO
make test                # NO
erl -eval "..."          # NO
dialyzer                 # NO
ct_run                   # NO
```

### Rewrite Calculus

Transform all host commands to Docker equivalents:

```bash
# Host command → Docker command
rebar3 compile        → docker compose run --rm erlmcp-build make compile
rebar3 eunit          → docker compose run --rm erlmcp-unit make eunit
rebar3 ct             → docker compose run --rm erlmcp-ct make ct
rebar3 dialyzer       → docker compose run --rm erlmcp-check make dialyzer
rebar3 xref           → docker compose run --rm erlmcp-check make xref
make benchmark        → docker compose run --rm erlmcp-bench make benchmark
```

### Gate Mapping

Quality gates map to Docker services:

| Gate | Docker Service | Purpose |
|------|----------------|---------|
| `compile` | `erlmcp-build` | Compilation with OTP 28 |
| `eunit` | `erlmcp-unit` | Unit tests with EUnit |
| `ct` | `erlmcp-ct` | Integration tests with Common Test |
| `dialyzer` | `erlmcp-check` | Static analysis |
| `xref` | `erlmcp-check` | Cross-reference analysis |
| `coverage` | `erlmcp-check` | Coverage ≥80% validation |
| `bench` | `erlmcp-bench` | Performance benchmarks |
| `cluster` | `erlmcp-node*` | Distributed Erlang cluster tests |

### Quality Invariants

- **errors=0**: Zero compilation errors
- **failures=0**: Zero test failures
- **coverage≥0.8**: Minimum 80% code coverage
- **regression<0.1**: Performance regression <10%

---

## Multi-Stage Builds

erlmcp uses optimized multi-stage Docker builds for minimal attack surface and fast deployments.

### Stage 1: Builder

Full build environment with OTP 28.3.1, rebar3, and all dependencies.

```dockerfile
# ============================================================================
# STAGE 1: BUILDER - Full build environment with OTP 28.3.1
# ============================================================================
FROM erlang:28.3.1-alpine AS builder

LABEL stage="builder"

# Build metadata
ARG BUILD_DATE
ARG VCS_REF
ARG VERSION=3.0.0

# Deterministic builds
ENV ERL_COMPILER_OPTIONS=deterministic \
    REBAR_NO_USER_CONFIG=true

WORKDIR /build

# Install build dependencies
RUN apk add --no-cache \
    ca-certificates \
    git \
    make \
    g++ \
    gcc \
    musl-dev \
    openssl-dev \
    ncurses-dev \
    zlib-dev

# Copy project structure
COPY rebar.config rebar.lock ./
COPY apps/ ./apps/
COPY config/ ./config/
COPY include/ ./include/

# Build production release
RUN rebar3 as prod get-deps compile
RUN rebar3 as prod release

# Verify release
RUN ls -la _build/prod/rel/erlmcp && \
    test -x _build/prod/rel/erlmcp/bin/erlmcp
```

**Build Command:**

```bash
docker build --target builder -t erlmcp:3.0.0-builder .
```

### Stage 2: Runtime

Minimal runtime image (<150MB) with only production dependencies.

```dockerfile
# ============================================================================
# STAGE 2: RUNTIME - Minimal production image
# ============================================================================
FROM alpine:3.20

LABEL stage="runtime"

# Install runtime dependencies only
RUN apk add --no-cache \
    ca-certificates \
    libssl3 \
    libcrypto3 \
    ncurses-libs \
    libstdc++ \
    bash \
    curl \
    tzdata

# Create application directories
RUN mkdir -p /opt/erlmcp \
    /var/log/erlmcp \
    /var/lib/erlmcp \
    /etc/erlmcp

WORKDIR /opt/erlmcp

# Copy release from builder
COPY --from=builder /build/_build/prod/rel/erlmcp /opt/erlmcp

# Create non-root user (uid 1000)
RUN addgroup -S -g 1000 erlmcp && \
    adduser -S -u 1000 -G erlmcp -h /opt/erlmcp -s /bin/sh erlmcp && \
    chown -R erlmcp:erlmcp /opt/erlmcp /var/log/erlmcp /var/lib/erlmcp

USER erlmcp

# Exposed ports
EXPOSE 8080 9100 9090

HEALTHCHECK --interval=15s --timeout=10s --start-period=45s --retries=3 \
    CMD /opt/erlmcp/bin/healthcheck.sh

ENTRYPOINT ["/opt/erlmcp/bin/start-cluster.sh"]
```

**Build Command:**

```bash
docker build -t erlmcp:3.0.0 .
```

### Stage 3: Debug

Debug image with additional tools for troubleshooting.

```dockerfile
# ============================================================================
# STAGE 3: DEBUG - Debug image with tools
# ============================================================================
FROM erlang:28.3.1-alpine AS debug

# Install debug tools
RUN apk add --no-cache \
    vim nano htop procps lsof \
    netcat-openbsd strace tcpdump \
    bind-tools tini

COPY --from=builder /build/_build/prod/rel/erlmcp /opt/erlmcp

USER erlmcp

ENTRYPOINT ["/sbin/tini", "--"]
CMD ["/opt/erlmcp/bin/erlmcp", "foreground"]
```

**Build Command:**

```bash
docker build --target debug -t erlmcp:3.0.0-debug .
```

---

## Docker Compose Setup

### Development Environment

Quick start for local development:

```bash
# Start development environment
docker compose --profile dev up -d

# View logs
docker compose --profile dev logs -f erlmcp-dev

# Stop environment
docker compose --profile dev down
```

**Configuration:**

```yaml
services:
  erlmcp-dev:
    build:
      context: .
      dockerfile: Dockerfile
      target: debug
    container_name: erlmcp-dev
    hostname: erlmcp-dev
    networks:
      - erlmcp-network
    ports:
      - "8081:8080"
      - "9110:9100"
    environment:
      - ERLMCP_ENV=development
      - ERLMCP_LOG_LEVEL=debug
    volumes:
      - .:/workspace
      - erlang-cache:/root/.cache
    working_dir: /workspace
    command: /bin/sh
    stdin_open: true
    tty: true
    profiles:
      - dev
```

### Production Environment

Secure production deployment with Docker secrets:

```bash
# Create secrets directory
mkdir -p docker-secrets

# Generate secrets
openssl rand -base64 32 > docker-secrets/erlang.cookie
openssl rand -base64 32 > docker-secrets/postgres-password
openssl rand -base64 32 > docker-secrets/redis-password

# Start production stack
docker compose -f docker-compose.prod.yml up -d

# Check health
docker compose -f docker-compose.prod.yml ps
docker compose -f docker-compose.prod.yml logs erlmcp
```

---

## Service Definitions

### Main Application Service

```yaml
services:
  erlmcp:
    build:
      context: .
      dockerfile: Dockerfile
      args:
        BUILD_DATE: ${BUILD_DATE}
        VCS_REF: ${VCS_REF}
        VERSION: ${VERSION:-3.0.0}
    image: erlmcp:3.0.0
    container_name: erlmcp
    hostname: erlmcp
    restart: unless-stopped

    # Port mappings
    ports:
      - "${ERLMCP_PORT:-8080}:8080"     # HTTP API
      - "${ERL_DIST_PORT:-9100}:9100"   # EPMD-less distribution
      - "${METRICS_PORT:-9100}:9100"    # Prometheus metrics
      - "${HEALTH_PORT:-9090}:9090"     # Health checks

    # Environment configuration
    environment:
      - ERLMCP_ENV=${ERLMCP_ENV:-production}
      - ERLMCP_NODE_NAME=${ERLMCP_NODE_NAME:-erlmcp@erlmcp}
      - ERLANG_COOKIE=${ERLANG_COOKIE}
      - ERL_AFLAGS=-proto_dist inet_tls
      - ERL_DIST_PORT=${ERL_DIST_PORT:-9100}
      - ERLANG_DISTRIBUTION_PORT_RANGE=9100-9200
      - ERL_MAX_PORTS=65536
      - ERL_MAX_ETS_TABLES=50000

    # Dependencies
    depends_on:
      postgres:
        condition: service_healthy
      redis:
        condition: service_healthy

    # Networks
    networks:
      - erlmcp-network

    # Security
    security_opt:
      - no-new-privileges:true
      - apparmor:docker-default

    # Resource limits
    deploy:
      resources:
        limits:
          cpus: '2.0'
          memory: 4G
          pids: 4096
        reservations:
          cpus: '1.0'
          memory: 2G

    # Health check (3-level verification)
    healthcheck:
      test: [CMD, /opt/erlmcp/bin/healthcheck.sh]
      interval: 15s
      timeout: 10s
      retries: 3
      start_period: 45s

    # Logging
    logging:
      driver: "json-file"
      options:
        max-size: "10m"
        max-file: "3"
```

### Database Service (PostgreSQL)

```yaml
  postgres:
    image: postgres:16-alpine
    container_name: erlmcp-postgres
    hostname: postgres
    restart: unless-stopped

    ports:
      - "${DB_PORT:-5432}:5432"

    environment:
      - POSTGRES_DB=${DB_NAME:-erlmcp}
      - POSTGRES_USER=${DB_USER:-erlmcp}
      - POSTGRES_PASSWORD_FILE=/run/secrets/postgres-password
      - POSTGRES_INITDB_ARGS=--encoding=UTF8 --locale=C
      - PGDATA=/var/lib/postgresql/data/pgdata

    secrets:
      - postgres-password

    volumes:
      - postgres-data:/var/lib/postgresql/data
      - ./config/docker/postgres-init:/docker-entrypoint-initdb.d:ro

    networks:
      - erlmcp-network

    healthcheck:
      test: [CMD-SHELL, "pg_isready -U ${DB_USER:-erlmcp}"]
      interval: 10s
      timeout: 5s
      retries: 5
      start_period: 30s
```

### Cache Service (Redis)

```yaml
  redis:
    image: redis:7-alpine
    container_name: erlmcp-redis
    hostname: redis
    restart: unless-stopped

    ports:
      - "${REDIS_PORT:-6379}:6379"

    secrets:
      - redis-password

    volumes:
      - redis-data:/data

    networks:
      - erlmcp-network

    command: >
      redis-server
      --requirepass $(cat /run/secrets/redis-password)
      --appendonly yes
      --maxmemory 2gb
      --maxmemory-policy allkeys-lru

    healthcheck:
      test: [CMD, redis-cli, ping]
      interval: 10s
      timeout: 5s
      retries: 5
```

---

## Volume Management

### Volume Strategy

**Principles:**
- **Explicit volumes only**: No implicit host mounts
- **Read-only mounts**: Source code and config are read-only in production
- **Persistent data**: Named volumes for data that must survive container restarts
- **Ephemeral data**: tmpfs for temporary data

### Volume Definitions

```yaml
volumes:
  # Application data (persistent)
  erlmcp-data:
    driver: local
    driver_opts:
      type: none
      o: bind
      device: /data/erlmcp

  # Application logs (persistent, monitored)
  erlmcp-logs:
    driver: local
    driver_opts:
      type: none
      o: bind
      device: /var/log/erlmcp

  # Build cache (persistent, speeds up rebuilds)
  erlmcp-build-cache:
    driver: local

  # Hex package cache (persistent)
  hex-cache:
    driver: local

  # Dialyzer PLT cache (persistent)
  erlmcp-dialyzer-plt:
    driver: local

  # Database data (persistent, backed up)
  postgres-data:
    driver: local
    driver_opts:
      type: none
      o: bind
      device: /data/postgres

  # Redis data (persistent)
  redis-data:
    driver: local

  # Prometheus metrics (persistent)
  prometheus-data:
    driver: local

  # Grafana dashboards (persistent)
  grafana-data:
    driver: local
```

### Volume Mounts

```yaml
services:
  erlmcp:
    volumes:
      # Configuration (read-only for security)
      - ./config/sys.config:/opt/erlmcp/etc/sys.config:ro
      - ./vm.args:/opt/erlmcp/releases/3.0.0/vm.args:ro

      # Data (read-write, persistent)
      - erlmcp-data:/var/lib/erlmcp:rw

      # Logs (read-write, monitored)
      - erlmcp-logs:/var/log/erlmcp:rw

      # Secrets (read-only)
      - ./certs:/opt/erlmcp/certs:ro
```

### Backup Strategy

```bash
# Backup volumes
docker run --rm \
  -v erlmcp-data:/data:ro \
  -v $(pwd)/backups:/backup \
  alpine tar czf /backup/erlmcp-data-$(date +%Y%m%d).tar.gz -C /data .

# Restore volumes
docker run --rm \
  -v erlmcp-data:/data:rw \
  -v $(pwd)/backups:/backup:ro \
  alpine tar xzf /backup/erlmcp-data-20260206.tar.gz -C /data

# Database backup
docker compose exec postgres pg_dump -U erlmcp erlmcp > backup_$(date +%Y%m%d).sql

# Database restore
docker compose exec -T postgres psql -U erlmcp erlmcp < backup_20260206.sql
```

---

## Networking Configuration

### Network Architecture

```
┌─────────────────────────────────────────────────────┐
│                 External Network                     │
│                  (Internet)                          │
└────────────────────┬────────────────────────────────┘
                     │
            ┌────────▼────────┐
            │   Load Balancer │
            │    (Traefik)    │
            └────────┬────────┘
                     │
        ┌────────────┼────────────┐
        │     erlmcp-network      │
        │    (Bridge/Overlay)     │
        │                         │
        │  ┌──────────────────┐  │
        │  │  erlmcp (nodes)  │  │
        │  └─────────┬────────┘  │
        │            │            │
        │  ┌─────────▼────────┐  │
        │  │  postgres/redis  │  │
        │  └──────────────────┘  │
        └─────────────────────────┘
                     │
        ┌────────────▼────────────┐
        │  monitoring-network     │
        │  (Isolated Overlay)     │
        │                         │
        │  ┌──────────────────┐  │
        │  │ Prometheus       │  │
        │  │ Grafana          │  │
        │  │ OpenTelemetry    │  │
        │  └──────────────────┘  │
        └─────────────────────────┘
```

### Network Definitions

```yaml
networks:
  # Application network (bridge for single-host, overlay for swarm)
  erlmcp-network:
    driver: bridge
    ipam:
      config:
        - subnet: 172.25.0.0/16
          gateway: 172.25.0.1
    driver_opts:
      com.docker.network.bridge.name: erlmcp0
      com.docker.network.bridge.enable_icc: "true"
      com.docker.network.bridge.enable_ip_masquerade: "true"

  # Monitoring network (isolated)
  monitoring-network:
    driver: bridge
    internal: false
    ipam:
      config:
        - subnet: 172.30.0.0/24
```

### Overlay Network (Docker Swarm)

```yaml
networks:
  erlmcp-overlay:
    driver: overlay
    attachable: true
    ipam:
      driver: default
      config:
        - subnet: 172.20.0.0/24
          gateway: 172.20.0.1
    driver_opts:
      encrypted: "true"
```

### Port Mapping Strategy

| Port | Service | Protocol | Purpose |
|------|---------|----------|---------|
| 8080 | erlmcp | HTTP | JSON-RPC API |
| 8443 | erlmcp | HTTPS | TLS JSON-RPC API |
| 9090 | erlmcp | HTTP | Health checks |
| 9100 | erlmcp | TCP | Prometheus metrics + EPMD-less distribution |
| 9100-9200 | erlmcp | TCP | Distributed Erlang port range |
| 5432 | postgres | TCP | PostgreSQL |
| 6379 | redis | TCP | Redis |
| 3000 | grafana | HTTP | Dashboards |
| 9090 | prometheus | HTTP | Metrics |
| 4317 | otel-collector | gRPC | OTLP telemetry |

### EPMD-less Clustering

erlmcp uses EPMD-less clustering for production deployments:

```yaml
environment:
  # EPMD-less configuration
  - ERL_AFLAGS=-proto_dist inet_tls
  - ERL_DIST_PORT=9100
  - ERLANG_DISTRIBUTION_PORT_RANGE=9100-9200
```

**Benefits:**
- No EPMD daemon required
- Fixed port range for firewall rules
- TLS-encrypted distribution
- Docker overlay network compatible

---

## Security Best Practices

### Zero-Trust Security Model

**Principles:**
- Least privilege containers
- No privileged containers
- Explicit volumes only
- Secrets externalization
- Auditability mandatory
- Defense in depth

### Docker Secrets

**NEVER hardcode secrets in docker-compose.yml or Dockerfiles.**

```bash
# Create secrets
mkdir -p docker-secrets
openssl rand -base64 32 > docker-secrets/erlang.cookie
openssl rand -base64 32 > docker-secrets/postgres-password
openssl rand -base64 32 > docker-secrets/redis-password
chmod 600 docker-secrets/*
```

**Usage in docker-compose.yml:**

```yaml
secrets:
  erlang-cookie:
    file: ./docker-secrets/erlang.cookie
  postgres-password:
    file: ./docker-secrets/postgres-password
  redis-password:
    file: ./docker-secrets/redis-password

services:
  erlmcp:
    secrets:
      - erlang-cookie
      - postgres-password
      - redis-password
    environment:
      - ERLANG_COOKIE_FILE=/run/secrets/erlang-cookie
```

### Container Hardening

```yaml
services:
  erlmcp:
    # Run as non-root user
    user: erlmcp

    # Security options
    security_opt:
      - no-new-privileges:true
      - apparmor:docker-default
      - seccomp:default

    # Read-only root filesystem
    read_only: true

    # Temporary filesystems
    tmpfs:
      - /tmp:mode=1777,size=100m
      - /var/tmp:mode=1777,size=100m

    # Drop all capabilities, add only required
    cap_drop:
      - ALL
    cap_add:
      - NET_BIND_SERVICE
      - CHOWN
      - SETUID
      - SETGID

    # Resource limits (prevent DoS)
    deploy:
      resources:
        limits:
          cpus: '2.0'
          memory: 4G
          pids: 4096
```

### Network Security

```yaml
services:
  erlmcp:
    networks:
      - erlmcp-network
    # No host network mode
    # No privileged mode
    # Isolated from host

networks:
  erlmcp-network:
    driver: bridge
    # Enable network isolation
    internal: false
    # Enable encryption for overlay
    driver_opts:
      encrypted: "true"
```

### Image Security

```bash
# Scan images for vulnerabilities
docker scan erlmcp:3.0.0

# Use specific tags (never :latest in production)
docker build -t erlmcp:3.0.0 .

# Sign images
docker trust sign erlmcp:3.0.0

# Verify signatures
docker trust inspect --pretty erlmcp:3.0.0
```

### TLS Configuration

```yaml
services:
  erlmcp:
    environment:
      - ERLMCP_TLS_ENABLED=true
      - ERLMCP_TLS_CERT_FILE=/run/secrets/tls-cert
      - ERLMCP_TLS_KEY_FILE=/run/secrets/tls-key
      - ERLMCP_TLS_CA_FILE=/run/secrets/tls-ca
    secrets:
      - tls-cert
      - tls-key
      - tls-ca
```

---

## Production Deployment Patterns

### Blue-Green Deployment

```bash
# Deploy green (new version)
docker compose -f docker-compose.prod.yml up -d erlmcp-green

# Wait for health check
docker compose -f docker-compose.prod.yml ps erlmcp-green

# Switch traffic to green
docker compose -f docker-compose.prod.yml exec traefik \
  update-config --service erlmcp --target erlmcp-green

# Stop blue (old version)
docker compose -f docker-compose.prod.yml stop erlmcp-blue
```

### Rolling Updates (Docker Swarm)

```yaml
services:
  erlmcp:
    deploy:
      replicas: 5
      update_config:
        parallelism: 1          # Update 1 at a time
        delay: 30s              # Wait 30s between updates
        failure_action: rollback
        monitor: 120s
        max_failure_ratio: 0.2
      rollback_config:
        parallelism: 1
        delay: 30s
```

**Deploy:**

```bash
docker stack deploy -c docker-compose.swarm.yml erlmcp
```

### Canary Deployment

```yaml
services:
  erlmcp-stable:
    image: erlmcp:3.0.0
    deploy:
      replicas: 9
      labels:
        - "traefik.http.services.erlmcp.loadbalancer.server.weight=90"

  erlmcp-canary:
    image: erlmcp:3.1.0
    deploy:
      replicas: 1
      labels:
        - "traefik.http.services.erlmcp.loadbalancer.server.weight=10"
```

### High Availability

```yaml
services:
  erlmcp:
    deploy:
      mode: replicated
      replicas: 5
      placement:
        constraints:
          - node.role == worker
        preferences:
          - spread: node.labels.zone
      restart_policy:
        condition: on-failure
        delay: 5s
        max_attempts: 5
        window: 120s
```

---

## Quality Gates

### Compilation Gate

```bash
# Run compilation in Docker
docker compose run --rm erlmcp-build make compile

# Expected output: All apps compiled successfully
# Exit code: 0 (success)
```

**Service definition:**

```yaml
erlmcp-build:
  build:
    context: .
    dockerfile: Dockerfile
    target: builder
  image: erlmcp:3.0.0-build
  container_name: erlmcp-build
  environment:
    - ERL_COMPILER_OPTIONS=deterministic
    - REBAR_NO_USER_CONFIG=true
  volumes:
    - .:/workspace:cached
    - erlmcp-build-cache:/workspace/_build
  working_dir: /workspace
  command: ["make", "compile"]
```

### Unit Test Gate

```bash
# Run EUnit tests in Docker
docker compose run --rm erlmcp-unit make eunit

# Expected output: All tests passed
# Exit code: 0 (success)
```

### Integration Test Gate

```bash
# Run Common Test in Docker
docker compose run --rm erlmcp-ct make ct

# Expected output: All suites passed
# Exit code: 0 (success)
```

### Static Analysis Gate

```bash
# Run Dialyzer in Docker
docker compose run --rm erlmcp-check make dialyzer

# Run Xref in Docker
docker compose run --rm erlmcp-check make xref

# Expected output: No warnings
# Exit code: 0 (success)
```

### Coverage Gate

```bash
# Run coverage analysis in Docker
docker compose run --rm erlmcp-check make coverage

# Expected output: Coverage ≥ 80%
# Exit code: 0 (success)
```

### Performance Gate

```bash
# Run benchmarks in Docker
docker compose run --rm erlmcp-bench make benchmark

# Expected output: Regression < 10%
# Exit code: 0 (success)
```

---

## Cluster Deployment

### EPMD-less Clustering

erlmcp uses EPMD-less clustering for Docker/Kubernetes compatibility.

**Architecture:**

```
┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│ erlmcp-node1 │────▶│ erlmcp-node2 │────▶│ erlmcp-node3 │
│   :9100      │     │   :9100      │     │   :9100      │
└──────────────┘     └──────────────┘     └──────────────┘
       │                    │                    │
       └────────────────────┴────────────────────┘
              Distributed Erlang (TLS)
                Port Range: 9100-9200
```

**Configuration:**

```yaml
services:
  erlmcp-node1:
    image: erlmcp:3.0.0
    hostname: erlmcp-node1
    environment:
      - ERLMCP_NODE_NAME=erlmcp@erlmcp-node1
      - ERLANG_COOKIE=${ERLANG_COOKIE}
      - ERL_AFLAGS=-proto_dist inet_tls
      - ERL_DIST_PORT=9100
      - ERLANG_DISTRIBUTION_PORT_RANGE=9100-9200
    networks:
      - erlmcp-cluster

  erlmcp-node2:
    image: erlmcp:3.0.0
    hostname: erlmcp-node2
    environment:
      - ERLMCP_NODE_NAME=erlmcp@erlmcp-node2
      - ERLANG_COOKIE=${ERLANG_COOKIE}
      - ERL_AFLAGS=-proto_dist inet_tls
      - ERL_DIST_PORT=9100
      - ERLANG_DISTRIBUTION_PORT_RANGE=9100-9200
    networks:
      - erlmcp-cluster

  erlmcp-node3:
    image: erlmcp:3.0.0
    hostname: erlmcp-node3
    environment:
      - ERLMCP_NODE_NAME=erlmcp@erlmcp-node3
      - ERLANG_COOKIE=${ERLANG_COOKIE}
      - ERL_AFLAGS=-proto_dist inet_tls
      - ERL_DIST_PORT=9100
      - ERLANG_DISTRIBUTION_PORT_RANGE=9100-9200
    networks:
      - erlmcp-cluster

networks:
  erlmcp-cluster:
    driver: overlay
    attachable: true
```

**Start cluster:**

```bash
docker compose -f docker-compose.cluster.yml up -d
```

**Verify cluster:**

```bash
# Check node connectivity
docker compose -f docker-compose.cluster.yml exec erlmcp-node1 \
  /opt/erlmcp/bin/erlmcp eval 'nodes().'

# Expected output: ['erlmcp@erlmcp-node2', 'erlmcp@erlmcp-node3']
```

---

## Monitoring and Observability

### OpenTelemetry Integration

```yaml
services:
  otel-collector:
    image: otel/opentelemetry-collector-contrib:0.114.0
    container_name: erlmcp-otel
    ports:
      - "4317:4317"  # OTLP gRPC
      - "4318:4318"  # OTLP HTTP
    volumes:
      - ./config/docker/otel-collector.yaml:/etc/otel-collector/config.yaml:ro
    networks:
      - erlmcp-network
    command: ["--config=/etc/otel-collector/config.yaml"]
```

### Prometheus

```yaml
services:
  prometheus:
    image: prom/prometheus:v2.48.0
    container_name: erlmcp-prometheus
    ports:
      - "9090:9090"
    volumes:
      - ./config/docker/prometheus.yml:/etc/prometheus/prometheus.yml:ro
      - prometheus-data:/prometheus
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'
      - '--storage.tsdb.retention.time=15d'
    networks:
      - erlmcp-network
```

### Grafana

```yaml
services:
  grafana:
    image: grafana/grafana:10.2.2
    container_name: erlmcp-grafana
    ports:
      - "3000:3000"
    environment:
      - GF_SECURITY_ADMIN_PASSWORD_FILE=/run/secrets/grafana-password
      - GF_USERS_ALLOW_SIGN_UP=false
    secrets:
      - grafana-password
    volumes:
      - grafana-data:/var/lib/grafana
      - ./config/docker/grafana:/etc/grafana/provisioning:ro
    networks:
      - erlmcp-network
    depends_on:
      - prometheus
```

**Start monitoring stack:**

```bash
docker compose --profile monitoring up -d
```

**Access dashboards:**

- Grafana: http://localhost:3000
- Prometheus: http://localhost:9090
- Jaeger: http://localhost:16686

---

## Troubleshooting

### Container Won't Start

```bash
# Check logs
docker compose logs erlmcp

# Check resource usage
docker compose stats erlmcp

# Enter container for debugging
docker compose exec erlmcp /bin/sh

# Check health status
docker compose ps erlmcp
```

### Database Connection Issues

```bash
# Test database connection
docker compose exec postgres psql -U erlmcp -c "\dt"

# Check database health
docker compose ps postgres

# View database logs
docker compose logs postgres
```

### Memory Issues

```bash
# Monitor memory usage
docker compose stats --no-stream erlmcp

# Check Erlang memory (via Docker)
docker compose exec erlmcp /opt/erlmcp/bin/erlmcp eval 'erlang:memory().'
```

### Networking Issues

```bash
# Check network connectivity
docker compose exec erlmcp ping postgres

# Inspect network
docker network inspect erlmcp_erlmcp-network

# Check DNS resolution
docker compose exec erlmcp nslookup postgres
```

### Performance Issues

```bash
# Check resource limits
docker compose config | grep -A 5 resources

# Monitor CPU usage
docker compose stats erlmcp

# Check I/O
docker compose exec erlmcp iostat -x 1
```

### Quality Gate Failures

```bash
# Compilation failure
docker compose run --rm erlmcp-build make compile
# Fix: Check error messages, verify dependencies

# Test failure
docker compose run --rm erlmcp-ct make ct
# Fix: Check test logs in _build/test/logs/

# Coverage failure
docker compose run --rm erlmcp-check make coverage
# Fix: Add tests to reach 80% coverage

# Dialyzer warnings
docker compose run --rm erlmcp-check make dialyzer
# Fix: Address type warnings in code
```

---

## References

### Documentation

- [Dockerfile Best Practices](https://docs.docker.com/develop/develop-images/dockerfile_best-practices/)
- [Docker Compose Specification](https://docs.docker.com/compose/compose-file/)
- [Docker Security](https://docs.docker.com/engine/security/)
- [Erlang/OTP Distribution](https://www.erlang.org/doc/reference_manual/distributed.html)

### Related Guides

- [Kubernetes Deployment Guide](../kubernetes/README.md)
- [Security Guide](../../security/README.md)
- [Observability Guide](../../observability/README.md)
- [Development Guide](../../../development/README.md)

---

## License

Copyright 2026 erlmcp contributors.

Licensed under the Apache License, Version 2.0.

---

**CODE LIKE A AGI Joe Armstrong.**

**"Make it work, make it right, make it fast."**

**DOCKER-ONLY. NO EXCEPTIONS.**
