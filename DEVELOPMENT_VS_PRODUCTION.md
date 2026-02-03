# Development vs Production Configuration Guide

This guide helps you choose the right configuration for your erlmcp deployment.

## Quick Reference

| Feature | Development | Production | Testing |
|---------|-------------|------------|---------|
| **Docker Target** | `debug` stage | `runtime` stage | `builder` stage |
| **Image Size** | ~1.5GB | ~150MB | ~1.5GB |
| **Debug Tools** | vim, htop, strace | None | Full build tools |
| **Log Level** | Debug | Info | Info/Debug |
| **Resource Limits** | None (host) | CPU/Memory constrained | Quality lane specific |
| **Health Checks** | Basic | 3-level verification | Test-specific |
| **Profiling** | Enabled | Disabled (security) | As needed |
| **Hot Reload** | Yes | No | N/A |

---

## Development Configuration

### Use When
- Building and testing locally
- Debugging issues
- Developing new features
- Running benchmarks

### Docker Profile: `dev`

```bash
# Start development container
docker compose --profile dev up -d

# Access interactive shell
docker attach erlmcp-dev
```

### Characteristics

| Aspect | Configuration |
|--------|---------------|
| **Base Image** | `erlang:28.3.1-alpine` |
| **Installed Tools** | vim, htop, strace, tcpdump, bind-tools |
| **Ports** | 8081 (HTTP), 9110 (distribution) |
| **Volume Mount** | Source code read-write |
| **Resource Limits** | Optional (can be disabled) |
| **Log Level** | Debug |
| **Profiling** | fprof, eprof available |

### Development Workflow

```bash
# 1. Start development container
docker compose --profile dev up -d

# 2. Run compilation
docker compose run --rm erlmcp-build rebar3 compile

# 3. Run tests
docker compose run --rm erlmcp-unit rebar3 eunit
docker compose run --rm erlmcp-ct rebar3 ct

# 4. Run quality analysis
docker compose run --rm erlmcp-check make check

# 5. Interactive debugging
docker attach erlmcp-dev
# Inside container:
make console
# Erlang shell with full debug capabilities
```

### Debugging in Development

```bash
# Attach to running container
docker exec -it erlmcp-dev /bin/bash

# View logs in real-time
docker logs -f erlmcp-dev

# Run debugger
docker exec erlmcp-dev rebar3 shell
# Then: debugger:start().
```

---

## Production Configuration

### Use When
- Deploying to production
- Running in cloud environments
- Serving real traffic
- Multi-node clusters

### Docker Profile: `runtime`

```bash
# Start production runtime
docker compose --profile runtime up -d

# Verify health
curl http://localhost:8080/health
```

### Characteristics

| Aspect | Configuration |
|--------|---------------|
| **Base Image** | `alpine:3.20` |
| **Installed Tools** | None (minimal) |
| **Ports** | 8080 (HTTP), 9090 (health), 9100 (metrics) |
| **Volume Mount** | Configs read-only, data/log read-write |
| **Resource Limits** | CPU: 2.0, Memory: 4G (configurable) |
| **Log Level** | Info |
| **Profiling** | Disabled (security) |
| **User** | Non-root (uid 1000) |

### Production Deployment Options

#### Option 1: Docker Compose (Single Node)

```bash
# Production single-node
docker compose --profile runtime up -d
```

**Best for**: POC, testing, single-server deployments

#### Option 2: Docker Swarm (Multi-Node)

```bash
# Initialize swarm
docker swarm init

# Deploy stack
docker stack deploy -c docker/docker-stack.yml erlmcp-swarm
```

**Best for**: Production HA, multi-server deployments

#### Option 3: Kubernetes

```bash
# Deploy with Helm
helm install erlmcp ./helm/erlmcp-enterprise \
  --namespace erlmcp \
  --values config/scalability/production.values.yaml
```

**Best for**: Cloud-native, auto-scaling deployments

---

## Testing Configuration

### Quality Lanes

erlmcp uses Docker-based quality lanes for all validation.

| Lane | Service | Purpose | Resources |
|------|---------|---------|-----------|
| Compile | `erlmcp-build` | Compilation gate | 2 CPUs, 2GB RAM |
| EUnit | `erlmcp-unit` | Unit test gate | 1 CPU, 1GB RAM |
| CT | `erlmcp-ct` | Integration test gate | 2 CPUs, 2GB RAM |
| Check | `erlmcp-check` | Quality analysis gate | 4 CPUs, 4GB RAM |
| Bench | `erlmcp-bench` | Performance gate | 2 CPUs, 2GB RAM |
| Cluster | `erlmcp-node` | Cluster testing | 1 CPU, 1GB RAM |

### Running Quality Lanes

```bash
# Compile
docker compose run --rm erlmcp-build rebar3 compile

# Unit tests
docker compose run --rm erlmcp-unit make eunit

# Integration tests
docker compose run --rm erlmcp-ct make ct

# Quality analysis (dialyzer, xref, coverage)
docker compose run --rm erlmcp-check make check

# Benchmarks
docker compose run --rm erlmcp-bench make benchmark

# Full pipeline
docker compose run --rm erlmcp-build make compile && \
docker compose run --rm erlmcp-unit make eunit && \
docker compose run --rm erlmcp-ct make ct && \
docker compose run --rm erlmcp-check make check
```

---

## Configuration Comparison

### Image Size Comparison

```
Development (debug):  ~1.5GB
├── Erlang/OTP 28.3.1-alpine:  ~200MB
├── Build tools:              ~800MB
├── Debug tools:              ~200MB
└── Source code:              ~300MB

Production (runtime): ~150MB
├── Alpine 3.20:              ~5MB
├── Erlang runtime:           ~80MB
├── Application release:      ~60MB
└── Health scripts:           ~5MB
```

### Resource Limits by Configuration

| Configuration | CPU Limit | Memory Limit | PIDs Limit |
|---------------|-----------|--------------|------------|
| Development | None (host) | None (host) | None (host) |
| Production | 2.0 (4 max) | 4GB (8GB max) | 4096 |
| Build Lane | 2.0 | 2GB | 4096 |
| Unit Lane | 1.0 | 1GB | 2048 |
| CT Lane | 2.0 | 2GB | 4096 |
| Check Lane | 4.0 | 4GB | 8192 |
| Bench Lane | 2.0 | 2GB | 4096 |

---

## Security Comparison

| Security Aspect | Development | Production |
|-----------------|-------------|------------|
| **Run as User** | root (for debugging) | erlmcp (uid 1000) |
| **Privileged Mode** | No | No |
| **Capabilities** | Many (debug tools) | Minimal (NET_BIND_SERVICE) |
| **Read-only Root** | No | Yes |
| **tmpfs** | /tmp, /var/tmp | /tmp, /var/tmp (noexec) |
| **Secrets** | None (hardcoded) | Docker secrets |
| **TLS Distribution** | Optional | Required (inet_tls) |

---

## Logging Configuration

### Development Logging

```erlang
# config/sys.config.src (development)
{erlmcp_core, [
  {log_level, debug},
  {log_console, true},
  {log_file, "/var/log/erlmcp/debug.log"},
  {error_logger_format_maintenance, false}
]}.
```

### Production Logging

```erlang
# config/sys.config.src (production)
{erlmcp_core, [
  {log_level, info},
  {log_console, false},
  {log_file, "/var/log/erlmcp/production.log"},
  {log_rotation, daily},
  {log_retention, 30},
  {error_logger_format_maintenance, true}
]}.
```

---

## Health Check Configuration

### Development Health Check

```bash
#!/bin/bash
# Basic health check (development)
/opt/erlmcp/bin/erlmcp ping
```

### Production Health Check

```bash
#!/bin/bash
# 3-level health verification (production)

# Level 1: HTTP /health endpoint (application health)
if curl -f -s --max-time 5 http://localhost:8080/health > /dev/null 2>&1; then
    echo "health: HTTP endpoint healthy (Level 1: Application)"
    exit 0
fi

# Level 2: Node ping (distribution check)
if /opt/erlmcp/bin/erlmcp ping > /dev/null 2>&1; then
    echo "health: Node responding (Level 2: Distribution)"
    exit 0
fi

# Level 3: Process running check (state check)
if pgrep -f "beam.smp.*erlmcp" > /dev/null; then
    echo "health: Degraded (Level 3: Process running)"
    exit 1
fi

echo "health: Down"
exit 1
```

---

## Performance Tuning

### Development Tuning

```erlang
# vm.args (development)
+K true -A 128                    # Async threads
+MBacul 0                         # No carrier shutdown
+Msbagf 512                       # Binary allocator
+P 1048576                        # Max processes
+Q 65536                          # Max ports
```

### Production Tuning

```erlang
# vm.args (production)
+K true -A 128                    # Async threads
+MBacul 0                         # No carrier shutdown
+Msbagf 512                       # Binary allocator
+P 1048576                        # Max processes
+Q 65536                          # Max ports
+SDio 20                          # Dirty I/O scheduler
+SP 10000                         # Scheduler spin time
+swt very_low                     # Scheduler wait threshold
```

---

## Migration: Development to Production

### Step 1: Build Production Image

```bash
# Build from source
docker build \
  --target runtime \
  --tag erlmcp:3.0.0 \
  --build-arg BUILD_DATE=$(date -u +"%Y-%m-%dT%H:%M:%SZ") \
  --build-arg VCS_REF=$(git rev-parse --short HEAD) \
  .
```

### Step 2: Test Production Image

```bash
# Test locally first
docker run --rm -p 8080:8080 erlmcp:3.0.0

# Verify health
curl http://localhost:8080/health
```

### Step 3: Deploy to Production

```bash
# Docker Compose
docker compose --profile runtime up -d

# Docker Swarm
docker stack deploy -c docker/docker-stack.yml erlmcp-swarm

# Kubernetes
kubectl apply -f k8s/production/
```

---

## Checklist: Choose Your Configuration

### Choose Development If
- [ ] You're building or modifying erlmcp
- [ ] You need to debug issues
- [ ] You're running tests
- [ ] You're developing features

### Choose Production If
- [ ] You're deploying to production
- [ ] You're serving real traffic
- [ ] You need minimal image size
- [ ] You need maximum security

### Choose Testing If
- [ ] You're running CI/CD pipelines
- [ ] You need quality gates
- [ ] You're running benchmarks
- [ ] You're validating compliance

---

## Next Steps

- [Docker Quick Start](QUICKSTART_DOCKER.md) - Get started with Docker
- [Production Deployment Checklist](DOCKER_DEPLOYMENT_CHECKLIST.md) - Production readiness
- [Quality Lanes](README.md#quality-lanes) - Testing procedures
- [Deployment Guide](docs/deployment/) - Complete deployment procedures
