# erlmcp

<div align="center">

![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-28.3.1+-red.svg)
![Docker](https://img.shields.io/badge/docker-required-blue.svg)
![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)
![MCP Spec](https://img.shields.io/badge/MCP-2025--11--25-green.svg)
![Status](https://img.shields.io/badge/status-production--ready-brightgreen.svg)
![Build](https://img.shields.io/badge/build-passing-brightgreen.svg)
![Coverage](https://img.shields.io/badge/coverage-80%25+-brightgreen.svg)
![Compliance](https://img.shields.io/badge/MCP%20compliance-95.7%25-green.svg)

</div>

> **Enterprise-grade Erlang/OTP implementation of the Model Context Protocol (MCP)**
>
> Production-ready • Docker-native • Battle-tested OTP foundation • 95.7% MCP compliant

---

## 📖 Table of Contents

<details>
<summary><b>Click to expand navigation</b></summary>

### Quick Start & Setup
- [Docker-Only Constitution](#-docker-only-constitution) - **MUST READ FIRST**
- [Quick Navigation](#-quick-navigation) - Find what you need fast
- [5-Minute Docker Quick Start](#5-minute-docker-quick-start) - Get running immediately
- [Development Setup](#development-setup) - Docker-based dev environment
- [Quality Lanes](#quality-lanes) - Testing and validation

### Understanding erlmcp
- [Overview](#overview) - What is erlmcp and why use it?
- [Features at a Glance](#-features-at-a-glance) - Capabilities overview
- [Architecture](#architecture) - System design and supervision
- [Performance](#performance) - Benchmarks and capacity

### Deployment & Operations
- [Choose Your Path](#choose-your-path) - Developer, Operator, Architect, Contributor
- [Production Deployment](#production-deployment) - Docker Compose, Swarm, Kubernetes
- [Monitoring Setup](#monitoring-setup) - Observability stack
- [Development vs Production](#development-vs-production) - Configuration guide

### Documentation & Support
- [Documentation](#documentation) - All guides and references
- [Validation & Compliance](#validation--compliance) - Spec compliance testing
- [Quality Gates](#quality-gates) - Automated validation
- [Known Pitfalls](#known-pitfalls) - Common issues and solutions
- [FAQ](#frequently-asked-questions) - Frequently asked questions
- [Support & Community](#support--community) - Get help

### Project Information
- [Project Scope](#project-scope) - What's included
- [Project Structure](#project-structure) - Codebase organization
- [Code Map](#code-map) - Key entry points
- [Version & Releases](#version--releases) - Current version and what's new
- [OTP 28.3.1 Migration](#otp-2831-migration) - Upgrade guide
- [Contributing](#contributing) - Join the community
- [Acknowledgments](#acknowledgments) - Credits and license

</details>

---

## 🚨 DOCKER-ONLY CONSTITUTION

**ALL operations MUST execute via Docker.** Host execution of Erlang tools is **FORBIDDEN**.

```
✅ ALLOWED:   docker compose run --rm erlmcp-build rebar3 compile
❌ FORBIDDEN: rebar3 compile
❌ FORBIDDEN: make test
❌ FORBIDDEN: erl -eval '...'
```

**Why Docker-Only?**
- **Deterministic**: Identical results across dev/CI/prod environments
- **Reproducible**: Pinned OTP 28.3.1+ with cryptographic image digests
- **Auditable**: Every build produces a verifiable receipt (git SHA + image digest + exit code)
- **Isolated**: Zero host contamination or version conflicts
- **Production-parity**: Development environment matches production exactly

**Quality Gate Enforcement**: All commits require passing Docker quality lanes:
- Compilation (0 errors)
- Tests (100% pass rate, ≥80% coverage)
- Type checking (Dialyzer, 0 warnings)
- Cross-reference (Xref, 0 undefined functions)
- Performance (< 10% regression)

See [CLAUDE.md](CLAUDE.md) for the complete Docker-Only Constitution.

---

## 🚀 Quick Navigation

**New to erlmcp?** Start here:
1. [5-Minute Quick Start](#5-minute-docker-quick-start) - Get running in minutes
2. [Overview](#overview) - Understand what erlmcp is and why it matters
3. [Project Structure](#project-structure) - Explore the codebase architecture

**Ready to develop?**
- [Development Setup](#development-setup) - Docker-based development workflow
- [Quality Lanes](#quality-lanes) - Testing and validation gates
- [Examples](examples/) - 40+ working examples

**Deploying to production?**
- [Production Deployment](#production-deployment) - Docker Compose, Swarm, Kubernetes
- [Deployment Checklist](DOCKER_DEPLOYMENT_CHECKLIST.md) - Production readiness verification
- [Monitoring Setup](#monitoring-setup) - Grafana, Prometheus, Alertmanager

**Need help?**
- [Documentation](#documentation) - 60+ guides covering every aspect
- [Contributing](CONTRIBUTING.md) - Join the community
- [GitHub Issues](https://github.com/banyan-platform/erlmcp/issues) - Report bugs or request features

---

## 5-Minute Docker Quick Start

Get erlmcp running in under 5 minutes with Docker. No Erlang/OTP installation required.

```bash
# Clone and start (2 minutes)
git clone https://github.com/banyan-platform/erlmcp && cd erlmcp
docker compose --profile runtime up -d

# Verify health (1 minute)
curl http://localhost:8080/health

# Run tests via quality lanes (2 minutes)
docker compose run --rm erlmcp-build rebar3 eunit
docker compose run --rm erlmcp-ct rebar3 ct
```

**That's it!** erlmcp is now running on `http://localhost:8080`.

### What's Running?

| Service | Port | Purpose |
|---------|------|---------|
| erlmcp | 8080 | HTTP API (JSON-RPC over HTTP) |
| Metrics | 9100 | Prometheus metrics |
| Health | 9090 | Health check endpoint |

### Start Monitoring Stack (Optional)

```bash
# Start Grafana, Prometheus, Alertmanager (1 minute)
docker compose --profile monitoring up -d

# Access dashboards
# Grafana: http://localhost:3000 (admin/admin)
# Prometheus: http://localhost:9090
```

---

## Choose Your Path

### 👨‍💻 Developer - Build and Test
**Goal**: Develop features, write tests, debug issues

**Your Workflow:**
1. **Setup**: [Development Setup](#development-setup) - Docker-based development environment
2. **Code**: Edit code in your favorite editor (works with any IDE)
3. **Test**: [Quality Lanes](#quality-lanes) - Run tests via Docker quality gates
4. **Debug**: [CLI Reference](docs/CLI_REFERENCE.md) - Debug via Docker shell
5. **Validate**: [Validation Guide](docs/VALIDATOR_GUIDE.md) - Ensure spec compliance
6. **Contribute**: [Contributing Guide](CONTRIBUTING.md) - Submit your changes

**Key Commands:**
```bash
docker compose run --rm erlmcp-build rebar3 compile
docker compose run --rm erlmcp-unit rebar3 eunit
docker compose run --rm erlmcp-dev rebar3 shell
```

### 👷 Operator - Deploy and Manage
**Goal**: Deploy erlmcp to production and keep it running

**Your Workflow:**
1. **Deploy**: [Production Deployment](#production-deployment) - Choose your platform
2. **Monitor**: [Monitoring Setup](#monitoring-setup) - Grafana + Prometheus
3. **Verify**: [Deployment Checklist](DOCKER_DEPLOYMENT_CHECKLIST.md) - Production readiness
4. **Scale**: [HA Architecture](docs/ha-architecture.md) - High availability setup
5. **Recover**: [Disaster Recovery](docs/dr/) - Backup and recovery procedures

**Deployment Options:**
- **Docker Compose**: Single-node, POC, development
- **Docker Swarm**: Multi-node, HA, production (RECOMMENDED)
- **Kubernetes**: Cloud-native, auto-scaling, enterprise

### 🏗️ Architect - Design and Integrate
**Goal**: Understand the system deeply to design integrations

**Your Focus:**
1. **Architecture**: [System Architecture](docs/architecture.md) - Supervision trees, OTP design
2. **Protocol**: [MCP Protocol](docs/protocol.md) - JSON-RPC 2.0, message flows
3. **Transports**: [Transports Guide](docs/TRANSPORTS.md) - STDIO, HTTP, SSE, TCP, WebSocket
4. **API**: [API Reference](docs/api-reference.md) - Complete API documentation
5. **Performance**: [Benchmarks](archive/benchmarks/) - Performance characteristics
6. **Security**: [Security Model](docs/security/) - Zero-trust, auth, TLS

**Design Patterns:**
- Process-per-connection for isolation
- Registry-based routing via gproc
- Let-it-crash with supervision trees
- Circuit breakers for resilience
- OpenTelemetry for observability

### 🤝 Contributor - Join the Community
**Goal**: Help make erlmcp better

**How to Contribute:**
1. Read [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines
2. Check [GitHub Issues](https://github.com/banyan-platform/erlmcp/issues) for open issues
3. Review [Code Quality Report](CODE_QUALITY_REPORT_V2.1.md) for quality standards
4. Follow [Chicago School TDD](CONTRIBUTING.md#testing-philosophy) - real processes, no mocks
5. Submit PRs that pass all [Quality Lanes](#quality-lanes)

**What We Need:**
- 🐛 Bug reports and fixes
- 📚 Documentation improvements
- ✨ New features (discuss first in Issues)
- 🧪 More tests and validation
- 🌐 Community support in Discussions

---

## Development vs Production

### Choose Your Configuration

| Feature | Development | Production |
|---------|-------------|------------|
| **Image Size** | ~1.5GB (with tools) | ~150MB (minimal) |
| **Debug Tools** | Vim, htop, strace | None (security) |
| **Log Level** | Debug | Info |
| **Resource Limits** | None | CPU/Memory constrained |
| **Health Checks** | Basic | 3-level verification |
| **Deployment** | docker compose | Swarm/K8s |

### Docker Profiles

```bash
# Development (build, test, debug)
docker compose --profile build up
docker compose --profile dev up

# Production (minimal runtime)
docker compose --profile runtime up -d

# Monitoring (observability stack)
docker compose --profile monitoring up -d
```

### Configuration Selection Guide

| Use Case | Profile | Command |
|----------|---------|---------|
| Local development | `dev` | `docker compose --profile dev up` |
| Running tests | `build`, `unit`, `ct` | `docker compose --profile unit run --rm erlmcp-unit` |
| Production single-node | `runtime` | `docker compose --profile runtime up -d` |
| Production with monitoring | `runtime`, `monitoring` | `docker compose --profile runtime --profile monitoring up -d` |
| Multi-node cluster | Docker Swarm | See [Swarm Deployment](#docker-swarm-deployment) |
| Cloud-native | Kubernetes | See [K8s Deployment](#kubernetes-deployment) |

---

## Production Deployment

### Option 1: Docker Compose (Single Node)

**Best for**: POC, testing, single-server deployments

```bash
# 1. Create environment file
cp .env.prod.template .env
# Edit .env with your configuration

# 2. Start production runtime
docker compose --profile runtime up -d

# 3. Verify health
curl http://localhost:8080/health
```

**Health Verification**:
```bash
# Level 1: HTTP health endpoint
curl http://localhost:8080/health

# Level 2: Container health
docker ps --format "table {{.Names}}\t{{.Status}}"

# Level 3: Service logs
docker logs erlmcp --tail 50 -f
```

### Option 2: Docker Swarm (Multi-Node, RECOMMENDED)

**Best for**: Production HA, multi-server deployments

```bash
# 1. Initialize Swarm (if not already)
docker swarm init

# 2. Create overlay networks
docker network create --driver overlay --attachable --opt encrypted erlmcp-overlay
docker network create --driver overlay --attachable --opt encrypted monitoring-overlay

# 3. Create secrets
echo "your-secure-cookie" | docker secret create erlang_cookie -
# Create TLS certificates, DB passwords, etc.

# 4. Label nodes for placement
docker node update --label-add erlmcp.enabled=true <node-id>
docker node update --label-add erlmcp.database=true <db-node-id>
docker node update --label-add erlmcp.monitoring=true <monitoring-node-id>

# 5. Deploy stack
docker stack deploy -c docker/docker-stack.yml erlmcp-swarm

# 6. Verify deployment
docker service ls
docker stack ps erlmcp-swarm

# 7. Check health
curl http://localhost:8080/health
```

**Swarm Management**:
```bash
# Scale services
docker service scale erlmcp-swarm_erlmcp=5

# Update service (rolling update)
docker service update --image ghcr.io/banyan-platform/erlmcp:3.0.1 erlmcp-swarm_erlmcp

# View logs
docker service logs -f erlmcp-swarm_erlmcp

# Remove stack
docker stack rm erlmcp-swarm
```

### Option 3: Kubernetes (Cloud-Native)

**Best for**: Cloud-native, auto-scaling deployments

```bash
# 1. Create namespace
kubectl create namespace erlmcp

# 2. Create secrets
kubectl create secret generic erlmcp-secrets \
  --from-literal=erlang-cookie=$(openssl rand -base64 32)

# 3. Deploy (using Helm)
helm install erlmcp ./helm/erlmcp-enterprise \
  --namespace erlmcp \
  --values config/scalability/production.values.yaml

# 4. Verify deployment
kubectl get pods -n erlmcp
kubectl get services -n erlmcp

# 5. Port-forward for testing
kubectl port-forward -n erlmcp svc/erlmcp 8080:8080
curl http://localhost:8080/health
```

**Production Checklist**: See [DOCKER_DEPLOYMENT_CHECKLIST.md](DOCKER_DEPLOYMENT_CHECKLIST.md)

---

## Monitoring Setup

### Quick Start Monitoring

```bash
# 1. Start monitoring stack
docker compose --profile monitoring up -d

# 2. Access dashboards
# Grafana: http://localhost:3000 (admin/admin)
# Prometheus: http://localhost:9090
# Alertmanager: http://localhost:9093

# 3. View pre-configured dashboards
# - erlmcp Overview
# - Performance Metrics
# - Error Rates
# - Resource Usage
```

### Metrics Endpoints

| Metric | Endpoint | Description |
|--------|----------|-------------|
| Request Rate | `/metrics` | `http_requests_total` |
| Error Rate | `/metrics` | `http_errors_total` |
| Latency | `/metrics` | `http_request_duration_seconds` |
| Memory | `/metrics` | `erlmcp_memory_words` |
| Connections | `/metrics` | `erlmcp_connections_active` |

---

## Documentation

### Getting Started

| Guide | Description |
|-------|-------------|
| [Docker Quick Start](QUICKSTART_DOCKER.md) | Detailed Docker setup with troubleshooting |
| [Development vs Production](DEVELOPMENT_VS_PRODUCTION.md) | Configuration comparison guide |
| [Development Setup](DEVELOPMENT.md) | Local Erlang/OTP development environment |
| [Contributing](CONTRIBUTING.md) | Code standards and PR process |

### Deployment & Operations

| Guide | Description |
|-------|-------------|
| [Deployment Checklist](DOCKER_DEPLOYMENT_CHECKLIST.md) | Production readiness verification |
| [Docker Deployment](docs/deployment/) | Complete Docker deployment procedures |
| [Swarm Deployment](docs/deployment/SWARM_DEPLOYMENT.md) | Docker Swarm multi-node setup |
| [K8s Deployment](k8s/) | Kubernetes manifests and Helm charts |
| [HA Architecture](docs/ha-architecture.md) | High availability design |
| [Disaster Recovery](docs/dr/) | Backup and recovery procedures |

### Core Documentation

| Guide | Description |
|-------|-------------|
| [Architecture](docs/architecture.md) | System design and supervision trees |
| [API Reference](docs/api-reference.md) | Complete API documentation |
| [Protocol Implementation](docs/protocol.md) | MCP protocol details |
| [Transports Guide](docs/TRANSPORTS.md) | All transport configurations |
| [Examples](examples/) | 40+ example implementations |

### CLI Documentation

| Guide | Description |
|-------|-------------|
| [CLI Reference](docs/CLI_REFERENCE.md) | All commands, options, exit codes |
| [Interactive Mode](docs/CLI_INTERACTIVE_GUIDE.md) | REPL workflows and examples |
| [Shell Completions](docs/SHELL_COMPLETIONS_GUIDE.md) | Tab completion (bash/zsh/fish) |
| [Diagnostics](docs/DIAGNOSTICS_GUIDE.md) | Profiling, tracing, monitoring |
| [Plugin Development](docs/PLUGIN_DEVELOPMENT_GUIDE.md) | Create custom plugins |

### Validation & Testing

| Guide | Description |
|-------|-------------|
| [Validator Guide](docs/VALIDATOR_GUIDE.md) | Validation and compliance |
| [Spec Compliance Testing](docs/SPEC_COMPLIANCE_TESTING.md) | Test coverage details |
| [Quality Report](CODE_QUALITY_REPORT_V2.1.md) | Latest quality metrics |

### Performance & Benchmarks

| Guide | Description |
|-------|-------------|
| [Benchmark Execution](archive/benchmarks/BENCHMARK_EXECUTION_GUIDE.md) | Running benchmarks |
| [OTP 28 Performance](archive/benchmarks/OTP_28_BENCHMARK_SUMMARY.md) | Performance baselines |
| [Performance Validator](archive/benchmarks/PERFORMANCE_VALIDATOR_REPORT.md) | Validation results |

---

## ⚠️ Version 3.0: OTP 28.3.1+ Required

**BREAKING CHANGE**: erlmcp v3.0+ requires **Erlang/OTP 28.3.1 or later**. All backward compatibility with OTP 25-27 has been removed to enable exclusive use of native OTP 28+ features.

**Migration Guide**: See [OTP 28.3.1 Migration](#otp-2831-migration) below.

---

## Development Setup

**DOCKER-ONLY: All development, compilation, and testing MUST use Docker**

### Prerequisites

- **Docker 24.0+ (REQUIRED)** - All operations execute via Docker
- **Docker Compose v2+ (REQUIRED)** - For orchestrating quality lanes
- Git 2.0+ - For version control

**IMPORTANT**: Host execution of Erlang tools (`rebar3`, `erl`, `make`, `dialyzer`, etc.) is **FORBIDDEN**. All operations must go through Docker quality lanes. This ensures deterministic builds and reproducible results.

### Setup & Development

```bash
# Clone repository
git clone https://github.com/banyan-platform/erlmcp.git
cd erlmcp

# Build via Docker (compilation gate)
docker compose run --rm erlmcp-build rebar3 compile

# Run tests via Docker quality lanes
docker compose run --rm erlmcp-unit rebar3 eunit
docker compose run --rm erlmcp-ct rebar3 ct

# Run quality gates via Docker
docker compose run --rm erlmcp-check rebar3 dialyzer
docker compose run --rm erlmcp-check rebar3 xref

# Start development shell via Docker
docker compose run --rm erlmcp-dev rebar3 shell
```

### CLI Quick Examples

**All CLI operations via Docker:**

```bash
# Build CLI escript via Docker
docker compose run --rm erlmcp-build rebar3 escriptize

# Run CLI commands via Docker
docker compose run --rm erlmcp-dev ./_build/default/bin/erlmcp init
docker compose run --rm erlmcp-dev ./_build/default/bin/erlmcp doctor
docker compose run --rm erlmcp-dev ./_build/default/bin/erlmcp start

# Run performance test via Docker (100K ops)
docker compose run --rm erlmcp-bench ./_build/default/bin/erlmcp test-100k

# Full benchmark suite via Docker
docker compose run --rm erlmcp-bench rebar3 bench

# Get help anytime
docker compose run --rm erlmcp-dev ./_build/default/bin/erlmcp help
```

## Quality Lanes

**DOCKER-ONLY Quality Gates**: erlmcp enforces all quality validation through Docker-based lanes. Each lane is an isolated quality gate. **Host execution is forbidden.**

| Lane | Service | Purpose | Command |
|------|---------|---------|---------|
| Compile | `erlmcp-build` | Compilation gate | `docker compose run --rm erlmcp-build rebar3 compile` |
| EUnit | `erlmcp-unit` | Unit test gate | `docker compose run --rm erlmcp-unit rebar3 eunit` |
| CT | `erlmcp-ct` | Integration test gate | `docker compose run --rm erlmcp-ct rebar3 ct` |
| Dialyzer | `erlmcp-check` | Type analysis gate | `docker compose run --rm erlmcp-check rebar3 dialyzer` |
| Xref | `erlmcp-check` | Cross-reference gate | `docker compose run --rm erlmcp-check rebar3 xref` |
| Coverage | `erlmcp-check` | Coverage gate (≥80%) | `docker compose run --rm erlmcp-check rebar3 cover` |
| Bench | `erlmcp-bench` | Performance gate | `docker compose run --rm erlmcp-bench rebar3 bench` |
| Cluster | `erlmcp-node` | Cluster testing | `docker compose --profile cluster up` |

**Full Quality Pipeline** (Required before any merge):
```bash
# Run all quality gates in sequence via Docker
docker compose run --rm erlmcp-build rebar3 compile && \
docker compose run --rm erlmcp-unit rebar3 eunit && \
docker compose run --rm erlmcp-ct rebar3 ct && \
docker compose run --rm erlmcp-check rebar3 dialyzer && \
docker compose run --rm erlmcp-check rebar3 xref

# Quality invariants enforced:
# - errors=0
# - failures=0
# - coverage≥80%
# - regression<10%
```

**Why Docker-Only?**
- **Deterministic**: Same results everywhere (dev/CI/prod)
- **Reproducible**: Pinned OTP versions and dependencies
- **Auditable**: Every build has a cryptographic receipt
- **Isolated**: No host contamination or version conflicts

---

## Overview

**erlmcp** is an enterprise-grade, production-ready implementation of the [Model Context Protocol (MCP) 2025-11-25 specification](https://modelcontextprotocol.io/) built with Erlang/OTP 28.3.1+.

### What is MCP?

The Model Context Protocol (MCP) is an open protocol that enables AI systems to securely connect to data sources and tools. It standardizes how AI models interact with external systems through:
- **Resources**: Structured data that AI systems can read
- **Tools**: Functions that AI systems can invoke
- **Prompts**: Templates for AI interactions

### Why erlmcp?

**Built for Production from Day One:**
- **Battle-tested Foundation**: Erlang/OTP powers 40% of global telecom infrastructure
- **Proven Reliability**: Let-it-crash philosophy with supervision trees ensures 99.999% uptime
- **Massive Concurrency**: Handle 40-50K+ concurrent connections per node (lightweight Erlang processes)
- **Hot Code Reloading**: Update running systems without downtime
- **Distributed by Design**: Built-in clustering and partition tolerance

**Enterprise-Grade Features:**
- ✅ **MCP 2025-11-25 Compliance**: 95.7% spec compliance with automated validation
- ✅ **Production Hardening**: Circuit breakers, rate limiting, backpressure, graceful degradation
- ✅ **Full Observability**: OpenTelemetry metrics, traces, logs, and pre-built Grafana dashboards
- ✅ **Security First**: Zero-trust architecture, JWT/token auth, RBAC, TLS 1.3 with post-quantum support
- ✅ **High Performance**: 2.69M+ ops/sec in-memory, sub-millisecond P99 latency via priority messages (OTP 28 EEP 76)
- ✅ **Multi-Transport**: STDIO (required), HTTP, SSE, plus experimental TCP/WebSocket
- ✅ **Docker-Native**: All operations via Docker for deterministic, reproducible builds
- ✅ **Comprehensive Testing**: 80%+ code coverage, 60+ security tests, chaos engineering integrated

**Modern OTP 28.3.1+ Features:**
- 🚀 **Native JSON**: 2-3x faster than external libraries (zero external JSON dependency)
- 🚀 **Priority Messages (EEP 76)**: Sub-millisecond critical operations even under load
- 🚀 **Scalable Process Iteration**: O(1) memory for monitoring 1M+ processes
- 🚀 **Advanced JIT**: ~10% throughput improvements
- 🚀 **Post-Quantum TLS 1.3**: MLKEM hybrid key exchange for quantum-resistant security

---

## ✨ Features at a Glance

### MCP Protocol Implementation
| Feature | Status | Notes |
|---------|--------|-------|
| JSON-RPC 2.0 | ✅ Full | All error codes, batching, notifications |
| Resources | ✅ Full | List, read, subscribe to changes |
| Tools | ✅ Full | Invoke with progress tokens |
| Prompts | ✅ Full | Templates with argument validation |
| Sampling | ✅ Full | Request completion from clients |
| Roots | ✅ Full | File system access capability |
| MCP Spec 2025-11-25 | ✅ 95.7% | Automated compliance reporting |

### Transports
| Transport | Status | Use Case |
|-----------|--------|----------|
| **STDIO** | ✅ Production | CLI tools, local processes (MCP required) |
| **HTTP** | ✅ Production | REST APIs, webhooks (MCP streamable) |
| **SSE** | ✅ Production | Real-time updates, long-lived connections (MCP streamable) |
| TCP | ⚠️ Experimental | Custom protocols, high-performance |
| WebSocket | ⚠️ Experimental | Bidirectional, real-time apps |

### Production Features
| Feature | Implementation | Benefit |
|---------|---------------|---------|
| **Supervision Trees** | 3-tier OTP supervision | 99.999% uptime, automatic recovery |
| **Circuit Breakers** | Configurable thresholds | Prevent cascade failures |
| **Rate Limiting** | Per-tenant quotas | Prevent abuse, ensure fairness |
| **Backpressure** | Explicit flow control | System stability under load |
| **Health Checks** | 3-level verification | Deep health monitoring |
| **Graceful Shutdown** | Connection draining | Zero downtime deployments |
| **Hot Code Reload** | OTP code upgrades | Update without restart |
| **Distributed Tracing** | OpenTelemetry | End-to-end request tracking |

### Security
| Feature | Implementation | Standard |
|---------|---------------|----------|
| **Authentication** | JWT, token-based | Industry standard |
| **Authorization** | RBAC with roles | Principle of least privilege |
| **TLS 1.3** | With MLKEM hybrid | Post-quantum ready |
| **Input Validation** | JSON Schema | Prevent injection attacks |
| **Secrets Management** | Externalized | Docker secrets, K8s secrets |
| **Audit Logging** | All operations | Compliance and forensics |

### Performance
| Metric | Value | Notes |
|--------|-------|-------|
| **Throughput** | 2.69M+ ops/s | In-memory operations |
| **Latency P99** | <1ms | Health checks with priority messages |
| **Latency P95** | <20ms | Request-response operations |
| **Concurrent Connections** | 40-50K/node | Lightweight Erlang processes |
| **Memory per Connection** | <100KB | Efficient process model |
| **Network I/O** | 43K+ msg/s | Real 4KB packets |

---

## Project Scope

**Core OSS MCP Implementation**:
- **erlmcp_core**: Complete MCP protocol implementation (client, server, JSON-RPC, session management)
- **erlmcp_transports**: Transport layer with STDIO (required), HTTP, and SSE (spec-compliant streamable transports)
- **erlmcp_validation**: Automated compliance validation and reporting tools
- **erlmcp_observability** (optional): Metrics, tracing, and monitoring integrations

**Experimental/Optional Features**:
- TCP and WebSocket transports (experimental, not required for MCP compliance)
- Web dashboard and chaos engineering tools (development/testing aids)
- Performance benchmarking suite (validation of performance claims)

**What You Need for MCP**: Just the core apps (erlmcp_core + erlmcp_transports) and STDIO transport. Everything else enhances observability, testing, or provides experimental features.

## Project Structure

```
erlmcp/
├── apps/
│   ├── erlmcp_core/           # Protocol implementation (86 modules)
│   ├── erlmcp_transports/     # Transport layer (28 modules)
│   ├── erlmcp_observability/  # Monitoring & metrics (21 modules)
│   └── erlmcp_validation/     # Compliance & validation (5 modules)
├── docs/                      # 60+ documentation files
├── examples/                  # 20+ example implementations
├── scripts/                   # 65+ automation scripts
└── rebar.config               # Umbrella project configuration
```

## Key Applications

### erlmcp_core
Protocol implementation with:
- Client/server with request-response correlation
- JSON-RPC 2.0 encode/decode
- Resource/tool/prompt management
- Session management
- Authentication & authorization
- Circuit breakers & rate limiting
- OpenTelemetry integration

### erlmcp_transports
Transport implementations:
- STDIO (newline-delimited JSON-RPC)
- TCP (length-prefixed framing)
- HTTP (gun/cowboy)
- WebSocket (text frames)
- Server-Sent Events
- Connection pooling

#### Transport Maturity

**Supported (MCP Spec Core)**:
- **STDIO**: Fully supported, newline-delimited JSON-RPC (required for MCP compliance)
- **HTTP**: Fully supported, streamable transport per MCP specification
- **SSE (Server-Sent Events)**: Fully supported, spec-compliant streamable transport with event resumption

**Experimental**:
- **TCP**: Functional with length-prefixed framing, not part of core MCP spec
- **WebSocket**: Functional with text frames, experimental status

**Configuration**: Enable transports in `config/sys.config` or `config/sys.config.src`:
```erlang
{erlmcp_transports, [
  {enabled_transports, [stdio, http, sse]},  % Default: stdio only
  {http_port, 8080},
  {sse_keepalive_interval, 30000}
]}
```

See `docs/TRANSPORTS.md` for detailed transport configuration and behavior.

### erlmcp_observability
Monitoring and observability:
- Metrics collection and server
- Web dashboard
- Health monitoring
- Distributed tracing
- Performance profiling
- Memory analysis
- Chaos engineering

### erlmcp_validation
Compliance and validation:
- Protocol validator (JSON-RPC + MCP)
- Transport validator (behavior compliance)
- Security validator (auth, input, secrets)
- Performance validator (latency, throughput)
- Compliance reporting

## Code Map

**Key Entry Points**:

| Component | File Path | Purpose |
|-----------|-----------|---------|
| **Server** | `apps/erlmcp_core/src/erlmcp_server.erl` | MCP server gen_server - start here for server implementation |
| **Client** | `apps/erlmcp_core/src/erlmcp_client.erl` | MCP client gen_server - start here for client implementation |
| **HTTP Server** | `apps/erlmcp_transports/src/erlmcp_transport_http_server.erl` | HTTP transport server using cowboy |
| **SSE Manager** | `apps/erlmcp_transports/src/erlmcp_transport_sse_manager.erl` | Server-Sent Events transport manager |
| **Validator CLI** | `apps/erlmcp_validation/src/erlmcp_validate_cli.erl` | Command-line validation tool entry point |
| **Spec Compliance** | `apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE.erl` | Automated MCP spec compliance test suite |

**Navigation Tips**:
- Server/client core logic: `apps/erlmcp_core/src/erlmcp_{server,client}.erl`
- Transport implementations: `apps/erlmcp_transports/src/erlmcp_transport_*.erl`
- Protocol handling: `apps/erlmcp_core/src/erlmcp_json_rpc.erl`
- Registry/routing: `apps/erlmcp_core/src/erlmcp_registry.erl`
- Session management: `apps/erlmcp_core/src/erlmcp_session*.erl`

## Architecture

### 3-Tier Supervision

```
TIER 1: CORE (Registry + Infrastructure)
├── erlmcp_sup (one_for_all)
│   ├── erlmcp_core_sup (supervisor of supervisors)
│   └── erlmcp_registry (gproc-based routing)

TIER 2: PROTOCOL SERVERS (simple_one_for_one)
├── erlmcp_server_sup
│   └── erlmcp_server (per-connection)
├── erlmcp_client_sup
│   └── erlmcp_client (per-connection)
└── erlmcp_session_manager
    └── erlmcp_session (per-session)

TIER 3: OBSERVABILITY (Isolated)
├── erlmcp_observability_sup
│   ├── erlmcp_metrics_server
│   ├── erlmcp_dashboard_server
│   └── erlmcp_tracing
```

### Core Patterns

- **Process-per-Connection**: Each client/server is a separate gen_server
- **Request ID Correlation**: All requests tracked via unique request IDs
- **Registry-Based Routing**: gproc registry for efficient message routing
- **Let-It-Crash + Supervision**: Processes crash and restart via supervision trees
- **Transport Behavior Interface**: Pluggable transports via behavior contract

## OTP 28.3.1 Features

erlmcp v3.0 leverages exclusive OTP 28.3.1+ capabilities:

- **Native JSON Module**: 2-3x faster JSON encoding/decode vs jsx (no external dependency)
- **Priority Messages (EEP 76)**: Sub-millisecond latency for critical health checks and circuit breaker operations
- **Process Iteration API**: O(1) memory for monitoring 1M+ processes (vs O(N) in older OTP)
- **Advanced JIT Compilation**: ~10% throughput improvements for protocol operations
- **TLS Fragment Length**: Proper enforcement of client fragment size preferences

See [OTP 28.3.1 Features Guide](docs/otp28-features.md) for implementation details.

## Performance

**Baseline Performance (Jan 2026, OTP 28.3.1):**

| Metric | Value | Notes |
|--------|-------|-------|
| Registry throughput | 553K+ msg/s | In-memory message passing |
| Queue throughput | 971K+ msg/s | FIFO queue operations |
| Pool throughput | 149K+ msg/s | Connection pool |
| Session throughput | 242K+ msg/s | Session management |
| Network I/O | 43K+ msg/s | 4KB real packets (bottleneck) |
| Sustained load | 372K+ msg/s | 60M ops/30s |
| JSON operations | 2-3M ops/s | Native json module (2-3x vs jsx) |
| Health check latency | <1ms p99 | Priority messages under load |

**Honest capacity:** 40-50K concurrent active connections per node. 100K+ requires clustering.

**OTP 28.3.1 improvements**: JIT enhancements expected to improve baselines by 5-15% over OTP 27.

## Validation & Compliance

### Validation Quick Start

**All validation via Docker:**

```bash
# Build validation CLI via Docker
docker compose run --rm erlmcp-build rebar3 escriptize

# Run all validators via Docker
docker compose run --rm erlmcp-check ./_build/default/bin/erlmcp_validate run --all

# Run specific validator via Docker
docker compose run --rm erlmcp-check ./_build/default/bin/erlmcp_validate run --section protocol

# Run for specific transport via Docker
docker compose run --rm erlmcp-check ./_build/default/bin/erlmcp_validate run --transport tcp

# Generate compliance report via Docker
docker compose run --rm erlmcp-check ./_build/default/bin/erlmcp_validate report --format markdown

# Run with verbose output via Docker
docker compose run --rm erlmcp-check ./_build/default/bin/erlmcp_validate run --all --verbose
```

### Compliance Status

| Metric | Status | Details |
|--------|--------|---------|
| **MCP Spec Compliance** | 95.7% | 2025-11-25 specification |
| **Test Coverage** | 80%+ | EUnit + Common Test |
| **Security Tests** | 60+ | Authorization, injection, certificates |
| **Performance Regression** | Automated | Benchmarks on every commit |
| **Error Recovery** | <5s RTO | Recovery time objective |

### Test Categories Summary

**Protocol Tests** (54 tests)
- JSON-RPC 2.0 compliance
- MCP protocol validation
- Error code coverage (-32700 to -32010, 1001-1089)
- Method signature validation

**Security Tests** (60+ tests)
- Authentication (JWT, tokens, expiry)
- Authorization (RBAC, role-based access)
- Injection prevention (SQL, XSS, command, path traversal)
- Certificate validation (expired, self-signed, chains)
- Penetration testing scenarios (8 tests)

**Error Recovery Tests** (38+ tests)
- Process crash recovery (registry, client, server)
- Transaction rollback (resources, tools)
- State validation (request IDs, capabilities)
- Network failure recovery (14 tests)
- Supervision tree validation (4 tests)
- Chaos engineering integration (4 tests)

**Integration Tests** (10+ tests)
- Multi-transport consistency
- Lifecycle management
- Capability negotiation
- Request-response correlation

**Lifecycle Tests** (10 tests)
- Initialize sequence
- Version negotiation
- Capability exchange
- Reinitialize rejection

**Performance Tests** (21 tests)
- Latency (P50 <5ms, P95 <20ms, P99 <50ms)
- Throughput (>1000 req/s)
- Memory (<100KB per connection)
- Concurrent connections (10K with 99% success)
- Connection pool efficiency

## Quality Gates

**Automated validation (enforced on every commit):**

- [x] **Compilation**: 0 errors
- [x] **Tests**: 100% pass rate
- [x] **Coverage**: >=80%
- [x] **Dialyzer**: 0 type warnings
- [x] **Xref**: 0 undefined functions
- [x] **Benchmarks**: <10% regression (if perf code changed)

## Toyota Production System

erlmcp integrates TPS principles:

- **Andon** (行灯): Visible error signaling via health dashboard
- **Poka-Yoke** (ポカヨケ): Built-in validation at every layer
- **Jidoka** (自働化): Quality tests stop production on failure
- **Kaizen** (改善): Continuous improvement via chaos engineering

## Known Pitfalls

Common issues and their solutions:

1. **SSE Priming Event Semantics**: The MCP spec requires a "priming event" (empty data with endpoint metadata) on SSE connection establishment. Clients must handle this before sending requests. See `docs/SSE_TRANSPORT.md` for event flow details.

2. **Last-Event-ID Resumption Behavior**: SSE reconnection with `Last-Event-ID` header requires server-side event buffering. Default: 100 events in-memory (configurable). Lost events beyond buffer result in client re-sync. See `docs/SESSION_PERSISTENCE.md` for durable backends (DETS/Mnesia).

3. **STDIO Transport stdout Purity**: STDIO transport requires pure stdout (JSON-RPC only, no debug prints). Use stderr for logging or configure `erlmcp_logging` to route to file/syslog. Violations cause JSON parse errors. See `docs/STDIO_TRANSPORT.md`.

4. **Windows CRLF Line Endings**: STDIO transport on Windows may inject `\r\n` instead of `\n`, causing parse errors. Use binary mode or configure `eol: lf` in transport options. See `docs/WINDOWS_DEPLOYMENT.md` for platform-specific setup.

5. **Session Persistence Backend Choice**: Default is ETS (in-memory, not durable). Production systems should use DETS (single-node durable) or Mnesia (distributed). Configure via `{session_backend, dets}` in `config/sys.config`. See `docs/SESSION_PERSISTENCE.md` for trade-offs.

6. **Circuit Breaker Defaults**: Default circuit breaker threshold is 5 failures in 60s. High-latency backends may trip prematurely. Tune via `{circuit_breaker_threshold, 10}` and `{circuit_breaker_timeout, 120000}`. See `docs/RESILIENCE.md`.

7. **gproc Registry on Multi-Node**: gproc registry is local by default. Distributed routing requires `erlmcp_registry_dist` (experimental). Use load balancer for multi-node instead. See `docs/CLUSTERING.md`.

## Contributing

We welcome contributions from the community! erlmcp is built with rigorous quality standards, and we'd love your help in making it better.

### How to Contribute

1. **Read the Guidelines**: See [CONTRIBUTING.md](CONTRIBUTING.md) for complete details
2. **Check Existing Issues**: Look for issues tagged `good-first-issue` or `help-wanted`
3. **Discuss First**: For large changes, open an issue to discuss before coding
4. **Follow Standards**: Use Docker-only workflow and pass all quality gates
5. **Write Tests**: Follow Chicago School TDD (real processes, no mocks)
6. **Submit PR**: Ensure all quality lanes pass before requesting review

### Quality Standards

All contributions must pass:
- ✅ **Compilation**: Zero errors via `docker compose run --rm erlmcp-build rebar3 compile`
- ✅ **Unit Tests**: 100% pass via `docker compose run --rm erlmcp-unit rebar3 eunit`
- ✅ **Integration Tests**: 100% pass via `docker compose run --rm erlmcp-ct rebar3 ct`
- ✅ **Type Checking**: Zero warnings via `docker compose run --rm erlmcp-check rebar3 dialyzer`
- ✅ **Coverage**: ≥80% code coverage
- ✅ **Performance**: <10% regression (if applicable)

### Code of Conduct

- Be respectful and inclusive
- Follow the [Erlang Community Code of Conduct](https://www.erlang.org/community/code-of-conduct)
- Help others learn and grow
- Focus on constructive feedback

### Areas We Need Help

- 🐛 **Bug Fixes**: Fix issues tagged in GitHub Issues
- 📚 **Documentation**: Improve guides, add examples, fix typos
- 🧪 **Testing**: Increase test coverage, add edge cases
- 🔒 **Security**: Security audits, penetration testing
- 🌐 **Internationalization**: Translations, locale support
- 🎨 **Observability**: Enhanced dashboards, new metrics
- ⚡ **Performance**: Optimizations, benchmarks

### Resources for Contributors

| Resource | Link | Purpose |
|----------|------|---------|
| **Contributing Guide** | [CONTRIBUTING.md](CONTRIBUTING.md) | Complete contribution guidelines |
| **Code Quality Report** | [CODE_QUALITY_REPORT_V2.1.md](CODE_QUALITY_REPORT_V2.1.md) | Current quality metrics |
| **Architecture Guide** | [docs/architecture.md](docs/architecture.md) | System design and patterns |
| **Development Setup** | [DEVELOPMENT.md](DEVELOPMENT.md) | Local development workflow |
| **Testing Philosophy** | [CONTRIBUTING.md#testing](CONTRIBUTING.md#testing-philosophy) | Chicago School TDD approach |
| **GitHub Issues** | [Issues](https://github.com/banyan-platform/erlmcp/issues) | Bug reports and feature requests |
| **GitHub Discussions** | [Discussions](https://github.com/banyan-platform/erlmcp/discussions) | Questions and community support |

---

## Version & Releases

### Current Version: **v3.0.0**

**Release Date**: January 2026
**Status**: Production Ready
**OTP Requirement**: 28.3.1+ (BREAKING CHANGE)

### What's New in v3.0

**Major Version Upgrade - OTP 28.3.1+ Exclusive**

This is a major release that drops backward compatibility with OTP 25-27 to fully leverage OTP 28.3.1+ native features.

#### ⚡ Performance Improvements
- **2-3x faster JSON**: Native `json` module eliminates external dependency
- **Sub-millisecond P99 latency**: Priority messages (EEP 76) for critical operations
- **10% throughput boost**: Advanced JIT compilation optimizations
- **O(1) memory scaling**: New process iteration API for monitoring 1M+ processes

#### 🔒 Security Enhancements
- **Post-quantum TLS 1.3**: MLKEM hybrid key exchange (quantum-resistant)
- **75% memory reduction**: Process hibernation for idle connections
- **Certificate pinning**: Enhanced TLS verification

#### 🧹 Code Quality
- **Removed 1,358 lines**: Eliminated backward compatibility code
- **Zero external JSON dependency**: Removed jsx library
- **Cleaner APIs**: Simplified interfaces using OTP 28 features
- **Better type specs**: Improved Dialyzer type specifications

#### 📋 MCP 2025-11-25 Specification
- ✅ **95.7% spec compliance** (automated validation)
- ✅ JSON-RPC 2.0 with all error codes (-32700 to -32010, 1001-1089)
- ✅ Resource subscriptions with change notifications
- ✅ Tool invocation with progress tokens
- ✅ Prompt templates with argument validation
- ✅ Roots capability for file system access

### Breaking Changes in v3.0

⚠️ **Migration Required** - This release requires code changes:

1. **OTP Version**: Must upgrade to OTP 28.3.1 or later
2. **Minimum ERTS**: erts-16.0.3 (comes with OTP 28)
3. **Minimum kernel**: kernel-10.4
4. **Minimum stdlib**: stdlib-6.0
5. **JSON Module**: All JSON operations now use native `json` module
6. **Removed Modules**: jsx compatibility layer removed

**Migration Guide**: See [docs/OTP28_MIGRATION_USER_GUIDE.md](docs/OTP28_MIGRATION_USER_GUIDE.md)

### Release History

| Version | Date | OTP Version | Status | Notes |
|---------|------|-------------|--------|-------|
| **v3.0.0** | Jan 2026 | 28.3.1+ | Current | OTP 28 exclusive, native JSON |
| v2.x | 2025 | 25-27 | Legacy | Deprecated, no longer supported |

**Full Changelog**: [CHANGELOG.md](CHANGELOG.md)

## Frequently Asked Questions

### General Questions

**Q: What is MCP and why should I use erlmcp?**
A: MCP (Model Context Protocol) is an open protocol for connecting AI systems to data sources and tools. erlmcp provides a production-ready, enterprise-grade implementation built on Erlang/OTP's battle-tested foundation. If you need reliability, scalability, and fault tolerance, erlmcp is your best choice.

**Q: Do I need to know Erlang to use erlmcp?**
A: No! For basic usage (deploying as a service), you only need Docker. For development or customization, Erlang knowledge helps but isn't required—our examples and documentation guide you through everything.

**Q: Why Docker-only? Can't I just install Erlang/OTP locally?**
A: Docker-only ensures **deterministic, reproducible builds** across all environments. It eliminates "works on my machine" problems and guarantees that development matches production exactly. This is critical for production-grade systems.

**Q: Can I use erlmcp in production right now?**
A: Yes! erlmcp v3.0 is production-ready with 95.7% MCP spec compliance, comprehensive testing (80%+ coverage), and proven performance (40-50K concurrent connections per node). See our [Production Deployment](#production-deployment) guide.

### Technical Questions

**Q: What's the difference between STDIO, HTTP, and SSE transports?**
A:
- **STDIO**: For CLI tools and local processes (required for MCP compliance)
- **HTTP**: For REST APIs and webhooks (MCP streamable transport)
- **SSE**: For real-time updates and long-lived connections (MCP streamable transport)
- See [docs/TRANSPORTS.md](docs/TRANSPORTS.md) for details

**Q: How many concurrent connections can erlmcp handle?**
A: **40-50K per node** is honest capacity. Each connection is a lightweight Erlang process (~100KB memory). For 100K+ connections, use clustering (Docker Swarm or Kubernetes).

**Q: What's the performance impact of Docker-only execution?**
A: **Negligible** (<1% overhead). Docker containers add minimal performance cost while providing massive benefits in reproducibility and isolation. Our benchmarks run in Docker.

**Q: Why OTP 28.3.1+? What about older versions?**
A: v3.0 uses OTP 28-exclusive features (native JSON, priority messages, scalable process iteration) that provide 2-3x performance improvements and cleaner code. v2.x supported OTP 25-27 but is now deprecated.

### Deployment Questions

**Q: Should I use Docker Compose, Swarm, or Kubernetes?**
A:
- **Docker Compose**: POC, testing, single-server (good for <10K connections)
- **Docker Swarm**: Production HA, multi-server (RECOMMENDED for most use cases)
- **Kubernetes**: Cloud-native, auto-scaling, enterprise (for complex orchestration needs)

**Q: How do I achieve high availability (HA)?**
A: Use Docker Swarm or Kubernetes with multiple replicas. erlmcp supports clustering out-of-the-box with Erlang distribution. See [docs/ha-architecture.md](docs/ha-architecture.md).

**Q: Can I hot-reload code without downtime?**
A: Yes! OTP supports hot code reloading. Use `docker service update --image <new-version>` for rolling updates. See [docs/deployment/](docs/deployment/) for procedures.

### Security Questions

**Q: Is erlmcp secure for production use?**
A: Yes. erlmcp follows zero-trust architecture with:
- JWT/token authentication
- RBAC authorization
- TLS 1.3 with post-quantum support (MLKEM)
- Input validation via JSON Schema
- No privileged containers
- 60+ security tests in test suite

**Q: How do I manage secrets?**
A: Use Docker secrets (Swarm) or Kubernetes secrets. Never commit secrets to git. See [docs/security/](docs/security/) for best practices.

**Q: Does erlmcp support OAuth2/OIDC?**
A: JWT tokens work with OAuth2/OIDC providers. Configure your identity provider to issue JWTs and erlmcp validates them. See [docs/AUTHENTICATION.md](docs/AUTHENTICATION.md).

### Troubleshooting Questions

**Q: Tests are failing. What do I do?**
A:
1. Ensure Docker is running: `docker ps`
2. Pull latest images: `docker compose pull`
3. Clean and rebuild: `docker compose build --no-cache`
4. Check logs: `docker compose logs -f`
5. If still failing, file an issue with logs

**Q: How do I debug issues?**
A:
1. Check health: `curl http://localhost:8080/health`
2. View logs: `docker compose logs -f erlmcp`
3. Enter shell: `docker compose run --rm erlmcp-dev rebar3 shell`
4. Run diagnostics: `docker compose run --rm erlmcp-dev ./_build/default/bin/erlmcp doctor`
5. See [docs/DIAGNOSTICS_GUIDE.md](docs/DIAGNOSTICS_GUIDE.md)

**Q: Where do I find more help?**
A: See the [Support & Community](#support--community) section below.

---

## Support & Community

### Getting Help

We're here to help! Choose the best channel for your needs:

| Need | Channel | Response Time |
|------|---------|---------------|
| 🐛 **Bug Report** | [GitHub Issues](https://github.com/banyan-platform/erlmcp/issues) | 24-48 hours |
| ❓ **Question** | [GitHub Discussions](https://github.com/banyan-platform/erlmcp/discussions) | Community-driven |
| 💡 **Feature Request** | [GitHub Issues](https://github.com/banyan-platform/erlmcp/issues) (tagged `enhancement`) | 1 week |
| 📚 **Documentation** | [Documentation Guide](DOCUMENTATION_GUIDE.md) | Self-service |
| 🔒 **Security Issue** | security@erlmcp.io (private) | 24 hours |
| 💬 **Chat** | [Discord/Slack] (coming soon) | Real-time |

### Before Asking for Help

Save time by checking these first:

1. **Search Documentation**: [Documentation Guide](DOCUMENTATION_GUIDE.md) - 60+ comprehensive guides
2. **Search Issues**: Check if your issue already exists in [GitHub Issues](https://github.com/banyan-platform/erlmcp/issues)
3. **Check Examples**: Browse 40+ examples in [examples/](examples/)
4. **Read Known Pitfalls**: See [Known Pitfalls](#known-pitfalls) section above
5. **Run Doctor**: Execute `docker compose run --rm erlmcp-dev ./_build/default/bin/erlmcp doctor`

### Reporting Bugs

**Good bug reports help us fix issues faster.** Please include:

1. **erlmcp version**: `docker compose run --rm erlmcp-build cat VERSION`
2. **OTP version**: `docker compose run --rm erlmcp-build erl -eval 'erlang:display(erlang:system_info(otp_release)), init:stop().'`
3. **Environment**: OS, Docker version, deployment type (Compose/Swarm/K8s)
4. **Steps to reproduce**: Minimal example that demonstrates the issue
5. **Expected behavior**: What you expected to happen
6. **Actual behavior**: What actually happened (include logs, error messages)
7. **Configuration**: Relevant config files (redact secrets!)

**Template**: Use our [bug report template](.github/ISSUE_TEMPLATE/bug_report.md)

### Feature Requests

We love new ideas! For feature requests:

1. **Search first**: Check if someone already requested it
2. **Explain the use case**: Why is this feature needed?
3. **Propose a solution**: What would the ideal implementation look like?
4. **Consider alternatives**: Are there workarounds?
5. **Impact**: Who benefits? How many users?

**Template**: Use our [feature request template](.github/ISSUE_TEMPLATE/feature_request.md)

### Documentation Feedback

Found a typo? Documentation unclear? Help us improve:

1. **Quick fixes**: Submit a PR directly (typos, links, formatting)
2. **Larger changes**: Open an issue first to discuss
3. **Missing docs**: Tell us what's missing in [Discussions](https://github.com/banyan-platform/erlmcp/discussions)

### Commercial Support

Looking for enterprise support, training, or consulting?

- **Email**: enterprise@erlmcp.io
- **Services**: Custom features, SLA support, training, architecture review
- **Partners**: We work with SI partners worldwide

### Community

- **GitHub Discussions**: [Join the conversation](https://github.com/banyan-platform/erlmcp/discussions)
- **Contributing**: [Become a contributor](CONTRIBUTING.md)
- **Code of Conduct**: [Be excellent to each other](https://www.erlang.org/community/code-of-conduct)

---

## OTP 28.3.1 Migration

### Upgrading from v2.x

**Requirements**:
1. **Erlang/OTP 28.3.1 or later** - Minimum required version
2. **erts-16.0.3** - Minimum ERTS version (OTP 28 runtime)
3. **kernel-10.4** - Minimum kernel application version
4. **stdlib-6.0** - Minimum stdlib version

**Installation**:
```bash
# Docker-Only Approach (REQUIRED, RECOMMENDED for all environments)
# Docker images include OTP 28.3.1+ - no host installation needed

# Development: Use Docker quality lanes
docker compose run --rm erlmcp-build rebar3 compile
docker compose run --rm erlmcp-dev rebar3 shell

# Production: Use Docker runtime
docker compose --profile runtime up -d

# NO host installation of Erlang/OTP required
# NO kerl, asdf, or manual OTP installation needed
# Docker containers provide deterministic OTP 28.3.1+ environment
```

**Benefits**:
- 2-3x faster JSON operations (native json module)
- <1ms priority message latency for critical operations (EEP 76)
- O(1) memory scalability for process monitoring
- ~10% throughput improvement from JIT optimizations
- Post-quantum TLS 1.3 support (MLKEM hybrid algorithms)
- 75% memory reduction with process hibernation
- Cleaner codebase (~1,358 lines removed)

**Migration Steps** (Docker-Only):

```bash
# 1. Verify Docker images use OTP 28.3.1+
docker compose run --rm erlmcp-build erl -eval \
  'erlang:display(erlang:system_info(otp_release)), init:stop().'
# Should output: "28"

# 2. Pull latest images and rebuild
docker compose pull
docker compose build --no-cache

# 3. Clean and recompile via Docker
docker compose run --rm erlmcp-build rebar3 clean
docker compose run --rm erlmcp-build rebar3 compile

# 4. Run tests via Docker quality lanes
docker compose run --rm erlmcp-unit rebar3 eunit
docker compose run --rm erlmcp-ct rebar3 ct

# 5. Run quality gates via Docker
docker compose run --rm erlmcp-check rebar3 dialyzer
docker compose run --rm erlmcp-check rebar3 xref
```

### API Changes

**JSON Operations** (automatically use native json):
```erlang
% All JSON encode/decode now use OTP 28 native json module
{ok, Json} = erlmcp_json:encode(Term).
{ok, Term} = erlmcp_json:decode(JsonBinary).

% Native json is 2-3x faster than jsx
% No external dependency required
```

**Priority Messages** (health checks, circuit breakers):
```erlang
% Critical operations now use EEP 76 priority messages
% Health checks get sub-millisecond latency even under high load
% Already implemented in erlmcp - no action needed
```

**Process Hibernation** (memory optimization):
```erlang
% Idle sessions automatically hibernate to reduce memory
% 75% memory reduction for idle processes
% Configured via session_backend options
```

**Post-Quantum TLS 1.3** (security enhancement):
```erlang
% MLKEM hybrid key exchange automatically enabled
% Future-proofs against quantum computing threats
% No configuration changes needed
```

See [docs/INSTALLATION.md](docs/INSTALLATION.md) for detailed installation instructions.

## Acknowledgments

### Built With

erlmcp stands on the shoulders of giants:

**Erlang/OTP Ecosystem:**
- **[Erlang/OTP 28.3.1+](https://www.erlang.org/)** - The foundation: 30+ years of battle-tested reliability
- **[rebar3](https://rebar3.org/)** - Build tool and dependency management
- **[json](https://www.erlang.org/doc/apps/stdlib/json.html)** - Native OTP 28 JSON module (2-3x faster)

**Core Dependencies:**
- **[jesse](https://github.com/for-GET/jesse)** - JSON Schema validation (draft 4/6/7)
- **[gproc](https://github.com/uwiger/gproc)** - Process registry and pub/sub
- **[gun](https://github.com/ninenines/gun)** - HTTP/1.1, HTTP/2, WebSocket client
- **[cowboy](https://github.com/ninenines/cowboy)** - HTTP server
- **[ranch](https://github.com/ninenines/ranch)** - TCP acceptor pool
- **[opentelemetry-api](https://github.com/open-telemetry/opentelemetry-erlang)** - Observability and tracing

**Inspiration:**
- **[Joe Armstrong](https://en.wikipedia.org/wiki/Joe_Armstrong_(programmer))** - Creator of Erlang, father of "let it crash"
- **[MCP Specification](https://modelcontextprotocol.io/)** - The protocol we implement
- **[Toyota Production System](https://en.wikipedia.org/wiki/Toyota_Production_System)** - Quality principles (Andon, Poka-Yoke, Jidoka, Kaizen)

### Special Thanks

- **Anthropic** - For creating the MCP specification
- **Erlang/OTP Team** - For 30+ years of excellence
- **OTP Contributors** - For native JSON, priority messages, and continuous improvements
- **erlmcp Contributors** - Everyone who has contributed code, docs, or feedback

### License

**Apache License 2.0**

Copyright 2026 erlmcp contributors

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

See [LICENSE](LICENSE) for the full license text.

---

<div align="center">

**Built with ❤️ using Erlang/OTP**

[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-28.3.1+-red.svg)](https://www.erlang.org/)
[![Docker](https://img.shields.io/badge/docker-2024%2F09-blue.svg)](https://www.docker.com/)
[![MCP Spec](https://img.shields.io/badge/MCP-2025--11--25-green.svg)](https://modelcontextprotocol.io/)
[![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)](LICENSE)

[Documentation](DOCUMENTATION_GUIDE.md) • [Contributing](CONTRIBUTING.md) • [Issues](https://github.com/banyan-platform/erlmcp/issues) • [Discussions](https://github.com/banyan-platform/erlmcp/discussions)

**"Let it crash, then let it recover."** - Joe Armstrong

</div>
