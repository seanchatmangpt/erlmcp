# erlmcp

![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-28.3.1+-red.svg)
![Docker](https://img.shields.io/badge/docker-2024%2F09-blue.svg)
![License](https://img.shields.io/badge/license-Apache--2.0-blue.svg)
![MCP Spec](https://img.shields.io/badge/MCP-2025--11--25-green.svg)

> **Erlang/OTP SDK for the Model Context Protocol (MCP) - Production-ready client and server implementation**

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

### 👨‍💻 Developer
Build, test, and debug erlmcp locally.
- **Setup**: [Development Setup](#development-setup)
- **Testing**: [Quality Lanes](#quality-lanes)
- **Debugging**: [CLI Reference](docs/CLI_REFERENCE.md)

### 👷 Operator
Deploy and manage erlmcp in production.
- **Docker Compose**: [Single Node Deployment](#production-deployment)
- **Docker Swarm**: [Multi-Node Deployment](#docker-swarm-deployment)
- **Kubernetes**: [K8s Deployment](#kubernetes-deployment)

### 🏗️ Architect
Understand system design and integration.
- **Architecture**: [System Architecture](docs/architecture.md)
- **API Reference**: [Complete API](docs/api-reference.md)
- **Transport Specs**: [Transports Guide](docs/TRANSPORTS.md)

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

**For local development with Erlang/OTP 28.3.1+**

### Prerequisites

- Erlang/OTP 28.3.1 or later
- Docker 24.0+ (for Docker-based quality lanes)
- Make or rebar3

### Setup & Development

```bash
# Verify Erlang version
erl -version  # Must be 28.3.1 or later

# Clone and compile
git clone https://github.com/banyan-platform/erlmcp.git
cd erlmcp
rebar3 compile

# Run tests (via Docker quality lanes)
docker compose run --rm erlmcp-unit make eunit
docker compose run --rm erlmcp-ct make ct

# Run quality gates
docker compose run --rm erlmcp-check make check

# Start Erlang shell
make console
```

### CLI Quick Examples

```bash
# One-time initialization
erlmcp init

# Check system readiness
erlmcp doctor

# Start interactive shell
erlmcp start

# Run performance test (100K ops)
erlmcp test-100k

# Full benchmark suite
erlmcp benchmark

# Get help anytime
erlmcp help
```

## Quality Lanes

erlmcp uses Docker-based quality lanes for all validation. Each lane corresponds to a quality gate.

| Lane | Service | Purpose | Command |
|------|---------|---------|---------|
| Compile | `erlmcp-build` | Compilation gate | `docker compose run --rm erlmcp-build make compile` |
| EUnit | `erlmcp-unit` | Unit test gate | `docker compose run --rm erlmcp-unit make eunit` |
| CT | `erlmcp-ct` | Integration test gate | `docker compose run --rm erlmcp-ct make ct` |
| Check | `erlmcp-check` | Quality analysis gate | `docker compose run --rm erlmcp-check make check` |
| Bench | `erlmcp-bench` | Performance gate | `docker compose run --rm erlmcp-bench make benchmark` |
| Cluster | `erlmcp-node` | Cluster testing | `docker compose run --rm erlmcp-node make test-cluster` |

**Full Quality Pipeline**:
```bash
# Run all quality lanes in sequence
docker compose run --rm erlmcp-build make compile && \
docker compose run --rm erlmcp-unit make eunit && \
docker compose run --rm erlmcp-ct make ct && \
docker compose run --rm erlmcp-check make check
```

---

## Overview

erlmcp is a robust, production-grade implementation of the [MCP 2025-11-25 specification](https://modelcontextprotocol.io/) built with Erlang/OTP 28.3.1+. It provides:

- **MCP Protocol Implementation**: Implements MCP 2025-11-25 with automated compliance reporting (currently 95.7%—run `make compliance-report` to see exact gaps). JSON-RPC 2.0 with all MCP methods (tools, resources, prompts)
- **Multiple Transports**: STDIO, TCP, HTTP, WebSocket, Server-Sent Events
- **Production-Ready**: Supervision trees, circuit breakers, rate limiting, observability
- **Comprehensive Validation**: Automated spec compliance testing and reporting
- **High Performance**: 2.69M+ ops/sec in-memory, 40-50K concurrent connections per node
- **OTP 28+ Features**: Native JSON, priority messages, scalable process iteration

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

```bash
# Build validation CLI
rebar3 escriptize

# Run all validators
./_build/default/bin/erlmcp_validate run --all

# Run specific validator
./_build/default/bin/erlmcp_validate run --section protocol

# Run for specific transport
./_build/default/bin/erlmcp_validate run --transport tcp

# Generate compliance report
./_build/default/bin/erlmcp_validate report --format markdown

# Run with verbose output
./_build/default/bin/erlmcp_validate run --all --verbose
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

See [CONTRIBUTING.md](CONTRIBUTING.md) for:
- Development setup
- Code style guidelines
- Testing requirements (Chicago School TDD)
- Pull request process

## License

Apache License 2.0

## Version

Current: **v3.0.0** (OTP 28.3.1+ required)

**Breaking Changes in v3.0**:
- Dropped support for OTP 25-27 (exclusive use of OTP 28.3.1+ features)
- Removed ~1,358 lines of backward compatibility code
- Removed jsx dependency (native json module now mandatory)
- All APIs now use OTP 28.3.1+ features exclusively
- New minimum runtime dependencies: erts-16.0.3, kernel-10.4, stdlib-6.0

**MCP 2025-11-25 Specification Compliance**:
- Full support for MCP 2025-11-25 specification
- JSON-RPC 2.0 protocol with all error codes (-32700 to -32010, 1001-1089)
- Resource subscriptions with change notifications
- Tool invocation with progress token support
- Prompt templates with argument validation
- Roots capability for file system access

See [CHANGELOG.md](CHANGELOG.md) for full release history and [docs/OTP28_MIGRATION_USER_GUIDE.md](docs/OTP28_MIGRATION_USER_GUIDE.md) for migration details.

## Support

- Documentation: [Documentation Guide](DOCUMENTATION_GUIDE.md) - Start here for navigation
- Issues: [GitHub Issues](https://github.com/banyan-platform/erlmcp/issues)
- Discussions: [GitHub Discussions](https://github.com/banyan-platform/erlmcp/discussions)

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
# Option 1: Use custom OTP installation (recommended for development)
export ERLMCP_OTP_BIN="/Users/sac/.erlmcp/otp-28.3.1/bin"
export PATH="$ERLMCP_OTP_BIN:$PATH"

# Option 2: Install OTP 28.3.1 via kerl (version manager)
kerl install 28.3.1 $HOME/.kerl/28.3.1
. $HOME/.kerl/28.3.1/activate

# Option 3: Use Docker (recommended for production)
docker compose --profile runtime up -d
```

**Benefits**:
- 2-3x faster JSON operations (native json module)
- <1ms priority message latency for critical operations (EEP 76)
- O(1) memory scalability for process monitoring
- ~10% throughput improvement from JIT optimizations
- Post-quantum TLS 1.3 support (MLKEM hybrid algorithms)
- 75% memory reduction with process hibernation
- Cleaner codebase (~1,358 lines removed)

**Migration Steps**:

```bash
# 1. Verify Erlang version
erl -version  # Must be 28.3.1 or later
erl -eval 'erlang:display(erlang:system_info(otp_release)), init:stop().'
# Should output: "28"

# 2. Clean and recompile
rebar3 clean
rebar3 compile

# 3. Run tests
docker compose run --rm erlmcp-unit make eunit
docker compose run --rm erlmcp-ct make ct

# 4. Run quality gates
docker compose run --rm erlmcp-check make check
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

Built with:
- **Erlang/OTP 28.3.1+** (required)
- rebar3 (build tool)
- **json** (native stdlib - JSON encoding/decoding)
- jesse (JSON Schema validation)
- gproc (process registry)
- gun/cowboy (HTTP)
- ranch (TCP acceptor pool)
- opentelemetry-api (observability)
