# erlmcp

Erlang/OTP SDK for the Model Context Protocol (MCP) - Production-ready client and server implementation with comprehensive validation.

## Overview

erlmcp is a robust, production-grade implementation of the [MCP 2025-11-25 specification](https://modelcontextprotocol.io/) built with Erlang/OTP. It provides:

- **Full MCP Protocol Support**: JSON-RPC 2.0 with all MCP methods (tools, resources, prompts)
- **Multiple Transports**: STDIO, TCP, HTTP, WebSocket, Server-Sent Events
- **Production-Ready**: Supervision trees, circuit breakers, rate limiting, observability
- **Comprehensive Validation**: 95%+ MCP spec compliance with automated testing
- **High Performance**: 2.69M ops/sec in-memory, 40-50K concurrent connections

## Quick Start

```bash
# Clone and compile
git clone https://github.com/yourusername/erlmcp.git
cd erlmcp
rebar3 compile

# Run tests
rebar3 eunit
rebar3 ct

# Start Erlang shell
make console
```

## Documentation

- [Architecture](docs/architecture.md) - System design and OTP patterns
- [API Reference](docs/api-reference.md) - Complete API documentation
- [Protocol](docs/protocol.md) - MCP protocol implementation
- [Examples](examples/) - Example implementations
- [Validator Guide](docs/VALIDATOR_GUIDE.md) - Validation and compliance
- [Spec Compliance Testing](docs/SPEC_COMPLIANCE_TESTING.md) - Test coverage

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

## Performance

**Baseline Performance (Jan 2026):**

| Metric | Value | Notes |
|--------|-------|-------|
| Registry throughput | 553K msg/s | In-memory message passing |
| Queue throughput | 971K msg/s | FIFO queue operations |
| Pool throughput | 149K msg/s | Connection pool |
| Session throughput | 242K msg/s | Session management |
| Network I/O | 43K msg/s | 4KB real packets (bottleneck) |
| Sustained load | 372K msg/s | 60M ops/30s |

**Honest capacity:** 40-50K concurrent active connections per node. 100K+ requires clustering.

## Quality Gates

**Automated validation (enforced on every commit):**

- [x] **Compilation**: 0 errors
- [x] **Tests**: 100% pass rate
- [x] **Coverage**: ≥80%
- [x] **Dialyzer**: 0 type warnings
- [x] **Xref**: 0 undefined functions
- [x] **Benchmarks**: <10% regression (if perf code changed)

## Toyota Production System

erlmcp integrates TPS principles:

- **Andon** (行灯): Visible error signaling via health dashboard
- **Poka-Yoke** (ポカヨケ): Built-in validation at every layer
- **Jidoka** (自働化): Quality tests stop production on failure
- **Kaizen** (改善): Continuous improvement via chaos engineering

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for:
- Development setup
- Code style guidelines
- Testing requirements (Chicago School TDD)
- Pull request process

## License

[Specify your license here]

## Version

Current: **v2.1.0**

See [CHANGELOG.md](CHANGELOG.md) for release history.

## Support

- Documentation: [docs/](docs/)
- Issues: [GitHub Issues](https://github.com/yourusername/erlmcp/issues)
- Discussions: [GitHub Discussions](https://github.com/yourusername/erlmcp/discussions)

## Acknowledgments

Built with:
- Erlang/OTP 25-28
- rebar3 (build tool)
- jsx (JSON)
- jesse (JSON Schema validation)
- gproc (process registry)
- gun/cowboy (HTTP)
- ranch (TCP acceptor pool)
- opentelemetry-api (observability)
