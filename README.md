# erlmcp

Erlang/OTP SDK for the Model Context Protocol (MCP) - Production-ready client and server implementation with comprehensive validation.

> **Quick Start?** See **[docs/README_RUN_LOCAL.md](docs/README_RUN_LOCAL.md)** for all entry points: STDIO server, HTTP server, validator, examples, and Erlang shell.

## ⚠️ Version 3.0: OTP 28.3.1+ Required

**BREAKING CHANGE**: erlmcp v3.0+ requires **Erlang/OTP 28.3.1 or later**. All backward compatibility with OTP 25-27 has been removed to enable exclusive use of native OTP 28+ features.

See [OTP 28.3.1 Migration Guide](#otp-2831-migration) below.

## Project Status

**Grade: A- (Production-Ready)** | **Last Evaluation**: 2026-02-01

[![Stability: Production](https://img.shields.io/badge/Stability-Production--Ready-brightgreen)](PROJECT_STATUS_REPORT_v3.0.0.md)
[![Test Coverage: 80%+](https://img.shields.io/badge/Coverage-80%25%2B-green)](PROJECT_STATUS_REPORT_v3.0.0.md)
[![MCP Spec Compliance: 95.7%](https://img.shields.io/badge/MCP%20Compliance-95.7%25-green)](PROJECT_STATUS_REPORT_v3.0.0.md)
[![Architecture: A-](https://img.shields.io/badge/Architecture-A%2D-brightgreen)](docs/architecture.md)
[![Chicago TDD: 100%](https://img.shields.io/badge/Chicago%20TDD-100%25-blue)](PROJECT_STATUS_REPORT_v3.0.0.md)

**Full Evaluation Report**: [PROJECT_STATUS_REPORT_v3.0.0.md](PROJECT_STATUS_REPORT_v3.0.0.md)

---

## Overview

erlmcp is a robust, production-grade implementation of the [MCP 2025-11-25 specification](https://modelcontextprotocol.io/) built with Erlang/OTP 28.3.1+. It provides:

- **MCP Protocol Implementation**: Implements MCP 2025-11-25 with automated compliance reporting (currently 95.7%—run `make compliance-report` to see exact gaps). JSON-RPC 2.0 with all MCP methods (tools, resources, prompts)
- **Multiple Transports**: STDIO, TCP, HTTP, WebSocket, Server-Sent Events
- **Production-Ready**: Supervision trees, circuit breakers, rate limiting, observability
- **Comprehensive Validation**: Automated spec compliance testing and reporting
- **High Performance**: 553K registry ops/s, 971K queue ops/s, 40-50K concurrent connections per node
- **OTP 28+ Features**: Native JSON, priority messages, scalable process iteration
- **Cloud-Native**: SessionStart hook, autonomous execution, deterministic quality gates

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

**Reference Materials** (not required for MCP compliance):
- `docs/tcps/`: Toyota Production System quality framework documentation
- `archive/`: Historical performance baselines, quality reports, implementation details
- Marketplace documentation: Deployment guides for large-scale scenarios

**What You Need for MCP**: Just the core apps (erlmcp_core + erlmcp_transports) and STDIO transport. Everything else enhances observability, testing, or provides experimental features.

## Quick Start

**Prerequisites**: Erlang/OTP 28.3.1 or later

### Setup & Development

```bash
# Verify Erlang version
erl -version  # Must be 28.3.1 or later

# Clone and compile
git clone https://github.com/yourusername/erlmcp.git
cd erlmcp
rebar3 compile

# Run tests
rebar3 eunit
rebar3 ct

# Run quality gates
make check

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

See [CLI Reference](docs/CLI_REFERENCE.md) for complete command documentation.

## Documentation

### Documentation Structure

The project documentation is organized for easy navigation:

- **Root Level**: Essential operational files (setup, contributing, changelog)
- **docs/**: Official specifications, API references, deployment guides
- **archive/**: Reference material (implementation details, performance baselines, quality reports)

For comprehensive navigation and finding specific topics:
- **[Documentation Guide](DOCUMENTATION_GUIDE.md)** - Complete navigation guide with quick links by topic
- **[Archive Index](ARCHIVE_INDEX.md)** - Guide to all archived reference materials

### Quick Links

**Getting Started**:
- [Development Setup](DEVELOPMENT.md) - Environment setup and workflow
- [Contributing Guidelines](CONTRIBUTING.md) - Code standards and PR process
- [System Architecture](CLAUDE.md) - Formal specification, OTP patterns, quality gates

**CLI Documentation** (NEW):
- [CLI Reference Guide](docs/CLI_REFERENCE.md) - All commands, options, exit codes
- [Interactive Mode Guide](docs/CLI_INTERACTIVE_GUIDE.md) - REPL workflows and examples
- [Shell Completions](docs/SHELL_COMPLETIONS_GUIDE.md) - Tab completion setup (bash/zsh/fish)
- [Diagnostics & Profiling](docs/DIAGNOSTICS_GUIDE.md) - Profiling, tracing, monitoring
- [Plugin Development](docs/PLUGIN_DEVELOPMENT_GUIDE.md) - Create custom plugins
- [CLI Examples](examples/cli/) - validate-spec.sh, connect-server.sh, recorded sessions

**Core Documentation**:
- [Architecture](docs/architecture.md) - System design and supervision trees
- [API Reference](docs/api-reference.md) - Complete API documentation
- [Protocol Implementation](docs/protocol.md) - MCP protocol details
- [Examples](examples/) - 40+ example implementations

**Validation & Testing**:
- [Validator Guide](docs/VALIDATOR_GUIDE.md) - Validation and compliance
- [Spec Compliance Testing](docs/SPEC_COMPLIANCE_TESTING.md) - Test coverage details
- [Quality Report](CODE_QUALITY_REPORT_V2.1.md) - Latest quality metrics

**Deployment & Operations**:
- [Deployment Guide](archive/tasks/DEPLOYMENT_GUIDE_100X.md) - Production deployment procedures
- [Production Readiness](archive/tasks/PRODUCTION_READINESS_SCORE.md) - Readiness assessment
- [Cluster Setup](archive/tasks/CLUSTER_SETUP.md) - Multi-node configuration

**Performance & Benchmarks**:
- [Benchmark Execution Guide](archive/benchmarks/BENCHMARK_EXECUTION_GUIDE.md) - Running benchmarks
- [OTP 28 Benchmark Summary](archive/benchmarks/OTP_28_BENCHMARK_SUMMARY.md) - Performance baselines
- [Performance Validator Report](archive/benchmarks/PERFORMANCE_VALIDATOR_REPORT.md) - Validation results

**API & Integration**:
- [MCP Endpoints Guide](archive/misc/MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md) - Comprehensive MCP API reference
- [Integration Plan](archive/misc/GGEN_INTEGRATION_PLAN.md) - Large-scale integration architecture

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

See `docs/architecture.md` (Transport Layer section) for detailed transport configuration and behavior.

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

See [CLAUDE.md](CLAUDE.md) (section: OTP 28.3.1 Features) for implementation details.

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

## Cloud Execution (Claude Code Web)

erlmcp is designed for **autonomous cloud execution** with deterministic quality gates:

```bash
# Cloud environment automatically:
# 1. Detects/installs OTP 28.3.1 (SessionStart hook)
# 2. Pre-compiles core modules (warm cache)
# 3. Runs parallel quality gates (compile, test, dialyzer, xref)
# 4. Generates audit receipts (.erlmcp/receipts/*.json)

# Full validation in 120s (parallel) or 240s (sequential)
make check  # All quality gates must pass
```

**SessionStart Hook** (.claude/hooks/SessionStart.sh):
- ✅ Automatic OTP 28.3.1 bootstrap (60s)
- ✅ Pre-compiled dependencies (warm cache)
- ✅ Environment validation
- ✅ Idempotent execution

**Cost Optimization**:
- Compile: $0.01 (30s)
- EUnit: $0.02 (60s)
- CT: $0.04 (120s)
- Dialyzer: $0.03 (90s)
- **Total**: $0.12 per full validation

See [CLAUDE.md](CLAUDE.md) (sections: Cloud Execution, SessionStart Hook) for complete details.

---

## Known Pitfalls & Troubleshooting

Common issues and solutions:

1. **SSE Priming Event Semantics**: The MCP spec requires a "priming event" (empty data with endpoint metadata) on SSE connection establishment. Clients must handle this before sending requests. See `docs/protocol.md` or `examples/sse_example.erl` for implementation.

2. **Last-Event-ID Resumption**: SSE reconnection with `Last-Event-ID` header requires server-side event buffering. Default: 100 events in-memory. Use DETS/Mnesia for durable backends. Configure via `{session_backend, dets}` in `config/sys.config`.

3. **STDIO Transport stdout Purity**: STDIO requires pure stdout (JSON-RPC only, no debug prints). Use stderr for logging. Violations cause JSON parse errors. See examples/stdio_server.erl.

4. **Windows CRLF Line Endings**: On Windows, STDIO may inject `\r\n` instead of `\n`. Use binary mode or configure `eol: lf` in transport options.

5. **Session Persistence**: Default is ETS (in-memory). Production must use DETS (single-node) or Mnesia (distributed). Configure in `config/sys.config`.

6. **Circuit Breaker Tuning**: Default threshold is 5 failures in 60s. High-latency backends may trip prematurely. Adjust via `config/sys.config`.

7. **Multi-Node Registry**: gproc is local by default. Use load balancer for multi-node instead of `erlmcp_registry_dist` (experimental).

**Comprehensive Troubleshooting**: See [docs/architecture.md](docs/architecture.md) (Troubleshooting section) or run `erlmcp doctor` for automated health checks.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for:
- Development setup
- Code style guidelines
- Testing requirements (Chicago School TDD)
- Pull request process

## License

[Specify your license here]

## Version

Current: **v2.1.0** | **Next Release**: v3.0.0 (pending quality gates)

**v2.1.0 Status**: ✅ Production-Ready (all quality gates passing)

**v3.0.0 Planning** (upgrading OTP 28.3.1 features):
- Dropping support for OTP 25-27
- Removing ~1,358 lines of backward compatibility code
- Making native json module mandatory (replacing jsx)
- All APIs will use OTP 28.3.1+ features exclusively

See [CHANGELOG.md](CHANGELOG.md) for release history.

## Support

- Documentation: [Documentation Guide](DOCUMENTATION_GUIDE.md) - Start here for navigation
- Issues: [GitHub Issues](https://github.com/yourusername/erlmcp/issues)
- Discussions: [GitHub Discussions](https://github.com/yourusername/erlmcp/discussions)

## OTP 28.3.1 Migration

### Upgrading from v2.1.0

**Requirements**:
1. Upgrade to Erlang/OTP 28.3.1 or later
2. Update rebar.config to use OTP 28.3.1 minimum
3. Remove any custom backward compatibility code

**Benefits**:
- ✅ 2-3x faster JSON operations (native json module)
- ✅ <1ms priority message latency for critical operations
- ✅ O(1) memory scalability for process monitoring
- ✅ ~10% throughput improvement from JIT
- ✅ Cleaner codebase (~1,358 lines removed)

**Migration Steps**:

```bash
# 1. Check current Erlang version
erl -version  # Must be 28.3.1 or later

# 2. Update your rebar.config
{minimum_otp_vsn, "28.3.1"}.

# 3. Remove jsx dependency (if you added it)
# erlmcp now uses native json module

# 4. Recompile
rebar3 clean
rebar3 compile

# 5. Run tests
rebar3 eunit
rebar3 ct
```

### API Changes

**JSON Operations** (automatically use native json):
```erlang
% All JSON encode/decode now use OTP 28 native json module
{ok, Json} = erlmcp_json:encode(Term).
{ok, Term} = erlmcp_json:decode(JsonBinary).
```

**Priority Messages** (health checks, circuit breakers):
```erlang
% Already implemented in erlmcp - no action needed
% Health checks now use EEP 76 priority messages automatically
```

**Process Monitoring** (internal optimization):
```erlang
% Internal code now uses erlang:processes_iterator/0 for O(1) memory
% No API changes - existing process monitoring APIs unchanged
```

See [docs/otp28-features.md](docs/otp28-features.md) for detailed feature documentation.

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
