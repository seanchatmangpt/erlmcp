# erlmcp

[![Build Status][gh-actions-badge]][gh-actions]

[![Project Logo][logo]][logo]

*Erlang implementation of the Model Context Protocol (MCP) SDK.*

MCP enables seamless communication between AI assistants and local services through a standardized protocol. This SDK provides both client and server implementations with full OTP compliance, allowing you to build robust, fault-tolerant integrations that expose resources, tools, and prompts to AI systems.

## Installation

**Requirements:** Erlang/OTP 25 or later

```bash
# Add to your rebar.config deps
{deps, [
    {erlmcp, {git, "https://github.com/banyan-platform/erlmcp.git", {branch, "main"}}}
]}.

# Fetch and compile (umbrella builds all apps)
rebar3 get-deps
rebar3 compile
```

## Quick Start

### Building the Umbrella

erlmcp v2.0.0 is an **umbrella application** with 4 independent OTP applications:

```bash
# Build all apps from root
rebar3 compile

# Run all tests
rebar3 eunit
rebar3 ct

# Full validation
make check
```

**Quick reference:** `make help` shows all available targets.

### Umbrella Structure

```
erlmcp/                           # Umbrella root
├── rebar.config                  # Umbrella configuration
├── apps/
│   ├── erlmcp_core/              # Core MCP protocol (14 modules)
│   │   ├── src/
│   │   │   ├── erlmcp_client.erl       # MCP client gen_server
│   │   │   ├── erlmcp_server.erl       # MCP server gen_server
│   │   │   ├── erlmcp_registry.erl     # gproc-based routing
│   │   │   ├── erlmcp_json_rpc.erl     # JSON-RPC 2.0
│   │   │   └── ...                     # Protocol core
│   │   └── test/                       # Core tests
│   │
│   ├── erlmcp_transports/        # Transport layer (8 modules)
│   │   ├── src/
│   │   │   ├── erlmcp_transport_stdio.erl   # Standard I/O
│   │   │   ├── erlmcp_transport_tcp.erl     # TCP (ranch)
│   │   │   ├── erlmcp_transport_http.erl    # HTTP/2 (gun)
│   │   │   ├── erlmcp_transport_ws.erl      # WebSocket
│   │   │   └── ...                          # Transport behaviors
│   │   └── test/                            # Transport tests
│   │
│   ├── erlmcp_observability/     # Metrics & traces (9 modules)
│   │   ├── src/
│   │   │   ├── erlmcp_metrics.erl          # Performance metrics
│   │   │   ├── erlmcp_otel.erl             # OpenTelemetry integration
│   │   │   ├── erlmcp_receipt_chain.erl    # Deterministic receipts
│   │   │   └── ...                         # Observability
│   │   └── test/                           # Observability tests
│   │
│   └── tcps_erlmcp/              # TCPS quality system (63 modules)
│       ├── src/
│       │   ├── tcps_shacl_validator.erl    # SHACL ontology validation
│       │   ├── tcps_quality_gates.erl      # Quality enforcement
│       │   ├── tcps_receipt_chain.erl      # SHA-256 hash chain
│       │   └── ...                         # TCPS manufacturing
│       └── test/                           # TCPS tests
│
├── examples/                     # Example applications
└── docs/                         # Complete documentation
```

### Creating an MCP Server

```erlang
%% Start a server that exposes resources
{ok, Server} = erlmcp_server:start_link({stdio, []}, Capabilities),

%% Add a resource
erlmcp_server:add_resource(Server, <<"hello://world">>,
    fun(_Uri) -> <<"Hello from Erlang!">> end),

%% Add a tool with JSON Schema validation
Schema = #{<<"type">> => <<"object">>,
           <<"properties">> => #{<<"name">> => #{<<"type">> => <<"string">>}}},
erlmcp_server:add_tool_with_schema(Server, <<"greet">>,
    fun(#{<<"name">> := Name}) ->
        <<"Hello, ", Name/binary, "!">>
    end, Schema).
```

### Creating an MCP Client

```erlang
%% Connect to an MCP server
{ok, Client} = erlmcp_client:start_link({stdio, []}, #{strict_mode => false}),

%% Initialize connection
{ok, _} = erlmcp_client:initialize(Client, Capabilities),

%% List available resources
{ok, #{<<"resources">> := Resources}} = erlmcp_client:list_resources(Client),

%% Call a tool
{ok, Result} = erlmcp_client:call_tool(Client, <<"greet">>,
                                        #{<<"name">> => <<"World">>}).
```

## v2.0.0 Architecture

erlmcp v2.0.0 is an **umbrella application** with 4 independent OTP applications. This architecture provides:

✅ **Separation of Concerns** - Clear application boundaries
✅ **Optional Features** - Exclude TCPS for minimal deployments
✅ **Independent Testing** - Test apps in isolation or together
✅ **Fault Isolation** - Failures contained within app boundaries
✅ **Parallel Development** - Teams work on different apps simultaneously

### 4 Independent OTP Applications

| App | Modules | Purpose | Dependencies |
|-----|---------|---------|--------------|
| **erlmcp_core** | 14 | Core MCP protocol, JSON-RPC, registry, client/server | jsx, jesse, gproc |
| **erlmcp_transports** | 8 | STDIO, TCP, HTTP/2, WebSocket, SSE, GraphQL | gun, ranch, poolboy, **erlmcp_core** |
| **erlmcp_observability** | 9 | Metrics, OpenTelemetry, receipts, health monitoring | opentelemetry_*, **erlmcp_core** |
| **tcps_erlmcp** | 63 | Toyota Code Production System (OPTIONAL) | bbmustache, cowboy, jobs, fs, **erlmcp_core**, **erlmcp_observability** |

**Total:** 94 modules organized into focused applications

### Supervision Architecture

```
erlmcp_sup (one_for_one)
├── erlmcp_core_sup (one_for_one)
│   ├── Registry & Routing (gproc-based)
│   ├── Session & Task Management
│   ├── Cache (L1/L2/L3)
│   └── Cluster & Failover
├── erlmcp_server_sup (simple_one_for_one)
│   └── [Dynamic MCP server instances]
└── erlmcp_observability_sup (one_for_one)
    ├── Metrics & Aggregation
    ├── OpenTelemetry Integration
    ├── Health Monitor
    └── Chaos Engineering

erlmcp_transport_sup (one_for_one)
└── [Dynamic transport instances]
    ├── STDIO (temporary restart)
    ├── TCP (transient, ranch pools)
    ├── HTTP/2 (transient, gun)
    ├── WebSocket (transient)
    ├── SSE (transient)
    └── GraphQL (transient)

tcps_erlmcp_sup (one_for_one) [OPTIONAL]
├── Work Order Management
├── Kanban (WIP limits)
├── Quality Gates (8 gates)
├── SKU Release Management
└── Dashboard & Metrics
```

**Key Design Principles:**
- **Bulkhead Pattern** - Failures isolated within app boundaries
- **No Cascading Restarts** - `one_for_one` strategy prevents domino effects
- **Observability Isolation** - Monitoring failures don't affect protocol
- **Transport Independence** - Each transport type fails independently

### Build Targets by Category

**Umbrella Targets** (build all 4 apps):
```bash
make build                # Compile all apps (core, transports, observability, tcps)
make test                 # Run all tests (unit + integration + property)
make test-unit            # Unit tests only
make test-integration     # Common Test (CT) integration tests
make test-property        # Property-based tests (PropEr)
make lint                 # Static analysis (xref + dialyzer)
make check                # Full validation (compile, lint, test)
make release              # Build production release
```

**Per-App Targets** (optional):
```bash
rebar3 compile --app erlmcp_core           # Compile core only
rebar3 eunit --app erlmcp_transports       # Test transports only
rebar3 dialyzer --app erlmcp_observability # Analyze observability only
```

**Analysis & Development**:
```bash
make xref                 # Cross-reference analysis
make dialyzer             # Type analysis
make coverage-report      # Generate coverage metrics
make console              # Start interactive Erlang shell
make dev-console          # Start dev shell with sys.config
make observer             # Launch Observer GUI for debugging
make profile              # Performance profiling
```

### Makefile Workflow Example

```bash
# 1. Initial setup
make setup                 # Downloads dependencies, creates directories

# 2. Development cycle
make build                 # Compile all 4 apps
make test                  # Verify all tests pass
make lint                  # Check code quality

# 3. Quick validation
make check                 # Run full validation (combines above)

# 4. Production release
make release               # Build release with all/selected apps
```

### SLO and Performance Targets

All build targets use `rebar3` with configured timeouts and performance gates:
- **Compile**: <15s (incremental), <30s (full)
- **Test Suite**: <60s (all tests including property-based)
- **Linting**: <30s (xref + dialyzer)
- **Release Build**: <120s

## Examples

See the [examples directory](examples/README.md) for comprehensive examples:

- **[Weather Server](examples/README.md#1-weather-server-weather_servererl)** - Full MCP server with resources, tools, and subscriptions
- **[Calculator Client](examples/README.md#2-calculator-client-calculator_clienterl)** - Sophisticated client with connection management
- **[Complete Application](examples/README.md#3-mcp-application-mcp_applicationerl)** - OTP application with supervision

## Documentation

**Start here**: [Documentation Index](docs/INDEX.md) - Navigation hub for all documentation

### Quick Guides by Role
- **[For Developers](docs/FOR_DEVELOPERS.md)** - Development setup, testing, debugging
- **[For Operators](docs/FOR_OPERATORS.md)** - Deployment, monitoring, scaling
- **[For Architects](docs/FOR_ARCHITECTS.md)** - System design, performance, security

### Complete Documentation
- [Getting Started](docs/GETTING_STARTED.md) - 5-minute quickstart
- [Architecture Overview](docs/ARCHITECTURE_OVERVIEW.md) - System design and components
- [Build System](docs/BUILD_SYSTEM.md) - Build targets, SLOs, performance gates
- [Deployment Guide](docs/DEPLOYMENT.md) - How to deploy to GCP/Kubernetes
- [Troubleshooting](docs/TROUBLESHOOTING.md) - Common issues and solutions
- [Protocol Guide](docs/protocol.md) - MCP protocol implementation details
- [Transport Behavior](docs/transport_behavior.md) - Transport interface and implementation guide
- [Transport Configuration](docs/transport_configuration.md) - Configuration examples for all transports
- [Migration Guide](docs/migration_guide.md) - Upgrade guide from v0.5.x to v0.6.0
- [OTP Patterns](docs/otp-patterns.md) - Erlang/OTP best practices used
- [API Reference](docs/api-reference.md) - Complete API documentation

### Contributing
- [Contributing Guide](CONTRIBUTING.md) - How to contribute code and documentation

## Ontology-Driven Code Generation with ggen

erlmcp integrates with [ggen](https://github.com/seanchatmangpt/ggen) for ontology-driven code generation, transforming TCPS (Toyota Code Production System) RDF ontologies into production artifacts.

### What is ggen?

ggen is a deterministic code generator that bridges semantic web technologies (RDF, SPARQL, OWL) with modern programming languages. It enables:

- **Single Source of Truth**: Define domain models once in RDF
- **SPARQL Queries**: Extract and transform ontology data
- **Tera Templates**: Generate code in any language (Erlang, Rust, TypeScript, etc.)
- **Deterministic Builds**: Same ontology + templates = identical output

### Quick Start with ggen

**Installation**:
```bash
# macOS/Linux
brew install seanchatmangpt/ggen/ggen

# Docker
docker pull seanchatman/ggen:6.0.0
```

**Generate TCPS artifacts**:
```bash
# Validate ontologies with SHACL
ggen validate --shacl

# Generate all artifacts (SKU listings, receipts, types, docs)
ggen sync

# Generate specific artifact types
ggen generate --rule sku_listings
ggen generate --rule erlang_types
ggen generate --rule quality_reports
```

### What Does ggen Generate?

From the TCPS ontologies in `ontology/`, ggen produces:

1. **SKU Marketplace Listings** (`generated/marketplace/`)
   - Product descriptions with quality receipts
   - Installation instructions
   - Feature lists and metadata

2. **Erlang Type Definitions** (`include/generated_types.hrl`)
   - Type-safe Erlang records from ontology classes
   - Guaranteed consistency between RDF and code

3. **Production Receipts** (`generated/receipts/`)
   - Cryptographic proof-of-work (SHA256)
   - Stage completion evidence
   - Deterministic and auditable

4. **Quality Reports** (`generated/reports/`)
   - Build and test metrics
   - SHACL validation results
   - Andon event summaries

5. **Work Orders & Andon Events** (`generated/work_orders/`, `generated/andon/`)
   - RDF instances for TCPS production flow
   - Pull-based demand signals
   - Quality alerts and root cause analysis

6. **Standard Work Documentation** (`generated/docs/standard-work/`)
   - Production stage procedures
   - SLO definitions and quality gates
   - Failure modes and mitigations

### Directory Structure

```
erlmcp/
├── ggen.toml                 # ggen configuration
├── ontology/                 # RDF ontologies (source of truth)
│   ├── tcps_core.ttl         # Core TCPS ontology
│   ├── tcps_quality.ttl      # Quality metrics ontology
│   └── tcps_flow.ttl         # Production flow ontology
├── shapes/                   # SHACL validation shapes
│   └── tcps_shapes.ttl
├── sparql/                   # SPARQL queries
│   └── tcps_queries/
│       ├── sku_readiness.rq
│       └── quality_metrics.rq
├── templates/                # Tera templates
│   └── ggen/
│       ├── sku_listing.md.tera
│       ├── receipt.json.tera
│       └── erlang_types.hrl.tera
└── generated/                # Generated artifacts (gitignored)
    ├── marketplace/
    ├── receipts/
    └── docs/
```

### Documentation

- **[GGEN Integration Guide](docs/GGEN_INTEGRATION.md)** - Complete ggen usage guide
- **[Example Script](scripts/ggen_example.sh)** - Interactive examples

### Benefits

- ✅ **Consistency**: Ontology drives both documentation and code
- ✅ **Determinism**: Reproducible builds with cryptographic receipts
- ✅ **Validation**: SHACL shapes catch errors before generation
- ✅ **Automation**: CI/CD integration for continuous generation
- ✅ **Type Safety**: Generated Erlang types match ontology exactly

## Key Features

### Core MCP Support
- ✅ Full MCP protocol support (resources, tools, prompts)
- ✅ OTP-compliant with supervision trees
- ✅ **Pluggable transport architecture** (stdio, TCP, HTTP, custom)
- ✅ **Behavior-based transport interface** for extensibility
- ✅ JSON Schema validation for tools
- ✅ Resource subscriptions with notifications
- ✅ **Automatic reconnection with exponential backoff**
- ✅ Comprehensive error handling and transport-specific errors
- ✅ **Production-ready logging and monitoring**
- ✅ **Test-friendly with transport mocking support**

### v0.6.0 Library Integration 🆕
- ✅ **gproc** registry - Automatic process monitoring and cleanup
- ✅ **gun** HTTP/2 - Modern HTTP/1.1 and HTTP/2 support
- ✅ **ranch** TCP - Production-grade connection pooling
- ✅ **poolboy** - Efficient worker pool management
- ✅ **~770 LOC reduction** - Less custom code, more reliability

### Production-Ready Architecture
- 🔧 Distributed process registry (gproc)
- 🚀 HTTP/2 multiplexing (gun)
- 🔌 Connection pooling (ranch + poolboy)
- 📊 Better performance under load
- 🛡️ Battle-tested libraries from Erlang ecosystem

## Development

### Setup Development Environment

For detailed development setup and workflows, see [DEVELOPMENT.md](DEVELOPMENT.md).

```bash
# One-time setup
direnv allow            # Enable automatic environment loading
make setup              # Initialize workspace

# Start development
make build              # Compile workspace
make test               # Run test suite
make dev-console        # Start interactive console
```

### Development Workflow

1. **Make changes** to `src/` or `taiea/apps/*/src/`
2. **Run tests**: `make test` (or specific: `make test-unit`)
3. **Check quality**: `make lint`
4. **Validate everything**: `make workspace-check`
5. **Commit** with confidence

### Contributing Guidelines

- Write tests first (TDD approach)
- Run `make check` before committing
- Ensure all tests pass locally
- Keep commits focused and documented
- Use `make format` to maintain code style

## What's New in v0.6.0

### Library Integration - Production-Grade Architecture

erlmcp v0.6.0 replaces **~770 lines of custom code** with battle-tested Erlang libraries:

```erlang
%% Before: Custom 411 LOC registry
%% After: gproc with automatic monitoring
{ok, Pid} = erlmcp_registry:find_server(ServerId).  % Automatic cleanup!

%% Before: Custom 461 LOC HTTP client
%% After: gun with HTTP/2 support
{ok, GunPid} = gun:open(Host, Port, #{protocols => [http2, http]}).

%% Before: Custom 349 LOC TCP handler
%% After: ranch with built-in pooling
ranch:start_listener(Name, ranch_tcp, #{port => 8080}, Handler, Opts).

%% NEW: Connection pooling
poolboy:transaction(http_pool, fun(Worker) ->
    erlmcp_http_worker:request(Worker, Req)
end).
```

**Impact**:
- 🚀 **20-100% performance improvements** in key operations
- 🔧 **HTTP/2 support** via gun (multiplexing, better throughput)
- 🔌 **Connection pooling** via ranch + poolboy
- 📊 **Distributed registry** support via gproc
- 🛡️ **Better reliability** - proven libraries vs custom code

See [Library Migration Guide](docs/library-migration-guide.md) for complete details.

## Quality Gate Enforcement System 🛡️

erlmcp implements Toyota Production System (TPS) Jidoka principles for zero-defect software delivery through automated quality gates.

### 8 Sequential Quality Gates

1. **SHACL Validation** - Ontology conformance (100% required)
2. **Compilation** - Zero-error builds (0 errors tolerated)
3. **Test Execution** - 95% pass rate, 80% coverage minimum
4. **Security Scan** - Zero critical vulnerabilities
5. **Deterministic Build** - Reproducibility verification
6. **Quality Metrics** - Production thresholds enforcement
7. **Release Verification** - SBOM, licenses, dependencies
8. **Smoke Test** - Basic functionality validation

**Core Principle:** Stop-the-line authority. Any gate failure triggers Andon event and blocks progression.

### Quick Start

```bash
# Check all quality gates
make quality-check

# Or using rebar3
SKU_ID="myfeature-$(git rev-parse --short HEAD)"
rebar3 tcps check-all-gates --sku=$SKU_ID

# View gate status
rebar3 tcps gate-status --sku=$SKU_ID

# View quality metrics
rebar3 tcps quality-metrics
```

### Quality Standards

- **Test Pass Rate:** ≥95% (Toyota standard)
- **Test Coverage:** ≥80% (industry best practice)
- **Defect Rate:** ≤5% (Lean Six Sigma 3σ)
- **First Pass Yield:** ≥90% (process effectiveness)

### Documentation

**Complete quality gate documentation:**
- **[Quality Gates Index](docs/quality-enforcement/INDEX.md)** - Overview and quick start
- **[User Guide](docs/quality-enforcement/USER_GUIDE.md)** - Daily usage for developers
- **[Admin Guide](docs/quality-enforcement/ADMIN_GUIDE.md)** - Configuration and management
- **[Architecture](docs/quality-enforcement/ARCHITECTURE.md)** - System design and components
- **[Philosophy](docs/quality-enforcement/PHILOSOPHY.md)** - Jidoka and zero-defects principles
- **[Migration Guide](docs/quality-enforcement/MIGRATION.md)** - Adopting quality gates
- **[FAQ](docs/quality-enforcement/FAQ.md)** - Common questions and troubleshooting

**Benefits:**
- ✅ 80% fewer production bugs (industry data)
- ✅ 50% faster incident resolution
- ✅ Immutable audit trail (SHA-256 receipt chain)
- ✅ Compliance-ready (SOC2, ISO 27001)

## Documentation

### Core Documentation
- **[Architecture Overview](docs/architecture.md)** - v2.0.0 umbrella design, supervision trees, inter-app communication
- [DEVELOPMENT.md](DEVELOPMENT.md) - Development environment setup and workflows
- [Protocol Guide](docs/protocol.md) - MCP protocol implementation details
- [OTP Patterns](docs/otp-patterns.md) - Erlang/OTP best practices with library patterns
- [API Reference](docs/api-reference.md) - Complete API documentation with transport config

### Migration & Quality
- [Library Migration Guide](docs/library-migration-guide.md) - v0.5 → v0.6.0 migration
- **[Quality Gates Documentation](docs/quality-enforcement/INDEX.md)** - Zero-defect delivery system (TCPS)

## License

Apache 2.0

## External Resources

- [Get started with the Model Context Protocol (MCP)](https://modelcontextprotocol.io/introduction)
- [Erlang/OTP Documentation](https://www.erlang.org/doc/)
- [rebar3 Documentation](https://rebar3.org/)
- [TAI Autonomics](https://github.com/seanchatmangpt/taiea)

[//]: ---Named-Links---

[logo]: priv/images/logo.png
[gh-actions-badge]: https://github.com/erlsci/erlmcp/workflows/ci/badge.svg
[gh-actions]: https://github.com/erlsci/erlmcp/actions?query=workflow%3Aci
