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

# Fetch and compile
rebar3 get-deps
rebar3 compile
```

## Quick Start

### Building the Workspace

The erlmcp workspace contains two coordinated systems: **erlmcp** (core MCP implementation) and **taiea** (autonomic governance and receipts).

```bash
# Setup environment (one-time)
make setup

# Build the entire workspace
make workspace-build

# Run all tests across workspace
make workspace-test

# Full validation (build + lint + test)
make workspace-check

# Build production releases for both systems
make workspace-release

# Clean all build artifacts
make workspace-clean
```

**Quick reference:** `make help` shows all available targets.

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

## Build System Overview

### Workspace Structure

```
erlmcp/                    # Root workspace (MCP core)
├── Makefile              # Workspace-level build orchestration
├── rebar.config          # Root rebar configuration
├── src/                  # erlmcp source code
├── test/                 # erlmcp tests
├── examples/             # Example applications
└── taiea/                # TAIEA umbrella application (separate)
    ├── rebar.config      # TAIEA-specific configuration
    ├── apps/
    │   ├── taiea_core/    # Core autonomic logic
    │   ├── taiea_mcp/     # MCP integration
    │   ├── taiea_governor/ # Governance engine
    │   └── taiea_receipts/ # Deterministic receipts
    └── rel/              # Release configuration
```

### Build Targets by Category

**Workspace Targets** (orchestrate both erlmcp + taiea):
```bash
make workspace-build      # Compile entire workspace
make workspace-test       # Run all tests
make workspace-lint       # Static analysis (xref, dialyzer)
make workspace-check      # Full validation
make workspace-clean      # Clean all artifacts
make workspace-release    # Build production releases
```

**Erlmcp Application Targets**:
```bash
make build                # Compile erlmcp
make test                 # Run all tests (unit + integration + property)
make test-unit            # Unit tests only
make test-integration     # Common Test (CT) integration tests
make test-property        # Property-based tests (PropEr)
make lint                 # Static analysis (xref + dialyzer)
make check                # Full validation (compile, lint, test)
make release              # Build production release
```

**Analysis & Development**:
```bash
make xref                 # Cross-reference analysis
make dialyzer             # Type analysis
make coverage-report      # Generate coverage metrics
make format               # Auto-format code
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
make build                 # Compile changes
make test                  # Verify tests pass
make lint                  # Check code quality

# 3. Quick validation
make check                 # Run full validation (combines above)

# 4. Production release
make workspace-release     # Build releases for erlmcp + taiea
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

- [Architecture Overview](docs/architecture.md) - System design and components
- [Protocol Guide](docs/protocol.md) - MCP protocol implementation details
- [OTP Patterns](docs/otp-patterns.md) - Erlang/OTP best practices used
- [API Reference](docs/api-reference.md) - Complete API documentation

## Key Features

- ✅ Full MCP protocol support (resources, tools, prompts)
- ✅ OTP-compliant with supervision trees
- ✅ Multiple transport layers (stdio, TCP, HTTP)
- ✅ JSON Schema validation for tools
- ✅ Resource subscriptions with notifications
- ✅ Automatic reconnection with backoff
- ✅ Comprehensive error handling
- ✅ Production-ready logging and monitoring

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

## Documentation

- [DEVELOPMENT.md](DEVELOPMENT.md) - Development environment setup and workflows
- [Architecture Overview](docs/architecture.md) - System design and components
- [Protocol Guide](docs/protocol.md) - MCP protocol implementation details
- [OTP Patterns](docs/otp-patterns.md) - Erlang/OTP best practices
- [API Reference](docs/api-reference.md) - Complete API documentation

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
