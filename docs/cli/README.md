# ERLMCP CLI Documentation

Welcome to the erlmcp CLI comprehensive documentation. This guide covers all aspects of the erlmcp Command Line Interface, including API documentation, integration guides, configuration, troubleshooting, and best practices.

## Quick Start

```bash
# Install erlmcp CLI
rebar3 escriptize
./erlmcp init          # Initialize development environment
./erlmcp start         # Start erlmcp cluster
./erlmcp status        # Check cluster status
./erlmcp benchmark     # Run performance benchmarks
```

## Documentation Structure

- [API Documentation](./api-reference.md) - Complete API reference with examples
- [Integration Guide](./integration-guide.md) - MCP server integration setup
- [Configuration Guide](./configuration.md) - Environment variables and config files
- [Examples & Tutorials](./examples/README.md) - Usage examples and tutorials
- [Migration Guide](./migration.md) - Version upgrade procedures
- [Troubleshooting Guide](./troubleshooting.md) - Common issues and solutions
- [Architecture Documentation](./architecture.md) - CLI component overview
- [Best Practices](./best-practices.md) - Performance optimization and security

## CLI Commands

### Development Environment
| Command | Description |
|---------|-------------|
| `init` | Initialize local development environment |
| `start` | Start local erlmcp cluster |
| `stop` | Stop local erlmcp cluster |
| `status` | Show cluster status and health |

### Testing & Validation
| Command | Description |
|---------|-------------|
| `test-100k` | Run 100K concurrent operations test |
| `benchmark` | Run performance benchmarks |
| `bench run [options]` | Run reproducible benchmarks |
| `chaos run [options]` | Run chaos engineering tests |

### Configuration Management
| Command | Description |
|---------|-------------|
| `profile list` | List available configuration profiles |
| `profile show <name>` | Show profile configuration |
| `profile apply <name>` | Apply profile to runtime |
| `profile validate <name>` | Validate profile configuration |

### Upgrade Management
| Command | Description |
|---------|-------------|
| `upgrade plan V1 V2` | Show upgrade plan between versions |
| `upgrade verify` | Verify post-upgrade system health |

## Features

- **Multi-Transport Support**: stdio, TCP, HTTP, WebSocket, SSE
- **MCP Protocol Compliance**: Full JSON-RPC 2.0 implementation
- **Interactive Mode**: REPL with command history and completion
- **Configuration Profiles**: Development, production, and government profiles
- **Performance Monitoring**: Built-in benchmarks and metrics
- **Chaos Engineering**: Resilience testing capabilities
- **OTEL Integration**: OpenTelemetry observability

## Requirements

- **Erlang/OTP**: 25+ (minimum version 25)
- **rebar3**: Build tool for Erlang projects
- **Memory**: 512MB minimum recommended
- **Network**: Standard TCP/HTTP connectivity

## Getting Help

```bash
# Command help
./erlmcp help

# Validate CLI installation
./erlmcp validate spec-check

# Get version information
./erlmcp profile list
```

---

For detailed information about any specific component, refer to the corresponding section in this documentation.