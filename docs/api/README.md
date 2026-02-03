# erlmcp v3.0 API Documentation

Complete API documentation for erlmcp Model Context Protocol Server v3.0.

## Quick Links

| Document | Description |
|----------|-------------|
| [Quick Start](quick-start.md) | Get started in 5 minutes |
| [OpenAPI Specification](openapi-spec.yaml) | Complete OpenAPI 3.0 specification |
| [JSON-RPC Reference](json-rpc-reference.md) | Protocol method reference |
| [Transport Protocols](transport-protocols.md) | stdio, TCP, HTTP, WebSocket, SSE |
| [Error Codes](error-codes.md) | Complete error code reference |
| [Security](security.md) | Authentication, authorization, secrets |
| [Client SDKs](client-sdks.md) | Python, Go, Java, JavaScript, Rust |
| [Integration Examples](integration-examples.md) | Real-world integration patterns |
| [Migration Guide](migration-guide.md) | Version upgrade procedures |

## Overview

erlmcp implements the [Model Context Protocol (MCP)](https://spec.modelcontextprotocol.io/) specification version 2025-11-25. MCP enables AI assistants to securely interact with external tools, resources, and prompts through a standardized JSON-RPC 2.0 protocol.

### Key Features

- **Multiple Transports**: stdio (local), TCP, HTTP, WebSocket, SSE
- **Comprehensive Security**: mTLS, JWT, OAuth 2.0, encrypted secrets storage
- **High Performance**: Built on Erlang/OTP, handles 100K+ messages/second
- **Production Ready**: Health checks, rate limiting, observability with OTEL
- **Full MCP Compliance**: Complete implementation of MCP 2025-11-25 spec

### Protocol Versions

| erlmcp Version | MCP Spec | Status |
|----------------|----------|--------|
| 3.0.x | 2025-11-25 | Current |
| 2.x | 2024-11-05 | Previous |
| 1.x | 2024-03-01 | Deprecated |

## Getting Started

### Installation

```bash
# Clone repository
git clone https://github.com/erlmcp/erlmcp.git
cd erlmcp

# Compile
rebar3 compile

# Run tests
rebar3 eunit
rebar3 ct
```

### Minimal Server Example

```erlang
-module(my_server).
-export([start/0]).

start() ->
    application:ensure_all_started(erlmcp_core),

    Capabilities = #mcp_server_capabilities{
        tools = #mcp_tools_capability{}
    },

    {ok, Server} = erlmcp_server:start_link(my_server, Capabilities),

    erlmcp_server:add_tool(Server, #{
        name => <<"hello">>,
        description => <<"Say hello">>,
        handler => fun(_) ->
            #{content => [{type => text, text => <<"Hello World!">>}]}
        end
    }),

    {ok, _} = erlmcp_transport_stdio:start_link(Server),
    receive stop -> ok end.
```

## API Endpoints

### Core Methods

| Method | Description |
|--------|-------------|
| `initialize` | Initialize connection |
| `ping` | Liveness check |
| `shutdown` | Graceful shutdown |

### Resources

| Method | Description |
|--------|-------------|
| `resources/list` | List available resources |
| `resources/read` | Read resource contents |
| `resources/subscribe` | Subscribe to updates |
| `resources/unsubscribe` | Unsubscribe from updates |

### Tools

| Method | Description |
|--------|-------------|
| `tools/list` | List available tools |
| `tools/call` | Execute a tool |

### Prompts

| Method | Description |
|--------|-------------|
| `prompts/list` | List prompt templates |
| `prompts/get` | Get prompt with arguments |

## Transport Selection

| Transport | Use Case | Performance |
|-----------|----------|-------------|
| **stdio** | Claude Desktop, local tools | 50K msg/s |
| **TCP** | High-performance custom clients | 100K msg/s |
| **HTTP** | Web applications, REST APIs | 10K req/s |
| **WebSocket** | Real-time, browser clients | 80K msg/s |
| **SSE** | Server-to-client streaming | 20K evt/s |

## Support

- **Documentation**: https://github.com/erlmcp/erlmcp/tree/main/docs
- **Issues**: https://github.com/erlmcp/erlmcp/issues
- **Discussions**: https://github.com/erlmcp/erlmcp/discussions
- **Discord**: https://discord.gg/erlmcp

## License

Apache License 2.0 - see LICENSE file for details.
