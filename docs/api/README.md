# erlmcp v3.0 API Documentation

Complete API documentation for erlmcp Model Context Protocol Server v3.0.

## Quick Links

| Document | Description |
|----------|-------------|
| [Quick Start](quick-start.md) | Get started in 5 minutes |
| [Complete Example](complete-example.md) | Full client-server implementation |
| **Core API References** | |
| [Server API Reference](server-api.md) | Complete server-side API documentation |
| [Client API Reference](client-api.md) | Complete client-side API documentation |
| [JSON-RPC Reference](json-rpc-reference.md) | Protocol method reference |
| **Transport & Communication** | |
| [Transport Protocols](transport-protocols.md) | stdio, TCP, HTTP, WebSocket, SSE configuration |
| [Error Handling Patterns](error-handling-patterns.md) | Comprehensive error handling guide |
| [Error Codes](error-codes.md) | Complete error code reference |
| **Security & Advanced Topics** | |
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

All compilation and testing must be performed via Docker as per project requirements:

```bash
# Clone repository
git clone https://github.com/erlmcp/erlmcp.git
cd erlmcp

# Compile via Docker
docker compose run erlmcp-build rebar3 compile

# Run tests via Docker
docker compose run erlmcp-unit rebar3 eunit
docker compose run erlmcp-ct rebar3 ct
```

### Minimal Server Example

```erlang
-module(my_server).
-export([start/0]).

start() ->
    %% Start required applications
    application:ensure_all_started(erlmcp_core),

    %% Define server capabilities
    Capabilities = #{
        tools => #{},
        resources => #{},
        prompts => #{}
    },

    %% Start the MCP server
    {ok, Server} = erlmcp_server:start_link(my_server, Capabilities),

    %% Add a simple tool
    erlmcp_server:add_tool(Server, #{
        name => <<"hello">>,
        description => <<"Say hello to the world">>,
        input_schema => #{
            type => object,
            properties => #{
                name => #{type => string}
            }
        },
        handler => fun(Args) ->
            Name = maps:get(<<"name">>, Args, <<"World">>),
            #{content => [#{
                type => text,
                text => <<"Hello, ", Name/binary, "!">>
            }]}
        end
    }),

    %% Start stdio transport for Claude Desktop integration
    {ok, _Transport} = erlmcp_transport_stdio:start_link(Server),
    receive stop -> ok end.
```

### Minimal Client Example

```erlang
-module(my_client).
-export([start/0, call_tool/0]).

start() ->
    %% Start required applications
    application:ensure_all_started(erlmcp_core),

    %% Define client capabilities
    Capabilities = #{
        roots => #{},
        sampling => #{}
    },

    ClientInfo = #{
        name => <<"my-client">>,
        version => <<"1.0.0">>
    },

    %% Connect via stdio transport
    {ok, Client} = erlmcp_client:start_link(#{
        transport => stdio,
        capabilities => Capabilities,
        client_info => ClientInfo
    }),

    {ok, Client}.

call_tool() ->
    {ok, Client} = start(),

    %% List available tools
    {ok, #{<<"tools">> := Tools}} = erlmcp_client:list_tools(Client),
    io:format("Available tools: ~p~n", [Tools]),

    %% Call a tool
    {ok, Result} = erlmcp_client:call_tool(Client, <<"hello">>, #{
        <<"name">> => <<"Erlang">>
    }),
    io:format("Result: ~p~n", [Result]).
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
