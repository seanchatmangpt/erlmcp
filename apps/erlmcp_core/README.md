# erlmcp_core - Core MCP Protocol

**Version:** 2.0.0
**Application:** erlmcp_core
**Modules:** 14

Core implementation of the Model Context Protocol (MCP) in Erlang/OTP. Provides JSON-RPC 2.0, client/server gen_servers, gproc-based registry, and protocol primitives.

## Overview

erlmcp_core implements the foundational MCP protocol layer:
- **Client/Server** - OTP-compliant gen_server implementations with supervision
- **JSON-RPC 2.0** - Request/response encoding, notifications, error handling
- **Registry** - gproc-based process discovery and routing
- **Protocol Types** - Resources, tools, prompts, capabilities
- **Request Correlation** - ID-based request tracking and timeout management

## Core Modules (14 total)

### Client & Server
- **erlmcp_client.erl** - MCP client gen_server with request correlation
- **erlmcp_server.erl** - MCP server gen_server with resource/tool/prompt management
- **erlmcp_client_sup.erl** - Client supervisor (simple_one_for_one)
- **erlmcp_server_sup.erl** - Server supervisor (simple_one_for_one)

### Protocol Layer
- **erlmcp_json_rpc.erl** - JSON-RPC 2.0 encoding/decoding
- **erlmcp.hrl** - Protocol records and type definitions
- **erlmcp_capabilities.erl** - Capability negotiation and validation
- **erlmcp_types.erl** - MCP type definitions (resources, tools, prompts)

### Registry & Routing
- **erlmcp_registry.erl** - gproc-based process registration and discovery
- **erlmcp_router.erl** - Message routing between clients/servers/transports

### Application & Supervision
- **erlmcp_app.erl** - OTP application behavior
- **erlmcp_sup.erl** - Top-level supervisor (rest_for_one strategy)

### Utilities
- **erlmcp_schema.erl** - JSON Schema validation (jesse integration)
- **erlmcp_utils.erl** - Common utilities (binary operations, validation)

## Dependencies

| Library | Version | Purpose |
|---------|---------|---------|
| **jsx** | 3.1.0 | JSON encoding/decoding |
| **jesse** | 1.8.1 | JSON Schema validation |
| **gproc** | 0.9.0 | Process registry with automatic monitoring |

## Quick Start

### Client Example

```erlang
%% Connect via stdio
{ok, Client} = erlmcp_client:start_link({stdio, []}, #{strict_mode => false}),

%% Initialize connection
{ok, ServerCaps} = erlmcp_client:initialize(Client, #{
    <<"clientInfo">> => #{
        <<"name">> => <<"my-client">>,
        <<"version">> => <<"1.0.0">>
    }
}),

%% List resources
{ok, #{<<"resources">> := Resources}} = erlmcp_client:list_resources(Client),

%% Call a tool
{ok, Result} = erlmcp_client:call_tool(Client, <<"greet">>, #{
    <<"name">> => <<"World">>
}).
```

### Server Example

```erlang
%% Start server
{ok, Server} = erlmcp_server:start_link({stdio, []}, #{
    <<"serverInfo">> => #{
        <<"name">> => <<"my-server">>,
        <<"version">> => <<"1.0.0">>
    }
}),

%% Add a resource
erlmcp_server:add_resource(Server, <<"hello://world">>,
    fun(_Uri) -> <<"Hello from Erlang!">> end),

%% Add a tool with schema
Schema = #{
    <<"type">> => <<"object">>,
    <<"properties">> => #{
        <<"name">> => #{<<"type">> => <<"string">>}
    }
},
erlmcp_server:add_tool_with_schema(Server, <<"greet">>,
    fun(#{<<"name">> := Name}) ->
        <<"Hello, ", Name/binary, "!">>
    end, Schema).
```

## API Reference

See [../../docs/api-reference.md](../../docs/api-reference.md) for complete API documentation.

### Key Functions

**erlmcp_client:**
- `start_link/2` - Start client with transport
- `initialize/2` - Initialize MCP session
- `list_resources/1`, `read_resource/2` - Resource operations
- `call_tool/3` - Tool invocation
- `subscribe/2` - Resource change notifications

**erlmcp_server:**
- `start_link/2` - Start server with capabilities
- `add_resource/3`, `add_tool_with_schema/4` - Register handlers
- `notify_resource_changed/2` - Push notifications

**erlmcp_registry:**
- `register_server/2`, `find_server/1` - Server registration
- `route_to_server/3` - Message routing

## Build & Test

```bash
# Compile
rebar3 compile --app erlmcp_core

# Unit tests
rebar3 eunit --app erlmcp_core

# Specific module
rebar3 eunit --module=erlmcp_client_tests

# Type checking
rebar3 dialyzer --app erlmcp_core

# Coverage
rebar3 cover --app erlmcp_core
```

**Coverage Target:** 80% minimum (enforced by TCPS)

## Configuration

Override defaults in `sys.config`:

```erlang
[
    {erlmcp_core, [
        {client_defaults, #{
            timeout => 10000,
            strict_mode => true
        }},
        {server_defaults, #{
            max_subscriptions_per_resource => 2000
        }}
    ]}
].
```

## See Also

- [erlmcp_transports](../erlmcp_transports/README.md) - Transport layer
- [erlmcp_observability](../erlmcp_observability/README.md) - Metrics & traces
- [Architecture](../../docs/architecture.md) - System design
