# erlmcp_transports - Transport Layer

**Version:** 2.0.0
**Application:** erlmcp_transports
**Modules:** 8

Production-grade transport implementations for the Model Context Protocol. Provides STDIO, TCP (ranch), HTTP/2 (gun), and WebSocket transports with connection pooling and automatic reconnection.

## Overview

erlmcp_transports implements the transport layer with:
- **STDIO** - Standard input/output for command-line tools
- **TCP** - Production TCP server via ranch (40-50K connections/node)
- **HTTP** - HTTP/1.1 and HTTP/2 client via gun
- **WebSocket** - WebSocket transport for browser clients
- **Behavior Interface** - Extensible transport behavior for custom implementations
- **Connection Pooling** - poolboy integration for HTTP/TCP connection reuse

## Transport Modules (8 total)

### Transport Implementations
- **erlmcp_transport_stdio.erl** - Standard I/O (pipes) for CLI tools
- **erlmcp_transport_tcp.erl** - TCP server/client with ranch acceptor pools
- **erlmcp_transport_http.erl** - HTTP/2 client with gun, connection pooling
- **erlmcp_transport_ws.erl** - WebSocket transport for browser integration

### Transport Infrastructure
- **erlmcp_transport.erl** - Transport behavior definition and API
- **erlmcp_transport_sup.erl** - Transport supervisor (simple_one_for_one)

### Application
- **erlmcp_transports_app.erl** - OTP application callback
- **erlmcp_transports_sup.erl** - Top-level supervisor

## Dependencies

| Library | Version | Purpose |
|---------|---------|---------|
| **gun** | 2.0.1 | HTTP/1.1, HTTP/2, WebSocket client |
| **ranch** | 2.1.0 | TCP acceptor pool and connection management |
| **poolboy** | 1.5.2 | Connection pooling for HTTP/TCP |
| **erlmcp_core** | 2.0.0 | Core MCP protocol |

## Transport Behavior

All transports implement the `erlmcp_transport` behavior:

```erlang
-callback init(TransportId :: atom(), Config :: map()) ->
    {ok, State} | {error, Reason}.

-callback send(State, Data :: iodata()) ->
    ok | {error, Reason}.

-callback close(State) -> ok.

%% Optional callbacks
-callback get_info(State) ->
    #{type => atom(), status => atom(), peer => term()}.

-optional_callbacks([get_info/1]).
```

## Usage Examples

### STDIO Transport (Default)

```erlang
%% Client - communicate via stdin/stdout
{ok, Client} = erlmcp_client:start_link({stdio, []}, Opts),

%% Server - read from stdin, write to stdout
{ok, Server} = erlmcp_server:start_link({stdio, []}, ServerCaps).
```

**Use cases:** CLI tools, shell scripts, process pipelines

### TCP Transport (Ranch)

```erlang
%% TCP Server - listen on port 3000 (ranch acceptor pool)
{ok, Server} = erlmcp_server:start_link({tcp, #{
    port => 3000,
    num_acceptors => 10,
    max_connections => 50000
}}, ServerCaps),

%% TCP Client - connect to server
{ok, Client} = erlmcp_client:start_link({tcp, #{
    host => "localhost",
    port => 3000,
    connect_timeout => 5000,
    keepalive => true
}}, Opts).
```

**Use cases:** High-throughput server deployments, microservices, distributed systems

**Performance:** 40-50K concurrent connections per node (validated via benchmarks)

### HTTP Transport (Gun)

```erlang
%% HTTP Client with gun (HTTP/2 support)
{ok, Client} = erlmcp_client:start_link({http, #{
    url => <<"http://localhost:3001">>,
    protocol => http2,  % or http
    connect_timeout => 5000,
    request_timeout => 30000,
    pool_size => 10     % Connection pool
}}, Opts).
```

**Features:**
- Automatic HTTP/2 upgrade when supported
- Connection pooling via poolboy
- TLS/SSL support
- Keepalive and connection reuse

**Performance:** 5-10K concurrent connections (HTTP/2 multiplexing)

### WebSocket Transport

```erlang
%% WebSocket Client
{ok, Client} = erlmcp_client:start_link({ws, #{
    url => <<"ws://localhost:8080/mcp">>,
    protocols => [<<"mcp">>],
    connect_timeout => 5000
}}, Opts).
```

**Use cases:** Browser clients, real-time dashboards, web applications

## Adding a Custom Transport

Implement the `erlmcp_transport` behavior:

```erlang
-module(erlmcp_transport_custom).
-behaviour(erlmcp_transport).

-export([init/2, send/2, close/1, get_info/1]).

init(TransportId, Config) ->
    %% Initialize transport state
    State = #{id => TransportId, config => Config},
    {ok, State}.

send(State, Data) ->
    %% Send data to peer
    ok.

close(State) ->
    %% Cleanup resources
    ok.

get_info(State) ->
    #{type => custom, status => connected}.
```

Register in erlmcp_core:

```erlang
{ok, Client} = erlmcp_client:start_link({custom, #{}}Opts).
```

## Configuration

Default transport config in `erlmcp_transports.app.src`:

```erlang
{transport_defaults, #{
    tcp => #{
        connect_timeout => 5000,
        keepalive => true,
        nodelay => true,
        port => 3000
    },
    http => #{
        connect_timeout => 5000,
        request_timeout => 30000,
        max_connections => 100,
        port => 3001
    },
    stdio => #{
        buffer_size => 65536,
        read_timeout => infinity
    }
}}
```

Override in `sys.config`:

```erlang
[
    {erlmcp_transports, [
        {transport_defaults, #{
            tcp => #{port => 8000},
            http => #{max_connections => 200}
        }}
    ]}
].
```

## Build & Test

```bash
# Compile
rebar3 compile --app erlmcp_transports

# Unit tests
rebar3 eunit --app erlmcp_transports

# Integration tests (requires network)
rebar3 ct --app erlmcp_transports

# Type checking
rebar3 dialyzer --app erlmcp_transports
```

## Performance Characteristics

**Benchmarked (v1.5.0):**

| Transport | Throughput | Latency (p50) | Max Connections | Notes |
|-----------|------------|---------------|-----------------|-------|
| **TCP** | 43K msg/s | 11 μs | 40-50K/node | ranch acceptor pool |
| **HTTP** | 12K req/s | 87 μs | 5-10K/node | HTTP/2 multiplexing |
| **STDIO** | 5K msg/s | 203 μs | 1 (single process) | CLI tools only |
| **WebSocket** | 8K msg/s | 125 μs | 10K/node | Browser clients |

**Bottlenecks:**
- STDIO: Single-threaded by design
- HTTP: TLS handshake overhead (use connection pooling)
- TCP: File descriptor limits (increase with `ulimit -n`)

## Migration from v0.5.x

v2.0.0 moves transports into separate app. Update your code:

```erlang
% Before (v0.5.x - flat structure)
{ok, Client} = erlmcp_client:start_link(stdio, []).

% After (v2.0.0 - tuple transport config)
{ok, Client} = erlmcp_client:start_link({stdio, []}, Opts).
```

Transport modules remain in same namespace - no code changes beyond config format.

## See Also

- [erlmcp_core](../erlmcp_core/README.md) - Core MCP protocol
- [erlmcp_observability](../erlmcp_observability/README.md) - Metrics for transports
- [Transport Configuration Guide](../../docs/transport_configuration.md) - Detailed config examples
- [Architecture](../../docs/architecture.md) - System design
